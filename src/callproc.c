/* Old synchronous subprocess invocation for SXEmacs.
   Copyright (C) 1985, 86, 87, 88, 93, 94, 95 Free Software Foundation, Inc.

This file is part of SXEmacs

SXEmacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SXEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */


/* Synched up with: Mule 2.0, FSF 19.30. */
/* Partly sync'ed with 19.36.4 */

/* #### This ENTIRE file is only used in batch mode.

   We only need two things to get rid of both this and ntproc.c:

   -- my `stderr-proc' ws, which adds support for a separate stderr
      in asynch. subprocesses. (it's a feature in `old-call-process-internal'.)
   -- a noninteractive event loop that supports processes.
*/

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "commands.h"
#include "ui/insdel.h"
#include "lstream.h"
#include "process.h"
#include "sysdep.h"
#include "ui/window.h"
#ifdef FILE_CODING
#include "mule/file-coding.h"
#endif

#include "systime.h"
#include "sysproc.h"
#include "sysfile.h"		/* Always include after sysproc.h */
#include "syssignal.h"		/* Always include before systty.h */
#include "ui/TTY/systty.h"



Lisp_Object Vshell_file_name;

/* The environment to pass to all subprocesses when they are started.
   This is in the semi-bogus format of ("VAR=VAL" "VAR2=VAL2" ... )
 */
Lisp_Object Vprocess_environment;

/* True iff we are about to fork off a synchronous process or if we
   are waiting for it.  */
volatile int synch_process_alive;

/* Nonzero => this is a string explaining death of synchronous subprocess.  */
const char *synch_process_death;

/* If synch_process_death is zero,
   this is exit code of synchronous subprocess.  */
int synch_process_retcode;

/* Clean up when exiting Fcall_process_internal.
   On Windows, delete the temporary file on any kind of termination.
   On Unix, kill the process and any children on termination by signal.  */

/* Nonzero if this is termination due to exit.  */
static int call_process_exited;

Lisp_Object Vlisp_EXEC_SUFFIXES;

static Lisp_Object call_process_kill(Lisp_Object fdpid)
{
	Lisp_Object fd = Fcar(fdpid);
	Lisp_Object pid = Fcdr(fdpid);

	if (!NILP(fd))
		close(XINT(fd));

	if (!NILP(pid))
		EMACS_KILLPG(XINT(pid), SIGKILL);

	synch_process_alive = 0;
	return Qnil;
}

static Lisp_Object call_process_cleanup(Lisp_Object fdpid)
{
	int fd = XINT(Fcar(fdpid));
	int pid = XINT(Fcdr(fdpid));

	if (!call_process_exited && EMACS_KILLPG(pid, SIGINT) == 0) {
		int speccount = specpdl_depth();

		record_unwind_protect(call_process_kill, fdpid);
		/* #### "c-G" -- need non-consing Single-key-description */
		message
		    ("Waiting for process to die...(type C-g again to kill it instantly)");

		wait_for_termination(pid);

		/* "Discard" the unwind protect.  */
		XCAR(fdpid) = Qnil;
		XCDR(fdpid) = Qnil;
		unbind_to(speccount, Qnil);

		message("Waiting for process to die... done");
	}
	synch_process_alive = 0;
	close(fd);
	return Qnil;
}

static Lisp_Object fork_error;

DEFUN("old-call-process-internal", Fold_call_process_internal, 1, MANY, 0, /*
Call PROGRAM synchronously in separate process, with coding-system specified.
Arguments are
(PROGRAM &optional INFILE BUFFER DISPLAY &rest ARGS).
The program's input comes from file INFILE (nil means `/dev/null').
Insert output in BUFFER before point; t means current buffer;
nil for BUFFER means discard it; 0 means discard and don't wait.
BUFFER can also have the form (REAL-BUFFER STDERR-FILE); in that case,
REAL-BUFFER says what to do with standard output, as above,
while STDERR-FILE says what to do with standard error in the child.
STDERR-FILE may be nil (discard standard error output),
t (mix it with ordinary output), or a file name string.

Fourth arg DISPLAY non-nil means redisplay buffer as output is inserted.
Remaining arguments are strings passed as command arguments to PROGRAM.

If BUFFER is 0, `call-process' returns immediately with value nil.
Otherwise it waits for PROGRAM to terminate and returns a numeric exit status
or a signal description string.
If you quit, the process is killed with SIGINT, or SIGKILL if you
quit again.
*/
      (int nargs, Lisp_Object * args))
{
	/* This function can GC */
	Lisp_Object infile, buffer, current_dir, display, path;
	int fd[2];
	int filefd;
	int pid;
	char buf[16384];
	char *bufptr = buf;
	int bufsize = 16384;
	int speccount = specpdl_depth();
	struct gcpro gcpro1, gcpro2, gcpro3;
	char **new_argv = alloca_array(char *, max(2, nargs - 2));

	/* File to use for stderr in the child.
	   t means use same as standard output.  */
	Lisp_Object error_file;

	CHECK_STRING(args[0]);

	error_file = Qt;

#if defined (NO_SUBPROCESSES)
	/* Without asynchronous processes we cannot have BUFFER == 0.  */
	if (nargs >= 3 && !INTP(args[2]))
		error
		    ("Operating system cannot handle asynchronous subprocesses");
#endif				/* NO_SUBPROCESSES */

	/* Do this before building new_argv because GC in Lisp code
	 *  called by various filename-hacking routines might relocate strings */
	locate_file(Vexec_path, args[0], Vlisp_EXEC_SUFFIXES, &path, X_OK);

	/* Make sure that the child will be able to chdir to the current
	   buffer's current directory, or its unhandled equivalent.  We
	   can't just have the child check for an error when it does the
	   chdir, since it's in a vfork. */
	{
		struct gcpro ngcpro1, ngcpro2;
		/* Do this test before building new_argv because GC in Lisp code
		 *  called by various filename-hacking routines might relocate strings */
		/* Make sure that the child will be able to chdir to the current
		   buffer's current directory.  We can't just have the child check
		   for an error when it does the chdir, since it's in a vfork.  */

		current_dir = current_buffer->directory;
		NGCPRO2(current_dir, path);	/* Caller gcprotects args[] */
		current_dir = Funhandled_file_name_directory(current_dir);
		current_dir = expand_and_dir_to_file(current_dir, Qnil);
#if 0
		/* This is in FSF, but it breaks everything in the presence of
		   ange-ftp-visited files, so away with it.  */
		if (NILP(Ffile_accessible_directory_p(current_dir)))
			report_file_error("Setting current directory",
					  Fcons(current_buffer->directory,
						Qnil));
#endif				/* 0 */
		NUNGCPRO;
	}

	GCPRO2(current_dir, path);

	if (nargs >= 2 && !NILP(args[1])) {
		struct gcpro ngcpro1;
		NGCPRO1(current_buffer->directory);
		infile = Fexpand_file_name(args[1], current_buffer->directory);
		NUNGCPRO;
		CHECK_STRING(infile);
	} else
		infile = build_string(NULL_DEVICE);

	UNGCPRO;

	GCPRO3(infile, current_dir, path);	/* Fexpand_file_name might trash it */

	if (nargs >= 3) {
		buffer = args[2];

		/* If BUFFER is a list, its meaning is
		   (BUFFER-FOR-STDOUT FILE-FOR-STDERR).  */
		if (CONSP(buffer)) {
			if (CONSP(XCDR(buffer))) {
				Lisp_Object file_for_stderr =
				    XCAR(XCDR(buffer));

				if (NILP(file_for_stderr)
				    || EQ(Qt, file_for_stderr))
					error_file = file_for_stderr;
				else
					error_file =
					    Fexpand_file_name(file_for_stderr,
							      Qnil);
			}

			buffer = XCAR(buffer);
		}

		if (!(EQ(buffer, Qnil)
		      || EQ(buffer, Qt)
		      || ZEROP(buffer))) {
			Lisp_Object spec_buffer = buffer;
			buffer = Fget_buffer(buffer);
			/* Mention the buffer name for a better error message.  */
			if (NILP(buffer))
				CHECK_BUFFER(spec_buffer);
			CHECK_BUFFER(buffer);
		}
	} else
		buffer = Qnil;

	UNGCPRO;

	display = ((nargs >= 4) ? args[3] : Qnil);

	/* From here we assume we won't GC (unless an error is signaled). */
	{
		REGISTER int i;
		for (i = 4; i < nargs; i++) {
			CHECK_STRING(args[i]);
			new_argv[i - 3] = (char *)XSTRING_DATA(args[i]);
		}
	}
	new_argv[max(nargs - 3, 1)] = 0;

	if (NILP(path))
		report_file_error("Searching for program",
				  Fcons(args[0], Qnil));
	new_argv[0] = (char *)XSTRING_DATA(path);

	filefd = open((char *)XSTRING_DATA(infile), O_RDONLY | OPEN_BINARY, 0);
	if (filefd < 0)
		report_file_error("Opening process input file",
				  Fcons(infile, Qnil));

	if (INTP(buffer)) {
		fd[1] = open(NULL_DEVICE, O_WRONLY | OPEN_BINARY, 0);
		fd[0] = -1;
	} else {
		if( pipe(fd) < 0 )
			report_file_error("Opening process input file pipe",
					  Fcons(infile, Qnil));

#if 0
		/* Replaced by close_process_descs */
		set_exclusive_use(fd[0]);
#endif
	}

	{
		/* child_setup must clobber environ in systems with true vfork.
		   Protect it from permanent change.  */
		REGISTER char **save_environ = environ;
		REGISTER int fd1 = fd[1];
		int fd_error = fd1;

		/* Record that we're about to create a synchronous process.  */
		synch_process_alive = 1;

		/* These vars record information from process termination.
		   Clear them now before process can possibly terminate,
		   to avoid timing error if process terminates soon.  */
		synch_process_death = 0;
		synch_process_retcode = 0;

		if (NILP(error_file))
			fd_error = open(NULL_DEVICE, O_WRONLY | OPEN_BINARY);
		else if (STRINGP(error_file)) {
			fd_error = open((const char *)XSTRING_DATA(error_file),
					O_WRONLY | O_TRUNC | O_CREAT |
					OPEN_BINARY, CREAT_MODE
			    );
		}

		if (fd_error < 0) {
			int save_errno = errno;
			close(filefd);
			close(fd[0]);
			if (fd1 >= 0)
				close(fd1);
			errno = save_errno;
			report_file_error("Cannot open",
					  Fcons(error_file, Qnil));
		}

		fork_error = Qnil;
		pid = fork();

		if (pid == 0) {
			if (fd[0] >= 0)
				close(fd[0]);
			/* This is necessary because some shells may attempt to
			   access the current controlling terminal and will hang
			   if they are run in the background, as will be the case
			   when XEmacs is started in the background.  Martin
			   Buchholz observed this problem running a subprocess
			   that used zsh to call gzip to uncompress an info
			   file. */
			disconnect_controlling_terminal();
			child_setup(filefd, fd1, fd_error, new_argv,
				    (char *)XSTRING_DATA(current_dir));
		}
		if (fd_error >= 0)
			close(fd_error);

		environ = save_environ;

		/* Close most of our fd's, but not fd[0]
		   since we will use that to read input from.  */
		close(filefd);
		if (fd1 >= 0)
			close(fd1);
	}

	if (!NILP(fork_error))
		signal_error(Qfile_error, fork_error);

	if (pid < 0) {
		int save_errno = errno;
		if (fd[0] >= 0)
			close(fd[0]);
		errno = save_errno;
		report_file_error("Doing fork", Qnil);
	}

	if (INTP(buffer)) {
		if (fd[0] >= 0)
			close(fd[0]);
#if defined (NO_SUBPROCESSES)
		/* If Emacs has been built with asynchronous subprocess support,
		   we don't need to do this, I think because it will then have
		   the facilities for handling SIGCHLD.  */
		wait_without_blocking();
#endif				/* NO_SUBPROCESSES */
		return Qnil;
	}

	{
		int nread;
		int total_read = 0;
		Lisp_Object instream;
		struct gcpro ngcpro1;

		/* Enable sending signal if user quits below.  */
		call_process_exited = 0;

		record_unwind_protect(call_process_cleanup,
				      Fcons(make_int(fd[0]), make_int(pid)));

		/* FSFmacs calls Fset_buffer() here.  We don't have to because
		   we can insert into buffers other than the current one. */
		if (EQ(buffer, Qt))
			XSETBUFFER(buffer, current_buffer);
		instream =
		    make_filedesc_input_stream(fd[0], 0, -1, LSTR_ALLOW_QUIT);
#ifdef FILE_CODING
		instream =
		    make_decoding_input_stream
		    (XLSTREAM(instream),
		     Fget_coding_system(Vcoding_system_for_read));
		Lstream_set_character_mode(XLSTREAM(instream));
#endif
		NGCPRO1(instream);
		while (1) {
			QUIT;
			/* Repeatedly read until we've filled as much as possible
			   of the buffer size we have.  But don't read
			   less than 1024--save that for the next bufferfull.  */

			nread = 0;
			while (nread < bufsize - 1024) {
				Lstream_data_count this_read
				    =
				    Lstream_read(XLSTREAM(instream),
						 bufptr + nread,
						 bufsize - nread);

				if (this_read < 0)
					goto give_up;

				if (this_read == 0)
					goto give_up_1;

				nread += this_read;
			}

		      give_up_1:

			/* Now NREAD is the total amount of data in the buffer.  */
			if (nread == 0)
				break;


			total_read += nread;

			if (!NILP(buffer))
				buffer_insert_raw_string(XBUFFER(buffer),
							 (Bufbyte *) bufptr,
							 nread);

			/* Make the buffer bigger as we continue to read more data,
			   but not past 64k.  */
			if (bufsize < 64 * 1024 && total_read > 32 * bufsize) {
				bufsize *= 2;
				bufptr = (char *)alloca(bufsize);
			}

			if (!NILP(display) && INTERACTIVE) {
				redisplay();
			}
		}
	      give_up:
		Lstream_close(XLSTREAM(instream));
		NUNGCPRO;

		QUIT;
		/* Wait for it to terminate, unless it already has.  */
		wait_for_termination(pid);

		/* Don't kill any children that the subprocess may have left behind
		   when exiting.  */
		call_process_exited = 1;
		unbind_to(speccount, Qnil);

		if (synch_process_death)
			return build_string(synch_process_death);
		return make_int(synch_process_retcode);
	}
}

static int max_filedesc(void)
{
	/* Cache it to avoid calling getrlimit all the time.
	   It won't really change over time 
	*/
	static int maxfd = -1;

	if (maxfd >= 0)
		return maxfd;
	maxfd = MAXDESC;

#  ifdef HAVE_GETRLIMIT64
	struct rlimit64 rlim;
	(void)getrlimit64(RLIMIT_NOFILE, &rlim);
	maxfd = rlim.rlim_cur;
#  elif  HAVE_GETRLIMIT
	struct rlimit rlim;
	(void)getrlimit(RLIMIT_NOFILE, &rlim);
	maxfd = rlim.rlim_cur;
#  endif

	return maxfd;
}


/* Move the file descriptor FD so that its number is not less than MIN. *
   The original file descriptor remains open.  */
static int relocate_fd(int fd, int minfd)
{
	int newfd = -1;

	if (minfd < 0) {
		stderr_out("Bad relocated_fd minimum file descriptor: %d\n", 
			   minfd);
		_exit(1);
	}
	if (fd >= minfd)
		return fd;
	
	newfd = dup(fd);

	if (newfd == -1) {
		stderr_out("Error while setting up child: %s\n",
			   strerror(errno));
		_exit(1);
	}
	if (newfd >= minfd )
		return newfd;
	else {
		int recurse_fd = relocate_fd(newfd, minfd);
		/* Close all the previously recursivelly dup'ed
		   file descriptors 
		*/
		close(newfd);
		return recurse_fd;
	}
}

/* This is the last thing run in a newly forked inferior
   either synchronous or asynchronous.
   Copy descriptors IN, OUT and ERR
   as descriptors STDIN_FILENO, STDOUT_FILENO, and STDERR_FILENO.
   Initialize inferior's priority, pgrp, connected dir and environment.
   then exec another program based on new_argv.

   This function may change environ for the superior process.
   Therefore, the superior process must save and restore the value
   of environ around the fork and the call to this function.

   ENV is the environment for the subprocess.

   XEmacs: We've removed the SET_PGRP argument because it's already
   done by the callers of child_setup.

   CURRENT_DIR is an elisp string giving the path of the current
   directory the subprocess should have.  Since we can't really signal
   a decent error from within the child, this should be verified as an
   executable directory by the parent.  */

void
child_setup(int in, int out, int err, char **new_argv, const char *current_dir)
{
	char **env;
	char *pwd;

#ifdef SET_EMACS_PRIORITY
	if (emacs_priority != 0)
		nice(-emacs_priority);
#endif

	/* Under Windows, we are not in a child process at all, so we should
	   not close handles inherited from the parent -- we are the parent
	   and doing so will screw up all manner of things!  Similarly, most
	   of the rest of the cleanup done in this function is not done
	   under Windows.

	   #### This entire child_setup() function is an utter and complete
	   piece of shit.  I would rewrite it, at the very least splitting
	   out the Windows and non-Windows stuff into two completely
	   different functions; but instead I'm trying to make it go away
	   entirely, using the Lisp definition in process.el.  What's left
	   is to fix up the routines in event-msw.c (and in event-Xt.c and
	   event-tty.c) to allow for stream devices to be handled correctly.
	   There isn't much to do, in fact, and I'll fix it shortly.  That
	   way, the Lisp definition can be used non-interactively too. */
#if !defined (NO_SUBPROCESSES)
	/* Close Emacs's descriptors that this process should not have.  */
	close_process_descs();
#endif				/* not NO_SUBPROCESSES */
	close_load_descs();

	/* Note that use of alloca is always safe here.  It's obvious for systems
	   that do not have true vfork or that have true (stack) alloca.
	   If using vfork and C_ALLOCA it is safe because that changes
	   the superior's static variables as if the superior had done alloca
	   and will be cleaned up in the usual way.  */
	{
		REGISTER int i;

		i = strlen(current_dir);
		pwd = alloca_array(char, i + 6);
		memcpy(pwd, "PWD=", 4);
		memcpy(pwd + 4, current_dir, i);
		i += 4;
		if (!IS_DIRECTORY_SEP(pwd[i - 1]))
			pwd[i++] = DIRECTORY_SEP;
		pwd[i] = 0;

		/* We can't signal an Elisp error here; we're in a vfork.  Since
		   the callers check the current directory before forking, this
		   should only return an error if the directory's permissions
		   are changed between the check and this chdir, but we should
		   at least check.  */
		if (chdir(pwd + 4) < 0) {
			/* Don't report the chdir error, or ange-ftp.el doesn't work. */
			/* (FSFmacs does _exit (errno) here.) */
			pwd = 0;
		} else {
			/* Strip trailing "/".  Cretinous *[]&@$#^%@#$% Un*x */
			/* leave "//" (from FSF) */
			while (i > 6 && IS_DIRECTORY_SEP(pwd[i - 1]))
				pwd[--i] = 0;
		}
	}

	/* Set `env' to a vector of the strings in Vprocess_environment.  */
	/* + 2 to include PWD and terminating 0.  */
	env = alloca_array(char *, XINT(Flength(Vprocess_environment)) + 2);
	{
		REGISTER Lisp_Object tail;
		char **new_env = env;

		/* If we have a PWD envvar and we know the real current directory,
		   pass one down, but with corrected value.  */
		if (pwd && getenv("PWD"))
			*new_env++ = pwd;

		/* Copy the Vprocess_environment strings into new_env.  */
		for (tail = Vprocess_environment;
		     CONSP(tail) && STRINGP(XCAR(tail)); tail = XCDR(tail)) {
			char **ep = env;
			char *envvar_external;

			TO_EXTERNAL_FORMAT(LISP_STRING, XCAR(tail),
					   C_STRING_ALLOCA, envvar_external,
					   Qfile_name);

			/* See if envvar_external duplicates any string already in the env.
			   If so, don't put it in.
			   When an env var has multiple definitions,
			   we keep the definition that comes first in process-environment.  */
			for (; ep != new_env; ep++) {
				char *p = *ep, *q = envvar_external;
				while (1) {
					if (*q == 0)
						/* The string is malformed; might as well drop it.  */
						goto duplicate;
					if (*q != *p)
						break;
					if (*q == '=')
						goto duplicate;
					p++, q++;
				}
			}
			if (pwd && !strncmp("PWD=", envvar_external, 4)) {
				*new_env++ = pwd;
				pwd = 0;
			} else
				*new_env++ = envvar_external;

		      duplicate:;
		}
		*new_env = 0;
	}

	/* Make sure that in, out, and err are not actually already in
	   descriptors zero, one, or two; this could happen if Emacs is
	   started with its standard in, out, or error closed, as might
	   happen under X.  */
	in = relocate_fd(in, 3);
	out = relocate_fd(out, 3);
	err = relocate_fd(err, 3);


#ifdef HAVE_DUP2
	/* dup2 will automatically close STD* handles before duping
	   them 
	*/
	dup2(in, STDIN_FILENO);
	dup2(out, STDOUT_FILENO);
	dup2(err, STDERR_FILENO);
#else
	/* Set the standard input/output channels of the new process.  */
	close(STDIN_FILENO);
	close(STDOUT_FILENO);
	close(STDERR_FILENO);

	/* Sub-optimally hoping that dup will use the just closed STD*
	   handles */
	dup(in);
	dup(out);
	dup(err);
#endif

	close(in);
	close(out);
	close(err);

	/* Close non-process-related file descriptors. It would be cleaner to
	   close just the ones that need to be, but the following brute
	   force approach is certainly effective, and not too slow. */

	{
		int fd;

		for (fd = 3; fd < max_filedesc(); fd++)
			close(fd);
	}

#ifdef vipc
	something missing here;
#endif				/* vipc */

	/* execvp does not accept an environment arg so the only way
	   to pass this environment is to set environ.  Our caller
	   is responsible for restoring the ambient value of environ.  */
	environ = env;
	execvp(new_argv[0], new_argv);

	stdout_out("Can't exec program %s\n", new_argv[0]);
	_exit(1);
}

static int
getenv_internal(const Bufbyte * var,
		Bytecount varlen, Bufbyte ** value, Bytecount * valuelen)
{
	Lisp_Object scan;

	for (scan = Vprocess_environment; CONSP(scan); scan = XCDR(scan)) {
		Lisp_Object entry = XCAR(scan);

		if (STRINGP(entry)
		    && XSTRING_LENGTH(entry) > varlen
		    && XSTRING_BYTE(entry, varlen) == '='
		    && !memcmp(XSTRING_DATA(entry), var, varlen)
		    ) {
			*value = XSTRING_DATA(entry) + (varlen + 1);
			*valuelen = XSTRING_LENGTH(entry) - (varlen + 1);
			return 1;
		}
	}

	return 0;
}

DEFUN("getenv", Fgetenv, 1, 2, "sEnvironment variable: \np",	/*
Return the value of environment variable VAR, as a string.
VAR is a string, the name of the variable.
When invoked interactively, prints the value in the echo area.
*/
      (var, interactivep))
{
	Bufbyte *value = NULL;
	Bytecount valuelen;
	Lisp_Object v = Qnil;
	struct gcpro gcpro1;

	CHECK_STRING(var);
	GCPRO1(v);
	if (getenv_internal(XSTRING_DATA(var), XSTRING_LENGTH(var),
			    &value, &valuelen)) {
		v = make_string(value, valuelen);
	}
	if (!NILP(interactivep)) {
		if (NILP(v))
			message("%s not defined in environment",
				XSTRING_DATA(var));
		else
			/* #### Should use Fprin1_to_string or Fprin1 to handle string
			   containing quotes correctly.  */
			message("\"%s\"", value);
	}
	RETURN_UNGCPRO(v);
}

/* A version of getenv that consults process_environment, easily
   callable from C.  */
char *egetenv(const char *var)
{
	/* This cannot GC -- 7-28-00 ben */
	Bufbyte *value;
	Bytecount valuelen;

	if (getenv_internal
	    ((const Bufbyte *)var, strlen(var), &value, &valuelen))
		return (char *)value;
	else
		return 0;
}

void init_callproc(void)
{
	/* This function can GC */

	{
		/* jwz: always initialize Vprocess_environment, so that egetenv()
		   works in temacs. */
		char **envp;
		Vprocess_environment = Qnil;
		for (envp = environ; envp && *envp; envp++)
			Vprocess_environment =
			    Fcons(build_ext_string(*envp, Qfile_name),
				  Vprocess_environment);
	}

	{
		/* Initialize shell-file-name from environment variables or best guess. */
		const char *shell = egetenv("SHELL");
		if (!shell)
			shell = "/bin/sh";
		Vshell_file_name = build_string(shell);
	}
}

#if 0
void set_process_environment(void)
{
	REGISTER char **envp;

	Vprocess_environment = Qnil;
#ifndef CANNOT_DUMP
	if (initialized)
#endif
		for (envp = environ; *envp; envp++)
			Vprocess_environment = Fcons(build_string(*envp),
						     Vprocess_environment);
}
#endif				/* unused */

void syms_of_callproc(void)
{
	DEFSUBR(Fold_call_process_internal);
	DEFSUBR(Fgetenv);
}

void vars_of_callproc(void)
{
	/* This function can GC */

	DEFVAR_LISP("shell-file-name", &Vshell_file_name	/*
*File name to load inferior shells from.
Initialized from the SHELL environment variable.
								 */ );

	DEFVAR_LISP("process-environment", &Vprocess_environment	/*
List of environment variables for subprocesses to inherit.
Each element should be a string of the form ENVVARNAME=VALUE.
The environment which Emacs inherits is placed in this variable
when Emacs starts.
									 */ );

	Vlisp_EXEC_SUFFIXES = build_string(EXEC_SUFFIXES);
	staticpro(&Vlisp_EXEC_SUFFIXES);
}
