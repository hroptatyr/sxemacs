/* Asynchronous subprocess implementation for UNIX
   Copyright (C) 1985, 1986, 1987, 1988, 1992, 1993, 1994, 1995
   Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996 Ben Wing.

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


/* This file has been Mule-ized except for `start-process-internal',
   `open-network-stream-internal' and `open-multicast-group-internal'. */

/* This file has been split into process.c and process-unix.c by
   Kirill M. Katsnelson <kkm@kis.ru>, so please bash him and not
   the original author(s) */

/* The IPv6 support is derived from the code for GNU Emacs-20.3
   written by Wolfgang S. Rupprecht */

#include <config.h>

#if !defined (NO_SUBPROCESSES)

/* The entire file is within this conditional */

#include "lisp.h"

#include "buffer.h"
#include "events/events.h"
#include "ui/frame.h"
#include "hash.h"
#include "lstream.h"
#include "opaque.h"
#include "process.h"
#include "procimpl.h"
#include "sysdep.h"
#include "ui/window.h"
#ifdef FILE_CODING
#include "mule/file-coding.h"
#endif

#include <setjmp.h>
#include "sysfile.h"
#include "sysproc.h"
#include "systime.h"
#include "syssignal.h"		/* Always include before systty.h */
#include "ui/TTY/systty.h"
#include "syswait.h"

#ifdef HPUX
#include <grp.h>		/* See grantpt fixups for HPUX below. */
#endif

#if defined(HAVE_OPENSSL) && defined(OPENSSL_SSL)
#include "openssl.h"
#endif


/*
 * Implementation-specific data. Pointed to by Lisp_Process->process_data
 */

struct unix_process_data {
	/* Always 0.  Used to be for tooltalk only. */
	int connected_via_filedesc_p;
	/* Descriptor by which we read from this process.  -1 for dead process */
	int infd;
	/* Descriptor for the tty which this process is using.
	   -1 if we didn't record it (on some systems, there's no need).  */
	int subtty;
	/* Name of subprocess terminal. */
	Lisp_Object tty_name;
	/* Non-false if communicating through a pty.  */
	char pty_flag;
};


extern struct hash_table *usid_to_process;


#define UNIX_DATA(p) ((struct unix_process_data*)((p)->process_data))

/**********************************************************************/
/*                    Static helper routines                          */
/**********************************************************************/

static SIGTYPE close_safely_handler(int signo)
{
	EMACS_REESTABLISH_SIGNAL(signo, close_safely_handler);
	SIGRETURN;
}

static void close_safely(int fd)
{
	stop_interrupts();
	signal(SIGALRM, close_safely_handler);
	alarm(1);
	close(fd);
	alarm(0);
	start_interrupts();
}

static void close_descriptor_pair(int in, int out)
{
	if (in >= 0)
		close(in);
	if (out != in && out >= 0)
		close(out);
}

/* Close all descriptors currently in use for communication
   with subprocess.  This is used in a newly-forked subprocess
   to get rid of irrelevant descriptors.  */

static int
close_process_descs_mapfun(const void *key, void *contents, void *arg)
{
	Lisp_Object proc;
	CVOID_TO_LISP(proc, contents);
	event_stream_delete_stream_pair(XPROCESS(proc)->pipe_instream,
					XPROCESS(proc)->pipe_outstream);
	return 0;
}

/* #### This function is currently called from child_setup
   in callproc.c. It should become static though - kkm */
void close_process_descs(void)
{
	maphash(close_process_descs_mapfun, usid_to_process, 0);
}

/* connect to an existing file descriptor.  This is very similar to
   open-network-stream except that it assumes that the connection has
   already been initialized.  It is currently used for ToolTalk
   communication. */

/* This function used to be visible on the Lisp level, but there is no
   real point in doing that.  Here is the doc string:

  "Connect to an existing file descriptor.
Return a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER INFD OUTFD.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may also be nil, meaning that this process is not associated
 with any buffer.
INFD and OUTFD specify the file descriptors to use for input and
 output, respectively."
*/

Lisp_Object
connect_to_file_descriptor(Lisp_Object name, Lisp_Object buffer,
			   Lisp_Object infd, Lisp_Object outfd)
{
	/* This function can GC */
	Lisp_Object proc;
	EMACS_INT inch, outch;

	CHECK_STRING(name);
	CHECK_INT(infd);
	CHECK_INT(outfd);

	inch = XINT(infd);
	outch = XINT(outfd);
	if (get_process_from_usid(FD_TO_USID(inch)))
		invalid_operation("There is already a process connected to fd",
				  infd);
	if (!NILP(buffer))
		buffer = Fget_buffer_create(buffer);
	proc = make_process_internal(name);

	XPROCESS(proc)->pid = Fcons(infd, name);
	XPROCESS(proc)->buffer = buffer;
	init_process_io_handles(XPROCESS(proc), (void*)inch, (void*)outch, 0);
	UNIX_DATA(XPROCESS(proc))->connected_via_filedesc_p = 1;

	event_stream_select_process(XPROCESS(proc));

	return proc;
}

#ifdef HAVE_PTYS
static int allocate_pty_the_old_fashioned_way(void);

/* The file name of the (slave) pty opened by allocate_pty().  */
#ifndef MAX_PTYNAME_LEN
#define MAX_PTYNAME_LEN 64
#endif
static char pty_name[MAX_PTYNAME_LEN];

/* Open an available pty, returning a file descriptor.
   Return -1 on failure.
   The file name of the terminal corresponding to the pty
   is left in the variable `pty_name'.  */

static int allocate_pty(void)
{
	/* Unix98 standardized grantpt, unlockpt, and ptsname, but not the
	   functions required to open a master pty in the first place :-(

	   Modern Unix systems all seems to have convenience methods to open
	   a master pty fd in one function call, but there is little
	   agreement on how to do it.

	   allocate_pty() tries all the different known easy ways of opening
	   a pty.  In case of failure, we resort to the old BSD-style pty
	   grovelling code in allocate_pty_the_old_fashioned_way(). */
#ifndef FORCE_ALLOCATE_PTY_THE_OLD_FASHIONED_WAY
	int master_fd = -1;
	const char *slave_name = NULL;
	const char *_clone_ = NULL;
	static const char *const clones[] = {
		/* Different pty master clone devices */
		"/dev/ptmx",	/* Various systems */
		"/dev/ptm/clone",	/* HPUX */
		"/dev/ptc",	/* AIX */
		"/dev/ptmx_bsd"	/* Tru64 */
	};

#ifdef HAVE_GETPT		/* glibc */
	master_fd = getpt();
	if (master_fd >= 0)
		goto have_master;
#endif				/* HAVE_GETPT */

#if defined(HAVE_OPENPTY)	/* BSD, Tru64, glibc */
	{
		int slave_fd = -1;
		int rc;
		EMACS_BLOCK_SIGNAL(SIGCHLD);
		rc = openpty(&master_fd, &slave_fd, NULL, NULL, NULL);
		EMACS_UNBLOCK_SIGNAL(SIGCHLD);
		if (rc == 0) {
			slave_name = ttyname(slave_fd);
			close(slave_fd);
			goto have_slave_name;
		} else {
			if (master_fd >= 0)
				close(master_fd);
			if (slave_fd >= 0)
				close(slave_fd);
		}
	}
#endif				/* HAVE_OPENPTY */

#if defined(HAVE__GETPTY) && defined (O_NDELAY)	/* SGI */
	master_fd = -1;
	EMACS_BLOCK_SIGNAL(SIGCHLD);
	slave_name = _getpty(&master_fd, O_RDWR | O_NDELAY, 0600, 0);
	EMACS_UNBLOCK_SIGNAL(SIGCHLD);
	if (master_fd >= 0 && slave_name != NULL)
		goto have_slave_name;
#endif				/* HAVE__GETPTY */

	/* Master clone devices are available on most systems */
	{
		int i;
		for (i = 0; i < countof(clones); i++) {
			_clone_ = clones[i];
			master_fd =
			    open(_clone_, O_RDWR | O_NONBLOCK | OPEN_BINARY, 0);
			if (master_fd >= 0)
				goto have_master;
		}
		_clone_ = NULL;
	}

	goto lose;

      have_master:

#if defined (HAVE_PTSNAME)
	slave_name = ptsname(master_fd);
	if (slave_name)
		goto have_slave_name;
#endif

	/* AIX docs say to use ttyname, not ptsname, to get slave_name */
	if (_clone_ && !strcmp(_clone_, "/dev/ptc")
	    && (slave_name = ttyname(master_fd)) != NULL)
		goto have_slave_name;

	goto lose;

      have_slave_name:
	if( slave_name != NULL )
		strncpy(pty_name, slave_name, sizeof(pty_name));
	else
		strncpy(pty_name, "<NULL ttyname>", sizeof(pty_name));
	pty_name[sizeof(pty_name) - 1] = '\0';
	setup_pty(master_fd);

	/* We jump through some hoops to frob the pty.
	   It's not obvious that checking the return code here is useful. */

	/* "The grantpt() function will fail if it is unable to successfully
	   invoke the setuid root program.  It may also fail if the
	   application has installed a signal handler to catch SIGCHLD
	   signals." */
#if defined (HAVE_GRANTPT) || defined (HAVE_UNLOCKPT)
	EMACS_BLOCK_SIGNAL(SIGCHLD);

#if defined (HAVE_GRANTPT)
	grantpt(master_fd);
#ifdef HPUX
	/* grantpt() behavior on some versions of HP-UX differs from what's
	   specified in the man page: the group of the slave PTY is set to
	   the user's primary group, and we fix that. */
	{
		struct group *tty_group = getgrnam("tty");
		if (tty_group != NULL)
			chown(pty_name, (uid_t) - 1, tty_group->gr_gid);
	}
#endif				/* HPUX has broken grantpt() */
#endif				/* HAVE_GRANTPT */

#if defined (HAVE_UNLOCKPT)
	unlockpt(master_fd);
#endif

	EMACS_UNBLOCK_SIGNAL(SIGCHLD);
#endif				/* HAVE_GRANTPT || HAVE_UNLOCKPT */

	return master_fd;

      lose:
	if (master_fd >= 0)
		close(master_fd);
#endif				/* ndef FORCE_ALLOCATE_PTY_THE_OLD_FASHIONED_WAY */
	return allocate_pty_the_old_fashioned_way();
}

/* This function tries to allocate a pty by iterating through file
   pairs with names like /dev/ptyp1 and /dev/ttyp1. */
static int allocate_pty_the_old_fashioned_way(void)
{
	struct stat stb;

	/* Some systems name their pseudoterminals so that there are gaps in
	   the usual sequence - for example, on HP9000/S700 systems, there
	   are no pseudoterminals with names ending in 'f'.  So we wait for
	   three failures in a row before deciding that we've reached the
	   end of the ptys.  */
	int failed_count = 0;
	int fd;
	int i;
	int c;

#ifdef PTY_ITERATION
	PTY_ITERATION
#else
# ifndef FIRST_PTY_LETTER
# define FIRST_PTY_LETTER 'p'
# endif
	for (c = FIRST_PTY_LETTER; c <= 'z'; c++)
		for (i = 0; i < 16; i++)
#endif				/* PTY_ITERATION */

		{
			int sz;

#ifdef PTY_NAME_SPRINTF
			PTY_NAME_SPRINTF;
#else
			sz = snprintf(pty_name, sizeof(pty_name), "/dev/pty%c%x", c, i);
			assert(sz >= 0 && (size_t)sz < sizeof(pty_name));
#endif				/* no PTY_NAME_SPRINTF */

			if (sxemacs_stat(pty_name, &stb) < 0) {
				if (++failed_count >= 3)
					return -1;
			} else
				failed_count = 0;
			fd = open(pty_name, O_RDWR | O_NONBLOCK | OPEN_BINARY,
				  0);

			if (fd >= 0) {
#ifdef PTY_TTY_NAME_SPRINTF
				PTY_TTY_NAME_SPRINTF;
#else
				sz = snprintf(pty_name, sizeof(pty_name),
						  "/dev/tty%c%x", c, i);
				assert(sz >= 0 && (size_t)sz < sizeof(pty_name));
#endif				/* no PTY_TTY_NAME_SPRINTF */
				if (access(pty_name, R_OK | W_OK) == 0) {
					setup_pty(fd);
					return fd;
				}
				close(fd);
			}
		}		/* iteration */
	return -1;
}
#endif				/* HAVE_PTYS */

static int
create_bidirectional_pipe(long int *inchannel, long int *outchannel,
			  volatile int *forkin, volatile int *forkout)
{
	int sv[2];

#ifdef SKTPAIR
	if (socketpair(AF_UNIX, SOCK_STREAM, 0, sv) < 0)
		return -1;
	*outchannel = *inchannel = sv[0];
	*forkout = *forkin = sv[1];
#else				/* not SKTPAIR */
	int temp;
	temp = pipe(sv);
	if (temp < 0)
		return -1;
	*inchannel = sv[0];
	*forkout = sv[1];
	temp = pipe(sv);
	if (temp < 0)
		return -1;
	*outchannel = sv[1];
	*forkin = sv[0];
#endif				/* not SKTPAIR */
	return 0;
}

#ifdef HAVE_SOCKETS

#if !(defined(HAVE_GETADDRINFO) && defined(HAVE_GETNAMEINFO))
static int
get_internet_address(Lisp_Object host, struct sockaddr_in *address,
		     Error_behavior errb)
{
	struct hostent *host_info_ptr = NULL;
#ifdef TRY_AGAIN
	int count = 0;
#endif

	xzero(*address);

	while (1) {
#ifdef TRY_AGAIN
		if (count++ > 10)
			break;
		h_errno = 0;
#endif
		/* Some systems can't handle SIGIO/SIGALARM in gethostbyname. */
		slow_down_interrupts();
		host_info_ptr = gethostbyname((char *)XSTRING_DATA(host));
		speed_up_interrupts();
#ifdef TRY_AGAIN
		if (!(host_info_ptr == 0 && h_errno == TRY_AGAIN))
#endif
			break;
		Fsleep_for(make_int(1));
	}
	if (host_info_ptr) {
		address->sin_family = host_info_ptr->h_addrtype;
		memcpy(&address->sin_addr, host_info_ptr->h_addr,
		       host_info_ptr->h_length);
	} else {
		IN_ADDR numeric_addr;
		/* Attempt to interpret host as numeric inet address */
		numeric_addr = inet_addr((char *)XSTRING_DATA(host));
		if (NUMERIC_ADDR_ERROR) {
			maybe_error(Qprocess, errb,
				    "Unknown host \"%s\"", XSTRING_DATA(host));
			return 0;
		}

		/* There was some broken code here that called strlen() here
		   on (char *) &numeric_addr and even sometimes accessed
		   uninitialized data. */
		address->sin_family = AF_INET;
		*(IN_ADDR *) & address->sin_addr = numeric_addr;
	}

	return 1;
}
#endif				/*  !(HAVE_GETADDRINFO && HAVE_GETNAMEINFO) */

static void set_socket_nonblocking_maybe(int fd, int port, const char *proto)
{
#ifdef PROCESS_IO_BLOCKING
	Lisp_Object tail;

	for (tail = network_stream_blocking_port_list; CONSP(tail);
	     tail = XCDR(tail)) {
		Lisp_Object tail_port = XCAR(tail);

		if (STRINGP(tail_port)) {
			struct servent *svc_info;
			CHECK_STRING(tail_port);
			svc_info =
			    getservbyname((char *)XSTRING_DATA(tail_port),
					  proto);
			if ((svc_info != 0) && (svc_info->s_port == port))
				break;
			else
				continue;
		} else if (INTP(tail_port)
			   && (htons((unsigned short)XINT(tail_port)) == port))
			break;
	}

	if (!CONSP(tail)) {
		set_descriptor_non_blocking(fd);
	}
#else
	set_descriptor_non_blocking(fd);
#endif				/* PROCESS_IO_BLOCKING */
}

#endif				/* HAVE_SOCKETS */

/* Compute the Lisp form of the process status from
   the numeric status that was returned by `wait'.  */

static void update_status_from_wait_code(Lisp_Process * p, int *w_fmh)
{
	/* C compiler lossage when attempting to pass w directly */
	int w = *w_fmh;

	if (WIFSTOPPED(w)) {
		p->status_symbol = Qstop;
		p->exit_code = WSTOPSIG(w);
		p->core_dumped = 0;
	} else if (WIFEXITED(w)) {
		p->status_symbol = Qexit;
		p->exit_code = WEXITSTATUS(w);
		p->core_dumped = 0;
	} else if (WIFSIGNALED(w)) {
		p->status_symbol = Qsignal;
		p->exit_code = WTERMSIG(w);
		p->core_dumped = WCOREDUMP(w);
	} else {
		p->status_symbol = Qrun;
		p->exit_code = 0;
	}
}

#ifdef SIGCHLD

#define MAX_EXITED_PROCESSES 1000
static volatile pid_t exited_processes[MAX_EXITED_PROCESSES];
static volatile int exited_processes_status[MAX_EXITED_PROCESSES];
static volatile int exited_processes_index;

static volatile int sigchld_happened;

/* On receipt of a signal that a child status has changed,
 loop asking about children with changed statuses until
 the system says there are no more.  All we do is record
 the processes and wait status.

 This function could be called from within the SIGCHLD
 handler, so it must be completely reentrant.  When
 not called from a SIGCHLD handler, BLOCK_SIGCHLD should
 be non-zero so that SIGCHLD is blocked while this
 function is running. (This is necessary so avoid
 race conditions with the SIGCHLD_HAPPENED flag). */

static void record_exited_processes(int block_sigchld)
{
	if (!sigchld_happened) {
		return;
	}
#ifdef EMACS_BLOCK_SIGNAL
	if (block_sigchld)
		EMACS_BLOCK_SIGNAL(SIGCHLD);
#endif

	while (sigchld_happened) {
		int pid;
		int w;

		/* Keep trying to get a status until we get a definitive result.  */
		do {
			errno = 0;
#ifdef WNOHANG
#  ifndef WUNTRACED
#    define WUNTRACED 0
#  endif			/* not WUNTRACED */
#  ifdef HAVE_WAITPID
			pid = waitpid((pid_t) - 1, &w, WNOHANG | WUNTRACED);
#  else
			pid = wait3(&w, WNOHANG | WUNTRACED, 0);
#  endif
#else				/* not WNOHANG */
			pid = wait(&w);
#endif				/* not WNOHANG */
		}
		while (pid <= 0 && errno == EINTR);

		if (pid <= 0)
			break;

		if (exited_processes_index < MAX_EXITED_PROCESSES) {
			exited_processes[exited_processes_index] = pid;
			exited_processes_status[exited_processes_index] = w;
			exited_processes_index++;
		}

		/* On systems with WNOHANG, we just ignore the number
		   of times that SIGCHLD was signalled, and keep looping
		   until there are no more processes to wait on.  If we
		   don't have WNOHANG, we have to rely on the count in
		   SIGCHLD_HAPPENED. */
#ifndef WNOHANG
		sigchld_happened--;
#endif				/* not WNOHANG */
	}

	sigchld_happened = 0;

	if (block_sigchld)
		EMACS_UNBLOCK_SIGNAL(SIGCHLD);
}

/* For any processes that have changed status and are recorded
   and such, update the corresponding Lisp_Process.
   We separate this from record_exited_processes() so that
   we never have to call this function from within a signal
   handler.  We block SIGCHLD in case record_exited_processes()
   is called from a signal handler. */

/** USG WARNING:  Although it is not obvious from the documentation
 in signal(2), on a USG system the SIGCLD handler MUST NOT call
 signal() before executing at least one wait(), otherwise the handler
 will be called again, resulting in an infinite loop.  The relevant
 portion of the documentation reads "SIGCLD signals will be queued
 and the signal-catching function will be continually reentered until
 the queue is empty".  Invoking signal() causes the kernel to reexamine
 the SIGCLD queue.   Fred Fish, UniSoft Systems Inc.

 (Note that now this only applies in SYS V Release 2 and before.
 On SYS V Release 3, we use sigset() to set the signal handler for
 the first time, and so we don't have to reestablish the signal handler
 in the handler below.  On SYS V Release 4, we don't get this weirdo
 behavior when we use sigaction(), which we do use.) */

static SIGTYPE sigchld_handler(int signo)
{
#ifdef OBNOXIOUS_SYSV_SIGCLD_BEHAVIOR
	int old_errno = errno;

	sigchld_happened++;
	record_exited_processes(0);
	errno = old_errno;
#else
	sigchld_happened++;
#endif
#ifdef HAVE_UNIXOID_EVENT_LOOP
	signal_fake_event();
#endif
	/* WARNING - must come after wait3() for USG systems */
	EMACS_REESTABLISH_SIGNAL(signo, sigchld_handler);
	SIGRETURN;
}

#endif				/* SIGCHLD */

#ifdef SIGNALS_VIA_CHARACTERS
/* Get signal character to send to process if SIGNALS_VIA_CHARACTERS */

static int process_signal_char(int tty_fd, int signo)
{
	/* Invalid tty_fd ... */
	if (tty_fd < 0)
		return '\0';

	/* If it's not a tty, pray that these default values work */
	if (!isatty(tty_fd)) {
#define CNTL(ch) (037 & (ch))
		switch (signo) {
		case SIGINT:
			return CNTL('C');
		case SIGQUIT:
			return CNTL('\\');
#ifdef SIGTSTP
		case SIGTSTP:
			return CNTL('Z');
#endif
		default:
			break;
		}
	}
#ifdef HAVE_TERMIOS
	/* TERMIOS is the latest and bestest, and seems most likely to work.
	   If the system has it, use it. */
	{
		struct termios t;
		tcgetattr(tty_fd, &t);
		switch (signo) {
		case SIGINT:
			return t.c_cc[VINTR];
		case SIGQUIT:
			return t.c_cc[VQUIT];
#if defined(SIGTSTP) && defined(VSUSP)
		case SIGTSTP:
			return t.c_cc[VSUSP];
#endif
		default:
			break;
		}
	}

# elif defined (TIOCGLTC) && defined (TIOCGETC)	/* not HAVE_TERMIOS */
	{
		/* On Berkeley descendants, the following IOCTL's retrieve the
		   current control characters.  */
		struct tchars c;
		struct ltchars lc;
		switch (signo) {
		case SIGINT:
			ioctl(tty_fd, TIOCGETC, &c);
			return c.t_intrc;
		case SIGQUIT:
			ioctl(tty_fd, TIOCGETC, &c);
			return c.t_quitc;
#  ifdef SIGTSTP
		case SIGTSTP:
			ioctl(tty_fd, TIOCGLTC, &lc);
			return lc.t_suspc;
#  endif			/* SIGTSTP */
		}
	}

# elif defined (TCGETA)		/* ! defined (TIOCGLTC) && defined (TIOCGETC) */
	{
		/* On SYSV descendants, the TCGETA ioctl retrieves the current
		   control characters.  */
		struct termio t;
		ioctl(tty_fd, TCGETA, &t);
		switch (signo) {
		case SIGINT:
			return t.c_cc[VINTR];
		case SIGQUIT:
			return t.c_cc[VQUIT];
#  ifdef SIGTSTP
		case SIGTSTP:
			return t.c_cc[VSWTCH];
#  endif			/* SIGTSTP */
		}
	}
# else				/* ! defined (TCGETA) */
#error ERROR! Using SIGNALS_VIA_CHARACTERS, but not HAVE_TERMIOS || (TIOCGLTC && TIOCGETC) || TCGETA
	/* If your system configuration files define SIGNALS_VIA_CHARACTERS,
	   you'd better be using one of the alternatives above!  */
# endif				/* ! defined (TCGETA) */
	return '\0';
}
#endif				/* SIGNALS_VIA_CHARACTERS */

/**********************************************************************/
/*              Process implementation methods                        */
/**********************************************************************/

/*
 * Allocate and initialize Lisp_Process->process_data
 */

static void unix_alloc_process_data(Lisp_Process * p)
{
	p->process_data = xnew(struct unix_process_data);

	UNIX_DATA(p)->connected_via_filedesc_p = 0;
	UNIX_DATA(p)->infd = -1;
	UNIX_DATA(p)->subtty = -1;
	UNIX_DATA(p)->tty_name = Qnil;
	UNIX_DATA(p)->pty_flag = 0;
}

/*
 * Mark any Lisp objects in Lisp_Process->process_data
 */

static void unix_mark_process_data(Lisp_Process * proc)
{
	mark_object(UNIX_DATA(proc)->tty_name);
}

/*
 * Initialize SXEmacs process implementation once
 */

#ifdef SIGCHLD
static void unix_init_process(void)
{
#ifndef CANNOT_DUMP
	if (!noninteractive || initialized)
#endif
		signal(SIGCHLD, sigchld_handler);
}
#endif				/* SIGCHLD */

/*
 * Initialize any process local data. This is called when newly
 * created process is connected to real OS file handles. The
 * handles are generally represented by void* type, but are
 * of type int (file descriptors) for UNIX.
 */

static void
unix_init_process_io_handles(Lisp_Process * p, void *in, void *out, int flags)
{
	Lisp_Object process = Qnil;
	USID        usid = FD_TO_USID((EMACS_INT)in);
	XSETPROCESS(process, p);
	puthash((const void *)usid, LISP_TO_VOID(process),
		usid_to_process);
	UNIX_DATA(p)->infd = (EMACS_INT)in;
}

/*
 * Fork off a subprocess. P is a pointer to a newly created subprocess
 * object. If this function signals, the caller is responsible for
 * deleting (and finalizing) the process object.
 *
 * The method must return PID of the new process, a (positive??? ####) number
 * which fits into Lisp_Int. No return value indicates an error, the method
 * must signal an error instead.
 */

static int
unix_create_process(Lisp_Process * p,
		    Lisp_Object * argv, int nargv,
		    Lisp_Object program, Lisp_Object cur_dir)
{
	int pid;
	long int inchannel = -1;
	long int outchannel = -1;
	/* Use volatile to protect variables from being clobbered by longjmp.  */
	volatile int forkin = -1;
	volatile int forkout = -1;
	volatile int pty_flag = 0;

#ifdef HAVE_PTYS
	if (!NILP(Vprocess_connection_type)) {
		/* find a new pty, open the master side, return the opened
		   file handle, and store the name of the corresponding slave
		   side in global variable pty_name. */
		outchannel = inchannel = allocate_pty();
	}

	if (inchannel >= 0) {
		/* You're "supposed" to now open the slave in the child.
		   On some systems, we can open it here; this allows for
		   better error checking. */
#if !defined(USG)
		/* On USG systems it does not work to open the pty's tty here
		   and then close and reopen it in the child.  */
#ifdef O_NOCTTY
		/* Don't let this terminal become our controlling terminal
		   (in case we don't have one).  */
		forkout = forkin =
		    open(pty_name, O_RDWR | O_NOCTTY | OPEN_BINARY, 0);
#else
		forkout = forkin = open(pty_name, O_RDWR | OPEN_BINARY, 0);
#endif
		if (forkin < 0)
			goto io_failure;
#endif				/* not USG */
		UNIX_DATA(p)->pty_flag = pty_flag = 1;
	} else
#endif				/* HAVE_PTYS */
	if (create_bidirectional_pipe(
		    (void*)&inchannel, (void*)&outchannel,
		    &forkin, &forkout) < 0)
		goto io_failure;

#if 0
	/* Replaced by close_process_descs */
	set_exclusive_use(inchannel);
	set_exclusive_use(outchannel);
#endif

	set_descriptor_non_blocking(inchannel);
	set_descriptor_non_blocking(outchannel);

	/* Record this as an active process, with its channels.
	   As a result, child_setup will close Emacs's side of the pipes.  */
	init_process_io_handles(p, (void *)inchannel, (void *)outchannel,
				pty_flag ? STREAM_PTY_FLUSHING : 0);
	/* Record the tty descriptor used in the subprocess.  */
	UNIX_DATA(p)->subtty = forkin;

	{
		/* child_setup must clobber environ on systems with true vfork.
		   Protect it from permanent change.  */
		char **save_environ = environ;

		pid = fork();
		if (pid == 0) {
	/**** Now we're in the child process ****/
			int xforkin = forkin;
			int xforkout = forkout;

			/* Checking for quit in the child is bad because that will
			   cause I/O, and that, in turn, can confuse the X connection. */
			begin_dont_check_for_quit();

			/* Disconnect the current controlling terminal, pursuant to
			   making the pty be the controlling terminal of the process.
			   Also put us in our own process group. */

			disconnect_controlling_terminal();

#ifdef HAVE_PTYS
			if (pty_flag) {
				/* Open the pty connection and make the pty's terminal
				   our controlling terminal.

				   On systems with TIOCSCTTY, we just use it to set
				   the controlling terminal.  On other systems, the
				   first TTY we open becomes the controlling terminal.
				   So, we end up with four possibilities:

				   (1) on USG and TIOCSCTTY systems, we open the pty
				   and use TIOCSCTTY.
				   (2) on other USG systems, we just open the pty.
				   (3) on non-USG systems with TIOCSCTTY, we
				   just use TIOCSCTTY. (On non-USG systems, we
				   already opened the pty in the parent process.)
				   (4) on non-USG systems without TIOCSCTTY, we
				   close the pty and reopen it.

				   This would be cleaner if we didn't open the pty
				   in the parent process, but doing it that way
				   makes it possible to trap error conditions.
				   It's harder to convey an error from the child
				   process, and I don't feel like messing with
				   this now. */

				/* There was some weirdo, probably wrong,
				   conditionalization on RTU and UNIPLUS here.
				   I deleted it.  So sue me. */

				/* SunOS has TIOCSCTTY but the close/open method
				   also works. */

#  if defined (USG) || !defined (TIOCSCTTY)
				/* Now close the pty (if we had it open) and reopen it.
				   This makes the pty the controlling terminal of the
				   subprocess.  */
				/* I wonder if close (open (pty_name, ...)) would work?  */
				if (xforkin >= 0)
					close(xforkin);
				xforkout = xforkin =
				    open(pty_name, O_RDWR | OPEN_BINARY, 0);
				if (xforkin < 0) {
					write(1,
					      "Couldn't open the pty terminal ",
					      31);
					write(1, pty_name, strlen(pty_name));
					write(1, "\n", 1);
					_exit(1);
				}
#  endif			/* USG or not TIOCSCTTY */

				/* Miscellaneous setup required for some systems.
				   Must be done before using tc* functions on xforkin.
				   This guarantees that isatty(xforkin) is true. */

#  if defined (HAVE_ISASTREAM) && defined (I_PUSH)
				if (isastream(xforkin)) {
#    if defined (I_FIND)
#      define stream_module_pushed(fd, module) (ioctl (fd, I_FIND, module) == 1)
#    else
#      define stream_module_pushed(fd, module) 0
#    endif
					if (!stream_module_pushed
					    (xforkin, "ptem"))
						ioctl(xforkin, I_PUSH, "ptem");
					if (!stream_module_pushed
					    (xforkin, "ldterm"))
						ioctl(xforkin, I_PUSH,
						      "ldterm");
					if (!stream_module_pushed
					    (xforkin, "ttcompat"))
						ioctl(xforkin, I_PUSH,
						      "ttcompat");
				}
#  endif			/* HAVE_ISASTREAM */

#  ifdef TIOCSCTTY
				/* We ignore the return value
				   because faith@cs.unc.edu says that is necessary on Linux.  */
				assert(isatty(xforkin));
				ioctl(xforkin, TIOCSCTTY, 0);
#  endif			/* TIOCSCTTY */

				/* Change the line discipline. */

# if defined (HAVE_TERMIOS) && defined (LDISC1)
				{
					struct termios t;
					assert(isatty(xforkin));
					tcgetattr(xforkin, &t);
					t.c_lflag = LDISC1;
					if (tcsetattr(xforkin, TCSANOW, &t) < 0)
						perror
						    ("create_process/tcsetattr LDISC1 failed\n");
				}
# elif defined (NTTYDISC) && defined (TIOCSETD)
				{
					/* Use new line discipline.  TIOCSETD is accepted and
					   ignored on Sys5.4 systems with ttcompat. */
					int ldisc = NTTYDISC;
					assert(isatty(xforkin));
					ioctl(xforkin, TIOCSETD, &ldisc);
				}
# endif				/* TIOCSETD & NTTYDISC */

				/* Make our process group be the foreground group
				   of our new controlling terminal. */

				{
					pid_t piddly =
					    EMACS_GET_PROCESS_GROUP();
					EMACS_SET_TTY_PROCESS_GROUP(xforkin,
								    &piddly);
				}

				/* On AIX, we've disabled SIGHUP above once we start a
				   child on a pty.  Now reenable it in the child, so it
				   will die when we want it to.
				   JV: This needs to be done ALWAYS as we might have inherited
				   a SIG_IGN handling from our parent (nohup) and we are in new
				   process group.
				 */
				signal(SIGHUP, SIG_DFL);
			}

			if (pty_flag)
				/* Set up the terminal characteristics of the pty. */
				child_setup_tty(xforkout);

#endif				/* HAVE_PTYS */

			signal(SIGINT, SIG_DFL);
			signal(SIGQUIT, SIG_DFL);

			{
				char *current_dir;
				char **new_argv =
				    alloca_array(char *, nargv + 2);
				int i;

				/* Nothing below here GCs so our string pointers shouldn't move. */
				new_argv[0] = (char *)XSTRING_DATA(program);
				for (i = 0; i < nargv; i++) {
					CHECK_STRING(argv[i]);
					new_argv[i + 1] =
					    (char *)XSTRING_DATA(argv[i]);
				}
				new_argv[i + 1] = 0;

				LISP_STRING_TO_EXTERNAL(cur_dir, current_dir,
							Qfile_name);

				child_setup(xforkin, xforkout, xforkout,
					    new_argv, current_dir);
			}

		}

	    /**** End of child code ****/
 /**** Back in parent process ****/
		environ = save_environ;
	}

	if (pid < 0) {
		int save_errno = errno;
		close_descriptor_pair(forkin, forkout);
		errno = save_errno;
		report_file_error("Doing fork", Qnil);
	}

	/* #### dmoore - why is this commented out, otherwise we leave
	   subtty = forkin, but then we close forkin just below. */
	/* UNIX_DATA(p)->subtty = -1; */

	/* If the subfork execv fails, and it exits,
	   this close hangs.  I don't know why.
	   So have an interrupt jar it loose.  */
	if (forkin >= 0)
		close_safely(forkin);
	if (forkin != forkout && forkout >= 0)
		close(forkout);

#ifdef HAVE_PTYS
	if (pty_flag)
		UNIX_DATA(p)->tty_name = build_string(pty_name);
	else
#endif
		UNIX_DATA(p)->tty_name = Qnil;

	/* Notice that SIGCHLD was not blocked. (This is not possible on
	   some systems.) No biggie if SIGCHLD occurs right around the
	   time that this call happens, because SIGCHLD() does not actually
	   deselect the process (that doesn't occur until the next time
	   we're waiting for an event, when status_notify() is called). */
	return pid;

      io_failure:
	{
		int save_errno = errno;
		close_descriptor_pair(forkin, forkout);
		close_descriptor_pair(inchannel, outchannel);
		errno = save_errno;
		report_file_error("Opening pty or pipe", Qnil);
		return 0;	/* not reached */
	}
}

/* This is called to set process' virtual terminal size */

static int unix_set_window_size(Lisp_Process * p, int cols, int rows)
{
	return set_window_size(UNIX_DATA(p)->infd, cols, rows);
}

/*
 * This method is called to update status fields of the process
 * structure. If the process has not existed, this method is
 * expected to do nothing.
 *
 * The method is called only for real child processes.
 */

#ifdef HAVE_WAITPID
static void unix_update_status_if_terminated(Lisp_Process * p)
{
	int w;
#ifdef SIGCHLD
	EMACS_BLOCK_SIGNAL(SIGCHLD);
#endif
	if (waitpid(XINT(p->pid), &w, WNOHANG) == XINT(p->pid)) {
		p->tick++;
		update_status_from_wait_code(p, &w);
	}
#ifdef SIGCHLD
	EMACS_UNBLOCK_SIGNAL(SIGCHLD);
#endif
}
#endif

/*
 * Update status of all exited processes. Called when SIGCLD has signaled.
 */

#ifdef SIGCHLD
static void unix_reap_exited_processes(void)
{
	int i;
	Lisp_Process *p;

#ifndef OBNOXIOUS_SYSV_SIGCLD_BEHAVIOR
	record_exited_processes(1);
#endif

	if (exited_processes_index <= 0) {
		return;
	}
#ifdef  EMACS_BLOCK_SIGNAL
	EMACS_BLOCK_SIGNAL(SIGCHLD);
#endif
	for (i = 0; i < exited_processes_index; i++) {
		int pid = exited_processes[i];
		int w = exited_processes_status[i];

		/* Find the process that signaled us, and record its status.  */

		p = 0;
		{
			Lisp_Object tail;
			LIST_LOOP(tail, Vprocess_list) {
				Lisp_Object proc = XCAR(tail);
				p = XPROCESS(proc);
				if (INTP(p->pid) && XINT(p->pid) == pid)
					break;
				p = 0;
			}
		}

		if (p) {
			/* Change the status of the process that was found.  */
			p->tick++;
			process_tick++;
			update_status_from_wait_code(p, &w);

			/* If process has terminated, stop waiting for its output.  */
			if (WIFSIGNALED(w) || WIFEXITED(w)) {
				if (!NILP(p->pipe_instream)) {
					/* We can't just call event_stream->unselect_process_cb (p)
					   here, because that calls XtRemoveInput, which is not
					   necessarily reentrant, so we can't call this at interrupt
					   level.
					 */
				}
			}
		} else {
			/* There was no asynchronous process found for that id.  Check
			   if we have a synchronous process. Only set sync process status
			   if there is one, so we work OK with the waitpid() call in
			   wait_for_termination(). */
			if (synch_process_alive != 0) {	/* Set the global sync process status variables. */
				synch_process_alive = 0;

				/* Report the status of the synchronous process.  */
				if (WIFEXITED(w))
					synch_process_retcode = WEXITSTATUS(w);
				else if (WIFSIGNALED(w))
					synch_process_death =
					    signal_name(WTERMSIG(w));
			}
		}
	}

	exited_processes_index = 0;

	EMACS_UNBLOCK_SIGNAL(SIGCHLD);
}
#endif				/* SIGCHLD */

/*
 * Stuff the entire contents of LSTREAM to the process output pipe
 */

static JMP_BUF send_process_frame;

static SIGTYPE send_process_trap(int signum)
{
	EMACS_REESTABLISH_SIGNAL(signum, send_process_trap);
	EMACS_UNBLOCK_SIGNAL(signum);
	LONGJMP(send_process_frame, 1);
}

static void
unix_send_process(Lisp_Object proc, lstream_t lstream)
{
	/* Use volatile to protect variables from being clobbered by longjmp.  */
	SIGTYPE(*volatile old_sigpipe) (int) = 0;
	volatile Lisp_Object vol_proc = proc;
	Lisp_Process *volatile p = XPROCESS(proc);

	/* #### JV: layering violation?

	   This function knows too much about the relation between the encoding
	   stream (DATA_OUTSTREAM) and the actual output stream p->output_stream.

	   If encoding streams properly forwarded all calls, we could simply
	   use DATA_OUTSTREAM everywhere. */

	if (!SETJMP(send_process_frame)) {
		/* use a reasonable-sized buffer (somewhere around the size of the
		   stream buffer) so as to avoid inundating the stream with blocked
		   data. */
		Bufbyte chunkbuf[512];
		Bytecount chunklen = 0;

		do {
			Lstream_data_count writeret;
			if (p->process_type!=PROCESS_TYPE_NETWORK_SERVER_LISTEN) {
				chunklen = Lstream_read(lstream, chunkbuf, 512);
			}
			old_sigpipe =
			    (SIGTYPE(*)(int))signal(SIGPIPE, send_process_trap);
			if (chunklen > 0) {
				int save_errno;

				switch (p->process_type) {
				case PROCESS_TYPE_NETWORK_SERVER_LISTEN:
					report_file_error ("no writing to listen process possible",
							   list1 (proc));
					break;
				case PROCESS_TYPE_SSL:
				case PROCESS_TYPE_PROC:
				case PROCESS_TYPE_NETWORK:
				case PROCESS_TYPE_MULTICAST:
				default:
/* Lstream_write() will never successfully write less than
 * the amount sent in.  In the worst case, it just buffers
 * the unwritten data. */
					writeret = Lstream_write
						(XLSTREAM (DATA_OUTSTREAM(p)),
						 chunkbuf, chunklen);
					break;
				}
				save_errno = errno;
				signal (SIGPIPE, old_sigpipe);
				errno = save_errno;
				if (writeret < 0)
/* This is a real error.  Blocking errors are handled
 * specially inside of the filedesc stream.
 */
					report_file_error ("writing to process",
							   list1 (proc));
			} else {
/* Need to make sure that everything up to and including the
 * last chunk is flushed, even when the pipe is currently
 * blocked. */
				Lstream_flush (XLSTREAM (DATA_OUTSTREAM(p)));
				signal (SIGPIPE, old_sigpipe);
			}
			while (Lstream_was_blocked_p
			       (XLSTREAM(p->pipe_outstream))) {
				/* Buffer is full.  Wait, accepting input;
				 * that may allow the program
				 * to finish doing output and read more.
				 */
				Faccept_process_output(Qnil, make_int(1), Qnil);
				/* It could have *really* finished,
				 * deleting the process */
				if (NILP(p->pipe_outstream))
					return;
				old_sigpipe = (SIGTYPE(*)(int))signal(
					SIGPIPE,
					send_process_trap);
				Lstream_flush(XLSTREAM(p->pipe_outstream));
				signal(SIGPIPE, old_sigpipe);
			}
			/* Perhaps should abort() if < 0?
			 * This should never happen.
			 */
		}
		while (chunklen > 0);
	} else {	/* We got here from a longjmp() from the SIGPIPE handler */
		signal(SIGPIPE, old_sigpipe);
		/* Close the file lstream so we don't attempt to write to it further */
		/* #### There is controversy over whether this might cause fd leakage */
		/*      my tests say no. -slb */
		XLSTREAM(p->pipe_outstream)->flags &= ~LSTREAM_FL_IS_OPEN;
#ifdef FILE_CODING
		XLSTREAM(p->coding_outstream)->flags &= ~LSTREAM_FL_IS_OPEN;
#endif
		p->status_symbol = Qexit;
		p->exit_code = 256;	/* #### SIGPIPE ??? */
		p->core_dumped = 0;
		p->tick++;
		process_tick++;
		deactivate_process(vol_proc);
		invalid_operation("SIGPIPE raised on process; closed it",
				  p->name);
	}

	old_sigpipe = (SIGTYPE(*)(int))signal(SIGPIPE, send_process_trap);
	Lstream_flush(XLSTREAM(DATA_OUTSTREAM(p)));
	signal(SIGPIPE, old_sigpipe);
}

/*
 * Send EOF to the process. The default implementation simply
 * closes the output stream. The method must return 0 to call
 * the default implementation, or 1 if it has taken all care about
 * sending EOF to the process.
 */

static int unix_process_send_eof(Lisp_Object proc)
{
	if (!UNIX_DATA(XPROCESS(proc))->pty_flag)
		return 0;

	/* #### get_eof_char simply doesn't return the correct character
	   here.  Maybe it is needed to determine the right eof
	   character in init_process_io_handles but here it simply screws
	   things up. */
#if 0
	Bufbyte eof_char = get_eof_char(XPROCESS(proc));
	send_process(proc, Qnil, &eof_char, 0, 1);
#else
	send_process(proc, Qnil, (const Bufbyte *)"\004", 0, 1);
#endif
	return 1;
}

/*
 * Called before the process is deactivated. The process object
 * is not immediately finalized, just undergoes a transition to
 * inactive state.
 *
 * The return value is a unique stream ID, as returned by
 * event_stream_delete_stream_pair
 *
 * In the lack of this method, only event_stream_delete_stream_pair
 * is called on both I/O streams of the process.
 *
 * The UNIX version guards this by ignoring possible SIGPIPE.
 */

static USID unix_deactivate_process(Lisp_Process * p)
{
	SIGTYPE(*old_sigpipe) (int) = 0;
	USID usid;

	if (UNIX_DATA(p)->infd >= 0)
		flush_pending_output(UNIX_DATA(p)->infd);

	/* closing the outstream could result in SIGPIPE, so ignore it. */
	old_sigpipe = (SIGTYPE(*)(int))signal(SIGPIPE, SIG_IGN);
	usid =  FD_TO_USID(UNIX_DATA(p)->infd);
	event_stream_delete_stream_pair(p->pipe_instream,
					p->pipe_outstream);

	signal(SIGPIPE, old_sigpipe);

	UNIX_DATA(p)->infd = -1;

	return usid;
}

/* If the subtty field of the process data is not filled in, do so now. */
static void try_to_initialize_subtty(struct unix_process_data *upd)
{
	if (upd->pty_flag && (upd->subtty == -1 || !isatty(upd->subtty))
	    && STRINGP(upd->tty_name))
		upd->subtty =
		    open((char *)XSTRING_DATA(upd->tty_name), O_RDWR, 0);
}

/* Send signal number SIGNO to PROCESS.
   CURRENT_GROUP means send to the process group that currently owns
   the terminal being used to communicate with PROCESS.
   This is used for various commands in shell mode.
   If NOMSG is zero, insert signal-announcements into process's buffers
   right away.

   If we can, we try to signal PROCESS by sending control characters
   down the pty.  This allows us to signal inferiors who have changed
   their uid, for which killpg would return an EPERM error,
   or processes running on other machines via remote login.

   The method signals an error if the given SIGNO is not valid. */

static void
unix_kill_child_process(Lisp_Object proc, int signo,
			int current_group, int nomsg)
{
	pid_t pgid = -1;
	Lisp_Process *p = XPROCESS(proc);
	struct unix_process_data *d = UNIX_DATA(p);

	switch (signo) {
#ifdef SIGCONT
	case SIGCONT:
		p->status_symbol = Qrun;
		p->exit_code = 0;
		p->tick++;
		process_tick++;
		if (!nomsg)
			status_notify();
		break;
#endif				/* ! defined (SIGCONT) */
	case SIGINT:
	case SIGQUIT:
	case SIGKILL:
		flush_pending_output(d->infd);
		break;
	default:
		break;
	}

	if (!d->pty_flag)
		current_group = 0;

	/* If current_group is true, we want to send a signal to the
	   foreground process group of the terminal our child process is
	   running on.  You would think that would be easy.

	   The BSD people invented the TIOCPGRP ioctl to get the foreground
	   process group of a tty.  That, combined with killpg, gives us
	   what we want.

	   However, the POSIX standards people, in their infinite wisdom,
	   have seen fit to only allow this for processes which have the
	   terminal as controlling terminal, which doesn't apply to us.

	   Sooo..., we have to do something non-standard.  The ioctls
	   TIOCSIGNAL, TIOCSIG, and TIOCSIGSEND send the signal directly on
	   many systems.  POSIX tcgetpgrp(), since it is *documented* as not
	   doing what we want, is actually less likely to work than the BSD
	   ioctl TIOCGPGRP it is supposed to obsolete.  Sometimes we have to
	   use TIOCGPGRP on the master end, sometimes the slave end
	   (probably an AIX bug).  So we better get a fd for the slave if we
	   haven't got it yet.

	   Anal operating systems like SGI Irix and Compaq Tru64 adhere
	   strictly to the letter of the law, so our hack doesn't work.
	   The following fragment from an Irix header file is suggestive:

	   #ifdef __notdef__
	   // this is not currently supported
	   #define TIOCSIGNAL      (tIOC|31)       // pty: send signal to slave
	   #endif

	   On those systems where none of our tricks work, we just fall back
	   to the non-current_group behavior and kill the process group of
	   the child.
	 */
	if (current_group) {
		try_to_initialize_subtty(d);

#ifdef SIGNALS_VIA_CHARACTERS
		/* If possible, send signals to the entire pgrp
		   by sending an input character to it.  */
		if (d->subtty >= 0)
		{
			char sigchar = process_signal_char(d->subtty, signo);
			if (sigchar) {
				send_process(proc, Qnil, (Bufbyte *) & sigchar,
					     0, 1);
				return;
			}
		}
#endif				/* SIGNALS_VIA_CHARACTERS */

#ifdef TIOCGPGRP
		if (pgid == -1)
			ioctl(d->infd, TIOCGPGRP, &pgid);	/* BSD */
		if (pgid == -1 && d->subtty >= 0)
			/* Only this works on AIX! */
			ioctl(d->subtty, TIOCGPGRP, &pgid);
#endif				/* TIOCGPGRP */

		if (pgid == -1) {
			/* Many systems provide an ioctl to send a signal directly */
#ifdef TIOCSIGNAL		/* Solaris, HP-UX */
			if (ioctl(d->infd, TIOCSIGNAL, signo) != -1)
				return;
#endif				/* TIOCSIGNAL */

#ifdef TIOCSIG			/* BSD */
			if (ioctl(d->infd, TIOCSIG, signo) != -1)
				return;
#endif				/* TIOCSIG */
		}
	}
	/* current_group */
	if (pgid == -1)
		/* Either current_group is 0, or we failed to get the foreground
		   process group using the trickery above.  So we fall back to
		   sending the signal to the process group of our child process.
		   Since this is often a shell that ignores signals like SIGINT,
		   the shell's subprocess is killed, which is the desired effect.
		   The process group of p->pid is always p->pid, since it was
		   created as a process group leader. */
		pgid = XINT(p->pid);

	/* Finally send the signal. */
	if (EMACS_KILLPG(pgid, signo) == -1) {
		/* It's not an error if our victim is already dead.
		   And we can't rely on the result of killing a zombie, since
		   XPG 4.2 requires that killing a zombie fail with ESRCH,
		   while FIPS 151-2 requires that it succeeds! */
#ifdef ESRCH
		if (errno != ESRCH)
#endif
			error("kill (%ld, %ld) failed: %s",
			      (long)pgid, (long)signo, strerror(errno));
	}
}

/* Send signal SIGCODE to any process in the system given its PID.
   Return zero if successful, a negative number upon failure. */

static int unix_kill_process_by_pid(int pid, int sigcode)
{
	return kill(pid, sigcode);
}

/* Return TTY name used to communicate with subprocess. */

static Lisp_Object unix_get_tty_name(Lisp_Process * p)
{
	return UNIX_DATA(p)->tty_name;
}

/* Canonicalize host name HOST, and return its canonical form.
   The default implementation just takes HOST for a canonical name. */

#ifdef HAVE_SOCKETS
static Lisp_Object unix_canonicalize_host_name(Lisp_Object host)
{
#if defined(HAVE_GETADDRINFO) && defined(HAVE_GETNAMEINFO)
	struct addrinfo hints, *res;
	static char addrbuf[NI_MAXHOST];
	Lisp_Object canonname;
	int retval;
	char *ext_host;

	xzero(hints);
	hints.ai_flags = AI_CANONNAME;
#ifdef IPV6_CANONICALIZE
	hints.ai_family = AF_UNSPEC;
#else
	hints.ai_family = PF_INET;
#endif
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_protocol = 0;
	LISP_STRING_TO_EXTERNAL(host, ext_host, Qnative);
	retval = getaddrinfo(ext_host, NULL, &hints, &res);
	if (retval != 0) {
		char *gai_error_l;

		EXTERNAL_TO_C_STRING(gai_strerror(retval),
				     gai_error_l, Qnative);
		maybe_error(Qprocess, ERROR_ME_NOT,
			    "%s \"%s\"", gai_error_l, XSTRING_DATA(host));
		canonname = host;
	} else {
		int gni = getnameinfo(res->ai_addr, res->ai_addrlen,
				      addrbuf, sizeof(addrbuf),
				      NULL, 0, NI_NUMERICHOST);
		canonname = gni ? host : build_ext_string(addrbuf, Qnative);

		freeaddrinfo(res);
	}

	return canonname;
#else				/* ! HAVE_GETADDRINFO */
	struct sockaddr_in address;

	if (!get_internet_address(host, &address, ERROR_ME_NOT))
		return host;

	if (address.sin_family == AF_INET)
		return build_string(inet_ntoa(address.sin_addr));
	else
		/* #### any clue what to do here? */
		return host;
#endif				/* ! HAVE_GETADDRINFO */
}

/* Open a TCP network connection to a given HOST/SERVICE.
   Treated exactly like a normal process when reading and writing.
   Only differences are in status display and process deletion.
   A network connection has no PID; you cannot signal it.  All you can
   do is deactivate and close it via delete-process. */

static void
unix_open_network_stream(Lisp_Object name, Lisp_Object host,
			 Lisp_Object service, Lisp_Object protocol,
			 void **vinfd, void **voutfd)
{
	EMACS_INT inch;
	EMACS_INT outch;
	volatile int s;
	volatile int port;
	volatile int retry = 0;
	int retval;

	CHECK_STRING(host);

	if (!EQ(protocol, Qtcp) && !EQ(protocol, Qudp))
		invalid_argument("Unsupported protocol", protocol);

	{
#if defined(HAVE_GETADDRINFO) && defined(HAVE_GETNAMEINFO)
		struct addrinfo hints, *res;
		struct addrinfo *volatile lres;
		char *portstring;
		volatile int xerrno = 0;
		volatile int failed_connect = 0;
		char *ext_host;
		char portbuf[sizeof(long)*3 + 2];
		/*
		 * Caution: service can either be a string or int.
		 * Convert to a C string for later use by getaddrinfo.
		 */
		if (INTP(service)) {
			int sz= snprintf(portbuf, sizeof(portbuf), "%ld",
					 (long)XINT(service));
			assert(sz >= 0 && (size_t)sz < sizeof(portbuf));
			portstring = portbuf;
			port = htons((unsigned short)XINT(service));
		} else {
			CHECK_STRING(service);
			LISP_STRING_TO_EXTERNAL(service, portstring, Qnative);
			port = 0;
		}

		xzero(hints);
		hints.ai_flags = 0;
		hints.ai_family = AF_UNSPEC;
		if (EQ(protocol, Qtcp))
			hints.ai_socktype = SOCK_STREAM;
		else		/* EQ (protocol, Qudp) */
			hints.ai_socktype = SOCK_DGRAM;
		hints.ai_protocol = 0;
		LISP_STRING_TO_EXTERNAL(host, ext_host, Qnative);
		retval = getaddrinfo(ext_host, portstring, &hints, &res);
		if (retval != 0) {
			char *gai_error_l;

			EXTERNAL_TO_C_STRING(gai_strerror(retval),
					     gai_error_l, Qnative);
			error("%s/%s %s", XSTRING_DATA(host), portstring,
			      gai_error_l);
		}

		/* address loop */
		for (lres = res; lres; lres = lres->ai_next) {
			if (EQ(protocol, Qtcp))
				s = socket(lres->ai_family, SOCK_STREAM, 0);
			else	/* EQ (protocol, Qudp) */
				s = socket(lres->ai_family, SOCK_DGRAM, 0);

			if (s < 0)
				continue;

			/* Turn off interrupts here -- see comments below.
			   There used to be code which called
			   bind_polling_period() to slow the polling period down
			   rather than turn it off, but that seems rather bogus
			   to me.  Best thing here is to use a non-blocking
			   connect or something, to check for QUIT. */

			/* Comments that are not quite valid: */

			/* Kernel bugs (on Ultrix at least) cause lossage (not
			   just EINTR) when connect is interrupted.  So let's
			   not let it get interrupted.  Note we do not turn off
			   polling, because polling is only used when not
			   interrupt_input, and thus not normally used on the
			   systems which have this bug.  On systems which use
			   polling, there's no way to quit if polling is turned
			   off.  */

			/* Slow down polling.  Some kernels have a bug which
			   causes retrying connect to fail after a connect.  */

			slow_down_interrupts();

		      loop:

			/* A system call interrupted with a SIGALRM or SIGIO
			   comes back here, with can_break_system_calls reset to
			   0. */
			SETJMP(break_system_call_jump);
			if (QUITP) {
				speed_up_interrupts();
				REALLY_QUIT;
				/* In case something really weird happens ... */
				slow_down_interrupts();
			}

			/* Break out of connect with a signal (it isn't
			   otherwise possible).  Thus you don't get screwed with
			   a hung network. */
			can_break_system_calls = 1;
			retval = connect(s, lres->ai_addr, lres->ai_addrlen);
			can_break_system_calls = 0;
			if (retval == -1) {
				xerrno = errno;
				if (errno != EISCONN) {
					if (errno == EINTR)
						goto loop;
					if (errno == EADDRINUSE && retry < 20) {
						/* A delay here is needed on
						   some FreeBSD systems, and
						   it is harmless, since this
						   retrying takes time anyway
						   and should be infrequent.
						   `sleep-for' allowed for
						   quitting this loop with
						   interrupts slowed down so
						   it can't be used here.
						   Async timers should already
						   be disabled at this point
						   so we can use `sleep'. */
						retry++;
						goto loop;
					}
				}

				failed_connect = 1;
				close(s);
				s = -1;

				speed_up_interrupts();

				continue;
			}

			if (port == 0) {
				int gni;
				char servbuf[NI_MAXSERV];

				if (EQ(protocol, Qtcp))
					gni =
					    getnameinfo(lres->ai_addr,
							lres->ai_addrlen, NULL,
							0, servbuf,
							sizeof(servbuf),
							NI_NUMERICSERV);
				else	/* EQ (protocol, Qudp) */
					gni =
					    getnameinfo(lres->ai_addr,
							lres->ai_addrlen, NULL,
							0, servbuf,
							sizeof(servbuf),
							NI_NUMERICSERV |
							NI_DGRAM);

				if (gni == 0)
					port = strtol(servbuf, NULL, 10);
			}

			break;
		}		/* address loop */

		speed_up_interrupts();

		freeaddrinfo(res);
		if (s < 0) {
			errno = xerrno;

			if (failed_connect)
				report_file_error("connection failed",
						  list2(host, name));
			else
				report_file_error("error creating socket",
						  list1(name));
		}
#else				/* ! HAVE_GETADDRINFO */
		struct sockaddr_in address;

		if (INTP(service))
			port = htons((unsigned short)XINT(service));
		else {
			struct servent *svc_info;
			CHECK_STRING(service);

			if (EQ(protocol, Qtcp))
				svc_info =
				    getservbyname((char *)XSTRING_DATA(service),
						  "tcp");
			else	/* EQ (protocol, Qudp) */
				svc_info =
				    getservbyname((char *)XSTRING_DATA(service),
						  "udp");

			if (svc_info == 0)
				invalid_argument("Unknown service", service);
			port = svc_info->s_port;
		}

		get_internet_address(host, &address, ERROR_ME);
		address.sin_port = port;

		if (EQ(protocol, Qtcp))
			s = socket(address.sin_family, SOCK_STREAM, 0);
		else		/* EQ (protocol, Qudp) */
			s = socket(address.sin_family, SOCK_DGRAM, 0);

		if (s < 0)
			report_file_error("error creating socket", list1(name));

		/* Turn off interrupts here -- see comments below.  There used to
		   be code which called bind_polling_period() to slow the polling
		   period down rather than turn it off, but that seems rather
		   bogus to me.  Best thing here is to use a non-blocking connect
		   or something, to check for QUIT. */

		/* Comments that are not quite valid: */

		/* Kernel bugs (on Ultrix at least) cause lossage (not just EINTR)
		   when connect is interrupted.  So let's not let it get interrupted.
		   Note we do not turn off polling, because polling is only used
		   when not interrupt_input, and thus not normally used on the systems
		   which have this bug.  On systems which use polling, there's no way
		   to quit if polling is turned off.  */

		/* Slow down polling.  Some kernels have a bug which causes retrying
		   connect to fail after a connect.  */

		slow_down_interrupts();

	      loop:

		/* A system call interrupted with a SIGALRM or SIGIO comes back
		   here, with can_break_system_calls reset to 0. */
		SETJMP(break_system_call_jump);
		if (QUITP) {
			speed_up_interrupts();
			REALLY_QUIT;
			/* In case something really weird happens ... */
			slow_down_interrupts();
		}

		/* Break out of connect with a signal (it isn't otherwise possible).
		   Thus you don't get screwed with a hung network. */
		can_break_system_calls = 1;
		retval =
		    connect(s, (struct sockaddr *)&address, sizeof(address));
		can_break_system_calls = 0;
		if (retval == -1 && errno != EISCONN) {
			int xerrno = errno;
			if (errno == EINTR)
				goto loop;
			if (errno == EADDRINUSE && retry < 20) {
				/* A delay here is needed on some FreeBSD
				   systems, and it is harmless, since this
				   retrying takes time anyway and should
				   be infrequent.  `sleep-for' allowed for
				   quitting this loop with interrupts
				   slowed down so it can't be used here.
				   Async timers should already be disabled
				   at this point so we can use `sleep'. */
				retry++;
				goto loop;
			}

			close(s);

			speed_up_interrupts();

			errno = xerrno;
			report_file_error("connection failed",
					  list2(host, name));
		}

		speed_up_interrupts();
#endif				/* ! HAVE_GETADDRINFO */
	}

	inch = s;
	outch = dup(s);
	if (outch < 0) {
		close(s);	/* this used to be leaked; from Kyle Jones */
		report_file_error("error duplicating socket", list1(name));
	}

	set_socket_nonblocking_maybe(inch, port, "tcp");

	*vinfd = (void *)inch;
	*voutfd = (void *)outch;
}


/*
  Return the listener process of the accepted listened process
*/
static Lisp_Object unix_network_process_listener(Lisp_Object process)
{
	Lisp_Process *listener = 0,
		     *p = XPROCESS(process);
	Lisp_Object   ret = Qnil;
	int           sock = 0;
	struct  gcpro ngcpro1;

	if (!PROCESS_READABLE_P(p))
		return Qnil;

	if (!CONSP(p->pid) || NILP(XCDR(XCDR(p->pid))))
		return Qnil;

	NGCPRO1(ret);
	sock = XINT(XCAR(XCDR(p->pid)));
	listener = get_process_from_usid(FD_TO_USID(sock));
	ret = listener ? (Lisp_Object)listener : Qnil;
	NUNGCPRO;
	return ret;
}

/*
  Unwind a call to the network server stream accept below
*/
static Lisp_Object exec_acceptor_unwind(Lisp_Object datum)
{
	Lisp_Cons *d = XCONS(datum);
	free_cons(d);
	return Qnil;
}


/*
  Accept a connection being listened in the given network server
  stream process.
  Create a new network stream for the accepted connection.
  Call the acceptor callback and setup the sentinel and filter functions
*/
static void unix_network_server_accept(Lisp_Object process)
{
	Lisp_Process *p = XPROCESS(process);
	Lisp_Object np = Qnil;
	Lisp_Object acceptor = Qnil, filter = Qnil, sentinel = Qnil;
	Lisp_Object bufname = Qnil;
	Lisp_Object buffer = Qnil;
	long int ns, inch, outch;
	struct sockaddr_in sa;
	int sa_size = sizeof(sa);
	struct gcpro ngcpro1, ngcpro2, ngcpro3, ngcpro4, ngcpro5;

	if (!PROCESS_READABLE_P(p))
		return;

	/* Make sure the listen process is not disconnected
	   afterwards.  We have to make this here because in process.c
	   we should not have any knowledge we need to do this, and in
	   unix_open_network_server_stream we have no access to the
	   process struct. It works, so I'm not complaining...
	 */
	UNIX_DATA(p)->connected_via_filedesc_p = 1;

	errno = 0; /* if we got an error, let it be from the accept call */
	ns = accept((int)UNIX_DATA(p)->infd, (struct sockaddr*)(&sa),
		    (socklen_t*)&sa_size);
	if ( ns < 0 )
		return;

	NGCPRO5(np,bufname,acceptor,filter,sentinel);
	if (CONSP(p->process_type_data)) {
		acceptor = XCAR(p->process_type_data);
		filter = XCDR(p->process_type_data);
		if (CONSP(filter)) {
			sentinel = XCDR(filter);
			filter   = XCAR(filter);
		}
		if (CONSP(sentinel)) {
			bufname  = XCDR(sentinel);
			sentinel = XCAR(sentinel);
		}
		if (CONSP(bufname)) {
			bufname  = XCAR(bufname);
		}
	}
	if (!NILP(bufname)) {
		Lisp_Object args[] = {
			build_string("<server port:%S listened_on:%S>"),
			make_int(sa.sin_port), bufname
		};
		bufname = Fformat( 3, args );
	} else {
		Lisp_Object args[] = {
			build_string("<server proc:%S pid:%S service:%S>"),
			p->name, p->pid, make_int(sa.sin_port)
		};
		bufname = Fformat( 5, args );
	}
	if (!NILP(bufname) ) {
		bufname = Fgenerate_new_buffer_name(bufname,Qnil);
		buffer  =  Fget_buffer_create(bufname);
	}
	np = make_process_internal(p->name);
	XPROCESS(np)->pid = Fcons( make_int(sa.sin_port),
				   Fcons(make_int(UNIX_DATA(p)->infd),p->pid));
	XPROCESS(np)->process_type = PROCESS_TYPE_NETWORK;
	XPROCESS(np)->buffer = buffer;
	inch = ns;
	outch = dup(ns);
	set_socket_nonblocking_maybe(inch, sa.sin_port, "tcp");
	init_process_io_handles(XPROCESS(np), (void *)inch, (void *)outch,
				STREAM_NETWORK_CONNECTION);
	/* Process the call backs.. */
	if (CONSP(p->process_type_data)) {
		if (!NILP(filter)) {
			XPROCESS(np)->filter = filter;
		}
		if (!NILP(sentinel)) {
			XPROCESS(np)->sentinel = sentinel;
		}
		if (!NILP(acceptor)) {
			int speccount = specpdl_depth();
			record_unwind_protect(exec_acceptor_unwind,
					      noseeum_cons(process, acceptor));
			running_asynch_code = 1;
			call1_trapping_errors("Error in server stream acceptor",
					      acceptor, np);
			running_asynch_code = 0;
			restore_match_data();
			unbind_to(speccount, Qnil);

		}
	} else {
		/* We have to log something here... */
	}
	event_stream_select_process(XPROCESS(np));
	NUNGCPRO;
}

/* Open a TCP network connection to a given HOST/SERVICE.
   Treated exactly like a normal process when reading and writing.
   Only differences are in status display and process deletion.
   A network connection has no PID; you cannot signal it.  All you can
   do is deactivate and close it via delete-process. */

static void
unix_open_network_server_stream(Lisp_Object name, Lisp_Object host,
				Lisp_Object service, Lisp_Object protocol,
				void **vinfd, void **voutfd)
{
	EMACS_INT inch;
	EMACS_INT outch;
	volatile int s;
	volatile int port;
	volatile int retry = 0;
	int retval;
	/* FIXME: Limited to 5 since it is the maximum for several BSD
	   based implementations of sockets, and it is an acceptable
	   value for a low rate of service purpose like this facility
	   was designed for. */
	int listenQ = 5;


	if (!EQ(protocol, Qtcp) && !EQ(protocol, Qudp))
		invalid_argument("Unsupported protocol", protocol);

	{
#if defined(HAVE_GETADDRINFO) && defined(HAVE_GETNAMEINFO)
		struct addrinfo hints, *res;
		struct addrinfo *volatile lres;
		char *portstring;
		volatile int xerrno = 0;
		volatile int failed_connect = 0;
		char *ext_host;
		char portbuf[sizeof(long)*3 + 2];
		/*
		 * Caution: service can either be a string or int.
		 * Convert to a C string for later use by getaddrinfo.
		 */
		if (INTP(service)) {
			int sz = snprintf(portbuf, sizeof(portbuf), "%ld",
					  (long)XINT(service));
			assert(sz >= 0 && (size_t)sz < sizeof(portbuf));
			portstring = portbuf;
			port = htons((unsigned short)XINT(service));
		} else {
			CHECK_STRING(service);
			LISP_STRING_TO_EXTERNAL(service, portstring, Qnative);
			port = 0;
		}

		xzero(hints);
		hints.ai_flags = 0;
		hints.ai_family = AF_UNSPEC;
		if (EQ(protocol, Qtcp))
			hints.ai_socktype = SOCK_STREAM;
		else		/* EQ (protocol, Qudp) */
			hints.ai_socktype = SOCK_DGRAM;
		hints.ai_protocol = 0;
		if (SYMBOLP(host) ) {
			if (EQ(host,Qip_any)) {
				hints.ai_flags |= AI_PASSIVE;
			} else if (!EQ(host,Qlocalhost)) {
				invalid_argument("invalid host ",host);
			} else {
				/* If using localhost, not passing
				   AI_PASSIVE will cause getaddrinfo
				   to return a proper addr spec for
				   listening only to local
				   connections. */
			}
			retval = getaddrinfo(NULL, portstring, &hints, &res);
		} else {
			CHECK_STRING(host);
			LISP_STRING_TO_EXTERNAL(host, ext_host, Qnative);
			retval = getaddrinfo(ext_host, portstring, &hints, &res);
		}
		if (retval != 0) {
			char *gai_error_l;

			EXTERNAL_TO_C_STRING(gai_strerror(retval),
					     gai_error_l, Qnative);
			error("%s/%s %s", XSTRING_DATA(host), portstring,
			      gai_error_l);
		}

		/* address loop */
		for (lres = res; lres; lres = lres->ai_next) {
			if (EQ(protocol, Qtcp))
				s = socket(lres->ai_family, SOCK_STREAM, 0);
			else	/* EQ (protocol, Qudp) */
				s = socket(lres->ai_family, SOCK_DGRAM, 0);

			if (s < 0)
				continue;

			/* Turn off interrupts here -- see comments below.
			   There used to be code which called
			   bind_polling_period() to slow the polling period down
			   rather than turn it off, but that seems rather bogus
			   to me.  Best thing here is to use a non-blocking
			   connect or something, to check for QUIT. */

			/* Comments that are not quite valid: */

			/* Kernel bugs (on Ultrix at least) cause lossage (not
			   just EINTR) when connect is interrupted.  So let's
			   not let it get interrupted.  Note we do not turn off
			   polling, because polling is only used when not
			   interrupt_input, and thus not normally used on the
			   systems which have this bug.  On systems which use
			   polling, there's no way to quit if polling is turned
			   off.  */

			/* Slow down polling.  Some kernels have a bug which
			   causes retrying connect to fail after a connect.  */

			slow_down_interrupts();

		      loop:

			/* A system call interrupted with a SIGALRM or SIGIO
			   comes back here, with can_break_system_calls reset to
			   0. */
			SETJMP(break_system_call_jump);
			if (QUITP) {
				speed_up_interrupts();
				REALLY_QUIT;
				/* In case something really weird happens ... */
				slow_down_interrupts();
			}

			/* Break out of connect with a signal (it isn't
			   otherwise possible).  Thus you don't get screwed with
			   a hung network. */
			can_break_system_calls = 1;
			retval = bind(s, lres->ai_addr, lres->ai_addrlen);
			if (retval >= 0 )
				retval = listen(s, listenQ);
			can_break_system_calls = 0;
			if (retval == -1) {
				xerrno = errno;
				if (errno != EISCONN) {
					if (errno == EINTR)
						goto loop;
					if (errno == EADDRINUSE && retry < 20) {
						/* A delay here is needed on
						   some FreeBSD systems, and it
						   is harmless, since this
						   retrying takes time anyway
						   and should be infrequent.
						   `sleep-for' allowed for
						   quitting this loop with
						   interrupts slowed down so it
						   can't be used here.  Async
						   timers should already be
						   disabled at this point so we
						   can use `sleep'. */
						retry++;
						goto loop;
					}
				}

				failed_connect = 1;
				close(s);
				s = -1;

				speed_up_interrupts();

				continue;
			}

			if (port == 0) {
				int gni;
				char servbuf[NI_MAXSERV];

				if (EQ(protocol, Qtcp))
					gni =
					    getnameinfo(lres->ai_addr,
							lres->ai_addrlen, NULL,
							0, servbuf,
							sizeof(servbuf),
							NI_NUMERICSERV);
				else	/* EQ (protocol, Qudp) */
					gni =
					    getnameinfo(lres->ai_addr,
							lres->ai_addrlen, NULL,
							0, servbuf,
							sizeof(servbuf),
							NI_NUMERICSERV |
							NI_DGRAM);

				if (gni == 0)
					port = strtol(servbuf, NULL, 10);
			}

			break;
		}		/* address loop */

		speed_up_interrupts();

		freeaddrinfo(res);
		if (s < 0) {
			errno = xerrno;

			if (failed_connect)
				report_file_error("bind failed",
						  list2(host, name));
			else
				report_file_error("error creating socket",
						  list1(name));
		}
#else				/* ! HAVE_GETADDRINFO */
		struct sockaddr_in address;

		if (INTP(service))
			port = htons((unsigned short)XINT(service));
		else {
			struct servent *svc_info;
			CHECK_STRING(service);

			if (EQ(protocol, Qtcp))
				svc_info =
				    getservbyname((char *)XSTRING_DATA(service),
						  "tcp");
			else	/* EQ (protocol, Qudp) */
				svc_info =
				    getservbyname((char *)XSTRING_DATA(service),
						  "udp");

			if (svc_info == 0)
				invalid_argument("Unknown service", service);
			port = svc_info->s_port;
		}
		if (SYMBOLP(host)) {
			if (EQ(host,Qip_any)) {
				address.sin_addr.s_host = htonl(INADDR_ANY);
			} else if (EQ(host,Qlocalhost)) {
				address.sin_addr.s_host = htonl(INADDR_LOOPBACK);
			} else {
				invalid_argument("invalid host ",host);
			}
		} else {
			get_internet_address(host, &address, ERROR_ME);
		}
		address.sin_port = port;

		if (EQ(protocol, Qtcp))
			s = socket(address.sin_family, SOCK_STREAM, 0);
		else		/* EQ (protocol, Qudp) */
			s = socket(address.sin_family, SOCK_DGRAM, 0);

		if (s < 0)
			report_file_error("error creating socket", list1(name));

		/* Turn off interrupts here -- see comments below.  There used to
		   be code which called bind_polling_period() to slow the polling
		   period down rather than turn it off, but that seems rather
		   bogus to me.  Best thing here is to use a non-blocking connect
		   or something, to check for QUIT. */

		/* Comments that are not quite valid: */

		/* Kernel bugs (on Ultrix at least) cause lossage (not just EINTR)
		   when connect is interrupted.  So let's not let it get interrupted.
		   Note we do not turn off polling, because polling is only used
		   when not interrupt_input, and thus not normally used on the systems
		   which have this bug.  On systems which use polling, there's no way
		   to quit if polling is turned off.  */

		/* Slow down polling.  Some kernels have a bug which causes retrying
		   connect to fail after a connect.  */

		slow_down_interrupts();

	      loop:

		/* A system call interrupted with a SIGALRM or SIGIO comes back
		   here, with can_break_system_calls reset to 0. */
		SETJMP(break_system_call_jump);
		if (QUITP) {
			speed_up_interrupts();
			REALLY_QUIT;
			/* In case something really weird happens ... */
			slow_down_interrupts();
		}

		/* Break out of connect with a signal (it isn't otherwise possible).
		   Thus you don't get screwed with a hung network. */
		can_break_system_calls = 1;
		retval =
		    bind(s, (struct sockaddr *)&address, sizeof(address));
		if ( retval >= 0 )
			listen( s, listenQ ); /* @@@ FIXME: This should be a parameter */
		can_break_system_calls = 0;
		if (retval == -1 && errno != EISCONN) {
			int xerrno = errno;
			if (errno == EINTR)
				goto loop;
			if (errno == EADDRINUSE && retry < 20) {
				/* A delay here is needed on some FreeBSD
				   systems, and it is harmless, since this
				   retrying takes time anyway and should be
				   infrequent.  `sleep-for' allowed for quitting
				   this loop with interrupts slowed down so it
				   can't be used here.  Async timers should
				   already be disabled at this point so we can
				   use `sleep'. */
				retry++;
				goto loop;
			}

			close(s);

			speed_up_interrupts();

			errno = xerrno;
			report_file_error("connection failed",
					  list2(host, name));
		}

		speed_up_interrupts();
#endif				/* ! HAVE_GETADDRINFO */
	}

	inch = s;
	outch = dup(s);
	if (outch < 0) {
		close(s);	/* this used to be leaked; from Kyle Jones */
		report_file_error("error duplicating socket", list1(name));
	}

	set_socket_nonblocking_maybe(inch, port, "tcp");

	*vinfd = (void *)inch;
	*voutfd = (void *)outch;
}


#ifdef HAVE_MULTICAST

/* Didier Verna <didier@xemacs.org> Nov. 28 1997.

   This function is similar to open-network-stream-internal, but provides a
   mean to open an UDP multicast connection instead of a TCP one. Like in the
   TCP case, the multicast connection will be seen as a sub-process,

   Some notes:
   - Normally, we should use sendto and recvfrom with non connected
   sockets. The current code doesn't allow us to do this. In the future, it
   would be a good idea to extend the process data structure in order to deal
   properly with the different types network connections.
   - For the same reason, when leaving a multicast group, it is better to make
   a setsockopt - IP_DROP_MEMBERSHIP before closing the descriptors.
   Unfortunately, this can't be done here because delete_process doesn't know
   about the kind of connection we have. However, this is not such an
   important issue.
*/

static void
unix_open_multicast_group(Lisp_Object name, Lisp_Object dest,
			  Lisp_Object port, Lisp_Object ttl, void **vinfd,
			  void **voutfd)
{
	struct ip_mreq imr;
	struct sockaddr_in sa;
	struct protoent *udp;
	EMACS_INT ws, rs;
	int theport;
	unsigned char thettl;
	int one = 1;		/* For REUSEADDR */
	int ret;
	volatile int retry = 0;

	CHECK_STRING(dest);

	CHECK_NATNUM(port);
	theport = htons((unsigned short)XINT(port));

	CHECK_NATNUM(ttl);
	thettl = (unsigned char)XINT(ttl);

	if ((udp = getprotobyname("udp")) == NULL)
		type_error(Qinvalid_operation,
			   "No info available for UDP protocol");

	/* Init the sockets. Yes, I need 2 sockets. I couldn't duplicate one. */
	if ((rs = socket(PF_INET, SOCK_DGRAM, udp->p_proto)) < 0)
		report_file_error("error creating socket", list1(name));
	if ((ws = socket(PF_INET, SOCK_DGRAM, udp->p_proto)) < 0) {
		close(rs);
		report_file_error("error creating socket", list1(name));
	}

	/* This will be used for both sockets */
	memset(&sa, 0, sizeof(sa));
	sa.sin_family = AF_INET;
	sa.sin_port = theport;
	sa.sin_addr.s_addr = inet_addr((char *)XSTRING_DATA(dest));

	/* Socket configuration for reading ------------------------ */

	/* Multiple connections from the same machine. This must be done before
	   bind. If it fails, it shouldn't be fatal. The only consequence is that
	   people won't be able to connect twice from the same machine. */
	if (setsockopt(rs, SOL_SOCKET, SO_REUSEADDR, (char *)&one, sizeof(one))
	    < 0)
		warn_when_safe(Qmulticast, Qwarning,
			       "Cannot reuse socket address");

	/* bind socket name */
	if (bind(rs, (struct sockaddr *)&sa, sizeof(sa))) {
		close(rs);
		close(ws);
		report_file_error("error binding socket", list2(name, port));
	}

	/* join multicast group */
	imr.imr_multiaddr.s_addr = inet_addr((char *)XSTRING_DATA(dest));
	imr.imr_interface.s_addr = htonl(INADDR_ANY);
	if (setsockopt(rs, IPPROTO_IP, IP_ADD_MEMBERSHIP,
		       &imr, sizeof(struct ip_mreq)) < 0) {
		close(ws);
		close(rs);
		report_file_error("error adding membership", list2(name, dest));
	}

	/* Socket configuration for writing ----------------------- */

	/* Normally, there's no 'connect' in multicast, since we prefer to use
	   'sendto' and 'recvfrom'. However, in order to handle this connection
	   in the process-like way it is done for TCP, we must be able to use
	   'write' instead of 'sendto'. Consequently, we 'connect' this
	   socket. */

	/* See open-network-stream-internal for comments on this part of the
	   code */
	slow_down_interrupts();

      loop:

	/* A system call interrupted with a SIGALRM or SIGIO comes back
	   here, with can_break_system_calls reset to 0. */
	SETJMP(break_system_call_jump);
	if (QUITP) {
		speed_up_interrupts();
		REALLY_QUIT;
		/* In case something really weird happens ... */
		slow_down_interrupts();
	}

	/* Break out of connect with a signal (it isn't otherwise possible).
	   Thus you don't get screwed with a hung network. */
	can_break_system_calls = 1;
	ret = connect(ws, (struct sockaddr *)&sa, sizeof(sa));
	can_break_system_calls = 0;
	if (ret == -1 && errno != EISCONN) {
		int xerrno = errno;

		if (errno == EINTR)
			goto loop;
		if (errno == EADDRINUSE && retry < 20) {
			/* A delay here is needed on some FreeBSD systems, and
			   it is harmless, since this retrying takes time anyway
			   and should be infrequent.  `sleep-for' allowed for
			   quitting this loop with interrupts slowed down so it
			   can't be used here.  Async timers should already be
			   disabled at this point so we can use `sleep'. */
			retry++;
			goto loop;
		}

		close(rs);
		close(ws);
		speed_up_interrupts();

		errno = xerrno;
		report_file_error("error connecting socket", list2(name, port));
	}

	speed_up_interrupts();

	/* scope */
	if (setsockopt(ws, IPPROTO_IP, IP_MULTICAST_TTL,
		       &thettl, sizeof(thettl)) < 0) {
		close(rs);
		close(ws);
		report_file_error("error setting ttl", list2(name, ttl));
	}

	set_socket_nonblocking_maybe(rs, theport, "udp");

	*vinfd = (void *)rs;
	*voutfd = (void *)ws;
}

#endif				/* HAVE_MULTICAST */

#endif				/* HAVE_SOCKETS */

/**********************************************************************/
/*                            Initialization                          */
/**********************************************************************/

void process_type_create_unix(void)
{
	PROCESS_HAS_METHOD(unix, alloc_process_data);
	PROCESS_HAS_METHOD(unix, mark_process_data);
#ifdef SIGCHLD
	PROCESS_HAS_METHOD(unix, init_process);
	PROCESS_HAS_METHOD(unix, reap_exited_processes);
#endif
	PROCESS_HAS_METHOD(unix, init_process_io_handles);
	PROCESS_HAS_METHOD(unix, create_process);
	PROCESS_HAS_METHOD(unix, set_window_size);
#ifdef HAVE_WAITPID
	PROCESS_HAS_METHOD(unix, update_status_if_terminated);
#endif
	PROCESS_HAS_METHOD(unix, send_process);
	PROCESS_HAS_METHOD(unix, process_send_eof);
	PROCESS_HAS_METHOD(unix, deactivate_process);
	PROCESS_HAS_METHOD(unix, kill_child_process);
	PROCESS_HAS_METHOD(unix, kill_process_by_pid);
	PROCESS_HAS_METHOD(unix, get_tty_name);
#ifdef HAVE_SOCKETS
	PROCESS_HAS_METHOD(unix, canonicalize_host_name);
	PROCESS_HAS_METHOD(unix, open_network_stream);
	PROCESS_HAS_METHOD(unix, open_network_server_stream);
	PROCESS_HAS_METHOD(unix, network_server_accept);
	PROCESS_HAS_METHOD(unix, network_process_listener);
#ifdef HAVE_MULTICAST
	PROCESS_HAS_METHOD(unix, open_multicast_group);
#endif
#endif
}

void vars_of_process_unix(void)
{
	Fprovide(intern("unix-processes"));
}

#endif				/* !defined (NO_SUBPROCESSES) */
