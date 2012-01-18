/* SXEmacs -- Fully extensible Emacs, running on Unix and other platforms.
   Copyright (C) 1985, 1986, 1987, 1992, 1993, 1994
   Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2000, 2002 Ben Wing.
   Copyright (C) 2004 Steve Youngs.

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


/* Synched up with: Mule 2.0, FSF 19.28. */

/* Capsule summary of the various releases of Lucid Emacs/SXEmacs and
   FSF/GNU Emacs.  Provided here for use in cross-referencing version
   releases and dates in comments, esp. in the authorship comments at
   the beginning of each file.  More information about history can be
   found in the beginning of the Internals Manual and in the About page.

-- A time line for Lucid Emacs/XEmacs is

version 19.0 shipped with Energize 1.0, April 1992.
version 19.1 released June 4, 1992.
version 19.2 released June 19, 1992.
version 19.3 released September 9, 1992.
version 19.4 released January 21, 1993.
version 19.5 was a repackaging of 19.4 with a few bug fixes and
  shipped with Energize 2.0.  Never released to the net.
version 19.6 released April 9, 1993.
version 19.7 was a repackaging of 19.6 with a few bug fixes and
  shipped with Energize 2.1.  Never released to the net.
version 19.8 released September 6, 1993.
version 19.9 released January 12, 1994.
version 19.10 released May 27, 1994.
version 19.11 (first XEmacs) released September 13, 1994.
version 19.12 released June 23, 1995.
version 19.13 released September 1, 1995.
version 19.14 released June 23, 1996.
version 20.0 released February 9, 1997.
version 19.15 released March 28, 1997.
version 20.1 (not released to the net) April 15, 1997.
version 20.2 released May 16, 1997.
version 19.16 released October 31, 1997.
version 20.3 (the first stable version of XEmacs 20.x) released
  November 30, 1997.
version 20.4 released February 28, 1998.

-- A time line for GNU Emacs version 19 is

version 19.7 (beta) (first beta release) released May 22, 1993.
version 19.8 (beta) released May 27, 1993.
version 19.9 (beta) released May 27, 1993.
version 19.10 (beta) released May 30, 1993.
version 19.11 (beta) released June 1, 1993.
version 19.12 (beta) released June 2, 1993.
version 19.13 (beta) released June 8, 1993.
version 19.14 (beta) released June 17, 1993.
version 19.15 (beta) released June 19, 1993.
version 19.16 (beta) released July 6, 1993.
version 19.17 (beta) released late July, 1993.
version 19.18 (beta) released August 9, 1993.
version 19.19 (beta) released August 15, 1993.
version 19.20 (beta) released November 17, 1993.
version 19.21 (beta) released November 17, 1993.
version 19.22 (beta) released November 28, 1993.
version 19.23 (beta) released May 17, 1994.
version 19.24 (beta) released May 16, 1994.
version 19.25 (beta) released June 3, 1994.
version 19.26 (beta) released September 11, 1994.
version 19.27 (beta) released September 14, 1994.
version 19.28 (first ``official'' release) released November 1, 1994.
version 19.29 released June 21, 1995.
version 19.30 released November 24, 1995.
version 19.31 released May 25, 1996.
version 19.32 released July 31, 1996.
version 19.33 released August 11, 1996.
version 19.34 released August 21, 1996.
version 19.34b released September 6, 1996.

-- A time line for GNU Emacs version 20 is

version 20.1 released September 17, 1997.
version 20.2 released September 20, 1997.
version 20.3 released August 19, 1998.

-- A time line for GNU Emacs version 18 and older is

GNU Emacs version 13 (the first public release) was released on
  March 20, 1985.
GNU Emacs version 15 (15.34) was released on May 7, 1985 and
  shared some code with a version of Emacs written by James Gosling (the
  same James Gosling who later created the Java language).
GNU Emacs version 16 (first released version was 16.56) was released on
  July 15, 1985.  All Gosling code was removed due to potential copyright
  problems with the code.
version 16.57: released on September 16, 1985.
versions 16.58, 16.59: released on September 17, 1985.
version 16.60: released on September 19, 1985.  These later version 16's
  incorporated patches from the net, esp. for getting Emacs to work under
  System V.
version 17.36 (first official v17 release) released on December 20, 1985.
  Included a TeX-able user manual.  First official unpatched version that
   worked on vanilla System V machines.
version 17.43 (second official v17 release) released on January 25, 1986.
version 17.45 released on January 30, 1986.
version 17.46 released on February 4, 1986.
version 17.48 released on February 10, 1986.
version 17.49 released on February 12, 1986.
version 17.55 released on March 18, 1986.
version 17.57 released on March 27, 1986.
version 17.58 released on April 4, 1986.
version 17.61 released on April 12, 1986.
version 17.63 released on May 7, 1986.
version 17.64 released on May 12, 1986.
version 18.24 (a beta version) released on October 2, 1986.
version 18.30 (a beta version) released on November 15, 1986.
version 18.31 (a beta version) released on November 23, 1986.
version 18.32 (a beta version) released on December 7, 1986.
version 18.33 (a beta version) released on December 12, 1986.
version 18.35 (a beta version) released on January 5, 1987.
version 18.36 (a beta version) released on January 21, 1987.
January 27, 1987: The Great Usenet Renaming.  net.emacs is now comp.emacs.
version 18.37 (a beta version) released on February 12, 1987.
version 18.38 (a beta version) released on March 3, 1987.
version 18.39 (a beta version) released on March 14, 1987.
version 18.40 (a beta version) released on March 18, 1987.
version 18.41 (the first ``official'' release) released on March 22, 1987.
version 18.45 released on June 2, 1987.
version 18.46 released on June 9, 1987.
version 18.47 released on June 18, 1987.
version 18.48 released on September 3, 1987.
version 18.49 released on September 18, 1987.
version 18.50 released on February 13, 1988.
version 18.51 released on May 7, 1988.
version 18.52 released on September 1, 1988.
version 18.53 released on February 24, 1989.
version 18.54 released on April 26, 1989.
version 18.55 released on August 23, 1989.  This is the earliest version
  that is still available by FTP.
version 18.56 released on January 17, 1991.
version 18.57 released late January, 1991.
version 18.58 released ?????.
version 18.59 released October 31, 1992.

*/

/* Note: It is necessary to specify <config.h> and not "config.h" in
   order for the --srcdir type of compilation to work properly.
   Otherwise the config.h from the srcdir, rather than the one from
   the build dir, will be used. */

#include <config.h>
#include "lisp.h"

#include "backtrace.h"		/* run-emacs-from-temacs needs this */
#include "buffer.h"
#include "commands.h"
#include "ui/console.h"
#include "process.h"
#include "ui/redisplay.h"
#include "ui/frame.h"
#include "sysdep.h"

#include "syssignal.h"		/* Always include before systty.h */
#include "ui/systty.h"
#include "sysfile.h"
#include "systime.h"

#if defined WITH_EMODULES && defined HAVE_EMODULES
# include "emodules-ng.h"
#endif

#ifdef PDUMP
#include "dumper.h"
#endif

#ifndef SEPCHAR
#define SEPCHAR ':'
#endif

#ifdef QUANTIFY
#include <quantify.h>
#endif

#if defined WITH_EMODULES && defined HAVE_EMODULES && 0
#include "sysdll.h"
#endif

#if defined (HAVE_LOCALE_H) && \
   (defined (I18N2) || defined (I18N3) || defined (I18N4))
#include <locale.h>
#endif

/* For PATH_EXEC */
#include <sxe-paths.h>

/* for stack exploitation */
#if defined HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif

/* for the reinit funs */
#include "skiplist.h"
#include "dllist.h"
#include "elhash.h"

#if defined HAVE_BDWGC && defined EF_USE_BDWGC
# if defined HAVE_GC_GC_H
#  include "gc/gc.h"
# elif defined HAVE_GC_H
#  include "gc.h"
# elif 1
/* declare the 3 funs we need */
extern void *GC_init(void);
# else
#  error Go back to your planet!
# endif
#endif	/* HAVE_BDWGC */

#if defined (HEAP_IN_DATA) && !defined(PDUMP)
void report_sheap_usage(int die_if_pure_storage_exceeded);
#endif

#if !defined (SYSTEM_MALLOC) && !defined (DOUG_LEA_MALLOC)
extern void *(*__malloc_hook) (size_t);
extern void *(*__realloc_hook) (void *, size_t);
extern void (*__free_hook) (void *);
#endif				/* not SYSTEM_MALLOC && not DOUG_LEA_MALLOC */

/* Command line args from shell, as list of strings */
Lisp_Object Vcommand_line_args;

/* Set nonzero after SXEmacs has started up the first time.
  Prevents reinitialization of the Lisp world and keymaps
  on subsequent starts.  */
int initialized;

#ifdef DOUG_LEA_MALLOC
# include <malloc.h>
/* Preserves a pointer to the memory allocated that copies that
   static data inside glibc's malloc.  */
static void *malloc_state_ptr;
#endif				/* DOUG_LEA_MALLOC */

# ifdef REL_ALLOC
void r_alloc_reinit(void);
# endif

/* Variable whose value is symbol giving operating system type. */
Lisp_Object Vsystem_type;

/* Variable whose value is string giving configuration built for.  */
Lisp_Object Vsystem_configuration;

/* Variable whose value is string containing the configuration options
   SXEmacs was built with.  */
Lisp_Object Vsystem_configuration_options;

/* Version numbers and strings */
Lisp_Object Vemacs_major_version;
Lisp_Object Vemacs_minor_version;
Lisp_Object Vemacs_patch_level;
Lisp_Object Vemacs_beta_version;
Lisp_Object Vsxemacs_git_version;
Lisp_Object Vsxemacs_codename;
#ifdef INFODOCK
Lisp_Object Vinfodock_major_version;
Lisp_Object Vinfodock_minor_version;
Lisp_Object Vinfodock_build_version;
#endif

/* The path under which SXEmacs was invoked. */
Lisp_Object Vinvocation_path;

/* The name under which SXEmacs was invoked, with any leading directory
   names discarded.  */
Lisp_Object Vinvocation_name;

/* The directory name from which SXEmacs was invoked.  */
Lisp_Object Vinvocation_directory;

#if 0				/* FSFmacs */
/* The directory name in which to find subdirs such as lisp and etc.
   nil means get them only from PATH_LOADSEARCH.  */
Lisp_Object Vinstallation_directory;
#endif

Lisp_Object Vemacs_program_name, Vemacs_program_version;
Lisp_Object Vexec_path;
Lisp_Object Vexec_directory, Vconfigure_exec_directory;
Lisp_Object Vlisp_directory, Vconfigure_lisp_directory;
Lisp_Object Vmule_lisp_directory, Vconfigure_mule_lisp_directory;
Lisp_Object Vmodule_directory, Vconfigure_module_directory;
Lisp_Object Vsite_module_directory, Vconfigure_site_module_directory;
Lisp_Object Vconfigure_package_path;
Lisp_Object Vdata_directory, Vconfigure_data_directory;
Lisp_Object Vdoc_directory, Vconfigure_doc_directory;
Lisp_Object Vconfigure_lock_directory;
Lisp_Object Vdata_directory_list;
Lisp_Object Vconfigure_info_directory;
Lisp_Object Vconfigure_info_path;
Lisp_Object Vinternal_error_checking;
Lisp_Object Vmail_lock_methods, Vconfigure_mail_lock_method;
Lisp_Object Vpath_separator;

/* The default base directory SXEmacs is installed under. */
Lisp_Object Vconfigure_exec_prefix_directory, Vconfigure_prefix_directory;

/* If nonzero, set SXEmacs to run at this priority.  This is also used
   in child_setup and sys_suspend to make sure subshells run at normal
   priority. */
Fixnum emacs_priority;

/* Some FSF junk with running_asynch_code, to preserve the match
   data.  Not necessary because we don't call process filters
   asynchronously (i.e. from within QUIT). */
/* #### Delete this when merging the rest of my code */
int running_asynch_code;

/* If non-zero, a window-system was specified on the command line. */
int display_arg;

/* Type of display specified.  We cannot use a Lisp symbol here because
   Lisp symbols may not initialized at the time that we set this
   variable. */
const char *display_use;

/* If non-zero, then the early error handler will only print the error
   message and exit. */
int suppress_early_error_handler_backtrace;

/* An address near the bottom of the stack.
   Tells GC how to save a copy of the stack.  */
char *stack_bottom;
/* the stack size as imposed by the system */
size_t sys_stk_sz = 0;

#ifdef USG_SHARED_LIBRARIES
/* If nonzero, this is the place to put the end of the writable segment
   at startup.  */

uintptr_t bss_end = 0;
#endif

/* Number of bytes of writable memory we can expect to be able to get */
#ifdef _RLIM_T_DECLARED
rlim_t lim_data;
#else
unsigned long lim_data;
#endif

/* WARNING!

   Some LISP-visible command-line options are set by SXEmacs _before_ the
   data is dumped in building a --pdump SXEmacs, but used _after_ it is
   restored in normal operation.  Thus the dump-time values overwrite the
   values SXEmacs is getting at runtime.  Such variables must be saved
   before loading the dumpfile, and restored afterward.

   Therefore these variables may not be initialized in vars_of_emacs().

   The save/restore is done immediately before and after pdump_load() in
   main_1().  See that function for the current list of protected variables.

   Note that saving/restoring is only necessary for a few variables that are
     o command line arguments effective at runtime (as opposed to dump-time),
     o parsed before pdump_load, and
     o exported to Lisp via a DEFVAR.
*/

/* Nonzero means running SXEmacs without interactive terminal.  */

int noninteractive;

/* Value of Lisp variable `noninteractive'.
   Normally same as C variable `noninteractive'
   but nothing terrible happens if user sets this one.

   Shadowed from the pdumper by `noninteractive'. */

int noninteractive1;

/* Nonzero means don't perform site-modules searches at startup */
int inhibit_site_modules;

/* Nonzero means don't respect early packages at startup */
int inhibit_early_packages;

/* Nonzero means we warn about early packages shadowing late packages at startup */
int warn_early_package_shadows;

/* Nonzero means don't load package autoloads at startup */
int inhibit_autoloads;

/* Nonzero means don't load the dump file (ignored if not PDUMP)  */
int nodumpfile;

/* Nonzero means we assume all ttys are 8 color ANSI terminals */
int assume_colorterm;

/* Nonzero means print debug information about path searching */
int debug_paths;

/* Save argv and argc.  */
static Extbyte **initial_argv;	/* #### currently unused */
static int initial_argc;	/* #### currently unused */

static void sort_args(int argc, char **argv);

Lisp_Object Qkill_emacs_hook;
Lisp_Object Qsave_buffers_kill_emacs;

extern Lisp_Object Vlisp_EXEC_SUFFIXES;

/* Ben's capsule summary about expected and unexpected exits from SXEmacs.

   Expected exits occur when the user directs SXEmacs to exit, for example
   by pressing the close button on the only frame in SXEmacs, or by typing
   C-x C-c.  This runs `save-buffers-kill-emacs', which saves any necessary
   buffers, and then exits using the primitive `kill-emacs'.

   However, unexpected exits occur in a few different ways:

     -- a memory access violation or other hardware-generated exception
        occurs.  This is the worst possible problem to deal with, because
        the fault can occur while SXEmacs is in any state whatsoever, even
        quite unstable ones.  As a result, we need to be *extremely* careful
        what we do.
     -- we are using one X display (or if we've used more, we've closed the
        others already), and some hardware or other problem happens and
        suddenly we've lost our connection to the display.  In this situation,
	things are not so dire as in the last one; our code itself isn't
	trashed, so we can continue execution as normal, after having set
	things up so that we can exit at the appropriate time.  Our exit
	still needs to be of the emergency nature; we have no displays, so
	any attempts to use them will fail.  We simply want to auto-save
	(the single most important thing to do during shut-down), do minimal
	cleanup of stuff that has an independent existence outside of SXEmacs,
	and exit.

	Currently, both unexpected exit scenarios described above set
	preparing_for_armageddon to indicate that nonessential and possibly
	dangerous things should not be done, specifically:

	-- no garbage collection.
	-- no hooks are run.
	-- no messages of any sort from autosaving.
	-- autosaving tries harder, ignoring certain failures.
	-- existing frames are not deleted.

	(Also, all places that set preparing_for_armageddon also
	set dont_check_for_quit.  This happens separately because it's
	also necessary to set other variables to make absolutely sure
	no quitting happens.)

	In the first scenario above (the access violation), we also set
	fatal_error_in_progress.  This causes more things to not happen:

	-- assertion failures do not abort.
	-- printing code does not do code conversion or gettext when
	   printing to stdout/stderr.
*/

/* Nonzero if handling a fatal error already. */
int fatal_error_in_progress;

/* Non-nil means we're going down, so we better not run any hooks
   or do other non-essential stuff. */
int preparing_for_armageddon;

/* Nonzero means we're in an unstable situation and need to skip
   i18n conversions and such during printing. */
int inhibit_non_essential_printing_operations;

static JMP_BUF run_temacs_catch;

static int run_temacs_argc;
static char **run_temacs_argv;
static char *run_temacs_args;
static EMACS_INT run_temacs_argv_size;
static EMACS_INT run_temacs_args_size;

static void shut_down_emacs(int sig, Lisp_Object stuff, int no_auto_save);

/* ------------------------------- */
/*  low-level debugging functions  */
/* ------------------------------- */

#define debugging_breakpoint()

void
debug_break(void)
{
	debugging_breakpoint();
}

/* #### There must be a better way!!!! */

static JMP_BUF memory_error_jump;

#if 1
static SIGTYPE
debug_memory_error(int signum)
{
	EMACS_REESTABLISH_SIGNAL(signum, debug_memory_error);
	EMACS_UNBLOCK_SIGNAL(signum);
	LONGJMP(memory_error_jump, 1);
}
#endif

static char dummy_char;

/* Return whether all bytes in the specified memory block can be read. */
int
debug_can_access_memory(void *ptr, Bytecount len)
{
	/* Use volatile to protect variables from being clobbered by longjmp. */
	SIGTYPE(*volatile old_sigbus) (int);
	SIGTYPE(*volatile old_sigsegv) (int);
	volatile int old_errno = errno;
	volatile int retval = 1;

	if (!SETJMP(memory_error_jump)) {
#if 1
		old_sigbus =
			(SIGTYPE(*)(int))signal(SIGBUS, debug_memory_error);
		old_sigsegv =
			(SIGTYPE(*)(int))signal(SIGSEGV, debug_memory_error);
#endif
                /*
                 * Examine memory pool at PTR, trying to cheat
                 * compiler's optimisations.
                 */
                while (len-- > 0) {
                        dummy_char = ((char*)ptr)[len];
		}
	} else {
		retval = 0;
	}
	signal(SIGBUS, old_sigbus);
	signal(SIGSEGV, old_sigsegv);
	errno = old_errno;

	return retval;
}

#ifdef DEBUG_SXEMACS

DEFUN("force-debugging-signal", Fforce_debugging_signal, 0, 1, 0, /*
Cause SXEmacs to enter the debugger.
On some systems, there may be no way to do this gracefully; if so,
nothing happens unless ABORT is non-nil, in which case SXEmacs will
abort() -- a sure-fire way to immediately get back to the debugger,
but also a sure-fire way to kill SXEmacs (and dump core on Unix
systems)!
*/
      (abort_))
{
	debugging_breakpoint();
	if (!NILP(abort_))
		abort();
	return Qnil;
}

#endif				/* DEBUG_SXEMACS */

static void
ensure_no_quitting_from_now_on(void)
{
	/* make sure no quitting from now on!! */
	dont_check_for_quit = 1;
	Vinhibit_quit = Qt;
	Vquit_flag = Qnil;
}

#if 1
/* Handle bus errors, illegal instruction, etc. */
SIGTYPE
fatal_error_signal(int sig)
{
	fatal_error_in_progress++;
	inhibit_non_essential_printing_operations = 1;
	preparing_for_armageddon = 1;

	ensure_no_quitting_from_now_on();

	/* Unblock the signal so that if the same signal gets sent in the
	   code below, we avoid a deadlock. */
	EMACS_UNBLOCK_SIGNAL(sig);

	/* Only try auto-saving first time through.  If we crash in auto-saving,
	   don't do it again. */
	if (fatal_error_in_progress == 1) {
		Fdo_auto_save(Qt, Qnil);	/* do this before anything hazardous */
		/* Do this so that the variable has the same value of 2 regardless of
		   whether we made it through auto-saving correctly. */
		fatal_error_in_progress++;
	} else if (fatal_error_in_progress == 2)
		stderr_out("WARNING: Unable to auto-save your files properly.\n"
			   "Some or all may in fact have been auto-saved.\n"
			   "\n");

	/* Now, reset our signal handler, so the next time, we just die.
	   Don't do this before auto-saving. */
	signal(sig, SIG_DFL);

	/* Keep in mind that there's more than one signal that we can crash
	   on. */
	/* If fatal error occurs in code below, avoid infinite recursion.  */
	if (fatal_error_in_progress <= 2) {
		shut_down_emacs(sig, Qnil, 1);
		stderr_out("\nLisp backtrace follows:\n\n");
		Fbacktrace(Qexternal_debugging_output, Qt);
# if 0				/* This is evil, rarely useful, and causes grief in some cases. */
		/* Check for Sun-style stack printing via /proc */
		{
			const char *pstack = "/usr/proc/bin/pstack";
			if (access(pstack, X_OK) == 0) {
				char buf[100];
				int sz = snprintf(buf, sizeof(buf), "%s %d >&2", pstack,
					(int)getpid());
				stderr_out("\nC backtrace follows:\n"
					   "(A real debugger may provide better information)\n\n");
				if ( sz >= 0 && sz < sizeof(buf)) {
					sz = system(buf);
					if ( sz != 0 )
						stderr_out("\nStacktrace utility execution error code: %d\n", sz);
				} else {
					stderr_out("\nCould not build command line for stacktrace utility.\n");
				}
			}
		}
# endif
	}
	/* Signal the same code; this time it will really be fatal. */
	kill(getpid(), sig);
	SIGRETURN;
}
#endif


#ifdef SIGDANGER

/* Handler for SIGDANGER.  */
SIGTYPE
memory_warning_signal(int sig)
{
	/* #### bad bad bad; this function shouldn't do anything except
	   set a flag, or weird corruption could happen. */
	signal(sig, memory_warning_signal);

	malloc_warning
	    (GETTEXT
	     ("Operating system warns that virtual memory is running low.\n"));

	/* It might be unsafe to call do_auto_save now.  */
	force_auto_save_soon();
}
#endif				/* SIGDANGER */

/* Code for dealing with Lisp access to the Unix command line */

static Lisp_Object
make_arg_list_1(int argc, Extbyte ** argv, int skip_args)
{
	Lisp_Object result = Qnil;
	REGISTER int i;

	for (i = argc - 1; i >= 0; i--) {
                if (i != 0 && i <= skip_args)
                        continue;

                result = Fcons(build_ext_string(argv[i], Qcommand_argument_encoding), result);
	}
	return result;
}

Lisp_Object
make_arg_list(int argc, Extbyte ** argv)
{
	return make_arg_list_1(argc, argv, 0);
}

/* Calling functions are also responsible for calling free_argc_argv
   when they are done with the generated list. */
void
make_argc_argv(Lisp_Object argv_list, int *argc, Extbyte *** argv)
{
	Lisp_Object next;
	int n = XINT(Flength(argv_list));
	REGISTER int i;
	*argv = (Extbyte **) malloc((n + 1) * sizeof(Extbyte *));

	for (i = 0, next = argv_list; i < n; i++, next = XCDR(next)) {
		const Extbyte *temp;
		CHECK_STRING(XCAR(next));

		LISP_STRING_TO_EXTERNAL(XCAR(next), temp,
					Qcommand_argument_encoding);
		(*argv)[i] = strdup(temp);
	}
	(*argv)[n] = 0;
	*argc = i;
}

void free_argc_argv(Extbyte ** argv)
{
	int elt = 0;

	while (argv[elt]) {
		free(argv[elt]);
		elt++;
	}
	free(argv);
}

static void init_cmdargs(int argc, Extbyte ** argv, int skip_args)
{
	initial_argv = argv;
	initial_argc = argc;

	Vcommand_line_args = make_arg_list_1(argc, argv, skip_args);
}

DEFUN("invocation-name", Finvocation_name, 0, 0, 0, /*
Return the program name that was used to run SXEmacs.
Any directory names are omitted.
*/
      ())
{
	return Fcopy_sequence(Vinvocation_name);
}

DEFUN("invocation-directory", Finvocation_directory, 0, 0, 0, /*
Return the directory name in which the Emacs executable was located.
*/
      ())
{
	return Fcopy_sequence(Vinvocation_directory);
}

#ifdef I18N4
				/* #### - don't know why I18N4 on SunOS/JLE
				   can't deal with this.  It's a potential
				   bug that needs to be looked at. */
# undef RUN_TIME_REMAP
#endif

/* Test whether the next argument in ARGV matches SSTR or a prefix of
   LSTR (at least MINLEN characters).  If so, then if VALPTR is non-null
   (the argument is supposed to have a value) store in *VALPTR either
   the next argument or the portion of this one after the equal sign.
   ARGV is read starting at position *SKIPPTR; this index is advanced
   by the number of arguments used.

   Too bad we can't just use getopt for all of this, but we don't have
   enough information to do it right.  */

static int
argmatch(char **argv, int argc, char *sstr, char *lstr,
	 int minlen, char **valptr, int *skipptr)
{
	char *p = NULL;
	int arglen;
	char *arg;

	/* Don't access argv[argc]; give up in advance.  */
	if (argc <= *skipptr + 1)
		return 0;

	arg = argv[*skipptr + 1];
	if (arg == NULL)
		return 0;
	if (strcmp(arg, sstr) == 0) {
		if (valptr != NULL) {
			*valptr = argv[*skipptr + 2];
			*skipptr += 2;
		} else
			*skipptr += 1;
		return 1;
	}
	arglen = (valptr != NULL && (p = strchr(arg, '=')) != NULL
		  ? p - arg : (int)strlen(arg));
	if (lstr == 0 || arglen < minlen || strncmp(arg, lstr, arglen) != 0)
		return 0;
	else if (valptr == NULL) {
		*skipptr += 1;
		return 1;
	} else if (p != NULL) {
		*valptr = p + 1;
		*skipptr += 1;
		return 1;
	} else if (argv[*skipptr + 2] != NULL) {
		*valptr = argv[*skipptr + 2];
		*skipptr += 2;
		return 1;
	} else {
		return 0;
	}
}

static int
make_docfile(int c, char **v)
{
#define make_docfile_prog	"make-docfile\0"
#define make_docfile_opt	"--make-docfile"
	/* C99 we need you */
	size_t edlen = XSTRING_LENGTH(Vexec_directory);
	char mdocfile[edlen+countof(make_docfile_prog)];
	char **newargv = xnew_array_and_zero(char*, c), **p;
	int  ret = -1;

	/* set up the program call */
	xstrncpy(mdocfile,
		 (char*)XSTRING_DATA(Vexec_directory),
		 XSTRING_LENGTH(Vexec_directory));
	xstrncpy(mdocfile+XSTRING_LENGTH(Vexec_directory),
		 make_docfile_prog, countof(make_docfile_prog));

	/* find the --make-docfile option */
	for (p = v; *p; p++) {
		if (strncmp(*p, make_docfile_opt,
			    countof(make_docfile_opt)) == 0) {
			p++;
			break;
		}
	}

	/* fill the new argv array */
	newargv[0] = make_docfile_prog;
	for (char **o = p, **n = &newargv[1]; *o;) {
		*n++ = *o++;
	}
	ret = execv(mdocfile, newargv);
	xfree(newargv);
	return ret;
}

static inline void*
__get_sp(void)
{
	void *sp = 0;

#if 0
/* we need some checks whether this is supported! */
	__asm__ __volatile__ (
		"movl %%esp, %[stkptr]\n\t"
		: [stkptr] "=m" (sp));
#endif

	return sp;
}

static size_t
__sys_stk_sz(void)
{
/* return the stack size limit */
#if defined HAVE_GETRLIMIT64
	struct rlimit64 foo;
	(void)getrlimit64(RLIMIT_STACK, &foo);
#elif defined HAVE_GETRLIMIT
	struct rlimit foo;
	(void)getrlimit(RLIMIT_STACK, &foo);
#else
	/* bollocks, maybe just a small one? 64k? */
	struct {size_t rlim_cur;} foo = {65536};
#endif
	return foo.rlim_cur;
}


/* Make stack traces always identify version + configuration */
#define main_1 STACK_TRACE_EYE_CATCHER

/* This function is not static, so that the compiler is less likely to
   inline it, which would make it not show up in stack traces.

   The restart argument is a flag that indicates that main_1 is now
   being called for the second time in this invocation of sxemacs; this can
   only happen in an sxemacs that is not loaded with dumped data (temacs
   with the conventional dumper or sxemacs -nd with the pdumper).   See
   Frun_emacs_from_temacs().

   restart interacts with initialized as follows (per Olivier Galibert):

     It's perverted.

     initialized==0 => temacs
     initialized!=0 && restart!=0 => run-temacs
     initialized!=0 && restart==0 => sxemacs/post pdump_load()
*/
DECLARE_DOESNT_RETURN(main_1(int, char **, char **, int));
DOESNT_RETURN main_1(int argc, char **argv, char **envp, int restart)
{
	char stack_bottom_variable;
	int skip_args = 0;
	Lisp_Object load_me;
	int inhibit_window_system;
#ifdef NeXT
	extern int malloc_cookie;
#endif

#if (!defined (SYSTEM_MALLOC) && !defined (HAVE_LIBMCHECK)	\
     && !defined (DOUG_LEA_MALLOC))
	/* Make sure that any libraries we link against haven't installed a
	   hook for a gmalloc of a potentially incompatible version. */
	/* If we're using libmcheck, the hooks have already been initialized, */
	/* don't touch them. -slb */
	__malloc_hook = NULL;
	__realloc_hook = NULL;
	__free_hook = NULL;
#endif	/* not SYSTEM_MALLOC or HAVE_LIBMCHECK or DOUG_LEA_MALLOC */

	noninteractive = 0;
	inhibit_non_essential_printing_operations = 1;

#ifdef NeXT
	/* 19-Jun-1995 -baw
	 * NeXT secret magic, ripped from Emacs-for-NS by Carl Edman
	 * <cedman@princeton.edu>.  Note that even Carl doesn't know what this
	 * does; it was provided by NeXT, and it presumable makes NS's mallocator
	 * work with dumping.  But malloc_jumpstart() and malloc_freezedry() in
	 * unexnext.c are both completely undocumented, even in NS header files!
	 * But hey, it solves all NS related memory problems, so who's
	 * complaining? */
	if (initialized && malloc_jumpstart(malloc_cookie) != 0)
		stderr_out("malloc jumpstart failed!\n");
#endif				/* NeXT */

	/*
	   #if defined (GNU_MALLOC) && \
	   defined (ERROR_CHECK_MALLOC) && \
	   !defined (HAVE_LIBMCHECK)
	 */
#if defined(LOSING_GCC_DESTRUCTOR_FREE_BUG)
	/* Prior to SXEmacs 21, this was `#if 0'ed out.  */
	/* I'm enabling this because it is the only reliable way I've found to */
	/* prevent a very annoying problem where GCC will attempt to free(3) */
	/* memory at exit() and cause a coredump. */
#if 0
	init_free_hook();
#endif
#endif

	sort_args(argc, argv);

#if defined(_SCO_DS)
	environ = envp;
#endif

	/* Record (approximately) where the stack begins.  */
	stack_bottom = &stack_bottom_variable;
	/* and determine the system's stack limit */
	sys_stk_sz = __sys_stk_sz();

#ifdef USG_SHARED_LIBRARIES
	if (bss_end)
		brk((void *)bss_end);
#endif

	clearerr(stdin);

#if defined (HAVE_MMAP) && defined (REL_ALLOC)
	/* ralloc can only be used if using the GNU memory allocator. */
	init_ralloc();
#elif defined (REL_ALLOC) && !defined(DOUG_LEA_MALLOC)
	if (initialized)
		init_ralloc();
#endif

#ifdef HAVE_SOCKS
	if (initialized)
		SOCKSinit(argv[0]);
#endif				/* HAVE_SOCKS */

#if !defined SYSTEM_MALLOC && !(defined HAVE_BDWGC && defined EF_USE_BDWGC)
	if (!initialized)
		/* Arrange to get warning messages as memory fills up.  */
		memory_warnings(0, malloc_warning);
#endif				/* not SYSTEM_MALLOC */

#ifdef SET_EMACS_PRIORITY
	if (emacs_priority != 0)
		nice(-emacs_priority);
	setuid(getuid());
#endif				/* SET_EMACS_PRIORITY */

#ifdef EXTRA_INITIALIZE
	EXTRA_INITIALIZE;
#endif

#ifdef HAVE_WINDOW_SYSTEM
	inhibit_window_system = 0;
#else
	inhibit_window_system = 1;
#endif

	/* Handle the --make-docfile argument */
	if (argmatch(argv, argc,
		     "--make-docfile", 0, 9, NULL, &skip_args)) {

		/* we need load the dump file as the exec-directory is in
		   there */
		if (UNLIKELY(!pdump_load(argv[0]))) {
			exit(1);
		}

		exit(make_docfile(argc, argv));
	}

	/* Handle the -sd/--show-dump-id switch, which means show the hex
	   dump_id and quit */
	if (argmatch(argv, argc,
		     "-sd", "--show-dump-id",
		     9, NULL, &skip_args)) {
#ifdef PDUMP
		printf("%08x\n", dump_id);
#else
		printf
		    ("Portable dumper not configured; -sd just forces exit.\n");
#endif
		exit(0);
	}

	/* Handle the -t switch, which specifies filename to use as terminal */
	{
		char *term;
		if (argmatch
		    (argv, argc, "-t", "--terminal", 4, &term, &skip_args)) {
			close(0);
			close(1);
			if (open(term, O_RDWR | OPEN_BINARY, 2) < 0)
				fatal("%s: %s", term, strerror(errno));
			if( dup(0) < 0)
				fatal("dup failed %s: %s", term, strerror(errno));
			if (!isatty(0))
				fatal("%s: not a tty", term);

#if 0
			stderr_out("Using %s", ttyname(0));
#endif
			stderr_out("Using %s", term);
			inhibit_window_system = 1;	/* -t => -nw */
		}
	}

	/* Handle the --no-dump-file/-nd switch, which means don't
	 * load the dump file (ignored when not using pdump) */
	if (argmatch(argv, argc, "-nd", "--no-dump-file", 7,
		     NULL, &skip_args)) {
		nodumpfile = 1;
	}

	if (argmatch(argv, argc, "-ct", "--color-terminal", 5,
		     NULL, &skip_args)) {
		assume_colorterm = 1;
	}

	/* Handle -nw switch */
	if (argmatch(argv, argc, "-nw", "--no-windows", 6, NULL, &skip_args))
		inhibit_window_system = 1;

	/* Handle the -batch switch, which means don't do interactive display */
	if (argmatch(argv, argc, "-batch", "--batch", 5, NULL, &skip_args)) {
#if 0				/* I don't think this is correct. */
		inhibit_autoloads = 1;
#endif
		noninteractive = 1;
	}

	if (argmatch(argv, argc, "-debug-paths", "--debug-paths",
		     11, NULL, &skip_args))
		debug_paths = 1;

	/* Partially handle -no-autoloads, -no-early-packages and -vanilla.
	   Packages */
	/* are searched prior to the rest of the command line being parsed in */
	/* startup.el */
	if (argmatch(argv, argc, "-no-early-packages", "--no-early-packages",
		     6, NULL, &skip_args)) {
		inhibit_early_packages = 1;
		skip_args--;
	}
#if defined WITH_EMODULES && defined HAVE_EMODULES
	if (argmatch(argv, argc, "-no-site-modules", "--no-site-modules",
		     9, NULL, &skip_args)) {
		inhibit_site_modules = 1;
		skip_args--;
	}
#else
	inhibit_site_modules = 1;
#endif
	if (argmatch(argv, argc, "-vanilla", "--vanilla", 7, NULL, &skip_args)) {
		inhibit_early_packages = 1;
		skip_args--;
	}

	if (argmatch(argv, argc, "-no-autoloads", "--no-autoloads",
		     7, NULL, &skip_args)) {
		/* Inhibit everything */
		inhibit_autoloads = 1;
		skip_args--;
	}

	if (argmatch(argv, argc, "-debug-paths", "--debug-paths",
		     6, NULL, &skip_args)) {
		debug_paths = 1;
		skip_args--;
	}

	/* Partially handle the -version and -help switches: they imply -batch,
	   but are not removed from the list. */
	if (argmatch(argv, argc, "-help", "--help", 3, NULL, &skip_args))
		noninteractive = 1, skip_args--;

	if (argmatch(argv, argc, "-version", "--version", 3, NULL, &skip_args)
	    || argmatch(argv, argc, "-V", 0, 2, NULL, &skip_args))
		noninteractive = 1, skip_args--;

	/* Now, figure out which type of console is our first console. */

	display_arg = 0;

	if (noninteractive)
		display_use = "stream";
	else
		display_use = "tty";

#ifndef HAVE_TTY
	if (inhibit_window_system)
		fatal("Sorry, this SXEmacs was not compiled with TTY support");
#endif

#ifdef HAVE_WINDOW_SYSTEM
	/* Stupid kludge to catch command-line display spec.  We can't
	   handle this argument entirely in window-system-dependent code
	   because we don't even know which window-system-dependent code
	   to run until we've recognized this argument.  */
	if (!inhibit_window_system && !noninteractive) {
#ifdef HAVE_X_WINDOWS
		char *dpy = 0;
		int count_before = skip_args;

		if (argmatch(argv, argc, "-d", "--display", 3, &dpy, &skip_args)
		    || argmatch(argv, argc, "-display", 0, 3, &dpy,
				&skip_args)) {
			display_arg = 1;
			display_use = "x";
		}
		/* If we have the form --display=NAME,
		   convert it into  -d name.
		   This requires inserting a new element into argv.  */
		if (dpy != 0 && skip_args - count_before == 1) {
			char **new =
			    (char **)xmalloc(sizeof(char *) * (argc + 2));
			int j;

			for (j = 0; j < count_before + 1; j++)
				new[j] = argv[j];
			new[count_before + 1] = "-d";
			new[count_before + 2] = dpy;
			for (j = count_before + 2; j < argc; j++)
				new[j + 1] = argv[j];
			argv = new;
			argc++;
		}
		/* Change --display to -d, when its arg is separate.  */
		else if (dpy != 0 && skip_args > count_before
			 && argv[count_before + 1][1] == '-')
			argv[count_before + 1] = "-d";

		/* Don't actually discard this arg.  */
		skip_args = count_before;

		/* If there is a non-empty environment var DISPLAY, set
		   `display_use', but not `display_arg', which is only to be set
		   if the display was specified on the command line. */
		if ((dpy = getenv("DISPLAY")) && dpy[0])
			display_use = "x";

#endif				/* HAVE_X_WINDOWS */
	}
#endif				/* HAVE_WINDOW_SYSTEM */

	noninteractive1 = noninteractive;

	/****** Now initialize everything *******/

	/* First, do really basic environment initialization -- catching signals
	   and the like.  These functions have no dependence on any part of
	   the Lisp engine and need to be done both at dump time and at run time. */

	init_signals_very_early();
	init_data_very_early();	/* Catch math errors. */
#ifdef HAVE_FPFLOAT
	init_floatfns_very_early();	/* Catch floating-point math errors. */
#endif
	init_process_times_very_early();	/* Initialize our process timers.
						   As early as possible, of course,
						   so we can be fairly accurate. */
	init_intl_very_early();	/* set up the locale and domain for gettext and
				   such. */

	/* Now initialize the Lisp engine and the like.  Done only during
	   dumping.  No dependence on anything that may be in the user's
	   environment when the dumped SXEmacs is run.

	   We try to do things in an order that minimizes the non-obvious
	   dependencies between functions. */

	/* purify_flag 1 is correct even if CANNOT_DUMP.
	 * loadup.el will set to nil at end. */

	purify_flag = 0;
#ifdef PDUMP
	if (restart) {
		initialized = 1;
	} else if (nodumpfile) {
		initialized = 0;
		purify_flag = 1;
	} else {

		/* Keep command options from getting stomped.

		   Some LISP-visible options are changed by SXEmacs _after_ the data is
		   dumped in building a --pdump SXEmacs, but _before_ it is restored in
		   normal operation.  Thus the restored values overwrite the values
		   SXEmacs is getting at run-time.  Such variables must be saved here,
		   and restored after loading the dumped data.

		   Boy, this is ugly, but how else to do it?
		 */

		/* noninteractive1 is saved in noninteractive, which isn't
		   LISP-visible */
		int inhibit_early_packages_save = inhibit_early_packages;
		int inhibit_autoloads_save = inhibit_autoloads;
		int debug_paths_save = debug_paths;
		int inhibit_site_modules_save = inhibit_site_modules;

		initialized = pdump_load(argv[0]);

		/* Now unstomp everything */
		noninteractive1 = noninteractive;
		inhibit_early_packages = inhibit_early_packages_save;
		inhibit_autoloads = inhibit_autoloads_save;
		debug_paths = debug_paths_save;
		inhibit_site_modules = inhibit_site_modules_save;

		if (initialized)
			run_temacs_argc = -1;
		else
			purify_flag = 1;
	}
#else
	if (!initialized)
		purify_flag = 1;
#endif

	if (!initialized) {
		/* Initialize things so that new Lisp objects
		   can be created and objects can be staticpro'd.
		   Must be basically the very first thing done
		   because pretty much all of the initialization
		   routines below create new objects. */
		init_alloc_once_early();

		/* Initialize Qnil, Qt, Qunbound, and the
		   obarray.  After this, symbols can be
		   interned.  This depends on init_alloc_once_early(). */
		init_symbols_once_early();

		/* Declare the basic symbols pertaining to errors,
		   So that DEFERROR*() can be called. */
		init_errors_once_early();

		/* Make sure that opaque pointers can be created. */
		init_opaque_once_early();

		/* Now declare all the symbols and define all the Lisp primitives.

		   The *only* thing that the syms_of_*() functions are allowed to do
		   is call one of the following:

		   INIT_LRECORD_IMPLEMENTATION()
		   defsymbol(), DEFSYMBOL(), or DEFSYMBOL_MULTIWORD_PREDICATE()
		   defsubr() (i.e. DEFSUBR)
		   deferror(), DEFERROR(), or DEFERROR_STANDARD()
		   defkeyword() or DEFKEYWORD()

		   Order does not matter in these functions.
		 */

		syms_of_abbrev();
		syms_of_alloc();
		syms_of_buffer();
		syms_of_bytecode();
		syms_of_callint();
		syms_of_callproc();
		syms_of_casefiddle();
		syms_of_casetab();
		syms_of_chartab();
		syms_of_cmdloop();
		syms_of_cmds();
		syms_of_console();
		syms_of_data();
#ifdef DEBUG_SXEMACS
		syms_of_debug();
		syms_of_tests();
#endif				/* DEBUG_SXEMACS */
		syms_of_device();
#ifdef HAVE_DIALOGS
		syms_of_dialog();
#endif
		syms_of_dired();
		syms_of_doc();
		syms_of_editfns();
		syms_of_elhash();
		syms_of_emacs();
		syms_of_eval();
#ifdef HAVE_X_WINDOWS
		syms_of_event_Xt();
#endif
#ifdef HAVE_DRAGNDROP
		syms_of_dragdrop();
#endif
#ifdef EF_USE_ASYNEQ
		syms_of_event_queue();
		syms_of_workers();
		syms_of_worker_asyneq();
#endif
		syms_of_event_stream();
		syms_of_events();
		syms_of_extents();
		syms_of_faces();
		syms_of_fileio();
#ifdef CLASH_DETECTION
		syms_of_filelock();
#endif				/* CLASH_DETECTION */
		syms_of_floatfns();
		syms_of_fns();
		syms_of_font_lock();
		syms_of_frame();
		syms_of_general();
		syms_of_glyphs();
		syms_of_glyphs_eimage();
		syms_of_glyphs_widget();
		syms_of_gui();
		syms_of_gutter();
		syms_of_indent();
		syms_of_intl();
		syms_of_keymap();
		syms_of_lread();
		syms_of_macros();
		syms_of_marker();
		syms_of_md5();
#ifdef HAVE_DATABASE
		syms_of_database();
#endif
#ifdef HAVE_MENUBARS
		syms_of_menubar();
#endif
		syms_of_media();
		syms_of_minibuf();
		syms_of_dynacat();
#if defined WITH_EMODULES && defined HAVE_EMODULES && 0
		syms_of_module();
#elif defined WITH_EMODULES && defined HAVE_EMODULES
		syms_of_emodng();
#endif
		syms_of_objects();
		syms_of_print();
#if !defined (NO_SUBPROCESSES)
		syms_of_process();
#endif
		syms_of_profile();
#if defined (HAVE_MMAP) && defined (REL_ALLOC) && !defined(DOUG_LEA_MALLOC)
		syms_of_ralloc();
#endif				/* HAVE_MMAP && REL_ALLOC */
		syms_of_rangetab();
		syms_of_redisplay();
		syms_of_search();
		syms_of_select();
		syms_of_signal();
		syms_of_sound();
		syms_of_specifier();
		syms_of_symbols();
		syms_of_syntax();
#ifdef HAVE_SCROLLBARS
		syms_of_scrollbar();
#endif
#ifdef HAVE_TOOLBARS
		syms_of_toolbar();
#endif
		syms_of_undo();
		syms_of_widget();
		syms_of_window();

#ifdef HAVE_TTY
		syms_of_console_tty();
		syms_of_device_tty();
		syms_of_objects_tty();
#endif

#ifdef HAVE_X_WINDOWS
#ifdef HAVE_BALLOON_HELP
		syms_of_balloon_x();
#endif
		syms_of_device_x();
#ifdef HAVE_DIALOGS
		syms_of_dialog_x();
#endif
		syms_of_frame_x();
		syms_of_glyphs_x();
		syms_of_objects_x();
#ifdef HAVE_MENUBARS
		syms_of_menubar_x();
#endif
		syms_of_select_x();
#if defined (HAVE_MENUBARS) || defined (HAVE_SCROLLBARS) || defined (HAVE_DIALOGS) || defined (HAVE_TOOLBARS)
		syms_of_gui_x();
#endif
#ifdef HAVE_XIM
#ifdef XIM_XLIB
		syms_of_input_method_xlib();
#endif
#endif				/* HAVE_XIM */
#endif				/* HAVE_X_WINDOWS */

#ifdef MULE
		syms_of_mule();
		syms_of_mule_ccl();
		syms_of_mule_charset();
#endif
#ifdef FILE_CODING
		syms_of_file_coding();
#endif
#ifdef MULE
#ifdef HAVE_WNN
		syms_of_mule_wnn();
#endif
#ifdef HAVE_CANNA
		syms_of_mule_canna();
#endif				/* HAVE_CANNA */
#endif				/* MULE */

#ifdef SYMS_SYSTEM
		SYMS_SYSTEM;
#endif

#ifdef SYMS_MACHINE
		SYMS_MACHINE;
#endif

		/*
		   #if defined (GNU_MALLOC) && \
		   defined (ERROR_CHECK_MALLOC) && \
		   !defined (HAVE_LIBMCHECK)
		 */
		/* Prior to SXEmacs 21, this was `#if 0'ed out. -slb */
#if defined (LOSING_GCC_DESTRUCTOR_FREE_BUG)
		syms_of_free_hook();
#endif

#ifdef SUNPRO
		syms_of_sunpro();
#endif

#ifdef HAVE_LDAP
		syms_of_eldap();
#endif

#ifdef HAVE_GPM
		syms_of_gpmevent();
#endif

#ifdef HAVE_POSTGRESQL
		syms_of_postgresql();
#endif

#ifdef HAVE_OPENSSL
		syms_of_openssl();
#endif

#ifdef WITH_NUMBER_TYPES
		syms_of_ent();
#endif

#ifdef HAVE_LIBFFI
		syms_of_ffi();
#endif

		syms_of_dllist();
		syms_of_skiplist();
		syms_of_bloom();

		/* Now create the subtypes for the types that have them.
		   We do this before the vars_*() because more symbols
		   may get initialized here. */

		/* Now initialize the console types and associated symbols.
		   Other than the first function below, the functions may
		   make exactly the following function/macro calls:

		   INITIALIZE_CONSOLE_TYPE()
		   CONSOLE_HAS_METHOD()

		   For any given console type, the former macro must be called
		   before the any calls to the latter macro. */

		console_type_create();

		console_type_create_stream();

#ifdef HAVE_TTY
		console_type_create_tty();
		console_type_create_device_tty();
		console_type_create_frame_tty();
		console_type_create_objects_tty();
		console_type_create_redisplay_tty();
#endif

#ifdef HAVE_X_WINDOWS
		console_type_create_x();
		console_type_create_device_x();
		console_type_create_frame_x();
		console_type_create_glyphs_x();
		console_type_create_select_x();
#ifdef HAVE_MENUBARS
		console_type_create_menubar_x();
#endif
		console_type_create_objects_x();
		console_type_create_redisplay_x();
#ifdef HAVE_SCROLLBARS
		console_type_create_scrollbar_x();
#endif
#ifdef HAVE_TOOLBARS
		console_type_create_toolbar_x();
#endif
#ifdef HAVE_DIALOGS
		console_type_create_dialog_x();
#endif
#endif				/* HAVE_X_WINDOWS */

		/* Now initialize the specifier types and associated symbols.
		   Other than the first function below, the functions may
		   make exactly the following function/macro calls:

		   INITIALIZE_SPECIFIER_TYPE()
		   SPECIFIER_HAS_METHOD()

		   For any given specifier type, the former macro must be called
		   before the any calls to the latter macro. */

		specifier_type_create();

		specifier_type_create_image();
		specifier_type_create_gutter();
		specifier_type_create_objects();
#ifdef HAVE_TOOLBARS
		specifier_type_create_toolbar();
#endif

		/* Now initialize the structure types and associated symbols.
		   Other than the first function below, the functions may
		   make exactly the following function/macro calls:

		   define_structure_type()
		   define_structure_type_keyword()

		 */

		structure_type_create();

		structure_type_create_chartab();
		structure_type_create_faces();
		structure_type_create_rangetab();
		structure_type_create_hash_table();

		/* Now initialize the image instantiator formats and associated symbols.
		   Other than the first function below, the functions may
		   make exactly the following function/macro calls:

		   INITIALIZE_IMAGE_INSTANTIATOR_FORMAT()
		   IIFORMAT_HAS_METHOD()
		   IIFORMAT_VALID_KEYWORD()

		   For any given image instantiator format, the first macro must be
		   called before the any calls to the other macros. */

		image_instantiator_format_create();
		image_instantiator_format_create_glyphs_eimage();
		image_instantiator_format_create_glyphs_widget();
#ifdef HAVE_TTY
		image_instantiator_format_create_glyphs_tty();
#endif
#ifdef HAVE_X_WINDOWS
		image_instantiator_format_create_glyphs_x();
#endif				/* HAVE_X_WINDOWS */

		/* Now initialize the lstream types and associated symbols.
		   Other than the first function below, the functions may
		   make exactly the following function/macro calls:

		   LSTREAM_HAS_METHOD()

		 */

		lstream_type_create();
#ifdef FILE_CODING
		lstream_type_create_file_coding();
#endif

		/* Initialize processes implementation.
		   The functions may make exactly the following function/macro calls:

		   PROCESS_HAS_METHOD()
		 */
#ifdef HAVE_UNIX_PROCESSES
		process_type_create_unix();
#endif

		/* Now initialize most variables.

		   These functions may do exactly the following:

		   DEFVAR_INT()
		   DEFVAR_LISP()
		   DEFVAR_BOOL()
		   DEFER_GETTEXT()
		   Dynarr_*()
		   Blocktype_*()
		   staticpro()
		   Fprovide(symbol)
		   intern()
		   Fput()
		   xmalloc()
		   defsymbol(), if it's absolutely necessary and you're sure that
		   the symbol isn't referenced anywhere else in the initialization
		   code
		   Fset() on a symbol that is unbound
		   assigning a symbol or constant value to a variable
		   using a global variable that has been initialized
		   earlier on in the same function

		   Any of the object-creating functions in alloc.c: e.g.

		   make_pure_*()
		   make_string()
		   build_string()
		   make_vector()
		   make_int()
		   make_extent()
		   alloc_lcrecord()
		   Fcons()
		   listN()
		   make_opaque_ptr()

		   perhaps a few others.

		   NB:  Initialization or assignment should not be done here to certain
		   variables settable from the command line.  See the comment above
		   the call to pdump_load() in main_1().  This caveat should only
		   apply to vars_of_emacs().
		 */

		/* Now allow Fprovide() statements to be made. */
		init_provide_once();

		/* Do that before any specifier creation (esp. vars_of_glyphs()) */
		vars_of_specifier();

		vars_of_abbrev();
		vars_of_alloc();
		vars_of_buffer();
		vars_of_bytecode();
		vars_of_callint();
		vars_of_callproc();
		vars_of_chartab();
		vars_of_cmdloop();
		vars_of_cmds();
		vars_of_console();
		vars_of_data();
#ifdef DEBUG_SXEMACS
		vars_of_debug();
		vars_of_tests();
#endif
		vars_of_console_stream();
		vars_of_device();
#ifdef HAVE_DIALOGS
		vars_of_dialog();
#endif
		vars_of_dired();
		vars_of_doc();
#ifdef HAVE_DRAGNDROP
		vars_of_dragdrop();
#endif
		vars_of_editfns();
		vars_of_elhash();
		vars_of_emacs();
		vars_of_eval();

#ifdef HAVE_X_WINDOWS
		vars_of_event_Xt();
#endif
#if defined(HAVE_TTY) && (defined (DEBUG_TTY_EVENT_STREAM) || !defined (HAVE_X_WINDOWS))
		vars_of_event_tty();
#endif
		vars_of_event_stream();
#ifdef EF_USE_ASYNEQ
		vars_of_workers();
		vars_of_worker_asyneq();
#endif

		vars_of_events();
		vars_of_extents();
		vars_of_faces();
		vars_of_fileio();
#ifdef CLASH_DETECTION
		vars_of_filelock();
#endif
		vars_of_floatfns();
		vars_of_font_lock();
		vars_of_frame();
		vars_of_glyphs();
		vars_of_glyphs_eimage();
		vars_of_glyphs_widget();
		vars_of_gui();
		vars_of_gutter();
		vars_of_indent();
		vars_of_insdel();
		vars_of_intl();
#ifdef HAVE_XIM
#ifdef XIM_MOTIF
		vars_of_input_method_motif();
#else				/* XIM_XLIB */
		vars_of_input_method_xlib();
#endif
#endif				/* HAVE_XIM */
		vars_of_keymap();
		vars_of_lread();
		vars_of_lstream();
		vars_of_macros();
		vars_of_md5();
#ifdef HAVE_DATABASE
		vars_of_database();
#endif
#ifdef HAVE_MENUBARS
		vars_of_menubar();
#endif
		vars_of_media();
		vars_of_minibuf();
		vars_of_dynacat();
#if defined WITH_EMODULES && defined HAVE_EMODULES && 0
		vars_of_module();
#elif defined WITH_EMODULES && defined HAVE_EMODULES
		vars_of_emodng();
#endif
		vars_of_objects();
		vars_of_print();

#ifndef NO_SUBPROCESSES
		vars_of_process();
#ifdef HAVE_UNIX_PROCESSES
		vars_of_process_unix();
#endif
#endif

		vars_of_profile();
#if defined (HAVE_MMAP) && defined (REL_ALLOC) && !defined(DOUG_LEA_MALLOC)
		vars_of_ralloc();
#endif				/* HAVE_MMAP && REL_ALLOC */
		vars_of_redisplay();
#ifdef HAVE_SCROLLBARS
		vars_of_scrollbar();
#endif
		vars_of_search();
		vars_of_select();
		vars_of_sound();
		vars_of_symbols();
		vars_of_syntax();
#ifdef HAVE_TOOLBARS
		vars_of_toolbar();
#endif
		vars_of_undo();
		vars_of_window();

#ifdef HAVE_TTY
		vars_of_console_tty();
		vars_of_frame_tty();
		vars_of_objects_tty();
#endif

#ifdef HAVE_X_WINDOWS
#ifdef HAVE_BALLOON_HELP
		vars_of_balloon_x();
#endif
		vars_of_device_x();
#ifdef HAVE_DIALOGS
		vars_of_dialog_x();
#endif
		vars_of_frame_x();
		vars_of_glyphs_x();
#ifdef HAVE_MENUBARS
		vars_of_menubar_x();
#endif
		vars_of_objects_x();
		vars_of_select_x();
#ifdef HAVE_SCROLLBARS
		vars_of_scrollbar_x();
#endif
#if defined (HAVE_MENUBARS) || defined (HAVE_SCROLLBARS) || defined (HAVE_DIALOGS) || defined (HAVE_TOOLBARS)
		vars_of_gui_x();
#endif
#endif				/* HAVE_X_WINDOWS */

#ifdef MULE
		vars_of_mule();
		vars_of_mule_ccl();
		vars_of_mule_charset();
#endif
#ifdef FILE_CODING
		vars_of_file_coding();
#endif
#ifdef MULE
#ifdef HAVE_WNN
		vars_of_mule_wnn();
#endif
#ifdef HAVE_CANNA
		vars_of_mule_canna();
#endif				/* HAVE_CANNA */
#endif				/* MULE */

#ifdef SUNPRO
		vars_of_sunpro();
#endif

#ifdef HAVE_LDAP
		vars_of_eldap();
#endif

#ifdef HAVE_POSTGRESQL
		vars_of_postgresql();
#endif

#ifdef HAVE_OPENSSL
		vars_of_openssl();
#endif

#ifdef HAVE_GPM
		vars_of_gpmevent();
#endif

#ifdef WITH_NUMBER_TYPES
		vars_of_ent();
#endif

#ifdef HAVE_LIBFFI
                vars_of_ffi();
#endif

		vars_of_dllist();
		vars_of_skiplist();
		vars_of_bloom();

		/* Now initialize any specifier variables.  We do this later
		   because it has some dependence on the vars initialized
		   above.

		   These functions should *only* initialize specifier variables,
		   and may make use of the following functions/macros in addition
		   to the ones listed above:

		   DEFVAR_SPECIFIER()
		   Fmake_specifier()
		   set_specifier_fallback()
		   set_specifier_caching()
		 */

		specifier_vars_of_glyphs();
		specifier_vars_of_glyphs_widget();
		specifier_vars_of_gutter();
#ifdef HAVE_MENUBARS
		specifier_vars_of_menubar();
#endif
		specifier_vars_of_redisplay();
#ifdef HAVE_SCROLLBARS
		specifier_vars_of_scrollbar();
#endif
#ifdef HAVE_TOOLBARS
		specifier_vars_of_toolbar();
#endif
		specifier_vars_of_window();

		/* Now comes all the rest of the variables that couldn't
		   be handled above.  There may be dependencies on variables
		   initialized above, and dependencies between one complex_vars_()
		   function and another. */

		/* Calls Fmake_range_table(). */
		complex_vars_of_regex();
		/* Calls Fmake_range_table(). */
		complex_vars_of_search();

		/* Calls make_lisp_hash_table(). */
		complex_vars_of_extents();

		/* Depends on hash tables and specifiers. */
		complex_vars_of_faces();

#ifdef MULE
		/* These two depend on hash tables and various variables declared
		   earlier.  The second may also depend on the first. */
		complex_vars_of_mule_charset();
#endif
#ifdef FILE_CODING
		complex_vars_of_file_coding();
#endif

		/* This calls allocate_glyph(), which creates specifiers
		   and also relies on a variable (Vthe_nothing_vector) initialized
		   above.  It also calls make_ext_string(), which under Mule
		   could require that the charsets be initialized. */
		complex_vars_of_glyphs();

		/* These rely on the glyphs just created in the previous function,
		   and call Fadd_spec_to_specifier(), which relies on various
		   variables initialized above. */
#ifdef HAVE_X_WINDOWS
		complex_vars_of_glyphs_x();
#endif

		/* This calls Fmake_glyph_internal(). */
		complex_vars_of_alloc();

		/* This calls Fmake_glyph_internal(). */
#ifdef HAVE_MENUBARS
		complex_vars_of_menubar();
#endif

		/* This calls Fmake_glyph_internal(). */
#ifdef HAVE_SCROLLBARS
		complex_vars_of_scrollbar();
#endif

		/* This calls allocate_glyph(). */
		complex_vars_of_frame();

		/* This calls Fcopy_category_table() under Mule, which calls who
		   knows what. */
		complex_vars_of_chartab();

		/* This calls set_string_char(), which (under Mule) depends on the
		   charsets being initialized. */
		complex_vars_of_casetab();

		/* This calls Fcopy_syntax_table(), which relies on char tables. */
		complex_vars_of_syntax();

		/* This initializes buffer-local variables, sets things up so
		   that buffers can be created, and creates a couple of basic
		   buffers.  This depends on Vstandard_syntax_table and
		   Vstandard_category_table (initialized in the previous
		   functions), as well as a whole horde of variables that may
		   have been initialized above. */
		complex_vars_of_buffer();

		/* This initializes console-local variables. */
		complex_vars_of_console();

		/* This creates a couple more buffers, and depends on the
		   previous function. */
		complex_vars_of_minibuf();

		/* These two might call Ffile_name_as_directory(), which
		   might depend on all sorts of things; I'm not sure. */
		complex_vars_of_emacs();

		/* This creates a couple of basic keymaps and depends on Lisp
		   hash tables and Ffset() (both of which depend on some variables
		   initialized in the vars_of_*() section) and possibly other
		   stuff. */
		complex_vars_of_keymap();

		/* Calls make_lisp_hash_table() and creates a keymap */
		complex_vars_of_event_stream();

#ifdef ERROR_CHECK_GC
		{
			extern int always_gc;
			if (always_gc)	/* purification debugging hack */
				garbage_collect_1();
		}
#endif
#ifdef PDUMP
	} else if (!restart) {	/* after successful pdump_load() */
		reinit_alloc_once_early();
		reinit_symbols_once_early();
		reinit_opaque_once_early();

		reinit_console_type_create_stream();
#ifdef HAVE_TTY
		reinit_console_type_create_tty();
#endif
#ifdef HAVE_X_WINDOWS
		reinit_console_type_create_x();
		reinit_console_type_create_device_x();
#endif

		reinit_specifier_type_create();
		reinit_specifier_type_create_image();
		reinit_specifier_type_create_gutter();
		reinit_specifier_type_create_objects();
#ifdef HAVE_TOOLBARS
		reinit_specifier_type_create_toolbar();
#endif

		structure_type_create();

		structure_type_create_chartab();
		structure_type_create_faces();
		structure_type_create_rangetab();
		structure_type_create_hash_table();

		lstream_type_create();
#ifdef FILE_CODING
		lstream_type_create_file_coding();
#endif
#ifdef HAVE_UNIX_PROCESSES
		process_type_create_unix();
#endif

		reinit_vars_of_buffer();
		reinit_vars_of_console();
#ifdef DEBUG_SXEMACS
		reinit_vars_of_debug();
#endif
		reinit_vars_of_device();
		reinit_vars_of_eval();
#ifdef HAVE_X_WINDOWS
		reinit_vars_of_event_Xt();
#endif
#if defined(HAVE_TTY) && (defined (DEBUG_TTY_EVENT_STREAM) || !defined (HAVE_X_WINDOWS))
		reinit_vars_of_event_tty();
#endif
		reinit_vars_of_event_stream();
#ifdef EF_USE_ASYNEQ
		reinit_vars_of_worker_asyneq();
#endif
		reinit_vars_of_events();
		reinit_vars_of_extents();
		reinit_vars_of_fileio();
		reinit_vars_of_font_lock();
		reinit_vars_of_glyphs();
		reinit_vars_of_glyphs_widget();
		reinit_vars_of_insdel();
		reinit_vars_of_lread();
		reinit_vars_of_lstream();
		reinit_vars_of_minibuf();
		reinit_vars_of_dynacat();
#if defined WITH_EMODULES && defined HAVE_EMODULES && 0
		reinit_vars_of_module();
#elif defined WITH_EMODULES && defined HAVE_EMODULES
		reinit_vars_of_emodng();
#endif
		reinit_vars_of_objects();
		reinit_vars_of_print();
		reinit_vars_of_search();
		reinit_vars_of_undo();
		reinit_vars_of_window();

#ifdef HAVE_X_WINDOWS
		reinit_vars_of_device_x();
#ifdef HAVE_SCROLLBARS
		reinit_vars_of_scrollbar_x();
#endif
#ifdef HAVE_MENUBARS
		reinit_vars_of_menubar_x();
#endif
		reinit_vars_of_select_x();
#if defined (HAVE_MENUBARS) || defined (HAVE_SCROLLBARS) || defined (HAVE_DIALOGS) || defined (HAVE_TOOLBARS)
		reinit_vars_of_gui_x();
#endif
#endif				/* HAVE_X_WINDOWS */

#if defined(MULE) && defined(HAVE_WNN)
		reinit_vars_of_mule_wnn();
#endif

		reinit_complex_vars_of_buffer();
		reinit_complex_vars_of_console();
		reinit_complex_vars_of_minibuf();

#ifdef HAVE_LIBFFI
                reinit_vars_of_ffi();
#endif

#if defined USE_STATIC_ASE && USE_STATIC_ASE
		reinit_vars_of_ase();
#endif

#endif				/* PDUMP */
	}

	/* the category subsystem needs some inits */
	dllist_reinit();
	elhash_reinit();
	skiplist_reinit();

	/* CONGRATULATIONS!!!  We have successfully initialized the Lisp
	   engine. */

	if (initialized) {
		/* Stuff that should not be done at dump time, including stuff that
		   needs to be reset at run time.  Order below should not matter.

		   Many initializations taken from the environment should go here. */
		reinit_alloc();
		reinit_eval();
#ifdef MULE_REGEXP
		reinit_mule_category();
#endif
#ifdef HAVE_POSTGRESQL
		init_postgresql_from_environment();
#endif
	}

	/* Now do further initialization/setup of stuff that is not needed by the
	   syms_of_() routines.  This involves stuff that only is enabled in
	   an interactive run (redisplay, user input, etc.) and stuff that is
	   not needed until we start loading Lisp code (the reader).  A lot
	   of this stuff involves querying the current environment and needs
	   to be done both at dump time and at run time. */

	init_initial_directory();	/* get the directory to use for the
					   "*scratch*" buffer, etc. */

	init_callproc();	/* Set up the process environment (so that egetenv
				   works), the basic directory variables
				   (exec-directory and so on), and stuff
				   related to subprocesses.  This should be
				   first because many of the functions below
				   call egetenv() to get environment variables. */
	init_lread();		/* Set up the Lisp reader. */
	init_cmdargs(argc, (Extbyte **) argv, skip_args);	/* Create list Vcommand_line_args */
	init_buffer();		/* Set default directory of *scratch* buffer */

	init_redisplay();	/* Determine terminal type.
				   init_sys_modes uses results */
	init_frame();
	init_event_stream();	/* Set up so we can get user input. */
	init_macros();		/* set up so we can run macros. */
	init_editfns();		/* Determine the name of the user we're running as */
	init_sxemacs_process();	/* set up for calling subprocesses */

#ifdef WITH_NUMBER_TYPES
	/* Set up bignums, ratios, bigfloats, complex numbers.
	 * This must be done before the Lisp reader is set up.
	 */
	init_ent ();
#endif

#ifdef SUNPRO
	init_sunpro();		/* Set up Sunpro usage tracking */
#endif
#if defined (HAVE_NATIVE_SOUND) && defined (hp9000s800) && 0
	init_hpplay();
#endif
#ifdef HAVE_TTY
	init_device_tty();
#endif
	init_console_stream(restart);	/* Create the first console */

	/* try to get the actual pathname of the exec file we are running */
	if (!restart) {
		Vinvocation_name = Fcar(Vcommand_line_args);
		if (XSTRING_DATA(Vinvocation_name)[0] == '-') {
			/* SXEmacs as a login shell, oh goody! */
			Vinvocation_name = build_string(getenv("SHELL"));
		}
		Vinvocation_directory = Vinvocation_name;

		if (!NILP(Ffile_name_directory(Vinvocation_name))) {
			/* invocation-name includes a directory component -- presumably it
			   is relative to cwd, not $PATH */
			Vinvocation_directory =
			    Fexpand_file_name(Vinvocation_name, Qnil);
			Vinvocation_path = Qnil;
		} else {
			Vinvocation_path = decode_env_path("PATH", NULL);
			locate_file(Vinvocation_path, Vinvocation_name,
				    Vlisp_EXEC_SUFFIXES,
				    &Vinvocation_directory, X_OK);
		}

		if (NILP(Vinvocation_directory))
			Vinvocation_directory = Vinvocation_name;

		Vinvocation_name =
		    Ffile_name_nondirectory(Vinvocation_directory);
		Vinvocation_directory =
		    Ffile_name_directory(Vinvocation_directory);
	}

#if defined (LOCALTIME_CACHE) && defined (HAVE_TZSET)
	/* sun's localtime() has a bug.  it caches the value of the time
	   zone rather than looking it up every time.  Since localtime() is
	   called to bolt the undumping time into the undumped emacs, this
	   results in localtime() ignoring the TZ environment variable.
	   This flushes the new TZ value into localtime(). */
	tzset();
#endif				/* LOCALTIME_CACHE and TZSET */

	load_me = Qnil;
	if (!initialized) {
		/* Handle -l loadup-and-dump, args passed by Makefile. */
		if (argc > 2 + skip_args && !strcmp(argv[1 + skip_args], "-l"))
			load_me = build_string(argv[2 + skip_args]);
#if 0
		/* CANNOT_DUMP - this can never be right in SXEmacs --andyp */
		/* Unless next switch is -nl, load "loadup.el" first thing.  */
		if (!
		    (argc > 1 + skip_args
		     && !strcmp(argv[1 + skip_args], "-nl")))
			load_me = build_string("loadup.el");
#endif				/* CANNOT_DUMP */
	}
#ifdef QUANTIFY
	if (initialized)
		quantify_start_recording_data();
#endif				/* QUANTIFY */

	initialized = 1;
	inhibit_non_essential_printing_operations = 0;

	/* This never returns.  */
	initial_command_loop(load_me);
	/* NOTREACHED */
}

/* Sort the args so we can find the most important ones
   at the beginning of argv.  */

/* First, here's a table of all the standard options.  */

struct standard_args {
	const char *name;
	const char *longname;
	int priority;
	int nargs;
};

static const struct standard_args standard_args[] = {
	/* Handled by main_1 above: */
	{"--make-docfile", 0, 105, 0},
	{"-sd", "--show-dump-id", 105, 0},
	{"-t", "--terminal", 100, 1},
	{"-nd", "--no-dump-file", 95, 0},
	{"-ct", "--color-terminal", 92, 0},
	{"-nw", "--no-windows", 90, 0},
	{"-batch", "--batch", 85, 0},
	{"-debug-paths", "--debug-paths", 82, 0},
	{"-help", "--help", 80, 0},
	{"-version", "--version", 75, 0},
	{"-V", 0, 75, 0},
	{"-d", "--display", 80, 1},
	{"-display", 0, 80, 1},
	{"-NXHost", 0, 79, 0},
	{"-MachLaunch", 0, 79, 0},

	/* Handled by command-line-early in startup.el: */
	{"-q", "--no-init-file", 50, 0},
	{"-unmapped", 0, 50, 0},
	{"-no-init-file", 0, 50, 0},
	{"-vanilla", "--vanilla", 50, 0},
	{"-no-autoloads", "--no-autoloads", 50, 0},
	{"-no-site-file", "--no-site-file", 40, 0},
	{"-no-early-packages", "--no-early-packages", 35, 0},
	{"-u", "--user", 30, 1},
	{"-user", 0, 30, 1},
	{"-debug-init", "--debug-init", 20, 0},
	{"-debug-paths", "--debug-paths", 20, 0},

	/* Xt options: */
	{"-i", "--icon-type", 15, 0},
	{"-itype", 0, 15, 0},
	{"-iconic", "--iconic", 15, 0},
	{"-bg", "--background-color", 10, 1},
	{"-background", 0, 10, 1},
	{"-fg", "--foreground-color", 10, 1},
	{"-foreground", 0, 10, 1},
	{"-bd", "--border-color", 10, 1},
	{"-bw", "--border-width", 10, 1},
	{"-ib", "--internal-border", 10, 1},
	{"-ms", "--mouse-color", 10, 1},
	{"-cr", "--cursor-color", 10, 1},
	{"-fn", "--font", 10, 1},
	{"-font", 0, 10, 1},
	{"-g", "--geometry", 10, 1},
	{"-geometry", 0, 10, 1},
	{"-T", "--title", 10, 1},
	{"-title", 0, 10, 1},
	{"-name", "--name", 10, 1},
	{"-xrm", "--xrm", 10, 1},
	{"-r", "--reverse-video", 5, 0},
	{"-rv", 0, 5, 0},
	{"-reverse", 0, 5, 0},
	{"-hb", "--horizontal-scroll-bars", 5, 0},
	{"-vb", "--vertical-scroll-bars", 5, 0},

	/* These have the same priority as ordinary file name args,
	   so they are not reordered with respect to those.  */
	{"-L", "--directory", 0, 1},
	{"-directory", 0, 0, 1},
	{"-l", "--load", 0, 1},
	{"-load", 0, 0, 1},
	{"-f", "--funcall", 0, 1},
	{"-funcall", 0, 0, 1},
	{"-eval", "--eval", 0, 1},
	{"-insert", "--insert", 0, 1},
	/* This should be processed after ordinary file name args and the like.  */
	{"-kill", "--kill", -10, 0},
};

/* Reorder the elements of ARGV (assumed to have ARGC elements)
   so that the highest priority ones come first.
   Do not change the order of elements of equal priority.
   If an option takes an argument, keep it and its argument together.  */

static void sort_args(int argc, char **argv)
{
	char **new_argv = (char**)malloc(sizeof(char*) * argc);
	/* For each element of argv,
	   the corresponding element of options is:
	   0 for an option that takes no arguments,
	   1 for an option that takes one argument, etc.
	   -1 for an ordinary non-option argument.  */
	int *options = (int*)malloc(sizeof(int) * argc);
	int *priority = (int*)malloc(sizeof(int) * argc);
	int to = 1;
	int from;
	int i;
	int end_of_options_p = 0;

	/* Categorize all the options,
	   and figure out which argv elts are option arguments.  */
	for (from = 1; from < argc; from++) {
		options[from] = -1;
		priority[from] = 0;
		/* Pseudo options "--" and "run-temacs" indicate end of
		   options */
		if (!strcmp(argv[from], "--") ||
		    !strcmp(argv[from], "run-temacs"))
			end_of_options_p = 1;
		if (!end_of_options_p && argv[from][0] == '-') {
			int match, thislen;
			char *equals;

			/* Look for a match with a known old-fashioned
			   option.  */
			for (i = 0; i < countof(standard_args); i++) {
				if (!strcmp(argv[from],
					    standard_args[i].name)) {
					options[from] = standard_args[i].nargs;
					priority[from] =
					    standard_args[i].priority;
					if (from + standard_args[i].nargs >=
					    argc) {
						fatal("Option `%s' requires "
						      "an argument\n",
						      argv[from]);
					}
					from += standard_args[i].nargs;
					goto done;
				}
			}

			/* Look for a match with a known long option.  MATCH is
			   -1 if no match so far, -2 if two or more matches so
			   far,
			   >= 0 (the table index of the match) if just one match
			   >so far.  */
			if (argv[from][1] == '-') {
				match = -1;
				thislen = strlen(argv[from]);
				equals = strchr(argv[from], '=');
				if (equals != 0)
					thislen = equals - argv[from];

				for (i = 0; i < countof(standard_args); i++)
					if (standard_args[i].longname
					    && !strncmp(argv[from],
							standard_args[i].
							longname, thislen)) {
						if (match == -1)
							match = i;
						else
							match = -2;
					}

				/* If we found exactly one match, use that.  */
				if (match >= 0) {
					options[from] =
						standard_args[match].nargs;
					priority[from] =
						standard_args[match].priority;
					/* If --OPTION=VALUE syntax is used,
					   this option uses just one argv
					   element.  */
					if (equals != 0)
						options[from] = 0;
					if (from + options[from] >= argc) {
						fatal("Option `%s' requires "
						      "an argument\n",
						      argv[from]);
					}
					from += options[from];
				}
			}
		done:
			;
		}
	}

	/* Copy the arguments, in order of decreasing priority, to NEW_ARGV.  */
	new_argv[0] = argv[0];
	while (to < argc) {
		int best = -1;
		int best_priority = -9999;

		/* Find the highest priority remaining option.
		   If several have equal priority, take the first of them.  */
		for (from = 1; from < argc; from++) {
			if (argv[from] != 0 && priority[from] > best_priority) {
				best_priority = priority[from];
				best = from;
			}
			/* Skip option arguments--they are tied to the options.  */
			if (options[from] > 0) {
				from += options[from];
			}
		}

		if (best < 0) {
			abort();
		}

		/* Copy the highest priority remaining option, with its args, to
		   NEW_ARGV.  */
		new_argv[to++] = argv[best];
		for (i = 0; i < options[best]; i++) {
			new_argv[to++] = argv[best + i + 1];
		}
		/* Clear out this option in ARGV.  */
		argv[best] = 0;
		for (i = 0; i < options[best]; i++) {
			argv[best + i + 1] = 0;
		}
	}

	memcpy(argv, new_argv, sizeof(char *) * argc);
	free(new_argv);
	free(options);
	free(priority);
	return;
}

DEFUN("running-temacs-p", Frunning_temacs_p, 0, 0, 0, /*
True if running temacs.  This means we are in the dumping stage.
This is false during normal execution of the `sxemacs' program, and
becomes false once `run-emacs-from-temacs' is run.
*/
      ())
{
	return run_temacs_argc >= 0 ? Qt : Qnil;
}

DEFUN("run-emacs-from-temacs", Frun_emacs_from_temacs, 0, MANY, 0, /*
Do not call this.  It will reinitialize your SXEmacs.  You'll be sorry.
*/
/* If this function is called from startup.el, it will be possible to run
   temacs as an editor using 'temacs -batch -l loadup.el run-temacs', instead
   of having to dump an emacs and then run that (when debugging emacs itself,
   this can be much faster)). [Actually, the speed difference isn't that
   much as long as your filesystem is local, and you don't end up with
   a dumped version in case you want to rerun it.  This function is most
   useful when used as part of the `make all-elc' command. --ben]
   This will "restart" emacs with the specified command-line arguments.

   Martin thinks this function is most useful when using debugging
   tools like Purify or tcov that get confused by SXEmacs' dumping.  */
      (int nargs, Lisp_Object * args))
{
	int ac;
	const Extbyte *wampum = NULL;
	int namesize;
	int total_len;
	Lisp_Object orig_invoc_name = Fcar(Vcommand_line_args);
	const Extbyte **wampum_all = alloca_array(const Extbyte *, nargs);
	int *wampum_all_len = alloca_array(int, nargs);

	assert(!gc_in_progress);

	if (run_temacs_argc < 0)
		error("I've lost my temacs-hood.");

	/* Need to convert the orig_invoc_name and all of the arguments
	   to external format. */

	TO_EXTERNAL_FORMAT(LISP_STRING, orig_invoc_name,
			   ALLOCA, (wampum, namesize), Qnative);
	if ( wampum == NULL )
		error("Could not transcode invocation name");

	namesize++;

	for (ac = 0, total_len = namesize; ac < nargs; ac++) {
		CHECK_STRING(args[ac]);
		wampum_all[ac]=NULL;
		TO_EXTERNAL_FORMAT(LISP_STRING, args[ac],
				   ALLOCA, (wampum_all[ac], wampum_all_len[ac]),
				   Qnative);
		if(wampum_all[ac]==NULL) {
			error("Could not transcode arguments");
		}
		wampum_all_len[ac]++;
		total_len += wampum_all_len[ac];
	}
	DO_REALLOC(run_temacs_args, run_temacs_args_size, total_len, char);
	DO_REALLOC(run_temacs_argv, run_temacs_argv_size, nargs + 2, char *);

	memcpy(run_temacs_args, wampum, namesize);
	run_temacs_argv[0] = run_temacs_args;
	for (ac = 0; ac < nargs; ac++) {
		memcpy(run_temacs_args + namesize,
		       wampum_all[ac], wampum_all_len[ac]);
		run_temacs_argv[ac + 1] = run_temacs_args + namesize;
		namesize += wampum_all_len[ac];
	}
	run_temacs_argv[nargs + 1] = 0;
	catchlist = NULL;	/* Important!  Otherwise free_cons() calls in
				   condition_case_unwind() may lead to GC death. */
	unbind_to(0, Qnil);	/* this closes loadup.el */
	purify_flag = 0;
	run_temacs_argc = nargs + 1;
#if defined (HEAP_IN_DATA) && !defined(PDUMP)
	report_sheap_usage(0);
#endif
	LONGJMP(run_temacs_catch, 1);
	return Qnil;		/* not reached; warning suppression */
}

/* defined in alloc.c */
extern void init_bdwgc(void);

/* ARGSUSED */
int
main(int argc, char **argv, char **envp)
{
        int volatile vol_argc = argc;
        char **volatile vol_argv = argv;
        char **volatile vol_envp = envp;
        /* This is hairy.  We need to compute where the SXEmacs binary was invoked
           from because temacs initialization requires it to find the lisp
           directories.  The code that recomputes the path is guarded by the
           restarted flag.  There are three possible paths I've found so far
           through this:

           temacs -- When running temacs for basic build stuff, the first main_1
           will be the only one invoked.  It must compute the path else there
           will be a very ugly bomb in startup.el (can't find obvious location
           for doc-directory data-directory, etc.).

           temacs w/ run-temacs on the command line -- This is run to bytecompile
           all the out of date dumped lisp.  It will execute both of the main_1
           calls and the second one must not touch the first computation because
           argc/argv are hosed the second time through.

           sxemacs -- Only the second main_1 is executed.  The invocation path must
           computed but this only matters when running in place or when running
           as a login shell.

           As a bonus for straightening this out, SXEmacs can now be run in place
           as a login shell.  This never used to work.

           As another bonus, we can now guarantee that
           (concat invocation-directory invocation-name) contains the filename
           of the SXEmacs binary we are running.  This can now be used in a
           definite test for out of date dumped files.  -slb */
        int restarted = 0;
#ifdef QUANTIFY
        quantify_stop_recording_data();
        quantify_clear_data();
#endif /* QUANTIFY */

        inhibit_non_essential_printing_operations = 1;
        suppress_early_error_handler_backtrace = 0;
        lim_data = 0;	/* force reinitialization of this variable */

        /* Lisp_Object must fit in a word; check VALBITS and GCTYPEBITS */
        assert(sizeof(Lisp_Object) == sizeof(void *));

#ifdef LINUX_SBRK_BUG
        sbrk(1);
#endif

	/* defined in alloc.c */
	init_bdwgc();

        if (!initialized) {
#ifdef DOUG_LEA_MALLOC
                if (mallopt(M_MMAP_MAX, 0) != 1)
                        abort();
#endif
                run_temacs_argc = 0;
                if (!SETJMP(run_temacs_catch)) {
                        main_1(vol_argc, vol_argv, vol_envp, 0);
                }
                /* run-emacs-from-temacs called */
                restarted = 1;
                vol_argc = run_temacs_argc;
                vol_argv = run_temacs_argv;
#ifdef _SCO_DS
                /* This makes absolutely no sense to anyone involved.  There are
                   several people using this stuff.  We've compared versions on
                   everything we can think of.  We can find no difference.
                   However, on both my systems environ is a plain old global
                   variable initialized to zero.  _environ is the one that
                   contains pointers to the actual environment.

                   Since we can't figure out the difference (and we're hours
                   away from a release), this takes a very cowardly approach and
                   is bracketed with both a system specific preprocessor test
                   and a runtime "do you have this problem" test

                   06/20/96 robertl@dgii.com */
                {
                        extern char **_environ;
                        if ((unsigned)environ == 0)
                                environ = _environ;
                }
#endif				/* _SCO_DS */
                vol_envp = environ;
        }
#if defined (RUN_TIME_REMAP) && ! defined (PDUMP)
        else
                /* obviously no-one uses this because where it was before initialized was
                 *always* true */
                run_time_remap(argv[0]);
#endif

#ifdef DOUG_LEA_MALLOC
        if (initialized && (malloc_state_ptr != NULL)) {
                int rc = malloc_set_state(malloc_state_ptr);
                if (rc != 0) {
                        stderr_out("malloc_set_state failed, rc = %d\n",
                                   rc);
                        abort();
                }
#if 0
                free(malloc_state_ptr);
#endif
		/* mmap works in glibc-2.1, glibc-2.0 (Non-Mule only)
		 * and Linux libc5 */
#if (defined(__GLIBC__) && __GLIBC_MINOR__ >= 1) ||                     \
        defined(_NO_MALLOC_WARNING_) ||                                 \
        (defined(__GLIBC__) && __GLIBC_MINOR__ < 1 && !defined(MULE)) || \
        defined(DEBUG_DOUG_LEA_MALLOC)
                if (mallopt(M_MMAP_MAX, 0) != 1)
                        abort();
#endif
#ifdef REL_ALLOC
                r_alloc_reinit();
#endif
        }
 #endif				/* DOUG_LEA_MALLOC */

        run_temacs_argc = -1;

        main_1(vol_argc, vol_argv, vol_envp, restarted);

	return 0;		/* unreached */
}


/* Dumping apparently isn't supported by versions of GCC >= 2.8. */
/* The following needs conditionalization on whether either SXEmacs or */
/* various system shared libraries have been built and linked with */
/* GCC >= 2.8.  -slb */
#if defined(GNU_MALLOC)
static void voodoo_free_hook(void *mem)
{
	/* Disable all calls to free() when SXEmacs is exiting and it doesn't */
	/* matter. */
	__free_hook =
#if defined __GNUC__ || defined __INTEL_COMPILER
/* prototype of __free_hook varies with glibc version */
	    (__typeof__(__free_hook))
#endif
	    voodoo_free_hook;
}
#endif				/* GNU_MALLOC */

DEFUN("kill-emacs", Fkill_emacs, 0, 1, "P", /*
Exit the SXEmacs job and kill it.  Ask for confirmation, without argument.
If ARG is an integer, return ARG as the exit program code.
If ARG is a string, stuff it as keyboard input.

The value of `kill-emacs-hook', if not void,
is a list of functions (of no args),
all of which are called before SXEmacs is actually killed.
*/
      (arg))
{
	/* This function can GC */
	struct gcpro gcpro1;

	GCPRO1(arg);

	if (feof(stdin))
		arg = Qt;

	if (!preparing_for_armageddon && !noninteractive)
		run_hook(Qkill_emacs_hook);

	ensure_no_quitting_from_now_on();

	if (!preparing_for_armageddon) {
		Lisp_Object concons, nextcons;

		/* Normally, go ahead and delete all the consoles now.
		   Some unmentionably lame window systems (MS Wwwww...... eek,
		   I can't even say it) don't properly clean up after themselves,
		   and even for those that do, it might be cleaner this way.
		   If we're going down, however, we don't do this (might
		   be too dangerous), and if we get a crash somewhere within
		   this loop, we'll still autosave and won't try this again. */

		LIST_LOOP_DELETING(concons, nextcons, Vconsole_list) {
			/* There is very little point in deleting the stream console.
			   It uses stdio, which should flush any buffered output and
			   something can only go wrong. -slb */
			/* I changed my mind.  There's a stupid hack in close to add
			   a trailing newline. */
			/*if (!CONSOLE_STREAM_P (XCONSOLE (XCAR (concons)))) */
			delete_console_internal(XCONSOLE(XCAR(concons)), 1, 1,
						0);
		}
	}

	UNGCPRO;

	shut_down_emacs(0, STRINGP(arg) ? arg : Qnil, 0);

#if defined(GNU_MALLOC)
	__free_hook =
#if defined __GNUC__ || defined __INTEL_COMPILER
/* prototype of __free_hook varies with glibc version */
	    (__typeof__(__free_hook))
#endif
	    voodoo_free_hook;
#endif

	exit(INTP(arg) ? XINT(arg) : 0);
	/* NOTREACHED */
	return Qnil;		/* I'm sick of the compiler warning */
}

/* Perform an orderly shutdown of SXEmacs.  Autosave any modified
   buffers, kill any child processes, clean up the terminal modes (if
   we're in the foreground), and other stuff like that.  Don't perform
   any redisplay; this may be called when SXEmacs is shutting down in
   the background, or after its X connection has died.

   If SIG is a signal number, print a message for it.

   This is called by fatal signal handlers and Fkill_emacs.  It used to
   be called by X protocol error handlers, but instead they now call
   Fkill_emacs. */
static void shut_down_emacs(int sig, Lisp_Object stuff, int no_auto_save)
{
	/* This function can GC */
	/* Prevent running of hooks and other non-essential stuff
	   from now on.  */
	preparing_for_armageddon = 1;

	ensure_no_quitting_from_now_on();

#ifdef QUANTIFY
	quantify_stop_recording_data();
#endif				/* QUANTIFY */

	/* This is absolutely the most important thing to do, so make sure
	   we do it now, before anything else.  We might have crashed and
	   be in a weird inconsistent state, and potentially anything could
	   set off another protection fault and cause us to bail out
	   immediately. */
	/* Steve writes the following:

	   [[I'm not removing the code entirely, yet.  We have run up against
	   a spate of problems in diagnosing crashes due to crashes within
	   crashes.  It has very definitely been determined that code called
	   during auto-saving cannot work if SXEmacs crashed inside of GC.
	   We already auto-save on an itimer so there cannot be too much
	   unsaved stuff around, and if we get better crash reports we might
	   be able to get more problems fixed so I'm disabling this.  -slb]]

	   and DISABLES AUTO-SAVING ENTIRELY during crashes!  Way way bad idea.

	   Instead let's just be more intelligent about avoiding crashing
	   when possible, esp. nested crashes.
	 */
	if (!no_auto_save)
		Fdo_auto_save(Qt, Qnil);	/* do this before anything hazardous */

	fflush(stdout);
	reset_all_consoles();
	if (sig && sig != SIGTERM) {
		if (sig == -1)
			stderr_out("\nFatal error.\n\n");
		else
			stderr_out("\nFatal error (%d).\n\n", sig);
		stderr_out
		    ("Your files have been auto-saved.\n"
		     "Use `M-x recover-session' to recover them.\n"
		     "\n"
		     "Your version of SXEmacs was distributed with a PROBLEMS file that  may describe\n"
		     "your crash, and with luck a workaround.  Please check it first, but do report\n"
		     "the crash anyway.  "
#ifdef INFODOCK
		     "\n\nPlease report this bug by selecting `Report-Bug' in the InfoDock menu.\n"
		     "*BE SURE* to include the SXEmacs configuration from M-x describe-installation,\n"
		     "or the file Installation in the top directory of the build tree.\n"
#else
		     "Please report this bug by invoking M-x report-sxemacs-bug,\n"
		     "or by selecting `Send Bug Report' from the Help menu.  If necessary, send\n"
		     "ordinary email to `sxemacs-devel@sxemacs.org'.  *MAKE SURE* to include the SXEmacs\n"
		     "configuration from M-x describe-installation, or equivalently the file\n"
		     "Installation in the top of the build tree.\n"
#endif
#ifndef _MSC_VER
		     "\n"
		     "*Please* try *hard* to obtain a C stack backtrace; without it, we are unlikely\n"
		     "to be able to analyze the problem.  Locate the core file produced as a result\n"
		     "of this crash (often called `core' or `core.<process-id>', and located in\n"
		     "the directory in which you started SXEmacs or your home directory), and type\n"
		     "\n" "  gdb "
#endif
		    );
		{
			const char *name;
			char *dir = 0;

			/* Now try to determine the actual path to the executable,
			   to try to make the backtrace-determination process as foolproof
			   as possible. */
			if (STRINGP(Vinvocation_name))
				name = (char *)XSTRING_DATA(Vinvocation_name);
			else
				name = "sxemacs";
			if (STRINGP(Vinvocation_directory))
				dir =
				    (char *)XSTRING_DATA(Vinvocation_directory);
			if (!dir || dir[0] != '/')
				stderr_out("`which %s`", name);
			else if (dir[strlen(dir) - 1] != '/')
				stderr_out("%s/%s", dir, name);
			else
				stderr_out("%s%s", dir, name);
		}
		stderr_out
		    (" core\n"
		     "\n"
		     "then type `where' at the debugger prompt.  "
		     "No GDB on your system?  You may\n"
		     "have DBX, or XDB, or SDB.  (Ask your system "
		     "administrator if you need help.)\n"
		     "If no core file was produced, enable them "
		     "(often with `ulimit -c unlimited'\n"
		     "in case of future recurrance of the crash.\n");
	}

	stuff_buffered_input(stuff);

	kill_buffer_processes(Qnil);

#ifdef CLASH_DETECTION
	unlock_all_files();
#endif

}

#ifndef CANNOT_DUMP

#if !defined(PDUMP) || !defined(SYSTEM_MALLOC)
extern char my_edata[];
#endif

extern void disable_free_hook(void);

DEFUN("dump-emacs", Fdump_emacs, 2, 2, 0, /*
Dump current state of SXEmacs into executable file FILENAME.
Take symbols from SYMFILE (presumably the file you executed to run SXEmacs).
This is used in the file `loadup.el' when building SXEmacs.

Remember to set `command-line-processed' to nil before dumping
if you want the dumped SXEmacs to process its command line
and announce itself normally when it is run.
*/
      (symfile, filename))
{
	/* This function can GC */
	struct gcpro gcpro1, gcpro2;
	int opurify;

	GCPRO2(filename, symfile);

#if 0
	/* kick them */
	Vinvocation_directory = Vinvocation_name = Qnil;
	Vcommand_line_args = Qnil;
#endif

#ifdef FREE_CHECKING
	Freally_free(Qnil);

	/* When we're dumping, we can't use the debugging free() */
	disable_free_hook();
#endif

	CHECK_STRING(filename);
	filename = Fexpand_file_name(filename, Qnil);
	if (!NILP(symfile)) {
		CHECK_STRING(symfile);
		if (XSTRING_LENGTH(symfile) > 0) {
			symfile = Fexpand_file_name(symfile, Qnil);
		} else {
			symfile = Qnil;
		}
	}

	opurify = purify_flag;
	purify_flag = 0;

#if defined (HEAP_IN_DATA) && !defined(PDUMP)
	report_sheap_usage(1);
#endif

	clear_message();

	fflush(stderr);
	fflush(stdout);

	disksave_object_finalization();
	release_breathing_space();

	/* Tell malloc where start of impure now is */
	/* Also arrange for warnings when nearly out of space.  */
#ifndef SYSTEM_MALLOC
	memory_warnings(my_edata, malloc_warning);
#endif

	UNGCPRO;

	{
		char *filename_ext;
		char *symfile_ext;

		LISP_STRING_TO_EXTERNAL(filename, filename_ext, Qfile_name);

		if (STRINGP(symfile)) {
			LISP_STRING_TO_EXTERNAL(symfile, symfile_ext,
						Qfile_name);
		} else {
			symfile_ext = 0;
		}

		garbage_collect_1();

#ifdef PDUMP
		pdump(filename_ext);
#else

#ifdef DOUG_LEA_MALLOC
		malloc_state_ptr = malloc_get_state();
#endif
		/* here we break our rule that the filename conversion should
		   be performed at the actual time that the system call is made.
		   It's a whole lot easier to do the conversion here than to
		   modify all the unexec routines to ensure that filename
		   conversion is applied everywhere.  Don't worry about memory
		   leakage because this call only happens once. */
		unexec(filename_ext, symfile_ext, (uintptr_t) my_edata, 0, 0);
#ifdef DOUG_LEA_MALLOC
		free(malloc_state_ptr);
#endif
#endif				/* not PDUMP */
	}

	purify_flag = opurify;

	return Qnil;
}

#endif				/* not CANNOT_DUMP */

/* Split STRING into a list of substrings.  The substrings are the
   parts of original STRING separated by SEPCHAR.  */
static Lisp_Object
split_string_by_emchar_1(const Bufbyte * string, Bytecount size, Emchar sepchar)
{
	Lisp_Object result = Qnil;
	const Bufbyte *end = string + size;

	while (1) {
		const Bufbyte *p = string;
		while (p < end) {
			if (charptr_emchar(p) == sepchar)
				break;
			INC_CHARPTR(p);
		}
		result = Fcons(make_string(string, p - string), result);
		if (p < end) {
			string = p;
			INC_CHARPTR(string);	/* skip sepchar */
		} else
			break;
	}
	return Fnreverse(result);
}

/* The same as the above, except PATH is an external C string (it is
   converted using Qfile_name), and sepchar is hardcoded to SEPCHAR
   (':' or whatever).  */
Lisp_Object decode_path(/*const*/ char *path)
{
	Bytecount newlen;
	Bufbyte *newpath;
	if (!path)
		return Qnil;

	TO_INTERNAL_FORMAT(C_STRING, path, ALLOCA, (newpath, newlen),
			   Qfile_name);

	/* #### Does this make sense?  It certainly does for
	   decode_env_path(), but it looks dubious here.  Does any code
	   depend on decode_path("") returning nil instead of an empty
	   string?  */
	if (!newlen)
		return Qnil;

	return split_string_by_emchar_1(newpath, newlen, SEPCHAR);
}

Lisp_Object decode_env_path(const char *evarname, /*const*/ char *default_)
{
	/*const*/ char *path = 0;
	if (evarname)
		path = egetenv(evarname);
	if (!path)
		path = default_;
	return decode_path(path);
}

/* Ben thinks this function should not exist or be exported to Lisp.
   We use it to define split-path-string in subr.el (not!).  */

DEFUN("split-string-by-char", Fsplit_string_by_char, 2, 2, 0, /*
Split STRING into a list of substrings originally separated by SEPCHAR.
*/
      (string, sepchar))
{
	CHECK_STRING(string);
	CHECK_CHAR(sepchar);
	return split_string_by_emchar_1(XSTRING_DATA(string),
					XSTRING_LENGTH(string), XCHAR(sepchar));
}

/* #### This was supposed to be in subr.el, but is used VERY early in
   the bootstrap process, so it goes here.  Damn.  */

DEFUN("split-path", Fsplit_path, 1, 1, 0, /*
Explode a search path into a list of strings.
The path components are separated with the characters specified
with `path-separator'.
*/
      (path))
{
	CHECK_STRING(path);

	while (!STRINGP(Vpath_separator)
	       || (XSTRING_CHAR_LENGTH(Vpath_separator) != 1))
		Vpath_separator = signal_simple_continuable_error
		    ("`path-separator' should be set to a single-character string",
		     Vpath_separator);

	return (split_string_by_emchar_1
		(XSTRING_DATA(path), XSTRING_LENGTH(path),
		 charptr_emchar(XSTRING_DATA(Vpath_separator))));
}

DEFUN("noninteractive", Fnoninteractive, 0, 0, 0, /*
Non-nil return value means SXEmacs is running without interactive terminal.
*/
      ())
{
	return noninteractive ? Qt : Qnil;
}

#ifdef USE_ASSERTIONS
static int in_assert_failed = 0;
static const char *assert_failed_file = NULL;
static int assert_failed_line = 0;
static const char *assert_failed_expr = NULL;
/* This flag is useful to define if you're under a debugger; this way, you
   can put a breakpoint of assert_failed() and debug multiple problems
   in one session without having to recompile. */
static int assertions_dont_abort = 0;

#ifdef fprintf
#undef fprintf
#endif

#ifdef abort
#undef abort			/* avoid infinite #define loop... */
#endif

#define enter_debugger()

void
assert_failed(const char *file, int line, const char *expr)
{
	/* If we're already crashing, let's not crash again.  This might be
	   critical to getting auto-saving working properly. */
	if (fatal_error_in_progress)
		return;

	/* We are extremely paranoid so we sensibly deal with recursive
	   assertion failures. */
	in_assert_failed++;
	inhibit_non_essential_printing_operations = 1;

	if (in_assert_failed >= 4)
		_exit(-1);
	else if (in_assert_failed == 3) {
		enter_debugger();
		abort();
	} else if (in_assert_failed == 2) {
		/* Not stderr_out(), which does additional things and may trigger
		   a recursive assertion failure.  fprintf was undeffed above, in
		   case it was encapsulated. */
		fprintf(stderr,
			"\n\nFatal error: recursive assertion failure, "
			"file %s, line %d, %s\n", file, line, expr);
		fprintf(stderr,
			"Original assertion failure: file %s, line %d, %s\n",
			assert_failed_file, assert_failed_line,
			assert_failed_expr);
		fflush(stderr);
		enter_debugger();
		debug_short_backtrace(0x7FFF);
	} else {
		assert_failed_file = file;
		assert_failed_line = line;
		assert_failed_expr = expr;

		if (!initialized)
			fprintf(stderr,
				"\nFatal error: assertion failed, file %s, line %d, %s\n",
				file, line, expr);
		else
			stderr_out
			    ("\nFatal error: assertion failed, file %s, line %d, %s\n",
			     file, line, expr);
		fflush(stderr);
		enter_debugger();
		debug_backtrace();
	}
	if (! assertions_dont_abort) {
		abort();
	}
	inhibit_non_essential_printing_operations = 0;
	in_assert_failed = 0;
}
#endif				/* USE_ASSERTIONS */

#ifdef QUANTIFY
DEFUN("quantify-start-recording-data", Fquantify_start_recording_data, 0, 0, "", /*
Start recording Quantify data.
*/
      ())
{
	quantify_start_recording_data();
	return Qnil;
}

DEFUN("quantify-stop-recording-data", Fquantify_stop_recording_data, 0, 0, "", /*
Stop recording Quantify data.
*/
      ())
{
	quantify_stop_recording_data();
	return Qnil;
}

DEFUN("quantify-clear-data", Fquantify_clear_data, 0, 0, "", /*
Clear all Quantify data.
*/
      ())
{
	quantify_clear_data();
	return Qnil;
}
#endif				/* QUANTIFY */

void syms_of_emacs(void)
{
#ifndef CANNOT_DUMP
	DEFSUBR(Fdump_emacs);
#endif				/* !CANNOT_DUMP */

	DEFSUBR(Frun_emacs_from_temacs);
	DEFSUBR(Frunning_temacs_p);
	DEFSUBR(Finvocation_name);
	DEFSUBR(Finvocation_directory);
	DEFSUBR(Fkill_emacs);
	DEFSUBR(Fnoninteractive);

#ifdef DEBUG_SXEMACS
	DEFSUBR(Fforce_debugging_signal);
#endif

#ifdef QUANTIFY
	DEFSUBR(Fquantify_start_recording_data);
	DEFSUBR(Fquantify_stop_recording_data);
	DEFSUBR(Fquantify_clear_data);
#endif				/* QUANTIFY */

	DEFSUBR(Fsplit_string_by_char);
	DEFSUBR(Fsplit_path);	/* #### */

	defsymbol(&Qkill_emacs_hook, "kill-emacs-hook");
	defsymbol(&Qsave_buffers_kill_emacs, "save-buffers-kill-emacs");
}

void vars_of_emacs(void)
{
	DEFVAR_BOOL("suppress-early-error-handler-backtrace", &suppress_early_error_handler_backtrace /*
Non-nil means early error handler shouldn't print a backtrace.
												      */ );

	DEFVAR_LISP("command-line-args", &Vcommand_line_args /*
Args passed by shell to SXEmacs, as a list of strings.
							     */ );

	DEFVAR_LISP("invocation-name", &Vinvocation_name /*
The program name that was used to run SXEmacs.
Any directory names are omitted.
							 */ );

	DEFVAR_LISP("invocation-directory", &Vinvocation_directory /*
The directory in which the SXEmacs executable was found, to run it.
The value is simply the program name if that directory's name is not known.
								   */ );

	DEFVAR_LISP("invocation-path", &Vinvocation_path /*
The path in which the SXEmacs executable was found, to run it.
The value is simply the value of environment variable PATH on startup
if SXEmacs was found there.
							 */ );

#if 0				/* FSFmacs */
	xxDEFVAR_LISP("installation-directory", &Vinstallation_directory,
		      "A directory within which to look for the `lib-src' and `etc' directories.\n"
		      "This is non-nil when we can't find those directories in their standard\n"
		      "installed locations, but we can find them\n"
		      "near where the SXEmacs executable was found.");
#endif

	DEFVAR_LISP("system-type", &Vsystem_type /*
Symbol indicating type of operating system you are using.
						 */ );
	Vsystem_type = intern(SYSTEM_TYPE);
	Fprovide(intern(SYSTEM_TYPE));

#ifndef EMACS_CONFIGURATION
# define EMACS_CONFIGURATION "UNKNOWN"
#endif
	DEFVAR_LISP("system-configuration", &Vsystem_configuration /*
String naming the configuration SXEmacs was built for.
								   */ );
	Vsystem_configuration = build_string(EMACS_CONFIGURATION);

#ifndef EMACS_CONFIG_OPTIONS
# define EMACS_CONFIG_OPTIONS "UNKNOWN"
#endif
	DEFVAR_LISP("system-configuration-options", &Vsystem_configuration_options /*
String containing the configuration options SXEmacs was built with.
										   */ );
	Vsystem_configuration_options = build_string(EMACS_CONFIG_OPTIONS);

	DEFVAR_LISP("emacs-major-version", &Vemacs_major_version /*
Major version number of this version of Emacs, as an integer.
Warning: this variable did not exist in Emacs versions earlier than:
  FSF Emacs:   19.23
  XEmacs:      19.10
								 */ );
	Vemacs_major_version = make_int(EMACS_MAJOR_VERSION);

	DEFVAR_LISP("emacs-minor-version", &Vemacs_minor_version /*
Minor version number of this version of Emacs, as an integer.
Warning: this variable did not exist in Emacs versions earlier than:
  FSF Emacs:   19.23
  XEmacs:      19.10
								 */ );
	Vemacs_minor_version = make_int(EMACS_MINOR_VERSION);

	DEFVAR_LISP("emacs-patch-level", &Vemacs_patch_level /*
The patch level of this version of Emacs, as an integer.
The value is non-nil if this version of SXEmacs is part of a series of
stable SXEmacsen, but has bug fixes applied.
Warning: this variable does not exist in FSF Emacs or in XEmacs versions
earlier than 21.1.1
							     */ );
#ifdef EMACS_PATCH_LEVEL
	Vemacs_patch_level = make_int(EMACS_PATCH_LEVEL);
#else
	Vemacs_patch_level = Qnil;
#endif

	DEFVAR_LISP("emacs-beta-version", &Vemacs_beta_version /*
Beta number of this version of Emacs, as an integer.
The value is nil if this is an officially released version of SXEmacs.
Warning: this variable does not exist in FSF Emacs or in XEmacs versions
earlier than 20.3.
							       */ );
#ifdef EMACS_BETA_VERSION
	Vemacs_beta_version = make_int(EMACS_BETA_VERSION);
#else
	Vemacs_beta_version = Qnil;
#endif

	DEFVAR_LISP("sxemacs-git-version", &Vsxemacs_git_version /*
This revision name of this SXEmacs.
Warning: this variable does not exist in FSF Emacs or XEmacs.
								   */ );

	Vsxemacs_git_version = build_string(SXEMACS_GIT_VERSION);

#ifdef INFODOCK
	DEFVAR_LISP("infodock-major-version", &Vinfodock_major_version /*
Major version number of this InfoDock release.
								       */ );
	Vinfodock_major_version = make_int(INFODOCK_MAJOR_VERSION);

	DEFVAR_LISP("infodock-minor-version", &Vinfodock_minor_version /*
Minor version number of this InfoDock release.
								       */ );
	Vinfodock_minor_version = make_int(INFODOCK_MINOR_VERSION);

	DEFVAR_LISP("infodock-build-version", &Vinfodock_build_version /*
Build version of this InfoDock release.
								       */ );
	Vinfodock_build_version = make_int(INFODOCK_BUILD_VERSION);
#endif

	DEFVAR_LISP("sxemacs-codename", &Vsxemacs_codename /*
Codename of this version of SXEmacs (a string).
							   */ );
#ifndef SXEMACS_CODENAME
#define SXEMACS_CODENAME "Noname"
#endif
	Vsxemacs_codename = build_string(SXEMACS_CODENAME);

	/* Lisp variables which contain command line flags.

	   The portable dumper stomps on these; they must be saved and restored
	   if they are processed before the call to pdump_load() in main_1().
	 */
	DEFVAR_BOOL("noninteractive", &noninteractive1 /*
Non-nil means SXEmacs is running without interactive terminal.
						       */ );

	DEFVAR_BOOL("inhibit-early-packages", &inhibit_early_packages /*
Set to non-nil when the early packages should not be respected at startup.
								      */ );
	DEFVAR_BOOL("warn-early-package-shadows", &warn_early_package_shadows /*
Set to non-nil when the early packages should not shadow late packages. Issues
warning at startup when that happens.
									      */ );
	warn_early_package_shadows = 0;

	DEFVAR_BOOL("inhibit-autoloads", &inhibit_autoloads /*
 Set to non-nil when autoloads should not be loaded at startup.
							    */ );

	DEFVAR_BOOL("debug-paths", &debug_paths	/*
Set to non-nil when debug information about paths should be printed.
						 */ );

	DEFVAR_BOOL("inhibit-site-modules", &inhibit_site_modules /*
Set to non-nil when site-modules should not be searched at startup.
								  */ );
#ifdef INHIBIT_SITE_MODULES
	inhibit_site_modules = 1;
#endif

	DEFVAR_INT("emacs-priority", &emacs_priority /*
Priority for SXEmacs to run at.
This value is effective only if set before SXEmacs is dumped,
and only if the SXEmacs executable is installed with setuid to permit
it to change priority.  (SXEmacs sets its uid back to the real uid.)
Currently, you need to define SET_EMACS_PRIORITY in `config.h'
before you compile SXEmacs, to enable the code for this feature.
						     */ );
	emacs_priority = 0;

	DEFVAR_CONST_LISP("internal-error-checking", &Vinternal_error_checking /*
Internal error checking built-in into this instance of SXEmacs.
This is a list of symbols, initialized at build-time.  Legal symbols
are:
   extents     - check extents prior to each extent change;
   typecheck   - check types strictly, aborting in case of error;
   malloc      - check operation of malloc;
   gc          - check garbage collection;
   bufpos      - check buffer positions.
   quick-build - user has requested the "quick-build" configure option.
									       */ );
	Vinternal_error_checking = Qnil;
#ifdef ERROR_CHECK_EXTENTS
	Vinternal_error_checking = Fcons(intern("extents"),
					 Vinternal_error_checking);
#endif
#ifdef ERROR_CHECK_TYPECHECK
	Vinternal_error_checking = Fcons(intern("typecheck"),
					 Vinternal_error_checking);
#endif
#ifdef ERROR_CHECK_MALLOC
	Vinternal_error_checking = Fcons(intern("malloc"),
					 Vinternal_error_checking);
#endif
#ifdef ERROR_CHECK_GC
	Vinternal_error_checking = Fcons(intern("gc"),
					 Vinternal_error_checking);
#endif
#ifdef ERROR_CHECK_BUFPOS
	Vinternal_error_checking = Fcons(intern("bufpos"),
					 Vinternal_error_checking);
#endif
#ifdef QUICK_BUILD
	Vinternal_error_checking = Fcons(intern("quick-build"),
					 Vinternal_error_checking);
#endif

	DEFVAR_CONST_LISP("mail-lock-methods", &Vmail_lock_methods /*
Mail spool locking methods supported by this instance of SXEmacs.
This is a list of symbols.  Each of the symbols is one of the
following: dot, lockf, flock, locking, mmdf.
								   */ );
	{
		Vmail_lock_methods = Qnil;
		Vmail_lock_methods = Fcons(intern("dot"), Vmail_lock_methods);
#ifdef HAVE_LOCKF
		Vmail_lock_methods = Fcons(intern("lockf"), Vmail_lock_methods);
#endif
#ifdef HAVE_FLOCK
		Vmail_lock_methods = Fcons(intern("flock"), Vmail_lock_methods);
#endif
#ifdef HAVE_MMDF
		Vmail_lock_methods = Fcons(intern("mmdf"), Vmail_lock_methods);
#endif
#ifdef HAVE_LOCKING
		Vmail_lock_methods =
		    Fcons(intern("locking"), Vmail_lock_methods);
#endif
	}

	DEFVAR_CONST_LISP("configure-mail-lock-method", &Vconfigure_mail_lock_method /*
Mail spool locking method suggested by configure.  This is one
of the symbols in MAIL-LOCK-METHODS.
										     */ );
	{
#if defined(MAIL_LOCK_FLOCK) && defined(HAVE_FLOCK)
		Vconfigure_mail_lock_method = intern("flock");
#elif defined(MAIL_LOCK_LOCKF) && defined(HAVE_LOCKF)
		Vconfigure_mail_lock_method = intern("lockf");
#elif defined(MAIL_LOCK_MMDF) && defined(HAVE_MMDF)
		Vconfigure_mail_lock_method = intern("mmdf");
#elif defined(MAIL_LOCK_LOCKING) && defined(HAVE_LOCKING)
		Vconfigure_mail_lock_method = intern("locking");
#else
		Vconfigure_mail_lock_method = intern("dot");
#endif
	}

	DEFVAR_LISP("path-separator", &Vpath_separator /*
The directory separator in search paths, as a string.
						       */ );
	{
		char c = SEPCHAR;
		Vpath_separator = make_string((Bufbyte *) & c, 1);
	}
}

void complex_vars_of_emacs(void)
{
	/* This is all related to path searching. */

	DEFVAR_LISP("emacs-program-name", &Vemacs_program_name /*
*Name of the Emacs variant.
For example, this may be \"sxemacs\" or \"infodock\".
This is mainly meant for use in path searching.
							       */ );
	Vemacs_program_name = build_string((char *)PATH_PROGNAME);

	DEFVAR_LISP("emacs-program-version", &Vemacs_program_version /*
*Version of the Emacs variant.
This typically has the form NN.NN-bNN.
This is mainly meant for use in path searching.
								     */ );
	Vemacs_program_version = build_string((char *)PATH_VERSION);

	DEFVAR_LISP("exec-path", &Vexec_path /*
*List of directories to search programs to run in subprocesses.
Each element is a string (directory name) or nil (try default directory).
					     */ );
	Vexec_path = Qnil;

	DEFVAR_LISP("exec-directory", &Vexec_directory /*
*Directory of architecture-dependent files that come with SXEmacs,
especially executable programs intended for SXEmacs to invoke.
						       */ );
	Vexec_directory = Qnil;

	DEFVAR_LISP("configure-exec-directory", &Vconfigure_exec_directory /*
For internal use by the build procedure only.
configure's idea of what `exec-directory' will be.
									   */ );
#ifdef PATH_EXEC
	Vconfigure_exec_directory = Ffile_name_as_directory
	    (build_string((char *)PATH_EXEC));
#else
	Vconfigure_exec_directory = Qnil;
#endif

	DEFVAR_LISP("lisp-directory", &Vlisp_directory /*
*Directory of core Lisp files that come with SXEmacs.
*/ );
	Vlisp_directory = Qnil;

	DEFVAR_LISP("configure-lisp-directory", &Vconfigure_lisp_directory /*
For internal use by the build procedure only.
configure's idea of what `lisp-directory' will be.
									   */ );
#ifdef PATH_LOADSEARCH
	Vconfigure_lisp_directory = Ffile_name_as_directory
	    (build_string((char *)PATH_LOADSEARCH));
#else
	Vconfigure_lisp_directory = Qnil;
#endif

	DEFVAR_LISP("mule-lisp-directory", &Vmule_lisp_directory /*
*Directory of Mule Lisp files that come with SXEmacs.
*/ );
	Vmule_lisp_directory = Qnil;

	DEFVAR_LISP("configure-mule-lisp-directory", &Vconfigure_mule_lisp_directory /*
For internal use by the build procedure only.
configure's idea of what `mule-lisp-directory' will be.
										     */ );
#ifdef PATH_MULELOADSEARCH
	Vconfigure_mule_lisp_directory = Ffile_name_as_directory
	    (build_string((char *)PATH_MULELOADSEARCH));
#else
	Vconfigure_mule_lisp_directory = Qnil;
#endif

	DEFVAR_LISP("module-directory", &Vmodule_directory /*
*Directory of core dynamic modules that come with SXEmacs.
*/ );
	Vmodule_directory = Qnil;

	DEFVAR_LISP("configure-module-directory", &Vconfigure_module_directory /*
For internal use by the build procedure only.
configure's idea of what `module-directory' will be.
									       */ );
#ifdef PATH_MODULESEARCH
	Vconfigure_module_directory = Ffile_name_as_directory
	    (build_string((char *)PATH_MODULESEARCH));
#else
	Vconfigure_module_directory = Qnil;
#endif

	DEFVAR_LISP("configure-package-path", &Vconfigure_package_path /*
For internal use by the build procedure only.
configure's idea of what the package path will be.
								       */ );
#ifdef PATH_PACKAGEPATH
	Vconfigure_package_path = decode_path(PATH_PACKAGEPATH);
#else
	Vconfigure_package_path = Qnil;
#endif

	DEFVAR_LISP("data-directory", &Vdata_directory /*
*Directory of architecture-independent files that come with SXEmacs,
intended for SXEmacs to use.
Use of this variable in new code is almost never correct.  See the
functions `locate-data-file' and `locate-data-directory' and the variable
`data-directory-list'.
						       */ );
	Vdata_directory = Qnil;

	DEFVAR_LISP("configure-data-directory", &Vconfigure_data_directory /*
For internal use by the build procedure only.
configure's idea of what `data-directory' will be.
									   */ );
#ifdef PATH_DATA
	Vconfigure_data_directory = Ffile_name_as_directory
	    (build_string((char *)PATH_DATA));
#else
	Vconfigure_data_directory = Qnil;
#endif

	DEFVAR_LISP("data-directory-list", &Vdata_directory_list /*
*List of directories of architecture-independent files that come with SXEmacs
or were installed as packages, and are intended for SXEmacs to use.
								 */ );
	Vdata_directory_list = Qnil;

	DEFVAR_LISP("site-module-directory", &Vsite_module_directory /*
*Directory of site-specific loadable modules that come with SXEmacs.
*/ );
	Vsite_module_directory = Qnil;

	DEFVAR_LISP("configure-site-module-directory", &Vconfigure_site_module_directory /*
For internal use by the build procedure only.
configure's idea of what `site-directory' will be.
											 */ );
#ifdef PATH_SITE_MODULES
	Vconfigure_site_module_directory = Ffile_name_as_directory
	    (build_string((char *)PATH_SITE_MODULES));
#else
	Vconfigure_site_module_directory = Qnil;
#endif

	DEFVAR_LISP("doc-directory", &Vdoc_directory /*
*Directory containing the DOC file that comes with SXEmacs.
This is usually the same as `exec-directory'.
						     */ );
	Vdoc_directory = Qnil;

	DEFVAR_LISP("configure-doc-directory", &Vconfigure_doc_directory /*
For internal use by the build procedure only.
configure's idea of what `doc-directory' will be.
									 */ );
#ifdef PATH_DOC
	Vconfigure_doc_directory = Ffile_name_as_directory
	    (build_string((char *)PATH_DOC));
#else
	Vconfigure_doc_directory = Qnil;
#endif

	DEFVAR_LISP("configure-exec-prefix-directory", &Vconfigure_exec_prefix_directory /*
For internal use by the build procedure only.
configure's idea of what `exec-prefix-directory' will be.
											 */ );
#ifdef PATH_EXEC_PREFIX
	Vconfigure_exec_prefix_directory = Ffile_name_as_directory
	    (build_string((char *)PATH_EXEC_PREFIX));
#else
	Vconfigure_exec_prefix_directory = Qnil;
#endif

	DEFVAR_LISP("configure-prefix-directory", &Vconfigure_prefix_directory /*
For internal use by the build procedure only.
configure's idea of what `prefix-directory' will be.
									       */ );
#ifdef PATH_PREFIX
	Vconfigure_prefix_directory = Ffile_name_as_directory
	    (build_string((char *)PATH_PREFIX));
#else
	Vconfigure_prefix_directory = Qnil;
#endif

	DEFVAR_LISP("configure-info-directory", &Vconfigure_info_directory /*
For internal use by the build procedure only.
This is the name of the directory in which the build procedure installed
Emacs's info files; the default value for Info-default-directory-list
includes this.
									   */ );
#ifdef PATH_INFO
	Vconfigure_info_directory =
	    Ffile_name_as_directory(build_string(PATH_INFO));
#else
	Vconfigure_info_directory = Qnil;
#endif

	DEFVAR_LISP("configure-info-path", &Vconfigure_info_path /*
The configured initial path for info documentation.
								 */ );
#ifdef PATH_INFOPATH
	Vconfigure_info_path = decode_path(PATH_INFOPATH);
#else
	Vconfigure_info_path = Qnil;
#endif
}

#if defined(__sgi) && !defined(PDUMP)
/* This is so tremendously ugly I'd puke. But then, it works.
 * The target is to override the static constructor from the
 * libiflPNG.so library which is masquerading as libz, and
 * cores on us when re-started from the dumped executable.
 * This will have to go for 21.1  -- OG.
 */
void __sti__iflPNGFile_c___(void);
void __sti__iflPNGFile_c___(void)
{
}

#endif
