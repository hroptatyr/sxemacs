/* Handling asynchronous signals.
   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
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


/* Synched up with: Not synched with FSF.  Split out of keyboard.c. */

#include <config.h>
#include "lisp.h"

#include "ui/console.h"
#include "events/events.h"		/* for signal_fake_event() */
#include "ui/frame.h"
#include "sysdep.h"
#include "syssignal.h"
#include "systime.h"

#include "sysfile.h"

/* Set to 1 when a quit-check signal (either a SIGIO interrupt or
   the asynch. timeout for poll-for-quit) occurs.  The QUITP
   macro may look at this. */
volatile int quit_check_signal_happened;

/* Count of the number of times a quit-check signal has occurred.
   Some stuff in event-Xt.c looks at this. */
volatile int quit_check_signal_tick_count;

/* Set to 1 when a SIGINT (or SIGQUIT) interrupt is processed.
   maybe_read_quit_event() looks at this. */
volatile int sigint_happened;

/* Set to 1 when an asynch. timeout signal occurs. */
static volatile int alarm_happened;

/* This is used to synchronize setting the waiting_for_user_input_p
   flag. */
static volatile int alarm_happened_while_emacs_was_blocking;

/* See check_quit() for when this is set. */
int dont_check_for_quit;

#if !defined (SIGIO) && !defined (DONT_POLL_FOR_QUIT)
int poll_for_quit_id;
#endif

#if defined(HAVE_UNIX_PROCESSES) && !defined(SIGCHLD)
int poll_for_sigchld_id;
#endif

/* This variable is used to communicate to a lisp
   process-filter/sentinel/asynchronous callback (via the function
   Fwaiting_for_user_input_p below) whether SXEmacs was waiting for
   user-input when that process-filter was called. */
static int waiting_for_user_input_p;

static int interrupts_slowed_down;

#define SLOWED_DOWN_INTERRUPTS_SECS 15
#define NORMAL_QUIT_CHECK_TIMEOUT_MSECS 250
#define NORMAL_SIGCHLD_CHECK_TIMEOUT_MSECS 250

/* Used so that signals can break out of system calls that aren't
   naturally interruptible. */

JMP_BUF break_system_call_jump;
volatile int can_break_system_calls;

/**********************************************************************/
/*                  Asynchronous timeout functions                    */
/**********************************************************************/

/* The pending timers are stored in an ordered list, where the first timer
   on the list is the first one to fire.  Times recorded here are
   absolute. */
static struct low_level_timeout *async_timer_queue;

/* Nonzero means async timers are temporarily suppressed.  */
static int async_timer_suppress_count;

static void set_one_shot_timer(EMACS_TIME interval)
{
#ifdef HAVE_SETITIMER
	struct itimerval it;
	it.it_value = interval;
	EMACS_SET_SECS_USECS(it.it_interval, 0, 0);
	qxe_setitimer(ITIMER_REAL, &it, 0);
#else
	int secs;
	EMACS_TIME_TO_INT(interval, secs);
	alarm(secs);
#endif
}

static void reset_interval_timer(void)
{
	EMACS_TIME interval;

	/* Get the interval to set.  If an interval is available,
	   make sure it's not zero (this is a valid return, but it will
	   cause the timer to get disabled, so convert it to a very short
	   time). */
	if (get_low_level_timeout_interval(async_timer_queue, &interval)) {
		if (EMACS_SECS(interval) == 0 && EMACS_USECS(interval) == 0)
			EMACS_SET_USECS(interval, 1);
	} else
		/* A time of 0 means "disable". */
		EMACS_SET_SECS_USECS(interval, 0, 0);

	set_one_shot_timer(interval);
}

int event_stream_add_async_timeout(EMACS_TIME thyme)
{
	int id = add_low_level_timeout(&async_timer_queue, thyme);

	/* If this timeout is at the head of the queue, then we need to
	   set the timer right now for this timeout.  Otherwise, things
	   are fine as-is; after the timers ahead of us are signalled,
	   the timer will be set for us. */

	if (async_timer_queue->id == id)
		reset_interval_timer();

	return id;
}

void event_stream_remove_async_timeout(int id)
{
	int first = (async_timer_queue && async_timer_queue->id == id);
	remove_low_level_timeout(&async_timer_queue, id);

	/* If we removed the timeout from the head of the queue, then
	   we need to reset the interval timer right now. */
	if (first)
		reset_interval_timer();
}

/* Handle an alarm once each second and read pending input
   so as to handle a C-g if it comes in.  */

static SIGTYPE alarm_signal(int signo)
{
	if (interrupts_slowed_down) {
		something_happened = 1;	/* tell QUIT to wake up */
		/* we are in "slowed-down interrupts" mode; the only alarm
		   happening here is the slowed-down quit-check alarm, so
		   we set this flag.

		   Do NOT set alarm_happened, because we don't want anyone
		   looking at the timeout queue.  We didn't set it and
		   it needs to stay the way it is. */
		quit_check_signal_happened = 1;

		/* can_break_system_calls is set when we want to break out of
		   non-interruptible system calls. */
		if (can_break_system_calls) {
			/* reset the flag for safety and such.  Do this *before*
			   unblocking or reestablishing the signal to avoid potential
			   race conditions. */
			can_break_system_calls = 0;
			EMACS_UNBLOCK_SIGNAL(signo);
			EMACS_REESTABLISH_SIGNAL(signo, alarm_signal);
			LONGJMP(break_system_call_jump, 0);
		}

		EMACS_REESTABLISH_SIGNAL(signo, alarm_signal);
		SIGRETURN;
	}

	something_happened = 1;	/* tell QUIT to wake up */
	alarm_happened = 1;
	if (emacs_is_blocking)
		alarm_happened_while_emacs_was_blocking = 1;
	/* #### This is for QUITP.  When it is run, it may not be the
	   place to do arbitrary stuff like run asynch. handlers, but
	   it needs to know whether the poll-for-quit asynch. timeout
	   went off.  Rather than put the code in to compute this
	   specially, we just set this flag.  Should fix this. */
	quit_check_signal_happened = 1;

#ifdef HAVE_UNIXOID_EVENT_LOOP
	signal_fake_event();
#endif

	EMACS_REESTABLISH_SIGNAL(signo, alarm_signal);
	SIGRETURN;
}

static void init_async_timeouts(void)
{
	signal(SIGALRM, alarm_signal);
	async_timer_suppress_count = 0;
}

/* Turn off async timeouts.  */

extern void stop_async_timeouts(void)
{
	if (async_timer_suppress_count == 0) {
		/* If timer was on, turn it off. */
		EMACS_TIME thyme;
		EMACS_SET_SECS_USECS(thyme, 0, 0);
		set_one_shot_timer(thyme);
	}
	async_timer_suppress_count++;
}

/* Turn on async timeouts again. */

extern void start_async_timeouts(void)
{
	assert(async_timer_suppress_count > 0);
	async_timer_suppress_count--;
	if (async_timer_suppress_count == 0) {
		/* Some callers turn off async timeouts and then use the alarm
		   for their own purposes; so reinitialize everything. */
		signal(SIGALRM, alarm_signal);
		reset_interval_timer();
	}
}

/* Some functions don't like being interrupted with SIGALRM or SIGIO.
   Previously we were calling stop_interrupts() / start_interrupts(),
   but then if the program hangs in one of those functions, e.g.
   waiting for a connect(), we're really screwed.  So instead we
   just "slow them down".  We do this by disabling all interrupts
   and then installing a timer of length fairly large, like 5 or
   10 secs.  That way, any "legitimate" connections (which should
   take a fairly short amount of time) go through OK, but we can
   interrupt bogus ones. */

void slow_down_interrupts(void)
{
	EMACS_TIME thyme;

	/* We have to set the flag *before* setting the slowed-down timer,
	   to avoid a race condition -- if the signal occurs between the
	   call to set_one_shot_timer() and the setting of this flag,
	   alarm_happened will get set, which will be a Bad Thing if
	   there were no timeouts on the queue. */
	interrupts_slowed_down++;
	if (interrupts_slowed_down == 1) {
		stop_interrupts();
		EMACS_SET_SECS_USECS(thyme, SLOWED_DOWN_INTERRUPTS_SECS, 0);
		set_one_shot_timer(thyme);
	}
}

void speed_up_interrupts(void)
{
	if (interrupts_slowed_down > 0) {
		start_interrupts();
		/* Change this flag AFTER fiddling with interrupts, for the same
		   race-condition reasons as above. */
		interrupts_slowed_down--;
	}
}

static void handle_alarm_going_off(void)
{
	int interval_id;

	/* If asynch. timeouts are blocked, then don't do anything now,
	   but make this function get called again next QUIT.

	   #### This is a bit inefficient because there will be function call
	   overhead each time QUIT occurs. */

	if (!NILP(Vinhibit_quit)) {
		something_happened = 1;
		alarm_happened = 1;
		return;
	}

	interval_id = pop_low_level_timeout(&async_timer_queue, 0);

	reset_interval_timer();
	if (alarm_happened_while_emacs_was_blocking) {
		alarm_happened_while_emacs_was_blocking = 0;
		waiting_for_user_input_p = 1;
	}
	event_stream_deal_with_async_timeout(interval_id);
	waiting_for_user_input_p = 0;
}

#ifdef HAVE_SETITIMER

unsigned int alarm(unsigned int howlong)
{
	struct itimerval old_it, new_it;

	/* If alarm() gets called when polling isn't disabled, it can mess
	   up the periodic timer. */
	assert(async_timer_suppress_count > 0);

	new_it.it_value.tv_sec = howlong;
	new_it.it_value.tv_usec = 0;
	new_it.it_interval.tv_sec = 0;
	new_it.it_interval.tv_usec = 0;
	qxe_setitimer(ITIMER_REAL, &new_it, &old_it);

	/* Never return zero if there was a timer outstanding. */
	return old_it.it_value.tv_sec + (old_it.it_value.tv_usec > 0 ? 1 : 0);
}

int
qxe_setitimer(int kind, const struct itimerval *itnew, struct itimerval *itold)
{
	return setitimer(kind, itnew, itold);
}

#endif				/* HAVE_SETITIMER */

DEFUN("waiting-for-user-input-p", Fwaiting_for_user_input_p, 0, 0, 0,	/*
Return non-nil if SXEmacs is waiting for input from the user.
This is intended for use by asynchronous timeout callbacks and by
asynchronous process output filters and sentinels (not yet implemented
in SXEmacs).  It will always be nil if SXEmacs is not inside of
an asynchronous timeout or process callback.
*/
      ())
{
	return waiting_for_user_input_p ? Qt : Qnil;
}

/**********************************************************************/
/*                        Control-G checking                          */
/**********************************************************************/

/* Set this for debugging, to have a way to get out */
int stop_character;		/* #### not currently implemented */

/* This routine is called in response to a SIGINT or SIGQUIT.
   On TTY's, one of these two signals will get generated in response
   to C-g.  (When running under X, C-g is handled using the SIGIO
   handler, which sets a flag telling the QUIT macro to scan the
   unread events for a ^G.)

   Otherwise it sets the Lisp variable  quit-flag  not-nil.
   This causes  eval  to throw, when it gets a chance.
   If  quit-flag  is already non-nil, it stops the job right away.  */

static SIGTYPE interrupt_signal(int sig)
{
	/* This function can call lisp */
	/* #### we should NOT be calling lisp from a signal handler, boys
	   and girls */
	/* Must preserve main program's value of errno.  */
	int old_errno = errno;

	EMACS_REESTABLISH_SIGNAL(sig, interrupt_signal);

/* with the macroized error-checking stuff, the garbage below
   may mess things up because XCONSOLE() and such can use and
   change global vars. */
#if ! (defined (ERROR_CHECK_TYPECHECK) && defined (MACROIZE_ERROR_CHECKING))
	if (sigint_happened && CONSOLEP(Vcontrolling_terminal) &&
	    CONSOLE_LIVE_P(XCONSOLE(Vcontrolling_terminal)) &&
	    !emacs_is_blocking) {
		int c;
		fflush(stdout);
		reset_initial_console();
		EMACS_UNBLOCK_SIGNAL(sig);
#ifdef SIGTSTP			/* Support possible in later USG versions */
/*
 * On systems which can suspend the current process and return to the original
 * shell, this command causes the user to end up back at the shell.
 * The "Auto-save" and "Abort" questions are not asked until
 * the user elects to return to emacs, at which point he can save the current
 * job and either dump core or continue.
 */
		sys_suspend();
#else
		/* Perhaps should really fork an inferior shell?
		   But that would not provide any way to get back
		   to the original shell, ever.  */
		stdout_out
		    ("No support for stopping a process on this operating system;\n");
		stdout_out("you can continue or abort.\n");
#endif				/* not SIGTSTP */
		stdout_out("Auto-save? (y or n) ");
		if (((c = getc(stdin)) & ~040) == 'Y')
			Fdo_auto_save(Qnil, Qnil);
		while (c != '\n')
			c = getc(stdin);
		stdout_out("Abort (and dump core)? (y or n) ");
		if (((c = getc(stdin)) & ~040) == 'Y')
			abort();
		while (c != '\n')
			c = getc(stdin);
		stdout_out("Continuing...\n");
		reinit_initial_console();
		MARK_FRAME_CHANGED(XFRAME(DEVICE_SELECTED_FRAME
					  (XDEVICE(CONSOLE_SELECTED_DEVICE
						   (XCONSOLE
						    (Vcontrolling_terminal))))));
	} else
#endif				/* ! (defined (ERROR_CHECKING) && defined (MACROIZE_ERROR_CHECKING)) */
	{
		/* Else request quit when it's safe */
		Vquit_flag = Qt;
		sigint_happened = 1;
#ifdef HAVE_UNIXOID_EVENT_LOOP
		signal_fake_event();
#endif
	}
	errno = old_errno;
	SIGRETURN;
}

static Lisp_Object restore_dont_check_for_quit(Lisp_Object val)
{
	dont_check_for_quit = XINT(val);
	return Qnil;
}

void begin_dont_check_for_quit(void)
{
	specbind(Qinhibit_quit, Qt);
	record_unwind_protect(restore_dont_check_for_quit,
			      make_int(dont_check_for_quit));
	dont_check_for_quit = 1;
}

/* The effect of this function is to set Vquit_flag if the user pressed
   ^G and discard the ^G, so as to not notice the same ^G again. */
int check_quit(void)
{
	/* dont_check_for_quit is set in two circumstances:

	   (1) when we are in the process of changing the window
	   configuration.  The frame might be in an inconsistent state,
	   which will cause assertion failures if we check for QUIT.

	   (2) when we are reading events, and want to read the C-g
	   as an event.  The normal check for quit will discard the C-g,
	   which would be bad.

	   #### C-g is still often read as quit, e.g. if you type C-x C-g
	   (the C-g happens during the sit-for in maybe_echo_keys(); even
	   if we attempt to inhibit quit here, there is still a check
	   later on for QUIT.  To fix this properly requires a fairly
	   substantial overhaul of the quit-checking code, which is
	   probably not worth it.)

	   We should *not* conditionalize on Vinhibit_quit, or
	   critical-quit (Control-Shift-G) won't work right. */

	if (dont_check_for_quit)
		return 0;

	if (quit_check_signal_happened) {
		quit_check_signal_happened = 0;
		event_stream_quit_p();
		return 1;
	} else
		return 0;
}

int check_what_happened(void)
{				/* called from QUIT when
				   something_happened gets set */
	something_happened = 0;
	if (alarm_happened) {
		alarm_happened = 0;
		handle_alarm_going_off();
	}
	return check_quit();
}

void init_poll_for_quit(void)
{
#if !defined (SIGIO) && !defined (DONT_POLL_FOR_QUIT)
	/* Check for C-g every 1/4 of a second.

	   #### This is just a guess.  Some investigation will have to be
	   done to see what the best value is.  The best value is the
	   smallest possible value that doesn't cause a significant amount
	   of running time to be spent in C-g checking. */
	if (!poll_for_quit_id)
		poll_for_quit_id =
		    event_stream_generate_wakeup
		    (NORMAL_QUIT_CHECK_TIMEOUT_MSECS,
		     NORMAL_QUIT_CHECK_TIMEOUT_MSECS, Qnil, Qnil, 1);
#endif				/* not SIGIO and not DONT_POLL_FOR_QUIT */
}

void reset_poll_for_quit(void)
{
#if !defined (SIGIO) && !defined (DONT_POLL_FOR_QUIT)
	if (poll_for_quit_id) {
		event_stream_disable_wakeup(poll_for_quit_id, 1);
		poll_for_quit_id = 0;
	}
#endif				/* not SIGIO and not DONT_POLL_FOR_QUIT */
}

#if defined(HAVE_UNIX_PROCESSES) && !defined(SIGCHLD)

static void init_poll_for_sigchld(void)
{
	/* Check for terminated processes every 1/4 of a second.

	   #### This is just a guess.  Some investigation will have to be
	   done to see what the best value is.  The best value is the
	   smallest possible value that doesn't cause a significant amount
	   of running time to be spent in process-termination checking.
	 */
	poll_for_sigchld_id =
	    event_stream_generate_wakeup(NORMAL_SIGCHLD_CHECK_TIMEOUT_MSECS,
					 NORMAL_SIGCHLD_CHECK_TIMEOUT_MSECS,
					 Qnil, Qnil, 1);
}

#endif				/* not SIGCHLD */

#ifdef SIGIO

static void input_available_signal(int signo)
{
	something_happened = 1;	/* tell QUIT to wake up */
	quit_check_signal_happened = 1;
	quit_check_signal_tick_count++;
	EMACS_REESTABLISH_SIGNAL(signo, input_available_signal);
	SIGRETURN;
}

#endif				/* SIGIO */

/**********************************************************************/
/*                     Enabling/disabling signals                     */
/**********************************************************************/

static int interrupts_initted;

void stop_interrupts(void)
{
	if (!interrupts_initted)
		return;
#if defined(SIGIO) && !defined(BROKEN_SIGIO)
	unrequest_sigio();
#endif
	stop_async_timeouts();
}

void start_interrupts(void)
{
	if (!interrupts_initted)
		return;
#if defined(SIGIO) && !defined(BROKEN_SIGIO)
	request_sigio();
#endif
	start_async_timeouts();
}

/* Cheesy but workable implementation of sleep() that doesn't
   interfere with our periodic timers. */

void emacs_sleep(int secs)
{
	stop_interrupts();
	sleep(secs);
	start_interrupts();
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

/* If we've been nohup'ed, keep it that way.
   This allows `nohup sxemacs &' to work.
   More generally, if a normally fatal signal has been redirected
   to SIG_IGN by our invocation environment, trust the environment.
   This keeps sxemacs from being killed by a SIGQUIT intended for a
   different process after having been backgrounded under a
   non-job-control shell! */
static void handle_signal_if_fatal(int signo)
{
#if 1
	if (signal(signo, fatal_error_signal) == SIG_IGN) {
		signal(signo, SIG_IGN);
	}
#endif
}

void init_signals_very_early(void)
{
	/* Catch all signals that would kill us.
	   Don't catch these signals in batch mode if not initialized.
	   On some machines, this sets static data that would make
	   signal fail to work right when the dumped Emacs is run.  */
	if (noninteractive && !initialized)
		return;

	handle_signal_if_fatal(SIGILL);	/* ANSI */
	handle_signal_if_fatal(SIGABRT);	/* ANSI */
	handle_signal_if_fatal(SIGFPE);	/* ANSI */
	handle_signal_if_fatal(SIGSEGV);	/* ANSI */
	handle_signal_if_fatal(SIGTERM);	/* ANSI */

#ifdef SIGHUP
	handle_signal_if_fatal(SIGHUP);	/* POSIX */
#endif
#ifdef SIGQUIT
	handle_signal_if_fatal(SIGQUIT);	/* POSIX */
#endif
#ifdef SIGTRAP
	handle_signal_if_fatal(SIGTRAP);	/* POSIX */
#endif
#ifdef SIGUSR1
	handle_signal_if_fatal(SIGUSR1);	/* POSIX */
#endif
#ifdef SIGUSR2
	handle_signal_if_fatal(SIGUSR2);	/* POSIX */
#endif
#ifdef SIGPIPE
	handle_signal_if_fatal(SIGPIPE);	/* POSIX */
#endif
#ifdef SIGALRM
	/* This will get reset later, once we're
	   capable of handling it properly. */
	handle_signal_if_fatal(SIGALRM);	/* POSIX */
#endif

#ifdef SIGBUS
	handle_signal_if_fatal(SIGBUS);	/* XPG5 */
#endif
#ifdef SIGSYS
	handle_signal_if_fatal(SIGSYS);	/* XPG5 */
#endif
#ifdef SIGXCPU
# if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
	handle_signal_if_fatal(SIGXCPU);	/* XPG5 */
# endif	 /* !BDWGC */
#endif
#ifdef SIGXFSZ
	handle_signal_if_fatal(SIGXFSZ);	/* XPG5 */
#endif
#ifdef SIGVTALRM
	handle_signal_if_fatal(SIGVTALRM);	/* XPG5 */
#endif
#ifdef SIGPROF
	/* Messes up the REAL profiler */
	/* handle_signal_if_fatal (SIGPROF); *//* XPG5 */
#endif

#ifdef SIGHWE
	handle_signal_if_fatal(SIGHWE);
#endif
#ifdef SIGPRE
	handle_signal_if_fatal(SIGPRE);
#endif
#ifdef SIGORE
	handle_signal_if_fatal(SIGORE);
#endif
#ifdef SIGUME
	handle_signal_if_fatal(SIGUME);
#endif
#ifdef SIGDLK
	handle_signal_if_fatal(SIGDLK);
#endif
#ifdef SIGCPULIM
	handle_signal_if_fatal(SIGCPULIM);
#endif
#ifdef SIGIOT
	handle_signal_if_fatal(SIGIOT);
#endif
#ifdef SIGEMT
	handle_signal_if_fatal(SIGEMT);
#endif
#ifdef SIGLOST
# if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
	handle_signal_if_fatal(SIGLOST);
# endif	 /* !BDWGC */
#endif
#ifdef SIGSTKFLT		/* coprocessor stack fault under Linux */
	handle_signal_if_fatal(SIGSTKFLT);
#endif
#ifdef SIGUNUSED		/* exists under Linux, and will kill process! */
	handle_signal_if_fatal(SIGUNUSED);
#endif

#ifdef AIX
/* 20 is SIGCHLD, 21 is SIGTTIN, 22 is SIGTTOU.  */
#ifndef _I386
	handle_signal_if_fatal(SIGIOINT);
#endif
	handle_signal_if_fatal(SIGGRANT);
	handle_signal_if_fatal(SIGRETRACT);
	handle_signal_if_fatal(SIGSOUND);
	handle_signal_if_fatal(SIGMSG);
#endif				/* AIX */

#ifdef SIGDANGER
	/* This just means available memory is getting low.  */
	signal(SIGDANGER, memory_warning_signal);
#endif
}

void syms_of_signal(void)
{
	DEFSUBR(Fwaiting_for_user_input_p);
}

void init_interrupts_late(void)
{
	if (!noninteractive) {
		signal(SIGINT, interrupt_signal);
#ifdef HAVE_TERMIO
		/* On  systems with TERMIO, C-g is set up for both SIGINT and SIGQUIT
		   and we can't tell which one it will give us.  */
		signal(SIGQUIT, interrupt_signal);
#endif				/* HAVE_TERMIO */
		init_async_timeouts();
#ifdef SIGIO
		signal(SIGIO, input_available_signal);
# ifdef SIGPOLL			/* XPG5 */
		/* Some systems (e.g. Motorola SVR4) losingly have different
		   values for SIGIO and SIGPOLL, and send SIGPOLL instead of
		   SIGIO.  On those same systems, an uncaught SIGPOLL kills the
		   process. */
		signal(SIGPOLL, input_available_signal);
# endif
#elif !defined (DONT_POLL_FOR_QUIT)
		init_poll_for_quit();
#endif
	}
#if defined(HAVE_UNIX_PROCESSES) && !defined(SIGCHLD)
	init_poll_for_sigchld();
#endif

	EMACS_UNBLOCK_ALL_SIGNALS();

	interrupts_initted = 1;
}
