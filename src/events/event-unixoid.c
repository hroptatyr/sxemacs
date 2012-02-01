/* Code shared between all event loops that use select() and have a
   different input descriptor for each device.
   Copyright (C) 1991, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
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


/* Synched up with: Not in FSF. */

/* This file has been Mule-ized. */

#include <config.h>
#include "lisp.h"

#include "ui/console-stream.h"
#include "ui/TTY/console-tty.h" /* for stuff in
				   event_stream_unixoid_select_console. Needs
				   refactoring */
#include "ui/device.h"
#define INCLUDE_EVENTS_H_PRIVATE_SPHERE
#include "events.h"
#include "lstream.h"
#include "process.h"

#include "sysdep.h"
#include "sysfile.h"
#include "sysproc.h"		/* select stuff */
#include "systime.h"

/* Mask of bits indicating the descriptors that we wait for input on.
   These work as follows:

   input_wait_mask == mask of all file descriptors we select() on,
		      including TTY/stream console descriptors,
		      process descriptors, and the signal event pipe.
		      Only used in event-tty.c; event-Xt.c uses
		      XtAppAddInput(), and the call to select() is down in
		      the guts of Xt.

   non_fake_input_wait_mask == same as input_wait_mask but minus the
			       signal event pipe.  Also only used in
			       event-tty.c.

   process_only_mask == only the process descriptors.

   tty_only_mask == only the TTY/stream console descriptors.
   */
SELECT_TYPE input_wait_mask, non_fake_input_wait_mask;
SELECT_TYPE process_only_mask, tty_only_mask;

/* This is used to terminate the select(), when an event came in
   through a signal (e.g. window-change or C-g on controlling TTY). */
int signal_event_pipe[2];

int signal_event_pipe_initialized;

int fake_event_occurred;

int
read_event_from_tty_or_stream_desc(Lisp_Event * event,
				   struct console *con, int fd)
{
	unsigned char ch;
	int nread;
	Lisp_Object console;

	XSETCONSOLE(console, con);

	nread = read(fd, &ch, 1);
	if (nread <= 0) {
		/* deleting the console might not be safe right now ... */
		enqueue_magic_eval_event(io_error_delete_console, console);
		/* but we definitely need to unselect it to avoid infinite
		   loops reading EOF's */
		Fconsole_disable_input(console);
	} else {
		character_to_event(ch, event, con, 1, 1);
		event->channel = console;
		return 1;
	}
	return 0;
}

void signal_fake_event(void)
{
	char byte = 0;
	/* We do the write always.  Formerly I tried to "optimize" this
	   by setting a flag indicating whether we're blocking and only
	   doing the write in that case, but there is a race condition
	   if the signal occurs after we've checked for the signal
	   occurrence (which could occur in many places throughout
	   an iteration of the command loop, e.g. in status_notify()),
	   but before we set the blocking flag.

	   This should be OK as long as write() is reentrant, which
	   I'm fairly sure it is since it's a system call. */

	if (signal_event_pipe_initialized)
		/* In case a signal comes through while we're dumping */
	{
		int old_errno = errno;
		write(signal_event_pipe[1], &byte, 1);
		errno = old_errno;
	}
}

void drain_signal_event_pipe(void)
{
	char chars[128];
	/* The input end of the pipe has been set to non-blocking. */
	while (read(signal_event_pipe[0], chars, sizeof(chars)) > 0) ;
}

int event_stream_unixoid_select_console(struct console *con)
{
	int infd;

	if (CONSOLE_STREAM_P(con))
		infd = fileno(CONSOLE_STREAM_DATA(con)->in);
	else {
		assert(CONSOLE_TTY_P(con));
		infd = CONSOLE_TTY_DATA(con)->infd;
	}

	assert(infd >= 0);

	FD_SET(infd, &input_wait_mask);
	FD_SET(infd, &non_fake_input_wait_mask);
	FD_SET(infd, &tty_only_mask);
	return infd;
}

int event_stream_unixoid_unselect_console(struct console *con)
{
	int infd;

	if (CONSOLE_STREAM_P(con))
		infd = fileno(CONSOLE_STREAM_DATA(con)->in);
	else {
		assert(CONSOLE_TTY_P(con));
		infd = CONSOLE_TTY_DATA(con)->infd;
	}

	assert(infd >= 0);

	FD_CLR(infd, &input_wait_mask);
	FD_CLR(infd, &non_fake_input_wait_mask);
	FD_CLR(infd, &tty_only_mask);
	return infd;
}

static int get_process_infd(Lisp_Process * p)
{
	Lisp_Object instr, outstr;
	get_process_streams(p, &instr, &outstr);
	assert(!NILP(instr));

	return Lstream_get_fd(XLSTREAM(instr));
}

int event_stream_unixoid_select_process(Lisp_Process * proc)
{
	int infd = get_process_infd(proc);

	FD_SET(infd, &input_wait_mask);
	FD_SET(infd, &non_fake_input_wait_mask);
	FD_SET(infd, &process_only_mask);
	return infd;
}

int event_stream_unixoid_unselect_process(Lisp_Process * proc)
{
	int infd = get_process_infd(proc);

	FD_CLR(infd, &input_wait_mask);
	FD_CLR(infd, &non_fake_input_wait_mask);
	FD_CLR(infd, &process_only_mask);
	return infd;
}

int poll_fds_for_input(SELECT_TYPE mask)
{
	EMACS_TIME sometime;
	EMACS_SELECT_TIME select_time;
	SELECT_TYPE temp_mask;
	int retval;

	while (1) {
		EMACS_SET_SECS_USECS(sometime, 0, 0);
		EMACS_TIME_TO_SELECT_TIME(sometime, select_time);
		temp_mask = mask;
		/* To effect a poll, tell select() to block for zero seconds. */
		retval = select(MAXDESC, &temp_mask, 0, 0, &select_time);
		if (retval >= 0)
			return retval;
		if (errno != EINTR) {
			/* Something went seriously wrong; don't abort since maybe
			   the TTY just died at the wrong time. */
			stderr_out("sxemacs: select failed: errno = %d\n",
				   errno);
			return 0;
		}
		/* else, we got interrupted by a signal, so try again. */
	}

	RETURN_NOT_REACHED(0)	/* not reached */
}

/****************************************************************************/
/*     Unixoid (file descriptors based) process I/O streams routines        */
/****************************************************************************/

USID
event_stream_unixoid_create_stream_pair(void *inhandle, void *outhandle,
					Lisp_Object * instream,
					Lisp_Object * outstream, int flags)
{
	int infd, outfd;
	/* Decode inhandle and outhandle. Their meaning depends on
	   the process implementation being used. */
#if defined (HAVE_UNIX_PROCESSES)
	/* We are passed plain old file descs */
	infd = (EMACS_INT)inhandle;
	outfd = (EMACS_INT)outhandle;
#else
# error Which processes do you have?
#endif

	*instream = (infd >= 0 ? make_filedesc_input_stream(infd, 0, -1, 0)
		     : Qnil);

	*outstream = (outfd >= 0
		      ? make_filedesc_output_stream(outfd, 0, -1,
						    LSTR_BLOCKED_OK)
		      : Qnil);

#if defined(HAVE_UNIX_PROCESSES) && defined(HAVE_PTYS)
	/* FLAGS is process->pty_flag for UNIX_PROCESSES */
	if ((flags & STREAM_PTY_FLUSHING) && outfd >= 0) {
		Bufbyte eof_char = get_eof_char(outfd);
		int pty_max_bytes = get_pty_max_bytes(outfd);
		filedesc_stream_set_pty_flushing(XLSTREAM(*outstream),
						 pty_max_bytes, eof_char);
	}
#endif

	return FD_TO_USID(infd);
}

USID
event_stream_unixoid_delete_stream_pair(Lisp_Object instream,
					Lisp_Object outstream)
{
	int in = -1;
	int out = -1;

	if (!NILP(instream))
		in = Lstream_get_fd(XLSTREAM(instream));

	if (!NILP(outstream))
		out = Lstream_get_fd(XLSTREAM(outstream));

	if (in >= 0)
		close(in);
	if (out != in && out >= 0)
		close(out);

	return FD_TO_USID(in);
}

void init_event_unixoid(void)
{
	/* Do this first; the init_event_*_late() functions
	   pay attention to it. */
	if (pipe(signal_event_pipe) < 0) {
		perror("SXEmacs: can't open pipe");
		exit(-1);
	}
	signal_event_pipe_initialized = 1;

	/* Set it non-blocking so we can drain its output. */
	set_descriptor_non_blocking(signal_event_pipe[0]);

	/* Also set the write descriptor non-blocking so we don't
	   hang in case a long time passes between times when
	   we drain the pipe. */
	set_descriptor_non_blocking(signal_event_pipe[1]);

	/* WARNING: In order for the signal-event pipe to work correctly
	   and not cause lockups, the following need to be followed:

	   1) event_pending_p() must ignore input on the signal-event pipe.
	   2) As soon as next_event() notices input on the signal-event
	   pipe, it must drain it. */
	FD_ZERO(&input_wait_mask);
	FD_ZERO(&non_fake_input_wait_mask);
	FD_ZERO(&process_only_mask);
	FD_ZERO(&tty_only_mask);

	FD_SET(signal_event_pipe[0], &input_wait_mask);
}
