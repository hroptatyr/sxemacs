/* The event_stream interface for tty's.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995 Ben Wing.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Not in FSF. */

#include <config.h>
#include "lisp.h"

#include "device.h"
#include "console-tty.h"
#include "events.h"
#include "frame.h"
#include "process.h"

#include "sysproc.h"
#include "syswait.h"
#include "systime.h"

/* Mask of bits indicating the descriptors that we wait for input on */
extern SELECT_TYPE input_wait_mask, non_fake_input_wait_mask;
extern SELECT_TYPE process_only_mask, tty_only_mask;

static struct event_stream *tty_event_stream;


/************************************************************************/
/*				timeout events				*/
/************************************************************************/

/* The pending timers are stored in an ordered list, where the first timer
   on the list is the first one to fire.  Times recorded here are
   absolute. */
static struct low_level_timeout *tty_timer_queue;

static int
emacs_tty_add_timeout (EMACS_TIME thyme)
{
  return add_low_level_timeout (&tty_timer_queue, thyme);
}

static void
emacs_tty_remove_timeout (int id)
{
  remove_low_level_timeout (&tty_timer_queue, id);
}

static void
tty_timeout_to_emacs_event (Lisp_Event *emacs_event)
{
  emacs_event->event_type = timeout_event;
  /* timeout events have nil as channel */
  emacs_event->timestamp  = 0; /* #### */
  emacs_event->event.timeout.interval_id =
    pop_low_level_timeout (&tty_timer_queue, 0);
  emacs_event->event.timeout.function = Qnil;
  emacs_event->event.timeout.object = Qnil;
}



static int
emacs_tty_event_pending_p (int user_p)
{
  if (!user_p)
    {
      EMACS_TIME sometime;
      /* see if there's a pending timeout. */
      EMACS_GET_TIME (sometime);
      if (tty_timer_queue &&
	  EMACS_TIME_EQUAL_OR_GREATER (sometime, tty_timer_queue->time))
	return 1;
    }

  return poll_fds_for_input (user_p ? tty_only_mask :
			     non_fake_input_wait_mask);
}

struct console *
tty_find_console_from_fd (int fd)
{
  Lisp_Object concons;

  CONSOLE_LOOP (concons)
    {
      struct console *c;

      c = XCONSOLE (XCAR (concons));
      if (CONSOLE_TTY_P (c) && CONSOLE_TTY_DATA (c)->infd == fd)
	return c;
    }

  return 0;
}

static void
emacs_tty_next_event (Lisp_Event *emacs_event)
{
  while (1)
    {
      int ndesc;
      int i;
      SELECT_TYPE temp_mask = input_wait_mask;
      EMACS_TIME time_to_block;
      EMACS_SELECT_TIME select_time_to_block, *pointer_to_this;

      if (!get_low_level_timeout_interval (tty_timer_queue, &time_to_block))
	/* no timer events; block indefinitely */
 	pointer_to_this = 0;
      else
	{
	  EMACS_TIME_TO_SELECT_TIME (time_to_block, select_time_to_block);
	  pointer_to_this = &select_time_to_block;
	}

      ndesc = select (MAXDESC, &temp_mask, 0, 0, pointer_to_this);
      if (ndesc > 0)
	{
	  /* Look for a TTY event */
	  for (i = 0; i < MAXDESC; i++)
	    {
	      /* To avoid race conditions (among other things, an infinite
		 loop when called from Fdiscard_input()), we must return
		 user events ahead of process events. */
	      if (FD_ISSET (i, &temp_mask) && FD_ISSET (i, &tty_only_mask))
		{
		  struct console *c = tty_find_console_from_fd (i);

		  assert (c);
		  if (read_event_from_tty_or_stream_desc (emacs_event, c, i))
		    return;
		}
	    }

	  /* Look for a process event */
	  for (i = 0; i < MAXDESC; i++)
	    {
	      if (FD_ISSET (i, &temp_mask) && FD_ISSET (i, &process_only_mask))
		{
		  Lisp_Object process;
		  Lisp_Process *p = get_process_from_usid (FD_TO_USID(i));

		  assert (p);
		  XSETPROCESS (process, p);
		  emacs_event->event_type = process_event;
		  /* process events have nil as channel */
		  emacs_event->timestamp  = 0; /* #### */
		  emacs_event->event.process.process = process;
		  return;
		}
	    }

	  /* We might get here when a fake event came through a signal. */
	  /* Return a dummy event, so that a cycle of the command loop will
	     occur. */
	  drain_signal_event_pipe ();
	  emacs_event->event_type = eval_event;
	  /* eval events have nil as channel */
	  emacs_event->event.eval.function = Qidentity;
	  emacs_event->event.eval.object = Qnil;
	  return;
	}
      else if (ndesc == 0) /* timeout fired */
	{
	  tty_timeout_to_emacs_event (emacs_event);
	  return;
	}
    }
}

static void
emacs_tty_handle_magic_event (Lisp_Event *emacs_event)
{
  /* Nothing to do currently */
}


static void
emacs_tty_select_process (Lisp_Process *process)
{
  event_stream_unixoid_select_process (process);
}

static void
emacs_tty_unselect_process (Lisp_Process *process)
{
  event_stream_unixoid_unselect_process (process);
}

static void
emacs_tty_select_console (struct console *con)
{
  event_stream_unixoid_select_console (con);
}

static void
emacs_tty_unselect_console (struct console *con)
{
  event_stream_unixoid_unselect_console (con);
}

static void
emacs_tty_quit_p (void)
{
  /* Nothing to do currently because QUIT is handled through SIGINT.
     This could change. */
}

static USID
emacs_tty_create_stream_pair (void* inhandle, void* outhandle,
		Lisp_Object* instream, Lisp_Object* outstream, int flags)
{
  return event_stream_unixoid_create_stream_pair
		(inhandle, outhandle, instream, outstream, flags);
}

static USID
emacs_tty_delete_stream_pair (Lisp_Object instream, Lisp_Object outstream)
{
  return event_stream_unixoid_delete_stream_pair (instream, outstream);
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
reinit_vars_of_event_tty (void)
{
  tty_event_stream = xnew (struct event_stream);

  tty_event_stream->event_pending_p 	= emacs_tty_event_pending_p;
  tty_event_stream->force_event_pending = 0;
  tty_event_stream->next_event_cb	= emacs_tty_next_event;
  tty_event_stream->handle_magic_event_cb = emacs_tty_handle_magic_event;
  tty_event_stream->add_timeout_cb 	= emacs_tty_add_timeout;
  tty_event_stream->remove_timeout_cb 	= emacs_tty_remove_timeout;
  tty_event_stream->select_console_cb 	= emacs_tty_select_console;
  tty_event_stream->unselect_console_cb = emacs_tty_unselect_console;
  tty_event_stream->select_process_cb 	= emacs_tty_select_process;
  tty_event_stream->unselect_process_cb = emacs_tty_unselect_process;
  tty_event_stream->quit_p_cb		= emacs_tty_quit_p;
  tty_event_stream->create_stream_pair_cb = emacs_tty_create_stream_pair;
  tty_event_stream->delete_stream_pair_cb = emacs_tty_delete_stream_pair;
}

void
vars_of_event_tty (void)
{
  reinit_vars_of_event_tty ();
}

void
init_event_tty_late (void)
{
  event_stream = tty_event_stream;
}
