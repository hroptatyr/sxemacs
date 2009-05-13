/* The portable interface to event streams.
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

/* Authorship:

   Created 1991 by Jamie Zawinski.
   A great deal of work over the ages by Ben Wing (Mule-ization for 19.12,
     device abstraction for 19.12/19.13, async timers for 19.14,
     rewriting of focus code for 19.12, pre-idle hook for 19.12,
     redoing of signal and quit handling for 19.9 and 19.12,
     misc-user events to clean up menu/scrollbar handling for 19.11,
     function-key-map/key-translation-map/keyboard-translate-table for
     19.13/19.14, open-dribble-file for 19.13, much other cleanup).
   focus-follows-mouse from Chuck Thompson, 1995.
   XIM stuff by Martin Buchholz, c. 1996?.
*/

/* This file has been Mule-ized. */

/*
 *	DANGER!!
 *
 *	If you ever change ANYTHING in this file, you MUST run the
 *	testcases at the end to make sure that you haven't changed
 *	the semantics of recent-keys, last-input-char, or keyboard
 *	macros.  You'd be surprised how easy it is to break this.
 *
 */

/* TODO:
   This stuff is way too hard to maintain - needs rework.

   The command builder should deal only with key and button events.
   Other command events should be able to come in the MIDDLE of a key
   sequence, without disturbing the key sequence composition, or the
   command builder structure representing it.

   Someone should rethink universal-argument and figure out how an
   arbitrary command can influence the next command (universal-argument
   or universal-coding-system-argument) or the next key (hyperify).

   Both C-h and Help in the middle of a key sequence should trigger
   prefix-help-command.  help-char is stupid.  Maybe we need
   keymap-of-last-resort?

   After prefix-help is run, one should be able to CONTINUE TYPING,
   instead of RETYPING, the key sequence.
 */

#include <config.h>
#include "lisp.h"

#include "blocktype.h"
#include "buffer.h"
#include "commands.h"
#include "device.h"
#include "elhash.h"
#define INCLUDE_EVENTS_H_PRIVATE_SPHERE
#include "events.h"
#include "frame.h"
#include "insdel.h"		/* for buffer_reset_changes */
#include "keymap.h"
#include "lstream.h"
#include "macros.h"		/* for defining_keyboard_macro */
#include "menubar.h"		/* #### for evil kludges. */
#include "process.h"
#include "window.h"

#include "sysdep.h"		/* init_poll_for_quit() */
#include "syssignal.h"		/* SIGCHLD, etc. */
#include "sysfile.h"
#include "systime.h"		/* to set Vlast_input_time */

#include "events-mod.h"
#ifdef EF_USE_ASYNEQ
#include "event-queue.h"
#include "workers.h"
#include "worker-asyneq.h"
#endif
#ifdef FILE_CODING
#include "file-coding.h"
#endif

#include <errno.h>

/* The number of keystrokes between auto-saves. */
static Fixnum auto_save_interval;

Lisp_Object Qundefined_keystroke_sequence;

Lisp_Object Qcommand_event_p;

/* Hooks to run before and after each command.  */
Lisp_Object Vpre_command_hook, Vpost_command_hook;
Lisp_Object Qpre_command_hook, Qpost_command_hook;

/* See simple.el */
Lisp_Object Qhandle_pre_motion_command, Qhandle_post_motion_command;

/* Hook run when SXEmacs is about to be idle. */
Lisp_Object Qpre_idle_hook, Vpre_idle_hook;

/* Control gratuitous keyboard focus throwing. */
int focus_follows_mouse;

/* When true, modifier keys are sticky. */
int modifier_keys_are_sticky;
/* Modifier keys are sticky for this many milliseconds. */
Lisp_Object Vmodifier_keys_sticky_time;

/* Here FSF Emacs 20.7 defines Vpost_command_idle_hook,
   post_command_idle_delay, Vdeferred_action_list, and
   Vdeferred_action_function, but we don't because that stuff is crap,
   and we're smarter than them, and their momas are fat. */

/* FSF Emacs 20.7 also defines Vinput_method_function,
   Qinput_method_exit_on_first_char and Qinput_method_use_echo_area.
   I don't know this should be imported or not. */

/* Non-nil disable property on a command means
   do not execute it; call disabled-command-hook's value instead. */
Lisp_Object Qdisabled, Vdisabled_command_hook;

EXFUN(Fnext_command_event, 2);

static void pre_command_hook(void);
static void post_command_hook(void);

/* Last keyboard or mouse input event read as a command. */
Lisp_Object Vlast_command_event;

/* The nearest ASCII equivalent of the above. */
Lisp_Object Vlast_command_char;

/* Last keyboard or mouse event read for any purpose. */
Lisp_Object Vlast_input_event;

/* The nearest ASCII equivalent of the above. */
Lisp_Object Vlast_input_char;

Lisp_Object Vcurrent_mouse_event;

/* This is fbound in cmdloop.el, see the commentary there */
Lisp_Object Qcancel_mode_internal;

/* If not Qnil, event objects to be read as the next command input */
Lisp_Object Vunread_command_events;
Lisp_Object Vunread_command_event;	/* obsoleteness support */

static Lisp_Object Qunread_command_events, Qunread_command_event;

/* Previous command, represented by a Lisp object.
   Does not include prefix commands and arg setting commands. */
Lisp_Object Vlast_command;

/* Contents of this-command-properties for the last command. */
Lisp_Object Vlast_command_properties;

/* If a command sets this, the value goes into
   last-command for the next command. */
Lisp_Object Vthis_command;

/* If a command sets this, the value goes into
   last-command-properties for the next command. */
Lisp_Object Vthis_command_properties;

/* The value of point when the last command was executed.  */
Bufpos last_point_position;

/* The frame that was current when the last command was started. */
Lisp_Object Vlast_selected_frame;

/* The buffer that was current when the last command was started.  */
Lisp_Object last_point_position_buffer;

/* A (16bit . 16bit) representation of the time of the last-command-event. */
Lisp_Object Vlast_input_time;

/* A (16bit 16bit usec) representation of the time
   of the last-command-event. */
Lisp_Object Vlast_command_event_time;

/* Character to recognize as the help char.  */
Lisp_Object Vhelp_char;

/* Form to execute when help char is typed.  */
Lisp_Object Vhelp_form;

/* Command to run when the help character follows a prefix key.  */
Lisp_Object Vprefix_help_command;

/* Flag to tell QUIT that some interesting occurrence (e.g. a keypress)
   may have happened. */
volatile int something_happened;

/* Hash table to translate keysyms through */
Lisp_Object Vkeyboard_translate_table;

/* If control-meta-super-shift-X is undefined, try control-meta-super-x */
Lisp_Object Vretry_undefined_key_binding_unshifted;
Lisp_Object Qretry_undefined_key_binding_unshifted;

#ifdef HAVE_XIM
/* If composed input is undefined, use self-insert-char */
Lisp_Object Vcomposed_character_default_binding;
#endif				/* HAVE_XIM */

/* Console that corresponds to our controlling terminal */
Lisp_Object Vcontrolling_terminal;

/* An event (actually an event chain linked through event_next) or Qnil.
 */
Lisp_Object Vthis_command_keys;
Lisp_Object Vthis_command_keys_tail;

/* #### kludge! */
Lisp_Object Qauto_show_make_point_visible;

/* File in which we write all commands we read; an lstream */
static Lisp_Object Vdribble_file;

/* Recent keys ring location; a vector of events or nil-s */
Lisp_Object Vrecent_keys_ring;
int recent_keys_ring_size;
int recent_keys_ring_index;

/* Boolean specifying whether keystrokes should be added to
   recent-keys. */
int inhibit_input_event_recording;

Lisp_Object Qself_insert_defer_undo;

/* this is in keymap.c */
extern Lisp_Object Fmake_keymap(Lisp_Object name);

#ifdef DEBUG_SXEMACS
Fixnum debug_emacs_events;

static void
external_debugging_print_event(char *event_description, Lisp_Object event)
{
	write_c_string("(", Qexternal_debugging_output);
	write_c_string(event_description, Qexternal_debugging_output);
	write_c_string(") ", Qexternal_debugging_output);
	print_internal(event, Qexternal_debugging_output, 1);
	write_c_string("\n", Qexternal_debugging_output);
}

#define DEBUG_PRINT_EMACS_EVENT(event_description, event) do {	\
  if (debug_emacs_events)					\
    external_debugging_print_event (event_description, event);	\
} while (0)
#else
#define DEBUG_PRINT_EMACS_EVENT(string, event)
#endif

/* The callback routines for the window system or terminal driver */
struct event_stream *event_stream;

static void echo_key_event(struct command_builder *, Lisp_Object event);
static void maybe_kbd_translate(Lisp_Object event);

#if defined(EF_USE_ASYNEQ)
/* everybody may use me */
event_queue_t asyneq = Qnull_pointer;
static Lisp_Object Vasyneq;
#define EQ_EMPTY_P()	eq_queue_empty_p(asyneq)
#define EQ_LARGE_P()	(eq_queue_size(asyneq) > 1)
#else
/* This structure is basically a typeahead queue: things like
   wait-reading-process-output will delay the execution of
   keyboard and mouse events by pushing them here.

   Chained through event_next()
   command_event_queue_tail is a pointer to the last-added element.
 */
static Lisp_Object command_event_queue;
static Lisp_Object command_event_queue_tail;
#define EQ_EMPTY_P()	NILP(command_event_queue)
#define EQ_LARGE_P()	!NILP(command_event_queue_tail)
#endif

/* Nonzero means echo unfinished commands after this many seconds of pause. */
static Lisp_Object Vecho_keystrokes;

/* The number of keystrokes since the last auto-save. */
static int keystrokes_since_auto_save;

/* Used by the C-g signal handler so that it will never "hard quit"
   when waiting for an event.  Otherwise holding down C-g could
   cause a suspension back to the shell, which is generally
   undesirable. (#### This doesn't fully work.) */

int emacs_is_blocking;

/* Handlers which run during sit-for, sleep-for and accept-process-output
   are not allowed to recursively call these routines.  We record here
   if we are in that situation. */

static Lisp_Object recursive_sit_for;

/**********************************************************************/
/*                       Command-builder object                       */
/**********************************************************************/

#define XCOMMAND_BUILDER(x) \
  XRECORD (x, command_builder, struct command_builder)
#define XSETCOMMAND_BUILDER(x, p) XSETRECORD (x, p, command_builder)
#define COMMAND_BUILDERP(x) RECORDP (x, command_builder)
#define CHECK_COMMAND_BUILDER(x) CHECK_RECORD (x, command_builder)

static Lisp_Object mark_command_builder(Lisp_Object obj)
{
	struct command_builder *builder = XCOMMAND_BUILDER(obj);
	mark_object(builder->prefix_events);
	mark_object(builder->current_events);
	mark_object(builder->most_current_event);
	mark_object(builder->last_non_munged_event);
	mark_object(builder->munge_me[0].first_mungeable_event);
	mark_object(builder->munge_me[1].first_mungeable_event);
	return builder->console;
}

static void finalize_command_builder(void *header, int for_disksave)
{
	if (!for_disksave) {
		xfree(((struct command_builder *)header)->echo_buf);
		((struct command_builder *)header)->echo_buf = 0;
	}
}

DEFINE_LRECORD_IMPLEMENTATION("command-builder", command_builder,
			      mark_command_builder, internal_object_printer,
			      finalize_command_builder, 0, 0, 0,
			      struct command_builder);

static void reset_command_builder_event_chain(struct command_builder *builder)
{
	builder->prefix_events = Qnil;
	builder->current_events = Qnil;
	builder->most_current_event = Qnil;
	builder->last_non_munged_event = Qnil;
	builder->munge_me[0].first_mungeable_event = Qnil;
	builder->munge_me[1].first_mungeable_event = Qnil;
}

Lisp_Object allocate_command_builder(Lisp_Object console)
{
	Lisp_Object builder_obj;
	struct command_builder *builder =
	    alloc_lcrecord_type(struct command_builder,
				&lrecord_command_builder);

	builder->console = console;
	reset_command_builder_event_chain(builder);
	builder->echo_buf_length = 300;	/* #### Kludge */
	builder->echo_buf =
		xnew_atomic_array(Bufbyte, builder->echo_buf_length);
	builder->echo_buf[0] = 0;
	builder->echo_buf_index = -1;
	builder->echo_buf_index = -1;
	builder->self_insert_countdown = 0;

	XSETCOMMAND_BUILDER(builder_obj, builder);
	return builder_obj;
}

static void
command_builder_append_event(struct command_builder *builder, Lisp_Object event)
{
	assert(EVENTP(event));

	if (EVENTP(builder->most_current_event))
		XSET_EVENT_NEXT(builder->most_current_event, event);
	else
		builder->current_events = event;

	builder->most_current_event = event;
	if (NILP(builder->munge_me[0].first_mungeable_event))
		builder->munge_me[0].first_mungeable_event = event;
	if (NILP(builder->munge_me[1].first_mungeable_event))
		builder->munge_me[1].first_mungeable_event = event;
}

/**********************************************************************/
/*             Low-level interfaces onto event methods                */
/**********************************************************************/

enum event_stream_operation {
	EVENT_STREAM_PROCESS,
	EVENT_STREAM_TIMEOUT,
	EVENT_STREAM_CONSOLE,
	EVENT_STREAM_READ
};

static void check_event_stream_ok(enum event_stream_operation op)
{
	if (!event_stream && noninteractive) {
		switch (op) {
		case EVENT_STREAM_PROCESS:
			error("Can't start subprocesses in -batch mode");
		case EVENT_STREAM_TIMEOUT:
			error("Can't add timeouts in -batch mode");
		case EVENT_STREAM_CONSOLE:
			error("Can't add consoles in -batch mode");
		case EVENT_STREAM_READ:
			error("Can't read events in -batch mode");
		default:
			abort();
		}
	} else if (!event_stream) {
		error
		    ("event-stream callbacks not initialized (internal error?)");
	}
}

static int event_stream_event_pending_p(int user)
{
	return event_stream && event_stream->event_pending_p(user);
}

static void event_stream_force_event_pending(struct frame *f)
{
	if (event_stream->force_event_pending)
		event_stream->force_event_pending(f);
}

static int maybe_read_quit_event(Lisp_Event * event)
{
	/* A C-g that came from `sigint_happened' will always come from the
	   controlling terminal.  If that doesn't exist, however, then the
	   user manually sent us a SIGINT, and we pretend the C-g came from
	   the selected console. */
	struct console *con;

	if (CONSOLEP(Vcontrolling_terminal) &&
	    CONSOLE_LIVE_P(XCONSOLE(Vcontrolling_terminal))) {
		con = XCONSOLE(Vcontrolling_terminal);
	} else {
		Lisp_Object tmp = Fselected_console();
		con = XCONSOLE(tmp);
	}

	if (sigint_happened) {
		int ch = CONSOLE_QUIT_CHAR(con);
		sigint_happened = 0;
		Vquit_flag = Qnil;
		character_to_event(ch, event, con, 1, 1);
		event->channel = make_console(con);
		return 1;
	}
	return 0;
}

void event_stream_next_event(Lisp_Event * event)
{
	Lisp_Object event_obj;

	check_event_stream_ok(EVENT_STREAM_READ);

	XSETEVENT(event_obj, event);
	zero_event(event);
	/* If C-g was pressed, treat it as a character to be read.
	   Note that if C-g was pressed while we were blocking,
	   the SIGINT signal handler will be called.  It will
	   set Vquit_flag and write a byte on our "fake pipe",
	   which will unblock us. */
	if (maybe_read_quit_event(event)) {
		DEBUG_PRINT_EMACS_EVENT("SIGINT", event_obj);
		return;
	}

	/* If a longjmp() happens in the callback, we're screwed.
	   Let's hope it doesn't.  I think the code here is fairly
	   clean and doesn't do this. */
	emacs_is_blocking = 1;
	event_stream->next_event_cb(event);
	emacs_is_blocking = 0;

#ifdef DEBUG_SXEMACS
	/* timeout events have more info set later, so
	   print the event out in next_event_internal(). */
	if (event->event_type != timeout_event)
		DEBUG_PRINT_EMACS_EVENT("real", event_obj);
#endif
	maybe_kbd_translate(event_obj);
}

void event_stream_handle_magic_event(Lisp_Event * event)
{
	check_event_stream_ok(EVENT_STREAM_READ);
	event_stream->handle_magic_event_cb(event);
}

static int event_stream_add_timeout(EMACS_TIME timeout)
{
	check_event_stream_ok(EVENT_STREAM_TIMEOUT);
	return event_stream->add_timeout_cb(timeout);
}

static void event_stream_remove_timeout(int id)
{
	check_event_stream_ok(EVENT_STREAM_TIMEOUT);
	event_stream->remove_timeout_cb(id);
}

void event_stream_select_console(struct console *con)
{
	check_event_stream_ok(EVENT_STREAM_CONSOLE);
	if (!con->input_enabled) {
		event_stream->select_console_cb(con);
		con->input_enabled = 1;
	}
}

void event_stream_unselect_console(struct console *con)
{
	check_event_stream_ok(EVENT_STREAM_CONSOLE);
	if (con->input_enabled) {
		event_stream->unselect_console_cb(con);
		con->input_enabled = 0;
	}
}

void event_stream_select_process(Lisp_Process * proc)
{
	check_event_stream_ok(EVENT_STREAM_PROCESS);
	if (!get_process_selected_p(proc)) {
		event_stream->select_process_cb(proc);
		set_process_selected_p(proc, 1);
	}
}

void event_stream_unselect_process(Lisp_Process * proc)
{
	check_event_stream_ok(EVENT_STREAM_PROCESS);
	if (get_process_selected_p(proc)) {
		event_stream->unselect_process_cb(proc);
		set_process_selected_p(proc, 0);
	}
}

USID
event_stream_create_stream_pair(void *inhandle, void *outhandle,
				Lisp_Object * instream, Lisp_Object * outstream,
				int flags)
{
	check_event_stream_ok(EVENT_STREAM_PROCESS);
	return event_stream->create_stream_pair_cb
	    (inhandle, outhandle, instream, outstream, flags);
}

USID
event_stream_delete_stream_pair(Lisp_Object instream, Lisp_Object outstream)
{
	check_event_stream_ok(EVENT_STREAM_PROCESS);
	return event_stream->delete_stream_pair_cb(instream, outstream);
}

void event_stream_quit_p(void)
{
	if (event_stream)
		event_stream->quit_p_cb();
}

static int event_stream_current_event_timestamp(struct console *c)
{
	if (event_stream && event_stream->current_event_timestamp_cb)
		return event_stream->current_event_timestamp_cb(c);
	else
		return 0;
}

/**********************************************************************/
/*                      Character prompting                           */
/**********************************************************************/

static void
echo_key_event(struct command_builder *command_builder, Lisp_Object event)
{
	/* This function can GC */
	char buf[255];
	Bytecount buf_index = command_builder->echo_buf_index;
	Bufbyte *e;
	Bytecount len;

	if (buf_index < 0) {
		buf_index = 0;	/* We're echoing now */
		clear_echo_area(selected_frame(), Qnil, 0);
	}

	format_event_object(buf, XEVENT(event), 1);
	len = strlen(buf);

	if (len + buf_index + 4 > command_builder->echo_buf_length)
		return;
	e = command_builder->echo_buf + buf_index;
	memcpy(e, buf, len);
	e += len;

	e[0] = ' ';
	e[1] = '-';
	e[2] = ' ';
	e[3] = 0;

	command_builder->echo_buf_index = buf_index + len + 1;
}

static void
regenerate_echo_keys_from_this_command_keys(struct command_builder *builder)
{
	Lisp_Object event;

	builder->echo_buf_index = 0;

	EVENT_CHAIN_LOOP(event, Vthis_command_keys)
	    echo_key_event(builder, event);
}

static void
maybe_echo_keys(struct command_builder *command_builder, int no_snooze)
{
	/* This function can GC */
	double echo_keystrokes;
	struct frame *f = selected_frame();
	/* Message turns off echoing unless more keystrokes turn it on again. */
	if (echo_area_active(f) && !EQ(Qcommand, echo_area_status(f)))
		return;

	if (INTP(Vecho_keystrokes) || FLOATP(Vecho_keystrokes))
		echo_keystrokes = extract_float(Vecho_keystrokes);
	else
		echo_keystrokes = 0;

	if (minibuf_level == 0 && echo_keystrokes > 0.0
#if defined (HAVE_X_WINDOWS) && defined (LWLIB_MENUBARS_LUCID)
	    && !x_kludge_lw_menu_active()
#endif
	    ) {
		if (!no_snooze) {
			/* #### C-g here will cause QUIT.  Setting dont_check_for_quit
			   doesn't work.  See check_quit. */
			if (NILP(Fsit_for(Vecho_keystrokes, Qnil)))
				/* input came in, so don't echo. */
				return;
		}

		echo_area_message(f, command_builder->echo_buf, Qnil, 0,
				  /* not echo_buf_index.  That doesn't include
				     the terminating " - ". */
				  strlen((char *)command_builder->echo_buf),
				  Qcommand);
	}
}

static void
reset_key_echo(struct command_builder *command_builder,
	       int remove_echo_area_echo)
{
	/* This function can GC */
	struct frame *f = selected_frame();

	if (command_builder)
		command_builder->echo_buf_index = -1;

	if (remove_echo_area_echo)
		clear_echo_area(f, Qcommand, 0);
}

/**********************************************************************/
/*                          random junk                               */
/**********************************************************************/

static void maybe_kbd_translate(Lisp_Object event)
{
	Emchar c;
	int did_translate = 0;

	if (XEVENT_TYPE(event) != key_press_event)
		return;
	if (!HASH_TABLEP(Vkeyboard_translate_table))
		return;
	if (EQ(Fhash_table_count(Vkeyboard_translate_table), Qzero))
		return;

	c = event_to_character(XEVENT(event), 0, 0, 0);
	if (c != -1) {
		Lisp_Object traduit =
		    Fgethash(make_char(c), Vkeyboard_translate_table,
			     Qnil);
		if (!NILP(traduit) && SYMBOLP(traduit)) {
			XEVENT(event)->event.key.keysym = traduit;
			XEVENT(event)->event.key.modifiers = 0;
			did_translate = 1;
		} else if (CHARP(traduit)) {
			Lisp_Event ev2;

			/* This used to call Fcharacter_to_event() directly into EVENT,
			   but that can eradicate timestamps and other such stuff.
			   This way is safer. */
			zero_event(&ev2);
			character_to_event(XCHAR(traduit), &ev2,
					   XCONSOLE(EVENT_CHANNEL
						    (XEVENT(event))), 0, 1);
			XEVENT(event)->event.key.keysym = ev2.event.key.keysym;
			XEVENT(event)->event.key.modifiers =
			    ev2.event.key.modifiers;
			did_translate = 1;
		}
	}

	if (!did_translate) {
		Lisp_Object traduit = Fgethash(XEVENT(event)->event.key.keysym,
					       Vkeyboard_translate_table, Qnil);
		if (!NILP(traduit) && SYMBOLP(traduit)) {
			XEVENT(event)->event.key.keysym = traduit;
			did_translate = 1;
		} else if (CHARP(traduit)) {
			Lisp_Event ev2;

			zero_event(&ev2);
			character_to_event(XCHAR(traduit), &ev2,
					   XCONSOLE(EVENT_CHANNEL
						    (XEVENT(event))), 0, 1);
			XEVENT(event)->event.key.keysym = ev2.event.key.keysym;
			XEVENT(event)->event.key.modifiers |=
			    ev2.event.key.modifiers;
			did_translate = 1;
		}
	}
#ifdef DEBUG_SXEMACS
	if (did_translate)
		DEBUG_PRINT_EMACS_EVENT("->keyboard-translate-table", event);
#endif
}

/* NB: The following auto-save stuff is in keyboard.c in FSFmacs, and
   keystrokes_since_auto_save is equivalent to the difference between
   num_nonmacro_input_chars and last_auto_save. */

/* When an auto-save happens, record the number of keystrokes, and
   don't do again soon.  */

void record_auto_save(void)
{
	keystrokes_since_auto_save = 0;
}

/* Make an auto save happen as soon as possible at command level.  */

void force_auto_save_soon(void)
{
	keystrokes_since_auto_save = 1 + max(auto_save_interval, 20);
}

static void maybe_do_auto_save(void)
{
	/* This function can call lisp */
	keystrokes_since_auto_save++;
	if (auto_save_interval > 0 &&
	    keystrokes_since_auto_save > max(auto_save_interval, 20) &&
	    !detect_input_pending()) {
		Fdo_auto_save(Qnil, Qnil);
		record_auto_save();
	}
}

static Lisp_Object print_help(Lisp_Object object)
{
	Fprinc(object, Qnil);
	return Qnil;
}

static void
execute_help_form(struct command_builder *command_builder, Lisp_Object event)
{
	/* This function can GC */
	Lisp_Object help = Qnil;
	int speccount = specpdl_depth();
	Bytecount buf_index = command_builder->echo_buf_index;
	Lisp_Object echo = ((buf_index <= 0)
			    ? Qnil : make_string(command_builder->echo_buf,
						 buf_index));
	struct gcpro gcpro1, gcpro2;
	GCPRO2(echo, help);

	record_unwind_protect(save_window_excursion_unwind,
			      Fcurrent_window_configuration(Qnil));
	reset_key_echo(command_builder, 1);

	help = Feval(Vhelp_form);
	if (STRINGP(help))
		internal_with_output_to_temp_buffer(build_string("*Help*"),
						    print_help, help, Qnil);
	Fnext_command_event(event, Qnil);
	/* Remove the help from the frame */
	unbind_to(speccount, Qnil);
	/* Hmmmm.  Tricky.  The unbind restores an old window configuration,
	   apparently bypassing any setting of windows_structure_changed.
	   So we need to set it so that things get redrawn properly. */
	/* #### This is massive overkill.  Look at doing it better once the
	   new redisplay is fully in place. */
	{
		Lisp_Object frmcons, devcons, concons;
		FRAME_LOOP_NO_BREAK(frmcons, devcons, concons) {
			struct frame *f = XFRAME(XCAR(frmcons));
			MARK_FRAME_WINDOWS_STRUCTURE_CHANGED(f);
		}
	}

	redisplay();
	if (event_matches_key_specifier_p(XEVENT(event), make_char(' '))) {
		/* Discard next key if it is a space */
		reset_key_echo(command_builder, 1);
		Fnext_command_event(event, Qnil);
	}

	command_builder->echo_buf_index = buf_index;
	if (buf_index > 0)
		memcpy(command_builder->echo_buf, XSTRING_DATA(echo), buf_index + 1);	/* terminating 0 */
	UNGCPRO;
}

/**********************************************************************/
/*                          input pending                             */
/**********************************************************************/

int detect_input_pending(void)
{
	/* Always call the event_pending_p hook even if there's an unread
	   character, because that might do some needed ^G detection (on
	   systems without SIGIO, for example).
	 */
	if (event_stream_event_pending_p(1))
		return 1;
	if (!NILP(Vunread_command_events) || !NILP(Vunread_command_event))
		return 1;
	if (!EQ_EMPTY_P()) {
		Lisp_Object event;

#if defined(EF_USE_ASYNEQ)
		EQ_TRAVERSE(
			asyneq, event,
			if (XEVENT_TYPE(event) != eval_event &&
			    XEVENT_TYPE(event) != magic_eval_event) {
				RETURN_FROM_EQ_TRAVERSE(asyneq, 1);
			});
#else
		EVENT_CHAIN_LOOP(event, command_event_queue) {
			if (XEVENT_TYPE(event) != eval_event
			    && XEVENT_TYPE(event) != magic_eval_event)
				return 1;
		}
#endif
	}
	return 0;
}

DEFUN("input-pending-p", Finput_pending_p, 0, 0, 0,	/*
Return t if command input is currently available with no waiting.
Actually, the value is nil only if we can be sure that no input is available.
							 */
      ())
{
	return detect_input_pending()? Qt : Qnil;
}

/**********************************************************************/
/*                            timeouts                                */
/**********************************************************************/

/**** Low-level timeout functions. ****

   These functions maintain a sorted list of one-shot timeouts (where
   the timeouts are in absolute time).  They are intended for use by
   functions that need to convert a list of absolute timeouts into a
   series of intervals to wait for. */

/* We ensure that 0 is never a valid ID, so that a value of 0 can be
   used to indicate an absence of a timer. */
static int low_level_timeout_id_tick;

static struct low_level_timeout_blocktype {
	Blocktype_declare(struct low_level_timeout);
} *the_low_level_timeout_blocktype;

/* Add a one-shot timeout at time TIME to TIMEOUT_LIST.  Return
   a unique ID identifying the timeout. */

int
add_low_level_timeout(struct low_level_timeout **timeout_list, EMACS_TIME thyme)
{
	struct low_level_timeout *tm;
	struct low_level_timeout *t, **tt;

	/* Allocate a new time struct. */

	tm = Blocktype_alloc(the_low_level_timeout_blocktype);
	tm->next = NULL;
	if (low_level_timeout_id_tick == 0)
		low_level_timeout_id_tick++;
	tm->id = low_level_timeout_id_tick++;
	tm->time = thyme;

	/* Add it to the queue. */

	tt = timeout_list;
	t = *tt;
	while (t && EMACS_TIME_EQUAL_OR_GREATER(tm->time, t->time)) {
		tt = &t->next;
		t = *tt;
	}
	tm->next = t;
	*tt = tm;

	return tm->id;
}

/* Remove the low-level timeout identified by ID from TIMEOUT_LIST.
   If the timeout is not there, do nothing. */

void remove_low_level_timeout(struct low_level_timeout **timeout_list, int id)
{
	struct low_level_timeout *t, *prev;

	/* find it */

	for (t = *timeout_list, prev = NULL; t && t->id != id; t = t->next)
		prev = t;

	if (!t)
		return;		/* couldn't find it */

	if (!prev)
		*timeout_list = t->next;
	else
		prev->next = t->next;

	Blocktype_free(the_low_level_timeout_blocktype, t);
}

/* If there are timeouts on TIMEOUT_LIST, store the relative time
   interval to the first timeout on the list into INTERVAL and
   return 1.  Otherwise, return 0. */

int
get_low_level_timeout_interval(struct low_level_timeout *timeout_list,
			       EMACS_TIME * interval)
{
	if (!timeout_list)	/* no timer events; block indefinitely */
		return 0;
	else {
		EMACS_TIME current_time;

		/* The time to block is the difference between the first
		   (earliest) timer on the queue and the current time.
		   If that is negative, then the timer will fire immediately
		   but we still have to call select(), with a zero-valued
		   timeout: user events must have precedence over timer events. */
		EMACS_GET_TIME(current_time);
		if (EMACS_TIME_GREATER(timeout_list->time, current_time))
			EMACS_SUB_TIME(*interval, timeout_list->time,
				       current_time);
		else
			EMACS_SET_SECS_USECS(*interval, 0, 0);
		return 1;
	}
}

/* Pop the first (i.e. soonest) timeout off of TIMEOUT_LIST and return
   its ID.  Also, if TIME_OUT is not 0, store the absolute time of the
   timeout into TIME_OUT. */

int
pop_low_level_timeout(struct low_level_timeout **timeout_list,
		      EMACS_TIME * time_out)
{
	struct low_level_timeout *tm = *timeout_list;
	int id;

	assert(tm);
	id = tm->id;
	if (time_out)
		*time_out = tm->time;
	*timeout_list = tm->next;
	Blocktype_free(the_low_level_timeout_blocktype, tm);
	return id;
}

/**** High-level timeout functions. ****/

static int timeout_id_tick;

static Lisp_Object pending_timeout_list, pending_async_timeout_list;

#if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
static Lisp_Object Vtimeout_free_list;
#endif	/* !BDWGC */

static Lisp_Object mark_timeout(Lisp_Object obj)
{
	Lisp_Timeout *tm = XTIMEOUT(obj);
	mark_object(tm->function);
	return tm->object;
}

/* Should never, ever be called. (except by an external debugger) */
static void
print_timeout(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	const Lisp_Timeout *t = XTIMEOUT(obj);
	char buf[64];

	sprintf(buf, "#<INTERNAL OBJECT (SXEmacs bug?) (timeout) 0x%lx>",
		(unsigned long)t);
	write_c_string(buf, printcharfun);
}

static const struct lrecord_description timeout_description[] = {
	{XD_LISP_OBJECT, offsetof(Lisp_Timeout, function)},
	{XD_LISP_OBJECT, offsetof(Lisp_Timeout, object)},
	{XD_END}
};

DEFINE_LRECORD_IMPLEMENTATION("timeout", timeout,
			      mark_timeout, print_timeout,
			      0, 0, 0, timeout_description, Lisp_Timeout);

/* Generate a timeout and return its ID. */

int
event_stream_generate_wakeup(unsigned int milliseconds,
			     unsigned int vanilliseconds,
			     Lisp_Object function, Lisp_Object object,
			     int async_p)
{
#if defined HAVE_BDWGC && defined EF_USE_BDWGC
	Lisp_Object op = wrap_object(
		alloc_lcrecord(sizeof(Lisp_Timeout), &lrecord_timeout));
#else  /* !BDWGC */
	Lisp_Object op = allocate_managed_lcrecord(Vtimeout_free_list);
#endif	/* BDWGC */
	Lisp_Timeout *timeout = XTIMEOUT(op);
	EMACS_TIME current_time;
	EMACS_TIME interval;

	timeout->id = timeout_id_tick++;
	timeout->resignal_msecs = vanilliseconds;
	timeout->function = function;
	timeout->object = object;

	EMACS_GET_TIME(current_time);
	EMACS_SET_SECS_USECS(interval, milliseconds / 1000,
			     1000 * (milliseconds % 1000));
	EMACS_ADD_TIME(timeout->next_signal_time, current_time, interval);

	if (async_p) {
		timeout->interval_id =
		    event_stream_add_async_timeout(timeout->next_signal_time);
		pending_async_timeout_list = noseeum_cons(op,
							  pending_async_timeout_list);
	} else {
		timeout->interval_id =
		    event_stream_add_timeout(timeout->next_signal_time);
		pending_timeout_list = noseeum_cons(op, pending_timeout_list);
	}
	return timeout->id;
}

/* Given the INTERVAL-ID of a timeout just signalled, resignal the timeout
   as necessary and return the timeout's ID and function and object slots.

   This should be called as a result of receiving notice that a timeout
   has fired.  INTERVAL-ID is *not* the timeout's ID, but is the ID that
   identifies this particular firing of the timeout.  INTERVAL-ID's and
   timeout ID's are in separate number spaces and bear no relation to
   each other.  The INTERVAL-ID is all that the event callback routines
   work with: they work only with one-shot intervals, not with timeouts
   that may fire repeatedly.

   NOTE: The returned FUNCTION and OBJECT are *not* GC-protected at all.
*/

static int
event_stream_resignal_wakeup(int interval_id, int async_p,
			     Lisp_Object * function, Lisp_Object * object)
{
	Lisp_Object op = Qnil, rest;
	Lisp_Timeout *timeout;
	Lisp_Object *timeout_list;
	struct gcpro gcpro1;
	int id;

	GCPRO1(op);		/* just in case ...  because it's removed from the list
				   for awhile. */

	timeout_list =
	    async_p ? &pending_async_timeout_list : &pending_timeout_list;

	/* Find the timeout on the list of pending ones. */
	LIST_LOOP(rest, *timeout_list) {
		timeout = XTIMEOUT(XCAR(rest));
		if (timeout->interval_id == interval_id)
			break;
	}

	assert(!NILP(rest));
	op = XCAR(rest);
	timeout = XTIMEOUT(op);
	/* We make sure to snarf the data out of the timeout object before
	   we free it with free_managed_lcrecord(). */
	id = timeout->id;
	*function = timeout->function;
	*object = timeout->object;

	/* Remove this one from the list of pending timeouts */
	*timeout_list = delq_no_quit_and_free_cons(op, *timeout_list);

	/* If this timeout wants to be resignalled, do it now. */
	if (timeout->resignal_msecs) {
		EMACS_TIME current_time;
		EMACS_TIME interval;

		/* Determine the time that the next resignalling should occur.
		   We do that by adding the interval time to the last signalled
		   time until we get a time that's current.

		   (This way, it doesn't matter if the timeout was signalled
		   exactly when we asked for it, or at some time later.)
		 */
		EMACS_GET_TIME(current_time);
		EMACS_SET_SECS_USECS(interval, timeout->resignal_msecs / 1000,
				     1000 * (timeout->resignal_msecs % 1000));
		do {
			EMACS_ADD_TIME(timeout->next_signal_time,
				       timeout->next_signal_time, interval);
		} while (EMACS_TIME_GREATER
			 (current_time, timeout->next_signal_time));

		if (async_p) {
			timeout->interval_id =
				event_stream_add_async_timeout(
					timeout->next_signal_time);
		} else {
			timeout->interval_id =
				event_stream_add_timeout(
					timeout->next_signal_time);
		}
		/* Add back onto the list.  Note that the effect of this
		   is to move frequently-hit timeouts to the front of the
		   list, which is a good thing. */
		*timeout_list = noseeum_cons(op, *timeout_list);
	} else {
#if defined HAVE_BDWGC && defined EF_USE_BDWGC
		xfree(op);
#else  /* !BDWGC */
		free_managed_lcrecord(Vtimeout_free_list, op);
#endif	/* BDWGC */
	}
	UNGCPRO;
	return id;
}

void event_stream_disable_wakeup(int id, int async_p)
{
	Lisp_Timeout *timeout = 0;
	Lisp_Object rest;
	Lisp_Object *timeout_list;

	if (async_p) {
		timeout_list = &pending_async_timeout_list;
	} else {
		timeout_list = &pending_timeout_list;
	}
	/* Find the timeout on the list of pending ones, if it's still there. */
	LIST_LOOP(rest, *timeout_list) {
		timeout = XTIMEOUT(XCAR(rest));
		if (timeout->id == id) {
			break;
		}
	}

	/* If we found it, remove it from the list and disable the pending
	   one-shot. */
	if (!NILP(rest)) {
		Lisp_Object op = XCAR(rest);
		*timeout_list = delq_no_quit_and_free_cons(op, *timeout_list);
		if (async_p) {
			event_stream_remove_async_timeout(timeout->interval_id);
		} else {
			event_stream_remove_timeout(timeout->interval_id);
		}
#if defined HAVE_BDWGC && defined EF_USE_BDWGC
		xfree(op);
#else  /* !BDWGC */
		free_managed_lcrecord(Vtimeout_free_list, op);
#endif	/* BDWGC */
	}
}

static int event_stream_wakeup_pending_p(int id, int async_p)
{
	Lisp_Timeout *timeout;
	Lisp_Object rest;
	Lisp_Object timeout_list;
	int found = 0;

	if (async_p)
		timeout_list = pending_async_timeout_list;
	else
		timeout_list = pending_timeout_list;

	/* Find the element on the list of pending ones, if it's still there. */
	LIST_LOOP(rest, timeout_list) {
		timeout = XTIMEOUT(XCAR(rest));
		if (timeout->id == id) {
			found = 1;
			break;
		}
	}

	return found;
}

/**** Asynch. timeout functions (see also signal.c) ****/

#if !defined (SIGIO) && !defined (DONT_POLL_FOR_QUIT)
extern int poll_for_quit_id;
#endif

#if defined(HAVE_UNIX_PROCESSES) && !defined(SIGCHLD)
extern int poll_for_sigchld_id;
#endif

void event_stream_deal_with_async_timeout(int interval_id)
{
	/* This function can GC */
	Lisp_Object humpty, dumpty;
#if ((!defined (SIGIO) && !defined (DONT_POLL_FOR_QUIT)) \
     || defined(HAVE_UNIX_PROCESSES) && !defined(SIGCHLD))
	int id =
#endif
	    event_stream_resignal_wakeup(interval_id, 1, &humpty, &dumpty);

#if !defined (SIGIO) && !defined (DONT_POLL_FOR_QUIT)
	if (id == poll_for_quit_id) {
		quit_check_signal_happened = 1;
		quit_check_signal_tick_count++;
		return;
	}
#endif

#if defined(HAVE_UNIX_PROCESSES) && !defined(SIGCHLD)
	if (id == poll_for_sigchld_id) {
		kick_status_notify();
		return;
	}
#endif

	/* call1 GC-protects its arguments */
	call1_trapping_errors("Error in asynchronous timeout callback",
			      humpty, dumpty);
}

/**** Lisp-level timeout functions. ****/

static unsigned long lisp_number_to_milliseconds(Lisp_Object secs, int allow_0)
{
#if defined(WITH_NUMBER_TYPES)
	double fsecs;
	CHECK_NUMBER(secs);
	fsecs = extract_float(secs);
#else  /* !WITH_NUMBER_TYPES */
#ifdef HAVE_FPFLOAT
	double fsecs;
	CHECK_INT_OR_FLOAT(secs);
	fsecs = XFLOATINT(secs);
#else
	long fsecs;
	CHECK_INT(secs);
	fsecs = XINT(secs);
#endif	/* HAVE_FPFLOAT */
#endif	/* WITH_NUMBER_TYPES */
	if (fsecs < 0)
		signal_simple_error("timeout is negative", secs);
	if (!allow_0 && fsecs == 0)
		signal_simple_error("timeout is non-positive", secs);
	if (fsecs >= (((unsigned int)0xFFFFFFFF) / 1000))
		signal_simple_error
		    ("timeout would exceed 32 bits when represented in milliseconds",
		     secs);

	return (unsigned long)(1000 * fsecs);
}

DEFUN("add-timeout", Fadd_timeout, 3, 4, 0,	/*
Add a timeout, to be signaled after the timeout period has elapsed.
SECS is a number of seconds, expressed as an integer or a float.
FUNCTION will be called after that many seconds have elapsed, with one
argument, the given OBJECT.  If the optional RESIGNAL argument is provided,
then after this timeout expires, `add-timeout' will automatically be called
again with RESIGNAL as the first argument.

This function returns an object which is the id number of this particular
timeout.  You can pass that object to `disable-timeout' to turn off the
timeout before it has been signalled.

NOTE: Id numbers as returned by this function are in a distinct namespace
from those returned by `add-async-timeout'.  This means that the same id
number could refer to a pending synchronous timeout and a different pending
asynchronous timeout, and that you cannot pass an id from `add-timeout'
to `disable-async-timeout', or vice-versa.

The number of seconds may be expressed as a floating-point number, in which
case some fractional part of a second will be used.  Caveat: the usable
timeout granularity will vary from system to system.

Adding a timeout causes a timeout event to be returned by `next-event', and
the function will be invoked by `dispatch-event,' so if emacs is in a tight
loop, the function will not be invoked until the next call to sit-for or
until the return to top-level (the same is true of process filters).

If you need to have a timeout executed even when SXEmacs is in the midst of
running Lisp code, use `add-async-timeout'.

WARNING: if you are thinking of calling add-timeout from inside of a
callback function as a way of resignalling a timeout, think again.  There
is a race condition.  That's why the RESIGNAL argument exists.
						 */
      (secs, function, object, resignal))
{
	unsigned long msecs = lisp_number_to_milliseconds(secs, 0);
	unsigned long msecs2 = (NILP(resignal) ? 0 :
				lisp_number_to_milliseconds(resignal, 0));
	int id;
	Lisp_Object lid;
	id = event_stream_generate_wakeup(msecs, msecs2, function, object, 0);
	lid = make_int(id);
	if (id != XINT(lid))
		abort();
	return lid;
}

DEFUN("disable-timeout", Fdisable_timeout, 1, 1, 0,	/*
Disable a timeout from signalling any more.
ID should be a timeout id number as returned by `add-timeout'.  If ID
corresponds to a one-shot timeout that has already signalled, nothing
will happen.

It will not work to call this function on an id number returned by
`add-async-timeout'.  Use `disable-async-timeout' for that.
							 */
      (id))
{
	CHECK_INT(id);
	event_stream_disable_wakeup(XINT(id), 0);
	return Qnil;
}

DEFUN("add-async-timeout", Fadd_async_timeout, 3, 4, 0,	/*
Add an asynchronous timeout, to be signaled after an interval has elapsed.
SECS is a number of seconds, expressed as an integer or a float.
FUNCTION will be called after that many seconds have elapsed, with one
argument, the given OBJECT.  If the optional RESIGNAL argument is provided,
then after this timeout expires, `add-async-timeout' will automatically be
called again with RESIGNAL as the first argument.

This function returns an object which is the id number of this particular
timeout.  You can pass that object to `disable-async-timeout' to turn off
the timeout before it has been signalled.

NOTE: Id numbers as returned by this function are in a distinct namespace
from those returned by `add-timeout'.  This means that the same id number
could refer to a pending synchronous timeout and a different pending
asynchronous timeout, and that you cannot pass an id from
`add-async-timeout' to `disable-timeout', or vice-versa.

The number of seconds may be expressed as a floating-point number, in which
case some fractional part of a second will be used.  Caveat: the usable
timeout granularity will vary from system to system.

Adding an asynchronous timeout causes the function to be invoked as soon
as the timeout occurs, even if SXEmacs is in the midst of executing some
other code. (This is unlike the synchronous timeouts added with
`add-timeout', where the timeout will only be signalled when SXEmacs is
waiting for events, i.e. the next return to top-level or invocation of
`sit-for' or related functions.) This means that the function that is
called *must* not signal an error or change any global state (e.g. switch
buffers or windows) except when locking code is in place to make sure
that race conditions don't occur in the interaction between the
asynchronous timeout function and other code.

Under most circumstances, you should use `add-timeout' instead, as it is
much safer.  Asynchronous timeouts should only be used when such behavior
is really necessary.

Asynchronous timeouts are blocked and will not occur when `inhibit-quit'
is non-nil.  As soon as `inhibit-quit' becomes nil again, any pending
asynchronous timeouts will get called immediately. (Multiple occurrences
of the same asynchronous timeout are not queued, however.) While the
callback function of an asynchronous timeout is invoked, `inhibit-quit'
is automatically bound to non-nil, and thus other asynchronous timeouts
will be blocked unless the callback function explicitly sets `inhibit-quit'
to nil.

WARNING: if you are thinking of calling `add-async-timeout' from inside of a
callback function as a way of resignalling a timeout, think again.  There
is a race condition.  That's why the RESIGNAL argument exists.
							 */
      (secs, function, object, resignal))
{
	unsigned long msecs = lisp_number_to_milliseconds(secs, 0);
	unsigned long msecs2 = (NILP(resignal) ? 0 :
				lisp_number_to_milliseconds(resignal, 0));
	int id;
	Lisp_Object lid;
	id = event_stream_generate_wakeup(msecs, msecs2, function, object, 1);
	lid = make_int(id);
	if (id != XINT(lid))
		abort();
	return lid;
}

DEFUN("disable-async-timeout", Fdisable_async_timeout, 1, 1, 0,	/*
Disable an asynchronous timeout from signalling any more.
ID should be a timeout id number as returned by `add-async-timeout'.  If ID
corresponds to a one-shot timeout that has already signalled, nothing
will happen.

It will not work to call this function on an id number returned by
`add-timeout'.  Use `disable-timeout' for that.
								 */
      (id))
{
	CHECK_INT(id);
	event_stream_disable_wakeup(XINT(id), 1);
	return Qnil;
}

/**********************************************************************/
/*                    enqueuing and dequeuing events                  */
/**********************************************************************/

/* Add an event to the back of the command-event queue: it will be the next
   event read after all pending events.   This only works on keyboard,
   mouse-click, misc-user, and eval events.
 */
static void enqueue_command_event(Lisp_Object event)
{
#ifdef EF_USE_ASYNEQ
	eq_enqueue(asyneq, event);
#else
	enqueue_event(event, &command_event_queue, &command_event_queue_tail);
#endif
}

static Lisp_Object dequeue_command_event(void)
{
#ifdef EF_USE_ASYNEQ
	return eq_dequeue(asyneq);
#else
	return dequeue_event(&command_event_queue, &command_event_queue_tail);
#endif
}

/* put the event on the typeahead queue, unless
   the event is the quit char, in which case the `QUIT'
   which will occur on the next trip through this loop is
   all the processing we should do - leaving it on the queue
   would cause the quit to be processed twice.
   */
static void enqueue_command_event_1(Lisp_Object event_to_copy)
{
	/* do not call check_quit() here.  Vquit_flag was set in
	   next_event_internal. */
	if (NILP(Vquit_flag))
		enqueue_command_event(Fcopy_event(event_to_copy, Qnil));
}

void enqueue_magic_eval_event(void (*fun) (Lisp_Object), Lisp_Object object)
{
	Lisp_Object event = Fmake_event(Qnil, Qnil);

	XEVENT(event)->event_type = magic_eval_event;
	/* channel for magic_eval events is nil */
	XEVENT(event)->event.magic_eval.internal_function = fun;
	XEVENT(event)->event.magic_eval.object = object;
	enqueue_command_event(event);
}

DEFUN("enqueue-eval-event", Fenqueue_eval_event, 2, 2, 0,	/*
Add an eval event to the back of the eval event queue.
When this event is dispatched, FUNCTION (which should be a function
of one argument) will be called with OBJECT as its argument.
See `next-event' for a description of event types and how events
are received.
								 */
      (function, object))
{
	Lisp_Object event = Fmake_event(Qnil, Qnil);

	XEVENT(event)->event_type = eval_event;
	/* channel for eval events is nil */
	XEVENT(event)->event.eval.function = function;
	XEVENT(event)->event.eval.object = object;
	enqueue_command_event(event);

	return event;
}

Lisp_Object
enqueue_misc_user_event(Lisp_Object channel, Lisp_Object function,
			Lisp_Object object)
{
	Lisp_Object event = Fmake_event(Qnil, Qnil);

	XEVENT(event)->event_type = misc_user_event;
	XEVENT(event)->channel = channel;
	XEVENT(event)->event.misc.function = function;
	XEVENT(event)->event.misc.object = object;
	XEVENT(event)->event.misc.button = 0;
	XEVENT(event)->event.misc.modifiers = 0;
	XEVENT(event)->event.misc.x = -1;
	XEVENT(event)->event.misc.y = -1;
	enqueue_command_event(event);

	return event;
}

Lisp_Object
enqueue_misc_user_event_pos(Lisp_Object channel, Lisp_Object function,
			    Lisp_Object object,
			    int button, int modifiers, int x, int y)
{
	Lisp_Object event = Fmake_event(Qnil, Qnil);

	XEVENT(event)->event_type = misc_user_event;
	XEVENT(event)->channel = channel;
	XEVENT(event)->event.misc.function = function;
	XEVENT(event)->event.misc.object = object;
	XEVENT(event)->event.misc.button = button;
	XEVENT(event)->event.misc.modifiers = modifiers;
	XEVENT(event)->event.misc.x = x;
	XEVENT(event)->event.misc.y = y;
	enqueue_command_event(event);

	return event;
}

/**********************************************************************/
/*                       focus-event handling                         */
/**********************************************************************/

/*

Ben's capsule lecture on focus:

In FSFmacs `select-frame' never changes the window-manager frame
focus.  All it does is change the "selected frame".  This is similar
to what happens when we call `select-device' or `select-console'.
Whenever an event comes in (including a keyboard event), its frame is
selected; therefore, evaluating `select-frame' in *scratch* won't
cause any effects because the next received event (in the same frame)
will cause a switch back to the frame displaying *scratch*.

Whenever a focus-change event is received from the window manager, it
generates a `switch-frame' event, which causes the Lisp function
`handle-switch-frame' to get run.  This basically just runs
`select-frame' (see below, however).

In FSFmacs, if you want to have an operation run when a frame is
selected, you supply an event binding for `switch-frame' (and then
maybe call `handle-switch-frame', or something ...).

In SXEmacs, we *do* change the window-manager frame focus as a result
of `select-frame', but not until the next time an event is received,
so that a function that momentarily changes the selected frame won't
cause WM focus flashing. (#### There's something not quite right here;
this is causing the wrong-cursor-focus problems that you occasionally
see.  But the general idea is correct.) This approach is winning for
people who use the explicit-focus model, but is trickier to implement.

We also don't make the `switch-frame' event visible but instead have
`select-frame-hook', which is a better approach.

There is the problem of surrogate minibuffers, where when we enter the
minibuffer, you essentially want to temporarily switch the WM focus to
the frame with the minibuffer, and switch it back when you exit the
minibuffer.

FSFmacs solves this with the crockish `redirect-frame-focus', which
says "for keyboard events received from FRAME, act like they're
coming from FOCUS-FRAME".  I think what this means is that, when
a keyboard event comes in and the event manager is about to select the
event's frame, if that frame has its focus redirected, the redirected-to
frame is selected instead.  That way, if you're in a minibufferless
frame and enter the minibuffer, then all Lisp functions that run see
the selected frame as the minibuffer's frame rather than the minibufferless
frame you came from, so that (e.g.) your typing actually appears in
the minibuffer's frame and things behave sanely.

There's also some weird logic that switches the redirected frame focus
from one frame to another if Lisp code explicitly calls `select-frame'
\(but not if `handle-switch-frame' is called), and saves and restores
the frame focus in window configurations, etc. etc.  All of this logic
is heavily #if 0'd, with lots of comments saying "No, this approach
doesn't seem to work, so I'm trying this ...  is it reasonable?
Well, I'm not sure ..." that are a red flag indicating crockishness.

Because of our way of doing things, we can avoid all this crock.
Keyboard events never cause a select-frame (who cares what frame
they're associated with?  They come from a console, only).  We change
the actual WM focus to a surrogate minibuffer frame, so we don't have
to do any internal redirection.  In order to get the focus back,
I took the approach in minibuf.el of just checking to see if the
frame we moved to is still the selected frame, and move back to the
old one if so.  Conceivably we might have to do the weird "tracking"
that FSFmacs does when `select-frame' is called, but I don't think
so.  If the selected frame moved from the minibuffer frame, then
we just leave it there, figuring that someone knows what they're
doing.  Because we don't have any redirection recorded anywhere,
it's safe to do this, and we don't end up with unwanted redirection.

*/

static void run_select_frame_hook(void)
{
	run_hook(Qselect_frame_hook);
}

static void run_deselect_frame_hook(void)
{
	run_hook(Qdeselect_frame_hook);
}

/* When select-frame is called and focus_follows_mouse is false, we want
   to tell the window system that the focus should be changed to point to
   the new frame.  However,
   sometimes Lisp functions will temporarily change the selected frame
   (e.g. to call a function that operates on the selected frame),
   and it's annoying if this focus-change happens exactly when
   select-frame is called, because then you get some flickering of the
   window-manager border and perhaps other undesirable results.  We
   really only want to change the focus when we're about to retrieve
   an event from the user.  To do this, we keep track of the frame
   where the window-manager focus lies on, and just before waiting
   for user events, check the currently selected frame and change
   the focus as necessary.

   On the other hand, if focus_follows_mouse is true, we need to switch the
   selected frame back to the frame with window manager focus just before we
   execute the next command in Fcommand_loop_1, just as the selected buffer is
   reverted after a set-buffer.

   Both cases are handled by this function.  It must be called as appropriate
   from these two places, depending on the value of focus_follows_mouse. */

void investigate_frame_change(void)
{
	Lisp_Object devcons, concons;

	/* if the selected frame was changed, change the window-system
	   focus to the new frame.  We don't do it when select-frame was
	   called, to avoid flickering and other unwanted side effects when
	   the frame is just changed temporarily. */
	DEVICE_LOOP_NO_BREAK(devcons, concons) {
		struct device *d = XDEVICE(XCAR(devcons));
		Lisp_Object sel_frame = DEVICE_SELECTED_FRAME(d);

		/* You'd think that maybe we should use FRAME_WITH_FOCUS_REAL,
		   but that can cause us to end up in an infinite loop focusing
		   between two frames.  It seems that since the call to `select-frame'
		   in emacs_handle_focus_change_final() is based on the _FOR_HOOKS
		   value, we need to do so too. */
		if (!NILP(sel_frame) &&
		    !EQ(DEVICE_FRAME_THAT_OUGHT_TO_HAVE_FOCUS(d), sel_frame) &&
		    !NILP(DEVICE_FRAME_WITH_FOCUS_FOR_HOOKS(d)) &&
		    !EQ(DEVICE_FRAME_WITH_FOCUS_FOR_HOOKS(d), sel_frame)) {
			/* At this point, we know that the frame has been changed.  Now, if
			 * focus_follows_mouse is not set, we finish off the frame change,
			 * so that user events will now come from the new frame.  Otherwise,
			 * if focus_follows_mouse is set, no gratuitous frame changing
			 * should take place.  Set the focus back to the frame which was
			 * originally selected for user input.
			 */
			if (!focus_follows_mouse) {
				/* prevent us from issuing the same request more than once */
				DEVICE_FRAME_THAT_OUGHT_TO_HAVE_FOCUS(d) =
				    sel_frame;
				MAYBE_DEVMETH(d, focus_on_frame,
					      (XFRAME(sel_frame)));
			} else {
				Lisp_Object old_frame = Qnil;

				/* #### Do we really want to check OUGHT ??
				 * It seems to make sense, though I have never seen us
				 * get here and have it be non-nil.
				 */
				if (FRAMEP
				    (DEVICE_FRAME_THAT_OUGHT_TO_HAVE_FOCUS(d)))
					old_frame =
					    DEVICE_FRAME_THAT_OUGHT_TO_HAVE_FOCUS
					    (d);
				else if (FRAMEP
					 (DEVICE_FRAME_WITH_FOCUS_FOR_HOOKS(d)))
					old_frame =
					    DEVICE_FRAME_WITH_FOCUS_FOR_HOOKS
					    (d);

				/* #### Can old_frame ever be NIL?  play it safe.. */
				if (!NILP(old_frame)) {
					/* Fselect_frame is not really the right thing: it frobs the
					 * buffer stack.  But there's no easy way to do the right
					 * thing, and this code already had this problem anyway.
					 */
					Fselect_frame(old_frame);
				}
			}
		}
	}
}

static Lisp_Object cleanup_after_missed_defocusing(Lisp_Object frame)
{
	if (FRAMEP(frame) && FRAME_LIVE_P(XFRAME(frame)))
		Fselect_frame(frame);
	return Qnil;
}

void emacs_handle_focus_change_preliminary(Lisp_Object frame_inp_and_dev)
{
	Lisp_Object frame = Fcar(frame_inp_and_dev);
	Lisp_Object device = Fcar(Fcdr(frame_inp_and_dev));
	int in_p = !NILP(Fcdr(Fcdr(frame_inp_and_dev)));
	struct device *d;

	if (!DEVICE_LIVE_P(XDEVICE(device)))
		return;
	else
		d = XDEVICE(device);

	/* Any received focus-change notifications render invalid any
	   pending focus-change requests. */
	DEVICE_FRAME_THAT_OUGHT_TO_HAVE_FOCUS(d) = Qnil;
	if (in_p) {
		Lisp_Object focus_frame;

		if (!FRAME_LIVE_P(XFRAME(frame)))
			return;
		else
			focus_frame = DEVICE_FRAME_WITH_FOCUS_REAL(d);

		/* Mark the minibuffer as changed to make sure it gets updated
		   properly if the echo area is active. */
		{
			struct window *w =
			    XWINDOW(FRAME_MINIBUF_WINDOW(XFRAME(frame)));
			MARK_WINDOWS_CHANGED(w);
		}

		if (FRAMEP(focus_frame) && FRAME_LIVE_P(XFRAME(focus_frame))
		    && !EQ(frame, focus_frame)) {
			/* Oops, we missed a focus-out event. */
			DEVICE_FRAME_WITH_FOCUS_REAL(d) = Qnil;
			redisplay_redraw_cursor(XFRAME(focus_frame), 1);
		}
		DEVICE_FRAME_WITH_FOCUS_REAL(d) = frame;
		if (!EQ(frame, focus_frame)) {
			redisplay_redraw_cursor(XFRAME(frame), 1);
		}
	} else {
		/* We ignore the frame reported in the event.  If it's different
		   from where we think the focus was, oh well -- we messed up.
		   Nonetheless, we pretend we were right, for sensible behavior. */
		frame = DEVICE_FRAME_WITH_FOCUS_REAL(d);
		if (!NILP(frame)) {
			DEVICE_FRAME_WITH_FOCUS_REAL(d) = Qnil;

			if (FRAME_LIVE_P(XFRAME(frame)))
				redisplay_redraw_cursor(XFRAME(frame), 1);
		}
	}
}

/* Called from the window-system-specific code when we receive a
   notification that the focus lies on a particular frame.
   Argument is a cons: (frame . (device . in-p)) where in-p is non-nil
   for focus-in.
 */
void emacs_handle_focus_change_final(Lisp_Object frame_inp_and_dev)
{
	Lisp_Object frame = Fcar(frame_inp_and_dev);
	Lisp_Object device = Fcar(Fcdr(frame_inp_and_dev));
	int in_p = !NILP(Fcdr(Fcdr(frame_inp_and_dev)));
	struct device *d;
	int count;

	if (!DEVICE_LIVE_P(XDEVICE(device)))
		return;
	else
		d = XDEVICE(device);

	if (in_p) {
		Lisp_Object focus_frame;

		if (!FRAME_LIVE_P(XFRAME(frame)))
			return;
		else
			focus_frame = DEVICE_FRAME_WITH_FOCUS_FOR_HOOKS(d);

		DEVICE_FRAME_WITH_FOCUS_FOR_HOOKS(d) = frame;
		if (FRAMEP(focus_frame) && !EQ(frame, focus_frame)) {
			/* Oops, we missed a focus-out event. */
			Fselect_frame(focus_frame);
			/* Do an unwind-protect in case an error occurs in
			   the deselect-frame-hook */
			count = specpdl_depth();
			record_unwind_protect(cleanup_after_missed_defocusing,
					      frame);
			run_deselect_frame_hook();
			unbind_to(count, Qnil);
			/* the cleanup method changed the focus frame to nil, so
			   we need to reflect this */
			focus_frame = Qnil;
		} else
			Fselect_frame(frame);
		if (!EQ(frame, focus_frame))
			run_select_frame_hook();
	} else {
		/* We ignore the frame reported in the event.  If it's different
		   from where we think the focus was, oh well -- we messed up.
		   Nonetheless, we pretend we were right, for sensible behavior. */
		frame = DEVICE_FRAME_WITH_FOCUS_FOR_HOOKS(d);
		if (!NILP(frame)) {
			DEVICE_FRAME_WITH_FOCUS_FOR_HOOKS(d) = Qnil;
			run_deselect_frame_hook();
		}
	}
}

/**********************************************************************/
/*                      retrieving the next event                     */
/**********************************************************************/

static int in_single_console;

/* #### These functions don't currently do anything. */
void single_console_state(void)
{
	in_single_console = 1;
}

void any_console_state(void)
{
	in_single_console = 0;
}

int in_single_console_state(void)
{
	return in_single_console;
}

/* the number of keyboard characters read.  callint.c wants this. */
Charcount num_input_chars;

static void next_event_internal(Lisp_Object target_event, int allow_queued)
{
	struct gcpro gcpro1;
	/* QUIT;   This is incorrect - the caller must do this because some
	   callers (ie, Fnext_event()) do not want to QUIT. */

	assert(NILP(XEVENT_NEXT(target_event)));

	GCPRO1(target_event);

	/* When focus_follows_mouse is nil, if a frame change took place, we need
	 * to actually switch window manager focus to the selected window now.
	 */
	if (!focus_follows_mouse)
		investigate_frame_change();

	if (allow_queued && !EQ_EMPTY_P()) {
		Lisp_Object event = dequeue_command_event();
		Fcopy_event(event, target_event);
		Fdeallocate_event(event);
		DEBUG_PRINT_EMACS_EVENT("command event queue", target_event);
	} else {
		Lisp_Event *e = XEVENT(target_event);

		/* The command_event_queue was empty.  Wait for an event. */
		event_stream_next_event(e);
		/* If this was a timeout, then we need to extract some data
		   out of the returned closure and might need to resignal
		   it. */
		if (e->event_type == timeout_event) {
			Lisp_Object tristan, isolde;

			e->event.timeout.id_number =
			    event_stream_resignal_wakeup(e->event.timeout.
							 interval_id, 0,
							 &tristan, &isolde);

			e->event.timeout.function = tristan;
			e->event.timeout.object = isolde;
			/* next_event_internal() doesn't print out timeout events
			   because of the extra info we just set. */
			DEBUG_PRINT_EMACS_EVENT("real, timeout", target_event);
		}

		/* If we read a ^G, then set quit-flag but do not discard the ^G.
		   The callers of next_event_internal() will do one of two things:

		   -- set Vquit_flag to Qnil. (next-event does this.) This will
		   cause the ^G to be treated as a normal keystroke.
		   -- not change Vquit_flag but attempt to enqueue the ^G, at
		   which point it will be discarded.  The next time QUIT is
		   called, it will notice that Vquit_flag was set.

		 */
		if (e->event_type == key_press_event &&
		    event_matches_key_specifier_p
		    (e,
		     make_char(CONSOLE_QUIT_CHAR(XCONSOLE(EVENT_CHANNEL(e))))))
		{
			Vquit_flag = Qt;
		}
	}

	UNGCPRO;
}

static void run_pre_idle_hook(void)
{
	if (!NILP(Vpre_idle_hook)
	    && !detect_input_pending())
		safe_run_hook_trapping_errors
		    ("Error in `pre-idle-hook' (setting hook to nil)",
		     Qpre_idle_hook, 1);
}

static void push_this_command_keys(Lisp_Object event);
static void push_recent_keys(Lisp_Object event);
static void dribble_out_event(Lisp_Object event);
static void execute_internal_event(Lisp_Object event);
static int is_scrollbar_event(Lisp_Object event);

DEFUN("next-event", Fnext_event, 0, 2, 0,	/*
Return the next available event.
Pass this object to `dispatch-event' to handle it.
In most cases, you will want to use `next-command-event', which returns
the next available "user" event (i.e. keypress, button-press,
button-release, or menu selection) instead of this function.

If EVENT is non-nil, it should be an event object and will be filled in
and returned; otherwise a new event object will be created and returned.
If PROMPT is non-nil, it should be a string and will be displayed in the
echo area while this function is waiting for an event.

The next available event will be

-- any events in `unread-command-events' or `unread-command-event'; else
-- the next event in the currently executing keyboard macro, if any; else
-- an event queued by `enqueue-eval-event', if any, or any similar event
   queued internally, such as a misc-user event. (For example, when an item
   is selected from a menu or from a `question'-type dialog box, the item's
   callback is not immediately executed, but instead a misc-user event
   is generated and placed onto this queue; when it is dispatched, the
   callback is executed.) Else
-- the next available event from the window system or terminal driver.

In the last case, this function will block until an event is available.

The returned event will be one of the following types:

-- a key-press event.
-- a button-press or button-release event.
-- a misc-user-event, meaning the user selected an item on a menu or used
   the scrollbar.
-- a process event, meaning that output from a subprocess is available.
-- a timeout event, meaning that a timeout has elapsed.
-- an eval event, which simply causes a function to be executed when the
   event is dispatched.  Eval events are generated by `enqueue-eval-event'
   or by certain other conditions happening.
-- a magic event, indicating that some window-system-specific event
   happened (such as a focus-change notification) that must be handled
   synchronously with other events.  `dispatch-event' knows what to do with
   these events.
						*/
      (event, prompt)) {
	/* This function can call lisp */
	/* #### We start out using the selected console before an event
	   is received, for echoing the partially completed command.
	   This is most definitely wrong -- there needs to be a separate
	   echo area for each console! */
	struct console *con = XCONSOLE(Vselected_console);
	struct command_builder *command_builder =
	    XCOMMAND_BUILDER(con->command_builder);
	int store_this_key = 0;
	struct gcpro gcpro1;

	GCPRO1(event);
	/* DO NOT do QUIT anywhere within this function or the functions it calls.
	   We want to read the ^G as an event. */

#ifdef LWLIB_MENUBARS_LUCID
	/*
	 * #### Fix the menu code so this isn't necessary.
	 *
	 * We cannot allow the lwmenu code to be reentered, because the
	 * code is not written to be reentrant and will crash.  Therefore
	 * paths from the menu callbacks back into the menu code have to
	 * be blocked.  Fnext_event is the normal path into the menu code,
	 * so we signal an error here.
	 */
	if (in_menu_callback)
		error("Attempt to call next-event inside menu callback");
#endif				/* LWLIB_MENUBARS_LUCID */

	if (NILP(event))
		event = Fmake_event(Qnil, Qnil);
	else
		CHECK_LIVE_EVENT(event);

	if (!NILP(prompt)) {
		Bytecount len;
		CHECK_STRING(prompt);

		len = XSTRING_LENGTH(prompt);
		if (command_builder->echo_buf_length < len)
			len = command_builder->echo_buf_length - 1;
		memcpy(command_builder->echo_buf, XSTRING_DATA(prompt), len);
		command_builder->echo_buf[len] = 0;
		command_builder->echo_buf_index = len;
		echo_area_message(XFRAME(CONSOLE_SELECTED_FRAME(con)),
				  command_builder->echo_buf,
				  Qnil, 0,
				  command_builder->echo_buf_index, Qcommand);
	}

      start_over_and_avoid_hosage:
	/* If there is something in unread-command-events, simply return it.
	   But do some error checking to make sure the user hasn't put something
	   in the unread-command-events that they shouldn't have.
	   This does not update this-command-keys and recent-keys.
	 */
	if (!NILP(Vunread_command_events)) {
		if (!CONSP(Vunread_command_events)) {
			Vunread_command_events = Qnil;
			signal_error(Qwrong_type_argument,
				     list3(Qconsp, Vunread_command_events,
					   Qunread_command_events));
		} else {
			Lisp_Object e = XCAR(Vunread_command_events);
			Vunread_command_events = XCDR(Vunread_command_events);
			if (!EVENTP(e) || !command_event_p(e))
				signal_error(Qwrong_type_argument,
					     list3(Qcommand_event_p, e,
						   Qunread_command_events));
			redisplay();
			if (!EQ(e, event))
				Fcopy_event(e, event);
			DEBUG_PRINT_EMACS_EVENT("unread-command-events", event);
		}
	}

	/* Do similar for unread-command-event (obsoleteness support). */
	else if (!NILP(Vunread_command_event)) {
		Lisp_Object e = Vunread_command_event;
		Vunread_command_event = Qnil;

		if (!EVENTP(e) || !command_event_p(e)) {
			signal_error(Qwrong_type_argument,
				     list3(Qeventp, e, Qunread_command_event));
		}
		if (!EQ(e, event))
			Fcopy_event(e, event);
		redisplay();
		DEBUG_PRINT_EMACS_EVENT("unread-command-event", event);
	}

	/* If we're executing a keyboard macro, take the next event from that,
	   and update this-command-keys and recent-keys.
	   Note that the unread-command-events take precedence over kbd macros.
	 */
	else {
		if (!NILP(Vexecuting_macro)) {
			redisplay();
			pop_kbd_macro_event(event);	/* This throws past us at
							   end-of-macro. */
			store_this_key = 1;
			DEBUG_PRINT_EMACS_EVENT("keyboard macro", event);
		}
		/* Otherwise, read a real event, possibly from the
		   command_event_queue, and update this-command-keys and
		   recent-keys. */
		else {
			run_pre_idle_hook();
			redisplay();
			next_event_internal(event, 1);
			Vquit_flag = Qnil;	/* Read C-g as an event. */
			store_this_key = 1;
		}
	}

	status_notify();	/* Notice process change */

#ifdef C_ALLOCA
	alloca(0);		/* Cause a garbage collection now */
	/* Since we can free the most stuff here
	 *  (since this is typically called from
	 *  the command-loop top-level). */
#endif				/* C_ALLOCA */

	if (object_dead_p(XEVENT(event)->channel)) {
		/* event_console_or_selected may crash if the channel is dead.
		   Best just to eat it and get the next event. */
		goto start_over_and_avoid_hosage;
	}

	/* OK, now we can stop the selected-console kludge and use the
	   actual console from the event. */
	con = event_console_or_selected(event);
	command_builder = XCOMMAND_BUILDER(con->command_builder);

	switch (XEVENT_TYPE(event)) {
	case button_release_event:
	case misc_user_event:
		/* don't echo menu accelerator keys */
		reset_key_echo(command_builder, 1);
		goto EXECUTE_KEY;
	case button_press_event:
		/* key or mouse input can trigger prompting */
		goto STORE_AND_EXECUTE_KEY;
	case key_press_event:
		/* any key input can trigger autosave */
		break;

		/* just list the other events here */
	case empty_event:
	case pointer_motion_event:
	case process_event:
	case timeout_event:
	case magic_event:
	case magic_eval_event:
	case eval_event:
#ifdef EF_USE_ASYNEQ
	case eaten_myself_event:
	case work_started_event:
	case work_finished_event:
#endif  /* EF_USE_ASYNEQ */
	case dead_event:
	default:
		goto RETURN;
	}

	maybe_do_auto_save();
	num_input_chars++;
      STORE_AND_EXECUTE_KEY:
	if (store_this_key) {
		echo_key_event(command_builder, event);
	}

      EXECUTE_KEY:
	/* Store the last-input-event.  The semantics of this is that it is
	   the thing most recently returned by next-command-event.  It need
	   not have come from the keyboard or a keyboard macro, it may have
	   come from unread-command-events.  It's always a command-event (a
	   key, click, or menu selection), never a motion or process event.
	 */
	if (!EVENTP(Vlast_input_event))
		Vlast_input_event = Fmake_event(Qnil, Qnil);
	if (XEVENT_TYPE(Vlast_input_event) == dead_event) {
		Vlast_input_event = Fmake_event(Qnil, Qnil);
		error("Someone deallocated last-input-event!");
	}
	if (!EQ(event, Vlast_input_event))
		Fcopy_event(event, Vlast_input_event);

	/* last-input-char and last-input-time are derived from
	   last-input-event.
	   Note that last-input-char will never have its high-bit set, in an
	   effort to sidestep the ambiguity between M-x and oslash.
	 */
	Vlast_input_char = Fevent_to_character(Vlast_input_event,
					       Qnil, Qnil, Qnil);
	{
		EMACS_TIME t;
		EMACS_GET_TIME(t);
		if (!CONSP(Vlast_input_time))
			Vlast_input_time = Fcons(Qnil, Qnil);
		XCAR(Vlast_input_time) =
		    make_int((EMACS_SECS(t) >> 16) & 0xffff);
		XCDR(Vlast_input_time) =
		    make_int((EMACS_SECS(t) >> 0) & 0xffff);
		if (!CONSP(Vlast_command_event_time))
			Vlast_command_event_time = list3(Qnil, Qnil, Qnil);
		XCAR(Vlast_command_event_time) =
		    make_int((EMACS_SECS(t) >> 16) & 0xffff);
		XCAR(XCDR(Vlast_command_event_time)) =
		    make_int((EMACS_SECS(t) >> 0) & 0xffff);
		XCAR(XCDR(XCDR(Vlast_command_event_time)))
		    = make_int(EMACS_USECS(t));
	}
	/* If this key came from the keyboard or from a keyboard macro, then
	   it goes into the recent-keys and this-command-keys vectors.
	   If this key came from the keyboard, and we're defining a keyboard
	   macro, then it goes into the macro.
	 */
	if (store_this_key) {
		if (!is_scrollbar_event(event))	/* #### not quite right, see
						   comment in execute_command_event */
			push_this_command_keys(event);
		if (!inhibit_input_event_recording)
			push_recent_keys(event);
		dribble_out_event(event);
		if (!NILP(con->defining_kbd_macro) && NILP(Vexecuting_macro)) {
			if (!EVENTP(command_builder->current_events))
				finalize_kbd_macro_chars(con);
			store_kbd_macro_event(event);
		}
	}
	/* If this is the help char and there is a help form, then execute the
	   help form and swallow this character.  This is the only place where
	   calling Fnext_event() can cause arbitrary lisp code to run.  Note
	   that execute_help_form() calls Fnext_command_event(), which calls
	   this function, as well as Fdispatch_event.
	 */
	if (!NILP(Vhelp_form) &&
	    event_matches_key_specifier_p(XEVENT(event), Vhelp_char))
		execute_help_form(command_builder, event);

      RETURN:
	UNGCPRO;
	return event;
}

DEFUN("next-command-event", Fnext_command_event, 0, 2, 0,	/*
Return the next available "user" event.
Pass this object to `dispatch-event' to handle it.

If EVENT is non-nil, it should be an event object and will be filled in
and returned; otherwise a new event object will be created and returned.
If PROMPT is non-nil, it should be a string and will be displayed in the
echo area while this function is waiting for an event.

The event returned will be a keyboard, mouse press, or mouse release event.
If there are non-command events available (mouse motion, sub-process output,
etc) then these will be executed (with `dispatch-event') and discarded.  This
function is provided as a convenience; it is roughly equivalent to the lisp code

  (while (progn
	   (next-event event prompt)
	   (not (or (key-press-event-p event)
		    (button-press-event-p event)
		    (button-release-event-p event)
		    (misc-user-event-p event))))
    (dispatch-event event))

but it also makes a provision for displaying keystrokes in the echo area.
								 */
      (event, prompt)) {
	/* This function can GC */
	struct gcpro gcpro1;
	GCPRO1(event);
	maybe_echo_keys(XCOMMAND_BUILDER(XCONSOLE(Vselected_console)->command_builder), 0);	/* #### This sucks bigtime */
	for (;;) {
		event = Fnext_event(event, prompt);
		if (command_event_p(event))
			break;
		else
			execute_internal_event(event);
	}
	UNGCPRO;
	return event;
}

DEFUN("dispatch-non-command-events", Fdispatch_non_command_events, 0, 0, 0,	/*
Dispatch any pending "magic" events.

This function is useful for forcing the redisplay of native
widgets. Normally these are redisplayed through a native window-system
event encoded as magic event, rather than by the redisplay code.  This
function does not call redisplay or do any of the other things that
`next-event' does.
										 */
      ())
{
	/* This function can GC */
	Lisp_Object event = Qnil;
	struct gcpro gcpro1;
	GCPRO1(event);
	event = Fmake_event(Qnil, Qnil);

	/* Make sure that there will be something in the native event queue
	   so that externally managed things (e.g. widgets) get some CPU
	   time. */
	event_stream_force_event_pending(selected_frame());

	while (event_stream_event_pending_p(0)) {
		QUIT;		/* next_event_internal() does not QUIT. */

		/* We're a generator of the command_event_queue, so we can't be a
		   consumer as well.  Also, we have no reason to consult the
		   command_event_queue; there are only user and eval-events there,
		   and we'd just have to put them back anyway.
		 */
		next_event_internal(event, 0);	/* blocks */
		/* See the comment in accept-process-output about Vquit_flag */
		if (XEVENT_TYPE(event) == magic_event ||
		    XEVENT_TYPE(event) == timeout_event ||
		    XEVENT_TYPE(event) == process_event ||
		    XEVENT_TYPE(event) == pointer_motion_event)
			execute_internal_event(event);
		else {
			enqueue_command_event_1(event);
			break;
		}
	}

	Fdeallocate_event(event);
	UNGCPRO;
	return Qnil;
}

static void reset_current_events(struct command_builder *command_builder)
{
	Lisp_Object event = command_builder->current_events;
	reset_command_builder_event_chain(command_builder);
	if (EVENTP(event))
		deallocate_event_chain(event);
}

DEFUN("discard-input", Fdiscard_input, 0, 0, 0,	/*
Discard any pending "user" events.
Also cancel any kbd macro being defined.
A user event is a key press, button press, button release, or
"misc-user" event (menu selection or scrollbar action).
						 */
      ())
{
	/* This throws away user-input on the queue, but doesn't process any
	   events.  Calling dispatch_event() here leads to a race condition.
	 */
	Lisp_Object event = Fmake_event(Qnil, Qnil);
#ifndef EF_USE_ASYNEQ
	Lisp_Object head = Qnil, tail = Qnil;
#endif
	Lisp_Object oiq = Vinhibit_quit;
	struct gcpro gcpro1, gcpro2;
	/* #### not correct here with Vselected_console?  Should
	   discard-input take a console argument, or maybe map over
	   all consoles? */
	struct console *con = XCONSOLE(Vselected_console);

	/* next_event_internal() can cause arbitrary Lisp code to be evalled */
	GCPRO2(event, oiq);
	Vinhibit_quit = Qt;
	/* If a macro was being defined then we have to mark the modeline
	   has changed to ensure that it gets updated correctly. */
	if (!NILP(con->defining_kbd_macro))
		MARK_MODELINE_CHANGED;
	con->defining_kbd_macro = Qnil;
	reset_current_events(XCOMMAND_BUILDER(con->command_builder));

#ifdef EF_USE_ASYNEQ
	/* very raw :| */
	WITH_DLLIST_TRAVERSE(
		eq_queue(asyneq),
		sxe_event_t *ev = dllist_item;
		if (command_event_p((Lisp_Object)ev)) {
			dllist_pop_inner(eq_queue(asyneq), _el);
		});
#else
	while (!EQ_EMPTY_P() || event_stream_event_pending_p(1)) {
		/* This will take stuff off the command_event_queue, or read it
		   from the event_stream, but it will not block.
		 */
		next_event_internal(event, 1);
		Vquit_flag = Qnil;	/* Treat C-g as a user event (ignore it).
					   It is vitally important that we reset
					   Vquit_flag here.  Otherwise, if we're
					   reading from a TTY console,
					   maybe_read_quit_event() will notice
					   that C-g has been set and send us
					   another C-g.  That will cause us
					   to get right back here, and read
					   another C-g, ad infinitum ... */

		/* If the event is a user event, ignore it. */
		if (!command_event_p(event)) {
			/* Otherwise, chain the event onto our list of events
			   not to ignore, and keep reading until the queue is
			   empty.  This does not mean that if a subprocess is
			   generating an infinite amount of output, we will
			   never terminate (*provided* that the behavior of
			   next_event_cb() is correct -- see the comment in
			   events.h), because this loop ends as soon as there
			   are no more user events on the command_event_queue or
			   event_stream.
			 */
			enqueue_event(Fcopy_event(event, Qnil), &head, &tail);
		}
	}

	if (!EQ_EMPTY_P() || EQ_LARGE_P())
		abort();

	/* Now tack our chain of events back on to the front of the queue.
	   Actually, since the queue is now drained, we can just replace it.
	   The effect of this will be that we have deleted all user events
	   from the input stream without changing the relative ordering of
	   any other events.  (Some events may have been taken from the
	   event_stream and added to the command_event_queue, however.)

	   At this time, the command_event_queue will contain only eval_events.
	 */
	command_event_queue = head;
	command_event_queue_tail = tail;
#endif

	Fdeallocate_event(event);
	UNGCPRO;

	Vinhibit_quit = oiq;
	return Qnil;
}

/**********************************************************************/
/*                     pausing until an action occurs                 */
/**********************************************************************/

/* This is used in accept-process-output, sleep-for and sit-for.
   Before running any process_events in these routines, we set
   recursive_sit_for to Qt, and use this unwind protect to reset it to
   Qnil upon exit.  When recursive_sit_for is Qt, calling sit-for will
   cause it to return immediately.

   All of these routines install timeouts, so we clear the installed
   timeout as well.

   Note: It's very easy to break the desired behaviors of these
   3 routines.  If you make any changes to anything in this area, run
   the regression tests at the bottom of the file.  -- dmoore */

static Lisp_Object sit_for_unwind(Lisp_Object timeout_id)
{
	if (!NILP(timeout_id))
		Fdisable_timeout(timeout_id);

	recursive_sit_for = Qnil;
	return Qnil;
}

/* #### Is (accept-process-output nil 3) supposed to be like (sleep-for 3)?
 */

DEFUN("accept-process-output", Faccept_process_output, 0, 3, 0,	/*
Allow any pending output from subprocesses to be read by Emacs.
It is read into the process' buffers or given to their filter functions.
Non-nil arg PROCESS means do not return until some output has been received
from PROCESS. Nil arg PROCESS means do not return until some output has
been received from any process.

If the second arg is non-nil, it is the maximum number of seconds to wait:
this function will return after that much time even if no input has arrived
from PROCESS.  This argument may be a float, meaning wait some fractional
part of a second.

If the third arg is non-nil, it is a number of milliseconds that is added
to the second arg.  (This exists only for compatibility.)
Return non-nil iff we received any output before the timeout expired.
								 */
      (process, timeout_secs, timeout_msecs))
{
	/* This function can GC */
	struct gcpro gcpro1, gcpro2;
	Lisp_Object event = Qnil;
	Lisp_Object result = Qnil;
	int timeout_id = -1;
	int timeout_enabled = 0;
	int done = 0;
	struct buffer *old_buffer = current_buffer;
	int count;

	/* We preserve the current buffer but nothing else.  If a focus
	   change alters the selected window then the top level event loop
	   will eventually alter current_buffer to match.  In the mean time
	   we don't want to mess up whatever called this function. */

	if (!NILP(process))
		CHECK_PROCESS(process);

	GCPRO2(event, process);

	if (!NILP(timeout_secs) || !NILP(timeout_msecs)) {
		unsigned long msecs = 0;
		if (!NILP(timeout_secs))
			msecs = lisp_number_to_milliseconds(timeout_secs, 1);
		if (!NILP(timeout_msecs)) {
			CHECK_NATNUM(timeout_msecs);
			msecs += XINT(timeout_msecs);
		}
		if (msecs) {
			timeout_id =
			    event_stream_generate_wakeup(msecs, 0, Qnil, Qnil,
							 0);
			timeout_enabled = 1;
		}
	}

	event = Fmake_event(Qnil, Qnil);

	count = specpdl_depth();
	record_unwind_protect(sit_for_unwind,
			      timeout_enabled ? make_int(timeout_id) : Qnil);
	recursive_sit_for = Qt;

	while (!done &&
	       ((NILP(process) && timeout_enabled) ||
		(NILP(process) && event_stream_event_pending_p(0)) ||
		(!NILP(process))))
		/* Calling detect_input_pending() is the wrong thing here, because
		   that considers the Vunread_command_events and command_event_queue.
		   We don't need to look at the command_event_queue because we are
		   only interested in process events, which don't go on that.  In
		   fact, we can't read from it anyway, because we put stuff on it.

		   Note that event_stream->event_pending_p must be called in such
		   a way that it says whether any events *of any kind* are ready,
		   not just user events, or (accept-process-output nil) will fail
		   to dispatch any process events that may be on the queue.  It is
		   not clear to me that this is important, because the top-level
		   loop will process it, and I don't think that there is ever a
		   time when one calls accept-process-output with a nil argument
		   and really need the processes to be handled. */
	{
		/* If our timeout has arrived, we move along. */
		if (timeout_enabled
		    && !event_stream_wakeup_pending_p(timeout_id, 0)) {
			timeout_enabled = 0;
			done = 1;	/* We're  done. */
			continue;	/* Don't call next_event_internal */
		}

		QUIT;		/* next_event_internal() does not QUIT, so check
				   for ^G before reading output from the process
				   - this makes it less likely that the filter
				   will actually be aborted.
				 */

		next_event_internal(event, 0);
		/* If C-g was pressed while we were waiting, Vquit_flag got
		   set and next_event_internal() also returns C-g.  When
		   we enqueue the C-g below, it will get discarded.  The
		   next time through, QUIT will be called and will signal a quit. */
		switch (XEVENT_TYPE(event)) {
		case process_event:
			if (NILP(process) ||
			    EQ(XEVENT(event)->event.process.process,
			       process)) {
				done = 1;
				/* RMS's version always returns nil when
				   proc is nil, and only returns t if
				   input ever arrived on proc. */
				result = Qt;
			}

			execute_internal_event(event);
			break;

		case timeout_event:
			/* We execute the event even if it's ours, and notice
			   that it's happened above. */
		case pointer_motion_event:
		case magic_event:
			execute_internal_event(event);
			break;

		/* just list the other events here */
		case empty_event:
		case key_press_event:
		case button_press_event:
		case button_release_event:
		case misc_user_event:
		case magic_eval_event:
		case eval_event:
#ifdef EF_USE_ASYNEQ
		case eaten_myself_event:
		case work_started_event:
		case work_finished_event:
#endif  /* EF_USE_ASYNEQ */
		case dead_event:
		default:
			enqueue_command_event_1(event);
			break;
		}
	}

	unbind_to(count, timeout_enabled ? make_int(timeout_id) : Qnil);

	Fdeallocate_event(event);
	UNGCPRO;
	current_buffer = old_buffer;
	return result;
}

DEFUN("sleep-for", Fsleep_for, 1, 1, 0,	/*
Pause, without updating display, for SECONDS seconds.
SECONDS may be a float, allowing pauses for fractional parts of a second.

It is recommended that you never call sleep-for from inside of a process
filter function or timer event (either synchronous or asynchronous).
					 */
      (seconds))
{
	/* This function can GC */
	unsigned long msecs = lisp_number_to_milliseconds(seconds, 1);
	int id;
	Lisp_Object event = Qnil;
	int count;
	struct gcpro gcpro1;

	GCPRO1(event);

	id = event_stream_generate_wakeup(msecs, 0, Qnil, Qnil, 0);
	event = Fmake_event(Qnil, Qnil);

	count = specpdl_depth();
	record_unwind_protect(sit_for_unwind, make_int(id));
	recursive_sit_for = Qt;

	while (1) {
		/* If our timeout has arrived, we move along. */
		if (!event_stream_wakeup_pending_p(id, 0))
			goto DONE_LABEL;

		/* next_event_internal() does not QUIT, so check for ^G before
		   reading output from the process - this makes it less likely
		   that the filter will actually be aborted.
		*/
		QUIT;

		/* We're a generator of the command_event_queue, so we can't be
		   a consumer as well.  We don't care about command and
		   eval-events anyway.
		 */
		next_event_internal(event, 0);	/* blocks */
		/* See the comment in accept-process-output about Vquit_flag */
		switch (XEVENT_TYPE(event)) {
		case timeout_event:
			/* We execute the event even if it's ours, and notice
			   that it's happened above. */
		case process_event:
		case pointer_motion_event:
		case magic_event:
			execute_internal_event(event);
			break;

			/* just list the other events here */
		case empty_event:
		case key_press_event:
		case button_press_event:
		case button_release_event:
		case magic_eval_event:
		case eval_event:
		case misc_user_event:
#ifdef EF_USE_ASYNEQ
		case eaten_myself_event:
		case work_started_event:
		case work_finished_event:
#endif  /* EF_USE_ASYNEQ */
		case dead_event:
		default:
			enqueue_command_event_1(event);
			break;
		}
	}
      DONE_LABEL:
	unbind_to(count, make_int(id));
	Fdeallocate_event(event);
	UNGCPRO;
	return Qnil;
}

DEFUN("sit-for", Fsit_for, 1, 2, 0,	/*
Perform redisplay, then wait SECONDS seconds or until user input is available.
SECONDS may be a float, meaning a fractional part of a second.
Optional second arg NODISPLAY non-nil means don't redisplay; just wait.
Redisplay is preempted as always if user input arrives, and does not
happen if input is available before it starts.
Value is t if waited the full time with no input arriving.

If sit-for is called from within a process filter function or timer
event (either synchronous or asynchronous) it will return immediately.
					 */
      (seconds, nodisplay)) {
	/* This function can GC */
	unsigned long msecs = lisp_number_to_milliseconds(seconds, 1);
	Lisp_Object event, result;
	struct gcpro gcpro1;
	int id;
	int count;

	/* The unread-command-events count as pending input */
	if (!NILP(Vunread_command_events) || !NILP(Vunread_command_event))
		return Qnil;

	/* If the command-builder already has user-input on it (not eval events)
	   then that means we're done too.
	 */
	if (!EQ_EMPTY_P()) {
#if defined(EF_USE_ASYNEQ)
		EQ_TRAVERSE(
			asyneq, event,
			if (command_event_p(event)) {
				RETURN_FROM_EQ_TRAVERSE(asyneq, Qnil);
			});
#else
		EVENT_CHAIN_LOOP(event, command_event_queue) {
			if (command_event_p(event))
				return Qnil;
		}
#endif
	}

	/* If we're in a macro, or noninteractive, or early in temacs, then
	   don't wait. */
	if (noninteractive || !NILP(Vexecuting_macro))
		return Qnil;

	/* Recursive call from a filter function or timeout handler. */
	if (!NILP(recursive_sit_for)) {
		if (!event_stream_event_pending_p(1) && NILP(nodisplay)) {
			run_pre_idle_hook();
			redisplay();
		}
		return Qnil;
	}

	/* Otherwise, start reading events from the event_stream.
	   Do this loop at least once even if (sit-for 0) so that we
	   redisplay when no input pending.
	 */
	GCPRO1(event);
	event = Fmake_event(Qnil, Qnil);

	/* Generate the wakeup even if MSECS is 0, so that existing timeout/etc.
	   events get processed.  The old (pre-19.12) code special-cased this
	   and didn't generate a wakeup, but the resulting behavior was less than
	   ideal; viz. the occurrence of (sit-for 0.001) scattered throughout
	   the E-Lisp universe. */

	id = event_stream_generate_wakeup(msecs, 0, Qnil, Qnil, 0);

	count = specpdl_depth();
	record_unwind_protect(sit_for_unwind, make_int(id));
	recursive_sit_for = Qt;

	while (1) {
		/* If there is no user input pending, then redisplay.
		 */
		if (!event_stream_event_pending_p(1) && NILP(nodisplay)) {
			run_pre_idle_hook();
			redisplay();
		}

		/* If our timeout has arrived, we move along. */
		if (!event_stream_wakeup_pending_p(id, 0)) {
			result = Qt;
			goto DONE_LABEL;
		}

		/* next_event_internal() does not QUIT, so check for ^G
		   before reading output from the process - this makes it
		   less likely that the filter will actually be aborted.
		*/
		QUIT;

		/* We're a generator of the command_event_queue, so we can't be
		   a consumer as well.  In fact, we know there's nothing on the
		   command_event_queue that we didn't just put there.
		 */
		next_event_internal(event, 0);	/* blocks */
		/* See the comment in accept-process-output about Vquit_flag */

		if (command_event_p(event)) {
			QUIT;	/* If the command was C-g check it here
				   so that we abort out of the sit-for,
				   not the next command.  sleep-for and
				   accept-process-output continue looping
				   so they check QUIT again implicitly. */
			result = Qnil;
			goto DONE_LABEL;
		}

		switch (XEVENT_TYPE(event)) {
		case eval_event:
			/* eval-events get delayed until later. */
			enqueue_command_event(Fcopy_event(event, Qnil));
			break;

		case timeout_event:
			/* We execute the event even if it's ours, and notice
			   that it's happened above. */

			/* just list the rest here too */
		case empty_event:
		case key_press_event:
		case button_press_event:
		case button_release_event:
		case pointer_motion_event:
		case process_event:
		case magic_event:
		case magic_eval_event:
		case misc_user_event:
#ifdef EF_USE_ASYNEQ
		case eaten_myself_event:
		case work_started_event:
		case work_finished_event:
#endif  /* EF_USE_ASYNEQ */
		case dead_event:
		default:
			execute_internal_event(event);
			break;
		}
	}

      DONE_LABEL:
	unbind_to(count, make_int(id));

	/* Put back the event (if any) that made Fsit_for() exit before the
	   timeout.  Note that it is being added to the back of the queue, which
	   would be inappropriate if there were any user events on the queue
	   already: we would be misordering them.  But we know that there are
	   no user-events on the queue, or else we would not have reached this
	   point at all.
	 */
	if (NILP(result))
		enqueue_command_event(event);
	else
		Fdeallocate_event(event);

	UNGCPRO;
	return result;
}

/* This handy little function is used by select-x.c to wait for replies
   from processes that aren't really processes (e.g. the X server) */
void wait_delaying_user_input(int (*predicate) (void *arg), void *predicate_arg)
{
	/* This function can GC */
	Lisp_Object event = Fmake_event(Qnil, Qnil);
	struct gcpro gcpro1;
	GCPRO1(event);

	while (!(*predicate) (predicate_arg)) {
		QUIT;		/* next_event_internal() does not QUIT. */

		/* We're a generator of the command_event_queue, so we can't be a
		   consumer as well.  Also, we have no reason to consult the
		   command_event_queue; there are only user and eval-events there,
		   and we'd just have to put them back anyway.
		 */
		next_event_internal(event, 0);
		/* See the comment in accept-process-output about Vquit_flag */
		if (command_event_p(event)
		    || (XEVENT_TYPE(event) == eval_event)
		    || (XEVENT_TYPE(event) == magic_eval_event))
			enqueue_command_event_1(event);
		else
			execute_internal_event(event);
	}
	UNGCPRO;
}

/**********************************************************************/
/*                dispatching events; command builder                 */
/**********************************************************************/

static void
execute_internal_event(Lisp_Object event)
{
	/* events on dead channels get silently eaten */
	if (object_dead_p(XEVENT(event)->channel)) {
		return;
	}

	/* This function can GC */
	switch (XEVENT_TYPE(event)) {
	case empty_event:
		return;

	case eval_event:
		call1(XEVENT(event)->event.eval.function,
		      XEVENT(event)->event.eval.object);
		return;

	case magic_eval_event:
		(XEVENT(event)->event.magic_eval.internal_function)
			(XEVENT(event)->event.magic_eval.object);
		return;

	case pointer_motion_event:
		if (!NILP(Vmouse_motion_handler))
			call1(Vmouse_motion_handler, event);
		return;

	case process_event: {
		Lisp_Object p = XEVENT(event)->event.process.process;
		Charcount readstatus;

		assert(PROCESSP(p));
		while ((readstatus = read_process_output(p)) > 0) ;
		if (readstatus > 0) ;	/* this clauses never gets
					   executed but allows the
					   #ifdefs to work cleanly. */
#ifdef EWOULDBLOCK
		else if (readstatus == -1 && errno == EWOULDBLOCK) ;
#endif				/* EWOULDBLOCK */
#ifdef EAGAIN
		else if (readstatus == -1 && errno == EAGAIN) ;
#endif				/* EAGAIN */
		else if ((readstatus == 0 &&
			  /* Note that we cannot distinguish between no
			     input available now and a closed pipe.
			     With luck, a closed pipe will be
			     accompanied by subprocess termination and
			     SIGCHLD.  */
			  (!network_connection_p(p) ||
			   /*
			     When connected to ToolTalk (i.e.
			     connected_via_filedesc_p()), it's not
			     possible to reliably determine whether
			     there is a message waiting for ToolTalk to
			     receive.  ToolTalk expects to have
			     tt_message_receive() called exactly once
			     every time the file descriptor becomes
			     active, so the filter function forces this
			     by returning 0.  Emacs must not interpret
			     this as a closed pipe. */
			   connected_via_filedesc_p(XPROCESS(p))))
#ifdef HAVE_PTYS
			 /* On some OSs with ptys, when the process on
			    one end of a pty exits, the other end gets
			    an error reading with errno = EIO instead of
			    getting an EOF (0 bytes read).  Therefore,
			    if we get an error reading and errno = EIO,
			    just continue, because the child process has
			    exited and should clean itself up soon
			    (e.g. when we get a SIGCHLD). */
			 || (readstatus == -1 && errno == EIO)
#endif
			) {
			/* Currently, we rely on SIGCHLD to indicate
			   that the process has terminated.
			   Unfortunately, on some systems the SIGCHLD
			   gets missed some of the time.  So we put an
			   additional check in status_notify() to see
			   whether a process has terminated.  We must
			   tell status_notify() to enable that check,
			   and we do so now. */
			kick_status_notify();
		} else {
			/* Deactivate network connection */
			Lisp_Object status = Fprocess_status(p);
			if (EQ(status, Qopen)
			    /* In case somebody changes the theory of
			       whether to return open as opposed to run
			       for network connection "processes"... */
			    || EQ(status, Qrun))
				update_process_status(p, Qexit, 256, 0);
			deactivate_process(p);
		}

		/* We must call status_notify here to allow the
		   event_stream->unselect_process_cb to be run if appropriate.
		   Otherwise, dead fds may be selected for, and we will get a
		   continuous stream of process events for them.  Since we don't
		   return until all process events have been flushed, we would
		   get stuck here, processing events on a process whose status
		   was 'exit.  Call this after dispatch-event, or the fds will
		   have been closed before we read the last data from them.
		   It's safe for the filter to signal an error because
		   status_notify() will be called on return to top-level.
		*/
		status_notify();
		return;
	}

	case timeout_event: {
		Lisp_Event *e = XEVENT(event);
		if (!NILP(e->event.timeout.function))
			call1(e->event.timeout.function,
			      e->event.timeout.object);
		return;
	}

	case magic_event:
		event_stream_handle_magic_event(XEVENT(event));
		return;

#ifdef EF_USE_ASYNEQ
	case work_started_event:
	case work_finished_event:
	case eaten_myself_event:
		return;
#endif  /* EF_USE_ASYNEQ */

		/* not sure about the next ones, but they've
		 * always been unhandled and so be they ... */
	case key_press_event:
	case button_press_event:
	case button_release_event:
	case misc_user_event:
		/* and now the ones i'm quite sure about */
	case dead_event:
	default:
		abort();
	}
}


static void
this_command_keys_replace_suffix(Lisp_Object suffix, Lisp_Object chain)
{
	Lisp_Object first_before_suffix =
	    event_chain_find_previous(Vthis_command_keys, suffix);

	if (NILP(first_before_suffix))
		Vthis_command_keys = chain;
	else
		XSET_EVENT_NEXT(first_before_suffix, chain);
	deallocate_event_chain(suffix);
	Vthis_command_keys_tail = event_chain_tail(chain);
}

static void
command_builder_replace_suffix(struct command_builder *builder,
			       Lisp_Object suffix, Lisp_Object chain)
{
	Lisp_Object first_before_suffix =
	    event_chain_find_previous(builder->current_events, suffix);

	if (NILP(first_before_suffix))
		builder->current_events = chain;
	else
		XSET_EVENT_NEXT(first_before_suffix, chain);
	deallocate_event_chain(suffix);
	builder->most_current_event = event_chain_tail(chain);
}

static Lisp_Object command_builder_find_leaf_1(struct command_builder *builder)
{
	Lisp_Object event0 = builder->current_events;

	if (NILP(event0))
		return Qnil;

	return event_binding(event0, 1);
}

/* See if we can do function-key-map or key-translation-map translation
   on the current events in the command builder.  If so, do this, and
   return the resulting binding, if any. */

static Lisp_Object
munge_keymap_translate(struct command_builder *builder,
		       enum munge_me_out_the_door munge,
		       int has_normal_binding_p)
{
	Lisp_Object suffix;

	EVENT_CHAIN_LOOP(suffix, builder->munge_me[munge].first_mungeable_event) {
		Lisp_Object result =
		    munging_key_map_event_binding(suffix, munge);

		if (NILP(result))
			continue;

		if (KEYMAPP(result)) {
			if (NILP(builder->last_non_munged_event)
			    && !has_normal_binding_p)
				builder->last_non_munged_event =
				    builder->most_current_event;
		} else
			builder->last_non_munged_event = Qnil;

		if (!KEYMAPP(result) && !VECTORP(result) && !STRINGP(result)) {
			struct gcpro gcpro1;
			GCPRO1(suffix);
			result = call1(result, Qnil);
			UNGCPRO;
			if (NILP(result))
				return Qnil;
		}

		if (KEYMAPP(result))
			return result;

		if (VECTORP(result) || STRINGP(result)) {
			Lisp_Object new_chain =
			    key_sequence_to_event_chain(result);
			Lisp_Object tempev;
			int n, tckn;

			/* If the first_mungeable_event of the other munger is
			   within the events we're munging, then it will point to
			   deallocated events afterwards, which is bad -- so make it
			   point at the beginning of the munged events. */
			EVENT_CHAIN_LOOP(tempev, suffix) {
				Lisp_Object *mungeable_event =
				    &builder->munge_me[1 -
						       munge].
				    first_mungeable_event;
				if (EQ(tempev, *mungeable_event)) {
					*mungeable_event = new_chain;
					break;
				}
			}

			n = event_chain_count(suffix);
			command_builder_replace_suffix(builder, suffix,
						       new_chain);
			builder->munge_me[munge].first_mungeable_event = Qnil;
			/* Now hork this-command-keys as well. */

			/* We just assume that the events we just replaced are
			   sitting in copied form at the end of this-command-keys.
			   If the user did weird things with `dispatch-event' this
			   may not be the case, but at least we make sure we won't
			   crash. */
			new_chain = copy_event_chain(new_chain);
			tckn = event_chain_count(Vthis_command_keys);
			if (tckn >= n) {
				this_command_keys_replace_suffix
				    (event_chain_nth
				     (Vthis_command_keys, tckn - n), new_chain);
			}

			result = command_builder_find_leaf_1(builder);
			return result;
		}

		signal_simple_error((munge == MUNGE_ME_FUNCTION_KEY ?
				     "Invalid binding in function-key-map" :
				     "Invalid binding in key-translation-map"),
				    result);
	}

	return Qnil;
}

/* Compare the current state of the command builder against the local and
   global keymaps, and return the binding.  If there is no match, try again,
   case-insensitively.  The return value will be one of:
      -- nil (there is no binding)
      -- a keymap (part of a command has been specified)
      -- a command (anything that satisfies `commandp'; this includes
                    some symbols, lists, subrs, strings, vectors, and
		    compiled-function objects)
 */
static Lisp_Object
command_builder_find_leaf(struct command_builder *builder,
			  int allow_misc_user_events_p)
{
	/* This function can GC */
	Lisp_Object result;
	Lisp_Object evee = builder->current_events;

	if (XEVENT_TYPE(evee) == misc_user_event) {
		if (allow_misc_user_events_p && (NILP(XEVENT_NEXT(evee))))
			return list2(XEVENT(evee)->event.eval.function,
				     XEVENT(evee)->event.eval.object);
		else
			return Qnil;
	}

	/* if we're currently in a menu accelerator, check there for further
	   events */
	/* #### fuck me!  who wrote this crap?  think "abstraction", baby. */
#if defined(HAVE_X_WINDOWS) && defined(LWLIB_MENUBARS_LUCID)
	if (x_kludge_lw_menu_active()) {
		return command_builder_operate_menu_accelerator(builder);
	} else {
		result = Qnil;
		if (EQ(Vmenu_accelerator_enabled, Qmenu_force))
			result = command_builder_find_menu_accelerator(builder);
		if (NILP(result))
#endif
			result = command_builder_find_leaf_1(builder);
#if defined(HAVE_X_WINDOWS) && defined(LWLIB_MENUBARS_LUCID)
		if (NILP(result)
		    && EQ(Vmenu_accelerator_enabled, Qmenu_fallback))
			result = command_builder_find_menu_accelerator(builder);
	}
#endif

	/* Check to see if we have a potential function-key-map match. */
	if (NILP(result)) {
		result =
		    munge_keymap_translate(builder, MUNGE_ME_FUNCTION_KEY, 0);
		regenerate_echo_keys_from_this_command_keys(builder);
	}
	/* Check to see if we have a potential key-translation-map match. */
	{
		Lisp_Object key_translate_result =
		    munge_keymap_translate(builder, MUNGE_ME_KEY_TRANSLATION,
					   !NILP(result));
		if (!NILP(key_translate_result)) {
			result = key_translate_result;
			regenerate_echo_keys_from_this_command_keys(builder);
		}
	}

	if (!NILP(result))
		return result;

	/* If key-sequence wasn't bound, we'll try some fallbacks.  */

	/* If we didn't find a binding, and the last event in the sequence is
	   a shifted character, then try again with the lowercase version.  */

	if (XEVENT_TYPE(builder->most_current_event) == key_press_event
	    && !NILP(Vretry_undefined_key_binding_unshifted)) {
		Lisp_Object terminal = builder->most_current_event;
		struct key_data *key = &XEVENT(terminal)->event.key;
		Emchar c = 0;
		if ((key->modifiers & XEMACS_MOD_SHIFT)
		    || (CHAR_OR_CHAR_INTP(key->keysym)
			&& ((c = XCHAR_OR_CHAR_INT(key->keysym)), c >= 'A'
			    && c <= 'Z'))) {
			Lisp_Event terminal_copy = *XEVENT(terminal);

			if (key->modifiers & XEMACS_MOD_SHIFT)
				key->modifiers &= (~XEMACS_MOD_SHIFT);
			else
				key->keysym = make_char(c + 'a' - 'A');

			result =
			    command_builder_find_leaf(builder,
						      allow_misc_user_events_p);
			if (!NILP(result))
				return result;
			/* If there was no match with the lower-case version either,
			   then put back the upper-case event for the error
			   message.  But make sure that function-key-map didn't
			   change things out from under us. */
			if (EQ(terminal, builder->most_current_event))
				*XEVENT(terminal) = terminal_copy;
		}
	}

	/* help-char is `auto-bound' in every keymap */
	if (!NILP(Vprefix_help_command) &&
	    event_matches_key_specifier_p(XEVENT(builder->most_current_event),
					  Vhelp_char))
		return Vprefix_help_command;

#ifdef HAVE_XIM
	/* If keysym is a non-ASCII char, bind it to self-insert-char by default. */
	if (XEVENT_TYPE(builder->most_current_event) == key_press_event
	    && !NILP(Vcomposed_character_default_binding)) {
		Lisp_Object keysym =
		    XEVENT(builder->most_current_event)->event.key.keysym;
		if (CHARP(keysym) && !CHAR_ASCII_P(XCHAR(keysym)))
			return Vcomposed_character_default_binding;
	}
#endif				/* HAVE_XIM */

	/* If we read extra events attempting to match a function key but end
	   up failing, then we release those events back to the command loop
	   and fail on the original lookup.  The released events will then be
	   reprocessed in the context of the first part having failed. */
	if (!NILP(builder->last_non_munged_event)) {
		Lisp_Object event0 = builder->last_non_munged_event;

		/* Put the commands back on the event queue. */
#ifdef EF_USE_ASYNEQ
		eq_enqueue_event_chain(asyneq, XEVENT_NEXT(event0));
#else
		enqueue_event_chain(XEVENT_NEXT(event0),
				    &command_event_queue,
				    &command_event_queue_tail);
#endif
		/* Then remove them from the command builder. */
		XSET_EVENT_NEXT(event0, Qnil);
		builder->most_current_event = event0;
		builder->last_non_munged_event = Qnil;
	}

	return Qnil;
}

/* Every time a command-event (a key, button, or menu selection) is read by
   Fnext_event(), it is stored in the recent_keys_ring, in Vlast_input_event,
   and in Vthis_command_keys.  (Eval-events are not stored there.)

   Every time a command is invoked, Vlast_command_event is set to the last
   event in the sequence.

   This means that Vthis_command_keys is really about "input read since the
   last command was executed" rather than about "what keys invoked this
   command."  This is a little counterintuitive, but that's the way it
   has always worked.

   As an extra kink, the function read-key-sequence resets/updates the
   last-command-event and this-command-keys.  It doesn't append to the
   command-keys as read-char does.  Such are the pitfalls of having to
   maintain compatibility with a program for which the only specification
   is the code itself.

   (We could implement recent_keys_ring and Vthis_command_keys as the same
   data structure.)
 */

DEFUN("recent-keys", Frecent_keys, 0, 1, 0,	/*
Return a vector of recent keyboard or mouse button events read.
If NUMBER is non-nil, not more than NUMBER events will be returned.
Change number of events stored using `set-recent-keys-ring-size'.

This copies the event objects into a new vector; it is safe to keep and
modify them.
						 */
      (number)) {
	struct gcpro gcpro1;
	Lisp_Object val = Qnil;
	int nwanted;
	int start, nkeys, i, j;
	GCPRO1(val);

	if (NILP(number))
		nwanted = recent_keys_ring_size;
	else {
		CHECK_NATNUM(number);
		nwanted = XINT(number);
	}

	/* Create the keys ring vector, if none present. */
	if (NILP(Vrecent_keys_ring)) {
		Vrecent_keys_ring = make_vector(recent_keys_ring_size, Qnil);
		/* And return nothing in particular. */
		RETURN_UNGCPRO(make_vector(0, Qnil));
	}

	if (NILP(XVECTOR_DATA(Vrecent_keys_ring)[recent_keys_ring_index]))
		/* This means the vector has not yet wrapped */
	{
		nkeys = recent_keys_ring_index;
		start = 0;
	} else {
		nkeys = recent_keys_ring_size;
		start =
		    ((recent_keys_ring_index ==
		      nkeys) ? 0 : recent_keys_ring_index);
	}

	if (nwanted < nkeys) {
		start += nkeys - nwanted;
		if (start >= recent_keys_ring_size)
			start -= recent_keys_ring_size;
		nkeys = nwanted;
	} else
		nwanted = nkeys;

	val = make_vector(nwanted, Qnil);

	for (i = 0, j = start; i < nkeys; i++) {
		Lisp_Object e = XVECTOR_DATA(Vrecent_keys_ring)[j];

		if (NILP(e))
			abort();
		XVECTOR_DATA(val)[i] = Fcopy_event(e, Qnil);
		if (++j >= recent_keys_ring_size)
			j = 0;
	}
	UNGCPRO;
	return val;
}

DEFUN("recent-keys-ring-size", Frecent_keys_ring_size, 0, 0, 0,	/*
The maximum number of events `recent-keys' can return.
								 */
      ())
{
	return make_int(recent_keys_ring_size);
}

DEFUN("set-recent-keys-ring-size", Fset_recent_keys_ring_size, 1, 1, 0,	/*
Set the maximum number of events to be stored internally.
									 */
      (size))
{
	Lisp_Object new_vector = Qnil;
	int i, j, nkeys, start, min;
	struct gcpro gcpro1;

	CHECK_INT(size);
	if (XINT(size) <= 0)
		error("Recent keys ring size must be positive");
	if (XINT(size) == recent_keys_ring_size)
		return size;

	GCPRO1(new_vector);
	new_vector = make_vector(XINT(size), Qnil);

	if (NILP(Vrecent_keys_ring)) {
		Vrecent_keys_ring = new_vector;
		RETURN_UNGCPRO(size);
	}

	if (NILP(XVECTOR_DATA(Vrecent_keys_ring)[recent_keys_ring_index]))
		/* This means the vector has not yet wrapped */
	{
		nkeys = recent_keys_ring_index;
		start = 0;
	} else {
		nkeys = recent_keys_ring_size;
		start =
		    ((recent_keys_ring_index ==
		      nkeys) ? 0 : recent_keys_ring_index);
	}

	if (XINT(size) > nkeys)
		min = nkeys;
	else
		min = XINT(size);

	for (i = 0, j = start; i < min; i++) {
		XVECTOR_DATA(new_vector)[i] =
		    XVECTOR_DATA(Vrecent_keys_ring)[j];
		if (++j >= recent_keys_ring_size)
			j = 0;
	}
	recent_keys_ring_size = XINT(size);
	recent_keys_ring_index = (i < recent_keys_ring_size) ? i : 0;

	Vrecent_keys_ring = new_vector;

	UNGCPRO;
	return size;
}

/* Vthis_command_keys having value Qnil means that the next time
   push_this_command_keys is called, it should start over.
   The times at which the command-keys are reset
   (instead of merely being augmented) are pretty counterintuitive.
   (More specifically:

   -- We do not reset this-command-keys when we finish reading a
      command.  This is because some commands (e.g. C-u) act
      like command prefixes; they signal this by setting prefix-arg
      to non-nil.
   -- Therefore, we reset this-command-keys when we finish
      executing a command, unless prefix-arg is set.
   -- However, if we ever do a non-local exit out of a command
      loop (e.g. an error in a command), we need to reset
      this-command-keys.  We do this by calling reset_this_command_keys()
      from cmdloop.c, whenever an error causes an invocation of the
      default error handler, and whenever there's a throw to top-level.)
 */

void reset_this_command_keys(Lisp_Object console, int clear_echo_area_p)
{
	if (!NILP(console)) {
		/* console is nil if we just deleted the console as a result of C-x 5
		   0.  Unfortunately things are currently in a messy situation where
		   some stuff is console-local and other stuff isn't, so we need to
		   do everything that's not console-local. */
		struct command_builder *command_builder =
		    XCOMMAND_BUILDER(XCONSOLE(console)->command_builder);

		reset_key_echo(command_builder, clear_echo_area_p);
		reset_current_events(command_builder);
	} else
		reset_key_echo(0, clear_echo_area_p);

	deallocate_event_chain(Vthis_command_keys);
	Vthis_command_keys = Qnil;
	Vthis_command_keys_tail = Qnil;
}

static void push_this_command_keys(Lisp_Object event)
{
	Lisp_Object new = Fmake_event(Qnil, Qnil);

	Fcopy_event(event, new);
	enqueue_event(new, &Vthis_command_keys, &Vthis_command_keys_tail);
}

/* The following two functions are used in call-interactively,
   for the @ and e specifications.  We used to just use
   `current-mouse-event' (i.e. the last mouse event in this-command-keys),
   but FSF does it more generally so we follow their lead. */

Lisp_Object extract_this_command_keys_nth_mouse_event(int n)
{
	Lisp_Object event;

	EVENT_CHAIN_LOOP(event, Vthis_command_keys) {
		if (EVENTP(event)
		    && (XEVENT_TYPE(event) == button_press_event
			|| XEVENT_TYPE(event) == button_release_event
			|| XEVENT_TYPE(event) == misc_user_event)) {
			if (!n) {
				/* must copy to avoid an abort() in next_event_internal() */
				if (!NILP(XEVENT_NEXT(event)))
					return Fcopy_event(event, Qnil);
				else
					return event;
			}
			n--;
		}
	}

	return Qnil;
}

Lisp_Object extract_vector_nth_mouse_event(Lisp_Object vector, int n)
{
	int i;
	int len = XVECTOR_LENGTH(vector);

	for (i = 0; i < len; i++) {
		Lisp_Object event = XVECTOR_DATA(vector)[i];
		if (EVENTP(event)) {
			switch (XEVENT_TYPE(event)) {
			case button_press_event:
			case button_release_event:
			case misc_user_event:
				if (n == 0)
					return event;
				n--;
				break;

				/* the rest of 'em cases */
			case empty_event:
			case key_press_event:
			case pointer_motion_event:
			case process_event:
			case timeout_event:
			case magic_event:
			case magic_eval_event:
			case eval_event:
#ifdef EF_USE_ASYNEQ
			case eaten_myself_event:
			case work_started_event:
			case work_finished_event:
#endif  /* EF_USE_ASYNEQ */
			case dead_event:
			default:
				continue;
			}
		}
	}

	return Qnil;
}

static void push_recent_keys(Lisp_Object event)
{
	Lisp_Object e;

	if (NILP(Vrecent_keys_ring))
		Vrecent_keys_ring = make_vector(recent_keys_ring_size, Qnil);

	e = XVECTOR_DATA(Vrecent_keys_ring)[recent_keys_ring_index];

	if (NILP(e)) {
		e = Fmake_event(Qnil, Qnil);
		XVECTOR_DATA(Vrecent_keys_ring)[recent_keys_ring_index] = e;
	}
	Fcopy_event(event, e);
	if (++recent_keys_ring_index == recent_keys_ring_size)
		recent_keys_ring_index = 0;
}

static Lisp_Object
current_events_into_vector(struct command_builder *command_builder)
{
	Lisp_Object vector;
	Lisp_Object event;
	int n = event_chain_count(command_builder->current_events);

	/* Copy the vector and the events in it. */
	/*  No need to copy the events, since they're already copies, and
	   nobody other than the command-builder has pointers to them */
	vector = make_vector(n, Qnil);
	n = 0;
	EVENT_CHAIN_LOOP(event, command_builder->current_events)
	    XVECTOR_DATA(vector)[n++] = event;
	reset_command_builder_event_chain(command_builder);
	return vector;
}

/*
   Given the current state of the command builder and a new command event
   that has just been dispatched:

   -- add the event to the event chain forming the current command
      (doing meta-translation as necessary)
   -- return the binding of this event chain; this will be one of:
      -- nil (there is no binding)
      -- a keymap (part of a command has been specified)
      -- a command (anything that satisfies `commandp'; this includes
                    some symbols, lists, subrs, strings, vectors, and
		    compiled-function objects)
 */
static Lisp_Object
lookup_command_event(struct command_builder *command_builder,
		     Lisp_Object event, int allow_misc_user_events_p)
{
	/* This function can GC */
	struct frame *f = selected_frame();
	/* Clear output from previous command execution */
	if (!EQ(Qcommand, echo_area_status(f))
	    /* but don't let mouse-up clear what mouse-down just printed */
	    && (XEVENT(event)->event_type != button_release_event))
		clear_echo_area(f, Qnil, 0);

	/* Add the given event to the command builder.
	   Extra hack: this also updates the recent_keys_ring and Vthis_command_keys
	   vectors to translate "ESC x" to "M-x" (for any "x" of course).
	 */
	{
		Lisp_Object recent = command_builder->most_current_event;

		if (EVENTP(recent)
		    && event_matches_key_specifier_p(XEVENT(recent),
						     Vmeta_prefix_char)) {
			Lisp_Event *e;
			/* When we see a sequence like "ESC x", pretend we really saw "M-x".
			   DoubleThink the recent-keys and this-command-keys as well. */

			/* Modify the previous most-recently-pushed event on the command
			   builder to be a copy of this one with the meta-bit set instead of
			   pushing a new event.
			 */
			Fcopy_event(event, recent);
			e = XEVENT(recent);
			if (e->event_type == key_press_event)
				e->event.key.modifiers |= XEMACS_MOD_META;
			else if (e->event_type == button_press_event
				 || e->event_type == button_release_event)
				e->event.button.modifiers |= XEMACS_MOD_META;
			else
				abort();

			{
				int tckn =
				    event_chain_count(Vthis_command_keys);
				if (tckn >= 2)
					/* ??? very strange if it's < 2. */
					this_command_keys_replace_suffix
					    (event_chain_nth
					     (Vthis_command_keys, tckn - 2),
					     Fcopy_event(recent, Qnil));
			}

			regenerate_echo_keys_from_this_command_keys
			    (command_builder);
		} else {
			event = Fcopy_event(event, Fmake_event(Qnil, Qnil));

			command_builder_append_event(command_builder, event);
		}
	}

	{
		Lisp_Object leaf = command_builder_find_leaf(command_builder,
							     allow_misc_user_events_p);
		struct gcpro gcpro1;
		GCPRO1(leaf);

		if (KEYMAPP(leaf)) {
#if defined (HAVE_X_WINDOWS) && defined (LWLIB_MENUBARS_LUCID)
			if (!x_kludge_lw_menu_active())
#else
			if (1)
#endif
			{
				Lisp_Object prompt = Fkeymap_prompt(leaf, Qt);
				if (STRINGP(prompt)) {
					/* Append keymap prompt to key echo buffer */
					int buf_index =
					    command_builder->echo_buf_index;
					Bytecount len = XSTRING_LENGTH(prompt);

					if (len + buf_index + 1 <=
					    command_builder->echo_buf_length) {
						Bufbyte *echo =
						    command_builder->echo_buf +
						    buf_index;
						memcpy(echo,
						       XSTRING_DATA(prompt),
						       len);
						echo[len] = 0;
					}
					maybe_echo_keys(command_builder, 1);
				} else
					maybe_echo_keys(command_builder, 0);
			} else if (!NILP(Vquit_flag)) {
				Lisp_Object quit_event =
				    Fmake_event(Qnil, Qnil);
				Lisp_Event *e = XEVENT(quit_event);
				/* if quit happened during menu acceleration,
				   pretend we read it */
				Lisp_Object tmp = Fselected_console();
				struct console *con = XCONSOLE(tmp);
				int ch = CONSOLE_QUIT_CHAR(con);

				character_to_event(ch, e, con, 1, 1);
				e->channel = make_console(con);

				enqueue_command_event(quit_event);
				Vquit_flag = Qnil;
			}
		} else if (!NILP(leaf)) {
			if (EQ(Qcommand, echo_area_status(f))
			    && command_builder->echo_buf_index > 0) {
				/* If we had been echoing keys, echo the last
				   one (without the trailing dash) and redisplay
				   before executing the command. */
				command_builder->echo_buf[command_builder->
							  echo_buf_index] = 0;
				maybe_echo_keys(command_builder, 1);
				Fsit_for(Qzero, Qt);
			}
		}
		RETURN_UNGCPRO(leaf);
	}
}

static int is_scrollbar_event(Lisp_Object event)
{
#ifdef HAVE_SCROLLBARS
	Lisp_Object fun;

	if (XEVENT(event)->event_type != misc_user_event)
		return 0;
	fun = XEVENT(event)->event.misc.function;

	return (EQ(fun, Qscrollbar_line_up) ||
		EQ(fun, Qscrollbar_line_down) ||
		EQ(fun, Qscrollbar_page_up) ||
		EQ(fun, Qscrollbar_page_down) ||
		EQ(fun, Qscrollbar_to_top) ||
		EQ(fun, Qscrollbar_to_bottom) ||
		EQ(fun, Qscrollbar_vertical_drag) ||
		EQ(fun, Qscrollbar_char_left) ||
		EQ(fun, Qscrollbar_char_right) ||
		EQ(fun, Qscrollbar_page_left) ||
		EQ(fun, Qscrollbar_page_right) ||
		EQ(fun, Qscrollbar_to_left) ||
		EQ(fun, Qscrollbar_to_right) ||
		EQ(fun, Qscrollbar_horizontal_drag));
#else
	return 0;
#endif				/* HAVE_SCROLLBARS */
}

static void
execute_command_event(struct command_builder *cmd_builder, Lisp_Object event)
{
	/* This function can GC */
	struct console *con = XCONSOLE(cmd_builder->console);
	struct gcpro gcpro1;

	GCPRO1(event);		/* event may be freshly created */

	/* #### This call to is_scrollbar_event() isn't quite right, but
	   fixing properly it requires more work than can go into 21.4.
	   (We really need to split out menu, scrollbar, dialog, and other
	   types of events from misc-user, and put the remaining ones in a
	   new `user-eval' type that behaves like an eval event but is a
	   user event and thus has all of its semantics -- e.g. being
	   delayed during `accept-process-output' and similar wait states.)

	   The real issue here is that "user events" and "command events"
	   are not the same thing, but are very much confused in
	   event-stream.c.  User events are, essentially, any event that
	   should be delayed by accept-process-output, should terminate a
	   sit-for, etc. -- basically, any event that needs to be processed
	   synchronously with key and mouse events.  Command events are
	   those that participate in command building; scrollbar events
	   clearly don't belong because they should be transparent in a
	   sequence like C-x @ h <scrollbar-drag> x, which used to cause a
	   crash before checks similar to the is_scrollbar_event() call were
	   added.  Do other events belong with scrollbar events?  I'm not
	   sure; we need to categorize all misc-user events and see what
	   their semantics are.

	   (You might ask, why do scrollbar events need to be user events?
	   That's a good question.  The answer seems to be that they can
	   change point, and having this happen asynchronously would be a
	   very bad idea.  According to the "proper" functioning of
	   scrollbars, this should not happen, but SXEmacs does not allow
	   point to go outside of the window.)

	   Scrollbar events and similar non-command events should obviously
	   not be recorded in this-command-keys, so we need to check for
	   this in next-event.

	   #### We call reset_current_events() twice in this function --
	   #### here, and later as a result of reset_this_command_keys().
	   #### This is almost certainly wrong; need to figure out what's
	   #### correct.

	   #### We need to figure out what's really correct w.r.t. scrollbar
	   #### events.  With these new fixes in, it actually works to do
	   #### C-x <scrollbar-drag> 5 2, but the key echo gets messed up
	   #### (starts over at 5).  We really need to be special-casing
	   #### scrollbar events at a lower level, and not really passing
	   #### them through the command builder at all.  (e.g. do scrollbar
	   #### events belong in macros???  doubtful; probably only the
	   #### point movement, if any, belongs, special-cased as a
	   #### pseudo-issued M-x goto-char command).  #### Need more work
	   #### here.  Do this when separating out scrollbar events.
	 */

	if (!is_scrollbar_event(event))
		reset_current_events(cmd_builder);

	switch (XEVENT(event)->event_type) {
	case key_press_event:
		Vcurrent_mouse_event = Qnil;
		break;
	case button_press_event:
	case button_release_event:
	case misc_user_event:
		Vcurrent_mouse_event = Fcopy_event(event, Qnil);
		break;

		/* just list the other cases here */
	case empty_event:
	case pointer_motion_event:
	case process_event:
	case timeout_event:
	case magic_event:
	case magic_eval_event:
	case eval_event:
#ifdef EF_USE_ASYNEQ
	case eaten_myself_event:
	case work_started_event:
	case work_finished_event:
#endif  /* EF_USE_ASYNEQ */
	case dead_event:
	default:
		break;
	}

	/* Store the last-command-event.  The semantics of this is that it
	   is the last event most recently involved in command-lookup. */
	if (!EVENTP(Vlast_command_event))
		Vlast_command_event = Fmake_event(Qnil, Qnil);
	if (XEVENT(Vlast_command_event)->event_type == dead_event) {
		Vlast_command_event = Fmake_event(Qnil, Qnil);
		error("Someone deallocated the last-command-event!");
	}

	if (!EQ(event, Vlast_command_event))
		Fcopy_event(event, Vlast_command_event);

	/* Note that last-command-char will never have its high-bit set, in
	   an effort to sidestep the ambiguity between M-x and oslash. */
	Vlast_command_char = Fevent_to_character(Vlast_command_event,
						 Qnil, Qnil, Qnil);

	/* Actually call the command, with all sorts of hair to preserve or clear
	   the echo-area and region as appropriate and call the pre- and post-
	   command-hooks. */
	{
		int old_kbd_macro = con->kbd_macro_end;
		Lisp_Object tmp = Fselected_window(Qnil);
		struct window *w = XWINDOW(tmp);

		/* We're executing a new command, so the old value is irrelevant. */
		zmacs_region_stays = 0;

		/* If the previous command tried to force a specific window-start,
		   reset the flag in case this command moves point far away from
		   that position.  Also, reset the window's buffer's change
		   information so that we don't trigger an incremental update. */
		if (w->force_start) {
			w->force_start = 0;
			buffer_reset_changes(XBUFFER(w->buffer));
		}

		pre_command_hook();

		if (XEVENT(event)->event_type == misc_user_event) {
			call1(XEVENT(event)->event.eval.function,
			      XEVENT(event)->event.eval.object);
		} else {
			Fcommand_execute(Vthis_command, Qnil, Qnil);
		}

		post_command_hook();

		/* Console might have been deleted by command */
		if (CONSOLE_LIVE_P(con) && !NILP(con->prefix_arg)) {
			/* Commands that set the prefix arg don't update
			   last-command, don't reset the echoing state, and
			   don't go into keyboard macros unless followed by
			   another command.  Also don't quit here.  */
			int speccount = specpdl_depth();
			specbind(Qinhibit_quit, Qt);
			maybe_echo_keys(cmd_builder, 0);
			unbind_to(speccount, Qnil);

			/* If we're recording a keyboard macro, and the last
			   command executed set a prefix argument, then
			   decrement the pointer to the "last character really
			   in the macro" to be just before this command.  This
			   is so that the ^U in "^U ^X )" doesn't go onto the
			   end of macro. */
			if (!NILP(con->defining_kbd_macro))
				con->kbd_macro_end = old_kbd_macro;
		} else {
			/* Start a new command next time */
			Vlast_command = Vthis_command;
			Vlast_command_properties = Vthis_command_properties;
			Vthis_command_properties = Qnil;

			/* Emacs 18 doesn't unconditionally clear the echoed
			   keystrokes, so we don't either */
			/* who cares about RMSmacs 18? */
			if (!is_scrollbar_event(event))
				reset_this_command_keys(CONSOLE_LIVE_P(con) ?
							make_console(con)
							: Qnil, 0);
		}
	}
	UNGCPRO;
}

/* Run the pre command hook. */

static void pre_command_hook(void)
{
	last_point_position = BUF_PT(current_buffer);
	XSETBUFFER(last_point_position_buffer, current_buffer);
	/* This function can GC */
	safe_run_hook_trapping_errors
	    ("Error in `pre-command-hook' (setting hook to nil)",
	     Qpre_command_hook, 1);

	/* This is a kludge, but necessary; see simple.el */
	call0(Qhandle_pre_motion_command);
}

/* Run the post command hook. */

static void post_command_hook(void)
{
	/* This function can GC */
	/* Turn off region highlighting unless this command requested that
	   it be left on, or we're in the minibuffer.  We don't turn it off
	   when we're in the minibuffer so that things like M-x write-region
	   still work!

	   This could be done via a function on the post-command-hook, but
	   we don't want the user to accidentally remove it.
	 */

	Lisp_Object win = Fselected_window(Qnil);

	/* If the last command deleted the frame, `win' might be nil.
	   It seems safest to do nothing in this case. */
	/* Note: Someone added the following comment and put #if 0's around
	   this code, not realizing that doing this invites a crash in the
	   line after. */
	/* #### This doesn't really fix the problem,
	   if delete-frame is called by some hook */
	if (NILP(win))
		return;

	/* This is a kludge, but necessary; see simple.el */
	call0(Qhandle_post_motion_command);

	if (!zmacs_region_stays && (!MINI_WINDOW_P(XWINDOW(win))
				    || EQ(zmacs_region_buffer(),
					  WINDOW_BUFFER(XWINDOW(win)))))
		zmacs_deactivate_region();
	else
		zmacs_update_region();

	safe_run_hook_trapping_errors
	    ("Error in `post-command-hook' (setting hook to nil)",
	     Qpost_command_hook, 1);

	/* #### Kludge!!! This is necessary to make sure that things
	   are properly positioned even if post-command-hook moves point.
	   #### There should be a cleaner way of handling this. */
	call0(Qauto_show_make_point_visible);
}

DEFUN("dispatch-event", Fdispatch_event, 1, 1, 0,	/*
Given an event object EVENT as returned by `next-event', execute it.

Key-press, button-press, and button-release events get accumulated
until a complete key sequence (see `read-key-sequence') is reached,
at which point the sequence is looked up in the current keymaps and
acted upon.

Mouse motion events cause the low-level handling function stored in
`mouse-motion-handler' to be called. (There are very few circumstances
under which you should change this handler.  Use `mode-motion-hook'
instead.)

Menu, timeout, and eval events cause the associated function or handler
to be called.

Process events cause the subprocess's output to be read and acted upon
appropriately (see `start-process').

Magic events are handled as necessary.
							 */
      (event))
{
	/* This function can GC */
	struct command_builder *command_builder;
	Lisp_Event *ev;
	Lisp_Object console;
	Lisp_Object channel;

	CHECK_LIVE_EVENT(event);
	ev = XEVENT(event);

	/* events on dead channels get silently eaten */
	channel = EVENT_CHANNEL(ev);
	if (object_dead_p(channel))
		return Qnil;

	/* Some events don't have channels (e.g. eval events). */
	console = CDFW_CONSOLE(channel);
	if (NILP(console))
		console = Vselected_console;
	else if (!EQ(console, Vselected_console))
		Fselect_console(console);

	command_builder = XCOMMAND_BUILDER(XCONSOLE(console)->command_builder);
	switch (XEVENT(event)->event_type) {
	case button_press_event:
	case button_release_event:
	case key_press_event: {
		Lisp_Object leaf =
			lookup_command_event(command_builder, event, 1);

		if (KEYMAPP(leaf))
			/* Incomplete key sequence */
			break;
		if (NILP(leaf)) {
			/* At this point, we know that the sequence is
			   not bound to a command.  Normally, we beep
			   and print a message informing the user of
			   this.  But we do not beep or print a message
			   when:

			   o  the last event in this sequence is a
			   mouse-up event; or
			   o  the last event in this sequence is a
			   mouse-down event and there is a binding
			   for the mouse-up version.

			   That is, if the sequence ``C-x button1'' is
			   typed, and is not bound to a command, but the
			   sequence ``C-x button1up'' is bound to a
			   command, we do not complain about the ``C-x
			   button1'' sequence.  If neither ``C-x
			   button1'' nor ``C-x button1up'' is bound to a
			   command, then we complain about the ``C-x
			   button1'' sequence, but later will *not*
			   complain about the ``C-x button1up''
			   sequence, which would be redundant.

			   This is pretty hairy, but I think it's the
			   most intuitive behavior.
			*/
			Lisp_Object terminal =
				command_builder->most_current_event;

			if (XEVENT_TYPE(terminal) == button_press_event) {
				int no_bitching;
				/* Temporarily pretend the last event
				   was an "up" instead of a "down", and
				   look up its binding. */
				XEVENT_TYPE(terminal) =
					button_release_event;
				/* If the "up" version is bound, don't
				   complain. */
				no_bitching
					=
					!NILP(command_builder_find_leaf
					      (command_builder, 0));
				/* Undo the temporary changes we just made. */
				XEVENT_TYPE(terminal) =
					button_press_event;
				if (no_bitching) {
					/* Pretend this press was not
					   seen (treat as a prefix) */
					if (EQ
					    (command_builder->
					     current_events,
					     terminal)) {
						reset_current_events
							(command_builder);
					} else {
						Lisp_Object eve;

						EVENT_CHAIN_LOOP(eve,
								 command_builder->
								 current_events)
							if (EQ
							    (XEVENT_NEXT
							     (eve),
							     terminal))
								break;

						Fdeallocate_event
							(command_builder->
							 most_current_event);
						XSET_EVENT_NEXT(eve,
								Qnil);
						command_builder->
							most_current_event =
							eve;
					}
					maybe_echo_keys(command_builder,
							1);
					break;
				}
			}

			/* Complain that the typed sequence is not
			   defined, if this is the kind of sequence that
			   warrants a complaint. */
			XCONSOLE(console)->defining_kbd_macro = Qnil;
			XCONSOLE(console)->prefix_arg = Qnil;
			/* Don't complain about undefined button-release
			   events */
			if (XEVENT_TYPE(terminal) !=
			    button_release_event) {
				Lisp_Object keys =
					current_events_into_vector
					(command_builder);
				struct gcpro gcpro1;

				/* Run the pre-command-hook before
				   barfing about an undefined key. */
				Vthis_command = Qnil;
				GCPRO1(keys);
				pre_command_hook();
				UNGCPRO;
				/* The post-command-hook doesn't run. */
				Fsignal(Qundefined_keystroke_sequence,
					list1(keys));
			}
			/* Reset the command builder for reading the
			   next sequence. */
			reset_this_command_keys(console, 1);
		} else {	/* key sequence is bound to a command */

			int magic_undo = 0;
			int magic_undo_count = 20;

			Vthis_command = leaf;

			/* Don't push an undo boundary if the command
			   set the prefix arg, or if we are executing a
			   keyboard macro, or if in the minibuffer.  If
			   the command we are about to execute is
			   self-insert, it's tricky: up to 20
			   consecutive self-inserts may be done without
			   an undo boundary.  This counter is reset as
			   soon as a command other than
			   self-insert-command is executed.

			   Programmers can also use the
			   `self-insert-defer-undo' property to install
			   that behavior on functions other than
			   `self-insert-command', or to change the magic
			   number 20 to something else.  #### DOCUMENT
			   THIS!  */

			if (SYMBOLP(leaf)) {
				Lisp_Object prop =
					Fget(leaf, Qself_insert_defer_undo,
					     Qnil);
				if (NATNUMP(prop))
					magic_undo =
						1, magic_undo_count =
						XINT(prop);
				else if (!NILP(prop))
					magic_undo = 1;
				else if (EQ(leaf, Qself_insert_command))
					magic_undo = 1;
			}

			if (!magic_undo)
				command_builder->self_insert_countdown =
					0;
			if (NILP(XCONSOLE(console)->prefix_arg)
			    && NILP(Vexecuting_macro)
			    && command_builder->self_insert_countdown ==
			    0)
				Fundo_boundary();

			if (magic_undo) {
				if (--command_builder->
				    self_insert_countdown < 0)
					command_builder->
						self_insert_countdown =
						magic_undo_count;
			}
			execute_command_event
				(command_builder,
				 internal_equal(event,
						command_builder->
						most_current_event, 0)
				 ? event
				 /* Use the translated event that was most
				    recently seen.  This way,
				    last-command-event becomes f1 instead of
				    the P from ESC O P.  But we must copy
				    it, else we'll lose when the
				    command-builder events are
				    deallocated. */
				 : Fcopy_event(command_builder->
					       most_current_event, Qnil));
		}
		break;
	}
	case misc_user_event: {
		/* Jamie said:

		   We could just always use the menu item entry,
		   whatever it is, but this might break some Lisp code
		   that expects `this-command' to always contain a
		   symbol.  So only store it if this is a simple
		   `call-interactively' sort of menu item.

		   But this is bogus.  `this-command' could be a string
		   or vector anyway (for keyboard macros).  There's even
		   one instance (in pending-del.el) of `this-command'
		   getting set to a cons (a lambda expression).  So in
		   the `eval' case I'll just convert it into a lambda
		   expression.
		*/
		if (EQ
		    (XEVENT(event)->event.eval.function,
		     Qcall_interactively)
		    && SYMBOLP(XEVENT(event)->event.eval.object)) {
			Vthis_command =
				XEVENT(event)->event.eval.object;
		} else if (EQ(XEVENT(event)->event.eval.function, Qeval)) {
			Vthis_command =
				Fcons(Qlambda,
				      Fcons(Qnil,
					    XEVENT(event)->event.eval.
					    object));
		} else if (SYMBOLP(XEVENT(event)->event.eval.function)) {
			/* A scrollbar command or the like. */
			Vthis_command =
				XEVENT(event)->event.eval.function;
		} else {
			/* Huh? */
			Vthis_command = Qnil;
		}

		/* clear the echo area */
		reset_key_echo(command_builder, 1);

		command_builder->self_insert_countdown = 0;
		if (NILP(XCONSOLE(console)->prefix_arg)
		    && NILP(Vexecuting_macro)
		    && !EQ(minibuf_window, Fselected_window(Qnil)))
			Fundo_boundary();
		execute_command_event(command_builder, event);
		break;
	}
#ifdef EF_USE_ASYNEQ
	case eaten_myself_event:
		/* try to find the worker in the workers dllist and pop it */
		/* raw :( */
		/* since this affects garbage collection, we better lock that
		   mutex, too */
		lock_allocator();
		WITH_DLLIST_TRAVERSE(
			workers,
			if (ev->event.eaten_myself.worker == dllist_item) {
				dllist_pop_inner(workers, _el);
				break;
			});
		unlock_allocator();
		fini_worker(ev->event.eaten_myself.worker);
		EQUEUE_DEBUG_WORKER("Successfully eaten 0x%lx\n",
				    (long unsigned int)
				    ev->event.eaten_myself.worker);
		break;
	case work_started_event: {
		Lisp_Object ljob = ev->event.work_started.job;
		worker_job_t job = XWORKER_JOB(ljob);
		work_handler_t hdl = XWORKER_JOB_HANDLER(ljob);
		if (hdl && work_started(hdl))
			work_started(hdl)(job);
		break;
	}
	case work_finished_event: {
		Lisp_Object ljob = ev->event.work_finished.job;
		worker_job_t job = XWORKER_JOB(ljob);
		work_handler_t hdl = XWORKER_JOB_HANDLER(ljob);
		if (hdl && work_finished(hdl))
			work_finished(hdl)(job);
		break;
	}
#endif

		/* and the rest */
	case empty_event:
	case pointer_motion_event:
	case process_event:
	case timeout_event:
	case magic_event:
	case magic_eval_event:
	case eval_event:
	case dead_event:
	default:
		execute_internal_event(event);
		break;
	}
	return Qnil;
}

DEFUN("read-key-sequence", Fread_key_sequence, 1, 3, 0,	/*
Read a sequence of keystrokes or mouse clicks.
Returns a vector of the event objects read.  The vector and the event
objects it contains are freshly created (and so will not be side-effected
by subsequent calls to this function).

The sequence read is sufficient to specify a non-prefix command starting
from the current local and global keymaps.  A C-g typed while in this
function is treated like any other character, and `quit-flag' is not set.

First arg PROMPT is a prompt string.  If nil, do not prompt specially.

Second optional arg CONTINUE-ECHO non-nil means this key echoes as a
continuation of the previous key.

Third optional arg DONT-DOWNCASE-LAST non-nil means do not convert the
last event to lower case.  (Normally any upper case event is converted
to lower case if the original event is undefined and the lower case
equivalent is defined.) This argument is provided mostly for FSF
compatibility; the equivalent effect can be achieved more generally by
binding `retry-undefined-key-binding-unshifted' to nil around the call
to `read-key-sequence'.

If the user selects a menu item while we are prompting for a key-sequence,
the returned value will be a vector of a single menu-selection event.
An error will be signalled if you pass this value to `lookup-key' or a
related function.

`read-key-sequence' checks `function-key-map' for function key
sequences, where they wouldn't conflict with ordinary bindings.
See `function-key-map' for more details.
							 */
      (prompt, continue_echo, dont_downcase_last)) {
	/* This function can GC */
	struct console *con = XCONSOLE(Vselected_console);	/* #### correct?
								   Probably not -- see
								   comment in
								   next-event */
	struct command_builder *command_builder =
	    XCOMMAND_BUILDER(con->command_builder);
	Lisp_Object result;
	Lisp_Object event = Fmake_event(Qnil, Qnil);
	int speccount = specpdl_depth();
	struct gcpro gcpro1;
	GCPRO1(event);

	record_unwind_protect(Fset_buffer, Fcurrent_buffer());
	if (!NILP(prompt))
		CHECK_STRING(prompt);
	/* else prompt = Fkeymap_prompt (current_buffer->keymap); may GC */
	QUIT;

	if (NILP(continue_echo))
		reset_this_command_keys(make_console(con), 1);

	specbind(Qinhibit_quit, Qt);

	if (!NILP(dont_downcase_last))
		specbind(Qretry_undefined_key_binding_unshifted, Qnil);

	for (;;) {
		Fnext_event(event, prompt);
		/* restore the selected-console damage */
		con = event_console_or_selected(event);
		command_builder = XCOMMAND_BUILDER(con->command_builder);
		if (!command_event_p(event))
			execute_internal_event(event);
		else {
			if (XEVENT(event)->event_type == misc_user_event)
				reset_current_events(command_builder);
			result =
			    lookup_command_event(command_builder, event, 1);
			if (!KEYMAPP(result)) {
				result =
				    current_events_into_vector(command_builder);
				reset_key_echo(command_builder, 0);
				break;
			}
			prompt = Qnil;
		}
	}

	Vquit_flag = Qnil;	/* In case we read a ^G; do not call
				   check_quit() here */
	Fdeallocate_event(event);
	RETURN_UNGCPRO(unbind_to(speccount, result));
}

DEFUN("this-command-keys", Fthis_command_keys, 0, 0, 0,	/*
Return a vector of the keyboard or mouse button events that were used
to invoke this command.  This copies the vector and the events; it is safe
to keep and modify them.
							 */
      ()) {
	Lisp_Object event;
	Lisp_Object result;
	int len;

	if (NILP(Vthis_command_keys))
		return make_vector(0, Qnil);

	len = event_chain_count(Vthis_command_keys);

	result = make_vector(len, Qnil);
	len = 0;
	EVENT_CHAIN_LOOP(event, Vthis_command_keys)
	    XVECTOR_DATA(result)[len++] = Fcopy_event(event, Qnil);
	return result;
}

DEFUN("reset-this-command-lengths", Freset_this_command_lengths, 0, 0, 0,	/*
Used for complicated reasons in `universal-argument-other-key'.

`universal-argument-other-key' rereads the event just typed.
It then gets translated through `function-key-map'.
The translated event gets included in the echo area and in
the value of `this-command-keys' in addition to the raw original event.
That is not right.

Calling this function directs the translated event to replace
the original event, so that only one version of the event actually
appears in the echo area and in the value of `this-command-keys'.
										 */
      ())
{
	/* #### I don't understand this at all, so currently it does nothing.
	   If there is ever a problem, maybe someone should investigate. */
	return Qnil;
}

static void dribble_out_event(Lisp_Object event)
{
	if (NILP(Vdribble_file))
		return;

	if (XEVENT(event)->event_type == key_press_event &&
	    !XEVENT(event)->event.key.modifiers) {
		Lisp_Object keysym = XEVENT(event)->event.key.keysym;
		if (CHARP(XEVENT(event)->event.key.keysym)) {
			Emchar ch = XCHAR(keysym);
			Bufbyte str[MAX_EMCHAR_LEN];
			Bytecount len = set_charptr_emchar(str, ch);
			Lstream_write(XLSTREAM(Vdribble_file), str, len);
		} else if (string_char_length(XSYMBOL(keysym)->name) == 1)
			/* one-char key events are printed with just the key name */
			Fprinc(keysym, Vdribble_file);
		else if (EQ(keysym, Qreturn))
			Lstream_putc(XLSTREAM(Vdribble_file), '\n');
		else if (EQ(keysym, Qspace))
			Lstream_putc(XLSTREAM(Vdribble_file), ' ');
		else
			Fprinc(event, Vdribble_file);
	} else
		Fprinc(event, Vdribble_file);
	Lstream_flush(XLSTREAM(Vdribble_file));
}

DEFUN("open-dribble-file", Fopen_dribble_file, 1, 1, "FOpen dribble file: ",	/*
Start writing all keyboard characters to a dribble file called FILENAME.
If FILENAME is nil, close any open dribble file.
										 */
      (filename))
{
	/* This function can GC */
	/* XEmacs change: always close existing dribble file. */
	/* FSFmacs uses FILE *'s here.  With lstreams, that's unnecessary. */
	if (!NILP(Vdribble_file)) {
		Lstream_close(XLSTREAM(Vdribble_file));
		Vdribble_file = Qnil;
	}
	if (!NILP(filename)) {
		int fd;

		filename = Fexpand_file_name(filename, Qnil);
		fd = open((char *)XSTRING_DATA(filename),
			  O_WRONLY | O_TRUNC | O_CREAT | OPEN_BINARY,
			  CREAT_MODE);
		if (fd < 0)
			error("Unable to create dribble file");
		Vdribble_file =
		    make_filedesc_output_stream(fd, 0, 0, LSTR_CLOSING);
#ifdef MULE
		Vdribble_file =
		    make_encoding_output_stream(XLSTREAM(Vdribble_file),
						Fget_coding_system
						(Qescape_quoted));
#endif
	}
	return Qnil;
}

DEFUN("current-event-timestamp", Fcurrent_event_timestamp, 0, 1, 0,	/*
Return the current event timestamp of the window system associated with CONSOLE.
CONSOLE defaults to the selected console if omitted.
									 */
      (console))
{
	struct console *c = decode_console(console);
	int tiempo = event_stream_current_event_timestamp(c);

	/* This junk is so that timestamps don't get to be negative, but contain
	   as many bits as this particular emacs will allow.
	 */
	return make_int(EMACS_INT_MAX & tiempo);
}


/* generalised asynchronous worker queue */
#if defined(EF_USE_ASYNEQ)
void
asyneq_handle_event(event_queue_t eq)
{
	if (!eq_queue_empty_p(eq)) {
		Lisp_Object eqev = eq_dequeue(eq);
		Fdispatch_event(eqev);
	}
}

void
asyneq_handle_non_command_event(event_queue_t eq)
{
        Lisp_Object eqev = Qnil;

        WITH_DLLIST_TRAVERSE(
                eq_queue(eq),
                if (!command_event_p((Lisp_Object)dllist_item)) {
                        eqev = (Lisp_Object)dllist_pop_inner(eq_queue(eq), _el);
                        break;
                });

        if (!NILP(eqev))
                execute_internal_event(eqev);
}
#endif

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void syms_of_event_stream(void)
{
	INIT_LRECORD_IMPLEMENTATION(command_builder);
	INIT_LRECORD_IMPLEMENTATION(timeout);

	defsymbol(&Qdisabled, "disabled");
	defsymbol(&Qcommand_event_p, "command-event-p");

	DEFERROR_STANDARD(Qundefined_keystroke_sequence, Qinvalid_argument);

	DEFSUBR(Frecent_keys);
	DEFSUBR(Frecent_keys_ring_size);
	DEFSUBR(Fset_recent_keys_ring_size);
	DEFSUBR(Finput_pending_p);
	DEFSUBR(Fenqueue_eval_event);
	DEFSUBR(Fnext_event);
	DEFSUBR(Fnext_command_event);
	DEFSUBR(Fdiscard_input);
	DEFSUBR(Fsit_for);
	DEFSUBR(Fsleep_for);
	DEFSUBR(Faccept_process_output);
	DEFSUBR(Fadd_timeout);
	DEFSUBR(Fdisable_timeout);
	DEFSUBR(Fadd_async_timeout);
	DEFSUBR(Fdisable_async_timeout);
	DEFSUBR(Fdispatch_event);
	DEFSUBR(Fdispatch_non_command_events);
	DEFSUBR(Fread_key_sequence);
	DEFSUBR(Fthis_command_keys);
	DEFSUBR(Freset_this_command_lengths);
	DEFSUBR(Fopen_dribble_file);
	DEFSUBR(Fcurrent_event_timestamp);

	defsymbol(&Qpre_command_hook, "pre-command-hook");
	defsymbol(&Qpost_command_hook, "post-command-hook");
	defsymbol(&Qunread_command_events, "unread-command-events");
	defsymbol(&Qunread_command_event, "unread-command-event");
	defsymbol(&Qpre_idle_hook, "pre-idle-hook");
	defsymbol(&Qhandle_pre_motion_command, "handle-pre-motion-command");
	defsymbol(&Qhandle_post_motion_command, "handle-post-motion-command");
	defsymbol(&Qretry_undefined_key_binding_unshifted,
		  "retry-undefined-key-binding-unshifted");
	defsymbol(&Qauto_show_make_point_visible,
		  "auto-show-make-point-visible");

	defsymbol(&Qself_insert_defer_undo, "self-insert-defer-undo");
	defsymbol(&Qcancel_mode_internal, "cancel-mode-internal");
}

void reinit_vars_of_event_stream(void)
{
	recent_keys_ring_index = 0;
	recent_keys_ring_size = 100;
	num_input_chars = 0;
#if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
	Vtimeout_free_list = make_lcrecord_list(sizeof(Lisp_Timeout),
						&lrecord_timeout);
	staticpro_nodump(&Vtimeout_free_list);
#endif	/* !BDWGC */
	the_low_level_timeout_blocktype =
		Blocktype_new(struct low_level_timeout_blocktype);
	something_happened = 0;
	recursive_sit_for = Qnil;

#if defined(EF_USE_ASYNEQ)
	/* the main event queue */
	asyneq = make_event_queue();
	XSETEVENT_QUEUE(Vasyneq, asyneq);
	staticpro_nodump(&Vasyneq);
#endif	/* EF_USE_ASYNEQ */
}

void vars_of_event_stream(void)
{
	reinit_vars_of_event_stream();
	Vrecent_keys_ring = Qnil;
	staticpro(&Vrecent_keys_ring);

	Vthis_command_keys = Qnil;
	staticpro(&Vthis_command_keys);
	Vthis_command_keys_tail = Qnil;
	dump_add_root_object(&Vthis_command_keys_tail);

#ifndef EF_USE_ASYNEQ
	command_event_queue = Qnil;
	staticpro(&command_event_queue);
	command_event_queue_tail = Qnil;
	dump_add_root_object(&command_event_queue_tail);
#endif

	Vlast_selected_frame = Qnil;
	staticpro(&Vlast_selected_frame);

	pending_timeout_list = Qnil;
	staticpro(&pending_timeout_list);

	pending_async_timeout_list = Qnil;
	staticpro(&pending_async_timeout_list);

	last_point_position_buffer = Qnil;
	staticpro(&last_point_position_buffer);

	DEFVAR_LISP("echo-keystrokes", &Vecho_keystrokes	/*
*Nonzero means echo unfinished commands after this many seconds of pause.
								 */ );
	Vecho_keystrokes = make_int(1);

	DEFVAR_INT("auto-save-interval", &auto_save_interval	/*
*Number of keyboard input characters between auto-saves.
Zero means disable autosaving due to number of characters typed.
See also the variable `auto-save-timeout'.
								 */ );
	auto_save_interval = 300;

	DEFVAR_LISP("pre-command-hook", &Vpre_command_hook	/*
Function or functions to run before every command.
This may examine the `this-command' variable to find out what command
is about to be run, or may change it to cause a different command to run.
Function on this hook must be careful to avoid signalling errors!
								 */ );
	Vpre_command_hook = Qnil;

	DEFVAR_LISP("post-command-hook", &Vpost_command_hook	/*
Function or functions to run after every command.
This may examine the `this-command' variable to find out what command
was just executed.
								 */ );
	Vpost_command_hook = Qnil;

	DEFVAR_LISP("pre-idle-hook", &Vpre_idle_hook	/*
Normal hook run when SXEmacs it about to be idle.
This occurs whenever it is going to block, waiting for an event.
This generally happens as a result of a call to `next-event',
`next-command-event', `sit-for', `sleep-for', `accept-process-output',
or `x-get-selection'.

Errors running the hook are caught and ignored.
							 */ );
	Vpre_idle_hook = Qnil;

	DEFVAR_BOOL("focus-follows-mouse", &focus_follows_mouse	/*
*Variable to control SXEmacs behavior with respect to focus changing.
If this variable is set to t, then SXEmacs will not gratuitously change
the keyboard focus.  SXEmacs cannot in general detect when this mode is
used by the window manager, so it is up to the user to set it.
								 */ );
	focus_follows_mouse = 0;

	DEFVAR_LISP("last-command-event", &Vlast_command_event	/*
Last keyboard or mouse button event that was part of a command.  This
variable is off limits: you may not set its value or modify the event that
is its value, as it is destructively modified by `read-key-sequence'.  If
you want to keep a pointer to this value, you must use `copy-event'.
								 */ );
	Vlast_command_event = Qnil;

	DEFVAR_LISP("last-command-char", &Vlast_command_char	/*
If the value of `last-command-event' is a keyboard event, then
this is the nearest ASCII equivalent to it.  This is the value that
`self-insert-command' will put in the buffer.  Remember that there is
NOT a 1:1 mapping between keyboard events and ASCII characters: the set
of keyboard events is much larger, so writing code that examines this
variable to determine what key has been typed is bad practice, unless
you are certain that it will be one of a small set of characters.
								 */ );
	Vlast_command_char = Qnil;

	DEFVAR_LISP("last-input-event", &Vlast_input_event	/*
Last keyboard or mouse button event received.  This variable is off
limits: you may not set its value or modify the event that is its value, as
it is destructively modified by `next-event'.  If you want to keep a pointer
to this value, you must use `copy-event'.
								 */ );
	Vlast_input_event = Qnil;

	DEFVAR_LISP("current-mouse-event", &Vcurrent_mouse_event	/*
The mouse-button event which invoked this command, or nil.
This is usually what `(interactive "e")' returns.
									 */ );
	Vcurrent_mouse_event = Qnil;

	DEFVAR_LISP("last-input-char", &Vlast_input_char	/*
If the value of `last-input-event' is a keyboard event, then
this is the nearest ASCII equivalent to it.  Remember that there is
NOT a 1:1 mapping between keyboard events and ASCII characters: the set
of keyboard events is much larger, so writing code that examines this
variable to determine what key has been typed is bad practice, unless
you are certain that it will be one of a small set of characters.
								 */ );
	Vlast_input_char = Qnil;

	DEFVAR_LISP("last-input-time", &Vlast_input_time	/*
The time (in seconds since Jan 1, 1970) of the last-command-event,
represented as a cons of two 16-bit integers.  This is destructively
modified, so copy it if you want to keep it.
								 */ );
	Vlast_input_time = Qnil;

	DEFVAR_LISP("last-command-event-time", &Vlast_command_event_time	/*
The time (in seconds since Jan 1, 1970) of the last-command-event,
represented as a list of three integers.  The first integer contains
the most significant 16 bits of the number of seconds, and the second
integer contains the least significant 16 bits.  The third integer
contains the remainder number of microseconds, if the current system
supports microsecond clock resolution.  This list is destructively
modified, so copy it if you want to keep it.
										 */ );
	Vlast_command_event_time = Qnil;

	DEFVAR_LISP("unread-command-events", &Vunread_command_events	/*
List of event objects to be read as next command input events.
This can be used to simulate the receipt of events from the user.
Normally this is nil.
Events are removed from the front of this list.
									 */ );
	Vunread_command_events = Qnil;

	DEFVAR_LISP("unread-command-event", &Vunread_command_event	/*
Obsolete.  Use `unread-command-events' instead.
									 */ );
	Vunread_command_event = Qnil;

	DEFVAR_LISP("last-command", &Vlast_command	/*
The last command executed.  Normally a symbol with a function definition,
but can be whatever was found in the keymap, or whatever the variable
`this-command' was set to by that command.
							 */ );
	Vlast_command = Qnil;

	DEFVAR_LISP("this-command", &Vthis_command	/*
The command now being executed.
The command can set this variable; whatever is put here
will be in `last-command' during the following command.
							 */ );
	Vthis_command = Qnil;

	DEFVAR_LISP("last-command-properties", &Vlast_command_properties	/*
Value of `this-command-properties' for the last command.
Used by commands to help synchronize consecutive commands, in preference
to looking at `last-command' directly.
										 */ );
	Vlast_command_properties = Qnil;

	DEFVAR_LISP("this-command-properties", &Vthis_command_properties	/*
Properties set by the current command.
At the beginning of each command, the current value of this variable is
copied to `last-command-properties', and then it is set to nil.  Use `putf'
to add properties to this variable.  Commands should use this to communicate
with pre/post-command hooks, subsequent commands, wrapping commands, etc.
in preference to looking at and/or setting `this-command'.
										 */ );
	Vthis_command_properties = Qnil;

	DEFVAR_LISP("help-char", &Vhelp_char	/*
Character to recognize as meaning Help.
When it is read, do `(eval help-form)', and display result if it's a string.
If the value of `help-form' is nil, this char can be read normally.
This can be any form recognized as a single key specifier.
The help-char cannot be a negative number in SXEmacs.
						 */ );
	Vhelp_char = make_char(8);	/* C-h */

	DEFVAR_LISP("help-form", &Vhelp_form	/*
Form to execute when character help-char is read.
If the form returns a string, that string is displayed.
If `help-form' is nil, the help char is not recognized.
						 */ );
	Vhelp_form = Qnil;

	DEFVAR_LISP("prefix-help-command", &Vprefix_help_command	/*
Command to run when `help-char' character follows a prefix key.
This command is used only when there is no actual binding
for that character after that prefix key.
									 */ );
	Vprefix_help_command = Qnil;

	DEFVAR_CONST_LISP("keyboard-translate-table", &Vkeyboard_translate_table	/*
sh table used as translate table for keyboard input.
e `keyboard-translate' to portably add entries to this table.
ch key-press event is looked up in this table as follows:

-- If an entry maps a symbol to a symbol, then a key-press event whose
   keysym is the former symbol (with any modifiers at all) gets its
   keysym changed and its modifiers left alone.  This is useful for
   dealing with non-standard X keyboards, such as the grievous damage
   that Sun has inflicted upon the world.

-- If an entry maps a symbol to a character, then a key-press event
   whose keysym is the former symbol (with any modifiers at all) gets
   changed into a key-press event matching the latter character, and the
   resulting modifiers are the union of the original and new modifiers.

-- If an entry maps a character to a character, then a key-press event
   matching the former character gets converted to a key-press event
   matching the latter character.  This is useful on ASCII terminals
   for (e.g.) making C-\\ look like C-s, to get around flow-control
   problems.

-- If an entry maps a character to a symbol, then a key-press event
   matching the character gets converted to a key-press event whose
   keysym is the given symbol and which has no modifiers.

re's an example: This makes typing parens and braces easier by rerouting
eir positions to eliminate the need to use the Shift key.

(keyboard-translate ?[ ?()
(keyboard-translate ?] ?))
(keyboard-translate ?{ ?[)
(keyboard-translate ?} ?])
(keyboard-translate 'f11 ?{)
(keyboard-translate 'f12 ?})
											 */ );

	DEFVAR_LISP("retry-undefined-key-binding-unshifted", &Vretry_undefined_key_binding_unshifted	/*
If a key-sequence which ends with a shifted keystroke is undefined
and this variable is non-nil then the command lookup is retried again
with the last key unshifted.  (e.g. C-X C-F would be retried as C-X C-f.)
If lookup still fails, a normal error is signalled.  In general,
you should *bind* this, not set it.
													 */ );
	Vretry_undefined_key_binding_unshifted = Qt;

	DEFVAR_BOOL("modifier-keys-are-sticky", &modifier_keys_are_sticky	/*
*Non-nil makes modifier keys sticky.
This means that you can release the modifier key before pressing down
the key that you wish to be modified.  Although this is non-standard
behavior, it is recommended because it reduces the strain on your hand,
thus reducing the incidence of the dreaded Emacs-pinky syndrome.

Modifier keys are sticky within the inverval specified by
`modifier-keys-sticky-time'.
		    */ );
	modifier_keys_are_sticky = 0;

	DEFVAR_LISP("modifier-keys-sticky-time", 
		    &Vmodifier_keys_sticky_time	/*
*Modifier keys are sticky within this many milliseconds.
If you don't want modifier keys sticking to be bounded, set this to
non-integer value.

This variable has no effect when `modifier-keys-are-sticky' is nil.
Currently only implemented under X Window System.
						*/ );
	Vmodifier_keys_sticky_time = make_int(500);

#ifdef HAVE_XIM
	DEFVAR_LISP("composed-character-default-binding", 
		    &Vcomposed_character_default_binding	/*
The default keybinding to use for key events from composed input.
Window systems frequently have ways to allow the user to compose
single characters in a language using multiple keystrokes.
SXEmacs sees these as single character keypress events.
								*/ );
	Vcomposed_character_default_binding = Qself_insert_command;
#endif				/* HAVE_XIM */

	Vcontrolling_terminal = Qnil;
	staticpro(&Vcontrolling_terminal);

	Vdribble_file = Qnil;
	staticpro(&Vdribble_file);

#ifdef DEBUG_SXEMACS
	DEFVAR_INT("debug-emacs-events", &debug_emacs_events	/*
o, display debug information about Emacs events that SXEmacs sees.
n is displayed on stderr.

 event, the source of the event is displayed in parentheses,
 of the following:

real event from the window system or
rminal driver, as far as SXEmacs can tell.

 macro) An event generated from a keyboard macro.

ommand-events) An event taken from `unread-command-events'.

ommand-event) An event taken from `unread-command-event'.

event queue) An event taken from an internal queue.
             Events end up on this queue when
             `enqueue-eval-event' is called or when
             user or eval events are received while
             SXEmacs is blocking (e.g. in `sit-for',
             `sleep-for', or `accept-process-output',
             or while waiting for the reply to an
             X selection).

rd-translate-table) The result of an event translated 
                    through keyboard-translate-table.  Note 
                    that in this case, two events are 
                    printed even though only one is really
                    generated.

A faked C-g resulting when SXEmacs receives
a SIGINT (e.g. C-c was pressed in SXEmacs'
controlling terminal or the signal was
explicitly sent to the SXEmacs process).
								*/ );
	debug_emacs_events = 0;
#endif

	DEFVAR_BOOL("inhibit-input-event-recording",
		    &inhibit_input_event_recording	/*
Non-nil inhibits recording of input-events to recent-keys ring.
							*/ );
	inhibit_input_event_recording = 0;
}

void complex_vars_of_event_stream(void)
{
	Vkeyboard_translate_table =
	    make_lisp_hash_table(100, HASH_TABLE_NON_WEAK, HASH_TABLE_EQ);
}

void init_event_stream(void)
{
	if (initialized) {
#ifdef HAVE_UNIXOID_EVENT_LOOP
		init_event_unixoid();
#endif
#ifdef HAVE_X_WINDOWS
		if (!strcmp(display_use, "x"))
			init_event_Xt_late();
		else
#endif
#ifdef HAVE_GTK
		if (!strcmp(display_use, "gtk"))
			init_event_gtk_late();
		else
#endif
		{
			/* For TTY's, use the Xt event loop if we can; it allows
			   us to later open an X connection. */
#if defined (HAVE_X_WINDOWS) && !defined (DEBUG_TTY_EVENT_STREAM)
			init_event_Xt_late();
#elif defined (HAVE_TTY)
			init_event_tty_late();
#endif
		}
		init_interrupts_late();
	}
}

/*
useful testcases for v18/v19 compatibility:

(defun foo ()
 (interactive)
 (setq unread-command-event (character-to-event ?A (allocate-event)))
 (setq x (list (read-char)
;	  (read-key-sequence "") ; try it with and without this
	  last-command-char last-input-char
	  (recent-keys) (this-command-keys))))
(global-set-key "\^Q" 'foo)

without the read-key-sequence:
  ^Q		==>  (?A ?\^Q ?A [... ^Q] [^Q])
  ^U^U^Q	==>  (?A ?\^Q ?A [... ^U ^U ^Q] [^U ^U ^Q])
  ^U^U^U^G^Q	==>  (?A ?\^Q ?A [... ^U ^U ^U ^G ^Q] [^Q])

with the read-key-sequence:
  ^Qb		==>  (?A [b] ?\^Q ?b [... ^Q b] [b])
  ^U^U^Qb	==>  (?A [b] ?\^Q ?b [... ^U ^U ^Q b] [b])
  ^U^U^U^G^Qb	==>  (?A [b] ?\^Q ?b [... ^U ^U ^U ^G ^Q b] [b])

;the evi-mode command "4dlj.j.j.j.j.j." is also a good testcase (gag)

;(setq x (list (read-char) quit-flag))^J^G
;(let ((inhibit-quit t)) (setq x (list (read-char) quit-flag)))^J^G
;for BOTH, x should get set to (7 t), but no result should be printed.
;; #### According to the doc of quit-flag, second test should return
;; (?\^G nil).  Accidentaly SXEmacs returns correct value.  However,
;; XEmacs 21.1.12 and 21.2.36 both fails on first test.

;also do this: make two frames, one viewing "*scratch*", the other "foo".
;in *scratch*, type (sit-for 20)^J
;wait a couple of seconds, move cursor to foo, type "a"
;a should be inserted in foo.  Cursor highlighting should not change in
;the meantime.

;do it with sleep-for.  move cursor into foo, then back into *scratch*
;before typing.
;repeat also with (accept-process-output nil 20)

;make sure ^G aborts sit-for, sleep-for and accept-process-output:

 (defun tst ()
  (list (condition-case c
	    (sleep-for 20)
	  (quit c))
	(read-char)))

 (tst)^Ja^G    ==>  ((quit) ?a) with no signal
 (tst)^J^Ga    ==>  ((quit) ?a) with no signal
 (tst)^Jabc^G  ==>  ((quit) ?a) with no signal, and "bc" inserted in buffer

; with sit-for only do the 2nd test.
; Do all 3 tests with (accept-process-output nil 20)

Do this:
  (setq enable-recursive-minibuffers t
      minibuffer-max-depth nil)
 ESC ESC ESC ESC	- there are now two minibuffers active
 C-g C-g C-g		- there should be active 0, not 1
Similarly:
 C-x C-f ~ / ?		- wait for "Making completion list..." to display
 C-g			- wait for "Quit" to display
 C-g			- minibuffer should not be active
however C-g before "Quit" is displayed should leave minibuffer active.

;do it all in both v18 and v19 and make sure all results are the same.
;all of these cases matter a lot, but some in quite subtle ways.
*/

/*
Additional test cases for accept-process-output, sleep-for, sit-for.
Be sure you do all of the above checking for C-g and focus, too!

; Make sure that timer handlers are run during, not after sit-for:
(defun timer-check ()
  (add-timeout 2 '(lambda (ignore) (message "timer ran")) nil)
  (sit-for 5)
  (message "after sit-for"))

; The first message should appear after 2 seconds, and the final message
; 3 seconds after that.
; repeat above test with (sleep-for 5) and (accept-process-output nil 5)

; Make sure that process filters are run during, not after sit-for.
(defun fubar ()
  (message "sit-for = %s" (sit-for 30)))
(add-hook 'post-command-hook 'fubar)

; Now type M-x shell RET
; wait for the shell prompt then send: ls RET
; the output of ls should fill immediately, and not wait 30 seconds.

; repeat above test with (sleep-for 30) and (accept-process-output nil 30)

; Make sure that recursive invocations return immediately:
(defmacro test-diff-time (start end)
  `(+ (* (- (car ,end) (car ,start)) 65536.0)
      (- (cadr ,end) (cadr ,start))
      (/ (- (caddr ,end) (caddr ,start)) 1000000.0)))

(defun testee (ignore)
  (sit-for 10))

(defun test-them ()
  (let ((start (current-time))
        end)
    (add-timeout 2 'testee nil)
    (sit-for 5)
    (add-timeout 2 'testee nil)
    (sleep-for 5)
    (add-timeout 2 'testee nil)
    (accept-process-output nil 5)
    (setq end (current-time))
    (test-diff-time start end)))

(test-them) should sit for 15 seconds.
Repeat with testee set to sleep-for and accept-process-output.
These should each delay 36 seconds.

*/
