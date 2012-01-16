/* GPM (General purpose mouse) functions
   Copyright (C) 1997 William M. Perry <wmperry@gnu.org>
   Copyright (C) 1999 Free Software Foundation, Inc.

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

/* Authors: William Perry */

#include <config.h>
#include "lisp.h"
#include "ui/console.h"
#include "console-tty.h"
#include "ui/device.h"
#define INCLUDE_EVENTS_H_PRIVATE_SPHERE
#include "events/events.h"
#include "events/events-mod.h"
#include "sysdep.h"
#include "commands.h"
#include "lstream.h"
#include "sysproc.h"		/* for MAXDESC */
#include "process.h"

#ifdef HAVE_GPM
#include "gpmevent.h"
#include <gpm.h>

#if (!defined(__linux__))	/* possible under xterm */
#define KG_SHIFT	0
#define KG_CTRL		2
#define KG_ALT		3
#else
#include <linux/keyboard.h>
#endif

extern int gpm_tried;
extern void *gpm_stack;

static int (*orig_event_pending_p) (int);
static void (*orig_next_event_cb) (Lisp_Event *);

static Lisp_Object gpm_event_queue;
static Lisp_Object gpm_event_queue_tail;

struct __gpm_state {
	int gpm_tried;
	int gpm_flag;
	void *gpm_stack;
};

static struct __gpm_state gpm_state_information[MAXDESC];

static void store_gpm_state(int fd)
{
	if (fd < 0) {
		warn_when_safe(Qnil, Qcritical, "store_gpm_state negative fd - %d",
			       fd);
		return;
	}
	gpm_state_information[fd].gpm_tried = gpm_tried;
	gpm_state_information[fd].gpm_flag = gpm_flag;
	gpm_state_information[fd].gpm_stack = gpm_stack;
}

static void restore_gpm_state(int fd)
{
	if (fd < 0) {
		warn_when_safe(Qnil, Qcritical, "restore_gpm_state negative fd - %d",
			       fd);
		return;
	}
	gpm_tried = gpm_state_information[fd].gpm_tried;
	gpm_flag = gpm_state_information[fd].gpm_flag;
	gpm_stack = gpm_state_information[fd].gpm_stack;
	gpm_consolefd = gpm_fd = fd;
}

static void clear_gpm_state(int fd)
{
	if (fd >= 0) {
		memset(&gpm_state_information[fd], '\0',
		       sizeof(struct __gpm_state));
	}
	gpm_tried = gpm_flag = 1;
	gpm_fd = gpm_consolefd = -1;
	gpm_stack = NULL;
}

static int get_process_infd(Lisp_Process * p)
{
	Lisp_Object instr, outstr;
	get_process_streams(p, &instr, &outstr);
	assert(!NILP(instr));
	return Lstream_get_fd(XLSTREAM(instr));
}

DEFUN("receive-gpm-event", Freceive_gpm_event, 0, 2, 0,	/*
Run GPM_GetEvent().
This function is the process handler for the GPM connection.
*/
      (process, string))
{
	Gpm_Event ev;
	int modifiers = 0;
	int button = 1;
	Lisp_Object fake_event = Qnil;
	Lisp_Event *event = NULL;
	struct gcpro gcpro1;
	static int num_events;

	CHECK_PROCESS(process);

	restore_gpm_state(get_process_infd(XPROCESS(process)));

	if (!Gpm_GetEvent(&ev)) {
		warn_when_safe(Qnil, Qcritical, "Gpm_GetEvent failed - %d",
			       gpm_fd);
		return (Qzero);
	}

	GCPRO1(fake_event);

	num_events++;

	fake_event = Fmake_event(Qnil, Qnil);
	event = XEVENT(fake_event);

	event->timestamp = 0;
	event->channel = Fselected_frame(Qnil);	/* CONSOLE_SELECTED_FRAME (con); */

	/* Whow, wouldn't named defines be NICE!?!?! */
	modifiers = 0;

	if (ev.modifiers & 1)
		modifiers |= XEMACS_MOD_SHIFT;
	if (ev.modifiers & 2)
		modifiers |= XEMACS_MOD_META;
	if (ev.modifiers & 4)
		modifiers |= XEMACS_MOD_CONTROL;
	if (ev.modifiers & 8)
		modifiers |= XEMACS_MOD_META;

	if (ev.buttons & GPM_B_LEFT) {
		button = 1;
	} else if (ev.buttons & GPM_B_MIDDLE) {
		button = 2;
	} else if (ev.buttons & GPM_B_RIGHT) {
		button = 3;
	}

	switch (GPM_BARE_EVENTS(ev.type)) {
	case GPM_DOWN:
	case GPM_UP:
		event->event_type =
		    (ev.
		     type & GPM_DOWN) ? button_press_event :
		    button_release_event;
		event->event.button.x = ev.x;
		event->event.button.y = ev.y;
		event->event.button.button = button;
		event->event.button.modifiers = modifiers;
		break;
	case GPM_MOVE:
	case GPM_DRAG:
		event->event_type = pointer_motion_event;
		event->event.motion.x = ev.x;
		event->event.motion.y = ev.y;
		event->event.motion.modifiers = modifiers;
	default:
		/* This will never happen */
		break;
	}

	/* Handle the event */
	enqueue_event(fake_event, &gpm_event_queue, &gpm_event_queue_tail);

	UNGCPRO;

	return (Qzero);
}

static void turn_off_gpm(char *process_name)
{
	Lisp_Object process = Fget_process(build_string(process_name));
	int fd = -1;

	if (NILP(process)) {
		/* Something happened to our GPM process - fail silently */
		return;
	}

	fd = get_process_infd(XPROCESS(process));

	restore_gpm_state(fd);

	Gpm_Close();

	clear_gpm_state(fd);

	Fdelete_process(build_string(process_name));
}

#ifdef TIOCLINUX
static Lisp_Object
tty_get_foreign_selection(Lisp_Object selection_symbol, Lisp_Object target_type)
{
	/* This function can GC */
	struct device *d = decode_device(Qnil);
	int fd = DEVICE_INFD(d);
	char c = 3;
	Lisp_Object output_stream = Qnil;
	Lisp_Object terminal_stream = Qnil;
	Lisp_Object output_string = Qnil;
	struct gcpro gcpro1, gcpro2, gcpro3;

	GCPRO3(output_stream, terminal_stream, output_string);

	/* The ioctl() to paste actually puts things in the input queue of
	 ** the virtual console, so we need to trap that data, since we are
	 ** supposed to return the actual string selection from this
	 ** function.
	 */

	/* I really hate doing this, but it doesn't seem to cause any
	 ** problems, and it makes the Lstream_read stuff further down
	 ** error out correctly instead of trying to indefinitely read from
	 ** the console.
	 **
	 ** There is no set_descriptor_blocking() function call, but in my
	 ** testing under linux, it has not proved fatal to leave the
	 ** descriptor in non-blocking mode.
	 **
	 ** William Perry Nov 5, 1999
	 */
	set_descriptor_non_blocking(fd);

	/* We need two streams, one for reading from the selected device,
	 ** and one to write the data into.  There is no writable version
	 ** of the lisp-string lstream, so we make do with a resizing
	 ** buffer stream, and make a string out of it after we are
	 ** done.
	 */
	output_stream = make_resizing_buffer_output_stream();
	terminal_stream =
	    make_filedesc_input_stream(fd, 0, -1, LSTR_BLOCKED_OK);
	output_string = Qnil;

	/* #### We should arguably use a specbind() and an unwind routine here,
	 ** #### but I don't care that much right now.
	 */
	if (NILP(output_stream) || NILP(terminal_stream)) {
		/* Should we signal an error here? */
		goto out;
	}

	if (ioctl(fd, TIOCLINUX, &c) < 0) {
		/* Could not get the selection - eek */
		UNGCPRO;
		return (Qnil);
	}

	while (1) {
		Bufbyte tempbuf[1024];	/* some random amount */
		Lstream_data_count i;
		Lstream_data_count size_in_bytes =
		    Lstream_read(XLSTREAM(terminal_stream),
				 tempbuf, sizeof(tempbuf));

		if (size_in_bytes <= 0) {
			/* end of the stream */
			break;
		}

		/* convert CR->LF */
		for (i = 0; i < size_in_bytes; i++) {
			if (tempbuf[i] == '\r') {
				tempbuf[i] = '\n';
			}
		}

		Lstream_write(XLSTREAM(output_stream), tempbuf, size_in_bytes);
	}

	Lstream_flush(XLSTREAM(output_stream));

	output_string =
	    make_string(resizing_buffer_stream_ptr(XLSTREAM(output_stream)),
			Lstream_byte_count(XLSTREAM(output_stream)));

	Lstream_delete(XLSTREAM(output_stream));
	Lstream_delete(XLSTREAM(terminal_stream));

      out:
	UNGCPRO;
	return (output_string);
}

static Lisp_Object
tty_selection_exists_p(Lisp_Object selection, Lisp_Object selection_type)
{
	return (Qt);
}
#endif				/* TIOCLINUX */

#if 0
static Lisp_Object
tty_own_selection(Lisp_Object selection_name, Lisp_Object selection_value,
		  Lisp_Object how_to_add, Lisp_Object selection_type)
{
	/* There is no way to do this cleanly - the GPM selection
	 ** 'protocol' (actually the TIOCLINUX ioctl) requires a start and
	 ** end position on the _screen_, not a string to stick in there.
	 ** Lame.
	 **
	 ** William Perry Nov 4, 1999
	 */
}
#endif

/* This function appears to work once in a blue moon.  I'm not sure
** exactly why either.  *sigh*
**
** William Perry Nov 4, 1999
**
** Apparently, this is the way (mouse-position) is supposed to work,
** and I was just expecting something else.  (mouse-pixel-position)
** works just fine.
**
** William Perry Nov 7, 1999
*/
static int
tty_get_mouse_position(struct device *d, Lisp_Object * frame, int *x, int *y)
{
	Gpm_Event ev;
	int num_buttons;

	memset(&ev, '\0', sizeof(ev));

	num_buttons = Gpm_GetSnapshot(&ev);

	if (!num_buttons) {
		/* This means there are events pending... */

		/* #### In theory, we should drain the events pending, stick
		 ** #### them in the queue, and return the mouse position
		 ** #### anyway.
		 */
		return (-1);
	}
	*x = ev.x;
	*y = ev.y;
	*frame = DEVICE_SELECTED_FRAME(d);
	return (1);
}

static void tty_set_mouse_position(struct window *w, int x, int y)
{
	/*
	   #### I couldn't find any GPM functions that set the mouse position.
	   #### Mr. Perry had left this function empty; that must be why.
	   #### karlheg
	 */
}

static int gpm_event_pending_p(int user_p)
{
	Lisp_Object event;

	EVENT_CHAIN_LOOP(event, gpm_event_queue) {
		if (!user_p || command_event_p(event)) {
			return (1);
		}
	}
	return (orig_event_pending_p(user_p));
}

static void gpm_next_event_cb(Lisp_Event * event)
{
	/* #### It would be nice to preserve some sort of ordering of the
	 ** #### different types of events, but that would be quite a bit
	 ** #### of work, and would more than likely break the abstraction
	 ** #### between the other event loops and this one.
	 */

	if (!NILP(gpm_event_queue)) {
		Lisp_Object queued_event =
		    dequeue_event(&gpm_event_queue, &gpm_event_queue_tail);
		*event = *(XEVENT(queued_event));

		if (event->event_type == pointer_motion_event) {
			struct device *d = decode_device(event->channel);
			int fd = DEVICE_INFD(d);

			/* Ok, now this is just freaky.  Bear with me though.
			 **
			 ** If you run gnuclient and attach to a SXEmacs running in
			 ** X or on another TTY, the mouse cursor does not get
			 ** drawn correctly.  This is because the ioctl() fails
			 ** with EPERM because the TTY specified is not our
			 ** controlling terminal.  If you are the superuser, it
			 ** will work just spiffy.  The appropriate source file (at
			 ** least in linux 2.2.x) is
			 ** .../linux/drivers/char/console.c in the function
			 ** tioclinux().  The following bit of code is brutal to
			 ** us:
			 **
			 ** if (current->tty != tty && !suser())
			 **    return -EPERM;
			 **
			 ** I even tried setting us as a process leader, removing
			 ** our controlling terminal, and then using the TIOCSCTTY
			 ** to set up a new controlling terminal, all with no luck.
			 **
			 ** What is even weirder is if you run SXEmacs in a VC, and
			 ** attach to it from another VC with gnuclient, go back to
			 ** the original VC and hit a key, the mouse pointer
			 ** displays (in BOTH VCs), until you hit a key in the
			 ** second VC, after which it does not display in EITHER
			 ** VC.  Bizarre, no?
			 **
			 ** All I can say is thank god Linux comes with source code
			 ** or I would have been completely confused.  Well, ok,
			 ** I'm still completely confused.  I don't see why they
			 ** don't just check the permissions on the device
			 ** (actually, if you have enough access to it to get the
			 ** console's file descriptor, you should be able to do
			 ** with it as you wish, but maybe that is just me).
			 **
			 ** William M. Perry - Nov 9, 1999
			 */

			Gpm_DrawPointer(event->event.motion.x,
					event->event.motion.y, fd);
		}

		return;
	}

	orig_next_event_cb(event);
}

static void hook_event_callbacks_once(void)
{
	static int hooker;

	if (!hooker) {
		orig_event_pending_p = event_stream->event_pending_p;
		orig_next_event_cb = event_stream->next_event_cb;
		event_stream->event_pending_p = gpm_event_pending_p;
		event_stream->next_event_cb = gpm_next_event_cb;
		hooker = 1;
	}
}

static void hook_console_methods_once(void)
{
	static int hooker;

	if (!hooker) {
		/* Install the mouse position methods for the TTY console type */
		CONSOLE_HAS_METHOD(tty, get_mouse_position);
		CONSOLE_HAS_METHOD(tty, set_mouse_position);
		CONSOLE_HAS_METHOD(tty, get_foreign_selection);
		CONSOLE_HAS_METHOD(tty, selection_exists_p);
#if 0
		CONSOLE_HAS_METHOD(tty, own_selection);
#endif
	}
}

DEFUN("gpm-enabled-p", Fgpm_enabled_p, 0, 1, 0,	/*
Return non-nil if GPM mouse support is currently enabled on DEVICE.
*/
      (device))
{
	char *console_name = ttyname(DEVICE_INFD(decode_device(device)));
	char process_name[1024];
	int sz;
	Lisp_Object proc;

	if (!console_name) {
		return (Qnil);
	}

	sz = snprintf(process_name, sizeof(process_name), "gpm for %s",
		      console_name);
	assert(sz >= 0 && sz < sizeof(process_name));

	proc = Fget_process(build_string(process_name));

	if (NILP(proc)) {
		return (Qnil);
	}

	if (1) {		/* (PROCESS_LIVE_P (proc)) */
		return (Qt);
	}
	return (Qnil);
}

DEFUN("gpm-enable", Fgpm_enable, 0, 2, 0,	/*
Toggle accepting of GPM mouse events.
*/
      (device, arg))
{
	Gpm_Connect conn;
	int rval, sz;
	Lisp_Object gpm_process;
	Lisp_Object gpm_filter;
	struct device *d = decode_device(device);
	int fd = DEVICE_INFD(d);
	char *console_name = ttyname(fd);
	char process_name[1024];

	hook_event_callbacks_once();
	hook_console_methods_once();

	if (noninteractive) {
		error("Can't connect to GPM in batch mode.");
	}

	if (!console_name) {
		/* Something seriously wrong here... */
		return (Qnil);
	}

	sz = snprintf(process_name, sizeof(process_name), "gpm for %s",
		      console_name);
	assert(sz >= 0 && sz < sizeof(process_name));

	if (NILP(arg)) {
		turn_off_gpm(process_name);
		return (Qnil);
	}

	/* DANGER DANGER.
	 ** Though shalt not call (gpm-enable t) after we have already
	 ** started, or stuff blows up.
	 */
	if (!NILP(Fgpm_enabled_p(device))) {
		error("GPM already enabled for this console.");
	}

	conn.eventMask = GPM_DOWN | GPM_UP | GPM_MOVE | GPM_DRAG;
	conn.defaultMask = GPM_MOVE;
	conn.minMod = 0;
	conn.maxMod = ((1 << KG_SHIFT) | (1 << KG_ALT) | (1 << KG_CTRL));

	/* Reset some silly static variables so that multiple Gpm_Open()
	 ** calls have even a slight chance of working
	 */
	gpm_tried = 0;
	gpm_flag = 0;
	gpm_stack = NULL;

	/* Make sure Gpm_Open() does ioctl() on the correct
	 ** descriptor, or it can get the wrong terminal sizes, etc.
	 */
	gpm_consolefd = fd;

	/* We have to pass the virtual console manually, otherwise if you
	 ** use 'gnuclient -nw' to connect to an SXEmacs that is running in
	 ** X, Gpm_Open() tries to use ttyname(0 | 1 | 2) to find out which
	 ** console you are using, which is of course not correct for the
	 ** new tty device.
	 */
	if (strncmp(console_name, "/dev/tty", 8) || !isdigit(console_name[8])) {
		/* Urk, something really wrong */
		return (Qnil);
	}

	rval = Gpm_Open(&conn, atoi(console_name + 8));

	switch (rval) {
	case -1:		/* General failure */
		break;
	case -2:		/* We are running under an XTerm */
		Gpm_Close();
		break;
	default:
		/* Is this really necessary? */
		set_descriptor_non_blocking(gpm_fd);
		store_gpm_state(gpm_fd);
		gpm_process =
		    connect_to_file_descriptor(build_string(process_name), Qnil,
					       make_int(gpm_fd),
					       make_int(gpm_fd));

		if (!NILP(gpm_process)) {
			rval = 0;
			Fprocess_kill_without_query(gpm_process, Qnil);
			XSETSUBR(gpm_filter, &SFreceive_gpm_event);
			set_process_filter(gpm_process, gpm_filter, 1);

			/* Keep track of the device for later */
			/* Fput (gpm_process, intern ("gpm-device"), device); */
		} else {
			Gpm_Close();
			rval = -1;
		}
	}

	return (rval ? Qnil : Qt);
}

void vars_of_gpmevent(void)
{
	gpm_event_queue = Qnil;
	gpm_event_queue_tail = Qnil;
	staticpro(&gpm_event_queue);
	staticpro(&gpm_event_queue_tail);
	dump_add_root_object(&gpm_event_queue);
	dump_add_root_object(&gpm_event_queue_tail);
}

void syms_of_gpmevent(void)
{
	DEFSUBR(Freceive_gpm_event);
	DEFSUBR(Fgpm_enable);
	DEFSUBR(Fgpm_enabled_p);
}

#endif				/* HAVE_GPM */
