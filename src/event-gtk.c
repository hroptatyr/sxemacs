/* The event_stream interface for X11 with gtk, and/or tty frames.
   Copyright (C) 1991-5, 1997 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1996 Ben Wing.
   Copyright (C) 2000 William Perry.

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


/* This file is heavily based upon event-Xt.c */

/* Synched up with: Not in FSF. */

#include <config.h>
#include "lisp.h"

#include "console-gtk.h"

#include "blocktype.h"
#include "buffer.h"
#include "commands.h"
#include "console.h"
#include "console-tty.h"
#include "events.h"
#include "frame.h"
#include "objects-gtk.h"
#include "process.h"
#include "redisplay.h"
#include "elhash.h"

#include "gtk-xemacs.h"

#include "systime.h"
#include "sysproc.h"		/* for MAXDESC */

#ifdef FILE_CODING
#include "lstream.h"
#include "file-coding.h"
#endif

#include <gdk/gdkkeysyms.h>

#ifdef HAVE_DRAGNDROP
#include "dragdrop.h"
#endif

#ifdef HAVE_MENUBARS
# include "menubar.h"
#endif

#if defined (HAVE_OFFIX_DND)
#include "offix.h"
#endif

#include "events-mod.h"

#include <gdk/gdkx.h>

static struct event_stream *gtk_event_stream;

/* Do we accept events sent by other clients? */
int gtk_allow_sendevents;

static int process_events_occurred;
static int tty_events_occurred;

/* Mask of bits indicating the descriptors that we wait for input on */
extern SELECT_TYPE input_wait_mask, process_only_mask, tty_only_mask;

static Lisp_Object gtk_keysym_to_emacs_keysym();
void debug_process_finalization(struct Lisp_Process *p);
gboolean emacs_gtk_event_handler(GtkWidget * wid /* unused */ ,
				 GdkEvent * event,
				 gpointer closure /* unused */ );

static int last_quit_check_signal_tick_count;

Lisp_Object Qkey_mapping;
Lisp_Object Qsans_modifiers;

void enqueue_gtk_dispatch_event(Lisp_Object event);

/*
 * Identify if the keysym is a modifier.  This implementation mirrors x.org's
 * IsModifierKey(), but for GDK keysyms.
 */
#ifdef GDK_ISO_Lock
#define IS_MODIFIER_KEY(keysym)						\
	((((keysym) >= GDK_Shift_L) && ((keysym) <= GDK_Hyper_R))	\
	 || (((keysym) >= GDK_ISO_Lock) &&				\
	     ((keysym) <= GDK_ISO_Last_Group_Lock))			\
	 || ((keysym) == GDK_Mode_switch)				\
	 || ((keysym) == GDK_Num_Lock))
#else
#define IS_MODIFIER_KEY(keysym)						\
	((((keysym) >= GDK_Shift_L) && ((keysym) <= GDK_Hyper_R))	\
	 || ((keysym) == GDK_Mode_switch)				\
	 || ((keysym) == GDK_Num_Lock))
#endif


/************************************************************************/
/*                           magic-event handling                       */
/************************************************************************/
static void handle_focus_event_1(struct frame *f, int in_p)
{
	/* We don't want to handle the focus change now, because we might
	   be in an accept-process-output, sleep-for, or sit-for.  So
	   we enqueue it.

	   Actually, we half handle it: we handle it as far as changing the
	   box cursor for redisplay, but we don't call any hooks or do any
	   select-frame stuff until after the sit-for.
	 */

	if (in_p) {
		GTK_WIDGET_SET_FLAGS(FRAME_GTK_TEXT_WIDGET(f), GTK_HAS_FOCUS);
	} else {
		GTK_WIDGET_UNSET_FLAGS(FRAME_GTK_TEXT_WIDGET(f), GTK_HAS_FOCUS);
	}
	gtk_widget_grab_focus(FRAME_GTK_TEXT_WIDGET(f));
	gtk_widget_draw_focus(FRAME_GTK_TEXT_WIDGET(f));

	{
		Lisp_Object frm;
		Lisp_Object conser;
		struct gcpro gcpro1;

		XSETFRAME(frm, f);
		conser = Fcons(frm, Fcons(FRAME_DEVICE(f), in_p ? Qt : Qnil));
		GCPRO1(conser);

		emacs_handle_focus_change_preliminary(conser);
		enqueue_magic_eval_event(emacs_handle_focus_change_final,
					 conser);
		UNGCPRO;
	}
}

/* both GDK_MAP and GDK_VISIBILITY_NOTIFY can cause this
   JV is_visible has the same semantics as f->visible*/
static void change_frame_visibility(struct frame *f, int is_visible)
{
	Lisp_Object frame;

	XSETFRAME(frame, f);

	if (!FRAME_VISIBLE_P(f) && is_visible) {
		FRAME_VISIBLE_P(f) = is_visible;
		/* This improves the double flicker when uniconifying a frame
		   some.  A lot of it is not showing a buffer which has changed
		   while the frame was iconified.  To fix it further requires
		   the good 'ol double redisplay structure. */
		MARK_FRAME_WINDOWS_STRUCTURE_CHANGED(f);
		va_run_hook_with_args(Qmap_frame_hook, 1, frame);
	} else if (FRAME_VISIBLE_P(f) && !is_visible) {
		FRAME_VISIBLE_P(f) = 0;
		va_run_hook_with_args(Qunmap_frame_hook, 1, frame);
	} else if (FRAME_VISIBLE_P(f) * is_visible < 0) {
		FRAME_VISIBLE_P(f) = -FRAME_VISIBLE_P(f);
		if (FRAME_REPAINT_P(f))
			MARK_FRAME_WINDOWS_STRUCTURE_CHANGED(f);
		va_run_hook_with_args(Qmap_frame_hook, 1, frame);
	}
}

static void handle_map_event(struct frame *f, GdkEvent * event)
{
	Lisp_Object frame;

	XSETFRAME(frame, f);
	if (event->any.type == GDK_MAP) {
		FRAME_GTK_TOTALLY_VISIBLE_P(f) = 1;
		change_frame_visibility(f, 1);
	} else {
		FRAME_GTK_TOTALLY_VISIBLE_P(f) = 0;
		change_frame_visibility(f, 0);
		/* Calling Fframe_iconified_p is the only way we have to
		   correctly update FRAME_ICONIFIED_P */
		Fframe_iconified_p(frame);
	}
}

static void handle_client_message(struct frame *f, GdkEvent * event)
{
	Lisp_Object frame;

	XSETFRAME(frame, f);

	/* The event-Xt code used to handle WM_DELETE_WINDOW here, but we
	   handle that directly in frame-gtk.c */

	if (event->client.message_type == gdk_atom_intern("WM_PROTOCOLS", 0) &&
	    (GdkAtom) event->client.data.l[0] ==
	    gdk_atom_intern("WM_TAKE_FOCUS", 0)) {
		handle_focus_event_1(f, 1);
	}
}

static void emacs_gtk_handle_magic_event(struct Lisp_Event *emacs_event)
{
	/* This function can GC */
	GdkEvent *event = &emacs_event->event.magic.underlying_gdk_event;
	struct frame *f = XFRAME(EVENT_CHANNEL(emacs_event));

	if (!FRAME_LIVE_P(f))
		return;

	switch (event->any.type) {
	case GDK_CLIENT_EVENT:
		handle_client_message(f, event);
		break;

	case GDK_FOCUS_CHANGE:
		handle_focus_event_1(f, event->focus_change.in);
		break;

	case GDK_MAP:
	case GDK_UNMAP:
		handle_map_event(f, event);
		break;

	case GDK_ENTER_NOTIFY:
		if (event->crossing.detail != GDK_NOTIFY_INFERIOR) {
			Lisp_Object frame;

			XSETFRAME(frame, f);
			/* FRAME_X_MOUSE_P (f) = 1; */
			va_run_hook_with_args(Qmouse_enter_frame_hook, 1,
					      frame);
		}
		break;

	case GDK_LEAVE_NOTIFY:
		if (event->crossing.detail != GDK_NOTIFY_INFERIOR) {
			Lisp_Object frame;

			XSETFRAME(frame, f);
			/* FRAME_X_MOUSE_P (f) = 0; */
			va_run_hook_with_args(Qmouse_leave_frame_hook, 1,
					      frame);
		}
		break;

	case GDK_VISIBILITY_NOTIFY:	/* window visiblity has changed */
		if (event->visibility.window ==
		    GET_GTK_WIDGET_WINDOW(FRAME_GTK_SHELL_WIDGET(f))) {
			FRAME_GTK_TOTALLY_VISIBLE_P(f) =
			    (event->visibility.state ==
			     GDK_VISIBILITY_UNOBSCURED);
			/* Note that the fvwm pager only sends VisibilityNotify when
			   changing pages. Is this all we need to do ? JV */
			/* Nope.  We must at least trigger a redisplay here.
			   Since this case seems similar to MapNotify, I've
			   factored out some code to change_frame_visibility().
			   This triggers the necessary redisplay and runs
			   (un)map-frame-hook.  - dkindred@cs.cmu.edu */
			/* Changed it again to support the tristate visibility flag */
			change_frame_visibility(f, (event->visibility.state
						    !=
						    GDK_VISIBILITY_FULLY_OBSCURED)
						? 1 : -1);
		}
		break;

	default:
		break;
	}
}

/************************************************************************/
/*                 Gtk to Emacs event conversion                        */
/************************************************************************/

static int keysym_obeys_caps_lock_p(guint sym, struct device *d)
{
	struct gtk_device *gd = DEVICE_GTK_DATA(d);
	/* Eeeeevil hack.  Don't apply Caps_Lock to things that aren't alphabetic
	   characters, where "alphabetic" means something more than simply A-Z.
	   That is, if Caps_Lock is down, typing ESC doesn't produce Shift-ESC.
	   But if shift-lock is down, then it does. */
	if (gd->lock_interpretation == GDK_Shift_Lock)
		return 1;

	return
	    ((sym >= GDK_A) && (sym <= GDK_Z)) ||
	    ((sym >= GDK_a) && (sym <= GDK_z)) ||
	    ((sym >= GDK_Agrave) && (sym <= GDK_Odiaeresis)) ||
	    ((sym >= GDK_agrave) && (sym <= GDK_odiaeresis)) ||
	    ((sym >= GDK_Ooblique) && (sym <= GDK_Thorn)) ||
	    ((sym >= GDK_oslash) && (sym <= GDK_thorn));
}

static void set_last_server_timestamp(struct device *d, GdkEvent * gdk_event)
{
	guint32 t;
	switch (gdk_event->type) {
	case GDK_KEY_PRESS:
	case GDK_KEY_RELEASE:
		t = gdk_event->key.time;
		break;
	case GDK_BUTTON_PRESS:
	case GDK_2BUTTON_PRESS:
	case GDK_3BUTTON_PRESS:
	case GDK_BUTTON_RELEASE:
		t = gdk_event->button.time;
		break;
	case GDK_ENTER_NOTIFY:
	case GDK_LEAVE_NOTIFY:
		t = gdk_event->crossing.time;
		break;
	case GDK_MOTION_NOTIFY:
		t = gdk_event->motion.time;
		break;
	case GDK_PROPERTY_NOTIFY:
		t = gdk_event->property.time;
		break;
	case GDK_SELECTION_CLEAR:
	case GDK_SELECTION_REQUEST:
	case GDK_SELECTION_NOTIFY:
		t = gdk_event->selection.time;
		break;
	default:
		return;
	}
	DEVICE_GTK_LAST_SERVER_TIMESTAMP(d) = t;
}

static Lisp_Object gtk_keysym_to_emacs_keysym(guint keysym, int simple_p)
{
	char *name;
	if (keysym >= GDK_exclam && keysym <= GDK_asciitilde)
		/* We must assume that the X keysym numbers for the ASCII graphic
		   characters are the same as their ASCII codes.  */
		return make_char(keysym);

	switch (keysym) {
		/* These would be handled correctly by the default case, but by
		   special-casing them here we don't garbage a string or call
		   intern().  */
	case GDK_BackSpace:
		return QKbackspace;
	case GDK_Tab:
		return QKtab;
	case GDK_Linefeed:
		return QKlinefeed;
	case GDK_Return:
		return QKreturn;
	case GDK_Escape:
		return QKescape;
	case GDK_space:
		return QKspace;
	case GDK_Delete:
		return QKdelete;
	case 0:
		return Qnil;
	default:
		if (simple_p)
			return Qnil;
		/* !!#### not Mule-ized */
		name = gdk_keyval_name(keysym);
		if (!name || !name[0])
			/* This happens if there is a mismatch between the Xlib of
			   SXEmacs and the Xlib of the X server...

			   Let's hard-code in some knowledge of common keysyms introduced
			   in recent X11 releases.  Snarfed from X11/keysymdef.h

			   Probably we should add some stuff here for X11R6. */
			switch (keysym) {
			case 0xFF95:
				return KEYSYM("kp-home");
			case 0xFF96:
				return KEYSYM("kp-left");
			case 0xFF97:
				return KEYSYM("kp-up");
			case 0xFF98:
				return KEYSYM("kp-right");
			case 0xFF99:
				return KEYSYM("kp-down");
			case 0xFF9A:
				return KEYSYM("kp-prior");
			case 0xFF9B:
				return KEYSYM("kp-next");
			case 0xFF9C:
				return KEYSYM("kp-end");
			case 0xFF9D:
				return KEYSYM("kp-begin");
			case 0xFF9E:
				return KEYSYM("kp-insert");
			case 0xFF9F:
				return KEYSYM("kp-delete");

			case 0x1005FF10:
				return KEYSYM("SunF36");	/* labeled F11 */
			case 0x1005FF11:
				return KEYSYM("SunF37");	/* labeled F12 */
			default:
				{
					char buf[64];
					sprintf(buf, "unknown-keysym-0x%X",
						(int)keysym);
					return KEYSYM(buf);
				}
			}
		/* If it's got a one-character name, that's good enough. */
		if (!name[1])
			return make_char(name[0]);

		/* If it's in the "Keyboard" character set, downcase it.
		   The case of those keysyms is too totally random for us to
		   force anyone to remember them.
		   The case of the other character sets is significant, however.
		 */
		if ((((unsigned int)keysym) & (~0x1FF)) ==
		    ((unsigned int)0xFE00)) {
			char buf[255];
			char *s1, *s2;
			for (s1 = name, s2 = buf; *s1; s1++, s2++) {
				if (*s1 == '_') {
					*s2 = '-';
				} else {
					*s2 = tolower(*(unsigned char *)s1);
				}
			}
			*s2 = 0;
			return KEYSYM(buf);
		}
		return KEYSYM(name);
	}
}

static Lisp_Object
gtk_to_emacs_keysym(struct device *d, GdkEventKey * event, int simple_p)
     /* simple_p means don't try too hard (ASCII only) */
{
	if (event->length != 1) {
#ifdef FILE_CODING
		/* Generate multiple emacs events */
		Emchar ch;
		Lisp_Object instream, fb_instream;
		Lstream *istr;
		struct gcpro gcpro1, gcpro2;

		fb_instream =
		    make_fixed_buffer_input_stream((unsigned char *)event->
						   string, event->length);

		/* ### Use Fget_coding_system (Vcomposed_input_coding_system) */
		instream =
		    make_decoding_input_stream(XLSTREAM(fb_instream),
					       Fget_coding_system(Qundecided));

		istr = XLSTREAM(instream);

		GCPRO2(instream, fb_instream);
		while ((ch = Lstream_get_emchar(istr)) != EOF) {
			Lisp_Object emacs_event = Fmake_event(Qnil, Qnil);
			struct Lisp_Event *ev = XEVENT(emacs_event);
			ev->channel = DEVICE_CONSOLE(d);
			ev->event_type = key_press_event;
			ev->timestamp = event->time;
			ev->event.key.modifiers = 0;
			ev->event.key.keysym = make_char(ch);
			enqueue_gtk_dispatch_event(emacs_event);
		}
		Lstream_close(istr);
		UNGCPRO;
		Lstream_delete(istr);
		Lstream_delete(XLSTREAM(fb_instream));
#else
		int i;
		for (i = 0; i < event->length; i++) {
			Lisp_Object emacs_event = Fmake_event(Qnil, Qnil);
			struct Lisp_Event *ev = XEVENT(emacs_event);
			ev->channel = DEVICE_CONSOLE(d);
			ev->event_type = key_press_event;
			ev->timestamp = event->time;
			ev->event.key.modifiers = 0;
			ev->event.key.keysym = make_char(event->string[i]);
			enqueue_gtk_dispatch_event(emacs_event);
		}
#endif
		if (IS_MODIFIER_KEY(event->keyval)
		    || (event->keyval == GDK_Mode_switch))
			return (Qnil);
		return (gtk_keysym_to_emacs_keysym(event->keyval, simple_p));
	} else {
		if (IS_MODIFIER_KEY(event->keyval)
		    || (event->keyval == GDK_Mode_switch))
			return (Qnil);
		return (gtk_keysym_to_emacs_keysym(event->keyval, simple_p));
	}
}

/************************************************************************/
/*				timeout events				*/
/************************************************************************/

static int timeout_id_tick;

struct GTK_timeout {
	int id;
	guint timeout_id;
	struct GTK_timeout *next;
} *pending_timeouts, *completed_timeouts;

struct GTK_timeout_blocktype {
	Blocktype_declare(struct GTK_timeout);
} *the_GTK_timeout_blocktype;

/* called by the gtk main loop */
static gint gtk_timeout_callback(gpointer closure)
{
	struct GTK_timeout *timeout = (struct GTK_timeout *)closure;
	struct GTK_timeout *t2 = pending_timeouts;

	/* Remove this one from the list of pending timeouts */
	if (t2 == timeout)
		pending_timeouts = pending_timeouts->next;
	else {
		while (t2->next && t2->next != timeout)
			t2 = t2->next;
		assert(t2->next);
		t2->next = t2->next->next;
	}
	/* Add this one to the list of completed timeouts */
	timeout->next = completed_timeouts;
	completed_timeouts = timeout;
	return (FALSE);
}

static int emacs_gtk_add_timeout(EMACS_TIME thyme)
{
	struct GTK_timeout *timeout =
	    Blocktype_alloc(the_GTK_timeout_blocktype);
	EMACS_TIME current_time;
	int milliseconds;

	timeout->id = timeout_id_tick++;
	timeout->next = pending_timeouts;
	pending_timeouts = timeout;
	EMACS_GET_TIME(current_time);
	EMACS_SUB_TIME(thyme, thyme, current_time);
	milliseconds = EMACS_SECS(thyme) * 1000 + EMACS_USECS(thyme) / 1000;
	if (milliseconds < 1)
		milliseconds = 1;
	timeout->timeout_id = gtk_timeout_add(milliseconds,
					      gtk_timeout_callback,
					      (gpointer) timeout);
	return timeout->id;
}

static void emacs_gtk_remove_timeout(int id)
{
	struct GTK_timeout *timeout, *t2;

	timeout = NULL;

	/* Find the timeout on the list of pending ones, if it's still there. */
	if (pending_timeouts) {
		if (id == pending_timeouts->id) {
			timeout = pending_timeouts;
			pending_timeouts = pending_timeouts->next;
		} else {
			t2 = pending_timeouts;
			while (t2->next && t2->next->id != id)
				t2 = t2->next;
			if (t2->next) {	/*found it */
				timeout = t2->next;
				t2->next = t2->next->next;
			}
		}
		/* if it was pending, we have removed it from the list */
		if (timeout)
			gtk_timeout_remove(timeout->timeout_id);
	}

	/* It could be that the call back was already called but we didn't convert
	   into an Emacs event yet */
	if (!timeout && completed_timeouts) {
		/* Code duplication! */
		if (id == completed_timeouts->id) {
			timeout = completed_timeouts;
			completed_timeouts = completed_timeouts->next;
		} else {
			t2 = completed_timeouts;
			while (t2->next && t2->next->id != id)
				t2 = t2->next;
			if (t2->next) {	/*found it */
				timeout = t2->next;
				t2->next = t2->next->next;
			}
		}
	}

	/* If we found the thing on the lists of timeouts,
	   and removed it, deallocate
	 */
	if (timeout)
		Blocktype_free(the_GTK_timeout_blocktype, timeout);
}

static void gtk_timeout_to_emacs_event(struct Lisp_Event *emacs_event)
{
	struct GTK_timeout *timeout = completed_timeouts;
	assert(timeout);
	completed_timeouts = completed_timeouts->next;
	emacs_event->event_type = timeout_event;
	/* timeout events have nil as channel */
	emacs_event->timestamp = 0;	/* #### wrong!! */
	emacs_event->event.timeout.interval_id = timeout->id;
	Blocktype_free(the_GTK_timeout_blocktype, timeout);
}

/************************************************************************/
/*			process and tty events				*/
/************************************************************************/

struct what_is_ready_closure {
	int fd;
	Lisp_Object what;
	gint id;
};

static Lisp_Object *filedesc_with_input;
static struct what_is_ready_closure **filedesc_to_what_closure;

static void init_what_input_once(void)
{
	int i;

	filedesc_with_input = xnew_array(Lisp_Object, MAXDESC);
	filedesc_to_what_closure =
	    xnew_array(struct what_is_ready_closure *, MAXDESC);

	for (i = 0; i < MAXDESC; i++) {
		filedesc_to_what_closure[i] = 0;
		filedesc_with_input[i] = Qnil;
	}

	process_events_occurred = 0;
	tty_events_occurred = 0;
}

static void mark_what_as_being_ready(struct what_is_ready_closure *closure)
{
	if (NILP(filedesc_with_input[closure->fd])) {
		SELECT_TYPE temp_mask;
		FD_ZERO(&temp_mask);
		FD_SET(closure->fd, &temp_mask);
		/* Check to make sure there's *really* input available.
		   Sometimes things seem to get confused and this gets called
		   for the tty fd when there's really only input available
		   on some process's fd.  (It will subsequently get called
		   for that process's fd, so returning without setting any
		   flags will take care of it.)  To see the problem, uncomment
		   the stderr_out below, turn NORMAL_QUIT_CHECK_TIMEOUT_MSECS
		   down to 25, do sh -c 'sxemacs -nw -q -f shell 2>/tmp/log'
		   and press return repeatedly.  (Seen under AIX & Linux.)
		   -dkindred@cs.cmu.edu */
		if (!poll_fds_for_input(temp_mask)) {
#if 0
			stderr_out
			    ("mark_what_as_being_ready: no input available (fd=%d)\n",
			     closure->fd);
#endif
			return;
		}
		filedesc_with_input[closure->fd] = closure->what;
		if (PROCESSP(closure->what)) {
			/* Don't increment this if the current process is already marked
			 *  as having input. */
			process_events_occurred++;
		} else {
			tty_events_occurred++;
		}
	}
}

static void
gtk_what_callback(gpointer closure, gint source, GdkInputCondition why)
{
	/* If closure is 0, then we got a fake event from a signal handler.
	   The only purpose of this is to make XtAppProcessEvent() stop
	   blocking. */
	if (closure)
		mark_what_as_being_ready((struct what_is_ready_closure *)
					 closure);
	else {
		fake_event_occurred++;
		drain_signal_event_pipe();
	}
}

static void select_filedesc(int fd, Lisp_Object what)
{
	struct what_is_ready_closure *closure;

	/* If somebody is trying to select something that's already selected
	   for, then something went wrong.  The generic routines ought to
	   detect this and error before here. */
	assert(!filedesc_to_what_closure[fd]);

	closure = xnew(struct what_is_ready_closure);
	closure->fd = fd;
	closure->what = what;
	closure->id = gdk_input_add(fd, GDK_INPUT_READ,
				    (GdkInputFunction) gtk_what_callback,
				    closure);
	filedesc_to_what_closure[fd] = closure;
}

static void unselect_filedesc(int fd)
{
	struct what_is_ready_closure *closure = filedesc_to_what_closure[fd];

	assert(closure);
	if (!NILP(filedesc_with_input[fd])) {
		/* We are unselecting this process before we have drained the rest of
		   the input from it, probably from status_notify() in the command loop.
		   This can happen like so:

		   - We are waiting in XtAppNextEvent()
		   - Process generates output
		   - Process is marked as being ready
		   - Process dies, SIGCHLD gets generated before we return (!?)
		   It could happen I guess.
		   - sigchld_handler() marks process as dead
		   - Somehow we end up getting a new KeyPress event on the queue
		   at the same time (I'm really so sure how that happens but I'm
		   not sure it can't either so let's assume it can...).
		   - Key events have priority so we return that instead of the proc.
		   - Before dispatching the lisp key event we call status_notify()
		   - Which deselects the process that SIGCHLD marked as dead.

		   Thus we never remove it from _with_input and turn it into a lisp
		   event, so we need to do it here.  But this does not mean that we're
		   throwing away the last block of output - status_notify() has already
		   taken care of running the proc filter or whatever.
		 */
		filedesc_with_input[fd] = Qnil;
		if (PROCESSP(closure->what)) {
			assert(process_events_occurred > 0);
			process_events_occurred--;
		} else {
			assert(tty_events_occurred > 0);
			tty_events_occurred--;
		}
	}
	gdk_input_remove(closure->id);
	xfree(closure);
	filedesc_to_what_closure[fd] = 0;
}

static void emacs_gtk_select_process(struct Lisp_Process *p)
{
	Lisp_Object process;
	int infd = event_stream_unixoid_select_process(p);

	XSETPROCESS(process, p);
	select_filedesc(infd, process);
}

static void emacs_gtk_unselect_process(struct Lisp_Process *p)
{
	int infd = event_stream_unixoid_unselect_process(p);

	unselect_filedesc(infd);
}

static USID
emacs_gtk_create_stream_pair(void *inhandle, void *outhandle,
			     Lisp_Object * instream, Lisp_Object * outstream,
			     int flags)
{
	USID u = event_stream_unixoid_create_stream_pair
	    (inhandle, outhandle, instream, outstream, flags);
	if (u != USID_ERROR)
		u = USID_DONTHASH;
	return u;
}

static USID
emacs_gtk_delete_stream_pair(Lisp_Object instream, Lisp_Object outstream)
{
	event_stream_unixoid_delete_stream_pair(instream, outstream);
	return USID_DONTHASH;
}

/* This is called from GC when a process object is about to be freed.
   If we've still got pointers to it in this file, we're gonna lose hard.
 */
void debug_process_finalization(struct Lisp_Process *p)
{
#if 0				/* #### */
	int i;
	Lisp_Object instr, outstr;

	get_process_streams(p, &instr, &outstr);
	/* if it still has fds, then it hasn't been killed yet. */
	assert(NILP(instr));
	assert(NILP(outstr));
	/* Better not still be in the "with input" table; we know it's got no fds. */
	for (i = 0; i < MAXDESC; i++) {
		Lisp_Object process = filedesc_fds_with_input[i];
		assert(!PROCESSP(process) || XPROCESS(process) != p);
	}
#endif
}

static void gtk_process_to_emacs_event(struct Lisp_Event *emacs_event)
{
	int i;
	Lisp_Object process;

	assert(process_events_occurred > 0);
	for (i = 0; i < MAXDESC; i++) {
		process = filedesc_with_input[i];
		if (PROCESSP(process))
			break;
	}
	assert(i < MAXDESC);
	filedesc_with_input[i] = Qnil;
	process_events_occurred--;
	/* process events have nil as channel */
	emacs_event->event_type = process_event;
	emacs_event->timestamp = 0;	/* #### */
	emacs_event->event.process.process = process;
}

static void emacs_gtk_select_console(struct console *con)
{
	Lisp_Object console;
	int infd;

	if (CONSOLE_GTK_P(con))
		return;		/* Gtk consoles are automatically selected for when we initialize them */
	infd = event_stream_unixoid_select_console(con);
	XSETCONSOLE(console, con);
	select_filedesc(infd, console);
}

static void emacs_gtk_unselect_console(struct console *con)
{
	Lisp_Object console;
	int infd;

	if (CONSOLE_GTK_P(con))
		return;		/* X consoles are automatically selected for when we initialize them */
	infd = event_stream_unixoid_unselect_console(con);
	XSETCONSOLE(console, con);
	unselect_filedesc(infd);
}

/* read an event from a tty, if one is available.  Returns non-zero
   if an event was available.  Note that when this function is
   called, there should always be a tty marked as ready for input.
   However, the input condition might actually be EOF, so there
   may not really be any input available. (In this case,
   read_event_from_tty_or_stream_desc() will arrange for the TTY device
   to be deleted.) */

static int gtk_tty_to_emacs_event(struct Lisp_Event *emacs_event)
{
	int i;

	assert(tty_events_occurred > 0);
	for (i = 0; i < MAXDESC; i++) {
		Lisp_Object console = filedesc_with_input[i];
		if (CONSOLEP(console)) {
			assert(tty_events_occurred > 0);
			tty_events_occurred--;
			filedesc_with_input[i] = Qnil;
			if (read_event_from_tty_or_stream_desc
			    (emacs_event, XCONSOLE(console), i))
				return 1;
		}
	}

	return 0;
}

/************************************************************************/
/*			Drag 'n Drop handling				*/
/************************************************************************/
#ifdef HAVE_DRAGNDROP
#define TARGET_URI_LIST   0x00
#define TARGET_TEXT_PLAIN 0x01
#define TARGET_FILE_NAME  0x02
#define TARGET_NETSCAPE   0x03

static GdkAtom preferred_targets[10];

void
dragndrop_data_received(GtkWidget * widget,
			GdkDragContext * context,
			gint x,
			gint y, GtkSelectionData * data, guint info, guint time)
{
	Lisp_Object event = Fmake_event(Qnil, Qnil);
	struct device *d = gtk_any_window_to_device(widget->window);
	struct frame *f = gtk_any_widget_or_parent_to_frame(d, widget);
	struct Lisp_Event *ev = XEVENT(event);
	Lisp_Object l_type = Qnil, l_data = Qnil;
	Lisp_Object l_dndlist = Qnil, l_item = Qnil;
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

	GCPRO4(l_type, l_data, l_dndlist, l_item);

	ev->event_type = misc_user_event;
	ev->timestamp = time;

	XSETFRAME(ev->channel, f);

	ev->event.misc.x = x;
	ev->event.misc.y = y;

	if (data->type == preferred_targets[TARGET_URI_LIST]) {
		/* newline-separated list of URLs */
		int start, end;
		const char *string_data = (char *)data->data;

		l_type = Qdragdrop_URL;

		for (start = 0, end = 0; string_data && string_data[end]; end++) {
			if ((string_data[end] == '\r')
			    && (string_data[end + 1] == '\n')) {
				l_item =
				    make_string(&string_data[start],
						end - start);
				l_dndlist = Fcons(l_item, l_dndlist);
				++end;
				start = ++end;
			}
		}
	} else if (data->type == preferred_targets[TARGET_TEXT_PLAIN]) {
		/* Arbitrary string */
		l_type = Qdragdrop_MIME;
		l_dndlist = list1(list3(list1(build_string("text/plain")),
					build_string("8_bit"),
					make_ext_string(data->data,
							strlen((char *)data->
							       data), Qctext)));
	} else if (data->type == preferred_targets[TARGET_FILE_NAME]) {
		/* Random filename */
		char *hurl = dnd_url_hexify_string(data->data, "file:");

		l_dndlist = list1(make_string((Bufbyte *) hurl, strlen(hurl)));
		l_type = Qdragdrop_URL;

		xfree(hurl);
	} else if (data->type == preferred_targets[TARGET_NETSCAPE]) {
		/* Single URL */
		l_dndlist = list1(make_string((Extbyte *) data->data,
					      strlen((char *)data->data)));
		l_type = Qdragdrop_URL;
	} else {
		/* Unknown type - what to do?
		   We just pass it up to lisp - we already have a mime type.
		 */
		l_type = Qdragdrop_MIME;
		l_dndlist =
		    list1(list3
			  (list1(build_string(gdk_atom_name(data->type))),
			   build_string("8bit"),
			   make_ext_string((Extbyte *) data->data, data->length,
					   Qbinary)));
	}

	ev->event.misc.function = Qdragdrop_drop_dispatch;
	ev->event.misc.object = Fcons(l_type, l_dndlist);

	UNGCPRO;

	gtk_drag_finish(context, TRUE, FALSE, time);
	enqueue_gtk_dispatch_event(event);
}

gboolean
dragndrop_dropped(GtkWidget * widget,
		  GdkDragContext * drag_context,
		  gint x, gint y, guint time, gpointer user_data)
{
	/* Netscape drops things like:
	   STRING
	   _SGI_ICON
	   _SGI_ICON_TYPE
	   SGI_FILE
	   FILE_NAME
	   _NETSCAPE_URL

	   gmc drops things like
	   application/x-mc-desktop-icon
	   text/uri-list
	   text/plain
	   _NETSCAPE_URL

	   We prefer:
	   text/uri-list
	   text/plain
	   FILE_NAME
	   _NETSCAPE_URL
	   first one
	 */
	GdkAtom found = 0;
	GList *list = drag_context->targets;

	int i;

	if (!preferred_targets[0]) {
		preferred_targets[TARGET_URI_LIST] =
		    gdk_atom_intern("text/uri-list", FALSE);
		preferred_targets[TARGET_TEXT_PLAIN] =
		    gdk_atom_intern("text/plain", FALSE);
		preferred_targets[TARGET_FILE_NAME] =
		    gdk_atom_intern("FILE_NAME", FALSE);
		preferred_targets[TARGET_NETSCAPE] =
		    gdk_atom_intern("_NETSCAPE_URL", FALSE);
	}
#if 0
	stderr_out("Drop info available in the following formats: \n");
	while (list) {
		stderr_out("\t%s\n", gdk_atom_name((GdkAtom) list->data));
		list = list->next;
	}
	list = drag_context->targets;
#endif

	while (list && !found) {
		for (i = 0; preferred_targets[i] && !found; i++) {
			if ((GdkAtom) list->data == preferred_targets[i]) {
				found = (GdkAtom) list->data;
			}
		}
		list = list->next;
	}

	if (!found) {
		found = (GdkAtom) drag_context->targets->data;
	}

	gtk_drag_get_data(GTK_WIDGET(user_data), drag_context, found, time);
	return (TRUE);
}
#endif				/* HAVE_DRAGNDROP */

/************************************************************************/
/*			get the next event from gtk			*/
/************************************************************************/

static Lisp_Object dispatch_event_queue, dispatch_event_queue_tail;

void enqueue_gtk_dispatch_event(Lisp_Object event)
{
	enqueue_event(event, &dispatch_event_queue, &dispatch_event_queue_tail);
}

static Lisp_Object dequeue_gtk_dispatch_event(void)
{
	return dequeue_event(&dispatch_event_queue, &dispatch_event_queue_tail);
}

/* This business exists because menu events "happen" when
   menubar_selection_callback() is called from somewhere deep
   within XtAppProcessEvent in emacs_Xt_next_event().  The
   callback needs to terminate the modal loop in that function
   or else it will continue waiting until another event is
   received.

   Same business applies to scrollbar events. */

void
signal_special_gtk_user_event(Lisp_Object channel, Lisp_Object function,
			      Lisp_Object object)
{
	Lisp_Object event = Fmake_event(Qnil, Qnil);

	XEVENT(event)->event_type = misc_user_event;
	XEVENT(event)->channel = channel;
	XEVENT(event)->event.eval.function = function;
	XEVENT(event)->event.eval.object = object;

	enqueue_gtk_dispatch_event(event);
}

static void emacs_gtk_next_event(struct Lisp_Event *emacs_event)
{
      we_didnt_get_an_event:

	while (NILP(dispatch_event_queue) &&
	       !completed_timeouts &&
	       !fake_event_occurred &&
	       !process_events_occurred && !tty_events_occurred) {
		gtk_main_iteration();
	}

	if (!NILP(dispatch_event_queue)) {
		Lisp_Object event, event2;
		XSETEVENT(event2, emacs_event);
		event = dequeue_gtk_dispatch_event();
		Fcopy_event(event, event2);
		Fdeallocate_event(event);
	} else if (tty_events_occurred) {
		if (!gtk_tty_to_emacs_event(emacs_event))
			goto we_didnt_get_an_event;
	} else if (completed_timeouts)
		gtk_timeout_to_emacs_event(emacs_event);
	else if (fake_event_occurred) {
		/* A dummy event, so that a cycle of the command loop will occur. */
		fake_event_occurred = 0;
		/* eval events have nil as channel */
		emacs_event->event_type = eval_event;
		emacs_event->event.eval.function = Qidentity;
		emacs_event->event.eval.object = Qnil;
	} else			/* if (process_events_occurred) */
		gtk_process_to_emacs_event(emacs_event);
}

int
gtk_event_to_emacs_event(struct frame *frame, GdkEvent * gdk_event,
			 struct Lisp_Event *emacs_event)
{
	struct device *d = NULL;
	struct gtk_device *gd = NULL;
	gboolean accept_any_window = FALSE;

	if (!frame) {
		frame = XFRAME(Fselected_frame(Vdefault_gtk_device));
		accept_any_window = TRUE;
	}

	d = XDEVICE(FRAME_DEVICE(frame));
	gd = DEVICE_GTK_DATA(d);

	set_last_server_timestamp(d, gdk_event);

	switch (gdk_event->type) {
		/* SXEmacs handles double and triple clicking on its own, and if
		   we capture these events, it royally confuses the code in
		   ../lisp/mouse.el */
	case GDK_2BUTTON_PRESS:
	case GDK_3BUTTON_PRESS:
		return (0);

	case GDK_BUTTON_PRESS:
	case GDK_BUTTON_RELEASE:
		/* We need to ignore button events outside our main window or
		   things get ugly.  The standard scrollbars in Gtk try to be
		   nice and pass the button press events up to the parent
		   widget.  This causes us no end of grief though.  Effects
		   range from setting point to the wrong place to selecting
		   new windows. */
		{
			GdkWindow *w = gdk_window_at_pointer(NULL, NULL);

			/* If you press mouse button and drag it around, and release
			   it outside the window, you will get a NULL GdkWindow at
			   pointer.  We need to forward these events on to SXEmacs so
			   that the mouse selection voodoo works.
			 */
			if (w && (w != gdk_window_lookup(GDK_ROOT_WINDOW()))) {
				GdkEvent ev;
				GtkWidget *wid = NULL;

				ev.any.window = w;
				wid = gtk_get_event_widget(&ev);

				if (!GTK_IS_XEMACS(wid) && !accept_any_window) {
					return (0);
				}
			}
			if (!accept_any_window)
				gtk_widget_grab_focus(FRAME_GTK_TEXT_WIDGET
						      (frame));
		}
		/* Fall through */
	case GDK_KEY_PRESS:
		{
			unsigned int modifiers = 0;
			int shift_p, lock_p;
			gboolean key_event_p =
			    (gdk_event->type == GDK_KEY_PRESS);
			unsigned int *state =
			    key_event_p ? &gdk_event->key.state : &gdk_event->
			    button.state;

			/* If this is a synthetic KeyPress or Button event, and the user
			   has expressed a disinterest in this security hole, then drop
			   it on the floor. */
			/* #### BILL!!! Should this be a generic check for ANY synthetic
			   event? */
			if ((gdk_event->any.send_event)
			    && !gtk_allow_sendevents)
				return 0;

			DEVICE_GTK_MOUSE_TIMESTAMP(d) =
			    DEVICE_GTK_GLOBAL_MOUSE_TIMESTAMP(d) =
			    key_event_p ? gdk_event->key.time : gdk_event->
			    button.time;

			if (*state & GDK_CONTROL_MASK)
				modifiers |= XEMACS_MOD_CONTROL;
			if (*state & gd->MetaMask)
				modifiers |= XEMACS_MOD_META;
			if (*state & gd->SuperMask)
				modifiers |= XEMACS_MOD_SUPER;
			if (*state & gd->HyperMask)
				modifiers |= XEMACS_MOD_HYPER;
			if (*state & gd->AltMask)
				modifiers |= XEMACS_MOD_ALT;

			{
				int numero_de_botao = -1;

				if (!key_event_p)
					numero_de_botao =
					    gdk_event->button.button;

				/* the button gets noted either in the button or the modifiers
				   field, but not both. */
				if (numero_de_botao != 1
				    && (*state & GDK_BUTTON1_MASK))
					modifiers |= XEMACS_MOD_BUTTON1;
				if (numero_de_botao != 2
				    && (*state & GDK_BUTTON2_MASK))
					modifiers |= XEMACS_MOD_BUTTON2;
				if (numero_de_botao != 3
				    && (*state & GDK_BUTTON3_MASK))
					modifiers |= XEMACS_MOD_BUTTON3;
				if (numero_de_botao != 4
				    && (*state & GDK_BUTTON4_MASK))
					modifiers |= XEMACS_MOD_BUTTON4;
				if (numero_de_botao != 5
				    && (*state & GDK_BUTTON5_MASK))
					modifiers |= XEMACS_MOD_BUTTON5;
			}

			/* Ignore the Caps_Lock key if:
			   - any other modifiers are down, so that Caps_Lock doesn't
			   turn C-x into C-X, which would suck.
			   - the event was a mouse event. */
			if (modifiers || !key_event_p)
				*state &= (~GDK_LOCK_MASK);

			shift_p = *state & GDK_SHIFT_MASK;
			lock_p = *state & GDK_LOCK_MASK;

			if (shift_p || lock_p)
				modifiers |= XEMACS_MOD_SHIFT;

			if (key_event_p) {
				GdkEventKey *key_event = &gdk_event->key;
				Lisp_Object keysym;

#ifdef HAVE_MENUBARS
				/* If the user wants see if the event is a menu bar accelerator.
				   The process of checking absorbs the event and starts menu
				   processing so send a null event into XEmacs to make sure it
				   does nothing.
				 */
				if (!NILP(Vmenu_accelerator_enabled)
				    &&
				    gtk_accel_groups_activate(GTK_OBJECT
							      (FRAME_GTK_SHELL_WIDGET
							       (frame)),
							      key_event->keyval,
							      *state)) {
					zero_event(emacs_event);
					return 1;
				}
#endif

				/* This used to compute the frame from the given X window and
				   store it here, but we really don't care about the frame. */
				emacs_event->channel = DEVICE_CONSOLE(d);

				/* Keysym mucking has already been done inside the
				   GdkEventKey parsing */
				keysym = gtk_to_emacs_keysym(d, key_event, 0);

				/* If the emacs keysym is nil, then that means that the X
				   keysym was either a Modifier or NoSymbol, which
				   probably means that we're in the midst of reading a
				   Multi_key sequence, or a "dead" key prefix, or XIM
				   input. Ignore it. */
				if (NILP(keysym))
					return 0;

				/* More Caps_Lock garbage: Caps_Lock should *only* add the
				   shift modifier to two-case keys (that is, A-Z and
				   related characters). So at this point (after looking up
				   the keysym) if the keysym isn't a dual-case alphabetic,
				   and if the caps lock key was down but the shift key
				   wasn't, then turn off the shift modifier.  Gag barf */
				/* #### type lossage: assuming equivalence of emacs and
				   X keysyms */
				/* !!#### maybe fix for Mule */
				if (lock_p && !shift_p &&
				    !(CHAR_OR_CHAR_INTP(keysym)
				      && keysym_obeys_caps_lock_p
				      ((guint) XCHAR_OR_CHAR_INT(keysym), d)))
					modifiers &= (~XEMACS_MOD_SHIFT);

				/* If this key contains two distinct keysyms, that is,
				   "shift" generates a different keysym than the
				   non-shifted key, then don't apply the shift modifier
				   bit: it's implicit.  Otherwise, if there would be no
				   other way to tell the difference between the shifted
				   and unshifted version of this key, apply the shift bit.
				   Non-graphics, like Backspace and F1 get the shift bit
				   in the modifiers slot.  Neither the characters "a",
				   "A", "2", nor "@" normally have the shift bit set.
				   However, "F1" normally does. */
				if (modifiers & XEMACS_MOD_SHIFT) {
					if (CHAR_OR_CHAR_INTP(keysym)) {
						modifiers &= ~XEMACS_MOD_SHIFT;
					}
				}

				emacs_event->event_type = key_press_event;
				emacs_event->timestamp = key_event->time;
				emacs_event->event.key.modifiers = modifiers;
				emacs_event->event.key.keysym = keysym;
			} else {	/* Mouse press/release event */

				GdkEventButton *button_event =
				    &gdk_event->button;
				XSETFRAME(emacs_event->channel, frame);

				emacs_event->event_type =
				    (button_event->type ==
				     GDK_BUTTON_RELEASE) ? button_release_event
				    : button_press_event;

				emacs_event->event.button.modifiers = modifiers;
				emacs_event->timestamp = button_event->time;
				emacs_event->event.button.button =
				    button_event->button;
				emacs_event->event.button.x = button_event->x;
				emacs_event->event.button.y = button_event->y;
			}
		}
		break;
	case GDK_KEY_RELEASE:
		return 0;
		break;
	case GDK_MOTION_NOTIFY:
		{
			GdkEventMotion *ev = &gdk_event->motion;
			unsigned int modifiers = 0;
			gint x, y;
			GdkModifierType mask;

			/* We use MOTION_HINT_MASK, so we will get only one motion
			   event until the next time we call gdk_window_get_pointer or
			   the user clicks the mouse.  So call gdk_window_get_pointer
			   now (meaning that the event will be in sync with the server
			   just before Fnext_event() returns).  If the mouse is still
			   in motion, then the server will immediately generate
			   exactly one more motion event, which will be on the queue
			   waiting for us next time around. */
			gdk_window_get_pointer(ev->window, &x, &y, &mask);

			DEVICE_GTK_MOUSE_TIMESTAMP(d) = ev->time;

			XSETFRAME(emacs_event->channel, frame);
			emacs_event->event_type = pointer_motion_event;
			emacs_event->timestamp = ev->time;
			emacs_event->event.motion.x = x;
			emacs_event->event.motion.y = y;
			if (mask & GDK_SHIFT_MASK)
				modifiers |= XEMACS_MOD_SHIFT;
			if (mask & GDK_CONTROL_MASK)
				modifiers |= XEMACS_MOD_CONTROL;
			if (mask & gd->MetaMask)
				modifiers |= XEMACS_MOD_META;
			if (mask & gd->SuperMask)
				modifiers |= XEMACS_MOD_SUPER;
			if (mask & gd->HyperMask)
				modifiers |= XEMACS_MOD_HYPER;
			if (mask & gd->AltMask)
				modifiers |= XEMACS_MOD_ALT;
			if (mask & GDK_BUTTON1_MASK)
				modifiers |= XEMACS_MOD_BUTTON1;
			if (mask & GDK_BUTTON2_MASK)
				modifiers |= XEMACS_MOD_BUTTON2;
			if (mask & GDK_BUTTON3_MASK)
				modifiers |= XEMACS_MOD_BUTTON3;
			if (mask & GDK_BUTTON4_MASK)
				modifiers |= XEMACS_MOD_BUTTON4;
			if (mask & GDK_BUTTON5_MASK)
				modifiers |= XEMACS_MOD_BUTTON5;

			/* Currently ignores Shift_Lock but probably shouldn't
			   (but it definitely should ignore Caps_Lock). */
			emacs_event->event.motion.modifiers = modifiers;
		}
		break;

	default:		/* it's a magic event */
		return (0);
		break;
	}
	return 1;
}

static const char *event_name(GdkEvent *);

static gboolean generic_event_handler(GtkWidget * widget, GdkEvent * event)
{
	Lisp_Object emacs_event = Qnil;
	if (!GTK_IS_XEMACS(widget)) {
		stderr_out("Got a %s event for a non-SXEmacs widget\n",
			   event_name(event));
		return (FALSE);
	}

	emacs_event = Fmake_event(Qnil, Qnil);

	if (gtk_event_to_emacs_event
	    (GTK_XEMACS_FRAME(widget), event, XEVENT(emacs_event))) {
		enqueue_gtk_dispatch_event(emacs_event);
		return (TRUE);
	} else {
		Fdeallocate_event(emacs_event);
	}
	return (FALSE);
}

gint emacs_gtk_key_event_handler(GtkWidget * widget, GdkEventKey * event)
{
	return (generic_event_handler(widget, (GdkEvent *) event));
}

gint emacs_gtk_button_event_handler(GtkWidget * widget, GdkEventButton * event)
{
	return (generic_event_handler(widget, (GdkEvent *) event));
}

gint emacs_gtk_motion_event_handler(GtkWidget * widget, GdkEventMotion * event)
{
	return (generic_event_handler(widget, (GdkEvent *) event));
}

gboolean emacs_shell_event_handler(GtkWidget * wid /* unused */ ,
				   GdkEvent * event, gpointer closure)
{
	struct frame *frame = (struct frame *)closure;
	Lisp_Object lisp_event = Fmake_event(Qnil, Qnil);
	struct Lisp_Event *emacs_event = XEVENT(lisp_event);
	GdkEvent *gdk_event_copy =
	    &emacs_event->event.magic.underlying_gdk_event;
	struct device *d = XDEVICE(FRAME_DEVICE(frame));
	gboolean ignore_p = FALSE;

	set_last_server_timestamp(d, event);

#define FROB(event_member) gdk_event_copy->event_member = event->event_member

	switch (event->type) {
	case GDK_SELECTION_REQUEST:
	case GDK_SELECTION_CLEAR:
	case GDK_SELECTION_NOTIFY:
		FROB(selection);
		break;
	case GDK_PROPERTY_NOTIFY:
		FROB(property);
		break;
	case GDK_CLIENT_EVENT:
		FROB(client);
		break;
	case GDK_MAP:
	case GDK_UNMAP:
		FROB(any);
		break;
	case GDK_CONFIGURE:
		FROB(configure);
		break;
	case GDK_ENTER_NOTIFY:
	case GDK_LEAVE_NOTIFY:
		FROB(crossing);
		break;
	case GDK_FOCUS_CHANGE:
		FROB(focus_change);
		break;
	case GDK_VISIBILITY_NOTIFY:
		FROB(visibility);
		break;
	default:
		ignore_p = TRUE;
		/* Hrmm... do we really want to swallow all the other events as magic? */
		*gdk_event_copy = *event;
		break;
	}
#undef FROB

	emacs_event->event_type = magic_event;
	XSETFRAME(emacs_event->channel, frame);

	if (ignore_p) {
		stderr_out("Ignoring event... (%s)\n", event_name(event));
		Fdeallocate_event(lisp_event);
		return (FALSE);
	} else {
		enqueue_gtk_dispatch_event(lisp_event);
		return (TRUE);
	}
}

/************************************************************************/
/*                      input pending / C-g checking                    */
/************************************************************************/
static void gtk_check_for_quit_char(struct device *d);

static void check_for_tty_quit_char(struct device *d)
{
	SELECT_TYPE temp_mask;
	int infd = DEVICE_INFD(d);
	struct console *con = XCONSOLE(DEVICE_CONSOLE(d));
	Emchar quit_char = CONSOLE_QUIT_CHAR(con);

	FD_ZERO(&temp_mask);
	FD_SET(infd, &temp_mask);

	while (1) {
		Lisp_Object event;
		Emchar the_char;

		if (!poll_fds_for_input(temp_mask))
			return;

		event = Fmake_event(Qnil, Qnil);
		if (!read_event_from_tty_or_stream_desc
		    (XEVENT(event), con, infd))
			/* EOF, or something ... */
			return;
		/* #### bogus.  quit-char should be allowed to be any sort
		   of event. */
		the_char = event_to_character(XEVENT(event), 1, 0, 0);
		if (the_char >= 0 && the_char == quit_char) {
			Vquit_flag = Qt;
			/* do not queue the C-g.  See above. */
			return;
		}

		/* queue the read event to be read for real later. */
		enqueue_gtk_dispatch_event(event);
	}
}

static void emacs_gtk_quit_p(void)
{
	Lisp_Object devcons, concons;

	CONSOLE_LOOP(concons) {
		struct console *con = XCONSOLE(XCAR(concons));
		if (!con->input_enabled)
			continue;

		CONSOLE_DEVICE_LOOP(devcons, con) {
			struct device *d;
			d = XDEVICE(XCAR(devcons));

			if (DEVICE_GTK_P(d))
				/* emacs may be exiting */
				gtk_check_for_quit_char(d);
			else if (DEVICE_TTY_P(d))
				check_for_tty_quit_char(d);
		}
	}
}

#include <gdk/gdkx.h>

static void drain_gtk_queue(void)
{
	/* We can't just spin through here and wait for GTKs idea of the
	   event queue to get empty, or the queue never gets drained.  The
	   situation is as follows.  A process event gets signalled, we put
	   it on the queue, then we go into Fnext_event(), which calls
	   drain_gtk_queue().  But gtk_events_pending() will always return
	   TRUE if there are file-descriptor (aka our process) events
	   pending.  Using GDK_events_pending() only shows us windowing
	   system events.
	 */
	if (GDK_DISPLAY())
		while (gdk_events_pending())
			gtk_main_iteration();
}

static int emacs_gtk_event_pending_p(int user_p)
{
	Lisp_Object event;
	int tick_count_val;

	/* If `user_p' is false, then this function returns whether there are any
	   X, timeout, or fd events pending (that is, whether emacs_gtk_next_event()
	   would return immediately without blocking).

	   if `user_p' is true, then this function returns whether there are any
	   *user generated* events available (that is, whether there are keyboard
	   or mouse-click events ready to be read).  This also implies that
	   emacs_Xt_next_event() would not block.

	   In a non-SIGIO world, this also checks whether the user has typed ^G,
	   since this is a convenient place to do so.  We don't need to do this
	   in a SIGIO world, since input causes an interrupt.
	 */

	/* This function used to simply check whether there were any X
	   events (or if user_p was 1, it iterated over all the pending
	   X events using XCheckIfEvent(), looking for keystrokes and
	   button events).  That worked in the old cheesoid event loop,
	   which didn't go through XtAppDispatchEvent(), but it doesn't
	   work any more -- X events may not result in anything.  For
	   example, a button press in a blank part of the menubar appears
	   as an X event but will not result in any Emacs events (a
	   button press that activates the menubar results in an Emacs
	   event through the stop_next_event mechanism).

	   The only accurate way of determining whether these X events
	   translate into Emacs events is to go ahead and dispatch them
	   until there's something on the dispatch queue. */

	/* See if there are any user events already on the queue. */
	EVENT_CHAIN_LOOP(event, dispatch_event_queue)
	    if (!user_p || command_event_p(event))
		return 1;

	/* See if there's any TTY input available.
	 */
	if (poll_fds_for_input(tty_only_mask))
		return 1;

	if (!user_p) {
		/* If not user_p and there are any timer or file-desc events
		   pending, we know there will be an event so we're through. */
/*      XtInputMask pending_value; */

		/* Note that formerly we just checked the value of XtAppPending()
		   to determine if there was file-desc input.  This doesn't
		   work any more with the signal_event_pipe; XtAppPending()
		   will says "yes" in this case but there isn't really any
		   input.  Another way of fixing this problem is for the
		   signal_event_pipe to generate actual input in the form
		   of an identity eval event or something. (#### maybe this
		   actually happens?) */

		if (poll_fds_for_input(process_only_mask))
			return 1;

		/* #### Is there any way to do this in Gtk?  I don't think there
		   is a 'peek' for events */
#if 0
		pending_value = XtAppPending(Xt_app_con);

		if (pending_value & XtIMTimer)
			return 1;
#endif
	}

	/* XtAppPending() can be super-slow, esp. over a network connection.
	   Quantify results have indicated that in some cases the
	   call to detect_input_pending() completely dominates the
	   running time of redisplay().  Fortunately, in a SIGIO world
	   we can more quickly determine whether there are any X events:
	   if an event has happened since the last time we checked, then
	   a SIGIO will have happened.  On a machine with broken SIGIO,
	   we'll still be in an OK state -- the sigio_happened flag
	   will get set at least once a second, so we'll be no more than
	   one second behind reality. (In general it's OK if we
	   erroneously report no input pending when input is actually
	   pending() -- preemption is just a bit less efficient, that's
	   all.  It's bad bad bad if you err the other way -- you've
	   promised that `next-event' won't block but it actually will,
	   and some action might get delayed until the next time you
	   hit a key.)
	 */

	/* quit_check_signal_tick_count is volatile so try to avoid race conditions
	   by using a temporary variable */
	tick_count_val = quit_check_signal_tick_count;
	if (last_quit_check_signal_tick_count != tick_count_val) {
		last_quit_check_signal_tick_count = tick_count_val;

		/* We need to drain the entire queue now -- if we only
		   drain part of it, we may later on end up with events
		   actually pending but detect_input_pending() returning
		   false because there wasn't another SIGIO. */

		drain_gtk_queue();

		EVENT_CHAIN_LOOP(event, dispatch_event_queue)
		    if (!user_p || command_event_p(event))
			return 1;
	}

	return 0;
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void syms_of_event_gtk(void)
{
	defsymbol(&Qkey_mapping, "key-mapping");
	defsymbol(&Qsans_modifiers, "sans-modifiers");
}

void reinit_vars_of_event_gtk(void)
{
	gtk_event_stream = xnew(struct event_stream);
	gtk_event_stream->event_pending_p = emacs_gtk_event_pending_p;
	gtk_event_stream->next_event_cb = emacs_gtk_next_event;
	gtk_event_stream->handle_magic_event_cb = emacs_gtk_handle_magic_event;
	gtk_event_stream->add_timeout_cb = emacs_gtk_add_timeout;
	gtk_event_stream->remove_timeout_cb = emacs_gtk_remove_timeout;
	gtk_event_stream->select_console_cb = emacs_gtk_select_console;
	gtk_event_stream->unselect_console_cb = emacs_gtk_unselect_console;
	gtk_event_stream->select_process_cb = emacs_gtk_select_process;
	gtk_event_stream->unselect_process_cb = emacs_gtk_unselect_process;
	gtk_event_stream->quit_p_cb = emacs_gtk_quit_p;
	gtk_event_stream->create_stream_pair_cb = emacs_gtk_create_stream_pair;
	gtk_event_stream->delete_stream_pair_cb = emacs_gtk_delete_stream_pair;

	the_GTK_timeout_blocktype = Blocktype_new(struct GTK_timeout_blocktype);

	/* this function only makes safe calls */
	init_what_input_once();
}

void vars_of_event_gtk(void)
{
	reinit_vars_of_event_gtk();

	dispatch_event_queue = Qnil;
	staticpro(&dispatch_event_queue);
	dispatch_event_queue_tail = Qnil;
	staticpro(&dispatch_event_queue_tail);

	DEFVAR_BOOL("gtk-allow-sendevents", &gtk_allow_sendevents	/*
*Non-nil means to allow synthetic events.  Nil means they are ignored.
Beware: allowing emacs to process SendEvents opens a big security hole.
									 */ );
	gtk_allow_sendevents = 0;

	last_quit_check_signal_tick_count = 0;
}

void init_event_gtk_late(void)
{				/* called when already initialized */
	timeout_id_tick = 1;
	pending_timeouts = 0;
	completed_timeouts = 0;

	event_stream = gtk_event_stream;

#if 0
	/* Shut GDK the hell up */
	gdk_error_trap_push();
#endif

	gdk_input_add(signal_event_pipe[0], GDK_INPUT_READ,
		      (GdkInputFunction) gtk_what_callback, NULL);
}

/* Bogus utility routines */
static const char *event_name(GdkEvent * ev)
{
	return (gtk_event_name(ev->any.type));
}

/* This is down at the bottom of the file so I can avoid polluting the
   generic code with this X specific CRAP! */

#include <gdk/gdkx.h>
#include <X11/keysym.h>
/* #### BILL!!! Fix this please! */

/************************************************************************/
/*                            keymap handling                           */
/************************************************************************/

/* X bogusly doesn't define the interpretations of any bits besides
   ModControl, ModShift, and ModLock; so the Interclient Communication
   Conventions Manual says that we have to bend over backwards to figure
   out what the other modifier bits mean.  According to ICCCM:

   - Any keycode which is assigned ModControl is a "control" key.

   - Any modifier bit which is assigned to a keycode which generates Meta_L
     or Meta_R is the modifier bit meaning "meta".  Likewise for Super, Hyper,
     etc.

   - Any keypress event which contains ModControl in its state should be
     interpreted as a "control" character.

   - Any keypress event which contains a modifier bit in its state which is
     generated by a keycode whose corresponding keysym is Meta_L or Meta_R
     should be interpreted as a "meta" character.  Likewise for Super, Hyper,
     etc.

   - It is illegal for a keysym to be associated with more than one modifier
     bit.

   This means that the only thing that emacs can reasonably interpret as a
   "meta" key is a key whose keysym is Meta_L or Meta_R, and which generates
   one of the modifier bits Mod1-Mod5.

   Unfortunately, many keyboards don't have Meta keys in their default
   configuration.  So, if there are no Meta keys, but there are "Alt" keys,
   emacs will interpret Alt as Meta.  If there are both Meta and Alt keys,
   then the Meta keys mean "Meta", and the Alt keys mean "Alt" (it used to
   mean "Symbol," but that just confused the hell out of way too many people).

   This works with the default configurations of the 19 keyboard-types I've
   checked.

   Emacs detects keyboard configurations which violate the above rules, and
   prints an error message on the standard-error-output.  (Perhaps it should
   use a pop-up-window instead.)
 */

static void gtk_reset_key_mapping(struct device *d)
{
	Display *display = GDK_DISPLAY();
	struct gtk_device *xd = DEVICE_GTK_DATA(d);
	XModifierKeymap *map = (XModifierKeymap *) xd->x_keysym_map;
	KeySym *keysym, *keysym_end;
	Lisp_Object hashtable;
	int key_code_count, keysyms_per_code;

	if (map)
		XFree((char *)map);
	XDisplayKeycodes(display,
			 &xd->x_keysym_map_min_code,
			 &xd->x_keysym_map_max_code);
	key_code_count =
	    xd->x_keysym_map_max_code - xd->x_keysym_map_min_code + 1;
	map = (XModifierKeymap *)
	    XGetKeyboardMapping(display, xd->x_keysym_map_min_code,
				key_code_count,
				&xd->x_keysym_map_keysyms_per_code);

	xd->x_keysym_map = (void *)map;
	hashtable = xd->x_keysym_map_hashtable;
	if (HASH_TABLEP(hashtable)) {
		Fclrhash(hashtable);
	} else {
		xd->x_keysym_map_hashtable = hashtable =
		    make_lisp_hash_table(128, HASH_TABLE_NON_WEAK,
					 HASH_TABLE_EQUAL);
	}

	for (keysym = (KeySym *) map,
	     keysyms_per_code = xd->x_keysym_map_keysyms_per_code,
	     keysym_end = keysym + (key_code_count * keysyms_per_code);
	     keysym < keysym_end; keysym += keysyms_per_code) {
		int j;

		if (keysym[0] == NoSymbol)
			continue;

		{
			char *name = XKeysymToString(keysym[0]);
			Lisp_Object sym =
			    gtk_keysym_to_emacs_keysym(keysym[0], 0);
			if (name) {
				Fputhash(build_string(name), Qsans_modifiers,
					 hashtable);
				Fputhash(sym, Qsans_modifiers, hashtable);
			}
		}

		for (j = 1; j < keysyms_per_code; j++) {
			if (keysym[j] != keysym[0] && keysym[j] != NoSymbol) {
				char *name = XKeysymToString(keysym[j]);
				Lisp_Object sym =
				    gtk_keysym_to_emacs_keysym(keysym[j], 0);
				if (name
				    && NILP(Fgethash(sym, hashtable, Qnil))) {
					Fputhash(build_string(name), Qt,
						 hashtable);
					Fputhash(sym, Qt, hashtable);
				}
			}
		}
	}
}

static const char *index_to_name(int indice)
{
	switch (indice) {
	case ShiftMapIndex:
		return "ModShift";
	case LockMapIndex:
		return "ModLock";
	case ControlMapIndex:
		return "ModControl";
	case Mod1MapIndex:
		return "Mod1";
	case Mod2MapIndex:
		return "Mod2";
	case Mod3MapIndex:
		return "Mod3";
	case Mod4MapIndex:
		return "Mod4";
	case Mod5MapIndex:
		return "Mod5";
	default:
		return "???";
	}
}

/* Boy, I really wish C had local functions... */
struct c_doesnt_have_closures {	/* #### not yet used */
	int warned_about_overlapping_modifiers;
	int warned_about_predefined_modifiers;
	int warned_about_duplicate_modifiers;
	int meta_bit;
	int hyper_bit;
	int super_bit;
	int alt_bit;
	int mode_bit;
};

static void gtk_reset_modifier_mapping(struct device *d)
{
	Display *display = GDK_DISPLAY();
	struct gtk_device *xd = DEVICE_GTK_DATA(d);
	int modifier_index, modifier_key, column, mkpm;
	int warned_about_overlapping_modifiers = 0;
	/*  int warned_about_predefined_modifiers  = 0; */
	/* int warned_about_duplicate_modifiers   = 0; */
	int meta_bit = 0;
	int hyper_bit = 0;
	int super_bit = 0;
	int alt_bit = 0;
	int mode_bit = 0;
	XModifierKeymap *map = (XModifierKeymap *) xd->x_modifier_keymap;

	xd->lock_interpretation = 0;

	if (map)
		XFreeModifiermap(map);

	gtk_reset_key_mapping(d);

	xd->x_modifier_keymap = map = XGetModifierMapping(display);

	/* Boy, I really wish C had local functions...
	 */

	/* The call to warn_when_safe must be on the same line as the string or
	   make-msgfile won't pick it up properly (the newline doesn't confuse
	   it, but the backslash does). */

#define store_modifier(name,old)					   \
    old = modifier_index;

	mkpm = map->max_keypermod;
	for (modifier_index = 0; modifier_index < 8; modifier_index++)
		for (modifier_key = 0; modifier_key < mkpm; modifier_key++) {
			KeySym last_sym = 0;
			for (column = 0; column < 4; column += 2) {
				KeyCode code =
				    map->modifiermap[modifier_index * mkpm +
						     modifier_key];
				KeySym sym =
				    (code ?
				     XKeycodeToKeysym(display, code,
						      column) : 0);
				if (sym == last_sym)
					continue;
				last_sym = sym;
				switch (sym) {
				case XK_Mode_switch:
					store_modifier("Mode_switch", mode_bit);
					break;
				case XK_Meta_L:
					store_modifier("Meta_L", meta_bit);
					break;
				case XK_Meta_R:
					store_modifier("Meta_R", meta_bit);
					break;
				case XK_Super_L:
					store_modifier("Super_L", super_bit);
					break;
				case XK_Super_R:
					store_modifier("Super_R", super_bit);
					break;
				case XK_Hyper_L:
					store_modifier("Hyper_L", hyper_bit);
					break;
				case XK_Hyper_R:
					store_modifier("Hyper_R", hyper_bit);
					break;
				case XK_Alt_L:
					store_modifier("Alt_L", alt_bit);
					break;
				case XK_Alt_R:
					store_modifier("Alt_R", alt_bit);
					break;
#if 0
				case XK_Control_L:
					check_modifier("Control_L",
						       ControlMask);
					break;
				case XK_Control_R:
					check_modifier("Control_R",
						       ControlMask);
					break;
				case XK_Shift_L:
					check_modifier("Shift_L", ShiftMask);
					break;
				case XK_Shift_R:
					check_modifier("Shift_R", ShiftMask);
					break;
#endif
				case XK_Shift_Lock:	/* check_modifier ("Shift_Lock", LockMask); */
					xd->lock_interpretation = XK_Shift_Lock;
					break;
				case XK_Caps_Lock:	/* check_modifier ("Caps_Lock", LockMask); */
					xd->lock_interpretation = XK_Caps_Lock;
					break;

					/* It probably doesn't make any sense for a modifier bit to be
					   assigned to a key that is not one of the above, but OpenWindows
					   assigns modifier bits to a couple of random function keys for
					   no reason that I can discern, so printing a warning here would
					   be annoying. */
				}
			}
		}
#undef store_modifier
#undef check_modifier
#undef modwarn
#undef modbarf

	/* If there was no Meta key, then try using the Alt key instead.
	   If there is both a Meta key and an Alt key, then the Alt key
	   is not disturbed and remains an Alt key. */
	if (!meta_bit && alt_bit)
		meta_bit = alt_bit, alt_bit = 0;

	/* mode_bit overrides everything, since it's processed down inside of
	   XLookupString() instead of by us.  If Meta and Mode_switch both
	   generate the same modifier bit (which is an error), then we don't
	   interpret that bit as Meta, because we can't make XLookupString()
	   not interpret it as Mode_switch; and interpreting it as both would
	   be totally wrong. */
	if (mode_bit) {
		const char *warn = 0;
		if (mode_bit == meta_bit)
			warn = "Meta", meta_bit = 0;
		else if (mode_bit == hyper_bit)
			warn = "Hyper", hyper_bit = 0;
		else if (mode_bit == super_bit)
			warn = "Super", super_bit = 0;
		else if (mode_bit == alt_bit)
			warn = "Alt", alt_bit = 0;
		if (warn) {
			warn_when_safe
			    (Qkey_mapping, Qwarning,
			     "SXEmacs:  %s is being used for both Mode_switch and %s.",
			     index_to_name(mode_bit), warn),
			    warned_about_overlapping_modifiers = 1;
		}
	}
#undef index_to_name

	xd->MetaMask = (meta_bit ? (1 << meta_bit) : 0);
	xd->HyperMask = (hyper_bit ? (1 << hyper_bit) : 0);
	xd->SuperMask = (super_bit ? (1 << super_bit) : 0);
	xd->AltMask = (alt_bit ? (1 << alt_bit) : 0);
	xd->ModeMask = (mode_bit ? (1 << mode_bit) : 0);	/* unused */

}

void gtk_init_modifier_mapping(struct device *d)
{
	struct gtk_device *gd = DEVICE_GTK_DATA(d);
	gd->x_keysym_map_hashtable = Qnil;
	gd->x_keysym_map = NULL;
	gd->x_modifier_keymap = NULL;
	gtk_reset_modifier_mapping(d);
}

#if 0
static int gtk_key_is_modifier_p(KeyCode keycode, struct device *d)
{
	struct gtk_device *xd = DEVICE_GTK_DATA(d);
	KeySym *syms;
	KeySym *map = (KeySym *) xd->x_keysym_map;
	int i;

	if (keycode < xd->x_keysym_map_min_code ||
	    keycode > xd->x_keysym_map_max_code)
		return 0;

	syms = &map[(keycode - xd->x_keysym_map_min_code) *
		    xd->x_keysym_map_keysyms_per_code];
	for (i = 0; i < xd->x_keysym_map_keysyms_per_code; i++)
		if (IsModifierKey(syms[i]) || syms[i] == XK_Mode_switch)	/* why doesn't IsModifierKey count this? */
			return 1;
	return 0;
}
#endif

struct _quit_predicate_closure {
	struct device *device;
	Bool *critical;
};

static Bool
quit_char_predicate(Display * display, XEvent * event, XPointer data)
{
	struct _quit_predicate_closure *cl =
	    (struct _quit_predicate_closure *)data;
	struct device *d = cl->device;
	struct frame *f = NULL;
	struct gtk_device *gd = DEVICE_GTK_DATA(d);
	char c, quit_char;
	Bool *critical = cl->critical;
	Lisp_Object keysym;
	GdkWindow *window = gdk_window_lookup(event->xany.window);
	guint32 keycode = 0;
	GdkEventKey gdk_event;

	if (window)
		f = gtk_any_window_to_frame(d, window);

	if (critical)
		*critical = False;

	if ((event->type != KeyPress) ||
	    (!window) ||
	    (!f) ||
	    (event->xkey.state
	     & (gd->MetaMask | gd->HyperMask | gd->SuperMask | gd->AltMask))) {
		return 0;
	}

	{
		char dummy[256];
		XLookupString(&(event->xkey), dummy, 200, (KeySym *) & keycode,
			      0);
	}

	memset(&gdk_event, 0, sizeof(gdk_event));
	gdk_event.type = GDK_KEY_PRESS;
	gdk_event.window = window;
	gdk_event.keyval = keycode;
	gdk_event.state = event->xkey.state;

	/* This duplicates some code that exists elsewhere, but it's relatively
	   fast and doesn't cons. */
	keysym = gtk_to_emacs_keysym(d, &gdk_event, 1);
	if (NILP(keysym))
		return 0;
	if (CHAR_OR_CHAR_INTP(keysym))
		c = XCHAR_OR_CHAR_INT(keysym);
	/* Highly doubtful that these are the quit character, but... */
	else if (EQ(keysym, QKbackspace))
		c = '\b';
	else if (EQ(keysym, QKtab))
		c = '\t';
	else if (EQ(keysym, QKlinefeed))
		c = '\n';
	else if (EQ(keysym, QKreturn))
		c = '\r';
	else if (EQ(keysym, QKescape))
		c = 27;
	else if (EQ(keysym, QKspace))
		c = ' ';
	else if (EQ(keysym, QKdelete))
		c = 127;
	else
		return 0;

	if (event->xkey.state & gd->MetaMask)
		c |= 0x80;
	if ((event->xkey.state & ControlMask) && !(c >= 'A' && c <= 'Z'))
		c &= 0x1F;	/* unshifted control characters */
	quit_char = CONSOLE_QUIT_CHAR(XCONSOLE(DEVICE_CONSOLE(d)));

	if (c == quit_char)
		return True;
	/* If we've got Control-Shift-G instead of Control-G, that means
	   we have a critical_quit.  Caps_Lock is its own modifier, so it
	   won't cause ^G to act differently than before. */
	if (event->xkey.state & ControlMask)
		c &= 0x1F;
	if (c == quit_char) {
		if (critical)
			*critical = True;
		return True;
	}
	return False;
}

static void gtk_check_for_quit_char(struct device *d)
{
	XEvent event;
	int queued;
	Bool critical_quit = False;
	struct _quit_predicate_closure closure;

	XEventsQueued(GDK_DISPLAY(), QueuedAfterReading);

	closure.device = d;
	closure.critical = &critical_quit;

	queued =
	    XCheckIfEvent(GDK_DISPLAY(), &event, quit_char_predicate,
			  (char *)&closure);

	if (queued) {
		Vquit_flag = (critical_quit ? Qcritical : Qt);
	}
}
