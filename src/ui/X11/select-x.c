/* X Selection processing for SXEmacs
   Copyright (C) 1990, 1991, 1992, 1993, 1994 Free Software Foundation, Inc.

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


/* Synched up with: Not synched with FSF. */

/* Rewritten by jwz */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "console-x.h"
#include "objects-x.h"

#include "ui/frame.h"
#include "opaque.h"
#include "systime.h"
#include "ui/select.h"

int lisp_to_time(Lisp_Object, time_t *);
Lisp_Object time_to_lisp(time_t);

#ifdef LWLIB_USES_MOTIF
# define MOTIF_CLIPBOARDS
#endif

#ifdef MOTIF_CLIPBOARDS
# include <Xm/CutPaste.h>
static void hack_motif_clipboard_selection(Atom selection_atom,
					   Lisp_Object selection_value,
					   Time thyme, Display * display,
					   Window selecting_window,
					   int owned_p);
#endif

#define CUT_BUFFER_SUPPORT

#ifdef CUT_BUFFER_SUPPORT
Lisp_Object QCUT_BUFFER0, QCUT_BUFFER1, QCUT_BUFFER2, QCUT_BUFFER3,
    QCUT_BUFFER4, QCUT_BUFFER5, QCUT_BUFFER6, QCUT_BUFFER7;
#endif

Lisp_Object Vx_sent_selection_hooks;

/* If this is a smaller number than the max-request-size of the display,
   emacs will use INCR selection transfer when the selection is larger
   than this.  The max-request-size is usually around 64k, so if you want
   emacs to use incremental selection transfers when the selection is
   smaller than that, set this.  I added this mostly for debugging the
   incremental transfer stuff, but it might improve server performance.
 */
#define MAX_SELECTION_QUANTUM 0xFFFFFF

#define SELECTION_QUANTUM(dpy) ((XMaxRequestSize (dpy) << 2) - 100)

/* If the selection owner takes too long to reply to a selection request,
   we give up on it.  This is in seconds (0 = no timeout).
 */
Fixnum x_selection_timeout;

/* Enable motif selection optimizations. */
int x_selection_strict_motif_ownership;

/* Utility functions */

static void lisp_data_to_selection_data(struct device *, Lisp_Object obj,
					unsigned char **data_ret, Atom * type_ret,
					unsigned int *size_ret, int *format_ret);
static Lisp_Object selection_data_to_lisp_data(struct device *, Extbyte * data,
					       size_t size, Atom type, int format);
static Lisp_Object x_get_window_property_as_lisp_data(Display *, Window, Atom property,
						      Lisp_Object target_type, Atom selection_atom);

static int expect_property_change(Display *, Window, Atom prop, int state);
static void wait_for_property_change(long);
static void unexpect_property_change(int);
static int
waiting_for_other_props_on_window(Display *, Window)
#if defined __GNUC__
	__attribute__((unused));
#endif
	;
/* This converts a Lisp symbol to a server Atom, avoiding a server
   roundtrip whenever possible.
 */
static Atom
symbol_to_x_atom(struct device *d, Lisp_Object sym, int only_if_exists)
{
	Display *display = DEVICE_X_DISPLAY(d);

	if (NILP(sym))
		return XA_PRIMARY;
	if (EQ(sym, Qt))
		return XA_SECONDARY;
	if (EQ(sym, QPRIMARY))
		return XA_PRIMARY;
	if (EQ(sym, QSECONDARY))
		return XA_SECONDARY;
	if (EQ(sym, QSTRING))
		return XA_STRING;
	if (EQ(sym, QINTEGER))
		return XA_INTEGER;
	if (EQ(sym, QATOM))
		return XA_ATOM;
	if (EQ(sym, QCLIPBOARD))
		return DEVICE_XATOM_CLIPBOARD(d);
	if (EQ(sym, QTIMESTAMP))
		return DEVICE_XATOM_TIMESTAMP(d);
	if (EQ(sym, QTEXT))
		return DEVICE_XATOM_TEXT(d);
	if (EQ(sym, QDELETE))
		return DEVICE_XATOM_DELETE(d);
	if (EQ(sym, QMULTIPLE))
		return DEVICE_XATOM_MULTIPLE(d);
	if (EQ(sym, QINCR))
		return DEVICE_XATOM_INCR(d);
	if (EQ(sym, QEMACS_TMP))
		return DEVICE_XATOM_EMACS_TMP(d);
	if (EQ(sym, QTARGETS))
		return DEVICE_XATOM_TARGETS(d);
	if (EQ(sym, QNULL))
		return DEVICE_XATOM_NULL(d);
	if (EQ(sym, QATOM_PAIR))
		return DEVICE_XATOM_ATOM_PAIR(d);
	if (EQ(sym, QCOMPOUND_TEXT))
		return DEVICE_XATOM_COMPOUND_TEXT(d);

#ifdef CUT_BUFFER_SUPPORT
	if (EQ(sym, QCUT_BUFFER0))
		return XA_CUT_BUFFER0;
	if (EQ(sym, QCUT_BUFFER1))
		return XA_CUT_BUFFER1;
	if (EQ(sym, QCUT_BUFFER2))
		return XA_CUT_BUFFER2;
	if (EQ(sym, QCUT_BUFFER3))
		return XA_CUT_BUFFER3;
	if (EQ(sym, QCUT_BUFFER4))
		return XA_CUT_BUFFER4;
	if (EQ(sym, QCUT_BUFFER5))
		return XA_CUT_BUFFER5;
	if (EQ(sym, QCUT_BUFFER6))
		return XA_CUT_BUFFER6;
	if (EQ(sym, QCUT_BUFFER7))
		return XA_CUT_BUFFER7;
#endif				/* CUT_BUFFER_SUPPORT */

	{
		const char *nameext;
		LISP_STRING_TO_EXTERNAL(Fsymbol_name(sym), nameext, Qctext);
		return XInternAtom(display, nameext,
				   only_if_exists ? True : False);
	}
}

/* This converts a server Atom to a Lisp symbol, avoiding server roundtrips
   and calls to intern whenever possible.
 */
static Lisp_Object
x_atom_to_symbol(struct device *d, Atom atom)
{
	Display *display = DEVICE_X_DISPLAY(d);

	if (!atom)
		return Qnil;
	if (atom == XA_PRIMARY)
		return QPRIMARY;
	if (atom == XA_SECONDARY)
		return QSECONDARY;
	if (atom == XA_STRING)
		return QSTRING;
	if (atom == XA_INTEGER)
		return QINTEGER;
	if (atom == XA_ATOM)
		return QATOM;
	if (atom == DEVICE_XATOM_CLIPBOARD(d))
		return QCLIPBOARD;
	if (atom == DEVICE_XATOM_TIMESTAMP(d))
		return QTIMESTAMP;
	if (atom == DEVICE_XATOM_TEXT(d))
		return QTEXT;
	if (atom == DEVICE_XATOM_DELETE(d))
		return QDELETE;
	if (atom == DEVICE_XATOM_MULTIPLE(d))
		return QMULTIPLE;
	if (atom == DEVICE_XATOM_INCR(d))
		return QINCR;
	if (atom == DEVICE_XATOM_EMACS_TMP(d))
		return QEMACS_TMP;
	if (atom == DEVICE_XATOM_TARGETS(d))
		return QTARGETS;
	if (atom == DEVICE_XATOM_NULL(d))
		return QNULL;
	if (atom == DEVICE_XATOM_ATOM_PAIR(d))
		return QATOM_PAIR;
	if (atom == DEVICE_XATOM_COMPOUND_TEXT(d))
		return QCOMPOUND_TEXT;

#ifdef CUT_BUFFER_SUPPORT
	if (atom == XA_CUT_BUFFER0)
		return QCUT_BUFFER0;
	if (atom == XA_CUT_BUFFER1)
		return QCUT_BUFFER1;
	if (atom == XA_CUT_BUFFER2)
		return QCUT_BUFFER2;
	if (atom == XA_CUT_BUFFER3)
		return QCUT_BUFFER3;
	if (atom == XA_CUT_BUFFER4)
		return QCUT_BUFFER4;
	if (atom == XA_CUT_BUFFER5)
		return QCUT_BUFFER5;
	if (atom == XA_CUT_BUFFER6)
		return QCUT_BUFFER6;
	if (atom == XA_CUT_BUFFER7)
		return QCUT_BUFFER7;
#endif

	{
		char *intstr;
		char *str = XGetAtomName(display, atom);

		if (!str)
			return Qnil;

		TO_INTERNAL_FORMAT(C_STRING, str,
				   C_STRING_ALLOCA, intstr, Qctext);
		XFree(str);
		return intern(intstr);
	}
}

/* Do protocol to assert ourself as a selection owner.
 */
static Lisp_Object
x_own_selection(Lisp_Object selection_name, Lisp_Object selection_value,
		Lisp_Object how_to_add, Lisp_Object selection_type, int owned_p)
{
	struct device *d = decode_x_device(Qnil);
	Display *display = DEVICE_X_DISPLAY(d);
	struct frame *sel_frame = selected_frame();
	Window selecting_window = XtWindow(FRAME_X_TEXT_WIDGET(sel_frame));
	Lisp_Object selection_time;
	/* Use the time of the last-read mouse or keyboard event.
	   For selection purposes, we use this as a sleazy way of knowing what the
	   current time is in server-time.  This assumes that the most recently read
	   mouse or keyboard event has something to do with the assertion of the
	   selection, which is probably true.
	 */
	Time thyme = DEVICE_X_MOUSE_TIMESTAMP(d);
	Atom selection_atom;

	CHECK_SYMBOL(selection_name);
	selection_atom = symbol_to_x_atom(d, selection_name, 0);

	XSetSelectionOwner(display, selection_atom, selecting_window, thyme);

	/* We do NOT use time_to_lisp() here any more, like we used to.
	   That assumed equivalence of time_t and Time, which is not
	   necessarily the case (e.g. under OSF on the Alphas, where
	   Time is a 64-bit quantity and time_t is a 32-bit quantity).

	   Opaque pointers are the clean way to go here.
	 */
	selection_time = make_opaque(&thyme, sizeof(thyme));

#ifdef MOTIF_CLIPBOARDS
	hack_motif_clipboard_selection(selection_atom, selection_value,
				       thyme, display, selecting_window,
				       owned_p);
#endif
	return selection_time;
}

#ifdef MOTIF_CLIPBOARDS		/* Bend over baby.  Take it and like it. */

# ifdef MOTIF_INCREMENTAL_CLIPBOARDS_WORK
static void motif_clipboard_cb();
# endif

static void
hack_motif_clipboard_selection(Atom selection_atom,
			       Lisp_Object selection_value,
			       Time thyme,
			       Display * display,
			       Window selecting_window, int owned_p)
{
	struct device *d = get_device_from_display(display);
	/* Those Motif wankers can't be bothered to follow the ICCCM, and do
	   their own non-Xlib non-Xt clipboard processing.  So we have to do
	   this so that linked-in Motif widgets don't get themselves wedged.
	 */
	if (selection_atom == DEVICE_XATOM_CLIPBOARD(d)
	    && STRINGP(selection_value)

	    /* If we already own the clipboard, don't own it again in the Motif
	       way.  This might lose in some subtle way, since the timestamp won't
	       be current, but owning the selection on the Motif way does a
	       SHITLOAD of X protocol, and it makes killing text be incredibly
	       slow when using an X terminal.  ARRRRGGGHHH!!!!
	     */
	    /* No, this is no good, because then Motif text fields don't bother
	       to look up the new value, and you can't Copy from a buffer, Paste
	       into a text field, then Copy something else from the buffer and
	       paste it into the text field -- it pastes the first thing again. */
	    && (!owned_p
		/* Selectively re-enable this because for most users its
		   just too painful - especially over a remote link. */
		|| x_selection_strict_motif_ownership)
	    ) {
#ifdef MOTIF_INCREMENTAL_CLIPBOARDS_WORK
		Widget widget = FRAME_X_TEXT_WIDGET(selected_frame());
#endif
		long itemid;
#if XmVersion >= 1002
		long dataid;
#else
		int dataid;	/* 1.2 wants long, but 1.1.5 wants int... */
#endif
		XmString fmh;
		String encoding = "STRING";
		const Bufbyte *data = XSTRING_DATA(selection_value);
		Bytecount bytes = XSTRING_LENGTH(selection_value);

#ifdef MULE
		{
			enum { ASCII, LATIN_1, WORLD } chartypes = ASCII;
			const Bufbyte *ptr = data, *end = ptr + bytes;
			/* Optimize for the common ASCII case */
			while (ptr <= end) {
				if (BYTE_ASCII_P(*ptr)) {
					ptr++;
					continue;
				}

				if ((*ptr) == LEADING_BYTE_LATIN_ISO8859_1 ||
				    (*ptr) == LEADING_BYTE_CONTROL_1) {
					chartypes = LATIN_1;
					ptr += 2;
					continue;
				}

				chartypes = WORLD;
				break;
			}

			if (chartypes == LATIN_1)
				TO_EXTERNAL_FORMAT(LISP_STRING, selection_value,
						   ALLOCA, (data, bytes),
						   Qbinary);
			else if (chartypes == WORLD) {
				TO_EXTERNAL_FORMAT(LISP_STRING, selection_value,
						   ALLOCA, (data, bytes),
						   Qctext);
				encoding = "COMPOUND_TEXT";
			}
		}
#endif				/* MULE */

		fmh = XmStringCreateLtoR("Clipboard", XmSTRING_DEFAULT_CHARSET);
		while (ClipboardSuccess !=
		       XmClipboardStartCopy(display, selecting_window, fmh,
					    thyme,
#ifdef MOTIF_INCREMENTAL_CLIPBOARDS_WORK
					    widget, motif_clipboard_cb,
#else
					    0, NULL,
#endif
					    &itemid)) ;
		XmStringFree(fmh);
		while (ClipboardSuccess !=
		       XmClipboardCopy(display, selecting_window, itemid,
				       encoding,
#ifdef MOTIF_INCREMENTAL_CLIPBOARDS_WORK
				       /* O'Reilly examples say size can be 0,
					  but this clearly is not the case. */
				       0, bytes, (int)selecting_window,	/* private id */
#else				/* !MOTIF_INCREMENTAL_CLIPBOARDS_WORK */
				       (XtPointer) data, bytes, 0,
#endif				/* !MOTIF_INCREMENTAL_CLIPBOARDS_WORK */
				       &dataid)) ;
		while (ClipboardSuccess !=
		       XmClipboardEndCopy(display, selecting_window, itemid)) ;
	}
}

# ifdef MOTIF_INCREMENTAL_CLIPBOARDS_WORK
/* I tried to treat the clipboard like a real selection, and not send
   the data until it was requested, but it looks like that just doesn't
   work at all unless the selection owner and requestor are in different
   processes.  From reading the Motif source, it looks like they never
   even considered having two widgets in the same application transfer
   data between each other using "by-name" clipboard values.  What a
   bunch of fuckups.
 */
static void
motif_clipboard_cb(Widget widget, int *data_id, int *private_id, int *reason)
{
	switch (*reason) {
	case XmCR_CLIPBOARD_DATA_REQUEST: {
		Display *dpy = XtDisplay(widget);
		Window window = (Window) * private_id;
		Lisp_Object selection =
			select_convert_out(QCLIPBOARD, Qnil, Qnil);

		/* Whichever lazy git wrote this originally just called abort()
		   when anything didn't go their way... */

		/* Try some other text types */
		if (NILP(selection))
			selection =
				select_convert_out(QCLIPBOARD, QSTRING,
						   Qnil);
		if (NILP(selection))
			selection =
				select_convert_out(QCLIPBOARD, QTEXT, Qnil);
		if (NILP(selection))
			selection =
				select_convert_out(QCLIPBOARD,
						   QCOMPOUND_TEXT, Qnil);

		if (CONSP(selection) && SYMBOLP(XCAR(selection))
		    && (EQ(XCAR(selection), QSTRING)
			|| EQ(XCAR(selection), QTEXT)
			|| EQ(XCAR(selection), QCOMPOUND_TEXT)))
			selection = XCDR(selection);

		if (NILP(selection))
			signal_error(Qselection_conversion_error,
				     build_string("no selection"));

		if (!STRINGP(selection))
			signal_error(Qselection_conversion_error,
				     build_string
				     ("couldn't convert selection to string"));

		XmClipboardCopyByName(dpy, window, *data_id,
				      (char *)XSTRING_DATA(selection),
				      XSTRING_LENGTH(selection) + 1, 0);
		break;
	}
	case XmCR_CLIPBOARD_DATA_DELETE:
	default:
		/* don't need to free anything */
		break;
	}
}
# endif				/* MOTIF_INCREMENTAL_CLIPBOARDS_WORK */
#endif				/* MOTIF_CLIPBOARDS */

/* Send a SelectionNotify event to the requestor with property=None, meaning
   we were unable to do what they wanted.
 */
static void
x_decline_selection_request(XSelectionRequestEvent * event)
{
	XSelectionEvent reply;
	reply.type = SelectionNotify;
	reply.display = event->display;
	reply.requestor = event->requestor;
	reply.selection = event->selection;
	reply.time = event->time;
	reply.target = event->target;
	reply.property = None;

	XSendEvent(reply.display, reply.requestor, False, 0L,
		   (XEvent *) & reply);
	XFlush(reply.display);
}

/* Used as an unwind-protect clause so that, if a selection-converter signals
   an error, we tell the requestor that we were unable to do what they wanted
   before we throw to top-level or go into the debugger or whatever.
 */
static Lisp_Object
x_selection_request_lisp_error(Lisp_Object closure)
{
	XSelectionRequestEvent *event = (XSelectionRequestEvent *)
	    get_opaque_ptr(closure);

	free_opaque_ptr(closure);
	if (event->type == 0)	/* we set this to mean "completed normally" */
		return Qnil;
	x_decline_selection_request(event);
	return Qnil;
}

/* Convert our selection to the requested type, and put that data where the
   requestor wants it.  Then tell them whether we've succeeded.
 */
static void
x_reply_selection_request(XSelectionRequestEvent * event, int format,
			  unsigned char *data, int size, Atom type)
{
	/* This function can GC */
	XSelectionEvent reply;
	Display *display = event->display;
#ifdef HAVE_XTREGISTERDRAWABLE
	struct device *d = get_device_from_display(display);
#endif
	Window window = event->requestor;
	int bytes_remaining;
	int format_bytes = format / 8;
	int max_bytes = SELECTION_QUANTUM(display);
	if (max_bytes > MAX_SELECTION_QUANTUM)
		max_bytes = MAX_SELECTION_QUANTUM;

	reply.type = SelectionNotify;
	reply.display = display;
	reply.requestor = window;
	reply.selection = event->selection;
	reply.time = event->time;
	reply.target = event->target;
	reply.property =
	    (event->property == None ? event->target : event->property);

	/* #### XChangeProperty can generate BadAlloc, and we must handle it! */

	/* Store the data on the requested property.
	   If the selection is large, only store the first N bytes of it.
	 */
	bytes_remaining = size * format_bytes;
	if (bytes_remaining <= max_bytes) {
		/* Send all the data at once, with minimal handshaking. */
#if 0
		stderr_out("\nStoring all %d\n", bytes_remaining);
#endif
		XChangeProperty(display, window, reply.property, type, format,
				PropModeReplace, data, size);
		/* At this point, the selection was successfully stored; ack it. */
		XSendEvent(display, window, False, 0L, (XEvent *) & reply);
		XFlush(display);
	} else {
#ifndef HAVE_XTREGISTERDRAWABLE
		invalid_operation("Copying that much data requires X11R6.", Qunbound);
#else
		/* Send an INCR selection. */
		int prop_id;
		Widget widget = FRAME_X_TEXT_WIDGET (XFRAME(DEVICE_SELECTED_FRAME(d)));

		if (x_window_to_frame(d, window))	/* #### debug */
			error("attempt to transfer an INCR to ourself!");
#if 0
		stderr_out("\nINCR %d\n", bytes_remaining);
#endif
      /* Tell Xt not to drop PropertyNotify events that arrive for the
	 target window, rather, pass them to us. This would be a hack, but
	 the Xt selection routines are broken for our purposes--we can't
	 pass them callbacks from Lisp, for example. Let's call it a
	 workaround.

	 The call to wait_for_property_change means we can break out of that
	 function, switch to another frame on the same display (which will
	 be another Xt widget), select a huge amount of text, and have the
	 same (foreign) app ask for another incremental selection
	 transfer. Programming like X11 made sense, would mean that, in that
	 case, XtRegisterDrawable is called twice with different widgets.

	 Since the results of calling XtRegisterDrawable when the drawable
	 is already registered with another widget are undefined, we want to
	 avoid that--so, only call it when XtWindowToWidget returns NULL,
	 which it will only do with a valid Window if it's not already
	 registered. */
		if (NULL == XtWindowToWidget(display, window)) {
			XtRegisterDrawable(display, (Drawable)window, widget);
		}

		prop_id =
		    expect_property_change(display, window, reply.property,
					   PropertyDelete);

		XChangeProperty(display, window, reply.property,
				DEVICE_XATOM_INCR(d), 32, PropModeReplace,
				(unsigned char *)
				&bytes_remaining, 1);
		XSelectInput(display, window, PropertyChangeMask);
		/* Tell 'em the INCR data is there... */
		XSendEvent(display, window, False, 0L, (XEvent *) & reply);
		XFlush(display);

		/* First, wait for the requestor to ack by deleting the property.
		   This can run random lisp code (process handlers) or signal.
		 */
		wait_for_property_change(prop_id);

		while (bytes_remaining) {
			int i = ((bytes_remaining < max_bytes)
				 ? bytes_remaining : max_bytes);
			prop_id =
			    expect_property_change(display, window,
						   reply.property,
						   PropertyDelete);
#if 0
			stderr_out("  INCR adding %d\n", i);
#endif
			/* Append the next chunk of data to the property. */
			XChangeProperty(display, window, reply.property, type,
					format, PropModeAppend, data,
					i / format_bytes);
			bytes_remaining -= i;
			data += i;

			/* Now wait for the requestor to ack this chunk by deleting the
			   property.   This can run random lisp code or signal.
			 */
			wait_for_property_change(prop_id);
		}
		/* Now write a zero-length chunk to the property to tell the requestor
		   that we're done. */
#if 0
		stderr_out("  INCR done\n");
#endif
		if (!waiting_for_other_props_on_window(display, window)) {
			XSelectInput(display, window, 0L);
			XtUnregisterDrawable(display, (Drawable)window);
		}

		XChangeProperty(display, window, reply.property, type, format,
				PropModeReplace, data, 0);
#endif	/* HAVE_XTREGISTERDRAWABLE */
	}
}

/* Called from the event-loop in response to a SelectionRequest event.
 */
void
x_handle_selection_request(XSelectionRequestEvent * event)
{
	/* This function can GC */
	struct gcpro gcpro1, gcpro2;
	Lisp_Object temp_obj;
	Lisp_Object selection_symbol;
	Lisp_Object target_symbol = Qnil;
	Lisp_Object converted_selection = Qnil;
	Time local_selection_time;
	Lisp_Object successful_p = Qnil;
	int count;
	struct device *d = get_device_from_display(event->display);

	GCPRO2(converted_selection, target_symbol);

	selection_symbol = x_atom_to_symbol(d, event->selection);
	target_symbol = x_atom_to_symbol(d, event->target);

#if 0				/* #### MULTIPLE doesn't work yet */
	if (EQ(target_symbol, QMULTIPLE))
		target_symbol = fetch_multiple_target(event);
#endif

	temp_obj = Fget_selection_timestamp(selection_symbol);

	if (NILP(temp_obj)) {
		/* We don't appear to have the selection. */
		x_decline_selection_request(event);

		goto DONE_LABEL;
	}

	local_selection_time = *(Time *) XOPAQUE_DATA(temp_obj);

	if (event->time != CurrentTime && local_selection_time > event->time) {
		/* Someone asked for the selection, and we have one, but not the one
		   they're looking for. */
		x_decline_selection_request(event);
		goto DONE_LABEL;
	}

	converted_selection = select_convert_out(selection_symbol,
						 target_symbol, Qnil);

	/* #### Is this the right thing to do? I'm no X expert. -- ajh */
	if (NILP(converted_selection)) {
		/* We don't appear to have a selection in that data type. */
		x_decline_selection_request(event);
		goto DONE_LABEL;
	}

	count = specpdl_depth();
	record_unwind_protect(x_selection_request_lisp_error,
			      make_opaque_ptr(event));

	{
		unsigned char *data;
		unsigned int size;
		int format;
		Atom type;
		lisp_data_to_selection_data(d, converted_selection,
					    &data, &type, &size, &format);

		x_reply_selection_request(event, format, data, size, type);
		successful_p = Qt;
		/* Tell x_selection_request_lisp_error() it's cool. */
		event->type = 0;
		xfree(data);
	}

	unbind_to(count, Qnil);

      DONE_LABEL:

	UNGCPRO;

	/* Let random lisp code notice that the selection has been asked for. */
	{
		Lisp_Object val = Vx_sent_selection_hooks;
		if (!UNBOUNDP(val) && !NILP(val)) {
			Lisp_Object rest;
			if (CONSP(val) && !EQ(XCAR(val), Qlambda))
				for (rest = val; !NILP(rest); rest = Fcdr(rest))
					call3(Fcar(rest), selection_symbol,
					      target_symbol, successful_p);
			else
				call3(val, selection_symbol, target_symbol,
				      successful_p);
		}
	}
}

/* Called from the event-loop in response to a SelectionClear event.
 */
void
x_handle_selection_clear(XSelectionClearEvent * event)
{
	Display *display = event->display;
	struct device *d = get_device_from_display(display);
	Atom selection = event->selection;
	Time changed_owner_time = event->time;

	Lisp_Object selection_symbol, local_selection_time_lisp;
	Time local_selection_time;

	selection_symbol = x_atom_to_symbol(d, selection);

	local_selection_time_lisp = Fget_selection_timestamp(selection_symbol);

	/* We don't own the selection, so that's fine. */
	if (NILP(local_selection_time_lisp))
		return;

	local_selection_time =
	    *(Time *) XOPAQUE_DATA(local_selection_time_lisp);

	/* This SelectionClear is for a selection that we no longer own, so we can
	   disregard it.  (That is, we have reasserted the selection since this
	   request was generated.)
	 */
	if (changed_owner_time != CurrentTime &&
	    local_selection_time > changed_owner_time)
		return;

	handle_selection_clear(selection_symbol);
}

/* This stuff is so that INCR selections are reentrant (that is, so we can
   be servicing multiple INCR selection requests simultaneously).  I haven't
   actually tested that yet.
 */

static int prop_location_tick;

static struct prop_location {
	int tick;
	Display *display;
	Window window;
	Atom property;
	int desired_state;
	struct prop_location *next;
} *for_whom_the_bell_tolls;

static int
property_deleted_p(void *tick)
{
	struct prop_location *rest = for_whom_the_bell_tolls;
	while (rest)
		if (rest->tick == (long)tick)
			return 0;
		else
			rest = rest->next;
	return 1;
}

static int
waiting_for_other_props_on_window(Display * display, Window window)
{
	struct prop_location *rest = for_whom_the_bell_tolls;
	while (rest)
		if (rest->display == display && rest->window == window)
			return 1;
		else
			rest = rest->next;
	return 0;
}

static int
expect_property_change(Display * display, Window window,
		       Atom property, int state)
{
	struct prop_location *pl = xnew(struct prop_location);
	pl->tick = ++prop_location_tick;
	pl->display = display;
	pl->window = window;
	pl->property = property;
	pl->desired_state = state;
	pl->next = for_whom_the_bell_tolls;
	for_whom_the_bell_tolls = pl;
	return pl->tick;
}

static void
unexpect_property_change(int tick)
{
	struct prop_location *prev = 0, *rest = for_whom_the_bell_tolls;
	while (rest) {
		if (rest->tick == tick) {
			if (prev)
				prev->next = rest->next;
			else
				for_whom_the_bell_tolls = rest->next;
			xfree(rest);
			return;
		}
		prev = rest;
		rest = rest->next;
	}
}

static void
wait_for_property_change(long tick)
{
	/* This function can GC */
	wait_delaying_user_input(property_deleted_p, (void *)tick);
}

/*
 * Called from the event-loop in response to a PropertyNotify event.
 */
void
x_handle_property_notify(XPropertyEvent * event)
{
	struct prop_location *prev = 0, *rest = for_whom_the_bell_tolls;
	while (rest) {
		if (rest->property == event->atom &&
		    rest->window == event->window &&
		    rest->display == event->display &&
		    rest->desired_state == event->state)
		{
			if (prev)
				prev->next = rest->next;
			else
				for_whom_the_bell_tolls = rest->next;
			xfree(rest);
			return;
		}
		prev = rest; rest = rest->next;
	}
}


#if 0				/* #### MULTIPLE doesn't work yet */
static Lisp_Object
fetch_multiple_target(XSelectionRequestEvent * event)
{
	/* This function can GC */
	Display * display = event->display;
	Window window = event->requestor;
	Atom target = event->target;
	Atom selection_atom = event->selection;
	int result;

	return Fcons(QMULTIPLE, x_get_window_property_as_lisp_data
		     (display, window, target, QMULTIPLE, selection_atom));
}

static Lisp_Object
copy_multiple_data(Lisp_Object obj)
{
	Lisp_Object vec;
	int i; int len;

	if (CONSP(obj))
		return Fcons(XCAR(obj), copy_multiple_data(XCDR(obj)));

	CHECK_VECTOR(obj); len = XVECTOR_LENGTH(obj);
	vec = make_vector(len, Qnil);
	for (i = 0; i < len; i++) {
		Lisp_Object vec2 = XVECTOR_DATA(obj)[i];
		CHECK_VECTOR(vec2);
		if (XVECTOR_LENGTH(vec2) != 2)
			signal_error(Qerror, list2(build_string ("vectors must be of length 2"), vec2));
		XVECTOR_DATA(vec)[i] = make_vector(2, Qnil);
		XVECTOR_DATA(XVECTOR_DATA(vec)[i])[0] =
			XVECTOR_DATA(vec2)[0];
		XVECTOR_DATA(XVECTOR_DATA(vec)[i])[1] =
			XVECTOR_DATA(vec2)[1];
	}
	return vec;
}
#endif /* 0 */


static Window reading_selection_reply;
static Atom reading_which_selection;
static int selection_reply_timed_out;

static int
selection_reply_done(void *ignore)
{
	return !reading_selection_reply;
}

static Lisp_Object Qx_selection_reply_timeout_internal;

DEFUN("x-selection-reply-timeout-internal", Fx_selection_reply_timeout_internal, 1, 1, 0, /*
*/
      (arg))
{
	selection_reply_timed_out = 1;
	reading_selection_reply = 0;

	return Qnil;
}

/*
 * Do protocol to read selection-data from the server.
 * Converts this to lisp data and returns it.
 */
static Lisp_Object
x_get_foreign_selection(Lisp_Object selection_symbol, Lisp_Object target_type)
{
	/* This function can GC */
	struct device *d = decode_x_device(Qnil);
	Display * display = DEVICE_X_DISPLAY(d);
	struct frame *sel_frame = selected_frame();
	Window requestor_window = XtWindow(FRAME_X_TEXT_WIDGET(sel_frame));
	Time requestor_time = CurrentTime;
	Atom target_property = DEVICE_XATOM_EMACS_TMP(d);
	Atom selection_atom = symbol_to_x_atom(d, selection_symbol, 0);
	int speccount;
	Atom type_atom = symbol_to_x_atom(d, (CONSP(target_type) ? XCAR(target_type) : target_type), 0);

	XConvertSelection(display, selection_atom, type_atom, target_property,
			  requestor_window, requestor_time);
	/* Block until the reply has been read. */
	reading_selection_reply = requestor_window;
	reading_which_selection = selection_atom;
	selection_reply_timed_out = 0;
	speccount = specpdl_depth();
	/* add a timeout handler */
	if (x_selection_timeout > 0) {
		Lisp_Object id = Fadd_timeout(make_int(x_selection_timeout),
					      Qx_selection_reply_timeout_internal,
					      Qnil, Qnil);
		record_unwind_protect(Fdisable_timeout, id);
	}

	/* This is ^Gable */
	wait_delaying_user_input(selection_reply_done, 0);
	if (selection_reply_timed_out)
		error("timed out waiting for reply from selection owner");
	unbind_to(speccount, Qnil);
	/* otherwise, the selection is waiting for us on the requested property. */
	return select_convert_in(selection_symbol, target_type,
				 x_get_window_property_as_lisp_data (display, requestor_window,
								     target_property, target_type,
								     selection_atom));
}

static void
x_get_window_property(Display * display, Window window, Atom property,
		      Extbyte ** data_ret, int *bytes_ret, Atom * actual_type_ret,
		      int *actual_format_ret, unsigned long *actual_size_ret,
		      int delete_p)
{
	size_t total_size;
	unsigned long bytes_remaining;
	int offset = 0; unsigned char *tmp_data = 0;
	int result;
	int buffer_size = SELECTION_QUANTUM(display);

	if (buffer_size > MAX_SELECTION_QUANTUM)
		buffer_size = MAX_SELECTION_QUANTUM;
	/* First probe the thing to find out how big it is. */
	result = XGetWindowProperty(display, window, property, 0, 0,
				    False, AnyPropertyType, actual_type_ret,
				   actual_format_ret, actual_size_ret,
				   &bytes_remaining, &tmp_data);
	if (result != Success) {
		*data_ret = 0;
		*bytes_ret = 0;
		return;
	}
	XFree((char *)tmp_data);
	if (*actual_type_ret == None || *actual_format_ret == 0) {
		if (delete_p)
			XDeleteProperty(display, window, property);
		*data_ret = 0;
		*bytes_ret = 0;
		return;
	}

	total_size = bytes_remaining + 1;
	*data_ret = (Extbyte *) xmalloc_atomic(total_size);
	/* Now read, until we've gotten it all. */
	while (bytes_remaining) {
		result = XGetWindowProperty(display, window, property,
					   offset / 4, buffer_size / 4,
					   (delete_p ? True : False),
					   AnyPropertyType, actual_type_ret,
					   actual_format_ret, actual_size_ret,
					   &bytes_remaining, &tmp_data);

		/* If this doesn't return Success at this point, it means that
		   some clod deleted the selection while we were in the midst of
		   reading it.  Deal with that, I guess....
		*/
		if (result != Success)
			break;
		*actual_size_ret *= *actual_format_ret / 8;
		memcpy((*data_ret) + offset, tmp_data, *actual_size_ret);
		offset += *actual_size_ret;
		XFree((char *)tmp_data);
	}
	*bytes_ret = offset;
}

static void
receive_incremental_selection(Display * display, Window window, Atom property,
			      /* this one is for error messages only */
			      Lisp_Object target_type, unsigned int min_size_bytes,
			      Extbyte ** data_ret, int *size_bytes_ret,
			      Atom * type_ret, int *format_ret,
			      unsigned long *size_ret)
{
	/* This function can GC */
	int offset = 0;
	int prop_id;

	*size_bytes_ret = min_size_bytes;
	*data_ret = (Extbyte *) xmalloc_atomic(*size_bytes_ret);

	/* At this point, we have read an INCR property, and deleted it (which
	   is how we ack its receipt: the sending window will be selecting
	   PropertyNotify events on our window to notice this).

	   Now, we must loop, waiting for the sending window to put a value on
	   that property, then reading the property, then deleting it to ack.
	   We are done when the sender places a property of length 0.
	*/
	prop_id = expect_property_change(display, window, property, PropertyNewValue);
	while (1) {
		Extbyte * tmp_data; int tmp_size_bytes;
		wait_for_property_change(prop_id);
		/* expect it again immediately, because x_get_window_property may
		   .. no it won't, I don't get it.
		   .. Ok, I get it now, the Xt code that implements INCR is broken.
		*/
		prop_id = expect_property_change(display, window, property, PropertyNewValue);
		x_get_window_property(display, window, property, &tmp_data, &tmp_size_bytes,
				      type_ret, format_ret, size_ret, 1);
		if (tmp_size_bytes == 0) {	/* we're done */
			unexpect_property_change(prop_id);
			if (tmp_data) xfree(tmp_data);
			break;
		}
		if (*size_bytes_ret < offset + tmp_size_bytes) {
			*size_bytes_ret = offset + tmp_size_bytes;
			*data_ret = (Extbyte *) xrealloc(*data_ret, *size_bytes_ret);
		}
		memcpy((*data_ret) + offset, tmp_data, tmp_size_bytes);
		offset += tmp_size_bytes; xfree(tmp_data);
	}
}

static Lisp_Object
x_get_window_property_as_lisp_data(Display * display, Window window, Atom property,
				   /* next two for error messages only */
				   Lisp_Object target_type, Atom selection_atom)
{
	/* This function can GC */
	Atom actual_type;
	int actual_format;
	unsigned long actual_size;
	Extbyte * data = NULL;
	int bytes = 0;
	Lisp_Object val;
	struct device *d = get_device_from_display(display);

	x_get_window_property(display, window, property, &data, &bytes, &actual_type,
			      &actual_format, &actual_size, 1);
	if (!data) {
		if (XGetSelectionOwner(display, selection_atom))
			/* there is a selection owner */
			signal_error(Qselection_conversion_error,
				     Fcons(build_string("selection owner couldn't convert"),
					   Fcons(x_atom_to_symbol (d, selection_atom),
						 actual_type ? list2(target_type, x_atom_to_symbol (d, actual_type))
						 : list1(target_type))));
			else
				signal_error(Qerror, list2(build_string ("no selection"),
							   x_atom_to_symbol(d, selection_atom)));
	}

	if (actual_type == DEVICE_XATOM_INCR(d)) {
		/* Ok, that data wasn't *the* data, it was just the beginning. */
		unsigned int min_size_bytes = *((unsigned int *)data); xfree(data);
		receive_incremental_selection(display, window, property, target_type,
					      min_size_bytes, &data, &bytes, &actual_type,
					      &actual_format, &actual_size);
	}

	/* It's been read.  Now convert it to a lisp object in some semi-rational
	   manner. */
	val = selection_data_to_lisp_data(d, data, bytes, actual_type, actual_format);
	xfree(data);
	return val;
}


/* #### These are going to move into Lisp code(!) with the aid of
   some new functions I'm working on - ajh */

/* These functions convert from the selection data read from the server into
   something that we can use from elisp, and vice versa.

   Type:	Format:	Size:		Elisp Type:
   -----	-------	-----		-----------
   *	8	*		String
   ATOM	32	1		Symbol
   ATOM	32	> 1		Vector of Symbols
   *	16	1		Integer
   *	16	> 1		Vector of Integers
   *	32	1		if <=16 bits: Integer
   if > 16 bits: Cons of top16, bot16
   *	32	> 1		Vector of the above

   When converting a Lisp number to C, it is assumed to be of format 16 if
   it is an integer, and of format 32 if it is a cons of two integers.

   When converting a vector of numbers from Elisp to C, it is assumed to be
   of format 16 if every element in the vector is an integer, and is assumed
   to be of format 32 if any element is a cons of two integers.

   When converting an object to C, it may be of the form (SYMBOL . <data>)
   where SYMBOL is what we should claim that the type is.  Format and
   representation are as above.

   NOTE: Under Mule, when someone shoves us a string without a type, we
   set the type to 'COMPOUND_TEXT and automatically convert to Compound
   Text.  If the string has a type, we assume that the user wants the
   data sent as-is so we just do "binary" conversion.
*/

static Lisp_Object
selection_data_to_lisp_data(struct device *d, Extbyte * data,
			    size_t size, Atom type, int format)
{
	if (type == DEVICE_XATOM_NULL(d))
		return QNULL;
	/* Convert any 8-bit data to a string, for compactness. */
	else if (format == 8)
		return make_ext_string(data, size, type == DEVICE_XATOM_TEXT(d)
				       || type == DEVICE_XATOM_COMPOUND_TEXT(d) ? Qctext : Qbinary);
	/* Convert a single atom to a Lisp Symbol.
	   Convert a set of atoms to a vector of symbols. */
	else if (type == XA_ATOM) {
		if (size == sizeof(Atom))
			return x_atom_to_symbol(d, *((Atom *) data));
		else {
			int i;
			int len = size / sizeof(Atom);
			Lisp_Object v = Fmake_vector(make_int(len), Qzero);
			for (i = 0; i < len; i++)
				Faset(v, make_int(i), x_atom_to_symbol(d, ((Atom *) data)[i]));
			return v;
		}
	}

	/* Convert a single 16 or small 32 bit number to a Lisp Int.
	   If the number is > 16 bits, convert it to a cons of integers,
	   16 bits in each half.
	*/
	else if (format == 32 && size == sizeof(long))
		return word_to_lisp(((unsigned long *)data)[0]);
	else if (format == 16 && size == sizeof(short))
		return make_int((int)(((unsigned short *)data)[0]));
	/* Convert any other kind of data to a vector of numbers, represented
	   as above (as an integer, or a cons of two 16 bit integers).

	   #### Perhaps we should return the actual type to lisp as well.

	   (x-get-selection-internal 'PRIMARY 'LINE_NUMBER)
	   ==> [4 4]

	   and perhaps it should be

	   (x-get-selection-internal 'PRIMARY 'LINE_NUMBER)
	   ==> (SPAN . [4 4])

	   Right now the fact that the return type was SPAN is discarded before
	   lisp code gets to see it.
	*/
	else if (format == 16) {
		int i;
		Lisp_Object v = make_vector(size / 4, Qzero);
		for (i = 0; i < (int)size / 4; i++) {
			int j = (int)((unsigned short *)data)[i];
			Faset(v, make_int(i), make_int(j));
		}
		return v;
	} else {
		int i;
		Lisp_Object v = make_vector(size / 4, Qzero);
		for (i = 0; i < (int)size / 4; i++) {
			unsigned long j = ((unsigned long *)data)[i];
			Faset(v, make_int(i), word_to_lisp(j));
		}
		return v;
	}
}

#define tmp_err_1				\
	"all elements of the vector must be of the same type"
#define tmp_err_2							\
	"elements of the vector must be vectors of exactly two elements"
#define tmp_err_3				\
	"all elements of the vector must be of the same type"
#define tmp_err_4				\
	"all elements of the vector must be integers or conses of integers"

static void
lisp_data_to_selection_data(struct device *d, Lisp_Object obj,
			    unsigned char **data_ret, Atom * type_ret,
			    unsigned int *size_ret, int *format_ret)
{
	Lisp_Object type = Qnil;

	if (CONSP(obj) && SYMBOLP(XCAR(obj))) {
		type = XCAR(obj); obj = XCDR(obj);
		if (CONSP(obj) && NILP(XCDR(obj)))
			obj = XCAR(obj);
	}
	if (EQ(obj, QNULL) || (EQ(type, QNULL))) {
		/* This is not the same as declining */
		*format_ret = 32;
		*size_ret = 0; *data_ret = 0; type = QNULL;
	} else if (STRINGP(obj)) {
		const Extbyte * extval = NULL;
		Extcount extvallen;
		TO_EXTERNAL_FORMAT(LISP_STRING, obj, ALLOCA, (extval, extvallen),
				   (NILP(type) ? Qctext : Qbinary));
		if ( extval != NULL ) {
			*format_ret = 8;
			*size_ret = extvallen;
			*data_ret = (unsigned char *)xmalloc_atomic(*size_ret);
			memcpy(*data_ret, extval, *size_ret);
		} else {
			error("Could not transcode string");
		}
#ifdef MULE
		if (NILP(type))
			type = QCOMPOUND_TEXT;
#else
		if (NILP(type)) type = QSTRING;
#endif
	} else if (CHARP(obj)) {
		Bufbyte buf[MAX_EMCHAR_LEN];
		Bytecount len;
		const Extbyte * extval = NULL;
		Extcount extvallen;
		*format_ret = 8;
		len = set_charptr_emchar(buf, XCHAR(obj));
		TO_EXTERNAL_FORMAT(DATA, (buf, len), ALLOCA, (extval, extvallen),
				   Qctext);
		if ( extval != NULL ) {
			*size_ret = extvallen;
			*data_ret = (unsigned char *)xmalloc_atomic(*size_ret);
			memcpy(*data_ret, extval, *size_ret);
		} else {
			error("Could not transcode data");
		}
#ifdef MULE
		if (NILP(type))
			type = QCOMPOUND_TEXT;
#else
		if (NILP(type))
			type = QSTRING;
#endif
	} else if (SYMBOLP(obj)) {
		*format_ret = 32;
		*size_ret = 1;
		*data_ret =
			(unsigned char *)xmalloc_atomic(sizeof(Atom) + 1);
		(*data_ret)[sizeof(Atom)] = 0;
		(*(Atom **) data_ret)[0] =
			symbol_to_x_atom(d, obj, 0);
		if (NILP(type)) type = QATOM;
	} else if (INTP(obj) && XINT(obj) <= 0x7FFF && XINT(obj) >= -0x8000) {
		*format_ret = 16;
		*size_ret = 1;
		*data_ret = (unsigned char *)xmalloc_atomic(sizeof(short) + 1);
		(*data_ret)[sizeof(short)] = 0;
		(*(short **)data_ret)[0] = (short)XINT(obj);
		if (NILP(type))
			type = QINTEGER;
	} else if (INTP(obj) || CONSP(obj)) {
		*format_ret = 32;
		*size_ret = 1;
		*data_ret = (unsigned char *)xmalloc_atomic(sizeof(long) + 1);
		(*data_ret)[sizeof(long)] = 0;
		(*(unsigned long **)data_ret)[0] = lisp_to_word(obj);
		if (NILP(type))
			type = QINTEGER;
	} else if (VECTORP(obj)) {
		/* Lisp Vectors may represent a set of ATOMs;
		   a set of 16 or 32 bit INTEGERs;
		   or a set of ATOM_PAIRs (represented as [[A1 A2] [A3 A4] ...]
		*/
		int i;
		if (SYMBOLP(XVECTOR_DATA(obj)[0])) {
			/* This vector is an ATOM set */
			if (NILP(type)) {
				type = QATOM;
			}
			*size_ret = XVECTOR_LENGTH(obj);
			*format_ret = 32;
			*data_ret = (unsigned char *)xmalloc_atomic(
				(*size_ret) * sizeof(Atom));
			for (i = 0; i < (int)(*size_ret); i++) {
				if (SYMBOLP(XVECTOR_DATA(obj)[i])) {
					(*(Atom **) data_ret)[i] =
						symbol_to_x_atom(
							d,
							XVECTOR_DATA(obj)[i], 0);
				} else {
					/* was: Qselection_error */
					signal_error(
						Qerror,
						list2(build_string(tmp_err_1),
						      obj));
				}
			}

#if 0				/* #### MULTIPLE doesn't work yet */
		} else if (VECTORP(XVECTOR_DATA(obj)[0])) {
			/* This vector is an ATOM_PAIR set */
			if (NILP(type)) type = QATOM_PAIR;
			*size_ret = XVECTOR_LENGTH(obj);
			*format_ret = 32;
			*data_ret = (unsigned char *)
				xmalloc_atomic((*size_ret) * sizeof(Atom) * 2);
			for (i = 0; i < *size_ret; i++) {
				if (VECTORP(XVECTOR_DATA(obj)[i])) {
					Lisp_Object pair = XVECTOR_DATA(obj)[i];
					if (XVECTOR_LENGTH(pair) != 2) {
						signal_error(
							Qerror,
							list2(build_string(
								      tmp_err_2),
							      pair));
					}
					(*(Atom **) data_ret)[i * 2] =
						symbol_to_x_atom(
							d,
							XVECTOR_DATA(pair)[0], 0);
					(*(Atom **) data_ret)[(i * 2) + 1] =
						symbol_to_x_atom(
							d,
							XVECTOR_DATA(pair)[1], 0);
				} else {
					signal_error(Qerror,
						     list2(build_string(
								   tmp_err_3),
							   obj));
				}
			}
#endif
		} else {
			/* This vector is an INTEGER set or something like it */
			*size_ret = XVECTOR_LENGTH(obj);
			if (NILP(type)) {
				type = QINTEGER;
			}
			*format_ret = 16;
			for (i = 0; i < (int)(*size_ret); i++) {
				if (CONSP(XVECTOR_DATA(obj)[i])) {
					* format_ret = 32;
				} else {
					if (!INTP(XVECTOR_DATA(obj)[i])) {
						/* was: Qselection_error */
						signal_error(
							Qerror,
							list2(build_string(
								      tmp_err_4),
							      obj));
					}
				}
			}
			*data_ret = xmalloc_atomic(*size_ret *
						   (*format_ret / 8));
			for (i = 0; i < (int)(*size_ret); i++)
				if (*format_ret == 32)
					(*((unsigned long **)data_ret))[i] =
						lisp_to_word(XVECTOR_DATA(obj)[i]);
				else
					(*((unsigned short **)data_ret))[i] =
						(unsigned short) lisp_to_word(XVECTOR_DATA(obj)[i]);
		}
	} else {
		signal_error(Qerror,	/* Qselection_error */
			     list2(build_string("unrecognized selection data"),
				   obj));
	}
	*type_ret = symbol_to_x_atom(d, type, 0);
}


/* Called from the event loop to handle SelectionNotify events.
   I don't think this needs to be reentrant.
*/
void
x_handle_selection_notify(XSelectionEvent * event)
{
	if (!reading_selection_reply)
		message("received an unexpected SelectionNotify event");
	else if (event->requestor != reading_selection_reply)
		message("received a SelectionNotify event for the wrong window");
	else if (event->selection != reading_which_selection)
		message("received the wrong selection type in SelectionNotify!");
	else
		reading_selection_reply = 0;	/* we're done now. */
}

static void
x_disown_selection(Lisp_Object selection, Lisp_Object timeval)
{
	struct device *d = decode_x_device(Qnil);
	Display * display = DEVICE_X_DISPLAY(d);
	Time timestamp;
	Atom selection_atom;

	CHECK_SYMBOL(selection);
	if (NILP(timeval))
		timestamp = DEVICE_X_MOUSE_TIMESTAMP(d);
	else {
		/* #### This is bogus.  See the comment above about problems
		   on OSF/1 and DEC Alphas.  Yet another reason why it sucks
		   to have the implementation (i.e. cons of two 16-bit
		   integers) exposed. */
		time_t the_time;
		lisp_to_time(timeval, &the_time);
		timestamp = (Time) the_time;
	}

	selection_atom = symbol_to_x_atom(d, selection, 0);
	XSetSelectionOwner(display, selection_atom, None, timestamp);
}

static Lisp_Object
x_selection_exists_p(Lisp_Object selection, Lisp_Object selection_type)
{
	struct device *d = decode_x_device(Qnil);
	Display * dpy = DEVICE_X_DISPLAY(d);
	return XGetSelectionOwner(dpy, symbol_to_x_atom(d, selection, 0)) != None ? Qt : Qnil;
}


#ifdef CUT_BUFFER_SUPPORT

static int cut_buffers_initialized;	/* Whether we're sure they all exist */
/* Ensure that all 8 cut buffers exist.  ICCCM says we gotta... */
static void
initialize_cut_buffers(Display * display, Window window)
{
	static unsigned const char *const data = (unsigned const char *)"";
#define FROB(atom) XChangeProperty (display, window, atom, XA_STRING, 8, \
				    PropModeAppend, data, 0)
	FROB(XA_CUT_BUFFER0);
	FROB(XA_CUT_BUFFER1);
	FROB(XA_CUT_BUFFER2);
	FROB(XA_CUT_BUFFER3);
	FROB(XA_CUT_BUFFER4);
	FROB(XA_CUT_BUFFER5);
	FROB(XA_CUT_BUFFER6);
	FROB(XA_CUT_BUFFER7);
#undef FROB
	cut_buffers_initialized = 1;
}

#define CHECK_CUTBUFFER(symbol) do {                                    \
	CHECK_SYMBOL (symbol);                                                  \
	if (! (EQ (symbol, QCUT_BUFFER0) ||                                     \
	       EQ (symbol, QCUT_BUFFER1) ||                                     \
	       EQ (symbol, QCUT_BUFFER2) ||                                     \
	       EQ (symbol, QCUT_BUFFER3) ||                                     \
	       EQ (symbol, QCUT_BUFFER4) ||                                     \
	       EQ (symbol, QCUT_BUFFER5) ||                                     \
	       EQ (symbol, QCUT_BUFFER6) ||                                     \
	       EQ (symbol, QCUT_BUFFER7)))                                      \
		signal_simple_error ("Doesn't name a cutbuffer", symbol);       \
} while (0)

DEFUN("x-get-cutbuffer-internal", Fx_get_cutbuffer_internal, 1, 1, 0, /*
Return the value of the named CUTBUFFER (typically CUT_BUFFER0).
*/
      (cutbuffer))
{
	struct device *d = decode_x_device(Qnil);
	Display * display = DEVICE_X_DISPLAY(d);
	Window window = RootWindow(display, 0); /* Cutbuffers are on frame 0 */
	Atom cut_buffer_atom;
	Extbyte * data;
	int bytes;
	Atom type;
	int format;
	unsigned long size;
	Lisp_Object ret;

	CHECK_CUTBUFFER(cutbuffer);
	cut_buffer_atom = symbol_to_x_atom(d, cutbuffer, 0);
	x_get_window_property(display, window, cut_buffer_atom, &data,
			      &bytes, &type, &format, &size, 0);
	if (!data)
		return Qnil;
	if (format != 8 || type != XA_STRING)
		signal_simple_error_2("Cut buffer doesn't contain 8-bit STRING data",
				      x_atom_to_symbol(d, type), make_int(format));
	/* We cheat - if the string contains an ESC character, that's
	   technically not allowed in a STRING, so we assume it's
	   COMPOUND_TEXT that we stored there ourselves earlier,
	   in x-store-cutbuffer-internal  */
	ret = (bytes ? make_ext_string(data, bytes, memchr(data, 0x1b, bytes) ? Qctext : Qbinary) : Qnil);
	xfree(data);
	return ret;
}

DEFUN("x-store-cutbuffer-internal", Fx_store_cutbuffer_internal, 2, 2, 0,	/*
Set the value of the named CUTBUFFER (typically CUT_BUFFER0) to STRING.
*/
      (cutbuffer, string))
{
	struct device *d = decode_x_device(Qnil);
	Display * display = DEVICE_X_DISPLAY(d);
	Window window = RootWindow(display, 0);	/* Cutbuffers are on frame 0 */
	Atom cut_buffer_atom;
	const Bufbyte * data = XSTRING_DATA(string);
	Bytecount bytes = XSTRING_LENGTH(string);
	Bytecount bytes_remaining;
	int max_bytes = SELECTION_QUANTUM(display);
#ifdef MULE
	const Bufbyte * ptr, *end;
	enum { ASCII, LATIN_1, WORLD } chartypes = ASCII;
#endif
	if (max_bytes > MAX_SELECTION_QUANTUM)
		max_bytes = MAX_SELECTION_QUANTUM;
	CHECK_CUTBUFFER(cutbuffer);
	CHECK_STRING(string);
	cut_buffer_atom =
		symbol_to_x_atom(d, cutbuffer, 0);
	if (!cut_buffers_initialized)
		initialize_cut_buffers(display, window);
	/* We use the STRING encoding (Latin-1 only) if we can, else COMPOUND_TEXT.
	   We cheat and use type = `STRING' even when using COMPOUND_TEXT.
	   The ICCCM requires that this be so, and other clients assume it,
	   as we do ourselves in initialize_cut_buffers.  */
#ifdef MULE
	/* Optimize for the common ASCII case */
	for (ptr = data, end = ptr + bytes; ptr <= end;) {
		if (BYTE_ASCII_P(*ptr)) {
			ptr++;
			continue;
		}

		if ((*ptr) == LEADING_BYTE_LATIN_ISO8859_1
		    || (*ptr) == LEADING_BYTE_CONTROL_1) {
			chartypes = LATIN_1;
			ptr += 2;
			continue;
		}

		chartypes = WORLD;
		break;
	}

	if (chartypes == LATIN_1)
		TO_EXTERNAL_FORMAT(LISP_STRING, string, ALLOCA, (data, bytes), Qbinary);
	else if (chartypes == WORLD)
		TO_EXTERNAL_FORMAT(LISP_STRING, string, ALLOCA, (data, bytes), Qctext);
#endif				/* MULE */
	bytes_remaining = bytes;
	while (bytes_remaining) {
		int chunk = bytes_remaining < max_bytes ? bytes_remaining : max_bytes;
		XChangeProperty(display, window, cut_buffer_atom, XA_STRING, 8,
				(bytes_remaining == bytes ? PropModeReplace : PropModeAppend),
				data, chunk);
		data += chunk;
		bytes_remaining -= chunk;
	}
	return string;
}

DEFUN("x-rotate-cutbuffers-internal", Fx_rotate_cutbuffers_internal, 1, 1, 0,	/*
Rotate the values of the cutbuffers by the given number of steps;
positive means move values forward, negative means backward.
*/
      (n))
{
	struct device *d = decode_x_device(Qnil);
	Display * display = DEVICE_X_DISPLAY(d);
	Window window = RootWindow(display, 0);	/* Cutbuffers are on frame 0 */
	Atom props[8];

	CHECK_INT(n);
	if (XINT(n) == 0)
		return n;
	if (!cut_buffers_initialized)
		initialize_cut_buffers(display, window);
	props[0] = XA_CUT_BUFFER0;
	props[1] = XA_CUT_BUFFER1;
	props[2] = XA_CUT_BUFFER2;
	props[3] = XA_CUT_BUFFER3;
	props[4] = XA_CUT_BUFFER4;
	props[5] = XA_CUT_BUFFER5;
	props[6] = XA_CUT_BUFFER6;
	props[7] = XA_CUT_BUFFER7;
	XRotateWindowProperties(display, window, props, 8, XINT(n));
	return n;
}
#endif				/* CUT_BUFFER_SUPPORT */


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_select_x(void)
{
#ifdef CUT_BUFFER_SUPPORT
	DEFSUBR(Fx_get_cutbuffer_internal);
	DEFSUBR(Fx_store_cutbuffer_internal);
	DEFSUBR(Fx_rotate_cutbuffers_internal);
#endif				/* CUT_BUFFER_SUPPORT */
	/* Unfortunately, timeout handlers must be lisp functions. */
	defsymbol (&Qx_selection_reply_timeout_internal,
		   "x-selection-reply-timeout-internal");
	DEFSUBR(Fx_selection_reply_timeout_internal);
#ifdef CUT_BUFFER_SUPPORT
	defsymbol(&QCUT_BUFFER0, "CUT_BUFFER0");
	defsymbol(&QCUT_BUFFER1, "CUT_BUFFER1");
	defsymbol(&QCUT_BUFFER2, "CUT_BUFFER2");
	defsymbol(&QCUT_BUFFER3, "CUT_BUFFER3");
	defsymbol(&QCUT_BUFFER4, "CUT_BUFFER4");
	defsymbol(&QCUT_BUFFER5, "CUT_BUFFER5");
	defsymbol(&QCUT_BUFFER6, "CUT_BUFFER6");
	defsymbol(&QCUT_BUFFER7, "CUT_BUFFER7");
#endif				/* CUT_BUFFER_SUPPORT */
}

void
console_type_create_select_x(void)
{
	CONSOLE_HAS_METHOD(x, own_selection);
	CONSOLE_HAS_METHOD(x, disown_selection);
	CONSOLE_HAS_METHOD(x, get_foreign_selection);
	CONSOLE_HAS_METHOD(x, selection_exists_p);
}

void
reinit_vars_of_select_x(void)
{
	reading_selection_reply = 0;
	reading_which_selection = 0;
	selection_reply_timed_out = 0;
	for_whom_the_bell_tolls = 0;
	prop_location_tick = 0;
}

void
vars_of_select_x(void)
{
	reinit_vars_of_select_x();
#ifdef CUT_BUFFER_SUPPORT
	cut_buffers_initialized = 0;
	Fprovide(intern("cut-buffer"));
#endif
	DEFVAR_LISP("x-sent-selection-hooks", &Vx_sent_selection_hooks	/*
A function or functions to be called after we have responded to some
other client's request for the value of a selection that we own.  The
function(s) will be called with four arguments:
  - the name of the selection (typically PRIMARY, SECONDARY, or CLIPBOARD);
  - the name of the selection-type which we were requested to convert the
    selection into before sending (for example, STRING or LENGTH);
  - and whether we successfully transmitted the selection.
We might have failed (and declined the request) for any number of reasons,
including being asked for a selection that we no longer own, or being asked
to convert into a type that we don't know about or that is inappropriate.
This hook doesn't let you change the behavior of emacs's selection replies,
it merely informs you that they have happened.
									*/ );
	Vx_sent_selection_hooks = Qunbound;

	DEFVAR_INT("x-selection-timeout", &x_selection_timeout	/*
If the selection owner doesn't reply in this many seconds, we give up.
A value of 0 means wait as long as necessary.  This is initialized from the
\"*selectionTimeout\" resource (which is expressed in milliseconds).
								*/ );
	x_selection_timeout = 0;

	DEFVAR_BOOL("x-selection-strict-motif-ownership", &x_selection_strict_motif_ownership	/*
*If nil and SXEmacs already owns the clipboard, don't own it again in the
Motif way. Owning the selection on the Motif way does a huge amount of
X protocol, and it makes killing text incredibly slow when using an
X terminal.  However, when enabled Motif text fields don't bother to look up
the new value, and you can't Copy from a buffer, Paste into a text
field, then Copy something else from the buffer and paste it into the
text field; it pastes the first thing again.
												*/ );
	x_selection_strict_motif_ownership = 1;
}

void
Xatoms_of_select_x(struct device *d)
{
	Display * D = DEVICE_X_DISPLAY(d);
	/* Non-predefined atoms that we might end up using a lot */
	DEVICE_XATOM_CLIPBOARD(d) =
		XInternAtom(D, "CLIPBOARD", False);
	DEVICE_XATOM_TIMESTAMP(d) =
		XInternAtom(D, "TIMESTAMP", False);
	DEVICE_XATOM_TEXT(d) =
		XInternAtom(D, "TEXT", False);
	DEVICE_XATOM_DELETE(d) =
		XInternAtom(D, "DELETE", False);
	DEVICE_XATOM_MULTIPLE(d) =
		XInternAtom(D, "MULTIPLE", False);
	DEVICE_XATOM_INCR(d) =
		XInternAtom(D, "INCR", False);
	DEVICE_XATOM_TARGETS(d) =
		XInternAtom(D, "TARGETS", False);
	DEVICE_XATOM_NULL(d) =
		XInternAtom(D, "NULL", False);
	DEVICE_XATOM_ATOM_PAIR(d) =
		XInternAtom(D, "ATOM_PAIR", False);
	DEVICE_XATOM_COMPOUND_TEXT(d) =
		XInternAtom(D, "COMPOUND_TEXT", False);
	/* #### I don't like the looks of this... what is it for? - ajh */
	DEVICE_XATOM_EMACS_TMP(d) =
		XInternAtom(D, "_EMACS_TMP_", False);
}
