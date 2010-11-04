/* Stream device functions.
   Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 1996 Ben Wing.

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

/* Written by Ben Wing. */

#include <config.h>
#include "lisp.h"

#include "console-stream.h"
#include "events/events.h"
#include "frame.h"
#include "redisplay.h"
#include "sysdep.h"
#include "sysfile.h"
#include "window.h"

DEFINE_CONSOLE_TYPE(stream);

Lisp_Object Vterminal_console;
Lisp_Object Vterminal_device;
Lisp_Object Vterminal_frame;

Lisp_Object Vstdio_str;

static void stream_init_console(struct console *con, Lisp_Object params)
{
	Lisp_Object tty = CONSOLE_CONNECTION(con);
	struct stream_console *stream_con;

	if (CONSOLE_STREAM_DATA(con) == NULL) {
		CONSOLE_DATA(con) = xnew(struct stream_console);
	}
	stream_con = CONSOLE_STREAM_DATA(con);

	stream_con->needs_newline = 0;

	/* Open the specified console */
	if (NILP(tty) || internal_equal(tty, Vstdio_str, 0)) {
		stream_con->in = stdin;
		stream_con->out = stdout;
		stream_con->err = stderr;
	} else {
		CHECK_STRING(tty);
		stream_con->in = stream_con->out = stream_con->err =
		    /* #### We don't currently do coding-system translation on
		       this descriptor. */
		    fopen((char *)XSTRING_DATA(tty), READ_PLUS_TEXT);
		if (!stream_con->in)
			error("Unable to open tty %s", XSTRING_DATA(tty));
	}
}

static void stream_init_device(struct device *d, Lisp_Object params)
{
	struct console *con = XCONSOLE(DEVICE_CONSOLE(d));

	DEVICE_INFD(d) = fileno(CONSOLE_STREAM_DATA(con)->in);
	DEVICE_OUTFD(d) = fileno(CONSOLE_STREAM_DATA(con)->out);
	init_baud_rate(d);
	init_one_device(d);
}

static int stream_initially_selected_for_input(struct console *con)
{
	return noninteractive && initialized;
}

extern int stdout_needs_newline;

static void stream_delete_console(struct console *con)
{
	struct stream_console *stream_con = CONSOLE_STREAM_DATA(con);
	if (stream_con) {
		if (		/* stream_con->needs_newline */
			   stdout_needs_newline) {	/* #### clean this up */
			fputc('\n', stream_con->out);
			fflush(stream_con->out);
		}
		if (stream_con->in != stdin)
			fclose(stream_con->in);

		xfree(stream_con);
		CONSOLE_DATA(con) = NULL;
	}
}

Lisp_Object
stream_semi_canonicalize_console_connection(Lisp_Object connection,
					    Error_behavior errb)
{
	return NILP(connection) ? Vstdio_str : connection;
}

Lisp_Object
stream_canonicalize_console_connection(Lisp_Object connection,
				       Error_behavior errb)
{
	if (NILP(connection) || internal_equal(connection, Vstdio_str, 0))
		return Vstdio_str;

	if (!ERRB_EQ(errb, ERROR_ME)) {
		if (!STRINGP(connection))
			return Qunbound;
	} else
		CHECK_STRING(connection);

	return Ffile_truename(connection, Qnil);
}

Lisp_Object
stream_semi_canonicalize_device_connection(Lisp_Object connection,
					   Error_behavior errb)
{
	return stream_semi_canonicalize_console_connection(connection, errb);
}

Lisp_Object
stream_canonicalize_device_connection(Lisp_Object connection,
				      Error_behavior errb)
{
	return stream_canonicalize_console_connection(connection, errb);
}

static void stream_init_frame_1(struct frame *f, Lisp_Object props)
{
#if 0
	struct device *d = XDEVICE(FRAME_DEVICE(f));
	if (!NILP(DEVICE_FRAME_LIST(d)))
		error("Only one frame allowed on stream devices");
#endif
	f->name = build_string("stream");
	f->height = 80;
	f->width = 24;
	f->visible = 0;		/* so redisplay doesn't try to do anything */
}

static int
stream_text_width(struct frame *f, struct face_cachel *cachel,
		  const Emchar * str, Charcount len)
{
	return len;
}

static int stream_left_margin_width(struct window *w)
{
	return 0;
}

static int stream_right_margin_width(struct window *w)
{
	return 0;
}

static int stream_divider_height(void)
{
	return 1;
}

static int stream_eol_cursor_width(void)
{
	return 1;
}

static void
stream_output_display_block(struct window *w, struct display_line *dl,
			    int block, int start, int end,
			    int start_pixpos, int cursor_start,
			    int cursor_width, int cursor_height)
{
}

static void
stream_clear_region(Lisp_Object window, struct device *d, struct frame *f,
		    face_index findex, int x, int y,
		    int width, int height, Lisp_Object fcolor,
		    Lisp_Object bcolor, Lisp_Object background_pixmap)
{
}

static int stream_flash(struct device *d)
{
	return 0;		/* sorry can't do it */
}

static void
stream_ring_bell(struct device *d, int volume, int pitch, int duration)
{
	struct console *c = XCONSOLE(DEVICE_CONSOLE(d));
	fputc(07, CONSOLE_STREAM_DATA(c)->out);
	fflush(CONSOLE_STREAM_DATA(c)->out);
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void console_type_create_stream(void)
{
	INITIALIZE_CONSOLE_TYPE(stream, "stream", "console-stream-p");

	/* console methods */
	CONSOLE_HAS_METHOD(stream, init_console);
	CONSOLE_HAS_METHOD(stream, initially_selected_for_input);
	CONSOLE_HAS_METHOD(stream, delete_console);
	CONSOLE_HAS_METHOD(stream, canonicalize_console_connection);
	CONSOLE_HAS_METHOD(stream, canonicalize_device_connection);
	CONSOLE_HAS_METHOD(stream, semi_canonicalize_console_connection);
	CONSOLE_HAS_METHOD(stream, semi_canonicalize_device_connection);

	/* device methods */
	CONSOLE_HAS_METHOD(stream, init_device);

	/* frame methods */
	CONSOLE_HAS_METHOD(stream, init_frame_1);

	/* redisplay methods */
	CONSOLE_HAS_METHOD(stream, left_margin_width);
	CONSOLE_HAS_METHOD(stream, right_margin_width);
	CONSOLE_HAS_METHOD(stream, text_width);
	CONSOLE_HAS_METHOD(stream, output_display_block);
	CONSOLE_HAS_METHOD(stream, divider_height);
	CONSOLE_HAS_METHOD(stream, eol_cursor_width);
	CONSOLE_HAS_METHOD(stream, clear_region);
	CONSOLE_HAS_METHOD(stream, flash);
	CONSOLE_HAS_METHOD(stream, ring_bell);
}

void reinit_console_type_create_stream(void)
{
	REINITIALIZE_CONSOLE_TYPE(stream);
}

void vars_of_console_stream(void)
{
	DEFVAR_LISP("terminal-console", &Vterminal_console	/*
The initial console object, which represents SXEmacs' stdout.
								 */ );
	Vterminal_console = Qnil;

	DEFVAR_LISP("terminal-device", &Vterminal_device	/*
The initial device object, which represents SXEmacs' stdout.
								 */ );
	Vterminal_device = Qnil;

	DEFVAR_LISP("terminal-frame", &Vterminal_frame	/*
The initial frame object, which represents SXEmacs' stdout.
							 */ );
	Vterminal_frame = Qnil;

	/* Moved from console-tty.c */
	Vstdio_str = build_string("stdio");
	staticpro(&Vstdio_str);
}

#ifndef PDUMP
void init_console_stream(int reinit)
{
	/* This function can GC */
	if (!initialized) {
		Vterminal_device = Fmake_device(Qstream, Qnil, Qnil);
		Vterminal_console = Fdevice_console(Vterminal_device);
		Vterminal_frame = Fmake_frame(Qnil, Vterminal_device);
		minibuf_window = XFRAME(Vterminal_frame)->minibuffer_window;
	} else {
		/* Re-initialize the FILE fields of the console. */
		stream_init_console(XCONSOLE(Vterminal_console), Qnil);
		if (noninteractive)
			event_stream_select_console(XCONSOLE
						    (Vterminal_console));
	}
}

#else

void init_console_stream(int reinit)
{
	/* This function can GC */
	if (!reinit) {
		Vterminal_device = Fmake_device(Qstream, Qnil, Qnil);
		Vterminal_console = Fdevice_console(Vterminal_device);
		Vterminal_frame = Fmake_frame(Qnil, Vterminal_device);
		minibuf_window = XFRAME(Vterminal_frame)->minibuffer_window;
	}
	if (initialized) {
		stream_init_console(XCONSOLE(Vterminal_console), Qnil);
		if (noninteractive)
			event_stream_select_console(XCONSOLE
						    (Vterminal_console));
	}
}
#endif
