/* TTY console functions.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.
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

/* Authors: Ben Wing and Chuck Thompson. */

#include <config.h>
#include "lisp.h"

#include "console-tty.h"
#include "ui/console-stream.h"
#include "ui/faces.h"
#include "ui/frame.h"
#include "lstream.h"
#include "ui/glyphs.h"
#include "sysdep.h"
#include "sysfile.h"
#ifdef FILE_CODING
#include "mule/file-coding.h"
#endif

DEFINE_CONSOLE_TYPE(tty);
DECLARE_IMAGE_INSTANTIATOR_FORMAT(nothing);
DECLARE_IMAGE_INSTANTIATOR_FORMAT(string);
DECLARE_IMAGE_INSTANTIATOR_FORMAT(formatted_string);
DECLARE_IMAGE_INSTANTIATOR_FORMAT(inherit);

Lisp_Object Qterminal_type;
Lisp_Object Qcontrolling_process;

/*
 * Allocate and initialize the console object
 */
static void allocate_tty_console_struct(struct console *con)
{
	/* zero out all slots except the lisp ones ... */
	CONSOLE_DATA(con) = xnew_and_zero(struct tty_console);
	CONSOLE_TTY_DATA(con)->terminal_type = Qnil;
	CONSOLE_TTY_DATA(con)->instream = Qnil;
	CONSOLE_TTY_DATA(con)->outstream = Qnil;
}

/*
 * Initialize the console. Determine the console type and the proper
 * control sequences to send.
 */
static void tty_init_console(struct console *con, Lisp_Object props)
{
	Lisp_Object tty = CONSOLE_CONNECTION(con);
	Lisp_Object terminal_type = Qnil;
	Lisp_Object controlling_process = Qnil;
	struct tty_console *tty_con;
	struct gcpro gcpro1, gcpro2;

	GCPRO2(terminal_type, controlling_process);

	terminal_type = Fplist_get(props, Qterminal_type, Qnil);
	controlling_process = Fplist_get(props, Qcontrolling_process, Qnil);

	/* Determine the terminal type */

	if (!NILP(terminal_type))
		CHECK_STRING(terminal_type);
	else {
		char *temp_type = getenv("TERM");

		if (!temp_type) {
			error("Cannot determine terminal type");
		} else
			terminal_type = build_string(temp_type);
	}

	/* Determine the controlling process */
	if (!NILP(controlling_process))
		CHECK_INT(controlling_process);

	/* Open the specified console */

	allocate_tty_console_struct(con);
	tty_con = CONSOLE_TTY_DATA(con);

	if (internal_equal(tty, Vstdio_str, 0)) {
		tty_con->infd = fileno(stdin);
		tty_con->outfd = fileno(stdout);
		tty_con->is_stdio = 1;
	} else {
		tty_con->infd = tty_con->outfd =
		    open((char *)XSTRING_DATA(tty), O_RDWR);
		if (tty_con->infd < 0)
			error("Unable to open tty %s", XSTRING_DATA(tty));
		tty_con->is_stdio = 0;
	}

	tty_con->instream = make_filedesc_input_stream(tty_con->infd, 0, -1, 0);
	tty_con->outstream =
	    make_filedesc_output_stream(tty_con->outfd, 0, -1, 0);
#ifdef FILE_CODING
	tty_con->instream =
	    make_decoding_input_stream(XLSTREAM(tty_con->instream),
				       Fget_coding_system(Qkeyboard));
	Lstream_set_character_mode(XLSTREAM(tty_con->instream));
	tty_con->outstream =
	    make_encoding_output_stream(XLSTREAM(tty_con->outstream),
					Fget_coding_system(Qterminal));
#endif				/* FILE_CODING */
	tty_con->terminal_type = terminal_type;
	tty_con->controlling_process = controlling_process;

	if (NILP(CONSOLE_NAME(con)))
		CONSOLE_NAME(con) = Ffile_name_nondirectory(tty);
	{
		pid_t tty_pg;
		pid_t controlling_tty_pg;
		int cfd;

		/* OK, the only sure-fire way I can think of to determine
		   whether a particular TTY is our controlling TTY is to check
		   if it has the same foreground process group as our controlling
		   TTY.  This is OK because a process group can never simultaneously
		   be the foreground process group of two TTY's (in that case it
		   would have two controlling TTY's, which is not allowed). */

		EMACS_GET_TTY_PROCESS_GROUP(tty_con->infd, &tty_pg);
		cfd = open("/dev/tty", O_RDWR, 0);
		tty_con->controlling_terminal = 0;
		if (cfd >= 0 ) {
			EMACS_GET_TTY_PROCESS_GROUP(cfd, &controlling_tty_pg);
			close(cfd);
			if (tty_pg == controlling_tty_pg) {
				tty_con->controlling_terminal = 1;
				XSETCONSOLE(Vcontrolling_terminal, con);
				munge_tty_process_group();
			}
		}
	}

	UNGCPRO;
}

static void tty_mark_console(struct console *con)
{
	struct tty_console *tty_con = CONSOLE_TTY_DATA(con);
	mark_object(tty_con->terminal_type);
	mark_object(tty_con->instream);
	mark_object(tty_con->outstream);
}

static int tty_initially_selected_for_input(struct console *con)
{
	return 1;
}

static void free_tty_console_struct(struct console *con)
{
	struct tty_console *tty_con = CONSOLE_TTY_DATA(con);
	if (tty_con) {
		if (tty_con->term_entry_buffer) {	/* allocated in term_init () */
			xfree(tty_con->term_entry_buffer);
			tty_con->term_entry_buffer = NULL;
		}
		xfree(tty_con);
		CONSOLE_DATA(con) = NULL;
	}
}

static void tty_delete_console(struct console *con)
{
	Lstream_close(XLSTREAM(CONSOLE_TTY_DATA(con)->instream));
	Lstream_close(XLSTREAM(CONSOLE_TTY_DATA(con)->outstream));
	if (!CONSOLE_TTY_DATA(con)->is_stdio)
		close(CONSOLE_TTY_DATA(con)->infd);
	if (CONSOLE_TTY_DATA(con)->controlling_terminal) {
		Vcontrolling_terminal = Qnil;
		unmunge_tty_process_group();
	}
	free_tty_console_struct(con);
}

static struct console *decode_tty_console(Lisp_Object console)
{
	XSETCONSOLE(console, decode_console(console));
	CHECK_TTY_CONSOLE(console);
	return XCONSOLE(console);
}

DEFUN("console-tty-terminal-type", Fconsole_tty_terminal_type, 0, 1, 0,	/*
Return the terminal type of TTY console CONSOLE.
*/
      (console))
{
	return CONSOLE_TTY_DATA(decode_tty_console(console))->terminal_type;
}

DEFUN("console-tty-max-colors", Fconsole_tty_max_colors, 0, 1, 0, /*
Return the maximum number of colors of TTY console CONSOLE.
*/
      (console))
{
	return make_int(CONSOLE_TTY_DATA(
				decode_tty_console(console))->maxcolors);
}

DEFUN("console-tty-controlling-process", Fconsole_tty_controlling_process, 0, 1, 0,	/*
Return the controlling process of tty console CONSOLE.
*/
      (console))
{
	return CONSOLE_TTY_DATA(decode_tty_console(console))->
	    controlling_process;
}

#ifdef FILE_CODING

DEFUN("console-tty-input-coding-system", Fconsole_tty_input_coding_system, 0, 1, 0,	/*
Return the input coding system of tty console CONSOLE.
*/
      (console))
{
	return decoding_stream_coding_system
	    (XLSTREAM(CONSOLE_TTY_DATA(decode_tty_console(console))->instream));
}

DEFUN("set-console-tty-input-coding-system", Fset_console_tty_input_coding_system, 0, 2, 0,	/*
Set the input coding system of tty console CONSOLE to CODESYS.
CONSOLE defaults to the selected console.
CODESYS defaults to the value of `keyboard-coding-system'.
*/
      (console, codesys))
{
	set_decoding_stream_coding_system
	    (XLSTREAM(CONSOLE_TTY_DATA(decode_tty_console(console))->instream),
	     Fget_coding_system(NILP(codesys) ? Qkeyboard : codesys));
	return Qnil;
}

DEFUN("console-tty-output-coding-system", Fconsole_tty_output_coding_system, 0, 1, 0,	/*
Return TTY CONSOLE's output coding system.
*/
      (console))
{
	return encoding_stream_coding_system
	    (XLSTREAM
	     (CONSOLE_TTY_DATA(decode_tty_console(console))->outstream));
}

DEFUN("set-console-tty-output-coding-system", Fset_console_tty_output_coding_system, 0, 2, 0,	/*
Set the coding system of tty output of console CONSOLE to CODESYS.
CONSOLE defaults to the selected console.
CODESYS defaults to the value of `terminal-coding-system'.
*/
      (console, codesys))
{
	set_encoding_stream_coding_system
	    (XLSTREAM(CONSOLE_TTY_DATA(decode_tty_console(console))->outstream),
	     Fget_coding_system(NILP(codesys) ? Qterminal : codesys));
	/* Redraw tty */
	face_property_was_changed(Vdefault_face, Qfont, Qtty);
	return Qnil;
}

/* #### Move this function to lisp */
DEFUN("set-console-tty-coding-system", Fset_console_tty_coding_system, 0, 2, 0,	/*
Set the input and output coding systems of tty console CONSOLE to CODESYS.
CONSOLE defaults to the selected console.
If CODESYS is nil, the values of `keyboard-coding-system' and
`terminal-coding-system' will be used for the input and
output coding systems of CONSOLE.
*/
      (console, codesys))
{
	Fset_console_tty_input_coding_system(console, codesys);
	Fset_console_tty_output_coding_system(console, codesys);
	return Qnil;
}
#endif				/* FILE_CODING */

Lisp_Object
tty_semi_canonicalize_console_connection(Lisp_Object connection,
					 Error_behavior errb)
{
	return stream_semi_canonicalize_console_connection(connection, errb);
}

Lisp_Object
tty_canonicalize_console_connection(Lisp_Object connection, Error_behavior errb)
{
	return stream_canonicalize_console_connection(connection, errb);
}

Lisp_Object
tty_semi_canonicalize_device_connection(Lisp_Object connection,
					Error_behavior errb)
{
	return stream_semi_canonicalize_console_connection(connection, errb);
}

Lisp_Object
tty_canonicalize_device_connection(Lisp_Object connection, Error_behavior errb)
{
	return stream_canonicalize_console_connection(connection, errb);
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void syms_of_console_tty(void)
{
	DEFSUBR(Fconsole_tty_terminal_type);
	DEFSUBR(Fconsole_tty_controlling_process);
	DEFSUBR(Fconsole_tty_max_colors);
	defsymbol(&Qterminal_type, "terminal-type");
	defsymbol(&Qcontrolling_process, "controlling-process");
#ifdef FILE_CODING
	DEFSUBR(Fconsole_tty_output_coding_system);
	DEFSUBR(Fset_console_tty_output_coding_system);
	DEFSUBR(Fconsole_tty_input_coding_system);
	DEFSUBR(Fset_console_tty_input_coding_system);
	DEFSUBR(Fset_console_tty_coding_system);
#endif				/* FILE_CODING */
}

void console_type_create_tty(void)
{
	INITIALIZE_CONSOLE_TYPE(tty, "tty", "console-tty-p");

	/* console methods */
	CONSOLE_HAS_METHOD(tty, init_console);
	CONSOLE_HAS_METHOD(tty, mark_console);
	CONSOLE_HAS_METHOD(tty, initially_selected_for_input);
	CONSOLE_HAS_METHOD(tty, delete_console);
	CONSOLE_HAS_METHOD(tty, canonicalize_console_connection);
	CONSOLE_HAS_METHOD(tty, canonicalize_device_connection);
	CONSOLE_HAS_METHOD(tty, semi_canonicalize_console_connection);
	CONSOLE_HAS_METHOD(tty, semi_canonicalize_device_connection);
}

void reinit_console_type_create_tty(void)
{
	REINITIALIZE_CONSOLE_TYPE(tty);
}

void image_instantiator_format_create_glyphs_tty(void)
{
	IIFORMAT_VALID_CONSOLE(tty, nothing);
	IIFORMAT_VALID_CONSOLE(tty, string);
	IIFORMAT_VALID_CONSOLE(tty, formatted_string);
	IIFORMAT_VALID_CONSOLE(tty, inherit);
}

void vars_of_console_tty(void)
{
	Fprovide(Qtty);
}
