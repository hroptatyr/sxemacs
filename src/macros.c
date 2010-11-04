/* Keyboard macros.
   Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.

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


/* Synched up with: FSF 19.30. */

/* A keyboard macro is a string of ASCII characters, or a vector of event
   objects.  Only key-press, mouse-press, mouse-release, and menu-selection
   events ever get into a keyboard macro.

   When interactively defining a keyboard macro, it will always be a vector
   of events; strings may be executed for backwards compatibility.
 */

#include <config.h>
#include "lisp.h"
#include "events/events.h"
#include "macros.h"
#include "commands.h"
#include "ui/console.h"
#include "buffer.h"
#include "ui/window.h"
#include "ui/frame.h"
#include "ui/keymap.h"

Lisp_Object Qexecute_kbd_macro;

/* The current macro and our position in it.  When executing nested kbd
   macros, previous values for these are wound through the execution stack
   with unwind-protect.
 */
Lisp_Object Vexecuting_macro;
int executing_macro_index;

DEFUN("start-kbd-macro", Fstart_kbd_macro, 1, 1, "P",	/*
Record subsequent keyboard and menu input, defining a keyboard macro.
The commands are recorded even as they are executed.
Use \\[end-kbd-macro] to finish recording and make the macro available.
Use \\[name-last-kbd-macro] to give it a permanent name.
Non-nil arg (prefix arg) means append to last macro defined;
This begins by re-executing that macro as if you typed it again.
*/
      (append))
{
	/* This function can GC */
	struct console *con = XCONSOLE(Vselected_console);
	if (!NILP(con->defining_kbd_macro))
		error("Already defining kbd macro");

	if (NILP(con->kbd_macro_builder))
		con->kbd_macro_builder = make_vector(30, Qnil);

	zmacs_region_stays = 1;	/* set this before calling Fexecute_kbd_macro()
				   so that functions there can override */
	MARK_MODELINE_CHANGED;
	if (NILP(append)) {
		con->kbd_macro_ptr = 0;
		con->kbd_macro_end = 0;
		message("Defining kbd macro...");
	} else {
		message("Appending to kbd macro...");
		con->kbd_macro_ptr = con->kbd_macro_end;
		Fexecute_kbd_macro(con->last_kbd_macro, make_int(1));
	}
	con->defining_kbd_macro = Qt;

	return Qnil;
}

DEFUN("end-kbd-macro", Fend_kbd_macro, 0, 1, "P",	/*
Finish defining a keyboard macro.
The definition was started by \\[start-kbd-macro].
The macro is now available for use via \\[call-last-kbd-macro],
or it can be given a name with \\[name-last-kbd-macro] and then invoked
under that name.

With numeric arg, repeat macro now that many times,
counting the definition just completed as the first repetition.
An argument of zero means repeat until error.
*/
      (arg))
{
	/* This function can GC */
	struct console *con = XCONSOLE(Vselected_console);
	int repeat;

	if (NILP(con->defining_kbd_macro))
		error("Not defining kbd macro");

	if (NILP(arg))
		repeat = -1;
	else
		repeat = XINT(Fprefix_numeric_value(arg));

	if (!NILP(con->defining_kbd_macro)) {
		int i;
		int size = con->kbd_macro_end;

		if (size < 0)
			size = 0;
		con->last_kbd_macro = make_vector(size, Qnil);
		for (i = 0; i < size; i++)
			XVECTOR_DATA(con->last_kbd_macro)[i] =
			    XVECTOR_DATA(con->kbd_macro_builder)[i];
		con->defining_kbd_macro = Qnil;
		MARK_MODELINE_CHANGED;
		message("Keyboard macro defined");
	}

	zmacs_region_stays = 1;	/* set this before calling Fexecute_kbd_macro()
				   so that functions there can override */
	if (repeat < 0)
		return Qnil;
	else if (repeat == 0)
		return Fexecute_kbd_macro(con->last_kbd_macro, Qzero);
	else
		return Fexecute_kbd_macro(con->last_kbd_macro,
					  make_int(repeat - 1));
}

/* #### Read the comment in modeline.el to see why this ugliness is
   needed.  #### Try to avoid it, somehow!  */
DEFUN("zap-last-kbd-macro-event", Fzap_last_kbd_macro_event, 0, 0, 0,	/*
Don't look at this lest you vomit or spontaneously combust.
*/
      ())
{
	struct console *con = XCONSOLE(Vselected_console);
	if (con->kbd_macro_end)
		--con->kbd_macro_end;
	return Qnil;
}

/* Store event into kbd macro being defined
 */
void store_kbd_macro_event(Lisp_Object event)
{
	struct console *con = event_console_or_selected(event);

	if (con->kbd_macro_ptr == XVECTOR_LENGTH(con->kbd_macro_builder)) {
		int i;
		int old_size = XVECTOR_LENGTH(con->kbd_macro_builder);
		int new_size = old_size * 2;
		Lisp_Object new = make_vector(new_size, Qnil);
		for (i = 0; i < old_size; i++)
			XVECTOR_DATA(new)[i] =
			    XVECTOR_DATA(con->kbd_macro_builder)[i];
		con->kbd_macro_builder = new;
	}
	XVECTOR_DATA(con->kbd_macro_builder)[con->kbd_macro_ptr++] =
	    Fcopy_event(event, Qnil);
}

/* Extract the next kbd-macro element into the given event.
   If we're done, throws to the catch in Fexecute_kbd_macro().
 */
void pop_kbd_macro_event(Lisp_Object event)
{
	if (NILP(Vexecuting_macro))
		abort();

	if (STRINGP(Vexecuting_macro) || VECTORP(Vexecuting_macro)) {
		if (executing_macro_index < XINT(Flength(Vexecuting_macro))) {
			nth_of_key_sequence_as_event(Vexecuting_macro,
						     executing_macro_index++,
						     event);
			return;
		}
	} else if (!EQ(Vexecuting_macro, Qt))	/* Some things replace the macro
						   with Qt to force an early exit. */
		error("junk in executing-macro");

	Fthrow(Qexecute_kbd_macro, Qt);
}

/* Declare that all chars stored so far in the kbd macro being defined
   really belong to it.  This is done in between editor commands. */

void finalize_kbd_macro_chars(struct console *con)
{
	con->kbd_macro_end = con->kbd_macro_ptr;
}

DEFUN("cancel-kbd-macro-events", Fcancel_kbd_macro_events, 0, 0, 0,	/*
Cancel the events added to a keyboard macro for this command.
*/
      ())
{
	struct console *con = XCONSOLE(Vselected_console);

	con->kbd_macro_ptr = con->kbd_macro_end;

	return Qnil;
}

DEFUN("call-last-kbd-macro", Fcall_last_kbd_macro, 0, 1, "p",	/*
Call the last keyboard macro that you defined with \\[start-kbd-macro].

A prefix argument serves as a repeat count.  Zero means repeat until error.

To make a macro permanent so you can call it even after
defining others, use \\[name-last-kbd-macro].
*/
      (prefix))
{
	/* This function can GC */
	struct console *con = XCONSOLE(Vselected_console);

	if (NILP(con->last_kbd_macro)) {
		error("No kbd macro has been defined");
		return Qnil;
	}
	Fexecute_kbd_macro(con->last_kbd_macro, prefix);
	if (!NILP(con->defining_kbd_macro)) {
		int i;
		Fcancel_kbd_macro_events();
		for (i = 0; i < XVECTOR_LENGTH(con->last_kbd_macro); i++)
			store_kbd_macro_event(XVECTOR_DATA(con->last_kbd_macro)[i]);
	}
	return Qnil;
}

/* Restore Vexecuting_macro and executing_macro_index - called when
   the unwind-protect in Fexecute_kbd_macro gets invoked.  */
static Lisp_Object pop_kbd_macro(Lisp_Object info)
{
	Vexecuting_macro = Fcar(info);
	executing_macro_index = XINT(Fcdr(info));
	return Qnil;
}

DEFUN("execute-kbd-macro", Fexecute_kbd_macro, 1, 2, 0,	/*
Execute MACRO as string of editor command characters.
If MACRO is a symbol, its function definition is used.
COUNT is a repeat count, or nil for once, or 0 for infinite loop.
*/
      (macro, count))
{
	/* This function can GC */
	Lisp_Object final;
	Lisp_Object tem;
	int speccount = specpdl_depth();
	int repeat = 1;
	struct gcpro gcpro1;
	struct console *con = XCONSOLE(Vselected_console);

	if (!NILP(count)) {
		count = Fprefix_numeric_value(count);
		repeat = XINT(count);
	}

	final = indirect_function(macro, 1);
	if (!STRINGP(final) && !VECTORP(final))
		error("Keyboard macros must be strings or vectors");

	tem = Fcons(Vexecuting_macro, make_int(executing_macro_index));
	record_unwind_protect(pop_kbd_macro, tem);

	GCPRO1(final);
	do {
		Vexecuting_macro = final;
		executing_macro_index = 0;
		con->prefix_arg = Qnil;
		internal_catch(Qexecute_kbd_macro, call_command_loop, Qnil, 0);
	}
	while (--repeat != 0
	       && (STRINGP(Vexecuting_macro) || VECTORP(Vexecuting_macro)));

	UNGCPRO;
	return unbind_to(speccount, Qnil);
}

void syms_of_macros(void)
{
	DEFSUBR(Fstart_kbd_macro);
	DEFSUBR(Fend_kbd_macro);
	DEFSUBR(Fzap_last_kbd_macro_event);
	DEFSUBR(Fcall_last_kbd_macro);
	DEFSUBR(Fexecute_kbd_macro);
	DEFSUBR(Fcancel_kbd_macro_events);
	defsymbol(&Qexecute_kbd_macro, "execute-kbd-macro");
}

void vars_of_macros(void)
{
	DEFVAR_LISP("executing-macro", &Vexecuting_macro	/*
Currently executing keyboard macro (a vector of events or string);
nil if none executing.
								 */ );

	DEFVAR_LISP("executing-kbd-macro", &Vexecuting_macro	/*
Currently executing keyboard macro (a vector of events or string);
nil if none executing.
								 */ );
}

void init_macros(void)
{
	Vexecuting_macro = Qnil;
}
