/* Call a Lisp function interactively.
   Copyright (C) 1985, 1986, 1992, 1993, 1994 Free Software Foundation, Inc.
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


/* Synched up with: FSF 19.30, Mule 2.0. */

/* Authorship:

   FSF: long ago.
   Mly or JWZ: various changes.
 */

#include <config.h>
#include "lisp.h"
#include "ent/ent.h"

#include "buffer.h"
#include "bytecode.h"
#include "commands.h"
#define INCLUDE_EVENTS_H_PRIVATE_SPHERE
#include "events/events.h"
#include "ui/insdel.h"
#include "ui/window.h"

extern Charcount num_input_chars;

Lisp_Object Vcurrent_prefix_arg;
Lisp_Object Qcall_interactively;
Lisp_Object Vcommand_history;

Lisp_Object Vcommand_debug_status, Qcommand_debug_status;
Lisp_Object Qenable_recursive_minibuffers;

#if 0				/* FSFmacs */
/* Non-nil means treat the mark as active
   even if mark_active is 0.  */
Lisp_Object Vmark_even_if_inactive;
#endif

#if 0				/* ill-conceived */
/* FSF calls Qmouse_leave_buffer_hook at all sorts of random places,
   including a bunch of places in their mouse.el.  If this is
   implemented, it has to be done cleanly. */
Lisp_Object Vmouse_leave_buffer_hook, Qmouse_leave_buffer_hook;
#endif

Lisp_Object QletX, Qsave_excursion;

Lisp_Object Qread_from_minibuffer;
Lisp_Object Qread_file_name;
Lisp_Object Qread_directory_name;
Lisp_Object Qcompleting_read;
Lisp_Object Qread_buffer;
Lisp_Object Qread_function;
Lisp_Object Qread_variable;
Lisp_Object Qread_expression;
Lisp_Object Qread_command;
Lisp_Object Qread_number;
Lisp_Object Qread_string;
Lisp_Object Qevents_to_keys;

#if defined(MULE) || defined(FILE_CODING)
Lisp_Object Qread_coding_system;
Lisp_Object Qread_non_nil_coding_system;
#endif

/* ARGSUSED */
DEFUN("interactive", Finteractive, 0, UNEVALLED, 0,	/*
Specify a way of parsing arguments for interactive use of a function.
For example, write
(defun foo (arg) "Doc string" (interactive "p") ...use arg...)
to make ARG be the prefix argument when `foo' is called as a command.
The "call" to `interactive' is actually a declaration rather than a function;
it tells `call-interactively' how to read arguments
to pass to the function.
When actually called, `interactive' just returns nil.

The argument of `interactive' is usually a string containing a code letter
followed by a prompt.  (Some code letters do not use I/O to get
the argument and do not need prompts.)  To prompt for multiple arguments,
give a code letter, its prompt, a newline, and another code letter, etc.
Prompts are passed to format, and may use % escapes to print the
arguments that have already been read.
If the argument is not a string, it is evaluated to get a list of
arguments to pass to the function.
Just `(interactive)' means pass no args when calling interactively.

Code letters available are:
a -- Function name: symbol with a function definition.
b -- Name of existing buffer.
B -- Name of buffer, possibly nonexistent.
c -- Character.
C -- Command name: symbol with interactive function definition.
d -- Value of point as number.  Does not do I/O.
D -- Directory name.
e -- Last mouse-button or misc-user event that invoked this command.
If used more than once, the Nth `e' returns the Nth such event.
Does not do I/O.
f -- Existing file name.
F -- Possibly nonexistent file name.
i -- Always nil, ignore.  Use to skip arguments when interactive.
k -- Key sequence (a vector of events).
K -- Key sequence to be redefined (do not automatically down-case).
m -- Value of mark as number.  Does not do I/O.
n -- Number read using minibuffer.
N -- Prefix arg converted to number, or if none, do like code `n'.
p -- Prefix arg converted to number.  Does not do I/O.
P -- Prefix arg in raw form.  Does not do I/O.
r -- Region: point and mark as 2 numeric args, smallest first.  Does no I/O.
s -- Any string.
S -- Any symbol.
v -- Variable name: symbol that is user-variable-p.
x -- Lisp expression read but not evaluated.
X -- Lisp expression read and evaluated.
z -- Coding system. (Always nil if no Mule support.)
Z -- Coding system, nil if no prefix arg. (Always nil if no Mule support.)
In addition, if the string begins with `*'
then an error is signaled if the buffer is read-only.
This happens before reading any arguments.
If the string begins with `@', then the window the mouse is over is selected
before anything else is done.
If the string begins with `_', then this command will not cause the region
to be deactivated when it completes; that is, `zmacs-region-stays' will be
set to t when the command exits successfully.
You may use any of `@', `*' and `_' at the beginning of the string;
they are processed in the order that they appear.
*/
      (args))
{
	return Qnil;
}

/* Originally, this was just a function -- but `custom' used a
   garden-variety version, so why not make it a subr?  */
/* #### Move it to another file! */
DEFUN("quote-maybe", Fquote_maybe, 1, 1, 0,	/*
Quote EXPR if it is not self quoting.
*/
      (expr))
{
	return ((NILP(expr)
		 || EQ(expr, Qt)
		 || INTP(expr)
		 || FLOATP(expr)
		 || CHARP(expr)
		 || STRINGP(expr)
		 || VECTORP(expr)
		 || KEYWORDP(expr)
		 || BIT_VECTORP(expr)
		 || (CONSP(expr) && EQ(XCAR(expr), Qlambda)))
		? expr : list2(Qquote, expr));
}

/* Modify EXPR by quotifying each element (except the first).  */
static Lisp_Object quotify_args(Lisp_Object expr)
{
	Lisp_Object tail;
	Lisp_Cons *ptr;
	for (tail = expr; CONSP(tail); tail = ptr->cdr) {
		ptr = XCONS(tail);
		ptr->car = Fquote_maybe(ptr->car);
	}
	return expr;
}

static Bufpos check_mark(void)
{
	Lisp_Object tem;

	if (zmacs_regions && !zmacs_region_active_p)
		error("The region is not active now");

	tem = Fmarker_buffer(current_buffer->mark);
	if (NILP(tem) || (XBUFFER(tem) != current_buffer))
		error("The mark is not set now");

	return marker_position(current_buffer->mark);
}

static Lisp_Object
callint_prompt(const Bufbyte * prompt_start, Bytecount prompt_length,
	       const Lisp_Object *args, int nargs)
{
	Lisp_Object s = make_string(prompt_start, prompt_length);
	struct gcpro gcpro1;

	/* Fformat no longer smashes its arg vector, so no need to copy it. */

	if (!strchr((char*)XSTRING_DATA(s), '%')) {
		return s;
	}
	GCPRO1(s);
	RETURN_UNGCPRO(emacs_doprnt_string_lisp(0, s, 0, nargs, args));
}

/* `lambda' for RECORD-FLAG is an XEmacs addition. */

DEFUN("call-interactively", Fcall_interactively, 1, 3, 0,	/*
Call FUNCTION, reading args according to its interactive calling specs.
Return the value FUNCTION returns.
The function contains a specification of how to do the argument reading.
In the case of user-defined functions, this is specified by placing a call
to the function `interactive' at the top level of the function body.
See `interactive'.

If optional second arg RECORD-FLAG is the symbol `lambda', the interactive
calling arguments for FUNCTION are read and returned as a list,
but the function is not called on them.

If RECORD-FLAG is `t' then unconditionally put this command in the
command-history.  Otherwise, this is done only if an arg is read using
the minibuffer.

The argument KEYS specifies the value to use instead of (this-command-keys)
when reading the arguments.
*/
      (function, record_flag, keys))
{
	/* This function can GC */
	int speccount = specpdl_depth();
	Lisp_Object prefix;

	Lisp_Object fun;
	Lisp_Object specs = Qnil;
#ifdef IT_SEEMS_THAT_MLY_DOESNT_LIKE_THIS
	Lisp_Object enable;
#endif
	/* If SPECS is a string, we reset prompt_data to string_data
	 * (XSTRING (specs)) every time a GC might have occurred */
	const char *prompt_data = 0;
	int prompt_index = 0;
	int argcount;
	int set_zmacs_region_stays = 0;
	int mouse_event_count = 0;

	if (!NILP(keys)) {
		int i, len;

		CHECK_VECTOR(keys);
		len = XVECTOR_LENGTH(keys);
		for (i = 0; i < len; i++)
			CHECK_LIVE_EVENT(XVECTOR_DATA(keys)[i]);
	}

	/* Save this now, since use of minibuffer will clobber it. */
	prefix = Vcurrent_prefix_arg;

      retry:

#ifdef IT_SEEMS_THAT_MLY_DOESNT_LIKE_THIS
	/* Marginal kludge.  Use an evaluated interactive spec instead of this! */
	if (SYMBOLP(function))
		enable = Fget(function, Qenable_recursive_minibuffers, Qnil);
#endif

	fun = indirect_function(function, 1);

	/* Decode the kind of function.  Either handle it and return,
	   or go to `lose' if not interactive, or go to `retry'
	   to specify a different function, or set either PROMPT_DATA or SPECS. */

	if (SUBRP(fun)) {
		prompt_data = XSUBR(fun)->prompt;
		if (!prompt_data) {
		      lose:
			function = wrong_type_argument(Qcommandp, function);
			goto retry;
		}
#if 0 /* FSFmacs */		/* Huh? Where is this used? */
		if ((EMACS_INT) prompt_data == 1)
			/* Let SPECS (which is nil) be used as the args.  */
			prompt_data = 0;
#endif
	} else if (COMPILED_FUNCTIONP(fun)) {
		Lisp_Compiled_Function *f = XCOMPILED_FUNCTION(fun);
		if (!f->flags.interactivep)
			goto lose;
		specs = compiled_function_interactive(f);
	} else if (!CONSP(fun))
		goto lose;
	else {
		Lisp_Object funcar = Fcar(fun);

		if (EQ(funcar, Qautoload)) {
			struct gcpro gcpro1;
			GCPRO1(prefix);
			/* do_autoload GCPROs both arguments */
			do_autoload(fun, function);
			UNGCPRO;
			goto retry;
		} else if (EQ(funcar, Qlambda)) {
			specs = Fassq(Qinteractive, Fcdr(Fcdr(fun)));
			if (NILP(specs))
				goto lose;
			specs = Fcar(Fcdr(specs));
		} else
			goto lose;
	}

	/* FSFmacs makes an alloca() copy of prompt_data here.
	   We're more intelligent about this and just reset prompt_data
	   as necessary. */
	/* If either specs or prompt_data is set to a string, use it.  */
	if (!STRINGP(specs) && prompt_data == 0) {
		struct gcpro gcpro1, gcpro2, gcpro3;
		int i = num_input_chars;
		Lisp_Object input = specs;

		GCPRO3(function, specs, input);
		/* Compute the arg values using the user's expression.  */
		specs = Feval(specs);
		if (EQ(record_flag, Qlambda)) {	/* XEmacs addition */
			UNGCPRO;
			return specs;
		}
		if (!NILP(record_flag) || i != num_input_chars) {
			/* We should record this command on the command history.  */
			/* #### The following is too specific; should have general
			   mechanism for doing this. */
			Lisp_Object values, car;
			/* Make a copy of the list of values, for the command history,
			   and turn them into things we can eval.  */
			values = quotify_args(Fcopy_sequence(specs));
			/* If the list of args was produced with an explicit call to `list',
			   look for elements that were computed with (region-beginning)
			   or (region-end), and put those expressions into VALUES
			   instead of the present values.  */
			if (CONSP(input)) {
				car = XCAR(input);
				/* Skip through certain special forms.  */
				while (EQ(car, Qlet) || EQ(car, QletX)
				       || EQ(car, Qsave_excursion)) {
					while (CONSP(XCDR(input)))
						input = XCDR(input);
					input = XCAR(input);
					if (!CONSP(input))
						break;
					car = XCAR(input);
				}
				if (EQ(car, Qlist)) {
					Lisp_Object intail, valtail;
					for (intail = Fcdr(input), valtail =
					     values; CONSP(valtail);
					     intail = Fcdr(intail), valtail =
					     Fcdr(valtail)) {
						Lisp_Object elt;
						elt = Fcar(intail);
						if (CONSP(elt)) {
							Lisp_Object eltcar =
							    Fcar(elt);
							if (EQ(eltcar, Qpoint)
							    || EQ(eltcar, Qmark)
							    || EQ(eltcar,
								  Qregion_beginning)
							    || EQ(eltcar,
								  Qregion_end))
								Fsetcar(valtail,
									Fcar
									(intail));
						}
					}
				}
			}
			Vcommand_history
			    = Fcons(Fcons(function, values), Vcommand_history);
		}
		single_console_state();
		RETURN_UNGCPRO(apply1(fun, specs));
	}

	/* Here if function specifies a string to control parsing the defaults */

#ifdef I18N3
	/* Translate interactive prompt. */
	if (STRINGP(specs)) {
		Lisp_Object domain = Qnil;
		if (COMPILED_FUNCTIONP(fun))
			domain =
			    compiled_function_domain(XCOMPILED_FUNCTION(fun));
		if (NILP(domain))
			specs = Fgettext(specs);
		else
			specs = Fdgettext(domain, specs);
	} else if (prompt_data)
		/* We do not have to worry about domains in this case because
		   prompt_data is non-nil only for built-in functions, which
		   always use the default domain. */
		prompt_data = gettext(prompt_data);
#endif

	/* Handle special starting chars `*' and `@' and `_'.  */
	/* Note that `+' is reserved for user extensions.  */
	prompt_index = 0;
	{
		struct gcpro gcpro1, gcpro2;
		GCPRO2(function, specs);

		for (;;) {
			if (STRINGP(specs))
				prompt_data = (char *)XSTRING_DATA(specs);

			if (prompt_data[prompt_index] == '+')
				error
				    ("`+' is not used in `interactive' for ordinary commands");
			else if (prompt_data[prompt_index] == '*') {
				prompt_index++;
				if (!NILP(current_buffer->read_only))
					barf_if_buffer_read_only(current_buffer,
								 -1, -1);
			} else if (prompt_data[prompt_index] == '@') {
				Lisp_Object event;
				prompt_index++;

				if (!NILP(keys))
					event =
					    extract_vector_nth_mouse_event(keys,
									   0);
				else
#if 0
					event =
					    extract_this_command_keys_nth_mouse_event
					    (0);
#else
					/* Doesn't work; see below */
					event = Vcurrent_mouse_event;
#endif
				if (!NILP(event)) {
					Lisp_Object window =
					    Fevent_window(event);
					if (!NILP(window)) {
						if (MINI_WINDOW_P
						    (XWINDOW(window))
						    && !(minibuf_level > 0
							 && EQ(window,
							       minibuf_window)))
							error
							    ("Attempt to select inactive minibuffer window");

#if 0				/* unclean! see event-stream.c */
						/* If the current buffer wants to clean up, let it.  */
						if (!NILP
						    (Vmouse_leave_buffer_hook))
							run_hook
							    (Qmouse_leave_buffer_hook);
#endif

						Fselect_window(window, Qnil);
					}
				}
			} else if (prompt_data[prompt_index] == '_') {
				prompt_index++;
				set_zmacs_region_stays = 1;
			} else {
				UNGCPRO;
				break;
			}
		}
	}

	/* Count the number of arguments the interactive spec would have
	   us give to the function.  */
	argcount = 0;
	{
		const char *tem;
		for (tem = prompt_data + prompt_index; *tem;) {
			/* 'r' specifications ("point and mark as 2 numeric args")
			   produce *two* arguments.  */
			if (*tem == 'r')
				argcount += 2;
			else
				argcount += 1;
			tem = (const char *)strchr(tem + 1, '\n');
			if (!tem)
				break;
			tem++;
		}
	}

#ifdef IT_SEEMS_THAT_MLY_DOESNT_LIKE_THIS
	if (!NILP(enable))
		specbind(Qenable_recursive_minibuffers, Qt);
#endif

	if (argcount == 0) {
		/* Interactive function or no arguments; just call it */
		if (EQ(record_flag, Qlambda))
			return Qnil;
		if (!NILP(record_flag)) {
			Vcommand_history =
			    Fcons(list1(function), Vcommand_history);
		}
		specbind(Qcommand_debug_status, Qnil);
		/* XEmacs: was fun = call0 (fun), but that's backtraced wrong */
		{
			struct gcpro gcpro1;

			GCPRO1(fun);
			fun = Ffuncall(1, &fun);
			UNGCPRO;
		}
		if (set_zmacs_region_stays)
			zmacs_region_stays = 1;
		return unbind_to(speccount, fun);
	}

	/* Read interactive arguments */
	{
		/* args[-1] is the function to call */
		/* args[n] is the n'th argument to the function */
		int alloca_size = (1	/* function to call */
				   + argcount	/* actual arguments */
				   + argcount	/* visargs */
				   + argcount	/* varies */
		    );
		Lisp_Object *fcall= alloca_array(Lisp_Object, alloca_size);
		Lisp_Object *args = fcall+1;
		/* visargs is an array of either Qnil or user-friendlier
		    versions (often
		 *  strings) of previous arguments, to use in prompts for
		    successive
		 *  arguments.  ("Often strings" because emacs didn't used to
		    have
		 *  format %S and prin1-to-string.) */
		Lisp_Object *visargs = args + argcount;
		/* If varies[i] is non-null, the i'th argument shouldn't just
		   have its value in this call quoted in the command history.
		   It should be recorded as a call to the function named
		   varies[i]]. */
		Lisp_Object *varies = visargs + argcount;
		int arg_from_tty = 0;
		REGISTER int argnum;
		struct gcpro gcpro1, gcpro2;

		fcall[0] = function;
		for (argnum = 0; argnum < alloca_size - 1; argnum++)
			args[argnum] = Qnil;

		/* Must GC-protect args[-1] (ie function) because Ffuncall
		   doesn't */
		/* `function' itself isn't GC-protected -- use args[-1] from
		   here (actually, doesn't matter since Emacs GC doesn't
		   relocate, sigh) */
		GCPRO1n(prefix, &args[-1], alloca_size);

		for (argnum = 0;; argnum++) {
			const char *prompt_start =
				prompt_data + prompt_index + 1;
			char *prompt_limit =
				(char *)strchr(prompt_start, '\n');
			int prompt_length;
			prompt_length = ((prompt_limit)
					 ? (prompt_limit - prompt_start)
					 : (int)strlen(prompt_start));
			if (prompt_limit && prompt_limit[1] == 0) {
				/* "sfoo:\n" -- strip tailing return */
				prompt_limit = 0;
				prompt_length -= 1;
			}
			/* This uses `visargs' instead of `args' so that
			   global-set-key prompts with "Set key C-x C-f to
			   command: "instead of printing event objects in there.
			 */
#define PROMPT() callint_prompt ((const Bufbyte *) prompt_start,	\
				 prompt_length, visargs, argnum)
			switch (prompt_data[prompt_index]) {
			case 'a': {
				/* Symbol defined as a function */
				Lisp_Object tem = call1(
					Qread_function, PROMPT());
				args[argnum] = tem;
				arg_from_tty = 1;
				break;
			}
			case 'b': {
				/* Name of existing buffer */
				Lisp_Object def = Fcurrent_buffer();
				if (EQ(Fselected_window(Qnil),
				       minibuf_window))
					def = Fother_buffer(def, Qnil, Qnil);
				/* read-buffer returns a buffer name, not a
				   buffer! */
				args[argnum] = call3(
					Qread_buffer, PROMPT(), def, Qt);
				arg_from_tty = 1;
				break;
			}
			case 'B': {
				/* Name of buffer, possibly nonexistent */
				/* read-buffer returns a buffer name, not a
				   buffer! */
				args[argnum] = call2(Qread_buffer, PROMPT(),
						     Fother_buffer(
							     Fcurrent_buffer(),
							     Qnil, Qnil));
				arg_from_tty = 1;
				break;
			}
			case 'c': {
				/* Character */
				Lisp_Object tem;
				int shadowing_speccount =
					specpdl_depth();

				specbind(Qcursor_in_echo_area, Qt);
				{
					Lisp_Object tmp = PROMPT();
					message("%s", XSTRING_DATA(tmp));
				}
				tem = (call0(Qread_char));
				args[argnum] = tem;
				/* visargs[argnum] = Fsingle_key_description
				   (tem); */
				/* FSF has visargs[argnum] = Fchar_to_string
				   (tem); */

				unbind_to(shadowing_speccount, Qnil);

				/* #### `C-x / a' should not leave the prompt in
				   #### the minibuffer.
				   This isn't the right fix, because (message
				   ...) (read-char) shouldn't leave the message
				   there either... */
				clear_message();

				arg_from_tty = 1;
				break;
			}
			case 'C': {
				/* Command: symbol with interactive function */
				Lisp_Object tem =
					call1(Qread_command, PROMPT());
				args[argnum] = tem;
				arg_from_tty = 1;
				break;
			}
			case 'd': {
				/* Value of point.  Does not do I/O.  */
				args[argnum] = Fcopy_marker(current_buffer->
							    point_marker, Qt);
				varies[argnum] = Qpoint;
				break;
			}
			case 'e':
				{
					Lisp_Object event;

					if (!NILP(keys))
						event =
						    extract_vector_nth_mouse_event
						    (keys, mouse_event_count);
					else
#if 0
						/* This doesn't quite work because this-command-keys
						   behaves in utterly counterintuitive ways.  Sometimes
						   it retrieves an event back in the future, e.g. when
						   one command invokes another command and both are
						   invoked with the mouse. */
						event =
						    (extract_this_command_keys_nth_mouse_event
						     (mouse_event_count));
#else
						event = Vcurrent_mouse_event;
#endif

					if (NILP(event))
						error
						    ("%s must be bound to a mouse or misc-user event",
						     (SYMBOLP(function)
						      ? (char *)
						      string_data(XSYMBOL
								  (function)->
								  name)
						      : "command"));
					args[argnum] = event;
					mouse_event_count++;
					break;
				}
			case 'D':	/* Directory name. */
				{
					args[argnum] = call4(Qread_directory_name, PROMPT(), Qnil,	/* dir */
							     current_buffer->directory,	/* default */
							     Qt	/* must-match */
					    );
					arg_from_tty = 1;
					break;
				}
			case 'f':	/* Existing file name. */
				{
					Lisp_Object tem =
					    call4(Qread_file_name, PROMPT(),
						  Qnil,	/* dir */
						  Qnil,	/* default */
						  Qzero	/* must-match */
					    );
					args[argnum] = tem;
					arg_from_tty = 1;
					break;
				}
			case 'F':	/* Possibly nonexistent file name. */
				{
					args[argnum] = call4(Qread_file_name, PROMPT(), Qnil,	/* dir */
							     Qnil,	/* default */
							     Qnil	/* must-match */
					    );
					arg_from_tty = 1;
					break;
				}
			case 'i':	/* Ignore: always nil. Use to skip arguments. */
				{
					args[argnum] = Qnil;
					break;
				}
			case 'k':	/* Key sequence (vector of events) */
				{
					struct gcpro ngcpro1;
					Lisp_Object tem;
					Lisp_Object key_prompt = PROMPT();

					NGCPRO1(key_prompt);
					tem =
					    Fread_key_sequence(key_prompt, Qnil,
							       Qnil);
					NUNGCPRO;

					visargs[argnum] = Fkey_description(tem);
					/* The following makes `describe-key' not work with
					   extent-local keymaps and such; and anyway, it's
					   contrary to the documentation. */
					/* args[argnum] = call1 (Qevents_to_keys, tem); */
					args[argnum] = tem;
					arg_from_tty = 1;
					break;
				}
			case 'K':	/* Key sequence (vector of events),
					   no automatic downcasing */
				{
					struct gcpro ngcpro1;
					Lisp_Object tem;
					Lisp_Object key_prompt = PROMPT();

					NGCPRO1(key_prompt);
					tem =
					    Fread_key_sequence(key_prompt, Qnil,
							       Qt);
					NUNGCPRO;

					visargs[argnum] = Fkey_description(tem);
					/* The following makes `describe-key' not work with
					   extent-local keymaps and such; and anyway, it's
					   contrary to the documentation. */
					/* args[argnum] = call1 (Qevents_to_keys, tem); */
					args[argnum] = tem;
					arg_from_tty = 1;
					break;
				}

			case 'm':	/* Value of mark.  Does not do I/O.  */
				{
					args[argnum] = current_buffer->mark;
					varies[argnum] = Qmark;
					break;
				}
			case 'n':	/* Read number from minibuffer.  */
				{
				      read_number:
					args[argnum] =
					    call2(Qread_number, PROMPT(), Qnil);
					/* numbers are too boring to go on command history */
					/* arg_from_tty = 1; */
					break;
				}
			case 'N':	/* Prefix arg, else number from minibuffer */
				{
					if (NILP(prefix))
						goto read_number;
					else
						goto prefix_value;
				}
			case 'P':	/* Prefix arg in raw form.  Does no I/O.  */
				{
					args[argnum] = prefix;
					break;
				}
			case 'p':	/* Prefix arg converted to number.  No I/O. */
				{
				      prefix_value:
					{
						Lisp_Object tem =
						    Fprefix_numeric_value
						    (prefix);
						args[argnum] = tem;
					}
					break;
				}
			case 'r':	/* Region, point and mark as 2 args. */
				{
					Bufpos tem = check_mark();
					args[argnum] =
					    (BUF_PT(current_buffer) <
					     tem ? Fcopy_marker(current_buffer->
								point_marker,
								Qt)
					     : current_buffer->mark);
					varies[argnum] = Qregion_beginning;
					args[++argnum] =
					    (BUF_PT(current_buffer) >
					     tem ? Fcopy_marker(current_buffer->
								point_marker,
								Qt)
					     : current_buffer->mark);
					varies[argnum] = Qregion_end;
					break;
				}
			case 's':	/* String read via minibuffer.  */
				{
					args[argnum] =
					    call1(Qread_string, PROMPT());
					arg_from_tty = 1;
					break;
				}
			case 'S':	/* Any symbol.  */
				{
					visargs[argnum] = Qnil;
					for (;;) {
						Lisp_Object tem =
						    call5(Qcompleting_read,
							  PROMPT(),
							  Vobarray,
							  Qnil,
							  Qnil,
							  /* nil, or prev attempt */
							  visargs[argnum]);
						visargs[argnum] = tem;
						/* I could use condition-case with this loser, but why bother?
						 * tem = Fread (tem); check-symbol-p;
						 */
						tem = Fintern(tem, Qnil);
						args[argnum] = tem;
						if (string_length
						    (XSYMBOL(tem)->name) > 0)
							/* Don't accept the empty-named symbol.  If the loser
							   really wants this s/he can call completing-read
							   directly */
							break;
					}
					arg_from_tty = 1;
					break;
				}
			case 'v':	/* Variable name: user-variable-p symbol */
				{
					Lisp_Object tem =
					    call1(Qread_variable, PROMPT());
					args[argnum] = tem;
					arg_from_tty = 1;
					break;
				}
			case 'x':	/* Lisp expression read but not evaluated */
				{
					args[argnum] =
					    call1(Qread_expression, PROMPT());
					/* visargs[argnum] = Fprin1_to_string (args[argnum], Qnil); */
					arg_from_tty = 1;
					break;
				}
			case 'X':	/* Lisp expression read and evaluated */
				{
					Lisp_Object tem =
					    call1(Qread_expression, PROMPT());
					/* visargs[argnum] = Fprin1_to_string (tem, Qnil); */
					args[argnum] = Feval(tem);
					arg_from_tty = 1;
					break;
				}
			case 'Z':	/* Coding-system symbol or nil if no prefix */
				{
#if defined(MULE) || defined(FILE_CODING)
					if (NILP(prefix)) {
						args[argnum] = Qnil;
					} else {
						args[argnum] =
						    call1
						    (Qread_non_nil_coding_system,
						     PROMPT());
						arg_from_tty = 1;
					}
#else
					args[argnum] = Qnil;
#endif
					break;
				}
			case 'z':	/* Coding-system symbol */
				{
#if defined(MULE) || defined(FILE_CODING)
					args[argnum] =
					    call1(Qread_coding_system,
						  PROMPT());
					arg_from_tty = 1;
#else
					args[argnum] = Qnil;
#endif
					break;
				}

				/* We have a case for `+' so we get an error
				   if anyone tries to define one here.  */
			case '+':
			default:
				{
					error
					    ("Invalid `interactive' control letter \"%c\" (#o%03o).",
					     prompt_data[prompt_index],
					     prompt_data[prompt_index]);
				}
			}
#undef PROMPT
			if (NILP(visargs[argnum]))
				visargs[argnum] = args[argnum];

			if (!prompt_limit)
				break;
			if (STRINGP(specs)) {
				prompt_data = (char *)XSTRING_DATA(specs);
			}
			/* +1 to skip spec, +1 for \n */
			prompt_index += prompt_length + 1 + 1;
		}
		unbind_to(speccount, Qnil);

		QUIT;

		if (EQ(record_flag, Qlambda)) {
			RETURN_UNGCPRO(Flist(argcount, args));
		}

		if (arg_from_tty || !NILP(record_flag)) {
			/* Reuse visargs as a temporary for constructing the command history */
			for (argnum = 0; argnum < argcount; argnum++) {
				if (!NILP(varies[argnum]))
					visargs[argnum] = list1(varies[argnum]);
				else
					visargs[argnum] =
					    Fquote_maybe(args[argnum]);
			}
			Vcommand_history =
			    Fcons(Fcons(args[-1], Flist(argcount, visargs)),
				  Vcommand_history);
		}

		/* If we used a marker to hold point, mark, or an end of the region,
		   temporarily, convert it to an integer now.  */
		for (argnum = 0; argnum < argcount; argnum++)
			if (!NILP(varies[argnum]))
				XSETINT(args[argnum],
					marker_position(args[argnum]));

		single_console_state();
		specbind(Qcommand_debug_status, Qnil);
		fun = Ffuncall(argcount + 1, args - 1);
		UNGCPRO;
		if (set_zmacs_region_stays)
			zmacs_region_stays = 1;
		return unbind_to(speccount, fun);
	}
}

DEFUN("prefix-numeric-value", Fprefix_numeric_value, 1, 1, 0,	/*
Return numeric meaning of raw prefix argument RAW.
A raw prefix argument is what you get from `(interactive "P")'.
Its numeric meaning is what you would get from `(interactive "p")'.
*/
      (raw))
{
	if (NILP(raw))
		return make_int(1);
	if (EQ(raw, Qminus))
		return make_int(-1);
	if (INTP(raw))
		return raw;
	if (CONSP(raw) && INTP(XCAR(raw)))
		return XCAR(raw);

	return make_int(1);
}

void syms_of_callint(void)
{
	defsymbol(&Qcall_interactively, "call-interactively");
	defsymbol(&Qread_from_minibuffer, "read-from-minibuffer");
	defsymbol(&Qcompleting_read, "completing-read");
	defsymbol(&Qread_file_name, "read-file-name");
	defsymbol(&Qread_directory_name, "read-directory-name");
	defsymbol(&Qread_string, "read-string");
	defsymbol(&Qread_buffer, "read-buffer");
	defsymbol(&Qread_variable, "read-variable");
	defsymbol(&Qread_function, "read-function");
	defsymbol(&Qread_command, "read-command");
	defsymbol(&Qread_number, "read-number");
	defsymbol(&Qread_expression, "read-expression");
#if defined(MULE) || defined(FILE_CODING)
	defsymbol(&Qread_coding_system, "read-coding-system");
	defsymbol(&Qread_non_nil_coding_system, "read-non-nil-coding-system");
#endif
	defsymbol(&Qevents_to_keys, "events-to-keys");
	defsymbol(&Qcommand_debug_status, "command-debug-status");
	defsymbol(&Qenable_recursive_minibuffers,
		  "enable-recursive-minibuffers");

	defsymbol(&QletX, "let*");
	defsymbol(&Qsave_excursion, "save-excursion");
#if 0				/* ill-conceived */
	defsymbol(&Qmouse_leave_buffer_hook, "mouse-leave-buffer-hook");
#endif

	DEFSUBR(Finteractive);
	DEFSUBR(Fquote_maybe);
	DEFSUBR(Fcall_interactively);
	DEFSUBR(Fprefix_numeric_value);
}

void vars_of_callint(void)
{
	DEFVAR_LISP("current-prefix-arg", &Vcurrent_prefix_arg	/*
The value of the prefix argument for this editing command.
It may be a number, or the symbol `-' for just a minus sign as arg,
or a list whose car is a number for just one or more C-U's
or nil if no argument has been specified.
This is what `(interactive "P")' returns.
								 */ );
	Vcurrent_prefix_arg = Qnil;

	DEFVAR_LISP("command-history", &Vcommand_history	/*
List of recent commands that read arguments from terminal.
Each command is represented as a form to evaluate.
								 */ );
	Vcommand_history = Qnil;

	DEFVAR_LISP("command-debug-status", &Vcommand_debug_status	/*
Debugging status of current interactive command.
Bound each time `call-interactively' is called;
may be set by the debugger as a reminder for itself.
									 */ );
	Vcommand_debug_status = Qnil;

#if 0				/* FSFmacs */
	xxDEFVAR_LISP("mark-even-if-inactive", &Vmark_even_if_inactive	/*
*Non-nil means you can use the mark even when inactive.
This option makes a difference in Transient Mark mode.
When the option is non-nil, deactivation of the mark
turns off region highlighting, but commands that use the mark
behave as if the mark were still active.
									 */ );
	Vmark_even_if_inactive = Qnil;
#endif

#if 0				/* Doesn't work and is totally ill-conceived anyway. */
	xxDEFVAR_LISP("mouse-leave-buffer-hook", &Vmouse_leave_buffer_hook	/*
Hook to run when about to switch windows with a mouse command.
Its purpose is to give temporary modes such as Isearch mode
a way to turn themselves off when a mouse command switches windows.
										 */ );
	Vmouse_leave_buffer_hook = Qnil;
#endif
}
