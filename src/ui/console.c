/* The console object.
   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
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

/* Written by Ben Wing. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "TTY/console-tty.h" /* for Fconsole_tty_controlling process ins
				suspend-console. Needs refactoring...*/
#include "events/events.h"
#include "frame.h"
#include "redisplay.h"
#include "sysdep.h"
#include "window.h"

Lisp_Object Vconsole_list, Vselected_console;

Lisp_Object Vcreate_console_hook, Vdelete_console_hook;

Lisp_Object Qconsolep, Qconsole_live_p;
Lisp_Object Qcreate_console_hook;
Lisp_Object Qdelete_console_hook;

Lisp_Object Qsuspend_hook;
Lisp_Object Qsuspend_resume_hook;

/* This structure holds the default values of the console-local
   variables defined with DEFVAR_CONSOLE_LOCAL, that have special
   slots in each console.  The default value occupies the same slot
   in this structure as an individual console's value occupies in
   that console.  Setting the default value also goes through the
   list of consoles and stores into each console that does not say
   it has a local value.  */
Lisp_Object Vconsole_defaults;
static void *console_defaults_saved_slots;

/* This structure marks which slots in a console have corresponding
   default values in console_defaults.
   Each such slot has a nonzero value in this structure.
   The value has only one nonzero bit.

   When a console has its own local value for a slot,
   the bit for that slot (found in the same slot in this structure)
   is turned on in the console's local_var_flags slot.

   If a slot in this structure is 0, then there is a DEFVAR_CONSOLE_LOCAL
   for the slot, but there is no default value for it; the corresponding
   slot in console_defaults is not used except to initialize newly-created
   consoles.

   If a slot is -1, then there is a DEFVAR_CONSOLE_LOCAL for it
  as well as a default value which is used to initialize newly-created
   consoles and as a reset-value when local-vars are killed.

   If a slot is -2, there is no DEFVAR_CONSOLE_LOCAL for it.
   (The slot is always local, but there's no lisp variable for it.)
   The default value is only used to initialize newly-creation consoles.

   If a slot is -3, then there is no DEFVAR_CONSOLE_LOCAL for it but
   there is a default which is used to initialize newly-creation
   consoles and as a reset-value when local-vars are killed.

   */
struct console console_local_flags;

/* This structure holds the names of symbols whose values may be
   console-local.  It is indexed and accessed in the same way as the above. */
static Lisp_Object Vconsole_local_symbols;
static void *console_local_symbols_saved_slots;

DEFINE_CONSOLE_TYPE(dead);

Lisp_Object Vconsole_type_list;

console_type_entry_dynarr *the_console_type_entry_dynarr;

static Lisp_Object mark_console(Lisp_Object obj)
{
	struct console *con = XCONSOLE(obj);

#define MARKED_SLOT(x) mark_object (con->x)
#include "conslots.h"
#undef MARKED_SLOT

	/* Can be zero for Vconsole_defaults, Vconsole_local_symbols */
	if (con->conmeths) {
		mark_object(con->conmeths->symbol);
		MAYBE_CONMETH(con, mark_console, (con));
	}

	return Qnil;
}

static void
print_console(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	struct console *con = XCONSOLE(obj);

	if (print_readably)
		error("printing unreadable object #<console %s 0x%x>",
		      XSTRING_DATA(con->name), con->header.uid);

	write_fmt_string(printcharfun, "#<%s-console",
			 (!CONSOLE_LIVE_P(con) ? "dead" : CONSOLE_TYPE_NAME(con)));
	if (CONSOLE_LIVE_P(con) && !NILP(CONSOLE_CONNECTION(con))) {
		write_c_string(" on ", printcharfun);
		print_internal(CONSOLE_CONNECTION(con), printcharfun, 1);
	}
	write_fmt_str(printcharfun, " 0x%x>", con->header.uid);
}

DEFINE_LRECORD_IMPLEMENTATION("console", console,
			      mark_console, print_console, 0, 0, 0, 0,
			      struct console);

static struct console *allocate_console(void)
{
	Lisp_Object console;
	struct console *con =
	    alloc_lcrecord_type(struct console, &lrecord_console);
	struct gcpro gcpro1;

	copy_lcrecord(con, XCONSOLE(Vconsole_defaults));

	XSETCONSOLE(console, con);
	GCPRO1(console);

	con->quit_char = 7;	/* C-g */
	con->command_builder = allocate_command_builder(console);
	con->function_key_map = Fmake_sparse_keymap(Qnil);

	UNGCPRO;
	return con;
}

struct console *decode_console(Lisp_Object console)
{
	if (NILP(console))
		console = Fselected_console();
	/* quietly accept devices and frames for the console arg */
	if (DEVICEP(console) || FRAMEP(console))
		console = DEVICE_CONSOLE(decode_device(console));
	CHECK_LIVE_CONSOLE(console);
	return XCONSOLE(console);
}

struct console_methods *decode_console_type(Lisp_Object type,
					    Error_behavior errb)
{
	int i;

	for (i = 0; i < Dynarr_length(the_console_type_entry_dynarr); i++)
		if (EQ
		    (type, Dynarr_at(the_console_type_entry_dynarr, i).symbol))
			return Dynarr_at(the_console_type_entry_dynarr,
					 i).meths;

	maybe_signal_simple_error("Invalid console type", type, Qconsole, errb);

	return 0;
}

int valid_console_type_p(Lisp_Object type)
{
	return decode_console_type(type, ERROR_ME_NOT) != 0;
}

DEFUN("valid-console-type-p", Fvalid_console_type_p, 1, 1, 0,	/*
Return t if CONSOLE-TYPE is a valid console type.
Valid types are 'x, 'tty, and 'stream.
*/
      (console_type))
{
	return valid_console_type_p(console_type) ? Qt : Qnil;
}

DEFUN("console-type-list", Fconsole_type_list, 0, 0, 0,	/*
Return a list of valid console types.
*/
      ())
{
	return Fcopy_sequence(Vconsole_type_list);
}

DEFUN("cdfw-console", Fcdfw_console, 1, 1, 0,	/*
Given a console, device, frame, or window, return the associated console.
Return nil otherwise.
*/
      (object))
{
	return CDFW_CONSOLE(object);
}

DEFUN("selected-console", Fselected_console, 0, 0, 0,	/*
Return the console which is currently active.
*/
      ())
{
	return Vselected_console;
}

/* Called from selected_device_1(), called from selected_frame_1(),
   called from Fselect_window() */
void select_console_1(Lisp_Object console)
{
	/* perhaps this should do something more complicated */
	Vselected_console = console;

	/* #### Schedule this to be removed in 19.14 */
#ifdef HAVE_X_WINDOWS
	if (CONSOLE_X_P(XCONSOLE(console)))
		Vwindow_system = Qx;
	else
#endif
		Vwindow_system = Qnil;
}

DEFUN("select-console", Fselect_console, 1, 1, 0,	/*
Select the console CONSOLE.
Subsequent editing commands apply to its selected device, selected frame,
and selected window.  The selection of CONSOLE lasts until the next time
the user does something to select a different console, or until the next
time this function is called.
*/
      (console))
{
	Lisp_Object device;

	CHECK_LIVE_CONSOLE(console);

	device = CONSOLE_SELECTED_DEVICE(XCONSOLE(console));
	if (!NILP(device)) {
		struct device *d = XDEVICE(device);
		Lisp_Object frame = DEVICE_SELECTED_FRAME(d);
		if (!NILP(frame)) {
			struct frame *f = XFRAME(frame);
			Fselect_window(FRAME_SELECTED_WINDOW(f), Qnil);
		} else
			error("Can't select console with no frames.");
	} else
		error("Can't select a console with no devices");
	return Qnil;
}

void set_console_last_nonminibuf_frame(struct console *con, Lisp_Object frame)
{
	con->last_nonminibuf_frame = frame;
}

DEFUN("consolep", Fconsolep, 1, 1, 0,	/*
Return non-nil if OBJECT is a console.
*/
      (object))
{
	return CONSOLEP(object) ? Qt : Qnil;
}

DEFUN("console-live-p", Fconsole_live_p, 1, 1, 0,	/*
Return non-nil if OBJECT is a console that has not been deleted.
*/
      (object))
{
	return CONSOLEP(object) && CONSOLE_LIVE_P(XCONSOLE(object)) ? Qt : Qnil;
}

DEFUN("console-type", Fconsole_type, 0, 1, 0,	/*
Return the console type (e.g. `x' or `tty') of CONSOLE.
Value is `tty' for a tty console (a character-only terminal),
`x' for a console that is an X display,
`mswindows' for a console that is a Windows NT/95/97 connection,
`pc' for a console that is a direct-write MS-DOS connection (not yet
implemented),
`stream' for a stream console (which acts like a stdio stream), and
`dead' for a deleted console.
*/
      (console))
{
	/* don't call decode_console() because we want to allow for dead
	   consoles. */
	if (NILP(console))
		console = Fselected_console();
	CHECK_CONSOLE(console);
	return CONSOLE_TYPE(XCONSOLE(console));
}

DEFUN("console-name", Fconsole_name, 0, 1, 0,	/*
Return the name of CONSOLE.
*/
      (console))
{
	return CONSOLE_NAME(decode_console(console));
}

DEFUN("console-connection", Fconsole_connection, 0, 1, 0,	/*
Return the connection of the specified console.
CONSOLE defaults to the selected console if omitted.
*/
      (console))
{
	return CONSOLE_CONNECTION(decode_console(console));
}

Lisp_Object make_console(struct console * con)
{
	Lisp_Object console;
	XSETCONSOLE(console, con);
	return console;
}

static Lisp_Object
semi_canonicalize_console_connection(struct console_methods *meths,
				     Lisp_Object name, Error_behavior errb)
{
	if (HAS_CONTYPE_METH_P(meths, semi_canonicalize_console_connection))
		return CONTYPE_METH(meths, semi_canonicalize_console_connection,
				    (name, errb));
	else
		return CONTYPE_METH_OR_GIVEN(meths,
					     canonicalize_console_connection,
					     (name, errb), name);
}

static Lisp_Object
canonicalize_console_connection(struct console_methods *meths,
				Lisp_Object name, Error_behavior errb)
{
	if (HAS_CONTYPE_METH_P(meths, canonicalize_console_connection))
		return CONTYPE_METH(meths, canonicalize_console_connection,
				    (name, errb));
	else
		return CONTYPE_METH_OR_GIVEN(meths,
					     semi_canonicalize_console_connection,
					     (name, errb), name);
}

static Lisp_Object
find_console_of_type(struct console_methods *meths, Lisp_Object canon)
{
	Lisp_Object concons;

	CONSOLE_LOOP(concons) {
		Lisp_Object console = XCAR(concons);

		if (EQ(CONMETH_TYPE(meths), CONSOLE_TYPE(XCONSOLE(console)))
		    &&
		    internal_equal(CONSOLE_CANON_CONNECTION(XCONSOLE(console)),
				   canon, 0))
			return console;
	}

	return Qnil;
}

DEFUN("find-console", Ffind_console, 1, 2, 0,	/*
Look for an existing console attached to connection CONNECTION.
Return the console if found; otherwise, return nil.

If TYPE is specified, only return consoles of that type; otherwise,
return consoles of any type. (It is possible, although unlikely,
that two consoles of different types could have the same connection
name; in such a case, the first console found is returned.)
*/
      (connection, type))
{
	Lisp_Object canon = Qnil;
	struct gcpro gcpro1;

	GCPRO1(canon);

	if (!NILP(type)) {
		struct console_methods *conmeths =
		    decode_console_type(type, ERROR_ME);
		canon =
		    canonicalize_console_connection(conmeths, connection,
						    ERROR_ME_NOT);
		if (UNBOUNDP(canon))
			RETURN_UNGCPRO(Qnil);

		RETURN_UNGCPRO(find_console_of_type(conmeths, canon));
	} else {
		int i;

		for (i = 0; i < Dynarr_length(the_console_type_entry_dynarr);
		     i++) {
			struct console_methods *conmeths =
			    Dynarr_at(the_console_type_entry_dynarr, i).meths;
			canon =
			    canonicalize_console_connection(conmeths,
							    connection,
							    ERROR_ME_NOT);
			if (!UNBOUNDP(canon)) {
				Lisp_Object console =
				    find_console_of_type(conmeths, canon);
				if (!NILP(console))
					RETURN_UNGCPRO(console);
			}
		}

		RETURN_UNGCPRO(Qnil);
	}
}

DEFUN("get-console", Fget_console, 1, 2, 0,	/*
Look for an existing console attached to connection CONNECTION.
Return the console if found; otherwise, signal an error.

If TYPE is specified, only return consoles of that type; otherwise,
return consoles of any type. (It is possible, although unlikely,
that two consoles of different types could have the same connection
name; in such a case, the first console found is returned.)
*/
      (connection, type))
{
	Lisp_Object console = Ffind_console(connection, type);
	if (NILP(console)) {
		if (NILP(type))
			signal_simple_error("No such console", connection);
		else
			signal_simple_error_2("No such console", type,
					      connection);
	}
	return console;
}

Lisp_Object
create_console(Lisp_Object name, Lisp_Object type, Lisp_Object connection,
	       Lisp_Object props)
{
	/* This function can GC */
	struct console *con;
	Lisp_Object console;
	struct gcpro gcpro1;

	console = Ffind_console(connection, type);
	if (!NILP(console))
		return console;

	con = allocate_console();
	XSETCONSOLE(console, con);

	GCPRO1(console);

	con->conmeths = decode_console_type(type, ERROR_ME);

	CONSOLE_NAME(con) = name;
	CONSOLE_CONNECTION(con) =
	    semi_canonicalize_console_connection(con->conmeths, connection,
						 ERROR_ME);
	CONSOLE_CANON_CONNECTION(con) =
	    canonicalize_console_connection(con->conmeths, connection,
					    ERROR_ME);

	MAYBE_CONMETH(con, init_console, (con, props));

	/* Do it this way so that the console list is in order of creation */
	Vconsole_list = nconc2(Vconsole_list, Fcons(console, Qnil));

	if (CONMETH_OR_GIVEN(con, initially_selected_for_input, (con), 0))
		event_stream_select_console(con);

	UNGCPRO;
	return console;
}

void
add_entry_to_console_type_list(Lisp_Object symbol,
			       struct console_methods *meths)
{
	struct console_type_entry entry;

	entry.symbol = symbol;
	entry.meths = meths;
	Dynarr_add(the_console_type_entry_dynarr, entry);
	Vconsole_type_list = Fcons(symbol, Vconsole_type_list);
}

/* find a console other than the selected one.  Prefer non-stream
   consoles over stream consoles. */

static Lisp_Object find_other_console(Lisp_Object console)
{
	Lisp_Object concons;

	/* look for a non-stream console */
	CONSOLE_LOOP(concons) {
		Lisp_Object con = XCAR(concons);
		if (!CONSOLE_STREAM_P(XCONSOLE(con))
		    && !EQ(con, console)
		    && !NILP(CONSOLE_SELECTED_DEVICE(XCONSOLE(con)))
		    && !NILP(DEVICE_SELECTED_FRAME
			     (XDEVICE(CONSOLE_SELECTED_DEVICE(XCONSOLE(con))))))
			break;
	}
	if (!NILP(concons))
		return XCAR(concons);

	/* OK, now look for a stream console */
	CONSOLE_LOOP(concons) {
		Lisp_Object con = XCAR(concons);
		if (!EQ(con, console)
		    && !NILP(CONSOLE_SELECTED_DEVICE(XCONSOLE(con)))
		    && !NILP(DEVICE_SELECTED_FRAME
			     (XDEVICE(CONSOLE_SELECTED_DEVICE(XCONSOLE(con))))))
			break;
	}
	if (!NILP(concons))
		return XCAR(concons);

	/* Sorry, there ain't none */
	return Qnil;
}

static int
find_nonminibuffer_frame_not_on_console_predicate(Lisp_Object frame,
						  void *closure)
{
	Lisp_Object console;

	VOID_TO_LISP(console, closure);
	if (FRAME_MINIBUF_ONLY_P(XFRAME(frame)))
		return 0;
	if (EQ(console, FRAME_CONSOLE(XFRAME(frame))))
		return 0;
	return 1;
}

static Lisp_Object find_nonminibuffer_frame_not_on_console(Lisp_Object console)
{
	return
	    find_some_frame(find_nonminibuffer_frame_not_on_console_predicate,
			    LISP_TO_VOID(console));
}

/* Delete console CON.

   If FORCE is non-zero, allow deletion of the only frame.

   If CALLED_FROM_KILL_EMACS is non-zero, then, if
   deleting the last console, just delete it,
   instead of calling `save-buffers-kill-emacs'.

   If FROM_IO_ERROR is non-zero, then the console is gone due
   to an I/O error.  This affects what happens if we exit
   (we do an emergency exit instead of `save-buffers-kill-emacs'.)
*/

void
delete_console_internal(struct console *con, int force,
			int called_from_kill_emacs, int from_io_error)
{
	/* This function can GC */
	Lisp_Object console;
	struct gcpro gcpro1;

	/* OK to delete an already-deleted console. */
	if (!CONSOLE_LIVE_P(con))
		return;

	XSETCONSOLE(console, con);
	GCPRO1(console);

	if (!called_from_kill_emacs) {
		int down_we_go = 0;

		if ((XINT(Flength(Vconsole_list)) == 1)
		    /* if we just created the console, it might not be listed,
		       or something ... */
		    && !NILP(memq_no_quit(console, Vconsole_list)))
			down_we_go = 1;
		/* If there aren't any nonminibuffer frames that would
		   be left, then exit. */
		else if (NILP(find_nonminibuffer_frame_not_on_console(console)))
			down_we_go = 1;

		if (down_we_go) {
			if (!force)
				error("Attempt to delete the only frame");
			else if (from_io_error) {
				/* Mayday mayday!  We're going down! */
				stderr_out("  Autosaving and exiting...\n");
				Vwindow_system = Qnil;	/* let it lie! */
				preparing_for_armageddon = 1;
				Fkill_emacs(make_int(70));
			} else {
				call0(Qsave_buffers_kill_emacs);
				UNGCPRO;
				/* If we get here, the user said they didn't want
				   to exit, so don't. */
				return;
			}
		}
	}

	/* Breathe a sigh of relief.  We're still alive. */

	{
		Lisp_Object frmcons, devcons;

		/* First delete all frames without their own minibuffers,
		   to avoid errors coming from attempting to delete a frame
		   that is a surrogate for another frame.

		   We don't set "called_from_delete_console" because we want the
		   device to go ahead and get deleted if we delete the last frame
		   on a device.  We won't run into trouble here because for any
		   frame without a minibuffer, there has to be another one on
		   the same console with a minibuffer, and we're not deleting that,
		   so delete_console_internal() won't get recursively called.

		   WRONG!  With surrogate minibuffers this isn't true.  Frames
		   with only a minibuffer are not enough to prevent
		   delete_frame_internal from triggering a device deletion. */
		CONSOLE_FRAME_LOOP_NO_BREAK(frmcons, devcons, con) {
			struct frame *f = XFRAME(XCAR(frmcons));
			/* delete_frame_internal() might do anything such as run hooks,
			   so be defensive. */
			if (FRAME_LIVE_P(f) && !FRAME_HAS_MINIBUF_P(f))
				delete_frame_internal(f, 1, 1, from_io_error);

			if (!CONSOLE_LIVE_P(con)) {	/* make sure the delete-*-hook didn't
							   go ahead and delete anything */
				UNGCPRO;
				return;
			}
		}

		CONSOLE_DEVICE_LOOP(devcons, con) {
			struct device *d = XDEVICE(XCAR(devcons));
			/* delete_device_internal() might do anything such as run hooks,
			   so be defensive. */
			if (DEVICE_LIVE_P(d))
				delete_device_internal(d, 1, 1, from_io_error);
			if (!CONSOLE_LIVE_P(con)) {	/* make sure the delete-*-hook didn't
							   go ahead and delete anything */
				UNGCPRO;
				return;
			}
		}
	}

	CONSOLE_SELECTED_DEVICE(con) = Qnil;

	/* try to select another console */

	if (EQ(console, Fselected_console())) {
		Lisp_Object other_dev = find_other_console(console);
		if (!NILP(other_dev))
			Fselect_console(other_dev);
		else {
			/* necessary? */
			Vselected_console = Qnil;
			Vwindow_system = Qnil;
		}
	}

	if (con->input_enabled)
		event_stream_unselect_console(con);

	MAYBE_CONMETH(con, delete_console, (con));

	Vconsole_list = delq_no_quit(console, Vconsole_list);
	RESET_CHANGED_SET_FLAGS;
	con->conmeths = dead_console_methods;

	UNGCPRO;
}

void io_error_delete_console(Lisp_Object console)
{
	delete_console_internal(XCONSOLE(console), 1, 0, 1);
}

DEFUN("delete-console", Fdelete_console, 1, 2, 0,	/*
Delete CONSOLE, permanently eliminating it from use.
Normally, you cannot delete the last non-minibuffer-only frame (you must
use `save-buffers-kill-emacs' or `kill-emacs').  However, if optional
second argument FORCE is non-nil, you can delete the last frame. (This
will automatically call `save-buffers-kill-emacs'.)
*/
      (console, force))
{
	CHECK_CONSOLE(console);
	delete_console_internal(XCONSOLE(console), !NILP(force), 0, 0);
	return Qnil;
}

DEFUN("console-list", Fconsole_list, 0, 0, 0,	/*
Return a list of all consoles.
*/
      ())
{
	return Fcopy_sequence(Vconsole_list);
}

DEFUN("console-device-list", Fconsole_device_list, 0, 1, 0,	/*
Return a list of all devices on CONSOLE.
If CONSOLE is nil, the selected console is used.
*/
      (console))
{
	return Fcopy_sequence(CONSOLE_DEVICE_LIST(decode_console(console)));
}

DEFUN("console-enable-input", Fconsole_enable_input, 1, 1, 0,	/*
Enable input on console CONSOLE.
*/
      (console))
{
	struct console *con = decode_console(console);
	if (!con->input_enabled)
		event_stream_select_console(con);
	return Qnil;
}

DEFUN("console-disable-input", Fconsole_disable_input, 1, 1, 0,	/*
Disable input on console CONSOLE.
*/
      (console))
{
	struct console *con = decode_console(console);
	if (con->input_enabled)
		event_stream_unselect_console(con);
	return Qnil;
}

DEFUN("console-on-window-system-p", Fconsole_on_window_system_p, 0, 1, 0,	/*
Return t if CONSOLE is on a window system.
If CONSOLE is nil, the selected console is used.
This generally means that there is support for the mouse, the menubar,
the toolbar, glyphs, etc.
*/
      (console))
{
	Lisp_Object type = CONSOLE_TYPE(decode_console(console));

	return !EQ(type, Qtty) && !EQ(type, Qstream) ? Qt : Qnil;
}

/**********************************************************************/
/*               Miscellaneous low-level functions                    */
/**********************************************************************/

static Lisp_Object unwind_init_sys_modes(Lisp_Object console)
{
	reinit_initial_console();

	if (!no_redraw_on_reenter &&
	    CONSOLEP(console) && CONSOLE_LIVE_P(XCONSOLE(console))) {
		struct frame *f =
		    XFRAME(DEVICE_SELECTED_FRAME
			   (XDEVICE
			    (CONSOLE_SELECTED_DEVICE(XCONSOLE(console)))));
		MARK_FRAME_CHANGED(f);
	}
	return Qnil;
}

DEFUN("suspend-emacs", Fsuspend_emacs, 0, 1, "",	/*
Stop Emacs and return to superior process.  You can resume later.
On systems that don't have job control, run a subshell instead.

If optional arg STUFFSTRING is non-nil, its characters are stuffed
to be read as terminal input by Emacs's superior shell.

Before suspending, run the normal hook `suspend-hook'.
After resumption run the normal hook `suspend-resume-hook'.

Some operating systems cannot stop the Emacs process and resume it later.
On such systems, Emacs will start a subshell and wait for it to exit.
*/
      (stuffstring))
{
	int speccount = specpdl_depth();
	struct gcpro gcpro1;

	if (!NILP(stuffstring))
		CHECK_STRING(stuffstring);
	GCPRO1(stuffstring);

	/* There used to be a check that the initial console is TTY.
	   This is bogus.  Even checking to see whether any console
	   is a controlling terminal is not correct -- maybe
	   the user used the -t option or something.  If we want to
	   suspend, then we suspend.  Period. */

	/* Call value of suspend-hook. */
	run_hook(Qsuspend_hook);

	reset_initial_console();
	/* sys_suspend can get an error if it tries to fork a subshell
	   and the system resources aren't available for that.  */
	record_unwind_protect(unwind_init_sys_modes, Vcontrolling_terminal);
	stuff_buffered_input(stuffstring);
	sys_suspend();
	/* the console is un-reset inside of the unwind-protect. */
	unbind_to(speccount, Qnil);

#ifdef SIGWINCH
	/* It is possible that a size change occurred while we were
	   suspended.  Assume one did just to be safe.  It won't hurt
	   anything if one didn't. */
	asynch_device_change_pending++;
#endif

	/* Call value of suspend-resume-hook
	   if it is bound and value is non-nil.  */
	run_hook(Qsuspend_resume_hook);

	UNGCPRO;
	return Qnil;
}

/* If STUFFSTRING is a string, stuff its contents as pending terminal input.
   Then in any case stuff anything Emacs has read ahead and not used.  */

void stuff_buffered_input(Lisp_Object stuffstring)
{
/* stuff_char works only in BSD, versions 4.2 and up.  */
#if defined (BSD) && defined (HAVE_TTY)
	if (!CONSOLEP(Vcontrolling_terminal) ||
	    !CONSOLE_LIVE_P(XCONSOLE(Vcontrolling_terminal)))
		return;

	if (STRINGP(stuffstring)) {
		Extcount count;
		Extbyte *p;

		TO_EXTERNAL_FORMAT(LISP_STRING, stuffstring,
				   ALLOCA, (p, count), Qkeyboard);
		while (count-- > 0)
			stuff_char(XCONSOLE(Vcontrolling_terminal), *p++);
		stuff_char(XCONSOLE(Vcontrolling_terminal), '\n');
	}
	/* Anything we have read ahead, put back for the shell to read.  */
# if 0				/* oh, who cares about this silliness */
	while (kbd_fetch_ptr != kbd_store_ptr) {
		if (kbd_fetch_ptr == kbd_buffer + KBD_BUFFER_SIZE)
			kbd_fetch_ptr = kbd_buffer;
		stuff_char(XCONSOLE(Vcontrolling_terminal), *kbd_fetch_ptr++);
	}
# endif
#endif				/* BSD && HAVE_TTY */
}

DEFUN("suspend-console", Fsuspend_console, 0, 1, "",	/*
Suspend a console.  For tty consoles, it sends a signal to suspend
the process in charge of the tty, and removes the devices and
frames of that console from the display.

If optional arg CONSOLE is non-nil, it is the console to be suspended.
Otherwise it is assumed to be the selected console.

Some operating systems cannot stop processes and resume them later.
On such systems, who knows what will happen.
*/
      (console))
{
#ifdef HAVE_TTY
	struct console *con = decode_console(console);

	if (CONSOLE_TTY_P(con)) {
		/*
		 * hide all the unhidden frames so the display code won't update
		 * them while the console is suspended.
		 */
		Lisp_Object device = CONSOLE_SELECTED_DEVICE(con);
		if (!NILP(device)) {
			struct device *d = XDEVICE(device);
			Lisp_Object frame_list = DEVICE_FRAME_LIST(d);
			while (CONSP(frame_list)) {
				struct frame *f = XFRAME(XCAR(frame_list));
				if (FRAME_REPAINT_P(f))
					f->visible = -1;
				frame_list = XCDR(frame_list);
			}
		}
		reset_one_console(con);
		event_stream_unselect_console(con);
		sys_suspend_process(XINT
				    (Fconsole_tty_controlling_process
				     (console)));
	}
#endif				/* HAVE_TTY */

	return Qnil;
}

DEFUN("resume-console", Fresume_console, 1, 1, "",	/*
Re-initialize a previously suspended console.
For tty consoles, do stuff to the tty to make it sane again.
*/
      (console))
{
#ifdef HAVE_TTY
	struct console *con = decode_console(console);

	if (CONSOLE_TTY_P(con)) {
		/* raise the selected frame */
		Lisp_Object device = CONSOLE_SELECTED_DEVICE(con);
		if (!NILP(device)) {
			struct device *d = XDEVICE(device);
			Lisp_Object frame = DEVICE_SELECTED_FRAME(d);
			if (!NILP(frame)) {
				/* force the frame to be cleared */
				SET_FRAME_CLEAR(XFRAME(frame));
				Fraise_frame(frame);
			}
		}
		init_one_console(con);
		event_stream_select_console(con);
#ifdef SIGWINCH
		/* The same as in Fsuspend_emacs: it is possible that a size
		   change occurred while we were suspended.  Assume one did just
		   to be safe.  It won't hurt anything if one didn't. */
		asynch_device_change_pending++;
#endif
	}
#endif				/* HAVE_TTY */

	return Qnil;
}

DEFUN("set-input-mode", Fset_input_mode, 3, 5, 0,	/*
Set mode of reading keyboard input.
First arg is ignored, for backward compatibility.
Second arg FLOW non-nil means use ^S/^Q flow control for output to terminal
(no effect except in CBREAK mode).
Third arg META t means accept 8-bit input (for a Meta key).
META nil means ignore the top bit, on the assumption it is parity.
Otherwise, accept 8-bit input and don't use the top bit for Meta.
First three arguments only apply to TTY consoles.
Optional fourth arg QUIT if non-nil specifies character to use for quitting.
Optional fifth arg CONSOLE specifies console to make changes to; nil means
the selected console.
See also `current-input-mode'.
*/
      (ignored, flow, meta, quit, console))
{
	struct console *con = decode_console(console);
	int meta_key = (!CONSOLE_TTY_P(con) ? 1 :
			EQ(meta, Qnil) ? 0 : EQ(meta, Qt) ? 1 : 2);

	if (!NILP(quit)) {
		CHECK_CHAR_COERCE_INT(quit);
		CONSOLE_QUIT_CHAR(con) =
		    ((unsigned int)XCHAR(quit)) & (meta_key ? 0377 : 0177);
	}
#ifdef HAVE_TTY
	if (CONSOLE_TTY_P(con)) {
		reset_one_console(con);
		TTY_FLAGS(con).flow_control = !NILP(flow);
		TTY_FLAGS(con).meta_key = meta_key;
		init_one_console(con);
		MARK_FRAME_CHANGED(XFRAME(CONSOLE_SELECTED_FRAME(con)));
	}
#endif

	return Qnil;
}

DEFUN("current-input-mode", Fcurrent_input_mode, 0, 1, 0,	/*
Return information about the way Emacs currently reads keyboard input.
Optional arg CONSOLE specifies console to return information about; nil means
the selected console.
The value is a list of the form (nil FLOW META QUIT), where
FLOW is non-nil if Emacs uses ^S/^Q flow control for output to the
terminal; this does not apply if Emacs uses interrupt-driven input.
META is t if accepting 8-bit input with 8th bit as Meta flag.
META nil means ignoring the top bit, on the assumption it is parity.
META is neither t nor nil if accepting 8-bit input and using
all 8 bits as the character code.
QUIT is the character Emacs currently uses to quit.
FLOW, and META are only meaningful for TTY consoles.
The elements of this list correspond to the arguments of
`set-input-mode'.
*/
      (console))
{
	struct console *con = decode_console(console);
	Lisp_Object flow, meta, quit;

#ifdef HAVE_TTY
	flow = CONSOLE_TTY_P(con) && TTY_FLAGS(con).flow_control ? Qt : Qnil;
	meta = (!CONSOLE_TTY_P(con) ? Qt :
		TTY_FLAGS(con).meta_key == 1 ? Qt :
		TTY_FLAGS(con).meta_key == 2 ? Qzero : Qnil);
#else
	flow = Qnil;
	meta = Qt;
#endif
	quit = make_char(CONSOLE_QUIT_CHAR(con));

	return list4(Qnil, flow, meta, quit);
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void syms_of_console(void)
{
	INIT_LRECORD_IMPLEMENTATION(console);

	DEFSUBR(Fvalid_console_type_p);
	DEFSUBR(Fconsole_type_list);
	DEFSUBR(Fcdfw_console);
	DEFSUBR(Fselected_console);
	DEFSUBR(Fselect_console);
	DEFSUBR(Fconsolep);
	DEFSUBR(Fconsole_live_p);
	DEFSUBR(Fconsole_type);
	DEFSUBR(Fconsole_name);
	DEFSUBR(Fconsole_connection);
	DEFSUBR(Ffind_console);
	DEFSUBR(Fget_console);
	DEFSUBR(Fdelete_console);
	DEFSUBR(Fconsole_list);
	DEFSUBR(Fconsole_device_list);
	DEFSUBR(Fconsole_enable_input);
	DEFSUBR(Fconsole_disable_input);
	DEFSUBR(Fconsole_on_window_system_p);
	DEFSUBR(Fsuspend_console);
	DEFSUBR(Fresume_console);

	DEFSUBR(Fsuspend_emacs);
	DEFSUBR(Fset_input_mode);
	DEFSUBR(Fcurrent_input_mode);

	defsymbol(&Qconsolep, "consolep");
	defsymbol(&Qconsole_live_p, "console-live-p");

	defsymbol(&Qcreate_console_hook, "create-console-hook");
	defsymbol(&Qdelete_console_hook, "delete-console-hook");

	defsymbol(&Qsuspend_hook, "suspend-hook");
	defsymbol(&Qsuspend_resume_hook, "suspend-resume-hook");
}

static const struct lrecord_description cte_description_1[] = {
	{XD_LISP_OBJECT, offsetof(console_type_entry, symbol)},
	{XD_STRUCT_PTR, offsetof(console_type_entry, meths), 1,
	 &console_methods_description},
	{XD_END}
};

static const struct struct_description cte_description = {
	sizeof(console_type_entry),
	cte_description_1
};

static const struct lrecord_description cted_description_1[] = {
	XD_DYNARR_DESC(console_type_entry_dynarr, &cte_description),
	{XD_END}
};

const struct struct_description cted_description = {
	sizeof(console_type_entry_dynarr),
	cted_description_1
};

static const struct lrecord_description console_methods_description_1[] = {
	{XD_LISP_OBJECT, offsetof(struct console_methods, symbol)},
	{XD_LISP_OBJECT, offsetof(struct console_methods, predicate_symbol)},
	{XD_LISP_OBJECT,
	 offsetof(struct console_methods, image_conversion_list)},
	{XD_END}
};

const struct struct_description console_methods_description = {
	sizeof(struct console_methods),
	console_methods_description_1
};

void console_type_create(void)
{
	the_console_type_entry_dynarr = Dynarr_new(console_type_entry);
	dump_add_root_struct_ptr(&the_console_type_entry_dynarr,
				 &cted_description);

	Vconsole_type_list = Qnil;
	staticpro(&Vconsole_type_list);

	/* Initialize the dead console type */
	INITIALIZE_CONSOLE_TYPE(dead, "dead", "console-dead-p");

	/* then reset the console-type lists, because `dead' is not really
	   a valid console type */
	Dynarr_reset(the_console_type_entry_dynarr);
	Vconsole_type_list = Qnil;
}

void reinit_vars_of_console(void)
{
	staticpro_nodump(&Vconsole_list);
	Vconsole_list = Qnil;
	staticpro_nodump(&Vselected_console);
	Vselected_console = Qnil;
}

void vars_of_console(void)
{
	reinit_vars_of_console();

	DEFVAR_LISP("create-console-hook", &Vcreate_console_hook	/*
Function or functions to call when a console is created.
One argument, the newly-created console.
This is called after the first frame has been created, but before
calling the `create-device-hook' or `create-frame-hook'.
Note that in general the console will not be selected.
									 */ );
	Vcreate_console_hook = Qnil;

	DEFVAR_LISP("delete-console-hook", &Vdelete_console_hook	/*
Function or functions to call when a console is deleted.
One argument, the to-be-deleted console.
									 */ );
	Vdelete_console_hook = Qnil;

#ifdef HAVE_WINDOW_SYSTEM
	Fprovide(intern("window-system"));
#endif
}

/* The docstrings for DEFVAR_* are recorded externally by make-docfile.  */
#if defined HAVE_BDWGC && defined EF_USE_BDWGC

#define DEFVAR_CONSOLE_LOCAL_1(lname, field_name, forward_type, magicfun) do {	\
  static const struct symbol_value_forward I_hate_C =				\
  { /* struct symbol_value_forward */						\
    { /* struct symbol_value_magic */						\
      { /* struct lcrecord_header */						\
	{ /* struct lrecord_header */						\
	  lrecord_type_symbol_value_forward, /* lrecord_type_index */		\
	  1, /* mark bit */							\
	  1, /* c_readonly bit */						\
	  1  /* lisp_readonly bit */						\
	},									\
	0, /* uid  */								\
	0  /* free */								\
      },									\
      &(console_local_flags.field_name),					\
      forward_type								\
    },										\
    magicfun									\
  };										\
										\
  {										\
    int offset = ((char *)symbol_value_forward_forward (&I_hate_C)		\
		  - (char *)&console_local_flags);				\
										\
    defvar_magic (lname, &I_hate_C);						\
										\
    *((Lisp_Object *)(offset + (char *)XCONSOLE (Vconsole_local_symbols)))	\
      = intern (lname);								\
  }										\
} while (0)

#else  /* !BDWGC */

#define DEFVAR_CONSOLE_LOCAL_1(lname, field_name, forward_type, magicfun) do {	\
  static const struct symbol_value_forward I_hate_C =				\
  { /* struct symbol_value_forward */						\
    { /* struct symbol_value_magic */						\
      { /* struct lcrecord_header */						\
	{ /* struct lrecord_header */						\
	  lrecord_type_symbol_value_forward, /* lrecord_type_index */		\
	  1, /* mark bit */							\
	  1, /* c_readonly bit */						\
	  1  /* lisp_readonly bit */						\
	},									\
	0, /* next */								\
	0, /* uid  */								\
	0  /* free */								\
      },									\
      &(console_local_flags.field_name),					\
      forward_type								\
    },										\
    magicfun									\
  };										\
										\
  {										\
    int offset = ((char *)symbol_value_forward_forward (&I_hate_C)		\
		  - (char *)&console_local_flags);				\
										\
    defvar_magic (lname, &I_hate_C);						\
										\
    *((Lisp_Object *)(offset + (char *)XCONSOLE (Vconsole_local_symbols)))	\
      = intern (lname);								\
  }										\
} while (0)

#endif	/* BDWGC */

#define DEFVAR_CONSOLE_LOCAL_MAGIC(lname, field_name, magicfun)		\
	DEFVAR_CONSOLE_LOCAL_1 (lname, field_name,			\
				SYMVAL_SELECTED_CONSOLE_FORWARD, magicfun)
#define DEFVAR_CONSOLE_LOCAL(lname, field_name)				\
	DEFVAR_CONSOLE_LOCAL_MAGIC (lname, field_name, 0)
#define DEFVAR_CONST_CONSOLE_LOCAL_MAGIC(lname, field_name, magicfun)	\
	DEFVAR_CONSOLE_LOCAL_1 (lname, field_name,			\
				SYMVAL_CONST_SELECTED_CONSOLE_FORWARD, magicfun)
#define DEFVAR_CONST_CONSOLE_LOCAL(lname, field_name)			\
	DEFVAR_CONST_CONSOLE_LOCAL_MAGIC (lname, field_name, 0)

#define DEFVAR_CONSOLE_DEFAULTS_MAGIC(lname, field_name, magicfun)	\
	DEFVAR_SYMVAL_FWD(lname, &(console_local_flags.field_name),	\
			  SYMVAL_DEFAULT_CONSOLE_FORWARD, magicfun)
#define DEFVAR_CONSOLE_DEFAULTS(lname, field_name)			\
	DEFVAR_CONSOLE_DEFAULTS_MAGIC (lname, field_name, 0)

static void nuke_all_console_slots(struct console *con, Lisp_Object zap)
{
	zero_lcrecord(con);

#define MARKED_SLOT(x)	con->x = zap
#include "conslots.h"
#undef MARKED_SLOT
}

static void common_init_complex_vars_of_console(void)
{
	/* Make sure all markable slots in console_defaults
	   are initialized reasonably, so mark_console won't choke.
	 */
	struct console *defs =
	    alloc_lcrecord_type(struct console, &lrecord_console);
	struct console *syms =
	    alloc_lcrecord_type(struct console, &lrecord_console);

	staticpro_nodump(&Vconsole_defaults);
	staticpro_nodump(&Vconsole_local_symbols);
	XSETCONSOLE(Vconsole_defaults, defs);
	XSETCONSOLE(Vconsole_local_symbols, syms);

	nuke_all_console_slots(syms, Qnil);
	nuke_all_console_slots(defs, Qnil);

	/* Set up the non-nil default values of various console slots.
	   Must do these before making the first console.
	 */
	/* #### Anything needed here? */

	{
		/*  0 means var is always local.  Default used only at creation.
		 * -1 means var is always local.  Default used only at reset and
		 *    creation.
		 * -2 means there's no lisp variable corresponding to this slot
		 *    and the default is only used at creation.
		 * -3 means no Lisp variable.  Default used only at reset and creation.
		 * >0 is mask.  Var is local if ((console->local_var_flags & mask) != 0)
		 *              Otherwise default is used.
		 *
		 * #### We don't currently ever reset console variables, so there
		 * is no current distinction between 0 and -1, and between -2 and -3.
		 */
		Lisp_Object always_local_resettable = make_int(-1);

#if 0				/* not used */
		Lisp_Object always_local_no_default = make_int(0);
		Lisp_Object resettable = make_int(-3);
#endif

		/* Assign the local-flags to the slots that have default values.
		   The local flag is a bit that is used in the console
		   to say that it has its own local value for the slot.
		   The local flag bits are in the local_var_flags slot of the
		   console.  */

		nuke_all_console_slots(&console_local_flags, make_int(-2));
		console_local_flags.defining_kbd_macro =
		    always_local_resettable;
		console_local_flags.last_kbd_macro = always_local_resettable;
		console_local_flags.prefix_arg = always_local_resettable;
		console_local_flags.default_minibuffer_frame =
		    always_local_resettable;
		console_local_flags.overriding_terminal_local_map =
		    always_local_resettable;
#ifdef HAVE_TTY
		console_local_flags.tty_erase_char = always_local_resettable;
#endif

		console_local_flags.function_key_map = make_int(1);

		/* #### Warning, 0x4000000 (that's six zeroes) is the largest number
		   currently allowable due to the XINT() handling of this value.
		   With some rearrangement you can get 4 more bits. */
	}
}

#define CONSOLE_SLOTS_SIZE (offsetof (struct console, CONSOLE_SLOTS_LAST_NAME) - offsetof (struct console, CONSOLE_SLOTS_FIRST_NAME) + sizeof (Lisp_Object))
#define CONSOLE_SLOTS_COUNT (CONSOLE_SLOTS_SIZE / sizeof (Lisp_Object))

void reinit_complex_vars_of_console(void)
{
	struct console *defs, *syms;

	common_init_complex_vars_of_console();

	defs = XCONSOLE(Vconsole_defaults);
	syms = XCONSOLE(Vconsole_local_symbols);
	memcpy(&defs->CONSOLE_SLOTS_FIRST_NAME,
	       console_defaults_saved_slots, CONSOLE_SLOTS_SIZE);
	memcpy(&syms->CONSOLE_SLOTS_FIRST_NAME,
	       console_local_symbols_saved_slots, CONSOLE_SLOTS_SIZE);
}

static const struct lrecord_description console_slots_description_1[] = {
	{XD_LISP_OBJECT_ARRAY, 0, CONSOLE_SLOTS_COUNT},
	{XD_END}
};

static const struct struct_description console_slots_description = {
	CONSOLE_SLOTS_SIZE,
	console_slots_description_1
};

void complex_vars_of_console(void)
{
	struct console *defs, *syms;

	common_init_complex_vars_of_console();

	defs = XCONSOLE(Vconsole_defaults);
	syms = XCONSOLE(Vconsole_local_symbols);
	console_defaults_saved_slots = &defs->CONSOLE_SLOTS_FIRST_NAME;
	console_local_symbols_saved_slots = &syms->CONSOLE_SLOTS_FIRST_NAME;
	dump_add_root_struct_ptr(&console_defaults_saved_slots,
				 &console_slots_description);
	dump_add_root_struct_ptr(&console_local_symbols_saved_slots,
				 &console_slots_description);

	DEFVAR_CONSOLE_DEFAULTS("default-function-key-map", function_key_map	/*
Default value of `function-key-map' for consoles that don't override it.
This is the same as (default-value 'function-key-map).
										 */ );

	DEFVAR_CONSOLE_LOCAL("function-key-map", function_key_map	/*
Keymap mapping ASCII function key sequences onto their preferred forms.
This allows Emacs to recognize function keys sent from ASCII
terminals at any point in a key sequence.

The `read-key-sequence' function replaces any subsequence bound by
`function-key-map' with its binding.  More precisely, when the active
keymaps have no binding for the current key sequence but
`function-key-map' binds a suffix of the sequence to a vector or string,
`read-key-sequence' replaces the matching suffix with its binding, and
continues with the new sequence.  See `key-binding'.

The events that come from bindings in `function-key-map' are not
themselves looked up in `function-key-map'.

For example, suppose `function-key-map' binds `ESC O P' to [f1].
Typing `ESC O P' to `read-key-sequence' would return
\[#<keypress-event f1>].  Typing `C-x ESC O P' would return
\[#<keypress-event control-X> #<keypress-event f1>].  If [f1]
were a prefix key, typing `ESC O P x' would return
\[#<keypress-event f1> #<keypress-event x>].
									 */ );

#ifdef HAVE_TTY
	/* #### Should this somehow go to TTY data?  How do we make it
	   accessible from Lisp, then?  */
	DEFVAR_CONSOLE_LOCAL("tty-erase-char", tty_erase_char	/*
The ERASE character as set by the user with stty.
When this value cannot be determined or would be meaningless (on non-TTY
consoles, for example), it is set to nil.
								 */ );
#endif

	/* While this should be const it can't be because some things
	   (i.e. edebug) do manipulate it. */
	DEFVAR_CONSOLE_LOCAL("defining-kbd-macro", defining_kbd_macro	/*
Non-nil while a keyboard macro is being defined.  Don't set this!
									 */ );

	DEFVAR_CONSOLE_LOCAL("last-kbd-macro", last_kbd_macro	/*
Last keyboard macro defined, as a vector of events; nil if none defined.
								 */ );

	DEFVAR_CONSOLE_LOCAL("prefix-arg", prefix_arg	/*
The value of the prefix argument for the next editing command.
It may be a number, or the symbol `-' for just a minus sign as arg,
or a list whose car is a number for just one or more C-U's
or nil if no argument has been specified.

You cannot examine this variable to find the argument for this command
since it has been set to nil by the time you can look.
Instead, you should use the variable `current-prefix-arg', although
normally commands can get this prefix argument with (interactive "P").
							 */ );

	DEFVAR_CONSOLE_LOCAL("default-minibuffer-frame", default_minibuffer_frame	/*
Minibufferless frames use this frame's minibuffer.

Emacs cannot create minibufferless frames unless this is set to an
appropriate surrogate.

SXEmacs consults this variable only when creating minibufferless
frames; once the frame is created, it sticks with its assigned
minibuffer, no matter what this variable is set to.  This means that
this variable doesn't necessarily say anything meaningful about the
current set of frames, or where the minibuffer is currently being
displayed.
											 */ );

	DEFVAR_CONSOLE_LOCAL("overriding-terminal-local-map", overriding_terminal_local_map	/*
Keymap that overrides all other local keymaps, for the selected console only.
If this variable is non-nil, it is used as a keymap instead of the
buffer's local map, and the minor mode keymaps and text property keymaps.
												 */ );

	/* Check for DEFVAR_CONSOLE_LOCAL without initializing the corresponding
	   slot of console_local_flags and vice-versa.  Must be done after all
	   DEFVAR_CONSOLE_LOCAL() calls. */
#define MARKED_SLOT(slot)					\
  if ((XINT (console_local_flags.slot) != -2 &&			\
	 XINT (console_local_flags.slot) != -3)			\
      != !(NILP (XCONSOLE (Vconsole_local_symbols)->slot)))	\
  abort ()
#include "conslots.h"
#undef MARKED_SLOT
}
