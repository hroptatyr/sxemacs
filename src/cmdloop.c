/* Editor command loop.
   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996 Ben Wing.

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

/* Synched up with: Mule 2.0.  Not synched with FSF.
   This was renamed from keyboard.c.  However, it only contains the
   command-loop stuff from FSF's keyboard.c; all the rest is in
   event*.c, console.c, or signal.c. */

/* #### This module purports to separate out the command-loop stuff
   from event-stream.c, but it doesn't really.  Perhaps this file
   should just be merged into event-stream.c, given its shortness. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "commands.h"
#include "frame.h"
#include "events.h"
#include "window.h"

/* Current depth in recursive edits.  */
Fixnum command_loop_level;

#ifndef LISP_COMMAND_LOOP
/* Form to evaluate (if non-nil) when Emacs is started.  */
Lisp_Object Vtop_level;
#else
/* Function to call to evaluate to read and process events.  */
Lisp_Object Vcommand_loop;
#endif /* LISP_COMMAND_LOOP */

Lisp_Object Venter_window_hook, Vleave_window_hook;

/* The error handler.  */
Lisp_Object Qcommand_error;

/* The emergency error handler, before we're ready.  */
Lisp_Object Qreally_early_error_handler;

/* Variable defined in Lisp. */
Lisp_Object Qerrors_deactivate_region;

Lisp_Object Qtop_level;

static Lisp_Object command_loop_1 (Lisp_Object dummy);
EXFUN (Fcommand_loop_1, 0);

/* There are two possible command loops -- one written entirely in
   C and one written mostly in Lisp, except stuff written in C for
   speed.  The advantage of the Lisp command loop is that the user
   can specify their own command loop to use by changing the variable
   `command-loop'.  Its disadvantage is that it's slow. */

static Lisp_Object
default_error_handler (Lisp_Object data)
{
  int speccount = specpdl_depth ();

  /* None of this is invoked, normally.  This code is almost identical
     to the `command-error' function, except `command-error' does cool
     tricks with sounds.  This function is a fallback, invoked if
     command-error is unavailable.  */

  Fding (Qnil, Qnil, Qnil);

  if (!NILP (Fboundp (Qerrors_deactivate_region))
      && !NILP (Fsymbol_value (Qerrors_deactivate_region)))
    zmacs_deactivate_region ();
  Fdiscard_input ();
  specbind (Qinhibit_quit, Qt);
  Vstandard_output = Qt;
  Vstandard_input = Qt;
  Vexecuting_macro = Qnil;
  Fset (intern ("last-error"), data);
  clear_echo_area (selected_frame (), Qnil, 0);
  Fdisplay_error (data, Qt);
  check_quit (); /* make Vquit_flag accurate */
  Vquit_flag = Qnil;
  return (unbind_to (speccount, Qt));
}

DEFUN ("really-early-error-handler", Freally_early_error_handler, 1, 1, 0, /*
You should almost certainly not be using this.
*/
       (x))
{
  /* This is an error handler used when we're running temacs and when
     we're in the early stages of XEmacs.  No errors ought to be
     occurring in those cases (or they ought to be trapped and
     dealt with elsewhere), but if an error slips through, we need
     to deal with it.  We could write this function in Lisp (and it
     used to be this way, at the beginning of loadup.el), but we do
     it this way in case an error occurs before we get to loading
     loadup.el.  Note that there is also an `early-error-handler',
     used in startup.el to catch more reasonable errors that
     might occur during startup if the sysadmin or whoever fucked
     up.  This function is more conservative in what it does
     and is used only as a last resort, indicating that the
     programmer himself fucked up somewhere. */
  stderr_out ("*** Error in XEmacs initialization");
  Fprint (x, Qexternal_debugging_output);
  stderr_out ("*** Backtrace\n");
  Fbacktrace (Qexternal_debugging_output, Qt);
  stderr_out ("*** Killing XEmacs\n");
#ifdef HAVE_MS_WINDOWS
  Fmswindows_message_box (build_string ("Initialization error"),
			  Qnil, Qnil);
#endif
  return Fkill_emacs (make_int (-1));
}


/**********************************************************************/
/*                     Command-loop (in C)                            */
/**********************************************************************/

#ifndef LISP_COMMAND_LOOP

/* The guts of the command loop are in command_loop_1().  This function
   doesn't catch errors, though -- that's the job of command_loop_2(),
   which is a condition-case wrapper around command_loop_1().
   command_loop_1() never returns, but may get thrown out of.

   When an error occurs, cmd_error() is called, which usually
   invokes the Lisp error handler in `command-error'; however,
   a default error handler is provided if `command-error' is nil
   (e.g. during startup).  The purpose of the error handler is
   simply to display the error message and do associated cleanup;
   it does not need to throw anywhere.  When the error handler
   finishes, the condition-case in command_loop_2() will finish and
   command_loop_2() will reinvoke command_loop_1().

   command_loop_2() is invoked from three places: from
   initial_command_loop() (called from main() at the end of
   internal initialization), from the Lisp function `recursive-edit',
   and from call_command_loop().

   call_command_loop() is called when a macro is started and when the
   minibuffer is entered; normal termination of the macro or
   minibuffer causes a throw out of the recursive command loop. (To
   'execute-kbd-macro for macros and 'exit for minibuffers.  Note also
   that the low-level minibuffer-entering function,
   `read-minibuffer-internal', provides its own error handling and
   does not need command_loop_2()'s error encapsulation; so it tells
   call_command_loop() to invoke command_loop_1() directly.)

   Note that both read-minibuffer-internal and recursive-edit set
   up a catch for 'exit; this is why `abort-recursive-edit', which
   throws to this catch, exits out of either one.

   initial_command_loop(), called from main(), sets up a catch
   for 'top-level when invoking command_loop_2(), allowing functions
   to throw all the way to the top level if they really need to.
   Before invoking command_loop_2(), initial_command_loop() calls
   top_level_1(), which handles all of the startup stuff (creating
   the initial frame, handling the command-line options, loading
   the user's .emacs file, etc.).  The function that actually does this
   is in Lisp and is pointed to by the variable `top-level';
   normally this function is `normal-top-level'.  top_level_1() is
   just an error-handling wrapper similar to command_loop_2().
   Note also that initial_command_loop() sets up a catch for 'top-level
   when invoking top_level_1(), just like when it invokes
   command_loop_2(). */


static Lisp_Object
cmd_error (Lisp_Object data, Lisp_Object dummy)
{
  /* This function can GC */
  check_quit (); /* make Vquit_flag accurate */
  Vquit_flag = Qnil;

  any_console_state ();

  if (!NILP (Ffboundp (Qcommand_error)))
    return call1 (Qcommand_error, data);

  return default_error_handler (data);
}

static Lisp_Object
top_level_1 (Lisp_Object dummy)
{
  /* This function can GC */
  /* On entry to the outer level, run the startup file */
  if (!NILP (Vtop_level))
    condition_case_1 (Qerror, Feval, Vtop_level, cmd_error, Qnil);
#if 1
  else
    {
      message ("\ntemacs can only be run in -batch mode.");
      noninteractive = 1; /* prevent things under kill-emacs from blowing up */
      Fkill_emacs (make_int (-1));
    }
#else
  else if (purify_flag)
    message ("Bare impure Emacs (standard Lisp code not loaded)");
  else
    message ("Bare Emacs (standard Lisp code not loaded)");
#endif

  return Qnil;
}

/* Here we catch errors in execution of commands within the
   editing loop, and reenter the editing loop.
   When there is an error, cmd_error runs and the call
   to condition_case_1() returns. */

/* Avoid confusing the compiler. A helper function for command_loop_2 */
static DOESNT_RETURN
command_loop_3 (void)
{
#ifdef LWLIB_MENUBARS_LUCID
  extern int in_menu_callback;  /* defined in menubar-x.c */
#endif /* LWLIB_MENUBARS_LUCID */

#ifdef LWLIB_MENUBARS_LUCID
  /*
   * #### Fix the menu code so this isn't necessary.
   *
   * We cannot allow the lwmenu code to be reentered, because the
   * code is not written to be reentrant and will crash.  Therefore
   * paths from the menu callbacks back into the menu code have to
   * be blocked.  Fnext_event is the normal path into the menu code,
   * but waiting to signal an error there is too late in case where
   * a new command loop has been started.  The error will be caught
   * and Fnext_event will be called again, looping forever.  So we
   * signal an error here to avoid the loop.
   */
  if (in_menu_callback)
    error ("Attempt to enter command_loop_3 inside menu callback");
#endif /* LWLIB_MENUBARS_LUCID */
  /* This function can GC */
  for (;;)
    {
      condition_case_1 (Qerror, command_loop_1, Qnil, cmd_error, Qnil);
      /* #### wrong with selected-console? */
      /* See command in initial_command_loop about why this value
	 is 0. */
      reset_this_command_keys (Vselected_console, 0);
    }
}

static Lisp_Object
command_loop_2 (Lisp_Object dummy)
{
  command_loop_3(); /* doesn't return */
  return Qnil;
}

/* This is called from emacs.c when it's done with initialization. */

DOESNT_RETURN
initial_command_loop (Lisp_Object load_me)
{
  /* This function can GC */
  if (!NILP (load_me))
    Vtop_level = list2 (Qload, load_me);

  /* First deal with startup and command-line arguments.  A throw
     to 'top-level gets us back here directly (does this ever happen?).
     Otherwise, this function will return normally when all command-
     line arguments have been processed, the user's initialization
     file has been read in, and the first frame has been created. */
  internal_catch (Qtop_level, top_level_1, Qnil, 0);

  /* If an error occurred during startup and the initial console
     wasn't created, then die now (the error was already printed out
     on the terminal device). */
  if (!noninteractive &&
      (!CONSOLEP (Vselected_console) ||
       CONSOLE_STREAM_P (XCONSOLE (Vselected_console))))
    Fkill_emacs (make_int (-1));

  /* End of -batch run causes exit here. */
  if (noninteractive)
    Fkill_emacs (Qt);

  for (;;)
    {
      command_loop_level = 0;
      MARK_MODELINE_CHANGED;
      /* Now invoke the command loop.  It never returns; however, a
	 throw to 'top-level will place us at the end of this loop. */
      internal_catch (Qtop_level, command_loop_2, Qnil, 0);
      /* #### wrong with selected-console? */
      /* We don't actually call clear_echo_area() here, partially
	 at least because that runs Lisp code and it may be unsafe
	 to do so -- we are outside of the normal catches for
	 errors and such. */
      reset_this_command_keys (Vselected_console, 0);
    }
}

/* This function is invoked when a macro or minibuffer starts up.
   Normal termination of the macro or minibuffer causes a throw past us.
   See the comment above.

   Note that this function never returns (but may be thrown out of). */

Lisp_Object
call_command_loop (Lisp_Object catch_errors)
{
  /* This function can GC */
  if (NILP (catch_errors))
    return (command_loop_1 (Qnil));
  else
    return (command_loop_2 (Qnil));
}

static Lisp_Object
recursive_edit_unwind (Lisp_Object buffer)
{
  if (!NILP (buffer))
    Fset_buffer (buffer);

  command_loop_level--;
  MARK_MODELINE_CHANGED;

  return Qnil;
}

DEFUN ("recursive-edit", Frecursive_edit, 0, 0, "", /*
Invoke the editor command loop recursively.
To get out of the recursive edit, a command can do `(throw 'exit nil)';
that tells this function to return.
Alternately, `(throw 'exit t)' makes this function signal an error.
*/
       ())
{
  /* This function can GC */
  Lisp_Object val;
  int speccount = specpdl_depth ();

  command_loop_level++;
  MARK_MODELINE_CHANGED;

  record_unwind_protect (recursive_edit_unwind,
			 ((current_buffer
                           != XBUFFER (XWINDOW (Fselected_window
						(Qnil))->buffer))
                          ? Fcurrent_buffer ()
                          : Qnil));

  specbind (Qstandard_output, Qt);
  specbind (Qstandard_input, Qt);

  val = internal_catch (Qexit, command_loop_2, Qnil, 0);

  if (EQ (val, Qt))
    /* Turn abort-recursive-edit into a quit. */
    Fsignal (Qquit, Qnil);

  return unbind_to (speccount, Qnil);
}

#endif /* !LISP_COMMAND_LOOP */


/**********************************************************************/
/*             Alternate command-loop (largely in Lisp)               */
/**********************************************************************/

#ifdef LISP_COMMAND_LOOP

static Lisp_Object
load1 (Lisp_Object name)
{
  /* This function can GC */
  call4 (Qload, name, Qnil, Qt, Qnil);
  return (Qnil);
}

/* emergency backups for cold-load-stream use */
static Lisp_Object
cold_load_command_error (Lisp_Object datum, Lisp_Object ignored)
{
  /* This function can GC */
  check_quit (); /* make Vquit_flag accurate */
  Vquit_flag = Qnil;

  return default_error_handler (datum);
}

static Lisp_Object
cold_load_command_loop (Lisp_Object dummy)
{
  /* This function can GC */
  return (condition_case_1 (Qt,
                            command_loop_1, Qnil,
                            cold_load_command_error, Qnil));
}

Lisp_Object
call_command_loop (Lisp_Object catch_errors)
{
  /* This function can GC */
  reset_this_command_keys (Vselected_console, 0); /* #### bleagh */

 loop:
  for (;;)
    {
      if (NILP (Vcommand_loop))
	break;
      call1 (Vcommand_loop, catch_errors);
    }

  /* This isn't a "correct" definition, but you're pretty hosed if
     you broke "command-loop" anyway */
  /* #### not correct with Vselected_console */
  XCONSOLE (Vselected_console)->prefix_arg = Qnil;
  if (NILP (catch_errors))
    Fcommand_loop_1 ();
  else
    internal_catch (Qtop_level,
                    cold_load_command_loop, Qnil, 0);
  goto loop;
  return Qnil;
}

static Lisp_Object
initial_error_handler (Lisp_Object datum, Lisp_Object ignored)
{
  /* This function can GC */
  Vcommand_loop =  Qnil;
  Fding (Qnil, Qnil, Qnil);

  if (CONSP (datum) && EQ (XCAR (datum), Qquit))
    /* Don't bother with the message */
    return (Qt);

  message ("Error in command-loop!!");
  Fset (intern ("last-error"), datum); /* #### Better/different name? */
  Fsit_for (make_int (2), Qnil);
  cold_load_command_error (datum, Qnil);
  return (Qt);
}

DOESNT_RETURN
initial_command_loop (Lisp_Object load_me)
{
  /* This function can GC */
  if (!NILP (load_me))
    {
      if (!NILP (condition_case_1 (Qt, load1, load_me,
                                   initial_error_handler, Qnil)))
	Fkill_emacs (make_int (-1));
    }

  for (;;)
    {
      command_loop_level = 0;
      MARK_MODELINE_CHANGED;

      condition_case_1 (Qt,
			call_command_loop, Qtop_level,
			initial_error_handler, Qnil);
    }
}

#endif /* LISP_COMMAND_LOOP */


/**********************************************************************/
/*                     Guts of command loop                           */
/**********************************************************************/

static Lisp_Object
command_loop_1 (Lisp_Object dummy)
{
  /* This function can GC */
  /* #### not correct with Vselected_console */
  XCONSOLE (Vselected_console)->prefix_arg = Qnil;
  return (Fcommand_loop_1 ());
}

/* This is the actual command reading loop, sans error-handling
   encapsulation.  This is used for both the C and Lisp command
   loops.  Originally this function was written in Lisp when
   the Lisp command loop was used, but it was too slow that way.

   Under the C command loop, this function will never return
   (although someone might throw past it).  Under the Lisp
   command loop, this will return only when the user specifies
   a new command loop by changing the command-loop variable. */

DEFUN ("command-loop-1", Fcommand_loop_1, 0, 0, 0, /*
Invoke the internals of the canonical editor command loop.
Don't call this unless you know what you're doing.
*/
       ())
{
  /* This function can GC */
  Lisp_Object event = Fmake_event (Qnil, Qnil);
  Lisp_Object old_loop = Qnil;
  struct gcpro gcpro1, gcpro2;
  int was_locked = in_single_console_state ();
  GCPRO2 (event, old_loop);

  /* cancel_echoing (); */
  /* This magically makes single character keyboard macros work just
     like the real thing.  This is slightly bogus, but it's in here for
     compatibility with Emacs 18.  It's not even clear what the "right
     thing" is. */
  if (!((STRINGP (Vexecuting_macro) || VECTORP (Vexecuting_macro))
	&& XINT (Flength (Vexecuting_macro)) == 1))
    Vlast_command = Qt;

#ifndef LISP_COMMAND_LOOP
  while (1)
#else
  old_loop = Vcommand_loop;
  while (EQ (Vcommand_loop, old_loop))
#endif /* LISP_COMMAND_LOOP */
    {
      /* If focus_follows_mouse, make sure the frame with window manager
         focus is selected. */
      if (focus_follows_mouse)
        investigate_frame_change ();

      /* Make sure the current window's buffer is selected.  */
      {
	Lisp_Object selected_window = Fselected_window (Qnil);

	if (!NILP (selected_window) &&
	    (XBUFFER (XWINDOW (selected_window)->buffer) != current_buffer))
	  {
	    set_buffer_internal (XBUFFER (XWINDOW (selected_window)->buffer));
	  }
      }

#if 0 /* What's wrong with going through ordinary procedure of quit?
         quitting here leaves overriding-terminal-local-map
         when you type C-u C-u C-g. */
      /* If ^G was typed before we got here (that is, before emacs was
	 idle and waiting for input) then we treat that as an interrupt. */
      QUIT;
#endif

      /* If minibuffer on and echo area in use, wait 2 sec and redraw
	 minibuffer.  Treat a ^G here as a command, not an interrupt.
       */
      if (minibuf_level > 0 && echo_area_active (selected_frame ()))
	{
	  /* Bind dont_check_for_quit to 1 so that C-g gets read in
	     rather than quitting back to the minibuffer.  */
	  int count = specpdl_depth ();
	  begin_dont_check_for_quit ();
	  Fsit_for (make_int (2), Qnil);
	  clear_echo_area (selected_frame (), Qnil, 0);
	  unbind_to (count, Qnil);
	}

      Fnext_event (event, Qnil);
      /* If ^G was typed while emacs was reading input from the user, then
	 Fnext_event() will have read it as a normal event and
	 next_event_internal() will have set Vquit_flag.  We reset this
	 so that the ^G is treated as just another key.  This is strange,
	 but it is what emacs 18 did.

	 Do not call check_quit() here. */
      Vquit_flag = Qnil;
      Fdispatch_event (event);

      if (!was_locked)
	any_console_state ();
#if (defined (_MSC_VER) 			\
     || defined (__SUNPRO_C)			\
     || defined (__SUNPRO_CC)			\
     || (defined (DEC_ALPHA)			\
	 && defined (OSF1)))
      if (0) return Qnil; /* Shut up compiler */
#endif
    }
#ifdef LISP_COMMAND_LOOP
  UNGCPRO;
  return Qnil;
#endif
}


/**********************************************************************/
/*                         Initialization                             */
/**********************************************************************/

void
syms_of_cmdloop (void)
{
  defsymbol (&Qcommand_error, "command-error");
  defsymbol (&Qreally_early_error_handler, "really-early-error-handler");
  defsymbol (&Qtop_level, "top-level");
  defsymbol (&Qerrors_deactivate_region, "errors-deactivate-region");

#ifndef LISP_COMMAND_LOOP
  DEFSUBR (Frecursive_edit);
#endif
  DEFSUBR (Freally_early_error_handler);
  DEFSUBR (Fcommand_loop_1);
}

void
vars_of_cmdloop (void)
{
  DEFVAR_INT ("command-loop-level", &command_loop_level /*
Number of recursive edits in progress.
*/ );
  command_loop_level = 0;

  DEFVAR_LISP ("disabled-command-hook", &Vdisabled_command_hook /*
Value is called instead of any command that is disabled,
i.e. has a non-nil `disabled' property.
*/ );
  Vdisabled_command_hook = intern ("disabled-command-hook");

  DEFVAR_LISP ("leave-window-hook", &Vleave_window_hook /*
Not yet implemented.
*/ );
  Vleave_window_hook = Qnil;

  DEFVAR_LISP ("enter-window-hook", &Venter_window_hook /*
Not yet implemented.
*/ );
  Venter_window_hook = Qnil;

#ifndef LISP_COMMAND_LOOP
  DEFVAR_LISP ("top-level", &Vtop_level /*
Form to evaluate when Emacs starts up.
Useful to set before you dump a modified Emacs.
*/ );
  Vtop_level = Qnil;
#else
  DEFVAR_LISP ("command-loop", &Vcommand_loop /*
Function or one argument to call to read and process keyboard commands.
The passed argument specifies whether or not to handle errors.
*/ );
  Vcommand_loop = Qnil;
#endif /* LISP_COMMAND_LOOP */
}
