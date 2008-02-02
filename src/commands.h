/* Definitions needed by most editing commands.
   Copyright (C) 1985-1994 Free Software Foundation, Inc.

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

#ifndef INCLUDED_commands_h_
#define INCLUDED_commands_h_

#if 0				/* FSFmacs */
#define Ctl(c) ((c)&037)
#endif

/* bunches of FSF keymap and minibuffer stuff not here (in Lisp) */

extern Lisp_Object Vlast_command_event;

extern Lisp_Object Vlast_input_event;

/* These next two for compatibility; they are V... because they can be
   nil. (Many FSFmacs equivalent variables incorrectly omit the V
   even though they are Lisp_Objects.) */
/* Last character of last key sequence.  */
extern Lisp_Object Vlast_command_char;

extern Lisp_Object Vlast_input_char;

#if 0				/* FSFmacs */
/* Last input character read as a command, not counting menus
   reached by the mouse.  */
extern Lisp_Object Vlast_nonmenu_event;
#endif

#if 0				/* Local to event-stream.c */
/* List of command events to be re-read, or Qnil.  */
extern Lisp_Object Vunread_command_events;
#endif

#if 0				/* FSFmacs */
/* Command char event to be re-read, or -1 if none.
   Setting this is obsolete, but some things should still check it.  */
extern int unread_command_char;
#endif

/* Last command executed by the editor command loop, not counting
   commands that set the prefix argument.  */

extern Lisp_Object Vlast_command;

/* The command being executed by the command loop.
   Commands may set this, and the value set will be copied into
   Vlast_command instead of the actual command.  */
extern Lisp_Object Vthis_command;

#if 0				/* FSFmacs */
/* If not Qnil, this is a switch-frame event which we decided to put
   off until the end of a key sequence.  This should be read as the
   next command input, after any Vunread_command_events.

   read_key_sequence uses this to delay switch-frame events until the
   end of the key sequence; Fread_char uses it to put off switch-frame
   events until a non-ASCII event is acceptable as input.  */
extern Lisp_Object unread_switch_frame;
#endif

#if 0				/* Local to event-stream.c */
/* The value of point when the last command was executed.  */
extern int last_point_position;

/* The buffer that was current when the last command was started.  */
extern Lisp_Object last_point_position_buffer;
#endif

/* This is so incredibly losing that it's been completely eliminated
   from the code.  Trust me, there are cleaner, safer ways of
   achieving the same functionality (e.g. use select()). */
/* extern int immediate_quit;	    Nonzero means ^G can quit instantly */

/* Nonzero if input is coming from the keyboard */

#define INTERACTIVE (NILP (Vexecuting_macro) && !noninteractive)

/* Set this nonzero to force reconsideration of modeline. */

extern int modeline_changed;

extern Lisp_Object recent_keys_ring;
extern int recent_keys_ring_index;

/* #ifndef LISP_COMMAND_LOOP */
extern Lisp_Object Vtop_level;
/* #else */
extern Lisp_Object Vcommand_loop;
/* #endif */
DECLARE_DOESNT_RETURN(initial_command_loop(Lisp_Object));
Lisp_Object call_command_loop(Lisp_Object catch_errors);
extern Fixnum command_loop_level;

extern Lisp_Object Vkeyboard_translate_table;
extern Lisp_Object Vlast_input_time;
extern Lisp_Object Vcurrent_mouse_event;

extern int zmacs_regions;
extern int zmacs_region_active_p;
extern int zmacs_region_stays;
void zmacs_update_region(void);
void zmacs_deactivate_region(void);
Lisp_Object zmacs_region_buffer(void);

extern Lisp_Object Vthis_command_keys;	/* event-stream.c */

#endif				/* INCLUDED_commands_h_ */
