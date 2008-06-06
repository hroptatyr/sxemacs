/* Definitions of marked slots in consoles
   Copyright (C) 1990, 1992, 1993 Free Software Foundation, Inc.

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


/* Synched up with: Mule 2.0, FSF 19.30. (see FSF keyboard.h.) */

/* In the declaration of the console structure, this file is included
   after defining MARKED_SLOT(x) to be Lisp_Object x; i.e. just a slot
   definition.  In the garbage collector this file is included after
   defining MARKED_SLOT(x) to be mark_object(console->x). */

#ifndef CONSOLE_SLOTS_FIRST_NAME
#define CONSOLE_SLOTS_FIRST_NAME name
#endif

    /* Name of this console, for resourcing and printing purposes.
       If not explicitly given, it's initialized in a console-specific
       manner. */
MARKED_SLOT(name);

    /* What this console is connected to */
MARKED_SLOT(connection);

    /* A canonical name for the connection that is used to determine
       whether create_console() is being called on an existing console. */
MARKED_SLOT(canon_connection);

    /* List of devices on this console.  */
MARKED_SLOT(device_list);

    /* Currently selected device.  */
MARKED_SLOT(selected_device);

    /* Most-recently-selected non-minibuffer-only frame.  Always
       the same as the selected frame, unless that's a minibuffer-only
       frame. */
MARKED_SLOT(last_nonminibuf_frame);

    /* If non-nil, a keymap that overrides all others but applies only to
       this console.  Lisp code that uses this instead of calling next-event
       can effectively wait for input in the any-console state, and hence
       avoid blocking out the other consoles.  See universal-argument in
       lisp/simple.el for an example.

       #### This comes from FSF Emacs; but there's probably a better
       solution that involves making next-event itself work over all
       consoles. */
MARKED_SLOT(overriding_terminal_local_map);

    /* Last command executed by the editor command loop, not counting
       commands that set the prefix argument.  */
MARKED_SLOT(last_command);

    /* The prefix argument for the next command, in raw form.  */
MARKED_SLOT(prefix_arg);

    /* Where information about a partially completed key sequence
       is kept.  */
MARKED_SLOT(command_builder);

    /* Non-nil while a kbd macro is being defined.  */
MARKED_SLOT(defining_kbd_macro);

    /* This is a lisp vector, which contains the events of the keyboard macro
       currently being read.  It is reallocated when the macro gets too large.
     */
MARKED_SLOT(kbd_macro_builder);

    /* Last anonymous kbd macro defined.  */
MARKED_SLOT(last_kbd_macro);

#ifdef HAVE_TTY
    /* ERASE character from stty settings.  */
MARKED_SLOT(tty_erase_char);
#endif

    /* Minibufferless frames on this console use this frame's minibuffer.  */
MARKED_SLOT(default_minibuffer_frame);

    /* Keymap mapping ASCII function key sequences onto their preferred forms.
       Initialized by the terminal-specific lisp files.  */
MARKED_SLOT(function_key_map);

#ifndef CONSOLE_SLOTS_LAST_NAME
#define CONSOLE_SLOTS_LAST_NAME function_key_map
#endif
