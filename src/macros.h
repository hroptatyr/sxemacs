/* Definitions for keyboard macro interpretation in XEmacs.
   Copyright (C) 1985, 1992, 1993 Free Software Foundation, Inc.

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

#ifndef INCLUDED_macros_h_
#define INCLUDED_macros_h_

/* Index of next character to fetch from that macro */

extern int executing_macro_index;

void store_kbd_macro_event(Lisp_Object event);
void pop_kbd_macro_event(Lisp_Object event);
void finalize_kbd_macro_chars(struct console *con);

EXFUN(Fexecute_kbd_macro, 2);

extern Lisp_Object Vexecuting_macro;

#endif				/* INCLUDED_macros_h_ */
