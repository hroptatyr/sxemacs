/* ToolTalk Interface.
   Copyright (C) 1993 Sun Microsystems, Inc.

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
Boston, MA 02111-1307, USA.

*/

/* Synched up with: Not in FSF. */

#ifndef INCLUDED_tooltalk_h_
#define INCLUDED_tooltalk_h_
#include TT_C_H_FILE

typedef struct Lisp_Tooltalk_Message Lisp_Tooltalk_Message;
DECLARE_LRECORD (tooltalk_message, Lisp_Tooltalk_Message);
#define XTOOLTALK_MESSAGE(x) XRECORD (x, tooltalk_message, Lisp_Tooltalk_Message)
#define XSETTOOLTALK_MESSAGE(x, p) XSETRECORD (x, p, tooltalk_message)
#define TOOLTALK_MESSAGEP(x) RECORDP (x, tooltalk_message)
#define CHECK_TOOLTALK_MESSAGE(x) CHECK_RECORD (x, tooltalk_message)

typedef struct Lisp_Tooltalk_Pattern Lisp_Tooltalk_Pattern;
DECLARE_LRECORD (tooltalk_pattern, Lisp_Tooltalk_Pattern);
#define XTOOLTALK_PATTERN(x) XRECORD (x, tooltalk_pattern, Lisp_Tooltalk_Pattern)
#define XSETTOOLTALK_PATTERN(x, p) XSETRECORD (x, p, tooltalk_pattern)
#define TOOLTALK_PATTERNP(x) RECORDP (x, tooltalk_pattern)
#define CHECK_TOOLTALK_PATTERN(x) CHECK_RECORD (x, tooltalk_pattern)

#define TOOLTALK_MESSAGE_KEY 100
#define TOOLTALK_PATTERN_KEY 101

#define CHECK_TOOLTALK_CONSTANT(x) do {		\
  if (!(INTP (x) || SYMBOLP (x)))		\
    dead_wrong_type_argument (Qsymbolp, (x));	\
} while (0)

#define VALID_TOOLTALK_MESSAGEP(m) \
   (m && (tt_ptr_error (m) == TT_OK))

#define VALID_TOOLTALK_PATTERNP(p) \
   (p && (tt_ptr_error (p) == TT_OK))

Lisp_Object box_tooltalk_message (Tt_message m);
Tt_message unbox_tooltalk_message (Lisp_Object msg);

extern Lisp_Object Qtooltalk_error;

#endif /* INCLUDED_tooltalk_h_ */
