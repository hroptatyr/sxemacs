/* TTY-specific Lisp objects.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Ben Wing

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

/* Synched up with:  Not in FSF. */

#ifndef INCLUDED_objects_tty_h_
#define INCLUDED_objects_tty_h_

#include "objects.h"

struct tty_color_instance_data
{
  Lisp_Object symbol; /* so we don't have to constantly call Fintern() */
};

#define TTY_COLOR_INSTANCE_DATA(c) 				\
  ((struct tty_color_instance_data *) (c)->data)

#define COLOR_INSTANCE_TTY_SYMBOL(c) (TTY_COLOR_INSTANCE_DATA (c)->symbol)

struct tty_font_instance_data
{
  Lisp_Object charset;
};

#define TTY_FONT_INSTANCE_DATA(c) 				\
  ((struct tty_font_instance_data *) (c)->data)

#define FONT_INSTANCE_TTY_CHARSET(c) (TTY_FONT_INSTANCE_DATA (c)->charset)

extern Lisp_Object Vtty_color_alist, Vtty_dynamic_color_bg;
extern Lisp_Object Vtty_dynamic_color_fg;

#endif /* INCLUDED_objects_tty_h_ */
