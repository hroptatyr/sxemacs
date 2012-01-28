/* TTY-specific Lisp objects.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Ben Wing

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


/* Synched up with:  Not in FSF. */

#ifndef INCLUDED_objects_tty_h_
#define INCLUDED_objects_tty_h_

#include "ui/objects.h"

struct tty_color_instance_data {
	Lisp_Object symbol;	/* so we don't have to constantly call Fintern() */
};

#define TTY_COLOR_INSTANCE_DATA(c)				\
  ((struct tty_color_instance_data *)((c)->data))

#define COLOR_INSTANCE_TTY_SYMBOL(c) (TTY_COLOR_INSTANCE_DATA (c)->symbol)

struct tty_font_instance_data {
	Lisp_Object charset;
};

#define TTY_FONT_INSTANCE_DATA(c)				\
  ((struct tty_font_instance_data *) (c)->data)

#define FONT_INSTANCE_TTY_CHARSET(c) (TTY_FONT_INSTANCE_DATA (c)->charset)

extern Lisp_Object Vtty_color_alist;


EXFUN(Ffind_tty_color,3);

#endif				/* INCLUDED_objects_tty_h_ */
