/* Balloon Help
   Copyright (c) 1997 Douglas Keller

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

/* Synched up with: Not in FSF. */

#ifndef INCLUDED_balloon_help_h_
#define INCLUDED_balloon_help_h_

#include "xintrinsic.h"

void balloon_help_create (Display* dpy,
			  Pixel fg, Pixel bg, Pixel shine, Pixel shadow,
			  XFontStruct* font);
void balloon_help_set_delay (unsigned long milliseconds);
void balloon_help_show (const char* text);
void balloon_help_hide (void);
void balloon_help_move_to_pointer (void);

#endif /* INCLUDED_balloon_help_h_ */
