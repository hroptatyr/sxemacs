/* Balloon Help
   Copyright (c) 1997 Douglas Keller

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

#ifndef INCLUDED_balloon_help_h_
#define INCLUDED_balloon_help_h_

#include "xintrinsic.h"

void balloon_help_create(Display * dpy,
			 Pixel fg, Pixel bg, Pixel shine, Pixel shadow,
			 XFontStruct * font);
void balloon_help_set_delay(unsigned long milliseconds);
void balloon_help_show(const char *text);
void balloon_help_hide(void);
void balloon_help_move_to_pointer(void);

#endif				/* INCLUDED_balloon_help_h_ */
