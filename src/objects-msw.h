/* mswindows-specific Lisp objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1997, Jonathan Harris.

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

/* Authorship:

   Ultimately based on FSF.
   Rewritten by Ben Wing.
   Rewritten for mswindows by Jonathan Harris, November 1997 for 21.0.
 */


#ifndef INCLUDED_objects_msw_h_
#define INCLUDED_objects_msw_h_

#include "objects.h"

struct mswindows_color_instance_data
{
  COLORREF  color;
};

#define MSWINDOWS_COLOR_INSTANCE_DATA(c)	\
	((struct mswindows_color_instance_data *) (c)->data)
#define COLOR_INSTANCE_MSWINDOWS_COLOR(c)	\
	(MSWINDOWS_COLOR_INSTANCE_DATA (c)->color)

/* The four HFONTS are for the 4 (underlined, strikethrough)
   combinations.  Only the one at index 0, neither underlined nor
   struk through is created with the font instance. Other fonts are
   created as necessary during redisplay, using the one at index 0
   as prototype */
#define MSWINDOWS_NUM_FONT_VARIANTS 4
struct mswindows_font_instance_data
{
  HFONT hfont [MSWINDOWS_NUM_FONT_VARIANTS];
};

#define MSWINDOWS_FONT_INSTANCE_DATA(c)	\
	((struct mswindows_font_instance_data *) (c)->data)

#define FONT_INSTANCE_MSWINDOWS_HFONT_I(c,i) \
	(MSWINDOWS_FONT_INSTANCE_DATA(c)->hfont[(i)])

#define FONT_INSTANCE_MSWINDOWS_HFONT_VARIANT(c,under,strike) \
	FONT_INSTANCE_MSWINDOWS_HFONT_I (c, (!!(strike)<<1)|!!(under))

/* If font creation during redisplay fails, then the following
   value is used to prevent future attempts to create this font.
   Redisplay uses the "main" font when encounters this value */
#define MSWINDOWS_BAD_HFONT ((HFONT)INVALID_HANDLE_VALUE)

HFONT mswindows_get_hfont (Lisp_Font_Instance* f, int under, int strike);

Lisp_Object mswindows_color_to_string (COLORREF color);

#endif /* INCLUDED_objects_msw_h_ */
