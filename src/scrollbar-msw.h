/* Define mswindows specific scrollbar instance.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.

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

#ifndef INCLUDED_scrollbar_msw_h_
#define INCLUDED_scrollbar_msw_h_

#if defined (HAVE_MS_WINDOWS) && defined (HAVE_SCROLLBARS)

#include "scrollbar.h"

struct mswindows_scrollbar_data
{
  HWND hwnd;
  
  char *name;

  SCROLLINFO info;

  int scrollbar_x, scrollbar_y;
  int scrollbar_width, scrollbar_height;
  int size;
};

#define SCROLLBAR_MSW_DATA(i) ((struct mswindows_scrollbar_data *) ((i)->scrollbar_data))

#define SCROLLBAR_MSW_HANDLE(i) (SCROLLBAR_MSW_DATA (i)->hwnd)
#define SCROLLBAR_MSW_NAME(i) (SCROLLBAR_MSW_DATA (i)->name)
#define SCROLLBAR_MSW_INFO(i) (SCROLLBAR_MSW_DATA (i)->info)
#define SCROLLBAR_MSW_X(i) (SCROLLBAR_MSW_DATA (i)->x)
#define SCROLLBAR_MSW_Y(i) (SCROLLBAR_MSW_DATA (i)->y)
#define SCROLLBAR_MSW_WIDTH(i) (SCROLLBAR_MSW_DATA (i)->width)
#define SCROLLBAR_MSW_HEIGHT(i) (SCROLLBAR_MSW_DATA (i)->height)
#define SCROLLBAR_MSW_SIZE(i) (SCROLLBAR_MSW_DATA (i)->size)
     /*
void mswindows_update_frame_scrollbars (struct frame *f);
void mswindows_set_scrollbar_pointer (struct frame *f, Lisp_Object cursor);
EMACS_INT mswindows_window_is_scrollbar (struct frame *f, Window win);
     */

void mswindows_handle_scrollbar_event (HWND hwnd, int code, int pos);
int mswindows_handle_mousewheel_event (Lisp_Object frame, int keys, int delta,
				       POINTS where);

#endif /* HAVE_MS_WINDOWS and HAVE_SCROLLBARS */
#endif /* INCLUDED_scrollbar_msw_h_ */
