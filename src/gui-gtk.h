/* General GUI code -- X-specific header file.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1996 Ben Wing.

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

#ifndef _XEMACS_GUI_GTK_H_
#define _XEMACS_GUI_GTK_H_

#include <gtk/gtk.h>

typedef unsigned int GUI_ID;
extern GUI_ID new_gui_id (void);

extern void gcpro_popup_callbacks (GUI_ID id, Lisp_Object data);
extern void ungcpro_popup_callbacks (GUI_ID id);
extern Lisp_Object get_gcpro_popup_callbacks (GUI_ID id);

#endif /* _XEMACS_GUI_GTK_H_ */
