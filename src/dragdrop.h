/* Definitions for the new drag and drop model;
   created 03-may-98 by Oliver Graf <ograf@fga.de>
   Copyright (C) 1998 Oliver Graf

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

#ifndef INCLUDED_dragdrop_h_
#define INCLUDED_dragdrop_h_

/* Drag'n'Drop data types known by XEmacs */
extern Lisp_Object Qdragdrop_MIME;
extern Lisp_Object Qdragdrop_URL;

/* External defined functions to handle Drag'n'Drop */
extern Lisp_Object Qdragdrop_drop_dispatch;

/* some utility functions */
char *dnd_url_hexify_string (const char *s, const char *m);

/* emacs interface */
void syms_of_dragdrop (void);

#endif /* INCLUDED_dragdrop_h_ */
