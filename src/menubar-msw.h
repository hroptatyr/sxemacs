/* Implements an elisp-programmable menubar -- Win32
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
   Copyright (C) 1997 Kirill M. Katsnelson <kkm@kis.ru>

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

/* Author:
   Initially written by kkm 12/24/97,
   */

#ifndef INCLUDED_menubar_msw_h_
#define INCLUDED_menubar_msw_h_


#ifdef HAVE_MENUBARS

/* Message handlers. Called from window procedure */
Lisp_Object mswindows_handle_wm_initmenupopup (HMENU hmenu, struct frame* frm);
Lisp_Object mswindows_handle_wm_initmenu (HMENU hmenu, struct frame* f);
Lisp_Object mswindows_handle_wm_command (struct frame* f, WORD command);

#endif /* HAVE_MENUBARS */

#endif /* INCLUDED_menubar_msw_h_ */

