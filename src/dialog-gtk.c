/* Implements elisp-programmable dialog boxes -- Gtk interface.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Tinker Systems and INS Engineering Corp.

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

#include <config.h>
#include "lisp.h"

#include "console-gtk.h"
#include "gui-gtk.h"

#include "buffer.h"
#include "commands.h"           /* zmacs_regions */
#include "events.h"
#include "frame.h"
#include "gui.h"
#include "opaque.h"
#include "window.h"

Lisp_Object Qgtk_make_dialog_box_internal;

/* We just bounce up into lisp here... see $(srcdir)/lisp/dialog-gtk.el */
static Lisp_Object
gtk_make_dialog_box_internal (struct frame* f, Lisp_Object type, Lisp_Object keys)
{
  return (call2 (Qgtk_make_dialog_box_internal, type, keys));
}

void
syms_of_dialog_gtk (void)
{
  defsymbol (&Qgtk_make_dialog_box_internal, "gtk-make-dialog-box-internal");
}

void
console_type_create_dialog_gtk (void)
{
  CONSOLE_HAS_METHOD (gtk, make_dialog_box_internal);
}

void
vars_of_dialog_gtk (void)
{
  Fprovide (intern ("gtk-dialogs"));
}
