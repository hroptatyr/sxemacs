/* General GUI code -- X-specific. (menubars, scrollbars, toolbars, dialogs)
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1998 Free Software Foundation, Inc.

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
#include "device.h"
#include "frame.h"
#include "gui.h"
#include "opaque.h"

#ifdef HAVE_POPUPS
Lisp_Object Qmenu_no_selection_hook;
#endif

static GUI_ID gui_id_ctr = 0;

GUI_ID
new_gui_id (void)
{
  return (++gui_id_ctr);
}

/* This is like FRAME_MENUBAR_DATA (f), but contains an alist of
   (id . popup-data) for GCPRO'ing the callbacks of the popup menus
   and dialog boxes. */
static Lisp_Object Vpopup_callbacks;

void
gcpro_popup_callbacks (GUI_ID id, Lisp_Object data)
{
  Vpopup_callbacks = Fcons (Fcons (make_int (id), data), Vpopup_callbacks);
}

void
ungcpro_popup_callbacks (GUI_ID id)
{
  Lisp_Object lid = make_int (id);
  Lisp_Object this = assq_no_quit (lid, Vpopup_callbacks);
  Vpopup_callbacks = delq_no_quit (this, Vpopup_callbacks);
}

Lisp_Object
get_gcpro_popup_callbacks (GUI_ID id)
{
  Lisp_Object lid = make_int (id);
  Lisp_Object this = assq_no_quit (lid, Vpopup_callbacks);

  if (!NILP (this))
    {
      return (XCDR (this));
    }
  return (Qnil);
}

void
syms_of_gui_gtk (void)
{
#ifdef HAVE_POPUPS
  defsymbol (&Qmenu_no_selection_hook, "menu-no-selection-hook");
#endif
}

void
vars_of_gui_gtk (void)
{
  staticpro (&Vpopup_callbacks);
  Vpopup_callbacks = Qnil;
#ifdef HAVE_POPUPS
  popup_up_p = 0;

#if 0
  /* This DEFVAR_LISP is just for the benefit of make-docfile. */
  /* #### misnamed */
  DEFVAR_LISP ("menu-no-selection-hook", &Vmenu_no_selection_hook /*
Function or functions to call when a menu or dialog box is dismissed
without a selection having been made.
*/ );
#endif

  Fset (Qmenu_no_selection_hook, Qnil);
#endif /* HAVE_POPUPS */
}
