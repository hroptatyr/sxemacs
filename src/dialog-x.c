/* Implements elisp-programmable dialog boxes -- X interface.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
   Copyright (C) 2000 Ben Wing.

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

/* This file Mule-ized by Ben Wing, 7-8-00. */

#include <config.h>
#include "lisp.h"

#include "console-x.h"
#include "EmacsFrame.h"
#include "gui-x.h"

#include "buffer.h"
#include "commands.h"           /* zmacs_regions */
#include "events.h"
#include "frame.h"
#include "gui.h"
#include "opaque.h"
#include "window.h"


static void
maybe_run_dbox_text_callback (LWLIB_ID id)
{
  widget_value *wv;
  int got_some;
  wv = xmalloc_widget_value ();
  wv->name = xstrdup ("value");
  got_some = lw_get_some_values (id, wv);
  if (got_some)
    {
      Lisp_Object text_field_callback;
      Extbyte *text_field_value = wv->value;
      VOID_TO_LISP (text_field_callback, wv->call_data);
      text_field_callback = XCAR (XCDR (text_field_callback));
      if (text_field_value)
	{
	  void *tmp =
	    LISP_TO_VOID (cons3 (Qnil,
				 list2 (text_field_callback,
					build_ext_string (text_field_value,
							  Qlwlib_encoding)),
				 Qnil));
	  popup_selection_callback (0, id, (XtPointer) tmp);
	}
    }
  /* This code tried to optimize, newing/freeing. This is generally
     unsafe so we will always strdup and always use
     free_widget_value_tree. */
  free_widget_value_tree (wv);
}

static void
dbox_selection_callback (Widget widget, LWLIB_ID id, XtPointer client_data)
{
  /* This is called with client_data == -1 when WM_DELETE_WINDOW is sent
     instead of a button being selected. */
  struct device *d = get_device_from_display (XtDisplay (widget));
  struct frame *f = 0;
  Widget cur_widget = widget;

  /* The parent which is actually connected to our EmacsFrame may be a
     ways up the tree. */
  while (!f && cur_widget)
    {
      f = x_any_window_to_frame (d, XtWindow (cur_widget));
      cur_widget = XtParent (cur_widget);
    }

  if (popup_handled_p (id))
    return;
  assert (popup_up_p != 0);
  ungcpro_popup_callbacks (id);
  popup_up_p--;
  maybe_run_dbox_text_callback (id);
  popup_selection_callback (widget, id, client_data);
  /* #### need to error-protect!  will do so when i merge in
     my working ws */
  va_run_hook_with_args (Qdelete_dialog_box_hook, 1, make_int (id));
  lw_destroy_all_widgets (id);

  /* The Motif dialog box sets the keyboard focus to itself.  When it
     goes away we have to take care of getting the focus back
     ourselves. */
#ifdef EXTERNAL_WIDGET
  /* #### Not sure if this special case is necessary. */
  if (!FRAME_X_EXTERNAL_WINDOW_P (f) && f)
#else
  if (f)
#endif
    lw_set_keyboard_focus (FRAME_X_SHELL_WIDGET (f), FRAME_X_TEXT_WIDGET (f));
}

static const Extbyte * const button_names [] = {
  "button1", "button2", "button3", "button4", "button5",
  "button6", "button7", "button8", "button9", "button10" };

static widget_value *
dbox_descriptor_to_widget_value (Lisp_Object keys)
{
  /* This function can GC */
  int lbuttons = 0, rbuttons = 0;
  int partition_seen = 0;
  int text_field_p = 0;
  int allow_text_p = 1;
  widget_value *prev = 0, *kids = 0;
  int n = 0;
  int count = specpdl_depth ();
  Lisp_Object wv_closure, gui_item;
  Lisp_Object question = Qnil;
  Lisp_Object title    = Qnil;  /* #### currently unused */
  Lisp_Object buttons  = Qnil;

  {
    EXTERNAL_PROPERTY_LIST_LOOP_3 (key, value, keys)
      {
	if (EQ (key, Q_question))
	  {
	    CHECK_STRING (value);
	    question = value;
	  }
	else if (EQ (key, Q_title))
	  {
	    CHECK_STRING (value);
	    title = value;
	  }
	else if (EQ (key, Q_buttons))
	  {
	    CHECK_LIST (value);
	    buttons = value;
	  }
	else
	  syntax_error ("Unrecognized question-dialog keyword", key);
      }
  }

  if (NILP (question))
    syntax_error ("Dialog descriptor provides no question", keys);

  /* Inhibit GC during this conversion.  The reasons for this are
     the same as in menu_item_descriptor_to_widget_value(); see
     the large comment above that function. */

  record_unwind_protect (restore_gc_inhibit,
			 make_int (gc_currently_forbidden));
  gc_currently_forbidden = 1;

  kids = prev = xmalloc_widget_value ();

  /* Also make sure that we free the partially-created widget_value
     tree on Lisp error. */

  wv_closure = make_opaque_ptr (kids);
  record_unwind_protect (widget_value_unwind, wv_closure);
  prev->name = xstrdup ("message");
  LISP_STRING_TO_EXTERNAL_MALLOC (question, prev->value, Qlwlib_encoding);
  prev->enabled = 1;

  {
    EXTERNAL_LIST_LOOP_2 (button, buttons)
      {
	widget_value *wv;

	if (NILP (button))
	  {
	    if (partition_seen)
	      syntax_error ("More than one partition (nil) seen in dbox spec",
			    keys);
	    partition_seen = 1;
	    continue;
	  }
	CHECK_VECTOR (button);
	wv = xmalloc_widget_value ();

	gui_item = gui_parse_item_keywords (button);
	if (!button_item_to_widget_value (Qdialog,
					  gui_item, wv, allow_text_p, 1, 0, 1))
	  {
	    free_widget_value_tree (wv);
	    continue;
	  }

	if (wv->type == TEXT_TYPE)
	  {
	    text_field_p = 1;
	    allow_text_p = 0;	 /* only allow one */
	  }
	else			/* it's a button */
	  {
	    allow_text_p = 0;	 /* only allow text field at the front */
	    if (wv->value)
	      xfree (wv->value);
	    wv->value = wv->name;	/* what a mess... */
	    wv->name = xstrdup (button_names [n]);

	    if (partition_seen)
	      rbuttons++;
	    else
	      lbuttons++;
	    n++;

	    if (lbuttons > 9 || rbuttons > 9)
	      syntax_error ("Too many buttons (9)",
			    keys); /* #### this leaks */
	  }

	prev->next = wv;
	prev = wv;
      }
  }

  if (n == 0)
    syntax_error ("Dialog boxes must have some buttons", keys);

  {
    Extbyte type = (text_field_p ? 'P' : 'Q');
    static Extbyte tmp_dbox_name [255];

    widget_value *dbox;
    sprintf (tmp_dbox_name, "%c%dBR%d", type, lbuttons + rbuttons, rbuttons);
    dbox = xmalloc_widget_value ();
    dbox->name = xstrdup (tmp_dbox_name);
    dbox->contents = kids;

    /* No more need to free the half-filled-in structures. */
    set_opaque_ptr (wv_closure, 0);
    unbind_to (count, Qnil);
    return dbox;
  }
}

static Lisp_Object
x_make_dialog_box_internal (struct frame* f, Lisp_Object type,
			    Lisp_Object keys)
{
  int dbox_id;
  widget_value *data;
  Widget parent, dbox;

  if (!EQ (type, Qquestion))
    signal_type_error (Qunimplemented, "Dialog box type", type);

  data = dbox_descriptor_to_widget_value (keys);

  parent = FRAME_X_SHELL_WIDGET (f);

  dbox_id = new_lwlib_id ();
  dbox = lw_create_widget (data->name, "dialog", dbox_id, data, parent, 1, 0,
			   dbox_selection_callback, 0);
  lw_modify_all_widgets (dbox_id, data, True);
  lw_modify_all_widgets (dbox_id, data->contents, True);
  free_popup_widget_value_tree (data);

  gcpro_popup_callbacks (dbox_id);

  /* Setting zmacs-region-stays is necessary here because executing a
     command from a dialog is really a two-command process: the first
     command (bound to the button-click) simply pops up the dialog,
     and returns.  This causes a sequence of magic-events (destined
     for the dialog widget) to begin.  Eventually, a dialog item is
     selected, and a misc-user-event blip is pushed onto the end of
     the input stream, which is then executed by the event loop.

     So there are two command-events, with a bunch of magic-events
     between them.  We don't want the *first* command event to alter
     the state of the region, so that the region can be available as
     an argument for the second command. */
  if (zmacs_regions)
    zmacs_region_stays = 1;

  popup_up_p++;
  lw_pop_up_all_widgets (dbox_id);

  /* #### this could (theoretically) cause problems if we are up for
     a REALLY REALLY long time -- too big to fit into lisp integer. */
  return make_int (dbox_id);
}

void
syms_of_dialog_x (void)
{
}

void
console_type_create_dialog_x (void)
{
  CONSOLE_HAS_METHOD (x, make_dialog_box_internal);
}

void
vars_of_dialog_x (void)
{
#if defined (LWLIB_DIALOGS_LUCID)
  Fprovide (intern ("lucid-dialogs"));
#elif defined (LWLIB_DIALOGS_MOTIF)
  Fprovide (intern ("motif-dialogs"));
#elif defined (LWLIB_DIALOGS_ATHENA)
  Fprovide (intern ("athena-dialogs"));
#endif
}
