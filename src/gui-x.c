/* General GUI code -- X-specific. (menubars, scrollbars, toolbars, dialogs)
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996, 2000 Ben Wing.
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

/* This file Mule-ized by Ben Wing, 7-8-00. */

#include <config.h>
#include "lisp.h"

#include "console-x.h"
#ifdef LWLIB_USES_MOTIF
#include <Xm/Xm.h> /* for XmVersion */
#endif
#include "gui-x.h"
#include "buffer.h"
#include "device.h"
#include "events.h"
#include "frame.h"
#include "gui.h"
#include "glyphs.h"
#include "redisplay.h"
#include "opaque.h"

/* we need a unique id for each popup menu, dialog box, and scrollbar */
static unsigned int lwlib_id_tick;

LWLIB_ID
new_lwlib_id (void)
{
  return ++lwlib_id_tick;
}

widget_value *
xmalloc_widget_value (void)
{
  widget_value *tmp = malloc_widget_value ();
  if (!tmp) memory_full ();
  return tmp;
}


static int
mark_widget_value_mapper (widget_value *val, void *closure)
{
  Lisp_Object markee;
  if (val->call_data)
    {
      VOID_TO_LISP (markee, val->call_data);
      mark_object (markee);
    }

  if (val->accel)
    {
      VOID_TO_LISP (markee, val->accel);
      mark_object (markee);
    }
  return 0;
}

static Lisp_Object
mark_popup_data (Lisp_Object obj)
{
  struct popup_data *data = (struct popup_data *) XPOPUP_DATA (obj);

  /* Now mark the callbacks and such that are hidden in the lwlib
     call-data */

  if (data->id)
    lw_map_widget_values (data->id, mark_widget_value_mapper, 0);

  return data->last_menubar_buffer;
}

DEFINE_LRECORD_IMPLEMENTATION ("popup-data", popup_data,
                               mark_popup_data, internal_object_printer,
			       0, 0, 0, 0, struct popup_data);

/* This is like FRAME_MENUBAR_DATA (f), but contains an alist of
   (id . popup-data) for GCPRO'ing the callbacks of the popup menus
   and dialog boxes. */
static Lisp_Object Vpopup_callbacks;

void
gcpro_popup_callbacks (LWLIB_ID id)
{
  struct popup_data *pdata;
  Lisp_Object lid = make_int (id);
  Lisp_Object lpdata;

  assert (NILP (assq_no_quit (lid, Vpopup_callbacks)));
  pdata = alloc_lcrecord_type (struct popup_data, &lrecord_popup_data);
  pdata->id = id;
  pdata->last_menubar_buffer = Qnil;
  pdata->menubar_contents_up_to_date = 0;
  XSETPOPUP_DATA (lpdata, pdata);
  Vpopup_callbacks = Fcons (Fcons (lid, lpdata), Vpopup_callbacks);
}

void
ungcpro_popup_callbacks (LWLIB_ID id)
{
  Lisp_Object lid = make_int (id);
  Lisp_Object this = assq_no_quit (lid, Vpopup_callbacks);
  assert (!NILP (this));
  Vpopup_callbacks = delq_no_quit (this, Vpopup_callbacks);
}

int
popup_handled_p (LWLIB_ID id)
{
  return NILP (assq_no_quit (make_int (id), Vpopup_callbacks));
}

/* menu_item_descriptor_to_widget_value() et al. mallocs a
   widget_value, but then may signal lisp errors.  If an error does
   not occur, the opaque ptr we have here has had its pointer set to 0
   to tell us not to do anything.  Otherwise we free the widget value.
   (This has nothing to do with GC, it's just about not dropping
   pointers to malloc'd data when errors happen.) */

Lisp_Object
widget_value_unwind (Lisp_Object closure)
{
  widget_value *wv = (widget_value *) get_opaque_ptr (closure);
  free_opaque_ptr (closure);
  if (wv)
    free_widget_value_tree (wv);
  return Qnil;
}

#if 0
static void
print_widget_value (widget_value *wv, int depth)
{
  /* strings in wv are in external format; use printf not stdout_out
     because the latter takes internal-format strings */
  Extbyte d [200];
  int i;
  for (i = 0; i < depth; i++) d[i] = ' ';
  d[depth]=0;
  /* #### - print type field */
  printf ("%sname:    %s\n", d, (wv->name ? wv->name : "(null)"));
  if (wv->value) printf ("%svalue:   %s\n", d, wv->value);
  if (wv->key)   printf ("%skey:     %s\n", d, wv->key);
  printf ("%senabled: %d\n", d, wv->enabled);
  if (wv->contents)
    {
      printf ("\n%scontents: \n", d);
      print_widget_value (wv->contents, depth + 5);
    }
  if (wv->next)
    {
      printf ("\n");
      print_widget_value (wv->next, depth);
    }
}
#endif

/* This recursively calls free_widget_value() on the tree of widgets.
   It must free all data that was malloc'ed for these widget_values.

   It used to be that emacs only allocated new storage for the `key' slot.
   All other slots are pointers into the data of Lisp_Strings, and must be
   left alone.  */
void
free_popup_widget_value_tree (widget_value *wv)
{
  if (! wv) return;
  if (wv->key) xfree (wv->key);
  if (wv->value) xfree (wv->value);
  if (wv->name) xfree (wv->name);

  wv->name = wv->value = wv->key = (char *) 0xDEADBEEF;

  if (wv->contents && (wv->contents != (widget_value*)1))
    {
      free_popup_widget_value_tree (wv->contents);
      wv->contents = (widget_value *) 0xDEADBEEF;
    }
  if (wv->next)
    {
      free_popup_widget_value_tree (wv->next);
      wv->next = (widget_value *) 0xDEADBEEF;
    }
  free_widget_value (wv);
}

/* The following is actually called from somewhere within XtDispatchEvent(),
   called from XtAppProcessEvent() in event-Xt.c

   Callback function for widgets and menus.
 */

void
popup_selection_callback (Widget widget, LWLIB_ID ignored_id,
			  XtPointer client_data)
{
  Lisp_Object data, image_instance, callback, callback_ex;
  Lisp_Object frame, event;
  int update_subwindows_p = 0;
  struct device *d = get_device_from_display (XtDisplay (widget));
  struct frame *f = x_any_widget_or_parent_to_frame (d, widget);

  /* set in lwlib to the time stamp associated with the most recent menu
     operation */
  extern Time x_focus_timestamp_really_sucks_fix_me_better;

  if (!f)
    return;
  if (((EMACS_INT) client_data) == 0)
    return;
  VOID_TO_LISP (data, client_data);
  XSETFRAME (frame, f);

#if 0
  /* #### What the hell?  I can't understand why this call is here,
     and doing it is really courting disaster in the new event
     model, since popup_selection_callback is called from
     within next_event_internal() and Faccept_process_output()
     itself calls next_event_internal().  --Ben */

  /* Flush the X and process input */
  Faccept_process_output (Qnil, Qnil, Qnil);
#endif

  if (((EMACS_INT) client_data) == -1)
    {
      event = Fmake_event (Qnil, Qnil);

      XEVENT (event)->event_type = misc_user_event;
      XEVENT (event)->channel = frame;
      XEVENT (event)->event.eval.function = Qrun_hooks;
      XEVENT (event)->event.eval.object = Qmenu_no_selection_hook;
    }
  else
    {
      image_instance = XCAR (data);
      callback = XCAR (XCDR (data));
      callback_ex = XCDR (XCDR (data));
      update_subwindows_p = 1;
      /* It is possible for a widget action to cause it to get out of
	 sync with its instantiator. Thus it is necessary to signal
	 this possibility. */
      if (IMAGE_INSTANCEP (image_instance))
	XIMAGE_INSTANCE_WIDGET_ACTION_OCCURRED (image_instance) = 1;

      if (!NILP (callback_ex) && !UNBOUNDP (callback_ex))
	{
	  event = Fmake_event (Qnil, Qnil);

	  XEVENT (event)->event_type = misc_user_event;
	  XEVENT (event)->channel = frame;
	  XEVENT (event)->event.eval.function = Qeval;
	  XEVENT (event)->event.eval.object =
	    list4 (Qfuncall, callback_ex, image_instance, event);
	}
      else if (NILP (callback) || UNBOUNDP (callback))
	event = Qnil;
      else
	{
	  Lisp_Object fn, arg;

	  event = Fmake_event (Qnil, Qnil);

	  get_gui_callback (callback, &fn, &arg);
	  XEVENT (event)->event_type = misc_user_event;
	  XEVENT (event)->channel = frame;
	  XEVENT (event)->event.eval.function = fn;
	  XEVENT (event)->event.eval.object = arg;
	}
    }

  /* This is the timestamp used for asserting focus so we need to get an
     up-to-date value event if no events have been dispatched to emacs
     */
#if defined(HAVE_MENUBARS)
  DEVICE_X_MOUSE_TIMESTAMP (d) = x_focus_timestamp_really_sucks_fix_me_better;
#else
  DEVICE_X_MOUSE_TIMESTAMP (d) = DEVICE_X_GLOBAL_MOUSE_TIMESTAMP (d);
#endif
  if (!NILP (event))
    enqueue_Xt_dispatch_event (event);
  /* The result of this evaluation could cause other instances to change so
     enqueue an update callback to check this. */
  if (update_subwindows_p && !NILP (event))
    enqueue_magic_eval_event (update_widget_instances, frame);
}

#if 1
  /* Eval the activep slot of the menu item */
# define wv_set_evalable_slot(slot,form) do {	\
  Lisp_Object wses_form = (form);		\
  (slot) = (NILP (wses_form) ? 0 :		\
	    EQ (wses_form, Qt) ? 1 :		\
	    !NILP (Feval (wses_form)));		\
} while (0)
#else
  /* Treat the activep slot of the menu item as a boolean */
# define wv_set_evalable_slot(slot,form)	\
      ((void) (slot = (!NILP (form))))
#endif

Extbyte *
menu_separator_style_and_to_external (const Bufbyte *s)
{
  const Bufbyte *p;
  Bufbyte first;

  if (!s || s[0] == '\0')
    return NULL;
  first = s[0];
  if (first != '-' && first != '=')
    return NULL;
  for (p = s; *p == first; p++)
    DO_NOTHING;

  /* #### - cannot currently specify a separator tag "--!tag" and a
     separator style "--:style" at the same time. */
  /* #### - Also, the motif menubar code doesn't deal with the
     double etched style yet, so it's not good to get into the habit of
     using "===" in menubars to get double-etched lines */
  if (*p == '!' || *p == '\0')
    return ((first == '-')
	    ? NULL			/* single etched is the default */
	    : xstrdup ("shadowDoubleEtchedIn"));
  else if (*p == ':')
    {
      Extbyte *retval;

      C_STRING_TO_EXTERNAL_MALLOC (p + 1, retval, Qlwlib_encoding);
      return retval;
    }

  return NULL;
}

Extbyte *
add_accel_and_to_external (Lisp_Object string)
{
  int i;
  int found_accel = 0;
  Extbyte *retval;
  Bufbyte *name = XSTRING_DATA (string);

  for (i = 0; name[i]; ++i)
    if (name[i] == '%' && name[i+1] == '_')
      {
	found_accel = 1;
	break;
      }

  if (found_accel)
    LISP_STRING_TO_EXTERNAL_MALLOC (string, retval, Qlwlib_encoding);
  else
    {
      size_t namelen = XSTRING_LENGTH (string);
      Bufbyte *chars = (Bufbyte *) alloca (namelen + 3);
      chars[0] = '%';
      chars[1] = '_';
      memcpy (chars + 2, name, namelen + 1);
      C_STRING_TO_EXTERNAL_MALLOC (chars, retval, Qlwlib_encoding);
    }

  return retval;
}

/* This does the dirty work.  gc_currently_forbidden is 1 when this is called.
 */
int
button_item_to_widget_value (Lisp_Object gui_object_instance,
			     Lisp_Object gui_item, widget_value *wv,
			     int allow_text_field_p, int no_keys_p,
			     int menu_entry_p, int accel_p)
{
  /* This function cannot GC because gc_currently_forbidden is set when
     it's called */
  Lisp_Gui_Item* pgui = 0;

  /* degenerate case */
  if (STRINGP (gui_item))
    {
      wv->type = TEXT_TYPE;
      if (accel_p)
	wv->name = add_accel_and_to_external (gui_item);
      else
	LISP_STRING_TO_EXTERNAL_MALLOC (gui_item, wv->name, Qlwlib_encoding);
      return 1;
    }
  else if (!GUI_ITEMP (gui_item))
    syntax_error ("need a string or a gui_item here", gui_item);

  pgui = XGUI_ITEM (gui_item);

  if (!NILP (pgui->filter))
    syntax_error (":filter keyword not permitted on leaf nodes", gui_item);

#ifdef HAVE_MENUBARS
  if (menu_entry_p && !gui_item_included_p (gui_item, Vmenubar_configuration))
    {
      /* the include specification says to ignore this item. */
      return 0;
    }
#endif /* HAVE_MENUBARS */

  if (!STRINGP (pgui->name))
    pgui->name = Feval (pgui->name);

  CHECK_STRING (pgui->name);
  if (accel_p)
    {
      wv->name = add_accel_and_to_external (pgui->name);
      wv->accel = LISP_TO_VOID (gui_item_accelerator (gui_item));
    }
  else
    {
      LISP_STRING_TO_EXTERNAL_MALLOC (pgui->name, wv->name, Qlwlib_encoding);
      wv->accel = LISP_TO_VOID (Qnil);
    }

  if (!NILP (pgui->suffix))
    {
      Lisp_Object suffix2;

      /* Shortcut to avoid evaluating suffix each time */
      if (STRINGP (pgui->suffix))
	suffix2 = pgui->suffix;
      else
	{
	  suffix2 = Feval (pgui->suffix);
	  CHECK_STRING (suffix2);
	}

      LISP_STRING_TO_EXTERNAL_MALLOC (suffix2, wv->value, Qlwlib_encoding);
    }

  wv_set_evalable_slot (wv->enabled, pgui->active);
  wv_set_evalable_slot (wv->selected, pgui->selected);

  if (!NILP (pgui->callback) || !NILP (pgui->callback_ex))
    wv->call_data = LISP_TO_VOID (cons3 (gui_object_instance,
					 pgui->callback,
					 pgui->callback_ex));

  if (no_keys_p
#ifdef HAVE_MENUBARS
      || (menu_entry_p && !menubar_show_keybindings)
#endif
      )
    wv->key = 0;
  else if (!NILP (pgui->keys))	/* Use this string to generate key bindings */
    {
      CHECK_STRING (pgui->keys);
      pgui->keys = Fsubstitute_command_keys (pgui->keys);
      if (XSTRING_LENGTH (pgui->keys) > 0)
	LISP_STRING_TO_EXTERNAL_MALLOC (pgui->keys, wv->key, Qlwlib_encoding);
      else
	wv->key = 0;
    }
  else if (SYMBOLP (pgui->callback))	/* Show the binding of this command. */
    {
      char buf[1024]; /* #### */
      /* #### Warning, dependency here on current_buffer and point */
      where_is_to_char (pgui->callback, buf);
      if (buf [0])
	C_STRING_TO_EXTERNAL_MALLOC (buf, wv->key, Qlwlib_encoding);
      else
	wv->key = 0;
    }

  CHECK_SYMBOL (pgui->style);
  if (NILP (pgui->style))
    {
      Bufbyte *intname;
      Bytecount intlen;
      /* If the callback is nil, treat this item like unselectable text.
	 This way, dashes will show up as a separator. */
      if (!wv->enabled)
	wv->type = BUTTON_TYPE;
      TO_INTERNAL_FORMAT (C_STRING, wv->name,
			  ALLOCA, (intname, intlen),
			  Qlwlib_encoding);
      if (separator_string_p (intname))
	{
	  wv->type = SEPARATOR_TYPE;
	  wv->value = menu_separator_style_and_to_external (intname);
	}
      else
	{
#if 0
	  /* #### - this is generally desirable for menubars, but it breaks
	     a package that uses dialog boxes and next_command_event magic
	     to use the callback slot in dialog buttons for data instead of
	     a real callback.

	     Code is data, right?  The beauty of LISP abuse.   --Stig */
	  if (NILP (callback))
	    wv->type = TEXT_TYPE;
	  else
#endif
	    wv->type = BUTTON_TYPE;
	}
    }
  else if (EQ (pgui->style, Qbutton))
    wv->type = BUTTON_TYPE;
  else if (EQ (pgui->style, Qtoggle))
    wv->type = TOGGLE_TYPE;
  else if (EQ (pgui->style, Qradio))
    wv->type = RADIO_TYPE;
  else if (EQ (pgui->style, Qtext))
    {
      wv->type = TEXT_TYPE;
#if 0
      wv->value = wv->name;
      wv->name = "value";
#endif
    }
  else
    syntax_error_2 ("Unknown style", pgui->style, gui_item);

  if (!allow_text_field_p && (wv->type == TEXT_TYPE))
    syntax_error ("Text field not allowed in this context", gui_item);

  if (!NILP (pgui->selected) && EQ (pgui->style, Qtext))
    syntax_error
      (":selected only makes sense with :style toggle, radio or button",
       gui_item);
  return 1;
}

/* parse tree's of gui items into widget_value hierarchies */
static void gui_item_children_to_widget_values (Lisp_Object
						gui_object_instance,
						Lisp_Object items,
						widget_value* parent,
						int accel_p);

static widget_value *
gui_items_to_widget_values_1 (Lisp_Object gui_object_instance,
			      Lisp_Object items, widget_value* parent,
			      widget_value* prev, int accel_p)
{
  widget_value* wv = 0;

  assert ((parent || prev) && !(parent && prev));
  /* now walk the tree creating widget_values as appropriate */
  if (!CONSP (items))
    {
      wv = xmalloc_widget_value ();
      if (parent)
	parent->contents = wv;
      else
	prev->next = wv;
      if (!button_item_to_widget_value (gui_object_instance,
					items, wv, 0, 1, 0, accel_p))
	{
	  free_widget_value_tree (wv);
	  if (parent)
	    parent->contents = 0;
	  else
	    prev->next = 0;
	}
      else
	wv->value = xstrdup (wv->name);	/* what a mess... */
    }
  else
    {
      /* first one is the parent */
      if (CONSP (XCAR (items)))
	syntax_error ("parent item must not be a list", XCAR (items));

      if (parent)
	wv = gui_items_to_widget_values_1 (gui_object_instance,
					   XCAR (items), parent, 0, accel_p);
      else
	wv = gui_items_to_widget_values_1 (gui_object_instance,
					   XCAR (items), 0, prev, accel_p);
      /* the rest are the children */
      gui_item_children_to_widget_values (gui_object_instance,
					  XCDR (items), wv, accel_p);
    }
  return wv;
}

static void
gui_item_children_to_widget_values (Lisp_Object gui_object_instance,
				    Lisp_Object items, widget_value* parent,
				    int accel_p)
{
  widget_value* wv = 0, *prev = 0;
  Lisp_Object rest;
  CHECK_CONS (items);

  /* first one is master */
  prev = gui_items_to_widget_values_1 (gui_object_instance, XCAR (items),
				       parent, 0, accel_p);
  /* the rest are the children */
  LIST_LOOP (rest, XCDR (items))
    {
      Lisp_Object tab = XCAR (rest);
      wv = gui_items_to_widget_values_1 (gui_object_instance, tab, 0, prev,
					 accel_p);
      prev = wv;
    }
}

widget_value *
gui_items_to_widget_values (Lisp_Object gui_object_instance, Lisp_Object items,
			    int accel_p)
{
  /* This function can GC */
  widget_value *control = 0, *tmp = 0;
  int count = specpdl_depth ();
  Lisp_Object wv_closure;

  if (NILP (items))
    syntax_error ("must have some items", items);

  /* Inhibit GC during this conversion.  The reasons for this are
     the same as in menu_item_descriptor_to_widget_value(); see
     the large comment above that function. */
  record_unwind_protect (restore_gc_inhibit,
			 make_int (gc_currently_forbidden));
  gc_currently_forbidden = 1;

  /* Also make sure that we free the partially-created widget_value
     tree on Lisp error. */
  control = xmalloc_widget_value ();
  wv_closure = make_opaque_ptr (control);
  record_unwind_protect (widget_value_unwind, wv_closure);

  gui_items_to_widget_values_1 (gui_object_instance, items, control, 0,
				accel_p);

  /* mess about getting the data we really want */
  tmp = control;
  control = control->contents;
  tmp->next = 0;
  tmp->contents = 0;
  free_widget_value_tree (tmp);

  /* No more need to free the half-filled-in structures. */
  set_opaque_ptr (wv_closure, 0);
  unbind_to (count, Qnil);

  return control;
}

/* This is a kludge to make sure emacs can only link against a version of
   lwlib that was compiled in the right way.  Emacs references symbols which
   correspond to the way it thinks lwlib was compiled, and if lwlib wasn't
   compiled in that way, then somewhat meaningful link errors will result.
   The alternatives to this range from obscure link errors, to obscure
   runtime errors that look a lot like bugs.
 */

static void
sanity_check_lwlib (void)
{
#define MACROLET(v) { extern int v; v = 1; }

#if (XlibSpecificationRelease == 4)
  MACROLET (lwlib_uses_x11r4);
#elif (XlibSpecificationRelease == 5)
  MACROLET (lwlib_uses_x11r5);
#elif (XlibSpecificationRelease == 6)
  MACROLET (lwlib_uses_x11r6);
#else
  MACROLET (lwlib_uses_unknown_x11);
#endif
#ifdef LWLIB_USES_MOTIF
  MACROLET (lwlib_uses_motif);
#else
  MACROLET (lwlib_does_not_use_motif);
#endif
#if (XmVersion >= 1002)
  MACROLET (lwlib_uses_motif_1_2);
#else
  MACROLET (lwlib_does_not_use_motif_1_2);
#endif
#ifdef LWLIB_MENUBARS_LUCID
  MACROLET (lwlib_menubars_lucid);
#elif defined (HAVE_MENUBARS)
  MACROLET (lwlib_menubars_motif);
#endif
#ifdef LWLIB_SCROLLBARS_LUCID
  MACROLET (lwlib_scrollbars_lucid);
#elif defined (LWLIB_SCROLLBARS_MOTIF)
  MACROLET (lwlib_scrollbars_motif);
#elif defined (HAVE_SCROLLBARS)
  MACROLET (lwlib_scrollbars_athena);
#endif
#ifdef LWLIB_DIALOGS_MOTIF
  MACROLET (lwlib_dialogs_motif);
#elif defined (HAVE_DIALOGS)
  MACROLET (lwlib_dialogs_athena);
#endif
#ifdef LWLIB_WIDGETS_MOTIF
  MACROLET (lwlib_widgets_motif);
#elif defined (HAVE_WIDGETS)
  MACROLET (lwlib_widgets_athena);
#endif

#undef MACROLET
}

void
syms_of_gui_x (void)
{
  INIT_LRECORD_IMPLEMENTATION (popup_data);
}

void
reinit_vars_of_gui_x (void)
{
  lwlib_id_tick = (1<<16);	/* start big, to not conflict with Energize */
#ifdef HAVE_POPUPS
  popup_up_p = 0;
#endif

  /* this makes only safe calls as in emacs.c */
  sanity_check_lwlib ();
}

void
vars_of_gui_x (void)
{
  reinit_vars_of_gui_x ();

  Vpopup_callbacks = Qnil;
  staticpro (&Vpopup_callbacks);
}
