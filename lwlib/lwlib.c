/* A general interface to the widgets of different toolkits.
   Copyright (C) 1992, 1993, 1994 Lucid, Inc.
   Copyright (C) 1995 Tinker Systems and INS Engineering Corp.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifdef NeXT
#undef __STRICT_BSD__ /* ick */
#endif

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <X11/StringDefs.h>
#include "lwlib-internal.h"
#include "lwlib-utils.h"

#ifdef NEED_LUCID
#include "lwlib-Xlw.h"
#endif
#ifdef NEED_MOTIF
#include "lwlib-Xm.h"
#ifdef LWLIB_WIDGETS_MOTIF
#include <Xm/Xm.h>
#endif
#endif
#ifdef NEED_ATHENA
#include "lwlib-Xaw.h"
#endif

/* #### Does a check need to be put back in here to make sure we have
   sufficient defines to function properly or are the checks in the
   makefile sufficient? */

/* List of all widgets managed by the library.  Note that each "widget"
   listed here may actually be a tree of widgets; for example, a
   single entry here might represent a single menubar or popup menu,
   each of which might be implemented with a tree of widgets.
   */
static widget_info *all_widget_info = NULL;

/* boolean flag indicating that the menubar is active */
int lw_menu_active = 0;

/* X11 menubar widget */
Widget lw_menubar_widget = NULL;

/* whether the last menu operation was a keyboard accelerator */
int lw_menu_accelerate = False;


/* Forward declarations */
static void instantiate_widget_instance (widget_instance *instance);
static void free_widget_value_args (widget_value* wv);


/* utility functions for widget_instance and widget_info */
static char *
safe_strdup (const char *s)
{
  char *result;
  if (! s) return 0;
  result = (char *) malloc (strlen (s) + 1);
  if (! result)
    return 0;
  strcpy (result, s);
  return result;
}

static void
safe_free_str (char *s)
{
  if (s) free (s);
}

static widget_value *widget_value_free_list = 0;

widget_value *
malloc_widget_value (void)
{
  widget_value *wv;
  if (widget_value_free_list)
    {
      wv = widget_value_free_list;
      widget_value_free_list = wv->free_list;
      wv->free_list = 0;
    }
  else
    {
      wv = (widget_value *) malloc (sizeof (widget_value));
    }
  if (wv)
    {
      memset (wv, '\0', sizeof (widget_value));
    }
  return wv;
}

/* this is analogous to free().  It frees only what was allocated
   by malloc_widget_value(), and no substructures.
 */
void
free_widget_value (widget_value *wv)
{
  if (wv->free_list)
    abort ();
  wv->free_list = widget_value_free_list;
  widget_value_free_list = wv;
}

static void
free_widget_value_contents (widget_value *wv)
{
  if (wv->name)  free (wv->name);
  if (wv->value) free (wv->value);
  if (wv->key)   free (wv->key);

  /* #### - all of this 0xDEADBEEF stuff should be unnecessary
     in production code...  it should be conditionalized. */
  wv->name = wv->value = wv->key = (char *) 0xDEADBEEF;

  if (wv->toolkit_data && wv->free_toolkit_data)
    {
      XtFree ((char *) wv->toolkit_data);
      wv->toolkit_data = (void *) 0xDEADBEEF;
    }
#ifdef NEED_SCROLLBARS
  if (wv->scrollbar_data)
    {
      free (wv->scrollbar_data);
      wv->scrollbar_data = NULL;
    }
#endif
  if (wv->contents && (wv->contents != (widget_value*)1))
    {
      free_widget_value_tree (wv->contents);
      wv->contents = (widget_value *) 0xDEADBEEF;
    }

  free_widget_value_args (wv);

  if (wv->next)
    {
      free_widget_value_tree (wv->next);
      wv->next = (widget_value *) 0xDEADBEEF;
    }
}

void
free_widget_value_tree (widget_value *wv)
{
  if (!wv)
    return;

  free_widget_value_contents (wv);
  free_widget_value (wv);
}

#ifdef NEED_SCROLLBARS

static void
copy_scrollbar_values (widget_value *val, widget_value *copy)
{
  if (!copy->scrollbar_data)
    copy->scrollbar_data =
      (scrollbar_values *) malloc (sizeof (scrollbar_values));

  if (val->scrollbar_data)
    *copy->scrollbar_data = *val->scrollbar_data;
  else
    memset (copy->scrollbar_data, '\0', sizeof (scrollbar_values));
}

/*
 * Return true if old->scrollbar_data were not equivalent
 * to new->scrollbar_data.
 */
static Boolean
merge_scrollbar_values (widget_value *old, widget_value *new)
{
  Boolean changed = False;

  if (new->scrollbar_data && !old->scrollbar_data)
    {
      copy_scrollbar_values (new, old);
      changed = True;
    }
  else if (!new->scrollbar_data && old->scrollbar_data)
    {
      free (old->scrollbar_data);
      old->scrollbar_data = NULL;
    }
  else if (new->scrollbar_data && old->scrollbar_data)
    {
      scrollbar_values *old_sb = old->scrollbar_data;
      scrollbar_values *new_sb = new->scrollbar_data;

      if ((old_sb->line_increment   != new_sb->line_increment)	 ||
	  (old_sb->page_increment   != new_sb->page_increment)	 ||
	  (old_sb->minimum	    != new_sb->minimum)		 ||
	  (old_sb->maximum	    != new_sb->maximum)		 ||
	  (old_sb->slider_size	    != new_sb->slider_size)	 ||
	  (old_sb->slider_position  != new_sb->slider_position)	 ||
	  (old_sb->scrollbar_width  != new_sb->scrollbar_width)	 ||
	  (old_sb->scrollbar_height != new_sb->scrollbar_height) ||
	  (old_sb->scrollbar_x	    != new_sb->scrollbar_x)	 ||
	  (old_sb->scrollbar_y	    != new_sb->scrollbar_y))
	changed = True;

      *old_sb = *new_sb;
    }

  return changed;
}

#endif /* NEED_SCROLLBARS */

#ifdef HAVE_WIDGETS
/*
 * Return true if old->args was not equivalent
 * to new->args.
 */
static Boolean
merge_widget_value_args (widget_value *old, widget_value *new)
{
  Boolean changed = False;

  if (new->args && !old->args)
    {
      lw_copy_widget_value_args (new, old);
      changed = True;
    }
  /* Generally we don't want to lose values that are already in the
     widget. */
  else if (!new->args && old->args)
    {
      lw_copy_widget_value_args (old, new);
      changed = True;
    }
  else if (new->args && old->args && new->args != old->args)
    {
      /* #### Do something more sensible here than just copying the
         new values (like actually merging the values). */
      lw_copy_widget_value_args (new, old);
      changed = True;
    }
  else if (new->args && new->args == old->args && new->args->args_changed == True)
    {
      changed = True;
    }

  return changed;
}
#endif /* HAVE_WIDGETS */

/* Make a complete copy of a widget_value tree.  Store CHANGE into
   the widget_value tree's `change' field. */

widget_value *
copy_widget_value_tree (widget_value *val, change_type change)
{
  widget_value *copy;

  if (!val)
    return NULL;
  if (val == (widget_value *) 1)
    return val;

  copy = malloc_widget_value ();
  if (copy)
    {
      /* #### - don't seg fault *here* if out of memory.  Menus will be
	 truncated inexplicably. */
      copy->type = val->type;
      copy->name = safe_strdup (val->name);
      copy->value = safe_strdup (val->value);
      copy->key = safe_strdup (val->key);
      copy->accel = val->accel;
      copy->enabled = val->enabled;
      copy->selected = val->selected;
      copy->edited = False;
      copy->change = change;
      copy->contents = copy_widget_value_tree (val->contents, change);
      copy->call_data = val->call_data;
      copy->next = copy_widget_value_tree (val->next, change);
      copy->toolkit_data = NULL;
      copy->free_toolkit_data = False;

      lw_copy_widget_value_args (val, copy);
#ifdef NEED_SCROLLBARS
      copy_scrollbar_values (val, copy);
#endif
    }
  return copy;
}

/* This function is used to implement incremental menu construction. */

widget_value *
replace_widget_value_tree (widget_value *node, widget_value *newtree)
{
  widget_value *copy;

  if (!node || !newtree)
    abort ();

  copy = copy_widget_value_tree (newtree, STRUCTURAL_CHANGE);

  free_widget_value_contents (node);
  *node = *copy;
  free_widget_value (copy);	/* free the node, but not its contents. */
  return node;
}

static widget_info *
allocate_widget_info (const char *type, const char *name,
                      LWLIB_ID id, widget_value *val,
		      lw_callback pre_activate_cb, lw_callback selection_cb,
		      lw_callback post_activate_cb)
{
  widget_info *info = (widget_info *) malloc (sizeof (widget_info));
  info->type = safe_strdup (type);
  info->name = safe_strdup (name);
  info->id = id;
  info->val = copy_widget_value_tree (val, STRUCTURAL_CHANGE);
  info->busy = False;
  info->pre_activate_cb = pre_activate_cb;
  info->selection_cb = selection_cb;
  info->post_activate_cb = post_activate_cb;
  info->instances = NULL;

  info->next = all_widget_info;
  all_widget_info = info;

  return info;
}

static void
free_widget_info (widget_info *info)
{
  safe_free_str (info->type);
  safe_free_str (info->name);
  free_widget_value_tree (info->val);
  memset (info, '\0', sizeof (widget_info));
  free (info);
}

static void
mark_widget_destroyed (Widget widget, XtPointer closure, XtPointer call_data)
{
  widget_instance *instance = (widget_instance*)closure;

  /* be very conservative */
  if (instance->widget == widget)
    instance->widget = NULL;
}

static widget_instance *
allocate_widget_instance (widget_info *info, Widget parent, Boolean pop_up_p)
{
  widget_instance *instance =
    (widget_instance *) malloc (sizeof (widget_instance));
  instance->parent = parent;
  instance->pop_up_p = pop_up_p;
  instance->info = info;
  instance->next = info->instances;
  info->instances = instance;

  instantiate_widget_instance (instance);

  XtAddCallback (instance->widget, XtNdestroyCallback,
		 mark_widget_destroyed, (XtPointer)instance);
  return instance;
}

static void
free_widget_instance (widget_instance *instance)
{
  memset (instance, '\0', sizeof (widget_instance));
  free (instance);
}

static widget_info *
get_widget_info (LWLIB_ID id, Boolean remove_p)
{
  widget_info *info;
  widget_info *prev;
  for (prev = NULL, info = all_widget_info;
       info;
       prev = info, info = info->next)
    if (info->id == id)
     {
       if (remove_p)
	 {
	   if (prev)
	     prev->next = info->next;
	   else
	     all_widget_info = info->next;
	 }
      return info;
     }
  return NULL;
}

/* Internal function used by the library dependent implementation to get the
   widget_value for a given widget in an instance */
widget_info *
lw_get_widget_info (LWLIB_ID id)
{
  return get_widget_info (id, 0);
}

static int
map_widget_values (widget_value *value, int (*mapfunc) (widget_value *value,
							void *closure),
		   void *closure)
{
  int retval = 0;

  if (value->contents)
    retval = map_widget_values (value->contents, mapfunc, closure);
  if (retval)
    return retval;

  if (value->next)
    retval = map_widget_values (value->next, mapfunc, closure);
  if (retval)
    return retval;

  return (mapfunc) (value, closure);
}

int
lw_map_widget_values (LWLIB_ID id, int (*mapfunc) (widget_value *value,
						   void *closure),
		      void *closure)
{
  widget_info *info = get_widget_info (id, 0);

  if (!info)
    abort ();

  if (info->val)
    return map_widget_values (info->val, mapfunc, closure);
  return 0;
}

static widget_instance *
get_widget_instance (Widget widget, Boolean remove_p)
{
  widget_info *info;
  widget_instance *instance;
  widget_instance *prev;
  for (info = all_widget_info; info; info = info->next)
    for (prev = NULL, instance = info->instances;
	 instance;
	 prev = instance, instance = instance->next)
      if (instance->widget == widget)
	{
	  if (remove_p)
	    {
	      if (prev)
		prev->next = instance->next;
	      else
		info->instances = instance->next;
	    }
	  return instance;
	}
  return (widget_instance *) 0;
}

static widget_instance*
find_instance (LWLIB_ID id, Widget parent, Boolean pop_up_p)
{
  widget_info *info = get_widget_info (id, False);
  widget_instance *instance;

  if (info)
    for (instance = info->instances; instance; instance = instance->next)
      if (instance->parent == parent && instance->pop_up_p == pop_up_p)
	return instance;

  return NULL;
}


/* utility function for widget_value */
static Boolean
safe_strcmp (const char *s1, const char *s2)
{
  if (!!s1 ^ !!s2) return True;
  return (s1 && s2) ? strcmp (s1, s2) : s1 ? False : !!s2;
}

#ifndef WIN32_NATIVE
static change_type
max (change_type i1, change_type i2)
{
  return (int)i1 > (int)i2 ? i1 : i2;
}
#endif


#if 0
# define EXPLAIN(name, oc, nc, desc, a1, a2)				\
   printf ("Change: \"%s\"\tmax(%s=%d,%s=%d)\t%s %d %d\n",		\
	   name,							\
	   (oc == NO_CHANGE ? "none" :					\
	    (oc == INVISIBLE_CHANGE ? "invisible" :			\
	     (oc == VISIBLE_CHANGE ? "visible" :			\
	      (oc == STRUCTURAL_CHANGE ? "structural" : "???")))),	\
	   oc,								\
	   (nc == NO_CHANGE ? "none" :					\
	    (nc == INVISIBLE_CHANGE ? "invisible" :			\
	     (nc == VISIBLE_CHANGE ? "visible" :			\
	      (nc == STRUCTURAL_CHANGE ? "structural" : "???")))),	\
	   nc, desc, a1, a2)
#else
# define EXPLAIN(name, oc, nc, desc, a1, a2)
#endif


static widget_value *
merge_widget_value (widget_value *val1, widget_value *val2, int level)
{
  change_type change;
  widget_value *merged_next;
  widget_value *merged_contents;

  if (!val1)
    {
      if (val2)
	return copy_widget_value_tree (val2, STRUCTURAL_CHANGE);
      else
	return NULL;
    }
  if (!val2)
    {
      free_widget_value_tree (val1);
      return NULL;
    }

  change = NO_CHANGE;

  if (val1->type != val2->type)
    {
      EXPLAIN (val1->name, change, STRUCTURAL_CHANGE, "type change",
	       val1->type, val2->type);
      change = max (change, STRUCTURAL_CHANGE);
      val1->type = val2->type;
    }
  if (safe_strcmp (val1->name, val2->name))
    {
      EXPLAIN (val1->name, change, STRUCTURAL_CHANGE, "name change",
	       val1->name, val2->name);
      change = max (change, STRUCTURAL_CHANGE);
      safe_free_str (val1->name);
      val1->name = safe_strdup (val2->name);
    }
  if (safe_strcmp (val1->value, val2->value))
    {
      EXPLAIN (val1->name, change, VISIBLE_CHANGE, "value change",
	       val1->value, val2->value);
      change = max (change, VISIBLE_CHANGE);
      safe_free_str (val1->value);
      val1->value = safe_strdup (val2->value);
    }
  if (safe_strcmp (val1->key, val2->key))
    {
      EXPLAIN (val1->name, change, VISIBLE_CHANGE, "key change",
	       val1->key, val2->key);
      change = max (change, VISIBLE_CHANGE);
      safe_free_str (val1->key);
      val1->key = safe_strdup (val2->key);
    }
  if (val1->accel != val2->accel)
    {
      EXPLAIN (val1->name, change, VISIBLE_CHANGE, "accelerator change",
	       val1->accel, val2->accel);
      change = max (change, VISIBLE_CHANGE);
      val1->accel = val2->accel;
    }
  if (val1->enabled != val2->enabled)
    {
      EXPLAIN (val1->name, change, VISIBLE_CHANGE, "enablement change",
	       val1->enabled, val2->enabled);
      change = max (change, VISIBLE_CHANGE);
      val1->enabled = val2->enabled;
    }
  if (val1->selected != val2->selected)
    {
      EXPLAIN (val1->name, change, VISIBLE_CHANGE, "selection change",
	       val1->selected, val2->selected);
      change = max (change, VISIBLE_CHANGE);
      val1->selected = val2->selected;
    }
  if (val1->call_data != val2->call_data)
    {
      EXPLAIN (val1->name, change, INVISIBLE_CHANGE, "call-data change",
	       val1->call_data, val2->call_data);
      change = max (change, INVISIBLE_CHANGE);
      val1->call_data = val2->call_data;
    }
#ifdef HAVE_WIDGETS
  if (merge_widget_value_args (val1, val2))
    {
      EXPLAIN (val1->name, change, VISIBLE_CHANGE, "widget change", 0, 0);
      change = max (change, VISIBLE_CHANGE);
    }
#endif

#ifdef NEED_SCROLLBARS
  if (merge_scrollbar_values (val1, val2))
    {
      EXPLAIN (val1->name, change, VISIBLE_CHANGE, "scrollbar change", 0, 0);
      change = max (change, VISIBLE_CHANGE);
    }
#endif

  if (level > 0)
    {
      merged_contents =
	merge_widget_value (val1->contents, val2->contents, level - 1);

      if (val1->contents && !merged_contents)
	{
	  EXPLAIN (val1->name, change, INVISIBLE_CHANGE, "(contents gone)",
		   0, 0);
	  change = max (change, INVISIBLE_CHANGE);
	}
      else if (merged_contents && merged_contents->change != NO_CHANGE)
	{
	  EXPLAIN (val1->name, change, INVISIBLE_CHANGE, "(contents change)",
		   0, 0);
	  change = max (change, INVISIBLE_CHANGE);
	}

      val1->contents = merged_contents;
    }

  merged_next = merge_widget_value (val1->next, val2->next, level);

  if (val1->next && !merged_next)
    {
      EXPLAIN (val1->name, change, STRUCTURAL_CHANGE, "(following gone)",
	       0, 0);
      change = max (change, STRUCTURAL_CHANGE);
    }
  else if (merged_next)
    {
      if (merged_next->change)
      {
	EXPLAIN (val1->name, change, merged_next->change, "(following change)",
		 0, 0);
      }
      change = max (change, merged_next->change);
    }

  val1->next = merged_next;

  val1->change = change;

  if (change > NO_CHANGE && val1->toolkit_data)
    {
      if (val1->free_toolkit_data)
	XtFree ((char *) val1->toolkit_data);
      val1->toolkit_data = NULL;
    }

  return val1;
}


/* modifying the widgets */
static Widget
name_to_widget (widget_instance *instance, const char *name)
{
  Widget widget = NULL;

  if (!instance->widget)
    return NULL;

  if (!strcmp (XtName (instance->widget), name))
    widget = instance->widget;
  else
    {
      int length = strlen (name) + 2;
      char *real_name = (char *) alloca (length);
      real_name [0] = '*';
      strcpy (real_name + 1, name);

      widget = XtNameToWidget (instance->widget, real_name);
    }
  return widget;
}

static void
set_one_value (widget_instance *instance, widget_value *val, Boolean deep_p)
{
  Widget widget = name_to_widget (instance, val->name);

  if (widget)
    {
#ifdef NEED_LUCID
      if (lw_lucid_widget_p (instance->widget))
	xlw_update_one_widget (instance, widget, val, deep_p);
#endif
#ifdef NEED_MOTIF
      if (lw_motif_widget_p (instance->widget))
	xm_update_one_widget (instance, widget, val, deep_p);
#endif
#ifdef NEED_ATHENA
      if (lw_xaw_widget_p (instance->widget))
	xaw_update_one_widget (instance, widget, val, deep_p);
#endif
    }
}

static void
update_one_widget_instance (widget_instance *instance, Boolean deep_p)
{
  widget_value *val;

  if (!instance->widget)
    /* the widget was destroyed */
    return;

  for (val = instance->info->val; val; val = val->next)
    if (val->change != NO_CHANGE)
      set_one_value (instance, val, deep_p);
}

static void
update_all_widget_values (widget_info *info, Boolean deep_p)
{
  widget_instance *instance;
  widget_value *val;

  for (instance = info->instances; instance; instance = instance->next)
    update_one_widget_instance (instance, deep_p);

  for (val = info->val; val; val = val->next)
    {
      val->change = NO_CHANGE;
      if (val->args)
	val->args->args_changed = False;
    }
}

void
lw_modify_all_widgets (LWLIB_ID id, widget_value *val, Boolean deep_p)
{
  widget_info *info = get_widget_info (id, False);
  widget_value *new_val;
  widget_value *next_new_val;
  widget_value *cur;
  widget_value *prev;
  widget_value *next;
  int		found;

  if (!info)
    return;

  for (new_val = val; new_val; new_val = new_val->next)
    {
      next_new_val = new_val->next;
      new_val->next = NULL;
      found = False;
      for (prev = NULL, cur = info->val; cur; prev = cur, cur = cur->next)
	if (!strcmp (cur->name, new_val->name))
	  {
	    found = True;
	    next = cur->next;
	    cur->next = NULL;
	    cur = merge_widget_value (cur, new_val, deep_p ? 1000 : 1);
	    if (prev)
	      prev->next = cur ? cur : next;
	    else
	      info->val = cur ? cur : next;
	    if (cur)
	      cur->next = next;
	    break;
	  }
      if (!found)
	{
	  /* Could not find it, add it */
	  if (prev)
	    prev->next = copy_widget_value_tree (new_val, STRUCTURAL_CHANGE);
	  else
	    info->val = copy_widget_value_tree (new_val, STRUCTURAL_CHANGE);
	}
      new_val->next = next_new_val;
    }

  update_all_widget_values (info, deep_p);
}


/* creating the widgets */

static void
initialize_widget_instance (widget_instance *instance)
{
  widget_value *val;

  for (val = instance->info->val; val; val = val->next)
    val->change = STRUCTURAL_CHANGE;

  update_one_widget_instance (instance, True);

  for (val = instance->info->val; val; val = val->next)
    {
      val->change = NO_CHANGE;
      if (val->args)
	val->args->args_changed = False;
    }
}

/* strcasecmp() is not sufficiently portable or standard,
   and it's easier just to write our own. */
static int
ascii_strcasecmp (const char *s1, const char *s2)
{
  while (1)
    {
      char c1 = *s1++;
      char c2 = *s2++;
      if (c1 >= 'A' && c1 <= 'Z') c1 += 'a' - 'A';
      if (c2 >= 'A' && c2 <= 'Z') c2 += 'a' - 'A';
      if (c1 != c2) return c1 - c2;
      if (c1 == '\0') return 0;
    }
}

static widget_creation_function
find_in_table (const char *type, const widget_creation_entry table[])
{
  const widget_creation_entry *cur;
  for (cur = table; cur->type; cur++)
    if (!ascii_strcasecmp (type, cur->type))
      return cur->function;
  return NULL;
}

static Boolean
dialog_spec_p (const char *name)
{
  /* return True if name matches [EILPQeilpq][1-9][Bb] or
     [EILPQeilpq][1-9][Bb][Rr][1-9] */
  if (!name)
    return False;

  switch (name [0])
    {
    case 'E': case 'I': case 'L': case 'P': case 'Q':
    case 'e': case 'i': case 'l': case 'p': case 'q':
      if (name [1] >= '0' && name [1] <= '9')
	{
	  if (name [2] != 'B' && name [2] != 'b')
	    return False;
	  if (!name [3])
	    return True;
	  if ((name [3] == 'T' || name [3] == 't') && !name [4])
	    return True;
	  if ((name [3] == 'R' || name [3] == 'r')
	      && name [4] >= '0' && name [4] <= '9' && !name [5])
	    return True;
	  return False;
	}
      else
	return False;

    default:
      return False;
    }
}

static void
instantiate_widget_instance (widget_instance *instance)
{
  widget_creation_function function = NULL;

#ifdef NEED_LUCID
  if (!function)
    function = find_in_table (instance->info->type, xlw_creation_table);
#endif
#ifdef NEED_MOTIF
  if (!function)
    function = find_in_table (instance->info->type, xm_creation_table);
#endif
#ifdef NEED_ATHENA
  if (!function)
    function = find_in_table (instance->info->type, xaw_creation_table);
#endif

  if (!function)
    {
      if (dialog_spec_p (instance->info->type))
	{
#ifdef LWLIB_DIALOGS_MOTIF
	  if (!function)
	    function = xm_create_dialog;
#endif
#ifdef LWLIB_DIALOGS_ATHENA
	  if (!function)
	    function = xaw_create_dialog;
#endif
#ifdef LWLIB_DIALOGS_LUCID
	  /* not yet (not ever?) */
#endif
	}
    }

  if (!function)
    {
      fprintf (stderr, "No creation function for widget type %s\n",
	       instance->info->type);
      abort ();
    }

  instance->widget = (*function) (instance);

  if (!instance->widget)
    abort ();

  /*   XtRealizeWidget (instance->widget);*/
}

void
lw_register_widget (const char *type, const char *name,
                    LWLIB_ID id, widget_value *val,
		    lw_callback pre_activate_cb, lw_callback selection_cb,
		    lw_callback post_activate_cb)
{
  if (!get_widget_info (id, False))
    allocate_widget_info (type, name, id, val, pre_activate_cb, selection_cb,
			  post_activate_cb);
}

Widget
lw_get_widget (LWLIB_ID id, Widget parent, Boolean pop_up_p)
{
  widget_instance *instance = find_instance (id, parent, pop_up_p);
  return instance ? instance->widget : NULL;
}

Widget
lw_make_widget (LWLIB_ID id, Widget parent, Boolean pop_up_p)
{
  widget_instance *instance = find_instance (id, parent, pop_up_p);

  if (!instance)
    {
      widget_info *info = get_widget_info (id, False);
      if (!info)
	return NULL;
      instance = allocate_widget_instance (info, parent, pop_up_p);
      initialize_widget_instance (instance);
    }
  if (!instance->widget)
    abort ();
  return instance->widget;
}

Widget
lw_create_widget (const char *type, const char *name,
                  LWLIB_ID id, widget_value *val,
		  Widget parent, Boolean pop_up_p, lw_callback pre_activate_cb,
		  lw_callback selection_cb, lw_callback post_activate_cb)
{
  lw_register_widget (type, name, id, val, pre_activate_cb, selection_cb,
		      post_activate_cb);
  return lw_make_widget (id, parent, pop_up_p);
}


/* destroying the widgets */
static void
destroy_one_instance (widget_instance *instance)
{
  /* Remove the destroy callback on the widget; that callback will try to
     dereference the instance object (to set its widget slot to 0, since the
     widget is dead.)  Since the instance is now dead, we don't have to worry
     about the fact that its widget is dead too.

     This happens in the Phase2Destroy of the widget, so this callback would
     not have been run until arbitrarily long after the instance was freed.
   */
  if (instance->widget)
    XtRemoveCallback (instance->widget, XtNdestroyCallback,
		      mark_widget_destroyed, (XtPointer)instance);

  if (instance->widget)
    {
      /* The else are pretty tricky here, including the empty statement
	 at the end because it would be very bad to destroy a widget
	 twice. */
#ifdef NEED_LUCID
      if (lw_lucid_widget_p (instance->widget))
	xlw_destroy_instance (instance);
      else
#endif
#ifdef NEED_MOTIF
      if (lw_motif_widget_p (instance->widget))
	xm_destroy_instance (instance);
      else
#endif
#ifdef NEED_ATHENA
      if (lw_xaw_widget_p (instance->widget))
	xaw_destroy_instance (instance);
      else
#endif
        {
          /* do not remove the empty statement */
          ;
        }
    }

  free_widget_instance (instance);
}

void
lw_destroy_widget (Widget w)
{
  widget_instance *instance = get_widget_instance (w, True);

  if (instance)
    {
      widget_info *info = instance->info;
      /* instance has already been removed from the list; free it */
      destroy_one_instance (instance);
      /* if there are no instances left, free the info too */
      if (!info->instances)
	lw_destroy_all_widgets (info->id);
    }
}

void
lw_destroy_all_widgets (LWLIB_ID id)
{
  widget_info *info = get_widget_info (id, True);
  widget_instance *instance;
  widget_instance *next;

  if (info)
    {
      for (instance = info->instances; instance; )
	{
	  next = instance->next;
	  destroy_one_instance (instance);
	  instance = next;
	}
      free_widget_info (info);
    }
}

void
lw_destroy_everything (void)
{
  while (all_widget_info)
    lw_destroy_all_widgets (all_widget_info->id);
}

void
lw_destroy_all_pop_ups (void)
{
  widget_info *info;
  widget_info *next;
  widget_instance *instance;

  for (info = all_widget_info; info; info = next)
    {
      next = info->next;
      instance = info->instances;
      if (instance && instance->pop_up_p)
	lw_destroy_all_widgets (info->id);
    }
}

Widget
lw_raise_all_pop_up_widgets (void)
{
  widget_info *info;
  widget_instance *instance;
  Widget result = NULL;

  for (info = all_widget_info; info; info = info->next)
    for (instance = info->instances; instance; instance = instance->next)
      if (instance->pop_up_p)
	{
	  Widget widget = instance->widget;
	  if (widget)
	    {
	      if (XtIsManaged (widget)
#ifdef NEED_MOTIF
		  /* What a complete load of crap!!!!
		     When a dialogShell is on the screen, it is not managed!
		   */
		  || (lw_motif_widget_p (instance->widget) &&
		      XtIsManaged (first_child (widget)))
#endif
		  )
		{
		  if (!result)
		    result = widget;
		  XMapRaised (XtDisplay (widget), XtWindow (widget));
		}
	    }
	}
  return result;
}

static void
lw_pop_all_widgets (LWLIB_ID id, Boolean up)
{
  widget_info *info = get_widget_info (id, False);
  widget_instance *instance;

  if (info)
    for (instance = info->instances; instance; instance = instance->next)
      if (instance->pop_up_p && instance->widget)
	{
#ifdef NEED_LUCID
	  if (lw_lucid_widget_p (instance->widget))
	    {
	      XtRealizeWidget (instance->widget);
	      xlw_pop_instance (instance, up);
	    }
#endif
#ifdef NEED_MOTIF
	  if (lw_motif_widget_p (instance->widget))
	    {
	      XtRealizeWidget (instance->widget);
	      xm_pop_instance (instance, up);
	    }
#endif
#ifdef NEED_ATHENA
	  if (lw_xaw_widget_p (instance->widget))
	    {
	      XtRealizeWidget (XtParent (instance->widget));
	      XtRealizeWidget (instance->widget);
	      xaw_pop_instance (instance, up);
	    }
#endif
	}
}

void
lw_pop_up_all_widgets (LWLIB_ID id)
{
  lw_pop_all_widgets (id, True);
}

void
lw_pop_down_all_widgets (LWLIB_ID id)
{
  lw_pop_all_widgets (id, False);
}

void
lw_popup_menu (Widget widget, XEvent *event)
{
#ifdef LWLIB_MENUBARS_LUCID
  if (lw_lucid_widget_p (widget))
    xlw_popup_menu (widget, event);
#endif
#ifdef LWLIB_MENUBARS_MOTIF
  if (lw_motif_widget_p (widget))
    xm_popup_menu (widget, event);
#endif
#ifdef LWLIB_MENUBARS_ATHENA
  if (lw_xaw_widget_p (widget))
    xaw_popup_menu (widget, event); /* not implemented */
#endif
}

/* get the values back */
static Boolean
get_one_value (widget_instance *instance, widget_value *val)
{
  Widget widget = name_to_widget (instance, val->name);

  if (widget)
    {
#ifdef NEED_LUCID
      if (lw_lucid_widget_p (instance->widget))
	xlw_update_one_value (instance, widget, val);
#endif
#ifdef NEED_MOTIF
      if (lw_motif_widget_p (instance->widget))
	xm_update_one_value (instance, widget, val);
#endif
#ifdef NEED_ATHENA
      if (lw_xaw_widget_p (instance->widget))
	xaw_update_one_value (instance, widget, val);
#endif
      return True;
    }
  else
    return False;
}

Boolean
lw_get_some_values (LWLIB_ID id, widget_value *val_out)
{
  widget_info *info = get_widget_info (id, False);
  widget_instance *instance;
  widget_value *val;
  Boolean result = False;

  if (!info)
    return False;

  instance = info->instances;
  if (!instance)
    return False;

  for (val = val_out; val; val = val->next)
    if (get_one_value (instance, val))
      result = True;

  return result;
}

widget_value*
lw_get_all_values (LWLIB_ID id)
{
  widget_info *info = get_widget_info (id, False);
  widget_value *val = info->val;
  if (lw_get_some_values (id, val))
    return val;
  else
    return NULL;
}

/* internal function used by the library dependent implementation to get the
   widget_value for a given widget in an instance */
widget_value*
lw_get_widget_value_for_widget (widget_instance *instance, Widget w)
{
  char *name = XtName (w);
  widget_value *cur;
  for (cur = instance->info->val; cur; cur = cur->next)
    if (!strcmp (cur->name, name))
      return cur;
  return NULL;
}


/* update other instances value when one thing changed */
/* This function can be used as a an XtCallback for the widgets that get
  modified to update other instances of the widgets.  Closure should be the
  widget_instance. */
void
lw_internal_update_other_instances (Widget widget, XtPointer closure,
				    XtPointer call_data)
{
  /* To forbid recursive calls */
  static Boolean updating;

  widget_instance *instance = (widget_instance*)closure;
  char *name = XtName (widget);
  widget_info *info;
  widget_instance *cur;
  widget_value *val;

  /* never recurse as this could cause infinite recursions. */
  if (updating)
    return;

  /* protect against the widget being destroyed */
  if (XtWidgetBeingDestroyedP (widget))
    return;

  /* Return immediately if there are no other instances */
  info = instance->info;
  if (!info->instances->next)
    return;

  updating = True;

  for (val = info->val; val && strcmp (val->name, name); val = val->next);

  if (val && get_one_value (instance, val))
    for (cur = info->instances; cur; cur = cur->next)
      if (cur != instance)
	set_one_value (cur, val, True);

  updating = False;
}



/* get the id */

LWLIB_ID
lw_get_widget_id (Widget w)
{
  widget_instance *instance = get_widget_instance (w, False);

  return instance ? instance->info->id : 0;
}


/* set the keyboard focus */
void
lw_set_keyboard_focus (Widget parent, Widget w)
{
#if defined(NEED_MOTIF) && !defined(LESSTIF_VERSION)
  /* This loses with Lesstif v0.75a */
  xm_set_keyboard_focus (parent, w);
#else
  XtSetKeyboardFocus (parent, w);
#endif
}


/* Show busy */
static void
show_one_widget_busy (Widget w, Boolean flag)
{
  Pixel foreground = 0;
  Pixel background = 1;
  Widget widget_to_invert = XtNameToWidget (w, "*sheet");
  Arg al [2];

  if (!widget_to_invert)
    widget_to_invert = w;

  XtSetArg (al [0], XtNforeground, &foreground);
  XtSetArg (al [1], XtNbackground, &background);
  XtGetValues (widget_to_invert, al, 2);

  XtSetArg (al [0], XtNforeground, background);
  XtSetArg (al [1], XtNbackground, foreground);
  XtSetValues (widget_to_invert, al, 2);
}

void
lw_show_busy (Widget w, Boolean busy)
{
  widget_instance *instance = get_widget_instance (w, False);
  widget_info *info;
  widget_instance *next;

  if (instance)
    {
      info = instance->info;
      if (info->busy != busy)
	{
	  for (next = info->instances; next; next = next->next)
	    if (next->widget)
	      show_one_widget_busy (next->widget, busy);
	  info->busy = busy;
	}
    }
}

void lw_add_value_args_to_args (widget_value* wv, ArgList addto, int* offset)
{
  int i;
  if (wv->args && wv->args->nargs)
    {
      for (i = 0; i<wv->args->nargs; i++)
	{
	  addto[i + *offset] = wv->args->args[i];
	}
      *offset += wv->args->nargs;
    }
}

XtArgVal lw_get_value_arg (widget_value* wv, String name)
{
  int i;
  if (wv->args)
    {
      for (i = 0; i < wv->args->nargs; i++)
	{
	  if (!strcmp (wv->args->args[i].name, name))
	    {
	      return wv->args->args[i].value;
	    }
	}
    }
  return (XtArgVal)0;
}

void lw_add_widget_value_arg (widget_value* wv, String name, XtArgVal value)
{
  int i = 0;
  if (!wv->args)
    {
      wv->args = (widget_args *) malloc (sizeof (widget_args));
      memset (wv->args, '\0', sizeof (widget_args));
      wv->args->ref_count = 1;
      wv->args->nargs = 0;
      wv->args->args = (ArgList) malloc (sizeof (Arg) * 10);
      memset (wv->args->args, '\0', sizeof (Arg) * 10);
    }
  
  if (wv->args->nargs > 10)
    return;

  /* Register the change. */
  wv->args->args_changed = True;
  /* If the arg is already there then we must replace it. */
  for (i = 0; i < wv->args->nargs; i++)
    {
      if (!strcmp (wv->args->args[i].name, name))
	{
	  XtSetArg (wv->args->args [i], name, value);
	  break;
	}
    }
  if (i >= wv->args->nargs)
    {
      XtSetArg (wv->args->args [wv->args->nargs], name, value);   wv->args->nargs++;
    }
}

static void free_widget_value_args (widget_value* wv)
{
  if (wv->args)
    {
      if (--wv->args->ref_count <= 0)
	{
#ifdef LWLIB_WIDGETS_MOTIF
	  int i;
	  for (i = 0; i < wv->args->nargs; i++)
	    {
	      if (!strcmp (wv->args->args[i].name, XmNfontList))
		XmFontListFree ((XmFontList)wv->args->args[i].value);
	    }
#endif
	  free (wv->args->args);
	  free (wv->args);
	  wv->args = 0;
	}
    }
}

void lw_copy_widget_value_args (widget_value* val, widget_value* copy)
{
  if (val == copy || val->args == copy->args)
    return;

  if (copy->args)
    {
      free_widget_value_args (copy);
    }

  if (val->args)
    {
      copy->args = val->args;
      copy->args->ref_count++;
    }
}

/* Remove %_ and convert %% to %.  We can do this in-place because we
   are always shortening, never lengthening, the string. */
void
lw_remove_accelerator_spec (char *val)
{
  char *foo = val, *bar = val;

  while (*bar)
    {
      if (*bar == '%' && *(bar+1) == '_')
	bar += 2;
      else if (*bar == '%' && *(bar+1) == '%')
	{
	  *foo++ = *bar++;
	  bar++;
	}
      else
	*foo++ = *bar++;
    }
  *foo = '\0';
}
