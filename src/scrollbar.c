/* Generic scrollbar implementation.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995 Darrell Kindred <dkindred+@cmu.edu>.

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

/* This file has been Mule-ized. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "commands.h"
#include "scrollbar.h"
#include "device.h"
#include "frame.h"
#include "glyphs.h"
#include "gutter.h"
#include "window.h"

Lisp_Object Qinit_scrollbar_from_resources;

Lisp_Object Qscrollbar_line_up;
Lisp_Object Qscrollbar_line_down;
Lisp_Object Qscrollbar_page_up;
Lisp_Object Qscrollbar_page_down;
Lisp_Object Qscrollbar_to_top;
Lisp_Object Qscrollbar_to_bottom;
Lisp_Object Qscrollbar_vertical_drag;

Lisp_Object Qscrollbar_char_left;
Lisp_Object Qscrollbar_char_right;
Lisp_Object Qscrollbar_page_left;
Lisp_Object Qscrollbar_page_right;
Lisp_Object Qscrollbar_to_left;
Lisp_Object Qscrollbar_to_right;
Lisp_Object Qscrollbar_horizontal_drag;

#define DEFAULT_SCROLLBAR_WIDTH 15
#define DEFAULT_SCROLLBAR_HEIGHT 15

/* Width and height of the scrollbar. */
Lisp_Object Vscrollbar_width;
Lisp_Object Vscrollbar_height;

/* Scrollbar visibility specifiers */
Lisp_Object Vhorizontal_scrollbar_visible_p;
Lisp_Object Vvertical_scrollbar_visible_p;

/* Scrollbar location specifiers */
Lisp_Object Vscrollbar_on_left_p;
Lisp_Object Vscrollbar_on_top_p;

Lisp_Object Vscrollbar_pointer_glyph;

EXFUN (Fcenter_to_window_line, 2);

static void update_scrollbar_instance (struct window *w, int vertical,
				       struct scrollbar_instance *instance);


static void
free_scrollbar_instance (struct scrollbar_instance *instance,
			 struct frame *frame)
{
  if (!instance)
    return;
  else
    {
      struct device *d = XDEVICE (frame->device);

      MAYBE_DEVMETH (d, free_scrollbar_instance, (instance));
      xfree (instance);
    }
}

static void
free_window_mirror_scrollbars (struct window_mirror *mir)
{
  free_scrollbar_instance (mir->scrollbar_vertical_instance, mir->frame);
  mir->scrollbar_vertical_instance = 0;

  free_scrollbar_instance (mir->scrollbar_horizontal_instance, mir->frame);
  mir->scrollbar_horizontal_instance = 0;
}

static struct window_mirror *
free_scrollbars_loop (Lisp_Object window, struct window_mirror *mir)
{
  struct window_mirror *retval = NULL;

  while (mir)
    {
      assert (!NILP (window));

      if (mir->vchild)
	{
	  retval = free_scrollbars_loop (XWINDOW (window)->vchild,
					 mir->vchild);
	}
      else if (mir->hchild)
	{
	  retval = free_scrollbars_loop (XWINDOW (window)->hchild,
					 mir->hchild);
	}

      if (retval != NULL)
	return retval;

      if (mir->scrollbar_vertical_instance ||
	  mir->scrollbar_horizontal_instance)
	free_window_mirror_scrollbars (mir);

      mir = mir->next;
      window = XWINDOW (window)->next;
    }

  return NULL;
}

/* Destroy all scrollbars associated with FRAME.  Only called from
   delete_frame_internal. */
void
free_frame_scrollbars (struct frame *f)
{
  if (!HAS_FRAMEMETH_P (f, create_scrollbar_instance))
    return;

  if (f->mirror_dirty)
    update_frame_window_mirror (f);

  free_scrollbars_loop (f->root_window, f->root_mirror);

  while (FRAME_SB_VCACHE (f))
    {
      struct scrollbar_instance *tofree = FRAME_SB_VCACHE (f);
      FRAME_SB_VCACHE (f) = FRAME_SB_VCACHE (f)->next;
      tofree->next = NULL;
      free_scrollbar_instance (tofree, f);
    }

  while (FRAME_SB_HCACHE (f))
    {
      struct scrollbar_instance *tofree = FRAME_SB_HCACHE (f);
      FRAME_SB_HCACHE (f) = FRAME_SB_HCACHE (f)->next;
      tofree->next = NULL;
      free_scrollbar_instance (tofree, f);
    }
}


static struct scrollbar_instance *
create_scrollbar_instance (struct frame *f, int vertical)
{
  struct device *d = XDEVICE (f->device);
  struct scrollbar_instance *instance =
    xnew_and_zero (struct scrollbar_instance);

  MAYBE_DEVMETH (d, create_scrollbar_instance, (f, vertical, instance));

  return instance;
}


#define GET_SCROLLBAR_INSTANCE_INTERNAL(cache)				\
  do {									\
    if (FRAME_SB_##cache (f))						\
      {									\
        struct scrollbar_instance *retval = FRAME_SB_##cache (f);	\
        FRAME_SB_##cache (f) = FRAME_SB_##cache (f)->next;		\
        retval->next = NULL;						\
        return retval;							\
      }									\
  } while (0)

static struct scrollbar_instance *
get_scrollbar_instance (struct frame *f, int vertical)
{
  /* Check if there are any available scrollbars already in existence. */
  if (vertical)
    GET_SCROLLBAR_INSTANCE_INTERNAL (VCACHE);
  else
    GET_SCROLLBAR_INSTANCE_INTERNAL (HCACHE);

  return create_scrollbar_instance (f, vertical);
}
#undef GET_SCROLLBAR_INSTANCE_INTERNAL

#define RELEASE_SCROLLBAR_INSTANCE_INTERNAL(cache)			\
  do {									\
    if (!FRAME_SB_##cache (f))						\
      {									\
	instance->next = NULL;						\
	FRAME_SB_##cache (f) = instance;				\
      }									\
    else								\
      {									\
	instance->next = FRAME_SB_##cache (f);				\
	FRAME_SB_##cache (f) = instance;				\
      }									\
  } while (0)

static void
release_scrollbar_instance (struct frame *f, int vertical,
			    struct scrollbar_instance *instance)
{
  /* #### should we do "instance->mir = 0;" for safety? */
  if (vertical)
    RELEASE_SCROLLBAR_INSTANCE_INTERNAL (VCACHE);
  else
    RELEASE_SCROLLBAR_INSTANCE_INTERNAL (HCACHE);
}
#undef RELEASE_SCROLLBAR_INSTANCE_INTERNAL

#ifdef MEMORY_USAGE_STATS

int
compute_scrollbar_instance_usage (struct device *d,
				  struct scrollbar_instance *inst,
				  struct overhead_stats *ovstats)
{
  int total = 0;

  total += DEVMETH (d, compute_scrollbar_instance_usage, (d, inst, ovstats));

  while (inst)
    {
      total += malloced_storage_size (inst, sizeof (*inst), ovstats);
      inst = inst->next;
    }

  return total;
}

#endif /* MEMORY_USAGE_STATS */

void
update_window_scrollbars (struct window *w, struct window_mirror *mirror,
			  int active, int horiz_only)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);

  if (!HAS_DEVMETH_P (d, create_scrollbar_instance))
    return;

  in_display++;

  /* It is possible for this to get called from the mirror update
     routines.  In that case the structure is in an indeterminate
     state but we know exactly what struct we are working with.  So we
     pass it in in that case.  We also take advantage of it at some
     other points where we know what the mirror struct is. */
  if (!mirror)
    mirror = find_window_mirror (w);

  if (!mirror->scrollbar_vertical_instance && active)
    mirror->scrollbar_vertical_instance = get_scrollbar_instance (f, 1);

  if (!mirror->scrollbar_horizontal_instance && active)
    mirror->scrollbar_horizontal_instance = get_scrollbar_instance (f, 0);

  if (!horiz_only && mirror->scrollbar_vertical_instance)
    {
      int size = (active ? window_scrollbar_width (w) : 0);
      struct scrollbar_instance *instance;

      instance = mirror->scrollbar_vertical_instance;
      instance->scrollbar_is_active = active;
      instance->mirror = mirror;

      if (active && size)
	update_scrollbar_instance (w, 1, instance);
      MAYBE_DEVMETH (d, update_scrollbar_instance_status,
		     (w, active, size, instance));

      if (!active)
 	{
 	  release_scrollbar_instance (f, 1, instance);
 	  mirror->scrollbar_vertical_instance = NULL;
 	}
    }

  if (mirror->scrollbar_horizontal_instance)
    {
      int size = (active ? window_scrollbar_height (w) : 0);
      struct scrollbar_instance *instance;

      instance = mirror->scrollbar_horizontal_instance;
      instance->scrollbar_is_active = active;
      instance->mirror = mirror;

      if (active && size)
	update_scrollbar_instance (w, 0, instance);
      MAYBE_DEVMETH (d, update_scrollbar_instance_status,
		     (w, active, size, instance));

      if (!active)
 	{
 	  release_scrollbar_instance (f, 0, instance);
 	  mirror->scrollbar_horizontal_instance = NULL;
 	}
    }

  in_display--;
}

void
release_window_mirror_scrollbars (struct window_mirror *mir)
{
  struct device *d = XDEVICE (mir->frame->device);

  if (!HAS_DEVMETH_P (d, create_scrollbar_instance))
    return;

  if (mir->scrollbar_vertical_instance)
    {
      release_scrollbar_instance (mir->frame, 1,
				  mir->scrollbar_vertical_instance);
      MAYBE_DEVMETH (d, release_scrollbar_instance,
		     (mir->scrollbar_vertical_instance));
    }
  mir->scrollbar_vertical_instance = 0;

  if (mir->scrollbar_horizontal_instance)
    {
      release_scrollbar_instance (mir->frame, 0,
				  mir->scrollbar_horizontal_instance);
      MAYBE_DEVMETH (d, release_scrollbar_instance,
		     (mir->scrollbar_horizontal_instance));
    }
  mir->scrollbar_horizontal_instance = 0;
}

/*
 * If w->sb_point is on the top line then return w->sb_point else
 * return w->start.  If flag, then return beginning point of line
 * which w->sb_point lies on.
 */
static Bufpos
scrollbar_point (struct window *w, int flag)
{
  Bufpos start_pos, end_pos, sb_pos;
  Lisp_Object buf;
  struct buffer *b;

  if (NILP (w->buffer)) /* non-leaf window */
    return 0;

  start_pos = marker_position (w->start[CURRENT_DISP]);
  sb_pos = marker_position (w->sb_point);

  if (!flag && sb_pos < start_pos)
    return start_pos;

  buf = get_buffer (w->buffer, 0);
  if (!NILP (buf))
    b = XBUFFER (buf);
  else
    return start_pos;

  if (flag)
    end_pos = find_next_newline_no_quit (b, sb_pos, -1);
  else
    end_pos = find_next_newline_no_quit (b, start_pos, 1);

  if (flag)
    return end_pos;
  else if (sb_pos > end_pos)
    return start_pos;
  else
    return sb_pos;
}

/*
 * Update a window's horizontal or vertical scrollbar.
 */
static void
update_scrollbar_instance (struct window *w, int vertical,
			   struct scrollbar_instance *instance)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  struct buffer *b = XBUFFER (w->buffer);
  Bufpos start_pos, end_pos, sb_pos;
  int scrollbar_width  = window_scrollbar_width  (w);
  int scrollbar_height = window_scrollbar_height (w);

  int new_line_increment = -1, new_page_increment = -1;
  int new_minimum = -1, new_maximum = -1;
  int new_slider_size = -1, new_slider_position = -1;
  int new_width = -1, new_height = -1, new_x = -1, new_y = -1;
  struct window *new_window = 0; /* #### currently unused */

  end_pos = BUF_Z (b) - w->window_end_pos[CURRENT_DISP];
  sb_pos = scrollbar_point (w, 0);
  start_pos = sb_pos;

  /* The end position must be strictly greater than the start
     position, at least for the Motify scrollbar.  It shouldn't hurt
     anything for other scrollbar implementations. */
  if (end_pos <= start_pos)
    end_pos = start_pos + 1;

  if (vertical)
    {
      new_height = WINDOW_TEXT_HEIGHT (w);
      new_width = scrollbar_width;
    }
  else
    {
      new_height = scrollbar_height;
      new_width = WINDOW_TEXT_WIDTH (w);
    }

  /* If the height and width are not greater than 0, then later on the
     Motif widgets will bitch and moan. */
  if (new_height <= 0)
    new_height = 1;
  if (new_width <= 0)
    new_width = 1;

  assert (instance->mirror && XWINDOW (real_window(instance->mirror, 0)) == w);

  /* Only character-based scrollbars are implemented at the moment.
     Line-based will be implemented in the future. */

  instance->scrollbar_is_active = 1;
  new_line_increment = 1;
  new_page_increment = 1;

  /* We used to check for inhibit_scrollbar_slider_size_change here,
     but that seems bogus.  */
  {
    int x_offset, y_offset;

    /* Scrollbars are always the farthest from the text area, barring
       gutters. */
    if (vertical)
      {
	if (!NILP (w->scrollbar_on_left_p))
	  {
	    x_offset = WINDOW_LEFT (w);
	  }
	else
	  {
	    x_offset = WINDOW_RIGHT (w) - scrollbar_width;
	    if (window_needs_vertical_divider (w))
	      x_offset -= window_divider_width (w);
	  }
	y_offset = WINDOW_TEXT_TOP (w) + f->scrollbar_y_offset;
      }
    else
      {
	x_offset = WINDOW_TEXT_LEFT (w);
	y_offset = f->scrollbar_y_offset;

	if (!NILP (w->scrollbar_on_top_p))
	  {
	    y_offset += WINDOW_TOP (w);
	  }
	else
	  {
	    y_offset += WINDOW_TEXT_BOTTOM (w);
	  }
      }

    new_x = x_offset;
    new_y = y_offset;
  }

  /* A disabled scrollbar has its slider sized to the entire height of
     the scrollbar.  Currently the minibuffer scrollbar is
     disabled. */
  if (!MINI_WINDOW_P (w) && vertical)
    {
      if (!DEVMETH_OR_GIVEN (d, inhibit_scrollbar_slider_size_change, (), 0))
	{
	  new_minimum = BUF_BEGV (b);
	  new_maximum = max (BUF_ZV (b), new_minimum + 1);
	  new_slider_size = min ((end_pos - start_pos),
				 (new_maximum - new_minimum));
	  new_slider_position = sb_pos;
	  new_window = w;
	}
    }
  else if (!MINI_WINDOW_P (w))
    {
      /* The minus one is to account for the truncation glyph. */
      int wcw = window_char_width (w, 0) - 1;
      int max_width, max_slide;

      if (w->max_line_len < wcw)
	{
	  max_width = 1;
	  max_slide = 1;
	  wcw = 1;
	}
      else
	{
	  max_width = w->max_line_len + 2;
	  max_slide = max_width - wcw;
	}

      new_minimum = 0;
      new_maximum = max_width;
      new_slider_size = wcw;
      new_slider_position = min (w->hscroll, max_slide);
    }
  else /* MINI_WINDOW_P (w) */
    {
      new_minimum = 1;
      new_maximum = 2;
      new_slider_size = 1;
      new_slider_position = 1;
      instance->scrollbar_is_active = 0;
    }

  DEVMETH (d, update_scrollbar_instance_values, (w, instance,
						 new_line_increment,
						 new_page_increment,
						 new_minimum,
						 new_maximum,
						 new_slider_size,
						 new_slider_position,
						 new_width, new_height,
						 new_x, new_y));
}

void
init_frame_scrollbars (struct frame *f)
{
  struct device *d = XDEVICE (f->device);

  if (HAS_DEVMETH_P (d, create_scrollbar_instance))
    {
      int depth = unlock_ghost_specifiers_protected ();
      Lisp_Object frame;
      XSETFRAME (frame, f);
      call_critical_lisp_code (XDEVICE (FRAME_DEVICE (f)),
			       Qinit_scrollbar_from_resources,
			       frame);
      unbind_to (depth, Qnil);
    }
}

void
init_device_scrollbars (struct device *d)
{
  if (HAS_DEVMETH_P (d, create_scrollbar_instance))
    {
      int depth = unlock_ghost_specifiers_protected ();
      Lisp_Object device;
      XSETDEVICE (device, d);
      call_critical_lisp_code (d,
			       Qinit_scrollbar_from_resources,
			       device);
      unbind_to (depth, Qnil);
    }
}

void
init_global_scrollbars (struct device *d)
{
  if (HAS_DEVMETH_P (d, create_scrollbar_instance))
    {
      int depth = unlock_ghost_specifiers_protected ();
      call_critical_lisp_code (d,
			       Qinit_scrollbar_from_resources,
			       Qglobal);
      unbind_to (depth, Qnil);
    }
}

static void
vertical_scrollbar_changed_in_window (Lisp_Object specifier,
				      struct window *w,
				      Lisp_Object oldval)
{
  /* Hold on your cerebella guys. If we always show the dividers,
     changing scrollbar affects only how the text and scrollbar are
     laid out in the window. If we do not want the dividers to show up
     always, then we mark more drastic change, because changing
     divider appearance changes lotta things. Although we actually need
     to do this only if the scrollbar has appeared or disappeared
     completely at either window edge, we do this always, as users
     usually do not reposition scrollbars 200 times a second or so. Do
     you? */
  if (NILP (w->vertical_divider_always_visible_p))
    MARK_FRAME_WINDOWS_STRUCTURE_CHANGED (XFRAME (WINDOW_FRAME (w)));
  else
    MARK_WINDOWS_CHANGED (w);
}

/* This function is called as a result of a change to the
   `scrollbar-pointer' glyph.  */
static void
scrollbar_pointer_changed_in_window (Lisp_Object specifier, struct window *w,
				     Lisp_Object oldval)
{
  struct frame *f = XFRAME (WINDOW_FRAME (w));

  if (f->init_finished)
    MAYBE_FRAMEMETH (f, scrollbar_pointer_changed_in_window, (w));
}

/* ####

   All of the following stuff is functions that handle scrollbar
   actions.  All of it should be moved into Lisp.  This may require
   adding some badly-needed primitives. */

/********** vertical scrollbar stuff **********/

/*
 * If the original point is still visible, put the cursor back there.
 * Otherwise, when scrolling down stick it at the beginning of the
 * first visible line and when scrolling up stick it at the beginning
 * of the last visible line.
 */

/* #### This function should be moved into Lisp */
static void
scrollbar_reset_cursor (Lisp_Object win, Lisp_Object orig_pt)
{
  /* When this function is called we know that start is already
     accurate.  We know this because either set-window-start or
     recenter was called immediately prior to it being called. */
  Lisp_Object buf;
  Bufpos start_pos = XINT (Fwindow_start (win));
  Bufpos ptint = XINT (orig_pt);
  struct window *w = XWINDOW (win);
  int selected = ((w == XWINDOW (Fselected_window (XFRAME (w->frame)->device)))
		  ? 1
		  : 0);

  buf = Fwindow_buffer (win);
  if (NILP (buf))
    return;	/* the window was deleted out from under us */

  if (ptint < XINT (Fwindow_start (win)))
    {
      if (selected)
	Fgoto_char (make_int (start_pos), buf);
      else
	Fset_window_point (win, make_int (start_pos));
    }
  else if (!point_would_be_visible (XWINDOW (win), start_pos, ptint))
    {
      Fmove_to_window_line (make_int (-1), win);

      if (selected)
	Fbeginning_of_line (Qnil, buf);
      else
	{
	  /* #### Taken from forward-line. */
	  Bufpos pos;

	  pos = find_next_newline (XBUFFER (buf),
				   marker_position (w->pointm[CURRENT_DISP]),
				   -1);
	  Fset_window_point (win, make_int (pos));
	}
    }
  else
    {
      if (selected)
	Fgoto_char (orig_pt, buf);
      else
	Fset_window_point (win, orig_pt);
    }
}

DEFUN ("scrollbar-line-up", Fscrollbar_line_up, 1, 1, 0, /*
Function called when the line-up arrow on the scrollbar is clicked.
This is the little arrow at the top of the scrollbar.  One argument, the
scrollbar's window.  You can advise this function to change the scrollbar
behavior.
*/
       (window))
{
  CHECK_LIVE_WINDOW (window);
  window_scroll (window, make_int (1), -1, ERROR_ME_NOT);
  zmacs_region_stays = 1;
  return Qnil;
}

DEFUN ("scrollbar-line-down", Fscrollbar_line_down, 1, 1, 0, /*
Function called when the line-down arrow on the scrollbar is clicked.
This is the little arrow at the bottom of the scrollbar.  One argument, the
scrollbar's window.  You can advise this function to change the scrollbar
behavior.
*/
       (window))
{
  CHECK_LIVE_WINDOW (window);
  window_scroll (window, make_int (1), 1, ERROR_ME_NOT);
  zmacs_region_stays = 1;
  return Qnil;
}

DEFUN ("scrollbar-page-up", Fscrollbar_page_up, 1, 1, 0, /*
Function called when the user gives the "page-up" scrollbar action.
\(The way this is done can vary from scrollbar to scrollbar.) One argument,
a cons containing the scrollbar's window and a value (#### document me!
This value is nil for Motif/Lucid scrollbars and a number for Athena
scrollbars).  You can advise this function to change the scrollbar
behavior.
*/
       (object))
{
  Lisp_Object window = Fcar (object);

  CHECK_LIVE_WINDOW (window);
  /* Motif and Athena scrollbars behave differently, but in accordance
     with their standard behaviors.  It is not possible to hide the
     differences down in lwlib because knowledge of XEmacs buffer and
     cursor motion routines is necessary. */

  if (NILP (XCDR (object)))
    window_scroll (window, Qnil, -1, ERROR_ME_NOT);
  else
    {
      Bufpos bufpos;
      Lisp_Object value = Fcdr (object);

      CHECK_INT (value);
      Fmove_to_window_line (Qzero, window);
      /* can't use Fvertical_motion() because it moves the buffer point
	 rather than the window's point.

	 #### It does?  Why does it take a window argument then? */
      bufpos = vmotion (XWINDOW (window), XINT (Fwindow_point (window)),
			XINT (value), 0);
      Fset_window_point (window, make_int (bufpos));
      Fcenter_to_window_line (Qzero, window);
    }

  zmacs_region_stays = 1;
  return Qnil;
}

DEFUN ("scrollbar-page-down", Fscrollbar_page_down, 1, 1, 0, /*
Function called when the user gives the "page-down" scrollbar action.
\(The way this is done can vary from scrollbar to scrollbar.) One argument,
a cons containing the scrollbar's window and a value (#### document me!
This value is nil for Motif/Lucid scrollbars and a number for Athena
scrollbars).  You can advise this function to change the scrollbar
behavior.
*/
       (object))
{
  Lisp_Object window = Fcar (object);

  CHECK_LIVE_WINDOW (window);
  /* Motif and Athena scrollbars behave differently, but in accordance
     with their standard behaviors.  It is not possible to hide the
     differences down in lwlib because knowledge of XEmacs buffer and
     cursor motion routines is necessary. */

  if (NILP (XCDR (object)))
    window_scroll (window, Qnil, 1, ERROR_ME_NOT);
  else
    {
      Lisp_Object value = Fcdr (object);
      CHECK_INT (value);
      Fmove_to_window_line (value, window);
      Fcenter_to_window_line (Qzero, window);
    }

  zmacs_region_stays = 1;
  return Qnil;
}

DEFUN ("scrollbar-to-top", Fscrollbar_to_top, 1, 1, 0, /*
Function called when the user invokes the "to-top" scrollbar action.
The way this is done can vary from scrollbar to scrollbar, but
C-button1 on the up-arrow is very common. One argument, the
scrollbar's window.  You can advise this function to change the
scrollbar behavior.
*/
       (window))
{
  Lisp_Object orig_pt = Fwindow_point (window);
  Fset_window_point (window, Fpoint_min (Fwindow_buffer (window)));
  Fcenter_to_window_line (Qzero, window);
  scrollbar_reset_cursor (window, orig_pt);
  zmacs_region_stays = 1;
  return Qnil;
}

DEFUN ("scrollbar-to-bottom", Fscrollbar_to_bottom, 1, 1, 0, /*
Function called when the user invokes the "to-bottom" scrollbar action.
The way this is done can vary from scrollbar to scrollbar, but
C-button1 on the down-arrow is very common. One argument, the
scrollbar's window.  You can advise this function to change the
scrollbar behavior.
*/
       (window))
{
  Lisp_Object orig_pt = Fwindow_point (window);
  Fset_window_point (window, Fpoint_max (Fwindow_buffer (window)));
  Fcenter_to_window_line (make_int (-3), window);
  scrollbar_reset_cursor (window, orig_pt);
  zmacs_region_stays = 1;
  return Qnil;
}

DEFUN ("scrollbar-vertical-drag", Fscrollbar_vertical_drag, 1, 1, 0, /*
Function called when the user drags the vertical scrollbar slider.
One argument, a cons containing the scrollbar's window and a value
between point-min and point-max.  You can advise this function to
change the scrollbar behavior.
*/
       (object))
{
  Bufpos start_pos;
  Lisp_Object orig_pt;
  Lisp_Object window = Fcar (object);
  Lisp_Object value = Fcdr (object);

  orig_pt = Fwindow_point (window);
  Fset_marker (XWINDOW (window)->sb_point, value, Fwindow_buffer (window));
  start_pos = scrollbar_point (XWINDOW (window), 1);
  Fset_window_start (window, make_int (start_pos), Qnil);
  scrollbar_reset_cursor (window, orig_pt);
  Fsit_for(Qzero, Qnil);
  zmacs_region_stays = 1;
  return Qnil;
}

DEFUN ("scrollbar-set-hscroll", Fscrollbar_set_hscroll, 2, 2, 0, /*
Set WINDOW's hscroll position to VALUE.
This ensures that VALUE is in the proper range for the horizontal scrollbar.
*/
       (window, value))
{
  struct window *w;
  int hscroll, wcw, max_len;

  CHECK_LIVE_WINDOW (window);
  if (!EQ (value, Qmax))
    CHECK_INT (value);

  w = XWINDOW (window);
  wcw = window_char_width (w, 0) - 1;
  /* #### We should be able to scroll further right as long as there is
     a visible truncation glyph.  This calculation for max is bogus.  */
  max_len = w->max_line_len + 2;

  if (EQ (value, Qmax) || (XINT (value) > (max_len - wcw)))
    hscroll = max_len - wcw;
  else
    hscroll = XINT (value);

  /* Can't allow this out of set-window-hscroll's acceptable range. */
  /* #### What hell on the earth this code limits scroll size to the
     machine-dependent SHORT size? -- kkm */
  if (hscroll < 0)
    hscroll = 0;
  else if (hscroll >= (1 << (SHORTBITS - 1)) - 1)
    hscroll = (1 << (SHORTBITS - 1)) - 1;

  if (hscroll != w->hscroll)
    Fset_window_hscroll (window, make_int (hscroll));

  return Qnil;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_scrollbar (void)
{
  defsymbol (&Qscrollbar_line_up, "scrollbar-line-up");
  defsymbol (&Qscrollbar_line_down, "scrollbar-line-down");
  defsymbol (&Qscrollbar_page_up, "scrollbar-page-up");
  defsymbol (&Qscrollbar_page_down, "scrollbar-page-down");
  defsymbol (&Qscrollbar_to_top, "scrollbar-to-top");
  defsymbol (&Qscrollbar_to_bottom, "scrollbar-to-bottom");
  defsymbol (&Qscrollbar_vertical_drag, "scrollbar-vertical-drag");

  defsymbol (&Qscrollbar_char_left, "scrollbar-char-left");
  defsymbol (&Qscrollbar_char_right, "scrollbar-char-right");
  defsymbol (&Qscrollbar_page_left, "scrollbar-page-left");
  defsymbol (&Qscrollbar_page_right, "scrollbar-page-right");
  defsymbol (&Qscrollbar_to_left, "scrollbar-to-left");
  defsymbol (&Qscrollbar_to_right, "scrollbar-to-right");
  defsymbol (&Qscrollbar_horizontal_drag, "scrollbar-horizontal-drag");

  defsymbol (&Qinit_scrollbar_from_resources, "init-scrollbar-from-resources");

  /* #### All these functions should be moved into Lisp.
     See comment above. */
  DEFSUBR (Fscrollbar_line_up);
  DEFSUBR (Fscrollbar_line_down);
  DEFSUBR (Fscrollbar_page_up);
  DEFSUBR (Fscrollbar_page_down);
  DEFSUBR (Fscrollbar_to_top);
  DEFSUBR (Fscrollbar_to_bottom);
  DEFSUBR (Fscrollbar_vertical_drag);

  DEFSUBR (Fscrollbar_set_hscroll);
}

void
vars_of_scrollbar (void)
{
  DEFVAR_LISP ("scrollbar-pointer-glyph", &Vscrollbar_pointer_glyph /*
*The shape of the mouse-pointer when over a scrollbar.
This is a glyph; use `set-glyph-image' to change it.
If unspecified in a particular domain, the window-system-provided
default pointer is used.
*/ );

  Fprovide (intern ("scrollbar"));
}

void
specifier_vars_of_scrollbar (void)
{
  DEFVAR_SPECIFIER ("scrollbar-width", &Vscrollbar_width /*
*Width of vertical scrollbars.
This is a specifier; use `set-specifier' to change it.
*/ );
  Vscrollbar_width = make_magic_specifier (Qnatnum);
  set_specifier_fallback
    (Vscrollbar_width,
     list1 (Fcons (Qnil, make_int (DEFAULT_SCROLLBAR_WIDTH))));
  set_specifier_caching (Vscrollbar_width,
			 offsetof (struct window, scrollbar_width),
			 vertical_scrollbar_changed_in_window,
			 offsetof (struct frame, scrollbar_width),
			 frame_size_slipped, 0);

  DEFVAR_SPECIFIER ("scrollbar-height", &Vscrollbar_height /*
*Height of horizontal scrollbars.
This is a specifier; use `set-specifier' to change it.
*/ );
  Vscrollbar_height = make_magic_specifier (Qnatnum);
  set_specifier_fallback
    (Vscrollbar_height,
     list1 (Fcons (Qnil, make_int (DEFAULT_SCROLLBAR_HEIGHT))));
  set_specifier_caching (Vscrollbar_height,
			 offsetof (struct window, scrollbar_height),
			 some_window_value_changed,
			 offsetof (struct frame, scrollbar_height),
			 frame_size_slipped, 0);

  DEFVAR_SPECIFIER ("horizontal-scrollbar-visible-p", &Vhorizontal_scrollbar_visible_p /*
*Whether the horizontal scrollbar is visible.
This is a specifier; use `set-specifier' to change it.
*/ );
  Vhorizontal_scrollbar_visible_p = Fmake_specifier (Qboolean);
  set_specifier_fallback (Vhorizontal_scrollbar_visible_p,
			  list1 (Fcons (Qnil, Qt)));
  set_specifier_caching (Vhorizontal_scrollbar_visible_p,
			 offsetof (struct window,
				   horizontal_scrollbar_visible_p),
			 some_window_value_changed,
			 offsetof (struct frame,
				   horizontal_scrollbar_visible_p),
			 frame_size_slipped, 0);

  DEFVAR_SPECIFIER ("vertical-scrollbar-visible-p", &Vvertical_scrollbar_visible_p /*
*Whether the vertical scrollbar is visible.
This is a specifier; use `set-specifier' to change it.
*/ );
  Vvertical_scrollbar_visible_p = Fmake_specifier (Qboolean);
  set_specifier_fallback (Vvertical_scrollbar_visible_p,
			  list1 (Fcons (Qnil, Qt)));
  set_specifier_caching (Vvertical_scrollbar_visible_p,
			 offsetof (struct window,
				   vertical_scrollbar_visible_p),
			 vertical_scrollbar_changed_in_window,
			 offsetof (struct frame,
				   vertical_scrollbar_visible_p),
			 frame_size_slipped, 0);

  DEFVAR_SPECIFIER ("scrollbar-on-left-p", &Vscrollbar_on_left_p /*
*Whether the vertical scrollbar is on the left side of window or frame.
This is a specifier; use `set-specifier' to change it.
*/ );
  Vscrollbar_on_left_p = Fmake_specifier (Qboolean);

  {
    /* Kludge. Under X, we want athena scrollbars on the left,
       while all other scrollbars go on the right by default. */
    Lisp_Object fallback = list1 (Fcons (Qnil, Qnil));
#if defined (HAVE_X_WINDOWS)			\
    && !defined (LWLIB_SCROLLBARS_MOTIF)	\
    && !defined (LWLIB_SCROLLBARS_LUCID) 	\
    && !defined (LWLIB_SCROLLBARS_ATHENA3D)

    fallback = Fcons (Fcons (list1 (Qx), Qt), fallback);
#endif
    set_specifier_fallback (Vscrollbar_on_left_p, fallback);
  }

  set_specifier_caching (Vscrollbar_on_left_p,
			 offsetof (struct window, scrollbar_on_left_p),
			 vertical_scrollbar_changed_in_window,
			 offsetof (struct frame, scrollbar_on_left_p),
			 frame_size_slipped, 0);

  DEFVAR_SPECIFIER ("scrollbar-on-top-p", &Vscrollbar_on_top_p /*
*Whether the horizontal scrollbar is on the top side of window or frame.
This is a specifier; use `set-specifier' to change it.
*/ );
  Vscrollbar_on_top_p = Fmake_specifier (Qboolean);
  set_specifier_fallback (Vscrollbar_on_top_p,
			  list1 (Fcons (Qnil, Qnil)));
  set_specifier_caching (Vscrollbar_on_top_p,
			 offsetof (struct window, scrollbar_on_top_p),
			 some_window_value_changed,
			 offsetof (struct frame, scrollbar_on_top_p),
			 frame_size_slipped, 0);
}

void
complex_vars_of_scrollbar (void)
{
  Vscrollbar_pointer_glyph = Fmake_glyph_internal (Qpointer);

  set_specifier_caching (XGLYPH (Vscrollbar_pointer_glyph)->image,
			 offsetof (struct window, scrollbar_pointer),
			 scrollbar_pointer_changed_in_window,
			 0, 0, 0);
}
