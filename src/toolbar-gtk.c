/* toolbar implementation -- X interface.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1996 Chuck Thompson.

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
#include "glyphs-gtk.h"
#include "objects-gtk.h"
#include "gtk-xemacs.h"
#include "gccache-gtk.h"

#include "faces.h"
#include "frame.h"
#include "toolbar.h"
#include "window.h"

extern GdkGC *gtk_get_gc (struct device *d, Lisp_Object font, Lisp_Object fg, Lisp_Object bg,
			  Lisp_Object bg_pmap, Lisp_Object lwidth);

static GdkGC *get_toolbar_gc (struct frame *f)
{
  Lisp_Object fg, bg;
  Lisp_Object frame;

  XSETFRAME (frame, f);

  fg = Fspecifier_instance (Fget (Vtoolbar_face, Qforeground, Qnil), frame, Qnil, Qnil);
  bg = Fspecifier_instance (Fget (Vtoolbar_face, Qbackground, Qnil), frame, Qnil, Qnil);
				   
  /* Need to swap the foreground/background here or most themes look bug ugly */
  return (gtk_get_gc (XDEVICE (FRAME_DEVICE (f)), Qnil, bg, fg, Qnil, Qnil));
}

static void
gtk_draw_blank_toolbar_button (struct frame *f, int x, int y, int width,
			       int height, int threed, int border_width,
			       int vertical)
{
  GtkXEmacs *ef = GTK_XEMACS (FRAME_GTK_TEXT_WIDGET (f));
  int sx = x, sy = y, swidth = width, sheight = height;
  GdkWindow *x_win = GTK_WIDGET (ef)->window;
  GdkGC *background_gc = get_toolbar_gc (f);

  if (vertical)
    {
      sx += border_width;
      swidth -= 2 * border_width;
    }
  else
    {
      sy += border_width;
      sheight -= 2 * border_width;
    }

  /* Blank the entire area. */
  gdk_draw_rectangle (x_win, background_gc, TRUE, sx, sy, swidth, sheight);

  /* Draw the outline. */
  if (threed)
    gtk_output_shadows (f, sx, sy, swidth, sheight, 2);

  /* Do the border */
  gdk_draw_rectangle (x_win, background_gc, TRUE, x, y,
		      (vertical ? border_width : width),
		      (vertical ? height : border_width));
  gdk_draw_rectangle (x_win, background_gc, TRUE,
		      (vertical ? sx + swidth : x),
		      (vertical ? y : sy + sheight),
		      (vertical ? border_width : width),
		      (vertical ? height : border_width));
}

static void
gtk_output_toolbar_button (struct frame *f, Lisp_Object button)
{
  int shadow_thickness = 2;
  int x_adj, y_adj, width_adj, height_adj;
  GdkWindow *x_win = FRAME_GTK_TEXT_WIDGET (f)->window;
  GdkGC *background_gc = get_toolbar_gc (f);
  Lisp_Object instance, frame, window, glyph;
  struct toolbar_button *tb = XTOOLBAR_BUTTON (button);
  struct Lisp_Image_Instance *p;
  struct window *w;
  int vertical = tb->vertical;
  int border_width = tb->border_width;

  if (vertical)
    {
      x_adj = border_width;
      width_adj = - 2 * border_width;
      y_adj = 0;
      height_adj = 0;
    }
  else
    {
      x_adj = 0;
      width_adj = 0;
      y_adj = border_width;
      height_adj = - 2 * border_width;
    }

  XSETFRAME (frame, f);
  window = FRAME_LAST_NONMINIBUF_WINDOW (f);
  w = XWINDOW (window);

  glyph = get_toolbar_button_glyph (w, tb);

  if (tb->enabled)
    {
      if (tb->down)
	{
	  shadow_thickness = -2;
	}
      else
	{
	  shadow_thickness = 2;
	}
    }
  else
    {
      shadow_thickness = 0;
    }

  background_gc = get_toolbar_gc (f);

  /* Clear the entire area. */
  gdk_draw_rectangle (x_win, background_gc, TRUE,
		      tb->x + x_adj,
		      tb->y + y_adj,
		      tb->width + width_adj,
		      tb->height + height_adj);

  /* Draw the outline. */
  if (shadow_thickness)
    gtk_output_shadows (f, tb->x + x_adj, tb->y + y_adj,
			tb->width + width_adj, tb->height + height_adj,
			shadow_thickness);

  /* Do the border. */
  gdk_draw_rectangle (x_win, background_gc, TRUE, tb->x, tb->y,
		      (vertical ? border_width : tb->width),
		      (vertical ? tb->height : border_width));

  gdk_draw_rectangle (x_win, background_gc, TRUE,
		      (vertical ? tb->x + tb->width - border_width : tb->x),
		      (vertical ? tb->y : tb->y + tb->height - border_width),
		      (vertical ? border_width : tb->width),
		      (vertical ? tb->height : border_width));

  background_gc = get_toolbar_gc (f);

  /* #### It is currently possible for users to trash us by directly
     changing the toolbar glyphs.  Avoid crashing in that case. */
  if (GLYPHP (glyph))
    instance = glyph_image_instance (glyph, window, ERROR_ME_NOT, 1);
  else
    instance = Qnil;

  if (IMAGE_INSTANCEP (instance))
    {
      int width = tb->width + width_adj - shadow_thickness * 2;
      int height = tb->height + height_adj - shadow_thickness * 2;
      int x_offset = x_adj + shadow_thickness;
      int y_offset = y_adj + shadow_thickness;

      p = XIMAGE_INSTANCE (instance);

      if (IMAGE_INSTANCE_PIXMAP_TYPE_P (p))
	{
	  if (width > (int) IMAGE_INSTANCE_PIXMAP_WIDTH (p))
	    {
	      x_offset += ((int) (width - IMAGE_INSTANCE_PIXMAP_WIDTH (p))
			   / 2);
	      width = IMAGE_INSTANCE_PIXMAP_WIDTH (p);
	    }
	  if (height > (int) IMAGE_INSTANCE_PIXMAP_HEIGHT (p))
	    {
	      y_offset += ((int) (height - IMAGE_INSTANCE_PIXMAP_HEIGHT (p))
			   / 2);
	      height = IMAGE_INSTANCE_PIXMAP_HEIGHT (p);
	    }

	  gtk_output_gdk_pixmap (f, XIMAGE_INSTANCE (instance), tb->x + x_offset,
				 tb->y + y_offset, 0, 0, 0, 0, width, height,
				 0, 0, 0, background_gc);
	}
      else if (IMAGE_INSTANCE_TYPE (p) == IMAGE_TEXT)
	{
	  /* #### We need to make the face used configurable. */
	  struct face_cachel *cachel =
	    WINDOW_FACE_CACHEL (w, DEFAULT_INDEX);
	  struct display_line dl;
	  Lisp_Object string = IMAGE_INSTANCE_TEXT_STRING (p);
	  unsigned char charsets[NUM_LEADING_BYTES];
	  Emchar_dynarr *buf;
	  struct font_metric_info fm;

	  /* This could be true if we were called via the Expose event
             handler.  Mark the button as dirty and return
             immediately. */
	  if (f->window_face_cache_reset)
	    {
	      tb->dirty = 1;
	      MARK_TOOLBAR_CHANGED;
	      return;
	    }
	  buf = Dynarr_new (Emchar);
	  convert_bufbyte_string_into_emchar_dynarr
	    (XSTRING_DATA (string), XSTRING_LENGTH (string), buf);
	  find_charsets_in_emchar_string (charsets, Dynarr_atp (buf, 0),
					  Dynarr_length (buf));
	  ensure_face_cachel_complete (cachel, window, charsets);
	  face_cachel_charset_font_metric_info (cachel, charsets, &fm);

	  dl.ascent = fm.ascent;
	  dl.descent = fm.descent;
	  dl.ypos = tb->y + y_offset + fm.ascent;

	  if (fm.ascent + fm.descent <= height)
	    {
	      dl.ypos += (height - fm.ascent - fm.descent) / 2;
	      dl.clip = 0;
	    }
	  else
	    {
	      dl.clip = fm.ascent + fm.descent - height;
	    }

	  gtk_output_string (w, &dl, buf, tb->x + x_offset, 0, 0, width,
			     DEFAULT_INDEX, 0, 0, 0, 0);
	  Dynarr_free (buf);
	}

      /* We silently ignore the image if it isn't a pixmap or text. */
    }

  tb->dirty = 0;
}

static int
gtk_get_button_size (struct frame *f, Lisp_Object window,
		     struct toolbar_button *tb, int vert, int pos)
{
  int shadow_thickness = 2;
  int size;

  if (tb->blank)
    {
      if (!NILP (tb->down_glyph))
	size = XINT (tb->down_glyph);
      else
	size = DEFAULT_TOOLBAR_BLANK_SIZE;
    }
  else
    {
      struct window *w = XWINDOW (window);
      Lisp_Object glyph = get_toolbar_button_glyph (w, tb);

      /* Unless, of course, the user has done something stupid like
         change the glyph out from under us.  Use a blank placeholder
         in that case. */
      if (NILP (glyph))
	return XINT (f->toolbar_size[pos]);

      if (vert)
	size = glyph_height (glyph, window);
      else
	size = glyph_width (glyph, window);
    }

  if (!size)
    {
      /* If the glyph doesn't have a size we'll insert a blank
         placeholder instead. */
      return XINT (f->toolbar_size[pos]);
    }

  size += shadow_thickness * 2;

  return (size);
}

#define GTK_OUTPUT_BUTTONS_LOOP(left)					\
  do {									\
    while (!NILP (button))						\
      {									\
	struct toolbar_button *tb = XTOOLBAR_BUTTON (button);		\
	int size, height, width;					\
									\
	if (left && tb->pushright)					\
	  break;							\
									\
        size = gtk_get_button_size (f, window, tb, vert, pos);		\
									\
	if (vert)							\
	  {								\
	    width = bar_width;						\
	    if (y + size > max_pixpos)					\
	      height = max_pixpos - y;					\
	    else							\
	      height = size;						\
	  }								\
	else								\
	  {								\
	    if (x + size > max_pixpos)					\
	      width = max_pixpos - x;					\
	    else							\
	      width = size;						\
	    height = bar_height;					\
	  }								\
									\
	if (tb->x != x							\
	    || tb->y != y						\
	    || tb->width != width					\
	    || tb->height != height					\
	    || tb->dirty						\
	    || f->clear) /* This is clearly necessary. */		\
	  {								\
	    if (width && height)					\
	      {								\
		tb->x = x;						\
		tb->y = y;						\
		tb->width = width;					\
		tb->height = height;					\
	        tb->border_width = border_width;			\
	        tb->vertical = vert;					\
									\
                if (tb->blank || NILP (tb->up_glyph))			\
		  {							\
		    int threed = (EQ (Qt, tb->up_glyph) ? 1 : 0);	\
		    gtk_draw_blank_toolbar_button (f, x, y, width,	\
						 height, threed,	\
						 border_width, vert);	\
		  }							\
	        else							\
		  gtk_output_toolbar_button (f, button);		\
	      }								\
	  }								\
									\
	if (vert)							\
	  y += height;							\
	else								\
	  x += width;							\
									\
	if ((vert && y == max_pixpos) || (!vert && x == max_pixpos))	\
	  button = Qnil;						\
	else								\
	  button = tb->next;						\
      }									\
  } while (0)

#define SET_TOOLBAR_WAS_VISIBLE_FLAG(frame, pos, flag)			\
  do {									\
    switch (pos)							\
      {									\
      case TOP_TOOLBAR:							\
	(frame)->top_toolbar_was_visible = flag;			\
	break;								\
      case BOTTOM_TOOLBAR:						\
	(frame)->bottom_toolbar_was_visible = flag;			\
	break;								\
      case LEFT_TOOLBAR:						\
	(frame)->left_toolbar_was_visible = flag;			\
	break;								\
      case RIGHT_TOOLBAR:						\
	(frame)->right_toolbar_was_visible = flag;			\
	break;								\
      default:								\
	abort ();							\
      }									\
  } while (0)

static void
gtk_output_toolbar (struct frame *f, enum toolbar_pos pos)
{
  int x, y, bar_width, bar_height, vert;
  int max_pixpos, right_size, right_start, blank_size;
  int border_width = FRAME_REAL_TOOLBAR_BORDER_WIDTH (f, pos);
  Lisp_Object button, window;
  GdkWindow *x_win = FRAME_GTK_TEXT_WIDGET (f)->window;
  GdkGC *background_gc = get_toolbar_gc (f);

  get_toolbar_coords (f, pos, &x, &y, &bar_width, &bar_height, &vert, 1);
  window = FRAME_LAST_NONMINIBUF_WINDOW (f);

  /* Do the border */
  gdk_draw_rectangle (x_win, background_gc, TRUE, x, y,
		      (vert ? bar_width : border_width),
		      (vert ? border_width : bar_height));
  gdk_draw_rectangle (x_win, background_gc, TRUE,
		      (vert ? x : x + bar_width - border_width),
		      (vert ? y + bar_height - border_width : y),
		      (vert ? bar_width : border_width),
		      (vert ? border_width : bar_height));

  if (vert)
    {
      max_pixpos = y + bar_height - border_width;
      y += border_width;
    }
  else
    {
      max_pixpos = x + bar_width - border_width;
      x += border_width;
    }

  button = FRAME_TOOLBAR_BUTTONS (f, pos);
  right_size = 0;

  /* First loop over all of the buttons to determine how much room we
     need for left hand and right hand buttons.  This loop will also
     make sure that all instances are instantiated so when we actually
     output them they will come up immediately. */
  while (!NILP (button))
    {
      struct toolbar_button *tb = XTOOLBAR_BUTTON (button);
      int size = gtk_get_button_size (f, window, tb, vert, pos);

      if (tb->pushright)
	right_size += size;

      button = tb->next;
    }

  button = FRAME_TOOLBAR_BUTTONS (f, pos);

  /* Loop over the left buttons, updating and outputting them. */
  GTK_OUTPUT_BUTTONS_LOOP (1);

  /* Now determine where the right buttons start. */
  right_start = max_pixpos - right_size;
  if (right_start < (vert ? y : x))
    right_start = (vert ? y : x);

  /* Output the blank which goes from the end of the left buttons to
     the start of the right. */
  blank_size = right_start - (vert ? y : x);
  if (blank_size)
    {
      int height, width;

      if (vert)
	{
	  width = bar_width;
	  height = blank_size;
	}
      else
	{
	  width = blank_size;
	  height = bar_height;
	}

      /*
       * Use a 3D pushright separator only if there isn't a toolbar
       * border.  A flat separator meshes with the border and looks
       * better.
       */
      gtk_draw_blank_toolbar_button (f, x, y, width, height, !border_width,
				     border_width, vert);

      if (vert)
	y += height;
      else
	x += width;
    }

  /* Loop over the right buttons, updating and outputting them. */
  GTK_OUTPUT_BUTTONS_LOOP (0);

  if (!vert)
    {
      Lisp_Object frame;

      XSETFRAME (frame, f);
      redisplay_clear_region (frame,
			      DEFAULT_INDEX, FRAME_PIXWIDTH (f) - 1, y, 1,
			      bar_height);
    }

  SET_TOOLBAR_WAS_VISIBLE_FLAG (f, pos, 1);

  gdk_flush ();
}

static void
gtk_clear_toolbar (struct frame *f, enum toolbar_pos pos, int thickness_change)
{
  Lisp_Object frame;
  int x, y, width, height, vert;

  get_toolbar_coords (f, pos, &x, &y, &width, &height, &vert, 1);
  XSETFRAME (frame, f);

  /* The thickness_change parameter is used by the toolbar resize routines
     to clear any excess toolbar if the size shrinks. */
  if (thickness_change < 0)
    {
      if (pos == LEFT_TOOLBAR || pos == RIGHT_TOOLBAR)
	{
	  x = x + width + thickness_change;
	  width = -thickness_change;
	}
      else
	{
	  y = y + height + thickness_change;
	  height = -thickness_change;
	}
    }

  SET_TOOLBAR_WAS_VISIBLE_FLAG (f, pos, 0);

  redisplay_clear_region (frame, DEFAULT_INDEX, x, y, width, height);
  gdk_flush ();
}

static void
gtk_output_frame_toolbars (struct frame *f)
{
  assert (FRAME_GTK_P (f));

  if (FRAME_REAL_TOP_TOOLBAR_VISIBLE (f))
    gtk_output_toolbar (f, TOP_TOOLBAR);
  if (FRAME_REAL_BOTTOM_TOOLBAR_VISIBLE (f))
    gtk_output_toolbar (f, BOTTOM_TOOLBAR);
  if (FRAME_REAL_LEFT_TOOLBAR_VISIBLE (f))
    gtk_output_toolbar (f, LEFT_TOOLBAR);
  if (FRAME_REAL_RIGHT_TOOLBAR_VISIBLE (f))
    gtk_output_toolbar (f, RIGHT_TOOLBAR);
}

static void
gtk_clear_frame_toolbars (struct frame *f)
{
  assert (FRAME_GTK_P (f));

  if (f->top_toolbar_was_visible
      && !FRAME_REAL_TOP_TOOLBAR_VISIBLE (f))
    gtk_clear_toolbar (f, TOP_TOOLBAR, 0);
  if (f->bottom_toolbar_was_visible
      && !FRAME_REAL_BOTTOM_TOOLBAR_VISIBLE (f))
    gtk_clear_toolbar (f, BOTTOM_TOOLBAR, 0);
  if (f->left_toolbar_was_visible 
      && !FRAME_REAL_LEFT_TOOLBAR_VISIBLE (f))
    gtk_clear_toolbar (f, LEFT_TOOLBAR, 0);
  if (f->right_toolbar_was_visible 
      && !FRAME_REAL_RIGHT_TOOLBAR_VISIBLE (f))
    gtk_clear_toolbar (f, RIGHT_TOOLBAR, 0);
}

static void
gtk_redraw_exposed_toolbar (struct frame *f, enum toolbar_pos pos, int x, int y,
			    int width, int height)
{
  int bar_x, bar_y, bar_width, bar_height, vert;
  Lisp_Object button = FRAME_TOOLBAR_BUTTONS (f, pos);

  get_toolbar_coords (f, pos, &bar_x, &bar_y, &bar_width, &bar_height,
		      &vert, 1);

  if (((y + height) < bar_y) || (y > (bar_y + bar_height)))
    return;
  if (((x + width) < bar_x) || (x > (bar_x + bar_width)))
    return;

  while (!NILP (button))
    {
      struct toolbar_button *tb = XTOOLBAR_BUTTON (button);

      if (vert)
	{
	  if (((tb->y + tb->height) > y) && (tb->y < (y + height)))
	    tb->dirty = 1;

	  /* If this is true we have gone past the exposed region. */
	  if (tb->y > (y + height))
	    break;
	}
      else
	{
	  if (((tb->x + tb->width) > x) && (tb->x < (x + width)))
	    tb->dirty = 1;

	  /* If this is true we have gone past the exposed region. */
	  if (tb->x > (x + width))
	    break;
	}

      button = tb->next;
    }

  /* Even if none of the buttons is in the area, the blank region at
     the very least must be because the first thing we did is verify
     that some portion of the toolbar is in the exposed region. */
  gtk_output_toolbar (f, pos);
}

static void
gtk_redraw_exposed_toolbars (struct frame *f, int x, int y, int width,
			     int height)
{
  assert (FRAME_GTK_P (f));

  if (FRAME_REAL_TOP_TOOLBAR_VISIBLE (f))
    gtk_redraw_exposed_toolbar (f, TOP_TOOLBAR, x, y, width, height);

  if (FRAME_REAL_BOTTOM_TOOLBAR_VISIBLE (f))
    gtk_redraw_exposed_toolbar (f, BOTTOM_TOOLBAR, x, y, width, height);

  if (FRAME_REAL_LEFT_TOOLBAR_VISIBLE (f))
    gtk_redraw_exposed_toolbar (f, LEFT_TOOLBAR, x, y, width, height);

  if (FRAME_REAL_RIGHT_TOOLBAR_VISIBLE (f))
    gtk_redraw_exposed_toolbar (f, RIGHT_TOOLBAR, x, y, width, height);
}

static void
gtk_redraw_frame_toolbars (struct frame *f)
{
  /* There are certain startup paths that lead to update_EmacsFrame in
     faces.c being called before a new frame is fully initialized.  In
     particular before we have actually mapped it.  That routine can
     call this one.  So, we need to make sure that the frame is
     actually ready before we try and draw all over it. */

  if (GTK_WIDGET_REALIZED (FRAME_GTK_TEXT_WIDGET (f)))
    gtk_redraw_exposed_toolbars (f, 0, 0, FRAME_PIXWIDTH (f),
				 FRAME_PIXHEIGHT (f));
}


static void
gtk_initialize_frame_toolbars (struct frame *f)
{
}

/* This only calls one function but we go ahead and create this in
   case we ever do decide that we need to do more work. */
static void
gtk_free_frame_toolbars (struct frame *f)
{
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
console_type_create_toolbar_gtk (void)
{
  CONSOLE_HAS_METHOD (gtk, output_frame_toolbars);
  CONSOLE_HAS_METHOD (gtk, clear_frame_toolbars);
  CONSOLE_HAS_METHOD (gtk, initialize_frame_toolbars);
  CONSOLE_HAS_METHOD (gtk, free_frame_toolbars);
  CONSOLE_HAS_METHOD (gtk, output_toolbar_button);
  CONSOLE_HAS_METHOD (gtk, redraw_exposed_toolbars);
  CONSOLE_HAS_METHOD (gtk, redraw_frame_toolbars);
}
