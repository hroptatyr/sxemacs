/* toolbar implementation -- GTK interface.
   Copyright (C) 2000 Aaron Lehmann

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

#include "faces.h"
#include "frame.h"
#include "toolbar.h"
#include "window.h"

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
gtk_clear_toolbar (struct frame *f, enum toolbar_pos pos);

static void
gtk_toolbar_callback (GtkWidget *w, gpointer user_data)
{
  struct toolbar_button *tb = (struct toolbar_button *) user_data;

  call0 (tb->callback);
}


static void
gtk_output_toolbar (struct frame *f, enum toolbar_pos pos)
{
  GtkWidget *toolbar;
  Lisp_Object button, window, glyph, instance;
  unsigned int checksum = 0;
  struct window *w;
  int x, y, bar_width, bar_height, vert;
  int cur_x, cur_y;

  window = FRAME_LAST_NONMINIBUF_WINDOW (f);
  w = XWINDOW (window);

  get_toolbar_coords (f, pos, &x, &y, &bar_width, &bar_height, &vert, 0);
	
  /* Get the toolbar and delete the old widgets in it */
  button = FRAME_TOOLBAR_BUTTONS (f, pos);
	
  /* First loop over all of the buttons to determine how many there
     are. This loop will also make sure that all instances are
     instantiated so when we actually output them they will come up
     immediately. */
  while (!NILP (button))
    {
      struct toolbar_button *tb = XTOOLBAR_BUTTON (button);
      checksum = HASH4 (checksum, 
			internal_hash (get_toolbar_button_glyph(w, tb), 0),
			internal_hash (tb->callback, 0),
			0 /* width */);
      button = tb->next;
    }

  /* Only do updates if the toolbar has changed, or this is the first
     time we have drawn it in this position
  */
  if (FRAME_GTK_TOOLBAR_WIDGET (f)[pos] &&
      FRAME_GTK_TOOLBAR_CHECKSUM (f, pos) == checksum)
    {
      return;
    }

  /* Loop through buttons and add them to our toolbar.
     This code ignores the button dimensions as we let GTK handle that :)
     Attach the toolbar_button struct to the toolbar button so we know what
     function to use as a callback. */

  {
    gtk_clear_toolbar (f, pos);
    FRAME_GTK_TOOLBAR_WIDGET (f)[pos] = toolbar =
      gtk_toolbar_new (((pos == TOP_TOOLBAR) || (pos == BOTTOM_TOOLBAR)) ?
		       GTK_ORIENTATION_HORIZONTAL : GTK_ORIENTATION_VERTICAL,
		       GTK_TOOLBAR_BOTH);
  }

  if (NILP (w->toolbar_buttons_captioned_p))
    gtk_toolbar_set_style (toolbar, GTK_TOOLBAR_ICONS);
  else
    gtk_toolbar_set_style (toolbar, GTK_TOOLBAR_BOTH);

  FRAME_GTK_TOOLBAR_CHECKSUM(f, pos) = checksum;
  button = FRAME_TOOLBAR_BUTTONS (f, pos);

  cur_x = 0;
  cur_y = 0;

  while (!NILP (button))
    {
      struct toolbar_button *tb = XTOOLBAR_BUTTON (button);

      if (tb->blank)
	{
	  /* It is a blank space... we do not pay attention to the
             size, because the GTK toolbar does not allow us to
             specify different spacings.  *sigh*
	  */
	  gtk_toolbar_append_space (GTK_TOOLBAR (toolbar));
	}
      else
	{
	  /* It actually has a glyph associated with it!  What WILL
             they think of next?
	  */
	  glyph = tb->up_glyph;

	  /* #### It is currently possible for users to trash us by directly
	     changing the toolbar glyphs.  Avoid crashing in that case. */
	  if (GLYPHP (glyph))
	    instance = glyph_image_instance (glyph, window, ERROR_ME_NOT, 1);
	  else
	    instance = Qnil;
	  
	  if (IMAGE_INSTANCEP(instance))
	    {
	      GtkWidget *pixmapwid;
	      GdkPixmap *pixmap;
	      GdkBitmap *mask;
	      char *tooltip = NULL;

	      if (STRINGP (tb->help_string))
		tooltip = XSTRING_DATA (tb->help_string);
	      
	      pixmap = XIMAGE_INSTANCE_GTK_PIXMAP(instance);
	      mask = XIMAGE_INSTANCE_GTK_MASK(instance);
	      pixmapwid = gtk_pixmap_new (pixmap, mask);

	      gtk_widget_set_usize (pixmapwid, tb->width, tb->height);
	      
	      gtk_toolbar_append_item (GTK_TOOLBAR(toolbar), NULL, tooltip, NULL,
				       pixmapwid, gtk_toolbar_callback, (gpointer) tb);
	    }
	}
      cur_x += vert ? 0 : tb->width;
      cur_y += vert ? tb->height : 0;
      /* Who's idea was it to use a linked list for toolbar buttons? */
      button = tb->next;
    }

  SET_TOOLBAR_WAS_VISIBLE_FLAG (f, pos, 1);

  x -= vert ? 3 : 2;
  y -= vert ? 2 : 3;
  
  gtk_fixed_put (GTK_FIXED (FRAME_GTK_TEXT_WIDGET (f)), FRAME_GTK_TOOLBAR_WIDGET (f)[pos],x, y);
  gtk_widget_show_all (FRAME_GTK_TOOLBAR_WIDGET (f)[pos]);
}

static void
gtk_clear_toolbar (struct frame *f, enum toolbar_pos pos)
{
  FRAME_GTK_TOOLBAR_CHECKSUM (f, pos) = 0;
  SET_TOOLBAR_WAS_VISIBLE_FLAG (f, pos, 0);
  if (FRAME_GTK_TOOLBAR_WIDGET(f)[pos])
    gtk_widget_destroy (FRAME_GTK_TOOLBAR_WIDGET(f)[pos]);
}

static void
gtk_output_frame_toolbars (struct frame *f)
{
  if (FRAME_REAL_TOP_TOOLBAR_VISIBLE (f))
    gtk_output_toolbar (f, TOP_TOOLBAR);
  else if (f->top_toolbar_was_visible)
    gtk_clear_toolbar (f, TOP_TOOLBAR);

  if (FRAME_REAL_BOTTOM_TOOLBAR_VISIBLE (f))
    gtk_output_toolbar (f, BOTTOM_TOOLBAR);
  else if (f->bottom_toolbar_was_visible)
    gtk_clear_toolbar (f, LEFT_TOOLBAR);

  if (FRAME_REAL_LEFT_TOOLBAR_VISIBLE (f))
    gtk_output_toolbar (f, LEFT_TOOLBAR);
  else if (f->left_toolbar_was_visible)
    gtk_clear_toolbar (f, LEFT_TOOLBAR);

  if (FRAME_REAL_RIGHT_TOOLBAR_VISIBLE (f))
    gtk_output_toolbar (f, RIGHT_TOOLBAR);
  else if (f->right_toolbar_was_visible)
    gtk_clear_toolbar (f, RIGHT_TOOLBAR);
}

static void
gtk_initialize_frame_toolbars (struct frame *f)
{
  stderr_out ("We should draw toolbars\n");
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
console_type_create_toolbar_gtk (void)
{
  CONSOLE_HAS_METHOD (gtk, output_frame_toolbars);
  CONSOLE_HAS_METHOD (gtk, initialize_frame_toolbars);
}
