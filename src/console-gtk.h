/* Define X specific console, device, and frame object for XEmacs.
   Copyright (C) 1989, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.

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


/* Authorship:

   Ultimately based on FSF, then later on JWZ work for Lemacs.
   Rewritten over time by Ben Wing and Chuck Thompson (original
      multi-device work by Chuck Thompson).
 */

#ifndef _XEMACS_CONSOLE_GTK_H_
#define _XEMACS_CONSOLE_GTK_H_

#ifdef HAVE_GTK

#include "console.h"
#include <gtk/gtk.h>

#define GDK_DRAWABLE(x) (GdkDrawable *) (x)
#define GET_GTK_WIDGET_WINDOW(x) (GTK_WIDGET (x)->window)
#define GET_GTK_WIDGET_PARENT(x) (GTK_WIDGET (x)->parent)

DECLARE_CONSOLE_TYPE (gtk);

struct gtk_device
{
  /* Gtk application info. */
  GtkWidget *gtk_app_shell;

  /* Cache of GC's for frame's on this device. */
  struct gc_cache *gc_cache;

  /* Selected visual, depth and colormap for this device */
  GdkVisual *visual;
  int depth;
  GdkColormap *device_cmap;

  /* Used by x_bevel_modeline in redisplay-x.c */
  GdkBitmap *gray_pixmap;

  /* frame that holds the WM_COMMAND property; there should be exactly
     one of these per device. */
  Lisp_Object WM_COMMAND_frame;

  /* The following items are all used exclusively in event-gtk.c. */
  int MetaMask, HyperMask, SuperMask, AltMask, ModeMask;
  guint lock_interpretation;

  void *x_modifier_keymap; /* Really an (XModifierKeymap *)*/

  guint *x_keysym_map;
  int x_keysym_map_min_code;
  int x_keysym_map_max_code;
  int x_keysym_map_keysyms_per_code;
  Lisp_Object x_keysym_map_hashtable;

  /* #### It's not clear that there is much distinction anymore
     between mouse_timestamp and global_mouse_timestamp, now that
     Emacs doesn't see most (all?) events not destined for it. */

  /* The timestamp of the last button or key event used by emacs itself.
     This is used for asserting selections and input focus. */
  guint32 mouse_timestamp;

  /* This is the timestamp the last button or key event whether it was
     dispatched to emacs or widgets. */
  guint32 global_mouse_timestamp;

  /* This is the last known timestamp received from the server.  It is
     maintained by x_event_to_emacs_event and used to patch bogus
     WM_TAKE_FOCUS messages sent by Mwm. */
  guint32 last_server_timestamp;

  GdkAtom atom_WM_PROTOCOLS;
  GdkAtom atom_WM_TAKE_FOCUS;
  GdkAtom atom_WM_STATE;

#if 0
	/* #### BILL!!! */
  /* stuff for sticky modifiers: */
  unsigned int need_to_add_mask, down_mask;
  KeyCode last_downkey;
  guint32 release_time;
#endif
};

#define DEVICE_GTK_DATA(d) DEVICE_TYPE_DATA (d, gtk)

#define DEVICE_GTK_VISUAL(d)	(DEVICE_GTK_DATA (d)->visual)
#define DEVICE_GTK_DEPTH(d)	(DEVICE_GTK_DATA (d)->depth)
#define DEVICE_GTK_COLORMAP(d) 	(DEVICE_GTK_DATA (d)->device_cmap)
#define DEVICE_GTK_APP_SHELL(d) 	(DEVICE_GTK_DATA (d)->gtk_app_shell)
#define DEVICE_GTK_GC_CACHE(d) 	(DEVICE_GTK_DATA (d)->gc_cache)
#define DEVICE_GTK_GRAY_PIXMAP(d) (DEVICE_GTK_DATA (d)->gray_pixmap)
#define DEVICE_GTK_WM_COMMAND_FRAME(d) (DEVICE_GTK_DATA (d)->WM_COMMAND_frame)
#define DEVICE_GTK_MOUSE_TIMESTAMP(d)  (DEVICE_GTK_DATA (d)->mouse_timestamp)
#define DEVICE_GTK_GLOBAL_MOUSE_TIMESTAMP(d) (DEVICE_GTK_DATA (d)->global_mouse_timestamp)
#define DEVICE_GTK_LAST_SERVER_TIMESTAMP(d)  (DEVICE_GTK_DATA (d)->last_server_timestamp)

/* The maximum number of widgets that can be displayed above the text
   area at one time.  Currently no more than 3 will ever actually be
   displayed (menubar, psheet, debugger panel). */
#define MAX_CONCURRENT_TOP_WIDGETS 8

struct gtk_frame
{
  /* The widget of this frame. */
  GtkWidget *widget;		/* This is really a GtkWindow */

  /* The layout manager */
  GtkWidget *container;		/* actually a GtkVBox. */

  /* The widget of the menubar */
  GtkWidget *menubar_widget;

  /* The widget of the edit portion of this frame; this is a GtkDrawingArea,
     and the window of this widget is what the redisplay code draws on. */
  GtkWidget *edit_widget;

  /* Lists the widgets above the text area, in the proper order. */
  GtkWidget *top_widgets[MAX_CONCURRENT_TOP_WIDGETS];
  int num_top_widgets;

  /* Our container widget as a Lisp_Object */
  Lisp_Object lisp_visible_widgets[10];

  /*************************** Miscellaneous **************************/

  /* The icon pixmaps; these are Lisp_Image_Instance objects, or Qnil. */
  Lisp_Object icon_pixmap;
  Lisp_Object icon_pixmap_mask;

  /* geometry string that ought to be freed. */
  char *geom_free_me_please;

  /* 1 if the frame is completely visible on the display, 0 otherwise.
     if 0 the frame may have been iconified or may be totally
     or partially hidden by another X window */
  unsigned int totally_visible_p :1;

    /* Is it visible at all? */
  unsigned int visible_p :1;

  /* Are we a top-level frame?  This means that our shell is a
     TopLevelShell, and we should do certain things to interact with
     the window manager. */
  unsigned int top_level_frame_p :1;

  /* Are we iconfied right now? */
  unsigned int iconified_p :1;

  /* Data for widget callbacks.  It is impossible to pass all the necessary
     data through the GTK signal API so instead it is registered here and the
     hash key is passed instead. */
  Lisp_Object widget_instance_hash_table;
  Lisp_Object widget_callback_hash_table;
  Lisp_Object widget_callback_ex_hash_table;
};

#define FRAME_GTK_DATA(f) FRAME_TYPE_DATA (f, gtk)

#define FRAME_GTK_SHELL_WIDGET(f)	    (FRAME_GTK_DATA (f)->widget)
#define FRAME_GTK_CONTAINER_WIDGET(f) (FRAME_GTK_DATA (f)->container)
#define FRAME_GTK_MENUBAR_WIDGET(f)   (FRAME_GTK_DATA (f)->menubar_widget)
#define FRAME_GTK_TEXT_WIDGET(f)	    (FRAME_GTK_DATA (f)->edit_widget)
#define FRAME_GTK_TOP_WIDGETS(f)	    (FRAME_GTK_DATA (f)->top_widgets)
#define FRAME_GTK_NUM_TOP_WIDGETS(f)	  (FRAME_GTK_DATA (f)->num_top_widgets)
#define FRAME_GTK_ICONIFIED_P(f)	  (FRAME_GTK_DATA (f)->iconfigied_p)

#define FRAME_GTK_LISP_WIDGETS(f)      (FRAME_GTK_DATA (f)->lisp_visible_widgets)
#define FRAME_GTK_ICON_PIXMAP(f)	    (FRAME_GTK_DATA (f)->icon_pixmap)
#define FRAME_GTK_ICON_PIXMAP_MASK(f) (FRAME_GTK_DATA (f)->icon_pixmap_mask)

#define FRAME_GTK_GEOM_FREE_ME_PLEASE(f) (FRAME_GTK_DATA (f)->geom_free_me_please)

#define FRAME_GTK_TOTALLY_VISIBLE_P(f) (FRAME_GTK_DATA (f)->totally_visible_p)
#define FRAME_GTK_VISIBLE_P(f) (FRAME_GTK_DATA (f)->visible_p)
#define FRAME_GTK_TOP_LEVEL_FRAME_P(f) (FRAME_GTK_DATA (f)->top_level_frame_p)
#define FRAME_GTK_WIDGET_INSTANCE_HASH_TABLE(f) (FRAME_GTK_DATA (f)->widget_instance_hash_table)
#define FRAME_GTK_WIDGET_CALLBACK_HASH_TABLE(f) (FRAME_GTK_DATA (f)->widget_callback_hash_table)
#define FRAME_GTK_WIDGET_CALLBACK_EX_HASH_TABLE(f) (FRAME_GTK_DATA (f)->widget_callback_ex_hash_table)

/* Special data used to quickly identify the frame that contains a widget. */
#define GTK_DATA_FRAME_IDENTIFIER "xemacs::frame"

/* The hashcode in the frame hash table of a tab_control tab's callback data. */
#define GTK_DATA_TAB_HASHCODE_IDENTIFIER "xemacs::tab_hashcode"

#define GTK_DATA_GUI_IDENTIFIER "xemacs::gui_id"

/* Variables associated with the X display frame this emacs is using. */

extern Lisp_Object Vx_gc_pointer_shape;
extern Lisp_Object Vx_scrollbar_pointer_shape;

extern struct console_type *gtk_console_type;
extern Lisp_Object Vdefault_gtk_device;

/* Number of pixels below each line. */
extern int gtk_interline_space;

extern int gtk_selection_timeout;

struct frame *gtk_widget_to_frame (GtkWidget *);
struct frame *gtk_any_window_to_frame (struct device *d, GdkWindow *);
struct frame *gtk_window_to_frame (struct device *d, GdkWindow *);
struct frame *gtk_any_widget_or_parent_to_frame (struct device *d, GtkWidget *widget);
struct frame *decode_gtk_frame (Lisp_Object);
struct device *gtk_any_window_to_device (GdkWindow *);
struct device *decode_gtk_device (Lisp_Object);
void gtk_handle_property_notify (GdkEventProperty *event);

void signal_special_gtk_user_event (Lisp_Object channel, Lisp_Object function,
				    Lisp_Object object);
void gtk_redraw_exposed_area (struct frame *f, int x, int y,
			    int width, int height);
void gtk_output_string (struct window *w, struct display_line *dl,
		      Emchar_dynarr *buf, int xpos, int xoffset,
		      int start_pixpos, int width, face_index findex,
		      int cursor, int cursor_start, int cursor_width,
		      int cursor_height);
void gtk_output_gdk_pixmap (struct frame *f, struct Lisp_Image_Instance *p,
			    int x, int y, int clip_x, int clip_y,
			    int clip_width, int clip_height, int width,
			    int height, int pixmap_offset,
			    GdkColor *fg, GdkColor *bg,
			    GdkGC *override_gc);
void gtk_output_shadows (struct frame *f, int x, int y, int width,
		       int height, int shadow_thickness);

int gtk_initialize_frame_menubar (struct frame *f);
void gtk_init_modifier_mapping (struct device *d);

void Initialize_Locale (void);

extern Lisp_Object Vgtk_initial_argv_list; /* #### ugh! */

const char *gtk_event_name (GdkEventType event_type);
#endif /* HAVE_GTK */
#endif /* _XEMACS_DEVICE_X_H_ */
