/* Device functions for X windows.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.

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

/* Original authors: Jamie Zawinski and the FSF */
/* Rewritten by Ben Wing and Chuck Thompson. */
/* Gtk flavor written by William Perry */

#include <config.h>
#include "lisp.h"

#include "console-gtk.h"
#include "gccache-gtk.h"
#include "glyphs-gtk.h"
#include "objects-gtk.h"
#include "gtk-xemacs.h"

#include "buffer.h"
#include "events.h"
#include "faces.h"
#include "frame.h"
#include "redisplay.h"
#include "sysdep.h"
#include "window.h"
#include "elhash.h"

#include "sysfile.h"
#include "systime.h"

#ifdef HAVE_GNOME
#include <libgnomeui/libgnomeui.h>
#endif

#ifdef HAVE_BONOBO
#include <bonobo.h>
#endif

Lisp_Object Vdefault_gtk_device;

/* Qdisplay in general.c */
Lisp_Object Qinit_pre_gtk_win, Qinit_post_gtk_win;

/* The application class of Emacs. */
Lisp_Object Vgtk_emacs_application_class;

Lisp_Object Vgtk_initial_argv_list; /* #### ugh! */
Lisp_Object Vgtk_initial_geometry;

static void gtk_device_init_x_specific_cruft (struct device *d);


/************************************************************************/
/*                          helper functions                            */
/************************************************************************/

struct device *
decode_gtk_device (Lisp_Object device)
{
  XSETDEVICE (device, decode_device (device));
  CHECK_GTK_DEVICE (device);
  return XDEVICE (device);
}


/************************************************************************/
/*		      initializing a GTK connection			*/
/************************************************************************/
extern Lisp_Object
xemacs_gtk_convert_color(GdkColor *c, GtkWidget *w);

extern Lisp_Object __get_gtk_font_truename (GdkFont *gdk_font, int expandp);

#define convert_font(f) __get_gtk_font_truename (f, 0)

static void
allocate_gtk_device_struct (struct device *d)
{
  d->device_data = xnew_and_zero (struct gtk_device);
  DEVICE_GTK_DATA (d)->x_keysym_map_hashtable = Qnil;
}

static void
gtk_init_device_class (struct device *d)
{
  if (DEVICE_GTK_DEPTH(d) > 2)
    {
      switch (DEVICE_GTK_VISUAL(d)->type)
	{
	case GDK_VISUAL_STATIC_GRAY:
	case GDK_VISUAL_GRAYSCALE:
	  DEVICE_CLASS (d) = Qgrayscale;
	  break;
	default:
	  DEVICE_CLASS (d) = Qcolor;
	}
    }
  else
    DEVICE_CLASS (d) = Qmono;
}

#ifdef HAVE_GDK_IMLIB_INIT
extern void gdk_imlib_init(void);
#endif

extern void emacs_gtk_selection_handle (GtkWidget *,
					GtkSelectionData *selection_data,
					guint info,
					guint time_stamp,
					gpointer data);
extern void emacs_gtk_selection_clear_event_handle (GtkWidget *widget,
                                                    GdkEventSelection *event,
                                                    gpointer data);
extern void emacs_gtk_selection_received (GtkWidget *widget,
					  GtkSelectionData *selection_data,
					  gpointer user_data);

#ifdef HAVE_BONOBO
static CORBA_ORB orb;
#endif

DEFUN ("gtk-init", Fgtk_init, 1, 1, 0, /*
Initialize the GTK subsystem.
ARGS is a standard list of command-line arguments.

No effect if called more than once.  Called automatically when
creating the first GTK device.  Must be called manually from batch
mode.
*/
       (args))
{
  int argc;
  char **argv;
  static int done;

  if (done)
    {
      return (Qt);
    }

  make_argc_argv (args, &argc, &argv);

  slow_down_interrupts ();
#ifdef HAVE_GNOME
#ifdef INFODOCK
  gnome_init ("InfoDock", EMACS_VERSION, argc, argv);
#else
  gnome_init ("XEmacs", EMACS_VERSION, argc, argv);
#endif /* INFODOCK */
#else
  gtk_init (&argc, &argv);
#endif

#ifdef HAVE_BONOBO
  orb = oaf_init (argc, argv);

  if (bonobo_init (orb, NULL, NULL) == FALSE)
    {
      g_warning ("Could not initialize bonobo...");
    }

  bonobo_activate ();
#endif

  speed_up_interrupts ();

  free_argc_argv (argv);
  return (Qt);
}

static void
gtk_init_device (struct device *d, Lisp_Object props)
{
  Lisp_Object device;
  Lisp_Object display;
  GtkWidget *app_shell = NULL;
  GdkVisual *visual = NULL;
  GdkColormap *cmap = NULL;

  XSETDEVICE (device, d);

  /* gtk_init() and even gtk_check_init() are so brain dead that
     getting an empty argv array causes them to abort. */
  if (NILP (Vgtk_initial_argv_list))
    {
      signal_simple_error ("gtk-initial-argv-list must be set before creating Gtk devices", Vgtk_initial_argv_list);
      return;
    }

  allocate_gtk_device_struct (d);
  display = DEVICE_CONNECTION (d);

  /* Attempt to load a site-specific gtkrc */
  {
    Lisp_Object gtkrc = Fexpand_file_name (build_string ("gtkrc"), Vdata_directory);
    gchar **default_files = gtk_rc_get_default_files ();
    gint num_files;

    if (STRINGP (gtkrc))
      {
	/* Found one, load it up! */
	gchar **new_rc_files = NULL;
	int ctr;

	for (num_files = 0; default_files[num_files]; num_files++);

	new_rc_files = xnew_array_and_zero (gchar *, num_files + 3);

	new_rc_files[0] = XSTRING_DATA (gtkrc);
	for (ctr = 1; default_files[ctr-1]; ctr++)
	  new_rc_files[ctr] = g_strdup (default_files[ctr-1]);

	gtk_rc_set_default_files (new_rc_files);

	for (ctr = 1; new_rc_files[ctr]; ctr++)
	  free(new_rc_files[ctr]);

	xfree (new_rc_files);
      }
  }

  Fgtk_init (Vgtk_initial_argv_list);

#ifdef __FreeBSD__
  gdk_set_use_xshm (FALSE);
#endif

  /* We attempt to load this file so that the user can set
  ** gtk-initial-geometry and not need GNOME & session management to
  ** set their default frame size.  It also avoids the flicker
  ** associated with setting the frame size in your .emacs file.
  */
  call4 (Qload, build_string ("~/.xemacs/gtk-options.el"), Qt, Qt, Qt);

#ifdef HAVE_GDK_IMLIB_INIT
  /* Some themes in Gtk are so lame (most notably the Pixmap theme)
     that they rely on gdk_imlib, but don't call its initialization
     routines.  This makes them USELESS for non-gnome applications.
     So we bend over backwards to try and make them work.  Losers. */
  gdk_imlib_init ();
#endif

  if (NILP (DEVICE_NAME (d)))
    DEVICE_NAME (d) = display;

  /* Always search for the best visual */
  visual = gdk_visual_get_best();
  cmap = gdk_colormap_new (visual, TRUE);

  DEVICE_GTK_VISUAL (d) = visual;
  DEVICE_GTK_COLORMAP (d) = cmap;
  DEVICE_GTK_DEPTH (d) = visual->depth;

  {
    GtkWidget *w = gtk_window_new (GTK_WINDOW_TOPLEVEL);

    app_shell = gtk_xemacs_new (NULL);
    gtk_container_add (GTK_CONTAINER (w), app_shell);

    gtk_widget_realize (w);
  }

  DEVICE_GTK_APP_SHELL (d) = app_shell;

  /* Realize the app_shell so that its window exists for GC creation
     purposes */
  gtk_widget_realize (GTK_WIDGET (app_shell));

  /* Need to set up some selection handlers */
  gtk_selection_add_target (GTK_WIDGET (app_shell), GDK_SELECTION_PRIMARY,
			    GDK_SELECTION_TYPE_STRING, 0);
  gtk_selection_add_target (GTK_WIDGET (app_shell),
                            gdk_atom_intern("CLIPBOARD", FALSE),
			    GDK_SELECTION_TYPE_STRING, 0);
  
  gtk_signal_connect (GTK_OBJECT (app_shell), "selection_get",
		      GTK_SIGNAL_FUNC (emacs_gtk_selection_handle), NULL);
  gtk_signal_connect (GTK_OBJECT (app_shell), "selection_clear_event",
                      GTK_SIGNAL_FUNC (emacs_gtk_selection_clear_event_handle),
                      NULL);
  gtk_signal_connect (GTK_OBJECT (app_shell), "selection_received",
		      GTK_SIGNAL_FUNC (emacs_gtk_selection_received), NULL);

  DEVICE_GTK_WM_COMMAND_FRAME (d) = Qnil;

  gtk_init_modifier_mapping (d);

  gtk_device_init_x_specific_cruft (d);

  init_baud_rate (d);
  init_one_device (d);

  DEVICE_GTK_GC_CACHE (d) = make_gc_cache (GTK_WIDGET (app_shell));
  DEVICE_GTK_GRAY_PIXMAP (d) = NULL;

  gtk_init_device_class (d);

  /* Run the elisp side of the X device initialization. */
  call0 (Qinit_pre_gtk_win);
}

static void
gtk_finish_init_device (struct device *d, Lisp_Object props)
{
  call0 (Qinit_post_gtk_win);
}

static void
gtk_mark_device (struct device *d)
{
  mark_object (DEVICE_GTK_WM_COMMAND_FRAME (d));
  mark_object (DEVICE_GTK_DATA (d)->x_keysym_map_hashtable);
}


/************************************************************************/
/*                       closing an X connection	                */
/************************************************************************/

static void
free_gtk_device_struct (struct device *d)
{
  xfree (d->device_data);
}

static void
gtk_delete_device (struct device *d)
{
  Lisp_Object device;

#ifdef FREE_CHECKING
  extern void (*__free_hook)();
  int checking_free;
#endif

  XSETDEVICE (device, d);
  if (1)
    {
#ifdef FREE_CHECKING
      checking_free = (__free_hook != 0);

      /* Disable strict free checking, to avoid bug in X library */
      if (checking_free)
	disable_strict_free_check ();
#endif

      free_gc_cache (DEVICE_GTK_GC_CACHE (d));

#ifdef FREE_CHECKING
      if (checking_free)
	enable_strict_free_check ();
#endif
    }

  if (EQ (device, Vdefault_gtk_device))
    {
      Lisp_Object devcons, concons;
      /* #### handle deleting last X device */
      Vdefault_gtk_device = Qnil;
      DEVICE_LOOP_NO_BREAK (devcons, concons)
	{
	  if (DEVICE_GTK_P (XDEVICE (XCAR (devcons))) &&
	      !EQ (device, XCAR (devcons)))
	    {
	      Vdefault_gtk_device = XCAR (devcons);
	      goto double_break;
	    }
	}
    }
 double_break:
  free_gtk_device_struct (d);
}


/************************************************************************/
/*				handle X errors				*/
/************************************************************************/

const char *
gtk_event_name (GdkEventType event_type)
{
  GtkEnumValue *vals = gtk_type_enum_get_values (GTK_TYPE_GDK_EVENT_TYPE);

  while (vals && (vals->value != event_type)) vals++;

  if (vals)
    return (vals->value_nick);

  return (NULL);
}


/************************************************************************/
/*                   display information functions                      */
/************************************************************************/

DEFUN ("default-gtk-device", Fdefault_gtk_device, 0, 0, 0, /*
Return the default GTK device for resourcing.
This is the first-created GTK device that still exists.
*/
       ())
{
  return Vdefault_gtk_device;
}

DEFUN ("gtk-display-visual-class", Fgtk_display_visual_class, 0, 1, 0, /*
Return the visual class of the GTK display DEVICE is using.
The returned value will be one of the symbols `static-gray', `gray-scale',
`static-color', `pseudo-color', `true-color', or `direct-color'.
*/
       (device))
{
  GdkVisual *vis = DEVICE_GTK_VISUAL (decode_gtk_device (device));
  switch (vis->type)
    {
    case GDK_VISUAL_STATIC_GRAY:  return intern ("static-gray");
    case GDK_VISUAL_GRAYSCALE:    return intern ("gray-scale");
    case GDK_VISUAL_STATIC_COLOR: return intern ("static-color");
    case GDK_VISUAL_PSEUDO_COLOR: return intern ("pseudo-color");
    case GDK_VISUAL_TRUE_COLOR:   return intern ("true-color");
    case GDK_VISUAL_DIRECT_COLOR: return intern ("direct-color");
    default:
      error ("display has an unknown visual class");
      return Qnil;	/* suppress compiler warning */
    }
}

DEFUN ("gtk-display-visual-depth", Fgtk_display_visual_depth, 0, 1, 0, /*
Return the bitplane depth of the visual the GTK display DEVICE is using.
*/
       (device))
{
   return make_int (DEVICE_GTK_DEPTH (decode_gtk_device (device)));
}

static Lisp_Object
gtk_device_system_metrics (struct device *d,
			   enum device_metrics m)
{
#if 0
  GtkStyle *style = gtk_widget_get_style (GTK_WIDGET (DEVICE_GTK_APP_SHELL (d)));

  style = gtk_style_attach (style, w);
#endif
  
  switch (m)
    {
    case DM_size_device:
      return Fcons (make_int (gdk_screen_width ()),
		    make_int (gdk_screen_height ()));
    case DM_size_device_mm:
      return Fcons (make_int (gdk_screen_width_mm ()),
		    make_int (gdk_screen_height_mm ()));
    case DM_num_color_cells:
      return make_int (gdk_colormap_get_system_size ());
    case DM_num_bit_planes:
      return make_int (DEVICE_GTK_DEPTH (d));

#if 0
    case DM_color_default:
    case DM_color_select:
    case DM_color_balloon:
    case DM_color_3d_face:
    case DM_color_3d_light:
    case DM_color_3d_dark:
    case DM_color_menu:
    case DM_color_menu_highlight:
    case DM_color_menu_button:
    case DM_color_menu_disabled:
    case DM_color_toolbar:
    case DM_color_scrollbar:
    case DM_color_desktop:
    case DM_color_workspace:
    case DM_font_default:
    case DM_font_menubar:
    case DM_font_dialog:
    case DM_size_cursor:
    case DM_size_scrollbar:
    case DM_size_menu:
    case DM_size_toolbar:
    case DM_size_toolbar_button:
    case DM_size_toolbar_border:
    case DM_size_icon:
    case DM_size_icon_small:
    case DM_size_workspace:
    case DM_device_dpi:
    case DM_mouse_buttons:
    case DM_swap_buttons:
    case DM_show_sounds:
    case DM_slow_device:
    case DM_security:
#endif
    default: /* No such device metric property for GTK devices  */
      return Qunbound;
    }
}

DEFUN ("gtk-keysym-on-keyboard-p", Fgtk_keysym_on_keyboard_p, 1, 2, 0, /*
Return true if KEYSYM names a key on the keyboard of DEVICE.
More precisely, return true if some keystroke (possibly including modifiers)
on the keyboard of DEVICE keys generates KEYSYM.
Valid keysyms are listed in the files /usr/include/X11/keysymdef.h and in
/usr/lib/X11/XKeysymDB, or whatever the equivalents are on your system.
The keysym name can be provided in two forms:
- if keysym is a string, it must be the name as known to X windows.
- if keysym is a symbol, it must be the name as known to XEmacs.
The two names differ in capitalization and underscoring.
*/
       (keysym, device))
{
  struct device *d = decode_device (device);

  if (!DEVICE_GTK_P (d))
    signal_simple_error ("Not a GTK device", device);

  return (NILP (Fgethash (keysym, DEVICE_GTK_DATA (d)->x_keysym_map_hashtable, Qnil)) ?
	  Qnil : Qt);
}


/************************************************************************/
/*                          grabs and ungrabs                           */
/************************************************************************/

DEFUN ("gtk-grab-pointer", Fgtk_grab_pointer, 0, 3, 0, /*
Grab the pointer and restrict it to its current window.
If optional DEVICE argument is nil, the default device will be used.
If optional CURSOR argument is non-nil, change the pointer shape to that
 until `gtk-ungrab-pointer' is called (it should be an object returned by the
 `make-cursor-glyph' function).
If the second optional argument IGNORE-KEYBOARD is non-nil, ignore all
  keyboard events during the grab.
Returns t if the grab is successful, nil otherwise.
*/
       (device, cursor, ignore_keyboard))
{
  GdkWindow *w;
  int result;
  struct device *d = decode_gtk_device (device);

  if (!NILP (cursor))
    {
      CHECK_POINTER_GLYPH (cursor);
      cursor = glyph_image_instance (cursor, device, ERROR_ME, 0);
    }

  /* We should call gdk_pointer_grab() and (possibly) gdk_keyboard_grab() here instead */
  w = GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (device_selected_frame (d)));

  result = gdk_pointer_grab (w, FALSE,
			     GDK_POINTER_MOTION_MASK |
			     GDK_POINTER_MOTION_HINT_MASK |
			     GDK_BUTTON1_MOTION_MASK |
			     GDK_BUTTON2_MOTION_MASK |
			     GDK_BUTTON3_MOTION_MASK |
			     GDK_BUTTON_PRESS_MASK |
			     GDK_BUTTON_RELEASE_MASK,
			     w,
			     NULL, /* #### BILL!!! Need to create a GdkCursor * as necessary! */
			     GDK_CURRENT_TIME);

  return (result == 0) ? Qt : Qnil;
}

DEFUN ("gtk-ungrab-pointer", Fgtk_ungrab_pointer, 0, 1, 0, /*
Release a pointer grab made with `gtk-grab-pointer'.
If optional first arg DEVICE is nil the default device is used.
If it is t the pointer will be released on all GTK devices.
*/
       (device))
{
  if (!EQ (device, Qt))
    {
	gdk_pointer_ungrab (GDK_CURRENT_TIME);
    }
  else
    {
      Lisp_Object devcons, concons;

      DEVICE_LOOP_NO_BREAK (devcons, concons)
	{
	  struct device *d = XDEVICE (XCAR (devcons));

	  if (DEVICE_GTK_P (d))
	      gdk_pointer_ungrab (GDK_CURRENT_TIME);
	}
    }
  return Qnil;
}

DEFUN ("gtk-grab-keyboard", Fgtk_grab_keyboard, 0, 1, 0, /*
Grab the keyboard on the given device (defaulting to the selected one).
So long as the keyboard is grabbed, all keyboard events will be delivered
to emacs -- it is not possible for other clients to eavesdrop on them.
Ungrab the keyboard with `gtk-ungrab-keyboard' (use an unwind-protect).
Returns t if the grab is successful, nil otherwise.
*/
       (device))
{
  struct device *d = decode_gtk_device (device);
  GdkWindow *w = GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (device_selected_frame (d)));

  gdk_keyboard_grab (w, FALSE, GDK_CURRENT_TIME );

  return Qt;
}

DEFUN ("gtk-ungrab-keyboard", Fgtk_ungrab_keyboard, 0, 1, 0, /*
Release a keyboard grab made with `gtk-grab-keyboard'.
*/
       (device))
{
  gdk_keyboard_ungrab (GDK_CURRENT_TIME);
  return Qnil;
}


/************************************************************************/
/*                              Style Info                              */
/************************************************************************/
DEFUN ("gtk-style-info", Fgtk_style_info, 0, 1, 0, /*
Get the style information for a Gtk device.
*/
       (device))
{
  struct device *d = decode_device (device);
  GtkStyle *style = NULL;
  Lisp_Object result = Qnil;
  GtkWidget *app_shell = GTK_WIDGET (DEVICE_GTK_APP_SHELL (d));
  GdkWindow *w = GET_GTK_WIDGET_WINDOW (app_shell);

  if (!DEVICE_GTK_P (d))
    return (Qnil);

  style = gtk_widget_get_style (app_shell);
  style = gtk_style_attach (style, w);

  if (!style) return (Qnil);

#define FROB_COLOR(slot, name) \
 result = nconc2 (result, \
		list2 (intern (name), \
		list5 (xemacs_gtk_convert_color (&style->slot[GTK_STATE_NORMAL], app_shell),\
			xemacs_gtk_convert_color (&style->slot[GTK_STATE_ACTIVE], app_shell),\
			xemacs_gtk_convert_color (&style->slot[GTK_STATE_PRELIGHT], app_shell),\
			xemacs_gtk_convert_color (&style->slot[GTK_STATE_SELECTED], app_shell),\
			xemacs_gtk_convert_color (&style->slot[GTK_STATE_INSENSITIVE], app_shell))))

  FROB_COLOR (fg, "foreground");
  FROB_COLOR (bg, "background");
  FROB_COLOR (light, "light");
  FROB_COLOR (dark, "dark");
  FROB_COLOR (mid, "mid");
  FROB_COLOR (text, "text");
  FROB_COLOR (base, "base");
#undef FROB_COLOR

  result = nconc2 (result, list2 (Qfont, convert_font (style->font)));

#define FROB_PIXMAP(state) (style->rc_style->bg_pixmap_name[state] ? build_string (style->rc_style->bg_pixmap_name[state]) : Qnil)

  if (style->rc_style)
    result = nconc2 (result, list2 (Qbackground,
				    list5 ( FROB_PIXMAP (GTK_STATE_NORMAL),
					    FROB_PIXMAP (GTK_STATE_ACTIVE),
					    FROB_PIXMAP (GTK_STATE_PRELIGHT),
					    FROB_PIXMAP (GTK_STATE_SELECTED),
					    FROB_PIXMAP (GTK_STATE_INSENSITIVE))));
#undef FROB_PIXMAP

  return (result);
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_device_gtk (void)
{
  DEFSUBR (Fdefault_gtk_device);
  DEFSUBR (Fgtk_keysym_on_keyboard_p);
  DEFSUBR (Fgtk_display_visual_class);
  DEFSUBR (Fgtk_display_visual_depth);
  DEFSUBR (Fgtk_style_info);
  DEFSUBR (Fgtk_grab_pointer);
  DEFSUBR (Fgtk_ungrab_pointer);
  DEFSUBR (Fgtk_grab_keyboard);
  DEFSUBR (Fgtk_ungrab_keyboard);
  DEFSUBR (Fgtk_init);

  defsymbol (&Qinit_pre_gtk_win, "init-pre-gtk-win");
  defsymbol (&Qinit_post_gtk_win, "init-post-gtk-win");
}

void
console_type_create_device_gtk (void)
{
  CONSOLE_HAS_METHOD (gtk, init_device);
  CONSOLE_HAS_METHOD (gtk, finish_init_device);
  CONSOLE_HAS_METHOD (gtk, mark_device);
  CONSOLE_HAS_METHOD (gtk, delete_device);
  CONSOLE_HAS_METHOD (gtk, device_system_metrics);
  /* CONSOLE_IMPLEMENTATION_FLAGS (gtk, XDEVIMPF_PIXEL_GEOMETRY); */
  /* I inserted the above commented out statement, as the original
     implementation of gtk_device_implementation_flags(), which I
     deleted, contained commented out XDEVIMPF_PIXEL_GEOMETRY - kkm*/
}

void
vars_of_device_gtk (void)
{
  Fprovide (Qgtk);

  staticpro (&Vdefault_gtk_device);

  DEFVAR_LISP ("gtk-initial-argv-list", &Vgtk_initial_argv_list /*
You don't want to know.
This is used during startup to communicate the remaining arguments in
`command-line-args-left' to the C code, which passes the args to
the GTK initialization code, which removes some args, and then the
args are placed back into `gtk-initial-arg-list' and thence into
`command-line-args-left'.  Perhaps `command-line-args-left' should
just reside in C.
*/ );

  DEFVAR_LISP ("gtk-initial-geometry", &Vgtk_initial_geometry /*
You don't want to know.
This is used during startup to communicate the default geometry to GTK.
*/ );

  Vdefault_gtk_device = Qnil;
  Vgtk_initial_geometry = Qnil;
  Vgtk_initial_argv_list = Qnil;
}

#include <gdk/gdkx.h>
static void
gtk_device_init_x_specific_cruft (struct device *d)
{
  DEVICE_INFD (d) = DEVICE_OUTFD (d) = ConnectionNumber (GDK_DISPLAY ());
}
