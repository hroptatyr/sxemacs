/* Define X specific console, device, and frame object for SXEmacs.
   Copyright (C) 1989, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.

This file is part of SXEmacs

SXEmacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SXEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */


/* Synched up with: Not in FSF. */

/* Authorship:

   Ultimately based on FSF, then later on JWZ work for Lemacs.
   Rewritten over time by Ben Wing and Chuck Thompson (original
      multi-device work by Chuck Thompson).
 */

#ifndef INCLUDED_console_x_h_
#define INCLUDED_console_x_h_

#ifdef HAVE_X_WINDOWS

#include "ui/console.h"
/* must come before Xutil.h, which is included indirectly from xintrinsic.h,
   to avoid breakage in keysym-parsing macros from X11R6, at least X.org as
   of 2005-01-17, and probably XFree86 too from reports on c.e.x */
#include <X11/keysym.h>
#include "xintrinsic.h"

#include <X11/cursorfont.h>
#include <X11/Xatom.h>
#include <X11/StringDefs.h>

#ifdef HAVE_XPM
#include <X11/xpm.h>
#endif

/* R5 defines the XPointer type, but R4 doesn't.
   R4 also doesn't define a version number, but R5 does. */
#if (XlibSpecificationRelease < 5)
# define XPointer char *
#endif

DECLARE_CONSOLE_TYPE(x);

struct x_device {
	/* The X connection of this device. */
	Display *display;

	/* Set by x_IO_error_handler(). */
	int being_deleted;

	/* Xt application info. */
	Widget Xt_app_shell;

	/* Cache of GC's for frames on this device. */
	struct gc_cache *gc_cache;

	/* Selected visual, depth and colormap for this device */
	Visual *visual;
	int depth;
	Colormap device_cmap;

	/* Used by x_bevel_modeline in redisplay-x.c */
	Pixmap gray_pixmap;

	/* Atoms associated with this device. */
	/* allocated in Xatoms_of_device_x */
	Atom Xatom_WM_PROTOCOLS;
	Atom Xatom_WM_DELETE_WINDOW;
	Atom Xatom_WM_SAVE_YOURSELF;
	Atom Xatom_WM_TAKE_FOCUS;
	Atom Xatom_WM_STATE;

	/* allocated in Xatoms_of_select_x */
	Atom Xatom_CLIPBOARD;
	Atom Xatom_TIMESTAMP;
	Atom Xatom_TEXT;
	Atom Xatom_DELETE;
	Atom Xatom_MULTIPLE;
	Atom Xatom_INCR;
	Atom Xatom_EMACS_TMP;
	Atom Xatom_TARGETS;
	Atom Xatom_NULL;
	Atom Xatom_ATOM_PAIR;
	Atom Xatom_COMPOUND_TEXT;

	/* allocated in Xatoms_of_objects_x */
	Atom Xatom_FOUNDRY;
	Atom Xatom_FAMILY_NAME;
	Atom Xatom_WEIGHT_NAME;
	Atom Xatom_SLANT;
	Atom Xatom_SETWIDTH_NAME;
	Atom Xatom_ADD_STYLE_NAME;
	Atom Xatom_PIXEL_SIZE;
	Atom Xatom_POINT_SIZE;
	Atom Xatom_RESOLUTION_X;
	Atom Xatom_RESOLUTION_Y;
	Atom Xatom_SPACING;
	Atom Xatom_AVERAGE_WIDTH;
	Atom Xatom_CHARSET_REGISTRY;
	Atom Xatom_CHARSET_ENCODING;

	/* The following items are all used exclusively in event-Xt.c. */
	int MetaMask, HyperMask, SuperMask, AltMask, ModeMask;
	KeySym lock_interpretation;

	XModifierKeymap *x_modifier_keymap;

	KeySym *x_keysym_map;
	int x_keysym_map_min_code;
	int x_keysym_map_max_code;
	int x_keysym_map_keysyms_per_code;
	Lisp_Object x_keysym_map_hash_table;

	/* frame that holds the WM_COMMAND property; there should be exactly
	   one of these per device. */
	Lisp_Object WM_COMMAND_frame;

	/* #### It's not clear that there is much distinction anymore
	   between mouse_timestamp and global_mouse_timestamp, now that
	   Emacs doesn't see most (all?) events not destined for it. */

	/* The timestamp of the last button or key event used by emacs itself.
	   This is used for asserting selections and input focus. */
	Time mouse_timestamp;

	/* This is the timestamp the last button or key event whether it was
	   dispatched to emacs or widgets. */
	Time global_mouse_timestamp;

	/* This is the last known timestamp received from the server.  It is
	   maintained by x_event_to_emacs_event and used to patch bogus
	   WM_TAKE_FOCUS messages sent by Mwm. */
	Time last_server_timestamp;

	/* Used by Xlib to preserve information across calls to
	   XLookupString(), to implement compose processing.

	   According to The X Window System, p. 467, "The creation of
	   XComposeStatus structures is implementation dependent;
	   a portable program must pass NULL for this argument."
	   But this means that a portable program cannot implement
	   compose processing! WTF?

	   So we just set it to all zeros. */

	/* No X Server ever used this, AFAIK -- mrb */
	/* XComposeStatus x_compose_status; */

#ifdef HAVE_XIM
	XIM xim;
	XIMStyles *xim_styles;
#endif				/* HAVE_XIM */

	/* stuff for sticky modifiers: */

	unsigned int need_to_add_mask, down_mask;
	KeyCode last_downkey;
	Time release_time;
	Time modifier_release_time;
};

#define DEVICE_X_DATA(d) DEVICE_TYPE_DATA (d, x)

#define FRAME_X_DISPLAY(f) (DEVICE_X_DISPLAY (XDEVICE (f->device)))
#define DEVICE_X_DISPLAY(d)	(DEVICE_X_DATA (d)->display)
#define DEVICE_X_BEING_DELETED(d) (DEVICE_X_DATA (d)->being_deleted)
#define DEVICE_X_VISUAL(d)	(DEVICE_X_DATA (d)->visual)
#define DEVICE_X_DEPTH(d)	(DEVICE_X_DATA (d)->depth)
#define DEVICE_X_COLORMAP(d)	(DEVICE_X_DATA (d)->device_cmap)
#define DEVICE_XT_APP_SHELL(d)	(DEVICE_X_DATA (d)->Xt_app_shell)
#define DEVICE_X_GC_CACHE(d)	(DEVICE_X_DATA (d)->gc_cache)
#define DEVICE_X_GRAY_PIXMAP(d) (DEVICE_X_DATA (d)->gray_pixmap)
#define DEVICE_X_WM_COMMAND_FRAME(d) (DEVICE_X_DATA (d)->WM_COMMAND_frame)
#define DEVICE_X_MOUSE_TIMESTAMP(d)  (DEVICE_X_DATA (d)->mouse_timestamp)
#define DEVICE_X_GLOBAL_MOUSE_TIMESTAMP(d) (DEVICE_X_DATA (d)->global_mouse_timestamp)
#define DEVICE_X_LAST_SERVER_TIMESTAMP(d)  (DEVICE_X_DATA (d)->last_server_timestamp)
#define DEVICE_X_KEYSYM_MAP_HASH_TABLE(d)  (DEVICE_X_DATA (d)->x_keysym_map_hash_table)
/* #define DEVICE_X_X_COMPOSE_STATUS(d) (DEVICE_X_DATA (d)->x_compose_status) */
#ifdef HAVE_XIM
#define DEVICE_X_XIM(d)        (DEVICE_X_DATA (d)->xim)
#define DEVICE_X_XIM_STYLES(d) (DEVICE_X_DATA (d)->xim_styles)
#define DEVICE_X_FONTSET(d)    (DEVICE_X_DATA (d)->fontset)
#endif				/* HAVE_XIM */

/* allocated in Xatoms_of_device_x */
#define DEVICE_XATOM_WM_PROTOCOLS(d)	 (DEVICE_X_DATA (d)->Xatom_WM_PROTOCOLS)
#define DEVICE_XATOM_WM_DELETE_WINDOW(d) (DEVICE_X_DATA (d)->Xatom_WM_DELETE_WINDOW)
#define DEVICE_XATOM_WM_SAVE_YOURSELF(d) (DEVICE_X_DATA (d)->Xatom_WM_SAVE_YOURSELF)
#define DEVICE_XATOM_WM_TAKE_FOCUS(d)	 (DEVICE_X_DATA (d)->Xatom_WM_TAKE_FOCUS)
#define DEVICE_XATOM_WM_STATE(d)	 (DEVICE_X_DATA (d)->Xatom_WM_STATE)

/* allocated in Xatoms_of_select_x */
#define DEVICE_XATOM_CLIPBOARD(d)	(DEVICE_X_DATA (d)->Xatom_CLIPBOARD)
#define DEVICE_XATOM_TIMESTAMP(d)	(DEVICE_X_DATA (d)->Xatom_TIMESTAMP)
#define DEVICE_XATOM_TEXT(d)		(DEVICE_X_DATA (d)->Xatom_TEXT)
#define DEVICE_XATOM_DELETE(d)		(DEVICE_X_DATA (d)->Xatom_DELETE)
#define DEVICE_XATOM_MULTIPLE(d)	(DEVICE_X_DATA (d)->Xatom_MULTIPLE)
#define DEVICE_XATOM_INCR(d)		(DEVICE_X_DATA (d)->Xatom_INCR)
#define DEVICE_XATOM_EMACS_TMP(d)	(DEVICE_X_DATA (d)->Xatom_EMACS_TMP)
#define DEVICE_XATOM_TARGETS(d)	(DEVICE_X_DATA (d)->Xatom_TARGETS)
#define DEVICE_XATOM_NULL(d)		(DEVICE_X_DATA (d)->Xatom_NULL)
#define DEVICE_XATOM_ATOM_PAIR(d)	(DEVICE_X_DATA (d)->Xatom_ATOM_PAIR)
#define DEVICE_XATOM_COMPOUND_TEXT(d)	(DEVICE_X_DATA (d)->Xatom_COMPOUND_TEXT)

/* allocated in Xatoms_of_objects_x */
#define DEVICE_XATOM_FOUNDRY(d)		(DEVICE_X_DATA (d)->Xatom_FOUNDRY)
#define DEVICE_XATOM_FAMILY_NAME(d)	(DEVICE_X_DATA (d)->Xatom_FAMILY_NAME)
#define DEVICE_XATOM_WEIGHT_NAME(d)	(DEVICE_X_DATA (d)->Xatom_WEIGHT_NAME)
#define DEVICE_XATOM_SLANT(d)		(DEVICE_X_DATA (d)->Xatom_SLANT)
#define DEVICE_XATOM_SETWIDTH_NAME(d)	(DEVICE_X_DATA (d)->Xatom_SETWIDTH_NAME)
#define DEVICE_XATOM_ADD_STYLE_NAME(d)	(DEVICE_X_DATA (d)->Xatom_ADD_STYLE_NAME)
#define DEVICE_XATOM_PIXEL_SIZE(d)	(DEVICE_X_DATA (d)->Xatom_PIXEL_SIZE)
#define DEVICE_XATOM_POINT_SIZE(d)	(DEVICE_X_DATA (d)->Xatom_POINT_SIZE)
#define DEVICE_XATOM_RESOLUTION_X(d)	(DEVICE_X_DATA (d)->Xatom_RESOLUTION_X)
#define DEVICE_XATOM_RESOLUTION_Y(d)	(DEVICE_X_DATA (d)->Xatom_RESOLUTION_Y)
#define DEVICE_XATOM_SPACING(d)		(DEVICE_X_DATA (d)->Xatom_SPACING)
#define DEVICE_XATOM_AVERAGE_WIDTH(d)	(DEVICE_X_DATA (d)->Xatom_AVERAGE_WIDTH)
#define DEVICE_XATOM_CHARSET_REGISTRY(d) (DEVICE_X_DATA (d)->Xatom_CHARSET_REGISTRY)
#define DEVICE_XATOM_CHARSET_ENCODING(d) (DEVICE_X_DATA (d)->Xatom_CHARSET_ENCODING)

#define Xt_SET_VALUE(widget, resource, value) do {	\
  Arg al;						\
  XtSetArg (al, resource, value);			\
  XtSetValues (widget, &al, 1);				\
} while (0)

#define Xt_GET_VALUE(widget, resource, location) do {	\
  Arg al;						\
  XtSetArg (al, resource, location);			\
  XtGetValues (widget, &al, 1);				\
} while (0)

/* The maximum number of widgets that can be displayed above the text
   area at one time.  Currently no more than 3 will ever actually be
   displayed (menubar, psheet, debugger panel). */
#define MAX_CONCURRENT_TOP_WIDGETS 8

struct x_frame {
	/* The widget of this frame.  This is an EmacsShell or an
	   ExternalShell. */
	Widget widget;

	/* The parent of the EmacsFrame, the menubar, and the scrollbars.
	   This is an EmacsManager. */
	Widget container;

	/* The widget of the menubar, of whatever widget class it happens to be. */
	Widget menubar_widget;

	/* The widget of the edit portion of this frame; this is an EmacsFrame,
	   and the window of this widget is what the redisplay code draws on. */
	Widget edit_widget;

	/* Lists the widgets above the text area, in the proper order.
	   Used by the EmacsManager. */
	Widget top_widgets[MAX_CONCURRENT_TOP_WIDGETS];
	int num_top_widgets;

  /*************************** Miscellaneous **************************/

	/* The icon pixmaps; these are Lisp_Image_Instance objects, or Qnil. */
	Lisp_Object icon_pixmap;
	Lisp_Object icon_pixmap_mask;

#ifdef HAVE_TOOLBARS
	int old_toolbar_size[4];

	/* We don't provide a mechanism for changing these after they are
	   initialized so we might as well keep pointers to them and avoid
	   lots of expensive calls to gc_cache_lookup. */
	GC toolbar_top_shadow_gc;
	GC toolbar_bottom_shadow_gc;
	GC toolbar_blank_background_gc;
	GC toolbar_pixmap_background_gc;
#endif				/* HAVE_TOOLBARS */

	/* geometry string that ought to be freed. */
	char *geom_free_me_please;

#ifdef HAVE_XIM
	XPoint xic_spot;	/* Spot Location cache */
#ifdef XIM_XLIB
	XIC xic;
	/* Could get these at any time by asking xic, but... */
	XIMStyle xic_style;	/* XIM Style cache */
#endif				/* XIM_XLIB */
#endif				/* HAVE_XIM */

	/* 1 if the frame is completely visible on the display, 0 otherwise.
	   if 0 the frame may have been iconified or may be totally
	   or partially hidden by another X window */
	unsigned int totally_visible_p:1;

	/* NB: Both of the following flags are derivable from the 'shell'
	   field above, but it's easier if we also have them separately here. */

	/* Are we a top-level frame?  This means that our shell is a
	   TopLevelShell, and we should do certain things to interact with
	   the window manager. */
	unsigned int top_level_frame_p:1;

#ifdef EXTERNAL_WIDGET
	/* Are we using somebody else's window for our shell window?  This
	   means that our shell is an ExternalShell.  If this flag is set, then
	   `top_level_frame_p' will never be set. */
	unsigned int external_window_p:1;
#endif				/* EXTERNAL_WIDGET */
};

#define FRAME_X_DATA(f) FRAME_TYPE_DATA (f, x)

#define FRAME_X_SHELL_WIDGET(f)	    (FRAME_X_DATA (f)->widget)
#define FRAME_X_CONTAINER_WIDGET(f) (FRAME_X_DATA (f)->container)
#define FRAME_X_MENUBAR_WIDGET(f)   (FRAME_X_DATA (f)->menubar_widget)
#define FRAME_X_TEXT_WIDGET(f)	    (FRAME_X_DATA (f)->edit_widget)
#define FRAME_X_TOP_WIDGETS(f)	    (FRAME_X_DATA (f)->top_widgets)
#define FRAME_X_NUM_TOP_WIDGETS(f)  (FRAME_X_DATA (f)->num_top_widgets)

#define FRAME_X_ICON_PIXMAP(f)	    (FRAME_X_DATA (f)->icon_pixmap)
#define FRAME_X_ICON_PIXMAP_MASK(f) (FRAME_X_DATA (f)->icon_pixmap_mask)

#ifdef HAVE_TOOLBARS
#define FRAME_X_OLD_TOOLBAR_SIZE(f, pos) (FRAME_X_DATA (f)->old_toolbar_size[pos])

#define FRAME_X_TOOLBAR_TOP_SHADOW_GC(f)	(FRAME_X_DATA (f)->toolbar_top_shadow_gc)
#define FRAME_X_TOOLBAR_BOTTOM_SHADOW_GC(f)	(FRAME_X_DATA (f)->toolbar_bottom_shadow_gc)
#define FRAME_X_TOOLBAR_BLANK_BACKGROUND_GC(f)	(FRAME_X_DATA (f)->toolbar_blank_background_gc)
#define FRAME_X_TOOLBAR_PIXMAP_BACKGROUND_GC(f) (FRAME_X_DATA (f)->toolbar_pixmap_background_gc)
#endif				/* HAVE_TOOLBARS */

#define FRAME_X_GEOM_FREE_ME_PLEASE(f) (FRAME_X_DATA (f)->geom_free_me_please)

#define FRAME_X_TOTALLY_VISIBLE_P(f) (FRAME_X_DATA (f)->totally_visible_p)
#define FRAME_X_TOP_LEVEL_FRAME_P(f) (FRAME_X_DATA (f)->top_level_frame_p)

#ifdef EXTERNAL_WIDGET
#define FRAME_X_EXTERNAL_WINDOW_P(f) (FRAME_X_DATA (f)->external_window_p)
#endif

#ifdef HAVE_XIM
#define FRAME_X_XIC_SPOT(f)  (FRAME_X_DATA (f)->xic_spot)
#ifdef XIM_XLIB
#define FRAME_X_XIC(f)	     (FRAME_X_DATA (f)->xic)
#define FRAME_X_XIC_STYLE(f) (FRAME_X_DATA (f)->xic_style)
#endif				/* XIM_XLIB */
#endif				/* HAVE_XIM */

/* Variables associated with the X display frame this emacs is using. */
extern XtAppContext Xt_app_con;

extern Lisp_Object Vx_gc_pointer_shape;
extern Lisp_Object Vx_scrollbar_pointer_shape;
extern Lisp_Object Qx_error;

extern struct console_type *x_console_type;
extern Lisp_Object Vdefault_x_device;

/* Number of pixels below each line. */
extern int x_interline_space;	/* #### implement me */

extern Fixnum x_selection_timeout;

struct frame *x_any_window_to_frame(struct device *d, Window);
struct frame *x_any_widget_or_parent_to_frame(struct device *d, Widget widget);
struct frame *decode_x_frame(Lisp_Object);
struct frame *x_window_to_frame(struct device *d, Window);
struct device *get_device_from_display(Display * dpy);
struct device *decode_x_device(Lisp_Object);

void x_handle_selection_notify(XSelectionEvent * event);
void x_handle_selection_request(XSelectionRequestEvent * event);
void x_handle_selection_clear(XSelectionClearEvent * event);
void x_handle_property_notify(XPropertyEvent * event);

void Xatoms_of_select_x(struct device *d);
void Xatoms_of_objects_x(struct device *d);

void x_wm_set_shell_iconic_p(Widget shell, int iconic_p);
void x_wm_set_cell_size(Widget wmshell, int cw, int ch);
void x_wm_set_variable_size(Widget wmshell, int width, int height);

const char *x_event_name(int event_type);
int x_error_handler(Display * disp, XErrorEvent * event);
void expect_x_error(Display * dpy);
int x_error_occurred_p(Display * dpy);
int signal_if_x_error(Display * dpy, int resumable_p);
int x_IO_error_handler(Display * disp);

void x_redraw_exposed_area(struct frame *f, int x, int y,
			   int width, int height);
void x_output_string(struct window *w, struct display_line *dl,
		     Emchar_dynarr * buf, int xpos, int xoffset,
		     int start_pixpos, int width, face_index findex,
		     int cursor, int cursor_start, int cursor_width,
		     int cursor_height);
void x_output_x_pixmap(struct frame *f, Lisp_Image_Instance * p,
		       int x, int y, int xoffset, int yoffset,
		       int width, int height,
		       unsigned long fg, unsigned long bg, GC override_gc);
void x_output_shadows(struct frame *f, int x, int y, int width,
		      int height, GC top_shadow_gc,
		      GC bottom_shadow_gc, GC background_gc,
		      int shadow_thickness, int edges);
void x_generate_shadow_pixels(struct frame *f,
			      unsigned long *top_shadow,
			      unsigned long *bottom_shadow,
			      unsigned long background,
			      unsigned long core_background);

int x_initialize_frame_menubar(struct frame *f);
void x_init_modifier_mapping(struct device *d);

#define X_ERROR_OCCURRED(dpy, body)	\
     (expect_x_error (dpy), body, x_error_occurred_p (dpy))

#define HANDLING_X_ERROR(dpy, body)	\
     (expect_x_error (dpy), body, signal_if_x_error (dpy, 0))

void Initialize_Locale(void);

#ifdef HAVE_XIM

/* X Input Method `methods' */
void XIM_init_device(struct device *d);
void XIM_init_frame(struct frame *f);
void XIM_SetSpotLocation(struct frame *f, int x, int y);
void XIM_SetGeometry(struct frame *f);
void XIM_focus_event(struct frame *f, int in_p);

#ifdef XIM_XLIB
/* XtTypeConverter */
Boolean EmacsXtCvtStringToXIMStyles(Display * dpy,
				    XrmValuePtr args,
				    Cardinal * num_args,
				    XrmValuePtr from,
				    XrmValuePtr to_in_out,
				    XtPointer * converter_data);

/* XtDestructor */
void EmacsFreeXIMStyles(XtAppContext app,
			XrmValuePtr to,
			XtPointer converter_data,
			XrmValuePtr args, Cardinal * num_args);

#ifdef DEBUG_SXEMACS
void describe_Window(Window win);
void describe_XFontSet(XFontSet font_set);
void describe_XIM(XIM im);
void describe_XIMStyle(XIMStyle style);
void describe_XIMStyles(XIMStyles * styles);
void describe_XIC(XIC ic);
void describe_event_mask(unsigned long mask);
void describe_XRectangle(char *name, XRectangle * rect);
void describe_Status(Status status);
#endif				/* DEBUG_SXEMACS */
#endif				/* XIM_XLIB */
#endif				/* HAVE_XIM */

extern int in_resource_setting;
extern int in_specifier_change_function;

extern Lisp_Object Vx_initial_argv_list;	/* #### ugh! */

/* Standins for various X encodings, until we know them better */

/* !!#### Need to verify the encoding used in lwlib -- Qnative or Qctext?
   Almost certainly the former.  Use a standin for now. */
#define Qlwlib_encoding Qnative

#define Qx_atom_name_encoding Qctext
/* font names are often stored in atoms, so it gets sticky if we set this
   to something different from atom-name encoding */
#define Qx_font_name_encoding Qctext

#define Qx_color_name_encoding Qctext

/* the following probably must agree with Qcommand_argument_encoding and
   Qenvironment_variable_encoding */
#define Qx_display_name_encoding Qnative

#endif				/* HAVE_X_WINDOWS */

#endif				/* INCLUDED_console_x_h_ */
