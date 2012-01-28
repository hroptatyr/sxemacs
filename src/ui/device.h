/* Define device-object for XEmacs.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Ben Wing
   Copyright (C) 1995 Sun Microsystems

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

/* Written by Chuck Thompson and Ben Wing. */

#ifndef INCLUDED_device_h_
#define INCLUDED_device_h_

#include "console.h"

/* This should really be in redisplay.h but by putting it here we
   won't have to ensure that redisplay.h is always included before
   this file. */
struct pixel_to_glyph_translation_cache {
	unsigned int valid:1;
	struct frame *frame;
	int low_x_coord, high_x_coord, col, obj_x;
	int low_y_coord, high_y_coord, row, obj_y;
	struct window *w;
	Bufpos bufpos;
	Bufpos closest;
	Charcount modeline_closest;
	Lisp_Object obj1, obj2;
	int retval;
};

#define DEVICE_TYPE_NAME(d) ((d)->devmeths->name)
#define DEVICE_TYPE(d) ((d)->devmeths->symbol)
#define DEVICE_IMPL_FLAG(d, f) CONMETH_IMPL_FLAG ((d)->devmeths, (f))
#define DEVICE_SPECIFIC_FRAME_PROPS(d) \
  ((d)->devmeths->device_specific_frame_props)

/******** Accessing / calling a device method *********/

#define HAS_DEVMETH_P(d, m) HAS_CONTYPE_METH_P ((d)->devmeths, m)
#define DEVMETH(d, m, args) CONTYPE_METH ((d)->devmeths, m, args)
#define MAYBE_DEVMETH(d, m, args) MAYBE_CONTYPE_METH ((d)->devmeths, m, args)
#define DEVMETH_OR_GIVEN(d, m, args, given) \
  CONTYPE_METH_OR_GIVEN((d)->devmeths, m, args, given)
#define MAYBE_INT_DEVMETH(d, m, args) \
  MAYBE_INT_CONTYPE_METH ((d)->devmeths, m, args)
#define MAYBE_LISP_DEVMETH(d, m, args) \
  MAYBE_LISP_CONTYPE_METH ((d)->devmeths, m, args)

struct device {
	struct lcrecord_header header;

	/* Methods for this device's console.  This can also be retrieved
	   through device->console, but it's faster this way. */
	struct console_methods *devmeths;

	/* Name of this device, for resourcing and printing purposes.
	   If not explicitly given, it's initialized in a device-specific
	   manner. */
	Lisp_Object name;

	/* What this device is connected to */
	Lisp_Object connection;

	/* A canonical name for the connection that is used to determine
	   whether `make-device' is being called on an existing device. */
	Lisp_Object canon_connection;

	/* List of frames on this device. */
	Lisp_Object frame_list;

	/* The console this device is on. */
	Lisp_Object console;

	/* Frame which is "currently selected".  This is what `selected-frame'
	   returns and is the default frame for many operations.  This may
	   not be the same as frame_with_focus; `select-frame' changes the
	   selected_frame but not the frame_with_focus.  However, eventually
	   either the two values will be the same, or frame_with_focus will
	   be nil: right before waiting for an event, the focus is changed
	   to point to the selected_frame if XEmacs currently has the focus
	   on this device.  Note that frame_with_focus may be nil (none of the
	   frames on this device have the window-system focus), but
	   selected_frame will never be nil if there are any frames on
	   the device. */
	Lisp_Object selected_frame;
	/* Frame that currently contains the window-manager focus, or none.
	   Note that we've split frame_with_focus into two variables.
	   frame_with_focus_real is the value we use most of the time,
	   but frame_with_focus_for_hooks is used for running the select-frame-hook
	   and deselect-frame-hook.  We do this because we split the focus handling
	   into two parts: one part (deals with drawing the solid/box cursor)
	   runs as soon as a focus event is received; the other (running the
	   hooks) runs after any pending sit-for/sleep-for/accept-process-output
	   calls are done. */
	Lisp_Object frame_with_focus_real;
	Lisp_Object frame_with_focus_for_hooks;
	/* If we have recently issued a request to change the focus as a
	   result of select-frame having been called, the following variable
	   records the frame we are trying to focus on.  The reason for this
	   is that the window manager may not grant our request to change
	   the focus (so we can't just change frame_with_focus), and we don't
	   want to keep sending requests again and again to the window manager.
	   This variable is reset whenever a focus-change event is seen. */
	Lisp_Object frame_that_ought_to_have_focus;

	/* Color class of this device. */
	Lisp_Object device_class;

	/* Alist of values for user-defined tags in this device. */
	Lisp_Object user_defined_tags;

	/* Hash tables for device-specific objects (fonts, colors, etc).
	   These are key-weak hash tables (or hash tables containing key-weak
	   hash tables) so that they disappear when the key goes away. */

	/* This is a simple key-weak hash table hashing color names to
	   instances. */
	Lisp_Object color_instance_cache;

	/* This is a simple key-weak hash table hashing font names to
	   instances. */
	Lisp_Object font_instance_cache;

#ifdef MULE
	/* This is a bi-level cache, where the hash table in this slot here
	   indexes charset objects to key-weak hash tables, which in turn
	   index font names to more specific font names that match the
	   given charset's registry.  This speeds up the horrendously
	   slow XListFonts() operation that needs to be done in order
	   to determine an appropriate font. */
	Lisp_Object charset_font_cache;
#endif

	/* This is a bi-level cache, where the hash table in this slot here
	   indexes image-instance-type masks (there are currently 6
	   image-instance types and thus 64 possible masks) to key-weak hash
	   tables like the one for colors. */
	Lisp_Object image_instance_cache;

	/* A structure of auxiliary data specific to the device type.
	   struct x_device is used for X window frames; defined in console-x.h
	   struct tty_device is used to TTY's; defined in console-tty.h */
	void *device_data;

	/* redisplay flags */
	unsigned int buffers_changed:1;
	unsigned int clip_changed:1;
	unsigned int extents_changed:1;
	unsigned int faces_changed:1;
	unsigned int frame_changed:1;
	unsigned int frame_layout_changed:1;	/* The layout of frame
						   elements has changed. */
	unsigned int glyphs_changed:1;
	unsigned int subwindows_changed:1;
	unsigned int subwindows_state_changed:1;
	unsigned int icon_changed:1;
	unsigned int menubar_changed:1;
	unsigned int modeline_changed:1;
	unsigned int point_changed:1;
	unsigned int size_changed:1;
	unsigned int gutter_changed:1;
	unsigned int toolbar_changed:1;
	unsigned int windows_changed:1;
	unsigned int windows_structure_changed:1;

	unsigned int locked:1;

	/* Cache information about last pixel position translated to a
	   glyph.  The law of locality applies very heavily here so caching
	   the value leads to a significant win.  At the moment this is
	   really X specific but once we have generic mouse support it won't
	   be. */
	struct pixel_to_glyph_translation_cache pixel_to_glyph_cache;

	/* Output baud rate of device; used for redisplay decisions.  */
	int baud_rate;

	/* sound flags */
	unsigned int on_console_p:1;
	unsigned int connected_to_nas_p:1;

	/* File descriptors for input and output.  Much of the time
	   (but not always) these will be the same.  For an X device,
	   these both hold the file descriptor of the socket used
	   to communicate with the X server.  For a TTY device, these
	   may or may not be the same and point to the terminal that
	   is used for I/O. */
	int infd, outfd;

	/* infd and outfd are moved outside HAVE_UNIXOID_EVENT_LOOP conditionals,
	   because Win32, presumably the first port which does not use select()
	   polling, DOES have handles for a console device. -- kkm */

	/* We removed Win32 support, but does it make sense to move infd
	   and outfd back inside HAVE_UNIXOID_EVENT_LOOP conditionals?
	   -- njsf */

#ifdef HAVE_UNIXOID_EVENT_LOOP
	/* holds some data necessary for SIGIO control.  Perhaps this should
	   be inside of device_data; but it is used for both TTY's and X
	   device.  Perhaps it should be conditionalized on SIGIO; but
	   this requires including syssignal.h and systty.h. */
	int old_fcntl_owner;
#endif
};

DECLARE_LRECORD(device, struct device);
#define XDEVICE(x) XRECORD (x, device, struct device)
#define XSETDEVICE(x, p) XSETRECORD (x, p, device)
#define wrap_device(p) wrap_object (p)
#define DEVICEP(x) RECORDP (x, device)
#define CHECK_DEVICE(x) CHECK_RECORD (x, device)
#define CONCHECK_DEVICE(x) CONCHECK_RECORD (x, device)

#define CHECK_LIVE_DEVICE(x) do {			\
  CHECK_DEVICE (x);					\
  if (! DEVICE_LIVE_P (XDEVICE (x)))			\
    dead_wrong_type_argument (Qdevice_live_p, (x));	\
} while (0)
#define CONCHECK_LIVE_DEVICE(x) do {			\
  CONCHECK_DEVICE (x);					\
  if (! DEVICE_LIVE_P (XDEVICE (x)))			\
    x = wrong_type_argument (Qdevice_live_p, (x));	\
} while (0)

#define DEVICE_TYPE_P(d, type)	EQ (DEVICE_TYPE (d), Q##type)

#ifdef ERROR_CHECK_TYPECHECK
extern_inline struct device *error_check_device_type(struct device *d,
						     Lisp_Object sym);
extern_inline struct device *error_check_device_type(struct device *d,
						     Lisp_Object sym)
{
	assert(EQ(DEVICE_TYPE(d), sym));
	return d;
}

# define DEVICE_TYPE_DATA(d, type)			\
  ((struct type##_device *) error_check_device_type (d, Q##type)->device_data)
#else
# define DEVICE_TYPE_DATA(d, type)			\
  ((struct type##_device *) (d)->device_data)
#endif

#define CHECK_DEVICE_TYPE(x, type)			\
  do {							\
    CHECK_DEVICE (x);					\
    if (!(DEVICEP (x) && DEVICE_TYPE_P (XDEVICE (x),	\
					 type)))	\
      dead_wrong_type_argument				\
	(type##_console_methods->predicate_symbol, x);	\
  } while (0)
#define CONCHECK_DEVICE_TYPE(x, type)			\
  do {							\
    CONCHECK_DEVICE (x);				\
    if (!(DEVICEP (x) && DEVICE_TYPE_P (XDEVICE (x),	\
					 type)))	\
      x = wrong_type_argument				\
	(type##_console_methods->predicate_symbol, x);	\
  } while (0)

#define DEVICE_DISPLAY_P(dev)				\
  (DEVICE_LIVE_P (dev) &&				\
    !DEVICE_IMPL_FLAG (dev, XDEVIMPF_IS_A_PRINTER))

#define CHECK_DISPLAY_DEVICE(dev)			\
  do {							\
    CHECK_DEVICE (dev);					\
    if (!(DEVICEP (dev)					\
	  && DEVICE_DISPLAY_P (XDEVICE (dev))))		\
      dead_wrong_type_argument (Qdisplay, dev);		\
  } while (0)

#define CONCHECK_DISPLAY_DEVICE(dev)			\
  do {							\
    CONCHECK_DEVICE (dev);				\
    if (!(DEVICEP (dev)					\
	  && DEVICE_DISPLAY_P (XDEVICE (dev))))		\
      wrong_type_argument (Qdisplay, dev);		\
  } while (0)

#define DEVICE_PRINTER_P(dev)				\
  (DEVICE_LIVE_P (dev) && !DEVICE_DISPLAY_P (dev))

#define CHECK_PRINTER_DEVICE(dev)			\
  do {							\
    CHECK_DEVICE (dev);					\
    if (!(DEVICEP (dev)					\
	  && DEVICE_PRINTER_P (XDEVICE (dev))))		\
      dead_wrong_type_argument (Qprinter, dev);		\
  } while (0)

#define CONCHECK_PRINTER_DEVICE(dev)			\
  do {							\
    CONCHECK_DEVICE (dev);				\
    if (!(DEVICEP (dev)					\
	  && DEVICE_PRINTER_P (XDEVICE (dev))))		\
      wrong_type_argument (Qprinter, dev);		\
  } while (0)

/* #### These should be in the device-*.h files but there are
   too many places where the abstraction is broken.  Need to
   fix. */

#define DEVICE_X_P(dev) CONSOLE_TYPESYM_X_P (DEVICE_TYPE (dev))
#define CHECK_X_DEVICE(z) CHECK_DEVICE_TYPE (z, x)
#define CONCHECK_X_DEVICE(z) CONCHECK_DEVICE_TYPE (z, x)

#define DEVICE_TTY_P(dev) CONSOLE_TYPESYM_TTY_P (DEVICE_TYPE (dev))
#define CHECK_TTY_DEVICE(z) CHECK_DEVICE_TYPE (z, tty)
#define CONCHECK_TTY_DEVICE(z) CONCHECK_DEVICE_TYPE (z, tty)

#define DEVICE_STREAM_P(dev) CONSOLE_TYPESYM_STREAM_P (DEVICE_TYPE (dev))
#define CHECK_STREAM_DEVICE(z) CHECK_DEVICE_TYPE (z, stream)
#define CONCHECK_STREAM_DEVICE(z) CONCHECK_DEVICE_TYPE (z, stream)

#define DEVICE_WIN_P(dev) CONSOLE_TYPESYM_WIN_P (DEVICE_TYPE (dev))

EXFUN(Fdevice_console, 1);
EXFUN(Fdevice_name, 1);
EXFUN(Ffind_device, 2);
EXFUN(Fmake_device, 3);
EXFUN(Fselected_device, 1);

extern Lisp_Object Qcreate_device_hook, Qdelete_device_hook, Qgrayscale;
extern Lisp_Object Qinit_post_tty_win, Qmono, Vdefault_x_device;
extern Lisp_Object Vdevice_class_list;

int valid_device_class_p(Lisp_Object class);

#define DEVICE_LIVE_P(d) (!EQ (DEVICE_TYPE (d), Qdead))

#define DEVICE_REDISPLAY_INFO(d) ((d)->redisplay_info)

#define DEVICE_NAME(d) ((d)->name)
#define DEVICE_CLASS(d) ((d)->device_class)
/* Catch people attempting to set this. */
#define DEVICE_SELECTED_FRAME(d) NON_LVALUE ((d)->selected_frame)
#define DEVICE_FRAME_WITH_FOCUS_REAL(d) ((d)->frame_with_focus_real)
#define DEVICE_FRAME_WITH_FOCUS_FOR_HOOKS(d) ((d)->frame_with_focus_for_hooks)
#define DEVICE_FRAME_THAT_OUGHT_TO_HAVE_FOCUS(d)			\
  ((d)->frame_that_ought_to_have_focus)
#define DEVICE_USER_DEFINED_TAGS(d) ((d)->user_defined_tags)
#define DEVICE_FRAME_LIST(d) ((d)->frame_list)
#define DEVICE_CONNECTION(d) ((d)->connection)
#define DEVICE_CANON_CONNECTION(d) ((d)->canon_connection)
#define DEVICE_CONSOLE(d) ((d)->console)
#define DEVICE_BAUD_RATE(d) ((d)->baud_rate)
#define DEVICE_INFD(d) ((d)->infd)
#define DEVICE_OUTFD(d) ((d)->outfd)
#define DEVICE_OLD_FCNTL_OWNER(d) ((d)->old_fcntl_owner)
#define DEVICE_ON_CONSOLE_P(d) ((d)->on_console_p)
#define DEVICE_CONNECTED_TO_NAS_P(d) ((d)->connected_to_nas_p)

#define LOCK_DEVICE(d) ((void) ((d)->locked = 1))
#define UNLOCK_DEVICE(d) ((void) ((d)->locked = 0))

#define INVALIDATE_DEVICE_PIXEL_TO_GLYPH_CACHE(d)			\
  ((void) ((d)->pixel_to_glyph_cache.valid = 0))

#define INVALIDATE_PIXEL_TO_GLYPH_CACHE do {					\
  Lisp_Object IPTGC_devcons, IPTGC_concons;					\
  DEVICE_LOOP_NO_BREAK (IPTGC_devcons, IPTGC_concons)				\
    INVALIDATE_DEVICE_PIXEL_TO_GLYPH_CACHE (XDEVICE (XCAR (IPTGC_devcons)));	\
} while (0)

#define MARK_DEVICE_FACES_CHANGED(d)			\
  ((void) (faces_changed = (d)->faces_changed = 1))

#define MARK_DEVICE_GLYPHS_CHANGED(d)			\
  ((void) (glyphs_changed = (d)->glyphs_changed = 1))

#define MARK_DEVICE_SUBWINDOWS_CHANGED(d)			\
  ((void) (subwindows_changed = (d)->subwindows_changed = 1))

#define MARK_DEVICE_SUBWINDOWS_STATE_CHANGED(d)		\
  ((void) (subwindows_state_changed = (d)->subwindows_state_changed = 1))

#define MARK_DEVICE_TOOLBARS_CHANGED(d)			\
  ((void) (toolbar_changed = (d)->toolbar_changed = 1))

#define MARK_DEVICE_GUTTERS_CHANGED(d)		\
  ((void) (gutter_changed = (d)->gutter_changed = 1))

#define MARK_DEVICE_SIZE_CHANGED(d)			\
  ((void) (size_changed = (d)->size_changed = 1))

#define MARK_DEVICE_FRAMES_FACES_CHANGED(d) do {	\
  struct device *mdffc_d = (d);				\
  Lisp_Object frmcons;					\
  DEVICE_FRAME_LOOP (frmcons, mdffc_d)			\
    XFRAME (XCAR (frmcons))->faces_changed = 1;		\
  MARK_DEVICE_FACES_CHANGED (mdffc_d);			\
} while (0)

#define MARK_DEVICE_FRAMES_GLYPHS_CHANGED(d) do {	\
  struct device *mdffc_d = (d);				\
  Lisp_Object frmcons;					\
  DEVICE_FRAME_LOOP (frmcons, mdffc_d)			\
    XFRAME (XCAR (frmcons))->glyphs_changed = 1;		\
  MARK_DEVICE_GLYPHS_CHANGED (mdffc_d);		\
} while (0)

#define MARK_DEVICE_FRAME_CHANGED(d)			\
  ((void) (frame_changed = (d)->frame_changed = 1))

#define MARK_DEVICE_FRAME_LAYOUT_CHANGED(d)			\
  ((void) (frame_layout_changed = (d)->frame_layout_changed = 1))

#define MARK_DEVICE_WINDOWS_CHANGED(d)			\
  ((void) (windows_changed = (d)->windows_changed = 1))

#define MARK_DEVICE_WINDOWS_STRUCTURE_CHANGED(d)	\
  ((void) (windows_structure_changed = (d)->windows_structure_changed = 1))

/* #### unify this with DOMAIN_DEVICE once the latter has image instances
   expunged from it. */
/* This turns out to be used heavily so we make it a macro to make it
   inline.  Also, the majority of the time the object will turn out to
   be a window so we move it from being checked last to being checked
   first. */
#define DFW_DEVICE(obj)					\
   (WINDOWP (obj) ? WINDOW_DEVICE (XWINDOW (obj))	\
 : (FRAMEP  (obj) ? FRAME_DEVICE (XFRAME (obj))		\
 : (DEVICEP (obj) ? obj					\
 : Qnil)))

/* NO_BREAK means that "break" doesn't do what you think it does!
   Use goto instead.  "continue" is OK, though. */
#define DEVICE_LOOP_NO_BREAK(devcons, concons)			\
  CONSOLE_LOOP (concons)					\
    CONSOLE_DEVICE_LOOP (devcons, XCONSOLE (XCAR (concons)))
#define DEVICE_FRAME_LOOP(frmcons, d) \
  LIST_LOOP (frmcons, DEVICE_FRAME_LIST (d))
#define CONSOLE_FRAME_LOOP_NO_BREAK(frmcons, devcons, con) \
  CONSOLE_DEVICE_LOOP (devcons, con)			   \
    DEVICE_FRAME_LOOP (frmcons, XDEVICE (XCAR (devcons)))

void select_device_1(Lisp_Object);
struct device *decode_device(Lisp_Object);
void handle_asynch_device_change(void);
void call_critical_lisp_code(struct device *d, Lisp_Object function,
			     Lisp_Object object);
void delete_device_internal(struct device *d, int force,
			    int called_from_delete_console, int from_io_error);
void io_error_delete_device(Lisp_Object device);
Lisp_Object find_nonminibuffer_frame_not_on_device(Lisp_Object device);
void set_device_selected_frame(struct device *d, Lisp_Object frame);
Lisp_Object domain_device_type(Lisp_Object domain);
int window_system_pixelated_geometry(Lisp_Object domain);

#endif				/* INCLUDED_device_h_ */
