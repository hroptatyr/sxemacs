/* Functions for the mswindows window system.
   Copyright (C) 1989, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996 Ben Wing.

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

/* Synched up with: Not synched with FSF. */

/* Authorship:

   Ultimately based on FSF.
   Substantially rewritten for XEmacs by Ben Wing.
   Rewritten for mswindows by Jonathan Harris, November 1997 for 21.0.
   Graphics features added and frame resizing fiddled with by Andy Piper.
 */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "elhash.h"
#include "console-msw.h"
#include "glyphs-msw.h"
#include "elhash.h"
#include "events.h"
#include "faces.h"
#include "frame.h"
#include "redisplay.h"
#include "window.h"

#define MSWINDOWS_FRAME_STYLE (WS_CLIPCHILDREN | WS_CLIPSIBLINGS | WS_OVERLAPPEDWINDOW)
#define MSWINDOWS_POPUP_STYLE (WS_CLIPCHILDREN | WS_CLIPSIBLINGS | WS_POPUP \
			       | WS_CAPTION | WS_BORDER | WS_SYSMENU | WS_MINIMIZEBOX)

#define MSWINDOWS_FRAME_EXSTYLE WS_EX_OVERLAPPEDWINDOW
#define MSWINDOWS_POPUP_EXSTYLE WS_EX_PALETTEWINDOW

/* Default popup left top corner offset from the same
   corner of the parent frame, in pixel */
#define POPUP_OFFSET 30

/* Default popup size, in characters */
#define POPUP_WIDTH 30
#define POPUP_HEIGHT 10

/* Default regular frame size, in characters */
#define DEFAULT_FRAME_WIDTH 80
#define DEFAULT_FRAME_HEIGHT 35

#ifdef HAVE_MENUBARS
#define ADJR_MENUFLAG TRUE
#else
#define ADJR_MENUFLAG FALSE
#endif

/* Default properties to use when creating frames.  */
Lisp_Object Vdefault_mswindows_frame_plist;
Lisp_Object Vdefault_msprinter_frame_plist;
Lisp_Object Vmswindows_use_system_frame_size_defaults;

/* This does not need to be GC protected, as it holds a
   frame Lisp_Object already protected by Fmake_frame */
Lisp_Object Vmswindows_frame_being_created;

/*---------------------------------------------------------------------*/
/*-----                    DISPLAY FRAME                          -----*/
/*---------------------------------------------------------------------*/

HWND
mswindows_get_selected_frame_hwnd (void)
{
  Lisp_Object frame, device;

  device = Ffind_device (Qnil, Qmswindows);
  if (NILP (device))
    return NULL;
  frame = DEVICE_SELECTED_FRAME (XDEVICE (device));
  if (NILP (frame))
    return NULL;

  return FRAME_MSWINDOWS_HANDLE (XFRAME (frame));
}

static void
mswindows_init_frame_1 (struct frame *f, Lisp_Object props)
{
  Lisp_Object initially_unmapped;
  Lisp_Object name, height, width, popup, top, left;
  Lisp_Object frame_obj = Qnil;
  RECT rect;
  XEMACS_RECT_WH rect_default;
  DWORD style, exstyle;
  HWND hwnd, hwnd_parent;

  /* Pick up relevant properties */
  initially_unmapped = Fplist_get (props, Qinitially_unmapped, Qnil);
  name = Fplist_get (props, Qname, Qnil);

  popup = Fplist_get (props, Qpopup, Qnil);
  if (EQ (popup, Qt))
    popup = Fselected_frame (Qnil);

  left = Fplist_get (props, Qleft, Qnil);
  if (!NILP (left))
    CHECK_INT (left);

  top = Fplist_get (props, Qtop, Qnil);
  if (!NILP (top))
    CHECK_INT (top);

  width = Fplist_get (props, Qwidth, Qnil);
  if (!NILP (width))
    CHECK_INT (width);

  height = Fplist_get (props, Qheight, Qnil);
  if (!NILP (height))
    CHECK_INT (height);

  f->frame_data = xnew_and_zero (struct mswindows_frame);
  FRAME_MSWINDOWS_TARGET_RECT (f) = xnew_and_zero (XEMACS_RECT_WH);

  FRAME_MSWINDOWS_TARGET_RECT (f)->left = NILP (left) ? -1 : abs (XINT (left));
  FRAME_MSWINDOWS_TARGET_RECT (f)->top = NILP (top) ? -1 : abs (XINT (top));
  FRAME_MSWINDOWS_TARGET_RECT (f)->width = NILP (width) ? -1 :
    abs (XINT (width));
  FRAME_MSWINDOWS_TARGET_RECT (f)->height = NILP (height) ? -1 :
    abs (XINT (height));

  /* Misc frame stuff */
  FRAME_MSWINDOWS_MENU_HASH_TABLE(f) = Qnil;
#ifdef HAVE_TOOLBARS
  FRAME_MSWINDOWS_TOOLBAR_HASH_TABLE(f) =
    make_lisp_hash_table (50, HASH_TABLE_NON_WEAK, HASH_TABLE_EQUAL);
#endif
  /* hashtable of instantiated glyphs on the frame. */
  FRAME_MSWINDOWS_WIDGET_HASH_TABLE1 (f) =
    make_lisp_hash_table (50, HASH_TABLE_VALUE_WEAK, HASH_TABLE_EQUAL);
  FRAME_MSWINDOWS_WIDGET_HASH_TABLE2 (f) =
    make_lisp_hash_table (50, HASH_TABLE_VALUE_WEAK, HASH_TABLE_EQUAL);
  FRAME_MSWINDOWS_WIDGET_HASH_TABLE3 (f) =
    make_lisp_hash_table (50, HASH_TABLE_VALUE_WEAK, HASH_TABLE_EQUAL);
  /* Will initialize these in WM_SIZE handler. We cannot do it now,
     because we do not know what is CW_USEDEFAULT height and width */
  FRAME_WIDTH (f) = 0;
  FRAME_HEIGHT (f) = 0;
  FRAME_PIXWIDTH (f) = 0;
  FRAME_PIXHEIGHT (f) = 0;

  if (NILP (popup))
    {
      style = MSWINDOWS_FRAME_STYLE;
      exstyle = MSWINDOWS_FRAME_EXSTYLE;
      hwnd_parent = NULL;

      rect_default.left = rect_default.top = CW_USEDEFAULT;
      rect_default.width = rect_default.height = CW_USEDEFAULT;
    }
  else
    {
      style = MSWINDOWS_POPUP_STYLE;
      exstyle = MSWINDOWS_POPUP_EXSTYLE;

      CHECK_MSWINDOWS_FRAME (popup);
      hwnd_parent = FRAME_MSWINDOWS_HANDLE (XFRAME (popup));
      assert (IsWindow (hwnd_parent));

      /* We cannot use CW_USEDEFAULT when creating a popup window.
	 So by default, we offset the new popup 30 pixels right
	 and down from its parent, and give it size of 30x10 characters.
	 These dimensions look adequate on both high and low res monitors */
      GetWindowRect (hwnd_parent, &rect);
      rect_default.left = rect.left + POPUP_OFFSET;
      rect_default.top = rect.top + POPUP_OFFSET;
      char_to_real_pixel_size (f, POPUP_WIDTH, POPUP_HEIGHT,
			       &rect_default.width, &rect_default.height);
      FRAME_MSWINDOWS_POPUP (f) = 1;
    }

  AdjustWindowRectEx(&rect, style, ADJR_MENUFLAG, exstyle);

  XSETFRAME (frame_obj, f);

  Vmswindows_frame_being_created = frame_obj;

  hwnd = CreateWindowEx (exstyle,
			 XEMACS_CLASS,
			 STRINGP (f->name) ? (LPCTSTR) XSTRING_DATA (f->name) :
			 (STRINGP (name) ? (LPCTSTR) XSTRING_DATA (name) :
			  XEMACS_CLASS),
			 style,
			 rect_default.left, rect_default.top,
			 rect_default.width, rect_default.height,
			 hwnd_parent, NULL, NULL, NULL);

  Vmswindows_frame_being_created = Qnil;

  if (hwnd == NULL)
    invalid_operation ("System call to create frame failed",
		       STRINGP (f->name) ? f->name :
		       STRINGP (name) ? name :
		       Qunbound);

  FRAME_MSWINDOWS_HANDLE(f) = hwnd;

  SetWindowLong (hwnd, XWL_FRAMEOBJ, (LONG)LISP_TO_VOID(frame_obj));
  FRAME_MSWINDOWS_DC(f) = GetDC (hwnd);
  SetTextAlign (FRAME_MSWINDOWS_DC(f), TA_BASELINE | TA_LEFT | TA_NOUPDATECP);

  if (FRAME_MSWINDOWS_POPUP (f))
    mswindows_register_popup_frame (frame_obj);
}

static void
mswindows_init_frame_2 (struct frame *f, Lisp_Object props)
{
  if (NILP (Vmswindows_use_system_frame_size_defaults))
    {
      /* I don't think anything can set the frame size before this
         since we don't have X resources. This may change if we look
         at the registry. Even so these values can get overridden
         later.*/
      XEMACS_RECT_WH dest = { -1, -1, DEFAULT_FRAME_WIDTH,
			      DEFAULT_FRAME_HEIGHT };
      mswindows_size_frame_internal (f, &dest);
    }
}

/* Called after frame's properties are set */
static void
mswindows_init_frame_3 (struct frame *f)
{
  /* Don't do this earlier or we get a WM_PAINT before the frame is ready.
   * The SW_x parameter in the first call that an app makes to ShowWindow is
   * ignored, and the parameter specified in the caller's STARTUPINFO is
   * substituted instead. That parameter is SW_HIDE if we were started by
   * runemacs, so call this twice. #### runemacs is evil */
  ShowWindow (FRAME_MSWINDOWS_HANDLE(f), SW_SHOWNORMAL);
  ShowWindow (FRAME_MSWINDOWS_HANDLE(f), SW_SHOWNORMAL);
  SetForegroundWindow (FRAME_MSWINDOWS_HANDLE(f));
  DragAcceptFiles (FRAME_MSWINDOWS_HANDLE(f), TRUE);
}

static void
mswindows_after_init_frame (struct frame *f, int first_on_device,
		            int first_on_console)
{
  /* Windows, unlike X, is very synchronous. After the initial
     frame is created, it will never be displayed, except for
     hollow border, unless we start pumping messages. Load progress
     messages show in the bottom of the hollow frame, which is ugly.
     We redisplay the initial frame here, so modeline and root window
     background show.
  */
  if (first_on_console)
    redisplay ();
}

static void
mswindows_mark_frame (struct frame *f)
{
  mark_object (FRAME_MSWINDOWS_MENU_HASH_TABLE (f));
#ifdef HAVE_TOOLBARS
  mark_object (FRAME_MSWINDOWS_TOOLBAR_HASH_TABLE (f));
#endif
  mark_object (FRAME_MSWINDOWS_WIDGET_HASH_TABLE1 (f));
  mark_object (FRAME_MSWINDOWS_WIDGET_HASH_TABLE2 (f));
  mark_object (FRAME_MSWINDOWS_WIDGET_HASH_TABLE3 (f));
}

static void
mswindows_focus_on_frame (struct frame *f)
{
  SetForegroundWindow (FRAME_MSWINDOWS_HANDLE(f));
}

static void
mswindows_delete_frame (struct frame *f)
{
  if (f->frame_data)
    {
      Lisp_Object frame;
      XSETFRAME (frame, f);
      mswindows_unregister_popup_frame (frame);
      ReleaseDC(FRAME_MSWINDOWS_HANDLE(f), FRAME_MSWINDOWS_DC(f));
      DestroyWindow(FRAME_MSWINDOWS_HANDLE(f));
      xfree (f->frame_data);
    }
  f->frame_data = 0;
}

static void
mswindows_set_frame_size (struct frame *f, int width, int height)
{
  RECT rect;
  int columns, rows;

  rect.left = rect.top = 0;
  rect.right = width;
  rect.bottom = height;

  pixel_to_char_size (f, rect.right, rect.bottom, &columns, &rows);
  change_frame_size (f, rows, columns, 0);

  AdjustWindowRectEx (&rect,
		      GetWindowLong (FRAME_MSWINDOWS_HANDLE(f), GWL_STYLE),
		      GetMenu (FRAME_MSWINDOWS_HANDLE(f)) != NULL,
		      GetWindowLong (FRAME_MSWINDOWS_HANDLE(f), GWL_EXSTYLE));

  if (IsIconic (FRAME_MSWINDOWS_HANDLE(f)) || IsZoomed (FRAME_MSWINDOWS_HANDLE(f)))
    ShowWindow (FRAME_MSWINDOWS_HANDLE(f), SW_RESTORE);

  SetWindowPos (FRAME_MSWINDOWS_HANDLE(f), NULL,
		0, 0, rect.right-rect.left, rect.bottom-rect.top,
		SWP_NOACTIVATE | SWP_NOZORDER | SWP_NOSENDCHANGING | SWP_NOMOVE);
}

static void
mswindows_set_frame_position (struct frame *f, int xoff, int yoff)
{
  SetWindowPos (FRAME_MSWINDOWS_HANDLE(f), NULL,
		xoff, yoff, 0, 0,
		SWP_NOACTIVATE | SWP_NOZORDER | SWP_NOSENDCHANGING | SWP_NOSIZE);
}

static void
mswindows_make_frame_visible (struct frame *f)
{
  if (!FRAME_VISIBLE_P(f))
    ShowWindow (FRAME_MSWINDOWS_HANDLE(f), SW_RESTORE);
  else
    ShowWindow (FRAME_MSWINDOWS_HANDLE(f), SW_SHOW);
  f->visible = 1;
  f->iconified = 0;
}

static void
mswindows_make_frame_invisible (struct frame *f)
{
  if (!FRAME_VISIBLE_P(f))
    return;

  ShowWindow (FRAME_MSWINDOWS_HANDLE(f), SW_HIDE);
  f->visible = 0;
}

static int
mswindows_frame_totally_visible_p (struct frame *f)
{
  RECT rc_me, rc_other, rc_temp;
  HWND hwnd = FRAME_MSWINDOWS_HANDLE(f);

  /* We test against not a whole window rectangle, only against its
     client part. So, if non-client are is covered and client area is
     not, we return true. */
  GetClientRect (hwnd, &rc_me);
  MapWindowPoints (hwnd, HWND_DESKTOP, (LPPOINT)&rc_me, 2);

  /* First see if we're off the desktop */
  GetWindowRect (GetDesktopWindow(), &rc_other);
  UnionRect(&rc_temp, &rc_me, &rc_other);
  if (!EqualRect (&rc_temp, &rc_other))
    return 0;

  /* Then see if any window above us obscures us */
  while ((hwnd = GetWindow (hwnd, GW_HWNDPREV)) != NULL)
    if (IsWindowVisible (hwnd))
      {
	GetWindowRect (hwnd, &rc_other);
	if (IntersectRect(&rc_temp, &rc_me, &rc_other))
	  return 0;
      }

  return 1;
}

static int
mswindows_frame_visible_p (struct frame *f)
{
  return IsWindowVisible (FRAME_MSWINDOWS_HANDLE(f))
    && !IsIconic (FRAME_MSWINDOWS_HANDLE(f));
}


static void
mswindows_iconify_frame (struct frame *f)
{
  ShowWindow (FRAME_MSWINDOWS_HANDLE(f), SW_MINIMIZE);
  f->visible = 0;
  f->iconified = 1;
}

static int
mswindows_frame_iconified_p (struct frame *f)
{
  return IsIconic (FRAME_MSWINDOWS_HANDLE(f));
}

static void
mswindows_set_frame_icon (struct frame *f)
{
  if (IMAGE_INSTANCEP (f->icon)
      && IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (f->icon)))
    {
      if (!XIMAGE_INSTANCE_MSWINDOWS_ICON (f->icon))
	{
	  mswindows_initialize_image_instance_icon (XIMAGE_INSTANCE (f->icon),
						    FALSE);
	}

      SetClassLong (FRAME_MSWINDOWS_HANDLE (f), GCL_HICON,
		    (LONG) XIMAGE_INSTANCE_MSWINDOWS_ICON (f->icon));
    }
}

static void
mswindows_set_frame_pointer (struct frame *f)
{
  if (IMAGE_INSTANCEP (f->pointer)
      && IMAGE_INSTANCE_TYPE (XIMAGE_INSTANCE (f->pointer)) == IMAGE_POINTER)
    {
      SetClassLong (FRAME_MSWINDOWS_HANDLE (f), GCL_HCURSOR,
		    (LONG) XIMAGE_INSTANCE_MSWINDOWS_ICON (f->pointer));
      /* we only have to do this because GC doesn't cause a mouse
         event and doesn't give time to event processing even if it
         did. */
      SetCursor (XIMAGE_INSTANCE_MSWINDOWS_ICON (f->pointer));
    }
}

static void
mswindows_set_mouse_position (struct window *w, int x, int y)
{
  struct frame *f = XFRAME (w->frame);
  POINT pt;

  pt.x = w->pixel_left + x;
  pt.y = w->pixel_top  + y;
  ClientToScreen (FRAME_MSWINDOWS_HANDLE(f), &pt);
  SetCursorPos (pt.x, pt.y);
}

static int
mswindows_get_mouse_position (struct device *d, Lisp_Object *frame, int *x, int *y)
{
  POINT pt;
  HWND hwnd;

  GetCursorPos (&pt);

  /* What's under cursor? */
  hwnd = WindowFromPoint (pt);
  if (hwnd == NULL)
    return 0;

  /* Get grandest parent of the window */
  {
    HWND hwnd_parent;
    while ((hwnd_parent = GetParent (hwnd)) != NULL)
      hwnd = hwnd_parent;
  }

  /* Make sure it belongs to us */
  if (GetWindowThreadProcessId (hwnd, NULL) != GetCurrentThreadId ())
    return 0;

  /* And that the window is an XEmacs frame */
  {
    char class_name [sizeof(XEMACS_CLASS) + 1];
    if (!GetClassName (hwnd, class_name, sizeof(XEMACS_CLASS))
	|| strcmp (class_name, XEMACS_CLASS) != 0)
      return 0;
  }

  /* Yippie! */
  ScreenToClient (hwnd, &pt);
  VOID_TO_LISP (*frame, GetWindowLong (hwnd, XWL_FRAMEOBJ));
  *x = pt.x;
  *y = pt.y;
  return 1;
}

static void
mswindows_raise_frame (struct frame *f)
{
  BringWindowToTop (FRAME_MSWINDOWS_HANDLE(f));
}

static void
mswindows_lower_frame (struct frame *f)
{
  SetWindowPos (FRAME_MSWINDOWS_HANDLE(f), HWND_BOTTOM, 0, 0, 0, 0,
		SWP_NOSIZE | SWP_NOMOVE | SWP_NOSENDCHANGING);
}

static void
mswindows_enable_frame (struct frame *f)
{
  EnableWindow (FRAME_MSWINDOWS_HANDLE (f), TRUE);
}

static void
mswindows_disable_frame (struct frame *f)
{
  EnableWindow (FRAME_MSWINDOWS_HANDLE (f), FALSE);
}

static void
mswindows_set_title_from_bufbyte (struct frame *f, Bufbyte *title)
{
  unsigned int new_checksum = hash_string (title, strlen (title));
  if (new_checksum != FRAME_MSWINDOWS_TITLE_CHECKSUM(f))
    {
      FRAME_MSWINDOWS_TITLE_CHECKSUM(f) = new_checksum;
      SetWindowText (FRAME_MSWINDOWS_HANDLE(f), title);
    }
}

static Lisp_Object
mswindows_frame_property (struct frame *f, Lisp_Object property)
{
  if (EQ (Qleft, property) || EQ (Qtop, property))
    {
      RECT rc;
      GetWindowRect (FRAME_MSWINDOWS_HANDLE(f), &rc);
      return make_int (EQ (Qtop,  property) ? rc.top : rc.left);
    }
  return Qunbound;
}

static int
mswindows_internal_frame_property_p (struct frame *f, Lisp_Object property)
{
  return EQ (property, Qleft)
    || EQ (property, Qtop);
  /* #### frame-x.c has also this. Why?
    || STRINGP (property);
  */
}

static Lisp_Object
mswindows_frame_properties (struct frame *f)
{
  Lisp_Object props = Qnil;
  RECT rc;
  GetWindowRect (FRAME_MSWINDOWS_HANDLE(f), &rc);

  props = cons3 (Qtop,  make_int (rc.top), props);
  props = cons3 (Qleft, make_int (rc.left), props);

  return props;
}

static void
mswindows_set_frame_properties (struct frame *f, Lisp_Object plist)
{
  int x=-1, y=-1;
  int width = -1, height = -1;
  BOOL width_specified_p = FALSE;
  BOOL height_specified_p = FALSE;
  BOOL x_specified_p = FALSE;
  BOOL y_specified_p = FALSE;
  Lisp_Object tail;

  /* Extract the properties from plist */
  for (tail = plist; !NILP (tail); tail = Fcdr (Fcdr (tail)))
    {
      Lisp_Object prop = Fcar (tail);
      Lisp_Object val = Fcar (Fcdr (tail));

      if (SYMBOLP (prop))
	{
	  /* Kludge to handle the font property. */
	  if (EQ (prop, Qfont))
	    {
	      /* If the value is not a string we silently ignore it. */
	      if (STRINGP (val))
		{
		  Lisp_Object frm, font_spec;

		  XSETFRAME (frm, f);
		  font_spec = Fget (Fget_face (Qdefault), Qfont, Qnil);

		  Fadd_spec_to_specifier (font_spec, val, frm, Qnil, Qnil);
		  update_frame_face_values (f);
		}
	    }
	  else if (EQ (prop, Qwidth))
	    {
	      CHECK_INT (val);
	      width = XINT (val);
	      width_specified_p = TRUE;
	    }
	  else if (EQ (prop, Qheight))
	    {
	      CHECK_INT (val);
	      height = XINT (val);
	      height_specified_p = TRUE;
	    }
	  else if (EQ (prop, Qleft))
	    {
	      CHECK_INT (val);
	      x = XINT (val);
	      x_specified_p = TRUE;
	    }
	  else if (EQ (prop, Qtop))
	    {
	      CHECK_INT (val);
	      y = XINT (val);
	      y_specified_p = TRUE;
	    }
	}
    }

  /* Now we've extracted the properties, apply them.
     Do not apply geometric properties during frame creation. This
     is excessive anyways, and this loses because WM_SIZE has not
     been sent yet, so frame width and height fields are not initialized.

     unfortunately WM_SIZE loses as well since the resize is only
     applied once and the first time WM_SIZE is applied not everything
     is initialised in the frame (toolbars for instance). enabling
     this always makes no visible difference and fixes a whole host of
     bugs (and is more consistent with X) so I am going to reenable it.
     --andyp */
  if ( FRAME_PIXWIDTH (f) && FRAME_PIXHEIGHT (f)
       && (width_specified_p || height_specified_p
	   || x_specified_p || y_specified_p))
    {
      XEMACS_RECT_WH dest = { x, y, width, height };

      mswindows_size_frame_internal (f, &dest);
    }
}

void
mswindows_size_frame_internal (struct frame* f, XEMACS_RECT_WH* dest)
{
  RECT rect, ws_rect;
  int pixel_width, pixel_height;
  int size_p = (dest->width >=0 || dest->height >=0);
  int move_p = (dest->top >=0 || dest->left >=0);
  char_to_real_pixel_size (f, dest->width, dest->height, &pixel_width,
			   &pixel_height);

  if (dest->width < 0)
    pixel_width = FRAME_PIXWIDTH (f);
  if (dest->height < 0)
    pixel_height = FRAME_PIXHEIGHT (f);

  GetWindowRect (FRAME_MSWINDOWS_HANDLE(f), &rect);
  if (dest->left < 0)
    dest->left = rect.left;
  if (dest->top < 0)
    dest->top = rect.top;

  rect.left = rect.top = 0;
  rect.right = pixel_width;
  rect.bottom = pixel_height;

  AdjustWindowRectEx (&rect,
		      GetWindowLong (FRAME_MSWINDOWS_HANDLE(f), GWL_STYLE),
		      GetMenu (FRAME_MSWINDOWS_HANDLE(f)) != NULL,
		      GetWindowLong (FRAME_MSWINDOWS_HANDLE(f), GWL_EXSTYLE));

  /* resize and move the window so that it fits in the workspace. This is
  not restrictive since this will happen later anyway in WM_SIZE.  We
  have to do this after adjusting the rect to account for menubar
  etc. */
  mswindows_get_workspace_coords (&ws_rect);
  pixel_width = rect.right - rect.left;
  pixel_height = rect.bottom - rect.top;
  if (pixel_width > ws_rect.right - ws_rect.left)
    {
      pixel_width = ws_rect.right - ws_rect.left;
      size_p=1;
    }
  if (pixel_height > ws_rect.bottom - ws_rect.top)
    {
      pixel_height = ws_rect.bottom - ws_rect.top;
      size_p=1;
    }

  /* adjust position so window is in workspace */
  if (dest->left + pixel_width > ws_rect.right)
    {
      dest->left = ws_rect.right - pixel_width;
      move_p=1;
    }
  if (dest->left < ws_rect.left)
    {
      dest->left = ws_rect.left;
      move_p=1;
    }

  if (dest->top + pixel_height > ws_rect.bottom)
    {
      dest->top = ws_rect.bottom - pixel_height;
      move_p=1;
    }
  if (dest->top < ws_rect.top)
    {
      dest->top = ws_rect.top;
      move_p=1;
    }

  if (IsIconic (FRAME_MSWINDOWS_HANDLE(f))
      || IsZoomed (FRAME_MSWINDOWS_HANDLE(f)))
    ShowWindow (FRAME_MSWINDOWS_HANDLE(f), SW_RESTORE);

  SetWindowPos (FRAME_MSWINDOWS_HANDLE(f), NULL,
		dest->left, dest->top, pixel_width, pixel_height,
		SWP_NOACTIVATE | SWP_NOZORDER | SWP_NOSENDCHANGING
		| (size_p ? 0 : SWP_NOSIZE)
		| (move_p ? 0 : SWP_NOMOVE));
}

static Lisp_Object
mswindows_get_frame_parent (struct frame *f)
{
  HWND hwnd = FRAME_MSWINDOWS_HANDLE(f);
  hwnd = GetParent (hwnd);
  if (hwnd)
    {
      Lisp_Object parent;
      VOID_TO_LISP (parent, GetWindowLong (hwnd, XWL_FRAMEOBJ));
      assert (FRAME_MSWINDOWS_P (XFRAME (parent)));
      return parent;
    }
  else
    return Qnil;
}

static void
mswindows_update_frame_external_traits (struct frame* frm, Lisp_Object name)
{
}

static int
mswindows_frame_size_fixed_p (struct frame *f)
{
  /* Frame size cannot change if the frame is maximized */
  return IsZoomed (FRAME_MSWINDOWS_HANDLE (f));
}

/*---------------------------------------------------------------------*/
/*-----                    PRINTER FRAME                          -----*/
/*---------------------------------------------------------------------*/

/*
 * With some driver/os combination (I discovered this with HP drivers
 * under W2K), DC geometry is reset upon StartDoc and EndPage
 * calls. This is called every time one of these calls is made.
 */
static void
apply_dc_geometry (struct frame* f)
{
  HDC hdc = DEVICE_MSPRINTER_HDC (XDEVICE (FRAME_DEVICE (f)));
  SetTextAlign (hdc, TA_BASELINE | TA_LEFT | TA_NOUPDATECP);
  SetViewportOrgEx (hdc, FRAME_MSPRINTER_PIXLEFT(f),
		    FRAME_MSPRINTER_PIXTOP(f), NULL);
}

void
msprinter_start_page (struct frame *f)
{
  if (!FRAME_MSPRINTER_PAGE_STARTED (f))
    {
      FRAME_MSPRINTER_PAGE_STARTED (f) = 1;
      StartPage (DEVICE_MSPRINTER_HDC (XDEVICE (FRAME_DEVICE (f))));
      apply_dc_geometry (f);
    }
}

static void
error_frame_unsizable (struct frame *f)
{
  Lisp_Object frame;
  XSETFRAME (frame, f);
  invalid_change ("Cannot resize frame (margins) after print job has started.",
		  frame);
}

static void
maybe_error_if_job_active (struct frame *f)
{
  if (FRAME_MSPRINTER_JOB_STARTED (f))
    error_frame_unsizable (f);
}

static void
msprinter_init_frame_1 (struct frame *f, Lisp_Object props)
{
  /* Make sure this is the only frame on device. Windows printer can
     handle only one job at a time. */
  if (!NILP (DEVICE_FRAME_LIST (XDEVICE (FRAME_DEVICE (f)))))
    invalid_operation ("Only one frame (print job) at a time is allowed on "
		       "this printer device", FRAME_DEVICE (f));

  f->frame_data = xnew_and_zero (struct msprinter_frame);

  FRAME_MSPRINTER_TOP_MARGIN (f) =
    mswindows_get_default_margin (Qtop_margin);
  FRAME_MSPRINTER_BOTTOM_MARGIN (f) =
    mswindows_get_default_margin (Qbottom_margin);
  FRAME_MSPRINTER_LEFT_MARGIN (f) =
    mswindows_get_default_margin (Qleft_margin);
  FRAME_MSPRINTER_RIGHT_MARGIN (f) =
    mswindows_get_default_margin (Qright_margin);

  /* Negative for "uinspecified" */
  FRAME_MSPRINTER_CHARWIDTH (f) = -1;
  FRAME_MSPRINTER_CHARHEIGHT (f) = -1;
}

static void
msprinter_init_frame_3 (struct frame *f)
{
  DOCINFO di;
  struct device *device = XDEVICE (FRAME_DEVICE (f));
  int frame_left, frame_top, frame_width, frame_height;
  
  /* DC might be recreated in msprinter_apply_devmode,
     so do not initialize until now */
  HDC hdc = DEVICE_MSPRINTER_HDC (device);
  int logpixelsx = GetDeviceCaps (hdc, LOGPIXELSX);
  int logpixelsy = GetDeviceCaps (hdc, LOGPIXELSY);
  int physicaloffsetx = GetDeviceCaps (hdc, PHYSICALOFFSETX);
  int physicaloffsety = GetDeviceCaps (hdc, PHYSICALOFFSETY);
  int physicalheight = GetDeviceCaps (hdc, PHYSICALHEIGHT);
  int physicalwidth = GetDeviceCaps (hdc, PHYSICALWIDTH);

  /* Compute geometry properties.
     Conversion is from TWIPS -> inches -> pixels. */
  frame_left = MulDiv (logpixelsx, FRAME_MSPRINTER_LEFT_MARGIN(f), 1440)
    - physicaloffsetx;

  if (FRAME_MSPRINTER_CHARWIDTH(f) > 0)
    {
      char_to_real_pixel_size (f, FRAME_MSPRINTER_CHARWIDTH(f), 0,
			       &frame_width, NULL);
      FRAME_MSPRINTER_RIGHT_MARGIN(f) =
	MulDiv (physicalwidth - (frame_left + frame_width), 1440,
		logpixelsx);
    }
  else
    frame_width = physicalwidth - frame_left
      - MulDiv (logpixelsx, FRAME_MSPRINTER_RIGHT_MARGIN(f), 1440)
      - physicaloffsetx;

  frame_top = MulDiv (logpixelsy, FRAME_MSPRINTER_TOP_MARGIN(f), 1440)
    - physicaloffsety;

  if (FRAME_MSPRINTER_CHARHEIGHT(f) > 0)
    {
      char_to_real_pixel_size (f, 0, FRAME_MSPRINTER_CHARHEIGHT(f),
			       NULL, &frame_height);

      FRAME_MSPRINTER_BOTTOM_MARGIN(f) =
	MulDiv (physicalheight - (frame_top + frame_height), 1440,
		logpixelsy);
    }
  else
    frame_height = physicalheight - frame_top
      - MulDiv (logpixelsy, FRAME_MSPRINTER_BOTTOM_MARGIN(f), 1440)
      - physicaloffsety;

  /* Geometry sanity checks */
  if (!frame_pixsize_valid_p (f, frame_width, frame_height))
    invalid_operation ("Area inside print margins has shrunk to naught",
		       STRINGP (f->name) ? f->name : Qunbound);

  if (frame_left < 0
      || frame_top < 0
      || frame_left + frame_width > GetDeviceCaps (hdc, HORZRES)
      || frame_top + frame_height > GetDeviceCaps (hdc, VERTRES))
    invalid_operation ("Print area is ouside of the printer's "
		       "hardware printable area",
		       STRINGP (f->name) ? f->name : Qunbound);

  /* Apply XEmacs frame geometry and layout windows */
  {
    int rows, columns;
    FRAME_PIXWIDTH(f) = frame_width;
    FRAME_PIXHEIGHT(f) = frame_height;
    pixel_to_char_size (f, frame_width, frame_height, &columns, &rows);
    change_frame_size (f, rows, columns, 0);
  }

  FRAME_MSPRINTER_PIXLEFT(f) = frame_left;
  FRAME_MSPRINTER_PIXTOP(f) = frame_top;

  /* Start print job */
  di.cbSize = sizeof (di);
  di.lpszDocName = (STRINGP(f->name)
		    ? (char*) XSTRING_DATA(f->name)
		    : "XEmacs print document");
  di.lpszOutput = NULL;
  di.lpszDatatype = NULL;
  di.fwType = 0;

  if (StartDoc (hdc, &di) <= 0)
    invalid_operation ("Cannot start print job",
		       STRINGP (f->name) ? f->name : Qunbound);

  apply_dc_geometry (f);

  /* Finish frame setup */
  FRAME_MSPRINTER_JOB_STARTED (f) = 1;
  FRAME_VISIBLE_P(f) = 0;
}

static void
msprinter_mark_frame (struct frame *f)
{
}

static void
msprinter_delete_frame (struct frame *f)
{
  if (f->frame_data)
    {
      HDC hdc = DEVICE_MSPRINTER_HDC (XDEVICE (FRAME_DEVICE (f)));
      if (FRAME_MSPRINTER_PAGE_STARTED (f))
	EndPage (hdc);
      if (FRAME_MSPRINTER_JOB_STARTED (f))
	EndDoc (hdc);
      xfree (f->frame_data);
    }

  f->frame_data = 0;
}

static Lisp_Object
msprinter_frame_property (struct frame *f, Lisp_Object property)
{
  if (EQ (Qleft_margin, property))
    return make_int (FRAME_MSPRINTER_LEFT_MARGIN(f));
  else if (EQ (Qtop_margin, property))
    return make_int (FRAME_MSPRINTER_TOP_MARGIN(f));
  if (EQ (Qright_margin, property))
    return make_int (FRAME_MSPRINTER_RIGHT_MARGIN(f));
  else if (EQ (Qbottom_margin, property))
    return make_int (FRAME_MSPRINTER_BOTTOM_MARGIN(f));
  else
    return Qunbound;
}

static int
msprinter_internal_frame_property_p (struct frame *f, Lisp_Object property)
{
  return (EQ (Qleft_margin, property) || EQ (Qtop_margin, property) ||
	  EQ (Qright_margin, property) || EQ (Qbottom_margin, property));
}

static Lisp_Object
msprinter_frame_properties (struct frame *f)
{
  Lisp_Object props = Qnil;
  props = cons3 (Qbottom_margin,
		 make_int (FRAME_MSPRINTER_BOTTOM_MARGIN(f)), props);
  props = cons3 (Qright_margin,
		 make_int (FRAME_MSPRINTER_RIGHT_MARGIN(f)), props);
  props = cons3 (Qtop_margin,
		 make_int (FRAME_MSPRINTER_TOP_MARGIN(f)), props);
  props = cons3 (Qleft_margin,
		 make_int (FRAME_MSPRINTER_LEFT_MARGIN(f)), props);
  return props;
}

static void
msprinter_set_frame_properties (struct frame *f, Lisp_Object plist)
{
  Lisp_Object tail;

  /* Extract the properties from plist */
  for (tail = plist; !NILP (tail); tail = Fcdr (Fcdr (tail)))
    {
      Lisp_Object prop = Fcar (tail);
      Lisp_Object val = Fcar (Fcdr (tail));

      if (SYMBOLP (prop))
	{
	  if (EQ (prop, Qwidth))
	    {
	      maybe_error_if_job_active (f);
	      if (!NILP (val))
		{
		  CHECK_NATNUM (val);
		  FRAME_MSPRINTER_CHARWIDTH(f) = XINT (val);
		}
	    }
	  if (EQ (prop, Qheight))
	    {
	      maybe_error_if_job_active (f);
	      if (!NILP (val))
		{
		  CHECK_NATNUM (val);
		  FRAME_MSPRINTER_CHARHEIGHT(f) = XINT (val);
		}
	    }
	  else if (EQ (prop, Qleft_margin))
	    {
	      maybe_error_if_job_active (f);
	      CHECK_NATNUM (val);
	      FRAME_MSPRINTER_LEFT_MARGIN(f) = XINT (val);
	    }
	  else if (EQ (prop, Qtop_margin))
	    {
	      maybe_error_if_job_active (f);
	      CHECK_NATNUM (val);
	      FRAME_MSPRINTER_TOP_MARGIN(f) = XINT (val);
	    }
	  else if (EQ (prop, Qright_margin))
	    {
	      maybe_error_if_job_active (f);
	      CHECK_NATNUM (val);
	      FRAME_MSPRINTER_RIGHT_MARGIN(f) = XINT (val);
	    }
	  else if (EQ (prop, Qbottom_margin))
	    {
	      maybe_error_if_job_active (f);
	      CHECK_NATNUM (val);
	      FRAME_MSPRINTER_BOTTOM_MARGIN(f) = XINT (val);
	    }
	}
    }
}

static void
msprinter_set_frame_size (struct frame *f, int width, int height)
{
  /* We're absolutely unsizeable */
  error_frame_unsizable (f);
}

static void
msprinter_eject_page (struct frame *f)
{
  /* #### Should we eject empty pages? */
  if (FRAME_MSPRINTER_PAGE_STARTED (f))
    {
      FRAME_MSPRINTER_PAGE_STARTED (f) = 0;
      EndPage (DEVICE_MSPRINTER_HDC (XDEVICE (FRAME_DEVICE (f))));
      apply_dc_geometry (f);
    }
}


void
console_type_create_frame_mswindows (void)
{
  /* Display frames */
  CONSOLE_HAS_METHOD (mswindows, init_frame_1);
  CONSOLE_HAS_METHOD (mswindows, init_frame_2);
  CONSOLE_HAS_METHOD (mswindows, init_frame_3);
  CONSOLE_HAS_METHOD (mswindows, after_init_frame);
  CONSOLE_HAS_METHOD (mswindows, mark_frame);
  CONSOLE_HAS_METHOD (mswindows, focus_on_frame);
  CONSOLE_HAS_METHOD (mswindows, delete_frame);
  CONSOLE_HAS_METHOD (mswindows, get_mouse_position);
  CONSOLE_HAS_METHOD (mswindows, set_mouse_position);
  CONSOLE_HAS_METHOD (mswindows, raise_frame);
  CONSOLE_HAS_METHOD (mswindows, lower_frame);
  CONSOLE_HAS_METHOD (mswindows, enable_frame);
  CONSOLE_HAS_METHOD (mswindows, disable_frame);
  CONSOLE_HAS_METHOD (mswindows, make_frame_visible);
  CONSOLE_HAS_METHOD (mswindows, make_frame_invisible);
  CONSOLE_HAS_METHOD (mswindows, iconify_frame);
  CONSOLE_HAS_METHOD (mswindows, set_frame_size);
  CONSOLE_HAS_METHOD (mswindows, set_frame_position);
  CONSOLE_HAS_METHOD (mswindows, frame_property);
  CONSOLE_HAS_METHOD (mswindows, internal_frame_property_p);
  CONSOLE_HAS_METHOD (mswindows, frame_properties);
  CONSOLE_HAS_METHOD (mswindows, set_frame_properties);
  CONSOLE_HAS_METHOD (mswindows, set_title_from_bufbyte);
/*  CONSOLE_HAS_METHOD (mswindows, set_icon_name_from_bufbyte); */
  CONSOLE_HAS_METHOD (mswindows, frame_visible_p);
  CONSOLE_HAS_METHOD (mswindows, frame_totally_visible_p);
  CONSOLE_HAS_METHOD (mswindows, frame_iconified_p);
  CONSOLE_HAS_METHOD (mswindows, set_frame_pointer);
  CONSOLE_HAS_METHOD (mswindows, set_frame_icon);
  CONSOLE_HAS_METHOD (mswindows, get_frame_parent);
  CONSOLE_HAS_METHOD (mswindows, update_frame_external_traits);
  CONSOLE_HAS_METHOD (mswindows, frame_size_fixed_p);

  /* Printer frames, aka print jobs */
  CONSOLE_HAS_METHOD (msprinter, init_frame_1);
  CONSOLE_HAS_METHOD (msprinter, init_frame_3);
  CONSOLE_HAS_METHOD (msprinter, mark_frame);
  CONSOLE_HAS_METHOD (msprinter, delete_frame);
  CONSOLE_HAS_METHOD (msprinter, frame_property);
  CONSOLE_HAS_METHOD (msprinter, internal_frame_property_p);
  CONSOLE_HAS_METHOD (msprinter, frame_properties);
  CONSOLE_HAS_METHOD (msprinter, set_frame_properties);
  CONSOLE_HAS_METHOD (msprinter, set_frame_size);
  CONSOLE_HAS_METHOD (msprinter, eject_page);
}

void
syms_of_frame_mswindows (void)
{
}

void
reinit_vars_of_frame_mswindows (void)
{
  /* Needn't staticpro -- see comment above.  */
  Vmswindows_frame_being_created = Qnil;
}

void
vars_of_frame_mswindows (void)
{
  reinit_vars_of_frame_mswindows ();

  DEFVAR_LISP ("mswindows-use-system-frame-size-defaults", &Vmswindows_use_system_frame_size_defaults /*
Controls whether to use system or XEmacs defaults for frame size.
If nil then reasonable defaults are used for initial frame sizes. If t
then the system will choose default sizes for the frame.
*/ );
  Vmswindows_use_system_frame_size_defaults = Qnil;

  DEFVAR_LISP ("default-mswindows-frame-plist", &Vdefault_mswindows_frame_plist /*
Plist of default frame-creation properties for mswindows frames.
These override what is specified in `default-frame-plist', but are
overridden by the arguments to the particular call to `make-frame'.

Note: In many cases, properties of a frame are available as specifiers
instead of through the frame-properties mechanism.

Here is a list of recognized frame properties, other than those
documented in `set-frame-properties' (they can be queried and
set at any time, except as otherwise noted):

  initially-unmapped		If non-nil, the frame will not be visible
				when it is created.  In this case, you
				need to call `make-frame-visible' to make
				the frame appear.
  popup				If non-nil, it should be a frame, and this
				frame will be created as a "popup" frame
				whose parent is the given frame.  This
				will make the window manager treat the
				frame as a dialog box, which may entail
				doing different things (e.g. not asking
				for positioning, and not iconifying
				separate from its parent).
  top				Y position (in pixels) of the upper-left
				outermost corner of the frame (i.e. the
				upper-left of the window-manager
				decorations).
  left				X position (in pixels) of the upper-left
				outermost corner of the frame (i.e. the
				upper-left of the window-manager
				decorations).

See also `default-frame-plist', which specifies properties which apply
to all frames, not just mswindows frames.
*/ );
  Vdefault_mswindows_frame_plist = Qnil;

  mswindows_console_methods->device_specific_frame_props =
    &Vdefault_mswindows_frame_plist;

  DEFVAR_LISP ("default-msprinter-frame-plist", &Vdefault_msprinter_frame_plist /*
Plist of default frame-creation properties for msprinter print job frames.
These override what is specified in `default-frame-plist', but are
overridden by the arguments to the particular call to `make-frame'.

Note: In many cases, properties of a frame are available as specifiers
instead of through the frame-properties mechanism.

Here is a list of recognized frame properties, other than those
documented in `set-frame-properties' (they can be queried and
set at any time, except as otherwise noted):

  left-margin                   Margin of the page, in twips. Twip is a
  top-margin			typographical unit of measurement,
  right-margin                  equal to 1/1440 of an inch, or 1/20 of a
  bottom-margin			point, and roughly equal to 7/400 of a
				millimeter.  If not specified, the left
				and right margins default to 1 inch
				(25.4 mm) and the top and bottom margins
				to 0.5 inch (12.7 mm).

     MARGINS NOTE. right-margin and bottom-margin are overridden by
       the height and width properties. If you want to specify size
       of the printable area in character, as with the rest of XEmacs,
       use these properties. If height and/or width are nil, then
       corresponding margin setting is taken into account. If you
       specify height and/or width in `default-frame-plist', but still
       want to specify right/bottom margins, set height/width in this
       plist to nil, as in this example:

	  (setq default-frame-plist '(height 55 width 80)
		default-msprinter-frame-plist '(height nil width nil))

See also `default-frame-plist', which specifies properties which apply
to all frames, not just mswindows frames.
*/ );
  Vdefault_msprinter_frame_plist = Qnil;

  msprinter_console_methods->device_specific_frame_props =
    &Vdefault_msprinter_frame_plist;
}
