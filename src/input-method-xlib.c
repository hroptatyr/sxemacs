/* Various functions for X11R5+ input methods, using the Xlib interface.
   Copyright (C) 1996 Sun Microsystems.

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

/* Written by Martin Buchholz. */

/* This file implements an interface to X input methods, available
   with X11R5 and above.  See O'Reilly, Xlib programmer's guide,
   and X11 R6 release guide chapters on internationalized input,
   for further details */

/*
  Policy:

  The XIM is of the device, by the device, for the device.
  The XIC is of each frame, by each frame, for each frame.
  The exceptions are:
      1.  Activate XICs on poor frames when the XIM is back.
      2.  Deactivate all the XICs when the XIM goes down.

  Implementation:

    -  Register a callback for an XIM when the X device is being initialized.
       XIM_init_device (d) { XRegisterIMInstantiateCallback (); }
       The "XRegisterIMInstantiateCallback" is called when an XIM become
       available on the X display.

    -  Catch the XIC when the frame is being initialized if XIM was available.
       XIM_init_frame (f) { ... XCreateIC (); ... }

    -  Release the XIC when the frame is being closed.
       XIM_delete_frame (f) { ... FRAME_X_XIC (f) = NULL; ... }
       "XIM_delete_frame" is a "DestroyCallback" function declared in
       XIM_init_frame ();

    -  Release all the XICs when the XIM was down accidentally.
       In IMDestroyCallback:
           DEVICE_FRAME_LOOP (...) { FRAME_X_XIC (f) = NULL; }

    -  Re-enable XIC for all the frames which don't have XIC when the XIM
       is back.
       In IMInstantiateCallback:
           DEVICE_FRAME_LOOP (...) { XIM_init_frame (f); }


  Note:

    -  Currently, we don't use XDestroyIC because of _XimProtoCloseIM
       (internally registered as im->methods->close) does "Xfree (ic)".

 */

#include <config.h>
#include "lisp.h"
#include <X11/Xlocale.h>        /* More portable than <locale.h> ? */
#include <X11/Xlib.h>
#include "frame.h"
#include "device.h"
#include "window.h"
#include "buffer.h"
#include "console-x.h"
#include "EmacsFrame.h"
#include "events.h"

#if !defined (XIM_XLIB) && !defined (USE_XFONTSET)
#error  neither XIM_XLIB nor USE_XFONTSET is defined??
#endif

Lisp_Object Qxim_xlib;
#define xim_warn(str) warn_when_safe (Qxim_xlib, Qwarning, str);
#define xim_warn1(fmt, str) warn_when_safe (Qxim_xlib, Qwarning, fmt, str);
#define xim_info(str) warn_when_safe (Qxim_xlib, Qinfo, str);

#ifdef XIM_XLIB /* XIM_XLIB specific */
/* Get/Set IC values for just one attribute */
#ifdef DEBUG_XEMACS
#define XIC_Value(Get_Set, xic, name, attr, value)			\
do { 									\
  char *bad_arg;							\
  XVaNestedList list = XVaCreateNestedList (0, attr, value, NULL);	\
  if ((bad_arg = X##Get_Set##ICValues (xic, name, list, NULL)) != NULL)	\
    stderr_out ("X" #Get_Set "ICValues " "bad Arg: %s\n", bad_arg);	\
  XFree (list);								\
} while (0)
#else /* ! DEBUG_XEMACS */
#define XIC_Value(Get_Set, xic, name, attr, value)			\
do {									\
  XVaNestedList list = XVaCreateNestedList (0, attr, value, NULL);	\
  X##Get_Set##ICValues (xic, name, list, NULL);				\
  XFree (list);								\
} while (0)
#endif /* DEBUG_XEMACS */

static char DefaultXIMStyles[] =
"XIMPreeditPosition|XIMStatusArea\n"
"XIMPreeditPosition|XIMStatusNone\n"
"XIMPreeditPosition|XIMStatusNothing\n"
"XIMPreeditNothing|XIMStatusArea\n"
"XIMPreeditNothing|XIMStatusNothing\n"
"XIMPreeditNothing|XIMStatusNone\n"
"XIMPreeditNone|XIMStatusArea\n"
"XIMPreeditNone|XIMStatusNothing\n"
"XIMPreeditNone|XIMStatusNone";

static XIMStyle best_style (XIMStyles *user, XIMStyles *xim);
#endif /* XIM_XLIB only */

/* This function is documented, but no prototype in the header files */
EXTERN_C char * XSetIMValues(XIM, ...);

void
Initialize_Locale (void)
{
  char *locale;

  /* dverna - Nov. 98: #### DON'T DO THIS !!! The default XtLanguageProc
     routine calls setlocale(LC_ALL, lang) which fucks up our lower-level
     locale management, and especially the value of LC_NUMERIC. Anyway, since
     at this point, we don't know yet whether we're gonna need an X11 frame,
     we should really do it manually and not use Xlib's dumb default routine */
  /*XtSetLanguageProc (NULL, (XtLanguageProc) NULL, NULL);*/
  if ((locale = setlocale (LC_ALL, "")) == NULL)
    {
      xim_warn ("Can't set locale.\n"
		"Using C locale instead.\n");
      putenv ("LANG=C");
      putenv ("LC_ALL=C");
      if ((locale = setlocale (LC_ALL, "C")) == NULL)
	{
	  xim_warn ("Can't even set locale to `C'!\n");
	  return;
	}
    }

  if (!XSupportsLocale ())
    {
      xim_warn1 ("X Windows does not support locale `%s'\n"
		 "Using C Locale instead\n", locale);
      putenv ("LANG=C");
      putenv ("LC_ALL=C");
      if ((locale = setlocale (LC_ALL, "C")) == NULL)
	{
	  xim_warn ("Can't even set locale to `C'!\n");
	  return;
	}
      if (!XSupportsLocale ())
        {
          xim_warn ("X Windows does not even support locale `C'!\n");
          return;
        }
    }

  setlocale(LC_NUMERIC, "C");

  if (XSetLocaleModifiers ("") == NULL)
    {
      xim_warn ("XSetLocaleModifiers(\"\") failed\n"
		"Check the value of the XMODIFIERS environment variable.\n");
    }
}

#ifdef XIM_XLIB /* starting XIM specific codes */

/* Callbacks for IM are supported from X11R6 or later. */
#ifdef HAVE_XREGISTERIMINSTANTIATECALLBACK

static Boolean xim_initted = False;

/* Called from when XIM is destroying.
   Clear all the XIC when the XIM was destroying... */
static void
IMDestroyCallback (XIM im, XPointer client_data, XPointer call_data)
{
  struct device *d = (struct device *)client_data;
  Lisp_Object tail;

  DEVICE_FRAME_LOOP (tail, d)
    {
      struct frame *target_frame = XFRAME (XCAR (tail));
      if (FRAME_X_P (target_frame) && FRAME_X_XIC (target_frame))
	{
	  /* XDestroyIC (FRAME_X_XIC (target_frame)); */
	  FRAME_X_XIC (target_frame) = NULL;
	}
    }

  DEVICE_X_XIM (d) = NULL;
  xim_initted = False;
  return;
}

/* This is registered in XIM_init_device (when DEVICE is initializing).
   This activates XIM when XIM becomes available. */
static void
IMInstantiateCallback (Display *dpy, XPointer client_data, XPointer call_data)
{
  struct device *d = (struct device *)client_data;
  XIM xim;
  char *name, *class;
  XIMCallback ximcallback;
  Lisp_Object tail;

  /* if no xim is presented, initialize xim ... */
  if ( xim_initted == False )
    {
      xim_initted = True;
      XtGetApplicationNameAndClass (dpy, &name, &class);
      DEVICE_X_XIM (d) = xim = XOpenIM (dpy, XtDatabase (dpy), name, class);

      /* destroy callback for im */
      ximcallback.callback = (XIMProc) IMDestroyCallback;
      ximcallback.client_data = (XPointer) d;
      XSetIMValues (xim, XNDestroyCallback, &ximcallback, NULL);
    }

  /* activate XIC on all the X frames... */
  DEVICE_FRAME_LOOP (tail, d)
    {
      struct frame *target_frame = XFRAME (XCAR (tail));
      if (FRAME_X_P (target_frame) && !FRAME_X_XIC (target_frame))
	{
	  XIM_init_frame (target_frame);
	}
    }
  return;
}
#endif /* HAVE_XREGISTERIMINSTANTIATECALLBACK */

/* Initialize XIM for X device.
   Register the use of XIM using XRegisterIMInstantiateCallback. */
void
XIM_init_device (struct device *d)
{
#ifdef HAVE_XREGISTERIMINSTANTIATECALLBACK /* X11R6+ */
  DEVICE_X_XIM (d) = NULL;
  XRegisterIMInstantiateCallback (DEVICE_X_DISPLAY (d), NULL, NULL, NULL,
#ifdef XREGISTERIMINSTANTIATECALLBACK_NONSTANDARD_PROTOTYPE
				  /* The sixth parameter is of type
				     XPointer in XFree86 but (XPointer *)
				     on most other X11's. */
				  (XIDProc) IMInstantiateCallback,
				  (XPointer) d
#else /* X Consortium prototype */
				  (XIMProc) IMInstantiateCallback,
				  (XPointer *) d
#endif /* XREGISTERIMINSTANTIATECALLBACK_NONSTANDARD_PROTOTYPE */
				  );
  return;
#else /* pre-X11R6 */
  Display *dpy = DEVICE_X_DISPLAY (d);
  char *name, *class;
  XIM xim;

  XtGetApplicationNameAndClass (dpy, &name, &class);
  DEVICE_X_XIM (d) = xim = XOpenIM (dpy, XtDatabase (dpy), name, class);
  if (xim == NULL)
    {
      xim_warn ("XOpenIM() failed...no input server available\n");
      return;
    }
  else
    {
      XGetIMValues (xim, XNQueryInputStyle, &DEVICE_X_XIM_STYLES (d), NULL);
      return;
    }
#endif /* HAVE_XREGISTERIMINSTANTIATECALLBACK */
}


/*
 * For the frames
 */

/* Callback for the deleting frame. */
static void
XIM_delete_frame (Widget w, XtPointer client_data, XtPointer call_data)
{
  struct frame *f = (struct frame *) client_data;
  struct device *d = XDEVICE (FRAME_DEVICE (f));

  if (DEVICE_X_XIM (d))
    {
      if (FRAME_X_XIC (f))
	{
	  XDestroyIC (FRAME_X_XIC (f));
	  FRAME_X_XIC (f) = NULL;
	}
    }
  return;
}

/* Initialize XIC for new frame.
   Create an X input context (XIC) for this frame. */
void
XIM_init_frame (struct frame *f)
{
  struct device *d = XDEVICE (FRAME_DEVICE (f));
  XIM xim;
  Widget w = FRAME_X_TEXT_WIDGET (f);
  Window win = XtWindow (w);
  XRectangle p_area = {0,0,1,1}, s_area = {0,0,1,1};
  XPoint spot = {0,0};
  XIMStyle style;
  XVaNestedList p_list, s_list;
  typedef struct
  {
    XIMStyles styles;
    XFontSet  fontset;
    Pixel     fg;
    Pixel     bg;
    char      *inputmethod;
  } xic_vars_t;
  xic_vars_t xic_vars;
  XIC xic;

#define res(name, class, representation, field, default_value) \
  { name, class, representation, sizeof(xic_vars.field), \
     XtOffsetOf(xic_vars_t, field), XtRString, default_value }

  static XtResource resources[] =
  {
    /*  name              class          represent'n   field    default value */
    res(XtNximStyles,     XtCXimStyles,  XtRXimStyles, styles,  (XtPointer) DefaultXIMStyles),
    res(XtNfontSet,       XtCFontSet,    XtRFontSet,   fontset, (XtPointer) XtDefaultFontSet),
    res(XtNximForeground, XtCForeground, XtRPixel,     fg,      (XtPointer) XtDefaultForeground),
    res(XtNximBackground, XtCBackground, XtRPixel,     bg,      (XtPointer) XtDefaultBackground)
  };


  xim = DEVICE_X_XIM (d);

  if (!xim)
    {
      return;
    }

  w = FRAME_X_TEXT_WIDGET (f);

  /*
   * initialize XIC
   */
  if (FRAME_X_XIC (f)) return;
  XtGetApplicationResources (w, &xic_vars,
			     resources, XtNumber (resources),
			     NULL, 0);
  if (!xic_vars.fontset)
    {
      xim_warn ("Can't get fontset resource for Input Method\n");
      FRAME_X_XIC (f) = NULL;
      return;
    }

  /* construct xic */
  XGetIMValues (xim, XNQueryInputStyle, &DEVICE_X_XIM_STYLES(d), NULL);
  FRAME_X_XIC_STYLE (f) = style =
    best_style (&xic_vars.styles, (XIMStyles *)DEVICE_X_XIM_STYLES(d));

  p_list = XVaCreateNestedList (0,
				XNArea,         &p_area,
				XNSpotLocation, &spot,
				XNForeground,   xic_vars.fg,
				XNBackground,   xic_vars.bg,
				XNFontSet,      xic_vars.fontset,
				NULL);

  s_list = XVaCreateNestedList (0,
				XNArea,         &s_area,
				XNForeground,   xic_vars.fg,
				XNBackground,   xic_vars.bg,
				XNFontSet,      xic_vars.fontset,
				NULL);

  FRAME_X_XIC (f) = xic =
    XCreateIC (xim,
	       XNInputStyle, style,
	       XNClientWindow, win,
	       XNFocusWindow, win,
	       XNPreeditAttributes, p_list,
	       XNStatusAttributes, s_list,
	       NULL);
  XFree (p_list);
  XFree (s_list);

  if (!xic)
    {
      xim_warn ("Warning: XCreateIC failed.\n");
      return;
    }

  if (style & XIMPreeditPosition)
    {
      XPoint *frame_spot = &(FRAME_X_XIC_SPOT(f));
      frame_spot->x = frame_spot->y = -1;
    }

  XIM_SetGeometry (f);

  XSetICFocus (xic);

#ifdef HAVE_XREGISTERIMINSTANTIATECALLBACK
  /* when frame is going to be destroyed (closed) */
  XtAddCallback (FRAME_X_TEXT_WIDGET(f), XNDestroyCallback,
		 XIM_delete_frame, (XtPointer)f);
#endif
}


void
XIM_SetGeometry (struct frame *f)
{
  XIC      xic   = FRAME_X_XIC (f);
  XIMStyle style = FRAME_X_XIC_STYLE (f);
  XRectangle area;

  if (!xic || !f)
    return;

  if (style & XIMStatusArea)
    {
      /* Place Status Area in bottom right corner */
      /* Negotiate geometry of status area */
      /* See O'Reilly Xlib XIM chapter (but beware, it's buggy) */
      XRectangle *needed;

      /* If input method has existing status area, use its current size */
      /* The following at least works for Sun's htt */
      area.x = area.y = area.width = area.height = 0;
      XIC_Value (Set, xic, XNStatusAttributes, XNAreaNeeded, &area);
      XIC_Value (Get, xic, XNStatusAttributes, XNAreaNeeded, &needed);
      if (needed->width == 0)   /* Use XNArea instead of XNAreaNeeded */
        XIC_Value (Get, xic, XNStatusAttributes, XNArea, &needed);

      area.width  = needed->width;
      area.height = needed->height;
      area.x = FRAME_RIGHT_BORDER_START  (f) - area.width;
      area.y = FRAME_BOTTOM_BORDER_START (f) - area.height;

#ifdef DEBUG_XIM
      stderr_out ("Putting StatusArea in x=%d y=%d w=%d h=%d\n",
                  area.x, area.y, area.width, area.height);
#endif /* DEBUG_XIM */

      XIC_Value (Set, xic, XNStatusAttributes, XNArea, &area);
    }

  if (style & XIMPreeditPosition)
    {
      /* Set Preedit Area to whole frame size (sans border) */
      /* We include the border because Preedit window might be larger
         than display line at edge. #### FIX: we should adjust to make
         sure that there is always room for the spot sub-window */
      area.x      = FRAME_LEFT_BORDER_START (f);
      area.y      = FRAME_TOP_BORDER_START  (f);
      area.width  = FRAME_RIGHT_BORDER_END  (f) - area.x;
      area.height = FRAME_BOTTOM_BORDER_END (f) - area.y;
      XIC_Value(Set, xic, XNPreeditAttributes, XNArea, &area);
    }

#ifdef DEBUG_XIM
  describe_XIC (xic);
#endif
}

void
XIM_SetSpotLocation (struct frame *f, int x, int y)
{
  XIC xic = FRAME_X_XIC (f);
  XPoint *spot = &(FRAME_X_XIC_SPOT (f));

  /* Only care if we have a valid XIC using Over the Spot in
   * a different location */
  if (!xic ||
      !(FRAME_X_XIC_STYLE (f) & XIMPreeditPosition) ||
      (spot->x == (short) x &&
       spot->y == (short) y))
    return;

  spot->x = (short) x;
  spot->y = (short) y;

  /* #### FIX: Must make sure spot fits within Preedit Area */
  XIC_Value (Set, xic, XNPreeditAttributes, XNSpotLocation, spot);
#ifdef DEBUG_XIM
  stderr_out ("Spot: %d %d\n", spot->x, spot->y);
#endif
}

void
XIM_focus_event (struct frame *f, int in_p)
{
  if (FRAME_X_XIC (f) /* && FRAME_X_XIM_REGISTERED(f) */)
    (in_p ? XSetICFocus : XUnsetICFocus) (FRAME_X_XIC (f));
}

#if 0
#define XIM_Composed_Text_BUFSIZE 64
typedef struct XIM_Composed_Text
{
  int size;
  wchar_t data [XIM_Composed_Text_BUFSIZE];
} XIM_Composed_Text;

static XIM_Composed_Text composed_input_buf = {XIM_Composed_Text_BUFSIZE, {0}};
Window main_window;

/* get_XIM_input -- Process results of input method composition.

   This function copies the results of the input method composition to
   composed_input_buf.  Then for each character, a custom event of type
   wc_atom is sent with the character as its data.

   It is probably more efficient to copy the composition results to some
   allocated memory and send a single event pointing to that memory.
   That would cut down on the event processing as well as allow quick
   insertion into the buffer of the whole string.  It might require some
   care, though, to avoid fragmenting memory through the allocation and
   freeing of many small chunks.  Maybe the existing system for
   (single-byte) string allocation can be used, multiplying the length by
   sizeof (wchar_t) to get the right size.
*/
void
get_XIM_input (XKeyPressedEvent *x_key_event, XIC ic, Display *dpy)
{
  KeySym keysym;
  Status status;
  int len;
  int i;
  XClientMessageEvent new_event;

retry:
  len = XwcLookupString (ic, x_key_event, composed_input_buf.data,
			 composed_input_buf.size, &keysym, &status);
  switch (status)
    {
    case XBufferOverflow:
      /* GROW_WC_STRING (&composed_input_buf, 32); mrb */
      goto retry;
    case XLookupChars:
      break;
    default:
      abort ();
    }

  new_event.type = ClientMessage;
  new_event.display = x_key_event->display;
  new_event.window = x_key_event->window;
  new_event.message_type = wc_atom;
  new_event.format = 32;  /* 32-bit wide data */
  new_event.data.l[2] = new_event.data.l[3] = new_event.data.l[4] = 0L;
  new_event.data.l[0] = x_key_event->time;
  for (i = 0; i < len; i++)
    {
      new_event.data.l[1] = ((wchar_t *) composed_input_buf.data)[i];
      XSendEvent (display, main_window, False, 0L, (XEvent *) &new_event);
    }
}
#endif /* 0 */

/* ============================================================== */
/* X input method style determination */
/* ============================================================== */

#if 0
#define done(type, value)                \
  if (toVal->addr != NULL) {             \
    if (toVal->size < sizeof(type)) {    \
      toVal->size = sizeof(type);        \
      return False;                      \
    }                                    \
    *(type*)toVal->addr = (value);       \
  } else {                               \
    static type static_val;              \
    static_val = (value);                \
    toVal->addr = (XPointer)&static_val; \
  }                                      \
  toVal->size = sizeof(type);            \
  return True /* Caller supplies `;' */
#endif /* 0 */

/*
 * This is a standard Xt type converter, except that the caller MUST
 * supply a proper non-NULL toVal XIMStyles structure that we will
 * fill in.
 *
 * fromVal points to a string like
 *
 "XIMPreeditPosition|XIMStatusArea,
 XIMPreeditPosition|XIMStatusNothing
 XIMPreeditNothing|XIMStatusNothing"
 *
 * This is converted in the obvious way to a XIMStyles structure.
 *
 * mrb: #### Fix this to handle Motif-style specifications for
 * XIMStyles as well: overTheSpot, rootWindow, none */

/* XtTypeConverter */
Boolean
EmacsXtCvtStringToXIMStyles (
  Display     *dpy,
  XrmValuePtr  args,
  Cardinal    *num_args,
  XrmValuePtr  fromVal,
  XrmValuePtr  toVal,
  XtPointer   *converter_data)
{
#define STYLE_INFO(style) { style, #style, sizeof(#style) }
  static struct XIMStyleInfo
  {
    const XIMStyle style;
    const char   * const name;
    const int      namelen;
  } emacs_XIMStyleInfo[] = {
    STYLE_INFO (XIMPreeditPosition|XIMStatusArea),
    STYLE_INFO (XIMPreeditPosition|XIMStatusNothing),
    STYLE_INFO (XIMPreeditPosition|XIMStatusNone),
    STYLE_INFO (XIMPreeditNothing|XIMStatusArea),
    STYLE_INFO (XIMPreeditNothing|XIMStatusNothing),
    STYLE_INFO (XIMPreeditNothing|XIMStatusNone),
    STYLE_INFO (XIMPreeditNone|XIMStatusArea),
    STYLE_INFO (XIMPreeditNone|XIMStatusNothing),
    STYLE_INFO (XIMPreeditNone|XIMStatusNone)
  };
#undef STYLE_INFO

  char *s   = (char *) fromVal->addr;
  char *end = s + fromVal->size;
  XIMStyles * const p = (XIMStyles *) toVal->addr;
  const char * const delimiter = " \t\n\r:;," ;
  const int  max_styles = XtNumber(emacs_XIMStyleInfo);
  int i;
  char *c;

#ifdef DEBUG_XIM
  stderr_out ("EmacsCvtStringToXIMStyles called with size=%d, string=\"%s\"\n",
              fromVal->size, (char *) fromVal->addr);
#endif /* DEBUG_XIM */

  if (*num_args != 0)
    {
      XtAppContext the_app_con = XtDisplayToApplicationContext (dpy);
      XtAppWarningMsg(the_app_con, "wrongParameters", "cvtStringToXIMStyle",
                      "XtToolkitError",
                      "String to XIMStyle conversion requires exactly 0 parameters",
                      (String *)NULL, (Cardinal *)NULL);
      return False;
    }

#ifdef DEBUG_XEMACS
  /* Make sure caller is giving us good data */
  assert (fromVal->addr != NULL);
  assert (fromVal->size == strlen(fromVal->addr)+1);
  assert (toVal->addr   != NULL);
  assert (toVal->size   == sizeof(XIMStyles));
#endif /* DEBUG_XEMACS */

  p->count_styles = 0;
  p->supported_styles = xnew_array (XIMStyle, max_styles);

  /*
   * The following routine assumes that the style name resource is
   * identical with the programmatic name of style.  For example,
   * "XIMPreeditPosition|XIMStatusArea" means the
   * XIMPreeditPosition|XIMStatusArea value is specified.  If the
   * style name is changed, such as "OverTheSpot|imDisplaysInClient",
   * the parsing logic below should be modified as well. */

  if ((c = strtok(s, delimiter)) == NULL)
    c = end;

  while (c < end)
    {
      for(i=0 ; i<max_styles ; i++)
        {
          struct XIMStyleInfo *rec = emacs_XIMStyleInfo + i;
          if(!strncmp(c, rec->name, rec->namelen - 1)) {
            p->supported_styles[p->count_styles] = rec->style;
            p->count_styles++;
            break;
          }
        }
      if((c = strtok(NULL, delimiter)) == NULL) {
        break ;
      }
    }

  if (p->count_styles == 0)
    {   /* No valid styles? */
      char *buf = (char *)alloca (strlen (fromVal->addr)
				  + strlen (DefaultXIMStyles)
				  + 100);
      XrmValue new_from;
      XtAppContext the_app_con = XtDisplayToApplicationContext (dpy);

      sprintf(buf, "Cannot convert string \"%s\" to type XIMStyles.\n"
              "Using default string \"%s\" instead.\n",
              fromVal->addr, DefaultXIMStyles);
      XtAppWarningMsg(the_app_con, "wrongParameters", "cvtStringToXIMStyle",
                      "XtToolkitError",
                      buf, (String *)NULL, (Cardinal *)NULL);
      new_from.addr = DefaultXIMStyles;
      new_from.size = sizeof(DefaultXIMStyles);
      return EmacsXtCvtStringToXIMStyles (dpy, args, num_args,
                                          &new_from, toVal, converter_data);
    }
  XREALLOC_ARRAY (p->supported_styles, XIMStyle, p->count_styles);
  *converter_data = (char *) True;
  return True;
}

/* XtDestructor */
void
EmacsFreeXIMStyles (
  XtAppContext app,
  XrmValuePtr  toVal,
  XtPointer    converter_data,
  XrmValuePtr  args,
  Cardinal    *num_args)
{
#ifdef DEBUG_XIM
  stderr_out ("Converter data: %x\n", converter_data);
  stderr_out ("EmacsFreeXIMStyles called\n");
#endif /* DEBUG_XIM */

  if (*num_args != 0)
    {
      XtAppWarningMsg(app, "wrongParameters","freeXIMStyles","XtToolkitError",
                      "Freeing an XIMStyles requires that zero arguments be passwd",
                      (String *)NULL, (Cardinal *)NULL);
      return;
    }

  if (converter_data)
    {
      Boolean free_p    = (Boolean) (int) converter_data;
      XIMStyles *styles = (XIMStyles *) toVal->addr;
      if (free_p)
        XFree ( styles->supported_styles );
    }
}

#if 0
/* O'Reilly XLib Programming Manual, pg. 371 */
/* Much nicer implementation than O'Reilly */
/* Choose the more `complicated', hence nicer, XIM input style */
static XIMStyle
BetterStyle (XIMStyle s, XIMStyle t)
{
#define CHECK_XIMStyle_BIT(bit)  \
  if ((s ^ t) & bit) { return (s & bit) ? s : t; }

  CHECK_XIMStyle_BIT (XIMPreeditCallbacks);
  CHECK_XIMStyle_BIT (XIMPreeditPosition);
  CHECK_XIMStyle_BIT (XIMPreeditArea);
  CHECK_XIMStyle_BIT (XIMPreeditNothing);
  CHECK_XIMStyle_BIT (XIMStatusCallbacks);
  CHECK_XIMStyle_BIT (XIMStatusArea);
  CHECK_XIMStyle_BIT (XIMStatusNothing);
#undef CHECK_XIMStyle_BIT
  return s ? s : t ;
}
#endif /* 0 */

/* Choose the best style, given:
 * - user preferences (already checked to be supported by XEmacs)
 * - styles supported by the input method */
#define DEFAULTStyle  (XIMPreeditNothing|XIMStatusNothing)
static XIMStyle
best_style (XIMStyles *user, XIMStyles *xim)
{
  REGISTER int i, j;
  for (i=0 ; i<user->count_styles ; i++)
    {
      for (j=0 ; j<xim->count_styles ; j++)
        {
          if (user->supported_styles[i] == xim->supported_styles[j])
            return user->supported_styles[i];
        }
    }
  return DEFAULTStyle; /* Default Style */
}

/* These lisp-callable functions will be sealed until xim-leim is needed. 
   Oct 22 1999 - kazz */
#if 0
/*
 * External callable function for XIM
 */
DEFUN ("x-open-xim", Fx_open_xim, 1, 1, 0, /*
Open the XIC on the frame if XIM is available.
Commonly, use this as \(x-open-xim \(selected-frame)).
If the frame is not on X device, return signal.
If XIC is created successfully return t.  If not return nil.
*/
       (frame))
{
  struct frame *f;

  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);
  if (!FRAME_X_P (f))
    return signal_simple_error ("This frame is not on X device", frame);

  XIM_init_frame (f);
  return FRAME_X_XIC (f) ? Qt : Qnil;
}

DEFUN ("x-close-xim", Fx_close_xim, 1, 1, 0, /*
Close the XIC on the frame if it exists.
Commonly, use this as \(x-close-xim \(selected-frame)).
If the frame is not on X device, return signal.
Otherwise, it destroys the XIC if it exists, then returns t anyway.
*/
       (frame))
{
  struct frame *f;
  struct device *d;

  CHECK_LIVE_FRAME (frame);
  f = XFRAME (frame);
  if (!FRAME_X_P (f))
    return signal_simple_error ("This frame is not on X device", frame);

  d = XDEVICE (FRAME_DEVICE (f));
  if (DEVICE_X_XIM (d)) {
    /* XDestroyIC (FRAME_X_XIC (XFRAME (f))); */
    FRAME_X_XIC (XFRAME (f)) = NULL;
  }
  return Qt;
}
#endif /* if 0 */

void
syms_of_input_method_xlib (void)
{
  defsymbol (&Qxim_xlib, "xim-xlib");
#if 0 /* see above */
  DEFSUBR (Fx_open_xim);
  DEFSUBR (Fx_close_xim);
#endif
}

void
vars_of_input_method_xlib (void)
{
  Fprovide (intern ("xim"));
}


/* ====================================================================== */
/* Internal Debugging Routines */
/* ====================================================================== */
#ifdef DEBUG_XEMACS

void
describe_XIM (XIM xim)
{
  XIMStyles *styles;

  /* Print locale of XIM */
  stderr_out ("\nXIM Locale of IM: %s\n", XLocaleOfIM(xim));

  /* List supported input method styles */
  XGetIMValues(xim, XNQueryInputStyle, &styles, NULL);

  stderr_out ("\n%d input style(s) supported by input method.\n",
              styles->count_styles);

#ifdef DEBUG_XIM
  {
    int i;
    for (i=0; i < styles->count_styles; i++)
      describe_XIMStyle (styles->supported_styles[i]);
  }
#endif /* DEBUG_XIM */
  XFree(styles);
}

void
describe_XFontSet (XFontSet fontset)
{
  XFontStruct **font_struct_list;
  char **font_name_list;
  int count, i;

  if (fontset == NULL)
    {
      stderr_out ("NULL\n");
      return;
    }

  count = XFontsOfFontSet (fontset, &font_struct_list, &font_name_list);
  stderr_out ( "%d font(s) available:\n", count);
  for (i=0 ; i < count ; i++)
    stderr_out ("Font: %s\n", *(font_name_list+i));
}

void
describe_Status (Status status)
{
#define DESCRIBE_STATUS(value) \
  if (status == value) stderr_out ("Status: " #value "\n")

  DESCRIBE_STATUS (XBufferOverflow);
  DESCRIBE_STATUS (XLookupNone);
  DESCRIBE_STATUS (XLookupKeySym);
  DESCRIBE_STATUS (XLookupBoth);
  DESCRIBE_STATUS (XLookupChars);
#undef DESCRIBE_STATUS
}

void
describe_Window (Window win)
{
  char xwincmd[128];
  sprintf (xwincmd, "xwininfo -id 0x%x >&2; xwininfo -events -id 0x%x >&2",
           (int) win, (int) win);
  system (xwincmd);
}

void
describe_XIC (XIC xic)
{
  XIMStyle style;
  Window client_win=0, focus_win=0;
  char *resourceName  = NULL;
  char *resourceClass = NULL;
  char *bad_arg       = NULL;
  unsigned long filter_mask = NoEventMask;
  XVaNestedList p_list, s_list;
  XFontSet      p_fontset = NULL, s_fontset = NULL;
  Pixel         p_fg=0, p_bg = 0, s_fg=0, s_bg = 0;
  XRectangle   *p_area   = NULL, *s_area   = NULL;
  XRectangle   *p_needed = NULL, *s_needed = NULL;
  XPoint       *p_spot = NULL;

  /* Check for valid input context and method */
  if (!xic)
    stderr_out ("Input method is NULL\n");

  if (!XIMOfIC(xic))
    stderr_out ("XIMOfIC() returns NULL\n");

  /* Print out Input Context Attributes */
  p_list = XVaCreateNestedList (0,
                                XNFontSet,      &p_fontset,
                                XNArea,         &p_area,
                                XNAreaNeeded,   &p_needed,
                                XNSpotLocation, &p_spot,
                                XNForeground,   &p_fg,
                                XNBackground,   &p_bg,
                                NULL);

  s_list = XVaCreateNestedList (0,
                                XNFontSet,      &s_fontset,
                                XNArea,         &s_area,
                                XNAreaNeeded,   &s_needed,
                                XNForeground,   &s_fg,
                                XNBackground,   &s_bg,
                                NULL);

  bad_arg = XGetICValues(xic,
                         XNInputStyle,        &style,
                         XNFilterEvents,      &filter_mask,
                         XNClientWindow,      &client_win,
                         XNFocusWindow,       &focus_win,
                         XNResourceName,      &resourceName,
                         XNResourceClass,     &resourceClass,
                         XNPreeditAttributes, p_list,
                         XNStatusAttributes,  s_list,
                         NULL);
  XFree(p_list);
  XFree(s_list);

  if (bad_arg != NULL)
    stderr_out ("Couldn't get IC value: %s\n", bad_arg);

  stderr_out ("\nInput method context attributes:\n");
  stderr_out ("Style: "); describe_XIMStyle (style);
  stderr_out ("Client window: %lx\n", (unsigned long int)client_win);
  stderr_out ("Focus window: %lx\n",  (unsigned long int)focus_win);
  stderr_out ("Preedit:\n");
  describe_XRectangle ("  Area", p_area);
  describe_XRectangle ("  Area needed", p_needed);
  stderr_out ("  foreground: %lx\n", (unsigned long int)p_fg);
  stderr_out ("  background: %lx\n", (unsigned long int)p_bg);
  stderr_out ("  fontset: "); describe_XFontSet (p_fontset);
  stderr_out ("Status:\n");
  describe_XRectangle ("  Area", s_area);
  describe_XRectangle ("  Area needed", s_needed);
  stderr_out ("  foreground: %lx\n", (unsigned long int)s_fg);
  stderr_out ("  background: %lx\n", (unsigned long int)s_bg);
  stderr_out ("  fontset: \n"); describe_XFontSet (s_fontset);
  stderr_out ("XNResourceName: %s\n",  resourceName  ? resourceName  : "NULL");
  stderr_out ("XNResourceClass: %s\n", resourceClass ? resourceClass : "NULL");
  stderr_out ("XNFilterEvents: "); describe_event_mask (filter_mask);
}

void
describe_XRectangle (char *name, XRectangle *r)
{
  if (r == NULL)
    stderr_out ("%s: NULL\n", name);
  else
    stderr_out ("%s: x=%d y=%d w=%d h=%d\n",
                name, r->x, r->y, r->width, r->height);
}

/* Print out elements of Event mask */
/* Defines from X11/X.h */
void
describe_event_mask (unsigned long mask)
{
#define DESCRIBE_EVENT_MASK(bit) if ((bit) & mask) stderr_out (#bit " ")
  DESCRIBE_EVENT_MASK (NoEventMask);
  DESCRIBE_EVENT_MASK (KeyPressMask);
  DESCRIBE_EVENT_MASK (KeyReleaseMask);
  DESCRIBE_EVENT_MASK (ButtonPressMask);
  DESCRIBE_EVENT_MASK (ButtonReleaseMask);
  DESCRIBE_EVENT_MASK (EnterWindowMask);
  DESCRIBE_EVENT_MASK (LeaveWindowMask);
  DESCRIBE_EVENT_MASK (PointerMotionMask);
  DESCRIBE_EVENT_MASK (PointerMotionHintMask);
  DESCRIBE_EVENT_MASK (Button1MotionMask);
  DESCRIBE_EVENT_MASK (Button2MotionMask);
  DESCRIBE_EVENT_MASK (Button3MotionMask);
  DESCRIBE_EVENT_MASK (Button4MotionMask);
  DESCRIBE_EVENT_MASK (Button5MotionMask);
  DESCRIBE_EVENT_MASK (ButtonMotionMask);
  DESCRIBE_EVENT_MASK (KeymapStateMask);
  DESCRIBE_EVENT_MASK (ExposureMask);
  DESCRIBE_EVENT_MASK (VisibilityChangeMask);
  DESCRIBE_EVENT_MASK (StructureNotifyMask);
  DESCRIBE_EVENT_MASK (ResizeRedirectMask);
  DESCRIBE_EVENT_MASK (SubstructureNotifyMask);
  DESCRIBE_EVENT_MASK (SubstructureRedirectMask);
  DESCRIBE_EVENT_MASK (FocusChangeMask);
  DESCRIBE_EVENT_MASK (PropertyChangeMask);
  DESCRIBE_EVENT_MASK (ColormapChangeMask);
  DESCRIBE_EVENT_MASK (OwnerGrabButtonMask);
#undef DESCRIBE_EVENT_MASK
  stderr_out("\n");
}

void
describe_XIMStyle (XIMStyle style)
{
#define DESCRIBE_STYLE(bit) \
  if (bit & style)          \
    stderr_out (#bit " ");

  DESCRIBE_STYLE (XIMPreeditArea);
  DESCRIBE_STYLE (XIMPreeditCallbacks);
  DESCRIBE_STYLE (XIMPreeditPosition);
  DESCRIBE_STYLE (XIMPreeditNothing);
  DESCRIBE_STYLE (XIMPreeditNone);
  DESCRIBE_STYLE (XIMStatusArea);
  DESCRIBE_STYLE (XIMStatusCallbacks);
  DESCRIBE_STYLE (XIMStatusNothing);
  DESCRIBE_STYLE (XIMStatusNone);
#undef DESCRIBE_STYLE
  stderr_out("\n");
}

void
describe_XIMStyles (XIMStyles *p)
{
  int i;
  stderr_out ("%d Style(s):\n", p->count_styles);
  for (i=0; i<p->count_styles ; i++)
    {
      describe_XIMStyle (p->supported_styles[i]);
    }
}

#endif /* DEBUG_XEMACS */

/* Random cruft follows */

#if 0
static void
Unit_Test (struct frame *f, char * s)
/* mrb unit testing */
{
  XrmValue fromVal, toVal;

  fromVal.addr = s;
  fromVal.size = strlen (s);
  toVal.addr = (XtPointer) &user_preferred_XIMStyles;
  toVal.size = sizeof (XIMStyles);

  if (XtConvertAndStore (FRAME_X_TEXT_WIDGET (f), XtRString, &fromVal,
			 XtRXimStyles, &toVal) != False)
    {
      stderr_out ("Unit_Test: fromVal.addr=0x%x\n",fromVal.addr);
      stderr_out ("Unit_Test: fromVal.size=%d\n",  fromVal.size);
      stderr_out ("Unit_Test:   toVal.addr=0x%x\n",  toVal.addr);
      stderr_out ("Unit_Test:   toVal.size=%d\n",    toVal.size);
      describe_XIMStyles ((XIMStyles *) toVal.addr);
    }
}
#endif
#endif /* XIM_XLIB only */

#if 0
/* Get a fontset for IM to use */
void
x_init_fontset (struct device *d)
{
  Display *dpy = DEVICE_X_DISPLAY (d);
  XFontSet fontset;
  char ** missing_charsets;
  int num_missing_charsets;
  char * default_string;
  /*  char * font_set_string = "-dt-interface user-medium-r-normal-s*-*-*-*-*-*-*-*-*";*/
  char * font_set_string = "-dt-interface user-medium-r-normal-s*-*-*-*-*-*-*-*-*, -misc-fixed-medium-r-normal--14-130-75-75-c-70-jisx0201.1976-0,-misc-fixed-medium-r-normal--14-130-75-75-c-140-jisx0208.1983-0, -misc-fixed-medium-r-normal--14-130-75-75-c-70-jisx0201.1976-0" ;

  DEVICE_X_FONTSET (d) = fontset =
    XCreateFontSet (dpy,
		    font_set_string,
		    &missing_charsets,
		    &num_missing_charsets,
		    &default_string);

  if (fontset == NULL)
    {
      stderr_out ("Unable to create fontset from string:\n%s\n", font_set_string);
      return;
    }
  if (num_missing_charsets > 0)
    {
      int i;
      stderr_out ("\nMissing charsets for fontset %s:\n", font_set_string);
      for (i=0; i < num_missing_charsets; i++)
        {
          stderr_out ("%s\n", missing_charsets[i]);
        }
      XFreeStringList (missing_charsets);
      stderr_out ("Default string: %s\n", default_string);
    }

#ifdef DEBUG_XIM
  describe_XFontSet (fontset);
#endif
}
#endif /* 0 */
