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

/* 7-8-00 !!#### This file needs definite Mule review. */

/* Original authors: Jamie Zawinski and the FSF */
/* Rewritten by Ben Wing and Chuck Thompson. */

#include <config.h>
#include "lisp.h"

#include "console-x.h"
#include "xintrinsicp.h"	/* CoreP.h needs this */
#include <X11/CoreP.h>		/* Numerous places access the fields of
				   a core widget directly.  We could
				   use XtGetValues(), but ... */
#include "xgccache.h"
#include <X11/Shell.h>
#include "xmu.h"
#include "glyphs-x.h"
#include "objects-x.h"

#include "buffer.h"
#include "elhash.h"
#include "events.h"
#include "faces.h"
#include "frame.h"
#include "redisplay.h"
#include "sysdep.h"
#include "window.h"

#include "sysfile.h"
#include "systime.h"

#if defined(HAVE_SHLIB) && defined(LWLIB_USES_ATHENA) && !defined(HAVE_ATHENA_3D)
#include "sysdll.h"
#endif /* HAVE_SHLIB and LWLIB_USES_ATHENA and not HAVE_ATHENA_3D */

#ifdef HAVE_OFFIX_DND
#include "offix.h"
#endif

Lisp_Object Vdefault_x_device;
#if defined(MULE) && (defined(LWLIB_MENUBARS_MOTIF) || defined(HAVE_XIM) || defined (USE_XFONTSET))
Lisp_Object Vx_app_defaults_directory;
#endif

/* Qdisplay in general.c */
Lisp_Object Qx_error;
Lisp_Object Qinit_pre_x_win, Qinit_post_x_win;

/* The application class of Emacs. */
Lisp_Object Vx_emacs_application_class;

Lisp_Object Vx_initial_argv_list; /* #### ugh! */

static XrmOptionDescRec emacs_options[] =
{
  {"-geometry", ".geometry", XrmoptionSepArg, NULL},
  {"-iconic", ".iconic", XrmoptionNoArg, "yes"},

  {"-internal-border-width", "*EmacsFrame.internalBorderWidth", XrmoptionSepArg, NULL},
  {"-ib",                    "*EmacsFrame.internalBorderWidth", XrmoptionSepArg, NULL},
  {"-scrollbar-width",       "*EmacsFrame.scrollBarWidth",      XrmoptionSepArg, NULL},
  {"-scrollbar-height",      "*EmacsFrame.scrollBarHeight",     XrmoptionSepArg, NULL},

  {"-privatecolormap", ".privateColormap", XrmoptionNoArg,  "yes"},
  {"-visual",   ".EmacsVisual",	    XrmoptionSepArg, NULL},

  /* #### Beware!  If the type of the shell changes, update this. */
  {"-T",        "*TopLevelEmacsShell.title",    XrmoptionSepArg, NULL},
  {"-wn",       "*TopLevelEmacsShell.title",    XrmoptionSepArg, NULL},
  {"-title",    "*TopLevelEmacsShell.title",    XrmoptionSepArg, NULL},

  {"-iconname", "*TopLevelEmacsShell.iconName", XrmoptionSepArg, NULL},
  {"-in",       "*TopLevelEmacsShell.iconName", XrmoptionSepArg, NULL},
  {"-mc",       "*pointerColor",                XrmoptionSepArg, NULL},
  {"-cr",       "*cursorColor",                 XrmoptionSepArg, NULL},
  {"-fontset",  "*FontSet",                     XrmoptionSepArg, NULL},
};

/* Functions to synchronize mirroring resources and specifiers */
int in_resource_setting;

/************************************************************************/
/*                          helper functions                            */
/************************************************************************/

/* JH 97/11/25 removed the static declaration because I need it during setup in event-Xt... */
struct device * get_device_from_display_1 (Display *dpy);
struct device *
get_device_from_display_1 (Display *dpy)
{
  Lisp_Object devcons, concons;

  DEVICE_LOOP_NO_BREAK (devcons, concons)
    {
      struct device *d = XDEVICE (XCAR (devcons));
      if (DEVICE_X_P (d) && DEVICE_X_DISPLAY (d) == dpy)
	return d;
    }

  return 0;
}

struct device *
get_device_from_display (Display *dpy)
{
  struct device *d = get_device_from_display_1 (dpy);

#if !defined(INFODOCK)
# define FALLBACK_RESOURCE_NAME "xemacs"
# else
# define FALLBACK_RESOURCE_NAME "infodock"
#endif

  if (!d) {
    /* This isn't one of our displays.  Let's crash? */
    stderr_out
      ("\n%s: Fatal X Condition.  Asked about display we don't own: \"%s\"\n",
       (STRINGP (Vinvocation_name) ?
	(char *) XSTRING_DATA (Vinvocation_name) : FALLBACK_RESOURCE_NAME),
       DisplayString (dpy) ? DisplayString (dpy) : "???");
    abort();
  }

#undef FALLBACK_RESOURCE_NAME

  return d;
}

struct device *
decode_x_device (Lisp_Object device)
{
  XSETDEVICE (device, decode_device (device));
  CHECK_X_DEVICE (device);
  return XDEVICE (device);
}

static Display *
get_x_display (Lisp_Object device)
{
  return DEVICE_X_DISPLAY (decode_x_device (device));
}


/************************************************************************/
/*		      initializing an X connection			*/
/************************************************************************/

static struct device *device_being_initialized = NULL;

static void
allocate_x_device_struct (struct device *d)
{
  d->device_data = xnew_and_zero (struct x_device);
}

static void
Xatoms_of_device_x (struct device *d)
{
  Display *D = DEVICE_X_DISPLAY (d);

  DEVICE_XATOM_WM_PROTOCOLS    (d) = XInternAtom (D, "WM_PROTOCOLS",    False);
  DEVICE_XATOM_WM_DELETE_WINDOW(d) = XInternAtom (D, "WM_DELETE_WINDOW",False);
  DEVICE_XATOM_WM_SAVE_YOURSELF(d) = XInternAtom (D, "WM_SAVE_YOURSELF",False);
  DEVICE_XATOM_WM_TAKE_FOCUS   (d) = XInternAtom (D, "WM_TAKE_FOCUS",   False);
  DEVICE_XATOM_WM_STATE        (d) = XInternAtom (D, "WM_STATE",        False);
}

static void
sanity_check_geometry_resource (Display *dpy)
{
  char *app_name, *app_class, *s;
  char buf1 [255], buf2 [255];
  char *type;
  XrmValue value;
  XtGetApplicationNameAndClass (dpy, &app_name, &app_class);
  strcpy (buf1, app_name);
  strcpy (buf2, app_class);
  for (s = buf1; *s; s++) if (*s == '.') *s = '_';
  strcat (buf1, "._no_._such_._resource_.geometry");
  strcat (buf2, "._no_._such_._resource_.Geometry");
  if (XrmGetResource (XtDatabase (dpy), buf1, buf2, &type, &value) == True)
    {
      warn_when_safe (Qgeometry, Qerror,
		      "\n"
"Apparently \"%s*geometry: %s\" or \"%s*geometry: %s\" was\n"
"specified in the resource database.  Specifying \"*geometry\" will make\n"
"XEmacs (and most other X programs) malfunction in obscure ways. (i.e.\n"
"the Xt or Xm libraries will probably crash, which is a very bad thing.)\n"
"You should always use \".geometry\" or \"*EmacsFrame.geometry\" instead.\n",
		  app_name, (char *) value.addr,
		  app_class, (char *) value.addr);
      suppress_early_error_handler_backtrace = 1;
      error ("Invalid geometry resource");
    }
}

static void
x_init_device_class (struct device *d)
{
  if (DEVICE_X_DEPTH(d) > 2)
    {
      switch (DEVICE_X_VISUAL(d)->class)
	{
	case StaticGray:
	case GrayScale:
	  DEVICE_CLASS (d) = Qgrayscale;
	  break;
	default:
	  DEVICE_CLASS (d) = Qcolor;
	}
    }
  else
    DEVICE_CLASS (d) = Qmono;
}

/*
 * Figure out what application name to use for xemacs
 *
 * Since we have decomposed XtOpenDisplay into XOpenDisplay and
 * XtDisplayInitialize, we no longer get this for free.
 *
 * If there is a `-name' argument in argv, use that.
 * Otherwise use the last component of argv[0].
 *
 * I have removed the gratuitous use of getenv("RESOURCE_NAME")
 * which was in X11R5, but left the matching of any prefix of `-name'.
 * Finally, if all else fails, return `xemacs', as it is more
 * appropriate (X11R5 returns `main').
 */
static Extbyte *
compute_x_app_name (int argc, Extbyte **argv)
{
  int i;
  Extbyte *ptr;

  for (i = 1; i < argc - 1; i++)
    if (!strncmp(argv[i], "-name", max (2, strlen (argv[1]))))
      return argv[i+1];

  if (argc > 0 && argv[0] && *argv[0])
    return (ptr = strrchr (argv[0], '/')) ? ++ptr : argv[0];

  return "xemacs";
}

/*
 * This function figures out whether the user has any resources of the
 * form "XEmacs.foo" or "XEmacs*foo".
 *
 * Currently we only consult the display's global resources; to look
 * for screen specific resources, we would need to also consult:
 * xdefs = XScreenResourceString(ScreenOfDisplay(dpy, scrno));
 */
static int
have_xemacs_resources_in_xrdb (Display *dpy)
{
  char *xdefs, *key;
  int len;

#ifdef INFODOCK
  key = "InfoDock";
#else
  key = "XEmacs";
#endif
  len = strlen (key);

  if (!dpy)
    return 0;

  xdefs = XResourceManagerString (dpy);      /* don't free - owned by X */
  while (xdefs && *xdefs)
    {
      if (strncmp (xdefs, key, len) == 0  &&
          (xdefs[len] == '*' || xdefs[len] == '.'))
        return 1;

      while (*xdefs && *xdefs++ != '\n')     /* find start of next entry.. */
        ;
    }

  return 0;
}

/* Only the characters [-_A-Za-z0-9] are allowed in the individual
   components of a resource.  Convert invalid characters to `-' */

static char valid_resource_char_p[256];

static void
validify_resource_component (char *str, size_t len)
{
  for (; len; len--, str++)
    if (!valid_resource_char_p[(unsigned char) (*str)])
      *str = '-';
}

static void
Dynarr_add_validified_lisp_string (char_dynarr *cda, Lisp_Object str)
{
  Bytecount len = XSTRING_LENGTH (str);
  Dynarr_add_many (cda, (char *) XSTRING_DATA (str), len);
  validify_resource_component (Dynarr_atp (cda, Dynarr_length (cda) - len), len);
}

#if 0
/* compare visual info for qsorting */
static int
x_comp_visual_info (const void *elem1, const void *elem2)
{
  XVisualInfo *left, *right;

  left = (XVisualInfo *)elem1;
  right = (XVisualInfo *)elem2;

  if ( left == NULL )
    return -1;
  if ( right == NULL )
    return 1;

  if ( left->depth > right->depth ) {
    return 1;
  }
  else if ( left->depth == right->depth ) {
    if ( left->colormap_size > right->colormap_size )
      return 1;
    if ( left->class > right->class )
      return 1;
    else if ( left->class < right->class )
      return -1;
    else
      return 0;
  }
  else {
    return -1;
  }

}
#endif /* if 0 */

#define XXX_IMAGE_LIBRARY_IS_SOMEWHAT_BROKEN
static Visual *
x_try_best_visual_class (Screen *screen, int scrnum, int visual_class)
{
  Display *dpy = DisplayOfScreen (screen);
  XVisualInfo vi_in;
  XVisualInfo *vi_out = NULL;
  int out_count;

  vi_in.class = visual_class;
  vi_in.screen = scrnum;
  vi_out = XGetVisualInfo (dpy, (VisualClassMask | VisualScreenMask),
			   &vi_in, &out_count);
  if ( vi_out )
    {
      int i, best;
      Visual *visual;
      for (i = 0, best = 0; i < out_count; i++)
	/* It's better if it's deeper, or if it's the same depth with
	   more cells (does that ever happen?  Well, it could...)
	   NOTE: don't allow pseudo color to get larger than 8! */
	if (((vi_out [i].depth > vi_out [best].depth) ||
	     ((vi_out [i].depth == vi_out [best].depth) &&
	      (vi_out [i].colormap_size > vi_out [best].colormap_size)))
#ifdef XXX_IMAGE_LIBRARY_IS_SOMEWHAT_BROKEN
	    /* For now, the image library doesn't like PseudoColor visuals
	       of depths other than 1 or 8.  Depths greater than 8 only occur
	       on machines which have TrueColor anyway, so probably we'll end
	       up using that (it is the one that `Best' would pick) but if a
	       PseudoColor visual is explicitly specified, pick the 8 bit one.
	    */
	    && (visual_class != PseudoColor ||
		vi_out [i].depth == 1 ||
		vi_out [i].depth == 8)
#endif

	    /* SGI has 30-bit deep visuals.  Ignore them.
                (We only have 24-bit data anyway.)
              */
	    && (vi_out [i].depth <= 24)
	    )
	  best = i;
      visual = vi_out[best].visual;
      XFree ((char *) vi_out);
      return visual;
    }
  else
    return 0;
}

static int
x_get_visual_depth (Display *dpy, Visual *visual)
{
  XVisualInfo vi_in;
  XVisualInfo *vi_out;
  int out_count, d;

  vi_in.visualid = XVisualIDFromVisual (visual);
  vi_out = XGetVisualInfo (dpy, /*VisualScreenMask|*/VisualIDMask,
			   &vi_in, &out_count);
  if (! vi_out) abort ();
  d = vi_out [0].depth;
  XFree ((char *) vi_out);
  return d;
}

static Visual *
x_try_best_visual (Display *dpy, int scrnum)
{
  Visual *visual = NULL;
  Screen *screen = ScreenOfDisplay (dpy, scrnum);
  if ((visual = x_try_best_visual_class (screen, scrnum, TrueColor))
      && x_get_visual_depth (dpy, visual) >= 16 )
    return visual;
  if ((visual = x_try_best_visual_class (screen, scrnum, PseudoColor)))
    return visual;
  if ((visual = x_try_best_visual_class (screen, scrnum, TrueColor)))
    return visual;
#ifdef DIRECTCOLOR_WORKS
  if ((visual = x_try_best_visual_class (screen, scrnum, DirectColor)))
    return visual;
#endif

  visual = DefaultVisualOfScreen (screen);
  if ( x_get_visual_depth (dpy, visual) >= 8 )
    return visual;

  if ((visual = x_try_best_visual_class (screen, scrnum, StaticGray)))
    return visual;
  if ((visual = x_try_best_visual_class (screen, scrnum, GrayScale)))
    return visual;
  return DefaultVisualOfScreen (screen);
}


static void
x_init_device (struct device *d, Lisp_Object props)
{
  Lisp_Object display;
  Lisp_Object device;
  Display *dpy;
  Widget app_shell;
  int argc;
  Extbyte **argv;
  const char *app_class;
  const char *app_name;
  const char *disp_name;
  Visual *visual = NULL;
  int depth = 8;		/* shut up the compiler */
  Colormap cmap;
  int screen;
  /* */
  int best_visual_found = 0;

#if defined(HAVE_SHLIB) && defined(LWLIB_USES_ATHENA) && !defined(HAVE_ATHENA_3D)
  /*
   * In order to avoid the lossage with flat Athena widgets dynamically
   * linking to one of the ThreeD variants, using the dynamic symbol helpers
   * to look for symbols that shouldn't be there and refusing to run if they
   * are seems a less toxic idea than having XEmacs crash when we try and
   * use a subclass of a widget that has changed size.
   *
   * It's ugly, I know, and not going to work everywhere. It seems better to
   * do our damnedest to try and tell the user what to expect rather than
   * simply blow up though.
   *
   * All the ThreeD variants I have access to define the following function
   * symbols in the shared library. The flat Xaw library does not define them:
   *
   * Xaw3dComputeBottomShadowRGB
   * Xaw3dComputeTopShadowRGB
   *
   * So far only Linux has shown this problem. This seems to be portable to
   * all the distributions (certainly all the ones I checked - Debian and
   * Redhat)
   *
   * This will only work, sadly, with dlopen() -- the other dynamic linkers
   * are simply not capable of doing what is needed. :/
   */

  {
    /* Get a dll handle to the main process. */
    dll_handle xaw_dll_handle = dll_open (NULL);

    /* Did that fail?  If so, continue without error.
     * We could die here but, well, that's unfriendly and all -- plus I feel
     * better about some crashing somewhere rather than preventing a perfectly
     * good configuration working just because dll_open failed.
     */
    if (xaw_dll_handle != NULL)
      {
	/* Look for the Xaw3d function */
	dll_func xaw_function_handle =
	  dll_function (xaw_dll_handle, "Xaw3dComputeTopShadowRGB");

	/* If we found it, warn the user in big, nasty, unfriendly letters */
	if (xaw_function_handle != NULL)
	  {
	    warn_when_safe (Qdevice, Qerror, "\n"
"It seems that XEmacs is built dynamically linked to the flat Athena widget\n"
"library but it finds a 3D Athena variant with the same name at runtime.\n"
"\n"
"This WILL cause your XEmacs process to dump core at some point.\n"
"You should not continue to use this binary without resolving this issue.\n"
"\n"
"This can be solved with the xaw-wrappers package under Debian\n"
"(register XEmacs as incompatible with all 3d widget sets, see\n"
"update-xaw-wrappers(8) and .../doc/xaw-wrappers/README.packagers).  It\n"
"can be verified by checking the runtime path in /etc/ld.so.conf and by\n"
"using `ldd /path/to/xemacs' under other Linux distributions.  One\n"
"solution is to use LD_PRELOAD or LD_LIBRARY_PATH to force ld.so to\n"
"load the flat Athena widget library instead of the aliased 3D widget\n"
"library (see ld.so(8) for use of these environment variables).\n\n"
			    );

	  }

	/* Otherwise release the handle to the library
	 * No error catch here; I can't think of a way to recover anyhow.
	 */
	dll_close (xaw_dll_handle);
      }
  }
#endif /* HAVE_SHLIB and LWLIB_USES_ATHENA and not HAVE_ATHENA_3D */


  XSETDEVICE (device, d);
  display = DEVICE_CONNECTION (d);

  allocate_x_device_struct (d);

  make_argc_argv (Vx_initial_argv_list, &argc, &argv);

  LISP_STRING_TO_EXTERNAL (display, disp_name, Qctext);

  /*
   * Break apart the old XtOpenDisplay call into XOpenDisplay and
   * XtDisplayInitialize so we can figure out whether there
   * are any XEmacs resources in the resource database before
   * we initialize Xt.  This is so we can automagically support
   * both `Emacs' and `XEmacs' application classes.
   */
  slow_down_interrupts ();
  /* May not be needed but XtOpenDisplay could not deal with signals here. */
  device_being_initialized = d;
  dpy = DEVICE_X_DISPLAY (d) = XOpenDisplay (disp_name);
  device_being_initialized = NULL;
  speed_up_interrupts ();

  if (dpy == 0)
    {
      suppress_early_error_handler_backtrace = 1;
      signal_simple_error ("X server not responding\n", display);
    }

  if (STRINGP (Vx_emacs_application_class) &&
      XSTRING_LENGTH (Vx_emacs_application_class) > 0)
    LISP_STRING_TO_EXTERNAL (Vx_emacs_application_class, app_class, Qctext);
  else
    {
      app_class = (NILP (Vx_emacs_application_class)  &&
                   have_xemacs_resources_in_xrdb (dpy))
#ifdef INFODOCK
                  ? "InfoDock"
#else
                  ? "XEmacs"
#endif
                  : "Emacs";
      /* need to update Vx_emacs_application_class: */
      Vx_emacs_application_class = build_string (app_class);
    }

  slow_down_interrupts ();
  /* May not be needed but XtOpenDisplay could not deal with signals here.
     Yuck. */
  XtDisplayInitialize (Xt_app_con, dpy, compute_x_app_name (argc, argv),
                       app_class, emacs_options,
                       XtNumber (emacs_options), &argc, (char **) argv);
  speed_up_interrupts ();

  screen = DefaultScreen (dpy);
  if (NILP (Vdefault_x_device))
    Vdefault_x_device = device;

#ifdef MULE
#if defined(LWLIB_MENUBARS_MOTIF) || defined(HAVE_XIM) || defined (USE_XFONTSET)
  {
    /* Read in locale-specific resources from
       data-directory/app-defaults/$LANG/Emacs.
       This is in addition to the standard app-defaults files, and
       does not override resources defined elsewhere */
    const char *data_dir;
    char *path;
    XrmDatabase db = XtDatabase (dpy); /* #### XtScreenDatabase(dpy) ? */
    const char *locale = XrmLocaleOfDatabase (db);

    if (STRINGP (Vx_app_defaults_directory) &&
	XSTRING_LENGTH (Vx_app_defaults_directory) > 0)
      {
	LISP_STRING_TO_EXTERNAL (Vx_app_defaults_directory, data_dir, Qfile_name);
	path = (char *)alloca (strlen (data_dir) + strlen (locale) + 7);
	sprintf (path, "%s%s/Emacs", data_dir, locale);
	if (!access (path, R_OK))
	  XrmCombineFileDatabase (path, &db, False);
      }
    else if (STRINGP (Vdata_directory) && XSTRING_LENGTH (Vdata_directory) > 0)
      {
	LISP_STRING_TO_EXTERNAL (Vdata_directory, data_dir, Qfile_name);
	path = (char *)alloca (strlen (data_dir) + 13 + strlen (locale) + 7);
	sprintf (path, "%sapp-defaults/%s/Emacs", data_dir, locale);
	if (!access (path, R_OK))
	  XrmCombineFileDatabase (path, &db, False);
      }
 }
#endif /* LWLIB_MENUBARS_MOTIF or HAVE_XIM USE_XFONTSET */
#endif /* MULE */

  if (NILP (DEVICE_NAME (d)))
    DEVICE_NAME (d) = display;

  /* We're going to modify the string in-place, so be a nice XEmacs */
  DEVICE_NAME (d) = Fcopy_sequence (DEVICE_NAME (d));
  /* colons and periods can't appear in individual elements of resource
     strings */

  XtGetApplicationNameAndClass (dpy, (char **) &app_name, (char **) &app_class);
  /* search for a matching visual if requested by the user, or setup the display default */
  {
    int resource_name_length = max (sizeof (".emacsVisual"),
				    sizeof (".privateColormap"));
    char *buf1 = alloca_array (char, strlen (app_name)  + resource_name_length);
    char *buf2 = alloca_array (char, strlen (app_class) + resource_name_length);
    char *type;
    XrmValue value;

    sprintf (buf1, "%s.emacsVisual", app_name);
    sprintf (buf2, "%s.EmacsVisual", app_class);
    if (XrmGetResource (XtDatabase (dpy), buf1, buf2, &type, &value) == True)
      {
	int cnt = 0;
	int vis_class = PseudoColor;
	XVisualInfo vinfo;
	char *str = (char*) value.addr;

#define CHECK_VIS_CLASS(visual_class)					\
 else if (memcmp (str, #visual_class, sizeof (#visual_class) - 1) == 0)	\
	cnt = sizeof (#visual_class) - 1, vis_class = visual_class

	if (1)
	  ;
	CHECK_VIS_CLASS (StaticGray);
	CHECK_VIS_CLASS (StaticColor);
	CHECK_VIS_CLASS (TrueColor);
	CHECK_VIS_CLASS (GrayScale);
	CHECK_VIS_CLASS (PseudoColor);
	CHECK_VIS_CLASS (DirectColor);

	if (cnt)
	  {
	    depth = atoi (str + cnt);
	    if (depth == 0)
	      {
		stderr_out ("Invalid Depth specification in %s... ignoring...\n", str);
	      }
	    else
	      {
		if (XMatchVisualInfo (dpy, screen, depth, vis_class, &vinfo))
		  {
		    visual = vinfo.visual;
		  }
		else
		  {
		    stderr_out ("Can't match the requested visual %s... using defaults\n", str);
		  }
	      }
	  }
	else
	  {
	    stderr_out( "Invalid Visual specification in %s... ignoring.\n", str);
	  }
      }
    if (visual == NULL)
      {
	/*
	  visual = DefaultVisual(dpy, screen);
	  depth = DefaultDepth(dpy, screen);
	*/
	visual = x_try_best_visual (dpy, screen);
	depth = x_get_visual_depth (dpy, visual);
	best_visual_found = (visual != DefaultVisual (dpy, screen));
      }

    /* If we've got the same visual as the default and it's PseudoColor,
       check to see if the user specified that we need a private colormap */
    if (visual == DefaultVisual (dpy, screen))
      {
	sprintf (buf1, "%s.privateColormap", app_name);
	sprintf (buf2, "%s.PrivateColormap", app_class);
	if ((visual->class == PseudoColor) &&
	    (XrmGetResource (XtDatabase (dpy), buf1, buf2, &type, &value) == True))
	  {
	     cmap = XCopyColormapAndFree (dpy, DefaultColormap (dpy, screen));
	  }
	else
	  {
	    cmap = DefaultColormap (dpy, screen);
	  }
      }
    else
      {
	if ( best_visual_found )
	  {
	    cmap = XCreateColormap (dpy,  RootWindow (dpy, screen), visual, AllocNone);
	  }
	else
	  {
	    /* We have to create a matching colormap anyway...
	       #### think about using standard colormaps (need the Xmu libs?) */
	    cmap = XCreateColormap(dpy, RootWindow(dpy, screen), visual, AllocNone);
	    XInstallColormap(dpy, cmap);
	  }
      }
  }

  DEVICE_X_VISUAL   (d) = visual;
  DEVICE_X_COLORMAP (d) = cmap;
  DEVICE_X_DEPTH    (d) = depth;
  validify_resource_component ((char *) XSTRING_DATA (DEVICE_NAME (d)),
			       XSTRING_LENGTH (DEVICE_NAME (d)));

  {
    Arg al[3];
    XtSetArg (al[0], XtNvisual,   visual);
    XtSetArg (al[1], XtNdepth,    depth);
    XtSetArg (al[2], XtNcolormap, cmap);

    app_shell = XtAppCreateShell (NULL, app_class,
				  applicationShellWidgetClass,
				  dpy, al, countof (al));
  }

  DEVICE_XT_APP_SHELL (d) = app_shell;

#ifdef HAVE_XIM
  XIM_init_device(d);
#endif /* HAVE_XIM */

  /* Realize the app_shell so that its window exists for GC creation purposes,
     and set it to the size of the root window for child placement purposes */
  {
    Arg al[5];
    XtSetArg (al[0], XtNmappedWhenManaged, False);
    XtSetArg (al[1], XtNx, 0);
    XtSetArg (al[2], XtNy, 0);
    XtSetArg (al[3], XtNwidth,  WidthOfScreen  (ScreenOfDisplay (dpy, screen)));
    XtSetArg (al[4], XtNheight, HeightOfScreen (ScreenOfDisplay (dpy, screen)));
    XtSetValues (app_shell, al, countof (al));
    XtRealizeWidget (app_shell);
  }

#ifdef HAVE_WMCOMMAND
  {
    int new_argc;
    Extbyte **new_argv;
    make_argc_argv (Vcommand_line_args, &new_argc, &new_argv);
    XSetCommand (XtDisplay (app_shell), XtWindow (app_shell),
		 (char **) new_argv, new_argc);
    free_argc_argv (new_argv);
  }
#endif /* HAVE_WMCOMMAND */


#ifdef HAVE_OFFIX_DND
  DndInitialize ( app_shell );
#endif

  Vx_initial_argv_list = make_arg_list (argc, argv);
  free_argc_argv (argv);

  DEVICE_X_WM_COMMAND_FRAME (d) = Qnil;

  sanity_check_geometry_resource (dpy);

  /* In event-Xt.c */
  x_init_modifier_mapping (d);

  DEVICE_INFD (d) = DEVICE_OUTFD (d) = ConnectionNumber (dpy);
  init_baud_rate (d);
  init_one_device (d);

  DEVICE_X_GC_CACHE (d) = make_gc_cache (dpy, XtWindow(app_shell));
  DEVICE_X_GRAY_PIXMAP (d) = None;
  Xatoms_of_device_x (d);
  Xatoms_of_select_x (d);
  Xatoms_of_objects_x (d);
  x_init_device_class (d);

  /* Run the elisp side of the X device initialization. */
  call0 (Qinit_pre_x_win);
}

static void
x_finish_init_device (struct device *d, Lisp_Object props)
{
  call0 (Qinit_post_x_win);
}

static void
x_mark_device (struct device *d)
{
  mark_object (DEVICE_X_WM_COMMAND_FRAME (d));
  mark_object (DEVICE_X_DATA (d)->x_keysym_map_hash_table);
}


/************************************************************************/
/*                       closing an X connection	                */
/************************************************************************/

static void
free_x_device_struct (struct device *d)
{
  xfree (d->device_data);
}

static void
x_delete_device (struct device *d)
{
  Lisp_Object device;
  Display *display;
#ifdef FREE_CHECKING
  extern void (*__free_hook) (void *);
  int checking_free;
#endif

  XSETDEVICE (device, d);
  display = DEVICE_X_DISPLAY (d);

  if (display)
    {
#ifdef FREE_CHECKING
      checking_free = (__free_hook != 0);

      /* Disable strict free checking, to avoid bug in X library */
      if (checking_free)
	disable_strict_free_check ();
#endif

      free_gc_cache (DEVICE_X_GC_CACHE (d));
      if (DEVICE_X_DATA (d)->x_modifier_keymap)
	XFreeModifiermap (DEVICE_X_DATA (d)->x_modifier_keymap);
      if (DEVICE_X_DATA (d)->x_keysym_map)
	XFree ((char *) DEVICE_X_DATA (d)->x_keysym_map);

      if (DEVICE_XT_APP_SHELL (d))
	{
	  XtDestroyWidget (DEVICE_XT_APP_SHELL (d));
	  DEVICE_XT_APP_SHELL (d) = NULL;
	}

      XtCloseDisplay (display);
      DEVICE_X_DISPLAY (d) = 0;
#ifdef FREE_CHECKING
      if (checking_free)
	enable_strict_free_check ();
#endif
    }

  if (EQ (device, Vdefault_x_device))
    {
      Lisp_Object devcons, concons;
      /* #### handle deleting last X device */
      Vdefault_x_device = Qnil;
      DEVICE_LOOP_NO_BREAK (devcons, concons)
	{
	  if (DEVICE_X_P (XDEVICE (XCAR (devcons))) &&
	      !EQ (device, XCAR (devcons)))
	    {
	      Vdefault_x_device = XCAR (devcons);
	      goto double_break;
	    }
	}
    }
 double_break:
  free_x_device_struct (d);
}


/************************************************************************/
/*				handle X errors				*/
/************************************************************************/

const char *
x_event_name (int event_type)
{
  static const char *events[] =
  {
    "0: ERROR!",
    "1: REPLY",
    "KeyPress",
    "KeyRelease",
    "ButtonPress",
    "ButtonRelease",
    "MotionNotify",
    "EnterNotify",
    "LeaveNotify",
    "FocusIn",
    "FocusOut",
    "KeymapNotify",
    "Expose",
    "GraphicsExpose",
    "NoExpose",
    "VisibilityNotify",
    "CreateNotify",
    "DestroyNotify",
    "UnmapNotify",
    "MapNotify",
    "MapRequest",
    "ReparentNotify",
    "ConfigureNotify",
    "ConfigureRequest",
    "GravityNotify",
    "ResizeRequest",
    "CirculateNotify",
    "CirculateRequest",
    "PropertyNotify",
    "SelectionClear",
    "SelectionRequest",
    "SelectionNotify",
    "ColormapNotify",
    "ClientMessage",
    "MappingNotify",
    "LASTEvent"
  };

  if (event_type < 0 || event_type >= countof (events))
    return NULL;
  return events [event_type];
}

/* Handling errors.

   If an X error occurs which we are not expecting, we have no alternative
   but to print it to stderr.  It would be nice to stuff it into a pop-up
   buffer, or to print it in the minibuffer, but that's not possible, because
   one is not allowed to do any I/O on the display connection from an error
   handler. The guts of Xlib expect these functions to either return or exit.

   However, there are occasions when we might expect an error to reasonably
   occur.  The interface to this is as follows:

   Before calling some X routine which may error, call
	expect_x_error (dpy);

   Just after calling the X routine, call either:

	x_error_occurred_p (dpy);

   to ask whether an error happened (and was ignored), or:

	signal_if_x_error (dpy, resumable_p);

   which will call Fsignal() with args appropriate to the X error, if there
   was one.  (Resumable_p is whether the debugger should be allowed to
   continue from the call to signal.)

   You must call one of these two routines immediately after calling the X
   routine; think of them as bookends like BLOCK_INPUT and UNBLOCK_INPUT.
 */

static int error_expected;
static int error_occurred;
static XErrorEvent last_error;

/* OVERKILL! */

#ifdef EXTERNAL_WIDGET
static Lisp_Object
x_error_handler_do_enqueue (Lisp_Object frame)
{
  enqueue_magic_eval_event (io_error_delete_frame, frame);
  return Qt;
}

static Lisp_Object
x_error_handler_error (Lisp_Object data, Lisp_Object dummy)
{
  return Qnil;
}
#endif /* EXTERNAL_WIDGET */

int
x_error_handler (Display *disp, XErrorEvent *event)
{
  if (error_expected)
    {
      error_expected = 0;
      error_occurred = 1;
      last_error = *event;
    }
  else
    {
#ifdef EXTERNAL_WIDGET
      struct frame *f;
      struct device *d = get_device_from_display (disp);

      if ((event->error_code == BadWindow ||
	   event->error_code == BadDrawable)
	  && ((f = x_any_window_to_frame (d, event->resourceid)) != 0))
	{
	  Lisp_Object frame;

	/* one of the windows comprising one of our frames has died.
	   This occurs particularly with ExternalShell frames when the
	   client that owns the ExternalShell's window dies.

	   We cannot do any I/O on the display connection so we need
	   to enqueue an eval event so that the deletion happens
	   later.

	   Furthermore, we need to trap any errors (out-of-memory) that
	   may occur when Fenqueue_eval_event is called.
	 */

	if (f->being_deleted)
	  return 0;
	XSETFRAME (frame, f);
	if (!NILP (condition_case_1 (Qerror, x_error_handler_do_enqueue,
				     frame, x_error_handler_error, Qnil)))
	  {
	    f->being_deleted = 1;
	    f->visible = 0;
	  }
	return 0;
      }
#endif /* EXTERNAL_WIDGET */

#if 0
      /* This ends up calling X, which isn't allowed in an X error handler
       */
      stderr_out ("\n%s: ",
		  (STRINGP (Vinvocation_name)
		   ? (char *) XSTRING_DATA (Vinvocation_name)
		   : "xemacs"));
#endif
      XmuPrintDefaultErrorMessage (disp, event, stderr);
    }
  return 0;
}

void
expect_x_error (Display *dpy)
{
  assert (!error_expected);
  XSync (dpy, 0);	/* handle pending errors before setting flag */
  error_expected = 1;
  error_occurred = 0;
}

int
x_error_occurred_p (Display *dpy)
{
  int val;
  XSync (dpy, 0);	/* handle pending errors before setting flag */
  val = error_occurred;
  error_expected = 0;
  error_occurred = 0;
  return val;
}

int
signal_if_x_error (Display *dpy, int resumable_p)
{
  char buf[1024];
  Lisp_Object data;
  if (! x_error_occurred_p (dpy))
    return 0;
  data = Qnil;
  sprintf (buf, "0x%X", (unsigned int) last_error.resourceid);
  data = Fcons (build_string (buf), data);
  {
    char num [32];
    sprintf (num, "%d", last_error.request_code);
    XGetErrorDatabaseText (last_error.display, "XRequest", num, "",
			   buf, sizeof (buf));
    if (! *buf)
      sprintf (buf, "Request-%d", last_error.request_code);
    data = Fcons (build_string (buf), data);
  }
  XGetErrorText (last_error.display, last_error.error_code, buf, sizeof (buf));
  data = Fcons (build_string (buf), data);
 again:
  Fsignal (Qx_error, data);
  if (! resumable_p) goto again;
  return 1;
}

int
x_IO_error_handler (Display *disp)
{
  /* This function can GC */
  Lisp_Object dev;
  struct device *d = get_device_from_display_1 (disp);

  if (!d)
    d = device_being_initialized;

  assert (d != NULL);
  XSETDEVICE (dev, d);

  if (NILP (find_nonminibuffer_frame_not_on_device (dev)))
    {
      /* We're going down. */
      stderr_out
	("\n%s: Fatal I/O Error %d (%s) on display connection \"%s\"\n",
         (STRINGP (Vinvocation_name) ?
	  (char *) XSTRING_DATA (Vinvocation_name) : "xemacs"),
	 errno, strerror (errno), DisplayString (disp));
      stderr_out
        ("  after %lu requests (%lu known processed) with %d events remaining.\n",
         NextRequest (disp) - 1, LastKnownRequestProcessed (disp),
         QLength (disp));
      /* assert (!_Xdebug); */
    }
  else
    {
      warn_when_safe
	(Qx, Qcritical,
	 "I/O Error %d (%s) on display connection\n"
	 "  \"%s\" after after %lu requests (%lu known processed)\n"
	 "  with %d events remaining.\n"
	 "  Throwing to top level.\n",
	 errno, strerror (errno), DisplayString (disp),
         NextRequest (disp) - 1, LastKnownRequestProcessed (disp),
         QLength (disp));
    }

  /* According to X specs, we should not return from this function, or
     Xlib might just decide to exit().  So we mark the offending
     console for deletion and throw to top level.  */
  if (d)
    enqueue_magic_eval_event (io_error_delete_device, dev);
  DEVICE_X_BEING_DELETED (d) = 1;
  Fthrow (Qtop_level, Qnil);

  return 0; /* not reached */
}

DEFUN ("x-debug-mode", Fx_debug_mode, 1, 2, 0, /*
With a true arg, make the connection to the X server synchronous.
With false, make it asynchronous.  Synchronous connections are much slower,
but are useful for debugging. (If you get X errors, make the connection
synchronous, and use a debugger to set a breakpoint on `x_error_handler'.
Your backtrace of the C stack will now be useful.  In asynchronous mode,
the stack above `x_error_handler' isn't helpful because of buffering.)
If DEVICE is not specified, the selected device is assumed.

Calling this function is the same as calling the C function `XSynchronize',
or starting the program with the `-sync' command line argument.
*/
       (arg, device))
{
  struct device *d = decode_x_device (device);

  XSynchronize (DEVICE_X_DISPLAY (d), !NILP (arg));

  if (!NILP (arg))
    message ("X connection is synchronous");
  else
    message ("X connection is asynchronous");

  return arg;
}


/************************************************************************/
/*                             X resources                              */
/************************************************************************/

#if 0 /* bah humbug.  The whole "widget == resource" stuff is such
	 a crock of shit that I'm just going to ignore it all. */

/* If widget is NULL, we are retrieving device or global face data. */

static void
construct_name_list (Display *display, Widget widget, char *fake_name,
		     char *fake_class, char *name, char *class)
{
  char *stack [100][2];
  Widget this;
  int count = 0;
  char *name_tail, *class_tail;

  if (widget)
    {
      for (this = widget; this; this = XtParent (this))
	{
	  stack [count][0] = this->core.name;
	  stack [count][1] = XtClass (this)->core_class.class_name;
	  count++;
	}
      count--;
    }
  else if (fake_name && fake_class)
    {
      stack [count][0] = fake_name;
      stack [count][1] = fake_class;
      count++;
    }

  /* The root widget is an application shell; resource lookups use the
     specified application name and application class in preference to
     the name/class of that widget (which is argv[0] / "ApplicationShell").
     Generally the app name and class will be argv[0] / "Emacs" but
     the former can be set via the -name command-line option, and the
     latter can be set by changing `x-emacs-application-class' in
     lisp/term/x-win.el.
   */
  XtGetApplicationNameAndClass (display,
				&stack [count][0],
				&stack [count][1]);

  name [0] = 0;
  class [0] = 0;

  name_tail  = name;
  class_tail = class;
  for (; count >= 0; count--)
    {
      strcat (name_tail,  stack [count][0]);
      for (; *name_tail; name_tail++)
	if (*name_tail == '.') *name_tail = '_';
      strcat (name_tail, ".");
      name_tail++;

      strcat (class_tail, stack [count][1]);
      for (; *class_tail; class_tail++)
	if (*class_tail == '.') *class_tail = '_';
      strcat (class_tail, ".");
      class_tail++;
    }
}

#endif /* 0 */

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

static char_dynarr *name_char_dynarr;
static char_dynarr *class_char_dynarr;

/* Given a locale and device specification from x-get-resource or
x-get-resource-prefix, return the resource prefix and display to
fetch the resource on. */

static void
x_get_resource_prefix (Lisp_Object locale, Lisp_Object device,
		       Display **display_out, char_dynarr *name,
		       char_dynarr *class)
{
  if (NILP (locale))
    locale = Qglobal;
  if (NILP (Fvalid_specifier_locale_p (locale)))
    signal_simple_error ("Invalid locale", locale);
  if (WINDOWP (locale))
    /* #### I can't come up with any coherent way of naming windows.
       By relative position?  That seems tricky because windows
       can change position, be split, etc.  By order of creation?
       That seems less than useful. */
    signal_simple_error ("Windows currently can't be resourced", locale);

  if (!NILP (device) && !DEVICEP (device))
    CHECK_DEVICE (device);
  if (DEVICEP (device) && !DEVICE_X_P (XDEVICE (device)))
    device = Qnil;
  if (NILP (device))
    {
      device = DFW_DEVICE (locale);
      if (DEVICEP (device) && !DEVICE_X_P (XDEVICE (device)))
	device = Qnil;
      if (NILP (device))
	device = Vdefault_x_device;
      if (NILP (device))
	{
	  *display_out = 0;
	  return;
	}
    }

  *display_out = DEVICE_X_DISPLAY (XDEVICE (device));

  {
    char *appname, *appclass;
    int name_len, class_len;
    XtGetApplicationNameAndClass (*display_out, &appname, &appclass);
    name_len  = strlen (appname);
    class_len = strlen (appclass);
    Dynarr_add_many (name , appname,  name_len);
    Dynarr_add_many (class, appclass, class_len);
    validify_resource_component (Dynarr_atp (name,  0), name_len);
    validify_resource_component (Dynarr_atp (class, 0), class_len);
  }

  if (EQ (locale, Qglobal))
    return;
  if (BUFFERP (locale))
    {
      Dynarr_add_literal_string (name, ".buffer.");
      /* we know buffer is live; otherwise we got an error above. */
      Dynarr_add_validified_lisp_string (name, Fbuffer_name (locale));
      Dynarr_add_literal_string (class, ".EmacsLocaleType.EmacsBuffer");
    }
  else if (FRAMEP (locale))
    {
      Dynarr_add_literal_string (name, ".frame.");
      /* we know frame is live; otherwise we got an error above. */
      Dynarr_add_validified_lisp_string (name, Fframe_name (locale));
      Dynarr_add_literal_string (class, ".EmacsLocaleType.EmacsFrame");
    }
  else
    {
      assert (DEVICEP (locale));
      Dynarr_add_literal_string (name, ".device.");
      /* we know device is live; otherwise we got an error above. */
      Dynarr_add_validified_lisp_string (name, Fdevice_name (locale));
      Dynarr_add_literal_string (class, ".EmacsLocaleType.EmacsDevice");
    }
  return;
}

DEFUN ("x-get-resource", Fx_get_resource, 3, 6, 0, /*
Retrieve an X resource from the resource manager.

The first arg is the name of the resource to retrieve, such as "font".
The second arg is the class of the resource to retrieve, such as "Font".
The third arg must be one of the symbols 'string, 'integer, 'natnum, or
  'boolean, specifying the type of object that the database is searched for.
The fourth arg is the locale to search for the resources on, and can
  currently be a buffer, a frame, a device, or 'global.  If omitted, it
  defaults to 'global.
The fifth arg is the device to search for the resources on. (The resource
  database for a particular device is constructed by combining non-device-
  specific resources such as any command-line resources specified and any
  app-defaults files found [or the fallback resources supplied by XEmacs,
  if no app-defaults file is found] with device-specific resources such as
  those supplied using xrdb.) If omitted, it defaults to the device of
  LOCALE, if a device can be derived (i.e. if LOCALE is a frame or device),
  and otherwise defaults to the value of `default-x-device'.
The sixth arg NOERROR, if non-nil, means do not signal an error if a
  bogus resource specification was retrieved (e.g. if a non-integer was
  given when an integer was requested).  In this case, a warning is issued
  instead, unless NOERROR is t, in which case no warning is issued.

The resource names passed to this function are looked up relative to the
locale.

If you want to search for a subresource, you just need to specify the
resource levels in NAME and CLASS.  For example, NAME could be
"modeline.attributeFont", and CLASS "Face.AttributeFont".

Specifically,

1) If LOCALE is a buffer, a call

    (x-get-resource "foreground" "Foreground" 'string SOME-BUFFER)

is an interface to a C call something like

    XrmGetResource (db, "xemacs.buffer.BUFFER-NAME.foreground",
			"Emacs.EmacsLocaleType.EmacsBuffer.Foreground",
			"String");

2) If LOCALE is a frame, a call

    (x-get-resource "foreground" "Foreground" 'string SOME-FRAME)

is an interface to a C call something like

    XrmGetResource (db, "xemacs.frame.FRAME-NAME.foreground",
			"Emacs.EmacsLocaleType.EmacsFrame.Foreground",
			"String");

3) If LOCALE is a device, a call

    (x-get-resource "foreground" "Foreground" 'string SOME-DEVICE)

is an interface to a C call something like

    XrmGetResource (db, "xemacs.device.DEVICE-NAME.foreground",
			"Emacs.EmacsLocaleType.EmacsDevice.Foreground",
			"String");

4) If LOCALE is 'global, a call

    (x-get-resource "foreground" "Foreground" 'string 'global)

is an interface to a C call something like

    XrmGetResource (db, "xemacs.foreground",
			"Emacs.Foreground",
			"String");

Note that for 'global, no prefix is added other than that of the
application itself; thus, you can use this locale to retrieve
arbitrary application resources, if you really want to.

The returned value of this function is nil if the queried resource is not
found.  If the third arg is `string', a string is returned, and if it is
`integer', an integer is returned.  If the third arg is `boolean', then the
returned value is the list (t) for true, (nil) for false, and is nil to
mean ``unspecified''.
*/
       (name, class, type, locale, device, noerror))
{
  char* name_string, *class_string;
  char *raw_result;
  XrmDatabase db;
  Display *display;
  Error_behavior errb = decode_error_behavior_flag (noerror);

  CHECK_STRING (name);
  CHECK_STRING (class);
  CHECK_SYMBOL (type);

  Dynarr_reset (name_char_dynarr);
  Dynarr_reset (class_char_dynarr);

  x_get_resource_prefix (locale, device, &display,
			 name_char_dynarr, class_char_dynarr);
  if (!display)
    return Qnil;

  db = XtDatabase (display);

  Dynarr_add (name_char_dynarr, '.');
  Dynarr_add_lisp_string (name_char_dynarr, name);
  Dynarr_add (class_char_dynarr, '.');
  Dynarr_add_lisp_string (class_char_dynarr, class);
  Dynarr_add (name_char_dynarr,  '\0');
  Dynarr_add (class_char_dynarr, '\0');

  name_string  = Dynarr_atp (name_char_dynarr,  0);
  class_string = Dynarr_atp (class_char_dynarr, 0);

  {
    XrmValue xrm_value;
    XrmName namelist[100];
    XrmClass classlist[100];
    XrmName *namerest = namelist;
    XrmClass *classrest = classlist;
    XrmRepresentation xrm_type;
    XrmRepresentation string_quark;
    int result;
    XrmStringToNameList (name_string, namelist);
    XrmStringToClassList (class_string, classlist);
    string_quark = XrmStringToQuark ("String");

    /* ensure that they have the same length */
    while (namerest[0] && classrest[0])
      namerest++, classrest++;
    if (namerest[0] || classrest[0])
      signal_simple_error_2
	("class list and name list must be the same length", name, class);
    result = XrmQGetResource (db, namelist, classlist, &xrm_type, &xrm_value);

    if (result != True || xrm_type != string_quark)
      return Qnil;
    raw_result = (char *) xrm_value.addr;
  }

  if (EQ (type, Qstring))
    return build_string (raw_result);
  else if (EQ (type, Qboolean))
    {
      if (!ascii_strcasecmp (raw_result, "off")   ||
	  !ascii_strcasecmp (raw_result, "false") ||
	  !ascii_strcasecmp (raw_result, "no"))
	return Fcons (Qnil, Qnil);
      if (!ascii_strcasecmp (raw_result, "on")   ||
	  !ascii_strcasecmp (raw_result, "true") ||
	  !ascii_strcasecmp (raw_result, "yes"))
	return Fcons (Qt, Qnil);
      return maybe_continuable_error
	(Qresource, errb,
	 "can't convert %s: %s to a Boolean", name_string, raw_result);
    }
  else if (EQ (type, Qinteger) || EQ (type, Qnatnum))
    {
      int i;
      char c;
      if (1 != sscanf (raw_result, "%d%c", &i, &c))
	return maybe_continuable_error
	  (Qresource, errb,
	   "can't convert %s: %s to an integer", name_string, raw_result);
      else if (EQ (type, Qnatnum) && i < 0)
	return maybe_continuable_error
	  (Qresource, errb,
	   "invalid numerical value %d for resource %s", i, name_string);
      else
	return make_int (i);
    }
  else
    {
      return maybe_signal_continuable_error
	(Qwrong_type_argument,
	 list2 (build_translated_string
		("should be string, integer, natnum or boolean"),
		type),
	 Qresource, errb);
    }
}

DEFUN ("x-get-resource-prefix", Fx_get_resource_prefix, 1, 2, 0, /*
Return the resource prefix for LOCALE on DEVICE.
The resource prefix is the strings used to prefix resources if
the LOCALE and DEVICE arguments were passed to `x-get-resource'.
The returned value is a cons of a name prefix and a class prefix.
For example, if LOCALE is a frame, the returned value might be
\("xemacs.frame.FRAME-NAME" . "Emacs.EmacsLocaleType.EmacsFrame").
If no valid X device for resourcing can be obtained, this function
returns nil. (In such a case, `x-get-resource' would always return nil.)
*/
       (locale, device))
{
  Display *display;

  Dynarr_reset (name_char_dynarr );
  Dynarr_reset (class_char_dynarr);

  x_get_resource_prefix (locale, device, &display,
			 name_char_dynarr, class_char_dynarr);
  if (!display)
    return Qnil;

  return Fcons (make_string ((Bufbyte *) Dynarr_atp (name_char_dynarr, 0),
			     Dynarr_length (name_char_dynarr)),
		make_string ((Bufbyte *) Dynarr_atp (class_char_dynarr, 0),
			     Dynarr_length (class_char_dynarr)));
}

DEFUN ("x-put-resource", Fx_put_resource, 1, 2, 0, /*
Add a resource to the resource database for DEVICE.
RESOURCE-LINE specifies the resource to add and should be a
standard resource specification.
*/
       (resource_line, device))
{
  struct device *d = decode_device (device);
  char *str, *colon_pos;

  CHECK_STRING (resource_line);
  str = (char *) XSTRING_DATA (resource_line);
  if (!(colon_pos = strchr (str, ':')) || strchr (str, '\n'))
  invalid:
    signal_simple_error ("Invalid resource line", resource_line);
  if (strspn (str,
	      /* Only the following chars are allowed before the colon */
	      " \t.*?abcdefghijklmnopqrstuvwxyz"
	      "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-")
      != (size_t) (colon_pos - str))
    goto invalid;

  if (DEVICE_X_P (d))
    {
      XrmDatabase db = XtDatabase (DEVICE_X_DISPLAY (d));
      XrmPutLineResource (&db, str);
    }

  return Qnil;
}


/************************************************************************/
/*                   display information functions                      */
/************************************************************************/

DEFUN ("default-x-device", Fdefault_x_device, 0, 0, 0, /*
Return the default X device for resourcing.
This is the first-created X device that still exists.
*/
       ())
{
  return Vdefault_x_device;
}

DEFUN ("x-display-visual-class", Fx_display_visual_class, 0, 1, 0, /*
Return the visual class of the X display DEVICE is using.
This can be altered from the default at startup using the XResource "EmacsVisual".
The returned value will be one of the symbols `static-gray', `gray-scale',
`static-color', `pseudo-color', `true-color', or `direct-color'.
*/
       (device))
{
  Visual *vis = DEVICE_X_VISUAL (decode_x_device (device));
  switch (vis->class)
    {
    case StaticGray:  return intern ("static-gray");
    case GrayScale:   return intern ("gray-scale");
    case StaticColor: return intern ("static-color");
    case PseudoColor: return intern ("pseudo-color");
    case TrueColor:   return intern ("true-color");
    case DirectColor: return intern ("direct-color");
    default:
      error ("display has an unknown visual class");
      return Qnil;	/* suppress compiler warning */
    }
}

DEFUN ("x-display-visual-depth", Fx_display_visual_depth, 0, 1, 0, /*
Return the bitplane depth of the visual the X display DEVICE is using.
*/
       (device))
{
   return make_int (DEVICE_X_DEPTH (decode_x_device (device)));
}

static Lisp_Object
x_device_system_metrics (struct device *d,
			 enum device_metrics m)
{
  Display *dpy = DEVICE_X_DISPLAY (d);

  switch (m)
    {
    case DM_size_device:
      return Fcons (make_int (DisplayWidth (dpy, DefaultScreen (dpy))),
		    make_int (DisplayHeight (dpy, DefaultScreen (dpy))));
    case DM_size_device_mm:
      return Fcons (make_int (DisplayWidthMM (dpy, DefaultScreen (dpy))),
		    make_int (DisplayHeightMM (dpy, DefaultScreen (dpy))));
    case DM_num_bit_planes:
      return make_int (DisplayPlanes (dpy, DefaultScreen (dpy)));
    case DM_num_color_cells:
      return make_int (DisplayCells (dpy, DefaultScreen (dpy)));
    default: /* No such device metric property for X devices  */
      return Qunbound;
    }
}

DEFUN ("x-server-vendor", Fx_server_vendor, 0, 1, 0, /*
Return the vendor ID string of the X server DEVICE is on.
Return the empty string if the vendor ID string cannot be determined.
*/
       (device))
{
  Display *dpy = get_x_display (device);
  char *vendor = ServerVendor (dpy);

  return build_string (vendor ? vendor : "");
}

DEFUN ("x-server-version", Fx_server_version, 0, 1, 0, /*
Return the version numbers of the X server DEVICE is on.
The returned value is a list of three integers: the major and minor
version numbers of the X Protocol in use, and the vendor-specific release
number.  See also `x-server-vendor'.
*/
       (device))
{
  Display *dpy = get_x_display (device);

  return list3 (make_int (ProtocolVersion  (dpy)),
		make_int (ProtocolRevision (dpy)),
		make_int (VendorRelease    (dpy)));
}

DEFUN ("x-valid-keysym-name-p", Fx_valid_keysym_name_p, 1, 1, 0, /*
Return true if KEYSYM names a keysym that the X library knows about.
Valid keysyms are listed in the files /usr/include/X11/keysymdef.h and in
/usr/lib/X11/XKeysymDB, or whatever the equivalents are on your system.
*/
       (keysym))
{
  const char *keysym_ext;

  CHECK_STRING (keysym);
  LISP_STRING_TO_EXTERNAL (keysym, keysym_ext, Qctext);

  return XStringToKeysym (keysym_ext) ? Qt : Qnil;
}

DEFUN ("x-keysym-hash-table", Fx_keysym_hash_table, 0, 1, 0, /*
Return a hash table containing a key for all keysyms on DEVICE.
DEVICE must be an X11 display device.  See `x-keysym-on-keyboard-p'.
*/
       (device))
{
  struct device *d = decode_device (device);
  if (!DEVICE_X_P (d))
    signal_simple_error ("Not an X device", device);

  return DEVICE_X_DATA (d)->x_keysym_map_hash_table;
}

DEFUN ("x-keysym-on-keyboard-sans-modifiers-p", Fx_keysym_on_keyboard_sans_modifiers_p,
       1, 2, 0, /*
Return true if KEYSYM names a key on the keyboard of DEVICE.
More precisely, return true if pressing a physical key
on the keyboard of DEVICE without any modifier keys generates KEYSYM.
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
  if (!DEVICE_X_P (d))
    signal_simple_error ("Not an X device", device);

  return (EQ (Qsans_modifiers,
	      Fgethash (keysym, DEVICE_X_KEYSYM_MAP_HASH_TABLE (d), Qnil)) ?
	  Qt : Qnil);
}


DEFUN ("x-keysym-on-keyboard-p", Fx_keysym_on_keyboard_p, 1, 2, 0, /*
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
  if (!DEVICE_X_P (d))
    signal_simple_error ("Not an X device", device);

  return (NILP (Fgethash (keysym, DEVICE_X_KEYSYM_MAP_HASH_TABLE (d), Qnil)) ?
	  Qnil : Qt);
}


/************************************************************************/
/*                          grabs and ungrabs                           */
/************************************************************************/

DEFUN ("x-grab-pointer", Fx_grab_pointer, 0, 3, 0, /*
Grab the pointer and restrict it to its current window.
If optional DEVICE argument is nil, the default device will be used.
If optional CURSOR argument is non-nil, change the pointer shape to that
 until `x-ungrab-pointer' is called (it should be an object returned by the
 `make-cursor-glyph' function).
If the second optional argument IGNORE-KEYBOARD is non-nil, ignore all
  keyboard events during the grab.
Returns t if the grab is successful, nil otherwise.
*/
       (device, cursor, ignore_keyboard))
{
  Window w;
  int pointer_mode, result;
  struct device *d = decode_x_device (device);

  if (!NILP (cursor))
    {
      CHECK_POINTER_GLYPH (cursor);
      cursor = glyph_image_instance (cursor, device, ERROR_ME, 0);
    }

  if (!NILP (ignore_keyboard))
    pointer_mode = GrabModeSync;
  else
    pointer_mode = GrabModeAsync;

  w = XtWindow (FRAME_X_TEXT_WIDGET (device_selected_frame (d)));

  /* #### Possibly this needs to gcpro the cursor somehow, but it doesn't
     seem to cause a problem if XFreeCursor is called on a cursor in use
     in a grab; I suppose the X server counts the grab as a reference
     and doesn't free it until it exits? */
  result = XGrabPointer (DEVICE_X_DISPLAY (d), w,
			 False,
			 ButtonMotionMask  |
			 ButtonPressMask   |
			 ButtonReleaseMask |
			 PointerMotionHintMask,
			 GrabModeAsync,	      /* Keep pointer events flowing */
			 pointer_mode,	      /* Stall keyboard events */
			 w,		      /* Stay in this window */
			 (NILP (cursor) ? 0
			  : XIMAGE_INSTANCE_X_CURSOR (cursor)),
			 CurrentTime);
  return (result == GrabSuccess) ? Qt : Qnil;
}

DEFUN ("x-ungrab-pointer", Fx_ungrab_pointer, 0, 1, 0, /*
Release a pointer grab made with `x-grab-pointer'.
If optional first arg DEVICE is nil the default device is used.
If it is t the pointer will be released on all X devices.
*/
       (device))
{
  if (!EQ (device, Qt))
    {
      Display *dpy = get_x_display (device);
      XUngrabPointer (dpy, CurrentTime);
    }
  else
    {
      Lisp_Object devcons, concons;

      DEVICE_LOOP_NO_BREAK (devcons, concons)
	{
	  struct device *d = XDEVICE (XCAR (devcons));

	  if (DEVICE_X_P (d))
	    XUngrabPointer (DEVICE_X_DISPLAY (d), CurrentTime);
	}
    }

  return Qnil;
}

DEFUN ("x-grab-keyboard", Fx_grab_keyboard, 0, 1, 0, /*
Grab the keyboard on the given device (defaulting to the selected one).
So long as the keyboard is grabbed, all keyboard events will be delivered
to emacs -- it is not possible for other X clients to eavesdrop on them.
Ungrab the keyboard with `x-ungrab-keyboard' (use an unwind-protect).
Returns t if the grab is successful, nil otherwise.
*/
       (device))
{
  struct device *d = decode_x_device (device);
  Window w = XtWindow (FRAME_X_TEXT_WIDGET (device_selected_frame (d)));
  Display *dpy = DEVICE_X_DISPLAY (d);
  Status status;
  XSync (dpy, False);
  status = XGrabKeyboard (dpy, w, True,
			  /* I don't really understand sync-vs-async
			     grabs, but this is what xterm does. */
			  GrabModeAsync, GrabModeAsync,
			  /* Use the timestamp of the last user action
			     read by emacs proper; xterm uses CurrentTime
			     but there's a comment that says "wrong"...
			     (Despite the name this is the time of the
			     last key or mouse event.) */
			  DEVICE_X_MOUSE_TIMESTAMP (d));
  if (status == GrabSuccess)
    {
      /* The XUngrabKeyboard should generate a FocusIn back to this
         window but it doesn't unless we explicitly set focus to the
         window first (which should already have it.  The net result
         is that without this call when x-ungrab-keyboard is called
         the selected frame ends up not having focus. */
      XSetInputFocus (dpy, w, RevertToParent, DEVICE_X_MOUSE_TIMESTAMP (d));
      return Qt;
    }
  else
    return Qnil;
}

DEFUN ("x-ungrab-keyboard", Fx_ungrab_keyboard, 0, 1, 0, /*
Release a keyboard grab made with `x-grab-keyboard'.
*/
       (device))
{
  Display *dpy = get_x_display (device);
  XUngrabKeyboard (dpy, CurrentTime);
  return Qnil;
}

DEFUN ("x-get-font-path", Fx_get_font_path, 0, 1, 0, /*
Get the X Server's font path.

See also `x-set-font-path'.
*/
       (device))
{
  Display *dpy = get_x_display (device);
  int ndirs_return;
  const char **directories = (const char **) XGetFontPath (dpy, &ndirs_return);
  Lisp_Object font_path = Qnil;

  if (!directories)
    signal_simple_error ("Can't get X font path", device);

  while (ndirs_return--)
      font_path = Fcons (build_ext_string (directories[ndirs_return],
                                           Qfile_name),
			 font_path);

  return font_path;
}

DEFUN ("x-set-font-path", Fx_set_font_path, 1, 2, 0, /*
Set the X Server's font path to FONT-PATH.

There is only one font path per server, not one per client.  Use this
sparingly.  It uncaches all of the X server's font information.

Font directories should end in the path separator and should contain
a file called fonts.dir usually created with the program mkfontdir.

Setting the FONT-PATH to nil tells the X server to use the default
font path.

See also `x-get-font-path'.
*/
       (font_path, device))
{
  Display *dpy = get_x_display (device);
  Lisp_Object path_entry;
  const char **directories;
  int i=0,ndirs=0;

  EXTERNAL_LIST_LOOP (path_entry, font_path)
    {
      CHECK_STRING (XCAR (path_entry));
      ndirs++;
    }

  directories = alloca_array (const char *, ndirs);

  EXTERNAL_LIST_LOOP (path_entry, font_path)
    {
      LISP_STRING_TO_EXTERNAL (XCAR (path_entry), directories[i++], Qfile_name);
    }

  expect_x_error (dpy);
  XSetFontPath (dpy, (char **) directories, ndirs);
  signal_if_x_error (dpy, 1/*resumable_p*/);

  return Qnil;
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_device_x (void)
{
  DEFSUBR (Fx_debug_mode);
  DEFSUBR (Fx_get_resource);
  DEFSUBR (Fx_get_resource_prefix);
  DEFSUBR (Fx_put_resource);

  DEFSUBR (Fdefault_x_device);
  DEFSUBR (Fx_display_visual_class);
  DEFSUBR (Fx_display_visual_depth);
  DEFSUBR (Fx_server_vendor);
  DEFSUBR (Fx_server_version);
  DEFSUBR (Fx_valid_keysym_name_p);
  DEFSUBR (Fx_keysym_hash_table);
  DEFSUBR (Fx_keysym_on_keyboard_p);
  DEFSUBR (Fx_keysym_on_keyboard_sans_modifiers_p);

  DEFSUBR (Fx_grab_pointer);
  DEFSUBR (Fx_ungrab_pointer);
  DEFSUBR (Fx_grab_keyboard);
  DEFSUBR (Fx_ungrab_keyboard);

  DEFSUBR (Fx_get_font_path);
  DEFSUBR (Fx_set_font_path);

  defsymbol (&Qx_error, "x-error");
  defsymbol (&Qinit_pre_x_win, "init-pre-x-win");
  defsymbol (&Qinit_post_x_win, "init-post-x-win");
}

void
reinit_console_type_create_device_x (void)
{
  /* Initialize variables to speed up X resource interactions */
  const char *valid_resource_chars =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
  while (*valid_resource_chars)
    valid_resource_char_p[(unsigned int) (*valid_resource_chars++)] = 1;

  name_char_dynarr  = Dynarr_new (char);
  class_char_dynarr = Dynarr_new (char);
}

void
console_type_create_device_x (void)
{
  reinit_console_type_create_device_x ();
  CONSOLE_HAS_METHOD (x, init_device);
  CONSOLE_HAS_METHOD (x, finish_init_device);
  CONSOLE_HAS_METHOD (x, mark_device);
  CONSOLE_HAS_METHOD (x, delete_device);
  CONSOLE_HAS_METHOD (x, device_system_metrics);
}

void
reinit_vars_of_device_x (void)
{
  error_expected = 0;
  error_occurred = 0;

  in_resource_setting = 0;
}

void
vars_of_device_x (void)
{
  reinit_vars_of_device_x ();

  DEFVAR_LISP ("x-emacs-application-class", &Vx_emacs_application_class /*
The X application class of the XEmacs process.
This controls, among other things, the name of the `app-defaults' file
that XEmacs will use.  For changes to this variable to take effect, they
must be made before the connection to the X server is initialized, that is,
this variable may only be changed before emacs is dumped, or by setting it
in the file lisp/term/x-win.el.

If this variable is nil before the connection to the X server is first
initialized (which it is by default), the X resource database will be
consulted and the value will be set according to whether any resources
are found for the application class `XEmacs'.  If the user has set any
resources for the XEmacs application class, the XEmacs process will use
the application class `XEmacs'.  Otherwise, the XEmacs process will use
the application class `Emacs' which is backwards compatible to previous
XEmacs versions but may conflict with resources intended for GNU Emacs.
*/ );
  Vx_emacs_application_class = Qnil;

  DEFVAR_LISP ("x-initial-argv-list", &Vx_initial_argv_list /*
You don't want to know.
This is used during startup to communicate the remaining arguments in
`command-line-args-left' to the C code, which passes the args to
the X initialization code, which removes some args, and then the
args are placed back into `x-initial-arg-list' and thence into
`command-line-args-left'.  Perhaps `command-line-args-left' should
just reside in C.
*/ );
  Vx_initial_argv_list = Qnil;

#if defined(MULE) && (defined(LWLIB_MENUBARS_MOTIF) || defined(HAVE_XIM) || defined (USE_XFONTSET))
  DEFVAR_LISP ("x-app-defaults-directory", &Vx_app_defaults_directory /*
Used by the Lisp code to communicate to the low level X initialization
where the localized init files are.
*/ );
  Vx_app_defaults_directory = Qnil;
#endif

  Fprovide (Qx);

  staticpro (&Vdefault_x_device);
  Vdefault_x_device = Qnil;
}
