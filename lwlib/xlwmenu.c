/* Implements a lightweight menubar widget.
   Copyright (C) 1992, 1993, 1994 Lucid, Inc.
   Copyright (C) 1995 Tinker Systems and INS Engineering Corp.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Created by devin@lucid.com */

#include <config.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <sys/types.h>
#include <limits.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <X11/IntrinsicP.h>
#include <X11/ShellP.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include <X11/bitmaps/gray>

#ifdef NEED_MOTIF
#include <Xm/Xm.h>
#if XmVersion < 1002 /* 1.1 or ancient */
#undef XmFONTLIST_DEFAULT_TAG
#define XmFONTLIST_DEFAULT_TAG XmSTRING_DEFAULT_CHARSET
#endif /* XmVersion < 1.2 */
#endif
#include "xlwmenuP.h"

#ifdef USE_DEBUG_MALLOC
#include <dmalloc.h>
#endif

/* simple, naive integer maximum */
#ifndef max
#define max(a,b) ((a)>(b)?(a):(b))
#endif

static char
xlwMenuTranslations [] =
"<BtnDown>:	start()\n\
<BtnMotion>:	drag()\n\
<BtnUp>:	select()\n\
";

extern Widget lw_menubar_widget;

#define offset(field) XtOffset(XlwMenuWidget, field)
static XtResource
xlwMenuResources[] =
{
#ifdef NEED_MOTIF
  /* There are three font list resources, so that we can accept either of
     the resources *fontList: or *font:, and so that we can tell the
     difference between them being specified, and being defaulted to a
     font from the XtRString specified here. */
  {XmNfontList,  XmCFontList, XmRFontList, sizeof(XmFontList),
     offset(menu.font_list),  XtRImmediate, (XtPointer)0},
  {XtNfont,      XtCFont,     XmRFontList, sizeof(XmFontList),
     offset(menu.font_list_2),XtRImmediate, (XtPointer)0},
  {XmNfontList,  XmCFontList, XmRFontList, sizeof(XmFontList),
     offset(menu.fallback_font_list),
     /* We must use an iso8859-1 font here, or people without $LANG set lose.
	It's fair to assume that those who do have $LANG set also have the
	*fontList resource set, or at least know how to deal with this. */
     XtRString, (XtPointer) "-*-helvetica-bold-r-*-*-*-120-*-*-*-*-iso8859-1"},
#else
  {XtNfont,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
     offset(menu.font), XtRString, (XtPointer) "XtDefaultFont"},
# ifdef USE_XFONTSET
  /* #### Consider using the same method as for Motif; see the comment in
     XlwMenuInitialize(). */
  {XtNfontSet,  XtCFontSet, XtRFontSet, sizeof(XFontSet),
     offset(menu.font_set), XtRString, (XtPointer) "XtDefaultFontSet"},
# endif
#endif
  {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(menu.foreground), XtRString, (XtPointer) "XtDefaultForeground"},
  {XtNbuttonForeground, XtCButtonForeground, XtRPixel, sizeof(Pixel),
     offset(menu.button_foreground), XtRString, (XtPointer) "XtDefaultForeground"},
  {XtNhighlightForeground, XtCHighlightForeground, XtRPixel, sizeof(Pixel),
     offset(menu.highlight_foreground), XtRString, (XtPointer) "XtDefaultForeground"},
  {XtNtitleForeground, XtCTitleForeground, XtRPixel, sizeof(Pixel),
     offset(menu.title_foreground), XtRString, (XtPointer) "XtDefaultForeground"},
  {XtNmargin, XtCMargin, XtRDimension,  sizeof(Dimension),
     offset(menu.margin), XtRImmediate, (XtPointer)2},
  {XmNmarginWidth, XmCMarginWidth, XmRHorizontalDimension, sizeof(Dimension),
     offset(menu.horizontal_margin), XtRImmediate, (XtPointer)2},
  {XmNmarginHeight, XmCMarginHeight, XmRVerticalDimension, sizeof(Dimension),
     offset(menu.vertical_margin), XtRImmediate, (XtPointer)1},
  {XmNspacing, XmCSpacing, XmRHorizontalDimension,  sizeof(Dimension),
     offset(menu.column_spacing), XtRImmediate, (XtPointer)4},
  {XmNindicatorSize, XmCIndicatorSize, XtRDimension,  sizeof(Dimension),
     offset(menu.indicator_size), XtRImmediate, (XtPointer)0},
#if 0
  {XmNshadowThickness, XmCShadowThickness, XmRHorizontalDimension,
     sizeof (Dimension), offset (menu.shadow_thickness),
     XtRImmediate, (XtPointer) 2},
#else
  {XmNshadowThickness, XmCShadowThickness, XtRDimension,
     sizeof (Dimension), offset (menu.shadow_thickness),
     XtRImmediate, (XtPointer) 2},
#endif
  {XmNselectColor, XmCSelectColor, XtRPixel, sizeof (Pixel),
     offset (menu.select_color), XtRImmediate, (XtPointer)-1},
  {XmNtopShadowColor, XmCTopShadowColor, XtRPixel, sizeof (Pixel),
     offset (menu.top_shadow_color), XtRImmediate, (XtPointer)-1},
  {XmNbottomShadowColor, XmCBottomShadowColor, XtRPixel, sizeof (Pixel),
     offset (menu.bottom_shadow_color), XtRImmediate, (XtPointer)-1},
  {XmNtopShadowPixmap, XmCTopShadowPixmap, XtRPixmap, sizeof (Pixmap),
     offset (menu.top_shadow_pixmap), XtRImmediate, (XtPointer)None},
  {XmNbottomShadowPixmap, XmCBottomShadowPixmap, XtRPixmap, sizeof (Pixmap),
     offset (menu.bottom_shadow_pixmap), XtRImmediate, (XtPointer)None},

  {XtNopen, XtCCallback, XtRCallback, sizeof(XtPointer),
     offset(menu.open), XtRCallback, (XtPointer)NULL},
  {XtNselect, XtCCallback, XtRCallback, sizeof(XtPointer),
     offset(menu.select), XtRCallback, (XtPointer)NULL},
  {XtNmenu, XtCMenu, XtRPointer, sizeof(XtPointer),
     offset(menu.contents), XtRImmediate, (XtPointer)NULL},
  {XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
     offset(menu.cursor_shape), XtRString, (XtPointer) "right_ptr"},
  {XtNhorizontal, XtCHorizontal, XtRInt, sizeof(int),
     offset(menu.horizontal), XtRImmediate, (XtPointer)True},
  {XtNuseBackingStore, XtCUseBackingStore, XtRBoolean, sizeof (Boolean),
     offset (menu.use_backing_store), XtRImmediate, (XtPointer)False},
  {XtNbounceDown, XtCBounceDown, XtRBoolean, sizeof (Boolean),
     offset (menu.bounce_down), XtRImmediate, (XtPointer)True},
  {XtNresourceLabels, XtCResourceLabels, XtRBoolean, sizeof (Boolean),
     offset (menu.lookup_labels), XtRImmediate, (XtPointer)False},
};
#undef offset

static Boolean XlwMenuSetValues (Widget current, Widget request, Widget new,
				 ArgList args, Cardinal *num_args);
static void XlwMenuRealize (Widget w, Mask *valueMask,
			    XSetWindowAttributes *attributes);
static void XlwMenuRedisplay (Widget w, XEvent *ev, Region region);
static void XlwMenuResize (Widget w);
static void XlwMenuInitialize (Widget request, Widget new, ArgList args,
			       Cardinal *num_args);
static void XlwMenuDestroy (Widget w);
static void XlwMenuClassInitialize (void);
static void Start (Widget w, XEvent *ev, String *params, Cardinal *num_params);
static void Drag  (Widget w, XEvent *ev, String *params, Cardinal *num_params);
static void Select(Widget w, XEvent *ev, String *params, Cardinal *num_params);

#ifdef NEED_MOTIF
static XFontStruct *default_font_of_font_list (XmFontList);
#endif

static XtActionsRec
xlwMenuActionsList [] =
{
  {"start",	Start},
  {"drag",	Drag},
  {"select",	Select},
};

#define SuperClass ((CoreWidgetClass)&coreClassRec)

XlwMenuClassRec xlwMenuClassRec =
{
  {  /* CoreClass fields initialization */
    (WidgetClass) SuperClass,		/* superclass		  */
    "XlwMenu",				/* class_name		  */
    sizeof(XlwMenuRec),			/* size			  */
    XlwMenuClassInitialize,		/* class_initialize	  */
    NULL,				/* class_part_initialize  */
    FALSE,				/* class_inited		  */
    XlwMenuInitialize,			/* initialize		  */
    NULL,				/* initialize_hook	  */
    XlwMenuRealize,			/* realize		  */
    xlwMenuActionsList,			/* actions		  */
    XtNumber(xlwMenuActionsList),	/* num_actions		  */
    xlwMenuResources,			/* resources		  */
    XtNumber(xlwMenuResources),		/* resource_count	  */
    NULLQUARK,				/* xrm_class		  */
    TRUE,				/* compress_motion	  */
    XtExposeCompressMaximal,		/* compress_exposure	  */
    TRUE,				/* compress_enterleave    */
    FALSE,				/* visible_interest	  */
    XlwMenuDestroy,			/* destroy		  */
    XlwMenuResize,			/* resize		  */
    XlwMenuRedisplay,			/* expose		  */
    XlwMenuSetValues,			/* set_values		  */
    NULL,				/* set_values_hook	  */
    XtInheritSetValuesAlmost,		/* set_values_almost	  */
    NULL,				/* get_values_hook	  */
    NULL, /* #### - should this be set for grabs? accept_focus	  */
    XtVersion,				/* version		  */
    NULL,				/* callback_private	  */
    xlwMenuTranslations,		/* tm_table		  */
    XtInheritQueryGeometry,		/* query_geometry	  */
    XtInheritDisplayAccelerator,	/* display_accelerator	  */
    NULL				/* extension		  */
  },  /* XlwMenuClass fields initialization */
  {
    0					/* dummy */
  },
};

WidgetClass xlwMenuWidgetClass = (WidgetClass) &xlwMenuClassRec;

extern int lw_menu_accelerate;

/* Utilities */
#if 0 /* Apparently not used anywhere */

static char *
safe_strdup (char *s)
{
  char *result;
  if (! s) return 0;
  result = (char *) malloc (strlen (s) + 1);
  if (! result)
    return 0;
  strcpy (result, s);
  return result;
}

#endif /* 0 */

/* Replacement for XAllocColor() that tries to return the nearest
   available color if the colormap is full.  From FSF Emacs. */

static int
allocate_nearest_color (Display *display, Colormap screen_colormap,
		        XColor *color_def)
{
  int status = XAllocColor (display, screen_colormap, color_def);
  if (status)
    return status;

    {
      /* If we got to this point, the colormap is full, so we're
	 going to try to get the next closest color.
	 The algorithm used is a least-squares matching, which is
	 what X uses for closest color matching with StaticColor visuals.  */

      int nearest, x;
      unsigned long nearest_delta = ULONG_MAX;

      int no_cells = XDisplayCells (display, XDefaultScreen (display));
      /* Don't use alloca here because lwlib doesn't have the
         necessary configuration information that src does. */
      XColor *cells = (XColor *) malloc (sizeof (XColor) * no_cells);

      for (x = 0; x < no_cells; x++)
	cells[x].pixel = x;

      XQueryColors (display, screen_colormap, cells, no_cells);

      for (nearest = 0, x = 0; x < no_cells; x++)
	{
	  long dred   = (color_def->red   >> 8) - (cells[x].red   >> 8);
	  long dgreen = (color_def->green >> 8) - (cells[x].green >> 8);
	  long dblue  = (color_def->blue  >> 8) - (cells[x].blue  >> 8);
	  unsigned long delta = dred * dred + dgreen * dgreen + dblue * dblue;

	  if (delta < nearest_delta)
	    {
	      nearest = x;
	      nearest_delta = delta;
	    }
	}
      color_def->red   = cells[nearest].red;
      color_def->green = cells[nearest].green;
      color_def->blue  = cells[nearest].blue;
      free (cells);
      return XAllocColor (display, screen_colormap, color_def);
    }
}

static void
push_new_stack (XlwMenuWidget mw, widget_value *val)
{
  if (!mw->menu.new_stack)
    {
      mw->menu.new_stack_length = 10;
      mw->menu.new_stack =
	(widget_value**)XtCalloc (mw->menu.new_stack_length,
				  sizeof (widget_value*));
    }
  else if (mw->menu.new_depth == mw->menu.new_stack_length)
    {
      mw->menu.new_stack_length *= 2;
      mw->menu.new_stack =
	(widget_value**)XtRealloc ((char *)mw->menu.new_stack,
				   mw->menu.new_stack_length *
				   sizeof (widget_value*));
    }
  mw->menu.new_stack [mw->menu.new_depth++] = val;
}

static void
pop_new_stack_if_no_contents (XlwMenuWidget mw)
{
  if (mw->menu.new_depth &&
      !mw->menu.new_stack [mw->menu.new_depth - 1]->contents)
    mw->menu.new_depth -= 1;
}

static void
make_old_stack_space (XlwMenuWidget mw, int n)
{
  if (!mw->menu.old_stack)
    {
      mw->menu.old_stack_length = max (10, n);
      mw->menu.old_stack =
	(widget_value**)XtCalloc (mw->menu.old_stack_length,
				  sizeof (widget_value*));
    }
  else if (mw->menu.old_stack_length < n)
    {
      while (mw->menu.old_stack_length < n)
      mw->menu.old_stack_length *= 2;

      mw->menu.old_stack =
	(widget_value**)XtRealloc ((char *)mw->menu.old_stack,
				   mw->menu.old_stack_length *
				   sizeof (widget_value*));
    }
}

static Boolean
close_to_reference_time (Widget w, Time reference_time, XEvent *ev)
{
  return
    reference_time &&
    (ev->xbutton.time - reference_time
     < (Time) XtGetMultiClickTime (XtDisplay (w)));
}

/* Size code */
static int
string_width (XlwMenuWidget mw,
#ifdef NEED_MOTIF
	      XmString s
#else
	      char *s
#endif
	      )
{
#ifdef NEED_MOTIF
  Dimension width, height;
  XmStringExtent (mw->menu.font_list, s, &width, &height);
  return width;
#else
# ifdef USE_XFONTSET
  XRectangle ri, rl;
  XmbTextExtents (mw->menu.font_set, s, strlen (s), &ri, &rl);
  return rl.width;
# else
  XCharStruct xcs;
  int drop;
  XTextExtents (mw->menu.font, s, strlen (s), &drop, &drop, &drop, &xcs);
  return xcs.width;
# endif /* USE_XFONTSET */
#endif
}

static char massaged_resource_char[256];

static void
initialize_massaged_resource_char (void)
{
  int j;
  for (j = 0; j < (int) sizeof (massaged_resource_char); j++)
    {
      if ((j >= 'a' && j <= 'z') ||
	  (j >= 'A' && j <= 'Z') ||
	  (j >= '0' && j <= '9') ||
	  (j == '_')             ||
	  (j >= 0xa0))
	massaged_resource_char[j] = (char) j;
    }
  massaged_resource_char ['_'] = '_';
  massaged_resource_char ['+'] = 'P'; /* Convert C++ to cPP */
  massaged_resource_char ['.'] = '_'; /* Convert Buffers... to buffers___ */
}

static int
string_width_u (XlwMenuWidget mw,
#ifdef NEED_MOTIF
	      XmString string
#else
	      char *string
#endif
	      )
{
#ifdef NEED_MOTIF
  Dimension width, height;
  XmString newstring;
#else
# ifdef USE_XFONTSET
  XRectangle ri, rl;
# else /* ! USE_XFONTSET */
  XCharStruct xcs;
  int drop;
# endif
#endif
  char* newchars;
  int charslength;
  char *chars;
  int i, j;

#ifdef NEED_MOTIF
  chars = "";
  if (!XmStringGetLtoR (string, XmFONTLIST_DEFAULT_TAG, &chars))
    chars = "";
#else
  chars = string;
#endif
  charslength = strlen (chars);
  newchars = (char *) alloca (charslength + 1);

  for (i = j = 0; chars[i] && (j < charslength); i++)
    if (chars[i]=='%'&&chars[i+1]=='_')
	    i++;
    else
	    newchars[j++] = chars[i];
  newchars[j] = '\0';

#ifdef NEED_MOTIF
  newstring = XmStringLtoRCreate (newchars, XmFONTLIST_DEFAULT_TAG);
  XmStringExtent (mw->menu.font_list, newstring, &width, &height);
  XmStringFree (newstring);
  XtFree (chars);
  return width;
#else
# ifdef USE_XFONTSET
  XmbTextExtents (mw->menu.font_set, newchars, j, &ri, &rl);
  return rl.width;
# else /* ! USE_XFONTSET */
  XTextExtents (mw->menu.font, newchars, j, &drop, &drop, &drop, &xcs);
  return xcs.width;
# endif /* USE_XFONTSET */
#endif
}

static void
massage_resource_name (const char *in, char *out)
{
  /* Turn a random string into something suitable for using as a resource.
     For example:

     "Kill Buffer"		->	"killBuffer"
     "Find File..."		->	"findFile___"
     "Search and Replace..."	->	"searchAndReplace___"
     "C++ Mode Commands"        ->      "cppModeCommands"

     Valid characters in a resource NAME component are:  a-zA-Z0-9_
   */

#ifdef PRINT_XLWMENU_RESOURCE_CONVERSIONS
  /* Compile with -DPRINT_XLWMENU_RESOURCE_CONVERSIONS to generate a
     translation file for menu localizations. */
  char *save_in = in, *save_out = out;
#endif

  Boolean firstp = True;
  while (*in)
    {
      if (*in == '%' && *(in + 1) == '_')
	in += 2;
      else
	{
	  char ch;

	  if (*in == '%' && *(in + 1) == '%')
	    in++;
	  ch = massaged_resource_char[(unsigned char) *in++];
	  if (ch)
	    {
	      int int_ch = (int) (unsigned char) ch;
	      *out++ = firstp ? tolower (int_ch) : toupper (int_ch);
	      firstp = False;
	      while ((ch = massaged_resource_char[(unsigned char) *in++])
		     != '\0')
		*out++ = ch;
	      if (!*(in-1))		/* Overshot the NULL byte? */
		break;
	    }
	}
    }
  *out = 0;

#ifdef PRINT_XLWMENU_RESOURCE_CONVERSIONS
  printf ("! Emacs*XlwMenu.%s.labelString:\t%s\n", save_out, save_in);
  printf (  "Emacs*XlwMenu.%s.labelString:\n",     save_out);
#endif
}

static XtResource
nameResource[] =
{
  { "labelString", "LabelString", XtRString, sizeof(String),
    0, XtRImmediate, 0 }
};

/* This function searches STRING for parameter inserts of the form:
       %[padding]1
   padding is either space (' ') or dash ('-') meaning
   padding to the left or right of the inserted parameter.
   In essence, all %1 strings are replaced by VALUE in the return value.
   The caller is expected to free the return value using XtFree().
   %% means insert one % (like printf).
   %1 means insert VALUE.
   %-1 means insert VALUE followed by one space. The latter is
   not inserted if VALUE is a zero length string.
*/
static char*
parameterize_string (const char *string, const char *value)
{
  const char *percent;
  char *result;
  unsigned int done = 0;
  unsigned int ntimes;

  if (!string)
    {
      result = XtMalloc(1);
      result[0] = '\0';
      return result;
    }

  if (!value)
    value = "";

  for (ntimes = 1, percent = string;
       (percent = strchr (percent, '%'));
       ntimes++)
    percent++;

  result = XtMalloc ((ntimes * strlen(value)) + strlen(string) + 4);
  result[0] = '\0';

  while ((percent = strchr (string, '%')))
    {
      unsigned int left_pad;
      unsigned int right_pad;
      const char *p;

      if (percent[1] == '%')
	{	/* it's a real % */
	  strncat (result, string, 1 + percent - string); /* incl % */
	  string = &percent[2];	/* after the second '%' */
	  continue;		/* with the while() loop */
	}

      left_pad = 0;
      right_pad = 0;

      for (p = &percent[1]; /* test *p inside the loop */ ; p++)
	{
	  if (*p == ' ')
	    {			/* left pad */
	      left_pad++;
	    }
	  else if (*p == '-')
	    {			/* right pad */
	      right_pad++;
	    }
	  else if (*p == '1')
	    {			/* param and terminator */
	      strncat (result, string, percent - string);
	      if (value[0] != '\0')
		{
		  unsigned int i;
		  for (i = 0; i < left_pad; i++)
		    strcat (result, " ");
		  strcat (result, value);
		  for (i = 0; i < right_pad; i++)
		    strcat (result, " ");
		}
	      string = &p[1];	/* after the '1' */
	      done++;		/* no need to do old way */
	      break;		/* out of for() loop */
	    }
	  else
	    {			/* bogus, copy the format as is */
				/* out of for() loop */
	      strncat (result, string, 1 + p - string);
	      string = (*p ? &p[1] : p);
	      break;
	    }
	}
    }

  /* Copy the tail of the string */
  strcat (result, string);

  /* If we have not processed a % string, and we have a value, tail it. */
  if (!done && value[0] != '\0')
    {
      strcat (result, " ");
      strcat (result, value);
    }

  return result;
}

#ifdef NEED_MOTIF

static XmString
resource_widget_value (XlwMenuWidget mw, widget_value *val)
{
  if (!val->toolkit_data)
    {
      char *resourced_name = NULL;
      char *converted_name, *str;
      XmString complete_name;
      char massaged_name [1024];

      if (mw->menu.lookup_labels)
	{
	  /* Convert value style name into resource style name.
	     eg: "Free Willy" becomes "freeWilly" */
	  massage_resource_name (val->name, massaged_name);

	  /* If we have a value (parameter) see if we can find a "Named"
	     resource. */
	  if (val->value)
	    {
	      char named_name[1024];
	      sprintf (named_name, "%sNamed", massaged_name);
	      XtGetSubresources ((Widget) mw,
				 (XtPointer) &resourced_name,
				 named_name, named_name,
				 nameResource, 1, NULL, 0);
	    }

	  /* If nothing yet, try to load from the massaged name. */
	  if (!resourced_name)
	    {
	      XtGetSubresources ((Widget) mw,
				 (XtPointer) &resourced_name,
				 massaged_name, massaged_name,
				 nameResource, 1, NULL, 0);
	    }
	} /* if (mw->menu.lookup_labels) */

      /* Still nothing yet, use the name as the value. */
      if (!resourced_name)
	resourced_name = val->name;

      /* Parameterize the string. */
      converted_name = parameterize_string (resourced_name, val->value);

      /* nuke newline characters to prevent menubar screwups */
      for ( str = converted_name ; *str ; str++ )
	{
	  if (str[0] == '\n') str[0] = ' ';
	}

      /* Improve OSF's bottom line. */
#if (XmVersion >= 1002)
      complete_name = XmStringCreateLocalized (converted_name);
#else
      complete_name = XmStringCreateLtoR (converted_name,
					  XmSTRING_DEFAULT_CHARSET);
#endif
      XtFree (converted_name);

      val->toolkit_data = complete_name;
      val->free_toolkit_data = True;
    }
  return (XmString) val->toolkit_data;
}

/* Unused */
#if 0
/* These two routines should be a separate file..djw */
static char *
xlw_create_localized_string (Widget w,
			     char *name,
			     char **args,
			     unsigned int nargs)
{
  char *string = NULL;
  char *arg = NULL;

  if (nargs > 0)
    arg = args[0];

  XtGetSubresources (w,
		     (XtPointer)&string,
		     name,
		     name,
		     nameResource, 1,
		     NULL, 0);

  if (!string)
    string = name;

  return parameterize_string (string, arg);
}

static XmString
xlw_create_localized_xmstring (Widget w,
			       char *name,
			       char **args,
			       unsigned int nargs)
{
  char *   string = xlw_create_localized_string (w, name, args, nargs);
  XmString xm_string = XmStringCreateLtoR (string, XmSTRING_DEFAULT_CHARSET);
  XtFree (string);
  return xm_string;
}
#endif /* 0 */

#else /* !Motif */

static char*
resource_widget_value (XlwMenuWidget mw, widget_value *val)
{
  if (!val->toolkit_data)
    {
      char *resourced_name = NULL;
      char *complete_name;
      char massaged_name [1024];

      if (mw->menu.lookup_labels)
	{
	  massage_resource_name (val->name, massaged_name);

	  XtGetSubresources ((Widget) mw,
			     (XtPointer) &resourced_name,
			     massaged_name, massaged_name,
			     nameResource, 1, NULL, 0);
	}
      if (!resourced_name)
	resourced_name = val->name;

      complete_name = parameterize_string (resourced_name, val->value);

      val->toolkit_data = complete_name;
      /* nuke newline characters to prevent menubar screwups */
      for ( ; *complete_name ; complete_name++ )
	{
	  if (complete_name[0] == '\n')
	    complete_name[0] = ' ';
	}
      val->free_toolkit_data = True;
    }
  return (char *) val->toolkit_data;
}

#endif /* !Motif */

/* Code for drawing strings. */
static void
string_draw (XlwMenuWidget mw,
	     Window window,
	     int x, int y,
	     GC gc,
#ifdef NEED_MOTIF
	     XmString string
#else
	     char *string
#endif
)
{
#ifdef NEED_MOTIF
  XmStringDraw (XtDisplay (mw), window,
		mw->menu.font_list,
		string, gc,
		x, y,
		1000,	/* ???? width */
		XmALIGNMENT_BEGINNING,
		0, /* ???? layout_direction */
		0);
#else
# ifdef USE_XFONTSET
  XmbDrawString (XtDisplay (mw), window, mw->menu.font_set, gc,
	       x, y + mw->menu.font_ascent, string, strlen (string));
# else
  XDrawString (XtDisplay (mw), window, gc,
	       x, y + mw->menu.font_ascent, string, strlen (string));
# endif /* USE_XFONTSET */

#endif
}

static int
string_draw_range (
	XlwMenuWidget mw,
	Window window,
	int x, int y,
	GC gc,
	char *string,
	int start,
	int end
)
{
#ifdef NEED_MOTIF
	Dimension width, height;
	XmString newstring;
	int c;

	if (end <= start)
		return 0;
	c = string[end];
	string[end] = '\0';
	newstring = XmStringLtoRCreate (&string[start], XmFONTLIST_DEFAULT_TAG);
	XmStringDraw (
		XtDisplay (mw), window,
		mw->menu.font_list,
		newstring, gc,
		x, y,
		1000,	/* ???? width */
		XmALIGNMENT_BEGINNING,
		0, /* ???? layout_direction */
		0
	);
	XmStringExtent (mw->menu.font_list, newstring, &width, &height);
	XmStringFree (newstring);
	string[end] = c;
	return width;
#else
# ifdef USE_XFONTSET
	XRectangle ri, rl;

	if (end <= start)
		return 0;
	XmbDrawString (
		XtDisplay (mw), window, mw->menu.font_set, gc,
		x, y + mw->menu.font_ascent, &string[start], end - start);
	XmbTextExtents (
		mw->menu.font_set, &string[start], end - start, &ri, &rl);
	return rl.width;
# else
	XCharStruct xcs;
	int drop;

	if (end <= start)
		return 0;
	XDrawString (
		XtDisplay (mw), window, gc,
		x, y + mw->menu.font_ascent, &string[start], end - start);
	XTextExtents (
		mw->menu.font, &string[start], end - start,
		&drop, &drop, &drop, &xcs);
	return xcs.width;
# endif
#endif
}

static void
string_draw_u (XlwMenuWidget mw,
	       Window window,
	       int x, int y,
	       GC gc,
#ifdef NEED_MOTIF
	       XmString string
#else
	       char *string
#endif
)
{
  int i, s = 0;
  char *chars;

#ifdef NEED_MOTIF
  chars = "";
  if (!XmStringGetLtoR (string, XmFONTLIST_DEFAULT_TAG, &chars))
    chars = "";
#else
  chars = string;
#endif
  for (i=0; chars[i]; ++i) {
      if (chars[i] == '%' && chars[i+1] == '_') {
	  int w;

	  x += string_draw_range (mw, window, x, y, gc, chars, s, i);
	  w = string_draw_range (mw, window, x, y, gc, chars, i+2, i+3);

	  /* underline next character */
	  XDrawLine (XtDisplay (mw), window, gc, x - 1,
		     y + mw->menu.font_ascent + 1,
		     x + w - 1, y + mw->menu.font_ascent + 1 );
	  x += w;
	  s = i + 3;
	  i += 2;
      }
  }
  x += string_draw_range (mw, window, x, y, gc, chars, s, i);
#ifdef NEED_MOTIF
  XtFree (chars);
#endif
}

static void
binding_draw (XlwMenuWidget mw, Window w, int x, int y, GC gc, char *value)
{
#ifdef NEED_MOTIF
  XmString xm_value = XmStringCreateLtoR(value, XmSTRING_DEFAULT_CHARSET);
  string_draw (mw, w, x, y, gc, xm_value);
  XmStringFree (xm_value);
#else
  string_draw (mw, w, x, y, gc, value);
#endif
}

/* Low level code for drawing 3-D edges. */
static void
shadow_rectangle_draw (Display *dpy,
		       Window window,
		       GC top_gc,
		       GC bottom_gc,
		       int x, int y,
		       unsigned int width,
		       unsigned int height,
		       unsigned int thickness)
{
  XPoint points [4];

  if (!thickness)
    return;

  points [0].x = x;
  points [0].y = y;
  points [1].x = x + width;
  points [1].y = y;
  points [2].x = x + width - thickness;
  points [2].y = y + thickness;
  points [3].x = x;
  points [3].y = y + thickness;
  XFillPolygon (dpy, window, top_gc, points, 4, Convex, CoordModeOrigin);
  points [0].x = x;
  points [0].y = y + thickness;
  points [1].x = x;
  points [1].y = y + height;
  points [2].x = x + thickness;
  points [2].y = y + height - thickness;
  points [3].x = x + thickness;
  points [3].y = y + thickness;
  XFillPolygon (dpy, window, top_gc, points, 4, Convex, CoordModeOrigin);
  points [0].x = x + width;
  points [0].y = y;
  points [1].x = x + width - thickness;
  points [1].y = y + thickness;
  points [2].x = x + width - thickness;
  points [2].y = y + height - thickness;
  points [3].x = x + width;
  points [3].y = y + height - thickness;
  XFillPolygon (dpy, window, bottom_gc, points, 4, Convex, CoordModeOrigin);
  points [0].x = x;
  points [0].y = y + height;
  points [1].x = x + width;
  points [1].y = y + height;
  points [2].x = x + width;
  points [2].y = y + height - thickness;
  points [3].x = x + thickness;
  points [3].y = y + height - thickness;
  XFillPolygon (dpy, window, bottom_gc, points, 4, Convex, CoordModeOrigin);
}

typedef enum e_shadow_type
{
  /* these are Motif compliant */
  SHADOW_BACKGROUND,
  SHADOW_OUT,
  SHADOW_IN,
  SHADOW_ETCHED_OUT,
  SHADOW_ETCHED_IN,
  SHADOW_ETCHED_OUT_DASH,
  SHADOW_ETCHED_IN_DASH,
  SHADOW_SINGLE_LINE,
  SHADOW_DOUBLE_LINE,
  SHADOW_SINGLE_DASHED_LINE,
  SHADOW_DOUBLE_DASHED_LINE,
  SHADOW_NO_LINE,
  /* these are all non-Motif */
  SHADOW_DOUBLE_ETCHED_OUT,
  SHADOW_DOUBLE_ETCHED_IN,
  SHADOW_DOUBLE_ETCHED_OUT_DASH,
  SHADOW_DOUBLE_ETCHED_IN_DASH
} shadow_type;

static void
shadow_draw (XlwMenuWidget mw,
	     Window window,
	     int x, int y,
	     unsigned int width,
	     unsigned int height,
	     shadow_type type)
{
  Display *dpy = XtDisplay (mw);
  GC top_gc;
  GC bottom_gc;
  int thickness = mw->menu.shadow_thickness;
#if 0
  XPoint points [4];
#endif /* 0 */
  Boolean etched = False;

  switch (type)
    {
    case SHADOW_BACKGROUND:
      top_gc = bottom_gc = mw->menu.background_gc;
      break;
    case SHADOW_ETCHED_IN:
      top_gc = mw->menu.shadow_bottom_gc;
      bottom_gc = mw->menu.shadow_top_gc;
      etched = True;
      break;
    case SHADOW_ETCHED_OUT:
      top_gc = mw->menu.shadow_top_gc;
      bottom_gc = mw->menu.shadow_bottom_gc;
      etched = True;
      break;
    case SHADOW_IN:
      top_gc = mw->menu.shadow_bottom_gc;
      bottom_gc = mw->menu.shadow_top_gc;
      break;
    case SHADOW_OUT:
    default:
      top_gc = mw->menu.shadow_top_gc;
      bottom_gc = mw->menu.shadow_bottom_gc;
      break;
    }

  if (etched)
    {
      unsigned int half = thickness/2;
      shadow_rectangle_draw (dpy,
			     window,
			     top_gc,
			     top_gc,
			     x, y,
			     width - half, height - half,
			     thickness - half);
      shadow_rectangle_draw (dpy,
			     window,
			     bottom_gc,
			     bottom_gc,
			     x + half, y + half,
			     width - half , height - half,
			     half);
    }
  else
    {
      shadow_rectangle_draw (dpy,
			     window,
			     top_gc,
			     bottom_gc,
			     x, y,
			     width, height,
			     thickness);
    }
}

static void
arrow_decoration_draw (XlwMenuWidget mw,
		       Window window,
		       int x, int y,
		       unsigned int width,
		       Boolean raised)
{
  Display *dpy = XtDisplay (mw);
  GC top_gc;
  GC bottom_gc;
  GC select_gc;
  int thickness = mw->menu.shadow_thickness;
  XPoint points [4];
  int half_width;
  int length = (int)((double)width * 0.87);
  int thick_med = (int)((double)thickness * 1.73);

  if (width & 0x1)
    half_width = width/2 + 1;
  else
    half_width = width/2;

  select_gc = mw->menu.background_gc;

  if (raised)
    {
      top_gc    = mw->menu.shadow_bottom_gc;
      bottom_gc = mw->menu.shadow_top_gc;
    }
  else
    {
      top_gc    = mw->menu.shadow_top_gc;
      bottom_gc = mw->menu.shadow_bottom_gc;
    }

  /* Fill internal area.  We do this first so that the borders have a
     nice sharp edge.  */
  points [0].x = x + thickness;
  points [0].y = y + thickness;
  points [1].x = x + length - thickness;
  points [1].y = y + half_width;
  points [2].x = x + length - thickness;
  points [2].y = y + half_width + thickness;
  points [3].x = x + thickness;
  points [3].y = y + width - thickness;

  XFillPolygon (dpy,
		window,
		select_gc,
		points,
		4,
		Convex,
		CoordModeOrigin);

  /* left border */
  points [0].x = x;
  points [0].y = y;
  points [1].x = x + thickness;
  points [1].y = y + thick_med;
  points [2].x = x + thickness;
  points [2].y = y + width - thick_med;
  points [3].x = x;
  points [3].y = y + width;

  XFillPolygon (dpy, window, top_gc, points, 4, Convex, CoordModeOrigin);

  /* top border */
  points [0].x = x;
  points [0].y = y + width;
  points [1].x = x + length;
  points [1].y = y + half_width;
  points [2].x = x + length - (thickness + thickness);
  points [2].y = y + half_width;
  points [3].x = x + thickness;
  points [3].y = y + width - thick_med;

  XFillPolygon (dpy, window, bottom_gc, points, 4, Convex, CoordModeOrigin);

  /* bottom shadow */
  points [0].x = x;
  points [0].y = y;
  points [1].x = x + length;
  points [1].y = y + half_width;
  points [2].x = x + length - (thickness + thickness);
  points [2].y = y + half_width;
  points [3].x = x + thickness;
  points [3].y = y + thick_med;

  XFillPolygon (dpy, window, top_gc, points, 4, Convex, CoordModeOrigin);
}

static void
toggle_decoration_draw (XlwMenuWidget mw,
			Window window,
			int x, int y,
			unsigned int width,
			Boolean set)
{
  Display *dpy = XtDisplay (mw);
  int thickness = mw->menu.shadow_thickness;
  shadow_type type;
  GC select_gc = mw->menu.select_gc;

  if (set)
    type = SHADOW_IN;
  else
    type = SHADOW_OUT;

  /* Fill internal area. */
  if (set)
    XFillRectangle (dpy,
		    window,
		    select_gc,
		    x + thickness,
		    y + thickness,
		    width - (2*thickness),
		    width - (2*thickness));

  shadow_draw (mw, window, x, y, width, width, type);
}

static void
radio_decoration_draw (XlwMenuWidget mw,
		       Window window,
		       int x, int y,
		       unsigned int width,
		       Boolean enabled)
{
  Display *dpy = XtDisplay (mw);
  GC top_gc;
  GC bottom_gc;
  GC select_gc = mw->menu.select_gc;
  int thickness = mw->menu.shadow_thickness;
  XPoint points[6];
  int half_width;
#if 0
  int npoints;
#endif /* 0 */

  if (width & 0x1)
    width++;

  half_width = width/2;

  if (enabled)
    {
      top_gc    = mw->menu.shadow_bottom_gc;
      bottom_gc = mw->menu.shadow_top_gc;
    }
  else
    {
      top_gc    = mw->menu.shadow_top_gc;
      bottom_gc = mw->menu.shadow_bottom_gc;
    }

#if 1
  /*  Draw the bottom first, just in case the regions overlap.
      The top should cast the longer shadow. */
  points [0].x = x; /* left corner */
  points [0].y = y + half_width;
  points [1].x = x + half_width; /* bottom corner */
  points [1].y = y + width;
  points [2].x = x + half_width; /* bottom inside corner */
  points [2].y = y + width - thickness;
  points [3].x = x + thickness; /* left inside corner */
  points [3].y = y + half_width;

  XFillPolygon (dpy, window, bottom_gc, points, 4, Convex, CoordModeOrigin);

  points [0].x = x + half_width; /* bottom corner */
  points [0].y = y + width;
  points [1].x = x + width; /* right corner */
  points [1].y = y + half_width;
  points [2].x = x + width - thickness; /* right inside corner */
  points [2].y = y + half_width;
  points [3].x = x + half_width; /* bottom inside corner */
  points [3].y = y + width - thickness;

  XFillPolygon (dpy, window, bottom_gc, points, 4, Convex, CoordModeOrigin);

  points [0].x = x; /* left corner */
  points [0].y = y + half_width;
  points [1].x = x + half_width; /* top corner */
  points [1].y = y;
  points [2].x = x + half_width; /* top inside corner */
  points [2].y = y + thickness;
  points [3].x = x + thickness; /* left inside corner */
  points [3].y = y + half_width;

  XFillPolygon (dpy, window, top_gc, points, 4, Convex, CoordModeOrigin);

  points [0].x = x + half_width; /* top corner */
  points [0].y = y;
  points [1].x = x + width; /* right corner */
  points [1].y = y + half_width;
  points [2].x = x + width - thickness; /* right inside corner */
  points [2].y = y + half_width;
  points [3].x = x + half_width; /* top inside corner */
  points [3].y = y + thickness;

  XFillPolygon (dpy, window, top_gc, points, 4, Convex, CoordModeOrigin);
#else
  /* Draw the bottom first, just in case the regions overlap.
     The top should cast the longer shadow. */
  npoints = 0;
  points [npoints].x = x; /* left corner */
  points [npoints++].y = y + half_width;
  points [npoints].x = x + half_width; /* bottom corner */
  points [npoints++].y = y + width;
  points [npoints].x = x + width; /* right corner */
  points [npoints++].y = y + half_width;
  points [npoints].x = x + width - thickness; /* right inside corner */
  points [npoints++].y = y + half_width;
  points [npoints].x = x + half_width; /* bottom inside corner */
  points [npoints++].y = y + width - thickness;
  points [npoints].x = x + thickness; /* left inside corner */
  points [npoints++].y = y + half_width;

  XFillPolygon (dpy, window, bottom_gc,
		points, npoints, Nonconvex, CoordModeOrigin);

  npoints = 0;

  points [npoints].x = x; /* left corner */
  points [npoints++].y = y + half_width;
  points [npoints].x = x + half_width; /* top corner */
  points [npoints++].y = y;
  points [npoints].x = x + width; /* right corner */
  points [npoints++].y = y + half_width;
  points [npoints].x = x + width - thickness; /* right inside corner */
  points [npoints++].y = y + half_width;
  points [npoints].x = x + half_width; /* top inside corner */
  points [npoints++].y = y + thickness;
  points [npoints].x = x + thickness; /* left inside corner */
  points [npoints++].y = y + half_width;

  XFillPolygon (dpy, window, top_gc, points, npoints, Nonconvex,
		CoordModeOrigin);
#endif


  /* Fill internal area. */
  if (enabled)
    {
      points [0].x = x + thickness;
      points [0].y = y + half_width;
      points [1].x = x + half_width;
      points [1].y = y + thickness;
      points [2].x = x + width - thickness;
      points [2].y = y + half_width;
      points [3].x = x + half_width;
      points [3].y = y + width - thickness;
      XFillPolygon (dpy,
		    window,
		    select_gc,
		    points,
		    4,
		    Convex,
		    CoordModeOrigin);
    }
}

static void
separator_decoration_draw (XlwMenuWidget mw,
			   Window window,
			   int x, int y,
			   unsigned int width,
			   Boolean vertical,
			   shadow_type type)
{
  Display *dpy = XtDisplay (mw);
  GC top_gc;
  GC bottom_gc;
  unsigned int offset = 0;
  unsigned int num_separators = 1;
  unsigned int top_line_thickness = 0;
  unsigned int bottom_line_thickness = 0;
  Boolean dashed = False;

  switch (type)
    {
    case SHADOW_NO_LINE: /* nothing to do */
      return;
    case SHADOW_DOUBLE_LINE:
      num_separators = 2;
    case SHADOW_SINGLE_LINE:
      top_gc = bottom_gc = mw->menu.foreground_gc;
      top_line_thickness = 1;
      break;
    case SHADOW_DOUBLE_DASHED_LINE:
      num_separators = 2;
    case SHADOW_SINGLE_DASHED_LINE:
      top_gc = bottom_gc = mw->menu.foreground_gc;
      top_line_thickness = 1;
      dashed = True;
      break;
    case SHADOW_DOUBLE_ETCHED_OUT_DASH:
      num_separators = 2;
    case SHADOW_ETCHED_OUT_DASH:
      top_gc = mw->menu.shadow_top_gc;
      bottom_gc = mw->menu.shadow_bottom_gc;
      top_line_thickness = mw->menu.shadow_thickness/2;
      bottom_line_thickness = mw->menu.shadow_thickness - top_line_thickness;
      dashed = True;
      break;
    case SHADOW_DOUBLE_ETCHED_IN_DASH:
      num_separators = 2;
    case SHADOW_ETCHED_IN_DASH:
      top_gc = mw->menu.shadow_bottom_gc;
      bottom_gc = mw->menu.shadow_top_gc;
      top_line_thickness = mw->menu.shadow_thickness/2;
      bottom_line_thickness = mw->menu.shadow_thickness - top_line_thickness;
      dashed = True;
      break;
    case SHADOW_DOUBLE_ETCHED_OUT:
      num_separators = 2;
    case SHADOW_ETCHED_OUT:
      top_gc = mw->menu.shadow_top_gc;
      bottom_gc = mw->menu.shadow_bottom_gc;
      top_line_thickness = mw->menu.shadow_thickness/2;
      bottom_line_thickness = mw->menu.shadow_thickness - top_line_thickness;
      break;
    case SHADOW_DOUBLE_ETCHED_IN:
      num_separators = 2;
    case SHADOW_ETCHED_IN:
    default:
      top_gc = mw->menu.shadow_bottom_gc;
      bottom_gc = mw->menu.shadow_top_gc;
      top_line_thickness = mw->menu.shadow_thickness/2;
      bottom_line_thickness = mw->menu.shadow_thickness - top_line_thickness;
      break;
    }

  if (dashed)
    {
      XGCValues values;
      values.line_style = LineOnOffDash;
      if (top_line_thickness > 0)
	XChangeGC (dpy, top_gc, GCLineStyle, &values);
      if (bottom_line_thickness > 0 && bottom_gc != top_gc)
	XChangeGC (dpy, bottom_gc, GCLineStyle, &values);
    }

  while (num_separators--)
    {
      unsigned int i;
      for (i = 0; i < top_line_thickness; i++)
	XDrawLine (dpy, window, top_gc, x, y + i, x + width, y + i);

      for (i = 0; i < bottom_line_thickness; i++)
	XDrawLine (dpy, window, bottom_gc,
		   x, y + top_line_thickness + offset + i,
		   x + width, y + top_line_thickness + offset + i);
      y += (top_line_thickness + offset + bottom_line_thickness + 1);
    }

  if (dashed)
    {
      XGCValues values;
      values.line_style = LineSolid;
      if (top_line_thickness > 0)
	XChangeGC (dpy, top_gc, GCLineStyle, &values);
      if (bottom_line_thickness > 0 && bottom_gc != top_gc)
	XChangeGC (dpy, bottom_gc, GCLineStyle, &values);
    }
}

#define SLOPPY_TYPES 0		/* 0=off, 1=error check, 2=easy to please */
#if SLOPPY_TYPES
#if SLOPPY_TYPES < 2

static char *wv_types[] =
{
  "UNSPECIFIED",
  "BUTTON",
  "TOGGLE",
  "RADIO",
  "TEXT",
  "SEPARATOR",
  "CASCADE",
  "PUSHRIGHT",
  "INCREMENTAL"
};

static void
print_widget_value (widget_value *wv, int just_one, int depth)
{
  char d [200];
  int i;
  for (i = 0; i < depth; i++)
    d[i] = ' ';
  d[depth]=0;
  if (!wv)
    {
      printf ("%s(null widget value pointer)\n", d);
      return;
    }
  printf ("%stype:    %s\n", d, wv_types [wv->type]);
#if 0
  printf ("%sname:    %s\n", d, (wv->name ? wv->name : "(null)"));
#else
  if (wv->name)  printf ("%sname:    %s\n", d, wv->name);
#endif
  if (wv->value) printf ("%svalue:   %s\n", d, wv->value);
  if (wv->key)   printf ("%skey:     %s\n", d, wv->key);
  printf ("%senabled: %d\n", d, wv->enabled);
  if (wv->contents)
    {
      printf ("\n%scontents: \n", d);
      print_widget_value (wv->contents, 0, depth + 5);
    }
  if (!just_one && wv->next)
    {
      printf ("\n");
      print_widget_value (wv->next, 0, depth);
    }
}
#endif /* SLOPPY_TYPES < 2 */

static Boolean
all_dashes_p (char *s)
{
  char *p;
  if (!s || s[0] == '\0')
    return False;
  for (p = s; *p == '-'; p++);

  if (*p == '!' || *p == '\0')
    return True;
  return False;
}
#endif /* SLOPPY_TYPES */

static widget_value_type
menu_item_type (widget_value *val)
{
  if (val->type != UNSPECIFIED_TYPE)
    return val->type;
#if SLOPPY_TYPES
  else if (all_dashes_p (val->name))
    return SEPARATOR_TYPE;
  else if (val->name && val->name[0] == '\0') /* push right */
    return PUSHRIGHT_TYPE;
  else if (val->contents) /* cascade */
    return CASCADE_TYPE;
  else if (val->call_data) /* push button */
    return BUTTON_TYPE;
  else
    return TEXT_TYPE;
#else
  else
    abort();
  return UNSPECIFIED_TYPE; /* Not reached */
#endif
}

static void
label_button_size (XlwMenuWidget mw,
		   widget_value *val,
		   Boolean in_menubar,
		   unsigned int *toggle_width,
		   unsigned int *label_width,
		   unsigned int *bindings_width,
		   unsigned int *height)
{
  *height = (mw->menu.font_ascent + mw->menu.font_descent +
	     2 * mw->menu.vertical_margin +
	     2 * mw->menu.shadow_thickness);
  /* no left column decoration */
  *toggle_width = mw->menu.horizontal_margin + mw->menu.shadow_thickness;

  *label_width  = string_width_u (mw, resource_widget_value (mw, val));
  *bindings_width =  mw->menu.horizontal_margin + mw->menu.shadow_thickness;
}

static void
label_button_draw (XlwMenuWidget mw,
		   widget_value *val,
		   Boolean       in_menubar,
		   Boolean       highlighted,
		   Window        window,
		   int x, int y,
		   unsigned int width,
		   unsigned int height,
		   unsigned int label_offset,
		   unsigned int binding_tab)
{
  int y_offset = mw->menu.shadow_thickness + mw->menu.vertical_margin;
  GC gc;

  if (!label_offset)
    label_offset = mw->menu.shadow_thickness + mw->menu.horizontal_margin;

  if (highlighted && (in_menubar || val->contents))
    gc = mw->menu.highlight_gc;
  else if (in_menubar || val->contents)
    gc = mw->menu.foreground_gc;
  else
    gc = mw->menu.title_gc;

  /*  Draw the label string. */
  string_draw_u (mw,
	       window,
	       x + label_offset, y + y_offset,
	       gc,
	       resource_widget_value (mw, val));
}

static void
push_button_size (XlwMenuWidget mw,
		  widget_value *val,
		  Boolean in_menubar,
		  unsigned int *toggle_width,
		  unsigned int *label_width,
		  unsigned int *bindings_width,
		  unsigned int *height)
{
  /* inherit */
  label_button_size (mw, val, in_menubar,
		     toggle_width, label_width, bindings_width,
		     height);

  /* key bindings to display? */
  if (!in_menubar && val->key)
    {
      int w;
#ifdef NEED_MOTIF
      XmString key = XmStringCreateLtoR (val->key, XmSTRING_DEFAULT_CHARSET);
      w = string_width (mw, key);
      XmStringFree (key);
#else
      char *key = val->key;
      w = string_width (mw, key);
#endif
      *bindings_width += w + mw->menu.column_spacing;
    }
}

static void
push_button_draw (XlwMenuWidget mw,
		  widget_value *val,
		  Boolean       in_menubar,
		  Boolean       highlighted,
		  Window        window,
		  int x, int y,
		  unsigned int width,
		  unsigned int height,
		  unsigned int label_offset,
		  unsigned int binding_offset)
{
  int y_offset = mw->menu.shadow_thickness + mw->menu.vertical_margin;
  GC gc;
  shadow_type type;
  Boolean menu_pb = in_menubar && (menu_item_type (val) == BUTTON_TYPE);

  /* Draw the label string. */
  if (!label_offset)
    label_offset = mw->menu.shadow_thickness + mw->menu.horizontal_margin;

  if (highlighted)
    {
      if (val->enabled)
	gc = mw->menu.highlight_gc;
      else
	gc = mw->menu.inactive_gc;
    }
  else if (menu_pb)
    {
      if (val->enabled)
	gc = mw->menu.button_gc;
      else
	gc = mw->menu.inactive_button_gc;
    }
  else
    {
      if (val->enabled)
	gc = mw->menu.foreground_gc;
      else
	gc = mw->menu.inactive_gc;
    }

  string_draw_u (mw,
	       window,
	       x + label_offset, y + y_offset,
	       gc,
	       resource_widget_value (mw, val));

  /* Draw the keybindings */
  if (val->key)
    {
      if (!binding_offset)
	{
	  unsigned int s_width =
	    string_width (mw, resource_widget_value (mw, val));
	  binding_offset = label_offset + s_width +  mw->menu.shadow_thickness;
	}
      binding_draw (mw, window,
		    x + binding_offset + mw->menu.column_spacing,
		    y + y_offset, gc, val->key);
    }

  /* Draw the shadow */
  if (menu_pb)
    {
      if (highlighted)
	type = SHADOW_OUT;
      else
	type = (val->selected ? SHADOW_ETCHED_OUT : SHADOW_ETCHED_IN);
    }
  else
    {
      if (highlighted)
	type = SHADOW_OUT;
      else
	type = SHADOW_BACKGROUND;
    }

  shadow_draw (mw, window, x, y, width, height, type);
}

static unsigned int
arrow_decoration_height (XlwMenuWidget mw)
{
  int result = (mw->menu.font_ascent + mw->menu.font_descent) / 2;

  result += 2 * mw->menu.shadow_thickness;

  if (result > (mw->menu.font_ascent + mw->menu.font_descent))
    result = mw->menu.font_ascent + mw->menu.font_descent;

  return result;
}

static void
cascade_button_size (XlwMenuWidget mw,
		     widget_value *val,
		     Boolean in_menubar,
		     unsigned int *toggle_width,
		     unsigned int *label_width,
		     unsigned int *arrow_width,
		     unsigned int *height)
{
  /* inherit */
  label_button_size (mw, val, in_menubar,
		     toggle_width, label_width, arrow_width,
		     height);
  /* we have a pull aside arrow */
  if (!in_menubar)
    {
      *arrow_width += arrow_decoration_height (mw) + mw->menu.column_spacing;
    }
}

static void
cascade_button_draw (XlwMenuWidget mw,
		     widget_value *val,
		     Boolean       in_menubar,
		     Boolean       highlighted,
		     Window        window,
		     int x, int y,
		     unsigned int  width,
		     unsigned int  height,
		     unsigned int  label_offset,
		     unsigned int  binding_offset)
{
  shadow_type type;

  /* Draw the label string. */
  label_button_draw (mw, val, in_menubar, highlighted,
		     window, x, y, width, height, label_offset,
		     binding_offset);

  /* Draw the pull aside arrow */
  if (!in_menubar && val->contents)
    {
      int y_offset;
      unsigned int arrow_height = arrow_decoration_height (mw);

      y_offset = mw->menu.shadow_thickness + mw->menu.vertical_margin +
	(mw->menu.font_ascent+mw->menu.font_descent - arrow_height)/2;

      if (!binding_offset)
	{
	  unsigned int s_width =
	    string_width (mw, resource_widget_value (mw, val));

	  if (!label_offset)
	    label_offset = mw->menu.shadow_thickness +
	      mw->menu.horizontal_margin;

	  binding_offset = label_offset + s_width +  mw->menu.shadow_thickness;
	}

      arrow_decoration_draw (mw,
			     window,
			     x + binding_offset + mw->menu.column_spacing,
			     y + y_offset,
			     arrow_height,
			     highlighted);
    }

  /* Draw the shadow */
  if (highlighted)
    type = SHADOW_OUT;
  else
    type = SHADOW_BACKGROUND;

  shadow_draw (mw, window, x, y, width, height, type);
}

static unsigned int
toggle_decoration_height (XlwMenuWidget mw)
{
  int rv;
  if (mw->menu.indicator_size > 0)
    rv = mw->menu.indicator_size;
  else
    rv = mw->menu.font_ascent;

  if (rv > (mw->menu.font_ascent + mw->menu.font_descent))
    rv = mw->menu.font_ascent + mw->menu.font_descent;

  /* radio button can't be smaller than its border or a filling
     error will occur. */
  if (rv < 2 * mw->menu.shadow_thickness)
    rv = 2 * mw->menu.shadow_thickness;

  return rv;
}

static void
toggle_button_size (XlwMenuWidget mw,
		    widget_value *val,
		    Boolean in_menubar,
		    unsigned int *toggle_width,
		    unsigned int *label_width,
		    unsigned int *bindings_width,
		    unsigned int *height)
{
  /* inherit */
  push_button_size (mw, val, in_menubar,
		    toggle_width, label_width, bindings_width,
		    height);
  /* we have a toggle */
  *toggle_width += toggle_decoration_height (mw) + mw->menu.column_spacing;
}

static void
toggle_button_draw (XlwMenuWidget mw,
		    widget_value *val,
		    Boolean       in_menubar,
		    Boolean highlighted,
		    Window        window,
		    int x, int y,
		    unsigned int  width,
		    unsigned int  height,
		    unsigned int  label_tab,
		    unsigned int  binding_tab)
{
  int x_offset;
  int y_offset;
  unsigned int t_height = toggle_decoration_height (mw);

  /* Draw a toggle. */
  x_offset = mw->menu.shadow_thickness + mw->menu.horizontal_margin;
  y_offset = mw->menu.shadow_thickness + mw->menu.vertical_margin;
  y_offset += (mw->menu.font_ascent + mw->menu.font_descent - t_height)/2;

  toggle_decoration_draw (mw, window, x + x_offset, y + y_offset,
			  t_height, val->selected);

  /* Draw the pushbutton parts. */
  push_button_draw (mw, val, in_menubar, highlighted, window, x, y, width,
		    height, label_tab, binding_tab);
}

static unsigned int
radio_decoration_height (XlwMenuWidget mw)
{
  return toggle_decoration_height (mw);
}

static void
radio_button_draw (XlwMenuWidget mw,
		   widget_value *val,
		   Boolean       in_menubar,
		   Boolean       highlighted,
		   Window        window,
		   int x, int y,
		   unsigned int  width,
		   unsigned int  height,
		   unsigned int  label_tab,
		   unsigned int  binding_tab)
{
  int x_offset;
  int y_offset;
  unsigned int r_height = radio_decoration_height (mw);

  /* Draw a toggle. */
  x_offset = mw->menu.shadow_thickness + mw->menu.horizontal_margin;
  y_offset = mw->menu.shadow_thickness + mw->menu.vertical_margin;
  y_offset += (mw->menu.font_ascent + mw->menu.font_descent - r_height)/2;

  radio_decoration_draw (mw, window, x + x_offset, y + y_offset, r_height,
			 val->selected);

  /* Draw the pushbutton parts. */
  push_button_draw (mw, val, in_menubar, highlighted, window, x, y, width,
		    height, label_tab, binding_tab);
}

static struct _shadow_names
{
  const char *      name;
  shadow_type type;
} shadow_names[] =
{
  /* Motif */
  { "singleLine", SHADOW_SINGLE_LINE },
  { "doubleLine", SHADOW_DOUBLE_LINE },
  { "singleDashedLine", SHADOW_SINGLE_DASHED_LINE },
  { "doubleDashedLine", SHADOW_DOUBLE_DASHED_LINE },
  { "noLine", SHADOW_NO_LINE },
  { "shadowEtchedIn", SHADOW_ETCHED_IN },
  { "shadowEtchedOut", SHADOW_ETCHED_OUT },
  { "shadowEtchedInDash", SHADOW_ETCHED_IN_DASH },
  { "shadowEtchedOutDash", SHADOW_ETCHED_OUT_DASH },
  /* non-Motif */
  { "shadowDoubleEtchedIn", SHADOW_DOUBLE_ETCHED_IN },
  { "shadowDoubleEtchedOut", SHADOW_DOUBLE_ETCHED_OUT },
  { "shadowDoubleEtchedInDash", SHADOW_DOUBLE_ETCHED_IN_DASH },
  { "shadowDoubleEtchedOutDash", SHADOW_DOUBLE_ETCHED_OUT_DASH }
};

static shadow_type
separator_type (char *name)
{
  if (name)
    {
      int i;
      for (i = 0; i < (int) (XtNumber (shadow_names)); i++ )
	{
	  if (strcmp (name, shadow_names[i].name) == 0)
	    return shadow_names[i].type;
	}
    }
  return SHADOW_BACKGROUND;
}

static unsigned int
separator_decoration_height (XlwMenuWidget mw, widget_value *val)
{

  switch (separator_type (val->value))
    {
    case SHADOW_NO_LINE:
    case SHADOW_SINGLE_LINE:
    case SHADOW_SINGLE_DASHED_LINE:
      return 1;
    case SHADOW_DOUBLE_LINE:
    case SHADOW_DOUBLE_DASHED_LINE:
      return 3;
    case SHADOW_DOUBLE_ETCHED_OUT:
    case SHADOW_DOUBLE_ETCHED_IN:
    case SHADOW_DOUBLE_ETCHED_OUT_DASH:
    case SHADOW_DOUBLE_ETCHED_IN_DASH:
      return (1 + 2 * mw->menu.shadow_thickness);
    case SHADOW_ETCHED_OUT:
    case SHADOW_ETCHED_IN:
    default:
      return mw->menu.shadow_thickness;
    }
}

static void
separator_size (XlwMenuWidget mw,
		widget_value *val,
		Boolean in_menubar,
		unsigned int *toggle_width,
		unsigned int *label_width,
		unsigned int *rest_width,
		unsigned int *height)
{
  *height = separator_decoration_height (mw, val);
  *label_width = 1;
  *toggle_width = *rest_width = 0;
}

static void
separator_draw (XlwMenuWidget mw,
		widget_value *val,
		Boolean       in_menubar,
		Boolean       highlighted,
		Window        window,
		int x, int y,
		unsigned int  width,
		unsigned int  height,
		unsigned int  label_tab,
		unsigned int  binding_tab)
{
  unsigned int sep_width;

  if (in_menubar)
    sep_width = height;
  else
    sep_width = width;

  separator_decoration_draw (mw,
			     window,
			     x,
			     y,
			     sep_width,
			     in_menubar,
			     separator_type(val->value));
}

static void
pushright_size (XlwMenuWidget mw,
		widget_value *val,
		Boolean in_menubar,
		unsigned int *toggle_width,
		unsigned int *label_width,
		unsigned int *rest_width,
		unsigned int *height)
{
  *height = *label_width = *toggle_width = *rest_width = 0;
}

static void
size_menu_item (XlwMenuWidget mw,
		widget_value *val,
		int horizontal,
		unsigned int *toggle_width,
		unsigned int *label_width,
		unsigned int *rest_width,
		unsigned int *height)
{
  void (*function_ptr) (XlwMenuWidget _mw,
			widget_value *_val,
			Boolean _in_menubar,
			unsigned int *_toggle_width,
			unsigned int *_label_width,
			unsigned int *_rest_width,
			unsigned int *_height);

  switch (menu_item_type (val))
    {
    case TOGGLE_TYPE:
    case RADIO_TYPE:
      function_ptr = toggle_button_size;
      break;
    case SEPARATOR_TYPE:
      function_ptr = separator_size;
      break;
    case INCREMENTAL_TYPE:
    case CASCADE_TYPE:
      function_ptr = cascade_button_size;
      break;
    case BUTTON_TYPE:
      function_ptr = push_button_size;
      break;
    case PUSHRIGHT_TYPE:
      function_ptr = pushright_size;
      break;
    case TEXT_TYPE:
    default:
      function_ptr = label_button_size;
      break;
    }

  (*function_ptr) (mw,
		   val,
		   horizontal,
		   toggle_width,
		   label_width,
		   rest_width,
		   height);
}

static void
display_menu_item (XlwMenuWidget mw,
		   widget_value *val,
		   window_state *ws,
		   XPoint *where,
		   Boolean highlighted,
		   Boolean horizontal,
		   Boolean just_compute)
{

  int x = where->x /* + mw->menu.shadow_thickness */ ;
  int y = where->y /* + mw->menu.shadow_thickness */ ;
  unsigned int toggle_width;
  unsigned int label_width;
  unsigned int binding_width;
  unsigned int width;
  unsigned int height;
  unsigned int label_tab;
  unsigned int binding_tab;
  void (*function_ptr) (XlwMenuWidget _mw,
			widget_value *_val,
			Boolean _in_menubar,
			Boolean _highlighted,
			Window        _window,
			int _x, int _y,
			unsigned int _width,
			unsigned int _height,
			unsigned int _label_tab,
			unsigned int  _binding_tab);

  size_menu_item (mw, val, horizontal,
		  &toggle_width, &label_width, &binding_width, &height);

  if (horizontal)
    {
      width = toggle_width + label_width + binding_width;
      height = ws->height - 2 * mw->menu.shadow_thickness;
    }
  else
    {
      width = ws->width - 2 * mw->menu.shadow_thickness;
      toggle_width = ws->toggle_width;
      label_width = ws->label_width;
    }

  where->x += width;
  where->y += height;

  if (just_compute)
    return;

  label_tab = toggle_width;
  binding_tab = toggle_width + label_width;

  switch (menu_item_type (val))
    {
    case TOGGLE_TYPE:
      function_ptr = toggle_button_draw;
      break;
    case RADIO_TYPE:
      function_ptr = radio_button_draw;
      break;
    case SEPARATOR_TYPE:
      function_ptr = separator_draw;
      break;
    case INCREMENTAL_TYPE:
    case CASCADE_TYPE:
      function_ptr = cascade_button_draw;
      break;
    case BUTTON_TYPE:
      function_ptr = push_button_draw;
      break;
    case TEXT_TYPE:
      function_ptr = label_button_draw;
      break;
    default: /* do no drawing */
      return;
    }

  (*function_ptr) (mw,
		   val,
		   horizontal,
		   highlighted,
		   ws->window,
		   x, y,
		   width, height,
		   label_tab,
		   binding_tab);
}

static void
size_menu (XlwMenuWidget mw, int level)
{
  unsigned int	toggle_width;
  unsigned int	label_width;
  unsigned int	rest_width;
  unsigned int	height;
  unsigned int	max_toggle_width = 0;
  unsigned int	max_label_width  = 0;
  unsigned int	max_rest_width   = 0;
  unsigned int	max_height = 0;
  int		horizontal_p = mw->menu.horizontal && (level == 0);
  widget_value*	val;
  window_state*	ws;

  if (level >= mw->menu.old_depth)
    abort ();

  ws = &mw->menu.windows [level];

  for (val = mw->menu.old_stack [level]->contents; val; val = val->next)
    {
      size_menu_item (mw,
		      val,
		      horizontal_p,
		      &toggle_width,
		      &label_width,
		      &rest_width,
		      &height);
      if (horizontal_p)
	{
	  max_label_width += toggle_width + label_width + rest_width;
	  if (height > max_height)
	    max_height = height;
	}
      else
	{
	  if (max_toggle_width < toggle_width)
	      max_toggle_width = toggle_width;
	  if (max_label_width < label_width)
	      max_label_width = label_width;
	  if (max_rest_width < rest_width)
	      max_rest_width = rest_width;
	  max_height += height;
	}
    }

  ws->height = max_height;
  ws->width = max_label_width + max_rest_width + max_toggle_width;
  ws->toggle_width = max_toggle_width;
  ws->label_width  = max_label_width;

  ws->width  += 2 * mw->menu.shadow_thickness;
  ws->height += 2 * mw->menu.shadow_thickness;
}

static void
display_menu (XlwMenuWidget mw, int level, Boolean just_compute_p,
	      XPoint *highlighted_pos, XPoint *hit, widget_value **hit_return,
	      widget_value *this, widget_value *that)
{
  widget_value *val;
  widget_value *following_item;
  window_state *ws;
  XPoint	where;
  int horizontal_p = mw->menu.horizontal && (level == 0);
  int highlighted_p;
  int just_compute_this_one_p;

  if (level >= mw->menu.old_depth)
    abort ();

  if (level < mw->menu.old_depth - 1)
    following_item = mw->menu.old_stack [level + 1];
  else
    {
      if (lw_menu_accelerate
	  && level == mw->menu.old_depth - 1
	  && mw->menu.old_stack [level]->type == CASCADE_TYPE)
	just_compute_p = True;
      following_item = NULL;
    }

#if SLOPPY_TYPES == 1
  puts("===================================================================");
  print_widget_value (following_item, 1, 0);
#endif

  if (hit)
    *hit_return = NULL;

  where.x = mw->menu.shadow_thickness;
  where.y = mw->menu.shadow_thickness;

  ws = &mw->menu.windows [level];
  for (val = mw->menu.old_stack [level]->contents; val; val = val->next)
    {
      XPoint start;

      highlighted_p = (val == following_item);
      /* If this is the partition (the dummy item which says that menus
	 after this should be flushright) then figure out how big the
	 following items are.  This means we walk down the tail of the
	 list twice, but that's no big deal - it's short.
       */
      if (horizontal_p && (menu_item_type (val) == PUSHRIGHT_TYPE))
	{
	  widget_value *rest;
	  XPoint flushright_size;
	  int new_x;
	  flushright_size.x = 0;
	  flushright_size.y = 0;
	  for (rest = val; rest; rest = rest->next)
	    display_menu_item (mw, rest, ws, &flushright_size,
			       highlighted_p, horizontal_p, True);
	  new_x = ws->width - (flushright_size.x + mw->menu.shadow_thickness);
	  if (new_x > where.x)
	    where.x = new_x;
	  /* We know what we need; don't draw this item. */
	  continue;
	}

      if (highlighted_p && highlighted_pos)
	{
	  if (horizontal_p)
	    highlighted_pos->x = where.x;
	  else
	    highlighted_pos->y = where.y;
	}

      just_compute_this_one_p =
	just_compute_p || ((this || that) && val != this &&  val != that);

      start.x = where.x;
      start.y = where.y;
      display_menu_item (mw, val, ws, &where, highlighted_p, horizontal_p,
			 just_compute_this_one_p);

      if (highlighted_p && highlighted_pos)
	{
	  if (horizontal_p)
	    highlighted_pos->y = ws->height;
	  else
	    highlighted_pos->x = ws->width;
	}

      if (hit && !*hit_return)
	{
	  if (horizontal_p && hit->x > start.x && hit->x <= where.x)
	    *hit_return = val;
	  else if (!horizontal_p && hit->y > start.y && hit->y <= where.y)
	    *hit_return = val;
	}

      if (horizontal_p)
	where.y = mw->menu.shadow_thickness;
      else
	where.x = mw->menu.shadow_thickness;
    }

  /* Draw slab edges around menu */
  if (!just_compute_p)
    shadow_draw(mw, ws->window, 0, 0, ws->width, ws->height, SHADOW_OUT);
}

/* Motion code */
static void
set_new_state (XlwMenuWidget mw, widget_value *val, int level)
{
  int i;

  mw->menu.new_depth = 0;
  for (i = 0; i < level; i++)
    push_new_stack (mw, mw->menu.old_stack [i]);
  if (val)
    push_new_stack (mw, val);
}

static void
make_windows_if_needed (XlwMenuWidget mw, int n)
{
  int i;
  int start_at;
  XSetWindowAttributes xswa;
  Widget p;
  unsigned long mask;
  int depth;
  Visual *visual;
  window_state *windows;
  Window root;

  if (mw->menu.windows_length >= n)
    return;

  root = RootWindowOfScreen (XtScreen(mw));
  /* grab the visual and depth from the nearest shell ancestor */
  visual = CopyFromParent;
  depth = CopyFromParent;
  p = XtParent(mw);
  while (visual == CopyFromParent && p)
    {
      if (XtIsShell(p))
	{
	  visual = ((ShellWidget)p)->shell.visual;
	  depth = p->core.depth;
	}
      p = XtParent(p);
    }

  xswa.save_under = True;
  xswa.override_redirect = True;
  xswa.background_pixel = mw->core.background_pixel;
  xswa.border_pixel = mw->core.border_pixel;
  xswa.event_mask = (ExposureMask | ButtonMotionMask
		     | ButtonReleaseMask | ButtonPressMask);
  xswa.cursor = mw->menu.cursor_shape;
  xswa.colormap = mw->core.colormap;
  mask = CWSaveUnder | CWOverrideRedirect | CWBackPixel | CWBorderPixel
    | CWEventMask | CWCursor | CWColormap;

  if (mw->menu.use_backing_store)
    {
      xswa.backing_store = Always;
      mask |= CWBackingStore;
    }

  if (!mw->menu.windows)
    {
      mw->menu.windows =
	(window_state *) XtMalloc (n * sizeof (window_state));
      start_at = 0;
    }
  else
    {
      mw->menu.windows =
	(window_state *) XtRealloc ((char *) mw->menu.windows,
				    n * sizeof (window_state));
      start_at = mw->menu.windows_length;
    }
  mw->menu.windows_length = n;

  windows = mw->menu.windows;

  for (i = start_at; i < n; i++)
   {
     windows [i].x = 0;
     windows [i].y = 0;
     windows [i].width = 1;
     windows [i].height = 1;
     windows [i].window =
       XCreateWindow (XtDisplay (mw),
		      root,
		      0, 0, 1, 1,
		      0, depth, CopyFromParent, visual, mask, &xswa);
  }
}

/* Make the window fit in the screen */
static void
fit_to_screen (XlwMenuWidget mw, window_state *ws, window_state *previous_ws,
	       Boolean horizontal_p)
{
  int screen_width = WidthOfScreen (XtScreen (mw));
  int screen_height = HeightOfScreen (XtScreen (mw));

  if (ws->x < 0)
    ws->x = 0;
  else if ((int) (ws->x + ws->width) > screen_width)
    {
      if (!horizontal_p)
	ws->x = previous_ws->x - ws->width;
      else
	{
	  ws->x = screen_width - ws->width;

	  /* This check is to make sure we cut off the right side
             instead of the left side if the menu is wider than the
             screen. */
	  if (ws->x < 0)
	    ws->x = 0;
	}
    }
  if (ws->y < 0)
    ws->y = 0;
  else if ((int) (ws->y + ws->height) > screen_height)
    {
      if (horizontal_p)
	{
	  /* A pulldown must either be entirely above or below the menubar.
	     If we're here, the pulldown doesn't fit below the menubar, so
             let's determine if it will fit above the menubar.
             Only put it above if there is more room above than below.
	     Note shadow_thickness offset to allow for slab surround.
	     */
	  if (ws->y > (screen_height / 2))
	    ws->y = previous_ws->y - ws->height + mw->menu.shadow_thickness;
	}
      else
	{
	  ws->y = screen_height - ws->height;
	   /* if it's taller than the screen, display the topmost part
	      that will fit, beginning at the top of the screen. */
	  if (ws->y < 0)
	    ws->y = 0;
	}
    }
}

/* Updates old_stack from new_stack and redisplays. */
static void
remap_menubar (XlwMenuWidget mw)
{
  int i;
  int last_same;
  XPoint selection_position;
  int old_depth = mw->menu.old_depth;
  int new_depth = mw->menu.new_depth;
  widget_value **old_stack;
  widget_value **new_stack;
  window_state *windows;
  widget_value *old_selection;
  widget_value *new_selection;

  /* Check that enough windows and old_stack are ready. */
  make_windows_if_needed (mw, new_depth);
  make_old_stack_space (mw, new_depth);
  windows = mw->menu.windows;
  old_stack = mw->menu.old_stack;
  new_stack = mw->menu.new_stack;

  /* compute the last identical different entry */
  for (i = 1; i < old_depth && i < new_depth; i++)
    if (old_stack [i] != new_stack [i])
      break;
  last_same = i - 1;

  if (lw_menu_accelerate
      && last_same
      && last_same == old_depth - 1
      && old_stack [last_same]->contents)
    last_same--;

  /* Memorize the previously selected item to be able to refresh it */
  old_selection = last_same + 1 < old_depth ? old_stack [last_same + 1] : NULL;
  new_selection = last_same + 1 < new_depth ? new_stack [last_same + 1] : NULL;

  /* updates old_state from new_state.  It has to be done now because
     display_menu (called below) uses the old_stack to know what to display. */
  for (i = last_same + 1; i < new_depth; i++)
    old_stack [i] = new_stack [i];

  mw->menu.old_depth = new_depth;

  /* refresh the last selection */
  selection_position.x = 0;
  selection_position.y = 0;
  display_menu (mw, last_same, new_selection == old_selection,
		&selection_position, NULL, NULL, old_selection, new_selection);

  /* Now popup the new menus */
  for (i = last_same + 1; i < new_depth && new_stack [i]->contents; i++)
    {
      window_state *previous_ws = &windows [i - 1];
      window_state *ws = &windows [i];

      if (lw_menu_accelerate && i == new_depth - 1)
	break;

      ws->x = previous_ws->x + selection_position.x;
      ws->y = previous_ws->y + selection_position.y;

      /* take into account the slab around the new menu */
      ws->y -= mw->menu.shadow_thickness;

      {
	widget_value *val = mw->menu.old_stack [i];
	if (val->contents->type == INCREMENTAL_TYPE)
	{
	  /* okay, we're now doing a lisp callback to incrementally generate
	     more of the menu. */
	  XtCallCallbackList ((Widget)mw,
			      mw->menu.open,
			      (XtPointer)val->contents);
	}
      }

      size_menu (mw, i);

      fit_to_screen (mw, ws, previous_ws, mw->menu.horizontal && i == 1);

      XClearWindow (XtDisplay (mw), ws->window);
      XMoveResizeWindow (XtDisplay (mw), ws->window, ws->x, ws->y,
			 ws->width, ws->height);
      XMapRaised (XtDisplay (mw), ws->window);
      display_menu (mw, i, False, &selection_position, NULL, NULL, NULL, NULL);
    }

  /* unmap the menus that popped down */

  last_same = new_depth;
  if (lw_menu_accelerate
      && last_same > 1
      && new_stack [last_same - 1]->contents)
    last_same--;

  for (i = last_same - 1; i < old_depth; i++)
    if (i >= last_same || !new_stack [i]->contents)
      XUnmapWindow (XtDisplay (mw), windows [i].window);
}

static Boolean
motion_event_is_in_menu (XlwMenuWidget mw, XMotionEvent *ev, int level,
			 XPoint *relative_pos)
{
  window_state *ws = &mw->menu.windows [level];
  int x = level == 0 ? ws->x : ws->x + mw->menu.shadow_thickness;
  int y = level == 0 ? ws->y : ws->y + mw->menu.shadow_thickness;
  relative_pos->x = ev->x_root - x;
  relative_pos->y = ev->y_root - y;
  return (x < ev->x_root && ev->x_root < (int) (x + ws->width) &&
	  y < ev->y_root && ev->y_root < (int) (y + ws->height));
}

static Boolean
map_event_to_widget_value (XlwMenuWidget mw, XMotionEvent *ev,
			   widget_value **val_ptr, int *level,
			   Boolean *inside_menu)
{
  int 		i;
  XPoint	relative_pos;
  window_state*	ws;

  *val_ptr = NULL;
  *inside_menu = False;

  /* Find the window */
#if 1
  for (i = mw->menu.old_depth - 1; i >= 0; i--)
#else
  for (i = 0; i <= mw->menu.old_depth - 1; i++)
#endif
    {
      ws = &mw->menu.windows [i];
      if (ws && motion_event_is_in_menu (mw, ev, i, &relative_pos))
	{
	  *inside_menu = True;	/* special logic for menubar below... */
	  if ((ev->type == ButtonPress) ||
	      (ev->state != 0))
	    {
	      display_menu (mw, i, True, NULL, &relative_pos,
			    val_ptr, NULL, NULL);
	      if (*val_ptr)
		{
		  *level = i + 1;
		  *inside_menu = True;
		  return True;
		}
	      else if (mw->menu.horizontal || i == 0)
		{
		  /* if we're clicking on empty part of the menubar, then
		     unpost the stay-up menu */
		  *inside_menu = False;
		}
	    }
	}
    }
  return False;
}

/* Procedures */
static void
make_drawing_gcs (XlwMenuWidget mw)
{
  XGCValues xgcv;
  unsigned long flags = (GCFont | GCForeground | GCBackground);

#ifdef NEED_MOTIF
  xgcv.font = default_font_of_font_list (mw->menu.font_list)->fid;
#else
  xgcv.font = mw->menu.font->fid;
#endif

  xgcv.foreground = mw->core.background_pixel;
  xgcv.background = mw->menu.foreground;
  mw->menu.background_gc = XtGetGC ((Widget) mw, flags, &xgcv);

  xgcv.foreground = mw->menu.foreground;
  xgcv.background = mw->core.background_pixel;
  mw->menu.foreground_gc = XtGetGC ((Widget) mw, flags, &xgcv);

  if (mw->menu.select_color != (Pixel)-1)
    {
      xgcv.foreground = mw->menu.select_color;
    }
  else
    {
      Display *dpy = XtDisplay(mw);
      if (CellsOfScreen(DefaultScreenOfDisplay(dpy)) <= 2)
	{ /* mono */
	  xgcv.foreground = mw->menu.foreground;
	}
      else
	{ /* color */
	  XColor xcolor;
	  Colormap cmap = mw->core.colormap;
	  xcolor.pixel = mw->core.background_pixel;
	  XQueryColor (dpy, cmap, &xcolor);
	  xcolor.red   = (xcolor.red   * 17) / 20;
	  xcolor.green = (xcolor.green * 17) / 20;
	  xcolor.blue  = (xcolor.blue  * 17) / 20;
	  if (allocate_nearest_color (dpy, cmap, &xcolor))
	    xgcv.foreground = xcolor.pixel;
	}
    }
  xgcv.background = mw->core.background_pixel;
  mw->menu.select_gc = XtGetGC ((Widget)mw, flags, &xgcv);

  xgcv.foreground = mw->menu.foreground;
  xgcv.background = mw->core.background_pixel;
  xgcv.fill_style = FillStippled;
  xgcv.stipple = mw->menu.gray_pixmap;
  mw->menu.inactive_gc = XtGetGC ((Widget)mw,
				  (flags | GCFillStyle | GCStipple),
				  &xgcv);

  xgcv.foreground = mw->menu.highlight_foreground;
  xgcv.background = mw->core.background_pixel;
  mw->menu.highlight_gc = XtGetGC ((Widget)mw, flags, &xgcv);

  xgcv.foreground = mw->menu.title_foreground;
  xgcv.background = mw->core.background_pixel;
  mw->menu.title_gc = XtGetGC ((Widget)mw, flags, &xgcv);

  xgcv.foreground = mw->menu.button_foreground;
  xgcv.background = mw->core.background_pixel;
  mw->menu.button_gc = XtGetGC ((Widget)mw, flags, &xgcv);

  xgcv.fill_style = FillStippled;
  xgcv.stipple = mw->menu.gray_pixmap;
  mw->menu.inactive_button_gc = XtGetGC ((Widget)mw,
					 (flags | GCFillStyle | GCStipple),
					 &xgcv);
}

static void
release_drawing_gcs (XlwMenuWidget mw)
{
  XtReleaseGC ((Widget) mw, mw->menu.foreground_gc);
  XtReleaseGC ((Widget) mw, mw->menu.button_gc);
  XtReleaseGC ((Widget) mw, mw->menu.highlight_gc);
  XtReleaseGC ((Widget) mw, mw->menu.title_gc);
  XtReleaseGC ((Widget) mw, mw->menu.inactive_gc);
  XtReleaseGC ((Widget) mw, mw->menu.inactive_button_gc);
  XtReleaseGC ((Widget) mw, mw->menu.background_gc);
  XtReleaseGC ((Widget) mw, mw->menu.select_gc);
  /* let's get some segvs if we try to use these... */
  mw->menu.foreground_gc      = (GC) -1;
  mw->menu.button_gc          = (GC) -1;
  mw->menu.highlight_gc       = (GC) -1;
  mw->menu.title_gc           = (GC) -1;
  mw->menu.inactive_gc        = (GC) -1;
  mw->menu.inactive_button_gc = (GC) -1;
  mw->menu.background_gc      = (GC) -1;
  mw->menu.select_gc          = (GC) -1;
}

#define MINL(x,y) ((((unsigned long) (x)) < ((unsigned long) (y))) \
		   ? ((unsigned long) (x)) : ((unsigned long) (y)))

static void
make_shadow_gcs (XlwMenuWidget mw)
{
  XGCValues xgcv;
  unsigned long pm = 0;
  Display *dpy = XtDisplay ((Widget) mw);
  Colormap cmap = mw->core.colormap;
  XColor topc, botc;
  int top_frobbed = 0, bottom_frobbed = 0;

  if (mw->menu.top_shadow_color == (Pixel) (-1))
      mw->menu.top_shadow_color = mw->core.background_pixel;
  if (mw->menu.bottom_shadow_color == (Pixel) (-1))
      mw->menu.bottom_shadow_color = mw->menu.foreground;

  if (mw->menu.top_shadow_color == mw->core.background_pixel ||
      mw->menu.top_shadow_color == mw->menu.foreground)
    {
      topc.pixel = mw->core.background_pixel;
      XQueryColor (dpy, cmap, &topc);
      /* don't overflow/wrap! */
      topc.red   = MINL (65535, topc.red   * 1.2);
      topc.green = MINL (65535, topc.green * 1.2);
      topc.blue  = MINL (65535, topc.blue  * 1.2);
      if (allocate_nearest_color (dpy, cmap, &topc))
	{
          if (topc.pixel == mw->core.background_pixel)
	    {
	      XFreeColors( dpy, cmap, &topc.pixel, 1, 0);
	      topc.red   = MINL (65535, topc.red   + 0x8000);
	      topc.green = MINL (65535, topc.green + 0x8000);
	      topc.blue  = MINL (65535, topc.blue  + 0x8000);
	      if (allocate_nearest_color (dpy, cmap, &topc))
		{
		  mw->menu.top_shadow_color = topc.pixel;
		}
	    }
	  else
	    {
	      mw->menu.top_shadow_color = topc.pixel;
	    }

	  top_frobbed = 1;
	}
    }
  if (mw->menu.bottom_shadow_color == mw->menu.foreground ||
      mw->menu.bottom_shadow_color == mw->core.background_pixel)
    {
      botc.pixel = mw->core.background_pixel;
      XQueryColor (dpy, cmap, &botc);
      botc.red   = (botc.red   * 3) / 5;
      botc.green = (botc.green * 3) / 5;
      botc.blue  = (botc.blue  * 3) / 5;
      if (allocate_nearest_color (dpy, cmap, &botc))
	{
	  if (botc.pixel == mw->core.background_pixel)
	    {
	      XFreeColors (dpy, cmap, &botc.pixel, 1, 0);
	      botc.red   = MINL (65535, botc.red   + 0x4000);
	      botc.green = MINL (65535, botc.green + 0x4000);
	      botc.blue  = MINL (65535, botc.blue  + 0x4000);
	      if (allocate_nearest_color (dpy, cmap, &botc))
		{
		  mw->menu.bottom_shadow_color = botc.pixel;
		}
	    }
	  else
	    {
	      mw->menu.bottom_shadow_color = botc.pixel;
	    }

          bottom_frobbed = 1;
	}
    }

  if (top_frobbed && bottom_frobbed)
    {
      int top_avg = ((topc.red / 3) + (topc.green / 3) + (topc.blue / 3));
      int bot_avg = ((botc.red / 3) + (botc.green / 3) + (botc.blue / 3));
      if (bot_avg > top_avg)
	{
	  Pixel tmp = mw->menu.top_shadow_color;
	  mw->menu.top_shadow_color = mw->menu.bottom_shadow_color;
	  mw->menu.bottom_shadow_color = tmp;
	}
      else if (topc.pixel == botc.pixel)
	{
	  if (botc.pixel == mw->menu.foreground)
	    mw->menu.top_shadow_color = mw->core.background_pixel;
	  else
	    mw->menu.bottom_shadow_color = mw->menu.foreground;
	}
    }

  if (!mw->menu.top_shadow_pixmap &&
      mw->menu.top_shadow_color == mw->core.background_pixel)
    {
      mw->menu.top_shadow_pixmap = mw->menu.gray_pixmap;
      mw->menu.top_shadow_color = mw->menu.foreground;
    }
  if (!mw->menu.bottom_shadow_pixmap &&
      mw->menu.bottom_shadow_color == mw->core.background_pixel)
    {
      mw->menu.bottom_shadow_pixmap = mw->menu.gray_pixmap;
      mw->menu.bottom_shadow_color = mw->menu.foreground;
    }

  xgcv.fill_style = FillOpaqueStippled;
  xgcv.foreground = mw->menu.top_shadow_color;
  xgcv.background = mw->core.background_pixel;
/*  xgcv.stipple = mw->menu.top_shadow_pixmap; gtb */
  if (mw->menu.top_shadow_pixmap &&
      mw->menu.top_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
     xgcv.stipple = mw->menu.top_shadow_pixmap;
  else
     xgcv.stipple = 0;
  pm = (xgcv.stipple ? GCStipple|GCFillStyle : 0);
  mw->menu.shadow_top_gc =
    XtGetGC((Widget)mw, GCForeground|GCBackground|pm, &xgcv);

  xgcv.foreground = mw->menu.bottom_shadow_color;
/*  xgcv.stipple = mw->menu.bottom_shadow_pixmap; gtb */
  if (mw->menu.bottom_shadow_pixmap &&
      mw->menu.bottom_shadow_pixmap != XmUNSPECIFIED_PIXMAP)
     xgcv.stipple = mw->menu.bottom_shadow_pixmap;
  else
     xgcv.stipple = 0;
  pm = (xgcv.stipple ? GCStipple|GCFillStyle : 0);
  mw->menu.shadow_bottom_gc =
    XtGetGC ((Widget)mw, GCForeground|GCBackground|pm, &xgcv);
}


static void
release_shadow_gcs (XlwMenuWidget mw)
{
  XtReleaseGC ((Widget) mw, mw->menu.shadow_top_gc);
  XtReleaseGC ((Widget) mw, mw->menu.shadow_bottom_gc);
}


static void
extract_font_extents (XlwMenuWidget mw)
{
#ifdef NEED_MOTIF
  /* Find the maximal ascent/descent of the fonts in the font list
     so that all menu items can be the same height... */
  mw->menu.font_ascent  = 0;
  mw->menu.font_descent = 0;

  {
    XmFontContext context;
#if (XmVersion >= 1002)
    XmFontListEntry fontentry;
#else
    XmStringCharSet charset;
#endif
    XFontStruct *font;

    if (! XmFontListInitFontContext (&context, mw->menu.font_list))
      abort ();
#if (XmVersion >= 1002)
    /* There is a BUG in the 1.2 version of XmFontListGetNextFont() (or more
       specifically, in _XmGetFirstFont()) that can cause a null pointer to be
       passed to XFontsOfFontSet.  Use XmFontListNextEntry(), which is the
       newer equivalent, instead.  Also, it supports font sets, and the
       older function doesn't. */
    while ((fontentry = XmFontListNextEntry (context)))
      {
	XmFontType rettype;

	XtPointer one_of_them = XmFontListEntryGetFont (fontentry, &rettype);
	if (rettype == XmFONT_IS_FONTSET)
	  {
	    XFontSet fontset = (XFontSet) one_of_them;
	    XFontStruct **fontstruct_list;
	    char **fontname_list;
	    int fontcount = XFontsOfFontSet (fontset, &fontstruct_list,
					     &fontname_list);
	    while (--fontcount >= 0)
	      {
		font = fontstruct_list[fontcount];
		if (font->ascent > (int) mw->menu.font_ascent)
		  mw->menu.font_ascent = font->ascent;
		if (font->descent > (int) mw->menu.font_descent)
		  mw->menu.font_descent = font->descent;
	      }
	  }
	else /* XmFONT_IS_FONT */
	  {
	    font = (XFontStruct *) one_of_them;
	    if (font->ascent > (int) mw->menu.font_ascent)
	      mw->menu.font_ascent = font->ascent;
	    if (font->descent > (int) mw->menu.font_descent)
	      mw->menu.font_descent = font->descent;
	  }
      }
#else /* motif 1.1 */
    while (XmFontListGetNextFont (context, &charset, &font))
      {
	if (font->ascent > (int) mw->menu.font_ascent)
	  mw->menu.font_ascent = font->ascent;
	if (font->descent > (int) mw->menu.font_descent)
	  mw->menu.font_descent = font->descent;
	XtFree (charset);
      }
#endif /* Motif version */
    XmFontListFreeFontContext (context);
  }
#else /* Not Motif */
# ifdef USE_XFONTSET
  XFontStruct **fontstruct_list;
  char **fontname_list;
  XFontStruct *font;
  int fontcount = XFontsOfFontSet(mw->menu.font_set, &fontstruct_list,
                                      &fontname_list);
  mw->menu.font_ascent  = 0;
  mw->menu.font_descent = 0;
#  if 0 /* nasty, personal debug, Kazz */
  fprintf(stderr, "fontSet count is %d\n", fontcount);
#  endif
  while (--fontcount >= 0) {
      font = fontstruct_list[fontcount];
      if (font->ascent > (int) mw->menu.font_ascent)
          mw->menu.font_ascent = font->ascent;
      if (font->descent > (int) mw->menu.font_descent)
          mw->menu.font_descent = font->descent;
  }
# else /* ! USE_XFONTSET */
  mw->menu.font_ascent  = mw->menu.font->ascent;
  mw->menu.font_descent = mw->menu.font->descent;
# endif
#endif /* NEED_MOTIF */
}

#ifdef NEED_MOTIF
static XFontStruct *
default_font_of_font_list (XmFontList font_list)
{
  XFontStruct *font = 0;
# if 0
  /* Xm/Label.c does this: */
  _XmFontListGetDefaultFont (font_list, &font);
# else  /* !0 */
  {
    XmFontContext context;
#if (XmVersion >= 1002)
    XmFontListEntry fontentry;
    XmFontType rettype;
    XtPointer one_of_them;
#else
    XmStringCharSet charset;
#endif

    if (! XmFontListInitFontContext (&context, font_list))
      abort ();
#if (XmVersion >= 1002)
    /* There is a BUG in the 1.2 version of XmFontListGetNextFont() (or more
       specifically, in _XmGetFirstFont()) that can cause a null pointer to be
       passed to XFontsOfFontSet.  Use XmFontListNextEntry(), which is the
       newer equivalent, instead. */
    fontentry = XmFontListNextEntry (context);
    one_of_them = XmFontListEntryGetFont (fontentry, &rettype);
    if (rettype == XmFONT_IS_FONTSET)
      {
	XFontSet fontset = (XFontSet) one_of_them;
	XFontStruct **fontstruct_list;
	char **fontname_list;
	(void) XFontsOfFontSet (fontset, &fontstruct_list, &fontname_list);
	font = fontstruct_list[0];
      }
    else /* XmFONT_IS_FONT */
      {
	font = (XFontStruct *) one_of_them;
      }
#else
    if (! XmFontListGetNextFont (context, &charset, &font))
      abort ();
    XtFree (charset);
#endif
    XmFontListFreeFontContext (context);
  }
# endif /* !0 */

  if (! font) abort ();
  return font;
}
#endif /* NEED_MOTIF */

static void
XlwMenuInitialize (Widget request, Widget new, ArgList args,
		   Cardinal *num_args)
{
  /* Get the GCs and the widget size */
  XlwMenuWidget mw = (XlwMenuWidget)new;
  Window window = RootWindowOfScreen (DefaultScreenOfDisplay (XtDisplay (mw)));
  Display *display = XtDisplay (mw);

/*  mw->menu.cursor = XCreateFontCursor (display, mw->menu.cursor_shape); */
  mw->menu.cursor = mw->menu.cursor_shape;

  mw->menu.gray_pixmap =
    XCreatePixmapFromBitmapData (display, window, (char *) gray_bits,
				 gray_width, gray_height, 1, 0, 1);

#ifdef NEED_MOTIF
  /* #### Even if it's a kludge!!!, we should consider doing the same for
     X Font Sets. */
  /* The menu.font_list slot came from the *fontList resource (Motif standard.)
     The menu.font_list_2 slot came from the *font resource, for backward
     compatibility with older versions of this code, and consistency with the
     rest of emacs.  If both font and fontList are specified, we use fontList.
     If only one is specified, we use that.  If neither are specified, we
     use the "fallback" value.  What a kludge!!!

     Note that this has the bug that a more general wildcard like "*fontList:"
     will override a more specific resource like "Emacs*menubar.font:".  But
     I can't think of a way around that.
   */
  if (mw->menu.font_list)	  /* if *fontList is specified, use that */
    ;
  else if (mw->menu.font_list_2)  /* else if *font is specified, use that */
    mw->menu.font_list = mw->menu.font_list_2;
  else				  /* otherwise use default */
    mw->menu.font_list = mw->menu.fallback_font_list;
#endif

  make_drawing_gcs     (mw);
  make_shadow_gcs      (mw);
  extract_font_extents (mw);

  mw->menu.popped_up              = False;
  mw->menu.pointer_grabbed        = False;
  mw->menu.next_release_must_exit = False;

  mw->menu.old_depth = 1;
  mw->menu.old_stack = XtNew (widget_value*);
  mw->menu.old_stack_length = 1;
  mw->menu.old_stack [0] = mw->menu.contents;

  mw->menu.new_depth = 0;
  mw->menu.new_stack = 0;
  mw->menu.new_stack_length = 0;
  push_new_stack (mw, mw->menu.contents);

  mw->menu.windows = XtNew (window_state);
  mw->menu.windows_length = 1;
  mw->menu.windows [0].x = 0;
  mw->menu.windows [0].y = 0;
  mw->menu.windows [0].width = 0;
  mw->menu.windows [0].height = 0;
  size_menu (mw, 0);

  mw->core.width  = mw->menu.windows [0].width;
  mw->core.height = mw->menu.windows [0].height;
}

static void
XlwMenuClassInitialize (void)
{
  initialize_massaged_resource_char();
}

static void
XlwMenuRealize (Widget w, Mask *valueMask, XSetWindowAttributes *attributes)
{
  XlwMenuWidget mw = (XlwMenuWidget)w;
  XSetWindowAttributes xswa;
  unsigned long mask;

  (*xlwMenuWidgetClass->core_class.superclass->core_class.realize)
    (w, valueMask, attributes);

  xswa.save_under = True;
  xswa.cursor = mw->menu.cursor_shape;
  mask = CWSaveUnder | CWCursor;
  if (mw->menu.use_backing_store)
    {
      xswa.backing_store = Always;
      mask |= CWBackingStore;
    }
  XChangeWindowAttributes (XtDisplay (w), XtWindow (w), mask, &xswa);

  mw->menu.windows [0].window = XtWindow (w);
  mw->menu.windows [0].x = w->core.x;
  mw->menu.windows [0].y = w->core.y;
  mw->menu.windows [0].width = w->core.width;
  mw->menu.windows [0].height = w->core.height;
}

/* Only the toplevel menubar/popup is a widget so it's the only one that
   receives expose events through Xt.  So we repaint all the other panes
   when receiving an Expose event. */
static void
XlwMenuRedisplay (Widget w, XEvent *ev, Region region)
{
  XlwMenuWidget mw = (XlwMenuWidget)w;
  int i;

  if (mw->core.being_destroyed) return;

  for (i = 0; i < mw->menu.old_depth; i++)
    display_menu (mw, i, False, NULL, NULL, NULL, NULL, NULL);
  set_new_state (mw, NULL, mw->menu.old_depth); /* #### - ??? */
  remap_menubar (mw);		/* #### - do these two lines do anything? */
}

static void
XlwMenuDestroy (Widget w)
{
  int i;
  XlwMenuWidget mw = (XlwMenuWidget) w;

  if (mw->menu.pointer_grabbed)
    {
      XtUngrabPointer (w, CurrentTime);
      mw->menu.pointer_grabbed = False;
    }

  release_drawing_gcs (mw);
  release_shadow_gcs  (mw);

  /* this doesn't come from the resource db but is created explicitly
     so we must free it ourselves. */
  XFreePixmap (XtDisplay (mw), mw->menu.gray_pixmap);
  mw->menu.gray_pixmap = (Pixmap) -1;

  /* Don't free mw->menu.contents because that comes from our creator.
     The `*_stack' elements are just pointers into `contents' so leave
     that alone too.  But free the stacks themselves. */
  if (mw->menu.old_stack) XtFree ((char *) mw->menu.old_stack);
  if (mw->menu.new_stack) XtFree ((char *) mw->menu.new_stack);

  /* Remember, you can't free anything that came from the resource
     database.  This includes:
         mw->menu.cursor
         mw->menu.top_shadow_pixmap
         mw->menu.bottom_shadow_pixmap
         mw->menu.font
         mw->menu.font_set
     Also the color cells of top_shadow_color, bottom_shadow_color,
     foreground, and button_foreground will never be freed until this
     client exits.  Nice, eh?
   */

  /* start from 1 because the one in slot 0 is w->core.window */
  for (i = 1; i < mw->menu.windows_length; i++)
    XDestroyWindow (XtDisplay (mw), mw->menu.windows [i].window);
  if (mw->menu.windows)
    XtFree ((char *) mw->menu.windows);
}

static Boolean
XlwMenuSetValues (Widget current, Widget request, Widget new, ArgList args,
		  Cardinal *num_args)
{
  XlwMenuWidget oldmw = (XlwMenuWidget)current;
  XlwMenuWidget newmw = (XlwMenuWidget)new;
  Boolean redisplay = False;
  int i;

  if (newmw->menu.contents
      && newmw->menu.contents->contents
      && newmw->menu.contents->contents->change >= VISIBLE_CHANGE)
    redisplay = True;

  if (newmw->core.background_pixel != oldmw->core.background_pixel
      || newmw->menu.foreground != oldmw->menu.foreground
      /* For the XEditResource protocol, which may want to change the font. */
#ifdef NEED_MOTIF
      || newmw->menu.font_list          != oldmw->menu.font_list
      || newmw->menu.font_list_2        != oldmw->menu.font_list_2
      || newmw->menu.fallback_font_list != oldmw->menu.fallback_font_list
#else
      || newmw->menu.font != oldmw->menu.font
#endif
      )
    {
      release_drawing_gcs (newmw);
      make_drawing_gcs (newmw);
      redisplay = True;

      for (i = 0; i < oldmw->menu.windows_length; i++)
	{
	  XSetWindowBackground (XtDisplay (oldmw),
				oldmw->menu.windows [i].window,
				newmw->core.background_pixel);
	  /* clear windows and generate expose events */
	  XClearArea (XtDisplay (oldmw), oldmw->menu.windows[i].window,
		      0, 0, 0, 0, True);
	}
    }

  return redisplay;
}

static void
XlwMenuResize (Widget w)
{
  XlwMenuWidget mw = (XlwMenuWidget)w;

  mw->menu.windows [0].width  = mw->core.width;
  mw->menu.windows [0].height = mw->core.height;
}

/* Action procedures */
static void
handle_single_motion_event (XlwMenuWidget mw, XMotionEvent *ev,
			    Boolean select_p)
{
  widget_value *val;
  Boolean      stay_up;
  int	       level;

  if (!map_event_to_widget_value (mw, ev, &val, &level, &stay_up))
    {
      /* we wind up here when: (a) the event is in the menubar, (b) the
	 event isn't in the menubar or any of the panes, (c) the event is on
	 a disabled menu item */
      pop_new_stack_if_no_contents (mw);
      if (select_p && !stay_up) {
	/* pop down all menus and exit */
	mw->menu.next_release_must_exit = True;
	set_new_state(mw, (val = NULL), 1);
      }
    }
  else
    {
      /* we wind up here when: (a) the event pops up a pull_right menu,
	 (b) a menu item that is not disabled is highlighted */
      if (select_p && mw->menu.bounce_down
	       && close_to_reference_time((Widget)mw,
					  mw->menu.menu_bounce_time,
					  (XEvent *)ev))
	{
	  /* motion can cause more than one event.  Don't bounce right back
	     up if we've just bounced down. */
	  val = NULL;
	}
      else if (select_p && mw->menu.bounce_down &&
	       mw->menu.last_selected_val &&
	       (mw->menu.last_selected_val == val))
	{
	  val = NULL;		/* assigned to mw->last_selected_val below */
	  mw->menu.menu_bounce_time = ev->time;
	  /* popdown last menu if we're selecting the same menu item as we did
	     last time and the XlwMenu.bounceDown resource is set, if the
	     item is on the menubar itself, then exit. */
	  if (level == (mw->menu.popped_up ? 0 : 1))
	    mw->menu.next_release_must_exit = True;
	}
      else
	mw->menu.menu_bounce_time = 0;
      set_new_state (mw, val, level);
    }
  mw->menu.last_selected_val = val;
  remap_menubar (mw);

  /* Sync with the display.  Makes it feel better on X terms. */
  XFlush (XtDisplay (mw));
}

static void
handle_motion_event (XlwMenuWidget mw, XMotionEvent *ev,
		     Boolean select_p)
{
  int x = ev->x_root;
  int y = ev->y_root;
  unsigned int state = ev->state;
  XMotionEvent *event= ev, dummy;

  /* allow motion events to be generated again */
  dummy.window = ev->window;
  if (ev->is_hint
      && XQueryPointer (XtDisplay (mw), dummy.window,
			&dummy.root, &dummy.subwindow,
			&dummy.x_root, &dummy.y_root,
			&dummy.x, &dummy.y,
			&dummy.state)
      && dummy.state == state
      && (dummy.x_root != x || dummy.y_root != y))
    {
      /* don't handle the event twice or that breaks bounce_down.  --Stig */
      dummy.type = ev->type;
      event = &dummy;
    }

  lw_menu_accelerate = False;
  handle_single_motion_event (mw, event, select_p);
}

Time x_focus_timestamp_really_sucks_fix_me_better;

static void
Start (Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
  XlwMenuWidget mw = (XlwMenuWidget)w;

  lw_menubar_widget = w;

  lw_menu_active = True;

  if (!mw->menu.pointer_grabbed)
    {
      mw->menu.menu_post_time = ev->xbutton.time;
      mw->menu.menu_bounce_time = 0;
      mw->menu.next_release_must_exit = True;
      mw->menu.last_selected_val = NULL;
      x_focus_timestamp_really_sucks_fix_me_better =
	((XButtonPressedEvent*)ev)->time;
      XtCallCallbackList ((Widget)mw, mw->menu.open, NULL);

      /* notes the absolute position of the menubar window */
      mw->menu.windows [0].x = ev->xmotion.x_root - ev->xmotion.x;
      mw->menu.windows [0].y = ev->xmotion.y_root - ev->xmotion.y;

      XtGrabPointer ((Widget)mw, False,
		     (ButtonMotionMask | ButtonReleaseMask | ButtonPressMask),
		     GrabModeAsync, GrabModeAsync,
		     None, mw->menu.cursor_shape,
		     ((XButtonPressedEvent*)ev)->time);
      mw->menu.pointer_grabbed = True;
    }

  /* handles the down like a move, slots are mostly compatible */
  handle_motion_event (mw, &ev->xmotion, True);
}

static void
Drag (Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
  XlwMenuWidget mw = (XlwMenuWidget)w;
  handle_motion_event (mw, &ev->xmotion, False);
}

static void
Select (Widget w, XEvent *ev, String *params, Cardinal *num_params)
{
  XlwMenuWidget mw = (XlwMenuWidget)w;
  widget_value *selected_item = mw->menu.old_stack [mw->menu.old_depth - 1];

  lw_menu_accelerate = False;

  /* If user releases the button quickly, without selecting anything,
     after the initial down-click that brought the menu up,
     do nothing. */
  if ((selected_item == 0 || selected_item->call_data == 0)
      && (!mw->menu.next_release_must_exit
	  || close_to_reference_time(w, mw->menu.menu_post_time, ev)))
    {
      mw->menu.next_release_must_exit = False;
      return;
    }

  /* pop down everything */
  mw->menu.new_depth = 1;
  remap_menubar (mw);

      /* Destroy() only gets called for popup menus.  Menubar widgets aren't
	 destroyed when their menu panes get nuked. */
  if (mw->menu.pointer_grabbed)
    {
      XtUngrabPointer ((Widget)w, ev->xmotion.time);
      mw->menu.pointer_grabbed = False;
    }

  if (mw->menu.popped_up)
    {
      mw->menu.popped_up = False;
      XtPopdown (XtParent (mw));
    }

  lw_menu_active = False;

  x_focus_timestamp_really_sucks_fix_me_better =
    ((XButtonPressedEvent*)ev)->time;

  /* callback */
  XtCallCallbackList ((Widget) mw, mw->menu.select, (XtPointer) selected_item);
}

/* Action procedures for keyboard accelerators */

/* set the menu */
void
xlw_set_menu (Widget w, widget_value *val)
{
  lw_menubar_widget = w;
  set_new_state ((XlwMenuWidget)w, val, 1);
}

/* prepare the menu structure via the call-backs */
void
xlw_map_menu (Time t)
{
  XlwMenuWidget mw = (XlwMenuWidget)lw_menubar_widget;

  lw_menu_accelerate = True;

  if (!mw->menu.pointer_grabbed)
    {
      XWindowAttributes ret;
      Window parent,root;
      Window *waste;
      unsigned int num_waste;

      lw_menu_active = True;

      mw->menu.menu_post_time = t;
      mw->menu.menu_bounce_time = 0;

      mw->menu.next_release_must_exit = True;
      mw->menu.last_selected_val = NULL;

      XtCallCallbackList ((Widget)mw, mw->menu.open, NULL);

      /* do this for keyboards too! */
      /* notes the absolute position of the menubar window */
      /*
      mw->menu.windows [0].x = ev->xmotion.x_root - ev->xmotion.x;
      mw->menu.windows [0].y = ev->xmotion.y_root - ev->xmotion.y;
      */

      /* get the geometry of the menubar */

      /* there has to be a better way than this. */

      mw->menu.windows [0].x = 0;
      mw->menu.windows [0].y = 0;

      parent = XtWindow (lw_menubar_widget);
      do
	{
	  XGetWindowAttributes (XtDisplay (lw_menubar_widget), parent, &ret);
	  mw->menu.windows [0].x += ret.x;
	  mw->menu.windows [0].y += ret.y;

	  if (parent)
	    XQueryTree (XtDisplay (lw_menubar_widget), parent, &root, &parent, &waste,
			&num_waste);
	  if (waste)
	    {
	      XFree (waste);
	    }
	}
      while (parent != root);

      XtGrabPointer ((Widget)mw, False,
		     (ButtonMotionMask | ButtonReleaseMask | ButtonPressMask),
		     GrabModeAsync, GrabModeAsync,
		     None, mw->menu.cursor_shape, t);
      mw->menu.pointer_grabbed = True;
    }
}

/* display the stupid menu already */
void
xlw_display_menu (Time t)
{
  XlwMenuWidget mw = (XlwMenuWidget)lw_menubar_widget;

  lw_menu_accelerate = True;

  remap_menubar (mw);

  /* Sync with the display.  Makes it feel better on X terms. */
  XFlush (XtDisplay (mw));
}

/* push a sub menu */
void
xlw_push_menu (widget_value *val)
{
  push_new_stack ((XlwMenuWidget)lw_menubar_widget, val);
}

/* pop a sub menu */
int
xlw_pop_menu (void)
{
  if (((XlwMenuWidget)lw_menubar_widget)->menu.new_depth > 0)
    ((XlwMenuWidget)lw_menubar_widget)->menu.new_depth --;
  else
    return 0;
  return 1;
}

void
xlw_kill_menus (widget_value *val)
{
  XlwMenuWidget mw = (XlwMenuWidget)lw_menubar_widget;

  lw_menu_accelerate = False;

  mw->menu.new_depth = 1;
  remap_menubar (mw);

  if (mw->menu.pointer_grabbed)
    {
      XtUngrabPointer (lw_menubar_widget, CurrentTime);
      mw->menu.pointer_grabbed = False;
    }

  lw_menu_active = False;
  XtCallCallbackList (lw_menubar_widget, mw->menu.select, (XtPointer)val);
}

/* set the menu item */
void
xlw_set_item (widget_value *val)
{
  if (((XlwMenuWidget)lw_menubar_widget)->menu.new_depth > 0)
    ((XlwMenuWidget) lw_menubar_widget)->menu.new_depth --;
  push_new_stack ((XlwMenuWidget) lw_menubar_widget, val);
}

/* get either the current entry or a list of all entries in the current submenu */
widget_value *
xlw_get_entries (int allp)
{
  XlwMenuWidget mw = (XlwMenuWidget)lw_menubar_widget;
  if (allp)
    {
      if (mw->menu.new_depth >= 2)
	return mw->menu.new_stack [mw->menu.new_depth - 2]->contents;
      else
	return mw->menu.new_stack[0];
    }
  else
    if (mw->menu.new_depth >= 1)
      return mw->menu.new_stack [mw->menu.new_depth - 1];

  return NULL;
}

int
xlw_menu_level (void)
{
  return ((XlwMenuWidget)lw_menubar_widget)->menu.new_depth;
}


/* Special code to pop-up a menu */
void
xlw_pop_up_menu (XlwMenuWidget mw, XButtonPressedEvent *event)
{
  int		x = event->x_root;
  int		y = event->y_root;
  int		w;
  int		h;
  int		borderwidth = mw->menu.shadow_thickness;
  Screen*	screen = XtScreen (mw);

  mw->menu.menu_post_time = event->time;
  mw->menu.menu_bounce_time = 0;
  mw->menu.next_release_must_exit = True;
  mw->menu.last_selected_val = NULL;

  XtCallCallbackList ((Widget) mw, mw->menu.open, NULL);

  size_menu (mw, 0);

  w = mw->menu.windows [0].width;
  h = mw->menu.windows [0].height;

  x -= borderwidth;
  y -= borderwidth;

  if (x < borderwidth)
      x = borderwidth;

  if (x > WidthOfScreen (screen) - w - 2 * borderwidth)
      x = WidthOfScreen (screen) - w - 2 * borderwidth;

  if (y < borderwidth)
      y = borderwidth;

  if (y > HeightOfScreen (screen) - h - 2 * borderwidth)
      y = HeightOfScreen (screen) - h - 2 * borderwidth;

  mw->menu.popped_up = True;
  XtConfigureWidget (XtParent (mw), x, y, w, h,
		     XtParent (mw)->core.border_width);
  XtPopup (XtParent (mw), XtGrabExclusive);
  display_menu (mw, 0, False, NULL, NULL, NULL, NULL, NULL);
  if (!mw->menu.pointer_grabbed)
    {
      XtGrabPointer ((Widget)mw, False,
		     (ButtonMotionMask | ButtonReleaseMask | ButtonPressMask),
		     GrabModeAsync, GrabModeAsync,
		     None, mw->menu.cursor_shape, event->time);
      mw->menu.pointer_grabbed = True;
    }

  mw->menu.windows [0].x = x + borderwidth;
  mw->menu.windows [0].y = y + borderwidth;

  handle_motion_event (mw, (XMotionEvent *) event, True);
}

/* #### unused */
#if 0
/*
 *    This is a horrible function which should not be needed.
 *    use it to put the resize method back the way the XlwMenu
 *    class initializer put it. Motif screws with this when
 *    the XlwMenu class gets instantiated.
 */
void
xlw_unmunge_class_resize (Widget w)
{
  if (w->core.widget_class->core_class.resize != XlwMenuResize)
      w->core.widget_class->core_class.resize  = XlwMenuResize;
}
#endif /* 0 */

