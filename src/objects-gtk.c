/* X-specific Lisp objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Tinker Systems.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1995 Sun Microsystems, Inc.

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

/* Authors: Jamie Zawinski, Chuck Thompson, Ben Wing */
/* Gtk version by William Perry */

#include <config.h>
#include "lisp.h"

#include "console-gtk.h"
#include "objects-gtk.h"

#include "buffer.h"
#include "device.h"
#include "insdel.h"

/* sigh */
#include <gdk/gdkx.h>


/************************************************************************/
/*                          color instances                             */
/************************************************************************/

/* Replacement for XAllocColor() that tries to return the nearest
   available color if the colormap is full.  Original was from FSFmacs,
   but rewritten by Jareth Hein <jareth@camelot-soft.com> 97/11/25
   Modified by Lee Kindness <lkindness@csl.co.uk> 31/08/99 to handle previous
   total failure which was due to a read/write colorcell being the nearest
   match - tries the next nearest...

   Gdk takes care of all this behind the scenes, so we don't need to
   worry about it.

   Return value is 1 for normal success, 2 for nearest color success,
   3 for Non-deallocable sucess. */
int
allocate_nearest_color (GdkColormap *colormap, GdkVisual *visual,
		        GdkColor *color_def)
{
  int rc;

  rc = gdk_colormap_alloc_color (colormap, color_def, FALSE, TRUE);

  if (rc == TRUE)
      return (1);

  return (0);
}

int
gtk_parse_nearest_color (struct device *d, GdkColor *color, Bufbyte *name,
			 Bytecount len, Error_behavior errb)
{
  GdkColormap *cmap;
  GdkVisual *visual;
  int result;

  cmap = DEVICE_GTK_COLORMAP(d);
  visual = DEVICE_GTK_VISUAL (d);

  xzero (*color);
  {
    const Extbyte *extname;
    Extcount extnamelen;

    TO_EXTERNAL_FORMAT (DATA, (name, len), ALLOCA, (extname, extnamelen), Qbinary);

    result = gdk_color_parse (extname, color);
  }
  
  if (result == FALSE)
    {
      maybe_signal_simple_error ("unrecognized color", make_string (name, len),
				 Qcolor, errb);
      return 0;
    }
  result = allocate_nearest_color (cmap, visual, color);
  if (!result)
    {
      maybe_signal_simple_error ("couldn't allocate color",
				 make_string (name, len), Qcolor, errb);
      return 0;
    }

  return result;
}

static int
gtk_initialize_color_instance (struct Lisp_Color_Instance *c, Lisp_Object name,
			       Lisp_Object device, Error_behavior errb)
{
  GdkColor color;
  int result;

  result = gtk_parse_nearest_color (XDEVICE (device), &color,
				    XSTRING_DATA   (name),
				    XSTRING_LENGTH (name),
				    errb);

  if (!result)
    return 0;

  /* Don't allocate the data until we're sure that we will succeed,
     or the finalize method may get fucked. */
  c->data = xnew (struct gtk_color_instance_data);
  if (result == 3)
    COLOR_INSTANCE_GTK_DEALLOC (c) = 0;
  else
    COLOR_INSTANCE_GTK_DEALLOC (c) = 1;
  COLOR_INSTANCE_GTK_COLOR (c) = gdk_color_copy (&color);
  return 1;
}

static void
gtk_print_color_instance (struct Lisp_Color_Instance *c,
			  Lisp_Object printcharfun,
			  int escapeflag)
{
  char buf[100];
  GdkColor *color = COLOR_INSTANCE_GTK_COLOR (c);
  sprintf (buf, " %ld=(%X,%X,%X)",
	   color->pixel, color->red, color->green, color->blue);
  write_c_string (buf, printcharfun);
}

static void
gtk_finalize_color_instance (struct Lisp_Color_Instance *c)
{
  if (c->data)
    {
      if (DEVICE_LIVE_P (XDEVICE (c->device)))
	{
	  if (COLOR_INSTANCE_GTK_DEALLOC (c))
	    {
		gdk_colormap_free_colors (DEVICE_GTK_COLORMAP (XDEVICE (c->device)),
					  COLOR_INSTANCE_GTK_COLOR (c), 1);
	    }
	    gdk_color_free (COLOR_INSTANCE_GTK_COLOR (c));
	}
      xfree (c->data);
      c->data = 0;
    }
}

/* Color instances are equal if they resolve to the same color on the
   screen (have the same RGB values).  I imagine that
   "same RGB values" == "same cell in the colormap."  Arguably we should
   be comparing their names or pixel values instead. */

static int
gtk_color_instance_equal (struct Lisp_Color_Instance *c1,
			  struct Lisp_Color_Instance *c2,
			  int depth)
{
    return (gdk_color_equal (COLOR_INSTANCE_GTK_COLOR (c1),
			     COLOR_INSTANCE_GTK_COLOR (c2)));
}

static unsigned long
gtk_color_instance_hash (struct Lisp_Color_Instance *c, int depth)
{
    return (gdk_color_hash (COLOR_INSTANCE_GTK_COLOR (c), NULL));
}

static Lisp_Object
gtk_color_instance_rgb_components (struct Lisp_Color_Instance *c)
{
  GdkColor *color = COLOR_INSTANCE_GTK_COLOR (c);
  return (list3 (make_int (color->red),
		 make_int (color->green),
		 make_int (color->blue)));
}

static int
gtk_valid_color_name_p (struct device *d, Lisp_Object color)
{
  GdkColor c;
  const char *extname;

  TO_EXTERNAL_FORMAT (LISP_STRING, color, C_STRING_ALLOCA, extname, Qctext);

  if (gdk_color_parse (extname, &c) != TRUE)
      return(0);
  return (1);
}


/************************************************************************/
/*                           font instances                             */
/************************************************************************/

static int
gtk_initialize_font_instance (struct Lisp_Font_Instance *f, Lisp_Object name,
			      Lisp_Object device, Error_behavior errb)
{
  GdkFont *gf;
  XFontStruct *xf;
  const char *extname;

  TO_EXTERNAL_FORMAT (LISP_STRING, f->name, C_STRING_ALLOCA, extname, Qctext);

  gf = gdk_font_load (extname);

  if (!gf)
    {
      maybe_signal_simple_error ("couldn't load font", f->name,
				 Qfont, errb);
      return 0;
    }

  xf = GDK_FONT_XFONT (gf);

  /* Don't allocate the data until we're sure that we will succeed,
     or the finalize method may get fucked. */
  f->data = xnew (struct gtk_font_instance_data);
  FONT_INSTANCE_GTK_TRUENAME (f) = Qnil;
  FONT_INSTANCE_GTK_FONT (f) = gf;
  f->ascent = gf->ascent;
  f->descent = gf->descent;
  f->height = gf->ascent + gf->descent;

  /* Now lets figure out the width of the font */
  {
    /* following change suggested by Ted Phelps <phelps@dstc.edu.au> */
    unsigned int def_char = 'n'; /*xf->default_char;*/
    unsigned int byte1, byte2;

  once_more:
    byte1 = def_char >> 8;
    byte2 = def_char & 0xFF;

    if (xf->per_char)
      {
	/* Old versions of the R5 font server have garbage (>63k) as
	   def_char. 'n' might not be a valid character. */
	if (byte1 < xf->min_byte1         ||
	    byte1 > xf->max_byte1         ||
	    byte2 < xf->min_char_or_byte2 ||
	    byte2 > xf->max_char_or_byte2)
	  f->width = 0;
	else
	  f->width = xf->per_char[(byte1 - xf->min_byte1) *
				  (xf->max_char_or_byte2 -
				   xf->min_char_or_byte2 + 1) +
				  (byte2 - xf->min_char_or_byte2)].width;
      }
    else
      f->width = xf->max_bounds.width;

    /* Some fonts have a default char whose width is 0.  This is no good.
       If that's the case, first try 'n' as the default char, and if n has
       0 width too (unlikely) then just use the max width. */
    if (f->width == 0)
      {
	if (def_char == xf->default_char)
	  f->width = xf->max_bounds.width;
	else
	  {
	    def_char = xf->default_char;
	    goto once_more;
	  }
      }
  }

  /* If all characters don't exist then there could potentially be
     0-width characters lurking out there.  Not setting this flag
     trips an optimization that would make them appear to have width
     to redisplay.  This is bad.  So we set it if not all characters
     have the same width or if not all characters are defined.
     */
  /* #### This sucks.  There is a measurable performance increase
     when using proportional width fonts if this flag is not set.
     Unfortunately so many of the fucking X fonts are not fully
     defined that we could almost just get rid of this damn flag and
     make it an assertion. */
  f->proportional_p = (xf->min_bounds.width != xf->max_bounds.width ||
		       (/* x_handle_non_fully_specified_fonts */ 0 &&
			!xf->all_chars_exist));
#if 0
  f->width = gdk_char_width (gf, 'n');
  f->proportional_p = (gdk_char_width (gf, '|') != gdk_char_width (gf, 'W')) ? 1 : 0;
#endif
  return 1;
}

static void
gtk_mark_font_instance (struct Lisp_Font_Instance *f)
{
  mark_object (FONT_INSTANCE_GTK_TRUENAME (f));
}

static void
gtk_print_font_instance (struct Lisp_Font_Instance *f,
			 Lisp_Object printcharfun,
			 int escapeflag)
{
  char buf[200];
  sprintf (buf, " 0x%lx", (unsigned long) gdk_font_id (FONT_INSTANCE_GTK_FONT (f)));
  write_c_string (buf, printcharfun);
}

static void
gtk_finalize_font_instance (struct Lisp_Font_Instance *f)
{
  if (f->data)
    {
      if (DEVICE_LIVE_P (XDEVICE (f->device)))
	{
	    gdk_font_unref (FONT_INSTANCE_GTK_FONT (f));
	}
      xfree (f->data);
      f->data = 0;
    }
}

/* Forward declarations for X specific functions at the end of the file */
Lisp_Object __get_gtk_font_truename (GdkFont *gdk_font, int expandp);
static Lisp_Object __gtk_list_fonts_internal (const char *pattern);

static Lisp_Object
gtk_font_instance_truename (struct Lisp_Font_Instance *f, Error_behavior errb)
{
  if (NILP (FONT_INSTANCE_GTK_TRUENAME (f)))
    {
      FONT_INSTANCE_GTK_TRUENAME (f) = __get_gtk_font_truename (FONT_INSTANCE_GTK_FONT (f), 1);

      if (NILP (FONT_INSTANCE_GTK_TRUENAME (f)))
	{
	  /* Ok, just this once, return the font name as the truename.
	     (This is only used by Fequal() right now.) */
	  return f->name;
	}
    }
  return (FONT_INSTANCE_GTK_TRUENAME (f));
}

static Lisp_Object
gtk_font_instance_properties (struct Lisp_Font_Instance *f)
{
  Lisp_Object result = Qnil;

  /* #### BILL!!! */
  /* There seems to be no way to get this information under Gtk */
  return result;
}

static Lisp_Object
gtk_list_fonts (Lisp_Object pattern, Lisp_Object device)
{
  const char *patternext;

  TO_EXTERNAL_FORMAT (LISP_STRING, pattern, C_STRING_ALLOCA, patternext, Qbinary);

  return (__gtk_list_fonts_internal (patternext));
}

#ifdef MULE

static int
gtk_font_spec_matches_charset (struct device *d, Lisp_Object charset,
			       const Bufbyte *nonreloc, Lisp_Object reloc,
			       Bytecount offset, Bytecount length)
{
  if (UNBOUNDP (charset))
    return 1;
  /* Hack! Short font names don't have the registry in them,
     so we just assume the user knows what they're doing in the
     case of ASCII.  For other charsets, you gotta give the
     long form; sorry buster.
     */
  if (EQ (charset, Vcharset_ascii))
    {
      const Bufbyte *the_nonreloc = nonreloc;
      int i;
      Bytecount the_length = length;

      if (!the_nonreloc)
	the_nonreloc = XSTRING_DATA (reloc);
      fixup_internal_substring (nonreloc, reloc, offset, &the_length);
      the_nonreloc += offset;
      if (!memchr (the_nonreloc, '*', the_length))
	{
	  for (i = 0;; i++)
	    {
	      const Bufbyte *new_nonreloc = (const Bufbyte *)
		memchr (the_nonreloc, '-', the_length);
	      if (!new_nonreloc)
		break;
	      new_nonreloc++;
	      the_length -= new_nonreloc - the_nonreloc;
	      the_nonreloc = new_nonreloc;
	    }

	  /* If it has less than 5 dashes, it's a short font.
	     Of course, long fonts always have 14 dashes or so, but short
	     fonts never have more than 1 or 2 dashes, so this is some
	     sort of reasonable heuristic. */
	  if (i < 5)
	    return 1;
	}
    }

  return (fast_string_match (XCHARSET_REGISTRY (charset),
			     nonreloc, reloc, offset, length, 1,
			     ERROR_ME, 0) >= 0);
}

/* find a font spec that matches font spec FONT and also matches
   (the registry of) CHARSET. */
static Lisp_Object gtk_find_charset_font (Lisp_Object device, Lisp_Object font, Lisp_Object charset);

#endif /* MULE */


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_objects_gtk (void)
{
}

void
console_type_create_objects_gtk (void)
{
  /* object methods */

  CONSOLE_HAS_METHOD (gtk, initialize_color_instance);
  CONSOLE_HAS_METHOD (gtk, print_color_instance);
  CONSOLE_HAS_METHOD (gtk, finalize_color_instance);
  CONSOLE_HAS_METHOD (gtk, color_instance_equal);
  CONSOLE_HAS_METHOD (gtk, color_instance_hash);
  CONSOLE_HAS_METHOD (gtk, color_instance_rgb_components);
  CONSOLE_HAS_METHOD (gtk, valid_color_name_p);

  CONSOLE_HAS_METHOD (gtk, initialize_font_instance);
  CONSOLE_HAS_METHOD (gtk, mark_font_instance);
  CONSOLE_HAS_METHOD (gtk, print_font_instance);
  CONSOLE_HAS_METHOD (gtk, finalize_font_instance);
  CONSOLE_HAS_METHOD (gtk, font_instance_truename);
  CONSOLE_HAS_METHOD (gtk, font_instance_properties);
  CONSOLE_HAS_METHOD (gtk, list_fonts);
#ifdef MULE
  CONSOLE_HAS_METHOD (gtk, find_charset_font);
  CONSOLE_HAS_METHOD (gtk, font_spec_matches_charset);
#endif
}

void
vars_of_objects_gtk (void)
{
}

/* #### BILL!!! Try to make this go away eventually */
/* X Specific stuff */
#include <X11/Xatom.h>

/* Unbounded, for sufficiently small values of infinity... */
#define MAX_FONT_COUNT 5000

#ifdef MULE
/* find a font spec that matches font spec FONT and also matches
   (the registry of) CHARSET. */
static Lisp_Object
gtk_find_charset_font (Lisp_Object device, Lisp_Object font, Lisp_Object charset)
{
  char **names;
  int count = 0;
  Lisp_Object result = Qnil;
  const char *patternext;
  int i;

  TO_EXTERNAL_FORMAT (LISP_STRING, font, C_STRING_ALLOCA, patternext, Qbinary);

  names = XListFonts (GDK_DISPLAY (),
		      patternext, MAX_FONT_COUNT, &count);
  /* ### This code seems awfully bogus -- mrb */
  for (i = 0; i < count; i ++)
    {
      const Bufbyte *intname;
      Bytecount intlen;

      TO_INTERNAL_FORMAT (C_STRING, names[i], ALLOCA, (intname, intlen),
			  Qctext);
      if (gtk_font_spec_matches_charset (XDEVICE (device), charset,
					 intname, Qnil, 0, -1))
	{
	  result = make_string ((char *) intname, intlen);
	  break;
	}
    }

  if (names)
    XFreeFontNames (names);

  /* Check for a short font name. */
  if (NILP (result)
      && gtk_font_spec_matches_charset (XDEVICE (device), charset, 0,
					font, 0, -1))
    return font;

  return result;
}
#endif /* MULE */

/* Unbounded, for sufficiently small values of infinity... */
#define MAX_FONT_COUNT 5000

static int
valid_font_name_p (Display *dpy, char *name)
{
  /* Maybe this should be implemented by callign XLoadFont and trapping
     the error.  That would be a lot of work, and wasteful as hell, but
     might be more correct.
   */
  int nnames = 0;
  char **names = 0;
  if (! name)
    return 0;
  names = XListFonts (dpy, name, 1, &nnames);
  if (names)
    XFreeFontNames (names);
  return (nnames != 0);
}

Lisp_Object
__get_gtk_font_truename (GdkFont *gdk_font, int expandp)
{
  Display *dpy = GDK_FONT_XDISPLAY (gdk_font);
  GSList *names = ((GdkFontPrivate *) gdk_font)->names;
  Lisp_Object font_name = Qnil;

  while (names)
    {
      if (names->data)
	{
	  if (valid_font_name_p (dpy, names->data))
	    {
	      if (!expandp)
		{
		  /* They want the wildcarded version */
		  font_name = build_string (names->data);
		}
	      else
		{
		  /* Need to expand out */
		  int nnames = 0;
		  char **x_font_names = 0;

		  x_font_names = XListFonts (dpy, names->data, 1, &nnames);
		  if (x_font_names)
		    {
		      font_name = build_string (x_font_names[0]);
		      XFreeFontNames (x_font_names);
		    }
		}
	      break;
	    }
	}
      names = names->next;
    }
  return (font_name);
}

static Lisp_Object __gtk_list_fonts_internal (const char *pattern)
{
  char **names;
  int count = 0;
  Lisp_Object result = Qnil;

  names = XListFonts (GDK_DISPLAY (), pattern, MAX_FONT_COUNT, &count);
  while (count--)
    result = Fcons (build_ext_string (names [count], Qbinary), result);
  if (names)
    XFreeFontNames (names);

  return result;
}
