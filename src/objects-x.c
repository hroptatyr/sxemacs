/* X-specific Lisp objects.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Tinker Systems.
   Copyright (C) 1995, 1996, 2000 Ben Wing.
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

/* This file Mule-ized by Ben Wing, 7-10-00. */

#include <config.h>
#include "lisp.h"

#include "console-x.h"
#include "objects-x.h"

#include "buffer.h"
#include "device.h"
#include "insdel.h"

int x_handle_non_fully_specified_fonts;


/************************************************************************/
/*                          color instances                             */
/************************************************************************/

/* Replacement for XAllocColor() that tries to return the nearest
   available color if the colormap is full.  Original was from FSFmacs,
   but rewritten by Jareth Hein <jareth@camelot-soft.com> 97/11/25
   Modified by Lee Kindness <lkindness@csl.co.uk> 31/08/99 to handle previous
   total failure which was due to a read/write colorcell being the nearest
   match - tries the next nearest...

   Return value is 1 for normal success, 2 for nearest color success,
   3 for Non-deallocable success. */
int
allocate_nearest_color (Display *display, Colormap colormap, Visual *visual,
		        XColor *color_def)
{
  int status;

  if (visual->class == DirectColor || visual->class == TrueColor)
    {
      if (XAllocColor (display, colormap, color_def) != 0)
	{
	  status = 1;
	}
      else
	{
	  /* We're dealing with a TrueColor/DirectColor visual, so play games
	     with the RGB values in the XColor struct. */
	  /* #### JH: I'm not sure how a call to XAllocColor can fail in a
	     TrueColor or DirectColor visual, so I will just reformat the
	     request to match the requirements of the visual, and re-issue
	     the request.  If this fails for anybody, I wanna know about it
	     so I can come up with a better plan */

	  unsigned long rshift,gshift,bshift,rbits,gbits,bbits,junk;
	  junk = visual->red_mask;
	  rshift = 0;
	  while ((junk & 0x1) == 0) {
	    junk = junk >> 1;
	    rshift ++;
	  }
	  rbits = 0;
	  while (junk != 0) {
	    junk = junk >> 1;
	    rbits++;
	  }
	  junk = visual->green_mask;
	  gshift = 0;
	  while ((junk & 0x1) == 0) {
	    junk = junk >> 1;
	    gshift ++;
	  }
	  gbits = 0;
	  while (junk != 0) {
	    junk = junk >> 1;
	    gbits++;
	  }
	  junk = visual->blue_mask;
	  bshift = 0;
	  while ((junk & 0x1) == 0) {
	    junk = junk >> 1;
	    bshift ++;
	  }
	  bbits = 0;
	  while (junk != 0) {
	    junk = junk >> 1;
	    bbits++;
 	  }

	  color_def->red = color_def->red >> (16 - rbits);
	  color_def->green = color_def->green >> (16 - gbits);
	  color_def->blue = color_def->blue >> (16 - bbits);
	  if (XAllocColor (display, colormap, color_def) != 0)
	    status = 1;
	  else
  	    {
  	      int rd, gr, bl;
	      /* #### JH: I'm punting here, knowing that doing this will at
		 least draw the color correctly.  However, unless we convert
		 all of the functions that allocate colors (graphics
		 libraries, etc) to use this function doing this is very
		 likely to cause problems later... */

	      if (rbits > 8)
		rd = color_def->red << (rbits - 8);
	      else
		rd = color_def->red >> (8 - rbits);
	      if (gbits > 8)
		gr = color_def->green << (gbits - 8);
	      else
		gr = color_def->green >> (8 - gbits);
	      if (bbits > 8)
		bl = color_def->blue << (bbits - 8);
	      else
		bl = color_def->blue >> (8 - bbits);
	      color_def->pixel = (rd << rshift) | (gr << gshift) | (bl <<
								    bshift);
	      status = 3;
	    }
	}
    }
  else
    {
      XColor *cells = NULL;
      /* JH: I can't believe there's no way to go backwards from a
	 colormap ID and get its visual and number of entries, but X
	 apparently isn't built that way... */
      int no_cells = visual->map_entries;
      status = 0;

      if (XAllocColor (display, colormap, color_def) != 0)
	status = 1;
      else while( status != 2 )
	{
	  /* If we got to this point, the colormap is full, so we're
	     going to try and get the next closest color.  The algorithm used
	     is a least-squares matching, which is what X uses for closest
	     color matching with StaticColor visuals. */
	  int nearest;
	  long nearest_delta, trial_delta;
	  int x;

	  if( cells == NULL )
	    {
	      cells = alloca_array (XColor, no_cells);
	      for (x = 0; x < no_cells; x++)
		cells[x].pixel = x;

	      /* read the current colormap */
	      XQueryColors (display, colormap, cells, no_cells);
	    }

	  nearest = 0;
	  /* I'm assuming CSE so I'm not going to condense this. */
	  nearest_delta = ((((color_def->red >> 8) - (cells[0].red >> 8))
			    * ((color_def->red >> 8) - (cells[0].red >> 8)))
			   +
			   (((color_def->green >> 8) - (cells[0].green >> 8))
			    * ((color_def->green >> 8) - (cells[0].green >>
							  8)))
			   +
			   (((color_def->blue >> 8) - (cells[0].blue >> 8))
			    * ((color_def->blue >> 8) - (cells[0].blue >>
							 8))));
	  for (x = 1; x < no_cells; x++)
	    {
	      trial_delta = ((((color_def->red >> 8) - (cells[x].red >> 8))
			      * ((color_def->red >> 8) - (cells[x].red >> 8)))
			     +
			     (((color_def->green >> 8) - (cells[x].green >> 8))
			      * ((color_def->green >> 8) - (cells[x].green >>
							    8)))
			     +
			     (((color_def->blue >> 8) - (cells[x].blue >> 8))
			      * ((color_def->blue >> 8) - (cells[x].blue >>
							   8))));

	      /* less? Ignore cells marked as previously failing */
	      if( (trial_delta < nearest_delta) &&
		  (cells[x].pixel != ULONG_MAX) )
		{
		  nearest = x;
		  nearest_delta = trial_delta;
		}
	    }
	  color_def->red = cells[nearest].red;
	  color_def->green = cells[nearest].green;
	  color_def->blue = cells[nearest].blue;
	  if (XAllocColor (display, colormap, color_def) != 0)
	    status = 2;
	  else
	    /* LSK: Either the colour map has changed since
	     * we read it, or the colour is allocated
	     * read/write... Mark this cmap entry so it's
	     * ignored in the next iteration.
	     */
	    cells[nearest].pixel = ULONG_MAX;
	}
    }
  return status;
}

static int
x_parse_nearest_color (struct device *d, XColor *color, Lisp_Object name,
		       Error_behavior errb)
{
  Display *dpy   = DEVICE_X_DISPLAY  (d);
  Colormap cmap  = DEVICE_X_COLORMAP (d);
  Visual *visual = DEVICE_X_VISUAL   (d);
  int result;

  xzero (*color);
  {
    const Extbyte *extname;

    LISP_STRING_TO_EXTERNAL (name, extname, Qx_color_name_encoding);
    result = XParseColor (dpy, cmap, extname, color);
  }
  if (!result)
    {
      maybe_signal_simple_error ("Unrecognized color", name, Qcolor, errb);
      return 0;
    }
  result = allocate_nearest_color (dpy, cmap, visual, color);
  if (!result)
    {
      maybe_signal_simple_error ("Couldn't allocate color", name, Qcolor,
				 errb);
      return 0;
    }

  return result;
}

static int
x_initialize_color_instance (Lisp_Color_Instance *c, Lisp_Object name,
			     Lisp_Object device, Error_behavior errb)
{
  XColor color;
  int result;

  result = x_parse_nearest_color (XDEVICE (device), &color, name, errb);

  if (!result)
    return 0;

  /* Don't allocate the data until we're sure that we will succeed,
     or the finalize method may get fucked. */
  c->data = xnew (struct x_color_instance_data);
  if (result == 3)
    COLOR_INSTANCE_X_DEALLOC (c) = 0;
  else
    COLOR_INSTANCE_X_DEALLOC (c) = 1;
  COLOR_INSTANCE_X_COLOR (c) = color;
  return 1;
}

static void
x_print_color_instance (Lisp_Color_Instance *c,
			Lisp_Object printcharfun,
			int escapeflag)
{
  char buf[100];
  XColor color = COLOR_INSTANCE_X_COLOR (c);
  sprintf (buf, " %ld=(%X,%X,%X)",
	   color.pixel, color.red, color.green, color.blue);
  write_c_string (buf, printcharfun);
}

static void
x_finalize_color_instance (Lisp_Color_Instance *c)
{
  if (c->data)
    {
      if (DEVICE_LIVE_P (XDEVICE (c->device)))
	{
	  if (COLOR_INSTANCE_X_DEALLOC (c))
	    {
	      XFreeColors (DEVICE_X_DISPLAY (XDEVICE (c->device)),
			   DEVICE_X_COLORMAP (XDEVICE (c->device)),
			   &COLOR_INSTANCE_X_COLOR (c).pixel, 1, 0);
	    }
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
x_color_instance_equal (Lisp_Color_Instance *c1,
			Lisp_Color_Instance *c2,
			int depth)
{
  XColor color1 = COLOR_INSTANCE_X_COLOR (c1);
  XColor color2 = COLOR_INSTANCE_X_COLOR (c2);
  return ((color1.red == color2.red) &&
	  (color1.green == color2.green) &&
	  (color1.blue == color2.blue));
}

static unsigned long
x_color_instance_hash (Lisp_Color_Instance *c, int depth)
{
  XColor color = COLOR_INSTANCE_X_COLOR (c);
  return HASH3 (color.red, color.green, color.blue);
}

static Lisp_Object
x_color_instance_rgb_components (Lisp_Color_Instance *c)
{
  XColor color = COLOR_INSTANCE_X_COLOR (c);
  return (list3 (make_int (color.red),
		 make_int (color.green),
		 make_int (color.blue)));
}

static int
x_valid_color_name_p (struct device *d, Lisp_Object color)
{
  XColor c;
  Display *dpy = DEVICE_X_DISPLAY (d);
  Colormap cmap = DEVICE_X_COLORMAP (d);
  const Extbyte *extname;

  LISP_STRING_TO_EXTERNAL (color, extname, Qx_color_name_encoding);

  return XParseColor (dpy, cmap, extname, &c);
}


/************************************************************************/
/*                           font instances                             */
/************************************************************************/

static int
x_initialize_font_instance (Lisp_Font_Instance *f, Lisp_Object name,
			    Lisp_Object device, Error_behavior errb)
{
  Display *dpy = DEVICE_X_DISPLAY (XDEVICE (device));
  XFontStruct *xf;
  const Extbyte *extname;

  LISP_STRING_TO_EXTERNAL (f->name, extname, Qx_font_name_encoding);
  xf = XLoadQueryFont (dpy, extname);

  if (!xf)
    {
      maybe_signal_simple_error ("Couldn't load font", f->name,
				 Qfont, errb);
      return 0;
    }

  if (!xf->max_bounds.width)
    {
      /* yes, this has been known to happen. */
      XFreeFont (dpy, xf);
      maybe_signal_simple_error ("X font is too small", f->name,
				 Qfont, errb);
      return 0;
    }

  /* Don't allocate the data until we're sure that we will succeed,
     or the finalize method may get fucked. */
  f->data = xnew (struct x_font_instance_data);
  FONT_INSTANCE_X_TRUENAME (f) = Qnil;
  FONT_INSTANCE_X_FONT (f) = xf;
  f->ascent = xf->ascent;
  f->descent = xf->descent;
  f->height = xf->ascent + xf->descent;
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
		       (x_handle_non_fully_specified_fonts &&
			!xf->all_chars_exist));

  return 1;
}

static void
x_mark_font_instance (Lisp_Font_Instance *f)
{
  mark_object (FONT_INSTANCE_X_TRUENAME (f));
}

static void
x_print_font_instance (Lisp_Font_Instance *f,
		       Lisp_Object printcharfun,
		       int escapeflag)
{
  char buf[200];
  sprintf (buf, " 0x%lx", (unsigned long) FONT_INSTANCE_X_FONT (f)->fid);
  write_c_string (buf, printcharfun);
}

static void
x_finalize_font_instance (Lisp_Font_Instance *f)
{

  if (f->data)
    {
      if (DEVICE_LIVE_P (XDEVICE (f->device)))
	{
	  Display *dpy = DEVICE_X_DISPLAY (XDEVICE (f->device));

	  XFreeFont (dpy, FONT_INSTANCE_X_FONT (f));
	}
      xfree (f->data);
      f->data = 0;
    }
}

/* Determining the truename of a font is hard.  (Big surprise.)

   By "truename" we mean an XLFD-form name which contains no wildcards, yet
   which resolves to *exactly* the same font as the one which we already have
   the (probably wildcarded) name and `XFontStruct' of.

   One might think that the first font returned by XListFonts would be the one
   that XOpenFont would pick.  Apparently this is the case on some servers,
   but not on others.  It would seem not to be specified.

   The MIT R5 server sometimes appears to be picking the lexicographically
   smallest font which matches the name (thus picking "adobe" fonts before
   "bitstream" fonts even if the bitstream fonts are earlier in the path, and
   also picking 100dpi adobe fonts over 75dpi adobe fonts even though the
   75dpi are in the path earlier) but sometimes appears to be doing something
   else entirely (for example, removing the bitstream fonts from the path will
   cause the 75dpi adobe fonts to be used instead of the 100dpi, even though
   their relative positions in the path (and their names!) have not changed).

   The documentation for XSetFontPath() seems to indicate that the order of
   entries in the font path means something, but it's pretty noncommittal about
   it, and the spirit of the law is apparently not being obeyed...

   All the fonts I've seen have a property named `FONT' which contains the
   truename of the font.  However, there are two problems with using this: the
   first is that the X Protocol Document is quite explicit that all properties
   are optional, so we can't depend on it being there.  The second is that
   it's conceivable that this alleged truename isn't actually accessible as a
   font, due to some difference of opinion between the font designers and
   whoever installed the font on the system.

   So, our first attempt is to look for a FONT property, and then verify that
   the name there is a valid name by running XListFonts on it.  There's still
   the potential that this could be true but we could still be being lied to,
   but that seems pretty remote.

     Late breaking news: I've gotten reports that SunOS 4.1.3U1
     with OpenWound 3.0 has a font whose truename is really
     "-Adobe-Courier-Medium-R-Normal--12-120-75-75-M-70-ISO8859-1"
     but whose FONT property contains "Courier".

     So we disbelieve the FONT property unless it begins with a dash and
     is more than 30 characters long.  X Windows: The defacto substandard.
     X Windows: Complex nonsolutions to simple nonproblems.  X Windows:
     Live the nightmare.

   If the FONT property doesn't exist, then we try and construct an XLFD name
   out of the other font properties (FOUNDRY, FAMILY_NAME, WEIGHT_NAME, etc).
   This is necessary at least for some versions of OpenWound.  But who knows
   what the future will bring.

   If that doesn't work, then we use XListFonts and either take the first font
   (which I think is the most sensible thing) or we find the lexicographically
   least, depending on whether the preprocessor constant `XOPENFONT_SORTS' is
   defined.  This sucks because the two behaviors are a property of the server
   being used, not the architecture on which emacs has been compiled.  Also,
   as I described above, sorting isn't ALWAYS what the server does.  Really it
   does something seemingly random.  There is no reliable way to win if the
   FONT property isn't present.

   Another possibility which I haven't bothered to implement would be to map
   over all of the matching fonts and find the first one that has the same
   character metrics as the font we already have loaded.  Even if this didn't
   return exactly the same font, it would at least return one whose characters
   were the same sizes, which would probably be good enough.

   More late-breaking news: on RS/6000 AIX 3.2.4, the expression
        XLoadQueryFont (dpy, "-*-Fixed-Medium-R-*-*-*-130-75-75-*-*-ISO8859-1")
   actually returns the font
        -Misc-Fixed-Medium-R-Normal--13-120-75-75-C-80-ISO8859-1
   which is crazy, because that font doesn't even match that pattern!  It is
   also not included in the output produced by `xlsfonts' with that pattern.

   So this is yet another example of XListFonts() and XOpenFont() using
   completely different algorithms.  This, however, is a goofier example of
   this bug, because in this case, it's not just the search order that is
   different -- the sets don't even intersect.

   If anyone has any better ideas how to do this, or any insights on what it is
   that the various servers are actually doing, please let me know!  -- jwz. */

static int
valid_x_font_name_p (Display *dpy, Extbyte *name)
{
  /* Maybe this should be implemented by calling XLoadFont and trapping
     the error.  That would be a lot of work, and wasteful as hell, but
     might be more correct.
   */
  int nnames = 0;
  Extbyte **names = 0;
  if (! name)
    return 0;
  names = XListFonts (dpy, name, 1, &nnames);
  if (names)
    XFreeFontNames (names);
  return (nnames != 0);
}

static Extbyte *
truename_via_FONT_prop (Display *dpy, XFontStruct *font)
{
  unsigned long value = 0;
  Extbyte *result = 0;
  if (XGetFontProperty (font, XA_FONT, &value))
    result = XGetAtomName (dpy, value);
  /* result is now 0, or the string value of the FONT property. */
  if (result)
    {
      /* Verify that result is an XLFD name (roughly...) */
      if (result [0] != '-' || strlen (result) < (unsigned int) 30)
	{
	  XFree (result);
	  result = 0;
	}
    }
  return result;	/* this must be freed by caller if non-0 */
}

static Extbyte *
truename_via_random_props (Display *dpy, XFontStruct *font)
{
  struct device *d = get_device_from_display (dpy);
  unsigned long value = 0;
  Extbyte *foundry, *family, *weight, *slant, *setwidth, *add_style;
  unsigned long pixel, point, res_x, res_y;
  Extbyte *spacing;
  unsigned long avg_width;
  Extbyte *registry, *encoding;
  Extbyte composed_name [2048];
  int ok = 0;
  Extbyte *result;

#define get_string(atom,var)				\
  if (XGetFontProperty (font, (atom), &value))		\
    var = XGetAtomName (dpy, value);			\
  else	{						\
    var = 0;						\
    goto FAIL; }
#define get_number(atom,var)				\
  if (!XGetFontProperty (font, (atom), &var) ||		\
      var > 999)					\
    goto FAIL;

  foundry = family = weight = slant = setwidth = 0;
  add_style = spacing = registry = encoding = 0;

  get_string (DEVICE_XATOM_FOUNDRY (d), foundry);
  get_string (DEVICE_XATOM_FAMILY_NAME (d), family);
  get_string (DEVICE_XATOM_WEIGHT_NAME (d), weight);
  get_string (DEVICE_XATOM_SLANT (d), slant);
  get_string (DEVICE_XATOM_SETWIDTH_NAME (d), setwidth);
  get_string (DEVICE_XATOM_ADD_STYLE_NAME (d), add_style);
  get_number (DEVICE_XATOM_PIXEL_SIZE (d), pixel);
  get_number (DEVICE_XATOM_POINT_SIZE (d), point);
  get_number (DEVICE_XATOM_RESOLUTION_X (d), res_x);
  get_number (DEVICE_XATOM_RESOLUTION_Y (d), res_y);
  get_string (DEVICE_XATOM_SPACING (d), spacing);
  get_number (DEVICE_XATOM_AVERAGE_WIDTH (d), avg_width);
  get_string (DEVICE_XATOM_CHARSET_REGISTRY (d), registry);
  get_string (DEVICE_XATOM_CHARSET_ENCODING (d), encoding);
#undef get_number
#undef get_string

  sprintf (composed_name,
	   "-%s-%s-%s-%s-%s-%s-%ld-%ld-%ld-%ld-%s-%ld-%s-%s",
	   foundry, family, weight, slant, setwidth, add_style, pixel,
	   point, res_x, res_y, spacing, avg_width, registry, encoding);
  ok = 1;

 FAIL:
  if (ok)
    {
      int L = strlen (composed_name) + 1;
      result = (Extbyte *) xmalloc (L);
      strncpy (result, composed_name, L);
    }
  else
    result = 0;

  if (foundry) XFree (foundry);
  if (family) XFree (family);
  if (weight) XFree (weight);
  if (slant) XFree (slant);
  if (setwidth) XFree (setwidth);
  if (add_style) XFree (add_style);
  if (spacing) XFree (spacing);
  if (registry) XFree (registry);
  if (encoding) XFree (encoding);

  return result;
}

/* Unbounded, for sufficiently small values of infinity... */
#define MAX_FONT_COUNT 5000

static Extbyte *
truename_via_XListFonts (Display *dpy, Extbyte *font_name)
{
  Extbyte *result = 0;
  Extbyte **names;
  int count = 0;

#ifndef XOPENFONT_SORTS
  /* In a sensible world, the first font returned by XListFonts()
     would be the font that XOpenFont() would use.  */
  names = XListFonts (dpy, font_name, 1, &count);
  if (count) result = names [0];
#else
  /* But the world I live in is much more perverse. */
  names = XListFonts (dpy, font_name, MAX_FONT_COUNT, &count);
  while (count--)
    /* !!#### Not Mule-friendly */
    /* If names[count] is lexicographically less than result, use it.
       (#### Should we be comparing case-insensitively?) */
    if (result == 0 || (strcmp (result, names [count]) < 0))
      result = names [count];
#endif

  if (result)
    result = xstrdup (result);
  if (names)
    XFreeFontNames (names);

  return result;	/* this must be freed by caller if non-0 */
}

static Lisp_Object
x_font_truename (Display *dpy, Extbyte *name, XFontStruct *font)
{
  Extbyte *truename_FONT = 0;
  Extbyte *truename_random = 0;
  Extbyte *truename = 0;

  /* The search order is:
     - if FONT property exists, and is a valid name, return it.
     - if the other props exist, and add up to a valid name, return it.
     - if we find a matching name with XListFonts, return it.
     - if FONT property exists, return it regardless.
     - if other props exist, return the resultant name regardless.
     - else return 0.
   */

  truename = truename_FONT = truename_via_FONT_prop (dpy, font);
  if (truename && !valid_x_font_name_p (dpy, truename))
    truename = 0;
  if (!truename)
    truename = truename_random = truename_via_random_props (dpy, font);
  if (truename && !valid_x_font_name_p (dpy, truename))
    truename = 0;
  if (!truename && name)
    truename = truename_via_XListFonts (dpy, name);

  if (!truename)
    {
      /* Gag - we weren't able to find a seemingly-valid truename.
	 Well, maybe we're on one of those braindead systems where
	 XListFonts() and XLoadFont() are in violent disagreement.
	 If we were able to compute a truename, try using that even
	 if evidence suggests that it's not a valid name - because
	 maybe it is, really, and that's better than nothing.
	 X Windows: You'll envy the dead.
       */
      if (truename_FONT)
	truename = truename_FONT;
      else if (truename_random)
	truename = truename_random;
    }

  /* One or both of these are not being used - free them. */
  if (truename_FONT && truename_FONT != truename)
    XFree (truename_FONT);
  if (truename_random && truename_random != truename)
    XFree (truename_random);

  if (truename)
    {
      Lisp_Object result = build_ext_string (truename, Qx_font_name_encoding);
      XFree (truename);
      return result;
    }
  else
    return Qnil;
}

static Lisp_Object
x_font_instance_truename (Lisp_Font_Instance *f, Error_behavior errb)
{
  struct device *d = XDEVICE (f->device);

  if (NILP (FONT_INSTANCE_X_TRUENAME (f)))
    {
      Display *dpy = DEVICE_X_DISPLAY (d);
      {
	Extbyte *nameext;

	LISP_STRING_TO_EXTERNAL (f->name, nameext, Qx_font_name_encoding);
	FONT_INSTANCE_X_TRUENAME (f) =
	  x_font_truename (dpy, nameext, FONT_INSTANCE_X_FONT (f));
      }
      if (NILP (FONT_INSTANCE_X_TRUENAME (f)))
	{
	  Lisp_Object font_instance;
	  XSETFONT_INSTANCE (font_instance, f);

	  maybe_signal_simple_error ("Couldn't determine font truename",
				     font_instance, Qfont, errb);
	  /* Ok, just this once, return the font name as the truename.
	     (This is only used by Fequal() right now.) */
	  return f->name;
	}
    }
  return FONT_INSTANCE_X_TRUENAME (f);
}

static Lisp_Object
x_font_instance_properties (Lisp_Font_Instance *f)
{
  struct device *d = XDEVICE (f->device);
  int i;
  Lisp_Object result = Qnil;
  Display *dpy = DEVICE_X_DISPLAY (d);
  XFontProp *props = FONT_INSTANCE_X_FONT (f)->properties;

  for (i = FONT_INSTANCE_X_FONT (f)->n_properties - 1; i >= 0; i--)
    {
      Lisp_Object name, value;
      Atom atom = props [i].name;
      Bufbyte *name_str = 0;
      size_t name_len;
      Extbyte *namestrext = XGetAtomName (dpy, atom);

      if (namestrext)
	TO_INTERNAL_FORMAT (C_STRING, namestrext,
			    ALLOCA, (name_str, name_len),
			    Qx_atom_name_encoding);

      name = (name_str ? intern ((char *) name_str) : Qnil);
      if (name_str &&
	  (atom == XA_FONT ||
	   atom == DEVICE_XATOM_FOUNDRY (d) ||
	   atom == DEVICE_XATOM_FAMILY_NAME (d) ||
	   atom == DEVICE_XATOM_WEIGHT_NAME (d) ||
	   atom == DEVICE_XATOM_SLANT (d) ||
	   atom == DEVICE_XATOM_SETWIDTH_NAME (d) ||
	   atom == DEVICE_XATOM_ADD_STYLE_NAME (d) ||
	   atom == DEVICE_XATOM_SPACING (d) ||
	   atom == DEVICE_XATOM_CHARSET_REGISTRY (d) ||
	   atom == DEVICE_XATOM_CHARSET_ENCODING (d) ||
	   !bufbyte_strcmp (name_str, "CHARSET_COLLECTIONS") ||
	   !bufbyte_strcmp (name_str, "FONTNAME_REGISTRY") ||
	   !bufbyte_strcmp (name_str, "CLASSIFICATION") ||
	   !bufbyte_strcmp (name_str, "COPYRIGHT") ||
	   !bufbyte_strcmp (name_str, "DEVICE_FONT_NAME") ||
	   !bufbyte_strcmp (name_str, "FULL_NAME") ||
	   !bufbyte_strcmp (name_str, "MONOSPACED") ||
	   !bufbyte_strcmp (name_str, "QUALITY") ||
	   !bufbyte_strcmp (name_str, "RELATIVE_SET") ||
	   !bufbyte_strcmp (name_str, "RELATIVE_WEIGHT") ||
	   !bufbyte_strcmp (name_str, "STYLE")))
	{
	  Extbyte *val_str = XGetAtomName (dpy, props [i].card32);

	  value = (val_str ? build_ext_string (val_str, Qx_atom_name_encoding)
		   : Qnil);
	}
      else
	value = make_int (props [i].card32);
      if (namestrext) XFree (namestrext);
      result = Fcons (Fcons (name, value), result);
    }
  return result;
}

static Lisp_Object
x_list_fonts (Lisp_Object pattern, Lisp_Object device)
{
  Extbyte **names;
  int count = 0;
  Lisp_Object result = Qnil;
  const Extbyte *patternext;

  LISP_STRING_TO_EXTERNAL (pattern, patternext, Qx_font_name_encoding);

  names = XListFonts (DEVICE_X_DISPLAY (XDEVICE (device)),
		      patternext, MAX_FONT_COUNT, &count);
  while (count--)
    result = Fcons (build_ext_string (names[count], Qx_font_name_encoding),
		    result);
  if (names)
    XFreeFontNames (names);
  return result;
}

#ifdef MULE

static int
x_font_spec_matches_charset (struct device *d, Lisp_Object charset,
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
static Lisp_Object
x_find_charset_font (Lisp_Object device, Lisp_Object font, Lisp_Object charset)
{
  Extbyte **names;
  int count = 0;
  Lisp_Object result = Qnil;
  const Extbyte *patternext;
  int i;

  LISP_STRING_TO_EXTERNAL (font, patternext, Qx_font_name_encoding);

  names = XListFonts (DEVICE_X_DISPLAY (XDEVICE (device)),
		      patternext, MAX_FONT_COUNT, &count);
  /* #### This code seems awfully bogus -- mrb */
  for (i = 0; i < count; i ++)
    {
      const Bufbyte *intname;
      Bytecount intlen;

      TO_INTERNAL_FORMAT (C_STRING, names[i],
			  ALLOCA, (intname, intlen),
			  Qx_font_name_encoding);
      if (x_font_spec_matches_charset (XDEVICE (device), charset,
				       intname, Qnil, 0, -1))
	{
	  result = make_string (intname, intlen);
	  break;
	}
    }

  if (names)
    XFreeFontNames (names);

  /* Check for a short font name. */
  if (NILP (result)
      && x_font_spec_matches_charset (XDEVICE (device), charset, 0,
				      font, 0, -1))
    return font;

  return result;
}

#endif /* MULE */


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
syms_of_objects_x (void)
{
}

void
console_type_create_objects_x (void)
{
  /* object methods */

  CONSOLE_HAS_METHOD (x, initialize_color_instance);
  CONSOLE_HAS_METHOD (x, print_color_instance);
  CONSOLE_HAS_METHOD (x, finalize_color_instance);
  CONSOLE_HAS_METHOD (x, color_instance_equal);
  CONSOLE_HAS_METHOD (x, color_instance_hash);
  CONSOLE_HAS_METHOD (x, color_instance_rgb_components);
  CONSOLE_HAS_METHOD (x, valid_color_name_p);

  CONSOLE_HAS_METHOD (x, initialize_font_instance);
  CONSOLE_HAS_METHOD (x, mark_font_instance);
  CONSOLE_HAS_METHOD (x, print_font_instance);
  CONSOLE_HAS_METHOD (x, finalize_font_instance);
  CONSOLE_HAS_METHOD (x, font_instance_truename);
  CONSOLE_HAS_METHOD (x, font_instance_properties);
  CONSOLE_HAS_METHOD (x, list_fonts);
#ifdef MULE
  CONSOLE_HAS_METHOD (x, find_charset_font);
  CONSOLE_HAS_METHOD (x, font_spec_matches_charset);
#endif
}

void
vars_of_objects_x (void)
{
  DEFVAR_BOOL ("x-handle-non-fully-specified-fonts",
	       &x_handle_non_fully_specified_fonts /*
If this is true then fonts which do not have all characters specified
will be considered to be proportional width even if they are actually
fixed-width.  If this is not done then characters which are supposed to
have 0 width may appear to actually have some width.

Note:  While setting this to t guarantees correct output in all
circumstances, it also causes a noticeable performance hit when using
fixed-width fonts.  Since most people don't use characters which could
cause problems this is set to nil by default.
*/ );
  x_handle_non_fully_specified_fonts = 0;
}

void
Xatoms_of_objects_x (struct device *d)
{
  Display *D = DEVICE_X_DISPLAY (d);

  DEVICE_XATOM_FOUNDRY         (d) = XInternAtom (D, "FOUNDRY",         False);
  DEVICE_XATOM_FAMILY_NAME     (d) = XInternAtom (D, "FAMILY_NAME",     False);
  DEVICE_XATOM_WEIGHT_NAME     (d) = XInternAtom (D, "WEIGHT_NAME",     False);
  DEVICE_XATOM_SLANT           (d) = XInternAtom (D, "SLANT",           False);
  DEVICE_XATOM_SETWIDTH_NAME   (d) = XInternAtom (D, "SETWIDTH_NAME",   False);
  DEVICE_XATOM_ADD_STYLE_NAME  (d) = XInternAtom (D, "ADD_STYLE_NAME",  False);
  DEVICE_XATOM_PIXEL_SIZE      (d) = XInternAtom (D, "PIXEL_SIZE",      False);
  DEVICE_XATOM_POINT_SIZE      (d) = XInternAtom (D, "POINT_SIZE",      False);
  DEVICE_XATOM_RESOLUTION_X    (d) = XInternAtom (D, "RESOLUTION_X",    False);
  DEVICE_XATOM_RESOLUTION_Y    (d) = XInternAtom (D, "RESOLUTION_Y",    False);
  DEVICE_XATOM_SPACING         (d) = XInternAtom (D, "SPACING",         False);
  DEVICE_XATOM_AVERAGE_WIDTH   (d) = XInternAtom (D, "AVERAGE_WIDTH",   False);
  DEVICE_XATOM_CHARSET_REGISTRY(d) = XInternAtom (D, "CHARSET_REGISTRY",False);
  DEVICE_XATOM_CHARSET_ENCODING(d) = XInternAtom (D, "CHARSET_ENCODING",False);
}
