/* X output and frame manipulation routines.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994 Lucid, Inc.
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

/* Synched up with:  Not in FSF. */

/* Author: Chuck Thompson */
/* Gtk flavor by William Perry */

/* Lots of work done by Ben Wing for Mule */

#include <config.h>
#include "lisp.h"

#include "console-gtk.h"
#include "gccache-gtk.h"
#include "glyphs-gtk.h"
#include "objects-gtk.h"

#include "buffer.h"
#include "debug.h"
#include "faces.h"
#include "frame.h"
#include "gutter.h"
#include "redisplay.h"
#include "sysdep.h"
#include "window.h"

#include "sysproc.h" /* for select() */

#ifdef MULE
#include "mule-ccl.h"
#include "file-coding.h" /* for CCL conversion */
#endif

#ifdef HAVE_POLL
#include <sys/poll.h>
#endif

#define CONST const

#define EOL_CURSOR_WIDTH	5

static void gtk_output_pixmap (struct window *w, struct display_line *dl,
			       Lisp_Object image_instance, int xpos,
			       int xoffset,
			       int start_pixpos, int width, face_index findex,
			       int cursor_start, int cursor_width,
			       int cursor_height);
static void gtk_output_vertical_divider (struct window *w, int clear);
static void gtk_output_blank (struct window *w, struct display_line *dl,
			      struct rune *rb, int start_pixpos,
			      int cursor_start, int cursor_width);
static void gtk_output_hline (struct window *w, struct display_line *dl,
			      struct rune *rb);
static void gtk_redraw_exposed_window (struct window *w, int x, int y,
				       int width, int height);
static void gtk_redraw_exposed_windows (Lisp_Object window, int x, int y,
					int width, int height);
static void gtk_clear_region (Lisp_Object locale, struct device* d, struct frame* f,
			      face_index findex, int x, int y,
			      int width, int height, Lisp_Object fcolor, Lisp_Object bcolor,
			      Lisp_Object background_pixmap);
static void gtk_output_eol_cursor (struct window *w, struct display_line *dl,
				   int xpos, face_index findex);
static void gtk_clear_frame (struct frame *f);
static void gtk_clear_frame_windows (Lisp_Object window);
static void gtk_bevel_modeline (struct window *w, struct display_line *dl);

#if 0
static void __describe_gc (GdkGC *);
#endif

struct textual_run
{
  Lisp_Object charset;
  unsigned char *ptr;
  int len;
  int dimension;
};

/* Separate out the text in DYN into a series of textual runs of a
   particular charset.  Also convert the characters as necessary into
   the format needed by XDrawImageString(), XDrawImageString16(), et
   al.  (This means converting to one or two byte format, possibly
   tweaking the high bits, and possibly running a CCL program.) You
   must pre-allocate the space used and pass it in. (This is done so
   you can alloca() the space.)  You need to allocate (2 * len) bytes
   of TEXT_STORAGE and (len * sizeof (struct textual_run)) bytes of
   RUN_STORAGE, where LEN is the length of the dynarr.

   Returns the number of runs actually used. */

static int
separate_textual_runs (unsigned char *text_storage,
		       struct textual_run *run_storage,
		       CONST Emchar *str, Charcount len)
{
  Lisp_Object prev_charset = Qunbound; /* not Qnil because that is a
					  possible valid charset when
					  MULE is not defined */
  int runs_so_far = 0;
  int i;
#ifdef MULE
  struct ccl_program char_converter;
  int need_ccl_conversion = 0;
#endif

  for (i = 0; i < len; i++)
    {
      Emchar ch = str[i];
      Lisp_Object charset;
      int byte1, byte2;
      int dimension;
      int graphic;

      BREAKUP_CHAR (ch, charset, byte1, byte2);
      dimension = XCHARSET_DIMENSION (charset);
      graphic   = XCHARSET_GRAPHIC   (charset);

      if (!EQ (charset, prev_charset))
	{
	  run_storage[runs_so_far].ptr       = text_storage;
	  run_storage[runs_so_far].charset   = charset;
	  run_storage[runs_so_far].dimension = dimension;

	  if (runs_so_far)
	    {
	      run_storage[runs_so_far - 1].len =
		text_storage - run_storage[runs_so_far - 1].ptr;
	      if (run_storage[runs_so_far - 1].dimension == 2)
		run_storage[runs_so_far - 1].len >>= 1;
	    }
	  runs_so_far++;
	  prev_charset = charset;
#ifdef MULE
	  {
	    Lisp_Object ccl_prog = XCHARSET_CCL_PROGRAM (charset);
	    need_ccl_conversion = !NILP (ccl_prog);
	    if (need_ccl_conversion)
	      setup_ccl_program (&char_converter, ccl_prog);
	  }
#endif
	}

      if (graphic == 0)
	{
	  byte1 &= 0x7F;
	  byte2 &= 0x7F;
	}
      else if (graphic == 1)
	{
	  byte1 |= 0x80;
	  byte2 |= 0x80;
	}
#ifdef MULE
      if (need_ccl_conversion)
	{
	  char_converter.reg[0] = XCHARSET_ID (charset);
	  char_converter.reg[1] = byte1;
	  char_converter.reg[2] = byte2;
	  ccl_driver (&char_converter, 0, 0, 0, 0, CCL_MODE_ENCODING);
	  byte1 = char_converter.reg[1];
	  byte2 = char_converter.reg[2];
	}
#endif
      *text_storage++ = (unsigned char) byte1;
      if (dimension == 2)
	*text_storage++ = (unsigned char) byte2;
    }

  if (runs_so_far)
    {
      run_storage[runs_so_far - 1].len =
	text_storage - run_storage[runs_so_far - 1].ptr;
      if (run_storage[runs_so_far - 1].dimension == 2)
	run_storage[runs_so_far - 1].len >>= 1;
    }

  return runs_so_far;
}

/****************************************************************************/
/*                                                                          */
/*                          Gtk output routines                             */
/*                                                                          */
/****************************************************************************/

static int
gtk_text_width_single_run (struct face_cachel *cachel, struct textual_run *run)
{
  Lisp_Object font_inst = FACE_CACHEL_FONT (cachel, run->charset);
  struct Lisp_Font_Instance *fi = XFONT_INSTANCE (font_inst);

  if (!fi->proportional_p)
  {
    return fi->width * run->len;
  }
  else
    {
      if (run->dimension == 2)
	{
	  stderr_out ("Measuring wide characters\n");
	  return gdk_text_width_wc (FONT_INSTANCE_GTK_FONT (fi),
				    (GdkWChar *) run->ptr, run->len);
	}
      else
	{
	  return gdk_text_width (FONT_INSTANCE_GTK_FONT (fi),
				 (char *) run->ptr, run->len);
	}
    }
}

/*
   gtk_text_width

   Given a string and a face, return the string's length in pixels when
   displayed in the font associated with the face.
   */

static int
gtk_text_width (struct frame *f, struct face_cachel *cachel, CONST Emchar *str,
		Charcount len)
{
  int width_so_far = 0;
  unsigned char *text_storage = (unsigned char *) alloca (2 * len);
  struct textual_run *runs = alloca_array (struct textual_run, len);
  int nruns;
  int i;

  nruns = separate_textual_runs (text_storage, runs, str, len);

  for (i = 0; i < nruns; i++)
    width_so_far += gtk_text_width_single_run (cachel, runs + i);

  return width_so_far;
}

/*****************************************************************************
 gtk_divider_height

 Return the height of the horizontal divider.  This is a function because
 divider_height is a device method.

 #### If we add etched horizontal divider lines this will have to get
 smarter.
 ****************************************************************************/
static int
gtk_divider_height (void)
{
  return 2;
}

/*****************************************************************************
 gtk_eol_cursor_width

 Return the width of the end-of-line cursor.  This is a function
 because eol_cursor_width is a device method.
 ****************************************************************************/
static int
gtk_eol_cursor_width (void)
{
  return EOL_CURSOR_WIDTH;
}

/*****************************************************************************
 gtk_output_display_block

 Given a display line, a block number for that start line, output all
 runes between start and end in the specified display block.
 ****************************************************************************/
static void
gtk_output_display_block (struct window *w, struct display_line *dl, int block,
			  int start, int end, int start_pixpos, int cursor_start,
			  int cursor_width, int cursor_height)
{
  struct frame *f = XFRAME (w->frame);
  Emchar_dynarr *buf = Dynarr_new (Emchar);
  Lisp_Object window;

  struct display_block *db = Dynarr_atp (dl->display_blocks, block);
  rune_dynarr *rba = db->runes;
  struct rune *rb;

  int elt = start;
  face_index findex;
  int xpos, width;
  Lisp_Object charset = Qunbound; /* Qnil is a valid charset when
				     MULE is not defined */

  XSETWINDOW (window, w);
  rb = Dynarr_atp (rba, start);

  if (!rb)
    {
      /* Nothing to do so don't do anything. */
      return;
    }
  else
    {
      findex = rb->findex;
      xpos = rb->xpos;
      width = 0;
      if (rb->type == RUNE_CHAR)
	charset = CHAR_CHARSET (rb->object.chr.ch);
    }

  if (end < 0)
    end = Dynarr_length (rba);
  Dynarr_reset (buf);

  while (elt < end)
    {
      rb = Dynarr_atp (rba, elt);

      if (rb->findex == findex && rb->type == RUNE_CHAR
	  && rb->object.chr.ch != '\n' && rb->cursor_type != CURSOR_ON
	  && EQ (charset, CHAR_CHARSET (rb->object.chr.ch)))
	{
	  Dynarr_add (buf, rb->object.chr.ch);
	  width += rb->width;
	  elt++;
	}
      else
	{
	  if (Dynarr_length (buf))
	    {
	      gtk_output_string (w, dl, buf, xpos, 0, start_pixpos, width,
				 findex, 0, cursor_start, cursor_width,
				 cursor_height);
	      xpos = rb->xpos;
	      width = 0;
	    }
	  Dynarr_reset (buf);
	  width = 0;

	  if (rb->type == RUNE_CHAR)
	    {
	      findex = rb->findex;
	      xpos = rb->xpos;
	      charset = CHAR_CHARSET (rb->object.chr.ch);

	      if (rb->cursor_type == CURSOR_ON)
		{
		  if (rb->object.chr.ch == '\n')
		    {
		      gtk_output_eol_cursor (w, dl, xpos, findex);
		    }
		  else
		    {
		      Dynarr_add (buf, rb->object.chr.ch);
		      gtk_output_string (w, dl, buf, xpos, 0, start_pixpos,
					 rb->width, findex, 1,
					 cursor_start, cursor_width,
					 cursor_height);
		      Dynarr_reset (buf);
		    }

		  xpos += rb->width;
		  elt++;
		}
	      else if (rb->object.chr.ch == '\n')
		{
		  /* Clear in case a cursor was formerly here. */
		  int height = dl->ascent + dl->descent - dl->clip;

		  redisplay_clear_region (window, findex, xpos, dl->ypos - dl->ascent,
					  rb->width, height);
		  elt++;
		}
	    }
	  else if (rb->type == RUNE_BLANK || rb->type == RUNE_HLINE)
	    {
	      if (rb->type == RUNE_BLANK)
		gtk_output_blank (w, dl, rb, start_pixpos, cursor_start,
				  cursor_width);
	      else
		{
		  /* #### Our flagging of when we need to redraw the
                     modeline shadows sucks.  Since RUNE_HLINE is only used
                     by the modeline at the moment it is a good bet
                     that if it gets redrawn then we should also
                     redraw the shadows.  This won't be true forever.
                     We borrow the shadow_thickness_changed flag for
                     now. */
		  w->shadow_thickness_changed = 1;
		  gtk_output_hline (w, dl, rb);
		}

	      elt++;
	      if (elt < end)
		{
		  rb = Dynarr_atp (rba, elt);

		  findex = rb->findex;
		  xpos = rb->xpos;
		}
	    }
	  else if (rb->type == RUNE_DGLYPH)
	    {
	      Lisp_Object instance;
	      struct display_box dbox;
	      struct display_glyph_area dga;
	      redisplay_calculate_display_boxes (dl, rb->xpos, rb->object.dglyph.xoffset,
						 rb->object.dglyph.yoffset ,start_pixpos,
                                                 rb->width, &dbox, &dga);

	      XSETWINDOW (window, w);
	      instance = glyph_image_instance (rb->object.dglyph.glyph,
					       window, ERROR_ME_NOT, 1);
	      findex = rb->findex;

	      if (IMAGE_INSTANCEP (instance))
		switch (XIMAGE_INSTANCE_TYPE (instance))
		  {
		  case IMAGE_TEXT:
		    {
		      /* #### This is way losing.  See the comment in
			 add_glyph_rune(). */
		      Lisp_Object string =
			XIMAGE_INSTANCE_TEXT_STRING (instance);
		      convert_bufbyte_string_into_emchar_dynarr
			(XSTRING_DATA (string), XSTRING_LENGTH (string), buf);

		      gtk_output_string (w, dl, buf, xpos,
					 rb->object.dglyph.xoffset,
					 start_pixpos, -1, findex,
					 (rb->cursor_type == CURSOR_ON),
					 cursor_start, cursor_width,
					 cursor_height);
		      Dynarr_reset (buf);
		    }
		    break;

		  case IMAGE_MONO_PIXMAP:
		  case IMAGE_COLOR_PIXMAP:
		    gtk_output_pixmap (w, dl, instance, xpos,
				       rb->object.dglyph.xoffset, start_pixpos,
				       rb->width, findex, cursor_start,
				       cursor_width, cursor_height);
		    break;

		  case IMAGE_POINTER:
		    abort ();

		  case IMAGE_WIDGET:
		      if (EQ (XIMAGE_INSTANCE_WIDGET_TYPE (instance),
			      Qlayout))
			{
			  redisplay_output_layout (window, instance, &dbox,
						   &dga, findex,
						   cursor_start, cursor_width,
						   cursor_height);
			  break;
			}

		  case IMAGE_SUBWINDOW:
		    redisplay_output_subwindow (w, instance, &dbox, &dga,
						findex, cursor_start,
						cursor_width, cursor_height);
		    break;

		  case IMAGE_NOTHING:
		    /* nothing is as nothing does */
		    break;

		  default:
		    abort ();
		  }

	      xpos += rb->width;
	      elt++;
	    }
	  else
	    abort ();
	}
    }

  if (Dynarr_length (buf))
    gtk_output_string (w, dl, buf, xpos, 0, start_pixpos, width, findex,
		       0, cursor_start, cursor_width, cursor_height);

  /* #### This is really conditionalized well for optimized
     performance. */
  if (dl->modeline
      && !EQ (Qzero, w->modeline_shadow_thickness)
      && (f->clear
	  || f->windows_structure_changed
	  || w->shadow_thickness_changed))
    gtk_bevel_modeline (w, dl);

  Dynarr_free (buf);
}

/*****************************************************************************
 gtk_bevel_modeline

 Draw a 3d border around the modeline on window W.
 ****************************************************************************/
static void
gtk_bevel_modeline (struct window *w, struct display_line *dl)
{
  struct frame *f = XFRAME (w->frame);
  int shadow_thickness = MODELINE_SHADOW_THICKNESS (w);
  int x,y, width, height;

  x = WINDOW_MODELINE_LEFT (w);
  width = WINDOW_MODELINE_RIGHT (w) - x;
  y = dl->ypos - dl->ascent - shadow_thickness;
  height = dl->ascent + dl->descent + 2 * shadow_thickness;

  gtk_output_shadows (f, x, y, width, height, shadow_thickness);
}

/*****************************************************************************
 gtk_get_gc

 Given a number of parameters return a GC with those properties.
 ****************************************************************************/
GdkGC *
gtk_get_gc (struct device *d, Lisp_Object font, Lisp_Object fg, Lisp_Object bg,
	    Lisp_Object bg_pmap, Lisp_Object lwidth)
{
  GdkGCValues gcv;
  unsigned long mask;

  memset (&gcv, ~0, sizeof (gcv));
  gcv.graphics_exposures = FALSE;
  /* Make absolutely sure that we don't pick up a clipping region in
     the GC returned by this function. */
  gcv.clip_mask = 0;
  gcv.clip_x_origin = 0;
  gcv.clip_y_origin = 0;
  gcv.fill = GDK_SOLID;
  mask = GDK_GC_EXPOSURES | GDK_GC_CLIP_MASK | GDK_GC_CLIP_X_ORIGIN | GDK_GC_CLIP_Y_ORIGIN;
  mask |= GDK_GC_FILL;

  if (!NILP (font))
    {
      gcv.font = FONT_INSTANCE_GTK_FONT (XFONT_INSTANCE (font));
      mask |= GDK_GC_FONT;
    }

  /* evil kludge! */
  if (!NILP (fg) && !COLOR_INSTANCEP (fg) && !INTP (fg))
    {
      /* #### I fixed once case where this was getting it.  It was a
         bad macro expansion (compiler bug). */
      fprintf (stderr, "Help! gtk_get_gc got a bogus fg value! fg = ");
      debug_print (fg);
      fg = Qnil;
    }

  if (!NILP (fg))
    {
      if (COLOR_INSTANCEP (fg))
	gcv.foreground = * COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (fg));
      else
	gcv.foreground.pixel = XINT (fg);
      mask |= GDK_GC_FOREGROUND;
    }

  if (!NILP (bg))
    {
      if (COLOR_INSTANCEP (bg))
	gcv.background = * COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (bg));
      else
	gcv.background.pixel = XINT (fg);
      mask |= GDK_GC_BACKGROUND;
    }

  if (IMAGE_INSTANCEP (bg_pmap)
      && IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (bg_pmap)))
    {
      if (XIMAGE_INSTANCE_PIXMAP_DEPTH (bg_pmap) == 0)
	{
	  gcv.fill = GDK_OPAQUE_STIPPLED;
	  gcv.stipple = XIMAGE_INSTANCE_GTK_PIXMAP (bg_pmap);
	  mask |= (GDK_GC_STIPPLE | GDK_GC_FILL);
	}
      else
	{
	  gcv.fill = GDK_TILED;
	  gcv.tile = XIMAGE_INSTANCE_GTK_PIXMAP (bg_pmap);
	  mask |= (GDK_GC_TILE | GDK_GC_FILL);
	}
    }

  if (!NILP (lwidth))
    {
      gcv.line_width = XINT (lwidth);
      mask |= GDK_GC_LINE_WIDTH;
    }

  return gc_cache_lookup (DEVICE_GTK_GC_CACHE (d), &gcv, mask);
}

/*****************************************************************************
 gtk_output_string

 Given a string and a starting position, output that string in the
 given face.  If cursor is true, draw a cursor around the string.
 Correctly handles multiple charsets in the string.

 The meaning of the parameters is something like this:

 W		Window that the text is to be displayed in.
 DL		Display line that this text is on.  The values in the
 		structure are used to determine the vertical position and
		clipping range of the text.
 BUF		Dynamic array of Emchars specifying what is actually to be
		drawn.
 XPOS		X position in pixels where the text should start being drawn.
 XOFFSET	Number of pixels to be chopped off the left side of the
 		text.  The effect is as if the text were shifted to the
		left this many pixels and clipped at XPOS.
 CLIP_START	Clip everything left of this X position.
 WIDTH		Clip everything right of XPOS + WIDTH.
 FINDEX		Index for the face cache element describing how to display
 		the text.
 CURSOR		#### I don't understand this.  There's something
 		strange and overcomplexified with this variable.
		Chuck, explain please?
 CURSOR_START	Starting X position of cursor.
 CURSOR_WIDTH	Width of cursor in pixels.
 CURSOR_HEIGHT	Height of cursor in pixels.

 Starting Y position of cursor is the top of the text line.
 The cursor is drawn sometimes whether or not CURSOR is set. ???
 ****************************************************************************/
void
gdk_draw_text_image (GdkDrawable *drawable,
		     GdkFont     *font,
		     GdkGC       *gc,
		     gint         x,
		     gint         y,
		     const gchar *text,
		     gint         text_length);

void
gtk_output_string (struct window *w, struct display_line *dl,
		   Emchar_dynarr *buf, int xpos, int xoffset, int clip_start,
		   int width, face_index findex, int cursor,
		   int cursor_start, int cursor_width, int cursor_height)
{
  /* General variables */
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  Lisp_Object device;
  Lisp_Object window;
  GdkWindow *x_win = GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (f));

  int clip_end;

  /* Cursor-related variables */
  int focus = EQ (w->frame, DEVICE_FRAME_WITH_FOCUS_REAL (d));
  int cursor_clip;
  Lisp_Object bar_cursor_value = symbol_value_in_buffer (Qbar_cursor,
							 WINDOW_BUFFER (w));
  struct face_cachel *cursor_cachel = 0;

  /* Text-related variables */
  Lisp_Object bg_pmap;
  GdkGC *bgc, *gc;
  int height;
  int len = Dynarr_length (buf);
  unsigned char *text_storage = (unsigned char *) alloca (2 * len);
  struct textual_run *runs = alloca_array (struct textual_run, len);
  int nruns;
  int i;
  struct face_cachel *cachel = WINDOW_FACE_CACHEL (w, findex);

  XSETDEVICE (device, d);
  XSETWINDOW (window, w);

  if (width < 0)
    width = gtk_text_width (f, cachel, Dynarr_atp (buf, 0), Dynarr_length (buf));
  height = dl->ascent + dl->descent - dl->clip;

  /* Regularize the variables passed in. */

  if (clip_start < xpos)
    clip_start = xpos;
  clip_end = xpos + width;
  if (clip_start >= clip_end)
    /* It's all clipped out. */
    return;

  xpos -= xoffset;

  nruns = separate_textual_runs (text_storage, runs, Dynarr_atp (buf, 0),
				 Dynarr_length (buf));

  cursor_clip = (cursor_start >= clip_start &&
		 cursor_start < clip_end);

  /* This cursor code is really a mess. */
  if (!NILP (w->text_cursor_visible_p)
      && (cursor
	  || cursor_clip
	  || (cursor_width
	      && (cursor_start + cursor_width >= clip_start)
	      && !NILP (bar_cursor_value))))
    {
      /* These have to be in separate statements in order to avoid a
         compiler bug. */
      face_index sucks = get_builtin_face_cache_index (w, Vtext_cursor_face);
      cursor_cachel = WINDOW_FACE_CACHEL (w, sucks);

      /* We have to reset this since any call to WINDOW_FACE_CACHEL
         may cause the cache to resize and any pointers to it to
         become invalid. */
      cachel = WINDOW_FACE_CACHEL (w, findex);
    }

  bg_pmap = cachel->background_pixmap;
  if (!IMAGE_INSTANCEP (bg_pmap)
      || !IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (bg_pmap)))
    bg_pmap = Qnil;

  if ((cursor && focus && NILP (bar_cursor_value)
       && !NILP (w->text_cursor_visible_p)) || NILP (bg_pmap))
    bgc = 0;
  else
    bgc = gtk_get_gc (d, Qnil, cachel->foreground, cachel->background,
		      bg_pmap, Qnil);

  if (bgc)
    gdk_draw_rectangle (GDK_DRAWABLE (x_win), bgc, TRUE, clip_start,
			dl->ypos - dl->ascent, clip_end - clip_start,
			height);

  for (i = 0; i < nruns; i++)
    {
      Lisp_Object font = FACE_CACHEL_FONT (cachel, runs[i].charset);
      struct Lisp_Font_Instance *fi = XFONT_INSTANCE (font);
      GdkFont *gdk_font = FONT_INSTANCE_GTK_FONT (fi);
      int this_width;
      int need_clipping;

      if (EQ (font, Vthe_null_font_instance))
	continue;

      this_width = gtk_text_width_single_run (cachel, runs + i);
      need_clipping = (dl->clip || clip_start > xpos ||
		       clip_end < xpos + this_width);

      /* XDrawImageString only clears the area equal to the height of
	 the given font.  It is possible that a font is being displayed
	 on a line taller than it is, so this would cause us to fail to
	 clear some areas. */
      if ((int) fi->height < (int) (height + dl->clip))
	{
	  int clear_start = max (xpos, clip_start);
	  int clear_end = min (xpos + this_width, clip_end);

	  if (cursor)
	    {
	      int ypos1_line, ypos1_string, ypos2_line, ypos2_string;

	      ypos1_string = dl->ypos - fi->ascent;
	      ypos2_string = dl->ypos + fi->descent;
	      ypos1_line = dl->ypos - dl->ascent;
	      ypos2_line = dl->ypos + dl->descent - dl->clip;

	      /* Make sure we don't clear below the real bottom of the
		 line. */
	      if (ypos1_string > ypos2_line)
		ypos1_string = ypos2_line;
	      if (ypos2_string > ypos2_line)
		ypos2_string = ypos2_line;

	      if (ypos1_line < ypos1_string)
		{
		  redisplay_clear_region (window, findex, clear_start, ypos1_line,
				    clear_end - clear_start,
				    ypos1_string - ypos1_line);
		}

	      if (ypos2_line > ypos2_string)
		{
		  redisplay_clear_region (window, findex, clear_start, ypos2_string,
					  clear_end - clear_start,
					  ypos2_line - ypos2_string);
		}
	    }
	  else
	    {
	      redisplay_clear_region (window, findex, clear_start,
				      dl->ypos - dl->ascent, clear_end - clear_start,
				      height);
	    }
	}

      if (cursor && cursor_cachel && focus && NILP (bar_cursor_value))
      {
	gc = gtk_get_gc (d, font, cursor_cachel->foreground,
			 cursor_cachel->background, Qnil, Qnil);
      }
      else
      {
	gc = gtk_get_gc (d, font, cachel->foreground, cachel->background,
			 Qnil, Qnil);
      }

      if (need_clipping)
	{
	  GdkRectangle clip_box;

	  clip_box.x = 0;
	  clip_box.y = 0;
	  clip_box.width = clip_end - clip_start;
	  clip_box.height = height;

	  gdk_gc_set_clip_rectangle (gc, &clip_box);
	  gdk_gc_set_clip_origin (gc, clip_start, dl->ypos - dl->ascent);
	}

      /* The X specific called different functions (XDraw*String
         vs. XDraw*String16), but apparently gdk_draw_text takes care
         of that for us.

	 BUT, gdk_draw_text also does too much, by dividing the length
	 by 2.  So we fake them out my multiplying the length by the
	 dimension of the text.  This will do the right thing for
	 single-dimension runs as well of course.
      */
      (bgc ? gdk_draw_text : gdk_draw_text_image) (GDK_DRAWABLE (x_win), gdk_font, gc, xpos,
						   dl->ypos, (char *) runs[i].ptr,
						   runs[i].len * runs[i].dimension);

      /* We draw underlines in the same color as the text. */
      if (cachel->underline)
	{
	  unsigned long upos, uthick;

	  /* Cannot get at font properties in Gtk, so we resort to
             guessing */
	  upos = dl->descent / 2;
	  uthick = 1;

	  if (dl->ypos + upos < dl->ypos + dl->descent - dl->clip)
	    {
	      if (dl->ypos + upos + uthick > dl->ypos + dl->descent - dl->clip)
		uthick = dl->descent - dl->clip - upos;

	      if (uthick == 1)
		{
		  gdk_draw_line (GDK_DRAWABLE (x_win), gc, xpos, dl->ypos + upos,
			     xpos + this_width, dl->ypos + upos);
		}
	      else if (uthick > 1)
		{
		    gdk_draw_rectangle (GDK_DRAWABLE (x_win), gc, TRUE, xpos,
					dl->ypos + upos, this_width, uthick);
		}
	    }
	}

      if (cachel->strikethru) {
	unsigned long ascent,descent,upos, uthick;
	GdkFont *gfont = FONT_INSTANCE_GTK_FONT (XFONT_INSTANCE (font));

	/* Cannot get at font properties in Gtk, so we resort to
           guessing */

	ascent = gfont->ascent;
	descent = gfont->descent;
	uthick = 1;

	upos = ascent - ((ascent + descent) / 2) + 1;

	/* Generally, upos will be positive (above the baseline),so subtract */
	if (dl->ypos - upos < dl->ypos + dl->descent - dl->clip)
	  {
	    if (dl->ypos - upos + uthick > dl->ypos + dl->descent - dl->clip)
	      uthick = dl->descent - dl->clip + upos;

	    if (uthick == 1)
	      {
		  gdk_draw_line (GDK_DRAWABLE (x_win), gc, xpos, dl->ypos - upos,
				 xpos + this_width, dl->ypos - upos);
	      }
	    else if (uthick > 1)
	      {
		  gdk_draw_rectangle (GDK_DRAWABLE (x_win), gc, TRUE, xpos, dl->ypos + upos,
				      this_width, uthick);
	      }
	  }
      }

      /* Restore the GC */
      if (need_clipping)
	{
	    gdk_gc_set_clip_rectangle (gc, NULL);
	    gdk_gc_set_clip_origin (gc, 0, 0);
	}

      /* If we are actually superimposing the cursor then redraw with just
	 the appropriate section highlighted. */
      if (cursor_clip && !cursor && focus && cursor_cachel)
	{
	  GdkGC *cgc;
	  GdkRectangle clip_box;

	  cgc = gtk_get_gc (d, font, cursor_cachel->foreground,
			    cursor_cachel->background, Qnil, Qnil);

	  clip_box.x = 0;
	  clip_box.y = 0;
	  clip_box.width = cursor_width;
	  clip_box.height = height;

	  gdk_gc_set_clip_rectangle (cgc, &clip_box);
	  gdk_gc_set_clip_origin (cgc, cursor_start, dl->ypos - dl->ascent);

	  /* The X specific called different functions (XDraw*String
	     vs. XDraw*String16), but apparently gdk_draw_text takes care
	     of that for us.

	     BUT, gdk_draw_text also does too much, by dividing the
	     length by 2.  So we fake them out my multiplying the
	     length by the dimension of the text.  This will do the
	     right thing for single-dimension runs as well of course.
	  */
	  gdk_draw_text_image (GDK_DRAWABLE (x_win), gdk_font, cgc, xpos,
			       dl->ypos, (char *) runs[i].ptr,
			       runs[i].len * runs[i].dimension);

	  gdk_gc_set_clip_rectangle (cgc, NULL);
	  gdk_gc_set_clip_origin (cgc, 0, 0);
	}

      xpos += this_width;
    }

  /* Draw the non-focus box or bar-cursor as needed. */
  /* Can't this logic be simplified? */
  if (cursor_cachel
      && ((cursor && !focus && NILP (bar_cursor_value))
	  || (cursor_width
	      && (cursor_start + cursor_width >= clip_start)
	      && !NILP (bar_cursor_value))))
    {
      int tmp_height, tmp_y;
      int bar_width = EQ (bar_cursor_value, Qt) ? 1 : 2;
      int need_clipping = (cursor_start < clip_start
			   || clip_end < cursor_start + cursor_width);

      /* #### This value is correct (as far as I know) because
	 all of the times we need to draw this cursor, we will
	 be called with exactly one character, so we know we
	 can always use runs[0].

	 This is bogus as all hell, however.  The cursor handling in
	 this function is way bogus and desperately needs to be
	 cleaned up. (In particular, the drawing of the cursor should
	 really really be separated out of this function.  This may be
	 a bit tricky now because this function itself does way too
	 much stuff, a lot of which needs to be moved into
	 redisplay.c) This is the only way to be able to easily add
	 new cursor types or (e.g.) make the bar cursor be able to
	 span two characters instead of overlaying just one. */
      int bogusly_obtained_ascent_value =
	XFONT_INSTANCE (FACE_CACHEL_FONT (cachel, runs[0].charset))->ascent;

      if (!NILP (bar_cursor_value))
	{
	  gc = gtk_get_gc (d, Qnil, cursor_cachel->background, Qnil, Qnil,
			   make_int (bar_width));
	}
      else
	{
	  gc = gtk_get_gc (d, Qnil, cursor_cachel->background,
			   Qnil, Qnil, Qnil);
	}

      tmp_y = dl->ypos - bogusly_obtained_ascent_value;
      tmp_height = cursor_height;
      if (tmp_y + tmp_height > (int) (dl->ypos - dl->ascent + height))
	{
	  tmp_y = dl->ypos - dl->ascent + height - tmp_height;
	  if (tmp_y < (int) (dl->ypos - dl->ascent))
	    tmp_y = dl->ypos - dl->ascent;
	  tmp_height = dl->ypos - dl->ascent + height - tmp_y;
	}

      if (need_clipping)
	{
	  GdkRectangle clip_box;
	  clip_box.x = 0;
	  clip_box.y = 0;
	  clip_box.width = clip_end - clip_start;
	  clip_box.height = tmp_height;

	  gdk_gc_set_clip_rectangle (gc, &clip_box);
	  gdk_gc_set_clip_origin (gc, clip_start, tmp_y);
	}

      if (!focus && NILP (bar_cursor_value))
	{
	    gdk_draw_rectangle (GDK_DRAWABLE (x_win), gc, FALSE,
				cursor_start, tmp_y,
				cursor_width - 1, tmp_height - 1);
	}
      else if (focus && !NILP (bar_cursor_value))
	{
	    gdk_draw_line (GDK_DRAWABLE (x_win), gc,
			   cursor_start + bar_width - 1, tmp_y,
			   cursor_start + bar_width - 1, tmp_y + tmp_height - 1);
	}

      /* Restore the GC */
      if (need_clipping)
	{
	    gdk_gc_set_clip_rectangle (gc, NULL);
	    gdk_gc_set_clip_origin (gc, 0, 0);
	}
    }
}

static void
our_draw_bitmap (GdkDrawable *drawable,
		 GdkGC       *gc,
		 GdkPixmap   *src,
		 gint         xsrc,
		 gint         ysrc,
		 gint         xdest,
		 gint         ydest,
		 gint         width,
		 gint         height);

void
gtk_output_gdk_pixmap (struct frame *f, struct Lisp_Image_Instance *p, int x,
		       int y, int clip_x, int clip_y, int clip_width,
		       int clip_height, int width, int height, int pixmap_offset,
		       GdkColor *fg, GdkColor *bg, GdkGC *override_gc)
{
  struct device *d = XDEVICE (f->device);
  GdkWindow *x_win = GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (f));

  GdkGC *gc;
  GdkGCValues gcv;
  unsigned long pixmap_mask;
  int need_clipping = (clip_x || clip_y);

  if (!override_gc)
    {
      memset (&gcv, ~0, sizeof (gcv));
      gcv.graphics_exposures = FALSE;
      gcv.foreground = *fg;
      gcv.background = *bg;
      pixmap_mask = GDK_GC_FOREGROUND | GDK_GC_BACKGROUND | GDK_GC_EXPOSURES;

      if (IMAGE_INSTANCE_GTK_MASK (p))
	{
	  gcv.function = GDK_COPY;
	  gcv.clip_mask = IMAGE_INSTANCE_GTK_MASK (p);
	  gcv.clip_x_origin = x;
	  gcv.clip_y_origin = y - pixmap_offset;
	  pixmap_mask |= (GDK_GC_FUNCTION | GDK_GC_CLIP_MASK | GDK_GC_CLIP_X_ORIGIN |
			  GDK_GC_CLIP_Y_ORIGIN);
	  /* Can't set a clip rectangle below because we already have a mask.
	     We could conceivably create a new clipmask by zeroing out
	     everything outside the clip region.  Is it worth it?
	     Is it possible to get an equivalent effect by changing the
	     args to XCopyArea below rather than messing with a clip box?
	     - dkindred@cs.cmu.edu */
	  need_clipping = 0;
	}

      gc = gc_cache_lookup (DEVICE_GTK_GC_CACHE (d), &gcv, pixmap_mask);
    }
  else
    {
      gc = override_gc;
      /* override_gc might have a mask already--we don't want to nuke it.
	 Maybe we can insist that override_gc have no mask, or use
	 one of the suggestions above. */
      need_clipping = 0;
    }

  if (need_clipping)
    {
      GdkRectangle clip_box;

      clip_box.x = clip_x;
      clip_box.y = clip_y;
      clip_box.width = clip_width;
      clip_box.height = clip_height;

      gdk_gc_set_clip_rectangle (gc, &clip_box);
      gdk_gc_set_clip_origin (gc, x, y);
    }

  if (IMAGE_INSTANCE_PIXMAP_DEPTH (p) > 0)
    {
      gdk_draw_pixmap (GDK_DRAWABLE (x_win), gc,
		       IMAGE_INSTANCE_GTK_PIXMAP (p),
		       0, pixmap_offset, x, y, width, height);
    }
  else
    {
      our_draw_bitmap (GDK_DRAWABLE (x_win), gc,
		       IMAGE_INSTANCE_GTK_PIXMAP (p),
		       0, pixmap_offset, x, y, width, height);
    }

  if (need_clipping)
  {
      gdk_gc_set_clip_rectangle (gc, NULL);
      gdk_gc_set_clip_origin (gc, 0, 0);
  }
}

static void
gtk_output_pixmap (struct window *w, struct display_line *dl,
		   Lisp_Object image_instance, int xpos, int xoffset,
		   int start_pixpos, int width, face_index findex,
		   int cursor_start, int cursor_width, int cursor_height)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  struct Lisp_Image_Instance *p = XIMAGE_INSTANCE (image_instance);
  Lisp_Object window;

  GdkWindow *x_win = GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (f));
  int lheight = dl->ascent + dl->descent - dl->clip;
  int pheight = ((int) IMAGE_INSTANCE_PIXMAP_HEIGHT (p) > lheight ? lheight :
		 IMAGE_INSTANCE_PIXMAP_HEIGHT (p));
  int pwidth = min (width + xoffset, (int) IMAGE_INSTANCE_PIXMAP_WIDTH (p));
  int clip_x, clip_y, clip_width, clip_height;

  /* The pixmap_offset is used to center the pixmap on lines which are
     shorter than it is.  This results in odd effects when scrolling
     pixmaps off of the bottom.  Let's try not using it. */
#if 0
  int pixmap_offset = (int) (IMAGE_INSTANCE_PIXMAP_HEIGHT (p) - lheight) / 2;
#else
  int pixmap_offset = 0;
#endif

  XSETWINDOW (window, w);

  if ((start_pixpos >= 0 && start_pixpos > xpos) || xoffset)
    {
      if (start_pixpos > xpos && start_pixpos > xpos + width)
	return;

      clip_x = xoffset;
      clip_width = width;
      if (start_pixpos > xpos)
	{
	  clip_x += (start_pixpos - xpos);
	  clip_width -= (start_pixpos - xpos);
	}
    }
  else
    {
      clip_x = 0;
      clip_width = 0;
    }

  /* Place markers for possible future functionality (clipping the top
     half instead of the bottom half; think pixel scrolling). */
  clip_y = 0;
  clip_height = pheight;

  /* Clear the area the pixmap is going into.  The pixmap itself will
     always take care of the full width.  We don't want to clear where
     it is going to go in order to avoid flicker.  So, all we have to
     take care of is any area above or below the pixmap. */
  /* #### We take a shortcut for now.  We know that since we have
     pixmap_offset hardwired to 0 that the pixmap is against the top
     edge so all we have to worry about is below it. */
  /* #### Unless the pixmap has a mask in which case we have to clear
     the whole damn thing since we can't yet clear just the area not
     included in the mask. */
  if (((int) (dl->ypos - dl->ascent + pheight) <
       (int) (dl->ypos + dl->descent - dl->clip))
      || IMAGE_INSTANCE_GTK_MASK (p))
    {
      int clear_x, clear_y, clear_width, clear_height;

      if (IMAGE_INSTANCE_GTK_MASK (p))
	{
	  clear_y = dl->ypos - dl->ascent;
	  clear_height = lheight;
	}
      else
	{
	  clear_y = dl->ypos - dl->ascent + pheight;
	  clear_height = lheight - pheight;
	}

      if (start_pixpos >= 0 && start_pixpos > xpos)
	{
	  clear_x = start_pixpos;
	  clear_width = xpos + width - start_pixpos;
	}
      else
	{
	  clear_x = xpos;
	  clear_width = width;
	}

      redisplay_clear_region (window, findex, clear_x, clear_y,
			      clear_width, clear_height);
    }

  /* Output the pixmap. */
  {
    Lisp_Object tmp_pixel;
    GdkColor *tmp_bcolor, *tmp_fcolor;

    tmp_pixel = WINDOW_FACE_CACHEL_FOREGROUND (w, findex);
    tmp_fcolor = COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (tmp_pixel));
    tmp_pixel = WINDOW_FACE_CACHEL_BACKGROUND (w, findex);
    tmp_bcolor = COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (tmp_pixel));

    gtk_output_gdk_pixmap (f, p, xpos - xoffset, dl->ypos - dl->ascent, clip_x,
			   clip_y, clip_width, clip_height,
			   pwidth, pheight, pixmap_offset,
			   tmp_fcolor, tmp_bcolor, 0);
  }

  /* Draw a cursor over top of the pixmap. */
  if (cursor_width && cursor_height && (cursor_start >= xpos)
      && !NILP (w->text_cursor_visible_p)
      && (cursor_start < xpos + pwidth))
    {
      GdkGC *gc;
      int focus = EQ (w->frame, DEVICE_FRAME_WITH_FOCUS_REAL (d));
      int y = dl->ypos - dl->ascent;
      struct face_cachel *cursor_cachel =
	WINDOW_FACE_CACHEL (w,
			    get_builtin_face_cache_index
			    (w, Vtext_cursor_face));

      gc = gtk_get_gc (d, Qnil, cursor_cachel->background, Qnil, Qnil, Qnil);

      if (cursor_width > xpos + pwidth - cursor_start)
	cursor_width = xpos + pwidth - cursor_start;

      gdk_draw_rectangle (GDK_DRAWABLE (x_win), gc, focus ? TRUE : FALSE,
			  cursor_start, y, cursor_width,
			  cursor_height);
    }
}

/*****************************************************************************
 gtk_output_vertical_divider

 Draw a vertical divider down the right side of the given window.
 ****************************************************************************/
static void
gtk_output_vertical_divider (struct window *w, int clear)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  GdkWindow *x_win = GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (f));
  GdkGC *background_gc;
  Lisp_Object tmp_pixel;
  GdkGCValues gcv;
  unsigned long mask;
  int x, y1, y2, width, shadow_thickness, spacing, line_width;
  face_index div_face = get_builtin_face_cache_index (w, Vvertical_divider_face);
  
  width = window_divider_width (w);
  shadow_thickness = XINT (w->vertical_divider_shadow_thickness);
  spacing = XINT (w->vertical_divider_spacing);
  line_width = XINT (w->vertical_divider_line_width);
  x = WINDOW_RIGHT (w) - width;
  y1 = WINDOW_TOP (w);
  y2 = WINDOW_BOTTOM (w);
  
  memset (&gcv, ~0, sizeof (gcv));
  
  tmp_pixel = WINDOW_FACE_CACHEL_BACKGROUND (w, div_face);
  
  gcv.background = * COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (tmp_pixel));
  gcv.foreground = gcv.background;
  gcv.graphics_exposures = FALSE;
  mask = GDK_GC_FOREGROUND | GDK_GC_BACKGROUND | GDK_GC_EXPOSURES;

  background_gc = gc_cache_lookup (DEVICE_GTK_GC_CACHE (d), &gcv, mask);

  /* Clear the divider area first.  This needs to be done when a
     window split occurs. */
  /* if (clear) */
  gdk_draw_rectangle (GDK_DRAWABLE (x_win), background_gc, TRUE,
		      x, y1, width, y2 - y1);

#if 0
  /* Draw the divider line. */
  gdk_draw_rectangle (GDK_DRAWABLE (x_win), background_gc, TRUE,
		      x + spacing + shadow_thickness, y1,
		      line_width, y2 - y1);
#endif
  
  /* Draw the shadows around the divider line */
  gtk_output_shadows (f, x + spacing, y1, 
		      width - 2 * spacing, y2 - y1,
		      shadow_thickness);
}

/*****************************************************************************
 gtk_output_blank

 Output a blank by clearing the area it covers in the foreground color
 of its face.
 ****************************************************************************/
static void
gtk_output_blank (struct window *w, struct display_line *dl, struct rune *rb,
		  int start_pixpos, int cursor_start, int cursor_width)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);

  GdkWindow *x_win = GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (f));
  GdkGC *gc;
  struct face_cachel *cursor_cachel =
    WINDOW_FACE_CACHEL (w,
			get_builtin_face_cache_index
			(w, Vtext_cursor_face));
  Lisp_Object bg_pmap;
  Lisp_Object buffer = WINDOW_BUFFER (w);
  Lisp_Object bar_cursor_value = symbol_value_in_buffer (Qbar_cursor,
							 buffer);

  int x = rb->xpos;
  int y = dl->ypos - dl->ascent;
  int width = rb->width;
  int height = dl->ascent + dl->descent - dl->clip;

  if (start_pixpos > x)
    {
      if (start_pixpos >= (x + width))
	return;
      else
	{
	  width -= (start_pixpos - x);
	  x = start_pixpos;
	}
    }

  bg_pmap = WINDOW_FACE_CACHEL_BACKGROUND_PIXMAP (w, rb->findex);
  if (!IMAGE_INSTANCEP (bg_pmap)
      || !IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (bg_pmap)))
    bg_pmap = Qnil;

  if (NILP (bg_pmap))
    gc = gtk_get_gc (d, Qnil, WINDOW_FACE_CACHEL_BACKGROUND (w, rb->findex),
		     Qnil, Qnil, Qnil);
  else
    gc = gtk_get_gc (d, Qnil, WINDOW_FACE_CACHEL_FOREGROUND (w, rb->findex),
		     WINDOW_FACE_CACHEL_BACKGROUND (w, rb->findex), bg_pmap,
		     Qnil);

  gdk_draw_rectangle (GDK_DRAWABLE (x_win), gc, TRUE, x, y, width, height);

  /* If this rune is marked as having the cursor, then it is actually
     representing a tab. */
  if (!NILP (w->text_cursor_visible_p)
      && (rb->cursor_type == CURSOR_ON
	  || (cursor_width
	      && (cursor_start + cursor_width > x)
	      && cursor_start < (x + width))))
    {
      int cursor_height, cursor_y;
      int focus = EQ (w->frame, DEVICE_FRAME_WITH_FOCUS_REAL (d));
      struct Lisp_Font_Instance *fi;

      fi = XFONT_INSTANCE (FACE_CACHEL_FONT
			   (WINDOW_FACE_CACHEL (w, rb->findex),
			    Vcharset_ascii));

      gc = gtk_get_gc (d, Qnil, cursor_cachel->background, Qnil, Qnil, Qnil);

      cursor_y = dl->ypos - fi->ascent;
      cursor_height = fi->height;
      if (cursor_y + cursor_height > y + height)
	cursor_height = y + height - cursor_y;

      if (focus)
	{
	  if (NILP (bar_cursor_value))
	    {
		gdk_draw_rectangle (GDK_DRAWABLE (x_win), gc, TRUE,
				    cursor_start, cursor_y,
				    fi->width, cursor_height);
	    }
	  else
	    {
	      int bar_width = EQ (bar_cursor_value, Qt) ? 1 : 2;

	      gc = gtk_get_gc (d, Qnil, cursor_cachel->background, Qnil, Qnil,
			       make_int (bar_width));
	      gdk_draw_line (GDK_DRAWABLE (x_win), gc, cursor_start + bar_width - 1,
			     cursor_y, cursor_start + bar_width - 1,
			     cursor_y + cursor_height - 1);
	    }
	}
      else if (NILP (bar_cursor_value))
	{
	    gdk_draw_rectangle (GDK_DRAWABLE (x_win), gc, FALSE,
				cursor_start, cursor_y,
				fi->width - 1, cursor_height - 1);
	}
    }
}

/*****************************************************************************
 gtk_output_hline

 Output a horizontal line in the foreground of its face.
 ****************************************************************************/
static void
gtk_output_hline (struct window *w, struct display_line *dl, struct rune *rb)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  GtkStyle *style = FRAME_GTK_TEXT_WIDGET (f)->style;

  GdkWindow *x_win = GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (f));
  GdkGC *gc;

  int x = rb->xpos;
  int width = rb->width;
  int height = dl->ascent + dl->descent - dl->clip;

  int ypos1, ypos2, ypos3, ypos4;

  ypos1 = dl->ypos - dl->ascent;
  ypos2 = ypos1 + rb->object.hline.yoffset;
  ypos3 = ypos2 + rb->object.hline.thickness;
  ypos4 = dl->ypos + dl->descent - dl->clip;

  /* First clear the area not covered by the line. */
  if (height - rb->object.hline.thickness > 0)
    {
      gc = gtk_get_gc (d, Qnil, WINDOW_FACE_CACHEL_FOREGROUND (w, rb->findex),
		     Qnil, Qnil, Qnil);

      if (ypos2 - ypos1 > 0)
	  gdk_draw_rectangle (GDK_DRAWABLE (x_win), gc, TRUE, x, ypos1, width, ypos2 - ypos1);
      if (ypos4 - ypos3 > 0)
	  gdk_draw_rectangle (GDK_DRAWABLE (x_win), gc, TRUE, x, ypos1, width, ypos2 - ypos1);
    }

  gtk_paint_hline (style, x_win, GTK_STATE_NORMAL, NULL, FRAME_GTK_TEXT_WIDGET (f),
		   "hline", x, x + width, ypos2);
#if 0
  /* Now draw the line. */
  gc = gtk_get_gc (d, Qnil, WINDOW_FACE_CACHEL_BACKGROUND (w, rb->findex),
		   Qnil, Qnil, Qnil);

  if (ypos2 < ypos1)
    ypos2 = ypos1;
  if (ypos3 > ypos4)
    ypos3 = ypos4;

  if (ypos3 - ypos2 > 0)
      gdk_draw_rectangle (GDK_DRAWABLE (x_win), gc, TRUE, x, ypos2, width, ypos3 - ypos2);
#endif
}

/*****************************************************************************
 gtk_output_shadows

 Draw a shadow around the given area using the standard theme engine routines.
 ****************************************************************************/
void
gtk_output_shadows (struct frame *f, int x, int y, int width, int height,
		    int shadow_thickness)
{
  GdkWindow *x_win = GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (f));
  GtkStyle *style = FRAME_GTK_TEXT_WIDGET (f)->style;
  GtkShadowType stype = GTK_SHADOW_OUT;

  if (shadow_thickness < 0)
  {
      stype = GTK_SHADOW_IN;
  }
  else if (shadow_thickness == 0)
  {
      stype = GTK_SHADOW_NONE;
  }

  /* Do we want to have some magic constants to set
     GTK_SHADOW_ETCHED_IN or GTK_SHADOW_ETCHED_OUT? */

  gtk_paint_shadow (style, x_win, GTK_STATE_NORMAL, stype, NULL,
		    FRAME_GTK_TEXT_WIDGET (f), "modeline",
		    x, y, width, height);
}

/*****************************************************************************
 gtk_clear_to_window_end

 Clear the area between ypos1 and ypos2.  Each margin area and the
 text area is handled separately since they may each have their own
 background color.
 ****************************************************************************/
static void
gtk_clear_to_window_end (struct window *w, int ypos1, int ypos2)
{
  int height = ypos2 - ypos1;

  if (height)
    {
      struct frame *f = XFRAME (w->frame);
      Lisp_Object window;
      int bflag = (window_needs_vertical_divider (w) ? 0 : 1);
      layout_bounds bounds;

      bounds = calculate_display_line_boundaries (w, bflag);
      XSETWINDOW (window, w);

      if (window_is_leftmost (w))
	redisplay_clear_region (window, DEFAULT_INDEX, FRAME_LEFT_BORDER_START (f),
				ypos1, FRAME_BORDER_WIDTH (f), height);

      if (bounds.left_in - bounds.left_out > 0)
	redisplay_clear_region (window,
				get_builtin_face_cache_index (w, Vleft_margin_face),
				bounds.left_out, ypos1,
				bounds.left_in - bounds.left_out, height);

      if (bounds.right_in - bounds.left_in > 0)
	redisplay_clear_region (window, DEFAULT_INDEX, bounds.left_in, ypos1,
				bounds.right_in - bounds.left_in, height);

      if (bounds.right_out - bounds.right_in > 0)
	redisplay_clear_region (window,
				get_builtin_face_cache_index (w, Vright_margin_face),
				bounds.right_in, ypos1,
				bounds.right_out - bounds.right_in, height);

      if (window_is_rightmost (w))
	redisplay_clear_region (window, DEFAULT_INDEX, FRAME_RIGHT_BORDER_START (f),
				ypos1, FRAME_BORDER_WIDTH (f), height);
    }
}

/*****************************************************************************
 gtk_redraw_exposed_window

 Given a bounding box for an area that needs to be redrawn, determine
 what parts of what lines are contained within and re-output their
 contents.
 ****************************************************************************/
static void
gtk_redraw_exposed_window (struct window *w, int x, int y, int width, int height)
{
  struct frame *f = XFRAME (w->frame);
  int line;
  int start_x, start_y, end_x, end_y;
  int orig_windows_structure_changed;

  display_line_dynarr *cdla = window_display_lines (w, CURRENT_DISP);

  if (!NILP (w->vchild))
    {
      gtk_redraw_exposed_windows (w->vchild, x, y, width, height);
      return;
    }
  else if (!NILP (w->hchild))
    {
      gtk_redraw_exposed_windows (w->hchild, x, y, width, height);
      return;
    }

  /* If the window doesn't intersect the exposed region, we're done here. */
  if (x >= WINDOW_RIGHT (w) || (x + width) <= WINDOW_LEFT (w)
      || y >= WINDOW_BOTTOM (w) || (y + height) <= WINDOW_TOP (w))
    {
      return;
    }
  else
    {
      start_x = max (WINDOW_LEFT (w), x);
      end_x = min (WINDOW_RIGHT (w), (x + width));
      start_y = max (WINDOW_TOP (w), y);
      end_y = min (WINDOW_BOTTOM (w), y + height);

      /* We do this to make sure that the 3D modelines get redrawn if
         they are in the exposed region. */
      orig_windows_structure_changed = f->windows_structure_changed;
      f->windows_structure_changed = 1;
    }

  if (window_needs_vertical_divider (w))
    {
      gtk_output_vertical_divider (w, 0);
    }

  for (line = 0; line < Dynarr_length (cdla); line++)
    {
      struct display_line *cdl = Dynarr_atp (cdla, line);
      int top_y = cdl->ypos - cdl->ascent;
      int bottom_y = cdl->ypos + cdl->descent;

      if (bottom_y >= start_y)
	{
	  if (top_y > end_y)
	    {
	      if (line == 0)
		continue;
	      else
		break;
	    }
	  else
	    {
	      output_display_line (w, 0, cdla, line, start_x, end_x);
	    }
	}
    }

  f->windows_structure_changed = orig_windows_structure_changed;

  /* If there have never been any face cache_elements created, then this
     expose event doesn't actually have anything to do. */
  if (Dynarr_largest (w->face_cachels))
    redisplay_clear_bottom_of_window (w, cdla, start_y, end_y);
}

/*****************************************************************************
 gtk_redraw_exposed_windows

 For each window beneath the given window in the window hierarchy,
 ensure that it is redrawn if necessary after an Expose event.
 ****************************************************************************/
static void
gtk_redraw_exposed_windows (Lisp_Object window, int x, int y, int width,
			    int height)
{
  for (; !NILP (window); window = XWINDOW (window)->next)
    gtk_redraw_exposed_window (XWINDOW (window), x, y, width, height);
}

/*****************************************************************************
 gtk_redraw_exposed_area

 For each window on the given frame, ensure that any area in the
 Exposed area is redrawn.
 ****************************************************************************/
void
gtk_redraw_exposed_area (struct frame *f, int x, int y, int width, int height)
{
  /* If any window on the frame has had its face cache reset then the
     redisplay structures are effectively invalid.  If we attempt to
     use them we'll blow up.  We mark the frame as changed to ensure
     that redisplay will do a full update.  This probably isn't
     necessary but it can't hurt. */

#ifdef HAVE_TOOLBARS
  /* #### We would rather put these off as well but there is currently
     no combination of flags which will force an unchanged toolbar to
     redraw anyhow. */
  MAYBE_FRAMEMETH (f, redraw_exposed_toolbars, (f, x, y, width, height));
#endif
  redraw_exposed_gutters (f, x, y, width, height);

  if (!f->window_face_cache_reset)
    {
      gtk_redraw_exposed_windows (f->root_window, x, y, width, height);
    }
  else
    MARK_FRAME_CHANGED (f);
}

/****************************************************************************
 gtk_clear_region

 Clear the area in the box defined by the given parameters using the
 given face.
 ****************************************************************************/
static void
gtk_clear_region (Lisp_Object locale, struct device* d, struct frame* f, face_index findex,
		  int x, int y,
		  int width, int height, Lisp_Object fcolor, Lisp_Object bcolor,
		  Lisp_Object background_pixmap)
{
  GdkWindow *x_win;
  GdkGC *gc = NULL;

  x_win = GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (f));

  if (!UNBOUNDP (background_pixmap))
    {
      gc = gtk_get_gc (d, Qnil, fcolor, bcolor, background_pixmap, Qnil);
    }

  if (gc)
    {
      gdk_draw_rectangle (GDK_DRAWABLE (x_win), gc,TRUE,
			  x, y, width, height);
    }
  else
    {
      gdk_window_clear_area (x_win, x, y, width, height);
    }
}

/*****************************************************************************
 gtk_output_eol_cursor

 Draw a cursor at the end of a line.  The end-of-line cursor is
 narrower than the normal cursor.
 ****************************************************************************/
static void
gtk_output_eol_cursor (struct window *w, struct display_line *dl, int xpos,
		       face_index findex)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  Lisp_Object window;

  GdkWindow *x_win = GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (f));
  GdkGC *gc;
  face_index elt = get_builtin_face_cache_index (w, Vtext_cursor_face);
  struct face_cachel *cursor_cachel = WINDOW_FACE_CACHEL (w, elt);

  int focus = EQ (w->frame, DEVICE_FRAME_WITH_FOCUS_REAL (d));
  Lisp_Object bar_cursor_value = symbol_value_in_buffer (Qbar_cursor,
							 WINDOW_BUFFER (w));

  int x = xpos;
  int y = dl->ypos - dl->ascent;
  int width = EOL_CURSOR_WIDTH;
  int height = dl->ascent + dl->descent - dl->clip;
  int cursor_height, cursor_y;
  int defheight, defascent;

  XSETWINDOW (window, w);
  redisplay_clear_region (window, findex, x, y, width, height);

  if (NILP (w->text_cursor_visible_p))
    return;

  gc = gtk_get_gc (d, Qnil, cursor_cachel->background, Qnil, Qnil, Qnil);

  default_face_font_info (window, &defascent, 0, &defheight, 0, 0);

  /* make sure the cursor is entirely contained between y and y+height */
  cursor_height = min (defheight, height);
  cursor_y = max (y, min (y + height - cursor_height,
			  dl->ypos - defascent));

  if (focus)
    {
      if (NILP (bar_cursor_value))
	{
	    gdk_draw_rectangle (GDK_DRAWABLE (x_win), gc, TRUE, x, cursor_y, width, cursor_height);
	}
      else
	{
	  int bar_width = EQ (bar_cursor_value, Qt) ? 1 : 2;

	  gc = gtk_get_gc (d, Qnil, cursor_cachel->background, Qnil, Qnil,
			   make_int (bar_width));
	  gdk_draw_line (GDK_DRAWABLE (x_win), gc, x + bar_width - 1, cursor_y,
			 x + bar_width - 1, cursor_y + cursor_height - 1);
	}
    }
  else if (NILP (bar_cursor_value))
    {
	gdk_draw_rectangle (GDK_DRAWABLE (x_win), gc, FALSE, x, cursor_y, width - 1,
			    cursor_height - 1);
    }
}

static void
gtk_clear_frame_window (Lisp_Object window)
{
  struct window *w = XWINDOW (window);

  if (!NILP (w->vchild))
    {
      gtk_clear_frame_windows (w->vchild);
      return;
    }

  if (!NILP (w->hchild))
    {
      gtk_clear_frame_windows (w->hchild);
      return;
    }

  gtk_clear_to_window_end (w, WINDOW_TEXT_TOP (w), WINDOW_TEXT_BOTTOM (w));
}

static void
gtk_clear_frame_windows (Lisp_Object window)
{
  for (; !NILP (window); window = XWINDOW (window)->next)
    gtk_clear_frame_window (window);
}

static void
gtk_clear_frame (struct frame *f)
{
  GdkWindow *x_win = GET_GTK_WIDGET_WINDOW (FRAME_GTK_TEXT_WIDGET (f));
  int x, y, width, height;
  Lisp_Object frame;

  x = FRAME_LEFT_BORDER_START (f);
  width = (FRAME_PIXWIDTH (f) - FRAME_REAL_LEFT_TOOLBAR_WIDTH (f) -
	   FRAME_REAL_RIGHT_TOOLBAR_WIDTH (f) -
	   2 * FRAME_REAL_LEFT_TOOLBAR_BORDER_WIDTH (f) -
	   2 * FRAME_REAL_RIGHT_TOOLBAR_BORDER_WIDTH (f));
  /* #### This adjustment by 1 should be being done in the macros.
     There is some small differences between when the menubar is on
     and off that we still need to deal with. */
  y = FRAME_TOP_BORDER_START (f) - 1;
  height = (FRAME_PIXHEIGHT (f) - FRAME_REAL_TOP_TOOLBAR_HEIGHT (f) -
	    FRAME_REAL_BOTTOM_TOOLBAR_HEIGHT (f) -
	    2 * FRAME_REAL_TOP_TOOLBAR_BORDER_WIDTH (f) -
	    2 * FRAME_REAL_BOTTOM_TOOLBAR_BORDER_WIDTH (f)) + 1;

  gdk_window_clear_area (x_win, x, y, width, height);

  XSETFRAME (frame, f);

  if (!UNBOUNDP (FACE_BACKGROUND_PIXMAP (Vdefault_face, frame))
      || !UNBOUNDP (FACE_BACKGROUND_PIXMAP (Vleft_margin_face, frame))
      || !UNBOUNDP (FACE_BACKGROUND_PIXMAP (Vright_margin_face, frame)))
    {
      gtk_clear_frame_windows (f->root_window);
    }
}

static int
gtk_flash (struct device *d)
{
  GdkGCValues gcv;
  GdkGC *gc;
  GdkColor tmp_fcolor, tmp_bcolor;
  Lisp_Object tmp_pixel, frame;
  struct frame *f = device_selected_frame (d);
  struct window *w = XWINDOW (FRAME_ROOT_WINDOW (f));

  XSETFRAME (frame, f);

  tmp_pixel = FACE_FOREGROUND (Vdefault_face, frame);
  tmp_fcolor = * (COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (tmp_pixel)));
  tmp_pixel = FACE_BACKGROUND (Vdefault_face, frame);
  tmp_bcolor = * (COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (tmp_pixel)));

  memset (&gcv, ~0, sizeof (gcv)); /* initialize all slots to ~0 */
  gcv.foreground.pixel  = (tmp_fcolor.pixel ^ tmp_bcolor.pixel);
  gcv.function = GDK_XOR;
  gcv.graphics_exposures = FALSE;
  gc = gc_cache_lookup (DEVICE_GTK_GC_CACHE (XDEVICE (f->device)), &gcv,
			GDK_GC_FOREGROUND | GDK_GC_FUNCTION | GDK_GC_EXPOSURES);

  gdk_draw_rectangle (GDK_DRAWABLE (GET_GTK_WIDGET_WINDOW (FRAME_GTK_SHELL_WIDGET (f))),
		      gc, TRUE, w->pixel_left, w->pixel_top,
		      w->pixel_width, w->pixel_height);

  gdk_flush ();

#ifdef HAVE_POLL
  poll (0, 0, 100);
#else /* !HAVE_POLL */
#ifdef HAVE_SELECT
  {
    int usecs = 100000;
    struct timeval tv;
    tv.tv_sec  = usecs / 1000000L;
    tv.tv_usec = usecs % 1000000L;
    /* I'm sure someone is going to complain about this... */
    select (0, 0, 0, 0, &tv);
  }
#else
  bite me
#endif /* HAVE_POLL */
#endif /* HAVE_SELECT */

  gdk_draw_rectangle (GDK_DRAWABLE (GET_GTK_WIDGET_WINDOW (FRAME_GTK_SHELL_WIDGET (f))),
		      gc, TRUE, w->pixel_left, w->pixel_top,
		      w->pixel_width, w->pixel_height);

  gdk_flush ();

  return 1;
}

static void
gtk_bevel_area (struct window *w, face_index findex,
		int x, int y, int width, int height,
		int shadow_thickness, int edges, enum edge_style style)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);

  gtk_output_shadows (f, x, y, width, height, shadow_thickness);
}



/* Make audible bell.  */
static void
gtk_ring_bell (struct device *d, int volume, int pitch, int duration)
{
	/* Gdk does not allow us to control the duration / pitch / volume */
	gdk_beep ();
}


/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
console_type_create_redisplay_gtk (void)
{
  /* redisplay methods */
  CONSOLE_HAS_METHOD (gtk, text_width);
  CONSOLE_HAS_METHOD (gtk, output_display_block);
  CONSOLE_HAS_METHOD (gtk, divider_height);
  CONSOLE_HAS_METHOD (gtk, eol_cursor_width);
  CONSOLE_HAS_METHOD (gtk, output_vertical_divider);
  CONSOLE_HAS_METHOD (gtk, clear_to_window_end);
  CONSOLE_HAS_METHOD (gtk, clear_region);
  CONSOLE_HAS_METHOD (gtk, clear_frame);
  CONSOLE_HAS_METHOD (gtk, flash);
  CONSOLE_HAS_METHOD (gtk, ring_bell);
  CONSOLE_HAS_METHOD (gtk, bevel_area);
  CONSOLE_HAS_METHOD (gtk, output_string);
  /*  CONSOLE_HAS_METHOD (gtk, output_pixmap); */
}

/* This makes me feel incredibly dirty... but there is no other way to
   get this done right other than calling clear_area before every
   single $#!%@ing piece of text, which I do NOT want to do. */
#define USE_X_SPECIFIC_DRAW_ROUTINES 1

#include <gdk/gdkx.h>

void
gdk_draw_text_image (GdkDrawable *drawable,
		     GdkFont     *font,
		     GdkGC       *gc,
		     gint         x,
		     gint         y,
		     const gchar *text,
		     gint         text_length)
{
#if !USE_X_SPECIFIC_DRAW_ROUTINES
  int width = gdk_text_measure (font, text, text_length);
  int height = gdk_text_height (font, text, text_length);

  gdk_draw_rectangle (drawable, gc, TRUE, x, y, width, height);
  gdk_draw_text (drawable, font, gc, x, y, text, text_length);
#else
  GdkWindowPrivate *drawable_private;
  GdkFontPrivate *font_private;
  GdkGCPrivate *gc_private;

  g_return_if_fail (drawable != NULL);
  g_return_if_fail (font != NULL);
  g_return_if_fail (gc != NULL);
  g_return_if_fail (text != NULL);

  drawable_private = (GdkWindowPrivate*) drawable;
  if (drawable_private->destroyed)
    return;
  gc_private = (GdkGCPrivate*) gc;
  font_private = (GdkFontPrivate*) font;

  if (font->type == GDK_FONT_FONT)
    {
      XFontStruct *xfont = (XFontStruct *) font_private->xfont;
      XSetFont(drawable_private->xdisplay, gc_private->xgc, xfont->fid);
      if ((xfont->min_byte1 == 0) && (xfont->max_byte1 == 0))
	{
	  XDrawImageString (drawable_private->xdisplay, drawable_private->xwindow,
			    gc_private->xgc, x, y, text, text_length);
	}
      else
	{
	  XDrawImageString16 (drawable_private->xdisplay, drawable_private->xwindow,
			      gc_private->xgc, x, y, (XChar2b *) text, text_length / 2);
	}
    }
  else if (font->type == GDK_FONT_FONTSET)
    {
      XFontSet fontset = (XFontSet) font_private->xfont;
      XmbDrawImageString (drawable_private->xdisplay, drawable_private->xwindow,
			  fontset, gc_private->xgc, x, y, text, text_length);
    }
  else
    g_error("undefined font type\n");
#endif
}

static void
our_draw_bitmap (GdkDrawable *drawable,
		 GdkGC       *gc,
		 GdkPixmap   *src,
		 gint         xsrc,
		 gint         ysrc,
		 gint         xdest,
		 gint         ydest,
		 gint         width,
		 gint         height)
{
  GdkWindowPrivate *drawable_private;
  GdkWindowPrivate *src_private;
  GdkGCPrivate *gc_private;

  g_return_if_fail (drawable != NULL);
  g_return_if_fail (src != NULL);
  g_return_if_fail (gc != NULL);

  drawable_private = (GdkWindowPrivate*) drawable;
  src_private = (GdkWindowPrivate*) src;
  if (drawable_private->destroyed || src_private->destroyed)
    return;
  gc_private = (GdkGCPrivate*) gc;

  if (width == -1)
    width = src_private->width;
  if (height == -1)
    height = src_private->height;

  XCopyPlane (drawable_private->xdisplay,
	     src_private->xwindow,
	     drawable_private->xwindow,
	     gc_private->xgc,
	     xsrc, ysrc,
	     width, height,
	     xdest, ydest, 1L);
}
