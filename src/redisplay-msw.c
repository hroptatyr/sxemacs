/* mswindows output and frame manipulation routines.
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

/* Authorship:

   Chuck Thompson
   Lots of work done by Ben Wing for Mule

   Partially rewritten for mswindows by Jonathan Harris, November 1997
   for 21.0.  */

#include <config.h>
#include "lisp.h"

#include "console-msw.h"
#include "objects-msw.h"

#include "buffer.h"
#include "debug.h"
#include "events.h"
#include "faces.h"
#include "frame.h"
#include "glyphs-msw.h"
#include "gutter.h"
#include "redisplay.h"
#include "sysdep.h"
#include "window.h"

#ifdef MULE
#include "mule-ccl.h"
#include "mule-charset.h"
#endif

#define MSWINDOWS_EOL_CURSOR_WIDTH	5

/*
 * Random forward declarations
 */
static void mswindows_update_dc (HDC hdc, Lisp_Object fg, Lisp_Object bg,
				 Lisp_Object bg_pmap);
static void mswindows_set_dc_font (HDC hdc, Lisp_Object font,
				   int under, int strike);
static void mswindows_output_vertical_divider (struct window *w, int clear);
static void mswindows_redraw_exposed_windows (Lisp_Object window, int x,
					int y, int width, int height);
static void mswindows_output_dibitmap (struct frame *f, 
				       Lisp_Image_Instance *p,
				       struct display_box* db,
				       struct display_glyph_area* dga);

typedef struct textual_run
{
  Lisp_Object charset;
  unsigned char *ptr;
  int len;
  int dimension;
} textual_run;

/* Separate out the text in DYN into a series of textual runs of a
   particular charset.  Also convert the characters as necessary into
   the format needed by XDrawImageString(), XDrawImageString16(), et
   al.  (This means converting to one or two byte format, possibly
   tweaking the high bits, and possibly running a CCL program.) You
   must pre-allocate the space used and pass it in. (This is done so
   you can alloca() the space.)  You need to allocate (2 * len) bytes
   of TEXT_STORAGE and (len * sizeof (textual_run)) bytes of
   RUN_STORAGE, where LEN is the length of the dynarr.

   Returns the number of runs actually used. */

static int
separate_textual_runs (unsigned char *text_storage,
		       textual_run *run_storage,
		       const Emchar *str, Charcount len)
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
	    if ((!NILP (ccl_prog))
		  && (setup_ccl_program (&char_converter, ccl_prog) >= 0))
	      need_ccl_conversion = 1;
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
	  char_converter.ic = 0; /* start at beginning each time */
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


static int
mswindows_text_width_single_run (HDC hdc, struct face_cachel *cachel,
				 textual_run *run)
{
  Lisp_Object font_inst = FACE_CACHEL_FONT (cachel, run->charset);
  Lisp_Font_Instance *fi = XFONT_INSTANCE (font_inst);
  SIZE size;

  if (!fi->proportional_p || !hdc)
    return (fi->width * run->len);
  else
    {
      assert(run->dimension == 1);	/* #### FIXME! */
      mswindows_set_dc_font (hdc, font_inst,
			     cachel->underline, cachel->strikethru);
      GetTextExtentPoint32 (hdc, run->ptr, run->len, &size);
      return(size.cx);
    }
}

/*
 * Given F, retrieve device context. F can be a display frame, or
 * a print job. For a print job, page is also started when printer's
 * device context is first time requested. 
 */
static HDC
get_frame_dc (struct frame *f, int start_page_p)
{
  if (FRAME_MSWINDOWS_P (f))
    return FRAME_MSWINDOWS_DC (f);
  else
    {
      if (start_page_p && !FRAME_MSPRINTER_PAGE_STARTED (f))
	msprinter_start_page (f);
      return DEVICE_MSPRINTER_HDC (XDEVICE (FRAME_DEVICE (f)));
    }
}

/*
 * Given F, retrieve compatible device context. F can be a display
 * frame, or a print job.
 */
static HDC
get_frame_compdc (struct frame *f)
{
  struct device *d = XDEVICE (FRAME_DEVICE (f));
  if (DEVICE_MSWINDOWS_P (d))
    return DEVICE_MSWINDOWS_HCDC (d);
  else
    return DEVICE_MSPRINTER_HCDC (d);
}

/*****************************************************************************
 mswindows_update_dc

 Given a number of parameters munge the DC so it has those properties.
 ****************************************************************************/
static void
mswindows_update_dc (HDC hdc, Lisp_Object fg, Lisp_Object bg,
		     Lisp_Object bg_pmap)
{
  if (!NILP (fg))
    {
      SetTextColor (hdc, COLOR_INSTANCE_MSWINDOWS_COLOR 
		    (XCOLOR_INSTANCE (fg)));
    }

  if (!NILP (bg))
    { 
      SetBkMode (hdc, OPAQUE);
      SetBkColor (hdc, COLOR_INSTANCE_MSWINDOWS_COLOR (XCOLOR_INSTANCE (bg)));
    }
  else 
    {
      SetBkMode (hdc, TRANSPARENT);
    }
}

static void mswindows_set_dc_font (HDC hdc, Lisp_Object font,
				   int under, int strike)
{
  SelectObject(hdc, mswindows_get_hfont (XFONT_INSTANCE (font),
					 under, strike));
}

/*****************************************************************************
 mswindows_output_hline

 Output a horizontal line in the foreground of its face.
 ****************************************************************************/
static void
mswindows_output_hline (struct window *w, struct display_line *dl, struct rune *rb)
{ /* XXX Implement me */
}


/*****************************************************************************
 mswindows_output_blank

 Output a blank by clearing the area it covers in the background color
 of its face.
 ****************************************************************************/
static void
mswindows_output_blank (struct window *w, struct display_line *dl, 
			struct rune *rb, int start_pixpos)
{
  struct frame *f = XFRAME (w->frame);
  HDC hdc = get_frame_dc (f, 1);
  RECT rect = { rb->xpos, DISPLAY_LINE_YPOS (dl),
		rb->xpos+rb->width, 
		DISPLAY_LINE_YEND (dl) };
  struct face_cachel *cachel = WINDOW_FACE_CACHEL (w, rb->findex);

  Lisp_Object bg_pmap = WINDOW_FACE_CACHEL_BACKGROUND_PIXMAP (w, rb->findex);

  /* Unmap all subwindows in the area we are going to blank. */
  redisplay_unmap_subwindows_maybe (f, rb->xpos, DISPLAY_LINE_YPOS (dl),
				    rb->width, DISPLAY_LINE_HEIGHT (dl));

  if (!IMAGE_INSTANCEP (bg_pmap)
      || !IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (bg_pmap)))
    bg_pmap = Qnil;

  if (!NILP(bg_pmap))
    {
      struct display_box db;
      struct display_glyph_area dga;
      redisplay_calculate_display_boxes (dl, rb->xpos, 
					 /*rb->object.dglyph.xoffset*/ 0,
                                         /*rb->object.dglyph.yoffset*/ 0,
					 start_pixpos, rb->width,
					 &db, &dga);
      /* blank the background in the appropriate color */
      mswindows_update_dc (hdc, cachel->foreground,
			   cachel->background, Qnil);
      redisplay_output_pixmap (w, bg_pmap, &db, &dga, rb->findex,
			       0, 0, 0, TRUE);
    }
  else 
    {
      mswindows_update_dc (hdc, Qnil, cachel->background, Qnil);
      ExtTextOut (hdc, 0, 0, ETO_OPAQUE, &rect, NULL, 0, NULL);
    }
}


/*****************************************************************************
 mswindows_output_cursor

 Draw a normal or end-of-line cursor. The end-of-line cursor is
 narrower than the normal cursor.
 ****************************************************************************/
static void
mswindows_output_cursor (struct window *w, struct display_line *dl, int xpos,
			 int width, face_index findex, Emchar ch, int image_p)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  struct face_cachel *cachel=0;
  Lisp_Object font = Qnil;
  int focus = EQ (w->frame, DEVICE_FRAME_WITH_FOCUS_REAL (d));
  HDC hdc = get_frame_dc (f, 1);
  unsigned int local_face_index=0;
  char *p_char = NULL;
  int n_char = 0;
  RECT rect = { xpos,
		DISPLAY_LINE_YPOS (dl),
		xpos + width,
		DISPLAY_LINE_YEND (dl) };
  Lisp_Object bar = symbol_value_in_buffer (Qbar_cursor,
					    WINDOW_BUFFER (w));
  int bar_p = image_p || !NILP (bar);
  int cursor_p = !NILP (w->text_cursor_visible_p);
  int real_char_p = ch != 0;

  /* Unmap all subwindows in the area we are going to blank. */
  redisplay_unmap_subwindows_maybe (f, xpos, DISPLAY_LINE_YPOS (dl),
				    width, DISPLAY_LINE_HEIGHT (dl));

  if (real_char_p)
    {
      /* Use the font from the underlying character */
      cachel = WINDOW_FACE_CACHEL (w, findex);

      /* #### MULE: Need to know the charset! */
      font = FACE_CACHEL_FONT (cachel, Vcharset_ascii);
    }

  if ((focus || bar_p) && real_char_p)
    {
      p_char = (char*) &ch;
      n_char = 1;
    }

  if (!image_p)
    {
      struct face_cachel *color_cachel;

      /* Use cursor fg/bg for block cursor, or character fg/bg for the bar
	 or when we need to erase the cursor. Output nothing at eol if bar
	 cursor */
      local_face_index = get_builtin_face_cache_index (w, Vtext_cursor_face);
      color_cachel = WINDOW_FACE_CACHEL (w, ((!cursor_p || bar_p) ?
					     findex : local_face_index));
      mswindows_update_dc (hdc, color_cachel->foreground,
			   color_cachel->background, Qnil);
      if (real_char_p)
        mswindows_set_dc_font (hdc, font,
			       cachel->underline, cachel->strikethru);

      ExtTextOut (hdc, xpos, dl->ypos, ETO_OPAQUE|ETO_CLIPPED, &rect, p_char, n_char, NULL);
    }

  if (!cursor_p)
    return;

  if (focus && bar_p)
    {
      rect.right = rect.left + (EQ (bar, Qt) ? 1 : min (2, width));
      local_face_index = get_builtin_face_cache_index (w, Vtext_cursor_face);
      cachel = WINDOW_FACE_CACHEL (w, local_face_index);
      mswindows_update_dc (hdc, Qnil, cachel->background, Qnil);
      ExtTextOut (hdc, xpos, dl->ypos, ETO_OPAQUE, &rect, NULL, 0, NULL);
    }
  else if (!focus)
    {
      /* Now have real character drawn in its own color. We deflate
	 the rectangle so character cell will be bounded by the
	 previously drawn cursor shape */
      InflateRect (&rect, -1, -1);

      if (real_char_p)
	{
	  p_char = (char*) &ch;
	  n_char = 1;
	}

      local_face_index = get_builtin_face_cache_index (w, Vdefault_face);
      cachel = WINDOW_FACE_CACHEL (w, (real_char_p ? findex : local_face_index));
      mswindows_update_dc (hdc, 
			   cachel->foreground, cachel->background, Qnil);
      ExtTextOut (hdc, xpos, dl->ypos, ETO_OPAQUE | ETO_CLIPPED,
		  &rect, p_char, n_char, NULL);
    }
}


/*****************************************************************************
 mswindows_output_string

 Given a string and a starting position, output that string in the
 given face.
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
 ****************************************************************************/
static void
mswindows_output_string (struct window *w, struct display_line *dl,
			 Emchar_dynarr *buf, int xpos, int xoffset, int clip_start,
			 int width, face_index findex,
			 int cursor, int cursor_start, int cursor_width,
			 int cursor_height)
{
  struct frame *f = XFRAME (w->frame);
  /* struct device *d = XDEVICE (f->device);*/
  Lisp_Object window;
  HDC hdc = get_frame_dc (f, 1);
  int clip_end;
  Lisp_Object bg_pmap;
  int len = Dynarr_length (buf);
  unsigned char *text_storage = (unsigned char *) alloca (2 * len);
  textual_run *runs = alloca_array (textual_run, len);
  int nruns;
  int i, height;
  RECT rect;
  struct face_cachel *cachel = WINDOW_FACE_CACHEL (w, findex);

  XSETWINDOW (window, w);

#if 0	/* #### FIXME? */
  /* We can't work out the width before we've set the font in the DC */
  if (width < 0)
    width = mswindows_text_width (cachel, Dynarr_atp (buf, 0), Dynarr_length (buf));
#else
  assert(width>=0);
#endif

  /* Regularize the variables passed in. */
  if (clip_start < xpos)
    clip_start = xpos;
  clip_end = xpos + width;
  if (clip_start >= clip_end)
    /* It's all clipped out. */
    return;

  xpos -= xoffset;

  /* sort out the destination rectangle */
  height = DISPLAY_LINE_HEIGHT (dl);
  rect.left = clip_start;
  rect.top  = DISPLAY_LINE_YPOS (dl);
  rect.right = clip_end;
  rect.bottom = rect.top + height;

  /* make sure the area we are about to display is subwindow free. */
  redisplay_unmap_subwindows_maybe (f, clip_start, DISPLAY_LINE_YPOS (dl),
				    clip_end - clip_start, DISPLAY_LINE_HEIGHT (dl));

  /* output the background pixmap if there is one */
  bg_pmap = cachel->background_pixmap;
  if (!IMAGE_INSTANCEP (bg_pmap)
      || !IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (bg_pmap)))
    bg_pmap = Qnil;

  if (!NILP(bg_pmap))
    {
      struct display_box db;
      struct display_glyph_area dga;
      redisplay_calculate_display_boxes (dl, xpos + xoffset, 0, 0,
					 clip_start, width, &db, &dga);
      /* blank the background in the appropriate color */
      mswindows_update_dc (hdc,
			   cachel->foreground, cachel->background, Qnil);
      redisplay_output_pixmap (w, bg_pmap, &db, &dga, findex,
			       0, 0, 0, TRUE);
      /* output pixmap calls this so we have to recall to get correct
         references */
      cachel = WINDOW_FACE_CACHEL (w, findex);
    }

  nruns = separate_textual_runs (text_storage, runs, Dynarr_atp (buf, 0),
				 Dynarr_length (buf));

  for (i = 0; i < nruns; i++)
    {
      Lisp_Object font = FACE_CACHEL_FONT (cachel, runs[i].charset);
      Lisp_Font_Instance *fi = XFONT_INSTANCE (font);
      int this_width;

      if (EQ (font, Vthe_null_font_instance))
	continue;

      mswindows_update_dc (hdc, cachel->foreground,
			   NILP(bg_pmap) ? cachel->background : Qnil, Qnil);
      mswindows_set_dc_font (hdc, font, cachel->underline, cachel->strikethru);

      this_width = mswindows_text_width_single_run (hdc, cachel, runs + i);
      
      /* cope with fonts taller than lines */
      if ((int) fi->height < (int) (height + dl->clip + dl->top_clip))
	{
	  int clear_start = max (xpos, clip_start);
	  int clear_end = min (xpos + this_width, clip_end);
	  
	  {
	    redisplay_clear_region (window, findex, clear_start,
				    DISPLAY_LINE_YPOS (dl), 
				    clear_end - clear_start,
				    height);
	    /* output pixmap calls this so we have to recall to get correct
	       references */
	    cachel = WINDOW_FACE_CACHEL (w, findex);
	  }
	}

      assert (runs[i].dimension == 1);	/* #### FIXME: Broken when Mule? */
      ExtTextOut (hdc, xpos, dl->ypos,
		  NILP(bg_pmap) ? ETO_CLIPPED | ETO_OPAQUE : ETO_CLIPPED,
		  &rect, (char *) runs[i].ptr, runs[i].len, NULL); 

      xpos += this_width;
    }
}

static void
mswindows_output_dibitmap (struct frame *f, Lisp_Image_Instance *p,
			   struct display_box* db,
			   struct display_glyph_area* dga)
{
  HDC hdc = get_frame_dc (f, 1);
  HDC hcompdc = get_frame_compdc (f);
  HGDIOBJ old=NULL;
  const int real_x = IMAGE_INSTANCE_MSWINDOWS_BITMAP_REAL_WIDTH (p);
  const int real_y = IMAGE_INSTANCE_MSWINDOWS_BITMAP_REAL_HEIGHT (p);
  const int surface_x = IMAGE_INSTANCE_PIXMAP_WIDTH (p);
  const int surface_y = IMAGE_INSTANCE_PIXMAP_HEIGHT (p);

  /* first blit the mask */
  if (IMAGE_INSTANCE_MSWINDOWS_MASK (p))
    {
      RGBQUAD bg;
      COLORREF bgcolor;

      old = SelectObject (hcompdc, IMAGE_INSTANCE_MSWINDOWS_MASK (p));
      
      if (IMAGE_INSTANCE_TYPE (p) == IMAGE_MONO_PIXMAP)
       {
         COLORREF fgcolor;
         RGBQUAD fg;

         fgcolor = GetTextColor (hdc);
         fg.rgbBlue = GetBValue (fgcolor);
         fg.rgbRed = GetRValue (fgcolor);
         fg.rgbGreen = GetGValue (fgcolor);
         fg.rgbReserved = 0;
         SetDIBColorTable (hcompdc, 0, 1, &fg);
       }

      bgcolor = GetBkColor (hdc);
      bg.rgbBlue = GetBValue (bgcolor);
      bg.rgbRed = GetRValue (bgcolor);
      bg.rgbGreen = GetGValue (bgcolor);
      bg.rgbReserved = 0;
      SetDIBColorTable (hcompdc, 1, 1, &bg);

      StretchBlt (hdc, 
		  db->xpos, db->ypos,
		  dga->width, dga->height, 
		  hcompdc,
		  MulDiv (dga->xoffset, real_x, surface_x),
		  MulDiv (dga->yoffset, real_y, surface_y),
		  MulDiv (dga->width, real_x, surface_x),
		  MulDiv (dga->height, real_y, surface_y),
		  SRCCOPY);                  

      SelectObject (hcompdc, old);
    }
  
  /* Now blit the bitmap itself, or one of its slices. */
  old = SelectObject (hcompdc,
		      IMAGE_INSTANCE_MSWINDOWS_BITMAP_SLICE 
		      (p, IMAGE_INSTANCE_PIXMAP_SLICE (p)));

  StretchBlt (hdc, 
	      db->xpos, db->ypos,
	      dga->width, dga->height,
	      hcompdc,
	      MulDiv (dga->xoffset, real_x, surface_x),
	      MulDiv (dga->yoffset, real_y, surface_y),
	      MulDiv (dga->width, real_x, surface_x),
	      MulDiv (dga->height, real_y, surface_y),
	      IMAGE_INSTANCE_MSWINDOWS_MASK (p) ? SRCINVERT : SRCCOPY);

  SelectObject (hcompdc, old);
}

/* X gc's have this nice property that setting the bg pixmap will
 * output it offset relative to the window. Windows doesn't have this
 * feature so we have to emulate this by outputting multiple pixmaps.
 * This is only used for background pixmaps. Normal pixmaps are
 * outputted once and are scrollable */
static void
mswindows_output_dibitmap_region (struct frame *f, 
				  Lisp_Image_Instance *p,
				  struct display_box *db,
				  struct display_glyph_area *dga)
{
  struct display_box xdb = { db->xpos, db->ypos, db->width, db->height };
  struct display_glyph_area xdga
    = { 0, 0, IMAGE_INSTANCE_PIXMAP_WIDTH (p),
	IMAGE_INSTANCE_PIXMAP_HEIGHT (p) };
  int pxoffset = 0, pyoffset = 0;

  if (dga)
    {	
      xdga.width = dga->width;
      xdga.height = dga->height;
    }
  else if (!redisplay_normalize_glyph_area (&xdb, &xdga))
    return;

  /* when doing a bg pixmap do a partial pixmap first so that we
     blt whole pixmaps thereafter */
  xdga.height = min (xdga.height, IMAGE_INSTANCE_PIXMAP_HEIGHT (p) -
		      db->ypos % IMAGE_INSTANCE_PIXMAP_HEIGHT (p));

  while (xdga.height > 0)
    {
      xdga.width = min (min (db->width, IMAGE_INSTANCE_PIXMAP_WIDTH (p)),
			IMAGE_INSTANCE_PIXMAP_WIDTH (p) -
			db->xpos % IMAGE_INSTANCE_PIXMAP_WIDTH (p));
      pxoffset = 0;
      while (xdga.width > 0)
	{
	  xdb.xpos = db->xpos + pxoffset;
	  xdb.ypos = db->ypos + pyoffset;
	    /* do we need to offset the pixmap vertically? this is necessary
	       for background pixmaps. */
	  xdga.yoffset = xdb.ypos % IMAGE_INSTANCE_PIXMAP_HEIGHT (p);
	  xdga.xoffset = xdb.xpos % IMAGE_INSTANCE_PIXMAP_WIDTH (p);
	  /* the width is handled by mswindows_output_pixmap_region */
	  mswindows_output_dibitmap (f, p, &xdb, &xdga);
	  pxoffset += xdga.width;
	  xdga.width = min ((db->width - pxoffset),
			    IMAGE_INSTANCE_PIXMAP_WIDTH (p));
	}
      pyoffset += xdga.height;
      xdga.height = min ((db->height - pyoffset), 
			 IMAGE_INSTANCE_PIXMAP_HEIGHT (p));
    }
}

/* Output a pixmap at the desired location. 
   DB		normalized display_box.
   DGA		normalized display_glyph_area. */
static void
mswindows_output_pixmap (struct window *w, Lisp_Object image_instance,
			 struct display_box *db, struct display_glyph_area *dga,
			 face_index findex, int cursor_start, int cursor_width,
			 int cursor_height, int bg_pixmap)
{
  struct frame *f = XFRAME (w->frame);
  HDC hdc = get_frame_dc (f, 1);

  Lisp_Image_Instance *p = XIMAGE_INSTANCE (image_instance);
  Lisp_Object window;

  XSETWINDOW (window, w);

  /* Output the pixmap. Have to do this as many times as is required
   to fill the given area */
  mswindows_update_dc (hdc,
		       WINDOW_FACE_CACHEL_FOREGROUND (w, findex),
		       WINDOW_FACE_CACHEL_BACKGROUND (w, findex), Qnil);

  if (bg_pixmap)
    mswindows_output_dibitmap_region (f, p, db, dga);
  else
    mswindows_output_dibitmap (f, p, db, dga);
}

#ifdef HAVE_SCROLLBARS
/*
 * This function paints window's deadbox, a rectangle between window
 * borders and two short edges of both scrollbars.
 *
 * Function checks whether deadbox intersects with the rectangle pointed
 * to by PRC, and paints only the intersection
 */
static void
mswindows_redisplay_deadbox_maybe (struct window *w, const RECT* prc)
{
  int sbh = window_scrollbar_height (w);
  int sbw = window_scrollbar_width (w);
  RECT rect_dead, rect_paint;
  if (sbh == 0 || sbw == 0)
    return;

  if (!NILP (w->scrollbar_on_left_p))
    rect_dead.left = WINDOW_LEFT (w);
  else
    rect_dead.left = WINDOW_TEXT_RIGHT (w);
  rect_dead.right = rect_dead.left + sbw;

  if (!NILP (w->scrollbar_on_top_p))
    rect_dead.top = WINDOW_TOP (w);
  else
    rect_dead.top = WINDOW_TEXT_BOTTOM (w);
  rect_dead.bottom = rect_dead.top + sbh;
      
  if (IntersectRect (&rect_paint, &rect_dead, prc))
    {
      struct frame *f = XFRAME (WINDOW_FRAME (w));
      FillRect (get_frame_dc (f, 1), &rect_paint,
		(HBRUSH) (COLOR_BTNFACE+1));
    }
}

#endif /* HAVE_SCROLLBARS */

/*****************************************************************************
 mswindows_redraw_exposed_window

 Given a bounding box for an area that needs to be redrawn, determine
 what parts of what lines are contained within and re-output their
 contents.
 Copied from redisplay-x.c
 ****************************************************************************/
static void
mswindows_redraw_exposed_window (struct window *w, int x, int y, int width,
			   int height)
{
  struct frame *f = XFRAME (w->frame);
  int line;
  int orig_windows_structure_changed;
  RECT rect_window = { WINDOW_LEFT (w), WINDOW_TOP (w),
		       WINDOW_RIGHT (w), WINDOW_BOTTOM (w) };
  RECT rect_expose = { x, y, x + width, y + height };
  RECT rect_draw;

  display_line_dynarr *cdla = window_display_lines (w, CURRENT_DISP);

  if (!NILP (w->vchild))
    {
      mswindows_redraw_exposed_windows (w->vchild, x, y, width, height);
      return;
    }
  else if (!NILP (w->hchild))
    {
      mswindows_redraw_exposed_windows (w->hchild, x, y, width, height);
      return;
    }

  /* If the window doesn't intersect the exposed region, we're done here. */
  if (!IntersectRect (&rect_draw, &rect_window, &rect_expose))
      return;

  /* We do this to make sure that the 3D modelines get redrawn if
     they are in the exposed region. */
  orig_windows_structure_changed = f->windows_structure_changed;
  f->windows_structure_changed = 1;

  if (window_needs_vertical_divider (w))
    {
      mswindows_output_vertical_divider (w, 0);
    }

  for (line = 0; line < Dynarr_length (cdla); line++)
    {
      struct display_line *cdl = Dynarr_atp (cdla, line);

      if (DISPLAY_LINE_YPOS (cdl) + DISPLAY_LINE_HEIGHT (cdl)
	  >= rect_draw.top)
	{
	  if (DISPLAY_LINE_YPOS (cdl) > rect_draw.bottom)
	    {
	      if (line == 0)
		continue;
	      else
		break;
	    }
	  else
	    {
	      output_display_line (w, 0, cdla, line,
				   rect_draw.left, rect_draw.right);
	    }
	}
    }

  f->windows_structure_changed = orig_windows_structure_changed;

  /* If there have never been any face cache_elements created, then this
     expose event doesn't actually have anything to do. */
  if (Dynarr_largest (w->face_cachels))
    redisplay_clear_bottom_of_window (w, cdla, rect_draw.top, rect_draw.bottom);

#ifdef HAVE_SCROLLBARS
  mswindows_redisplay_deadbox_maybe (w, &rect_expose);
#endif
}

/*****************************************************************************
 mswindows_redraw_exposed_windows

 For each window beneath the given window in the window hierarchy,
 ensure that it is redrawn if necessary after an Expose event.
 ****************************************************************************/
static void
mswindows_redraw_exposed_windows (Lisp_Object window, int x, int y, int width,
			    int height)
{
  for (; !NILP (window); window = XWINDOW (window)->next)
    mswindows_redraw_exposed_window (XWINDOW (window), x, y, width, height);
}

/*****************************************************************************
 mswindows_redraw_exposed_area

 For each window on the given frame, ensure that any area in the
 Exposed area is redrawn.
 ****************************************************************************/
void
mswindows_redraw_exposed_area (struct frame *f, int x, int y, int width, int height)
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
	  mswindows_redraw_exposed_windows (f->root_window, x, y, width, height);
	  GdiFlush();
	}
  else
    MARK_FRAME_CHANGED (f);
}


/*****************************************************************************
 mswindows_bevel_area

 Draw a 3d border around the specified area on window W.
 ****************************************************************************/
static void
mswindows_bevel_area (struct window *w, face_index findex, int x, int y, 
		      int width, int height, int thickness,
		      int edges, enum edge_style style)
{
  struct frame *f = XFRAME (w->frame);
  UINT edge;
  UINT border = 0;

  if (style == EDGE_ETCHED_IN)
    edge = EDGE_ETCHED;
  else if (style == EDGE_ETCHED_OUT)
    edge = EDGE_BUMP;
  else if (style == EDGE_BEVEL_IN)
    {
      if (thickness == 1)
	edge = BDR_SUNKENINNER;
      else
	edge = EDGE_SUNKEN;
    }
  else				/* EDGE_BEVEL_OUT */
    {
      if (thickness == 1)
	edge = BDR_RAISEDINNER;
      else
	edge = EDGE_RAISED;
    }

  if (edges & EDGE_TOP)
    border |= BF_TOP;
  if (edges & EDGE_LEFT)
    border |= BF_LEFT;
  if (edges & EDGE_BOTTOM)
    border |= BF_BOTTOM;
  if (edges & EDGE_RIGHT)
    border |= BF_RIGHT;

  {
    RECT rect = { x, y, x + width, y + height };
    Lisp_Object color = WINDOW_FACE_CACHEL_BACKGROUND (w, findex);
    HDC hdc = get_frame_dc (f, 1);

    mswindows_update_dc (hdc, Qnil, color, Qnil);
    DrawEdge (hdc, &rect, edge, border);
  }
}


/*****************************************************************************
 Display methods
*****************************************************************************/

/*****************************************************************************
 mswindows_divider_height

 Return the height of the horizontal divider.
 ****************************************************************************/
static int
mswindows_divider_height (void)
{
  return 1;   /* XXX Copied from redisplay-X.c. What is this? */
}

/*****************************************************************************
 mswindows_eol_cursor_width

 Return the width of the end-of-line cursor.
 ****************************************************************************/
static int
mswindows_eol_cursor_width (void)
{
  return MSWINDOWS_EOL_CURSOR_WIDTH;
}

/*****************************************************************************
 mswindows_frame_output_begin

 Perform any necessary initialization prior to an update.
 ****************************************************************************/
static void
mswindows_frame_output_begin (struct frame *f)
{
}

/*****************************************************************************
 mswindows_frame_output_end

 Perform any necessary flushing of queues when an update has completed.
 ****************************************************************************/
static void
mswindows_frame_output_end (struct frame *f)
{
#ifdef DEFER_WINDOW_POS
  HDWP hdwp = FRAME_MSWINDOWS_DATA (f)->hdwp;

  if (hdwp != 0)
    {
      EndDeferWindowPos (hdwp);
      FRAME_MSWINDOWS_DATA (f)->hdwp = 0;
    }
#endif
  GdiFlush();
}

/* Printer version is more lightweight. */
static void
msprinter_frame_output_end (struct frame *f)
{
  GdiFlush();
}

static int
mswindows_flash (struct device *d)
{
  struct frame *f = device_selected_frame (d);
  HDC hdc = get_frame_dc (f, 1);
  RECT rc;

  GetClientRect (FRAME_MSWINDOWS_HANDLE (f), &rc);
  InvertRect (hdc, &rc);
  GdiFlush ();
  Sleep (25);
  InvertRect (hdc, &rc);

  return 1;
}

static void
mswindows_ring_bell (struct device *d, int volume, int pitch, int duration)
{
  /* Beep does not work at all, anyways! -kkm */
  MessageBeep (MB_OK);
}

/*****************************************************************************
 mswindows_output_display_block

 Given a display line, a block number for that start line, output all
 runes between start and end in the specified display block.
 Ripped off with minimal thought from the corresponding X routine.
 ****************************************************************************/
static void
mswindows_output_display_block (struct window *w, struct display_line *dl, int block,
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
      /* Nothing to do so don't do anything. */
      return;

  findex = rb->findex;
  xpos = rb->xpos;
  width = 0;
  if (rb->type == RUNE_CHAR)
    charset = CHAR_CHARSET (rb->object.chr.ch);

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
	      mswindows_output_string (w, dl, buf, xpos, 0, start_pixpos, width,
				 findex, 0, 0, 0, 0);
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
		      mswindows_output_cursor (w, dl, xpos, cursor_width,
					       findex, 0, 0);
		    }
		  else
		    {
		      Dynarr_add (buf, rb->object.chr.ch);
		      mswindows_output_cursor (w, dl, xpos, cursor_width,
					       findex, rb->object.chr.ch, 0);
		      Dynarr_reset (buf);
		    }

		  xpos += rb->width;
		  elt++;
		}
	      else if (rb->object.chr.ch == '\n')
		{
		  /* Clear in case a cursor was formerly here. */
		  redisplay_clear_region (window, findex, xpos, 
					  DISPLAY_LINE_YPOS (dl),
					  rb->width, DISPLAY_LINE_HEIGHT (dl));
		  elt++;
		}
	    }
	  else if (rb->type == RUNE_BLANK || rb->type == RUNE_HLINE)
	    {
	      if (rb->type == RUNE_BLANK)
		mswindows_output_blank (w, dl, rb, start_pixpos);
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
		  mswindows_output_hline (w, dl, rb);
		}

	      if (rb->cursor_type == CURSOR_ON)
		mswindows_output_cursor (w, dl, xpos, cursor_width, rb->findex, 0, 0);

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
						 rb->object.dglyph.yoffset,
                                                 start_pixpos, rb->width, &dbox, &dga);

	      XSETWINDOW (window, w);
	      instance = glyph_image_instance (rb->object.dglyph.glyph,
					       window, ERROR_ME_NOT, 1);
	      findex = rb->findex;

	      if (IMAGE_INSTANCEP (instance))
		{
		  switch (XIMAGE_INSTANCE_TYPE (instance))
		    {
		    case IMAGE_MONO_PIXMAP:
		    case IMAGE_COLOR_PIXMAP:
		      redisplay_output_pixmap (w, instance, &dbox, &dga, findex,
					       cursor_start, cursor_width,
					       cursor_height, 0);
		      if (rb->cursor_type == CURSOR_ON)
			mswindows_output_cursor (w, dl, xpos, cursor_width,
						 findex, 0, 1);
		      break;
		      
		    case IMAGE_WIDGET:
		      if (EQ (XIMAGE_INSTANCE_WIDGET_TYPE (instance),
			      Qlayout))
			{
			  redisplay_output_layout (window, instance, &dbox, &dga, findex,
						   cursor_start, cursor_width,
						   cursor_height);
			  if (rb->cursor_type == CURSOR_ON)
			    mswindows_output_cursor (w, dl, xpos, cursor_width,
						     findex, 0, 1);
			  break;
			}
		    case IMAGE_SUBWINDOW:
		      redisplay_output_subwindow (w, instance, &dbox, &dga, findex,
						  cursor_start, cursor_width,
						  cursor_height);
		      if (rb->cursor_type == CURSOR_ON)
			mswindows_output_cursor (w, dl, xpos, cursor_width,
						 findex, 0, 1);
		      break;
		      
		    case IMAGE_NOTHING:
		      /* nothing is as nothing does */
		      break;

		    case IMAGE_TEXT:
		    case IMAGE_POINTER:
		    default:
		      abort ();
		    }
		  IMAGE_INSTANCE_OPTIMIZE_OUTPUT 
		    (XIMAGE_INSTANCE (instance)) = 0;
		}
	      xpos += rb->width;
	      elt++;
	    }
	  else
	    abort ();
	}
    }

  if (Dynarr_length (buf))
    mswindows_output_string (w, dl, buf, xpos, 0, start_pixpos, width, findex,
			     0, 0, 0, 0);

  if (dl->modeline
      && !EQ (Qzero, w->modeline_shadow_thickness)
      && (f->clear
	  || f->windows_structure_changed
	  || w->shadow_thickness_changed))
    bevel_modeline (w, dl);

  Dynarr_free (buf);
}


/*****************************************************************************
 mswindows_output_vertical_divider

 Draw a vertical divider down the right side of the given window.
 ****************************************************************************/
static void
mswindows_output_vertical_divider (struct window *w, int clear_unused)
{
  struct frame *f = XFRAME (w->frame);
  HDC hdc = get_frame_dc (f, 1);
  RECT rect;
  int spacing = XINT (w->vertical_divider_spacing);
  int shadow = XINT (w->vertical_divider_shadow_thickness);
  int abs_shadow = abs (shadow);
  int line_width = XINT (w->vertical_divider_line_width);
  int div_left = WINDOW_RIGHT (w) - window_divider_width (w);
  int y1 = WINDOW_TOP (w);
  int y2 = WINDOW_BOTTOM (w);

  /* Clear left and right spacing areas */
  if (spacing)
    {
      rect.top = y1;
      rect.bottom = y2;
      mswindows_update_dc (hdc, Qnil,
		   WINDOW_FACE_CACHEL_BACKGROUND (w, DEFAULT_INDEX), Qnil);
      rect.right = WINDOW_RIGHT (w);
      rect.left = rect.right - spacing;
      ExtTextOut (hdc, 0, 0, ETO_OPAQUE, 
		  &rect, NULL, 0, NULL);
      rect.left = div_left;
      rect.right = div_left + spacing;
      ExtTextOut (hdc, 0, 0, ETO_OPAQUE, 
		  &rect, NULL, 0, NULL);
    }
  
  /* Clear divider face */
  rect.top = y1 + abs_shadow;
  rect.bottom = y2 - abs_shadow;
  rect.left = div_left + spacing + abs_shadow;
  rect.right = rect.left + line_width;
  if (rect.left < rect.right)
    {
      face_index div_face
	= get_builtin_face_cache_index (w, Vvertical_divider_face);
      mswindows_update_dc (hdc, Qnil,
		   WINDOW_FACE_CACHEL_BACKGROUND (w, div_face), Qnil);
      ExtTextOut (hdc, 0, 0, ETO_OPAQUE, &rect, NULL, 0, NULL);
    }

  /* Draw a shadow around the divider */
  if (shadow != 0)
    {
      /* #### This will be fixed to support arbitrary thickness */
      InflateRect (&rect, abs_shadow, abs_shadow);
      DrawEdge (hdc, &rect,
		shadow > 0 ? EDGE_RAISED : EDGE_SUNKEN, BF_RECT);
    }
}

/****************************************************************************
 mswindows_text_width

 Given a string and a face, return the string's length in pixels when
 displayed in the font associated with the face.
 ****************************************************************************/
static int
mswindows_text_width (struct frame *f, struct face_cachel *cachel,
		      const Emchar *str, Charcount len)
{
  HDC hdc = get_frame_dc (f, 0);
  int width_so_far = 0;
  unsigned char *text_storage = (unsigned char *) alloca (2 * len);
  textual_run *runs = alloca_array (textual_run, len);
  int nruns;
  int i;

  nruns = separate_textual_runs (text_storage, runs, str, len);

  for (i = 0; i < nruns; i++)
    width_so_far += mswindows_text_width_single_run (hdc,
						     cachel, runs + i);

  return width_so_far;
}


/****************************************************************************
 mswindows_clear_region

 Clear the area in the box defined by the given parameters using the
 given face.
 ****************************************************************************/
static void
mswindows_clear_region (Lisp_Object locale, struct device* d, struct frame* f, 
			face_index findex, int x, int y,
			int width, int height, Lisp_Object fcolor, Lisp_Object bcolor,
			Lisp_Object background_pixmap)
{
  RECT rect = { x, y, x+width, y+height };
  HDC hdc = get_frame_dc (f, 1);

  if (!NILP (background_pixmap))
    {
      struct display_box db = { x, y, width, height };
      mswindows_update_dc (hdc,
			   fcolor, bcolor, background_pixmap);
      mswindows_output_dibitmap_region 
	( f, XIMAGE_INSTANCE (background_pixmap), &db, 0);
    }
  else
    {
      mswindows_update_dc (hdc, Qnil, fcolor, Qnil);
      ExtTextOut (hdc, 0, 0, ETO_OPAQUE, 
		  &rect, NULL, 0, NULL);
    }

#ifdef HAVE_SCROLLBARS
  if (WINDOWP (locale))
    mswindows_redisplay_deadbox_maybe (XWINDOW (locale), &rect);
#endif
}

/* XXX Implement me! */
static void
mswindows_clear_frame (struct frame *f)
{
  GdiFlush();
}



/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void
console_type_create_redisplay_mswindows (void)
{
  /* redisplay methods - display*/
  CONSOLE_HAS_METHOD (mswindows, text_width);
  CONSOLE_HAS_METHOD (mswindows, output_display_block);
  CONSOLE_HAS_METHOD (mswindows, divider_height);
  CONSOLE_HAS_METHOD (mswindows, eol_cursor_width);
  CONSOLE_HAS_METHOD (mswindows, output_vertical_divider);
  CONSOLE_HAS_METHOD (mswindows, clear_region);
  CONSOLE_HAS_METHOD (mswindows, clear_frame);
  CONSOLE_HAS_METHOD (mswindows, frame_output_begin);
  CONSOLE_HAS_METHOD (mswindows, frame_output_end);
  CONSOLE_HAS_METHOD (mswindows, flash);
  CONSOLE_HAS_METHOD (mswindows, ring_bell);
  CONSOLE_HAS_METHOD (mswindows, bevel_area);
  CONSOLE_HAS_METHOD (mswindows, output_string);
  CONSOLE_HAS_METHOD (mswindows, output_pixmap);

  /* redisplay methods - printer */
  CONSOLE_HAS_METHOD (msprinter, frame_output_end);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, text_width);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, output_display_block);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, divider_height);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, eol_cursor_width);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, output_vertical_divider);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, clear_region);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, clear_frame);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, frame_output_begin);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, bevel_area);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, output_string);
  CONSOLE_INHERITS_METHOD (msprinter, mswindows, output_pixmap);
}
