/* Synchronize redisplay structures and output changes.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1996 Chuck Thompson.
   Copyright (C) 1999, 2002 Andy Piper.

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

/* This file has been Mule-ized. */

/* Author: Chuck Thompson */

/* Heavily hacked for modularity, gutter and subwindow support by Andy
   Piper. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "window.h"
#include "frame.h"
#include "device.h"
#include "glyphs.h"
#include "redisplay.h"
#include "faces.h"
#include "gutter.h"

static int compare_runes (struct window *w, struct rune *crb,
			  struct rune *drb);
static void redraw_cursor_in_window (struct window *w,
				     int run_end_begin_glyphs);
static void redisplay_output_display_block (struct window *w, struct display_line *dl,
					    int block, int start, int end, int start_pixpos,
					    int cursor_start, int cursor_width,
					    int cursor_height);
static void redisplay_normalize_display_box (struct display_box* dest,
					     struct display_glyph_area* src);
static int redisplay_display_boxes_in_window_p (struct window* w,
						struct display_box* db,
						struct display_glyph_area* dga);
static void redisplay_clear_clipped_region (Lisp_Object locale, face_index findex,
					    struct display_box* dest,
					    struct display_glyph_area* glyphsrc,
					    int fullheight_p, Lisp_Object);

/*****************************************************************************
 sync_rune_structs

 Synchronize the given rune blocks.
 ****************************************************************************/
static void
sync_rune_structs (struct window *w, rune_dynarr *cra, rune_dynarr *dra)
{
  int rune_elt;
  int max_move = ((Dynarr_length (dra) > Dynarr_largest (cra))
		  ? Dynarr_largest (cra)
		  : Dynarr_length (dra));

  if (max_move)
    {
      /* #### Doing this directly breaks the encapsulation.  But, the
         running time of this function has a measurable impact on
         redisplay performance so avoiding all excess overhead is a
         good thing.  Is all of this true? */
      memcpy (cra->base, dra->base, sizeof (struct rune) * max_move);
      Dynarr_set_size (cra, max_move);
    }
  else
    Dynarr_reset (cra);

  for (rune_elt = max_move; rune_elt < Dynarr_length (dra); rune_elt++)
    {
      struct rune rb, *crb;
      struct rune *drb = Dynarr_atp (dra, rune_elt);

      crb = &rb;
      memcpy (crb, drb, sizeof (struct rune));
      Dynarr_add (cra, *crb);
    }
}

/*****************************************************************************
 sync_display_line_structs

 For the given LINE in window W, make the current display line equal
 the desired display line.
 ****************************************************************************/
void
sync_display_line_structs (struct window *w, int line, int do_blocks,
			   display_line_dynarr *cdla,
			   display_line_dynarr *ddla)
{
  int cdla_len = Dynarr_length (cdla);

  struct display_line dl, *clp, *dlp;
  int db_elt;

  dlp = Dynarr_atp (ddla, line);
  if (line >= Dynarr_largest (cdla))
    {
      clp = &dl;
      clp->display_blocks = Dynarr_new (display_block);
    }
  else
    {
      clp = Dynarr_atp (cdla, line);
      if (clp->display_blocks)
	Dynarr_reset (clp->display_blocks);
      if (clp->left_glyphs)
	{
	  Dynarr_free (clp->left_glyphs);
	  clp->left_glyphs = 0;
	}
      if (clp->right_glyphs)
	{
	  Dynarr_free (clp->right_glyphs);
	  clp->right_glyphs = 0;
	}
    }
  {
    display_block_dynarr *tdb = clp->display_blocks;

    memcpy (clp, dlp, sizeof (struct display_line));
    clp->display_blocks = tdb;
    clp->left_glyphs = 0;
    clp->right_glyphs = 0;
  }

  if (!do_blocks && line >= cdla_len)
    {
      Dynarr_add (cdla, *clp);
      return;
    }

  for (db_elt = 0; db_elt < Dynarr_length (dlp->display_blocks); db_elt++)
    {
      struct display_block db, *cdb;
      struct display_block *ddb = Dynarr_atp (dlp->display_blocks, db_elt);

      if (db_elt >= Dynarr_largest (clp->display_blocks))
	{
	  cdb = &db;
	  memcpy (cdb, ddb, sizeof (struct display_block));
	  cdb->runes = Dynarr_new (rune);
	  Dynarr_add (clp->display_blocks, *cdb);
	}
      else
	{
	  rune_dynarr *tr;

	  cdb = Dynarr_atp (clp->display_blocks, db_elt);
	  tr = cdb->runes;
	  memcpy (cdb, ddb, sizeof (struct display_block));
	  cdb->runes = tr;
	  Dynarr_increment (clp->display_blocks);
	}

      sync_rune_structs (w, cdb->runes, ddb->runes);
    }

  if (line >= cdla_len)
    Dynarr_add (cdla, *clp);
}

/*****************************************************************************
 compare_runes

 Compare two runes to see if each of their fields is equal.  If so,
 return true otherwise return false.
 ****************************************************************************/
static int
compare_runes (struct window *w, struct rune *crb, struct rune *drb)
{
  /* Do not compare the values of bufpos and endpos.  They do not
     affect the display characteristics. */

  /* Note: (hanoi 6) spends 95% of its time in redisplay, and about
     30% here. Not using bitfields for rune.type alone gives a redisplay
     speed up of 10%.

     #### In profile arcs run of a normal Gnus session this function
     is run 6.76 million times, only to return 1 in 6.73 million of
     those.

     In addition a quick look GCC sparc assembly shows that GCC is not
     doing a good job here.
     1. The function is not inlined (too complicated?)
     2. It seems to be reloading the crb and drb variables all the
     time.
     3. It doesn't seem to notice that the second half of these if's
     are really a switch statement.

     So I (JV) conjecture

     #### It would really be worth it to arrange for this function to
     be (almost) a single call to memcmp. */

  if (crb->xpos != drb->xpos)
    return 0;
  else if (crb->width != drb->width)
    return 0;
  else if (crb->cursor_type != drb->cursor_type)
    return 0;
  else if (crb->type != drb->type)
    return 0;
  else if (crb->type == RUNE_CHAR &&
	   (crb->object.chr.ch != drb->object.chr.ch))
    return 0;
  else if (crb->type == RUNE_HLINE &&
	   (crb->object.hline.thickness != drb->object.hline.thickness ||
	    crb->object.hline.yoffset != drb->object.hline.yoffset))
    return 0;
  else if (crb->type == RUNE_DGLYPH &&
	   (!EQ (crb->object.dglyph.glyph, drb->object.dglyph.glyph) ||
	    !EQ (crb->object.dglyph.extent, drb->object.dglyph.extent) ||
	    crb->object.dglyph.xoffset != drb->object.dglyph.xoffset ||
	    crb->object.dglyph.yoffset != drb->object.dglyph.yoffset ||
            crb->object.dglyph.ascent != drb->object.dglyph.ascent ||
            crb->object.dglyph.descent != drb->object.dglyph.descent))
    return 0;
  /* Only check dirtiness if we know something has changed. */
  else if (crb->type == RUNE_DGLYPH &&
	   (XGLYPH_DIRTYP (crb->object.dglyph.glyph) ||
	    crb->findex != drb->findex))
    {
      /* We need some way of telling redisplay_output_layout () that the
         only reason we are outputting it is because something has
         changed internally. That way we can optimize whether we need
         to clear the layout first and also only output the components
         that have changed. The image_instance dirty flag and
         display_hash are no good to us because these will invariably
         have been set anyway if the layout has changed. So it looks
         like we need yet another change flag that we can set here and
         then clear in redisplay_output_layout (). */
      Lisp_Object window, image;
      Lisp_Image_Instance* ii;
      XSETWINDOW (window, w);
      image = glyph_image_instance (crb->object.dglyph.glyph,
				    window, ERROR_ME_NOT, 1);

      if (!IMAGE_INSTANCEP (image))
	return 0;
      ii = XIMAGE_INSTANCE (image);

      if (TEXT_IMAGE_INSTANCEP (image) &&
	  (crb->findex != drb->findex ||
	   WINDOW_FACE_CACHEL_DIRTY (w, drb->findex)))
	return 0;

      /* It is quite common for the two glyphs to be EQ since in many
	 cases they will actually be the same object. This does not
	 mean, however, that nothing has changed. We therefore need to
	 check the current hash of the glyph against the last recorded
	 display hash and the pending display items. See
	 update_subwindow (). */
      if (image_instance_changed (image) ||
	  crb->findex != drb->findex ||
	  WINDOW_FACE_CACHEL_DIRTY (w, drb->findex))
	{
	  /* Now we are going to re-output the glyph, but since
	     this is for some internal reason not related to geometry
	     changes, send a hint to the output routines that they can
	     take some short cuts. This is most useful for
	     layouts. This flag should get reset by the output
	     routines.

	     #### It is possible for us to get here when the
	     face_cachel is dirty. I do not know what the implications
	     of this are.*/
	  IMAGE_INSTANCE_OPTIMIZE_OUTPUT (ii) = 1;
	  return 0;
	}
      else
	return 1;
    }
  /* We now do this last so that glyph checks can do their own thing
     for face changes. Face changes quite often happen when we are
     trying to output something in the gutter, this would normally
     lead to a lot of flashing. The indices can quite often be
     different and yet the faces are the same, we do not want to
     re-output in this instance. */
  else  if (crb->findex != drb->findex ||
	    WINDOW_FACE_CACHEL_DIRTY (w, drb->findex))
    return 0;
  else
    return 1;
}

/*****************************************************************************
 get_next_display_block

 Return the next display starting at or overlapping START_POS.  Return
 the start of the next region in NEXT_START.
 ****************************************************************************/
int
get_next_display_block (layout_bounds bounds, display_block_dynarr *dba,
			int start_pos, int *next_start)
{
  int next_display_block = NO_BLOCK;
  int priority = -1;
  int block;

  /* If we don't find a display block covering or starting at
     start_pos, then we return the starting point of the next display
     block or the next division boundary, whichever is closer to
     start_pos. */
  if (next_start)
    {
      if (start_pos >= bounds.left_out && start_pos < bounds.left_in)
	*next_start = bounds.left_in;
      else if (start_pos < bounds.left_white)
	*next_start = bounds.left_white;
      else if (start_pos < bounds.right_white)
	*next_start = bounds.right_white;
      else if (start_pos < bounds.right_in)
	*next_start = bounds.right_in;
      else if (start_pos <= bounds.right_out)
	*next_start = bounds.right_out;
      else
	abort ();
    }

  for (block = 0; block < Dynarr_length (dba); block++)
    {
      struct display_block *db = Dynarr_atp (dba, block);

      if (db->start_pos <= start_pos && db->end_pos > start_pos)
	{
	  if ((int) db->type > priority)
	    {
	      priority = db->type;
	      next_display_block = block;
	      if (next_start)
		*next_start = db->end_pos;
	    }
	}
      else if (next_start && db->start_pos > start_pos)
	{
	  if (db->start_pos < *next_start)
	    *next_start = db->start_pos;
	}
    }

  return next_display_block;
}

/*****************************************************************************
 get_cursor_size_and_location

 Return the information defining the pixel location of the cursor.
 ****************************************************************************/
static void
get_cursor_size_and_location (struct window *w, struct display_block *db,
			      int cursor_location,
			      int *cursor_start, int *cursor_width,
			      int *cursor_height)
{
  struct rune *rb;
  Lisp_Object window;
  int defheight, defwidth;

  if (Dynarr_length (db->runes) <= cursor_location)
    abort ();

  XSETWINDOW (window, w);

  rb = Dynarr_atp (db->runes, cursor_location);
  *cursor_start = rb->xpos;

  default_face_height_and_width (window, &defheight, &defwidth);
  *cursor_height = defheight;

  if (rb->type == RUNE_BLANK)
    *cursor_width = defwidth;
  else
    *cursor_width = rb->width;
}

/*****************************************************************************
 compare_display_blocks

 Given two display blocks, output only those areas where they differ.
 ****************************************************************************/
static int
compare_display_blocks (struct window *w, struct display_line *cdl,
			struct display_line *ddl, int c_block, int d_block,
			int start_pixpos, int cursor_start, int cursor_width,
			int cursor_height)
{
  struct frame *f = XFRAME (w->frame);
  struct display_block *cdb, *ddb;
  int start_pos;
  int stop_pos;
  int force = 0;
  int block_end;

  cdb = Dynarr_atp (cdl->display_blocks, c_block);
  ddb = Dynarr_atp (ddl->display_blocks, d_block);

  assert (cdb->type == ddb->type);

  start_pos = -1;
  stop_pos = min (Dynarr_length (cdb->runes), Dynarr_length (ddb->runes));

  block_end =
    (!Dynarr_length (ddb->runes)
     ? 0
     : (Dynarr_atp (ddb->runes, Dynarr_length (ddb->runes) - 1)->xpos +
	Dynarr_atp (ddb->runes, Dynarr_length (ddb->runes) - 1)->width));

  /* If the new block type is not text and the cursor status is
     changing and it overlaps the position of this block then force a
     full redraw of the block in order to make sure that the cursor is
     updated properly. */
  if (ddb->type != TEXT
#if 0
      /* I'm not sure exactly what this code wants to do, but it's
       * not right--it doesn't update when cursor_elt changes from, e.g.,
       * 0 to 8, and the new or old cursor loc overlaps this block.
       * I've replaced it with the more conservative test below.
       * -dkindred@cs.cmu.edu 23-Mar-1997 */
      && ((cdl->cursor_elt == -1 && ddl->cursor_elt != -1)
	  || (cdl->cursor_elt != -1 && ddl->cursor_elt == -1))
      && (ddl->cursor_elt == -1 ||
	  (cursor_start
	   && cursor_width
	   && (cursor_start + cursor_width) >= start_pixpos
	   && cursor_start <= block_end))
#else
      && (cdl->cursor_elt != ddl->cursor_elt)
#endif
      )
    force = 1;

  if (f->windows_structure_changed ||
      /* #### Why is this so? We have face cachels so that we don't
         have to recalculate all the display blocks when faces
         change. I have fixed this for glyphs and am inclined to think
         that faces should "Just Work", but I'm not feeling brave
         today. Maybe its because the face cachels represent merged
         faces rather than simply instantiations in a particular
         domain. */
      f->faces_changed ||
      cdl->ypos != ddl->ypos ||
      cdl->ascent != ddl->ascent ||
      cdl->descent != ddl->descent ||
      cdl->clip != ddl->clip ||
      force)
    {
      start_pos = 0;
      force = 1;
    }
  else
    {
      int elt = 0;

      while (start_pos < 0 && elt < stop_pos)
	{
	  if (!compare_runes (w, Dynarr_atp (cdb->runes, elt),
			      Dynarr_atp (ddb->runes, elt)))
	    {
	      start_pos = elt;
	    }
	  else
	    {
	      elt++;
	    }
	}

      /* If nothing has changed in the area where the blocks overlap, but
	 there are new blocks in the desired block, then adjust the start
	 point accordingly. */
      if (elt == stop_pos && stop_pos < Dynarr_length (ddb->runes))
	start_pos = stop_pos;
    }

  if (start_pos >= 0)
    {
      if ((Dynarr_length (ddb->runes) != Dynarr_length (cdb->runes))
	  || force)
	{
	  stop_pos = Dynarr_length (ddb->runes);
	}
      else
	{
	  /* If the lines have the same number of runes and we are not
	     forcing a full redraw because the display line has
	     changed position then we try and optimize how much of the
	     line we actually redraw by scanning backwards from the
	     end for the first changed rune.  This optimization is
	     almost always triggered by face changes. */

	  int elt = Dynarr_length (ddb->runes) - 1;

	  while (elt > start_pos)
	    {
	      if (!compare_runes (w, Dynarr_atp (cdb->runes, elt),
				  Dynarr_atp (ddb->runes, elt)))
		break;
	      else
		elt--;
	    }
	  stop_pos = elt + 1;
	}

      redisplay_output_display_block (w, ddl, d_block, start_pos,
				      stop_pos, start_pixpos,
				      cursor_start, cursor_width,
				      cursor_height);
      return 1;
    }

  return 0;
}

/*****************************************************************************
 clear_left_border

 Clear the lefthand outside border.
 ****************************************************************************/
static void
clear_left_border (struct window *w, int y, int height)
{
  struct frame *f = XFRAME (w->frame);
  Lisp_Object window;

  XSETWINDOW (window, w);
  redisplay_clear_region (window, DEFAULT_INDEX,
		FRAME_LEFT_BORDER_START (f), y,
		FRAME_BORDER_WIDTH (f), height);
}

/*****************************************************************************
 clear_right_border

 Clear the righthand outside border.
 ****************************************************************************/
static void
clear_right_border (struct window *w, int y, int height)
{
  struct frame *f = XFRAME (w->frame);
  Lisp_Object window;

  XSETWINDOW (window, w);
  redisplay_clear_region (window, DEFAULT_INDEX,
		FRAME_RIGHT_BORDER_START (f),
		y, FRAME_BORDER_WIDTH (f), height);
}

/*****************************************************************************
 output_display_line

 Ensure that the contents of the given display line is correct
 on-screen.  The force_ parameters are used by redisplay_move_cursor
 to correctly update cursor locations and only cursor locations.
 ****************************************************************************/
void
output_display_line (struct window *w, display_line_dynarr *cdla,
		     display_line_dynarr *ddla, int line, int force_start,
		     int force_end)

{
  struct frame *f = XFRAME (w->frame);
  struct buffer *b = XBUFFER (w->buffer);
  struct buffer *old_b = window_display_buffer (w);
  struct display_line *cdl, *ddl;
  display_block_dynarr *cdba, *ddba;
  int start_pixpos, end_pixpos;
  int cursor_start, cursor_width, cursor_height;

  int force = (force_start >= 0 || force_end >= 0);
  int clear_border = 0;
  int must_sync = 0;

  if (cdla && line < Dynarr_length (cdla))
    {
      cdl = Dynarr_atp (cdla, line);
      cdba = cdl->display_blocks;
    }
  else
    {
      cdl = NULL;
      cdba = NULL;
    }

  ddl = Dynarr_atp (ddla, line);      /* assert line < Dynarr_length (ddla) */
  ddba = ddl->display_blocks;

  if (force_start >= 0 && force_start >= ddl->bounds.left_out)
    start_pixpos = force_start;
  else
    start_pixpos = ddl->bounds.left_out;

  if (force_end >= 0 && force_end < ddl->bounds.right_out)
    end_pixpos = force_end;
  else
    end_pixpos = ddl->bounds.right_out;

  /* Get the cursor parameters. */
  if (ddl->cursor_elt != -1)
    {
      struct display_block *db;

      /* If the lines cursor parameter is not -1 then it indicates
         which rune in the TEXT block contains the cursor.  This means
         that there must be at least one display block.  The TEXT
         block, if present, must always be the first display block. */
      assert (Dynarr_length (ddba) != 0);

      db = Dynarr_atp (ddba, 0);
      assert (db->type == TEXT);

      get_cursor_size_and_location (w, db, ddl->cursor_elt, &cursor_start,
				    &cursor_width, &cursor_height);
    }
  else
    {
      cursor_start = cursor_width = cursor_height = 0;
    }

  /* The modeline should only have a single block and it had better be
     a TEXT block. */
  if (ddl->modeline)
    {
      /* The shadow thickness check is necessary if only the sign of
         the size changed. */
      if (cdba && !w->shadow_thickness_changed)
	{
	  must_sync |= compare_display_blocks (w, cdl, ddl, 0, 0,
					       start_pixpos, 0, 0, 0);
	}
      else
	{
	  redisplay_output_display_block (w, ddl, 0, 0, -1, start_pixpos,
					  0, 0, 0);
	  must_sync = 1;
	}

      if (must_sync)
	clear_border = 1;
    }

  while (!ddl->modeline && start_pixpos < end_pixpos)
    {
      int block;
      int next_start_pixpos;

      block = get_next_display_block (ddl->bounds, ddba, start_pixpos,
				      &next_start_pixpos);

      /* If we didn't find a block then we should blank the area
         between start_pos and next_start if necessary. */
      if (block == NO_BLOCK)
	{
	  /* We only erase those areas which were actually previously
             covered by a display block unless the window structure
             changed.  In that case we clear all areas since the current
             structures may actually represent a different buffer. */
	  while (start_pixpos < next_start_pixpos)
	    {
	      int block_end;
	      int old_block;

	      if (cdba)
		old_block = get_next_display_block (ddl->bounds, cdba,
						    start_pixpos, &block_end);
	      else
		{
		  old_block = NO_BLOCK;
		  block_end = next_start_pixpos;
		}

	      if (!cdba || old_block != NO_BLOCK || b != old_b ||
		  f->windows_structure_changed ||
		  f->faces_changed ||
		  force ||
		  (cdl && (cdl->ypos != ddl->ypos ||
			   cdl->ascent != ddl->ascent ||
			   cdl->descent != ddl->descent ||
			   cdl->top_clip != ddl->top_clip ||
			   cdl->clip != ddl->clip)))
		{
		  int x, y, width, height;
		  face_index findex;

		  must_sync = 1;
		  x = start_pixpos;
		  y = DISPLAY_LINE_YPOS (ddl);
		  width = min (next_start_pixpos, block_end) - x;
		  height = DISPLAY_LINE_HEIGHT (ddl);

		  if (x < ddl->bounds.left_in)
		    {
		      findex = ddl->left_margin_findex ?
			ddl->left_margin_findex
			: get_builtin_face_cache_index (w, Vleft_margin_face);
		    }
		  else if (x < ddl->bounds.right_in)
		    {
		      /* no check here because DEFAULT_INDEX == 0 anyway */
		      findex = ddl->default_findex;
		    }
		  else if (x < ddl->bounds.right_out)
		    {
		      findex = ddl->right_margin_findex ?
			ddl->right_margin_findex
			: get_builtin_face_cache_index (w, Vright_margin_face);
		    }
		  else
		    findex = (face_index) -1;

		  if (findex != (face_index) -1)
		    {
		      Lisp_Object window;

		      XSETWINDOW (window, w);

		      /* Clear the empty area. */
		      redisplay_clear_region (window, findex, x, y, width, height);

		      /* Mark that we should clear the border.  This is
			 necessary because italic fonts may leave
			 droppings in the border. */
		      clear_border = 1;
		    }
		}

	      start_pixpos = min (next_start_pixpos, block_end);
	    }
	}
      else
	{
	  struct display_block *cdb, *ddb;
	  int block_end;
	  int old_block;

	  if (cdba)
	    old_block = get_next_display_block (ddl->bounds, cdba,
						start_pixpos, &block_end);
	  else
	    old_block = NO_BLOCK;

	  ddb = Dynarr_atp (ddba, block);
	  cdb = (old_block != NO_BLOCK ? Dynarr_atp (cdba, old_block) : 0);

	  /* If there was formerly no block over the current
	     region or if it was a block of a different type, then
	     output the entire ddb.  Otherwise, compare cdb and
	     ddb and output only the changed region. */
	  if (!force && cdb && ddb->type == cdb->type
	      /* If there was no buffer being display before the
                 compare anyway as we might be outputting a gutter. */
	      &&
	      (b == old_b || !old_b))
	    {
	      must_sync |= compare_display_blocks (w, cdl, ddl, old_block,
						   block, start_pixpos,
						   cursor_start, cursor_width,
						   cursor_height);
	    }
	  else
	    {
	      int elt;
	      int first_elt = 0;
	      int last_elt = -1;

	      for (elt = 0; elt < Dynarr_length (ddb->runes); elt++)
		{
		  struct rune *rb = Dynarr_atp (ddb->runes, elt);

		  if (start_pixpos >= rb->xpos
		      && start_pixpos < rb->xpos + rb->width)
		    first_elt = elt;

		  if (end_pixpos > rb->xpos
		      && end_pixpos <= rb->xpos + rb->width)
		    {
		      last_elt = elt + 1;
		      if (last_elt > Dynarr_length (ddb->runes))
			last_elt = Dynarr_length (ddb->runes);
		      break;
		    }
		}

	      must_sync = 1;
	      redisplay_output_display_block (w, ddl, block, first_elt,
					      last_elt,
					      start_pixpos,
					      cursor_start, cursor_width,
					      cursor_height);
	    }

	  start_pixpos = next_start_pixpos;
	}
    }

  /* Clear the internal border if we are next to it and the window
     structure or frame size has changed or if something caused
     clear_border to be tripped.  */
  /* #### Doing this on f->clear sucks but is necessary because of
     window-local background values. */
  if (f->windows_structure_changed || f->faces_changed || clear_border
      || f->clear)
    {
      int y = DISPLAY_LINE_YPOS (ddl);
      int height = DISPLAY_LINE_HEIGHT (ddl);

      /* If we are in the gutter then we musn't clear the borders. */
      if (y >= WINDOW_TEXT_TOP (w) && (y + height) <= WINDOW_TEXT_BOTTOM (w))
	{
	  if (ddl->modeline)
	    {
	      y -= MODELINE_SHADOW_THICKNESS (w);
	      height += (2 * MODELINE_SHADOW_THICKNESS (w));
	    }

	  if (window_is_leftmost (w))
	    clear_left_border (w, y, height);
	  if (window_is_rightmost (w))
	    clear_right_border (w, y, height);
	}
    }

  if (cdla)
    sync_display_line_structs (w, line, must_sync, cdla, ddla);
}

/*****************************************************************************
 redisplay_move_cursor

 For the given window W, move the cursor to NEW_POINT.  Returns a
 boolean indicating success or failure.
 ****************************************************************************/

#define ADJ_BUFPOS (rb->bufpos + dl->offset)
#define ADJ_ENDPOS (rb->endpos + dl->offset)

int
redisplay_move_cursor (struct window *w, Bufpos new_point, int no_output_end)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);

  display_line_dynarr *cla = window_display_lines (w, CURRENT_DISP);
  struct display_line *dl;
  struct display_block *db;
  struct rune *rb;
  int x = w->last_point_x[CURRENT_DISP];
  int y = w->last_point_y[CURRENT_DISP];

  /*
   * Bail if cursor_in_echo_area is non-zero and we're fiddling with
   * the cursor in a non-active minibuffer window, since that is a
   * special case that is handled elsewhere and this function need
   * not handle it.  Return 1 so the caller will assume we
   * succeeded.
   */
  if (cursor_in_echo_area && MINI_WINDOW_P (w) &&
      w != XWINDOW (FRAME_SELECTED_WINDOW (f)))
    return 1;

  if (y < 0 || y >= Dynarr_length (cla))
    return 0;

  dl = Dynarr_atp (cla, y);
  db = get_display_block_from_line (dl, TEXT);

  if (x < 0 || x >= Dynarr_length (db->runes))
    return 0;

  rb = Dynarr_atp (db->runes, x);

  if (rb->cursor_type == CURSOR_OFF)
    return 0;
  else if (ADJ_BUFPOS == new_point
	   || (ADJ_ENDPOS && (new_point >= ADJ_BUFPOS)
	       && (new_point <= ADJ_ENDPOS)))
    {
      w->last_point_x[CURRENT_DISP] = x;
      w->last_point_y[CURRENT_DISP] = y;
      Fset_marker (w->last_point[CURRENT_DISP], make_int (ADJ_BUFPOS),
		   w->buffer);
      dl->cursor_elt = x;
      return 1;
    }
  else
    {
      {
	MAYBE_DEVMETH (d, frame_output_begin, (f));
	MAYBE_DEVMETH (d, window_output_begin, (w));
      }
      rb->cursor_type = CURSOR_OFF;
      dl->cursor_elt = -1;
      output_display_line (w, 0, cla, y, rb->xpos, rb->xpos + rb->width);
    }

  w->last_point_x[CURRENT_DISP] = -1;
  w->last_point_y[CURRENT_DISP] = -1;
  Fset_marker (w->last_point[CURRENT_DISP], Qnil, w->buffer);

  /* If this isn't the selected frame, then erasing the old cursor is
     all we actually had to do. */
  if (w != XWINDOW (FRAME_SELECTED_WINDOW (device_selected_frame (d))))
    {
      if (!no_output_end)
	{
	  MAYBE_DEVMETH (d, window_output_end, (w));
	  MAYBE_DEVMETH (d, frame_output_end, (f));
	}

      return 1;
    }

  /* This should only occur in the minibuffer. */
  if (new_point == 0)
    {
      w->last_point_x[CURRENT_DISP] = 0;
      w->last_point_y[CURRENT_DISP] = y;
      Fset_marker (w->last_point[CURRENT_DISP], Qzero, w->buffer);

      rb = Dynarr_atp (db->runes, 0);
      rb->cursor_type = CURSOR_ON;
      dl->cursor_elt = 0;

      output_display_line (w, 0, cla, y, rb->xpos, rb->xpos + rb->width);

      if (!no_output_end)
	{
	  MAYBE_DEVMETH (d, window_output_end, (w));
	  MAYBE_DEVMETH (d, frame_output_end, (f));
	}
      return 1;
    }
  else
    {
      int cur_rb = 0;
      int first = 0;
      int cur_dl, up;

      if (ADJ_BUFPOS < new_point)
	{
	  up = 1;
	  cur_rb = x + 1;
	  cur_dl = y;
	}
      else /* (rb->bufpos + dl->offset) > new_point */
	{
	  up = 0;

	  if (!x)
	    {
	      cur_dl = y - 1;
	      first = 0;
	    }
	  else
	    {
	      cur_rb = x - 1;
	      cur_dl = y;
	      first = 1;
	    }
	}

      while (up ? (cur_dl < Dynarr_length (cla)) : (cur_dl >= 0))
	{
	  dl = Dynarr_atp (cla, cur_dl);
	  db = get_display_block_from_line (dl, TEXT);

	  if (!up && !first)
	    cur_rb = Dynarr_length (db->runes) - 1;

	  while ((!scroll_on_clipped_lines || !dl->clip) &&
		 (up ? (cur_rb < Dynarr_length (db->runes)) : (cur_rb >= 0)))
	    {
	      rb = Dynarr_atp (db->runes, cur_rb);

	      if (rb->cursor_type != IGNORE_CURSOR
		  && rb->cursor_type != NO_CURSOR &&
		  (ADJ_BUFPOS == new_point
		   || (ADJ_ENDPOS && (new_point >= ADJ_BUFPOS)
		       && (new_point <= ADJ_BUFPOS))))
		{
		  rb->cursor_type = CURSOR_ON;
		  dl->cursor_elt = cur_rb;


		  output_display_line (w, 0, cla, cur_dl, rb->xpos,
				       rb->xpos + rb->width);

		  w->last_point_x[CURRENT_DISP] = cur_rb;
		  w->last_point_y[CURRENT_DISP] = cur_dl;
		  Fset_marker (w->last_point[CURRENT_DISP],
			       make_int (ADJ_BUFPOS), w->buffer);

		  if (!no_output_end)
		    {
		      MAYBE_DEVMETH (d, window_output_end, (w));
		      MAYBE_DEVMETH (d, frame_output_end, (f));
		    }
		  return 1;
		}

	      (up ? cur_rb++ : cur_rb--);
	    }

	  (up ? (cur_rb = 0) : (first = 0));
	  (up ? cur_dl++ : cur_dl--);
	}
    }

  if (!no_output_end)
    {
      MAYBE_DEVMETH (d, window_output_end, (w));
      MAYBE_DEVMETH (d, frame_output_end, (f));
    }
  return 0;
}
#undef ADJ_BUFPOS
#undef ADJ_ENDPOS

/*****************************************************************************
 redraw_cursor_in_window

 For the given window W, redraw the cursor if it is contained within
 the window.
 ****************************************************************************/
static void
redraw_cursor_in_window (struct window *w, int run_end_begin_meths)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);

  display_line_dynarr *dla = window_display_lines (w, CURRENT_DISP);
  struct display_line *dl;
  struct display_block *db;
  struct rune *rb;

  int x = w->last_point_x[CURRENT_DISP];
  int y = w->last_point_y[CURRENT_DISP];

  if (cursor_in_echo_area && MINI_WINDOW_P (w) &&
      !echo_area_active (f) && minibuf_level == 0)
    {
      MAYBE_DEVMETH (d, set_final_cursor_coords, (f, w->pixel_top, 0));
    }

  if (y < 0 || y >= Dynarr_length (dla))
    return;

  if (MINI_WINDOW_P (w) && f != device_selected_frame (d) &&
      !is_surrogate_for_selected_frame (f))
    return;

  dl = Dynarr_atp (dla, y);
  db = get_display_block_from_line (dl, TEXT);

  if (x < 0 || x >= Dynarr_length (db->runes))
    return;

  rb = Dynarr_atp (db->runes, x);

  /* Don't call the output routine if the block isn't actually the
     cursor. */
  if (rb->cursor_type == CURSOR_ON)
    {
      MAYBE_DEVMETH (d, set_final_cursor_coords,
		     (f, dl->ypos - 1, rb->xpos));

      if (run_end_begin_meths)
	{
	  MAYBE_DEVMETH (d, frame_output_begin, (f));
	  MAYBE_DEVMETH (d, window_output_begin, (w));
	}

      output_display_line (w, 0, dla, y, rb->xpos, rb->xpos + rb->width);

      if (run_end_begin_meths)
	{
	  MAYBE_DEVMETH (d, window_output_end, (w));
	  MAYBE_DEVMETH (d, frame_output_end, (f));
	}
    }
}

/*****************************************************************************
 redisplay_redraw_cursor

 For the given frame F, redraw the cursor on the selected window.
 This is used to update the cursor after focus changes.
 ****************************************************************************/
void
redisplay_redraw_cursor (struct frame *f, int run_end_begin_meths)
{
  Lisp_Object window;

  if (!cursor_in_echo_area)
    window = FRAME_SELECTED_WINDOW (f);
  else if (FRAME_HAS_MINIBUF_P (f))
    window = FRAME_MINIBUF_WINDOW (f);
  else
    return;

  redraw_cursor_in_window (XWINDOW (window), run_end_begin_meths);
}

/****************************************************************************
 redisplay_output_display_block

 Given a display line, a block number for that start line, output all
 runes between start and end in the specified display block.
 ****************************************************************************/
static void
redisplay_output_display_block (struct window *w, struct display_line *dl, int block,
				int start, int end, int start_pixpos, int cursor_start,
				int cursor_width, int cursor_height)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  /* Temporarily disabled until generalization is done. */
#if 0
  struct display_block *db = Dynarr_atp (dl->display_blocks, block);
  rune_dynarr *rba = db->runes;
  struct rune *rb;
  int xpos, width;
  rb = Dynarr_atp (rba, start);

  if (!rb)
      /* Nothing to do so don't do anything. */
      return;

  xpos = max (start_pixpos, rb->xpos);

  if (end < 0)
    end = Dynarr_length (rba);

  rb  = Dynarr_atp (rba, end - 1);
  width = rb->xpos + rb->width - xpos;
#endif
  /* now actually output the block. */
  DEVMETH (d, output_display_block, (w, dl, block, start,
				     end, start_pixpos,
				     cursor_start, cursor_width,
				     cursor_height));
}

/****************************************************************************
 redisplay_unmap_subwindows

 Remove subwindows from the area in the box defined by the given
 parameters.
 ****************************************************************************/
static void
redisplay_unmap_subwindows (struct frame* f, int x, int y, int width, int height,
			    Lisp_Object ignored_window)
{
  Lisp_Object rest;

  LIST_LOOP (rest, XWEAK_LIST_LIST (FRAME_SUBWINDOW_CACHE (f)))
    {
      Lisp_Image_Instance *ii = XIMAGE_INSTANCE (XCAR (rest));
      if (IMAGE_INSTANCE_SUBWINDOW_DISPLAYEDP (ii)
	  &&
	  IMAGE_INSTANCE_DISPLAY_X (ii)
	  + IMAGE_INSTANCE_DISPLAY_WIDTH (ii) > (unsigned) x
	  &&
	  IMAGE_INSTANCE_DISPLAY_X (ii) < (unsigned) (x + width)
	  &&
	  IMAGE_INSTANCE_DISPLAY_Y (ii)
	  + IMAGE_INSTANCE_DISPLAY_HEIGHT (ii) > (unsigned) y
	  &&
	  IMAGE_INSTANCE_DISPLAY_Y (ii) < (unsigned) (y + height)
	  &&
	  !EQ (XCAR (rest), ignored_window))
	{
	  unmap_subwindow (XCAR (rest));
	}
    }
}

/****************************************************************************
 redisplay_unmap_subwindows_maybe

 Potentially subwindows from the area in the box defined by the given
 parameters.
 ****************************************************************************/
void redisplay_unmap_subwindows_maybe (struct frame* f, int x, int y, int width, int height)
{
  if (!NILP (XWEAK_LIST_LIST (FRAME_SUBWINDOW_CACHE (f))))
    {
      redisplay_unmap_subwindows (f, x, y, width, height, Qnil);
    }
}

static void redisplay_unmap_subwindows_except_us (struct frame* f, int x, int y, int width,
						  int height, Lisp_Object subwindow)
{
  if (!NILP (XWEAK_LIST_LIST (FRAME_SUBWINDOW_CACHE (f))))
    {
      redisplay_unmap_subwindows (f, x, y, width, height, subwindow);
    }
}

/****************************************************************************
 redisplay_output_subwindow

 output a subwindow.  This code borrows heavily from the pixmap stuff,
 although is much simpler not needing to account for partial
 pixmaps, backgrounds etc.
 ****************************************************************************/
void
redisplay_output_subwindow (struct window *w,
			    Lisp_Object image_instance,
			    struct display_box* db, struct display_glyph_area* dga,
			    face_index findex, int cursor_start, int cursor_width,
			    int cursor_height)
{
  Lisp_Image_Instance *p = XIMAGE_INSTANCE (image_instance);
  Lisp_Object window;
  struct display_glyph_area sdga;

  dga->height = IMAGE_INSTANCE_HEIGHT (p);
  dga->width = IMAGE_INSTANCE_WIDTH (p);

  /* The first thing we are going to do is update the display
     characteristics of the subwindow. This also clears the dirty
     flags as a side effect. */
  redisplay_subwindow (image_instance);

  /* This makes the glyph area fit into the display area. */
  if (!redisplay_normalize_glyph_area (db, dga))
    return;

  XSETWINDOW (window, w);

  /* Clear the area the subwindow is going into. */
  redisplay_clear_clipped_region (window, findex,
				  db, dga, 0, image_instance);

  /* This shrinks the display box to exactly enclose the glyph
     area. */
  redisplay_normalize_display_box (db, dga);

  /* if we can't view the whole window we can't view any of it. We
     have to be careful here since we may be being asked to display
     part of a subwindow, the rest of which is on-screen as well. We
     need to allow this case and map the entire subwindow. We also
     need to be careful since the subwindow could be outside the
     window in the gutter or modeline - we also need to allow these
     cases.*/
  sdga.xoffset = -dga->xoffset;
  sdga.yoffset = -dga->yoffset;
  sdga.height = IMAGE_INSTANCE_HEIGHT (p);
  sdga.width = IMAGE_INSTANCE_WIDTH (p);

  if (redisplay_display_boxes_in_window_p (w, db, &sdga) == 0
      ||
      /* We only want to do full subwindow display for windows that
	 are completely in the gutter, otherwise we must clip to be
	 safe. */
      display_boxes_in_gutter_p (XFRAME (w->frame), db, &sdga) <= 0)
    {
      map_subwindow (image_instance, db->xpos, db->ypos, dga);
    }
  else
    {
      sdga.xoffset = sdga.yoffset = 0;
      map_subwindow (image_instance, db->xpos - dga->xoffset,
		     db->ypos - dga->yoffset, &sdga);
    }
}

/****************************************************************************
 redisplay_output_layout

 Output a widget hierarchy. This can safely call itself recursively.

 The complexity of outputting layouts is deciding whether to do it or
 not. Consider a layout enclosing some text, the text changes and is
 marked as dirty, but the enclosing layout has not been marked as
 dirty so no updates occur and the text will potentially be truncated.
 Alternatively we hold a back pointer in the image instance to the
 parent and mark the parent as dirty. But the layout code assumes that
 if the layout is dirty then the whole layout should be redisplayed,
 so we then get lots of flashing even though only the text has changed
 size. Of course if the text shrinks in size then we do actually need
 to redisplay the layout to repaint the exposed area. So what happens
 if we make a non-structural change like changing color? Either we
 redisplay everything, or we redisplay nothing. These are exactly the
 issues lwlib has to grapple with. We really need to know what has
 actually changed and make a layout decision based on that. We also
 really need to know what has changed so that we can only make the
 necessary changes in update_subwindow.  This has all now been
 implemented, Viva la revolution!
 ****************************************************************************/
void
redisplay_output_layout (Lisp_Object domain,
			 Lisp_Object image_instance,
			 struct display_box* db, struct display_glyph_area* dga,
			 face_index findex, int cursor_start, int cursor_width,
			 int cursor_height)
{
  Lisp_Image_Instance *p = XIMAGE_INSTANCE (image_instance);
  Lisp_Object rest, window = DOMAIN_WINDOW (domain);
  Emchar_dynarr *buf = Dynarr_new (Emchar);
  struct window *w = XWINDOW (window);
  struct device *d = DOMAIN_XDEVICE (domain);
  int layout_height, layout_width;

  layout_height = glyph_height (image_instance, domain);
  layout_width = glyph_width (image_instance, domain);

  dga->height = layout_height;
  dga->width = layout_width;
#ifdef DEBUG_WIDGET_OUTPUT
  printf ("outputing layout glyph %p\n", p);
#endif
  /* This makes the glyph area fit into the display area. */
  if (!redisplay_normalize_glyph_area (db, dga))
    return;

  /* Highly dodgy optimization. We want to only output the whole
     layout if we really have to. */
  if (!IMAGE_INSTANCE_OPTIMIZE_OUTPUT (p)
      || IMAGE_INSTANCE_LAYOUT_CHANGED (p)
      || IMAGE_INSTANCE_WIDGET_FACE_CHANGED (p)
      || IMAGE_INSTANCE_SIZE_CHANGED (p)
      || IMAGE_INSTANCE_WIDGET_ITEMS_CHANGED (p))
    {
      /* First clear the area we are drawing into. This is the easiest
	 thing to do since we have many gaps that we have to make sure are
	 filled in. */
      redisplay_clear_clipped_region (window, findex, db, dga, 1, Qnil);

      /* Output a border if required */
      if (!NILP (IMAGE_INSTANCE_LAYOUT_BORDER (p)))
	{
	  int edges = 0;
	  enum edge_style style;
	  int ypos = db->ypos;
	  int xpos = db->xpos;
	  int height = dga->height;
	  int width = dga->width;

	  /* The bevel_area routines always draw in from the specified
	     area so there is no need to adjust the displayed area to
	     make sure that the lines are visible. */
	  if (dga->xoffset >= 0) 
	    edges |= EDGE_LEFT;
	  if (dga->width - dga->xoffset == layout_width) 
	    edges |= EDGE_RIGHT;
	  if (dga->yoffset >= 0) 
	    edges |= EDGE_TOP;
	  if (dga->height - dga->yoffset == layout_height)
	    edges |= EDGE_BOTTOM;
	  
	  if (EQ (IMAGE_INSTANCE_LAYOUT_BORDER (p), Qetched_in))
	    style = EDGE_ETCHED_IN;
	  else if (EQ (IMAGE_INSTANCE_LAYOUT_BORDER (p), Qetched_out))
	    style = EDGE_ETCHED_OUT;
	  else if (EQ (IMAGE_INSTANCE_LAYOUT_BORDER (p), Qbevel_in))
	    style = EDGE_BEVEL_IN;
	  else if (INTP (IMAGE_INSTANCE_LAYOUT_BORDER (p)))
	    {
	      style = EDGE_ETCHED_IN;
	      if (edges & EDGE_TOP)
		{
		  ypos += XINT (IMAGE_INSTANCE_LAYOUT_BORDER (p));
		  height -= XINT (IMAGE_INSTANCE_LAYOUT_BORDER (p));
		}
	    }
	  else
	    style = EDGE_BEVEL_OUT;

	  MAYBE_DEVMETH (d, bevel_area,
			 (w, findex, xpos, ypos, width, height,
			  DEFAULT_WIDGET_SHADOW_WIDTH, edges, style));
	}
    }

  /* This shrinks the display box to exactly enclose the glyph
     area. */
  redisplay_normalize_display_box (db, dga);

  /* Flip through the widgets in the layout displaying as necessary */
  LIST_LOOP (rest, IMAGE_INSTANCE_LAYOUT_CHILDREN (p))
    {
      Lisp_Object child = glyph_image_instance (XCAR (rest), image_instance,
						ERROR_ME_NOT, 1);

      struct display_box cdb;
      /* For losing HP-UX */
      cdb.xpos = db->xpos;
      cdb.ypos = db->ypos;
      cdb.width = db->width;
      cdb.height = db->height;

      /* First determine if the image is visible at all */
      if (IMAGE_INSTANCEP (child))
	{
	  Lisp_Image_Instance* childii = XIMAGE_INSTANCE (child);

	  /* The enclosing layout offsets are +ve at this point */
	  struct display_glyph_area cdga;
	  cdga.xoffset  = IMAGE_INSTANCE_XOFFSET (childii) - dga->xoffset;
	  cdga.yoffset = IMAGE_INSTANCE_YOFFSET (childii) - dga->yoffset;
	  cdga.width = glyph_width (child, image_instance);
	  cdga.height = glyph_height (child, image_instance);

	  IMAGE_INSTANCE_OPTIMIZE_OUTPUT (childii) =
	    IMAGE_INSTANCE_OPTIMIZE_OUTPUT (p);

	  /* Although normalization is done by the output routines
	     we have to do it here so that they don't try and
	     clear all of db. This is true below also. */
	  if (redisplay_normalize_glyph_area (&cdb, &cdga))
	    {
	      redisplay_normalize_display_box (&cdb, &cdga);
	      /* Since the display boxes will now be totally in the
		 window if they are visible at all we can now check this easily. */
	      if (cdb.xpos < db->xpos || cdb.ypos < db->ypos
		  || cdb.xpos + cdb.width > db->xpos + db->width
		  || cdb.ypos + cdb.height > db->ypos + db->height)
		continue;
	      /* We have to invert the offset here as normalization
		 will have made them positive which the output
		 routines will treat as a truly +ve offset. */
	      cdga.xoffset = -cdga.xoffset;
	      cdga.yoffset = -cdga.yoffset;

	      switch (IMAGE_INSTANCE_TYPE (childii))
		{
		case IMAGE_TEXT:
		  {
		    /* #### This is well hacked and could use some
		       generalisation.*/
		    if (redisplay_normalize_glyph_area (&cdb, &cdga)
			&&
			(!IMAGE_INSTANCE_OPTIMIZE_OUTPUT (childii) ||
			 IMAGE_INSTANCE_DIRTYP (childii)))
		      {
			struct display_line dl;	/* this is fake */
			Lisp_Object string =
			  IMAGE_INSTANCE_TEXT_STRING (childii);
			unsigned char charsets[NUM_LEADING_BYTES];
			struct face_cachel *cachel = WINDOW_FACE_CACHEL (w, findex);

			find_charsets_in_bufbyte_string (charsets,
							 XSTRING_DATA (string),
							 XSTRING_LENGTH (string));
			ensure_face_cachel_complete (cachel, window, charsets);

			convert_bufbyte_string_into_emchar_dynarr
			  (XSTRING_DATA (string), XSTRING_LENGTH (string), buf);

			redisplay_normalize_display_box (&cdb, &cdga);
			/* Offsets are now +ve again so be careful
			   when fixing up the display line. */
			xzero (dl);
			/* Munge boxes into display lines. */
			dl.ypos = (cdb.ypos - cdga.yoffset)
			  + glyph_ascent (child, image_instance);
			dl.ascent = glyph_ascent (child, image_instance);
			dl.descent = glyph_descent (child, image_instance);
			dl.top_clip = cdga.yoffset;
			dl.clip = (dl.ypos + dl.descent) - (cdb.ypos + cdb.height);
			/* output_string doesn't understand offsets in
			   the same way as other routines - we have to
			   add the offset to the width so that we
			   output the full string. */
			MAYBE_DEVMETH (d, output_string, (w, &dl, buf, cdb.xpos,
							  cdga.xoffset, cdb.xpos,
							  cdga.width + cdga.xoffset,
							  findex, 0, 0, 0, 0));
			Dynarr_reset (buf);
		      }
		  }
		  break;

		case IMAGE_MONO_PIXMAP:
		case IMAGE_COLOR_PIXMAP:
		  if (!IMAGE_INSTANCE_OPTIMIZE_OUTPUT (childii)
		      || IMAGE_INSTANCE_DIRTYP (childii))
		    redisplay_output_pixmap (w, child, &cdb, &cdga, findex,
					     0, 0, 0, 0);
		  break;

		case IMAGE_WIDGET:
		  if (EQ (IMAGE_INSTANCE_WIDGET_TYPE (childii), Qlayout))
		    {
		      redisplay_output_layout (image_instance, child, &cdb, &cdga, findex,
					       0, 0, 0);
		      break;
		    }
		case IMAGE_SUBWINDOW:
		  if (!IMAGE_INSTANCE_OPTIMIZE_OUTPUT (childii) ||
		      IMAGE_INSTANCE_DIRTYP (childii))
		    redisplay_output_subwindow (w, child, &cdb, &cdga, findex,
						0, 0, 0);
		  break;

		case IMAGE_NOTHING:
		  /* nothing is as nothing does */
		  break;

		case IMAGE_POINTER:
		default:
		  abort ();
		}
	    }
	  IMAGE_INSTANCE_OPTIMIZE_OUTPUT (childii) = 0;
	}
    }

  /* Update any display properties. I'm not sure whether this actually
     does anything for layouts except clear the changed flags. */
  redisplay_subwindow (image_instance);

  Dynarr_free (buf);
}

/****************************************************************************
 redisplay_output_pixmap


 output a pixmap.
 ****************************************************************************/
void
redisplay_output_pixmap (struct window *w,
			 Lisp_Object image_instance,
			 struct display_box* db, struct display_glyph_area* dga,
			 face_index findex, int cursor_start, int cursor_width,
			 int cursor_height, int offset_bitmap)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  Lisp_Image_Instance *p = XIMAGE_INSTANCE (image_instance);
  Lisp_Object window;
  XSETWINDOW (window, w);

  dga->height = IMAGE_INSTANCE_PIXMAP_HEIGHT (p);
  dga->width = IMAGE_INSTANCE_PIXMAP_WIDTH (p);

#ifdef DEBUG_REDISPLAY
  printf ("redisplay_output_pixmap(request) \
[%dx%d@%d+%d] in [%dx%d@%d+%d]\n", 
	  db->width, db->height, db->xpos, db->ypos,
	  dga->width, dga->height, dga->xoffset, dga->yoffset);
#endif

  /* This makes the glyph area fit into the display area. */
  if (!redisplay_normalize_glyph_area (db, dga))
    return;

#ifdef DEBUG_REDISPLAY
  printf ("redisplay_output_pixmap(normalized) \
[%dx%d@%d+%d] in [%dx%d@%d+%d]\n",
	  db->width, db->height, db->xpos, db->ypos,
	  dga->width, dga->height, dga->xoffset, dga->yoffset);
#endif

  /* Clear the area the pixmap is going into.  The pixmap itself will
     always take care of the full width.  We don't want to clear where
     it is going to go in order to avoid flicker.  So, all we have to
     take care of is any area above or below the pixmap. If the pixmap
     has a mask in which case we have to clear the whole damn thing
     since we can't yet clear just the area not included in the
     mask. */
  if (!offset_bitmap)
    {
      redisplay_clear_clipped_region (window, findex,
				      db, dga,
				      (IMAGE_INSTANCE_PIXMAP_MASK (p) != 0),
				      Qnil);

      /* This shrinks the display box to exactly enclose the glyph
	 area. */
      redisplay_normalize_display_box (db, dga);
    }
  assert (db->xpos >= 0 && db->ypos >= 0);

  MAYBE_DEVMETH (d, output_pixmap, (w, image_instance,
				    db, dga,
				    findex, cursor_start,
				    cursor_width, cursor_height,
				    offset_bitmap));
}

/****************************************************************************
 redisplay_clear_region

 Clear the area in the box defined by the given parameters using the
 given face. This has been generalised so that subwindows can be
 coped with effectively.
 ****************************************************************************/
void
redisplay_clear_region (Lisp_Object locale, face_index findex, int x, int y,
			int width, int height)
{
  struct window *w = NULL;
  struct frame *f = NULL;
  struct device *d;
  Lisp_Object background_pixmap = Qunbound;
  Lisp_Object fcolor = Qnil, bcolor = Qnil;

  if (!width || !height)
     return;

  if (WINDOWP (locale))
    {
      w = XWINDOW (locale);
      f = XFRAME (w->frame);
    }
  else if (FRAMEP (locale))
    {
      w = NULL;
      f = XFRAME (locale);
    }
  else
    abort ();

  d = XDEVICE (f->device);

  /* if we have subwindows in the region we have to unmap them */
  redisplay_unmap_subwindows_maybe (f, x, y, width, height);

  /* #### This isn't quite right for when this function is called
     from the toolbar code. */

  /* Don't use a backing pixmap in the border area */
  if (x >= FRAME_LEFT_BORDER_END (f)
      && x < FRAME_RIGHT_BORDER_START (f)
      && y >= FRAME_TOP_BORDER_END (f)
      && y < FRAME_BOTTOM_BORDER_START (f))
    {
      Lisp_Object temp;

      if (w)
	{
	  temp = WINDOW_FACE_CACHEL_BACKGROUND_PIXMAP (w, findex);

	  if (IMAGE_INSTANCEP (temp)
	      && IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (temp)))
	    {
	      /* #### maybe we could implement such that a string
		 can be a background pixmap? */
	      background_pixmap = temp;
	    }
	}
      else
	{
	  temp = FACE_BACKGROUND_PIXMAP (Vdefault_face, locale);

	  if (IMAGE_INSTANCEP (temp)
	      && IMAGE_INSTANCE_PIXMAP_TYPE_P (XIMAGE_INSTANCE (temp)))
	    {
	      background_pixmap = temp;
	    }
	}
    }

  if (!UNBOUNDP (background_pixmap) &&
      XIMAGE_INSTANCE_PIXMAP_DEPTH (background_pixmap) == 0)
    {
      if (w)
	{
	  fcolor = WINDOW_FACE_CACHEL_FOREGROUND (w, findex);
	  bcolor = WINDOW_FACE_CACHEL_BACKGROUND (w, findex);
	}
      else
	{
	  fcolor = FACE_FOREGROUND (Vdefault_face, locale);
	  bcolor = FACE_BACKGROUND (Vdefault_face, locale);
	}
    }
  else
    {
      fcolor = (w ?
		WINDOW_FACE_CACHEL_BACKGROUND (w, findex) :
		FACE_BACKGROUND (Vdefault_face, locale));

    }

  if (UNBOUNDP (background_pixmap))
    background_pixmap = Qnil;

  DEVMETH (d, clear_region,
	   (locale, d, f, findex, x, y, width, height, fcolor, bcolor, background_pixmap));
}

/****************************************************************************
 redisplay_clear_clipped_region

 Clear the area in the dest display_box not covered by the src
 display_glyph_area using the given face. This is a common occurrence
 for images shorter than the display line. Clipping can be played
 around with by altering these. glyphsrc should be normalized.
 ****************************************************************************/
static void
redisplay_clear_clipped_region (Lisp_Object window, face_index findex,
	struct display_box* dest, struct display_glyph_area* glyphsrc,
	int fullheight_p, Lisp_Object ignored_subwindow)
{
  /* assume dest->xpos >= 0 */
  int clear_x;
  struct frame* f = XFRAME (XWINDOW (window)->frame);

  if (glyphsrc->xoffset > 0)
    {
      clear_x = dest->xpos + glyphsrc->xoffset;
    }
  else
    {
      clear_x = dest->xpos;
    }

  /* If we need the whole height cleared then just do it. */
  if (fullheight_p)
    {
      redisplay_clear_region (window, findex, clear_x, dest->ypos,
			      glyphsrc->width, dest->height);
    }
  else
    {
      int yoffset = (glyphsrc->yoffset > 0 ? glyphsrc->yoffset : 0);

      /* We need to make sure that subwindows are unmapped from the
         whole area. */
      redisplay_unmap_subwindows_except_us (f, clear_x, dest->ypos,
					    glyphsrc->width, dest->height,
					    ignored_subwindow);
      /* first the top box */
      if (yoffset > 0)
	{
	  redisplay_clear_region (window, findex, clear_x, dest->ypos,
				  glyphsrc->width, yoffset);

	}
      /* Then the bottom box */
      if (yoffset + glyphsrc->height < dest->height)
	{
	  redisplay_clear_region (window, findex, clear_x,
				  dest->ypos + yoffset + glyphsrc->height,
				  glyphsrc->width,
				  dest->height - (yoffset + glyphsrc->height));

	}
    }
}

/*****************************************************************************
 redisplay_normalize_glyph_area
 redisplay_normalize_display_box

 Calculate the visible box for displaying glyphsrc in dest.

 display_box and display_glyph_area are used to represent an area to
 displayed and where to display it. Using these two structures all
 combinations of clipping and position can be accommodated.

 dest - display_box

	xpos - absolute horizontal position of area.

  	ypos - absolute vertical position of area.

  glyphsrc - display_glyph_area

	xoffset - horizontal offset of the glyph, +ve means display
	the glyph with the x position offset by xoffset, -ve means
	display starting xoffset into the glyph.

	yoffset - vertical offset of the glyph, +ve means display the
	glyph with y position offset by yoffset, -ve means display
	starting xoffset into the glyph.

 ****************************************************************************/
int
redisplay_normalize_glyph_area (struct display_box* dest,
				struct display_glyph_area* glyphsrc)
{
  if (dest->xpos + glyphsrc->xoffset > dest->xpos + dest->width
      ||
      dest->ypos + glyphsrc->yoffset > dest->ypos + dest->height
      ||
      -glyphsrc->xoffset >= glyphsrc->width
      ||
      -glyphsrc->yoffset >= glyphsrc->height
      ||
      /* #### Not sure why this wasn't coped with before but normalizing
	 to zero width or height is definitely wrong. */
      (dest->xpos + glyphsrc->xoffset + glyphsrc->width > dest->xpos + dest->width
       &&
       dest->width - glyphsrc->xoffset <= 0)
      ||
      (dest->ypos + glyphsrc->yoffset + glyphsrc->height > dest->ypos + dest->height
       &&
       dest->height - glyphsrc->yoffset <= 0))
    {
      /* It's all clipped out */
      return 0;
    }

  /* Horizontal offsets. This works because xoffset can be -ve as well
     as +ve.  When we enter this function the glyphsrc width and
     height are set to the actual glyph width and height irrespective
     of how much can be displayed. We are trying to clip both the
     offset into the image and the rightmost bounding box. Its
     possible for the glyph width to be much larger than the area we
     are displaying into (e.g. a large glyph in a small frame). */
  if (dest->xpos + glyphsrc->xoffset + glyphsrc->width > dest->xpos + dest->width)
    {
      /* glyphsrc offset is +ve we are trying to display offset from the
	 origin (the bounding box contains some space and then the
	 glyph). At most the width we want to display is dest->width -
	 glyphsrc->xoffset. */
      if (glyphsrc->xoffset > 0)
	glyphsrc->width = dest->width - glyphsrc->xoffset;
      /* glyphsrc offset is -ve we are trying to display hard up
	 against the dest corner inset into the glyphsrc by
	 xoffset.*/
      else if (glyphsrc->xoffset < 0) 
	{
	  glyphsrc->width += glyphsrc->xoffset;
	  glyphsrc->width = min (glyphsrc->width, dest->width);
	}
      else
	glyphsrc->width = dest->width;
    }

  else if (glyphsrc->xoffset < 0) 
    glyphsrc->width += glyphsrc->xoffset;

  /* Vertical offsets. This works because yoffset can be -ve as well as +ve */
  if (dest->ypos + glyphsrc->yoffset + glyphsrc->height > dest->ypos + dest->height)
    {
      if ((glyphsrc->yoffset > 0) && (dest->height > glyphsrc->yoffset))
	glyphsrc->height = dest->height - glyphsrc->yoffset;
      else if (glyphsrc->yoffset < 0) 
	{
	  glyphsrc->height += glyphsrc->yoffset;
	  glyphsrc->height = min (glyphsrc->height, dest->height);
	}
      else
	glyphsrc->height = dest->height;
    }

  else if (glyphsrc->yoffset < 0)
    glyphsrc->height += glyphsrc->yoffset;

  return 1;
}

static void
redisplay_normalize_display_box (struct display_box* dest,
				 struct display_glyph_area* glyphsrc)
{
  /* Adjust the destination area. At the end of this the destination
   area will exactly enclose the glyph area. The only remaining
   adjustment will be offsets into the glyph area. */

  /* Horizontal adjustment. */
  if (glyphsrc->xoffset > 0)
    {
      dest->xpos += glyphsrc->xoffset;
      dest->width -= glyphsrc->xoffset;
      glyphsrc->xoffset = 0;
    }
  else
    glyphsrc->xoffset = -glyphsrc->xoffset;

  if (glyphsrc->width < dest->width)
    dest->width = glyphsrc->width;

  /* Vertical adjustment. */
  if (glyphsrc->yoffset > 0)
    {
      dest->ypos += glyphsrc->yoffset;
      dest->height -= glyphsrc->yoffset;
      glyphsrc->yoffset = 0;
    }
  else
    glyphsrc->yoffset = -glyphsrc->yoffset;

  if (glyphsrc->height < dest->height)
    dest->height = glyphsrc->height;
}

/*****************************************************************************
 redisplay_display_boxes_in_window_p

 Determine whether the required display_glyph_area is completely inside
 the window. -1 means the display_box is not in the window. 1 means the
 display_box and the display_glyph_area are in the window. 0 means
 the display_box is in the window but the display_glyph_area is not.
 ****************************************************************************/
static int
redisplay_display_boxes_in_window_p (struct window* w,
				     struct display_box* db,
				     struct display_glyph_area* dga)
{
  int left = WINDOW_TEXT_LEFT (w);
  int right = WINDOW_TEXT_RIGHT (w);
  int top = WINDOW_TEXT_TOP (w);
  int bottom = WINDOW_TEXT_BOTTOM (w);

  if (db->xpos < left || db->ypos < top
      || db->xpos + db->width > right
      || db->ypos + db->height > bottom)
      /* We are not displaying in a window at all */
      return -1;

  if (db->xpos + dga->xoffset >= left
      &&
      db->ypos + dga->yoffset >= top
      &&
      db->xpos + dga->xoffset + dga->width <= right
      &&
      db->ypos + dga->yoffset + dga->height <= bottom)
    return 1;

  return 0;
}

/*****************************************************************************
 redisplay_calculate_display_boxes

 Convert from rune/display_line co-ordinates to display_box
 co-ordinates.
 ****************************************************************************/
int
redisplay_calculate_display_boxes (struct display_line *dl, int xpos,
				   int xoffset, int yoffset, int start_pixpos,
                                   int width, struct display_box* dest,
				   struct display_glyph_area* src)
{
  dest->xpos = xpos;
  dest->ypos = DISPLAY_LINE_YPOS (dl);
  dest->width = width;
  dest->height = DISPLAY_LINE_HEIGHT (dl);

  src->xoffset = -xoffset;
  src->width = 0;
  src->height = 0;

  src->yoffset = -dl->top_clip + yoffset;

  if (start_pixpos >=0 && start_pixpos > xpos)
    {
      /* Oops, we're asking for a start outside of the displayable
         area. */
      if (start_pixpos > xpos + width)
	return 0;
      dest->xpos = start_pixpos;
      dest->width -= (start_pixpos - xpos);
      /* Offsets are -ve when we want to clip pixels off the displayed
         glyph. */
      src->xoffset -= (start_pixpos - xpos);
    }

  return 1;
}

/*****************************************************************************
 redisplay_clear_top_of_window

 If window is topmost, clear the internal border above it.
 ****************************************************************************/
void
redisplay_clear_top_of_window (struct window *w)
{
  Lisp_Object window;
  XSETWINDOW (window, w);

  if (!NILP (Fwindow_highest_p (window)))
    {
      struct frame *f = XFRAME (w->frame);
      int x, y, width, height;

      x = w->pixel_left;
      width = w->pixel_width;

      if (window_is_leftmost (w))
	{
	  x -= FRAME_BORDER_WIDTH (f);
	  width += FRAME_BORDER_WIDTH (f);
	}
      if (window_is_rightmost (w))
	width += FRAME_BORDER_WIDTH (f);

      y = FRAME_TOP_BORDER_START (f) - 1;
      height = FRAME_BORDER_HEIGHT (f) + 1;

      redisplay_clear_region (window, DEFAULT_INDEX, x, y, width, height);
    }
}

/*****************************************************************************
 redisplay_clear_to_window_end

 Clear the area between ypos1 and ypos2.  Each margin area and the
 text area is handled separately since they may each have their own
 background color.
 ****************************************************************************/
void
redisplay_clear_to_window_end (struct window *w, int ypos1, int ypos2)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);

  if (HAS_DEVMETH_P (d, clear_to_window_end))
    DEVMETH (d, clear_to_window_end, (w, ypos1, ypos2));
  else
    {
      int height = ypos2 - ypos1;

      if (height)
	{
	  Lisp_Object window;
	  int bflag = 0 ; /* (window_needs_vertical_divider (w) ? 0 : 1);*/
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
	    redisplay_clear_region (window,
				    DEFAULT_INDEX,
				    bounds.left_in, ypos1,
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
}

/*****************************************************************************
 redisplay_clear_bottom_of_window

 Clear window from right below the last display line to right above
 the modeline.  The calling function can limit the area actually
 erased by setting min_start and/or max_end to positive values.
 ****************************************************************************/
void
redisplay_clear_bottom_of_window (struct window *w, display_line_dynarr *ddla,
				  int min_start, int max_end)
{
  struct frame *f = XFRAME (w->frame);
  int ypos1, ypos2;
  int ddla_len = Dynarr_length (ddla);

  ypos2 = WINDOW_TEXT_BOTTOM (w);
#ifdef HAVE_SCROLLBARS
  /* This adjustment is to catch the intersection of any scrollbars. */
  if (f->windows_structure_changed && NILP (w->scrollbar_on_top_p))
    ypos2 += window_scrollbar_height (w);
#endif

  if (ddla_len)
    {
      if (ddla_len == 1 && Dynarr_atp (ddla, 0)->modeline)
	{
	  ypos1 = WINDOW_TEXT_TOP (w);
#ifdef HAVE_SCROLLBARS
	  /* This adjustment is to catch the intersection of any scrollbars. */
	  if (f->windows_structure_changed && !NILP (w->scrollbar_on_top_p))
	    ypos1 -= window_scrollbar_height (w);
#endif
	}
      else
	{
	  struct display_line *dl = Dynarr_atp (ddla, ddla_len - 1);
	  ypos1 = dl->ypos + dl->descent - dl->clip;
	}
    }
  else
    ypos1 = WINDOW_TEXT_TOP (w);

  /* #### See if this can be made conditional on the frame
     changing size. */
  if (MINI_WINDOW_P (w))
    ypos2 += FRAME_BORDER_HEIGHT (f);

  if (min_start >= 0 && ypos1 < min_start)
    ypos1 = min_start;
  if (max_end >= 0 && ypos2 > max_end)
    ypos2 = max_end;

  if (ypos2 <= ypos1)
    return;

  redisplay_clear_to_window_end (w, ypos1, ypos2);
}

/*****************************************************************************
 redisplay_update_line

 This is used during incremental updates to update a single line and
 correct the offsets on all lines below it.  At the moment
 update_values is false if we are only updating the modeline.
 ****************************************************************************/
void
redisplay_update_line (struct window *w, int first_line, int last_line,
		       int update_values)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);

  display_line_dynarr *cdla = window_display_lines (w, CURRENT_DISP);
  display_line_dynarr *ddla = window_display_lines (w, DESIRED_DISP);

  MAYBE_DEVMETH (d, window_output_begin, (w));

  while (first_line <= last_line)
    {
      Charcount old_len = (Dynarr_atp (cdla, first_line)->end_bufpos -
			   Dynarr_atp (cdla, first_line)->bufpos);
      Charcount new_len = (Dynarr_atp (ddla, first_line)->end_bufpos -
			   Dynarr_atp (ddla, first_line)->bufpos);

      assert (Dynarr_length (cdla) == Dynarr_length (ddla));

      /* Output the changes. */
      output_display_line (w, cdla, ddla, first_line, -1, -1);

      /* Update the offsets. */
      if (update_values)
	{
	  int cur_line = first_line + 1;
	  while (cur_line < Dynarr_length (cdla))
	    {
	      Dynarr_atp (cdla, cur_line)->offset += (new_len - old_len);
	      Dynarr_atp (ddla, cur_line)->offset += (new_len - old_len);
	      cur_line++;
	    }
	}

      /* Update the window_end_pos and other settings. */
      if (update_values)
	{
	  w->window_end_pos[CURRENT_DISP] -= (new_len - old_len);

	  if (Dynarr_atp (ddla, first_line)->cursor_elt != -1)
	    {
	      w->last_point_x[CURRENT_DISP] = w->last_point_x[DESIRED_DISP];
	      w->last_point_y[CURRENT_DISP] = w->last_point_y[DESIRED_DISP];
	    }
	}

      first_line++;
    }

  /* Update the window max line length.  We have to scan the entire
     set of display lines otherwise we might not detect if the max is
     supposed to shrink. */
  if (update_values)
    {
      int line = 0;

      w->max_line_len = 0;
      while (line < Dynarr_length (ddla))
	{
	  struct display_line *dl = Dynarr_atp (ddla, line);

	  if (!dl->modeline)
	    w->max_line_len = max (dl->num_chars, w->max_line_len);

	  line++;
	}
    }

  w->last_modified[CURRENT_DISP] = w->last_modified[DESIRED_DISP];
  w->last_facechange[CURRENT_DISP] = w->last_facechange[DESIRED_DISP];
  Fset_marker (w->last_point[CURRENT_DISP],
	       Fmarker_position (w->last_point[DESIRED_DISP]), w->buffer);
  Fset_marker (w->last_start[CURRENT_DISP],
	       Fmarker_position (w->last_start[DESIRED_DISP]), w->buffer);

  /* We don't bother updating the vertical scrollbars here.  This
     gives us a performance increase while having minimal loss of
     quality to the scrollbar slider size and position since when this
     function is called we know that the changes to the buffer were
     very localized.  We have to update the horizontal scrollbars,
     though, because this routine could cause a change which has a
     larger impact on their sizing. */
  /* #### See if we can get away with only calling this if
     max_line_len is greater than the window_char_width. */
  /* #### BILL!!! Should we do this for GTK as well? */
#if defined(HAVE_SCROLLBARS) && defined(HAVE_X_WINDOWS)
  {
    extern int stupid_vertical_scrollbar_drag_hack;

    update_window_scrollbars (w, NULL, 1, stupid_vertical_scrollbar_drag_hack);
    stupid_vertical_scrollbar_drag_hack = 1;
  }
#endif

  redisplay_redraw_cursor (f, 0);
  MAYBE_DEVMETH (d, window_output_end, (w));
}

/*****************************************************************************
 redisplay_output_window

 For the given window W, ensure that the current display lines are
 equal to the desired display lines, outputing changes as necessary.

 #### Fuck me.  This just isn't going to cut it for tty's.  The output
 decisions for them must be based on the contents of the entire frame
 because that is how the available output capabilities think.  The
 solution is relatively simple.  Create redisplay_output_frame.  This
 will basically merge all of the separate window display structs into
 a single one for the frame.  This combination structure will be able
 to be passed to the same output_display_line which works for windows
 on X frames and the right things will happen.  It just takes time to
 do.
 ****************************************************************************/
void
redisplay_output_window (struct window *w)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);

  display_line_dynarr *cdla = window_display_lines (w, CURRENT_DISP);
  display_line_dynarr *ddla = window_display_lines (w, DESIRED_DISP);

  int cdla_len = Dynarr_length (cdla);
  int ddla_len = Dynarr_length (ddla);

  int line;
  int need_to_clear_bottom = 0;
  int need_to_clear_start = -1;
  int need_to_clear_end = -1;

  /* Backgrounds may have changed or windows may have gone away
     leaving dividers lying around. */
  if (f->faces_changed
      || f->windows_structure_changed
      || w->shadow_thickness_changed)
    need_to_clear_bottom = 1;

  /* The first thing we do is determine if we are going to need to
     clear the bottom of the window.  We only need to do this if the
     bottom of the current display lines is below the bottom of the
     desired display lines.  Note that the number of lines is
     irrelevant.  Only the position matters.  We also clear to the
     bottom of the window if the modeline has shifted position. */
  /* #### We can't blindly not clear the bottom if f->clear is true
     since there might be a window-local background.  However, for
     those cases where there isn't, clearing the end of the window in
     this case sucks. */
  if (!need_to_clear_bottom)
    {
      struct display_line *cdl, *ddl;

      /* If the modeline has changed position or size, clear the bottom
	 of the window. */
      if (!need_to_clear_bottom)
	{
	  cdl = ddl = 0;

	  if (cdla_len)
	    cdl = Dynarr_atp (cdla, 0);
	  if (ddla_len)
	    ddl = Dynarr_atp (ddla, 0);

	  if (!cdl || !ddl)
	    need_to_clear_bottom = 1;
	  else if ((!cdl->modeline && ddl->modeline)
		   || (cdl->modeline && !ddl->modeline))
	    need_to_clear_bottom = 1;
	  else if (cdl->ypos != ddl->ypos ||
		   cdl->ascent != ddl->ascent ||
		   cdl->descent != ddl->descent ||
		   cdl->clip != ddl->clip)
	    need_to_clear_bottom = 1;

	  /* #### This kludge is to make sure the modeline shadows get
	     redrawn if the modeline position shifts. */
	  if (need_to_clear_bottom)
	    w->shadow_thickness_changed = 1;
	}

      if (!need_to_clear_bottom)
	{
	  cdl = ddl = 0;

	  if (cdla_len)
	    cdl = Dynarr_atp (cdla, cdla_len - 1);
	  if (ddla_len)
	    ddl = Dynarr_atp (ddla, ddla_len - 1);

	  if (!cdl || !ddl)
	    need_to_clear_bottom = 1;
	  else
	    {
	      int cdl_bottom, ddl_bottom;

	      cdl_bottom = cdl->ypos + cdl->descent;
	      ddl_bottom = ddl->ypos + ddl->descent;

	      if (cdl_bottom > ddl_bottom)
		{
		  need_to_clear_bottom = 1;
		  need_to_clear_start = ddl_bottom;
		  need_to_clear_end = cdl_bottom;
		}
	    }
	}
    }

  /* Perform any output initialization. */
  MAYBE_DEVMETH (d, window_output_begin, (w));

  /* If the window's structure has changed clear the internal border
     above it if it is topmost (the function will check). */
  if (f->windows_structure_changed || f->faces_changed)
    redisplay_clear_top_of_window (w);

  /* Output each line. */
  for (line = 0; line < Dynarr_length (ddla); line++)
    {
      output_display_line (w, cdla, ddla, line, -1, -1);
    }

  /* If the number of display lines has shrunk, adjust. */
  if (cdla_len > ddla_len)
    {
      Dynarr_length (cdla) = ddla_len;
    }

  /* Output a vertical divider between windows, if necessary. */
  if (window_needs_vertical_divider (w)
      && (f->windows_structure_changed || f->clear))
    {
      MAYBE_DEVMETH (d, output_vertical_divider, (w, f->windows_structure_changed));
    }

  /* Clear the rest of the window, if necessary. */
  if (need_to_clear_bottom)
    {
      redisplay_clear_bottom_of_window (w, ddla, need_to_clear_start,
					need_to_clear_end);
    }

  w->window_end_pos[CURRENT_DISP] = w->window_end_pos[DESIRED_DISP];
  Fset_marker (w->start[CURRENT_DISP],
	       make_int (marker_position (w->start[DESIRED_DISP])),
	       w->buffer);
  Fset_marker (w->pointm[CURRENT_DISP],
	       make_int (marker_position (w->pointm[DESIRED_DISP])),
	       w->buffer);
  w->last_modified[CURRENT_DISP] = w->last_modified[DESIRED_DISP];
  w->last_facechange[CURRENT_DISP] = w->last_facechange[DESIRED_DISP];
  Fset_marker (w->last_start[CURRENT_DISP],
	       Fmarker_position (w->last_start[DESIRED_DISP]), w->buffer);
  Fset_marker (w->last_point[CURRENT_DISP],
	       Fmarker_position (w->last_point[DESIRED_DISP]), w->buffer);
  w->last_point_x[CURRENT_DISP] = w->last_point_x[DESIRED_DISP];
  w->last_point_y[CURRENT_DISP] = w->last_point_y[DESIRED_DISP];
  w->shadow_thickness_changed = 0;

  set_window_display_buffer (w, XBUFFER (w->buffer));
  find_window_mirror (w)->truncate_win = window_truncation_on (w);

  /* Overkill on invalidating the cache.  It is very bad for it to not
     get invalidated when it should be. */
  INVALIDATE_DEVICE_PIXEL_TO_GLYPH_CACHE (d);

  redisplay_redraw_cursor (f, 0);
  MAYBE_DEVMETH (d, window_output_end, (w));

#ifdef HAVE_SCROLLBARS
  update_window_scrollbars (w, NULL, !MINI_WINDOW_P (w), 0);
#endif
}

/*****************************************************************************
 bevel_modeline

 Draw a 3d border around the modeline on window W.
 ****************************************************************************/
void
bevel_modeline (struct window *w, struct display_line *dl)
{
  struct frame *f = XFRAME (w->frame);
  struct device *d = XDEVICE (f->device);
  int x, y, width, height;
  int shadow_thickness = MODELINE_SHADOW_THICKNESS (w);
  enum edge_style style;

  x = WINDOW_MODELINE_LEFT (w);
  width = WINDOW_MODELINE_RIGHT (w) - x;
  y = dl->ypos - dl->ascent - shadow_thickness;
  height = dl->ascent + dl->descent + 2 * shadow_thickness;

  if (XINT (w->modeline_shadow_thickness) < 0)
    {
      style = EDGE_BEVEL_IN;
    }
  else
    {
      style = EDGE_BEVEL_OUT;
    }

  MAYBE_DEVMETH (d, bevel_area,
		 (w, MODELINE_INDEX, x, y, width, height, shadow_thickness,
		  EDGE_ALL, style));
}
