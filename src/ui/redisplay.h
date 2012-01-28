/* Redisplay data structures.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1996 Chuck Thompson.
   Copyright (C) 1995, 1996 Ben Wing.

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

#ifndef INCLUDED_redisplay_h_
#define INCLUDED_redisplay_h_

/* Redisplay DASSERT types */
#define DB_DISP_POS		1
#define DB_DISP_TEXT_LAYOUT	2
#define DB_DISP_REDISPLAY	4

/* These are the possible return values from pixel_to_glyph_translation. */
#define OVER_MODELINE		0
#define OVER_TEXT		1
#define OVER_OUTSIDE		2
#define OVER_NOTHING		3
#define OVER_BORDER		4
#define OVER_TOOLBAR		5
#define OVER_V_DIVIDER		6

#define NO_BLOCK	-1

/* Imagine that the text in the buffer is displayed on a piece of paper
   the width of the frame and very very tall.  The line start cache is
   an array of struct line_start_cache's, describing the start and
   end buffer positions for a contiguous set of lines on that piece
   of paper. */

typedef struct line_start_cache line_start_cache;
struct line_start_cache {
	Bufpos start, end;
	int height;
};

typedef struct {
	Dynarr_declare(line_start_cache);
} line_start_cache_dynarr;

/* The possible types of runes.

   #### The Lisp_Glyph type is broken.  There should instead be a pixmap
   type.  Currently the device-specific output routines have to worry
   about whether the glyph is textual or not, etc.  For Mule this is
   a big problem because you might need multiple fonts to display the
   text.  It also eliminates optimizations that could come from glumping
   the text of multiple text glyphs together -- this makes displaying
   binary files (with lots of control chars, etc.) very very slow. */

#define RUNE_BLANK	0
#define RUNE_CHAR	1
#define RUNE_DGLYPH	2
#define RUNE_HLINE	3
#define RUNE_VLINE	4

#define CURSOR_ON	0
#define CURSOR_OFF	1
#define NO_CURSOR	2
#define NEXT_CURSOR	3
#define IGNORE_CURSOR	4

#define DEFAULT_INDEX	(face_index) 0
#define MODELINE_INDEX	(face_index) 1

/* A rune is a single display element, such as a printable character
   or pixmap.  Any single character in a buffer has one or more runes
   (or zero, if the character is invisible) corresponding to it.
   (Printable characters typically have one rune associated with them,
   but control characters have two -- a ^ and a letter -- and other
   non-printing characters (those displayed in octal) have four. */

/* WARNING! In compare_runes (one of the most heavily used functions)
   two runes are compared. So please be careful with changes to this
   structure. See comments in compare_runes.

   #### This should really be made smaller.
*/

typedef struct rune rune;
struct rune {
	face_index findex;	/* face rune is displayed with.  The
				   face_index is an index into a
				   window-specific array of face cache
				   elements.  Each face cache element
				   corresponds to one "merged face"
				   (the result of merging all the
				   faces that overlap the rune) and
				   contains the instance values for
				   each of the face properties in this
				   particular window. */

	Bufpos bufpos;		/* buffer position this rune is displaying;
				   for the modeline, the value here is a
				   Charcount, but who's looking? */
	Bufpos endpos;		/* if set this rune covers a range of pos */
	/* #### Chuck, what does it mean for a rune
	   to cover a range of pos?  I don't get
	   this. */
	/* #### This isn't used as an rvalue anywhere!
	   remove! */

	short xpos;		/* horizontal starting position in pixels */
	short width;		/* pixel width of rune */

	unsigned char cursor_type;	/* is this rune covered by the cursor? */
	unsigned char type;	/* type of rune object */
	/* We used to do bitfields here, but if I
	   (JV) count correctly that doesn't matter
	   for the size of the structure. All the bit
	   fiddling _does_ slow down redisplay by
	   about 10%. So don't do that */

	union {			/* Information specific to the type of rune */
		/* #### Glyphs are rare. Is it really necessary to waste 8 bytes on every
		   rune for that?! */
		/* DGLYPH */
		struct {
			Lisp_Object glyph;
			Lisp_Object extent;	/* extent rune is attached to, if any.
						   If this is a rune in the modeline
						   then this might be nil. */

			int ascent;	/* Ascent of this glyph, in pixels. */
			int descent;	/* Descent of this glyph, in pixels. */
			int yoffset;	/* Offset from line top to reach glyph top */
			int xoffset;	/* Number of pixels that need to be
					   chopped off the left of the glyph.
					   This has the effect of shifting the
					   glyph to the left while still clipping
					   at XPOS. */
		} dglyph;

		/* CHAR */
		struct {
			Emchar ch;	/* Character of this rune. */
		} chr;

		/* HLINE */
		struct {
			short thickness;	/* how thick to make hline */
			short yoffset;	/* how far down from top of line to put top */
		} hline;
	} object;		/* actual rune object */
};

typedef struct {
	Dynarr_declare(rune);
} rune_dynarr;

/* These must have distinct values.  Note that the ordering actually
   represents priority levels.  TEXT has the lowest priority level. */
enum display_type {
	TEXT,
	LEFT_OUTSIDE_MARGIN,
	LEFT_INSIDE_MARGIN,
	RIGHT_INSIDE_MARGIN,
	RIGHT_OUTSIDE_MARGIN,
	OVERWRITE
};

/* A display block represents a run of text on a single line.
   Apparently there is only one display block per line for each
   of the types listed in `enum display_type'.

   A display block consists mostly of an array of runes, one per
   atomic display element (printable character, pixmap, etc.). */

/* #### Yuckity yuckity yuck yuck yuck yuck yuck!!

   Chuck, I think you should redo this.  It should not be the
   responsibility of the device-specific code to worry about
   the different faces.  The generic stuff in redisplay-output.c
   should glump things up into sub-blocks, each of which
   corresponds to a single pixmap or a single run of text in
   the same font.

   It might still make sense for the device-specific output routine
   to get passed an entire display line.  That way, it can make
   calls to XDrawText() (which draws multiple runs of single-font
   data) instead of XDrawString().  The reason for this is to
   reduce the amount of X traffic, which will help things significantly
   on a slow line. */

typedef struct display_block display_block;
struct display_block {
	enum display_type type;	/* type of display block */

	int start_pos;		/* starting pixel position of block */
	int end_pos;		/* ending pixel position of block */

	rune_dynarr *runes;	/* Dynamic array of runes */
};

typedef struct {
	Dynarr_declare(display_block);
} display_block_dynarr;

typedef struct layout_bounds_type {
	int left_out;
	int left_in;
	int left_white;
	int right_white;
	int right_in;
	int right_out;
} layout_bounds;

typedef struct glyph_block glyph_block;
struct glyph_block {
	Lisp_Object glyph;
	Lisp_Object extent;

	face_index findex; /* Only used by margin routines.  */
	int active;	   /* For begin/end glyph indicates type,
			      otherwise for margin routines. */
	int width;	   /* Only used by margin routines.  */
};

typedef struct {
	Dynarr_declare(glyph_block);
} glyph_block_dynarr;

/*************************************************************************/
/*                              display lines                             */
/*************************************************************************/

/*  Modeline commentary: IMO the modeline is handled very badly, we
  special case virtually *everything* in the redisplay routines for
  the modeline. The fact that dl->bufpos can be either a buffer
  position or a char count highlights this. There is no abstraction at
  all that I can find and it means that the code is made very ugly as
  a result. Either we should treat the modeline *entirely* separately,
  or we should abstract to something that applies equally well to the
  modeline and to buffer text, the things are not enormously different
  after all and handling them identically at some level would
  eliminate some bugs that still exist (mainly to do with modeline
  handling). This problem doesn't help trying to implement gutters
  which are somewhere in between buffer text and modeline text.

  Redisplay commentary: Everything in redisplay is tied very tightly
  to the things that are being displayed, and the context,
  e.g. buffers and windows. According to Chuck this is so that we can
  get speed, which seems fine to me, however this usage is extended
  too far down the redisplay routines IMO. At some level there should
  be functions that know how to display strings with extents and
  faces, regardless of buffer etc. After all the window system does
  not care. <andy@xemacs.org> */

typedef struct display_line display_line;
struct display_line {
	short ypos;		/* vertical position in pixels
				   of the baseline for this line. */
	unsigned short ascent, descent;	/* maximum values for this line.
					   The ascent is the number of
					   pixels above the baseline, and
					   the descent is the number of
					   pixels below the baseline.
					   The descent includes the baseline
					   pixel-row itself, I think. */
	unsigned short clip;	/* amount of bottom of line to clip
				   in pixels. */
	unsigned short top_clip;	/* amount of top of line to clip
					   in pixels. */
	Bufpos bufpos;		/* first buffer position on line */
	Bufpos end_bufpos;	/* last buffer position on line */
	Charcount offset;	/* adjustment to bufpos vals */
	Charcount num_chars;	/* # of chars on line
				   including expansion of tabs
				   and control chars */
	int cursor_elt;		/* rune block of TEXT display
				   block cursor is at or -1 */
	char used_prop_data;	/* can't incrementally update if line
				   used propagation data */

	layout_bounds bounds;	/* line boundary positions */

	char modeline;		/* t if this line is a modeline */

	char line_continuation;	/* t if this line continues to
				   next display line. */

	/* Dynamic array of display blocks */
	display_block_dynarr *display_blocks;

	/* Dynamic arrays of left and right glyph blocks */
	glyph_block_dynarr *left_glyphs;
	glyph_block_dynarr *right_glyphs;

	face_index left_margin_findex;
	face_index right_margin_findex;
	face_index default_findex;
};

#define DISPLAY_LINE_HEIGHT(dl) \
(dl->ascent + dl->descent - (dl->clip + dl->top_clip))
#define DISPLAY_LINE_YPOS(dl) \
(dl->ypos - (dl->ascent - dl->top_clip))
#define DISPLAY_LINE_YEND(dl) \
((dl->ypos + dl->descent) - dl->clip)

typedef struct {
	Dynarr_declare(display_line);
} display_line_dynarr;

/* The following two structures are used to represent an area to
displayed and where to display it. Using these two structures all
combinations of clipping and position can be accommodated.  */

/* This represents an area to be displayed into. */
typedef struct display_box display_box;
struct display_box {
	int xpos;		/* absolute horizontal position of area */
	int ypos;		/* absolute vertical position of area */
	int width, height;
};

/* This represents the area from a glyph to be displayed. */
typedef struct display_glyph_area display_glyph_area;
struct display_glyph_area {
	int xoffset;		/* horizontal offset of the glyph, +ve means
				   display the glyph with x offset by xoffset,
				   -ve means display starting xoffset into the
				   glyph. */
	int yoffset;		/* vertical offset of the glyph, +ve means
				   display the glyph with y offset by yoffset,
				   -ve means display starting xoffset into the
				   glyph. */
	int width, height;	/* width and height of glyph to display. */
};

/* It could be argued that the following two structs belong in
   extents.h, but they're only used by redisplay and it simplifies
   the header files to put them here. */

typedef struct {
	Dynarr_declare(EXTENT);
} EXTENT_dynarr;

struct font_metric_info {
	int width;
	int height;		/* always ascent + descent; for convenience */
	int ascent;
	int descent;

	int proportional_p;
};

/* NOTE NOTE NOTE: Currently the positions in an extent fragment
   structure are Bytind's, not Bufpos's.  This could change. */

struct extent_fragment {
	Lisp_Object object;	/* buffer or string */
	struct frame *frm;
	Bytind pos, end;
	EXTENT_dynarr *extents;
	glyph_block_dynarr *glyphs;
	unsigned int invisible:1;
	unsigned int invisible_ellipses:1;
	unsigned int previously_invisible:1;
	unsigned int invisible_ellipses_already_displayed:1;
};

#define EDGE_TOP 1
#define EDGE_LEFT 2
#define EDGE_BOTTOM 4
#define EDGE_RIGHT 8
#define EDGE_ALL (EDGE_TOP | EDGE_LEFT | EDGE_BOTTOM | EDGE_RIGHT)

/*************************************************************************/
/*                              change flags                             */
/*************************************************************************/

/* Quick flags to signal redisplay.  redisplay() sets them all to 0
   when it finishes.  If none of them are set when it starts, it
   assumes that nothing needs to be done.  Functions that make a change
   that is (potentially) visible on the screen should set the
   appropriate flag.

   If any of these flags are set, redisplay will look more carefully
   to see if anything has really changed. */

/* Nonzero if the contents of a buffer have changed since the last time
   redisplay completed. */
extern int buffers_changed;
extern int buffers_changed_set;

/* Nonzero if head_clip or tail_clip of a buffer has changed
   since last redisplay that finished. */
extern int clip_changed;
extern int clip_changed_set;

/* Nonzero if any extent has changed since the last time redisplay completed. */
extern int extents_changed;
extern int extents_changed_set;

/* Nonzero if any face has changed since the last time redisplay completed. */
extern int faces_changed;

/* Nonzero means one or more frames have been marked as garbaged. */
extern int frame_changed;

/* True if any of the builtin display glyphs (continuation,
   hscroll, control-arrow, etc) is in need of updating
   somewhere. */
extern int glyphs_changed;
extern int glyphs_changed_set;

/* True if any displayed subwindow is in need of updating
   somewhere. */
extern int subwindows_changed;
extern int subwindows_changed_set;

/* True if any displayed subwindow is in need of updating
   somewhere. */
extern int subwindows_state_changed;
extern int subwindows_state_changed_set;

/* True if an icon is in need of updating somewhere. */
extern int icon_changed;
extern int icon_changed_set;

/* True if a menubar is in need of updating somewhere. */
extern int menubar_changed;
extern int menubar_changed_set;

/* True iff we should redraw the modelines on the next redisplay. */
extern int modeline_changed;
extern int modeline_changed_set;

/* Nonzero if point has changed in some buffer since the last time
   redisplay completed. */
extern int point_changed;
extern int point_changed_set;

/* Nonzero if some frame has changed its size. */
extern int size_changed;

/* Nonzero if some device has signaled that it wants to change size. */
extern int asynch_device_change_pending;

/* Nonzero if some frame has changed the layout of internal elements
   (gutters or toolbars). */
extern int frame_layout_changed;

/* Nonzero if any toolbar has changed. */
extern int toolbar_changed;
extern int toolbar_changed_set;

/* Nonzero if any gutter has changed. */
extern int gutter_changed;
extern int gutter_changed_set;

/* Nonzero if any window has changed since the last time redisplay completed */
extern int windows_changed;

/* Nonzero if any frame's window structure has changed since the last
   time redisplay completed. */
extern int windows_structure_changed;

/* These macros can be relatively expensive.  Since they are often
   called numerous times between each call to redisplay, we keep track
   if each has already been called and don't bother doing most of the
   work if it is currently set. */

#define MARK_TYPE_CHANGED(object) do {				\
  if (!object##_changed_set) {					\
    Lisp_Object MTC_devcons, MTC_concons;			\
    DEVICE_LOOP_NO_BREAK (MTC_devcons, MTC_concons)		\
      {								\
	Lisp_Object MTC_frmcons;				\
	struct device *MTC_d = XDEVICE (XCAR (MTC_devcons));	\
	DEVICE_FRAME_LOOP (MTC_frmcons, MTC_d)			\
	  {							\
	    struct frame *MTC_f = XFRAME (XCAR (MTC_frmcons));	\
	    MTC_f->object##_changed = 1;			\
	    MTC_f->modiff++;					\
	  }							\
	MTC_d->object##_changed = 1;				\
      }								\
    object##_changed = 1;					\
    object##_changed_set = 1; }					\
  }  while (0)

#define MARK_BUFFERS_CHANGED MARK_TYPE_CHANGED (buffers)
#define MARK_CLIP_CHANGED MARK_TYPE_CHANGED (clip)
#define MARK_EXTENTS_CHANGED MARK_TYPE_CHANGED (extents)
#define MARK_ICON_CHANGED MARK_TYPE_CHANGED (icon)
#define MARK_MENUBAR_CHANGED MARK_TYPE_CHANGED (menubar)
#define MARK_MODELINE_CHANGED MARK_TYPE_CHANGED (modeline)
#define MARK_POINT_CHANGED MARK_TYPE_CHANGED (point)
#define MARK_TOOLBAR_CHANGED MARK_TYPE_CHANGED (toolbar)
#define MARK_GUTTER_CHANGED MARK_TYPE_CHANGED (gutter)
#define MARK_GLYPHS_CHANGED MARK_TYPE_CHANGED (glyphs)
#define MARK_SUBWINDOWS_CHANGED MARK_TYPE_CHANGED (subwindows)
#define MARK_SUBWINDOWS_STATE_CHANGED MARK_TYPE_CHANGED (subwindows_state)

#define CLASS_RESET_CHANGED_FLAGS(p) do {	\
  (p)->buffers_changed = 0;			\
  (p)->clip_changed = 0;			\
  (p)->extents_changed = 0;			\
  (p)->faces_changed = 0;			\
  (p)->frame_changed = 0;			\
  (p)->frame_layout_changed = 0;		\
  (p)->icon_changed = 0;			\
  (p)->menubar_changed = 0;			\
  (p)->modeline_changed = 0;			\
  (p)->point_changed = 0;			\
  (p)->toolbar_changed = 0;			\
  (p)->gutter_changed = 0;			\
  (p)->glyphs_changed = 0;			\
  (p)->subwindows_changed = 0;			\
  (p)->subwindows_state_changed = 0;		\
  (p)->windows_changed = 0;			\
  (p)->windows_structure_changed = 0;		\
} while (0)

#define GLOBAL_RESET_CHANGED_FLAGS do {		\
  buffers_changed = 0;				\
  clip_changed = 0;				\
  extents_changed = 0;				\
  frame_changed = 0;				\
  frame_layout_changed = 0;			\
  icon_changed = 0;				\
  menubar_changed = 0;				\
  modeline_changed = 0;				\
  point_changed = 0;				\
  toolbar_changed = 0;				\
  gutter_changed = 0;				\
  glyphs_changed = 0;				\
  subwindows_changed = 0;			\
  subwindows_state_changed = 0;			\
  windows_changed = 0;				\
  windows_structure_changed = 0;		\
} while (0)

#define CLASS_REDISPLAY_FLAGS_CHANGEDP(p)	\
  ( (p)->buffers_changed ||			\
    (p)->clip_changed ||			\
    (p)->extents_changed ||			\
    (p)->faces_changed ||			\
    (p)->frame_changed ||			\
    (p)->frame_layout_changed ||		\
    (p)->icon_changed ||			\
    (p)->menubar_changed ||			\
    (p)->modeline_changed ||			\
    (p)->point_changed ||			\
    (p)->toolbar_changed ||			\
    (p)->gutter_changed ||			\
    (p)->glyphs_changed ||			\
    (p)->size_changed ||			\
    (p)->subwindows_changed ||			\
    (p)->subwindows_state_changed ||		\
    (p)->windows_changed ||			\
    (p)->windows_structure_changed )

#define GLOBAL_REDISPLAY_FLAGS_CHANGEDP		\
  ( buffers_changed ||				\
    clip_changed ||				\
    extents_changed ||				\
    faces_changed ||				\
    frame_changed ||				\
    frame_layout_changed ||			\
    icon_changed ||				\
    menubar_changed ||				\
    modeline_changed ||				\
    point_changed ||				\
    toolbar_changed ||				\
    gutter_changed ||				\
    glyphs_changed ||				\
    size_changed ||				\
    subwindows_changed ||			\
    subwindows_state_changed ||			\
    windows_changed ||				\
    windows_structure_changed )

/* Anytime a console, device or frame is added or deleted we need to reset
   these flags. */
#define RESET_CHANGED_SET_FLAGS do {	\
  buffers_changed_set = 0;		\
  clip_changed_set = 0;			\
  extents_changed_set = 0;		\
  icon_changed_set = 0;			\
  menubar_changed_set = 0;		\
  modeline_changed_set = 0;		\
  point_changed_set = 0;		\
  toolbar_changed_set = 0;		\
  gutter_changed_set = 0;		\
  glyphs_changed_set = 0;		\
  subwindows_changed_set = 0;		\
  subwindows_state_changed_set = 0;	\
} while (0)

/*************************************************************************/
/*                       redisplay global variables                      */
/*************************************************************************/

/* redisplay structure used by various utility routines. */
extern display_line_dynarr *cmotion_display_lines;

/* Nonzero means truncate lines in all windows less wide than the frame. */
extern int truncate_partial_width_windows;

/* Nonzero if we're in a display critical section. */
extern int in_display;

/* Nonzero means no need to redraw the entire frame on resuming
   a suspended Emacs.  This is useful on terminals with multiple pages,
   where one page is used for Emacs and another for all else. */
extern int no_redraw_on_reenter;

/* Non-nil means flash the frame instead of ringing the bell.  */
extern Lisp_Object Vvisible_bell;

/* Thickness of shadow border around 3D modelines. */
extern Lisp_Object Vmodeline_shadow_thickness;

/* Scroll if point lands on the bottom line and that line is partially
   clipped. */
extern int scroll_on_clipped_lines;

extern Lisp_Object Vglobal_mode_string;

/* The following two variables are defined in emacs.c and are used
   to convey information discovered on the command line way early
   (before *anything* is initialized). */

/* If non-zero, a window-system was specified on the command line.
   Defined in emacs.c. */
extern int display_arg;

/* Type of display specified.  Defined in emacs.c. */
extern const char *display_use;

/* Nonzero means reading single-character input with prompt
   so put cursor on minibuffer after the prompt.  */

extern int cursor_in_echo_area;

extern Lisp_Object Qbar_cursor, Qcursor_in_echo_area, Vwindow_system;

extern Lisp_Object Qtop_bottom;

/*************************************************************************/
/*                     redisplay exported functions                      */
/*************************************************************************/
EXFUN(Fredraw_frame, 2);

int redisplay_text_width_string(struct window *w, int findex,
				Bufbyte * nonreloc, Lisp_Object reloc,
				Bytecount offset, Bytecount len);
int redisplay_frame_text_width_string(struct frame *f,
				      Lisp_Object face,
				      Bufbyte * nonreloc,
				      Lisp_Object reloc,
				      Bytecount offset, Bytecount len);
int redisplay_frame(struct frame *f, int preemption_check);
void redisplay(void);
struct display_block *get_display_block_from_line(struct display_line *dl,
						  enum display_type type);
layout_bounds calculate_display_line_boundaries(struct window *w, int modeline);
Bufpos point_at_center(struct window *w, int type, Bufpos start, Bufpos point);
int line_at_center(struct window *w, int type, Bufpos start, Bufpos point);
int window_half_pixpos(struct window *w);
void redisplay_echo_area(void);
void free_display_structs(struct window_mirror *mir);
void free_display_lines(display_line_dynarr * dla);
void mark_redisplay_structs(display_line_dynarr * dla);
void generate_displayable_area(struct window *w, Lisp_Object disp_string,
			       int xpos, int ypos, int width, int height,
			       display_line_dynarr * dl,
			       Bufpos start_pos, face_index default_face);
/* `generate_title_string' in frame.c needs this */
void generate_formatted_string_db(Lisp_Object format_str,
				  Lisp_Object result_str,
				  struct window *w,
				  struct display_line *dl,
				  struct display_block *db,
				  face_index findex,
				  int min_pixpos, int max_pixpos, int type);
int real_current_modeline_height(struct window *w);
int pixel_to_glyph_translation(struct frame *f, int x_coord,
			       int y_coord, int *col, int *row,
			       int *obj_x, int *obj_y,
			       struct window **w, Bufpos * bufpos,
			       Bufpos * closest, Charcount * modeline_closest,
			       Lisp_Object * obj1, Lisp_Object * obj2);
void glyph_to_pixel_translation(struct window *w, int char_x,
				int char_y, int *pix_x, int *pix_y);
void mark_redisplay(void);
int point_in_line_start_cache(struct window *w, Bufpos point, int min_past);
int point_would_be_visible(struct window *w, Bufpos startp, Bufpos point);
Bufpos start_of_last_line(struct window *w, Bufpos startp);
Bufpos end_of_last_line(struct window *w, Bufpos startp);
Bufpos start_with_line_at_pixpos(struct window *w, Bufpos point, int pixpos);
Bufpos start_with_point_on_display_line(struct window *w, Bufpos point,
					int line);
int redisplay_variable_changed(Lisp_Object sym, Lisp_Object * val,
			       Lisp_Object in_object, int flags);
void redisplay_glyph_changed(Lisp_Object glyph, Lisp_Object property,
			     Lisp_Object locale);

#ifdef MEMORY_USAGE_STATS
int compute_display_line_dynarr_usage(display_line_dynarr * dyn,
				      struct overhead_stats *ovstats);
int compute_line_start_cache_dynarr_usage(line_start_cache_dynarr * dyn,
					  struct overhead_stats *ovstats);
#endif

/* defined in redisplay-output.c */
int get_next_display_block(layout_bounds bounds,
			   display_block_dynarr * dba, int start_pos,
			   int *next_start);
void redisplay_output_layout(Lisp_Object domain,
			     Lisp_Object image_instance,
			     struct display_box *db,
			     struct display_glyph_area *dga, face_index findex,
			     int cursor_start, int cursor_width,
			     int cursor_height);
void redisplay_output_subwindow(struct window *w, Lisp_Object image_instance,
				struct display_box *db,
				struct display_glyph_area *dga,
				face_index findex, int cursor_start,
				int cursor_width, int cursor_height);
void redisplay_unmap_subwindows_maybe(struct frame *f, int x, int y, int width,
				      int height);
void redisplay_output_pixmap(struct window *w, Lisp_Object image_instance,
			     struct display_box *db,
			     struct display_glyph_area *dga, face_index findex,
			     int cursor_start, int cursor_width,
			     int cursor_height, int offset_bitmap);
int redisplay_calculate_display_boxes(struct display_line *dl, int xpos,
				      int xoffset, int yoffset,
				      int start_pixpos, int width,
				      struct display_box *dest,
				      struct display_glyph_area *src);
int redisplay_normalize_glyph_area(struct display_box *dest,
				   struct display_glyph_area *glyphsrc);
void redisplay_clear_to_window_end(struct window *w, int ypos1, int ypos2);
void redisplay_clear_region(Lisp_Object window, face_index findex, int x,
			    int y, int width, int height);
void redisplay_clear_top_of_window(struct window *w);
void redisplay_clear_bottom_of_window(struct window *w,
				      display_line_dynarr * ddla,
				      int min_start, int max_end);
void redisplay_update_line(struct window *w, int first_line,
			   int last_line, int update_values);
void redisplay_output_window(struct window *w);
void bevel_modeline(struct window *w, struct display_line *dl);
int redisplay_move_cursor(struct window *w, Bufpos new_point,
			  int no_output_end);
void redisplay_redraw_cursor(struct frame *f, int run_begin_end_meths);
void output_display_line(struct window *w, display_line_dynarr * cdla,
			 display_line_dynarr * ddla, int line,
			 int force_start, int force_end);
void sync_display_line_structs(struct window *w, int line, int do_blocks,
			       display_line_dynarr * cdla,
			       display_line_dynarr * ddla);

#endif				/* INCLUDED_redisplay_h_ */
