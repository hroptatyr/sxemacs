/* Window definitions for XEmacs.
   Copyright (C) 1985, 1986, 1992, 1993, 1994, 1995
   Free Software Foundation, Inc.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996 Ben Wing.
   Copyright (C) 1996 Chuck Thompson.

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


/* Synched up with: FSF 19.30. */

#ifndef INCLUDED_window_h_
#define INCLUDED_window_h_

#include "redisplay.h"
#ifdef HAVE_SCROLLBARS
#include "scrollbar.h"
#endif

/* All windows in use are arranged into a tree, with pointers up and down.

Windows that are leaves of the tree are actually displayed
and show the contents of buffers.  Windows that are not leaves
are used for representing the way groups of leaf windows are
arranged on the frame.  Leaf windows never become non-leaves.
They are deleted only by calling delete-window on them (but
this can be done implicitly).  Combination windows can be created
and deleted at any time.

A leaf window has a non-nil buffer field, and also
 has markers in its start and pointm fields.  Non-leaf windows
 have nil in these fields.

Non-leaf windows are either vertical or horizontal combinations.

A vertical combination window has children that are arranged on the frame
one above the next.  Its vchild field points to the uppermost child.
The parent field of each of the children points to the vertical
combination window.  The next field of each child points to the
child below it, or is nil for the lowest child.  The prev field
of each child points to the child above it, or is nil for the
highest child.

A horizontal combination window has children that are side by side.
Its hchild field points to the leftmost child.  In each child
the next field points to the child to the right and the prev field
points to the child to the left.

The children of a vertical combination window may be leaf windows
or horizontal combination windows.  The children of a horizontal
combination window may be leaf windows or vertical combination windows.

At the top of the tree are two windows which have nil as parent.
The second of these is minibuf_window.  The first one manages all
the frame area that is not minibuffer, and is called the root window.
Different windows can be the root at different times;
initially the root window is a leaf window, but if more windows
are created then that leaf window ceases to be root and a newly
made combination window becomes root instead.

In any case, on screens which have an ordinary window and a
minibuffer, prev of the minibuf window is the root window and next of
the root window is the minibuf window.  On minibufferless screens or
minibuffer-only screens, the root window and the minibuffer window are
one and the same, so its prev and next members are nil.

A dead window has the `dead' flag set on it.  Note that unlike other
dead objects, dead windows can be made live again through restoring a
window configuration.  This means that the values in a dead window
need to be preserved, except for those that are reconstructed by from
the window configuration. */

struct window {
	struct lcrecord_header header;

	/* The frame this window is on.  */
	Lisp_Object frame;
	/* t if this window is a minibuffer window.  */
	Lisp_Object mini_p;
	/* Following child (to right or down) at same level of tree */
	Lisp_Object next;
	/* Preceding child (to left or up) at same level of tree */
	Lisp_Object prev;
	/* First child of this window. */
	/* vchild is used if this is a vertical combination,
	   hchild if this is a horizontal combination. */
	Lisp_Object hchild, vchild;
	/* The window this one is a child of. */
	Lisp_Object parent;

	/* The upper left corner coordinates of this window,
	   as integers (pixels) relative to upper left corner of frame = 0, 0 */
	int pixel_left;
	int pixel_top;
	/* The size of the window (in pixels) */
	int pixel_height;
	int pixel_width;

	/* The buffer displayed in this window */
	/* Of the fields vchild, hchild and buffer, only one is non-nil.  */
	Lisp_Object buffer;
	/* A marker pointing to where in the text to start displaying */
	/* need one for each set of display structures */
	Lisp_Object start[3];
	/* A marker pointing to where in the text point is in this window,
	   used only when the window is not selected.
	   This exists so that when multiple windows show one buffer
	   each one can have its own value of point.  */
	/* need one for each set of display structures */
	Lisp_Object pointm[3];
	/* A marker pointing to where in the text the scrollbar is pointing */
	Lisp_Object sb_point;
	/* Number of columns display within the window is scrolled to the left. */
	int hscroll;
	/* Idem for the window's modeline */
	Charcount modeline_hscroll;
	/* Amount to clip off the top line for pixel-based scrolling. Point
	   will remain constant but this will be incremented to
	   incrementally shift lines up. */
	int top_yoffset;
	/* Amount to clip off the left of the lines for pixel-based
	   scrolling. Hscroll will remain constant but this will be
	   incremented to incrementally shift lines left. */
	int left_xoffset;
	/* Number saying how recently window was selected */
	Lisp_Object use_time;
	/* text.modified of displayed buffer as of last time display completed */
	Lisp_Object last_modified[3];
	/* Value of point at that time */
	Lisp_Object last_point[3];
	/* Value of start at that time */
	Lisp_Object last_start[3];
	/* buf.face_change as of last time display completed */
	Lisp_Object last_facechange[3];

	/* face cache elements correct for this window and its current buffer */
	face_cachel_dynarr *face_cachels;
	/* glyph cache elements correct for this window and its current buffer */
	glyph_cachel_dynarr *glyph_cachels;
	/* we cannot have a per-device cache of widgets / subwindows because
	   each visible instance needs to be a separate instance. The lowest
	   level of granularity we can get easily is the window that the
	   subwindow is in. This will fail if we attach the same subwindow
	   twice to a buffer. However, we are quite unlikely to do this,
	   especially with buttons which will need individual callbacks. The
	   proper solution is probably not worth the effort. */
	Lisp_Object subwindow_instance_cache;
	/* List of starting positions for display lines.  Only valid if
	   buffer has not changed. */
	line_start_cache_dynarr *line_start_cache;
	Lisp_Object line_cache_last_updated;
	int line_cache_validation_override;

	/* Length of longest line currently displayed.  Used to control the
	   width of the horizontal scrollbars. */
	int max_line_len;

	/* Frame coords of point at that time */
	int last_point_x[3];
	int last_point_y[3];

	/* Number of characters in buffer past bottom of window,
	   as of last redisplay that finished. */
	/* need one for each set of display structures */
	int window_end_pos[3];

	/* If redisplay in this window goes beyond this buffer position,
	   must run the redisplay-end-trigger-functions.  */
	Lisp_Object redisplay_end_trigger;

	/* Set by the extent code when extents in the gutter are changed. */
	int gutter_extent_modiff[4];

	/* Set by redisplay to the last position seen.  This is used
	   to implement the redisplay-end-trigger-functions. */
	Bufpos last_redisplay_pos;

#define WINDOW_SLOT_DECLARATION
#define WINDOW_SLOT(slot, compare) Lisp_Object slot
#include "winslots.h"

	/* one-bit flags: */

	/* marker used when restoring a window configuration */
	unsigned int config_mark:1;
	/* Non-zero means window was dead. */
	unsigned int dead:1;
	/* Non-zero means next redisplay must use the value of start
	   set up for it in advance.  Set by scrolling commands.  */
	unsigned int force_start:1;
	/* Non-zero means must regenerate modeline of this window */
	unsigned int redo_modeline:1;
	/* Non-zero means current value of `start'
	   was the beginning of a line when it was chosen.  */
	unsigned int start_at_line_beg:1;
	/* new redisplay flag */
	unsigned int windows_changed:1;
	unsigned int shadow_thickness_changed:1;
	/* Vertical divider flag and validity of it */
	unsigned int need_vertical_divider_p:1;
	unsigned int need_vertical_divider_valid_p:1;
};

#define CURRENT_DISP	0
#define DESIRED_DISP	1
#define CMOTION_DISP	2

struct window_mirror {
	/* Frame this mirror is on. */
	struct frame *frame;

	/* Following child (to right or down) at same level of tree */
	struct window_mirror *next;

	/* There is no prev field because we never traverse this structure
	   backwards.  Same goes for the parent field. */

	/* First child of this window. */
	/* vchild is used if this is a vertical combination,
	   hchild if this is a horizontal combination. */
	struct window_mirror *hchild, *vchild;

	/* Dynamic array of display lines */
	display_line_dynarr *current_display_lines;
	display_line_dynarr *desired_display_lines;

	/* Buffer current_display_lines represent. */
	struct buffer *buffer;

#ifdef HAVE_SCROLLBARS
	/* Scrollbars associated with window, if any. */
	struct scrollbar_instance *scrollbar_vertical_instance;
	struct scrollbar_instance *scrollbar_horizontal_instance;
#endif				/* HAVE_SCROLLBARS */

	/* Flag indicating whether a subwindow is currently being displayed. */
	unsigned int subwindows_being_displayed:1;

	/* Keep track of the truncation status in this window so we can
	   detect when it has changed.  #### Magic variables would be a huge
	   win here. */
	unsigned int truncate_win:1;
};

#ifdef emacs			/* some things other than emacs want the structs */

DECLARE_LRECORD(window, struct window);
#define XWINDOW(x) XRECORD (x, window, struct window)
#define XSETWINDOW(x, p) XSETRECORD (x, p, window)
#define WINDOWP(x) RECORDP (x, window)
#define CHECK_WINDOW(x) CHECK_RECORD (x, window)
#define CONCHECK_WINDOW(x) CONCHECK_RECORD (x, window)

#define WINDOW_LIVE_P(x) (!(x)->dead)
#define CHECK_LIVE_WINDOW(x) do {			\
  CHECK_WINDOW (x);					\
  if (!WINDOW_LIVE_P (XWINDOW (x)))			\
    dead_wrong_type_argument (Qwindow_live_p, (x));	\
} while (0)
#define CONCHECK_LIVE_WINDOW(x) do {			\
  CONCHECK_WINDOW (x);					\
  if (!WINDOW_LIVE_P (XWINDOW (x)))			\
    x = wrong_type_argument (Qwindow_live_p, (x));	\
} while (0)

/* 1 if W is a minibuffer window.  */
#define MINI_WINDOW_P(W)  (!NILP ((W)->mini_p))

/* 1 if we are dealing with a parentless window (this includes the
   root window on a frame and the minibuffer window; both of these
   are siblings). */
#define TOP_LEVEL_WINDOW_P(w) NILP ((w)->parent)

/* Set all redisplay flags indicating a window has changed */
#define MARK_WINDOWS_CHANGED(w) do {			\
  (w)->windows_changed = 1;				\
  if (!NILP (w->frame))					\
    {							\
      struct frame *mwc_frame = XFRAME (w->frame);	\
      MARK_FRAME_WINDOWS_CHANGED (mwc_frame);		\
    }							\
  else							\
    windows_changed = 1;				\
} while (0)

/* #### This should be fixed not to call MARK_FRAME_CHANGED because
   faces are cached per window.  Also, other code which changes window's
   face should use this macro.
*/
#define MARK_WINDOW_FACES_CHANGED(w)	\
  MARK_FRAME_FACES_CHANGED (XFRAME ((w)->frame))

#define WINDOW_TTY_P(w) FRAME_TTY_P (XFRAME ((w)->frame))
#define WINDOW_X_P(w)   FRAME_X_P   (XFRAME ((w)->frame))
#define WINDOW_NS_P(w)  FRAME_NS_P  (XFRAME ((w)->frame))
#define WINDOW_WIN_P(w) FRAME_WIN_P (XFRAME ((w)->frame))

DECLARE_LRECORD(window_configuration, struct window_config);

EXFUN(Fget_buffer_window, 3);
EXFUN(Fmove_to_window_line, 2);
EXFUN(Frecenter, 2);
EXFUN(Freplace_buffer_in_windows, 3);
EXFUN(Fselect_window, 2);
EXFUN(Fselected_window, 1);
EXFUN(Fset_window_buffer, 3);
EXFUN(Fset_window_hscroll, 2);
EXFUN(Fset_window_point, 2);
EXFUN(Fset_window_start, 3);
EXFUN(Fwindow_buffer, 1);
EXFUN(Fwindow_highest_p, 1);
EXFUN(Fwindow_point, 1);
EXFUN(Fwindow_start, 1);

/* The minibuffer window of the selected frame.
   Note that you cannot test for minibufferness of an arbitrary window
   by comparing against this; but you can test for minibufferness of
   the selected window or of any window that is displayed.  */
extern Lisp_Object minibuf_window;

/* Prompt to display in front of the minibuffer contents, or nil */
extern Lisp_Object Vminibuf_prompt;
/* Prompt to display in front of the minibuffer prompt, or nil */
extern Lisp_Object Vminibuf_preprompt;

Lisp_Object allocate_window(void);
int window_char_width(struct window *, int include_margins_p);
int window_char_height(struct window *, int include_gutters_p);
int window_displayed_height(struct window *);
int window_is_leftmost(struct window *w);
int window_is_rightmost(struct window *w);
int window_is_lowest(struct window *w);
int window_is_highest(struct window *w);
int window_truncation_on(struct window *w);
int window_needs_vertical_divider(struct window *);
int window_scrollbar_width(struct window *w);
int window_scrollbar_height(struct window *w);
int window_modeline_height(struct window *w);
int window_left_margin_width(struct window *w);
int window_right_margin_width(struct window *w);
int window_top_gutter_height(struct window *w);
int window_bottom_gutter_height(struct window *w);
int window_left_gutter_width(struct window *w, int modeline);
int window_right_gutter_width(struct window *w, int modeline);

void delete_all_subwindows(struct window *w);
void undedicate_windows(Lisp_Object buffer, Lisp_Object frame);
void set_window_pixheight(Lisp_Object window, int pixheight, int nodelete);
void set_window_pixwidth(Lisp_Object window, int pixwidth, int nodelete);
void window_scroll(Lisp_Object window, Lisp_Object n, int direction,
		   Error_behavior errb);
int buffer_window_count(struct buffer *b, struct frame *f);
int buffer_window_mru(struct window *w);
void check_frame_size(struct frame *frame, int *rows, int *cols);
int frame_pixsize_valid_p(struct frame *frame, int width, int height);
int frame_size_valid_p(struct frame *frame, int rows, int cols);
struct window *decode_window(Lisp_Object window);
struct window *find_window_by_pixel_pos(int pix_x, int pix_y, Lisp_Object win);

/* new functions to handle the window mirror */
void free_window_mirror(struct window_mirror *mir);
Lisp_Object real_window(struct window_mirror *mir, int no_abort);
struct window_mirror *find_window_mirror(struct window *w);
display_line_dynarr *window_display_lines(struct window *w, int);
struct buffer *window_display_buffer(struct window *w);
void set_window_display_buffer(struct window *w, struct buffer *b);
void update_frame_window_mirror(struct frame *f);

int map_windows(struct frame *f,
		int (*mapfun) (struct window * w, void *closure),
		void *closure);
void some_window_value_changed(Lisp_Object specifier, struct window *w,
			       Lisp_Object oldval);
int invalidate_vertical_divider_cache_in_window(struct window *w,
						void *u_n_u_s_e_d);
int window_divider_width(struct window *w);

#define WINDOW_FRAME(w) ((w)->frame)
#define WINDOW_XFRAME(w) XFRAME (WINDOW_FRAME (w))
#define WINDOW_BUFFER(w) ((w)->buffer)
#define WINDOW_XBUFFER(w) XBUFFER (WINDOW_BUFFER (w))
#define WINDOW_DEVICE(w) FRAME_DEVICE (XFRAME (WINDOW_FRAME (w)))
#define WINDOW_XDEVICE(w) XDEVICE (WINDOW_DEVICE (w))
#define WINDOW_CONSOLE(w) DEVICE_CONSOLE (XDEVICE (WINDOW_DEVICE (w)))
#define WINDOW_XCONSOLE(w) XCONSOLE (WINDOW_CONSOLE (w))

/* XEmacs window size and positioning macros. */
#define WINDOW_TOP(w) ((w)->pixel_top)
#define WINDOW_TEXT_TOP(w) (WINDOW_TOP (w) + window_top_gutter_height (w))
#define WINDOW_TEXT_TOP_CLIP(w) ((w)->top_yoffset)
#define WINDOW_BOTTOM(w) ((w)->pixel_top + (w)->pixel_height)
#define WINDOW_TEXT_BOTTOM(w) (WINDOW_BOTTOM (w) - window_bottom_gutter_height (w))
#define WINDOW_LEFT(w) ((w)->pixel_left)
#define WINDOW_TEXT_LEFT(w) (WINDOW_LEFT (w) + window_left_gutter_width (w, 0))
#define WINDOW_MODELINE_LEFT(w)	\
  (WINDOW_LEFT (w) + window_left_gutter_width (w, 1))
#define WINDOW_RIGHT(w) ((w)->pixel_left + (w)->pixel_width)
#define WINDOW_TEXT_RIGHT(w)	\
  (WINDOW_RIGHT (w) - window_right_gutter_width (w, 0))
#define WINDOW_MODELINE_RIGHT(w)	\
  (WINDOW_RIGHT (w) - window_right_gutter_width (w, 1))

#define WINDOW_HEIGHT(w) ((w)->pixel_height)
#define WINDOW_TEXT_HEIGHT(w) (WINDOW_TEXT_BOTTOM (w) - WINDOW_TEXT_TOP (w))
#define WINDOW_WIDTH(w) ((w)->pixel_width)
#define WINDOW_TEXT_WIDTH(w) (WINDOW_TEXT_RIGHT (w) - WINDOW_TEXT_LEFT (w))

#define WINDOW_HAS_MODELINE_P(w) (!NILP (w->has_modeline_p))

#define MODELINE_OFF_SHADOW_THICKNESS_ADJUSTED(win)		\
 abs ((!WINDOW_HAS_MODELINE_P (win)				\
       ? ((XINT (win->modeline_shadow_thickness) > 1)		\
	  ? XINT (win->modeline_shadow_thickness) - 1		\
	  : ((XINT (win->modeline_shadow_thickness) < -1)	\
	     ? XINT (win->modeline_shadow_thickness) + 1	\
	     : XINT (win->modeline_shadow_thickness)))		\
       : XINT (win->modeline_shadow_thickness)))

#define MODELINE_SHADOW_THICKNESS(win)				\
 (MODELINE_OFF_SHADOW_THICKNESS_ADJUSTED (win) > 10		\
  ? 10								\
  : MODELINE_OFF_SHADOW_THICKNESS_ADJUSTED (win))

#endif				/* emacs */

#endif				/* INCLUDED_window_h_ */
