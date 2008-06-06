/* Definitions of marked slots in windows and window configs
   Copyright (C) 1985, 1986, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
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


/* Split out of window.h and window.c
   by Kirill Katsnelson <kkm@kis.ru>, May 1998 */

#ifdef WINDOW_SLOT_DECLARATION
#define WINDOW_SLOT_ARRAY(slot, size, compare) WINDOW_SLOT (slot[size], compare)
#else
#define WINDOW_SLOT_ARRAY(slot, size, compare) do {	\
  int wsaidx;						\
  for (wsaidx = 0; wsaidx < size; wsaidx++)		\
    {							\
      WINDOW_SLOT (slot[wsaidx], compare);		\
    }							\
} while (0);
#endif

#define EQUAL_WRAPPED(x,y) internal_equal ((x), (y), 0)

  /*** Non-specifier vars of window and window config ***/

  /* Non-nil means window is marked as dedicated.  */
WINDOW_SLOT(dedicated, EQ);

  /*** specifier values cached in the struct window ***/

  /* Display-table to use for displaying chars in this window. */
WINDOW_SLOT(display_table, EQUAL_WRAPPED);
  /* Thickness of modeline shadow, in pixels.  If negative, draw
     as recessed. */
WINDOW_SLOT(modeline_shadow_thickness, EQ);
  /* Non-nil means to display a modeline for the buffer. */
WINDOW_SLOT(has_modeline_p, EQ);
  /* Thickness of vertical divider shadow, in pixels.  If negative, draw as
     recessed. */
WINDOW_SLOT(vertical_divider_shadow_thickness, EQ);
  /* Divider surface width (not counting 3-d borders) */
WINDOW_SLOT(vertical_divider_line_width, EQ);
  /* Spacing between outer edge of divider border and window edge */
WINDOW_SLOT(vertical_divider_spacing, EQ);
  /* Whether vertical dividers are always displayed */
WINDOW_SLOT(vertical_divider_always_visible_p, EQ);

#ifdef HAVE_SCROLLBARS
  /* Width of vertical scrollbars. */
WINDOW_SLOT(scrollbar_width, EQ);
  /* Height of horizontal scrollbars. */
WINDOW_SLOT(scrollbar_height, EQ);
  /* Whether the scrollbars are visible */
WINDOW_SLOT(horizontal_scrollbar_visible_p, EQ);
WINDOW_SLOT(vertical_scrollbar_visible_p, EQ);
  /* Scrollbar positions */
WINDOW_SLOT(scrollbar_on_left_p, EQ);
WINDOW_SLOT(scrollbar_on_top_p, EQ);
  /* Pointer to use for vertical and horizontal scrollbars. */
WINDOW_SLOT(scrollbar_pointer, EQ);
#endif				/* HAVE_SCROLLBARS */
#ifdef HAVE_TOOLBARS
  /* Toolbar specification for each of the four positions.
     This is not a size hog because the value here is not copied,
     and will be shared with the specs in the specifier. */
WINDOW_SLOT_ARRAY(toolbar, 4, EQUAL_WRAPPED);
  /* Toolbar size for each of the four positions. */
WINDOW_SLOT_ARRAY(toolbar_size, 4, EQUAL_WRAPPED);
  /* Toolbar border width for each of the four positions. */
WINDOW_SLOT_ARRAY(toolbar_border_width, 4, EQUAL_WRAPPED);
  /* Toolbar visibility status for each of the four positions. */
WINDOW_SLOT_ARRAY(toolbar_visible_p, 4, EQUAL_WRAPPED);
  /* Caption status of toolbar. */
WINDOW_SLOT(toolbar_buttons_captioned_p, EQ);
  /* The following five don't really need to be cached except
     that we need to know when they've changed. */
WINDOW_SLOT(default_toolbar, EQUAL_WRAPPED);
WINDOW_SLOT(default_toolbar_width, EQ);
WINDOW_SLOT(default_toolbar_height, EQ);
WINDOW_SLOT(default_toolbar_visible_p, EQ);
WINDOW_SLOT(default_toolbar_border_width, EQ);
#endif				/* HAVE_TOOLBARS */

  /* Gutter specification for each of the four positions.
     This is not a size hog because the value here is not copied,
     and will be shared with the specs in the specifier. */
WINDOW_SLOT_ARRAY(gutter, 4, EQUAL_WRAPPED);
  /* Real (pre-calculated) gutter specification for each of the four positions.
     This is not a specifier, it is calculated by the specifier change
     functions. */
WINDOW_SLOT_ARRAY(real_gutter, 4, EQUAL_WRAPPED);
  /* Gutter size for each of the four positions. */
WINDOW_SLOT_ARRAY(gutter_size, 4, EQUAL_WRAPPED);
  /* Real (pre-calculated) gutter size for each of the four positions.
     This is not a specifier, it is calculated by the specifier change
     functions. */
WINDOW_SLOT_ARRAY(real_gutter_size, 4, EQUAL_WRAPPED);
  /* Gutter border width for each of the four positions. */
WINDOW_SLOT_ARRAY(gutter_border_width, 4, EQUAL_WRAPPED);
  /* Gutter visibility status for each of the four positions. */
WINDOW_SLOT_ARRAY(gutter_visible_p, 4, EQUAL_WRAPPED);
  /* The following five don't really need to be cached except
     that we need to know when they've changed. */
WINDOW_SLOT(default_gutter, EQUAL_WRAPPED);
WINDOW_SLOT(default_gutter_width, EQ);
WINDOW_SLOT(default_gutter_height, EQ);
WINDOW_SLOT(default_gutter_visible_p, EQ);
WINDOW_SLOT(default_gutter_border_width, EQ);
/* margins */
WINDOW_SLOT(left_margin_width, EQ);
WINDOW_SLOT(right_margin_width, EQ);
WINDOW_SLOT(minimum_line_ascent, EQ);
WINDOW_SLOT(minimum_line_descent, EQ);
WINDOW_SLOT(use_left_overflow, EQ);
WINDOW_SLOT(use_right_overflow, EQ);
#ifdef HAVE_MENUBARS
  /* Visibility of menubar. */
WINDOW_SLOT(menubar_visible_p, EQ);
#endif				/* HAVE_MENUBARS */
WINDOW_SLOT(text_cursor_visible_p, EQ);

  /* Hara-kiri */
#undef EQUAL_WRAPPED
#undef WINDOW_SLOT_DECLARATION
#undef WINDOW_SLOT
#undef WINDOW_SLOT_ARRAY
