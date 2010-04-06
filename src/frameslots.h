/* Definitions of marked slots in frames
   Copyright (C) 1988, 1992, 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1996 Ben Wing.

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


/* Synched up with: FSF 19.30.  Split out of frame.h.  */

#ifdef FRAME_SLOT_DECLARATION
#define MARKED_SLOT_ARRAY(slot, size) MARKED_SLOT(slot[size])
#else
#define MARKED_SLOT_ARRAY(slot, size) do {		\
    int mslotidx;					\
    for (mslotidx = 0; mslotidx < size; mslotidx++)	\
      {							\
	MARKED_SLOT (slot[mslotidx]);			\
      }							\
  } while (0);
#endif

  /* device frame belongs to. */
MARKED_SLOT(device);

  /* Name of this frame: a Lisp string.
     NOT the same as the frame's title, even though FSF bogusly
     confuses the two.  The frame's name is used for resourcing
     and lookup purposes and is something you can count on having
     a specific value, while the frame's title may vary depending
     on the user's choice of `frame-title-format'. */
MARKED_SLOT(name);

  /* The frame which should receive keystrokes that occur in this
     frame, or nil if they should go to the frame itself.  This is
     usually nil, but if the frame is minibufferless, we can use this
     to redirect keystrokes to a surrogate minibuffer frame when
     needed.

     Note that a value of nil is different than having the field point
     to the frame itself.  Whenever the Fselect_frame function is used
     to shift from one frame to the other, any redirections to the
     original frame are shifted to the newly selected frame; if
     focus_frame is nil, Fselect_frame will leave it alone.  */
MARKED_SLOT(focus_frame);

  /* This frame's root window.  Every frame has one.
     If the frame has only a minibuffer window, this is it.
     Otherwise, if the frame has a minibuffer window, this is its sibling.  */
MARKED_SLOT(root_window);

  /* This frame's selected window.
     Each frame has its own window hierarchy
     and one of the windows in it is selected within the frame.
     The selected window of the selected frame is Emacs's selected window.  */
MARKED_SLOT(selected_window);

  /* This frame's minibuffer window.
     Most frames have their own minibuffer windows,
     but only the selected frame's minibuffer window
     can actually appear to exist.  */
MARKED_SLOT(minibuffer_window);

  /* The most recently selected nonminibuf window.
     This is used by things like the toolbar code, which doesn't
     want the toolbar to change when moving to the minibuffer.
     This will only be a minibuf window if we are a minibuf-only
     frame. */
MARKED_SLOT(last_nonminibuf_window);

  /* frame property list */
MARKED_SLOT(plist);

  /* buffer_alist at last redisplay. */
MARKED_SLOT(old_buffer_alist);

  /* A copy of the global Vbuffer_list, to maintain a per-frame buffer
     ordering.  The Vbuffer_list variable and the buffer_list slot of each
     frame contain exactly the same data, just in different orders.  */
MARKED_SLOT(buffer_alist);

  /* Predicate for selecting buffers for other-buffer.  */
MARKED_SLOT(buffer_predicate);

  /* The current mouse pointer for the frame.  This is set by calling
     `set-frame-pointer'. */
MARKED_SLOT(pointer);

  /* The current icon for the frame. */
MARKED_SLOT(icon);

#ifdef HAVE_MENUBARS
  /* Vector representing the menubar currently displayed.  See menubar-x.c. */
MARKED_SLOT(menubar_data);
#endif

  /* specifier values cached in the struct frame: */

#ifdef HAVE_MENUBARS
MARKED_SLOT(menubar_visible_p);
#endif

#ifdef HAVE_SCROLLBARS
  /* Width and height of the scrollbars. */
MARKED_SLOT(scrollbar_width);
MARKED_SLOT(scrollbar_height);
  /* Whether the scrollbars are visible */
MARKED_SLOT(horizontal_scrollbar_visible_p);
MARKED_SLOT(vertical_scrollbar_visible_p);
  /* Scrollbars location */
MARKED_SLOT(scrollbar_on_left_p);
MARKED_SLOT(scrollbar_on_top_p);
#endif

#ifdef HAVE_TOOLBARS
  /* The following three don't really need to be cached except
     that we need to know when they've changed. */
MARKED_SLOT(default_toolbar_width);
MARKED_SLOT(default_toolbar_height);
MARKED_SLOT(default_toolbar_visible_p);
MARKED_SLOT(default_toolbar_border_width);

  /* List of toolbar buttons of current toolbars */
MARKED_SLOT_ARRAY(toolbar_buttons, 4);
  /* Size of the toolbars.  The frame-local toolbar space is
     subtracted before the windows are arranged.  Window and buffer
     local toolbars overlay their windows. */
MARKED_SLOT_ARRAY(toolbar_size, 4);
  /* Visibility of the toolbars.  This acts as a valve for toolbar_size. */
MARKED_SLOT_ARRAY(toolbar_visible_p, 4);
  /* Thickness of the border around the toolbar. */
MARKED_SLOT_ARRAY(toolbar_border_width, 4);
#endif

/* Cache of subwindow instances for this frame */
MARKED_SLOT(subwindow_instance_cache);

  /* Possible frame-local default for outside margin widths. */
MARKED_SLOT(left_margin_width);
MARKED_SLOT(right_margin_width);

#undef MARKED_SLOT
#undef MARKED_SLOT_ARRAY
#undef FRAME_SLOT_DECLARATION
