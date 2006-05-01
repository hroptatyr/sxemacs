;;; gdk.el --- Import GDK functions into XEmacs

;; Copyright (C) 2000 Free Software Foundation

;; Maintainer: William Perry <wmperry@gnu.org>
;; Keywords: extensions, dumped

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; This file is dumped with XEmacs.

(eval-and-compile
  (require 'gtk-ffi))

(gtk-import-function nil gdk_set_show_events (gboolean . show_events))
(gtk-import-function nil gdk_set_use_xshm (gboolean . use_xshm))
(gtk-import-function GtkString gdk_get_display)
(gtk-import-function nil gdk_flush)
(gtk-import-function nil gdk_beep)

(gtk-import-function nil gdk_key_repeat_disable)
(gtk-import-function nil gdk_key_repeat_restore)

(gtk-import-function gint gdk_visual_get_best_depth)
(gtk-import-function GdkVisualType gdk_visual_get_best_type)
(gtk-import-function GdkVisual gdk_visual_get_system)
(gtk-import-function GdkVisual gdk_visual_get_best)
(gtk-import-function GdkVisual gdk_visual_get_best_with_depth (gint . depth))
(gtk-import-function GdkVisual gdk_visual_get_best_with_type (GdkVisualType . visual_type))
(gtk-import-function GdkVisual gdk_visual_get_best_with_both
		     (gint . depth)
		     (GdkVisualType . visual_type))

(gtk-import-function gboolean gdk_window_is_visible (GdkWindow . window))
(gtk-import-function gboolean gdk_window_is_viewable (GdkWindow . window))

(gtk-import-function gboolean gdk_window_set_static_gravities
		     (GdkWindow . window)
		     (gboolean  . use_static))

(gtk-import-function nil gdk_window_set_cursor
		     (GdkWindow	. window)
		     (GdkCursor . cursor))

(gtk-import-function GdkVisual gdk_window_get_visual (GdkWindow . window))
(gtk-import-function GdkWindowType gdk_window_get_type (GdkWindow . window))
(gtk-import-function GdkWindow gdk_window_get_parent (GdkWindow . window))
(gtk-import-function GdkWindow gdk_window_get_toplevel (GdkWindow . window))
(gtk-import-function GdkEventMask gdk_window_get_events (GdkWindow . window))
(gtk-import-function none gdk_window_set_events (GdkWindow . window) (GdkEventMask . events))
(gtk-import-function none gdk_window_set_icon
		     (GdkWindow . window)
		     (GdkWindow . icon_window)
		     (GdkPixmap . pixmap)
		     (GdkBitmap . mask))
(gtk-import-function none gdk_window_set_icon_name (GdkWindow . window) (GtkString . name))
(gtk-import-function none gdk_window_set_group (GdkWindow . window) (GdkWindow . leader))
(gtk-import-function none gdk_window_set_decorations
		     (GdkWindow . window)
		     (GdkWMDecoration . decorations))
(gtk-import-function none gdk_window_set_functions
		     (GdkWindow . window)
		     (GdkWMFunction . functions))

;; Cursors are handled by glyphs in XEmacs
;; GCs are handled by faces in XEmacs
;; Pixmaps are handled by glyphs in XEmacs
;; Images are handled by glyphs in XEmacs
;; Colors are handled natively by XEmacs
;; Fonts are handled natively by XEmacs

(gtk-import-function none gdk_draw_point
		     (GdkDrawable . drawable)
		     (GdkGC . gc)
		     (gint . x)
		     (gint . y))
(gtk-import-function none gdk_draw_line
		     (GdkDrawable . drawable)
		     (GdkGC . gc)
		     (gint . x1)
		     (gint . y1)
		     (gint . x2)
		     (gint . y2))
(gtk-import-function none gdk_draw_rectangle
		     (GdkDrawable . drawable)
		     (GdkGC . gc)
		     (gboolean . filled)
		     (gint . x)
		     (gint . y)
		     (gint . width)
		     (gint . height))
(gtk-import-function none gdk_draw_arc
		     (GdkDrawable . drawable)
		     (GdkGC . gc)
		     (gboolean . filled)
		     (gint . x)
		     (gint . y)
		     (gint . width)
		     (gint . height)
		     (gint . angle1)
		     (gint . angle2))
(gtk-import-function none gdk_draw_string
		     (GdkDrawable . drawable)
		     (GdkFont     . font)
		     (GdkGC       . gc)
		     (gint        . x)
		     (gint        . y)
		     (GtkString   . string))
(gtk-import-function none gdk_draw_text
		     (GdkDrawable . drawable)
		     (GdkFont     . font)
		     (GdkGC       . gc)
		     (gint        . x)
		     (gint        . y)
		     (GtkString   . string)
		     (gint        . text_length))
(gtk-import-function none gdk_draw_pixmap
		     (GdkDrawable . drawable)
		     (GdkGC       . gc)
		     (GdkImage    . image)
		     (gint        . xsrc)
		     (gint        . ysrc)
		     (gint        . xdest)
		     (gint        . ydest)
		     (gint        . width)
		     (gint        . height))

;; Selections are handled natively by XEmacs

(provide 'gdk)
