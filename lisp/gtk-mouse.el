;;; gtk-mouse.el --- Mouse support for GTK window system.

;; Copyright (C) 1985, 1992-4, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996 Ben Wing.
;; Copyright (C) 2000 William Perry

;; Maintainer: XEmacs Development Team
;; Keywords: mouse, dumped

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
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not synched.

;;; Commentary:

;; This file is dumped with XEmacs (when GTK support is compiled in).

;;; Code:

(defvar gtk-pointers-initialized nil)

(defun gtk-init-pointers ()
  (if gtk-pointers-initialized
      nil
    (set-glyph-image text-pointer-glyph
		     [gtk-resource :resource-type cursor :resource-id xterm]
		     'gtk)
    (set-glyph-image nontext-pointer-glyph
		     [gtk-resource :resource-type cursor :resource-id xterm]
		     'gtk)
    (set-glyph-image selection-pointer-glyph
		     [gtk-resource :resource-type cursor :resource-id top-left-arrow]
		     'gtk)
    (set-glyph-image modeline-pointer-glyph
		     [gtk-resource :resource-type cursor :resource-id sb-v-double-arrow]
		     'gtk)
    (set-glyph-image divider-pointer-glyph
		     [gtk-resource :resource-type cursor :resource-id sb-h-double-arrow]
		     'gtk)
    (set-glyph-image busy-pointer-glyph
		     [gtk-resource :resource-type cursor :resource-id watch]
		     'gtk)
    (set-glyph-image gc-pointer-glyph
		     [gtk-resource :resource-type cursor :resource-id watch]
		     'gtk)

    (when (featurep 'toolbar)
      (set-glyph-image toolbar-pointer-glyph
		       [gtk-resource :resource-type cursor :resource-id top-left-arrow]
		       'gtk))

    (when (featurep 'scrollbar)
      (set-glyph-image scrollbar-pointer-glyph
		       [gtk-resource :resource-type cursor :resource-id top-left-arrow]
		       'gtk))

    (setq gtk-pointers-initialized t)))
