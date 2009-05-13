;;; gtk-mouse.el --- Mouse support for GTK window system.

;; Copyright (C) 1985, 1992-4, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996 Ben Wing.
;; Copyright (C) 2000 William Perry

;; Maintainer: SXEmacs Development Team
;; Keywords: mouse, dumped

;; This file is part of SXEmacs.

;; SXEmacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; SXEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Synched up with: Not synched.

;;; Commentary:

;; This file is dumped with SXEmacs (when GTK support is compiled in).

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
