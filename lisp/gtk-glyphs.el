;;; gtk-glyphs.el --- Support for glyphs in Gtk

;; Copyright (C) 1994, 1997 Free Software Foundation, Inc.

;; Author: Kirill M. Katsnelson <kkm@kis.ru>
;; Maintainer: XEmacs Development Team
;; Keywords: extensions, internal, dumped

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file contains temporary definitions for 'gtk glyphs.
;; Since there currently is no image support, the glyps are defined
;; TTY-style. This file has to be removed or reworked completely
;; when we have images.

;; This file is dumped with SXEmacs.

;;; Code:

(progn
  (if (featurep 'gtk)
      (set-console-type-image-conversion-list
       'gtk
       `(,@(if (featurep 'xpm) '((#r"\.xpm\'" [xpm :file nil] 2)))
 	   (#r"\.xbm\'" [xbm :file nil] 2)
 	   ,@(if (featurep 'xpm) '((#r"\`/\* XPM \*/" [xpm :data nil] 2)))
 	   ,@(if (featurep 'xface) '(("\\`X-Face:" [xface :data nil] 2)))
 	   ,@(if (featurep 'gif) '((#r"\.gif\'" [gif :file nil] 2)
 				   ("\\`GIF8[79]" [gif :data nil] 2)))
 	   ,@(if (featurep 'jpeg) '((#r"\.jpe?g\'" [jpeg :file nil] 2)))
 	   ;; all of the JFIF-format JPEG's that I've seen begin with
 	   ;; the following.  I have no idea if this is standard.
 	   ,@(if (featurep 'jpeg) '(("\\`\377\330\377\340\000\020JFIF"
 				     [jpeg :data nil] 2)))
 	   ,@(if (featurep 'png) '((#r"\.png\'" [png :file nil] 2)))
 	   ,@(if (featurep 'png) '(("\\`\211PNG" [png :data nil] 2)))
 	   ("" [autodetect :data nil] 2))))
  (cond ((featurep 'xpm)
	 (set-glyph-image frame-icon-glyph
			  (concat "../etc/" "xemacs-icon3.xpm")
			  'global 'gtk)
	 (set-glyph-image sxemacs-logo
			  (concat "../etc/"
				  (if emacs-beta-version
				      "sxemacs-beta.xpm"
				    "sxemacs.xpm"))
			  'global 'gtk))
	(t
	 (set-glyph-image sxemacs-logo
			  "XEmacs <insert spiffy graphic logo here>"
			  'global 'gtk)))
  (set-glyph-image octal-escape-glyph "\\")
  (set-glyph-image control-arrow-glyph "^")
  (set-glyph-image invisible-text-glyph " ...")
  )

;;; gtk-glyphs.el ends here
