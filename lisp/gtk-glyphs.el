;;; gtk-glyphs.el --- Support for glyphs in Gtk

;; Copyright (C) 1994, 1997 Free Software Foundation, Inc.

;; Author: Kirill M. Katsnelson <kkm@kis.ru>
;; Maintainer: XEmacs Development Team
;; Keywords: extensions, internal, dumped

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
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file contains temporary definitions for 'gtk glyphs.
;; Since there currently is no image support, the glyps are defined
;; TTY-style. This file has to be removed or reworked completely
;; when we have images.

;; This file is dumped with XEmacs.

;;; Code:

(progn
  (if (featurep 'gtk)
      (set-console-type-image-conversion-list
       'gtk
       `(,@(if (featurep 'xpm) '(("\\.xpm\\'" [xpm :file nil] 2)))
 	   ("\\.xbm\\'" [xbm :file nil] 2)
 	   ,@(if (featurep 'xpm) '(("\\`/\\* XPM \\*/" [xpm :data nil] 2)))
 	   ,@(if (featurep 'xface) '(("\\`X-Face:" [xface :data nil] 2)))
 	   ,@(if (featurep 'gif) '(("\\.gif\\'" [gif :file nil] 2)
 				   ("\\`GIF8[79]" [gif :data nil] 2)))
 	   ,@(if (featurep 'jpeg) '(("\\.jpe?g\\'" [jpeg :file nil] 2)))
 	   ;; all of the JFIF-format JPEG's that I've seen begin with
 	   ;; the following.  I have no idea if this is standard.
 	   ,@(if (featurep 'jpeg) '(("\\`\377\330\377\340\000\020JFIF"
 				     [jpeg :data nil] 2)))
 	   ,@(if (featurep 'png) '(("\\.png\\'" [png :file nil] 2)))
 	   ,@(if (featurep 'png) '(("\\`\211PNG" [png :data nil] 2)))
 	   ("" [autodetect :data nil] 2))))
  (cond ((featurep 'xpm)
	 (set-glyph-image frame-icon-glyph
			  (concat "../etc/" "xemacs-icon3.xpm")
			  'global 'gtk)
	 (set-glyph-image xemacs-logo
			  (concat "../etc/"
				  (if emacs-beta-version
				      "xemacs-beta.xpm"
				    "xemacs.xpm"))
			  'global 'gtk))
	(t
	 (set-glyph-image xemacs-logo
			  "XEmacs <insert spiffy graphic logo here>"
			  'global 'gtk)))
  (set-glyph-image octal-escape-glyph "\\")
  (set-glyph-image control-arrow-glyph "^")
  (set-glyph-image invisible-text-glyph " ...")
  )

;;; gtk-glyphs.el ends here
