;;; msw-glyphs.el --- Support for glyphs in ms windows

;; Copyright (C) 1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 2002 Ben Wing.

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

;; Initialization code for MS Windows glyphs.

;; This file is dumped with XEmacs (when MS Windows support is
;; compiled in). Make sure this is the first of msw-*.el files
;; dumped.

;;; Code:

(defun msgdi-device-p (&optional device)
  "Return non-nil if DEVICE is a GDI device, that is 'mswindows or 'msprinter.
MS GDI devices are mutuially WYSIWIG-compatible, so that many common glyph,
color and font properties apply to them equally.

This function is also a predicate for 'msgdi device tag, matching this
device class."
  (memq (device-type device) '(mswindows msprinter)))

(progn

  (define-specifier-tag 'msgdi (function msgdi-device-p))

  (set-console-type-image-conversion-list
   'mswindows
   `(("\\.bmp\\'" [bmp :file nil] 2)
     ("\\`BM" [bmp :data nil] 2)
     ,@(if (featurep 'xpm) '(("\\.xpm\\'" [xpm :file nil] 2)))
     ("\\.xbm\\'" [xbm :file nil] 2)
     ,@(if (featurep 'xpm) '(("\\`/\\* XPM \\*/" [xpm :data nil] 2)))
     ,@(if (featurep 'gif) '(("\\.gif\\'" [gif :file nil] 2)
			     ("\\`GIF8[79]" [gif :data nil] 2)))
     ,@(if (featurep 'jpeg) '(("\\.jpe?g\\'" [jpeg :file nil] 2)))
     ;; all of the JFIF-format JPEG's that I've seen begin with
     ;; the following.  I have no idea if this is standard.
     ,@(if (featurep 'jpeg) '(("\\`\377\330\377\340\000\020JFIF"
			       [jpeg :data nil] 2)))
     ,@(if (featurep 'png) '(("\\.png\\'" [png :file nil] 2)))
     ,@(if (featurep 'png) '(("\\`\211PNG" [png :data nil] 2)))
     ,@(if (featurep 'tiff) '(("\\.tif?f\\'" [tiff :file nil] 2)))
     ("\\`X-Face:" [string :data "[xface]"])
     ("\\`/\\* XPM \\*/" [string :data "[xpm]"])
     ("" [string :data nil] 2)
     ;; this last one is here for pointers and icons and such --
     ;; strings are not allowed so they will be ignored.
     ("" [nothing])))

  (set-console-type-image-conversion-list
   'msprinter (console-type-image-conversion-list 'mswindows))

  (set-face-font 'border-glyph "WingDings:Regular:11::Symbol"
		 'global 'msgdi)
  (set-glyph-image continuation-glyph "\xC3" 'global 'msgdi)
  (set-glyph-image truncation-glyph "\xF0" 'global 'msgdi)
  (set-glyph-image hscroll-glyph "\xEF" 'global 'msgdi)
  (set-glyph-contrib-p continuation-glyph nil)
  (set-glyph-contrib-p truncation-glyph nil)
  (set-glyph-contrib-p hscroll-glyph nil)

  (set-glyph-image octal-escape-glyph "\\")
  (set-glyph-image control-arrow-glyph "^")
  (set-glyph-image invisible-text-glyph " ...")

  (cond ((featurep 'xpm)
	 (set-glyph-image frame-icon-glyph
			  (concat "../etc/" "xemacs-icon3.xpm")
			  'global 'mswindows)
	 (set-glyph-image xemacs-logo
			  (concat "../etc/"
				  (if emacs-beta-version
				      "xemacs-beta.xpm"
				    "xemacs.xpm"))
			  'global 'msgdi))
	(t
	 (set-glyph-image xemacs-logo
			  "XEmacs <insert spiffy graphic logo here>"
			  'global 'msgdi)))
)

;;; msw-glyphs.el ends here
