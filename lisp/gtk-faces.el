;;; gtk-faces.el --- GTK-specific face frobnication, aka black magic.

;; Copyright (C) 1992-4, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996 Ben Wing.
;; Copyright (c) 2000 William Perry

;; Author: William M. Perry <wmperry@gnu.org>
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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not synched.

;;; Commentary:

;; This file is dumped with XEmacs (when GTK support is compiled in).


(defun gtk-init-find-device ()
  (let ((dev nil)
	(devices (device-list)))
    (while (and (not dev) devices)
      (if (eq (device-type (car devices)) 'gtk)
	  (setq dev (car devices)))
      (setq devices (cdr devices)))
    dev))

;;; gtk-init-device-faces is responsible for initializing default
;;; values for faces on a newly created device.
;;;
(defun gtk-init-device-faces (device)
  ;;
  ;; If the "default" face didn't have a font specified, try to pick one.
  ;;
  (if (not (eq (device-type device) 'gtk))
      nil
    (gtk-init-pointers)
    (let* ((style (gtk-style-info device))
	   ;;(normal 0)			; GTK_STATE_NORMAL
	   ;;(active 1)			; GTK_STATE_ACTIVE
	   (prelight 2)			; GTK_STATE_PRELIGHT
	   (selected 3)			; GTK_STATE_SELECTED
	   ;;(insensitive 4)		; GTK_STATE_INSENSITIVE
	   )
      (set-face-foreground 'highlight
			   (nth prelight (plist-get style 'text))
			   device)
      (set-face-background 'highlight
			   (nth prelight (plist-get style 'background))
			   device)
      (set-face-foreground 'zmacs-region
			   (nth selected (plist-get style 'text))
			   device)
      (set-face-background 'zmacs-region
			   (nth selected (plist-get style 'background))
			   device))
    (set-face-background 'text-cursor "red3" device)))

;;; This is called from `init-frame-faces', which is called from
;;; init_frame_faces() which is called from Fmake_frame(), to perform
;;; any device-specific initialization.
;;;
(defun gtk-init-frame-faces (frame)
  )

;;; gtk-init-global-faces is responsible for ensuring that the
;;; default face has some reasonable fallbacks if nothing else is
;;; specified.
;;;
(defun gtk-init-global-faces ()
  (let* ((dev (gtk-init-find-device))
	 (default-font (or (face-font 'default 'global)
			   ;(plist-get (gtk-style-info dev) 'font)
			   "-*-courier-medium-r-*-*-*-120-*-*-*-*-iso8859-*"))
	 (italic-font (or (gtk-make-font-italic default-font dev) default-font))
	 (bold-font (or (gtk-make-font-bold default-font dev) default-font))
	 (bi-font (or (gtk-make-font-bold-italic default-font dev) default-font)))

    (or (face-font 'default 'global)
	(set-face-font 'default default-font 'global '(gtk default)))

    (or (face-font 'bold 'global)
	(set-face-font 'bold bold-font 'global '(gtk default)))

    (or (face-font 'bold-italic 'global)
	(set-face-font 'bold-italic bi-font 'global '(gtk default)))

    (or (face-font 'italic 'global)
	(set-face-font 'italic italic-font 'global '(gtk default)))))


;;; Lots of this stolen from x-faces.el - I'm not sure if this will
;;; require a rewrite for win32 or not?
(defconst gtk-font-regexp nil)
(defconst gtk-font-regexp-head nil)
(defconst gtk-font-regexp-head-2 nil)
(defconst gtk-font-regexp-weight nil)
(defconst gtk-font-regexp-slant nil)
(defconst gtk-font-regexp-pixel nil)
(defconst gtk-font-regexp-point nil)
(defconst gtk-font-regexp-foundry-and-family nil)
(defconst gtk-font-regexp-registry-and-encoding nil)
(defconst gtk-font-regexp-spacing nil)

;;; Regexps matching font names in "Host Portable Character Representation."
;;;
(let ((- 		"[-?]")
      (foundry		"[^-]*")
      (family 		"[^-]*")
      (weight		"\\(bold\\|demibold\\|medium\\|black\\)")	; 1
;     (weight\?		"\\(\\*\\|bold\\|demibold\\|medium\\|\\)")	; 1
      (weight\?		"\\([^-]*\\)")					; 1
      (slant		"\\([ior]\\)")					; 2
;     (slant\?		"\\([ior?*]?\\)")				; 2
      (slant\?		"\\([^-]?\\)")					; 2
;     (swidth		"\\(\\*\\|normal\\|semicondensed\\|\\)")	; 3
      (swidth		"\\([^-]*\\)")					; 3
;     (adstyle		"\\(\\*\\|sans\\|\\)")				; 4
      (adstyle		"\\([^-]*\\)")					; 4
      (pixelsize	"\\(\\*\\|[0-9]+\\)")				; 5
      (pointsize	"\\(\\*\\|0\\|[0-9][0-9]+\\)")			; 6
;      (resx		"\\(\\*\\|[0-9][0-9]+\\)")			; 7
;      (resy		"\\(\\*\\|[0-9][0-9]+\\)")			; 8
      (resx		"\\([*0]\\|[0-9][0-9]+\\)")			; 7
      (resy		"\\([*0]\\|[0-9][0-9]+\\)")			; 8
      (spacing		"[cmp?*]")
      (avgwidth		"\\(\\*\\|[0-9]+\\)")				; 9
      (registry		"[^-]*") ; some fonts have omitted registries
;      (encoding	".+")		; note that encoding may contain "-"...
      (encoding	"[^-]+")		; false!
      )
  (setq gtk-font-regexp
	(purecopy
	 (concat "\\`\\*?[-?*]"
		 foundry - family - weight\? - slant\? - swidth - adstyle -
		 pixelsize - pointsize - resx - resy - spacing - avgwidth -
		 registry - encoding "\\'"
		 )))
  (setq gtk-font-regexp-head
	(purecopy
          (concat "\\`[-?*]" foundry - family - weight\? - slant\?
		  "\\([-*?]\\|\\'\\)")))
  (setq gtk-font-regexp-head-2
	(purecopy
          (concat "\\`[-?*]" foundry - family - weight\? - slant\?
		  - swidth - adstyle - pixelsize - pointsize
		  "\\([-*?]\\|\\'\\)")))
  (setq gtk-font-regexp-slant (purecopy (concat - slant -)))
  (setq gtk-font-regexp-weight (purecopy (concat - weight -)))
  ;; if we can't match any of the more specific regexps (unfortunate) then
  ;; look for digits; assume 2+ digits is 10ths of points, and 1-2 digits
  ;; is pixels.  Bogus as hell.
  (setq gtk-font-regexp-pixel (purecopy "[-?*]\\([0-9][0-9]?\\)[-?*]"))
  (setq gtk-font-regexp-point (purecopy "[-?*]\\([0-9][0-9]+\\)[-?*]"))
  ;; the following two are used by x-font-menu.el.
  (setq gtk-font-regexp-foundry-and-family
	(purecopy (concat "\\`[-?*]" foundry - "\\(" family "\\)" -)))
  (setq gtk-font-regexp-registry-and-encoding
	(purecopy (concat - "\\(" registry "\\)" - "\\(" encoding "\\)\\'")))
  (setq gtk-font-regexp-spacing
	(purecopy (concat - "\\(" spacing "\\)" - avgwidth
			  - registry - encoding "\\'")))
  )

(defvaralias 'x-font-regexp 'gtk-font-regexp)
(defvaralias 'x-font-regexp-head 'gtk-font-regexp-head)
(defvaralias 'x-font-regexp-head-2 'gtk-font-regexp-head-2)
(defvaralias 'x-font-regexp-weight 'gtk-font-regexp-weight)
(defvaralias 'x-font-regexp-slant 'gtk-font-regexp-slant)
(defvaralias 'x-font-regexp-pixel 'gtk-font-regexp-pixel)
(defvaralias 'x-font-regexp-point 'gtk-font-regexp-point)
(defvaralias 'x-font-regexp-foundry-and-family 'gtk-font-regexp-foundry-and-family)
(defvaralias 'x-font-regexp-registry-and-encoding 'gtk-font-regexp-registry-and-encoding)
(defvaralias 'x-font-regexp-spacing 'gtk-font-regexp-spacing)

(defun gtk-frob-font-weight (font which)
  (if (font-instance-p font) (setq font (font-instance-name font)))
  (cond ((null font) nil)
	((or (string-match gtk-font-regexp font)
	     (string-match gtk-font-regexp-head font)
	     (string-match gtk-font-regexp-weight font))
	 (concat (substring font 0 (match-beginning 1)) which
		 (substring font (match-end 1))))
	(t nil)))

(defun gtk-frob-font-slant (font which)
  (if (font-instance-p font) (setq font (font-instance-name font)))
  (cond ((null font) nil)
	((or (string-match gtk-font-regexp font)
	     (string-match gtk-font-regexp-head font))
	 (concat (substring font 0 (match-beginning 2)) which
		 (substring font (match-end 2))))
	((string-match gtk-font-regexp-slant font)
	 (concat (substring font 0 (match-beginning 1)) which
		 (substring font (match-end 1))))
	(t nil)))

(defun gtk-make-font-bold (font &optional device)
  (or (try-font-name (gtk-frob-font-weight font "bold") device)
      (try-font-name (gtk-frob-font-weight font "black") device)
      (try-font-name (gtk-frob-font-weight font "demibold") device)))

(defun gtk-make-font-unbold (font &optional device)
  (try-font-name (gtk-frob-font-weight font "medium") device))

(defcustom *try-oblique-before-italic-fonts* t
  "*If nil, italic fonts are searched before oblique fonts.
If non-nil, oblique fonts are tried before italic fonts.  This is mostly
applicable to adobe-courier fonts"
  :type 'boolean
  :tag "Try Oblique Before Italic Fonts"
  :group 'x)

(defun gtk-make-font-italic (font &optional device)
  (if *try-oblique-before-italic-fonts*
      (or (try-font-name (gtk-frob-font-slant font "o") device)
	  (try-font-name (gtk-frob-font-slant font "i") device))
    (or (try-font-name (gtk-frob-font-slant font "i") device)
	(try-font-name (gtk-frob-font-slant font "o") device))))

(defun gtk-make-font-unitalic (font &optional device)
  (try-font-name (gtk-frob-font-slant font "r") device))

(defun gtk-make-font-bold-italic (font &optional device)
  (if *try-oblique-before-italic-fonts*
      (or (try-font-name
	   (gtk-frob-font-slant (gtk-frob-font-weight font "bold") "o") device)
	  (try-font-name
	   (gtk-frob-font-slant (gtk-frob-font-weight font "bold") "i") device)
	  (try-font-name
	   (gtk-frob-font-slant (gtk-frob-font-weight font "black") "o") device)
	  (try-font-name
	   (gtk-frob-font-slant (gtk-frob-font-weight font "black") "i") device)
	  (try-font-name
	   (gtk-frob-font-slant (gtk-frob-font-weight font "demibold") "o") device)
	  (try-font-name
	   (gtk-frob-font-slant (gtk-frob-font-weight font "demibold") "i") device))
    (or (try-font-name
	 (gtk-frob-font-slant (gtk-frob-font-weight font "bold") "i") device)
	(try-font-name
	 (gtk-frob-font-slant (gtk-frob-font-weight font "bold") "o") device)
	(try-font-name
	 (gtk-frob-font-slant (gtk-frob-font-weight font "black") "i") device)
	(try-font-name
	 (gtk-frob-font-slant (gtk-frob-font-weight font "black") "o") device)
	(try-font-name
	 (gtk-frob-font-slant (gtk-frob-font-weight font "demibold") "i") device)
	(try-font-name
	 (gtk-frob-font-slant (gtk-frob-font-weight font "demibold") "o") device))))

(defun gtk-choose-font ()
  (interactive)
  (require 'x-font-menu)
  (require 'font)
  (let ((locale (if font-menu-this-frame-only-p
		    (selected-frame)
		  nil))
	(dialog nil))
    (setq dialog (gtk-font-selection-dialog-new "Choose default font..."))
    (put dialog 'modal t)
    (put dialog 'type 'dialog)

    (gtk-widget-set-sensitive (gtk-font-selection-dialog-apply-button dialog) nil)
    (gtk-signal-connect dialog 'destroy (lambda (&rest ignored) (gtk-main-quit)))
    (gtk-signal-connect (gtk-font-selection-dialog-ok-button dialog)
			'clicked
			(lambda (button data)
			  (let* ((dialog (car data))
				 (locale (cdr data))
				 (font (font-create-object
					(gtk-font-selection-dialog-get-font-name dialog))))
			    (gtk-widget-destroy dialog)
			    (font-menu-set-font (car (font-family font)) nil (* 10 (font-size font)))))
			(cons dialog locale))
    (gtk-signal-connect (gtk-font-selection-dialog-cancel-button dialog)
			'clicked
			(lambda (button dialog)
			  (gtk-widget-destroy dialog)) dialog)

    (gtk-widget-show-all dialog)
    (gtk-main)))
