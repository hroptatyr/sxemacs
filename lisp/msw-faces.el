;;; msw-faces.el --- mswindows-specific face stuff.

;;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
;;; Copyright (C) 1995, 1996 Ben Wing.

;; Author: Jamie Zawinski
;; Modified by:  Chuck Thompson
;; Modified by:  Ben Wing
;; Modified by:  Martin Buchholz
;; Rewritten for mswindows by:  Jonathan Harris

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

;; This file does the magic to parse mswindows font names, and make sure that
;; the default and modeline attributes of new frames are specified enough.

(defun mswindows-init-device-faces (device)
  (let ((color-default (device-system-metric device 'color-default))
	(color-3d-face (device-system-metric device 'color-3d-face)))
    ; Force creation of the default face font so that if it fails we get
    ; an error now instead of a crash at frame creation.
    (unless (face-font-instance 'default device)
      (error "Can't find a suitable default font"))
    
    (if (car color-default)
	(set-face-foreground 'default (car color-default)) device)
    (if (cdr color-default)
	(set-face-background 'default (cdr color-default)) device)
    (if (car color-3d-face)
	(set-face-foreground 'gui-element (car color-3d-face)) device)
    (if (cdr color-3d-face)
	(set-face-background 'gui-element (cdr color-3d-face)) device)
    (set-face-font 'gui-element "MS Sans Serif:Regular:8" device)))

(defun mswindows-init-frame-faces (frame)
  )

;; Other functions expect these regexps
(defconst mswindows-font-regexp
  (let
      ((- 		":")
       (fontname	"\\([a-zA-Z ]+\\)")
       (weight	"\\([a-zA-Z]*\\)?")
       (style	"\\( [a-zA-Z]*\\)?")
       (pointsize	"\\([0-9]+\\)?")
       (effects	"\\([a-zA-Z ]*\\)?")
       (charset	"\\([a-zA-Z 0-9]*\\)")
       )
    (concat "^"
	    fontname - weight style - pointsize - effects - charset "$")))

;;; Fill in missing parts of a font spec. This is primarily intended as a
;;; helper function for the functions below.
;;; mswindows fonts look like:
;;;	fontname[:[weight][ style][:pointsize[:effects]]][:charset]
;;; A minimal mswindows font spec looks like:
;;;	Courier New
;;; A maximal mswindows font spec looks like:
;;;	Courier New:Bold Italic:10:underline strikeout:Western
;;; Missing parts of the font spec should be filled in with these values:
;;;	Courier New:Regular:10::Western
(defun mswindows-font-canonicalize-name (font)
  "Given a mswindows font or font name, this returns its name in
canonical form."
  (if (or (font-instance-p font)
	  (stringp font))
      (let ((name (if (font-instance-p font) 
		      (font-instance-name font)
		    font)))
	(cond ((string-match
		"^[a-zA-Z ]+:[a-zA-Z ]*:[0-9]+:[a-zA-Z ]*:[a-zA-Z 0-9]*$"
		name) name)
	      ((string-match "^[a-zA-Z ]+:[a-zA-Z ]*:[0-9]+:[a-zA-Z ]*$"
			     name) (concat name ":Western"))
	      ((string-match "^[a-zA-Z ]+:[a-zA-Z ]*:[0-9]+$" name)
	       (concat name "::Western"))
	      ((string-match "^[a-zA-Z ]+:[a-zA-Z ]*$" name)
	       (concat name ":10::Western"))
	      ((string-match "^[a-zA-Z ]+$" name)
	       (concat name ":Regular:10::Western"))
	      (t "Courier New:Regular:10::Western")))))

(defun mswindows-make-font-bold (font &optional device)
  "Given a mswindows font specification, this attempts to make a bold font.
If it fails, it returns nil."
  (if (font-instance-p font)
      (let ((name (mswindows-font-canonicalize-name font))
	    (oldwidth (font-instance-width font)))
	(string-match "^[a-zA-Z ]+:\\([a-zA-Z ]*\\):" name)
	(let ((newfont (make-font-instance
			(concat (substring name 0 (match-beginning 1))
				"Bold" (substring name (match-end 1)))
		       device t)))
; Hack! on mswindows, bold fonts (even monospaced) are often wider than the
; equivalent non-bold font. Making the bold font one point smaller usually
; makes it the same width (maybe at the expense of making it one pixel shorter)
	  (if (font-instance-p newfont)
	      (if (> (font-instance-width newfont) oldwidth)
		  (mswindows-find-smaller-font newfont device)
		newfont))))))

(defun mswindows-make-font-unbold (font &optional device)
  "Given a mswindows font specification, this attempts to make a non-bold font.
If it fails, it returns nil."
  (if (font-instance-p font)
      (let ((name (mswindows-font-canonicalize-name font)))
	(string-match "^[a-zA-Z ]+:\\([a-zA-Z ]*\\):" name)
	(make-font-instance (concat
			     (substring name 0 (match-beginning 1))
			     "Regular" (substring name (match-end 1)))
			    device t))))

(defun mswindows-make-font-italic (font &optional device)
  "Given a mswindows font specification, this attempts to make an `italic'
font. If it fails, it returns nil."
  (if (font-instance-p font)
      (let ((name (mswindows-font-canonicalize-name font)))
	(string-match "^[a-zA-Z ]+:\\([a-zA-Z ]*\\):" name)
	(make-font-instance (concat
			     (substring name 0 (match-beginning 1))
			     "Italic" (substring name (match-end 1)))
			    device t))))

(defun mswindows-make-font-unitalic (font &optional device)
  "Given a mswindows font specification, this attempts to make a non-italic
font. If it fails, it returns nil."
  (if (font-instance-p font)
      (let ((name (mswindows-font-canonicalize-name font)))
	(string-match "^[a-zA-Z ]+:\\([a-zA-Z ]*\\):" name)
	(make-font-instance (concat
			     (substring name 0 (match-beginning 1))
			     "Regular" (substring name (match-end 1)))
			    device t))))

(defun mswindows-make-font-bold-italic (font &optional device)
  "Given a mswindows font specification, this attempts to make a `bold-italic'
font. If it fails, it returns nil."
  (if (font-instance-p font)
      (let ((name (mswindows-font-canonicalize-name font))
	    (oldwidth (font-instance-width font)))
	(string-match "^[a-zA-Z ]+:\\([a-zA-Z ]*\\):" name)
	(let ((newfont (make-font-instance
			(concat (substring name 0 (match-beginning 1))
				"Bold Italic" (substring name (match-end 1)))
		       device t)))
; Hack! on mswindows, bold fonts (even monospaced) are often wider than the
; equivalent non-bold font. Making the bold font one point smaller usually
; makes it the same width (maybe at the expense of making it one pixel shorter)
	  (if (font-instance-p newfont)
	      (if (> (font-instance-width newfont) oldwidth)
		  (mswindows-find-smaller-font newfont device)
		newfont))))))

(defun mswindows-find-smaller-font (font &optional device)
  "Loads a new version of the given font (or font name) 1 point smaller.
Returns the font if it succeeds, nil otherwise."
  (if (stringp font) (setq font (make-font-instance font device)))
  (if (font-instance-p font) (setq font (font-instance-truename font)))
  (if (stringp font) (setq font (make-font-instance font device)))
  (if (font-instance-p font)
      (let (old-size (name (mswindows-font-canonicalize-name font)))
	(string-match "^[a-zA-Z ]+:[a-zA-Z ]*:\\([0-9]+\\):" name)
	(setq old-size (string-to-int
			(substring name (match-beginning 1) (match-end 1))))
	(if (> old-size 0)
	    (make-font-instance (concat
				 (substring name 0 (match-beginning 1))
				 (int-to-string (- old-size 1))
				 (substring name (match-end 1)))
				device t)))))

(defun mswindows-find-larger-font (font &optional device)
  "Loads a new version of the given font (or font name) 1 point larger.
Returns the font if it succeeds, nil otherwise."
  (if (stringp font) (setq font (make-font-instance font device)))
  (if (font-instance-p font) (setq font (font-instance-truename font)))
  (if (stringp font) (setq font (make-font-instance font device)))
  (if (font-instance-p font)
      (let (old-size (name (mswindows-font-canonicalize-name font)))
	(string-match "^[a-zA-Z ]+:[a-zA-Z ]*:\\([0-9]+\\):" name)
	(setq old-size (string-to-int
			(substring name (match-beginning 1) (match-end 1))))
	(make-font-instance (concat
			     (substring name 0 (match-beginning 1))
			     (int-to-string (+ old-size 1))
			     (substring name (match-end 1)))
			    device t))))
