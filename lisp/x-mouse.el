;;; x-mouse.el --- Mouse support for X window system.

;; Copyright (C) 1985, 1992-4, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996 Ben Wing.

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

;; This file is dumped with SXEmacs (when X support is compiled in).

;;; Code:

(globally-declare-fboundp
 '(x-store-cutbuffer x-get-resource))

;;(define-key global-map 'button2 'x-set-point-and-insert-selection)
;; This is reserved for use by Hyperbole.
;;(define-key global-map '(shift button2) 'x-mouse-kill)
(define-key global-map '(control button2) 'x-set-point-and-move-selection)

(define-obsolete-function-alias 'x-insert-selection 'insert-selection)

(defun x-mouse-kill (event)
  "Kill the text between the point and mouse and copy it to the clipboard and
to the cut buffer."
  (interactive "@e")
  (let ((old-point (point)))
    (mouse-set-point event)
    (let ((s (buffer-substring old-point (point))))
      (own-clipboard s)
      (x-store-cutbuffer s))
    (kill-region old-point (point))))

(make-obsolete 'x-set-point-and-insert-selection 'mouse-yank)
(defun x-set-point-and-insert-selection (event)
  "Set point where clicked and insert the primary selection or the cut buffer."
  (interactive "e")
  (let ((mouse-yank-at-point nil))
    (mouse-yank event)))

(defun x-set-point-and-move-selection (event)
  "Set point where clicked and move the selected text to that location."
  (interactive "e")
  ;; Don't try to move the selection if x-kill-primary-selection if going
  ;; to fail; just let the appropriate error message get issued. (We need
  ;; to insert the selection and set point first, or the selection may
  ;; get inserted at the wrong place.)
  (and (selection-owner-p)
       primary-selection-extent
       (insert-selection t event))
  (kill-primary-selection))

(defun mouse-track-and-copy-to-cutbuffer (event)
  "Make a selection like `mouse-track', but also copy it to the cutbuffer."
  (interactive "e")
  (mouse-track event)
  (cond
   ((null primary-selection-extent)
    nil)
   ((consp primary-selection-extent)
    (save-excursion
      (set-buffer (extent-object (car primary-selection-extent)))
      (x-store-cutbuffer
       (mapconcat
	#'identity
	(declare-fboundp
	 (extract-rectangle
	  (extent-start-position (car primary-selection-extent))
	  (extent-end-position (car (reverse primary-selection-extent)))))
	"\n"))))
   (t
    (save-excursion
      (set-buffer (extent-object primary-selection-extent))
      (x-store-cutbuffer
       (buffer-substring (extent-start-position primary-selection-extent)
			 (extent-end-position primary-selection-extent)))))))


(defvar x-pointers-initialized nil)

(defun x-init-pointer-shape (device)
  "Initialize the mouse-pointers of DEVICE from the X resource database."
  (if x-pointers-initialized  ; only do it when the first device is created
      nil
    (set-glyph-image text-pointer-glyph
	  (or (x-get-resource "textPointer" "Cursor" 'string device nil 'warn)
	      [cursor-font :data "xterm"]))
    (set-glyph-image selection-pointer-glyph
	  (or (x-get-resource "selectionPointer" "Cursor" 'string device
			      nil 'warn)
	      [cursor-font :data "top_left_arrow"]))
    (set-glyph-image nontext-pointer-glyph
	  (or (x-get-resource "spacePointer" "Cursor" 'string device nil 'warn)
	      [cursor-font :data "xterm"])) ; was "crosshair"
    (set-glyph-image modeline-pointer-glyph
	  (or (x-get-resource "modeLinePointer" "Cursor" 'string device
			      nil 'warn)
;;	      "fleur"))
	      [cursor-font :data "sb_v_double_arrow"]))
    (set-glyph-image gc-pointer-glyph
	  (or (x-get-resource "gcPointer" "Cursor" 'string device nil 'warn)
	      [cursor-font :data "watch"]))
    (when (featurep 'scrollbar)
      (set-glyph-image
       scrollbar-pointer-glyph
       (or (x-get-resource "scrollbarPointer" "Cursor" 'string device
			   nil 'warn)
	   ;; bizarrely if we don't specify the specific locale (x) this
	   ;; gets instantiated on the stream device. Bad puppy.
	   [cursor-font :data "top_left_arrow"]) 'global '(default x)))
    (set-glyph-image busy-pointer-glyph
	  (or (x-get-resource "busyPointer" "Cursor" 'string device nil 'warn)
	      [cursor-font :data "watch"]))
    (set-glyph-image toolbar-pointer-glyph
	  (or (x-get-resource "toolBarPointer" "Cursor" 'string device
			      nil 'warn)
	      [cursor-font :data "left_ptr"]))
    (set-glyph-image divider-pointer-glyph
	  (or (x-get-resource "dividerPointer" "Cursor" 'string device
			      nil 'warn)
	      [cursor-font :data "sb_h_double_arrow"]))
    (let ((fg
	   (x-get-resource "pointerColor" "Foreground" 'string device
			   nil 'warn)))
      (and fg
	   (set-face-foreground 'pointer fg)))
    (let ((bg
	   (x-get-resource "pointerBackground" "Background" 'string device
			   nil 'warn)))
      (and bg
	   (set-face-background 'pointer bg)))
    (setq x-pointers-initialized t))
  nil)

;;; x-mouse.el ends here
