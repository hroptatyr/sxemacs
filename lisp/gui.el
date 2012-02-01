;;; gui.el --- Basic GUI functions for SXEmacs.

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) 1996 Ben Wing

;; Maintainer: SXEmacs Development Team
;; Keywords: internal, dumped

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

;;; Synched up with: Not in FSF

;;; Commentary:

;; This file is dumped with SXEmacs (when window system support is compiled in).

;;; Code:

(defcustom dialog-frame-plist '(width 60 height 20)
  "Plist of frame properties for initially creating a dialog frame.
Properties specified here supersede the values given in
`default-frame-plist'."
  :type 'plist
  :group 'frames)

(defun make-dialog-frame (&optional props parent)
  "Create a frame suitable for use as a dialog box.
The frame is made a child of PARENT (defaults to the selected frame),
and has additional properties PROPS, as well as `dialog-frame-plist'.
Normally it also has no modelines, menubars, or toolbars."
  (or parent (setq parent (selected-frame)))
  (let* ((ftop (frame-property parent 'top))
	 (fleft (frame-property parent 'left))
	 (fwidth (frame-pixel-width parent))
	 (fheight (frame-pixel-height parent))
	 (fonth (font-height (face-font 'default)))
	 (fontw (font-width (face-font 'default)))
	 (props (append props dialog-frame-plist))
	 (dfheight (plist-get props 'height))
	 (dfwidth (plist-get props 'width))
	 ;; under FVWM at least, if I don't specify the initial position,
	 ;; it ends up always at (0, 0).  xwininfo doesn't tell me
	 ;; that there are any program-specified position hints, so
	 ;; it must be an FVWM bug.  So just be smashing and position
	 ;; in the center of the selected frame.
	 (frame (make-frame
		 (append props
			 `(popup ,parent initially-unmapped t
				 menubar-visible-p nil
				 has-modeline-p nil
				 default-toolbar-visible-p nil
				 default-gutter-visible-p nil
				 modeline-shadow-thickness 0
				 left ,(+ fleft (- (/ fwidth 2)
						   (/ (* dfwidth fontw)
						      2)))
				 top ,(+ ftop (- (/ fheight 2)
						 (/ (* dfheight fonth)
						    2))))))))
    (set-face-foreground 'modeline [default foreground] frame)
    (set-face-background 'modeline [default background] frame)
    (make-frame-visible frame)
    frame))

(defvar gui-button-shadow-thickness 2)

(defun gui-button-p (object)
  "True if OBJECT is a GUI button."
  (and (vectorp object)
       (> (length object) 0)
       (eq 'button (aref object 0))))

(make-face 'gui-button-face "Face used for gui buttons")
(if (not (face-differs-from-default-p 'gui-button-face))
    (progn
      (set-face-reverse-p 'gui-button-face t)
      (set-face-background 'gui-button-face '(((x color) . "grey75")))
      (set-face-foreground 'gui-button-face '(((x color) . "black")))))


(defun gui-button-action (instance action user-data)
  (let ((domain (image-instance-domain instance)))
    (with-current-buffer (if (windowp domain)
			     (window-buffer domain) nil)
      (funcall action user-data))))

(defun make-gui-button (string &optional action user-data)
  "Make a GUI button whose label is STRING and whose action is ACTION.
If the button is inserted in a buffer and then clicked on, and ACTION
is non-nil, ACTION will be called with one argument, USER-DATA.
When ACTION is called, the buffer containing the button is made current."
  (vector 'button
	  :descriptor string
	  :face 'gui-button-face
	  :callback-ex `(lambda (image-instance event)
			  (gui-button-action image-instance
					     (quote ,action)
					     (quote ,user-data)))))

(defun insert-gui-button (button &optional pos buffer)
  "Insert GUI button BUTTON at POS in BUFFER."
  (check-argument-type 'gui-button-p button)
  (declare-fboundp (make-annotation (make-glyph button)
				    pos 'text buffer nil)))

;;; gui.el ends here
