;;; buffer.el --- buffer routines taken from C

;; Copyright (C) 1985-1989, 1992-1995, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 1995, 1996 Ben Wing.

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

;;; Synched up with: FSF 19.30 buffer.c.

;;; Commentary:

;; This file is dumped with SXEmacs.

;;; Code:

(defun switch-to-buffer (bufname &optional norecord)
  "Select buffer BUFNAME in the current window.
BUFNAME may be a buffer or a buffer name and is created if it did not exist.
Optional second arg NORECORD non-nil means do not put this buffer at the
front of the list of recently selected ones.

WARNING: This is NOT the way to work on another buffer temporarily
within a Lisp program!  Use `set-buffer' instead.  That avoids messing with
the window-buffer correspondences."
  (interactive "BSwitch to buffer: ")
  ;; #ifdef I18N3
  ;; #### Doc string should indicate that the buffer name will get
  ;; translated.
  ;; #endif
  (if (eq (minibuffer-window) (selected-window))
      (error "Cannot switch buffers in minibuffer window"))
  (if (window-dedicated-p (selected-window))
      (error "Cannot switch buffers in a dedicated window"))
  (let (buf)
    (if (null bufname)
	(setq buf (other-buffer (current-buffer)))
      (setq buf (get-buffer bufname))
      (if (null buf)
	  (progn
	    (setq buf (get-buffer-create bufname))
	    (set-buffer-major-mode buf))))
    (push-window-configuration)
    (set-buffer buf)
    (set-window-buffer (last-nonminibuf-window) buf norecord)
    buf))

(defun pop-to-buffer (bufname &optional not-this-window-p on-frame)
  "Select buffer BUFNAME in some window, preferably a different one.
If BUFNAME is nil, then some other buffer is chosen.
If `pop-up-windows' is non-nil, windows can be split to do this.
If optional second arg NOT-THIS-WINDOW-P is non-nil, insist on finding
another window even if BUFNAME is already visible in the selected window.
If optional third arg is non-nil, it is the frame to pop to this
buffer on.
If `focus-follows-mouse' is non-nil, keyboard focus is left unchanged.

Buffers with names that are members of the `same-window-buffer-names'
list, or that match an element of the `same-window-regexps' list are
treated specially by this function--they are always selected in the
same window rather than in a different one."
  ;; #ifdef I18N3
  ;; #### Doc string should indicate that the buffer name will get
  ;; translated.
  ;; #endif
  ;; This is twisted.  It is evil to throw the keyboard focus around
  ;; willy-nilly if the user wants focus-follows-mouse.
  (let ((oldbuf (current-buffer))
	buf window frame)
    (if (null bufname)
	(setq buf (other-buffer (current-buffer)))
      (setq buf (get-buffer bufname))
      (if (null buf)
	  (progn
	    (setq buf (get-buffer-create bufname))
	    (set-buffer-major-mode buf))))
    (push-window-configuration)
    (set-buffer buf)
    (setq window (display-buffer buf not-this-window-p on-frame))
    (setq frame (window-frame window))
    ;; if the display-buffer hook decided to show this buffer in another
    ;; frame, then select that frame, (unless obeying focus-follows-mouse -sb).
    (if (and (not focus-follows-mouse)
	     (not (eq frame (selected-frame))))
	(select-frame frame))
    (record-buffer buf)
    (if (and focus-follows-mouse
	     on-frame
	     (not (eq on-frame (selected-frame))))
	(set-buffer oldbuf)
      ;; select-window will modify the internal keyboard focus of XEmacs
      (select-window window))
    buf))

;;; buffer.el ends here
