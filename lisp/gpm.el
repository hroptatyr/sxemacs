;;; gpm.el --- Support the mouse when emacs run on a Linux console.

;; Copyright (C) 1999 Free Software Foundation

;; Author: William Perry <wmperry@gnu.org>
;; Keywords: mouse, terminals

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defvar gpm-enabled-devices (make-hash-table :test 'eq
					     :size 13
					     :weakness 'key)
  "A hash table of devices with GPM currently turned on.")

(defun gpm-mode (&optional arg device)
  "Toggle GPM mouse mode.
With prefix arg, turn GPM mouse mode on if and only if arg is positive."
  (interactive (list current-prefix-arg (selected-device)))
  (cond
   ((null arg)				; Toggle
    (if (gethash device gpm-enabled-devices)
	(progn
	  (gpm-enable device nil)
	  (remhash device gpm-enabled-devices))
      (gpm-enable device t)
      (puthash device t gpm-enabled-devices)))
   ((> arg 0)				; Turn on
    (gpm-enable device t)
    (puthash device t gpm-enabled-devices))
   ((gethash device gpm-enabled-devices) ; Turn off
    (gpm-enable device nil)
    (remhash device gpm-enabled-devices))))

(defun turn-on-gpm-mouse-tracking (&optional device)
  ;; Enable mouse tracking on linux console
  (gpm-mode 5 device))

(defun turn-off-gpm-mouse-tracking (&optional device)
  ;; Disable mouse tracking on linux console
  (gpm-mode -5 device))

(defun gpm-create-device-hook (device)
  (if (and (not noninteractive)		; Don't want to do this in batch mode
	   (fboundp 'gpm-enable)	; Must have C-level GPM support
	   (eq system-type 'linux)	; Must be running linux
	   (eq (device-type device) 'tty) ; on a tty
	   (equal "linux" (console-tty-terminal-type ; an a linux terminal type
			   (device-console device))))
      (turn-on-gpm-mouse-tracking device)))

(defun gpm-delete-device-hook (device)
  (if (and (not noninteractive)		; Don't want to do this in batch mode
	   (fboundp 'gpm-enable)	; Must have C-level GPM support
	   (eq system-type 'linux)	; Must be running linux
	   (eq (device-type device) 'tty) ; on a tty
	   (equal "linux" (console-tty-terminal-type ; an a linux terminal type
			   (device-console device))))
      (turn-off-gpm-mouse-tracking device)))

;; Restore normal mouse behavior outside Emacs

(add-hook 'suspend-hook 'turn-off-gpm-mouse-tracking)
(add-hook 'suspend-resume-hook 'turn-on-gpm-mouse-tracking)
(add-hook 'create-device-hook 'gpm-create-device-hook)
(add-hook 'delete-device-hook 'gpm-delete-device-hook)

(provide 'gpm)
