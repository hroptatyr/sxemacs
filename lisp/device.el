;;; device.el --- miscellaneous device functions not written in C

;; Copyright (C) 1994-5, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996 Ben Wing

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file is dumped with SXEmacs.

;;; Code:

;;; Initialization

; Specifier tag 'printer which matches printers
(define-specifier-tag 'printer (function device-printer-p))

; Specifier tag 'display which matches displays
(define-specifier-tag 'display (function
				(lambda (device)
				  (not (device-printer-p device)))))

;;; Functions

(defun device-list ()
  "Return a list of all devices."
  (apply 'nconc (mapcar 'console-device-list (console-list))))

(defun device-type (&optional device)
  "Return the type of the specified device (e.g. `x' or `tty').
This is equivalent to the type of the device's console.
Value is `tty' for a tty device (a character-only terminal),
`x' for a device that is a screen on an X display,
`ns' for a device that is a NeXTstep connection (not yet implemented),
`stream' for a stream device (which acts like a stdio stream), and
`dead' for a deleted device."
  (or device (setq device (selected-device)))
  (if (not (device-live-p device)) 'dead
    (console-type (device-console device))))

(defun make-tty-device (&optional tty terminal-type controlling-process)
  "Create a new device on TTY.
  TTY should be the name of a tty device file (e.g. \"/dev/ttyp3\" under
SunOS et al.), as returned by the `tty' command.  A value of nil means
use the stdin and stdout as passed to XEmacs from the shell.
  If TERMINAL-TYPE is non-nil, it should be a string specifying the
type of the terminal attached to the specified tty.  If it is nil,
the terminal type will be inferred from the TERM environment variable.
  If CONTROLLING-PROCESS is non-nil, it should be an integer
specifying the process id of the process in control of the specified tty.  If
it is nil, it is assumes to be the value returned by emacs-pid."
  (make-device 'tty tty (list 'terminal-type terminal-type
			      'controlling-process controlling-process)))

(defun device-pixel-width (&optional device)
  "Return the width in pixels of DEVICE, or nil if unknown."
  (let ((ds (device-system-metric device 'size-device)))
    (and ds (car ds))))

(defun device-pixel-height (&optional device)
  "Return the height in pixels of DEVICE, or nil if unknown."
  (let ((ds (device-system-metric device 'size-device)))
    (and ds (cdr ds))))

(defun device-mm-width (&optional device)
  "Return the width in millimeters of DEVICE, or nil if unknown."
  (let ((ds (device-system-metric device 'size-device-mm)))
    (and ds (car ds))))

(defun device-mm-height (&optional device)
  "Return the height in millimeters of DEVICE, or nil if unknown."
  (let ((ds (device-system-metric device 'size-device-mm)))
    (and ds (cdr ds))))

(defun device-bitplanes (&optional device)
  "Return the number of bitplanes of DEVICE, or nil if unknown."
  (device-system-metric device 'num-bit-planes))

(defun device-color-cells (&optional device)
  "Return the number of color cells of DEVICE, or nil if unknown."
  (device-system-metric device 'num-color-cells))

(defun make-x-device (&optional display)
  "Create a new device connected to DISPLAY."
  (make-device 'x display))

(defun device-on-window-system-p (&optional device)
  "Return non-nil if DEVICE is on a window system.
This generally means that there is support for the mouse, the menubar,
the toolbar, glyphs, etc."
  (or device (setq device (selected-device)))
  (console-on-window-system-p (device-console device)))

(defun call-device-method (name device &rest args)
  "Call a DEVICE-specific function with the generic name NAME.
If DEVICE is not provided then the selected device is used."
  (or device (setq device (selected-device)))
  (or (symbolp name) (error "function name must be a symbol"))
  (let ((devmeth (intern (concat (symbol-name
				  (device-type device)) "-" (symbol-name name)))))
    (if (functionp devmeth)
	(if args
	    (apply devmeth args)
	  (funcall devmeth))
      nil)))

(defmacro define-device-method (name &optional docstring)
  "Define NAME to be a device method."
  `(defun ,name (&rest arglist) ,docstring
     (apply 'call-device-method (quote ,name) nil arglist)))

(defmacro define-device-method* (name &optional docstring)
  "Define NAME to be a device method."
  `(defun* ,name (&rest arglist) ,docstring
     (apply 'call-device-method (quote ,name) nil arglist)))

(defalias 'valid-device-type-p 'valid-console-type-p)
(defalias 'device-type-list 'console-type-list)
(defalias 'device-pixel-depth 'device-bitplanes)

;;; device.el ends here
