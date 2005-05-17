;;; tty-init.el --- initialization code for tty's

;; Copyright (C) 1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1996 Ben Wing <ben@xemacs.org>.

;; Maintainer: XEmacs Development Team
;; Keywords: terminals, dumped

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
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not synched.

;;; Commentary:

;; This file is dumped with XEmacs (when TTY support is compiled in).

;;; Code:

(defvar pre-tty-win-initted nil)

;; called both from init-tty-win and from the C code.
(defun init-pre-tty-win ()
  "Initialize TTY at startup (pre).  Don't call this."
  (unless pre-tty-win-initted
    (register-tty-color "black"   "\e[30m" "\e[40m")
    (register-tty-color "red"     "\e[31m" "\e[41m")
    (register-tty-color "green"   "\e[32m" "\e[42m")
    (register-tty-color "yellow"  "\e[33m" "\e[43m")
    (register-tty-color "blue"    "\e[34m" "\e[44m")
    (register-tty-color "magenta" "\e[35m" "\e[45m")
    (register-tty-color "cyan"    "\e[36m" "\e[46m")
    (register-tty-color "white"   "\e[37m" "\e[47m")

    ;; Define `highlighted' tty colors
    (register-tty-color "darkgrey"      "\e[1;30m" "\e[1;40m")
    (register-tty-color "brightred"     "\e[1;31m" "\e[1;41m")
    (register-tty-color "brightgreen"   "\e[1;32m" "\e[1;42m")
    (register-tty-color "brightyellow"  "\e[1;33m" "\e[1;43m")
    (register-tty-color "brightblue"    "\e[1;34m" "\e[1;44m")
    (register-tty-color "brightmagenta" "\e[1;35m" "\e[1;45m")
    (register-tty-color "brightcyan"    "\e[1;36m" "\e[1;46m")
    (register-tty-color "brightwhite"   "\e[1;37m" "\e[1;47m")

    (setq pre-tty-win-initted t)))

;; called both from init-tty-win and from the C code.
;; we have to do this for every created TTY console.
(defun init-post-tty-win (console)
  "Initialize TTY at console creation time (post).  Don't call this."
  ;; load the appropriate term-type-specific Lisp file.
  ;; we don't do this at startup here so that the user can
  ;; override term-file-prefix. (startup.el does it after
  ;; loading the init file.)
  (if (featurep 'mule)
      (init-mule-tty-win))
  (when init-file-loaded
    ;; temporarily select the console so that the changes
    ;; to function-key-map are made for the right console.
    (let ((foobar (selected-console)))
      (unwind-protect
	  (progn
	    (select-console console)
	    (load-terminal-library))
	(select-console foobar)))))

(defvar tty-win-initted nil)

(defun init-tty-win ()
  "Initialize TTY at startup.  Don't call this."
  (unless tty-win-initted
    (init-pre-tty-win)
    (make-tty-device nil nil)
    (init-post-tty-win (selected-console))
    (setq tty-win-initted t)))

(defun make-frame-on-tty (tty &optional props)
  "Create a frame on the TTY connection named TTY.
TTY should be a TTY device name such as \"/dev/ttyp3\" (as returned by
the `tty' command in that TTY), or nil for the standard input/output
of the running XEmacs process.

PROPS should be a plist of properties, as in the call to `make-frame'.

This function opens a connection to the TTY or reuses an existing
connection.

This function is a trivial wrapper around `make-frame-on-device'."
  (interactive "sMake frame on TTY: ")
  (if (equal tty "") (setq tty nil))
  (make-frame-on-device 'tty tty props))

;;; tty-init.el ends here
