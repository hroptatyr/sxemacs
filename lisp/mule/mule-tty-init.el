;;; mule-tty-init.el --- Initialization code for console tty under MULE

;; Copyright (C) 1998 Free Software Foundation, Inc.
;; Copyright (C) 1998 Kazuyuki IENAGA <kazz@imasy.or.jp>

;; Author: Kazuyuki IENAGA <kazz@imasy.or.jp>
;; Keywords: mule, tty, console, dumped

;; This file is part of XEmacs.
;;
;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file is dumped with XEmacs when Mule and TTY support are enabled.

;;; Code:

(defvar mule-tty-win-initted nil)

(defun init-mule-tty-win ()
  "Initialize TTY for MULE at startup. Don't call this."
  (unless mule-tty-win-initted
    (add-hook
     'before-init-hook
     (lambda ()
       (when (eq (device-type) 'tty)
	 (when (string-match "^kterm" (getenv "TERM"))
	   (set-terminal-coding-system 'euc-jp)
	   (set-keyboard-coding-system 'euc-jp))
	 (set-console-tty-coding-system))))
    (setq mule-tty-win-initted t)))

;;; mule-tty-init.el ends here
