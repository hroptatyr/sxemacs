;;; mule-tty-init.el --- Initialization code for console tty under MULE

;; Copyright (C) 1998 Free Software Foundation, Inc.
;; Copyright (C) 1998 Kazuyuki IENAGA <kazz@imasy.or.jp>

;; Author: Kazuyuki IENAGA <kazz@imasy.or.jp>
;; Keywords: mule, tty, console, dumped

;; This file is part of SXEmacs.
;;
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

;;; Commentary:

;; This file is dumped with SXEmacs when Mule and TTY support are enabled.

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
