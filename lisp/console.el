;;; console.el --- miscellaneous console functions not written in C

;; Copyright (C) 1994-5, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996 Ben Wing

;; Maintainer: XEmacs Development Team
;; Keywords: internal, dumped

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
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file is dumped with XEmacs.

;;; Code:

(defun quit-char (&optional console)
  "Return the character that causes a QUIT to happen.
This is normally C-g.  Optional arg CONSOLE specifies the console
that the information is returned for; nil means the current console."
  (nth 3 (current-input-mode console)))

(defun resume-pid-console (pid)
  "Resume the consoles with a controlling process of PID."
  (mapc (lambda (c) 
	  (if (and (eq (console-type c) 'tty)
		   (eql pid (console-tty-controlling-process c)))
	      (resume-console c)))
	(console-list))
  nil)

;;; console.el ends here
