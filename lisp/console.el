;;; console.el --- miscellaneous console functions not written in C

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

(provide 'console)

;;; console.el ends here
