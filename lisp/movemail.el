;;; movemail.el --- move mail spool to a place where SXEmacs can munge it

;; Copyright (C) 1985-1986, 1990, 1992-1997 Free Software Foundation, Inc.
;; Copyright (c) 1993, 1994 Sun Microsystems, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois

;; Author: Mike Sperber <sperber@informatik.uni-tuebingen.de>
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

;; This file handles calling movemail with the right locking command
;; line options.

;;; Code:

(defvar mail-lock-method
  (let ((stuff (getenv "EMACSLOCKMETHOD")))
    (if stuff
	(intern stuff)
      configure-mail-lock-method))
  "mail spool locking method used by this instance of XEmacs.
This must be one of the symbols in MAIL-LOCK-METHODS.")

(defun move-mail-spool (from to &optional buffer pop-password)
  "Move mail spool in file FROM to file TO.
BUFFER is a buffer for error messages.
POP-PASSWORD is a password for POP mailbox access."
  (apply 'call-process
	 (expand-file-name "movemail" exec-directory)
	 nil buffer nil
	 "-m"
	 (symbol-name mail-lock-method)
	 from to
	 (and pop-password
	      (list pop-password))))

;;; movemail.el ends here
