;;; gtk-select.el --- Lisp interface to GTK selections.

;; Copyright (C) 1990, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 2000 Free Software Foundation

;; Maintainer: William Perry <wmperry@gnu.org>
;; Keywords: extensions, dumped

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

;; This file is dumped with SXEmacs (when GTK support is compiled in).
;; #### Only copes with copying/pasting text

;;; Code:

(defun gtk-get-secondary-selection ()
  "Return text selected from some GTK window."
  (get-selection 'SECONDARY))

(defun gtk-own-secondary-selection (selection &optional type)
  "Make a secondary GTK Selection of the given argument.  The argument may be a
string or a cons of two markers (in which case the selection is considered to
be the text between those markers)."
  (interactive (if (not current-prefix-arg)
		   (list (read-string "Store text for pasting: "))
		 (list (cons ;; these need not be ordered.
			(copy-marker (point-marker))
			(copy-marker (mark-marker))))))
  (own-selection selection 'SECONDARY))

(defun gtk-notice-selection-requests (selection type successful)
  "for possible use as the value of `gtk-sent-selection-hooks'."
  (if (not successful)
      (message "Selection request failed to convert %s to %s"
	       selection type)
    (message "Sent selection %s as %s" selection type)))

(defun gtk-notice-selection-failures (selection type successful)
  "for possible use as the value of `gtk-sent-selection-hooks'."
  (or successful
      (message "Selection request failed to convert %s to %s"
	       selection type)))

;(setq gtk-sent-selection-hooks 'gtk-notice-selection-requests)
;(setq gtk-sent-selection-hooks 'gtk-notice-selection-failures)
