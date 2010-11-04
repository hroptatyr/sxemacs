;;; very-early-lisp.el --- Lisp support always needed by temacs

;; Copyright (C) 1998 by Free Software Foundation, Inc.

;; Author: SL Baur <steve@xemacs.org>
;;  Michael Sperber [Mr. Preprocessor] <sperber@Informatik.Uni-Tuebingen.De>
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

;;; Synched up with: Not in FSF

;;; Commentary:

;; This file must be loaded by temacs if temacs is to process bytecode
;; or dumped-lisp.el files.

;;; Code:

;;; Intended replacement for read-time Lisp reader macros #-, #+

(defmacro assemble-list (&rest components)
  "Assemble a list from COMPONENTS.
This is a poor man's backquote:
COMPONENTS is a list, each element of which is macro-expanded.
Each macro-expanded element either has the form (SPLICE stuff),
in which case stuff must be a list which is spliced into the result.
Otherwise, the component becomes an element of the list."
  (cons
   'append
   (mapcar #'(lambda (component)
	       (let ((component (macroexpand-internal component)))
		 (if (and (consp component)
			  (eq 'splice (car component)))
		     (car (cdr component))
		   (list 'list component))))
	   components)))

(defmacro when-feature (feature stuff)
  "Insert STUFF as a list element if FEATURE is a loaded feature.
This is intended for use as a component of ASSEMBLE-LIST."
  (list 'splice
	(list 'if (list 'featurep (list 'quote feature))
	      (list 'list stuff)
	      '())))

(defmacro unless-feature (feature stuff)
  "Insert STUFF as a list element if FEATURE is NOT a loaded feature.
This is intended for use as a component of ASSEMBLE-LIST."
  (list 'splice
	(list 'if (list 'featurep (list 'quote feature))
	      '()
	      (list 'list stuff))))

(provide 'very-early-lisp)

;;; very-early-lisp.el ends here
