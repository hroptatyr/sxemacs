;;; symbol-syntax.el --- find chars with symbol syntax

;; Copyright (C) 1992, 1993, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Sun Microsystems.

;; Author: JBW, JBW@_CORTEZ
;; Created: Wed Jun 20 15:15:34 1990
;; Maintainer: SXEmacs Development Team
;; Keywords: matching

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

;; Last modified by: Ben Wing, ben@xemacs.org
;; Last modified on: Mon Oct  2 02:32:05 GMT 1995

;;; Code:

(defvar symbol-syntax-table-alist nil)
;;  '((c-mode-syntax-table)
;;    (emacs-lisp-mode-syntax-table)
;;    (lisp-mode-syntax-table)
;;    (text-mode-syntax-table)))

(defun update-symbol-syntax-table-alist ()
  (let ((alist symbol-syntax-table-alist)
	item)
    (while (consp alist)
      (cond ((null (car alist))
	     (error "Missing alist item"))
	    ((null (car (car alist)))
	     (error "Alist item with null car"))
	    ;; this functionality not used
	    ((symbolp (setq item (car (car alist))))
	     (or (null (cdr (car alist)))
		 (error "Alist item expected to have null cdr"))
	     (while (symbolp item)
	       (setq item (symbol-value item)))
	     (setcar (car alist) item)))
      (cond ((not (syntax-table-p (car (car alist))))
	     (error "Alist item car expected to be symbol table"))
	    ((null (cdr (car alist)))
	     (setcdr (car alist)
		     (make-symbol-syntax-table (car (car alist))))))
      (setq alist (cdr alist)))))

(defun get-symbol-syntax-table (norm-table)
  (let (result)
    (if (setq result (assq norm-table symbol-syntax-table-alist))
	nil
      (update-symbol-syntax-table-alist)
      (if (setq result (assq norm-table symbol-syntax-table-alist))
	  nil
	(setq symbol-syntax-table-alist
	      (cons (list norm-table)
		    symbol-syntax-table-alist))
	(update-symbol-syntax-table-alist)
	(or (setq result (assq norm-table symbol-syntax-table-alist))
	    (error "Syntax table missing from symbol-syntax-table-alist"))))
    (or (setq result (cdr result))
	(error "Alist item has null cdr"))
    (or (syntax-table-p result)
	(error "Non-syntax-table item in alist"))
    result))

(defun make-symbol-syntax-table (in-table)
  (let ((out-table (copy-syntax-table in-table)))
    (map-syntax-table
     #'(lambda (key value)
	 (if (eq ?_ (char-syntax-from-code value))
	     (put-char-table key (set-char-syntax-in-code value ?w)
			     out-table))
	 nil)
     out-table)
    out-table))

;; stuff for examining contents of syntax tables
;;(show-chars-with-syntax
;; '(c-mode-syntax-table
;;   emacs-lisp-mode-syntax-table
;;   lisp-mode-syntax-table
;;   text-mode-syntax-table)
;; ?_)

(defun show-chars-with-syntax (tables syntax)
  (let ((schars nil))
    (unwind-protect
	(while (consp tables)
	  (let* ((chars nil)
		 (table-symbol (car tables))
		 (table table-symbol))
	    (or (symbolp table-symbol)
		(error "bad argument non-symbol"))
	    (while (symbolp table)
	      (setq table (symbol-value table)))
	    (map-syntax-table
	     #'(lambda (key value)
		 (if (eq syntax (char-syntax-from-code value))
		     (setq chars (cons key chars)))
		 nil)
	     table)
	    (setq schars (cons (list table-symbol (nreverse chars))
			       schars)))
	  (setq tables (cdr tables))))
    (nreverse schars)))

(provide 'symbol-syntax)

;;; symbol-syntax.el ends here
