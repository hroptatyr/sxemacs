;;; number.el -- Enhanced Number Types for SXEmacs

;; Copyright (C) 2005 by Sebastian Freundt

;; Author: Sebastian Freundt <hroptatyr@sxemacs.org
;; Created: Fri Nov  4 16:09:10 2005 UTC
;; Keywords: lisp, number

;; This file is part of SXEmacs.

;; SXEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; SXEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SXEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Comments:
;; This file provides some additional functionality not worth
;; implementing in C.
;; 
;; Note to myself: number.el is a stupid name :(


(defun coprimep (number1 number2 &rest numbers)
  "Return non-nil iff the arguments are coprime, nil otherwise.
Numbers are coprime if their gcd is 1."
  (= 1 (apply #'gcd number1 number2 numbers)))
(defalias 'relatively-prime-p #'coprimep)

(defun ^-1 (number)
  "Return the reciprocal of NUMBER."
  (// number))

(defun positivep (number)
  "Return t if OBJECT is a positive number.

We call a number object positive iff it is comparable
and it is nonnegative and not a zero."
  (and (nonnegativep number)
       (not (zerop number))))


;; coercion abbrevs
(defun int (number &optional precision)
  "Return the ordinary integer numerically equal to NUMBER.
The optional argument PRECISION is unused.

This is a convenience function analogous to `float'."
  (coerce-number number 'int precision))

(defun bigz (number &optional precision)
  "Return the MPZ number numerically equal to NUMBER.
The optional argument PRECISION is unused.

This is a convenience function analogous to `float'."
  (coerce-number number 'bigz precision))

(defun bigq (number &optional precision)
  "Return the MPQ number numerically equal to NUMBER.
The optional argument PRECISION is unused.

This is a convenience function analogous to `float'."
  (coerce-number number 'bigq precision))

(defun rational (number &optional precision)
  "Return a rational most suitable to represent NUMBER.
The optional argument PRECISION is unused.

This is a convenience function analogous to `float'."
  (coerce-number number 'rational precision))


;;(defun float ...)
;; already implemented

(defun bigf (number &optional precision)
  "Return the MPF number numerically equal to NUMBER.
If optional argument PRECISION is non-nil, its value
\(an integer\) is used as precision.

This is a convenience function analogous to `float'."
  (coerce-number number 'bigf precision))

(defun bigfr (number &optional precision)
  "Return the MPFR number numerically equal to NUMBER.
If optional argument PRECISION is non-nil, its value
\(an integer\) is used as precision.

This is a convenience function analogous to `float'."
  (coerce-number number 'bigfr precision))

(defun real (number &optional precision)
  "Return a real with respect to `read-real-as' numerically
equal to NUMBER.
If optional argument PRECISION is non-nil, its value
\(an integer\) is used as precision.

This is a convenience function analogous to `float'."
  (coerce-number number 'real precision))


(defun bigg (number &optional precision)
  "Return the Gaussian number numerically equal to NUMBER.
The optional argument PRECISION is unused.

This is a convenience function analogous to `float'."
  (coerce-number number 'bigg precision))

(defun bigc (number &optional precision)
  "Return the MPC number numerically equal to NUMBER.
If optional argument PRECISION is non-nil, its value
\(an integer\) is used as precision.

This is a convenience function analogous to `float'."
  (coerce-number number 'bigc precision))


;;; dealing with norms, valuations and such things
(defun canonical-valuation (number)
  "Return the canonical valuation of NUMBER."
  (cond ((archimedeanp number)
         (abs number))))


(provide 'number)

;;; number.el ends here
