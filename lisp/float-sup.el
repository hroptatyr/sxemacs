;;; float-sup.el --- detect absence of floating-point support in SXEmacs runtime

;; Copyright (C) 1985-7, 1997 Free Software Foundation, Inc.

;; Maintainer: FSF
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

;;; Synched up with: FSF 19.34.

;;; Code:

;; This file is dumped with SXEmacs.

;; Provide a meaningful error message if we are running on
;; bare (non-float) emacs.
;; Can't test for 'floatp since that may be defined by float-imitation
;; packages like float.el in this very directory.

;; XEmacs change
(or (featurep 'lisp-float-type)
    (error "Floating point was disabled at compile time"))

;; define pi and e via math-lib calls. (much less prone to killer typos.)
(unless (featurep 'bigfr)
  (defconst pi (* 4 (atan 1)) "The value of Pi (3.1415926...)")
  (defconst e (exp 1) "The value of e (2.7182818...)"))

;; Careful when editing this file ... typos here will be hard to spot.
;; (defconst pi       3.14159265358979323846264338327
;;  "The value of Pi (3.14159265358979323846264338327...)")

(unless (featurep 'bigfr)
  (defconst degrees-to-radians (/ pi 180.0)
    "Degrees to radian conversion constant")
  (defconst radians-to-degrees (/ 180.0 pi)
    "Radian to degree conversion constant"))

;; these expand to a single multiply by a float when byte compiled

(defmacro degrees-to-radians (x)
  "Convert ARG from degrees to radians."
  (list '* (/ pi 180.0) x))
(defmacro radians-to-degrees (x)
  "Convert ARG from radians to degrees."
  (list '* (/ 180.0 pi) x))

;; Provided in C code in XEmacs
;; (provide 'lisp-float-type)

(defalias 'expt #'^)

;;; float-sup.el ends here
