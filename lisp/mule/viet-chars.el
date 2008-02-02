;;; vietnamese-chars.el --- pre-loaded support for Vietnamese, part 1.

;; Copyright (C) 1992,93,94,95 Free Software Foundation, Inc.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1995 Sun Microsystems.

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

;;; Synched up with: Mule 2.3.

;; We have to split up the Vietnamese stuff into two files because
;; we are registering new charsets and then immediately using characters
;; from those sets.  We cannot reliably expect this to work if they
;; are in the same file because of the buffering that happens while
;; reading -- the place where we use the newly-defined sets may be
;; read in before the code that creates those sets is evaluated.

;; Vietnamese VISCII with two tables.
(make-charset 'vietnamese-viscii-lower "VISCII lower (Vietnamese)"
	      '(registry "VISCII1.1"
		dimension 1
		chars 96
		final ?1
		graphic 1
		))

(make-charset 'vietnamese-viscii-upper "VISCII upper (Vietnamese)"
	      '(registry "VISCII1.1"
		dimension 1
		chars 96
		final ?2
		graphic 1
		))

(modify-syntax-entry 'vietnamese-viscii-lower "w")
(modify-syntax-entry 'vietnamese-viscii-upper "w")

(define-category ?v "Vietnamese character.")
(modify-category-entry 'vietnamese-viscii-lower ?v)
(modify-category-entry 'vietnamese-viscii-upper ?v)

;;; vietnamese-chars.el ends here
