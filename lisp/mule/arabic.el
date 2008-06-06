;;; arabic.el --- pre-loaded support for Arabic.

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

;;; Commentary:

;; Synched up with: Mule 2.3.

;;; Code:

;; Three character sets for Arabic
(make-charset 'arabic-digit "Arabic digits"
	      '(registry "MuleArabic-0"
		dimension 1
		chars 94
		final ?2
		graphic 0
		direction l2r
		))

(make-charset 'arabic-1-column "Arabic 1-column"
	      '(registry "MuleArabic-1"
		dimension 1
		chars 94
		final ?3
		graphic 0
		direction r2l
		))

(make-charset 'arabic-2-column "Arabic 2-column"
	      '(registry "MuleArabic-2"
		dimension 1
		chars 94
		final ?4
		graphic 0
		direction r2l
		))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ARABIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-language-environment 'arabic
;;   "Arabic"
;;   (lambda ()
;;     (require 'arabic)))

;;; arabic.el ends here
