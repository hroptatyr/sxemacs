;;; thai-xtis-chars.el --- definition of the Thai XTIS charset.

;; Copyright (C) 1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Author: MORIOKA Tomohiko <tomo@etl.go.jp>

;; Keywords: mule, multilingual, Thai, XTIS

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

;; For Thai, the pre-composed character set proposed by
;; Virach Sornlertlamvanich <virach@links.nectec.or.th> is supported.

;;; Code:

(make-charset 'thai-xtis "Precomposed Thai (XTIS by Virach)."
	      '(registry "xtis-0"
			 dimension 2
			 columns 1
			 chars 94
			 final ??
			 graphic 0))

(define-category ?x "Precomposed Thai character.")
(modify-category-entry 'thai-xtis ?x)

;; thai-xtis-chars.el ends here.
