;;; hebrew.el --- Support for Hebrew -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, Hebrew

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

;;  ISO 8859-8 (Hebrew) support.

;;; Code:

;; Syntax of Hebrew characters
(loop for c from 96 to 122
      do (modify-syntax-entry (make-char 'hebrew-iso8859-8 c) "w"))
(modify-syntax-entry (make-char 'hebrew-iso8859-8 32) "w") ; no-break space


(make-coding-system
 'iso-8859-8 'iso2022
 "ISO-8859-8 (ISO 2022 based 8-bit encoding for Hebrew)"
 '(charset-g0 ascii
   charset-g1 hebrew-iso8859-8
   charset-g2 t
   charset-g3 t
   no-iso6429 t
   mnemonic "MIME/Hbrw"
   ))

(make-coding-system
 'ctext-hebrew 'iso2022
 "Coding-system for Hebrew."
 '(charset-g0 ascii
   charset-g1 hebrew-iso8859-8
   charset-g2 t
   charset-g3 t
   mnemonic "CText/Hbrw"
   ))

(defun setup-hebrew-environment ()
  "Setup multilingual environment (MULE) for Hebrew.
Note: right-to-left writing is not yet supported."
  (interactive)
  (set-language-environment "Hebrew"))

(set-language-info-alist
 "Hebrew" '((charset hebrew-iso8859-8)
	    (coding-system iso-8859-8)
	    (coding-priority iso-8859-8)
	    (input-method . "hebrew")
	    (sample-text . "Hebrew	[2],Hylem[0](B")
	    (documentation . "Right-to-left writing is not yet supported.")
	    ))

;;; hebrew.el ends here
