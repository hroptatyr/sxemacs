;;; greek.el --- Support for Greek

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Keywords: multilingual, Greek

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

;; For Greek, the character set ISO8859-7 is supported.

;;; Code:

;; For syntax of Greek
(loop for c from 54 to 126
      do (modify-syntax-entry (make-char 'greek-iso8859-7 c) "w"))
(modify-syntax-entry (make-char 'greek-iso8859-7 32) "w") ; no-break space
(modify-syntax-entry ?.FN7 ".")
(modify-syntax-entry ?N; ".")
(modify-syntax-entry ?N= ".")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GREEK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (make-coding-system
;;  'greek-iso-8bit 2 ?7
;;  "ISO 2022 based 8-bit encoding for Greek (MIME:ISO-8859-7)"
;;  '(ascii greek-iso8859-7 nil nil
;;    nil nil nil nil nil nil nil)
;;  '((safe-charsets ascii greek-iso8859-7)
;;    (mime-charset . iso-8859-7)))

;; (define-coding-system-alias 'iso-8859-7 'greek-iso-8bit)

(make-coding-system
 'iso-8859-7 'iso2022 "MIME ISO-8859-7"
 '(charset-g0 ascii
   charset-g1 greek-iso8859-7
   charset-g2 t
   charset-g3 t
   mnemonic "Grk"
   ))

(defun setup-greek-environment ()
  "Setup multilingual environment (MULE) for Greek."
  (interactive)
  (set-language-environment "Greek"))

(set-language-info-alist
 "Greek" '((charset greek-iso8859-7)
	   (coding-system iso-8859-7)
	   (coding-priority iso-8859-7)
	   (input-method . "greek")
	   (sample-text . "Greek (NGNkNkN]NmNiNjNa)	NCNeNiN\ NsNaNr")
	   (documentation . t)))

;;; greek.el ends here
