;;; misc-lang.el --- support for miscellaneous languages (characters)

;; Copyright (C) 1995,1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Keywords: multilingual, character set, coding system

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IPA (International Phonetic Alphabet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-charset 'ipa "International Phonetic Alphabet"
	      '(registry "MuleIPA"
		dimension 1
		chars 96
		final ?0
		graphic 1
		))		; for XEmacs

(defun setup-ipa-environment ()
  "Setup multilingual environment (MULE) for IPA."
  (interactive)
  (set-language-environment "IPA"))

(set-language-info-alist
 "IPA" '((charset . (ipa))
	 (coding-priority iso-2022-7bit)
	 (coding-system iso-2022-7bit)
	 (input-method . "ipa")
	 (documentation . "\
IPA is International Phonetic Alphabet for English, French, German
and Italian.")))

;;; misc-lang.el ends here
