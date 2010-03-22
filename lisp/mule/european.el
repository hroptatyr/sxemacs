;;; european.el --- European languages -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Keywords: multilingual, European

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

;; For Europeans, six coded character sets ISO8859-1,2,3,4,9 are supported.
;; Note: ISO 8859/15 (Latin-9) is supported via the latin-unity package.

;; #### latin.el would be a better name for this file.

;;; Code:

;; For syntax of Latin-1 characters.
(loop for c from 64 to 127              ; from ',A@(B' to ',A(B'
      do (modify-syntax-entry (make-char 'latin-iso8859-1 c) "w"))

(modify-syntax-entry (make-char 'latin-iso8859-1 32) "w") ; no-break space
(modify-syntax-entry ?,AW(B "_")
(modify-syntax-entry ?,Aw(B "_")

;; For syntax of Latin-2
(loop for c in '(?,B!(B ?,B#(B ?,B%(B ?,B&(B ?,B)(B ?,B*(B ?,B+(B ?,B,(B ?,B.(B ?,B/(B ?,B1(B ?,B3(B ?,B5(B ?,B6(B ?,B9(B ?,B:(B ?,B;(B ?,B<(B)
      do (modify-syntax-entry c "w"))

(loop for c from 62 to 126
      do (modify-syntax-entry (make-char 'latin-iso8859-2 c) "w"))

(modify-syntax-entry (make-char 'latin-iso8859-2 32) "w") ; no-break space
(modify-syntax-entry ?,BW(B ".")
(modify-syntax-entry ?,Bw(B ".")

;; For syntax of Latin-3
(loop for c in '(?,C!(B ?,C&(B ?,C)(B ?,C*(B ?,C+(B ?,C,(B ?,C/(B ?,C1(B ?,C5(B ?,C6(B ?,C:(B ?,C;(B ?,C<(B ?,C?(B)
  do (modify-syntax-entry c "w"))

(loop for c from 64 to 126
  do (modify-syntax-entry (make-char 'latin-iso8859-3 c) "w"))

(modify-syntax-entry (make-char 'latin-iso8859-3 32) "w") ; no-break space
(modify-syntax-entry ?,CW(B ".")
(modify-syntax-entry ?,Cw(B ".")

;; For syntax of Latin-4
(loop for c in '(?,D!(B ?,D"(B ?,D#(B ?,D%(B ?,D&(B ?,D)(B ?,D*(B ?,D+(B ?,D,(B ?,D.(B ?,D1(B ?,D3(B ?,D5(B ?,D6(B ?,D9(B ?,D:(B ?,D;(B ?,D<(B ?,D=(B ?,D>(B ?,D?(B)
  do (modify-syntax-entry c "w"))

(loop for c from 64 to 126
  do (modify-syntax-entry (make-char 'latin-iso8859-4 c) "w"))

(modify-syntax-entry (make-char 'latin-iso8859-4 32) "w") ; no-break space
(modify-syntax-entry ?,DW(B ".")
(modify-syntax-entry ?,Dw(B ".")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EUROPEANS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Latin-1 (ISO-8859-1)

;; (make-coding-system
;;  'iso-latin-1 2 ?1
;;  "ISO 2022 based 8-bit encoding for Latin-1 (MIME:ISO-8859-1)"
;;  '(ascii latin-iso8859-1 nil nil
;;    nil nil nil nil nil nil nil nil nil nil nil nil t)
;;  '((safe-charsets ascii latin-iso8859-1)
;;    (mime-charset . iso-8859-1)))

;; (define-coding-system-alias 'iso-8859-1 'iso-latin-1)
;; (define-coding-system-alias 'latin-1 'iso-latin-1)

;; (make-coding-system
;;  'compound-text 2 ?1
;;  "ISO 2022 based encoding used in inter client communication of X"
;;  '((ascii t) (latin-iso8859-1 t) nil nil
;;    nil ascii-eol ascii-cntl nil nil nil nil nil nil nil nil nil t)
;;  '((safe-charsets . t)))

;; (define-coding-system-alias 'ctext 'compound-text)

(defun setup-latin1-environment ()
  "Set up multilingual environment (MULE) for European Latin-1 users."
  (interactive)
  (set-language-environment "Latin-1"))

(set-language-info-alist
 "Latin-1" '((charset ascii latin-iso8859-1)
	     (coding-system iso-8859-1)
	     (coding-priority iso-8859-1)
	     (input-method . "latin-1-prefix")
	     (sample-text
	      . "Hello, Hej, Tere, Hei, Bonjour, Gr,A|_(B Gott, Ciao, ,A!(BHola!")
	     (documentation . "This language environment is a generic one for Latin-1 (ISO-8859-1)
character set which supports the following languages:
 Danish, Dutch, English, Faeroese, Finnish, French, German, Icelandic,
 Irish, Italian, Norwegian, Portuguese, Spanish, and Swedish.
We also have a German specific language environment \"German\"."))
 '("European"))

(set-language-info-alist
 "French" '((charset ascii latin-iso8859-1)
	    (coding-system iso-8859-1)
	    (coding-priority iso-8859-1)
	    (tutorial . "TUTORIAL.fr")
	    (sample-text
	     . "Hello, Hej, Tere, Hei, Bonjour, Gr,A|_(B Gott, Ciao, ,A!(BHola!")
	    (documentation . ("These languages are supported with the Latin-1 (ISO-8859-1) character set:
 Danish, Dutch, English, Faeroese, Finnish, French, German, Icelandic,
 Irish, Italian, Norwegian, Portuguese, Spanish, and Swedish.
")))
 '("European"))

(set-language-info-alist
 "Norwegian" '((charset ascii latin-iso8859-1)
	       (coding-system iso-8859-1)
	       (coding-priority iso-8859-1)
	       (tutorial . "TUTORIAL.no")
	       (sample-text
		. "Hello, Hej, Tere, Hei, Bonjour, Gr,A|_(B Gott, Ciao, ,A!(BHola!")
	       (documentation . ("These languages are supported with the Latin-1 (ISO-8859-1) character set:
 Danish, Dutch, English, Faeroese, Finnish, French, German, Icelandic,
 Irish, Italian, Norwegian, Portuguese, Spanish, and Swedish.
")))
 '("European"))


;; Latin-2 (ISO-8859-2)

;; (make-coding-system
;;  'iso-latin-2 2 ?2
;;  "ISO 2022 based 8-bit encoding (MIME:ISO-8859-2)"
;;  '(ascii latin-iso8859-2 nil nil
;;    nil nil nil nil nil nil nil)
;;  '((safe-charsets ascii latin-iso8859-2)
;;    (mime-charset . iso-8859-2)))

;; (define-coding-system-alias 'iso-8859-2 'iso-latin-2)
;; (define-coding-system-alias 'latin-2 'iso-latin-2)

(make-coding-system
 'iso-8859-2 'iso2022 "MIME ISO-8859-2"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-2
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-2"
   ))

(defun setup-latin2-environment ()
  "Set up multilingual environment (MULE) for European Latin-2 users."
  (interactive)
  (set-language-environment "Latin-2"))

(set-language-info-alist
 "Latin-2" '((charset ascii latin-iso8859-2)
	     (coding-system iso-8859-2)
	     (coding-priority iso-8859-2)
	     (input-method . "latin-2-prefix")
	     (documentation . "This language environment is a generic one for Latin-2 (ISO-8859-2)
character set which supports the following languages:
 Albanian, Czech, English, German, Hungarian, Polish, Romanian,
 Serbian, Croatian, Slovak, Slovene, Sorbian (upper and lower),
 and Swedish."))
 '("European"))

(set-language-info-alist
 "Croatian" '((charset ascii latin-iso8859-2)
	      (coding-system iso-8859-2)
	      (coding-priority iso-8859-2)
	      (tutorial . "TUTORIAL.hr")
	      (documentation . "This language environment is a generic one for Latin-2 (ISO-8859-2)
character set which supports the following languages:
 Albanian, Czech, English, German, Hungarian, Polish, Romanian,
 Serbian, Croatian, Slovak, Slovene, Sorbian (upper and lower),
 and Swedish."))
 '("European"))

(set-language-info-alist
 "Polish" '((charset ascii latin-iso8859-2)
	    (coding-system iso-8859-2)
	    (coding-priority iso-8859-2)
	    (tutorial . "TUTORIAL.pl")
	    (documentation . "This language environment is a generic one for Latin-2 (ISO-8859-2)
character set which supports the following languages:
 Albanian, Czech, English, German, Hungarian, Polish, Romanian,
 Serbian, Croatian, Slovak, Slovene, Sorbian (upper and lower),
 and Swedish."))
 '("European"))

;; Romanian support originally from romanian.el

(defun setup-romanian-environment ()
  "Setup multilingual environment (MULE) for Romanian."
  (interactive)
  (set-language-environment "Romanian"))

(set-language-info-alist
 "Romanian" '((charset ascii latin-iso8859-2)
	      (coding-system iso-8859-2)
	      (coding-priority iso-8859-2)
	      (input-method . "latin-2-postfix")
	      (tutorial . "TUTORIAL.ro")
	      (sample-text . "Bun,Bc(B ziua, bine a,B~(Bi venit!")
	      (documentation . t))
 '("European"))

(provide 'romanian)

;; Czech support originally from czech.el
;; Author: Milan Zamazal <pdm@fi.muni.cz>
;; Maintainer(for XEmacs): David Sauer <davids@penguin.cz>

(defun setup-czech-environment ()
  "Set up multilingual environment (MULE) for czech users."
  (interactive)
  (set-language-environment "Czech"))

(set-language-info-alist
 "Czech" '((charset ascii latin-iso8859-2)
	   (coding-system iso-8859-2)
	   (coding-priority iso-8859-2)
	   (tutorial . "TUTORIAL.cs")
	   (sample-text . "P,Bx(Bejeme v,Ba(Bm hezk,B}(B den!")
	   (documentation . t))
 '("European"))

(provide 'czech)


;; Latin-3 (ISO-8859-3)

;; (make-coding-system
;;  'iso-latin-3 2 ?3
;;  "ISO 2022 based 8-bit encoding (MIME:ISO-8859-3)"
;;  '(ascii latin-iso8859-3 nil nil
;;    nil nil nil nil nil nil nil)
;;  '((safe-charsets ascii latin-iso8859-3)
;;    (mime-charset . iso-8859-3)))

;; (define-coding-system-alias 'iso-8859-3 'iso-latin-3)
;; (define-coding-system-alias 'latin-3 'iso-latin-3)

(make-coding-system
 'iso-8859-3 'iso2022 "MIME ISO-8859-3"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-3
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-3"
   ))

(defun setup-latin3-environment ()
  "Set up multilingual environment (MULE) for European Latin-3 users."
  (interactive)
  (set-language-environment "Latin-3"))

(set-language-info-alist
 "Latin-3" '((charset ascii latin-iso8859-3)
	     (coding-system iso-8859-3)
	     (coding-priority iso-8859-3)
	     (input-method . "latin-3-prefix")
	     (documentation . "These languages are supported with the Latin-3 (ISO-8859-3) character set:
 Afrikaans, Catalan, Dutch, English, Esperanto, French, Galician,
 German, Italian, Maltese, Spanish, and Turkish."))
 '("European"))


;; Latin-4 (ISO-8859-4)

;; (make-coding-system
;;  'iso-latin-4 2 ?4
;;  "ISO 2022 based 8-bit encoding (MIME:ISO-8859-4)"
;;  '(ascii latin-iso8859-4 nil nil
;;    nil nil nil nil nil nil nil)
;;  '((safe-charsets ascii latin-iso8859-4)
;;    (mime-charset . iso-8895-4)))

;; (define-coding-system-alias 'iso-8859-4 'iso-latin-4)
;; (define-coding-system-alias 'latin-4 'iso-latin-4)

(make-coding-system
 'iso-8859-4 'iso2022 "MIME ISO-8859-4"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-4
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-4"
   ))

(defun setup-latin4-environment ()
  "Set up multilingual environment (MULE) for European Latin-4 users."
  (interactive)
  (set-language-environment "Latin-4"))

(set-language-info-alist
 "Latin-4" '((charset ascii latin-iso8859-4)
	     (coding-system iso-8859-4)
	     (coding-priority iso-8859-4)
	     (input-method . "latin-4-prefix")
	     (documentation . "These languages are supported with the Latin-4 (ISO-8859-4) character set:
 Danish, English, Estonian, Finnish, German, Greenlandic, Lappish,
 Latvian, Lithuanian, and Norwegian."))
 '("European"))


;; Latin-5 (ISO-8859-9)

;; (make-coding-system
;;  'iso-latin-5 2 ?9
;;  "ISO 2022 based 8-bit encoding (MIME:ISO-8859-9)"
;;  '(ascii latin-iso8859-9 nil nil
;;    nil nil nil nil nil nil nil)
;;  '((safe-charsets ascii latin-iso8859-9)
;;    (mime-charset . iso-8859-9)))

;; (define-coding-system-alias 'iso-8859-9 'iso-latin-5)
;; (define-coding-system-alias 'latin-5 'iso-latin-5)

(make-coding-system
 'iso-8859-9 'iso2022 "MIME ISO-8859-9"
 '(charset-g0 ascii
   charset-g1 latin-iso8859-9
   charset-g2 t
   charset-g3 t
   mnemonic "MIME/Ltn-5"
   ))

(defun setup-latin5-environment ()
  "Set up multilingual environment (MULE) for European Latin-5 users."
  (interactive)
  (set-language-environment "Latin-5"))

(set-language-info-alist
 "Latin-5" '((charset ascii latin-iso8859-9)
	     (coding-system iso-8859-9)
	     (coding-priority iso-8859-9)
	     (input-method . "latin-5-prefix")
	     (documentation . "These languages are supported with the Latin-5 (ISO-8859-9) character set."))
 '("European"))


(defun setup-german-environment ()
  "Set up multilingual environment (MULE) for German users."
  (interactive)
  (set-language-environment "German"))

(set-language-info-alist
 "German" '((tutorial . "TUTORIAL.de")
	    (charset ascii latin-iso8859-1)
	    (coding-system iso-8859-1)
	    (coding-priority iso-8859-1)
	    (input-method . "german-postfix")
	    (sample-text . "German (Deutsch Nord)	Guten Tag
German (Deutsch S,A|(Bd)	Gr,A|_(B Gott")
	    (documentation . "This language environment is almost the same as Latin-1,
but default input method is set to \"german-postfix\"."))
 '("European"))

(defun setup-slovenian-environment ()
  "Setup multilingual environment (MULE) for Slovenian."
  (interactive)
  (set-language-environment "Slovenian"))

(set-language-info-alist
 "Slovenian" '((charset . (ascii latin-iso8859-2))
	       (coding-system . (iso-8859-2))
	       (coding-priority . (iso-8859-2))
	       (input-method . "latin-2-postfix")
	       (tutorial . "TUTORIAL.sl")
	       (sample-text . ",B.(Belimo vam uspe,B9(Ben dan!")
	       (documentation . t))
 '("European"))

(provide 'slovenian)

;;; european.el ends here
