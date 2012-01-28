;;; cyrillic.el --- Support for Cyrillic -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995,1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Keywords: multilingual, Cyrillic

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

;; The character set ISO8859-5 is supported.
;; KOI-8, Windows-1251, and ALTERNATIVNYJ are converted to ISO8859-5
;; internally.

;;; Code:
(eval-when-compile (require 'ccl))

;; Cyrillic syntax
(modify-syntax-entry 'cyrillic-iso8859-5 "w")
(modify-syntax-entry ?,L-(B ".")
(modify-syntax-entry ?,Lp(B ".")
(modify-syntax-entry ?,L}(B ".")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CYRILLIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ISO-8859-5

(make-coding-system
 'iso-8859-5 'iso2022
 "ISO-8859-5 (ISO 2022 based 8-bit encoding for Cyrillic script)"
 '(charset-g0 ascii
   charset-g1 cyrillic-iso8859-5
   charset-g2 t
   charset-g3 t
   mnemonic "ISO8/Cyr"
   ))

(set-language-info-alist
 "Cyrillic-ISO" '((charset cyrillic-iso8859-5)
		  (tutorial . "TUTORIAL.ru")
		  (coding-system iso-8859-5)
		  (coding-priority iso-8859-5)
		  (input-method . "cyrillic-yawerty")
		  (features cyril-util)
		  (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		  (documentation . "Support for Cyrillic ISO-8859-5."))
 '("Cyrillic"))

;; KOI-8

(eval-and-compile

(defvar cyrillic-koi8-r-decode-table
  [
   0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
   16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
   32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
   48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
   64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
   80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
   96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
   ?$B(!(B ?$B("(B ?$B(#(B ?$B($(B ?$B(&(B ?$B(%(B ?$B('(B ?$B()(B ?$B(((B ?$B(*(B ?$B(+(B 32  ?$(G#'(B ?$(G#+(B ?$(G#/(B 32
   32  ?$(C"F(B 32  32  ?$B"#(B 32  ?$B"e(B ?$A!V(B ?$A!\(B ?$A!](B ?,L (B  32  ?,A0(B  ?,A2(B  ?,A7(B  ?,Aw(B
   ?$(G#D(B 32  32  ?,Lq(B  32  32  32  32  32  32  32  32  32  32  32  ?$(G#E(B
   32  32  ?$(G#G(B ?,L!(B  32  32  32  32  32  32  32  32  ?$(G#F(B 32  32  ?,A)(B
   ?,Ln(B  ?,LP(B  ?,LQ(B  ?,Lf(B  ?,LT(B  ?,LU(B  ?,Ld(B  ?,LS(B  ?,Le(B  ?,LX(B  ?,LY(B  ?,LZ(B  ?,L[(B  ?,L\(B  ?,L](B  ?,L^(B
   ?,L_(B  ?,Lo(B  ?,L`(B  ?,La(B  ?,Lb(B  ?,Lc(B  ?,LV(B  ?,LR(B  ?,Ll(B  ?,Lk(B  ?,LW(B  ?,Lh(B  ?,Lm(B  ?,Li(B  ?,Lg(B  ?,Lj(B
   ?,LN(B  ?,L0(B  ?,L1(B  ?,LF(B  ?,L4(B  ?,L5(B  ?,LD(B  ?,L3(B  ?,LE(B  ?,L8(B  ?,L9(B  ?,L:(B  ?,L;(B  ?,L<(B  ?,L=(B  ?,L>(B
   ?,L?(B  ?,LO(B  ?,L@(B  ?,LA(B  ?,LB(B  ?,LC(B  ?,L6(B  ?,L2(B  ?,LL(B  ?,LK(B  ?,L7(B  ?,LH(B  ?,LM(B  ?,LI(B  ?,LG(B  ?,LJ(B ]
  "Cyrillic KOI8-R decoding table.")

(defvar cyrillic-koi8-r-encode-table
  (let ((table (make-vector 256 32))
	(i 0))
    (while (< i 256)
      (let* ((ch (aref cyrillic-koi8-r-decode-table i))
	     (split (split-char ch)))
	(cond ((eq (car split) 'cyrillic-iso8859-5)
	       (aset table (logior (nth 1 split) 128) i)
	       )
	      ((eq ch 32))
	      ((eq (car split) 'ascii)
	       (aset table ch i)
	       )))
      (setq i (1+ i)))
    table)
  "Cyrillic KOI8-R encoding table.")

)

(define-ccl-program ccl-decode-koi8
  `(3
    ((read r0)
     (loop
      (write-read-repeat r0 ,cyrillic-koi8-r-decode-table))))
  "CCL program to decode KOI8.")

(define-ccl-program ccl-encode-koi8
  `(1
    ((read r0)
     (loop
      (if (r0 != ,(charset-id 'cyrillic-iso8859-5))
	  (write-read-repeat r0)
	((read r0)
	 (write-read-repeat r0 , cyrillic-koi8-r-encode-table))))))
  "CCL program to encode KOI8.")

;; (define-coding-system-alias 'koi8-r 'cyrillic-koi8)
;; (define-coding-system-alias 'koi8 'cyrillic-koi8)

(make-coding-system
 'koi8-r 'ccl
 "KOI8-R 8-bit encoding for Cyrillic."
 '(decode ccl-decode-koi8
   encode ccl-encode-koi8
   mnemonic "KOI8"))

;; `iso-8-1' is not correct, but XEmacs doesn't have a `ccl' category
(coding-system-put 'koi8-r 'category 'iso-8-1)

(define-ccl-program ccl-encode-koi8-r-font
   `(0
     ((r1 |= 128)
      (r1 = r1 ,cyrillic-koi8-r-encode-table)))
   "CCL program to encode Cyrillic chars to koi8-r font.")

;; (setq font-ccl-encoder-alist
;;       (cons (cons "koi8" ccl-encode-koi8-font) font-ccl-encoder-alist))

;; (defvar cyrillic-koi8-r-nonascii-translation-table
;;   (make-translation-table-from-vector cyrillic-koi8-r-decode-table)
;;   "Value of `nonascii-translation-table' in Cyrillic-KOI8 language environment..")

(set-language-info-alist
 "Cyrillic-KOI8" '((charset cyrillic-iso8859-5)
		   (coding-system koi8-r)
		   (coding-priority koi8-r)
		   (input-method . "cyrillic-yawerty")
		   (features cyril-util)
		   (tutorial . "TUTORIAL.ru")
		   (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		   (documentation . "Support for Cyrillic KOI8-R."))
 '("Cyrillic"))

;;; WINDOWS-1251

(eval-and-compile

(defvar cyrillic-windows-1251-decode-table
  [
   0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
   16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
   32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
   48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
   64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
   80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
   96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
   ?,L"(B ?,L#(B 32 ?,Ls(B 32 32 32 32 32 32 ?,L)(B 32 ?,L*(B ?,L,(B ?,L+(B ?,L/(B ;"
   ?,Lr(B 32 32 32 32 32 32 32 32 32 ?,Ly(B 32 ?,Lz(B ?,L|(B ?,L{(B ?,L(B
   ?,L (B ?,L.(B ?,L~(B ?,L((B ?,A$(B 32 ?,A&(B ?,L}(B ?,L!(B ?,A)(B ?,L$(B ?,A+(B ?,A,(B ?,L-(B ?,A.(B ?,L'(B
   ?,A0(B ?,A1(B ?,L&(B ?,Lv(B 32 ?,A5(B ?,A6(B ?,A7(B ?,Lq(B ?,Lp(B ?,Lt(B ?,A;(B ?,Lx(B ?,L%(B ?,Lu(B ?,Lw(B
  ?,L0(B ?,L1(B ?,L2(B ?,L3(B ?,L4(B ?,L5(B ?,L6(B ?,L7(B ?,L8(B ?,L9(B ?,L:(B ?,L;(B ?,L<(B ?,L=(B ?,L>(B ?,L?(B
  ?,L@(B ?,LA(B ?,LB(B ?,LC(B ?,LD(B ?,LE(B ?,LF(B ?,LG(B ?,LH(B ?,LI(B ?,LJ(B ?,LK(B ?,LL(B ?,LM(B ?,LN(B ?,LO(B
  ?,LP(B ?,LQ(B ?,LR(B ?,LS(B ?,LT(B ?,LU(B ?,LV(B ?,LW(B ?,LX(B ?,LY(B ?,LZ(B ?,L[(B ?,L\(B ?,L](B ?,L^(B ?,L_(B
  ?,L`(B ?,La(B ?,Lb(B ?,Lc(B ?,Ld(B ?,Le(B ?,Lf(B ?,Lg(B ?,Lh(B ?,Li(B ?,Lj(B ?,Lk(B ?,Ll(B ?,Lm(B ?,Ln(B ?,Lo(B ]
   "Cyrillic Windows-1251 decoding table.")

(defvar cyrillic-windows-1251-encode-table
  (let ((table (make-vector 256 32))
	(i 0))
    (while (< i 256)
      (let* ((ch (aref cyrillic-windows-1251-decode-table i))
	     (split (split-char ch)))
	(cond ((eq (car split) 'cyrillic-iso8859-5)
	       (aset table (logior (nth 1 split) 128) i)
	       )
	      ((eq ch 32))
	      ((eq (car split) 'ascii)
	       (aset table ch i)
	       )))
      (setq i (1+ i)))
    table)
  "Cyrillic Windows-1251 encoding table.")

)

(define-ccl-program ccl-decode-windows1251
  `(3
    ((read r0)
     (loop
      (write-read-repeat r0 ,cyrillic-windows-1251-decode-table))))
  "CCL program to decode Windows-1251.")

(define-ccl-program ccl-encode-windows1251
  `(1
    ((read r0)
     (loop
      (if (r0 != ,(charset-id 'cyrillic-iso8859-5))
	  (write-read-repeat r0)
	((read r0)
	 (write-read-repeat r0 , cyrillic-windows-1251-encode-table))))))
  "CCL program to encode Windows-1251.")

(make-coding-system
 'windows-1251 'ccl
 "Coding-system used for Windows-1251."
 '(decode ccl-decode-windows1251
   encode ccl-encode-windows1251
   mnemonic "CyrW"))

;; `iso-8-1' is not correct, but XEmacs doesn't have a `ccl' category
(coding-system-put 'windows-1251 'category 'iso-8-1)

(set-language-info-alist
 "Cyrillic-Win" '((charset cyrillic-iso8859-5)
		   (coding-system windows-1251)
		   (coding-priority windows-1251)
		   (input-method . "cyrillic-yawerty")
		   (features cyril-util)
		   (tutorial . "TUTORIAL.ru")
		   (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		   (documentation . "Support for Cyrillic Windows-1251."))
 '("Cyrillic"))

;;; ALTERNATIVNYJ

(eval-and-compile

(defvar cyrillic-alternativnyj-decode-table
  [
   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
   16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
   32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
   48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
   64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79
   80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
   96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
   ?,L0(B  ?,L1(B  ?,L2(B  ?,L3(B  ?,L4(B  ?,L5(B  ?,L6(B  ?,L7(B  ?,L8(B  ?,L9(B  ?,L:(B  ?,L;(B  ?,L<(B  ?,L=(B  ?,L>(B  ?,L?(B
   ?,L@(B  ?,LA(B  ?,LB(B  ?,LC(B  ?,LD(B  ?,LE(B  ?,LF(B  ?,LG(B  ?,LH(B  ?,LI(B  ?,LJ(B  ?,LK(B  ?,LL(B  ?,LM(B  ?,LN(B  ?,LO(B
   ?,LP(B  ?,LQ(B  ?,LR(B  ?,LS(B  ?,LT(B  ?,LU(B  ?,LV(B  ?,LW(B  ?,LX(B  ?,LY(B  ?,LZ(B  ?,L[(B  ?,L\(B  ?,L](B  ?,L^(B  ?,L_(B
   32  32  32  32  32  32  32  32  32  32  32  32  32  32  32  32
   32  32  32  32  32  32  32  32  32  32  32  32  32  32  32  32
   32  32  32  32  32  32  32  32  32  32  32  32  32  32  32  32
   ?,L`(B  ?,La(B  ?,Lb(B  ?,Lc(B  ?,Ld(B  ?,Le(B  ?,Lf(B  ?,Lg(B  ?,Lh(B  ?,Li(B  ?,Lj(B  ?,Lk(B  ?,Ll(B  ?,Lm(B  ?,Ln(B  ?,Lo(B
   ?,L!(B  ?,Lq(B   32  32  32  32  32  32  32  32  32  32  32  32  32 ?,Lp(B]
  "Cyrillic ALTERNATIVNYJ decoding table.")

(defvar cyrillic-alternativnyj-encode-table
  (let ((table (make-vector 256 32))
	(i 0))
    (while (< i 256)
      (let* ((ch (aref cyrillic-alternativnyj-decode-table i))
	     (split (split-char ch)))
	(if (eq (car split) 'cyrillic-iso8859-5)
	    (aset table (logior (nth 1 split) 128) i)
	  (if (/= ch 32)
	      (aset table ch i))))
      (setq i (1+ i)))
    table)
  "Cyrillic ALTERNATIVNYJ encoding table.")

)


(define-ccl-program ccl-decode-alternativnyj
  `(3
    ((read r0)
     (loop
      (write-read-repeat r0 ,cyrillic-alternativnyj-decode-table))))
  "CCL program to decode Alternativnyj.")

(define-ccl-program ccl-encode-alternativnyj
  `(1
    ((read r0)
     (loop
      (if (r0 != ,(charset-id 'cyrillic-iso8859-5))
	  (write-read-repeat r0)
	((read r0)
	 (write-read-repeat r0 ,cyrillic-alternativnyj-encode-table))))))
  "CCL program to encode Alternativnyj.")

;; (define-coding-system-alias 'alternativnyj 'cyrillic-alternativnyj)

(make-coding-system
 'alternativnyj 'ccl
 "Coding-system used for Alternativnyj"
 '(decode ccl-decode-alternativnyj
   encode ccl-encode-alternativnyj
   mnemonic "Cy.Alt"))

;; `iso-8-1' is not correct, but XEmacs doesn't have `ccl' category
(coding-system-put 'alternativnyj 'category 'iso-8-1)

;; (define-ccl-program ccl-encode-alternativnyj-font
;;   '(0
;;     ((r1 |= 128)
;;      (r1 = r1 ,cyrillic-alternativnyj-encode-table)))
;;   "CCL program to encode Cyrillic chars to Alternativnyj font.")

;; (setq font-ccl-encoder-alist
;;       (cons (cons "alternativnyj" ccl-encode-alternativnyj-font)
;;             font-ccl-encoder-alist))

;; (defvar cyrillic-alternativnyj-nonascii-translation-table
;;   (make-translation-table-from-vector cyrillic-alternativnyj-decode-table)
;;   "Value of `nonascii-translation-table' in Cyrillic-ALT language environment.")

(set-language-info-alist
 "Cyrillic-ALT" '((charset cyrillic-iso8859-5)
		  (coding-system alternativnyj)
		  (coding-priority alternativnyj)
		  (input-method . "cyrillic-yawerty")
		  (features cyril-util)
		  (tutorial . "TUTORIAL.ru")
		  (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		  (documentation . "Support for Cyrillic ALTERNATIVNYJ."))
 '("Cyrillic"))

;;
;; Setup case table
;;

;; FIXME: this defun is cut-and-pasted from mule/latin.el
(defun setup-case-pairs (charset pairs)
  (let ((tbl (standard-case-table)))
    (loop for (uc lc) in pairs do
      (put-case-table-pair (make-char charset uc) (make-char charset lc) tbl))))

(setup-case-pairs
 'cyrillic-iso8859-5
 '(
   (176 208)				;cyrillic letter a
   (167 247)				;cyrillic letter yi
   (205 237)				;cyrillic letter e
   (174 254)				;cyrillic letter short u
   (177 209)				;cyrillic letter be
   (197 229)				;cyrillic letter ha
   (184 216)				;cyrillic letter i
   (180 212)				;cyrillic letter de
   (198 230)				;cyrillic letter tse
   (206 238)				;cyrillic letter yu
   (190 222)				;cyrillic letter o
   (168 248)				;cyrillic letter je
   (202 234)				;cyrillic letter hard sign
   (199 231)				;cyrillic letter che
   (164 244)				;cyrillic letter ukrainian ie
   (162 242)				;cyrillic letter dje
   (179 211)				;cyrillic letter ghe
   (195 227)				;cyrillic letter u
   (191 223)				;cyrillic letter pe
   (163 243)				;cyrillic letter gje
   (194 226)				;cyrillic letter te
   (172 252)				;cyrillic letter kje
   (178 210)				;cyrillic letter ve
   (169 249)				;cyrillic letter lje
   (200 232)				;cyrillic letter sha
   (170 250)				;cyrillic letter nje
   (183 215)				;cyrillic letter ze
   (165 245)				;cyrillic letter dze
   (203 235)				;cyrillic letter yeru
   (201 233)				;cyrillic letter shcha
   (182 214)				;cyrillic letter zhe
   (175 255)				;cyrillic letter dzhe
   (196 228)				;cyrillic letter ef
   (186 218)				;cyrillic letter ka
   (204 236)				;cyrillic letter soft sign
   (181 213)				;cyrillic letter ie
   (187 219)				;cyrillic letter el
   (188 220)				;cyrillic letter em
   (189 221)				;cyrillic letter en
   (171 251)				;cyrillic letter tshe
   (192 224)				;cyrillic letter er
   (161 241)				;cyrillic letter io
   (166 246)				;cyrillic letter byelorussian-ukrainian i
   (193 225)				;cyrillic letter es
   (185 217)				;cyrillic letter short i
   (207 239)				;cyrillic letter ya
   ))

;; This is our utility function; we don't want it in the dumped XEmacs.

(fmakunbound 'setup-case-pairs)

;;; cyrillic.el ends here
