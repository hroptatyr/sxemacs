;;; vietnamese.el --- Support for Vietnamese -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Keywords: multilingual, Vietnamese

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

;;; Commentary:

;; For Vietnames, the character sets VISCII and VSCII are supported.

;;; Code:

(eval-and-compile

(defvar viet-viscii-decode-table
  [;; VISCII is a full 8-bit code.
   0 1 ?,2F(B 3 4 ?,2G(B ?,2g(B 7 8 9 10 11 12 13 14 15
   16 17 18 19 ?,2V(B 21 22 23 24 ?,2[(B 26 27 28 29 ?,2\(B 31
   32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
   48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
   64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
   80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
   96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 
   ?,2U(B ?,2!(B ?,2"(B ?,2#(B ?,2$(B ?,2%(B ?,2&(B ?,2'(B ?,2((B ?,2)(B ?,2*(B ?,2+(B ?,2,(B ?,2-(B ?,2.(B ?,2/(B
   ?,20(B ?,21(B ?,22(B ?,25(B ?,2~(B ?,2>(B ?,26(B ?,27(B ?,28(B ?,2v(B ?,2w(B ?,2o(B ?,2|(B ?,2{(B ?,2x(B ?,2O(B
   ?,2u(B ?,1!(B ?,1"(B ?,1#(B ?,1$(B ?,1%(B ?,1&(B ?,1'(B ?,1((B ?,1)(B ?,1*(B ?,1+(B ?,1,(B ?,1-(B ?,1.(B ?,1/(B
   ?,10(B ?,11(B ?,12(B ?,2^(B ?,2=(B ?,15(B ?,16(B ?,17(B ?,18(B ?,2q(B ?,2Q(B ?,2W(B ?,2X(B ?,1=(B ?,1>(B ?,2_(B
   ?,2`(B ?,2a(B ?,2b(B ?,2c(B ?,2d(B ?,2e(B ?,1F(B ?,1G(B ?,2h(B ?,2i(B ?,2j(B ?,2k(B ?,2l(B ?,2m(B ?,2n(B ?,1O(B
   ?,2p(B ?,1Q(B ?,2r(B ?,2s(B ?,2t(B ?,1U(B ?,1V(B ?,1W(B ?,1X(B ?,2y(B ?,2z(B ?,1[(B ?,1\(B ?,2}(B ?,1^(B ?,1_(B
   ?,1`(B ?,1a(B ?,1b(B ?,1c(B ?,1d(B ?,1e(B ?,1f(B ?,1g(B ?,1h(B ?,1i(B ?,1j(B ?,1k(B ?,1l(B ?,1m(B ?,1n(B ?,1o(B
   ?,1p(B ?,1q(B ?,1r(B ?,1s(B ?,1t(B ?,1u(B ?,1v(B ?,1w(B ?,1x(B ?,1y(B ?,1z(B ?,1{(B ?,1|(B ?,1}(B ?,1~(B ?,2f(B ]
  "Vietnamese VISCII decoding table.")

(defvar viet-viscii-encode-table
  (let ((table-lower (make-vector 128 0))
	(table-upper (make-vector 128 0))
	(i 0)
	char-component)
    (while (< i 256)
      (setq char-component
	    (split-char (aref viet-viscii-decode-table i)))
      (cond ((eq (car char-component) 'vietnamese-viscii-lower)
	     (aset table-lower (nth 1 char-component) i))
	    ((eq (car char-component) 'vietnamese-viscii-upper)
	     (aset table-upper (nth 1 char-component) i)))
      (setq i (1+ i)))
    (cons table-lower table-upper))
  "Vietnamese VISCII encoding table.
Cons of tables for encoding lower-case chars and upper-case characters.
Both tables are indexed by the position code of Vietnamese characters.")

(defvar viet-vscii-decode-table
  [;; VSCII is a full 8-bit code.
   0 ?,2z(B ?,2x(B 3 ?,2W(B ?,2X(B ?,2f(B 7 8 9 10 11 12 13 14 15
   16 ?,2Q(B ?,2_(B ?,2O(B ?,2V(B ?,2[(B ?,2}(B ?,2\(B 24 25 26 27 28 29 30 31
   32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
   48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
   64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
   80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
   96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 
   ?,2`(B ?,2d(B ?,2c(B ?,2a(B ?,2U(B ?,2#(B ?,2'(B ?,2h(B ?,2k(B ?,2((B ?,2i(B ?,2)(B ?,2.(B ?,2l(B ?,2o(B ?,2n(B
   ?,2m(B ?,28(B ?,2r(B ?,2v(B ?,2u(B ?,2s(B ?,2w(B ?,25(B ?,26(B ?,27(B ?,2^(B ?,2>(B ?,2~(B ?,2y(B ?,2|(B ?,2{(B
   160 ?,2e(B ?,2b(B ?,2j(B ?,2t(B ?,2=(B ?,2_(B ?,2p(B ?,1e(B ?,1b(B ?,1j(B ?,1t(B ?,1>(B ?,1y(B ?,1p(B ?,2"(B
   192 193 194 195 196 ?,1`(B ?,1d(B ?,1c(B ?,1a(B ?,1U(B ?,2F(B ?,1"(B ?,1F(B ?,1G(B ?,1!(B ?,2G(B
   ?,2!(B ?,2%(B ?,2&(B ?,2g(B ?,2%(B ?,2+(B ?,1#(B ?,1%(B ?,1&(B ?,1g(B ?,1$(B ?,1'(B ?,1h(B ?,2,(B ?,1k(B ?,1((B
   ?,1i(B ?,1)(B ?,1+(B ?,1,(B ?,1-(B ?,1*(B ?,1.(B ?,1l(B ?,1o(B ?,2-(B ?,2*(B ?,20(B ?,1n(B ?,1m(B ?,18(B ?,1r(B
   ?,21(B ?,1v(B ?,1u(B ?,1s(B ?,1w(B ?,10(B ?,11(B ?,12(B ?,1/(B ?,15(B ?,16(B ?,17(B ?,1^(B ?,1>(B ?,1~(B ?,1y(B
   ?,22(B ?,1|(B ?,1{(B ?,1z(B ?,1x(B ?,1W(B ?,1X(B ?,1f(B ?,1Q(B ?,1q(B ?,1O(B ?,1V(B ?,1[(B ?,1}(B ?,1\(B ?,2/(B]
  "Vietnamese VSCII decoding table.")

(defvar viet-vscii-encode-table
  (let ((table-lower (make-vector 128 0))
	(table-upper (make-vector 128 0))
	(i 0)
	char-component)
    (while (< i 256)
      (setq char-component
	    (split-char (aref viet-vscii-decode-table i)))
      (cond ((eq (car char-component) 'vietnamese-viscii-lower)
	     (aset table-lower (nth 1 char-component) i))
	    ((eq (car char-component) 'vietnamese-viscii-upper)
	     (aset table-upper (nth 1 char-component) i)))
      (setq i (1+ i)))
    (cons table-lower table-upper))
  "Vietnamese VSCII encoding table.
Cons of tables for encoding lower-case chars and upper-case characters.
Both tables are indexed by the position code of Vietnamese characters.")

)

(define-ccl-program ccl-decode-viscii
  `(3
    ((read r0)
     (loop
      (write-read-repeat r0 ,viet-viscii-decode-table))
     ))
  "CCL program to decode VISCII 1.1")

;; Multibyte form of a Vietnamese character is as follows (3-byte):
;;   LEADING-CODE-PRIVATE-11 LEADING-CODE-EXTENDED-11 POSITION-CODE
;; where LEADING-CODE-EXTENDED-11 for Vietnamese is
;; `vietnamese-viscii-lower' or `vietnamese-viscii-upper'.

(defvar leading-code-private-11 #x9E)

(define-ccl-program ccl-encode-viscii
  `(1
     ((read r0)
      (loop
       (if (r0 < 128)
	   ;; ASCII
	   (write-read-repeat r0)
	 ;; not ASCII
	 (if (r0 != ,leading-code-private-11)
	     ;; not Vietnamese
	     (write-read-repeat r0)
	   ((read-if (r0 == ,(charset-id 'vietnamese-viscii-lower))
	     (;; Vietnamese lower
	      (read r0)
	      (r0 -= 128)
	      (write-read-repeat r0 ,(car viet-viscii-encode-table)))
	     (if (r0 == ,(charset-id 'vietnamese-viscii-upper))
		 (;; Vietnamese upper
		  (read r0)
		  (r0 -= 128)
		  (write-read-repeat r0 ,(cdr viet-viscii-encode-table)))
	       ;; not Vietnamese
	       (write-read-repeat r0)))))))))
  "CCL program to encode VISCII 1.1")

(define-ccl-program ccl-encode-viscii-font
  `(0
    ;; In:  R0:vietnamese-viscii-lower/vietnamese-viscii-upper
    ;;      R1:position code
    ;; Out: R1:font code point
    (if (r0 == ,(charset-id 'vietnamese-viscii-lower))
	(r1 = r1 ,(car viet-viscii-encode-table))
      (r1 = r1 ,(cdr viet-viscii-encode-table)))
    )
  "CCL program to encode Vietnamese chars to VISCII 1.1 font")

(define-ccl-program ccl-decode-vscii
  `(3
    ((read r0)
     (loop
      (write-read-repeat r0 ,viet-vscii-decode-table))
     ))
  "CCL program to decode VSCII-1.")

(define-ccl-program ccl-encode-vscii
  `(1
    ((read r0)
     (loop
      (if (r0 < 128)
	  ;; ASCII
	  (write-read-repeat r0)
	;; not ASCII 
	(if (r0 != ,leading-code-private-11)
	    ;; not Vietnamese
	    (write-read-repeat r0)
	  (read-if (r0 == ,(charset-id 'vietnamese-viscii-lower))
		   (;; Vietnamese lower
		    (read r0)
		    (r0 -= 128)
		    (write-read-repeat r0 ,(car viet-vscii-encode-table)))
		   (if (r0 == ,(charset-id 'vietnamese-viscii-upper))
		       (;; Vietnamese upper
			(read r0)
			(r0 -= 128)
			(write-read-repeat r0 ,(cdr viet-vscii-encode-table)))
		     ;; not Vietnamese
		     (write-read-repeat r0))))))))
  "CCL program to encode VSCII-1.")

(define-ccl-program ccl-encode-vscii-font
  `(0
    ;; In:  R0:vietnamese-viscii-lower/vietnamese-viscii-upper
    ;;      R1:position code
    ;; Out: R1:font code point
    (if (r0 == ,(charset-id 'vietnamese-viscii-lower))
	(r1 = r1 ,(car viet-vscii-encode-table))
      (r1 = r1 ,(cdr viet-vscii-encode-table)))
    )
  "CCL program to encode Vietnamese chars to VSCII-1 font.")


(make-coding-system
 'viscii 'ccl
 "Coding-system used for VISCII 1.1."
 `(mnemonic "VISCII"
   decode ccl-decode-viscii
   encode ccl-encode-viscii))

;; it is not correct, but XEmacs doesn't have `ccl' category...
(coding-system-put 'viscii 'category 'iso-8-1)

;; (make-coding-system
;;  'vietnamese-viscii 4 ?V
;;  "8-bit encoding for Vietnamese VISCII 1.1 (MIME:VISCII)"
;;  '(ccl-decode-viscii . ccl-encode-viscii)
;;  '((safe-charsets ascii vietnamese-viscii-lower vietnamese-viscii-upper)
;;    (mime-charset . viscii)
;;    (valid-codes (0 . 255))))

;; (define-coding-system-alias 'viscii 'vietnamese-viscii)

(make-coding-system
 'vscii 'ccl
 "Coding-system used for VSCII 1.1."
 `(mnemonic "VSCII"
   decode ccl-decode-vscii
   encode ccl-encode-vscii))

;; (make-coding-system
;;  'vietnamese-vscii 4 ?v
;;  "8-bit encoding for Vietnamese VSCII-1"
;;  '(ccl-decode-vscii . ccl-encode-vscii)
;;  '((safe-charsets ascii vietnamese-viscii-lower vietnamese-viscii-upper)
;;    (valid-codes (0 . 255))))

;; (define-coding-system-alias 'vscii 'vietnamese-vscii)

(make-coding-system
 'viqr 'no-conversion
 "Coding-system used for VIQR."
 '(mnemonic "VIQR"
   eol-type lf
   post-read-conversion viqr-post-read-conversion
   pre-write-conversion viqr-pre-write-conversion))

;; (make-coding-system
;;  'vietnamese-viqr 0 ?q
;;  "Vietnamese latin transcription (VIQR)"
;;  nil
;;  '((safe-charsets ascii vietnamese-viscii-lower vietnamese-viscii-upper)
;;    (post-read-conversion . viqr-post-read-conversion)
;;    (pre-write-conversion . viqr-pre-write-conversion)
;;    (charset-origin-alist
;;     (vietnamese-viscii-lower "VISCII" viet-encode-viscii-char)
;;     (vietnamese-viscii-upper "VISCII" viet-encode-viscii-char))))

;; (define-coding-system-alias 'viqr 'vietnamese-viqr)

;; For VISCII users
(set-charset-ccl-program 'vietnamese-viscii-lower
			 'ccl-encode-viscii-font)
(set-charset-ccl-program 'vietnamese-viscii-upper
			 'ccl-encode-viscii-font)
;; For VSCII users
(set-charset-ccl-program 'vietnamese-viscii-lower 'ccl-encode-vscii-font)
(set-charset-ccl-program 'vietnamese-viscii-upper 'ccl-encode-vscii-font)

;; (setq font-ccl-encoder-alist
;;       (cons (cons "viscii" ccl-encode-viscii-font) font-ccl-encoder-alist))

;; (setq font-ccl-encoder-alist
;;       (cons (cons "vscii" ccl-encode-vscii-font) font-ccl-encoder-alist))

;; (defvar viet-viscii-nonascii-translation-table
;;   (make-translation-table-from-vector viet-viscii-decode-table)
;;   "Value of `nonascii-translation-table' in Vietnamese language environment.")

(set-language-info-alist
 "Vietnamese" '((charset vietnamese-viscii-lower vietnamese-viscii-upper)
		(coding-system viscii vscii viqr)
		(coding-priority viscii)
		(input-method . "vietnamese-viqr")
		(features viet-util)
		(sample-text . "Vietnamese (Ti,1*(Bng Vi,1.(Bt)	Ch,1`(Bo b,1U(Bn")
		(documentation . "For Vietnamese, Emacs uses special charsets internally.
They can be decoded from and encoded to VISCC, VSCII, and VIQR.
Current setting put higher priority to the coding system VISCII than VSCII.
If you prefer VSCII, please do: (prefer-coding-system 'vietnamese-vscii)")
		))

;;; vietnamese.el ends here
