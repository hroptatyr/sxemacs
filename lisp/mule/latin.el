;;; latin.el --- Support for Latin charsets.

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Author: Hrvoje Niksic <hniksic@xemacs.org>
;; Maintainer: XEmacs Development Team
;; Keywords: multilingual, European, dumped

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

;; This file is meant to provide support for Latin character sets.
;; The place for that used to be `european.el', but I am hesitant to
;; change that file, as it is full of old cruft that I hope to phase
;; out.  Currently this file provides only the case table setup.


;;; Code:

;; Case table setup.  We set up all the case tables using
;; put-case-table-pair.  The data for this comes from FSF Emacs 20.7
;; (lisp/international/latin-*.el), written by several people and
;; updated by Erik Naggum.

(defun setup-case-pairs (charset pairs)
  (let ((tbl (standard-case-table)))
    (loop for (uc lc) in pairs do
      (put-case-table-pair (make-char charset uc) (make-char charset lc) tbl))))

;; Latin 1.

(setup-case-pairs
 'latin-iso8859-1
 '((192 224)				;latin letter a with grave
   (193 225)				;latin letter a with acute
   (194 226)				;latin letter a with circumflex
   (195 227)				;latin letter a with tilde
   (196 228)				;latin letter a with diaeresis
   (197 229)				;latin letter a with ring above
   (198 230)				;latin letter ae
   (199 231)				;latin letter c with cedilla
   (200 232)				;latin letter e with grave
   (201 233)				;latin letter e with acute
   (202 234)				;latin letter e with circumflex
   (203 235)				;latin letter e with diaeresis
   (204 236)				;latin letter i with grave
   (205 237)				;latin letter i with acute
   (206 238)				;latin letter i with circumflex
   (207 239)				;latin letter i with diaeresis
   (208 240)				;latin letter eth
   (209 241)				;latin letter n with tilde
   (210 242)				;latin letter o with grave
   (211 243)				;latin letter o with acute
   (212 244)				;latin letter o with circumflex
   (213 245)				;latin letter o with tilde
   (214 246)				;latin letter o with diaeresis
   (216 248)				;latin letter o with stroke
   (217 249)				;latin letter u with grave
   (218 250)				;latin letter u with acute
   (219 251)				;latin letter u with circumflex
   (220 252)				;latin letter u with diaeresis
   (221 253)				;latin letter y with acute
   (222 254)				;latin letter thorn
   ))

;; Latin 2.

(setup-case-pairs
 'latin-iso8859-2
 '((161 177)				;latin letter a with ogonek
   (163 179)				;latin letter l with stroke
   (165 181)				;latin letter l with caron
   (166 182)				;latin letter s with acute
   (169 185)				;latin letter s with caron
   (170 186)				;latin letter s with cedilla
   (171 187)				;latin letter t with caron
   (172 188)				;latin letter z with acute
   (174 190)				;latin letter z with caron
   (175 191)				;latin letter z with dot above
   (192 224)				;latin letter r with acute
   (193 225)				;latin letter a with acute
   (194 226)				;latin letter a with circumflex
   (195 227)				;latin letter a with breve
   (196 228)				;latin letter a with diaeresis
   (197 229)				;latin letter l with acute
   (198 230)				;latin letter c with acute
   (199 231)				;latin letter c with cedilla
   (200 232)				;latin letter c with caron
   (201 233)				;latin letter e with acute
   (202 234)				;latin letter e with ogonek
   (203 235)				;latin letter e with diaeresis
   (204 236)				;latin letter e with caron
   (205 237)				;latin letter i with acute
   (206 238)				;latin letter i with circumflex
   (207 239)				;latin letter d with caron
   (208 240)				;latin letter d with stroke
   (209 241)				;latin letter n with acute
   (210 242)				;latin letter n with caron
   (211 243)				;latin letter o with acute
   (212 244)				;latin letter o with circumflex
   (213 245)				;latin letter o with double acute
   (214 246)				;latin letter o with diaeresis
   (216 248)				;latin letter r with caron
   (217 249)				;latin letter u with ring above
   (218 250)				;latin letter u with acute
   (219 251)				;latin letter u with double acute
   (220 252)				;latin letter u with diaeresis
   (221 253)				;latin letter y with acute
   (222 254)				;latin letter t with cedilla
   ))

;; Latin 3.

(setup-case-pairs
 'latin-iso8859-3
 '((161 177)				;latin letter h with stroke
   (166 182)				;latin letter h with circumflex
   (170 186)				;latin letter s with cedilla
   (171 187)				;latin letter g with breve
   (172 188)				;latin letter j with circumflex
   (175 191)				;latin letter z with dot above
   (192 224)				;latin letter a with grave
   (193 225)				;latin letter a with acute
   (194 226)				;latin letter a with circumflex
   (196 228)				;latin letter a with diaeresis
   (197 229)				;latin letter c with dot above
   (198 230)				;latin letter c with circumflex
   (199 231)				;latin letter c with cedilla
   (200 232)				;latin letter e with grave
   (201 233)				;latin letter e with acute
   (202 234)				;latin letter e with circumflex
   (203 235)				;latin letter e with diaeresis
   (204 236)				;latin letter i with grave
   (205 237)				;latin letter i with acute
   (206 238)				;latin letter i with circumflex
   (207 239)				;latin letter i with diaeresis
   (209 241)				;latin letter n with tilde
   (210 242)				;latin letter o with grave
   (211 243)				;latin letter o with acute
   (212 244)				;latin letter o with circumflex
   (213 245)				;latin letter g with dot above
   (214 246)				;latin letter o with diaeresis
   (216 248)				;latin letter g with circumflex
   (217 249)				;latin letter u with grave
   (218 250)				;latin letter u with acute
   (219 251)				;latin letter u with circumflex
   (220 252)				;latin letter u with diaeresis
   (221 253)				;latin letter u with breve
   (222 254)				;latin letter s with circumflex
   ))

;; Latin 4.

(setup-case-pairs
 'latin-iso8859-4
 '((161 177)				;latin letter a with ogonek
   (163 179)				;latin letter r with cedilla
   (165 181)				;latin letter i with tilde
   (166 182)				;latin letter l with cedilla
   (169 185)				;latin letter s with caron
   (170 186)				;latin letter e with macron
   (171 187)				;latin letter g with cedilla
   (172 188)				;latin letter t with stroke
   (174 190)				;latin letter z with caron
   (189 191)				;eng
   (192 224)				;latin letter a with macron
   (193 225)				;latin letter a with acute
   (194 226)				;latin letter a with circumflex
   (195 227)				;latin letter a with tilde
   (196 228)				;latin letter a with diaeresis
   (197 229)				;latin letter a with ring above
   (198 230)				;latin letter ae
   (199 231)				;latin letter i with ogonek
   (200 232)				;latin letter c with caron
   (201 233)				;latin letter e with acute
   (202 234)				;latin letter e with ogonek
   (203 235)				;latin letter e with diaeresis
   (204 236)				;latin letter e with dot above
   (205 237)				;latin letter i with acute
   (206 238)				;latin letter i with circumflex
   (207 239)				;latin letter i with macron
   (208 240)				;latin letter d with stroke
   (209 241)				;latin letter n with cedilla
   (210 242)				;latin letter o with macron
   (211 243)				;latin letter k with cedilla
   (212 244)				;latin letter o with circumflex
   (213 245)				;latin letter o with tilde
   (214 246)				;latin letter o with diaeresis
   (216 248)				;latin letter o with stroke
   (217 249)				;latin letter u with ogonek
   (218 250)				;latin letter u with acute
   (219 251)				;latin letter u with circumflex
   (220 252)				;latin letter u with diaeresis
   (221 253)				;latin letter u with tilde
   (222 254)				;latin letter u with macron
   ))

;; Latin 5.  Currently unsupported.

;(setup-case-pairs
; 'latin-iso8859-5
; '((192 224)				;latin letter a with grave
;   (193 225)				;latin letter a with acute
;   (194 226)				;latin letter a with circumflex
;   (195 227)				;latin letter a with tilde
;   (196 228)				;latin letter a with diaeresis
;   (197 229)				;latin letter a with ring above
;   (198 230)				;latin letter ae
;   (199 231)				;latin letter c with cedilla
;   (200 232)				;latin letter e with grave
;   (201 233)				;latin letter e with acute
;   (203 235)				;latin letter e with diaeresis
;   (205 237)				;latin letter i with acute
;   (206 238)				;latin letter i with circumflex
;   (208 240)				;latin letter g with breve
;   (209 241)				;latin letter n with tilde
;   (210 242)				;latin letter o with grave
;   (211 243)				;latin letter o with acute
;   (212 244)				;latin letter o with circumflex
;   (213 245)				;latin letter o with tilde
;   (214 246)				;latin letter o with diaeresis
;   (216 248)				;latin letter o with stroke
;   (217 249)				;latin letter u with grave
;   (218 250)				;latin letter u with acute
;   (219 251)				;latin letter u with circumflex
;   (220 252)				;latin letter u with diaeresis
;   (222 254)				;latin letter s with cedilla
;   ))

;; This is our utility function; we don't want it in the dumped XEmacs.

(fmakunbound 'setup-case-pairs)
