;;; mule-charset.el --- Charset functions for Mule.

;; Copyright (C) 1992 Free Software Foundation, Inc.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1996 Sun Microsystems.

;; Author: Unknown
;; Keywords: i18n, mule, internal

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

;;; Synched up with: Not synched.  API at source level synched with FSF 20.3.9.

;;; Commentary:

;; These functions are not compatible at the bytecode level with Emacs/Mule,
;; and they never will be.  -sb [1999-05-26]

;;; Code:

;;;; Classifying text according to charsets

(defun charsets-in-region (start end &optional buffer)
  "Return a list of the charsets in the region between START and END.
BUFFER defaults to the current buffer if omitted."
  (let (list)
    (save-excursion
      (if buffer
	  (set-buffer buffer))
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	(while (not (eobp))
	  (let* (prev-charset
		 (ch (char-after (point)))
		 (charset (char-charset ch)))
	    (if (not (eq prev-charset charset))
		(progn
		  (setq prev-charset charset)
		  (or (memq charset list)
		      (setq list (cons charset list))))))
	  (forward-char))))
    list))

(defun charsets-in-string (string)
  "Return a list of the charsets in STRING."
  (let ((i 0)
	(len (length string))
	prev-charset charset list)
    (while (< i len)
      (setq charset (char-charset (aref string i)))
      (if (not (eq prev-charset charset))
	  (progn
	    (setq prev-charset charset)
	    (or (memq charset list)
		(setq list (cons charset list)))))
      (setq i (1+ i)))
    list))


;;;; Charset accessors

(defun charset-iso-graphic-plane (charset)
  "Return the `graphic' property of CHARSET.
See `make-charset'."
  (charset-property charset 'graphic))

(defun charset-iso-final-char (charset)
  "Return the final byte of the ISO 2022 escape sequence designating CHARSET."
  (charset-property charset 'final))

(defun charset-chars (charset)
  "Return the number of characters per dimension of CHARSET."
  (charset-property charset 'chars))

(defun charset-width (charset)
  "Return the number of display columns per character of CHARSET.
This only applies to TTY mode (under X, the actual display width can
be automatically determined)."
  (charset-property charset 'columns))

;; #### FSFmacs returns 0
(defun charset-direction (charset)
  "Return the display direction (0 for `l2r' or 1 for `r2l') of CHARSET.
Only left-to-right is currently implemented."
  (if (eq (charset-property charset 'direction) 'l2r)
      0
    1))

;; Not in Emacs/Mule
(defun charset-registry (charset)
  "Return the registry of CHARSET.
This is a regular expression matching the registry field of fonts
that can display the characters in CHARSET."
  (charset-property charset 'registry))

(defun charset-ccl-program (charset)
  "Return the CCL program of CHARSET.
See `make-charset'."
  (charset-property charset 'ccl-program))

(defun charset-bytes (charset)
  "Useless in XEmacs, returns 1."
   1)

(define-obsolete-function-alias 'charset-columns 'charset-width) ;; 19990409
(define-obsolete-function-alias 'charset-final 'charset-iso-final-char) ;; 19990409
(define-obsolete-function-alias 'charset-graphic 'charset-iso-graphic-plane) ;; 19990409
(define-obsolete-function-alias 'charset-doc-string 'charset-description) ;; 19990409

;;;; Define setf methods for all settable Charset properties

(defsetf charset-registry    set-charset-registry)
(defsetf charset-ccl-program set-charset-ccl-program)

;;; FSF compatibility functions
(defun charset-after (&optional pos)
  "Return charset of a character in current buffer at position POS.
If POS is nil, it defauls to the current point.
If POS is out of range, the value is nil."
  (when (null pos)
    (setq pos (point)))
  (check-argument-type 'integerp pos)
  (unless (or (< pos (point-min))
	      (> pos (point-max)))
    (char-charset (char-after pos))))

;; Yuck!
;; We're not going to support this.
;(defun charset-info (charset)
;  "Return a vector of information of CHARSET.
;The elements of the vector are:
;        CHARSET-ID, BYTES, DIMENSION, CHARS, WIDTH, DIRECTION,
;        LEADING-CODE-BASE, LEADING-CODE-EXT,
;        ISO-FINAL-CHAR, ISO-GRAPHIC-PLANE,
;        REVERSE-CHARSET, SHORT-NAME, LONG-NAME, DESCRIPTION,
;        PLIST,
;where
;CHARSET-ID (integer) is the identification number of the charset.
;BYTES (integer) is the length of multi-byte form of a character in
;  the charset: one of 1, 2, 3, and 4.
;DIMENSION (integer) is the number of bytes to represent a character of
;the charset: 1 or 2.
;CHARS (integer) is the number of characters in a dimension: 94 or 96.
;WIDTH (integer) is the number of columns a character in the charset
;  occupies on the screen: one of 0, 1, and 2.
;DIRECTION (integer) is the rendering direction of characters in the
;  charset when rendering.  If 0, render from left to right, else
;  render from right to left.
;LEADING-CODE-BASE (integer) is the base leading-code for the
;  charset.
;LEADING-CODE-EXT (integer) is the extended leading-code for the
;  charset.  All charsets of less than 0xA0 has the value 0.
;ISO-FINAL-CHAR (character) is the final character of the
;  corresponding ISO 2022 charset.
;ISO-GRAPHIC-PLANE (integer) is the graphic plane to be invoked
;  while encoding to variants of ISO 2022 coding system, one of the
;  following: 0/graphic-plane-left(GL), 1/graphic-plane-right(GR).
;REVERSE-CHARSET (integer) is the charset which differs only in
;  LEFT-TO-RIGHT value from the charset.  If there's no such a
;  charset, the value is -1.
;SHORT-NAME (string) is the short name to refer to the charset.
;LONG-NAME (string) is the long name to refer to the charset
;DESCRIPTION (string) is the description string of the charset.
;PLIST (property list) may contain any type of information a user
;  want to put and get by functions `put-charset-property' and
;  `get-charset-property' respectively."
;  (vector
;   (charset-id charset)
;   1
;   (charset-dimension charset)
;   (charset-chars charset)
;   (charset-width charset)
;   (charset-direction charset)
;   nil ;; (charset-leading-code-base (charset))
;   nil ;; (charset-leading-code-ext (charset))
;   (charset-iso-final-char charset)
;   (charset-iso-graphic-plane charset)
;   -1
;   (charset-short-name charset)
;   (charset-long-name charset)
;   (charset-description charset)
;   (charset-plist charset)))

;(make-compatible 'charset-info "Don't use this if you can help it.")

(defun define-charset (charset-id charset property-vector)
  "Define CHARSET-ID as the identification number of CHARSET with INFO-VECTOR.
If CHARSET-ID is nil, it is decided automatically, which means CHARSET is
 treated as a private charset.
INFO-VECTOR is a vector of the format:
   [DIMENSION CHARS WIDTH DIRECTION ISO-FINAL-CHAR ISO-GRAPHIC-PLANE
    SHORT-NAME LONG-NAME DESCRIPTION]
The meanings of each elements is as follows:
DIMENSION (integer) is the number of bytes to represent a character: 1 or 2.
CHARS (integer) is the number of characters in a dimension: 94 or 96.
WIDTH (integer) is the number of columns a character in the charset
occupies on the screen: one of 0, 1, and 2.

DIRECTION (integer) is the rendering direction of characters in the
charset when rendering.  If 0, render from left to right, else
render from right to left.

ISO-FINAL-CHAR (character) is the final character of the
corresponding ISO 2022 charset.

ISO-GRAPHIC-PLANE (integer) is the graphic plane to be invoked
while encoding to variants of ISO 2022 coding system, one of the
following: 0/graphic-plane-left(GL), 1/graphic-plane-right(GR).


SHORT-NAME (string) is the short name to refer to the charset.

LONG-NAME (string) is the long name to refer to the charset.

DESCRIPTION (string) is the description string of the charset."
  (make-charset charset (aref property-vector 8)
		(list
		 'short-name (aref property-vector 6)
		 'long-name (aref property-vector 7)
		 'dimension (aref property-vector 0)
		 'columns (aref property-vector 2)
		 'chars (aref property-vector 1)
		 'final (aref property-vector 4)
		 'graphic (aref property-vector 5)
		 'direction (aref property-vector 3))))

(make-compatible 'define-charset "")

;;; Charset property

(defalias 'get-charset-property 'get)
(defalias 'put-charset-property 'put)
(defalias 'charset-plist 'object-plist)
(defalias 'set-charset-plist 'setplist)

;; Setup auto-fill-chars for charsets that should invoke auto-filling.
;; SPACE and NEWLIE are already set.
(let ((l '(katakana-jisx0201
	   japanese-jisx0208 japanese-jisx0212
	   chinese-gb2312 chinese-big5-1 chinese-big5-2)))
  (while l
    (put-char-table (car l) t auto-fill-chars)
    (setq l (cdr l))))

;;; mule-charset.el ends here
