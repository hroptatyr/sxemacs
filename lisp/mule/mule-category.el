;;; mule-category.el --- category functions for SXEmacs/Mule.

;; Copyright (C) 1992,93,94,95 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1997, 1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
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

;; Functions for working with category tables, which are a particular
;; type of char table.  Some function names / arguments should be
;; parallel with syntax tables.

;; Written by Ben Wing <ben@xemacs.org>.  The initialization code
;; at the end of this file comes from Mule.
;; Some bugfixes by Jareth Hein <jhod@po.iijnet.or.jp>

;;; Code:

(defvar defined-category-hashtable (make-hash-table :size 50))

(defun define-category (designator doc-string)
  "Make a new category whose designator is DESIGNATOR.
DESIGNATOR should be a visible letter of ' ' thru '~'.
STRING is a doc string for the category.
Letters of 'a' thru 'z' are already used or kept for the system."
  (check-argument-type 'category-designator-p designator)
  (check-argument-type 'stringp doc-string)
  (puthash designator doc-string defined-category-hashtable))

(defun undefine-category (designator)
  "Undefine DESIGNATOR as a designator for a category."
  (check-argument-type 'category-designator-p designator)
  (remhash designator defined-category-hashtable))

(defun defined-category-p (designator)
  "Return non-nil if DESIGNATOR is a designator for a defined category."
  (and (category-designator-p designator)
       (gethash designator defined-category-hashtable)))

(defun defined-category-list ()
  "Return a list of the currently defined categories.
Categories are given by their designators."
  (let (list)
    (maphash #'(lambda (key value)
		 (setq list (cons key list)))
	     defined-category-hashtable)
    (nreverse list)))

(defun undefined-category-designator ()
  "Return an undefined category designator, or nil if there are none."
  (let ((a 32) found)
    (while (and (< a 127) (not found))
      (unless (gethash a defined-category-hashtable)
	(setq found (make-char 'ascii a)))
      (setq a (1+ a)))
    found))

(defun category-doc-string (designator)
  "Return the doc-string for the category denoted by DESIGNATOR."
  (check-argument-type 'defined-category-p designator)
  (gethash designator defined-category-hashtable))

(defun modify-category-entry (char-range designator &optional category-table reset)
  "Add a category to the categories associated with CHAR-RANGE.
CHAR-RANGE is a single character or a range of characters,
 as per `put-char-table'.
The category is given by a designator character.
The changes are made in CATEGORY-TABLE, which defaults to the current
 buffer's category table.
If optional fourth argument RESET is non-nil, previous categories associated
 with CHAR-RANGE are removed before adding the specified category."
  (or category-table (setq category-table (category-table)))
  (check-argument-type 'category-table-p category-table)
  (check-argument-type 'defined-category-p designator)
  (if reset
      ;; clear all existing stuff.
      (put-char-table char-range nil category-table))
  (map-char-table
   #'(lambda (key value)
       ;; make sure that this range has a bit-vector assigned to it
       (if (not (bit-vector-p value))
	   (setq value (make-bit-vector 95 0))
	 (setq value (copy-sequence value)))
       ;; set the appropriate bit in that vector.
       (aset value (- designator 32) 1)
       ;; put the vector back, thus assuring we have a unique setting for this range
       (put-char-table key value category-table))
   category-table char-range))

(defun char-category-list (character &optional category-table)
  "Return a list of the categories that CHARACTER is in.
CATEGORY-TABLE defaults to the current buffer's category table.
The categories are given by their designators."
  (or category-table (setq category-table (category-table)))
  (check-argument-type 'category-table-p category-table)
  (let ((vec (get-char-table character category-table)))
    (if (null vec) nil
      (let ((a 32) list)
	(while (< a 127)
	  (if (= 1 (aref vec (- a 32)))
	      (setq list (cons (make-char 'ascii a) list)))
	  (setq a (1+ a)))
	(nreverse list)))))

;; implemented in C, file chartab.c (97/3/14 jhod@po.iijnet.or.jp)
;(defun char-in-category-p (char category &optional table)
;  "Return non-nil if CHAR is in CATEGORY.
;TABLE defaults to the current buffer's category table.
;Categories are specified by their designators."
;  (or table (setq table (category-table)))
;  (check-argument-type 'category-table-p table)
;  (check-argument-type 'category-designator-p category)
;  (let ((vec (get-char-table char table)))
;    (if (null vec) nil
;      (= 1 (aref vec (- category 32))))))

(defun describe-category ()
  "Describe the category specifications in the category table.
The descriptions are inserted in a buffer, which is then displayed."
  (interactive)
  (with-displaying-help-buffer
   (lambda ()
     (describe-category-table (category-table) standard-output))))

(defun describe-category-table (table stream)
  (let (first-char
	last-char
	prev-val
	(describe-one
	 (lambda (first last value stream)
	   (if (and (bit-vector-p value)
		    (> (reduce '+ value) 0))
	       (progn
		 (if (equal first last)
		     (cond ((vectorp first)
			    (princ (format "%s, row %d"
					   (charset-name
					    (aref first 0))
					   (aref first 1))
				   stream))
			   ((charsetp first)
			    (princ (charset-name first) stream))
			   (t (princ first stream)))
		   (cond ((vectorp first)
			  (princ (format "%s, rows %d .. %d"
					 (charset-name
					  (aref first 0))
					 (aref first 1)
					 (aref last 1))
				 stream))
			 (t
			  (princ (format "%s .. %s" first last)
				 stream))))
		 (describe-category-code value stream))))))
    (map-char-table
     (lambda (range value)
       (if (and (or
		 (and (characterp range)
		      (characterp first-char)
		      (eq (char-charset range) (char-charset first-char))
		      (= (char-to-int last-char) (1- (char-to-int range))))
		 (and (vectorp range)
		      (vectorp first-char)
		      (eq (aref range 0) (aref first-char 0))
		      (= (aref last-char 1) (1- (aref range 1))))
		 (equal value prev-val)))
	   (setq last-char range)
	 (if first-char
	     (progn
	       (funcall describe-one first-char last-char prev-val stream)
	       (setq first-char nil)))
	 (funcall describe-one range range value stream))
       nil)
     table)
    (if first-char
	(funcall describe-one first-char last-char prev-val stream))))

(defun describe-category-code (code stream)
  (let ((standard-output (or stream standard-output)))
    (princ "\tin categories: ")
    (if (not (bit-vector-p code))
	(princ "(none)")
      (let ((i 0)
	    already-matched)
	(while (< i 95)
	  (if (= 1 (aref code i))
	      (progn
		(if (not already-matched)
		    (setq already-matched t)
		  (princ " "))
		(princ (int-to-char (+ 32 i)))))
	  (setq i (1+ i)))
	(if (not already-matched)
	    (princ "(none)")))
      (let ((i 0))
	(while (< i 95)
	  (if (= 1 (aref code i))
	      (princ (format "\n\t\tmeaning: %s"
			    (category-doc-string (int-to-char (+ 32 i))))))
	  (setq i (1+ i)))))
    (terpri)))

(defconst predefined-category-list
  '((latin-iso8859-1	?l "Latin-1 through Latin-5 character set")
    (latin-iso8859-2	?l)
    (latin-iso8859-3	?l)
    (latin-iso8859-4	?l)
    (latin-iso8859-9	?l)
    (cyrillic-iso8859-5 ?y "Cyrillic character set")
    (arabic-iso8859-6	?b "Arabic character set")
    (greek-iso8859-7	?g "Greek character set")
    (hebrew-iso8859-8	?w "Hebrew character set")
    (katakana-jisx0201	?k "Japanese 1-byte Katakana character set")
    (latin-jisx0201	?r "Japanese 1-byte Roman character set")
    (japanese-jisx0208-1978 ?j "Japanese 2-byte character set (old)")
    (japanese-jisx0208	?j "Japanese 2-byte character set")
    (japanese-jisx0212	?j)
    (chinese-gb2312	?c "Chinese GB (China, PRC) 2-byte character set")
    (chinese-cns11643-1	?t "Chinese Taiwan (CNS or Big5) 2-byte character set")
    (chinese-cns11643-2	?t)
    (chinese-big5-1	?t)
    (chinese-big5-2	?t)
    (korean-ksc5601	?h "Hangul (Korean) 2-byte character set")
    )
  "List of predefined categories.
Each element is a list of a charset, a designator, and maybe a doc string.")

(let (i l)
  (define-category ?a "ASCII character set.")
  (define-category ?l "Latin-1 through Latin-5 character set")
  (setq i 32)
  (while (< i 127)
    (modify-category-entry i ?a)
    (modify-category-entry i ?l)
    (setq i (1+ i)))
  (setq l predefined-category-list)
  (while l
    (if (and (nth 2 (car l))
	     (not (defined-category-p (nth 2 (car l)))))
	(define-category (nth 1 (car l)) (nth 2 (car l))))
    (modify-category-entry (car (car l)) (nth 1 (car l)))
    (setq l (cdr l))))

;;; Setting word boundary.

(setq word-combining-categories
      '((?l . ?l)))

(setq word-separating-categories	;  (2-byte character sets)
      '((?A . ?K)			; Alpha numeric - Katakana
	(?A . ?C)			; Alpha numeric - Chinese
	(?H . ?A)			; Hiragana - Alpha numeric
	(?H . ?K)			; Hiragana - Katakana
	(?H . ?C)			; Hiragana - Chinese
	(?K . ?A)			; Katakana - Alpha numeric
	(?K . ?C)			; Katakana - Chinese
	(?C . ?A)			; Chinese - Alpha numeric
	(?C . ?K)			; Chinese - Katakana
	))

;;; At the present, I know Japanese and Chinese text can
;;; break line at any point under a restriction of 'kinsoku'.
;;; #### SJT this needs to be set by language environments and probably should
;;; be buffer-local---strategy for dealing with this: check all $language.el
;;; files and also mule-base/$language-utils.el files for variables set;
;;; these should be made buffer local and some kind of a- or p-list of vars
;;; to be set for a language environment created.
(defvar word-across-newline "\\(\\cj\\|\\cc\\|\\ct\\)"
  "Regular expression of such characters which can be a word across newline.")

(defvar ascii-char "[\40-\176]")
(defvar ascii-space "[ \t]")
(defvar ascii-symbols "[\40-\57\72-\100\133-\140\173-\176]")
(defvar ascii-numeric "[\60-\71]")
(defvar ascii-English-Upper "[\101-\132]")
(defvar ascii-English-Lower "[\141-\172]")
(defvar ascii-alphanumeric "[\60-\71\101-\132\141-\172]")

(defvar kanji-char "\\cj")
(defvar kanji-space "　")
(defvar kanji-symbols "\\cS")
(defvar kanji-numeric "[０-９]")
(defvar kanji-English-Upper "[Ａ-Ｚ]")
(defvar kanji-English-Lower  "[ａ-ｚ]")
(defvar kanji-hiragana "\\cH")
(defvar kanji-katakana "\\cK")
(defvar kanji-Greek-Upper "[Α-Ω]")
(defvar kanji-Greek-Lower "[α-ω]")
(defvar kanji-Russian-Upper "[А-Я]")
(defvar kanji-Russian-Lower "[а-я]")
(defvar kanji-Kanji-1st-Level  "[亜-腕]")
(defvar kanji-Kanji-2nd-Level  "[弌-瑤]")

(defvar kanji-kanji-char "\\(\\cH\\|\\cK\\|\\cC\\)")
