;; syntax.el --- Syntax-table hacking stuff, moved from syntax.c

;; Copyright (C) 1993, 1997 Free Software Foundation, Inc.
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

;;; Synched up with: FSF 19.28.

;;; Commentary:

;; This file is dumped with SXEmacs.

;; Note: FSF does not have a file syntax.el.  This stuff is
;; in syntax.c.  See comments there about not merging past 19.28.

;; Significantly hacked upon by Ben Wing.

;;; Code:

(defun make-syntax-table (&optional oldtable)
  "Return a new syntax table.
It inherits all characters from the standard syntax table."
  (make-char-table 'syntax))

(defun simple-set-syntax-entry (char spec table)
  (put-char-table char spec table))

(defun char-syntax-from-code (code)
  "Extract the syntax designator from the internal syntax code CODE.
CODE is the value actually contained in the syntax table."
  (if (consp code)
      (setq code (car code)))
  (aref (syntax-designator-chars) (logand code 127)))

(defun set-char-syntax-in-code (code desig)
  "Return a new internal syntax code whose syntax designator is DESIG.
Other characteristics are the same as in CODE."
  (let ((newcode (if (consp code) (car code) code)))
    (setq newcode (logior (string-match
			   (regexp-quote (char-to-string desig))
			   (syntax-designator-chars))
			  (logand newcode (lognot 127))))
    (if (consp code) (cons newcode (cdr code))
      newcode)))

(defun syntax-code-to-string (code)
  "Return a string equivalent to internal syntax code CODE.
The string can be passed to `modify-syntax-entry'.
If CODE is invalid, return nil."
  (let ((match (and (consp code) (cdr code)))
	(codes (syntax-designator-chars)))
    (if (consp code)
	(setq code (car code)))
    (if (or (not (integerp code))
	    (> (logand code 127) (length codes)))
	nil
      (with-output-to-string
       (let* ((spec (elt codes (logand code 127)))
	      (b3 (lsh code -16))
	      (start1  (/= 0 (logand b3 128))) ;logtest!
	      (start1b (/= 0 (logand b3  64)))
	      (start2  (/= 0 (logand b3  32)))
	      (start2b (/= 0 (logand b3  16)))
	      (end1    (/= 0 (logand b3   8)))
	      (end1b   (/= 0 (logand b3   4)))
	      (end2    (/= 0 (logand b3   2)))
	      (end2b   (/= 0 (logand b3   1)))
	      (prefix  (/= 0 (logand code 128)))
	      (single-char-p (or (= spec ?<) (= spec ?>)))
	      )
	 (write-char spec)
	 (write-char (if match match 32))
;;;	(if start1 (if single-char-p (write-char ?a) (write-char ?1)))
	 (if start1 (if single-char-p (write-char ? ) (write-char ?1)))
	 (if start2 (write-char ?2))
;;;	(if end1 (if single-char-p (write-char ?a) (write-char ?3)))
	 (if end1 (if single-char-p (write-char ? ) (write-char ?3)))
	 (if end2 (write-char ?4))
	 (if start1b (if single-char-p (write-char ?b) (write-char ?5)))
	 (if start2b (write-char ?6))
	 (if end1b (if single-char-p (write-char ?b) (write-char ?7)))
	 (if end2b (write-char ?8))
	 (if prefix (write-char ?p)))))))

(defun syntax-string-to-code (string)
  "Return the internal syntax code equivalent to STRING.
STRING should be something acceptable as the second argument to
`modify-syntax-entry'.
If STRING is invalid, signal an error."
  (let* ((bflag nil)
	 (b3 0)
	 (ch0 (aref string 0))
	 (len (length string))
	 (code (string-match (regexp-quote (char-to-string ch0))
			     (syntax-designator-chars)))
	 (i 2)
	 ch)
    (or code
	(error "Invalid syntax designator: %S" string))
    (while (< i len)
      (setq ch (aref string i))
      (incf i)
      (case ch
	(?1 (setq b3 (logior b3 128)))
	(?2 (setq b3 (logior b3  32)))
	(?3 (setq b3 (logior b3   8)))
	(?4 (setq b3 (logior b3   2)))
	(?5 (setq b3 (logior b3  64)))
	(?6 (setq b3 (logior b3  16)))
	(?7 (setq b3 (logior b3   4)))
	(?8 (setq b3 (logior b3   1)))
	(?a (case ch0
	      (?< (setq b3 (logior b3 128)))
	      (?> (setq b3 (logior b3   8)))))
	(?b (case ch0
	      (?< (setq b3 (logior b3  64) bflag t))
	      (?> (setq b3 (logior b3   4) bflag t))))
	(?p (setq code (logior code (lsh 1 7))))
	(?\  nil) ;; ignore for compatibility
	(otherwise
	 (error "Invalid syntax description flag: %S" string))))
    ;; default single char style if `b' has not been seen
    (if (not bflag)
	(case ch0
	  (?< (setq b3 (logior b3 128)))
	  (?> (setq b3 (logior b3   8)))))
    (setq code (logior code (lsh b3 16)))
    (if (and (> len 1)
	     ;; tough luck if you want to make space a paren!
	     (/= (aref string 1) ?\  ))
	(setq code (cons code (aref string 1))))
    code))

(defun modify-syntax-entry (char-range spec &optional syntax-table)
  "Set syntax for the characters CHAR-RANGE according to string SPEC.
CHAR-RANGE is a single character or a range of characters,
 as per `put-char-table'.
The syntax is changed only for SYNTAX-TABLE, which defaults to
 the current buffer's syntax table.
The first character of SPEC should be one of the following:
  Space    whitespace syntax.    w   word constituent.
  _        symbol constituent.   .   punctuation.
  \(        open-parenthesis.     \)   close-parenthesis.
  \"        string quote.         \\   character-quote.
  $        paired delimiter.     '   expression quote or prefix operator.
  <	   comment starter.	 >   comment ender.
  /        character-quote.      @   inherit from `standard-syntax-table'.

Only single-character comment start and end sequences are represented thus.
Two-character sequences are represented as described below.
The second character of SPEC is the matching parenthesis,
 used only if the first character is `(' or `)'.
Any additional characters are flags.
Defined flags are the characters 1, 2, 3, 4, 5, 6, 7, 8, p, a, and b.
 1 means C is the first of a two-char comment start sequence of style a.
 2 means C is the second character of such a sequence.
 3 means C is the first of a two-char comment end sequence of style a.
 4 means C is the second character of such a sequence.
 5 means C is the first of a two-char comment start sequence of style b.
 6 means C is the second character of such a sequence.
 7 means C is the first of a two-char comment end sequence of style b.
 8 means C is the second character of such a sequence.
 p means C is a prefix character for `backward-prefix-chars';
   such characters are treated as whitespace when they occur
   between expressions.
 a means C is comment starter or comment ender for comment style a (default)
 b means C is comment starter or comment ender for comment style b."
  (interactive
   ;; I really don't know why this is interactive
   ;; help-form should at least be made useful while reading the second arg
   "cSet syntax for character: \nsSet syntax for %c to: ")
  (simple-set-syntax-entry
   char-range
   (syntax-string-to-code spec)
   (cond ((syntax-table-p syntax-table)
	  syntax-table)
	 ((null syntax-table)
	  (syntax-table))
	 (t
	  (wrong-type-argument 'syntax-table-p syntax-table))))
  nil)

(defun map-syntax-table (__function __syntax_table &optional __range)
  "Map FUNCTION over entries in SYNTAX-TABLE, collapsing inheritance.
This is similar to `map-char-table', but works only on syntax tables, and
 collapses any entries that call for inheritance by invisibly substituting
 the inherited values from the standard syntax table."
  (check-argument-type 'syntax-table-p __syntax_table)
  (map-char-table #'(lambda (__key __value)
		      (if (eq ?@ (char-syntax-from-code __value))
			  (map-char-table #'(lambda (__key __value)
					      (funcall __function
						       __key __value))
					  (standard-syntax-table)
					  __key)
			(funcall __function __key __value)))
		  __syntax_table __range))

;(defun test-xm ()
;  (let ((o (copy-syntax-table))
;        (n (copy-syntax-table))
;        (codes (syntax-designator-chars))
;        (flags "12345678abp"))
;    (while t
;      (let ((spec (concat (char-to-string (elt codes
;						(random (length codes))))))
;                          (if (= (random 4) 0)
;                              "b"
;                              " ")
;                          (let* ((n (random 4))
;                                 (s (make-string n 0)))
;                            (while (> n 0)
;                              (setq n (1- n))
;                              (aset s n (aref flags (random (length flags)))))
;                            s))))
;        (message "%S..." spec)
;        (modify-syntax-entry ?a spec o)
;        (xmodify-syntax-entry ?a spec n)
;        (or (= (aref o ?a) (aref n ?a))
;            (error "%s"
;                   (format "fucked with %S: %x %x"
;                           spec (aref o ?a) (aref n ?a))))))))


(defun describe-syntax-table (table stream)
  (let (first-char
	last-char
	prev-val
	(describe-one
	 (if (featurep 'mule)
	     #'(lambda (first last value stream)
		 (if (equal first last)
		     (cond ((vectorp first)
			    (princ (format "%s, row %d\t"
					   (charset-name
					    (aref first 0))
					   (aref first 1))
				   stream))
			   ((symbolp first)
			    (princ first stream)
			    (princ "\t" stream))
			   (t
			    (princ (text-char-description first) stream)
			    (princ "\t" stream)))
		   (cond ((vectorp first)
			  (princ (format "%s, rows %d .. %d\t"
					 (charset-name
					  (aref first 0))
					 (aref first 1)
					 (aref last 1))
				 stream))
			 ((symbolp first)
			  (princ (format "%s .. %s\t" first last) stream))
			 (t
			  (princ (format "%s .. %s\t"
					 (text-char-description first)
					 (text-char-description last))
				 stream))))
		 (describe-syntax-code value stream))
	   #'(lambda (first last value stream)
	       (let* ((tem (text-char-description first))
		      (pos (length tem))
		      ;;(limit (cond ((numberp ctl-arrow) ctl-arrow)
		      ;;             ((memq ctl-arrow '(t nil)) 256)
		      ;;             (t 160)))
		      )
		 (princ tem stream)
		 (if (> last first)
		     (progn
		       (princ " .. " stream)
		       (setq tem (text-char-description last))
		       (princ tem stream)
		       (setq pos (+ pos (length tem) 4))))
		 (while (progn (write-char ?\  stream)
			       (setq pos (1+ pos))
			       (< pos 16))))
	       (describe-syntax-code value stream)))))
    (map-syntax-table
     #'(lambda (range value)
	 (cond
	  ((not first-char)
	   (setq first-char range
		 last-char range
		 prev-val value))
	  ((and (equal value prev-val)
		(or
		 (and (characterp range)
		      (characterp first-char)
		      (or (not (featurep 'mule))
			  (eq (char-charset range)
			      (char-charset first-char)))
		      (= (char-int last-char) (1- (char-int range))))
		 (and (vectorp range)
		      (vectorp first-char)
		      (eq (aref range 0) (aref first-char 0))
		      (= (aref last-char 1) (1- (aref range 1))))))
	   (setq last-char range))
	  (t
	   (funcall describe-one first-char last-char prev-val stream)
	   (setq first-char range
		 last-char range
		 prev-val value)))
	 nil)
     table)
    (if first-char
	(funcall describe-one first-char last-char prev-val stream))))

(defun describe-syntax-code (code stream)
  (let ((match (and (consp code) (cdr code)))
	(invalid (gettext "**invalid**")) ;(empty "") ;constants
	(standard-output (or stream standard-output))
	;; #### I18N3 should temporarily set buffer to output-translatable
	(in #'(lambda (string)
		(princ ",\n\t\t\t\t ")
		(princ string)))
	(syntax-string (syntax-code-to-string code)))
    (if (consp code)
	(setq code (car code)))
    (if (null syntax-string)
	(princ invalid)
      (princ syntax-string)
      (princ "\tmeaning: ")
      (princ (aref ["whitespace" "punctuation" "word-constituent"
		    "symbol-constituent" "open-paren" "close-paren"
		    "expression-prefix" "string-quote" "paired-delimiter"
		    "escape" "character-quote" "comment-begin" "comment-end"
		    "inherit" "extended-word-constituent"]
		   (logand code 127)))

      (if match
	  (progn
	    (princ ", matches ")
	    (princ (text-char-description match))))
      (let* ((spec (elt syntax-string 0))
	     (b3 (lsh code -16))
	     (start1  (/= 0 (logand b3 128))) ;logtest!
	     (start1b (/= 0 (logand b3  64)))
	     (start2  (/= 0 (logand b3  32)))
	     (start2b (/= 0 (logand b3  16)))
	     (end1    (/= 0 (logand b3   8)))
	     (end1b   (/= 0 (logand b3   4)))
	     (end2    (/= 0 (logand b3   2)))
	     (end2b   (/= 0 (logand b3   1)))
	     (prefix  (/= 0 (logand code 128)))
	     (single-char-p (or (= spec ?<) (= spec ?>))))
	(if start1
	    (if single-char-p
		(princ ", style A")
	      (funcall in
		       (gettext "first character of comment-start sequence A"))))
	(if start2
	    (funcall in
		     (gettext "second character of comment-start sequence A")))
	(if end1
	    (if single-char-p
		(princ ", style A")
	      (funcall in
		       (gettext "first character of comment-end sequence A"))))
	(if end2
	    (funcall in
		     (gettext "second character of comment-end sequence A")))
	(if start1b
	    (if single-char-p
		(princ ", style B")
	      (funcall in
		       (gettext "first character of comment-start sequence B"))))
	(if start2b
	    (funcall in
		     (gettext "second character of comment-start sequence B")))
	(if end1b
	    (if single-char-p
		(princ ", style B")
	      (funcall in
		       (gettext "first character of comment-end sequence B"))))
	(if end2b
	    (funcall in
		     (gettext "second character of comment-end sequence B")))
	(if prefix
	    (funcall in
		     (gettext "prefix character for `backward-prefix-chars'"))))
      (terpri stream))))

(defun symbol-near-point ()
  "Return the first textual item to the nearest point."
  (interactive)
  ;alg stolen from etag.el
  (save-excursion
	(if (or (bobp) (not (memq (char-syntax (char-before)) '(?w ?_))))
	    (while (not (looking-at #r"\sw\|\s_\|\'"))
	      (forward-char 1)))
	(while (looking-at #r"\sw\|\s_")
	  (forward-char 1))
	(if (re-search-backward #r"\sw\|\s_" nil t)
	    (regexp-quote
	     (progn (forward-char 1)
		    (buffer-substring (point)
				      (progn (forward-sexp -1)
					     (while (looking-at "\\s'")
					       (forward-char 1))
					     (point)))))
	  nil)))

;;; syntax.el ends here
