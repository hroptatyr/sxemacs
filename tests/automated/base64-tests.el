;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Hrvoje Niksic <hniksic@srce.hr>
;; Maintainer: Hrvoje Niksic <hniksic@srce.hr>
;; Created: 1999
;; Keywords: tests

;; This file is part of SXEmacs.

;; SXEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; SXEmacs is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; Test base64 functions.
;; See test-harness.el for instructions on how to run these tests.

(eval-when-compile
  (condition-case nil
      (require 'test-harness)
    (file-error
     (push "." load-path)
     (when (and (boundp 'load-file-name) (stringp load-file-name))
       (push (file-name-directory load-file-name) load-path))
     (require 'test-harness))))

;; We need to test the buffer and string functions.  We do it by
;; testing them in various circumstances, asserting the same result,
;; and returning that result.

(defvar bt-test-buffer (get-buffer-create " *base64-workhorse*"))

(defun bt-base64-encode-string (string &optional no-line-break)
  (let ((string-result (base64-encode-string string no-line-break))
	length)
    (with-current-buffer bt-test-buffer
      ;; the whole buffer
      (erase-buffer)
      (insert string)
      (setq length (base64-encode-region (point-min) (point-max) no-line-break))
      (Assert (eq length (- (point-max) (point-min))))
      (Assert-Equal (buffer-string) string-result)
      ;; partial
      (erase-buffer)
      (insert "random junk........\0\0';'eqwrkw[erpqf")
      (let ((p1 (point)) p2)
	(insert string)
	(setq p2 (point-marker))
	(insert "...more random junk.q,f3/.qrm314.r,m2typ' 2436T@W$^@$#^T@")
	(setq length (base64-encode-region p1 p2 no-line-break))
	(Assert (eq length (- p2 p1)))
	(Assert-Equal (buffer-substring p1 p2) string-result)))
    string-result))

(defun bt-base64-decode-string (string)
  (let ((string-result (base64-decode-string string))
	length)
    (with-current-buffer bt-test-buffer
      ;; the whole buffer
      (erase-buffer)
      (insert string)
      (setq length (base64-decode-region (point-min) (point-max)))
      (cond (string-result
	     (Assert (eq length (- (point-max) (point-min))))
	     (Assert-Equal (buffer-string) string-result))
	    (t
	     (Assert (null length))
	     ;; The buffer should not have been modified.
	     (Assert-Equal (buffer-string) string)))
      ;; partial
      (erase-buffer)
      (insert "random junk........\0\0';'eqwrkw[erpqf")
      (let ((p1 (point)) p2)
	(insert string)
	(setq p2 (point-marker))
	(insert "...more random junk.q,f3/.qrm314.\0\0r,m2typ' 2436T@W$^@$#T@")
	(setq length (base64-decode-region p1 p2))
	(cond (string-result
	       (Assert (eq length (- p2 p1)))
	       (Assert-Equal (buffer-substring p1 p2) string-result))
	      (t
	       (Assert (null length))
	       ;; The buffer should not have been modified.
	       (Assert-Equal (buffer-substring p1 p2) string)))))
    string-result))

(defun bt-remove-newlines (str)
  (apply #'string (delete ?\n (mapcar #'identity str))))

(defconst bt-allchars
  (let ((str (make-string 256 ?\0)))
    (dotimes (i 256)
      (aset str i (int-char i)))
    str))

(defconst bt-test-strings
  `(("" "")
    ("foo" "Zm9v")
    ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
     "QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVphYmNkZWZnaGlqa2xtbm9wcXJzdHV2d3h5ejAx
MjM0NTY3ODk=")
    (,bt-allchars
     "AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMzQ1
Njc4OTo7PD0+P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2RlZmdoaWpr
bG1ub3BxcnN0dXZ3eHl6e3x9fn+AgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeYmZqbnJ2en6Ch
oqOkpaanqKmqq6ytrq+wsbKztLW2t7i5uru8vb6/wMHCw8TFxsfIycrLzM3Oz9DR0tPU1dbX
2Nna29zd3t/g4eLj5OXm5+jp6uvs7e7v8PHy8/T19vf4+fr7/P3+/w==")
    ))

;;-----------------------------------------------------
;; Encoding base64
;;-----------------------------------------------------

(loop for (raw encoded) in bt-test-strings do
  (Assert-Equal (bt-base64-encode-string raw) encoded)
  ;; test the NO-LINE-BREAK flag
  (Assert-Equal (bt-base64-encode-string raw t) (bt-remove-newlines encoded)))

;; When Mule is around, Lisp programmers should make sure that the
;; buffer contains only characters whose `char-int' is in the [0, 256)
;; range.  If this condition is not satisfied for any character, an
;; error is signaled.
(when (featurep 'mule)
  ;; #### remove subtraction of 128 -- no longer needed with make-char
  ;; patch!
  (let* ((mule-string (format "Hrvoje Nik%ci%c"
			      ;; scaron == 185 in Latin 2
			      (make-char 'latin-iso8859-2 (- 185 128))
			      ;; cacute == 230 in Latin 2
			      (make-char 'latin-iso8859-2 (- 230 128)))))
    (Check-Error-Message error "Non-ascii character in base64 input"
      (bt-base64-encode-string mule-string))))

;;-----------------------------------------------------
;; Decoding base64
;;-----------------------------------------------------

(loop for (raw encoded) in bt-test-strings do
  (Assert-Equal (bt-base64-decode-string encoded) raw)
  (Assert-Equal (bt-base64-decode-string (bt-remove-newlines encoded)) raw))

;; Test errors
(dolist (str `("foo" "AAC" "foo\0bar" "====" "Zm=9v" ,bt-allchars))
  (Check-Error error (base64-decode-string str)))

;; base64-decode-string should ignore non-base64 characters anywhere
;; in the string.  We test this in the cheesiest manner possible, by
;; inserting non-base64 chars at the beginning, at the end, and in the
;; middle of the string.

(defconst bt-base64-chars '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J
			       ;; sometimes I hate Emacs indentation.
			       ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T
			       ?U ?V ?W ?X ?Y ?Z ?a ?b ?c ?d
			       ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n
			       ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x
			       ?y ?z ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7
			       ?8 ?9 ?+ ?/ ?=))

(defconst bt-nonbase64-chars (set-difference (mapcar #'identity bt-allchars)
					     bt-base64-chars))

(loop for (raw encoded) in bt-test-strings do
  (unless (equal raw "")
    (let* ((middlepos (/ (1+ (length encoded)) 2))
	   (left (substring encoded 0 middlepos))
	   (right (substring encoded middlepos)))
      ;; Whitespace at the beginning, end, and middle.
      (let ((mangled (concat bt-nonbase64-chars left bt-nonbase64-chars right
			     bt-nonbase64-chars)))
	(Assert-Equal (bt-base64-decode-string mangled) raw))

      ;; Whitespace between every char.
      (let ((mangled (concat bt-nonbase64-chars
			     ;; ENCODED with bt-nonbase64-chars
			     ;; between every character.
			     (mapconcat #'char-to-string encoded
					(apply #'string bt-nonbase64-chars))
			     bt-nonbase64-chars)))
	(Assert-Equal (bt-base64-decode-string mangled) raw)))))

;;-----------------------------------------------------
;; Mixed...
;;-----------------------------------------------------

;; The whole point of base64 is to ensure that an arbitrary sequence
;; of bytes passes through gateway hellfire unscathed, protected by
;; the asbestos suit of base64.  Here we test that
;; (base64-decode-string (base64-decode-string FOO)) equals FOO for
;; any FOO we can think of.  The following stunts stress-test
;; practically all aspects of the encoding and decoding process.

(loop for (raw ignored) in bt-test-strings do
  (Assert-Equal (bt-base64-decode-string
		  (bt-base64-encode-string raw))
		 raw)
  (Assert-Equal (bt-base64-decode-string
		  (bt-base64-decode-string
		   (bt-base64-encode-string
		    (bt-base64-encode-string raw))))
		 raw)
  (Assert-Equal (bt-base64-decode-string
		  (bt-base64-decode-string
		   (bt-base64-decode-string
		    (bt-base64-encode-string
		     (bt-base64-encode-string
		      (bt-base64-encode-string raw))))))
		 raw)
  (Assert-Equal (bt-base64-decode-string
		  (bt-base64-decode-string
		   (bt-base64-decode-string
		    (bt-base64-decode-string
		     (bt-base64-encode-string
		      (bt-base64-encode-string
		       (bt-base64-encode-string
			(bt-base64-encode-string raw))))))))
		 raw)
  (Assert-Equal (bt-base64-decode-string
		  (bt-base64-decode-string
		   (bt-base64-decode-string
		    (bt-base64-decode-string
		     (bt-base64-decode-string
		      (bt-base64-encode-string
		       (bt-base64-encode-string
			(bt-base64-encode-string
			 (bt-base64-encode-string
			  (bt-base64-encode-string raw))))))))))
		 raw))
