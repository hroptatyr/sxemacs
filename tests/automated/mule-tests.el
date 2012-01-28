;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Hrvoje Niksic <hniksic@xemacs.org>
;; Maintainers: Hrvoje Niksic <hniksic@xemacs.org>,
;;              Martin Buchholz <martin@xemacs.org>
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

;; Test some Mule functionality (most of these remain to be written) .
;; See test-harness.el for instructions on how to run these tests.

;; This file will be (read)ed by a non-mule XEmacs, so don't use
;; literal non-Latin1 characters.  Use (make-char) instead.

;;-----------------------------------------------------------------
;; Test whether all legal chars may be safely inserted to a buffer.
;;-----------------------------------------------------------------

(defun test-chars (&optional for-test-harness)
  "Insert all characters in a buffer, to see if XEmacs will crash.
This is done by creating a string with all the legal characters
in [0, 2^19) range, inserting it into the buffer, and checking
that the buffer's contents are equivalent to the string.

If FOR-TEST-HARNESS is specified, a temporary buffer is used, and
the Assert macro checks for correctness."
  (let ((max (expt 2 (if (featurep 'mule) 19 8)))
	(list nil)
	(i 0))
    (while (< i max)
      (and (not for-test-harness)
	   (zerop (% i 1000))
	   (message "%d" i))
      (and (int-char i)
	   ;; Don't aset to a string directly because random string
	   ;; access is O(n) under Mule.
	   (setq list (cons (int-char i) list)))
      (setq i (1+ i)))
    (let ((string (apply #'string (nreverse list))))
      (if for-test-harness
	  ;; For use with test-harness, use Assert and a temporary
	  ;; buffer.
	  (with-temp-buffer
	    (insert string)
	    (Assert-Equal (buffer-string) string))
	;; For use without test harness: use a normal buffer, so that
	;; you can also test whether redisplay works.
	(switch-to-buffer (get-buffer-create "test"))
	(erase-buffer)
	(buffer-disable-undo)
	(insert string)
	(assert (equal (buffer-string) string))))))

;; It would be really *really* nice if test-harness allowed a way to
;; run a test in byte-compiled mode only.  It's tedious to have
;; time-consuming tests like this one run twice, once interpreted and
;; once compiled, for no good reason.
(test-chars t)

;;-----------------------------------------------------------------
;; Test string modification functions that modify the length of a char.
;;-----------------------------------------------------------------

(when (featurep 'mule)
  ;;---------------------------------------------------------------
  ;; Test fillarray
  ;;---------------------------------------------------------------
  (macrolet
      ((fillarray-test
	(charset1 charset2)
	(let ((char1 (make-char charset1 69))
	      (char2 (make-char charset2 69)))
	  `(let ((string (make-string 1000 ,char1)))
	     (fillarray string ,char2)
	     (Assert (eq (aref string 0) ,char2))
	     (Assert (eq (aref string (1- (length string))) ,char2))
	     (Assert (eq (length string) 1000))))))
    (fillarray-test ascii latin-iso8859-1)
    (fillarray-test ascii latin-iso8859-2)
    (fillarray-test latin-iso8859-1 ascii)
    (fillarray-test latin-iso8859-2 ascii))

  ;; Test aset
  (let ((string (string (make-char 'ascii 69) (make-char 'latin-iso8859-2 69))))
    (aset string 0 (make-char 'latin-iso8859-2 42))
    (Assert (eq (aref string 1) (make-char 'latin-iso8859-2 69))))

  ;;---------------------------------------------------------------
  ;; Test coding system functions
  ;;---------------------------------------------------------------

  ;; Create alias for coding system without subsidiaries
  (Assert (coding-system-p (find-coding-system 'binary)))
  (Assert (coding-system-canonical-name-p 'binary))
  (Assert (not (coding-system-alias-p 'binary)))
  (Assert (not (coding-system-alias-p 'mule-tests-alias)))
  (Assert (not (coding-system-canonical-name-p 'mule-tests-alias)))
  (Check-Error-Message
   error "Symbol is the canonical name of a coding system and cannot be redefined"
   (define-coding-system-alias 'binary 'iso8859-2))
  (Check-Error-Message
   error "Symbol is not a coding system alias"
   (coding-system-aliasee 'binary))

  (define-coding-system-alias 'mule-tests-alias 'binary)
  (Assert (coding-system-alias-p 'mule-tests-alias))
  (Assert (not (coding-system-canonical-name-p 'mule-tests-alias)))
  (Assert (eq (get-coding-system 'binary) (get-coding-system 'mule-tests-alias)))
  (Assert (eq 'binary (coding-system-aliasee 'mule-tests-alias)))
  (Assert (not (coding-system-alias-p 'mule-tests-alias-unix)))
  (Assert (not (coding-system-alias-p 'mule-tests-alias-dos)))
  (Assert (not (coding-system-alias-p 'mule-tests-alias-mac)))

  (define-coding-system-alias 'mule-tests-alias (get-coding-system 'binary))
  (Assert (coding-system-alias-p 'mule-tests-alias))
  (Assert (not (coding-system-canonical-name-p 'mule-tests-alias)))
  (Assert (eq (get-coding-system 'binary) (get-coding-system 'mule-tests-alias)))
  (Assert (eq 'binary (coding-system-aliasee 'mule-tests-alias)))
  (Assert (not (coding-system-alias-p 'mule-tests-alias-unix)))
  (Assert (not (coding-system-alias-p 'mule-tests-alias-dos)))
  (Assert (not (coding-system-alias-p 'mule-tests-alias-mac)))

  (define-coding-system-alias 'nested-mule-tests-alias 'mule-tests-alias)
  (Assert (coding-system-alias-p 'nested-mule-tests-alias))
  (Assert (not (coding-system-canonical-name-p 'nested-mule-tests-alias)))
  (Assert (eq (get-coding-system 'binary) (get-coding-system 'nested-mule-tests-alias)))
  (Assert (eq (coding-system-aliasee 'nested-mule-tests-alias) 'mule-tests-alias))
  (Assert (eq 'mule-tests-alias (coding-system-aliasee 'nested-mule-tests-alias)))
  (Assert (not (coding-system-alias-p 'nested-mule-tests-alias-unix)))
  (Assert (not (coding-system-alias-p 'nested-mule-tests-alias-dos)))
  (Assert (not (coding-system-alias-p 'nested-mule-tests-alias-mac)))

  (Check-Error-Message
   error "Attempt to create a coding system alias loop"
   (define-coding-system-alias 'mule-tests-alias 'nested-mule-tests-alias))
  (Check-Error-Message
   error "No such coding system"
   (define-coding-system-alias 'no-such-coding-system 'no-such-coding-system))
  (Check-Error-Message
   error "Attempt to create a coding system alias loop"
   (define-coding-system-alias 'mule-tests-alias 'mule-tests-alias))

  (define-coding-system-alias 'nested-mule-tests-alias nil)
  (define-coding-system-alias 'mule-tests-alias nil)
  (Assert (coding-system-p (find-coding-system 'binary)))
  (Assert (coding-system-canonical-name-p 'binary))
  (Assert (not (coding-system-alias-p 'binary)))
  (Assert (not (coding-system-alias-p 'mule-tests-alias)))
  (Assert (not (coding-system-canonical-name-p 'mule-tests-alias)))
  (Check-Error-Message
   error "Symbol is the canonical name of a coding system and cannot be redefined"
   (define-coding-system-alias 'binary 'iso8859-2))
  (Check-Error-Message
   error "Symbol is not a coding system alias"
   (coding-system-aliasee 'binary))

  (define-coding-system-alias 'nested-mule-tests-alias nil)
  (define-coding-system-alias 'mule-tests-alias nil)

  ;; Create alias for coding system with subsidiaries
  (define-coding-system-alias 'mule-tests-alias 'iso-8859-7)
  (Assert (coding-system-alias-p 'mule-tests-alias))
  (Assert (not (coding-system-canonical-name-p 'mule-tests-alias)))
  (Assert (eq (get-coding-system 'iso-8859-7) (get-coding-system 'mule-tests-alias)))
  (Assert (eq 'iso-8859-7 (coding-system-aliasee 'mule-tests-alias)))
  (Assert (coding-system-alias-p 'mule-tests-alias-unix))
  (Assert (coding-system-alias-p 'mule-tests-alias-dos))
  (Assert (coding-system-alias-p 'mule-tests-alias-mac))

  (define-coding-system-alias 'mule-tests-alias (get-coding-system 'iso-8859-7))
  (Assert (coding-system-alias-p 'mule-tests-alias))
  (Assert (not (coding-system-canonical-name-p 'mule-tests-alias)))
  (Assert (eq (get-coding-system 'iso-8859-7) (get-coding-system 'mule-tests-alias)))
  (Assert (eq 'iso-8859-7 (coding-system-aliasee 'mule-tests-alias)))
  (Assert (coding-system-alias-p 'mule-tests-alias-unix))
  (Assert (coding-system-alias-p 'mule-tests-alias-dos))
  (Assert (coding-system-alias-p 'mule-tests-alias-mac))
  (Assert (eq (find-coding-system 'mule-tests-alias-mac)
	      (find-coding-system 'iso-8859-7-mac)))

  (define-coding-system-alias 'nested-mule-tests-alias 'mule-tests-alias)
  (Assert (coding-system-alias-p 'nested-mule-tests-alias))
  (Assert (not (coding-system-canonical-name-p 'nested-mule-tests-alias)))
  (Assert (eq (get-coding-system 'iso-8859-7)
	      (get-coding-system 'nested-mule-tests-alias)))
  (Assert (eq (coding-system-aliasee 'nested-mule-tests-alias) 'mule-tests-alias))
  (Assert (eq 'mule-tests-alias (coding-system-aliasee 'nested-mule-tests-alias)))
  (Assert (coding-system-alias-p 'nested-mule-tests-alias-unix))
  (Assert (coding-system-alias-p 'nested-mule-tests-alias-dos))
  (Assert (coding-system-alias-p 'nested-mule-tests-alias-mac))
  (Assert (eq (find-coding-system 'nested-mule-tests-alias-unix)
	      (find-coding-system 'iso-8859-7-unix)))

  (Check-Error-Message
   error "Attempt to create a coding system alias loop"
   (define-coding-system-alias 'mule-tests-alias 'nested-mule-tests-alias))
  (Check-Error-Message
   error "No such coding system"
   (define-coding-system-alias 'no-such-coding-system 'no-such-coding-system))
  (Check-Error-Message
   error "Attempt to create a coding system alias loop"
   (define-coding-system-alias 'mule-tests-alias 'mule-tests-alias))

  ;; Test dangling alias deletion
  (define-coding-system-alias 'mule-tests-alias nil)
  (Assert (not (coding-system-alias-p 'mule-tests-alias)))
  (Assert (not (coding-system-alias-p 'mule-tests-alias-unix)))
  (Assert (not (coding-system-alias-p 'nested-mule-tests-alias)))
  (Assert (not (coding-system-alias-p 'nested-mule-tests-alias-dos)))

  ;;---------------------------------------------------------------
  ;; Test strings waxing and waning across the 8k BIG_STRING limit (see alloc.c)
  ;;---------------------------------------------------------------
  (defun charset-char-string (charset)
    (let (lo hi string n (gc-cons-threshold most-positive-fixnum))
      (if (= (charset-chars charset) 94)
	  (setq lo 33 hi 126)
	(setq lo 32 hi 127))
      (if (= (charset-dimension charset) 1)
	  (progn
	    (setq string (make-string (1+ (- hi lo)) ??))
	    (setq n 0)
	    (loop for j from lo to hi do
	      (progn
		(aset string n (make-char charset j))
		(incf n)))
	    (garbage-collect)
	    string)
	(progn
	  (setq string (make-string (* (1+ (- hi lo)) (1+ (- hi lo))) ??))
	  (setq n 0)
	  (loop for j from lo to hi do
	    (loop for k from lo to hi do
	      (progn
		(aset string n (make-char charset j k))
		(incf n))))
	  (garbage-collect)
	  string))))

  ;; The following two used to crash xemacs!
  (Assert (charset-char-string 'japanese-jisx0208))
  (aset (make-string 9003 ??) 1 (make-char 'latin-iso8859-1 77))

  (let ((greek-string (charset-char-string 'greek-iso8859-7))
	(string (make-string (* 96 60) ??)))
    (loop for j from 0 below (length string) do
      (aset string j (aref greek-string (mod j 96))))
    (loop for k in '(0 1 58 59) do
      (Assert-Equal (substring string (* 96 k) (* 96 (1+ k))) greek-string)))

  (let ((greek-string (charset-char-string 'greek-iso8859-7))
	(string (make-string (* 96 60) ??)))
   (loop for j from (1- (length string)) downto 0 do
     (aset string j (aref greek-string (mod j 96))))
   (loop for k in '(0 1 58 59) do
     (Assert-Equal (substring string (* 96 k) (* 96 (1+ k))) greek-string)))

  (let ((ascii-string (charset-char-string 'ascii))
	(string (make-string (* 94 60) (make-char 'greek-iso8859-7 57))))
   (loop for j from 0 below (length string) do
      (aset string j (aref ascii-string (mod j 94))))
    (loop for k in '(0 1 58 59) do
      (Assert-Equal (substring string (* 94 k) (+ 94 (* 94 k))) ascii-string)))

  (let ((ascii-string (charset-char-string 'ascii))
	(string (make-string (* 94 60) (make-char 'greek-iso8859-7 57))))
    (loop for j from (1- (length string)) downto 0 do
      (aset string j (aref ascii-string (mod j 94))))
    (loop for k in '(0 1 58 59) do
      (Assert-Equal (substring string (* 94 k) (* 94 (1+ k))) ascii-string)))

  ;;---------------------------------------------------------------
  ;; Test file-system character conversion (and, en passant, file ops)
  ;;---------------------------------------------------------------
  (let* ((scaron (make-char 'latin-iso8859-2 57))
	 (latin2-string (make-string 4 scaron))
	 (prefix (concat (file-name-as-directory
			  (file-truename (temp-directory)))
			 latin2-string))
	 (name1 (make-temp-name prefix))
	 (name2 (make-temp-name prefix))
	 (file-name-coding-system 'iso-8859-2))
    (Silence-Message
      (Assert-Not-Equal name1 name2)
      ;; Kludge to handle Mac OS X which groks only UTF-8.
      (cond ((eq system-type 'darwin)
	     (Check-Error-Message 'file-error "Opening output file"
				  (write-region (point-min) (point-max) name1))
	     (require 'un-define)
	     (setq file-name-coding-system 'utf-8)))
      (Assert (not (file-exists-p name1)))
      (write-region (point-min) (point-max) name1)
      (Assert (file-exists-p name1))
      (when (fboundp 'make-symbolic-link)
	(make-symbolic-link name1 name2)
	(Assert (file-exists-p name2))
	(Assert-Equal (file-truename name2) (file-truename name1))
	(Assert-Equal (file-truename name2) name1)
	(Assert-Equal (file-truename name1) name1))

      (ignore-file-errors (delete-file name1) (delete-file name2))))

  ;; Add many more file operation tests here...

  ;;---------------------------------------------------------------
  ;; Test Unicode-related functions
  ;;---------------------------------------------------------------
  (let* ((scaron (make-char 'latin-iso8859-2 57)))
    (loop for code in '(#x0000 #x2222 #x4444 #xffff) do
      (progn
	(set-ucs-char code scaron)
	(Assert (eq scaron (ucs-char code)))))

    (Assert (eq nil (set-ucs-char #x1ffff scaron)))
    (Check-Error wrong-type-argument (set-ucs-char -10000 scaron)))

  )
