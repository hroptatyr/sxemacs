;;; -*- coding: iso-8859-1 -*-

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Yoshiki Hayashi  <yoshiki@xemacs.org>
;; Maintainer: Yoshiki Hayashi  <yoshiki@xemacs.org>
;; Created: 2000
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

;; Test regular expression.

(Check-Error-Message error "Trailing backslash"
		     (string-match "\\" "a"))
(Check-Error-Message error "Invalid preceding regular expression"
		     (string-match "a++" "a"))
(Check-Error-Message error "Invalid preceding regular expression"
		     (string-match "a**" "a"))
(Check-Error-Message error "Invalid preceding regular expression"
		     (string-match "a???" "a"))
(Check-Error-Message error "Unmatched \\[ or \\[^"
		     (string-match "[" "a"))
(Check-Error-Message error "Unmatched \\[ or \\[^"
		     (string-match "[abc" "a"))
(Check-Error-Message error "Unmatched ) or \\\\)"
		     (string-match "\\)" "a"))
(Check-Error-Message error "Invalid regular expression"
		     (string-match "\\(?.\\)" "a"))
(Check-Error-Message error "Unmatched \\\\{"
		     (string-match "a\\{" "a"))
(Check-Error-Message error "Invalid content of \\\\{\\\\}"
		     (string-match "a\\{a\\}" "a"))

;; exactn

;; string-match
(with-temp-buffer
  ;; case-insensitive
  (Assert (string-match "ä" "ä"))
  (Assert (string-match "ä" "Ä"))
  (Assert (string-match "Ä" "Ä"))
  (Assert (string-match "Ä" "ä"))
  ;; case-sensitive
  (setq case-fold-search nil)
  (Assert (string-match "ä" "ä"))
  (Assert (not (string-match "ä" "Ä")))
  (Assert (string-match "Ä" "Ä"))
  (Assert (not (string-match "Ä" "ä"))))

;; looking-at
(with-temp-buffer
  (insert "äÄ")
  ;; case-insensitive
  (goto-char (point-min))
  (Assert (looking-at "ä"))
  (Assert (looking-at "Ä"))
  (forward-char)
  (Assert (looking-at "ä"))
  (Assert (looking-at "Ä"))
  ;; case-sensitive
  (setq case-fold-search nil)
  (goto-char (point-min))
  (Assert (looking-at "ä"))
  (Assert (not (looking-at "Ä")))
  (forward-char)
  (Assert (not (looking-at "ä")))
  (Assert (looking-at "Ä")))

;; re-search-forward and re-search-backward
(with-temp-buffer
  (insert "äÄ")
  ;; case insensitive
  ;; forward
  (goto-char (point-min))
  ;; Avoid trivial regexp.
  (Assert (eq 2 (re-search-forward "ä\\|a" nil t)))
  (goto-char (point-min))
  (Assert (eq 2 (re-search-forward "Ä\\|a" nil t)))
  (goto-char (1+ (point-min)))
  (Assert (eq 3 (re-search-forward "ä\\|a" nil t)))
  (goto-char (1+ (point-min)))
  (Assert (eq 3 (re-search-forward "Ä\\|a" nil t)))
  ;; backward
  (goto-char (point-max))
  (Assert (eq 2 (re-search-backward "ä\\|a" nil t)))
  (goto-char (point-max))
  (Assert (eq 2 (re-search-backward "Ä\\|a" nil t)))
  (goto-char (1- (point-max)))
  (Assert (eq 1 (re-search-backward "ä\\|a" nil t)))
  (goto-char (1- (point-max)))
  (Assert (eq 1 (re-search-backward "Ä\\|a" nil t)))
  ;; case sensitive
  (setq case-fold-search nil)
  ;; forward
  (goto-char (point-min))
  (Assert (eq 2 (re-search-forward "ä\\|a" nil t)))
  (goto-char (point-min))
  (Assert (eq 3 (re-search-forward "Ä\\|a" nil t)))
  (goto-char (1+ (point-min)))
  (Assert (not (re-search-forward "ä\\|a" nil t)))
  (goto-char (1+ (point-min)))
  (Assert (eq 3 (re-search-forward "Ä\\|a" nil t)))
  ;; backward
  (goto-char (point-max))
  (Assert (eq 1 (re-search-backward "ä\\|a" nil t)))
  (goto-char (point-max))
  (Assert (eq 2 (re-search-backward "Ä\\|a" nil t)))
  (goto-char (1- (point-max)))
  (Assert (eq 1 (re-search-backward "ä\\|a" nil t)))
  (goto-char (1- (point-max)))
  (Assert (not (re-search-backward "Ä\\|a" nil t))))

;; duplicate
(with-temp-buffer
  ;; case insensitive
  (Assert (string-match "^\\(ä\\)\\1$" "ää"))
  (Assert (string-match "^\\(ä\\)\\1$" "äÄ"))
  (Assert (string-match "^\\(ä\\)\\1$" "ÄÄ"))
  (Assert (string-match "^\\(ä\\)\\1$" "Ää"))
  (Assert (string-match "^\\(Ä\\)\\1$" "ää"))
  (Assert (string-match "^\\(Ä\\)\\1$" "äÄ"))
  (Assert (string-match "^\\(Ä\\)\\1$" "ÄÄ"))
  (Assert (string-match "^\\(Ä\\)\\1$" "Ää"))
  ;; case sensitive
  (setq case-fold-search nil)
  (Assert (string-match "^\\(ä\\)\\1$" "ää"))
  (Assert (not (string-match "^\\(ä\\)\\1$" "äÄ")))
  (Assert (not (string-match "^\\(ä\\)\\1$" "ÄÄ")))
  (Assert (not (string-match "^\\(ä\\)\\1$" "Ää")))
  (Assert (not (string-match "^\\(Ä\\)\\1$" "ää")))
  (Assert (not (string-match "^\\(Ä\\)\\1$" "äÄ")))
  (Assert (string-match "^\\(Ä\\)\\1$" "ÄÄ"))
  (Assert (not (string-match "^\\(Ä\\)\\1$" "Ää"))))

;; multiple-match
;; Thanks to Manfred Bartz <MBartz@xix.com>
;; c.e.x <vn4rkkm7ouf3b5@corp.supernews.com>
;; #### Need to do repetitions of more complex regexps
;; #### WASH ME!
(with-temp-buffer
  (Assert (not (string-match "^a\\{4,4\\}$" "aaa")))
  (Assert      (string-match "^a\\{4,4\\}$" "aaaa"))
  (Assert (not (string-match "^a\\{4,4\\}$" "aaaaa")))
  (Assert (not (string-match "^[a]\\{4,4\\}$" "aaa")))
  (Assert      (string-match "^[a]\\{4,4\\}$" "aaaa"))
  (Assert (not (string-match "^[a]\\{4,4\\}$" "aaaaa")))
  (Assert (not (string-match "^\\(a\\)\\{4,4\\}$" "aaa")))
  (Assert      (string-match "^\\(a\\)\\{4,4\\}$" "aaaa"))
  (Assert (not (string-match "^\\(a\\)\\{4,4\\}$" "aaaaa")))
  ;; Use class because repetition of single char broken in 21.5.15
  (Assert (not (string-match "^[a]\\{3,5\\}$" "aa")))
  (Assert      (string-match "^[a]\\{3,5\\}$" "aaa"))
  (Assert      (string-match "^[a]\\{3,5\\}$" "aaaa"))
  (Assert      (string-match "^[a]\\{3,5\\}$" "aaaaa"))
  (Assert (not (string-match "^[a]\\{3,5\\}$" "aaaaaa")))
  (insert "\
aa
aaa
aaaa
aaaaa
aaaaaa
baaaa
")
  (goto-char (point-min))
  (forward-line 1)
  (Assert (not (looking-at "^a\\{4,4\\}$")))
  (forward-line 1)
  (Assert      (looking-at "^a\\{4,4\\}$"))
  (forward-line 1)
  (Assert (not (looking-at "^a\\{4,4\\}$")))
  (goto-char (point-min))
  (forward-line 1)
  (Assert (not (looking-at "^[a]\\{4,4\\}$")))
  (forward-line 1)
  (Assert      (looking-at "^[a]\\{4,4\\}$"))
  (forward-line 1)
  (Assert (not (looking-at "^[a]\\{4,4\\}$")))
  (goto-char (point-min))
  (forward-line 1)
  (Assert (not (looking-at "^\\(a\\)\\{4,4\\}$")))
  (forward-line 1)
  (Assert      (looking-at "^\\(a\\)\\{4,4\\}$"))
  (forward-line 1)
  (Assert (not (looking-at "^\\(a\\)\\{4,4\\}$")))
  ;; Use class because repetition of single char broken in 21.5.15
  (goto-char (point-min))
  (Assert (not (looking-at "^[a]\\{3,5\\}$")))
  (forward-line 1)
  (Assert      (looking-at "^[a]\\{3,5\\}$"))
  (forward-line 1)
  (Assert      (looking-at "^[a]\\{3,5\\}$"))
  (forward-line 1)
  (Assert      (looking-at "^[a]\\{3,5\\}$"))
  (forward-line 1)
  (Assert (not (looking-at "^[a]\\{3,5\\}$")))
  (goto-char (point-min))
  (Assert (= 12 (re-search-forward "a\\{4,4\\}")))
  (goto-char (point-min))
  (Assert (= 12 (re-search-forward "b?a\\{4,4\\}")))
  (goto-char (point-min))
  (Assert (= 31 (re-search-forward "ba\\{4,4\\}")))
  (goto-char (point-min))
  (Assert (= 31 (re-search-forward "[b]a\\{4,4\\}")))
  (goto-char (point-min))
  (Assert (= 31 (re-search-forward "\\(b\\)a\\{4,4\\}")))
  (goto-char (point-min))
  (Assert (= 12 (re-search-forward "^a\\{4,4\\}")))
  (goto-char (point-min))
  (Assert (= 12 (re-search-forward "^a\\{4,4\\}$")))
  (goto-char (point-min))
  (Assert (= 12 (re-search-forward "[a]\\{4,4\\}")))
  (goto-char (point-min))
  (Assert (= 12 (re-search-forward "^[a]\\{4,4\\}")))
  (goto-char (point-min))
  (Assert (= 12 (re-search-forward "^[a]\\{4,4\\}$")))
  )

;; charset, charset_not
;; Not called because it takes too much time.
(defun test-regexp-charset-paranoid ()
  (let ((i 0)
	(max (expt 2 (if (featurep 'mule) 19 8)))
	(range "[a-z]")
	(range-not "[^a-z]")
	char string)
    (while (< i max)
      (when (setq char (int-to-char i))
	(setq string (char-to-string char))
	(if (or (and (<= 65 i)
		     (<= i 90))
		(and (<= 97 i)
		     (<= i 122)))
	    (progn
	      (Assert (string-match range string))
	      (Assert (not (string-match range-not string))))
	  (Assert (not (string-match range string)))
	  (Assert (string-match range-not string))))
      (setq i (1+ i)))))

;; (test-regexp-charset-paranoid)

;; charset_mule, charset_mule_not
;; Not called because it takes too much time.
(defun test-regex-charset-mule-paranoid ()
  (if (featurep 'mule)
      (let ((i 0)
	    (max (expt 2 19))
	    (range (format "[%c-%c]"
			   (make-char 'japanese-jisx0208 36 34)
			   (make-char 'japanese-jisx0208 36 42)))
	    (range-not (format "[^%c-%c]"
			       (make-char 'japanese-jisx0208 36 34)
			       (make-char 'japanese-jisx0208 36 42)))
	    (min-int (char-to-int (make-char 'japanese-jisx0208 36 34)))
	    (max-int (char-to-int (make-char 'japanese-jisx0208 36 42)))
	    char string)
	(while (< i max)
	  (when (setq char (int-to-char i))
	    (setq string (char-to-string char))
	    (if (and (<= min-int i)
		     (<= i max-int))
		(progn
		  (Assert (string-match range string))
		  (Assert (not (string-match range-not string))))
	      (Assert (not (string-match range string)))
	      (Assert (string-match range-not string))))
	  (setq i (1+ i))))))

;; (test-regex-charset-mule-paranoid)

;; Test replace-match
(with-temp-buffer
  (insert "This is a test buffer.")
  (goto-char (point-min))
  (search-forward "this is a test ")
  (looking-at "Unmatchable text")
  (replace-match "")
  (Assert (looking-at "^buffer.$")))

;; Test that trivial regexps reset unused registers
;; Thanks to Martin Sternholm for the report.
;; xemacs-beta <5blm6h2ki5.fsf@lister.roxen.com>
(with-temp-buffer
  (insert "ab")
  (goto-char (point-min))
  (re-search-forward "\\(a\\)")
  ;; test the whole-match data, too -- one try scotched that, too!
  (Assert (string= (match-string 0) "a"))
  (Assert (string= (match-string 1) "a"))
  (re-search-forward "b")
  (Assert (string= (match-string 0) "b"))
  (Assert (not (match-string 1))))

;; Test word boundaries
(Assert (= (string-match " \\<a" " a") 0))
(Assert (= (string-match "a\\> " "a ") 0))
(Assert (= (string-match " \\ba" " a") 0))
(Assert (= (string-match "a\\b " "a ") 0))
(Assert (= (string-match "\\ba" " a") 1))
(Assert (= (string-match "a\\b" "a ") 0))
;; should work at target boundaries
(Assert (= (string-match "\\<a" "a") 0))
(Assert (= (string-match "a\\>" "a") 0))
(Assert (= (string-match "\\ba" "a") 0))
(Assert (= (string-match "a\\b" "a") 0))
;; but not if the "word" would be on the null side of the boundary!
(Assert (not (string-match "\\<" "")))
(Assert (not (string-match "\\>" "")))
(Assert (not (string-match " \\<" " ")))
(Assert (not (string-match "\\> " " ")))
(Assert (not (string-match "a\\<" "a")))
(Assert (not (string-match "\\>a" "a")))
;; Added Known-Bug 2002-09-09 sjt
;; These are now fixed 2003-03-21 sjt
(Assert (not (string-match "\\b" "")))
(Assert (not (string-match " \\b" " ")))
(Assert (not (string-match "\\b " " ")))

;; Added 2002-12-27
(if (featurep 'mule)
    ;; note: (int-to-char 65) => ?A
    (let ((ch0 (make-char 'japanese-jisx0208 52 65))
	  (ch1 (make-char 'japanese-jisx0208 51 65)))
      (Assert (not (string-match "A" (string ch0))))
      (Assert (not (string-match "[A]" (string ch0))))
      (Assert (eq (string-match "[^A]" (string ch0)) 0))
      (Assert (not (string-match "@A" (string ?@ ch0))))
      (Assert (not (string-match "@[A]" (string ?@ ch0))))
      (Assert (eq (string-match "@[^A]" (string ?@ ch0)) 0))
      (Assert (not (string-match "@?A" (string ?@ ch0))))
      (Assert (not (string-match "A" (string ch1))))
      (Assert (not (string-match "[A]" (string ch1))))
      (Assert (eq (string-match "[^A]" (string ch1)) 0))
      (Assert (not (string-match "@A" (string ?@ ch1))))
      (Assert (not (string-match "@[A]" (string ?@ ch1))))
      (Assert (eq (string-match "@[^A]" (string ?@ ch1)) 0))
      (Assert (not (string-match "@?A" (string ?@ ch1))))))

;; More stale match data tests.
;; Thanks to <bjacob@ca.metsci.com> for drawing attention to this issue.
;; Flying in the face of sanity, the Asserts with positive results below are
;; correct.  Too much code depends on failed matches preserving match-data.
(let ((a "a"))
  (Assert (string= (progn (string-match "a" a)
			  (string-match "b" a)
			  (match-string 0 a))
		   a))
  (Assert (not (progn (string-match "a" a)
		      (string-match "b" a)
		      (match-string 1 a))))
  ;; test both for the second match is a plain string match and a regexp match
  (Assert (string= (progn (string-match "\\(a\\)" a)
			  (string-match "\\(b\\)" a)
			  (match-string 0 a))
		   a))
  (Assert (string= (progn (string-match "\\(a\\)" a)
			  (string-match "b" a)
			  (match-string 0 a))
		   a))
  (Assert (string= (progn (string-match "\\(a\\)" a)
			  (string-match "\\(b\\)" a)
			  (match-string 1 a))
		   a))
  (Assert (string= (progn (string-match "\\(a\\)" a)
			  (string-match "b" a)
			  (match-string 1 a))
		   a)))

;; bug identified by Katsumi Yamaoka 2004-09-03 <b9ywtzbbpue.fsf_-_@jpl.org>
;; fix submitted by sjt 2004-09-08
;; trailing comments are values from buggy 21.4.15
(let ((text "abc"))
  (Assert (eq 0 (string-match "\\(?:ab+\\)*c" text)))	; 2
  (Assert (eq 0 (string-match "^\\(?:ab+\\)*c" text)))	; nil
  (Assert (eq 0 (string-match "^\\(?:ab+\\)*" text)))	; 0
  (Assert (eq 0 (string-match "^\\(?:ab+\\)c" text)))	; 0
  (Assert (eq 0 (string-match "^\\(?:ab\\)*c" text)))	; 0
  (Assert (eq 0 (string-match "^\\(?:a+\\)*b" text)))	; nil
  (Assert (eq 0 (string-match "^\\(?:a\\)*b" text)))	; 0
)

;; per Steve Youngs 2004-09-30 <microsoft-free.87ekkjhj7t.fsf@youngs.au.com>
;; fix submitted by sjt 2004-10-07
;; trailing comments are values from buggy 21.4.pre16
(let ((text "abc"))
  (Assert (eq 0 (string-match "\\(?:a\\(b\\)\\)" text)))	; 0
  (Assert (string= (match-string 1 text) "b"))			; ab
  (Assert (null (match-string 2 text)))				; b
  (Assert (null (match-string 3 text)))				; nil
  (Assert (eq 0 (string-match "\\(?:a\\(?:b\\(c\\)\\)\\)" text)))	; 0
  (Assert (string= (match-string 1 text) "c"))			; abc
  (Assert (null (match-string 2 text)))				; ab
  (Assert (null (match-string 3 text)))				; c
  (Assert (null (match-string 4 text)))				; nil
)

;; trivial subpatterns and backreferences with shy groups
(let ((text1 "abb")
      (text2 "aba")
      (re0 "\\(a\\)\\(b\\)\\2")
      (re1 "\\(?:a\\)\\(b\\)\\2")
      (re2 "\\(?:a\\)\\(b\\)\\1")
      (re3 "\\(a\\)\\(?:b\\)\\1"))

  (Assert (eq 0 (string-match re0 text1)))
  (Assert (string= text1 (match-string 0 text1)))
  (Assert (string= "a" (match-string 1 text1)))
  (Assert (string= "b" (match-string 2 text1)))
  (Assert (null (string-match re0 text2)))

  (Check-Error-Message 'invalid-regexp "Invalid back reference"
		       (string-match re1 text1))

  (Assert (eq 0 (string-match re2 text1)))
  (Assert (string= text1 (match-string 0 text1)))
  (Assert (string= "b" (match-string 1 text1)))
  (Assert (null (match-string 2 text1)))
  (Assert (null (string-match re2 text2)))

  (Assert (null (string-match re3 text1)))
  (Assert (eq 0 (string-match re3 text2)))
  (Assert (string= text2 (match-string 0 text2)))
  (Assert (string= "a" (match-string 1 text2)))
  (Assert (null (match-string 2 text2)))
  )
