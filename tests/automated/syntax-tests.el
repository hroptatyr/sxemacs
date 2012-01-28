;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Yoshiki Hayashi  <t90553@mail.ecc.u-tokyo.ac.jp>
;; Maintainer: Yoshiki Hayashi  <t90553@mail.ecc.u-tokyo.ac.jp>
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

;; Test syntax related functions.
;; Right now it tests scan_words using forward-word and backward-word.
;; See test-harness.el for instructions on how to run these tests.

;;; Notation
;; W:   word constituent character.
;; NW:  non word constituent character.
;; -!-: current point.
;; EOB: end of buffer
;; BOB: beginning of buffer.

;; Algorithm of scan_words is simple.  It just searches SW and then
;; moves to NW.  When with MULE, it also stops at word boundary.  Word
;; boundary is tricky and listing all possible cases will be huge.
;; Those test are omitted here as it doesn't affect core
;; functionality.

(defun test-forward-word (string stop)
  (goto-char (point-max))
  (let ((point (point)))
    (insert string)
    (goto-char point)
    (forward-word 1)
    (Assert (eq (point) (+ point stop)))))

(with-temp-buffer
  ;; -!- W NW
  (test-forward-word "W " 1)
  (test-forward-word "WO " 2)
  ;; -!- W EOB
  (test-forward-word "W" 1)
  (test-forward-word "WO" 2)
  ;; -!- NW EOB
  (test-forward-word " " 1)
  (test-forward-word " !" 2)
  ;; -!- NW W NW
  (test-forward-word " W " 2)
  (test-forward-word " WO " 3)
  (test-forward-word " !W " 3)
  (test-forward-word " !WO " 4)
  ;; -!- NW W EOB
  (test-forward-word " W" 2)
  (test-forward-word " WO" 3)
  (test-forward-word " !W" 3)
  (test-forward-word " !WO" 4))

(defun test-backward-word (string stop)
  (goto-char (point-min))
  (insert string)
  (let ((point (point)))
    (backward-word 1)
    (Assert (eq (point) (- point stop)))))

(with-temp-buffer
  ;; NW W -!-
  (test-backward-word " W" 1)
  (test-backward-word " WO" 2)
  ;; BOB W -!-
  (test-backward-word "W" 1)
  (test-backward-word "WO" 2)
  ;; BOB NW -!-
  ;; -!-NW EOB
  (test-backward-word " " 1)
  (test-backward-word " !" 2)
  ;; NW W NW -!-
  (test-backward-word " W " 2)
  (test-backward-word " WO " 3)
  (test-backward-word " W !" 3)
  (test-backward-word " WO !" 4)
  ;; BOB W NW -!-
  (test-backward-word "W " 2)
  (test-backward-word "WO " 3)
  (test-backward-word "W !" 3)
  (test-backward-word "WO !" 4))

;; Works like test-forward-word, except for the following:
;; after <string> is inserted, the syntax-table <apply-syntax>
;; is applied to position <apply-pos>.
;; <apply-pos> can be in the form (start . end), or can be a
;; character position.
(defun test-syntax-table (string apply-pos apply-syntax stop)
  ;; We don't necessarily have syntax-table properties ...
  (when (boundp 'lookup-syntax-properties) ; backwards compatible kludge
    ;; ... and they may not be enabled by default if we do.
    (setq lookup-syntax-properties t)
    (goto-char (point-max))
    (unless (consp apply-pos)
      (setq apply-pos `(,apply-pos . ,(+ 1 apply-pos))))
    (let ((point (point)))
      (insert string)
      (put-text-property (+ point (car apply-pos)) (+ point (cdr apply-pos))
			 'syntax-table apply-syntax)
      (goto-char point)
      (forward-word 1)
      (Assert (eq (point) (+ point stop))))))

;; test syntax-table extents
(with-temp-buffer
  ;; Apply punctuation to word
  (test-syntax-table "WO" 1 `(,(syntax-string-to-code ".")) 1)
  ;; Apply word to punctuation
  (test-syntax-table "W." 1 `(,(syntax-string-to-code "w")) 2))

;; Test forward-comment at buffer boundaries
;; #### The second Assert fails (once interpreted, once compiled) on 21.4.9
;; with sjt's version of Andy's syntax-text-property-killer patch.
(with-temp-buffer
  (if (not (fboundp 'c-mode))
      ;; #### This whole thing should go inside a macro Skip-Test
      (let* ((reason "c-mode unavailable")
	     (count (gethash reason skipped-test-reasons)))
	;;(message "%S: %S" reason count)
	(puthash reason (if (null count) 1 (1+ count))
		 skipped-test-reasons)
	(Print-Skip "comment and parse-partial-sexp tests" reason))
    (c-mode)

    (insert "// comment\n")
    (forward-comment -2)
    (Assert (eq (point) (point-min)))

    (let ((point (point)))
      (insert "/* comment */")
      (goto-char point)
      (forward-comment 2)
      (Assert (eq (point) (point-max)))

      ;; this last used to crash
      (parse-partial-sexp point (point-max)))))
