;;; os-tests.el --- test support for OS interaction

;; Copyright (C) 2004 Free Software Foundation

;; Author: Stephen J. Turnbull <stephen@xemacs.org>
;; Maintainer: Stephen J. Turnbull <stephen@xemacs.org>
;; Created: 2004 October 28
;; Keywords: tests, process support

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

;; Test OS support.  Processes, environment variables, etc.
;; See test-harness.el for instructions on how to run these tests.

;; call-process-region bug reported by Katsumi Yamaoka on 2004-10-26
;; in <b9yvfcyuscf.fsf@jpl.org>, who suggested the basic test scheme
;; in <b9yoeipvwn0.fsf@jpl.org>.

(and-fboundp 'executable-find
  ;; tac works by lines, unfortunately
  (let* ((original-string "a\nb\nc\nd\n")
	 (tac-cases (if (executable-find "tac")
			'((1 . "c\nb\na\nd\n")
			  (3 . "a\nc\nb\nd\n")
			  (5 . "a\nc\nb\nd\n")
			  (7 . "a\nc\nb\nd\n")
			  (9 . "a\nd\nc\nb\n"))
		      nil))
	 (cat-cases (if (executable-find "cat")
			'((1 . "b\nc\na\nd\n")
			  (3 . "a\nb\nc\nd\n")
			  (5 . "a\nb\nc\nd\n")
			  (7 . "a\nb\nc\nd\n")
			  (9 . "a\nd\nb\nc\n"))
		      nil))
	 cases case)
    (with-temp-buffer
      (Skip-Test-Unless tac-cases
			"tac executable not found"
			"Tests of call-process-region with region deleted after inserting
tac process output."
			(setq cases tac-cases)
			(while cases
			  (setq case (car cases)
				cases (cdr cases))
			  (flet ((do-test (pos result)
				   (erase-buffer)
				   (insert original-string)
				   (goto-char pos)
				   (call-process-region 3 7 "tac" t t)
				   (goto-char (point-min))
				   (Assert (looking-at result))))
			    (do-test (car case) (cdr case)))))
      ;; if you're in that much of a hurry you can blow cat off
      ;; if you've done tac, but I'm not going to bother
      (Skip-Test-Unless cat-cases
			"cat executable not found"
			"Tests of call-process-region with region deleted after inserting
cat process output."
			(setq cases cat-cases)
			(while cases
			  (setq case (car cases)
				cases (cdr cases))
			  (flet ((do-test (pos result)
				   (erase-buffer)
				   (insert original-string)
				   (goto-char pos)
				   (call-process-region 3 7 "cat" t t)
				   (goto-char (point-min))
				   (Assert (looking-at result))))
			    (do-test (car case) (cdr case))))))))

;;; end of os-tests.el
