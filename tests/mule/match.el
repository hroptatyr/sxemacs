;;; Testsuite for Mule string-matching - used to crash!

;; Copyright (C) 1996 Sun Microsystems.

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

;;; Just load the file to run the test.

(defmacro test-match (result regexp string)
  `(save-excursion
     (assert (eq ,result (string-match ,regexp ,string)))
     (let ((buf (get-buffer-create "*testsuite*"))
	   (random-text  "foo$(BEl5~(B"))
       (set-buffer buf)
       (erase-buffer)
       (insert random-text)
       (insert ,string)
       (goto-char (point-min))
       (forward-char (length random-text))
       (assert (eq (progn (re-search-forward ,regexp) (match-beginning 0))
		   (+ 1 ,result (length random-text))))
       )))

(when (featurep 'mule)
  (test-match 0 "a" "a")
  (test-match 0  "[^a]" "$(B4A;z(B")
  (test-match 2  "[^a]$(B;z(B" "ab$(B4A;z(B")
  (test-match 1  "[^a]" "ab$(B4A;z(B")
  (test-match 0  "[^a]" "$(B4A(Bb$(B4A;z(Bb")
  (test-match 6  "[a]" "b$(B4A(Bb$(B4A;z(Bba")
  (test-match 2  "[a]" "b$(B4A(Bab$(B4A;z(Bba")
  (test-match 1  "[$(B4A(Ba]" "b$(B4A(Bab$(B4A;z(Bba")
  (test-match 1  "[a$(B4A(B]" "b$(B4A(Bab$(B4A;z(Bba")
  (test-match 0  "[^$(B4A(Ba]" "b$(B4A(Bab$(B4A;z(Bba")
  (test-match 5  "$(B4A(B[^$(B4A(Ba]" "a$(B4A(Bab$(B4A4A;z(Bba")
  )
