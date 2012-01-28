;;;  inplace-tests.el -- Regression Tests for in-place mapping
;; Copyright (C) 2006 Sebastian Freundt
;;
;; Author: Sebastian Freundt <hroptatyr@sxemacs.org>
;; Keywords: tests
;;
;; This file is part of SXEmacs.
;;
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
;;
;;; Synched up with: Not in FSF.
;;
;;; Commentary:
;;
;; See test-harness.el for instructions on how to run these tests.

(eval-when-compile
  (condition-case nil
      (require 'test-harness)
    (file-error
     (push "." load-path)
     (when (and (boundp 'load-file-name) (stringp load-file-name))
       (push (file-name-directory load-file-name) load-path))
     (require 'test-harness))))

(let ((ilist '(1 2 3 4))
      (idllist (dllist 1 2 3 4))
      (ivector [1 2 3 4])
      (istring "abcdef")
      (ibvec (bit-vector 0 1 0 1 1)))

  (mapc-inplace #'1+ ilist)
  (Assert (listp ilist))
  (Assert (= (nth 0 ilist) 2))
  (Assert (= (nth 3 ilist) 5))

  (mapc-inplace #'1- idllist)
  (Assert (dllistp idllist))
  (Assert (= (dllist-car idllist) 0))
  (Assert (= (dllist-rac idllist) 3))

  (mapc-inplace #'evenp ivector)
  (Assert (vectorp ivector))
  (Assert-Equal ivector [nil t nil t])

  ;; we can't test strings at the moment

  (mapc-inplace #'zerop ibvec)
  (Assert (bit-vector-p ibvec))
  (Assert (= (aref ibvec 0) 1))
  (Assert (= (aref ibvec 1) 0))
  (Assert (= (aref ibvec 2) 1))
  (Assert (= (aref ibvec 3) 0))
  (Assert (= (aref ibvec 4) 0))

)

;;; inplace-tests.el ends here
