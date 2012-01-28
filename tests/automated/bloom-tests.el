;;;  bloom-tests.el -- Regression Tests for bloom filters
;; Copyright (C) 2005 Sebastian Freundt
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



;; create some bloom filters
(let ((b1 (make-bloom))
      (b2 (make-bloom))
      (b3 (make-bloom 512))
      (b4 (make-bloom 512 26)))
  (eval `(Assert (eq (bloom-order ,b1)
		     (bloom-order ,b2))))
  (eval `(Assert (eq (bloom-order ,b3) 512)))
  (eval `(Assert (eq (bloom-order ,b4) 512)))
  (eval `(Assert (eq (bloom-degree ,b1)
		     (bloom-degree ,b2))))
  (eval `(Assert (eq (bloom-degree ,b4) 26)))
  (eval `(Assert (eq (bloom-size ,b1) 0)))
  (eval `(Assert (eq (bloom-size ,b2) 0)))
  (eval `(Assert (eq (bloom-size ,b3) 0)))
  (eval `(Assert (eq (bloom-size ,b4) 0)))

  (Assert (bloomp b1))
  (Assert (bloomp b2))
  (Assert (bloomp b3))
  (Assert (bloomp b4))

  ;; add some elements
  (bloom-add b1 12)
  (bloom-add b1 21)
  (bloom-add b1 0)

  (eval `(Assert (bloom-owns-p ,b1 12)))
  (eval `(Assert (bloom-owns-p ,b1 21)))
  (eval `(Assert (bloom-owns-p ,b1 0)))
  (eval `(Assert (not (bloom-owns-p ,b1 13))))
  (eval `(Assert (not (bloom-owns-p ,b1 17))))
  (eval `(Assert (eq (bloom-size ,b1) 3)))

  (bloom-remove b1 12)
  (bloom-remove b1 21)

  (eval `(Assert (not (bloom-owns-p ,b1 12))))
  (eval `(Assert (not (bloom-owns-p ,b1 21))))
  (eval `(Assert (bloom-owns-p ,b1 0)))
  (eval `(Assert (eq (bloom-size ,b1) 1)))

  (bloom-add b1 12)
  (bloom-add b1 21)

  (bloom-add b2 2)
  (bloom-add b2 16)
  (bloom-add b2 0)

  (eval `(Assert (not (bloom-owns-p ,b2 12))))
  (eval `(Assert (not (bloom-owns-p ,b2 21))))
  (eval `(Assert (bloom-owns-p ,b2 2)))
  (eval `(Assert (bloom-owns-p ,b2 16)))
  (eval `(Assert (bloom-owns-p ,b2 0)))

  (setq b3 (bloom-union b1 b2))

  (eval `(Assert (bloom-owns-p ,b3 12)))
  (eval `(Assert (bloom-owns-p ,b3 21)))
  (eval `(Assert (bloom-owns-p ,b3 2)))
  (eval `(Assert (bloom-owns-p ,b3 16)))
  (eval `(Assert (bloom-owns-p ,b3 0)))

  (eval `(Assert (>= (bloom-size ,b3) 5)))
  (eval `(Assert (not (bloom-owns-p ,b3 17))))

;;; moved to man/lispref/lists.texi -hroptatyr
;;   ;; when we add 8-times-degree-times more elements than the order
;;   ;; of the bloom we expect it to turn into a universe
;;
;;   (dotimes (i (* 8 (bloom-degree b4) (bloom-order b4)))
;;     (bloom-add b4 i))
;;
;;   (eval `(Assert (bloom-owns-p ,b4 'a-symbol-i-never-added)))
;;   (eval `(Assert (bloom-owns-p ,b4 "a-string-i-never-added")))
;;   (eval `(Assert (bloom-owns-p ,b4 [a vector i never added])))
  )


;; dealing with universes
(let ((b1 (make-bloom-universe))
      (b2 (make-bloom-universe))
      (b3 (make-bloom-universe 512))
      (b4 (make-bloom-universe 512 26)))
  (eval `(Assert (eq (bloom-order ,b1)
		     (bloom-order ,b2))))
  (eval `(Assert (eq (bloom-order ,b3) 512)))
  (eval `(Assert (eq (bloom-order ,b4) 512)))
  (eval `(Assert (eq (bloom-degree ,b1)
		     (bloom-degree ,b2))))
  (eval `(Assert (eq (bloom-degree ,b4) 26)))
  (eval `(Assert (eq (bloom-size ,b1)
		     (bloom-size ,b2))))
  (eval `(Assert (eq (bloom-size ,b3)
		     (bloom-size ,b4))))

  (eval `(Assert (bloom-owns-p ,b1 21)))
  (eval `(Assert (bloom-owns-p ,b1 'a-symbol)))
  (eval `(Assert (bloom-owns-p ,b1 "a string")))
  (eval `(Assert (bloom-owns-p ,b1 [a vector])))

  ;; we can delete many elements from the universe, they are still in the bloom
  (bloom-remove b1 21)
  (eval `(Assert (bloom-owns-p ,b1 21)))

  (bloom-remove b1 'a-symbol)
  (eval `(Assert (bloom-owns-p ,b1 'a-symbol)))

  (bloom-remove b1 "a string")
  (eval `(Assert (bloom-owns-p ,b1 "a string")))

  (bloom-remove b1 [a vector])
  (eval `(Assert (bloom-owns-p ,b1 [a vector]))))


;;; bloom-tests.el ends here
