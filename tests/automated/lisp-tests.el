;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: Martin Buchholz <martin@xemacs.org>
;; Maintainer: Martin Buchholz <martin@xemacs.org>
;; Created: 1998
;; Keywords: tests

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;;; Test basic Lisp engine functionality
;;; See test-harness.el for instructions on how to run these tests.

(eval-when-compile
  (condition-case nil
      (require 'test-harness)
    (file-error
     (push "." load-path)
     (when (and (boundp 'load-file-name) (stringp load-file-name))
       (push (file-name-directory load-file-name) load-path))
     (require 'test-harness))))

(Check-Error wrong-number-of-arguments (setq setq-test-foo))
(Check-Error wrong-number-of-arguments (setq setq-test-foo 1 setq-test-bar))
(Check-Error wrong-number-of-arguments (setq-default setq-test-foo))
(Check-Error wrong-number-of-arguments (setq-default setq-test-foo 1 setq-test-bar))
(Assert (eq (setq)         nil))
(Assert (eq (setq-default) nil))
(Assert (eq (setq         setq-test-foo 42) 42))
(Assert (eq (setq-default setq-test-foo 42) 42))
(Assert (eq (setq         setq-test-foo 42 setq-test-bar 99) 99))
(Assert (eq (setq-default setq-test-foo 42 setq-test-bar 99) 99))

(macrolet ((test-setq (expected-result &rest body)
		      `(progn
			 (defun test-setq-fun () ,@body)
			 (Assert (eq ,expected-result (test-setq-fun)))
			 (byte-compile 'test-setq-fun)
			 (Assert (eq ,expected-result (test-setq-fun))))))
  (test-setq nil (setq))
  (test-setq nil (setq-default))
  (test-setq 42  (setq         test-setq-var 42))
  (test-setq 42  (setq-default test-setq-var 42))
  (test-setq 42  (setq         test-setq-bar 99 test-setq-var 42))
  (test-setq 42  (setq-default test-setq-bar 99 test-setq-var 42))
  )

(let ((my-vector [1 2 3 4])
      (my-bit-vector (bit-vector 1 0 1 0))
      (my-string "1234")
      (my-list '(1 2 3 4)))

  ;;(Assert (fooooo)) ;; Generate Other failure
  ;;(Assert (eq 1 2)) ;; Generate Assertion failure

  (dolist (sequence (list my-vector my-bit-vector my-string my-list))
    (Assert (sequencep sequence))
    (Assert (eq 4 (length sequence))))

  (dolist (array (list my-vector my-bit-vector my-string))
    (Assert (arrayp array)))

  (Assert (eq (elt my-vector 0) 1))
  (Assert (eq (elt my-bit-vector 0) 1))
  (Assert (eq (elt my-string 0) ?1))
  (Assert (eq (elt my-list 0) 1))

  (fillarray my-vector 5)
  (fillarray my-bit-vector 1)
  (fillarray my-string ?5)

  (dolist (array (list my-vector my-bit-vector))
    (Assert (eq 4 (length array))))

  (Assert (eq (elt my-vector 0) 5))
  (Assert (eq (elt my-bit-vector 0) 1))
  (Assert (eq (elt my-string 0) ?5))

  (Assert (eq (elt my-vector 3) 5))
  (Assert (eq (elt my-bit-vector 3) 1))
  (Assert (eq (elt my-string 3) ?5))

  (fillarray my-bit-vector 0)
  (Assert (eq 4 (length my-bit-vector)))
  (Assert (eq (elt my-bit-vector 2) 0))
  )

(defun make-circular-list (length)
  "Create evil emacs-crashing circular list of length LENGTH"
  (let ((circular-list
	 (make-list
	  length
	  'you-are-trapped-in-a-twisty-maze-of-cons-cells-all-alike)))
    (setcdr (last circular-list) circular-list)
    circular-list))

;;-----------------------------------------------------
;; Test `nconc'
;;-----------------------------------------------------
(defun make-list-012 () (list 0 1 2))

(Check-Error wrong-type-argument (nconc 'foo nil))

(dolist (length '(1 2 3 4 1000 2000))
  (Check-Error circular-list (nconc (make-circular-list length) 'foo))
  (Check-Error circular-list (nconc '(1 . 2) (make-circular-list length) 'foo))
  (Check-Error circular-list (nconc '(1 . 2) '(3 . 4) (make-circular-list length) 'foo)))

(Assert (eq (nconc) nil))
(Assert (eq (nconc nil) nil))
(Assert (eq (nconc nil nil) nil))
(Assert (eq (nconc nil nil nil) nil))

(let ((x (make-list-012))) (Assert (eq (nconc nil x) x)))
(let ((x (make-list-012))) (Assert (eq (nconc x nil) x)))
(let ((x (make-list-012))) (Assert (eq (nconc nil x nil) x)))
(let ((x (make-list-012))) (Assert (eq (nconc x) x)))
(let ((x (make-list-012))) (Assert (eq (nconc x (make-circular-list 3)) x)))

(Assert (equal (nconc '(1 . 2) '(3 . 4) '(5 . 6)) '(1 3 5 . 6)))

(let ((y (nconc (make-list-012) nil (list 3 4 5) nil)))
  (Assert (eq (length y) 6))
  (Assert (eq (nth 3 y) 3)))

;;-----------------------------------------------------
;; Test `last'
;;-----------------------------------------------------
(Check-Error wrong-type-argument (last 'foo))
(Check-Error wrong-number-of-arguments (last))
(Check-Error wrong-number-of-arguments (last '(1 2) 1 1))
(Check-Error circular-list (last (make-circular-list 1)))
(Check-Error circular-list (last (make-circular-list 2000)))
(let ((x (list 0 1 2 3)))
  (Assert (eq (last nil) nil))
  (Assert (eq (last x 0) nil))
  (Assert (eq (last x  ) (cdddr x)))
  (Assert (eq (last x 1) (cdddr x)))
  (Assert (eq (last x 2) (cddr x)))
  (Assert (eq (last x 3) (cdr x)))
  (Assert (eq (last x 4) x))
  (Assert (eq (last x 9) x))
  (Assert (eq (last '(1 . 2) 0) 2))
  )

;;-----------------------------------------------------
;; Test `butlast' and `nbutlast'
;;-----------------------------------------------------
(Check-Error wrong-type-argument (butlast  'foo))
(Check-Error wrong-type-argument (nbutlast 'foo))
(Check-Error wrong-number-of-arguments (butlast))
(Check-Error wrong-number-of-arguments (nbutlast))
(Check-Error wrong-number-of-arguments (butlast  '(1 2) 1 1))
(Check-Error wrong-number-of-arguments (nbutlast '(1 2) 1 1))
(Check-Error circular-list (butlast  (make-circular-list 1)))
(Check-Error circular-list (nbutlast (make-circular-list 1)))
(Check-Error circular-list (butlast  (make-circular-list 2000)))
(Check-Error circular-list (nbutlast (make-circular-list 2000)))

(let* ((x (list 0 1 2 3))
       (y (butlast x))
       (z (nbutlast x)))
  (Assert (eq z x))
  (Assert (not (eq y x)))
  (Assert (equal y '(0 1 2)))
  (Assert (equal z y)))

(let* ((x (list 0 1 2 3 4))
       (y (butlast x 2))
       (z (nbutlast x 2)))
  (Assert (eq z x))
  (Assert (not (eq y x)))
  (Assert (equal y '(0 1 2)))
  (Assert (equal z y)))

(let* ((x (list 0 1 2 3))
       (y (butlast x 0))
       (z (nbutlast x 0)))
  (Assert (eq z x))
  (Assert (not (eq y x)))
  (Assert (equal y '(0 1 2 3)))
  (Assert (equal z y)))

(Assert (eq (butlast  '(x)) nil))
(Assert (eq (nbutlast '(x)) nil))
(Assert (eq (butlast  '()) nil))
(Assert (eq (nbutlast '()) nil))

;;-----------------------------------------------------
;; Test `copy-list'
;;-----------------------------------------------------
(Check-Error wrong-type-argument (copy-list 'foo))
(Check-Error wrong-number-of-arguments (copy-list))
(Check-Error wrong-number-of-arguments (copy-list '(1 2) 1))
(Check-Error circular-list (copy-list (make-circular-list 1)))
(Check-Error circular-list (copy-list (make-circular-list 2000)))
(Assert (eq '() (copy-list '())))
(dolist (x '((1) (1 2) (1 2 3) (1 2 . 3)))
  (let ((y (copy-list x)))
    (Assert (and (equal x y) (not (eq x y))))))

;;-----------------------------------------------------
;; Arithmetic operations
;;-----------------------------------------------------

;; Test `+'
(Assert (eq (+ 1 1) 2))
(Assert (= (+ 1.0 1.0) 2.0))
(Assert (= (+ 1.0 3.0 0.0) 4.0))
(Assert (= (+ 1 1.0) 2.0))
(Assert (= (+ 1.0 1) 2.0))
(Assert (= (+ 1.0 1 1) 3.0))
(Assert (= (+ 1 1 1.0) 3.0))
(Assert (eq (1+ most-positive-fixnum) most-negative-fixnum))
(Assert (eq (+ most-positive-fixnum 1) most-negative-fixnum))

;; Test `-'
(Check-Error wrong-number-of-arguments (-))
(Assert (eq (- 0) 0))
(Assert (eq (- 1) -1))
(dolist (one `(1 1.0 ?\1 ,(Int-to-Marker 1)))
  (Assert (= (+ 1 one) 2))
  (Assert (= (+ one) 1))
  (Assert (= (+ one) one))
  (Assert (= (- one) -1))
  (Assert (= (- one one) 0))
  (Assert (= (- one one one) -1))
  (Assert (= (- 0 one) -1))
  (Assert (= (- 0 one one) -2))
  (Assert (= (+ one 1) 2))
  (dolist (zero '(0 0.0 ?\0))
    (Assert (= (+ 1 zero) 1))
    (Assert (= (+ zero 1) 1))
    (Assert (= (- zero) zero))
    (Assert (= (- zero) 0))
    (Assert (= (- zero zero) 0))
    (Assert (= (- zero one one) -2))))

(Assert (= (- 1.5 1) .5))
(Assert (= (- 1 1.5) (- .5)))

(Assert (eq (1- most-negative-fixnum) most-positive-fixnum))
(Assert (eq (- most-negative-fixnum 1) most-positive-fixnum))

;; Test `/'

;; Test division by zero errors
(dolist (zero '(0 0.0 ?\0))
  (Check-Error arith-error (/ zero))
  (dolist (n1 `(42 42.0 ?\042 ,(Int-to-Marker 42)))
    (Check-Error arith-error (/ n1 zero))
    (dolist (n2 `(3 3.0 ?\03 ,(Int-to-Marker 3)))
      (Check-Error arith-error (/ n1 n2 zero)))))

;; Other tests for `/'
(Check-Error wrong-number-of-arguments (/))
(let (x)
  (Assert (= (/ (setq x 2))   0))
  (Assert (= (/ (setq x 2.0)) 0.5)))

(dolist (six '(6 6.0 ?\06))
  (dolist (two '(2 2.0 ?\02))
    (dolist (three '(3 3.0 ?\03))
      (Assert (= (/ six two) three)))))

(dolist (three '(3 3.0 ?\03))
  (Assert (= (/ three 2.0) 1.5)))
(dolist (two '(2 2.0 ?\02))
  (Assert (= (/ 3.0 two) 1.5)))

;; Test `*'
(Assert (= 1 (*)))

(dolist (one `(1 1.0 ?\01 ,(Int-to-Marker 1)))
  (Assert (= 1 (* one))))

(dolist (two '(2 2.0 ?\02))
  (Assert (= 2 (* two))))

(dolist (six '(6 6.0 ?\06))
  (dolist (two '(2 2.0 ?\02))
    (dolist (three '(3 3.0 ?\03))
      (Assert (= (* three two) six)))))

(dolist (three '(3 3.0 ?\03))
  (dolist (two '(2 2.0 ?\02))
    (Assert (= (* 1.5 two) three))
    (dolist (five '(5 5.0 ?\05))
      (Assert (= 30 (* five two three))))))

;; Test `+'
(Assert (= 0 (+)))

(dolist (one `(1 1.0 ?\01 ,(Int-to-Marker 1)))
  (Assert (= 1 (+ one))))

(dolist (two '(2 2.0 ?\02))
  (Assert (= 2 (+ two))))

(dolist (five '(5 5.0 ?\05))
  (dolist (two '(2 2.0 ?\02))
    (dolist (three '(3 3.0 ?\03))
      (Assert (= (+ three two) five))
      (Assert (= 10 (+ five two three))))))

;; Test `max', `min'
(dolist (one `(1 1.0 ?\01 ,(Int-to-Marker 1)))
  (Assert (= one (max one)))
  (Assert (= one (max one one)))
  (Assert (= one (max one one one)))
  (Assert (= one (min one)))
  (Assert (= one (min one one)))
  (Assert (= one (min one one one)))
  (dolist (two `(2 2.0 ?\02 ,(Int-to-Marker 2)))
    (Assert (= one (min one two)))
    (Assert (= one (min one two two)))
    (Assert (= one (min two two one)))
    (Assert (= two (max one two)))
    (Assert (= two (max one two two)))
    (Assert (= two (max two two one)))))

;; The byte compiler has special handling for these constructs:
(let ((three 3) (five 5))
  (Assert (= (+ three five 1) 9))
  (Assert (= (+ 1 three five) 9))
  (Assert (= (+ three five -1) 7))
  (Assert (= (+ -1 three five) 7))
  (Assert (= (+ three 1) 4))
  (Assert (= (+ three -1) 2))
  (Assert (= (+ -1 three) 2))
  (Assert (= (+ -1 three) 2))
  (Assert (= (- three five 1) -3))
  (Assert (= (- 1 three five) -7))
  (Assert (= (- three five -1) -1))
  (Assert (= (- -1 three five) -9))
  (Assert (= (- three 1) 2))
  (Assert (= (- three 2 1) 0))
  (Assert (= (- 2 three 1) -2))
  (Assert (= (- three -1) 4))
  (Assert (= (- three 0) 3))
  (Assert (= (- three 0 five) -2))
  (Assert (= (- 0 three 0 five) -8))
  (Assert (= (- 0 three five) -8))
  (Assert (= (* three 2) 6))
  (Assert (= (* three -1 five) -15))
  (Assert (= (* three 1 five) 15))
  (Assert (= (* three 0 five) 0))
  (Assert (= (* three 2 five) 30))
  (Assert (= (/ three 1) 3))
  (Assert (= (/ three -1) -3))
  (Assert (= (/ (* five five) 2 2) 6))
  (Assert (= (/ 64 five 2) 6)))


;;-----------------------------------------------------
;; Logical bit-twiddling operations
;;-----------------------------------------------------
(Assert (= (logxor)  0))
(Assert (= (logior)  0))
(Assert (= (logand) -1))

(Check-Error wrong-type-argument (logxor 3.0))
(Check-Error wrong-type-argument (logior 3.0))
(Check-Error wrong-type-argument (logand 3.0))

(dolist (three '(3 ?\03))
  (Assert (eq 3 (logand three)))
  (Assert (eq 3 (logxor three)))
  (Assert (eq 3 (logior three)))
  (Assert (eq 3 (logand three three)))
  (Assert (eq 0 (logxor three three)))
  (Assert (eq 3 (logior three three))))

(dolist (one `(1 ?\01 ,(Int-to-Marker 1)))
  (dolist (two '(2 ?\02))
    (Assert (eq 0 (logand one two)))
    (Assert (eq 3 (logior one two)))
    (Assert (eq 3 (logxor one two))))
  (dolist (three '(3 ?\03))
    (Assert (eq 1 (logand one three)))
    (Assert (eq 3 (logior one three)))
    (Assert (eq 2 (logxor one three)))))

;;-----------------------------------------------------
;; Test `%', mod
;;-----------------------------------------------------
(Check-Error wrong-number-of-arguments (%))
(Check-Error wrong-number-of-arguments (% 1))
(Check-Error wrong-number-of-arguments (% 1 2 3))

(Check-Error wrong-number-of-arguments (mod))
(Check-Error wrong-number-of-arguments (mod 1))
(Check-Error wrong-number-of-arguments (mod 1 2 3))

(Check-Error wrong-type-argument (% 10.0 2))
(Check-Error wrong-type-argument (% 10 2.0))

(dotimes (j 30)
  (let ((x (- (random) (random))))
    (Assert (eq x (+ (% x 17) (* (/ x 17) 17))))
    (Assert (eq (- x) (+ (% (- x) 17) (* (/ (- x) 17) 17))))
    (Assert (eq (% x -17) (- (% (- x) 17))))
    ))

(macrolet
    ((division-test (seven)
    `(progn
       (Assert (eq (% ,seven      2)  1))
       (Assert (eq (% ,seven     -2)  1))
       (Assert (eq (% (- ,seven)  2) -1))
       (Assert (eq (% (- ,seven) -2) -1))

       (Assert (eq (% ,seven      4)  3))
       (Assert (eq (% ,seven     -4)  3))
       (Assert (eq (% (- ,seven)  4) -3))
       (Assert (eq (% (- ,seven) -4) -3))

       (Assert (eq (%  35 ,seven)     0))
       (Assert (eq (% -35 ,seven)     0))
       (Assert (eq (%  35 (- ,seven)) 0))
       (Assert (eq (% -35 (- ,seven)) 0))

       (Assert (eq (mod ,seven      2)  1))
       (Assert (eq (mod ,seven     -2) -1))
       (Assert (eq (mod (- ,seven)  2)  1))
       (Assert (eq (mod (- ,seven) -2) -1))

       (Assert (eq (mod ,seven      4)  3))
       (Assert (eq (mod ,seven     -4) -1))
       (Assert (eq (mod (- ,seven)  4)  1))
       (Assert (eq (mod (- ,seven) -4) -3))

       (Assert (eq (mod  35 ,seven)     0))
       (Assert (eq (mod -35 ,seven)     0))
       (Assert (eq (mod  35 (- ,seven)) 0))
       (Assert (eq (mod -35 (- ,seven)) 0))

       (Assert (= (mod ,seven      2.0)  1.0))
       (Assert (= (mod ,seven     -2.0) -1.0))
       (Assert (= (mod (- ,seven)  2.0)  1.0))
       (Assert (= (mod (- ,seven) -2.0) -1.0))

       (Assert (= (mod ,seven      4.0)  3.0))
       (Assert (= (mod ,seven     -4.0) -1.0))
       (Assert (= (mod (- ,seven)  4.0)  1.0))
       (Assert (= (mod (- ,seven) -4.0) -3.0))

       (Assert (eq (% 0 ,seven) 0))
       (Assert (eq (% 0 (- ,seven)) 0))

       (Assert (eq (mod 0 ,seven) 0))
       (Assert (eq (mod 0 (- ,seven)) 0))

       (Assert (= (mod 0.0 ,seven) 0.0))
       (Assert (= (mod 0.0 (- ,seven)) 0.0)))))

  (division-test 7)
  (division-test ?\07)
  (division-test (Int-to-Marker 7)))



;;-----------------------------------------------------
;; Arithmetic comparison operations
;;-----------------------------------------------------
(Check-Error wrong-number-of-arguments (=))
(Check-Error wrong-number-of-arguments (<))
(Check-Error wrong-number-of-arguments (>))
(Check-Error wrong-number-of-arguments (<=))
(Check-Error wrong-number-of-arguments (>=))
(Check-Error wrong-number-of-arguments (/=))

;; One argument always yields t
(loop for x in `(1 1.0 ,(Int-to-Marker 1) ?z) do
  (Assert (eq t (=  x)))
  (Assert (eq t (<  x)))
  (Assert (eq t (>  x)))
  (Assert (eq t (>= x)))
  (Assert (eq t (<= x)))
  (Assert (eq t (/= x)))
  )

;; Type checking
(Check-Error wrong-type-argument (=  'foo 1))
(Check-Error wrong-type-argument (<= 'foo 1))
(Check-Error wrong-type-argument (>= 'foo 1))
(Check-Error wrong-type-argument (<  'foo 1))
(Check-Error wrong-type-argument (>  'foo 1))
(Check-Error wrong-type-argument (/= 'foo 1))

;; Meat
(dolist (one `(1 1.0 ,(Int-to-Marker 1) ?\01))
  (dolist (two '(2 2.0 ?\02))
    (Assert (<  one two))
    (Assert (<= one two))
    (Assert (<= two two))
    (Assert (>  two one))
    (Assert (>= two one))
    (Assert (>= two two))
    (Assert (/= one two))
    (Assert (not (/= two two)))
    (Assert (not (< one one)))
    (Assert (not (> one one)))
    (Assert (<= one one two two))
    (Assert (not (< one one two two)))
    (Assert (>= two two one one))
    (Assert (not (> two two one one)))
    (Assert (= one one one))
    (Assert (not (= one one one two)))
    (Assert (not (/= one two one)))
    ))

(dolist (one `(1 1.0 ,(Int-to-Marker 1) ?\01))
  (dolist (two '(2 2.0 ?\02))
    (Assert (<  one two))
    (Assert (<= one two))
    (Assert (<= two two))
    (Assert (>  two one))
    (Assert (>= two one))
    (Assert (>= two two))
    (Assert (/= one two))
    (Assert (not (/= two two)))
    (Assert (not (< one one)))
    (Assert (not (> one one)))
    (Assert (<= one one two two))
    (Assert (not (< one one two two)))
    (Assert (>= two two one one))
    (Assert (not (> two two one one)))
    (Assert (= one one one))
    (Assert (not (= one one one two)))
    (Assert (not (/= one two one)))
    ))

;; ad-hoc
(Assert (< 1 2))
(Assert (< 1 2 3 4 5 6))
(Assert (not (< 1 1)))
(Assert (not (< 2 1)))


(Assert (not (< 1 1)))
(Assert (< 1 2 3 4 5 6))
(Assert (<= 1 2 3 4 5 6))
(Assert (<= 1 2 3 4 5 6 6))
(Assert (not (< 1 2 3 4 5 6 6)))
(Assert (<= 1 1))

(Assert (not (eq (point) (point-marker))))
(Assert (= 1 (Int-to-Marker 1)))
(Assert (= (point) (point-marker)))

;;-----------------------------------------------------
;; testing list-walker functions
;;-----------------------------------------------------
(macrolet
    ((test-fun
      (fun)
      `(progn
	 (Check-Error wrong-number-of-arguments (,fun))
	 (Check-Error wrong-number-of-arguments (,fun nil))
	 (Check-Error malformed-list (,fun nil 1))
	 ,@(loop for n in '(1 2 2000)
	     collect `(Check-Error circular-list (,fun 1 (make-circular-list ,n))))))
     (test-funs (&rest funs) `(progn ,@(loop for fun in funs collect `(test-fun ,fun)))))

  (test-funs member old-member
	     memq   old-memq
	     assoc  old-assoc
	     rassoc old-rassoc
	     rassq  old-rassq
	     delete old-delete
	     delq   old-delq
	     remassoc remassq remrassoc remrassq))

(let ((x '((1 . 2) 3 (4 . 5))))
  (Assert (eq (assoc  1 x) (car x)))
  (Assert (eq (assq   1 x) (car x)))
  (Assert (eq (rassoc 1 x) nil))
  (Assert (eq (rassq  1 x) nil))
  (Assert (eq (assoc  2 x) nil))
  (Assert (eq (assq   2 x) nil))
  (Assert (eq (rassoc 2 x) (car x)))
  (Assert (eq (rassq  2 x) (car x)))
  (Assert (eq (assoc  3 x) nil))
  (Assert (eq (assq   3 x) nil))
  (Assert (eq (rassoc 3 x) nil))
  (Assert (eq (rassq  3 x) nil))
  (Assert (eq (assoc  4 x) (caddr x)))
  (Assert (eq (assq   4 x) (caddr x)))
  (Assert (eq (rassoc 4 x) nil))
  (Assert (eq (rassq  4 x) nil))
  (Assert (eq (assoc  5 x) nil))
  (Assert (eq (assq   5 x) nil))
  (Assert (eq (rassoc 5 x) (caddr x)))
  (Assert (eq (rassq  5 x) (caddr x)))
  (Assert (eq (assoc  6 x) nil))
  (Assert (eq (assq   6 x) nil))
  (Assert (eq (rassoc 6 x) nil))
  (Assert (eq (rassq  6 x) nil)))

(let ((x '(("1" . "2") "3" ("4" . "5"))))
  (Assert (eq (assoc  "1" x) (car x)))
  (Assert (eq (assq   "1" x) nil))
  (Assert (eq (rassoc "1" x) nil))
  (Assert (eq (rassq  "1" x) nil))
  (Assert (eq (assoc  "2" x) nil))
  (Assert (eq (assq   "2" x) nil))
  (Assert (eq (rassoc "2" x) (car x)))
  (Assert (eq (rassq  "2" x) nil))
  (Assert (eq (assoc  "3" x) nil))
  (Assert (eq (assq   "3" x) nil))
  (Assert (eq (rassoc "3" x) nil))
  (Assert (eq (rassq  "3" x) nil))
  (Assert (eq (assoc  "4" x) (caddr x)))
  (Assert (eq (assq   "4" x) nil))
  (Assert (eq (rassoc "4" x) nil))
  (Assert (eq (rassq  "4" x) nil))
  (Assert (eq (assoc  "5" x) nil))
  (Assert (eq (assq   "5" x) nil))
  (Assert (eq (rassoc "5" x) (caddr x)))
  (Assert (eq (rassq  "5" x) nil))
  (Assert (eq (assoc  "6" x) nil))
  (Assert (eq (assq   "6" x) nil))
  (Assert (eq (rassoc "6" x) nil))
  (Assert (eq (rassq  "6" x) nil)))

(flet ((a () (list '(1 . 2) 3 '(4 . 5))))
  (Assert (let* ((x (a)) (y (remassoc  1 x))) (and (not (eq x y)) (equal y '(3 (4 . 5))))))
  (Assert (let* ((x (a)) (y (remassq   1 x))) (and (not (eq x y)) (equal y '(3 (4 . 5))))))
  (Assert (let* ((x (a)) (y (remrassoc 1 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassq  1 x))) (and (eq x y) (equal y (a)))))

  (Assert (let* ((x (a)) (y (remassoc  2 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remassq   2 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc 2 x))) (and (not (eq x y)) (equal y '(3 (4 . 5))))))
  (Assert (let* ((x (a)) (y (remrassq  2 x))) (and (not (eq x y)) (equal y '(3 (4 . 5))))))

  (Assert (let* ((x (a)) (y (remassoc  3 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remassq   3 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc 3 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassq  3 x))) (and (eq x y) (equal y (a)))))

  (Assert (let* ((x (a)) (y (remassoc  4 x))) (and (eq x y) (equal y '((1 . 2) 3)))))
  (Assert (let* ((x (a)) (y (remassq   4 x))) (and (eq x y) (equal y '((1 . 2) 3)))))
  (Assert (let* ((x (a)) (y (remrassoc 4 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassq  4 x))) (and (eq x y) (equal y (a)))))

  (Assert (let* ((x (a)) (y (remassoc  5 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remassq   5 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc 5 x))) (and (eq x y) (equal y '((1 . 2) 3)))))
  (Assert (let* ((x (a)) (y (remrassq  5 x))) (and (eq x y) (equal y '((1 . 2) 3)))))

  (Assert (let* ((x (a)) (y (remassoc  6 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remassq   6 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc 6 x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassq  6 x))) (and (eq x y) (equal y (a)))))

  (Assert (let* ((x (a)) (y (delete     3 x))) (and (eq x y) (equal y '((1 . 2) (4 . 5))))))
  (Assert (let* ((x (a)) (y (delq       3 x))) (and (eq x y) (equal y '((1 . 2) (4 . 5))))))
  (Assert (let* ((x (a)) (y (old-delete 3 x))) (and (eq x y) (equal y '((1 . 2) (4 . 5))))))
  (Assert (let* ((x (a)) (y (old-delq   3 x))) (and (eq x y) (equal y '((1 . 2) (4 . 5))))))

  (Assert (let* ((x (a)) (y (delete     '(1 . 2) x))) (and (not (eq x y)) (equal y '(3 (4 . 5))))))
  (Assert (let* ((x (a)) (y (delq       '(1 . 2) x))) (and      (eq x y)  (equal y (a)))))
  (Assert (let* ((x (a)) (y (old-delete '(1 . 2) x))) (and (not (eq x y)) (equal y '(3 (4 . 5))))))
  (Assert (let* ((x (a)) (y (old-delq   '(1 . 2) x))) (and      (eq x y)  (equal y (a)))))
  )



(flet ((a () (list '("1" . "2") "3" '("4" . "5"))))
  (Assert (let* ((x (a)) (y (remassoc  "1" x))) (and (not (eq x y)) (equal y '("3" ("4" . "5"))))))
  (Assert (let* ((x (a)) (y (remassq   "1" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc "1" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassq  "1" x))) (and (eq x y) (equal y (a)))))

  (Assert (let* ((x (a)) (y (remassoc  "2" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remassq   "2" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc "2" x))) (and (not (eq x y)) (equal y '("3" ("4" . "5"))))))
  (Assert (let* ((x (a)) (y (remrassq  "2" x))) (and (eq x y) (equal y (a)))))

  (Assert (let* ((x (a)) (y (remassoc  "3" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remassq   "3" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc "3" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassq  "3" x))) (and (eq x y) (equal y (a)))))

  (Assert (let* ((x (a)) (y (remassoc  "4" x))) (and (eq x y) (equal y '(("1" . "2") "3")))))
  (Assert (let* ((x (a)) (y (remassq   "4" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc "4" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassq  "4" x))) (and (eq x y) (equal y (a)))))

  (Assert (let* ((x (a)) (y (remassoc  "5" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remassq   "5" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc "5" x))) (and (eq x y) (equal y '(("1" . "2") "3")))))
  (Assert (let* ((x (a)) (y (remrassq  "5" x))) (and (eq x y) (equal y (a)))))

  (Assert (let* ((x (a)) (y (remassoc  "6" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remassq   "6" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassoc "6" x))) (and (eq x y) (equal y (a)))))
  (Assert (let* ((x (a)) (y (remrassq  "6" x))) (and (eq x y) (equal y (a))))))

;;-----------------------------------------------------
;; function-max-args, function-min-args
;;-----------------------------------------------------
(defmacro check-function-argcounts (fun min max)
  `(progn
     (Assert (eq (function-min-args ,fun) ,min))
     (Assert (eq (function-max-args ,fun) ,max))))

(check-function-argcounts 'prog1 1 nil)         ; special form
(check-function-argcounts 'command-execute 1 3)	; normal subr
(check-function-argcounts 'funcall 1 nil)       ; `MANY' subr
(check-function-argcounts 'garbage-collect 0 0) ; no args subr

;; Test interpreted and compiled functions
(loop for (arglist min max) in
  '(((arg1 arg2 &rest args) 2 nil)
    ((arg1 arg2 &optional arg3 arg4) 2 4)
    ((arg1 arg2 &optional arg3 arg4 &rest args) 2 nil)
    (() 0 0))
  do
  (eval
   `(progn
      (defun test-fun ,arglist nil)
      (check-function-argcounts '(lambda ,arglist nil) ,min ,max)
      (check-function-argcounts (byte-compile '(lambda ,arglist nil)) ,min ,max))))

;;-----------------------------------------------------
;; Detection of cyclic variable indirection loops
;;-----------------------------------------------------
(fset 'test-sym1 'test-sym1)
(Check-Error cyclic-function-indirection (test-sym1))

(fset 'test-sym1 'test-sym2)
(fset 'test-sym2 'test-sym1)
(Check-Error cyclic-function-indirection (test-sym1))
(fmakunbound 'test-sym1) ; else macroexpand-internal infloops!
(fmakunbound 'test-sym2)

;;-----------------------------------------------------
;; Test `type-of'
;;-----------------------------------------------------
(Assert (eq (type-of load-path) 'cons))
(Assert (eq (type-of obarray) 'vector))
(Assert (eq (type-of 42) 'integer))
(Assert (eq (type-of ?z) 'character))
(Assert (eq (type-of "42") 'string))
(Assert (eq (type-of 'foo) 'symbol))
(Assert (eq (type-of (selected-device)) 'device))

;;-----------------------------------------------------
;; Test mapping functions
;;-----------------------------------------------------
(Check-Error wrong-type-argument (mapcar #'identity (current-buffer)))
(Assert (equal (mapcar #'identity load-path) load-path))
(Assert (equal (mapcar #'identity '(1 2 3)) '(1 2 3)))
(Assert (equal (mapcar #'identity "123") '(?1 ?2 ?3)))
(Assert (equal (mapcar #'identity [1 2 3]) '(1 2 3)))
(Assert (equal (mapcar #'identity #*010) '(0 1 0)))

(let ((z 0) (list (make-list 1000 1)))
  (mapc (lambda (x) (incf z x)) list)
  (Assert (eq 1000 z)))

(Check-Error wrong-type-argument (mapvector #'identity (current-buffer)))
(Assert (equal (mapvector #'identity '(1 2 3)) [1 2 3]))
(Assert (equal (mapvector #'identity "123") [?1 ?2 ?3]))
(Assert (equal (mapvector #'identity [1 2 3]) [1 2 3]))
(Assert (equal (mapvector #'identity #*010) [0 1 0]))

(Check-Error wrong-type-argument (mapconcat #'identity (current-buffer) "foo"))
(Assert (equal (mapconcat #'identity '("1" "2" "3") "|") "1|2|3"))
(Assert (equal (mapconcat #'identity ["1" "2" "3"]  "|") "1|2|3"))

;; The following 2 functions used to crash XEmacs via mapcar1().
;; We don't test the actual values of the mapcar, since they're undefined.
(Assert
 (let ((x (list (cons 1 1) (cons 2 2) (cons 3 3))))
   (mapcar
    (lambda (y)
      "Devious evil mapping function"
      (when (eq (car y) 2) ; go out onto a limb
	(setcdr x nil)     ; cut it off behind us
	(garbage-collect)) ; are we riding a magic broomstick?
      (car y))             ; sorry, hard landing
    x)))

(Assert
 (let ((x (list (cons 1 1) (cons 2 2) (cons 3 3))))
   (mapcar
    (lambda (y)
      "Devious evil mapping function"
      (when (eq (car y) 1)
	(setcdr (cdr x) 42)) ; drop a brick wall onto the freeway
      (car y))
    x)))

;;-----------------------------------------------------
;; Test vector functions
;;-----------------------------------------------------
(Assert (equal [1 2 3] [1 2 3]))
(Assert (equal [] []))
(Assert (not (equal [1 2 3] [])))
(Assert (not (equal [1 2 3] [1 2 4])))
(Assert (not (equal [0 2 3] [1 2 3])))
(Assert (not (equal [1 2 3] [1 2 3 4])))
(Assert (not (equal [1 2 3 4] [1 2 3])))
(Assert (equal (vector 1 2 3) [1 2 3]))
(Assert (equal (make-vector 3 1) [1 1 1]))

;;-----------------------------------------------------
;; Test bit-vector functions
;;-----------------------------------------------------
(Assert (equal #*010 #*010))
(Assert (equal #* #*))
(Assert (not (equal #*010 #*011)))
(Assert (not (equal #*010 #*)))
(Assert (not (equal #*110 #*010)))
(Assert (not (equal #*010 #*0100)))
(Assert (not (equal #*0101 #*010)))
(Assert (equal (bit-vector 0 1 0) #*010))
(Assert (equal (make-bit-vector 3 1) #*111))
(Assert (equal (make-bit-vector 3 0) #*000))

;;-----------------------------------------------------
;; Test buffer-local variables used as (ugh!) function parameters
;;-----------------------------------------------------
(make-local-variable 'test-emacs-buffer-local-variable)
(byte-compile
 (defun test-emacs-buffer-local-parameter (test-emacs-buffer-local-variable)
   (setq test-emacs-buffer-local-variable nil)))
(test-emacs-buffer-local-parameter nil)

;;-----------------------------------------------------
;; Test split-string
;;-----------------------------------------------------
;; Hrvoje didn't like these tests so I'm disabling them for now. -sb
;(Assert (equal (split-string "foo" "") '("" "f" "o" "o" "")))
;(Assert (equal (split-string "foo" "^") '("" "foo")))
;(Assert (equal (split-string "foo" "$") '("foo" "")))
(Assert (equal (split-string "foo,bar" ",") '("foo" "bar")))
(Assert (equal (split-string ",foo,bar," ",") '("" "foo" "bar" "")))
(Assert (equal (split-string ",foo,bar," "^,") '("" "foo,bar,")))
(Assert (equal (split-string ",foo,bar," ",$") '(",foo,bar" "")))
(Assert (equal (split-string ",foo,,bar," ",") '("" "foo" "" "bar" "")))
(Assert (equal (split-string "foo,,,bar" ",") '("foo" "" "" "bar")))
(Assert (equal (split-string "foo,,bar,," ",") '("foo" "" "bar" "" "")))
(Assert (equal (split-string "foo,,bar" ",+") '("foo" "bar")))
(Assert (equal (split-string ",foo,,bar," ",+") '("" "foo" "bar" "")))

(Assert (not (string-match "\\(\\.\\=\\)" ".")))
(Assert (string= "" (let ((str "test string"))
		      (if (string-match "^.*$" str)
			  (replace-match "\\U" t nil str)))))
(with-temp-buffer
  (erase-buffer)
  (insert "test string")
  (re-search-backward "^.*$")
  (replace-match "\\U" t)
  (Assert (and (bobp) (eobp))))

;;-----------------------------------------------------
;; Test near-text buffer functions.
;;-----------------------------------------------------
(with-temp-buffer
  (erase-buffer)
  (Assert (eq (char-before) nil))
  (Assert (eq (char-before (point)) nil))
  (Assert (eq (char-before (point-marker)) nil))
  (Assert (eq (char-before (point) (current-buffer)) nil))
  (Assert (eq (char-before (point-marker) (current-buffer)) nil))
  (Assert (eq (char-after) nil))
  (Assert (eq (char-after (point)) nil))
  (Assert (eq (char-after (point-marker)) nil))
  (Assert (eq (char-after (point) (current-buffer)) nil))
  (Assert (eq (char-after (point-marker) (current-buffer)) nil))
  (Assert (eq (preceding-char) 0))
  (Assert (eq (preceding-char (current-buffer)) 0))
  (Assert (eq (following-char) 0))
  (Assert (eq (following-char (current-buffer)) 0))
  (insert "foobar")
  (Assert (eq (char-before) ?r))
  (Assert (eq (char-after) nil))
  (Assert (eq (preceding-char) ?r))
  (Assert (eq (following-char) 0))
  (goto-char (point-min))
  (Assert (eq (char-before) nil))
  (Assert (eq (char-after) ?f))
  (Assert (eq (preceding-char) 0))
  (Assert (eq (following-char) ?f))
  )

;;-----------------------------------------------------
;; Test plist manipulation functions.
;;-----------------------------------------------------
(let ((sym (make-symbol "test-symbol")))
  (Assert (eq t (get* sym t t)))
  (Assert (eq t (get  sym t t)))
  (Assert (eq t (getf nil t t)))
  (Assert (eq t (plist-get nil t t)))
  (put sym 'bar 'baz)
  (Assert (eq 'baz (get sym 'bar)))
  (Assert (eq 'baz (getf '(bar baz) 'bar)))
  (Assert (eq 'baz (getf (symbol-plist sym) 'bar)))
  (Assert (eq 2 (getf '(1 2) 1)))
  (Assert (eq 4 (put sym 3 4)))
  (Assert (eq 4 (get sym 3)))
  (Assert (eq t (remprop sym 3)))
  (Assert (eq nil (remprop sym 3)))
  (Assert (eq 5 (get sym 3 5)))
  )

(loop for obj in
  (list (make-symbol "test-symbol")
	"test-string"
	(make-extent nil nil nil)
	(make-face 'test-face))
  do
  (Assert (eq 2 (get obj ?1 2)))
  (Assert (eq 4 (put obj ?3 4)))
  (Assert (eq 4 (get obj ?3)))
  (when (or (stringp obj) (symbolp obj))
    (Assert (equal '(?3 4) (object-plist obj))))
  (Assert (eq t (remprop obj ?3)))
  (when (or (stringp obj) (symbolp obj))
    (Assert (eq '() (object-plist obj))))
  (Assert (eq nil (remprop obj ?3)))
  (when (or (stringp obj) (symbolp obj))
    (Assert (eq '() (object-plist obj))))
  (Assert (eq 5 (get obj ?3 5)))
  )

(Check-Error-Message
 error "Object type has no properties"
 (get 2 'property))

(Check-Error-Message
 error "Object type has no settable properties"
 (put (current-buffer) 'property 'value))

(Check-Error-Message
 error "Object type has no removable properties"
 (remprop ?3 'property))

(Check-Error-Message
 error "Object type has no properties"
 (object-plist (symbol-function 'car)))

(Check-Error-Message
 error "Can't remove property from object"
 (remprop (make-extent nil nil nil) 'detachable))

;;-----------------------------------------------------
;; Test subseq
;;-----------------------------------------------------
(Assert (equal (subseq nil 0) nil))
(Assert (equal (subseq [1 2 3] 0) [1 2 3]))
(Assert (equal (subseq [1 2 3] 1 -1) [2]))
(Assert (equal (subseq "123" 0) "123"))
(Assert (equal (subseq "1234" -3 -1) "23"))
(Assert (equal (subseq #*0011 0) #*0011))
(Assert (equal (subseq #*0011 -3 3) #*01))
(Assert (equal (subseq '(1 2 3) 0) '(1 2 3)))
(Assert (equal (subseq '(1 2 3 4) -3 nil) '(2 3 4)))

(Check-Error wrong-type-argument (subseq 3 2))
(Check-Error args-out-of-range (subseq [1 2 3] -42))
(Check-Error args-out-of-range (subseq [1 2 3] 0 42))

;;-----------------------------------------------------
;; Time-related tests
;;-----------------------------------------------------
(Assert (= (length (current-time-string)) 24))

;;-----------------------------------------------------
;; format test
;;-----------------------------------------------------
(Assert (string= (format "%d" 10) "10"))
(Assert (string= (format "%o" 8) "10"))
(Assert (string= (format "%x" 31) "1f"))
(Assert (string= (format "%X" 31) "1F"))
(Assert (string= (format "%e" 100) "1.000000e+02"))
(Assert (string= (format "%E" 100) "1.000000E+02"))
(Assert (string= (format "%f" 100) "100.000000"))
(Assert (string= (format "%7.3f" 12.12345) " 12.123"))
(Assert (string= (format "%07.3f" 12.12345) "012.123"))
(Assert (string= (format "%-7.3f" 12.12345) "12.123 "))
(Assert (string= (format "%-07.3f" 12.12345) "12.123 "))
(Assert (string= (format "%g" 100.0) "100"))
(Assert (string= (format "%g" 0.000001) "1e-06"))
(Assert (string= (format "%g" 0.0001) "0.0001"))
(Assert (string= (format "%G" 100.0) "100"))
(Assert (string= (format "%G" 0.000001) "1E-06"))
(Assert (string= (format "%G" 0.0001) "0.0001"))

(Assert (string= (format "%2$d%1$d" 10 20) "2010"))
(Assert (string= (format "%-d" 10) "10"))
(Assert (string= (format "%-4d" 10) "10  "))
(Assert (string= (format "%+d" 10) "+10"))
(Assert (string= (format "%+d" -10) "-10"))
(Assert (string= (format "%+4d" 10) " +10"))
(Assert (string= (format "%+4d" -10) " -10"))
(Assert (string= (format "% d" 10) " 10"))
(Assert (string= (format "% d" -10) "-10"))
(Assert (string= (format "% 4d" 10) "  10"))
(Assert (string= (format "% 4d" -10) " -10"))
(Assert (string= (format "%0d" 10) "10"))
(Assert (string= (format "%0d" -10) "-10"))
(Assert (string= (format "%04d" 10) "0010"))
(Assert (string= (format "%04d" -10) "-010"))
(Assert (string= (format "%*d" 4 10) "  10"))
(Assert (string= (format "%*d" 4 -10) " -10"))
(Assert (string= (format "%*d" -4 10) "10  "))
(Assert (string= (format "%*d" -4 -10) "-10 "))
(Assert (string= (format "%#d" 10) "10"))
(Assert (string= (format "%#o" 8) "010"))
(Assert (string= (format "%#x" 16) "0x10"))
(Assert (string= (format "%#e" 100) "1.000000e+02"))
(Assert (string= (format "%#E" 100) "1.000000E+02"))
(Assert (string= (format "%#f" 100) "100.000000"))
(Assert (string= (format "%#g" 100.0) "100.000"))
(Assert (string= (format "%#g" 0.000001) "1.00000e-06"))
(Assert (string= (format "%#g" 0.0001) "0.000100000"))
(Assert (string= (format "%#G" 100.0) "100.000"))
(Assert (string= (format "%#G" 0.000001) "1.00000E-06"))
(Assert (string= (format "%#G" 0.0001) "0.000100000"))
(Assert (string= (format "%.1d" 10) "10"))
(Assert (string= (format "%.4d" 10) "0010"))
;; Combination of `-', `+', ` ', `0', `#', `.', `*'
(Assert (string= (format "%-04d" 10) "10  "))
(Assert (string= (format "%-*d" 4 10) "10  "))
;; #### Correctness of this behavior is questionable.
;; It might be better to signal error.
(Assert (string= (format "%-*d" -4 10) "10  "))
;; These behavior is not specified.
;; (format "%-+d" 10)
;; (format "%- d" 10)
;; (format "%-01d" 10)
;; (format "%-#4x" 10)
;; (format "%-.1d" 10)

(Assert (string= (format "%01.1d" 10) "10"))
(Assert (string= (format "%03.1d" 10) " 10"))
(Assert (string= (format "%01.3d" 10) "010"))
(Assert (string= (format "%1.3d" 10) "010"))
(Assert (string= (format "%3.1d" 10) " 10"))

;;; The following two tests used to use 1000 instead of 100,
;;; but that merely found buffer overflow bugs in Solaris sprintf().
(Assert (= 102 (length (format "%.100f" 3.14))))
(Assert (= 100 (length (format "%100f" 3.14))))

;;; Check for 64-bit cleanness on LP64 platforms.
(Assert (= (read (format "%d"  most-positive-fixnum)) most-positive-fixnum))
(Assert (= (read (format "%ld" most-positive-fixnum)) most-positive-fixnum))
(Assert (= (read (format "%u"  most-positive-fixnum)) most-positive-fixnum))
(Assert (= (read (format "%lu" most-positive-fixnum)) most-positive-fixnum))
(Assert (= (read (format "%d"  most-negative-fixnum)) most-negative-fixnum))
(Assert (= (read (format "%ld" most-negative-fixnum)) most-negative-fixnum))

;;; "%u" is undocumented, and Emacs Lisp has no unsigned type.
;;; What to do if "%u" is used with a negative number?
;;; The most reasonable thing seems to be to print an un-read-able number.
;;; The printed value might be useful to a human, if not to Emacs Lisp.
(Check-Error invalid-read-syntax (read (format "%u" most-negative-fixnum)))
(Check-Error invalid-read-syntax (read (format "%u" -1)))

;; Check all-completions ignore element start with space.
(Assert (not (all-completions "" '((" hidden" . "object")))))
(Assert (all-completions " " '((" hidden" . "object"))))
