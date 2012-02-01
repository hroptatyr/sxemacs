;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: Martin Buchholz <martin@xemacs.org>
;; Maintainer: Martin Buchholz <martin@xemacs.org>
;; Created: 1998
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

;;; Test byte-compiler functionality
;;; See test-harness.el

(condition-case err
    (require 'test-harness)
  (file-error
   (when (and (boundp 'load-file-name) (stringp load-file-name))
     (push (file-name-directory load-file-name) load-path)
     (require 'test-harness))))

(require 'bytecomp)

;; test constant symbol warnings
(defmacro check-byte-compiler-message (message-regexp &rest body)
  `(Check-Message ,message-regexp (byte-compile '(lambda () ,@body))))

(check-byte-compiler-message "Attempt to set non-symbol" (setq 1 1))
(check-byte-compiler-message "Attempt to set constant symbol" (setq t 1))
(check-byte-compiler-message "Attempt to set constant symbol" (setq nil 1))
(check-byte-compiler-message "^$" (defconst :foo 1))

(check-byte-compiler-message "Attempt to let-bind non-symbol" (let ((1 'x)) 1))
(check-byte-compiler-message "Attempt to let-bind constant symbol" (let ((t 'x)) (foo)))
(check-byte-compiler-message "Attempt to let-bind constant symbol" (let ((nil 'x)) (foo)))
(check-byte-compiler-message "Attempt to let-bind constant symbol" (let ((:foo 'x)) (foo)))


(check-byte-compiler-message "bound but not referenced" (let ((foo 'x)) 1))
(Assert (not (boundp 'free-variable)))
(Assert (boundp 'byte-compile-warnings))
(check-byte-compiler-message "assignment to free variable" (setq free-variable 1))
(check-byte-compiler-message "reference to free variable" (car free-variable))
(check-byte-compiler-message "called with 2 args, but requires 1" (car 'x 'y))

(check-byte-compiler-message "^$" (setq :foo 1))
(let ((fun '(lambda () (setq :foo 1))))
  (fset 'test-byte-compiler-fun fun))
(Check-Error setting-constant (test-byte-compiler-fun))
(byte-compile 'test-byte-compiler-fun)
(Check-Error setting-constant (test-byte-compiler-fun))

(eval-when-compile (defvar setq-test-foo nil) (defvar setq-test-bar nil))
(progn
  (check-byte-compiler-message "set called with 1 arg, but requires 2" (setq setq-test-foo))
  (check-byte-compiler-message "set called with 1 arg, but requires 2" (setq setq-test-foo 1 setq-test-bar))
  (check-byte-compiler-message "set-default called with 1 arg, but requires 2" (setq-default setq-test-foo))
  (check-byte-compiler-message "set-default called with 1 arg, but requires 2" (setq-default setq-test-foo 1 setq-test-bar))
  )

;;-----------------------------------------------------
;; let, let*
;;-----------------------------------------------------

;; Test interpreted and compiled lisp separately here
(check-byte-compiler-message "malformed let binding" (let  ((x 1 2)) 3))
(check-byte-compiler-message "malformed let binding" (let* ((x 1 2)) 3))

(Check-Error-Message
 error "`let' bindings can have only one value-form"
 (eval '(let ((x 1 2)) 3)))

(Check-Error-Message
 error "`let' bindings can have only one value-form"
 (eval '(let* ((x 1 2)) 3)))

(defmacro before-and-after-compile-equal (&rest form)
  `(Assert-Equal (funcall (quote (lambda () ,@form)))
		 (funcall (byte-compile (quote (lambda () ,@form))))))

(defvar simplyamarker (point-min-marker))

;; The byte optimizer must be careful with +/- with a single argument.

(before-and-after-compile-equal (+))
(before-and-after-compile-equal (+ 2 2))
(before-and-after-compile-equal (+ 2 1))
(before-and-after-compile-equal (+ 1 2))
;; (+ 1) is OK. but (+1) signals an error.
(before-and-after-compile-equal (+ 1))
(before-and-after-compile-equal (+ 3))
(before-and-after-compile-equal (+ simplyamarker 1))
;; The optimization (+ m) --> m is invalid when m is a marker.
;; Currently the following test fails - controversial.
;; (before-and-after-compile-equal (+ simplyamarker))
;; Same tests for minus.
(before-and-after-compile-equal (- 2 2))
(before-and-after-compile-equal (- 2 1))
(before-and-after-compile-equal (- 1 2))
(before-and-after-compile-equal (- 1))
(before-and-after-compile-equal (- 3))
(before-and-after-compile-equal (- simplyamarker 1))
(before-and-after-compile-equal (- simplyamarker))

(before-and-after-compile-equal (let ((z 1)) (or (setq z 42)) z))

;; byte-after-unbind-ops

;; byte-constant
;; byte-dup

;; byte-symbolp
(before-and-after-compile-equal
 (let ((x 's))
   (unwind-protect
       (symbolp x)
     (setq x 1))))

;; byte-consp
(before-and-after-compile-equal
 (let ((x '(a b)))
   (unwind-protect
       (consp x)
     (setq x 1))))

;; byte-stringp
(before-and-after-compile-equal
 (let ((x "a"))
   (unwind-protect
       (stringp x)
     (setq x 1))))

;; byte-listp
(before-and-after-compile-equal
 (let ((x '(a b c)))
   (unwind-protect
       (listp x)
     (setq x 1))))

;; byte-numberp
(before-and-after-compile-equal
 (let ((x 1))
   (unwind-protect
       (numberp x)
     (setq x nil))))

;; byte-integerp
(before-and-after-compile-equal
 (let ((x 1))
   (unwind-protect
       (integerp x)
     (setq x nil))))

;; byte-equal
(before-and-after-compile-equal
 (let ((x 'a)
       (y 'a))
   (unwind-protect
       (eq x y)
     (setq x 'c))))

;; byte-not
(before-and-after-compile-equal
 (let (x)
   (unwind-protect
       (not x)
     (setq x t))))

;; byte-cons
(before-and-after-compile-equal
 (equal '(1 . 2)
	(let ((x 1)
	      (y 2))
	  (unwind-protect
	      (cons x y)
	    (setq x t)))))

;; byte-list1
(before-and-after-compile-equal
 (equal '(1)
	(let ((x 1))
	  (unwind-protect
	      (list x)
	    (setq x t)))))

;; byte-list2
(before-and-after-compile-equal
 (equal '(1 . 2)
	(let ((x 1)
	      (y 2))
	  (unwind-protect
	      (list x y)
	    (setq x t)))))

;; byte-interactive-p

;; byte-equal
(before-and-after-compile-equal
 (let (x y)
   (setq x '(1 . 2))
   (setq y '(1 . 2))
   (unwind-protect
       (equal x y)
     (setq y '(1 . 3)))))
