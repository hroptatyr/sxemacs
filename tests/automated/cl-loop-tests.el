;;;  cl-tests.el -- Tests for CL
;; Copyright (C) 2006, 2007 Sebastian Freundt
;;
;; Author: Sebastian Freundt <hroptatyr@sxemacs.org>
;; Keywords: tests
;;
;; This file is part of SXEmacs.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the author nor the names of any contributors
;;    may be used to endorse or promote products derived from this
;;    software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;; Synched up with: Not in FSF.
;;
;;; Commentary:
;; See test-harness.el for instructions on how to run these tests.

(eval-when-compile
  (condition-case nil
      (require 'test-harness)
    (file-error
     (push "." load-path)
     (when (and (boundp 'load-file-name) (stringp load-file-name))
       (push (file-name-directory load-file-name) load-path))
     (require 'test-harness))))

;;;###eval-me-first
(and (featurep 'modules)
     (locate-module "cl-loop")
     (require 'cl-loop))

(when (featurep 'cl-loop)
  ;;; do/dotimes/dolist tests
  (Assert
   (eq (cl:do ((temp-one 1 (1+ temp-one))
	       (temp-two 0 (1- temp-two)))
	      ((> (- temp-one temp-two) 5) temp-one)) 4))
  (Assert
   (eq (cl:do ((temp-one 1 (1+ temp-one))
	       (temp-two 0 (1+ temp-one)))
	      ((= 3 temp-two) temp-one)) 3))
  (Assert
   (eq (cl:do* ((temp-one 1 (1+ temp-one))
		(temp-two 0 (1+ temp-one)))
	       ((= 3 temp-two) temp-one)) 2))

  (cl:dotimes (i 10 j)
	      (setq j (1+ i)))
  (Assert (= j 10))

  (cl:dolist (i '(1 2 17) j)
	     (setq j (1+ i)))
  (Assert (= j 18))

  (Assert (null (cl:do-symbols (i))))
  (Assert (null (cl:do-all-symbols (i))))


  ;;; loop tests
  (let* ((coll 0))
    (cl:loop for i from -2 do (incf coll))
    (Assert (= coll 3)))
  (let* ((coll 0))
    (cl:loop for i downto -2 do (incf coll))
    (Assert (= coll 3)))
  (let* ((coll 0))
    (cl:loop for i above -2 do (incf coll))
    (Assert (= coll 2)))
  (let* ((coll 0))
    (cl:loop for i below 2 do (incf coll))
    (Assert (= coll 2)))
  (let* ((coll 0))
    (cl:loop for i to 20 by 10 do (incf coll))
    (Assert (= coll 3)))
  (let* ((coll 0))
    (cl:loop for i below 20 by 10 do (incf coll))
    (Assert (= coll 2)))
  (let* ((coll 0))
    (cl:loop for i downto -20 by 10 do (incf coll))
    (Assert (= coll 3)))
  (let* ((coll 0))
    (cl:loop for i downfrom 20 by 10 do (incf coll))
    (Assert (= coll 3)))

  (let* ((coll 0))
    (cl:loop
     for i to 20
     for j below 10
     do (incf coll))
    (Assert (= coll 10)))

  ;; testing epilogue clauses
  (Assert
   (equal
    (cl:loop with a = 1
	     with b = (+ a 2)
	     with c = (+ b 3)
	     return (list a b c))
    '(1 3 6)))

  (Assert
   (equal
    (let* ((a 0)
	   (b 0)
	   (c 0))
      (cl:loop with a = 1
	       and b = (+ a 2)
	       and c = (+ b 3)
	       return (list a b c)))
    '(1 2 3)))

  (Assert
   (equal
    (let* ((a 0)
	   (b 0)
	   (c 0))
      (cl:loop with a = 1
	       and b = (+ a 2)
	       and c = (+ b 3)
	       finally (setq b a)
	       return b))
    1))

  (Assert
   (equal
    (let* ((a 0)
	   (b 0)
	   (c 0))
      (cl:loop with a = 1
	       and b = (+ a 2)
	       and c = (+ b 3)
	       initially (setq a 100)
	       finally (setq b a)
	       return b))
    100))

  ;; multi-for
  (Assert
   (equal
    (cl:loop for x from 1 to 10
	     for y = nil then x
	     collect (list x y))
    '((1 nil) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9) (10 10))))

  (Assert
   (equal
    (cl:loop for x from 1 to 10
	     and y = nil then x
	     collect (list x y))
    '((1 nil) (2 1) (3 2) (4 3) (5 4) (6 5) (7 6) (8 7) (9 8) (10 9))))

  ;; accumulation COLLECT
  (if (featurep 'bigz)
      (Assert
       (equal
	(cl:loop as i across [1 2 3 4 5 6 7 8 9 10]
		 collect (and (primep i) i))
	'(nil 2 3 nil 5 nil 7 nil nil nil)))
    (Assert
     (equal
      (cl:loop as i across [1 2 3 4 5 6 7 8 9 10]
	       collect (and (evenp i) i))
      '(nil 2 nil 4 nil 6 nil 8 nil 10))))

  ;; accumulation COUNT
  (Assert
   (=
    (cl:loop for i in '(a nil b nil c d nil e)
	     count i)
    5))

  (Assert
   (equal
    (cl:loop for i in '(a nil b nil c d nil e)
	     count i into foo
	     return (list i foo))
    '(e 5)))

  (Assert
   (=
    (cl:loop for i in '(a nil b nil c d nil e)
	     count i into foo
	     count (null i) into bar
	     return bar)
    3))

  (Assert
   (=
    (cl:loop for i in '(a nil b nil c d nil e)
	     count i into foo
	     ;; last count should be returned anyway
	     count (null i) into bar)
    3))

  (Assert
   (=
    (cl:loop for i in '(a nil b nil c d nil e)
	     count (eq i t))
    0))

  (Assert
   (=
    (cl:loop for i in '(a nil b nil c d nil e)
	     count (memq i '(a b)))
    2))

  ;; accumulation SUM
  (Assert
   (=
    (cl:loop for i in '(1 2 3 4 5)
	     sum i)
    15))

  (Assert
   (=
    (cl:loop for i in '(1 2 3 4)
	     sum (* 2.0 i))
    20.0))

  (Assert
   (= (let* ((series '(1.25 4.5 5.5)))
	(cl:loop for v in series
		 sum (* 2.0 v)))
      22.5))

  (Assert
   (=
    (cl:loop for i in '(1 2 3 4)
	     sum (* 2.0 i))
    20.0))

  (Assert
   (=
    (cl:loop for i in '(1 2 3 4)
	     sum (* -2 i) into bar
	     sum bar)
    -40))

  ;; accumulate MAXIMISE/MINIMISE
  (Assert
   (=
    (cl:loop for i in '(1 2 3 4 3 2 1)
	     maximise i)
    4))

  (Assert
   (=
    (cl:loop for i in '(1 2 3 4 3 2 1)
	     minimise i)
    1))

  (Assert
   (equal
    (cl:loop for i in '(1 2 3 4 3 2 1)
	     maximise (1+ i) into foo
	     minimise (1- i) into bar
	     return (list foo bar))
    '(5 0)))

  (Assert
   (equal
    (cl:loop as i across [1 2 3 4 3 2 1]
	     maximise (1+ i) into foo
	     minimise (1- i) into bar
	     return (list foo bar))
    '(5 0)))

  (Assert
   (=
    (cl:loop for i on '(1 2 3 4 3 2 1)
	     maximise (length i))
    7))
  )
;;; cl-loop-tests.el ends here
