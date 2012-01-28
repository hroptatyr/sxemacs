;;;  ase-tests.el -- Tests for ASE
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
;; - test for conceptionally correct arithmetic
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
(progn
  (and (featurep 'modules)
       (locate-module "ase-set")
       (require 'ase-set))
  (and (locate-module "ase-cartesian")
       (require 'ase-cartesian)))

(when (featurep 'ase-set)
  ;; create some intervals first
  (setq i1 (ase-interval 0 1))
  (setq i2 (ase-interval 1 2))
  (setq i3 (ase-interval 2 3))
  (setq i5 (ase-interval 0 1 t t))
  (setq i6 (ase-interval 1 2 t t))
  (setq i7 (ase-interval 2 3 t t))
  (setq i8 (ase-interval 0 2))
  (setq i9 (ase-interval 1 3))
  (setq ia (ase-interval 1 1))
  (setq ib (ase-interval 1 1 t t)) ;; should be transformed to empty interv
  (setq ic (ase-interval 1 1 nil t)) ;; should be transformed to empty interv
  (setq id (ase-interval 0 0))
  (setq ie (ase-interval 2 2))
  (setq if (ase-interval 3 3))
  (setq aei (ase-empty-interval))
  (setq aui (ase-universe-interval))
  (setq it (ase-interval 2))
  (setq i123 (ase-interval 0 3))
  (setq i23 (ase-interval 1 3))

  (setq i56 (ase-interval-union i5 i6))
  (setq i67 (ase-interval-union i7 i6))
  (setq i567 (ase-interval-union i5 i6 i7))
  (setq i13 (ase-interval-union i3 i1))
  (setq isp (ase-interval-union
	     (ase-interval -infinity
			   (if (featurep 'bigq) -3/2 -3)) (ase-interval -1)))
  (setq huge (ase-interval 0 10))
  (setq tiny (ase-interval 2 5))

  (Assert (not (ase-interval-empty-p i1)))
  (Assert (not (ase-interval-empty-p i2)))
  (Assert (not (ase-interval-empty-p i3)))
  (Assert (not (ase-interval-empty-p i5)))
  (Assert (not (ase-interval-empty-p i6)))
  (Assert (not (ase-interval-empty-p i7)))
  (Assert (not (ase-interval-empty-p i8)))
  (Assert (not (ase-interval-empty-p ia)))
  (Assert (ase-interval-empty-p ib))
  (Assert (ase-interval-empty-p ic))
  (Assert (ase-interval-empty-p aei))
  (Assert (not (ase-interval-empty-p aui)))
  (Assert (ase-interval-empty-p (ase-interval-union)))
  (Assert (ase-interval-empty-p (ase-interval-union (ase-interval-union))))

  (Assert (not (ase-interval-imprimitive-p i1)))
  (Assert (not (ase-interval-imprimitive-p i2)))
  (Assert (not (ase-interval-imprimitive-p i3)))
  (Assert (not (ase-interval-imprimitive-p i5)))
  (Assert (not (ase-interval-imprimitive-p i6)))
  (Assert (not (ase-interval-imprimitive-p i7)))
  (Assert (not (ase-interval-imprimitive-p i8)))
  (Assert (not (ase-interval-imprimitive-p i9)))
  (Assert (ase-interval-imprimitive-p i567))
  (Assert (ase-interval-imprimitive-p i13))

  (Assert (ase-interval-contains-p i1 0))
  (Assert (ase-interval-contains-p i2 2))
  (Assert (not (ase-interval-contains-p i5 0)))
  (when (featurep 'bigq)
    (Assert (ase-interval-contains-p i5 1/2)))
  (Assert (ase-interval-contains-p i5 0.9999))
  (Assert (not (ase-interval-contains-p i8 i9)))
  (Assert (not (ase-interval-contains-p i9 i8)))
  (Assert (ase-interval-contains-p ia 1))
  (Assert (not (ase-interval-contains-p ib 1)))
  (Assert (not (ase-interval-contains-p i567 0)))
  (Assert (not (ase-interval-contains-p i567 1)))
  (when (featurep 'bigq)
    (Assert (ase-interval-contains-p i567 1/2)))
  (Assert (ase-interval-contains-p i567 i5))
  (Assert (ase-interval-contains-p i567 i56))
  (Assert (not (ase-interval-contains-p i567 i8)))
  (Assert (not (ase-interval-contains-p
		i567 (ase-interval-union i5 it))))
  (when (featurep 'bigq)
    (Assert (ase-interval-contains-p
	     i567 (ase-interval-union (ase-interval 1/2) (ase-interval 3/2))))
    (Assert (ase-interval-contains-p
	     i123
	     (ase-interval-union (ase-interval 1/2) (ase-interval 3/2)))))
  (Assert (not (ase-interval-contains-p i23 i56)))
  (Assert (ase-interval-contains-p i23 i67))
  (Assert (ase-interval-contains-p i123 i567))
  (Assert (not (ase-interval-contains-p i23 i567)))
  (Assert (not (ase-interval-contains-p aei aei)))
  (Assert (not (ase-interval-contains-p aei 0)))
  (Assert (not (ase-interval-contains-p aei -1)))
  (Assert (not (ase-interval-contains-p aei 1)))
  (Assert (not (ase-interval-contains-p aei +infinity)))
  (Assert (not (ase-interval-contains-p aei i1)))
  (Assert (not (ase-interval-contains-p aei i2)))
  (Assert (not (ase-interval-contains-p aei i3)))
  (Assert (not (ase-interval-contains-p aei i5)))
  (Assert (not (ase-interval-contains-p aei i6)))
  (Assert (not (ase-interval-contains-p aei i7)))
  (Assert (not (ase-interval-contains-p aei i8)))
  (Assert (not (ase-interval-contains-p aei i9)))
  (when (featurep 'bigq)
    (Assert (eq t (ase-interval-contains-where
		   i567 (ase-interval-union (ase-interval 1/2)
					    (ase-interval 3/2)))))
    (Assert (eq t (ase-interval-contains-where
		   i123 (ase-interval-union (ase-interval 1/2)
					    (ase-interval 3/2))))))
  (Assert (eq t (ase-interval-contains-where i123 i56)))
  (Assert (eq t (ase-interval-contains-where i123 i567)))
  (Assert (not (ase-interval-contains-where i23 i567)))
  (Assert (not (ase-interval-contains-where i567 0)))
  (Assert (not (ase-interval-contains-where i567 1)))
  (when (featurep 'bigq)
    (Assert (ase-interval-equal-p (ase-interval-contains-where i567 1/2) i5)))
  (Assert (ase-interval-equal-p (ase-interval-contains-where i567 i5) i5))
  (Assert (not (ase-interval-contains-p i6 ia)))
  (Assert (not (ase-interval-contains-p ia i6)))

  (Assert (ase-interval-connected-p i1 i2))
  (Assert (not (ase-interval-connected-p i1 i3)))
  (Assert (ase-interval-connected-p i1 i5))
  (Assert (ase-interval-connected-p i1 i6))
  (Assert (not (ase-interval-connected-p i5 i6)))
  (Assert (ase-interval-connected-p ia i5))
  (Assert (ase-interval-connected-p i8 i9))
  (Assert (not (ase-interval-connected-p i1 i3)))
  (Assert (ase-interval-connected-p i2 i1 i3))
  (Assert (ase-interval-connected-p i2 i13 i1 i3))
  (Assert (not (ase-interval-connected-p i13 i1)))
  (Assert (not (ase-interval-connected-p i13)))
  (Assert (not (ase-interval-connected-p i13 i13)))
  (Assert (ase-interval-connected-p i13 i2))
  (Assert (ase-interval-connected-p aei i13 i2 aei i2))
  (Assert (ase-interval-connected-p ia i6))
  (Assert (ase-interval-connected-p i6 ia))

  (Assert (not (ase-interval-disjoint-p i1 i2)))
  (Assert (ase-interval-disjoint-p i1 i3))
  (Assert (not (ase-interval-disjoint-p i1 i5)))
  (Assert (ase-interval-disjoint-p i1 i6))
  (Assert (ase-interval-disjoint-p i5 i6))
  (Assert (ase-interval-disjoint-p ia i5))
  (Assert (not (ase-interval-disjoint-p i8 i9)))
  (Assert (not (ase-interval-disjoint-p i13 i1)))
  (Assert (not (ase-interval-disjoint-p i13 i3)))
  (Assert (not (ase-interval-disjoint-p i1 i13)))
  (Assert (not (ase-interval-disjoint-p i3 i13)))
  (Assert (not (ase-interval-disjoint-p i1 i1 i3)))
  (Assert (not (ase-interval-disjoint-p i3 i1 i3)))
  (Assert (not (ase-interval-disjoint-p i13 i1)))
  (Assert (not (ase-interval-disjoint-p i13 i13)))
  (Assert (not (ase-interval-disjoint-p i13 (ase-interval-union i5 i7))))
  (Assert (ase-interval-disjoint-p i13 (ase-interval -infinity -1)))
  (Assert (ase-interval-disjoint-p i13 isp))

  (let ((i02o (ase-interval 0 2 t t)))
    (Assert (ase-interval-equal-p i02o (ase-interval-union i5 ia i6)))
    (Assert (ase-interval-equal-p i02o (ase-interval-union i5 ia i6)))
    (Assert (ase-interval-equal-p i02o (ase-interval-union ia i5 i6)))
    (Assert (ase-interval-equal-p i02o (ase-interval-union ia i6 i5)))
    (Assert (ase-interval-equal-p i02o (ase-interval-union i5 i6 ia)))
    (Assert (ase-interval-equal-p i02o (ase-interval-union i6 ia i5)))
    (Assert (ase-interval-equal-p i02o (ase-interval-union i6 i5 ia))))

  (Assert (ase-interval-open-p aei))
  (Assert (ase-interval-open-p aui))
  (Assert (ase-interval-open-p (ase-interval -infinity 2 t t)))
  (Assert (not (ase-interval-open-p (ase-interval -infinity 2))))
  (Assert (not (ase-interval-open-p i1)))
  (Assert (not (ase-interval-open-p i2)))
  (Assert (not (ase-interval-open-p i3)))
  (Assert (not (ase-interval-open-p (ase-interval 1 2 t))))
  (Assert (not (ase-interval-open-p (ase-interval 1 2 nil t))))
  (Assert (ase-interval-open-p (ase-interval 1 2 t t)))
  (Assert (ase-interval-open-p i5))
  (Assert (ase-interval-open-p i6))
  (Assert (ase-interval-open-p i7))
  (Assert (not (ase-interval-open-p i123)))
  (Assert (ase-interval-open-p i567))
  (Assert (not (ase-interval-open-p
		(ase-interval-union i5 (ase-interval 2 4)))))
  (Assert (not (ase-interval-open-p
		(ase-interval-union i1 (ase-interval 2 4)))))
  (Assert (ase-interval-open-p
	   (ase-interval-union i5 (ase-interval 2 4 t t))))

  (Assert (ase-interval-closed-p aei))
  (Assert (ase-interval-closed-p aui))
  (Assert (ase-interval-closed-p (ase-interval -infinity 2)))
  (Assert (not (ase-interval-closed-p (ase-interval -infinity 2 t t))))
  (Assert (ase-interval-closed-p i1))
  (Assert (ase-interval-closed-p i2))
  (Assert (ase-interval-closed-p i3))
  (Assert (not (ase-interval-closed-p (ase-interval 1 2 t))))
  (Assert (not (ase-interval-closed-p (ase-interval 1 2 nil t))))
  (Assert (not (ase-interval-closed-p (ase-interval 1 2 t t))))
  (Assert (not (ase-interval-closed-p i5)))
  (Assert (not (ase-interval-closed-p i6)))
  (Assert (not (ase-interval-closed-p i7)))
  (Assert (ase-interval-closed-p i123))
  (Assert (not (ase-interval-closed-p i567)))
  (Assert (not (ase-interval-closed-p
		(ase-interval-union i5 (ase-interval 2 4)))))
  (Assert (ase-interval-closed-p
	   (ase-interval-union i1 (ase-interval 2 4))))
  (Assert (not (ase-interval-closed-p
		(ase-interval-union i5 (ase-interval 2 4 t t)))))

  ;; testing unions
  (Assert (ase-interval-equal-p ia (ase-interval-union ia)))
  (Assert (ase-interval-equal-p huge (ase-interval-union huge)))
  (Assert (ase-interval-equal-p huge (ase-interval-union huge i1)))
  (Assert (ase-interval-equal-p huge (ase-interval-union huge i2)))
  (Assert (ase-interval-equal-p huge (ase-interval-union huge i3)))
  (Assert (ase-interval-equal-p huge (ase-interval-union huge i5)))
  (Assert (ase-interval-equal-p huge (ase-interval-union huge i6)))
  (Assert (ase-interval-equal-p huge (ase-interval-union huge i7)))
  (Assert (ase-interval-equal-p huge (ase-interval-union huge i123)))
  (Assert (ase-interval-equal-p huge (ase-interval-union huge i567)))
  (Assert (ase-interval-equal-p huge (ase-interval-union huge tiny)))
  (Assert (ase-interval-equal-p (ase-interval 1 2 nil t)
				(ase-interval-union i6 ia)))
  (Assert (ase-interval-equal-p (ase-interval 1 2 nil t)
				(ase-interval-union ia i6)))
  (Assert (ase-interval-equal-p (ase-interval-union i5 i3)
				(ase-interval-union aei i3 aei i5 i5 aei)))
  (Assert (ase-interval-equal-p (ase-interval-union i3 i5)
				(ase-interval-union aei i3 i5 aei i5 aei)))
  (Assert (ase-interval-equal-p
	   huge (ase-interval-union
		 (ase-interval-union i1 i3 (ase-interval 1 6)) huge)))

  (Assert (not (ase-interval-imprimitive-p (ase-interval-union ia))))
  (Assert (not (ase-interval-imprimitive-p (ase-interval-union huge))))
  (Assert (not (ase-interval-imprimitive-p (ase-interval-union huge i1))))
  (Assert (not (ase-interval-imprimitive-p (ase-interval-union huge i2))))
  (Assert (not (ase-interval-imprimitive-p (ase-interval-union huge i3))))
  (Assert (not (ase-interval-imprimitive-p (ase-interval-union huge i5))))
  (Assert (not (ase-interval-imprimitive-p (ase-interval-union huge i6))))
  (Assert (not (ase-interval-imprimitive-p (ase-interval-union huge i7))))
  (Assert (not (ase-interval-imprimitive-p (ase-interval-union huge i123))))
  (Assert (not (ase-interval-imprimitive-p (ase-interval-union huge i567))))
  (Assert (not (ase-interval-imprimitive-p (ase-interval-union huge tiny))))
  (Assert (not (ase-interval-imprimitive-p (ase-interval-union i6 ia))))
  (Assert (not (ase-interval-imprimitive-p (ase-interval-union ia i6))))
  (Assert (ase-interval-imprimitive-p (ase-interval-union aei i3 i5 aei i5 aei)))


  ;; test measures
  (Assert (= (ase-interval-lebesgue-measure i1) 1))
  (Assert (= (ase-interval-lebesgue-measure i2) 1))
  (Assert (= (ase-interval-lebesgue-measure i3) 1))
  (Assert (= (ase-interval-lebesgue-measure i5) 1))
  (Assert (= (ase-interval-lebesgue-measure i6) 1))
  (Assert (= (ase-interval-lebesgue-measure i7) 1))
  (Assert (= (ase-interval-lebesgue-measure i8) 2))
  (Assert (= (ase-interval-lebesgue-measure i9) 2))
  (Assert (= (ase-interval-lebesgue-measure ia) 0))
  (Assert (= (ase-interval-lebesgue-measure ib) 0))
  (Assert (= (ase-interval-lebesgue-measure ic) 0))
  (Assert (= (ase-interval-lebesgue-measure aei) 0))
  (Assert (= (ase-interval-lebesgue-measure aui) +infinity))
  (Assert (= (ase-interval-lebesgue-measure i56) 2))
  (Assert (= (ase-interval-lebesgue-measure i567) 3))
  (Assert (= (ase-interval-lebesgue-measure i13) 2))
  (Assert (= (ase-interval-lebesgue-measure huge) 10))
  (Assert (= (ase-interval-lebesgue-measure tiny) 3))
  (Assert
   (almost= (ase-interval-lebesgue-measure (ase-interval 0.1 1.2)) 1.1 1e-4))
  (Assert
   (almost= (ase-interval-lebesgue-measure (ase-interval -0.1 0.2)) 0.3 1e-4))
  (Assert
   (almost= (ase-interval-lebesgue-measure (ase-interval -1.2 -1.1)) 0.1 1e-4))

  (Assert (= (ase-interval-rational-measure i1) 2))
  (Assert (= (ase-interval-rational-measure i2) 2))
  (Assert (= (ase-interval-rational-measure i3) 2))
  (Assert (= (ase-interval-rational-measure i5) 0))
  (Assert (= (ase-interval-rational-measure i6) 0))
  (Assert (= (ase-interval-rational-measure i7) 0))
  (Assert (= (ase-interval-rational-measure i8) 3))
  (Assert (= (ase-interval-rational-measure i9) 3))
  (Assert (= (ase-interval-rational-measure ia) 1))
  (Assert (= (ase-interval-rational-measure ib) 0))
  (Assert (= (ase-interval-rational-measure ic) 0))
  (Assert (= (ase-interval-rational-measure aei) 0))
  (Assert (= (ase-interval-rational-measure aui) +infinity))
  (Assert (= (ase-interval-rational-measure i56) 0))
  (Assert (= (ase-interval-rational-measure i567) 0))
  (Assert (= (ase-interval-rational-measure i13) 4))
  (Assert (= (ase-interval-rational-measure huge) 11))
  (Assert (= (ase-interval-rational-measure tiny) 4))
  (Assert (= (ase-interval-rational-measure (ase-interval 0.1 1.2)) 1))
  (Assert (= (ase-interval-rational-measure (ase-interval -0.1 0.2)) 1))
  (Assert (= (ase-interval-rational-measure (ase-interval -1.2 -1.1)) 0))
  (Assert (= (ase-interval-rational-measure (ase-interval -1 1)) 3))
  (Assert (= (ase-interval-rational-measure (ase-interval -1 1 t t)) 1))


  ;; testing accessors
  (Assert (= (ase-interval-lower i1) 0))
  (Assert (= (ase-interval-lower i5) 0))
  (Assert (= (ase-interval-lower i2) 1))
  (Assert (= (ase-interval-lower i6) 1))
  (Assert (= (ase-interval-lower i3) 2))
  (Assert (= (ase-interval-lower i7) 2))
  (Assert (= (ase-interval-upper i1) 1))
  (Assert (= (ase-interval-upper i5) 1))
  (Assert (= (ase-interval-upper i2) 2))
  (Assert (= (ase-interval-upper i6) 2))
  (Assert (= (ase-interval-upper i3) 3))
  (Assert (= (ase-interval-upper i7) 3))
  (Assert (ase-interval-equal-p
	   (dllist-car (ase-interval-explode-union i567)) i5))

  ;; testing intersections
  (Assert (ase-interval-equal-p (ase-interval-intersection i123 i8) i8))
  (Assert (ase-interval-equal-p (ase-interval-intersection i567 i8) i56))
  (Assert (ase-interval-equal-p
	   (ase-interval-intersection
	    (ase-interval 1 3) (ase-interval 2 4)) i3))
  (Assert (eq
	   (ase-interval-intersection
	    (ase-interval 1 3) (ase-interval -2 -4)) aei))
  (Assert (ase-interval-equal-p
	   (ase-interval-intersection huge tiny)
	   (ase-interval-intersection tiny huge)))
  (Assert (ase-interval-equal-p
	   (ase-interval-intersection tiny tiny tiny tiny) tiny))
  (Assert (eq
	   (ase-interval-intersection tiny aei huge) aei))
  (Assert (ase-interval-equal-p
	   (ase-interval-intersection huge aui) huge))

  ;; testing differences
  (Assert (ase-interval-equal-p
	   (ase-interval-difference i8 i9) ;; [0 1)
	   (ase-interval 0 1 nil t)))
  (Assert (ase-interval-equal-p
	   (ase-interval-difference i9 i8) ;; (2 3]
	   (ase-interval 2 3 t)))
  (Assert (ase-interval-empty-p
	   (ase-interval-difference tiny huge)))
  (Assert (ase-interval-empty-p
	   (ase-interval-difference tiny tiny)))
  (Assert (ase-interval-empty-p
	   (ase-interval-difference aei tiny)))
  (Assert (ase-interval-equal-p
	   (ase-interval-difference huge tiny) ;; [0 2) + (5 10]
	   (ase-interval-union
	    (ase-interval 0 2 nil t) (ase-interval 5 10 t))))
  (Assert (ase-interval-equal-p
	   (ase-interval-difference huge tiny tiny) ;; [0 2) + (5 10]
	   (ase-interval-union
	    (ase-interval 0 2 nil t) (ase-interval 5 10 t))))
  (Assert (ase-interval-equal-p
	   (ase-interval-difference tiny aei) tiny))
  (Assert (ase-interval-equal-p
	   (ase-interval-difference huge aei) huge))
  (Assert (ase-interval-equal-p
	   (ase-interval-difference i567 aei) i567))
  (Assert (ase-interval-equal-p
	   (ase-interval-difference tiny) tiny))
  (Assert (ase-interval-equal-p
	   (ase-interval-difference i123 i3)
	   (ase-interval 0 2 nil t)))

  ;; boundaries
  (Assert (ase-interval-equal-p
	   (ase-interval-boundary i1)
	   (ase-interval-union (ase-interval 0) (ase-interval 1))))
  (Assert (ase-interval-empty-p
	   (ase-interval-boundary ia)))
  (Assert (ase-interval-equal-p
	   (ase-interval-boundary i567)
	   (ase-interval-union
	    (ase-interval 0) (ase-interval 1)
	    (ase-interval 2) (ase-interval 3))))
  (Assert (ase-interval-empty-p
	   (ase-interval-boundary
	    (ase-interval-boundary i1))))
  (Assert (ase-interval-empty-p
	   (ase-interval-boundary
	    (ase-interval-boundary i567))))
  (Assert (ase-interval-equal-p
	   ;; i123 \ b(i567) => i567
	   (ase-interval-difference
	    i123 (ase-interval-boundary i567)) i567))
  (Assert (ase-interval-empty-p
	   ;; i567 n b(i567) => ( )
	   (ase-interval-intersection
	    i567 (ase-interval-boundary i567))))
  ;; interiors
  (Assert (ase-interval-equal-p (ase-interval-interior i1) i5))
  (Assert (ase-interval-equal-p (ase-interval-interior i5) i5))
  (Assert (ase-interval-empty-p (ase-interval-interior ia)))
  (Assert (ase-interval-equal-p
	   (ase-interval-interior (ase-interval-union ia i7)) i7))
  (Assert (ase-interval-equal-p
	   (ase-interval-interior i123) (ase-interval 0 3 t t)))
  ;; closures
  (Assert (ase-interval-equal-p (ase-interval-closure i1) i1))
  (Assert (ase-interval-equal-p (ase-interval-closure i5) i1))
  (Assert (ase-interval-equal-p (ase-interval-closure ia) ia))
  (Assert (ase-interval-equal-p
	   (ase-interval-closure (ase-interval-union ia i7))
	   (ase-interval-union ia i3)))
  (Assert (ase-interval-equal-p
	   (ase-interval-closure i567) i123))
  )

;; testing interior products of intervals
(when (featurep 'ase-cartesian)
  ;; some interior products
  (setq c11 (ase-cartesian* i1 i1))
  (setq c12 (ase-cartesian* i1 i2))
  (setq c21 (ase-cartesian* i2 i1))
  (setq c22 (ase-cartesian* i2 i2))
  (setq c33 (ase-cartesian* i3 i3))
  (setq c555 (ase-cartesian* i5 i5 i5))
  (setq c777 (ase-cartesian* i7 i7 i7))
  (setq c788 (ase-cartesian* i7 i8 i8))
  (setq c55 (ase-cartesian* i5 i5))
  (setq c66 (ase-cartesian* i6 i6))
  (setq c15 (ase-cartesian* i1 i5))
  (setq c16 (ase-cartesian* i1 i6))
  (setq c51 (ase-cartesian* i5 i1))
  (setq c0303 (ase-cartesian* i123 i123))
  (setq c030303 (ase-cartesian* i123 i123 i123))
  (setq caa (ase-cartesian* ia ia))
  (setq cHO (ase-cartesian* (ase-interval 0 2 t)
			    (ase-interval -1 1 nil t)))
  ;; degenerated interior interval product
  (setq cdeg (ase-cartesian* ia i1))
  ;; only corner connected
  (setq ccc1 (ase-cartesian* (ase-interval 0 1 nil t)
			     (ase-interval 0 1 t))
	ccc2 (ase-cartesian* (ase-interval 1 2 nil t)
			     (ase-interval -1 0)))
  ;; points
  (setq p00 (ase-cartesian* 0 0))
  (setq p11 (ase-cartesian* 1 1))
  (setq phalfhalf (ase-cartesian* 0.5 0.5))
  ;; some unions
  (setq t1 (ase-interval-union c11 c22)) ;; doesnt merge
  (setq t2 (ase-interval-union c11 c12)) ;; does merge (2nd dimension)
  (setq t3 (ase-interval-union c21 c11)) ;; does merge (1st dimension)
  (setq t4 (ase-interval-union c11 c22 c21 c12)) ;; does merge (all dimensions)
  (setq t5 (ase-interval-union c21 c12))
  (setq t133 (ase-interval-union t1 c33))
  (setq t5566 (ase-interval-union c55 c66))
  (setq t15 (ase-interval-union t1 t5))
  (setq t555777 (ase-interval-union c555 c777))
  (setq t555788 (ase-interval-union c555 c788))
  (setq ttt12 (ase-interval-union ccc1 ccc2))
  (setq chuge (ase-cartesian* huge huge))
  (setq ctiny (ase-cartesian* tiny tiny))
  (setq cst1 (ase-cartesian* i3 i2))
  (setq cst2 (ase-cartesian* i123 i123))
  (setq cst3 (ase-cartesian* i1 i2))
  (setq cst4 (ase-cartesian* i8 i8))
  (setq cst5 (ase-cartesian* (ase-interval 2 4 t t) i2))
  (setq cst6 (ase-cartesian* (ase-interval 2 4 t t)
			     (ase-interval -1 1 t t)))

  (Assert (ase-interval-imprimitive-p c11))
  (Assert (ase-interval-imprimitive-p c555))
  (Assert (eq (ase-cartesian-ground-domain c11) 'ase:interval))
  (Assert (eq (ase-cartesian-ground-domain c555) 'ase:interval))
  (Assert (ase-interval-closed-p c11))
  (Assert (not (ase-interval-open-p c11)))
  (Assert (ase-interval-open-p c555))
  (Assert (not (ase-interval-closed-p c555)))
  (Assert (not (ase-interval-open-p c15)))
  (Assert (not (ase-interval-closed-p c15)))
  (Assert (not (ase-interval-open-p c51)))
  (Assert (not (ase-interval-closed-p c15)))
  (Assert (ase-interval-empty-p (ase-cartesian* i1 aei)))
  (Assert (ase-interval-empty-p (ase-cartesian* aei i1)))
  (Assert (ase-interval-empty-p (ase-cartesian* i1 aei i1)))
  (Assert (ase-interval-empty-p (ase-cartesian* i1 i1 i1 aei)))

  (Assert (ase-interval-contains-p c11 c55))
  (Assert (ase-interval-contains-p c0303 c11))
  (Assert (ase-interval-contains-p c0303 c55))
  (Assert (ase-interval-contains-p cHO c55))
  (Assert (not (ase-interval-contains-p cHO c11)))
  (Assert (not (ase-interval-contains-p c55 c11)))
  (Assert (ase-interval-contains-p c030303 c555))
  (Assert (not (ase-interval-contains-p c555 c030303)))
  (Check-Error embed-error (ase-interval-contains-p c11 c555))
  (Check-Error embed-error (ase-interval-contains-p c555 c55))
  (Check-Error embed-error (ase-interval-contains-p huge c11))
  (Check-Error embed-error (ase-interval-contains-p c11 i1))
  (Check-Error embed-error (ase-interval-contains-p c11 0))
  (Assert (ase-interval-contains-p c11 p00))
  (Assert (ase-interval-contains-p c11 p11))
  (Assert (ase-interval-contains-p c11 phalfhalf))
  (Assert (ase-interval-contains-p c55 phalfhalf))
  (Assert (not (ase-interval-contains-p c55 p00)))
  (Assert (not (ase-interval-contains-p c55 p11)))

  (Assert (not (ase-interval-disjoint-p c55 c11)))
  (Assert (not (ase-interval-disjoint-p c11 c55)))
  (Assert (not (ase-interval-disjoint-p c11 c12)))
  (Assert (not (ase-interval-disjoint-p c12 c11)))
  (Assert (ase-interval-disjoint-p c15 c16))
  (Assert (ase-interval-disjoint-p c16 c15))
  (Assert (not (ase-interval-disjoint-p cHO c11)))
  (Assert (ase-interval-disjoint-p cHO c12))
  (Assert (ase-interval-disjoint-p c12 cHO))
  (Assert (not (ase-interval-disjoint-p c21 cHO)))
  (Assert (ase-interval-disjoint-p ccc1 ccc2))
  (Assert (ase-interval-disjoint-p c55 caa c66))

  (Assert (ase-interval-equal-p c11 c11))
  (Assert (ase-interval-equal-p c15 c15))
  (Assert (not (ase-interval-equal-p c15 c51)))
  (Assert (not (ase-interval-equal-p c51 c15)))

  (Assert (ase-interval-connected-p cdeg))
  (Assert (ase-interval-connected-p c11))
  (Check-Error embed-error (ase-interval-connected-p i1 c11))
  (Assert (ase-interval-connected-p c11 c11))
  (Assert (ase-interval-connected-p c11 caa))
  (Assert (ase-interval-connected-p caa c11))
  (Assert (ase-interval-connected-p c11 cdeg))
  (Assert (ase-interval-connected-p cdeg c11))
  (Assert (ase-interval-connected-p c12 c15))
  (Assert (ase-interval-connected-p c15 c12))
  (Assert (ase-interval-connected-p c51 c15))
  (Assert (ase-interval-connected-p c15 c51))
  (Assert (ase-interval-connected-p c51 c11))
  (Assert (ase-interval-connected-p c11 c51))
  (Assert (ase-interval-connected-p c51 c16))
  (Assert (ase-interval-connected-p c16 c51))
  (Assert (ase-interval-connected-p c11 c22))
  (Assert (ase-interval-connected-p c15 caa))
  (Assert (ase-interval-connected-p c15 cdeg))
  (Assert (ase-interval-connected-p caa cdeg))
  (Assert (ase-interval-connected-p c55 caa))
  (Assert (ase-interval-connected-p c55 cdeg))
  (Assert (not (ase-interval-connected-p c55 c66)))
  (Assert (ase-interval-connected-p caa c66))
  (Assert (ase-interval-connected-p c55 caa c66))
  (Assert (ase-interval-connected-p c030303 c555))
  (Assert (ase-interval-connected-p c0303 c55))
  (Assert (ase-interval-connected-p c0303 c11))
  (Assert (ase-interval-connected-p cHO))
  (Assert (ase-interval-connected-p cHO c11))
  (Assert (ase-interval-connected-p cHO c12))
  (Assert (ase-interval-connected-p c12 cHO))
  (Assert (ase-interval-connected-p c21 cHO))
  (Assert (ase-interval-connected-p ccc1 ccc2))

  ;; testing unions of cartesian products
  (Assert (ase-interval-equal-p
	   (ase-interval-union c11 aei) c11))
  (Assert (ase-interval-equal-p
	   (ase-interval-union aei c11) c11))
  (Assert (ase-interval-equal-p
	   (ase-interval-union c555 aei) c555))
  (Assert (ase-interval-equal-p
	   (ase-interval-union aei c555) c555))
  (Assert (ase-interval-equal-p
	   t133 (ase-interval-union c11 c22 c33)))
  (Assert (ase-interval-equal-p
	   t133 (ase-interval-union c22 c33 c11)))
  (Assert (ase-interval-equal-p
	   (ase-interval-union t1 c21)
	   (ase-interval-union c21 c22 aei c11)))
  (Assert (ase-interval-equal-p t15 t4))
  (Assert (ase-interval-contains-p t133 p00))
  (Assert (ase-interval-contains-p t133 p11))
  (Assert (ase-interval-contains-p t133 phalfhalf))
  (Assert (ase-interval-contains-p t133 c55))
  (Assert (ase-interval-contains-p t133 c66))
  (Assert (not (ase-interval-contains-p t5566 p00)))
  (Assert (not (ase-interval-contains-p t5566 p11)))
  (Assert (ase-interval-contains-p t5566 phalfhalf))
  (Assert (ase-interval-connected-p t133))
  (Assert (not (ase-interval-connected-p t5566)))
  (Assert (ase-interval-connected-p t4 c33))
  (Assert (ase-interval-connected-p c33 t4))
  (Assert (not (ase-interval-disjoint-p t15 t4)))
  (Assert (not (ase-interval-disjoint-p t15 t4)))
  (Assert (not (ase-interval-disjoint-p t4 c11)))
  (Assert (not (ase-interval-disjoint-p c11 t4)))
  (Assert (not (ase-interval-disjoint-p c66 t15)))
  (Assert (not (ase-interval-disjoint-p t15 c66)))
  (Assert (not (ase-interval-disjoint-p t133 c55)))
  (Assert (not (ase-interval-disjoint-p c55 t133)))
  (Assert (not (ase-interval-disjoint-p t4 c11 c22 c55 c66)))
  (Assert (not (ase-interval-disjoint-p c11 t4 c22 c55 c66)))
  (Assert (not (ase-interval-disjoint-p c11 c22 t4)))

  ;; boundaries
  (Assert (ase-interval-equal-p
	   (ase-interval-boundary c11)
	   ;; #<ase:interval-union [0] x [0 1] u [1] x [0 1] u
	   ;;    [0 1] x [0] u [0 1] x [1]>
	   (ase-interval-union
	    (ase-cartesian* id i1)
	    (ase-cartesian* ia i1)
	    (ase-cartesian* i1 id)
	    (ase-cartesian* i1 ia))))
  (Assert (ase-interval-equal-p
	   (ase-interval-boundary cdeg)
	   ;; #<ase:interval-union [1] x [0] u [1] x [1]>
	   (ase-interval-union
	    (ase-cartesian* ia id)
	    (ase-cartesian* ia ia))))
  (Assert (ase-interval-empty-p
	   (ase-interval-boundary caa)))
  (Assert (ase-interval-equal-p
	   (ase-interval-boundary t1)
	   ;; #<ase:interval-union
	   ;;   [0] x [0 1] u [1] x [0 1] u [0 1] x [0] u [0 1] x [1] u
	   ;;   [1] x [1 2] u [2] x [1 2] u [1 2] x [1] u [1 2] x [2]>
	   (ase-interval-union
	    (ase-cartesian* id i1)
	    (ase-cartesian* ia i1)
	    (ase-cartesian* i1 id)
	    (ase-cartesian* i1 ia)
	    (ase-cartesian* ia i2)
	    (ase-cartesian* ie i2)
	    (ase-cartesian* i2 ia)
	    (ase-cartesian* i2 ie))))
  (Assert (ase-interval-equal-p
	   (ase-interval-boundary cHO)
	   ;; #<ase:interval-union [0] x [-1 1) u [2] x [-1 1) u
	   ;;    (0 2] x [-1] u (0 2] x [1]>
	   (ase-interval-union
	    (ase-cartesian* id (ase-interval -1 1 nil t))
	    (ase-cartesian* ie (ase-interval -1 1 nil t))
	    (ase-cartesian* (ase-interval 0 2 t) (ase-interval -1 -1))
	    (ase-cartesian* (ase-interval 0 2 t) (ase-interval 1 1)))))
  (Assert (ase-interval-equal-p
	   (ase-interval-boundary (ase-interval-boundary c11))
	   ;; #<ase:interval-union [0] x [0] u [0] x [1] u
	   ;;   [1] x [0] u [1] x [1]>
	   (ase-interval-union
	    (ase-cartesian* id id)
	    (ase-cartesian* id ia)
	    (ase-cartesian* ia id)
	    (ase-cartesian* ia ia))))
  (Assert (ase-interval-empty-p
	   (ase-interval-boundary
	    (ase-interval-union (ase-interval 0) ia))))

  ;; closures
  (Assert (ase-interval-equal-p
	   (ase-interval-closure c55) c11))
  (Assert (ase-interval-equal-p
	   (ase-interval-closure c11) c11))
  (Assert (ase-interval-equal-p
	   (ase-interval-closure t5566) t1))

  ;; interiors
  (Assert (ase-interval-equal-p
	   (ase-interval-interior c55) c55))
  (Assert (ase-interval-equal-p
	   (ase-interval-interior c11) c55))
  (Assert (ase-interval-equal-p
	   (ase-interval-interior t1) t5566))


  ;; measures
  (Assert (= (ase-interval-lebesgue-measure c11) 1))
  (Assert (= (ase-interval-lebesgue-measure c12) 1))
  (Assert (= (ase-interval-lebesgue-measure c555) 1))
  (Assert (= (ase-interval-lebesgue-measure c11) 1))
  (Assert (= (ase-interval-lebesgue-measure c0303) 9))
  (Assert (= (ase-interval-lebesgue-measure c030303) 27))
  (Assert (= (ase-interval-lebesgue-measure cHO) 4))
  (Assert (= (ase-interval-lebesgue-measure cdeg) 0))
  (Assert (= (ase-interval-lebesgue-measure caa) 0))
  (Assert (= (ase-interval-lebesgue-measure t1) 2))
  (Assert (= (ase-interval-lebesgue-measure t133) 3))
  (Assert (= (ase-interval-lebesgue-measure t5) 2))
  (Assert (= (ase-interval-lebesgue-measure t5566) 2))
  (Assert (= (ase-interval-lebesgue-measure t555777) 2))
  (Assert (= (ase-interval-lebesgue-measure t555788) 5))
  (Assert (= (ase-interval-lebesgue-measure ttt12) 2))
  (Assert (= (ase-interval-rational-measure c11) 4))
  (Assert (= (ase-interval-rational-measure c12) 4))
  (Assert (= (ase-interval-rational-measure c555) 0))
  (Assert (= (ase-interval-rational-measure c0303) 16))
  (Assert (= (ase-interval-rational-measure c030303) 64))
  (Assert (= (ase-interval-rational-measure cHO) 4))
  (Assert (= (ase-interval-rational-measure cdeg) 2))
  (Assert (= (ase-interval-rational-measure caa) 1))
  (Assert (= (ase-interval-rational-measure t1) 8))
  (Assert (= (ase-interval-rational-measure t133) 12))
  (Assert (= (ase-interval-rational-measure t5) 8))
  (Assert (= (ase-interval-rational-measure t5566) 0))
  (Assert (= (ase-interval-rational-measure t555777) 0))
  (Assert (= (ase-interval-rational-measure t555788) 0))
  (Assert (= (ase-interval-rational-measure ttt12) 3))


  ;; intersections
  (Assert (ase-interval-equal-p
	   (ase-interval-intersection
	    c11 (ase-cartesian*
		 (ase-interval 0.5 0.75) (ase-interval -1 2)))
	   (ase-cartesian*
	    (ase-interval 0.5 0.75) i1)))
  (Assert (ase-interval-equal-p
	   (ase-interval-intersection t1 c11) c11))
  (Assert (ase-interval-equal-p
	   (ase-interval-intersection t1 t2)
	   ;; #<ase:interval-union [0 1] x [0 1] u [1] x [1 2]>
	   (ase-interval-union
	    c11 (ase-cartesian* ia i2))))
  (Assert (ase-interval-equal-p
	   (ase-interval-intersection t1 t5)
	   ;; [0 2] x [1] u [1] x [0 2]
	   (ase-interval-union
	    (ase-cartesian* (ase-interval 0 2) (ase-interval 1))
	    (ase-cartesian* (ase-interval 1) (ase-interval 0 2)))))

  ;; differences
  (Assert (ase-interval-equal-p
	   (ase-interval-difference chuge cst1)
	   ;;#<ase:interval-union [0 2) x [1 2] u (3 10] x [1 2] u
	   ;;    [0 10] x [0 1) u [0 10] x (2 10]>
	   (ase-interval-union
	    (ase-cartesian* (ase-interval 0 2 nil t) i2)
	    (ase-cartesian* (ase-interval 3 10 t) i2)
	    (ase-cartesian* huge (ase-interval 0 1 nil t))
	    (ase-cartesian* huge (ase-interval 2 10 t)))))
  (Assert (ase-interval-equal-p
	   (ase-interval-difference cst2 cst1)
	   ;;#<ase:interval-union [0 2) x [1 2] u [0 3] x [0 1) u
	   ;;    [0 3] x (2 3]>
	   (ase-interval-union
	    (ase-cartesian* (ase-interval 0 2 nil t) i2)
	    (ase-cartesian* i123 (ase-interval 0 1 nil t))
	    (ase-cartesian* i123 (ase-interval 2 3 t)))))
  (Assert (ase-interval-equal-p
	   (ase-interval-difference cst2 cst3)
	   ;;#<ase:interval-union (1 3] x [1 2] u [0 3] x [0 1) u
	   ;;    [0 3] x (2 3]>
	   (ase-interval-union
	    (ase-cartesian* (ase-interval 1 3 t) i2)
	    (ase-cartesian* i123 (ase-interval 0 1 nil t))
	    (ase-cartesian* i123 (ase-interval 2 3 t)))))
  (Assert (ase-interval-equal-p
	   (ase-interval-difference cst4 cst1)
	   ;; #<ase:interval-union [0 2) x [1 2] u [0 2] x [0 1)>
	   (ase-interval-union
	    (ase-cartesian* (ase-interval 0 2 nil t) i2)
	    (ase-cartesian* i8 (ase-interval 0 1 nil t)))))
  (Assert (ase-interval-equal-p
	   (ase-interval-difference cst2 cst5)
	   ;;#<ase:interval-union [0 2] x [1 2] u [0 3] x [0 1) u
	   ;;     [0 3] x (2 3]>
	   (ase-interval-union
	    (ase-cartesian* i8 i2)
	    (ase-cartesian* i123 (ase-interval 0 1 nil t))
	    (ase-cartesian* i123 (ase-interval 2 3 t)))))
  (Assert (ase-interval-equal-p
	   (ase-interval-difference cst2 cst6)
	   ;;#<ase:interval-union [0 2] x (-1 1) u [0 3] x [1 3]>
	   (ase-interval-union
	    (ase-cartesian* i8 (ase-interval -1 1 t t))
	    (ase-cartesian* i123 (ase-interval 1 3)))))

  ;;(+ i1 1)
  ;;(+ 2 i1)
  ;;(+ 2.0 i1)
  )
;; ase-interval-tests.el ends here
