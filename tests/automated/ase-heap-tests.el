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
(and (featurep 'modules)
     (locate-module "ase-heap")
     (require 'ase-heap))

(when (featurep 'ase-heap)
  (defun test-heaps (kind)
    (setq h (ase-heap :kind kind))

    (ase-add-heap h 2)
    (ase-add-heap h 6)
    (ase-add-heap h 4)
    (ase-add-heap h 8)
    (ase-add-heap h 3)

    ;;(ase-heap-to-list h)
    ;;(ase-heap-to-list* h)

    (Assert (= (ase-heap-size h) 5))
    (ase-add-heap h 2)
    (Assert (= (ase-heap-size h) 6))

    ;; pop elements off the heap slowly
    (Assert (= (ase-heap-top h) 8))
    (Assert (= (ase-heap-top-rank h) 8))
    (Assert (= (ase-pop-heap h) 8))
    (Assert (= (ase-heap-size h) 5))

    (Assert (= (ase-heap-top h) 6))
    (Assert (= (ase-heap-top-rank h) 6))
    (Assert (= (ase-pop-heap h) 6))
    (Assert (= (ase-heap-size h) 4))

    (Assert (= (ase-heap-top h) 4))
    (Assert (= (ase-heap-top-rank h) 4))
    (Assert (= (ase-pop-heap h) 4))
    (Assert (= (ase-heap-size h) 3))

    (Assert (= (ase-heap-top h) 3))
    (Assert (= (ase-heap-top-rank h) 3))
    (Assert (= (ase-pop-heap h) 3))
    (Assert (= (ase-heap-size h) 2))

    (Assert (= (ase-heap-top h) 2))
    (Assert (= (ase-heap-top-rank h) 2))
    (Assert (= (ase-pop-heap h) 2))
    (Assert (= (ase-heap-size h) 1))

    (Assert (= (ase-heap-top h) 2))
    (Assert (= (ase-heap-top-rank h) 2))
    (Assert (= (ase-pop-heap h) 2))
    (Assert (= (ase-heap-size h) 0))

    (Assert (null (ase-heap-top h)))
    (Assert (null (ase-heap-top-rank h)))
    (Assert (null (ase-pop-heap h)))
    (Assert (= (ase-heap-size h) 0))

    (Assert (null (ase-heap-top h)))
    (Assert (null (ase-heap-top-rank h)))
    (Assert (null (ase-pop-heap h)))
    (Assert (= (ase-heap-size h) 0))

    (let* ((l '(8 7 6 5 4 3 2 1))
	   (k '(1 2 3 4 5 6 7 8)))
      ;; feed l
      (mapc-internal #'(lambda (e) (ase-add-heap h e)) l)
      (Assert (= (ase-heap-size h) 8))
      (Assert-Equal (ase-heap-to-list h) l)
      (Assert (= (ase-heap-size h) 8))
      (Assert-Equal (ase-heap-to-list* h) l)
      (Assert (= (ase-heap-size h) 0))
      ;; feed k
      (mapc-internal #'(lambda (e) (ase-add-heap h e)) k)
      (Assert (= (ase-heap-size h) 8))
      (Assert-Equal (ase-heap-to-list h) l)
      (Assert (= (ase-heap-size h) 8))
      (Assert-Equal (ase-heap-to-list* h) l)
      (Assert (= (ase-heap-size h) 0)))

    (let* ((l '(8 7 6 5 4 3 2 1))
	   (k '(1 2 3 4 5 6 7 8))
	   (v [8 7 6 5 4 3 2 1]))
      ;; feed l
      (mapc-internal #'(lambda (e) (ase-add-heap h e)) l)
      (Assert (= (ase-heap-size h) 8))
      (Assert-Equal (ase-heap-to-vector h) v)
      (Assert (= (ase-heap-size h) 8))
      (Assert-Equal (ase-heap-to-vector* h) v)
      (Assert (= (ase-heap-size h) 0))
      ;; feed k
      (mapc-internal #'(lambda (e) (ase-add-heap h e)) k)
      (Assert (= (ase-heap-size h) 8))
      (Assert-Equal (ase-heap-to-vector h) v)
      (Assert (= (ase-heap-size h) 8))
      (Assert-Equal (ase-heap-to-vector* h) v)
      (Assert (= (ase-heap-size h) 0)))

    (let* ((l '(8 7 6 5 4 3 2 1))
	   (k '(1 2 3 4 5 6 7 8))
	   (d (dllist 8 7 6 5 4 3 2 1)))
      ;; feed l
      (mapc-internal #'(lambda (e) (ase-add-heap h e)) l)
      (Assert (= (ase-heap-size h) 8))
      (Assert-Equal (ase-heap-to-dllist h) d)
      (Assert (= (ase-heap-size h) 8))
      (Assert-Equal (ase-heap-to-dllist* h) d)
      (Assert (= (ase-heap-size h) 0))
      ;; feed k
      (mapc-internal #'(lambda (e) (ase-add-heap h e)) k)
      (Assert (= (ase-heap-size h) 8))
      (Assert-Equal (ase-heap-to-dllist h) d)
      (Assert (= (ase-heap-size h) 8))
      (Assert-Equal (ase-heap-to-dllist* h) d)
      (Assert (= (ase-heap-size h) 0))))

  ;;; testing coloured heaps
  (defun test-coloured-heaps (kind)
    (setq h (ase-heap :coloured t :kind kind))

    (ase-add-heap h 2 'two)
    (ase-add-heap h 6 'six)
    (ase-add-heap h 4 'four)
    (ase-add-heap h 8 'eight)
    (ase-add-heap h 3 'three)

    (Assert (= (ase-heap-size h) 5))
    (ase-add-heap h 2)
    (Assert (= (ase-heap-size h) 6))

    ;; pop elements off the heap slowly
    (Assert (eq (ase-heap-top h) 'eight))
    (Assert (= (ase-heap-top-rank h) 8))
    (Assert (eq (ase-pop-heap h) 'eight))
    (Assert (= (ase-heap-size h) 5))

    (Assert (eq (ase-heap-top h) 'six))
    (Assert (= (ase-heap-top-rank h) 6))
    (Assert (eq (ase-pop-heap h) 'six))
    (Assert (= (ase-heap-size h) 4))

    (Assert (eq (ase-heap-top h) 'four))
    (Assert (= (ase-heap-top-rank h) 4))
    (Assert (eq (ase-pop-heap h) 'four))
    (Assert (= (ase-heap-size h) 3))

    (Assert (eq (ase-heap-top h) 'three))
    (Assert (= (ase-heap-top-rank h) 3))
    (Assert (eq (ase-pop-heap h) 'three))
    (Assert (= (ase-heap-size h) 2))

    (ase-pop-heap h)
    (Assert (= (ase-heap-size h) 1))

    (ase-pop-heap h)
    (Assert (= (ase-heap-size h) 0))

    (Assert (null (ase-heap-top h)))
    (Assert (null (ase-heap-top-rank h)))
    (Assert (null (ase-pop-heap h)))
    (Assert (= (ase-heap-size h) 0))

    (Assert (null (ase-heap-top h)))
    (Assert (null (ase-heap-top-rank h)))
    (Assert (null (ase-pop-heap h)))
    (Assert (= (ase-heap-size h) 0)))

  (defun test-char-heaps (kind)
    (setq h (ase-heap :kind kind))
    (ase-add-heap h ?a)
    (ase-add-heap h ?b)
    (ase-add-heap h ?c)
    (ase-add-heap h ?d)
    (ase-add-heap h ?e)
    (ase-add-heap h ?f)
    (ase-add-heap h ?g)
    (ase-add-heap h ?h)
    (ase-add-heap h ?i)
    (ase-add-heap h ?j)
    (ase-add-heap h ?k)
    (ase-add-heap h ?l)
    (ase-add-heap h ?m)
    (ase-add-heap h ?n)
    (ase-add-heap h ?o)
    (ase-add-heap h ?p)
    (ase-add-heap h ?q)
    (ase-add-heap h ?r)
    (ase-add-heap h ?s)
    (ase-add-heap h ?t)
    (ase-add-heap h ?u)
    (ase-add-heap h ?v)
    (ase-add-heap h ?w)
    (ase-add-heap h ?x)
    (ase-add-heap h ?y)
    (ase-add-heap h ?z)

    (Assert
     (equal (ase-heap-to-vector* h)
	    [?z ?y ?x ?w ?v ?u ?t ?s ?r ?q ?p ?o ?n ?m
		?l ?k ?j ?i ?h ?g ?f ?e ?d ?c ?b ?a])))

  (defun test-mixed-heaps (kind)
    (setq h (ase-heap :kind kind))
    ;;(ase-add-heap h ?a)
    (ase-add-heap h 3)
    (ase-add-heap h 7.5)
    (when (featurep 'bigz)
      (ase-add-heap h (factorial 200)))
    (when (featurep 'bigq)
      (ase-add-heap h 1984319/5))
    (when (featurep 'bigfr)
      (ase-add-heap h euler-mascheroni)
      (ase-add-heap h euler))
    (when (featurep 'indefinite)
      (ase-add-heap h +infinity)
      (ase-add-heap h -infinity))

    (when (featurep 'indefinite)
      (Assert (eq (ase-pop-heap h) +infinity)))
    (when (featurep 'bigz)
      (Assert (= (ase-pop-heap h) (factorial 200))))
    (when (featurep 'bigq)
      (Assert (= (ase-pop-heap h) 1984319/5)))
    ;;(Assert (= (ase-pop-heap h) ?a))
    (Assert (= (ase-pop-heap h) 7.5))
    (Assert (= (ase-pop-heap h) 3))
    (when (featurep 'bigfr)
      (Assert (= (ase-pop-heap h) euler))
      (Assert (= (ase-pop-heap h) euler-mascheroni)))
    (when (featurep 'indefinite)
      (Assert (eq (ase-pop-heap h) -infinity))))

  ;; the actual test
  (mapcar #'test-heaps '(dynamic dense weak))
  (mapcar #'test-coloured-heaps '(dynamic dense weak))

  (mapcar #'test-char-heaps '(dynamic dense weak))
  (mapcar #'test-mixed-heaps '(dynamic dense weak))

  ;; hardcore test (only for weak heaps, no?)
  (setq h (ase-heap :kind 'weak))
  (dotimes (j 6)
    (dotimes (i (10^ j))
      (ase-add-heap h (random 80000)))
    (Assert (apply #'>= (ase-heap-to-list h)))
    (dotimes (i (10^ j))
      (ase-add-heap h (random 80000)))
    (Assert (apply #'>= (ase-heap-to-list* h))))


  ;; The cool thing about heaps is that you can add whatever you want, i.e. _no_
  ;; restrictions there.  However, when you add the second element to the heap it
  ;; has to be compared somehow.  Most domains, however, thwart you here since
  ;; they do not possess (or can't possess) a partial ordering.

  (defun test-non-posets (kind)
    (setq h (ase-heap :kind kind))
    ;; lisp symbols dont form a poset
    (Assert (ase-add-heap h 'foo))
    ;; adding another symbol will make SXE cry
    (Check-Error relation-error (ase-add-heap h 'bar))
    ;; even if we add a poset object
    (Check-Error relation-error (ase-add-heap h 0))
    ;; however we can pop the original element we stored there
    (Assert (eq (ase-pop-heap h) 'foo))

    ;; same for other non-posets
    (when (featurep 'bigg)
      (Assert (ase-add-heap h 1+i))
      (Check-Error relation-error (ase-add-heap h -1+i))
      (Check-Error relation-error (ase-add-heap h 1))
      (Assert (= (ase-pop-heap h) 1+i))))

  ;;(mapcar #'test-non-posets '(dynamic dense weak))
  )
;; ase-heap-tests.el ends here
