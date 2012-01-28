;;;  ase-profs.el -- Benchmarks for ASE
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
;; See benchmark.el for instructions on how to run these tests.

(condition-case nil
    (require 'benchmark)
  (file-error
   (push "." load-path)
   (when (and (boundp 'load-file-name) (stringp load-file-name))
     (push (expand-file-name "../tests/benchmark"
			     (file-name-directory load-file-name))
	   load-path))
   (require 'benchmark)))

(require 'ase-heap)


(defun heap-fill (N)
  "heap filling"
  (while (nonnegativep N)
    (ase-add-heap h (random most-positive-fixnum))
    (setq N (1- N))))

(defun heap-dump-d (N)
  "heap -> dllist"
  (ase-heap-to-dllist h))

(defun heap-dump-v (N)
  "heap -> vector"
  (ase-heap-to-vector h))

(defun heap-dump-l (N)
  "heap -> list"
  (ase-heap-to-list h))

(defun heap-dump-d* (N)
  "heap -> dllist*"
  (ase-heap-to-dllist* h))

(defun heap-dump-v* (N)
  "heap -> vector*"
  (ase-heap-to-vector* h))

(defun heap-dump-l* (N)
  "heap -> list*"
  (ase-heap-to-list* h))


(defun yheap-constr (N)
  "dyna heap construction"
  (setq h (ase-heap :kind 'dynamic)))

(defun yheap-destr (N)
  "dyna heap cleanup"
  (setq h nil)
  (garbage-collect))

(defun dheap-constr (N)
  "dense heap construction"
  (setq h (ase-heap :kind 'dense)))

(defun dheap-destr (N)
  "dense heap cleanup"
  (setq h nil)
  (garbage-collect))

(defun wheap-constr (N)
  "weak heap construction"
  (setq h (ase-heap :kind 'weak)))

(defun wheap-destr (N)
  "weak heap cleanup"
  (setq h nil)
  (garbage-collect))



;; now the actual test suites
(bm-estimate-time
 (list
  :test-funv [yheap-constr heap-fill heap-dump-d heap-dump-d*]
  :test-range '(10 . 100000)
  :plot-file "benchmark-yheap.plot"))

(bm-estimate-time
 (list
  :test-funv [dheap-constr heap-fill heap-dump-d heap-dump-d*]
  :test-range '(10 . 100000)
  :plot-file "benchmark-dheap.plot"))

(bm-estimate-time
 (list
  :test-funv [wheap-constr heap-fill heap-dump-d heap-dump-d*]
  :test-range '(10 . 100000)
  :plot-file "benchmark-wheap.plot"))


;; ase-heap-profs.el ends here
