;;;  cl-profs.el -- Benchmarks for CL
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

(require 'cl-loop)


(defun cl:loop-for (N)
  "C for-arith clause"
  (cl:loop for i below N))

(defun cl:loop-count (N)
  "C count clause"
  (cl:loop for i below N
	   count (primep i)))

(defun loop-for (N)
  "lisp for-arith clause"
  (loop for i below N))

(defun loop-count (N)
  "lisp for-arith"
  (loop for i below N
    count (primep i)))


;; now the actual test suites
(bm-estimate-time
 (list
  :test-funv [cl:loop-for cl:loop-count]
  :test-range '(10 . 1000000)
  :plot-file "benchmark-C-loop.plot"))

(bm-estimate-time
 (list
  :test-funv [loop-for loop-count]
  :test-range '(10 . 1000000)
  :plot-file "benchmark-lisp-loop.plot"))

;; cl-loop-profs.el ends here
