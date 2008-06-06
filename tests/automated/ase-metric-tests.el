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
     (locate-module "ase-set")
     (require 'ase-set))

(when (featurep 'ase-set)
  (setq 2zero (ase-cartesian* 0 0)
	3zero (ase-cartesian* 0 0 0)
	2one (ase-cartesian* 1 0)
	3one (ase-cartesian* 1 0 0)
	2|11 (ase-cartesian* 1 1)
	2|1-1 (ase-cartesian* 1 -1)
	2|-11 (ase-cartesian* -1 1)
	2|-1-1 (ase-cartesian* -1 -1)
	3|111 (ase-cartesian* 1 1 1)
	3|-111 (ase-cartesian* -1 1 1)
	3|1-11 (ase-cartesian* 1 -1 1)
	3|11-1 (ase-cartesian* 1 1 -1)
	3|-11-1 (ase-cartesian* -1 1 -1)
	3|-1-11 (ase-cartesian* -1 -1 1)
	3|1-1-1 (ase-cartesian* 1 -1 -1)
	3|-1-1-1 (ase-cartesian* -1 -1 -1))

  ;; euclidean metric (standard metric in maths)
  (Assert (= (ase-metric-distance ase-euclidean-metric 3 5) 2))
  (Assert (= (ase-metric-distance ase-euclidean-metric 3 5.5) 2.5))
  (Check-Error embed-error (ase-metric-distance ase-euclidean-metric 3zero 2one))
  (Assert (= (ase-metric-distance ase-euclidean-metric 2zero 2|-11) (sqrt 2)))

  ;; supremum metric (default for intervals and neighbourhoods)
  (Assert (= (ase-metric-distance ase-supremum-metric 5 4) 1))
  (Assert (= (ase-metric-distance ase-supremum-metric 2zero 2|11) 1))
  (Assert (= (ase-metric-distance ase-supremum-metric 2|1-1 2zero) 1))
  (Assert (= (ase-metric-distance ase-supremum-metric 3|1-11 3zero) 1))
  (Assert (= (ase-metric-distance ase-supremum-metric 3|111 3|-1-1-1) 2))
  (Assert (= (ase-metric-distance ase-supremum-metric 3|111 3|11-1) 2))
  (Assert (= (ase-metric-distance ase-supremum-metric 3|11-1 3|11-1) 0))
  (Check-Error embed-error (ase-metric-distance ase-supremum-metric 3|111 2|11))

  ;; trivial metric
  (Assert (= (ase-metric-distance ase-trivial-metric 2zero 2|11) 1))
  (Assert (= (ase-metric-distance ase-trivial-metric 2zero 2zero) 0))
  (Assert (= (ase-metric-distance ase-trivial-metric 3|111 3|-1-1-1) 1))
  (Assert (= (ase-metric-distance ase-trivial-metric 3zero 3|-1-1-1) 1))

  ;; custom metrics
  (when (featurep 'bigfr)
    (setq R3m (ase-p-metric 3))
    (Assert (= (ase-metric-distance R3m 3zero 3|111) (cube-root 3)))
    (Assert (nonnegativep (ase-metric-distance R3m 3zero 3|-1-1-1)))
    (Assert (zerop (ase-metric-distance R3m 3|-1-1-1 3|-1-1-1))))

  (when (featurep 'bigq)
    (setq mym (ase-metric
	       #'(lambda (a b)
		   (if (= a b)
		       0
		     (// (+ a b) 2)))))

    (Assert (zerop (ase-metric-distance mym 0 0)))
    (Assert (= (ase-metric-distance mym 0 1) 0.5))
    (Check-Error metric-distance-error (ase-metric-distance mym -1 -2)))
  )
;; ase-metric-tests.el ends here
