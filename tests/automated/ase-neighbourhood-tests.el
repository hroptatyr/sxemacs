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
  (setq n1 (ase-neighbourhood 0 1))
  (setq n2 (ase-neighbourhood 2 1))
  (when (featurep 'bigq)
    (setq n4 (ase-neighbourhood 0 1/3)))
  (setq n5 (ase-neighbourhood 0 0.0001))
  (setq n6 (ase-neighbourhood (ase-cartesian* 0 0) 1))

  (Assert (ase-neighbourhood-contains-p n1 0))
  (Assert (ase-neighbourhood-contains-p n1 0.5))
  (Assert (ase-neighbourhood-contains-p n1 -0.5))
  (Assert (not (ase-neighbourhood-contains-p n1 1)))
  (Assert (not (ase-neighbourhood-contains-p n1 -1)))
  (Assert (ase-neighbourhood-contains-p n1 (// (sqrt 2) 2)))

  (Assert (ase-neighbourhood-contains-p n2 2))
  (Assert (ase-neighbourhood-contains-p n2 (sqrt 2)))
  (Assert (ase-neighbourhood-contains-p n2 (sqrt 3)))
  (Assert (ase-neighbourhood-contains-p n2 (sqrt 4)))

  (when (featurep 'bigq)
    (Assert (ase-neighbourhood-contains-p n4 0))
    (Assert (ase-neighbourhood-contains-p n4 1/4))
    (Assert (ase-neighbourhood-contains-p n4 -1/5))
    (Assert (ase-neighbourhood-contains-p n4 1/6))
    (Assert (ase-neighbourhood-contains-p n4 -0.1)))

  (Assert (ase-neighbourhood-contains-p n5 0))
  (Assert (ase-neighbourhood-contains-p n5 0.00001))
  (Assert (ase-neighbourhood-contains-p n5 -0.000001))
  (Assert (ase-neighbourhood-contains-p n5 1e-10))
  (Assert (ase-neighbourhood-contains-p n5 -1e-20))
  (Assert (not (ase-neighbourhood-contains-p n5 +infinity)))

  (Assert (not (ase-neighbourhood-contains-p n1 n2)))
  (Assert (not (ase-neighbourhood-contains-p n2 n1)))
  (Assert (ase-neighbourhood-contains-p n1 n1))
  (Assert (ase-neighbourhood-contains-p n1 n5))
  (when (featurep 'bigq)
    (Assert (ase-neighbourhood-contains-p n1 n4))
    (Assert (ase-neighbourhood-contains-p n4 n4))
    (Assert (ase-neighbourhood-contains-p n4 n5))
    (Assert (not (ase-neighbourhood-contains-p n5 n4))))

  (Assert (ase-neighbourhood-< n1 12))
  (Assert (not (ase-neighbourhood-< n1 -12)))
  (Assert (ase-neighbourhood-< -12 n1))
  (Assert (not (ase-neighbourhood-< 12 n1)))
  (Assert (ase-neighbourhood-< -1 n1))
  (Assert (ase-neighbourhood-< n1 1))
  (Assert (not (ase-neighbourhood-< n1 0)))
  (Assert (not (ase-neighbourhood-> n1 0)))

  ;; test the problematic cases
  ;; you know: (= (- 0.2 0.1) 0.1) => t
  ;; while: (= (- 0.9 0.8) 0.1) => nil
  (setq n6 (ase-neighbourhood 0.1 0.0001))
  (setq n7 (ase-neighbourhood 0.1 1e-14))
  (Assert (ase-neighbourhood-contains-p n6 (- 0.2 0.1)))
  (Assert (ase-neighbourhood-contains-p n6 (- 0.9 0.8)))
  (Assert (ase-neighbourhood-contains-p n6 (- -1.1 -1.2)))
  (Assert (ase-neighbourhood-contains-p n7 (- 0.2 0.1)))
  (Assert (ase-neighbourhood-contains-p n7 (- 0.9 0.8)))
  (Assert (ase-neighbourhood-contains-p n7 (- -1.1 -1.2)))

  (setq n2 (ase-neighbourhood 2 1))
  (when (featurep 'bigq)
    (setq n4 (ase-neighbourhood 0 1/3)))
  (setq n5 (ase-neighbourhood 0 0.0001))

  ;; testing measures
  (Assert (= (ase-neighbourhood-lebesgue-measure n1) 2))
  (Assert (= (ase-neighbourhood-lebesgue-measure n2) 2))
  (when (featurep 'bigq)
    (Assert (= (ase-neighbourhood-lebesgue-measure n4) 2/3)))
  (Assert (= (ase-neighbourhood-lebesgue-measure n5) 0.0002))
  (Assert (= (ase-neighbourhood-rational-measure n1) 1))
  (Assert (= (ase-neighbourhood-rational-measure n2) 1))
  (when (featurep 'bigq)
    (Assert (= (ase-neighbourhood-rational-measure n4) 1)))
  (Assert (= (ase-neighbourhood-rational-measure n5) 1))

  )
