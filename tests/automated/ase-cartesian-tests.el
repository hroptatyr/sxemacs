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
  ;; create some intervals first
  (setq i1 (ase-interval 0 1))
  (setq i2 (ase-interval 1 2))
  (setq i3 (ase-interval 2 3))

  (setq c12 (ase-cartesian i1 i2))
  (setq c123 (ase-cartesian i1 i2 i3))
  (setq c11 (ase-cartesian i1 i1)) ;; [0 1] x [0 1]
  (setq c12* (ase-cartesian* i1 i2))
  (setq c123* (ase-cartesian* i1 i2 i3))
  (setq c11* (ase-cartesian* i1 i1)) ;; [0 1] x [0 1]
  (setq cI1 (ase-cartesian 12 i1))
  (setq z00 (ase-cartesian 0 0))
  (setq z01 (ase-cartesian 0 1))
  (setq z00* (ase-cartesian* 0 0))
  (setq z01* (ase-cartesian* 0 1))

  (Assert (null (ase-cartesian)))
  (Assert (eq i1 (ase-cartesian i1)))
  (Check-Error interior-error (ase-cartesian* 12 i1))

  (Assert (ase-cartesianp c12))
  (Assert (ase-cartesianp c123))
  (Assert (ase-cartesianp c11))
  (Assert (ase-cartesian*p c12*))
  (Assert (ase-cartesian*p c123*))
  (Assert (ase-cartesian*p c11*))
  (Assert (ase-cartesianp cI1))
  (Assert (ase-cartesianp z00))
  (Assert (ase-cartesianp z01))
  (Assert (ase-cartesian*p z00*))
  (Assert (ase-cartesian*p z01*))

  (Assert (not (ase-cartesian*p c12)))
  (Assert (not (ase-cartesian*p c123)))
  (Assert (not (ase-cartesian*p c11)))
  (Assert (not (ase-cartesianp c12*)))
  (Assert (not (ase-cartesianp c123*)))
  (Assert (not (ase-cartesianp c11*)))
  (Assert (not (ase-cartesian*p cI1)))
  (Assert (not (ase-cartesian*p z00)))
  (Assert (not (ase-cartesian*p z01)))
  (Assert (not (ase-cartesianp z00*)))
  (Assert (not (ase-cartesianp z01*)))

  (Assert (eq (type-of i1) (ase-cartesian-ground-domain c12*)))
  (Assert (eq (type-of i2) (ase-cartesian-ground-domain c123*)))
  (Assert (eq (type-of 0) (ase-cartesian-ground-domain z00*)))
  (Assert (eq (type-of 1) (ase-cartesian-ground-domain z01*)))


  (setq c1 (ase-cartesian 1 2 3)
	c2 (ase-cartesian* 'a 'b 'c))

  (Assert (eq (ase-cartesian-projection c1 1) 1))
  (Assert (eq (ase-cartesian-projection c1 2) 2))
  (Assert (eq (ase-cartesian-projection c1 3) 3))
  (Assert (eq (ase-cartesian-projection c2 1) 'a))
  (Assert (eq (ase-cartesian-projection c2 2) 'b))
  (Assert (eq (ase-cartesian-projection c2 3) 'c))
  (Check-Error args-out-of-range (ase-cartesian-projection c1 0))
  (Check-Error args-out-of-range (ase-cartesian-projection c1 4))
  (Check-Error wrong-type-argument (ase-cartesian-projection c1 -1))
  (when (or (featurep 'fpfloat)
	    (featurep 'bigf)
	    (featurep 'bigfr))
    (Check-Error wrong-type-argument (ase-cartesian-projection c1 0.5)))
  (when (featurep 'bigq)
    (Check-Error wrong-type-argument (ase-cartesian-projection c1 1/2)))

  (Assert (= (ase-cartesian-projection
	      (ase-cartesian-embed c1 [2 3]) 1) 2))
  (Assert (= (ase-cartesian-projection
	      (ase-cartesian-embed c1 [2 3]) 2) 3))
  (Assert (eq (ase-cartesian-projection
	       (ase-cartesian-embed c2 [2 1 0 3]) 1) 'b))
  (Assert (eq (ase-cartesian-projection
	       (ase-cartesian-embed c2 [2 1 0 3]) 2) 'a))
  (Assert (eq (ase-cartesian-projection
	       (ase-cartesian-embed c2 [2 1 0 3]) 3) 0))
  (Assert (eq (ase-cartesian-projection
	       (ase-cartesian-embed c2 [2 1 0 3]) 4) 'c))
  (Assert (eq (ase-cartesian-projection
	       (ase-cartesian-embed c2 [2 1 0 3 3]) 5) 'c))

  )
;;; ase-cartesian-tests.el ends here
