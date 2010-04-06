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
     (locate-module "ase-resclass")
     (require 'ase-resclass))

(when (featurep 'ase-resclass)
  (let ((r1 (make-residue-class-ring 23))
	(r1p Z/23Z)
	(r2 (make-residue-class-ring 81))
	(r2p Z/81Z))

    ;; test the read syntax
    ;;(eval `(Assert (eq ,r1 ,r1p)))   ;; uh oh

    (eval `(Assert (residue-class-p (+ (make-residue-class 1 ,r1)
				       (make-residue-class 4 ,r1)))))
    (eval `(Assert (residue-class-p (+ (make-residue-class 1 ,r1) 4+23Z))))
    (eval `(Assert (residue-class-p (+ (make-residue-class 1 ,r2) 4+81Z))))
    (eval `(Assert (= 2+23Z (make-residue-class 2 ,r1))))

    (Assert (not (comparablep 2+21Z)))
    (Assert (not (comparablep 2+23Z)))
    (Assert (not (comparablep 0+23Z)))
    (Check-Error relation-error (< 2+23Z 3+23Z))
    (Check-Error relation-error (< 2+23Z 2+21Z))
    (Check-Error relation-error (> 2+23Z 3+23Z))
    (Check-Error relation-error (> 2+23Z 2+21Z))
    (Check-Error relation-error (<= 2+23Z 2+23Z))
    (Check-Error relation-error (<= 2+23Z 3+23Z))
    (Check-Error relation-error (<= 2+23Z 2+21Z))
    (Check-Error relation-error (<= 2+23Z 3+21Z))
    (Check-Error relation-error (>= 2+23Z 2+23Z))
    (Check-Error relation-error (>= 2+23Z 3+23Z))
    (Check-Error relation-error (>= 2+23Z 2+21Z))
    (Check-Error relation-error (>= 2+23Z 3+21Z))
    (Assert (= 2+23Z 2+23Z))
    (Assert (not (/= 2+23Z 2+23Z)))
    (Assert (/= 2+23Z 3+23Z))
    (Assert (not (= 2+23Z 3+23Z)))
    (Assert (= 2+23Z 25+23Z))
    (Assert (= -2+23Z 21+23Z))
    (Check-Error domain-error (= 2+23Z 2+22Z))
    (Check-Error domain-error (= -2+23Z -2+2Z))
    (Check-Error domain-error (/= 2+23Z 2+22Z))
    (Check-Error domain-error (/= -2+23Z -2+2Z))

    (Assert (zerop 2+2Z))
    (Assert (onep 3+2Z))

    (unless (boundp 'Z/0Z)
      (Check-Error (void-variable Z/0Z) Z/0Z))
    (unless (boundp '0+0Z)
      (Check-Error (void-variable 0+0Z) 0+0Z))
    )
  )
;; ase-resclass-tests.el ends here
