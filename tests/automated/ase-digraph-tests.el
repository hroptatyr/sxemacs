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
     (locate-module "ase-digraph")
     (require 'ase-digraph))

(when (featurep 'ase-digraph)
  ;; create a digraph
  (Assert (setq g (ase-digraph)))
  ;; add 3 nodes
  (Assert (ase-digraph-add-node g 'A))
  (Assert (ase-digraph-add-node g 'B))
  (Assert (ase-digraph-add-node g 'C))
  ;; A->B
  (Assert (ase-digraph-add-edge g 'A 'B))
  ;(Assert (ase-digraph-has-edge-p g 'A 'B))
  ;; kick A->B
  (Assert (ase-digraph-remove-edge g 'A 'B))
  ;(Assert (not (ase-digraph-has-edge-p g 'A 'B)))
  ;; B->C
  (Assert (ase-digraph-add-edge g 'B 'C))
  ;(Assert (ase-digraph-has-edge-p g 'B 'C))
  ;; C->A
  (Assert (ase-digraph-add-edge g 'C 'A))
  ;(Assert (ase-digraph-has-edge-p g 'C 'A))
  ;; make sure there are no other edges in the graph now
  ;(Assert (not (ase-digraph-has-edge-p g 'A 'A)))
  ;(Assert (not (ase-digraph-has-edge-p g 'A 'B)))
  ;(Assert (not (ase-digraph-has-edge-p g 'A 'C)))
  ;(Assert (not (ase-digraph-has-edge-p g 'B 'A)))
  ;(Assert (not (ase-digraph-has-edge-p g 'B 'B)))
  ;(Assert (not (ase-digraph-has-edge-p g 'C 'B)))
  ;(Assert (not (ase-digraph-has-edge-p g 'C 'C)))

  (Check-Error-Message (error) "no such nodes" (ase-digraph-add-edge g 'D 'A))
  ;(Assert (not (ase-digraph-has-edge-p g 'D 'A)))

  (Check-Error-Message (error) "no such nodes" (ase-digraph-add-edge g 'A 'D))
  ;(Assert (not (ase-digraph-has-edge-p g 'A 'D)))
  )

;; ase-digraph-tests.el ends here
