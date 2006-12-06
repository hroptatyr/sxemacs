;;;  skiplist-tests.el -- Regression Tests for skiplists
;; Copyright (C) 2006 Sebastian Freundt
;;
;; Author: Sebastian Freundt <hroptatyr@sxemacs.org>
;; Keywords: tests
;;
;; This file is part of SXEmacs.
;; 
;; SXEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.
;; 
;; SXEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with SXEmacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Synched up with: Not in FSF.
;;
;;; Commentary:
;;
;; See test-harness.el for instructions on how to run these tests.

(eval-when-compile
  (condition-case nil
      (require 'test-harness)
    (file-error
     (push "." load-path)
     (when (and (boundp 'load-file-name) (stringp load-file-name))
       (push (file-name-directory load-file-name) load-path))
     (require 'test-harness))))


;; create some skiplists
(let ((sl1 (make-skiplist))
      (sl2 (make-skiplist))
      (sl3)
      (sl4))

  (eval `(Assert (skiplistp ,sl1)))
  (eval `(Assert (skiplistp ,sl2)))

  (eval `(Assert (eq (skiplist-size ,sl1) 0)))
  (eval `(Assert (skiplist-empty-p ,sl2)))
  (eval `(Assert (eq (skiplist-to-alist ,sl1) '())))
  (eval `(Assert (skiplistp ,sl2)))

  ;; add some elements
  (put-skiplist sl1 12 'number)
  (put-skiplist sl1 'foo 'bar)
  (put-skiplist sl1 "string" t)

  ;; test consistency
  (eval `(Assert (skiplist-owns-p ,sl1 12)))
  (eval `(Assert (skiplist-owns-p ,sl1 'foo)))
  (eval `(Assert (skiplist-owns-p ,sl1 "string")))
  (eval `(Assert (not (skiplist-owns-p ,sl1 13))))
  (eval `(Assert (not (skiplist-owns-p ,sl1 'bar))))
  (eval `(Assert (not (skiplist-owns-p ,sl1 t))))
  (eval `(Assert (eq (skiplist-size ,sl1) 3)))

  (eval `(Assert (eq (get-skiplist ,sl1 12) 'number)))
  (eval `(Assert (eq (get-skiplist ,sl1 'foo) 'bar)))
  (eval `(Assert (eq (get-skiplist ,sl1 "string") t)))

  ;; remove some elements
  (remove-skiplist sl1 12)
  (remove-skiplist sl1 "string")

  ;; test consistency
  (eval `(Assert (not (skiplist-owns-p ,sl1 12))))
  (eval `(Assert (not (skiplist-owns-p ,sl1 "string"))))
  (eval `(Assert (eq (skiplist-size ,sl1) 1)))

  ;; re-add the elements
  (put-skiplist sl1 12 'number)
  (put-skiplist sl1 "string" t)

  ;; replace these elements (by adding the same keys)
  (put-skiplist sl1 12 'has-been-replaced)
  (put-skiplist sl1 'foo 'has-been-replaced)
  (put-skiplist sl1 "string" 'has-been-replaced)

  ;; test consistency
  (eval `(Assert (eq (get-skiplist ,sl1 12) 'has-been-replaced)))
  (eval `(Assert (eq (get-skiplist ,sl1 'foo) 'has-been-replaced)))
  (eval `(Assert (eq (get-skiplist ,sl1 "string") 'has-been-replaced)))


  ;; test cloning operations
  (setq sl3 (copy-skiplist sl1))
  (eval `(Assert (skiplistp ,sl3)))
  (eval `(Assert (= (skiplist-size ,sl1) (skiplist-size ,sl3))))

  (map-skiplist
   #'(lambda (key val)
       (Assert (skiplist-owns-p sl3 key))
       (Assert (equal (get-skiplist sl3 key) val)))
   sl1)
)


;;; skiplist-tests.el ends here
