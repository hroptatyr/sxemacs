;;;  map-tests.el -- Regression Tests for the map* functions
;; Copyright (C) 2006 Sebastian Freundt
;;
;; Author: Sebastian Freundt <hroptatyr@sxemacs.org>
;; Keywords: tests
;;
;; This file is part of SXEmacs.
;;
;; SXEmacs is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; SXEmacs is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
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

(defmacro Assert-set-equality (s1 s2)
  (let* ((m1 (if (arrayp s1) 'across 'in))
	 (m2 (if (arrayp s2) 'across 'in)))
    `(progn
       (Assert-Equal (type-of ,s1) (type-of ,s2))
       (Assert (= (length ,s1) (length ,s2)))
       (Assert
	(loop
	  for i ,m1 ,s1
	  always (loop
		   for j ,m2 ,s2
		   thereis (equal i j)))))))

(defmacro Assert-tup-equality (s1 s2)
  (let* ((m1 (if (arrayp s1) 'across 'in))
	 (m2 (if (arrayp s2) 'across 'in)))
    `(progn
       (Assert-Equal (type-of ,s1) (type-of ,s2))
       (Assert
	(loop
	  for i ,m1 ,s1
	  for j ,m2 ,s2
	  always (equal i j))))))


;; test trivial cases
(Assert
 (null (mapfam nil)))
(Assert
 (null (mapfam #'cons :arity 0 '(1 2) '(3 4))))
(Assert
 (null (mapfam nil :mode 'void '(1 2) '(3 4))))
(Assert
 (null (mapfam #'cons)))

;; test glues first
(Assert
 (equal
  (mapfam nil [1 2 3 4 5 6])
  '(1 2 3 4 5 6)))
(Assert
 (equal
  (mapfam nil :mode 'pntw [1 2 3 4 5 6])
  '(1 2 3 4 5 6)))
(Assert
 (equal
  (mapfam nil :mode 'pntw [1 2 3 4 5 6] :arity 2)
  '((1 2) (3 4) (5 6))))
(Assert
 (equal
  (mapfam nil :mode 'pntw [1 2 3 4 5 6] :arity 3)
  '((1 2 3) (4 5 6))))
;; same on vectors
(Assert
 (equal
  (mapfam nil :mode 'pntw :glue #'vector [1 2 3 4 5 6])
  '(1 2 3 4 5 6)))
(Assert
 (equal
  (mapfam nil :mode 'pntw :glue #'vector [1 2 3 4 5 6] :arity 2)
  '([1 2] [3 4] [5 6])))
(Assert
 (equal
  (mapfam nil :mode 'pntw :glue #'vector [1 2 3 4 5 6] :arity 3)
  '([1 2 3] [4 5 6])))
;; same on vectors/vectors
(Assert
 (equal
  (mapfam nil :mode 'pntw :result-type #'vector :glue #'vector [1 2 3 4 5 6])
  [1 2 3 4 5 6]))
(Assert
 (equal
  (mapfam nil :mode 'pntw :result-type #'vector :glue #'vector [1 2 3 4 5 6] :arity 2)
  [[1 2] [3 4] [5 6]]))
(Assert
 (equal
  (mapfam nil :mode 'pntw :result-type #'vector :glue #'vector [1 2 3 4 5 6] :arity 3)
  [[1 2 3] [4 5 6]]))
;; mismatching sizes
(Assert
 (equal
  (mapfam nil :mode 'pntw :arity 2 [1 2 3 4 5 6 7])
  '((1 2) (3 4) (5 6))))
(Assert
 (equal
  (mapfam nil :mode 'pntw :arity 3 [1 2 3 4 5 6 7])
  '((1 2 3) (4 5 6))))
(Assert
 (equal
  (mapfam nil :mode 'pntw :glue #'vector :arity 2 [1 2 3 4 5 6 7])
  '([1 2] [3 4] [5 6])))
(Assert
 (equal
  (mapfam nil :mode 'pntw :glue #'vector :arity 3 [1 2 3 4 5 6 7])
  '([1 2 3] [4 5 6])))
(Assert
 (equal
  (mapfam nil :mode 'pntw :result-type #'vector :glue #'vector :arity 2
	  [1 2 3 4 5 6 7])
  [[1 2] [3 4] [5 6]]))
(Assert
 (equal
  (mapfam nil :mode 'pntw :result-type #'vector :glue #'vector :arity 3
	  [1 2 3 4 5 6 7])
  [[1 2 3] [4 5 6]]))
;; larger glue than sequence
(Assert
 (equal
  (mapfam nil :mode 'pntw [1 2 3 4 5 6] :arity 8)
  nil))
(Assert
 (equal
  (mapfam nil :mode 'pntw [1 2 3 4 5 6] :glue #'vector :arity 8)
  nil))
(Assert
 (equal
  (mapfam nil :mode 'pntw [1 2 3 4 5 6] :result-type #'vector :arity 8)
  []))
(Assert
 (equal
  (mapfam nil :mode 'pntw [1 2 3 4 5 6] :result-type #'dllist :arity 8)
  (dllist)))
;; glueing dicts
;; prepare a hash-table
(setq test-ht (make-hash-table))
(puthash 12 'a test-ht)
(puthash 14 'b test-ht)
(puthash 13 'c test-ht)
(puthash 16 'e test-ht)
(puthash 15 'd test-ht)
;; ... and a skiplist
(setq test-sl (make-skiplist))
(put-skiplist test-sl 12 'a)
(put-skiplist test-sl 14 'b)
(put-skiplist test-sl 13 'c)
(put-skiplist test-sl 16 'e)
(put-skiplist test-sl 15 'd)
;; test the ht
(Assert-set-equality
 (mapfam nil test-ht)
 '((12 a) (13 c) (14 b) (16 e) (15 d)))
(Assert-set-equality
 (mapfam nil test-ht :arity '(1 1))
 '((12 a) (13 c) (14 b) (16 e) (15 d)))
(Assert-set-equality
 (mapfam nil test-ht :arity 1)
 '(12 13 14 16 15))
(Assert-set-equality
 (mapfam nil test-ht :mode 'keyw)
 '(12 13 14 16 15))
(Assert-set-equality
 (mapfam nil test-ht :glue #'vector)
 '([12 a] [13 c] [14 b] [16 e] [15 d]))
;; ... and the sl
(Assert-set-equality
 (mapfam nil test-sl)
 '((12 a) (13 c) (14 b) (16 e) (15 d)))
(Assert-set-equality
 (mapfam nil test-sl :arity '(1 1))
 '((12 a) (13 c) (14 b) (16 e) (15 d)))
(Assert-set-equality
 (mapfam nil test-sl :arity 1)
 '(12 13 14 16 15))
(Assert-set-equality
 (mapfam nil test-sl :mode 'keyw)
 '(12 13 14 16 15))
(Assert-set-equality
 (mapfam nil test-sl :glue #'vector)
 '([12 a] [13 c] [14 b] [16 e] [15 d]))

;; combinations
(Assert-set-equality
 (mapfam nil :mode 'comb [1 2 3 4] :arity 2)
 '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4)))
(Assert-tup-equality                    ;; we're sure that this is the order
 (mapfam nil :mode 'comb [1 2 3 4] :arity 2)
 '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4)))
(Assert-set-equality
 (mapfam nil :mode 'comb [1 2 3 4] :arity 2 :glue #'vector)
 '([1 2] [1 3] [1 4] [2 3] [2 4] [3 4]))
(Assert-tup-equality                    ;; we're sure that this is the order
 (mapfam nil :mode 'comb [1 2 3 4] :arity 2 :glue #'vector)
 '([1 2] [1 3] [1 4] [2 3] [2 4] [3 4]))
;; arity 3 tests
(Assert-set-equality
 (mapfam nil :mode 'comb [1 2 3 4] :arity 3)
 '((1 2 3) (1 2 4) (1 3 4) (2 3 4)))
(Assert-tup-equality                    ;; we're sure that this is the order
 (mapfam nil :mode 'comb [1 2 3 4] :arity 3)
 '((1 2 3) (1 2 4) (1 3 4) (2 3 4)))
(Assert-set-equality
 (mapfam nil :mode 'comb [1 2 3 4] :arity 3 :glue 'vector)
 '([1 2 3] [1 2 4] [1 3 4] [2 3 4]))
(Assert-tup-equality
 (mapfam nil :mode 'comb [1 2 3 4] :arity 3 :glue 'vector)
 '([1 2 3] [1 2 4] [1 3 4] [2 3 4]))
(Assert-set-equality
 (mapfam nil :mode 'comb [1 2 3 4] :arity 4)
 '((1 2 3 4)))
(Assert-tup-equality
 (mapfam nil :mode 'comb [1 2 3 4] :arity 4)
 '((1 2 3 4)))
(Assert-set-equality
 (mapfam nil :mode 'comb [1 2 3 4] :arity 4 :glue 'vector)
 '([1 2 3 4]))
(Assert-tup-equality
 (mapfam nil :mode 'comb [1 2 3 4] :arity 4 :glue 'vector)
 '([1 2 3 4]))
;; a bit larger now
(Assert-set-equality
 (mapfam nil :mode 'comb [1 2 3 4 5 6] :arity 2)
 '((1 2) (1 3) (1 4) (1 5) (1 6)
   (2 3) (2 4) (2 5) (2 6)
   (3 4) (3 5) (3 6)
   (4 5) (4 6)
   (5 6)))
(Assert-tup-equality
 (mapfam nil :mode 'comb [1 2 3 4 5 6] :arity 2)
 '((1 2) (1 3) (1 4) (1 5) (1 6)
   (2 3) (2 4) (2 5) (2 6)
   (3 4) (3 5) (3 6)
   (4 5) (4 6)
   (5 6)))
(Assert-set-equality
 (mapfam nil :mode 'comb [1 2 3 4 5 6] :arity 2 :glue 'dllist)
 (list
  (dllist 1 2) (dllist 1 3) (dllist 1 4) (dllist 1 5) (dllist 1 6)
   (dllist 2 3) (dllist 2 4) (dllist 2 5) (dllist 2 6)
   (dllist 3 4) (dllist 3 5) (dllist 3 6)
   (dllist 4 5) (dllist 4 6)
   (dllist 5 6)))
(Assert-tup-equality
 (mapfam nil :mode 'comb [1 2 3 4 5 6] :arity 2 :glue 'dllist)
 (list
  (dllist 1 2) (dllist 1 3) (dllist 1 4) (dllist 1 5) (dllist 1 6)
   (dllist 2 3) (dllist 2 4) (dllist 2 5) (dllist 2 6)
   (dllist 3 4) (dllist 3 5) (dllist 3 6)
   (dllist 4 5) (dllist 4 6)
   (dllist 5 6)))
(Assert-set-equality
 (mapfam nil :mode 'comb [1 2 3 4 5 6] :arity 3)
 '((1 2 3) (1 2 4) (1 2 5) (1 2 6)
   (1 3 4) (1 3 5) (1 3 6)
   (1 4 5) (1 4 6)
   (1 5 6)
   (2 3 4) (2 3 5) (2 3 6)
   (2 4 5) (2 4 6)
   (2 5 6)
   (3 4 5) (3 4 6)
   (3 5 6)
   (4 5 6)))
(Assert-tup-equality
 (mapfam nil :mode 'comb [1 2 3 4 5 6] :arity 3)
 '((1 2 3) (1 2 4) (1 2 5) (1 2 6)
   (1 3 4) (1 3 5) (1 3 6)
   (1 4 5) (1 4 6)
   (1 5 6)
   (2 3 4) (2 3 5) (2 3 6)
   (2 4 5) (2 4 6)
   (2 5 6)
   (3 4 5) (3 4 6)
   (3 5 6)
   (4 5 6)))
(Assert-tup-equality
 (mapfam nil :mode 'comb [1 2 3 4 5 6] :arity 3 :glue #'dllist)
 (list
  (dllist 1 2 3) (dllist 1 2 4) (dllist 1 2 5) (dllist 1 2 6)
  (dllist 1 3 4) (dllist 1 3 5) (dllist 1 3 6)
  (dllist 1 4 5) (dllist 1 4 6)
  (dllist 1 5 6)
  (dllist 2 3 4) (dllist 2 3 5) (dllist 2 3 6)
  (dllist 2 4 5) (dllist 2 4 6)
  (dllist 2 5 6)
  (dllist 3 4 5) (dllist 3 4 6)
  (dllist 3 5 6)
  (dllist 4 5 6)))
(Assert-tup-equality
 (mapfam nil :mode 'comb [1 2 3 4 5 6] :arity 4)
 '((1 2 3 4) (1 2 3 5) (1 2 3 6) (1 2 4 5) (1 2 4 6) (1 2 5 6)
   (1 3 4 5) (1 3 4 6) (1 3 5 6)
   (1 4 5 6)
   (2 3 4 5) (2 3 4 6) (2 3 5 6) (2 4 5 6)
   (3 4 5 6)))
;; a f'lot larger
(Assert-tup-equality
 (mapfam nil :mode 'comb [1 2 3 4 5 6 7 8 9] :arity 4)
 '((1 2 3 4) (1 2 3 5) (1 2 3 6) (1 2 3 7) (1 2 3 8) (1 2 3 9)
   (1 2 4 5) (1 2 4 6) (1 2 4 7) (1 2 4 8) (1 2 4 9)
   (1 2 5 6) (1 2 5 7) (1 2 5 8) (1 2 5 9)
   (1 2 6 7) (1 2 6 8) (1 2 6 9)
   (1 2 7 8) (1 2 7 9)
   (1 2 8 9)
   (1 3 4 5) (1 3 4 6) (1 3 4 7) (1 3 4 8) (1 3 4 9)
   (1 3 5 6) (1 3 5 7) (1 3 5 8) (1 3 5 9)
   (1 3 6 7) (1 3 6 8) (1 3 6 9)
   (1 3 7 8) (1 3 7 9)
   (1 3 8 9)
   (1 4 5 6) (1 4 5 7) (1 4 5 8) (1 4 5 9)
   (1 4 6 7) (1 4 6 8) (1 4 6 9)
   (1 4 7 8) (1 4 7 9)
   (1 4 8 9)
   (1 5 6 7) (1 5 6 8) (1 5 6 9)
   (1 5 7 8) (1 5 7 9)
   (1 5 8 9)
   (1 6 7 8) (1 6 7 9)
   (1 6 8 9)
   (1 7 8 9)
   (2 3 4 5) (2 3 4 6) (2 3 4 7) (2 3 4 8) (2 3 4 9)
   (2 3 5 6) (2 3 5 7) (2 3 5 8) (2 3 5 9)
   (2 3 6 7) (2 3 6 8) (2 3 6 9)
   (2 3 7 8) (2 3 7 9)
   (2 3 8 9)
   (2 4 5 6) (2 4 5 7) (2 4 5 8) (2 4 5 9)
   (2 4 6 7) (2 4 6 8) (2 4 6 9)
   (2 4 7 8) (2 4 7 9)
   (2 4 8 9)
   (2 5 6 7) (2 5 6 8) (2 5 6 9)
   (2 5 7 8) (2 5 7 9)
   (2 5 8 9)
   (2 6 7 8) (2 6 7 9)
   (2 6 8 9)
   (2 7 8 9)
   (3 4 5 6) (3 4 5 7) (3 4 5 8) (3 4 5 9)
   (3 4 6 7) (3 4 6 8) (3 4 6 9)
   (3 4 7 8) (3 4 7 9)
   (3 4 8 9)
   (3 5 6 7) (3 5 6 8) (3 5 6 9)
   (3 5 7 8) (3 5 7 9)
   (3 5 8 9)
   (3 6 7 8) (3 6 7 9)
   (3 6 8 9)
   (3 7 8 9)
   (4 5 6 7) (4 5 6 8) (4 5 6 9)
   (4 5 7 8) (4 5 7 9)
   (4 5 8 9)
   (4 6 7 8) (4 6 7 9)
   (4 6 8 9)
   (4 7 8 9)
   (5 6 7 8) (5 6 7 9)
   (5 6 8 9)
   (5 7 8 9)
   (6 7 8 9)))
;; testing PHenomena
(Assert (null (mapfam nil :mode 'comb :arity 2 [a])))
(Assert (null (mapfam nil :mode 'comb :arity 3 [a])))
(Assert (null (mapfam nil :mode 'comb :arity 4 [a])))
(Assert (null (mapfam nil :mode 'comb :arity 5 [a])))
(Assert (null (mapfam nil :mode 'comb :arity 6 [a])))
(Assert (null (mapfam nil :mode 'comb :arity 3 [a b])))
(Assert (null (mapfam nil :mode 'comb :arity 4 [a b])))
(Assert (null (mapfam nil :mode 'comb :arity 5 [a b])))
(Assert (null (mapfam nil :mode 'comb :arity 6 [a b])))
(Assert (null (mapfam nil :mode 'comb :arity 4 [a b c])))
(Assert (null (mapfam nil :mode 'comb :arity 5 [a b c])))
(Assert (null (mapfam nil :mode 'comb :arity 6 [a b c])))
;; more of them phenomena
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 2 [a] :result-type #'vector)
  []))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 3 [a] :result-type #'vector)
  []))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 4 [a] :result-type #'vector)
  []))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 5 [a] :result-type #'vector)
  []))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 6 [a] :result-type #'vector)
  []))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 3 [a b] :result-type #'vector)
  []))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 4 [a b] :result-type #'vector)
  []))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 5 [a b] :result-type #'vector)
  []))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 6 [a b] :result-type #'vector)
  []))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 4 [a b c] :result-type #'vector)
  []))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 5 [a b c] :result-type #'vector)
  []))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 6 [a b c] :result-type #'vector)
  []))
;; yet more
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 2 [a] :result-type #'dllist)
  (dllist)))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 3 [a] :result-type #'dllist)
  (dllist)))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 4 [a] :result-type #'dllist)
  (dllist)))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 5 [a] :result-type #'dllist)
  (dllist)))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 6 [a] :result-type #'dllist)
  (dllist)))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 3 [a b] :result-type #'dllist)
  (dllist)))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 4 [a b] :result-type #'dllist)
  (dllist)))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 5 [a b] :result-type #'dllist)
  (dllist)))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 6 [a b] :result-type #'dllist)
  (dllist)))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 4 [a b c] :result-type #'dllist)
  (dllist)))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 5 [a b c] :result-type #'dllist)
  (dllist)))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 6 [a b c] :result-type #'dllist)
  (dllist)))
;; not enough yet
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 2 [a] :result-type #'string)
  ""))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 3 [a] :result-type #'string)
  ""))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 4 [a] :result-type #'string)
  ""))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 5 [a] :result-type #'string)
  ""))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 6 [a] :result-type #'string)
  ""))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 3 [a b] :result-type #'string)
  ""))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 4 [a b] :result-type #'string)
  ""))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 5 [a b] :result-type #'string)
  ""))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 6 [a b] :result-type #'string)
  ""))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 4 [a b c] :result-type #'string)
  ""))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 5 [a b c] :result-type #'string)
  ""))
(Assert
 (equal
  (mapfam nil :mode 'comb :arity 6 [a b c] :result-type #'string)
  ""))

;; glue carts
(Assert
 (equal
  (mapfam nil [0 1 2] :mode 'cart :arity 1)
  '(0 1 2)))
(Assert
 (equal
  (mapfam nil [0 1 2] :mode 'cart :arity 2)
  '((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2))))
(Assert
 (equal
  (mapfam nil [0 1 2] :mode 'cart :arity 3)
  '((0 0 0) (0 0 1) (0 0 2) (0 1 0) (0 1 1) (0 1 2) (0 2 0) (0 2 1) (0 2 2)
    (1 0 0) (1 0 1) (1 0 2) (1 1 0) (1 1 1) (1 1 2) (1 2 0) (1 2 1) (1 2 2)
    (2 0 0) (2 0 1) (2 0 2) (2 1 0) (2 1 1) (2 1 2) (2 2 0) (2 2 1) (2 2 2))))
(Assert
 (equal
  (mapfam nil [0 1 2] :mode 'cart :arity 4)
  '((0 0 0 0) (0 0 0 1) (0 0 0 2) (0 0 1 0) (0 0 1 1) (0 0 1 2)
    (0 0 2 0) (0 0 2 1) (0 0 2 2) (0 1 0 0) (0 1 0 1) (0 1 0 2)
    (0 1 1 0) (0 1 1 1) (0 1 1 2) (0 1 2 0) (0 1 2 1) (0 1 2 2)
    (0 2 0 0) (0 2 0 1) (0 2 0 2) (0 2 1 0) (0 2 1 1) (0 2 1 2)
    (0 2 2 0) (0 2 2 1) (0 2 2 2)
    (1 0 0 0) (1 0 0 1) (1 0 0 2) (1 0 1 0) (1 0 1 1) (1 0 1 2)
    (1 0 2 0) (1 0 2 1) (1 0 2 2) (1 1 0 0) (1 1 0 1) (1 1 0 2)
    (1 1 1 0) (1 1 1 1) (1 1 1 2) (1 1 2 0) (1 1 2 1) (1 1 2 2)
    (1 2 0 0) (1 2 0 1) (1 2 0 2) (1 2 1 0) (1 2 1 1) (1 2 1 2)
    (1 2 2 0) (1 2 2 1) (1 2 2 2)
    (2 0 0 0) (2 0 0 1) (2 0 0 2) (2 0 1 0) (2 0 1 1) (2 0 1 2)
    (2 0 2 0) (2 0 2 1) (2 0 2 2) (2 1 0 0) (2 1 0 1) (2 1 0 2)
    (2 1 1 0) (2 1 1 1) (2 1 1 2) (2 1 2 0) (2 1 2 1) (2 1 2 2)
    (2 2 0 0) (2 2 0 1) (2 2 0 2) (2 2 1 0) (2 2 1 1) (2 2 1 2)
    (2 2 2 0) (2 2 2 1) (2 2 2 2))))
;; for the next one we just pick 20 random elements out of 243
;; ... this assumes that #'random works
(let ((5c (mapfam nil [0 1 2] :mode 'cart :arity 5)))
  (Assert (= (length 5c) 243))
  (dotimes (i 20)
    (Assert
     (member
      (list (random 3) (random 3) (random 3) (random 3) (random 3))
      5c))))
;; to use something other than ints ...
(let ((6c (mapfam nil [1.4142 1.4142 1.4142] :mode 'cart :arity 6)))
  (Assert (= (length 6c) 729))
  ;; all elements must look the same
  (dotimes (i 729)
    (Assert-Equal (car 6c) '(1.4142 1.4142 1.4142 1.4142 1.4142 1.4142))
    (setq 6c (cdr 6c))))
;; test invariance of input sequence type
(Assert
 (equal
  (mapfam nil '(0 1 2) :mode 'cart :arity 1)
  (mapfam nil (dllist 0 1 2) :mode 'cart :arity 1)))
(Assert
 (equal
  (mapfam nil '(0 1 2) :mode 'cart :arity 2)
  (mapfam nil (dllist 0 1 2) :mode 'cart :arity 2)))
(Assert
 (equal
  (mapfam nil '(0 1 2) :mode 'cart :arity 3)
  (mapfam nil (dllist 0 1 2) :mode 'cart :arity 3)))
(Assert
 (equal
  (mapfam nil '(0 1 2) :mode 'cart :arity 4)
  (mapfam nil (dllist 0 1 2) :mode 'cart :arity 4)))
(Assert
 (equal
  (mapfam nil '(0 1 2) :mode 'cart :arity 5)
  (mapfam nil (dllist 0 1 2) :mode 'cart :arity 5)))
(Assert
 (equal
  (mapfam nil '(0 1 2) :mode 'cart :arity 9)
  (mapfam nil (dllist 0 1 2) :mode 'cart :arity 9)))

;; testing perms
(Assert-set-equality
 (mapfam nil '(0 1) :mode 'perm)
 '((0 1) (1 0)))
(Assert
 (equal
  (mapfam nil '(0 1) :mode 'perm :arity 1)
  '(0 1)))
(Assert-set-equality
 (mapfam nil '(0 1) :mode 'perm :arity 2)
 '((0 1) (1 0)))
(Assert
 (null (mapfam nil '(0 1) :mode 'perm :arity 3)))

(Assert-set-equality
 (mapfam nil '(0 1 2) :mode 'perm)
 '((0 1 2) (1 2 0) (2 0 1) (2 1 0) (1 0 2) (0 2 1)))
(Assert
 (equal
  (mapfam nil '(0 1 2) :mode 'perm :arity 1)
  '(0 1 2)))
(Assert-set-equality
 (mapfam nil '(0 1 2) :mode 'perm :arity 2)
 '((0 1) (1 0) (0 2) (2 0) (1 2) (2 1)))
(Assert-set-equality
 (mapfam nil '(0 1 2) :mode 'perm :arity 3)
 '((0 1 2) (1 2 0) (2 0 1) (2 1 0) (1 0 2) (0 2 1)))
(Assert
 (null (mapfam nil '(0 1 2) :mode 'perm :arity 4)))

(Assert-set-equality
 (mapfam nil '(1 2 3 4) :mode 'perm)
 '((1 2 3 4) (1 2 4 3) (1 4 2 3) (1 4 3 2) (1 3 4 2) (1 3 2 4)
   (2 3 1 4) (2 3 4 1) (2 4 3 1) (2 4 1 3) (2 1 4 3) (2 1 3 4)
   (3 1 2 4) (3 1 4 2) (3 4 1 2) (3 4 2 1) (3 2 4 1) (3 2 1 4)
   (4 2 1 3) (4 2 3 1) (4 3 2 1) (4 3 1 2) (4 1 3 2) (4 1 2 3)))
(Assert
 (equal
  (mapfam nil '(1 2 3 4) :mode 'perm :arity 1)
  '(1 2 3 4)))
(Assert-set-equality
 (mapfam nil '(1 2 3 4) :mode 'perm :arity 2)
 '((1 2) (2 1) (1 3) (3 1) (1 4) (4 1)
   (2 3) (3 2) (2 4) (4 2)
   (3 4) (4 3)))
(Assert-set-equality
 (mapfam nil '(1 2 3 4) :mode 'perm :arity 3)
 ;; we've checked S_3 perms already, so just use them
 (append
  (mapfam nil '(1 2 3) :mode 'perm)
  (mapfam nil '(1 2 4) :mode 'perm)
  (mapfam nil '(1 3 4) :mode 'perm)
  (mapfam nil '(2 3 4) :mode 'perm)))
(Assert-set-equality
 (mapfam nil '(1 2 3 4) :mode 'perm :arity 4)
 '((1 2 3 4) (1 2 4 3) (1 4 2 3) (1 4 3 2) (1 3 4 2) (1 3 2 4)
   (2 3 1 4) (2 3 4 1) (2 4 3 1) (2 4 1 3) (2 1 4 3) (2 1 3 4)
   (3 1 2 4) (3 1 4 2) (3 4 1 2) (3 4 2 1) (3 2 4 1) (3 2 1 4)
   (4 2 1 3) (4 2 3 1) (4 3 2 1) (4 3 1 2) (4 1 3 2) (4 1 2 3)))
(Assert
 (null (mapfam nil '(1 2 3 4) :mode 'perm :arity 5)))

(Assert-set-equality
 (mapfam nil [0 1] :mode 'perm)
 (mapfam nil (dllist 0 1) :mode 'perm))
(Assert
 (equal
  (mapfam nil (dllist 0 1) :mode 'perm :arity 1)
  (mapfam nil [0 1] :mode 'perm :arity 1)))
(Assert-set-equality
 (mapfam nil [0 1] :mode 'perm :arity 2)
 (mapfam nil (dllist 0 1) :mode 'perm :arity 2))
(Assert
 (null (mapfam nil [0 1] :mode 'perm :arity 3)))
(Assert
 (null (mapfam nil (dllist 0 1) :mode 'perm :arity 3)))

(Assert-set-equality
 (mapfam nil [0 1 2] :mode 'perm)
 (mapfam nil (dllist 0 1 2) :mode 'perm))
(Assert
 (equal
  (mapfam nil [0 1 2] :mode 'perm :arity 1)
  (mapfam nil (dllist 0 1 2) :mode 'perm :arity 1)))
(Assert-set-equality
 (mapfam nil (dllist 0 1 2) :mode 'perm :arity 2)
 (mapfam nil [0 1 2] :mode 'perm :arity 2))
(Assert-set-equality
 (mapfam nil [0 1 2] :mode 'perm :arity 3)
 (mapfam nil (dllist 0 1 2) :mode 'perm :arity 3))
(Assert
 (null (mapfam nil [0 1 2] :mode 'perm :arity 4)))
(Assert
 (null (mapfam nil (dllist 0 1 2) :mode 'perm :arity 4)))

(Assert-set-equality
 (mapfam nil [1 2 3 4] :mode 'perm)
 (mapfam nil (dllist 1 2 3 4) :mode 'perm))
(Assert
 (equal
  (mapfam nil [1 2 3 4] :mode 'perm :arity 1)
  (mapfam nil (dllist 1 2 3 4) :mode 'perm :arity 1)))
(Assert-set-equality
 (mapfam nil [1 2 3 4] :mode 'perm :arity 2)
 (mapfam nil (dllist 1 2 3 4) :mode 'perm :arity 2))
(Assert-set-equality
 (mapfam nil [1 2 3 4] :mode 'perm :arity 3)
 (mapfam nil (dllist 1 2 3 4) :mode 'perm :arity 3))
(Assert-set-equality
 (mapfam nil (dllist 1 2 3 4) :mode 'perm :arity 4)
 (mapfam nil [1 2 3 4] :mode 'perm :arity 4))
(Assert
 (null (mapfam nil [1 2 3 4] :mode 'perm :arity 5)))
(Assert
 (null (mapfam nil (dllist 1 2 3 4) :mode 'perm :arity 5)))

;;; just some things that caused problems in the past
(Assert-set-equality
 (mapfam nil :glue #'string :mode 'perm "ab")
 '("ab" "ba"))
(Assert-set-equality
 (mapfam #'sxhash :glue #'string :mode 'perm "ab")
 (list (sxhash "ab") (sxhash "ba")))
(Assert-set-equality
 (mapfam #'string :mode 'perm "ab")
 '("ab" "ba"))

(Assert-set-equality
 (mapfam nil :glue #'string :mode 'perm "abc")
 '("abc" "acb" "cab" "cba" "bca" "bac"))
(Assert-set-equality
 (mapfam #'sxhash :glue #'string :mode 'perm "abc")
 (list (sxhash "abc") (sxhash "acb") (sxhash "cab")
       (sxhash "cba") (sxhash "bca") (sxhash "bac")))
(Assert-set-equality
 (mapfam #'string :mode 'perm "abc")
 '("abc" "acb" "cab" "cba" "bca" "bac"))

(Assert-set-equality
 (mapfam nil :glue #'string :mode 'perm "abcd")
 '("abcd" "abdc" "adbc" "adcb" "acdb" "acbd"
   "cabd" "cadb" "cdab" "cdba" "cbda" "cbad"
   "bcad" "bcda" "bdca" "bdac" "badc" "bacd"
   "dbac" "dbca" "dcba" "dcab" "dacb" "dabc"))
(Assert-set-equality
 (mapfam #'sxhash :glue #'string :mode 'perm "abcd")
 (list (sxhash "abcd") (sxhash "abdc") (sxhash "adbc")
       (sxhash "adcb") (sxhash "acdb") (sxhash "acbd")
       (sxhash "cabd") (sxhash "cadb") (sxhash "cdab")
       (sxhash "cdba") (sxhash "cbda") (sxhash "cbad")
       (sxhash "bcad") (sxhash "bcda") (sxhash "bdca")
       (sxhash "bdac") (sxhash "badc") (sxhash "bacd")
       (sxhash "dbac") (sxhash "dbca") (sxhash "dcba")
       (sxhash "dcab") (sxhash "dacb") (sxhash "dabc")))
(Assert-set-equality
 (mapfam #'string :mode 'perm "abcd")
 '("abcd" "abdc" "adbc" "adcb" "acdb" "acbd"
   "cabd" "cadb" "cdab" "cdba" "cbda" "cbad"
   "bcad" "bcda" "bdca" "bdac" "badc" "bacd"
   "dbac" "dbca" "dcba" "dcab" "dacb" "dabc"))

(mapfam nil :glue #'string :mode 'perm "abcde" :arity 4)
(mapfam #'sxhash :glue #'string :mode 'perm "abcde" :arity 4)
(mapfam #'string :mode 'perm "abcde" :arity 4)


;; now with real funs aboard
(Assert
 (equal
  (mapfam #'1- [1 2 3 4 5 6])
  '(0 1 2 3 4 5)))
(Assert
 (equal
  (mapfam #'1+ :mode 'pntw [1 2 3 4 5 6])
  '(2 3 4 5 6 7)))

(let* ((l ''(1 2 3 4 5 6))
       (v [1 2 3 4 5 6])
       (d (dllist 1 2 3 4 5 6))
       ;; a sequence of coprime length
       (l2 ''(1 2 3 4 5 6 7))
       (v2 [1 2 3 4 5 6 7])
       (d2 (dllist 1 2 3 4 5 6 7))
       ;; arity 2 results
       (rl '(list . '(3 7 11)))
       (rv '(vector . [3 7 11]))
       (rd '(dllist . (dllist 3 7 11)))
       ;; arity 3 results
       (rl2 '(list . '(6 15)))
       (rv2 '(vector . [6 15]))
       (rd2 '(dllist . (dllist 6 15))))
  (loop for i in (list l v d) do
    (loop for j in (list rl rv rd) do
      (eval
       `(Assert
	 (equal
	  (mapfam #'+ ,i :arity 2 :result-type #',(car j))
	  ,(cdr j))))))
  ;; seq length coprime to arity
  (loop for i in (list l2 v2 d2) do
    (loop for j in (list rl rv rd) do
      (eval
       `(Assert
	 (equal
	  (mapfam #'+ ,i :arity 2 :result-type #',(car j))
	  ,(cdr j))))))
  ;; arity 3
  (loop for i in (list l v d) do
    (loop for j in (list rl2 rv2 rd2) do
      (eval
       `(Assert
	 (equal
	  (mapfam #'+ ,i :arity 3 :result-type #',(car j))
	  ,(cdr j))))))
  ;; seq length coprime to arity
  (loop for i in (list l2 v2 d2) do
    (loop for j in (list rl2 rv2 rd2) do
      (eval
       `(Assert
	 (equal
	  (mapfam #'+ ,i :arity 3 :result-type #',(car j))
	  ,(cdr j)))))))

(Assert
 (equal
  (mapfam #'+ [1 2 3 4 5 6 7 8] :arity 4)
  '(10 26)))
(Assert
 (equal
  (mapfam #'+ [1 2 3 4 5 6 7 8] :arity 4 :result-type #'vector)
  [10 26]))
(Assert
 (equal
  (mapfam #'+ [1 2 3 4 5 6 7 8] :arity 4 :result-type #'dllist)
  (dllist 10 26)))

(Assert
 (equal
  (mapfam #'+ [1 2 3 4 5 6 7 8 9 10 11] :arity 4)
  '(10 26)))
(Assert
 (equal
  (mapfam #'+ '(1 2 3 4 5 6 7 8 9 10 11) :arity 4 :result-type #'vector)
  [10 26]))
(Assert
 (equal
  (mapfam #'+ (dllist 1 2 3 4 5 6 7 8 9 10 11) :arity 4 :result-type #'dllist)
  (dllist 10 26)))

;; wreck the sequence during traversal
;; (setq foo (dllist 1 2 3 4 5 6 7 8))
;; (defun wreck-1 (a)
;;   (dllist-pop-car foo)
;;   (1+ a))
;; (Assert
;;  (equal
;;   (mapfam #'wreck-1 foo)
;;   '(2 3 4 5 6 7 8 9)))
;; (Assert (= (dllist-size foo) 0))

;; ;; wreck the sequence during traversal, arity 2
;; (setq foo (dllist 1 2 3 4 5 6 7 8))
;; (defun wreck-2 (a b)
;;   (dllist-pop-rac foo)
;;   (+ a b))
;; (Assert
;;  (equal
;;   (mapfam #'wreck-2 foo :arity 2)
;;   '(3 7 11 15)))
;; ;; foo was called 4 times, so should be (dllist 1 2 3 4) now
;; (Assert
;;  (equal
;;   (mapfam #'wreck-2 foo :arity 2)
;;   '(3 7)))
;; ;; foo was called twice, so should be (dllist 1 2) now
;; (Assert
;;  (equal
;;   (mapfam #'wreck-2 foo :arity 2)
;;   '(3)))
;; foo was called once, so should be (dllist 1) now,
;; however just one element is below the requested arity, so
;; we expect nil now
;; (Assert
;;  (null (mapfam #'wreck-2 foo :arity 2)))
;; ;; foo should still have this one element
;; (Assert (= (dllist-size foo) 1))

;; ;; wreck the sequence during traversal, arity 3
;; (setq foo (dllist 1 2 3 4 5 6 7 8))
;; (defun wreck-3 (a b c)
;;   (dllist-pop-rac foo)
;;   (+ a b c))
;; (Assert
;;  (equal
;;   (mapfam #'wreck-3 foo :arity 3)
;;   '(6 15)))
;; ;; foo was called 2 times, so should be (dllist 1 2 3 4 5 6) now
;; (Assert
;;  (equal
;;   (mapfam #'wreck-3 foo :arity 3)
;;   '(6 15)))
;; ;; foo was called twice, so should be (dllist 1 2 3 4) now
;; (Assert
;;  (equal
;;   (mapfam #'wreck-3 foo :arity 3)
;;   '(6)))
;; ;; foo was called once, so should be (dllist 1 2 3) now,
;; (Assert
;;  (equal
;;   (mapfam #'wreck-3 foo :arity 3)
;;   '(6)))
;; ;; again, foo was called once, so should be (dllist 1 2) now,
;; ;; however that's less elements than arity so we expect nil now
;; (Assert
;;  (null (mapfam #'wreck-3 foo :arity 3)))
;; ;; foo should still have these two elements
;; (Assert (= (dllist-size foo) 2))
;; (Assert-Equal foo (dllist 1 2))

;; dicts
(setq test-ht (make-hash-table)
      test-sl (make-skiplist))

(puthash '1 'a test-ht)
(puthash '2 'b test-ht)
(puthash '3 'c test-ht)
(puthash '4 'd test-ht)
(puthash '5 'e test-ht)
(puthash '6 'f test-ht)
(puthash '7 'g test-ht)

(put-skiplist test-sl '1 'a)
(put-skiplist test-sl '2 'b)
(put-skiplist test-sl '3 'c)
(put-skiplist test-sl '4 'd)
(put-skiplist test-sl '5 'e)
(put-skiplist test-sl '6 'f)
(put-skiplist test-sl '7 'g)

;; initialise a summing variable
(setq sum 0)

(defun sum-2 (key val)
  (setq sum (+ sum key)))
;; we can't use the output of this one for it is uncertain in which
;; order the keys are passed
(mapfam #'sum-2 test-ht)
;; however, sum should be 28 (= 7 + 3 + 6 + 4 + 5 + 2 + 1) now
(Assert (= sum 28))
;; reset sum
(setq sum 0)
(Assert (= sum 0))
;; skiplists always iterate in hash-order and hashes of ints are
;; order preserving, so we are able to check the outcome
(Assert
 (equal
  (setq bar (mapfam #'sum-2 test-sl))
  '(1 3 6 10 15 21 28)))
(Assert (= sum 28))


;;; testing on 2 sequences
(Assert
 (equal
  (mapfam nil '(a b c) '(1 2 3))
  '((a 1) (b 2) (c 3))))
(Assert
 (equal
  (mapfam nil '(a b c) '(1 2 3 4))
  ;; the 4 doesnt count since another sequence has finished traversal by then
  '((a 1) (b 2) (c 3))))
;; the same with list and vector
(Assert
 (equal
  (mapfam nil '(a b c) [1 2 3])
  '((a 1) (b 2) (c 3))))
(Assert
 (equal
  (mapfam nil [a b c] '(1 2 3 4))
  ;; the 4 doesnt count since another sequence has finished traversal by then
  '((a 1) (b 2) (c 3))))
;; list and dllist
(Assert
 (equal
  (mapfam nil '(a b c) (dllist 1 2 3))
  '((a 1) (b 2) (c 3))))
(Assert
 (equal
  (mapfam nil (dllist 'a 'b 'c) '(1 2 3 4))
  ;; the 4 doesnt count since another sequence has finished traversal by then
  '((a 1) (b 2) (c 3))))
;; dllist and vector
(Assert
 (equal
  (mapfam nil (dllist 'a 'b 'c) [1 2 3])
  '((a 1) (b 2) (c 3))))
(Assert
 (equal
  (mapfam nil [a b c] (dllist 1 2 3 4))
  ;; the 4 doesnt count since another sequence has finished traversal by then
  '((a 1) (b 2) (c 3))))
;; string and vector
(Assert
 (equal
  (mapfam nil "abc" [1 2 3])
  '((?a 1) (?b 2) (?c 3))))
(Assert
 (equal
  (mapfam nil [a b c] "1234")
  ;; the 4 doesnt count since another sequence has finished traversal by then
  '((a ?1) (b ?2) (c ?3))))
;; more than 2 sequences
(Assert
 (equal
  (mapfam nil '(a b c) [1 2 3] (dllist ?x ?y ?z))
  '((a 1 ?x) (b 2 ?y) (c 3 ?z))))
(Assert
 (equal
  (mapfam nil '(a b c) [1 2 3 4] (dllist ?x ?y ?z))
  '((a 1 ?x) (b 2 ?y) (c 3 ?z))))

;; all of the above using a different glue
(Assert
 (equal
  (mapfam nil :glue #'vector '(a b c) '(1 2 3))
  '([a 1] [b 2] [c 3])))
(Assert
 (equal
  (mapfam nil :glue #'vector '(a b c) '(1 2 3 4))
  ;; the 4 doesnt count since another sequence has finished traversal by then
  '([a 1] [b 2] [c 3])))
;; the same with list and vector
(Assert
 (equal
  (mapfam nil :glue #'vector '(a b c) [1 2 3])
  '([a 1] [b 2] [c 3])))
(Assert
 (equal
  (mapfam nil :glue #'vector [a b c] '(1 2 3 4))
  ;; the 4 doesnt count since another sequence has finished traversal by then
  '([a 1] [b 2] [c 3])))
;; list and dllist
(Assert
 (equal
  (mapfam nil :glue #'vector '(a b c) (dllist 1 2 3))
  '([a 1] [b 2] [c 3])))
(Assert
 (equal
  (mapfam nil :glue #'vector (dllist 'a 'b 'c) '(1 2 3 4))
  ;; the 4 doesnt count since another sequence has finished traversal by then
  '([a 1] [b 2] [c 3])))
;; dllist and vector
(Assert
 (equal
  (mapfam nil :glue #'vector (dllist 'a 'b 'c) [1 2 3])
  '([a 1] [b 2] [c 3])))
(Assert
 (equal
  (mapfam nil :glue #'vector [a b c] (dllist 1 2 3 4))
  ;; the 4 doesnt count since another sequence has finished traversal by then
  '([a 1] [b 2] [c 3])))
;; string and vector
(Assert
 (equal
  (mapfam nil :glue #'vector "abc" [1 2 3])
  '([?a 1] [?b 2] [?c 3])))
(Assert
 (equal
  (mapfam nil :glue #'vector [a b c] "1234")
  ;; the 4 doesnt count since another sequence has finished traversal by then
  '([a ?1] [b ?2] [c ?3])))
;; more than 2 sequences
(Assert
 (equal
  (mapfam nil :glue #'vector '(a b c) [1 2 3] (dllist ?x ?y ?z))
  '([a 1 ?x] [b 2 ?y] [c 3 ?z])))
(Assert
 (equal
  (mapfam nil :glue #'vector '(a b c) [1 2 3 4] (dllist ?x ?y ?z))
  '([a 1 ?x] [b 2 ?y] [c 3 ?z])))


;;; combinations with more than 1 family
(Assert-set-equality
 (mapfam nil :mode 'comb [a b c] "1234")
 ;; the 4 doesnt count since another sequence has finished traversal by then
 '((a ?1) (a ?2) (a ?3) (a ?4)
   (b ?1) (b ?2) (b ?3) (b ?4)
   (c ?1) (c ?2) (c ?3) (c ?4)))

(Assert-set-equality
 (mapfam nil :glue #'vector :mode 'comb [a b c] "1234")
 ;; the 4 doesnt count since another sequence has finished traversal by then
 '([a ?1] [a ?2] [a ?3] [a ?4]
   [b ?1] [b ?2] [b ?3] [b ?4]
   [c ?1] [c ?2] [c ?3] [c ?4]))

(Assert-set-equality
 (mapfam #'cons :mode 'comb [a b c] "1234")
 ;; the 4 doesnt count since another sequence has finished traversal by then
 '((a . ?1) (a . ?2) (a . ?3) (a . ?4)
   (b . ?1) (b . ?2) (b . ?3) (b . ?4)
   (c . ?1) (c . ?2) (c . ?3) (c . ?4)))

;;; cartesians with more than 1 family
(Assert-set-equality
 (mapfam nil :mode 'cart [a b c] "1234")
 ;; the 4 doesnt count since another sequence has finished traversal by then
 '((a ?1) (a ?2) (a ?3) (a ?4)
   (b ?1) (b ?2) (b ?3) (b ?4)
   (c ?1) (c ?2) (c ?3) (c ?4)))

(Assert-set-equality
 (mapfam nil :glue #'vector :mode 'cart [a b c] "1234")
 ;; the 4 doesnt count since another sequence has finished traversal by then
 '([a ?1] [a ?2] [a ?3] [a ?4]
   [b ?1] [b ?2] [b ?3] [b ?4]
   [c ?1] [c ?2] [c ?3] [c ?4]))

(Assert-set-equality
 (mapfam #'cons :mode 'cart [a b c] "1234")
 ;; the 4 doesnt count since another sequence has finished traversal by then
 '((a . ?1) (a . ?2) (a . ?3) (a . ?4)
   (b . ?1) (b . ?2) (b . ?3) (b . ?4)
   (c . ?1) (c . ?2) (c . ?3) (c . ?4)))

(Assert-set-equality
 (mapfam nil :mode 'cart [?a ?b] "12" (dllist ?X))
 '((?a ?1 ?X) (?a ?2 ?X) (?b ?1 ?X) (?b ?2 ?X)))

(Assert-set-equality
 (mapfam nil :glue #'vector :mode 'cart [?a ?b] "12" (dllist ?X))
 '([?a ?1 ?X] [?a ?2 ?X] [?b ?1 ?X] [?b ?2 ?X]))

(Assert-set-equality
 (mapfam #'concat :glue #'vector :mode 'cart [?a ?b] "12" (dllist ?X))
 '("a1X" "a2X" "b1X" "b2X"))

(Assert-set-equality
 (mapfam #'string :mode 'cart [?a ?b] "12" (dllist ?X))
 '("a1X" "a2X" "b1X" "b2X"))

;;; perms with more than 1 family
(Assert-set-equality
 (mapfam nil :mode 'perm [?a ?b] "12")
 '((?a ?1) (?1 ?a)
   (?a ?2) (?2 ?a)
   (?b ?1) (?1 ?b)
   (?b ?2) (?2 ?b)))

(Assert-set-equality
 (mapfam nil :mode 'perm [?a ?b] "12" (dllist ?X))
 '((?a ?1 ?X) (?a ?X ?1) (?X ?a ?1) (?X ?1 ?a) (?1 ?X ?a) (?1 ?a ?X)
   (?a ?2 ?X) (?a ?X ?2) (?X ?a ?2) (?X ?2 ?a) (?2 ?X ?a) (?2 ?a ?X)
   (?b ?1 ?X) (?b ?X ?1) (?X ?b ?1) (?X ?1 ?b) (?1 ?X ?b) (?1 ?b ?X)
   (?b ?2 ?X) (?b ?X ?2) (?X ?b ?2) (?X ?2 ?b) (?2 ?X ?b) (?2 ?b ?X)))

(Assert-set-equality
 (mapfam #'string :mode 'perm [?a ?b] "12" (dllist ?X))
 '("a1X" "aX1" "Xa1" "X1a" "1Xa" "1aX"
   "a2X" "aX2" "Xa2" "X2a" "2Xa" "2aX"
   "b1X" "bX1" "Xb1" "X1b" "1Xb" "1bX"
   "b2X" "bX2" "Xb2" "X2b" "2Xb" "2bX"))

(Assert-set-equality
 (mapfam #'string :mode 'perm [?a ?b] "12" (dllist ?X) [?A])
 '("a1XA" "a1AX" "aA1X" "aAX1" "aXA1" "aX1A"
   "1aXA" "1aAX" "1AaX" "1AXa" "1XAa" "1XaA"
   "X1aA" "X1Aa" "XA1a" "XAa1" "XaA1" "Xa1A"
   "A1Xa" "A1aX" "Aa1X" "AaX1" "AXa1" "AX1a"

   "a2XA" "2aXA" "2XaA" "X2aA" "Xa2A" "aX2A"
   "aXA2" "XaA2" "XAa2" "AXa2" "AaX2" "aAX2"
   "2AXa" "A2Xa" "AX2a" "XA2a" "X2Aa" "2XAa"
   "2aAX" "a2AX" "aA2X" "Aa2X" "A2aX" "2AaX"

   "b1XA" "1bXA" "1XbA" "X1bA" "Xb1A" "bX1A"
   "bXA1" "XbA1" "XAb1" "AXb1" "AbX1" "bAX1"
   "1AXb" "A1Xb" "AX1b" "XA1b" "X1Ab" "1XAb"
   "1bAX" "b1AX" "bA1X" "Ab1X" "A1bX" "1AbX"

   "b2XA" "2bXA" "2XbA" "X2bA" "Xb2A" "bX2A"
   "bXA2" "XbA2" "XAb2" "AXb2" "AbX2" "bAX2"
   "2AXb" "A2Xb" "AX2b" "XA2b" "X2Ab" "2XAb"
   "2bAX" "b2AX" "bA2X" "Ab2X" "A2bX" "2AbX"))


;;; testing over multiple sequences with strange arities
(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity 1)
 '(?a ?b ?c ?d))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(1))
 '(?a ?b ?c ?d))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(2))
 '("ab" "cd"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(1 1))
 '("a1" "b2" "c3" "d4"))

;; (Assert-set-equality
;;  (mapfam nil :glue #'string "abcd" "1234" "another" :arity '(1 1))
;;  '("a1" "b2" "c3" "d4"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(1 1 1))
 '("a1" "b2" "c3" "d4"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(1 1 2))
 '("a1" "b2" "c3" "d4"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(1 2))
 '("a12" "b34"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(2 1))
 '("ab1" "cd2"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(2 1 1))
 '("ab1" "cd2"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(2 2))
 '("ab12" "cd34"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(3 1))
 '("abc1"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(1 3))
 '("a123"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(3 2))
 '("abc12"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(2 3))
 '("ab123"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(3 3))
 '("abc123"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(4 1))
 '("abcd1"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(1 4))
 '("a1234"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(4 2))
 '("abcd12"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(2 4))
 '("ab1234"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(4 3))
 '("abcd123"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(3 4))
 '("abc1234"))

(Assert-set-equality
 (mapfam nil :glue #'string "abcd" "1234" :arity '(4 4))
 '("abcd1234"))

(Assert
 (null
  (mapfam nil :glue #'string "abcd" "1234" :arity '(5 1))))

(Assert
 (null
  (mapfam nil :glue #'string "abcd" "1234" :arity '(1 5))))

(Assert
 (null
  (mapfam nil :glue #'string "abcd" "1234" :arity '(5 5))))

;; carts
(Assert-set-equality
 (mapfam nil :mode 'cart :glue #'string "abc" "123" :arity '(2 1))
 '("aa1" "aa2" "aa3" "ab1" "ab2" "ab3" "ac1" "ac2" "ac3"
   "ba1" "ba2" "ba3" "bb1" "bb2" "bb3" "bc1" "bc2" "bc3"
   "ca1" "ca2" "ca3" "cb1" "cb2" "cb3" "cc1" "cc2" "cc3"))
(Assert-set-equality
 (mapfam nil :mode 'cart :glue #'string "abc" "123" :arity '(2 2))
 '("aa11" "aa12" "aa13" "aa21" "aa22" "aa23" "aa31" "aa32" "aa33"
   "ab11" "ab12" "ab13" "ab21" "ab22" "ab23" "ab31" "ab32" "ab33"
   "ac11" "ac12" "ac13" "ac21" "ac22" "ac23" "ac31" "ac32" "ac33"

   "ba11" "ba12" "ba13" "ba21" "ba22" "ba23" "ba31" "ba32" "ba33"
   "bb11" "bb12" "bb13" "bb21" "bb22" "bb23" "bb31" "bb32" "bb33"
   "bc11" "bc12" "bc13" "bc21" "bc22" "bc23" "bc31" "bc32" "bc33"

   "ca11" "ca12" "ca13" "ca21" "ca22" "ca23" "ca31" "ca32" "ca33"
   "cb11" "cb12" "cb13" "cb21" "cb22" "cb23" "cb31" "cb32" "cb33"
   "cc11" "cc12" "cc13" "cc21" "cc22" "cc23" "cc31" "cc32" "cc33"))

;; combs
(Assert-set-equality
 (mapfam nil :mode 'comb :glue #'string "abcd" "123" :arity '(2 2))
 '("ab12" "ab13" "ab23" "ac12" "ac13" "ac23" "ad12" "ad13" "ad23"
   "bc12" "bc13" "bc23" "bd12" "bd13" "bd23"
   "cd12" "cd13" "cd23"))

(Assert-set-equality
 (mapfam nil :mode 'comb :glue #'string "abcd" "123" :arity '(2 1))
 '("ab1" "ab2" "ab3" "ac1" "ac2" "ac3" "ad1" "ad2" "ad3"
   "bc1" "bc2" "bc3" "bd1" "bd2" "bd3"
   "cd1" "cd2" "cd3"))

(Assert-set-equality
 (mapfam nil :mode 'comb :glue #'string "abcd" "1234" :arity '(2 3))
 '("ab123" "ab124" "ab134" "ab234"
   "ac123" "ac124" "ac134" "ac234"
   "ad123" "ad124" "ad134" "ad234"
   "bc123" "bc124" "bc134" "bc234"
   "bd123" "bd124" "bd134" "bd234"
   "cd123" "cd124" "cd134" "cd234"))

(Assert
 (null (mapfam nil :mode 'comb :glue #'string "abcd" "1234" :arity '(2 5))))

;; the same but with #'string being the mapper
(Assert-set-equality
 (mapfam #'string :mode 'comb "abcd" "123" :arity '(2 2))
 '("ab12" "ab13" "ab23" "ac12" "ac13" "ac23" "ad12" "ad13" "ad23"
   "bc12" "bc13" "bc23" "bd12" "bd13" "bd23"
   "cd12" "cd13" "cd23"))

(Assert-set-equality
 (mapfam #'string :mode 'comb "abcd" "123" :arity '(2 1))
 '("ab1" "ab2" "ab3" "ac1" "ac2" "ac3" "ad1" "ad2" "ad3"
   "bc1" "bc2" "bc3" "bd1" "bd2" "bd3"
   "cd1" "cd2" "cd3"))

(Assert-set-equality
 (mapfam #'string :mode 'comb "abcd" "1234" :arity '(2 3))
 '("ab123" "ab124" "ab134" "ab234"
   "ac123" "ac124" "ac134" "ac234"
   "ad123" "ad124" "ad134" "ad234"
   "bc123" "bc124" "bc134" "bc234"
   "bd123" "bd124" "bd134" "bd234"
   "cd123" "cd124" "cd134" "cd234"))

(Assert
 (null (mapfam #'string :mode 'comb "abcd" "1234" :arity '(2 5))))

;; the same but with #'concat being the mapper and #'string being glue
(Assert-set-equality
 (mapfam #'concat :mode 'comb :glue #'string "abcd" "123" :arity '(2 2))
 '("ab12" "ab13" "ab23" "ac12" "ac13" "ac23" "ad12" "ad13" "ad23"
   "bc12" "bc13" "bc23" "bd12" "bd13" "bd23"
   "cd12" "cd13" "cd23"))

(Assert-set-equality
 (mapfam #'concat :mode 'comb :glue #'string "abcd" "123" :arity '(2 1))
 '("ab1" "ab2" "ab3" "ac1" "ac2" "ac3" "ad1" "ad2" "ad3"
   "bc1" "bc2" "bc3" "bd1" "bd2" "bd3"
   "cd1" "cd2" "cd3"))

(Assert-set-equality
 (mapfam #'concat :mode 'comb :glue #'string "abcd" "1234" :arity '(2 3))
 '("ab123" "ab124" "ab134" "ab234"
   "ac123" "ac124" "ac134" "ac234"
   "ad123" "ad124" "ad134" "ad234"
   "bc123" "bc124" "bc134" "bc234"
   "bd123" "bd124" "bd134" "bd234"
   "cd123" "cd124" "cd134" "cd234"))

(Assert
 (null (mapfam #'concat :mode 'comb :glue #'string "abcd" "1234" :arity '(2 5))))

;; perms
(Assert-set-equality
 (mapfam nil :mode 'perm :glue #'string "abcd" "123" :arity '(2 2))
 (append
  (mapfam nil :mode 'perm :glue #'string "ab12")
  (mapfam nil :mode 'perm :glue #'string "ab13")
  (mapfam nil :mode 'perm :glue #'string "ab23")
  (mapfam nil :mode 'perm :glue #'string "ac12")
  (mapfam nil :mode 'perm :glue #'string "ac13")
  (mapfam nil :mode 'perm :glue #'string "ac23")
  (mapfam nil :mode 'perm :glue #'string "ad12")
  (mapfam nil :mode 'perm :glue #'string "ad13")
  (mapfam nil :mode 'perm :glue #'string "ad23")
  (mapfam nil :mode 'perm :glue #'string "bc12")
  (mapfam nil :mode 'perm :glue #'string "bc13")
  (mapfam nil :mode 'perm :glue #'string "bc23")
  (mapfam nil :mode 'perm :glue #'string "bd12")
  (mapfam nil :mode 'perm :glue #'string "bd13")
  (mapfam nil :mode 'perm :glue #'string "bd23")
  (mapfam nil :mode 'perm :glue #'string "cd12")
  (mapfam nil :mode 'perm :glue #'string "cd13")
  (mapfam nil :mode 'perm :glue #'string "cd23")))

(Assert-set-equality
 (mapfam nil :mode 'perm :glue #'string "abcd" "123" :arity '(1 1))
 '("a1" "1a" "a2" "2a" "a3" "3a" "b1" "1b" "b2" "2b" "b3" "3b"
   "c1" "1c" "c2" "2c" "c3" "3c" "d1" "1d" "d2" "2d" "d3" "3d"))

;;; test Steve's favourite
;;
;; (mapconcat #'concat (split-string "unsplit this split string") " ")
;;  => "unsplit this split string"
;;
;; Now with #'mapfam...
;;
;; First, with the exact same syntax as we did with #'mapconcat (because it is
;; our plan to replace all of our #'map* functions with aliases to #'mapfam)
;;
;; (mapfam #'concat (split-string "unsplit this split string") " ")
;;  => Wrong type argument: sequencep, ?\  <- it's a space character
;;
;; hrop: Yes, that's exactly what happens!  Let's make a test ...

(Check-Error wrong-type-argument
 (mapfam #'concat (split-string "unsplit this split string") " "))

;; ... why that you ask?
;; By design #'mapfam operates like CL's #'map, i.e. takes any number of
;; sequences as input.  Because of this there is no way to tell whether you
;; actually meant the third arg of the above mapfam call to act as separator
;; or as a sequence (a string is a sequence too), well, it is simply assumed
;; that you mean the sequence which is why the above call throws the
;; wrong-type-argument error.
;;
;; Now lets try to wing it with more fancy syntax...
;;
;; (mapfam nil :separator " " :result-type #'concat
;;         (split-string "unsplit this split string"))
;;  => "unsplitthissplitstring"
;;
;; hrop: That's a bug ... here's the test

(Assert
 (string= (mapfam nil :separator " " :result-type #'concat
		  (split-string "unsplit this split string"))
	  "unsplit this split string"))

;; here's the proof that it really inserts the separator element
(Assert
 (equal (mapfam nil :separator " " :result-type #'list
		(split-string "unsplit this split string"))
	'("unsplit" " " "this" " " "split" " " "string")))

;; (mapfam #'concat :result-type #'concat
;;         (split-string "unsplit this split string") " ")
;;  => Wrong type argument: sequencep, ?\
;;
;; hrop: um, I can't see what that is supposed to do, but yes, the wrong-type
;;       error is because of the second sequence (" ") which is kinda like [?\ ]
;;       let's do some fancy tests insteads

(Assert
 (string=
  (mapfam nil :initiator "!" :separator " " :terminator "?"
	  :result-type #'concat ["uuuuh" "yeeeeah"])
  "!uuuuh yeeeeah?"))

;; here to prove that the separator can be anything really
(Assert
 (equal
  (mapfam nil :initiator ?! :separator (cons 'foo 'bar) :terminator ??
	  :result-type #'vector ["does" "this" "work"])
  [?! "does" (foo . bar) "this" (foo . bar) "work" ?\?]))

;; and it is the same element actually ... which is an undocumented feature :)

(setq bar
      (mapfam nil :separator (cons 'foo 'bar) :result-type #'vector
	      ["a" "b" "c"]))
(setcar (aref bar 1) 'bar)
(Assert (eq (car (aref bar 3)) 'bar))

;; Oh, here's a way...
;; (mapfam #'concat :result-type #'concat
;;         (split-string "unsplit this split string") '(" " " " " " ""))
;;  => "unsplit this split string"
;;
;; hrop: yipp :) that works of course ...

;; (Assert
;;  (string=
;;   (mapfam #'concat :result-type #'concat
;;	  (split-string "unsplit this split string") '(" " " " " " ""))
;;   "unsplit this split string"))
;;
;; (when-fboundp #'divisiblep
;;   (Assert-set-equality
;;    (let ((divisors))
;;      (mapfam :result-type 'void
;;	     #'(lambda (p)
;;		 (garbage-collect)
;;		 (if (divisiblep 5041 p) (push p divisors))
;;		 (garbage-collect))
;;	     '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
;;		 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37
;;		 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54
;;		 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 69 70
;;		 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87
;;		 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103
;;		 104 105 106 107 108 109 110 111 112 113 114 115 116
;;		 117 118 119 120 121 122 123 124 125 126 127 128 129
;;		 130 131 132 133 134 135 136 137 138 139 140 141 142
;;		 143 144 145 146 147 148 149 150 151 152 153 154 155
;;		 156 157 158 159 160 161 162 163 164 165 166 167 168
;;		 169 170 171 172 173 174 175 176 177 178 179 180 181
;;		 182 183 184 185 186 187 188 189 190 191 192 193 194
;;		 195 196 197 198 199 200 201 202 203 204 205 206 207
;;		 208 209 210 211 212 213 214 215 216 217 218 219 220
;;		 221 222 223 224 225 226 227 228 229 230 231 232 233
;;		 234 235 236 237 238 239 240 241 242 243 244 245 246
;;		 247 248 249 250 251 252 253 254 255 256 257 258 259
;;		 260 261 262 263 264 265 266 267 268 269 270 271 272
;;		 273 274 275 276 277 278 279 280 281 282 283 284 285
;;		 ))
;;      divisors)
;;    '(1 71))
;;
;;   (Assert-set-equality
;;    (let ((divisors))
;;      (mapfam #'(lambda (p)
;;		 (garbage-collect)
;;		 (if (divisiblep 5041 p) (push p divisors))
;;		 (garbage-collect))
;;	     '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
;;		 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37
;;		 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54
;;		 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 69 70
;;		 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87
;;		 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103
;;		 104 105 106 107 108 109 110 111 112 113 114 115 116
;;		 117 118 119 120 121 122 123 124 125 126 127 128 129
;;		 130 131 132 133 134 135 136 137 138 139 140 141 142
;;		 143 144 145 146 147 148 149 150 151 152 153 154 155
;;		 156 157 158 159 160 161 162 163 164 165 166 167 168
;;		 169 170 171 172 173 174 175 176 177 178 179 180 181
;;		 182 183 184 185 186 187 188 189 190 191 192 193 194
;;		 195 196 197 198 199 200 201 202 203 204 205 206 207
;;		 208 209 210 211 212 213 214 215 216 217 218 219 220
;;		 221 222 223 224 225 226 227 228 229 230 231 232 233
;;		 234 235 236 237 238 239 240 241 242 243 244 245 246
;;		 247 248 249 250 251 252 253 254 255 256 257 258 259
;;		 260 261 262 263 264 265 266 267 268 269 270 271 272
;;		 273 274 275 276 277 278 279 280 281 282 283 284 285
;;		 ))
;;      divisors)
;;    '(1 71)))


(Assert (null (mapfam #'identity nil)))
(Assert (null (mapfam nil nil)))
(Assert (null (mapfam #'identity [])))
(Assert (null (mapfam nil [])))
(Assert (null (mapfam #'identity (dllist))))
(Assert (null (mapfam nil (dllist))))
(Assert (null (mapfam #'identity "")))
(Assert (null (mapfam nil "")))

(Assert (vectorp (mapfam #'identity nil :result-type #'vector)))
(Assert (vectorp (mapfam nil nil :result-type #'vector)))
(Assert (vectorp (mapfam #'identity [] :result-type #'vector)))
(Assert (vectorp (mapfam nil [] :result-type #'vector)))
(Assert (vectorp (mapfam #'identity (dllist) :result-type #'vector)))
(Assert (vectorp (mapfam nil (dllist) :result-type #'vector)))
(Assert (vectorp (mapfam #'identity "" :result-type #'vector)))
(Assert (vectorp (mapfam nil "" :result-type #'vector)))

(Assert (zerop (length (mapfam #'identity nil :result-type #'vector))))
(Assert (zerop (length (mapfam nil nil :result-type #'vector))))
(Assert (zerop (length (mapfam #'identity [] :result-type #'vector))))
(Assert (zerop (length (mapfam nil [] :result-type #'vector))))
(Assert (zerop (length (mapfam #'identity (dllist) :result-type #'vector))))
(Assert (zerop (length (mapfam nil (dllist) :result-type #'vector))))
(Assert (zerop (length (mapfam #'identity "" :result-type #'vector))))
(Assert (zerop (length (mapfam nil "" :result-type #'vector))))

;;; map-tests.el ends here
