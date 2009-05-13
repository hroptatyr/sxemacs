;;; undo-stack.el --- An "undoable stack" object.

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) 1996 Ben Wing.

;; Maintainer: SXEmacs Development Team
;; Keywords: extensions, dumped

;; This file is part of SXEmacs.

;; SXEmacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; SXEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file is dumped with SXEmacs.

;; An "undoable stack" is an object that can be used to implement
;; a history of positions, with undo and redo.  Conceptually, it
;; is the kind of data structure used to keep track of (e.g.)
;; visited Web pages, so that the "Back" and "Forward" operations
;; in the browser work.  Basically, I can successively visit a
;; number of Web pages through links, and then hit "Back" a
;; few times to go to previous positions, and then "Forward" a
;; few times to reverse this process.  This is similar to an
;; "undo" and "redo" mechanism.

;; Note that Emacs does not standardly contain structures like
;; this.  Instead, it implements history using either a ring
;; (the kill ring, the mark ring), or something like the undo
;; stack, where successive "undo" operations get recorded as
;; normal modifications, so that if you do a bunch of successive
;; undo's, then something else, then start undoing, you will
;; be redoing all your undo's back to the point before you did
;; the undo's, and then further undo's will act like the previous
;; round of undo's.  I think that both of these paradigms are
;; inferior to the "undoable-stack" paradigm because they're
;; confusing and difficult to keep track of.

;; Conceptually, imagine a position history like this:

;;   1 -> 2 -> 3 -> 4 -> 5 -> 6
;;                            ^^

;; where the arrow indicates where you currently are.  "Going back"
;; and "going forward" just amount to moving the arrow.  However,
;; what happens if the history state is this:

;;   1 -> 2 -> 3 -> 4 -> 5 -> 6
;;                  ^^

;; and then I visit new positions (7) and (8)?  In the most general
;; implementation, you've just caused a new branch like this:

;;   1 -> 2 -> 3 -> 4 -> 5 -> 6
;;                  |
;;                  |
;;                  7 -> 8
;;                       ^^

;; But then you can end up with a whole big tree, and you need
;; more sophisticated ways of navigating ("Forward" might involve
;; a choice of paths to follow) and managing its size (if you don't
;; want to keep unlimited history, you have to truncate at some point,
;; and how do you truncate a tree?)

;; My solution to this is just to insert the new positions like
;; this:

;;   1 -> 2 -> 3 -> 4 -> 7 -> 8 -> 5 -> 6
;;                            ^^

;; (Netscape, I think, would just truncate 5 and 6 completely,
;; but that seems a bit drastic.  In the Emacs-standard "ring"
;; structure, this problem is avoided by simply moving 5 and 6
;; to the beginning of the ring.  However, it doesn't seem
;; logical to me to have "going back past 1" get you to 6.)

;; Now what if we have a "maximum" size of (say) 7 elements?
;; When we add 8, we could truncate either 1 or 6.  Since 5 and
;; 6 are "undone" positions, we should presumably truncate
;; them before 1.  So, adding 8 truncates 6, adding 9 truncates
;; 5, and adding 10 truncates 1 because there is nothing more
;; that is forward of the insertion point.

;; Interestingly, this method of truncation is almost like
;; how a ring would truncate.  A ring would move 5 and 6
;; around to the back, like this:

;;   5 -> 6 -> 1 -> 2 -> 3 -> 4 -> 7 -> 8
;;                                      ^^

;; However, when 8 is added, the ring truncates 5 instead of
;; 6, which is less than optimal.

;; Conceptually, we can implement the "undoable stack" using
;; two stacks of a sort called "truncatable stack", which are
;; just simple stacks, but where you can truncate elements
;; off of the bottom of the stack.  Then, the undoable stack

;;   1 -> 2 -> 3 -> 4 -> 5 -> 6
;;                  ^^

;; is equivalent to two truncatable stacks:

;;   4 <- 3 <- 2 <- 1
;;   5 <- 6

;; where I reversed the direction to accord with the probable
;; implementation of a standard list.  To do another undo,
;; I pop 4 off of the first stack and move it to the top of
;; the second stack.  A redo operation does the opposite.
;; To truncate to the proper size, first chop off 6, then 5,
;; then 1 -- in all cases, truncating off the bottom.

;;; Code:

(define-error 'trunc-stack-bottom "Bottom of stack reached")

(defsubst trunc-stack-stack (stack)
  ;; return the list representing the trunc-stack's elements.
  ;; the head of the list is the most recent element.
  (aref stack 1))

(defsubst trunc-stack-length (stack)
  ;; return the number of elements in the trunc-stack.
  (aref stack 2))

(defsubst set-trunc-stack-stack (stack new)
  ;; set the list representing the trunc-stack's elements.
  (aset stack 1 new))

(defsubst set-trunc-stack-length (stack new)
  ;; set the length of the trunc-stack.
  (aset stack 2 new))

;; public functions:

(defun make-trunc-stack ()
  ;; make an empty trunc-stack.
  (vector 'trunc-stack nil 0))

(defun trunc-stack-push (stack el)
  ;; push a new element onto the head of the trunc-stack.
  (set-trunc-stack-stack stack (cons el (trunc-stack-stack stack)))
  (set-trunc-stack-length stack (1+ (trunc-stack-length stack))))

(defun trunc-stack-top (stack &optional n)
  ;; return the nth topmost element from the trunc-stack.
  ;; signal an error if the stack doesn't have that many elements.
  (or n (setq n 0))
  (if (>= n (trunc-stack-length stack))
      (signal-error 'trunc-stack-bottom (list stack))
    (nth n (trunc-stack-stack stack))))

(defun trunc-stack-pop (stack)
  ;; pop and return the topmost element from the stack.
  (prog1 (trunc-stack-top stack)
    (set-trunc-stack-stack stack (cdr (trunc-stack-stack stack)))
    (set-trunc-stack-length stack (1- (trunc-stack-length stack)))))

(defun trunc-stack-truncate (stack &optional n)
  ;; truncate N items off the bottom of the stack.  If the stack is
  ;; not that big, it just becomes empty.
  (or n (setq n 1))
  (if (> n 0)
      (let ((len (trunc-stack-length stack)))
	(if (>= n len)
	    (progn
	      (set-trunc-stack-length stack 0)
	      (set-trunc-stack-stack stack nil))
	  (setcdr (nthcdr (1- (- len n)) (trunc-stack-stack stack)) nil)
	  (set-trunc-stack-length stack (- len n))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FMH! FMH! FMH!  This object-oriented stuff doesn't really work
;;; properly without built-in structures (vectors suck) and without
;;; public and private functions and fields.

(defsubst undoable-stack-max (stack)
  (aref stack 1))

(defsubst undoable-stack-a (stack)
  (aref stack 2))

(defsubst undoable-stack-b (stack)
  (aref stack 3))

;; public functions:

(defun make-undoable-stack (max)
  ;; make an empty undoable stack of max size MAX.
  (vector 'undoable-stack max (make-trunc-stack) (make-trunc-stack)))

(defsubst set-undoable-stack-max (stack new)
  ;; change the max size of an undoable stack.
  (aset stack 1 new))

(defun undoable-stack-a-top (stack)
  ;; return the topmost element off the "A" stack of an undoable stack.
  ;; this is the most recent position pushed on the undoable stack.
  (trunc-stack-top (undoable-stack-a stack)))

(defun undoable-stack-a-length (stack)
  (trunc-stack-length (undoable-stack-a stack)))

(defun undoable-stack-b-top (stack)
  ;; return the topmost element off the "B" stack of an undoable stack.
  ;; this is the position that will become the most recent position,
  ;; after a redo operation.
  (trunc-stack-top (undoable-stack-b stack)))

(defun undoable-stack-b-length (stack)
  (trunc-stack-length (undoable-stack-b stack)))

(defun undoable-stack-push (stack el)
  ;; push an element onto the stack.
  (let*
      ((lena (trunc-stack-length (undoable-stack-a stack)))
       (lenb (trunc-stack-length (undoable-stack-b stack)))
       (max (undoable-stack-max stack))
       (len (+ lena lenb)))
    ;; maybe truncate some elements.  We have to deal with the
    ;; possibility that we have more elements than our max
    ;; (someone might have reduced the max).
    (if (>= len max)
	(let ((must-nuke (1+ (- len max))))
	  ;; chop off must-nuke elements from the B stack.
	  (trunc-stack-truncate (undoable-stack-b stack) must-nuke)
	  ;; but if there weren't that many elements to chop,
	  ;; take the rest off the A stack.
	  (if (< lenb must-nuke)
	      (trunc-stack-truncate (undoable-stack-a stack)
				    (- must-nuke lenb)))))
    (trunc-stack-push (undoable-stack-a stack) el)))

(defun undoable-stack-pop (stack)
  ;; pop an element off the stack.
  (trunc-stack-pop (undoable-stack-a stack)))

(defun undoable-stack-undo (stack)
  ;; transfer an element from the top of A to the top of B.
  ;; return value is undefined.
  (trunc-stack-push (undoable-stack-b stack)
		    (trunc-stack-pop (undoable-stack-a stack))))

(defun undoable-stack-redo (stack)
  ;; transfer an element from the top of B to the top of A.
  ;; return value is undefined.
  (trunc-stack-push (undoable-stack-a stack)
		    (trunc-stack-pop (undoable-stack-b stack))))


;;; undo-stack.el ends here
