;;; cl.el --- Common Lisp extensions for GNU Emacs Lisp

;; Copyright (C) 1993, 1997 Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Maintainer: SXEmacs Development Team
;; Version: 2.02
;; Keywords: extensions, dumped, lisp

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

;;; Synched up with: FSF 19.34.

;;; Commentary:

;; This file is dumped with SXEmacs.

;; These are extensions to Emacs Lisp that provide a degree of
;; Common Lisp compatibility, beyond what is already built-in
;; in Emacs Lisp.
;;
;; This package was written by Dave Gillespie; it is a complete
;; rewrite of Cesar Quiroz's original cl.el package of December 1986.
;;
;; This package works with Emacs 18, Emacs 19, and XEmacs/Lucid Emacs 19.
;;
;; Bug reports, comments, and suggestions are welcome!

;; This file contains the portions of the Common Lisp extensions
;; package which should always be present.


;;; Future notes:

;; Once Emacs 19 becomes standard, many things in this package which are
;; messy for reasons of compatibility can be greatly simplified.  For now,
;; I prefer to maintain one unified version.


;;; Change Log:

;; Version 2.02 (30 Jul 93):
;;  * Added "cl-compat.el" file, extra compatibility with old package.
;;  * Added `lexical-let' and `lexical-let*'.
;;  * Added `define-modify-macro', `callf', and `callf2'.
;;  * Added `ignore-errors'.
;;  * Changed `(setf (nthcdr N PLACE) X)' to work when N is zero.
;;  * Merged `*gentemp-counter*' into `*gensym-counter*'.
;;  * Extended `subseq' to allow negative START and END like `substring'.
;;  * Added `in-ref', `across-ref', `elements of-ref' loop clauses.
;;  * Added `concat', `vconcat' loop clauses.
;;  * Cleaned up a number of compiler warnings.

;; Version 2.01 (7 Jul 93):
;;  * Added support for FSF version of Emacs 19.
;;  * Added `add-hook' for Emacs 18 users.
;;  * Added `defsubst*' and `symbol-macrolet'.
;;  * Added `maplist', `mapc', `mapl', `mapcan', `mapcon'.
;;  * Added `map', `concatenate', `reduce', `merge'.
;;  * Added `revappend', `nreconc', `tailp', `tree-equal'.
;;  * Added `assert', `check-type', `typecase', `typep', and `deftype'.
;;  * Added destructuring and `&environment' support to `defmacro*'.
;;  * Added destructuring to `loop', and added the following clauses:
;;      `elements', `frames', `overlays', `intervals', `buffers', `key-seqs'.
;;  * Renamed `delete' to `delete*' and `remove' to `remove*'.
;;  * Completed support for all keywords in `remove*', `substitute', etc.
;;  * Added `most-positive-float' and company.
;;  * Fixed hash tables to work with latest Lucid Emacs.
;;  * `proclaim' forms are no longer compile-time-evaluating; use `declaim'.
;;  * Syntax for `warn' declarations has changed.
;;  * Improved implementation of `random*'.
;;  * Moved most sequence functions to a new file, cl-seq.el.
;;  * Moved `eval-when' into cl-macs.el.
;;  * Moved `pushnew' and `adjoin' to cl.el for most common cases.
;;  * Moved `provide' forms down to ends of files.
;;  * Changed expansion of `pop' to something that compiles to better code.
;;  * Changed so that no patch is required for Emacs 19 byte compiler.
;;  * Made more things dependent on `optimize' declarations.
;;  * Added a partial implementation of struct print functions.
;;  * Miscellaneous minor changes.

;; Version 2.00:
;;  * First public release of this package.


;;; Code:

(defvar cl-emacs-type (cond ((or (and (fboundp 'epoch::version)
				      (symbol-value 'epoch::version))
				 (string-lessp emacs-version "19")) 18)
			    ((string-match "XEmacs" emacs-version)
			     'lucid)
			    (t 19)))

(or (fboundp 'defalias) (fset 'defalias 'fset))

(defvar cl-optimize-speed 1)
(defvar cl-optimize-safety 1)


;;; Keywords used in this package.

;;; XEmacs - keywords are done in Fintern().
;;;
;;; (defconst :test ':test)
;;; (defconst :test-not ':test-not)
;;; (defconst :key ':key)
;;; (defconst :start ':start)
;;; (defconst :start1 ':start1)
;;; (defconst :start2 ':start2)
;;; (defconst :end ':end)
;;; (defconst :end1 ':end1)
;;; (defconst :end2 ':end2)
;;; (defconst :count ':count)
;;; (defconst :initial-value ':initial-value)
;;; (defconst :size ':size)
;;; (defconst :from-end ':from-end)
;;; (defconst :rehash-size ':rehash-size)
;;; (defconst :rehash-threshold ':rehash-threshold)
;;; (defconst :allow-other-keys ':allow-other-keys)


(defvar custom-print-functions nil
  "This is a list of functions that format user objects for printing.
Each function is called in turn with three arguments: the object, the
stream, and the print level (currently ignored).  If it is able to
print the object it returns true; otherwise it returns nil and the
printer proceeds to the next function on the list.")

(defun print-custom-object (obj &optional stream pl)
  "Print custom object OBJ into STREAM using print level PL."
  (let ((cpfs custom-print-functions)
	(ret nil))
    (while cpfs
      (when (funcall (car cpfs) obj stream pl)
	(setq cpfs nil
	      ret t))
      (setq cpfs (cdr cpfs)))
    ret))

(defun print-object-ignoring-custom-print-functions (obj &optional stream pl)
  "Print object OBJ not using custom printers."
  (let ((custom-object-printer nil))
    (princ obj stream)))

;; Install custom printer
(setq custom-object-printer 'print-custom-object)

;;; Predicates.

(defun eql (a b)    ; See compiler macro in cl-macs.el
  "Return t if the two args are the same Lisp object.
Floating-point numbers of equal value are `eql', but they may not be `eq'."
  (if (floatp a)
      (equal a b)
    (eq a b)))


;;; Generalized variables.  These macros are defined here so that they
;;; can safely be used in .emacs files.

(defmacro incf (place &optional x)
  "(incf PLACE [X]): increment PLACE by X (1 by default).
PLACE may be a symbol, or any generalized variable allowed by `setf'.
The return value is the incremented value of PLACE."
  (if (symbolp place)
      (list 'setq place (if x (list '+ place x) (list '1+ place)))
    ;; XEmacs byte-compiler optimizes (+ FOO 1) to (1+ FOO), so this
    ;; is OK.
    (list 'callf '+ place (or x 1))))

(defmacro decf (place &optional x)
  "(decf PLACE [X]): decrement PLACE by X (1 by default).
PLACE may be a symbol, or any generalized variable allowed by `setf'.
The return value is the decremented value of PLACE."
  (if (symbolp place)
      (list 'setq place (if x (list '- place x) (list '1- place)))
    (list 'callf '- place (or x 1))))

(defmacro pop (place)
  "(pop PLACE): remove and return the head of the list stored in PLACE.
Analogous to (prog1 (car PLACE) (setf PLACE (cdr PLACE))), though more
careful about evaluating each argument only once and in the right order.
PLACE may be a symbol, or any generalized variable allowed by `setf'."
  (if (symbolp place)
      `(car (prog1 ,place (setq ,place (cdr ,place))))
    (cl-do-pop place)))

(defmacro push (x place)
  "(push X PLACE): insert X at the head of the list stored in PLACE.
Analogous to (setf PLACE (cons X PLACE)), though more careful about
evaluating each argument only once and in the right order.  PLACE may
be a symbol, or any generalized variable allowed by `setf'; that is,
it does not necessarily have to be a list, though `push' is most often
used on lists."
  (if (symbolp place) `(setq ,place (cons ,x ,place))
    (list 'callf2 'cons x place)))

(defmacro pushnew (x place &rest keys)
  "(pushnew X PLACE): insert X at the head of the list stored in PLACE.
Like (push X PLACE), except that the list is unmodified if X is `eql'
to an element already on the list.
Keywords supported: :test :test-not :key"
  (if (symbolp place) (list 'setq place (list* 'adjoin x place keys))
    (list* 'callf2 'adjoin x place keys)))

(defun cl-set-elt (seq n val)
  (if (listp seq) (setcar (nthcdr n seq) val) (aset seq n val)))

(defun cl-set-nthcdr (n list x)
  (if (<= n 0) x (setcdr (nthcdr (1- n) list) x) list))

(defun cl-set-buffer-substring (start end val)
  (save-excursion (delete-region start end)
		  (goto-char start)
		  (insert val)
		  val))

(defun cl-set-substring (str start end val)
  (if end (if (< end 0) (incf end (length str)))
    (setq end (length str)))
  (if (< start 0) (incf start (length str)))
  (concat (and (> start 0) (substring str 0 start))
	  val
	  (and (< end (length str)) (substring str end))))


;;; Control structures.

;; The macros `when' and `unless' are so useful that we want them to
;; ALWAYS be available.  So they've been moved from cl.el to eval.c.
;; Note: FSF Emacs moved them to subr.el in FSF 20.

(defun cl-map-extents (&rest cl-args)
  ;; XEmacs: This used to check for overlays first, but that's wrong
  ;; because of the new compatibility library.  *duh*
  (cond ((fboundp 'map-extents)
	 (apply 'map-extents cl-args))
	((fboundp 'next-overlay-at)
	 (apply 'cl-map-overlays cl-args))))


;;; Blocks and exits.

(defalias 'cl-block-wrapper 'identity)
(defalias 'cl-block-throw 'throw)


;;; Multiple values.  True multiple values are not supported, or even
;;; simulated.  Instead, multiple-value-bind and friends simply expect
;;; the target form to return the values as a list.

(defalias 'values 'list)
(defalias 'values-list 'identity)
(defalias 'multiple-value-list 'identity)
(defalias 'multiple-value-call 'apply)  ; only works for one arg
(defalias 'nth-value 'nth)


;;; Macros.

(defvar cl-macro-environment nil)
;; XEmacs: we renamed the internal function to macroexpand-internal
;; to avoid doc-file problems.
(defvar cl-old-macroexpand (prog1 (symbol-function 'macroexpand-internal)
			     (defalias 'macroexpand 'cl-macroexpand)))

(defun cl-macroexpand (cl-macro &optional cl-env)
  "Return result of expanding macros at top level of FORM.
If FORM is not a macro call, it is returned unchanged.
Otherwise, the macro is expanded and the expansion is considered
in place of FORM.  When a non-macro-call results, it is returned.

The second optional arg ENVIRONMENT specifies an environment of macro
definitions to shadow the loaded ones for use in file byte-compilation."
  (let ((cl-macro-environment cl-env))
    (while (progn (setq cl-macro (funcall cl-old-macroexpand cl-macro cl-env))
		  (and (symbolp cl-macro)
		       (cdr (assq (symbol-name cl-macro) cl-env))))
      (setq cl-macro (cadr (assq (symbol-name cl-macro) cl-env))))
    cl-macro))


;;; Declarations.

(defvar cl-compiling-file nil)
(defun cl-compiling-file ()
  (or cl-compiling-file
      ;; XEmacs change
;      (and (boundp 'outbuffer) (bufferp (symbol-value 'outbuffer))
;	   (equal (buffer-name (symbol-value 'outbuffer))
;		  " *Compiler Output*"))
      (and (boundp 'byte-compile-outbuffer)
	   (bufferp (symbol-value 'byte-compile-outbuffer))
	   (equal (buffer-name (symbol-value 'byte-compile-outbuffer))
		  " *Compiler Output*"))
      ))

(defvar cl-proclaims-deferred nil)

(defun proclaim (spec)
  (if (fboundp 'cl-do-proclaim) (cl-do-proclaim spec t)
    (push spec cl-proclaims-deferred))
  nil)

(defmacro declaim (&rest specs)
  (let ((body (mapcar (function (lambda (x) (list 'proclaim (list 'quote x))))
		      specs)))
    (if (cl-compiling-file) (list* 'eval-when '(compile load eval) body)
      (cons 'progn body))))   ; avoid loading cl-macs.el for eval-when


;;; Symbols.

(defun cl-random-time ()
  (let* ((time (copy-sequence (current-time-string))) (i (length time)) (v 0))
    (while (>= (decf i) 0) (setq v (+ (* v 3) (aref time i))))
    v))

(defvar *gensym-counter* (* (logand (cl-random-time) 1023) 100))

(defun gensym (&optional arg)
  "Generate a new uninterned symbol.
The name is made by appending a number to a prefix.  If ARG is a string, it
is the prefix, otherwise the prefix defaults to \"G\".  If ARG is an integer,
the internal counter is reset to that number before creating the name.
There is no way to specify both using this function."
  (let ((prefix (if (stringp arg) arg "G"))
	(num (if (integerp arg) arg
	       (prog1 *gensym-counter*
		 (setq *gensym-counter* (1+ *gensym-counter*))))))
    (make-symbol (format "%s%d" prefix num))))

(defun gentemp (&optional arg)
  "Generate a new interned symbol with a unique name.
The name is made by appending a number to ARG, default \"G\".
If ARG is not a string, it is ignored."
  (let ((prefix (if (stringp arg) arg "G"))
	name)
    (while (intern-soft (setq name (format "%s%d" prefix *gensym-counter*)))
      (setq *gensym-counter* (1+ *gensym-counter*)))
    (intern name)))

;;; Numbers.

(defun floatp-safe (object)
  "Return t if OBJECT is a floating point number."
  (floatp object))

(defun plusp (number)
  "Return t if NUMBER is positive."
  (> number 0))

(defun minusp (number)
  "Return t if NUMBER is negative."
  (< number 0))

(unless (featurep 'ent)
  (defun oddp (integer)
    "Return t if INTEGER is odd."
    (eq (logand integer 1) 1))

  (defun evenp (integer)
    "Return t if INTEGER is even."
    (eq (logand integer 1) 0)))

(defun cl-abs (number)
  "Return the absolute value of NUMBER."
  (if (>= number 0) number (- number)))
(or (fboundp 'abs) (defalias 'abs 'cl-abs))   ; This is built-in to Emacs 19

(defvar *random-state* (vector 'cl-random-state-tag -1 30 (cl-random-time)))

;; Threse constants are defined in C when 'number-types is provided
(unless (featurep 'ent)
;;; We use `eval' in case VALBITS differs from compile-time to load-time.
  (defconst most-positive-fixnum (eval '(lsh -1 -1))
    "The integer closest in value to positive infinity.")
  (defconst most-negative-fixnum (eval '(- -1 (lsh -1 -1)))
    "The integer closest in value to negative infinity."))

;;; Sequence functions.

(defalias 'copy-seq 'copy-sequence)

(defun mapcar* (cl-func cl-x &rest cl-rest)
  "Apply FUNCTION to each element of SEQ, and make a list of the results.
If there are several SEQs, FUNCTION is called with that many arguments,
and mapping stops as soon as the shortest list runs out.  With just one
SEQ, this is like `mapcar'.  With several, it is like the Common Lisp
`mapcar' function extended to arbitrary sequence types."
  (if cl-rest
      (if (or (cdr cl-rest) (nlistp cl-x) (nlistp (car cl-rest)))
	  (cl-mapcar-many cl-func (cons cl-x cl-rest))
	(let ((cl-res nil) (cl-y (car cl-rest)))
	  (while (and cl-x cl-y)
	    (push (funcall cl-func (pop cl-x) (pop cl-y)) cl-res))
	  (nreverse cl-res)))
    (mapcar cl-func cl-x)))


;;; List functions.

;; These functions are made known to the byte-compiler by cl-macs.el
;; and turned into efficient car and cdr bytecodes.

(defalias 'first 'car)
(defalias 'rest 'cdr)
(defalias 'endp 'null)

(defun second (x)
  "Return the second element of the list LIST."
  (car (cdr x)))

(defun third (x)
  "Return the third element of the list LIST."
  (car (cdr (cdr x))))

(defun fourth (x)
  "Return the fourth element of the list LIST."
  (nth 3 x))

(defun fifth (x)
  "Return the fifth element of the list LIST."
  (nth 4 x))

(defun sixth (x)
  "Return the sixth element of the list LIST."
  (nth 5 x))

(defun seventh (x)
  "Return the seventh element of the list LIST."
  (nth 6 x))

(defun eighth (x)
  "Return the eighth element of the list LIST."
  (nth 7 x))

(defun ninth (x)
  "Return the ninth element of the list LIST."
  (nth 8 x))

(defun tenth (x)
  "Return the tenth element of the list LIST."
  (nth 9 x))

(defun caar (x)
  "Return the `car' of the `car' of X."
  (car (car x)))

(defun cadr (x)
  "Return the `car' of the `cdr' of X."
  (car (cdr x)))

(defun cdar (x)
  "Return the `cdr' of the `car' of X."
  (cdr (car x)))

(defun cddr (x)
  "Return the `cdr' of the `cdr' of X."
  (cdr (cdr x)))

(defun caaar (x)
  "Return the `car' of the `car' of the `car' of X."
  (car (car (car x))))

(defun caadr (x)
  "Return the `car' of the `car' of the `cdr' of X."
  (car (car (cdr x))))

(defun cadar (x)
  "Return the `car' of the `cdr' of the `car' of X."
  (car (cdr (car x))))

(defun caddr (x)
  "Return the `car' of the `cdr' of the `cdr' of X."
  (car (cdr (cdr x))))

(defun cdaar (x)
  "Return the `cdr' of the `car' of the `car' of X."
  (cdr (car (car x))))

(defun cdadr (x)
  "Return the `cdr' of the `car' of the `cdr' of X."
  (cdr (car (cdr x))))

(defun cddar (x)
  "Return the `cdr' of the `cdr' of the `car' of X."
  (cdr (cdr (car x))))

(defun cdddr (x)
  "Return the `cdr' of the `cdr' of the `cdr' of X."
  (cdr (cdr (cdr x))))

(defun caaaar (x)
  "Return the `car' of the `car' of the `car' of the `car' of X."
  (car (car (car (car x)))))

(defun caaadr (x)
  "Return the `car' of the `car' of the `car' of the `cdr' of X."
  (car (car (car (cdr x)))))

(defun caadar (x)
  "Return the `car' of the `car' of the `cdr' of the `car' of X."
  (car (car (cdr (car x)))))

(defun caaddr (x)
  "Return the `car' of the `car' of the `cdr' of the `cdr' of X."
  (car (car (cdr (cdr x)))))

(defun cadaar (x)
  "Return the `car' of the `cdr' of the `car' of the `car' of X."
  (car (cdr (car (car x)))))

(defun cadadr (x)
  "Return the `car' of the `cdr' of the `car' of the `cdr' of X."
  (car (cdr (car (cdr x)))))

(defun caddar (x)
  "Return the `car' of the `cdr' of the `cdr' of the `car' of X."
  (car (cdr (cdr (car x)))))

(defun cadddr (x)
  "Return the `car' of the `cdr' of the `cdr' of the `cdr' of X."
  (car (cdr (cdr (cdr x)))))

(defun cdaaar (x)
  "Return the `cdr' of the `car' of the `car' of the `car' of X."
  (cdr (car (car (car x)))))

(defun cdaadr (x)
  "Return the `cdr' of the `car' of the `car' of the `cdr' of X."
  (cdr (car (car (cdr x)))))

(defun cdadar (x)
  "Return the `cdr' of the `car' of the `cdr' of the `car' of X."
  (cdr (car (cdr (car x)))))

(defun cdaddr (x)
  "Return the `cdr' of the `car' of the `cdr' of the `cdr' of X."
  (cdr (car (cdr (cdr x)))))

(defun cddaar (x)
  "Return the `cdr' of the `cdr' of the `car' of the `car' of X."
  (cdr (cdr (car (car x)))))

(defun cddadr (x)
  "Return the `cdr' of the `cdr' of the `car' of the `cdr' of X."
  (cdr (cdr (car (cdr x)))))

(defun cdddar (x)
  "Return the `cdr' of the `cdr' of the `cdr' of the `car' of X."
  (cdr (cdr (cdr (car x)))))

(defun cddddr (x)
  "Return the `cdr' of the `cdr' of the `cdr' of the `cdr' of X."
  (cdr (cdr (cdr (cdr x)))))

;;; `last' is implemented as a C primitive, as of 1998-11

;(defun last (x &optional n)
;  "Return the last link in the list LIST.
;With optional argument N, return Nth-to-last link (default 1)."
;  (if n
;      (let ((m 0) (p x))
;	(while (consp p) (incf m) (pop p))
;	(if (<= n 0) p
;	  (if (< n m) (nthcdr (- m n) x) x)))
;    (while (consp (cdr x)) (pop x))
;    x))

;;; `butlast'  is implemented as a C primitive, as of 1998-11
;;; `nbutlast' is implemented as a C primitive, as of 1998-11

;(defun butlast (x &optional n)
;  "Return a copy of LIST with the last N elements removed."
;  (if (and n (<= n 0)) x
;    (nbutlast (copy-sequence x) n)))

;(defun nbutlast (x &optional n)
;  "Modify LIST to remove the last N elements."
;  (let ((m (length x)))
;    (or n (setq n 1))
;    (and (< n m)
;	 (progn
;	   (if (> n 0) (setcdr (nthcdr (- (1- m) n) x) nil))
;	   x))))

(defun list* (arg &rest rest)   ; See compiler macro in cl-macs.el
  "Return a new list with specified args as elements, cons'd to last arg.
Thus, `(list* A B C D)' is equivalent to `(nconc (list A B C) D)', or to
`(cons A (cons B (cons C D)))'."
  (cond ((not rest) arg)
	((not (cdr rest)) (cons arg (car rest)))
	(t (let* ((n (length rest))
		  (copy (copy-sequence rest))
		  (last (nthcdr (- n 2) copy)))
	     (setcdr last (car (cdr last)))
	     (cons arg copy)))))

(defun ldiff (list sublist)
  "Return a copy of LIST with the tail SUBLIST removed."
  (let ((res nil))
    (while (and (consp list) (not (eq list sublist)))
      (push (pop list) res))
    (nreverse res)))

;;; `copy-list' is implemented as a C primitive, as of 1998-11

;(defun copy-list (list)
;  "Return a copy of a list, which may be a dotted list.
;The elements of the list are not copied, just the list structure itself."
;  (if (consp list)
;      (let ((res nil))
;	(while (consp list) (push (pop list) res))
;	(prog1 (nreverse res) (setcdr res list)))
;    (car list)))

(defun cl-maclisp-member (item list)
  (while (and list (not (equal item (car list)))) (setq list (cdr list)))
  list)

;;; Define an Emacs 19-compatible `member' for the benefit of Emacs 18 users.
(or (and (fboundp 'member) (subrp (symbol-function 'member)))
    (defalias 'member 'cl-maclisp-member))

(defalias 'cl-member 'memq)   ; for compatibility with old CL package
(defalias 'cl-floor 'floor*)
(defalias 'cl-ceiling 'ceiling*)
(defalias 'cl-truncate 'truncate*)
(defalias 'cl-round 'round*)
(defalias 'cl-mod 'mod*)

(defun adjoin (cl-item cl-list &rest cl-keys)  ; See compiler macro in cl-macs
  "Return ITEM consed onto the front of LIST only if it's not already there.
Otherwise, return LIST unmodified.
Keywords supported:  :test :test-not :key"
  (cond ((or (equal cl-keys '(:test eq))
	     (and (null cl-keys) (not (numberp cl-item))))
	 (if (memq cl-item cl-list) cl-list (cons cl-item cl-list)))
	((or (equal cl-keys '(:test equal)) (null cl-keys))
	 (if (member cl-item cl-list) cl-list (cons cl-item cl-list)))
	(t (apply 'cl-adjoin cl-item cl-list cl-keys))))

(defun subst (cl-new cl-old cl-tree &rest cl-keys)
  "Substitute NEW for OLD everywhere in TREE (non-destructively).
Return a copy of TREE with all elements `eql' to OLD replaced by NEW.
Keywords supported:  :test :test-not :key"
  (if (or cl-keys (and (numberp cl-old) (not (integerp cl-old))))
      (apply 'sublis (list (cons cl-old cl-new)) cl-tree cl-keys)
    (cl-do-subst cl-new cl-old cl-tree)))

(defun cl-do-subst (cl-new cl-old cl-tree)
  (cond ((eq cl-tree cl-old) cl-new)
	((consp cl-tree)
	 (let ((a (cl-do-subst cl-new cl-old (car cl-tree)))
	       (d (cl-do-subst cl-new cl-old (cdr cl-tree))))
	   (if (and (eq a (car cl-tree)) (eq d (cdr cl-tree)))
	       cl-tree (cons a d))))
	(t cl-tree)))

(defun acons (a b c)
  "Return a new alist created by adding (KEY . VALUE) to ALIST."
  (cons (cons a b) c))

(defun pairlis (a b &optional c) (nconc (mapcar* 'cons a b) c))


;;; Miscellaneous.

;; XEmacs change
(define-error 'cl-assertion-failed "Assertion failed")

;;; This is defined in Emacs 19; define it here for Emacs 18 users.
(defun cl-add-hook (hook func &optional append)
  "Add to hook variable HOOK the function FUNC.
FUNC is not added if it already appears on the list stored in HOOK."
  (let ((old (and (boundp hook) (symbol-value hook))))
    (and (listp old) (not (eq (car old) 'lambda))
	 (setq old (list old)))
    (and (not (member func old))
	 (set hook (if append (nconc old (list func)) (cons func old))))))
(or (fboundp 'add-hook) (defalias 'add-hook 'cl-add-hook))

;; XEmacs change
;(load "cl-defs")

;;; Define data for indentation and edebug.
(mapcar
 #'(lambda (entry)
     (mapcar
      #'(lambda (func)
	  (put func 'lisp-indent-function (nth 1 entry))
	  (put func 'lisp-indent-hook (nth 1 entry))
	  (or (get func 'edebug-form-spec)
	      (put func 'edebug-form-spec (nth 2 entry))))
      (car entry)))
 '(((defun* defmacro*) defun)
   ((function*) nil
    (&or symbolp ([&optional 'macro] 'lambda (&rest sexp) &rest form)))
   ((eval-when) 1 (sexp &rest form))
   ((when unless) 1 (&rest form))
   ((declare) nil (&rest sexp))
   ((the) 1 (sexp &rest form))
   ((case ecase typecase etypecase) 1 (form &rest (sexp &rest form)))
   ((block return-from) 1 (sexp &rest form))
   ((return) nil (&optional form))
   ((do do*) 2 ((&rest &or symbolp (symbolp &optional form form))
		(form &rest form)
		&rest form))
   ((dolist dotimes) 1 ((symbolp form &rest form) &rest form))
   ((do-symbols) 1 ((symbolp form &optional form form) &rest form))
   ((do-all-symbols) 1 ((symbolp form &optional form) &rest form))
   ((psetq setf psetf) nil edebug-setq-form)
   ((progv) 2 (&rest form))
   ((flet labels macrolet) 1
    ((&rest (sexp sexp &rest form)) &rest form))
   ((symbol-macrolet lexical-let lexical-let*) 1
    ((&rest &or symbolp (symbolp form)) &rest form))
   ((multiple-value-bind) 2 ((&rest symbolp) &rest form))
   ((multiple-value-setq) 1 ((&rest symbolp) &rest form))
   ((incf decf remf pop push pushnew shiftf rotatef) nil (&rest form))
   ((letf letf*) 1 ((&rest (&rest form)) &rest form))
   ((callf destructuring-bind) 2 (sexp form &rest form))
   ((callf2) 3 (sexp form form &rest form))
   ((loop) defun (&rest &or symbolp form))
   ((ignore-errors) 0 (&rest form))))


;;; This goes here so that cl-macs can find it if it loads right now.
(provide 'cl-19)     ; usage: (require 'cl-19 "cl")


;;; Things to do after byte-compiler is loaded.
;;; As a side effect, we cause cl-macs to be loaded when compiling, so
;;; that the compiler-macros defined there will be present.

(defvar cl-hacked-flag nil)
(defun cl-hack-byte-compiler ()
  (if (and (not cl-hacked-flag) (fboundp 'byte-compile-file-form))
      (progn
	(when (not (fboundp 'cl-compile-time-init))
	  (load "cl-macs" nil t))
	(cl-compile-time-init)   ; in cl-macs.el
	(setq cl-hacked-flag t))))

;;; Try it now in case the compiler has already been loaded.
(cl-hack-byte-compiler)

;;; Also make a hook in case compiler is loaded after this file.
;;; The compiler doesn't call any hooks when it loads or runs, but
;;; we can take advantage of the fact that emacs-lisp-mode will be
;;; called when the compiler reads in the file to be compiled.
;;; BUG: If the first compilation is `byte-compile' rather than
;;; `byte-compile-file', we lose.  Oh, well.
(add-hook 'emacs-lisp-mode-hook 'cl-hack-byte-compiler)


;;; The following ensures that packages which expect the old-style cl.el
;;; will be happy with this one.

(provide 'cl)

(provide 'mini-cl)   ; for Epoch

(run-hooks 'cl-load-hook)

;;; cl.el ends here
