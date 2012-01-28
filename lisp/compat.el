;;; compat.el --- Mechanism for non-intrusively providing compatibility funs.

;; Copyright (C) 2000 Ben Wing.

;; Author: Ben Wing <ben@xemacs.org>
;; Maintainer: Ben Wing
;; Keywords: internal

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

;;; Authorship:

; Written May 2000 by Ben Wing.

;;; Commentary:

;; Typical usage:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Wrap modules that define compatibility functions like this:     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(compat-define-group 'fsf-compat)

;(compat-define-functions 'fsf-compat

;(defun overlayp (object)
;  "Return t if OBJECT is an overlay."
;  (and (extentp object)
;       (extent-property object 'overlay)))

;(defun make-overlay (beg end &optional buffer front-advance rear-advance)
;  ...)

;...

;) ;; end of (compat-define-group 'fsf-compat)

;;;; overlay.el ends here


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Wrap modules that use the compatibility functions like this:    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(compat 'fsf-compat

;(defun random-module-my-fun (bar baz)
;  (if (fboundp 'overlays-in) (overlays-in bar baz)))

;...

;) ;; end of (compat 'fsf-compat)

;;;; random-module.el ends here


(defun compat-hash-table (group)
  (get group 'compat-table))

(defun compat-make-hash-table (group)
  (put group 'compat-table (make-hash-table)))

(defmacro compat-define-group (group)
  "Define GROUP as a group of compatibility functions.
Individual functions are defined using `compat-define-functions'.
Once defined, the functions can be used by wrapping your code in the
`compat' macro.

If GROUP is already defined, nothing happens."
  (let ((group (eval group)))
    (or (hash-table-p (compat-hash-table group))
	(compat-make-hash-table group))))

(defmacro compat-clear-functions (group)
  "Clear all defined functions and macros out of GROUP."
  (let ((group (eval group)))
    (clrhash (compat-hash-table group))))

(defmacro compat-define-functions (group &rest body)
  "Define compatibility functions in GROUP.
You should simply wrap this around the code that defines the functions.
Any functions and macros defined at top level using `defun' or `defmacro'
will be noticed and added to GROUP.  Other top-level code will be executed
normally.  All code and definitions in this group can safely reference any
other functions in this group -- the code is effectively wrapped in a
`compat' call.  You can call `compat-define-functions' more than once, if
necessary, for a single group.

What actually happens is that the functions and macros defined here are in
fact defined using names prefixed with GROUP.  To use these functions,
wrap any calling code with the `compat' macro, which lexically renames
the function and macro calls appropriately."
  (let ((group (eval group)))
    (let (fundef
	  (body-tail body))
      (while body-tail
	(setq fundef (car body-tail))
	(when (and (consp fundef) (eq (car fundef) 'defun))
	  (puthash (second fundef) (third fundef) (compat-hash-table group)))
	(when (and (consp fundef) (eq (car fundef) 'defmacro))
	  (puthash (second fundef) (third fundef) (compat-hash-table group)))
	(setq body-tail (cdr body-tail))))
    (let (fundef
	  (body-tail body)
	  result)
      (while body-tail
	(setq fundef (car body-tail))
	(push
	 (cond ((and (consp fundef) (eq (car fundef) 'defun))
		(nconc (list 'defun
			      (intern (concat (symbol-name group) "-"
					      (symbol-name (second fundef))))
			      (third fundef))
			(nthcdr 3 fundef)))
	       ((and (consp fundef) (eq (car fundef) 'defmacro))
		(nconc (list 'defmacro
			      (intern (concat (symbol-name group) "-"
					      (symbol-name (second fundef))))
			      (third fundef))
			(nthcdr 3 fundef)))
	       (t fundef))
	 result)
	(setq body-tail (cdr body-tail)))
      (nconc (list 'compat (list 'quote group)) (nreverse result)))))

(defvar compat-active-groups nil)

(defun compat-fboundp (groups fun)
  "T if FUN is either `fboundp' or one of the compatibility funs in GROUPS.
GROUPS is a list of compatibility groups as defined using
`compat-define-group'."
  (or (fboundp fun)
      (block nil
	(mapcar #'(lambda (group)
		    (if (gethash fun (compat-hash-table group))
			(return t)))
		groups))))

(defmacro compat (group &rest body)
  "Make use of compatibility functions and macros in GROUP.
You should simply wrap this around the code that uses the functions
and macros in GROUP.  Typically, a call to `compat' should be placed
at the top of an ELisp module, with the closing parenthesis at the
bottom; use this in place of a `require' statement.  Wrapped code can
be either function or macro definitions or other ELisp code, and
wrapped function or macro definitions need not be at top level.  All
calls to the compatibility functions or macros will be noticed anywhere
within the wrapped code.  Calls to `fboundp' within the wrapped code
will also behave correctly when called on compatibility functions and
macros, even though they would return nil elsewhere (including in code
in other modules called dynamically from the wrapped code).

The functions and macros define in GROUP are actually defined under
prefixed names, to avoid namespace clashes and bad interactions with
other code that calls `fboundp'.  All calls inside of the wrapped code
to the compatibility functions and macros in GROUP are lexically
mapped to the prefixed names.  Since this is a lexical mapping, code
in other modules that is called by functions in this module will not
be affected."
  (let ((group (eval group))
	defs)
    (maphash
     #'(lambda (fun args)
	 (push
	  (list fun args
		(nconc
		 (list 'list
		       (list 'quote
			     (intern (concat (symbol-name group) "-"
					     (symbol-name fun)))))
		 args))
	  defs))
     (compat-hash-table group))
    ;; it would be cleaner to use `lexical-let' instead of `let', but that
    ;; causes function definitions to have obnoxious, unreadable junk in
    ;; them.  #### Move `lexical-let' into C!!!
    `(let ((compat-active-groups (cons ',group compat-active-groups)))
       (macrolet ((fboundp (fun) `(compat-fboundp ',compat-active-groups ,fun))
		  ,@defs)
	 ,@body))))
