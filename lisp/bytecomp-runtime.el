;;; bytecomp-runtime.el --- byte-compiler support for inlining

;; Copyright (C) 1992, 1997 Free Software Foundation, Inc.
;; Copyright (C) 2002 Ben Wing.

;; Author: Jamie Zawinski <jwz@jwz.org>
;; Author: Hallvard Furuseth <hbf@ulrik.uio.no>
;; Maintainer: SXEmacs Development Team
;; Keywords: internal, dumped

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

;;; Synched up with: FSF 19.30.

;;; Commentary:

;; This file is dumped with SXEmacs.

;; The code in this file should always be loaded, because it defines things
;; like "defsubst" which should work interpreted as well.  The code in
;; bytecomp.el and byte-optimize.el can be loaded as needed.

;; interface to selectively inlining functions.
;; This only happens when source-code optimization is turned on.

;;; Code:

;; Redefined in byte-optimize.el.
;; This is not documented--it's not clear that we should promote it.
(fset 'inline 'progn)
(put 'inline 'lisp-indent-hook 0)


;;; Interface to inline functions.

;; FSF comments the next two out, but I see no reason to do so. --ben
(defmacro proclaim-inline (&rest fns)
  "Cause the named functions to be open-coded when called from compiled code.
They will only be compiled open-coded when `byte-optimize' is true."
  (cons 'eval-and-compile
	(apply
	 'nconc
	 (mapcar
	  #'(lambda (x)
	      `((or (memq (get ',x 'byte-optimizer)
			  '(nil byte-compile-inline-expand))
		    (error
		     "%s already has a byte-optimizer, can't make it inline"
		     ',x))
		(put ',x 'byte-optimizer 'byte-compile-inline-expand)))
	  fns))))


(defmacro proclaim-notinline (&rest fns)
  "Cause the named functions to no longer be open-coded."
  (cons 'eval-and-compile
	(apply
	 'nconc
	 (mapcar
	  #'(lambda (x)
	      `((if (eq (get ',x 'byte-optimizer)
			'byte-compile-inline-expand)
		    (put ',x 'byte-optimizer nil))))
	  fns))))

;; This has a special byte-hunk-handler in bytecomp.el.
(defmacro defsubst (name arglist &rest body)
  "Define an inline function.  The syntax is just like that of `defun'."
  (or (memq (get name 'byte-optimizer)
	    '(nil byte-compile-inline-expand))
      (error "`%s' is a primitive" name))
  (list 'prog1
	(cons 'defun (cons name (cons arglist body)))
	(list 'proclaim-inline name)))
; Instead of the above line, FSF has this:
;	(list 'eval-and-compile
;	      (list 'put (list 'quote name)
;		    ''byte-optimizer ''byte-compile-inline-expand))))

(defun make-obsolete (fn new &optional when)
  "Make the byte-compiler warn that function FN is obsolete.
The warning will say that NEW should be used instead.
If NEW is a string, that is the `use instead' message.
If provided, WHEN should be a string indicating when the function
was first made obsolete, for example a date or a release number."
  (interactive "aMake function obsolete: \nxObsoletion replacement: ")
  (let ((handler (get fn 'byte-compile)))
    (if (eq 'byte-compile-obsolete handler)
	(setcar (get fn 'byte-obsolete-info) new)
      (put fn 'byte-obsolete-info (cons new handler))
      (put fn 'byte-compile 'byte-compile-obsolete)))
  fn)

(defun make-obsolete-variable (var new &optional when)
  "Make the byte-compiler warn that variable VAR is obsolete,
and NEW should be used instead.  If NEW is a string, then that is the
`use instead' message.
If provided, WHEN should be a string indicating when the variable
was first made obsolete, for example a date or a release number."
  (interactive
   (list
    (let ((str (completing-read "Make variable obsolete: " obarray 'boundp t)))
      (if (equal str "") (error ""))
      (intern str))
    (car (read-from-string (read-string "Obsoletion replacement: ")))))
  (put var 'byte-obsolete-variable new)
  var)

;; By overwhelming demand, we separate out truly obsolete symbols from
;; those that are present for GNU Emacs compatibility.
(defun make-compatible (fn new)
  "Make the byte-compiler know that function FN is provided for compatibility.
The warning will say that NEW should be used instead.
If NEW is a string, that is the `use instead' message."
  (interactive "aMake function compatible: \nxCompatible replacement: ")
  (let ((handler (get fn 'byte-compile)))
    (if (eq 'byte-compile-compatible handler)
	(setcar (get fn 'byte-compatible-info) new)
      (put fn 'byte-compatible-info (cons new handler))
      (put fn 'byte-compile 'byte-compile-compatible)))
  fn)

(defun make-compatible-variable (var new)
  "Make the byte-compiler know that variable VAR is provided for compatibility,
and NEW should be used instead.  If NEW is a string, then that is the
`use instead' message."
  (interactive
   (list
    (let ((str (completing-read "Make variable compatible: "
				obarray 'boundp t)))
      (if (equal str "") (error ""))
      (intern str))
    (car (read-from-string (read-string "Compatible replacement: ")))))
  (put var 'byte-compatible-variable new)
  var)

(put 'dont-compile 'lisp-indent-hook 0)
(defmacro dont-compile (&rest body)
  "Like `progn', but the body always runs interpreted (not compiled).
If you think you need this, you're probably making a mistake somewhere."
  (list 'eval (list 'quote (if (cdr body) (cons 'progn body) (car body)))))


;;; interface to evaluating things at compile time and/or load time
;;; these macro must come after any uses of them in this file, as their
;;; definition in the file overrides the magic definitions on the
;;; byte-compile-macro-environment.

(put 'eval-when-compile 'lisp-indent-hook 0)
(defmacro eval-when-compile (&rest body)
  "Like `progn', but evaluates the body at compile time.
The result of the body appears to the compiler as a quoted constant."
  ;; Not necessary because we have it in b-c-initial-macro-environment
  ;; (list 'quote (eval (cons 'progn body)))
  (cons 'progn body))

(put 'eval-and-compile 'lisp-indent-hook 0)
(defmacro eval-and-compile (&rest body)
  "Like `progn', but evaluates the body at compile time and at load time."
  ;; Remember, it's magic.
  (cons 'progn body))

;;; From Emacs 20.
(put 'eval-when-feature 'lisp-indent-hook 1)
(defmacro eval-when-feature (feature &rest body)
  "Run the body forms when FEATURE is featurep, be it now or later.
Called (eval-when-feature (FEATURE [. FILENAME]) BODYFORMS...).
If (featurep 'FEATURE), evals now; otherwise adds an elt to
`after-load-alist' (which see), using FEATURE as filename if FILENAME is nil."
  (let ((file (or (cdr feature) (symbol-name (car feature)))))
    `(let ((bodythunk #'(lambda () ,@body)))
       (if (featurep ',(car feature))
	   (funcall bodythunk)
	 (setq after-load-alist (cons '(,file . (list 'lambda '() bodythunk))
				      after-load-alist))))))


;;; Functions to cleanly eliminate warnings about undefined functions
;;; or variables when the code knows what it's doing.  These macros DO
;;; NOT rely on any byte-compiler changes, and thus can be copied into
;;; a package and used within it.

;; NOTE: As a result of the above requirement, the macros rely on
;; "tricks" to get the warnings suppressed.  A cleaner way, of course,
;; would be to extend the byte compiler to provide a proper interface.

;; #### Should we require an unquoted symbol rather than a quoted one,
;; as we currently do?  The quoting gets no generality, as `eval' is
;; called at compile time.  But most functions and macros want quoted
;; arguments, and I find it extremely confusing to deal with cases
;; such as `throw' requiring a quoted argument but `block' an unquoted
;; one.

(put 'with-boundp 'lisp-indent-function 1)
(defmacro with-boundp (variables &rest body)
  "Evaluate BODY, but do not issue bytecomp warnings about VARIABLES undefined.
VARIABLES can be a symbol or a list of symbols and must be quoted.  When
compiling this file, the warnings `reference to free variable VARIABLE' and
`assignment to free variable VARIABLE' will not occur anywhere in BODY, for
any of the listed variables.  This is a clean way to avoid such warnings.

See also `if-boundp', `when-boundp', and `and-boundp' (ways to
conditionalize on a variable being bound and avoid warnings),
`declare-boundp' (issue a variable call without warnings), and
`globally-declare-boundp' (avoid warnings throughout a file about a
variable)."
  (setq variables (eval variables))
  (unless (consp variables)
      (setq variables (list variables)))
  `(progn
     (declare (special ,@variables))
     ,@body))

(put 'if-boundp 'lisp-indent-function 2)
(defmacro if-boundp (variable then &rest else)
  "Equivalent to (if (boundp VARIABLE) THEN ELSE) but handles bytecomp warnings.
VARIABLE should be a quoted symbol.  When compiling this file, the warnings
`reference to free variable VARIABLE' and `assignment to free variable
VARIABLE' will not occur anywhere in the if-statement.  This is a clean way
to avoid such warnings.  See also `with-boundp' and friends."
  `(with-boundp ,variable
     (if (boundp ,variable) ,then ,@else)))

(put 'when-boundp 'lisp-indent-function 1)
(defmacro when-boundp (variable &rest body)
  "Equivalent to (when (boundp VARIABLE) BODY) but handles bytecomp warnings.
VARIABLE should be a quoted symbol.  When compiling this file, the warnings
`reference to free variable VARIABLE' and `assignment to free variable
VARIABLE' will not occur anywhere in the when-statement.  This is a clean
way to avoid such warnings.  See also `with-boundp' and friends."
  `(with-boundp ,variable
     (when (boundp ,variable) ,@body)))

(put 'and-boundp 'lisp-indent-function 1)
(defmacro and-boundp (variable &rest args)
  "Equivalent to (and (boundp VARIABLE) ARGS) but handles bytecomp warnings.
VARIABLE should be a quoted symbol.  When compiling this file, the warnings
`reference to free variable VARIABLE' and `assignment to free variable
VARIABLE' will not occur anywhere in the and-statement.  This is a clean
way to avoid such warnings.  See also `with-boundp' and friends."
  `(with-boundp ,variable
     (and (boundp ,variable) ,@args)))

(defmacro declare-boundp (variable)
  "Evaluate VARIABLE without bytecomp warnings about the symbol.

Sample usage is

  (declare-boundp gpm-minor-mode)

which is equivalent to

  (with-boundp 'gpm-minor-mode
    gpm-minor-mode)

See also `with-boundp' and friends."
  `(with-boundp ',variable ,variable))

(defmacro globally-declare-boundp (variables)
  "Declare that all free uses of VARIABLES in this file are valid.
VARIABLES can be a symbol or a list of symbols and must be quoted.

When compiling this file, the warnings `reference to free variable
VARIABLE' and `assignment to free variable VARIABLE' will not occur
regardless of where references to VARIABLE occur in the file.

In general, you should *NOT* use this; use `with-boundp' or its friends to
wrap individual uses, as necessary.  That way, you're more likely to
remember to put in the explicit checks for the variable's existence that
are usually necessary.  However, `globally-declare-boundp' is better in
some circumstances, such as when writing an ELisp package that makes
integral use of optionally-compiled-in functionality (typically, an
interface onto a system library) and checks for the existence of the
functionality at some entry point to the package.  See
`globally-declare-fboundp' for more information."
  (setq variables (eval variables))
  (if (not (consp variables))
      (setq variables (list variables)))
  `(progn
     ;; (defvar FOO) has no side effects.
     ,@(mapcar #'(lambda (sym) `(defvar ,sym)) variables)))

(defun byte-compile-with-fboundp (form)
  (byte-compile-form (cons 'progn (cdr (cdr form))))
  ;; Unfortunately, byte-compile-unresolved-functions is used not only
  ;; for unresolved-function warnings, but also in connection with the
  ;; following warnings:

  ;; "defsubst %s was used before it was defined"
  ;; "%s being defined to take %s%s, but was previously called with %s"

  ;; By hacking byte-compile-unresolved-functions like this, we
  ;; effectively disable these warnings.  But code should not be using
  ;; `with-fboundp' with a function defined later on in the same
  ;; file, so this is not a big deal.

  (let ((symbols (eval (car (cdr form)))))
    (unless (consp symbols)
      (setq symbols (list symbols)))
    (setq symbols (mapcar #'(lambda (sym) (cons sym nil)) symbols))
    (setq byte-compile-unresolved-functions
	  (set-difference byte-compile-unresolved-functions symbols
			  :key #'car))
    ))

;; EEEEEEEEVIL hack.  We need to create our own byte-compilation
;; method so that the proper variables are bound while compilation
;; takes place (which is when the warnings get noticed and batched
;; up).  What we really want to do is make `with-fboundp' a macro
;; that simply `progn's its BODY; but GOD DAMN IT, macros can't have
;; their own byte-compilation methods!  So we make `with-fboundp' a
;; macro calling `with-fboundp-1', which is cleverly aliased to
;; progn.  This way we can put a byte-compilation method on
;; `with-fboundp-1', and when interpreting, progn will duly skip
;; the first, quoted argument, i.e. the symbol name. (We could make
;; `with-fboundp-1' a regular function, but then we'd have to thunk
;; BODY and eval it at runtime.  We could probably just do this using
;; (apply 'progn BODY), but the existing method is more obviously
;; guaranteed to work.)
;;
;; In defense, cl-macs.el does a very similar thing with
;; `cl-block-wrapper'.

(put 'with-fboundp-1 'byte-compile 'byte-compile-with-fboundp)
(defalias 'with-fboundp-1 'progn)

(put 'with-fboundp 'lisp-indent-function 1)
(defmacro with-fboundp (functions &rest body)
  "Evaluate BODY, but do not issue bytecomp warnings about FUNCTIONS undefined.
FUNCTIONS can be a symbol or a list of symbols and must be quoted.  When
compiling this file, the warning `the function FUNCTION is not known to be
defined' will not occur anywhere in BODY, for any of the listed functions.
This is a clean way to avoid such warnings.

See also `if-fboundp', `when-fboundp', and `and-fboundp' (ways to
conditionalize on a function being bound and avoid warnings),
`declare-fboundp' (issue a function call without warnings), and
`globally-declare-fboundp' (avoid warnings throughout a file about a
function)."
  `(with-fboundp-1 ,functions ,@body))

(put 'if-fboundp 'lisp-indent-function 2)
(defmacro if-fboundp (function then &rest else)
  "Equivalent to (if (fboundp FUNCTION) THEN ELSE) but handles bytecomp warnings.
FUNCTION should be a quoted symbol.  When compiling this file, the warning
`the function FUNCTION is not known to be defined' will not occur anywhere
in the if-statement.  This is a clean way to avoid such warnings.  See also
`with-fboundp' and friends."
  `(with-fboundp ,function
     (if (fboundp ,function) ,then ,@else)))

(put 'when-fboundp 'lisp-indent-function 1)
(defmacro when-fboundp (function &rest body)
  "Equivalent to (when (fboundp FUNCTION) BODY) but handles bytecomp warnings.
FUNCTION should be a quoted symbol.  When compiling this file, the warning
`the function FUNCTION is not known to be defined' will not occur anywhere
in the when-statement.  This is a clean way to avoid such warnings.  See also
`with-fboundp' and friends."
  `(with-fboundp ,function
     (when (fboundp ,function) ,@body)))

(put 'and-fboundp 'lisp-indent-function 1)
(defmacro and-fboundp (function &rest args)
  "Equivalent to (and (fboundp FUNCTION) ARGS) but handles bytecomp warnings.
FUNCTION should be a quoted symbol.  When compiling this file, the warning
`the function FUNCTION is not known to be defined' will not occur anywhere
in the and-statement.  This is a clean way to avoid such warnings.  See also
`with-fboundp' and friends."
  `(with-fboundp ,function
     (and (fboundp ,function) ,@args)))

(defmacro declare-fboundp (form)
  "Execute FORM (a function call) without bytecomp warnings about the call.
Sample usage is

  (declare-fboundp (x-keysym-on-keyboard-sans-modifiers-p 'backspace))

which is equivalent to

  (with-fboundp 'x-keysym-on-keyboard-sans-modifiers-p
    (x-keysym-on-keyboard-sans-modifiers-p 'backspace))

See also `with-fboundp' and friends."
  `(with-fboundp ',(car form) ,form))

(defmacro globally-declare-fboundp (functions)
  "Declare that all calls to function FUNCTIONS in this file are valid.
FUNCTIONS can be a symbol or a list of symbols and must be quoted.

When compiling this file, the warning `the function FUNCTION is not known
to be defined' will not occur regardless of where calls to FUNCTION occur
in the file.

In general, you should *NOT* use this; use `with-fboundp' or its friends to
wrap individual uses, as necessary.  That way, you're more likely to
remember to put in the explicit checks for the function's existence that
are usually necessary.  However, `globally-declare-fboundp' is better in
some circumstances, such as when writing an ELisp package that makes
integral use of optionally-compiled-in functionality (typically, an
interface onto a system library) and checks for the existence of the
functionality at some entry point to the package.  The file `ldap.el' is a
good example: It provides a layer on top of the optional LDAP ELisp
primitives, makes calls to them throughout its code, and verifies the
presence of LDAP support at load time.  Putting calls to `declare-fboundp'
throughout the code would be a major annoyance."
  (when (cl-compiling-file)
    (setq functions (eval functions))
    (if (not (consp functions))
	(setq functions (list functions)))
    ;; Another hack.  This works because the autoload environment is
    ;; currently used ONLY to suppress warnings, and the actual
    ;; autoload definition is not used. (NOTE: With this definition,
    ;; we will get spurious "multiple autoloads for %s" warnings if we
    ;; have an autoload later in the file for any functions in FUNCTIONS.
    ;; This is not something that code should ever do, though.)
    (setq byte-compile-autoload-environment
	  (append (mapcar #'(lambda (sym) (cons sym nil)) functions)
		  byte-compile-autoload-environment)))
  nil)

(defun byte-compile-with-byte-compiler-warnings-suppressed (form)
  (let ((byte-compile-warnings byte-compile-warnings)
	(types (car (cdr form))))
    (unless (consp types)
      (setq types (list types)))
    (if (eq byte-compile-warnings t)
	(setq byte-compile-warnings byte-compile-default-warnings))
    (setq byte-compile-warnings (set-difference byte-compile-warnings types))
    (byte-compile-form (cons 'progn (cdr (cdr form))))))

;; Same hack here as with `with-fboundp'.
(put 'with-byte-compiler-warnings-suppressed-1 'byte-compile
     'byte-compile-with-byte-compiler-warnings-suppressed)
(defalias 'with-byte-compiler-warnings-suppressed-1 'progn)

(put 'with-byte-compiler-warnings-suppressed 'lisp-indent-function 1)
(defmacro with-byte-compiler-warnings-suppressed (type &rest body)
  "Evaluate BODY, but do not issue bytecomp warnings TYPE.
TYPE should be one of `redefine', `callargs', `subr-callargs',
`free-vars', `unresolved', `unused-vars', `obsolete', or `pedantic',
or a list of one or more of these symbols. (See `byte-compile-warnings'.)
TYPE must be quoted.

NOTE: You should *NOT* under normal circumstances be using this!
There are better ways of avoiding most of these warnings.  In particular:

-- use (declare (special ...)) if you are making use of
   dynamically-scoped variables.
-- use `with-fboundp' and friends to avoid warnings about undefined functions
   when you know the function actually exists.
-- use `with-boundp' and friends to avoid warnings about undefined variables
   when you know the variable actually exists.
-- use `with-obsolete-variable' or `with-obsolete-function' if you
   are purposely using such a variable or function."
  `(with-byte-compiler-warnings-suppressed-1 ,type ,@body))

;; #### These should be more clever.  You could (e.g.) try fletting
;; `byte-compile-obsolete' or temporarily removing the obsolete info
;; from the symbol and putting it back with an unwind-protect. (Or
;; better, modify the byte-compiler to provide a proper solution, and
;; fix these macros to use it if available, or fall back on the way
;; below.  Remember, these definitions need to work with an unchanged
;; byte compiler so that they can be copied and used in packages.)

(put 'with-obsolete-variable 'lisp-indent-function 1)
(defmacro with-obsolete-variable (symbol &rest body)
  "Evaluate BODY but do not warn about usage of obsolete variable SYMBOL.
SYMBOL must be quoted and can be a list of SYMBOLS.  See also
`with-obsolete-function'."
  `(with-byte-compiler-warnings-suppressed 'obsolete ,@body))

(put 'with-obsolete-function 'lisp-indent-function 1)
(defmacro with-obsolete-function (symbol &rest body)
  "Evaluate BODY but do not warn about usage of obsolete function SYMBOL.
SYMBOL must be quoted and can be a list of SYMBOLS.  See also
`with-obsolete-variable'."
  `(with-byte-compiler-warnings-suppressed 'obsolete ,@body))


;;; Interface to file-local byte-compiler parameters.
;;; Redefined in bytecomp.el.

;;; The great RMS speaketh:
;;;
;;; I nuked this because it's not a good idea for users to think of
;;; using it.  These options are a matter of installation preference,
;;; and have nothing to do with particular source files; it's a
;;; mistake to suggest to users that they should associate these with
;;; particular source files.  There is hardly any reason to change
;;; these parameters, anyway.  --rms.
;;;
;;; But I'll leave this stuff alone. --ben

(put 'byte-compiler-options 'lisp-indent-hook 0)
(defmacro byte-compiler-options (&rest args)
  "Set some compilation-parameters for this file.
This will affect only the file in which it appears; this does nothing when
evaluated, or when loaded from a .el file.

Each argument to this macro must be a list of a key and a value.

  Keys:		  Values:		Corresponding variable:

  verbose	  t, nil		byte-compile-verbose
  optimize	  t, nil, source, byte	byte-optimize
  warnings	  list of warnings	byte-compile-warnings
  file-format	  emacs19, emacs20	byte-compile-emacs19-compatibility

The value specified with the `warnings' option must be a list, containing
some subset of the following flags:

  free-vars	references to variables not in the current lexical scope.
  unused-vars	references to non-global variables bound but not referenced.
  unresolved	calls to unknown functions.
  callargs	lambda calls with args that don't match the definition.
  subr-callargs	calls to subrs with args that don't match the definition.
  redefine	function cell redefined from a macro to a lambda or vice
		versa, or redefined to take a different number of arguments.
  obsolete	use of an obsolete function or variable.
  pedantic	warn of use of compatible symbols.

If the first element if the list is `+' or `-' then the specified elements
are added to or removed from the current set of warnings, instead of the
entire set of warnings being overwritten.

For example, something like this might appear at the top of a source file:

    (byte-compiler-options
      (optimize t)
      (warnings (- callargs))		; Don't warn about arglist mismatch
      (warnings (+ unused-vars))	; Do warn about unused bindings
      (file-format emacs19))"
  nil)

(provide 'bytecomp-runtime)

;; XEmacs 21.4 gets the `if-fboundp' and friends macros from a library
;; in the packages, `bytedecl.el'.  This provide here is to prevent
;; that library from overwriting our macros. --SY.
(provide 'bytedecl)

;;; bytecomp-runtime.el ends here
