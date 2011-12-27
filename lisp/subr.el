;;; subr.el --- basic lisp subroutines for SXEmacs

;; Copyright (C) 1985, 86, 92, 94, 95, 99, 2000, 2001, 2002, 2003
;;   Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 2000, 2001, 2002, 2003 Ben Wing.

;; Maintainer: SXEmacs Development Team
;; Keywords: extensions, dumped, internal

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

;;; Synched up with: FSF 19.34.  Some things synched up with later versions.

;;; Commentary:

;; This file is dumped with SXEmacs.

;; There's not a whole lot in common now with the FSF version,
;; be wary when applying differences.  I've left in a number of lines
;; of commentary just to give diff(1) something to synch itself with to
;; provide useful context diffs. -sb

;; BEGIN SYNCHED WITH FSF 21.2

;;; Code:
(defvar custom-declare-variable-list nil
  "Record `defcustom' calls made before `custom.el' is loaded to handle them.
Each element of this list holds the arguments to one call to `defcustom'.")

;; Use this, rather than defcustom, in subr.el and other files loaded
;; before custom.el.  See dumped-lisp.el.
(defun custom-declare-variable-early (&rest arguments)
  (setq custom-declare-variable-list
	(cons arguments custom-declare-variable-list)))


(defun macro-declaration-function (macro decl)
  "Process a declaration found in a macro definition.
This is set as the value of the variable `macro-declaration-function'.
MACRO is the name of the macro being defined.
DECL is a list `(declare ...)' containing the declarations.
The return value of this function is not used."
  (dolist (d (cdr decl))
    (cond ((and (consp d) (eq (car d) 'indent))
	   (put macro 'lisp-indent-function (cadr d)))
	  ((and (consp d) (eq (car d) 'debug))
	   (put macro 'edebug-form-spec (cadr d)))
	  (t
	   (message "Unknown declaration %s" d)))))

(setq macro-declaration-function 'macro-declaration-function)


;;;; Lisp language features.

(defmacro lambda (&rest cdr)
  "Return a lambda expression.
A call of the form (lambda ARGS DOCSTRING INTERACTIVE BODY) is
self-quoting; the result of evaluating the lambda expression is the
expression itself.  The lambda expression may then be treated as a
function, i.e., stored as the function value of a symbol, passed to
funcall or mapcar, etc.

ARGS should take the same form as an argument list for a `defun'.
Optional DOCSTRING is a documentation string.
If present, it should describe how to call the function.  Docstrings are
rarely useful unless the lambda will be named, eg, using `fset'.
Optional INTERACTIVE should be a call to the function `interactive'.
BODY should be a list of lisp expressions.

The byte-compiler treats lambda expressions specially.  If the lambda
expression is syntactically a function to be called, it will be compiled
unless protected by `quote'.  Conversely, quoting a lambda expression with
`function' hints to the byte-compiler that it should compile the expression.
\(The byte-compiler may or may not actually compile it; for example it will
never compile lambdas nested in a data structure: `'(#'(lambda (x) x))').

The byte-compiler will warn about common problems such as the form
`(fset 'f '(lambda (x) x))' (the lambda cannot be byte-compiled; probably
the programmer intended `#'', although leaving the lambda unquoted will
normally suffice), but in general is it the programmer's responsibility to
quote lambda expressions appropriately."
  `(function (lambda ,@cdr)))

;; FSF 21.2 has various basic macros here.  We don't because they're either
;; in cl*.el (which we dump and hence is always available) or built-in.

;; More powerful versions in cl.el.
;(defmacro push (newelt listname)
;(defmacro pop (listname)

;; Built-in.
;(defmacro when (cond &rest body)
;(defmacro unless (cond &rest body)

;; More powerful versions in cl-macs.el.
;(defmacro dolist (spec &rest body)
;(defmacro dotimes (spec &rest body)

;; In cl.el.  Ours are defun, but cl arranges for them to be inlined anyway.
;(defsubst caar (x)
;(defsubst cadr (x)
;(defsubst cdar (x)
;(defsubst cddr (x)

;; Built-in.  Our `last' is more powerful in that it handles circularity.
;(defun last (x &optional n)
;(defun butlast (x &optional n)
;(defun nbutlast (x &optional n)

;; In cl-seq.el.
;(defun remove (elt seq)
;(defun remq (elt list)

(defmacro defun-when-void (&rest args)
  "Define a function, just like `defun', unless it's already defined.
Used for compatibility among different emacs variants."
  `(if (fboundp ',(car args))
       nil
     (defun ,@args)))

(defmacro define-function-when-void (&rest args)
  "Define a function, just like `define-function', unless it's already defined.
Used for compatibility among different emacs variants."
  `(if (fboundp ,(car args))
       nil
     (define-function ,@args)))


(defun assoc-default (key alist &optional test default)
  "Find object KEY in a pseudo-alist ALIST.
ALIST is a list of conses or objects.  Each element (or the element's car,
if it is a cons) is compared with KEY by evaluating (TEST (car elt) KEY).
If that is non-nil, the element matches;
then `assoc-default' returns the element's cdr, if it is a cons,
or DEFAULT if the element is not a cons.

If no element matches, the value is nil.
If TEST is omitted or nil, `equal' is used."
  (let (found (tail alist) value)
    (while (and tail (not found))
      (let ((elt (car tail)))
	(when (funcall (or test 'equal) (if (consp elt) (car elt) elt) key)
	  (setq found t value (if (consp elt) (cdr elt) default))))
      (setq tail (cdr tail)))
    value))

(defun assoc-ignore-case (key alist)
  "Like `assoc', but ignores differences in case and text representation.
KEY must be a string.  Upper-case and lower-case letters are treated as equal."
  (let (element)
    (while (and alist (not element))
      (if (string= (downcase key) (downcase (car (car alist))))
	  (setq element (car alist)))
      (setq alist (cdr alist)))
    element))

(defun assoc-ignore-representation (key alist)
  "Like `assoc', but ignores differences in text representation.
KEY must be a string."
  (let (element)
    (while (and alist (not element))
      (if (string= key (car (car alist)))
	  (setq element (car alist)))
      (setq alist (cdr alist)))
    element))

(defun member-ignore-case (elt list)
  "Like `member', but ignores differences in case and text representation.
ELT must be a string.  Upper-case and lower-case letters are treated as equal."
  (while (and list (not (string= (downcase elt) (downcase (car list)))))
    (setq list (cdr list)))
  list)


;;;; Keymap support.
;; XEmacs: removed to keymap.el

;;;; The global keymap tree.

;;; global-map, esc-map, and ctl-x-map have their values set up in
;;; keymap.c; we just give them docstrings here.

;;;; Event manipulation functions.

;; XEmacs: This stuff is done in C Code.

;;;; Obsolescent names for functions generally appear elsewhere, in
;;;; obsolete.el or in the files they are related do.  Many very old
;;;; obsolete stuff has been removed entirely (e.g. anything with `dot' in
;;;; place of `point').

; alternate names (not obsolete)
(if (not (fboundp 'mod)) (define-function 'mod '%))
(define-function 'move-marker 'set-marker)
(define-function 'beep 'ding)		; preserve lingual purity
(define-function 'indent-to-column 'indent-to)
(define-function 'backward-delete-char 'delete-backward-char)
(define-function 'search-forward-regexp (symbol-function 're-search-forward))
(define-function 'search-backward-regexp (symbol-function 're-search-backward))
(define-function 'remove-directory 'delete-directory)
(define-function 'set-match-data 'store-match-data)
(define-function 'send-string-to-terminal 'external-debugging-output)

;; XEmacs:
(defun local-variable-if-set-p (sym buffer)
  "Return t if SYM would be local to BUFFER after it is set.
A nil value for BUFFER is *not* the same as (current-buffer), but
can be used to determine whether `make-variable-buffer-local' has been
called on SYM."
  (local-variable-p sym buffer t))


;;;; Hook manipulation functions.

;; (defconst run-hooks 'run-hooks ...)

(defun make-local-hook (hook)
  "Make the hook HOOK local to the current buffer.
The return value is HOOK.

You never need to call this function now that `add-hook' does it for you
if its LOCAL argument is non-nil.

When a hook is local, its local and global values
work in concert: running the hook actually runs all the hook
functions listed in *either* the local value *or* the global value
of the hook variable.

This function works by making `t' a member of the buffer-local value,
which acts as a flag to run the hook functions in the default value as
well.  This works for all normal hooks, but does not work for most
non-normal hooks yet.  We will be changing the callers of non-normal
hooks so that they can handle localness; this has to be done one by
one.

This function does nothing if HOOK is already local in the current
buffer.

Do not use `make-local-variable' to make a hook variable buffer-local."
  (if (local-variable-p hook (current-buffer)) ; XEmacs
      nil
    (or (boundp hook) (set hook nil))
    (make-local-variable hook)
    (set hook (list t)))
  hook)

(defun add-hook (hook function &optional append local)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

The optional fourth argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes the hook buffer-local if needed.
To make a hook variable buffer-local, always use
`make-local-hook', not `make-local-variable'.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions.

You can remove this hook yourself using `remove-hook'.

See also `add-one-shot-hook'."
  (or (boundp hook) (set hook nil))
  (or (default-boundp hook) (set-default hook nil))
  (if local (unless (local-variable-if-set-p hook (current-buffer)) ; XEmacs
	      (make-local-hook hook))
    ;; Detect the case where make-local-variable was used on a hook
    ;; and do what we used to do.
    (unless (and (consp (symbol-value hook)) (memq t (symbol-value hook)))
      (setq local t)))
  (let ((hook-value (if local (symbol-value hook) (default-value hook))))
    ;; If the hook value is a single function, turn it into a list.
    (when (or (not (listp hook-value)) (eq (car hook-value) 'lambda))
      (setq hook-value (list hook-value)))
    ;; Do the actual addition if necessary
    (unless (member function hook-value)
      (setq hook-value
	    (if append
		(append hook-value (list function))
	      (cons function hook-value))))
    ;; Set the actual variable
    (if local (set hook hook-value) (set-default hook hook-value))))

(defun remove-hook (hook function &optional local)
  "Remove from the value of HOOK the function FUNCTION.
HOOK should be a symbol, and FUNCTION may be any valid function.  If
FUNCTION isn't the value of HOOK, or, if FUNCTION doesn't appear in the
list of hooks to run in HOOK, then nothing is done.  See `add-hook'.

The optional third argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes the hook buffer-local if needed.
To make a hook variable buffer-local, always use
`make-local-hook', not `make-local-variable'."
  (or (boundp hook) (set hook nil))
  (or (default-boundp hook) (set-default hook nil))
  (if local (unless (local-variable-if-set-p hook (current-buffer)) ; XEmacs
	      (make-local-hook hook))
    ;; Detect the case where make-local-variable was used on a hook
    ;; and do what we used to do.
    (unless (and (consp (symbol-value hook)) (memq t (symbol-value hook)))
      (setq local t)))
  (let ((hook-value (if local (symbol-value hook) (default-value hook))))
    ;; Remove the function, for both the list and the non-list cases.
    ;; XEmacs: add hook-test, for handling one-shot hooks.
    (flet ((hook-test
	     (fn hel)
	     (or (equal fn hel)
		 (and (symbolp hel)
		      (equal fn
			     (get hel 'one-shot-hook-fun))))))
      (if (or (not (listp hook-value)) (eq (car hook-value) 'lambda))
	  (if (equal hook-value function) (setq hook-value nil))
	(setq hook-value (delete* function (copy-sequence hook-value)
				  :test 'hook-test)))
      ;; If the function is on the global hook, we need to shadow it locally
      ;;(when (and local (member* function (default-value hook)
      ;;                          :test 'hook-test)
      ;;	       (not (member* (cons 'not function) hook-value
      ;;                             :test 'hook-test)))
      ;;  (push (cons 'not function) hook-value))
      ;; Set the actual variable
      (if local (set hook hook-value) (set-default hook hook-value)))))

;; XEmacs addition
;; #### we need a coherent scheme for indicating compatibility info,
;; so that it can be programmatically retrieved.
(defun add-local-hook (hook function &optional append)
  "Add to the local value of HOOK the function FUNCTION.
You don't need this any more.  It's equivalent to specifying the LOCAL
argument to `add-hook'."
  (add-hook hook function append t))

;; XEmacs addition
(defun remove-local-hook (hook function)
  "Remove from the local value of HOOK the function FUNCTION.
You don't need this any more.  It's equivalent to specifying the LOCAL
argument to `remove-hook'."
  (remove-hook hook function t))

(defun add-one-shot-hook (hook function &optional append local)
  "Add to the value of HOOK the one-shot function FUNCTION.
FUNCTION will automatically be removed from the hook the first time
after it runs (whether to completion or to an error).
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions.

You can remove this hook yourself using `remove-hook'.

See also `add-hook'."
  (let ((sym (gensym)))
    (fset sym `(lambda (&rest args)
		 (unwind-protect
		     (apply ',function args)
		   (remove-hook ',hook ',sym ',local))))
    (put sym 'one-shot-hook-fun function)
    (add-hook hook sym append local)))

(defun add-local-one-shot-hook (hook function &optional append)
  "Add to the local value of HOOK the one-shot function FUNCTION.
You don't need this any more.  It's equivalent to specifying the LOCAL
argument to `add-one-shot-hook'."
  (add-one-shot-hook hook function append t))


(defun add-hook-list (function  hook-list &optional append &optional local)
  "FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

The optional fourth argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes the hook buffer-local if needed.
To make a hook variable buffer-local, always use
`make-local-hook', not `make-local-variable'.

HOOK-LIST should be a list of symbols, and FUNCTION may be any valid
function.  If HOOK is void, it is first set to nil.  If HOOK's value
is a single function, it is changed to a list of functions.

You can remove this hook yourself using `remove-hook-list'.

See also `add-hook`, `remove-hook` and `add-one-shot-hook'."
  (mapc (lambda (hook) 
	  (add-hook hook function append local))
	hook-list))

(defun remove-hook-list (function  hook-list &optional local)
  "Remove from the value of all members of HOOK-LIST the function FUNCTION.
HOOK-LIST should be a list of symbols, and FUNCTION may be any valid
function.  If FUNCTION isn't the value of the member of HOOK-LIST, or,
if FUNCTION doesn't appear in the list of hooks to run in HOOK, then
nothing is done.  See `add-hook-list` and `add-hook'.

The optional third argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes the hook buffer-local if needed.
To make a hook variable buffer-local, always use
`make-local-hook', not `make-local-variable'."
  (mapc (lambda (hook) 
	  (remove-hook hook function local))
	hook-list))


(defun add-to-list (list-var element &optional append)
  "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
If ELEMENT is added, it is added at the beginning of the list,
unless the optional argument APPEND is non-nil, in which case
ELEMENT is added at the end.

If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
  (if (member element (symbol-value list-var))
      (symbol-value list-var)
    (set list-var
         (if append
             (append (symbol-value list-var) (list element))
           (cons element (symbol-value list-var))))))

;; END SYNCHED WITH FSF 21.2

;; XEmacs additions

(defun add-to-list* (list-var element &optional append)
    "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
If ELEMENT is added, it is added at the beginning of the list,
unless the optional argument APPEND is non-nil, in which case
ELEMENT is added at the end.

If LIST-VAR is not defined as a variable yet, it will set it to a list
of just the element.

If you want to use `add-to-list*' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list*'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
    (if (boundp list-var)
	(add-to-list list-var element append)
      (set list-var (list element))))

;; called by Fkill_buffer()
(defvar kill-buffer-hook nil
  "Function or functions to be called when a buffer is killed.
The value of this variable may be buffer-local.
The buffer about to be killed is current when this hook is run.")

;; in C in FSFmacs
(defvar kill-emacs-hook nil
  "Function or functions to be called when `kill-emacs' is called,
just before emacs is actually killed.")

;; not obsolete.
;; #### These are a bad idea, because the CL RPLACA and RPLACD
;; return the cons cell, not the new CAR/CDR.         -hniksic
;; The proper definition would be:
;; (defun rplaca (conscell newcar)
;;   (setcar conscell newcar)
;;   conscell)
;; ...and analogously for RPLACD.
(define-function 'rplaca 'setcar)
(define-function 'rplacd 'setcdr)

(defun copy-symbol (symbol &optional copy-properties)
  "Return a new uninterned symbol with the same name as SYMBOL.
If COPY-PROPERTIES is non-nil, the new symbol will have a copy of
SYMBOL's value, function, and property lists."
  (let ((new (make-symbol (symbol-name symbol))))
    (when copy-properties
      ;; This will not copy SYMBOL's chain of forwarding objects, but
      ;; I think that's OK.  Callers should not expect such magic to
      ;; keep working in the copy in the first place.
      (and (boundp symbol)
	   (set new (symbol-value symbol)))
      (and (fboundp symbol)
	   (fset new (symbol-function symbol)))
      (setplist new (copy-list (symbol-plist symbol))))
    new))

(defun set-symbol-value-in-buffer (sym val buffer)
  "Set the value of SYM to VAL in BUFFER.  Useful with buffer-local variables.
If SYM has a buffer-local value in BUFFER, or will have one if set, this
function allows you to set the local value.

NOTE: At some point, this will be moved into C and will be very fast."
  (with-current-buffer buffer
    (set sym val)))


;; BEGIN SYNCHED WITH FSF 21.2

;; #### #### #### AAaargh!  Must be in C, because it is used insanely
;; early in the bootstrap process.
;(defun split-path (path)
;  "Explode a search path into a list of strings.
;The path components are separated with the characters specified
;with `path-separator'."
;  (while (or (not stringp path-separator)
;	     (/= (length path-separator) 1))
;    (setq path-separator (signal 'error (list "\
;`path-separator' should be set to a single-character string"
;					      path-separator))))
;  (split-string-by-char path (aref separator 0)))

(defmacro with-current-buffer (buffer &rest body)
  "Temporarily make BUFFER the current buffer and execute the forms in BODY.
The value returned is the value of the last form in BODY.
See also `with-temp-buffer'."
  `(save-current-buffer
    (set-buffer ,buffer)
    ,@body))

(defmacro with-temp-file (filename &rest forms)
  "Create a new buffer, evaluate FORMS there, and write the buffer to FILENAME.
The value of the last form in FORMS is returned, like `progn'.
See also `with-temp-buffer'."
  (let ((temp-file (make-symbol "temp-file"))
	(temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-file ,filename)
	   (,temp-buffer
	    (get-buffer-create (generate-new-buffer-name " *temp file*"))))
       (unwind-protect
	   (prog1
	       (with-current-buffer ,temp-buffer
		 ,@forms)
	     (with-current-buffer ,temp-buffer
               (widen)
	       (write-region (point-min) (point-max) ,temp-file nil 0)))
	 (and (buffer-name ,temp-buffer)
	      (kill-buffer ,temp-buffer))))))

;; FSF compatibility
(defmacro with-temp-message (message &rest body)
  "Display MESSAGE temporarily while BODY is evaluated.
The original message is restored to the echo area after BODY has finished.
The value returned is the value of the last form in BODY.
If MESSAGE is nil, the echo area and message log buffer are unchanged.
Use a MESSAGE of \"\" to temporarily clear the echo area.

Note that this function exists for FSF compatibility purposes.  A better way
under XEmacs is to give the message a particular label (see `display-message');
then, the old message is automatically restored when you clear your message
with `clear-message'."
;; FSF additional doc string from 21.2:
;; MESSAGE is written to the message log buffer if `message-log-max' is non-nil.
  (let ((current-message (make-symbol "current-message"))
	(temp-message (make-symbol "with-temp-message")))
    `(let ((,temp-message ,message)
	   (,current-message))
       (unwind-protect
	   (progn
	     (when ,temp-message
	       (setq ,current-message (current-message))
	       (message "%s" ,temp-message))
	     ,@body)
	 (and ,temp-message ,current-message
	      (message "%s" ,current-message))))))

(defmacro with-temp-buffer (&rest forms)
  "Create a temporary buffer, and evaluate FORMS there like `progn'.
See also `with-temp-file' and `with-output-to-string'."
  (let ((temp-buffer (make-symbol "temp-buffer")))
    `(let ((,temp-buffer
	    (get-buffer-create (generate-new-buffer-name " *temp*"))))
       (unwind-protect
	   (with-current-buffer ,temp-buffer
	     ,@forms)
	 (and (buffer-name ,temp-buffer)
	      (kill-buffer ,temp-buffer))))))

(defmacro with-output-to-string (&rest body)
  "Execute BODY, return the text it sent to `standard-output', as a string."
  `(let ((standard-output
	  (get-buffer-create (generate-new-buffer-name " *string-output*"))))
     (let ((standard-output standard-output))
       ,@body)
     (with-current-buffer standard-output
       (prog1
	   (buffer-string)
	 (kill-buffer nil)))))

(defmacro with-local-quit (&rest body)
  "Execute BODY with `inhibit-quit' temporarily bound to nil."
  `(condition-case nil
       (let ((inhibit-quit nil))
	 ,@body)
     (quit (setq quit-flag t))))

;; FSF 21.3.

; (defmacro combine-after-change-calls (&rest body)
;   "Execute BODY, but don't call the after-change functions till the end.
; If BODY makes changes in the buffer, they are recorded
; and the functions on `after-change-functions' are called several times
; when BODY is finished.
; The return value is the value of the last form in BODY.

; If `before-change-functions' is non-nil, then calls to the after-change
; functions can't be deferred, so in that case this macro has no effect.

; Do not alter `after-change-functions' or `before-change-functions'
; in BODY."
;   (declare (indent 0) (debug t))
;   `(unwind-protect
;        (let ((combine-after-change-calls t))
; 	 . ,body)
;      (combine-after-change-execute)))


(defvar delay-mode-hooks nil
  "If non-nil, `run-mode-hooks' should delay running the hooks.")
(defvar delayed-mode-hooks nil
  "List of delayed mode hooks waiting to be run.")
(make-variable-buffer-local 'delayed-mode-hooks)
(put 'delay-mode-hooks 'permanent-local t)

(defun run-mode-hooks (&rest hooks)
  "Run mode hooks `delayed-mode-hooks' and HOOKS, or delay HOOKS.
Execution is delayed if `delay-mode-hooks' is non-nil.
Major mode functions should use this."
  (if delay-mode-hooks
      ;; Delaying case.
      (dolist (hook hooks)
	(push hook delayed-mode-hooks))
    ;; Normal case, just run the hook as before plus any delayed hooks.
    (setq hooks (nconc (nreverse delayed-mode-hooks) hooks))
    (setq delayed-mode-hooks nil)
    (apply 'run-hooks hooks)))

(defmacro delay-mode-hooks (&rest body)
  "Execute BODY, but delay any `run-mode-hooks'.
Only affects hooks run in the current buffer."
  `(progn
     (make-local-variable 'delay-mode-hooks)
     (let ((delay-mode-hooks t))
       ,@body)))

(defmacro with-syntax-table (table &rest body)
  "Evaluate BODY with syntax table of current buffer set to a copy of TABLE.
The syntax table of the current buffer is saved, BODY is evaluated, and the
saved table is restored, even in case of an abnormal exit.
Value is what BODY returns."
  (let ((old-table (make-symbol "table"))
	(old-buffer (make-symbol "buffer")))
    `(let ((,old-table (syntax-table))
	   (,old-buffer (current-buffer)))
       (unwind-protect
	   (progn
	     (set-syntax-table (copy-syntax-table ,table))
	     ,@body)
	 (save-current-buffer
	   (set-buffer ,old-buffer)
	   (set-syntax-table ,old-table))))))

(put 'with-syntax-table 'lisp-indent-function 1)
(put 'with-syntax-table 'edebug-form-spec '(form body))


;; Moved from mule-coding.el.
(defmacro with-string-as-buffer-contents (str &rest body)
  "With the contents of the current buffer being STR, run BODY.
Returns the new contents of the buffer, as modified by BODY.
The original current buffer is restored afterwards."
  `(with-temp-buffer
     (insert ,str)
     ,@body
     (buffer-string)))


(defmacro save-match-data (&rest body)
  "Execute BODY forms, restoring the global value of the match data."
  (let ((original (make-symbol "match-data")))
    (list 'let (list (list original '(match-data)))
	  (list 'unwind-protect
		(cons 'progn body)
		(list 'store-match-data original)))))


(defun match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
          (substring string (match-beginning num) (match-end num))
        (buffer-substring (match-beginning num) (match-end num)))))

(defun match-string-no-properties (num &optional string)
  "Return string of text matched by last search, without text properties.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (let ((result
		 (substring string (match-beginning num) (match-end num))))
	    (set-text-properties 0 (length result) nil result)
	    result)
	(buffer-substring-no-properties (match-beginning num)
					(match-end num)))))

(defconst split-string-default-separators "[ \f\t\n\r\v]+"
  "The default value of separators for `split-string'.

A regexp matching strings of whitespace.  May be locale-dependent
\(as yet unimplemented).  Should not match non-breaking spaces.

Warning: binding this to a different value and using it as default is
likely to have undesired semantics.")

;; specification for `split-string' agreed with rms 2003-04-23
;; xemacs design <87vfx5vor0.fsf@tleepslib.sk.tsukuba.ac.jp>

;; The specification says that if both SEPARATORS and OMIT-NULLS are
;; defaulted, OMIT-NULLS should be treated as t.  Simplifying the logical
;; expression leads to the equivalent implementation that if SEPARATORS
;; is defaulted, OMIT-NULLS is treated as t.

(defun split-string (string &optional separators omit-nulls)
  "Splits STRING into substrings bounded by matches for SEPARATORS.

The beginning and end of STRING, and each match for SEPARATORS, are
splitting points.  The substrings matching SEPARATORS are removed, and
the substrings between the splitting points are collected as a list,
which is returned.

If SEPARATORS is non-`nil', it should be a regular expression matching text
which separates, but is not part of, the substrings.  If `nil' it defaults to
`split-string-default-separators', normally \"[ \\f\\t\\n\\r\\v]+\", and
OMIT-NULLS is forced to `t'.

If OMIT-NULLS is `t', zero-length substrings are omitted from the list \(so
that for the default value of SEPARATORS leading and trailing whitespace
are effectively trimmed).  If `nil', all zero-length substrings are retained,
which correctly parses CSV format, for example.

Note that the effect of `(split-string STRING)' is the same as
`(split-string STRING split-string-default-separators t)').  In the rare
case that you wish to retain zero-length substrings when splitting on
whitespace, use `(split-string STRING split-string-default-separators nil)'.

Modifies the match data when successful; use `save-match-data' if necessary."

  (let ((keep-nulls (not (if separators omit-nulls t)))
	(rexp (or separators split-string-default-separators))
	(start 0)
	notfirst
	(list nil))
    (while (and (string-match rexp string
			      (if (and notfirst
				       (= start (match-beginning 0))
				       (< start (length string)))
				  (1+ start) start))
		(< start (length string)))
      (setq notfirst t)
      (if (or keep-nulls (< start (match-beginning 0)))
	  (setq list
		(cons (substring string start (match-beginning 0))
		      list)))
      (setq start (match-end 0)))
    (if (or keep-nulls (< start (length string)))
	(setq list
	      (cons (substring string start)
		    list)))
    (nreverse list)))

(defun subst-char-in-string (fromchar tochar string &optional inplace)
  "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
  (let ((i (length string))
	(newstr (if inplace string (copy-sequence string))))
    (while (> i 0)
      (setq i (1- i))
      (if (eq (aref newstr i) fromchar)
	  (aset newstr i tochar)))
    newstr))


;; XEmacs addition:
(defun replace-in-string (str regexp newtext &optional literal)
  "Replace all matches in STR for REGEXP with NEWTEXT string,
 and returns the new string.
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat `\\' in NEWTEXT as special:
  `\\&' in NEWTEXT means substitute original matched text.
  `\\N' means substitute what matched the Nth `\\(...\\)'.
       If Nth parens didn't match, substitute nothing.
  `\\\\' means insert one `\\'.
  `\\u' means upcase the next character.
  `\\l' means downcase the next character.
  `\\U' means begin upcasing all following characters.
  `\\L' means begin downcasing all following characters.
  `\\E' means terminate the effect of any `\\U' or `\\L'."
  (check-argument-type 'stringp str)
  (check-argument-type 'stringp newtext)
  (if (> (length str) 50)
      (let ((cfs case-fold-search))
	(with-temp-buffer
	  (setq case-fold-search cfs)
	  (insert str)
	  (goto-char 1)
	  (while (re-search-forward regexp nil t)
	    (replace-match newtext t literal))
	  (buffer-string)))
    (let ((start 0) newstr)
      (while (string-match regexp str start)
	(setq newstr (replace-match newtext t literal str)
	      start (+ (match-end 0) (- (length newstr) (length str)))
	      str newstr))
      str)))

(defun replace-regexp-in-string (regexp rep string &optional
					fixedcase literal subexp start)
  "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"

  ;; To avoid excessive consing from multiple matches in long strings,
  ;; don't just call `replace-match' continually.  Walk down the
  ;; string looking for matches of REGEXP and building up a (reversed)
  ;; list MATCHES.  This comprises segments of STRING which weren't
  ;; matched interspersed with replacements for segments that were.
  ;; [For a `large' number of replacments it's more efficient to
  ;; operate in a temporary buffer; we can't tell from the function's
  ;; args whether to choose the buffer-based implementation, though it
  ;; might be reasonable to do so for long enough STRING.]
  (let ((l (length string))
	(start (or start 0))
	matches str mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
	(setq mb (match-beginning 0)
	      me (match-end 0))
	;; If we matched the empty string, make sure we advance by one char
	(when (= me mb) (setq me (min l (1+ mb))))
	;; Generate a replacement for the matched substring.
	;; Operate only on the substring to minimize string consing.
	;; Set up match data for the substring for replacement;
	;; presumably this is likely to be faster than munging the
	;; match data directly in Lisp.
	(string-match regexp (setq str (substring string mb me)))
	(setq matches
	      (cons (replace-match (if (stringp rep)
				       rep
				     (funcall rep (match-string 0 str)))
				   fixedcase literal str subexp)
		    (cons (substring string start mb) ; unmatched prefix
			  matches)))
	(setq start me))
      ;; Reconstruct a string from the pieces.
      (setq matches (cons (substring string start l) matches)) ; leftover
      (apply #'concat (nreverse matches)))))

;; END SYNCHED WITH FSF 21.2


;; BEGIN SYNCHED WITH FSF 21.3

(defun add-to-invisibility-spec (arg)
  "Add elements to `buffer-invisibility-spec'.
See documentation for `buffer-invisibility-spec' for the kind of elements
that can be added."
  (if (eq buffer-invisibility-spec t)
      (setq buffer-invisibility-spec (list t)))
  (setq buffer-invisibility-spec
	(cons arg buffer-invisibility-spec)))

(defun remove-from-invisibility-spec (arg)
  "Remove elements from `buffer-invisibility-spec'."
  (if (consp buffer-invisibility-spec)
    (setq buffer-invisibility-spec (delete arg buffer-invisibility-spec))))

;; END SYNCHED WITH FSF 21.3


;;; Basic string functions

;; XEmacs
(defun string-equal-ignore-case (str1 str2)
  "Return t if two strings have identical contents, ignoring case differences.
Case is not significant.  Text properties and extents are ignored.
Symbols are also allowed; their print names are used instead.

See also `equalp'."
  (if (symbolp str1)
      (setq str1 (symbol-name str1)))
  (if (symbolp str2)
      (setq str2 (symbol-name str2)))
  (string= (downcase str1) (downcase str2)))

(defun insert-face (string face)
  "Insert STRING and highlight with FACE.  Return the extent created."
  (let ((p (point)) ext)
    (insert string)
    (setq ext (make-extent p (point)))
    (set-extent-face ext face)
    ext))

;; not obsolete.
(define-function 'string= 'string-equal)
(define-function 'string< 'string-lessp)
(define-function 'int-to-string 'number-to-string)
(define-function 'string-to-int 'string-to-number)

;; These two names are a bit awkward, as they conflict with the normal
;; foo-to-bar naming scheme, but CLtL2 has them, so they stay.
(define-function 'char-int 'char-to-int)
(define-function 'int-char 'int-to-char)

;; XEmacs addition.
(defun integer-to-bit-vector (integer &optional minlength)
  "Return INTEGER converted to a bit vector.
Optional argument MINLENGTH gives a minimum length for the returned vector.
If MINLENGTH is not given, zero high-order bits will be ignored."
  (check-argument-type #'integerp integer)
  (setq minlength (or minlength 0))
  (check-nonnegative-number minlength)
  (let* ((tmp (format "%&0.*b" minlength integer)))
    (aset tmp 1 ?*)
    (read tmp)))

;; XEmacs addition.
(defun bit-vector-to-integer (bit-vector)
  "Return BIT-VECTOR converted to an integer.
If bignum support is available, BIT-VECTOR's length is unlimited.
Otherwise the limit is the number of value bits in an Lisp integer. "
  (check-argument-type #'bit-vector-p bit-vector)
  (setq bit-vector (prin1-to-string bit-vector))
  (aset bit-vector 1 ?b)
  (read bit-vector))

(defun string-width (string)
  "Return number of columns STRING occupies when displayed.
With international (Mule) support, uses the charset-columns attribute of
the characters in STRING, which may not accurately represent the actual
display width when using a window system.  With no international support,
simply returns the length of the string."
  (if (featurep 'mule)
      (let ((col 0)
	    (len (length string))
	    (i 0))
	(with-fboundp '(charset-width char-charset)
	  (while (< i len)
	    (setq col (+ col (charset-width (char-charset (aref string i)))))
	    (setq i (1+ i))))
	col)
    (length string)))

(defun char-width (character)
  "Return number of columns a CHARACTER occupies when displayed."
  (if (featurep 'mule)
      (with-fboundp '(charset-width char-charset)
	(charset-width (char-charset character)))
    1))

;; The following several functions are useful in GNU Emacs 20 because
;; of the multibyte "characters" the internal representation of which
;; leaks into Lisp.  In XEmacs/Mule they are trivial and unnecessary.
;; We provide them for compatibility reasons solely.

(defun string-to-sequence (string type)
  "Convert STRING to a sequence of TYPE which contains characters in STRING.
TYPE should be `list' or `vector'."
  (ecase type
    (list
     (mapcar #'identity string))
    (vector
     (mapvector #'identity string))))

(defun string-to-list (string)
  "Return a list of characters in STRING."
  (mapcar #'identity string))

(defun string-to-vector (string)
  "Return a vector of characters in STRING."
  (mapvector #'identity string))

(defun store-substring (string idx obj)
  "Embed OBJ (string or character) at index IDX of STRING."
  (let* ((str (cond ((stringp obj) obj)
		    ((characterp obj) (char-to-string obj))
		    (t (error
			"Invalid argument (should be string or character): %s"
			obj))))
	 (string-len (length string))
	 (len (length str))
	 (i 0))
    (while (and (< i len) (< idx string-len))
      (aset string idx (aref str i))
      (setq idx (1+ idx) i (1+ i)))
    string))

;; From FSF 21.1; ELLIPSES is XEmacs addition.

(defun truncate-string-to-width (str end-column &optional start-column padding
				 ellipses)
  "Truncate string STR to end at column END-COLUMN.
The optional 3rd arg START-COLUMN, if non-nil, specifies
the starting column; that means to return the characters occupying
columns START-COLUMN ... END-COLUMN of STR.

The optional 4th arg PADDING, if non-nil, specifies a padding character
to add at the end of the result if STR doesn't reach column END-COLUMN,
or if END-COLUMN comes in the middle of a character in STR.
PADDING is also added at the beginning of the result
if column START-COLUMN appears in the middle of a character in STR.

If PADDING is nil, no padding is added in these cases, so
the resulting string may be narrower than END-COLUMN.

BUG: Currently assumes that the padding character is of width one.  You
will get weird results if not.

If ELLIPSES is non-nil, add ellipses (specified by ELLIPSES if a string,
else `...') if STR extends past END-COLUMN.  The ellipses will be added in
such a way that the total string occupies no more than END-COLUMN columns
-- i.e. if the string goes past END-COLUMN, it will be truncated somewhere
short of END-COLUMN so that, with the ellipses added (and padding, if the
proper place to truncate the string would be in the middle of a character),
the string occupies exactly END-COLUMN columns."
  (or start-column
      (setq start-column 0))
  (let ((len (length str))
	(idx 0)
	(column 0)
	(head-padding "") (tail-padding "")
	ch last-column last-idx from-idx)

    ;; find the index of START-COLUMN; bail out if end of string reached.
    (condition-case nil
	(while (< column start-column)
	  (setq ch (aref str idx)
		column (+ column (char-width ch))
		idx (1+ idx)))
      (args-out-of-range (setq idx len)))
    (if (< column start-column)
	;; if string ends before START-COLUMN, return either a blank string
	;; or a string entirely padded.
	(if padding (make-string (- end-column start-column) padding) "")
      (if (and padding (> column start-column))
	  (setq head-padding (make-string (- column start-column) padding)))
      (setq from-idx idx)
      ;; If END-COLUMN is before START-COLUMN, then bail out.
      (if (< end-column column)
	  (setq idx from-idx ellipses "")

	;; handle ELLIPSES
	(cond ((null ellipses) (setq ellipses ""))
	      ((if (<= (string-width str) end-column)
		   ;; string fits, no ellipses
		   (setq ellipses "")))
	      (t
	       ;; else, insert default value and ...
	       (or (stringp ellipses) (setq ellipses "..."))
	       ;; ... take away the width of the ellipses from the
	       ;; destination.  do all computations with new, shorter
	       ;; width.  the padding computed will get us exactly up to
	       ;; the shorted width, which is right -- it just gets added
	       ;; to the right of the ellipses.
	       (setq end-column (- end-column (string-width ellipses)))))

	;; find the index of END-COLUMN; bail out if end of string reached.
	(condition-case nil
	    (while (< column end-column)
	      (setq last-column column
		    last-idx idx
		    ch (aref str idx)
		    column (+ column (char-width ch))
		    idx (1+ idx)))
	  (args-out-of-range (setq idx len)))
	;; if we went too far (stopped in middle of character), back up.
	(if (> column end-column)
	    (setq column last-column idx last-idx))
	;; compute remaining padding
	(if (and padding (< column end-column))
	    (setq tail-padding (make-string (- end-column column) padding))))
      ;; get substring ...
      (setq str (substring str from-idx idx))
      ;; and construct result
      (if padding
	  (concat head-padding str tail-padding ellipses)
	(concat str ellipses)))))


;; alist/plist functions
(defun plist-to-alist (plist)
  "Convert property list PLIST into the equivalent association-list form.
The alist is returned.  This converts from

\(a 1 b 2 c 3)

into

\((a . 1) (b . 2) (c . 3))

The original plist is not modified.  See also `destructive-plist-to-alist'."
  (let (alist)
    (while plist
      (setq alist (cons (cons (car plist) (cadr plist)) alist))
      (setq plist (cddr plist)))
    (nreverse alist)))

(defun map-plist (_mp_fun _mp_plist)
  "Map _MP_FUN (a function of two args) over each key/value pair in _MP_PLIST.
Return a list of the results."
  (let (_mp_result)
    (while _mp_plist
      (push (funcall _mp_fun (car _mp_plist) (cadr _mp_plist)) _mp_result)
      (setq _mp_plist (cddr _mp_plist)))
    (nreverse _mp_result)))

(defun destructive-plist-to-alist (plist)
  "Convert property list PLIST into the equivalent association-list form.
The alist is returned.  This converts from

\(a 1 b 2 c 3)

into

\((a . 1) (b . 2) (c . 3))

The original plist is destroyed in the process of constructing the alist.
See also `plist-to-alist'."
  (let ((head plist)
	next)
    (while plist
      ;; remember the next plist pair.
      (setq next (cddr plist))
      ;; make the cons holding the property value into the alist element.
      (setcdr (cdr plist) (cadr plist))
      (setcar (cdr plist) (car plist))
      ;; reattach into alist form.
      (setcar plist (cdr plist))
      (setcdr plist next)
      (setq plist next))
    head))

(defun alist-to-plist (alist)
  "Convert association list ALIST into the equivalent property-list form.
The plist is returned.  This converts from

\((a . 1) (b . 2) (c . 3))

into

\(a 1 b 2 c 3)

The original alist is not modified.  See also `destructive-alist-to-plist'."
  (let (plist)
    (while alist
      (let ((el (car alist)))
	(setq plist (cons (cdr el) (cons (car el) plist))))
      (setq alist (cdr alist)))
    (nreverse plist)))

;; getf, remf in cl*.el.

(defmacro putf (plist property value)
  "Add property PROPERTY to plist PLIST with value VALUE.
Analogous to (setq PLIST (plist-put PLIST PROPERTY VALUE))."
  `(setq ,plist (plist-put ,plist ,property ,value)))

(defmacro laxputf (lax-plist property value)
  "Add property PROPERTY to lax plist LAX-PLIST with value VALUE.
Analogous to (setq LAX-PLIST (lax-plist-put LAX-PLIST PROPERTY VALUE))."
  `(setq ,lax-plist (lax-plist-put ,lax-plist ,property ,value)))

(defmacro laxremf (lax-plist property)
  "Remove property PROPERTY from lax plist LAX-PLIST.
Analogous to (setq LAX-PLIST (lax-plist-remprop LAX-PLIST PROPERTY))."
  `(setq ,lax-plist (lax-plist-remprop ,lax-plist ,property)))

;;; Error functions

(defun error (datum &rest args)
  "Signal a non-continuable error.
DATUM should normally be an error symbol, i.e. a symbol defined using
`define-error'.  ARGS will be made into a list, and DATUM and ARGS passed
as the two arguments to `signal', the most basic error handling function.

This error is not continuable: you cannot continue execution after the
error using the debugger `r' command.  See also `cerror'.

The correct semantics of ARGS varies from error to error, but for most
errors that need to be generated in Lisp code, the first argument
should be a string describing the *context* of the error (i.e. the
exact operation being performed and what went wrong), and the remaining
arguments or \"frobs\" (most often, there is one) specify the
offending object(s) and/or provide additional details such as the exact
error when a file error occurred, e.g.:

-- the buffer in which an editing error occurred.
-- an invalid value that was encountered. (In such cases, the string
   should describe the purpose or \"semantics\" of the value [e.g. if the
   value is an argument to a function, the name of the argument; if the value
   is the value corresponding to a keyword, the name of the keyword; if the
   value is supposed to be a list length, say this and say what the purpose
   of the list is; etc.] as well as specifying why the value is invalid, if
   that's not self-evident.)
-- the file in which an error occurred. (In such cases, there should be a
   second frob, probably a string, specifying the exact error that occurred.
   This does not occur in the string that precedes the first frob, because
   that frob describes the exact operation that was happening.

For historical compatibility, DATUM can also be a string.  In this case,
DATUM and ARGS are passed together as the arguments to `format', and then
an error is signalled using the error symbol `error' and formatted string.
Although this usage of `error' is very common, it is deprecated because it
totally defeats the purpose of having structured errors.  There is now
a rich set of defined errors you can use:

quit

error
  invalid-argument
    syntax-error
      invalid-read-syntax
      invalid-regexp
      structure-formation-error
        list-formation-error
          malformed-list
            malformed-property-list
          circular-list
            circular-property-list
    invalid-function
    no-catch
    undefined-keystroke-sequence
    invalid-constant
    wrong-type-argument
    args-out-of-range
    wrong-number-of-arguments

  invalid-state
    void-function
    cyclic-function-indirection
    void-variable
    cyclic-variable-indirection
    invalid-byte-code
    stack-overflow
    out-of-memory
    invalid-key-binding
    internal-error

  invalid-operation
    invalid-change
      setting-constant
      protected-field
    editing-error
      beginning-of-buffer
      end-of-buffer
      buffer-read-only
    io-error
      file-error
        file-already-exists
        file-locked
        file-supersession
        end-of-file
      process-error
      network-error
<<<<<<< HEAD
      tooltalk-error
=======
>>>>>>> master
      gui-error
        dialog-box-error
      sound-error
      conversion-error
        text-conversion-error
        image-conversion-error
        base64-conversion-error
        selection-conversion-error
    arith-error
      range-error
      domain-error
      singularity-error
      overflow-error
      underflow-error
    search-failed
    printing-unreadable-object
    unimplemented

Note the semantic differences between some of the more common errors:

-- `invalid-argument' is for all cases where a bad value is encountered.
-- `invalid-constant' is for arguments where only a specific set of values
   is allowed.
-- `syntax-error' is when complex structures (parsed strings, lists,
   and the like) are badly formed.  If the problem is just a single bad
   value inside the structure, you should probably be using something else,
   e.g. `invalid-constant', `wrong-type-argument', or `invalid-argument'.
-- `invalid-state' means that some settings have been changed in such a way
   that their current state is unallowable.  More and more, code is being
   written more carefully, and catches the error when the settings are being
   changed, rather than afterwards.  This leads us to the next error:
-- `invalid-change' means that an attempt is being made to change some settings
   into an invalid state.  `invalid-change' is a type of `invalid-operation'.
-- `invalid-operation' refers to all cases where code is trying to do something
   that's disallowed, or when an error occurred during an operation. (These
   two concepts are merged because there's no clear distinction between them.)
-- `io-error' refers to errors involving interaction with any external
   components (files, other programs, the operating system, etc).

See also `cerror', `signal', and `signal-error'."
  (while t (apply
	    'cerror datum args)))

(defun cerror (datum &rest args)
  "Like `error' but signals a continuable error."
  (cond ((stringp datum)
	 (signal 'error (list (apply 'format datum args))))
	((defined-error-p datum)
	 (signal datum args))
	(t
	 (error 'invalid-argument "datum not string or error symbol" datum))))

(defmacro check-argument-type (predicate argument)
  "Check that ARGUMENT satisfies PREDICATE.
This is a macro, and ARGUMENT is not evaluated.  If ARGUMENT is an lvalue,
this function signals a continuable `wrong-type-argument' error until the
returned value satisfies PREDICATE, and assigns the returned value
to ARGUMENT.  Otherwise, this function signals a non-continuable
`wrong-type-argument' error if the returned value does not satisfy PREDICATE."
  (if (symbolp argument)
      `(if (not (,(eval predicate) ,argument))
	   (setq ,argument
		 (wrong-type-argument ,predicate ,argument)))
    `(if (not (,(eval predicate) ,argument))
	 (signal-error 'wrong-type-argument (list ,predicate ,argument)))))

(defun args-out-of-range (value min max)
  "Signal an error until the correct in-range value is given by the user.
This function loops, signalling a continuable `args-out-of-range' error
with VALUE, MIN and MAX as the data associated with the error and then
checking the returned value to make sure it's not outside the given
boundaries \(nil for either means no boundary on that side).  At that
point, the gotten value is returned."
  (loop
    for newval = (signal 'args-out-of-range (list value min max))
    do (setq value newval)
    finally return value
    while (not (argument-in-range-p value min max))))

(defun argument-in-range-p (argument min max)
  "Return true if ARGUMENT is within the range of [MIN, MAX].
This includes boundaries.  nil for either value means no limit on that side."
  (and (or (not min) (<= min argument))
       (or (not max) (<= argument max))))

(defmacro check-argument-range (argument min max)
  "Check that ARGUMENT is within the range [MIN, MAX].
This is a macro, and ARGUMENT is not evaluated.  If ARGUMENT is an lvalue,
this function signals a continuable `args-out-of-range' error until the
returned value is within range, and assigns the returned value
to ARGUMENT.  Otherwise, this function signals a non-continuable
`args-out-of-range' error if the returned value is out of range."
  (if (symbolp argument)
      `(if (not (argument-in-range-p ,argument ,min ,max))
	   (setq ,argument
		 (args-out-of-range ,argument ,min ,max)))
    (let ((newsym (gensym)))
      `(let ((,newsym ,argument))
	 (if (not (argument-in-range-p ,newsym ,min ,max))
	     (signal-error 'args-out-of-range (list ,newsym ,min ,max)))))))

(defun signal-error (error-symbol data)
  "Signal a non-continuable error.  Args are ERROR-SYMBOL, and associated DATA.
An error symbol is a symbol defined using `define-error'.
DATA should be a list.  Its elements are printed as part of the error message.
If the signal is handled, DATA is made available to the handler.
See also `signal', and the functions to handle errors: `condition-case'
and `call-with-condition-handler'."
  (while t
    (signal error-symbol data)))

(defun define-error (error-sym doc-string &optional inherits-from)
  "Define a new error, denoted by ERROR-SYM.
DOC-STRING is an informative message explaining the error, and will be
printed out when an unhandled error occurs.
ERROR-SYM is a sub-error of INHERITS-FROM (which defaults to `error').

\[`define-error' internally works by putting on ERROR-SYM an `error-message'
property whose value is DOC-STRING, and an `error-conditions' property
that is a list of ERROR-SYM followed by each of its super-errors, up
to and including `error'.  You will sometimes see code that sets this up
directly rather than calling `define-error', but you should *not* do this
yourself.]"
  (check-argument-type 'symbolp error-sym)
  (check-argument-type 'stringp doc-string)
  (put error-sym 'error-message doc-string)
  (or inherits-from (setq inherits-from 'error))
  (let ((conds (get inherits-from 'error-conditions)))
    (or conds (signal-error 'error (list "Not an error symbol" error-sym)))
    (put error-sym 'error-conditions (cons error-sym conds))))

(defun defined-error-p (sym)
  "Returns non-nil if SYM names a currently-defined error."
  (and (symbolp sym) (not (null (get sym 'error-conditions)))))

(defun backtrace-in-condition-handler-eliminating-handler (handler-arg-name)
  "Return a backtrace inside of a condition handler, eliminating the handler.
This is for use in the condition handler inside of call-with-condition-handler,
when written like this:

\(call-with-condition-handler
    #'(lambda (__some_weird_arg__)
	do the handling ...)
    #'(lambda ()
	do the stuff that might cause an error))

Pass in the name (a symbol) of the argument used in the lambda function
that specifies the handler, and make sure the argument name is unique, and
this function generates a backtrace and strips off the part above where the
error occurred (i.e. the handler itself)."
  (let* ((bt (with-output-to-string (backtrace nil t)))
	 (bt (save-match-data
	       ;; Try to eliminate the part of the backtrace
	       ;; above where the error occurred.
	       (if (string-match
		    (concat "bind (\\(?:.* \\)?" (symbol-name handler-arg-name)
			    "\\(?:.* \\)?)[ \t\n]*\\(?:(lambda \\|#<compiled-function \\)("
			    (symbol-name handler-arg-name)
			    ").*\n\\(\\(?:.\\|\n\\)*\\)$")
		    bt) (match-string 1 bt) bt))))
    bt))

(put 'with-trapping-errors 'lisp-indent-function 0)
(defmacro with-trapping-errors (&rest keys-body)
  "Trap errors in BODY, outputting a warning and a backtrace.
Usage looks like

\(with-trapping-errors
    [:operation OPERATION]
    [:error-form ERROR-FORM]
    [:no-backtrace NO-BACKTRACE]
    [:class CLASS]
    [:level LEVEL]
    [:resignal RESIGNAL]
    BODY)

Return value without error is whatever BODY returns.  With error, return
result of ERROR-FORM (which will be evaluated only when the error actually
occurs), which defaults to nil.  OPERATION is given in the warning message.
CLASS and LEVEL are the warning class and level (default to class
`general', level `warning').  If NO-BACKTRACE is given, no backtrace is
displayed.  If RESIGNAL is given, the error is resignaled after the warning
is displayed and the ERROR-FORM is executed."
  (let ((operation "unknown")
	(error-form nil)
	(no-backtrace nil)
	(class ''general)
	(level ''warning)
	(resignal nil))
    (let* ((keys '(operation error-form no-backtrace class level resignal))
	   (keys-with-colon
	    (mapcar #'(lambda (sym)
			(intern (concat ":" (symbol-name sym)))) keys)))
      (while (memq (car keys-body) keys-with-colon)
	(let* ((key-with-colon (pop keys-body))
	       (key (intern (substring (symbol-name key-with-colon) 1))))
	  (set key (pop keys-body)))))
    `(condition-case ,(if resignal '__cte_cc_var__ nil)
	 (call-with-condition-handler
	     #'(lambda (__call_trapping_errors_arg__)
		 (let ((errstr (error-message-string
				__call_trapping_errors_arg__)))
		   ,(if no-backtrace
			`(lwarn ,class ,level
			   (if (warning-level-<
				,level
				display-warning-minimum-level)
			       "Error in %s: %s"
			     "Error in %s:\n%s\n")
			   ,operation errstr)
		      `(lwarn ,class ,level
			 "Error in %s: %s\n\nBacktrace follows:\n\n%s"
			 ,operation errstr
			 (backtrace-in-condition-handler-eliminating-handler
			  '__call_trapping_errors_arg__)))))
	     #'(lambda ()
		 (progn ,@keys-body)))
       (error
	,error-form
	,@(if resignal '((signal (car __cte_cc_var__) (cdr __cte_cc_var__)))))
       )))

;;;; Miscellanea.

;; This is now in C.
;(defun buffer-substring-no-properties (start end)
;  "Return the text from START to END, without text properties, as a string."
;  (let ((string (buffer-substring start end)))
;    (set-text-properties 0 (length string) nil string)
;    string))

(defun get-buffer-window-list (&optional buffer minibuf frame)
  "Return windows currently displaying BUFFER, or nil if none.
BUFFER defaults to the current buffer.
See `walk-windows' for the meaning of MINIBUF and FRAME."
  (cond ((null buffer)
	 (setq buffer (current-buffer)))
	((not (bufferp buffer))
	 (setq buffer (get-buffer buffer))))
  (let (windows)
    (walk-windows (lambda (window)
		    (if (eq (window-buffer window) buffer)
			(push window windows)))
		  minibuf frame)
    windows))

(defun ignore (&rest ignore)
  "Do nothing and return nil.
This function accepts any number of arguments, but ignores them."
  (interactive)
  nil)

;; defined in lisp/bindings.el in GNU Emacs.
(defmacro bound-and-true-p (var)
  "Return the value of symbol VAR if it is bound, else nil."
  `(and (boundp (quote ,var)) ,var))

;; `propertize' is a builtin in GNU Emacs 21.
(defun propertize (string &rest properties)
  "Return a copy of STRING with text properties added.
First argument is the string to copy.
Remaining arguments form a sequence of PROPERTY VALUE pairs for text
properties to add to the result."
  (let ((str (copy-sequence string)))
    (add-text-properties 0 (length str)
			 properties
			 str)
    str))

;; `delete-and-extract-region' is a builtin in GNU Emacs 21.
(defun delete-and-extract-region (start end)
  "Delete the text between START and END and return it."
  (let ((region (buffer-substring start end)))
    (delete-region start end)
    region))

(define-function 'eval-in-buffer 'with-current-buffer)
(make-obsolete 'eval-in-buffer 'with-current-buffer)

;;; The real defn is in abbrev.el but some early callers
;;;  (eg lisp-mode-abbrev-table) want this before abbrev.el is loaded...

(if (not (fboundp 'define-abbrev-table))
    (progn
      (setq abbrev-table-name-list '())
      (fset 'define-abbrev-table (function (lambda (name defs)
                                   ;; These are fixed-up when abbrev.el loads.
                                   (setq abbrev-table-name-list
                                         (cons (cons name defs)
                                               abbrev-table-name-list)))))))

;;; `functionp' has been moved into C.

;;(defun functionp (object)
;;  "Non-nil if OBJECT can be called as a function."
;;  (or (and (symbolp object) (fboundp object))
;;      (subrp object)
;;      (compiled-function-p object)
;;      (eq (car-safe object) 'lambda)))



(defun function-interactive (function)
  "Return the interactive specification of FUNCTION.
FUNCTION can be any funcallable object.
The specification will be returned as the list of the symbol `interactive'
 and the specs.
If FUNCTION is not interactive, nil will be returned."
  (setq function (indirect-function function))
  (cond ((compiled-function-p function)
	 (compiled-function-interactive function))
	((subrp function)
	 (subr-interactive function))
	((eq (car-safe function) 'lambda)
	 (let ((spec (if (stringp (nth 2 function))
			 (nth 3 function)
		       (nth 2 function))))
	   (and (eq (car-safe spec) 'interactive)
		spec)))
	(t
	 (error "Non-funcallable object: %s" function))))

(defun function-allows-args (function n)
  "Return whether FUNCTION can be called with N arguments."
  (and (<= (function-min-args function) n)
       (or (null (function-max-args function))
	   (<= n (function-max-args function)))))

;; This function used to be an alias to `buffer-substring', except
;; that FSF Emacs 20.4 added a BUFFER argument in an incompatible way.
;; The new FSF's semantics makes more sense, but we try to support
;; both for backward compatibility.
(defun buffer-string (&optional buffer old-end old-buffer)
  "Return the contents of the current buffer as a string.
If narrowing is in effect, this function returns only the visible part
of the buffer.

If BUFFER is specified, the contents of that buffer are returned.

The arguments OLD-END and OLD-BUFFER are supported for backward
compatibility with pre-21.2 XEmacsen times when arguments to this
function were (buffer-string &optional START END BUFFER)."
  (cond
   ((or (stringp buffer) (bufferp buffer))
    ;; Most definitely the new way.
    (buffer-substring nil nil buffer))
   ((or (stringp old-buffer) (bufferp old-buffer)
	(natnump buffer) (natnump old-end))
    ;; Definitely the old way.
    (buffer-substring buffer old-end old-buffer))
   (t
    ;; Probably the old way.
    (buffer-substring buffer old-end old-buffer))))

;; BEGIN SYNC WITH FSF 21.2

;; This was not present before.  I think Jamie had some objections
;; to this, so I'm leaving this undefined for now. --ben

;;; The objection is this: there is more than one way to load the same file.
;;; "foo", "foo.elc", "foo.el", and "/some/path/foo.elc" are all different
;;; ways to load the exact same code.  `eval-after-load' is too stupid to
;;; deal with this sort of thing.  If this sort of feature is desired, then
;;; it should work off of a hook on `provide'.  Features are unique and
;;; the arguments to (load) are not.  --Stig

;; We provide this for FSFmacs compatibility, at least until we devise
;; something better.

;;;; Specifying things to do after certain files are loaded.

(defun eval-after-load (file form)
  "Arrange that, if FILE is ever loaded, FORM will be run at that time.
This makes or adds to an entry on `after-load-alist'.
If FILE is already loaded, evaluate FORM right now.
It does nothing if FORM is already on the list for FILE.
FILE must match exactly.  Normally FILE is the name of a library,
with no directory or extension specified, since that is how `load'
is normally called."
  ;; Make sure `load-history' contains the files dumped with Emacs
  ;; for the case that FILE is one of the files dumped with Emacs.
  (if-fboundp 'load-symbol-file-load-history
      (load-symbol-file-load-history))
  ;; Make sure there is an element for FILE.
  (or (assoc file after-load-alist)
      (setq after-load-alist (cons (list file) after-load-alist)))
  ;; Add FORM to the element if it isn't there.
  (let ((elt (assoc file after-load-alist)))
    (or (member form (cdr elt))
	(progn
	  (nconc elt (list form))
	  ;; If the file has been loaded already, run FORM right away.
	  (and (assoc file load-history)
	       (eval form)))))
  form)
(make-compatible 'eval-after-load "")

(defun eval-next-after-load (file)
  "Read the following input sexp, and run it whenever FILE is loaded.
This makes or adds to an entry on `after-load-alist'.
FILE should be the name of a library, with no directory name."
  (eval-after-load file (read)))
(make-compatible 'eval-next-after-load "")

;; END SYNC WITH FSF 21.2

;; BEGIN SYNC WITH FSF 22.0.50.1 (CVS)
(defun delete-dups (list)
  "Destructively remove `equal' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list.
Of several `equal' occurrences of an element in LIST, the first
one is kept."
  (let ((tail list))
    (while tail
      (setcdr tail (delete (car tail) (cdr tail)))
      (setq tail (cdr tail))))
  list)

;; END SYNC WITH FSF 22.0.50.1 (CVS)

;; (defun shell-quote-argument (argument) in process.el.

;; (defun make-syntax-table (&optional oldtable) in syntax.el.

;; (defun syntax-after (pos) #### doesn't exist.

;; global-set-key, local-set-key, global-unset-key, local-unset-key in
;; keymap.el.

;; frame-configuration-p is in frame.el.

;; functionp is built-in.

;; interactive-form in obsolete.el.

;; assq-del-all in obsolete.el.

;; (defun make-temp-file (prefix &optional dir-flag suffix) #### doesn't exist.

;; add-minor-mode in modeline.el.

;; text-clone stuff #### doesn't exist; should go in text-props.el and
;; requires changes to extents.c (modification hooks).

;; play-sound is built-in.

;; define-mail-user-agent is in simple.el.

;; XEmacs; added. 
(defun skip-chars-quote (string)
  "Return a string that means all characters in STRING will be skipped,
if passed to `skip-chars-forward' or `skip-chars-backward'.

Ranges and carets are not treated specially.  This implementation is
in Lisp; do not use it in performance-critical code."
  (let ((list (delete-duplicates (string-to-list string) :test #'=)))
    (when (equal list '((?- ?\[) (?\[ ?\-)))
      (error 'invalid-argument
	     "Cannot create `skip-chars-forward' arg from string"
	     string))
    (when (memq ?\] list)
      (setq list (cons ?\] (delq ?\] list))))
    (when (eq ?^ (car list))
      (setq list (nconc (cdr list) '(?^))))
    (when (memq ?- list)
      (setq list (delq ?- list)
	    list (nconc list (list (second list) ?- (second list) ?-))))
    (apply #'string list)))

(provide 'subr)
;;; subr.el ends here
