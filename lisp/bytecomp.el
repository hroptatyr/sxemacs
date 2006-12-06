;;; bytecomp.el --- compilation of Lisp code into byte code.

;;; Copyright (C) 1985-1987, 1991-1994 Free Software Foundation, Inc.
;;; Copyright (C) 1996 Ben Wing.

;; Authors: Jamie Zawinski <jwz@jwz.org>
;;	Hallvard Furuseth <hbf@ulrik.uio.no>
;;	Ben Wing <ben@xemacs.org>
;;	Martin Buchholz <martin@xemacs.org>
;;	Richard Stallman <rms@gnu.org>
;; Keywords: internal lisp

(defconst byte-compile-version "2.27 XEmacs; 2000-09-12.")

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: FSF 19.30.

;;; Commentary:

;; The Emacs Lisp byte compiler.  This crunches lisp source into a
;; sort of p-code (`bytecode') which takes up less space and can be
;; interpreted faster.  First, the source code forms are converted to
;; an intermediate form, `lapcode' [`LAP' == `Lisp Assembly Program']
;; which is much easier to manipulate than bytecode.  Then the lapcode
;; is converted to bytecode, which can be considered to be actual
;; machine language.  Optimizations can occur at either the source
;; level or the lapcode level.

;; The user entry points are byte-compile-file,
;; byte-recompile-directory and byte-compile-buffer.

;;; Code:

;;; ========================================================================
;;; Entry points:
;;;	byte-recompile-directory, byte-compile-file,
;;;     batch-byte-compile, batch-byte-recompile-directory,
;;;	byte-compile, compile-defun,
;;;	display-call-tree
;;;  RMS says:
;;; (byte-compile-buffer and byte-compile-and-load-file were turned off
;;;  because they are not terribly useful and get in the way of completion.)
;;; But I'm leaving them. --ben

;;; This version of the byte compiler has the following improvements:
;;;  + optimization of compiled code:
;;;    - removal of unreachable code;
;;;    - removal of calls to side-effectless functions whose return-value
;;;      is unused;
;;;    - compile-time evaluation of safe constant forms, such as (consp nil)
;;;      and (ash 1 6);
;;;    - open-coding of literal lambdas;
;;;    - peephole optimization of emitted code;
;;;    - trivial functions are left uncompiled for speed.
;;;  + support for inline functions;
;;;  + compile-time evaluation of arbitrary expressions;
;;;  + compile-time warning messages for:
;;;    - functions being redefined with incompatible arglists;
;;;    - functions being redefined as macros, or vice-versa;
;;;    - functions or macros defined multiple times in the same file;
;;;    - functions being called with the incorrect number of arguments;
;;;    - functions being called which are not defined globally, in the
;;;      file, or as autoloads;
;;;    - assignment and reference of undeclared free variables;
;;;    - various syntax errors;
;;;  + correct compilation of nested defuns, defmacros, defvars and defsubsts;
;;;  + correct compilation of top-level uses of macros;
;;;  + the ability to generate a histogram of functions called.

;;; User customization variables:
;;;
;;; byte-compile-verbose	Whether to report the function currently being
;;;				compiled in the minibuffer;
;;; byte-optimize		Whether to do optimizations; this may be
;;;				t, nil, 'source, or 'byte;
;;; byte-optimize-log		Whether to report (in excruciating detail)
;;;				exactly which optimizations have been made.
;;;				This may be t, nil, 'source, or 'byte;
;;; byte-compile-error-on-warn	Whether to stop compilation when a warning is
;;;				produced;
;;; byte-compile-delete-errors	Whether the optimizer may delete calls or
;;;				variable references that are side-effect-free
;;;				except that they may return an error.
;;; byte-compile-generate-call-tree	Whether to generate a histogram of
;;;				function calls.  This can be useful for
;;;				finding unused functions, as well as simple
;;;				performance metering.
;;; byte-compile-warnings	List of warnings to issue, or t.  May contain
;;;				'free-vars (references to variables not in the
;;;					    current lexical scope)
;;;				'unused-vars (non-global variables bound but
;;;					      not referenced)
;;;				'unresolved (calls to unknown functions)
;;;				'callargs  (lambda calls with args that don't
;;;					    match the lambda's definition)
;;;				'subr-callargs (calls to subrs with args that
;;;					    don't match the subr's definition)
;;;				'redefine  (function cell redefined from
;;;					    a macro to a lambda or vice versa,
;;;					    or redefined to take other args)
;;;				'obsolete  (obsolete variables and functions)
;;;				'pedantic  (references to Emacs-compatible
;;;					    symbols)
;;; byte-compile-emacs19-compatibility	Whether the compiler should
;;;				generate .elc files which can be loaded into
;;;				generic emacs 19.
;;; emacs-lisp-file-regexp	Regexp for the extension of source-files;
;;;				see also the function `byte-compile-dest-file'.
;;; byte-compile-overwrite-file	If nil, delete old .elc files before saving.
;;;
;;; Most of the above parameters can also be set on a file-by-file basis; see
;;; the documentation of the `byte-compiler-options' macro.

;;; New Features:
;;;
;;;  o	The form `defsubst' is just like `defun', except that the function
;;;	generated will be open-coded in compiled code which uses it.  This
;;;	means that no function call will be generated, it will simply be
;;;	spliced in.  Lisp functions calls are very slow, so this can be a
;;;	big win.
;;;
;;;	You can generally accomplish the same thing with `defmacro', but in
;;;	that case, the defined procedure can't be used as an argument to
;;;	mapcar, etc.
;;;
;;;  o	You can make a given function be inline even if it has already been
;;;	defined with `defun' by using the `proclaim-inline' form like so:
;;;		(proclaim-inline my-function)
;;;	This is, in fact, exactly what `defsubst' does.  To make a function no
;;;	longer be inline, you must use `proclaim-notinline'.  Beware that if
;;;	you define a function with `defsubst' and later redefine it with
;;;	`defun', it will still be open-coded until you use `proclaim-notinline'.
;;;
;;;  o	You can also open-code one particular call to a function without
;;;	open-coding all calls.  Use the 'inline' form to do this, like so:
;;;
;;;		(inline (foo 1 2 3))	;; `foo' will be open-coded
;;;	or...
;;;		(inline			;;  `foo' and `baz' will be
;;;		 (foo 1 2 3 (bar 5))	;; open-coded, but `bar' will not.
;;;		 (baz 0))
;;;
;;;  o	It is possible to open-code a function in the same file it is defined
;;;	in without having to load that file before compiling it.  the
;;;	byte-compiler has been modified to remember function definitions in
;;;	the compilation environment in the same way that it remembers macro
;;;	definitions.
;;;
;;;  o  Forms like ((lambda ...) ...) are open-coded.
;;;
;;;  o  The form `eval-when-compile' is like `progn', except that the body
;;;     is evaluated at compile-time.  When it appears at top-level, this
;;;     is analogous to the Common Lisp idiom (eval-when (compile) ...).
;;;     When it does not appear at top-level, it is similar to the
;;;     Common Lisp #. reader macro (but not in interpreted code).
;;;
;;;  o  The form `eval-and-compile' is similar to `eval-when-compile',
;;;     but the whole form is evalled both at compile-time and at run-time.
;;;
;;;  o  The command M-x byte-compile-and-load-file does what you'd think.
;;;
;;;  o  The command `compile-defun' is analogous to `eval-defun'.
;;;
;;;  o  If you run `byte-compile-file' on a filename which is visited in a
;;;     buffer, and that buffer is modified, you are asked whether you want
;;;     to save the buffer before compiling.
;;;
;;;  o  You can add this to /etc/magic to make file(1) recognize the files
;;;     generated by this compiler:
;;;
;;;	  0	string		;ELC		GNU Emacs Lisp compiled file,
;;;	  >4	byte		x		version %d
;;;
;;; TO DO:
;;;
;;;  o	Should implement declarations and proclamations, notably special,
;;;	unspecial, and ignore.	Do this in such a way as to not break cl.el.
;;;  o	The bound-but-not-used warnings are not issued for variables whose
;;;	bindings were established in the arglist, due to the lack of an
;;;	ignore declaration.  Once ignore exists, this should be turned on.
;;;  o	Warn about functions and variables defined but not used?
;;;	Maybe add some kind of `export' declaration for this?
;;;	(With interactive functions being automatically exported?)
;;;  o	Any reference to a variable, even one which is a no-op, will cause
;;;	the warning not to be given.  Possibly we could use the for-effect
;;;	flag to determine when this reference is useless; possibly more
;;;	complex flow analysis would be necessary.
;;;  o  If the optimizer deletes a variable reference, we might be left with
;;;	a bound-but-not-referenced warning.  Generally this is ok, but not if
;;;	it's a synergistic result of macroexpansion.  Need some way to note
;;;	that a varref is being optimized away?  Of course it would be nice to
;;;	optimize away the binding too, someday, but it's unsafe today.
;;;  o	(See byte-optimize.el for the optimization TODO list.)

(require 'backquote)
(when purify-flag
  (require 'bytecomp-runtime)
  (require 'subr)
  (require 'replace)
  (require 'version)
  (require 'cl)
  (require 'cl-extra)
  (require 'custom)
  (require 'keymap)
  (require 'console)
  (require 'keydefs)
  (require 'lib-complete))

(or (fboundp 'defsubst)
    ;; This really ought to be loaded already!
    (load-library "bytecomp-runtime"))

(eval-when-compile
  (defvar byte-compile-single-version nil
    "If this is true, the choice of emacs version (v19 or v20) byte-codes will
be hard-coded into bytecomp when it compiles itself.  If the compiler itself
is compiled with optimization, this causes a speedup.")

  (cond
   (byte-compile-single-version
    (defmacro byte-compile-single-version () t)
    (defmacro byte-compile-version-cond (cond) (list 'quote (eval cond))))
   (t
    (defmacro byte-compile-single-version () nil)
    (defmacro byte-compile-version-cond (cond) cond)))
  )

(unless (fboundp #'compile-regexp)
  (defalias 'compile-regexp #'identity))

(defvar emacs-lisp-file-regexp (compile-regexp "\\.el$")
  "*Regexp which matches Emacs Lisp source files.
You may want to redefine `byte-compile-dest-file' if you change this.")

;; This enables file name handlers such as jka-compr
;; to remove parts of the file name that should not be copied
;; through to the output file name.
(defun byte-compiler-base-file-name (filename)
  (let ((handler (find-file-name-handler filename
					 'byte-compiler-base-file-name)))
    (if handler
	(funcall handler 'byte-compiler-base-file-name filename)
      filename)))

(unless (fboundp 'byte-compile-dest-file)
  ;; The user may want to redefine this along with emacs-lisp-file-regexp,
  ;; so only define it if it is undefined.
  (defun byte-compile-dest-file (filename)
    "Convert an Emacs Lisp source file name to a compiled file name."
    (setq filename (byte-compiler-base-file-name filename))
    (setq filename (file-name-sans-versions filename))
    (if (string-match emacs-lisp-file-regexp filename)
	(concat (substring filename 0 (match-beginning 0)) ".elc")
      (concat filename ".elc"))))

;; This can be the 'byte-compile property of any symbol.
(autoload 'byte-compile-inline-expand "byte-optimize")

;; This is the entrypoint to the lapcode optimizer pass1.
(autoload 'byte-optimize-form "byte-optimize")
;; This is the entrypoint to the lapcode optimizer pass2.
(autoload 'byte-optimize-lapcode "byte-optimize")
(autoload 'byte-compile-unfold-lambda "byte-optimize")

;; This is the entry point to the decompiler, which is used by the
;; disassembler.  The disassembler just requires 'byte-compile, but
;; that doesn't define this function, so this seems to be a reasonable
;; thing to do.
(autoload 'byte-decompile-bytecode "byte-optimize")

(defvar byte-compile-verbose
  (and (not noninteractive) (> (device-baud-rate) search-slow-speed))
  "*Non-nil means print messages describing progress of byte-compiler.")

(defvar byte-compile-emacs19-compatibility
  (not (emacs-version>= 20))
  "*Non-nil means generate output that can run in Emacs 19.")

(defvar byte-compile-print-gensym t
  "*Non-nil means generate code that creates unique symbols at run-time.
This is achieved by printing uninterned symbols using the `#:SYMBOL'
notation, so that they will be read uninterned when run.

With this feature, code that uses uninterned symbols in macros will
not be runnable under pre-21.0 XEmacsen.

When `byte-compile-emacs19-compatibility' is non-nil, this variable is
ignored and considered to be nil.")

(defvar byte-optimize t
  "*Enables optimization in the byte compiler.
nil means don't do any optimization.
t means do all optimizations.
`source' means do source-level optimizations only.
`byte' means do code-level optimizations only.")

(defvar byte-compile-delete-errors t
  "*If non-nil, the optimizer may delete forms that may signal an error.
This includes variable references and calls to functions such as `car'.")

;; XEmacs addition
(defvar byte-compile-new-bytecodes nil
  "This is completely ignored.  It is only around for backwards
compatibility.")


;; FSF enables byte-compile-dynamic-docstrings but not byte-compile-dynamic
;; by default.  This would be a reasonable conservative approach except
;; for the fact that if you enable either of these, you get incompatible
;; byte code that can't be read by XEmacs 19.13 or before or FSF 19.28 or
;; before.
;;
;; Therefore, neither is enabled for 19.14.  Both are enabled for 20.0
;; because we have no reason to be conservative about changing the
;; way things work. (Ben)

;; However, I don't think that defaulting byte-compile-dynamic to nil
;; is a compatibility issue - rather it is a performance issue.
;; Therefore I am setting byte-compile-dynamic back to nil. (mrb)

(defvar byte-compile-dynamic nil
  "*If non-nil, compile function bodies so they load lazily.
They are hidden comments in the compiled file, and brought into core when the
function is called.

To enable this option, make it a file-local variable
in the source file you want it to apply to.
For example, add  -*-byte-compile-dynamic: t;-*- on the first line.

When this option is true, if you load the compiled file and then move it,
the functions you loaded will not be able to run.")

(defvar byte-compile-dynamic-docstrings (emacs-version>= 20)
  "*If non-nil, compile doc strings for lazy access.
We bury the doc strings of functions and variables
inside comments in the file, and bring them into core only when they
are actually needed.

When this option is true, if you load the compiled file and then move it,
you won't be able to find the documentation of anything in that file.

To disable this option for a certain file, make it a file-local variable
in the source file.  For example, add this to the first line:
  -*-byte-compile-dynamic-docstrings:nil;-*-
You can also set the variable globally.

This option is enabled by default because it reduces Emacs memory usage.")

(defvar byte-optimize-log nil
  "*If true, the byte-compiler will log its optimizations into *Compile-Log*.
If this is 'source, then only source-level optimizations will be logged.
If it is 'byte, then only byte-level optimizations will be logged.")

(defvar byte-compile-error-on-warn nil
  "*If true, the byte-compiler reports warnings with `error'.")

;; byte-compile-warning-types in FSF.
(defvar byte-compile-default-warnings
  '(redefine callargs subr-callargs free-vars unresolved unused-vars obsolete)
  "*The warnings used when byte-compile-warnings is t.")

(defvar byte-compile-warnings t
  "*List of warnings that the compiler should issue (t for the default set).
Elements of the list may be:

  free-vars	references to variables not in the current lexical scope.
  unused-vars	references to non-global variables bound but not referenced.
  unresolved	calls to unknown functions.
  callargs	lambda calls with args that don't match the definition.
  subr-callargs	calls to subrs with args that don't match the definition.
  redefine	function cell redefined from a macro to a lambda or vice
		versa, or redefined to take a different number of arguments.
  obsolete	use of an obsolete function or variable.
  pedantic	warn of use of compatible symbols.

The default set is specified by `byte-compile-default-warnings' and
normally encompasses all possible warnings.

See also the macro `byte-compiler-options'.")

(defvar byte-compile-generate-call-tree nil
  "*Non-nil means collect call-graph information when compiling.
This records functions that were called and from where.
If the value is t, compilation displays the call graph when it finishes.
If the value is neither t nor nil, compilation asks you whether to display
the graph.

The call tree only lists functions called, not macros used. Those functions
which the byte-code interpreter knows about directly (eq, cons, etc.) are
not reported.

The call tree also lists those functions which are not known to be called
\(that is, to which no calls have been compiled).  Functions which can be
invoked interactively are excluded from this list.")

(defconst byte-compile-call-tree nil "Alist of functions and their call tree.
Each element looks like

  \(FUNCTION CALLERS CALLS\)

where CALLERS is a list of functions that call FUNCTION, and CALLS
is a list of functions for which calls were generated while compiling
FUNCTION.")

(defvar byte-compile-call-tree-sort 'name
  "*If non-nil, sort the call tree.
The values `name', `callers', `calls', `calls+callers'
specify different fields to sort on.")

(defvar byte-compile-overwrite-file t
  "If nil, old .elc files are deleted before the new is saved, and .elc
files will have the same modes as the corresponding .el file.  Otherwise,
existing .elc files will simply be overwritten, and the existing modes
will not be changed.  If this variable is nil, then an .elc file which
is a symbolic link will be turned into a normal file, instead of the file
which the link points to being overwritten.")

(defvar byte-recompile-directory-ignore-errors-p nil
  "If true, then `byte-recompile-directory' will continue compiling even
when an error occurs in a file.  This is bound to t by
`batch-byte-recompile-directory'.")

(defvar byte-recompile-directory-recursively t
  "*If true, then `byte-recompile-directory' will recurse on subdirectories.")

(defvar byte-compile-constants nil
  "list of all constants encountered during compilation of this form")
(defvar byte-compile-variables nil
  "list of all variables encountered during compilation of this form")
(defvar byte-compile-bound-variables nil
  "Alist of variables bound in the context of the current form,
that is, the current lexical environment.  This list lives partly
on the specbind stack.  The cdr of each cell is an integer bitmask.")

(defconst byte-compile-referenced-bit 1)
(defconst byte-compile-assigned-bit 2)
(defconst byte-compile-arglist-bit 4)
(defconst byte-compile-global-bit 8)

(defvar byte-compile-free-references)
(defvar byte-compile-free-assignments)

(defvar byte-compiler-error-flag)

;;; A form of eval that includes the currently defined macro definitions.
;;; This helps implement the promise made in the Lispref:
;;;
;;; "If a file being compiled contains a `defmacro' form, the macro is
;;; defined temporarily for the rest of the compilation of that file."
(defun byte-compile-eval (form)
  (let ((save-macro-environment nil))
    (unwind-protect
	(loop for (sym . def) in byte-compile-macro-environment do
	  (push
	   (if (fboundp sym) (cons sym (symbol-function sym)) sym)
	   save-macro-environment)
	  (fset sym (cons 'macro def))
	  finally return (eval form))
      (dolist (elt save-macro-environment)
	(if (symbolp elt)
	    (fmakunbound elt)
	  (fset (car elt) (cdr elt)))))))

(defconst byte-compile-initial-macro-environment
  '((byte-compiler-options . (lambda (&rest forms)
			       (apply 'byte-compiler-options-handler forms)))
    (eval-when-compile . (lambda (&rest body)
			   (list 'quote (byte-compile-eval (cons 'progn body)))))
    (eval-and-compile . (lambda (&rest body)
			  (byte-compile-eval (cons 'progn body))
			  (cons 'progn body))))
  "The default macro-environment passed to macroexpand by the compiler.
Placing a macro here will cause a macro to have different semantics when
expanded by the compiler as when expanded by the interpreter.")

(defvar byte-compile-macro-environment byte-compile-initial-macro-environment
  "Alist of macros defined in the file being compiled.
Each element looks like (MACRONAME . DEFINITION).  It is
\(MACRONAME . nil) when a macro is redefined as a function.")

(defvar byte-compile-function-environment nil
  "Alist of functions defined in the file being compiled.
This is so we can inline them when necessary.
Each element looks like (FUNCTIONNAME . DEFINITION).  It is
\(FUNCTIONNAME . nil) when a function is redefined as a macro.")

(defvar byte-compile-autoload-environment nil
 "Alist of functions and macros defined by autoload in the file being compiled.
This is so we can suppress warnings about calls to these functions, even though
they do not have `real' definitions.
Each element looks like (FUNCTIONNAME . CALL-TO-AUTOLOAD).")

(defvar byte-compile-unresolved-functions nil
  "Alist of undefined functions to which calls have been compiled (used for
warnings when the function is later defined with incorrect args).")

(defvar byte-compile-file-domain) ; domain of file being compiled

(defvar byte-compile-tag-number 0)
(defvar byte-compile-output nil
  "Alist describing contents to put in byte code string.
Each element is (INDEX . VALUE)")
(defvar byte-compile-depth 0 "Current depth of execution stack.")
(defvar byte-compile-maxdepth 0 "Maximum depth of execution stack.")


;;; The byte codes; this information is duplicated in bytecode.c

(defconst byte-code-vector nil
  "An array containing byte-code names indexed by byte-code values.")

(defconst byte-stack+-info nil
  "An array with the stack adjustment for each byte-code.")

(defmacro byte-defop (opcode stack-adjust opname &optional docstring)
  ;; This is a speed-hack for building the byte-code-vector at compile-time.
  ;; We fill in the vector at macroexpand-time, and then after the last call
  ;; to byte-defop, we write the vector out as a constant instead of writing
  ;; out a bunch of calls to aset.
  ;; Actually, we don't fill in the vector itself, because that could make
  ;; it problematic to compile big changes to this compiler; we store the
  ;; values on its plist, and remove them later in -extrude.
  (let ((v1 (or (get 'byte-code-vector 'tmp-compile-time-value)
		(put 'byte-code-vector 'tmp-compile-time-value
		     (make-vector 256 nil))))
	(v2 (or (get 'byte-stack+-info 'tmp-compile-time-value)
		(put 'byte-stack+-info 'tmp-compile-time-value
		     (make-vector 256 nil)))))
    (aset v1 opcode opname)
    (aset v2 opcode stack-adjust))
  (if docstring
      (list 'defconst opname opcode (concat "Byte code opcode " docstring "."))
      (list 'defconst opname opcode)))

(defmacro byte-extrude-byte-code-vectors ()
  (prog1 (list 'setq 'byte-code-vector
		     (get 'byte-code-vector 'tmp-compile-time-value)
		     'byte-stack+-info
		     (get 'byte-stack+-info 'tmp-compile-time-value))
    (remprop 'byte-code-vector 'tmp-compile-time-value)
    (remprop 'byte-stack+-info 'tmp-compile-time-value)))


;; unused: 0-7

;; These opcodes are special in that they pack their argument into the
;; opcode word.
;;
(byte-defop   8  1 byte-varref	"for variable reference")
(byte-defop  16 -1 byte-varset	"for setting a variable")
(byte-defop  24 -1 byte-varbind	"for binding a variable")
(byte-defop  32  0 byte-call	"for calling a function")
(byte-defop  40  0 byte-unbind	"for unbinding special bindings")
;; codes 8-47 are consumed by the preceding opcodes

;; unused: 48-55

(byte-defop  56 -1 byte-nth)
(byte-defop  57  0 byte-symbolp)
(byte-defop  58  0 byte-consp)
(byte-defop  59  0 byte-stringp)
(byte-defop  60  0 byte-listp)
(byte-defop  61 -1 byte-old-eq)
(byte-defop  62 -1 byte-old-memq)
(byte-defop  63  0 byte-not)
(byte-defop  64  0 byte-car)
(byte-defop  65  0 byte-cdr)
(byte-defop  66 -1 byte-cons)
(byte-defop  67  0 byte-list1)
(byte-defop  68 -1 byte-list2)
(byte-defop  69 -2 byte-list3)
(byte-defop  70 -3 byte-list4)
(byte-defop  71  0 byte-length)
(byte-defop  72 -1 byte-aref)
(byte-defop  73 -2 byte-aset)
(byte-defop  74  0 byte-symbol-value)
(byte-defop  75  0 byte-symbol-function) ; this was commented out
(byte-defop  76 -1 byte-set)
(byte-defop  77 -1 byte-fset) ; this was commented out
(byte-defop  78 -1 byte-get)
(byte-defop  79 -2 byte-substring)
(byte-defop  80 -1 byte-concat2)
(byte-defop  81 -2 byte-concat3)
(byte-defop  82 -3 byte-concat4)
(byte-defop  83  0 byte-sub1)
(byte-defop  84  0 byte-add1)
(byte-defop  85 -1 byte-eqlsign)
(byte-defop  86 -1 byte-gtr)
(byte-defop  87 -1 byte-lss)
(byte-defop  88 -1 byte-leq)
(byte-defop  89 -1 byte-geq)
(byte-defop  90 -1 byte-diff)
(byte-defop  91  0 byte-negate)
(byte-defop  92 -1 byte-plus)
(byte-defop  93 -1 byte-max)
(byte-defop  94 -1 byte-min)
(byte-defop  95 -1 byte-mult)
(byte-defop  96  1 byte-point)
(byte-defop  97 -1 byte-eq) ; new as of v20
(byte-defop  98  0 byte-goto-char)
(byte-defop  99  0 byte-insert)
(byte-defop 100  1 byte-point-max)
(byte-defop 101  1 byte-point-min)
(byte-defop 102  0 byte-char-after)
(byte-defop 103  1 byte-following-char)
(byte-defop 104  1 byte-preceding-char)
(byte-defop 105  1 byte-current-column)
(byte-defop 106  0 byte-indent-to)
(byte-defop 107 -1 byte-equal) ; new as of v20
(byte-defop 108  1 byte-eolp)
(byte-defop 109  1 byte-eobp)
(byte-defop 110  1 byte-bolp)
(byte-defop 111  1 byte-bobp)
(byte-defop 112  1 byte-current-buffer)
(byte-defop 113  0 byte-set-buffer)
(byte-defop 114  0 byte-save-current-buffer
  "To make a binding to record the current buffer.")
;;(byte-defop 114  1 byte-read-char-OBSOLETE) ;obsolete as of v19
(byte-defop 115 -1 byte-memq) ; new as of v20
(byte-defop 116  1 byte-interactive-p)

(byte-defop 117  0 byte-forward-char)
(byte-defop 118  0 byte-forward-word)
(byte-defop 119 -1 byte-skip-chars-forward)
(byte-defop 120 -1 byte-skip-chars-backward)
(byte-defop 121  0 byte-forward-line)
(byte-defop 122  0 byte-char-syntax)
(byte-defop 123 -1 byte-buffer-substring)
(byte-defop 124 -1 byte-delete-region)
(byte-defop 125 -1 byte-narrow-to-region)
(byte-defop 126  1 byte-widen)
(byte-defop 127  0 byte-end-of-line)

;; unused: 128

;; These store their argument in the next two bytes
(byte-defop 129  1 byte-constant2
   "for reference to a constant with vector index >= byte-constant-limit")
(byte-defop 130  0 byte-goto "for unconditional jump")
(byte-defop 131 -1 byte-goto-if-nil "to pop value and jump if it's nil")
(byte-defop 132 -1 byte-goto-if-not-nil
	    "to pop value and jump if it's not nil")
(byte-defop 133 -1 byte-goto-if-nil-else-pop
  "to examine top-of-stack, jump and don't pop it if it's nil,
otherwise pop it")
(byte-defop 134 -1 byte-goto-if-not-nil-else-pop
  "to examine top-of-stack, jump and don't pop it if it's non-nil,
otherwise pop it")

(byte-defop 135 -1 byte-return "to pop a value and return it from `byte-code'")
(byte-defop 136 -1 byte-discard "to discard one value from stack")
(byte-defop 137  1 byte-dup     "to duplicate the top of the stack")

(byte-defop 138  0 byte-save-excursion
  "to make a binding to record the buffer, point and mark")
(byte-defop 139  0 byte-save-window-excursion
  "to make a binding to record entire window configuration")
(byte-defop 140  0 byte-save-restriction
  "to make a binding to record the current buffer clipping restrictions")
(byte-defop 141 -1 byte-catch
  "for catch.  Takes, on stack, the tag and an expression for the body")
(byte-defop 142 -1 byte-unwind-protect
  "for unwind-protect.  Takes, on stack, an expression for the unwind-action")

;; For condition-case.  Takes, on stack, the variable to bind,
;; an expression for the body, and a list of clauses.
(byte-defop 143 -2 byte-condition-case)

;; For entry to with-output-to-temp-buffer.
;; Takes, on stack, the buffer name.
;; Binds standard-output and does some other things.
;; Returns with temp buffer on the stack in place of buffer name.
(byte-defop 144  0 byte-temp-output-buffer-setup)

;; For exit from with-output-to-temp-buffer.
;; Expects the temp buffer on the stack underneath value to return.
;; Pops them both, then pushes the value back on.
;; Unbinds standard-output and makes the temp buffer visible.
(byte-defop 145 -1 byte-temp-output-buffer-show)

;; To unbind back to the beginning of this frame.
;; Not used yet, but will be needed for tail-recursion elimination.
(byte-defop 146  0 byte-unbind-all)

(byte-defop 147 -2 byte-set-marker)
(byte-defop 148  0 byte-match-beginning)
(byte-defop 149  0 byte-match-end)
(byte-defop 150  0 byte-upcase)
(byte-defop 151  0 byte-downcase)
(byte-defop 152 -1 byte-string=)
(byte-defop 153 -1 byte-string<)
(byte-defop 154 -1 byte-old-equal)
(byte-defop 155 -1 byte-nthcdr)
(byte-defop 156 -1 byte-elt)
(byte-defop 157 -1 byte-old-member)
(byte-defop 158 -1 byte-old-assq)
(byte-defop 159  0 byte-nreverse)
(byte-defop 160 -1 byte-setcar)
(byte-defop 161 -1 byte-setcdr)
(byte-defop 162  0 byte-car-safe)
(byte-defop 163  0 byte-cdr-safe)
(byte-defop 164 -1 byte-nconc)
(byte-defop 165 -1 byte-quo)
(byte-defop 166 -1 byte-rem)
(byte-defop 167  0 byte-numberp)
(byte-defop 168  0 byte-integerp)

;; unused: 169

;; These are not present in FSF.
;;
(byte-defop 170  0 byte-rel-goto)
(byte-defop 171 -1 byte-rel-goto-if-nil)
(byte-defop 172 -1 byte-rel-goto-if-not-nil)
(byte-defop 173 -1 byte-rel-goto-if-nil-else-pop)
(byte-defop 174 -1 byte-rel-goto-if-not-nil-else-pop)

(byte-defop 175 nil byte-listN)
(byte-defop 176 nil byte-concatN)
(byte-defop 177 nil byte-insertN)

;; unused: 178-181

;; these ops are new to v20
(byte-defop 182 -1 byte-member)
(byte-defop 183 -1 byte-assq)

;; unused: 184-191

(byte-defop 192  1 byte-constant	"for reference to a constant")
;; codes 193-255 are consumed by byte-constant.
(defconst byte-constant-limit 64
  "Exclusive maximum index usable in the `byte-constant' opcode.")

(defconst byte-goto-ops
  '(byte-goto byte-goto-if-nil byte-goto-if-not-nil
	      byte-goto-if-nil-else-pop
	      byte-goto-if-not-nil-else-pop)
  "List of byte-codes whose offset is a pc.")

(defconst byte-goto-always-pop-ops
  '(byte-goto-if-nil byte-goto-if-not-nil))

(defconst byte-rel-goto-ops
  '(byte-rel-goto byte-rel-goto-if-nil byte-rel-goto-if-not-nil
		  byte-rel-goto-if-nil-else-pop byte-rel-goto-if-not-nil-else-pop)
  "byte-codes for relative jumps.")

(byte-extrude-byte-code-vectors)

;;; lapcode generator
;;;
;;; the byte-compiler now does source -> lapcode -> bytecode instead of
;;; source -> bytecode, because it's a lot easier to make optimizations
;;; on lapcode than on bytecode.
;;;
;;; Elements of the lapcode list are of the form (<instruction> . <parameter>)
;;; where instruction is a symbol naming a byte-code instruction,
;;; and parameter is an argument to that instruction, if any.
;;;
;;; The instruction can be the pseudo-op TAG, which means that this position
;;; in the instruction stream is a target of a goto.  (car PARAMETER) will be
;;; the PC for this location, and the whole instruction "(TAG pc)" will be the
;;; parameter for some goto op.
;;;
;;; If the operation is varbind, varref, varset or push-constant, then the
;;; parameter is (variable/constant . index_in_constant_vector).
;;;
;;; First, the source code is macroexpanded and optimized in various ways.
;;; Then the resultant code is compiled into lapcode.  Another set of
;;; optimizations are then run over the lapcode.  Then the variables and
;;; constants referenced by the lapcode are collected and placed in the
;;; constants-vector.  (This happens now so that variables referenced by dead
;;; code don't consume space.)  And finally, the lapcode is transformed into
;;; compacted byte-code.
;;;
;;; A distinction is made between variables and constants because the variable-
;;; referencing instructions are more sensitive to the variables being near the
;;; front of the constants-vector than the constant-referencing instructions.
;;; Also, this lets us notice references to free variables.

(defun byte-compile-lapcode (lap)
  "Turns lapcode into bytecode.  The lapcode is destroyed."
  ;; Lapcode modifications: changes the ID of a tag to be the tag's PC.
  (let ((pc 0)			; Program counter
	op off			; Operation & offset
	(bytes '())		; Put the output bytes here
	(patchlist nil)		; List of tags and goto's to patch
	rest rel tmp)
    (while lap
      (setq op (car (car lap))
	    off (cdr (car lap)))
      (cond ((not (symbolp op))
	     (error "Non-symbolic opcode `%s'" op))
	    ((eq op 'TAG)
	     (setcar off pc)
	     (push off patchlist))
	    ((memq op byte-goto-ops)
	     (setq pc (+ pc 3))
	     (setq bytes (cons (cons pc (cdr off))
			       (cons nil
				     (cons (symbol-value op) bytes))))
	     (push bytes patchlist))
	    (t
	     (setq bytes
		   (cond ((cond ((consp off)
				 ;; Variable or constant reference
				 (setq off (cdr off))
				 (eq op 'byte-constant)))
			  (cond ((< off byte-constant-limit)
				 (setq pc (1+ pc))
				 (cons (+ byte-constant off) bytes))
				(t
				 (setq pc (+ 3 pc))
				 (cons (lsh off -8)
				       (cons (logand off 255)
					     (cons byte-constant2 bytes))))))
			 ((and (<= byte-listN (symbol-value op))
			       (<= (symbol-value op) byte-insertN))
			  (setq pc (+ 2 pc))
			  (cons off (cons (symbol-value op) bytes)))
			 ((< off 6)
			  (setq pc (1+ pc))
			  (cons (+ (symbol-value op) off) bytes))
			 ((< off 256)
			  (setq pc (+ 2 pc))
			  (cons off (cons (+ (symbol-value op) 6) bytes)))
			 (t
			  (setq pc (+ 3 pc))
			  (cons (lsh off -8)
				(cons (logand off 255)
				      (cons (+ (symbol-value op) 7)
					    bytes))))))))
      (setq lap (cdr lap)))
    ;;(if (not (= pc (length bytes)))
    ;;    (error "Compiler error: pc mismatch - %s %s" pc (length bytes)))
    (cond (t ;; starting with Emacs 19.
	   ;; Make relative jumps
	   (setq patchlist (nreverse patchlist))
	   (while (progn
		    (setq off 0)	; PC change because of deleted bytes
		    (setq rest patchlist)
		    (while rest
		      (setq tmp (car rest))
		      (and (consp (car tmp)) ; Jump
			   (prog1 (null (nth 1 tmp)) ; Absolute jump
			     (setq tmp (car tmp)))
			   (progn
			     (setq rel (- (car (cdr tmp)) (car tmp)))
			     (and (<= -129 rel) (< rel 128)))
			   (progn
			     ;; Convert to relative jump.
			     (setcdr (car rest) (cdr (cdr (car rest))))
			     (setcar (cdr (car rest))
				     (+ (car (cdr (car rest)))
					(- byte-rel-goto byte-goto)))
			     (setq off (1- off))))
		      (setcar tmp (+ (car tmp) off)) ; Adjust PC
		      (setq rest (cdr rest)))
		    ;; If optimizing, repeat until no change.
		    (and byte-optimize
			 (not (zerop off)))))))
    ;; Patch PC into jumps
    (let (bytes)
      (while patchlist
	(setq bytes (car patchlist))
	(cond ((atom (car bytes)))	; Tag
	      ((nth 1 bytes)		; Relative jump
	       (setcar bytes (+ (- (car (cdr (car bytes))) (car (car bytes)))
				128)))
	      (t			; Absolute jump
	       (setq pc (car (cdr (car bytes))))	; Pick PC from tag
	       (setcar (cdr bytes) (logand pc 255))
	       (setcar bytes (lsh pc -8))))
	(setq patchlist (cdr patchlist))))
    (concat (nreverse bytes))))


;;; byte compiler messages

(defvar byte-compile-current-form nil)
(defvar byte-compile-current-file nil)
(defvar byte-compile-dest-file nil)

(defmacro byte-compile-log (format-string &rest args)
  `(when (and byte-optimize (memq byte-optimize-log '(t source)))
      (let ((print-escape-newlines t)
	    (print-level 4)
	    (print-length 4))
	(byte-compile-log-1 (format ,format-string ,@args)))))

(defconst byte-compile-last-warned-form 'nothing)

;; Log a message STRING in *Compile-Log*.
;; Also log the current function and file if not already done.
(defun byte-compile-log-1 (string &optional fill)
  (let* ((this-form (or byte-compile-current-form "toplevel forms"))
	 (while-compiling-msg
	  (when (or byte-compile-current-file
		    (not (eq this-form byte-compile-last-warned-form)))
	    (format
	     "While compiling %s%s:"
	     this-form
	     (cond
	      ((stringp byte-compile-current-file)
	       (concat " in file " byte-compile-current-file))
	      ((bufferp byte-compile-current-file)
	       (concat " in buffer "
		       (buffer-name byte-compile-current-file)))
	      (""))))))
    (if noninteractive
	(progn
	  (when while-compiling-msg (message "%s" while-compiling-msg))
	  (message "  %s" string))
      (with-current-buffer (get-buffer-create "*Compile-Log*")
	(goto-char (point-max))
	(when byte-compile-current-file
	  (when (> (point-max) (point-min))
	    (insert "\n\^L\n"))
	  (insert (current-time-string) "\n"))
	(when while-compiling-msg (insert while-compiling-msg "\n"))
	(insert "  " string "\n")
	(when (and fill (not (string-match "\n" string)))
	  (let ((fill-prefix "     ")
		(fill-column 78))
	    (fill-paragraph nil)))))
    (setq byte-compile-current-file nil)
    (setq byte-compile-last-warned-form this-form)))

;; Log the start of a file in *Compile-Log*, and mark it as done.
;; But do nothing in batch mode.
(defun byte-compile-log-file ()
  (when (and byte-compile-current-file (not noninteractive))
    (with-current-buffer (get-buffer-create "*Compile-Log*")
      (when (> (point-max) (point-min))
	(goto-char (point-max))
	(insert "\n\^L\n"))
      (insert "Compiling "
	      (if (stringp byte-compile-current-file)
		  (concat "file " byte-compile-current-file)
		(concat "buffer " (buffer-name byte-compile-current-file)))
	      " at " (current-time-string) "\n")
      (setq byte-compile-current-file nil))))

(defun byte-compile-warn (format &rest args)
  (setq format (apply 'format format args))
  (if byte-compile-error-on-warn
      (error "%s" format)		; byte-compile-file catches and logs it
    (byte-compile-log-1 (concat "** " format) t)
;;; RMS says:
;;; It is useless to flash warnings too fast to be read.
;;; Besides, they will all be shown at the end.
;;; and comments out the next two lines.
    (or noninteractive  ; already written on stdout.
	(message "Warning: %s" format))))

;;; This function should be used to report errors that have halted
;;; compilation of the current file.
(defun byte-compile-report-error (error-info)
  (setq byte-compiler-error-flag t)
  (byte-compile-log-1
   (concat "!! "
	   (format (if (cdr error-info) "%s (%s)" "%s")
		   (get (car error-info) 'error-message)
		   (prin1-to-string (cdr error-info)))))
  (if stack-trace-on-error
      (backtrace nil t)))

;;; Used by make-obsolete.
(defun byte-compile-obsolete (form)
  (let ((new (get (car form) 'byte-obsolete-info)))
    (if (memq 'obsolete byte-compile-warnings)
	(byte-compile-warn "%s is an obsolete function; %s" (car form)
			   (if (stringp (car new))
			       (car new)
			     (format "use %s instead." (car new)))))
    (funcall (or (cdr new) 'byte-compile-normal-call) form)))

;;; Used by make-obsolete.
(defun byte-compile-compatible (form)
  (let ((new (get (car form) 'byte-compatible-info)))
    (if (memq 'pedantic byte-compile-warnings)
	(byte-compile-warn "%s is provided for compatibility; %s" (car form)
			   (if (stringp (car new))
			       (car new)
			     (format "use %s instead." (car new)))))
    (funcall (or (cdr new) 'byte-compile-normal-call) form)))

;; Compiler options

(defconst byte-compiler-legal-options
  '((optimize byte-optimize (t nil source byte) val)
    (file-format byte-compile-emacs19-compatibility (emacs19 emacs20)
		 (eq val 'emacs19))
    (delete-errors byte-compile-delete-errors (t nil) val)
    (verbose byte-compile-verbose (t nil) val)
    (new-bytecodes byte-compile-new-bytecodes (t nil) val)
    (warnings byte-compile-warnings
	      ((callargs subr-callargs redefine free-vars unused-vars unresolved))
	      val)))

;; XEmacs addition
(defconst byte-compiler-obsolete-options
  '((new-bytecodes t)))

;; Inhibit v19/v20 selectors if the version is hardcoded.
;; #### This should print a warning if the user tries to change something
;; than can't be changed because the running compiler doesn't support it.
(cond
 ((byte-compile-single-version)
  (setcar (cdr (cdr (assq 'file-format byte-compiler-legal-options)))
	  (if (byte-compile-version-cond byte-compile-emacs19-compatibility)
	      '(emacs19) '(emacs20)))))

;; now we can copy it.
(setq byte-compiler-legal-options byte-compiler-legal-options)

(defun byte-compiler-options-handler (&rest args)
  (let (key val desc choices)
    (while args
      (if (or (atom (car args)) (nthcdr 2 (car args)) (null (cdr (car args))))
	  (error "malformed byte-compiler-option %s" (car args)))
      (setq key (car (car args))
	    val (car (cdr (car args)))
	    desc (assq key byte-compiler-legal-options))
      (or desc
	  (error "unknown byte-compiler option %s" key))
      (if (assq key byte-compiler-obsolete-options)
	  (byte-compile-warn "%s is an obsolete byte-compiler option." key))
      (setq choices (nth 2 desc))
      (if (consp (car choices))
	  (let* (this
		 (handler 'cons)
		 (var (nth 1 desc))
		 (ret (and (memq (car val) '(+ -))
			   (copy-sequence (if (eq t (symbol-value var))
					      (car choices)
					    (symbol-value var))))))
	    (setq choices (car  choices))
	    (while val
	      (setq this (car val))
	      (cond ((memq this choices)
		     (setq ret (funcall handler this ret)))
		    ((eq this '+) (setq handler 'cons))
		    ((eq this '-) (setq handler 'delq))
		    ((error "%s only accepts %s." key choices)))
	      (setq val (cdr val)))
	    (set (nth 1 desc) ret))
	(or (memq val choices)
	    (error "%s must be one of %s." key choices))
	(set (nth 1 desc) (eval (nth 3 desc))))
      (setq args (cdr args)))
    nil))

;;; sanity-checking arglists

(defun byte-compile-fdefinition (name macro-p)
  (let* ((list (if (memq macro-p '(nil subr))
		   byte-compile-function-environment
		 byte-compile-macro-environment))
	 (env (cdr (assq name list))))
    (or env
	(let ((fn name))
	  (while (and (symbolp fn)
		      (fboundp fn)
		      (or (symbolp (symbol-function fn))
			  (consp (symbol-function fn))
			  (and (not macro-p)
			       (compiled-function-p (symbol-function fn)))
			  (and (eq macro-p 'subr) (subrp fn))))
	    (setq fn (symbol-function fn)))
	  (if (or (and (not macro-p) (compiled-function-p fn))
		  (and (eq macro-p 'subr) (subrp fn)))
	      fn
	    (and (consp fn)
		 (not (eq macro-p 'subr))
		 (if (eq 'macro (car fn))
		     (cdr fn)
		   (if macro-p
		       nil
		     (if (eq 'autoload (car fn))
			 nil
		       fn)))))))))

(defun byte-compile-arglist-signature (arglist)
  (let ((args 0)
	opts
	restp)
    (while arglist
      (cond ((eq (car arglist) '&optional)
	     (or opts (setq opts 0)))
	    ((eq (car arglist) '&rest)
	     (if (cdr arglist)
		 (setq restp t
		       arglist nil)))
	    (t
	     (if opts
		 (setq opts (1+ opts))
		 (setq args (1+ args)))))
      (setq arglist (cdr arglist)))
    (cons args (if restp nil (if opts (+ args opts) args)))))


(defun byte-compile-arglist-signatures-congruent-p (old new)
  (not (or
	 (> (car new) (car old))  ; requires more args now
	 (and (null (cdr old))    ; tooks rest-args, doesn't any more
	      (cdr new))
	 (and (cdr new) (cdr old) ; can't take as many args now
	      (< (cdr new) (cdr old)))
	 )))

(defun byte-compile-arglist-signature-string (signature)
  (cond ((null (cdr signature))
	 (format "%d+" (car signature)))
	((= (car signature) (cdr signature))
	 (format "%d" (car signature)))
	(t (format "%d-%d" (car signature) (cdr signature)))))


;; Warn if the form is calling a function with the wrong number of arguments.
(defun byte-compile-callargs-warn (form)
  (let* ((def (or (byte-compile-fdefinition (car form) nil)
		  (byte-compile-fdefinition (car form) t)))
	 (sig (and def (byte-compile-arglist-signature
			 (if (eq 'lambda (car-safe def))
			     (nth 1 def)
			   (if (compiled-function-p def)
			       (compiled-function-arglist def)
			     '(&rest def))))))
	 (ncall (length (cdr form))))
    (if (and (null def)
	     (fboundp 'subr-min-args)
	     (setq def (byte-compile-fdefinition (car form) 'subr)))
	(setq sig (cons (subr-min-args def) (subr-max-args def))))
    (if sig
	(if (or (< ncall (car sig))
		(and (cdr sig) (> ncall (cdr sig))))
	    (byte-compile-warn
	      "%s called with %d argument%s, but %s %s"
	      (car form) ncall
	      (if (= 1 ncall) "" "s")
	      (if (< ncall (car sig))
		  "requires"
		  "accepts only")
	      (byte-compile-arglist-signature-string sig)))
      (or (fboundp (car form)) ; might be a subr or autoload.
	  ;; ## this doesn't work with recursion.
	  (eq (car form) byte-compile-current-form)
	  ;; It's a currently-undefined function.
	  ;; Remember number of args in call.
	  (let ((cons (assq (car form) byte-compile-unresolved-functions))
		(n (length (cdr form))))
	    (if cons
		(or (memq n (cdr cons))
		    (setcdr cons (cons n (cdr cons))))
		(setq byte-compile-unresolved-functions
		      (cons (list (car form) n)
			    byte-compile-unresolved-functions))))))))

;; Warn if the function or macro is being redefined with a different
;; number of arguments.
(defun byte-compile-arglist-warn (form macrop)
  (let ((old (byte-compile-fdefinition (nth 1 form) macrop)))
    (if old
	(let ((sig1 (byte-compile-arglist-signature
		      (if (eq 'lambda (car-safe old))
			  (nth 1 old)
			(if (compiled-function-p old)
			    (compiled-function-arglist old)
			  '(&rest def)))))
	      (sig2 (byte-compile-arglist-signature (nth 2 form))))
	  (or (byte-compile-arglist-signatures-congruent-p sig1 sig2)
	      (byte-compile-warn "%s %s used to take %s %s, now takes %s"
		(if (eq (car form) 'defun) "function" "macro")
		(nth 1 form)
		(byte-compile-arglist-signature-string sig1)
		(if (equal sig1 '(1 . 1)) "argument" "arguments")
		(byte-compile-arglist-signature-string sig2))))
      ;; This is the first definition.  See if previous calls are compatible.
      (let ((calls (assq (nth 1 form) byte-compile-unresolved-functions))
	    nums sig min max)
	(if calls
	    (progn
	      (setq sig (byte-compile-arglist-signature (nth 2 form))
		    nums (sort (copy-sequence (cdr calls)) (function <))
		    min (car nums)
		    max (car (nreverse nums)))
	      (if (or (< min (car sig))
		      (and (cdr sig) (> max (cdr sig))))
		  (byte-compile-warn
	    "%s being defined to take %s%s, but was previously called with %s"
	            (nth 1 form)
		    (byte-compile-arglist-signature-string sig)
		    (if (equal sig '(1 . 1)) " arg" " args")
		    (byte-compile-arglist-signature-string (cons min max))))

	      (setq byte-compile-unresolved-functions
		    (delq calls byte-compile-unresolved-functions)))))
      )))

;; If we have compiled any calls to functions which are not known to be
;; defined, issue a warning enumerating them.
;; `unresolved' in the list `byte-compile-warnings' disables this.
(defun byte-compile-warn-about-unresolved-functions (&optional msg)
  (if (memq 'unresolved byte-compile-warnings)
   (let ((byte-compile-current-form (or msg "the end of the data")))
     ;; First delete the autoloads from the list.
     (if byte-compile-autoload-environment
	 (let ((rest byte-compile-unresolved-functions))
	   (while rest
	     (if (assq (car (car rest)) byte-compile-autoload-environment)
		 (setq byte-compile-unresolved-functions
		       (delq (car rest) byte-compile-unresolved-functions)))
	     (setq rest (cdr rest)))))
     ;; Now warn.
     (if (cdr byte-compile-unresolved-functions)
	 (let* ((str "The following functions are not known to be defined: ")
		(L (+ (length str) 5))
		(rest (reverse byte-compile-unresolved-functions))
		s)
	   (while rest
	     (setq s (symbol-name (car (car rest)))
		   L (+ L (length s) 2)
		   rest (cdr rest))
	     (if (<= L (1- fill-column))
		 (setq str (concat str " " s (and rest ",")))
	       (setq str (concat str "\n    " s (and rest ","))
		     L (+ (length s) 4))))
	   (byte-compile-warn "%s" str))
       (if byte-compile-unresolved-functions
	   (byte-compile-warn "the function %s is not known to be defined."
	    (car (car byte-compile-unresolved-functions)))))))
  nil)

(defun byte-compile-defvar-p (var)
  ;; Whether the byte compiler thinks that non-lexical references to this
  ;; variable are ok.
  (or (globally-boundp var)
      (let ((rest byte-compile-bound-variables))
	(while (and rest var)
	  (if (and (eq var (car-safe (car rest)))
		   (not (= 0 (logand (cdr (car rest))
				     byte-compile-global-bit))))
	      (setq var nil))
	  (setq rest (cdr rest)))
	;; if var is nil at this point, it's a defvar in this file.
	(not var))
      ;; Perhaps (eval-when-compile (defvar foo))
      (and (boundp 'current-load-list)
	   (memq var current-load-list))))


;;; If we have compiled bindings of variables which have no referents, warn.
(defun byte-compile-warn-about-unused-variables ()
  (let ((rest byte-compile-bound-variables)
	(unreferenced '())
	cell)
    (while (and rest
		;; only warn about variables whose lifetime is now ending,
		;; that is, variables from the lexical scope that is now
		;; terminating.  (Think nested lets.)
		(not (eq (car rest) 'new-scope)))
      (setq cell (car rest))
      (if (and (= 0 (logand byte-compile-referenced-bit (cdr cell)))
	       ;; Don't warn about declared-but-unused arguments,
	       ;; for two reasons: first, the arglist structure
	       ;; might be imposed by external forces, and we don't
	       ;; have (declare (ignore x)) yet; and second, inline
	       ;; expansion produces forms like
	       ;;   ((lambda (arg) (byte-code "..." [arg])) x)
	       ;; which we can't (ok, well, don't) recognize as
	       ;; containing a reference to arg, so every inline
	       ;; expansion would generate a warning.  (If we had
	       ;; `ignore' then inline expansion could emit an
	       ;; ignore declaration.)
	       (= 0 (logand byte-compile-arglist-bit (cdr cell)))
	       ;; Don't warn about defvars because this is a
	       ;; legitimate special binding.
	       (not (byte-compile-defvar-p (car cell))))
	  (setq unreferenced (cons (car cell) unreferenced)))
      (setq rest (cdr rest)))
    (setq unreferenced (nreverse unreferenced))
    (while unreferenced
      (byte-compile-warn
       "variable %s bound but not referenced" (car unreferenced))
      (setq unreferenced (cdr unreferenced)))))


(defmacro byte-compile-constant-symbol-p (symbol)
  `(or (keywordp ,symbol) (memq ,symbol '(nil t))))

(defmacro byte-compile-constp (form)
  ;; Returns non-nil if FORM is a constant.
  `(cond ((consp ,form) (eq (car ,form) 'quote))
	 ((symbolp ,form) (byte-compile-constant-symbol-p ,form))
	 (t)))

(defmacro byte-compile-close-variables (&rest body)
  `(let
       (;;
	;; Close over these variables to encapsulate the
	;; compilation state
	;;
	(byte-compile-macro-environment
	 ;; Copy it because the compiler may patch into the
	 ;; macroenvironment.
	 (copy-alist byte-compile-initial-macro-environment))
	(byte-compile-function-environment nil)
	(byte-compile-autoload-environment nil)
	(byte-compile-unresolved-functions nil)
	(byte-compile-bound-variables nil)
	(byte-compile-free-references nil)
	(byte-compile-free-assignments nil)
	;;
	;; Close over these variables so that `byte-compiler-options'
	;; can change them on a per-file basis.
	;;
	(byte-compile-verbose byte-compile-verbose)
	(byte-optimize byte-optimize)
	(byte-compile-emacs19-compatibility
	 byte-compile-emacs19-compatibility)
	(byte-compile-dynamic byte-compile-dynamic)
	(byte-compile-dynamic-docstrings
	 byte-compile-dynamic-docstrings)
	(byte-compile-warnings (if (eq byte-compile-warnings t)
				   byte-compile-default-warnings
				 byte-compile-warnings))
	(byte-compile-file-domain nil))
     (prog1
	 (progn ,@body)
       (if (memq 'unused-vars byte-compile-warnings)
	   ;; done compiling in this scope, warn now.
	   (byte-compile-warn-about-unused-variables)))))


(defmacro displaying-byte-compile-warnings (&rest body)
  `(let* ((byte-compile-log-buffer (get-buffer-create "*Compile-Log*"))
	  (byte-compile-point-max-prev (point-max byte-compile-log-buffer)))
     ;; Log the file name or buffer name.
     (byte-compile-log-file)
     ;; Record how much is logged now.
     ;; We will display the log buffer if anything more is logged
     ;; before the end of BODY.
     (defvar byte-compile-warnings-beginning)
     (let ((byte-compile-warnings-beginning
	    (if (boundp 'byte-compile-warnings-beginning)
		byte-compile-warnings-beginning
	      (point-max byte-compile-log-buffer))))

       (unwind-protect
	   (call-with-condition-handler
	       #'(lambda (error-info)
		   (byte-compile-report-error error-info))
	       #'(lambda ()
		   (progn ,@body)))
	 ;; Always set point in log to start of interesting output.
	 (with-current-buffer byte-compile-log-buffer
	   (let ((show-begin
		  (progn (goto-char byte-compile-point-max-prev)
			 (skip-chars-forward "\^L\n")
			 (point))))
	     ;; If there were compilation warnings, display them.
	     (if temp-buffer-show-function
		 (let ((show-buffer (get-buffer-create "*Compile-Log-Show*")))
		   ;; Always clean show-buffer, even when not displaying it,
		   ;; so that misleading previous messages aren't left around.
		   (with-current-buffer show-buffer
		     (setq buffer-read-only nil)
		     (erase-buffer))
		   (copy-to-buffer show-buffer show-begin (point-max))
		   (when (< byte-compile-warnings-beginning (point-max))
		     (funcall temp-buffer-show-function show-buffer)))
	       (when (< byte-compile-warnings-beginning (point-max))
		 (select-window
		  (prog1 (selected-window)
		    (select-window (display-buffer (current-buffer)))
		    (goto-char show-begin)
		    (recenter 1)))))))))))


;;;###autoload
(defun byte-force-recompile (directory)
  "Recompile every `.el' file in DIRECTORY that already has a `.elc' file.
Files in subdirectories of DIRECTORY are processed also."
  (interactive "DByte force recompile (directory): ")
  (byte-recompile-directory directory nil nil t))

;;;###autoload
(defun byte-recompile-directory (directory &optional arg norecursion force)
  "Recompile every `.el' file in DIRECTORY that needs recompilation.
This is if a `.elc' file exists but is older than the `.el' file.
Files in subdirectories of DIRECTORY are also processed unless
optional argument NORECURSION is non-nil.

If the `.elc' file does not exist, normally the `.el' file is *not* compiled.
But a prefix argument (optional second arg) means ask user,
for each such `.el' file, whether to compile it.  Prefix argument 0 means
don't ask and compile the file anyway.

A nonzero prefix argument also means ask about each subdirectory.

If the fourth optional argument FORCE is non-nil,
recompile every `.el' file that already has a `.elc' file."
  (interactive "DByte recompile directory: \nP")
  (if arg
      (setq arg (prefix-numeric-value arg)))
  (if noninteractive
      nil
    (save-some-buffers)
    (redraw-modeline))
  (let ((directories (list (expand-file-name directory)))
	(file-count 0)
	(dir-count 0)
	last-dir)
    (displaying-byte-compile-warnings
     (while directories
       (setq directory (file-name-as-directory (car directories)))
       (or noninteractive (message "Checking %s..." directory))
       (let ((files (directory-files directory))
	     source dest)
	 (while files
	   (setq source (expand-file-name (car files) directory))
	   (if (and (not (member (car files) '("." ".." "RCS" "CVS" "SCCS")))
		    ;; Stay away from directory back-links, etc:
		    (not (file-symlink-p source))
		    (file-directory-p source)
		    byte-recompile-directory-recursively)
	       ;; This file is a subdirectory.  Handle them differently.
	       (if (or (null arg)
		       (eq arg 0)
		       (y-or-n-p (concat "Check " source "? ")))
		   (setq directories
			 (nconc directories (list source))))
	     ;; It is an ordinary file.  Decide whether to compile it.
	     (if (and (string-match emacs-lisp-file-regexp source)
		      (not (auto-save-file-name-p source))
		      (setq dest (byte-compile-dest-file source))
		      (if (file-exists-p dest)
			  ;; File was already compiled.
			  (or force (file-newer-than-file-p source dest))
			;; No compiled file exists yet.
			(and arg
			     (or (eq 0 arg)
				 (y-or-n-p (concat "Compile " source "? "))))))
		 (progn ;(if (and noninteractive (not byte-compile-verbose))
			;    (message "Compiling %s..." source))
		        ; we do this in byte-compile-file.
		        (if byte-recompile-directory-ignore-errors-p
			     (batch-byte-compile-1 source)
			  (byte-compile-file source))
			(or noninteractive
			    (message "Checking %s..." directory))
			(setq file-count (1+ file-count))
			(if (not (eq last-dir directory))
			    (setq last-dir directory
				  dir-count (1+ dir-count)))
			)))
	   (setq files (cdr files))))
       (setq directories (cdr directories))))
    (message "Done (Total of %d file%s compiled%s)"
	     file-count (if (= file-count 1) "" "s")
	     (if (> dir-count 1) (format " in %d directories" dir-count) ""))))

;;;###autoload
(defun byte-recompile-file (filename &optional force)
  "Recompile a file of Lisp code named FILENAME if it needs recompilation.
This is if the `.elc' file exists but is older than the `.el' file.

If the `.elc' file does not exist, normally the `.el' file is *not*
compiled.  But a prefix argument (optional second arg) means ask user
whether to compile it.  Prefix argument 0 don't ask and recompile anyway."
  (interactive "fByte recompile file: \nP")
  (let ((dest))
    (if (and (string-match emacs-lisp-file-regexp filename)
	     (not (auto-save-file-name-p filename))
	     (setq dest (byte-compile-dest-file filename))
	     (if (file-exists-p dest)
		 (file-newer-than-file-p filename dest)
	       (and force
		    (or (eq 0 force)
			(y-or-n-p (concat "Compile " filename "? "))))))
	(byte-compile-file filename))))

;;;###autoload
(defun byte-compile-file (filename &optional load)
  "Compile a file of Lisp code named FILENAME into a file of byte code.
The output file's name is made by appending `c' to the end of FILENAME.
With prefix arg (noninteractively: 2nd arg), load the file after compiling."
;;  (interactive "fByte compile file: \nP")
  (interactive
   (let ((file buffer-file-name)
	 (file-name nil)
	 (file-dir nil))
     (and file
	  (eq (cdr (assq 'major-mode (buffer-local-variables)))
	      'emacs-lisp-mode)
	  (setq file-name (file-name-nondirectory file)
		file-dir (file-name-directory file)))
     (list (read-file-name (if current-prefix-arg
			       "Byte compile and load file: "
			     "Byte compile file: ")
			   file-dir nil nil file-name)
	   current-prefix-arg)))
  ;; Expand now so we get the current buffer's defaults
  (setq filename (expand-file-name filename))

  ;; If we're compiling a file that's in a buffer and is modified, offer
  ;; to save it first.
  (or noninteractive
      (let ((b (get-file-buffer (expand-file-name filename))))
	(if (and b (buffer-modified-p b)
		 (y-or-n-p (format "save buffer %s first? " (buffer-name b))))
	    (save-excursion (set-buffer b) (save-buffer)))))

  (if (or noninteractive byte-compile-verbose) ; XEmacs change
      (message "Compiling %s..." filename))
  (let (;;(byte-compile-current-file (file-name-nondirectory filename))
	(byte-compile-current-file filename)
	target-file input-buffer output-buffer
	byte-compile-dest-file)
    (setq target-file (byte-compile-dest-file filename))
    (setq byte-compile-dest-file target-file)
    (save-excursion
      (setq input-buffer (get-buffer-create " *Compiler Input*"))
      (set-buffer input-buffer)
      (erase-buffer)
      (insert-file-contents filename)
      ;; Run hooks including the uncompression hook.
      ;; If they change the file name, then change it for the output also.
      (let ((buffer-file-name filename)
	    (default-major-mode 'emacs-lisp-mode)
	    (enable-local-eval nil))
        (normal-mode)
        (setq filename buffer-file-name)))
      (setq byte-compiler-error-flag nil)
    ;; It is important that input-buffer not be current at this call,
    ;; so that the value of point set in input-buffer
    ;; within byte-compile-from-buffer lingers in that buffer.
    (setq output-buffer (byte-compile-from-buffer input-buffer filename))
    (if byte-compiler-error-flag
	nil
      (if byte-compile-verbose
	  (message "Compiling %s...done" filename))
      (kill-buffer input-buffer)
      (save-excursion
	(set-buffer output-buffer)
	(goto-char (point-max))
	(insert "\n")			; aaah, unix.
	(setq target-file (byte-compile-dest-file filename))
	(unless byte-compile-overwrite-file
	  (ignore-file-errors (delete-file target-file)))
	(if (file-writable-p target-file)
	    (write-region 1 (point-max) target-file)
	  ;; This is just to give a better error message than write-region
	  (signal 'file-error
		  (list "Opening output file"
			(if (file-exists-p target-file)
			    "cannot overwrite file"
			  "directory not writable or nonexistent")
			target-file)))
	(or byte-compile-overwrite-file
	    (condition-case ()
		(set-file-modes target-file (file-modes filename))
	      (error nil)))
	(kill-buffer (current-buffer)))
      (if (and byte-compile-generate-call-tree
	       (or (eq t byte-compile-generate-call-tree)
		   (y-or-n-p (format "Report call tree for %s? " filename))))
	  (save-excursion
	    (display-call-tree filename)))
      (if load
	  (load target-file))
      t)))

;; RMS comments the next two out.

;;;###autoload
(defun byte-compile-and-load-file (&optional filename)
  "Compile a file of Lisp code named FILENAME into a file of byte code,
and then load it.  The output file's name is made by appending \"c\" to
the end of FILENAME."
  (interactive)
  (if filename ; I don't get it, (interactive-p) doesn't always work
	(byte-compile-file filename t)
    (let ((current-prefix-arg '(4)))
	(call-interactively 'byte-compile-file))))

;;;###autoload
(defun byte-compile-buffer (&optional buffer)
  "Byte-compile and evaluate contents of BUFFER (default: the current buffer)."
  (interactive "bByte compile buffer: ")
  (setq buffer (if buffer (get-buffer buffer) (current-buffer)))
  (message "Compiling %s..." buffer)
  (let* ((filename (or (buffer-file-name buffer)
		       (prin1-to-string buffer)))
	 (byte-compile-current-file buffer))
    (byte-compile-from-buffer buffer filename t))
  (message "Compiling %s...done" buffer)
  t)

;;; compiling a single function
;;;###autoload
(defun compile-defun (&optional arg)
  "Compile and evaluate the current top-level form.
Print the result in the minibuffer.
With argument, insert value in current buffer after the form."
  (interactive "P")
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (let* ((byte-compile-current-file (buffer-file-name))
	   (load-file-name (buffer-file-name))
	   (byte-compile-last-warned-form 'nothing)
	   (value (eval (displaying-byte-compile-warnings
			 (byte-compile-sexp (read (current-buffer))
					    "toplevel forms")))))
      (cond (arg
	     (message "Compiling from buffer... done.")
	     (prin1 value (current-buffer))
	     (insert "\n"))
	    ((message "%s" (prin1-to-string value)))))))

(defvar byte-compile-inbuffer)
(defvar byte-compile-outbuffer)

(defun byte-compile-from-buffer (byte-compile-inbuffer filename &optional eval)
  ;; buffer --> output-buffer, or buffer --> eval form, return nil
  (let (byte-compile-outbuffer
	;; Prevent truncation of flonums and lists as we read and print them
	(float-output-format nil)
	(case-fold-search nil)
	(print-length nil)
	(print-level nil)
	;; Simulate entry to byte-compile-top-level
	(byte-compile-constants nil)
	(byte-compile-variables nil)
	(byte-compile-tag-number 0)
	(byte-compile-depth 0)
	(byte-compile-maxdepth 0)
	(byte-compile-output nil)
	;;	  #### This is bound in b-c-close-variables.
	;;	  (byte-compile-warnings (if (eq byte-compile-warnings t)
	;;				     byte-compile-warning-types
	;;				   byte-compile-warnings))
	)
    (byte-compile-close-variables
     (save-excursion
       (setq byte-compile-outbuffer
	     (set-buffer (get-buffer-create " *Compiler Output*")))
       (erase-buffer)
       ;;	 (emacs-lisp-mode)
       (setq case-fold-search nil)
       (and filename
	    (not eval)
	    (byte-compile-insert-header filename
					byte-compile-inbuffer
					byte-compile-outbuffer))

       ;; This is a kludge.  Some operating systems (OS/2, DOS) need to
       ;; write files containing binary information specially.
       ;; Under most circumstances, such files will be in binary
       ;; overwrite mode, so those OS's use that flag to guess how
       ;; they should write their data.  Advise them that .elc files
       ;; need to be written carefully.
       (setq overwrite-mode 'overwrite-mode-binary))
     (displaying-byte-compile-warnings
      (save-excursion
	(set-buffer byte-compile-inbuffer)
	(goto-char 1)

	;; Compile the forms from the input buffer.
	(while (progn
		 (while (progn (skip-chars-forward " \t\n\^L")
			       (looking-at ";"))
		   (forward-line 1))
		 (not (eobp)))
	  (byte-compile-file-form (read byte-compile-inbuffer)))

	;; Compile pending forms at end of file.
	(byte-compile-flush-pending)
	(byte-compile-warn-about-unresolved-functions)
	;; Should we always do this?  When calling multiple files, it
	;; would be useful to delay this warning until all have
	;; been compiled.
	(setq byte-compile-unresolved-functions nil)))
     (save-excursion
       (set-buffer byte-compile-outbuffer)
       (goto-char (point-min))))
    (if (not eval)
	byte-compile-outbuffer
      (let (form)
	(while (condition-case nil
		   (progn (setq form (read byte-compile-outbuffer))
			  t)
		 (end-of-file nil))
	  (eval form)))
      (kill-buffer byte-compile-outbuffer)
      nil)))

(defun byte-compile-insert-header (filename byte-compile-inbuffer
					    byte-compile-outbuffer)
  (set-buffer byte-compile-inbuffer)
  (let ((dynamic-docstrings byte-compile-dynamic-docstrings))
    (set-buffer byte-compile-outbuffer)
    (goto-char 1)
    ;;
    ;; The magic number of .elc files is ";ELC", or 0x3B454C43.  After that is
    ;; the file-format version number (19 or 20) as a byte, followed by some
    ;; nulls.  The primary motivation for doing this is to get some binary
    ;; characters up in the first line of the file so that `diff' will simply
    ;; say "Binary files differ" instead of actually doing a diff of two .elc
    ;; files.  An extra benefit is that you can add this to /etc/magic:
    ;;
    ;; 0	string		;ELC		GNU Emacs Lisp compiled file,
    ;; >4	byte		x		version %d
    ;;
    (insert
     ";ELC"
     (if (byte-compile-version-cond byte-compile-emacs19-compatibility) 19 20)
     "\000\000\000\n"
     )
    (insert ";;; compiled by "
	    (or (and (boundp 'user-mail-address) user-mail-address)
		(concat (user-login-name) "@" (system-name)))
	    " on "
	    (current-time-string) "\n;;; from file " filename "\n")
    (insert ";;; emacs version " emacs-version ".\n")
    (insert ";;; bytecomp version " byte-compile-version "\n;;; "
     (cond
       ((eq byte-optimize 'source) "source-level optimization only")
       ((eq byte-optimize 'byte) "byte-level optimization only")
       (byte-optimize "optimization is on")
       (t "optimization is off"))
     (if (byte-compile-version-cond byte-compile-emacs19-compatibility)
	 "; compiled with Emacs 19 compatibility.\n"
       ".\n"))
   (if (not (byte-compile-version-cond byte-compile-emacs19-compatibility))
       (insert ";;; this file uses opcodes which do not exist in Emacs 19.\n"
	       ;; Have to check if emacs-version is bound so that this works
	       ;; in files loaded early in loadup.el.
	       "\n(if (and (boundp 'emacs-version)\n"
	       "\t (or (and (boundp 'epoch::version) epoch::version)\n"
	       "\t     (< emacs-major-version 20)))\n"
	       "    (error \"`"
	       ;; prin1-to-string is used to quote backslashes.
	       (substring (prin1-to-string (file-name-nondirectory filename))
			  1 -1)
	       "' was compiled for Emacs 20\"))\n\n"))
   (insert "(or (boundp 'current-load-list) (setq current-load-list nil))\n"
	   "\n")
   (if (and (byte-compile-version-cond byte-compile-emacs19-compatibility)
	    dynamic-docstrings)
       (insert ";;; this file uses opcodes which do not exist prior to\n"
	       ";;; XEmacs 19.14/GNU Emacs 19.29 or later."
	       ;; Have to check if emacs-version is bound so that this works
	       ;; in files loaded early in loadup.el.
	       "\n(if (and (boundp 'emacs-version)\n"
	       "\t (or (and (boundp 'epoch::version) epoch::version)\n"
	       "\t     (and (not (featurep 'xemacs))\n"
	       "\t          (<= emacs-major-version 19)\n"
               "\t          (< emacs-minor-version 29))\n"
	       "\t     (and (<= emacs-major-version 20)\n"
               "\t          (< emacs-minor-version 14))))\n"
	       "    (error \"`"
	       ;; prin1-to-string is used to quote backslashes.
	       (substring (prin1-to-string (file-name-nondirectory filename))
			  1 -1)
	       "' was compiled for XEmacs 19.14/Emacs 19.29 or later\"))\n\n"
	       )
      ))

  ;; back in the inbuffer; determine and set the coding system for the .elc
  ;; file if under Mule.  If there are any extended characters in the
  ;; input file, use `escape-quoted' to make sure that both binary and
  ;; extended characters are output properly and distinguished properly.
  ;; Otherwise, use `raw-text' for maximum portability with non-Mule
  ;; Emacsen.
  (when (featurep '(or mule file-coding))
    (defvar buffer-file-coding-system)
    (if (or (featurep '(not mule)) ;; Don't scan buffer if we are not muleized
	    (save-excursion
	      (set-buffer byte-compile-inbuffer)
	      (goto-char (point-min))
	      ;; mrb- There must be a better way than skip-chars-forward
	      (skip-chars-forward (concat (char-to-string 0) "-"
					  (char-to-string 255)))
	      (eq (point) (point-max))))
	(setq buffer-file-coding-system 'raw-text-unix)
      (insert "(require 'mule)\n;;;###coding system: escape-quoted\n")
      (setq buffer-file-coding-system 'escape-quoted)
      ;; #### Lazy loading not yet implemented for MULE files
      ;; mrb - Fix this someday.
      (save-excursion
	(set-buffer byte-compile-inbuffer)
	(setq byte-compile-dynamic nil
	      byte-compile-dynamic-docstrings nil))
      ;;(external-debugging-output (prin1-to-string (buffer-local-variables))))
      ))
  )


(defun byte-compile-output-file-form (form)
  ;; writes the given form to the output buffer, being careful of docstrings
  ;; in defun, defmacro, defvar, defconst and autoload because make-docfile is
  ;; so amazingly stupid.
  ;; defalias calls are output directly by byte-compile-file-form-defmumble;
  ;; it does not pay to first build the defalias in defmumble and then parse
  ;; it here.
  (if (and (memq (car-safe form) '(defun defmacro defvar defconst autoload))
	   (stringp (nth 3 form)))
      (byte-compile-output-docform nil nil '("\n(" 3 ")") form nil
				   (eq (car form) 'autoload))
    (let ((print-escape-newlines t)
	  (print-length nil)
	  (print-level nil)
	  (print-readably t)	; print #[] for bytecode, 'x for (quote x)
	  (print-gensym (if (and byte-compile-print-gensym
				 (not byte-compile-emacs19-compatibility))
			    t nil)))
      (princ "\n" byte-compile-outbuffer)
      (prin1 form byte-compile-outbuffer)
      nil)))

(defun byte-compile-output-docform (preface name info form specindex quoted)
  "Print a form with a doc string.  INFO is (prefix doc-index postfix).
If PREFACE and NAME are non-nil, print them too,
before INFO and the FORM but after the doc string itself.
If SPECINDEX is non-nil, it is the index in FORM
of the function bytecode string.  In that case,
we output that argument and the following argument (the constants vector)
together, for lazy loading.
QUOTED says that we have to put a quote before the
list that represents a doc string reference.
`autoload' needs that."
  ;; We need to examine byte-compile-dynamic-docstrings
  ;; in the input buffer (now current), not in the output buffer.
  (let ((dynamic-docstrings byte-compile-dynamic-docstrings))
    (set-buffer
     (prog1 (current-buffer)
       (set-buffer byte-compile-outbuffer)
       (let (position)

	 ;; Insert the doc string, and make it a comment with #@LENGTH.
	 (and (>= (nth 1 info) 0)
	      dynamic-docstrings
	      (progn
		;; Make the doc string start at beginning of line
		;; for make-docfile's sake.
		(insert "\n")
		(setq position
		      (byte-compile-output-as-comment
		       (nth (nth 1 info) form) nil))
		;; If the doc string starts with * (a user variable),
		;; negate POSITION.
		(if (and (stringp (nth (nth 1 info) form))
			 (> (length (nth (nth 1 info) form)) 0)
			 (char= (aref (nth (nth 1 info) form) 0) ?*))
		    (setq position (- position)))))

	 (if preface
	     (progn
	       (insert preface)
	       (prin1 name byte-compile-outbuffer)))
	 (insert (car info))
	 (let ((print-escape-newlines t)
	       (print-readably t)	; print #[] for bytecode, 'x for (quote x)
	       ;; Use a cons cell to say that we want
	       ;; print-gensym-alist not to be cleared between calls
	       ;; to print functions.
	       (print-gensym (if (and byte-compile-print-gensym
				      (not byte-compile-emacs19-compatibility))
				 '(t) nil))
	       print-gensym-alist
	       (index 0))
	   (prin1 (car form) byte-compile-outbuffer)
	   (while (setq form (cdr form))
	     (setq index (1+ index))
	     (insert " ")
	     (cond ((and (numberp specindex) (= index specindex))
		    (let ((position
			   (byte-compile-output-as-comment
			    (cons (car form) (nth 1 form))
			    t)))
		      (princ (format "(#$ . %d) nil" position)
			     byte-compile-outbuffer)
		      (setq form (cdr form))
		      (setq index (1+ index))))
		   ((= index (nth 1 info))
		    (if position
			(princ (format (if quoted "'(#$ . %d)"  "(#$ . %d)")
				       position)
			       byte-compile-outbuffer)
		      (let ((print-escape-newlines nil))
			(goto-char (prog1 (1+ (point))
				     (prin1 (car form)
					    byte-compile-outbuffer)))
			(insert "\\\n")
			(goto-char (point-max)))))
		   (t
		    (prin1 (car form) byte-compile-outbuffer)))))
	 (insert (nth 2 info))))))
  nil)

(defvar for-effect) ; ## Kludge!  This should be an arg, not a special.

(defun byte-compile-keep-pending (form &optional handler)
  (if (memq byte-optimize '(t source))
      (setq form (byte-optimize-form form t)))
  (if handler
      (let ((for-effect t))
	;; To avoid consing up monstrously large forms at load time, we split
	;; the output regularly.
	(and (memq (car-safe form) '(fset defalias define-function))
	     (nthcdr 300 byte-compile-output)
	     (byte-compile-flush-pending))
	(funcall handler form)
	(when for-effect
	  (byte-compile-discard)))
    (byte-compile-form form t))
  nil)

(defun byte-compile-flush-pending ()
  (if byte-compile-output
      (let ((form (byte-compile-out-toplevel t 'file)))
	(cond ((eq (car-safe form) 'progn)
	       (mapcar 'byte-compile-output-file-form (cdr form)))
	      (form
	       (byte-compile-output-file-form form)))
	(setq byte-compile-constants nil
	      byte-compile-variables nil
	      byte-compile-depth 0
	      byte-compile-maxdepth 0
	      byte-compile-output nil))))

(defun byte-compile-file-form (form)
  (let ((byte-compile-current-form nil)	; close over this for warnings.
	handler)
    (cond
     ((not (consp form))
      (byte-compile-keep-pending form))
     ((and (symbolp (car form))
	   (setq handler (get (car form) 'byte-hunk-handler)))
      (cond ((setq form (funcall handler form))
	     (byte-compile-flush-pending)
	     (byte-compile-output-file-form form))))
     ((eq form (setq form (macroexpand form byte-compile-macro-environment)))
      (byte-compile-keep-pending form))
     (t
      (byte-compile-file-form form)))))

;; Functions and variables with doc strings must be output separately,
;; so make-docfile can recognize them.  Most other things can be output
;; as byte-code.

(put 'defsubst 'byte-hunk-handler 'byte-compile-file-form-defsubst)
(defun byte-compile-file-form-defsubst (form)
  (cond ((assq (nth 1 form) byte-compile-unresolved-functions)
	 (setq byte-compile-current-form (nth 1 form))
	 (byte-compile-warn "defsubst %s was used before it was defined"
			    (nth 1 form))))
  (byte-compile-file-form
   (macroexpand form byte-compile-macro-environment))
  ;; Return nil so the form is not output twice.
  nil)

(put 'autoload 'byte-hunk-handler 'byte-compile-file-form-autoload)
(defun byte-compile-file-form-autoload (form)
  ;;
  ;; If this is an autoload of a macro, and all arguments are constants (that
  ;; is, there is no hairy computation going on here) then evaluate the form
  ;; at compile-time.  This is so that we can make use of macros which we
  ;; have autoloaded from the file being compiled.  Normal function autoloads
  ;; are not automatically evaluated at compile time, because there's not
  ;; much point to it (so why bother cluttering up the compile-time namespace.)
  ;;
  ;; If this is an autoload of a function, then record its definition in the
  ;; byte-compile-autoload-environment to suppress any `not known to be
  ;; defined' warnings at the end of this file (this only matters for
  ;; functions which are autoloaded and compiled in the same file, if the
  ;; autoload already exists in the compilation environment, we wouldn't have
  ;; warned anyway.)
  ;;
  (let* ((name (if (byte-compile-constp (nth 1 form))
		   (eval (nth 1 form))))
	 ;; In v19, the 5th arg to autoload can be t, nil, 'macro, or 'keymap.
	 (macrop (and (byte-compile-constp (nth 5 form))
		      (memq (eval (nth 5 form)) '(t macro))))
;;	 (functionp (and (byte-compile-constp (nth 5 form))
;;			 (eq 'nil (eval (nth 5 form)))))
	 )
    (if (and macrop
	     (let ((form form))
	       ;; all forms are constant
	       (while (if (setq form (cdr form))
			  (byte-compile-constp (car form))))
	       (null form)))
	;; eval the macro autoload into the compilation environment
	(eval form))

    (if name
	(let ((old (assq name byte-compile-autoload-environment)))
	  (cond (old
		 (if (memq 'redefine byte-compile-warnings)
		     (byte-compile-warn "multiple autoloads for %s" name))
		 (setcdr old form))
		(t
		 ;; We only use the names in the autoload environment, but
		 ;; it might be useful to have the bodies some day.
		 (setq byte-compile-autoload-environment
		       (cons (cons name form)
			     byte-compile-autoload-environment)))))))
  ;;
  ;; Now output the form.
  (if (stringp (nth 3 form))
      form
    ;; No doc string, so we can compile this as a normal form.
    (byte-compile-keep-pending form 'byte-compile-normal-call)))

(put 'defvar   'byte-hunk-handler 'byte-compile-file-form-defvar-or-defconst)
(put 'defconst 'byte-hunk-handler 'byte-compile-file-form-defvar-or-defconst)
(defun byte-compile-file-form-defvar-or-defconst (form)
  ;; (defvar|defconst VAR [VALUE [DOCSTRING]])
  (if (> (length form) 4)
      (byte-compile-warn
       "%s %s called with %d arguments, but accepts only %s"
       (car form) (nth 1 form) (length (cdr form)) 3))
  (if (and (> (length form) 3) (not (stringp (nth 3 form))))
      (byte-compile-warn "Third arg to %s %s is not a string: %s"
			 (car form) (nth 1 form) (nth 3 form)))
  (if (null (nth 3 form))
      ;; Since there is no doc string, we can compile this as a normal form,
      ;; and not do a file-boundary.
      (byte-compile-keep-pending form)
    (if (memq 'free-vars byte-compile-warnings)
	(setq byte-compile-bound-variables
	      (cons (cons (nth 1 form) byte-compile-global-bit)
		    byte-compile-bound-variables)))
    (cond ((consp (nth 2 form))
	   (setq form (copy-sequence form))
	   (setcar (cdr (cdr form))
		   (byte-compile-top-level (nth 2 form) nil 'file))))

    ;; The following turns out not to be necessary, since we emit a call to
    ;; defvar, which can hack Vfile_domain by itself!
    ;;
    ;; If a file domain has been set, emit (put 'VAR 'variable-domain ...)
    ;; after this defvar.
;    (if byte-compile-file-domain
;	(progn
;	  ;; Actually, this will emit the (put ...) before the (defvar ...)
;	  ;; but I don't think that can matter in this case.
;	  (byte-compile-keep-pending
;	   (list 'put (list 'quote (nth 1 form)) ''variable-domain
;		(list 'quote byte-compile-file-domain)))))
    form))

(put 'require 'byte-hunk-handler 'byte-compile-file-form-eval-boundary)
(defun byte-compile-file-form-eval-boundary (form)
  (eval form)
  (byte-compile-keep-pending form 'byte-compile-normal-call))

(put 'progn 'byte-hunk-handler 'byte-compile-file-form-progn)
(put 'prog1 'byte-hunk-handler 'byte-compile-file-form-progn)
(put 'prog2 'byte-hunk-handler 'byte-compile-file-form-progn)
(defun byte-compile-file-form-progn (form)
  (mapcar 'byte-compile-file-form (cdr form))
  ;; Return nil so the forms are not output twice.
  nil)

;; This handler is not necessary, but it makes the output from dont-compile
;; and similar macros cleaner.
(put 'eval 'byte-hunk-handler 'byte-compile-file-form-eval)
(defun byte-compile-file-form-eval (form)
  (if (eq (car-safe (nth 1 form)) 'quote)
      (nth 1 (nth 1 form))
    (byte-compile-keep-pending form)))

(put 'defun 'byte-hunk-handler 'byte-compile-file-form-defun)
(defun byte-compile-file-form-defun (form)
  (byte-compile-file-form-defmumble form nil))

(put 'defmacro 'byte-hunk-handler 'byte-compile-file-form-defmacro)
(defun byte-compile-file-form-defmacro (form)
  (byte-compile-file-form-defmumble form t))

(defun byte-compile-compiled-obj-to-list (obj)
  ;; #### this is fairly disgusting.  Rewrite the code instead
  ;; so that it doesn't create compiled objects in the first place!
  ;; Much better than creating them and then "uncreating" them
  ;; like this.
  (read (concat "("
		(substring (let ((print-readably t)
				 (print-gensym
				  (if (and byte-compile-print-gensym
					   (not byte-compile-emacs19-compatibility))
				      '(t) nil))
				 (print-gensym-alist nil))
			     (prin1-to-string obj))
			   2 -1)
		")")))

(defun byte-compile-file-form-defmumble (form macrop)
  (let* ((name (car (cdr form)))
	 (this-kind (if macrop 'byte-compile-macro-environment
		      'byte-compile-function-environment))
	 (that-kind (if macrop 'byte-compile-function-environment
		      'byte-compile-macro-environment))
	 (this-one (assq name (symbol-value this-kind)))
	 (that-one (assq name (symbol-value that-kind)))
	 (byte-compile-free-references nil)
	 (byte-compile-free-assignments nil))

    ;; When a function or macro is defined, add it to the call tree so that
    ;; we can tell when functions are not used.
    (if byte-compile-generate-call-tree
	(or (assq name byte-compile-call-tree)
	    (setq byte-compile-call-tree
		  (cons (list name nil nil) byte-compile-call-tree))))

    (setq byte-compile-current-form name) ; for warnings
    (when (memq 'redefine byte-compile-warnings)
      (byte-compile-arglist-warn form macrop))
    (defvar filename) ; #### filename used free
    (when byte-compile-verbose
      (message "Compiling %s... (%s)"
	       (if filename (file-name-nondirectory filename) "")
	       (nth 1 form)))
    (cond (that-one
	   (when (and (memq 'redefine byte-compile-warnings)
		      ;; hack hack: don't warn when compiling the stubs in
		      ;; bytecomp-runtime...
		      (not (assq (nth 1 form)
				 byte-compile-initial-macro-environment)))
	     (byte-compile-warn
	      "%s defined multiple times, as both function and macro"
	      (nth 1 form)))
	   (setcdr that-one nil))
	  (this-one
	   (when (and (memq 'redefine byte-compile-warnings)
		      ;; hack: don't warn when compiling the magic internal
		      ;; byte-compiler macros in bytecomp-runtime.el...
		      (not (assq (nth 1 form)
				 byte-compile-initial-macro-environment)))
	     (byte-compile-warn "%s %s defined multiple times in this file"
				(if macrop "macro" "function")
				(nth 1 form))))
	  ((and (fboundp name)
		(or (subrp (symbol-function name))
		    (eq (car-safe (symbol-function name))
		        (if macrop 'lambda 'macro))))
	   (if (memq 'redefine byte-compile-warnings)
	       (byte-compile-warn "%s %s being redefined as a %s"
				  (if (subrp (symbol-function name))
				      "subr"
				    (if macrop "function" "macro"))
				  (nth 1 form)
				  (if macrop "macro" "function")))
	   ;; shadow existing definition
	   (set this-kind
		(cons (cons name nil) (symbol-value this-kind)))))
    (let ((body (nthcdr 3 form)))
      (if (and (stringp (car body))
	       (symbolp (car-safe (cdr-safe body)))
	       (car-safe (cdr-safe body))
	       (stringp (car-safe (cdr-safe (cdr-safe body)))))
	  (byte-compile-warn "Probable `\"' without `\\' in doc string of %s"
			     (nth 1 form))))
    (let* ((new-one (byte-compile-lambda (cons 'lambda (nthcdr 2 form))))
	   (code (byte-compile-byte-code-maker new-one)))
      (if this-one
	  (setcdr this-one new-one)
	(set this-kind
	     (cons (cons name new-one) (symbol-value this-kind))))
      (if (and (stringp (nth 3 form))
	       (eq 'quote (car-safe code))
	       (eq 'lambda (car-safe (nth 1 code))))
	  (cons (car form)
		(cons name (cdr (nth 1 code))))
	(byte-compile-flush-pending)
	(if (not (stringp (nth 3 form)))
	    ;; No doc string.  Provide -1 as the "doc string index"
	    ;; so that no element will be treated as a doc string.
	    (byte-compile-output-docform
	     "\n(defalias '"
	     name
	     (cond ((atom code)
		    (if macrop '(" '(macro . #[" -1 "])") '(" #[" -1 "]")))
		   ((eq (car code) 'quote)
		    (setq code new-one)
		    (if macrop '(" '(macro " -1 ")") '(" '(" -1 ")")))
		   ((if macrop '(" (cons 'macro (" -1 "))") '(" (" -1 ")"))))
	     ;; FSF just calls `(append code nil)' here but that relies
	     ;; on horrible C kludges in concat() that accept byte-
	     ;; compiled objects and pretend they're vectors.
	     (if (compiled-function-p code)
		 (byte-compile-compiled-obj-to-list code)
	       (append code nil))
	     (and (atom code) byte-compile-dynamic
		  1)
	     nil)
	  ;; Output the form by hand, that's much simpler than having
	  ;; b-c-output-file-form analyze the defalias.
	  (byte-compile-output-docform
	   "\n(defalias '"
	   name
	   (cond ((atom code) ; compiled-function-p
		  (if macrop '(" '(macro . #[" 4 "])") '(" #[" 4 "]")))
		 ((eq (car code) 'quote)
		  (setq code new-one)
		  (if macrop '(" '(macro " 2 ")") '(" '(" 2 ")")))
		 ((if macrop '(" (cons 'macro (" 5 "))") '(" (" 5 ")"))))
	   ;; The result of byte-compile-byte-code-maker is either a
	   ;; compiled-function object, or a list of some kind.  If it's
	   ;; not a cons, we must coerce it into a list of the elements
	   ;; to be printed to the file.
	   (if (consp code)
	       code
	     (nconc (list
		     (compiled-function-arglist code)
		     (compiled-function-instructions code)
		     (compiled-function-constants code)
		     (compiled-function-stack-depth code))
		    (let ((doc (documentation code t)))
		      (if doc (list doc)))
		    (if (commandp code)
			(list (nth 1 (compiled-function-interactive code))))))
	   (and (atom code) byte-compile-dynamic
		1)
	   nil))
	(princ ")" byte-compile-outbuffer)
	nil))))

;; Print Lisp object EXP in the output file, inside a comment,
;; and return the file position it will have.
;; If QUOTED is non-nil, print with quoting; otherwise, print without quoting.
(defun byte-compile-output-as-comment (exp quoted)
  (let ((position (point)))
    (set-buffer
     (prog1 (current-buffer)
       (set-buffer byte-compile-outbuffer)

       ;; Insert EXP, and make it a comment with #@LENGTH.
       (insert " ")
       (if quoted
	   (prin1 exp byte-compile-outbuffer)
	 (princ exp byte-compile-outbuffer))
       (goto-char position)
       ;; Quote certain special characters as needed.
       ;; get_doc_string in doc.c does the unquoting.
       (while (search-forward "\^A" nil t)
	 (replace-match "\^A\^A" t t))
       (goto-char position)
       (while (search-forward "\000" nil t)
	 (replace-match "\^A0" t t))
       (goto-char position)
       (while (search-forward "\037" nil t)
	 (replace-match "\^A_" t t))
       (goto-char (point-max))
       (insert "\037")
       (goto-char position)
       (insert "#@" (format "%d" (- (point-max) position)))

       ;; Save the file position of the object.
       ;; Note we should add 1 to skip the space
       ;; that we inserted before the actual doc string,
       ;; and subtract 1 to convert from an 1-origin Emacs position
       ;; to a file position; they cancel.
       (setq position (point))
       (goto-char (point-max))))
    position))



;; The `domain' declaration.  This is legal only at top-level in a file, and
;; should generally be the first form in the file.  It is not legal inside
;; function bodies.

(put 'domain 'byte-hunk-handler 'byte-compile-file-form-domain)
(defun byte-compile-file-form-domain (form)
  (if (not (null (cdr (cdr form))))
      (byte-compile-warn "domain used with too many arguments: %s" form))
  (let ((domain (nth 1 form)))
    (or (null domain)
	(stringp domain)
	(progn
	  (byte-compile-warn
	   "argument to `domain' declaration must be a literal string: %s"
	   form)
	  (setq domain nil)))
    (setq byte-compile-file-domain domain))
  (byte-compile-keep-pending form 'byte-compile-normal-call))

(defun byte-compile-domain (form)
  (byte-compile-warn "The `domain' declaration is legal only at top-level: %s"
		     (let ((print-escape-newlines t)
			   (print-level 4)
			   (print-length 4))
		       (prin1-to-string form)))
  (byte-compile-normal-call
   (list 'signal ''error
	 (list 'quote (list "`domain' used inside a function" form)))))

;; This is part of bytecomp.el in 19.35:
(put 'custom-declare-variable 'byte-hunk-handler
     'byte-compile-file-form-custom-declare-variable)
(defun byte-compile-file-form-custom-declare-variable (form)
  (if (memq 'free-vars byte-compile-warnings)
      (setq byte-compile-bound-variables
	    (cons (cons (nth 1 (nth 1 form))
			byte-compile-global-bit)
		  byte-compile-bound-variables)))
  form)


;;;###autoload
(defun byte-compile (form)
  "If FORM is a symbol, byte-compile its function definition.
If FORM is a lambda or a macro, byte-compile it as a function."
  (displaying-byte-compile-warnings
   (byte-compile-close-variables
    (let* ((fun (if (symbolp form)
		    (and (fboundp form) (symbol-function form))
		  form))
	   (macro (eq (car-safe fun) 'macro)))
      (if macro
	  (setq fun (cdr fun)))
      (cond ((eq (car-safe fun) 'lambda)
	     (setq fun (if macro
			   (cons 'macro (byte-compile-lambda fun))
			 (byte-compile-lambda fun)))
	     (if (symbolp form)
		 (defalias form fun)
	       fun)))))))

;;;###autoload
(defun byte-compile-sexp (sexp &optional msg)
  "Compile and return SEXP."
  (displaying-byte-compile-warnings
   (byte-compile-close-variables
    (prog1
	(byte-compile-top-level sexp)
      (byte-compile-warn-about-unresolved-functions msg)))))

;; Given a function made by byte-compile-lambda, make a form which produces it.
(defun byte-compile-byte-code-maker (fun)
  (cond
   ;; ## atom is faster than compiled-func-p.
   ((atom fun)				; compiled-function-p
    fun)
   ;; b-c-lambda didn't produce a compiled-function, so it must be a trivial
   ;; function.
   ((let (tmp)
      (if (and (setq tmp (assq 'byte-code (cdr-safe (cdr fun))))
	       (null (cdr (memq tmp fun))))
	  ;; Generate a make-byte-code call.
	  (let* ((interactive (assq 'interactive (cdr (cdr fun)))))
	    (nconc (list 'make-byte-code
			 (list 'quote (nth 1 fun)) ;arglist
			 (nth 1 tmp)	;instructions
			 (nth 2 tmp)	;constants
			 (nth 3 tmp))	;stack-depth
		   (cond ((stringp (nth 2 fun))
			  (list (nth 2 fun))) ;docstring
			 (interactive
			  (list nil)))
		   (cond (interactive
			  (list (if (or (null (nth 1 interactive))
					(stringp (nth 1 interactive)))
				    (nth 1 interactive)
				  ;; Interactive spec is a list or a variable
				  ;; (if it is correct).
				  (list 'quote (nth 1 interactive))))))))
	;; a non-compiled function (probably trivial)
	(list 'quote fun))))))

;; Byte-compile a lambda-expression and return a valid function.
;; The value is usually a compiled function but may be the original
;; lambda-expression.
(defun byte-compile-lambda (fun)
  (or (eq 'lambda (car-safe fun))
      (error "not a lambda -- %s" (prin1-to-string fun)))
  (let* ((arglist (nth 1 fun))
	 (byte-compile-bound-variables
	  (let ((new-bindings
		 (mapcar #'(lambda (x) (cons x byte-compile-arglist-bit))
			 (and (memq 'free-vars byte-compile-warnings)
			      (delq '&rest (delq '&optional
						 (copy-sequence arglist)))))))
	    (nconc new-bindings
		   (cons 'new-scope byte-compile-bound-variables))))
	 (body (cdr (cdr fun)))
	 (doc (if (stringp (car body))
		  (prog1 (car body)
		    ;; Discard the doc string
		    ;; only if it is not the only element of the body.
		    (if (cdr body)
			(setq body (cdr body))))))
	 (int (assq 'interactive body)))
    (dolist (arg arglist)
      (cond ((not (symbolp arg))
	     (byte-compile-warn "non-symbol in arglist: %S" arg))
	    ((byte-compile-constant-symbol-p arg)
	     (byte-compile-warn "constant symbol in arglist: %s" arg))
	    ((and (char= ?\& (aref (symbol-name arg) 0))
		  (not (eq arg '&optional))
		  (not (eq arg '&rest)))
	     (byte-compile-warn "unrecognized `&' keyword in arglist: %s"
				arg))))
    (cond (int
	   ;; Skip (interactive) if it is in front (the most usual location).
	   (if (eq int (car body))
	       (setq body (cdr body)))
	   (cond ((consp (cdr int))
		  (if (cdr (cdr int))
		      (byte-compile-warn "malformed interactive spec: %s"
					 (prin1-to-string int)))
		  ;; If the interactive spec is a call to `list',
		  ;; don't compile it, because `call-interactively'
		  ;; looks at the args of `list'.
		  (let ((form (nth 1 int)))
		    (while (or (eq (car-safe form) 'let)
			       (eq (car-safe form) 'let*)
			       (eq (car-safe form) 'save-excursion))
		      (while (consp (cdr form))
			(setq form (cdr form)))
		      (setq form (car form)))
		    (or (eq (car-safe form) 'list)
			(setq int (list 'interactive
					(byte-compile-top-level (nth 1 int)))))))
		 ((cdr int)
		  (byte-compile-warn "malformed interactive spec: %s"
				     (prin1-to-string int))))))
    (let ((compiled (byte-compile-top-level (cons 'progn body) nil 'lambda)))
      (if (memq 'unused-vars byte-compile-warnings)
	  ;; done compiling in this scope, warn now.
	  (byte-compile-warn-about-unused-variables))
      (if (eq 'byte-code (car-safe compiled))
	  (apply 'make-byte-code
		 (append (list arglist)
			 ;; byte-string, constants-vector, stack depth
			 (cdr compiled)
			 ;; optionally, the doc string.
			 (if (or doc int)
			     (list doc))
			 ;; optionally, the interactive spec.
			 (if int
			     (list (nth 1 int)))))
	(setq compiled
	      (nconc (if int (list int))
		     (cond ((eq (car-safe compiled) 'progn) (cdr compiled))
			   (compiled (list compiled)))))
	(nconc (list 'lambda arglist)
	       (if (or doc (stringp (car compiled)))
		   (cons doc (cond (compiled)
				   (body (list nil))))
		 compiled))))))

(defun byte-compile-constants-vector ()
  ;; Builds the constants-vector from the current variables and constants.
  ;;   This modifies the constants from (const . nil) to (const . offset).
  ;; To keep the byte-codes to look up the vector as short as possible:
  ;;   First 6 elements are vars, as there are one-byte varref codes for those.
  ;;   Next up to byte-constant-limit are constants, still with one-byte codes.
  ;;   Next variables again, to get 2-byte codes for variable lookup.
  ;;   The rest of the constants and variables need 3-byte byte-codes.
  (let* ((i -1)
	 (rest (nreverse byte-compile-variables)) ; nreverse because the first
	 (other (nreverse byte-compile-constants)) ; vars often are used most.
	 ret tmp
	 (limits '(5			; Use the 1-byte varref codes,
		   63  ; 1-constlim	;  1-byte byte-constant codes,
		   255			;  2-byte varref codes,
		   65535))		;  3-byte codes for the rest.
	 limit)
    (while (or rest other)
      (setq limit (car limits))
      (while (and rest (not (eq i limit)))
	(if (setq tmp (assq (car (car rest)) ret))
	    (setcdr (car rest) (cdr tmp))
	  (setcdr (car rest) (setq i (1+ i)))
	  (setq ret (cons (car rest) ret)))
	(setq rest (cdr rest)))
      (setq limits (cdr limits)
	    rest (prog1 other
		   (setq other rest))))
    (apply 'vector (nreverse (mapcar 'car ret)))))

;; Given an expression FORM, compile it and return an equivalent byte-code
;; expression (a call to the function byte-code).
(defun byte-compile-top-level (form &optional for-effect output-type)
  ;; OUTPUT-TYPE advises about how form is expected to be used:
  ;;	'eval or nil	-> a single form,
  ;;	'progn or t	-> a list of forms,
  ;;	'lambda		-> body of a lambda,
  ;;	'file		-> used at file-level.
  (let ((byte-compile-constants nil)
	(byte-compile-variables nil)
	(byte-compile-tag-number 0)
	(byte-compile-depth 0)
	(byte-compile-maxdepth 0)
	(byte-compile-output nil))
    (if (memq byte-optimize '(t source))
	(setq form (byte-optimize-form form for-effect)))
    (while (and (eq (car-safe form) 'progn) (null (cdr (cdr form))))
      (setq form (nth 1 form)))
    (if (and (eq 'byte-code (car-safe form))
	     (not (memq byte-optimize '(t byte)))
	     (stringp (nth 1 form))
	     (vectorp (nth 2 form))
	     (natnump (nth 3 form)))
	form
      (byte-compile-form form for-effect)
      (byte-compile-out-toplevel for-effect output-type))))

(defun byte-compile-out-toplevel (&optional for-effect output-type)
  (if for-effect
      ;; The stack is empty. Push a value to be returned from (byte-code ..).
      (if (eq (car (car byte-compile-output)) 'byte-discard)
	  (setq byte-compile-output (cdr byte-compile-output))
	(byte-compile-push-constant
	 ;; Push any constant - preferably one which already is used, and
	 ;; a number or symbol - ie not some big sequence.  The return value
	 ;; isn't returned, but it would be a shame if some textually large
	 ;; constant was not optimized away because we chose to return it.
	 (and (not (assq nil byte-compile-constants)) ; Nil is often there.
	      (let ((tmp (reverse byte-compile-constants)))
		(while (and tmp (not (or (symbolp (car (car tmp)))
					 (numberp (car (car tmp))))))
		  (setq tmp (cdr tmp)))
		(car (car tmp)))))))
  (byte-compile-out 'byte-return 0)
  (setq byte-compile-output (nreverse byte-compile-output))
  (if (memq byte-optimize '(t byte))
      (setq byte-compile-output
	    (byte-optimize-lapcode byte-compile-output for-effect)))

  ;; Decompile trivial functions:
  ;; only constants and variables, or a single funcall except in lambdas.
  ;; Except for Lisp_Compiled objects, forms like (foo "hi")
  ;; are still quicker than (byte-code "..." [foo "hi"] 2).
  ;; Note that even (quote foo) must be parsed just as any subr by the
  ;; interpreter, so quote should be compiled into byte-code in some contexts.
  ;; What to leave uncompiled:
  ;;	lambda	-> never.  we used to leave it uncompiled if the body was
  ;;		   a single atom, but that causes confusion if the docstring
  ;;		   uses the (file . pos) syntax.  Besides, now that we have
  ;;		   the Lisp_Compiled type, the compiled form is faster.
  ;;	eval	-> atom, quote or (function atom atom atom)
  ;;	progn	-> as <<same-as-eval>> or (progn <<same-as-eval>> atom)
  ;;	file	-> as progn, but takes both quotes and atoms, and longer forms.
  (let (rest
	(maycall (not (eq output-type 'lambda))) ; t if we may make a funcall.
	tmp body)
    (cond
     ;; #### This should be split out into byte-compile-nontrivial-function-p.
     ((or (eq output-type 'lambda)
	  (nthcdr (if (eq output-type 'file) 50 8) byte-compile-output)
	  (assq 'TAG byte-compile-output) ; Not necessary, but speeds up a bit.
	  (not (setq tmp (assq 'byte-return byte-compile-output)))
	  (progn
	    (setq rest (nreverse
			(cdr (memq tmp (reverse byte-compile-output)))))
	    (while (cond
		    ((memq (car (car rest)) '(byte-varref byte-constant))
		     (setq tmp (car (cdr (car rest))))
		     (if (if (eq (car (car rest)) 'byte-constant)
			     (or (consp tmp)
				 (and (symbolp tmp)
				      (not (byte-compile-constant-symbol-p tmp)))))
			 (if maycall
			     (setq body (cons (list 'quote tmp) body)))
		       (setq body (cons tmp body))))
		    ((and maycall
			  ;; Allow a funcall if at most one atom follows it.
			  (null (nthcdr 3 rest))
			  (setq tmp
				;; XEmacs change for rms funs
				(or (and
				     (byte-compile-version-cond
				      byte-compile-emacs19-compatibility)
				     (get (car (car rest))
					  'byte-opcode19-invert))
				    (get (car (car rest))
					 'byte-opcode-invert)))
			  (or (null (cdr rest))
			      (and (memq output-type '(file progn t))
				   (cdr (cdr rest))
				   (eq (car (nth 1 rest)) 'byte-discard)
				   (progn (setq rest (cdr rest)) t))))
		     (setq maycall nil)	; Only allow one real function call.
		     (setq body (nreverse body))
		     (setq body (list
				 (if (and (eq tmp 'funcall)
					  (eq (car-safe (car body)) 'quote))
				     (cons (nth 1 (car body)) (cdr body))
				   (cons tmp body))))
		     (or (eq output-type 'file)
			 (not (delq nil (mapcar 'consp (cdr (car body))))))))
	      (setq rest (cdr rest)))
	    rest))
      (let ((byte-compile-vector (byte-compile-constants-vector)))
	(list 'byte-code (byte-compile-lapcode byte-compile-output)
	      byte-compile-vector byte-compile-maxdepth)))
     ;; it's a trivial function
     ((cdr body) (cons 'progn (nreverse body)))
     ((car body)))))

;; Given BODY, compile it and return a new body.
(defun byte-compile-top-level-body (body &optional for-effect)
  (setq body (byte-compile-top-level (cons 'progn body) for-effect t))
  (cond ((eq (car-safe body) 'progn)
	 (cdr body))
	(body
	 (list body))))

;; This is the recursive entry point for compiling each subform of an
;; expression.
;; If for-effect is non-nil, byte-compile-form will output a byte-discard
;; before terminating (ie. no value will be left on the stack).
;; A byte-compile handler may, when for-effect is non-nil, choose output code
;; which does not leave a value on the stack, and then set for-effect to nil
;; (to prevent byte-compile-form from outputting the byte-discard).
;; If a handler wants to call another handler, it should do so via
;; byte-compile-form, or take extreme care to handle for-effect correctly.
;; (Use byte-compile-form-do-effect to reset the for-effect flag too.)
;;
(defun byte-compile-form (form &optional for-effect)
  (setq form (macroexpand form byte-compile-macro-environment))
  (cond ((not (consp form))
	 (cond ((or (not (symbolp form))
		    (byte-compile-constant-symbol-p form))
		(byte-compile-constant form))
	       ((and for-effect byte-compile-delete-errors)
		(setq for-effect nil))
	       (t (byte-compile-variable-ref 'byte-varref form))))
	((symbolp (car form))
	 (let* ((fn (car form))
		(handler (get fn 'byte-compile)))
	   (if (memq fn '(t nil))
	       (byte-compile-warn "%s called as a function" fn))
	   (if (and handler
		    (or (not (byte-compile-version-cond
			      byte-compile-emacs19-compatibility))
			(not (get (get fn 'byte-opcode) 'emacs20-opcode))))
	       (funcall handler form)
	     (if (memq 'callargs byte-compile-warnings)
		 (byte-compile-callargs-warn form))
	     (byte-compile-normal-call form))))
	((and (or (compiled-function-p (car form))
		  (eq (car-safe (car form)) 'lambda))
	      ;; if the form comes out the same way it went in, that's
	      ;; because it was malformed, and we couldn't unfold it.
	      (not (eq form (setq form (byte-compile-unfold-lambda form)))))
	 (byte-compile-form form for-effect)
	 (setq for-effect nil))
	((byte-compile-normal-call form)))
  (when for-effect
    (byte-compile-discard)))

(defun byte-compile-normal-call (form)
  (if byte-compile-generate-call-tree
      (byte-compile-annotate-call-tree form))
  (byte-compile-push-constant (car form))
  (mapcar 'byte-compile-form (cdr form)) ; wasteful, but faster.
  (byte-compile-out 'byte-call (length (cdr form))))

;; kludge added to XEmacs to work around the bogosities of a nonlexical lisp.
(or (fboundp 'globally-boundp) (fset 'globally-boundp 'boundp))

(defun byte-compile-variable-ref (base-op var &optional varbind-flags)
  (if (or (not (symbolp var)) (byte-compile-constant-symbol-p var))
      (byte-compile-warn
       (case base-op
	 (byte-varref "Variable reference to %s %s")
	 (byte-varset "Attempt to set %s %s")
	 (byte-varbind "Attempt to let-bind %s %s"))
       (if (symbolp var) "constant symbol" "non-symbol")
       var)
    (if (and (get var 'byte-obsolete-variable)
	     (memq 'obsolete byte-compile-warnings))
	(let ((ob (get var 'byte-obsolete-variable)))
	  (byte-compile-warn "%s is an obsolete variable; %s" var
			     (if (stringp ob)
				 ob
			       (format "use %s instead." ob)))))
    (if (and (get var 'byte-compatible-variable)
	     (memq 'pedantic byte-compile-warnings))
	(let ((ob (get var 'byte-compatible-variable)))
	  (byte-compile-warn "%s is provided for compatibility; %s" var
			     (if (stringp ob)
				 ob
			       (format "use %s instead." ob)))))
    (if (memq 'free-vars byte-compile-warnings)
	(if (eq base-op 'byte-varbind)
	    (setq byte-compile-bound-variables
		  (cons (cons var (or varbind-flags 0))
			byte-compile-bound-variables))
	  (or (globally-boundp var)
	      (let ((cell (assq var byte-compile-bound-variables)))
		(if cell (setcdr cell
				 (logior (cdr cell)
					 (if (eq base-op 'byte-varset)
					     byte-compile-assigned-bit
					   byte-compile-referenced-bit)))))
	      (and (boundp 'current-load-list)
		   (memq var current-load-list))
	      (if (eq base-op 'byte-varset)
		  (or (memq var byte-compile-free-assignments)
		      (progn
			(byte-compile-warn "assignment to free variable %s"
					   var)
			(setq byte-compile-free-assignments
			      (cons var byte-compile-free-assignments))))
		(or (memq var byte-compile-free-references)
		    (progn
		      (byte-compile-warn "reference to free variable %s" var)
		      (setq byte-compile-free-references
			    (cons var byte-compile-free-references)))))))))
  (let ((tmp (assq var byte-compile-variables)))
    (or tmp
	(setq tmp (list var)
	      byte-compile-variables (cons tmp byte-compile-variables)))
    (byte-compile-out base-op tmp)))

(defmacro byte-compile-get-constant (const)
  `(or (if (stringp ,const)
	   (assoc ,const byte-compile-constants)
	 (assq ,const byte-compile-constants))
       (car (setq byte-compile-constants
		  (cons (list ,const) byte-compile-constants)))))

;; Use this when the value of a form is a constant.  This obeys for-effect.
(defun byte-compile-constant (const)
  (if for-effect
      (setq for-effect nil)
    (byte-compile-out 'byte-constant (byte-compile-get-constant const))))

;; Use this for a constant that is not the value of its containing form.
;; This ignores for-effect.
(defun byte-compile-push-constant (const)
  (let ((for-effect nil))
    (inline (byte-compile-constant const))))


;; Compile those primitive ordinary functions
;; which have special byte codes just for speed.

(defmacro byte-defop-compiler (function &optional compile-handler)
  ;; add a compiler-form for FUNCTION.
  ;; If FUNCTION is a symbol, then the variable "byte-SYMBOL" must name
  ;; the opcode to be used.  If is a list, the first element
  ;; is the function and the second element is the bytecode-symbol.
  ;; COMPILE-HANDLER is the function to use to compile this byte-op, or
  ;; may be the abbreviations 0, 1, 2, 3, 0-1, 1-2, 2-3, 0+1, 1+1, 2+1,
  ;; 0-1+1, 1-2+1, 2-3+1, 0+2, or 1+2.  If it is nil, then the handler is
  ;; "byte-compile-SYMBOL."
  (let (opcode)
    (if (symbolp function)
	(setq opcode (intern (concat "byte-" (symbol-name function))))
      (setq opcode (car (cdr function))
	    function (car function)))
    (let ((fnform
	   (list 'put (list 'quote function) ''byte-compile
		 (list 'quote
		       (or (cdr (assq compile-handler
				      '((0 . byte-compile-no-args)
					(1 . byte-compile-one-arg)
					(2 . byte-compile-two-args)
					(3 . byte-compile-three-args)
					(0-1 . byte-compile-zero-or-one-arg)
					(1-2 . byte-compile-one-or-two-args)
					(2-3 . byte-compile-two-or-three-args)
					(0+1 . byte-compile-no-args-with-one-extra)
					(1+1 . byte-compile-one-arg-with-one-extra)
					(2+1 . byte-compile-two-args-with-one-extra)
					(0-1+1 . byte-compile-zero-or-one-arg-with-one-extra)
					(1-2+1 . byte-compile-one-or-two-args-with-one-extra)
					(2-3+1 . byte-compile-two-or-three-args-with-one-extra)
					(0+2 . byte-compile-no-args-with-two-extra)
					(1+2 . byte-compile-one-arg-with-two-extra)

					)))
			   compile-handler
			   (intern (concat "byte-compile-"
					   (symbol-name function))))))))
      (if opcode
	  (list 'progn fnform
		(list 'put (list 'quote function)
		      ''byte-opcode (list 'quote opcode))
		(list 'put (list 'quote opcode)
		      ''byte-opcode-invert (list 'quote function)))
	fnform))))

(defmacro byte-defop-compiler20 (function &optional compile-handler)
  ;; Just like byte-defop-compiler, but defines an opcode that will only
  ;; be used when byte-compile-emacs19-compatibility is false.
  (if (and (byte-compile-single-version)
	   byte-compile-emacs19-compatibility)
      ;; #### instead of doing nothing, this should do some remprops,
      ;; #### to protect against the case where a single-version compiler
      ;; #### is loaded into a world that has contained a multi-version one.
      nil
    (list 'progn
      (list 'put
	(list 'quote
	  (or (car (cdr-safe function))
	      (intern (concat "byte-"
		        (symbol-name (or (car-safe function) function))))))
	''emacs20-opcode t)
      (list 'byte-defop-compiler function compile-handler))))

;; XEmacs addition:
(defmacro byte-defop-compiler-rmsfun (function &optional compile-handler)
  ;; for functions like `eq' that compile into different opcodes depending
  ;; on the Emacs version: byte-old-eq for v19, byte-eq for v20.
  (let ((opcode (intern (concat "byte-" (symbol-name function))))
	(opcode19 (intern (concat "byte-old-" (symbol-name function))))
	(fnform
	 (list 'put (list 'quote function) ''byte-compile
	       (list 'quote
		     (or (cdr (assq compile-handler
				    '((2 . byte-compile-two-args-19->20)
				      )))
			 compile-handler
			 (intern (concat "byte-compile-"
					 (symbol-name function))))))))
    (list 'progn fnform
	  (list 'put (list 'quote function)
		''byte-opcode (list 'quote opcode))
	  (list 'put (list 'quote function)
		''byte-opcode19 (list 'quote opcode19))
	  (list 'put (list 'quote opcode)
		''byte-opcode-invert (list 'quote function))
	  (list 'put (list 'quote opcode19)
		''byte-opcode19-invert (list 'quote function)))))

(defmacro byte-defop-compiler-1 (function &optional compile-handler)
  (list 'byte-defop-compiler (list function nil) compile-handler))


(put 'byte-call 'byte-opcode-invert 'funcall)
(put 'byte-list1 'byte-opcode-invert 'list)
(put 'byte-list2 'byte-opcode-invert 'list)
(put 'byte-list3 'byte-opcode-invert 'list)
(put 'byte-list4 'byte-opcode-invert 'list)
(put 'byte-listN 'byte-opcode-invert 'list)
(put 'byte-concat2 'byte-opcode-invert 'concat)
(put 'byte-concat3 'byte-opcode-invert 'concat)
(put 'byte-concat4 'byte-opcode-invert 'concat)
(put 'byte-concatN 'byte-opcode-invert 'concat)
(put 'byte-insertN 'byte-opcode-invert 'insert)

;; How old is this stuff? -slb
;(byte-defop-compiler (dot byte-point)		0+1)
;(byte-defop-compiler (dot-max byte-point-max)	0+1)
;(byte-defop-compiler (dot-min byte-point-min)	0+1)
(byte-defop-compiler point		0+1)
(byte-defop-compiler-rmsfun eq		2)
(byte-defop-compiler point-max		0+1)
(byte-defop-compiler point-min		0+1)
(byte-defop-compiler following-char	0+1)
(byte-defop-compiler preceding-char	0+1)
(byte-defop-compiler current-column	0+1)
;; FSF has special function here; generalized here by the 1+2 stuff.
(byte-defop-compiler (indent-to-column byte-indent-to) 1+2)
(byte-defop-compiler indent-to		1+2)
(byte-defop-compiler-rmsfun equal	2)
(byte-defop-compiler eolp		0+1)
(byte-defop-compiler eobp		0+1)
(byte-defop-compiler bolp		0+1)
(byte-defop-compiler bobp		0+1)
(byte-defop-compiler current-buffer	0)
;;(byte-defop-compiler read-char	0) ;; obsolete
(byte-defop-compiler-rmsfun memq	2)
(byte-defop-compiler interactive-p	0)
(byte-defop-compiler widen		0+1)
(byte-defop-compiler end-of-line	0-1+1)
(byte-defop-compiler forward-char	0-1+1)
(byte-defop-compiler forward-line	0-1+1)
(byte-defop-compiler symbolp		1)
(byte-defop-compiler consp		1)
(byte-defop-compiler stringp		1)
(byte-defop-compiler listp		1)
(byte-defop-compiler not		1)
(byte-defop-compiler (null byte-not)	1)
(byte-defop-compiler car		1)
(byte-defop-compiler cdr		1)
(byte-defop-compiler length		1)
(byte-defop-compiler symbol-value	1)
(byte-defop-compiler symbol-function	1)
(byte-defop-compiler (1+ byte-add1)	1)
(byte-defop-compiler (1- byte-sub1)	1)
(byte-defop-compiler goto-char		1+1)
(byte-defop-compiler char-after		0-1+1)
(byte-defop-compiler set-buffer		1)
;;(byte-defop-compiler set-mark		1) ;; obsolete
(byte-defop-compiler forward-word	0-1+1)
(byte-defop-compiler char-syntax	1+1)
(byte-defop-compiler nreverse		1)
(byte-defop-compiler car-safe		1)
(byte-defop-compiler cdr-safe		1)
(byte-defop-compiler numberp		1)
(byte-defop-compiler integerp		1)
(byte-defop-compiler skip-chars-forward     1-2+1)
(byte-defop-compiler skip-chars-backward    1-2+1)
(byte-defop-compiler (eql byte-eq) 	2)
(byte-defop-compiler20 old-eq 	 	2)
(byte-defop-compiler20 old-memq		2)
(byte-defop-compiler cons		2)
(byte-defop-compiler aref		2)
(byte-defop-compiler get		2+1)
(byte-defop-compiler nth		2)
(byte-defop-compiler substring		2-3)
(byte-defop-compiler (move-marker byte-set-marker) 2-3)
(byte-defop-compiler set-marker		2-3)
(byte-defop-compiler match-beginning	1)
(byte-defop-compiler match-end		1)
(byte-defop-compiler upcase		1+1)
(byte-defop-compiler downcase		1+1)
(byte-defop-compiler string=		2)
(byte-defop-compiler string<		2)
(byte-defop-compiler (string-equal byte-string=) 2)
(byte-defop-compiler (string-lessp byte-string<) 2)
(byte-defop-compiler20 old-equal	2)
(byte-defop-compiler nthcdr		2)
(byte-defop-compiler elt		2)
(byte-defop-compiler20 old-member	2)
(byte-defop-compiler20 old-assq		2)
(byte-defop-compiler (rplaca byte-setcar) 2)
(byte-defop-compiler (rplacd byte-setcdr) 2)
(byte-defop-compiler setcar		2)
(byte-defop-compiler setcdr		2)
(byte-defop-compiler delete-region	2+1)
(byte-defop-compiler narrow-to-region	2+1)
(byte-defop-compiler (% byte-rem)	2)
(byte-defop-compiler aset		3)

(byte-defop-compiler-rmsfun member	2)
(byte-defop-compiler-rmsfun assq	2)

;;####(byte-defop-compiler move-to-column	1)
(byte-defop-compiler-1 interactive byte-compile-noop)
(byte-defop-compiler-1 domain byte-compile-domain)

;; As of GNU Emacs 19.18 and Lucid Emacs 19.8, mod and % are different: `%'
;; means integral remainder and may have a negative result; `mod' is always
;; positive, and accepts floating point args.  All code which uses `mod' and
;; requires the new interpretation must be compiled with bytecomp version 2.18
;; or newer, or the emitted code will run the byte-code for `%' instead of an
;; actual call to `mod'.  So be careful of compiling new code with an old
;; compiler.  Note also that `%' is more efficient than `mod' because the
;; former is byte-coded and the latter is not.
;;(byte-defop-compiler (mod byte-rem) 2)


(defun byte-compile-subr-wrong-args (form n)
  (when (memq 'subr-callargs byte-compile-warnings)
    (byte-compile-warn "%s called with %d arg%s, but requires %s"
		       (car form) (length (cdr form))
		       (if (= 1 (length (cdr form))) "" "s") n))
  ;; get run-time wrong-number-of-args error.
  (byte-compile-normal-call form))

(defun byte-compile-no-args (form)
  (case (length (cdr form))
    (0 (byte-compile-out (get (car form) 'byte-opcode) 0))
    (t (byte-compile-subr-wrong-args form "none"))))

(defun byte-compile-one-arg (form)
  (case (length (cdr form))
    (1 (byte-compile-form (car (cdr form)))  ;; Push the argument
       (byte-compile-out (get (car form) 'byte-opcode) 0))
    (t (byte-compile-subr-wrong-args form 1))))

(defun byte-compile-two-args (form)
  (case (length (cdr form))
    (2 (byte-compile-form (nth 1 form))  ;; Push the arguments
       (byte-compile-form (nth 2 form))
       (byte-compile-out (get (car form) 'byte-opcode) 0))
    (t (byte-compile-subr-wrong-args form 2))))

(defun byte-compile-three-args (form)
  (case (length (cdr form))
    (3 (byte-compile-form (nth 1 form))  ;; Push the arguments
       (byte-compile-form (nth 2 form))
       (byte-compile-form (nth 3 form))
       (byte-compile-out (get (car form) 'byte-opcode) 0))
    (t (byte-compile-subr-wrong-args form 3))))

(defun byte-compile-zero-or-one-arg (form)
  (case (length (cdr form))
    (0 (byte-compile-one-arg (append form '(nil))))
    (1 (byte-compile-one-arg form))
    (t (byte-compile-subr-wrong-args form "0-1"))))

(defun byte-compile-one-or-two-args (form)
  (case (length (cdr form))
    (1 (byte-compile-two-args (append form '(nil))))
    (2 (byte-compile-two-args form))
    (t (byte-compile-subr-wrong-args form "1-2"))))

(defun byte-compile-two-or-three-args (form)
  (case (length (cdr form))
    (2 (byte-compile-three-args (append form '(nil))))
    (3 (byte-compile-three-args form))
    (t (byte-compile-subr-wrong-args form "2-3"))))

;; from Ben Wing <ben@xemacs.org>: some inlined functions have extra
;; optional args added to them in XEmacs 19.12.  Changing the byte
;; interpreter to deal with these args would be wrong and cause
;; incompatibility, so we generate non-inlined calls for those cases.
;; Without the following functions, spurious warnings will be generated;
;; however, they would still compile correctly because
;; `byte-compile-subr-wrong-args' also converts the call to non-inlined.

(defun byte-compile-no-args-with-one-extra (form)
  (case (length (cdr form))
    (0 (byte-compile-no-args form))
    (1 (if (eq nil (nth 1 form))
	   (byte-compile-no-args (butlast form))
	 (byte-compile-normal-call form)))
    (t (byte-compile-subr-wrong-args form "0-1"))))

(defun byte-compile-one-arg-with-one-extra (form)
  (case (length (cdr form))
    (1 (byte-compile-one-arg form))
    (2 (if (eq nil (nth 2 form))
	   (byte-compile-one-arg (butlast form))
	 (byte-compile-normal-call form)))
    (t (byte-compile-subr-wrong-args form "1-2"))))

(defun byte-compile-two-args-with-one-extra (form)
  (case (length (cdr form))
    (2 (byte-compile-two-args form))
    (3 (if (eq nil (nth 3 form))
	   (byte-compile-two-args (butlast form))
	 (byte-compile-normal-call form)))
    (t (byte-compile-subr-wrong-args form "2-3"))))

(defun byte-compile-zero-or-one-arg-with-one-extra (form)
  (case (length (cdr form))
    (0 (byte-compile-one-arg (append form '(nil))))
    (1 (byte-compile-one-arg form))
    (2 (if (eq nil (nth 2 form))
	   (byte-compile-one-arg (butlast form))
	 (byte-compile-normal-call form)))
    (t (byte-compile-subr-wrong-args form "0-2"))))

(defun byte-compile-one-or-two-args-with-one-extra (form)
  (case (length (cdr form))
    (1 (byte-compile-two-args (append form '(nil))))
    (2 (byte-compile-two-args form))
    (3 (if (eq nil (nth 3 form))
	   (byte-compile-two-args (butlast form))
	 (byte-compile-normal-call form)))
    (t (byte-compile-subr-wrong-args form "1-3"))))

(defun byte-compile-two-or-three-args-with-one-extra (form)
  (case (length (cdr form))
    (2 (byte-compile-three-args (append form '(nil))))
    (3 (byte-compile-three-args form))
    (4 (if (eq nil (nth 4 form))
	   (byte-compile-three-args (butlast form))
	 (byte-compile-normal-call form)))
    (t (byte-compile-subr-wrong-args form "2-4"))))

(defun byte-compile-no-args-with-two-extra (form)
  (case (length (cdr form))
    (0     (byte-compile-no-args form))
    ((1 2) (byte-compile-normal-call form))
    (t     (byte-compile-subr-wrong-args form "0-2"))))

(defun byte-compile-one-arg-with-two-extra (form)
  (case (length (cdr form))
    (1     (byte-compile-one-arg form))
    ((2 3) (byte-compile-normal-call form))
    (t     (byte-compile-subr-wrong-args form "1-3"))))

;; XEmacs: used for functions that have a different opcode in v19 than v20.
;; this includes `eq', `equal', and other old-ified functions.
(defun byte-compile-two-args-19->20 (form)
  (if (not (= (length form) 3))
      (byte-compile-subr-wrong-args form 2)
    (byte-compile-form (car (cdr form)))  ;; Push the arguments
    (byte-compile-form (nth 2 form))
    (if (byte-compile-version-cond byte-compile-emacs19-compatibility)
	(byte-compile-out (get (car form) 'byte-opcode19) 0)
      (byte-compile-out (get (car form) 'byte-opcode) 0))))

(defun byte-compile-noop (form)
  (byte-compile-constant nil))

(defun byte-compile-discard ()
  (byte-compile-out 'byte-discard 0))

(defun byte-compile-max (form)
  (let ((args (cdr form)))
    (case (length args)
      (0 (byte-compile-subr-wrong-args form "1 or more"))
      (1 (byte-compile-form (car args))
	 (when (not byte-compile-delete-errors)
	   (byte-compile-out 'byte-dup 0)
	   (byte-compile-out 'byte-max 0)))
      (t (byte-compile-form (car args))
	 (dolist (elt (cdr args))
	   (byte-compile-form elt)
	   (byte-compile-out 'byte-max 0))))))

(defun byte-compile-min (form)
  (let ((args (cdr form)))
    (case (length args)
      (0 (byte-compile-subr-wrong-args form "1 or more"))
      (1 (byte-compile-form (car args))
	 (when (not byte-compile-delete-errors)
	   (byte-compile-out 'byte-dup 0)
	   (byte-compile-out 'byte-min 0)))
      (t (byte-compile-form (car args))
	 (dolist (elt (cdr args))
	   (byte-compile-form elt)
	   (byte-compile-out 'byte-min 0))))))


;; more complicated compiler macros

(byte-defop-compiler list)
(byte-defop-compiler concat)
(byte-defop-compiler fset)
(byte-defop-compiler insert)
(byte-defop-compiler-1 function byte-compile-function-form)
(byte-defop-compiler max)
(byte-defop-compiler min)
(byte-defop-compiler (+ byte-plus)	byte-compile-plus)
(byte-defop-compiler-1 -		byte-compile-minus)
(byte-defop-compiler (* byte-mult)	byte-compile-mult)
(byte-defop-compiler (/ byte-quo)	byte-compile-quo)
(byte-defop-compiler nconc)
(byte-defop-compiler-1 beginning-of-line)

(byte-defop-compiler (=  byte-eqlsign)	byte-compile-arithcompare)
(byte-defop-compiler (<  byte-lss)	byte-compile-arithcompare)
(byte-defop-compiler (>  byte-gtr)	byte-compile-arithcompare)
(byte-defop-compiler (<= byte-leq)	byte-compile-arithcompare)
(byte-defop-compiler (>= byte-geq)	byte-compile-arithcompare)

(defun byte-compile-arithcompare (form)
  (case (length (cdr form))
    (0 (byte-compile-subr-wrong-args form "1 or more"))
    (1 (if byte-compile-delete-errors
	   (byte-compile-constant t)
	 (byte-compile-normal-call form)))
    (2 (byte-compile-two-args form))
    (t (byte-compile-normal-call form))))

(byte-defop-compiler /= byte-compile-/=)

(defun byte-compile-/= (form)
  (case (length (cdr form))
    (0 (byte-compile-subr-wrong-args form "1 or more"))
    (1 (byte-compile-constant t))
    ;; optimize (/= X Y) to (not (= X Y))
    (2 (byte-compile-form-do-effect `(not (= ,@(cdr form)))))
    (t (byte-compile-normal-call form))))

;; buffer-substring now has its own function.  This used to be
;; 2+1, but now all args are optional.
(byte-defop-compiler buffer-substring)

(defun byte-compile-buffer-substring (form)
  ;; buffer-substring used to take exactly two args, but now takes 0-3.
  ;; convert 0-2 to two args and use special bytecode operand.
  ;; convert 3 args to a normal call.
  (case (length (cdr form))
    (0 (byte-compile-two-args (append form '(nil nil))))
    (1 (byte-compile-two-args (append form '(nil))))
    (2 (byte-compile-two-args form))
    (3 (byte-compile-normal-call form))
    (t (byte-compile-subr-wrong-args form "0-3"))))

(defun byte-compile-list (form)
  (let* ((args (cdr form))
	 (nargs (length args)))
    (cond
     ((= nargs 0)
      (byte-compile-constant nil))
     ((< nargs 5)
      (mapcar 'byte-compile-form args)
      (byte-compile-out
       (aref [byte-list1 byte-list2 byte-list3 byte-list4] (1- nargs))
       0))
     ((< nargs 256)
      (mapcar 'byte-compile-form args)
      (byte-compile-out 'byte-listN nargs))
     (t (byte-compile-normal-call form)))))

(defun byte-compile-concat (form)
  (let* ((args (cdr form))
	 (nargs (length args)))
    ;; Concat of one arg is not a no-op if arg is not a string.
    (cond
     ((memq nargs '(2 3 4))
      (mapcar 'byte-compile-form args)
      (byte-compile-out
       (aref [byte-concat2 byte-concat3 byte-concat4] (- nargs 2))
       0))
     ((eq nargs 0)
      (byte-compile-form ""))
     ((< nargs 256)
      (mapcar 'byte-compile-form args)
      (byte-compile-out 'byte-concatN nargs))
     ((byte-compile-normal-call form)))))

(defun byte-compile-plus (form)
  (let ((args (cdr form)))
    (case (length args)
      (0 (byte-compile-constant 0))
      (1 (byte-compile-plus (append form '(0))))
      (t (byte-compile-form (car args))
	 (dolist (elt (cdr args))
	   (case elt
	     (0  (when (not byte-compile-delete-errors)
		   (byte-compile-constant 0)
		   (byte-compile-out 'byte-plus 0)))
	     (+1 (byte-compile-out 'byte-add1 0))
	     (-1 (byte-compile-out 'byte-sub1 0))
	     (t
	      (byte-compile-form elt)
	      (byte-compile-out 'byte-plus 0))))))))

(defun byte-compile-minus (form)
  (let ((args (cdr form)))
    (case (length args)
      (0 (byte-compile-subr-wrong-args form "1 or more"))
      (1 (byte-compile-form (car args))
	 (byte-compile-out 'byte-negate 0))
      (t (byte-compile-form (car args))
	 (dolist (elt (cdr args))
	   (case elt
	     (0  (when (not byte-compile-delete-errors)
		   (byte-compile-constant 0)
		   (byte-compile-out 'byte-diff 0)))
	     (+1 (byte-compile-out 'byte-sub1 0))
	     (-1 (byte-compile-out 'byte-add1 0))
	     (t
	      (byte-compile-form elt)
	      (byte-compile-out 'byte-diff 0))))))))

(defun byte-compile-mult (form)
  (let ((args (cdr form)))
    (case (length args)
      (0 (byte-compile-constant 1))
      (1 (byte-compile-mult (append form '(1))))
      (t (byte-compile-form (car args))
	 (dolist (elt (cdr args))
	   (case elt
	     (1  (when (not byte-compile-delete-errors)
		   (byte-compile-constant 1)
		   (byte-compile-out 'byte-mult 0)))
	     (-1 (byte-compile-out 'byte-negate 0))
	     (2  (byte-compile-out 'byte-dup 0)
		 (byte-compile-out 'byte-plus 0))
	     (t
	      (byte-compile-form elt)
	      (byte-compile-out 'byte-mult 0))))))))

(defun byte-compile-quo (form)
  (let ((args (cdr form)))
    (case (length args)
      (0 (byte-compile-subr-wrong-args form "1 or more"))
      (1 (byte-compile-constant 1)
	 (byte-compile-form (car args))
	 (byte-compile-out 'byte-quo 0))
      (t (byte-compile-form (car args))
	 (dolist (elt (cdr args))
	   (case elt
	     (+1 (when (not byte-compile-delete-errors)
		   (byte-compile-constant 1)
		   (byte-compile-out 'byte-quo 0)))
	     (-1 (byte-compile-out 'byte-negate 0))
	     (t
	      (when (and (numberp elt) (= elt 0))
		(byte-compile-warn "Attempt to divide by zero: %s" form))
	      (byte-compile-form elt)
	      (byte-compile-out 'byte-quo 0))))))))

(defun byte-compile-nconc (form)
  (let ((args (cdr form)))
    (case (length args)
      (0 (byte-compile-constant nil))
      ;; nconc of one arg is a noop, even if that arg isn't a list.
      (1 (byte-compile-form (car args)))
      (t (byte-compile-form (car args))
	 (dolist (elt (cdr args))
	   (byte-compile-form elt)
	   (byte-compile-out 'byte-nconc 0))))))

(defun byte-compile-fset (form)
  ;; warn about forms like (fset 'foo '(lambda () ...))
  ;; (where the lambda expression is non-trivial...)
  ;; Except don't warn if the first argument is 'make-byte-code, because
  ;; I'm sick of getting mail asking me whether that warning is a problem.
  (let ((fn (nth 2 form))
	body)
    (when (and (eq (car-safe fn) 'quote)
	       (eq (car-safe (setq fn (nth 1 fn))) 'lambda)
	       (not (eq (car-safe (cdr-safe (nth 1 form))) 'make-byte-code)))
      (setq body (cdr (cdr fn)))
      (if (stringp (car body)) (setq body (cdr body)))
      (if (eq 'interactive (car-safe (car body))) (setq body (cdr body)))
      (if (and (consp (car body))
	       (not (eq 'byte-code (car (car body)))))
	  (byte-compile-warn
    "A quoted lambda form is the second argument of fset.  This is probably
     not what you want, as that lambda cannot be compiled.  Consider using
     the syntax (function (lambda (...) ...)) instead."))))
  (byte-compile-two-args form))

(defun byte-compile-funarg (form)
  ;; (mapcar '(lambda (x) ..) ..) ==> (mapcar (function (lambda (x) ..)) ..)
  ;; for cases where it's guaranteed that first arg will be used as a lambda.
  (byte-compile-normal-call
   (let ((fn (nth 1 form)))
     (if (and (eq (car-safe fn) 'quote)
	      (eq (car-safe (nth 1 fn)) 'lambda))
	 (cons (car form)
	       (cons (cons 'function (cdr fn))
		     (cdr (cdr form))))
       form))))

;; (function foo) must compile like 'foo, not like (symbol-function 'foo).
;; Otherwise it will be incompatible with the interpreter,
;; and (funcall (function foo)) will lose with autoloads.

(defun byte-compile-function-form (form)
  (byte-compile-constant
   (cond ((symbolp (nth 1 form))
	  (nth 1 form))
	 ((byte-compile-lambda (nth 1 form))))))

(defun byte-compile-insert (form)
  (cond ((null (cdr form))
	 (byte-compile-constant nil))
	((<= (length form) 256)
	 (mapcar 'byte-compile-form (cdr form))
	 (if (cdr (cdr form))
	     (byte-compile-out 'byte-insertN (length (cdr form)))
	   (byte-compile-out 'byte-insert 0)))
	((memq t (mapcar 'consp (cdr (cdr form))))
	 (byte-compile-normal-call form))
	;; We can split it; there is no function call after inserting 1st arg.
	(t
	 (while (setq form (cdr form))
	   (byte-compile-form (car form))
	   (byte-compile-out 'byte-insert 0)
	   (when (cdr form)
	     (byte-compile-discard))))))

;; alas, the old (pre-19.12, and all existing versions of FSFmacs 19)
;; byte compiler will generate incorrect code for
;; (beginning-of-line nil buffer) because it buggily doesn't
;; check the number of arguments passed to beginning-of-line.

(defun byte-compile-beginning-of-line (form)
  (let ((len (length form)))
    (cond ((> len 3)
	   (byte-compile-subr-wrong-args form "0-2"))
	  ((or (= len 3) (not (byte-compile-constp (nth 1 form))))
	   (byte-compile-normal-call form))
	  (t
	   (byte-compile-form
	    (list 'forward-line
		  (if (integerp (setq form (or (eval (nth 1 form)) 1)))
		      (1- form)
		    (byte-compile-warn
		     "Non-numeric arg to beginning-of-line: %s" form)
		    (list '1- (list 'quote form))))
	    t)
	   (byte-compile-constant nil)))))


(byte-defop-compiler set)
(byte-defop-compiler-1 setq)
(byte-defop-compiler-1 set-default)
(byte-defop-compiler-1 setq-default)

(byte-defop-compiler-1 quote)
(byte-defop-compiler-1 quote-form)

(defun byte-compile-setq (form)
  (let ((args (cdr form)) var val)
    (if (null args)
	;; (setq), with no arguments.
	(byte-compile-form nil for-effect)
      (while args
	(setq var (pop args))
	(if (null args)
	    ;; Odd number of args?  Let `set' get the error.
	    (byte-compile-form `(set ',var) for-effect)
	  (setq val (pop args))
	  (if (keywordp var)
	      ;; (setq :foo ':foo) compatibility kludge
	      (byte-compile-form `(set ',var ,val) (if args t for-effect))
	    (byte-compile-form val)
	    (unless (or args for-effect)
	      (byte-compile-out 'byte-dup 0))
	    (byte-compile-variable-ref 'byte-varset var))))))
  (setq for-effect nil))

(defun byte-compile-set (form)
  ;; Compile (set 'foo x) as (setq foo x) for trivially better code and so
  ;; that we get applicable warnings.  Compile everything else (including
  ;; malformed calls) like a normal 2-arg byte-coded function.
  (let ((symform (nth 1 form))
	(valform (nth 2 form))
	sym)
    (if (and (= (length form) 3)
	     (= (safe-length symform) 2)
	     (eq (car symform) 'quote)
	     (symbolp (setq sym (car (cdr symform))))
	     (not (byte-compile-constant-symbol-p sym)))
	(byte-compile-setq `(setq ,sym ,valform))
      (byte-compile-two-args form))))

(defun byte-compile-setq-default (form)
  (let ((args (cdr form)))
    (if (null args)
	;; (setq-default), with no arguments.
	(byte-compile-form nil for-effect)
      ;; emit multiple calls to `set-default' if necessary
      (while args
	(byte-compile-form
	 ;; Odd number of args?  Let `set-default' get the error.
	 `(set-default ',(pop args) ,@(if args (list (pop args)) nil))
	 (if args t for-effect)))))
  (setq for-effect nil))


(defun byte-compile-set-default (form)
  (let* ((args (cdr form))
	 (nargs (length args))
	 (var (car args)))
    (when (and (= (safe-length var) 2)
	       (eq (car var) 'quote))
      (let ((sym (nth 1 var)))
	(cond
	 ((not (symbolp sym))
	  (byte-compile-warn "Attempt to set-globally non-symbol %s" sym))
	 ((byte-compile-constant-symbol-p sym)
	  (byte-compile-warn "Attempt to set-globally constant symbol %s" sym))
	 ((let ((cell (assq sym byte-compile-bound-variables)))
	    (and cell
		 (setcdr cell (logior (cdr cell) byte-compile-assigned-bit))
		 t)))
	 ;; notice calls to set-default/setq-default for variables which
	 ;; have not been declared with defvar/defconst.
	 ((globally-boundp sym))	; OK
	 ((not (memq 'free-vars byte-compile-warnings))) ; warnings suppressed?
	 ((memq sym byte-compile-free-assignments)) ; already warned about sym
	 (t
	  (byte-compile-warn "assignment to free variable %s" sym)
	  (push sym byte-compile-free-assignments)))))
    (if (= nargs 2)
	;; now emit a normal call to set-default
	(byte-compile-normal-call form)
      (byte-compile-subr-wrong-args form 2))))


(defun byte-compile-quote (form)
  (byte-compile-constant (car (cdr form))))

(defun byte-compile-quote-form (form)
  (byte-compile-constant (byte-compile-top-level (nth 1 form))))


;;; control structures

(defun byte-compile-body (body &optional for-effect)
  (while (cdr body)
    (byte-compile-form (car body) t)
    (setq body (cdr body)))
  (byte-compile-form (car body) for-effect))

(proclaim-inline byte-compile-body-do-effect)
(defun byte-compile-body-do-effect (body)
  (byte-compile-body body for-effect)
  (setq for-effect nil))

(proclaim-inline byte-compile-form-do-effect)
(defun byte-compile-form-do-effect (form)
  (byte-compile-form form for-effect)
  (setq for-effect nil))

(byte-defop-compiler-1 inline byte-compile-progn)
(byte-defop-compiler-1 progn)
(byte-defop-compiler-1 prog1)
(byte-defop-compiler-1 prog2)
(byte-defop-compiler-1 if)
(byte-defop-compiler-1 cond)
(byte-defop-compiler-1 and)
(byte-defop-compiler-1 or)
(byte-defop-compiler-1 while)
(byte-defop-compiler-1 funcall)
(byte-defop-compiler-1 apply byte-compile-funarg)
(byte-defop-compiler-1 mapcar byte-compile-funarg)
(byte-defop-compiler-1 mapatoms byte-compile-funarg)
(byte-defop-compiler-1 mapconcat byte-compile-funarg)
(byte-defop-compiler-1 let)
(byte-defop-compiler-1 let*)

(defun byte-compile-progn (form)
  (byte-compile-body-do-effect (cdr form)))

(defun byte-compile-prog1 (form)
  (setq form (cdr form))
  (byte-compile-form-do-effect (pop form))
  (byte-compile-body form t))

(defun byte-compile-prog2 (form)
  (setq form (cdr form))
  (byte-compile-form (pop form) t)
  (byte-compile-form-do-effect (pop form))
  (byte-compile-body form t))

(defmacro byte-compile-goto-if (cond discard tag)
  `(byte-compile-goto
    (if ,cond
	(if ,discard 'byte-goto-if-not-nil 'byte-goto-if-not-nil-else-pop)
      (if ,discard 'byte-goto-if-nil 'byte-goto-if-nil-else-pop))
    ,tag))

(defun byte-compile-if (form)
  (byte-compile-form (car (cdr form)))
  (if (null (nthcdr 3 form))
      ;; No else-forms
      (let ((donetag (byte-compile-make-tag)))
	(byte-compile-goto-if nil for-effect donetag)
	(byte-compile-form (nth 2 form) for-effect)
	(byte-compile-out-tag donetag))
    (let ((donetag (byte-compile-make-tag)) (elsetag (byte-compile-make-tag)))
      (byte-compile-goto 'byte-goto-if-nil elsetag)
      (byte-compile-form (nth 2 form) for-effect)
      (byte-compile-goto 'byte-goto donetag)
      (byte-compile-out-tag elsetag)
      (byte-compile-body (cdr (cdr (cdr form))) for-effect)
      (byte-compile-out-tag donetag)))
  (setq for-effect nil))

(defun byte-compile-cond (clauses)
  (let ((donetag (byte-compile-make-tag))
	nexttag clause)
    (while (setq clauses (cdr clauses))
      (setq clause (car clauses))
      (cond ((or (eq (car clause) t)
		 (and (eq (car-safe (car clause)) 'quote)
		      (car-safe (cdr-safe (car clause)))))
	     ;; Unconditional clause
	     (setq clause (cons t clause)
		   clauses nil))
	    ((cdr clauses)
	     (byte-compile-form (car clause))
	     (if (null (cdr clause))
		 ;; First clause is a singleton.
		 (byte-compile-goto-if t for-effect donetag)
	       (setq nexttag (byte-compile-make-tag))
	       (byte-compile-goto 'byte-goto-if-nil nexttag)
	       (byte-compile-body (cdr clause) for-effect)
	       (byte-compile-goto 'byte-goto donetag)
	       (byte-compile-out-tag nexttag)))))
    ;; Last clause
    (and (cdr clause) (not (eq (car clause) t))
	 (progn (byte-compile-form (car clause))
		(byte-compile-goto-if nil for-effect donetag)
		(setq clause (cdr clause))))
    (byte-compile-body-do-effect clause)
    (byte-compile-out-tag donetag)))

(defun byte-compile-and (form)
  (let ((failtag (byte-compile-make-tag))
	(args (cdr form)))
    (if (null args)
	(byte-compile-form-do-effect t)
      (while (cdr args)
	(byte-compile-form (car args))
	(byte-compile-goto-if nil for-effect failtag)
	(setq args (cdr args)))
      (byte-compile-form-do-effect (car args))
      (byte-compile-out-tag failtag))))

(defun byte-compile-or (form)
  (let ((wintag (byte-compile-make-tag))
	(args (cdr form)))
    (if (null args)
	(byte-compile-form-do-effect nil)
      (while (cdr args)
	(byte-compile-form (car args))
	(byte-compile-goto-if t for-effect wintag)
	(setq args (cdr args)))
      (byte-compile-form-do-effect (car args))
      (byte-compile-out-tag wintag))))

(defun byte-compile-while (form)
  (let ((endtag (byte-compile-make-tag))
	(looptag (byte-compile-make-tag)))
    (byte-compile-out-tag looptag)
    (byte-compile-form (car (cdr form)))
    (byte-compile-goto-if nil for-effect endtag)
    (byte-compile-body (cdr (cdr form)) t)
    (byte-compile-goto 'byte-goto looptag)
    (byte-compile-out-tag endtag)
    (setq for-effect nil)))

(defun byte-compile-funcall (form)
  (mapcar 'byte-compile-form (cdr form))
  (byte-compile-out 'byte-call (length (cdr (cdr form)))))


(defun byte-compile-let (form)
  ;; First compute the binding values in the old scope.
  (let ((varlist (car (cdr form))))
    (while varlist
      (if (consp (car varlist))
	  (byte-compile-form (car (cdr (car varlist))))
	(byte-compile-push-constant nil))
      (setq varlist (cdr varlist))))
  (let ((byte-compile-bound-variables
	 (cons 'new-scope byte-compile-bound-variables))
	(varlist (reverse (car (cdr form))))
	(extra-flags
	 ;; If this let is of the form (let (...) (byte-code ...))
	 ;; then assume that it is the result of a transformation of
	 ;; ((lambda (...) (byte-code ... )) ...) and thus compile
	 ;; the variable bindings as if they were arglist bindings
	 ;; (which matters for what warnings.)
	 (if (eq 'byte-code (car-safe (nth 2 form)))
	     byte-compile-arglist-bit
	   nil)))
    (while varlist
      (byte-compile-variable-ref 'byte-varbind
				 (if (consp (car varlist))
				     (car (car varlist))
				   (car varlist))
				 extra-flags)
      (setq varlist (cdr varlist)))
    (byte-compile-body-do-effect (cdr (cdr form)))
    (if (memq 'unused-vars byte-compile-warnings)
	;; done compiling in this scope, warn now.
	(byte-compile-warn-about-unused-variables))
    (byte-compile-out 'byte-unbind (length (car (cdr form))))))

(defun byte-compile-let* (form)
  (let ((byte-compile-bound-variables
	 (cons 'new-scope byte-compile-bound-variables))
	(varlist (copy-sequence (car (cdr form)))))
    (while varlist
      (if (atom (car varlist))
	  (byte-compile-push-constant nil)
	(byte-compile-form (car (cdr (car varlist))))
	(setcar varlist (car (car varlist))))
      (byte-compile-variable-ref 'byte-varbind (car varlist))
      (setq varlist (cdr varlist)))
    (byte-compile-body-do-effect (cdr (cdr form)))
    (if (memq 'unused-vars byte-compile-warnings)
	;; done compiling in this scope, warn now.
	(byte-compile-warn-about-unused-variables))
    (byte-compile-out 'byte-unbind (length (car (cdr form))))))


;;(byte-defop-compiler-1 /= byte-compile-negated)
(byte-defop-compiler-1 atom byte-compile-negated)
(byte-defop-compiler-1 nlistp byte-compile-negated)

;;(put '/= 'byte-compile-negated-op '=)
(put 'atom 'byte-compile-negated-op 'consp)
(put 'nlistp 'byte-compile-negated-op 'listp)

(defun byte-compile-negated (form)
  (byte-compile-form-do-effect (byte-compile-negation-optimizer form)))

;; Even when optimization is off, atom is optimized to (not (consp ...)).
(defun byte-compile-negation-optimizer (form)
  ;; an optimizer for forms where <form1> is less efficient than (not <form2>)
  (list 'not
    (cons (or (get (car form) 'byte-compile-negated-op)
	      (error
	       "Compiler error: `%s' has no `byte-compile-negated-op' property"
	       (car form)))
	  (cdr form))))

;;; other tricky macro-like special-forms

(byte-defop-compiler-1 catch)
(byte-defop-compiler-1 unwind-protect)
(byte-defop-compiler-1 condition-case)
(byte-defop-compiler-1 save-excursion)
(byte-defop-compiler-1 save-current-buffer)
(byte-defop-compiler-1 save-restriction)
(byte-defop-compiler-1 save-window-excursion)
(byte-defop-compiler-1 with-output-to-temp-buffer)
;; no track-mouse.

(defun byte-compile-catch (form)
  (byte-compile-form (car (cdr form)))
  (byte-compile-push-constant
    (byte-compile-top-level (cons 'progn (cdr (cdr form))) for-effect))
  (byte-compile-out 'byte-catch 0))

(defun byte-compile-unwind-protect (form)
  (byte-compile-push-constant
   (byte-compile-top-level-body (cdr (cdr form)) t))
  (byte-compile-out 'byte-unwind-protect 0)
  (byte-compile-form-do-effect (car (cdr form)))
  (byte-compile-out 'byte-unbind 1))

;;(defun byte-compile-track-mouse (form)
;;  (byte-compile-form
;;   (list
;;    'funcall
;;    (list 'quote
;;	    (list 'lambda nil
;;		  (cons 'track-mouse
;;			(byte-compile-top-level-body (cdr form))))))))

(defun byte-compile-condition-case (form)
  (let* ((var (nth 1 form))
	 (byte-compile-bound-variables
	  (if var
	      (cons (cons var 0)
		    (cons 'new-scope byte-compile-bound-variables))
	    (cons 'new-scope byte-compile-bound-variables))))
    (or (symbolp var)
	(byte-compile-warn
	 "%s is not a variable-name or nil (in condition-case)"
	 (prin1-to-string var)))
    (byte-compile-push-constant var)
    (byte-compile-push-constant (byte-compile-top-level
				 (nth 2 form) for-effect))
    (let ((clauses (cdr (cdr (cdr form))))
	  compiled-clauses)
      (while clauses
	(let* ((clause (car clauses))
               (condition (car clause)))
          (cond ((not (or (symbolp condition)
			  (and (listp condition)
			       (let ((syms condition) (ok t))
				 (while syms
				   (if (not (symbolp (car syms)))
				       (setq ok nil))
				   (setq syms (cdr syms)))
				 ok))))
                 (byte-compile-warn
                   "%s is not a symbol naming a condition or a list of such (in condition-case)"
                   (prin1-to-string condition)))
;;                ((not (or (eq condition 't)
;;			  (and (stringp (get condition 'error-message))
;;			       (consp (get condition 'error-conditions)))))
;;                 (byte-compile-warn
;;                   "%s is not a known condition name (in condition-case)"
;;                   condition))
		)
	  (setq compiled-clauses
		(cons (cons condition
			    (byte-compile-top-level-body
			     (cdr clause) for-effect))
		      compiled-clauses)))
	(setq clauses (cdr clauses)))
      (byte-compile-push-constant (nreverse compiled-clauses)))
    (if (memq 'unused-vars byte-compile-warnings)
	;; done compiling in this scope, warn now.
	(byte-compile-warn-about-unused-variables))
    (byte-compile-out 'byte-condition-case 0)))


(defun byte-compile-save-excursion (form)
  (byte-compile-out 'byte-save-excursion 0)
  (byte-compile-body-do-effect (cdr form))
  (byte-compile-out 'byte-unbind 1))

(defun byte-compile-save-restriction (form)
  (byte-compile-out 'byte-save-restriction 0)
  (byte-compile-body-do-effect (cdr form))
  (byte-compile-out 'byte-unbind 1))

(defun byte-compile-save-current-buffer (form)
  (if (byte-compile-version-cond byte-compile-emacs19-compatibility)
      ;; `save-current-buffer' special form is not available in XEmacs 19.
      (byte-compile-form
       `(let ((_byte_compiler_save_buffer_emulation_closure_ (current-buffer)))
	  (unwind-protect
	      (progn ,@(cdr form))
	    (and (buffer-live-p _byte_compiler_save_buffer_emulation_closure_)
		 (set-buffer _byte_compiler_save_buffer_emulation_closure_)))))
    (byte-compile-out 'byte-save-current-buffer 0)
    (byte-compile-body-do-effect (cdr form))
    (byte-compile-out 'byte-unbind 1)))

(defun byte-compile-save-window-excursion (form)
  (byte-compile-push-constant
   (byte-compile-top-level-body (cdr form) for-effect))
  (byte-compile-out 'byte-save-window-excursion 0))

(defun byte-compile-with-output-to-temp-buffer (form)
  (byte-compile-form (car (cdr form)))
  (byte-compile-out 'byte-temp-output-buffer-setup 0)
  (byte-compile-body (cdr (cdr form)))
  (byte-compile-out 'byte-temp-output-buffer-show 0))


;;; top-level forms elsewhere

(byte-defop-compiler-1 defun)
(byte-defop-compiler-1 defmacro)
(byte-defop-compiler-1 defvar)
(byte-defop-compiler-1 defvar   byte-compile-defvar-or-defconst)
(byte-defop-compiler-1 defconst byte-compile-defvar-or-defconst)
(byte-defop-compiler-1 autoload)
;; According to Mly this can go now that lambda is a macro
;(byte-defop-compiler-1 lambda byte-compile-lambda-form)
(byte-defop-compiler-1 defalias)
(byte-defop-compiler-1 define-function)

(defun byte-compile-defun (form)
  ;; This is not used for file-level defuns with doc strings.
  (byte-compile-two-args ; Use this to avoid byte-compile-fset's warning.
   (list 'fset (list 'quote (nth 1 form))
	 (byte-compile-byte-code-maker
	  (byte-compile-lambda (cons 'lambda (cdr (cdr form)))))))
  (byte-compile-discard)
  (byte-compile-constant (nth 1 form)))

(defun byte-compile-defmacro (form)
  ;; This is not used for file-level defmacros with doc strings.
  (byte-compile-body-do-effect
   (list (list 'fset (list 'quote (nth 1 form))
	       (let ((code (byte-compile-byte-code-maker
			    (byte-compile-lambda
			     (cons 'lambda (cdr (cdr form)))))))
		 (if (eq (car-safe code) 'make-byte-code)
		     (list 'cons ''macro code)
		   (list 'quote (cons 'macro (eval code))))))
	 (list 'quote (nth 1 form)))))

(defun byte-compile-defvar-or-defconst (form)
  ;; This is not used for file-level defvar/defconsts with doc strings:
  ;; byte-compile-file-form-defvar-or-defconst will be used in that case.
  ;; (defvar|defconst VAR [VALUE [DOCSTRING]])
  (let ((fun (nth 0 form))
	(var (nth 1 form))
	(value (nth 2 form))
	(string (nth 3 form)))
    (when (> (length form) 4)
      (byte-compile-warn
       "%s %s called with %d arguments, but accepts only %s"
       fun var (length (cdr form)) 3))
    (when (memq 'free-vars byte-compile-warnings)
      (push (cons var byte-compile-global-bit) byte-compile-bound-variables))
    (byte-compile-body-do-effect
     (list
      ;; Put the defined variable in this library's load-history entry
      ;; just as a real defvar would, but only in top-level forms with values.
      (when (and (> (length form) 2)
		 (null byte-compile-current-form))
	`(push ',var current-load-list))
      (when (> (length form) 3)
	(when (and string (not (stringp string)))
	  (byte-compile-warn "Third arg to %s %s is not a string: %s"
			     fun var string))
	`(put ',var 'variable-documentation ,string))
      (if (cdr (cdr form))		; `value' provided
	  (if (eq fun 'defconst)
	      ;; `defconst' sets `var' unconditionally.
	      `(setq ,var ,value)
	    ;; `defvar' sets `var' only when unbound.
	    `(if (not (default-boundp ',var)) (set-default ',var ,value))))
      `',var))))

(defun byte-compile-autoload (form)
  (and (byte-compile-constp (nth 1 form))
       (byte-compile-constp (nth 5 form))
       (memq (eval (nth 5 form)) '(t macro))  ; macro-p
       (not (fboundp (eval (nth 1 form))))
       (byte-compile-warn
	"The compiler ignores `autoload' except at top level.  You should
     probably put the autoload of the macro `%s' at top-level."
	(eval (nth 1 form))))
  (byte-compile-normal-call form))

;; Lambda's in valid places are handled as special cases by various code.
;; The ones that remain are errors.
;; According to Mly this can go now that lambda is a macro
;(defun byte-compile-lambda-form (form)
;  (byte-compile-warn
;   "`lambda' used in function position is invalid: probably you mean #'%s"
;   (let ((print-escape-newlines t)
;	 (print-level 4)
;	 (print-length 4))
;     (prin1-to-string form)))
;  (byte-compile-normal-call
;   (list 'signal ''error
;	 (list 'quote (list "`lambda' used in function position" form)))))

;; Compile normally, but deal with warnings for the function being defined.
(defun byte-compile-defalias (form)
  (if (and (consp (cdr form)) (consp (nth 1 form))
	   (eq (car (nth 1 form)) 'quote)
	   (consp (cdr (nth 1 form)))
	   (symbolp (nth 1 (nth 1 form)))
	   (consp (nthcdr 2 form))
	   (consp (nth 2 form))
	   (eq (car (nth 2 form)) 'quote)
	   (consp (cdr (nth 2 form)))
	   (symbolp (nth 1 (nth 2 form))))
      (progn
	(byte-compile-defalias-warn (nth 1 (nth 1 form))
				    (nth 1 (nth 2 form)))
	(setq byte-compile-function-environment
	      (cons (cons (nth 1 (nth 1 form))
			  (nth 1 (nth 2 form)))
		    byte-compile-function-environment))))
  (byte-compile-normal-call form))

(defun byte-compile-define-function (form)
  (byte-compile-defalias form))

;; Turn off warnings about prior calls to the function being defalias'd.
;; This could be smarter and compare those calls with
;; the function it is being aliased to.
(defun byte-compile-defalias-warn (new alias)
  (let ((calls (assq new byte-compile-unresolved-functions)))
    (if calls
	(setq byte-compile-unresolved-functions
	      (delq calls byte-compile-unresolved-functions)))))

;;; tags

;; Note: Most operations will strip off the 'TAG, but it speeds up
;; optimization to have the 'TAG as a part of the tag.
;; Tags will be (TAG . (tag-number . stack-depth)).
(defun byte-compile-make-tag ()
  (list 'TAG (setq byte-compile-tag-number (1+ byte-compile-tag-number))))


(defun byte-compile-out-tag (tag)
  (push tag byte-compile-output)
  (if (cdr (cdr tag))
      (progn
	;; ## remove this someday
	(and byte-compile-depth
	  (not (= (cdr (cdr tag)) byte-compile-depth))
	  (error "Compiler bug: depth conflict at tag %d" (car (cdr tag))))
	(setq byte-compile-depth (cdr (cdr tag))))
    (setcdr (cdr tag) byte-compile-depth)))

(defun byte-compile-goto (opcode tag)
  (push (cons opcode tag) byte-compile-output)
  (setcdr (cdr tag) (if (memq opcode byte-goto-always-pop-ops)
			(1- byte-compile-depth)
		      byte-compile-depth))
  (setq byte-compile-depth (and (not (eq opcode 'byte-goto))
				(1- byte-compile-depth))))

(defun byte-compile-out (opcode offset)
  (push (cons opcode offset) byte-compile-output)
  (case opcode
    (byte-call
     (setq byte-compile-depth (- byte-compile-depth offset)))
    (byte-return
     ;; This is actually an unnecessary case, because there should be
     ;; no more opcodes behind byte-return.
     (setq byte-compile-depth nil))
    (t
     (setq byte-compile-depth (+ byte-compile-depth
				 (or (aref byte-stack+-info
					   (symbol-value opcode))
				     (- (1- offset))))
	   byte-compile-maxdepth (max byte-compile-depth
				      byte-compile-maxdepth))))
  ;;(if (< byte-compile-depth 0) (error "Compiler error: stack underflow"))
  )


;;; call tree stuff

(defun byte-compile-annotate-call-tree (form)
  (let (entry)
    ;; annotate the current call
    (if (setq entry (assq (car form) byte-compile-call-tree))
	(or (memq byte-compile-current-form (nth 1 entry)) ;callers
	    (setcar (cdr entry)
		    (cons byte-compile-current-form (nth 1 entry))))
      (push (list (car form) (list byte-compile-current-form) nil)
	    byte-compile-call-tree))
    ;; annotate the current function
    (if (setq entry (assq byte-compile-current-form byte-compile-call-tree))
	(or (memq (car form) (nth 2 entry)) ;called
	    (setcar (cdr (cdr entry))
		    (cons (car form) (nth 2 entry))))
      (push (list byte-compile-current-form nil (list (car form)))
	    byte-compile-call-tree))))

;; Renamed from byte-compile-report-call-tree
;; to avoid interfering with completion of byte-compile-file.
;;;###autoload
(defun display-call-tree (&optional filename)
  "Display a call graph of a specified file.
This lists which functions have been called, what functions called
them, and what functions they call.  The list includes all functions
whose definitions have been compiled in this Emacs session, as well as
all functions called by those functions.

The call graph does not include macros, inline functions, or
primitives that the byte-code interpreter knows about directly \(eq,
cons, etc.\).

The call tree also lists those functions which are not known to be called
\(that is, to which no calls have been compiled\), and which cannot be
invoked interactively."
  (interactive)
  (message "Generating call tree...")
  (with-output-to-temp-buffer "*Call-Tree*"
    (set-buffer "*Call-Tree*")
    (erase-buffer)
    (message "Generating call tree... (sorting on %s)"
	     byte-compile-call-tree-sort)
    (insert "Call tree for "
	    (cond ((null byte-compile-current-file) (or filename "???"))
		  ((stringp byte-compile-current-file)
		   byte-compile-current-file)
		  (t (buffer-name byte-compile-current-file)))
	    " sorted on "
	    (prin1-to-string byte-compile-call-tree-sort)
	    ":\n\n")
    (if byte-compile-call-tree-sort
	(setq byte-compile-call-tree
	      (sort byte-compile-call-tree
		    (cond
		     ((eq byte-compile-call-tree-sort 'callers)
		      #'(lambda (x y) (< (length (nth 1 x))
					 (length (nth 1 y)))))
		     ((eq byte-compile-call-tree-sort 'calls)
		      #'(lambda (x y) (< (length (nth 2 x))
					 (length (nth 2 y)))))
		     ((eq byte-compile-call-tree-sort 'calls+callers)
		      #'(lambda (x y) (< (+ (length (nth 1 x))
					    (length (nth 2 x)))
					 (+ (length (nth 1 y))
					    (length (nth 2 y))))))
		     ((eq byte-compile-call-tree-sort 'name)
		      #'(lambda (x y) (string< (car x)
					       (car y))))
		     (t (error
		      "`byte-compile-call-tree-sort': `%s' - unknown sort mode"
			       byte-compile-call-tree-sort))))))
    (message "Generating call tree...")
    (let ((rest byte-compile-call-tree)
	  (b (current-buffer))
	  f p
	  callers calls)
      (while rest
	(prin1 (car (car rest)) b)
	(setq callers (nth 1 (car rest))
	      calls (nth 2 (car rest)))
	(insert "\t"
	  (cond ((not (fboundp (setq f (car (car rest)))))
		 (if (null f)
		     " <top level>";; shouldn't insert nil then, actually -sk
		   " <not defined>"))
		((subrp (setq f (symbol-function f)))
		 " <subr>")
		((symbolp f)
		 (format " ==> %s" f))
		((compiled-function-p f)
		 "<compiled function>")
		((not (consp f))
		 "<malformed function>")
		((eq 'macro (car f))
		 (if (or (compiled-function-p (cdr f))
			 (assq 'byte-code (cdr (cdr (cdr f)))))
		     " <compiled macro>"
		   " <macro>"))
		((assq 'byte-code (cdr (cdr f)))
		 "<compiled lambda>")
		((eq 'lambda (car f))
		 "<function>")
		(t "???"))
	  (format " (%d callers + %d calls = %d)"
		  ;; Does the optimizer eliminate common subexpressions?-sk
		  (length callers)
		  (length calls)
		  (+ (length callers) (length calls)))
	  "\n")
	(if callers
	    (progn
	      (insert "  called by:\n")
	      (setq p (point))
	      (insert "    " (if (car callers)
				 (mapconcat 'symbol-name callers ", ")
			       "<top level>"))
	      (let ((fill-prefix "    "))
		(fill-region-as-paragraph p (point)))))
	(if calls
	    (progn
	      (insert "  calls:\n")
	      (setq p (point))
	      (insert "    " (mapconcat 'symbol-name calls ", "))
	      (let ((fill-prefix "    "))
		(fill-region-as-paragraph p (point)))))
	(insert "\n")
	(setq rest (cdr rest)))

      (message "Generating call tree...(finding uncalled functions...)")
      (setq rest byte-compile-call-tree)
      (let ((uncalled nil))
	(while rest
	  (or (nth 1 (car rest))
	      (null (setq f (car (car rest))))
	      (byte-compile-fdefinition f t)
	      (commandp (byte-compile-fdefinition f nil))
	      (setq uncalled (cons f uncalled)))
	  (setq rest (cdr rest)))
	(if uncalled
	    (let ((fill-prefix "  "))
	      (insert "Noninteractive functions not known to be called:\n  ")
	      (setq p (point))
	      (insert (mapconcat 'symbol-name (nreverse uncalled) ", "))
	      (fill-region-as-paragraph p (point)))))
      )
    (message "Generating call tree...done.")
    ))


;;; by crl@newton.purdue.edu
;;;  Only works noninteractively.
;;;###autoload
(defun batch-byte-compile ()
  "Run `byte-compile-file' on the files remaining on the command line.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs.
Each file is processed even if an error occurred previously.
For example, invoke \"xemacs -batch -f batch-byte-compile $emacs/ ~/*.el\"."
  ;; command-line-args-left is what is left of the command line (from
  ;; startup.el)
  (defvar command-line-args-left)	;Avoid 'free variable' warning
  (if (not noninteractive)
      (error "`batch-byte-compile' is to be used only with -batch"))
  (let ((error nil))
    (while command-line-args-left
      (if (null (batch-byte-compile-one-file))
	  (setq error t)))
    (message "Done")
    (kill-emacs (if error 1 0))))

;;;###autoload
(defun batch-byte-compile-one-file ()
  "Run `byte-compile-file' on a single file remaining on the command line.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs."
  ;; command-line-args-left is what is left of the command line (from
  ;; startup.el)
  (defvar command-line-args-left)	;Avoid 'free variable' warning
  (if (not noninteractive)
      (error "`batch-byte-compile-one-file' is to be used only with -batch"))
  (let (error
	(file-to-process (car command-line-args-left)))
    (setq command-line-args-left (cdr command-line-args-left))
    (if (file-directory-p (expand-file-name file-to-process))
	(let ((files (directory-files file-to-process))
	      source dest)
	  (while files
	    (if (and (string-match emacs-lisp-file-regexp (car files))
		     (not (auto-save-file-name-p (car files)))
		     (setq source (expand-file-name
				   (car files)
				   file-to-process))
		     (setq dest (byte-compile-dest-file source))
		     (file-exists-p dest)
		     (file-newer-than-file-p source dest))
		(if (null (batch-byte-compile-1 source))
		    (setq error t)))
	    (setq files (cdr files)))
	  (null error))
      (batch-byte-compile-1 file-to-process))))

(defun batch-byte-compile-one-file-here ()
  "Run `byte-compile-file' on a single file remaining on the command line.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs."
  ;; command-line-args-left is what is left of the command line (from
  ;; startup.el)
  (defvar command-line-args-left)	;Avoid 'free variable' warning
  (if (not noninteractive)
      (error "`batch-byte-compile-one-file-here' is to be used only with -batch"))
  ;; we hard-redefine it, since we ought to be called in batch mode only
  (fset 'byte-compile-dest-file
        #'(lambda (filename)
            "Convert an Emacs Lisp source file name to a compiled file name."
            (let ((outfile
                   (file-name-sans-extension
                    (if (string-match "lisp/" filename)
                        (substring filename (match-end 0))
                      filename))))
              (expand-file-name (concat outfile ".elc") default-directory))))
  (batch-byte-compile-one-file))

(defun batch-byte-compile-1 (file)
  (condition-case err
      (progn (byte-compile-file file) t)
    (error
     (princ ">>Error occurred processing ")
     (princ file)
     (princ ": ")
     (if (fboundp 'display-error) ; XEmacs 19.8+
	 (display-error err nil)
       (princ (or (get (car err) 'error-message) (car err)))
       (mapcar #'(lambda (x) (princ " ") (prin1 x)) (cdr err)))
     (princ "\n")
     nil)))

;;;###autoload
(defun batch-byte-recompile-directory-norecurse ()
  "Same as `batch-byte-recompile-directory' but without recursion."
  (setq byte-recompile-directory-recursively nil)
  (batch-byte-recompile-directory))

;;;###autoload
(defun batch-byte-recompile-directory ()
  "Runs `byte-recompile-directory' on the dirs remaining on the command line.
Must be used only with `-batch', and kills Emacs on completion.
For example, invoke `xemacs -batch -f batch-byte-recompile-directory .'."
  ;; command-line-args-left is what is left of the command line (startup.el)
  (defvar command-line-args-left)	;Avoid 'free variable' warning
  (if (not noninteractive)
      (error "batch-byte-recompile-directory is to be used only with -batch"))
  (or command-line-args-left
      (setq command-line-args-left '(".")))
  (let ((byte-recompile-directory-ignore-errors-p t))
    (while command-line-args-left
      (byte-recompile-directory (car command-line-args-left))
      (setq command-line-args-left (cdr command-line-args-left))))
  (kill-emacs 0))

(make-obsolete 'elisp-compile-defun 'compile-defun)
(make-obsolete 'byte-compile-report-call-tree 'display-call-tree)

;; other make-obsolete calls in obsolete.el.

(provide 'byte-compile)
(provide 'bytecomp)


;;; report metering (see the hacks in bytecode.c)

(if (boundp 'byte-code-meter)
    (defun byte-compile-report-ops ()
      (defvar byte-code-meter)
      (with-output-to-temp-buffer "*Meter*"
	(set-buffer "*Meter*")
	(let ((i 0) n op off)
	  (while (< i 256)
	    (setq n (aref (aref byte-code-meter 0) i)
		  off nil)
	    (if t ;(not (zerop n))
		(progn
		  (setq op i)
		  (setq off nil)
		  (cond ((< op byte-nth)
			 (setq off (logand op 7))
			 (setq op (logand op 248)))
			((>= op byte-constant)
			 (setq off (- op byte-constant)
			       op byte-constant)))
		  (setq op (aref byte-code-vector op))
		  (insert (format "%-4d" i))
		  (insert (symbol-name op))
		  (if off (insert " [" (int-to-string off) "]"))
		  (indent-to 40)
		  (insert (int-to-string n) "\n")))
	    (setq i (1+ i)))))))


;; To avoid "lisp nesting exceeds max-lisp-eval-depth" when bytecomp compiles
;; itself, compile some of its most used recursive functions (at load time).
;;
(eval-when-compile
 (or (compiled-function-p (symbol-function 'byte-compile-form))
     (assq 'byte-code (symbol-function 'byte-compile-form))
     (let ((byte-optimize nil) ; do it fast
	   (byte-compile-warnings nil))
       (mapcar #'(lambda (x)
		   (or noninteractive (message "compiling %s..." x))
		   (byte-compile x)
		   (or noninteractive (message "compiling %s...done" x)))
	       '(byte-compile-normal-call
		 byte-compile-form
		 byte-compile-body
		 ;; Inserted some more than necessary, to speed it up.
		 byte-compile-top-level
		 byte-compile-out-toplevel
		 byte-compile-constant
		 byte-compile-variable-ref))))
 nil)

;;; bytecomp.el ends here
