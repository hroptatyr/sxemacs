;;; byte-optimize.el --- the optimization passes of the emacs-lisp byte compiler.

;;; Copyright (c) 1991, 1994 Free Software Foundation, Inc.

;; Authors: Jamie Zawinski <jwz@jwz.org>
;;          Hallvard Furuseth <hbf@ulrik.uio.no>
;;          Martin Buchholz <martin@xemacs.org>
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

;;; Synched up with: FSF 20.7 except where marked.
;;; [[ Synched up with: FSF 20.7. ]]
;;; DO NOT PUT IN AN INVALID SYNC MESSAGE WHEN YOU DO A PARTIAL SYNC. --ben

;; BEGIN SYNC WITH 20.7.

;;; Commentary:

;; ========================================================================
;; "No matter how hard you try, you can't make a racehorse out of a pig.
;; You can, however, make a faster pig."
;;
;; Or, to put it another way, the emacs byte compiler is a VW Bug.  This code
;; makes it be a VW Bug with fuel injection and a turbocharger...  You're
;; still not going to make it go faster than 70 mph, but it might be easier
;; to get it there.
;;

;; TO DO:
;;
;; (apply #'(lambda (x &rest y) ...) 1 (foo))
;;
;; maintain a list of functions known not to access any global variables
;; (actually, give them a 'dynamically-safe property) and then
;;   (let ( v1 v2 ... vM vN ) <...dynamically-safe...> )  ==>
;;   (let ( v1 v2 ... vM ) vN <...dynamically-safe...> )
;; by recursing on this, we might be able to eliminate the entire let.
;; However certain variables should never have their bindings optimized
;; away, because they affect everything.
;;   (put 'debug-on-error 'binding-is-magic t)
;;   (put 'debug-on-abort 'binding-is-magic t)
;;   (put 'debug-on-next-call 'binding-is-magic t)
;;   (put 'mocklisp-arguments 'binding-is-magic t)
;;   (put 'inhibit-quit 'binding-is-magic t)
;;   (put 'quit-flag 'binding-is-magic t)
;;   (put 't 'binding-is-magic t)
;;   (put 'nil 'binding-is-magic t)
;; possibly also
;;   (put 'gc-cons-threshold 'binding-is-magic t)
;;   (put 'track-mouse 'binding-is-magic t)
;; others?
;;
;; Simple defsubsts often produce forms like
;;    (let ((v1 (f1)) (v2 (f2)) ...)
;;       (FN v1 v2 ...))
;; It would be nice if we could optimize this to
;;    (FN (f1) (f2) ...)
;; but we can't unless FN is dynamically-safe (it might be dynamically
;; referring to the bindings that the lambda arglist established.)
;; One of the uncountable lossages introduced by dynamic scope...
;;
;; Maybe there should be a control-structure that says "turn on
;; fast-and-loose type-assumptive optimizations here."  Then when
;; we see a form like (car foo) we can from then on assume that
;; the variable foo is of type cons, and optimize based on that.
;; But, this won't win much because of (you guessed it) dynamic
;; scope.  Anything down the stack could change the value.
;; (Another reason it doesn't work is that it is perfectly valid
;; to call car with a null argument.)  A better approach might
;; be to allow type-specification of the form
;;   (put 'foo 'arg-types '(float (list integer) dynamic))
;;   (put 'foo 'result-type 'bool)
;; It should be possible to have these types checked to a certain
;; degree.
;;
;; collapse common subexpressions
;;
;; It would be nice if redundant sequences could be factored out as well,
;; when they are known to have no side-effects:
;;   (list (+ a b c) (+ a b c))   -->  a b add c add dup list-2
;; but beware of traps like
;;   (cons (list x y) (list x y))
;;
;; Tail-recursion elimination is not really possible in Emacs Lisp.
;; Tail-recursion elimination is almost always impossible when all variables
;; have dynamic scope, but given that the "return" byteop requires the
;; binding stack to be empty (rather than emptying it itself), there can be
;; no truly tail-recursive Emacs Lisp functions that take any arguments or
;; make any bindings.
;;
;; Here is an example of an Emacs Lisp function which could safely be
;; byte-compiled tail-recursively:
;;
;;  (defun tail-map (fn list)
;;    (cond (list
;;           (funcall fn (car list))
;;           (tail-map fn (cdr list)))))
;;
;; However, if there was even a single let-binding around the COND,
;; it could not be byte-compiled, because there would be an "unbind"
;; byte-op between the final "call" and "return."  Adding a
;; Bunbind_all byteop would fix this.
;;
;;   (defun foo (x y z) ... (foo a b c))
;;   ... (const foo) (varref a) (varref b) (varref c) (call 3) END: (return)
;;   ... (varref a) (varbind x) (varref b) (varbind y) (varref c) (varbind z) (goto 0) END: (unbind-all) (return)
;;   ... (varref a) (varset x) (varref b) (varset y) (varref c) (varset z) (goto 0) END: (return)
;;
;; this also can be considered tail recursion:
;;
;;   ... (const foo) (varref a) (call 1) (goto X) ... X: (return)
;; could generalize this by doing the optimization
;;   (goto X) ... X: (return)  -->  (return)
;;
;; But this doesn't solve all of the problems: although by doing tail-
;; recursion elimination in this way, the call-stack does not grow, the
;; binding-stack would grow with each recursive step, and would eventually
;; overflow.  I don't believe there is any way around this without lexical
;; scope.
;;
;; Wouldn't it be nice if Emacs Lisp had lexical scope.
;;
;; Idea: the form (lexical-scope) in a file means that the file may be
;; compiled lexically.  This proclamation is file-local.  Then, within
;; that file, "let" would establish lexical bindings, and "let-dynamic"
;; would do things the old way.  (Or we could use CL "declare" forms.)
;; We'd have to notice defvars and defconsts, since those variables should
;; always be dynamic, and attempting to do a lexical binding of them
;; should simply do a dynamic binding instead.
;; But!  We need to know about variables that were not necessarily defvarred
;; in the file being compiled (doing a boundp check isn't good enough.)
;; Fdefvar() would have to be modified to add something to the plist.
;;
;; A major disadvantage of this scheme is that the interpreter and compiler
;; would have different semantics for files compiled with (dynamic-scope).
;; Since this would be a file-local optimization, there would be no way to
;; modify the interpreter to obey this (unless the loader was hacked
;; in some grody way, but that's a really bad idea.)
;;
;; HA!  RMS removed the following paragraph from his version of
;; byte-optimize.el.
;;
;; Really the Right Thing is to make lexical scope the default across
;; the board, in the interpreter and compiler, and just FIX all of
;; the code that relies on dynamic scope of non-defvarred variables.

;; Other things to consider:

;; Associative math should recognize subcalls to identical function:
;;(disassemble #'(lambda (x) (+ (+ (foo) 1) (+ (bar) 2))))
;; This should generate the same as (1+ x) and (1- x)

;;(disassemble #'(lambda (x) (cons (+ x 1) (- x 1))))
;; An awful lot of functions always return a non-nil value.  If they're
;; error free also they may act as true-constants.

;;(disassemble #'(lambda (x) (and (point) (foo))))
;; When
;;   - all but one arguments to a function are constant
;;   - the non-constant argument is an if-expression (cond-expression?)
;; then the outer function can be distributed.  If the guarding
;; condition is side-effect-free [assignment-free] then the other
;; arguments may be any expressions.  Since, however, the code size
;; can increase this way they should be "simple".  Compare:

;;(disassemble #'(lambda (x) (eq (if (point) 'a 'b) 'c)))
;;(disassemble #'(lambda (x) (if (point) (eq 'a 'c) (eq 'b 'c))))

;; (car (cons A B)) -> (prog1 A B)
;;(disassemble #'(lambda (x) (car (cons (foo) 42))))

;; (cdr (cons A B)) -> (progn A B)
;;(disassemble #'(lambda (x) (cdr (cons 42 (foo)))))

;; (car (list A B ...)) -> (prog1 A ... B)
;;(disassemble #'(lambda (x) (car (list (foo) 42 (bar)))))

;; (cdr (list A B ...)) -> (progn A (list B ...))
;;(disassemble #'(lambda (x) (cdr (list 42 (foo) (bar)))))


;;; Code:

(require 'byte-compile "bytecomp")

(defun byte-compile-log-lap-1 (format &rest args)
  (if (aref byte-code-vector 0)
      (error "The old version of the disassembler is loaded.  Reload new-bytecomp as well."))
  (byte-compile-log-1
   (apply 'format format
	  (let (c a)
	    (mapcar
	     #'(lambda (arg)
		 (if (not (consp arg))
		     (if (and (symbolp arg)
			      (string-match "^byte-" (symbol-name arg)))
			 (intern (substring (symbol-name arg) 5))
		       arg)
		   (if (integerp (setq c (car arg)))
		       (error "non-symbolic byte-op %s" c))
		   (if (eq c 'TAG)
		       (setq c arg)
		     (setq a (cond ((memq c byte-goto-ops)
				    (car (cdr (cdr arg))))
				   ((memq c byte-constref-ops)
				    (car (cdr arg)))
				   (t (cdr arg))))
		     (setq c (symbol-name c))
		     (if (string-match "^byte-." c)
			 (setq c (intern (substring c 5)))))
		   (if (eq c 'constant) (setq c 'const))
		   (if (and (eq (cdr arg) 0)
			    (not (memq c '(unbind call const))))
		       c
		     (format "(%s %s)" c a))))
	     args)))))

(defmacro byte-compile-log-lap (format-string &rest args)
  (list 'and
	'(memq byte-optimize-log '(t byte))
	(cons 'byte-compile-log-lap-1
	      (cons format-string args))))


;;; byte-compile optimizers to support inlining

(put 'inline 'byte-optimizer 'byte-optimize-inline-handler)

(defun byte-optimize-inline-handler (form)
  "byte-optimize-handler for the `inline' special-form."
  (cons
   'progn
   (mapcar
    #'(lambda (sexp)
	(let ((fn (car-safe sexp)))
	  (if (and (symbolp fn)
		   (or (cdr (assq fn byte-compile-function-environment))
		       (and (fboundp fn)
			    (not (or (cdr (assq fn byte-compile-macro-environment))
				     (and (consp (setq fn (symbol-function fn)))
					  (eq (car fn) 'macro))
				     (subrp fn))))))
	      (byte-compile-inline-expand sexp)
	    sexp)))
    (cdr form))))


;; Splice the given lap code into the current instruction stream.
;; If it has any labels in it, you're responsible for making sure there
;; are no collisions, and that byte-compile-tag-number is reasonable
;; after this is spliced in.  The provided list is destroyed.
(defun byte-inline-lapcode (lap)
  (setq byte-compile-output (nconc (nreverse lap) byte-compile-output)))


(defun byte-compile-inline-expand (form)
  (let* ((name (car form))
	 (fn (or (cdr (assq name byte-compile-function-environment))
		 (and (fboundp name) (symbol-function name)))))
    (if (null fn)
	(progn
	  (byte-compile-warn "attempt to inline %s before it was defined" name)
	  form)
      ;; else
      (if (and (consp fn) (eq (car fn) 'autoload))
	  (progn
	    (load (nth 1 fn))
	    (setq fn (or (cdr (assq name byte-compile-function-environment))
			 (and (fboundp name) (symbol-function name))))))
      (if (and (consp fn) (eq (car fn) 'autoload))
	  (error "file \"%s\" didn't define \"%s\"" (nth 1 fn) name))
      (if (symbolp fn)
	  (byte-compile-inline-expand (cons fn (cdr form)))
	(if (compiled-function-p fn)
	    (progn
	      (fetch-bytecode fn)
	      (cons (list 'lambda (compiled-function-arglist fn)
			  (list 'byte-code
				(compiled-function-instructions fn)
				(compiled-function-constants fn)
				(compiled-function-stack-depth fn)))
		    (cdr form)))
	  (if (eq (car-safe fn) 'lambda)
	      (cons fn (cdr form))
	    ;; Give up on inlining.
	    form))))))

;;; ((lambda ...) ...)
;;;
(defun byte-compile-unfold-lambda (form &optional name)
  (or name (setq name "anonymous lambda"))
  (let ((lambda (car form))
	(values (cdr form)))
    (if (compiled-function-p lambda)
	(setq lambda (list 'lambda (compiled-function-arglist lambda)
			  (list 'byte-code
				(compiled-function-instructions lambda)
				(compiled-function-constants lambda)
				(compiled-function-stack-depth lambda)))))
    (let ((arglist (nth 1 lambda))
	  (body (cdr (cdr lambda)))
	  optionalp restp
	  bindings)
      (if (and (stringp (car body)) (cdr body))
	  (setq body (cdr body)))
      (if (and (consp (car body)) (eq 'interactive (car (car body))))
	  (setq body (cdr body)))
      (while arglist
	(cond ((eq (car arglist) '&optional)
	       ;; ok, I'll let this slide because funcall_lambda() does...
	       ;; (if optionalp (error "multiple &optional keywords in %s" name))
	       (if restp (error "&optional found after &rest in %s" name))
	       (if (null (cdr arglist))
		   (error "nothing after &optional in %s" name))
	       (setq optionalp t))
	      ((eq (car arglist) '&rest)
	       ;; ...but it is by no stretch of the imagination a reasonable
	       ;; thing that funcall_lambda() allows (&rest x y) and
	       ;; (&rest x &optional y) in arglists.
	       (if (null (cdr arglist))
		   (error "nothing after &rest in %s" name))
	       (if (cdr (cdr arglist))
		   (error "multiple vars after &rest in %s" name))
	       (setq restp t))
	      (restp
	       (setq bindings (cons (list (car arglist)
					  (and values (cons 'list values)))
				    bindings)
		     values nil))
	      ((and (not optionalp) (null values))
	       (byte-compile-warn "attempt to open-code %s with too few arguments" name)
	       (setq arglist nil values 'too-few))
	      (t
	       (setq bindings (cons (list (car arglist) (car values))
				    bindings)
		     values (cdr values))))
	(setq arglist (cdr arglist)))
      (if values
	  (progn
	    (or (eq values 'too-few)
		(byte-compile-warn
		 "attempt to open-code %s with too many arguments" name))
	    form)
       ;; This line, introduced in v1.10, can cause an infinite
       ;; recursion when inlining recursive defsubst's
;      (setq body (mapcar 'byte-optimize-form body))
	(let ((newform
	       (if bindings
		   (cons 'let (cons (nreverse bindings) body))
		 (cons 'progn body))))
	  (byte-compile-log "  %s\t==>\t%s" form newform)
	  newform)))))


;;; implementing source-level optimizers

(defun byte-optimize-form-code-walker (form for-effect)
  ;;
  ;; For normal function calls, We can just mapcar the optimizer the cdr.  But
  ;; we need to have special knowledge of the syntax of the special forms
  ;; like let and defun (that's why they're special forms :-).  (Actually,
  ;; the important aspect is that they are subrs that don't evaluate all of
  ;; their args.)
  ;;
  (let ((fn (car-safe form))
	tmp)
    (cond ((not (consp form))
	   (if (not (and for-effect
			 (or byte-compile-delete-errors
			     (not (symbolp form))
			     (eq form t))))
	     form))
	  ((eq fn 'quote)
	   (if (cdr (cdr form))
	       (byte-compile-warn "malformed quote form: %s"
				  (prin1-to-string form)))
	   ;; map (quote nil) to nil to simplify optimizer logic.
	   ;; map quoted constants to nil if for-effect (just because).
	   (and (nth 1 form)
		(not for-effect)
		form))
	  ((or (compiled-function-p fn)
	       (eq 'lambda (car-safe fn)))
	   (byte-compile-unfold-lambda form))
	  ((memq fn '(let let*))
	   ;; recursively enter the optimizer for the bindings and body
	   ;; of a let or let*.  This for depth-firstness: forms that
	   ;; are more deeply nested are optimized first.
	   (cons fn
	     (cons
	      (mapcar
	       #'(lambda (binding)
		   (if (symbolp binding)
		       binding
		     (if (cdr (cdr binding))
			 (byte-compile-warn "malformed let binding: %s"
					    (prin1-to-string binding)))
		     (list (car binding)
			   (byte-optimize-form (nth 1 binding) nil))))
	       (nth 1 form))
	      (byte-optimize-body (cdr (cdr form)) for-effect))))
	  ((eq fn 'cond)
	   (cons fn
		 (mapcar
		  #'(lambda (clause)
		      (if (consp clause)
			  (cons
			   (byte-optimize-form (car clause) nil)
			   (byte-optimize-body (cdr clause) for-effect))
			(byte-compile-warn "malformed cond form: %s"
					   (prin1-to-string clause))
			clause))
		  (cdr form))))
	  ((eq fn 'progn)
	   ;; as an extra added bonus, this simplifies (progn <x>) --> <x>
	   (if (cdr (cdr form))
	       (progn
		 (setq tmp (byte-optimize-body (cdr form) for-effect))
		 (if (cdr tmp) (cons 'progn tmp) (car tmp)))
	     (byte-optimize-form (nth 1 form) for-effect)))
	  ((eq fn 'prog1)
	   (if (cdr (cdr form))
	       (cons 'prog1
		     (cons (byte-optimize-form (nth 1 form) for-effect)
			   (byte-optimize-body (cdr (cdr form)) t)))
	     (byte-optimize-form (nth 1 form) for-effect)))
	  ((eq fn 'prog2)
	   (cons 'prog2
	     (cons (byte-optimize-form (nth 1 form) t)
	       (cons (byte-optimize-form (nth 2 form) for-effect)
		     (byte-optimize-body (cdr (cdr (cdr form))) t)))))

	  ((memq fn '(save-excursion save-restriction save-current-buffer))
	   ;; those subrs which have an implicit progn; it's not quite good
	   ;; enough to treat these like normal function calls.
	   ;; This can turn (save-excursion ...) into (save-excursion) which
	   ;; will be optimized away in the lap-optimize pass.
	   (cons fn (byte-optimize-body (cdr form) for-effect)))

	  ((eq fn 'with-output-to-temp-buffer)
	   ;; this is just like the above, except for the first argument.
	   (cons fn
	     (cons
	      (byte-optimize-form (nth 1 form) nil)
	      (byte-optimize-body (cdr (cdr form)) for-effect))))

	  ((eq fn 'if)
	   (cons fn
	     (cons (byte-optimize-form (nth 1 form) nil)
	       (cons
		(byte-optimize-form (nth 2 form) for-effect)
		(byte-optimize-body (nthcdr 3 form) for-effect)))))

	  ((memq fn '(and or))  ; remember, and/or are control structures.
	   ;; take forms off the back until we can't any more.
	   ;; In the future it could conceivably be a problem that the
	   ;; subexpressions of these forms are optimized in the reverse
	   ;; order, but it's ok for now.
	   (if for-effect
	       (let ((backwards (reverse (cdr form))))
		 (while (and backwards
			     (null (setcar backwards
					   (byte-optimize-form (car backwards)
							       for-effect))))
		   (setq backwards (cdr backwards)))
		 (if (and (cdr form) (null backwards))
		     (byte-compile-log
		      "  all subforms of %s called for effect; deleted" form))
		 (when backwards
		   ;; Now optimize the rest of the forms. We need the return
		   ;; values. We already did the car.
		   (setcdr backwards
			   (mapcar 'byte-optimize-form (cdr backwards))))
		 (cons fn (nreverse backwards)))
	     (cons fn (mapcar 'byte-optimize-form (cdr form)))))

	  ((eq fn 'interactive)
	   (byte-compile-warn "misplaced interactive spec: %s"
			      (prin1-to-string form))
	   nil)

	  ((memq fn '(defun defmacro function
		      condition-case save-window-excursion))
	   ;; These forms are compiled as constants or by breaking out
	   ;; all the subexpressions and compiling them separately.
	   form)

	  ((eq fn 'unwind-protect)
	   ;; the "protected" part of an unwind-protect is compiled (and thus
	   ;; optimized) as a top-level form, so don't do it here.  But the
	   ;; non-protected part has the same for-effect status as the
	   ;; unwind-protect itself.  (The protected part is always for effect,
	   ;; but that isn't handled properly yet.)
	   (cons fn
		 (cons (byte-optimize-form (nth 1 form) for-effect)
		       (cdr (cdr form)))))

	  ((eq fn 'catch)
	   ;; the body of a catch is compiled (and thus optimized) as a
	   ;; top-level form, so don't do it here.  The tag is never
	   ;; for-effect.  The body should have the same for-effect status
	   ;; as the catch form itself, but that isn't handled properly yet.
	   (cons fn
		 (cons (byte-optimize-form (nth 1 form) nil)
		       (cdr (cdr form)))))

	  ;; If optimization is on, this is the only place that macros are
	  ;; expanded.  If optimization is off, then macroexpansion happens
	  ;; in byte-compile-form.  Otherwise, the macros are already expanded
	  ;; by the time that is reached.
	  ((not (eq form
		    (setq form (macroexpand form
					    byte-compile-macro-environment))))
	   (byte-optimize-form form for-effect))

	  ;; Support compiler macros as in cl.el.
	  ((and (fboundp 'compiler-macroexpand)
		(symbolp (car-safe form))
		(get (car-safe form) 'cl-compiler-macro)
		(not (eq form
			 (setq form (compiler-macroexpand form)))))
	   (byte-optimize-form form for-effect))

	  ((not (symbolp fn))
	   (or (eq 'mocklisp (car-safe fn)) ; ha!
	       (byte-compile-warn "%s is a malformed function"
				  (prin1-to-string fn)))
	   form)

	  ((and for-effect (setq tmp (get fn 'side-effect-free))
		(or byte-compile-delete-errors
		    (eq tmp 'error-free)
		    (progn
		      (byte-compile-warn "%s called for effect"
					 (prin1-to-string form))
		      nil)))
	   (byte-compile-log "  %s called for effect; deleted" fn)
	   ;; appending a nil here might not be necessary, but it can't hurt.
	   (byte-optimize-form
	    (cons 'progn (append (cdr form) '(nil))) t))

	  (t
	   ;; Otherwise, no args can be considered to be for-effect,
	   ;; even if the called function is for-effect, because we
	   ;; don't know anything about that function.
	   (cons fn (mapcar 'byte-optimize-form (cdr form)))))))


(defun byte-optimize-form (form &optional for-effect)
  "The source-level pass of the optimizer."
  ;;
  ;; First, optimize all sub-forms of this one.
  (setq form (byte-optimize-form-code-walker form for-effect))
  ;;
  ;; After optimizing all subforms, optimize this form until it doesn't
  ;; optimize any further.  This means that some forms will be passed through
  ;; the optimizer many times, but that's necessary to make the for-effect
  ;; processing do as much as possible.
  ;;
  (let (opt new)
    (if (and (consp form)
	     (symbolp (car form))
	     (or (and for-effect
		      ;; we don't have any of these yet, but we might.
		      (setq opt (get (car form) 'byte-for-effect-optimizer)))
		 (setq opt (get (car form) 'byte-optimizer)))
	     (not (eq form (setq new (funcall opt form)))))
	(progn
;;	  (if (equal form new) (error "bogus optimizer -- %s" opt))
	  (byte-compile-log "  %s\t==>\t%s" form new)
	  (setq new (byte-optimize-form new for-effect))
	  new)
      form)))


(defun byte-optimize-body (forms all-for-effect)
  ;; Optimize the cdr of a progn or implicit progn; `forms' is a list of
  ;; forms, all but the last of which are optimized with the assumption that
  ;; they are being called for effect.  The last is for-effect as well if
  ;; all-for-effect is true.  Returns a new list of forms.
  (let ((rest forms)
	(result nil)
	fe new)
    (while rest
      (setq fe (or all-for-effect (cdr rest)))
      (setq new (and (car rest) (byte-optimize-form (car rest) fe)))
      (if (or new (not fe))
	  (setq result (cons new result)))
      (setq rest (cdr rest)))
    (nreverse result)))


;;; some source-level optimizers
;;;
;;; when writing optimizers, be VERY careful that the optimizer returns
;;; something not EQ to its argument if and ONLY if it has made a change.
;;; This implies that you cannot simply destructively modify the list;
;;; you must return something not EQ to it if you make an optimization.
;;;
;;; It is now safe to optimize code such that it introduces new bindings.

;; I'd like this to be a defsubst, but let's not be self-referential...
(defmacro byte-compile-trueconstp (form)
  ;; Returns non-nil if FORM is a non-nil constant.
  `(cond ((consp ,form) (eq (car ,form) 'quote))
	 ((not (symbolp ,form)))
	 ((eq ,form t))
	 ((keywordp ,form))))

;; If the function is being called with constant numeric args,
;; evaluate as much as possible at compile-time.  This optimizer
;; assumes that the function is associative, like + or *.
(defun byte-optimize-associative-math (form)
  (let ((args nil)
	(constants nil)
	(rest (cdr form)))
    (while rest
      (if (numberp (car rest))
	  (setq constants (cons (car rest) constants))
	  (setq args (cons (car rest) args)))
      (setq rest (cdr rest)))
    (if (cdr constants)
	(if args
	    (list (car form)
		  (apply (car form) constants)
		  (if (cdr args)
		      (cons (car form) (nreverse args))
		      (car args)))
	    (apply (car form) constants))
	form)))

;; If the function is being called with constant numeric args,
;; evaluate as much as possible at compile-time.  This optimizer
;; assumes that the function satisfies
;;   (op x1 x2 ... xn) == (op ...(op (op x1 x2) x3) ...xn)
;; like - and /.
(defun byte-optimize-nonassociative-math (form)
  (if (or (not (numberp (car (cdr form))))
	  (not (numberp (car (cdr (cdr form))))))
      form
    (let ((constant (car (cdr form)))
	  (rest (cdr (cdr form))))
      (while (numberp (car rest))
	(setq constant (funcall (car form) constant (car rest))
	      rest (cdr rest)))
      (if rest
	  (cons (car form) (cons constant rest))
	  constant))))

;;(defun byte-optimize-associative-two-args-math (form)
;;  (setq form (byte-optimize-associative-math form))
;;  (if (consp form)
;;      (byte-optimize-two-args-left form)
;;      form))

;;(defun byte-optimize-nonassociative-two-args-math (form)
;;  (setq form (byte-optimize-nonassociative-math form))
;;  (if (consp form)
;;      (byte-optimize-two-args-right form)
;;      form))

;; jwz: (byte-optimize-approx-equal 0.0 0.0) was returning nil
;; in xemacs 19.15 because it used < instead of <=.
(defun byte-optimize-approx-equal (x y)
  (<= (* (abs (- x y)) 100) (abs (+ x y))))

;; Collect all the constants from FORM, after the STARTth arg,
;; and apply FUN to them to make one argument at the end.
;; For functions that can handle floats, that optimization
;; can be incorrect because reordering can cause an overflow
;; that would otherwise be avoided by encountering an arg that is a float.
;; We avoid this problem by (1) not moving float constants and
;; (2) not moving anything if it would cause an overflow.
;;
;; This idea is going to be extended in case we have some
;; multi-precision library on-board.  Overflows are rare in that case,
;; but since there is no distinguishing syntax for mpfr, mpf and
;; emacs-floats, we will have problems to re-identify a printed real
;; number representation.  The worst thing that may happen is to lose
;; precision, that is why we attempt to treat any incoming real number
;; as bigfr, if provided, we fallback to bigf, if provided, and if
;; that does not help we fallback to float.
;;
;; BIG FAT WARNING:
;; Byte-compiled code with special number types is not readable by
;; SXEmacsen which do not have an mp spine.
;; Therefore always tag their usage using (featurep 'ent)
;; or the like
;; - hroptatyr
(defun byte-optimize-delay-constants-math (form start fun)
  ;; Merge all FORM's constants from number START, call FUN on them
  ;; and put the result at the end.
  (let ((rest (nthcdr (1- start) form))
	(orig form)
	;; t means we must check for overflow.
	(overflow (memq fun '(+ *))))
    (while (cdr (setq rest (cdr rest)))
      (if (if (featurep 'ent)
	      (numberp (car rest))
	    (integerp (car rest)))
	  (let (constants)
	    (setq form (copy-sequence form)
		  rest (nthcdr (1- start) form))
	    (while (setq rest (cdr rest))
	      (cond ((and (featurep 'ent)
			  (rationalp (car rest)))
		     (setq constants (cons (car rest) constants))
		     (setcar rest nil))
		    ((integerp (car rest))
		     (setq constants (cons (car rest) constants))
		     (setcar rest nil))
		    ((realp (car rest))
		     (setq constants
			   (cons
			    (coerce-number
			     (car rest)
			     (cond
			      ((featurep 'bigfr)
			       'bigfr)
			      ((featurep 'bigf)
			       'bigf)
			      ((featurep 'lisp-float-type)
			       'float)
			      (t
			       ;; shit ... what to do now?
			       ;;(segmentation-fault)
			       )))
			    constants))
		     (setcar rest nil))
		    ((and (or (featurep 'bigc)
			      (featurep 'bigg))
			  (complexp (car rest)))
		     (setq constants (cons (car rest) constants))
		     (setcar rest nil))
		    ((and (featurep 'resclass)
			  (declare-fboundp (residue-class-p (car rest))))
		     (setq constants (cons (car rest) constants))
		     (setcar rest nil))))
	    ;; If necessary, check now for overflow
	    ;; that might be caused by reordering.
	    (if (and overflow
		     ;; We have overflow if the result of doing the arithmetic
		     ;; on floats is not even close to the result
		     ;; of doing it on integers.
		     (not (featurep '(or bigz bigq bigf bigfr bigc bigg resclass)))
		     ;; This assumption, of course, is not valid if we
		     ;; have bigz numbers
		     (not (byte-optimize-approx-equal
			    (apply fun (mapcar 'float constants))
			    (float (apply fun constants)))))
		(setq form orig)
	      (setq form (nconc (delq nil form)
				(list (apply fun (nreverse constants)))))))))
    form))

;; END SYNC WITH 20.7.

;;; It is not safe to optimize calls to arithmetic ops with one arg
;;; away entirely (actually, it would be safe if we know the sole arg
;;; is not a marker or if it appears in other arithmetic).

;;; But this degree of paranoia is normally unjustified, so optimize unless
;;; the user has done (declaim (optimize (safety 3))).  See bytecomp.el.

(defun byte-optimize-plus (form)
  (byte-optimize-predicate (byte-optimize-delay-constants-math form 1 '+)))

(defun byte-optimize-multiply (form)
  (setq form (byte-optimize-delay-constants-math form 1 '*))
  ;; If there is a constant integer in FORM, it is now the last element.

  (case (car (last form))
    ;; (* x y 0) --> (progn x y 0)
    (0 (cons 'progn (cdr form)))
    (t (byte-optimize-predicate form))))

(defun byte-optimize-minus (form)
  ;; Put constants at the end, except the first arg.
  (setq form (byte-optimize-delay-constants-math form 2 '+))
  ;; Now only the first and last args can be integers.
  (let ((last (car (last (nthcdr 3 form)))))
    (cond
     ;; If form is (- CONST foo... CONST), merge first and last.
     ((and (numberp (nth 1 form)) (numberp last))
      (decf (nth 1 form) last)
      (butlast form))

     ;; (- 0 ...) -->
     ((eq 0 (nth 1 form))
      (case (length form)
	;; (- 0) --> 0
	(2 0)
	;; (- 0 x)  -->  (- x)
	(3 `(- ,(nth 2 form)))
	;; (- 0 x y ...)  -->  (- (- x) y ...)
	(t `(- (- ,(nth 2 form)) ,@(nthcdr 3 form)))))

     (t (byte-optimize-predicate form)))))

(defun byte-optimize-divide (form)
  ;; Put constants at the end, except the first arg.
  (setq form (byte-optimize-delay-constants-math form 2 '*))
  ;; Now only the first and last args can be integers.
  (let ((last (car (last (nthcdr 3 form)))))
    (cond
     ;; If form is (/ CONST foo... CONST), merge first and last.
     ((and (numberp (nth 1 form)) (numberp last))
      (condition-case nil
	  (cons (nth 0 form)
		(cons (/ (nth 1 form) last)
		      (butlast (cdr (cdr form)))))
	(error form)))

     ;; (/ 0 x y) --> (progn x y 0)
     ((eq (nth 1 form) 0)
      (append '(progn) (cdr (cdr form)) '(0)))

     ;; We don't have to check for divide-by-zero because `/' does.
     (t (byte-optimize-predicate form)))))

;; BEGIN SYNC WITH 20.7.

(defun byte-optimize-logmumble (form)
  (setq form (byte-optimize-delay-constants-math form 1 (car form)))
  (byte-optimize-predicate
   (cond ((memq 0 form)
	  (setq form (if (eq (car form) 'logand)
			 (cons 'progn (cdr form))
		       (delq 0 (copy-sequence form)))))
	 ((and (eq (car-safe form) 'logior)
	       (memq -1 form))
	  (cons 'progn (cdr form)))
	 (form))))


(defun byte-optimize-binary-predicate (form)
  (if (byte-compile-constp (nth 1 form))
      (if (byte-compile-constp (nth 2 form))
	  (condition-case ()
	      (list 'quote (eval form))
	    (error form))
	;; This can enable some lapcode optimizations.
	(list (car form) (nth 2 form) (nth 1 form)))
    form))

(defun byte-optimize-predicate (form)
  (let ((ok t)
	(rest (cdr form)))
    (while (and rest ok)
      (setq ok (byte-compile-constp (car rest))
	    rest (cdr rest)))
    (if ok
	(condition-case err
	    (list 'quote (eval form))
	  (error
	   (byte-compile-warn "evaluating %s: %s" form err)
	   form))
	form)))

(defun byte-optimize-identity (form)
  (if (and (cdr form) (null (cdr (cdr form))))
      (nth 1 form)
    (byte-compile-warn "identity called with %d arg%s, but requires 1"
		       (length (cdr form))
		       (if (= 1 (length (cdr form))) "" "s"))
    form))

(defun byte-optimize-car (form)
  (let ((arg (cadr form)))
    (cond
     ((and (byte-compile-trueconstp arg)
	   (not (and (consp arg)
		     (eq (car arg) 'quote)
		     (listp (cadr arg)))))
      (byte-compile-warn
       "taking car of a constant: %s" arg)
      form)
     ((and (eq (car-safe arg) 'cons)
	   (eq (length arg) 3))
      `(prog1 ,(nth 1 arg) ,(nth 2 arg)))
     ((eq (car-safe arg) 'list)
      `(prog1 ,@(cdr arg)))
     (t
      (byte-optimize-predicate form)))))

(defun byte-optimize-cdr (form)
  (let ((arg (cadr form)))
    (cond
     ((and (byte-compile-trueconstp arg)
	   (not (and (consp arg)
		     (eq (car arg) 'quote)
		     (listp (cadr arg)))))
      (byte-compile-warn
       "taking cdr of a constant: %s" arg)
      form)
     ((and (eq (car-safe arg) 'cons)
	    (eq (length arg) 3))
       `(progn ,(nth 1 arg) ,(nth 2 arg)))
      ((eq (car-safe arg) 'list)
       (if (> (length arg) 2)
	   `(progn ,(cadr arg) (list ,@(cddr arg)))
	 (cadr arg)))
      (t
       (byte-optimize-predicate form)))))

(put 'identity 'byte-optimizer 'byte-optimize-identity)

(put '+   'byte-optimizer 'byte-optimize-plus)
(put '*   'byte-optimizer 'byte-optimize-multiply)
(put '-   'byte-optimizer 'byte-optimize-minus)
(put '/   'byte-optimizer 'byte-optimize-divide)
(put '%   'byte-optimizer 'byte-optimize-predicate)
(put 'max 'byte-optimizer 'byte-optimize-associative-math)
(put 'min 'byte-optimizer 'byte-optimize-associative-math)

(put 'eq  'byte-optimizer 'byte-optimize-binary-predicate)
(put 'eql 'byte-optimizer 'byte-optimize-binary-predicate)
(put 'equal   'byte-optimizer 'byte-optimize-binary-predicate)
(put 'string= 'byte-optimizer 'byte-optimize-binary-predicate)
(put 'string-equal 'byte-optimizer 'byte-optimize-binary-predicate)

(put '=   'byte-optimizer 'byte-optimize-predicate)
(put '<   'byte-optimizer 'byte-optimize-predicate)
(put '>   'byte-optimizer 'byte-optimize-predicate)
(put '<=  'byte-optimizer 'byte-optimize-predicate)
(put '>=  'byte-optimizer 'byte-optimize-predicate)
(put '1+  'byte-optimizer 'byte-optimize-predicate)
(put '1-  'byte-optimizer 'byte-optimize-predicate)
(put 'not 'byte-optimizer 'byte-optimize-predicate)
(put 'null  'byte-optimizer 'byte-optimize-predicate)
(put 'memq  'byte-optimizer 'byte-optimize-predicate)
(put 'consp 'byte-optimizer 'byte-optimize-predicate)
(put 'listp 'byte-optimizer 'byte-optimize-predicate)
(put 'symbolp 'byte-optimizer 'byte-optimize-predicate)
(put 'stringp 'byte-optimizer 'byte-optimize-predicate)
(put 'string< 'byte-optimizer 'byte-optimize-predicate)
(put 'string-lessp 'byte-optimizer 'byte-optimize-predicate)
(put 'length 'byte-optimizer 'byte-optimize-predicate)

(put 'logand 'byte-optimizer 'byte-optimize-logmumble)
(put 'logior 'byte-optimizer 'byte-optimize-logmumble)
(put 'logxor 'byte-optimizer 'byte-optimize-logmumble)
(put 'lognot 'byte-optimizer 'byte-optimize-predicate)

(put 'car 'byte-optimizer 'byte-optimize-car)
(put 'cdr 'byte-optimizer 'byte-optimize-cdr)
(put 'car-safe 'byte-optimizer 'byte-optimize-predicate)
(put 'cdr-safe 'byte-optimizer 'byte-optimize-predicate)


;; I'm not convinced that this is necessary.  Doesn't the optimizer loop
;; take care of this? - Jamie
;; I think this may some times be necessary to reduce eg. (quote 5) to 5,
;; so arithmetic optimizers recognize the numeric constant.  - Hallvard
(put 'quote 'byte-optimizer 'byte-optimize-quote)
(defun byte-optimize-quote (form)
  (if (or (consp (nth 1 form))
	  (and (symbolp (nth 1 form))
	       ;; XEmacs addition:
	       (not (keywordp (nth 1 form)))
	       (not (memq (nth 1 form) '(nil t)))))
      form
    (nth 1 form)))

(defun byte-optimize-zerop (form)
  (cond ((numberp (nth 1 form))
	 (eval form))
	((featurep 'ent)
	 ;; we cannot compare to 0 anymore, since there are coercion
	 ;; issues and even non-comparable types
	 form)
	(byte-compile-delete-errors
	 (list '= (nth 1 form) 0))
	(form)))

(put 'zerop 'byte-optimizer 'byte-optimize-zerop)

(defun byte-optimize-and (form)
  ;; Simplify if less than 2 args.
  ;; if there is a literal nil in the args to `and', throw it and following
  ;; forms away, and surround the `and' with (progn ... nil).
  (cond ((null (cdr form)))
	((memq nil form)
	 (list 'progn
	       (byte-optimize-and
		(prog1 (setq form (copy-sequence form))
		  (while (nth 1 form)
		    (setq form (cdr form)))
		  (setcdr form nil)))
	       nil))
	((null (cdr (cdr form)))
	 (nth 1 form))
	((byte-optimize-predicate form))))

(defun byte-optimize-or (form)
  ;; Throw away nil's, and simplify if less than 2 args.
  ;; If there is a literal non-nil constant in the args to `or', throw away all
  ;; following forms.
  (if (memq nil form)
      (setq form (delq nil (copy-sequence form))))
  (let ((rest form))
    (while (cdr (setq rest (cdr rest)))
      (if (byte-compile-trueconstp (car rest))
	  (setq form (copy-sequence form)
		rest (setcdr (memq (car rest) form) nil))))
    (if (cdr (cdr form))
	(byte-optimize-predicate form)
      (nth 1 form))))

;; END SYNC WITH 20.7.

;;; For the byte optimizer, `cond' is just overly sweet syntactic sugar.
;;; So we rewrite (cond ...) in terms of `if' and `or',
;;; which are easier to optimize.
(defun byte-optimize-cond (form)
  (byte-optimize-cond-1 (cdr form)))

(defun byte-optimize-cond-1 (clauses)
  (cond
   ((null clauses) nil)
   ((consp (car clauses))
    (nconc
     (case (length (car clauses))
       (1 `(or ,(nth 0 (car clauses))))
       (2 `(if ,(nth 0 (car clauses)) ,(nth 1 (car clauses))))
       (t `(if ,(nth 0 (car clauses)) (progn ,@(cdr (car clauses))))))
     (when (cdr clauses) (list (byte-optimize-cond-1 (cdr clauses))))))
   (t (error "malformed cond clause %s" (car clauses)))))

;; BEGIN SYNC WITH 20.7.

(defun byte-optimize-if (form)
  ;; (if <true-constant> <then> <else...>) ==> <then>
  ;; (if <false-constant> <then> <else...>) ==> (progn <else...>)
  ;; (if <test> nil <else...>) ==> (if (not <test>) (progn <else...>))
  ;; (if <test> <then> nil) ==> (if <test> <then>)
  (let ((clause (nth 1 form)))
    (cond ((byte-compile-trueconstp clause)
	   (nth 2 form))
	  ((null clause)
	   (if (nthcdr 4 form)
	       (cons 'progn (nthcdr 3 form))
	     (nth 3 form)))
	  ((nth 2 form)
	   (if (equal '(nil) (nthcdr 3 form))
	       (list 'if clause (nth 2 form))
	     form))
	  ((or (nth 3 form) (nthcdr 4 form))
	   (list 'if
		 ;; Don't make a double negative;
		 ;; instead, take away the one that is there.
		 (if (and (consp clause) (memq (car clause) '(not null))
			  (= (length clause) 2)) ; (not xxxx) or (not (xxxx))
		     (nth 1 clause)
		   (list 'not clause))
		 (if (nthcdr 4 form)
		     (cons 'progn (nthcdr 3 form))
		   (nth 3 form))))
	  (t
	   (list 'progn clause nil)))))

(defun byte-optimize-while (form)
  (if (nth 1 form)
      form))

(put 'and   'byte-optimizer 'byte-optimize-and)
(put 'or    'byte-optimizer 'byte-optimize-or)
(put 'cond  'byte-optimizer 'byte-optimize-cond)
(put 'if    'byte-optimizer 'byte-optimize-if)
(put 'while 'byte-optimizer 'byte-optimize-while)

;; The supply of bytecodes is small and constrained by backward compatibility.
;; Several functions have byte-coded versions and hence are very efficient.
;; Related functions which can be expressed in terms of the byte-coded
;; ones should be transformed into bytecoded calls for efficiency.
;; This is especially the case for functions with a backward- and
;; forward- version, but with a bytecode only for the forward one.

;; Some programmers have hand-optimized calls like (backward-char)
;; into the call (forward-char -1).
;; But it's so much nicer for the byte-compiler to do this automatically!

;; (char-before) ==> (char-after (1- (point)))
(put 'char-before   'byte-optimizer 'byte-optimize-char-before)
(defun byte-optimize-char-before (form)
  `(char-after
    ,(cond
      ((null (nth 1 form))
       '(1- (point)))
      ((equal '(point) (nth 1 form))
       '(1- (point)))
      (t `(1- (or ,(nth 1 form) (point)))))
    ,@(cdr (cdr form))))

;; (backward-char n) ==> (forward-char (- n))
(put 'backward-char 'byte-optimizer 'byte-optimize-backward-char)
(defun byte-optimize-backward-char (form)
  `(forward-char
    ,(typecase (nth 1 form)
       (null -1)
       (integer (- (nth 1 form)))
       (t `(- (or ,(nth 1 form) 1))))
    ,@(cdr (cdr form))))

;; (backward-word n) ==> (forward-word (- n))
(put 'backward-word 'byte-optimizer 'byte-optimize-backward-word)
(defun byte-optimize-backward-word (form)
  `(forward-word
    ,(typecase (nth 1 form)
       (null -1)
       (integer (- (nth 1 form)))
       (t `(- (or ,(nth 1 form) 1))))
    ,@(cdr (cdr form))))

;; The following would be a valid optimization of the above kind, but
;; the gain in performance is very small, since the saved funcall is
;; counterbalanced by the necessity of adding a bytecode for (point).
;;
;; Also, users are more likely to have modified the behavior of
;; delete-char via advice or some similar mechanism.  This is much
;; less of a problem for the previous functions because it wouldn't
;; make sense to modify the behaviour of `backward-char' without also
;; modifying `forward-char', for example.

;; (delete-char n) ==> (delete-region (point) (+ (point) n))
;; (put 'delete-char 'byte-optimizer 'byte-optimize-delete-char)
;; (defun byte-optimize-delete-char (form)
;;   (case (length (cdr form))
;;     (0 `(delete-region (point) (1+ (point))))
;;     (1 `(delete-region (point) (+ (point) ,(nth 1 form))))
;;     (t form)))

;; byte-compile-negation-optimizer lives in bytecomp.el
;(put '/= 'byte-optimizer 'byte-compile-negation-optimizer)
(put 'atom 'byte-optimizer 'byte-compile-negation-optimizer)
(put 'nlistp 'byte-optimizer 'byte-compile-negation-optimizer)

(defun byte-optimize-funcall (form)
  ;; (funcall '(lambda ...) ...) ==> ((lambda ...) ...)
  ;; (funcall 'foo ...) ==> (foo ...)
  (let ((fn (nth 1 form)))
    (if (memq (car-safe fn) '(quote function))
	(cons (nth 1 fn) (cdr (cdr form)))
	form)))

(defun byte-optimize-apply (form)
  ;; If the last arg is a literal constant, turn this into a funcall.
  ;; The funcall optimizer can then transform (funcall 'foo ...) -> (foo ...).
  (let ((fn (nth 1 form))
	(last (nth (1- (length form)) form))) ; I think this really is fastest
    (or (if (or (null last)
		(eq (car-safe last) 'quote))
	    (if (listp (nth 1 last))
		(let ((butlast (nreverse (cdr (reverse (cdr (cdr form)))))))
		  (nconc (list 'funcall fn) butlast
			 (mapcar #'(lambda (x) (list 'quote x)) (nth 1 last))))
	      (byte-compile-warn
	       "last arg to apply can't be a literal atom: %s"
	       (prin1-to-string last))
	      nil))
	form)))

(put 'funcall 'byte-optimizer 'byte-optimize-funcall)
(put 'apply   'byte-optimizer 'byte-optimize-apply)


(put 'let 'byte-optimizer 'byte-optimize-letX)
(put 'let* 'byte-optimizer 'byte-optimize-letX)
(defun byte-optimize-letX (form)
  (cond ((null (nth 1 form))
	 ;; No bindings
	 (cons 'progn (cdr (cdr form))))
	((or (nth 2 form) (nthcdr 3 form))
	 form)
	 ;; The body is nil
	((eq (car form) 'let)
	 (append '(progn) (mapcar 'car-safe (mapcar 'cdr-safe (nth 1 form)))
		 '(nil)))
	(t
	 (let ((binds (reverse (nth 1 form))))
	   (list 'let* (reverse (cdr binds)) (nth 1 (car binds)) nil)))))


(put 'nth 'byte-optimizer 'byte-optimize-nth)
(defun byte-optimize-nth (form)
  (if (and (= (safe-length form) 3) (memq (nth 1 form) '(0 1)))
      (list 'car (if (zerop (nth 1 form))
		     (nth 2 form)
		   (list 'cdr (nth 2 form))))
    (byte-optimize-predicate form)))

(put 'nthcdr 'byte-optimizer 'byte-optimize-nthcdr)
(defun byte-optimize-nthcdr (form)
  (if (and (= (safe-length form) 3) (not (memq (nth 1 form) '(0 1 2))))
      (byte-optimize-predicate form)
    (let ((count (nth 1 form)))
      (setq form (nth 2 form))
      (while (>= (setq count (1- count)) 0)
	(setq form (list 'cdr form)))
      form)))

(put 'concat 'byte-optimizer 'byte-optimize-concat)
(defun byte-optimize-concat (form)
  (let ((args (cdr form))
	(constant t))
    (while (and args constant)
      (or (byte-compile-constp (car args))
	  (setq constant nil))
      (setq args (cdr args)))
    (if constant
	(eval form)
      form)))


;;; enumerating those functions which need not be called if the returned
;;; value is not used.  That is, something like
;;;    (progn (list (something-with-side-effects) (yow))
;;;           (foo))
;;; may safely be turned into
;;;    (progn (progn (something-with-side-effects) (yow))
;;;           (foo))
;;; Further optimizations will turn (progn (list 1 2 3) 'foo) into 'foo.

;;; I wonder if I missed any :-\)
(let ((side-effect-free-fns
       '(% * + - / /= 1+ 1- < <= = > >= abs acos append aref ash asin atan
	 assoc assq
	 boundp buffer-file-name buffer-local-variables buffer-modified-p
	 buffer-substring
	 capitalize car-less-than-car car cdr ceiling concat
	 ;; coordinates-in-window-p not in XEmacs
	 copy-marker cos count-lines
	 default-boundp default-value documentation downcase
	 elt exp expt fboundp featurep
	 file-directory-p file-exists-p file-locked-p file-name-absolute-p
	 file-newer-than-file-p file-readable-p file-symlink-p file-writable-p
	 float floor format
	 get get-buffer get-buffer-window getenv get-file-buffer
	 ;; hash-table functions
	 make-hash-table copy-hash-table
	 gethash
	 hash-table-count
	 hash-table-rehash-size
	 hash-table-rehash-threshold
	 hash-table-size
	 hash-table-test
	 hash-table-type
	 ;;
	 int-to-string
	 length log log10 logand logb logior lognot logxor lsh
	 marker-buffer max member memq min mod
	 next-window nth nthcdr number-to-string
	 parse-colon-path plist-get previous-window
	 radians-to-degrees rassq regexp-quote reverse round
	 sin sqrt string< string= string-equal string-lessp string-to-char
	 string-to-int string-to-number substring symbol-plist
	 tan upcase user-variable-p vconcat
	 ;; XEmacs change: window-edges -> window-pixel-edges
	 window-buffer window-dedicated-p window-pixel-edges window-height
	 window-hscroll window-minibuffer-p window-width
	 zerop
	 ;; functions defined by cl
	 oddp evenp plusp minusp
	 abs expt signum last butlast ldiff
	 pairlis gcd lcm
	 isqrt floor* ceiling* truncate* round* mod* rem* subseq
	 list-length getf
	 ))
      (side-effect-and-error-free-fns
       '(arrayp atom
	 bobp bolp buffer-end buffer-list buffer-size buffer-string bufferp
	 car-safe case-table-p cdr-safe char-or-string-p char-table-p
	 characterp commandp cons
	 consolep console-live-p consp
	 current-buffer
	 ;; XEmacs: extent functions, frame-live-p, various other stuff
	 devicep device-live-p
	 dot dot-marker eobp eolp eq eql equal eventp extentp
	 extent-live-p floatp framep frame-live-p
	 get-largest-window get-lru-window
	 hash-table-p
	 identity ignore integerp integer-or-marker-p interactive-p
	 invocation-directory invocation-name
	 keymapp list listp
	 make-marker mark mark-marker markerp memory-limit minibuffer-window
	 ;; mouse-movement-p not in XEmacs
	 natnump nlistp not null number-or-marker-p numberp
	 one-window-p ;; overlayp not in XEmacs
	 point point-marker point-min point-max processp
	 range-table-p
	 selected-window sequencep stringp subrp symbolp syntax-table-p
	 user-full-name user-login-name user-original-login-name
	 user-real-login-name user-real-uid user-uid
	 vector vectorp
	 window-configuration-p window-live-p windowp
	 ;; Functions defined by cl
	 eql floatp-safe list* subst acons equalp random-state-p
	 copy-tree sublis
	 )))
  (dolist (fn side-effect-free-fns)
    (put fn 'side-effect-free t))
  (dolist (fn side-effect-and-error-free-fns)
    (put fn 'side-effect-free 'error-free)))


(defun byte-compile-splice-in-already-compiled-code (form)
  ;; form is (byte-code "..." [...] n)
  (if (not (memq byte-optimize '(t byte)))
      (byte-compile-normal-call form)
    (byte-inline-lapcode
     (byte-decompile-bytecode-1 (nth 1 form) (nth 2 form) t))
    (setq byte-compile-maxdepth (max (+ byte-compile-depth (nth 3 form))
				     byte-compile-maxdepth))
    (setq byte-compile-depth (1+ byte-compile-depth))))

(put 'byte-code 'byte-compile 'byte-compile-splice-in-already-compiled-code)


(defconst byte-constref-ops
  '(byte-constant byte-constant2 byte-varref byte-varset byte-varbind))

;;; This function extracts the bitfields from variable-length opcodes.
;;; Originally defined in disass.el (which no longer uses it.)

(defun disassemble-offset ()
  "Don't call this!"
  ;; fetch and return the offset for the current opcode.
  ;; return NIL if this opcode has no offset
  ;; OP, PTR and BYTES are used and set dynamically
  (declare (special op ptr bytes))
  (cond ((< op byte-nth)
	 (let ((tem (logand op 7)))
	   (setq op (logand op 248))
	   (cond ((eq tem 6)
		  (setq ptr (1+ ptr))	;offset in next byte
		  ;; char-to-int to avoid downstream problems
		  ;; caused by chars appearing where ints are
		  ;; expected.  In bytecode the bytes in the
		  ;; opcode string are always interpreted as ints.
		  (char-to-int (aref bytes ptr)))
		 ((eq tem 7)
		  (setq ptr (1+ ptr))	;offset in next 2 bytes
		  (+ (aref bytes ptr)
		     (progn (setq ptr (1+ ptr))
			    (lsh (aref bytes ptr) 8))))
		 (t tem))))		;offset was in opcode
	((>= op byte-constant)
	 (prog1 (- op byte-constant)	;offset in opcode
	   (setq op byte-constant)))
	((and (>= op byte-constant2)
	      (<= op byte-goto-if-not-nil-else-pop))
	 (setq ptr (1+ ptr))		;offset in next 2 bytes
	 (+ (aref bytes ptr)
	    (progn (setq ptr (1+ ptr))
		   (lsh (aref bytes ptr) 8))))
	;; XEmacs: this code was here before.  FSF's first comparison
	;; is (>= op byte-listN).  It appears that the rel-goto stuff
	;; does not exist in FSF 19.30.  It doesn't exist in 19.28
	;; either, so I'm going to assume that this is an improvement
	;; on our part and leave it in. --ben
	((and (>= op byte-rel-goto)
	      (<= op byte-insertN))
	 (setq ptr (1+ ptr))		;offset in next byte
	 ;; Use char-to-int to avoid downstream problems caused by
	 ;; chars appearing where ints are expected.  In bytecode
	 ;; the bytes in the opcode string are always interpreted as
	 ;; ints.
	 (char-to-int (aref bytes ptr)))))


;;; This de-compiler is used for inline expansion of compiled functions,
;;; and by the disassembler.
;;;
;;; This list contains numbers, which are pc values,
;;; before each instruction.
(defun byte-decompile-bytecode (bytes constvec)
  "Turns BYTECODE into lapcode, referring to CONSTVEC."
  (let ((byte-compile-constants nil)
	(byte-compile-variables nil)
	(byte-compile-tag-number 0))
    (byte-decompile-bytecode-1 bytes constvec)))

;; As byte-decompile-bytecode, but updates
;; byte-compile-{constants, variables, tag-number}.
;; If MAKE-SPLICEABLE is true, then `return' opcodes are replaced
;; with `goto's destined for the end of the code.
;; That is for use by the compiler.
;; If MAKE-SPLICEABLE is nil, we are being called for the disassembler.
;; In that case, we put a pc value into the list
;; before each insn (or its label).
(defun byte-decompile-bytecode-1 (bytes constvec &optional make-spliceable)
  (let ((length (length bytes))
	(ptr 0) optr tags op offset
	;; tag unused
	lap tmp
	endtag
	;; (retcount 0) unused
	)
    (while (not (= ptr length))
      (or make-spliceable
	  (setq lap (cons ptr lap)))
      (setq op (aref bytes ptr)
	    optr ptr
	    offset (disassemble-offset)) ; this does dynamic-scope magic
      (setq op (aref byte-code-vector op))
      ;; XEmacs: the next line in FSF 19.30 reads
      ;; (cond ((memq op byte-goto-ops)
      ;; see the comment above about byte-rel-goto in XEmacs.
      (cond ((or (memq op byte-goto-ops)
		 (cond ((memq op byte-rel-goto-ops)
			(setq op (aref byte-code-vector
				       (- (symbol-value op)
					  (- byte-rel-goto byte-goto))))
			(setq offset (+ ptr (- offset 127)))
			t)))
	     ;; it's a pc
	     (setq offset
		   (cdr (or (assq offset tags)
			    (car (setq tags
				       (cons (cons offset
						   (byte-compile-make-tag))
					     tags)))))))
	    ((cond ((eq op 'byte-constant2) (setq op 'byte-constant) t)
		   ((memq op byte-constref-ops)))
	     (setq tmp (if (>= offset (length constvec))
			   (list 'out-of-range offset)
			 (aref constvec offset))
		   offset (if (eq op 'byte-constant)
			      (byte-compile-get-constant tmp)
			    (or (assq tmp byte-compile-variables)
				(car (setq byte-compile-variables
					   (cons (list tmp)
						 byte-compile-variables)))))))
	    ((and make-spliceable
		  (eq op 'byte-return))
	     (if (= ptr (1- length))
		 (setq op nil)
	       (setq offset (or endtag (setq endtag (byte-compile-make-tag)))
		     op 'byte-goto))))
      ;; lap = ( [ (pc . (op . arg)) ]* )
      (setq lap (cons (cons optr (cons op (or offset 0)))
		      lap))
      (setq ptr (1+ ptr)))
    ;; take off the dummy nil op that we replaced a trailing "return" with.
    (let ((rest lap))
      (while rest
	(cond ((numberp (car rest)))
	      ((setq tmp (assq (car (car rest)) tags))
	       ;; this addr is jumped to
	       (setcdr rest (cons (cons nil (cdr tmp))
				  (cdr rest)))
	       (setq tags (delq tmp tags))
	       (setq rest (cdr rest))))
	(setq rest (cdr rest))))
    (if tags (error "optimizer error: missed tags %s" tags))
    (if (null (car (cdr (car lap))))
	(setq lap (cdr lap)))
    (if endtag
	(setq lap (cons (cons nil endtag) lap)))
    ;; remove addrs, lap = ( [ (op . arg) | (TAG tagno) ]* )
    (mapcar #'(lambda (elt) (if (numberp elt) elt (cdr elt)))
	    (nreverse lap))))


;;; peephole optimizer

(defconst byte-tagref-ops (cons 'TAG byte-goto-ops))

(defconst byte-conditional-ops
  '(byte-goto-if-nil byte-goto-if-not-nil byte-goto-if-nil-else-pop
    byte-goto-if-not-nil-else-pop))

(defconst byte-after-unbind-ops
   '(byte-constant byte-dup
     byte-symbolp byte-consp byte-stringp byte-listp byte-numberp byte-integerp
     byte-eq byte-not
     byte-cons byte-list1 byte-list2	; byte-list3 byte-list4
     byte-interactive-p)
   ;; How about other side-effect-free-ops?  Is it safe to move an
   ;; error invocation (such as from nth) out of an unwind-protect?
   ;; No, it is not, because the unwind-protect forms can alter
   ;; the inside of the object to which nth would apply.
   ;; For the same reason, byte-equal was deleted from this list.
   "Byte-codes that can be moved past an unbind.")

(defconst byte-compile-side-effect-and-error-free-ops
  '(byte-constant byte-dup byte-symbolp byte-consp byte-stringp byte-listp
    byte-integerp byte-numberp byte-eq byte-equal byte-not byte-car-safe
    byte-cdr-safe byte-cons byte-list1 byte-list2 byte-point byte-point-max
    byte-point-min byte-following-char byte-preceding-char
    byte-current-column byte-eolp byte-eobp byte-bolp byte-bobp
    byte-current-buffer byte-interactive-p))

(defconst byte-compile-side-effect-free-ops
  (nconc
   '(byte-varref byte-nth byte-memq byte-car byte-cdr byte-length byte-aref
     byte-symbol-value byte-get byte-concat2 byte-concat3 byte-sub1 byte-add1
     byte-eqlsign byte-gtr byte-lss byte-leq byte-geq byte-diff byte-negate
     byte-plus byte-max byte-min byte-mult byte-char-after byte-char-syntax
     byte-buffer-substring byte-string= byte-string< byte-nthcdr byte-elt
     byte-member byte-assq byte-quo byte-rem)
   byte-compile-side-effect-and-error-free-ops))

;;; This piece of shit is because of the way DEFVAR_BOOL() variables work.
;;; Consider the code
;;;
;;;	(defun foo (flag)
;;;	  (let ((old-pop-ups pop-up-windows)
;;;		(pop-up-windows flag))
;;;	    (cond ((not (eq pop-up-windows old-pop-ups))
;;;		   (setq old-pop-ups pop-up-windows)
;;;		   ...))))
;;;
;;; Uncompiled, old-pop-ups will always be set to nil or t, even if FLAG is
;;; something else.  But if we optimize
;;;
;;;	varref flag
;;;	varbind pop-up-windows
;;;	varref pop-up-windows
;;;	not
;;; to
;;;	varref flag
;;;	dup
;;;	varbind pop-up-windows
;;;	not
;;;
;;; we break the program, because it will appear that pop-up-windows and
;;; old-pop-ups are not EQ when really they are.  So we have to know what
;;; the BOOL variables are, and not perform this optimization on them.
;;;

;;; This used to hold a large list of boolean variables, which had to
;;; be updated every time a new DEFVAR_BOOL is added, making it very
;;; hard to maintain.  Such a list is not necessary under XEmacs,
;;; where we can use `built-in-variable-type' to query for boolean
;;; variables.

;(defconst byte-boolean-vars
;   ...)

(defun byte-optimize-lapcode (lap &optional for-effect)
  "Simple peephole optimizer.  LAP is both modified and returned."
  (let (lap0
	lap1
	lap2
	variable-frequency
	(keep-going 'first-time)
	(add-depth 0)
	rest tmp tmp2 tmp3
	(side-effect-free (if byte-compile-delete-errors
			      byte-compile-side-effect-free-ops
			    byte-compile-side-effect-and-error-free-ops)))
    (while keep-going
      (or (eq keep-going 'first-time)
	  (byte-compile-log-lap "  ---- next pass"))
      (setq rest lap
	    keep-going nil)
      (while rest
	(setq lap0 (car rest)
	      lap1 (nth 1 rest)
	      lap2 (nth 2 rest))

	;; You may notice that sequences like "dup varset discard" are
	;; optimized but sequences like "dup varset TAG1: discard" are not.
	;; You may be tempted to change this; resist that temptation.
	(cond ;;
	      ;; <side-effect-free> pop -->  <deleted>
	      ;;  ...including:
	      ;; const-X pop   -->  <deleted>
	      ;; varref-X pop  -->  <deleted>
	      ;; dup pop       -->  <deleted>
	      ;;
	      ((and (eq 'byte-discard (car lap1))
		    (memq (car lap0) side-effect-free))
	       (setq keep-going t)
	       (setq tmp (aref byte-stack+-info (symbol-value (car lap0))))
	       (setq rest (cdr rest))
	       (cond ((= tmp 1)
		      (byte-compile-log-lap
		       "  %s discard\t-->\t<deleted>" lap0)
		      (setq lap (delq lap0 (delq lap1 lap))))
		     ((= tmp 0)
		      (byte-compile-log-lap
		       "  %s discard\t-->\t<deleted> discard" lap0)
		      (setq lap (delq lap0 lap)))
		     ((= tmp -1)
		      (byte-compile-log-lap
		       "  %s discard\t-->\tdiscard discard" lap0)
		      (setcar lap0 'byte-discard)
		      (setcdr lap0 0))
		     ((error "Optimizer error: too much on the stack"))))
	      ;;
	      ;; goto*-X X:  -->  X:
	      ;;
	      ((and (memq (car lap0) byte-goto-ops)
		    (eq (cdr lap0) lap1))
	       (cond ((eq (car lap0) 'byte-goto)
		      (setq lap (delq lap0 lap))
		      (setq tmp "<deleted>"))
		     ((memq (car lap0) byte-goto-always-pop-ops)
		      (setcar lap0 (setq tmp 'byte-discard))
		      (setcdr lap0 0))
		     ((error "Depth conflict at tag %d" (nth 2 lap0))))
	       (and (memq byte-optimize-log '(t byte))
		    (byte-compile-log "  (goto %s) %s:\t-->\t%s %s:"
				      (nth 1 lap1) (nth 1 lap1)
				      tmp (nth 1 lap1)))
	       (setq keep-going t))
	      ;;
	      ;; varset-X varref-X  -->  dup varset-X
	      ;; varbind-X varref-X  -->  dup varbind-X
	      ;; const/dup varset-X varref-X --> const/dup varset-X const/dup
	      ;; const/dup varbind-X varref-X --> const/dup varbind-X const/dup
	      ;; The latter two can enable other optimizations.
	      ;;
	      ((and (eq 'byte-varref (car lap2))
		    (eq (cdr lap1) (cdr lap2))
		    (memq (car lap1) '(byte-varset byte-varbind)))
	       (if (and (setq tmp (eq (built-in-variable-type (car (cdr lap2)))
				      'boolean))
			(not (eq (car lap0) 'byte-constant)))
		   nil
		 (setq keep-going t)
		 (if (memq (car lap0) '(byte-constant byte-dup))
		     (progn
		       (setq tmp (if (or (not tmp)
					 (memq (car (cdr lap0)) '(nil t)))
				     (cdr lap0)
				   (byte-compile-get-constant t)))
		       (byte-compile-log-lap "  %s %s %s\t-->\t%s %s %s"
					     lap0 lap1 lap2 lap0 lap1
					     (cons (car lap0) tmp))
		       (setcar lap2 (car lap0))
		       (setcdr lap2 tmp))
		   (byte-compile-log-lap "  %s %s\t-->\tdup %s" lap1 lap2 lap1)
		   (setcar lap2 (car lap1))
		   (setcar lap1 'byte-dup)
		   (setcdr lap1 0)
		   ;; The stack depth gets locally increased, so we will
		   ;; increase maxdepth in case depth = maxdepth here.
		   ;; This can cause the third argument to byte-code to
		   ;; be larger than necessary.
		   (setq add-depth 1))))
	      ;;
	      ;; dup varset-X discard  -->  varset-X
	      ;; dup varbind-X discard  -->  varbind-X
	      ;; (the varbind variant can emerge from other optimizations)
	      ;;
	      ((and (eq 'byte-dup (car lap0))
		    (eq 'byte-discard (car lap2))
		    (memq (car lap1) '(byte-varset byte-varbind)))
	       (byte-compile-log-lap "  dup %s discard\t-->\t%s" lap1 lap1)
	       (setq keep-going t
		     rest (cdr rest))
	       (setq lap (delq lap0 (delq lap2 lap))))
	      ;;
	      ;; not goto-X-if-nil              -->  goto-X-if-non-nil
	      ;; not goto-X-if-non-nil          -->  goto-X-if-nil
	      ;;
	      ;; it is wrong to do the same thing for the -else-pop variants.
	      ;;
	      ((and (eq 'byte-not (car lap0))
		    (or (eq 'byte-goto-if-nil (car lap1))
			(eq 'byte-goto-if-not-nil (car lap1))))
	       (byte-compile-log-lap "  not %s\t-->\t%s"
				     lap1
				     (cons
				      (if (eq (car lap1) 'byte-goto-if-nil)
					  'byte-goto-if-not-nil
					'byte-goto-if-nil)
				      (cdr lap1)))
	       (setcar lap1 (if (eq (car lap1) 'byte-goto-if-nil)
				'byte-goto-if-not-nil
				'byte-goto-if-nil))
	       (setq lap (delq lap0 lap))
	       (setq keep-going t))
	      ;;
	      ;; goto-X-if-nil     goto-Y X:  -->  goto-Y-if-non-nil X:
	      ;; goto-X-if-non-nil goto-Y X:  -->  goto-Y-if-nil     X:
	      ;;
	      ;; it is wrong to do the same thing for the -else-pop variants.
	      ;;
	      ((and (or (eq 'byte-goto-if-nil (car lap0))
			(eq 'byte-goto-if-not-nil (car lap0)))	; gotoX
		    (eq 'byte-goto (car lap1))			; gotoY
		    (eq (cdr lap0) lap2))			; TAG X
	       (let ((inverse (if (eq 'byte-goto-if-nil (car lap0))
				  'byte-goto-if-not-nil 'byte-goto-if-nil)))
		 (byte-compile-log-lap "  %s %s %s:\t-->\t%s %s:"
				       lap0 lap1 lap2
				       (cons inverse (cdr lap1)) lap2)
		 (setq lap (delq lap0 lap))
		 (setcar lap1 inverse)
		 (setq keep-going t)))
	      ;;
	      ;; const goto-if-* --> whatever
	      ;;
	      ((and (eq 'byte-constant (car lap0))
		    (memq (car lap1) byte-conditional-ops))
	       (cond ((if (or (eq (car lap1) 'byte-goto-if-nil)
			      (eq (car lap1) 'byte-goto-if-nil-else-pop))
			  (car (cdr lap0))
			(not (car (cdr lap0))))
		      (byte-compile-log-lap "  %s %s\t-->\t<deleted>"
					    lap0 lap1)
		      (setq rest (cdr rest)
			    lap (delq lap0 (delq lap1 lap))))
		     (t
		      (if (memq (car lap1) byte-goto-always-pop-ops)
			  (progn
			    (byte-compile-log-lap "  %s %s\t-->\t%s"
			     lap0 lap1 (cons 'byte-goto (cdr lap1)))
			    (setq lap (delq lap0 lap)))
			(byte-compile-log-lap "  %s %s\t-->\t%s" lap0 lap1
			 (cons 'byte-goto (cdr lap1))))
		      (setcar lap1 'byte-goto)))
	       (setq keep-going t))
	      ;;
	      ;; varref-X varref-X  -->  varref-X dup
	      ;; varref-X [dup ...] varref-X  -->  varref-X [dup ...] dup
	      ;; We don't optimize the const-X variations on this here,
	      ;; because that would inhibit some goto optimizations; we
	      ;; optimize the const-X case after all other optimizations.
	      ;;
	      ((and (eq 'byte-varref (car lap0))
		    (progn
		      (setq tmp (cdr rest))
		      (while (eq (car (car tmp)) 'byte-dup)
			(setq tmp (cdr tmp)))
		      t)
		    (eq (cdr lap0) (cdr (car tmp)))
		    (eq 'byte-varref (car (car tmp))))
	       (if (memq byte-optimize-log '(t byte))
		   (let ((str ""))
		     (setq tmp2 (cdr rest))
		     (while (not (eq tmp tmp2))
		       (setq tmp2 (cdr tmp2)
			     str (concat str " dup")))
		     (byte-compile-log-lap "  %s%s %s\t-->\t%s%s dup"
					   lap0 str lap0 lap0 str)))
	       (setq keep-going t)
	       (setcar (car tmp) 'byte-dup)
	       (setcdr (car tmp) 0)
	       (setq rest tmp))
	      ;;
	      ;; TAG1: TAG2: --> TAG1: <deleted>
	      ;; (and other references to TAG2 are replaced with TAG1)
	      ;;
	      ((and (eq (car lap0) 'TAG)
		    (eq (car lap1) 'TAG))
	       (and (memq byte-optimize-log '(t byte))
		    (byte-compile-log "  adjacent tags %d and %d merged"
				      (nth 1 lap1) (nth 1 lap0)))
	       (setq tmp3 lap)
	       (while (setq tmp2 (rassq lap0 tmp3))
		 (setcdr tmp2 lap1)
		 (setq tmp3 (cdr (memq tmp2 tmp3))))
	       (setq lap (delq lap0 lap)
		     keep-going t))
	      ;;
	      ;; unused-TAG: --> <deleted>
	      ;;
	      ((and (eq 'TAG (car lap0))
		    (not (rassq lap0 lap)))
	       (and (memq byte-optimize-log '(t byte))
		    (byte-compile-log "  unused tag %d removed" (nth 1 lap0)))
	       (setq lap (delq lap0 lap)
		     keep-going t))
	      ;;
	      ;; goto   ... --> goto   <delete until TAG or end>
	      ;; return ... --> return <delete until TAG or end>
	      ;;
	      ((and (memq (car lap0) '(byte-goto byte-return))
		    (not (memq (car lap1) '(TAG nil))))
	       (setq tmp rest)
	       (let ((i 0)
		     (opt-p (memq byte-optimize-log '(t lap)))
		     str deleted)
		 (while (and (setq tmp (cdr tmp))
			     (not (eq 'TAG (car (car tmp)))))
		   (if opt-p (setq deleted (cons (car tmp) deleted)
				   str (concat str " %s")
				   i (1+ i))))
		 (if opt-p
		     (let ((tagstr
			    (if (eq 'TAG (car (car tmp)))
				(format "%d:" (car (cdr (car tmp))))
			      (or (car tmp) ""))))
		       (if (< i 6)
			   (apply 'byte-compile-log-lap-1
				  (concat "  %s" str
					  " %s\t-->\t%s <deleted> %s")
				  lap0
				  (nconc (nreverse deleted)
					 (list tagstr lap0 tagstr)))
			 (byte-compile-log-lap
			  "  %s <%d unreachable op%s> %s\t-->\t%s <deleted> %s"
			  lap0 i (if (= i 1) "" "s")
			  tagstr lap0 tagstr))))
		 (rplacd rest tmp))
	       (setq keep-going t))
	      ;;
	      ;; <safe-op> unbind --> unbind <safe-op>
	      ;; (this may enable other optimizations.)
	      ;;
	      ((and (eq 'byte-unbind (car lap1))
		    (memq (car lap0) byte-after-unbind-ops))
	       (byte-compile-log-lap "  %s %s\t-->\t%s %s" lap0 lap1 lap1 lap0)
	       (setcar rest lap1)
	       (setcar (cdr rest) lap0)
	       (setq keep-going t))
	      ;;
	      ;; varbind-X unbind-N         -->  discard unbind-(N-1)
	      ;; save-excursion unbind-N    -->  unbind-(N-1)
	      ;; save-restriction unbind-N  -->  unbind-(N-1)
	      ;;
	      ((and (eq 'byte-unbind (car lap1))
		    (memq (car lap0) '(byte-varbind byte-save-excursion
				       byte-save-restriction))
		    (< 0 (cdr lap1)))
	       (if (zerop (setcdr lap1 (1- (cdr lap1))))
		   (delq lap1 rest))
	       (if (eq (car lap0) 'byte-varbind)
		   (setcar rest (cons 'byte-discard 0))
		 (setq lap (delq lap0 lap)))
	       (byte-compile-log-lap "  %s %s\t-->\t%s %s"
		 lap0 (cons (car lap1) (1+ (cdr lap1)))
		 (if (eq (car lap0) 'byte-varbind)
		     (car rest)
		   (car (cdr rest)))
		 (if (and (/= 0 (cdr lap1))
			  (eq (car lap0) 'byte-varbind))
		     (car (cdr rest))
		   ""))
	       (setq keep-going t))
	      ;;
	      ;; goto*-X ... X: goto-Y  --> goto*-Y
	      ;; goto-X ...  X: return  --> return
	      ;;
	      ((and (memq (car lap0) byte-goto-ops)
		    (memq (car (setq tmp (nth 1 (memq (cdr lap0) lap))))
			  '(byte-goto byte-return)))
	       (cond ((and (not (eq tmp lap0))
			   (or (eq (car lap0) 'byte-goto)
			       (eq (car tmp) 'byte-goto)))
		      (byte-compile-log-lap "  %s [%s]\t-->\t%s"
					    (car lap0) tmp tmp)
		      (if (eq (car tmp) 'byte-return)
			  (setcar lap0 'byte-return))
		      (setcdr lap0 (cdr tmp))
		      (setq keep-going t))))
	      ;;
	      ;; goto-*-else-pop X ... X: goto-if-* --> whatever
	      ;; goto-*-else-pop X ... X: discard --> whatever
	      ;;
	      ((and (memq (car lap0) '(byte-goto-if-nil-else-pop
				       byte-goto-if-not-nil-else-pop))
		    (memq (car (car (setq tmp (cdr (memq (cdr lap0) lap)))))
			  (eval-when-compile
			   (cons 'byte-discard byte-conditional-ops)))
		    (not (eq lap0 (car tmp))))
	       (setq tmp2 (car tmp))
	       (setq tmp3 (assq (car lap0) '((byte-goto-if-nil-else-pop
					      byte-goto-if-nil)
					     (byte-goto-if-not-nil-else-pop
					      byte-goto-if-not-nil))))
	       (if (memq (car tmp2) tmp3)
		   (progn (setcar lap0 (car tmp2))
			  (setcdr lap0 (cdr tmp2))
			  (byte-compile-log-lap "  %s-else-pop [%s]\t-->\t%s"
						(car lap0) tmp2 lap0))
		 ;; Get rid of the -else-pop's and jump one step further.
		 (or (eq 'TAG (car (nth 1 tmp)))
		     (setcdr tmp (cons (byte-compile-make-tag)
				       (cdr tmp))))
		 (byte-compile-log-lap "  %s [%s]\t-->\t%s <skip>"
				       (car lap0) tmp2 (nth 1 tmp3))
		 (setcar lap0 (nth 1 tmp3))
		 (setcdr lap0 (nth 1 tmp)))
	       (setq keep-going t))
	      ;;
	      ;; const goto-X ... X: goto-if-* --> whatever
	      ;; const goto-X ... X: discard   --> whatever
	      ;;
	      ((and (eq (car lap0) 'byte-constant)
		    (eq (car lap1) 'byte-goto)
		    (memq (car (car (setq tmp (cdr (memq (cdr lap1) lap)))))
			  (eval-when-compile
			    (cons 'byte-discard byte-conditional-ops)))
		    (not (eq lap1 (car tmp))))
	       (setq tmp2 (car tmp))
	       (cond ((memq (car tmp2)
			    (if (null (car (cdr lap0)))
				'(byte-goto-if-nil byte-goto-if-nil-else-pop)
			      '(byte-goto-if-not-nil
				byte-goto-if-not-nil-else-pop)))
		      (byte-compile-log-lap "  %s goto [%s]\t-->\t%s %s"
					    lap0 tmp2 lap0 tmp2)
		      (setcar lap1 (car tmp2))
		      (setcdr lap1 (cdr tmp2))
		      ;; Let next step fix the (const,goto-if*) sequence.
		      (setq rest (cons nil rest)))
		     (t
		      ;; Jump one step further
		      (byte-compile-log-lap
		       "  %s goto [%s]\t-->\t<deleted> goto <skip>"
		       lap0 tmp2)
		      (or (eq 'TAG (car (nth 1 tmp)))
			  (setcdr tmp (cons (byte-compile-make-tag)
					    (cdr tmp))))
		      (setcdr lap1 (car (cdr tmp)))
		      (setq lap (delq lap0 lap))))
	       (setq keep-going t))
	      ;;
	      ;; X: varref-Y    ...     varset-Y goto-X  -->
	      ;; X: varref-Y Z: ... dup varset-Y goto-Z
	      ;; (varset-X goto-BACK, BACK: varref-X --> copy the varref down.)
	      ;; (This is so usual for while loops that it is worth handling).
	      ;;
	      ((and (eq (car lap1) 'byte-varset)
		    (eq (car lap2) 'byte-goto)
		    (not (memq (cdr lap2) rest)) ;Backwards jump
		    (eq (car (car (setq tmp (cdr (memq (cdr lap2) lap)))))
			'byte-varref)
		    (eq (cdr (car tmp)) (cdr lap1))
		    (not (eq (built-in-variable-type (car (cdr lap1)))
			     'boolean)))
	       ;;(byte-compile-log-lap "  Pulled %s to end of loop" (car tmp))
	       (let ((newtag (byte-compile-make-tag)))
		 (byte-compile-log-lap
		  "  %s: %s ... %s %s\t-->\t%s: %s %s: ... %s %s %s"
		  (nth 1 (cdr lap2)) (car tmp)
		  lap1 lap2
		  (nth 1 (cdr lap2)) (car tmp)
		  (nth 1 newtag) 'byte-dup lap1
		  (cons 'byte-goto newtag)
		  )
		 (setcdr rest (cons (cons 'byte-dup 0) (cdr rest)))
		 (setcdr tmp (cons (setcdr lap2 newtag) (cdr tmp))))
	       (setq add-depth 1)
	       (setq keep-going t))
	      ;;
	      ;; goto-X Y: ... X: goto-if*-Y  -->  goto-if-not-*-X+1 Y:
	      ;; (This can pull the loop test to the end of the loop)
	      ;;
	      ((and (eq (car lap0) 'byte-goto)
		    (eq (car lap1) 'TAG)
		    (eq lap1
			(cdr (car (setq tmp (cdr (memq (cdr lap0) lap))))))
		    (memq (car (car tmp))
			  '(byte-goto byte-goto-if-nil byte-goto-if-not-nil
				      byte-goto-if-nil-else-pop)))
;;	       (byte-compile-log-lap "  %s %s, %s %s  --> moved conditional"
;;				     lap0 lap1 (cdr lap0) (car tmp))
	       (let ((newtag (byte-compile-make-tag)))
		 (byte-compile-log-lap
		  "%s %s: ... %s: %s\t-->\t%s ... %s:"
		  lap0 (nth 1 lap1) (nth 1 (cdr lap0)) (car tmp)
		  (cons (cdr (assq (car (car tmp))
				   '((byte-goto-if-nil . byte-goto-if-not-nil)
				     (byte-goto-if-not-nil . byte-goto-if-nil)
				     (byte-goto-if-nil-else-pop .
				      byte-goto-if-not-nil-else-pop)
				     (byte-goto-if-not-nil-else-pop .
				      byte-goto-if-nil-else-pop))))
			newtag)

		  (nth 1 newtag)
		  )
		 (setcdr tmp (cons (setcdr lap0 newtag) (cdr tmp)))
		 (if (eq (car (car tmp)) 'byte-goto-if-nil-else-pop)
		     ;; We can handle this case but not the -if-not-nil case,
		     ;; because we won't know which non-nil constant to push.
		   (setcdr rest (cons (cons 'byte-constant
					    (byte-compile-get-constant nil))
				      (cdr rest))))
	       (setcar lap0 (nth 1 (memq (car (car tmp))
					 '(byte-goto-if-nil-else-pop
					   byte-goto-if-not-nil
					   byte-goto-if-nil
					   byte-goto-if-not-nil
					   byte-goto byte-goto))))
	       )
	       (setq keep-going t))
	      )
	(setq rest (cdr rest)))
      )
    ;; Cleanup stage:
    ;; Rebuild byte-compile-constants / byte-compile-variables.
    ;; Simple optimizations that would inhibit other optimizations if they
    ;; were done in the optimizing loop, and optimizations which there is no
    ;; need to do more than once.
    (setq byte-compile-constants nil
	  byte-compile-variables nil
	  variable-frequency (make-hash-table :test 'eq))
    (setq rest lap)
    (while rest
      (setq lap0 (car rest)
	    lap1 (nth 1 rest))
      (if (memq (car lap0) byte-constref-ops)
	  (if (not (eq (car lap0) 'byte-constant))
	      (progn
		(incf (gethash (cdr lap0) variable-frequency 0))
		(or (memq (cdr lap0) byte-compile-variables)
		    (setq byte-compile-variables
			  (cons (cdr lap0) byte-compile-variables))))
	    (or (memq (cdr lap0) byte-compile-constants)
		(setq byte-compile-constants (cons (cdr lap0)
						   byte-compile-constants)))))
      (cond (;;
	     ;; const-C varset-X  const-C  -->  const-C dup varset-X
	     ;; const-C varbind-X const-C  -->  const-C dup varbind-X
	     ;;
	     (and (eq (car lap0) 'byte-constant)
		  (eq (car (nth 2 rest)) 'byte-constant)
		  (eq (cdr lap0) (cdr (nth 2 rest)))
		  (memq (car lap1) '(byte-varbind byte-varset)))
	     (byte-compile-log-lap "  %s %s %s\t-->\t%s dup %s"
				   lap0 lap1 lap0 lap0 lap1)
	     (setcar (cdr (cdr rest)) (cons (car lap1) (cdr lap1)))
	     (setcar (cdr rest) (cons 'byte-dup 0))
	     (setq add-depth 1))
	    ;;
	    ;; const-X  [dup/const-X ...]   -->  const-X  [dup ...] dup
	    ;; varref-X [dup/varref-X ...]  -->  varref-X [dup ...] dup
	    ;;
	    ((memq (car lap0) '(byte-constant byte-varref))
	     (setq tmp rest
		   tmp2 nil)
	     (while (progn
		      (while (eq 'byte-dup (car (car (setq tmp (cdr tmp))))))
		      (and (eq (cdr lap0) (cdr (car tmp)))
			   (eq (car lap0) (car (car tmp)))))
	       (setcar tmp (cons 'byte-dup 0))
	       (setq tmp2 t))
	     (if tmp2
		 (byte-compile-log-lap
		  "  %s [dup/%s]...\t-->\t%s dup..." lap0 lap0 lap0)))
	    ;;
	    ;; unbind-N unbind-M  -->  unbind-(N+M)
	    ;;
	    ((and (eq 'byte-unbind (car lap0))
		  (eq 'byte-unbind (car lap1)))
	     (byte-compile-log-lap "  %s %s\t-->\t%s" lap0 lap1
				   (cons 'byte-unbind
					 (+ (cdr lap0) (cdr lap1))))
	     (setq keep-going t)
	     (setq lap (delq lap0 lap))
	     (setcdr lap1 (+ (cdr lap1) (cdr lap0))))
	    )
      (setq rest (cdr rest)))
    ;; Since the first 6 entries of the compiled-function constants
    ;; vector are most efficient for varref/set/bind ops, we sort by
    ;; reference count.  This generates maximally space efficient and
    ;; pretty time-efficient byte-code.  See `byte-compile-constants-vector'.
    (setq byte-compile-variables
	  (sort byte-compile-variables
		#'(lambda (v1 v2)
		    (< (gethash v1 variable-frequency)
		       (gethash v2 variable-frequency)))))
    ;; Another hack - put the most used variable in position 6, for
    ;; better locality of reference with adjoining constants.
    (let ((tail (last byte-compile-variables 6)))
      (setq byte-compile-variables
	    (append (nbutlast byte-compile-variables 6)
		    (nreverse tail))))
    (setq byte-compile-maxdepth (+ byte-compile-maxdepth add-depth)))
  lap)

(provide 'byte-optimize)


;; To avoid "lisp nesting exceeds max-lisp-eval-depth" when this file compiles
;; itself, compile some of its most used recursive functions (at load time).
;;
(eval-when-compile
 (or (compiled-function-p (symbol-function 'byte-optimize-form))
     (assq 'byte-code (symbol-function 'byte-optimize-form))
     (let ((byte-optimize nil)
	   (byte-compile-warnings nil))
       (mapcar
	#'(lambda (x)
	    (or noninteractive (message "compiling %s..." x))
	    (byte-compile x)
	    (or noninteractive (message "compiling %s...done" x)))
	'(byte-optimize-form
	  byte-optimize-body
	  byte-optimize-predicate
	  byte-optimize-binary-predicate
	  ;; Inserted some more than necessary, to speed it up.
	  byte-optimize-form-code-walker
	  byte-optimize-lapcode))))
 nil)

;; END SYNC WITH 20.7.

;;; byte-optimize.el ends here
