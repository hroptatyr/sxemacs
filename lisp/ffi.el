;;; ffi.el --- FFI lisp layer.

;; Copyright (C) 2005-2010 by Zajcev Evgeny

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Feb 18 18:56:43 MSK 2005
;; Keywords: lisp, ffi

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

;;; Synched up with: Not in FSF

;;; Commentary:

;;; Types:
;;
;; BASIC    ->  byte unsigned-byte .. c-string

;; ARRAY    ->  (array TYPE SIZE)

;; STRUCT   ->  (struct name (slot1 TYPE) (slot2 TYPE) .. (slotn TYPE))

;; UNION    ->  (union name (slot1 TYPE) (slot2 TYPE) .. (slotn TYPE))

;; FUNCTION -> (function RET-TYPE IN-TYPE .. IN-TYPE)

;; C:  int a[10] = {1, 2, .. 10};    LISP: [1 2 .. 10]
;;
;; C:  struct {                      LISP:  ((i 1) (c ?c))
;;         int i;
;;         char c;
;;     } bb = {1, 'c'};
;;
;;; Code:
(eval-when-compile
  (globally-declare-boundp
   '(ffi-type-checker ffi-named-types ffi-loaded-libraries))
  (globally-declare-fboundp
   '(ffi-size-of-type make-ffi-object ffi-canonicalise-type
		      ffi-basic-type-p ffi-load-library ffi-dlerror
		      ffi-object-type ffi-fetch ffi-slot-offset
		      ffi-store ffi-aref ffi-make-pointer
		      ffi-object-address ffi-call-function
		      ffi-defun ffi-bind)))


(require 'alist)

(setq ffi-type-checker #'ffi-type-p)

(defun ffi-create-fo (type val)
  "Create a foreign object of TYPE and set its value to VAL.
Return created FFI object."
  (let* ((ctype (ffi-canonicalise-type type))
	 (size (cond ((or (eq ctype 'c-string) (eq ctype 'c-data))
		      (1+ (length val)))
		     ((and (consp ctype) (eq (car ctype) 'c-data)
			   (intp (cdr ctype)))
		      (cdr ctype))
		     (t
		      (ffi-size-of-type ctype))))
	 (fo (make-ffi-object type size)))
    (ffi-set fo val)
    fo))

(defun ffi-find-named-type (type-name)
  "Search for TYPE-NAME in named FFI types."
  (cdr (assq type-name ffi-named-types)))

(defmacro declare-ffi-type (name)
  `(put (quote ,name) 'declared-ffi-type t))

(defmacro ffi-declared-type-p (name)
  `(and (symbolp ,name) (get ,name 'declared-ffi-type)))
(defsetf ffi-declared-type-p (name) (val)
  `(and (symbolp ,name) (put ,name 'declared-ffi-type ,val)))

;; Null pointer
(defun ffi-null-pointer ()
  "Return null-pointer."
  (ffi-make-pointer 0))

(defun ffi-null-p (ptr)
  "Return non-nil if PTR is null pointer."
  (zerop (ffi-pointer-address ptr)))

;;; Type translators
(defvar ffi-type-to-translators nil)
(defvar ffi-type-from-translators nil)

(defmacro define-ffi-translator (translators type body)
  `(pushnew (cons ,type '(progn ,@body)) ,translators :key #'car))

(defmacro define-ffi-translator-to-foreign (type &rest body)
  "Define translator to foreign type for TYPE.
BODY should use `value' to reference typed value."
  `(define-ffi-translator ffi-type-to-translators ',type ,body))

(defmacro define-ffi-translator-from-foreign (type &rest body)
  "Define translator from foreign type for TYPE.
BODY should use `value' to reference typed value."
  `(define-ffi-translator ffi-type-from-translators ',type ,body))

(defmacro ffi-translate-foreign (value type translators)
  `(let ((translator (assq ,type ,translators)))
     (if translator
	 (eval (cdr translator))
       value)))

(defun ffi-translate-to-foreign (value type)
  (ffi-translate-foreign value type ffi-type-to-translators))

(defun ffi-translate-from-foreign (value type)
  (ffi-translate-foreign value type ffi-type-from-translators))

;;;###autoload
(defun ffi-define-type-internal (name type)
  (when (and name (ffi-find-named-type name))
    (warn "Already defined NAME" name))

  (unless (and name (ffi-find-named-type name))
    (when (ffi-declared-type-p name)
      (setf (ffi-declared-type-p name) nil))

    ;; Try to get name from union or struct
    (when (and (null name)
	       (listp type)
	       (memq (car type) '(struct union)))
      (setq name (cadr type)))

    (setq ffi-named-types
	  (put-alist name type ffi-named-types))

    ;; Copy translators, if any
    (let ((fft (assq type ffi-type-to-translators))
	  (tft (assq type ffi-type-from-translators)))
      (when fft
	(pushnew (cons name (cdr fft)) ffi-type-to-translators :key #'car))
      (when tft
	(pushnew (cons name (cdr tft)) ffi-type-from-translators :key #'car)))

    name))

(defmacro define-ffi-type (name type)
  "Associate NAME with FFI TYPE.
When defining global structures or unions, NAME may be
nil, in that case NAME is derived from the name of
TYPE."
  `(ffi-define-type-internal ',name ',type))

(defmacro define-ffi-struct (name &rest slots)
  "Define a new structure of NAME and SLOTS.
SLOTS are in form (NAME TYPE &key :offset)."
  (let ((forms `(progn
		  (define-ffi-type ,name (struct ,name ,@slots)))))
    (loop for sn in slots
      do (setq sn (car sn))
      do (let ((sym (intern (format "%S->%S" name sn))))
	   (setq forms (append forms
			       `((defun ,sym (obj)
				   (ffi-fetch obj (ffi-slot-offset ',name ',sn)
					      (ffi-slot-type ',name ',sn))))))
	   (setq forms
		 (append forms
			 `((defsetf ,sym (obj) (nv)
			     (list 'ffi-store obj
				   (list 'ffi-slot-offset '',name '',sn)
				   (list 'ffi-slot-type '',name '',sn)
				   nv)))))))
    forms))

;;;###autoload
(defun ffi-type-p (type &optional signal-p)
  "Return non-nil if TYPE is a valid FFI type.
If optional argument SIGNAL-P is non-nil and TYPE is not an
FFI type, additionally signal an error.

NOTE: returned non-nil value is actuall canonicalised type."
  (setq type (ffi-canonicalise-type type))
  (if (cond ((ffi-basic-type-p type) type)

	    ;; Pointer
	    ((or (eq type 'pointer)
		 (and (listp type)
		      (eq (car type) 'pointer)
		      (ffi-type-p (cadr type)))) type)

	    ;; Maybe TYPE is declared
	    ((ffi-declared-type-p type) type)

	    ;; Struct or Union
	    ((and (listp type)
		  (memq (car type) '(struct union)))
	     type)
;             (not (memq nil
;                        (mapcar #'(lambda (slot-type)
;                                    (ffi-type-p (cadr slot-type)))
;                                (cddr type)))))

	    ;; Complex c-data
	    ((and (consp type) (eq (car type) 'c-data)
		  (or (numberp (cdr type)) (null (cdr type))))
	     type)

	    ;; Array
	    ((and (listp type) (eq 'array (car type))
		  (ffi-type-p (cadr type))
		  (integerp (caddr type))
		  (> (caddr type) 0))
	     type)

	    ;; Function
	    ((and (listp type) (eq 'function (car type))
		  (ffi-type-p (cadr type)))
	     (not (memq nil (mapcar 'ffi-type-p (cddr type))))))
      type                              ; TYPE is valid FFI type

    (when signal-p
      (signal-error 'invalid-argument type))))

;;;###autoload
(defun ffi-load (libname)
  "Load library LIBNAME.
Return a foreign object handle if successful, or indicate an error if
the library cannot be loaded.

The argument LIBNAME should be the file-name string of a shared
object library (usual extension is `.so').

The library should reside in one of the directories specified by the
$LD_LIBRARY_PATH environment variable or the more global ld.so.cache."
  (let ((fo (ffi-load-library libname)))
    (unless fo
      (error "Can't load library `%s': %s" libname (ffi-dlerror)))

    (setq ffi-loaded-libraries
	  (put-alist libname fo ffi-loaded-libraries))
    fo))

(defun* ffi-get (fo &key (type (ffi-object-type fo)) (off 0)
		    (from-call nil))
  "Return FO's value.
Optional key :TYPE may be used to cast FO to the specified
type, it defaults to the object's assigned type.
Optional key :OFF may be used to specify an offset, it
defaults to 0.
FROM-CALL is magic, do not use it!"
  (let ((ctype (ffi-canonicalise-type type)))
    (cond ((ffi-basic-type-p ctype)
	   (ffi-fetch fo off type))
	  ;; Arrays
	  ((and (listp ctype)
		(eq (car ctype) 'array))
	   (vconcat
	    (loop for idx from 0 below (third ctype)
	      collect (ffi-get
		       fo :type (second ctype)
		       :off (+ off (* idx (ffi-size-of-type
					   (second ctype))))))))

	  ;; Structures
	  ((and (listp ctype)
		(eq (car ctype) 'struct))
	   (loop for sslot in (cddr ctype)
	     collect (list (first sslot)
			   (ffi-get
			    fo :type (second sslot)
			    :off (+ off (ffi-slot-offset
					 ctype (first sslot)))))))

	  ;; Extremely special case for safe-string!
	  ((eq type 'safe-string)
	   (unless (ffi-null-p fo)
	     (ffi-fetch fo off 'c-string)))

	  ((and (not from-call)
		(or (eq ctype 'pointer)
		    (and (listp ctype)
			 (eq (car ctype) 'pointer)
			 (ffi-type-p (cadr ctype)))))
	   (if (ffi-null-p fo)
	       nil
	     (ffi-fetch fo off type)))

	  (t
	   ;; Can't get value in proper form,
	   ;; just return FO unmodified
	   fo))))

(defun ffi-slot-type (type slot)
  "Return TYPE's SLOT type.
TYPE must be of structure or union type."
  (let ((ctype (ffi-canonicalise-type type)))
    (unless (memq (car ctype) '(struct union))
      (error "Not struct or union: %S" type))
    (or (cadr (find slot (cddr ctype) :key #'car :test #'eq))
	(error "No such slot: %S" slot))))

(defun ffi-slot (fo slot)
  "Setf-able slot accessor.
Return FO's SLOT value.  Can be used in conjunction with `setf' to set
FO's SLOT."
  ;; Note: `ffi-slot-offset' checks for struct or union type.
  (let ((soff (ffi-slot-offset (ffi-object-type fo) slot)))
    (ffi-fetch fo soff (ffi-slot-type (ffi-object-type fo) slot))))

(defsetf ffi-slot (fo slot) (val)
  `(let ((soff (ffi-slot-offset (ffi-object-type ,fo) ,slot)))
    (ffi-store ,fo soff (ffi-slot-type (ffi-object-type ,fo) ,slot) ,val)))

(defun ffi-set (fo val)
  "Set FO's foreign value to VAL."
  (let* ((type (ffi-object-type fo))
	 (ctype (ffi-canonicalise-type type)))
    (if (or (ffi-basic-type-p ctype)
	    (eq ctype 'pointer))
	(ffi-store fo 0 type val)

      ;; Pointer type, same as for basic
      (when (or (eq ctype 'pointer)
		(and (listp ctype) (eq (car ctype) 'pointer)))
	(ffi-store fo 0 type val))

      ;; TODO: Compound type
      )))

;; Dereferencing a pointer is done with aref (lg told me), however I
;; find it misleading, so ...
(defun ffi-deref (fo-pointer)
  "Return the data FO-POINTER points to.
This is the equivalent of the `*' operator in C.
Error will be signaled if FO-POINTER is not of pointer type."
  (ffi-aref fo-pointer 0))

(defmacro define-ffi-function (fsym args doc-string ftype ename)
  "Define ffi function visible from Emacs lisp as FSYM."
  `(defun ,fsym ,args ,doc-string
    (ffi-get (ffi-call-function (load-time-value (ffi-defun ,ftype ,ename))
                                ,@(mapcar* #'(lambda (type arg)
                                               `(if (ffi-object-p ,arg)
                                                    ,arg
                                                  (ffi-create-fo ',type ,arg)))
                                           (cddadr ftype) args))

             :from-call t)))
(put 'define-ffi-function 'lisp-indent-function 'defun)


;;; helpers

(defmacro ffi-enum (name &optional docstring &rest specs)
  "Define an enumeration NAME.
Optional argument DOCSTRING is a documentation string.

SPECS can be an arbitrary number of symbols which will be enumerated in
the respective order.

Additionally the cells of SPECS may look like

  foo = bar

to adhere a symbol `foo' to the enumeration with the value of the
symbol `bar' \(i.e. `foo' is an alias of `bar'\).

Moreover, it is possible to set the counter explicitly:

  baz = 5

would assign a value of 5 to the symbol `baz' and \(by side-effect\)
set the counter to 6 for the next symbol.

The defined enumeration will result in a \(defconst'd\) variable `NAME',
the value is an alist of the form \(\(symbol . value\) ...\), where
`value' is the C-value of `symbol'.

Furthermore, two functions \(named `NAME' and `NAME'-value\) will be
defined.  The first one is a simple lookup function returning the
C-value of a passed symbol.  The second does basically the same
but returns the representing \(elisp\) integer of a symbol.
Both functions return nil if the symbol is not in the enumeration."

  ;; first check if we were passed a docstring
  (unless (stringp docstring)
    ;; docstring is missing, the value of docstring already
    ;; contains the first symbol, hence we pump that one to specs
    (unless (null docstring)
      (setq specs (cons docstring specs)))
    (setq docstring (format "Enumeration `%s'" name)))

  ;; now build that pig of code
  (list 'prog1
	;; define the constant `name'
	(list 'defconst name nil
	      docstring)
	;; fill in the values
	(list 'setq name
	      (cons 'list
		    (let ((tmpspecs specs)
			  (i 0)
			  (delayed (dllist))
			  (result (dllist)))
		      (while (car tmpspecs)
			(if (eq (cadr tmpspecs) '=)
			    ;; this is the alias case
			    ;; we append a cons (left-of-= . right-of-=)
			    ;; to the dllist `delayed'
			    ;; if `right-of-=' (i.e. the caddr) is an integer
			    ;; we set the counter `i' to that value on go on
			    ;; from there
			    (let ((leftof (car tmpspecs))
				  (rightof (caddr tmpspecs)))

			      ;; pop off the cruft
			      (setq tmpspecs (nthcdr 3 tmpspecs))

			      (cond ((intp rightof)
				     ;; reset the counter
				     (setq i rightof)
				     ;; prepend leftof again
				     (setq tmpspecs
					   (cons leftof tmpspecs)))
				    (t
				     ;; push the stuff to the delayed list
				     (dllist-append
				      delayed (cons leftof rightof)))))

			  ;; ordinary case
			  (dllist-append result (cons (car tmpspecs) i))
			  (setq i (1+ i))
			  (setq tmpspecs (cdr tmpspecs))))

		      ;; convert `result' to alist
		      ;; this is necessary here, since we need the alist
		      ;; property right now to look up the delayed symbols
		      (setq result (dllist-to-list result))

		      ;; process those delayed thingies
		      ;; these are basically conses (alias . resolved-symbol)
		      ;; we lookup `resolved-symbol' in the alist `result'
		      ;; first and assign (alias . value-of-resolved-symbol)
		      ;; if that fails, we look at the cars of the delayed
		      ;; list if we can find `resolved-symbol' there
		      ;; if so, we re-append the whole cell to the delayed list
		      ;; if not, we try to find a huge horsewhip to treat
		      ;; the user to a little surprise :)
		      (while (dllist-car delayed)
			(let ((alias (dllist-pop-car delayed)))
			  (let ((val (cdr-safe (assoc (cdr alias) result))))
			    (if (null val)
				;; prevent infinite loops when the user
				;; is too stupid to give us a valid alias
				(when (let ((presentp))
					(mapc-internal
					 #'(lambda (item)
					     (and (eq (cdr alias) (car item))
						  (setq presentp t)))
					 delayed)
					presentp)
				    (dllist-append delayed alias))
			      (setq result
				    (cons (cons (car alias) val)
					  result))))))

		      ;; return `result'
		      (mapcar
		       #'(lambda (rescell)
			   (list 'cons
				 (list 'quote (car rescell))
				 (list
				  'let
				  (list (list 'ffival
					      (list 'ffi-create-fo
						    ''unsigned-int
						    (cdr rescell))))
				  (list 'put 'ffival ''value (cdr rescell))
				  'ffival)))
		       result))))

	;; define the lookup function
	(list 'defun name '(symbol)
	      (format "Lookup the value of SYMBOL in the enumeration `%s'."
		      name)
	      (list 'cdr-safe
		    (list 'assq 'symbol
			  name)))

	;; define the lookup function for the elisp value
	(list 'defun (intern (format "%s-value" name)) '(symbol)
	      (format (concat "Lookup the elisp value (an integer) of SYMBOL "
			      "in the enumeration `%s'.")
		      name)
	      (list 'get (list name 'symbol) ''value))))
(put 'ffi-enum 'lisp-indent-function 'defun)
;;; example
;; (ffi-enum example-enum
;;   ;;"Enum of control commands."
;;   test_thing = symbol_1
;;   symbol_0
;;   symbol_1
;;   foobbbbar = test_thing
;;   snaabar = foobbbbar
;;   go-on-with5 = 5
;;   guesswhat)
;; (example-enum-value 'guesswhat)

(defmacro define-ffi-enum (type-name &rest spec)
  "Create enumarate type which you can pass to C functions.
For example:
  \(define-ffi-enum MyEnum
    \(Boris 0\)
    Vova
    Micha\)"
  `(progn
     (define-ffi-type ,type-name int)
     (let* ((cv 0)
	    (fev (mapcar #'(lambda (sv)
			     (prog1
				 (if (and (listp sv)
					  (symbolp (car sv))
					  (numberp (cadr sv)))
				     (prog1
					 (cons (car sv) (cadr sv))
				       (setq cv (cadr sv)))
				   (cons sv cv))
			       (incf cv)))
			 '(,@spec))))
       (put ',type-name 'ffi-enum-values fev))
     ;; Translators
     (define-ffi-translator-to-foreign ,type-name
       (or (cdr (assq value (get ',type-name 'ffi-enum-values)))
	   0))
     (define-ffi-translator-from-foreign ,type-name
       (or (car (find-if #'(lambda (v)
			     (= (cdr v) value))
			 (get ',type-name 'ffi-enum-values)))
	   'undefined-enum-value))))

(defun ffi-enum-values (enum-type)
  "Return alist for ENUM-TYPE.
Where car is symbol and cdr is the numeric value for it."
  (get enum-type 'ffi-enum-values))

;;; Callbacks
(defmacro define-ffi-callback (sym retype args &rest body)
  "Create new callback to be called from C.
Return foreign object that you can pass as callback to some C
function.

For example:

\(define-ffi-callback my-nice-cb int
  \(\(proname c-string\) \(event pointer\)\)
  \"Print nice message in the minibuffer and return 10000.\"
  \(message \"nice message\"\)
  10000\)

To get foreign object for this callback function use `ffi-callback-fo'
and pass the name of the callback."
  (let ((argnames (mapcar #'first args))
	(argtypes (mapcar #'second args)))
  `(progn
     (defun ,sym ,argnames
       ,@body)
     (let ((cfo (ffi-make-callback ',sym ',retype ',argtypes 0)))
       (put ',sym 'ffi-callback-fo cfo)
       cfo))))

(put 'define-ffi-callback 'lisp-indent-function 'defun)

(defmacro ffi-callback-fo (sym)
  "Return SYM callback's foreign object."
  `(get ,sym 'ffi-callback-fo))


;; Define some types
(define-ffi-type boolean int)
(define-ffi-translator-to-foreign boolean
  (if value 1 0))
(define-ffi-translator-from-foreign boolean
  (not (zerop value)))

(define-ffi-type lisp-object pointer)
(define-ffi-translator-to-foreign lisp-object
  (ffi-lisp-object-to-pointer value))
(define-ffi-translator-from-foreign lisp-object
  (ffi-pointer-to-lisp-object value))

;; NOTE: use only for return values
(define-ffi-type safe-string pointer)

(define-ffi-type callback pointer)
(define-ffi-translator-to-foreign callback
  (ffi-callback-fo value))


;;; CFFI
(define-ffi-type :byte byte)
(define-ffi-type :unsigned-byte unsigned-byte)
(define-ffi-type :char char)
(define-ffi-type :unsigned-char unsigned-char)
(define-ffi-type :uchar unsigned-char)
(define-ffi-type :short short)
(define-ffi-type :unsigned-short unsigned-short)
(define-ffi-type :ushort unsigned-short)
(define-ffi-type :int int)
(define-ffi-type :unsigned-int unsigned-int)
(define-ffi-type :uint unsigned-int)
(define-ffi-type :long long)
(define-ffi-type :unsigned-long unsigned-long)
(define-ffi-type :ulong unsigned-long)
(define-ffi-type :float float)
(define-ffi-type :double double)
(define-ffi-type :void void)
(define-ffi-type :pointer pointer)
(define-ffi-type :boolean boolean)
(define-ffi-type :string c-string)

;;;# Accessing Foreign Globals

(defun cffi:lisp-var-name (name &optional fun-p)
  "Return the Lisp symbol for foreign var NAME."
  (etypecase name
    (list (second name))
    (string (intern (format "%s%s%s" (if fun-p "" "*")
			    (downcase (substitute ?- ?_ name)) (if fun-p "" "*"))))
    (symbol name)))

(defun cffi:foreign-var-name (name)
  "Return the foreign var name of NAME."
  (etypecase name
    (list (first name))
    (string name)
    (symbol (let ((dname (downcase (symbol-name name))))
	      (replace-in-string (substitute ?_ ?- dname) "\\*" "")))))

(defun cffi:get-var-pointer (symbol)
  "Return a pointer to the foreign global variable relative to SYMBOL."
  (cffi:foreign-symbol-pointer (get symbol 'foreign-var-name)))

(defmacro cffi:defcstruct (name &rest args)
  `(define-ffi-struct ,name ,@args))

(defmacro cffi:defcvar (name type)
  (let ((ns (cffi:lisp-var-name name)))
    `(defvar ,ns (ffi-bind ,type (cffi:foreign-var-name ',name)))))

(defmacro cffi:defcfun (name ret-type &optional docstring &rest in-args)
  ;; First check if we were passed a docstring
  (unless (stringp docstring)
    ;; docstring is missing, the value of docstring already contains
    ;; the first argument, hence we pump that one to in-args
    (unless (null docstring)
      (setq in-args (cons docstring in-args)))
    (setq docstring
	  (format "Lisp variant for `%s' foreign function."
		  (cffi:foreign-var-name name))))

  (let* ((nsl (cffi:lisp-var-name name t))
	 (nsf (cffi:foreign-var-name name))
	 (with-rest (when (eq (car (last in-args)) '&rest)
		      (setq in-args (butlast in-args))
		      t))
	 (as (mapcar 'first in-args))
	 (at (mapcar 'second in-args))
	 (flet-form) (defun-form nil))
    (setq flet-form
	  (append (list `(mapcar* #'setarg ',at (list ,@as)))
		  (when with-rest
		    (list '(while rest-args
			     (if (ffi-object-p (car rest-args))
				 (progn
				   (setarg (ffi-object-type (car rest-args))
					   (car rest-args))
				   (setq rest-args (cdr rest-args)))
			     (setarg (car rest-args) (cadr rest-args))
			     (setq rest-args (cddr rest-args))))))
		  (list '(setq ffiargs (nreverse ffiargs)))
		  (list `(setq ret (apply #'ffi-call-function
					  (get ',nsl 'ffi-fun) ffiargs)))
		  (list `(ffi-get ret :from-call t))))
    (setq defun-form
	  (append `(defun ,nsl)
		  (list (if with-rest
			    (append as '(&rest rest-args))
			  as))
		  (list docstring)
		  (list (append
			 '(let (ffiargs ret))
			 (list (append
				'(flet ((setarg (type arg)
					  (setq ffiargs
						(cons
						 (if (ffi-object-p arg)
						     arg
						   (ffi-create-fo type arg))
						 ffiargs)))))
				flet-form))
			 ))))
    (append '(progn) (list defun-form)
	    (list `(put ',nsl 'ffi-fun
			(ffi-defun '(function ,ret-type ,@at) ,nsf))))))

(put 'cffi:defcfun 'lisp-indent-function 'defun)

(defun ffi:canonicalize-symbol-name-case (name)
  (upcase name))

;;;# Allocation

(defun cffi:foreign-alloc (type)
  "Return pointer."
  (ffi-call-function
   (ffi-defun '(function pointer unsigned-int) "malloc")
   (ffi-create-fo 'unsigned-int (ffi-size-of-type type))))

(defun cffi:foreign-free (ptr)
  "Frees a PTR previously allocated with `cffi:foreign-alloc'."
  (ffi-call-function
   (ffi-defun '(function void pointer) "free")
   ptr))

(defmacro cffi:with-foreign-pointer (spec &rest body)
  "Bind VAR to SIZE bytes of foreign memory during BODY.  The
pointer in VAR is invalid beyond the dynamic extent of BODY, and
may be stack-allocated if supported by the implementation.  If
SIZE-VAR is supplied, it will be bound to SIZE during BODY."
  (let ((var (car spec))
	(size (cadr spec))
	(size-var (caddr spec)))
    (unless size-var
      (setf size-var (gensym "SIZE")))
    `(let* ((,size-var ,size)
	    (,var (cffi:foreign-alloc ,size-var)))
       (unwind-protect
	   (progn ,@body)
	 (cffi:foreign-free ,var)))))

;;;# Misc. Pointer Operations

(defun ffi-pointer-p (fo)
  "Return non-nil if ffi objct FO has pointer type."
  (let ((ctype (ffi-canonicalise-type (ffi-object-type fo))))
    (or (eq ctype 'pointer)
	(and (listp ctype)
	     (eq (car ctype) 'pointer)
	     (ffi-type-p (cadr ctype))))))

(defalias 'cffi:make-pointer 'ffi-make-pointer)
(defalias 'ffi-pointer-address 'ffi-object-address)
(defalias 'cffi:pointer-address 'ffi-pointer-address)
(defalias 'cffi:pointerp 'ffi-pointer-p)
(defalias 'cffi:null-pointer 'ffi-null-pointer)
(defalias 'cffi:null-pointer-p 'ffi-null-p)

(defun cffi:inc-pointer (ptr offset)
  "Return a pointer OFFSET bytes past PTR."
  (ffi-make-pointer (+ (ffi-object-address ptr) offset)))

(defun cffi:pointer-eq (ptr1 ptr2)
  "Return true if PTR1 and PTR2 point to the same address."
  (= (ffi-object-address ptr1) (ffi-object-address ptr2)))

;;;# Dereferencing

(defun* cffi:mem-ref (ptr type &optional (offset 0))
  "Dereference an object of TYPE at OFFSET bytes from PTR."
  (ffi-fetch ptr offset type))

; (defun cffi:%mem-set (value ptr type &optional (offset 0))
;   "Set an object of TYPE at OFFSET bytes from PTR."
;   (let* ((type (convert-foreign-type type))
;          (type-size (ffi:size-of-foreign-type type)))
;     (si:foreign-data-set-elt
;      (si:foreign-data-recast ptr (+ offset type-size) :void)
;      offset type value)))

;;;# Type Operations

(defalias 'cffi:foreign-type-size 'ffi-size-of-type)
(defalias 'cffi:foreign-type-alignment 'ffi-type-alignment)

(defalias 'cffi:load-foreign-library 'ffi-load)

;;;# Callbacks
(defmacro cffi:%defcallback (name rettype arg-names arg-types &rest body)
  `(define-ffi-callback ,name ,rettype ,(mapcar* #'list arg-names arg-types)
     ,@body))

(put 'cffi:%defcallback 'lisp-indent-function 'defun)

(defmacro cffi:%callback (name)
  `(ffi-callback-fo ,name))

;;;# Foreign Globals

(defun cffi:foreign-symbol-pointer (name)
  "Returns a pointer to a foreign symbol NAME."
  (ffi-bind 'pointer name))

;;;# Finalizers

(defun cffi:finalize (object function)
  (error "SXEmacs FFI does not support finalizers."))

(defun cffi:cancel-finalization (object)
  (error "SXEmacs FFI does not support finalizers."))

(provide 'ffi)

;;; ffi.el ends here
