;;; ffi.el --- FFI lisp layer.

;; Copyright (C) 2005 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Feb 18 18:56:43 MSK 2005
;; Keywords: lisp, ffi

;; This file is part of SXEmacs.

;; SXEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; SXEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SXEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

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


(require 'alist)

(setq ffi-type-checker #'ffi-type-p)

(defun ffi-create-fo (type val)
  "Create a foreign object of TYPE and set its value to VAL.
Return created FFI object."
  (let* ((size (cond ((or (eq type 'c-string)
                          (eq type 'c-data))
                      (1+ (length val)))
                     ((and (consp type)
                           (eq (car type) 'c-data)
                           (intp (cdr type)))
                      (cdr type))
                     (t
                      (ffi-size-of-type type))))
         (fo (make-ffi-object type size)))
    (ffi-set fo val)
    fo))

(defun ffi-find-named-type (type-name)
  "Search for named FFI type."
  (cdr (assq type-name ffi-named-types)))

(defmacro declare-ffi-type (name)
  `(put (quote ,name) 'declared-ffi-type t))

(defmacro ffi-declared-type-p (name)
  `(and (symbolp ,name) (get ,name 'declared-ffi-type)))
(defsetf ffi-declared-type-p (name) (val)
  `(and (symbolp ,name) (put ,name 'declared-ffi-type ,val)))

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
    name))

(defmacro define-ffi-type (name type)
  "Associate NAME with FFI TYPE.
When defining global structures or unions, NAME may be
`nil', in that case NAME is derived from the name of
TYPE."
  `(ffi-define-type-internal ',name ',type))

(defmacro define-ffi-struct (name &rest slots)
  "Define a new structure of NAME and SLOTS."
  (let ((forms `(progn
                  (define-ffi-type ,name (struct ,name ,@slots)))))
    (loop for sn in slots
      do (setq sn (car sn))
      do (let ((sym (intern (format "%S->%S" name sn))))
           (setq forms (append forms
                               `((defun ,sym (obj)
                                   (ffi-fetch obj (ffi-slot-offset ',name ',sn)
                                              (ffi-slot-type ',name ',sn))))))
           (setq forms (append forms
                               `((defsetf ,sym (obj) (nv)
                                   (list 'ffi-store obj (list 'ffi-slot-offset '',name '',sn)
                                               (list 'ffi-slot-type '',name '',sn)
                                               nv)))))))
    forms))

;;;###autoload
(defun ffi-type-p (type &optional signal-p)
  "Return non-nil if TYPE is a valid FFI type.
If optional argument SIGNAL-P is non-nil and TYPE is not an
FFI type, additionally signal an error."
  (setq type (ffi-canonicalise-type type))
  (if (cond ((ffi-basic-type-p type) t)

            ;; Maybe TYPE is declared
            ((ffi-declared-type-p type) t)

            ;; Struct or Union
            ((and (listp type)
                  (memq (car type) '(struct union)))
             t)
;             (not (memq nil
;                        (mapcar #'(lambda (slot-type)
;                                    (ffi-type-p (cadr slot-type)))
;                                (cddr type)))))

            ;; Complex c-data
            ((and (consp type) (eq (car type) 'c-data)
                  (or (numberp (cdr type)) (null (cdr type))))
             t)

            ;; Array
            ((and (listp type) (eq 'array (car type))
                  (ffi-type-p (cadr type))
                  (integerp (caddr type))
                  (> (caddr type) 0))
             t)

            ;; Pointer
            ((and (listp type) (eq 'pointer (car type))
                  (ffi-type-p (cadr type))))

            ;; Function
            ((and (listp type) (eq 'function (car type))
                  (ffi-type-p (cadr type)))
             (not (memq nil (mapcar 'ffi-type-p (cddr type))))))
      t                                 ; TYPE is valid FFI type

    (when signal-p
      (signal-error 'invalid-argument type))))

;;;###autoload
(defun ffi-load (libname)
  "Load library LIBNAME and return a foreign object handle if successful,
or indicate an error if the library cannot be loaded.

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

(defun* ffi-get (fo &key (type (ffi-object-type fo)) (off 0))
  "Return FO's value.

Optional key :TYPE may be used to cast FO to the specified
type, it defaults to the object's assigned type.
Optional key :OFF may be used to specify an offset, it
defaults to 0."
  (setq type (ffi-canonicalise-type type))
  (if (ffi-basic-type-p type)
      (ffi-fetch fo off type)
    (cond ((and (listp type)
                (eq (car type) 'array))
           (vconcat
            (loop for idx from 0 below (third type)
              collect (ffi-get fo :type (second type)
                               :off (+ off (* idx (ffi-size-of-type (second type))))))))
          ((and (listp type)
                (eq (car type) 'struct))
           (loop for sslot in (cddr type)
             collect (list (first sslot)
                           (ffi-get fo :type (second sslot)
                                    :off (+ off (ffi-slot-offset type (first sslot)))))))

          ((and (listp type)
                (eq (car type) 'pointer))
           (ffi-fetch fo off type))

          (t (error "Unsupported type: %S" type)))))

(defun ffi-slot-type (type slot)
  "Return TYPE's SLOT type.
TYPE must be of structure or union type."
  (setq type (ffi-canonicalise-type type))
  (unless (memq (car type) '(struct union))
    (error "Not struct or union: %S" type))
  (loop for sl in (cddr type)
    if (eq (car sl) slot) return (cadr sl)))

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
  (let ((ft (ffi-canonicalise-type (ffi-object-type fo))))
    (if (ffi-basic-type-p ft)
        (ffi-store fo 0 ft val)

      ;; TODO: Compound type
      )))

;; Dereferencing a pointer is done with aref (lg told me), however I
;; find it misleading, so ...
(defun ffi-deref (fo-pointer)
  "Return the data FO-POINTER points to.

This is the equivalent of the `*' operator in C."
  ;; hm, (ffi-pointer-p ...) would be nice
  (ffi-aref fo-pointer 0))


(defmacro define-ffi-function (fsym args doc-string ftype ename)
  "Define ffi function visible from Emacs lisp as FSYM."
  `(progn
     (declare (special ,fsym))
     (setq ,fsym (ffi-defun ,ftype ,ename))
     (defun ,fsym ,args
       ,doc-string
       (let ((ffiargs nil)
             (ret nil))
         (mapcar* #'(lambda (type arg)
                      (setq ffiargs (cons
                                     (if (ffi-object-p arg)
                                         arg
                                       (ffi-create-fo type arg))
                                     ffiargs)))
                  (cddr ,ftype) (list ,@args))
         (setq ffiargs (nreverse ffiargs))
         (setq ret (apply #'ffi-call-function ,fsym ffiargs))
         (if (ffi-basic-type-p (ffi-canonicalise-type (cadr ,ftype)))
             (ffi-get ret)
           ret)))))

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
Both functions return `nil' if the symbol is not in the enumeration."

  ;; first check if we were passed a docstring
  (unless (stringp docstring)
    ;; docstring is missing, the value of docstring already
    ;; contains the first symbol, hence we pump that one to specs
    (setq specs (cons docstring specs))
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


;;; cffi
(define-ffi-type :byte byte)
(define-ffi-type :unsigned-byte unsigned-byte)
(define-ffi-type :char char)
(define-ffi-type :unsigned-char unsigned-char)
(define-ffi-type :short short)
(define-ffi-type :unsigned-short unsigned-short)
(define-ffi-type :int int)
(define-ffi-type :unsigned-int unsigned-int)
(define-ffi-type :long long)
(define-ffi-type :unsigned-long unsigned-long)
(define-ffi-type :float float)
(define-ffi-type :double double)
(define-ffi-type :void void)
(define-ffi-type :pointer pointer)

(defmacro cffi:defcstruct (name &rest args)
  `(define-ffi-struct ,name ,@args))

(defmacro cffi:defcfun (name ret-type &rest in-args)
  (let* ((ns (intern name))
         (as (mapcar 'first in-args))
         (at (mapcar 'second in-args)))
    `(define-ffi-function ,ns (,@as)
       ""
       '(function ,ret-type ,@at)
       ,name)))


;;; TODO: - UFFI



(provide 'ffi)

;;; ffi.el ends here
