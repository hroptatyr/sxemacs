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


(setq ffi-type-checker #'ffi-type-p)

(defun ffi-create-fo (type val)
  "Create a foreign object of TYPE and set its value to VAL.
Return created FFI object."
  (let* ((size (if (eq type 'c-string)
                   (1+ (length val))
                 (ffi-size-of-type type)))
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
  (setq type (ffi-fixup-type type))
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
      (error "cannot load library" libname))
    
    (setq ffi-loaded-libraries
          (put-alist libname fo ffi-loaded-libraries))
    fo))

(defun* ffi-get (fo &key (type (ffi-object-type fo)) (off 0))
  "Return FO's value.

Optional key :TYPE may be used to cast FO to the specified
type, it defaults to the object's assigned type.
Optional key :OFF may be used to specify an offset, it 
defaults to 0."
  (setq type (ffi-fixup-type type))
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

          (t (error "Unsupported type" type)))))

(defun ffi-slot-type (type slot)
  "Return TYPE's SLOT type."
  (setq type (ffi-fixup-type type))
  (unless (memq (car type) '(struct union))
    (error "Not struct or union"))
  (loop for sl in (cddr type)
    if (eq (car sl) slot) return (cadr sl)))
  
(defun ffi-slot (fo slot)
  "Return FO's SLOT value."
  ;; Note: `ffi-slot-offset' checks for struct or union type.
  (let ((soff (ffi-slot-offset (ffi-object-type fo) slot)))
    (ffi-fetch fo soff (ffi-slot-type (ffi-object-type fo) slot))))

(defun ffi-slot-set (fo slot val)
  (let ((soff (ffi-slot-offset (ffi-object-type fo) slot)))
    (ffi-store fo soff (ffi-slot-type (ffi-object-type fo) slot) val)))

(defsetf ffi-slot (fo slot) (val)
  `(ffi-slot-set ,fo ,slot ,val))

(defun ffi-set (fo val)
  "Set FO's foreign value to VAL."
  (let ((ft (ffi-fixup-type (ffi-object-type fo))))
    (if (ffi-basic-type-p ft)
        (ffi-store fo 0 ft val)
    
      ;; TODO: Compound type
      )))

;; dereferencing a pointer is done with aref (lg told me), however I
;; find it misleading, so ...
(defun ffi-deref (fo-pointer)
  "Return the data FO-POINTER points to.

This is the equivalent of the `*' operator in C."
  ;; hm, (ffi-pointer-p ...) would be nice
  (ffi-aref pointer 0))


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
         (if (ffi-basic-type-p (ffi-fixup-type (cadr ,ftype)))
             (ffi-get ret)
           ret)))))

(put 'define-ffi-function 'lisp-indent-function 'defun)


;;; CFFI
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
