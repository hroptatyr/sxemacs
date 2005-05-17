;;; ffi.el --- FFI lisp layer.

;; Copyright (C) 2005 by XWEM Org.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Fri Feb 18 18:56:43 MSK 2005
;; Keywords: lisp, ffi

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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

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
;;         char c
;;     }; bb = {1, 'c'};
;; 
;;; Code:


(setq ffi-type-checker 'ffi-type-p)

(defconst ffi-basic-types
  '(byte unsigned-byte char unsigned-char
         short unsigned-short int unsigned-int
         long unsigned-long float double void
         c-string)
  "List of basic FFI types.")

(defun ffi-create-fo (type val)
  "Create foreign object of TYPE and set its value to VAL.
Return created FFI object."
  (let ((fo (make-ffi-object type)))
    (ffi-set fo val)
    fo))

(defun ffi-type-basic-p (type)
  "Return non-nil if TYPE is basic FFI type."
  (memq type ffi-basic-types))

(defun ffi-find-named-type (type-name)
  "Search for named FFI type."
  (cdr (assq type-name ffi-named-types)))

;;;###autoload
(defun ffi-define-type-internal (name type)
  (when (and name (ffi-find-named-type name))
    (error "Already defined NAME" name))
  
  ;; Try to get name from union or struct
  (when (and (null name)
             (listp type)
             (memq (car type) '(struct union)))
    (setq name (cadr type)))

  (setq ffi-named-types
        (cons (cons name type) ffi-named-types)))

;;;###autoload
(defmacro ffi-define-type (name type)
  "Associate NAME with ffi TYPE.  When defining global structures or
unions, NAME may be `nil', in that case NAME is taken from type's
name."
  `(ffi-define-type-internal (quote ,name) (quote ,type)))

(defun ffi-fixup-type (type)
  "Fixate ffi TYPE."
  (if (or (ffi-type-basic-p type)
          (not (symbolp type)))
      type
    (ffi-find-named-type type)))

;;;###autoload
(defun ffi-type-p (type &optional signal-p)
  "Return non-nil if TYPE is valid FFI type.
If optional SIGNAL-P is non-nil and TYPE is not FFI type - signal an
error."
  (setq type (ffi-fixup-type type))
  (if (cond ((ffi-type-basic-p type) t)

            ;; Struct or Union
            ((and (listp type)
                  (memq (car type) '(struct union)))
             (not (memq nil
                        (mapcar (lambda (slot-type)
                                  (ffi-type-p (cadr slot-type)))
                                (cddr type)))))

            ;; Array
            ((and (listp type) (eq 'array (car type))
                  (ffi-type-p (cadr type)) (numberp (caddr type)))
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
(defun ffi-load (lib-name)
  "Load library LIB-NAME."
  (let ((fo (ffi-load-library lib-name)))
    (unless fo
      (error "Can't load library" lib-name))
    
    (setq ffi-loaded-libraries
          (cons (cons lib-name fo) ffi-loaded-libraries))
    fo))

(defun ffi-get (fo)
  "Return FO's value."
  (if (ffi-type-basic-p (ffi-object-type fo))
      (ffi-fetch fo 0 (ffi-object-type fo))

    ;; TODO: Compound type
    ))

(defun ffi-set (fo val)
  "Set FO's foreign value to VAL."
  (if (ffi-type-basic-p (ffi-object-type fo))
      (ffi-store fo 0 (ffi-object-type fo) val)
    
    ;; TODO: Compound type
    ))

(defun ffi-set-foarg (type value)
  "Setup now fo argument."
  (let ((fo (if (ffi-type-basic-p type)
                (make-ffi-object type)
              (ffi-alloc type (ffi-size-of-type type)))))
    (ffi-set fo value)
    fo))

(defmacro define-foreign-function (funame args doc-string type
                                          &optional fun-sym)
  "Define new foreign function that accepts arguments."
  (progn
    (ffi-type-p type)                   ; check TYPE
    (let ((ret (cadr type))             ; ret TYPE
          (in-types (cddr type))
          (fsym (gensym "ffi-function")))
      `(when (setq ,fsym (ffi-defun
                          ,type (or ,fun-sym
                                   (replace-in-string
                                    (symbol-name (quote ,funame)) "-" "_"))))
         (defun ,funame ,args
           ,doc-string
           (let ((rv (apply 'ffi-call-function (quote ,fsym)
                            (mapcar* 'ffi-create-fo (cddr ,type) ,args))))
             (cond ((eq 'void (cadr ,type)) rv)
                   (t (ffi-get rv)))))))))


(provide 'ffi)

;;; ffi.el ends here
