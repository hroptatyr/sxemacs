;;; gtk-ffi.el --- Foreign function interface for the GTK object system

;; Copyright (C) 2000 Free Software Foundation

;; Maintainer: William Perry <wmperry@gnu.org>
;; Keywords: extensions, dumped

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

;; This file is dumped with XEmacs.

(defvar gtk-type-aliases '((GtkType . guint)
			   (GdkAtom . gulong)
			   (GdkBitmap . GdkWindow)
			   (time_t    . guint)
			   (none      . void)
			   (GdkDrawable . GdkWindow)
			   (GdkBitmap . GdkWindow)
			   (GdkPixmap . GdkWindow))
  "An assoc list of aliases for commonly used GTK types that are not
really part of the object system.")

(defvar gtk-ffi-debug nil
  "If non-nil, all functions defined wiht `gtk-import-function' will be checked
for missing marshallers.")

(defun gtk-ffi-check-function (func)
  ;; We don't call gtk-main or gtk-main-quit because it thoroughly
  ;; hoses us (locks up xemacs handling events, but no lisp).
  (if (not (memq func '(gtk-main gtk-main-quit)))
      (condition-case err
	  (funcall func)
	(error
	 (case (car err)
	   (wrong-number-of-arguments nil)
	   (error
	    (if (string= "Could not locate marshaller function" (nth 1 err))
		(progn
		  (set-buffer (get-buffer-create "needed marshallers"))
		  (display-buffer (current-buffer))
		  (goto-char (point-max))
		  (insert
		   (format "%S\n"
			   (split-string
			    (substring (nth 2 err) (length "emacs_gtk_marshal_")) "_+")))))))))))

(defmacro gtk-import-function (retval name &rest args)
  (if (symbolp name)
      (setq name (symbol-name name)))
  (let ((lisp-name (intern (replace-in-string name "_" "-")))
	(doc-string nil))
    (setq retval (or (cdr-safe (assoc retval gtk-type-aliases)) retval)
	  doc-string (concat "The lisp version of " name ".\n"
			     (if args
				 (concat "Prototype: " (prin1-to-string args)))))

    ;; Drop off any naming of arguments, etc.
    (if (and args (consp (car args)))
	(setq args (mapcar 'car args)))

    ;; Get rid of any type aliases.
    (setq args (mapcar (lambda (x)
			 (or (cdr-safe (assoc x gtk-type-aliases)) x)) args))

    `(progn
       (defun ,lisp-name (&rest args)
	 ,doc-string
	 (if (not (get (quote ,lisp-name) 'gtk-ffi nil))
	     (put (quote ,lisp-name) 'gtk-ffi
		  (gtk-import-function-internal (quote ,retval) ,name
						(quote ,args))))
	 (gtk-call-function (get (quote ,lisp-name) 'gtk-ffi 'ignore) args))
       (and gtk-ffi-debug (gtk-ffi-check-function (quote ,lisp-name))))))

(defmacro gtk-import-variable (type name)
  (if (symbolp name) (setq name (symbol-name name)))
  (let ((lisp-name (intern (replace-in-string name "_" "-")))
	(doc-string nil))
    (setq type (or (cdr-safe (assoc type gtk-type-aliases)) type)
	  doc-string (concat "Retrieve the variable " name " (type: " (symbol-name type) ").\n"))
    `(defun ,lisp-name ()
       ,doc-string
       (gtk-import-variable-internal (quote ,type) ,name))))

(provide 'gtk-ffi)
