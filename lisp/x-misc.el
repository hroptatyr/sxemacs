;;; x-misc.el --- miscellaneous X functions.

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 1995, 1996 Ben Wing.

;; Author: Ben Wing <ben@xemacs.org>
;; Maintainer: SXEmacs Development Team
;; Keywords: extensions, dumped

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

;;; Commentary:

;; This file is dumped with SXEmacs (when X support is compiled in).

;;; Code:

(defun x-bogosity-check-resource (name class type)
  "Check for a bogus resource specification."
  (let ((bogus (x-get-resource
		(concat "__no-such-friggin-locale__." name)
		(concat "__No-such-friggin-widget__." class)
		type 'global nil t)))
    (if bogus
	(display-warning
	 'resource
	 (format "Bad resource specification encountered: something like
     Emacs*%s: %s
You should replace the * with a . in order to get proper behavior when
you use the specifier and/or `set-face-*' functions." name bogus)))))

(defun x-init-specifier-from-resources (specifier type locale
						  &rest resource-list)
  "Initialize a specifier from the resource database.
LOCALE specifies the locale that is to be initialized and should be
a frame, a device, or 'global.  TYPE is the type of the resource and
should be one of 'string, 'boolean, 'integer, or 'natnum.  The
remaining args should be conses of names and classes of resources
to be examined.  The first resource with a value specified becomes
the spec for SPECIFIER in LOCALE. (However, if SPECIFIER already
has a spec in LOCALE, nothing is done.) Finally, if LOCALE is 'global,
a check is done for bogus resource specifications."
  (if (eq locale 'global)
      (mapcar #'(lambda (x)
		  (x-bogosity-check-resource (car x) (cdr x) type))
	      resource-list))
  (if (not (specifier-spec-list specifier locale))
      (catch 'done
	(while resource-list
	  (let* ((name (caar resource-list))
		 (class (cdar resource-list))
		 (resource
		  (x-get-resource name class type locale nil 'warn)))
	    (if resource
		(progn
		  (add-spec-to-specifier specifier resource locale)
		  (throw 'done t))))
	  (setq resource-list (cdr resource-list))))))

(defun x-get-resource-and-bogosity-check (name class type &optional locale)
  (x-bogosity-check-resource name class type)
  (x-get-resource name class type locale nil 'warn))

(defun x-get-resource-and-maybe-bogosity-check (name class type &optional
						     locale)
  (if (eq locale 'global)
      (x-bogosity-check-resource name class type))
  (x-get-resource name class type locale nil 'warn))

;;; x-misc.el ends here
