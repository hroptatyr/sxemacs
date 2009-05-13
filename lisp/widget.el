;;; widget.el --- a library of user interface components.

;; Copyright (C) 1996, 1997 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: Hrvoje Niksic <hniksic@xemacs.org>
;; Keywords: help, extensions, faces, hypermedia, dumped
;; Version: 1.9960-x
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

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

;; This file is dumped with SXEmacs.

;; If you want to use this code, please visit the URL above.

;; This file only contain the code needed to define new widget types.
;; Everything else is autoloaded from `wid-edit.el'.

;;; Code:

;; Neither XEmacs, nor latest GNU Emacs need this -- provided for
;; compatibility.
;; (defalias 'define-widget-keywords 'ignore)

(defmacro define-widget-keywords (&rest keys)
  "This doesn't do anything in Emacs 20 or XEmacs."
  `(eval-and-compile
     (let ((keywords (quote ,keys)))
       (while keywords
	 (or (boundp (car keywords))
	     (set (car keywords) (car keywords)))
	 (setq keywords (cdr keywords))))))

(defun define-widget (name class doc &rest args)
  "Define a new widget type named NAME from CLASS.

NAME and CLASS should both be symbols, CLASS should be one of the
existing widget types, or nil to create the widget from scratch.

After the new widget has been defined, the following two calls will
create identical widgets:

* (widget-create NAME)

* (apply 'widget-create CLASS ARGS)

The third argument DOC is a documentation string for the widget."
  (check-argument-type 'symbolp name)
  (check-argument-type 'symbolp class)
  (put name 'widget-type (cons class args))
  (put name 'widget-documentation doc)
  name)

;;; The End.

(provide 'widget)

;;; widget.el ends here
