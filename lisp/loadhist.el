;;; loadhist.el --- lisp functions for working with feature groups

;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Version: 1.0
;; Keywords: internal, dumped

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

;;; Synched up with: FSF 20.2.

;;; Commentary:

;; This file is dumped with SXEmacs.

;; These functions exploit the load-history system variable.
;; Entry points include `unload-feature', `symbol-file', and `feature-file'.

;;; Code:

;; load-history is a list of entries that look like this:
;; ("outline" outline-regexp ... (require . wid-edit) ... (provide . outline) ...)

;; XEmacs; this function is in subr.el in GNU, and does not deal with
;; built-in symbols.
(defun* symbol-file (sym &optional type)
  "Return the input source from which SYM was loaded.
This is a file name, or nil if the source was a buffer with no associated file.

If TYPE is nil or omitted, any kind of definition is acceptable.
If TYPE is `defun', then function, subr, special form or macro definitions
are acceptable.
If TYPE is `defvar', then variable definitions are acceptable.

`defface' specifies a face definition only, and for the moment, it won't
return faces created with `make-face' or `copy-face', just those created
with `defface' and `custom-declare-face'."
  (interactive "SFind source file for symbol: ") ; XEmacs
  (let (built-in-file autoload-cons symbol-details)
    (cond ((and (eq 'autoload
		    (car-safe
		     (setq autoload-cons
			   (and (fboundp sym) (symbol-function sym)))))
		(or (and (or (null type) (eq 'defvar type))
			 (eq (fifth autoload-cons) 'keymap))
		    (and (or (null type) (eq 'defun type))
			 (memq (fifth autoload-cons) '(nil macro)))))
	   (return-from symbol-file (locate-library (second autoload-cons))))
	  ((eq 'defvar type)
	   ;; Load history entries corresponding to variables are just
	   ;; symbols.
	   (dolist (entry load-history)
	     (when (memq sym (cdr entry))
	       (return-from symbol-file (car entry)))))
	   ((not (null type))
	    ;; Non-variables have the type stored as the car of the entry.
	    (dolist (entry load-history)
	      (when (and (setq symbol-details (rassq sym (cdr entry)))
			 (eq type (car symbol-details)))
		(return-from symbol-file (car entry)))))
	  (t
	   ;; If TYPE hasn't been specified, we need to check both for
	   ;; variables and other symbols.
	   (dolist (entry load-history)
	     (when (or (memq sym (cdr entry))
		       (rassq sym (cdr entry)))
	       (return-from symbol-file (car entry))))))
    (when (setq built-in-file (built-in-symbol-file sym type))
      (if (equal built-in-file (file-truename built-in-file))
	  ;; Probably a full path name:
	  built-in-file
	;; This is a bit heuristic, but shouldn't realistically be a
	;; problem:
	(if (string-match "\.elc?$" built-in-file)
	    (concat lisp-directory built-in-file)
	  (concat source-directory "/src/" built-in-file))))))

(defun feature-symbols (feature)
  "Return the file and list of symbols associated with a given FEATURE."
  (let ((pair `(provide . ,feature)))
    (dolist (entry load-history)
      (when (member pair (cdr entry))
	(return entry)))))

(defun feature-file (feature)
  "Return the file name from which a given FEATURE was loaded.
Actually, return the load argument, if any; this is sometimes the name of a
Lisp file without an extension.  If the feature came from an eval-buffer on
a buffer with no associated file, or an eval-region, return nil."
  (unless (featurep feature)
    (error "%s is not a currently loaded feature" (symbol-name feature)))
  (car (feature-symbols feature)))

(defun file-symbols (file)
  "Return the file and list of symbols associated with FILE.
The file name in the returned list is the string used to load the file,
and may not be the same string as FILE, but it will be equivalent."
  (or (assoc file load-history)
      (assoc (file-name-sans-extension file) load-history)
      (assoc (concat file ".el") load-history)
      (assoc (concat file ".elc") load-history)))

(defun file-provides (file)
  "Return the list of features provided by FILE."
  (let ((provides nil))
    (dolist (x (cdr (file-symbols file)))
      (when (eq (car-safe x) 'provide)
	(push (cdr x) provides)))
    provides))

(defun file-requires (file)
  "Return the list of features required by FILE."
  (let ((requires nil))
    (dolist (x (cdr (file-symbols file)))
      (when (eq (car-safe x) 'require)
	(push (cdr x) requires)))
    requires))

(defun file-dependents (file)
  "Return the list of loaded libraries that depend on FILE.
This can include FILE itself."
  (let ((provides (file-provides file))
	(dependents nil))
    (dolist (entry load-history)
      (dolist (x (cdr entry))
	(when (and (eq (car-safe x) 'require)
		   (memq (cdr-safe x) provides))
	  (push (car entry) dependents))))
    dependents))

;; FSFmacs
;(defun read-feature (prompt)
;  "Read a feature name \(string\) from the minibuffer,
;prompting with PROMPT and completing from `features', and
;return the feature \(symbol\)."
;  (intern (completing-read prompt
;			   (mapcar #'(lambda (feature)
;			             (list (symbol-name feature)))
;				   features)
;			   nil t)))

;; ;;;###autoload
(defun unload-feature (feature &optional force)
  "Unload the library that provided FEATURE, restoring all its autoloads.
If the feature is required by any other loaded code, and optional FORCE
is nil, raise an error."
  (interactive "SFeature: ")
  (unless (featurep feature)
    (error "%s is not a currently loaded feature" (symbol-name feature)))
  (when (not force)
    (let* ((file (feature-file feature))
	   (dependents (delete file (copy-sequence (file-dependents file)))))
      (when dependents
	(error "Loaded libraries %s depend on %s"
	       (prin1-to-string dependents) file))))
  (let* ((flist (feature-symbols feature))
	 (file (car flist)))
    (flet ((reset-aload (x)
	     (let ((aload (get x 'autoload)))
	       (if aload (fset x (cons 'autoload aload))))))
    (mapcar
     #'(lambda (x)
	 (cond ((stringp x) nil)
	       ((consp x)
		;; Remove any feature names that this file provided.
		(if (eq (car x) 'provide)
		    (setq features (delq (cdr x) features))))
	       ((and (boundp x)
		     (fboundp x))
		(makunbound x)
		(fmakunbound x)
		(reset-aload x))
	       ((boundp x)
		(makunbound x))
	       ((fboundp x)
		(fmakunbound x)
		(reset-aload x))))
     (cdr flist)))
    ;; Delete the load-history element for this file.
    (let ((elt (assoc file load-history)))
      (setq load-history (delq elt load-history)))))

(provide 'loadhist)

;;; loadhist.el ends here
