;;; compile-core.el --- Bytecompile out-of-date dumped files
;;
;; Copyright (C) 2006 Sebastian Freundt
;;
;; Author: Sebastian Freundt <hroptatyr@sxemacs.org>
;; Maintainer: SXEmacs Development Team
;; Keywords: internal
;;
;; This file is part of SXEmacs.
;;
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
;;
;;; Synched up with: Not in FSF.

;;; Commentary:

;;; Code:

(load "loadup-el.el")
(load "auto-autoloads.el")
(load "custom-defines.el")
(load "bytecomp.el")
(load "byte-optimize.el")
(when (featurep '(and mule (not mule-autoloads)))
  (load-file "mule/auto-autoloads.el")
  (load-file "mule/custom-defines.el"))
(when (and (fboundp #'ffi-defun)
	   (not (featurep 'ffi-autoloads)))
  (load-file "ffi/auto-autoloads.el")
  (load-file "ffi/custom-defines.el"))

(princ "Compiling core lisp files...")

(defvar destdir
  default-directory)
(defvar sourcedir
  (file-name-directory (locate-file "compile-core.el" load-path)))
(defvar lispdir-regexp
  (compile-regexp "lisp/"))

(defun byte-compile-dest-file (filename)
  "Convert an Emacs Lisp source file name to a compiled file name."
  (let ((outfile (if (string-match lispdir-regexp filename)
		     (file-name-sans-extension
		      (substring filename (match-end 0)))
		   filename)))
    (expand-file-name (concat outfile ".elc") destdir)))

(defun parse-command-line (cmdl)
  (let ((newcmdl (dllist))
	(cmdlpl (make-skiplist))
	(mm (compile-regexp "^--"))
	(ign (compile-regexp "^-[^-]")))
    (while (car cmdl)
      (let* ((file (car cmdl))
	     (current (expand-file-name file sourcedir))
	     (current (if (file-exists-p current)
			  current
			(expand-file-name file destdir))))
	(cond ((string-match mm file)
	       (let ((key (intern file))
		     (val (car (cdr-safe cmdl))))
		 (put-skiplist cmdlpl key val)
		 (setq cmdl (cdr-safe cmdl))))
	      ((string-match ign file)
	       (setq cmdl (cdr-safe cmdl)))
	      ((string-match emacs-lisp-file-regexp current)
	       (dllist-append newcmdl current))
	      (t nil)))
      (setq cmdl (cdr-safe cmdl)))
    (put newcmdl :tweaks cmdlpl)
    newcmdl))

(setq files (parse-command-line (cdr-safe command-line-args)))
(setq params (get files :tweaks))

(setq files-to-compile (dllist))
(if (get-skiplist params '--force)
    (setq files-to-compile files)
  (mapc-internal
   #'(lambda (file)
       (when (file-newer-than-file-p file (byte-compile-dest-file file))
	 (dllist-append files-to-compile file)))
   files))

(setq problem-files (dllist))
(mapc-internal
 #'(lambda (file)
     (condition-case nil
	 (byte-compile-file file)
       (error
	(progn
	  (dllist-append problem-files file)
	  (message "Dinn work: %s" file)))))
 files-to-compile)

;; (mapc-internal
;;  #'(lambda (file)
;;      (condition-case nil
;;          (byte-compile-file file)
;;        (error
;;         (progn
;;           (dllist-append problem-files file)
;;           (message "Dinn work: %s" file)))))
;;  problem-files)

;;; compile-core.el ends here
