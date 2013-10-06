;; emod-utils.el --- Lisp utils for emodules   -*- Emacs-Lisp -*-

;; Copyright (C) 2008 Steve Youngs

;; Author:     Steve Youngs <steve@sxemacs.org>
;; Maintainer: SXEmacs Development Team <sxemacs-devel@sxemacs.org>
;; Created:    <2008-05-01>
;; Homepage:   http://www.sxemacs.org/
;; Keywords:   util, module, emodule, dumped

;; This file is part of SXEmacs.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the author nor the names of any contributors
;;    may be used to endorse or promote products derived from this
;;    software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;;
;;    Here are a number of utils for interacting with emodules, such
;;    as finding them, loading them.  That sort of thing.
;;
;;    This file is dumped with SXEmacs.

;;; Todo:
;;
;;

;;; Code:
(defvar emodule-completions nil
  "List of emodules for use in completion with `load-module'.")

(defvar load-module-history nil
  "History for `load-module'.")

(defun emodule-completions (&optional path)
  "Return a list of emodules.

It searches through `module-load-path' by default, or PATH if that
optional argument is set.

PATH can be either a list of path strings, or it can be a colon
delimited path string."
  (let ((dirs (or (if (stringp path)
		      (split-string-by-char path ?:)
		    path)
		  module-load-path))
	(types (mapfam
	        #'(lambda (e)
	            (replace-in-string e "\\." ""))
	        :initiator "\\.\\("
	        :terminator "\\)$"
	        :separator "\\|"
	        :result-type #'concat module-extensions))
	completions)
    (while dirs
      (let ((files (directory-files-recur (car dirs) nil types 'list t 0)))
	(when (and files (> (length files) 0))
	  (setq completions
		(append completions
			(mapfam
			 #'file-name-sans-extension files
			 :result-type #'list))))
	(setq dirs (cdr dirs))))
    (remove-duplicates (remove nil completions) :test #'string-equal)))

(defun locate-module (emod)
  "Similar to `locate-library', but for emodules."
  (interactive
   (list (completing-read "Locate Emodule: "
			  (mapfam #'list (or emodule-completions
					     (emodule-completions)))
			  nil nil nil load-module-history)))
  (unless emodule-completions
    (setq emodule-completions (emodule-completions)))
  (let* ((emod (file-name-sans-extension emod))
	 (location (locate-file emod module-load-path
				module-extensions)))
    (if (interactive-p)
	(message "%s is: %s" emod location)
      location)))

(defun load-module (emod)
  "Similar to `load-library', but for emodules."
  (interactive
   (list (completing-read "Load emodule: "
			  (mapfam #'list (or emodule-completions
					     (emodule-completions)))
			  nil nil nil load-module-history)))
  (unless emodule-completions
    (setq emodule-completions (emodule-completions)))
  (if (string-equal emod "")
      (error 'invalid-argument emod)
    (and-fboundp #'load-module-file
      (load-module-file
       (or (locate-module emod)
	   emod)))))

(defun list-modules ()
  "Return a list of loaded emodules, display in echo area when interactive."
  (interactive)
  (and-fboundp #'list-loaded-modules
    (let ((emods (list-loaded-modules)))
      (if (interactive-p)
	  (message "Loaded emodules: %s"
		   (mapfam nil emods :separator " " :result-type #'concat))
	emods))))

(provide 'emod-utils)
;;; emod-utils.el ends here
