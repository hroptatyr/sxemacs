;; Mule default configuration file

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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; 87.6.9   created by K.handa
;;; (Note: following comment obsolete -- mrb)

;;; IMPORTANT NOTICE -- DON'T EDIT THIS FILE!!!
;;;  Keep this file unmodified for further patches being applied successfully.
;;;  All language specific basic environments are defined here.
;;;  By default, Japanese is set as the primary environment.
;;;  You can change primary environment in `./lisp/site-init.el by
;;;  `set-primary-environment'.  For instance,
;;;  	(set-primary-environment 'chinese)
;;;  makes Chinese the primary environment.
;;;  If you are still not satisfied with the settings, you can
;;;  override them after the above line.  For instance,
;;;  	(set-default-buffer-file-coding-system 'big5)
;;;  makes big5 be used for file I/O by default.
;;;  If you are not satisfied with other default settings in this file,
;;;  override any of them also in `./lisp/site-init.el'.  For instance,
;;;	(define-program-coding-system nil ".*mail.*" 'iso-8859-1)
;;;  makes the coding-system 'iso-8859-1 be used in mail.


;;;; GLOBAL ENVIRONMENT SETUP
(require 'cl)


;; (setq language-environment-list
;;       (sort (language-environment-list) 'string-lessp))

;; MULE keymap codes were moved to mule-cmds.el.

;; Alternative key definitions
;; Original mapping will be altered by set-keyboard-coding-system.
(define-key global-map [(meta \#)] 'ispell-word)	;originally "$"
;; (define-key global-map [(meta {)] 'insert-parentheses) ;originally "("

;; Following line isn't mule-specific --mrb
;;(setq-default modeline-buffer-identification '("XEmacs: %17b"))

;; MULE keymap codes were moved to mule-cmds.el.

;; (define-key help-map "T" 'help-with-tutorial-for-mule)

;; (defvar help-with-tutorial-language-alist
;;  '(("Japanese" . ".jp")
;;    ("Korean"   . ".kr")
;;    ("Thai"     . ".th")))

;(defun help-with-tutorial-for-mule (language)
;  "Select the Mule learn-by-doing tutorial."
;  (interactive (list (let ((completion-ignore-case t)
;			   lang)
;		       (completing-read
;			"Language: "
;			help-with-tutorial-language-alist))))
;  (setq language (cdr (assoc language help-with-tutorial-language-alist)))
;  (help-with-tutorial (concat "mule/TUTORIAL" (or language ""))))

(defvar auto-language-alist
  '(("^ja" . "Japanese")
    ("^zh_.*.GB.*" . "Chinese-GB")
    ("^zh_.*.BIG5.*" . "Chinese-BIG5")
    ("^ko" . "Korean"))
  "Alist of LANG patterns vs. corresponding language environment.
Each element looks like (REGEXP . LANGUAGE-ENVIRONMENT).
It the value of the environment variable LANG matches the regexp REGEXP,
then `set-language-environment' is called with LANGUAGE-ENVIRONMENT.")

(defun init-mule ()
  "Initialize MULE environment at startup.  Don't call this."
  (let ((lang (or (getenv "LC_ALL") (getenv "LC_MESSAGES") (getenv "LANG"))))
    (unless (or (null lang) (string-equal "C" lang))
      (let ((case-fold-search t))
	(loop for elt in auto-language-alist
	      if (string-match (car elt) lang)
	      return (progn
		       (setq lang (substring lang 0 (match-end 0)))
		       (set-language-environment (cdr elt))
		       )))
      ;; Load a (localizable) locale-specific init file, if it exists.
      (load (format "%s%s/locale-start"
		    (locate-data-directory "start-files")
		    lang) t t)))
  
  (when current-language-environment
    ;; Translate remaining args on command line using file-name-coding-system
    (loop for arg in-ref command-line-args-left do
	  (setf arg (decode-coding-string arg file-name-coding-system)))
    
    ;; rman seems to be incompatible with encoded text
    (setq Manual-use-rosetta-man nil)

    ;; Make sure ls -l output is readable by dired and encoded using
    ;; file-name-coding-system
    (add-hook
     'dired-mode-hook
     (lambda ()
       (make-local-variable 'process-environment)
       (setenv "LC_MESSAGES" "C")
       (setenv "LC_TIME"     "C"))))
  
  ;; Register available input methods by loading LEIM list file.
  (load "leim-list.el" 'noerror 'nomessage 'nosuffix)
  )

(add-hook 'before-init-hook 'init-mule)

;;;;; Enable the tm package by default
;;(defun init-mule-tm ()
;;  "Load MIME (TM) support for GNUS, VM, MH-E, and RMAIL."
;;  (load "mime-setup"))

;;(add-hook 'after-init-hook 'init-mule-tm)

;;; mule-init.el ends here
