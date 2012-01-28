;;; make-docfile.el --- Cache docstrings in external file

;; Copyright (C) 1985, 1986, 1992-1995, 1997 Free Software Foundation, Inc.

;; Author: Unknown
;; Maintainer: Steven L Baur <steve@xemacs.org>
;; Keywords: internal

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

;;; Synched up with: Not in FSF

;;; Commentary:

;; This is a front-end to the make-docfile program that gathers up all the
;; lisp files that will be dumped with XEmacs.  It would probably be best
;; to just move make-docfile.c completely to lisp and be done with it.

;;; SXEmacs Note: When we moved to a libtool based build chain, I had
;;; to remove all the #'princ calls here otherwise `make DOC' just
;;; gives a zero length DOC file.  I doubt that this is a good or even
;;; real fix. --SY.

;;; Code:

(defvar options nil)
(defvar processed nil)
(defvar docfile nil)
(defvar docfile-buffer nil)
(defvar site-file-list nil)
(defvar docfile-out-of-date nil)

(defvar build-directory (expand-file-name ".." (expand-file-name ".." invocation-directory)))
(defvar build-lib-src (expand-file-name "lib-src" build-directory))
(defvar source-lisp (file-name-directory #$))
(defvar source-src (expand-file-name "../src" source-lisp))

(defun message (fmt &rest args)
  (princ (apply #'format fmt args))
  (terpri))

;; Gobble up the stuff we don't wish to pass on.
(setq command-line-args (cdr (cdr (cdr (cdr command-line-args)))))

;; First gather up the command line options.
(let (done)
  (while (and (null done) command-line-args)
    (let ((arg (car command-line-args)))
      (cond ((or (string-equal arg "-o") ; Specify DOC file name
		 (string-equal arg "-a") ; Append to DOC file
		 (string-equal arg "-d")) ; Set working directory
	     (if (string-equal arg "-o")
		 (setq docfile (expand-file-name (car (cdr command-line-args)))))
	     (setq options (cons arg options))
	     (setq options (cons (expand-file-name (car (cdr command-line-args))) options)))
	    ((string-equal arg "-i") ; Set site files to scan
	     (setq site-file-list (car (cdr command-line-args))))
	    (t (setq done t)))
      (if (null done)
	  (setq command-line-args (cdr (cdr command-line-args)))))))
(setq options (nreverse options))

;; (print (concat "Options: " (prin1-to-string options)))

;; Next process the list of C files.
(while command-line-args
  (let ((arg (car command-line-args)))
    (if (null (member arg processed))
	(progn
	  (if (and (null docfile-out-of-date)
		   (file-newer-than-file-p arg docfile))
	      (setq docfile-out-of-date t))
	  (setq processed (cons arg processed)))))
  (setq command-line-args (cdr command-line-args)))

;; Then process the list of Lisp files.
;;; We use the ENVironment approach
;; (let ((build-root (expand-file-name ".." invocation-directory)))
;;   (setq load-path (list (expand-file-name "lisp" build-root))))

(load "very-early-lisp" nil t)

;; Then process the autoloads
(setq autoload-file-name "auto-autoloads.elc")
(load "find-paths.el")
(load "packages.el")
(load "setup-paths.el")
(load "dump-paths.el")
(load "bytecomp-runtime.el")
(load "subr.el")
(load "backquote.el")
(load "replace.el")
(load "version.el")
(load "cl.el")
(load "cl-extra.el")
(require 'custom)
(load "process")

(let (preloaded-file-list arg0 arg package-preloaded-file-list absolute)
  (load (expand-file-name "dumped-lisp.el" source-lisp))

  (setq package-preloaded-file-list
	(packages-collect-package-dumped-lisps late-package-load-path)
	preloaded-file-list
	(append package-preloaded-file-list
		preloaded-file-list
		packages-hardcoded-lisp)

	processed (cons "-d" processed)
	processed (cons source-lisp processed)
	;; Include loadup.el, which is never in preloaded-file-list:
	processed (cons "loadup.el" processed))

  (while preloaded-file-list
    (setq arg0 (packages-add-suffix (car preloaded-file-list))
	  arg (locate-library arg0)
	  absolute arg)
    (if (null arg)
	(progn
	  (message "Error: dumped file %s does not exist" arg0)
	  ;; Uncomment in case of difficulties
	  ;(message "late-package-hierarchies: %S"
	  ;         late-package-hierarchies)
	  ;(message "guessed-roots: %S" (paths-find-emacs-roots
	  ;                              invocation-directory
	  ;                              invocation-name
	  ;                              #'paths-emacs-root-p))
	  ;(message "guessed-data-roots: %S" (paths-find-emacs-roots
	  ;                                   invocation-directory
	  ;                                   invocation-name
	  ;                                   #'paths-emacs-data-root-p))
	  )
      (when (equal arg (expand-file-name arg0 source-lisp))
	;; Use relative paths where possible, since this makes file lookup
	;; in an installed XEmacs easier:
	(setq arg arg0))
      (if (null (member arg processed))
	  (progn
	    (if (and (null docfile-out-of-date)
		     ;; We need to check the absolute path here:
		     (file-newer-than-file-p absolute docfile))
		(setq docfile-out-of-date t))
	    (setq processed (cons arg processed)))))
    (setq preloaded-file-list (cdr preloaded-file-list))))

;; Finally process the list of site-loaded files.
(if site-file-list
    (let (site-load-packages)
      (load site-file-list t t)
      (while site-load-packages
	(let ((arg (car site-load-packages)))
	  (if (null (member arg processed))
	      (progn
		(if (and (null docfile-out-of-date)
			 (file-newer-than-file-p arg docfile))
		    (setq docfile-out-of-date t))
		(setq processed (cons arg processed)))))
	(setq site-load-packages (cdr site-load-packages)))))

;(let ((autoloads (packages-list-autoloads-path)))
;  ;; (print (concat "Autoloads: " (prin1-to-string autoloads)))
;  (while autoloads
;    (let ((arg (car autoloads)))
;      (if (null (member arg processed))
;	  (progn
;	    ;; (print arg)
;	    (if (and (null docfile-out-of-date)
;		     (file-newer-than-file-p arg docfile))
;		(setq docfile-out-of-date t))
;	    (setq processed (cons arg processed))))
;      (setq autoloads (cdr autoloads)))))

;; Now fire up make-docfile and we're done

(setq processed (nreverse processed))

;; (print (prin1-to-string (append options processed)))

(if docfile-out-of-date
    (progn
      (princ "Spawning make-docfile ...")
      ;; (print (prin1-to-string (append options processed)))

      (setq exec-path (list (concat default-directory "../lib-src")))

      ;; (locate-file-clear-hashing nil)
      (if (memq system-type '(berkeley-unix next-mach))
	  ;; Suboptimal, but we have a unresolved bug somewhere in the
	  ;; low-level process code
	  (call-process-internal
	   "/bin/csh"
	   nil
	   t
	   nil
	   "-fc"
	   (mapconcat
	    #'identity
	    (append
	     (list (concat default-directory "../lib-src/make-docfile"))
	     options processed)
	    " "))
	;; (print (prin1-to-string (append options processed)))
	(apply 'call-process-internal
	       ;; (concat default-directory "../lib-src/make-docfile")
	       "make-docfile"
	       nil
	       t
	       nil
	       (append options processed)))

      (princ "Spawning make-docfile ...done\n")
      ;; (write-region-internal (point-min) (point-max) "/tmp/DOC")
      )
  (princ "DOC file is up to date\n"))

(kill-emacs)

;;; make-docfile.el ends here
