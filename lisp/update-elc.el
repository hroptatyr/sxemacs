;;; update-elc.el --- Bytecompile out-of-date dumped files

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) 1996 Sun Microsystems, Inc.

;; Author: Ben Wing <ben@xemacs.org>, Steven L Baur <steve@xemacs.org>
;; Maintainer: SXEmacs Development Team
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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; Byte compile the .EL files necessary to dump out xemacs.
;; Use this file like this:

;; temacs -batch -l ../lisp/update-elc.el $lisp

;; where $lisp comes from the Makefile.  .elc files listed in $lisp will
;; cause the corresponding .el file to be compiled.  .el files listed in
;; $lisp will be ignored.

;; (the idea here is that you can bootstrap if your .ELC files
;; are missing or badly out-of-date)

;; See also update-elc-2.el

;;; Code:

(defvar processed nil)
(defvar update-elc-files-to-compile nil)

;(setq update-elc-files-to-compile
;      (delq nil
;	    (mapcar (function
;		     (lambda (x)
;		       (if (string-match "\.elc$" x)
;			   (let ((src (substring x 0 -1)))
;			     (if (file-newer-than-file-p src x)
;				 (progn
;				   (and (file-exists-p x)
;					(null (file-writable-p x))
;					(set-file-modes x (logior (file-modes x) 128)))
;				   src))))))
;		    ;; -batch gets filtered out.
;		    (nthcdr 3 command-line-args))))

;; (let ((build-root (getenv "top_srcdir"))) ;;;(expand-file-name ".." invocation-directory)))
;;   (setq load-path (list (expand-file-name "lisp" build-root))))

(load "very-early-lisp" nil t)

(load "find-paths.el")
(load "packages.el")
(load "setup-paths.el")
(load "dump-paths.el")

(let ((autol (packages-list-autoloads
	      (if (getenv "BUILD_TREE_ROOT")
		  (expand-file-name "lisp" (getenv "BUILD_TREE_ROOT"))
		(concat default-directory "../lisp")))))

  ;;(print (prin1-to-string autol))
  (while autol
    (let ((src (car autol)))
      (if (and (file-exists-p src)
	       (file-newer-than-file-p src (concat src "c")))
	  (setq update-elc-files-to-compile
		(cons src update-elc-files-to-compile))))
    (setq autol (cdr autol))))

;; (print (prin1-to-string update-elc-files-to-compile))

(let (preloaded-file-list site-load-packages need-to-dump dumped-exe)
  (load (if (getenv "SOURCE_TREE_ROOT")
	    (expand-file-name "lisp/dumped-lisp.el" (getenv "SOURCE_TREE_ROOT"))
	  (concat default-directory "../lisp/dumped-lisp.el")))

  (setq dumped-exe
	(cond ((file-exists-p "../src/sxemacs") "../src/sxemacs")
	      (t nil)))

  ;; Path setup
  (let ((package-preloaded-file-list
	 (packages-collect-package-dumped-lisps late-package-load-path)))

    (setq preloaded-file-list
	  (append package-preloaded-file-list
		  preloaded-file-list
		  '("bytecomp")
		  packages-hardcoded-lisp)))

  (load (concat default-directory "../site-packages") t t)
  (setq preloaded-file-list
	(append packages-hardcoded-lisp
		preloaded-file-list
		site-load-packages))

  (while preloaded-file-list
    (let ((arg (car preloaded-file-list)))
      ;; (print (prin1-to-string arg))

      ;; now check if .el or .elc is newer than the dumped exe.
      ;; if so, need to redump.
      (let ((frob
	     (if (string-match "\\.elc?\\'" arg)
		 (substring arg 0 (match-beginning 0))
	       arg)))
	    (when (and dumped-exe
		       (or (let ((frobel
				  (if (getenv "SOURCE_TREE_ROOT")
				      (expand-file-name
				       (concat "lisp/" frob ".el")
				       (getenv "SOURCE_TREE_ROOT"))
				    (concat "../lisp/" frob ".el"))))
			     (and (file-exists-p frobel)
				  (file-newer-than-file-p frobel dumped-exe)))
			   (let ((frobelc
				  (if (getenv "BUILD_TREE_ROOT")
				      (expand-file-name
				       (concat "lisp/" frob ".elc")
				       (getenv "BUILD_TREE_ROOT"))
				    (concat "../lisp/" frob ".elc"))))
			     (and (file-exists-p frobelc)
				  (file-newer-than-file-p frobelc dumped-exe)))))
	      (setq need-to-dump t)))

;      (if (null (member (file-name-nondirectory arg)
;			packages-unbytecompiled-lisp))
;	  (progn

      (setq arg (locate-library arg))
      (if (null arg)
	  (progn
	    (print (format "Error: Library file %s not found"
			   (car preloaded-file-list)))
	    ;; Uncomment in case of trouble
	    ;;(print (format "late-packages: %S" late-packages))
	    ;;(print (format "guessed-roots: %S" (paths-find-emacs-roots invocation-directory invocation-name)))
	    (kill-emacs)))
      (if (string-match #r"\.elc?\'" arg)
	  (setq arg (substring arg 0 (match-beginning 0))))
      (if (and (null (member arg processed))
	       (file-exists-p (concat arg ".el"))
	       (file-newer-than-file-p (concat arg ".el")
				       (concat arg ".elc")))
	  (setq processed (cons (concat arg ".el") processed)))
      (setq preloaded-file-list (cdr preloaded-file-list))))

  (if need-to-dump
      (condition-case nil
	  (write-region-internal "foo" nil "../src/NEEDTODUMP")
	(file-error nil)))

  )

(prin1 processed)
(setq update-elc-files-to-compile (append update-elc-files-to-compile
					  (nreverse processed)))

;; (print (prin1-to-string update-elc-files-to-compile))

(if update-elc-files-to-compile
    (progn
      (setq command-line-args
	    (append '("-l" "loadup-el.el" "run-temacs"
		      "-batch" "-q" "-no-site-file"
		      "-l" "bytecomp" "-f" "batch-byte-compile")
		    update-elc-files-to-compile))
      (load "loadup-el.el"))
  (condition-case nil
      (delete-file "../src/NOBYTECOMPILE")
    (file-error nil)))

(kill-emacs)

;;; update-elc.el ends here
