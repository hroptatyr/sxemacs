;; dump-paths.el --- set up SXEmacs paths for dumping

;; Copyright (C) 1985, 1986, 1992, 1994, 1997 Free Software Foundation, Inc.

;; Maintainer: SXEmacs Development Team
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

;;; Synched up with: Not in FSF

;;; Commentary:

;; This sets up the various paths for continuing loading files for
;; dumping.
;; #### This code is duplicated in startup.el (startup-setup-paths).

(let ((debug-paths (or debug-paths
		      (and (getenv "EMACSDEBUGPATHS")
			   t)))
      (roots (paths-find-emacs-roots invocation-directory
				     invocation-name t)))

  (when debug-paths
    (princ (format "SXEmacs thinks the roots of its hierarchy are:\n%S\n"
		   roots)))

  (let* ((package-locations
	  (packages-compute-package-locations
	   ;; temporary kludge:
	   ;; this should be synched with startup.el
	   (paths-construct-path '("~" ".sxemacs"))))
	 (stuff (packages-find-packages roots package-locations)))
    (setq late-packages (car (cdr stuff))))

  (setq late-package-load-path (packages-find-package-load-path late-packages))

  (if debug-paths
      (progn
	(princ (format "configure-package-path:\n%S\n" configure-package-path)
	       'external-debugging-output)
	(princ (format "late-packages and late-package-load-path:\n%S\n%S\n"
		       late-packages late-package-load-path)
	       'external-debugging-output)))

  (setq lisp-directory (paths-find-lisp-directory roots))
  (if debug-paths
      (princ (format "lisp-directory:\n%S\n" lisp-directory)
	     'external-debugging-output))
  (if (featurep 'mule)
      (progn
	(setq mule-lisp-directory
	      (paths-find-mule-lisp-directory roots
					      lisp-directory))
	(if debug-paths
	    (princ (format "mule-lisp-directory:\n%S\n"
			   mule-lisp-directory)
		   'external-debugging-output)))
    (setq mule-lisp-directory '()))

  (setq ffi-lisp-directory
	(when (fboundp #'ffi-defun)
	  (paths-find-ffi-lisp-directory roots
					 lisp-directory)))

  (setq load-path (paths-construct-load-path roots
					     '()
					     late-package-load-path
					     '()
					     lisp-directory
					     nil
					     mule-lisp-directory
					     ffi-lisp-directory))

  (setq exec-directory (paths-find-exec-directory roots))
  (if debug-paths
      (princ (format "exec-directory:\n%S\n" exec-directory)
	     'external-debugging-output))

  (setq module-directory (paths-find-module-directory roots))
  (if debug-paths
      (princ (format "module-directory:\n%S\n" module-directory)
	     'external-debugging-output))
  (setq site-module-directory (and (null inhibit-site-modules)
			    (paths-find-site-module-directory roots)))
  (if (and debug-paths (null inhibit-site-modules))
      (princ (format "site-module-directory:\n%S\n" site-module-directory)
	     'external-debugging-output))

  (when debug-paths
    (princ (format "load-path:\n%S\n" load-path))))

;;; dump-paths.el ends here
