;; dump-paths.el --- set up XEmacs paths for dumping

;; Copyright (C) 1985, 1986, 1992, 1994, 1997 Free Software Foundation, Inc.

;; Maintainer: XEmacs Development Team
;; Keywords: internal, dumped

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

;; This sets up the various paths for continuing loading files for
;; dumping.
;; #### This code is duplicated in startup.el (startup-setup-paths).

(let ((debug-paths (or debug-paths
		      (and (getenv "EMACSDEBUGPATHS")
			   t)))
      (roots (paths-find-emacs-roots invocation-directory
				     invocation-name)))

  (if debug-paths
      (princ (format "XEmacs thinks the roots of its hierarchy are:\n%S\n"
		     roots)))

  (let* ((package-locations
	  (packages-compute-package-locations
	   ;; temporary kludge:
	   ;; this should be synched with startup.el
	   (paths-construct-path '("~" ".xemacs"))))
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
  (setq site-directory (and (null inhibit-site-lisp)
			    (paths-find-site-lisp-directory roots)))
  (if (and debug-paths (null inhibit-site-lisp))
      (princ (format "site-directory:\n%S\n" site-directory)
	     'external-debugging-output))

  (setq load-path (paths-construct-load-path roots
					     '()
					     late-package-load-path
					     '()
					     lisp-directory
					     site-directory
					     mule-lisp-directory))

  (setq module-directory (paths-find-module-directory roots))
  (if debug-paths
      (princ (format "module-directory:\n%S\n" module-directory)
	     'external-debugging-output))
  (setq site-module-directory (and (null inhibit-site-modules)
			    (paths-find-site-module-directory roots)))
  (if (and debug-paths (null inhibit-site-modules))
      (princ (format "site-module-directory:\n%S\n" site-module-directory)
	     'external-debugging-output))

  (setq module-load-path (paths-construct-module-load-path roots
							 module-directory
							 site-module-directory)))

;;; dump-paths.el ends here
