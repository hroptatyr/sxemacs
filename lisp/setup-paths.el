;;; setup-paths.el --- setup various SXEmacs paths

;; Copyright (C) 1985-1986, 1990, 1992-1997 Free Software Foundation, Inc.
;; Copyright (c) 1993, 1994 Sun Microsystems, Inc.
;; Copyright (C) 1995 Board of Trustees, University of Illinois

;; Author: Mike Sperber <sperber@informatik.uni-tuebingen.de>
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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file is dumped with XEmacs.

;; This file describes and constructs the various paths into the
;; XEmacs hierarchy from a global viewpoint.

;; It requires find-paths.el and packages.el.

;;; Code:

(defvar paths-core-load-path-depth 0
  "Depth of load-path searches in core Lisp paths.")

(defvar paths-site-load-path-depth 1
  "Depth of load-path searches in site Lisp paths.")

(defvar paths-mule-load-path-depth 0
  "Depth of load-path searches in Mule Lisp paths.")

(defvar paths-ffi-load-path-depth 0
  "Depth of load-path searches in FFI Lisp paths.")

(defvar paths-module-load-path-depth 1
  "Depth of load-path searches in emodule paths.")

(defvar paths-default-info-directories
  (mapcar (function
	   (lambda (dirlist)
	     (paths-construct-path
	      dirlist (char-to-string directory-sep-char))))
	  '(("usr" "local" "info")
	    ("usr" "info")
	    ("usr" "local" "share" "info")
	    ("usr" "share" "info")))
  "Directories appended to the end of the info path by default.")

(defun paths-find-site-module-directory (roots)
  "Find the site modules directory of the XEmacs hierarchy."
  (paths-find-site-archdep-directory roots "site-modules"
                                       nil
                                       configure-site-module-directory))

(defun paths-find-lisp-directory (roots)
  "Find the main Lisp directory of the XEmacs hierarchy."
  (or (paths-find-version-archindep-directory
       roots "lisp" nil configure-lisp-directory)
      (paths-find-version-archdep-directory
       roots "lisp" nil configure-lisp-directory)))

(defun paths-find-mule-lisp-directory (roots &optional lisp-directory)
  "Find the Mule Lisp directory of the XEmacs hierarchy."
  ;; #### kludge
  (if lisp-directory
      (let ((guess
	     (file-name-as-directory
	      (paths-construct-path (list lisp-directory "mule")))))
	(if (paths-file-readable-directory-p guess)
	    guess
	  (paths-find-version-archindep-directory
           roots "mule-lisp" nil configure-mule-lisp-directory)))))

(defun paths-find-ffi-lisp-directory (roots &optional lisp-directory)
  "Find the FFI Lisp directory of the SXEmacs hierarchy."
  ;; #### kludge
  (if lisp-directory
      (let ((guess
	     (file-name-as-directory
	      (paths-construct-path (list lisp-directory "ffi")))))
	(if (paths-file-readable-directory-p guess)
	    guess
	  (or (paths-find-version-archdep-directory
               roots "ffi-lisp" nil)
              (paths-find-version-archindep-directory
               roots "ffi-lisp" nil))))))

(defun paths-find-module-directory (roots)
  "Find the main modules directory of the SXEmacs hierarchy."
  (or 
   ;; for inplace stuff
   (paths-find-emacs-directory roots "" "modules"
                               nil configure-module-directory)
   (paths-find-architecture-directory roots "modules"
                                      nil configure-module-directory)))

(defun paths-construct-module-load-path
  (root module-directory &optional site-module-directory)
  "Construct the modules load path.

If the environment variable \"EMACSMODULEPATH\" is set then it is used
for `module-load-path' and this function does squat.  EMACSMODULEPATH
should be a colon delimited list of directory paths.

ROOT is `emacs-roots'.  If that's nil, this will blow up.  But if
`emacs-roots' is nil, your SXEmacs will blow up before you get here
anyway.

MODULE-DIRECTORY is the directory containing the emodules distributed
with the core SXEmacs.  If for some reason it is nil, it'll be skipped
here.

If SITE-MODULE-DIRECTORY doesn't exist as a directory on disc, it is
skipped.  If it does exist but the argument here is nil, it is hunted
down."
  (if (getenv "EMACSMODULEPATH")
      (setq module-load-path (paths-decode-directory-path
			      (getenv "EMACSMODULEPATH") 'drop-empties))
    (let* ((user-path
	    (and system-configuration
		 user-init-directory
		 (paths-find-recursive-load-path 
		  (list (paths-construct-path
			 (list system-configuration "modules") user-init-directory))
		  paths-module-load-path-depth)))
	   (site-dir (paths-find-site-module-directory root))
	   (site-path
	    (or (and site-module-directory
		     (paths-find-recursive-load-path (list site-module-directory)
						     paths-module-load-path-depth))
		(and site-dir
		     (paths-find-recursive-load-path
		      (list site-dir) paths-module-load-path-depth))))
	   (module-path
	    (and module-directory
		 (paths-find-recursive-load-path (list module-directory)
						 paths-module-load-path-depth))))
      (setq module-load-path (append (when user-path user-path)
				     (when site-path site-path)
				     (when module-path module-path))))))

(defun paths-construct-load-path
  (roots early-package-load-path late-package-load-path last-package-load-path
	 lisp-directory
	 &optional unused mule-lisp-directory ffi-lisp-directory)
  "Construct the load path."
  (let* ((envvar-value (getenv "EMACSLOADPATH"))
	 (env-load-path
	  (and envvar-value
	       (paths-decode-directory-path envvar-value 'drop-empties)))
	 (mule-lisp-load-path
	  (and mule-lisp-directory
	       (paths-find-recursive-load-path (list mule-lisp-directory)
					       paths-mule-load-path-depth)))
 	 (ffi-lisp-load-path
 	  (and ffi-lisp-directory
	       (paths-find-recursive-load-path (list ffi-lisp-directory)
					       paths-ffi-load-path-depth)))
	 (lisp-load-path
	  (and lisp-directory
	       (paths-find-recursive-load-path (list lisp-directory)
					       paths-core-load-path-depth)))
	 (emod-load-path
	  (and module-directory
	       (paths-construct-module-load-path roots module-directory
						 site-module-directory)))
         (root-load-path
          (paths-find-recursive-load-path
           (mapcar #'(lambda (root)
                       (expand-file-name "lisp" root))
                   roots) 1)))

    (append env-load-path
	    emod-load-path
	    early-package-load-path
	    late-package-load-path
	    mule-lisp-load-path
	    ffi-lisp-load-path
	    lisp-load-path
            root-load-path
	    last-package-load-path)))

(defun paths-construct-info-path (roots early-packages late-packages last-packages)
  "Construct the info path."
  (let ((info-path-envval (getenv "INFOPATH")))
    (paths-uniq-append
     (append
      (list (paths-find-emacs-directory roots "share/" "info" nil configure-info-directory))
      (let ((info-directory
	     (or (paths-find-version-archindep-directory
                  roots "info" nil configure-info-directory)
                 (paths-find-version-archdep-directory
                  roots "info" nil configure-info-directory))))
	(and info-directory
	     (list info-directory)))
      (packages-find-package-info-path early-packages)
      (packages-find-package-info-path late-packages)
      (packages-find-package-info-path last-packages)
      (and info-path-envval
	   (paths-decode-directory-path info-path-envval 'drop-empties)))
     (and (null info-path-envval)
	  (paths-uniq-append
	   (paths-directories-which-exist configure-info-path)
	   (paths-directories-which-exist paths-default-info-directories))))))

(defun paths-find-doc-directory (roots)
  "Find the documentation directory."
  (cond
   ;; in-place from $blddir/src/.libs
   ((file-exists-p (paths-construct-path `(".." ,internal-doc-file-name)
					 (invocation-directory)))
    (expand-file-name "../" invocation-directory))
   ;; in-place from $blddir/src
   ((file-exists-p (expand-file-name internal-doc-file-name
				     (invocation-directory)))
    (invocation-directory))
   ;; installed
   (t
    (paths-find-architecture-directory roots nil nil
				       configure-doc-directory))))

(defun paths-find-exec-directory (roots)
  "Find the binary directory."
  (paths-find-architecture-directory roots "lib-src"
                                     nil configure-exec-directory))

(defun paths-construct-exec-path (roots exec-directory
				  early-packages late-packages last-packages)
  "Find the binary path."
  (append
   (let ((path-envval (getenv "PATH")))
     (if path-envval
	 (paths-decode-directory-path path-envval 'drop-empties)))
   (packages-find-package-exec-path early-packages)
   (packages-find-package-exec-path late-packages)
   (let ((emacspath-envval (getenv "EMACSPATH")))
     (and emacspath-envval
	  (split-path emacspath-envval)))
   (and exec-directory
	(list exec-directory))
   (packages-find-package-exec-path last-packages)))

(defun paths-find-data-directory (roots)
  "Find the data directory."
  ;; better approach wanted!!! -hroptatyr
  ;; for now we're just using the toolbar subdirectory as indicator
  ;; of a healthy etc/ directory
  (expand-file-name
   "../" (or
	  ;; in-place
	  (paths-find-version-archdep-directory
	   roots "etc/toolbar" "EMACSDATA" configure-data-directory)
	  ;; installed
	  (paths-find-version-archindep-directory
	   roots "etc/toolbar" "EMACSDATA" configure-data-directory))))

(defun paths-construct-data-directory-list (data-directory
					    early-packages late-packages last-packages)
  "Find the data path."
  (append
   (packages-find-package-data-path early-packages)
   (packages-find-package-data-path late-packages)
   (list data-directory)
   (packages-find-package-data-path last-packages)))

;;; setup-paths.el ends here
