;;; packages.el --- Low level support for XEmacs packages

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) 2002, 2003 Ben Wing.

;; Author: Steven L Baur <steve@xemacs.org>
;; Maintainer: Steven L Baur <steve@xemacs.org>
;; Keywords: internal, lisp, dumped

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

;; This file is dumped with XEmacs.

;; This file provides low level facilities for XEmacs startup --
;; particularly regarding the package setup.  This code has to run in
;; what we call "bare temacs" -- i.e. XEmacs without the usual Lisp
;; environment.  Pay special attention:

;; - not to use the `lambda' macro.  Use #'(lambda ...) instead.
;;   (this goes for any package loaded before `subr.el'.)
;;
;; - not to use macros, because they are not yet available (and this
;;   file must be loadable uncompiled.)  Built in macros, such as
;;   `when' and `unless' are fine, of course.
;;
;; - not to use `defcustom'.  If you must add user-customizable
;;   variables here, use `defvar', and add the variable to
;;   `cus-start.el'.

;; Because of all this, make sure that the stuff you put here really
;; belongs here.

;; This file requires find-paths.el.

;;; Code:

;;; Package versioning

(defvar packages-package-list nil
  "Database of installed packages and version numbers")

(defvar packages-hierarchy-depth 1
  "Depth of package hierarchies.")

(defvar packages-load-path-depth 1
  "Depth of load-path search in package hierarchies.")

(defvar packages-data-path-depth 1
  "Depth of data-path search in package hierarchies.")

(defvar early-packages nil
  "Packages early in the load path.")

(defvar early-package-load-path nil
  "Load path for packages early in the load path.")

(defvar late-packages nil
  "Packages late in the load path.")

(defvar late-package-load-path nil
  "Load path for packages late in the load path.")

(defvar last-packages nil
  "Packages last in the load path.")

(defvar last-package-load-path nil
  "Load path for packages last in the load path.")

(defun packages-compute-package-locations (user-init-directory)
  "Compute locations of the various package directories.
This is a list each of whose elements describes one directory.
A directory description is a three-element list.
The first element is either an absolute path or a subdirectory
in the XEmacs hierarchy.
The second component is one of the symbols EARLY, LATE, LAST,
depending on the load-path segment the hierarchy is supposed to
show up in.
The third component is a thunk which, if it returns NIL, causes
the directory to be ignored."
  (list
   (list (paths-construct-path (list user-init-directory "site-packages"))
	 'early #'(lambda () t))
   (list (paths-construct-path (list user-init-directory "infodock-packages"))
	 'early #'(lambda () (featurep 'infodock)))
   (list (paths-construct-path (list user-init-directory "mule-packages"))
	 'early #'(lambda () (featurep 'mule)))
   (list (paths-construct-path (list user-init-directory "xemacs-packages"))
	 'early #'(lambda () t))
   (list "site-packages"     'late  #'(lambda () t))
   (list "infodock-packages" 'late  #'(lambda () (featurep 'infodock)))
   (list "mule-packages"     'late  #'(lambda () (featurep 'mule)))
   (list "xemacs-packages"   'late  #'(lambda () t))))

(defun package-get-key-1 (info key)
  "Locate keyword `key' in list."
  (cond ((null info)
	 nil)
	((eq (car info) key)
	 (nth 1 info))
	(t (package-get-key-1 (cddr info) key))))

(defun package-get-key (name key)
  "Get info `key' from package `name'."
  (let ((info (assq name packages-package-list)))
    (when info
      (package-get-key-1 (cdr info) key))))

(defun package-provide (name &rest attributes)
  (let ((info (if (and attributes (floatp (car attributes)))
		  (list :version (car attributes))
		attributes)))
    (setq packages-package-list
	  (cons (cons name info) (remassq name packages-package-list)))))

(defun package-require (name version)
  (let ((pkg (assq name packages-package-list)))
    (cond ((null pkg)
	   (error 'invalid-state
		  (format "Package %s has not been loaded into this XEmacsen"
			  name)))
	  ((< (package-get-key name :version) version)
	   (error 'search-failed
		  (format "Need version %g of package %s, got version %g"
			  version name (package-get-key name :version))))
	  (t t))))

(defun package-delete-name (name)
  (let (pkg)
    ;; Delete ALL versions of package.
    ;; This is pretty memory-intensive, as we use copy-alist when deleting
    ;; package entries, to prevent side-effects in functions that call this
    ;; one.
    (while (setq pkg (assq name packages-package-list))
      (setq packages-package-list (delete pkg (copy-alist
					       packages-package-list))))))

;;; Build time stuff

(defvar autoload-file-name "auto-autoloads.el"
  "Filename that autoloads are expected to be found in.")

;; Moved from help.el.
;; Unlike the FSF version, our `locate-library' uses the `locate-file'
;; primitive, which should make it lightning-fast.

(defun locate-library (library &optional nosuffix path interactive-call)
  "Show the precise file name of Emacs library LIBRARY.
This command searches the directories in `load-path' like `M-x load-library'
to find the file that `M-x load-library RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `.elc' or `.el'
to the specified name LIBRARY.

If the optional third arg PATH is specified, that list of directories
is used instead of `load-path'."
  (interactive (list (read-library-name "Locate library: ")
                     nil nil
                     t))
  (let ((result
	 (locate-file
	  library
	  (or path load-path)
	  (cond ((or (rassq 'jka-compr-handler file-name-handler-alist)
		     (and (boundp 'find-file-hooks)
			  (member 'crypt-find-file-hook find-file-hooks)))
		 ;; Compression involved.
		 (if nosuffix
		     '("" ".gz" ".Z" ".bz2")
		   '(".elc" ".elc.gz" "elc.Z" ".elc.bz2"
		     ".el" ".el.gz" ".el.Z" ".el.bz2"
		     "" ".gz" ".Z" ".bz2")))
		(t
		 ;; No compression.
		 (if nosuffix
		     ""
		   '(".elc" ".el" "")))))))
    (and interactive-call
	 (if result
	     (message "Library is file %s" result)
	   (message "No library %s in search path" library)))
    result))

(defun packages-add-suffix (str)
  (if (null (string-match "\\.el\\'" str))
      (concat str ".elc")
    str))

(defun packages-list-autoloads-path ()
  "List autoloads from precomputed load-path."
  (let ((path load-path)
	autoloads)
    (while path
      (if (file-exists-p (concat (car path)
				 autoload-file-name))
	  (setq autoloads (cons (concat (car path)
					autoload-file-name)
				autoloads)))
      (setq path (cdr path)))
    autoloads))

(defun packages-list-autoloads (source-directory)
  "List autoload files in (what will be) the normal lisp search path.
This function is used during build to find where the global symbol files so
they can be perused for their useful information."
  (let ((files (directory-files (file-name-as-directory source-directory)
				t ".*"))
	file autolist)
    ;; (print (prin1-to-string source-directory))
    ;; (print (prin1-to-string files))
    (while (setq file (car-safe files))
      (if (and (file-directory-p file)
	       (file-exists-p (concat (file-name-as-directory file)
				      autoload-file-name)))
	  (setq autolist (cons (concat (file-name-as-directory file)
				       autoload-file-name)
			       autolist)))
      (setq files (cdr files)))
    autolist))

;; The following function cannot be called from a bare temacs
(defun packages-new-autoloads ()
  "Return autoloads files that have been added or modified since XEmacs dump."
  (require 'loadhist)
  (let ((me (concat invocation-directory invocation-name))
	(path load-path)
	result dir)
    (while path
      (setq dir (file-truename (car path)))
      (let ((autoload-file (file-name-sans-extension (concat
						      dir
						      autoload-file-name))))
	;; Check for:
	;; 1.  An auto-autoload file that hasn't provided a feature (because
	;;     it has been installed since XEmacs was dumped).
	;; 2.  auto-autoload.el being newer than the executable
	;; 3.  auto-autoload.elc being newer than the executable (the .el
	;;     could be missing or compressed)
	(when (or (and (null (file-provides autoload-file))
		       (or (file-exists-p (concat autoload-file ".elc"))
			   (file-exists-p (concat autoload-file ".el"))))
		  (and (file-newer-than-file-p (concat autoload-file ".el") me)
		       (setq autoload-file (concat autoload-file ".el")))
		  (and (file-newer-than-file-p (concat autoload-file
						       ".elc")
					       me)
		       (setq autoload-file (concat autoload-file ".elc"))))
	  (push autoload-file result)))
      (setq path (cdr path)))
    result))

;; The following function cannot be called from a bare temacs
(defun packages-reload-autoloads ()
  "Reload new or updated auto-autoloads files.
This is an extremely dangerous function to call after the user-init-files
is run.  Don't call it or you'll be sorry."
  (let ((autoload-list (packages-new-autoloads)))
    (while autoload-list
      (let* ((autoload-file (car autoload-list))
	     (feature (car-safe (file-provides autoload-file))))
	(when feature
	  ;; (message "(unload-feature %S)" feature)
	  (unload-feature feature))
	(condition-case nil
	    (load autoload-file)
	  (t nil)))
      (setq autoload-list (cdr autoload-list)))))

;; Data-directory is really a list now.  Provide something to search it for
;; directories.

(defun locate-data-directory-list (name &optional dir-list)
  "Locate the matching list of directories in a search path DIR-LIST.
If no DIR-LIST is supplied, it defaults to `data-directory-list'."
  (unless dir-list
    (setq dir-list data-directory-list))
  (let (found found-dir found-dir-list)
    (while dir-list
      (setq found (file-name-as-directory (concat (car dir-list) name))
	    found-dir (file-directory-p found))
      (and found-dir
	   (setq found-dir-list (cons found found-dir-list)))
      (setq dir-list (cdr dir-list)))
    (nreverse found-dir-list)))

;; Data-directory is really a list now.  Provide something to search it for
;; a directory.

(defun locate-data-directory (name &optional dir-list)
  "Locate a directory in a search path DIR-LIST (a list of directories).
If no DIR-LIST is supplied, it defaults to `data-directory-list'."
  (unless dir-list
    (setq dir-list data-directory-list))
  (let (found found-dir)
    (while (and (null found-dir) dir-list)
      (setq found (file-name-as-directory (concat (car dir-list) name))
	    found-dir (file-directory-p found))
      (or found-dir
	  (setq found nil))
      (setq dir-list (cdr dir-list)))
    found))

;; Data-directory is really a list now.  Provide something to search it for
;; files.

(defun locate-data-file (name &optional dir-list)
  "Locate a file in a search path DIR-LIST (a list of directories).
If no DIR-LIST is supplied, it defaults to `data-directory-list'.
This function is basically a wrapper over `locate-file'."
  (locate-file name (or dir-list data-directory-list)))

;; Path setup

(defun packages-find-package-directories (roots base)
  "Find a set of package directories."
  ;; make sure paths-find-version-directory and paths-find-site-directory
  ;; don't both pick up version-independent directories ...
  (let ((version-directory (paths-find-version-directory roots base nil nil t))
	(site-directory (paths-find-site-directory roots base)))
    (paths-uniq-append
     (and version-directory (list version-directory))
     (and site-directory (list site-directory)))))

(defvar packages-special-base-regexp "^\\(etc\\|info\\|man\\|lisp\\|lib-src\\|bin\\|pkginfo\\)$"
  "Special subdirectories of packages.")

(defvar packages-no-package-hierarchy-regexp
  (concat "\\(" paths-version-control-filename-regexp "\\)"
	  "\\|"
	  "\\(" packages-special-base-regexp "\\)")
  "Directories which can't be the roots of package hierarchies.")

(defun packages-find-packages-in-directories (directories)
  "Find all packages underneath directories in DIRECTORIES."
  (paths-find-recursive-path directories
			     packages-hierarchy-depth
			     packages-no-package-hierarchy-regexp))

(defun packages-split-path (path)
  "Split PATH at \"\", return pair with two components.
The second component is shared with PATH."
  (let ((reverse-tail '())
	(rest path))
    (while (and rest (null (string-equal "" (car rest))))
      (setq reverse-tail (cons (car rest) reverse-tail))
      (setq rest (cdr rest)))
    (if (null rest)
	(cons path nil)
      (cons (nreverse reverse-tail) (cdr rest)))))

(defun packages-split-package-path (package-path)
  "Split up PACKAGE-PATH into early, late and last components.
The separation is by \"\" components.
This returns (LIST EARLY-PACKAGES LATE-PACKAGES LAST-PACKAGES)."
  ;; When in doubt, it's late
  (let* ((stuff (packages-split-path package-path))
	 (early (and (cdr stuff) (car stuff)))
	 (late+last (or (cdr stuff) (car stuff)))
	 (stuff (packages-split-path late+last))
	 (late (car stuff))
	 (last (cdr stuff)))
    (list (packages-find-packages-in-directories early)
	  (packages-find-packages-in-directories late)
	  (packages-find-packages-in-directories last))))

(defun packages-deconstruct (list consumer)
  "Deconstruct LIST and feed it to CONSUMER."
  (apply consumer list))

(defun packages-find-packages-by-name (roots name)
  "Find a package hierarchy by its name."
  (packages-find-packages-in-directories
   (if (and (file-name-absolute-p name)
	    (file-name-directory (expand-file-name name)))
       (list (file-name-as-directory (expand-file-name name)))
    (packages-find-package-directories roots name))))

(defun packages-find-packages-at-time
  (roots package-locations time &optional default)
  "Find packages at given time.
For the format of PACKAGE-LOCATIONS, see the global variable of the same name.
TIME is either 'EARLY, 'LATE, or 'LAST.
DEFAULT is a default list of packages."
  (or default
      (let ((packages '()))
	(while package-locations
	  (packages-deconstruct
	   (car package-locations)
	   #'(lambda (name a-time thunk)
	       (if (and (eq time a-time)
			(funcall thunk))
		   (setq packages
			 (nconc packages
				(packages-find-packages-by-name roots name))))))
	  (setq package-locations (cdr package-locations)))
	packages)))

(defun packages-find-packages (roots package-locations)
  "Find the packages."
  (let ((envvar-value (getenv "EMACSPACKAGEPATH")))
    (if envvar-value
	(packages-split-package-path (paths-decode-directory-path envvar-value))
      (packages-deconstruct
       (packages-split-package-path configure-package-path)
       #'(lambda (configure-early-packages
		  configure-late-packages
		  configure-last-packages)
	   (list (packages-find-packages-at-time roots package-locations 'early
						 configure-early-packages)
		 (packages-find-packages-at-time roots package-locations 'late
						 configure-late-packages)
		 (packages-find-packages-at-time roots package-locations 'last
						 configure-last-packages)))))))

(defun packages-find-package-library-path (packages suffixes)
  "Construct a path into a component of the packages hierarchy.
PACKAGES is a list of package directories.
SUFFIXES is a list of names of package subdirectories to look for."
  (let ((directories
	 (apply
	  #'nconc
	  (mapcar #'(lambda (package)
		      (mapcar #'(lambda (suffix)
				  (file-name-as-directory (concat package suffix)))
			      suffixes))
		  packages))))
    (paths-directories-which-exist directories)))

(defun packages-find-package-load-path (packages)
  "Construct the load-path component for packages.
PACKAGES is a list of package directories."
  (paths-find-recursive-load-path
   (packages-find-package-library-path packages
				       '("lisp"))
   packages-load-path-depth))

(defun packages-find-package-exec-path (packages)
  "Construct the exec-path component for packages.
PACKAGES is a list of package directories."
  (packages-find-package-library-path packages
				      (list (paths-construct-path
					     (list "bin" system-configuration))
					    "lib-src")))

(defun packages-find-package-info-path (packages)
  "Construct the info-path component for packages.
PACKAGES is a list of package directories."
  (packages-find-package-library-path packages '("info")))

(defun packages-find-package-data-path (packages)
  "Construct the data-path component for packages.
PACKAGES is a list of package directories."
  (paths-find-recursive-load-path
   (packages-find-package-library-path packages
				       '("etc"))
   packages-data-path-depth))

;; Loading package initialization files

(defun packages-load-package-lisps (package-load-path base)
  "Load all Lisp files of a certain name along a load path.
BASE is the base name of the files."
  (mapcar #'(lambda (dir)
	    (let ((file-name (expand-file-name base dir)))
	      (condition-case error
		  (load file-name t t)
		(error
		 (warn (format "Autoload error in: %s:\n\t%s"
			       file-name
			       (with-output-to-string
				 (display-error error nil))))))))
	package-load-path))

(defun packages-load-package-auto-autoloads (package-load-path)
  "Load auto-autoload files along a load path."
  (packages-load-package-lisps package-load-path
			       (file-name-sans-extension autoload-file-name)))

(defun packages-handle-package-dumped-lisps (handle package-load-path)
  "Load dumped-lisp.el files along a load path.
Call HANDLE on each file off definitions of PACKAGE-LISP there."
  (mapcar #'(lambda (dir)
	    (let ((file-name (expand-file-name "dumped-lisp.el" dir)))
	      (if (file-exists-p file-name)
		  (let (package-lisp
			;; 20.4 packages could set this
			preloaded-file-list)
		    (load file-name)
		    ;; dumped-lisp.el could have set this ...
		    (if package-lisp
			(mapcar #'(lambda (base)
				  (funcall handle base))
			      package-lisp))))))
	package-load-path))

(defun packages-load-package-dumped-lisps (package-load-path)
  "Load dumped-lisp.el files along a load path.
Also load files off PACKAGE-LISP definitions there."
  (packages-handle-package-dumped-lisps #'load package-load-path))

(defun packages-collect-package-dumped-lisps (package-load-path)
  "Load dumped-lisp.el files along a load path.
Return list of files off PACKAGE-LISP definitions there."
  (let ((*files* '()))
    (packages-handle-package-dumped-lisps
     #'(lambda (file)
	 (setq *files* (cons file *files*)))
     package-load-path)
    (reverse *files*)))

(provide 'packages)

;;; packages.el ends here
