;;; package-admin.el --- Installation and Maintenance of XEmacs packages

;; Copyright (C) 1997 by Free Software Foundation, Inc.
;; Copyright (C) 2003, Steve Youngs.

;; Author: SL Baur <steve@xemacs.org>
;; Keywords: internal

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

;; First pass at lisp front end to package maintenance.

;;; Code:

(require 'config)

(defvar package-admin-xemacs (concat invocation-directory invocation-name)
  "Location of XEmacs binary to use.")

(defvar package-admin-temp-buffer "*Package Output*"
  "Temporary buffer where output of backend commands is saved.")

(defvar package-admin-install-function (if (eq system-type 'windows-nt)
					   'package-admin-install-function-mswindows
					 'package-admin-default-install-function)
  "The function to call to install a package.
Three args are passed: FILENAME PKG-DIR BUFFER
Install package FILENAME into directory PKG-DIR, with any messages output
to buffer BUFFER.")

(defvar package-admin-error-messages '(
				       "No space left on device"
				       "No such file or directory"
				       "Filename too long"
				       "Read-only file system"
				       "File too large"
				       "Too many open files"
				       "Not enough space"
				       "Permission denied"
				       "Input/output error"
				       "Out of memory"
				       "Unable to create directory"
				       "Directory checksum error"
				       "Cannot exclusively open file"
				       "corrupted file"
				       "incomplete .* tree"
				       "Bad table"
				       "corrupt input"
				       "invalid compressed data"
				       "too many leaves in Huffman tree"
				       "not a valid zip file"
				       "first entry not deflated or stored"
				       "encrypted file --"
				       "unexpected end of file"
				       )
  "Regular expressions of possible error messages.
After each package extraction, the `package-admin-temp-buffer' buffer is
scanned for these messages.  An error code is returned if one of these are
found.

This is awful, but it exists because error return codes aren't reliable
under MS Windows.")

(defvar package-admin-tar-filename-regexps
  '(
    ;; GNU tar:
    ;; drwxrwxr-x john/doe 123 1997-02-18 15:48 pathname
    "\\S-+\\s-+[-a-z0-9_/]+\\s-+[0-9]+\\s-+[-0-9]+\\s-+[0-9:]+\\s-+\\(\\S-.*\\)"
    ;; HP-UX & SunOS tar:
    ;; rwxrwxr-x 501/501    123 Feb 18 15:46 1997 pathname
    ;; Solaris tar (phooey!):
    ;; rwxrwxr-x501/501    123 Feb 18 15:46 1997 pathname
    ;; AIX tar:
    ;; -rw-r--r-- 147 1019   32919 Mar 26 12:00:09 1992 pathname
    "\\S-+\\s-*[-a-z0-9_]+[/ ][-a-z0-9_]+\\s-+[0-9]+\\s-+[a-z][a-z][a-z]\\s-+[0-9]+\\s-+[0-9:]+\\s-+[0-9]+\\s-+\\(\\S-.*\\)"

    ;; djtar:
    ;; drwx Aug 31 02:01:41 1998       123 pathname
    "\\S-+\\s-+[a-z][a-z][a-z]\\s-+[0-9]+\\s-+[0-9:]+\\s-+[0-9]+\\s-+[0-9]+\\s-+\\(\\S-.*\\)"

    )
  "List of regexps to use to search for tar filenames.
Note that \"\\(\" and \"\\)\" must be used to delimit the pathname (as
match #1).  Don't put \"^\" to match the beginning of the line; this
is already implicit, as `looking-at' is used.  Filenames can,
unfortunately, contain spaces, so be careful in constructing any
regexps.")

(defvar package-install-hook nil
  "*List of hook functions to be called when a new package is successfully
installed. The hook function is passed two arguments: the package name, and
the install directory.")

(defvar package-delete-hook nil
  "*List of hook functions to be called when a package is deleted. The
hook is called *before* the package is deleted. The hook function is passed
two arguments: the package name, and the install directory.")

(defun package-admin-install-function-mswindows (file pkg-dir buffer)
  "Install function for mswindows."
  (let ((default-directory (file-name-as-directory pkg-dir)))
    (unless (file-directory-p default-directory)
      (make-directory default-directory t))
    (call-process "minitar" nil buffer t file)))

(defun package-admin-default-install-function (filename pkg-dir buffer)
  "Default function to install a package.
Install package FILENAME into directory PKG-DIR, with any messages output
to BUFFER."
  (let* ((pkg-dir (file-name-as-directory pkg-dir))
	 (default-directory pkg-dir)
	 (filename (expand-file-name filename)))
    (unless (file-directory-p pkg-dir)
      (make-directory pkg-dir t))
    ;; Don't assume GNU tar.
    (if (shell-command (concat "gunzip -c " filename " | tar xvf -") buffer)
	0
      1)))

;; A few things needed by the following 2 functions.
(eval-when-compile
  (require 'packages)
  (autoload 'package-get-info "package-get")
  (autoload 'paths-decode-directory-path "find-paths")
  (defvar package-get-install-to-user-init-directory))

(defun package-admin-find-top-directory (type &optional user-dir)
  "Return the top level directory for a package.

Argument TYPE is a symbol that determines the type of package we're
trying to find a directory for.

Optional Argument USER-DIR if non-nil use directories off
`user-init-directory'.  This overrides everything except
\"EMACSPACKAGEPATH\".

This function honours the environment variable \"EMACSPACKAGEPATH\"
and returns directories found there as a priority.  If that variable
doesn't exist and USER-DIR is nil, check in the normal places.

If we still can't find a suitable directory, return nil.

Possible values for TYPE are:

    std  == For \"standard\" packages that go in '/xemacs-packages/'
    mule == For \"mule\" packages that go in '/mule-packages/'
    site == For \"unsupported\" packages that go in '/site-packages/'

Note:  Type \"site\" is not yet fully supported."
  (let* ((env-value (getenv "EMACSPACKAGEPATH"))
	 top-dir)
    ;; First, check the environment var.
    (if env-value
	(let ((path-list (paths-decode-directory-path env-value 'drop-empties)))
	  (cond ((eq type 'std)
		 (while path-list
		   (if (equal (file-name-nondirectory 
			       (directory-file-name (car path-list)))
			      "xemacs-packages")
		       (setq top-dir (car path-list)))
		   (setq path-list (cdr path-list))))
		((eq type 'mule)
		 (while path-list
		   (if (equal (file-name-nondirectory 
			       (directory-file-name (car path-list)))
			      "mule-packages")
		       (setq top-dir (car path-list)))
		   (setq path-list (cdr path-list)))))))
    ;; Wasn't in the environment, try `user-init-directory' if
    ;; USER-DIR is non-nil.
    (if (and user-dir
	     (not top-dir))
	(cond ((eq type 'std)
	       (setq top-dir (file-name-as-directory
			      (expand-file-name "xemacs-packages" user-init-directory))))
	      ((eq type 'mule)
	       (setq top-dir (file-name-as-directory
			      (expand-file-name "mule-packages" user-init-directory))))))
    ;; Finally check the normal places
    (if (not top-dir)
	(let ((path-list (nth 1 (packages-find-packages
				 emacs-roots
				 (packages-compute-package-locations user-init-directory)))))
	  (cond ((eq type 'std)
		 (while path-list
		   (if (equal (file-name-nondirectory 
			       (directory-file-name (car path-list)))
			      "xemacs-packages")
		       (setq top-dir (car path-list)))
		   (setq path-list (cdr path-list))))
		((eq type 'mule)
		 (while path-list
		   (if (equal (file-name-nondirectory 
			       (directory-file-name (car path-list)))
			      "mule-packages")
		       (setq top-dir (car path-list)))
		   (setq path-list (cdr path-list)))))))
    ;; Now return either the directory or nil.
    top-dir))

(defun package-admin-get-install-dir (package &optional pkg-dir)
  "Find a suitable installation directory for a package.

Argument PACKAGE is the package to find a installation directory for.
Optional Argument PKG-DIR, if non-nil is a directory to use for
installation.

If PKG-DIR is non-nil and writable, return that.  Otherwise check to
see if the PACKAGE is already installed and return that location, if
it is writable.  Finally, fall back to the `user-init-directory' if
all else fails.  As a side effect of installing packages under
`user-init-directory' these packages become part of `early-packages'."
  ;; If pkg-dir specified, return that if writable.
  (if (and pkg-dir
	   (file-writable-p (directory-file-name pkg-dir)))
      pkg-dir
    ;; If the user want her packages under ~/.xemacs/, do so.
    (let ((type (package-get-info package 'category)))
      (if package-get-install-to-user-init-directory
	  (progn
	    (cond ((equal type "standard")
		   (setq pkg-dir (package-admin-find-top-directory 'std 'user-dir)))
		  ((equal type "mule")
		   (setq pkg-dir (package-admin-find-top-directory 'mule 'user-dir))))
	    pkg-dir)
	;; Maybe the package has been installed before, if so, return
	;; that directory.
	(let ((package-feature (intern-soft (concat
					     (symbol-name package) "-autoloads")))
	      autoload-dir)
	  (when (and (not (eq package 'unknown))
		     (featurep package-feature)
		     (setq autoload-dir (feature-file package-feature))
		     (setq autoload-dir (file-name-directory autoload-dir))
		     (member autoload-dir (append early-package-load-path late-package-load-path)))
	    ;; Find the corresponding entry in late-package
	    (setq pkg-dir
		  (car-safe (member-if (lambda (h)
					 (string-match (concat "^" (regexp-quote h))
						       autoload-dir))
				       (append (cdr early-packages) late-packages)))))
	  (if (and pkg-dir
		   (file-writable-p (directory-file-name pkg-dir)))
	      pkg-dir
	    ;; OK, the package hasn't been previously installed so we need
	    ;; to guess where it should go.
	    (cond ((equal type "standard")
		   (setq pkg-dir (package-admin-find-top-directory 'std)))
		  ((equal type "mule")
		   (setq pkg-dir (package-admin-find-top-directory 'mule)))
		  (t
		   (error 'invalid-operation
			  "Invalid package type")))
	    (if (and pkg-dir
		     (file-writable-p (directory-file-name pkg-dir)))
		pkg-dir
	      ;; Oh no!  Either we still haven't found a suitable
	      ;; directory, or we can't write to the one we did find.
	      ;; Drop back to the `user-init-directory'.
	      (if (y-or-n-p (format "Directory isn't writable, use %s instead? "
				    user-init-directory))
		  (progn
		    (cond ((equal type "standard")
			   (setq pkg-dir (package-admin-find-top-directory 'std 'user-dir)))
			  ((equal type "mule")
			   (setq pkg-dir (package-admin-find-top-directory 'mule 'user-dir)))
			  (t
			   (error 'invalid-operation
				  "Invalid package type")))
		    ;; Turn on `package-get-install-to-user-init-directory'
		    ;; so we don't get asked for each package we try to
		    ;; install in this session.
		    (setq package-get-install-to-user-init-directory t)
		    pkg-dir)
		;; If we get to here XEmacs can't make up its mind and
		;; neither can the user, nothing left to do except barf. :-(
		(error 'search-failed
		       (format
			"Can't find suitable installation directory for package: %s" 
			package))))))))))

(defun package-admin-get-manifest-file (pkg-topdir package)
  "Return the name of the MANIFEST file for package PACKAGE.
Note that PACKAGE is a symbol, and not a string."
  (let ((dir (file-name-as-directory
	      (expand-file-name "pkginfo" pkg-topdir))))
    (expand-file-name (concat "MANIFEST." (symbol-name package)) dir)))

(defun package-admin-check-manifest (pkg-outbuf pkg-topdir)
  "Check for a MANIFEST.<package> file in the package distribution.
If it doesn't exist, create and write one.
PKG-OUTBUF is the buffer that holds the output from `tar', and PKG-TOPDIR
is the top-level directory under which the package was installed."
  (let ((manifest-buf " *pkg-manifest*")
	(old-case-fold-search case-fold-search)
	regexp package-name pathname regexps)
    (unwind-protect
	(save-excursion				;; Probably redundant.
	  (set-buffer (get-buffer pkg-outbuf))	;; Probably already the current buffer.
	  (goto-char (point-min))

	  ;; Make filenames case-insensitive, if necessary
	  (if (eq system-type 'windows-nt)
	      (setq case-fold-search t))

	  (setq regexp (concat "\\bpkginfo" 
			       (char-to-string directory-sep-char)
			       "MANIFEST\\...*"))

	  ;; Look for the manifest.
	  (if (not (re-search-forward regexp nil t))
	      (progn
		;; We didn't find a manifest.  Make one.

		;; Yuk.  We weren't passed the package name, and so we have
		;; to dig for it.  Look for it as the subdirectory name below
		;; "lisp", or "man".
		;; Here, we don't use a single regexp because we want to search
		;; the directories for a package name in a particular order.
		(if (catch 'done
		      (let ((dirs '("lisp" "man")) 
			    rexp)
			(while dirs
			  (setq rexp (concat "\\b" (car dirs)
					     "[\\/]\\([^\\/]+\\)[\//]"))
			  (if (re-search-forward rexp nil t)
			      (throw 'done t))
			  (setq dirs (cdr dirs)))))
		    (progn
		      (setq package-name (buffer-substring (match-beginning 1)
							   (match-end 1)))

		      ;; Get and erase the manifest buffer
		      (setq manifest-buf (get-buffer-create manifest-buf))
		      (buffer-disable-undo manifest-buf)
		      (erase-buffer manifest-buf)

		      ;; Now, scan through the output buffer, looking for
		      ;; file and directory names.
		      (goto-char (point-min))
		      ;; for each line ...
		      (while (< (point) (point-max))
			(beginning-of-line)
			(setq pathname nil)

			;; scan through the regexps, looking for a pathname
			(if (catch 'found-path
			      (setq regexps package-admin-tar-filename-regexps)
			      (while regexps
				(if (looking-at (car regexps))
				    (progn
				      (setq pathname
					    (buffer-substring
					     (match-beginning 1)
					     (match-end 1)))
				      (throw 'found-path t)))
				(setq regexps (cdr regexps))))
			    (progn
			      ;; found a pathname -- add it to the manifest
			      ;; buffer
			      (save-excursion
				(set-buffer manifest-buf)
				(goto-char (point-max))
				(insert pathname "\n"))))
			(forward-line 1))

		      ;; Processed all lines.
		      ;; Now, create the file, pkginfo/MANIFEST.<pkgname>

		      ;; We use `expand-file-name' instead of `concat',
		      ;; for portability.
		      (setq pathname (expand-file-name "pkginfo"
						       pkg-topdir))
		      ;; Create pkginfo, if necessary
		      (if (not (file-directory-p pathname))
			  (make-directory pathname))
		      (setq pathname (expand-file-name
				      (concat "MANIFEST." package-name)
				      pathname))
		      (save-excursion
			(set-buffer manifest-buf)
			;; Put the files in sorted order
			(if (fboundp 'sort-lines)
			    (sort-lines nil (point-min) (point-max))
			  (warn "`xemacs-base' not installed, MANIFEST.%s not sorted"
				package-name))
			;; Write the file.
			;; Note that using `write-region' *BYPASSES* any check
			;; to see if XEmacs is currently editing/visiting the
			;; file.
			(write-region (point-min) (point-max) pathname))
		      (kill-buffer manifest-buf))))))
      ;; Restore old case-fold-search status
      (setq case-fold-search old-case-fold-search))))

;;;###autoload
(defun package-admin-add-binary-package (file &optional pkg-dir)
  "Install a pre-bytecompiled XEmacs package into package hierarchy."
  (interactive "fPackage tarball: ")
  (let ((buf (get-buffer-create package-admin-temp-buffer))
	(status 1)
	start err-list)
    (setq pkg-dir (package-admin-get-install-dir 'unknown pkg-dir))
    ;; Ensure that the current directory doesn't change
    (save-excursion
      (set-buffer buf)
      ;; This is not really needed
      (setq default-directory (file-name-as-directory pkg-dir))
      (setq case-fold-search t)
      (buffer-disable-undo)
      (goto-char (setq start (point-max)))
      (if (= 0 (setq status (funcall package-admin-install-function
				     file pkg-dir buf)))
	  (progn
	    ;; First, check for errors.
	    ;; We can't necessarily rely upon process error codes.
	    (catch 'done
	      (goto-char start)
	      (setq err-list package-admin-error-messages)
	      (while err-list
		(if (re-search-forward (car err-list) nil t)
		    (progn
		      (setq status 1)
		      (throw 'done nil)))
		(setq err-list (cdr err-list))))
	    ;; Make sure that the MANIFEST file exists
	    (package-admin-check-manifest buf pkg-dir))))
    status))

(defun package-admin-rmtree (directory)
  "Delete a directory and all of its contents, recursively.
This is a feeble attempt at making a portable rmdir."
  (setq directory (file-name-as-directory directory))
  (let ((files (directory-files directory nil nil nil t))
        (dirs (directory-files directory nil nil nil 'dirs)))
    (while dirs
      (if (not (member (car dirs) '("." "..")))
          (let ((dir (expand-file-name (car dirs) directory)))
            (condition-case err
                (if (file-symlink-p dir) ;; just in case, handle symlinks
                    (delete-file dir)
                  (package-admin-rmtree dir))
              (file-error
               (message "%s: %s: \"%s\"" (nth 1 err) (nth 2 err) (nth 3 err)))))
        (setq dirs (cdr dirs))))
    (while files
      (condition-case err
          (delete-file (expand-file-name (car files) directory))
        (file-error
         (message "%s: %s: \"%s\"" (nth 1 err) (nth 2 err) (nth 3 err))))
      (setq files (cdr files)))
    (condition-case err
        (delete-directory directory)
      (file-error
       (message "%s: %s: \"%s\"" (nth 1 err) (nth 2 err) (nth 3 err))))))

(defun package-admin-get-lispdir  (pkg-topdir package)
  (let (package-lispdir)
    (if (and (setq package-lispdir (expand-file-name "lisp" pkg-topdir))
	     (setq package-lispdir (expand-file-name (symbol-name package)
						     package-lispdir))
	     (file-accessible-directory-p package-lispdir))
	package-lispdir)))

(defun package-admin-delete-binary-package (package pkg-topdir)
  "Delete a binary installation of PACKAGE below directory PKG-TOPDIR.
PACKAGE is a symbol, not a string."
  (let (manifest-file package-lispdir dirs file)
    (setq pkg-topdir (package-admin-get-install-dir package pkg-topdir))
    (setq manifest-file (package-admin-get-manifest-file pkg-topdir package))
    (run-hook-with-args 'package-delete-hook package pkg-topdir)
    (if (file-exists-p manifest-file)
	(progn
	  ;; The manifest file exists!  Use it to delete the old distribution.
	  (message "Removing old files for package \"%s\" ..." package)
	  (sit-for 0)
	  (with-temp-buffer
	    (buffer-disable-undo)
	    (erase-buffer)
	    (insert-file-contents manifest-file)
	    (goto-char (point-min))

	    ;; For each entry in the MANIFEST ...
	    (while (< (point) (point-max))
	      (beginning-of-line)
	      (setq file (expand-file-name (buffer-substring
					    (point)
					    (point-at-eol))
					   pkg-topdir))
	      (if (file-directory-p file)
		  ;; Keep a record of each directory
		  (setq dirs (cons file dirs))
		  ;; Delete each file.
		  ;; Make sure that the file is writable.
		  ;; (This is important under MS Windows.)
		  ;; I do not know why it important under MS Windows but
		  ;;    1. It bombs out when the file does not exist. This can be condition-cased
		  ;;    2. If I removed the write permissions, I do not want XEmacs to just ignore them.
		  ;;       If it wants to, XEmacs may ask, but that is about all
		  ;; (set-file-modes file 438) ;; 438 -> #o666
		  ;; Note, user might have removed the file!
		(condition-case ()
		    (delete-file file)
		  (error nil)))		;; We may want to turn the error into a Warning?
	      (forward-line 1))

	    ;; Delete empty directories.
	    (if dirs
		(progn
		  (mapc
		   (lambda (dir)
		     (condition-case ()
			 (delete-directory dir)))
		   dirs)))
	  ;; Delete the MANIFEST file
	  ;; (set-file-modes manifest-file 438) ;; 438 -> #o666
	  ;; Note. Packages can have MANIFEST in MANIFEST.
	  (condition-case ()
	      (delete-file manifest-file)
	    (error nil)) ;; Do warning?
	  (message "Removing old files for package \"%s\" ... done" package)))
      ;; The manifest file doesn't exist.  Fallback to just deleting the
      ;; package-specific lisp directory, if it exists.
      ;;
      ;; Delete old lisp directory, if any
      ;; Gads, this is ugly.  However, we're not supposed to use `concat'
      ;; in the name of portability.
      (setq package-lispdir (package-admin-get-lispdir pkg-topdir package))
      (when package-lispdir
	(message "Removing old lisp directory \"%s\" ..." package-lispdir)
	(sit-for 0)
	(package-admin-rmtree package-lispdir)
	(message "Removing old lisp directory \"%s\" ... done" package-lispdir)))
    ;; Delete the package from the database of installed packages.
    (package-delete-name package)))

(provide 'package-admin)

;;; package-admin.el ends here
