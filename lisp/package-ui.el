;;; package-ui.el ---

;; Copyright (C) 1998 by Darryl Okahata

;; Author: Darryl Okahata <darrylo@sr.hp.com>
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

(require 'package-get)		;; which, in turn, requires 'package-admin

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-changeable variables:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup pui nil
  "Convenient interface to the package system."
  :group 'package-tools
  :tag "Package User interface"
  :prefix "pui-")

(defcustom pui-package-install-dest-dir nil
  "*If non-nil (Automatic) path to package tree to install packages in.
Otherwise, use old path for installed packages and make a guess for
new ones."
  :group 'pui
  :tag "Install Location"
  :type '(choice (const :tag "Automatic" nil)
		 (directory)))
		 
(defcustom pui-list-verbose t
  "*If non-nil, display verbose info in the package list buffer."
  :group 'pui
  :tag "Verbose Listing"
  :type 'boolean)

(defcustom pui-up-to-date-package-face nil
  "*The face to use for packages that are up-to-date."
  :group 'pui
  :type 'face)

(defcustom pui-selected-package-face 'bold
  "*The face to use for selected packages.
Set this to `nil' to use the `default' face."
  :group 'pui
  :type 'face)

(defcustom pui-deleted-package-face 'blue
  "*The face to use for packages marked for removal.
Set this to `nil' to use the `default' face."
  :group 'pui
  :type 'face)

(defcustom pui-outdated-package-face 'red
  "*The face to use for outdated packages.
Set this to `nil' to use the `default' face."
  :group 'pui
  :type 'face)

(defcustom pui-uninstalled-package-face 'italic
  "*The face to use for uninstalled packages.
Set this to `nil' to use the `default' face."
   :group 'pui
   :type 'face)
   
(defcustom pui-info-buffer "*Packages*"
  "*Buffer to use for displaying package information."
  :group 'pui
  :type 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of user-changeable variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar pui-selected-packages nil
  "The list of user-selected packages to install.")

(defvar pui-deleted-packages nil
  "The list of user-selected packages to remove.")

(defvar pui-actual-package "")

(defvar pui-display-keymap
  (let ((m (make-keymap)))
    (suppress-keymap m)
    (set-keymap-name m 'pui-display-keymap)
    (define-key m "q" 'pui-quit)
    (define-key m "g" 'pui-list-packages)
    (define-key m "i" 'pui-display-info)
    (define-key m "m" 'pui-display-maintainer)
    (define-key m "?" 'describe-mode)
    (define-key m "v" 'pui-toggle-verbosity-redisplay)
    (define-key m "d" 'pui-toggle-package-delete-key)
    (define-key m "D" 'pui-toggle-package-delete-key)
    (define-key m [return] 'pui-toggle-package-key)
    (define-key m "x" 'pui-install-selected-packages)
    (define-key m "I" 'pui-install-selected-packages)
    (define-key m "r" 'pui-add-required-packages)
    (define-key m "n" 'next-line)
    (define-key m "+" 'pui-toggle-package-key)
    (define-key m "p" 'previous-line)
    (define-key m " " 'scroll-up-command)
    (define-key m [delete] 'scroll-down-command)
    m)
  "Keymap to use in the `pui-info-buffer' buffer")

(defvar pui-package-keymap
  (let ((m (make-sparse-keymap)))
    (set-keymap-name m 'pui-package-keymap)
    (define-key m 'button2 'pui-toggle-package-event)
;; We use a popup menu    
    (define-key m 'button3 'pui-popup-context-sensitive)
    m)
  "Keymap to use over package names/descriptions.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of variables


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration routines

;;;###autoload
(defun package-ui-add-site (site)
  "Add site to package-get-remote and possibly offer to update package list."
  (let ((had-none (null package-get-remote)))
    (setq package-get-remote site)    
    (when (and had-none package-get-was-current
	       (y-or-n-p "Update Package list?"))
      (setq package-get-was-current nil)
      (package-get-require-base t)
      (if (get-buffer pui-info-buffer)
	  (save-window-excursion
	    (pui-list-packages))))
    (set-menubar-dirty-flag)))

;;;###autoload
(defun package-ui-download-menu ()
  "Build the `Add Download Site' menu."
  (mapcar (lambda (site)
  	    (vector (car site)
  		    `(if (equal package-get-remote (quote ,(cdr site)))
 		      (setq package-get-remote nil)
 		      (package-ui-add-site (quote ,(cdr site))))
		    ;; I've used radio buttons so that only a single
		    ;; site can be selected, but they are in fact
		    ;; toggles.  SY.
  		    :style 'radio
  		    :selected `(equal package-get-remote (quote ,(cdr site)))))
  	  package-get-download-sites))

;;;###autoload
(defun package-ui-pre-release-download-menu ()
  "Build the 'Pre-Release Download Sites' menu."
  (mapcar (lambda (site)
  	    (vector (car site)
  		    `(if (equal package-get-remote (quote ,(cdr site)))
 		      (setq package-get-remote nil)
 		      (package-ui-add-site (quote ,(cdr site))))
		    ;; I've used radio buttons so that only a single
		    ;; site can be selected, but they are in fact
		    ;; toggles.  SY.
  		    :style 'radio
  		    :selected `(equal package-get-remote (quote ,(cdr site)))))
  	  package-get-pre-release-download-sites))

;;;###autoload
(defun package-ui-site-release-download-menu ()
  "Build the 'Site Release Download Sites' menu."
  (mapcar (lambda (site)
  	    (vector (car site)
  		    `(if (equal package-get-remote (quote ,(cdr site)))
 		      (setq package-get-remote nil)
 		      (package-ui-add-site (quote ,(cdr site))))
		    ;; I've used radio buttons so that only a single
		    ;; site can be selected, but they are in fact
		    ;; toggles.  SY.
  		    :style 'radio
  		    :selected `(equal package-get-remote (quote ,(cdr site)))))
  	  package-get-site-release-download-sites))

;;;###autoload
(defun pui-set-local-package-get-directory ()
  "Set a new package binary directory in `package-get-remote'.
Note that no provision is made for saving any changes made by this function.
It exists mainly as a convenience for one-time package installations from
disk."
  (interactive) 
  (let ((dir (read-directory-name
	      "New package binary directory to add? "
	      nil nil t)))
    (setq package-get-remote (list nil dir))
    (message "Package directory \"%s\" added." dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package list/installer routines

(defun pui-quit ()
  (interactive)
  (kill-buffer nil))

(defun pui-package-symbol-char (pkg-sym version)
  (progn
    (if (package-get-info-find-package packages-package-list pkg-sym)
        (let ((installed (package-get-key pkg-sym :version)))
          (if (>= (if (stringp installed)
                      (string-to-number installed)
                    installed)
                  (if (stringp version)
                      (string-to-number version)
                    version))
              (list " " pui-up-to-date-package-face)
            (list "*" pui-outdated-package-face)))
      (list "-" pui-uninstalled-package-face))))

(defun pui-update-package-display (extent &optional pkg-sym version)
  "Update the package status for EXTENT.
If PKG-SYM or VERSION are not given, they are read from the extent.
These are used to determine whether or not the package is installed,
and whether or not it is up-to-date."
  (let (buffer-read-only disp sym-char)
    (if (not pkg-sym)
	(setq pkg-sym (extent-property extent 'pui-package)))
    (if (not version)
	(setq version (package-get-info-prop (extent-property extent 'pui-info)
					     'version)))
    (cond ((member pkg-sym pui-selected-packages)
	     (if pui-selected-package-face
		 (set-extent-face extent (get-face pui-selected-package-face))
	       (set-extent-face extent (get-face 'default)))
	     (setq sym-char "+"))
	  ((member pkg-sym pui-deleted-packages)
	   (if pui-deleted-package-face
		 (set-extent-face extent (get-face pui-deleted-package-face))
	       (set-extent-face extent (get-face 'default)))
	     (setq sym-char "D"))
	  (t
	   (setq disp (pui-package-symbol-char pkg-sym version))
	   (setq sym-char (car disp))
	   (if (car (cdr disp))
	       (set-extent-face extent (get-face (car (cdr disp))))
	     (set-extent-face extent (get-face 'default)))))
    (save-excursion
      (goto-char (extent-start-position extent))
      (delete-char 1)
      (insert sym-char)
      (set-buffer-modified-p nil))))

(defun pui-toggle-package (extent)
  (let (pkg-sym)
    (setq pkg-sym (extent-property extent 'pui-package))
    (if (member pkg-sym pui-selected-packages)
	(setq pui-selected-packages
	      (delete pkg-sym pui-selected-packages))
      (setq pui-selected-packages
	    (cons pkg-sym pui-selected-packages))
      (setq pui-deleted-packages
	    (delete pkg-sym pui-deleted-packages)))
    (pui-update-package-display extent pkg-sym)))

(defun pui-toggle-package-key ()
  "Select/unselect package for installation, using the keyboard."
  (interactive)
  (let (extent)
    (if (setq extent (extent-at (point) (current-buffer) 'pui))
	(progn
	  (pui-toggle-package extent)
	  (forward-line 1))
      (error 'invalid-operation
	     "No package under cursor!"))))

(defun pui-toggle-package-delete (extent)
  (let (pkg-sym)
    (setq pkg-sym (extent-property extent 'pui-package))
    (if (member pkg-sym pui-deleted-packages)
	(setq pui-deleted-packages
	      (delete pkg-sym pui-deleted-packages))
      (setq pui-deleted-packages
	    (cons pkg-sym pui-deleted-packages))
      (setq pui-selected-packages
	    (delete pkg-sym pui-selected-packages)))
    (pui-update-package-display extent pkg-sym)))
  

(defun pui-toggle-package-delete-key ()
  "Select/unselect package for removal, using the keyboard."
  (interactive)
  (let (extent)
    (if (setq extent (extent-at (point) (current-buffer) 'pui))
	(progn
	  (pui-toggle-package-delete extent)
	  (forward-line 1))
      (error 'invalid-operation
	     "No package under cursor!"))))

(defun pui-current-package ()
  (let ((extent (extent-at (point) (current-buffer) 'pui)))
    (if extent
	(extent-property extent 'pui-package))))

(defun pui-toggle-package-event (event)
  "Select/unselect package for installation, using the mouse."
  (interactive "e")
  (let* ((ep (event-point event))
	 (buffer (window-buffer (event-window event)))
	 (extent (extent-at ep buffer 'pui-package)))
    (pui-toggle-package extent)))

(defun pui-toggle-verbosity-redisplay ()
  "Toggle verbose package info."
  (interactive)
  (progn
    (setq pui-list-verbose (not pui-list-verbose))
    (pui-list-packages)))

(defun pui-install-selected-packages ()
  "Install selected packages."
  (interactive)
  (let ((tmpbuf "*Packages-To-Remove*") 
	do-delete)
    (when pui-deleted-packages
      (save-window-excursion
	(with-output-to-temp-buffer tmpbuf
	  (display-completion-list (sort
				    (mapcar #'symbol-name pui-deleted-packages)
				    #'string<)
				   :activate-callback nil
				   :help-string "Packages selected for removal:\n"
				   :completion-string t))
	(setq tmpbuf (get-buffer-create tmpbuf))
	(display-buffer tmpbuf)
	(setq do-delete (yes-or-no-p "Remove these packages? "))
	(kill-buffer tmpbuf))	    
      (when do-delete
	(message "Deleting selected packages ...") (sit-for 0)
	(mapcar (lambda (pkg)
		  (package-admin-delete-binary-package
		   pkg (package-admin-get-install-dir pkg)))
		(nreverse pui-deleted-packages))
	(message "Packages deleted"))))
	 
  (let ((tmpbuf "*Packages-To-Install*") 
	do-install)
    (if pui-selected-packages
	(progn
	  ;; Don't change window config when asking the user if he really
	  ;; wants to install the packages.  We do this to avoid messing up
	  ;; the window configuration if errors occur (we don't want to
	  ;; display random buffers in addition to the error buffer, if
	  ;; errors occur, which would normally be caused by display-buffer).
	  (save-window-excursion
	    (with-output-to-temp-buffer tmpbuf
	      (display-completion-list
	       (sort (mapcar #'symbol-name pui-selected-packages) #'string<)
	       :activate-callback nil
	       :help-string "Packages selected for installation:\n"
	       :completion-string t))
	    (setq tmpbuf (get-buffer-create tmpbuf))
	    (display-buffer tmpbuf)
	    (setq do-install (y-or-n-p "Install these packages? "))
	    (kill-buffer tmpbuf))
	  (if do-install
	      (progn
		(save-excursion
		  ;; Clear old temp buffer history
		  (set-buffer (get-buffer-create package-admin-temp-buffer))
		  (buffer-disable-undo package-admin-temp-buffer)
		  (erase-buffer package-admin-temp-buffer))
		(message "Installing selected packages ...") (sit-for 0)
		(if (catch 'done
		      (mapcar (lambda (pkg)
				(if (not (package-get pkg nil nil
                                                      pui-package-install-dest-dir))
				    (throw 'done nil)))
			      (nreverse pui-selected-packages))
		      t)
		    (progn
		      (pui-list-packages)
		      (message "Packages installed"))))
	    (clear-message)))
      (if pui-deleted-packages
	  (pui-list-packages)
	(error 'invalid-operation
	       "No packages have been selected!")))
    ;; sync with windows type systems
    (package-net-update-installed-db)))

(defun pui-add-required-packages ()
  "Select packages required by those already selected for installation."
  (interactive)
  (let ((tmpbuf "*Required-Packages*") do-select)
    (if pui-selected-packages
	(let ((dependencies
               (delq nil (mapcar
                          (lambda (pkg)
                            (let ((installed
                                   (package-get-key pkg :version))
                                  (current
                                   (package-get-info-prop
                                    (package-get-info-version
                                     (package-get-info-find-package
                                      package-get-base pkg) nil)
                                    'version)))
                              (if (or (null installed)
                                     (< (if (stringp installed)
                                         (string-to-number installed)
                                       installed)
                                     (if (stringp current)
                                         (string-to-number current)
                                       current)))
                                  pkg
                                nil)))
                          (package-get-dependencies pui-selected-packages)))))
	  ;; Don't change window config when asking the user if he really
	  ;; wants to add the packages.  We do this to avoid messing up
	  ;; the window configuration if errors occur (we don't want to
	  ;; display random buffers in addition to the error buffer, if
	  ;; errors occur, which would normally be caused by display-buffer).
	  (save-window-excursion
	    (with-output-to-temp-buffer tmpbuf
	      (display-completion-list (sort
					(mapcar #'(lambda (pkg)
                                                    (symbol-name pkg))
						dependencies)
					'string<)
				       :activate-callback nil
				       :help-string "Required packages:\n"
				       :completion-string t))
	    (setq tmpbuf (get-buffer-create tmpbuf))
	    (display-buffer tmpbuf)
	    (setq do-select (y-or-n-p "Select these packages? "))
	    (kill-buffer tmpbuf))
	  (if do-select
              (progn
                (setq pui-selected-packages
                      (union pui-selected-packages dependencies))
                (map-extents #'(lambda (extent maparg)
                                 (pui-update-package-display extent))
                             nil nil nil nil nil 'pui)
                (message "added dependencies"))
	      (clear-message)))
      (error 'invalid-operation
	     "No packages have been selected!"))))

(defun pui-help-echo (extent &optional force-update)
  "Display additional package info in the modeline.
EXTENT determines the package to display (the package information is
attached to the extent as properties)."
  (let (pkg-sym info inst-ver inst-auth-ver auth-ver date maintainer balloon req)
    (if (or force-update (not (current-message))
	    (string-match ".*: .*: " (current-message)))
	(progn
	  (setq pkg-sym (extent-property extent 'pui-package)
		info (extent-property extent 'pui-info)
		inst-ver (package-get-key pkg-sym :version)
		inst-auth-ver (package-get-key pkg-sym :author-version)
		auth-ver (package-get-info-prop info 'author-version)
		date (package-get-info-prop info 'date)
		maintainer (package-get-info-prop info 'maintainer)
		req (package-get-info-prop info 'requires))
	  (if (not inst-ver)
	      (setq inst-ver 0))
	  (if (featurep 'balloon-help)
	      (progn
		(setq balloon (format "
Package Information:  [For package: \"%s\"]\n================
Installed Upstream Ver: %s  Available Upstream Ver: %s
Maintainer : %s
Released : %s
Required Packages : %s\n\n"
				      pkg-sym inst-auth-ver auth-ver maintainer 
				      date req))
		(set-extent-property extent 'balloon-help balloon)))
	  (format 
	   "Installed upstream ver: %s  Available upstream ver: %s" 
	   inst-auth-ver auth-ver)))))

(defun pui-display-info (&optional no-error event)
  "Display additional package info in the modeline.
Designed to be called interactively (from a keypress)."
  (interactive)
  (let (extent)
    (save-excursion
      (beginning-of-line)
      (if (setq extent 	(extent-at (point) (current-buffer) 'pui))
	  (message (pui-help-echo extent t))
	(if no-error
	    (clear-message nil)
	  (error 'invalid-operation
		 "No package under cursor!"))))))

(defun pui-display-maintainer (&optional no-error event)
  "Display a package's maintainer in the minibuffer."
  (interactive)
  (let (extent pkg-sym info maintainer)
    (save-excursion
      (beginning-of-line)
      (if (setq extent 	(extent-at (point) (current-buffer) 'pui))
	  (progn
	    (setq pkg-sym (extent-property extent 'pui-package)
		  info (extent-property extent 'pui-info)
		  maintainer (package-get-info-prop info 'maintainer))
	    (message (format "Maintainer: %s" maintainer)))
	(if no-error
	    (clear-message nil)
	  (error 'invalid-operation
		 "No package under cursor!"))))))

(defvar pui-menu
  '("Packages"
    ["Toggle install " pui-toggle-package-key :active (pui-current-package) :suffix (format "`%s'" (or (pui-current-package) "..."))]
    ["Toggle delete " pui-toggle-package-delete-key :active (pui-current-package) :suffix (format "`%s'" (or (pui-current-package) "..."))]
    ["Info on" pui-display-info  :active (pui-current-package) :suffix (format "`%s'" (or (pui-current-package) "..."))]
    "---"
    ["Add Required" pui-add-required-packages t]
    ["Install/Remove Selected" pui-install-selected-packages t]
    "---"
    ["Verbose" pui-toggle-verbosity-redisplay
     :active t :style toggle :selected pui-list-verbose]
    ["Refresh" pui-list-packages t]
    ["Help" pui-help t]
    ["Quit" pui-quit t]))

;;; "Why is there no standard function to do this?"
(defun pui-popup-context-sensitive (event)
  (interactive "e")
  (save-excursion
    (set-buffer (event-buffer event))
    (goto-char (event-point event))
    (popup-menu pui-menu event)
    ;; I agree with dired.el - this is seriously bogus.
    (while (popup-up-p)
      (dispatch-event (next-event)))))

(defun list-packages-mode ()
    "Symbols in the leftmost column:

  +	The package is marked for installation.
  -     The package has not been installed.
  D     The package has been marked for deletion.
  *     The currently installed package is old, and a newer version is
	available.

Useful keys:

  `\\[pui-toggle-package-key]' to select/unselect the current package for installation.
  `\\[pui-toggle-package-delete-key]' to select/unselect the current package for removal.
  `\\[pui-add-required-packages]' to add any packages required by those selected.
  `\\[pui-install-selected-packages]' to install/delete selected packages.
  `\\[pui-display-info]' to display additional information about the package in the modeline.
  `\\[pui-display-maintainer]' to display the package's maintainer in the minibuffer
  `\\[pui-list-packages]' to refresh the package list.
  `\\[pui-toggle-verbosity-redisplay]' to toggle between a verbose and non-verbose display.
  `\\[pui-quit]' to kill this buffer.
"
  (error 'invalid-operation
	 "You cannot enter this mode directly. Use `pui-list-packages'"))

(put 'list-packages-mode 'mode-class 'special)

;;;###autoload
(defun pui-list-packages ()
  "List all packages and package information.
The package name, version, and description are displayed.  From the displayed
buffer, the user can see which packages are installed, which are not, and
which are out-of-date (a newer version is available).  The user can then
select packages for installation via the keyboard or mouse."
  (interactive)
  (package-get-require-base t)
  (let ((outbuf (get-buffer-create pui-info-buffer))
	(sep-string "===============================================================================\n")
	start)
    (message "Creating package list ...") (sit-for 0)
    (set-buffer outbuf)
    (setq buffer-read-only nil)
    (buffer-disable-undo outbuf)
    (erase-buffer outbuf)
    (kill-all-local-variables)
    (use-local-map pui-display-keymap)
    (setq major-mode 'list-packages-mode)
    (setq mode-name "Packages")
    (setq truncate-lines t)

    (unless package-get-remote
      (insert "
Warning: No download sites specified.  Package index may be out of date.
         If you intend to install packages, specify download sites first.

"))
    
    (if pui-list-verbose
	(insert "                 Latest Installed
  Package name   Vers.  Vers.   Description
")
      (insert "                 Latest
  Package name   Vers.  Description
"))
    (insert sep-string)
    (setq start (point))
    (mapcar
     #'(lambda (pkg)
	 (let (pkg-sym info version desc
		       b e extent current-vers disp)
	   (setq pkg-sym (car pkg)
		 info (package-get-info-version (cdr pkg) nil))
	   (setq version (package-get-info-prop info 'version)
		 desc (package-get-info-prop info 'description))

	   (setq disp (pui-package-symbol-char pkg-sym
					       version))
	   (setq b (point))
	   (if pui-list-verbose
	       (progn
		 (setq current-vers (package-get-key pkg-sym :version))
		 (cond
		  ((not current-vers)
		   (setq current-vers "-----"))
		  ((stringp current-vers)
		   (setq current-vers
			 (format "%.2f"
				 (string-to-number current-vers))))
		  ((numberp current-vers)
		   (setq current-vers (format "%.2f" current-vers))))
		 (insert
		  (format "%s %-15s %-5.2f  %-5s  %s\n"
			  (car disp) pkg-sym 
			  (if (stringp version)
			      (string-to-number version)
			    version)
			  current-vers desc)))
	     (insert (format "%s %-15s %-5s %s\n"
			     (car disp)
			     pkg-sym version desc)))
	   (save-excursion
	     (setq e (progn
		       (forward-line -1)
		       (end-of-line)
		       (point))))
	   (setq extent (make-extent b e))
	   (if (car (cdr disp))
	       (set-extent-face extent (get-face (car (cdr disp))))
	     (set-extent-face extent (get-face 'default)))
	   (set-extent-property extent 'highlight t)
	   (set-extent-property extent 'pui t)
	   (set-extent-property extent 'pui-package pkg-sym)
	   (set-extent-property extent 'pui-info info)
	   (set-extent-property extent 'help-echo 'pui-help-echo)
	   (set-extent-property extent 'keymap pui-package-keymap)))
     (sort (copy-sequence package-get-base)
	   #'(lambda (a b)
	       (string< (symbol-name (car a))
			(symbol-name (car b))))))
    (insert sep-string)
    (insert (documentation 'list-packages-mode))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (pop-to-buffer outbuf)
    (delete-other-windows)
    (goto-char start)
    (setq pui-selected-packages nil)	; Reset list
    (setq pui-deleted-packages nil)	; Reset list
    (when (featurep 'menubar)
      (set-buffer-menubar current-menubar)
      (add-submenu '() pui-menu)
      (setq mode-popup-menu pui-menu))
    (clear-message)))

;;;###autoload
(defalias 'list-packages 'pui-list-packages)

(provide 'package-ui)

;;; package-ui.el ends here
