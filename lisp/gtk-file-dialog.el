;;; gtk-file-dialog.el --- A nicer file selection dialog for XEmacs w/GTK primitives

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Maintainer: William M. Perry <wmperry@gnu.org>
;; Keywords: extensions, internal

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
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; The default GTK file selection dialog is not sufficient for our
;; needs.  Limitations include:
;;
;; - not derived from GtkDialog
;; - no support for filters based on file types
;; - no support for setting an initial directory
;; - no way to tell it 'file must exist'
;; - no easy way to tell it to look at directories only
;; - ugly as sin
;;
;; This attempts to rectify the situation.

(defun gtk-file-dialog-fill-file-list (dialog dir)
  (if (not dir)
      (setq dir (get dialog 'x-file-dialog-current-dir nil)))

  (put dialog 'x-file-dialog-current-dir dir)

  (let ((list (get dialog 'x-file-dialog-files-list nil))
	(remotep (file-remote-p dir)))
    (if (not list)
	nil
      (gtk-clist-clear list)
      (gtk-clist-freeze list)
      ;; NOTE: Current versions of efs / ange-ftp do not honor the
      ;; files-only flag to directory-files, but actually DOING these
      ;; checks is hideously expensive.  Leave it turned off for now.
      (mapc (lambda (f)
	      (if (or t			; Lets just wait for EFS to
		      (not remotep)	; fix itself, shall we?
		      (not (file-directory-p (expand-file-name f dir))))
		  (gtk-clist-append list (list f))))
	    (directory-files dir nil
			     (get dialog 'x-file-dialog-active-filter nil)
			     nil t))
      (gtk-clist-thaw list))))

(defun gtk-file-dialog-fill-directory-list (dialog dir)
  (let ((subdirs (directory-files dir nil nil nil 5))
	(remotep (file-remote-p dir))
	(selected-dir (get dialog 'x-file-dialog-current-dir "/"))
	(directory-list (get dialog 'x-file-dialog-directory-list)))

    (gtk-clist-freeze directory-list)
    (gtk-clist-clear directory-list)

    (while subdirs
      (if (equal "." (car subdirs))
	  nil
	;; NOTE: Current versions of efs / ange-ftp do not honor the
	;; files-only flag to directory-files, but actually DOING these
	;; checks is hideously expensive.  Leave it turned off for now.
	(if (or t			; Lets just wait for EFS to
		(not remotep)		; fix itself, shall we?
		(file-directory-p (expand-file-name (car subdirs) dir)))
	    (gtk-clist-append directory-list (list (car subdirs)))))
      (pop subdirs))
    (gtk-clist-thaw directory-list)))

(defun gtk-file-dialog-update-dropdown (dialog dir)
  (let ((combo-box (get dialog 'x-file-dialog-select-list))
	(components (reverse
		     (delete ""
			     (split-string dir
					   (concat "[" (char-to-string directory-sep-char) "]")))))
	(entries nil))

    (while components
      (push (concat "/" (mapconcat 'identity (reverse components)
				   (char-to-string directory-sep-char)))
	    entries)
      (pop components))
    (push (expand-file-name "." "~/") entries)
    (gtk-combo-set-popdown-strings combo-box (nreverse entries))))

(defun gtk-file-dialog-select-directory (dialog dir)
  (gtk-file-dialog-fill-directory-list dialog dir)
  (gtk-file-dialog-fill-file-list dialog dir)
  (gtk-file-dialog-update-dropdown dialog dir))

(defun gtk-file-dialog-new (&rest keywords)
  "Create a XEmacs file selection dialog.
Optional keyword arguments allowed:

:title			The title of the dialog
:initial-directory	Initial directory to show
:filter-list		List of filter descriptions and filters
:file-must-exist	Whether the file must exist or not
:directory		Look for a directory instead
:callback		Function to call with one arg, the selection
"
  (let* ((dialog (gtk-dialog-new))
	 (vbox (gtk-dialog-vbox dialog))
	 (dir (plist-get keywords :initial-directory default-directory))
	 (button-area (gtk-dialog-action-area dialog))
	 (initializing-gtk-file-dialog t)
	 (select-box nil)
	 button hbox)

    (put dialog 'type 'dialog)

    (gtk-window-set-title dialog (plist-get keywords :title "Select a file..."))

    (setq button (gtk-button-new-with-label "OK"))
    (gtk-container-add button-area button)
    (gtk-signal-connect button 'clicked
			(lambda (button dialog)
			  (funcall
			   (get dialog 'x-file-dialog-callback 'ignore)
			   (gtk-entry-get-text
			    (get dialog 'x-file-dialog-entry nil)))
			  (gtk-widget-destroy dialog))
			dialog)
    (put dialog 'x-file-dialog-ok-button button)

    (setq button (gtk-button-new-with-label "Cancel"))
    (gtk-container-add button-area button)
    (gtk-signal-connect button 'clicked
			(lambda (button dialog)
			  (gtk-widget-destroy dialog)) dialog)

    (put dialog 'x-file-dialog-cancel-button button)
    (put dialog 'x-file-dialog-callback (plist-get keywords :callback 'ignore))
    (put dialog 'x-file-dialog-construct-args keywords)
    (put dialog 'x-file-dialog-current-dir dir)

    ;; Dropdown list of directories...
    (setq select-box (gtk-combo-new))
    (gtk-combo-disable-activate select-box)
    (gtk-box-pack-start vbox select-box nil nil 5)
    (put dialog 'x-file-dialog-select-list select-box)

    ;; Hitting return in the entry will change dirs...
    (gtk-signal-connect (gtk-combo-entry select-box) 'activate
			(lambda (entry dialog)
			  (gtk-file-dialog-select-directory dialog
							    (gtk-entry-get-text entry)))
			dialog)

    ;; Start laying out horizontally...
    (setq hbox (gtk-hbox-new nil 0))
    (gtk-box-pack-start vbox hbox t t 5)

    ;; Directory listing
    (let ((directories (gtk-clist-new-with-titles 1 '("Directories")))
	  (scrolled (gtk-scrolled-window-new nil nil))
	  (item nil))
      (gtk-container-add scrolled directories)
      (gtk-widget-set-usize scrolled 200 300)
      (gtk-box-pack-start hbox scrolled t t 0)
      (put dialog 'x-file-dialog-directory-list directories)
      (put dialog 'x-file-dialog-directory-scrolled scrolled)

      (gtk-signal-connect directories 'select-row
			  (lambda (list row column event dialog)
			    (let ((dir (expand-file-name
					 (gtk-clist-get-text
					  (get dialog 'x-file-dialog-directory-list)
					  row column)
					 (get dialog 'x-file-dialog-current-dir))))
			      (if (and (misc-user-event-p event)
				       (event-function event))
				  (gtk-file-dialog-select-directory dialog dir)
				(gtk-entry-set-text
				 (get dialog 'x-file-dialog-entry)
				 dir))))
			  dialog)
      )

    (if (plist-get keywords :directory nil)
	;; Directory listings only do not need the file or filters buttons.
	nil
      ;; File listing
      (let ((list (gtk-clist-new-with-titles 1 '("Files")))
	    (scrolled (gtk-scrolled-window-new nil nil)))
	(gtk-container-add scrolled list)
	(gtk-widget-set-usize scrolled 200 300)
	(gtk-box-pack-start hbox scrolled t t 0)

	(gtk-signal-connect list 'select-row
			    (lambda (list row column event dialog)
			      (gtk-entry-set-text
			       (get dialog 'x-file-dialog-entry nil)
			       (expand-file-name
				(gtk-clist-get-text list row column)
				(get dialog 'x-file-dialog-current-dir nil)))
			      (if (and (misc-user-event-p event)
				       (event-function event))
				  ;; Got a double or triple click event...
				  (gtk-button-clicked
				   (get dialog 'x-file-dialog-ok-button nil))))
			    dialog)

	(put dialog 'x-file-dialog-files-list list))

      ;; Filters
      (if (not (plist-get keywords :filter-list nil))
	  ;; Don't need to bother packing this
	  nil
	(setq hbox (gtk-hbox-new nil 0))
	(gtk-box-pack-start vbox hbox nil nil 0)

	(let ((label nil)
	      (options (plist-get keywords :filter-list nil))
	      (omenu nil)
	      (menu nil)
	      (item nil))
	  (setq omenu (gtk-option-menu-new)
		menu (gtk-menu-new)
		label (gtk-label-new "Filter: "))

	  (put dialog 'x-file-dialog-active-filter (cdr (car options)))
	  (mapc (lambda (o)
		  (setq item (gtk-menu-item-new-with-label (car o)))
		  (gtk-signal-connect item 'activate
				      (lambda (obj data)
					(put (car data) 'x-file-dialog-active-filter (cdr data))
					(gtk-file-dialog-fill-file-list (car data) nil))
				      (cons dialog (cdr o)))
		  (gtk-menu-append menu item)
		  (gtk-widget-show item)) options)
	  (gtk-option-menu-set-menu omenu menu)
	  (gtk-box-pack-end hbox omenu nil nil 0)
	  (gtk-box-pack-end hbox label nil nil 0))))

      ;; Entry
    (let ((entry (gtk-entry-new)))
      (if (plist-get keywords :directory nil)
	  nil
	(gtk-box-pack-start vbox entry nil nil 0))
      (if (plist-get keywords :file-must-exist nil)
	  (progn
	    (gtk-widget-set-sensitive (get dialog 'x-file-dialog-ok-button nil) nil)
	    (gtk-signal-connect entry 'changed
				(lambda (entry dialog)
				  (gtk-widget-set-sensitive
				   (get dialog 'x-file-dialog-ok-button)
				   (file-exists-p (gtk-entry-get-text entry))))
				dialog)))
      (put dialog 'x-file-dialog-entry entry))

    (gtk-widget-realize dialog)


    ;; Populate the file list if necessary
    (gtk-file-dialog-select-directory dialog dir)
    dialog))

(provide 'gtk-file-dialog)
