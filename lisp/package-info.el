;;; package-info.el --- Generate information about an XEmacs package

;; Copyright (C) 1998 by Free Software Foundation, Inc.

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

;; This file is used for building package distributions.

;;; Change Log:

;;; Code:

(defvar package-info "package-info"
  "File used to write out Package info")

(defvar package-info-template "package-info.in"
  "Template file for package-get info.")

;; Loses with Mule
;(defun pi-md5sum (file)
;  (let (result)
;    (with-temp-buffer
;      (let ((buffer-file-coding-system-for-read 'binary))
;	(insert-file-contents-literally file))
;      ;; (write-file "/tmp/x.x")
;      (setq result (md5 (current-buffer))))
;    result))

;;; APA: Stolen from package-get in package-get.el
(defun pi-md5sum (file)
  (with-temp-buffer
	(insert-file-contents-literally file)
	(md5 (current-buffer))))

(defun pi-update-key (key value)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (when (search-forward key)
	(replace-match value t)))))

(defun pi-author-version (author-version)
  (if (> (length author-version) 0)
      (format "\"%s\"" author-version)
    (format "\"%d.%d%s\"" emacs-major-version emacs-minor-version
	    (if (and (boundp 'xemacs-betaname) xemacs-betaname)
		(progn
		  (string-match "[0-9]+" xemacs-betaname)
		  (concat "b" (match-string 0 xemacs-betaname)))
	      ""))))

(defun pi-last-mod-date ()
  (condition-case nil
      (save-excursion
	(with-temp-buffer
	  (insert-file-contents-literally "ChangeLog")
	  (goto-char (point-min))
	  (looking-at "[-0-9]+")
	  (format "\"%s\""
		  (buffer-substring (match-beginning 0)
				    (match-end 0)))))
    ;; Fallback on current date if no valid ChangeLog entry
    (t (format-time-string "\"%Y-%m-%d\""))))

(defun batch-update-package-info ()
  "Generate a package-info file for use by package-get.el.
Parameters are:
version -- Package version number
filename -- Filename of tarball to generate info for.
requires -- Packages necessary for bytecompiling.
author-version -- The original Author's version #.
maintainer -- The package maintainer.
category -- The build category."
  (unless noninteractive
    (error 'invalid-operation
	   "`batch-update-package-info' is to be used only with -batch"))
  (let ((version (nth 0 command-line-args-left))
	(filename (nth 1 command-line-args-left))
	(requires (nth 2 command-line-args-left))
	(author-version (nth 3 command-line-args-left))
	(maintainer (nth 4 command-line-args-left))
	(category (nth 5 command-line-args-left)))
    (unless requires
      (setq requires ""))
    (find-file package-info)
    (erase-buffer)
    (insert-file-contents-literally package-info-template)
    (goto-char (point-min))
    (pi-update-key "VERSION" (format "\"%s\"" version))
    (pi-update-key "MD5SUM" (format "\"%s\""
				    (pi-md5sum filename)))
    (pi-update-key "FILENAME" (format "\"%s\""
				      (file-name-nondirectory filename)))
    (pi-update-key "SIZE" (format "%d"
				  (nth 7 (file-attributes filename))))
    (pi-update-key "REQUIRES" requires)
    (pi-update-key "AUTHOR_VERSION" (pi-author-version author-version))
    (pi-update-key "MAINTAINER" (format "\"%s\"" maintainer))
    (pi-update-key "CATEGORY" (format "\"%s\"" category))
    (pi-update-key "BUILD_DATE" (format-time-string "\"%Y-%m-%d\""))
    (pi-update-key "DATE" (pi-last-mod-date))
    (save-buffers-kill-emacs 0)))

(provide 'package-info)

;;; package-info.el ends here
