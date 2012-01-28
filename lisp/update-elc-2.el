;;; update-elc-2.el --- Recompile remaining .el files, post-dumping

;; Copyright (C) 1997 by Free Software Foundation, Inc.
;; Copyright (C) 2000 Ben Wing.

;; Author: Ben Wing <ben@xemacs.org>, based on cleantree.el by
;;         Steven L Baur <steve@xemacs.org>
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

;;; Synched up with: Not in FSF

;;; Commentary:

;; This file should be used after XEmacs has been dumped, to recompile
;; all remaining out-of-date .els and clean up orphaned .elcs.  It should
;; be called as
;;
;;   xemacs -batch -vanilla -l update-elc-2.el -f batch-update-elc-2 ${dirname}
;;
;; where ${dirname} is the directory tree to recompile, usually `lisp'.
;;
;; Note that this is very different from update-elc.el, which is called
;; BEFORE dumping, handles only the files needed to dump, and is called
;; from temacs instead of xemacs.
;;
;; The original cleantree.el had the comment: This code is derived
;; from Gnus based on a suggestion by David Moore <dmoore@ucsd.edu>

;;; Code:

(defvar update-elc-ignored-dirs
  `("." ".." "CVS" "SCCS" "RCS" "{arch}" ".arch-ids"
    ,@(unless (featurep 'mule) '("mule"))
    ,@(unless (fboundp #'ffi-defun) '("ffi"))))

(defvar update-elc-ignored-files
  ;; note: entries here are regexps
  `("^," ;; #### huh?
    "^paths\\.el$"
    "^loadup\\.el$"
    "^loadup-el\\.el$"
    "^update-elc\\.el$"
    "^update-elc-2\\.el$"
    "^dumped-lisp\\.el$"
    "^make-docfile\\.el$"
    "^site-start\\.el$"
    "^site-load\\.el$"
    "^site-init\\.el$"
    "^version\\.el$"
    "^very-early-lisp\\.el$"
    ,@(unless (fboundp 'ffi-defun) '("^ffi.*\\.el$"))))

;; SEEN accumulates the list of already-handled dirs.
(defun do-update-elc-2 (dir compile-stage-p seen)
  (setq dir (file-name-as-directory dir))
  ;; Only scan this sub-tree if we haven't been here yet.
  (unless (member (file-truename dir) seen)
    (push (file-truename dir) seen)

    ;; Do this directory.
    (if compile-stage-p
	;; Stage 2: Recompile necessary .els
	(let ((files (directory-files dir t "\\.el$"))
	      file file-c)
	  (while (setq file (car files))
	    (setq files (cdr files))
	    (setq file-c (concat file "c"))
	    (when (and (file-exists-p file)
		       (or (not (file-exists-p file-c))
			   (file-newer-than-file-p file file-c))
		       (let (ignore)
			 (mapcar
			  #'(lambda (regexp)
			      (if (string-match regexp
						(file-name-nondirectory file))
				  (setq ignore t)))
			  update-elc-ignored-files)
			 (not ignore)))
	      (byte-compile-file file))))

      ;; Stage 1.
      ;; Remove out-of-date elcs
      (let ((files (directory-files dir t "\\.el$"))
	    file file-c)
	(while (setq file (car files))
	  (setq files (cdr files))
	  (setq file-c (concat file "c"))
	  (when (and (file-exists-p file-c)
		     (file-newer-than-file-p file file-c))
	    (message "Removing out-of-date %s" file-c)
	    (delete-file file-c))))
      ;; Remove elcs without corresponding el
      (let ((files (directory-files dir t "\\.elc$"))
	    file file-c)
	(while (setq file-c (car files))
	  (setq files (cdr files))
	  (setq file (replace-in-string file-c "c$" ""))
	  (when (and (file-exists-p file-c)
		     (not (file-exists-p file)))
	    (message "Removing %s; no corresponding .el" file-c)
	    (delete-file file-c))))

    ;; We descend recursively
    (let ((dirs (directory-files dir t nil t 'subdir))
	  dir)
      (while (setq dir (pop dirs))
	(when (and (not (member (file-name-nondirectory dir)
				update-elc-ignored-dirs))
		   (file-directory-p dir))
	  (do-update-elc-2 dir compile-stage-p seen))))

    )))


(defun batch-update-elc-2 ()
  (defvar command-line-args-left)
  (unless noninteractive
    (error "`batch-update-elc-2' is to be used only with -batch"))
  (let ((dir (car command-line-args-left)))
    ;; We remove all the bad .elcs before any byte-compilation, because
    ;; there may be dependencies between one .el and another (even across
    ;; directories), and we don't want to load an out-of-date .elc while
    ;; byte-compiling a file.
    (message "Removing old or spurious .elcs in directory tree `%s'..." dir)
    (do-update-elc-2 dir nil nil)
    (message "Removing old or spurious .elcs in directory tree `%s'...done"
	     dir)
    (message "Recompiling updated .els in directory tree `%s'..." dir)
    (do-update-elc-2 dir t nil)
    (message "Recompiling updated .els in directory tree `%s'...done" dir))
  (setq command-line-args-left nil))

;;; update-elc-2.el ends here
