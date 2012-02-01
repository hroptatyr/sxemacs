;;; finder.el --- topic & keyword-based code finder

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Created: 16 Jun 1992
;; Version: 1.0
;; Keywords: help
;; X-Modified-by: Bob Weiner <weiner@mot.com>, 4/18/95, to include Lisp
;;	library directory names in finder-program-info, for fast display of
;;	Lisp libraries and associated commentaries.  Added {v}, finder-view,
;;	and {e}, finder-edit commands for displaying libraries.
;;
;;	Added user variable, 'finder-abbreviate-directory-list', used to
;;	abbreviate directories before they are saved to finder-program-info.
;;	Such relative directories can be portable from one Emacs installation
;;	to another.  Default value is based upon the value of Emacs'
;;      data-directory variable.

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

;;; Synched up with: FSF 19.34.

;;; Commentary:

;; This mode uses the Keywords library header to provide code-finding
;; services by keyword.
;;
;; Things to do:
;;    1. Support multiple keywords per search.  This could be extremely hairy;
;; there doesn't seem to be any way to get completing-read to exit on
;; an EOL with no substring pending, which is what we'd want to end the loop.
;;    2. Search by string in synopsis line?
;;    3. Function to check finder-package-info for unknown keywords.

;;; Code:

(require 'lisp-mnt)
(condition-case nil
    (require 'finder-inf)
  (t nil))
;; XEmacs addition
(require 'picture)
(require 'mode-motion)

(defvar finder-emacs-root-directory
  (file-name-directory (directory-file-name data-directory))
  "Root directory of current emacs tree.")

(defvar finder-abbreviate-directory-list
  (list finder-emacs-root-directory)
  "*List of directory roots to remove from finder-package-info directory entries.
The first element in the list is used when expanding relative package
directories to view or extract information from package source code.")

(defvar finder-file-regexp "\\.el$"
  "Regexp which matches file names but not Emacs Lisp finder keywords.")

;; Local variable in finder buffer.
(defvar finder-headmark)

(defvar finder-known-keywords
  `(
    (abbrev	. "abbreviation handling, typing shortcuts, macros")
    (bib	. "code related to the `bib' bibliography processor")
    (build	. "code used to build XEmacs")
    (c		. "C, C++, and Objective-C language support")
    (calendar	. "calendar and time management support")
    (comm	. "communications, networking, remote access to files")
    (content    . "contains content (menu/dialog box descs, text, images, &c)")
    (data	. "support for editing files of data")
    (docs	. "support for XEmacs documentation")
    (dumped     . "files preloaded into XEmacs")
    (emulations	. "emulations of other editors")
    (extensions	. "Emacs Lisp language extensions")
    (faces	. "support for multiple fonts")
    ,(when (fboundp #'ffi-defun)
       (cons 'ffi "foreign function interface"))
    (frames	. "support for XEmacs frames and window systems")
    (games	. "games, jokes and amusements")
    (gui	. "support for menubars, dialog boxes, and other GUI features")
    (hardware	. "support for interfacing with exotic hardware")
    (help	. "support for on-line help systems")
    (hypermedia	. "support for links between text or other media types")
    (i18n	. "internationalization and alternate character-set support")
    (internal	. "code implementing core functionality in XEmacs")
    (languages	. "specialized modes for editing programming languages")
    (lisp	. "Lisp support, including Emacs Lisp")
    (local	. "code local to your site")
    (mail	. "modes for electronic-mail handling")
    (maint	. "maintenance aids for the Emacs development group")
    (matching	. "various sorts of searching and matching")
    (mouse	. "mouse support")
    ,(when (featurep 'mule)
       (cons 'mule "multi-language extensions"))
    (news	. "support for netnews reading and posting")
    (oop	. "support for object-oriented programming")
    (outlines	. "support for hierarchical outlining")
    (processes	. "process, subshell, compilation, and job control support")
    (services	. "provides services for use by other programs (cf `user')")
    (terminals	. "support for terminal types")
    (tex	. "code related to the TeX formatter")
    (tools	. "programming tools")
    (unix	. "front-ends/assistants for, or emulators of, UNIX features")
    (user	. "program interacts directly with the user (cf `services'")
    (vms	. "support code for vms")
    (wp		. "word processing")
    (www	. "support for the Web (WWW, the World Wide Web)")
    ))

(defvar finder-mode-map nil)
(or finder-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map " "	'finder-select)
      (define-key map "f"	'finder-select)
      (define-key map "\C-m"	'finder-select)
      ;; XEmacs changes
      (define-key map "e"	'finder-edit)
      (define-key map "v"	'finder-view)
      (define-key map "?"	'finder-summary)
      (define-key map "q"	'finder-exit)
      (define-key map "d"	'finder-list-keywords)
      ;; XEmacs change
      (define-key map [button2]	'finder-mouse-select)
      (setq finder-mode-map map)))


;;; Code for regenerating the keyword list.

(defvar finder-package-info nil
  "Assoc list mapping file names to description & keyword lists.")

(defvar finder-compile-keywords-quiet nil
  "If non-nil finder-compile-keywords will not print any messages.")

(defun finder-compile-keywords (&rest dirs)
  "Regenerate the keywords association list into the file `finder-inf.el'.
Optional arguments are a list of Emacs Lisp directories to compile from; no
arguments compiles from `load-path'."
  (save-excursion
    ;; XEmacs change
    (find-file "finder-inf.el")
    (let ((processed nil)
	  (directory-abbrev-alist
	   (append
	   (mapcar (function (lambda (dir)
			       (cons (concat "^" (regexp-quote dir))
				     "")))
		    finder-abbreviate-directory-list)
	    directory-abbrev-alist))
	  (using-load-path))
      (or dirs (setq dirs load-path))
      (setq using-load-path (equal dirs load-path))
      (erase-buffer)
      (insert ";;; finder-inf.el --- keyword-to-package mapping\n")
      (insert ";; Keywords: help\n")
      (insert ";;; Commentary:\n")
      (insert ";; Don't edit this file.  It's generated by finder.el\n\n")
      (insert ";;; Code:\n")
      (insert "\n(defconst finder-package-info '(\n")
      (mapcar
       (lambda (d)
	 (mapcar
	  (lambda (f)
	    (let ((exhau-f (expand-file-name f d)))
	      (when (and (not (member f processed))
			 (file-readable-p exhau-f))
		(let (summary keystart keywords)
		  (setq processed (cons f processed))
		  (if (not finder-compile-keywords-quiet)
		      (message "Processing %s ..." f))
		  (save-excursion
		    (set-buffer (get-buffer-create "*finder-scratch*"))
		    (buffer-disable-undo (current-buffer))
		    (erase-buffer)
		    (insert-file-contents (expand-file-name f d))
		    (condition-case err
			(setq summary  (lm-synopsis)
			      keywords (lm-keywords))
		      (t (message "finder: error processing %s %S" f err))))
		  (when summary
		    (insert (format "    (\"%s\"\n        " f))
		    (prin1 summary (current-buffer))
		    (insert "\n        ")
		    (setq keystart (point))
		    (insert (if keywords (format "(%s)" keywords) "nil"))
		    (subst-char-in-region keystart (point) ?, ? )
		    (insert "\n        ")
		    (prin1 (abbreviate-file-name d) (current-buffer))
		    (insert ")\n"))))))
	  ;;
	  ;; Skip null, non-existent or relative pathnames, e.g. "./", if
	  ;; using load-path, so that they do not interfere with a scan of
	  ;; library directories only.
	  (if (and using-load-path
		   (not (and d (file-name-absolute-p d) (file-exists-p d))))
	      nil
	    (setq d (file-name-as-directory (or d ".")))
	    (directory-files d nil "^[^=].*\\.el$"))))
       dirs)
      (insert "))\n\n(provide 'finder-inf)\n\n;;; finder-inf.el ends here\n")
      (kill-buffer "*finder-scratch*")
      (unless noninteractive
	(eval-current-buffer)) ; So we get the new keyword list immediately
      (basic-save-buffer))))

(defun finder-compile-keywords-make-dist ()
  "Regenerate `finder-inf.el' for the Emacs distribution."
  (finder-compile-keywords default-directory))

;;; Now the retrieval code

(defun finder-insert-at-column (column &rest strings)
  "Insert list of STRINGS, at column COLUMN."
  (if (>= (current-column) column) (insert "\n"))
  (move-to-column column)
  (let ((col (current-column)))
    (if (< col column)
	(indent-to column)
      (if (and (/= col column)
	       (= (preceding-char) ?\t))
	  (let (indent-tabs-mode)
	    (delete-char -1)
	    (indent-to col)
	    (move-to-column column)))))
  (apply 'insert strings))

(defun finder-list-keywords ()
  "Display descriptions of the keywords in the Finder buffer."
  (interactive)
  (setq buffer-read-only nil)
  (erase-buffer)
  (mapcar
   (lambda (assoc)
     (let ((keyword (car assoc)))
       (insert (symbol-name keyword))
       (finder-insert-at-column 14 (concat (cdr assoc) "\n"))
       (cons (symbol-name keyword) keyword)))
   finder-known-keywords)
  (goto-char (point-min))
  (setq finder-headmark (point))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  ;; XEmacs change
  (if (not (one-window-p))
      (balance-windows))
  (finder-summary))

(defun finder-list-matches (key)
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((id (intern key)))
    (insert
     "The following packages match the keyword `" key "':\n\n")
    (setq finder-headmark (point))
    (mapcar
     (lambda (x)
       (if (memq id (car (cdr (cdr x))))
	   (progn
	     (insert (car x))
	     (finder-insert-at-column 16 (concat (car (cdr x)) "\n")))))
     finder-package-info)
    (goto-char (point-min))
    (forward-line)
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (shrink-window-if-larger-than-buffer)
    (finder-summary)))

;; Search for a file named FILE the same way `load' would search.
(defun finder-find-library (file)
  (if (file-name-absolute-p file)
      file
    (let ((dirs load-path)
	  found)
      (while (and dirs (not found))
	(if (file-exists-p (expand-file-name (concat file ".el") (car dirs)))
	    (setq found (expand-file-name file (car dirs)))
	  (if (file-exists-p (expand-file-name file (car dirs)))
	      (setq found (expand-file-name file (car dirs)))))
	(setq dirs (cdr dirs)))
      found)))

;;;###autoload
(defun finder-commentary (file)
  "Display FILE's commentary section.
FILE should be in a form suitable for passing to `locate-library'."
  (interactive "sLibrary name: ")
  (let* ((str (lm-commentary (or (finder-find-library file)
				 (finder-find-library (concat file ".el"))
				 (error "Can't find library %s" file)))))
    (if (null str)
	(error "Can't find any Commentary section"))
    (pop-to-buffer "*Finder*")
    ;; XEmacs change
    (setq buffer-read-only nil
	  mode-motion-hook 'mode-motion-highlight-line)
    (erase-buffer)
    (insert str)
    (goto-char (point-min))
    (delete-blank-lines)
    (goto-char (point-max))
    (delete-blank-lines)
    (goto-char (point-min))
    (while (re-search-forward "^;+ ?" nil t)
      (replace-match "" nil nil))
    (goto-char (point-min))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (shrink-window-if-larger-than-buffer)
    (finder-summary)))

(defun finder-current-item ()
  (if (and finder-headmark (< (point) finder-headmark))
      (error "No keyword or filename on this line")
    (save-excursion
      (beginning-of-line)
      (current-word))))

;; XEmacs change
(defun finder-edit ()
  (interactive)
  (let ((entry (finder-current-item)))
    (if (string-match finder-file-regexp entry)
	(let ((path (finder-find-library entry)))
	  (if path
	      (find-file-other-window path)
	    (error "Can't find Emacs Lisp library: '%s'" entry)))
      ;; a finder keyword
      (error "Finder-edit works on Emacs Lisp libraries only"))))

;; XEmacs change
(defun finder-view ()
  (interactive)
  (let ((entry (finder-current-item)))
    (if (string-match finder-file-regexp entry)
	(let ((path (finder-find-library entry)))
	  (if path
	      (declare-fboundp (view-file-other-window path))
	    (error "Can't find Emacs Lisp library: '%s'" entry)))
      ;; a finder keyword
      (error "Finder-view works on Emacs Lisp libraries only"))))

(defun finder-select ()
  (interactive)
  (let ((key (finder-current-item)))
    ;; XEmacs change
    (if (string-match finder-file-regexp key)
	(finder-commentary key)
      (finder-list-matches key))))

;; XEmacs change
(defun finder-mouse-select (ev)
  (interactive "e")
  (goto-char (event-point ev))
  (finder-select))

;; XEmacs change
;;;###autoload
(defun finder-by-keyword ()
  "Find packages matching a given keyword."
  (interactive)
  (finder-mode)
  (finder-list-keywords))

(defun finder-mode ()
  "Major mode for browsing package documentation.
\\<finder-mode-map>
\\[finder-select]	more help for the item on the current line
\\[finder-edit] edit Lisp library in another window
\\[finder-view] view Lisp library in another window
\\[finder-exit]	exit Finder mode and kill the Finder buffer.
"
  (interactive)
  (pop-to-buffer "*Finder*")
  ;; XEmacs change
  (setq buffer-read-only nil
	mode-motion-hook 'mode-motion-highlight-line)
  (erase-buffer)
  (use-local-map finder-mode-map)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (setq mode-name "Finder")
  (setq major-mode 'finder-mode)
  (make-local-variable 'finder-headmark)
  (setq finder-headmark nil))

(defun finder-summary ()
  "Summarize basic Finder commands."
  (interactive)
  (message "%s"
   (substitute-command-keys
    ;; XEmacs change
    "\\<finder-mode-map>\\[finder-select] = select, \\[finder-list-keywords] = keywords, \\[finder-edit] = edit, \\[finder-view] = view, \\[finder-exit] = quit, \\[finder-summary] = help")))

(defun finder-exit ()
  "Exit Finder mode and kill the buffer."
  (interactive)
  ;; XEmacs change
  (or (one-window-p t 0)
      (delete-window))
  (kill-buffer "*Finder*"))

(provide 'finder)

;;; finder.el ends here
