;;; lib-complete.el --- Completion on the lisp search path

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) Mike Williams <mike-w@cs.aukuni.ac.nz> 1991

;; Author: Mike Williams <mike-w@cs.aukuni.ac.nz>
;; Maintainer: SXEmacs Development Team
;; Keywords: lisp, extensions, dumped
;; Created: Sat Apr 20 17:47:21 1991

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

;; This file is dumped with SXEmacs.

;; ========================================================================
;; lib-complete.el --  Completion on a search path
;; Author          : Mike Williams <mike-w@cs.aukuni.ac.nz>
;; Created On      : Sat Apr 20 17:47:21 1991
;; Last Modified By: Heiko M|nkel <muenkel@tnt.uni-hannover.de>
;; Additional XEmacs integration By: Chuck Thompson <cthomp@cs.uiuc.edu>
;; Last Modified On: Thu Jul 1 14:23:00 1994
;; ========================================================================
;; NOTE: XEmacs must be redumped if this file is changed.
;;
;; Copyright (C) Mike Williams <mike-w@cs.aukuni.ac.nz> 1991
;;
;; Keywords: utility, lisp

;; Many thanks to Hallvard Furuseth <hallvard@ifi.uio.no> for his
;; helpful suggestions.

;; The function locate-file is removed, because of its incompatibility
;; with the buildin function of the lemacs 19.10 (Heiko M|nkel).

;; There is now the new function find-library in this package.

;;; ChangeLog:

;; 4/26/97: sb Mule-ize.
;; 6/24/1999 much rewriting from Bob Weiner

;;; Code:

;;=== Determine completions for filename in search path ===================

(defun library-all-completions (FILE SEARCH-PATH &optional FULL FAST)
  "Return all completions for FILE in any directory on SEARCH-PATH.
If optional third argument FULL is non-nil, returned pathnames should be
  absolute rather than relative to some directory on the SEARCH-PATH.
If optional fourth argument FAST is non-nil, don't sort the completions,
  or remove duplicates."
  (setq FILE (or FILE ""))
  (if (file-name-absolute-p FILE)
      ;; It's an absolute file name, so don't need SEARCH-PATH
      (progn
	(setq FILE (expand-file-name FILE))
	(file-name-all-completions
	 (file-name-nondirectory FILE) (file-name-directory FILE)))
    (let ((subdir (file-name-directory FILE))
	  (file (file-name-nondirectory FILE))
	  all-completions)
      ;; Make list of completions in each directory on SEARCH-PATH
      (while SEARCH-PATH
	(let* ((dir (concat (file-name-as-directory
			     (expand-file-name (car SEARCH-PATH)))
			    subdir))
	       (dir-prefix (if FULL dir subdir)))
	  (if (file-directory-p dir)
	      (let ((subdir-completions
		     (file-name-all-completions file dir)))
		(while subdir-completions
		  (setq all-completions
			(cons (concat dir-prefix (car subdir-completions))
			      all-completions))
		  (setq subdir-completions (cdr subdir-completions))))))
	(setq SEARCH-PATH (cdr SEARCH-PATH)))
      (if FAST all-completions
	(let ((sorted (nreverse (sort all-completions 'string<)))
	      compressed)
	  (while sorted
	    (if (equal (car sorted) (car compressed)) nil
	      (setq compressed (cons (car sorted) compressed)))
	    (setq sorted (cdr sorted)))
	  compressed)))))

;;=== Utilities ===========================================================

(defmacro progn-with-message (message &rest forms)
  "(progn-with-message MESSAGE FORMS ...)
Display MESSAGE and evaluate FORMS, returning value of the last one."
  ;; based on Hallvard Furuseth's funcall-with-message
  `(if (eq (selected-window) (minibuffer-window))
       (save-excursion
	 (goto-char (point-max))
	 (let ((orig-pmax (point-max)))
	   (unwind-protect
	       (progn
		 (insert " " ,message) (goto-char orig-pmax)
		 (sit-for 0)		; Redisplay
		 ,@forms)
	     (delete-region orig-pmax (point-max)))))
     (prog2
	 (message "%s" ,message)
	 (progn ,@forms)
       (message ""))))

(put 'progn-with-message 'lisp-indent-hook 1)

;;=== Completion caching ==================================================

(defconst lib-complete:cache nil
  "Used within `read-library' and `read-library-internal' to prevent
costly repeated calls to `library-all-completions'.
Format is a list of lists of the form

    ([<path> <subdir>] <cache-record> <cache-record> ...)

where each <cache-record> has the form

   (<root> <modtimes> <completion-table>)")

(defun lib-complete:better-root (ROOT1 ROOT2)
  "Return non-nil if ROOT1 is a superset of ROOT2."
  (and (equal (file-name-directory ROOT1) (file-name-directory ROOT2))
       (string-match
	(concat "^" (regexp-quote (file-name-nondirectory ROOT1)))
	ROOT2)))

(defun lib-complete:get-completion-table (FILE PATH FILTER)
  (let* ((subdir (file-name-directory FILE))
	 (root (file-name-nondirectory FILE))
	 (PATH
	  (mapcar
	   (function (lambda (dir) (file-name-as-directory
				    (expand-file-name (or dir "")))))
	   PATH))
	 (key (vector PATH subdir FILTER))
	 (real-dirs
	  (if subdir
	      (mapcar (function (lambda (dir) (concat dir subdir))) PATH)
	    PATH))
	 (path-modtimes
	  (mapcar
	   (function (lambda (fn) (if fn (nth 5 (file-attributes fn)))))
	   real-dirs))
	 (cache-entry (assoc key lib-complete:cache))
	 (cache-records (cdr cache-entry)))
    ;; Look for cached entry
    (catch 'table
      (while cache-records
	(if (and
	     (lib-complete:better-root (nth 0 (car cache-records)) root)
	     (equal (nth 1 (car cache-records)) path-modtimes))
	    (throw 'table (nth 2 (car cache-records))))
	(setq cache-records (cdr cache-records)))
      ;; Otherwise build completions
      (let ((completion-list
	     (progn-with-message "(building completion table...)"
	       (library-all-completions FILE PATH nil 'fast)))
	    (completion-table (make-vector 127 0)))
	(while completion-list
	  (let ((completion
		 (if (or (not FILTER)
			 (file-directory-p (car completion-list)))
		     (car completion-list)
		   (funcall FILTER (car completion-list)))))
	    (if completion
		(intern completion completion-table)))
	  (setq completion-list (cdr completion-list)))
	;; Cache the completions
	(lib-complete:cache-completions key root
					path-modtimes completion-table)
	completion-table))))

(defvar lib-complete:max-cache-size 40
  "*Maximum number of search paths which are cached.")

(defun lib-complete:cache-completions (key root modtimes table)
  (let* ((cache-entry (assoc key lib-complete:cache))
	 (cache-records (cdr cache-entry))
	 (new-cache-records (list (list root modtimes table))))
    (if (not cache-entry) nil
      ;; Remove old cache entry
      (setq lib-complete:cache (delq cache-entry lib-complete:cache))
      ;; Copy non-redundant entries from old cache entry
      (while cache-records
	(if (or (equal root (nth 0 (car cache-records)))
		(lib-complete:better-root root (nth 0 (car cache-records))))
	    nil
	  (setq new-cache-records
		(cons (car cache-records) new-cache-records)))
	(setq cache-records (cdr cache-records))))
    ;; Add entry to front of cache
    (setq lib-complete:cache
	  (cons (cons key (nreverse new-cache-records)) lib-complete:cache))
    ;; Trim cache
    (let ((tail (nthcdr lib-complete:max-cache-size lib-complete:cache)))
      (if tail (setcdr tail nil)))))

;;=== Read a filename, with completion in a search path ===================

(defun read-library-internal (FILE FILTER FLAG)
  "Don't call this."
  ;; Relies on read-library-internal-search-path being let-bound
  (declare (special read-library-internal-search-path))
  (let ((completion-table
	 (lib-complete:get-completion-table
	  FILE read-library-internal-search-path FILTER)))
    (cond
     ((not completion-table) nil)
     ;; Completion table is filtered before use, so the PREDICATE
     ;; argument is redundant.
     ((eq FLAG nil) (try-completion FILE completion-table nil))
     ((eq FLAG t) (all-completions FILE completion-table nil))
     ((eq FLAG 'lambda) (and (intern-soft FILE completion-table) t))
     )))

(defun read-library (PROMPT SEARCH-PATH &optional DEFAULT MUST-MATCH
			    FULL FILTER)
  "Read library name, prompting with PROMPT and completing in directories
from SEARCH-PATH.  A nil in the search path represents the current
directory.  Completions for a given search-path are cached, with the
cache being invalidated whenever one of the directories on the path changes.
Default to DEFAULT if user enters a null string.
Optional fourth arg MUST-MATCH non-nil means require existing file's name.
  Non-nil and non-t means also require confirmation after completion.
Optional fifth argument FULL non-nil causes a full pathname, rather than a
  relative pathname, to be returned.  Note that FULL implies MUST-MATCH.
Optional sixth argument FILTER can be used to provide a function to
  filter the completions.  This function is passed the filename, and should
  return a transformed filename (possibly a null transformation) or nil,
  indicating that the filename should not be included in the completions."
  (let* ((read-library-internal-search-path SEARCH-PATH)
	 (library (completing-read PROMPT 'read-library-internal
				   FILTER (or MUST-MATCH FULL) nil)))
    (cond
     ((equal library "") DEFAULT)
     (FULL (locate-file library read-library-internal-search-path
			 '(".el" ".el.gz" ".elc")))
     (t library))))

(defun read-library-name (prompt)
  "PROMPTs for and returns an existing Elisp library name (without any suffix) or the empty string."
  (interactive)
  (declare (special read-library-internal-search-path))
  (let ((read-library-internal-search-path load-path))
    (completing-read prompt
		     'read-library-internal
		     (lambda (fn)
		       (cond
			((string-match #r"\.el\(\.gz\|\.Z\)?$" fn)
			 (substring fn 0 (match-beginning 0)))))
		     t nil)))

;; NOTE: as a special case, read-library may be used to read a filename
;; relative to the current directory, returning a *relative* pathname
;; (read-file-name returns a full pathname).
;;
;; eg. (read-library "Local header: " '(nil) nil)

;;=== Replacement for load-library with completion ========================

(defun load-library (library)
  "Load the library named LIBRARY.
This is an interface to the function `load'."
  (interactive
   (list (read-library "Load library: " load-path nil nil nil
		       (function (lambda (fn)
				   (cond
				    ((string-match "\\.elc?$" fn)
				     (substring fn 0 (match-beginning 0))))))
		       )))
  (load library))

;;=== find-library with completion (Author: Bob Weiner) ===================

(defun find-library (library &optional codesys display-function)
  "Find and display in the current window the source for the Elisp LIBRARY.
LIBRARY should be a name without any path information and may include or omit
the \".el\" suffix.  Under XEmacs/Mule, the optional second argument CODESYS
specifies the coding system to use when decoding the file.  Interactively,
with a prefix argument, this prompts for the coding system.  Optional third
argument DISPLAY-FUNCTION must take two arguments, the filename to display
and CODESYS.  The default for DISPLAY-FUNCTION is `find-file'."
  (interactive
   (list (read-library-name "Find library: ")
	 (if current-prefix-arg
	     (read-coding-system "Coding System: "))))
  (let ((path (if (or (null library) (equal library ""))
		   nil
		(locate-file library load-path
			     '("" ".el" ".el.gz" ".el.Z")))))
    (if path (funcall (if (fboundp display-function)
			  display-function 'find-file)
		      path codesys)
      (error "(find-library): Cannot locate library `%s'" library))))

(defun find-library-other-window (library &optional codesys)
  "Find and display in another window the source for the Elisp LIBRARY.
LIBRARY should be a name without any path information and may include or omit
the \".el\" suffix.  Under XEmacs/Mule, the optional second argument CODESYS
specifies the coding system to use when decoding the file.  Interactively,
with a prefix argument, this prompts for the coding system."
  (interactive
   (list (read-library-name "Find library in other window: ")
	 (if current-prefix-arg
	     (read-coding-system "Coding System: "))))
  (find-library library codesys 'find-file-other-window))

(defun find-library-other-frame (library &optional codesys)
  "Find and display in another frame the source for the Elisp LIBRARY.
LIBRARY should be a name without any path information and may include or omit
the \".el\" suffix.  Under XEmacs/Mule, the optional second argument CODESYS
specifies the coding system to use when decoding the file.  Interactively,
with a prefix argument, this prompts for the coding system."
  (interactive
   (list (read-library-name "Find library in other frame: ")
	 (if current-prefix-arg
	     (read-coding-system "Coding System: "))))
  (find-library library codesys 'find-file-other-frame))

;; This conflicts with an existing binding.
;;(define-key global-map "\C-xl" 'find-library)
(define-key global-map "\C-x4l" 'find-library-other-window)
(define-key global-map "\C-x5l" 'find-library-other-frame)

(provide 'lib-complete)

;;; lib-complete.el ends here
