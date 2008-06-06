;;; autoload.el --- maintain autoloads in auto-autoloads files.

;; Copyright (C) 1991-1994, 1997, 2003 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 1996, 2000, 2002, 2003, 2004 Ben Wing.

;; Original Author: Roland McGrath <roland@gnu.ai.mit.edu>
;; Heavily Modified: XEmacs Maintainers
;; Keywords: maint

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

;;; Synched up with: FSF 21.2 by Ben Wing.
;;; Note that update-file-autoloads is seriously modified and not really
;;; syncable.

;;; Commentary:

;; This code keeps auto-autoloads.el files up to date.  It interprets
;; magic cookies (of the form ";;;###autoload" in Lisp source files
;; and "/* ###autoload */" in C source files) in various useful ways.
;; It is also used to maintain custom-defines.el files, since most of
;; the logic for computing them is the same as for auto-autoloads.el.

;; Usage
;; =====

;; Recommended usage for this library, as implemented in the core
;; build process, is

;; sxemacs -no-packages -batch \
;;        -l autoload -f batch-update-directory-autoloads PREFIX DIRECTORY

;; which causes XEmacs to update the file named by PATH from the .el
;; files in DIRECTORY (but not recursing into subdirectories) and (if
;; the autoload file is not already protected with a feature test) add
;; a check and provide for 'PREFIX-autoloads.  Currently there is no
;; sanity check for the provided feature; it is recommended that you
;; nuke any existing auto-autoloads.el before running the command.

;; There is not yet a recommended API for updating multiple directories
;; into a single auto-autoloads file.  Using the recipe above for each
;; DIRECTORY with the same PATH should work but has not been tested.
;; There is no API for recursing into subdirectories.  There probably
;; won't be; given the wide variety of ways that existing Lisp packages
;; arrange their files, and the fact that source packages and installed
;; packages have a somewhat different directory structure, this seems far
;; too risky.  Use a script or a Lisp library with an explicit list of
;; PATHs; see update-elc.el for how to do this without recursive invocation
;; of XEmacs).

;; The probable next step is to fix up the packages to use the
;; `batch-update-directory-autoloads' API.  However, for backward
;; compatibility with XEmacs 21.4 and 21.1, this can't be done quickly.

;; For backward compatibility the API used in the packages/XEmacs.rules:

;; sxemacs -vanilla -batch -eval "$(AUTOLOAD_PACKAGE_NAME)" \
;;        -l autoload -f batch-update-autoloads $(AUTOLOAD_PATH)

;; is supported, and the implementation is unchanged.  However,
;; revision of the API (in a backward compatible way) and the
;; implementation are planned, and until those stabilize it is too
;; risky to use this version of XEmacs for package releases.

;; Implementation:
;; ===============

;; #### This section should be moved to the Internals manual, or
;; (maybe) the Lispref, and integrated with the information above.
;; Don't believe anything written here; the code is still a mess, and
;; this is a lot of guesswork.

;; Autoloads are used in a number of contexts, including core Lisp,
;; packaged Lisp, and ELLs (dynamically loadable compiled objects
;; providing Lisp functionality).  There two general strategies for
;; collecting autoloads.  The first is to put autoloads for a package
;; in a package-specific auto-autoloads file.  This is easy to
;; implement, and allows packages to be distributed with prebuilt
;; auto-autoloads files.  The second is to collect all the autoloads
;; in a single global auto-autoloads file.  This is alleged to speed
;; up initialization significantly, but requires care to ensure that
;; auto-autoloads files remain synchronized with the libraries.

;; The traditional logic for determining where to put autoload
;; definitions is complex and is now deprecated.  The special variable
;; `generated-autoload-file' is used to hold the path to the file, and
;; is initialized to the traditional (well, it's a new tradition with
;; XEmacs 20) $blddir/lisp/auto-autoloads.el.  However, this variable
;; may be bound by calling code, or may be generated at collect time
;; and I'm not even sure the initialized value was ever used any more.

;; Because there may be multiple auto-autoloads files in use (in XEmacs
;; 21.x with a full complement of packages there are dozens), and they may
;; contain initializations that would be dangerous to reexecute, each is
;; protected by a feature test.  By convention, the feature symbol is of
;; the form "NAME-autoloads".  For packages, the special variable
;; `autoload-package-name' is used to determine NAME.  In the core,
;; autoloads are defined in the modules (all of which are collected in a
;; single auto-autoloads file), using NAME=modules, in the lisp directory
;; using NAME=lisp, and in the lisp/mule directory, using NAME=mule, for
;; the autoloads feature.  These latter are computed by the autoloads
;; function at collect time.

;; ChangeLog:

;; See ./ChangeLog.

;;; Code:

;; Need to load easy-mmode because we expand macro calls to easy-mmode
;; macros in make-autoloads below.
(require 'easy-mmode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard file and directory names

;; `autoload-file-name' is defvar'd and initialized in packages.el,
;; which is loaded (and dumped) very early.  If you find it isn't, you
;; know what you're doing.

(defconst autoload-target-directory "../lisp/"
  "Standard directory containing autoload declaration file.

Use `generated-autoload-file' (q.v.) to change its installation location.")

;; Dynamic variables for communication among functions

;; FSF 21.2:
;; The autoload file is assumed to contain a trailer starting with a FormFeed
;; character.

(defvar generated-autoload-file
  (expand-file-name autoload-file-name lisp-directory)
  "*File `update-file-autoloads' puts autoloads into.
A .el file can set this in its local variables section to make its
autoloads go somewhere else.

Note that `batch-update-directory' binds this variable to its own value,
generally the file named by `autoload-file-name' in the directory being
updated.  XEmacs.rules setq's this variable for package autoloads.")

(defvar generate-autoload-function
  #'generate-file-autoloads
  "Function to generate the autoloads for a file and insert at point.
Called with one argument, the file.")

(define-obsolete-variable-alias 'autoload-package-name
  'autoload-feature-prefix)
(defvar autoload-feature-prefix nil
  "If non-nil, use this string to prefix the autoload feature name.

Usually a package name (from AUTOLOAD_PACKAGE_NAME, defined in XEmacs.rules
in the top of the package hierarchy), or \"auto\" (reserved for the core Lisp
auto-autoloads file).  Highest priority candidate except for an explicit
argument to `autoload-make-feature-name' (q.v.).")

(defvar autoload-feature-suffix "-autoloads"
  "String added to `autoload-feature-prefix' to create the autoload feature name.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magic strings in source files

(defconst generate-autoload-cookie ";;;###autoload"
  "Magic comment indicating the following form should be autoloaded.
Used by `update-file-autoloads'.  This string should be
meaningless to Lisp (e.g., a comment).

This string is used:

;;;###autoload
\(defun function-to-be-autoloaded () ...)

If this string appears alone on a line, the following form will be
read and an autoload made for it.  If it is followed by the string
\"immediate\", then the form on the following line will be copied
verbatim.  If there is further text on the line, that text will be
copied verbatim to `generated-autoload-file'.")

(defconst generate-c-autoload-cookie "/* ###autoload"
  "Magic C comment indicating the following form should be autoloaded.
Used by `update-file-autoloads'.  This string should be
meaningless to C (e.g., a comment).

This string is used:

/* ###autoload */
DEFUN (\"function-to-be-autoloaded\", ... )

If this string appears alone on a line, the following form will be
read and an autoload made for it.  If there is further text on the line,
that text will be copied verbatim to `generated-autoload-file'.")

(defconst generate-c-autoload-module "/* ###module"
  "Magic C comment indicating the module containing autoloaded functions.
Since a module can consist of multiple C files, the module name may not be
the same as the C source file base name.  In that case, use this comment to
indicate the actual name of the module from which to autoload functions.")

(defconst generate-autoload-section-header "\f\n;;;### "
  "String inserted before the form identifying
the section of autoloads for a file.")

(defconst generate-autoload-section-trailer "\n;;;***\n"
  "String which indicates the end of the section of autoloads for a file.")

(defconst generate-autoload-section-continuation ";;;;;; "
  "String to add on each continuation of the section header form.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing the source file text.
;; Autoloads in C source differ from those in Lisp source.

(defun make-autoload (form file)
  "Turn FORM into an autoload or defvar for source file FILE.
Returns nil if FORM is not a special autoload form (i.e. a function definition
or macro definition or a defcustom)."
  (let ((car (car-safe form)) expand)
    (cond
     ;; For complex cases, try again on the macro-expansion.
     ((and (memq car '(easy-mmode-define-global-mode
		       easy-mmode-define-minor-mode define-minor-mode))
	   (setq expand (let ((load-file-name file)) (macroexpand form)))
	   (eq (car expand) 'progn)
	   (memq :autoload-end expand))
      (let ((end (memq :autoload-end expand)))
	;; Cut-off anything after the :autoload-end marker.
	(setcdr end nil)
	(cons 'progn
	      (mapcar (lambda (form) (make-autoload form file))
		      (cdr expand)))))

     ;; For special function-like operators, use the `autoload' function.
     ((memq car '(defun define-skeleton defmacro define-derived-mode
		   define-generic-mode easy-mmode-define-minor-mode
		   easy-mmode-define-global-mode
		   define-minor-mode defun* defmacro*))
      (let* ((macrop (memq car '(defmacro defmacro*)))
	     (name (nth 1 form))
	     (body (nthcdr (get car 'doc-string-elt) form))
	     (doc (if (stringp (car body)) (pop body))))
	;; `define-generic-mode' quotes the name, so take care of that
	(list 'autoload (if (listp name) name (list 'quote name)) file doc
	      (or (and (memq car '(define-skeleton define-derived-mode
				    define-generic-mode
				    easy-mmode-define-global-mode
				    easy-mmode-define-minor-mode
				    define-minor-mode)) t)
		  (eq (car-safe (car body)) 'interactive))
	      (if macrop (list 'quote 'macro) nil))))

     ;; Convert defcustom to a simpler (and less space-consuming) defvar,
     ;; but add some extra stuff if it uses :require.
     ((eq car 'defcustom)
      (let ((varname (car-safe (cdr-safe form)))
	    (init (car-safe (cdr-safe (cdr-safe form))))
	    (doc (car-safe (cdr-safe (cdr-safe (cdr-safe form)))))
	    (rest (cdr-safe (cdr-safe (cdr-safe (cdr-safe form))))))
	(if (not (plist-get rest :require))
	    `(defvar ,varname ,init ,doc)
	  `(progn
	     (defvar ,varname ,init ,doc)
	     (custom-add-to-group ,(plist-get rest :group)
				  ',varname 'custom-variable)
	     (custom-add-load ',varname
			      ,(plist-get rest :require))))))

     ;; nil here indicates that this is not a special autoload form.
     (t nil))))

(defun make-c-autoload (module)
  "Make an autoload list for the DEFUN at point in MODULE.
Returns nil if the DEFUN is malformed."
  (and
   ;; Match the DEFUN
   (search-forward "DEFUN" nil t)
   ;; Match the opening parenthesis
   (progn
     (skip-syntax-forward " ")
     (eq (char-after) ?\())
   ;; Match the opening quote of the Lisp function name
   (progn
     (forward-char)
     (skip-syntax-forward " ")
     (eq (char-after) ?\"))
   ;; Extract the Lisp function name, interactive indicator, and docstring
   (let* ((func-name (let ((begin (progn (forward-char) (point))))
		       (search-forward "\"" nil t)
		       (backward-char)
		       (intern (buffer-substring begin (point)))))
	  (interact (progn
		      (search-forward "," nil t 4)
		      (skip-syntax-forward " ")
		      (not (eq (char-after) ?0))))
	  (begin (progn
		   (search-forward "/*" nil t)
		   (forward-line 1)
		   (point))))
     (search-forward "*/" nil t)
     (goto-char (match-beginning 0))
     (skip-chars-backward " \t\n\f")
     (list 'autoload (list 'quote func-name) module
	   (buffer-substring begin (point)) interact nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating autoloads for a single file

;;;###autoload
(defun generate-file-autoloads (file)
  "Insert at point an autoload section for FILE.
autoloads are generated for defuns and defmacros in FILE
marked by `generate-autoload-cookie' (which see).
If FILE is being visited in a buffer, the contents of the buffer
are used."
  (interactive "fGenerate autoloads for file: ")
  (cond ((string-match "\\.el$" file)
	 (generate-autoload-type-section
	  file
	  (replace-in-string (file-name-nondirectory file) "\\.elc?$" "")
	  nil #'generate-lisp-file-autoloads-1))
	;; #### jj, are C++ modules possible?
	((string-match "\\.c$" file)
	 (generate-autoload-type-section
	  file
	  (replace-in-string (file-name-nondirectory file) "\\.c$" "")
	  t #'generate-c-file-autoloads-1))
	(t
	 (error 'wrong-type-argument file "not a C or Elisp source file"))))

(defun* generate-autoload-type-section (file load-name literal fun-to-call)
  "Insert at point an autoload-type section for FILE.
LOAD-NAME is the non-directory portion of the name, with the final .el, .elc
or .c section removed.  If LITERAL, open the file literally, without decoding.
Calls FUN-TO-CALL to compute the autoloads, with the loaded file in the
current buffer, passing it OUTBUF (where to write the autoloads), LOAD-NAME,
and TRIM-NAME (result of calling `autoload-trim-file-name' on FILE)."
  (let ((outbuf (current-buffer))
	(trim-name (autoload-trim-file-name file))
	(autoloads-done '())
	(print-length nil)
	(print-readably t) ; XEmacs
	(float-output-format nil)
	(visited (get-file-buffer file))
	suppress-form
	;; (done-any nil)
	output-end)

    ;; If the autoload section we create here uses an absolute
    ;; pathname for FILE in its header, and then Emacs is installed
    ;; under a different path on another system,
    ;; `update-autoloads-here' won't be able to find the files to be
    ;; autoloaded.  So, if FILE is in the same directory or a
    ;; subdirectory of the current buffer's directory, we'll make it
    ;; relative to the current buffer's directory.
    (setq file (expand-file-name file))
    ;; #### FSF 21.2.  Do we want this?
;     (let* ((source-truename (file-truename file))
; 	   (dir-truename (file-name-as-directory
; 			  (file-truename default-directory)))
; 	   (len (length dir-truename)))
;       (if (and (< len (length source-truename))
; 	       (string= dir-truename (substring source-truename 0 len)))
; 	  (setq file (substring source-truename len))))

    ;; Check for suppression form (XEmacs)
    (let* ((dir (file-name-directory file))
	   (_pkg (expand-file-name "_pkg.el" dir))
	   (pkg-vis (get-file-buffer _pkg))
	   pkg-buf)
      (save-excursion
	(when (file-readable-p _pkg)
	  (unwind-protect
	      (progn
		(let ((find-file-hooks nil)
		      (enable-local-variables nil))
		  (set-buffer (or pkg-vis (find-file-noselect _pkg)))
		  (set-syntax-table emacs-lisp-mode-syntax-table))
		(save-excursion
		  (save-restriction
		    (widen)
		    (goto-char (point-min))
		    (block nil
		      (while (search-forward "(package-suppress" nil t)
			;; skip over package-name
			(forward-sexp 1)
			(let ((supfile (read (current-buffer))))
			  (when (equal supfile load-name)
			    (setq suppress-form (eval (read (current-buffer))))
			    (return))))))))
	    (unless pkg-vis
	      ;; We created this buffer, so we should kill it.
	      (if pkg-buf (kill-buffer pkg-buf)))))))

    (save-excursion
      (unwind-protect
	  (progn
	    (let (;(find-file-hooks nil)
		  ;(enable-local-variables nil)
		  )
	      (set-buffer (or visited (find-file-noselect file literal literal
							  )))
	      ;; This doesn't look right for C files, but it is.  The only
	      ;; place we need the syntax table is when snarfing the Lisp
	      ;; function name.
	      (set-syntax-table emacs-lisp-mode-syntax-table))
; 	    (if visited
; 		(set-buffer visited)
; 	      ;; It is faster to avoid visiting the file.
; 	      (set-buffer (get-buffer-create " *generate-autoload-file*"))
; 	      (kill-all-local-variables)
; 	      (erase-buffer)
; 	      (setq buffer-undo-list t
; 		    buffer-read-only nil)
; 	      ;; This doesn't look right for C files, but it is.  The only
; 	      ;; place we need the syntax table is when snarfing the Lisp
; 	      ;; function name.
; 	      (emacs-lisp-mode)
; 	      (if literal
; 		  (insert-file-contents-literally file nil)
; 		(insert-file-contents file nil)))
	    (unless (setq autoloads-done
			  (funcall fun-to-call outbuf load-name trim-name))
	      (return-from generate-autoload-type-section))
	    )
	(unless visited
	  ;; We created this buffer, so we should kill it.
	  (kill-buffer (current-buffer)))
	(set-buffer outbuf)
	(setq output-end (point-marker))))
    (if t ;; done-any
	;; XEmacs -- always do this so that we cache the information
	;; that we've processed the file already.
	(progn
	  ;; Insert the section-header line
	  ;; which lists the file name and which functions are in it, etc.
	  (insert generate-autoload-section-header)
	  (prin1 (list 'autoloads autoloads-done load-name trim-name
		       ;; In FSF 21.2.  Also in FSF 19.30.  Presumably
		       ;; deleted from XEmacs.
		       ;; (nth 5 (file-attributes file))
		       )
		 outbuf)
	  (terpri outbuf)
	  ;; #### Alas, we will have to think about this.  Adding this means
	  ;; that, once we have created or maintained an auto-autoloads file,
	  ;; we alone and our successors can update the file.  The file itself
	  ;; will work fine in older XEmacsen, but they won't be able to
	  ;; update autoloads -- hence, to build.
; 	  ;; Break that line at spaces, to avoid very long lines.
; 	  ;; Make each sub-line into a comment.
; 	  (with-current-buffer outbuf
; 	    (save-excursion
; 	      (forward-line -1)
; 	      (while (not (eolp))
; 		(move-to-column 64)
; 		(skip-chars-forward "^ \n")
; 		(or (eolp)
; 		    (insert "\n" generate-autoload-section-continuation)))))
	  ;; XEmacs: This was commented out before.  #### Correct?
; 	  (insert ";;; Generated autoloads from "
; 		  (autoload-trim-file-name file) "\n")
	  ;; XEmacs -- handle suppression
	  (when suppress-form
	    (insert "\n;;; Suppress form from _pkg.el\n")
	    (insert "(unless " (prin1-to-string suppress-form) "\n\n"))
	  (goto-char output-end)
	  ;; XEmacs -- handle suppression
	  (when suppress-form
	    (insert "\n) ;; unless (suppressed)\n"))
	  (insert generate-autoload-section-trailer)))
    ))


(defun process-one-lisp-autoload (autoloads-done outbuf load-name)
  "Process a single autoload at point and write to OUTBUF.
Point should be just after a magic cookie string (e.g. ;;;###autoload).
Updates AUTOLOADS-DONE and returns the new value."
  (skip-chars-forward " \t")
  ;; (setq done-any t)
  (if (eolp)
      ;; Read the next form and make an autoload.
      (let* ((form (prog1 (read (current-buffer))
		     (or (bolp) (forward-line 1))))
	     (autoload (make-autoload form load-name)))
	(if autoload
	    (setq autoloads-done (cons (nth 1 form)
				       autoloads-done))
	  (setq autoload form))
	(autoload-print-form autoload outbuf ""))
    ;; Copy the rest of the line to the output.
    (cond ((looking-at "immediate\\s *$") ; XEmacs
	   ;; This is here so that you can automatically
	   ;; have small hook functions copied to
	   ;; auto-autoloads.el so that it's not necessary
	   ;; to load a whole file just to get a two-line
	   ;; do-nothing find-file-hook... --Stig
	   (forward-line 1)
	   (let ((begin (point)))
	     (forward-sexp)
	     (forward-line 1)
	     (princ (buffer-substring begin (point)) outbuf)))
	  (t
	   (princ (buffer-substring
		   (progn
		     ;; Back up over whitespace, to preserve it.
		     (skip-chars-backward " \f\t")
		     (if (= (char-after (1+ (point))) ? )
			 ;; Eat one space.
			 (forward-char 1))
		     (point))
		   (progn (forward-line 1) (point)))
		  outbuf))))
  autoloads-done)

(defun* generate-lisp-file-autoloads-1 (outbuf load-name trim-name)
  "Insert at point in OUTBUF an autoload section for an Elisp file.
The file is assumed to be already loaded and in the current buffer.
autoloads are generated for defuns and defmacros marked by
`generate-autoload-cookie' (which see)."
  (let ((autoloads-done '())
	)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(unless (search-forward generate-autoload-cookie nil t)
	  (message "No autoloads found in %s" trim-name)
	  (return-from generate-lisp-file-autoloads-1 nil))

	(message "Generating autoloads for %s..." trim-name)
	(goto-char (point-min))
	(while (not (eobp))
	  (skip-chars-forward " \t\n\f")
	  (cond
	   ((looking-at (regexp-quote generate-autoload-cookie))
	    (search-forward generate-autoload-cookie)
	    (setq autoloads-done
		  (process-one-lisp-autoload autoloads-done outbuf load-name)))
	   ((looking-at ";")
	    ;; Don't read the comment.
	    (forward-line 1))
	   (t
	    (forward-sexp 1)
	    (forward-line 1)))
	  )))
    (or noninteractive ; XEmacs: only need one line in -batch mode.
	(message "Generating autoloads for %s...done" trim-name))
    autoloads-done))

(defun* generate-c-file-autoloads-1 (outbuf load-name trim-name
				     &optional funlist)
  "Insert at point an autoload section for the C file FILE.
autoloads are generated for defuns and defmacros in FILE
marked by `generate-c-autoload-cookie' (which see).
If FILE is being visited in a buffer, the contents of the buffer
are used."
  (let (autoloads-done)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	;; Is there a module name comment?
	(when (search-forward generate-c-autoload-module nil t)
	  (skip-chars-forward " \t")
	  (let ((begin (point)))
	    (skip-chars-forward "^ \t\n\f")
	    (setq load-name (buffer-substring begin (point)))))
	(if funlist
	    (progn
	      (message "Generating autoloads for %s..." trim-name)
	      (dolist (arg funlist)
		(goto-char (point-min))
		(re-search-forward
		 (concat "DEFUN (\""
			 (regexp-quote (symbol-name arg))
			 "\""))
		(goto-char (match-beginning 0))
		(let ((autoload (make-c-autoload load-name)))
		  (when autoload
		    (push (nth 1 (nth 1 autoload)) autoloads-done)
		    (autoload-print-form autoload outbuf "")))))
	  (goto-char (point-min))
	  (let ((match
		 (search-forward generate-c-autoload-cookie nil t)))
	    (unless match
	      (message "No autoloads found in %s" trim-name)
	      (return-from generate-c-file-autoloads-1 nil))
	    (message "Generating autoloads for %s..." trim-name)
	    (while match
	      (forward-line 1)
	      (let ((autoload (make-c-autoload load-name)))
		(when autoload
		  (push (nth 1 (nth 1 autoload)) autoloads-done)
		  (autoload-print-form autoload outbuf "")))
	      (setq match
		    (search-forward generate-c-autoload-cookie nil t)))))))
    (or noninteractive ; XEmacs: only need one line in -batch mode.
	(message "Generating autoloads for %s...done" trim-name))
    autoloads-done))

;;;###autoload
(defun generate-custom-defines (file)
  "Insert at point a custom-define section for FILE.
If FILE is being visited in a buffer, the contents of the buffer
are used."
  (interactive "fGenerate custom defines for file: ")
  (cond ((string-match "\\.el$" file)
	 (generate-autoload-type-section
	  file
	  (replace-in-string (file-name-nondirectory file) "\\.elc?$" "")
	  nil #'generate-custom-defines-1))
	((string-match "\\.c$" file)
	 ;; no way to generate custom-defines for C files (currently?),
	 ;; but cannot signal an error.
	 nil)
	(t
	 (error 'wrong-type-argument file "not a C or Elisp source file"))))

(defun* generate-custom-defines-1 (outbuf load-name trim-name)
  "Insert at point in OUTBUF a custom-define section for an Elisp file.
This contains all defcustoms and defgroups in the file.
The file is assumed to be already loaded and in the current buffer."
  (let* ((search-regexp-1 "^(\\(defcustom\\|defgroup\\) ")
	 (search-string-2 ";;;###custom-define")
	 (search-regexp-2 (regexp-quote search-string-2))
	 (autoloads-done '()))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(unless (or (re-search-forward search-regexp-1 nil t)
		    (re-search-forward search-regexp-2 nil t))
	  (message "No custom defines found in %s" trim-name)
	  (return-from generate-custom-defines-1 nil))
	(message "Generating custom defines for %s..." trim-name)
	(princ "(defconst custom-define-current-source-file " outbuf)
	(prin1 (file-relative-name (buffer-file-name)
				   (symbol-value-in-buffer 'default-directory
							   outbuf)) outbuf)
	(princ ")\n" outbuf)
	       
	(goto-char (point-min))
	(while (not (eobp))
	  (skip-chars-forward " \t\n\f")
	  (cond
	   ((looking-at search-regexp-1)
	    ;; Read the next form and copy it to make an autoload.
	    (let* ((form (prog1 (read (current-buffer))
			   (or (bolp) (forward-line 1))))
		   (autoload form ;(make-autoload form load-name)
		     ))
	      (if autoload
		  (setq autoloads-done (cons (nth 1 form)
					     autoloads-done))
		(setq autoload form))
	      (autoload-print-form autoload outbuf ""))
	    )
	   ((looking-at search-regexp-2)
	    (search-forward search-string-2)
	    (beep)
	    (setq autoloads-done
		  (process-one-lisp-autoload autoloads-done outbuf load-name)))
	   ((looking-at ";")
	    ;; Don't read the comment.
	    (forward-line 1))
	   (t
	    (forward-sexp 1)
	    (forward-line 1)))
	  )))
    (or noninteractive ; XEmacs: only need one line in -batch mode.
	(message "Generating custom defines for %s...done" trim-name))
    autoloads-done))

;; Assorted utilities for generating autoloads and pieces thereof

(defun autoload-print-form (form outbuf margin)
  "Print an autoload form, handling special characters.
In particular, print docstrings with escapes inserted before left parentheses
at the beginning of lines and ^L characters."
  (cond
   ;; If the form is a sequence, recurse.
   ((eq (car form) 'progn)
    (mapcar #'(lambda (x) (autoload-print-form x outbuf margin))
	    (cdr form)))
   ;; Symbols at the toplevel are meaningless.
   ((symbolp form) nil)
   (t
    (let ((doc-string-elt (get (car-safe form) 'doc-string-elt)))
      (if (and doc-string-elt (stringp (nth doc-string-elt form)))
	  ;; We need to hack the printing because the doc-string must be
	  ;; printed specially for make-docfile (sigh).
	  (let* ((p (nthcdr (1- doc-string-elt) form))
		 (elt (cdr p))
		 (start-string (format "\n%s(" margin)))
	    (setcdr p nil)
	    (princ start-string outbuf)
	    ;; XEmacs change: don't let ^^L's get into
	    ;; the file or sorting is hard.
	    (let ((print-escape-newlines t)
		  ;;#### FSF 21.2 (print-escape-nonascii t)
		  (p (point outbuf))
		  p2)
	      (mapcar #'(lambda (elt)
			  (prin1 elt outbuf)
			  (princ " " outbuf))
		      form)
	      (with-current-buffer outbuf
		(setq p2 (point-marker))
		(goto-char p)
		(save-match-data
		  (while (search-forward "\^L" p2 t)
		    (delete-char -1)
		    (insert "\\^L")))
		(goto-char p2)))
	    (princ "\"\\\n" outbuf)
	    (let ((begin (point outbuf)))
	      (princ (substring (prin1-to-string (car elt)) 1) outbuf)
	      ;; Insert a backslash before each ( that appears at the beginning
	      ;; of a line in the doc string.
	      (with-current-buffer outbuf
		(save-excursion
		  (while (search-backward start-string begin t)
		    (forward-char 1)
		    (insert "\\"))))
	      (if (null (cdr elt))
		  (princ ")" outbuf)
		(princ " " outbuf)
		(princ (substring (prin1-to-string (cdr elt)) 1) outbuf))
	      (terpri outbuf)
	      (princ margin outbuf)))
	;; XEmacs change: another ^L hack
	(let ((p (point outbuf))
	      (print-escape-newlines t)
	      ;;#### FSF 21.2 (print-escape-nonascii t)
	      p2)
	  (print form outbuf)
	  (with-current-buffer outbuf
	    (setq p2 (point-marker))
	    (goto-char p)
	    (save-match-data
	      (while (search-forward "\^L" p2 t)
		(delete-char -1)
		(insert "\\^L")))
	    (goto-char p2))))))))

;;; Forms which have doc-strings which should be printed specially.
;;; A doc-string-elt property of ELT says that (nth ELT FORM) is
;;; the doc-string in FORM.
;;;
;;; There used to be the following note here:
;;; ;;; Note: defconst and defvar should NOT be marked in this way.
;;; ;;; We don't want to produce defconsts and defvars that
;;; ;;; make-docfile can grok, because then it would grok them twice,
;;; ;;; once in foo.el (where they are given with ;;;###autoload) and
;;; ;;; once in loaddefs.el.
;;;
;;; Counter-note: Yes, they should be marked in this way.
;;; make-docfile only processes those files that are loaded into the
;;; dumped Emacs, and those files should never have anything
;;; autoloaded here.  The above-feared problem only occurs with files
;;; which have autoloaded entries *and* are processed by make-docfile;
;;; there should be no such files.

(put 'autoload 'doc-string-elt 3)
(put 'defun    'doc-string-elt 3)
(put 'defun*   'doc-string-elt 3)
(put 'defvar   'doc-string-elt 3)
(put 'defcustom 'doc-string-elt 3)
(put 'defconst 'doc-string-elt 3)
(put 'defmacro 'doc-string-elt 3)
(put 'defmacro* 'doc-string-elt 3)
(put 'defsubst 'doc-string-elt 3)
(put 'define-skeleton 'doc-string-elt 2)
(put 'define-derived-mode 'doc-string-elt 4)
(put 'easy-mmode-define-minor-mode 'doc-string-elt 2)
(put 'define-minor-mode 'doc-string-elt 2)
(put 'define-generic-mode 'doc-string-elt 7)
;; defin-global-mode has no explicit docstring.
(put 'easy-mmode-define-global-mode 'doc-string-elt 1000)

(defun autoload-trim-file-name (file)
  "Returns relative pathname of FILE including the last directory."
  (setq file (expand-file-name file))
  (file-relative-name file (file-name-directory
                            (directory-file-name
                             (file-name-directory file)))))

(defun autoload-read-section-header ()
  "Read a section header form.
Since continuation lines have been marked as comments,
we must copy the text of the form and remove those comment
markers before we call `read'."
  (save-match-data
    (let ((beginning (point))
	  string)
      (forward-line 1)
      (while (looking-at generate-autoload-section-continuation)
	(forward-line 1))
      (setq string (buffer-substring beginning (point)))
      (with-current-buffer (get-buffer-create " *autoload*")
	(erase-buffer)
	(insert string)
	(goto-char (point-min))
	(while (search-forward generate-autoload-section-continuation nil t)
	  (replace-match " "))
	(goto-char (point-min))
	(read (current-buffer))))))

;;;###autoload
(defun update-file-autoloads (file)
  "Update the autoloads for FILE in `generated-autoload-file'
\(which FILE might bind in its local variables).
This function is a no-op for an autoloads file (ie, a file whose name is
equal to `autoload-file-name')."
  (interactive "fUpdate autoloads for file: ")
  (setq file (expand-file-name file))
  (when (and (file-newer-than-file-p file generated-autoload-file)
	     (not (member (file-name-nondirectory file)
			  (list autoload-file-name))))

    (let ((load-name (replace-in-string (file-name-nondirectory file)
					"\\.\\(elc?\\|c\\)$"
					""))
	  (trim-name (autoload-trim-file-name file))
	  section-begin form)
      (save-excursion
	;; FSF has: [[ We want to get a value for generated-autoload-file
	;; from the local variables section if it's there. ]] Not
	;; applicable in XEmacs, since we always keep the autoloads
	;; up-to-date.

	;; #### FSF 21.2 adds: [[ We must read/write the file without any
	;; code conversion, but still decode EOLs. ]] Not clear if we need
	;; this. --ben
	;; (let ((coding-system-for-read 'raw-text))
	(let ((find-file-hooks nil))
	  (set-buffer (or (get-file-buffer generated-autoload-file)
			  (find-file-noselect generated-autoload-file))))
	;; FSF 21.2 says:

	;; [[ This is to make generated-autoload-file have Unix EOLs, so
	;; that it is portable to all platforms. ]]
	;; (setq buffer-file-coding-system 'raw-text-unix))
	;; Not applicable in XEmacs, since we always keep the autoloads
	;; up-to-date and recompile when we build.

	;; FSF 21.2: [not applicable to XEmacs]
; 	(or (> (buffer-size) 0)
; 	    (error "Autoloads file %s does not exist" buffer-file-name))
; 	(or (file-writable-p buffer-file-name)
; 	    (error "Autoloads file %s is not writable" buffer-file-name))

	;; NOTE: The rest of this function is totally changed from FSF.
	;; Hence, not synched.

	;; Make sure we can scribble in it.
	(setq buffer-read-only nil)
	;; First delete all sections for this file.
	(goto-char (point-min))
	(while (search-forward generate-autoload-section-header nil t)
	  (setq section-begin (match-beginning 0))
	  (setq form (autoload-read-section-header))
	  (when (string= (nth 2 form) load-name)
	    (search-forward generate-autoload-section-trailer)
	    (delete-region section-begin (point))))

	;; Now find insertion point for new section
	(block find-insertion-point
	  (goto-char (point-min))
	  (while (search-forward generate-autoload-section-header nil t)
	    (setq form (autoload-read-section-header))
	    (when (string< trim-name (nth 3 form))
	      ;; Found alphabetically correct insertion point
	      (goto-char (match-beginning 0))
	      (return-from find-insertion-point))
	    (search-forward generate-autoload-section-trailer))
	  (when (eq (point) (point-min))	; No existing entries?
	    (goto-char (point-max))))	; Append.

	;; Add in new sections for file
	(funcall generate-autoload-function file))

      (when (interactive-p) (save-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for batch updates

(defun parse-command-line (cmdl)
  (let ((newcmdl (dllist))
        (cmdlpl (make-skiplist))
        (mm (compile-regexp "^--"))
        (ign (compile-regexp #r"^-\(?:batch\|nd\|vanilla\)"))
        (ign2 (compile-regexp "^-[lf]$")))
    (while (car cmdl)
      (cond ((string-match mm (car cmdl))
             (let ((key (intern (car cmdl)))
                   (val (car (cdr-safe cmdl))))
               (put-skiplist cmdlpl key val)
               (setq cmdl (cdr-safe cmdl))))
            ((string-match ign2 (car cmdl))
             ;; ignore a pair of parameters
             (setq cmdl (cdr-safe cmdl)))
            ((string-match ign (car cmdl))
             ;; ignore this single parameter
             )
            (t
             (dllist-append newcmdl (car cmdl))))
      (setq cmdl (cdr-safe cmdl)))
    (put newcmdl :tweaks cmdlpl)
    newcmdl))

;;;###autoload
(defun batch-create-autoloads ()
  "Update the autoloads for one or more directories.

#### The API and semantics of this function are subject to change."
  (unless noninteractive
    (error "batch-update-directory-autoloads: may be used only with -batch"))

  (let* ((cmds (parse-command-line (cdr-safe command-line-args)))
         (pl (get cmds :tweaks)))

    (unless (get-skiplist pl '--autoload-dir-name)
      (put-skiplist pl '--autoload-dir-name
                    (expand-file-name default-directory)))
    (unless (get-skiplist pl '--autoload-file-name)
      (put-skiplist pl '--autoload-file-name
                    (expand-file-name autoload-file-name
                                      (get-skiplist pl '--autoload-dir-name))))
    (unless (get-skiplist pl '--feature-prefix)
      (put-skiplist pl '--feature-prefix
                    (file-name-nondirectory
                     (directory-file-name
                      (expand-file-name
                       (get-skiplist pl '--autoload-dir-name))))))

    (create-autoload-files cmds t)))

(defun create-autoload-files (bunch &optional force)
  (let* ((pl (get bunch :tweaks))
         (relative (get-skiplist pl '--relative-to))
         (al-file (get-skiplist pl '--autoload-file-name))
         (fprefix (get-skiplist pl '--feature-prefix)))
    (mapc-inplace
     #'(lambda (dir)
         (expand-file-name dir relative))
     bunch)
    (update-autoload-files (dllist-to-list bunch) fprefix al-file force)))

;;;###autoload
(defun batch-update-directory-autoloads ()
  "Update the autoloads for a directory, using a specified feature prefix.
Must be used only with -batch.  The feature prefix and directory to update
are taken from the first and second elements of `command-line-args-left',
respectively, and they are then removed from `command-line-args-left'.

Runs `update-file-autoloads' on each file in the given directory.  Always
rewrites the autoloads file, even if unchanged.  Makes a feature name by
applying `autoload-make-feature-name' to the specified feature prefix.

#### The API and semantics of this function are subject to change."
  (unless noninteractive
    (error "batch-update-directory-autoloads: may be used only with -batch"))
  (update-autoload-files (list (cadr command-line-args-left))
			 (car command-line-args-left) nil t)
  (setq command-line-args-left (cddr command-line-args-left)))

;;;###autoload
(defun batch-update-directory-custom-defines ()
  "Update the custom defines for a directory, using a specified feature prefix.
Must be used only with -batch.  The feature prefix and directory to update
are taken from the first and second elements of `command-line-args-left',
respectively, and they are then removed from `command-line-args-left'.

Runs `update-file-autoloads' on each file in the given directory.  Always
rewrites the autoloads file, even if unchanged.  Makes a feature name by
applying `autoload-make-feature-name' to the specified feature prefix.

#### The API and semantics of this function are subject to change."
  (unless noninteractive
    (error "batch-update-directory-custom-defines: may be used only with -batch"))
  (update-custom-define-files (list (cadr command-line-args-left))
			      (car command-line-args-left) nil t)
  (setq command-line-args-left (cddr command-line-args-left)))

;;;###autoload
(defun update-autoload-files (files-or-dirs feature-prefix
			      &optional into-file force)
  "Update all the autoload files associated with FILES-OR-DIRS.
FILES-OR-DIRS is a list of files and/or directories to be processed.

An appropriate autoload file is chosen and a feature constructed for
each element of FILES-OR-DIRS.  Fixup code testing for the autoload file's
feature and to provide the feature is added.

If optional INTO-FILE is non-`nil', it should specify a file into which
the autoloads will be placed.  Otherwise, the autoloads will be placed into
a file named `auto-autoloads.el' in the directory of each element in
FILES-OR-DIRS.

FEATURE-PREFIX should be set to an appropriate prefix which will
be concatenated with \"-autoloads\" to produce the feature name.  Otherwise
the appropriate autoload file for each file or directory (located in that
directory, or in the directory of the specified file) will be updated with
the directory's or file's autoloads and the protective forms will be added,
and the files will be saved.  Use of the default here is unreliable, and
therefore deprecated.

Note that if some of FILES-OR-DIRS are directories, recursion goes only
one level deep.

If FORCE is non-nil, always save out the autoload files even if unchanged."
  (or (listp files-or-dirs) (setq files-or-dirs (list files-or-dirs)))
  (let ((defdir (directory-file-name default-directory))
	;; value for all-into-one-file
	(autoload-feature-name (autoload-make-feature-name feature-prefix))
	(enable-local-eval nil)	; Don't query in batch mode.
	(autoload-feature-prefix feature-prefix)
	;; protect from change
	(generated-autoload-file generated-autoload-file))
    (dolist (arg files-or-dirs)
      (setq arg (expand-file-name arg defdir))
      (cond
       ((file-directory-p arg)
	(setq generated-autoload-file
	      (or into-file (expand-file-name autoload-file-name arg)))
	(message "Updating autoloads for directory %s..." arg)
	(let ((simple-dir (file-name-as-directory
			   (file-name-nondirectory
			    (directory-file-name arg))))
	      (enable-local-eval nil))
	  (save-excursion
	    (let ((find-file-hooks nil))
	      (set-buffer (find-file-noselect generated-autoload-file)))
	    (goto-char (point-min))
	    (while (search-forward generate-autoload-section-header nil t)
	      (let* ((begin (match-beginning 0))
		     (form (autoload-read-section-header))
		     (file (nth 3 form)))
		(when (and (stringp file)
			   (string= (file-name-directory file) simple-dir)
			   (not (file-exists-p
				 (expand-file-name
				  (file-name-nondirectory file) arg))))
		  ;; Remove the obsolete section.
		  (search-forward generate-autoload-section-trailer)
		  (delete-region begin (point)))))
	    ;; Update or create autoload sections for existing files.
	    (mapcar 'update-file-autoloads
		    (directory-files arg t "^[^=].*\\.\\(el\\|c\\)$")))))
       ((file-exists-p arg)
	(setq generated-autoload-file
	      (or into-file (expand-file-name autoload-file-name
					      (file-name-directory arg))))
	(update-file-autoloads arg))
       (t (error "No such file or directory: %s" arg)))
      (when (not into-file)
	(autoload-featurep-protect-autoloads
	 (autoload-make-feature-name
	  (or feature-prefix
	      (file-name-nondirectory (directory-file-name arg)))))
	(if force (set-buffer-modified-p
		   t (find-file-noselect generated-autoload-file)))))
    (when into-file
      (autoload-featurep-protect-autoloads autoload-feature-name)
      (if force (set-buffer-modified-p
		 t (find-file-noselect into-file))))
    (save-some-buffers t)
    ))

;;;###autoload
(defun update-custom-define-files (files-or-dirs feature-prefix
				   &optional into-file force)
  "Update all the custom-define files associated with FILES-OR-DIRS.
Works just like `update-file-autoloads'."
  (let* ((autoload-feature-suffix "-custom-defines")
	 (autoload-file-name "custom-defines.el")
	 (generate-autoload-function #'generate-custom-defines))
    (update-autoload-files files-or-dirs feature-prefix into-file force)))

(defun autoload-featurep-protect-autoloads (sym)
  (save-excursion
    (set-buffer (find-file-noselect generated-autoload-file))
    (goto-char (point-min))
    (cond ((eq (point-min) (point-max)) nil)
	  ;; if there's some junk in the file but no sections, just
	  ;; delete everything.  the junk might be stuff inserted by
	  ;; an older version of this function.
	  ((not (search-forward generate-autoload-section-header nil t))
	   (delete-region (point-min) (point-max)))
	  (t
	   (goto-char (point-min))
	   (when (looking-at ";;; DO NOT MODIFY THIS FILE")
	     (delete-region (point-min)
			    (progn
			      (search-forward generate-autoload-section-header)
			      (match-beginning 0))))
	   ;; Determine and set the coding system for the file if under Mule.
	   ;; If there are any extended characters in the input file, use
	   ;; `escape-quoted' to make sure that both binary and extended
	   ;; characters are output properly and distinguished properly.
	   ;; Otherwise, use `raw-text' for maximum portability with non-Mule
	   ;; Emacsen.
	   (if (or (featurep '(not mule)) ;; Don't scan if no Mule support
		   (progn
		     (goto-char (point-min))
		     ;; mrb- There must be a better way than skip-chars-forward
		     (skip-chars-forward (concat (char-to-string 0) "-"
						 (char-to-string 255)))
		     (eq (point) (point-max))))
	       (setq buffer-file-coding-system 'raw-text-unix)
	     (setq buffer-file-coding-system 'escape-quoted))
	   (goto-char (point-min))
	   (insert ";;; DO NOT MODIFY THIS FILE")
	   ;; NOTE: XEmacs prior to 21.5.12 or so had a bug in that it
	   ;; recognized only one of the two magic-cookie styles (the -*- kind)
	   ;; in find-file, but both of them in load.  We go ahead and put both
	   ;; in, just to be safe.
	   (when (eq buffer-file-coding-system 'escape-quoted)
	     (insert " -*- coding: escape-quoted; -*-
\(or (featurep 'mule) (error \"Loading this file requires Mule support\"))
;;;###coding system: escape-quoted"))
	   (insert "\n(if (featurep '" sym ")")
	   (insert " (error \"Feature " sym " already loaded\"))\n")
	   (goto-char (point-max))
	   (save-excursion
	     (forward-line -1)
	     (when (looking-at "(provide")
	       (delete-region (point) (point-max))))
	   (unless (bolp) (insert "\n"))
	   (unless (eq (char-before (1- (point))) ?\^L)
	     (insert "\^L\n"))
	   (insert "(provide '" sym ")\n")))))

(defun autoload-make-feature-name (&optional prefix)
  "Generate the feature name to protect this auto-autoloads file from PREFIX.

If PREFIX is nil, it defaults to the value of `autoload-feature-prefix' if
that is non-nil.

The feature name must be globally unique for this version of XEmacs,
including packages.

For backward compatibility, if PREFIX and `autoload-feature-prefix' are both
`nil', PREFIX is computed as the last directory component of
`generated-autoload-file'.  This is likely to result in non-uniqueness, so
do not use this feature."
  (concat
   (cond (prefix)
	 (autoload-feature-prefix)
	 ((stringp generated-autoload-file)
	  (message "Warning: autoload computing feature prefix.
You should specify it as an argument to `autoload-make-feature-name'.")
	  (file-name-nondirectory
	   (directory-file-name
	    (file-name-directory generated-autoload-file))))
	 (t (error 'invalid-argument
		   "Could not compute a feature name")))
   autoload-feature-suffix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deprecated entry points

;; A grep of the core and packages shows use of `batch-update-autoloads'
;; by XEmacs.rules, pcomplete, eshell, oort-gnus; `batch-update-directory'
;; by liece.  The other two entry points (`batch-update-one-directory',
;; `batch-force-update-one-directory') were not used at all.
;;
;; All except the first are now history.  liece has been updated.
;; XEmacs.rules has been updated.  The others will be, eventually.

;; There don't seem to be very many packages that use the first one (the
;; "all-into-one-file" variety), and do they actually rely on this
;; functionality? --ben

;; but XEmacs.rules does, though maybe it doesn't "rely" on it, and
;; modules do now, and that relies on it. --sjt

;;;###autoload
(defun batch-update-autoloads ()
  "Update the autoloads for the files or directories on the command line.
Runs `update-file-autoloads' on files and `update-directory-autoloads'
on directories.  Must be used only with -batch, and kills Emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke `xemacs -batch -f batch-update-autoloads *.el'.
The directory to which the auto-autoloads.el file must be the first parameter
on the command line."
  (unless noninteractive
    (error "batch-update-autoloads is to be used only with -batch"))
  (update-autoload-files command-line-args-left autoload-feature-prefix
			 generated-autoload-file t)
  (kill-emacs 0))

;; Declare obsolescence

(make-obsolete-variable 'autoload-target-directory
  "Don't use this.  Bind `generated-autoload-file' to an absolute path.")
(make-obsolete 'batch-update-autoloads
	       'autoload-update-directory-autoloads)

(provide 'autoload)

;;; autoload.el ends here
