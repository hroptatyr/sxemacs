;;; derived.el --- allow inheritance of major modes
;;; (formerly mode-clone.el)

;; Copyright (C) 1993, 1994, 1999, 2003 Free Software Foundation, Inc.

;; Author: David Megginson (dmeggins@aix1.uottawa.ca)
;; Maintainer: SXEmacs Development Team
;; Keywords: extensions, dumped

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

;;; Synched up with: FSF 21.3.

;;; Commentary:

;; This file is dumped with SXEmacs.

;; XEmacs is already, in a sense, object oriented -- each object
;; (buffer) belongs to a class (major mode), and that class defines
;; the relationship between messages (input events) and methods
;; (commands) by means of a keymap.
;;
;; The only thing missing is a good scheme of inheritance.  It is
;; possible to simulate a single level of inheritance with generous
;; use of hooks and a bit of work -- sgml-mode, for example, also runs
;; the hooks for text-mode, and keymaps can inherit from other keymaps
;; -- but generally, each major mode ends up reinventing the wheel.
;; Ideally, someone should redesign all of Emacs's major modes to
;; follow a more conventional object-oriented system: when defining a
;; new major mode, the user should need only to name the existing mode
;; it is most similar to, then list the (few) differences.
;;
;; In the mean time, this package offers most of the advantages of
;; full inheritance with the existing major modes.  The macro
;; `define-derived-mode' allows the user to make a variant of an existing
;; major mode, with its own keymap.  The new mode will inherit the key
;; bindings of its parent, and will, in fact, run its parent first
;; every time it is called.  For example, the commands
;;
;;  (define-derived-mode hypertext-mode text-mode "Hypertext"
;;    "Major mode for hypertext.\n\n\\{hypertext-mode-map}"
;;    (setq case-fold-search nil))
;;
;;  (define-key hypertext-mode-map [down-mouse-3] 'do-hyper-link)
;;
;; will create a function `hypertext-mode' with its own (sparse)
;; keymap `hypertext-mode-map.'  The command M-x hypertext-mode will
;; perform the following actions:
;;
;; - run the command (text-mode) to get its default setup
;; - replace the current keymap with 'hypertext-mode-map,' which will
;;   inherit from 'text-mode-map'.
;; - replace the current syntax table with
;;   'hypertext-mode-syntax-table', which will borrow its defaults
;;   from the current text-mode-syntax-table.
;; - replace the current abbrev table with
;;   'hypertext-mode-abbrev-table', which will borrow its defaults
;;   from the current text-mode-abbrev table
;; - change the mode line to read "Hypertext"
;; - assign the value 'hypertext-mode' to the 'major-mode' variable
;; - run the body of commands provided in the macro -- in this case,
;;   set the local variable `case-fold-search' to nil.
;;
;; The advantages of this system are threefold.  First, text mode is
;; untouched -- if you had added the new keystroke to `text-mode-map,'
;; possibly using hooks, you would have added it to all text buffers
;; -- here, it appears only in hypertext buffers, where it makes
;; sense.  Second, it is possible to build even further, and make
;; a derived mode from a derived mode.  The commands
;;
;;   (define-derived-mode html-mode hypertext-mode "HTML")
;;   [various key definitions]
;;
;; will add a new major mode for HTML with very little fuss.
;;
;; Note also the function `derived-mode-p' which can tell if the current
;; mode derives from another.  In a hypertext-mode, buffer, for example,
;; (derived-mode-p 'text-mode) would return non-nil.  This should always
;; be used in place of (eq major-mode 'text-mode).

;;; Code:

;;; PRIVATE: defsubst must be defined before they are first used

(defsubst derived-mode-hook-name (mode)
  "Construct the mode hook name based on mode name MODE."
  (intern (concat (symbol-name mode) "-hook")))

(defsubst derived-mode-map-name (mode)
  "Construct a map name based on a MODE name."
  (intern (concat (symbol-name mode) "-map")))

(defsubst derived-mode-syntax-table-name (mode)
  "Construct a syntax-table name based on a MODE name."
  (intern (concat (symbol-name mode) "-syntax-table")))

(defsubst derived-mode-abbrev-table-name (mode)
  "Construct an abbrev-table name based on a MODE name."
  (intern (concat (symbol-name mode) "-abbrev-table")))

;; PUBLIC: define a new major mode which inherits from an existing one.

;; XEmacs -- no autoload
(defmacro define-derived-mode (child parent name &optional docstring &rest body)
  "Create a new mode as a variant of an existing mode.

The arguments to this command are as follow:

CHILD:     the name of the command for the derived mode.
PARENT:    the name of the command for the parent mode (e.g. `text-mode')
	   or nil if there is no parent.
NAME:      a string which will appear in the status line (e.g. \"Hypertext\")
DOCSTRING: an optional documentation string--if you do not supply one,
	   the function will attempt to invent something useful.
BODY:      forms to execute just before running the
	   hooks for the new mode.  Do not use `interactive' here.

BODY can start with a bunch of keyword arguments.  The following keyword
  arguments are currently understood:
:group GROUP
	Declare the customization group that corresponds to this mode.
:syntax-table TABLE
	Use TABLE instead of the default.
	A nil value means to simply use the same syntax-table as the parent.
:abbrev-table TABLE
	Use TABLE instead of the default.
	A nil value means to simply use the same abbrev-table as the parent.

Here is how you could define LaTeX-Thesis mode as a variant of LaTeX mode:

  (define-derived-mode LaTeX-thesis-mode LaTeX-mode \"LaTeX-Thesis\")

You could then make new key bindings for `LaTeX-thesis-mode-map'
without changing regular LaTeX mode.  In this example, BODY is empty,
and DOCSTRING is generated by default.

On a more complicated level, the following command uses `sgml-mode' as
the parent, and then sets the variable `case-fold-search' to nil:

  (define-derived-mode article-mode sgml-mode \"Article\"
    \"Major mode for editing technical articles.\"
    (setq case-fold-search nil))

Note that if the documentation string had been left out, it would have
been generated automatically, with a reference to the keymap.

The new mode runs the hook constructed by the function
`derived-mode-hook-name'."
  (declare (debug (&define name symbolp sexp [&optional stringp]
			   [&rest keywordp sexp] def-body)))

  (when (and docstring (not (stringp docstring)))
    ;; Some trickiness, since what appears to be the docstring may really be
    ;; the first element of the body.
    (push docstring body)
    (setq docstring nil))

  (when (eq parent 'fundamental-mode) (setq parent nil))

  (let ((map (derived-mode-map-name child))
	(syntax (derived-mode-syntax-table-name child))
	(abbrev (derived-mode-abbrev-table-name child))
	(declare-abbrev t)
	(declare-syntax t)
	(hook (derived-mode-hook-name child))
	(group nil))

    ;; Process the keyword args.
    (while (keywordp (car body))
      (case (pop body)
	(:group (setq group (pop body)))
	(:abbrev-table (setq abbrev (pop body)) (setq declare-abbrev nil))
	(:syntax-table (setq syntax (pop body)) (setq declare-syntax nil))
	(t (pop body))))

    (setq docstring (derived-mode-make-docstring
		     parent child docstring syntax abbrev))

    `(progn
       (defvar ,hook nil ,(format "Hook run when entering %s mode." name))
       (defvar ,map (make-sparse-keymap))
       ,(if declare-syntax
	    `(defvar ,syntax (make-syntax-table)))
       ,(if declare-abbrev
	    `(defvar ,abbrev
	       (progn (define-abbrev-table ',abbrev nil) ,abbrev)))
       (put ',child 'derived-mode-parent ',parent)
       ,(if group `(put ',child 'custom-mode-group ,group))

       (defun ,child ()
	 ,docstring
	 (interactive)
					; Run the parent.
	 (delay-mode-hooks

	  (,(or parent 'kill-all-local-variables))
					; Identify the child mode.
	  (setq major-mode (quote ,child))
	  (setq mode-name ,name)
					; Identify special modes.
	  ,(when parent
	     `(progn
		(if (get (quote ,parent) 'mode-class)
		    (put (quote ,child) 'mode-class
			 (get (quote ,parent) 'mode-class)))
					; Set up maps and tables.
		(unless (keymap-parent ,map)
		  (set-keymap-parents ,map (list (current-local-map))))
		,(when declare-syntax
		   ;; XEmacs change: we do not have char-table-parent
		   `(derived-mode-merge-syntax-tables
		     (syntax-table) ,syntax))))

	  (use-local-map ,map)
	  ,(when syntax `(set-syntax-table ,syntax))
	  ,(when abbrev `(setq local-abbrev-table ,abbrev))
					; Splice in the body (if any).
	  ,@body
	  )
	 ;; Run the hooks, if any.
	 ;; Make the generated code work in older Emacs versions
	 ;; that do not yet have run-mode-hooks.
	 (if (fboundp 'run-mode-hooks)
	     (run-mode-hooks ',hook)
	   (run-hooks ',hook))))))

;; PUBLIC: find the ultimate class of a derived mode.

(defun derived-mode-class (mode)
  "Find the class of a major MODE.
A mode's class is the first ancestor which is NOT a derived mode.
Use the `derived-mode-parent' property of the symbol to trace backwards.
Since major-modes might all derive from `fundamental-mode', this function
is not very useful."
  (while (get mode 'derived-mode-parent)
    (setq mode (get mode 'derived-mode-parent)))
  mode)
(make-obsolete 'derived-mode-class 'derived-mode-p)

;; PUBLIC: find if the current mode derives from another.
;; from GNU Emacs 21 subr.el

(defun derived-mode-p (&rest modes)
  "Non-nil if the current major mode is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace backwards."
  (let ((parent major-mode))
    (while (and (not (memq parent modes))
		(setq parent (get parent 'derived-mode-parent))))
    parent))


;;; PRIVATE

(defun derived-mode-make-docstring (parent child &optional
					   docstring syntax abbrev)
  "Construct a docstring for a new mode if none is provided."

  (let ((map (derived-mode-map-name child))
	(hook (derived-mode-hook-name child)))

    (unless (stringp docstring)
      ;; Use a default docstring.
      (setq docstring
	    (if (null parent)
		(format "Major-mode.
Uses keymap `%s', abbrev table `%s' and syntax-table `%s'." map abbrev syntax)
	      (format "Major mode derived from `%s' by `define-derived-mode'.
It inherits all of the parent's attributes, but has its own keymap,
abbrev table and syntax table:

  `%s', `%s' and `%s'

which more-or-less shadow %s's corresponding tables."
		      parent map abbrev syntax parent))))

    (unless (string-match (regexp-quote (symbol-name hook)) docstring)
      ;; Make sure the docstring mentions the mode's hook.
      (setq docstring
	    (concat docstring
		    (if (null parent)
			"\n\nThis mode "
		      (concat
		       "\n\nIn addition to any hooks its parent mode "
		       (if (string-match (regexp-quote (format "`%s'" parent))
					 docstring) nil
			 (format "`%s' " parent))
		       "might have run,\nthis mode "))
		    (format "runs the hook `%s'" hook)
		    ", as the final step\nduring initialization.")))

    (unless (or (string-match (regexp-quote "\\{") docstring)
		(string-match (regexp-quote "\\[") docstring))
      ;; And don't forget to put the mode's keymap.
      (setq docstring (concat docstring "\n\n\\{" (symbol-name map) "}")))

    docstring))


;;; OBSOLETE
;; The functions below are only provided for backward compatibility with
;; code byte-compiled with versions of derived.el prior to Emacs-21.

(defsubst derived-mode-setup-function-name (mode)
  "Construct a setup-function name based on a MODE name."
  (intern (concat (symbol-name mode) "-setup")))


;; Utility functions for defining a derived mode.

;; XEmacs -- don't autoload
(defun derived-mode-init-mode-variables (mode)
  "Initialise variables for a new MODE.
Right now, if they don't already exist, set up a blank keymap, an
empty syntax table, and an empty abbrev table -- these will be merged
the first time the mode is used."

  (if (boundp (derived-mode-map-name mode))
      t
    (eval `(defvar ,(derived-mode-map-name mode)
	     ;; XEmacs change
	     (make-sparse-keymap (derived-mode-map-name mode))
	     ,(format "Keymap for %s." mode)))
    (put (derived-mode-map-name mode) 'derived-mode-unmerged t))

  (if (boundp (derived-mode-syntax-table-name mode))
      t
    (eval `(defvar ,(derived-mode-syntax-table-name mode)
	     ;; XEmacs change
	     ;; Make a syntax table which doesn't specify anything
	     ;; for any char.  Valid data will be merged in by
	     ;; derived-mode-merge-syntax-tables.
	     ;; (make-char-table 'syntax-table nil)
	     (make-syntax-table)
	     ,(format "Syntax table for %s." mode)))
    (put (derived-mode-syntax-table-name mode) 'derived-mode-unmerged t))

  (if (boundp (derived-mode-abbrev-table-name mode))
      t
    (eval `(defvar ,(derived-mode-abbrev-table-name mode)
	     (progn
	       (define-abbrev-table (derived-mode-abbrev-table-name mode) nil)
	       (make-abbrev-table))
	     ,(format "Abbrev table for %s." mode)))))

;; Utility functions for running a derived mode.

(defun derived-mode-set-keymap (mode)
  "Set the keymap of the new MODE, maybe merging with the parent."
  (let* ((map-name (derived-mode-map-name mode))
	 (new-map (eval map-name))
	 (old-map (current-local-map)))
    (and old-map
	 (get map-name 'derived-mode-unmerged)
	 (derived-mode-merge-keymaps old-map new-map))
    (put map-name 'derived-mode-unmerged nil)
    (use-local-map new-map)))

(defun derived-mode-set-syntax-table (mode)
  "Set the syntax table of the new MODE, maybe merging with the parent."
  (let* ((table-name (derived-mode-syntax-table-name mode))
	 (old-table (syntax-table))
	 (new-table (eval table-name)))
    (if (get table-name 'derived-mode-unmerged)
	(derived-mode-merge-syntax-tables old-table new-table))
    (put table-name 'derived-mode-unmerged nil)
    (set-syntax-table new-table)))

(defun derived-mode-set-abbrev-table (mode)
  "Set the abbrev table for MODE if it exists.
Always merge its parent into it, since the merge is non-destructive."
  (let* ((table-name (derived-mode-abbrev-table-name mode))
	 (old-table local-abbrev-table)
	 (new-table (eval table-name)))
    (derived-mode-merge-abbrev-tables old-table new-table)
    (setq local-abbrev-table new-table)))

;;;(defun derived-mode-run-setup-function (mode)
;;;  "Run the setup function if it exists."

;;;  (let ((fname (derived-mode-setup-function-name mode)))
;;;    (if (fboundp fname)
;;;	(funcall fname))))

(defun derived-mode-run-hooks (mode)
  "Run the mode hook for MODE."
  (let ((hooks-name (derived-mode-hook-name mode)))
    (if (boundp hooks-name)
	(run-hooks hooks-name))))

;; Functions to merge maps and tables.

(defun derived-mode-merge-keymaps (old new)
  "Merge an OLD keymap into a NEW one.
The old keymap is set to be the last cdr of the new one, so that there will
be automatic inheritance."
  ;; XEmacs change.  FSF 19.30 to 21.3 has a whole bunch of weird crap here
  ;; for merging prefix keys and such.  Hopefully none of this is
  ;; necessary in XEmacs.
  (set-keymap-parents new (list old)))

(defun derived-mode-merge-syntax-tables (old new)
  "Merge an OLD syntax table into a NEW one.
Where the new table already has an entry, nothing is copied from the old one."
  ;; XEmacs change: on the other hand, Emacs 21.3 just has
  ;; (set-char-table-parent new old) here.
  ;; We use map-char-table, not map-syntax-table, so we can explicitly
  ;; check for inheritance.
  (map-char-table
   #'(lambda (key value)
       (let ((newval (get-range-char-table key new 'multi)))
	 (cond ((eq newval 'multi)	; OK, dive into the class hierarchy
		(map-char-table
		 #'(lambda (key1 value1)
		     (when (eq ?@ (char-syntax-from-code
				   (get-range-char-table key new ?@)))
		       (put-char-table key1 value new))
		     nil)
		 new
		 key))
	       ((eq ?@ (char-syntax-from-code newval)) ;; class at once
		(put-char-table key value new))))
       nil)
   old))

;; Merge an old abbrev table into a new one.
;; This function requires internal knowledge of how abbrev tables work,
;; presuming that they are obarrays with the abbrev as the symbol, the expansion
;; as the value of the symbol, and the hook as the function definition.
(defun derived-mode-merge-abbrev-tables (old new)
  (if old
      (mapatoms
       #'(lambda (symbol)
	   (or (intern-soft (symbol-name symbol) new)
	       (define-abbrev new (symbol-name symbol)
		 (symbol-value symbol) (symbol-function symbol))))
       old)))

(provide 'derived)

;;; derived.el ends here
