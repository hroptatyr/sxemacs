;;; cus-edit.el --- Tools for customizating Emacs and Lisp packages.
;;
;; Copyright (C) 2007 Didier Verna
;; Copyright (C) 2003 Ben Wing
;; Copyright (C) 1996, 1997, 2000 Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: Didier Verna <didier@xemacs.org>
;; Keywords: help, faces
;; Version: 1.9960-x
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

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

;;; Commentary:
;;
;; This file implements the code to create and edit customize buffers.
;;
;; See `custom.el'.

;; No commands should have names starting with `custom-' because
;; that interferes with completion.  Use `customize-' for commands
;; that the user will run with M-x, and `Custom-' for interactive commands.

;; NOTE: In many places within this file we use `mapatoms', which is
;; very slow in an average XEmacs because of the large number of
;; symbols requiring a large number of funcalls -- XEmacs with Gnus
;; can grow to some 17000 symbols without ever doing anything fancy.
;; It would probably pay off to make a hash table of symbols known to
;; Custom, similar to custom-group-hash-table.

;; This is not top priority, because none of the functions that do
;; mapatoms are speed-critical (the one that was now uses
;; custom-group-hash-table), but it would be nice to have.


;;; Code:

(require 'cus-face)
(require 'wid-edit)
(require 'easymenu)

(require 'cus-load)
(require 'cus-start)
(require 'cus-file)

;; Huh?  This looks dirty!
(put 'custom-define-hook 'custom-type 'hook)
(put 'custom-define-hook 'standard-value '(nil))
(custom-add-to-group 'customize 'custom-define-hook 'custom-variable)

;;; Customization Groups.

(defgroup emacs nil
  "Customization of the One True Editor."
  :link '(custom-manual "(XEmacs)Top"))

;; Most of these groups are stolen from `finder.el',
(defgroup editing nil
  "Basic text editing facilities."
  :group 'emacs)

(defgroup matching nil
  "Various sorts of searching and matching."
  :group 'editing)

(defgroup emulations nil
  "Emulations of other editors."
  :group 'editing)

(defgroup outlines nil
  "Support for hierarchical outlining."
  :group 'editing)

(defgroup external nil
  "Interfacing to external utilities."
  :group 'emacs)

(defgroup bib nil
  "Code related to the `bib' bibliography processor."
  :tag "Bibliography"
  :group 'external)

(defgroup programming nil
  "Support for programming in other languages."
  :group 'emacs)

(defgroup languages nil
  "Specialized modes for editing programming languages."
  :group 'programming)

;; #### This should be in cc-vars.el
(defgroup c nil
  "Support for the C language and related languages."
  :group 'languages)

(defgroup tools nil
  "Programming tools."
  :group 'programming)

(defgroup oop nil
  "Support for object-oriented programming."
  :group 'programming)

(defgroup applications nil
  "Applications written in Emacs."
  :group 'emacs)

;; #### This should be in calendar.el
(defgroup calendar nil
  "Calendar and time management support."
  :group 'applications)

(defgroup mail nil
  "Modes for electronic-mail handling."
  :group 'applications)

(defgroup news nil
  "Support for netnews reading and posting."
  :group 'applications)

(defgroup games nil
  "Games, jokes and amusements."
  :group 'applications)

(defgroup development nil
  "Support for further development of Emacs."
  :group 'emacs)

(defgroup docs nil
  "Support for Emacs documentation."
  :group 'development)

(defgroup extensions nil
  "Emacs Lisp language extensions."
  :group 'development)

(defgroup internal nil
  "Code for Emacs internals, build process, defaults."
  :group 'development)

(defgroup maint nil
  "Maintenance aids for the Emacs development group."
  :tag "Maintenance"
  :group 'development)

(defgroup environment nil
  "Fitting Emacs with its environment."
  :group 'emacs)

(defgroup comm nil
  "Communications, networking, remote access to files."
  :tag "Communication"
  :group 'environment)

(defgroup hardware nil
  "Support for interfacing with exotic hardware."
  :group 'environment)

(defgroup terminals nil
  "Support for terminal types."
  :group 'environment)

(defgroup unix nil
  "Front-ends/assistants for, or emulators of, UNIX features."
  :group 'environment)

(defgroup i18n nil
  "Internationalization and alternate character-set support."
  :group 'environment
  :group 'editing)

(defgroup data nil
  "Support editing files of data."
  :group 'emacs)

(defgroup wp nil
  "Word processing."
  :group 'emacs)

(defgroup tex nil
  "Code related to the TeX formatter."
  :group 'wp)

(defgroup hypermedia nil
  "Support for links between text or other media types."
  :group 'emacs)

(defgroup local nil
  "Code local to your site."
  :group 'emacs)

(defgroup customize '((widgets custom-group))
  "Customization of the Customization support."
  :link '(custom-manual "(custom)Top")
  :link '(url-link :tag "Development Page"
		   "http://www.dina.kvl.dk/~abraham/custom/")
  :prefix "custom-"
  :group 'help)

(defgroup custom-faces nil
  "Faces used by customize."
  :group 'customize
  :group 'faces)

(defgroup custom-browse nil
  "Control customize browser."
  :prefix "custom-"
  :group 'customize)

(defgroup custom-buffer nil
  "Control customize buffers."
  :prefix "custom-"
  :group 'customize)

(defgroup custom-menu nil
  "Control customize menus."
  :prefix "custom-"
  :group 'customize)

(defgroup alloc nil
  "Storage allocation and gc for XEmacs Lisp interpreter."
  :tag "Storage Allocation"
  :group 'internal)

(defgroup undo nil
  "Undoing changes in buffers."
  :group 'editing)

(defgroup editing-basics nil
  "Most basic editing facilities."
  :group 'editing)

(defgroup display nil
  "How characters are displayed in buffers."
  :group 'environment)

(defgroup installation nil
  "The Emacs installation."
  :group 'environment)

(defgroup limits nil
  "Internal Emacs limits."
  :group 'internal)

(defgroup debug nil
  "Debugging Emacs itself."
  :group 'development)

(defgroup mule nil
  "Mule XEmacs internationalization."
  :group 'i18n)


;;; Utilities.

(defun custom-quote (sexp)
  "Quote SEXP iff it is not self quoting."
  (if (or (memq sexp '(t nil))
	  (keywordp sexp)
	  (eq (car-safe sexp) 'lambda)
	  (stringp sexp)
	  (numberp sexp)
	  (characterp sexp)
	  (vectorp sexp)
	  (bit-vector-p sexp))
      sexp
    (list 'quote sexp)))

(defun custom-split-regexp-maybe (regexp)
  "If REGEXP is a string, split it to a list at `\\|'.
You can get the original back with from the result with:
  (mapconcat #'identity result \"\\|\")

IF REGEXP is not a string, return it unchanged."
  (if (stringp regexp)
      (split-string regexp #r"\\|")
    regexp))

(defun custom-variable-prompt ()
  ;; Code stolen from `help.el'.
  "Prompt for a variable, defaulting to the variable at point.
Return a list suitable for use in `interactive'."
   (let ((v (variable-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (completing-read
		(if (symbolp v)
		    (format "Customize variable: (default %s) " v)
		  "Customize variable: ")
		obarray (lambda (symbol)
			  (and (boundp symbol)
			       (or (get symbol 'custom-type)
				   (user-variable-p symbol))))
		t nil nil (and v (symbol-name v))))
     (list (if (equal val "")
	       (if (symbolp v) v nil)
	     (intern val)))))

;; Here we take not only the actual groups, but the loads, too.
(defun custom-group-prompt (prompt)
  "Read group from minibuffer."
  (let ((completion-ignore-case t))
    (list (completing-read
	   prompt obarray
	   (lambda (symbol)
	     (or (get symbol 'custom-group)
		 (get symbol 'custom-loads)))
	   t))))

(defun custom-menu-filter (menu widget)
  "Convert MENU to the form used by `widget-choose'.
MENU should be in the same format as `custom-variable-menu'.
WIDGET is the widget to apply the filter entries of MENU on."
  (let ((result nil)
	current name action filter)
    (while menu
      (setq current (car menu)
	    name (nth 0 current)
	    action (nth 1 current)
	    filter (nth 2 current)
	    menu (cdr menu))
      (if (or (null filter) (funcall filter widget))
	  (push (cons name action) result)
	(push name result)))
    (nreverse result)))


;;; Unlispify.

(defvar custom-prefix-list nil
  "List of prefixes that should be ignored by `custom-unlispify'")

(defcustom custom-save-pretty-print t
  "Non-nil means pretty-print values of customized variables if available."
  :group 'customize
  :type 'boolean)


(defcustom custom-unlispify-menu-entries t
  "Display menu entries as words instead of symbols if non nil."
  :group 'custom-menu
  :type 'boolean)

(defcustom custom-unlispify-remove-prefixes t
  "Non-nil means remove group prefixes from option names in buffers and menus.
This only has an effect when `custom-unlispify-tag-names' or
`custom-unlispify-menu-entries' is on."
  :group 'custom-menu
  :type 'boolean)

(defun custom-unlispify-menu-entry (symbol &optional no-suffix)
  "Convert symbol into a menu entry."
  (cond ((not custom-unlispify-menu-entries)
	 (symbol-name symbol))
	((get symbol 'custom-tag)
	 (if no-suffix
	     (get symbol 'custom-tag)
	   (concat (get symbol 'custom-tag) "...")))
	(t
	 (with-current-buffer (get-buffer-create " *Custom-Work*")
	   (erase-buffer)
	   (princ symbol (current-buffer))
	   (goto-char (point-min))
	   (when (and (eq (get symbol 'custom-type) 'boolean)
		      (re-search-forward "-p\\'" nil t))
	     (replace-match "" t t)
	     (goto-char (point-min)))
	   (when custom-unlispify-remove-prefixes
	     (let ((prefixes custom-prefix-list)
		   prefix)
	       (while prefixes
		 (setq prefix (car prefixes))
		 (if (search-forward prefix (+ (point) (length prefix)) t)
		     (progn
		       (setq prefixes nil)
		       (delete-region (point-min) (point)))
		   (setq prefixes (cdr prefixes))))))
	   (subst-char-in-region (point-min) (point-max) ?- ?\  t)
	   (capitalize-region (point-min) (point-max))
	   (unless no-suffix
	     (goto-char (point-max))
	     (insert "..."))
	   (buffer-string)))))

(defcustom custom-unlispify-tag-names t
  "Display tag names as words instead of symbols if non nil."
  :group 'custom-buffer
  :type 'boolean)

(defun custom-unlispify-tag-name (symbol)
  "Convert symbol into a menu entry."
  (let ((custom-unlispify-menu-entries custom-unlispify-tag-names))
    (custom-unlispify-menu-entry symbol t)))

(defun custom-prefix-add (symbol prefixes)
  ;; Add SYMBOL to list of ignored PREFIXES.
  (cons (or (get symbol 'custom-prefix)
	    (concat (symbol-name symbol) "-"))
	prefixes))


;;; Guess.

(defcustom custom-guess-name-alist
  '(("-p\\'" boolean)
    ("-hooks?\\'" hook)
    ("-face\\'" face)
    ("-file\\'" file)
    ("-function\\'" function)
    ("-functions\\'" (repeat function))
    ("-list\\'" (repeat sexp))
    ("-alist\\'" (repeat (cons sexp sexp))))
  "Alist of (MATCH TYPE).

MATCH should be a regexp matching the name of a symbol, and TYPE should
be a widget suitable for editing the value of that symbol.  The TYPE
of the first entry where MATCH matches the name of the symbol will be
used.

This is used for guessing the type of variables not declared with
customize."
  :type '(repeat (group (regexp :tag "Match") (sexp :tag "Type")))
  :group 'customize)

(defcustom custom-guess-doc-alist
  '((#r"\`\*?Non-nil " boolean))
  "Alist of (MATCH TYPE).

MATCH should be a regexp matching a documentation string, and TYPE
should be a widget suitable for editing the value of a variable with
that documentation string.  The TYPE of the first entry where MATCH
matches the name of the symbol will be used.

This is used for guessing the type of variables not declared with
customize."
  :type '(repeat (group (regexp :tag "Match") (sexp :tag "Type")))
  :group 'customize)

(defun custom-guess-type (symbol)
  "Guess a widget suitable for editing the value of SYMBOL.
This is done by matching SYMBOL with `custom-guess-name-alist' and
if that fails, the doc string with `custom-guess-doc-alist'."
  (let ((name (symbol-name symbol))
	(names custom-guess-name-alist)
	current found)
    (while names
      (setq current (car names)
	    names (cdr names))
      (when (string-match (nth 0 current) name)
	(setq found (nth 1 current)
	      names nil)))
    (unless found
      (let ((doc (documentation-property symbol 'variable-documentation))
	    (docs custom-guess-doc-alist))
	(when doc
	  (while docs
	    (setq current (car docs)
		  docs (cdr docs))
	    (when (string-match (nth 0 current) doc)
	      (setq found (nth 1 current)
		    docs nil))))))
    found))


;;; Sorting.

(defcustom custom-browse-sort-alphabetically nil
  "If non-nil, sort members of each customization group alphabetically."
  :type 'boolean
  :group 'custom-browse)

(defcustom custom-browse-order-groups nil
  "If non-nil, order group members within each customization group.
If `first', order groups before non-groups.
If `last', order groups after non-groups."
  :type '(choice (const first)
		 (const last)
		 (const :tag "none" nil))
  :group 'custom-browse)

(defcustom custom-browse-only-groups nil
  "If non-nil, show group members only within each customization group."
  :type 'boolean
  :group 'custom-browse)

(defcustom custom-buffer-sort-alphabetically nil
  "If non-nil, sort members of each customization group alphabetically."
  :type 'boolean
  :group 'custom-buffer)

(defcustom custom-buffer-order-groups 'last
  "If non-nil, order group members within each customization group.
If `first', order groups before non-groups.
If `last', order groups after non-groups."
  :type '(choice (const first)
		 (const last)
		 (const :tag "none" nil))
  :group 'custom-buffer)

(defcustom custom-menu-sort-alphabetically nil
  "If non-nil, sort members of each customization group alphabetically."
  :type 'boolean
  :group 'custom-menu)

(defcustom custom-menu-order-groups 'first
  "If non-nil, order group members within each customization group.
If `first', order groups before non-groups.
If `last', order groups after non-groups."
  :type '(choice (const first)
		 (const last)
		 (const :tag "none" nil))
  :group 'custom-menu)

(defun custom-sort-items (items sort-alphabetically order-groups)
  "Return a sorted copy of ITEMS.
ITEMS should be a `custom-group' property.
If SORT-ALPHABETICALLY non-nil, sort alphabetically.
If ORDER-GROUPS is `first' order groups before non-groups, if `last' order
groups after non-groups, if nil do not order groups at all."
  (sort (copy-sequence items)
   (lambda (a b)
     (let ((typea (nth 1 a)) (typeb (nth 1 b))
	   (namea (symbol-name (nth 0 a))) (nameb (symbol-name (nth 0 b))))
       (cond ((not order-groups)
	      ;; Since we don't care about A and B order, maybe sort.
	      (when sort-alphabetically
		(string-lessp namea nameb)))
	     ((eq typea 'custom-group)
	      ;; If B is also a group, maybe sort.  Otherwise, order A and B.
	      (if (eq typeb 'custom-group)
		  (when sort-alphabetically
		    (string-lessp namea nameb))
		(eq order-groups 'first)))
	     ((eq typeb 'custom-group)
	      ;; Since A cannot be a group, order A and B.
	      (eq order-groups 'last))
	     (sort-alphabetically
	      ;; Since A and B cannot be groups, sort.
	      (string-lessp namea nameb)))))))


;;; Custom Mode Commands.

(defvar custom-options nil
  "Customization widgets in the current buffer.")

(defun Custom-set ()
  "Set changes in all modified options."
  (interactive)
  (let ((children custom-options))
    (mapc (lambda (child)
	    (when (eq (widget-get child :custom-state) 'modified)
	      (widget-apply child :custom-set)))
	  children)))

(defun Custom-save ()
  "Set all modified options and save them."
  (interactive)
  (let ((all-children custom-options)
	children)
    (mapc (lambda (child)
	    (when (memq (widget-get child :custom-state) '(modified set))
	      (push child children)))
	  all-children)
    (let ((the-children children)
	  child)
      (while (setq child (pop the-children))
	(widget-apply child :custom-pre-save)))
    (custom-save-all)
    (let ((the-children children)
	  child)
      (while (setq child (pop the-children))
	(widget-apply child :custom-post-save)))
    ))

(defvar custom-reset-menu
  '(("Current" . Custom-reset-current)
    ("Saved" . Custom-reset-saved)
    ("Standard Settings" . Custom-reset-standard))
  "Alist of actions for the `Reset' button.
The key is a string containing the name of the action, the value is a
lisp function taking the widget as an element which will be called
when the action is chosen.")

(defun custom-reset (event)
  "Select item from reset menu."
  (let* ((completion-ignore-case t)
	 (answer (widget-choose "Reset to"
				custom-reset-menu
				event)))
    (if answer
	(funcall answer))))

(defun Custom-reset-current (&rest ignore)
  "Reset all modified group members to their current value."
  (interactive)
  (let ((children custom-options))
    (mapc (lambda (child)
	    (when (eq (widget-get child :custom-state) 'modified)
	      (widget-apply child :custom-reset-current)))
	  children)))

(defun Custom-reset-saved (&rest ignore)
  "Reset all modified or set group members to their saved value."
  (interactive)
  (let ((children custom-options))
    (mapc (lambda (child)
	    (when (eq (widget-get child :custom-state) 'modified)
	      (widget-apply child :custom-reset-saved)))
	  children)))

(defun Custom-reset-standard (&rest ignore)
  "Reset all modified, set, or saved group members to their standard settings."
  (interactive)
  (let ((all-children custom-options)
	children must-save)
    (mapc (lambda (child)
	    (when (memq (widget-get child :custom-state) '(modified set saved))
	      (push child children)))
	  all-children)
    (let ((the-children children)
	  child)
      (while (setq child (pop the-children))
	(and (widget-apply child :custom-pre-reset-standard)
	     (setq must-save t))))
    (and must-save (custom-save-all))
    (let ((the-children children)
	  child)
      (while (setq child (pop the-children))
	(widget-apply child :custom-post-reset-standard)))
    ))


;;; The Customize Commands

(defun custom-prompt-variable (prompt-var prompt-val &optional comment)
  "Prompt for a variable and a value and return them as a list.
PROMPT-VAR is the prompt for the variable, and PROMPT-VAL is the
prompt for the value.  The %s escape in PROMPT-VAL is replaced with
the name of the variable.

If the variable has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If the variable has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value.

If optional COMMENT argument is non nil, also prompt for a comment and return
it as the third element in the list."
  (let* ((var (read-variable (concat prompt-var ": ")))
	 (minibuffer-help-form '(describe-variable var))
	 (val
	  (let ((prop (get var 'variable-interactive))
		(type (get var 'custom-type))
		(prompt (format prompt-val var)))
	    (unless (listp type)
	      (setq type (list type)))
	    (cond (prop
		   ;; Use VAR's `variable-interactive' property
		   ;; as an interactive spec for prompting.
		   (call-interactively (list 'lambda '(arg)
					     (list 'interactive prop)
					     'arg)))
		  (type
		   (widget-prompt-value type
					prompt
					(if (boundp var)
					    (symbol-value var))
					(not (boundp var))))
		  (t
		   (eval-minibuffer (concat prompt ": ")))))))
    (if comment
	(list var val
	      (read-string "Comment: " (get var 'variable-comment)))
      (list var val))))

;;;###autoload
(defun customize-set-value (var val &optional comment)
  "Set VARIABLE to VALUE.  VALUE is a Lisp object.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If VARIABLE has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value.

If given a prefix (or a COMMENT argument), also prompt for a comment."
  (interactive (custom-prompt-variable "Set variable"
				       "Set value of %s"
				       current-prefix-arg))

  (set var val)
  (cond ((string= comment "")
	 (put var 'variable-comment nil))
	(comment
	 (put var 'variable-comment comment))))

;;;###autoload
(defun customize-set-variable (variable value &optional comment)
  "Set the default for VARIABLE to VALUE.  VALUE is any Lisp object.

If VARIABLE has a `custom-set' property, that is used for setting
VARIABLE, otherwise `set-default' is used.

The `customized-value' property of the VARIABLE will be set to a list
with a quoted VALUE as its sole list member.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If VARIABLE has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value.

If given a prefix (or a COMMENT argument), also prompt for a comment."
  (interactive (custom-prompt-variable "Set variable"
				       "Set customized value of %s"
				       current-prefix-arg))
  (funcall (or (get variable 'custom-set) 'set-default) variable value)
  (put variable 'customized-value (list (custom-quote value)))
  (cond ((string= comment "")
	 (put variable 'variable-comment nil)
	 (put variable 'customized-variable-comment nil))
	(comment
	 (put variable 'variable-comment comment)
	 (put variable 'customized-variable-comment comment))))


;;;###autoload
(defun customize-save-variable (variable value &optional comment)
  "Set the default for VARIABLE to VALUE, and save it for future sessions.
If VARIABLE has a `custom-set' property, that is used for setting
VARIABLE, otherwise `set-default' is used.

The `customized-value' property of the VARIABLE will be set to a list
with a quoted VALUE as its sole list member.

If VARIABLE has a `variable-interactive' property, that is used as if
it were the arg to `interactive' (which see) to interactively read the value.

If VARIABLE has a `custom-type' property, it must be a widget and the
`:prompt-value' property of that widget will be used for reading the value.

If given a prefix (or a COMMENT argument), also prompt for a comment."
  (interactive (custom-prompt-variable "Set and save variable"
				       "Set and save value of %s"
				       current-prefix-arg))
  (funcall (or (get variable 'custom-set) 'set-default) variable value)
  (put variable 'saved-value (list (custom-quote value)))
  (custom-push-theme 'theme-value variable 'user 'set (list (custom-quote value)))
  (cond ((string= comment "")
	 (put variable 'variable-comment nil)
	 (put variable 'saved-variable-comment nil))
	(comment
	 (put variable 'variable-comment comment)
	 (put variable 'saved-variable-comment comment)))
  (custom-save-all))

;;;###autoload
(defun customize (group)
  "Select a customization buffer which you can use to set user options.
User options are structured into \"groups\".
The default group is `Emacs'."
  (interactive (custom-group-prompt
		"Customize group: (default emacs) "))
  (when (stringp group)
    (if (string-equal "" group)
	(setq group 'emacs)
      (setq group (intern group))))
  (let ((name (format "*Customize Group: %s*"
		      (custom-unlispify-tag-name group))))
    (if (get-buffer name)
	(switch-to-buffer name)
      (custom-buffer-create (list (list group 'custom-group))
			    name
			    (concat " for group "
				    (custom-unlispify-tag-name group))))))

;;;###autoload
(defalias 'customize-group 'customize)

;;;###autoload
(defun customize-other-window (symbol)
  "Customize SYMBOL, which must be a customization group."
  (interactive (custom-group-prompt
		"Customize group: (default emacs) "))
  (when (stringp symbol)
    (if (string-equal "" symbol)
	(setq symbol 'emacs)
      (setq symbol (intern symbol))))
  (custom-buffer-create-other-window
   (list (list symbol 'custom-group))
   (format "*Customize Group: %s*" (custom-unlispify-tag-name symbol))))

;;;###autoload
(defalias 'customize-group-other-window 'customize-other-window)

;;;###autoload
(defalias 'customize-option 'customize-variable)

;;;###autoload
(defun customize-variable (symbol)
  "Customize SYMBOL, which must be a user option variable."
  (interactive (custom-variable-prompt))
  (custom-buffer-create (list (list symbol 'custom-variable))
			(format "*Customize Variable: %s*"
				(custom-unlispify-tag-name symbol))))

;;;###autoload
(defun customize-changed-options (since-version)
  "Customize all user option variables whose default values changed recently.
This means, in other words, variables defined with a `:version' keyword."
  (interactive
   "sCustomize options changed, since version (default all versions): ")
  (if (equal since-version "")
      (setq since-version nil))
  (let ((found nil))
    (mapatoms (lambda (symbol)
		(and (boundp symbol)
		     (let ((version (get symbol 'custom-version)))
		       (and version
			    (or (null since-version)
				(customize-version-lessp since-version
							 version))))
		     (push (list symbol 'custom-variable) found))))
    (unless found
      (error "No user options have changed defaults %s"
	     (if since-version
		 (format "since XEmacs %s" since-version)
	       "in recent Emacs versions")))
    (custom-buffer-create (custom-sort-items found t nil)
			  "*Customize Changed Options*")))

(defun customize-version-lessp (version1 version2)
  (let (major1 major2 minor1 minor2)
    (string-match #r"\([0-9]+\)[.]\([0-9]+\)" version1)
    (setq major1 (read (match-string 1 version1)))
    (setq minor1 (read (match-string 2 version1)))
    (string-match #r"\([0-9]+\)[.]\([0-9]+\)" version2)
    (setq major2 (read (match-string 1 version2)))
    (setq minor2 (read (match-string 2 version2)))
    (or (< major1 major2)
	(and (= major1 major2)
	     (< minor1 minor2)))))

;;;###autoload
(defalias 'customize-variable-other-window 'customize-option-other-window)

;;;###autoload
(defun customize-option-other-window (symbol)
  "Customize SYMBOL, which must be a user option variable.
Show the buffer in another window, but don't select it."
  (interactive (custom-variable-prompt))
  (custom-buffer-create-other-window
   (list (list symbol 'custom-variable))
   (format "*Customize Option: %s*" (custom-unlispify-tag-name symbol))))

;;;###autoload
(defun customize-face (&optional symbol)
  "Customize SYMBOL, which should be a face name or nil.
If SYMBOL is nil, customize all faces."
  (interactive (list (completing-read "Customize face: (default all) "
				      obarray 'find-face)))
  (if (or (null symbol) (and (stringp symbol) (zerop (length symbol))))
      (custom-buffer-create (custom-sort-items
			     (mapcar (lambda (symbol)
				       (list symbol 'custom-face))
				     (face-list))
			     t nil)
			    "*Customize Faces*")
    (when (stringp symbol)
      (setq symbol (intern symbol)))
    (check-argument-type 'symbolp symbol)
    (custom-buffer-create (list (list symbol 'custom-face))
			  (format "*Customize Face: %s*"
				  (custom-unlispify-tag-name symbol)))))

;;;###autoload
(defun customize-face-other-window (&optional symbol)
  "Show customization buffer for FACE in other window."
  (interactive (list (completing-read "Customize face: "
				      obarray 'find-face)))
  (if (or (null symbol) (and (stringp symbol) (zerop (length symbol))))
      ()
    (if (stringp symbol)
	(setq symbol (intern symbol)))
    (check-argument-type 'symbolp symbol)
    (custom-buffer-create-other-window
     (list (list symbol 'custom-face))
     (format "*Customize Face: %s*" (custom-unlispify-tag-name symbol)))))

;;;###autoload
(defun customize-customized ()
  "Customize all user options set since the last save in this session."
  (interactive)
  (let ((found nil))
    (mapatoms (lambda (symbol)
		(and (or (get symbol 'customized-face)
			 (get symbol 'customized-face-comment))
		     (find-face symbol)
		     (push (list symbol 'custom-face) found))
		(and (or (get symbol 'customized-value)
			 (get symbol 'customized-variable-comment))
		     (boundp symbol)
		     (push (list symbol 'custom-variable) found))))
    (if (not found)
	(error "No customized user options")
      (custom-buffer-create (custom-sort-items found t nil)
			    "*Customize Customized*"))))

;;;###autoload
(defun customize-saved ()
  "Customize all already saved user options."
  (interactive)
  (let ((found nil))
    (mapatoms (lambda (symbol)
		(and (or (get symbol 'saved-face)
			 (get symbol 'saved-face-comment))
		     (find-face symbol)
		     (push (list symbol 'custom-face) found))
		(and (or (get symbol 'saved-value)
			 (get symbol 'saved-variable-comment))
		     (boundp symbol)
		     (push (list symbol 'custom-variable) found))))
    (if (not found )
	(error "No saved user options")
      (custom-buffer-create (custom-sort-items found t nil)
			    "*Customize Saved*"))))

;;;###autoload
(defalias 'apropos-customize 'customize-apropos)

;;;###autoload
(defun customize-apropos (regexp &optional all)
  "Customize all user options matching REGEXP.
If ALL is `options', include only options.
If ALL is `faces', include only faces.
If ALL is `groups', include only groups.
If ALL is t (interactively, with prefix arg), include options which are not
user-settable, as well as faces and groups."
  (interactive "sCustomize regexp: \nP")
  (let ((found nil))
    (mapatoms (lambda (symbol)
		(when (string-match regexp (symbol-name symbol))
		  (when (and (not (memq all '(faces options)))
			     (get symbol 'custom-group))
		    (push (list symbol 'custom-group) found))
		  (when (and (not (memq all '(options groups)))
			     (find-face symbol))
		    (push (list symbol 'custom-face) found))
		  (when (and (not (memq all '(groups faces)))
			     (boundp symbol)
			     (or (get symbol 'saved-value)
				 (get symbol 'standard-value)
				 (if (memq all '(nil options))
				     (user-variable-p symbol)
				   (get symbol 'variable-documentation))))
		    (push (list symbol 'custom-variable) found)))))
    (if (not found)
	(error "No matches")
      (custom-buffer-create (custom-sort-items found t
					       custom-buffer-order-groups)
			    "*Customize Apropos*"))))

;;;###autoload
(defun customize-apropos-options (regexp &optional arg)
  "Customize all user options matching REGEXP.
With prefix arg, include options which are not user-settable."
  (interactive "sCustomize regexp: \nP")
  (customize-apropos regexp (or arg 'options)))

;;;###autoload
(defun customize-apropos-faces (regexp)
  "Customize all user faces matching REGEXP."
  (interactive "sCustomize regexp: \n")
  (customize-apropos regexp 'faces))

;;;###autoload
(defun customize-apropos-groups (regexp)
  "Customize all user groups matching REGEXP."
  (interactive "sCustomize regexp: \n")
  (customize-apropos regexp 'groups))


;;; Buffer.

(defcustom custom-buffer-style 'links
  "*Control the presentation style for customization buffers.
The value should be a symbol, one of:

brackets: groups nest within each other with big horizontal brackets.
links: groups have links to subgroups."
  :type '(radio (const :tag "brackets: Groups nest within each others" brackets)
		(const :tag "links: Group have links to subgroups" links))
  :group 'custom-buffer)

(defcustom custom-buffer-done-function 'kill-buffer
  "*Function to be used to remove the buffer when the user is done with it.
Choices include `kill-buffer' (the default) and `bury-buffer'.
The function will be called with one argument, the buffer to remove."
  :type '(radio (function-item kill-buffer)
		(function-item bury-buffer)
		(function :tag "Other" nil))
  :group 'custom-buffer)

(defcustom custom-buffer-indent 3
  "Number of spaces to indent nested groups."
  :type 'integer
  :group 'custom-buffer)

;;;###autoload
(defun custom-buffer-create (options &optional name description)
  "Create a buffer containing OPTIONS.
Optional NAME is the name of the buffer.
OPTIONS should be an alist of the form ((SYMBOL WIDGET)...), where
SYMBOL is a customization option, and WIDGET is a widget for editing
that option."
  (unless name (setq name "*Customization*"))
  (kill-buffer (get-buffer-create name))
  (switch-to-buffer (get-buffer-create name))
  (custom-buffer-create-internal options description))

;;;###autoload
(defun custom-buffer-create-other-window (options &optional name description)
  "Create a buffer containing OPTIONS.
Optional NAME is the name of the buffer.
OPTIONS should be an alist of the form ((SYMBOL WIDGET)...), where
SYMBOL is a customization option, and WIDGET is a widget for editing
that option."
  (unless name (setq name "*Customization*"))
  (kill-buffer (get-buffer-create name))
  (let ((window (selected-window)))
    (switch-to-buffer-other-window (get-buffer-create name))
    (custom-buffer-create-internal options description)
    (select-window window)))

(defcustom custom-reset-button-menu t
  "If non-nil, only show a single reset button in customize buffers.
This button will have a menu with all three reset operations."
  :type 'boolean
  :group 'custom-buffer)

(defconst custom-skip-messages 5)

(defun Custom-buffer-done ()
  "Remove current buffer.
This works by calling the function specified by
 `custom-buffer-done-function'."
  (interactive)
  (funcall custom-buffer-done-function (current-buffer)))

(defun custom-buffer-create-buttons ()
  (message "Creating customization buttons...")
  (widget-insert "\nOperate on everything in this buffer:\n ")
  (widget-create 'push-button
		 :tag "Set"
		 :help-echo "Make your editing in this buffer take effect for this session"
		 :action (lambda (widget &optional event)
			   (Custom-set)))
  (widget-insert " ")
  (widget-create 'push-button
		 :tag "Save"
		 :help-echo "Make your editing in this buffer take effect for future Emacs sessions"
		 :action (lambda (widget &optional event)
			   (Custom-save)))
  (if custom-reset-button-menu
      (progn
	(widget-insert " ")
	(widget-create 'push-button
		       :tag "Reset"
		       :tag-glyph '("reset-up" "reset-down")
		       :help-echo "Show a menu with reset operations"
		       :mouse-down-action (lambda (&rest junk) t)
		       :action (lambda (widget &optional event)
				 (custom-reset event))))
    (widget-insert " ")
    (widget-create 'push-button
		   :tag "Reset"
		   :help-echo "Reset all edited text in this buffer to reflect current values"
		   :action 'Custom-reset-current)
    (widget-insert " ")
    (widget-create 'push-button
		   :tag "Reset to Saved"
		   :help-echo "Reset all values in this buffer to their saved settings"
		   :action 'Custom-reset-saved)
    (widget-insert " ")
    (widget-create 'push-button
		   :tag "Reset to Standard"
		   :help-echo "Reset all values in this buffer to their standard settings"
		   :action 'Custom-reset-standard))
  (widget-insert "  ")
  (widget-create 'push-button
		 :tag "Done"
		 :help-echo "Remove the buffer"
		 :action (lambda (widget &optional event)
			   (Custom-buffer-done)))
  (widget-insert "\n"))

(defcustom custom-novice t
  "If non-nil, show help message at top of customize buffers."
  :type 'boolean
  :group 'custom-buffer)

(defcustom custom-display-global-buttons 'top
  "If `nil' don't display the global buttons.  If `top' display at the
beginning of custom buffers.  If `bottom', display at the end."
  :type '(choice (const top)
		 (const bottom)
		 (const :tag "don't" nil))
  :group 'custom-buffer)

(defun custom-buffer-create-internal (options &optional description)
  (message "Creating customization buffer...")
  (custom-mode)
  (widget-insert "This is a customization buffer")
  (if description
      (widget-insert description))
  (when custom-novice
      (widget-insert ".\n\
Type RET or click button2 on an active field to invoke its action.
Invoke ")
      (widget-create 'info-link
		     :tag "Help"
		     :help-echo "Read the online help"
		     "(XEmacs)Easy Customization")
      (widget-insert " for more information."))
  (widget-insert "\n")
  (if (equal custom-display-global-buttons 'top)
      (custom-buffer-create-buttons))
  (widget-insert "\n")
  (message "Creating customization items...")
  (setq custom-options
	(if (= (length options) 1)
	    (mapcar (lambda (entry)
		      (widget-create (nth 1 entry)
				     :documentation-shown t
				     :custom-state 'unknown
				     :tag (custom-unlispify-tag-name
					   (nth 0 entry))
				     :value (nth 0 entry)))
		    options)
	  (let ((count 0)
		(length (length options)))
	    (mapcar (lambda (entry)
		      (prog2
			  (display-message
			   'progress
			   (format "Creating customization items %2d%%..."
				   (/ (* 100.0 count) length)))
			  (widget-create (nth 1 entry)
					 :tag (custom-unlispify-tag-name
					       (nth 0 entry))
					 :value (nth 0 entry))
			(incf count)
			(unless (eq (preceding-char) ?\n)
			  (widget-insert "\n"))
			(widget-insert "\n")))
		    options))))
  (unless (eq (preceding-char) ?\n)
    (widget-insert "\n"))
  (if (equal custom-display-global-buttons 'bottom)
      (custom-buffer-create-buttons))
  (display-message 'progress
		   (format
		    "Creating customization items %2d%%...done" 100))
  (unless (eq custom-buffer-style 'tree)
    (mapc 'custom-magic-reset custom-options))
  (message "Creating customization setup...")
  (widget-setup)
  (goto-char (point-min))
  (message "Creating customization buffer...done"))


;;; The Tree Browser.

;;;###autoload
(defun customize-browse (&optional group)
  "Create a tree browser for the customize hierarchy."
  (interactive)
  (unless group
    (setq group 'emacs))
  (let ((name "*Customize Browser*"))
    (kill-buffer (get-buffer-create name))
    (switch-to-buffer (get-buffer-create name)))
  (custom-mode)
  (widget-insert "Square brackets show active fields; type RET or click button2
on an active field to invoke its action.
Invoke [+] below to expand a group, and [-] to collapse an expanded group.\n")
  (if custom-browse-only-groups
      (widget-insert "Invoke the [Group] button below to edit that item in another window.\n\n")
    (widget-insert "Invoke the ")
    (widget-create 'item
		   :format "%t"
		   :tag "[Group]"
		   :tag-glyph "folder")
    (widget-insert ", ")
    (widget-create 'item
		   :format "%t"
		   :tag "[Face]"
		   :tag-glyph "face")
    (widget-insert ", and ")
    (widget-create 'item
		   :format "%t"
		   :tag "[Option]"
		   :tag-glyph "option")
    (widget-insert " buttons below to edit that
item in another window.\n\n"))
  (let ((custom-buffer-style 'tree))
    (widget-create 'custom-group
		   :custom-last t
		   :custom-state 'unknown
		   :tag (custom-unlispify-tag-name group)
		   :value group))
  (widget-add-change)
  (goto-char (point-min)))

(define-widget 'custom-browse-visibility 'item
  "Control visibility of items in the customize tree browser."
  :format "%[[%t]%]"
  :action 'custom-browse-visibility-action)

(defun custom-browse-visibility-action (widget &rest ignore)
  (let ((custom-buffer-style 'tree))
    (custom-toggle-parent widget)))

(define-widget 'custom-browse-group-tag 'push-button
  "Show parent in other window when activated."
  :tag "Group"
  :tag-glyph "folder"
  :action 'custom-browse-group-tag-action)

(defun custom-browse-group-tag-action (widget &rest ignore)
  (let ((parent (widget-get widget :parent)))
    (customize-group-other-window (widget-value parent))))

(define-widget 'custom-browse-variable-tag 'push-button
  "Show parent in other window when activated."
  :tag "Option"
  :tag-glyph "option"
  :action 'custom-browse-variable-tag-action)

(defun custom-browse-variable-tag-action (widget &rest ignore)
  (let ((parent (widget-get widget :parent)))
    (customize-variable-other-window (widget-value parent))))

(define-widget 'custom-browse-face-tag 'push-button
  "Show parent in other window when activated."
  :tag "Face"
  :tag-glyph "face"
  :action 'custom-browse-face-tag-action)

(defun custom-browse-face-tag-action (widget &rest ignore)
  (let ((parent (widget-get widget :parent)))
    (customize-face-other-window (widget-value parent))))

(defconst custom-browse-alist '(("   " "space")
				(" | " "vertical")
				("-\\ " "top")
				(" |-" "middle")
				(" `-" "bottom")))

(defun custom-browse-insert-prefix (prefix)
  "Insert PREFIX.  On XEmacs convert it to line graphics."
  ;; #### Unfinished.
  (if nil ; (string-match "XEmacs" emacs-version)
      (progn
	(insert "*")
	(while (not (string-equal prefix ""))
	  (let ((entry (substring prefix 0 3)))
	    (setq prefix (substring prefix 3))
	    (let ((overlay (make-overlay (1- (point)) (point) nil t nil))
		  (name (nth 1 (assoc entry custom-browse-alist))))
	      (overlay-put overlay 'end-glyph (widget-glyph-find name entry))
	      (overlay-put overlay 'start-open t)
	      (overlay-put overlay 'end-open t)))))
    (insert prefix)))


;;; Modification of Basic Widgets.
;;
;; We add extra properties to the basic widgets needed here.  This is
;; fine, as long as we are careful to stay within out own namespace.
;;
;; We want simple widgets to be displayed by default, but complex
;; widgets to be hidden.

(widget-put (get 'item 'widget-type) :custom-show t)
(widget-put (get 'editable-field 'widget-type)
	    :custom-show (lambda (widget value)
			   ;; This used to call pp-to-string
			   (let ((pp (widget-prettyprint-to-string value)))
			     (cond ((string-match "\n" pp)
				    nil)
				   ((> (length pp) 40)
				    nil)
				   (t t)))))
(widget-put (get 'menu-choice 'widget-type) :custom-show t)

;;; The `custom-manual' Widget.

(define-widget 'custom-manual 'info-link
  "Link to the manual entry for this customization option."
  :tag "Manual")

;;; The `custom-magic' Widget.

(defgroup custom-magic-faces nil
  "Faces used by the magic button."
  :group 'custom-faces
  :group 'custom-buffer)

(defface custom-invalid-face '((((class color))
				(:foreground "yellow" :background "red"))
			       (t
				(:bold t :italic t :underline t)))
  "Face used when the customize item is invalid."
  :group 'custom-magic-faces)

(defface custom-rogue-face '((((class color))
			      (:foreground "pink" :background "black"))
			     (t
			      (:underline t)))
  "Face used when the customize item is not defined for customization."
  :group 'custom-magic-faces)

(defface custom-modified-face '((((class color))
				 (:foreground "white" :background "blue"))
				(t
				 (:italic t :bold)))
  "Face used when the customize item has been modified."
  :group 'custom-magic-faces)

(defface custom-set-face '((((class color))
				(:foreground "blue" :background "white"))
			       (t
				(:italic t)))
  "Face used when the customize item has been set."
  :group 'custom-magic-faces)

(defface custom-changed-face '((((class color))
				(:foreground "white" :background "blue"))
			       (t
				(:italic t)))
  "Face used when the customize item has been changed."
  :group 'custom-magic-faces)

(defface custom-saved-face '((t (:underline t)))
  "Face used when the customize item has been saved."
  :group 'custom-magic-faces)

(defconst custom-magic-alist
  '((nil "#" underline "uninitialized, you should not see this.")
    (unknown "?" italic "unknown, you should not see this.")
    (hidden "-" default
	    "hidden, invoke \"Show\" button in the previous line to show."
	    "group now hidden, invoke the above \"Show\" button to show contents.")
    (invalid "x" custom-invalid-face
	     "the value displayed for this %c is invalid and cannot be set.")
    (modified "*" custom-modified-face
	      "you have edited the value as text, but you have not set the %c."
	      "you have edited something in this group, but not set it.")
    (set "+" custom-set-face
	 "you have set this %c, but not saved it for future sessions."
	 "something in this group has been set, but not saved.")
    (changed ":" custom-changed-face
	     "this %c has been changed outside the customize buffer."
	     "something in this group has been changed outside customize.")
    (saved "!" custom-saved-face
	   "this %c has been set and saved."
	   "something in this group has been set and saved.")
    (rogue "@" custom-rogue-face
	   "this %c has not been changed with customize."
	   "something in this group is not prepared for customization.")
    (standard " " nil
	      "this %c is unchanged from its standard setting."
	      "visible group members are all at standard settings."))
  "Alist of customize option states.
Each entry is of the form (STATE MAGIC FACE ITEM-DESC [ GROUP-DESC ]), where

STATE is one of the following symbols:

`nil'
   For internal use, should never occur.
`unknown'
   For internal use, should never occur.
`hidden'
   This item is not being displayed.
`invalid'
   This item is modified, but has an invalid form.
`modified'
   This item is modified, and has a valid form.
`set'
   This item has been set but not saved.
`changed'
   The current value of this item has been changed temporarily.
`saved'
   This item is marked for saving.
`rogue'
   This item has no customization information.
`standard'
   This item is unchanged from the standard setting.

MAGIC is a string used to present that state.

FACE is a face used to present the state.

ITEM-DESC is a string describing the state for options.

GROUP-DESC is a string describing the state for groups.  If this is
left out, ITEM-DESC will be used.

The string %c in either description will be replaced with the
category of the item.  These are `group'. `option', and `face'.

The list should be sorted most significant first.")

(defcustom custom-magic-show 'long
  "If non-nil, show textual description of the state.
If `long', show a full-line description, not just one word."
  :type '(choice (const :tag "no" nil)
		 (const short)
		 (const long))
  :group 'custom-buffer)

(defcustom custom-magic-show-hidden '(option face)
  "Control whether the State button is shown for hidden items.
The value should be a list with the custom categories where the State
button should be visible.  Possible categories are `group', `option',
and `face'."
  :type '(set (const group) (const option) (const face))
  :group 'custom-buffer)

(defcustom custom-magic-show-button nil
  "Show a \"magic\" button indicating the state of each customization option."
  :type 'boolean
  :group 'custom-buffer)

(define-widget 'custom-magic 'default
  "Show and manipulate state for a customization option."
  :format "%v"
  :action 'widget-parent-action
  :notify 'ignore
  :value-get 'ignore
  :value-create 'custom-magic-value-create
  :value-delete 'widget-children-value-delete)

(defun widget-magic-mouse-down-action (widget &optional event)
  ;; Non-nil unless hidden.
  (not (eq (widget-get (widget-get (widget-get widget :parent) :parent)
		       :custom-state)
	   'hidden)))

(defun custom-magic-value-create (widget)
  ;; Create compact status report for WIDGET.
  (let* ((parent (widget-get widget :parent))
	 (state (widget-get parent :custom-state))
	 (hidden (eq state 'hidden))
	 (entry (assq state custom-magic-alist))
	 (magic (nth 1 entry))
	 (face (nth 2 entry))
	 (category (widget-get parent :custom-category))
	 (text (or (and (eq category 'group)
			(nth 4 entry))
		   (nth 3 entry)))
	 (form (widget-get parent :custom-form))
	 children)
    (while (string-match #r"\`\(.*\)%c\(.*\)\'" text)
      (setq text (concat (match-string 1 text)
			 (symbol-name category)
			 (match-string 2 text))))
    (when (and custom-magic-show
	       (or (not hidden)
		   (memq category custom-magic-show-hidden)))
      (insert "   ")
      (when (and (eq category 'group)
		 (not (and (eq custom-buffer-style 'links)
			   (> (widget-get parent :custom-level) 1))))
	(insert-char ?\  (* custom-buffer-indent
			    (widget-get parent :custom-level))))
      (push (widget-create-child-and-convert
	     widget 'choice-item
	     :help-echo "Change the state of this item"
	     :format (if hidden "%t" "%[%t%]")
	     :button-prefix 'widget-push-button-prefix
	     :button-suffix 'widget-push-button-suffix
	     :mouse-down-action 'widget-magic-mouse-down-action
	     :tag "State"
	     ;;:tag-glyph (or hidden '("state-up" "state-down"))
	     )
	    children)
      (insert ": ")
      (let ((start (point)))
	(if (eq custom-magic-show 'long)
	    (insert text)
	  (insert (symbol-name state)))
	(cond ((eq form 'lisp)
	       (insert " (lisp)"))
	      ((eq form 'mismatch)
	       (insert " (mismatch)")))
	(put-text-property start (point) 'face 'custom-state-face))
      (insert "\n"))
    (when (and (eq category 'group)
	       (not (and (eq custom-buffer-style 'links)
			 (> (widget-get parent :custom-level) 1))))
      (insert-char ?\  (* custom-buffer-indent
			  (widget-get parent :custom-level))))
    (when custom-magic-show-button
      (when custom-magic-show
	(let ((indent (widget-get parent :indent)))
	  (when indent
	    (insert-char ?\  indent))))
      (push (widget-create-child-and-convert
	     widget 'choice-item
	     :mouse-down-action 'widget-magic-mouse-down-action
	     :button-face face
	     :button-prefix ""
	     :button-suffix ""
	     :help-echo "Change the state"
	     :format (if hidden "%t" "%[%t%]")
	     :tag (if (memq form '(lisp mismatch))
		      (concat "(" magic ")")
		    (concat "[" magic "]")))
	    children)
      (insert " "))
    (widget-put widget :children children)))

(defun custom-magic-reset (widget)
  "Redraw the :custom-magic property of WIDGET."
  (let ((magic (widget-get widget :custom-magic)))
    (widget-value-set magic (widget-value magic))))

;;; The `custom' Widget.

(defface custom-button-face '((t (:bold t)))
  "Face used for buttons in customization buffers."
  :group 'custom-faces)

(defface custom-documentation-face nil
  "Face used for documentation strings in customization buffers."
  :group 'custom-faces)

(defface custom-state-face '((((class color)
			       (background dark))
			      (:foreground "lime green"))
			     (((class color)
			       (background light))
			      (:foreground "dark green"))
			     (t nil))
  "Face used for State descriptions in the customize buffer."
  :group 'custom-faces)

(define-widget 'custom 'default
  "Customize a user option."
  :format "%v"
  :convert-widget 'custom-convert-widget
  :notify 'custom-notify
  :custom-prefix ""
  :custom-level 1
  :custom-state 'hidden
  :documentation-property 'widget-subclass-responsibility
  :value-create 'widget-subclass-responsibility
  :value-delete 'widget-children-value-delete
  :value-get 'widget-value-value-get
  :validate 'widget-children-validate
  :match (lambda (widget value) (symbolp value)))

(defun custom-convert-widget (widget)
  ;; Initialize :value and :tag from :args in WIDGET.
  (let ((args (widget-get widget :args)))
    (when args
      (widget-put widget :value (widget-apply widget
					      :value-to-internal (car args)))
      (widget-put widget :tag (custom-unlispify-tag-name (car args)))
      (widget-put widget :args nil)))
  widget)

(defun custom-notify (widget &rest args)
  "Keep track of changes."
  (let ((state (widget-get widget :custom-state)))
    (unless (eq state 'modified)
      (unless (memq state '(nil unknown hidden))
	(widget-put widget :custom-state 'modified))
      (custom-magic-reset widget)
      (apply 'widget-default-notify widget args))))

(defun custom-redraw (widget)
  "Redraw WIDGET with current settings."
  (let ((line (count-lines (point-min) (point)))
	(column (current-column))
	(pos (point))
	(from (marker-position (widget-get widget :from)))
	(to (marker-position (widget-get widget :to))))
    (save-excursion
      (widget-value-set widget (widget-value widget))
      (custom-redraw-magic widget))
    (when (and (>= pos from) (<= pos to))
      (condition-case nil
	  (progn
	    (if (> column 0)
		(goto-line line)
	      (goto-line (1+ line)))
	    (move-to-column column))
	(error nil)))))

(defun custom-redraw-magic (widget)
  "Redraw WIDGET state with current settings."
  (while widget
    (let ((magic (widget-get widget :custom-magic)))
      (cond (magic
	     (widget-value-set magic (widget-value magic))
	     (when (setq widget (widget-get widget :group))
	       (custom-group-state-update widget)))
	    (t
	     (setq widget nil)))))
  (widget-setup))

(defun custom-show (widget value)
  "Non-nil if WIDGET should be shown with VALUE by default."
  (let ((show (widget-get widget :custom-show)))
    (cond ((null show)
	   nil)
	  ((eq t show)
	   t)
	  (t
	   (funcall show widget value)))))

(defvar custom-load-recursion nil
  "Hack to avoid recursive dependencies.")

(defun custom-load-symbol (symbol)
  "Load all dependencies for SYMBOL."
  (unless custom-load-recursion
    (let ((custom-load-recursion t)
	  (loads (get symbol 'custom-loads))
	  load)
      (while loads
	(setq load (car loads)
	      loads (cdr loads))
	(custom-load-symbol-1 load)))))

(defun custom-load-symbol-1 (load)
  (cond ((symbolp load)
	 (condition-case nil
	     (require load)
	   (error nil)))
	;; Don't reload a file already loaded.
	((and (boundp 'preloaded-file-list)
	      (member load preloaded-file-list)))
	((assoc load load-history))
	((assoc (locate-library load) load-history))
	(t
	 (condition-case nil
	     ;; Without this, we would load cus-edit recursively.
	     ;; We are still loading it when we call this,
	     ;; and it is not in load-history yet.
	     (or (equal load "cus-edit")
		 (load-library load))
	   (error nil)))))

(defvar custom-already-loaded-custom-defines nil
  "List of already-loaded `custom-defines' files.")
(defvar custom-define-current-source-file nil)
(defvar custom-warn-when-reloading-necessary nil
  "For package-debugging purposes: Warn when an error hit in custom-defines.el.
When this happens, the file from which the defcustom or defgroup was taken
is loaded, and custom-defines.el is then reloaded.  This works in most
cases, but may not be completely safe.  It's better if the package itself
arranges for the necessary functions and variables to be available, using
\;;;###autoload declarations.  When this variable is non-nil, warnings are
issued (with backtrace), to aid in tracking down the problems.")

(defun custom-load-custom-defines (symbol)
  "Load custom-defines for SYMBOL."
  (unless custom-load-recursion
    (let ((custom-load-recursion t)
	  (loads (get symbol 'custom-loads))
	  load)
      (while loads
	(setq load (car loads)
	      loads (cdr loads))
	(let* ((found (locate-library
		       (if (symbolp load) (symbol-name load) load)))
	       (dir (and found (file-name-directory found))))
	  ;; If we find a custom-defines file, assume the package is smart
	  ;; enough to have put all its defcustoms and defgroups here, and
	  ;; load it instead of the file itself.  Otherwise, do it the
	  ;; hard way.
	  (if (and found (or (file-exists-p
			      (expand-file-name "custom-defines.elc" dir))
			     (file-exists-p
			      (expand-file-name "custom-defines.el" dir))))
	      (when (not (member dir custom-already-loaded-custom-defines))
		(push dir custom-already-loaded-custom-defines)
		(custom-load-custom-defines-1 dir))))))))

(defun custom-load-custom-defines-1 (dir)
  ;; Actually load the custom-defines.el file in DIR.

  ;; If we get an error loading the custom-defines, it may be because of a
  ;; reference to something (e.g. a constant) that hasn't yet been defined
  ;; yet.  Properly, these should have been marked, so they either go into
  ;; the custom-defines.el file or are autoloaded.  But not everyone is so
  ;; careful, so for the moment we try to load the file that the
  ;; error-generating defcustom came from, and then reload the
  ;; custom-defines.el file.  We might loop a number of times if we have
  ;; various files that need loading.  If at any point we get an error that
  ;; can't be solved just by loading the appropriate file (e.g. we hit the
  ;; same error as before, the file is already loaded, etc.) then we signal
  ;; it as a real error.
  (let (source)
    ;; here's how this works: if we get an error loading custom-defines,
    ;; the condition handler is called; if we need to reload, we
    ;; `return-from', which throws out of the handler and returns nil from
    ;; the `block', which continues the while statement, executing the
    ;; `load' at the bottom of this function and then entering the block
    ;; again.  if the condition handler doesn't throw, but instead returns
    ;; normally, `signal' will continue as if nothing happened, and end up
    ;; signalling the error normally.
    (while
	(not
	 (block custom-load
	   ;; Use call-with-condition-handler so the error can be seen
	   ;; with the stack intact.
	   (call-with-condition-handler
	       #'(lambda (__custom_load_cd1__)
		   (when (and
			  custom-define-current-source-file
			  (progn
			    (setq source (expand-file-name
					  custom-define-current-source-file
					  dir))
			    (let ((nondir (file-name-nondirectory source)))
			      (and (file-exists-p source)
				   (not (assoc source load-history))
				   (not (assoc nondir load-history))
				   (not (and (boundp 'preloaded-file-list)
					     (member nondir
						     preloaded-file-list)))))))
		     (if custom-warn-when-reloading-necessary
			 (lwarn 'custom-defines 'warning
			   "Error while loading custom-defines, fetching source and reloading ...\n
Error: %s\n
Source file: %s\n\n
Backtrace follows:\n\n%s"
			   (error-message-string __custom_load_cd1__)
			   source
			   (backtrace-in-condition-handler-eliminating-handler
			    '__custom_load_cd1__)))
		     (return-from custom-load nil)))
	       #'(lambda ()
		   (load (expand-file-name "custom-defines" dir))))))
      ;; we get here only from the `return-from'; see above
      (load source))))

(defun custom-load-widget (widget)
  "Load all dependencies for WIDGET."
  (custom-load-symbol (widget-value widget)))

(defun custom-unloaded-symbol-p (symbol)
  "Return non-nil if the dependencies of SYMBOL has not yet been loaded."
  (let ((found nil)
	(loads (get symbol 'custom-loads))
	load)
    (while loads
      (setq load (car loads)
	    loads (cdr loads))
      (cond ((symbolp load)
	     (unless (featurep load)
	       (setq found t)))
	    ((assoc load load-history))
	    ((assoc (locate-library load) load-history)
	     ;; #### WTF???
	     (message nil))
	    (t
	     (setq found t))))
    found))

(defun custom-unloaded-widget-p (widget)
  "Return non-nil if the dependencies of WIDGET has not yet been loaded."
  (custom-unloaded-symbol-p (widget-value widget)))

(defun custom-toggle-hide (widget)
  "Toggle visibility of WIDGET."
  (custom-load-widget widget)
  (let ((state (widget-get widget :custom-state)))
    (cond ((memq state '(invalid modified))
	   (error "There are unset changes"))
	  ((eq state 'hidden)
	   (widget-put widget :custom-state 'unknown))
	  (t
	   (widget-put widget :documentation-shown nil)
	   (widget-put widget :custom-state 'hidden)))
    (custom-redraw widget)
    (widget-setup)))

(defun custom-toggle-parent (widget &rest ignore)
  "Toggle visibility of parent of WIDGET."
  (custom-toggle-hide (widget-get widget :parent)))

(defun custom-add-see-also (widget &optional prefix)
  "Add `See also ...' to WIDGET if there are any links.
Insert PREFIX first if non-nil."
  (let* ((symbol (widget-get widget :value))
	 (links (get symbol 'custom-links))
	 (many (> (length links) 2))
	 (buttons (widget-get widget :buttons))
	 (indent (widget-get widget :indent)))
    (when links
      (when indent
	(insert-char ?\  indent))
      (when prefix
	(insert prefix))
      (insert "See also ")
      (while links
	(push (widget-create-child-and-convert widget (car links))
	      buttons)
	(setq links (cdr links))
	(cond ((null links)
	       (insert ".\n"))
	      ((null (cdr links))
	       (if many
		   (insert ", and ")
		 (insert " and ")))
	      (t
	       (insert ", "))))
      (widget-put widget :buttons buttons))))

(defun custom-add-parent-links (widget &optional initial-string)
  "Add \"Parent groups: ...\" to WIDGET if the group has parents.
The value if non-nil if any parents were found.
If INITIAL-STRING is non-nil, use that rather than \"Parent groups:\"."
  (let ((name (widget-value widget))
	(type (widget-type widget))
	(buttons (widget-get widget :buttons))
	(start (point))
	found)
    (insert (or initial-string "Parent groups:"))
    (maphash (lambda (group ignore)
	       (let ((entry (assq name (get group 'custom-group))))
		 (when (eq (nth 1 entry) type)
		   (insert " ")
		   (push (widget-create-child-and-convert
			  widget 'custom-group-link
			  :tag (custom-unlispify-tag-name group)
			  group)
			 buttons)
		   (setq found t))))
	     custom-group-hash-table)
    (widget-put widget :buttons buttons)
    (if found
	(insert "\n")
      (delete-region start (point)))
    found))

;;; The `custom-comment' Widget.

;; like the editable field
(defface custom-comment-face '((((class grayscale color)
				 (background light))
				(:background "gray85"))
			       (((class grayscale color)
				 (background dark))
				(:background "dim gray"))
			       (t
				(:italic t)))
  "Face used for comments on variables or faces"
  :group 'custom-faces)

;; like font-lock-comment-face
(defface custom-comment-tag-face
  '((((class color) (background dark)) (:foreground "gray80"))
    (((class color) (background light)) (:foreground "blue4"))
    (((class grayscale) (background light))
     (:foreground "DimGray" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :bold t :italic t))
    (t (:bold t)))
  "Face used for variables or faces comment tags"
  :group 'custom-faces)

(define-widget 'custom-comment 'string
  "User comment"
  :tag "Comment"
  :help-echo "Edit a comment here"
  :sample-face 'custom-comment-tag-face
  :value-face 'custom-comment-face
  :value-set 'custom-comment-value-set
  :create 'custom-comment-create
  :delete 'custom-comment-delete)

(defun custom-comment-create (widget)
  (let (ext)
    (widget-default-create widget)
    (widget-put widget :comment-extent
		(setq ext (make-extent (widget-get widget :from)
				       (widget-get widget :to))))
    (set-extent-property ext 'start-open t)
    (when (equal (widget-get widget :value) "")
      (set-extent-property ext 'invisible t))
    ))

(defun custom-comment-delete (widget)
  (widget-default-delete widget)
  (delete-extent (widget-get widget :comment-extent)))

(defun custom-comment-value-set (widget value)
  (widget-default-value-set widget value)
  (if (equal value "")
      (set-extent-property (widget-get widget :comment-extent)
			   'invisible t)
    (set-extent-property (widget-get widget :comment-extent)
			 'invisible nil)))

;; Those functions are for the menu. WIDGET is NOT the comment widget. It's
;; the global custom one
(defun custom-comment-show (widget)
  (set-extent-property
   (widget-get (widget-get widget :comment-widget) :comment-extent)
   'invisible nil))

(defun custom-comment-invisible-p (widget)
  (extent-property
   (widget-get (widget-get widget :comment-widget) :comment-extent)
   'invisible))

;;; The `custom-variable' Widget.

(defface custom-variable-tag-face '((((class color)
				      (background dark))
				     (:foreground "light blue" :underline t))
				    (((class color)
				      (background light))
				     (:foreground "blue" :underline t))
				    (t (:underline t)))
  "Face used for unpushable variable tags."
  :group 'custom-faces)

(defface custom-variable-button-face '((t (:underline t :bold t)))
  "Face used for pushable variable tags."
  :group 'custom-faces)

(defcustom custom-variable-default-form 'edit
  "Default form of displaying variable values."
  :type '(choice (const edit)
		 (const lisp))
  :group 'custom-buffer)

(define-widget 'custom-variable 'custom
  "Customize variable."
  :format "%v"
  :help-echo "Set or reset this variable"
  :documentation-property 'variable-documentation
  :custom-category 'option
  :custom-state nil
  :custom-menu 'custom-variable-menu-create
  :custom-form nil ; defaults to value of `custom-variable-default-form'
  :value-create 'custom-variable-value-create
  :action 'custom-variable-action
  :custom-set 'custom-variable-set
  :custom-pre-save 'custom-variable-pre-save
  :custom-save 'custom-variable-save
  :custom-post-save 'custom-variable-post-save
  :custom-reset-current 'custom-redraw
  :custom-reset-saved 'custom-variable-reset-saved
  :custom-pre-reset-standard 'custom-variable-pre-reset-standard
  :custom-reset-standard 'custom-variable-reset-standard
  :custom-post-reset-standard 'custom-variable-post-reset-standard)

(defun custom-variable-type (symbol)
  "Return a widget suitable for editing the value of SYMBOL.
If SYMBOL has a `custom-type' property, use that.
Otherwise, look up symbol in `custom-guess-type-alist'."
  (let* ((type (or (get symbol 'custom-type)
		   (and (not (get symbol 'standard-value))
			(custom-guess-type symbol))
		   'sexp))
	 (options (get symbol 'custom-options))
	 (tmp (if (listp type)
		  (copy-sequence type)
		(list type))))
    (when options
      (widget-put tmp :options options))
    tmp))

(defun custom-variable-value-create (widget)
  "Here is where you edit the variables value."
  (custom-load-widget widget)
  (unless (widget-get widget :custom-form)
    (widget-put widget :custom-form custom-variable-default-form))
  (let* ((buttons (widget-get widget :buttons))
	 (children (widget-get widget :children))
	 (form (widget-get widget :custom-form))
	 (state (widget-get widget :custom-state))
	 (symbol (widget-get widget :value))
	 (tag (widget-get widget :tag))
	 (type (custom-variable-type symbol))
	 (conv (widget-convert type))
	 (get (or (get symbol 'custom-get) 'default-value))
	 (prefix (widget-get widget :custom-prefix))
	 (last (widget-get widget :custom-last))
	 (value (if (default-boundp symbol)
		    (funcall get symbol)
		  (widget-get conv :value))))
    ;; If the widget is new, the child determine whether it is hidden.
    (cond (state)
	  ((custom-show type value)
	   (setq state 'unknown))
	  (t
	   (setq state 'hidden)))
    ;; If we don't know the state, see if we need to edit it in lisp form.
    (when (eq state 'unknown)
      (unless (widget-apply conv :match value)
	;; (widget-apply (widget-convert type) :match value)
	(setq form 'mismatch)))
    ;; Now we can create the child widget.
    (cond ((eq custom-buffer-style 'tree)
	   (insert prefix (if last " `--- " " |--- "))
	   (push (widget-create-child-and-convert
		  widget 'custom-browse-variable-tag)
		 buttons)
	   (insert " " tag "\n")
	   (widget-put widget :buttons buttons))
	  ((eq state 'hidden)
	   ;; Indicate hidden value.
	   (push (widget-create-child-and-convert
		  widget 'item
		  :format "%{%t%}: "
		  :sample-face 'custom-variable-tag-face
		  :tag tag
		  :parent widget)
		 buttons)
	   (push (widget-create-child-and-convert
		  widget 'visibility
		  :help-echo "Show the value of this option"
		  :action 'custom-toggle-parent
		  nil)
		 buttons))
	  ((memq form '(lisp mismatch))
	   ;; In lisp mode edit the saved value when possible.
	   (let* ((value (cond ((get symbol 'saved-value)
				(car (get symbol 'saved-value)))
			       ((get symbol 'standard-value)
				(car (get symbol 'standard-value)))
			       ((default-boundp symbol)
				(custom-quote (funcall get symbol)))
			       (t
				(custom-quote (widget-get conv :value))))))
	     (insert (symbol-name symbol) ": ")
	     (push (widget-create-child-and-convert
		    widget 'visibility
		    :help-echo "Hide the value of this option"
		    :action 'custom-toggle-parent
		    t)
		   buttons)
	     (insert " ")
	     (push (widget-create-child-and-convert
		    widget 'sexp
		    :button-face 'custom-variable-button-face
		    :format "%v"
		    :tag (symbol-name symbol)
		    :parent widget
		    :value value)
		   children)))
	  (t
	   ;; Edit mode.
	   (let* ((format (widget-get type :format))
		  tag-format value-format)
	     (while (not (string-match ":" format))
	       (setq format (signal 'error (list "Bad format" format))))
	     (setq tag-format (substring format 0 (match-end 0)))
	     (setq value-format (substring format (match-end 0)))
	     (push (widget-create-child-and-convert
		    widget 'item
		    :format tag-format
		    :action 'custom-tag-action
		    :help-echo "Change value of this option"
		    :mouse-down-action 'custom-tag-mouse-down-action
		    :button-face 'custom-variable-button-face
		    :sample-face 'custom-variable-tag-face
		    tag)
		   buttons)
	     (insert " ")
	     (push (widget-create-child-and-convert
		  widget 'visibility
		  :help-echo "Hide the value of this option"
		  :action 'custom-toggle-parent
		  t)
		 buttons)
	     (push (widget-create-child-and-convert
		    widget type
		    :format value-format
		    :value value)
		   children))))
    (unless (eq custom-buffer-style 'tree)
      (unless (eq (preceding-char) ?\n)
	(widget-insert "\n"))
      ;; Create the magic button.
      (let ((magic (widget-create-child-and-convert
		    widget 'custom-magic nil)))
	(widget-put widget :custom-magic magic)
	(push magic buttons))
      ;; Insert documentation.
      ;; #### NOTE: this is ugly!!!! I need to do update the :buttons property
      ;; before the call to `widget-default-format-handler'. Otherwise, I
      ;; lose my current `buttons'. This function shouldn't be called like
      ;; this anyway. The doc string widget should be added like the others.
      ;; --dv
      (widget-put widget :buttons buttons)
      (widget-default-format-handler widget ?h)
      ;; The comment field
      (unless (eq state 'hidden)
	(let* ((comment (get symbol 'variable-comment))
	       (comment-widget
		(widget-create-child-and-convert
		 widget 'custom-comment
		 :parent widget
		 :value (or comment ""))))
	  (widget-put widget :comment-widget comment-widget)
	  ;; Don't push it !!! Custom assumes that the first child is the
	  ;; value one.
	  (setq children (append children (list comment-widget)))))
      ;; Update the rest of the properties.
      (widget-put widget :custom-form form)
      (widget-put widget :children children)
      ;; Now update the state.
      (if (eq state 'hidden)
	  (widget-put widget :custom-state state)
	(custom-variable-state-set widget))
      ;; See also.
      (unless (eq state 'hidden)
	(when (eq (widget-get widget :custom-level) 1)
	  (custom-add-parent-links widget))
	(custom-add-see-also widget)))))

(defun custom-tag-action (widget &rest args)
  "Pass :action to first child of WIDGET's parent."
  (apply 'widget-apply (car (widget-get (widget-get widget :parent) :children))
	 :action args))

(defun custom-tag-mouse-down-action (widget &rest args)
  "Pass :mouse-down-action to first child of WIDGET's parent."
  (apply 'widget-apply (car (widget-get (widget-get widget :parent) :children))
	 :mouse-down-action args))

(defun custom-variable-state-set (widget)
  "Set the state of WIDGET."
  (let* ((symbol (widget-value widget))
	 (get (or (get symbol 'custom-get) 'default-value))
	 (value (if (default-boundp symbol)
		    (funcall get symbol)
		  (widget-get widget :value)))
	 (comment (get symbol 'variable-comment))
	 tmp
	 temp
	 (state (cond ((progn (setq tmp (get symbol 'customized-value))
			      (setq temp
				    (get symbol 'customized-variable-comment))
			      (or tmp temp))
		       (if (condition-case nil
			       (and (equal value (eval (car tmp)))
				    (equal comment temp))
			     (error nil))
			   'set
			 'changed))
		      ((progn (setq tmp (get symbol 'saved-value))
			      (setq temp (get symbol 'saved-variable-comment))
			      (or tmp temp))
		       (if (condition-case nil
			       (and (equal value (eval (car tmp)))
				    (equal comment temp))
			     (error nil))
			   'saved
			 'changed))
		      ((setq tmp (get symbol 'standard-value))
		       (if (condition-case nil
			       (and (equal value (eval (car tmp)))
				    (equal comment nil))
			     (error nil))
			   'standard
			 'changed))
		      (t 'rogue))))
    (widget-put widget :custom-state state)))

(defvar custom-variable-menu
  `(("Set for Current Session" custom-variable-set
     ,#'(lambda (widget)
	  (eq (widget-get widget :custom-state) 'modified)))
    ("Save for Future Sessions" custom-variable-save
     ,#'(lambda (widget)
	  (memq (widget-get widget :custom-state)
		'(modified set changed rogue))))
    ("Reset to Current" custom-redraw
     ,#'(lambda (widget)
	  (and (default-boundp (widget-value widget))
	       (memq (widget-get widget :custom-state) '(modified changed)))))
    ("Reset to Saved" custom-variable-reset-saved
     ,#'(lambda (widget)
	  (and (or (get (widget-value widget) 'saved-value)
		   (get (widget-value widget) 'saved-variable-comment))
	       (memq (widget-get widget :custom-state)
		     '(modified set changed rogue)))))
    ("Reset to Standard Settings" custom-variable-reset-standard
     ,#'(lambda (widget)
	  (and (get (widget-value widget) 'standard-value)
	       (memq (widget-get widget :custom-state)
		     '(modified set changed saved rogue)))))
    ("---" ignore ignore)
    ("Add Comment" custom-comment-show custom-comment-invisible-p)
    ("---" ignore ignore)
    ("Don't show as Lisp expression" custom-variable-edit
     ,#'(lambda (widget)
	  (eq (widget-get widget :custom-form) 'lisp)))
    ("Show as Lisp expression" custom-variable-edit-lisp
     ,#'(lambda (widget)
	  (eq (widget-get widget :custom-form) 'edit))))
  "Alist of actions for the `custom-variable' widget.
Each entry has the form (NAME ACTION FILTER) where NAME is the name of
the menu entry, ACTION is the function to call on the widget when the
menu is selected, and FILTER is a predicate which takes a `custom-variable'
widget as an argument, and returns non-nil if ACTION is valid on that
widget. If FILTER is nil, ACTION is always valid.")

(defun custom-variable-action (widget &optional event)
  "Show the menu for `custom-variable' WIDGET.
Optional EVENT is the location for the menu."
  (if (eq (widget-get widget :custom-state) 'hidden)
      (custom-toggle-hide widget)
    (unless (eq (widget-get widget :custom-state) 'modified)
      (custom-variable-state-set widget))
    ;; Redrawing magic also depresses the state glyph.
    ;(custom-redraw-magic widget)
    (let* ((completion-ignore-case t)
	   (answer (widget-choose (concat "Operation on "
					  (custom-unlispify-tag-name
					   (widget-get widget :value)))
				  (custom-menu-filter custom-variable-menu
						      widget)
				  event)))
      (if answer
	  (funcall answer widget)))))

(defun custom-variable-edit (widget)
  "Edit value of WIDGET."
  (widget-put widget :custom-state 'unknown)
  (widget-put widget :custom-form 'edit)
  (custom-redraw widget))

(defun custom-variable-edit-lisp (widget)
  "Edit the lisp representation of the value of WIDGET."
  (widget-put widget :custom-state 'unknown)
  (widget-put widget :custom-form 'lisp)
  (custom-redraw widget))

(defun custom-variable-set (widget)
  "Set the current value for the variable being edited by WIDGET."
  (let* ((form (widget-get widget :custom-form))
	 (state (widget-get widget :custom-state))
	 (child (car (widget-get widget :children)))
	 (symbol (widget-value widget))
	 (set (or (get symbol 'custom-set) 'set-default))
	 (comment-widget (widget-get widget :comment-widget))
	 (comment (widget-value comment-widget))
	 val)
    (cond ((eq state 'hidden)
	   (error "Cannot set hidden variable"))
	  ((setq val (widget-apply child :validate))
	   (goto-char (widget-get val :from))
	   (error "%s" (widget-get val :error)))
	  ((memq form '(lisp mismatch))
	   (when (equal comment "")
	     (setq comment nil)
	     ;; Make the comment invisible by hand if it's empty
	     (set-extent-property (widget-get comment-widget :comment-extent)
				  'invisible t))
	   (funcall set symbol (eval (setq val (widget-value child))))
	   (put symbol 'customized-value (list val))
	   (put symbol 'variable-comment comment)
	   (put symbol 'customized-variable-comment comment))
	  (t
	   (when (equal comment "")
	     (setq comment nil)
	     ;; Make the comment invisible by hand if it's empty
	     (set-extent-property (widget-get comment-widget :comment-extent)
				  'invisible t))
	   (funcall set symbol (setq val (widget-value child)))
	   (put symbol 'customized-value (list (custom-quote val)))
	   (put symbol 'variable-comment comment)
	   (put symbol 'customized-variable-comment comment)))
    (custom-variable-state-set widget)
    (custom-redraw-magic widget)))

(defun custom-variable-pre-save (widget)
  "Prepare for saving the value for the variable being edited by WIDGET."
  (let* ((form (widget-get widget :custom-form))
	 (state (widget-get widget :custom-state))
	 (child (car (widget-get widget :children)))
	 (symbol (widget-value widget))
	 (set (or (get symbol 'custom-set) 'set-default))
	 (comment-widget (widget-get widget :comment-widget))
	 (comment (widget-value comment-widget))
	 val)
    (cond ((eq state 'hidden)
	   (error "Cannot set hidden variable"))
	  ((setq val (widget-apply child :validate))
	   (goto-char (widget-get val :from))
	   (error "%s" (widget-get val :error)))
	  ((memq form '(lisp mismatch))
	   (when (equal comment "")
	     (setq comment nil)
	     ;; Make the comment invisible by hand if it's empty
	     (set-extent-property (widget-get comment-widget :comment-extent)
				  'invisible t))
	   (put symbol 'saved-value (list (widget-value child)))
	   (custom-push-theme 'theme-value symbol 'user
			      'set (list (widget-value child)))
	   (funcall set symbol (eval (widget-value child)))
	   (put symbol 'variable-comment comment)
	   (put symbol 'saved-variable-comment comment))
	  (t
	   (when (equal comment "")
	     (setq comment nil)
	     ;; Make the comment invisible by hand if it's empty
	     (set-extent-property (widget-get comment-widget :comment-extent)
				  'invisible t))
	   (put symbol
		'saved-value (list (custom-quote (widget-value
						  child))))
	   (custom-push-theme 'theme-value symbol 'user
			      'set (list (custom-quote (widget-value
						  child))))
	   (funcall set symbol (widget-value child))
	   (put symbol 'variable-comment comment)
	   (put symbol 'saved-variable-comment comment)))
    (put symbol 'customized-value nil)
    (put symbol 'customized-variable-comment nil)
    ))

(defun custom-variable-post-save (widget)
  "Finish saving the variable being edited by WIDGET."
  (custom-variable-state-set widget)
  (custom-redraw-magic widget))

(defun custom-variable-save (widget)
  "Set and save the value for the variable being edited by WIDGET."
  (custom-variable-pre-save widget)
  (custom-save-all)
  (custom-variable-post-save widget))

(defun custom-variable-reset-saved (widget)
  "Restore the saved value for the variable being edited by WIDGET."
  (let* ((symbol (widget-value widget))
	 (set (or (get symbol 'custom-set) 'set-default))
	 (value (get symbol 'saved-value))
	 (comment (get symbol 'saved-variable-comment)))
    (cond ((or value comment)
	   (put symbol 'variable-comment comment)
	   (condition-case nil
	       (funcall set symbol (eval (car value)))
	     (error nil)))
	  (t
	   (signal 'error (list "No saved value for variable" symbol))))
    (put symbol 'customized-value nil)
    (put symbol 'customized-variable-comment nil)
    (widget-put widget :custom-state 'unknown)
    ;; This call will possibly make the comment invisible
    (custom-redraw widget)))

;; This function returns non nil if we need to re-save the options --dv.
(defun custom-variable-pre-reset-standard (widget)
  "Prepare for restoring the variable being edited by WIDGET to its
standard setting."
  (let* ((symbol (widget-value widget))
	 (set (or (get symbol 'custom-set) 'set-default)))
    (if (get symbol 'standard-value)
	(funcall set symbol (eval (car (get symbol 'standard-value))))
      (signal 'error (list "No standard setting known for variable" symbol)))
    (put symbol 'variable-comment nil)
    (put symbol 'customized-value nil)
    (put symbol 'customized-variable-comment nil)
    (when (or (get symbol 'saved-value) (get symbol 'saved-variable-comment))
      (put symbol 'saved-value nil)
      (custom-push-theme 'theme-value symbol 'user 'reset 'standard)
      ;; As a special optimizations we do not (explictly)
      ;; save resets to standard when no theme sets the value.
      (if (null (cdr (get symbol 'theme-value)))
	  (put symbol 'theme-value nil))
      (put symbol 'saved-variable-comment nil)
      widget)
    ))

(defun custom-variable-post-reset-standard (widget)
  "Finish resetting the variable being edited by WIDGET to its standard
value."
  (widget-put widget :custom-state 'unknown)
  ;; This call will possibly make the comment invisible
  (custom-redraw widget))

(defun custom-variable-reset-standard (widget)
  "Restore the standard setting for the variable being edited by WIDGET."
  (when (custom-variable-pre-reset-standard widget)
    (custom-save-all))
  (custom-variable-post-reset-standard widget))

;;; The `custom-face-edit' Widget.

(define-widget 'custom-face-edit 'checklist
  "Edit face attributes."
  :format "%t: %v"
  :tag "Attributes"
  :extra-offset 12
  :button-args '(:help-echo "Control whether this attribute has any effect")
  :args (mapcar (lambda (att)
		  (list 'group
			:inline t
			:sibling-args (widget-get (nth 1 att) :sibling-args)
			(list 'const :format "" :value (nth 0 att))
			(nth 1 att)))
		custom-face-attributes))

;;; The `custom-display' Widget.

(define-widget 'custom-display 'menu-choice
  "Select a display type."
  :tag "Display"
  :value t
  :help-echo "Specify frames where the face attributes should be used"
  :args '((const :tag "all" t)
	  (checklist
	   :offset 0
	   :extra-offset 9
	   :args ((group :sibling-args (:help-echo "Only match the specified window systems")
			 (const :format "Type: "
				type)
			 (checklist :inline t
				    :offset 0
				    (const :format "X "
					   :sibling-args (:help-echo "The X11 Window System")
					   x)
				    (const :format "PM "
					   :sibling-args (:help-echo "OS/2 Presentation Manager")
					   pm)
				    (const :format "TTY%n"
					   :sibling-args (:help-echo "Plain text terminals")
					   tty)))
		  (group :sibling-args (:help-echo "Only match display or printer devices")
			 (const :format "Output: "
				class)
			 (checklist :inline t
				    :offset 0
				    (const :format "Display "
					   :sibling-args (:help-echo "Match display devices")
					   display)
				    (const :format "Printer%n"
					   :sibling-args (:help-echo "Match printer devices")
					   printer)))
		  (group :sibling-args (:help-echo "Only match the frames with the specified color support")
			 (const :format "Color support: "
				class)
			 (checklist :inline t
				    :offset 0
				    (const :format "Color "
					   :sibling-args (:help-echo "Match color frames")
					   color)
				    (const :format "Grayscale "
					   :sibling-args (:help-echo "Match grayscale frames")
					   grayscale)
				    (const :format "Monochrome%n"
					   :sibling-args (:help-echo "Match frames with no color support")
					   mono)))
		  (group :sibling-args (:help-echo "Only match frames with the specified intensity")
			 (const :format "Background brightness: "
				background)
			 (checklist :inline t
				    :offset 0
				    (const :format "Light "
					   :sibling-args (:help-echo "Match frames with light backgrounds")
					   light)
				    (const :format "Dark\n"
					   :sibling-args (:help-echo "Match frames with dark backgrounds")
					   dark)))))))

;;; The `custom-face' Widget.

(defface custom-face-tag-face '((t (:underline t)))
  "Face used for face tags."
  :group 'custom-faces)

(defcustom custom-face-default-form 'selected
  "Default form of displaying face definition."
  :type '(choice (const all)
		 (const selected)
		 (const lisp))
  :group 'custom-buffer)

(define-widget 'custom-face 'custom
  "Customize face."
  :sample-face 'custom-face-tag-face
  :help-echo "Set or reset this face"
  :documentation-property #'(lambda (face)
			      (face-doc-string face))
  :value-create 'custom-face-value-create
  :action 'custom-face-action
  :custom-category 'face
  :custom-form nil ; defaults to value of `custom-face-default-form'
  :custom-set 'custom-face-set
  :custom-pre-save 'custom-face-pre-save
  :custom-save 'custom-face-save
  :custom-post-save 'custom-face-post-save
  :custom-reset-current 'custom-redraw
  :custom-reset-saved 'custom-face-reset-saved
  :custom-pre-reset-standard 'custom-face-pre-reset-standard
  :custom-reset-standard 'custom-face-reset-standard
  :custom-post-reset-standard 'custom-face-post-reset-standard
  :custom-menu 'custom-face-menu-create)

(define-widget 'custom-face-all 'editable-list
  "An editable list of display specifications and attributes."
  :entry-format "%i %d %v"
  :insert-button-args '(:help-echo "Insert new display specification here")
  :append-button-args '(:help-echo "Append new display specification here")
  :delete-button-args '(:help-echo "Delete this display specification")
  :args '((group :format "%v" custom-display custom-face-edit)))

(defconst custom-face-all (widget-convert 'custom-face-all)
  "Converted version of the `custom-face-all' widget.")

(define-widget 'custom-display-unselected 'item
  "A display specification that doesn't match the selected display."
  :match 'custom-display-unselected-match)

(defun custom-display-unselected-match (widget value)
  "Non-nil if VALUE is an unselected display specification."
  (not (face-spec-set-match-display value (selected-frame))))

(define-widget 'custom-face-selected 'group
  "Edit the attributes of the selected display in a face specification."
  :args '((repeat :format ""
		  :inline t
		  (group custom-display-unselected sexp))
	  (group (sexp :format "") custom-face-edit)
	  (repeat :format ""
		  :inline t
		  sexp)))

(defconst custom-face-selected (widget-convert 'custom-face-selected)
  "Converted version of the `custom-face-selected' widget.")

(defun custom-face-value-create (widget)
  "Create a list of the display specifications for WIDGET."
  (let ((buttons (widget-get widget :buttons))
	children
	(symbol (widget-get widget :value))
	(tag (widget-get widget :tag))
	(state (widget-get widget :custom-state))
	(begin (point))
	(is-last (widget-get widget :custom-last))
	(prefix (widget-get widget :custom-prefix)))
    (unless tag
      (setq tag (prin1-to-string symbol)))
    (cond ((eq custom-buffer-style 'tree)
	   (insert prefix (if is-last " `--- " " |--- "))
	   (push (widget-create-child-and-convert
		  widget 'custom-browse-face-tag)
		 buttons)
	   (insert " " tag "\n")
	   (widget-put widget :buttons buttons))
	  (t
	   ;; Create tag.
	   (insert tag)
	   (if (eq custom-buffer-style 'face)
	       (insert " ")
	     (widget-specify-sample widget begin (point))
	     (insert ": "))
	   ;; Sample.
	   (and (not (find-face symbol))
		;; XEmacs cannot display uninitialized faces.
		(make-face symbol))
	   (push (widget-create-child-and-convert widget 'item
						  :format "(%{%t%})"
						  :sample-face symbol
						  :tag "sample")
		 buttons)
	   ;; Visibility.
	   (insert " ")
	   (push (widget-create-child-and-convert
		  widget 'visibility
		  :help-echo "Hide or show this face"
		  :action 'custom-toggle-parent
		  (not (eq state 'hidden)))
		 buttons)
	   ;; Magic.
	   (insert "\n")
	   (let ((magic (widget-create-child-and-convert
			 widget 'custom-magic nil)))
	     (widget-put widget :custom-magic magic)
	     (push magic buttons))
	   ;; Update buttons.
	   (widget-put widget :buttons buttons)
	   ;; Insert documentation.
	   (widget-default-format-handler widget ?h)
	   ;; The comment field
	   (unless (eq state 'hidden)
	     (let* ((comment (get symbol 'face-comment))
		    (comment-widget
		     (widget-create-child-and-convert
		      widget 'custom-comment
		      :parent widget
		      :value (or comment ""))))
	       (widget-put widget :comment-widget comment-widget)
	       (push comment-widget children)))
	   ;; See also.
	   (unless (eq state 'hidden)
	     (when (eq (widget-get widget :custom-level) 1)
	       (custom-add-parent-links widget))
	     (custom-add-see-also widget))
	   ;; Editor.
	   (unless (eq (preceding-char) ?\n)
	     (insert "\n"))
	   (unless (eq state 'hidden)
	     (message "Creating face editor...")
	     (custom-load-widget widget)
	     (unless (widget-get widget :custom-form)
		 (widget-put widget :custom-form custom-face-default-form))
	     (let* ((symbol (widget-value widget))
		    (spec (custom-face-get-spec symbol))
		    (form (widget-get widget :custom-form))
		    (indent (widget-get widget :indent))
		    (edit (widget-create-child-and-convert
			   widget
			   (cond ((and (eq form 'selected)
				       (widget-apply custom-face-selected
						     :match spec))
				  (when indent (insert-char ?\  indent))
				  'custom-face-selected)
				 ((and (not (eq form 'lisp))
				       (widget-apply custom-face-all
						     :match spec))
				  'custom-face-all)
				 (t
				  (when indent (insert-char ?\  indent))
				  'sexp))
			   :value spec)))
	       (custom-face-state-set widget)
	       (push edit children)
	       (widget-put widget :children children))
	     (message "Creating face editor...done"))))))

(defvar custom-face-menu
  `(("Set for Current Session" custom-face-set)
    ("Save for Future Sessions" custom-face-save)
    ("Reset to Saved" custom-face-reset-saved
     ,#'(lambda (widget)
	  (or (get (widget-value widget) 'saved-face)
	      (get (widget-value widget) 'saved-face-comment))))
    ("Reset to Standard Setting" custom-face-reset-standard
     ,#'(lambda (widget)
	  (get (widget-value widget) 'face-defface-spec)))
    ("---" ignore ignore)
    ("Add Comment" custom-comment-show custom-comment-invisible-p)
    ("---" ignore ignore)
    ("Show all display specs" custom-face-edit-all
     ,#'(lambda (widget)
	  (not (eq (widget-get widget :custom-form) 'all))))
    ("Just current attributes" custom-face-edit-selected
     ,#'(lambda (widget)
	  (not (eq (widget-get widget :custom-form) 'selected))))
    ("Show as Lisp expression" custom-face-edit-lisp
     ,#'(lambda (widget)
	  (not (eq (widget-get widget :custom-form) 'lisp)))))
  "Alist of actions for the `custom-face' widget.
Each entry has the form (NAME ACTION FILTER) where NAME is the name of
the menu entry, ACTION is the function to call on the widget when the
menu is selected, and FILTER is a predicate which takes a `custom-face'
widget as an argument, and returns non-nil if ACTION is valid on that
widget. If FILTER is nil, ACTION is always valid.")

(defun custom-face-edit-selected (widget)
  "Edit selected attributes of the value of WIDGET."
  (widget-put widget :custom-state 'unknown)
  (widget-put widget :custom-form 'selected)
  (custom-redraw widget))

(defun custom-face-edit-all (widget)
  "Edit all attributes of the value of WIDGET."
  (widget-put widget :custom-state 'unknown)
  (widget-put widget :custom-form 'all)
  (custom-redraw widget))

(defun custom-face-edit-lisp (widget)
  "Edit the lisp representation of the value of WIDGET."
  (widget-put widget :custom-state 'unknown)
  (widget-put widget :custom-form 'lisp)
  (custom-redraw widget))

(defun custom-face-state-set (widget)
  "Set the state of WIDGET."
  (let* ((symbol (widget-value widget))
	 (comment (get symbol 'face-comment))
	 tmp temp)
    (widget-put widget :custom-state
		(cond ((progn
			 (setq tmp (get symbol 'customized-face))
			 (setq temp (get symbol 'customized-face-comment))
			 (or tmp temp))
		       (if (equal temp comment)
			   'set
			 'changed))
		      ((progn
			 (setq tmp (get symbol 'saved-face))
			 (setq temp (get symbol 'saved-face-comment))
			 (or tmp temp))
		       (if (equal temp comment)
			   'saved
			 'changed))
		      ((get symbol 'face-defface-spec)
		       (if (equal comment nil)
			   'standard
			 'changed))
		      (t
		       'rogue)))))

(defun custom-face-action (widget &optional event)
  "Show the menu for `custom-face' WIDGET.
Optional EVENT is the location for the menu."
  (if (eq (widget-get widget :custom-state) 'hidden)
      (custom-toggle-hide widget)
    (let* ((completion-ignore-case t)
	   (symbol (widget-get widget :value))
	   (answer (widget-choose (concat "Operation on "
					  (custom-unlispify-tag-name symbol))
				  (custom-menu-filter custom-face-menu
						      widget)
				  event)))
      (if answer
	  (funcall answer widget)))))

(defun custom-face-set (widget)
  "Make the face attributes in WIDGET take effect."
  (let* ((symbol (widget-value widget))
	 (child (car (widget-get widget :children)))
	 (value (widget-value child))
	 (comment-widget (widget-get widget :comment-widget))
	 (comment (widget-value comment-widget)))
    (when (equal comment "")
      (setq comment nil)
      ;; Make the comment invisible by hand if it's empty
      (set-extent-property (widget-get comment-widget :comment-extent)
			   'invisible t))
    (put symbol 'customized-face value)
    (face-spec-set symbol value nil '(custom))
    (put symbol 'customized-face-comment comment)
    (put symbol 'face-comment comment)
    (custom-face-state-set widget)
    (custom-redraw-magic widget)))

(defun custom-face-pre-save (widget)
  "Prepare for saving the face being edited by WIDGET."
  (let* ((symbol (widget-value widget))
	 (child (car (widget-get widget :children)))
	 (value (widget-value child))
	 (comment-widget (widget-get widget :comment-widget))
	 (comment (widget-value comment-widget)))
    (when (equal comment "")
      (setq comment nil)
      ;; Make the comment invisible by hand if it's empty
      (set-extent-property (widget-get comment-widget :comment-extent)
			   'invisible t))
    (face-spec-set symbol value nil '(custom))
    (put symbol 'saved-face value)
    (custom-push-theme 'theme-face symbol 'user 'set value)
    (put symbol 'customized-face nil)
    (put symbol 'face-comment comment)
    (put symbol 'customized-face-comment nil)
    (put symbol 'saved-face-comment comment)
    ))

(defun custom-face-post-save (widget)
  "Finish saving the face being edited by WIDGET."
  (custom-face-state-set widget)
  (custom-redraw-magic widget))

(defun custom-face-save (widget)
  "Save the face being edited by WIDGET."
  (custom-face-pre-save widget)
  (custom-save-all)
  (custom-face-post-save widget))

(defun custom-face-reset-saved (widget)
  "Reset the face being edited by WIDGET to its saved value."
  (let* ((symbol (widget-value widget))
	 (child (car (widget-get widget :children)))
	 (value (get symbol 'saved-face))
	 (comment (get symbol 'saved-face-comment))
	 (comment-widget (widget-get widget :comment-widget)))
    (unless (or value comment)
      (signal 'error (list "No saved value for this face" symbol)))
    (put symbol 'customized-face nil)
    (put symbol 'customized-face-comment nil)
    (face-spec-set symbol value nil '(custom))
    (put symbol 'face-comment comment)
    (widget-value-set child value)
    ;; This call manages the comment visibility
    (widget-value-set comment-widget (or comment ""))
    (custom-face-state-set widget)
    (custom-redraw-magic widget)))

;; This function returns non nil if we need to re-save the options --dv.
(defun custom-face-pre-reset-standard (widget)
  "Prepare for restoring the face edited by WIDGET to its standard
settings."
  (let* ((symbol (widget-value widget))
	 (value (get symbol 'face-defface-spec)))
    (unless value
      (signal 'error (list "No standard setting for this face" symbol)))
    (put symbol 'customized-face nil)
    (put symbol 'customized-face-comment nil)
    (when (or (get symbol 'saved-face) (get symbol 'saved-face-comment))
      (put symbol 'saved-face nil)
      (custom-push-theme 'theme-face symbol 'user 'reset 'standard)
      ;; Do not explictly save resets to standards without themes.
      (if (null (cdr (get symbol 'theme-face)))
	  (put symbol  'theme-face nil))
      (put symbol 'saved-face-comment nil)
      widget)
    ))

(defun custom-face-post-reset-standard (widget)
  "Finish restoring the face edited by WIDGET to its standard settings."
  (let* ((symbol (widget-value widget))
	 (child (car (widget-get widget :children)))
	 (value (get symbol 'face-defface-spec))
	 (comment-widget (widget-get widget :comment-widget)))
    (face-spec-set symbol value nil '(custom))
    (put symbol 'face-comment nil)
    (widget-value-set child value)
    ;; This call manages the comment visibility
    (widget-value-set comment-widget "")
    (custom-face-state-set widget)
    (custom-redraw-magic widget)
    ))

(defun custom-face-reset-standard (widget)
  "Restore the face edited by WIDGET to its standard settings."
  (when (custom-face-pre-reset-standard widget)
    (custom-save-all))
  (custom-face-post-reset-standard widget))


;;; The `face' Widget.

(define-widget 'face 'default
  "Select and customize a face."
  :convert-widget 'widget-value-convert-widget
  :button-prefix 'widget-push-button-prefix
  :button-suffix 'widget-push-button-suffix
  :format "%t: %[select face%] %v"
  :tag "Face"
  :value 'default
  :value-create 'widget-face-value-create
  :value-delete 'widget-face-value-delete
  :value-get 'widget-value-value-get
  :validate 'widget-children-validate
  :action 'widget-face-action
  :match (lambda (widget value) (symbolp value)))

(defun widget-face-value-create (widget)
  ;; Create a `custom-face' child.
  (let* ((symbol (widget-value widget))
	 (custom-buffer-style 'face)
	 (child (widget-create-child-and-convert
		 widget 'custom-face
		 :custom-level nil
		 :value symbol)))
    (custom-magic-reset child)
    (setq custom-options (cons child custom-options))
    (widget-put widget :children (list child))))

(defun widget-face-value-delete (widget)
  ;; Remove the child from the options.
  (let ((child (car (widget-get widget :children))))
    (setq custom-options (delq child custom-options))
    (widget-children-value-delete widget)))

(defvar face-history nil
  "History of entered face names.")

(defun widget-face-action (widget &optional event)
  "Prompt for a face."
  (let ((answer (completing-read "Face: "
				 (mapcar (lambda (face)
					   (list (symbol-name face)))
					 (face-list))
				 nil nil nil
				 'face-history)))
    (unless (zerop (length answer))
      (widget-value-set widget (intern answer))
      (widget-apply widget :notify widget event)
      (widget-setup))))

;;; The `hook' Widget.

(define-widget 'hook 'list
  "A emacs lisp hook"
  :value-to-internal (lambda (widget value)
		       (if (symbolp value)
			   (list value)
			 value))
  :match (lambda (widget value)
	   (or (symbolp value)
	       (widget-group-match widget value)))
  :convert-widget 'custom-hook-convert-widget
  :tag "Hook")

(defun custom-hook-convert-widget (widget)
  ;; Handle `:options'.
  (let* ((options (widget-get widget :options))
	 (other `(editable-list :inline t
				:entry-format "%i %d%v"
				(function :format " %v")))
	 (args (if options
		   (list `(checklist :inline t
				     ,@(mapcar (lambda (entry)
						 `(function-item ,entry))
					       options))
			 other)
		 (list other))))
    (widget-put widget :args args)
    widget))

;;; The `plist' Widget.

(define-widget 'plist 'list
  "A property list."
  :match (lambda (widget value)
	   (valid-plist-p value))
  :convert-widget 'custom-plist-convert-widget
  :tag "Property List")

;; #### Should handle options better.
(defun custom-plist-convert-widget (widget)
  (let* ((options (widget-get widget :options))
	 (other `(editable-list :inline t
				(group :inline t
				       (symbol :format "%t: %v "
					       :size 10
					       :tag "Property")
				       (sexp :tag "Value"))))
	 (args
	  (if options
	      `((checklist :inline t
			   ,@(mapcar 'custom-plist-process-option options))
		,other)
	    (list other))))
    (widget-put widget :args args)
    widget))

(defun custom-plist-process-option (entry)
  `(group :inline t
	  (const :tag "Property"
		 :format "%t: %v "
		 :size 10
		 ,entry)
	  (sexp :tag "Value")))

;;; The `custom-group-link' Widget.

(define-widget 'custom-group-link 'link
  "Show parent in other window when activated."
  :help-echo 'custom-group-link-help-echo
  :action 'custom-group-link-action)

(defun custom-group-link-help-echo (widget)
  (concat "Create customization buffer for the `"
	  (custom-unlispify-tag-name (widget-value widget))
	  "' group"))

(defun custom-group-link-action (widget &rest ignore)
  (customize-group (widget-value widget)))

;;; The `custom-group' Widget.

(defcustom custom-group-tag-faces nil
  ;; In XEmacs, this ought to play games with font size.
  "Face used for group tags.
The first member is used for level 1 groups, the second for level 2,
and so forth.  The remaining group tags are shown with
`custom-group-tag-face'."
  :type '(repeat face)
  :group 'custom-faces)

(defface custom-group-tag-face-1 '((((class color)
				     (background dark))
				    (:foreground "pink" :underline t))
				   (((class color)
				     (background light))
				    (:foreground "red" :underline t))
				   (t (:underline t)))
  "Face used for group tags.")

(defface custom-group-tag-face '((((class color)
				   (background dark))
				  (:foreground "light blue" :underline t))
				 (((class color)
				   (background light))
				  (:foreground "blue" :underline t))
				 (t (:underline t)))
  "Face used for low level group tags."
  :group 'custom-faces)

(define-widget 'custom-group 'custom
  "Customize group."
  :format "%v"
  :sample-face-get 'custom-group-sample-face-get
  :documentation-property 'group-documentation
  :help-echo "Set or reset all members of this group"
  :value-create 'custom-group-value-create
  :action 'custom-group-action
  :custom-category 'group
  :custom-set 'custom-group-set
  :custom-pre-save 'custom-group-pre-save
  :custom-save 'custom-group-save
  :custom-post-save 'custom-group-post-save
  :custom-reset-current 'custom-group-reset-current
  :custom-reset-saved 'custom-group-reset-saved
  :custom-pre-reset-standard 'custom-group-pre-reset-standard
  :custom-reset-standard 'custom-group-reset-standard
  :custom-post-reset-standard 'custom-group-post-reset-standard
  :custom-menu 'custom-group-menu-create)

(defun custom-group-sample-face-get (widget)
  ;; Use :sample-face.
  (or (nth (1- (widget-get widget :custom-level)) custom-group-tag-faces)
      'custom-group-tag-face))

(define-widget 'custom-group-visibility 'visibility
  "An indicator and manipulator for hidden group contents."
  :create 'custom-group-visibility-create)

(defun custom-group-visibility-create (widget)
  (let ((visible (widget-value widget)))
    (if visible
	(insert "--------")))
  (widget-default-create widget))

(defun custom-group-members (symbol groups-only)
  "Return SYMBOL's custom group members.
If GROUPS-ONLY non-nil, return only those members that are groups."
  (if (not groups-only)
      (get symbol 'custom-group)
    (let (members)
      (dolist (entry (get symbol 'custom-group) (nreverse members))
	(when (eq (nth 1 entry) 'custom-group)
	  (push entry members))))))

(defun custom-group-value-create (widget)
  "Insert a customize group for WIDGET in the current buffer."
  (let* ((state (widget-get widget :custom-state))
	 (level (widget-get widget :custom-level))
	 ;; (indent (widget-get widget :indent))
	 (prefix (widget-get widget :custom-prefix))
	 (buttons (widget-get widget :buttons))
	 (tag (widget-get widget :tag))
	 (symbol (widget-value widget))
	 (members (custom-group-members symbol
					(and (eq custom-buffer-style 'tree)
					     custom-browse-only-groups))))
    (cond ((and (eq custom-buffer-style 'tree)
		(eq state 'hidden)
		(or members (custom-unloaded-widget-p widget)))
	   (custom-browse-insert-prefix prefix)
	   (push (widget-create-child-and-convert
		  widget 'custom-browse-visibility
		  ;; :tag-glyph "plus"
		  :tag "+")
		 buttons)
	   (insert "-- ")
	   ;; (widget-glyph-insert nil "-- " "horizontal")
	   (push (widget-create-child-and-convert
		  widget 'custom-browse-group-tag)
		 buttons)
	   (insert " " tag "\n")
	   (widget-put widget :buttons buttons))
	  ((and (eq custom-buffer-style 'tree)
		(zerop (length members)))
	   (custom-browse-insert-prefix prefix)
	   (insert "[ ]-- ")
	   ;; (widget-glyph-insert nil "[ ]" "empty")
	   ;; (widget-glyph-insert nil "-- " "horizontal")
	   (push (widget-create-child-and-convert
		  widget 'custom-browse-group-tag)
		 buttons)
	   (insert " " tag "\n")
	   (widget-put widget :buttons buttons))
	  ((eq custom-buffer-style 'tree)
	   (custom-browse-insert-prefix prefix)
	   (custom-load-widget widget)
	   (if (zerop (length members))
	       (progn
		 (custom-browse-insert-prefix prefix)
		 (insert "[ ]-- ")
		 ;; (widget-glyph-insert nil "[ ]" "empty")
		 ;; (widget-glyph-insert nil "-- " "horizontal")
		 (push (widget-create-child-and-convert
			widget 'custom-browse-group-tag)
		       buttons)
		 (insert " " tag "\n")
		 (widget-put widget :buttons buttons))
	     (push (widget-create-child-and-convert
		    widget 'custom-browse-visibility
		    ;; :tag-glyph "minus"
		    :tag "-")
		   buttons)
	     (insert "-\\ ")
	     ;; (widget-glyph-insert nil "-\\ " "top")
	     (push (widget-create-child-and-convert
		    widget 'custom-browse-group-tag)
		   buttons)
	     (insert " " tag "\n")
	     (widget-put widget :buttons buttons)
	     (message "Creating group...")
	     (let* ((members (custom-sort-items members
			      custom-browse-sort-alphabetically
			      custom-browse-order-groups))
		    (prefixes (widget-get widget :custom-prefixes))
		    (custom-prefix-list (custom-prefix-add symbol prefixes))
		    (extra-prefix (if (widget-get widget :custom-last)
				      "   "
				    " | "))
		    (prefix (concat prefix extra-prefix))
		    children entry)
	       (while members
		 (setq entry (car members)
		       members (cdr members))
		 (push (widget-create-child-and-convert
			widget (nth 1 entry)
			:group widget
			:tag (custom-unlispify-tag-name (nth 0 entry))
			:custom-prefixes custom-prefix-list
			:custom-level (1+ level)
			:custom-last (null members)
			:value (nth 0 entry)
			:custom-prefix prefix)
		       children))
	       (widget-put widget :children (reverse children)))
	     (message "Creating group...done")))
	  ;; Nested style.
	  ((eq state 'hidden)
	   ;; Create level indicator.
	   (unless (eq custom-buffer-style 'links)
	     (insert-char ?\  (* custom-buffer-indent (1- level)))
	     (insert "-- "))
	   ;; Create link indicator.
	   (when (eq custom-buffer-style 'links)
	     (insert " ")
	     (push (widget-create-child-and-convert
		    widget 'custom-group-link
		    :tag "Open"
		    :tag-glyph '("open-up" "open-down")
		    symbol)
		   buttons)
	     (insert " "))
	   ;; Create tag.
	   (let ((begin (point)))
	     (insert tag)
	     (widget-specify-sample widget begin (point)))
	   (insert " group")
	   ;; Create visibility indicator.
	   (unless (eq custom-buffer-style 'links)
	     (insert ": ")
	     (push (widget-create-child-and-convert
		    widget 'custom-group-visibility
		    :help-echo "Show members of this group"
		    :action 'custom-toggle-parent
		    (not (eq state 'hidden)))
		   buttons))
	   (insert " \n")
	   ;; Create magic button.
	   (let ((magic (widget-create-child-and-convert
			 widget 'custom-magic nil)))
	     (widget-put widget :custom-magic magic)
	     (push magic buttons))
	   ;; Update buttons.
	   (widget-put widget :buttons buttons)
	   ;; Insert documentation.
	   (if (and (eq custom-buffer-style 'links) (> level 1))
	       (widget-put widget :documentation-indent 0))
	   (widget-default-format-handler widget ?h))
	  ;; Nested style.
	  (t				;Visible.
	   (custom-load-widget widget)
	   ;; Update members
	   (setq members (custom-group-members
			  symbol (and (eq custom-buffer-style 'tree)
				      custom-browse-only-groups)))
	   ;; Add parent groups references above the group.
	   (if t    ;;; This should test that the buffer
		    ;;; was made to display a group.
	       (when (eq level 1)
		 (if (custom-add-parent-links widget
					      "Go to parent group:")
		     (insert "\n"))))
	   ;; Create level indicator.
	   (insert-char ?\  (* custom-buffer-indent (1- level)))
	   (insert "/- ")
	   ;; Create tag.
	   (let ((start (point)))
	     (insert tag)
	     (widget-specify-sample widget start (point)))
	   (insert " group: ")
	   ;; Create visibility indicator.
	   (unless (eq custom-buffer-style 'links)
	     (insert "--------")
	     (push (widget-create-child-and-convert
		    widget 'visibility
		    :help-echo "Hide members of this group"
		    :action 'custom-toggle-parent
		    (not (eq state 'hidden)))
		   buttons)
	     (insert " "))
	   ;; Create more dashes.
	   ;; Use 76 instead of 75 to compensate for the temporary "<"
	   ;; added by `widget-insert'.
	   (insert-char ?- (- 76 (current-column)
			      (* custom-buffer-indent level)))
	   (insert "\\\n")
	   ;; Create magic button.
	   (let ((magic (widget-create-child-and-convert
			 widget 'custom-magic
			 :indent 0
			 nil)))
	     (widget-put widget :custom-magic magic)
	     (push magic buttons))
	   ;; Update buttons.
	   (widget-put widget :buttons buttons)
	   ;; Insert documentation.
	   (widget-default-format-handler widget ?h)
	   ;; Parent groups.
	   (if nil  ;;; This should test that the buffer
		    ;;; was not made to display a group.
	       (when (eq level 1)
		 (insert-char ?\  custom-buffer-indent)
		 (custom-add-parent-links widget)))
	   (custom-add-see-also widget
				(make-string (* custom-buffer-indent level)
					     ?\ ))
	   ;; Members.
	   (message "Creating group...")
	   (let* ((members (custom-sort-items members
					      custom-buffer-sort-alphabetically
					      custom-buffer-order-groups))
		  (prefixes (widget-get widget :custom-prefixes))
		  (custom-prefix-list (custom-prefix-add symbol prefixes))
		  (length (length members))
		  (count 0)
		  (children (mapcar
			     (lambda (entry)
			       (widget-insert "\n")
			       (when (zerop (% count custom-skip-messages))
				 (display-message
				  'progress
				  (format "Creating group members... %2d%%"
					  (/ (* 100.0 count) length))))
			       (incf count)
			       (prog1
				   (widget-create-child-and-convert
				    widget (nth 1 entry)
				    :group widget
				    :tag (custom-unlispify-tag-name
					  (nth 0 entry))
				    :custom-prefixes custom-prefix-list
				    :custom-level (1+ level)
				    :value (nth 0 entry))
				 (unless (eq (preceding-char) ?\n)
				   (widget-insert "\n"))))
			     members)))
	     (message "Creating group magic...")
	     (mapc 'custom-magic-reset children)
	     (message "Creating group state...")
	     (widget-put widget :children children)
	     (custom-group-state-update widget)
	     (message "Creating group... done"))
	   ;; End line
	   (insert "\n")
	   (insert-char ?\  (* custom-buffer-indent (1- level)))
	   (insert "\\- " (widget-get widget :tag) " group end ")
	   (insert-char ?- (- 75 (current-column) (* custom-buffer-indent level)))
	   (insert "/\n")))))

(defvar custom-group-menu
  `(("Set for Current Session" custom-group-set
     ,#'(lambda (widget)
	  (eq (widget-get widget :custom-state) 'modified)))
    ("Save for Future Sessions" custom-group-save
     ,#'(lambda (widget)
	  (memq (widget-get widget :custom-state) '(modified set))))
    ("Reset to Current" custom-group-reset-current
     ,#'(lambda (widget)
	  (memq (widget-get widget :custom-state) '(modified))))
    ("Reset to Saved" custom-group-reset-saved
     ,#'(lambda (widget)
	  (memq (widget-get widget :custom-state) '(modified set))))
    ("Reset to standard setting" custom-group-reset-standard
     ,#'(lambda (widget)
	  (memq (widget-get widget :custom-state) '(modified set saved)))))
  "Alist of actions for the `custom-group' widget.
Each entry has the form (NAME ACTION FILTER) where NAME is the name of
the menu entry, ACTION is the function to call on the widget when the
menu is selected, and FILTER is a predicate which takes a `custom-group'
widget as an argument, and returns non-nil if ACTION is valid on that
widget. If FILTER is nil, ACTION is always valid.")

(defun custom-group-action (widget &optional event)
  "Show the menu for `custom-group' WIDGET.
Optional EVENT is the location for the menu."
  (if (eq (widget-get widget :custom-state) 'hidden)
      (custom-toggle-hide widget)
    (let* ((completion-ignore-case t)
	   (answer (widget-choose (concat "Operation on "
					  (custom-unlispify-tag-name
					   (widget-get widget :value)))
				  (custom-menu-filter custom-group-menu
						      widget)
				  event)))
      (if answer
	  (funcall answer widget)))))

(defun custom-group-set (widget)
  "Set changes in all modified group members."
  (let ((children (widget-get widget :children)))
    (mapc (lambda (child)
	    (when (eq (widget-get child :custom-state) 'modified)
	      (widget-apply child :custom-set)))
	  children)))

(defun custom-group-pre-save (widget)
  "Prepare for saving all modified group members."
  (let ((children (widget-get widget :children)))
    (mapc (lambda (child)
	    (when (memq (widget-get child :custom-state) '(modified set))
	      (widget-apply child :custom-pre-save)))
	  children)))

(defun custom-group-post-save (widget)
  "Save all modified group members."
  (let ((children (widget-get widget :children)))
    (mapc (lambda (child)
	    (when (memq (widget-get child :custom-state) '(modified set))
	      (widget-apply child :custom-post-save)))
	  children)))

(defun custom-group-save (widget)
  "Save all modified group members."
  (custom-group-pre-save widget)
  (custom-save-all)
  (custom-group-post-save widget))

(defun custom-group-reset-current (widget)
  "Reset all modified group members."
  (let ((children (widget-get widget :children)))
    (mapc (lambda (child)
	    (when (eq (widget-get child :custom-state) 'modified)
	      (widget-apply child :custom-reset-current)))
	  children)))

(defun custom-group-reset-saved (widget)
  "Reset all modified or set group members."
  (let ((children (widget-get widget :children)))
    (mapc (lambda (child)
	    (when (memq (widget-get child :custom-state) '(modified set))
	      (widget-apply child :custom-reset-saved)))
	  children)))

;; This function returns non nil when we need to re-save the options --dv.
(defun custom-group-pre-reset-standard (widget)
  "Prepare for resetting all modified, set, or saved group members."
  (let ((children (widget-get widget :children))
	must-save)
    (mapc (lambda (child)
	    (when (memq (widget-get child :custom-state)
			'(modified set saved))
	      (and (widget-apply child :custom-pre-reset-standard)
		   (setq must-save t))))
	  children)
    must-save
    ))

(defun custom-group-post-reset-standard (widget)
  "Finish resetting all modified, set, or saved group members."
  (let ((children (widget-get widget :children)))
    (mapc (lambda (child)
	    (when (memq (widget-get child :custom-state)
			'(modified set saved))
	      (widget-apply child :custom-post-reset-standard)))
	  children)))

(defun custom-group-reset-standard (widget)
  "Reset all modified, set, or saved group members."
  (when (custom-group-pre-reset-standard widget)
    (custom-save-all))
  (custom-group-post-reset-standard widget))

(defun custom-group-state-update (widget)
  "Update magic."
  (unless (eq (widget-get widget :custom-state) 'hidden)
    (let* ((children (widget-get widget :children))
	   (states (mapcar (lambda (child)
			     (widget-get child :custom-state))
			   children))
	   (magics custom-magic-alist)
	   (found 'standard))
      (while magics
	(let ((magic (car (car magics))))
	  (if (and (not (eq magic 'hidden))
		   (memq magic states))
	      (setq found magic
		    magics nil)
	    (setq magics (cdr magics)))))
      (widget-put widget :custom-state found)))
  (custom-magic-reset widget))

(defun custom-save-delete (symbol)
  "Delete the call to SYMBOL form in `custom-file'.
Leave point at the location of the call, or after the last expression."
  (let ((find-file-hooks nil)
	(auto-mode-alist nil))
    (set-buffer (find-file-noselect custom-file)))
  (goto-char (point-min))
  (catch 'found
    (while t
      (let ((sexp (condition-case nil
		      (read (current-buffer))
		    (end-of-file (throw 'found nil)))))
	(when (and (listp sexp)
		   (eq (car sexp) symbol))
	  (delete-region (save-excursion
			   (backward-sexp)
			   (point))
			 (point))
	  (throw 'found nil))))))

(defun custom-save-delete-any (&rest symbols)
  "Delete the call to any symbol among SYMBOLS in `custom-file'.
Leave the point at the end of the file."
  (let ((find-file-hooks nil)
	(auto-mode-alist nil))
    (set-buffer (find-file-noselect custom-file)))
  (goto-char (point-min))
  (condition-case nil
      (while (not (eobp))
	(let ((sexp (read (current-buffer))))
	  (when (and (listp sexp)
		     (memq (car sexp) symbols))
	    (delete-region (save-excursion
			     (backward-sexp)
			     (point))
			   (point))
	    (while (and (eolp) (not (eobp)))
	      (delete-region (point) (prog2 (forward-line 1) (point))))
	    )))
    (end-of-file nil)))

(defsubst custom-save-variable-p (symbol)
  "Return non-nil if symbol SYMBOL is a customized variable."
  (and (symbolp symbol)
       (let ((spec (car-safe (get symbol 'theme-value))))
	 (or (and spec (eq (car spec) 'user)
		  (eq (second spec) 'set))
	     (get symbol 'saved-variable-comment)
	     ;; support non-themed vars
	     (and (null spec) (get symbol 'saved-value))))))

(defun custom-save-variable-internal (symbol)
  "Print variable SYMBOL to the standard output.
SYMBOL must be a customized variable."
  (let ((requests (get symbol 'custom-requests))
	(now (not (or (get symbol 'standard-value)
		      (and (not (boundp symbol))
			   (not (eq (get symbol 'force-value)
				    'rogue))))))
	(comment (get symbol 'saved-variable-comment))
	;; Print everything, no placeholders `...'
	(print-level nil)
	(print-length nil))
    (unless (custom-save-variable-p symbol)
      (error 'wrong-type-argument "Not a customized variable" symbol))
    (princ "\n '(")
    (prin1 symbol)
    (princ " ")
    ;; This comment stuff is in the way ####
    ;; Is (eq (third spec) (car saved-value)) ????
    ;; (prin1 (third spec))
    ;; XEmacs -- pretty-print value if available
    (if (and custom-save-pretty-print
	     (fboundp 'pp))
	;; To suppress bytecompiler warning
	(with-fboundp 'pp
	  (pp (car (get symbol 'saved-value))))
      (prin1 (car (get symbol 'saved-value))))
    (when (or now requests comment)
      (princ (if now " t" " nil")))
    (when (or comment requests)
      (princ " ")
      (prin1 requests))
    (when comment
      (princ " ")
      (prin1 comment))
    (princ ")")))

(defun custom-save-variables ()
   "Save all customized variables in `custom-file'."
   (save-excursion
     (custom-save-delete 'custom-load-themes)
     (custom-save-delete 'custom-reset-variables)
     (custom-save-delete 'custom-set-variables)
     ;; This leaves point at the end of file.
     ;; Adrian Aichner <adrian@xemacs.org> stated it is
     ;; a bad behavior <npak@ispras.ru>
     ;;(custom-save-delete-any 'custom-load-themes
     ;;                        'custom-reset-variables
     ;;                        'custom-set-variables)
     (custom-save-loaded-themes)
     (custom-save-resets 'theme-value 'custom-reset-variables nil)
     (let ((standard-output (current-buffer))
	   (sorted-list ()))
       ;; First create a sorted list of saved variables.
       (mapatoms
	(lambda (symbol)
	  (when (custom-save-variable-p symbol)
	    (push symbol sorted-list))))
       (setq sorted-list (sort sorted-list 'string<))
       (unless (bolp)
	 (princ "\n"))
       (princ "(custom-set-variables")
       (mapc 'custom-save-variable-internal
	     sorted-list)
       (princ ")")
       (unless (looking-at "\n")
	 (princ "\n")))))

(defvar custom-save-face-ignoring nil)

(defsubst custom-save-face-p (symbol)
  "Return non-nil if SYMBOL is a customized face."
  (let ((theme-spec (car-safe (get symbol 'theme-face)))
	(comment (get symbol 'saved-face-comment)))
    (or (and (not (memq symbol custom-save-face-ignoring))
	     ;; Don't print default face here.
	     (or (and theme-spec
		      (eq (car theme-spec) 'user)
		      (eq (second theme-spec) 'set))
		 ;; cope with non-themed faces
		 (and (null theme-spec)
		      (get symbol 'saved-face))))
	comment)))

(defun custom-save-face-internal (symbol)
  "Print face SYMBOL to the standard output.
SYMBOL must be a customized face."
  (let ((comment (get symbol 'saved-face-comment))
	(now (not (or (get symbol 'face-defface-spec)
	      (and (not (find-face symbol))
		   (not (eq (get symbol 'force-face) 'rogue))))))
	;; Print everything, no placeholders `...'
	(print-level nil)
	(print-length nil))
    (if (memq symbol custom-save-face-ignoring)
	;; Do nothing
	nil
      ;; Print face
      (unless (custom-save-face-p symbol)
	(error 'wrong-type-argument "Not a customized face" symbol))
      (princ "\n '(")
      (prin1 symbol)
      (princ " ")
      (prin1 (get symbol 'saved-face))
      (if (or comment now)
	  (princ (if now " t" " nil")))
      (when comment
	  (princ " ")
	  (prin1 comment))
      (princ ")"))))

(defun custom-save-faces ()
  "Save all customized faces in `custom-file'."
  (save-excursion
    (custom-save-delete 'custom-reset-faces)
    (custom-save-delete 'custom-set-faces)
    ;; This leaves point at the end of file.
    ;; Adrian Aichner <adrian@xemacs.org> stated it is
    ;; a bad behavior <npak@ispras.ru>
    ;;(custom-save-delete-any 'custom-reset-faces
    ;;                        'custom-set-faces)
    (custom-save-resets 'theme-face 'custom-reset-faces '(default))
    (let ((standard-output (current-buffer))
	  (sorted-list ()))
      ;; Create a sorted list of faces
      (mapatoms
       (lambda (symbol)
	 (when (custom-save-face-p symbol)
	   (push symbol sorted-list))))
      (setq sorted-list (sort sorted-list 'string<))
      (unless (bolp)
	(princ "\n"))
      (princ "(custom-set-faces")
	;; The default face must be first, since it affects the others.
      (when (custom-save-face-p 'default)
	(custom-save-face-internal 'default))
      (let ((custom-save-face-ignoring '(default)))
	(mapc 'custom-save-face-internal
	      sorted-list))
      (princ ")")
      (unless (looking-at "\n")
	(princ "\n")))))

(defmacro make-custom-save-resets-mapper (property setter)
  "Create a mapper for `custom-save-resets'."
  `(lambda (object)
     (let ((spec (car-safe (get object (quote ,property))))
	   (print-level nil)
	   (print-length nil))
       (with-boundp '(ignored-special started-writing)
	 (when (and (not (memq object ignored-special))
		    (eq (car spec) 'user)
		    (eq (second spec) 'reset))
	   ;; Do not write reset statements unless necessary.
	   (unless started-writing
	     (setq started-writing t)
	     (unless (bolp)
	       (princ "\n"))
	     (princ "(")
	     (princ (quote ,setter))
	     (princ "\n '(")
	     (prin1 object)
	     (princ " ")
	     (prin1 (third spec))
	     (princ ")")))))))

(defconst custom-save-resets-mapper-alist
  (eval-when-compile
    (list (list 'theme-value 'custom-reset-variables
		(byte-compile
		 (make-custom-save-resets-mapper
		  'theme-value 'custom-reset-variables)))
	  (list 'theme-face 'custom-reset-faces
		(byte-compile
		 (make-custom-save-resets-mapper
		  'theme-face 'custom-reset-faces)))))
  "Never use it.
Hashes several heavily used functions for `custom-save-resets'")

(defun custom-save-resets (property setter special)
  (declare (special ignored-special))
  (let (started-writing ignored-special)
    ;; (custom-save-delete setter) Done by caller
    (let ((standard-output (current-buffer))
	  (mapper (let ((triple (assq property custom-save-resets-mapper-alist)))
		    (if (and triple (eq (second triple) setter))
			(third triple)
		      (make-custom-save-resets-mapper property setter)))))
      (mapc mapper special)
      (setq ignored-special special)
      (mapatoms mapper)
      (when started-writing
	(princ ")\n")))))


(defun custom-save-loaded-themes ()
  (let ((themes (reverse (get 'user 'theme-loads-themes)))
	(standard-output (current-buffer))
	(print-level nil)
	(print-length nil))
    (when themes
      (unless (bolp) (princ "\n"))
      (princ "(custom-load-themes")
      (mapc (lambda (theme)
	      (princ "\n   '")
	      (prin1 theme)) themes)
      (princ " )\n"))))

;;;###autoload
(defun customize-save-customized ()
  "Save all user options which have been set in this session."
  (interactive)
  (mapatoms (lambda (symbol)
	      (let ((face (get symbol 'customized-face))
		    (value (get symbol 'customized-value))
		    (face-comment (get symbol 'customized-face-comment))
		    (variable-comment
		     (get symbol 'customized-variable-comment)))
		(when face
		  (put symbol 'saved-face face)
		  (custom-push-theme 'theme-face symbol 'user 'set value)
		  (put symbol 'customized-face nil))
		(when value
		  (put symbol 'saved-value value)
		  (custom-push-theme 'theme-value symbol 'user 'set value)
		  (put symbol 'customized-value nil))
		(when variable-comment
		  (put symbol 'saved-variable-comment variable-comment)
		  (put symbol 'customized-variable-comment nil))
		(when face-comment
		  (put symbol 'saved-face-comment face-comment)
		  (put symbol 'customized-face-comment nil)))))
  ;; We really should update all custom buffers here.
  (custom-save-all))

;;;###autoload
(defun custom-save-all ()
  "Save all customizations in `custom-file'."
  (let ((inhibit-read-only t))
    (custom-save-variables)
    (custom-save-faces)
    (let ((find-file-hooks nil)
	  (auto-mode-alist))
      (with-current-buffer (find-file-noselect custom-file)
	(save-buffer)))))


;;; The Customize Menu.

;;; Menu support

(defun custom-face-menu-create (widget symbol)
  "Ignoring WIDGET, create a menu entry for customization face SYMBOL."
  (vector (custom-unlispify-menu-entry symbol)
	  `(customize-face ',symbol)
	  t))

(defun custom-variable-menu-create (widget symbol)
  "Ignoring WIDGET, create a menu entry for customization variable SYMBOL."
  (let ((type (get symbol 'custom-type)))
    (unless (listp type)
      (setq type (list type)))
    (if (and type (widget-get type :custom-menu))
	(widget-apply type :custom-menu symbol)
      (vector (custom-unlispify-menu-entry symbol)
	      `(customize-variable ',symbol)
	      t))))

;; Add checkboxes to boolean variable entries.
(widget-put (get 'boolean 'widget-type)
	    :custom-menu (lambda (widget symbol)
			   `[,(custom-unlispify-menu-entry symbol)
			     (customize-variable ',symbol)
			     :style toggle
			     :selected ,symbol]))

;; XEmacs can create menus dynamically.
(defun custom-group-menu-create (widget symbol)
  "Ignoring WIDGET, create a menu entry for customization group SYMBOL."
  `( ,(custom-unlispify-menu-entry symbol t)
     :filter (lambda (&rest junk)
	       (let ((item (custom-menu-create ',symbol)))
		 (if (listp item)
		     (cdr item)
		   (list item))))))

;;;###autoload
(defun custom-menu-create (symbol)
  "Create menu for customization group SYMBOL.
The menu is in a format applicable to `easy-menu-define'."
  (menu-split-long-menu
   (let* ((item (vector (custom-unlispify-menu-entry symbol)
			`(customize-group ',symbol)
			t)))
     ;; Item is the entry for creating a menu buffer for SYMBOL.
     ;; We may nest, if the menu is not too big.
     (custom-load-custom-defines symbol)
     (if t ;(< (length (get symbol 'custom-group)) widget-menu-max-size)
	 ;; The menu is not too big.
	 (let ((custom-prefix-list (custom-prefix-add symbol
						      custom-prefix-list))
	       (members (custom-sort-items (get symbol 'custom-group)
					   custom-menu-sort-alphabetically
					   custom-menu-order-groups)))
	   ;; Create the menu.
	   `(,(custom-unlispify-menu-entry symbol t)
	     ,item
	     "--"
	     ,@(mapcar (lambda (entry)
			 (widget-apply (if (listp (nth 1 entry))
					   (nth 1 entry)
					 (list (nth 1 entry)))
				       :custom-menu (nth 0 entry)))
		       members)))
       ; else ;; The menu was too big.
       item
       ))))

;;;###autoload
(defun customize-menu-create (symbol &optional name)
  "Return a customize menu for customization group SYMBOL.
If optional NAME is given, use that as the name of the menu.
Otherwise the menu will be named `Customize'.
The format is suitable for use with `easy-menu-define'."
  (unless name
    (setq name "Customize"))
  `(,name
    :filter (lambda (&rest junk)
	      (cdr (custom-menu-create ',symbol)))))

;;; The Custom Mode.

(defvar custom-mode-map nil
  "Keymap for `custom-mode'.")

(unless custom-mode-map
  (setq custom-mode-map (make-sparse-keymap))
  (set-keymap-parents custom-mode-map widget-keymap)
  (suppress-keymap custom-mode-map)
  (define-key custom-mode-map " " 'scroll-up)
  (define-key custom-mode-map [delete] 'scroll-down)
  (define-key custom-mode-map "q" 'Custom-buffer-done)
  (define-key custom-mode-map "u" 'Custom-goto-parent)
  (define-key custom-mode-map "n" 'widget-forward)
  (define-key custom-mode-map "p" 'widget-backward))

(easy-menu-define Custom-mode-menu
    custom-mode-map
  "Menu used in customization buffers."
  `("Custom"
    ,(customize-menu-create 'customize)
    ["Set" Custom-set t]
    ["Save" Custom-save t]
    ["Reset to Current" Custom-reset-current t]
    ["Reset to Saved" Custom-reset-saved t]
    ["Reset to Standard Settings" Custom-reset-standard t]
    ["Info" (Info-goto-node "(xemacs)Easy Customization") t]))

(defun Custom-goto-parent ()
  "Go to the parent group listed at the top of this buffer.
If several parents are listed, go to the first of them."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\nGo to parent group: " nil t)
	(let* ((button (get-char-property (point) 'button))
	       (parent (downcase (widget-get  button :tag))))
	  (customize-group parent)))))

(defcustom custom-mode-hook nil
  "Hook called when entering custom-mode."
  :type 'hook
  :group 'custom-buffer )

(defun custom-state-buffer-message (widget)
  (if (eq (widget-get (widget-get widget :parent) :custom-state) 'modified)
      (message
       "To install your edits, invoke [State] and choose the Set operation")))

(defun custom-mode ()
  "Major mode for editing customization buffers.

The following commands are available:

Move to next button or editable field.     \\[widget-forward]
Move to previous button or editable field. \\[widget-backward]
\\<widget-field-keymap>\
Complete content of editable text field.   \\[widget-complete]
\\<custom-mode-map>\
Invoke button under point.		   \\[widget-button-press]
Set all modifications.			   \\[Custom-set]
Make all modifications default.		   \\[Custom-save]
Reset all modified options.		   \\[Custom-reset-current]
Reset all modified or set options.	   \\[Custom-reset-saved]
Reset all options.			   \\[Custom-reset-standard]

Entry to this mode calls the value of `custom-mode-hook'
if that value is non-nil."
  (kill-all-local-variables)
  (setq major-mode 'custom-mode
	mode-name "Custom")
  (use-local-map custom-mode-map)
  (easy-menu-add Custom-mode-menu)
  (make-local-variable 'custom-options)
  (make-local-variable 'widget-documentation-face)
  (setq widget-documentation-face 'custom-documentation-face)
  (make-local-variable 'widget-button-face)
  (setq widget-button-face 'custom-button-face)
  (make-local-hook 'widget-edit-functions)
  (add-hook 'widget-edit-functions 'custom-state-buffer-message nil t)
  (run-hooks 'custom-mode-hook))


;;;###autoload
(defun custom-migrate-custom-file (new-custom-file-name)
  "Migrate custom file from home directory."
  (mapc 'custom-save-delete
	'(custom-load-themes custom-reset-variables
			     custom-set-variables
			     custom-set-faces
			     custom-reset-faces))
  (with-current-buffer (find-file-noselect custom-file)
    (save-buffer))
  (setq custom-file new-custom-file-name)
  (custom-save-all))

;;; The End.

(provide 'cus-edit)

;; cus-edit.el ends here
