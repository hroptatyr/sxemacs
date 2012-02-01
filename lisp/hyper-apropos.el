;;; hyper-apropos.el --- Hypertext emacs lisp documentation interface.

;; Copyright (C)  1997 Free Software Foundation, Inc.
;; Copyright (C) 1994, 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 1995 Sun Microsystems.
;; Copyright (C) 1996 Ben Wing.

;; Maintainer: Jonathan Stigelman <Stig@hackvan.com>
;; Keywords: lisp, tools, help, docs, matching

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

;;  based upon emacs-apropos.el by Frank C. Guida <fcg@philabs.philips.com>
;;
;;  Rather than run apropos and print all the documentation at once,
;;  I find it easier to view a "table of contents" first, then
;;  get the details for symbols as you need them.
;;
;;  This version of apropos prints two lists of symbols matching the
;;  given regexp:  functions/macros and variables/constants.
;;
;;  The user can then do the following:
;;
;;      - add an additional regexp to narrow the search
;;      - display documentation for the current symbol
;;      - find the tag for the current symbol
;;      - show any keybindings if the current symbol is a command
;;	- invoke functions
;;	- set variables
;;
;;  An additional feature is the ability to search the current tags
;;  table, allowing you to interrogate functions not yet loaded (this
;;  isn't available with the standard package).
;;
;;  Mouse bindings and menus are provided for XEmacs.
;;
;; additions by Ben Wing <ben@xemacs.org> July 1995:
;; added support for function aliases, made programmer's apropos be the
;; default, various other hacking.
;; Massive changes by Christoph Wedler <wedler@fmi.uni-passau.de>
;; Some changes for XEmacs 20.3 by hniksic

;; #### The maintainer is supposed to be stig, but I haven't seen him
;; around for ages.  The real maintainer for the moment is Hrvoje
;; Niksic <hniksic@xemacs.org>.

;;; Code:
(require 'cus-edit)

(defgroup hyper-apropos nil
  "Hypertext emacs lisp documentation interface."
  :group 'docs
  :group 'lisp
  :group 'tools
  :group 'help
  :group 'matching)

(defcustom hyper-apropos-show-brief-docs t
  "*If non-nil, display some documentation in the \"*Hyper Apropos*\" buffer.
Setting this to nil will speed up searches."
  :type 'boolean
  :group 'hyper-apropos)
(define-obsolete-variable-alias
  'hypropos-show-brief-docs 'hyper-apropos-show-brief-docs)
;; I changed this to true because I think it's more useful this way. --ben

(defcustom hyper-apropos-programming-apropos t
  "*If non-nil, list all the functions and variables.
This will cause more output to be generated, and take a longer time.

Otherwise, only the interactive functions and user variables will be listed."
  :type 'boolean
  :group 'hyper-apropos)
(define-obsolete-variable-alias
  'hypropos-programming-apropos 'hyper-apropos-programming-apropos)

(defcustom hyper-apropos-shrink-window nil
  "*If non-nil, shrink *Hyper Help* buffer if possible."
  :type 'boolean
  :group 'hyper-apropos)
(define-obsolete-variable-alias
  'hypropos-shrink-window 'hyper-apropos-shrink-window)

(defcustom hyper-apropos-prettyprint-long-values t
  "*If non-nil, then try to beautify the printing of very long values."
  :type 'boolean
  :group 'hyper-apropos)
(define-obsolete-variable-alias
  'hypropos-prettyprint-long-values 'hyper-apropos-prettyprint-long-values)

(defgroup hyper-apropos-faces nil
  "Faces defined by hyper-apropos."
  :prefix "hyper-apropos-"
  :group 'faces)

(defface hyper-apropos-documentation
  '((((class color) (background light))
     (:foreground "darkred"))
    (((class color) (background dark))
     (:foreground "gray90")))
  "Hyper-apropos documentation."
  :group 'hyper-apropos-faces)

(defface hyper-apropos-hyperlink
  '((((class color) (background light))
     (:foreground "blue4"))
    (((class color) (background dark))
     (:foreground "lightseagreen"))
    (t
     (:bold t)))
  "Hyper-apropos hyperlinks."
  :group 'hyper-apropos-faces)

(defface hyper-apropos-major-heading '((t (:bold t)))
  "Hyper-apropos major heading."
  :group 'hyper-apropos-faces)

(defface hyper-apropos-section-heading '((t (:bold t :italic t)))
  "Hyper-apropos section heading."
  :group 'hyper-apropos-faces)

(defface hyper-apropos-heading '((t (:bold t)))
  "Hyper-apropos heading."
  :group 'hyper-apropos-faces)

(defface hyper-apropos-warning '((t (:bold t :foreground "red")))
  "Hyper-apropos warning."
  :group 'hyper-apropos-faces)

;;; Internal variables below this point

(defvar hyper-apropos-ref-buffer)
(defvar hyper-apropos-prev-wconfig)

(defvar hyper-apropos-help-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-name map 'hyper-apropos-help-map)
    ;; movement
    (define-key map " "     'scroll-up)
    (define-key map "b"     'scroll-down)
    (define-key map [delete] 'scroll-down)
    (define-key map [backspace] 'scroll-down)
    (define-key map "/"     'isearch-forward)
    (define-key map "?"     'isearch-backward)
    ;; follow links
    (define-key map [return] 'hyper-apropos-get-doc)
    (define-key map "s"     'hyper-apropos-set-variable)
    (define-key map "t"     'hyper-apropos-find-tag)
    (define-key map "l"     'hyper-apropos-last-help)
    (define-key map "c"     'hyper-apropos-customize-variable)
    (define-key map "f"     'hyper-apropos-find-function)
    (define-key map [button2] 'hyper-apropos-mouse-get-doc)
    (define-key map [button3] 'hyper-apropos-popup-menu)
    ;; for the totally hardcore...
    (define-key map "D"     'hyper-apropos-disassemble)
    ;; administrativa
    (define-key map "a"     'hyper-apropos)
    (define-key map "n"     'hyper-apropos)
    (define-key map "q"     'hyper-apropos-quit)
    map)
  "Keybindings for the *Hyper Help* buffer and the *Hyper Apropos* buffer")
(define-obsolete-variable-alias
  'hypropos-help-map 'hyper-apropos-help-map)

(defvar hyper-apropos-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'hyper-apropos-map)
    (set-keymap-parents map (list hyper-apropos-help-map))
    ;; slightly different scrolling...
    (define-key map " "     'hyper-apropos-scroll-up)
    (define-key map "b"     'hyper-apropos-scroll-down)
    (define-key map [delete] 'hyper-apropos-scroll-down)
    (define-key map [backspace] 'hyper-apropos-scroll-down)
    ;; act on the current line...
    (define-key map "w"     'hyper-apropos-where-is)
    (define-key map "i"     'hyper-apropos-invoke-fn)
;; this is already defined in the parent-keymap above, isn't it?
;;     (define-key map "s"     'hyper-apropos-set-variable)
    ;; more administrativa...
    (define-key map "P"     'hyper-apropos-toggle-programming-flag)
    (define-key map "k"     'hyper-apropos-add-keyword)
    (define-key map "e"     'hyper-apropos-eliminate-keyword)
    map)
  "Keybindings for the *Hyper Apropos* buffer.
This map inherits from `hyper-apropos-help-map.'")
(define-obsolete-variable-alias
  'hypropos-map 'hyper-apropos-map)

;;(defvar hyper-apropos-mousable-keymap
;;  (let ((map (make-sparse-keymap)))
;;    (define-key map [button2] 'hyper-apropos-mouse-get-doc)
;;    map))

(defvar hyper-apropos-mode-hook nil
  "*User function run after hyper-apropos mode initialization.  Usage:
\(add-hook 'hyper-apropos-mode-hook #'(lambda () ... your init forms ...)).")

;; ---------------------------------------------------------------------- ;;

(defconst hyper-apropos-junk-regexp
  #r"^Apropos\|^Functions\|^Variables\|^$")

(defvar hyper-apropos-currently-showing nil)	; symbol documented in
						; help buffer now
(defvar hyper-apropos-help-history nil)	; chain of symbols followed as links in
					; help buffer
(defvar hyper-apropos-face-history nil)
;;;(defvar hyper-apropos-variable-history nil)
;;;(defvar hyper-apropos-function-history nil)
(defvar hyper-apropos-regexp-history nil)
(defvar hyper-apropos-last-regexp nil)	; regex used for last apropos
(defconst hyper-apropos-apropos-buf "*Hyper Apropos*")
(defconst hyper-apropos-help-buf "*Hyper Help*")

;;;###autoload
(defun hyper-apropos (regexp toggle-apropos)
  "Display lists of functions and variables matching REGEXP
in buffer \"*Hyper Apropos*\".  If optional prefix arg is given, then the
value of `hyper-apropos-programming-apropos' is toggled for this search.
See also `hyper-apropos-mode'."
  (interactive (list (read-from-minibuffer "List symbols matching regexp: "
					   nil nil nil 'hyper-apropos-regexp-history)
		     current-prefix-arg))
  (or (memq major-mode '(hyper-apropos-mode hyper-apropos-help-mode))
      (setq hyper-apropos-prev-wconfig (current-window-configuration)))
  (if (string= "" regexp)
      (if (get-buffer hyper-apropos-apropos-buf)
	  (progn
	    (setq regexp hyper-apropos-last-regexp)
	    (if toggle-apropos
		(hyper-apropos-toggle-programming-flag)
	      (message "Using last search results")))
	(error "Be more specific..."))
    (set-buffer (get-buffer-create hyper-apropos-apropos-buf))
    (setq buffer-read-only nil)
    (erase-buffer)
    (if toggle-apropos
	(if (local-variable-p 'hyper-apropos-programming-apropos
			      (current-buffer))
	    (setq hyper-apropos-programming-apropos
		  (not hyper-apropos-programming-apropos))
	  (set (make-local-variable 'hyper-apropos-programming-apropos)
	       (not (default-value 'hyper-apropos-programming-apropos)))))
    (let ((flist (apropos-internal regexp
				   (if hyper-apropos-programming-apropos
				       #'fboundp
				     #'commandp)))
	  (vlist (apropos-internal regexp
				   (if hyper-apropos-programming-apropos
				       #'boundp
				     #'user-variable-p))))
      (insert-face (format "Apropos search for: %S\n\n" regexp)
		   'hyper-apropos-major-heading)
      (insert-face "* = command (M-x) or user-variable.\n"
		   'hyper-apropos-documentation)
      (insert-face "a = autoloaded, b = byte-compiled, i = internal, l = lambda, m = macro.\n\n"
		   'hyper-apropos-documentation)
      (insert-face "Functions and Macros:\n\n" 'hyper-apropos-major-heading)
      (hyper-apropos-grok-functions flist)
      (insert-face "\n\nVariables and Constants:\n\n"
		   'hyper-apropos-major-heading)
      (hyper-apropos-grok-variables vlist)
      (goto-char (point-min))))
  (switch-to-buffer hyper-apropos-apropos-buf)
  (hyper-apropos-mode regexp))

(defun hyper-apropos-toggle-programming-flag ()
  (interactive)
  (with-current-buffer hyper-apropos-apropos-buf
    (set (make-local-variable 'hyper-apropos-programming-apropos)
	 (not hyper-apropos-programming-apropos)))
  (message "Re-running apropos...")
  (hyper-apropos hyper-apropos-last-regexp nil))

(defun hyper-apropos-grok-functions (fns)
  (let (bind doc type)
    (dolist (fn fns)
      (setq bind (symbol-function fn)
	    type (cond ((subrp bind) ?i)
		       ((compiled-function-p bind) ?b)
		       ((consp bind) (or (cdr
					  (assq (car bind) '((autoload . ?a)
							     (lambda . ?l)
							     (macro . ?m))))
					 ??))
		       (t ?\ )))
      (insert type (if (commandp fn) "* " "  "))
      (let ((e (insert-face (format "%S" fn) 'hyper-apropos-hyperlink)))
	(set-extent-property e 'mouse-face 'highlight))
      (insert-char ?\  (let ((l (- 30 (length (format "%S" fn)))))
			 (if (natnump l) l 0)))
      (and hyper-apropos-show-brief-docs
	   (setq doc
		 ;; A symbol's function slot can point to an unbound symbol.
		 ;; In that case, `documentation' will fail.
		 (condition-case nil
		     (documentation fn)
		   (void-function "(alias for undefined function)")
		   (error "(unexpected error from `documentation')")))
	   (if  (string-match
		 "^([^\n\t )]+[\t ]*\\([^\n)]+\\)?)\\(:[\t ]*\\|\n?\\'\\)"
		 doc)
	       (setq doc (substring doc (match-end 0) (string-match "\n" doc)))
	     t)
	   (insert-face (if doc
			    (concat " - "
				    (substring doc 0 (string-match "\n" doc)))
			  " Not documented.")
			'hyper-apropos-documentation))
      (insert ?\n))))

(defun hyper-apropos-grok-variables (vars)
  (let (doc userp)
    (dolist (var vars)
      (setq userp (user-variable-p var))
      (insert (if userp " * " "   "))
      (let ((e (insert-face (format "%S" var) 'hyper-apropos-hyperlink)))
	(set-extent-property e 'mouse-face 'highlight))
      (insert-char ?\  (let ((l (- 30 (length (format "%S" var)))))
			 (if (natnump l) l 0)))
      (and hyper-apropos-show-brief-docs
	   (setq doc (documentation-property var 'variable-documentation))
	   (insert-face (if doc
			    (concat " - " (substring doc (if userp 1 0)
						     (string-match "\n" doc)))
			  " - Not documented.")
			'hyper-apropos-documentation))
      (insert ?\n))))

;; ---------------------------------------------------------------------- ;;

(defun hyper-apropos-mode (regexp)
  "Improved apropos mode for displaying Emacs documentation.  Function and
variable names are displayed in the buffer \"*Hyper Apropos*\".

Functions are preceded by a single character to indicates their types:
    a = autoloaded, b = byte-compiled, i = internal, l = lambda, m = macro.
Interactive functions are also preceded by an asterisk.
Variables are preceded by an asterisk if they are user variables.

General Commands:

	SPC	- scroll documentation or apropos window forward
	  b	- scroll documentation or apropos window backward
	  k     - eliminate all hits that don't contain keyword
	  n	- new search
	  /	- isearch-forward
	  q	- quit and restore previous window configuration

  Operations for Symbol on Current Line:

	RET	- toggle display of symbol's documentation
		  (also on button2 in xemacs)
	  w     - show the keybinding if symbol is a command
	  i	- invoke function on current line
	  s	- set value of variable on current line
	  t	- display the C or lisp source (find-tag)"
  (delete-other-windows)
  (setq mode-name "Hyper-Apropos"
	major-mode 'hyper-apropos-mode
	buffer-read-only t
	truncate-lines t
	hyper-apropos-last-regexp regexp
	modeline-buffer-identification
	(list (cons modeline-buffer-id-left-extent "Hyper Apropos: ")
	      (cons modeline-buffer-id-right-extent (concat "\"" regexp "\""))))
  (use-local-map hyper-apropos-map)
  (run-hooks 'hyper-apropos-mode-hook))

;; ---------------------------------------------------------------------- ;;

;; similar to `describe-key-briefly', copied from help.el by CW

;;;###autoload
(defun hyper-describe-key (key)
  (interactive "kDescribe key: ")
  (hyper-describe-key-briefly key t))

;;;###autoload
(defun hyper-describe-key-briefly (key &optional show)
  (interactive "kDescribe key briefly: \nP")
  (let (menup defn interm final msg)
    (setq defn (key-or-menu-binding key 'menup))
    (if (or (null defn) (integerp defn))
	(or (numberp show) (message "%s is undefined" (key-description key)))
      (cond ((stringp defn)
	     (setq interm defn
		   final (key-binding defn)))
	    ((vectorp defn)
	     (setq interm (append defn nil))
	     (while (and interm
			 (member (key-binding (vector (car interm)))
				 '(universal-argument digit-argument)))
	       (setq interm (cdr interm)))
	     (while (and interm
			 (not (setq final (key-binding (vconcat interm)))))
	       (setq interm (butlast interm)))
	     (if final
		 (setq interm (vconcat interm))
	       (setq interm defn
		     final (key-binding defn)))))
      (setq msg (format
		 "%s runs %s%s%s"
		 ;; This used to say 'This menu item' but it could also
		 ;; be a scrollbar event.  We can't distinguish at the
		 ;; moment.
		 (if menup "This item" (key-description key))
		 ;;(if (symbolp defn) defn (key-description defn))
		 (if (symbolp defn) defn (prin1-to-string defn))
		 (if final (concat ", " (key-description interm) " runs ") "")
		 (if final
		     (if (symbolp final) final (prin1-to-string final))
		   "")))
      (if (numberp show)
	  (or (not (symbolp defn))
	      (memq (symbol-function defn)
		    '(zkey-init-kbd-macro zkey-init-kbd-fn))
	      (progn (princ msg) (princ "\n")))
	(message "%s" msg)
	(if final (setq defn final))
	(if (and (or (symbolp defn) (symbolp (setq defn (car-safe defn))))
		 defn
		 show)
	    (hyper-apropos-get-doc defn t))
	(or (memq major-mode '(hyper-apropos-mode hyper-apropos-help-mode))
	  (setq hyper-apropos-prev-wconfig (current-window-configuration)))))))

;;;###autoload
(defun hyper-describe-face (symbol &optional this-ref-buffer)
  "Describe face..
See also `hyper-apropos' and `hyper-describe-function'."
  ;; #### - perhaps a prefix arg should suppress the prompt...
  (interactive
   (let (v val)
     (setq v (hyper-apropos-this-symbol))	; symbol under point
     (or (find-face v)
	 (setq v (variable-at-point)))
     (setq val (let ((enable-recursive-minibuffers t))
		 (completing-read
		  (concat (if (hyper-apropos-follow-ref-buffer current-prefix-arg)
			      "Follow face"
			    "Describe face")
			  (if v
			      (format " (default %s): " v)
			    ": "))
		  (mapcar #'(lambda (x) (list (symbol-name x)))
			  (face-list))
		  nil t nil 'hyper-apropos-face-history
		  (and v (symbol-name v)))))
     (list (intern-soft val)
	   current-prefix-arg)))
  (if (null symbol)
      (message "Sorry, nothing to describe.")
    (or (memq major-mode '(hyper-apropos-mode hyper-apropos-help-mode))
	(setq hyper-apropos-prev-wconfig (current-window-configuration)))
    (hyper-apropos-get-doc symbol t nil this-ref-buffer)))

;;;###autoload
(defun hyper-describe-variable (symbol &optional this-ref-buffer)
  "Hypertext drop-in replacement for `describe-variable'.
See also `hyper-apropos' and `hyper-describe-function'."
  ;; #### - perhaps a prefix arg should suppress the prompt...
  (interactive (list (hyper-apropos-read-variable-symbol
		      (if (hyper-apropos-follow-ref-buffer current-prefix-arg)
			  "Follow variable"
			"Describe variable"))
		     current-prefix-arg))
  (if (null symbol)
      (message "Sorry, nothing to describe.")
    (or (memq major-mode '(hyper-apropos-mode hyper-apropos-help-mode))
	(setq hyper-apropos-prev-wconfig (current-window-configuration)))
    (hyper-apropos-get-doc symbol t nil this-ref-buffer)))

;;;###autoload
(defun hyper-where-is (symbol)
  "Print message listing key sequences that invoke specified command."
  (interactive (list (hyper-apropos-read-function-symbol "Where is function")))
  (if (null symbol)
      (message "Sorry, nothing to describe.")
    (where-is symbol)))

;;;###autoload
(defun hyper-describe-function (symbol &optional this-ref-buffer)
  "Hypertext replacement for `describe-function'.  Unlike `describe-function'
in that the symbol under the cursor is the default if it is a function.
See also `hyper-apropos' and `hyper-describe-variable'."
  ;; #### - perhaps a prefix arg should suppress the prompt...
  (interactive (list (hyper-apropos-read-function-symbol
		      (if (hyper-apropos-follow-ref-buffer current-prefix-arg)
			  "Follow function"
			"Describe function"))
		     current-prefix-arg))
  (if (null symbol)
      (message "Sorry, nothing to describe.")
    (or (memq major-mode '(hyper-apropos-mode hyper-apropos-help-mode))
	(setq hyper-apropos-prev-wconfig (current-window-configuration)))
    (hyper-apropos-get-doc symbol t nil this-ref-buffer)))

;;;###autoload
(defun hyper-apropos-read-variable-symbol (prompt &optional predicate)
  "Hypertext drop-in replacement for `describe-variable'.
See also `hyper-apropos' and `hyper-describe-function'."
  ;; #### - perhaps a prefix arg should suppress the prompt...
  (or predicate (setq predicate 'boundp))
  (let (v val)
    (setq v (hyper-apropos-this-symbol))	; symbol under point
    (or (funcall predicate v)
	(setq v (variable-at-point)))
    (or (funcall predicate v)
	(setq v nil))
    (setq val (let ((enable-recursive-minibuffers t))
		(completing-read
		 (concat prompt
			 (if v
			     (format " (default %s): " v)
			   ": "))
		 obarray predicate t nil 'variable-history
		 (and v (symbol-name v)))))
    (intern-soft val)))

;;;###autoload
(define-obsolete-function-alias
  'hypropos-read-variable-symbol 'hyper-apropos-read-variable-symbol)

(defun hyper-apropos-read-function-symbol (prompt)
  "Read function symbol from minibuffer."
  (let ((fn (hyper-apropos-this-symbol))
	val)
    (or (fboundp fn)
	(setq fn (function-at-point)))
    (setq val (let ((enable-recursive-minibuffers t))
		(completing-read (if fn
				     (format "%s (default %s): " prompt fn)
				   (format "%s: " prompt))
				 obarray 'fboundp t nil
				 'function-history
				 (and fn (symbol-name fn)))))
    (intern-soft val)))

(defun hyper-apropos-last-help (arg)
  "Go back to the last symbol documented in the *Hyper Help* buffer."
  (interactive "P")
  (let ((win (get-buffer-window hyper-apropos-help-buf)))
    (or arg (setq arg (if win 1 0)))
    (cond ((= arg 0))
	  ((<= (length hyper-apropos-help-history) arg)
	   ;; go back as far as we can...
	   (setcdr (nreverse hyper-apropos-help-history) nil))
	  (t
	   (setq hyper-apropos-help-history
		 (nthcdr arg hyper-apropos-help-history))))
    (if (or win (> arg 0))
	(hyper-apropos-get-doc (car hyper-apropos-help-history) t)
      (display-buffer hyper-apropos-help-buf))))

(defun hyper-apropos-insert-face (string &optional face)
  "Insert STRING and fontify some parts with face `hyper-apropos-hyperlink'."
  (let ((beg (point)) end)
    (insert-face string (or face 'hyper-apropos-documentation))
    (setq end (point))
    (goto-char beg)
    (while (re-search-forward
	    #r"`\([-a-zA-Z0-9_][-a-zA-Z0-9_][-a-zA-Z0-9_.]+\)'"
	    end 'limit)
      (let ((e (make-extent (match-beginning 1) (match-end 1))))
	(set-extent-face e 'hyper-apropos-hyperlink)
	(set-extent-property e 'mouse-face 'highlight)))
    (goto-char beg)
    (while (re-search-forward
	    #r"M-x \([-a-zA-Z0-9_][-a-zA-Z0-9_][-a-zA-Z0-9_.]+\)"
	    end 'limit)
      (let ((e (make-extent (match-beginning 1) (match-end 1))))
	(set-extent-face e 'hyper-apropos-hyperlink)
	(set-extent-property e 'mouse-face 'highlight)))))

(defun hyper-apropos-insert-keybinding (keys string)
  (if keys
      (insert "  (" string " bound to \""
	      (mapconcat 'key-description
			 (sort* keys #'< :key #'length)
			 "\", \"")
	      "\")\n")))

(defun hyper-apropos-insert-section-heading (alias-desc &optional desc)
  (or desc (setq desc alias-desc
		 alias-desc nil))
  (if alias-desc
      (setq desc (concat alias-desc
			 (if (memq (aref desc 0)
				   '(?a ?e ?i ?o ?u))
			     ", an " ", a ")
			 desc)))
  (aset desc 0 (upcase (aref desc 0))) ; capitalize
  (goto-char (point-max))
  (newline 3) (delete-blank-lines) (newline 2)
  (hyper-apropos-insert-face desc 'hyper-apropos-section-heading))

(defun hyper-apropos-insert-value (string symbol val)
  (insert-face string 'hyper-apropos-heading)
  (insert (if (symbol-value symbol)
	      (if (or (null val) (eq val t) (integerp val))
		  (prog1
		      (symbol-value symbol)
		    (set symbol nil))
		"see below")
	    "is void")))

(defun hyper-apropos-follow-ref-buffer (this-ref-buffer)
  (and (not this-ref-buffer)
       (eq major-mode 'hyper-apropos-help-mode)
       hyper-apropos-ref-buffer
       (buffer-live-p hyper-apropos-ref-buffer)))

(defun hyper-apropos-get-alias (symbol alias-p next-symbol &optional use)
  "Return (TERMINAL-SYMBOL . ALIAS-DESC)."
  (let (aliases)
    (while (funcall alias-p symbol)
      (setq aliases (cons (if use (funcall use symbol) symbol) aliases))
      (setq symbol (funcall next-symbol symbol)))
    (cons symbol
	  (and aliases
	       (concat "an alias for `"
		       (mapconcat 'symbol-name
				  (nreverse aliases)
				  "',\nwhich is an alias for `")
		       "'")))))

(defun hyper-apropos-get-doc (&optional symbol force type this-ref-buffer)
  ;; #### - update this docstring
  "Toggle display of documentation for the symbol on the current line."
  ;; SYMBOL is the symbol to document.  FORCE, if non-nil, means to
  ;; regenerate the documentation even if it already seems to be there.  And
  ;; TYPE, if present, forces the generation of only variable documentation
  ;; or only function documentation.  Normally, if both are present, then
  ;; both will be generated.
  ;;
  ;; TYPES TO IMPLEMENT: obsolete face
  ;;
  (interactive)
  (or symbol
      (setq symbol (hyper-apropos-this-symbol)))
  (or type
      (setq type '(function variable face)))
  (if (and (eq hyper-apropos-currently-showing symbol)
	   (get-buffer hyper-apropos-help-buf)
	   (get-buffer-window hyper-apropos-help-buf)
	   (not force))
      ;; we're already displaying this help, so toggle its display.
      (delete-windows-on hyper-apropos-help-buf)
    ;; OK, we've got to refresh and display it...
    (or (eq symbol (car hyper-apropos-help-history))
	(setq hyper-apropos-help-history
	      (if (eq major-mode 'hyper-apropos-help-mode)
		  ;; if we're following a link in the help buffer, then
		  ;; record that in the help history.
		  (cons symbol hyper-apropos-help-history)
		;; otherwise clear the history because it's a new search.
		(list symbol))))
    (save-excursion
      (if (hyper-apropos-follow-ref-buffer this-ref-buffer)
	  (set-buffer hyper-apropos-ref-buffer)
	(setq hyper-apropos-ref-buffer (current-buffer)))
      (let (standard-output
	    ok beg
	    newsym symtype doc obsolete
	    (local mode-name)
	    global local-str global-str
	    font fore back undl
	    aliases alias-desc desc)
	(save-excursion
	  (set-buffer (get-buffer-create hyper-apropos-help-buf))
	  ;;(setq standard-output (current-buffer))
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (insert-face (format "`%s'" symbol) 'hyper-apropos-major-heading)
	  (insert (format " (buffer: %s, mode: %s)\n"
			  (buffer-name hyper-apropos-ref-buffer)
			  local)))
	;; function ----------------------------------------------------------
	(and (memq 'function type)
	     (fboundp symbol)
	     (progn
	       (setq ok t)
	       (setq aliases (hyper-apropos-get-alias (symbol-function symbol)
						 'symbolp
						 'symbol-function)
		     newsym (car aliases)
		     alias-desc (cdr aliases))
	       (if (eq 'macro (car-safe newsym))
		   (setq desc "macro"
			 newsym (cdr newsym))
		 (setq desc "function"))
	       (setq symtype (cond ((subrp newsym)                   'subr)
				   ((compiled-function-p newsym)     'bytecode)
				   ((eq (car-safe newsym) 'autoload) 'autoload)
				   ((eq (car-safe newsym) 'lambda)   'lambda))
		     desc (concat (if (commandp symbol) "interactive ")
				  (cdr (assq symtype
					     '((subr     . "built-in ")
					       (bytecode . "compiled Lisp ")
					       (autoload . "autoloaded Lisp ")
					       (lambda   . "Lisp "))))
				  desc
				  (case symtype
				    ((autoload) (format ",\n(autoloaded from \"%s\")"
							(nth 1 newsym)))
				    ((bytecode) (format ",\n(loaded from \"%s\")"
							(symbol-file symbol)))))
		     local (current-local-map)
		     global (current-global-map)
		     obsolete (get symbol 'byte-obsolete-info)
		     doc (or (condition-case nil
				 (documentation symbol)
			       (void-function
				"(alias for undefined function)")
			       (error "(unexpected error from `documention')"))
			     "function not documented"))
	       (save-excursion
		 (set-buffer hyper-apropos-help-buf)
		 (goto-char (point-max))
		 (setq standard-output (current-buffer))
		 (hyper-apropos-insert-section-heading alias-desc desc)
		 (insert ":\n")
		 (if local
		     (hyper-apropos-insert-keybinding
		      (where-is-internal symbol (list local) nil nil nil)
		      "locally"))
		 (hyper-apropos-insert-keybinding
		  (where-is-internal symbol (list global) nil nil nil)
		  "globally")
		 (insert "\n")
		 (if obsolete
		     (hyper-apropos-insert-face
		      (format "%s is an obsolete function; %s\n\n" symbol
			      (if (stringp (car obsolete))
				  (car obsolete)
				(format "use `%s' instead." (car obsolete))))
		      'hyper-apropos-warning))
		 (setq beg (point))
		 (insert-face "arguments: " 'hyper-apropos-heading)
		 (cond ((eq symtype 'lambda)
			(princ (or (nth 1 newsym) "()")))
		       ((eq symtype 'bytecode)
			(princ (or (compiled-function-arglist newsym)
				   "()")))
		       ((and (or (eq symtype 'subr) (eq symtype 'autoload))
			     (string-match
			      "[\n\t ]*\narguments: ?(\\([^)]*\\))\n?\\'"
			      doc))
			(insert (substring doc
					   (match-beginning 1)
					   (match-end 1)))
			(setq doc (substring doc 0 (match-beginning 0))))
		       ((and (eq symtype 'subr)
			     (string-match
			      "\[\n\t ]*([^\n\t )]+[\t ]*\\([^\n)]+\\)?)\\(:[\t ]*\\|\n?\\'\\)"
			      doc))
			(insert "("
				(if (match-end 1)
				    (substring doc
					       (match-beginning 1)
					       (match-end 1)))
				")")
			(setq doc (substring doc (match-end 0))))
		       (t (princ "[not available]")))
		 (insert "\n\n")
		 (hyper-apropos-insert-face doc)
		 (insert "\n")
		 (indent-rigidly beg (point) 2))))
	;; variable ----------------------------------------------------------
	(and (memq 'variable type)
	     (or (boundp symbol) (default-boundp symbol))
	     (progn
	       (setq ok t)
	       (setq aliases (hyper-apropos-get-alias symbol
						 'variable-alias
						 'variable-alias
						 'variable-alias)
		     newsym (car aliases)
		     alias-desc (cdr aliases))
	       (setq symtype (or (local-variable-p newsym (current-buffer))
				 (and (local-variable-p newsym
							(current-buffer) t)
				      'auto-local))
		     desc (concat (and (get newsym 'custom-type)
				       "customizable ")
				  (if (user-variable-p newsym)
				      "user variable"
				    "variable")
				  (cond ((eq symtype t) ", buffer-local")
					((eq symtype 'auto-local)
					 ", local when set")))
		     local (and (boundp newsym)
				(symbol-value newsym))
		     local-str (and (boundp newsym)
				    (prin1-to-string local))
		     global (and (eq symtype t)
				 (default-boundp newsym)
				 (default-value newsym))
		     global-str (and (eq symtype t)
				     (default-boundp newsym)
				     (prin1-to-string global))
		     obsolete (get symbol 'byte-obsolete-variable)
		     doc (or (documentation-property symbol
						     'variable-documentation)
			     "variable not documented"))
	       (save-excursion
		 (set-buffer hyper-apropos-help-buf)
		 (goto-char (point-max))
		 (setq standard-output (current-buffer))
		 (hyper-apropos-insert-section-heading alias-desc desc)
		 (when (and (user-variable-p newsym)
			    (get newsym 'custom-type))
		   (let ((e (make-extent (point-at-bol) (point))))
		     (set-extent-property e 'mouse-face 'highlight)
		     (set-extent-property e 'help-echo
					  (format "Customize %s" newsym))
		     (set-extent-property
		      e 'hyper-apropos-custom
		      `(lambda () (customize-variable (quote ,newsym))))))
		 (insert ":\n\n")
		 (setq beg (point))
		 (if obsolete
		     (hyper-apropos-insert-face
		      (format "%s is an obsolete function; %s\n\n" symbol
			      (if (stringp obsolete)
				  obsolete
				(format "use `%s' instead." obsolete)))
		      'hyper-apropos-warning))
		 ;; generally, the value of the variable is short and the
		 ;; documentation of the variable long, so it's desirable
		 ;; to see all of the value and the start of the
		 ;; documentation.  Some variables, though, have huge and
		 ;; nearly meaningless values that force you to page
		 ;; forward just to find the doc string.  That is
		 ;; undesirable.
		 (if (and (or (null local-str) (< (length local-str) 69))
			  (or (null global-str) (< (length global-str) 69)))
					; 80 cols.  docstrings assume this.
		     (progn (insert-face "value: " 'hyper-apropos-heading)
			    (insert (or local-str "is void"))
			    (if (eq symtype t)
				(progn
				  (insert "\n")
				  (insert-face "default value: " 'hyper-apropos-heading)
				  (insert (or global-str "is void"))))
			    (insert "\n\n")
			    (hyper-apropos-insert-face doc))
		   (hyper-apropos-insert-value "value: " 'local-str local)
		   (if (eq symtype t)
		       (progn
			 (insert ", ")
			 (hyper-apropos-insert-value "default-value: "
						'global-str global)))
		   (insert "\n\n")
		   (hyper-apropos-insert-face doc)
		   (if local-str
		       (progn
			 (newline 3) (delete-blank-lines) (newline 1)
			 (insert-face "value: " 'hyper-apropos-heading)
			 (if hyper-apropos-prettyprint-long-values
			     (condition-case nil
				 (cl-prettyprint local)
			       (error (insert local-str)))
			   (insert local-str))))
		   (if global-str
		       (progn
			 (newline 3) (delete-blank-lines) (newline 1)
			 (insert-face "default value: " 'hyper-apropos-heading)
			 (if hyper-apropos-prettyprint-long-values
			     (condition-case nil
				 (cl-prettyprint global)
			       (error (insert global-str)))
			   (insert global-str)))))
		 (indent-rigidly beg (point) 2))))
	;; face --------------------------------------------------------------
	(and (memq 'face type)
	     (find-face symbol)
	     (progn
	       (setq ok t)
	       (copy-face symbol 'hyper-apropos-temp-face 'global)
	       (mapcar #'(lambda (property)
			   (setq symtype (face-property-instance symbol
								 property))
			   (if symtype
			       (set-face-property 'hyper-apropos-temp-face
						  property
						  symtype)))
		       built-in-face-specifiers)
	       (setq font (cons (face-property-instance symbol 'font nil 0 t)
				(face-property-instance symbol 'font))
		     fore (cons (face-foreground-instance symbol nil 0 t)
				(face-foreground-instance symbol))
		     back (cons (face-background-instance symbol nil 0 t)
				(face-background-instance symbol))
		     undl (cons (face-underline-p symbol nil 0 t)
				(face-underline-p symbol))
		     doc  (face-doc-string symbol))
	       ;; #### - add some code here
	       (save-excursion
		 (set-buffer hyper-apropos-help-buf)
		 (setq standard-output (current-buffer))
		 (hyper-apropos-insert-section-heading
		  (concat "Face"
			  (when (get symbol 'face-defface-spec)
			    (let* ((str " (customizable)")
				   (e (make-extent 1 (length str) str)))
			      (set-extent-property e 'mouse-face 'highlight)
			      (set-extent-property e 'help-echo
						   (format "Customize %s" symbol))
			      (set-extent-property e 'unique t)
			      (set-extent-property e 'duplicable t)
			      (set-extent-property
			       e 'hyper-apropos-custom
			       `(lambda () (customize-face (quote ,symbol))))
			      str))
			  ":\n\n  "))
		 (insert-face "ABCDEFHIJKLMNOPQRSTUVWXYZ abcdefhijklmnopqrstuvwxyz 0123456789"
			      'hyper-apropos-temp-face)
		 (newline 2)
		 (insert-face "  Font: " 'hyper-apropos-heading)
		 (insert (format (if (numberp (car font)) "(%s)\n" "%s\n")
				 (and (cdr font)
				      (font-instance-name (cdr font)))))
		 (insert-face "  Foreground: " 'hyper-apropos-heading)
		 (insert (format (if (numberp (car fore)) "(%s)\n" "%s\n")
				 (and (cdr fore)
				      (color-instance-name (cdr fore)))))
		 (insert-face "  Background: " 'hyper-apropos-heading)
		 (insert (format (if (numberp (car back)) "(%s)\n" "%s\n")
				 (and (cdr back)
				      (color-instance-name (cdr back)))))
		 (insert-face "  Underline: " 'hyper-apropos-heading)
		 (insert (format (if (numberp (car undl)) "(%s)\n" "%s\n")
				 (cdr undl)))
		 (if doc
		     (progn
		       (newline)
		       (setq beg (point))
		       (insert doc)
		       (indent-rigidly beg (point) 2))))))
	;; not bound & property list -----------------------------------------
	(or ok
	    (save-excursion
	      (set-buffer hyper-apropos-help-buf)
	      (hyper-apropos-insert-section-heading
	       "symbol is not currently bound\n")))
	(if (and (setq symtype (symbol-plist symbol))
		 (or (> (length symtype) 2)
		     (not (memq 'variable-documentation symtype))))
	    (save-excursion
	      (set-buffer hyper-apropos-help-buf)
	      (goto-char (point-max))
	      (setq standard-output (current-buffer))
	      (hyper-apropos-insert-section-heading "property-list:\n\n")
	      (while symtype
		(if (memq (car symtype)
			  '(variable-documentation byte-obsolete-info))
		    (setq symtype (cdr symtype))
		  (insert-face (concat "  " (symbol-name (car symtype))
				       ": ")
			       'hyper-apropos-heading)
		  (setq symtype (cdr symtype))
		  (indent-to 32)
		  (insert (prin1-to-string (car symtype)) "\n"))
		(setq symtype (cdr symtype)))))))
    (save-excursion
      (set-buffer hyper-apropos-help-buf)
      (goto-char (point-min))
      ;; pop up window and shrink it if it's wasting space
      (if hyper-apropos-shrink-window
	  (shrink-window-if-larger-than-buffer
	   (display-buffer (current-buffer)))
	(display-buffer (current-buffer)))
      (hyper-apropos-help-mode))
    (setq hyper-apropos-currently-showing symbol)))
;;;###autoload
(define-obsolete-function-alias
  'hypropos-get-doc 'hyper-apropos-get-doc)

; -----------------------------------------------------------------------------

(defun hyper-apropos-help-mode ()
  "Major mode for hypertext XEmacs help.  In this mode, you can quickly
follow links between back and forth between the documentation strings for
different variables and functions.  Common commands:

\\{hyper-apropos-help-map}"
  (setq buffer-read-only t
	major-mode	     'hyper-apropos-help-mode
	mode-name	     "Hyper-Help")
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (use-local-map hyper-apropos-help-map))

;; ---------------------------------------------------------------------- ;;

(defun hyper-apropos-scroll-up ()
  "Scroll up the \"*Hyper Help*\" buffer if it's visible.
Otherwise, scroll the selected window up."
  (interactive)
  (let ((win (get-buffer-window hyper-apropos-help-buf))
	(owin (selected-window)))
    (if win
	(progn
	  (select-window win)
	  (condition-case nil
	       (scroll-up nil)
	      (error (goto-char (point-max))))
	  (select-window owin))
      (scroll-up nil))))

(defun hyper-apropos-scroll-down ()
  "Scroll down the \"*Hyper Help*\" buffer if it's visible.
Otherwise, scroll the selected window down."
  (interactive)
  (let ((win (get-buffer-window hyper-apropos-help-buf))
	(owin (selected-window)))
    (if win
	(progn
	  (select-window win)
	  (condition-case nil
	       (scroll-down nil)
	      (error (goto-char (point-max))))
	  (select-window owin))
      (scroll-down nil))))

;; ---------------------------------------------------------------------- ;;

(defun hyper-apropos-mouse-get-doc (event)
  "Get the documentation for the symbol the mouse is on."
  (interactive "e")
  (mouse-set-point event)
  (let ((e (extent-at (point) nil 'hyper-apropos-custom)))
    (if e
	(funcall (extent-property e 'hyper-apropos-custom))
      (save-excursion
	(let ((symbol (hyper-apropos-this-symbol)))
	  (if symbol
	      (hyper-apropos-get-doc symbol)
	    (error "Click on a symbol")))))))

;; ---------------------------------------------------------------------- ;;

(defun hyper-apropos-add-keyword (pattern)
  "Use additional keyword to narrow regexp match.
Deletes lines which don't match PATTERN."
  (interactive "sAdditional Keyword: ")
  (save-excursion
    (goto-char (point-min))
    (let (buffer-read-only)
      (keep-lines (concat pattern "\\|" hyper-apropos-junk-regexp))
      )))

(defun hyper-apropos-eliminate-keyword (pattern)
  "Use additional keyword to eliminate uninteresting matches.
Deletes lines which match PATTERN."
  (interactive "sKeyword to eliminate: ")
  (save-excursion
    (goto-char (point-min))
    (let (buffer-read-only)
      (flush-lines pattern))
      ))

;; ---------------------------------------------------------------------- ;;

(defun hyper-apropos-this-symbol ()
  (save-excursion
    (cond ((eq major-mode 'hyper-apropos-mode)
	   (beginning-of-line)
	   (if (looking-at hyper-apropos-junk-regexp)
	       nil
	     (forward-char 3)
	     (read (point-marker))))
	  ;; What's this?  This ends up in the same symbol already described.
;;	  ((and
;;	    (eq major-mode 'hyper-apropos-help-mode)
;;	    (> (point) (point-min)))
;;	   (save-excursion
;;	     (goto-char (point-min))
;;	     (hyper-apropos-this-symbol)))
	  (t
	   (let* ((st (progn
			(skip-syntax-backward "w_")
			;; !@(*$^%%# stupid backquote implementation!!!
			(skip-chars-forward "`")
			(point)))
		  (en (progn
			(skip-syntax-forward "w_")
			(skip-chars-backward ".':") ; : for Local Variables
			(point))))
	     (and (not (eq st en))
		  (intern-soft (buffer-substring st en))))))))

(defun hyper-apropos-where-is (symbol)
  "Find keybinding for symbol on current line."
  (interactive (list (hyper-apropos-this-symbol)))
  (where-is symbol))

(defun hyper-apropos-invoke-fn (fn)
  "Interactively invoke the function on the current line."
  (interactive (list (hyper-apropos-this-symbol)))
  (cond ((not (fboundp fn))
	 (error "%S is not a function" fn))
	(t (call-interactively fn))))

;;;###autoload
(defun hyper-set-variable (var val &optional this-ref-buffer)
  (interactive
   (let ((var (hyper-apropos-read-variable-symbol
	       (if (hyper-apropos-follow-ref-buffer current-prefix-arg)
		   "In ref buffer, set user option"
		 "Set user option")
	       'user-variable-p)))
     (list var (hyper-apropos-read-variable-value var) current-prefix-arg)))
  (hyper-apropos-set-variable var val this-ref-buffer))

;;;###autoload
(defun hyper-apropos-set-variable (var val &optional this-ref-buffer)
  "Interactively set the variable on the current line."
  (interactive
   (let ((var (hyper-apropos-this-symbol)))
     (or (and var (boundp var))
	 (setq var nil))
     (list var (hyper-apropos-read-variable-value var))))
  (and var
       (boundp var)
       (progn
	 (if (hyper-apropos-follow-ref-buffer this-ref-buffer)
	     (save-excursion
	       (set-buffer hyper-apropos-ref-buffer)
	       (set var val))
	   (set var val))
	 (hyper-apropos-get-doc var t '(variable) this-ref-buffer))))
;;;###autoload
(define-obsolete-function-alias
  'hypropos-set-variable 'hyper-apropos-set-variable)

(defun hyper-apropos-read-variable-value (var &optional this-ref-buffer)
  (and var
       (boundp var)
       (let ((prop (get var 'variable-interactive))
	     (print-readably t)
	     val str)
	 (hyper-apropos-get-doc var t '(variable) current-prefix-arg)
	 (if prop
	     (call-interactively (list 'lambda '(arg)
				       (list 'interactive prop)
				       'arg))
	   (setq val (if (hyper-apropos-follow-ref-buffer this-ref-buffer)
			 (save-excursion
			   (set-buffer hyper-apropos-ref-buffer)
			   (symbol-value var))
		       (symbol-value var))
		 str (prin1-to-string val))
	   (eval-minibuffer
	    (format "Set %s `%s' to value (evaluated): "
		    (if (user-variable-p var) "user option" "Variable")
		    var)
	    (condition-case nil
		(progn
		  (read str)
		  (format (if (or (consp val)
				  (and (symbolp val)
				       (not (memq val '(t nil)))))
			      "'%s" "%s")
			  str))
	      (error nil)))))))

(defun hyper-apropos-customize-variable ()
  (interactive)
  (let ((var (hyper-apropos-this-symbol)))
    (and
     (or (and var (boundp var))
	 (setq var nil))
     (customize-variable var))))

;; ---------------------------------------------------------------------- ;;

(autoload 'find-tag-other-window "etags" nil t)

(defun hyper-apropos-find-tag (&optional tag-name)
  "Find the tag for the symbol on the current line in other window.  In
order for this to work properly, the variable `tag-table-alist' or
`tags-file-name' must be set so that a TAGS file with tags for the emacs
source is found for the \"*Hyper Apropos*\" buffer."
  (interactive)
  ;; there ought to be a default tags file for this...
  (or tag-name (setq tag-name (symbol-name (hyper-apropos-this-symbol))))
  (find-tag-other-window (list tag-name)))

;; ---------------------------------------------------------------------- ;;

(defun hyper-apropos-find-function (fn)
  "Find the function for the symbol on the current line in other
window.  (See also `find-function'.)"
  (interactive
   (let ((fn (hyper-apropos-this-symbol)))
     (or (fboundp fn)
	 (setq fn nil))
     (list fn)))
  (if fn
      (declare-fboundp (find-function-other-window fn))))

;; ---------------------------------------------------------------------- ;;
(autoload 'disassemble "disass" nil t)

(defun hyper-apropos-disassemble (sym)
  "Disassemble FUN if it is byte-coded.  If it's a lambda, prettyprint it."
  (interactive (list (hyper-apropos-this-symbol)))
  (let ((fun sym) (trail nil) macrop)
    (while (and (symbolp fun) (not (memq fun trail)))
      (setq trail (cons fun trail)
	    fun (symbol-function fun)))
    (and (symbolp fun)
	 (error "Loop detected in function binding of `%s'" fun))
    (setq macrop (and  (consp fun)
		       (eq 'macro (car fun))))
    (cond ((compiled-function-p (if macrop (cdr fun) fun))
	   (disassemble fun)
	   (set-buffer "*Disassemble*")
	   (goto-char (point-min))
	   (forward-sexp 2)
	   (insert (format " for function `%S'" sym))
	   )
	  ((consp fun)
	   (with-current-buffer "*Disassemble*"
	     (cl-prettyprint (if macrop
				 (cons 'defmacro (cons sym (cdr (cdr fun))))
			       (cons 'defun (cons sym (cdr fun))))))
	   (set-buffer "*Disassemble*")
	   (emacs-lisp-mode))
	  ((or (vectorp fun) (stringp fun))
	   ;; #### - do something fancy here
	   (with-output-to-temp-buffer "*Disassemble*"
	     (princ (format "%s is a keyboard macro:\n\n\t" sym))
	     (prin1 fun)))
	  (t
	   (error "Sorry, cannot disassemble `%s'" sym)))))

;; ---------------------------------------------------------------------- ;;

(defun hyper-apropos-quit ()
  (interactive)
  "Quit Hyper Apropos and restore original window config."
  (let ((buf (get-buffer hyper-apropos-apropos-buf)))
    (and buf (bury-buffer buf)))
  (set-window-configuration hyper-apropos-prev-wconfig))

;; ---------------------------------------------------------------------- ;;

;;;###autoload
(defun hyper-apropos-popup-menu (event)
  (interactive "e")
  (mouse-set-point event)
  (let* ((sym (hyper-apropos-this-symbol))
	 (notjunk (not (null sym)))
	 (command-p (if (commandp sym) t))
	 (variable-p (and sym (boundp sym)))
	 (customizable-p (and variable-p
			      (get sym 'custom-type)
			      t))
	 (function-p (fboundp sym))
	 (apropos-p (eq 'hyper-apropos-mode
			(save-excursion (set-buffer (event-buffer event))
					major-mode)))
	 (name (if sym (symbol-name sym) ""))
	 (hyper-apropos-menu
	  (delete
	   nil
	   (list (concat "Hyper-Help: " name)
	    (vector "Display documentation" 'hyper-apropos-get-doc   notjunk)
	    (vector "Set variable"	'hyper-apropos-set-variable variable-p)
	    (vector "Customize variable" 'hyper-apropos-customize-variable
		    customizable-p)
	    (vector "Show keys for"     'hyper-apropos-where-is      command-p)
	    (vector "Invoke command"	'hyper-apropos-invoke-fn     command-p)
	    (vector "Find function"    'hyper-apropos-find-function function-p)
	    (vector "Find tag"		'hyper-apropos-find-tag	notjunk)
	    (and apropos-p
		 ["Add keyword..." hyper-apropos-add-keyword	t])
	    (and apropos-p
		 ["Eliminate keyword..." hyper-apropos-eliminate-keyword  t])
	    (if apropos-p
		["Programmers' Apropos" hyper-apropos-toggle-programming-flag
		 :style toggle :selected hyper-apropos-programming-apropos]
	      ["Programmers' Help" hyper-apropos-toggle-programming-flag
	       :style toggle :selected hyper-apropos-programming-apropos])
	    (and hyper-apropos-programming-apropos
		 (vector "Disassemble function"
			 'hyper-apropos-disassemble
			 function-p))
	    ["Help"                     describe-mode           t]
	    ["Quit"			hyper-apropos-quit		t]
	    ))))
    (popup-menu hyper-apropos-menu)))
;;;###autoload
(define-obsolete-function-alias
  'hypropos-popup-menu 'hyper-apropos-popup-menu)

(provide 'hyper-apropos)

;; end of hyper-apropos.el
