;;; mule-cmds.el --- Commands for multilingual environment

;; Copyright (C) 1995,1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko

;; Keywords: mule, multilingual

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

;; Note: Some of the code here is now in code-cmds.el

;;; Code:

;;; MULE related key bindings and menus.

(require 'code-cmds)

;; Preserve the old name
(defvaralias 'mule-keymap 'coding-keymap)

(define-key mule-keymap "x" 'set-selection-coding-system)
(define-key mule-keymap "X" 'set-next-selection-coding-system)
(define-key mule-keymap "\C-\\" 'set-input-method)
;;(define-key mule-keymap "c" 'list-coding-system-briefly) ; XEmacs
(define-key mule-keymap "C" 'describe-coding-system)	 ; XEmacs
(define-key mule-keymap "r" 'toggle-display-direction)	 ; XEmacs
(define-key mule-keymap "l" 'set-language-environment)

(define-key help-map "L" 'describe-language-environment)
(define-key help-map "\C-\\" 'describe-input-method)
(define-key help-map "I" 'describe-input-method)
(define-key help-map "h" 'view-hello-file)

;; Menu for XEmacs were moved to menubar-items.el.


;; This should be a single character key binding because users use it
;; very frequently while editing multilingual text.  Now we can use
;; only two such keys: "\C-\\" and "\C-^", but the latter is not
;; convenient because it requires shifting on most keyboards.  An
;; alternative is "\C-\]" which is now bound to `abort-recursive-edit'
;; but it won't be used that frequently.
(define-key global-map "\C-\\" 'toggle-input-method)

;;; This is no good because people often type Shift-SPC
;;; meaning to type SPC.  -- rms.
;;; ;; Here's an alternative key binding for X users (Shift-SPACE).
;;; (define-key global-map [?\S- ] 'toggle-input-method)

(defun coding-system-change-eol-conversion (coding-system eol-type)
  "Return a coding system which differs from CODING-SYSTEM in eol conversion.
The returned coding system converts end-of-line by EOL-TYPE
but text as the same way as CODING-SYSTEM.
EOL-TYPE should be `lf', `crlf', `cr' or nil.
If EOL-TYPE is nil, the returned coding system detects
how end-of-line is formatted automatically while decoding.

EOL-TYPE can be specified by an symbol `unix', `dos' or `mac'.
They means `lf', `crlf', and `cr' respectively."
  (if (symbolp eol-type)
      (setq eol-type (cond ((or (eq eol-type 'unix)
				(eq eol-type 'lf))
			    'eol-lf)
			   ((or (eq eol-type 'dos)
				(eq eol-type 'crlf))
			    'eol-crlf)
			   ((or (eq eol-type 'mac)
				(eq eol-type 'cr))
			    'eol-cr)
			   (t eol-type))))
  (let ((orig-eol-type (coding-system-eol-type coding-system)))
    (if (null orig-eol-type)
	(if (not eol-type)
	    coding-system
	  (coding-system-property coding-system eol-type))
      (let ((base (coding-system-base coding-system)))
	(if (not eol-type)
	    base
	  (if (eq eol-type orig-eol-type)
	      coding-system
	    (setq orig-eol-type (coding-system-eol-type base))
	    (if (null orig-eol-type)
		(coding-system-property base eol-type))))))))

;; (defun coding-system-change-text-conversion (coding-system coding)
;;   "Return a coding system which differs from CODING-SYSTEM in text conversion.
;; The returned coding system converts text by CODING
;; but end-of-line as the same way as CODING-SYSTEM.
;; If CODING is nil, the returned coding system detects
;; how text is formatted automatically while decoding."
;;   (if (not coding)
;;       (coding-system-base coding-system)
;;     (let ((eol-type (coding-system-eol-type coding-system)))
;;       (coding-system-change-eol-conversion
;;        coding
;;        (if (numberp eol-type) (aref [unix dos mac] eol-type))))))

(defun view-hello-file ()
  "Display the HELLO file which list up many languages and characters."
  (interactive)
  ;; We have to decode the file in any environment.
  (let ((coding-system-for-read 'iso-2022-7bit))
    (find-file-read-only (expand-file-name "HELLO" data-directory))))


;;; Language support stuff.

(defvar language-info-alist nil
  "Alist of language environment definitions.
Each element looks like:
	(LANGUAGE-NAME . ((KEY . INFO) ...))
where LANGUAGE-NAME is a string, the name of the language environment,
KEY is a symbol denoting the kind of information, and
INFO is the data associated with KEY.
Meaningful values for KEY include

  documentation      value is documentation of what this language environment
			is meant for, and how to use it.
  charset	     value is a list of the character sets used by this
			language environment.
  sample-text	     value is one line of text,
			written using those character sets,
			appropriate for this language environment.
  setup-function     value is a function to call to switch to this
			language environment.
  exit-function      value is a function to call to leave this
			language environment.
  coding-system      value is a list of coding systems that are good
			for saving text written in this language environment.
			This list serves as suggestions to the user;
			in effect, as a kind of documentation.
  coding-priority    value is a list of coding systems for this language
			environment, in order of decreasing priority.
			This is used to set up the coding system priority
			list when you switch to this language environment.
  input-method       value is a default input method for this language
			environment.
  features           value is a list of features requested in this
			language environment.
  tutorial           value is a tutorial file name written in the language.")

(defun get-language-info (lang-env key)
  "Return information listed under KEY for language environment LANG-ENV.
KEY is a symbol denoting the kind of information.
For a list of useful values for KEY and their meanings,
see `language-info-alist'."
  (if (symbolp lang-env)
      (setq lang-env (symbol-name lang-env)))
  (let ((lang-slot (assoc-ignore-case lang-env language-info-alist)))
    (if lang-slot
	(cdr (assq key (cdr lang-slot))))))

(defun set-language-info (lang-env key info)
  "Modify part of the definition of language environment LANG-ENV.
Specifically, this stores the information INFO under KEY
in the definition of this language environment.
KEY is a symbol denoting the kind of information.
INFO is the value for that information.

For a list of useful values for KEY and their meanings,
see `language-info-alist'."
  (if (symbolp lang-env)
      (setq lang-env (symbol-name lang-env)))
  (let (lang-slot key-slot)
    (setq lang-slot (assoc lang-env language-info-alist))
    (if (null lang-slot)		; If no slot for the language, add it.
	(setq lang-slot (list lang-env)
	      language-info-alist (cons lang-slot language-info-alist)))
    (setq key-slot (assq key lang-slot))
    (if (null key-slot)			; If no slot for the key, add it.
	(progn
	  (setq key-slot (list key))
	  (setcdr lang-slot (cons key-slot (cdr lang-slot)))))
    (setcdr key-slot info)))

(defun set-language-info-alist (lang-env alist &optional parents)
  "Store ALIST as the definition of language environment LANG-ENV.
ALIST is an alist of KEY and INFO values.  See the documentation of
`set-language-info' for the meanings of KEY and INFO."
  (if (symbolp lang-env)
      (setq lang-env (symbol-name lang-env)))
  (let (; (describe-map describe-language-environment-map)
	; (setup-map setup-language-environment-map)
	)
    ;; (if parents
    ;;     (let ((l parents)
    ;;           map parent-symbol parent)
    ;;       (while l
    ;;         (if (symbolp (setq parent-symbol (car l)))
    ;;             (setq parent (symbol-name parent))
    ;;           (setq parent parent-symbol parent-symbol (intern parent)))
    ;;         (setq map (lookup-key describe-map (vector parent-symbol)))
    ;;         (if (not map)
    ;;             (progn
    ;;               (setq map (intern (format "describe-%s-environment-map"
    ;;                                         (downcase parent))))
    ;;               (define-prefix-command map)
    ;;               (define-key-after describe-map (vector parent-symbol)
    ;;                 (cons parent map) t)))
    ;;         (setq describe-map (symbol-value map))
    ;;         (setq map (lookup-key setup-map (vector parent-symbol)))
    ;;         (if (not map)
    ;;             (progn
    ;;               (setq map (intern (format "setup-%s-environment-map"
    ;;                                         (downcase parent))))
    ;;               (define-prefix-command map)
    ;;               (define-key-after setup-map (vector parent-symbol)
    ;;                 (cons parent map) t)))
    ;;         (setq setup-map (symbol-value map))
    ;;         (setq l (cdr l)))))

    ;; Set up menu items for this language env.
    (let ((doc (assq 'documentation alist)))
      (when doc
	;; (define-key-after describe-map (vector (intern lang-env))
	;;   (cons lang-env 'describe-specified-language-support) t)
	(when (featurep 'menubar)
	  (eval-after-load
	      "menubar-items.elc"
	    `(add-menu-button
	      '("%_Edit" "%_Multilingual (\"Mule\")"
		"%_Describe Language Support")
	      (vector ,lang-env
		      '(describe-language-environment ,lang-env)
		      t))))
	))
    ;; (define-key-after setup-map (vector (intern lang-env))
    ;;   (cons lang-env 'setup-specified-language-environment) t)
    (when (featurep 'menubar)
      (eval-after-load
	  "menubar-items.elc"
	`(add-menu-button
	  '("%_Edit" "%_Multilingual (\"Mule\")"
	    "%_Set Language Environment")
	  (vector ,lang-env
		  '(set-language-environment ,lang-env)
		  t))))

    (while alist
      (set-language-info lang-env (car (car alist)) (cdr (car alist)))
      (setq alist (cdr alist)))))

(defun read-language-name (key prompt &optional default)
  "Read a language environment name which has information for KEY.
If KEY is nil, read any language environment.
Prompt with PROMPT.  DEFAULT is the default choice of language environment.
This returns a language environment name as a string."
  (let* ((completion-ignore-case t)
	 (name (completing-read prompt
				language-info-alist
				(and key
				     (function (lambda (elm) (assq key elm))))
				t nil nil default)))
    (if (and (> (length name) 0)
	     (or (not key)
		 (get-language-info name key)))
	name)))

;;; Multilingual input methods.

(defconst leim-list-file-name "leim-list.el"
  "Name of LEIM list file.
This file contains a list of libraries of Emacs input methods (LEIM)
in the format of Lisp expression for registering each input method.
Emacs loads this file at startup time.")

(defvar leim-list-header (format
";;; %s -- list of LEIM (Library of Emacs Input Method)
;;
;; This file contains a list of LEIM (Library of Emacs Input Method)
;; in the same directory as this file.  Loading this file registers
;; the whole input methods in Emacs.
;;
;; Each entry has the form:
;;   (register-input-method
;;    INPUT-METHOD LANGUAGE-NAME ACTIVATE-FUNC
;;    TITLE DESCRIPTION
;;    ARG ...)
;; See the function `register-input-method' for the meanings of arguments.
;;
;; If this directory is included in load-path, Emacs automatically
;; loads this file at startup time.

"
				 leim-list-file-name)
  "Header to be inserted in LEIM list file.")

(defvar leim-list-entry-regexp "^(register-input-method"
  "Regexp matching head of each entry in LEIM list file.
See also the variable `leim-list-header'")

(defvar update-leim-list-functions
  '(quail-update-leim-list-file)
  "List of functions to call to update LEIM list file.
Each function is called with one arg, LEIM directory name.")

(defun update-leim-list-file (&rest dirs)
  "Update LEIM list file in directories DIRS."
  (let ((functions update-leim-list-functions))
    (while functions
      (apply (car functions) dirs)
      (setq functions (cdr functions)))))

(defvar current-input-method nil
  "The current input method for multilingual text.
If nil, that means no input method is activated now.")
(make-variable-buffer-local 'current-input-method)
(put 'current-input-method 'permanent-local t)

(defvar current-input-method-title nil
  "Title string of the current input method shown in mode line.")
(make-variable-buffer-local 'current-input-method-title)
(put 'current-input-method-title 'permanent-local t)

(defcustom default-input-method nil
  "*Default input method for multilingual text (a string).
This is the input method activated automatically by the command
`toggle-input-method' (\\[toggle-input-method])."
  :group 'mule
  :type '(choice (const nil) string))

(put 'input-method-function 'permanent-local t)

(defvar input-method-history nil
  "History list for some commands that read input methods.")
(make-variable-buffer-local 'input-method-history)
(put 'input-method-history 'permanent-local t)

(defvar inactivate-current-input-method-function nil
  "Function to call for inactivating the current input method.
Every input method should set this to an appropriate value when activated.
This function is called with no argument.

This function should never change the value of `current-input-method'.
It is set to nil by the function `inactivate-input-method'.")
(make-variable-buffer-local 'inactivate-current-input-method-function)
(put 'inactivate-current-input-method-function 'permanent-local t)

(defvar describe-current-input-method-function nil
  "Function to call for describing the current input method.
This function is called with no argument.")
(make-variable-buffer-local 'describe-current-input-method-function)
(put 'describe-current-input-method-function 'permanent-local t)

(defvar input-method-alist nil
  "Alist of input method names vs how to use them.
Each element has the form:
   (INPUT-METHOD LANGUAGE-ENV ACTIVATE-FUNC TITLE DESCRIPTION ARGS...)
See the function `register-input-method' for the meanings of the elements.")

(defun register-input-method (input-method lang-env &rest args)
  "Register INPUT-METHOD as an input method for language environment ENV.
INPUT-METHOD and LANG-ENV are symbols or strings.

The remaining arguments are:
	ACTIVATE-FUNC, TITLE, DESCRIPTION, and ARGS...
ACTIVATE-FUNC is a function to call to activate this method.
TITLE is a string to show in the mode line when this method is active.
DESCRIPTION is a string describing this method and what it is good for.
The ARGS, if any, are passed as arguments to ACTIVATE-FUNC.
All told, the arguments to ACTIVATE-FUNC are INPUT-METHOD and the ARGS.

This function is mainly used in the file \"leim-list.el\" which is
created at building time of emacs, registering all quail input methods
contained in the emacs distribution.

In case you want to register a new quail input method by yourself, be
careful to use the same input method title as given in the third
parameter of `quail-define-package' (if the values are different, the
string specified in this function takes precedence).

The commands `describe-input-method' and `list-input-methods' need
this duplicated values to show some information about input methods
without loading the affected quail packages."
  (if (symbolp lang-env)
      (setq lang-env (symbol-name lang-env)))
  (if (symbolp input-method)
      (setq input-method (symbol-name input-method)))
  (let ((info (cons lang-env args))
	(slot (assoc input-method input-method-alist)))
    (if slot
	(setcdr slot info)
      (setq slot (cons input-method info))
      (setq input-method-alist (cons slot input-method-alist)))))

(defun read-input-method-name (prompt &optional default inhibit-null)
  "Read a name of input method from a minibuffer prompting with PROMPT.
If DEFAULT is non-nil, use that as the default,
  and substitute it into PROMPT at the first `%s'.
If INHIBIT-NULL is non-nil, null input signals an error.

The return value is a string."
  (if default
      (setq prompt (format prompt default)))
  (let* ((completion-ignore-case t)
	 ;; This binding is necessary because input-method-history is
	 ;; buffer local.
	 (input-method (completing-read prompt input-method-alist
					nil t nil 'input-method-history
					default)))
    (if (and input-method (symbolp input-method))
	(setq input-method (symbol-name input-method)))
    (if (> (length input-method) 0)
	input-method
      (if inhibit-null
	  (error "No valid input method is specified")))))

(defun activate-input-method (input-method)
  "Switch to input method INPUT-METHOD for the current buffer.
If some other input method is already active, turn it off first.
If INPUT-METHOD is nil, deactivate any current input method."
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (if (and current-input-method
	   (not (string= current-input-method input-method)))
      (inactivate-input-method))
  (unless (or current-input-method (null input-method))
    (let ((slot (assoc input-method input-method-alist)))
      (if (null slot)
	  (error "Can't activate input method `%s'" input-method))
      (let ((func (nth 2 slot)))
	(if (functionp func)
	    (apply (nth 2 slot) input-method (nthcdr 5 slot))
	  (if (and (consp func) (symbolp (car func)) (symbolp (cdr func)))
	      (progn
		(require (cdr func))
		(apply (car func) input-method (nthcdr 5 slot)))
	    (error "Can't activate input method `%s'" input-method))))
      (setq current-input-method input-method)
      (setq current-input-method-title (nth 3 slot))
      (unwind-protect
	  (run-hooks 'input-method-activate-hook)
	(force-mode-line-update)))))

(defun inactivate-input-method ()
  "Turn off the current input method."
  (when current-input-method
    (if input-method-history
	(unless (string= current-input-method (car input-method-history))
	  (setq input-method-history
		(cons current-input-method
		      (delete current-input-method input-method-history))))
      (setq input-method-history (list current-input-method)))
    (unwind-protect
	(funcall inactivate-current-input-method-function)
      (unwind-protect
	  (run-hooks 'input-method-inactivate-hook)
	(setq current-input-method nil
	      current-input-method-title nil)
	(force-mode-line-update)))))

(defun set-input-method (input-method)
  "Select and activate input method INPUT-METHOD for the current buffer.
This also sets the default input method to the one you specify."
  (interactive
   (let* ((default (or (car input-method-history) default-input-method)))
     (list (read-input-method-name
	    (if default "Select input method (default %s): " "Select input method: ")
	    default t))))
  (activate-input-method input-method)
  (setq default-input-method input-method))

(defun toggle-input-method (&optional arg)
  "Turn on or off a multilingual text input method for the current buffer.

With no prefix argument, if an input method is currently activated,
turn it off.  Otherwise, activate an input method -- the one most
recently used, or the one specified in `default-input-method', or
the one read from the minibuffer.

With a prefix argument, read an input method from the minibuffer and
turn it on.

The default is to use the most recent input method specified
\(not including the currently active input method, if any)."
  (interactive "P")
  (if (and current-input-method (not arg))
      (inactivate-input-method)
    (let ((default (or (car input-method-history) default-input-method)))
      (if (and arg default (equal current-input-method default)
	       (> (length input-method-history) 1))
	  (setq default (nth 1 input-method-history)))
      (activate-input-method
       (if (or arg (not default))
	   (progn
	     (read-input-method-name
	      (if default "Input method (default %s): " "Input method: " )
	      default t))
	 default))
      (or default-input-method
	  (setq default-input-method current-input-method)))))

(defun describe-input-method (input-method)
  "Describe input method INPUT-METHOD."
  (interactive
   (list (read-input-method-name
	  "Describe input method (default, current choice): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (if (null input-method)
      (describe-current-input-method)
    (with-output-to-temp-buffer "*Help*"
      (let ((elt (assoc input-method input-method-alist)))
	(princ (format "Input method: %s (`%s' in mode line) for %s\n  %s\n"
		       input-method (nth 3 elt) (nth 1 elt) (nth 4 elt)))))))

(defun describe-current-input-method ()
  "Describe the input method currently in use."
  (if current-input-method
      (if (and (symbolp describe-current-input-method-function)
	       (fboundp describe-current-input-method-function))
	  (funcall describe-current-input-method-function)
	(message "No way to describe the current input method `%s'"
		 current-input-method)
	(ding))
    (error "No input method is activated now")))

(defun read-multilingual-string (prompt &optional initial-input input-method)
  "Read a multilingual string from minibuffer, prompting with string PROMPT.
The input method selected last time is activated in minibuffer.
If optional second arg INITIAL-INPUT is non-nil, insert it in the minibuffer
initially.
Optional 3rd argument INPUT-METHOD specifies the input method
to be activated instead of the one selected last time.  It is a symbol
or a string."
  (setq input-method
	(or input-method
	    current-input-method
	    default-input-method
	    (read-input-method-name "Input method: " nil t)))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((prev-input-method current-input-method))
    (unwind-protect
	(progn
	  (activate-input-method input-method)
	  ;; FSF Emacs
	  ;; (read-string prompt initial-input nil nil t)
	  (read-string prompt initial-input nil))
      (activate-input-method prev-input-method))))

;; Variables to control behavior of input methods.  All input methods
;; should react to these variables.

(defcustom input-method-verbose-flag 'default
  "*A flag to control extra guidance given by input methods.
The value should be nil, t, `complex-only', or `default'.

The extra guidance is done by showing list of available keys in echo
area.  When you use the input method in the minibuffer, the guidance
is shown at the bottom short window (split from the existing window).

If the value is t, extra guidance is always given, if the value is
nil, extra guidance is always suppressed.

If the value is `complex-only', only complex input methods such as
`chinese-py' and `japanese' give extra guidance.

If the value is `default', complex input methods always give extra
guidance, but simple input methods give it only when you are not in
the minibuffer.

See also the variable `input-method-highlight-flag'."
  :type '(choice (const t) (const nil) (const complex-only) (const default))
  :group 'mule)

(defcustom input-method-highlight-flag t
  "*If this flag is non-nil, input methods highlight partially-entered text.
For instance, while you are in the middle of a Quail input method sequence,
the text inserted so far is temporarily underlined.
The underlining goes away when you finish or abort the input method sequence.
See also the variable `input-method-verbose-flag'."
  :type 'boolean
  :group 'mule)

(defvar input-method-activate-hook nil
  "Normal hook run just after an input method is activated.

The variable `current-input-method' keeps the input method name
just activated.")

(defvar input-method-inactivate-hook nil
  "Normal hook run just after an input method is inactivated.

The variable `current-input-method' still keeps the input method name
just inactivated.")

(defvar input-method-after-insert-chunk-hook nil
  "Normal hook run just after an input method insert some chunk of text.")

(defvar input-method-exit-on-first-char nil
  "This flag controls a timing when an input method returns.
Usually, the input method does not return while there's a possibility
that it may find a different translation if a user types another key.
But, it this flag is non-nil, the input method returns as soon as
the current key sequence gets long enough to have some valid translation.")

(defvar input-method-use-echo-area nil
  "This flag controls how an input method shows an intermediate key sequence.
Usually, the input method inserts the intermediate key sequence,
or candidate translations corresponding to the sequence,
at point in the current buffer.
But, if this flag is non-nil, it displays them in echo area instead.")

(defvar input-method-exit-on-invalid-key nil
  "This flag controls the behavior of an input method on invalid key input.
Usually, when a user types a key which doesn't start any character
handled by the input method, the key is handled by turning off the
input method temporarily.  After that key, the input method is re-enabled.
But, if this flag is non-nil, the input method is never back on.")


(defvar set-language-environment-hook nil
  "Normal hook run after some language environment is set.

When you set some hook function here, that effect usually should not
be inherited to another language environment.  So, you had better set
another function in `exit-language-environment-hook' (which see) to
cancel the effect.")

(defvar exit-language-environment-hook nil
  "Normal hook run after exiting from some language environment.
When this hook is run, the variable `current-language-environment'
is still bound to the language environment being exited.

This hook is mainly used for canceling the effect of
`set-language-environment-hook' (which-see).")

(put 'setup-specified-language-environment 'apropos-inhibit t)

(defun setup-specified-language-environment ()
  "Switch to a specified language environment."
  (interactive)
  (let (language-name)
    (if (and (symbolp last-command-event)
	     (or (not (eq last-command-event 'Default))
		 (setq last-command-event 'English))
	     (setq language-name (symbol-name last-command-event)))
	(set-language-environment language-name)
      (error "Bogus calling sequence"))))

(defcustom current-language-environment "English"
  "The last language environment specified with `set-language-environment'.
This variable should be set only with \\[customize], which is equivalent
to using the function `set-language-environment'."
  :link '(custom-manual "(emacs)Language Environments")
  :set (lambda (symbol value) (set-language-environment value))
  :get (lambda (x)
	 (or (car-safe (assoc-ignore-case
			(if (symbolp current-language-environment)
			    (symbol-name current-language-environment)
			  current-language-environment)
			language-info-alist))
	     "English"))
  :type (cons 'choice (mapcar (lambda (lang)
				(list 'const (car lang)))
			      language-info-alist))
  :initialize 'custom-initialize-default
  :group 'mule
  :type 'string)

(defun reset-language-environment ()
  "Reset multilingual environment of Emacs to the default status.

The default status is as follows:

  The default value of `buffer-file-coding-system' is nil.
  The default coding system for process I/O is nil.
  The default value for the command `set-terminal-coding-system' is nil.
  The default value for the command `set-keyboard-coding-system' is nil.

  The order of priorities of coding categories and the coding system
  bound to each category are as follows
	coding category		coding system
	--------------------------------------------------
	iso-7			iso-2022-7bit
	no-conversion		raw-text
	utf-8			utf-8
	iso-8-1			iso-8859-1
	iso-8-2			ctext (iso-8859-1 alias)
	iso-8-designate		ctext (iso-8859-1 alias)
	iso-lock-shift		iso-2022-lock
	shift-jis		shift_jis
	big5			big5
	ucs-4			iso-10646-ucs-4
"
;; The old table (from FSF synch?) was not what we use (cf mule-coding.el),
;; and as documented iso-8-designate is inconsistent with iso-2022-8bit-ss2.
;;  The order of priorities of coding categories and the coding system
;;  bound to each category are as follows
;;	coding category		coding system
;;	--------------------------------------------------
;;	iso-8-2			iso-8859-1
;;	iso-8-1			iso-8859-1
;;	iso-7			iso-2022-7bit
;;	iso-lock-shift		iso-2022-lock
;;	iso-8-designate		iso-2022-8bit-ss2
;;	no-conversion		raw-text
;;	shift-jis		shift_jis
;;	big5			big5
;;	ucs-4			----
;;	utf-8			----
  (interactive)

  (set-coding-category-system 'iso-7		'iso-2022-7)
  (set-coding-category-system 'iso-8-1		'iso-8859-1)
  (set-coding-category-system 'iso-8-2		'ctext)
  (set-coding-category-system 'iso-lock-shift	'iso-2022-lock)
  (set-coding-category-system 'iso-8-designate	'ctext)
  (set-coding-category-system 'no-conversion	'raw-text)
  (set-coding-category-system 'shift-jis	'shift_jis)
  (set-coding-category-system 'big5		'big5)
  ;; #### Can we now assume the existence of the 10646 coding systems?
  ;; #### These lists need to be synched with the ones in mule-coding.el.
  (cond ((eq (coding-system-type (coding-category-system 'utf-8)) 'utf-8)
	 (set-coding-category-system 'ucs-4 'iso-10646-ucs-4)
	 (set-coding-category-system 'utf-8 'utf-8)
	 (set-coding-priority-list
	  '(iso-7
	    no-conversion
	    utf-8
	    iso-8-1
	    iso-8-2
	    iso-8-designate
	    iso-lock-shift
	    shift-jis
	    big5
	    ucs-4))
	 )
	(t
	 (set-coding-priority-list
	  '(iso-7
	    no-conversion
	    iso-8-1
	    iso-8-2
	    iso-8-designate
	    iso-lock-shift
	    shift-jis
	    big5))
	 ))

  ;; (update-coding-systems-internal)

  (set-default-coding-systems nil)
  ;; Don't alter the terminal and keyboard coding systems here.
  ;; The terminal still supports the same coding system
  ;; that it supported a minute ago.
;;;  (set-terminal-coding-system-internal nil)
;;;  (set-keyboard-coding-system-internal nil)

  ;; (setq nonascii-translation-table nil
  ;;       nonascii-insert-offset 0)
  )

(defun set-language-environment (language-name)
  "Set up multi-lingual environment for using LANGUAGE-NAME.
This sets the coding system priority and the default input method
and sometimes other things.  LANGUAGE-NAME should be a string
which is the name of a language environment.  For example, \"Latin-1\"
specifies the character set for the major languages of Western Europe."
  (interactive (list (read-language-name
		      nil
		      "Set language environment (default, English): ")))
  (if language-name
      (if (symbolp language-name)
	  (setq language-name (symbol-name language-name)))
    (setq language-name "English"))
  (or (assoc-ignore-case language-name language-info-alist)
      (error "Language environment not defined: %S" language-name))
  (if current-language-environment
      (let ((func (get-language-info current-language-environment
				     'exit-function)))
	(run-hooks 'exit-language-environment-hook)
	(if (fboundp func) (funcall func))))
  (let ((default-eol-type (coding-system-eol-type
			   default-buffer-file-coding-system)))
    (reset-language-environment)

    (setq current-language-environment language-name)
    (set-language-environment-coding-systems language-name default-eol-type))
  (let ((input-method (get-language-info language-name 'input-method)))
    (when input-method
      (setq default-input-method input-method)
      (if input-method-history
	  (setq input-method-history
		(cons input-method
		      (delete input-method input-method-history))))))
  ;; (let ((nonascii (get-language-info language-name 'nonascii-translation))
  ;;       (dos-table
  ;;        (if (eq window-system 'pc)
  ;;            (intern
  ;;             (concat "cp" dos-codepage "-nonascii-translation-table")))))
  ;;   (cond
  ;;    ((char-table-p nonascii)
  ;;     (setq nonascii-translation-table nonascii))
  ;;    ((and (eq window-system 'pc) (boundp dos-table))
  ;;     ;; DOS terminals' default is to use a special non-ASCII translation
  ;;     ;; table as appropriate for the installed codepage.
  ;;     (setq nonascii-translation-table (symbol-value dos-table)))
  ;;    ((charsetp nonascii)
  ;;     (setq nonascii-insert-offset (- (make-char nonascii) 128)))))

  ;; (setq charset-origin-alist
  ;;       (get-language-info language-name 'charset-origin-alist))

  ;; Unibyte setups if necessary.
  ;; (unless default-enable-multibyte-characters
  ;;   ;; Syntax and case table.
  ;;   (let ((syntax (get-language-info language-name 'unibyte-syntax)))
  ;;     (if syntax
  ;;         (let ((set-case-syntax-set-multibyte nil))
  ;;           (load syntax nil t))
  ;;       ;; No information for syntax and case.  Reset to the defaults.
  ;;       (let ((syntax-table (standard-syntax-table))
  ;;             (case-table (standard-case-table))
  ;;             (ch (if (eq window-system 'pc) 128 160)))
  ;;         (while (< ch 256)
  ;;           (modify-syntax-entry ch " " syntax-table)
  ;;           (aset case-table ch ch)
  ;;           (setq ch (1+ ch)))
  ;;         (set-char-table-extra-slot case-table 0 nil)
  ;;         (set-char-table-extra-slot case-table 1 nil)
  ;;         (set-char-table-extra-slot case-table 2 nil))
  ;;       (set-standard-case-table (standard-case-table))
  ;;       (let ((list (buffer-list)))
  ;;         (while list
  ;;           (with-current-buffer (car list)
  ;;             (set-case-table (standard-case-table)))
  ;;           (setq list (cdr list))))))
  ;;   ;; Display table and coding system for terminal.
  ;;   (let ((coding (get-language-info language-name 'unibyte-display)))
  ;;     (if coding
  ;;         (standard-display-european-internal)
  ;;       (standard-display-default (if (eq window-system 'pc) 128 160) 255)
  ;;       (aset standard-display-table 146 nil))
  ;;     (or (eq window-system 'pc)
  ;;         (set-terminal-coding-system coding))))

  (let ((required-features (get-language-info language-name 'features)))
    (while required-features
      (require (car required-features))
      (setq required-features (cdr required-features))))
  (let ((func (get-language-info language-name 'setup-function)))
    (if (fboundp func)
	(funcall func)))
  (run-hooks 'set-language-environment-hook)
  (force-mode-line-update t))

;; (defun standard-display-european-internal ()
;;   ;; Actually set up direct output of non-ASCII characters.
;;   (standard-display-8bit (if (eq window-system 'pc) 128 160) 255)
;;   ;; Unibyte Emacs on MS-DOS wants to display all 8-bit characters with
;;   ;; the native font, and codes 160 and 146 stand for something very
;;   ;; different there.
;;   (or (and (eq window-system 'pc) (not default-enable-multibyte-characters))
;;       (progn
;;         ;; Make non-line-break space display as a plain space.
;;         ;; Most X fonts do the wrong thing for code 160.
;;         (aset standard-display-table 160 [32])
;;         ;; Most Windows programs send out apostrophe's as \222.  Most X fonts
;;         ;; don't contain a character at that position.  Map it to the ASCII
;;         ;; apostrophe.
;;         (aset standard-display-table 146 [39]))))

(defun set-language-environment-coding-systems (language-name
						&optional eol-type)
  "Do various coding system setups for language environment LANGUAGE-NAME.

The optional arg EOL-TYPE specifies the eol-type of the default value
of buffer-file-coding-system set by this function.

Note that `coding-priority-list' is not reset first; thus changing language
environment allows recognition of coding systems from previously set language
environments.  (This will not work if the desired coding systems are from the
same category.  E.g., starting with a Hebrew language environment, ISO 8859-8
will be recognized.  If you shift to Russian, ISO 8859-8 will be shadowed by
ISO 8859-5, and cannot be automatically recognized without resetting the
language environment to Hebrew.  However, if you shift from Japanese to
Russian, ISO-2022-JP will continue to be automatically recognized, since
ISO-8859-5 and ISO-2022-JP are different coding categories.)"
  (let* ((priority (get-language-info language-name 'coding-priority))
	 (default-coding (car priority)))
    (if priority
	(let ((categories (mapcar 'coding-system-category priority))
	      category checked-categories)
	  (set-default-coding-systems
	   (if (memq eol-type '(lf crlf cr unix dos mac))
	       (coding-system-change-eol-conversion default-coding eol-type)
	     default-coding))
	  ;; (setq default-sendmail-coding-system default-coding)
	  (while priority
	    (unless (memq (setq category (car categories)) checked-categories)
	      (set-coding-category-system category (car priority))
	      (setq checked-categories (cons category checked-categories)))
	    (setq priority (cdr priority)
		  categories (cdr categories)))
	  (set-coding-priority-list (nreverse checked-categories))
	  ;; (update-coding-systems-internal)
	  ))))

;; Print all arguments with `princ', then print "\n".
(defsubst princ-list (&rest args)
  (while args (princ (car args)) (setq args (cdr args)))
  (princ "\n"))

(put 'describe-specified-language-support 'apropos-inhibit t)

;; Print a language specific information such as input methods,
;; charsets, and coding systems.  This function is intended to be
;; called from the menu:
;;   [menu-bar mule describe-language-environment LANGUAGE]
;; and should not run it by `M-x describe-current-input-method-function'.
(defun describe-specified-language-support ()
  "Describe how Emacs supports the specified language environment."
  (interactive)
  (let (language-name)
    (if (not (and (symbolp last-command-event)
		  (setq language-name (symbol-name last-command-event))))
	(error "Bogus calling sequence"))
    (describe-language-environment language-name)))

(defun describe-language-environment (language-name)
  "Describe how Emacs supports language environment LANGUAGE-NAME."
  (interactive
   (list (read-language-name
	  'documentation
	  "Describe language environment (default, current choice): ")))
  (if (null language-name)
      (setq language-name current-language-environment))
  (if (or (null language-name)
	  (null (get-language-info language-name 'documentation)))
      (error "No documentation for the specified language"))
  (if (symbolp language-name)
      (setq language-name (symbol-name language-name)))
  (let ((doc (get-language-info language-name 'documentation)))
    (with-output-to-temp-buffer "*Help*"
      (princ-list language-name " language environment" "\n")
      (if (stringp doc)
	  (progn
	    (princ-list doc)
	    (terpri)))
      (let ((str (get-language-info language-name 'sample-text)))
	(if (stringp str)
	    (progn
	      (princ "Sample text:\n")
	      (princ-list "  " str)
	      (terpri))))
      (let ((input-method (get-language-info language-name 'input-method))
	    (l (copy-sequence input-method-alist)))
	(princ "Input methods")
	(when input-method
	  (princ (format " (default, %s)" input-method))
	  (setq input-method (assoc input-method input-method-alist))
	  (setq l (cons input-method (delete input-method l))))
	(princ ":\n")
	(while l
	  (if (string= language-name (nth 1 (car l)))
	      (princ-list "  " (car (car l))
			  (format " (`%s' in mode line)" (nth 3 (car l)))))
	  (setq l (cdr l))))
      (terpri)
      (princ "Character sets:\n")
      (let ((l (get-language-info language-name 'charset)))
	(if (null l)
	    (princ-list "  nothing specific to " language-name)
	  (while l
	    (princ-list "  " (car l) ": "
			(charset-description (car l)))
	    (setq l (cdr l)))))
      (terpri)
      (princ "Coding systems:\n")
      (let ((l (get-language-info language-name 'coding-system)))
	(if (null l)
	    (princ-list "  nothing specific to " language-name)
	  (while l
	    (princ ; (format "  %s (`%c' in mode line):\n\t%s\n"
	     ;; In XEmacs, `coding-system-mnemonic' returns string.
	     (format "  %s (`%s' in mode line):\n\t%s\n"
			   (car l)
			   (coding-system-mnemonic (car l))
			   (coding-system-doc-string (car l))))
	    ;; (let ((aliases (coding-system-get (car l) 'alias-coding-systems)))
	    ;;   (when aliases
	    ;;     (princ "\t")
	    ;;     (princ (cons 'alias: (cdr aliases)))
	    ;;     (terpri)))
	    (setq l (cdr l))))))))

;;; Charset property

;; (defsubst get-charset-property (charset propname)
;;   "Return the value of CHARSET's PROPNAME property.
;; This is the last value stored with
;; `(put-charset-property CHARSET PROPNAME VALUE)'."
;;   (plist-get (charset-plist charset) propname))

;; (defsubst put-charset-property (charset propname value)
;;   "Store CHARSETS's PROPNAME property with value VALUE.
;; It can be retrieved with `(get-charset-property CHARSET PROPNAME)'."
;;   (set-charset-plist charset
;;                      (plist-put (charset-plist charset) propname value)))

(defvar char-code-property-table
  (make-char-table 'generic)
  "Char-table containing a property list of each character code.

See also the documentation of `get-char-code-property' and
`put-char-code-property'")
;;   (let ((plist (aref char-code-property-table char)))
(defun get-char-code-property (char propname)
  "Return the value of CHAR's PROPNAME property in `char-code-property-table'."
  (let ((plist (get-char-table char char-code-property-table)))
    (if (listp plist)
	(car (cdr (memq propname plist))))))

(defun put-char-code-property (char propname value)
  "Store CHAR's PROPNAME property with VALUE in `char-code-property-table'.
It can be retrieved with `(get-char-code-property CHAR PROPNAME)'."
  (let ((plist (get-char-table char char-code-property-table)))
    (if plist
	(let ((slot (memq propname plist)))
	  (if slot
	      (setcar (cdr slot) value)
	    (nconc plist (list propname value))))
      (put-char-table char (list propname value) char-code-property-table)
      )))


;; Pretty description of encoded string

;; Alist of ISO 2022 control code vs the corresponding mnemonic string.
;; (defvar iso-2022-control-alist
;;   '((?\x1b . "ESC")
;;     (?\x0e . "SO")
;;     (?\x0f . "SI")
;;     (?\x8e . "SS2")
;;     (?\x8f . "SS3")
;;     (?\x9b . "CSI")))

;; (defun encoded-string-description (str coding-system)
;;   "Return a pretty description of STR that is encoded by CODING-SYSTEM."
;;   (setq str (string-as-unibyte str))
;;   (let ((char (aref str 0))
;;         desc)
;;     (when (< char 128)
;;       (setq desc (or (cdr (assq char iso-2022-control-alist))
;;                      (char-to-string char)))
;;       (let ((i 1)
;;             (len (length str)))
;;         (while (< i len)
;;           (setq char (aref str i))
;;           (if (>= char 128)
;;               (setq desc nil i len)
;;             (setq desc (concat desc " "
;;                                (or (cdr (assq char iso-2022-control-alist))
;;                                    (char-to-string char)))
;;                   i (1+ i))))))
;;     (or desc
;;         (mapconcat (function (lambda (x) (format "0x%02x" x))) str " "))))

;; (defun encode-coding-char (char coding-system)
;;   "Encode CHAR by CODING-SYSTEM and return the resulting string.
;; If CODING-SYSTEM can't safely encode CHAR, return nil."
;;   (if (cmpcharp char)
;;       (setq char (car (decompose-composite-char char 'list))))
;;   (let ((str1 (char-to-string char))
;;         (str2 (make-string 2 char))
;;         (safe-charsets (and coding-system
;;                             (coding-system-get coding-system 'safe-charsets)))
;;         enc1 enc2 i1 i2)
;;     (when (or (eq safe-charsets t)
;;               (memq (char-charset char) safe-charsets))
;;       ;; We must find the encoded string of CHAR.  But, just encoding
;;       ;; CHAR will put extra control sequences (usually to designate
;;       ;; ASCII charset) at the tail if type of CODING is ISO 2022.
;;       ;; To exclude such tailing bytes, we at first encode one-char
;;       ;; string and two-char string, then check how many bytes at the
;;       ;; tail of both encoded strings are the same.
;;
;;       (setq enc1 (string-as-unibyte (encode-coding-string str1 coding-system))
;;             i1 (length enc1)
;;             enc2 (string-as-unibyte (encode-coding-string str2 coding-system))
;;             i2 (length enc2))
;;       (while (and (> i1 0) (= (aref enc1 (1- i1)) (aref enc2 (1- i2))))
;;         (setq i1 (1- i1) i2 (1- i2)))
;;
;;       ;; Now (substring enc1 i1) and (substring enc2 i2) are the same,
;;       ;; and they are the extra control sequences at the tail to
;;       ;; exclude.
;;       (substring enc2 0 i2))))


;;; mule-cmds.el ends here
