;;; custom.el -- Tools for declaring and initializing options.

;; Copyright (C) 1996, 1997 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Maintainer: Hrvoje Niksic <hniksic@xemacs.org>
;; Keywords: help, faces, dumped
;; Version: 1.9960-x
;; X-URL: http://www.dina.kvl.dk/~abraham/custom/

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file is dumped with XEmacs.

;; This file only contain the code needed to declare and initialize
;; user options.  The code to customize options is autoloaded from
;; `cus-edit.el'.
;;
;; The code implementing face declarations is in `cus-face.el'

;;; Code:

(eval-when-compile
  (load "cl-macs" nil t))

(autoload 'custom-declare-face "cus-face")
(autoload 'defun* "cl-macs")

(require 'widget)

(defvar custom-define-hook nil
  ;; Customize information for this option is in `cus-edit.el'.
  "Hook called after defining each customize option.")

;;; The `defcustom' Macro.

(defun custom-initialize-default (symbol value)
  "Initialize SYMBOL with VALUE.
This will do nothing if symbol already has a default binding.
Otherwise, if symbol has a `saved-value' property, it will evaluate
the car of that and used as the default binding for symbol.
Otherwise, VALUE will be evaluated and used as the default binding for
symbol."
  (unless (default-boundp symbol)
    ;; Use the saved value if it exists, otherwise the standard setting.
    (set-default symbol (if (get symbol 'saved-value)
                            (eval (car (get symbol 'saved-value)))
                          (eval value)))))

(defun custom-initialize-set (symbol value)
  "Initialize SYMBOL with VALUE.
Like `custom-initialize-default', but use the function specified by
`:set' to initialize SYMBOL."
  (unless (default-boundp symbol)
    (funcall (or (get symbol 'custom-set) 'set-default)
             symbol
             (if (get symbol 'saved-value)
                 (eval (car (get symbol 'saved-value)))
               (eval value)))))

(defun custom-initialize-reset (symbol value)
  "Initialize SYMBOL with VALUE.
Like `custom-initialize-set', but use the function specified by
`:get' to reinitialize SYMBOL if it is already bound."
    (funcall (or (get symbol 'custom-set) 'set-default)
             symbol
             (cond ((default-boundp symbol)
                    (funcall (or (get symbol 'custom-get) 'default-value)
                             symbol))
                   ((get symbol 'saved-value)
                    (eval (car (get symbol 'saved-value))))
                   (t
                    (eval value)))))

(defun custom-initialize-changed (symbol value)
  "Initialize SYMBOL with VALUE.
Like `custom-initialize-reset', but only use the `:set' function if the
not using the standard setting.  Otherwise, use the `set-default'."
  (cond ((default-boundp symbol)
         (funcall (or (get symbol 'custom-set) 'set-default)
                  symbol
                  (funcall (or (get symbol 'custom-get) 'default-value)
                           symbol)))
        ((get symbol 'saved-value)
         (funcall (or (get symbol 'custom-set) 'set-default)
                  symbol
                  (eval (car (get symbol 'saved-value)))))
        (t
         (set-default symbol (eval value)))))

(defun custom-declare-variable (symbol value doc &rest args)
  "Like `defcustom', but SYMBOL and VALUE are evaluated as normal arguments."
  ;; Remember the standard setting.
  (put symbol 'standard-value (list value))
  ;; Maybe this option was rogue in an earlier version.  It no longer is.
  (when (eq (get symbol 'force-value) 'rogue)
    ;; It no longer is.
    (put symbol 'force-value nil))
  (when doc
    (put symbol 'variable-documentation doc))
  (let ((initialize 'custom-initialize-reset)
        (requests nil))
    (while args
      (let ((arg (car args)))
        (setq args (cdr args))
        (check-argument-type 'keywordp arg)
        (let ((keyword arg)
              (value (car args)))
          (unless args
            (signal 'error (list "Keyword is missing an argument" keyword)))
          (setq args (cdr args))
          (cond ((eq keyword :initialize)
                 (setq initialize value))
                ((eq keyword :set)
                 (put symbol 'custom-set value))
                ((eq keyword :get)
                 (put symbol 'custom-get value))
                ((eq keyword :require)
                 (setq requests (cons value requests)))
                ((eq keyword :type)
                 (put symbol 'custom-type value))
                ((eq keyword :options)
                 (if (get symbol 'custom-options)
                     ;; Slow safe code to avoid duplicates.
                     (mapc (lambda (option)
                             (custom-add-option symbol option))
                           value)
                   ;; Fast code for the common case.
                   (put symbol 'custom-options (copy-sequence value))))
                (t
                 (custom-handle-keyword symbol keyword value
                                        'custom-variable))))))
    (put symbol 'custom-requests requests)
    ;; Do the actual initialization.
    (funcall initialize symbol value))
  ;; #### This is a rough equivalent of LOADHIST_ATTACH.  However,
  ;; LOADHIST_ATTACH also checks for `initialized'.
  (push symbol current-load-list)
  (run-hooks 'custom-define-hook)
  symbol)

(defmacro defcustom (symbol value doc &rest args)
  "Declare SYMBOL as a customizable variable that defaults to VALUE.
DOC is the variable documentation.

Neither SYMBOL nor VALUE needs to be quoted.
If SYMBOL is not already bound, initialize it to VALUE.
The remaining arguments should have the form

   [KEYWORD VALUE]...

The following KEYWORD's are defined:

:type   VALUE should be a widget type for editing the symbols value.
        The default is `sexp'.
:options VALUE should be a list of valid members of the widget type.
:group  VALUE should be a customization group.
        Add SYMBOL to that group.
:initialize VALUE should be a function used to initialize the
        variable.  It takes two arguments, the symbol and value
        given in the `defcustom' call.  The default is
        `custom-initialize-set'
:set    VALUE should be a function to set the value of the symbol.
        It takes two arguments, the symbol to set and the value to
        give it.  The default is `custom-set-default'.
:get    VALUE should be a function to extract the value of symbol.
        The function takes one argument, a symbol, and should return
        the current value for that symbol.  The default is
        `default-value'.
:require VALUE should be a feature symbol.  Each feature will be
        required after initialization, of the user have saved this
        option.
:version VALUE should be a string specifying that the variable was
        first introduced, or its default value was changed, in Emacs
        version VERSION.
:set-after VARIABLE specifies that SYMBOL should be set after VARIABLE when
	both have been customized.

Read the section about customization in the Emacs Lisp manual for more
information."
  `(custom-declare-variable (quote ,symbol) (quote ,value) ,doc ,@args))

;;; The `defface' Macro.

(defmacro defface (face spec doc &rest args)
  "Declare FACE as a customizable face that defaults to SPEC.
FACE does not need to be quoted.

Third argument DOC is the face documentation.

If FACE has been set with `custom-set-face', set the face attributes
as specified by that function, otherwise set the face attributes
according to SPEC.

The remaining arguments should have the form

   [KEYWORD VALUE]...

The following KEYWORDs are defined:

:group  VALUE should be a customization group.
        Add FACE to that group.

SPEC should be an alist of the form ((DISPLAY ATTS)...).

ATTS is a list of face attributes and their values.  The possible
attributes are defined in the variable `custom-face-attributes'.

The ATTS of the first entry in SPEC where the DISPLAY matches the
frame should take effect in that frame.  DISPLAY can either be the
symbol t, which will match all frames, or an alist of the form
\((REQ ITEM...)...)

For the DISPLAY to match a FRAME, the REQ property of the frame must
match one of the ITEM.  The following REQ are defined:

`type' (the value of `window-system')
  Should be one of `x', `mswindows', or `tty'.

`class' (the frame's color support)
  Should be one of `color', `grayscale', or `mono'.

`background' (what color is used for the background text)
  Should be one of `light' or `dark'.

Read the section about customization in the Emacs Lisp manual for more
information."
  `(custom-declare-face (quote ,face) ,spec ,doc ,@args))

;;; The `defgroup' Macro.

(defun custom-declare-group (symbol members doc &rest args)
  "Like `defgroup', but SYMBOL is evaluated as a normal argument."
  (while members
    (apply 'custom-add-to-group symbol (car members))
    (pop members))
  (put symbol 'custom-group (nconc members (get symbol 'custom-group)))
  (when doc
    (put symbol 'group-documentation doc))
  (while args
    (let ((arg (car args)))
      (setq args (cdr args))
      (check-argument-type 'keywordp arg)
      (let ((keyword arg)
            (value (car args)))
        (unless args
          (signal 'error (list "Keyword is missing an argument" keyword)))
        (setq args (cdr args))
        (cond ((eq keyword :prefix)
               (put symbol 'custom-prefix value))
              (t
               (custom-handle-keyword symbol keyword value
                                      'custom-group))))))
  (run-hooks 'custom-define-hook)
  symbol)

(defmacro defgroup (symbol members doc &rest args)
  "Declare SYMBOL as a customization group containing MEMBERS.
SYMBOL does not need to be quoted.

Third arg DOC is the group documentation.

MEMBERS should be an alist of the form ((NAME WIDGET)...) where NAME
is a symbol and WIDGET is a widget for editing that symbol.  Useful
widgets are `custom-variable' for editing variables, `custom-face' for
edit faces, and `custom-group' for editing groups.

The remaining arguments should have the form

   [KEYWORD VALUE]...

The following KEYWORD's are defined:

:group  VALUE should be a customization group.
        Add SYMBOL to that group.

Read the section about customization in the Emacs Lisp manual for more
information."
  `(custom-declare-group (quote ,symbol) ,members ,doc ,@args))

(defvar custom-group-hash-table (make-hash-table :size 300 :test 'eq)
  "Hash-table of non-empty groups.")

(defun custom-add-to-group (group option widget)
  "To existing GROUP add a new OPTION of type WIDGET.
If there already is an entry for that option, overwrite it."
  (let* ((members (get group 'custom-group))
         (old (assq option members)))
    (if old
        (setcar (cdr old) widget)
      (put group 'custom-group (nconc members (list (list option widget))))))
  (puthash group t custom-group-hash-table))

;;; Properties.

(defun custom-handle-all-keywords (symbol args type)
  "For customization option SYMBOL, handle keyword arguments ARGS.
Third argument TYPE is the custom option type."
  (while args
    (let ((arg (car args)))
      (setq args (cdr args))
      (check-argument-type 'keywordp arg)
      (let ((keyword arg)
            (value (car args)))
        (unless args
          (signal 'error (list "Keyword is missing an argument" keyword)))
        (setq args (cdr args))
        (custom-handle-keyword symbol keyword value type)))))

(defun custom-handle-keyword (symbol keyword value type)
  "For customization option SYMBOL, handle KEYWORD with VALUE.
Fourth argument TYPE is the custom option type."
  (cond ((eq keyword :group)
	 (custom-add-to-group value symbol type))
	((eq keyword :version)
	 (custom-add-version symbol value))
	((eq keyword :link)
	 (custom-add-link symbol value))
	((eq keyword :load)
	 (custom-add-load symbol value))
	((eq keyword :tag)
	 (put symbol 'custom-tag value))
 	((eq keyword :set-after)
	 (custom-add-dependencies symbol value))
	(t
	 (signal 'error (list "Unknown keyword" keyword)))))

(defun custom-add-dependencies (symbol value)
  "To the custom option SYMBOL, add dependencies specified by VALUE.
VALUE should be a list of symbols.  For each symbol in that list,
this specifies that SYMBOL should be set after the specified symbol, if
both appear in constructs like `custom-set-variables'."
  (unless (listp value)
    (error "Invalid custom dependency `%s'" value))
  (let* ((deps (get symbol 'custom-dependencies))
	 (new-deps deps))
    (while value
      (let ((dep (car value)))
	(unless (symbolp dep)
	  (error "Invalid custom dependency `%s'" dep))
	(unless (memq dep new-deps)
	  (setq new-deps (cons dep new-deps)))
	(setq value (cdr value))))
    (unless (eq deps new-deps)
      (put symbol 'custom-dependencies new-deps))))

(defun custom-add-option (symbol option)
  "To the variable SYMBOL add OPTION.

If SYMBOL is a hook variable, OPTION should be a hook member.
For other types variables, the effect is undefined."
  (let ((options (get symbol 'custom-options)))
    (unless (member option options)
      (put symbol 'custom-options (cons option options)))))

(defun custom-add-link (symbol widget)
  "To the custom option SYMBOL add the link WIDGET."
  (let ((links (get symbol 'custom-links)))
    (unless (member widget links)
      (put symbol 'custom-links (cons widget links)))))

(defun custom-add-version (symbol version)
  "To the custom option SYMBOL add the version VERSION."
  (put symbol 'custom-version version))

(defun custom-add-load (symbol load)
  "To the custom option SYMBOL add the dependency LOAD.
LOAD should be either a library file name, or a feature name."
  (puthash symbol t custom-group-hash-table)
  (let ((loads (get symbol 'custom-loads)))
    (unless (member load loads)
      (put symbol 'custom-loads (cons load loads)))))

;;; deftheme macro

(defvar custom-known-themes '(user standard)
   "Themes that have been defthemed.")

;;  #### add strings for group
;; #### during bootstrap we cannot use cl-macs stuff
(defun* custom-define-theme (theme feature &optional doc
         &key short-description immediate variable-reset-string
         variable-set-string face-set-string face-reset-string
         &allow-other-keys)
  (push theme custom-known-themes)
  (put theme 'theme-feature feature)
  (put theme 'theme-documentation doc)
  (if immediate (put theme 'theme-immediate immediate))
  (if variable-reset-string
      (put theme 'theme-variable-reset-string variable-reset-string ))
  (if variable-set-string
      (put theme 'theme-variable-set-string variable-set-string ))
  (if face-reset-string
      (put theme 'theme-face-reset-string face-reset-string ))
  (if face-set-string
      (put theme 'theme-face-set-string face-set-string ))
  (if short-description
      (put theme 'theme-short-description short-description )))

(defun custom-make-theme-feature (theme)
  (intern (concat (symbol-name theme) "-theme")))

(defmacro deftheme (theme &rest body)
  "(deftheme THEME &optional DOC &key KEYWORDS)

Define a theme labeled by SYMBOL THEME. The optional argument DOC is a
doc string describing the theme. It is optionally followed by the
following keyword arguments

:short-description DESC
      DESC is a short (one line) description of the theme. If not given DOC
      is used.
:immediate FLAG
      If FLAG is non-nil variables set in this theme are bound
      immediately when loading the theme.
:variable-set-string VARIABLE_-SET-STRING
      A string used by the UI to indicate that the value takes it
      setting from this theme. It is passed to FORMAT with the
      name of the theme a additional argument.
      If not given, a generic description is used.
:variable-reset-string VARIABLE-RESET-STRING
      As above but used in the case the variable has been forced to
      the value in this theme.
:face-set-string FACE-SET-STRING
:face-reset-string FACE-RESET-STRING
      As above but for faces."
  (let ((feature (custom-make-theme-feature theme)))
    `(custom-define-theme (quote ,theme) (quote ,feature) ,@body)))

(defsubst custom-theme-p (theme)
  "Non-nil when THEME has been defined."
  (memq theme custom-known-themes))

(defsubst custom-check-theme (theme)
  "Check whether THEME is valid and signal an error if NOT."
  (unless (custom-theme-p theme)
    (error "Unknown theme `%s'" theme)))


; #### do we need to deftheme 'user and/or 'standard here to make the
;      code in cus-edit cleaner?.

;;; Initializing.

(defun custom-push-theme (prop symbol theme mode value)
  (let ((old (get symbol prop)))
    (if (eq (car-safe (car-safe old)) theme)
        (setq old (cdr old)))
    (put symbol prop (cons (list theme mode value) old))))

(defvar custom-local-buffer nil
  "Non-nil, in a Customization buffer, means customize a specific buffer.
If this variable is non-nil, it should be a buffer,
and it means customize the local bindings of that buffer.
This variable is a permanent local, and it normally has a local binding
in every Customization buffer.")
(put 'custom-local-buffer 'permanent-local t)

(defun custom-set-variables (&rest args)
  "Initialize variables according to user preferences.
The settings are registered as theme `user'.
Each argument should be a list of the form:

  (SYMBOL VALUE [NOW [REQUEST [COMMENT]]])

The unevaluated VALUE is stored as the saved value for SYMBOL.
If NOW is present and non-nil, VALUE is also evaluated and bound as
the default value for the SYMBOL.
REQUEST is a list of features we must 'require for SYMBOL.
COMMENT is a comment string about SYMBOL."
  (apply 'custom-theme-set-variables 'user args))

(defun custom-theme-set-variables (theme &rest args)
  "Initialize variables according to settings specified by args.
Records the settings as belonging to THEME.

See `custom-set-variables' for a description of the arguments ARGS."
  (custom-check-theme theme)
  (setq args
	(sort args
	      (lambda (a1 a2)
		(let* ((sym1 (car a1))
		       (sym2 (car a2))
		       (1-then-2 (memq sym1 (get sym2 'custom-dependencies)))
		       (2-then-1 (memq sym2 (get sym1 'custom-dependencies))))
		  (cond ((and 1-then-2 2-then-1)
			 (error "Circular custom dependency between `%s' and `%s'"
				sym1 sym2))
			(1-then-2 t)
			(2-then-1 nil)
			;; Put symbols with :require last.  The macro
			;; define-minor-mode generates a defcustom
			;; with a :require and a :set, where the
			;; setter function calls the mode function.
			;; Putting symbols with :require last ensures
			;; that the mode function will see other
			;; customized values rather than default
			;; values.
			(t (nth 3 a2)))))))
  (let ((immediate (get theme 'theme-immediate)))
    (while args
      (let ((entry (car args)))
        (if (listp entry)
            (let* ((symbol (nth 0 entry))
                   (value (nth 1 entry))
                   (now (nth 2 entry))
                   (requests (nth 3 entry))
                   (comment (nth 4 entry))
                   (set (or (get symbol 'custom-set) 'custom-set-default)))
              (put symbol 'saved-value (list value))
              (custom-push-theme 'theme-value symbol theme 'set value)
              (put symbol 'saved-variable-comment comment)
	  ;; Allow for errors in the case where the setter has
	  ;; changed between versions, say, but let the user know.
	      (condition-case data
              (cond ((or now immediate)
                     ;; Rogue variable, set it now.
                     (put symbol 'force-value (if now 'rogue 'immediate))
                     (funcall set symbol (eval value)))
                    ((default-boundp symbol)
                     ;; Something already set this, overwrite it.
                     (funcall set symbol (eval value))))
	      (error 
	       (message "Error setting %s: %s" symbol data)))
              (and (or now (default-boundp symbol))
                 (put symbol 'variable-comment comment))
              (when requests
                (put symbol 'custom-requests requests)
                (mapc 'require requests))
              (setq args (cdr args)))
          ;; Old format, a plist of SYMBOL VALUE pairs.
          (message "Warning: old format `custom-set-variables'")
          (ding)
          (sit-for 2)
          (let ((symbol (nth 0 args))
                (value (nth 1 args)))
            (put symbol 'saved-value (list value))
            (custom-push-theme 'theme-value symbol theme 'set value))
          (setq args (cdr (cdr args))))))))

(defvar custom-loaded-themes nil
  "Themes in the order they are loaded.")

(defun custom-theme-loaded-p (theme)
  "Return non-nil when THEME has been loaded."
  (memq theme custom-loaded-themes))

(defun provide-theme (theme)
  "Indicate that this file provides THEME."
  (custom-check-theme theme)
  (provide (get theme 'theme-feature))
  (push theme custom-loaded-themes))

(defun require-theme (theme &optional soft)
  "Try to load a theme by requiring its feature."
  ;; Note we do no check for validity of the theme here.
  ;; This allows to pull in themes by a file-name convention
  (require (get theme 'theme-feature (custom-make-theme-feature theme))))

(defun custom-do-theme-reset (theme)
  ; #### untested! slow!
  (let (spec-list)
    (mapatoms (lambda (symbol)
                (setq spec-list (get symbol 'theme-value))
                (when spec-list
                  (setq spec-list (delete-if (lambda (elt)
                                               (eq (car elt) theme))
                                             spec-list))
                  (put symbol 'theme-value spec-list)
                  (custom-theme-reset-internal symbol 'user))
                (setq spec-list (get symbol 'theme-face))
                (when spec-list
                  (setq spec-list (delete-if (lambda (elt)
                                               (eq (car elt) theme))
                                             spec-list))
                  (put symbol 'theme-face spec-list)
                  (custom-theme-reset-internal-face symbol 'user))))))

(defun custom-theme-load-themes (by-theme &rest body)
  "Load the themes specified by BODY and record them as required by
theme BY-THEME. BODY is a sequence of
       - a SYMBOL
            require the theme SYMBOL
       - a list (reset THEME)
            Undo all the settings made by THEME.
       - a list (hidden THEME)
            require the THEME but hide it from the user."
  (custom-check-theme by-theme)
  (dolist (theme body)
    (cond ((and (consp theme) (eq (car theme) 'reset))
           (custom-do-theme-reset (cadr theme)))
          ((and (consp theme) (eq (car theme) 'hidden))
           (require-theme (cadr theme))
           (unless (custom-theme-loaded-p (cadr theme))
             (put (cadr theme) 'theme-hidden t)))
          (t
           (require-theme theme)
           (remprop theme 'theme-hidden)))
    (push theme (get by-theme 'theme-loads-themes))))

(defun custom-load-themes (&rest body)
  "Load themes for the USER theme as specified by BODY.

BODY is as with custom-theme-load-themes."
  (apply #'custom-theme-load-themes 'user body))




(defsubst copy-upto-last (elt list)
  "Copy all the elements of the list upto the last occurrence of elt."
  ;; Is it faster to do more work in C than to do less in elisp?
  (nreverse (cdr (member elt (reverse list)))))

(defun custom-theme-value (theme theme-spec-list)
  "Determine the value for THEME defined by THEME-SPEC-LIST.
Returns (list value) if found. Nil otherwise."
  ;; Note we do _NOT_ signal an error if the theme is unknown
  ;; it might have gone away without the user knowing.
  (let ((theme-or-lower (memq theme (cons 'user custom-loaded-themes)))
        value)
    (mapc #'(lambda (theme-spec)
              (when (member (car theme-spec) theme-or-lower)
                (setq value (cdr theme-spec))
                ;; We need to continue because if theme =A and we found
                ;; B then if the load order is B A C B
                ;; we actually want the value in C.
                (setq theme-or-lower (copy-upto-last (car theme-spec)
                                                     theme-or-lower))
                ;; We could should circuit if this is now nil.
                ))
          theme-spec-list)
    (if value
        (if (eq (car value) 'set)
            (list (cadr value))
          ;; Yet another reset spec. car value = reset
          (custom-theme-value (cadr value) theme-spec-list)))))


(defun custom-theme-variable-value (variable theme)
  "Return (list value) value of VARIABLE in THEME if the THEME modifies the
VARIABLE.  Nil otherwise."
  (custom-theme-value theme (get variable 'theme-value)))

(defun custom-theme-reset-internal (symbol to-theme)
  (let ((value (custom-theme-variable-value symbol to-theme))
        was-in-theme)
    (setq was-in-theme value)
    (setq value (or value (get symbol 'standard-value)))
    (when value
      (put symbol 'saved-value was-in-theme)
      (if (or (get 'force-value symbol) (default-boundp symbol))
          (funcall (get symbol 'custom-set 'set-default) symbol
                   (eval (car value)))))
    value))


(defun custom-theme-reset-variables (theme &rest args)
  "Reset the value of the variables to values previously defined.
Associate this setting with THEME.

ARGS is a list of lists of the form

    (variable to-theme)

This means reset variable to its value in to-theme."
  (custom-check-theme theme)
  (mapc #'(lambda (arg)
            (apply #'custom-theme-reset-internal arg)
            (custom-push-theme 'theme-value (car arg) theme 'reset (cadr arg)))
        args))

(defun custom-reset-variables (&rest args)
    "Reset the value of the variables to values previously defined.
Associate this setting with the `user' theme.

The ARGS are as in `custom-theme-reset-variables'."
    (apply #'custom-theme-reset-variables 'user args))

(defun custom-set-default (variable value)
  "Default :set function for a customizable variable.
Normally, this sets the default value of VARIABLE to VALUE,
but if `custom-local-buffer' is non-nil,
this sets the local binding in that buffer instead."
  (if custom-local-buffer
      (with-current-buffer custom-local-buffer
	(set variable value))
    (set-default variable value)))

;;; The End.

(provide 'custom)

;; custom.el ends here
