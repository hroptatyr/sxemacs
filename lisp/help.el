;;; help.el --- help commands for XEmacs.

;; Copyright (C) 1985, 1986, 1992-4, 1997 Free Software Foundation, Inc.
;; Copyright (C) 2001 Ben Wing.

;; Maintainer: FSF
;; Keywords: help, internal, dumped

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
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: FSF 19.30.

;;; Commentary:

;; This file is dumped with XEmacs.

;; This code implements XEmacs's on-line help system, the one invoked by
;;`M-x help-for-help'.

;; 06/11/1997 -- Converted to use char-after instead of broken
;;  following-char. -slb

;;; Code:

;; Get the macro make-help-screen when this is compiled,
;; or run interpreted, but not when the compiled code is loaded.
(eval-when-compile (require 'help-macro))

(defgroup help nil
  "Support for on-line help systems."
  :group 'emacs)

(defgroup help-appearance nil
  "Appearance of help buffers."
  :group 'help)

(defvar help-map (let ((map (make-sparse-keymap)))
                   (set-keymap-name map 'help-map)
                   (set-keymap-prompt
		    map (gettext "(Type ? for further options)"))
                   map)
  "Keymap for characters following the Help key.")

;; global-map definitions moved to keydefs.el
(fset 'help-command help-map)

(define-key help-map (vector help-char) 'help-for-help)
(define-key help-map "?" 'help-for-help)
(define-key help-map 'help 'help-for-help)
(define-key help-map '(f1) 'help-for-help)

(define-key help-map "\C-l" 'describe-copying) ; on \C-c in FSFmacs
(define-key help-map "\C-d" 'describe-distribution)
(define-key help-map "\C-w" 'describe-no-warranty)
(define-key help-map "a" 'hyper-apropos) ; 'command-apropos in FSFmacs
(define-key help-map "A" 'command-apropos)

(define-key help-map "b" 'describe-bindings)
(define-key help-map "B" 'describe-beta)
(define-key help-map "\C-p" 'describe-pointer)

(define-key help-map "C" 'customize)
(define-key help-map "c" 'describe-key-briefly)
(define-key help-map "k" 'describe-key)

(define-key help-map "d" 'describe-function)
(define-key help-map "e" 'describe-last-error)
(define-key help-map "f" 'describe-function)

(define-key help-map "F" 'xemacs-local-faq)

(define-key help-map "i" 'info)
(define-key help-map '(control i) 'Info-query)
;; FSFmacs has Info-goto-emacs-command-node on C-f, no binding
;; for Info-elisp-ref
(define-key help-map '(control c) 'Info-goto-emacs-command-node)
(define-key help-map '(control k) 'Info-goto-emacs-key-command-node)
(define-key help-map '(control f) 'Info-elisp-ref)

(define-key help-map "l" 'view-lossage)

(define-key help-map "m" 'describe-mode)

(define-key help-map "\C-n" 'view-emacs-news)
(define-key help-map "n" 'view-emacs-news)

(define-key help-map "p" 'finder-by-keyword)

;; Do this right with an autoload cookie in finder.el.
;;(autoload 'finder-by-keyword "finder"
;;  "Find packages matching a given keyword." t)

(define-key help-map "s" 'describe-syntax)

(define-key help-map "t" 'help-with-tutorial)

(define-key help-map "w" 'where-is)

(define-key help-map "v" 'describe-variable)

(if (fboundp 'view-last-error)
    (define-key help-map "e" 'view-last-error))


(define-key help-map "q" 'help-quit)

;#### This stuff was an attempt to have font locking and hyperlinks in the
;help buffer, but it doesn't really work.  Some of this stuff comes from
;FSF Emacs; but the FSF Emacs implementation is rather broken, as usual.
;What needs to happen is this:
;
; -- we probably need a "hyperlink mode" from which help-mode is derived.
; -- this means we probably need multiple inheritance of modes!
;    Thankfully this is not hard to implement; we already have the
;    ability for a keymap to have multiple parents.  However, we'd
;    have to define any multiply-inherited-from modes using a standard
;    `define-mode' construction instead of manually doing it, because
;    we don't want each guy calling `kill-all-local-variables' and
;    messing up the previous one.
; -- we need to scan the buffer ourselves (not from font-lock, because
;    the user might not have font-lock enabled) and highlight only
;    those words that are *documented* functions and variables (and
;    probably excluding words without dashes in them unless enclosed
;    in quotes, so that common words like "list" and "point" don't
;    become hyperlinks.
; -- we should *not* use font-lock keywords like below.  Instead we
;    should add the font-lock stuff ourselves during the scanning phase,
;    if font-lock is enabled in this buffer.

;(defun help-follow-reference (event extent user-data)
;  (let ((symbol (intern-soft (extent-string extent))))
;    (cond ((and symbol (fboundp symbol))
;	   (describe-function symbol))
;	  ((and symbol (boundp symbol))
;	   (describe-variable symbol))
;	  (t nil))))

;(defvar help-font-lock-keywords
;  (let ((name-char "[-+a-zA-Z0-9_*]") (sym-char "[-+a-zA-Z0-9_:*]"))
;    (list
;     ;;
;     ;; The symbol itself.
;     (list (concat "\\`\\(" name-char "+\\)\\(:\\)?")
;	   '(1 (if (match-beginning 2)
;		   'font-lock-function-name-face
;		 'font-lock-variable-name-face)
;	       nil t))
;     ;;
;     ;; Words inside `' which tend to be symbol names.
;     (list (concat "`\\(" sym-char sym-char "+\\)'")
;	   1 '(prog1
;		  'font-lock-reference-face
;		(add-list-mode-item (match-beginning 1)
;			       (match-end 1)
;			       nil
;			       'help-follow-reference))
;	   t)
;     ;;
;     ;; CLisp `:' keywords as references.
;     (list (concat "\\<:" sym-char "+\\>") 0 'font-lock-reference-face t)))
;  "Default expressions to highlight in Help mode.")

;(put 'help-mode 'font-lock-defaults '(help-font-lock-keywords))

(define-derived-mode help-mode view-major-mode "Help"
  "Major mode for viewing help text.
Entry to this mode runs the normal hook `help-mode-hook'.
Commands:
\\{help-mode-map}"
  )

(define-key help-mode-map "q" 'help-mode-quit)
(define-key help-mode-map "Q" 'help-mode-bury)
(define-key help-mode-map "f" 'find-function-at-point)
(define-key help-mode-map "d" 'describe-function-at-point)
(define-key help-mode-map "v" 'describe-variable-at-point)
(define-key help-mode-map "i" 'Info-elisp-ref)
(define-key help-mode-map "c" 'customize-variable)
(define-key help-mode-map [tab] 'help-next-symbol)
(define-key help-mode-map [(shift tab)] 'help-prev-symbol)
(define-key help-mode-map "n" 'help-next-section)
(define-key help-mode-map "p" 'help-prev-section)

(defun describe-function-at-point ()
  "Describe directly the function at point in the other window."
  (interactive)
  (let ((symb (function-at-point)))
    (when symb
      (describe-function symb))))

(defun describe-variable-at-point ()
  "Describe directly the variable at point in the other window."
  (interactive)
  (let ((symb (variable-at-point)))
    (when symb
      (describe-variable symb))))

(defun help-next-symbol ()
  "Move point to the next quoted symbol."
  (interactive)
  (search-forward "`" nil t))

(defun help-prev-symbol ()
  "Move point to the previous quoted symbol."
  (interactive)
  (search-backward "'" nil t))

(defun help-next-section ()
  "Move point to the next quoted symbol."
  (interactive)
  (search-forward-regexp "^\\w+:" nil t))

(defun help-prev-section ()
  "Move point to the previous quoted symbol."
  (interactive)
  (search-backward-regexp "^\\w+:" nil t))

(defun help-mode-bury ()
  "Bury the help buffer, possibly restoring the previous window configuration."
  (interactive)
  (help-mode-quit t))

(defun help-mode-quit (&optional bury)
  "Exit from help mode, possibly restoring the previous window configuration.
If the optional argument BURY is non-nil, the help buffer is buried,
otherwise it is killed."
  (interactive)
  (let ((buf (current-buffer)))
    (cond (help-window-config
	   (set-window-configuration help-window-config))
	  ((not (one-window-p))
	   (delete-window)))
    (if bury
	(bury-buffer buf)
      (kill-buffer buf))))

(defun help-quit ()
  (interactive)
  nil)

;; This is a grody hack of the same genotype as `advertised-undo'; if the
;; bindings of Backspace and C-h are the same, we want the menubar to claim
;; that `info' is invoked with `C-h i', not `BS i'.

(defun deprecated-help-command ()
  (interactive)
  (if (eq 'help-command (key-binding "\C-h"))
      (setq unread-command-event (character-to-event ?\C-h))
    (help-for-help)))

;;(define-key global-map 'backspace 'deprecated-help-command)

;; help-with-tutorial moved to help-nomule.el and mule-help.el.

;; used by describe-key, describe-key-briefly, insert-key-binding, etc.
(defun key-or-menu-binding (key &optional menu-flag)
  "Return the command invoked by KEY.
Like `key-binding', but handles menu events and toolbar presses correctly.
KEY is any value returned by `next-command-event'.
MENU-FLAG is a symbol that should be set to t if KEY is a menu event,
 or nil otherwise."
  (let (defn)
    (and menu-flag (set menu-flag nil))
    ;; If the key typed was really a menu selection, grab the form out
    ;; of the event object and intuit the function that would be called,
    ;; and describe that instead.
    (if (and (vectorp key) (= 1 (length key))
	     (or (misc-user-event-p (aref key 0))
		 (eq (car-safe (aref key 0)) 'menu-selection)))
	(let ((event (aref key 0)))
	  (setq defn (if (eventp event)
			 (list (event-function event) (event-object event))
		       (cdr event)))
	  (and menu-flag (set menu-flag t))
	  (when (eq (car defn) 'eval)
	    (setq defn (car (cdr defn))))
	  (when (eq (car-safe defn) 'call-interactively)
	    (setq defn (car (cdr defn))))
	  (when (and (consp defn) (null (cdr defn)))
	    (setq defn (car defn))))
      ;; else
      (setq defn (key-binding key)))
    ;; kludge: if a toolbar button was pressed on, try to find the
    ;; binding of the toolbar button.
    (if (and (eq defn 'press-toolbar-button)
	     (vectorp key)
	     (button-press-event-p (aref key (1- (length key)))))
	;; wait for the button release.  We're on shaky ground here ...
	(let ((event (next-command-event))
	      button)
	  (if (and (button-release-event-p event)
		   (event-over-toolbar-p event)
		   (eq 'release-and-activate-toolbar-button
		       (key-binding (vector event)))
		   (setq button (event-toolbar-button event)))
	      (toolbar-button-callback button)
	    ;; if anything went wrong, try returning the binding of
	    ;; the button-up event, of the original binding
	    (or (key-or-menu-binding (vector event))
		defn)))
      ;; no toolbar kludge
      defn)
    ))

(defun describe-key-briefly (key &optional insert)
  "Print the name of the function KEY invokes.  KEY is a string.
If INSERT (the prefix arg) is non-nil, insert the message in the buffer."
  (interactive "kDescribe key briefly: \nP")
  (let ((standard-output (if insert (current-buffer) t))
	defn menup)
    (setq defn (key-or-menu-binding key 'menup))
    (if (or (null defn) (integerp defn))
        (princ (format "%s is undefined" (key-description key)))
      ;; If it's a keyboard macro which trivially invokes another command,
      ;; document that instead.
      (if (or (stringp defn) (vectorp defn))
	  (setq defn (or (key-binding defn)
			 defn)))
      (let ((last-event (and (vectorp key)
			     (aref key (1- (length key))))))
	(princ (format (cond (insert
			      "%s (%s)")
			     ((or (button-press-event-p last-event)
				  (button-release-event-p last-event))
			      (gettext "%s at that spot runs the command %s"))
			     (t
			      (gettext "%s runs the command %s")))
		       ;; This used to say 'This menu item' but it
		       ;; could also be a scrollbar event.  We can't
		       ;; distinguish at the moment.
		       (if menup
			   (if insert "item" "This item")
			 (key-description key))
		       (if (symbolp defn) defn (prin1-to-string defn))))))))

;; #### this is a horrible piece of shit function that should
;; not exist.  In FSF 19.30 this function has gotten three times
;; as long and has tons and tons of dumb shit checking
;; special-display-buffer-names and such crap.  I absolutely
;; refuse to insert that Ebolification here.  I wanted to delete
;; this function entirely but Mly bitched.
;;
;; If your user-land code calls this function, rewrite it to
;; call with-displaying-help-buffer.

(defun print-help-return-message (&optional function)
  "Display or return message saying how to restore windows after help command.
Computes a message and applies the optional argument FUNCTION to it.
If FUNCTION is nil, applies `message' to it, thus printing it."
  (and (not (get-buffer-window standard-output))
       (funcall
	(or function 'message)
	(concat
         (substitute-command-keys
          (if (one-window-p t)
              (if pop-up-windows
                  (gettext "Type \\[delete-other-windows] to remove help window.")
                (gettext "Type \\[switch-to-buffer] RET to remove help window."))
   (gettext "Type \\[switch-to-buffer-other-window] RET to restore the other window.")))
         (substitute-command-keys
          (gettext "  \\[scroll-other-window] to scroll the help."))))))

(defcustom help-selects-help-window t
  "*If nil, use the \"old Emacs\" behavior for Help buffers.
This just displays the buffer in another window, rather than selecting
the window."
  :type 'boolean
  :group 'help-appearance)

(defcustom help-max-help-buffers 10
  "*Maximum help buffers to allow before they start getting killed.
If this is a positive integer, before a help buffer is displayed
by `with-displaying-help-buffer', any excess help buffers which
are not being displayed are first killed.  Otherwise, if it is
zero or nil, only one help buffer, \"*Help*\" is ever used."
  :type '(choice integer (const :tag "None" nil))
  :group 'help-appearance)

(defvar help-buffer-list nil
  "List of help buffers used by `help-register-and-maybe-prune-excess'")

(defun help-register-and-maybe-prune-excess (newbuf)
  "Register use of a help buffer and possibly kill any excess ones."
  ;; remove new buffer from list
  (setq help-buffer-list (remove newbuf help-buffer-list))
  ;; maybe kill excess help buffers
  (if (and (integerp help-max-help-buffers)
           (> (length help-buffer-list) help-max-help-buffers))
      (let ((keep-list nil)
            (num-kill (- (length help-buffer-list)
                         help-max-help-buffers)))
        (while help-buffer-list
          (let ((buf (car help-buffer-list)))
            (if (and (or (equal buf newbuf) (get-buffer buf))
                     (string-match "^*Help" buf)
                     (save-excursion (set-buffer buf)
                                     (eq major-mode 'help-mode)))
                (if (and (>= num-kill (length help-buffer-list))
                         (not (get-buffer-window buf t t)))
                    (kill-buffer buf)
                  (setq keep-list (cons buf keep-list)))))
          (setq help-buffer-list (cdr help-buffer-list)))
        (setq help-buffer-list (nreverse keep-list))))
  ;; push new buffer
  (setq help-buffer-list (cons newbuf help-buffer-list)))

(defvar help-buffer-prefix-string "Help"
  "Initial string to use in constructing help buffer names.
You should never set this directory, only let-bind it.")

(defun help-buffer-name (name)
  "Return a name for a Help buffer using string NAME for context."
  (if (and (integerp help-max-help-buffers)
           (> help-max-help-buffers 0)
           (stringp name))
      (if help-buffer-prefix-string
	  (format "*%s: %s*" help-buffer-prefix-string name)
	(format "*%s*" name))
    (format "*%s*" help-buffer-prefix-string)))

;; with-displaying-help-buffer

;; #### Should really be a macro to eliminate the requirement of
;; caller to code a lambda form in THUNK -- mrb

;; #### BEFORE you rush to make this a macro, think about backward
;; compatibility.  The right way would be to create a macro with
;; another name (which is a shame, because w-d-h-b is a perfect name
;; for a macro) that uses with-displaying-help-buffer internally.

(defcustom mode-for-help 'help-mode
  "*Mode that help buffers are put into.")

(defvar help-sticky-window nil
;; Window into which help buffers will be displayed, rather than
;; always searching for a new one.  This is INTERNAL and liable to
;; change its interface and/or name at any moment.  It should be
;; bound, not set.
)

(defvar help-window-config nil)

(make-variable-buffer-local 'help-window-config)
(put 'help-window-config 'permanent-local t)

(defun with-displaying-help-buffer (thunk &optional name)
  "Form which makes a help buffer with given NAME and evaluates BODY there.
The actual name of the buffer is generated by the function `help-buffer-name'.

Use this function for displaying help when C-h something is pressed or
in similar situations.  Do *not* use it when you are displaying a help
message and then prompting for input in the minibuffer -- this macro
usually selects the help buffer, which is not what you want in those
situations."
  (let* ((winconfig (current-window-configuration))
	 (was-one-window (one-window-p))
	 (buffer-name (help-buffer-name name))
	 (help-not-visible
	  (not (and (windows-of-buffer buffer-name) ;shortcut
		    (memq (selected-frame)
			  (mapcar 'window-frame
				  (windows-of-buffer buffer-name)))))))
    (help-register-and-maybe-prune-excess buffer-name)
    ;; if help-sticky-window is bogus or deleted, get rid of it.
    (if (and help-sticky-window (or (not (windowp help-sticky-window))
				    (not (window-live-p help-sticky-window))))
	(setq help-sticky-window nil))
    (prog1
	(let ((temp-buffer-show-function
	       (if help-sticky-window
		   #'(lambda (buffer)
		       (set-window-buffer help-sticky-window buffer))
		 temp-buffer-show-function)))
	  (with-output-to-temp-buffer buffer-name
	    (prog1 (funcall thunk)
	      (save-excursion
		(set-buffer standard-output)
		(funcall mode-for-help)))))
      (let ((helpwin (get-buffer-window buffer-name)))
	(when helpwin
	  ;; If the *Help* buffer is already displayed on this
	  ;; frame, don't override the previous configuration
	  (when help-not-visible
	    (with-current-buffer (window-buffer helpwin)
	      (setq help-window-config winconfig)))
	  (when help-selects-help-window
	    (select-window helpwin))
	  (cond ((eq helpwin (selected-window))
		 (display-message 'command
		   (substitute-command-keys "Type \\[help-mode-quit] to remove help window, \\[scroll-up] to scroll the help.")))
		(was-one-window
		 (display-message 'command
		   (substitute-command-keys "Type \\[delete-other-windows] to remove help window, \\[scroll-other-window] to scroll the help.")))
		(t
		 (display-message 'command
		   (substitute-command-keys "Type \\[switch-to-buffer-other-window] to restore the other window, \\[scroll-other-window] to scroll the help.")))))))))

(defun describe-key (key)
  "Display documentation of the function invoked by KEY.
KEY is a string, or vector of events.
When called interactively, KEY may also be a menu selection."
  (interactive "kDescribe key: ")
  (let ((defn (key-or-menu-binding key))
	(key-string (key-description key)))
    (if (or (null defn) (integerp defn))
        (message "%s is undefined" key-string)
      (with-displaying-help-buffer
       (lambda ()
	 (princ key-string)
	 (princ " runs ")
	 (if (symbolp defn)
	     (princ (format "`%s'" defn))
	   (princ defn))
	 (princ "\n\n")
	 (cond ((or (stringp defn) (vectorp defn))
		(let ((cmd (key-binding defn)))
		  (if (not cmd)
		      (princ "a keyboard macro")
		    (progn
		      (princ "a keyboard macro which runs the command ")
		      (princ cmd)
		      (princ ":\n\n")
		      (if (documentation cmd) (princ (documentation cmd)))))))
	       ((and (consp defn) (not (eq 'lambda (car-safe defn))))
		(let ((describe-function-show-arglist nil))
		  (describe-function-1 (car defn))))
	       ((symbolp defn)
		(describe-function-1 defn))
	       ((documentation defn)
		(princ (documentation defn)))
	       (t
		(princ "not documented"))))
       (format "key `%s'" key-string)))))

(defun describe-mode ()
  "Display documentation of current major mode and minor modes.
For this to work correctly for a minor mode, the mode's indicator variable
\(listed in `minor-mode-alist') must also be a function whose documentation
describes the minor mode."
  (interactive)
  (with-displaying-help-buffer
   (lambda ()
     ;; XEmacs change: print the major-mode documentation before
     ;; the minor modes.
     (princ mode-name)
     (princ " mode:\n")
     (princ (documentation major-mode))
     (princ "\n\n----\n\n")
     (let ((minor-modes minor-mode-alist))
       (while minor-modes
	 (let* ((minor-mode (car (car minor-modes)))
		(indicator (car (cdr (car minor-modes)))))
	   ;; Document a minor mode if it is listed in minor-mode-alist,
	   ;; bound locally in this buffer, non-nil, and has a function
	   ;; definition.
	   (if (and (boundp minor-mode)
		    (symbol-value minor-mode)
		    (fboundp minor-mode))
	       (let ((pretty-minor-mode minor-mode))
		 (if (string-match "-mode\\'" (symbol-name minor-mode))
		     (setq pretty-minor-mode
			   (capitalize
			    (substring (symbol-name minor-mode)
				       0 (match-beginning 0)))))
		 (while (and (consp indicator) (extentp (car indicator)))
		   (setq indicator (cdr indicator)))
		 (while (and indicator (symbolp indicator))
		   (setq indicator (symbol-value indicator)))
		 (princ (format "%s minor mode (%s):\n"
				pretty-minor-mode
				(if indicator
				    (format "indicator%s" indicator)
				  "no indicator")))
		 (princ (documentation minor-mode))
		 (princ "\n\n----\n\n"))))
	 (setq minor-modes (cdr minor-modes)))))
   (format "%s mode" mode-name)))

;; So keyboard macro definitions are documented correctly
(fset 'defining-kbd-macro (symbol-function 'start-kbd-macro))

;; view a read-only file intelligently
(defun Help-find-file (file)
  (if (fboundp 'view-file)
      (view-file file)
    (find-file-read-only file)
    (goto-char (point-min))))

(defun describe-distribution ()
  "Display info on how to obtain the latest version of XEmacs."
  (interactive)
  (Help-find-file (locate-data-file "DISTRIB")))

(defun describe-beta ()
  "Display info on how to deal with Beta versions of XEmacs."
  (interactive)
  (Help-find-file (locate-data-file "BETA")))

(defun describe-copying ()
  "Display info on how you may redistribute copies of XEmacs."
  (interactive)
  (Help-find-file (locate-data-file "COPYING")))

(defun describe-pointer ()
  "Show a list of all defined mouse buttons, and their definitions."
  (interactive)
  (describe-bindings nil t))

(defun describe-project ()
  "Display info on the GNU project."
  (interactive)
  (Help-find-file (locate-data-file "GNU")))

(defun describe-no-warranty ()
  "Display info on all the kinds of warranty XEmacs does NOT have."
  (interactive)
  (describe-copying)
  (let (case-fold-search)
    (search-forward "NO WARRANTY")
    (recenter 0)))

(defun describe-bindings (&optional prefix mouse-only-p)
  "Show a list of all defined keys, and their definitions.
The list is put in a buffer, which is displayed.
If optional first argument PREFIX is supplied, only commands
which start with that sequence of keys are described.
If optional second argument MOUSE-ONLY-P (prefix arg, interactively)
is non-nil then only the mouse bindings are displayed."
  (interactive (list nil current-prefix-arg))
  (with-displaying-help-buffer
   (lambda ()
     (describe-bindings-1 prefix mouse-only-p))
   (format "bindings for %s" major-mode)))

(defun describe-bindings-1 (&optional prefix mouse-only-p)
  (let ((heading (if mouse-only-p
            (gettext "button          binding\n------          -------\n")
            (gettext "key             binding\n---             -------\n")))
        (buffer (current-buffer))
        (minor minor-mode-map-alist)
	(extent-maps (mapcar-extents
		      'extent-keymap
		      nil (current-buffer) (point) (point) nil 'keymap))
        (local (current-local-map))
        (shadow '()))
    (set-buffer standard-output)
    (while extent-maps
      (insert "Bindings for Text Region:\n"
	      heading)
      (describe-bindings-internal
       (car extent-maps) nil shadow prefix mouse-only-p)
       (insert "\n")
       (setq shadow (cons (car extent-maps) shadow)
	     extent-maps (cdr extent-maps)))
    (while minor
      (let ((sym (car (car minor)))
            (map (cdr (car minor))))
        (if (symbol-value-in-buffer sym buffer nil)
            (progn
              (insert (format "Minor Mode Bindings for `%s':\n"
                              sym)
                      heading)
              (describe-bindings-internal map nil shadow prefix mouse-only-p)
              (insert "\n")
              (setq shadow (cons map shadow))))
        (setq minor (cdr minor))))
    (if local
        (progn
          (insert "Local Bindings:\n" heading)
          (describe-bindings-internal local nil shadow prefix mouse-only-p)
          (insert "\n")
          (setq shadow (cons local shadow))))
    (insert "Global Bindings:\n" heading)
    (describe-bindings-internal (current-global-map)
                                nil shadow prefix mouse-only-p)
    (when (and prefix function-key-map (not mouse-only-p))
      (insert "\nFunction key map translations:\n" heading)
      (describe-bindings-internal function-key-map nil nil
				  prefix mouse-only-p))
    (set-buffer buffer)
    standard-output))

(defun describe-prefix-bindings ()
  "Describe the bindings of the prefix used to reach this command.
The prefix described consists of all but the last event
of the key sequence that ran this command."
  (interactive)
  (let* ((key (this-command-keys))
	 (prefix (make-vector (1- (length key)) nil))
	 i)
    (setq i 0)
    (while (< i (length prefix))
      (aset prefix i (aref key i))
      (setq i (1+ i)))
    (with-displaying-help-buffer
     (lambda ()
       (princ "Key bindings starting with ")
       (princ (key-description prefix))
       (princ ":\n\n")
       (describe-bindings-1 prefix nil))
     (format "%s prefix" (key-description prefix)))))

;; Make C-h after a prefix, when not specifically bound,
;; run describe-prefix-bindings.
(setq prefix-help-command 'describe-prefix-bindings)

(defun describe-installation ()
  "Display a buffer showing information about this XEmacs was compiled."
  (interactive)
  (if (and (boundp 'Installation-string)
	   (stringp Installation-string))
      (with-displaying-help-buffer
       (lambda ()
	 (princ
	  (if (fboundp 'decode-coding-string)
	      (decode-coding-string Installation-string 'automatic-conversion)
	    Installation-string)))
       "Installation")
    (error "No Installation information available.")))

(defun view-emacs-news ()
  "Display info on recent changes to XEmacs."
  (interactive)
  (Help-find-file (locate-data-file "NEWS")))

(defun xemacs-www-page ()
  "Go to the XEmacs World Wide Web page."
  (interactive)
  (if (fboundp 'browse-url)
      (browse-url "http://www.xemacs.org/")
    (error "xemacs-www-page requires browse-url")))

(defun xemacs-www-faq ()
  "View the latest and greatest XEmacs FAQ using the World Wide Web."
  (interactive)
  (if (fboundp 'browse-url)
      (browse-url "http://www.xemacs.org/faq/index.html")
    (error "xemacs-www-faq requires browse-url")))

(defun xemacs-local-faq ()
  "View the local copy of the XEmacs FAQ.
If you have access to the World Wide Web, you should use `xemacs-www-faq'
instead, to ensure that you get the most up-to-date information."
  (interactive)
  (save-window-excursion
    (info)
    (Info-find-node "xemacs-faq" "Top"))
  (switch-to-buffer "*info*"))

(defun view-sample-init-el ()
  "Display the sample init.el file."
  (interactive)
  (Help-find-file (locate-data-file "sample.init.el")))

(defcustom view-lossage-key-count 100
  "*Number of keys `view-lossage' shows.
The maximum number of available keys is governed by `recent-keys-ring-size'."
  :type 'integer
  :group 'help)

(defcustom view-lossage-message-count 100
  "*Number of minibuffer messages `view-lossage' shows."
  :type 'integer
  :group 'help)

(defun print-recent-messages (n)
  "Print N most recent messages to standard-output, most recent first.
If N is nil, all messages will be printed."
  (save-excursion
    (let ((buffer (get-buffer-create " *Message-Log*"))
	  oldpoint extent)
      (goto-char (point-max buffer) buffer)
      (set-buffer standard-output)
      (while (and (not (bobp buffer))
		  (or (null n) (>= (decf n) 0)))
	(setq oldpoint (point buffer))
	(setq extent (extent-at oldpoint buffer
				'message-multiline nil 'before))
	;; If the message was multiline, move all the way to the
	;; beginning.
	(if extent
	    (goto-char (extent-start-position extent) buffer)
	  (forward-line -1 buffer))
	(insert-buffer-substring buffer (point buffer) oldpoint)))))

(defun view-lossage ()
  "Display recent input keystrokes and recent minibuffer messages.
The number of keys shown is controlled by `view-lossage-key-count'.
The number of messages shown is controlled by `view-lossage-message-count'."
  (interactive)
  (with-displaying-help-buffer
   (lambda ()
     (princ (key-description (recent-keys view-lossage-key-count)))
     (save-excursion
       (set-buffer standard-output)
       (goto-char (point-min))
       (insert "Recent keystrokes:\n\n")
       (while (progn (move-to-column 50) (not (eobp)))
	 (search-forward " " nil t)
	 (insert "\n")))
     ;; XEmacs addition: copy the messages from " *Message-Log*",
     ;; reversing their order and handling multiline messages
     ;; correctly.
     (princ "\n\n\nRecent minibuffer messages (most recent first):\n\n")
     (print-recent-messages view-lossage-message-count))
   "lossage"))

(define-function 'help 'help-for-help)

(make-help-screen help-for-help
  "A B C F I K L M N P S T V W C-c C-d C-f C-i C-k C-n C-w;  ? for more help:"
  "Type a Help option:
\(Use SPC or DEL to scroll through this text.  Type \\<help-map>\\[help-quit] to exit the Help command.)

\\[hyper-apropos]	Type a substring; it shows a hypertext list of
        functions and variables that contain that substring.
	See also the `apropos' command.
\\[command-apropos]	Type a substring; it shows a list of commands
        (interactively callable functions) that contain that substring.
\\[describe-bindings]	Table of all key bindings.
\\[describe-key-briefly]	Type a command key sequence;
        it displays the function name that sequence runs.
\\[customize]	Customize Emacs options.
\\[Info-goto-emacs-command-node]	Type a function name; it displays the Info node for that command.
\\[describe-function]	Type a function name; it shows its documentation.
\\[Info-elisp-ref]	Type a function name; it jumps to the full documentation
	in the XEmacs Lisp Programmer's Manual.
\\[xemacs-local-faq]	Local copy of the XEmacs FAQ.
\\[info]	Info documentation reader.
\\[Info-query]	Type an Info file name; it displays it in Info reader.
\\[describe-key]	Type a command key sequence;
        it displays the documentation for the command bound to that key.
\\[Info-goto-emacs-key-command-node]	Type a command key sequence;
        it displays the Info node for the command bound to that key.
\\[view-lossage]	Recent input keystrokes and minibuffer messages.
\\[describe-mode]	Documentation of current major and minor modes.
\\[view-emacs-news]	News of recent XEmacs changes.
\\[finder-by-keyword]	Type a topic keyword; it finds matching packages.
\\[describe-pointer]	Table of all mouse-button bindings.
\\[describe-syntax]	Contents of syntax table with explanations.
\\[help-with-tutorial]	XEmacs learn-by-doing tutorial.
\\[describe-variable]	Type a variable name; it displays its documentation and value.
\\[where-is]	Type a command name; it displays which keystrokes invoke that command.
\\[describe-distribution]	XEmacs ordering information.
\\[describe-no-warranty]	Information on absence of warranty for XEmacs.
\\[describe-copying]	XEmacs copying permission (General Public License)."
  help-map)

(defmacro with-syntax-table (syntab &rest body)
  "Evaluate BODY with the SYNTAB as the current syntax table."
  `(let ((stab (syntax-table)))
     (unwind-protect
	 (progn
	   (set-syntax-table (copy-syntax-table ,syntab))
	   ,@body)
       (set-syntax-table stab))))
(put 'with-syntax-table 'lisp-indent-function 1)
(put 'with-syntax-table 'edebug-form-spec '(form body))

(defun function-called-at-point ()
  "Return the function which is called by the list containing point.
If that gives no function, return the function whose name is around point.
If that doesn't give a function, return nil."
  (or (ignore-errors
	(save-excursion
	  (save-restriction
	    (narrow-to-region (max (point-min) (- (point) 1000))
			      (point-max))
	    (backward-up-list 1)
	    (forward-char 1)
	    (let (obj)
	      (setq obj (read (current-buffer)))
	      (and (symbolp obj) (fboundp obj) obj)))))
      (ignore-errors
	(with-syntax-table emacs-lisp-mode-syntax-table
	  (save-excursion
	    (or (not (zerop (skip-syntax-backward "_w")))
		(eq (char-syntax (char-after (point))) ?w)
		(eq (char-syntax (char-after (point))) ?_)
		(forward-sexp -1))
	    (skip-chars-forward "`'")
	    (let ((obj (read (current-buffer))))
	      (and (symbolp obj) (fboundp obj) obj)))))))

(defun function-at-point ()
  "Return the function whose name is around point.
If that gives no function, return the function which is called by the
list containing point.  If that doesn't give a function, return nil."
  (or (ignore-errors
	(with-syntax-table emacs-lisp-mode-syntax-table
	  (save-excursion
	    (or (not (zerop (skip-syntax-backward "_w")))
		(eq (char-syntax (char-after (point))) ?w)
		(eq (char-syntax (char-after (point))) ?_)
		(forward-sexp -1))
	    (skip-chars-forward "`'")
	    (let ((obj (read (current-buffer))))
	      (and (symbolp obj) (fboundp obj) obj)))))
      (ignore-errors
	(save-excursion
	  (save-restriction
	    (narrow-to-region (max (point-min) (- (point) 1000))
			      (point-max))
	    (backward-up-list 1)
	    (forward-char 1)
	    (let (obj)
	      (setq obj (read (current-buffer)))
	      (and (symbolp obj) (fboundp obj) obj)))))))

(defun function-at-event (event)
  "Return the function whose name is around the position of EVENT.
EVENT should be a mouse event.  When calling from a popup or context menu,
use `last-popup-menu-event' to find out where the mouse was clicked.
\(You cannot use (interactive \"e\"), unfortunately.  This returns a
misc-user event.)

If the event contains no position, or the position is not over text, or
there is no function around that point, nil is returned."
  (if (and event (event-buffer event) (event-point event))
      (save-excursion
	(set-buffer (event-buffer event))
	(goto-char (event-point event))
	(function-at-point))))

;; Default to nil for the non-hackers?  Not until we find a way to
;; distinguish hackers from non-hackers automatically!
(defcustom describe-function-show-arglist t
  "*If non-nil, describe-function will show its arglist,
unless the function is autoloaded."
  :type 'boolean
  :group 'help-appearance)

(defun describe-symbol-find-file (symbol)
  (loop for (file . load-data) in load-history
    do (when (memq symbol load-data)
	 (return file))))

(define-obsolete-function-alias
  'describe-function-find-file
  'describe-symbol-find-file)

(defun describe-function (function)
  "Display the full documentation of FUNCTION (a symbol).
When run interactively, it defaults to any function found by
`function-at-point'."
  (interactive
    (let* ((fn (function-at-point))
           (val (let ((enable-recursive-minibuffers t))
                  (completing-read
                    (if fn
                        (format (gettext "Describe function (default %s): ")
				fn)
                        (gettext "Describe function: "))
                    obarray 'fboundp t nil 'function-history
		    (symbol-name fn)))))
      (list (intern val))))
  (with-displaying-help-buffer
   (lambda ()
     (describe-function-1 function)
     ;; Return the text we displayed.
     (buffer-string nil nil standard-output))
    (format "function `%s'" function)))

(defun function-obsolete-p (function)
  "Return non-nil if FUNCTION is obsolete."
  (not (null (get function 'byte-obsolete-info))))

(defun function-obsoleteness-doc (function)
  "If FUNCTION is obsolete, return a string describing this."
  (let ((obsolete (get function 'byte-obsolete-info)))
    (if obsolete
	(format "Obsolete; %s"
		(if (stringp (car obsolete))
		    (car obsolete)
		  (format "use `%s' instead." (car obsolete)))))))

(defun function-compatible-p (function)
  "Return non-nil if FUNCTION is present for Emacs compatibility."
  (not (null (get function 'byte-compatible-info))))

(defun function-compatibility-doc (function)
  "If FUNCTION is Emacs compatible, return a string describing this."
  (let ((compatible (get function 'byte-compatible-info)))
    (if compatible
	(format "Emacs Compatible; %s"
		(if (stringp (car compatible))
		    (car compatible)
		  (format "use `%s' instead." (car compatible)))))))

;Here are all the possibilities below spelled out, for the benefit
;of the I18N3 snarfer.
;
;(gettext "a built-in function")
;(gettext "an interactive built-in function")
;(gettext "a built-in macro")
;(gettext "an interactive built-in macro")
;(gettext "a compiled Lisp function")
;(gettext "an interactive compiled Lisp function")
;(gettext "a compiled Lisp macro")
;(gettext "an interactive compiled Lisp macro")
;(gettext "a Lisp function")
;(gettext "an interactive Lisp function")
;(gettext "a Lisp macro")
;(gettext "an interactive Lisp macro")
;(gettext "a mocklisp function")
;(gettext "an interactive mocklisp function")
;(gettext "a mocklisp macro")
;(gettext "an interactive mocklisp macro")
;(gettext "an autoloaded Lisp function")
;(gettext "an interactive autoloaded Lisp function")
;(gettext "an autoloaded Lisp macro")
;(gettext "an interactive autoloaded Lisp macro")

;; taken out of `describe-function-1'
(defun function-arglist (function)
  "Return a string giving the argument list of FUNCTION.
For example:

	(function-arglist 'function-arglist)
	=> (function-arglist FUNCTION)

This function is used by `describe-function-1' to list function
arguments in the standard Lisp style."
  (let* ((fnc (indirect-function function))
	 (fndef (if (eq (car-safe fnc) 'macro)
		    (cdr fnc)
		  fnc))
	 (arglist
	  (cond ((compiled-function-p fndef)
		 (compiled-function-arglist fndef))
		((eq (car-safe fndef) 'lambda)
		 (nth 1 fndef))
		((subrp fndef)
		 (let* ((doc (documentation function))
			(args (and (string-match
				    "[\n\t ]*\narguments: ?(\\(.*\\))\n?\\'"
				    doc)
				   (match-string 1 doc))))
		   ;; If there are no arguments documented for the
		   ;; subr, rather don't print anything.
		   (cond ((null args) t)
			 ((equal args "") nil)
			 (args))))
		(t t))))
    (cond ((listp arglist)
	   (prin1-to-string
	    (cons function (mapcar (lambda (arg)
				     (if (memq arg '(&optional &rest))
					 arg
				       (intern (upcase (symbol-name arg)))))
				   arglist))
	    t))
	  ((stringp arglist)
	   (format "(%s %s)" function arglist)))))

(defun function-documentation (function &optional strip-arglist)
  "Return a string giving the documentation for FUNCTION, if any.
If the optional argument STRIP-ARGLIST is non-nil, remove the arglist
part of the documentation of internal subroutines."
  (let ((doc (condition-case nil
		 (or (documentation function)
		     (gettext "not documented"))
	       (void-function "(alias for undefined function)")
	       (error "(unexpected error from `documention')"))))
    (if (and strip-arglist
	     (string-match "[\n\t ]*\narguments: ?(\\(.*\\))\n?\\'" doc))
	(setq doc (substring doc 0 (match-beginning 0))))
    doc))

;; replacement for `princ' that puts the text in the specified face,
;; if possible
(defun Help-princ-face (object face)
  (cond ((bufferp standard-output)
	 (let ((opoint (point standard-output)))
	   (princ object)
	   (put-nonduplicable-text-property opoint (point standard-output)
					    'face face standard-output)))
	((markerp standard-output)
	 (let ((buf (marker-buffer standard-output))
	       (pos (marker-position standard-output)))
	   (princ object)
	   (put-nonduplicable-text-property
	    pos (marker-position standard-output) 'face face buf)))
	(t (princ object))))

;; replacement for `prin1' that puts the text in the specified face,
;; if possible
(defun Help-prin1-face (object face)
  (cond ((bufferp standard-output)
	 (let ((opoint (point standard-output)))
	   (prin1 object)
	   (put-nonduplicable-text-property opoint (point standard-output)
					    'face face standard-output)))
	((markerp standard-output)
	 (let ((buf (marker-buffer standard-output))
	       (pos (marker-position standard-output)))
	   (prin1 object)
	   (put-nonduplicable-text-property
	    pos (marker-position standard-output) 'face face buf)))
	(t (prin1 object))))

(defvar help-symbol-regexp
  (let ((sym-char "[+a-zA-Z0-9_:*]")
	(sym-char-no-dash "[-+a-zA-Z0-9_:*]"))
    (concat "\\("
	    ;; a symbol with a - in it.
	    "\\<\\(" sym-char-no-dash "+\\(-" sym-char-no-dash "+\\)+\\)\\>"
	    "\\|"
	    "`\\(" sym-char "+\\)'"
	    "\\)")))

(defun help-symbol-run-function-1 (ev ex fun)
  (let ((help-sticky-window
	 ;; if we were called from a help buffer, make sure the new help
	 ;; goes in the same window.
	 (if (and (event-buffer ev)
		  (symbol-value-in-buffer 'help-window-config
					  (event-buffer ev)))
	     (event-window ev)
	   help-sticky-window)))
    (funcall fun (extent-property ex 'help-symbol))))

(defun help-symbol-run-function (fun)
  (let ((ex (extent-at-event last-popup-menu-event 'help-symbol)))
    (when ex
      (help-symbol-run-function-1 last-popup-menu-event ex fun))))

(defvar help-symbol-function-context-menu
  '(["View %_Documentation" (help-symbol-run-function 'describe-function)]
    ["Find %_Function Source" (help-symbol-run-function 'find-function)]
    ["Find %_Tag" (help-symbol-run-function 'find-tag)]
    ))

(defvar help-symbol-variable-context-menu
  '(["View %_Documentation" (help-symbol-run-function 'describe-variable)]
    ["Find %_Variable Source" (help-symbol-run-function 'find-variable)]
    ["Find %_Tag" (help-symbol-run-function 'find-tag)]
    ))

(defvar help-symbol-function-and-variable-context-menu
  '(["View Function %_Documentation" (help-symbol-run-function
				      'describe-function)]
    ["View Variable D%_ocumentation" (help-symbol-run-function
				      'describe-variable)]
    ["Find %_Function Source" (help-symbol-run-function 'find-function)]
    ["Find %_Variable Source" (help-symbol-run-function 'find-variable)]
    ["Find %_Tag" (help-symbol-run-function 'find-tag)]
    ))

(defun frob-help-extents (buffer)
  ;; Look through BUFFER, starting at the buffer's point and continuing
  ;; till end of file, and find documented functions and variables.
  ;; any such symbol found is tagged with an extent, that sets up these
  ;; properties:
  ;; 1. mouse-face is 'highlight (so the extent gets highlighted on mouse over)
  ;; 2. help-symbol is the name of the symbol.
  ;; 3. face is 'hyper-apropos-hyperlink.
  ;; 4. context-menu is a list of context menu items, specific to whether
  ;;    the symbol is a function, variable, or both.
  ;; 5. activate-function will cause the function or variable to be described,
  ;;    replacing the existing help contents.
  (save-excursion
    (set-buffer buffer)
    (let (b e name)
      (while (re-search-forward help-symbol-regexp nil t)
	(setq b (or (match-beginning 2) (match-beginning 4)))
	(setq e (or (match-end 2) (match-end 4)))
	(setq name (buffer-substring b e))
	(let* ((sym (intern-soft name))
	       (var (and sym (boundp sym)
			 (documentation-property sym
						 'variable-documentation t)))
	       (fun (and sym (fboundp sym)
			 (condition-case nil
			     (documentation sym t)
			   (void-function "(alias for undefined function)")
			   (error "(unexpected error from `documention')")))))
	  (when (or var fun)
	    (let ((ex (make-extent b e)))
	      (require 'hyper-apropos)
	      (set-extent-property ex 'mouse-face 'highlight)
	      (set-extent-property ex 'help-symbol sym)
	      (set-extent-property ex 'face 'hyper-apropos-hyperlink)
	      (set-extent-property
	       ex 'context-menu
	       (cond ((and var fun)
		      help-symbol-function-and-variable-context-menu)
		     (var help-symbol-variable-context-menu)
		     (fun help-symbol-function-context-menu)))
	      (set-extent-property
	       ex 'activate-function
	       (if fun
		   #'(lambda (ev ex)
		       (help-symbol-run-function-1 ev ex 'describe-function))
		 #'(lambda (ev ex)
		     (help-symbol-run-function-1 ev ex 'describe-variable))))
	      ))))))) ;; 11 parentheses!

(defun describe-function-1 (function &optional nodoc)
  "This function does the work for `describe-function'."
  (princ "`")
  ;; (Help-princ-face function 'font-lock-function-name-face) overkill
  (princ function)
  (princ "' is ")
  (let* ((def function)
	 aliases file-name autoload-file kbd-macro-p fndef macrop)
    (while (and (symbolp def) (fboundp def))
      (when (not (eq def function))
	(setq aliases
	      (if aliases
		  ;; I18N3 Need gettext due to concat
		  (concat aliases
			  (format
			   "\n     which is an alias for `%s', "
			   (symbol-name def)))
		(format "an alias for `%s', " (symbol-name def)))))
      (setq def (symbol-function def)))
    (if (and (fboundp 'compiled-function-annotation)
	     (compiled-function-p def))
	(setq file-name (compiled-function-annotation def)))
    (if (eq 'macro (car-safe def))
	(setq fndef (cdr def)
	      file-name (and (compiled-function-p (cdr def))
			     (fboundp 'compiled-function-annotation)
			     (compiled-function-annotation (cdr def)))
	      macrop t)
      (setq fndef def))
    (if aliases (princ aliases))
    (let ((int #'(lambda (string an-p macro-p)
		   (princ (format
			   (gettext (concat
				     (cond ((commandp def)
					    "an interactive ")
					   (an-p "an ")
					   (t "a "))
				     "%s"
				     (if macro-p " macro" " function")))
			   string)))))
      (cond ((or (stringp def) (vectorp def))
             (princ "a keyboard macro.")
	     (setq kbd-macro-p t))
            ((subrp fndef)
             (funcall int "built-in" nil macrop))
            ((compiled-function-p fndef)
             (funcall int "compiled Lisp" nil macrop))
            ((eq (car-safe fndef) 'lambda)
             (funcall int "Lisp" nil macrop))
            ((eq (car-safe fndef) 'mocklisp)
             (funcall int "mocklisp" nil macrop))
            ((eq (car-safe def) 'autoload)
	     (setq autoload-file (elt def 1))
	     (funcall int "autoloaded Lisp" t (elt def 4)))
	    ((and (symbolp def) (not (fboundp def)))
	     (princ "a symbol with a void (unbound) function definition."))
            (t
             nil)))
    (princ "\n")
    (if autoload-file
	(princ (format "  -- autoloads from \"%s\"\n" autoload-file)))
    (or file-name
	(setq file-name (describe-symbol-find-file function)))
    (if file-name
	(princ (format "  -- loaded from \"%s\"\n" file-name)))
;;     (terpri)
    (if describe-function-show-arglist
	(let ((arglist (function-arglist function)))
	  (when arglist
	    (require 'hyper-apropos)
	    (Help-princ-face arglist 'hyper-apropos-documentation)
	    (terpri))))
    (terpri)
    (cond (kbd-macro-p
	   (princ "These characters are executed:\n\n\t")
	   (princ (key-description def))
	   (cond ((setq def (key-binding def))
		  (princ (format "\n\nwhich executes the command `%s'.\n\n"
				 def))
		  (describe-function-1 def))))
	  (nodoc nil)
	  (t
	   ;; tell the user about obsoleteness.
	   ;; If the function is obsolete and is aliased, don't
	   ;; even bother to report the documentation, as a further
	   ;; encouragement to use the new function.
	   (let ((obsolete (function-obsoleteness-doc function))
		 (compatible (function-compatibility-doc function)))
	     (when obsolete
	       (princ obsolete)
	       (terpri)
	       (terpri))
	     (when compatible
	       (princ compatible)
	       (terpri)
	       (terpri))
	     (unless (and obsolete aliases)
	       (let ((doc (function-documentation function t)))
		 (princ "Documentation:\n")
		 (let ((oldp (point standard-output))
		       newp)
		   (princ doc)
		   (setq newp (point standard-output))
		   (goto-char oldp standard-output)
		   (frob-help-extents standard-output)
		   (goto-char newp standard-output))
		 (unless (or (equal doc "")
			     (eq ?\n (aref doc (1- (length doc)))))
		   (terpri)))))))))


;;; [Obnoxious, whining people who complain very LOUDLY on Usenet
;;; are binding this to keys.]
(defun describe-function-arglist (function)
  (interactive (list (or (function-at-point)
			 (error "no function call at point"))))
  (message nil)
  (message (function-arglist function)))

(defun variable-at-point ()
  (ignore-errors
    (with-syntax-table emacs-lisp-mode-syntax-table
      (save-excursion
	(or (not (zerop (skip-syntax-backward "_w")))
	    (eq (char-syntax (char-after (point))) ?w)
	    (eq (char-syntax (char-after (point))) ?_)
	    (forward-sexp -1))
	(skip-chars-forward "'")
	(let ((obj (read (current-buffer))))
	  (and (symbolp obj) (boundp obj) obj))))))

(defun variable-at-event (event)
  "Return the variable whose name is around the position of EVENT.
EVENT should be a mouse event.  When calling from a popup or context menu,
use `last-popup-menu-event' to find out where the mouse was clicked.
\(You cannot use (interactive \"e\"), unfortunately.  This returns a
misc-user event.)

If the event contains no position, or the position is not over text, or
there is no variable around that point, nil is returned."
  (if (and event (event-buffer event) (event-point event))
      (save-excursion
	(set-buffer (event-buffer event))
	(goto-char (event-point event))
	(variable-at-point))))

(defun variable-obsolete-p (variable)
  "Return non-nil if VARIABLE is obsolete."
  (not (null (get variable 'byte-obsolete-variable))))

(defun variable-obsoleteness-doc (variable)
  "If VARIABLE is obsolete, return a string describing this."
  (let ((obsolete (get variable 'byte-obsolete-variable)))
    (if obsolete
	(format "Obsolete; %s"
		(if (stringp obsolete)
		    obsolete
		  (format "use `%s' instead." obsolete))))))

(defun variable-compatible-p (variable)
  "Return non-nil if VARIABLE is Emacs compatible."
  (not (null (get variable 'byte-compatible-variable))))

(defun variable-compatibility-doc (variable)
  "If VARIABLE is Emacs compatible, return a string describing this."
  (let ((compatible (get variable 'byte-compatible-variable)))
    (if compatible
	(format "Emacs Compatible; %s"
		(if (stringp compatible)
		    compatible
		  (format "use `%s' instead." compatible))))))

(defun built-in-variable-doc (variable)
  "Return a string describing whether VARIABLE is built-in."
  (let ((type (built-in-variable-type variable)))
    (case type
      (integer "a built-in integer variable")
      (const-integer "a built-in constant integer variable")
      (boolean "a built-in boolean variable")
      (const-boolean "a built-in constant boolean variable")
      (object "a simple built-in variable")
      (const-object "a simple built-in constant variable")
      (const-specifier "a built-in constant specifier variable")
      (current-buffer "a built-in buffer-local variable")
      (const-current-buffer "a built-in constant buffer-local variable")
      (default-buffer "a built-in default buffer-local variable")
      (selected-console "a built-in console-local variable")
      (const-selected-console "a built-in constant console-local variable")
      (default-console "a built-in default console-local variable")
      (t
       (if type "an unknown type of built-in variable?"
	 "a variable declared in Lisp")))))

(defun describe-variable (variable)
  "Display the full documentation of VARIABLE (a symbol)."
  (interactive
   (let* ((v (variable-at-point))
          (val (let ((enable-recursive-minibuffers t))
                 (completing-read
                   (if v
                       (format "Describe variable (default %s): " v)
                       (gettext "Describe variable: "))
                   obarray 'boundp t nil 'variable-history
		   (symbol-name v)))))
     (list (intern val))))
  (with-displaying-help-buffer
   (lambda ()
     (let ((origvar variable)
	   aliases)
       (let ((print-escape-newlines t))
	 (princ "`")
	 ;; (Help-princ-face (symbol-name variable)
	 ;;		  'font-lock-variable-name-face) overkill
	 (princ (symbol-name variable))
	 (princ "' is ")
	 (while (variable-alias variable)
	   (let ((newvar (variable-alias variable)))
	     (if aliases
		 ;; I18N3 Need gettext due to concat
		 (setq aliases
		       (concat aliases
			       (format "\n     which is an alias for `%s',"
				       (symbol-name newvar))))
	       (setq aliases
		     (format "an alias for `%s',"
			     (symbol-name newvar))))
	     (setq variable newvar)))
	 (if aliases
	     (princ (format "%s" aliases)))
	 (princ (built-in-variable-doc variable))
	 (princ ".\n")
	 (let ((file-name (describe-symbol-find-file variable)))
	   (if file-name
	       (princ (format "  -- loaded from \"%s\"\n" file-name))))
	 (princ "\nValue: ")
	 (require 'hyper-apropos)
    	 (if (not (boundp variable))
	     (Help-princ-face "void\n" 'hyper-apropos-documentation)
	   (Help-prin1-face (symbol-value variable)
			    'hyper-apropos-documentation)
	   (terpri))
	 (terpri)
	 (cond ((local-variable-p variable (current-buffer))
		(let* ((void (cons nil nil))
		       (def (condition-case nil
				(default-value variable)
			      (error void))))
		  (princ "This value is specific to the current buffer.\n")
		  (if (local-variable-p variable nil)
		      (princ "(Its value is local to each buffer.)\n"))
		  (terpri)
		  (if (if (eq def void)
			  (boundp variable)
			(not (eq (symbol-value variable) def)))
		      ;; #### I18N3 doesn't localize properly!
		      (progn (princ "Default-value: ")
			     (if (eq def void)
				 (princ "void\n")
			       (prin1 def)
			       (terpri))
			     (terpri)))))
	       ((local-variable-p variable (current-buffer) t)
		(princ "Setting it would make its value buffer-local.\n\n"))))
       (princ "Documentation:")
       (terpri)
       (let ((doc (documentation-property variable 'variable-documentation))
	     (obsolete (variable-obsoleteness-doc origvar))
	     (compatible (variable-compatibility-doc origvar)))
	 (when obsolete
	   (princ obsolete)
	   (terpri)
	   (terpri))
	 (when compatible
	   (princ compatible)
	   (terpri)
	   (terpri))
	 ;; don't bother to print anything if variable is obsolete and aliased.
	 (when (or (not obsolete) (not aliases))
	   (if doc
	       ;; note: documentation-property calls substitute-command-keys.
	       (let ((oldp (point standard-output))
		     newp)
		 (princ doc)
		 (setq newp (point standard-output))
		 (goto-char oldp standard-output)
		 (frob-help-extents standard-output)
		 (goto-char newp standard-output))
	     (princ "not documented as a variable."))))
       (terpri)))
   (format "variable `%s'" variable)))

(defun sorted-key-descriptions (keys &optional separator)
  "Sort and separate the key descriptions for KEYS.
The sorting is done by length (shortest bindings first), and the bindings
are separated with SEPARATOR (\", \" by default)."
  (mapconcat 'key-description
	     (sort keys #'(lambda (x y)
			    (< (length x) (length y))))
	     (or separator ", ")))

(defun where-is (definition &optional insert)
  "Print message listing key sequences that invoke specified command.
Argument is a command definition, usually a symbol with a function definition.
When run interactively, it defaults to any function found by
`function-at-point'.
If INSERT (the prefix arg) is non-nil, insert the message in the buffer."
  (interactive
   (let ((fn (function-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq val (read-command
		(if fn (format "Where is command (default %s): " fn)
		  "Where is command: ")
                (and fn (symbol-name fn))))
     (list (if (equal (symbol-name val) "")
	       fn val)
	   current-prefix-arg)))
  (let ((keys (where-is-internal definition)))
    (if keys
	(if insert
	    (princ (format "%s (%s)" (sorted-key-descriptions keys)
			   definition) (current-buffer))
	  (message "%s is on %s" definition (sorted-key-descriptions keys)))
      (if insert
	  (princ (format (if (commandp definition) "M-x %s RET"
			   "M-: (%s ...)") definition) (current-buffer))
	(message "%s is not on any keys" definition))))
  nil)

;; `locate-library' moved to "packages.el"


;; Functions ported from C into Lisp in XEmacs

(defun describe-syntax ()
  "Describe the syntax specifications in the syntax table.
The descriptions are inserted in a buffer, which is then displayed."
  (interactive)
  (with-displaying-help-buffer
   (lambda ()
     ;; defined in syntax.el
     (describe-syntax-table (syntax-table) standard-output))
   (format "syntax-table for %s" major-mode)))

(defun list-processes ()
  "Display a list of all processes.
\(Any processes listed as Exited or Signaled are actually eliminated
after the listing is made.)"
  (interactive)
  (with-output-to-temp-buffer "*Process List*"
    (set-buffer standard-output)
    (buffer-disable-undo standard-output)
    (make-local-variable 'truncate-lines)
    (setq truncate-lines t)
    ;;      00000000001111111111222222222233333333334444444444
    ;;      01234567890123456789012345678901234567890123456789
    ;; rewritten for I18N3.  This one should stay rewritten
    ;; so that the dashes will line up properly.
    (princ "Proc         Status   Buffer         Tty         Command\n----         ------   ------         ---         -------\n")
    (let ((tail (process-list)))
      (while tail
	(let* ((p (car tail))
	       (pid (process-id p))
	       (s (process-status p)))
	  (setq tail (cdr tail))
	  (princ (format "%-13s" (process-name p)))
	  (princ s)
	  (if (and (eq s 'exit) (/= (process-exit-status p) 0))
	      (princ (format " %d" (process-exit-status p))))
	  (if (memq s '(signal exit closed))
	      ;; Do delete-exited-processes' work
	      (delete-process p))
	  (indent-to 22 1)		;####
	  (let ((b (process-buffer p)))
	    (cond ((not b)
		   (princ "(none)"))
		  ((not (buffer-name b))
		   (princ "(killed)"))
		  (t
		   (princ (buffer-name b)))))
	  (indent-to 37 1)		;####
	  (let ((tn (process-tty-name p)))
	    (cond ((not tn)
		   (princ "(none)"))
		  (t
		   (princ (format "%s" tn)))))
	  (indent-to 49 1)		;####
	  (if (not (integerp pid))
	      (progn
		(princ "network stream connection ")
		(princ (car pid))
		(princ "@")
		(princ (cdr pid)))
	    (let ((cmd (process-command p)))
	      (while cmd
		(princ (car cmd))
		(setq cmd (cdr cmd))
		(if cmd (princ " ")))))
	  (terpri))))))

;; Stop gap for 21.0 until we do help-char etc properly.
(defun help-keymap-with-help-key (keymap form)
  "Return a copy of KEYMAP with an help-key binding according to help-char
 invoking FORM like help-form.  An existing binding is not overridden.
 If FORM is nil then no binding is made."
  (let ((map (copy-keymap keymap))
	(key (if (characterp help-char)
		 (vector (character-to-event help-char))
	       help-char)))
    (when (and form key (not (lookup-key map key)))
      (define-key map key
	`(lambda () (interactive) (help-print-help-form ,form))))
    map))

(defun help-print-help-form (form)
  (let ((string (eval form)))
    (if (stringp string)
	(with-displaying-help-buffer
	 (insert string)))))

;;; help.el ends here
