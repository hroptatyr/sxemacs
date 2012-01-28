;;; toolbar-items.el -- Static initialization of SXEmacs toolbar

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) 1994 Andy Piper <andyp@parallax.demon.co.uk>
;; Copyright (C) 1995 Board of Trustees, University of Illinois
;; Copyright (C) 1996 Ben Wing <ben@xemacs.org>

;; Maintainer: SXEmacs development team
;; Keywords: frames, dumped

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

;;; Synched up:  Not in FSF

;;; Commentary:

;; This file is dumped with SXEmacs (when window system and toolbar support
;; is compiled in).

;; Miscellaneous toolbar functions, useful for users to redefine, in
;; order to get different behavior.

;;; Code:
(eval-when-compile
  (globally-declare-fboundp
   '(ispell-buffer ispell-message ispell-region compile gnus)))

(defgroup toolbar nil
  "Configure SXEmacs Toolbar functions and properties"
  :group 'environment)

;; #### The following function is slightly obnoxious as it stands.  I
;; think it should print a message like "Toolbar not configured; press
;; me again to configure it", and when the button is pressed again
;; (within a reasonable period of time), `customize-variable' should
;; be invoked for the appropriate variable.

(defun toolbar-not-configured ()
  (interactive)
  ;; Note: we don't use `susbtitute-command-keys' here, because
  ;; Customize is bound to `C-h C' by default, and that binding is not
  ;; familiar to people.  This is more descriptive.
  (error
   "Configure the item via `M-x customize RET toolbar RET'"))

(defcustom toolbar-open-function 'find-file
  "*Function to call when the open icon is selected."
  :type '(radio (function-item find-file)
		(function :tag "Other"))
  :group 'toolbar)

(defun toolbar-open ()
  (interactive)
  (call-interactively toolbar-open-function))

(defcustom toolbar-dired-function 'dired
  "*Function to call when the dired icon is selected."
  :type '(radio (function-item dired)
		(function :tag "Other"))
  :group 'toolbar)

(defun toolbar-dired (dir)
  (interactive "DDired directory: ")
  (funcall toolbar-dired-function dir))

(defcustom toolbar-save-function 'save-buffer
  "*Function to call when the save icon is selected."
  :type '(radio (function-item save-buffer)
		(function :tag "Other"))
  :group 'toolbar)

(defun toolbar-save ()
  (interactive)
  (call-interactively toolbar-save-function))

(defcustom toolbar-print-function 'lpr-buffer
  "*Function to call when the print icon is selected."
  :type '(radio (function-item lpr-buffer)
		(function :tag "Other"))
  :group 'toolbar)

(defun toolbar-print ()
  (interactive)
  (call-interactively toolbar-print-function))

(defcustom toolbar-cut-function 'kill-primary-selection
  "*Function to call when the cut icon is selected."
  :type '(radio (function-item kill-primary-selection)
		(function :tag "Other"))
  :group 'toolbar)

(defun toolbar-cut ()
  (interactive)
  (call-interactively toolbar-cut-function))

(defcustom toolbar-copy-function 'copy-primary-selection
  "*Function to call when the copy icon is selected."
  :type '(radio (function-item copy-primary-selection)
		(function :tag "Other"))
  :group 'toolbar)

(defun toolbar-copy ()
  (interactive)
  (call-interactively toolbar-copy-function))

(defcustom toolbar-paste-function 'yank-clipboard-selection
  "*Function to call when the paste icon is selected."
  :type '(radio (function-item yank-clipboard-selection)
		(function :tag "Other"))
  :group 'toolbar)

(defun toolbar-paste ()
  (interactive)
  ;; This horrible kludge is for pending-delete to work correctly.
  (and-boundp 'pending-delete-mode
    pending-delete-mode
    (let ((this-command toolbar-paste-function))
      (declare-fboundp (pending-delete-pre-hook))))
  (call-interactively toolbar-paste-function))

(defcustom toolbar-undo-function 'undo
  "*Function to call when the undo icon is selected."
  :type '(radio (function-item undo)
		(function :tag "Other"))
  :group 'toolbar)

(defun toolbar-undo ()
  (interactive)
  (call-interactively toolbar-undo-function))

(defcustom toolbar-replace-function 'query-replace
  "*Function to call when the replace icon is selected."
  :type '(radio (function-item query-replace)
		(function :tag "Other"))
  :group 'toolbar)

(defun toolbar-replace ()
  (interactive)
  (call-interactively toolbar-replace-function))

;;
;; toolbar ispell variables and defuns
;;

(defun toolbar-ispell-internal ()
  (interactive)
  (cond
   ((region-active-p) (ispell-region (region-beginning) (region-end)))
   ((eq major-mode 'mail-mode) (ispell-message))
   ((eq major-mode 'message-mode) (ispell-message))
   (t (ispell-buffer))))

(defcustom toolbar-ispell-function 'toolbar-ispell-internal
  "*Function to call when the ispell icon is selected."
  :type '(radio (function-item toolbar-ispell-internal)
		(function :tag "Other"))
  :group 'toolbar)

(defun toolbar-ispell ()
  "Intelligently spell the region or buffer."
  (interactive)
  (call-interactively toolbar-ispell-function))

;;
;; toolbar mail variables and defuns
;;

;; This used to be a macro that expanded its arguments to a form that
;; called `call-process'.  With the advent of customize, it's better
;; to have it as a defun, to make customization easier.
(defun toolbar-external (process &rest args)
  (interactive)
  (apply 'call-process process nil 0 nil args))

(defcustom toolbar-mail-commands-alist
  `((not-configured . toolbar-not-configured)
    (vm		. vm)
    (gnus	. gnus-no-server)
    (rmail	. rmail)
    (mh		. mh-rmail)
    (pine	. (toolbar-external "xterm" "-e" "pine")) ; *gag*
    (elm	. (toolbar-external "xterm" "-e" "elm"))
    (mutt	. (toolbar-external "xterm" "-e" "mutt"))
    (exmh	. (toolbar-external "exmh"))
    (netscape	. (toolbar-external "netscape" "mailbox:"))
    (send	. mail))
  "*Alist of mail readers and their commands.
The car of each alist element is the mail reader, and the cdr is the form
used to start it."
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Mailer") (function :tag "Start with")))
  :group 'toolbar)

(defcustom toolbar-mail-reader 'not-configured
  "*Mail reader toolbar will invoke.
The legal values are the keys from `toolbar-mail-command-alist', which
 should be used to add new mail readers.
Mail readers known by default are vm, gnus, rmail, mh, pine, elm,
 mutt, exmh, netscape and send."
  :type '(choice (const :tag "Not Configured" not-configured)
		 (const vm) (const gnus) (const rmail) (const mh)
		 (const pine) (const elm) (const mutt) (const exmh)
		 (const netscape)
		 (const send)
		 (symbol :tag "Other"
			 :validate (lambda (wid)
				     (if (assq (widget-value wid)
					       toolbar-mail-commands-alist)
					 nil
				       (widget-put wid :error
						   "Unknown mail reader")
				       wid))))
  :group 'toolbar)


(defun toolbar-mail ()
  "Run mail in a separate frame."
  (interactive)
  (let ((command (cdr (assq toolbar-mail-reader toolbar-mail-commands-alist))))
    (or command
	(error "Uknown mail reader %s" toolbar-mail-reader))
    (if (symbolp command)
	(call-interactively command)
      (eval command))))

;;
;; toolbar info variables and defuns
;;

(defcustom toolbar-info-use-separate-frame t
  "*Whether Info is invoked in a separate frame."
  :type 'boolean
  :group 'toolbar)

(defcustom toolbar-info-frame-plist
  ;; Info pages are 80 characters wide, so it makes a good default.
  `(width 80 ,@(let ((h (plist-get default-frame-plist 'height)))
		 (and h `(height ,h))))
  "*The properties of the frame in which news is displayed."
  :type 'plist
  :group 'info)

(define-obsolete-variable-alias 'Info-frame-plist
  'toolbar-info-frame-plist)

(defvar toolbar-info-frame nil
  "The frame in which info is displayed.")

(defun toolbar-info ()
  "Run info in a separate frame."
  (interactive)
  (when toolbar-info-use-separate-frame
    (cond ((or (not toolbar-info-frame)
	       (not (frame-live-p toolbar-info-frame)))
	   ;; We used to raise frame here, but it's a bad idea,
	   ;; because raising is a matter of WM policy.  However, we
	   ;; *must* select it, to ensure that the info buffer goes to
	   ;; the right frame.
	   (setq toolbar-info-frame (make-frame toolbar-info-frame-plist))
	   (select-frame toolbar-info-frame))
	  (t
	   ;; However, if the frame already exists, and the user
	   ;; clicks on info, it's OK to raise it.
	   (select-frame toolbar-info-frame)
	   (raise-frame toolbar-info-frame)))
    (when (frame-iconified-p toolbar-info-frame)
      (deiconify-frame toolbar-info-frame)))
  (declare-fboundp (info)))

;;
;; toolbar debug variables and defuns
;;

(defun toolbar-debug ()
  (interactive)
  (if (featurep 'eos-debugger)
      (call-interactively 'eos::start-debugger)
    (require 'gdbsrc)
    (call-interactively 'gdbsrc)))

(defun toolbar-compile ()
  "Run compile without having to touch the keyboard."
  (interactive)
  (declare (special compile-command toolbar-compile-already-run))
  (require 'compile)
  (if (boundp 'toolbar-compile-already-run)
      (compile compile-command)
    (setq toolbar-compile-already-run t)
    (if (should-use-dialog-box-p)
       (make-dialog-box 'question
			:question (concat "Compile:\n        " compile-command)
			:buttons
			'(["Compile" (compile compile-command) t]
			  ["Edit command" compile t]
			  nil
			  ["Cancel" (message "Quit") t]))
      (compile compile-command))))

;;
;; toolbar news variables and defuns
;;

(defcustom toolbar-news-commands-alist
  `((not-configured . toolbar-not-configured)
    (gnus	. toolbar-gnus)			; M-x all-hail-gnus
    (rn		. (toolbar-external "xterm" "-e" "rn"))
    (nn		. (toolbar-external "xterm" "-e" "nn"))
    (trn	. (toolbar-external "xterm" "-e" "trn"))
    (xrn	. (toolbar-external "xrn"))
    (slrn	. (toolbar-external "xterm" "-e" "slrn"))
    (pine	. (toolbar-external "xterm" "-e" "pine")) ; *gag*
    (tin	. (toolbar-external "xterm" "-e" "tin")) ; *gag*
    (netscape	. (toolbar-external "netscape" "news:")))
  "*Alist of news readers and their commands.
The car of each alist element the pair is the news reader, and the cdr
is the form used to start it."
  :type '(repeat (cons :format "%v"
		       (symbol :tag "Reader") (sexp :tag "Start with")))
  :group 'toolbar)

(defcustom toolbar-news-reader 'gnus
  "*News reader toolbar will invoke.
The legal values are the keys from `toolbar-news-command-alist', which should
 be used to add new news readers.
Newsreaders known by default are gnus, rn, nn, trn, xrn, slrn, pine
 and netscape."
  :type '(choice (const :tag "Not Configured" not-configured)
		 (const gnus) (const rn) (const nn) (const trn)
		 (const xrn) (const slrn) (const pine) (const tin)
		 (const netscape)
		 (symbol :tag "Other"
			 :validate (lambda (wid)
				     (if (assq (widget-value wid)
					       toolbar-news-commands-alist)
					 nil
				       (widget-put wid :error
						   "Unknown news reader")
				       wid))))
  :group 'toolbar)

(defcustom toolbar-news-use-separate-frame t
  "*Whether Gnus is invoked in a separate frame."
  :type 'boolean
  :group 'toolbar)

(defvar toolbar-news-frame nil
  "The frame in which news is displayed.")

(defcustom toolbar-news-frame-plist nil
  "*The properties of the frame in which news is displayed."
  :type 'plist
  :group 'toolbar)

(define-obsolete-variable-alias 'toolbar-news-frame-properties
  'toolbar-news-frame-plist)

(defun toolbar-gnus ()
  "Run Gnus in a separate frame."
  (interactive)
  (if (not toolbar-news-use-separate-frame)
      (gnus)
    (unless (frame-live-p toolbar-news-frame)
      (setq toolbar-news-frame (make-frame toolbar-news-frame-plist))
      (add-hook 'gnus-exit-gnus-hook
		(lambda ()
		  (when (frame-live-p toolbar-news-frame)
		    (if (cdr (frame-list))
			(delete-frame toolbar-news-frame))
		    (setq toolbar-news-frame nil))))
      (select-frame toolbar-news-frame)
      (gnus))
    (when (framep toolbar-news-frame)
      (when (frame-iconified-p toolbar-news-frame)
	(deiconify-frame toolbar-news-frame))
      (select-frame toolbar-news-frame)
      (raise-frame toolbar-news-frame))))

(defun toolbar-news ()
  "Run News."
  (interactive)
  (let ((command (cdr-safe
		  (assq toolbar-news-reader toolbar-news-commands-alist))))
    (or command
	(error "Unknown news reader %s" toolbar-news-reader))
    (if (symbolp command)
	(call-interactively command)
      (eval command))))

(defvar toolbar-last-win-icon nil "A `last-win' icon set.")
(defvar toolbar-next-win-icon nil "A `next-win' icon set.")
(defvar toolbar-file-icon     nil "A `file' icon set.")
(defvar toolbar-folder-icon   nil "A `folder' icon set")
(defvar toolbar-disk-icon     nil "A `disk' icon set.")
(defvar toolbar-printer-icon  nil "A `printer' icon set.")
(defvar toolbar-cut-icon      nil "A `cut' icon set.")
(defvar toolbar-copy-icon     nil "A `copy' icon set.")
(defvar toolbar-paste-icon    nil "A `paste' icon set.")
(defvar toolbar-undo-icon     nil "An `undo' icon set.")
(defvar toolbar-spell-icon    nil "A `spell' icon set.")
(defvar toolbar-replace-icon  nil "A `replace' icon set.")
(defvar toolbar-mail-icon     nil "A `mail' icon set.")
(defvar toolbar-info-icon     nil "An `info' icon set.")
(defvar toolbar-compile-icon  nil "A `compile' icon set.")
(defvar toolbar-debug-icon    nil "A `debugger' icon set.")
(defvar toolbar-news-icon     nil "A `news' icon set.")

;;; each entry maps a variable to the prefix used.

(defvar init-x-toolbar-list
  '((toolbar-last-win-icon . "last-win")
    (toolbar-next-win-icon . "next-win")
    (toolbar-file-icon     . "file")
    (toolbar-folder-icon   . "folder")
    (toolbar-disk-icon     . "disk")
    (toolbar-printer-icon  . "printer")
    (toolbar-cut-icon      . "cut")
    (toolbar-copy-icon     . "copy")
    (toolbar-paste-icon    . "paste")
    (toolbar-undo-icon     . "undo")
    (toolbar-spell-icon    . "spell")
    (toolbar-replace-icon  . "replace")
    (toolbar-mail-icon     . "mail")
    (toolbar-info-icon     . "info-def")
    (toolbar-compile-icon  . "compile")
    (toolbar-debug-icon    . "debug")
    (toolbar-news-icon     . "news")))

(defun init-x-toolbar ()
  (toolbar-add-item-data init-x-toolbar-list )
  ;; do this now because errors will occur if the icon symbols
  ;; are not initted
  (set-specifier default-toolbar initial-toolbar-spec))

(defun toolbar-add-item-data ( icon-list &optional icon-dir )
  (if (eq icon-dir nil)
      (setq icon-dir toolbar-icon-directory))
  (mapcar
   (lambda (cons)
     (let ((prefix (expand-file-name (cdr cons)  icon-dir)))
       ;; #### This should use a better mechanism for finding the
       ;; glyphs, allowing for formats other than x[pb]m.  Look at
       ;; `widget-glyph-find' for an example how it might be done.
       (set (car cons)
	    (if (featurep 'xpm)
		(toolbar-make-button-list
		 (concat prefix "-up.xpm")
		 nil
		 (concat prefix "-xx.xpm")
		 (concat prefix "-cap-up.xpm")
		 nil
		 (concat prefix "-cap-xx.xpm"))
	      (toolbar-make-button-list
	       (concat prefix "-up.xbm")
	       (concat prefix "-dn.xbm")
	       (concat prefix "-xx.xbm"))))))
   icon-list))

(defvar toolbar-vector-open
  [toolbar-file-icon            toolbar-open	t       "Open a file"]
  "Define the vector for the \"Open\" toolbar button")

(defvar toolbar-vector-dired
  [toolbar-folder-icon	        toolbar-dired	t	"Edit a directory"]
  "Define the vector for the \"Dired\" toolbar button")

(defvar toolbar-vector-save
  [toolbar-disk-icon		toolbar-save	t	"Save buffer"]
  "Define the vector for the \"Save\" toolbar button")

(defvar toolbar-vector-print
  [toolbar-printer-icon	        toolbar-print	t	"Print buffer"]
  "Define the vector for the \"Printer\" toolbar button")

(defvar toolbar-vector-cut
  [toolbar-cut-icon		toolbar-cut	t	"Kill region"]
  "Define the vector for the \"Cut\" toolbar button")

(defvar toolbar-vector-copy
  [toolbar-copy-icon		toolbar-copy	t	"Copy region"]
  "Define the vector for the \"Copy\" toolbar button")

(defvar toolbar-vector-paste
  [toolbar-paste-icon		toolbar-paste	t	"Paste from clipboard"]
  "Define the vector for the \"Paste\" toolbar button")

(defvar toolbar-vector-undo
  [toolbar-undo-icon		toolbar-undo	t	"Undo edit"]
  "Define the vector for the \"Undo\" toolbar button")

(defvar toolbar-vector-spell
  [toolbar-spell-icon		toolbar-ispell	t	"Check spelling"]
  "Define the vector for the \"Spell\" toolbar button")

(defvar toolbar-vector-replace
  [toolbar-replace-icon	        toolbar-replace	t	"Search & Replace"]
  "Define the vector for the \"Replace\" toolbar button")

(defvar toolbar-vector-mail
  [toolbar-mail-icon		toolbar-mail	t	"Read mail"]
  "Define the vector for the \"Mail\" toolbar button")

(defvar toolbar-vector-info
  [toolbar-info-icon		toolbar-info	t	"Info documentation"]
  "Define the vector for the \"Info\" toolbar button")

(defvar toolbar-vector-compile
  [toolbar-compile-icon	        toolbar-compile	t	"Start a compilation"]
  "Define the vector for the \"Compile\" toolbar button")

(defvar toolbar-vector-debug
  [toolbar-debug-icon		toolbar-debug	t	"Start a debugger"]
  "Define the vector for the \"Debug\" toolbar button")

(defvar toolbar-vector-news
  [toolbar-news-icon		toolbar-news	t	"Read news"]
  "Define the vector for the \"News\" toolbar button")

(defvar initial-toolbar-spec
  (list
    ;;[toolbar-last-win-icon	pop-window-configuration
    ;;(frame-property (selected-frame)
    ;;		'window-config-stack) t	"Most recent window config"]
    ;; #### Illicit knowledge?
    ;; #### These don't work right - not consistent!
    ;; I don't know what's wrong; perhaps `selected-frame' is wrong
    ;; sometimes when this is evaluated.  Note that I even tried to
    ;; kludge-fix this by calls to `set-specifier-dirty-flag' in
    ;; pop-window-configuration and such.

    ;;[toolbar-next-win-icon	unpop-window-configuration
    ;;(frame-property (selected-frame)
    ;;	'window-config-unpop-stack) t "Undo \"Most recent window config\""]
    ;; #### Illicit knowledge?
    toolbar-vector-open
    toolbar-vector-dired
    toolbar-vector-save
    toolbar-vector-print
    toolbar-vector-cut
    toolbar-vector-copy
    toolbar-vector-paste
    toolbar-vector-undo
    toolbar-vector-spell
    toolbar-vector-replace
    toolbar-vector-mail
    toolbar-vector-info
    toolbar-vector-compile
    toolbar-vector-debug
    toolbar-vector-news
    )
  "The initial toolbar for a buffer.")

(defun x-init-toolbar-from-resources (locale)
  (x-init-specifier-from-resources
   top-toolbar-height 'natnum locale
   '("topToolBarHeight" . "TopToolBarHeight"))
  (x-init-specifier-from-resources
   bottom-toolbar-height 'natnum locale
   '("bottomToolBarHeight" . "BottomToolBarHeight"))
  (x-init-specifier-from-resources
   left-toolbar-width 'natnum locale
   '("leftToolBarWidth" . "LeftToolBarWidth"))
  (x-init-specifier-from-resources
   right-toolbar-width 'natnum locale
   '("rightToolBarWidth" . "RightToolBarWidth"))
  (x-init-specifier-from-resources
   top-toolbar-border-width 'natnum locale
   '("topToolBarBorderWidth" . "TopToolBarBorderWidth"))
  (x-init-specifier-from-resources
   bottom-toolbar-border-width 'natnum locale
   '("bottomToolBarBorderWidth" . "BottomToolBarBorderWidth"))
  (x-init-specifier-from-resources
   left-toolbar-border-width 'natnum locale
   '("leftToolBarBorderWidth" . "LeftToolBarBorderWidth"))
  (x-init-specifier-from-resources
   right-toolbar-border-width 'natnum locale
   '("rightToolBarBorderWidth" . "RightToolBarBorderWidth")))

;;; toolbar-items.el ends here
