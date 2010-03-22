;;; lisp-mode.el --- Lisp mode, and its idiosyncratic commands.

;; Copyright (C) 1985, 1996, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems.

;; Maintainer: FSF
;; Keywords: lisp, languages, dumped

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

;;; Synched up with: FSF 19.34 (but starting to diverge).

;;; Commentary:

;; This file is dumped with SXEmacs.

;; The base major mode for editing Lisp code (used also for Emacs Lisp).
;; This mode is documented in the Emacs manual

;;; Code:

(defgroup lisp nil
  "Lisp support, including Emacs Lisp."
  :group 'languages
  :group 'development)

(defvar lisp-mode-syntax-table nil)
(defvar emacs-lisp-mode-syntax-table nil)
(defvar lisp-mode-abbrev-table nil)

(defun construct-lisp-mode-menu (popup-p emacs-lisp-p)
  (flet ((popup-wrap (form)
	   (if popup-p `(menu-call-at-event ',form) form)))
    `(,@(if emacs-lisp-p
	  `(["%_Byte-Compile This File" ,(popup-wrap
					  'emacs-lisp-byte-compile)]
	    ["B%_yte-Compile/Load This File"
	     ,(popup-wrap 'emacs-lisp-byte-compile-and-load)]
	    ["Byte-%_Recompile Directory..."
	     ,(popup-wrap 'byte-recompile-directory)]
	    "---"))
	["%_Evaluate Region or Defun"
	 ,(popup-wrap '(if (region-exists-p)
			   (call-interactively 'eval-region)
			 (call-interactively 'eval-defun)))]
	["Evaluate %_Whole Buffer" ,(popup-wrap 'eval-current-buffer)]
	["Evaluate Last %_S-expression" ,(popup-wrap 'eval-last-sexp)]
	"---"
	,@(if popup-p
	    '(["%_Find Function"
	       (find-function (menu-call-at-event '(function-at-point)))
	       :suffix (let ((fun (menu-call-at-event '(function-at-point))))
			 (if fun (symbol-name fun) ""))
	       :active (and (fboundp 'find-function)
			    (menu-call-at-event '(function-at-point)))]
	      ["%_Find Variable"
	       (find-variable (menu-call-at-event '(variable-at-point)))
	       :suffix (let ((fun (menu-call-at-event '(variable-at-point))))
			 (if fun (symbol-name fun) ""))
	       :active (and (fboundp 'find-variable)
			    (menu-call-at-event '(variable-at-point)))]
	      ["%_Help on Function"
	       (describe-function (menu-call-at-event '(function-at-point)))
	       :suffix (let ((fun (menu-call-at-event '(function-at-point))))
			 (if fun (symbol-name fun) ""))
	       :active (and (fboundp 'describe-function)
			    (menu-call-at-event '(function-at-point)))]
	      ["%_Help on Variable"
	       (describe-variable (menu-call-at-event '(variable-at-point)))
	       :suffix (let ((fun (menu-call-at-event '(variable-at-point))))
			 (if fun (symbol-name fun) ""))
	       :active (and (fboundp 'describe-variable)
			    (menu-call-at-event '(variable-at-point)))])
	    '(["Find %_Function..." find-function
	       :active (fboundp 'find-function)]
	      ["Find %_Variable..." find-variable
	       :active (fboundp 'find-variable)]
	      ["%_Help on Function..." describe-function
	       :active (fboundp 'describe-function)]
	      ["Hel%_p on Variable..." describe-variable
	       :active (fboundp 'describe-variable)]))
	"---"
	["Instrument This Defun for %_Debugging" ,(popup-wrap 'edebug-defun)]
	["%_Trace Function..." trace-function-background]
	["%_Untrace All Functions" untrace-all
	 :active (fboundp 'untrace-all)]
	"---"
	["%_Comment Out Region" comment-region :active (region-exists-p)]
	"---"
	["%_Indent Region or Balanced Expression"
	 ,(popup-wrap '(if (region-exists-p)
			   (call-interactively 'indent-region)
			 (call-interactively 'indent-sexp)))]
	["I%_ndent Defun"
	 ,(popup-wrap '(progn
			 (beginning-of-defun)
			 (indent-sexp)))]
	"---"
	"Look for debug-on-error under Options->Troubleshooting"
	)))

(defvar lisp-interaction-mode-popup-menu
  (cons "Lisp-Interaction" (construct-lisp-mode-menu t nil)))

(defvar emacs-lisp-mode-popup-menu
  (cons "Emacs-Lisp" (construct-lisp-mode-menu t t)))

;Don't have a menubar entry in Lisp Interaction mode.  Otherwise, the
;*scratch* buffer has a Lisp menubar item!  Very confusing.
;Jan Vroonhof really wants this, so it's back.  --ben
(defvar lisp-interaction-mode-menubar-menu
  (cons "%_Lisp" (construct-lisp-mode-menu nil nil)))

(defvar emacs-lisp-mode-menubar-menu
  (cons "%_Lisp" (construct-lisp-mode-menu nil t)))

(if (not emacs-lisp-mode-syntax-table)
    (let ((i 0))
      (setq emacs-lisp-mode-syntax-table (make-syntax-table))
      (while (< i ?0)
	(modify-syntax-entry i "_   " emacs-lisp-mode-syntax-table)
	(setq i (1+ i)))
      (setq i (1+ ?9))
      (while (< i ?A)
	(modify-syntax-entry i "_   " emacs-lisp-mode-syntax-table)
	(setq i (1+ i)))
      (setq i (1+ ?Z))
      (while (< i ?a)
	(modify-syntax-entry i "_   " emacs-lisp-mode-syntax-table)
	(setq i (1+ i)))
      (setq i (1+ ?z))
      (while (< i 128)
	(modify-syntax-entry i "_   " emacs-lisp-mode-syntax-table)
	(setq i (1+ i)))
      (modify-syntax-entry ?  "    " emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\t "    " emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\f "    " emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\n ">   " emacs-lisp-mode-syntax-table)
      ;; Give CR the same syntax as newline, for selective-display.
      (modify-syntax-entry ?\^m ">   " emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\; "<   " emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?` "'   " emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?' "'   " emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?, "'   " emacs-lisp-mode-syntax-table)
      ;; Used to be singlequote; changed for flonums.
      (modify-syntax-entry ?. "_   " emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?# "'   " emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\" "\"    " emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\\ "\\   " emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\( "()  " emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\) ")(  " emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\[ "(]  " emacs-lisp-mode-syntax-table)
      (modify-syntax-entry ?\] ")[  " emacs-lisp-mode-syntax-table)))

(if (not lisp-mode-syntax-table)
    (progn (setq lisp-mode-syntax-table
		 (copy-syntax-table emacs-lisp-mode-syntax-table))
	   (modify-syntax-entry ?\[ "_   " lisp-mode-syntax-table)
	   ;; XEmacs changes
	   (modify-syntax-entry ?\] "_   " lisp-mode-syntax-table)
	   (modify-syntax-entry ?#  "' 58" lisp-mode-syntax-table)
	   (modify-syntax-entry ?|  "\" 67" lisp-mode-syntax-table)))

(define-abbrev-table 'lisp-mode-abbrev-table ())

(defvar lisp-imenu-generic-expression
      '(
	(nil 
	 #r"^\s-*(def\(un\|subst\|macro\|advice\)\s-+\([-A-Za-z0-9+*|:]+\)" 2)
	("Variables" 
	 #r"^\s-*(def\(var\|const\|custom\)\s-+\([-A-Za-z0-9+*|:]+\)" 2)
	("Types" 
	 #r"^\s-*(def\(group\|type\|struct\|class\|ine-condition\)\s-+\([-A-Za-z0-9+*|:]+\)" 
	 2))

  "Imenu generic expression for Lisp mode.  See `imenu-generic-expression'.")

(defun lisp-mode-variables (lisp-syntax)
  (cond (lisp-syntax
	 (set-syntax-table lisp-mode-syntax-table)))
  (setq local-abbrev-table lisp-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat page-delimiter "\\|$" ))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'lisp-indent-region)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp ";;; \\|(....")
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  ;; XEmacs change
  (set (make-local-variable 'block-comment-start) ";;")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip (concat #r"\(\(^\|"
                                   "[^\\\\\n]"
                                   #r"\)\(\\\\\)*\);+ *"))
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'lisp-comment-indent)
  ;; XEmacs change
  (set (make-local-variable 'dabbrev-case-fold-search) nil)
  (set (make-local-variable 'dabbrev-case-replace) nil)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression lisp-imenu-generic-expression))

(defvar shared-lisp-mode-map ()
  "Keymap for commands shared by all sorts of Lisp modes.")

(if shared-lisp-mode-map
    ()
   (setq shared-lisp-mode-map (make-sparse-keymap))
   ;; XEmacs changes
   (set-keymap-name shared-lisp-mode-map 'shared-lisp-mode-map)
   (define-key shared-lisp-mode-map "\M-;" 'lisp-indent-for-comment)
   (define-key shared-lisp-mode-map "\e\C-q" 'indent-sexp))

(defvar emacs-lisp-mode-map ()
  "Keymap for Emacs Lisp mode.
All commands in `shared-lisp-mode-map' are inherited by this map.")

(if emacs-lisp-mode-map
    ()
  ;; XEmacs:  Ignore FSF nconc stuff
  (setq emacs-lisp-mode-map (make-sparse-keymap))
  (set-keymap-name emacs-lisp-mode-map 'emacs-lisp-mode-map)
  (set-keymap-parents emacs-lisp-mode-map (list shared-lisp-mode-map))
  (define-key emacs-lisp-mode-map "\e\t" 'lisp-complete-symbol)
  (define-key emacs-lisp-mode-map "\e\C-x" 'eval-defun)
  ;; XEmacs: Not sure what the FSF menu bindings are.  I hope XEmacs
  ;; doesn't need them.
)

;; XEmacs: add docstrings to the hooks
(defvar emacs-lisp-mode-hook nil
  "Hook to run when entering emacs-lisp-mode.")

(defvar lisp-mode-hook nil
  "Hook to run when entering lisp-mode.")

(defvar lisp-interaction-mode-hook nil
  "Hook to run when entering lisp-interaction-mode.")

(defun emacs-lisp-byte-compile ()
  "Byte compile the file containing the current buffer."
  (interactive)
  (if buffer-file-name
      ;; XEmacs change.  Force buffer save first
      (progn
	(save-buffer)
	(byte-compile-file buffer-file-name))
    (error "The buffer must be saved in a file first.")))

(defun emacs-lisp-byte-compile-and-load ()
  "Byte-compile the current file (if it has changed), then load compiled code."
  (interactive)
  (or buffer-file-name
      (error "The buffer must be saved in a file first"))
  (require 'bytecomp)
  ;; Recompile if file or buffer has changed since last compilation.
  (if (and (buffer-modified-p)
	   (y-or-n-p (format "save buffer %s first? " (buffer-name))))
      (save-buffer))
  (let ((compiled-file-name (byte-compile-dest-file buffer-file-name)))
    (if (file-newer-than-file-p compiled-file-name buffer-file-name)
	(load-file compiled-file-name)
      (byte-compile-file buffer-file-name t))))

(defun emacs-lisp-mode ()
  "Major mode for editing Lisp code to run in Emacs.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{emacs-lisp-mode-map}
Entry to this mode calls the value of `emacs-lisp-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map emacs-lisp-mode-map)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  ;; XEmacs changes
  (setq major-mode 'emacs-lisp-mode
	mode-popup-menu emacs-lisp-mode-popup-menu
	mode-name "Emacs-Lisp")
  (if (and (featurep 'menubar)
           current-menubar)
      (progn
	;; make a local copy of the menubar, so our modes don't
	;; change the global menubar
	(set-buffer-menubar current-menubar)
	(add-submenu nil emacs-lisp-mode-menubar-menu)))
  (lisp-mode-variables nil)
  (run-hooks 'emacs-lisp-mode-hook))

(put 'emacs-lisp-mode 'font-lock-lisp-like t)

(defvar lisp-mode-map ()
  "Keymap for ordinary Lisp mode.
All commands in `shared-lisp-mode-map' are inherited by this map.")

(if lisp-mode-map
    ()
  ;; XEmacs changes
  (setq lisp-mode-map (make-sparse-keymap))
  (set-keymap-name lisp-mode-map 'lisp-mode-map)
  (set-keymap-parents lisp-mode-map (list shared-lisp-mode-map))
  (define-key lisp-mode-map "\e\C-x" 'lisp-send-defun)
  ;; gag, no.  use ilisp.  -jwz
;;  (define-key lisp-mode-map "\C-c\C-z" 'run-lisp)
  )

(defun lisp-mode ()
  "Major mode for editing Lisp code for Lisps other than GNU Emacs Lisp.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{lisp-mode-map}
Note that `run-lisp' may be used either to start an inferior Lisp job
or to switch back to an existing one.

Entry to this mode calls the value of `lisp-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map lisp-mode-map)
  (setq major-mode 'lisp-mode)
  (setq mode-name "Lisp")
  (lisp-mode-variables t)
  (set-syntax-table lisp-mode-syntax-table)
  (run-hooks 'lisp-mode-hook))

;; This will do unless shell.el is loaded.
;; XEmacs change
(defun lisp-send-defun ()
  "Send the current defun to the Lisp process made by \\[run-lisp]."
  (interactive)
  (error "Process lisp does not exist"))

;; XEmacs change: emacs-lisp-mode-map is a more appropriate parent.
(defvar lisp-interaction-mode-map ()
  "Keymap for Lisp Interaction mode.
All commands in `shared-lisp-mode-map' are inherited by this map.")

(if lisp-interaction-mode-map
    ()
  ;; XEmacs set keymap our way
  (setq lisp-interaction-mode-map (make-sparse-keymap))
  (set-keymap-name lisp-interaction-mode-map 'lisp-interaction-mode-map)
  (set-keymap-parents lisp-interaction-mode-map (list emacs-lisp-mode-map))
  (define-key lisp-interaction-mode-map "\e\C-x" 'eval-defun)
  (define-key lisp-interaction-mode-map "\e\t" 'lisp-complete-symbol)
  (define-key lisp-interaction-mode-map "\n" 'eval-print-last-sexp))

(defun lisp-interaction-mode ()
  "Major mode for typing and evaluating Lisp forms.
Like Lisp mode except that \\[eval-print-last-sexp] evals the Lisp expression
before point, and prints its value into the buffer, advancing point.

Commands:
Delete converts tabs to spaces as it moves back.
Paragraphs are separated only by blank lines.
Semicolons start comments.
\\{lisp-interaction-mode-map}
Entry to this mode calls the value of `lisp-interaction-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map lisp-interaction-mode-map)
  (setq major-mode 'lisp-interaction-mode)
  (setq mode-name "Lisp Interaction")
  (setq mode-popup-menu lisp-interaction-mode-popup-menu)
  (if (and (featurep 'menubar)
           current-menubar)
      (progn
	;; make a local copy of the menubar, so our modes don't
	;; change the global menubar
	(set-buffer-menubar current-menubar)
	(add-submenu nil lisp-interaction-mode-menubar-menu)))
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (lisp-mode-variables nil)
  (run-hooks 'lisp-interaction-mode-hook))

(defun eval-print-last-sexp ()
  "Evaluate sexp before point; print value into current buffer."
  (interactive)
  (let ((standard-output (current-buffer)))
    (terpri)
    (eval-last-sexp t)
    (terpri)))

;; XEmacs change
(defcustom eval-interactive-verbose t
  "*Non-nil means that interactive evaluation can print messages.
The messages are printed when the expression is treated differently
using `\\[eval-last-sexp]' and `\\[eval-defun]' than it than it would have been
treated noninteractively.

The printed messages are \"defvar treated as defconst\" and \"defcustom
 evaluation forced\".  See `eval-interactive' for more details."
  :type 'boolean
  :group 'lisp)

(defun eval-interactive (expr)
  "Like `eval' except that it transforms defvars to defconsts.
The evaluation of defcustom forms is forced."
  (cond ((and (eq (car-safe expr) 'defvar)
	      (> (length expr) 2))
	 (eval (cons 'defconst (cdr expr)))
	 (when eval-interactive-verbose
	   (message "defvar treated as defconst")
	   (sit-for 1)
	   (message ""))
	 (nth 1 expr))
	((and (eq (car-safe expr) 'defcustom)
	      (> (length expr) 2)
	      (default-boundp (nth 1 expr)))
	 ;; Force variable to be bound
	 ;; #### defcustom might specify a different :set method.
	 (set-default (nth 1 expr) (eval (nth 2 expr)))
	 ;; And evaluate the defcustom
	 (eval expr)
	 (when eval-interactive-verbose
	   (message "defcustom evaluation forced")
	   (sit-for 1)
	   (message ""))
	 (nth 1 expr))
	(t
	 (eval expr))))

;; XEmacs change, based on Bob Weiner suggestion
(defun eval-last-sexp (eval-last-sexp-arg-internal) ;dynamic scoping wonderment
  "Evaluate sexp before point; print value in minibuffer.
With argument, print output into current buffer."
  (interactive "P")
  (let ((standard-output (if eval-last-sexp-arg-internal (current-buffer) t))
	(opoint (point))
	ignore-quotes)
    (prin1 (eval-interactive
	    (letf (((syntax-table) emacs-lisp-mode-syntax-table))
	      (save-excursion
		;; If this sexp appears to be enclosed in `...' then
		;; ignore the surrounding quotes.
		(setq ignore-quotes (or (eq (char-after) ?\')
					(eq (char-before) ?\')))
		(forward-sexp -1)
		;; vladimir@cs.ualberta.ca 30-Jul-1997: skip ` in
		;; `variable' so that the value is returned, not the
		;; name.
		(if (and ignore-quotes
			 (eq (char-after) ?\`))
		    (forward-char))
		(save-restriction
		  (narrow-to-region (point-min) opoint)
		  (let ((expr (read (current-buffer))))
		    (if (eq (car-safe expr) 'interactive)
			;; If it's an (interactive ...) form, it's
			;; more useful to show how an interactive call
			;; would use it.
			`(call-interactively
			  (lambda (&rest args)
			    ,expr args))
		      expr)))))))))

(defun eval-defun (eval-defun-arg-internal)
  "Evaluate defun that point is in or before.
Print value in minibuffer.
With argument, insert value in current buffer after the defun."
  (interactive "P")
  (let ((standard-output (if eval-defun-arg-internal (current-buffer) t)))
    (prin1 (eval-interactive (save-excursion
			       (end-of-defun)
			       (beginning-of-defun)
			       (read (current-buffer)))))))


(defun lisp-comment-indent ()
  (if (looking-at #r"\s<\s<\s<")
      (current-column)
    (if (looking-at #r"\s<\s<")
	;; #### FSF has:
	;; (let ((tem (or (calculate-lisp-indent) (current-column)))) ...
	(let ((tem (calculate-lisp-indent)))
	  (if (listp tem) (car tem) tem))
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
	   comment-column))))

;; XEmacs change
(defun lisp-indent-for-comment ()
  "Indent this line's comment appropriately, or insert an empty comment.
If adding a new comment on a blank line, use `block-comment-start' instead
of `comment-start' to open the comment."
  ;; by Stig@hackvan.com
  ;; #### - This functionality, the recognition of block-comment-{start,end},
  ;; will perhaps be standardized across modes and move to indent-for-comment.
  (interactive)
  (if (and block-comment-start
	   (save-excursion (beginning-of-line) (looking-at "^[ \t]*$")))
      (insert block-comment-start))
  (indent-for-comment))

(defvar lisp-indent-offset nil)
(defvar lisp-indent-function 'lisp-indent-function)

(defun lisp-indent-line (&optional whole-exp)
  "Indent current line as Lisp code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (interactive "P")
  (let ((indent (calculate-lisp-indent)) shift-amt beg end
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (looking-at #r"\s<\s<\s<")
	;; Don't alter indentation of a ;;; comment line.
	(goto-char (- (point-max) pos))
      (if (and (looking-at "\\s<") (not (looking-at #r"\s<\s<")))
	  ;; Single-semicolon comment lines should be indented
	  ;; as comment lines, not as code.
	  (progn (indent-for-comment) (backward-char 1))
	(if (listp indent) (setq indent (car indent)))
	(setq shift-amt (- indent (current-column)))
	(if (zerop shift-amt)
	    nil
	  (delete-region beg (point))
	  (indent-to indent)))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos)))
      ;; If desired, shift remaining lines of expression the same amount.
      (and whole-exp (not (zerop shift-amt))
	   (save-excursion
	     (goto-char beg)
	     (forward-sexp 1)
	     (setq end (point))
	     (goto-char beg)
	     (forward-line 1)
	     (setq beg (point))
	     (> end beg))
	   (indent-code-rigidly beg end shift-amt)))))

(defvar calculate-lisp-indent-last-sexp)

(defun calculate-lisp-indent (&optional parse-start)
  "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
Can instead return a list, whose car is the column to indent to.
This means that following lines at the same level of indentation
should not necessarily be indented the same way.
The second element of the list is the buffer position
of the start of the containing expression."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  ;; XEmacs change (remove paren-depth)
          state ;;paren-depth
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-lisp-indent-last-sexp containing-sexp)
      (if parse-start
          (goto-char parse-start)
          (beginning-of-defun))
      ;; Find outermost containing sexp
      (while (< (point) indent-point)
        (setq state (parse-partial-sexp (point) indent-point 0)))
      ;; Find innermost containing sexp
      (while (and retry
		  state
		  ;; XEmacs change (remove paren-depth)
                  (> ;;(setq paren-depth (elt state 0))
		     (elt state 0)
		     0))
        (setq retry nil)
        (setq calculate-lisp-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-lisp-indent-last-sexp
		 (> calculate-lisp-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
					    indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-lisp-indent-last-sexp)
	    ;; indent-point immediately follows open paren.
	    ;; Don't call hook.
            (setq desired-indent (current-column))
	  ;; Find the start of first element of containing sexp.
	  (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
	  (cond ((looking-at "\\s(")
		 ;; First element of containing sexp is a list.
		 ;; Indent under that list.
		 )
		((> (save-excursion (forward-line 1) (point))
		    calculate-lisp-indent-last-sexp)
		 ;; This is the first line to start within the containing sexp.
		 ;; It's almost certainly a function call.
		 (if (= (point) calculate-lisp-indent-last-sexp)
		     ;; Containing sexp has nothing before this line
		     ;; except the first element.  Indent under that element.
		     nil
		   ;; Skip the first element, find start of second (the first
		   ;; argument of the function call) and indent under.
		   (progn (forward-sexp 1)
			  (parse-partial-sexp (point)
					      calculate-lisp-indent-last-sexp
					      0 t)))
		 (backward-prefix-chars))
		(t
		 ;; Indent beneath first sexp on same line as
		 ;; calculate-lisp-indent-last-sexp.  Again, it's
		 ;; almost certainly a function call.
		 (goto-char calculate-lisp-indent-last-sexp)
		 (beginning-of-line)
		 (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
				     0 t)
		 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by lisp-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
               (goto-char indent-point)
               (skip-chars-forward " \t")
               (current-column))
              (desired-indent)
              ((and (boundp 'lisp-indent-function)
                    lisp-indent-function
                    (not retry))
               (or (funcall lisp-indent-function indent-point state)
                   normal-indent))
	      ;; XEmacs change:
              ;; lisp-indent-offset shouldn't override lisp-indent-function !
              ((and (integerp lisp-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ normal-indent lisp-indent-offset))
              (t
               normal-indent))))))

(defun lisp-indent-function (indent-point state)
  ;; free reference to `calculate-lisp-indent-last-sexp'
  ;; in #'calculate-lisp-indent
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at #r"\sw\|\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
					 calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(if (condition-case nil
		(save-excursion
		  (backward-up-list 1)
		  (backward-up-list 1)
		  (backward-up-list 1)
		  (looking-at "(flet\\s-"))
	      (error nil))
	    (setq method 'defun)
	  (setq method (or (get (intern-soft function) 'lisp-indent-function)
			   (get (intern-soft function) 'lisp-indent-hook))))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
	       (lisp-indent-defform state indent-point))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
		(funcall method state indent-point)))))))

(defvar lisp-body-indent 2
  "Number of columns to indent the second line of a `(def...)' form.")

(defun lisp-indent-specform (count state indent-point normal-indent)
  (let ((containing-form-start (elt state 1))
        (i count)
        body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  lisp-indent-function guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ lisp-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
                (condition-case ()
                    (progn
                      (setq count (1- count))
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))
                  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (if (> count 0)
        ;; A distinguished form.  If it is the first or second form use double
        ;; lisp-body-indent, else normal indent.  With lisp-body-indent bound
        ;; to 2 (the default), this just happens to work the same with if as
        ;; the older code, but it makes unwind-protect, condition-case,
        ;; with-output-to-temp-buffer, et. al. much more tasteful.  The older,
        ;; less hacked, behavior can be obtained by replacing below with
        ;; (list normal-indent containing-form-start).
        (if (<= (- i count) 1)
            (list (+ containing-form-column (* 2 lisp-body-indent))
                  containing-form-start)
            (list normal-indent containing-form-start))
      ;; A non-distinguished form.  Use body-indent if there are no
      ;; distinguished forms and this is the first undistinguished form,
      ;; or if this is the first undistinguished form and the preceding
      ;; distinguished form has indentation at least as great as body-indent.
      (if (or (and (= i 0) (= count 0))
              (and (= count 0) (<= body-indent normal-indent)))
          body-indent
          normal-indent))))

(defun lisp-indent-defform (state indent-point)
  (goto-char (car (cdr state)))
  (forward-line 1)
  (if (> (point) (car (cdr (cdr state))))
      (progn
	(goto-char (car (cdr state)))
	(+ lisp-body-indent (current-column)))))


;; (put 'progn 'lisp-indent-function 0), say, causes progn to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'lambda 'lisp-indent-function 'defun)
(put 'autoload 'lisp-indent-function 'defun)
(put 'progn 'lisp-indent-function 0)
(put 'prog1 'lisp-indent-function 1)
(put 'prog2 'lisp-indent-function 2)
(put 'save-excursion 'lisp-indent-function 0)
(put 'save-window-excursion 'lisp-indent-function 0)
(put 'save-selected-window 'lisp-indent-function 0)
(put 'with-selected-window 'lisp-indent-function 1)
(put 'save-selected-frame 'lisp-indent-function 0)
(put 'with-selected-frame 'lisp-indent-function 1)
(put 'save-restriction 'lisp-indent-function 0)
(put 'save-match-data 'lisp-indent-function 0)
(put 'let 'lisp-indent-function 1)
(put 'let* 'lisp-indent-function 1)
(put 'let-specifier 'lisp-indent-function 1)
(put 'flet 'lisp-indent-function 1)
(put 'while 'lisp-indent-function 1)
(put 'if 'lisp-indent-function 2)
(put 'catch 'lisp-indent-function 1)
(put 'condition-case 'lisp-indent-function 2)
(put 'handler-case 'lisp-indent-function 1)
(put 'handler-bind 'lisp-indent-function 1)
(put 'call-with-condition-handler 'lisp-indent-function 2)
(put 'unwind-protect 'lisp-indent-function 1)
(put 'save-current-buffer 'lisp-indent-function 0)
(put 'with-current-buffer 'lisp-indent-function 1)
(put 'with-string-as-buffer-contents 'lisp-indent-function 1)
(put 'with-temp-file 'lisp-indent-function 1)
(put 'with-temp-buffer 'lisp-indent-function 0)
(put 'with-output-to-string 'lisp-indent-function 0)
(put 'with-output-to-temp-buffer 'lisp-indent-function 1)
(put 'with-slots 'lisp-indent-function 2)
(put 'with-open-file 'lisp-indent-function 1)
(put 'with-open-stream 'lisp-indent-function 1)
(put 'eval-after-load 'lisp-indent-function 1)
(put 'display-message 'lisp-indent-function 1)
(put 'display-warning 'lisp-indent-function 1)
(put 'lmessage 'lisp-indent-function 2)
(put 'lwarn 'lisp-indent-function 2)
(put 'global-set-key 'lisp-indent-function 1)
(put 'print-unreadable-object 'lisp-indent-function 1)

(defun indent-sexp (&optional endpos)
  "Indent each line of the list starting just after point.
If optional arg ENDPOS is given, indent each line, stopping when
ENDPOS is encountered."
  (interactive)
  (let ((indent-stack (list nil))
	(next-depth 0)
	;; If ENDPOS is non-nil, use nil as STARTING-POINT
	;; so that calculate-lisp-indent will find the beginning of
	;; the defun we are in.
	;; If ENDPOS is nil, it is safe not to scan before point
	;; since every line we indent is more deeply nested than point is.
	(starting-point (if endpos nil (point)))
	(last-point (point))
	last-depth bol outer-loop-done inner-loop-done state this-indent)
    (or endpos
	;; Get error now if we don't have a complete sexp after point.
	(save-excursion (forward-sexp 1)))
    (save-excursion
      (setq outer-loop-done nil)
      (while (if endpos (< (point) endpos)
	       (not outer-loop-done))
	(setq last-depth next-depth
	      inner-loop-done nil)
	;; Parse this line so we can learn the state
	;; to indent the next line.
	;; This inner loop goes through only once
	;; unless a line ends inside a string.
	(while (and (not inner-loop-done)
		    (not (setq outer-loop-done (eobp))))
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq next-depth (car state))
	  ;; If the line contains a comment other than the sort
	  ;; that is indented like code,
	  ;; indent it now with indent-for-comment.
	  ;; Comments indented like code are right already.
	  ;; In any case clear the in-comment flag in the state
	  ;; because parse-partial-sexp never sees the newlines.
	  (if (car (nthcdr 4 state))
	      (progn (indent-for-comment)
		     (end-of-line)
		     (setcar (nthcdr 4 state) nil)))
	  ;; If this line ends inside a string,
	  ;; go straight to next line, remaining within the inner loop,
	  ;; and turn off the \-flag.
	  (if (car (nthcdr 3 state))
	      (progn
		(forward-line 1)
		(setcar (nthcdr 5 state) nil))
	    (setq inner-loop-done t)))
	(and endpos
	     (<= next-depth 0)
	     (progn
	       (setq indent-stack (append indent-stack
					  (make-list (- next-depth) nil))
		     last-depth (- last-depth next-depth)
		     next-depth 0)))
	(or outer-loop-done endpos
	    (setq outer-loop-done (<= next-depth 0)))
	(if outer-loop-done
	    (forward-line 1)
	  (while (> last-depth next-depth)
	    (setq indent-stack (cdr indent-stack)
		  last-depth (1- last-depth)))
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  last-depth (1+ last-depth)))
	  ;; Now go to the next line and indent it according
	  ;; to what we learned from parsing the previous one.
	  (forward-line 1)
	  (setq bol (point))
	  (skip-chars-forward " \t")
	  ;; But not if the line is blank, or just a comment
	  ;; (except for double-semi comments; indent them as usual).
	  (if (or (eobp) (looking-at "\\s<\\|\n"))
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		(setq this-indent (car indent-stack))
	      (let ((val (calculate-lisp-indent
			  (if (car indent-stack) (- (car indent-stack))
			    starting-point))))
		(if (integerp val)
		    (setcar indent-stack
			    (setq this-indent val))
		  (setcar indent-stack (- (car (cdr val))))
		  (setq this-indent (car val)))))
	    (if (/= (current-column) this-indent)
		(progn (delete-region bol (point))
		       (indent-to this-indent)))))
	(or outer-loop-done
	    (setq outer-loop-done (= (point) last-point))
	    (setq last-point (point)))))))

;; Indent every line whose first char is between START and END inclusive.
(defun lisp-indent-region (start end)
  (save-excursion
    (let ((endmark (copy-marker end)))
      (goto-char start)
      (and (bolp) (not (eolp))
	   (lisp-indent-line))
      (indent-sexp endmark)
      (set-marker endmark nil))))

;;;; Lisp paragraph filling commands.

(defun lisp-fill-paragraph (&optional justify)
  "Like \\[fill-paragraph], but handle Emacs Lisp comments.
If any of the current line is a comment, fill the comment or the
paragraph of it that point is in, preserving the comment's indentation
and initial semicolons."
  (interactive "P")
  (let (
	;; Non-nil if the current line contains a comment.
	has-comment

	;; Non-nil if the current line contains code and a comment.
	has-code-and-comment

	;; If has-comment, the appropriate fill-prefix for the comment.
	comment-fill-prefix
	)

    ;; Figure out what kind of comment we are looking at.
    (save-excursion
      (beginning-of-line)
      (cond

       ;; A line with nothing but a comment on it?
       ((looking-at "[ \t]*;[; \t]*")
	(setq has-comment t
	      comment-fill-prefix (buffer-substring (match-beginning 0)
						    (match-end 0))))

       ;; A line with some code, followed by a comment?  Remember that the
       ;; semi which starts the comment shouldn't be part of a string or
       ;; character.
       ;; XEmacs Try this the FSF and see if it works.
;       ((progn
;	  (while (not (looking-at ";\\|$"))
;	    (skip-chars-forward "^;\n\"\\\\?")
;	    (cond
;	     ((eq (char-after (point)) ?\\) (forward-char 2))
;	     ((memq (char-after (point)) '(?\" ??)) (forward-sexp 1))))
;	  (looking-at ";+[\t ]*"))
;	(setq has-comment t)
       ((condition-case nil
	    (save-restriction
	      (narrow-to-region (point-min)
				(save-excursion (end-of-line) (point)))
	      (while (not (looking-at ";\\|$"))
		(skip-chars-forward "^;\n\"\\\\?")
		(cond
		 ((eq (char-after (point)) ?\\) (forward-char 2))
		 ((memq (char-after (point)) '(?\" ??)) (forward-sexp 1))))
	      (looking-at ";+[\t ]*"))
	  (error nil))
	(setq has-comment t has-code-and-comment t)
	(setq comment-fill-prefix
	      (concat (make-string (/ (current-column) 8) ?\t)
		      (make-string (% (current-column) 8) ?\ )
		      (buffer-substring (match-beginning 0) (match-end 0)))))))

    (if (not has-comment)
	(fill-paragraph justify)

      ;; Narrow to include only the comment, and then fill the region.
      (save-excursion
	(save-restriction
	  (beginning-of-line)
	  (narrow-to-region
	   ;; Find the first line we should include in the region to fill.
	   (save-excursion
	     (while (and (zerop (forward-line -1))
			 (looking-at "^[ \t]*;")))
	     ;; We may have gone too far.  Go forward again.
	     (or (looking-at ".*;")
		 (forward-line 1))
	     (point))
	   ;; Find the beginning of the first line past the region to fill.
	   (save-excursion
	     (while (progn (forward-line 1)
			   (looking-at "^[ \t]*;")))
	     (point)))

	  ;; Lines with only semicolons on them can be paragraph boundaries.
	  (let* ((paragraph-start (concat paragraph-start "\\|[ \t;]*$"))
		 (paragraph-separate (concat paragraph-start "\\|[ \t;]*$"))
		 (paragraph-ignore-fill-prefix nil)
		 (fill-prefix comment-fill-prefix)
		 (after-line (if has-code-and-comment
				 (save-excursion
				   (forward-line 1) (point))))
		 (end (progn
			(forward-paragraph)
			(or (bolp) (newline 1))
			(point)))
		 ;; If this comment starts on a line with code,
		 ;; include that like in the filling.
		 (beg (progn (backward-paragraph)
			     (if (eq (point) after-line)
				 (forward-line -1))
			     (point))))
	    (fill-region-as-paragraph beg end
				      justify nil
				      (save-excursion
					(goto-char beg)
					(if (looking-at fill-prefix)
					    nil
					  (re-search-forward comment-start-skip)
					  (point))))))))
    t))

(defun indent-code-rigidly (start end arg &optional nochange-regexp)
  "Indent all lines of code, starting in the region, sideways by ARG columns.
Does not affect lines starting inside comments or strings, assuming that
the start of the region is not inside them.

Called from a program, takes args START, END, COLUMNS and NOCHANGE-REGEXP.
The last is a regexp which, if matched at the beginning of a line,
means don't indent that line."
  (interactive "r\np")
  (let (state)
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp)
	  (setq state (parse-partial-sexp (point)
					  (progn
					    (forward-line 1) (point))
					  nil nil state)))
      (while (< (point) end)
	(or (car (nthcdr 3 state))
	    (and nochange-regexp
		 (looking-at nochange-regexp))
	    ;; If line does not start in string, indent it
	    (let ((indent (current-indentation)))
	      (delete-region (point) (progn (skip-chars-forward " \t") (point)))
	      (or (eolp)
		  (indent-to (max 0 (+ indent arg)) 0))))
	(setq state (parse-partial-sexp (point)
					(progn
					  (forward-line 1) (point))
					nil nil state))))))

(provide 'lisp-mode)

;;; lisp-mode.el ends here
