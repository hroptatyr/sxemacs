;; issue-tracker.el --- SXEmacs Bug Reporting/Tracking

;; Copyright (C) 2005 Steve Youngs

;; Author:        Steve Youngs <steve@sxemacs.org>
;; Maintainer:    Steve Youngs <steve@sxemacs.org>
;; Created:       <2005-01-10>
;; Homepage:      http://www.sxemacs.org/
;; Keywords:      bugs issues

;; This file is part of SXEmacs.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the author nor the names of any contributors
;;    may be used to endorse or promote products derived from this
;;    software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;; 
;;    This will hopefully turn into an interface to the SXEmacs issue
;;    tracker.  For now, it is basically a clone of xemacsbug with a
;;    couple of things changed to suit SXEmacs conditions.

;;; Todo:
;;
;;    Interface to our issue tracker.

;;; Code:
(require 'shadow)

(eval-when-compile
  (defvar mh-before-send-letter-hook))

(defgroup sxemacsbug nil
  "Sending SXEmacs bug reports."
  :group 'maint
  :group 'mail)

;; >> These should be addresses which are accessible to your machine,
;; >> otherwise you can't use this file.  It will only work on the
;; >> internet with this address.

(defcustom report-sxemacs-bug-address "SXEmacs Devel <sxemacs-devel@sxemacs.org>"
  "*Address of mailing list for SXEmacs bugs."
  :group 'sxemacsbug
  :type 'string)

(defcustom report-sxemacs-bug-extra-headers nil
  "*An alist of mail-header value pairs for SXEmacs bugs.

It takes the format (HEADER . VALUE) where both HEADER and VALUE are
strings. See `compose-mail'."
  :group 'sxemacsbug
  :type '(repeat 
	  (cons (string :tag "Header") 
		(string :tag "Value"))))

(defcustom report-sxemacs-bug-beta-address "SXEmacs Devel <sxemacs-devel@sxemacs.org>"
  "*Address of mailing list for SXEmacs beta bugs."
  :group 'sxemacsbug
  :type 'string)

(defcustom report-sxemacs-bug-beta-extra-headers nil
  "*An alist of mail-header value pairs for SXEmacs beta bugs.

It takes the format (HEADER . VALUE) where both HEADER and VALUE are
strings. See `compose-mail'."
  :group 'sxemacsbug
  :type '(repeat 
	  (cons (string :tag "Header") 
		(string :tag "Value"))))

(defvar report-sxemacs-bug-orig-text nil
  "The automatically-created initial text of bug report.")

(defcustom report-sxemacs-bug-no-confirmation nil
  "*If non-nil, suppress the confirmations asked for the sake of novice users."
  :group 'sxemacsbug
  :type 'boolean)

(defcustom report-sxemacs-bug-no-explanations nil
  "*If non-nil, suppress the explanations given for the sake of novice users."
  :group 'sxemacsbug
  :type 'boolean)

(defcustom report-sxemacs-bug-send-init nil
  "*If non-nil, include the user's init.el file in the bug report."
  :group 'sxemacsbug
  :type 'boolean)

(defconst report-sxemacs-bug-help
"\nThis bug report will be sent to the SXEmacs Development Team,
not to your local site managers!!

Please write in English, because the SXEmacs maintainers do not have
translators to read other languages for them.

Please describe as succinctly as possible:
\t- What happened.
\t- What you thought should have happened.
\t- Precisely what you were doing at the time.

Also include a reliable recipe for triggering the bug, as well as
any C and lisp back-traces that you may have.
\(setq stack-trace-on-error t\), or \(setq debug-on-error t\) if you
are familiar with the debugger, to get a lisp back-trace.
To get a core file for the C back-trace on a GNU/Linux system do 
'ulimit -c unlimited' in the shell prior to starting SXEmacs.

Type \\[report-sxemacs-bug-info] to visit in Info the SXEmacs Manual section
about when and how to write a bug report,
and what information to supply so that the bug can be fixed.
Type SPC to scroll through this section and its subsections.

You are very welcome to scan through the bug report and remove any
potentially sensitive data.

Turn off this help buffer permanently by adding:

\t \(setq report-sxemacs-bug-no-explanations t\)

To your ~/.sxemacs/init.el")

(defun report-sxemacs-bug-help ()
  "Display the help buffer for `report-sxemacs-bug'."
  (with-electric-help
   '(lambda () 
      (define-key (current-local-map) "\C-c\C-i" 'report-sxemacs-bug-info)
      (princ (substitute-command-keys report-sxemacs-bug-help)) nil) "*Bug Help*"))

(defun report-sxemacs-bug-packages-list ()
  "Insert into the current buffer a list of installed packages."
  (let ((pkgs packages-package-list))
    (while pkgs
      (insert
       (format "(%s ver: %s upstream: %s)\n"
	       (nth 0 (car pkgs))
	       (nth 2 (car pkgs))
	       (nth 4 (car pkgs))))
      (setq pkgs (cdr pkgs)))))

;;;###autoload
(defun report-sxemacs-bug (topic &optional recent-keys)
  "Report a bug in SXEmacs.
Prompts for bug subject.  Leaves you in a mail buffer."
  ;; This strange form ensures that (recent-keys) is the value before
  ;; the bug subject string is read.
  (interactive (reverse (list (recent-keys) (read-string "Bug Subject: "))))
  (let (user-point)
    (setq topic (concat "[Bug: " emacs-program-version "] " topic))
    (if sxemacs-betaname
	  (compose-mail report-sxemacs-bug-beta-address
			topic
			report-sxemacs-bug-beta-extra-headers)
      (compose-mail report-sxemacs-bug-address
		    topic
		    report-sxemacs-bug-extra-headers))
    ;; The rest of this does not execute
    ;; if the user was asked to confirm and said no.
    (goto-char (point-min))
    (re-search-forward "^--text follows this line--$")
    (forward-line 1)
    (insert "================================================================\n")
    (insert "Dear Bug Team!\n\n")
    (setq user-point (point))
    (insert "\n\n================================================================\n
System Info to help track down your bug:
---------------------------------------\n\n")
    ;; Insert the output of 'describe-installation'.
    (insert (symbol-value 'Installation-string))
    ;; Load-path shadows can cause some grief.
    (flet ((append-message
	     (&rest args) ())
	   (clear-message
	     (&optional label frame stdout-p no-restore)
	     ()))
      (insert "\n\nLoad-Path Lisp Shadows:\n"
	      "----------------------\n")
      (let ((before-shadows (point)))
	(insert
	  (format "%s"
		  (find-emacs-lisp-shadows load-path)))
	(save-restriction
	  (narrow-to-region before-shadows (point))
	  (fill-paragraph t)
	  (insert "\n"))))
    ;; Insert a list of installed packages.
    (insert "\n\nInstalled SXEmacs Packages:\n"
	    "--------------------------\n")
    (report-sxemacs-bug-packages-list)
    (insert "\n")
    ;; Insert a list of installed modules.
    (if (fboundp 'list-modules)
	(progn
	  (insert "\n\nInstalled Modules:\n"
		  "-----------------\n")
	    (let* ((mods (list-modules)))
	      (while mods
		(cl-prettyprint (cdr (car mods)))
		(setq mods (cdr mods))))))
    ;; Insert a list of loaded features
    (let ((before-features (point)))
      (insert
       (format "\n\nFeatures:\n--------\n\n%s" (symbol-value 'features)))
      (save-restriction
	(narrow-to-region before-features (point))
	(fill-paragraph t)
	(insert "\n")))
    ;; Insert recent keystrokes.
    (insert "\n\n"
	    "Recent keystrokes:\n-----------------\n\n")
    (let ((before-keys (point)))
      (insert (key-description recent-keys))
      (save-restriction
	(narrow-to-region before-keys (point))
	(goto-char before-keys)
	(while (progn (move-to-column 50) (not (eobp)))
	  (search-forward " " nil t)
	  (insert "\n"))))
    ;; Insert recent minibuffer messages.
    (insert "\n\n\nRecent messages (most recent first):\n"
	    "-----------------------------------\n")
    (let ((standard-output (current-buffer)))
      (print-recent-messages 20)
      (insert "\n"))
    ;; Insert the contents of the user's init file if it exists.
    (if report-sxemacs-bug-send-init
      (if (file-readable-p user-init-file)
	  (save-excursion
	    (goto-char (point-max))
	    (beginning-of-line)
	    (insert "\n\nUser Init File:\n--------------\n\n")
	    (insert-file-contents user-init-file))))
    ;; This is so the user has to type something
    ;; in order to send easily.
    (use-local-map (let ((map (make-sparse-keymap)))
		     (set-keymap-parents map (current-local-map))
		     map))
    (define-key (current-local-map) "\C-c\C-i" 'report-sxemacs-bug-info)
    ;; Make it less likely people will send empty messages.
    (cond
     ((eq mail-user-agent 'sendmail-user-agent)
      (make-local-variable 'mail-send-hook)
      (add-hook 'mail-send-hook 'report-sxemacs-bug-hook))
     ((eq mail-user-agent 'message-user-agent)
      (make-local-variable 'message-send-hook)
      (add-hook 'message-send-hook 'report-sxemacs-bug-hook))
     ((eq mail-user-agent 'mh-e-user-agent)
      (make-local-variable 'mh-before-send-letter-hook)
      (add-hook 'mh-before-send-letter-hook 'report-sxemacs-bug-hook))
     (t
      (make-local-variable 'mail-send-hook)
      (add-hook 'mail-send-hook 'report-sxemacs-bug-hook)))
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (make-local-variable 'report-sxemacs-bug-orig-text)
      (setq report-sxemacs-bug-orig-text (buffer-substring (point-min) (point))))
    (goto-char user-point))
  (delete-other-windows)
  (unless report-sxemacs-bug-no-explanations
    (report-sxemacs-bug-help)
    (cond
     ((eq mail-user-agent 'sendmail-user-agent)
      (message (substitute-command-keys
		"Type \\[mail-send-and-exit] to send the bug report, \\[kill-buffer] to cancel.")))
     ((eq mail-user-agent 'message-user-agent)
      (message (substitute-command-keys
		"Type \\[message-send-and-exit] to send the bug report, \\[kill-buffer] to cancel.")))
     ((eq mail-user-agent 'mh-e-user-agent)
      (message (substitute-command-keys
		"Type \\[mh-send-letter] to send the bug report, \\[kill-buffer] to cancel.")))
     (t
      (message (substitute-command-keys
		"Type \\[mail-send-and-exit] to send the bug report, \\[kill-buffer] to cancel."))))))

(defun report-sxemacs-bug-info ()
  "Go to the Info node on reporting SXEmacs bugs."
  (interactive)
  (Info-goto-node "(xemacs)Bugs"))

(defun report-sxemacs-bug-hook ()
  "Hook run before sending a bug report."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (if (and (= (- (point) (point-min))
		(length report-sxemacs-bug-orig-text))
	     (equal (buffer-substring (point-min) (point))
		    report-sxemacs-bug-orig-text))
	(error "No text entered in bug report"))

    ;; The last warning for novice users.
    (if (or report-sxemacs-bug-no-confirmation
	    (yes-or-no-p
	     "Send this bug report to the SXEmacs maintainers? "))
	;; Just send the current mail.
	nil
      (goto-char (point-min))
      (let* ((top (point)))
	(re-search-forward "^--text follows this line--$")
	(save-restriction
	  (narrow-to-region top (point))
	  (goto-char (point-min))
	  (if (re-search-forward "^To: " (eobp) t)
	      (let ((pos (point)))
		(end-of-line)
		(delete-region pos (point))))
	  (goto-char (point-min))
	  (if (re-search-forward "^Cc: " (eobp) t)
	      (let ((pos (point)))
		(end-of-line)
		(delete-region pos (point))))))
      (cond
       ((eq mail-user-agent 'sendmail-user-agent)
	(kill-local-variable 'mail-send-hook))
       ((eq mail-user-agent 'message-user-agent)
	(kill-local-variable 'message-send-hook))
       ((eq mail-user-agent 'mh-e-user-agent)
	(kill-local-variable 'mh-before-send-letter-hook))
       (t
	(kill-local-variable 'mail-send-hook)))
      (unless report-sxemacs-bug-no-explanations
	(with-electric-help
	 '(lambda ()
	    (insert "\n
You invoked the command M-x report-sxemacs-bug,
but you decided not to mail the bug report to the SXEmacs maintainers.

If you want to mail it to someone else instead,
please insert the proper e-mail address after \"To: \",
and send the mail again.") nil) "*Bug Help*"))
      (error "Sending Bug Report Cancelled"))))

;;;###autoload
(ignore-errors
  (load "xemacsbug" nil 'nomessage)
  (eval-after-load "xemacsbug"
    (defalias 'report-xemacs-bug 'report-sxemacs-bug)
    (defalias 'report-emacs-bug 'report-sxemacs-bug)))

(provide 'issue-tracker)
;;; issue-tracker.el ends here
