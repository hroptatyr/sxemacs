;;; build-rpt.el --- Automatically formatted build reports for XEmacs

;; Copyright (C) 1997-2001 Adrian Aichner
;; Copyright (C) 2004 - 2007 Steve Youngs

;; Author: Adrian Aichner <adrian@xemacs.org>
;; Keywords: internal

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

;;; Synched up with: Not synched.

;;; Commentary:

;; The Idea:
;; Let SXEmacs report interesting aspects of how it was built.

;; The Concept:
;; User creates an SXEmacs Build Report by just calling
;; M-x build-rpt
;; which will initialise a mail buffer with relevant information
;; derived from the SXEmacs build process. Point is left at the
;; beginning of the report for user to input some personal notes and
;; send the report.

;; The Status:
;; This is the first `Proof of Concept'.

;; The Author:
;; Adrian Aichner, Teradyne GmbH Munich, Sun., Apr. 20, 1997.

;; First update for SXEmacs 2004-12-07 by Steve Youngs.

;; Renamed to build-rpt so it doesn't conflict with build-report.el in
;;   XEmacs package "build" -- 2006-01-21 by Steve Youngs.

;;; Code:

(require 'config)
(require 'custom)
(require 'cl)

;; gag the byte-compiler... we really do know what we are doing
(eval-when-compile
  (globally-declare-boundp
   '(gnus-newsgroup-name gnus-article-copy mail-header-separator))
  (globally-declare-fboundp
   '(gnus-alive-p gnus-slave gnus-slave-unplugged gnus-post-news
		  gnus-group-mail message-field-value message-goto-to
		  message-goto-subject message-goto-body user-mail-address
		  mail-send-and-exit)))


;;; Customization support for build-rpt starts here.

(defgroup build-rpt nil
  "Standardises the Creation of SXEmacs Build Rpts."
  :load 'build-rpt
  :group 'build)

(defcustom build-rpt-destination
  (list
   "SXEmacs Build Reports <sxemacs-builds@sxemacs.org>"
   "SXEmacs Devel <sxemacs-devel@sxemacs.org>"
   "Steve Youngs <steve@sxemacs.org>")
  "*The list of mail addresses SXEmacs Build Reports should most likely
go to."
  :type '(repeat
	  :custom-show t
	  :documentation-shown t
	  string)
  :group 'build-rpt)

(defcustom build-rpt-keep-regexp
  (list
   #r"^\(cd\|n?make\)\s-"
   "errors?"
   "warnings?"
   #r"pure.*\(space\|size\)"
   "hides\\b"
   "strange"
   "shadowings"
   #r"^Compil\(ing\s-+in\|ation\)"
   "^Using"
   "not\\s-+found"
   "^While\\s-+compiling.*\\(\n\\s-+.+\\)*"
   "^Note:"
   "Installing"
   "[Ff]ile(s) copied"
   "^[A-Za-z_]+="
   "\\s-+tests\\s-+"
   "^\\(real\\|user\\|sys\\)\\s-+[0-9]+m")
  "*Regexp of make process output lines to keep in the report."
  :type '(repeat
	  :custom-show t
	  :documentation-shown t
	  regexp)
  :group 'build-rpt)

(defcustom build-rpt-delete-regexp
  (list
   "confl.*with.*auto-inlining"
   "^Formatting:"
   "(100%) tests successful"
   "errors that should"
   "wrong-error")
  "*Regexp of make process output lines to delete from the report."
  :type '(repeat
	  :custom-show t
	  :documentation-shown t
	  regexp)
  :group 'build-rpt)

(defcustom build-rpt-make-output-dir (config-value 'sxe_blddir)
  "*Directory where the build report file is found."
  :type '(directory
	  :custom-show t
	  :documentation-shown t)
  :group 'build-rpt)

(defcustom build-rpt-make-output-files
  (list
   ",,vars.out"
   ",,beta.out"
   ",,make-all.out"
   ",,make-check-temacs.out"
   ",,make-check.out"
   ",,make-install.out")
  "*List of Filenames where stdout and stderr of SXEmacs make process
have been stored.  These are relative to
`build-rpt-make-output-dir`.  You'll have to run make with output
redirection, like so...

  $ make 2>&1 | tee ,,make-all.out
  $ make check 2>&1 | tee ,,make-check.out
"
  :type '(repeat
	  :custom-show t
	  :documentation-shown t
	  file)
  :group 'build-rpt)

(defcustom build-rpt-installation-file
  (expand-file-name "Installation"
		    (gethash 'sxe_blddir (config-value-hash-table)))
  "*Installation file produced by SXEmacs configure process."
  :type '(file
	  :custom-show t
	  :documentation-shown t)
  :group 'build-rpt)

(defcustom build-rpt-version-file
  (expand-file-name
   "config.h"
   (file-name-as-directory
    (paths-construct-path
     (list (gethash 'sxe_blddir (config-value-hash-table))
	   "src"))))
  "*File containing version info."
  :type '(file
	  :custom-show t
	  :documentation-shown t)
  :group 'build-rpt)

(defcustom build-rpt-subject
  (concat "[%s] "
	  (gethash 'SXEMACS_GIT_VERSION (config-value-hash-table))
	  " on " system-configuration)
  "*SXEmacs Build Report Subject Line. %s-sequences will be substituted
  with user input through `build-rpt' according to
  `build-rpt-prompts' using `format'."
  :type '(string
	  :custom-show t
	  :documentation-shown t)
  :group 'build-rpt)

(defcustom build-rpt-prompts
  (quote (("Status?: "  ("Success" "Tests fail" "Failure" "Vanilla problems" "Problems"))))
  "*SXEmacs Build Report Prompt(s). This is a list of prompt-string
  lists used by `build-rpt' in conjunction with
  `build-rpt-subject'. Each list consists of a prompt string
  followed by any number of strings which can be chosen via the history
  mechanism."
  :type '(repeat
	  :custom-show t
	  :documentation-shown t
	  (list
	   :tag "Prompt"
	   string
	   (repeat
	    :tag "Values"
	    string)))
  :group 'build-rpt)

(defcustom build-rpt-file-encoding
  "7bit"
  "*SXEmacs Build Report File Encoding to be used when MIME support is
  available."
  :group 'build-rpt)

(defcustom build-rpt-use-gnus-p nil
  "*Whether the SXEmacs Build Report should be set up by Gnus.
Note that Gnus has to be fully started, i.e. there has to be a
*Group* buffer"
  :group 'build-rpt
  :type 'boolean)

(defcustom build-rpt-use-gnus-group ""
  "*Name of a group in the group buffer to send the build report from.
This is useful if there are customised settings along with the group."
  :type 'string
  :group 'build-rpt)

(defcustom build-rpt-use-gnus-plugged t
  "*When non-nil, start Gnus in \"plugged\" mode if it isn't running."
  :type 'boolean
  :group 'build-rpt)

;; Symbol Name mappings from TM to SEMI serving as Compatibility
;; Bandaid
(when (featurep 'mime-setup)
  ;; No (defvaralias ...) so far. Thanks to "Didier Verna"
  ;; <didier@xemacs.org> for reporting my incorrect defvaraliasing of
  ;; `mime-editor/insert-tag'.
  ;; Thanks to Jens-Ulrik Holger Petersen
  ;; <petersen@kurims.kyoto-u.ac.jp> for suggesting the conditional
  ;; aliasing of SEMI functions.
  (unless (fboundp 'mime-edit-content-beginning)
    (defalias 'mime-edit-content-beginning 'mime-editor/content-beginning))
  (unless (fboundp 'mime-edit-insert-tag)
    (defalias 'mime-edit-insert-tag 'mime-editor/insert-tag))
  (unless (fboundp 'mime-edit-insert-binary-file)
    (defalias 'mime-edit-insert-binary-file
      'mime-editor/insert-binary-file)))

(defun build-rpt-make-output-get ()
  "Returns the filename the SXEmacs make output is saved in."
  (interactive)
  (if (or (string-equal build-rpt-make-output-dir "")
	  (null build-rpt-make-output-dir))
      (mapcar
       (function
	(lambda (f)
	  (expand-file-name
	   f
	   (file-name-as-directory
	    (gethash 'sxe_blddir (config-value-hash-table))))))
       build-rpt-make-output-files)
    (mapcar
     (function
      (lambda (f)
	(expand-file-name
	 f
	 (file-name-as-directory build-rpt-make-output-dir))))
     build-rpt-make-output-files)))

(defun build-rpt-read-destination ()
  (if (listp build-rpt-destination)
      (read-string
       "Build Report Destination: "
       (car build-rpt-destination)
       'build-rpt-destination)
    (read-string
     "Build Report Destination: "
     build-rpt-destination)))

(defvar build-rpt-interactive nil
  "Flag used signal when build report is being called interactively
and as such the user should be the one sending the email.
When nil the build report is sent as soon as it is built.")

(defvar build-rpt-email (if (listp build-rpt-destination)
			    (car build-rpt-destination)
			  build-rpt-destination)
  "The destination for the current build-report")



;;;###autoload
(defun send-build-rpt (&rest args)
  "Send report build information including Installation and make output.

Uses first argument for status.  Then uses
`compose-mail' to create a mail message.  The Subject header contains
status and version information.  Point is left at the beginning of the
mail text.  Add some notes if you like, and send the report.

Looks for Installation and the make output file (see
`build-rpt-make-output-files') in the build directory of the
running SXEmacs by default (customisable via
`build-rpt-make-output-dir').  The output from make is filtered
through `build-rpt-keep-regexp' and `build-rpt-delete-regexp'
before including in the message.

See also `mail-user-agent', `build-rpt-email', and
`build-rpt-installation-file'."
  (let ((user-mail-address (if user-mail-address
			       user-mail-address
			     (if (and (interactive-p)
				      (featurep 'sendmail))
				 (user-mail-address)
			       (concat (user-real-login-name)
				       "-notconfigured@"
				       (if mail-host-address
					   mail-host-address
					 "localhost"))))))
  (save-excursion
    (if (and build-rpt-use-gnus-p
	     (featurep 'gnus))
	(progn
	  (unless (gnus-alive-p)
	    (if build-rpt-use-gnus-plugged
		;; use slave in case there is a Gnus running in another
		;; SXEmacs process
		(gnus-slave)
	      (gnus-slave-unplugged)))
	  (if (not (string= "" build-rpt-use-gnus-group))
	      (let ((group gnus-newsgroup-name)
		    (gnus-article-copy))
		(setq gnus-newsgroup-name build-rpt-use-gnus-group)
		(gnus-post-news "" build-rpt-use-gnus-group)
		(setq gnus-newsgroup-name group))
	    (gnus-group-mail 1))
	  (unless (message-field-value "to")
	    (message-goto-to)
	    (insert build-rpt-email))
	  (message-goto-subject)
	  (insert (apply #'format build-rpt-subject args))
	  (message-goto-body))
      (if (featurep 'sendmail)
	  (progn
	    (compose-mail
	     build-rpt-email
	     (apply 'format build-rpt-subject args)
	     nil
	     nil
	     nil
	     nil
	     nil)
	    (goto-char (point-max))
	    (re-search-backward mail-header-separator)
	    (next-line 1))
	(pop-to-buffer "*build-rpt*")
	(insert (format (concat "Please save this buffer to a file and email it\n"
				"Or, alternatively, rerun `M-x build-rpt' after installing the\n"
				"\"mail-lib\" XEmacs package.\n\n"
				"To: SXEmacs Builds <sxemacs-builds@sxemacs.org>\n"
				"Subject: %s\n\n")
			(apply #'format build-rpt-subject args)))))
    (let* ((rpt-begin (point))
	   (files (reverse (build-rpt-make-output-get)))
	   (file (car files)))
      (while file
	(if (file-exists-p file)
	    (insert (build-rpt-insert-make-output rpt-begin file))
	  (insert (format "%s not found!\n" file)))
	(insert "\n")
	(setq files (cdr files))
	(setq file (car files)))
      (insert (build-rpt-insert-config-values rpt-begin))
      (insert "\n")
      (insert (build-rpt-insert-ldd rpt-begin))
      (insert "\n")
      (if (file-exists-p build-rpt-installation-file)
	  (insert (build-rpt-insert-installation-file rpt-begin))
	(insert (format "%s not found!\n" build-rpt-installation-file)))
      (insert "\n")
      (insert (build-rpt-insert-header rpt-begin))
      (if build-rpt-interactive
	  (goto-char rpt-begin)
	(mail-send-and-exit t))))))

;;;###autoload
(defun build-rpt (&rest args)
  "Report build information including Installation and make output.

Prompts for status (usually \"Success\" or \"Failure\").  Then uses
`compose-mail' to create a mail message.  The Subject header contains
status and version information.  Point is left at the beginning of the
mail text.  Add some notes if you like, and send the report.

Looks for Installation and the make output file (see
`build-rpt-make-output-files') in the build directory of the
running SXEmacs by default (customisable via
`build-rpt-make-output-dir').  The output from make is filtered
through `build-rpt-keep-regexp' and `build-rpt-delete-regexp'
before including in the message.

See also `mail-user-agent', `build-rpt-destination', and
`build-rpt-installation-file'."
  ;; `interactive' form returns value for formal parameter `args'.
  (interactive
   (let (prompt
	 hist
	 arg
	 (prompts build-rpt-prompts))
     (progn
       (while prompts
	 (defvar hist)
	 (setq prompt (caar prompts))
	 (setq hist (cdar prompts))
	 ;; `build-rpt-prompts' used to be a list of lists, the
	 ;; first element of each list being the prompt, the rest being
	 ;; the history.  The history is now in a separate list.  We
	 ;; better check for that.
	 (if (listp (car hist))
	     (setq hist (car hist)))
	 (setq prompts (cdr prompts))
	 (setq arg (cons (read-string prompt "" 'hist) arg)))
       arg)))
  (let ((build-rpt-email (build-rpt-read-destination))
	(build-rpt-interactive (interactive-p)))
    (apply 'send-build-rpt args)))

(defun build-rpt-insert-header (where)
  "Inserts the build-rpt-header at the point specified by `where'."
  (goto-char where)
  (with-temp-buffer
    (insert
     (format "
> SXEmacs Build Report generated by emacs-version
> %s
> with system-configuration
> %s
> follows:\n\n" emacs-version system-configuration))
    (buffer-string)))

(defun build-rpt-insert-make-output (where file)
  "Inserts the output of the SXEmacs Beta make run in the
current buffer at position WHERE.
The make process output must have been saved in
`build-rpt-make-output-files' during the SXEmacs Beta building."
  (goto-char where)
  (with-temp-buffer
    (if (file-exists-p file)
	(progn
	  (if (featurep 'mime-setup)
	      (progn
		(mime-edit-insert-tag
		 "text"
		 "plain"
		 (concat
		  "\nContent-Disposition: attachment;"
		  " filename=\""
		  (file-name-nondirectory
		   file)
		  "\""))
		(mime-edit-insert-binary-file
		 file
		 build-rpt-file-encoding))
	    (insert-file-contents file))
	  (when build-rpt-keep-regexp
	    (goto-char (point-min))
	    (delete-non-matching-lines (build-rpt-keep)))
	  (when build-rpt-delete-regexp
	    (goto-char (point-min))
	    (delete-matching-lines (build-rpt-delete)))
	  (goto-char (point-min))
	  (insert "\n")
	  (insert
	   (format "> Contents of %s\n" file)))
      (insert "> " file
	      " does not exist!\n\n"))
    (buffer-string)))

(defun build-rpt-insert-installation-file (where)
  "Inserts the contents of the `build-rpt-installation-file'
created by the SXEmacs Beta configure process."
  (goto-char where)
  (with-temp-buffer
    (if (file-exists-p build-rpt-installation-file)
	(progn
	  (insert "> Contents of "
		  build-rpt-installation-file
		  ":\n")
	  (insert "> (Output from ./configure)\n\n")
	  (if (featurep 'mime-setup)
	      (progn
		(mime-edit-insert-tag
		 "text"
		 "plain"
		 (concat
		  "\nContent-Disposition: attachment;"
		  " filename=\""
		  (file-name-nondirectory
		   build-rpt-installation-file)
		  "\""))
		(mime-edit-insert-binary-file
		 build-rpt-installation-file
		 build-rpt-file-encoding))
	    (insert-file-contents
	     build-rpt-installation-file)))
      (insert "> " build-rpt-installation-file
	      " does not exist!\n\n"))
    (buffer-string)))

(defun build-rpt-insert-config-values (where)
  "Inserts the contents of the `config-value-hash-table'.
created by the SXEmacs Beta configure process."
  (goto-char where)
  (with-temp-buffer
    (if (null (config-value-hash-table))
	(insert "> `config-value-hash-table' is empty, which is weird :(!\n\n")
	(progn
	  (insert "> Contents of `config-value-hash-table':\n")
	  (let ((curp (point))
		value-empty)
	    (maphash
	     #'(lambda (key value)
		 (if (and (stringp value)
			  (string= "" value))
		     (setq value-empty (cons key value-empty))
		   (insert (format "%s %S\n" key value))))
	     (config-value-hash-table))
	    (goto-char curp)
	    ;; we are at `curp' again
	    (insert (format "Empty keys: %s\n\n"
			    (mapconcat #'prin1-to-string
				       value-empty "  "))))))
    (buffer-string)))

(defun build-rpt-insert-ldd (where)
  "Inserts the output of the shell command ldd sxemacs."
  (goto-char where)
  (with-temp-buffer
    (let ((running-binary-tests
	   '((lambda ()
	       (expand-file-name (car command-line-args)
				 command-line-default-directory))
	     (lambda ()
	       (locate-file (car command-line-args)
			    (split-string (getenv "PATH") ":")))))
	   (running-binary)
	   (ldd
	    (let ((sysconfl (split-string system-configuration "-")))
	      (cond
	       ((member "apple" sysconfl)
		"otool -XL")
	       (t
		"ldd")))))

      ;; perform binary finder tests
      (while (and (null running-binary) (car-safe running-binary-tests))
	(let ((candidate (funcall (car running-binary-tests))))
	  (setq running-binary
		(and candidate
		     (file-exists-p candidate)
		     candidate)
		running-binary-tests (cdr running-binary-tests))))

      (if (null running-binary)
	  (insert "cannot obtain ld-dependencies.\n")
	(insert (shell-command-to-string (concat ldd " " running-binary)))
	(goto-char (point-min))
	(while (re-search-forward "^\\s-+" nil t)
	  (replace-match "")))
      (goto-char (point-min))
      (insert "> shared library dependencies:\n"))
    (buffer-string)))

(defun build-rpt-keep ()
  "Concatenate elements of `build-rpt-keep-regexp' and a general
MIME tag REGEXP.  The result is a REGEXP string matching either of the
REGEXPs in `build-rpt-keep-regexp' or a general MIME tag REGEXP."
  (mapconcat #'identity
	     (cons #r"^--\[\[\|\]\]$" build-rpt-keep-regexp) "\\|"))

(defun build-rpt-delete ()
  "Concatenate elements of `build-rpt-delete-regexp' and a general
MIME tag REGEXP.  The result is a REGEXP string matching either of the
REGEXPs in `build-rpt-delete-regexp' or a general MIME tag REGEXP."
  (mapconcat '(lambda (item) item)
	     build-rpt-delete-regexp "\\|"))

;; To ensure we always get the right build reporter, alias the
;; incompatible one to ours if it is ever loaded.
;;;###autoload
(eval-after-load "build-report"
  (defalias 'build-report 'build-rpt))

(provide 'build-rpt)
;;; build-rpt.el ends here
