;;; build-report.el --- Automatically formatted build reports for XEmacs

;; Copyright (C) 1997-2001 Adrian Aichner
;; Copyright (C) 2004 Steve Youngs

;; Author: Adrian Aichner <adrian@xemacs.org>
;; Keywords: internal

;; This file is part of SXEmacs.

;; SXEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; SXEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with SXEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not synched.

;;; Commentary:

;; The Idea:
;; Let SXEmacs report interesting aspects of how it was built.

;; The Concept:
;; User creates an XEmacs Build Report by just calling
;; M-x build-report
;; which will initialize a mail buffer with relevant information
;; derived from the XEmacs build process. Point is left at the
;; beginning of the report for user to input some personal notes and
;; send the report.

;; The Status:
;; This is the first `Proof of Concept'.

;; The Author:
;; Adrian Aichner, Teradyne GmbH Munich, Sun., Apr. 20, 1997.

;; First update for SXEmacs 2004-12-07 by Steve Youngs.

;;; Code:

(require 'config)
(require 'custom)
(require 'cl)
(provide 'build-report)

;;; Customization support for build-report starts here.

(defgroup build-report nil
  "Standardizes the Creation of XEmacs Build Reports."
  :load 'build-report
  :group 'build)

(defcustom build-report-destination
  (list
   "SXEmacs Devel <sxemacs-devel@sxemacs.org>"
   "Steve Youngs <steve@sxemacs.org>")
  "*The list of mail addresses SXEmacs Build Reports should most likely
go to."
  :type '(repeat
          :custom-show t
          :documentation-shown t
          string)
  :group 'build-report)

(defcustom build-report-keep-regexp
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
   "\\s-+tests\\s-+")
  "*Regexp of make process output lines to keep in the report."
  :type '(repeat
          :custom-show t
          :documentation-shown t
          regexp)
  :group 'build-report)

(defcustom build-report-delete-regexp
  (list
   "confl.*with.*auto-inlining"
   "^Formatting:"
   "(100%) tests successful")
  "*Regexp of make process output lines to delete from the report."
  :type '(repeat
          :custom-show t
          :documentation-shown t
          regexp)
  :group 'build-report)

(defcustom build-report-make-output-dir
  (gethash 'blddir (config-value-hash-table))
  "*Directory where the build report file is found.
  If this is empty or nil, the default, it is replaced by the value of
  the SXEmacs build directory."
  :type '(directory
          :custom-show t
          :documentation-shown t)
  :group 'build-report)

(defcustom build-report-make-output-files
  (list
   ",,beta.out"
   ",,make-all.out" 
   ",,make-check-temacs.out"
   ",,make-check.out"
   ",,make-install.out")
  "*List of Filenames where stdout and stderr of SXEmacs make process
have been stored.  These are relative to
`build-report-make-output-dir`.  You'll have to run make with output
redirection, like so...

  $ make 2>&1 | tee ,,make-all.out
  $ make check 2>&1 | tee ,,make-check.out
"
  :type '(repeat
          :custom-show t
          :documentation-shown t
          file)
  :group 'build-report)

(defcustom build-report-installation-file
  (expand-file-name "Installation"
                    (gethash 'blddir (config-value-hash-table)))
  "*Installation file produced by SXEmacs configure process."
  :type '(file
          :custom-show t
          :documentation-shown t)
  :group 'build-report)

(defcustom build-report-version-file
  (expand-file-name
   "config.h"
   (file-name-as-directory
    (paths-construct-path
     (list (gethash 'top_srcdir (config-value-hash-table))
	   "src"))))
  "*File containing version info."
  :type '(file
          :custom-show t
          :documentation-shown t)
  :group 'build-report)

(defcustom build-report-subject
  (concat "[%s] "
	  (gethash 'SXEMACS_ARCH_VERSION (config-value-hash-table))
	  " on " system-configuration)
  "*SXEmacs Build Report Subject Line. %s-sequences will be substituted
  with user input through `build-report' according to
  `build-report-prompts' using `format'."
  :type '(string
          :custom-show t
          :documentation-shown t)
  :group 'build-report)

(defcustom build-report-prompts
  (quote (("Status?: "  ("Success" "Failure"))))
  "*SXEmacs Build Report Prompt(s). This is a list of prompt-string
  lists used by `build-report' in conjunction with
  `build-report-subject'. Each list consists of a prompt string
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
  :group 'build-report)

(defcustom build-report-file-encoding
  "7bit"
  "*SXEmacs Build Report File Encoding to be used when MIME support is
  available."
  :group 'build-report)

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

(defun build-report-make-output-get ()
  "Returns the filename the SXEmacs make output is saved in."
  (interactive)
  (if (or (string-equal build-report-make-output-dir "")
          (null build-report-make-output-dir))
      (mapcar
       (function
        (lambda (f)
          (expand-file-name
           f
           (file-name-as-directory
            (gethash 'blddir (config-value-hash-table))))))
       build-report-make-output-files)
    (mapcar
     (function
      (lambda (f)
        (expand-file-name
         f
         (file-name-as-directory build-report-make-output-dir))))
     build-report-make-output-files)))

;;;###autoload
(defun build-report (&rest args)
  "Report build information including Installation and make output.

Prompts for status (usually \"Success\" or \"Failure\").  Then uses
`compose-mail' to create a mail message.  The Subject header contains
status and version information.  Point is left at the beginning of the
mail text.  Add some notes if you like, and send the report.

Looks for Installation and the make output file (see
`build-report-make-output-files') in the build directory of the
running SXEmacs by default (customisable via
`build-report-make-output-dir').  The output from make is filtered
through `build-report-keep-regexp' and `build-report-delete-regexp'
before including in the message.

See also `mail-user-agent', `build-report-destination', and
`build-report-installation-file'."
  ;; `interactive' form returns value for formal parameter `args'.
  (interactive
   (let (prompt
         hist
         arg
         (prompts build-report-prompts))
     (progn
       (while prompts
         (defvar hist)
         (setq prompt (caar prompts))
         (setq hist (cdar prompts))
         ;; `build-report-prompts' used to be a list of lists, the
         ;; first element of each list being the prompt, the rest being
         ;; the history.  The history is now in a separate list.  We
         ;; better check for that.
         (if (listp (car hist))
             (setq hist (car hist)))
         (setq prompts (cdr prompts))
         (setq arg (cons (read-string prompt "" 'hist) arg)))
       arg)))
  (save-excursion
    (compose-mail
     ;; `build-report-destination' used to be a single string, so
     ;; let's test if we really get a list of destinations.
     (if (listp build-report-destination)
         (read-string
          "Build Report Destination: "
          (car build-report-destination)
          'build-report-destination)
       (read-string
        "Build Report Destination: "
        build-report-destination)
       )
     (apply 'format build-report-subject args)
     nil
     nil
     nil
     nil
     nil)
    (let* ((report-begin (point))
           (files (reverse (build-report-make-output-get)))
           (file (car files)))
      (while file
        (if (file-exists-p file)
            (insert (build-report-insert-make-output report-begin file))
          (insert (format "%s not found!\n" file)))
        (insert "\n")
        (setq files (cdr files))
        (setq file (car files)))
      (if (file-exists-p build-report-installation-file)
          (insert (build-report-insert-installation-file report-begin))
        (insert (format "%s not found!\n" build-report-installation-file)))
      (insert "\n")
      (insert (build-report-insert-header report-begin))
      (goto-char report-begin))))

(defun build-report-insert-header (where)
  "Inserts the build-report-header at the point specified by `where'."
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

(defun build-report-insert-make-output (where file)
  "Inserts the output of the SXEmacs Beta make run in the
current buffer at position WHERE.
The make process output must have been saved in
`build-report-make-output-files' during the SXEmacs Beta building."
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
                 build-report-file-encoding))
            (insert-file-contents file))
          (when build-report-keep-regexp
            (goto-char (point-min))
            (delete-non-matching-lines (build-report-keep)))
          (when build-report-delete-regexp
            (goto-char (point-min))
            (delete-matching-lines (build-report-delete)))
          (goto-char (point-min))
          (insert "\n")
          (insert
           (format "> Contents of %s\n" file)))
      (insert "> " file
              " does not exist!\n\n"))
    (buffer-string)))

(defun build-report-insert-installation-file (where)
  "Inserts the contents of the `build-report-installation-file'
created by the SXEmacs Beta configure process."
  (goto-char where)
  (with-temp-buffer
    (if (file-exists-p build-report-installation-file)
	(progn
          (insert "> Contents of "
                  build-report-installation-file
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
                   build-report-installation-file)
                  "\""))
                (mime-edit-insert-binary-file
                 build-report-installation-file
                 build-report-file-encoding))
            (insert-file-contents
             build-report-installation-file)))
      (insert "> " build-report-installation-file
              " does not exist!\n\n"))
    (buffer-string)))

(defun build-report-keep ()
  "Concatenate elements of `build-report-keep-regexp' and a general
MIME tag REGEXP.  The result is a REGEXP string matching either of the
REGEXPs in `build-report-keep-regexp' or a general MIME tag REGEXP."
  (mapconcat #'identity
             (cons #r"^--\[\[\|\]\]$" build-report-keep-regexp) "\\|"))

(defun build-report-delete ()
  "Concatenate elements of `build-report-delete-regexp' and a general
MIME tag REGEXP.  The result is a REGEXP string matching either of the
REGEXPs in `build-report-delete-regexp' or a general MIME tag REGEXP."
  (mapconcat '(lambda (item) item)
             build-report-delete-regexp "\\|"))

;;; build-report.el ends here
