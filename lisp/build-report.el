;;; build-report.el --- Automatically formatted build reports for XEmacs

;; Copyright (C) 1997-2001 Adrian Aichner

;; Author: Adrian Aichner <adrian@xemacs.org>
;; Keywords: internal

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not synched.

;;; Commentary:

;; The Idea:
;; Let XEmacs report interesting aspects of how it was built.

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

;;; Code:

(require 'config)
(require 'custom)
(require 'cl)
(provide 'build-report)

;;; Constant definitions used internally by `build-report'.  These are not
;;; anticipated to be changed by users of `build-report'.
;;; If users do need to change the value of any of these, they need to do
;;; it after `build-report' has been loaded (not just required).  Please
;;; report it to the maintainers of `build-report' when you think you
;;; need to do this.
(defconst build-report-installation-version-regexp
  "XEmacs\\s-+\\([0-9]+\\)\\.\\([0-9]+\\)\\(\\(-b\\|\\.\\)\\([0-9]+\\)\\)?\\s-+\\\\?\"\\([^\\\"]+\\)\\\\?\"\\s-+configured\\s-+for\\s-+`\\(.+\\)'\\."
  "*REGEXP matching XEmacs Beta Version string in
`build-report-installation-file' file.  This variable is used by
`build-report-installation-data'.")

(defconst build-report-version-file-regexp
  "emacs_major_version\\s-*=\\s-*\\([0-9]+\\)
emacs_minor_version\\s-*=\\s-*\\([0-9]+\\)
emacs_beta_version\\s-*=\\s-*\\([0-9]+\\)?
xemacs_codename\\s-*=\\s-*\"\\([^\"]+\\)\""
  "*REGEXP matching XEmacs Beta Version variable assignments in
`build-report-version-file' file.  This variable is used by
`build-report-version-file-data'.")

(defconst build-report-installation-srcdir-regexp
  "\\s-*Where should the build process find the source code\\?\\s-*\\(.*\\)$"
  "REGEXP matching XEmacs Beta srcdir as the first substring match in
`build-report-installation-file' file.  This variable is used by
`build-report-installation-data'.")

;;; Customization support for build-report starts here.

(defgroup build-report nil
  "Standardizes the Creation of XEmacs Build Reports."
  :load 'build-report
  :group 'build)

(defcustom build-report-destination
  (list
   "XEmacs Build Reports List <xemacs-buildreports@xemacs.org>"
   "XEmacs Beta List <xemacs-beta@xemacs.org>")
  "*The list of mail addresses XEmacs Build Reports should most likely
go to."
  :type '(repeat
          :custom-show t
          :documentation-shown t
          string)
  :group 'build-report)

(defcustom build-report-keep-regexp
  (list
   "^\\(cd\\|n?make\\)\\s-"
   "errors?"
   "warnings?"
   "pure.*\\(space\\|size\\)"
   "hides\\b"
   "strange"
   "shadowings"
   "^Compil\\(ing\\s-+in\\|ation\\)"
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
  (cond 
   ((equal system-type 'windows-nt)
    (expand-file-name "nt"
                      (gethash 'blddir (config-value-hash-table))))
   (t
    (gethash 'blddir (config-value-hash-table))))
  "*Directory where the build report file is found.
  If this is empty or nil, the default, it is replaced by the value of
  the XEmacs build directory."
  :type '(directory
          :custom-show t
          :documentation-shown t)
  :group 'build-report)

(defcustom build-report-make-output-files
  (list
   "beta.err"
   "xemacs-make-all.err" 
   "xemacs-make-check-temacs.err"
   "xemacs-make-check.err"
   "xemacs-make-install.err")
  "*List of Filenames where stdout and stderr of XEmacs make process
have been stored.  These are relative to
`build-report-make-output-dir`.  You'll have to run make with output
redirection or use the `build' XEmacs package to save this output. You
may use following alias

alias mk 'make \!* >>&\! \!$.err &'

under csh, so that you get beta.err when you run `mk beta'."
  :type '(repeat
          :custom-show t
          :documentation-shown t
          file)
  :group 'build-report)

(defcustom build-report-installation-file
  (expand-file-name "Installation"
                    (gethash 'blddir (config-value-hash-table)))
  "*Installation file produced by XEmacs configure process."
  :type '(file
          :custom-show t
          :documentation-shown t)
  :group 'build-report)

(defcustom build-report-version-file
  (expand-file-name
   "version.sh"
   (gethash 'blddir (config-value-hash-table)))
  "*version.sh file identifying XEmacs (Beta) Distribution."
  :type '(file
          :custom-show t
          :documentation-shown t)
  :group 'build-report)

(defcustom build-report-installation-insert-all
  nil
  "*Tell build-report to insert the whole Installation file
  instead of just the last report."
  :type 'boolean
  :group 'build-report)

(defcustom build-report-subject
  (concat "[%s] " emacs-version " on " system-configuration)
  "*XEmacs Build Report Subject Line. %s-sequences will be substituted
  with user input through `build-report' according to
  `build-report-prompts' using `format'."
  :type '(string
          :custom-show t
          :documentation-shown t)
  :group 'build-report)

(defcustom build-report-prompts
  (quote (("Status?: "  ("Success" "Failure"))))
  "*XEmacs Build Report Prompt(s). This is a list of prompt-string
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
  "*XEmacs Build Report File Encoding to be used when MIME support is
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
  "Returns the filename the XEmacs make output is saved in."
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

Looks for Installation and the make output file (`beta.err' by
default, customizable via `build-report-make-output-files') in the
build directory of the running XEmacs by default (customizable via
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
    (if (file-exists-p build-report-installation-file)
        (multiple-value-bind
            (major minor beta codename configuration)
            (build-report-installation-data build-report-installation-file)
          (setq build-report-subject
                (format "[%%s] XEmacs %s.%s%s \"%s\", %s"
                        major minor beta codename configuration)))
      (multiple-value-bind
          (major minor beta codename)
          (build-report-version-file-data build-report-version-file)
        (setq build-report-subject
              (format "[%%s] XEmacs %s.%s%s \"%s\", %s"
                      major minor beta codename system-configuration))))
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
          (insert (build-report-insert-installation-file
                   report-begin
                   build-report-installation-insert-all))
        (insert (format "%s not found!\n" build-report-installation-file)))
;;;       (when (and (>= major 21) (>= minor 2) (or (null beta) (>= beta 32)))
;;;         (insert "\n")
;;;         (insert (build-report-insert-config-inc report-begin)))
      (insert "\n")
      (insert (build-report-insert-header report-begin))
      (goto-char report-begin))))

(defun build-report-insert-header (where)
  "Inserts the build-report-header at the point specified by `where'."
  (goto-char where)
  (with-temp-buffer
    (insert
     (format "
> XEmacs Build Report generated by emacs-version
> %s
> with system-configuration
> %s
> follows:\n\n" emacs-version system-configuration))
    (buffer-string)))

(defun build-report-insert-make-output (where file)
  "Inserts the output of the XEmacs Beta make run in the
current buffer at position WHERE.
The make process output must have been saved in
`build-report-make-output-files' during the XEmacs Beta building."
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
          (if build-report-keep-regexp
              (insert
               (format
                "> keeping lines matching
> \"%s\"
"
                (build-report-keep))))
          (if build-report-delete-regexp
              (insert
               (format
                "> %sdeleting lines matching
> \"%s\"
"
                (if build-report-keep-regexp
                    "and then "
                  "")
                (build-report-delete))))
          (insert "\n")
          (goto-char (point-min))
          (insert
           (format "> Contents of %s\n" file)))
      (insert "> " file
              " does not exist!\n\n"))
    (buffer-string)))

(defun build-report-insert-installation-file (where all)
  "Inserts the contents of the `build-report-installation-file'
created by the XEmacs Beta configure process."
  (goto-char where)
  (with-temp-buffer
    (if (file-exists-p build-report-installation-file)
        (let (file-begin last-configure)
          (insert "> Contents of "
                  build-report-installation-file
                  ":\n")
          (insert
           (format
            "> (Output from %s of ./configure)\n\n"
            (if all "all runs" "most recent run")))
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
                 build-report-file-encoding)
                (setq file-begin (mime-edit-content-beginning)))
            (setq file-begin (point))
            (insert-file-contents
             build-report-installation-file))
          (unless all
            (setq last-configure
                  (search-backward-regexp
                   "^\\(uname.*\\|osversion\\|OS\\):\\s-+" file-begin t))
            (if (and file-begin last-configure)
                (delete-region file-begin last-configure))))
      (insert "> " build-report-installation-file
              " does not exist!\n\n"))
    (buffer-string)))

(defun build-report-keep ()
  "Concatenate elements of `build-report-keep-regexp' and a general
MIME tag REGEXP.  The result is a REGEXP string matching either of the
REGEXPs in `build-report-keep-regexp' or a general MIME tag REGEXP."
  (mapconcat #'identity
             (cons "^--\\[\\[\\|\\]\\]$" build-report-keep-regexp) "\\|"))

(defun build-report-delete ()
  "Concatenate elements of `build-report-delete-regexp' and a general
MIME tag REGEXP.  The result is a REGEXP string matching either of the
REGEXPs in `build-report-delete-regexp' or a general MIME tag REGEXP."
  (mapconcat '(lambda (item) item)
             build-report-delete-regexp "\\|"))

(defun build-report-installation-data (&optional file)
  "Return a list of XEmacs installation data containing MAJOR_NUMBER
MINOR_NUMBER BETA_STRING CODENAME CONFIGURATION SRCDIR from FILE,
which defaults to `build-report-installation-file'."
  (interactive "fInstallation file: ")
  (unless file
    (setq file build-report-installation-file))
  (let
      (major minor beta codename configuration srcdir)
    (save-window-excursion
      (find-file-read-only file)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (cond
         ((looking-at build-report-installation-version-regexp)
          (goto-char (match-end 0))
          (setq major (match-string 1))
          (setq minor (match-string 2))
          (setq beta (match-string 3))
          (setq codename (match-string 6))
          (setq configuration (match-string 7)))
         ((looking-at build-report-installation-srcdir-regexp)
          (goto-char (match-end 0))
          (setq srcdir (match-string 1)))
         ;; We avoid matching a potentially zero-length string to avoid
         ;; infinite looping.
         ((looking-at
           "^.+$")
          (goto-char (match-end 0)))
         ((looking-at "\n")
          (goto-char (match-end 0)))))
      (values major minor (or beta "") codename configuration srcdir))))

(defun build-report-version-file-data (&optional file)
  "Return a list of XEmacs version information containing
MAJOR_NUMBER MINOR_NUMBER BETA_STRING CODENAME from FILE, which
defaults to `build-report-version-file'." 
  (interactive "fversion.sh file: ")
  (unless file
    (setq file build-report-version-file))
  (let
      (major minor beta codename)
    (save-window-excursion
      (find-file-read-only file)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (cond
         ((looking-at build-report-version-file-regexp)
          (goto-char (match-end 0))
          (setq major (match-string 1))
          (setq minor (match-string 2))
          (setq beta (match-string 3))
          (setq codename (match-string 4)))
         ;; We avoid matching a potentially zero-length string to avoid
         ;; infinite looping.
         ((looking-at
           "^.+$")
          (goto-char (match-end 0)))
         ((looking-at "\n")
          (goto-char (match-end 0)))))
      (values major minor (or beta "") codename))))

;;; build-report.el ends here
