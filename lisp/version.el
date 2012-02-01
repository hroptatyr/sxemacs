;; version.el --- Record version number of SXEmacs.

;; Copyright (C) 1985, 1991-1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 2004 Steve Youngs.

;; Maintainer: SXEmacs Development Team
;; Keywords: internal, dumped

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

;;; Synched up with: not synched.

;;; Commentary:

;; This file is dumped with SXEmacs.

;;; Code:

(defconst sxemacs-betaname
  (and emacs-beta-version (format "(beta%d)" emacs-beta-version))
  "Non-nil when this is a test (beta) version of XEmacs.
Warning, this variable did not exist in XEmacs versions prior to 20.3")

(defconst emacs-version (concat "SXEmacs: " sxemacs-git-version)
  "Version numbers of this version of XEmacs.")

;; Sadly, our `emacs-version' causes a problem for Dired.  This is because
;; Dired uses some regex matching against `emacs-version' instead of simply
;; using the "standard" `emacs-*-version' variables.  The following trivial
;; patch to dired.el will fix Dired for you.  I have already been in touch
;; with the Dired maintainers about this. --SY.
;;
;; --- dired.el.orig	2004-10-03 07:15:32.000000000 +1000
;; +++ dired.el	2004-12-13 16:06:47.000000000 +1000
;; @@ -81,7 +81,7 @@
;;  ;; it been since Lucid went away?
;;  (let ((lucid-p (string-match "XEmacs" emacs-version))
;;        ver subver)
;; -  (or (string-match "^\\([0-9]+\\)\\.\\([0-9]+\\)" emacs-version)
;; +  (or (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)" emacs-version)
;;        (error "dired does not work with emacs version %s" emacs-version))
;;    (setq ver (string-to-int (substring emacs-version (match-beginning 1)
;;				      (match-end 1)))
;; @@ -6617,7 +6617,7 @@
;;
;;  (let ((lucid-p (string-match "XEmacs" emacs-version))
;;        ver)
;; -  (or (string-match "^\\([0-9]+\\)\\." emacs-version)
;; +  (or (string-match "\\([0-9]+\\)\\." emacs-version)
;;        (error "Weird emacs version %s" emacs-version))
;;    (setq ver (string-to-int (substring emacs-version (match-beginning 1)
;;				      (match-end 1))))

(if (featurep 'infodock)
    (require 'id-vers))

(defconst emacs-build-time (current-time-string)
  "Time at which Emacs was dumped out.")

(defconst emacs-build-system (system-name))

(defun emacs-version  (&optional arg)
  "Return string describing the version of Emacs that is running.
When called interactively with a prefix argument, insert string at point.
Don't use this function in programs to choose actions according
to the system configuration; look at `system-configuration' instead."
  (interactive "p")
  (save-match-data
    (let ((version-string
	   (format
	    "SXEmacs: %s, built %s on %s"
	    sxemacs-git-version
	    emacs-build-time
	    emacs-build-system)))
      (cond
       ((null arg) version-string)
       ((eq arg 1) (message "%s" version-string))
       (t          (insert version-string))))))

;; from emacs-vers.el
(defun emacs-version>= (major &optional minor patch)
  "Return true if the Emacs version is >= to the given MAJOR, MINOR,
   and PATCH numbers.
The MAJOR version number argument is required, but the other arguments
argument are optional. Only the Non-nil arguments are used in the test."
  (let ((emacs-patch (or emacs-patch-level emacs-beta-version -1)))
    (cond ((> emacs-major-version major))
	  ((< emacs-major-version major) nil)
	  ((null minor))
	  ((> emacs-minor-version minor))
	  ((< emacs-minor-version minor) nil)
	  ((null patch))
	  ((>= emacs-patch patch)))))

;;; We hope that this alias is easier for people to find.
(define-function 'version 'emacs-version)

;; Put the emacs version number into the `pure[]' array in a form that
;; `what(1)' can extract from the executable or a core file.  We don't
;; actually need this to be pointed to from lisp; pure objects can't
;; be GCed.
(concat "\n@" "(#)" (emacs-version)
	"\n@" "(#)" "Configuration: "
	system-configuration "\n")

(provide 'version)

;;Local variables:
;;version-control: never
;;End:

;;; version.el ends here
