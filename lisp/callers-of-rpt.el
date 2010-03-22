;;; callers-of-rpt.el --- generate call graph of lisp in XEmacs

;; Copyright (C) 1997 Karl Hegbloom
;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Karl Hegbloom <karlheg@inetarena.com>
;; Maintainer: XEmacs Development Team
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

;;; Synched up with: not in FSF

;;; Commentary:

;; Grep-2.1 is required.
;; Modify the `xemacs-src-lisp-dir' and `xemacs-pkg-lisp-dir' to reflect
;;  where these directories live on your local system.

;;; Code:

(defvar xemacs-src-lisp-dir "/usr/src/xemacs-20.0/lisp/"
  "Where the XEmacs 20 lisp sources live.")
(defvar xemacs-pkg-lisp-dir "/home/xemacs/packages/"
  "Where the package lisp sources live.")

;; (makunbound 'caller-table)
(defconst caller-table (make-hash-table :test 'equal)
  "Hash table keyed on the symbols being required.  Each element will
  be a list of file-names of programs that depend on them.")

;;./apel/atype.el:(require 'emu)
;;./apel/atype.el:(require 'alist)
;;./apel/emu-e19.el:       (require 'emu-xemacs))
;;./apel/emu-e19.el:       (require 'emu-19)

(defun make-caller-report ()
  "Generate a simple report showing .el files that are `require'd by
  other .el files, and the list of programs that depend on them."
  (interactive)
  (let ((cmd-out (get-buffer-create "*caller-report find-grep output*"))
	(rpt (get-buffer-create "* caller report *"))
	file-name)
    (switch-to-buffer cmd-out)
    (buffer-disable-undo cmd-out)
    (set-syntax-table emacs-lisp-mode-syntax-table cmd-out)
    (erase-buffer cmd-out)
    (message "Running the find | grep...")
    (sit-for 0)
    ;; Note: Edit this part as needed for your installation.
    (shell-command (concat
		    ;; First the installed lisp
		    "cd " xemacs-src-lisp-dir " ;"
		    "grep -H '(require ' $(find -name '*.el' -print) |"
		    #r" grep -v 'auto-autoloads\.el\|callers-of-rpt\.el' |"
		    " grep -v 'el:[ \t]*;\\|require load' ;" ; ones commented off, and cus-edit.el
		    ;; Then the packages
		    "cd " xemacs-pkg-lisp-dir " ;"
		    "grep -H '(require ' $(find -name '*.el' -print) |"
		    #r" grep -v 'auto-autoloads\.el\|callers-of-rpt\.el' |"
		    " grep -v 'el:[ \t]*;' ;" ; ones commented off
		    )
		   cmd-out)
    (message "Running the find | grep... Done.")
    (goto-char (point-min))
    (sit-for 0)
    (while (not (eobp))
      (setq file-name (buffer-substring (+ (point) 2) ; skip the leading "./"
					(progn
					  (skip-chars-forward "^:")
					  (point))
					cmd-out))
      (re-search-forward "(require '" nil t)
      (let* ((key (buffer-substring (point) (progn
					      (skip-chars-forward "^) ")
					      (point))
				    cmd-out))
	     (lst (gethash key caller-table)))
	(unless (member file-name lst)
	  (puthash key (cons file-name lst) caller-table)))
      (forward-line 1)
      (sit-for 0))
    (switch-to-buffer rpt)
    (buffer-disable-undo rpt)
    (erase-buffer rpt)
    (sit-for 0)
    (let (keys)
      (maphash #'(lambda (key val) (push key keys)) caller-table)
      (setq keys (sort keys #'string<))
      (mapc #'(lambda (key)
		(insert (format "(%s '(" key))
		(let ((lst (gethash key caller-table)))
		  (while lst
		    (insert (format "%S" (car lst)))
		    (setq lst (cdr lst))
		    (when lst (insert " "))))
		(insert "))\n")
		(sit-for 0))
	    keys))))

(byte-compile 'make-caller-report)
(delete-other-windows)
(make-caller-report)

;;; callers-of-rpt.el ends here
