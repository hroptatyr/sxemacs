;;; loaddefs.el --- define standard autoloads of other files

;; Copyright (C) 1985-7, 1992-5, 1997 Free Software Foundation, Inc.

;; Maintainer: SXEmacs Development Team
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

;;; Synched up with: Not synched with FSF.

;;; Commentary:

;; The following commentary is completely out of date.  I would like to
;; delete it, but it serves as a useful reminder as to how things used to
;; work.

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; Special formatting conventions are used in this file!

;; a backslash-newline is used at the beginning of a documentation string
;; when that string should be stored in the file lib-src/DOCnnn, not in core.

;; Such strings read into Lisp as numbers (during the pure-loading phase).

;; But you must obey certain rules to make sure the string is understood
;; and goes into lib-src/DOCnnn properly.  Otherwise, the string will not go
;; anywhere!

;; The doc string must appear in the standard place in a call to
;; defun, autoload, defvar or defconst.  No Lisp macros are recognized.
;; The open-paren starting the definition must appear in column 0.

;; In defvar and defconst, there is an additional rule:
;; The double-quote that starts the string must be on the same
;; line as the defvar or defconst.
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;; **********************************************************************
;; You should never need to write autoloads by hand and put them here.

;; It is no longer necessary.  Instead use autoload.el to maintain them
;; for you.  Just insert ";;;###autoload" before defuns or defmacros you
;; want to be autoloaded, or other forms you want copied into loaddefs.el
;; (defvars, key definitions, etc.).  For example,
;;	;;;###autoload
;;	(defun foobar () ....)
;;	;;;###autoload (define-key global-map "f" 'foobar)
;;	;;;###autoload
;;	(defvar foobar-var nil
;;	  "This is foobar-var's doc-string.")

;; Then do M-x update-file-autoloads on the file to update loaddefs.el.

;; You can also use M-x update-directory-autoloads to update the autoloads
;; in loaddefs.el for all .el files in the lisp/ directory, or M-x
;; update-autoloads-here to update the autoloads for each file that
;; already has an autoload section in this file.
;; **********************************************************************


;;; Code:

;; These variables are used by autoloadable packages.
;; They are defined here so that they do not get overridden
;; by the loading of those packages.


;; Names in directory that end in one of these
;; are ignored in completion,
;; making it more likely you will get a unique match.
(setq completion-ignored-extensions
      ;; this is way way way bogus.
      ;; completely wtf?
      ;; the only things that should be here are those that are
      ;; (a) universally recognizable, and
      ;; (b) obvious backup files, or
      ;; (c) obvious binary files that are generated on a
      ;;     PER-SOURCE-FILE basis, so that they will actually
      ;;     cause annoyance.  This excludes executables (.exe, .com)
      ;;     and libraries (.a, .lib, .dll).
;	      '(".o" ".elc" "~" ".bin" ".lbin" ".fasl"
;		".dvi" ".toc" ;".log"
;		".aux" ".a" ".ln"
;		".lof" ".blg" ".bbl" ".glo" ".idx" ".lot" ".fmt"
;		".diff" ".oi" ".class")))
      '(".o" ".obj" ".elc" "~"
	".bin" ".lbin" ;; #### these are doubtful, esp. the latter.
	".dvi" ;; possibly doubtful, too.
	".class"))


;; This needs to be redone better. -slb
;(setq debug-ignored-errors
;      '(beginning-of-line
;	beginning-of-buffer
;	end-of-line
;        end-of-buffer
;	end-of-file buffer-read-only
;	"\\`Previous command was not a yank\\'"
;	"\\`Minibuffer is not active for completion\\'"
;	"\\`No \\(following\\|preceding\\) item in .*-history\\'"
;	"\\`No recursive edit is in progress\\'"
;	"\\`Changes to be undone are outside visible portion of buffer\\'"
;	"\\`No further undo information\\'"
;	"\\`No undo information in this buffer\\'"
;	"\\`Buffer modified since last undo/redo, cannot redo"
;	"\\`Save not confirmed\\'"
;	"\\`Canceled\\'"
;	"\\`\\(Revert\\|Steal\\|Recover-file\\) cancelled\\.\\'"

;	;; comint
;	"\\`Not at command line\\'"
;	"\\`Empty input ring\\'"
;	"\\`No history\\'"
;	"\\`Not found\\'" ;; To common?
;	"\\`Current buffer has no process\\'"

;	;; dabbrev
;	"\\`No \\(further \\)?dynamic expansion for .* found\\.?\\'"

;	;; Completion
;	"\\`To complete, the point must be after a symbol at least [0-9]* character long\\.\\'"
;	"\\`The string \".*\" is too short to be saved as a completion\\.\\'"

;	;; Compile
;	"\\`No more errors\\( yet\\|\\)\\'"

;	;; Gnus
;	;"\\`NNTP: Connection closed\\.\\'"

;	;; info
;	"\\`Node has no Previous\\'"
;	"\\`No \".*\" in index\\'"

;	;; imenu
;	;"\\`No items suitable for an index found in this buffer\\.\\'"
;	;"\\`The mode \".*\" does not take full advantage of imenu\\.el yet\\.\\'"

;	;; ispell
;	"\\`No word found to check!\\'"

;	;; man
;	"\\`.* not found\\'"
;	"\\`No more history\\.\\'"

;	;; etags
;	"\\`File .* is not a valid tag table\\'"
;	"\\`File .* is not a valid tags file\\'"
;	"\\`All files processed\\.\\'"
;	"No TAGS file name supplied\\'"
;	"\\`Nothing to complete\\'"

;	;; BBDB
;	"\\`no previous record\\'"
;	"\\`no next record\\'"))

(make-variable-buffer-local 'indent-tabs-mode)


;;; Load in generated autoloads (made by autoload.el).

;; (let ((dir load-path)
;;       purify-flag)
;;   (while dir
;;     (condition-case nil
;;	(load (concat (car dir) "auto-autoloads"))
;;      (t nil))
;;    (pop dir)))

;;; Local Variables:
;;; no-byte-compile: t
;;; no-update-autoloads: t
;;; End:
;;; loaddefs.el ends here
