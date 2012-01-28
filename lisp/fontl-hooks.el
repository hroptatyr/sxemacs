;;; fontl-hooks.el --- pre-loaded stuff for font-lock.

;; Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Amdahl Corporation.
;; Copyright (C) 1996 Ben Wing.

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

;;; Synched up with: FSF 19.30. (font-lock.el)

;;; Commentary:

;; The reason for the existence of this file is so that modes can
;; call `font-lock-set-defaults' without worrying about whether
;; font-lock is loaded.  We don't autoload this from font-lock.el
;; because loading font-lock.el automatically turns font-lock on.

;;; Code:

(defun font-lock-set-defaults (&optional explicit-defaults)
  "Set fontification defaults appropriately for this mode.
Sets `font-lock-keywords', `font-lock-keywords-only', `font-lock-syntax-table',
`font-lock-beginning-of-syntax-function' and
`font-lock-keywords-case-fold-search'.

If `font-lock-defaults' is currently set, it is used.  Otherwise, the
symbol naming the major mode is examined for a `font-lock-defaults'
property.  If that is not present, but a variable `foo-mode-font-lock-keywords'
is, the value of that variable is used as the default for
`font-lock-keywords'.  Various other backward-compatible behaviors also
exist -- if you're curious, look at the source.

The value of `font-lock-maximum-decoration' is used to determine which
set of keywords applies, if more than one exists.

This will also put the buffer into Font Lock mode if any keywords exist
and if auto-fontification is called for, as determined by
`font-lock-auto-fontify', `font-lock-mode-enable-list', and
`font-lock-mode-disable-list'.

Calling this function multiple times in the same buffer is safe -- this
function keeps track of whether it has already been called in this
buffer, and does nothing if so.  This allows for multiple ways of getting
Font Lock properly initialized in a buffer, to deal with existing major
modes that do not call this function. (For example, Font Lock adds this
function to `find-file-hooks'.)

Major modes that have any font-lock defaults specified should call this
function during their initialization process, after they have set
the variable `major-mode'.

If EXPLICIT-DEFAULTS is t, this function will not check whether it
has already been run in this buffer, and will always do the full
computation.

If EXPLICIT-DEFAULTS is not nil and not t, it should be something
that is allowable as a value for `font-lock-defaults' and will be
used to initialize the Font Lock variables."

  (with-boundp '(font-lock-auto-fontify
		 font-lock-mode-disable-list font-lock-mode-enable-list
		 font-lock-keywords)
    (when
	(and
	 (featurep 'font-lock)
	 (if font-lock-auto-fontify
	     (not (memq major-mode font-lock-mode-disable-list))
	   (memq major-mode font-lock-mode-enable-list))
	(or (declare-fboundp (font-lock-set-defaults-1 explicit-defaults))
	    font-lock-defaults-computed)
	 font-lock-keywords)
      (declare-fboundp (turn-on-font-lock)))))

(provide 'fontl-hooks)

;;; fontl-hooks.el ends here
