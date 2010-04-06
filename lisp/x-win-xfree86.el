;;; x-win-xfree86.el --- runtime initialization for XFree86 servers
;; Copyright (C) 1995 Sun Microsystems, Inc.
;; Copyright (C) 1995 Ben Wing.

;; Author: Ben Wing
;; Author: Martin Buchholz (rewritten to use function-key-map)
;; Keywords: terminals

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

;;; Commentary:

;; This file is loaded by x-win.el at run-time when we are sure that XEmacs
;; is running on the display of something running XFree86 (Linux,
;; NetBSD, FreeBSD, and perhaps other Intel Unixen).

;;; #### bleck!!! Use key-translation-map!

;;; #### Counter-bleck!! We shouldn't override a user binding for F13.
;;; So we use function-key-map for now.
;;; When we've implemented a fallback-style equivalent of
;;; keyboard-translate-table, we'll use that instead. (martin)

;; For no obvious reason, shift-F1 is called F13, although Meta-F1 and
;; Control-F1 have normal names.

;;;###autoload
(defun x-win-init-xfree86 ()
  (loop for (key sane-key) in
    '((f13 f1)
      (f14 f2)
      (f15 f3)
      (f16 f4)
      (f17 f5)
      (f18 f6)
      (f19 f7)
      (f20 f8)
      (f21 f9)
      (f22 f10)
      (f23 f11)
      (f24 f12))
    do
    (when (and (x-keysym-on-keyboard-p key)
	       (not (x-keysym-on-keyboard-sans-modifiers-p key)))
      ;; define also the control, meta, and meta-control versions.
      (loop for mods in '(() (control) (meta) (meta control)) do
	(define-key function-key-map `[(,@mods ,key)] `[(shift ,@mods ,sane-key)])
	))))

;;; x-win-xfree86.el ends here
