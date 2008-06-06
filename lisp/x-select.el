;;; x-select.el --- Lisp interface to X Selections.

;; Copyright (C) 1990, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Sun Microsystems.

;; Maintainer: SXEmacs Development Team
;; Keywords: extensions, dumped

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

;;; Synched up with: FSF 19.30 (select.el).

;;; Commentary:

;; This file is dumped with SXEmacs (when X support is compiled in).

;; The selection code requires us to use certain symbols whose names are
;; all upper-case; this may seem tasteless, but it makes there be a 1:1
;; correspondence between these symbols and X Atoms (which are upcased).

;;; Code:

(define-obsolete-function-alias 'x-selection-exists-p 'selection-exists-p)
(define-obsolete-function-alias 'x-selection-owner-p 'selection-owner-p)
(define-obsolete-variable-alias 'x-selection-converter-alist 'selection-converter-alist)
(define-obsolete-variable-alias 'x-lost-selection-hooks 'lost-selection-hooks)
(define-obsolete-variable-alias 'x-selected-text-type 'selected-text-type)
(define-obsolete-function-alias 'x-valid-simple-selection-p 'valid-simple-selection-p)
(define-obsolete-function-alias 'x-own-selection 'own-selection)
(define-obsolete-function-alias 'x-disown-selection 'disown-selection)
(define-obsolete-function-alias 'x-delete-primary-selection 'delete-primary-selection)
(define-obsolete-function-alias 'x-copy-primary-selection 'copy-primary-selection)
(define-obsolete-function-alias 'x-kill-primary-selection 'kill-primary-selection)
(define-obsolete-function-alias 'x-select-make-extent-for-selection
  'select-make-extent-for-selection)
(define-obsolete-function-alias 'x-cut-copy-clear-internal 'cut-copy-clear-internal)
(define-obsolete-function-alias 'x-get-selection 'get-selection)
(define-obsolete-function-alias 'x-get-clipboard 'get-clipboard)
(define-obsolete-function-alias 'x-yank-clipboard-selection
  'yank-clipboard-selection)
(define-obsolete-function-alias 'x-disown-selection-internal
  'disown-selection-internal)

(defun x-get-secondary-selection ()
  "Return text selected from some X window."
  (get-selection 'SECONDARY))

(defun x-own-secondary-selection (selection &optional type)
  "Make a secondary X Selection of the given argument.  The argument may be a
string or a cons of two markers (in which case the selection is considered to
be the text between those markers)."
  (interactive (if (not current-prefix-arg)
		   (list (read-string "Store text for pasting: "))
		 (list (cons ;; these need not be ordered.
			(copy-marker (point-marker))
			(copy-marker (mark-marker))))))
  (own-selection selection 'SECONDARY))

(defun x-notice-selection-requests (selection type successful)
  "for possible use as the value of `x-sent-selection-hooks'."
  (if (not successful)
      (message "Selection request failed to convert %s to %s"
	       selection type)
    (message "Sent selection %s as %s" selection type)))

(defun x-notice-selection-failures (selection type successful)
  "for possible use as the value of `x-sent-selection-hooks'."
  (or successful
      (message "Selection request failed to convert %s to %s"
	       selection type)))

;(setq x-sent-selection-hooks 'x-notice-selection-requests)
;(setq x-sent-selection-hooks 'x-notice-selection-failures)


;;; Cut Buffer support

;;; FSF name x-get-cut-buffer
(defun x-get-cutbuffer (&optional which-one)
  "Return the value of one of the 8 X server cut buffers.
Optional arg WHICH-ONE should be a number from 0 to 7, defaulting to 0.
Cut buffers are considered obsolete; you should use selections instead.
This function does nothing if cut buffer support was not compiled in."
  (when (fboundp 'x-get-cutbuffer-internal)
    (x-get-cutbuffer-internal
     (aref [CUT_BUFFER0 CUT_BUFFER1 CUT_BUFFER2 CUT_BUFFER3
			CUT_BUFFER4 CUT_BUFFER5 CUT_BUFFER6 CUT_BUFFER7]
	   (or which-one 0)))))

;;; FSF name x-set-cut-buffer
(defun x-store-cutbuffer (string &optional push)
  "Store STRING into the X server's primary cut buffer.
If optional arg PUSH is non-nil, also rotate the cut buffers: this
means the previous value of the primary cut buffer moves to the second
cut buffer, and the second to the third, and so on (there are 8 buffers.)
Cut buffers are considered obsolete; you should use selections instead.
This function does nothing if cut buffer support was not compiled in."
  (when (fboundp 'x-store-cutbuffer-internal)
    (when push
      (x-rotate-cutbuffers-internal 1))
    (x-store-cutbuffer-internal 'CUT_BUFFER0 string)))


;FSFmacs (provide 'select)

;;; x-select.el ends here.
