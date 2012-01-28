;;; auto-show.el --- perform automatic horizontal scrolling as point moves

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; This file is in the public domain.

;; Author: Pete Ware <ware@cis.ohio-state.edu>
;; Maintainer: XEmacs Development Team
;; Keywords: extensions, internal, dumped

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

;;; Synched up with: Emacs/Mule zeta.

;;; Commentary:

;; This file is dumped with XEmacs.

;; Modified by: Ben Wing <ben@xemacs.org>

;; This file provides functions that
;; automatically scroll the window horizontally when the point moves
;; off the left or right side of the window.

;; Once this library is loaded, automatic horizontal scrolling
;; occurs whenever long lines are being truncated.
;; To request truncation of long lines, set the variable
;; Setting the variable `truncate-lines' to non-nil.
;; You can do this for all buffers as follows:
;;
;; (set-default 'truncate-lines t)

;; Here is how to do it for C mode only:
;;
;; (set-default 'truncate-lines nil)	; this is the original value
;; (defun my-c-mode-hook ()
;;   "Run when C-mode starts up.  Changes ..."
;;   ... set various personal preferences ...
;;   (setq truncate-lines t))
;; (add-hook 'c-mode-hook 'my-c-mode-hook)
;;
;;
;; As a finer level of control, you can still have truncated lines but
;; without the automatic horizontal scrolling by setting the buffer
;; local variable `auto-show-mode' to nil.  The default value is t.
;; The command `auto-show-mode' toggles the value of the variable
;; `auto-show-mode'.

;;; Code:

(defgroup auto-show nil
  "Perform automatic horizontal scrolling as point moves."
  :group 'display
  :group 'extensions)

;; This is preloaded, so we don't need special :set, :require, etc.
(defcustom auto-show-mode t
  "*Non-nil enables automatic horizontal scrolling, when lines are truncated.
The default value is t.  To change the default, do this:
	(set-default 'auto-show-mode nil)
See also command `auto-show-mode'.
This variable has no effect when lines are not being truncated.
This variable is automatically local in each buffer where it is set."
  :type 'boolean
  :group 'auto-show)

(make-variable-buffer-local 'auto-show-mode)

(defcustom auto-show-shift-amount 8
  "*Extra columns to scroll. for automatic horizontal scrolling."
  :type 'integer
  :group 'auto-show)

(defcustom auto-show-show-left-margin-threshold 50
  "*Threshold column for automatic horizontal scrolling to the right.
If point is before this column, we try to scroll to make the left margin
visible.  Setting this to 0 disables this feature."
  :type 'number
  :group 'auto-show)

(defun auto-show-mode (arg)
  "Turn automatic horizontal scroll mode on or off.
With arg, turn auto scrolling on if arg is positive, off otherwise.
This mode is enabled or disabled for each buffer individually.
It takes effect only when `truncate-lines' is non-nil."
  (interactive "P")
  (setq auto-show-mode
	(if (null arg)
	    (not auto-show-mode)
	  (> (prefix-numeric-value arg) 0))))

;; XEmacs addition:
(defvar auto-show-inhibiting-commands
  '(scrollbar-char-left
    scrollbar-char-right
    scrollbar-page-left
    scrollbar-page-right
    scrollbar-to-left
    scrollbar-to-right
    scrollbar-horizontal-drag)
  "Commands that inhibit auto-show behavior.
This normally includes the horizontal scrollbar commands.")

;; XEmacs addition:
(defun auto-show-should-take-action-p ()
  (and auto-show-mode (window-truncated-p)
       (equal (window-buffer) (current-buffer))
       (not (memq this-command auto-show-inhibiting-commands))))

;; XEmacs addition:
(defun auto-show-make-region-visible (start end)
  "Move point in such a way that the region (START, END) is visible.
This only does anything if auto-show-mode is enabled, and it doesn't
actually do any horizontal scrolling; rather, it just sets things up so
that the region will be visible when `auto-show-make-point-visible'
is next called (this happens after every command)."
  (if (auto-show-should-take-action-p)
      (let* ((scroll (window-hscroll))	;how far window is scrolled
	     (w-width (- (window-width)
			 (if (> scroll 0)
			     2 1)))	;how wide window is on the screen
	     (right-col (+ scroll w-width))
	     (start-col (save-excursion (goto-char start) (current-column)))
	     (end-col (save-excursion (goto-char end) (current-column))))
	(cond ((and (>= start-col scroll)
		    (<= end-col right-col))
	       ;; already completely visible
	       nil)
	      ((< start-col scroll)
	       (scroll-right (- scroll start-col)))
	      (t
	       (scroll-left (- end-col right-col)))))))

(defun auto-show-make-point-visible (&optional ignore-arg)
  "Scroll horizontally to make point visible, if that is enabled.
This function only does something if `auto-show-mode' is non-nil
and longlines are being truncated in the selected window.
See also the command `auto-show-mode'."
  (interactive)
  ;; XEmacs change
  (if (auto-show-should-take-action-p)
      (let* ((col (current-column))	;column on line point is at
	     (scroll (window-hscroll))	;how far window is scrolled
	     (w-width (- (window-width)
			 (if (> scroll 0)
			     2 1)))	;how wide window is on the screen
	     (right-col (+ scroll w-width)))
	(if (and (< col auto-show-show-left-margin-threshold)
		 (< col (window-width))
		 (> scroll 0))
	    (scroll-right scroll)
	  (if (< col scroll)		;to the left of the screen
	      (scroll-right (+ (- scroll col) auto-show-shift-amount))
	    (if (or (> col right-col)	;to the right of the screen
		    (and (= col right-col)
			 (not (eolp))))
		(scroll-left (+ auto-show-shift-amount
				(- col (+ scroll w-width))))))))))

;; XEmacs change:
;; #### instead of this, we kludgily call it from the C code, to make sure
;; that it's done after any other things on post-command-hook (which might
;; move point).
;; Do auto-scrolling after commands.
;;(add-hook 'post-command-hook 'auto-show-make-point-visible)

;; If being dumped, turn it on right away.
(when (boundp 'pureload)
  (auto-show-mode 1))

;; Do auto-scrolling in comint buffers after process output also.
; XEmacs -- don't do this now, it messes up comint.
;(add-hook 'comint-output-filter-functions 'auto-show-make-point-visible t)

(provide 'auto-show)

;;; auto-show.el ends here
