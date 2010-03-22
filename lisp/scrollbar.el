;;; scrollbar.el --- Scrollbar support for SXEmacs

;; Copyright (C) 1995, 1997 Free Software Foundation, Inc.

;; Maintainer: SXEmacs Development Team
;; Keywords: internal, extensions, dumped

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

;;; Synched up with: Not in FSF. (Completely divergent from FSF scroll-bar.el)

;;; Commentary:

;; This file is dumped with SXEmacs (when scrollbar support is compiled in).

;;; Code:

;; added for the options menu - dverna
(defcustom scrollbars-visible-p t
  "Whether the scrollbars are globally visible.
This variable can be customized through the options menu."
  :type 'boolean
  :set (lambda (var val)
	 (set-specifier vertical-scrollbar-visible-p val)
	 (set-specifier horizontal-scrollbar-visible-p val)
	 (setq-default scrollbars-visible-p val))
  :group 'display)

(defun init-scrollbar-from-resources (locale)
  (when (and (featurep 'x)
	     (or (eq locale 'global)
		 (eq 'x (device-or-frame-type locale))))
    (x-init-scrollbar-from-resources locale)))

;;
;; vertical scrollbar functions
;;

;;; #### Move functions from C into Lisp here!

;;
;; horizontal scrollbar functions
;;

(defun scrollbar-char-left (window)
  "Function called when the char-left arrow on the scrollbar is clicked.
This is the little arrow to the left of the scrollbar.  One argument is
passed, the scrollbar's window.  You can advise this function to
change the scrollbar behavior."
  (when (window-live-p window)
    (scrollbar-set-hscroll window (- (window-hscroll window) 1))
    (setq zmacs-region-stays t)
    nil))

(defun scrollbar-char-right (window)
  "Function called when the char-right arrow on the scrollbar is clicked.
This is the little arrow to the right of the scrollbar.  One argument is
passed, the scrollbar's window.  You can advise this function to
change the scrollbar behavior."
  (when (window-live-p window)
    (scrollbar-set-hscroll window (+ (window-hscroll window) 1))
    (setq zmacs-region-stays t)
    nil))

(defun scrollbar-page-left (window)
  "Function called when the user gives the \"page-left\" scrollbar action.
\(The way this is done can vary from scrollbar to scrollbar.\) One argument is
passed, the scrollbar's window.  You can advise this function to
change the scrollbar behavior."
  (when (window-live-p window)
    (scrollbar-set-hscroll window (- (window-hscroll window)
				     (- (window-width window) 2)))
    (setq zmacs-region-stays t)
    nil))

(defun scrollbar-page-right (window)
  "Function called when the user gives the \"page-right\" scrollbar action.
\(The way this is done can vary from scrollbar to scrollbar.\) One argument is
passed, the scrollbar's window.  You can advise this function to
change the scrollbar behavior."
  (when (window-live-p window)
    (scrollbar-set-hscroll window (+ (window-hscroll window)
				     (- (window-width window) 2)))
    (setq zmacs-region-stays t)
    nil))

(defun scrollbar-to-left (window)
  "Function called when the user gives the \"to-left\" scrollbar action.
\(The way this is done can vary from scrollbar to scrollbar.\). One argument is
passed, the scrollbar's window.  You can advise this function to
change the scrollbar behavior."
  (when (window-live-p window)
    (scrollbar-set-hscroll window 0)
    (setq zmacs-region-stays t)
    nil))

(defun scrollbar-to-right (window)
  "Function called when the user gives the \"to-right\" scrollbar action.
\(The way this is done can vary from scrollbar to scrollbar.\). One argument is
passed, the scrollbar's window.  You can advise this function to
change the scrollbar behavior."
  (when (window-live-p window)
    (scrollbar-set-hscroll window 'max)
    (setq zmacs-region-stays t)
    nil))

(defun scrollbar-horizontal-drag (data)
  "Function called when the user drags the horizontal scrollbar thumb.
One argument is passed, a cons containing the scrollbar's window and a value
representing how many columns the thumb is slid over.  You can advise
this function to change the scrollbar behavior."
  (let ((window (car data))
	(value  (cdr data)))
    (when (and (window-live-p window) (integerp value))
      (scrollbar-set-hscroll window value)
      (setq zmacs-region-stays t)
      nil)))

;;; scrollbar.el ends here
