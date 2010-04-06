;;; toolbar.el --- Toolbar support for SXEmacs

;; Copyright (C) 1995, 1997 Free Software Foundation, Inc.

;; Maintainer: SXEmacs Development Team
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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This file is dumped with SXEmacs (when toolbar support is compiled in).

;;; Code:

(defcustom toolbar-visible-p ;; added for the options menu - dverna apr. 98
  (specifier-instance default-toolbar-visible-p)
  "Whether the default toolbar is globally visible.
This option can be customized through the options menu."
  :group 'display
  :type 'boolean
  :set #'(lambda (var val)
	   (set-specifier default-toolbar-visible-p val)
	   (setq toolbar-visible-p val))
  )

(defcustom toolbar-captioned-p ;; added for the options menu - dverna apr. 98
  (specifier-instance toolbar-buttons-captioned-p)
  "Whether the toolbars buttons are globally captioned.
This option can be customized through the options menu."
  :group 'display
  :type 'boolean
  :set #'(lambda (var val)
	   (set-specifier toolbar-buttons-captioned-p val)
	   (setq toolbar-captioned-p val))
  )

(defcustom default-toolbar-position ;; added for the options menu - dverna
  (default-toolbar-position)
  "The location of the default toolbar.
It can be 'top, 'bottom, 'left or 'right. This option can be
customized through the options menu."
  :group 'display
  :type '(choice (const :tag "top" top)
		 (const :tag "bottom" bottom)
		 (const :tag "left" left)
		 (const :tag "right" right))
  :set #'(lambda (var val)
	   (set-default-toolbar-position val)
	   (setq default-toolbar-position val))
  )

(defvar toolbar-help-enabled t
  "If non-nil help is echoed for toolbar buttons.")

(defvar toolbar-icon-directory nil
  "Location of standard toolbar icon bitmaps, with trailing path separator.")

(defun toolbar-make-button-list (up &optional down disabled cap-up cap-down cap-disabled)
  "Call make-glyph on each arg and return a list of the results."
  (let ((up-glyph (make-glyph up))
	    (down-glyph (and down (make-glyph down)))
	    (disabled-glyph (and disabled (make-glyph disabled)))
	    (cap-up-glyph (and cap-up (make-glyph cap-up)))
	    (cap-down-glyph (and cap-down (make-glyph cap-down)))
	    (cap-disabled-glyph (and cap-disabled (make-glyph cap-disabled))))
	(if cap-disabled
	    (list up-glyph down-glyph disabled-glyph
		  cap-up-glyph cap-down-glyph cap-disabled-glyph)
	  (if cap-down
	    (list up-glyph down-glyph disabled-glyph
		  cap-up-glyph cap-down-glyph)
	    (if cap-up
		(list up-glyph down-glyph disabled-glyph cap-up-glyph)
	      (if disabled-glyph
		  (list up-glyph down-glyph disabled-glyph)
		(if down-glyph
		    (list up-glyph down-glyph)
		  (list up-glyph))))))))

(defun init-toolbar-location ()
  (if (not toolbar-icon-directory)
      (let ((name (locate-data-directory "toolbar")))
	(if name
	    (setq toolbar-icon-directory
		  (file-name-as-directory name))))))

(defun init-toolbar-from-resources (locale)
  (if (and (featurep 'x)
	   (not (featurep 'infodock))
	   (or (eq locale 'global)
	       (eq 'x (device-or-frame-type locale))))
      (x-init-toolbar-from-resources locale)))


;; #### Is this actually needed or will the code in
;; default-mouse-motion-handler suffice?
(define-key global-map 'button1up 'release-toolbar-button)

(defvar toolbar-map (let ((m (make-sparse-keymap)))
		      (set-keymap-name m 'toolbar-map)
		      m)
  "Keymap consulted for mouse-clicks over a toolbar.")

(define-key toolbar-map 'button1 'press-toolbar-button)
(define-key toolbar-map 'button1up 'release-and-activate-toolbar-button)
(defvar last-pressed-toolbar-button nil)
(defvar toolbar-active nil)

(defvar toolbar-blank-press-function nil
  "Function to call if a blank area of the toolbar is pressed.")

;;
;; It really sucks that we also have to tie onto
;; default-mouse-motion-handler to make sliding buttons work right.
;;
(defun press-toolbar-button (event)
  "Press a toolbar button.  This only changes its appearance.
Call function stored in `toolbar-blank-press-function,' if any, with EVENT as
an argument if press is over a blank area of the toolbar."
  (interactive "_e")
  (setq this-command last-command)
  (let ((button (event-toolbar-button event)))
    ;; We silently ignore non-buttons.  This most likely means we are
    ;; over a blank part of the toolbar.
    (setq toolbar-active t)
    (if (toolbar-button-p button)
	(progn
	  (set-toolbar-button-down-flag button t)
	  (setq last-pressed-toolbar-button button))
      ;; Added by Bob Weiner, Motorola Inc., 10/6/95, to handle
      ;; presses on blank portions of toolbars.
      (when (functionp toolbar-blank-press-function)
	(funcall toolbar-blank-press-function event)))))

(defun release-and-activate-toolbar-button (event)
  "Release a toolbar button and activate its callback.
Call function stored in `toolbar-blank-release-function,' if any, with EVENT
as an argument if release is over a blank area of the toolbar."
  (interactive "_e")
  (or (button-release-event-p event)
      (error "%s must be invoked by a mouse-release" this-command))
  (release-toolbar-button event)
  (let ((button (event-toolbar-button event)))
    (if (and (toolbar-button-p button)
	     (toolbar-button-enabled-p button)
	     (toolbar-button-callback button))
	(let ((callback (toolbar-button-callback button)))
	  (setq this-command callback)
	  ;; Handle arbitrary functions.
	  (if (functionp callback)
	      (if (commandp callback)
		  (call-interactively callback)
		(funcall callback))
	    (eval callback))))))

;; If current is not t, then only release the toolbar button stored in
;; last-pressed-toolbar-button
(defun release-toolbar-button-internal (event current)
  (let ((button (event-toolbar-button event)))
    (setq zmacs-region-stays t)
    (if (and last-pressed-toolbar-button
	     (not (eq last-pressed-toolbar-button button))
	     (toolbar-button-p last-pressed-toolbar-button))
	(progn
	  (set-toolbar-button-down-flag last-pressed-toolbar-button nil)
	  (setq last-pressed-toolbar-button nil)))
    (if (and current (toolbar-button-p button))
	(set-toolbar-button-down-flag button nil))))

(defun release-toolbar-button (event)
  "Release all pressed toolbar buttons."
  (interactive "_e")
  (or (button-release-event-p event)
      (error "%s must be invoked by a mouse-release" this-command))
  (release-toolbar-button-internal event t)
  ;; Don't set this-command if we're being called
  ;; from release-and-activate-toolbar-button.
  (if (interactive-p)
      (setq this-command last-command))
  (setq toolbar-active nil))

(defun release-previous-toolbar-button (event)
  (setq zmacs-region-stays t)
  (release-toolbar-button-internal event nil))

(defun make-toolbar-specifier (spec-list)
  "Return a new `toolbar' specifier object with the given specification list.
SPEC-LIST can be a list of specifications (each of which is a cons of a
locale and a list of instantiators), a single instantiator, or a list
of instantiators.  See `make-specifier' for more information about
specifiers.

Toolbar specifiers are used to specify the format of a toolbar.
The values of the variables `default-toolbar', `top-toolbar',
`left-toolbar', `right-toolbar', and `bottom-toolbar' are always
toolbar specifiers.

Valid toolbar instantiators are called \"toolbar descriptors\"
and are lists of vectors.  See `default-toolbar' for a description
of the exact format."
  (make-specifier-and-init 'toolbar spec-list))

;;; toolbar.el ends here
