;;; gtk-password-dialog.el --- Reading passwords in a dialog

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Maintainer: William M. Perry <wmperry@gnu.org>
;; Keywords: extensions, internal

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

(defun gtk-password-dialog-ok-button (dlg)
  (get dlg 'x-ok-button))

(defun gtk-password-dialog-cancel-button (dlg)
  (get dlg 'x-cancel-button))

(defun gtk-password-dialog-entry-widget (dlg)
  (get dlg 'x-initial-entry))

(defun gtk-password-dialog-confirmation-widget (dlg)
  (get dlg 'x-verify-entry))

(defun gtk-password-dialog-new (&rest keywords)
  ;; Format is (:keyword value ...)
  ;; Allowed keywords are:
  ;;
  ;;  :callback function
  ;;  :default string
  ;;  :title string
  :;  :prompt string
  ;;  :default string
  ;;  :verify boolean
  ;;  :verify-prompt string
  (let* ((callback (plist-get keywords :callback 'ignore))
	 (dialog (gtk-dialog-new))
	 (vbox (gtk-dialog-vbox dialog))
	 (button-area (gtk-dialog-action-area dialog))
	 (default (plist-get keywords :default))
	 (widget nil))
    (gtk-window-set-title dialog (plist-get keywords :title "Enter password..."))

    ;; Make us modal...
    (put dialog 'type 'dialog)

    ;; Put the buttons in the bottom
    (setq widget (gtk-button-new-with-label "OK"))
    (gtk-container-add button-area widget)
    (gtk-signal-connect widget 'clicked
			(lambda (button data)
			  (funcall (car data)
				   (gtk-entry-get-text
				    (get (cdr data) 'x-initial-entry))))
			(cons callback dialog))
    (put dialog 'x-ok-button widget)

    (setq widget (gtk-button-new-with-label "Cancel"))
    (gtk-container-add button-area widget)
    (gtk-signal-connect widget 'clicked
			(lambda (button dialog)
			  (gtk-widget-destroy dialog))
			dialog)
    (put dialog 'x-cancel-button widget)

    ;; Now the entry area...
    (gtk-container-set-border-width vbox 5)
    (setq widget (gtk-label-new (plist-get keywords :prompt "Password:")))
    (gtk-misc-set-alignment widget 0.0 0.5)
    (gtk-container-add vbox widget)

    (setq widget (gtk-entry-new))
    (put widget 'visibility nil)
    (gtk-container-add vbox widget)
    (put dialog 'x-initial-entry widget)

    (if (plist-get keywords :verify)
	(let ((changed-cb (lambda (editable dialog)
			    (gtk-widget-set-sensitive
			     (get dialog 'x-ok-button)
			     (equal (gtk-entry-get-text
				     (get dialog 'x-initial-entry))
				    (gtk-entry-get-text
				     (get dialog 'x-verify-entry)))))))
	  (gtk-container-set-border-width vbox 5)
	  (setq widget (gtk-label-new (plist-get keywords :verify-prompt "Verify:")))
	  (gtk-misc-set-alignment widget 0.0 0.5)
	  (gtk-container-add vbox widget)

	  (setq widget (gtk-entry-new))
	  (put widget 'visibility nil)
	  (gtk-container-add vbox widget)
	  (put dialog 'x-verify-entry widget)

	  (gtk-signal-connect (get dialog 'x-initial-entry)
			      'changed changed-cb dialog)
	  (gtk-signal-connect (get dialog 'x-verify-entry)
			      'changed changed-cb dialog)
	  (gtk-widget-set-sensitive (get dialog 'x-ok-button) nil)))

    (if default
	(progn
	  (gtk-entry-set-text (get dialog 'x-initial-entry) default)
	  (gtk-entry-select-region (get dialog 'x-initial-entry)
				   0 (length default))))
    dialog))

(provide 'gtk-password-dialog)
