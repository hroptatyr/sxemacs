;;; widgets-gtk.el --- Embedded widget support for XEmacs w/GTK primitives

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Maintainer: William M. Perry <wmperry@gnu.org>
;; Keywords: extensions, internal, dumped

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

;;; Commentary:

;; This file is dumped with XEmacs (when embedded widgets are compiled in).

(defvar foo)

(defun gtk-widget-get-callback (widget plist instance)
  (let ((cb (plist-get plist :callback))
	(ex (plist-get plist :callback-ex))
	(real-cb nil))
    (cond
     (ex
      (gtk-signal-connect widget 'button-release-event
			  (lambda (widget event data)
			    (put widget 'last-event event)))
      `(lambda (widget &rest ignored)
	 (funcall ,ex ,instance (get widget 'last-event))))
     (cb
      `(lambda (widget &rest ignored)
	 (if (functionp ,real-cb)
	     (funcall ,real-cb)
	   (eval ,real-cb))))
     (t
      nil))))

(defun gtk-widget-instantiate-button-internal (plist instance)
  (let* ((type (or (plist-get plist :style) 'button))
	 (label (or (plist-get plist :descriptor) (symbol-name type)))
	 (widget nil))
    (case type
      (button
       (setq widget (gtk-button-new-with-label label))
       (gtk-signal-connect widget 'clicked
			   (gtk-widget-get-callback widget plist instance)))
      (radio
       (let ((aux nil)
	     (selected-p (plist-get plist :selected)))
	 (setq widget (gtk-radio-button-new-with-label nil label)
	       aux (gtk-radio-button-new-with-label
		    (gtk-radio-button-group widget)
		    "bogus sibling"))
	 (gtk-toggle-button-set-active widget (eval selected-p))
	 (gtk-signal-connect widget 'toggled
			     (gtk-widget-get-callback widget plist instance) aux)))
      (otherwise
       ;; Check boxes
       (setq widget (gtk-check-button-new-with-label label))
       (gtk-toggle-button-set-active widget
				     (eval (plist-get plist :selected)))
       (gtk-signal-connect widget 'toggled
			   (gtk-widget-get-callback widget plist instance))))
    (gtk-widget-show-all widget)
    widget))

(defun gtk-widget-instantiate-notebook-internal (plist callback)
  (let ((widget (gtk-notebook-new))
	(items (plist-get plist :items)))
;     (while items
;       (gtk-notebook-append-page widget
; 				(gtk-vbox-new nil 3)
; 				(gtk-label-new (aref (car items) 0)))
;       (setq items (cdr items)))
    widget))

(defun gtk-widget-instantiate-progress-internal (plist callback)
  (let* ((adj (gtk-adjustment-new 0.0 0.0 100.0 1.0 5.0 5.0))
	 (widget (gtk-progress-bar-new-with-adjustment adj)))
    (gtk-adjustment-set-value adj (or (plist-get plist :value) 0.0))
    widget))

(defun gtk-widget-instantiate-entry-internal (plist callback)
  (let* ((widget (gtk-entry-new))
	 (default (plist-get plist :descriptor)))
    (cond
     ((stringp default)
      nil)
     ((sequencep default)
      (setq default (mapconcat 'identity default "")))
     (t
      (error "Invalid default value: %S" default)))
    (gtk-entry-set-text widget default)
    widget))

(put 'button         'instantiator 'gtk-widget-instantiate-button-internal)
(put 'tab-control    'instantiator 'gtk-widget-instantiate-notebook-internal)
(put 'progress-gauge 'instantiator 'gtk-widget-instantiate-progress-internal)
(put 'tree-view      'instantiator 'ignore)
(put 'edit-field     'instantiator 'gtk-widget-instantiate-entry-internal)
(put 'combo-box      'instantiator 'ignore)
(put 'label          'instantiator 'ignore)
(put 'layout         'instantiator 'ignore)

(defun gtk-widget-instantiate-internal (instance
					instantiator
					pointer-fg
					pointer-bg
					domain)
  "The lisp side of widget/glyph instantiation code."
  (let* ((type (aref instantiator 0))
	 (plist (cdr (map 'list 'identity instantiator)))
	 (widget (funcall (or (get type 'instantiator) 'ignore)
			  plist instance)))
;     (add-timeout 0.1 (lambda (obj)
; 		       (gtk-widget-set-style obj
; 					     (gtk-widget-get-style
; 					      (frame-property nil 'text-widget))))
; 		 widget)
    (setq x widget)
    widget))

(defun gtk-widget-property-internal ()
  nil)

(defun gtk-widget-redisplay-internal ()
  nil)

(provide 'widgets-gtk)
