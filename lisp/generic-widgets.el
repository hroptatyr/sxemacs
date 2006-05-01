;;; generic-widgets.el --- Generic UI building

;; Copyright (C) 2000 Free Software Foundation

;; Maintainer: William Perry <wmperry@gnu.org>
;; Keywords: extensions, dumped

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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; This file is dumped with XEmacs.

(defun build-ui (ui)
  (if (null ui)
      (gtk-label-new "[empty]")
    (let ((builder-func (intern-soft (format "build-ui::%s" (car ui))))
	  (widget nil))
      (if (and builder-func (fboundp builder-func))
	  (progn
	    (setq widget (funcall builder-func ui))
	    (setcdr ui (plist-put (cdr ui) :x-internal-widget widget))
	    widget)
	(error "Unknown ui element: %s" (car ui))))))

(defun show-ui (ui)
  (let ((widget (plist-get (cdr ui) :x-internal-widget)))
    (if (not widget)
	(error "Attempting to show unrealized UI"))
    (gtk-widget-show-all widget)
    (gtk-signal-connect widget 'destroy
			(lambda (widget ui)
			  (setcdr ui (plist-put (cdr ui) :x-internal-widget nil))) ui)))


(defun build-ui::window (spec)
  "Create a top-level window for containing other widgets.
Properties:
:items		list			A list of child UI specs.  Only the first is used.
:type		toplevel/dialog/popup	What type of window to create.  Window managers
					can (and usually do) treat each type differently.
"
  (let ((plist (cdr spec))
	(window nil)
	(child nil))
    (setq window (gtk-window-new (plist-get plist :type 'toplevel))
	  child (build-ui (car (plist-get plist :items))))
    (gtk-container-add window child)
    window))

(defun build-ui::box (spec)
  "Create a box for containing other widgets.
Properties:
:items		list			A list of child UI specs.
:homogeneous	t/nil			Whether all children are the same width/height.
:spacing	number			Spacing between children.
:orientation	horizontal/vertical	How the widgets are stacked.

Additional properties on child widgets:
:expand		t/nil		Whether the new child is to be given extra space
				allocated to box. The extra space will be divided
				evenly between all children of box that use this
				option.
:fill		t/nil		Whether space given to child by the expand option is
				actually allocated to child, rather than just padding
				it. This parameter has no effect if :expand is set to
				nil. A child is always allocated the full height of a
				horizontal box and the full width of a vertical box.
				This option affects the other dimension.
:padding	number		Extra padding around this widget.
"
  (let* ((plist (cdr spec))
	 (orientation (plist-get plist :orientation 'horizontal))
	 (children (plist-get plist :items))
	 (box nil)
	 (child-widget nil)
	 (child-plist nil))
    (case orientation
      (vertical (setq box (gtk-vbox-new (plist-get plist :homogeneous)
					(plist-get plist :spacing))))
      (horizontal (setq box (gtk-hbox-new (plist-get plist :homogeneous)
					  (plist-get plist :spacing))))
      (otherwise (error "Unknown orientation for box: %s" orientation)))
    (mapc
     (lambda (child)
       (setq child-plist (cdr child)
	     child-widget (build-ui child))
       (if (listp child-widget)
	   (mapc (lambda (w)
		   (gtk-box-pack-start box w
				       (plist-get child-plist :expand)
				       (plist-get child-plist :fill)
				       (plist-get child-plist :padding))) child-widget)
	 (gtk-box-pack-start box child-widget
			     (plist-get child-plist :expand)
			     (plist-get child-plist :fill)
			     (plist-get child-plist :padding))))
     children)
    box))

(defun build-ui::tab-control (spec)
  "Create a notebook widget.
Properties:
:items		list		A list of UI specs to use as notebook pages.
:homogeneous	t/nil		Whether all tabs are the same width.
:orientation	top/bottom/left/right	Position of tabs
:show-tabs	t/nil		Show the tabs on screen?
:scrollable	t/nil		Allow scrolling to view all tab widgets?

Additional properties on child widgets:
:tab-label	ui		A UI spec to use for the tab label.
"
  (let* ((plist (cdr spec))
	 (notebook (gtk-notebook-new))
	 (children (plist-get plist :items))
	 (page-counter 1)
	 (label-widget nil)
	 (child-widget nil)
	 (child-plist nil))
    ;; Set all the properties
    (gtk-notebook-set-homogeneous-tabs notebook (plist-get plist :homogeneous))
    (gtk-notebook-set-scrollable notebook (plist-get plist :scrollable t))
    (gtk-notebook-set-show-tabs notebook (plist-get plist :show-tabs t))
    (gtk-notebook-set-tab-pos notebook (plist-get plist :orientation 'top))

    ;; Now fill in the tabs
    (mapc
     (lambda (child)
       (setq child-plist (cdr child)
	     child-widget (build-ui child)
	     label-widget (build-ui (plist-get child-plist :tab-label
					       (list 'label :text (format "tab %d" page-counter))))
	     page-counter (1+ page-counter))
       (gtk-notebook-append-page notebook child-widget label-widget))
     children)
    notebook))

(defun build-ui::text (spec)
  "Create a multi-line text widget.
Properties:
:editable	t/nil		Whether the user can change the contents
:word-wrap	t/nil		Automatic word wrapping?
:line-wrap	t/nil		Automatic line wrapping?
:text		string		Initial contents of the widget
:file		filename	File for initial contents (takes precedence over :text)
:face		facename	XEmacs face to use in the widget.
"
  (let* ((plist (cdr spec))
	 (text (gtk-text-new nil nil))
	 (face (plist-get plist :face 'default))
	 (info (plist-get plist :text))
	 (file (plist-get plist :file)))
    (gtk-text-set-editable text (plist-get plist :editable))
    (gtk-text-set-word-wrap text (plist-get plist :word-wrap))
    (gtk-text-set-line-wrap text (plist-get plist :line-wrap))
    (gtk-widget-set-style text 'default)

    ;; Possible convert the file portion
    (if (and file (not (stringp file)))
	(setq file (eval file)))

    (if (and info (not (stringp info)))
	(setq info (eval info)))

    (if (and file (file-exists-p file) (file-readable-p file))
	(save-excursion
	  (set-buffer (get-buffer-create " *improbable buffer name*"))
	  (insert-file-contents file)
	  (setq info (buffer-string))))

    (gtk-text-insert text
		     (face-font face)
		     (face-foreground face)
		     (face-background face)
		     info (length info))
    text))

(defun build-ui::label (spec)
  "Create a label widget.
Properties:
:text		string			Text inside the label
:face		facename		XEmacs face to use in the widget.
:justification  right/left/center	How to justify the text.
"
  (let* ((plist (cdr spec))
	 (label (gtk-label-new (plist-get plist :text))))
    (gtk-label-set-line-wrap label t)
    (gtk-label-set-justify label (plist-get plist :justification))
    (gtk-widget-set-style label (plist-get plist :face 'default))
    label))

(defun build-ui::pixmap (spec)
  "Create a multi-line text widget.
Properties:
:text		string			Text inside the label
:face		facename		XEmacs face to use in the widget.
:justification  right/left/center	How to justify the text.
"
  (let* ((plist (cdr spec))
	 (label (gtk-label-new (plist-get plist :text))))
    (gtk-label-set-line-wrap label t)
    (gtk-label-set-justify label (plist-get plist :justification))
    (gtk-widget-set-style label (plist-get plist :face 'default))
    label))

(defun build-ui::radio-group (spec)
  "A convenience when specifying a group of radio buttons."
  (let ((build-ui::radio-group nil))
    (mapcar 'build-ui (plist-get (cdr spec) :items))))

(defun build-ui::button (spec)
  "Create a button widget.
Properties:
:type		radio/check/toggle/nil	What type of button to create.
:text		string			Text in the button.
:glyph		glyph			Image in the button.
:label		ui			A UI spec to use for the label.
:relief		normal/half/none	How to draw button edges.

NOTE: Radio buttons must be in a radio-group object for them to work.
"
  (let ((plist (cdr spec))
	(button nil)
	(button-type (plist-get plist :type 'normal))
	(label nil))
    (case button-type
      (radio
       (if (not (boundp 'build-ui::radio-group))
	   (error "Attempt to use a radio button outside a radio-group"))
       (setq button (gtk-radio-button-new build-ui::radio-group)
	     build-ui::radio-group (gtk-radio-button-group button)))
      (check
       (setq button (gtk-check-button-new)))
      (toggle
       (setq button (gtk-toggle-button-new)))
      (normal
       (setq button (gtk-button-new)))
      (otherwise
       (error "Unknown button type: %s" button-type)))
    (gtk-container-add
     button
     (build-ui (plist-get plist :label
			  (list 'label :text
				(plist-get plist
					   :text (format "%s button" button-type))))))
    button))

(defun build-ui::progress-gauge (spec)
  "Create a progress meter.
Properties:
:orientation		left-to-right/right-to-left/top-to-bottom/bottom-to-top
:type			discrete/continuous

"
  (let ((plist (cdr spec))
	(gauge (gtk-progress-bar-new)))
    (gtk-progress-bar-set-orientation gauge (plist-get plist :orientation 'left-to-right))
    (gtk-progress-bar-set-bar-style gauge (plist-get plist :type 'continuous))
    gauge))

(provide 'generic-widgets)

(when (featurep 'gtk)			; just loading this file should be OK
(gtk-widget-show-all
  (build-ui
   '(window :type dialog
	    :items ((tab-control
		     :homogeneous t
		     :orientation bottom
		     :items ((box :orientation vertical
				  :tab-label (label :text "vertical")
				  :items ((label :text "Vertical")
					  (progress-gauge)					  
					  (label :text "Box stacking")))
			     (box :orientation horizontal
				  :spacing 10
				  :items ((label :text "Horizontal box")
					  (label :text "stacking")))

			     (box :orientation vertical
				  :items
				  ((radio-group
				    :items ((button :type radio
						    :expand nil
						    :fill nil
						    :text "Item 1")
					    (button :type radio
						    :expand nil
						    :fill nil
						    :text "Item 2")
					    (button :type radio
						    :expand nil
						    :fill nil
						    :text "Item 3")
					    (button :type radio
						    :expand nil
						    :fill nil)))))
			     (box :orientation vertical
				  :items ((button :type check
						  :text "Item 1")
					  (button :type check
						  :text "Item 2")
					  (button :type normal
						  :text "Item 3")
					  (button :type toggle)))
			     (text :editable t
				   :word-wrap t
				   :file (locate-data-file "COPYING"))
			     (text :editable t
				   :face display-time-mail-balloon-enhance-face
				   :word-wrap t
				   :text "Text with a face on it")))))))
)
