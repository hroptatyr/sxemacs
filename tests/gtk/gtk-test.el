;;; gtk-test.el --- Test harness for GTK widgets

;; Copyright (C) 2000 Free Software Foundation

;; Maintainer: William Perry <wmperry@gnu.org>
;; Keywords: tests

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

(require 'font)

(setq GTK_TOPLEVEL		(lsh 1 4)
      GTK_NO_WINDOW        	(lsh 1 5)
      GTK_REALIZED         	(lsh 1 6)
      GTK_MAPPED		(lsh 1 7)
      GTK_VISIBLE		(lsh 1 8)
      GTK_SENSITIVE		(lsh 1 9)
      GTK_PARENT_SENSITIVE	(lsh 1 10)
      GTK_CAN_FOCUS		(lsh 1 11)
      GTK_HAS_FOCUS		(lsh 1 12)
      GTK_CAN_DEFAULT		(lsh 1 13)
      GTK_HAS_DEFAULT		(lsh 1 14)
      GTK_HAS_GRAB		(lsh 1 15)
      GTK_RC_STYLE		(lsh 1 16)
      GTK_COMPOSITE_CHILD	(lsh 1 17)
      GTK_NO_REPARENT		(lsh 1 18)
      GTK_APP_PAINTABLE		(lsh 1 19)
      GTK_RECEIVES_DEFAULT	(lsh 1 20))

(defun gtk-widget-visible (widget)
  (= (logand (gtk-object-flags widget) GTK_VISIBLE) GTK_VISIBLE))

(defvar gtk-defined-tests nil
  "A list describing the defined tests.
Each element is of the form (DESCRIPTION TYPE FUNCTION)")

(defvar gtk-test-directory nil)
(defun gtk-test-directory ()
  (if (not gtk-test-directory)
      (mapc (lambda (c)
	      (if (and (not gtk-test-directory)
		       (string= (file-name-nondirectory (car c)) "gtk-test.el"))
		  (setq gtk-test-directory (file-name-directory (car c)))))
	    load-history))
  gtk-test-directory)

(defvar gtk-test-categories '((container . "Containers")
			      (basic     . "Basic Widgets")
			      (composite . "Composite Widgets")
			      (gimp      . "Gimp Widgets")
			      (misc      . "Miscellaneous")
			      (extra     . "GTK+ Extra")
			      (gdk       . "GDK Primitives")
			      (gnome     . "GNOME tests"))
  "An assoc list mapping test categories to friendly names.")

(defvar gtk-test-open-glyph
  (make-glyph [xpm :data "/* XPM */\nstatic char * book_open_xpm[] = {\n\"16 16 4 1\",\n\"       c None s None\",\n\".      c black\",\n\"X      c #808080\",\n\"o      c white\",\n\"                \",\n\"  ..            \",\n\" .Xo.    ...    \",\n\" .Xoo. ..oo.    \",\n\" .Xooo.Xooo...  \",\n\" .Xooo.oooo.X.  \",\n\" .Xooo.Xooo.X.  \",\n\" .Xooo.oooo.X.  \",\n\" .Xooo.Xooo.X.  \",\n\" .Xooo.oooo.X.  \",\n\"  .Xoo.Xoo..X.  \",\n\"   .Xo.o..ooX.  \",\n\"    .X..XXXXX.  \",\n\"    ..X.......  \",\n\"     ..         \",\n\"                \"};"]))

(defvar gtk-test-closed-glyph
  (make-glyph [xpm :data "/* XPM */\nstatic char * book_closed_xpm[] = {\n\"16 16 6 1\",\n\"       c None s None\",\n\".      c black\",\n\"X      c red\",\n\"o      c yellow\",\n\"O      c #808080\",\n\"#      c white\",\n\"                \",\n\"       ..       \",\n\"     ..XX.      \",\n\"   ..XXXXX.     \",\n\" ..XXXXXXXX.    \",\n\".ooXXXXXXXXX.   \",\n\"..ooXXXXXXXXX.  \",\n\".X.ooXXXXXXXXX. \",\n\".XX.ooXXXXXX..  \",\n\" .XX.ooXXX..#O  \",\n\"  .XX.oo..##OO. \",\n\"   .XX..##OO..  \",\n\"    .X.#OO..    \",\n\"     ..O..      \",\n\"      ..        \",\n\"                \"};\n"]))

(defvar gtk-test-mini-page-glyph
  (make-glyph [xpm :data "/* XPM */\nstatic char * mini_page_xpm[] = {\n\"16 16 4 1\",\n\"       c None s None\",\n\".      c black\",\n\"X      c white\",\n\"o      c #808080\",\n\"                \",\n\"   .......      \",\n\"   .XXXXX..     \",\n\"   .XoooX.X.    \",\n\"   .XXXXX....   \",\n\"   .XooooXoo.o  \",\n\"   .XXXXXXXX.o  \",\n\"   .XooooooX.o  \",\n\"   .XXXXXXXX.o  \",\n\"   .XooooooX.o  \",\n\"   .XXXXXXXX.o  \",\n\"   .XooooooX.o  \",\n\"   .XXXXXXXX.o  \",\n\"   ..........o  \",\n\"    oooooooooo  \",\n\"                \"};\n"]))

(defvar gtk-test-mini-gtk-glyph
  (make-glyph [xpm :data "/* XPM */\nstatic char * gtk_mini_xpm[] = {\n\"15 20 17 1\",\n\"       c None\",\n\".      c #14121F\",\n\"+      c #278828\",\n\"@      c #9B3334\",\n\"#      c #284C72\",\n\"$      c #24692A\",\n\"%      c #69282E\",\n\"&      c #37C539\",\n\"*      c #1D2F4D\",\n\"=      c #6D7076\",\n\"-      c #7D8482\",\n\";      c #E24A49\",\n\">      c #515357\",\n\",      c #9B9C9B\",\n\"'      c #2FA232\",\n\")      c #3CE23D\",\n\"!      c #3B6CCB\",\n\"               \",\n\"      ***>     \",\n\"    >.*!!!*    \",\n\"   ***....#*=  \",\n\"  *!*.!!!**!!# \",\n\" .!!#*!#*!!!!# \",\n\" @%#!.##.*!!$& \",\n\" @;%*!*.#!#')) \",\n\" @;;@%!!*$&)'' \",\n\" @%.%@%$'&)$+' \",\n\" @;...@$'*'*)+ \",\n\" @;%..@$+*.')$ \",\n\" @;%%;;$+..$)# \",\n\" @;%%;@$$$'.$# \",\n\" %;@@;;$$+))&* \",\n\"  %;;;@+$&)&*  \",\n\"   %;;@'))+>   \",\n\"    %;@'&#     \",\n\"     >%$$      \",\n\"      >=       \"};"]))


(defun build-option-menu (items history obj)
  (let (omenu menu menu-item group i)
    (setq omenu (gtk-option-menu-new)
	  menu (gtk-menu-new)
	  i 0)

    (while items
      (setq menu-item (gtk-radio-menu-item-new-with-label group (car (car items))))
      (gtk-signal-connect menu-item 'activate (cdr (car items)) obj)
      (setq group (gtk-radio-menu-item-group menu-item))
      (gtk-menu-append menu menu-item)
      (if (= i history)
	  (gtk-check-menu-item-set-active menu-item t))
      (gtk-widget-show menu-item)
      (setq items (cdr items))
      (incf i))

    (gtk-option-menu-set-menu omenu menu)
    (gtk-option-menu-set-history omenu history)
    omenu))

(defun gtk-test-notice-destroy (object symbol)
  ;; Set variable to NIL to aid in object destruction.
  (set symbol nil))

(defun gtk-test-make-sample-buttons (box maker)
  ;; Create buttons and pack them in a premade BOX.
  (mapcar (lambda (name)
	    (let ((button (funcall maker name)))
	      (gtk-box-pack-start box button t t 0)
	      (gtk-widget-show button)
	      button)) '("button1" "button2" "button3")))

(make-face 'gtk-test-face-large "A face with a large font, for use in GTK test cases")
(font-set-face-font 'gtk-test-face-large
	       (make-font :family '("LucidaBright" "Utopia" "Helvetica" "fixed")
			  :weight :normal
			  :size "36pt"))

(defvar gtk-test-shell nil
  "Where non-dialog tests should realize their widgets.")

(defmacro gtk-define-test (title type name-stub dialog-p &rest body)
  "Define a GTK demo/test.
TITLE is the friendly name of the test to show to the user.
TYPE is used to sort the items.
NAME-STUB is used to create the function definition.
DIALOG-P must be non-nil for demos that create their own top-level window.
BODY are the forms that actually create the demo.

They must pack their widgets into the dynamically bound WINDOW variable,
which is a GtkVBox.
"
  `(progn
     (if (not (assoc ,title gtk-defined-tests))
	 (push (list ,title (quote ,type)
		     (quote ,(intern (format "gtk-test-%s" name-stub)))) gtk-defined-tests))
     (defun ,(intern (format "gtk-test-%s" name-stub)) ()
       (let ((main-widget (if (not gtk-test-shell)
			      (gtk-window-new 'toplevel)
			    (gtk-frame-new ,title)))
	     (window nil))
	 (if gtk-test-shell
	     (progn
	       (mapc 'gtk-widget-destroy (gtk-container-children gtk-test-shell))
	       (gtk-box-pack-start gtk-test-shell main-widget nil nil 0))
	   (gtk-window-set-title main-widget ,title))
	 (if ,dialog-p
	     (let ((button (gtk-button-new-with-label ,title))
		   (blank (gtk-event-box-new)))
	       (setq window (gtk-hbox-new nil 0))
	       (gtk-signal-connect button 'clicked
				   (lambda (&rest ignored)
				     (let ((window nil))
				       ,@body
				       (gtk-widget-show-all window))))
	       (gtk-box-pack-start window
				   (gtk-label-new
				    (concat "This demo creates an external dialog.\n"
					    "Activate the button to see the demo."))
				   nil nil 0)
	       (gtk-box-pack-start window button nil nil 0)
	       (gtk-box-pack-start window blank t t 0)
	       (gtk-widget-show-all main-widget))
	   (setq window (gtk-vbox-new nil 0))
	   ,@body)
	 (gtk-container-add main-widget window)
	 (gtk-widget-show-all (or main-widget window))))))


;;;; Pixmaps
(gtk-define-test
  "Pixmaps" misc pixmap nil
  (let* ((button (gtk-button-new))
	 (pixmap (gtk-pixmap-new xemacs-logo nil))
	 (label (gtk-label-new "Pixmap test"))
	 (hbox (gtk-hbox-new nil 0)))
    (gtk-box-pack-start window button nil nil 0)
    (gtk-widget-show button)
    (gtk-container-set-border-width hbox 2)
    (gtk-container-add hbox pixmap)
    (gtk-container-add hbox label)
    (gtk-container-add button hbox)
    (gtk-widget-show pixmap)
    (gtk-widget-show label)
    (gtk-widget-show hbox)))


;;;; Scrolled windows
(gtk-define-test
 "Scrolled windows" container create-scrolled-windows nil
 (let* ((scrolled-win (gtk-scrolled-window-new nil nil))
	(viewport (gtk-viewport-new
		   (gtk-scrolled-window-get-hadjustment scrolled-win)
		   (gtk-scrolled-window-get-vadjustment scrolled-win)))
	(table (gtk-table-new 20 20 nil))
	(button nil))
   (gtk-container-set-border-width window 0)
   (gtk-container-set-border-width scrolled-win 10)
   (gtk-scrolled-window-set-policy scrolled-win 'automatic 'automatic)
   (gtk-box-pack-start window scrolled-win t t 0)
   (gtk-table-set-row-spacings table 10)
   (gtk-table-set-col-spacings table 10)
   (gtk-scrolled-window-add-with-viewport scrolled-win table)
   (gtk-container-set-focus-hadjustment 
    table (gtk-scrolled-window-get-hadjustment scrolled-win))
   (gtk-container-set-focus-vadjustment 
    table (gtk-scrolled-window-get-vadjustment scrolled-win))
   (loop for i from 0 to 19 do
     (loop for j from 0 to 19 do
       (setq button (gtk-button-new-with-label (format "button (%d, %d)\n" i j)))
       (gtk-table-attach-defaults table button i (1+ i) j (1+ j))))
   (gtk-widget-show-all scrolled-win)))


;;;; Lists
(gtk-define-test
 "List" basic create-list nil
 (let ((list-items '("hello" 
		     "world"
		     "blah"
		     "foo"
		     "bar"
		     "argh"
		     "wmperry"
		     "is a"
		     "wussy"
		     "programmer"))
       (scrolled-win (gtk-scrolled-window-new nil nil))
       (lyst (gtk-list-new))
       (add (gtk-button-new-with-label "add"))
       (remove (gtk-button-new-with-label "remove")))

   (gtk-scrolled-window-set-policy scrolled-win 'automatic 'automatic)
   (gtk-box-pack-start window scrolled-win t t 0)
   (gtk-widget-show scrolled-win)

   (gtk-list-set-selection-mode lyst 'multiple)
   (gtk-list-set-selection-mode lyst 'browse)
   (gtk-scrolled-window-add-with-viewport scrolled-win lyst)
   (gtk-widget-show lyst)

   (mapc (lambda (i)
	   (let ((list-item (gtk-list-item-new-with-label i)))
	     (gtk-container-add lyst list-item)
	     (gtk-widget-show list-item)))
	 list-items)

   (gtk-signal-connect add 'clicked
		       (lambda (obj data) (message "Should add to the list")))
   (gtk-box-pack-start window add nil t 0)
   (gtk-widget-show add)

   (gtk-signal-connect remove 'clicked
		       (lambda (obj list)
			 (if (gtk-list-selection list)
			     (gtk-list-remove-items list (gtk-list-selection list)))) lyst)
   (gtk-box-pack-start window remove nil t 0)
   (gtk-widget-show remove)

   (gtk-signal-connect lyst 'select_child 
		       (lambda (lyst child ignored)
			 (message "selected %S %d" child (gtk-list-child-position lyst child))))

   (gtk-widget-set-usize scrolled-win 200 75)

   (gtk-signal-connect lyst 'unselect_child (lambda (lyst child ignored)
					      (message "unselected %S" child)))))


;;;; Tooltips
(defvar gtk-test-tooltips nil)

(gtk-define-test
 "Tooltips" composite create-tooltips nil
  (if (not gtk-test-tooltips)
      (setq gtk-test-tooltips (gtk-tooltips-new)))
  (let ((buttons (gtk-test-make-sample-buttons window 'gtk-toggle-button-new-with-label))
	(tips '("This is button 1"
		"This is button 2"
		"This is button 3. This is also a really long tooltip which probably won't fit on a single line and will therefore need to be wrapped. Hopefully the wrapping will work correctly.")))
    (while buttons
      (gtk-tooltips-set-tip gtk-test-tooltips (pop buttons) (pop tips) ""))))


;;;; Panes
(defun toggle-resize (widget child)
  (let* ((paned (gtk-widget-parent child))
	 (is-child1 (eq child (gtk-paned-child1 paned)))
	 resize shrink)
    (setq resize (if is-child1
		     (gtk-paned-child1-resize paned)
		   (gtk-paned-child2-resize paned))
	  shrink (if is-child1
		     (gtk-paned-child1-shrink paned)
		   (gtk-paned-child2-shrink paned)))

    (gtk-widget-ref child)
    (gtk-container-remove paned child)
    (if is-child1
	(gtk-paned-pack1 paned child (not resize) shrink)
      (gtk-paned-pack2 paned child (not resize) shrink))
    (gtk-widget-unref child)))

(defun toggle-shrink (widget child)
  (let* ((paned (gtk-widget-parent child))
	 (is-child1 (eq child (gtk-paned-child1 paned)))
	 resize shrink)
    (setq resize (if is-child1
		     (gtk-paned-child1-resize paned)
		   (gtk-paned-child2-resize paned))
	  shrink (if is-child1
		     (gtk-paned-child1-shrink paned)
		   (gtk-paned-child2-shrink paned)))

    (gtk-widget-ref child)
    (gtk-container-remove paned child)
    (if is-child1
	(gtk-paned-pack1 paned child resize (not shrink))
      (gtk-paned-pack2 paned child resize (not shrink)))
    (gtk-widget-unref child)))

(defun create-pane-options (widget frame-label label1 label2)
  (let (frame table label check-button)
    (setq frame (gtk-frame-new frame-label))
    (gtk-container-set-border-width frame 4)

    (setq table (gtk-table-new 3 2 4))
    (gtk-container-add frame table)

    (setq label (gtk-label-new label1))
    (gtk-table-attach-defaults table label 0 1 0 1)

    (setq check-button (gtk-check-button-new-with-label "Resize"))
    (gtk-table-attach-defaults table check-button 0 1 1 2)
    (gtk-signal-connect check-button 'toggled 'toggle-resize (gtk-paned-child1 widget))

    (setq check-button (gtk-check-button-new-with-label "Shrink"))
    (gtk-table-attach-defaults table check-button 0 1 2 3)
    (gtk-toggle-button-set-active check-button t)
    (gtk-signal-connect check-button 'toggled 'toggle-shrink (gtk-paned-child1 widget))

    (setq label (gtk-label-new label2))
    (gtk-table-attach-defaults table label 1 2 0 1)

    (setq check-button (gtk-check-button-new-with-label "Resize"))
    (gtk-table-attach-defaults table check-button 1 2 1 2)
    (gtk-toggle-button-set-active check-button t)
    (gtk-signal-connect check-button 'toggled 'toggle-resize (gtk-paned-child2 widget))

    (setq check-button (gtk-check-button-new-with-label "Shrink"))
    (gtk-table-attach-defaults table check-button 1 2 2 3)
    (gtk-toggle-button-set-active check-button t)
    (gtk-signal-connect check-button 'toggled 'toggle-shrink (gtk-paned-child2 widget))
    frame))

(gtk-define-test
 "Panes" container panes nil
 (let (frame hpaned vpaned button vbox)
   (gtk-container-set-border-width window 0)

   (setq vpaned (gtk-vpaned-new))
   (gtk-box-pack-start window vpaned t t 0)
   (gtk-container-set-border-width vpaned 5)

   (setq hpaned (gtk-hpaned-new))
   (gtk-paned-add1 vpaned hpaned)

   (setq frame (gtk-frame-new nil))
   (gtk-frame-set-shadow-type frame 'in)
   (gtk-widget-set-usize frame 60 60)
   (gtk-paned-add1 hpaned frame)

   (setq button (gtk-button-new-with-label "Hi there"))
   (gtk-container-add frame button)

   (setq frame (gtk-frame-new nil))
   (gtk-frame-set-shadow-type frame 'in)
   (gtk-widget-set-usize frame 80 60)
   (gtk-paned-add2 hpaned frame)

   (setq frame (gtk-frame-new nil))
   (gtk-frame-set-shadow-type frame 'in)
   (gtk-widget-set-usize frame 60 80)
   (gtk-paned-add2 vpaned frame)

   ;; Now create toggle buttons to control sizing
   (gtk-box-pack-start window (create-pane-options hpaned "Horizontal" "Left" "Right") nil nil 0)
   (gtk-box-pack-start window (create-pane-options vpaned "Vertical" "Top" "Bottom") nil nil 0)
   (gtk-widget-show-all window)))


;;;; Entry
(gtk-define-test
 "Entry" basic entry nil
 (let ((box1 nil)
       (box2 nil)
       (editable-check nil)
       (sensitive-check nil)
       (entry nil)
       (cb nil)
       (button nil)
       (separator nil)
       (cbitems '("item0"
		  "item1 item1"
		  "item2 item2 item2"
		  "item3 item3 item3 item3"
		  "item4 item4 item4 item4 item4"
		  "item5 item5 item5 item5 item5 item5"
		  "item6 item6 item6 item6 item6"
		  "item7 item7 item7 item7"
		  "item8 item8 item8"
		  "item9 item9")))
   (gtk-container-set-border-width window 0)

   (setq box1 (gtk-vbox-new nil 0))
   (gtk-container-add window box1)
   (gtk-widget-show box1)

   (setq box2 (gtk-vbox-new nil 10))
   (gtk-container-set-border-width box2 10)
   (gtk-box-pack-start box1 box2 t t 0)
   (gtk-widget-show box2)

   (setq entry (gtk-entry-new))
   (gtk-entry-set-text entry "hello world")
   (gtk-editable-select-region entry 0 5)
   (gtk-box-pack-start box2 entry t t 0)
   (gtk-widget-show entry)

   (setq cb (gtk-combo-new))
   (gtk-combo-set-popdown-strings cb cbitems)
   (gtk-entry-set-text (gtk-combo-entry cb) "hellow world")
   (gtk-editable-select-region (gtk-combo-entry cb) 0 -1)
   (gtk-box-pack-start box2 cb t t 0)
   (gtk-widget-show cb)

   (setq editable-check (gtk-check-button-new-with-label "Editable"))
   (gtk-box-pack-start box2 editable-check nil t 0)
   (gtk-signal-connect editable-check 'toggled
		       (lambda (obj data)
			 (gtk-entry-set-editable
			  data
			  (gtk-toggle-button-get-active obj))) entry)
   (gtk-toggle-button-set-active editable-check t)
   (gtk-widget-show editable-check)

   (setq editable-check (gtk-check-button-new-with-label "Visible"))
   (gtk-box-pack-start box2 editable-check nil t 0)
   (gtk-signal-connect editable-check 'toggled
		       (lambda (obj data)
			 (gtk-entry-set-visibility data
						   (gtk-toggle-button-get-active obj))) entry)
   (gtk-toggle-button-set-active editable-check t)
   (gtk-widget-show editable-check)

   (setq sensitive-check (gtk-check-button-new-with-label "Sensitive"))
   (gtk-box-pack-start box2 sensitive-check nil t 0)
   (gtk-signal-connect sensitive-check 'toggled
		       (lambda (obj data)
			 (gtk-widget-set-sensitive data
						   (gtk-toggle-button-get-active obj))) entry)
   (gtk-toggle-button-set-active sensitive-check t)
   (gtk-widget-show sensitive-check)))


;;;; Various built-in dialog types
(gtk-define-test
 "Font Dialog" composite font-selection t
 (setq window (gtk-font-selection-dialog-new "font selection dialog"))
 (gtk-font-selection-dialog-set-preview-text window "Set from Emacs Lisp!")
 (gtk-signal-connect 
  (gtk-font-selection-dialog-cancel-button window)
  'clicked (lambda (button dlg)
	     (gtk-widget-destroy dlg))
  window)
 (gtk-signal-connect
  (gtk-font-selection-dialog-ok-button window)
  'clicked
  (lambda (button dlg)
    (message "Font selected: %s" (gtk-font-selection-dialog-get-font-name dlg)))
  window))

(gtk-define-test
 "File Selection Dialog" composite file-selection t
 (let (button)
   (setq window (gtk-file-selection-new "file selection"))
   (gtk-signal-connect
    (gtk-file-selection-ok-button window)
    'clicked (lambda (obj dlg) (message "You clicked ok: %s"
					(gtk-file-selection-get-filename dlg)))
    window)

    (gtk-signal-connect 
     (gtk-file-selection-cancel-button window)
     'clicked (lambda (obj dlg) (gtk-widget-destroy dlg)) window)

    (gtk-file-selection-hide-fileop-buttons window)

    (setq button (gtk-button-new-with-label "Hide Fileops"))
    (gtk-signal-connect 
     button 'clicked
     (lambda (obj dlg)
       (gtk-file-selection-hide-fileop-buttons dlg)) window)

    (gtk-box-pack-start (gtk-file-selection-action-area window)
			button nil nil 0)
    (gtk-widget-show button)

    (setq button (gtk-button-new-with-label "Show Fileops"))
    (gtk-signal-connect 
     button 'clicked
     (lambda (obj dlg)
       (gtk-file-selection-show-fileop-buttons dlg)) window)
    (gtk-box-pack-start (gtk-file-selection-action-area window)
			button nil nil 0)
    (gtk-widget-show button)))

(gtk-define-test
 "Color selection" composite color t
 (setq window (gtk-color-selection-dialog-new "GTK color selection"))
 (gtk-signal-connect (gtk-color-selection-dialog-cancel-button window)
		     'clicked
		     (lambda (button data)
		       (gtk-widget-destroy data)) window)
 (gtk-signal-connect (gtk-color-selection-dialog-ok-button window)
		     'clicked
		     (lambda (button data)
		       (let ((rgba (gtk-color-selection-get-color
				    (gtk-color-selection-dialog-colorsel data)))
			     r g b a)
			 (setq r (pop rgba)
			       g (pop rgba)
			       b (pop rgba)
			       a (pop rgba))
			 (gtk-widget-destroy data)
			 (message-box
			  "You selected color: red (%04x) blue (%04x) green (%04x) alpha (%g)"
			  (* 65535 r) (* 65535 g) (* 65535 b) a)))
		     window))


;;;; Dialog
(defun gtk-container-specific-children (parent predicate &optional data)
  (let ((children nil))
    (mapc (lambda (w)
	    (if (funcall predicate w data)
		(push w children)))
	  (gtk-container-children parent))
    children))

(gtk-define-test
 "Dialog" basic dialog t
 (let ((button nil)
       (label nil))
   (setq window (gtk-dialog-new))
   (gtk-container-set-border-width window 0)
   (gtk-widget-set-usize window 200 110)

   (setq button (gtk-button-new-with-label "OK"))
   (gtk-box-pack-start (gtk-dialog-action-area window) button t t 0)
   (gtk-widget-show button)
   (gtk-signal-connect button 'clicked
		       (lambda (obj data)
			 (gtk-widget-destroy data))
		       window)

   (setq button (gtk-button-new-with-label "Toggle"))
   (gtk-signal-connect
    button 'clicked
    (lambda (button dlg)
      (if (not (gtk-container-specific-children (gtk-dialog-vbox dlg)
						(lambda (w ignored)
						  (= (gtk-object-type w) (gtk-label-get-type)))))
	  (let ((label (gtk-label-new "Dialog Test")))
	    (gtk-box-pack-start (gtk-dialog-vbox dlg) label t t 0)
	    (gtk-widget-show label))
	(mapc 'gtk-widget-destroy
	      (gtk-container-specific-children (gtk-dialog-vbox dlg)
					       (lambda (w ignored)
						 (= (gtk-object-type w) (gtk-label-get-type)))))))
    window)
   (gtk-box-pack-start (gtk-dialog-action-area window) button t t 0)
   (gtk-widget-show button)))


;;;; Range controls
(gtk-define-test
 "Range Controls" basic range-controls nil
 (let* ((adjustment (gtk-adjustment-new 0.0 0.0 101.0 0.1 1.0 1.0))
	(scale (gtk-hscale-new adjustment))
	(scrollbar (gtk-hscrollbar-new adjustment)))
    (gtk-widget-set-usize scale 150 30)
    (gtk-range-set-update-policy scale 'delayed)
    (gtk-scale-set-digits scale 2)
    (gtk-scale-set-draw-value scale t)
    (gtk-box-pack-start window scale t t 0)
    (gtk-widget-show scale)

    (gtk-range-set-update-policy scrollbar 'continuous)
    (gtk-box-pack-start window scrollbar t t 0)
    (gtk-widget-show scrollbar)))


;;;; Ruler
'(gtk-define-test
 "Rulers" gimp rulers nil
 (let* ((table (gtk-table-new 2 2 nil))
	(hruler nil)
	(vruler nil)
	(ebox (gtk-event-box-new)))

   (gtk-widget-set-usize ebox 300 300)
   (gtk-widget-set-events ebox '(pointer-motion-mask pointer-motion-hint-mask))
   (gtk-container-set-border-width ebox 0)

   (gtk-container-add window ebox)
   (gtk-container-add ebox table)
   (gtk-widget-show table)

   (setq hruler (gtk-hruler-new))
   (gtk-ruler-set-metric hruler 'centimeters)
   (gtk-ruler-set-range hruler 100 0 0 20)
   (gtk-table-attach table hruler 1 2 0 1 '(expand fill) 'fill 0 0)
   (gtk-widget-show hruler)

   (setq vruler (gtk-vruler-new))
   (gtk-ruler-set-range vruler 5 15 0 20)
   (gtk-table-attach table vruler 0 1 1 2 'fill '(expand fill) 0 0)
   (gtk-widget-show vruler)

   (gtk-signal-connect 
    ebox 'motion_notify_event
    (lambda (object ev data)
      (gtk-widget-event (car data) ev)
      (gtk-widget-event (cdr data) ev))
    (cons hruler vruler))))


;;;; Toggle button types
(gtk-define-test
 "Toggle Buttons" basic toggle-buttons nil
 (gtk-container-set-border-width window 0)
 (gtk-test-make-sample-buttons window 'gtk-toggle-button-new-with-label))

(gtk-define-test
 "Check Buttons" basic check-buttons nil
 (gtk-container-set-border-width window 0)
 (gtk-test-make-sample-buttons window 'gtk-check-button-new-with-label))

(gtk-define-test
 "Radio Buttons" basic radio-buttons nil
 (gtk-container-set-border-width window 0)
 (let ((group nil))
   (gtk-test-make-sample-buttons window
				 (lambda (label)
				   (let ((button (gtk-radio-button-new-with-label group label)))
				     (setq group (gtk-radio-button-group button))
				     button)))))


;;;; Button weirdness
(gtk-define-test
 "Buttons" basic buttons nil
 (let ((box1 nil)
       (box2 nil)
       (table nil)
       (buttons nil)
       (separator nil)
       (connect-buttons (lambda (button1 button2)
			  (gtk-signal-connect button1 'clicked
					      (lambda (obj data)
						(if (gtk-widget-visible data)
						    (gtk-widget-hide data)
						  (gtk-widget-show data))) button2))))

   (gtk-container-set-border-width window 0)

   (setq box1 (gtk-vbox-new nil 0))
   (gtk-container-add window box1)

   (setq table (gtk-table-new 3 3 nil))
   (gtk-table-set-row-spacings table 5)
   (gtk-table-set-col-spacings table 5)
   (gtk-container-set-border-width table 10)
   (gtk-box-pack-start box1 table t t 0)

   (push (gtk-button-new-with-label "button9") buttons)
   (push (gtk-button-new-with-label "button8") buttons)
   (push (gtk-button-new-with-label "button7") buttons)
   (push (gtk-button-new-with-label "button6") buttons)
   (push (gtk-button-new-with-label "button5") buttons)
   (push (gtk-button-new-with-label "button4") buttons)
   (push (gtk-button-new-with-label "button3") buttons)
   (push (gtk-button-new-with-label "button2") buttons)
   (push (gtk-button-new-with-label "button1") buttons)

   (funcall connect-buttons (nth 0 buttons) (nth 1 buttons))
   (funcall connect-buttons (nth 1 buttons) (nth 2 buttons))
   (funcall connect-buttons (nth 2 buttons) (nth 3 buttons))
   (funcall connect-buttons (nth 3 buttons) (nth 4 buttons))
   (funcall connect-buttons (nth 4 buttons) (nth 5 buttons))
   (funcall connect-buttons (nth 5 buttons) (nth 6 buttons))
   (funcall connect-buttons (nth 6 buttons) (nth 7 buttons))
   (funcall connect-buttons (nth 7 buttons) (nth 8 buttons))
   (funcall connect-buttons (nth 8 buttons) (nth 0 buttons))

   (gtk-table-attach table (nth 0 buttons) 0 1 0 1 '(expand fill) '(expand fill) 0 0)
   (gtk-table-attach table (nth 1 buttons) 1 2 1 2 '(expand fill) '(expand fill) 0 0)
   (gtk-table-attach table (nth 2 buttons) 2 3 2 3 '(expand fill) '(expand fill) 0 0)
   (gtk-table-attach table (nth 3 buttons) 0 1 2 3 '(expand fill) '(expand fill) 0 0)
   (gtk-table-attach table (nth 4 buttons) 2 3 0 1 '(expand fill) '(expand fill) 0 0)
   (gtk-table-attach table (nth 5 buttons) 1 2 2 3 '(expand fill) '(expand fill) 0 0)
   (gtk-table-attach table (nth 6 buttons) 1 2 0 1 '(expand fill) '(expand fill) 0 0)
   (gtk-table-attach table (nth 7 buttons) 2 3 1 2 '(expand fill) '(expand fill) 0 0)
   (gtk-table-attach table (nth 8 buttons) 0 1 1 2 '(expand fill) '(expand fill) 0 0)
   ))


;;;; Testing labels and underlining
(gtk-define-test
 "Labels" basic labels nil
 (let ((hbox (gtk-hbox-new nil 5))
       (vbox (gtk-vbox-new nil 5))
       (frame nil)
       (label nil))
   (gtk-container-add window hbox)
   (gtk-box-pack-start hbox vbox nil nil 0)
   (gtk-container-set-border-width window 5)

   (setq frame (gtk-frame-new "Normal Label")
	 label (gtk-label-new "This is a Normal label"))
   (gtk-container-add frame label)
   (gtk-box-pack-start vbox frame nil nil 0)

   (setq frame (gtk-frame-new "Multi-line Label")
	 label (gtk-label-new "This is a multi-line label.\nSecond line\nThird line"))
   (gtk-container-add frame label)
   (gtk-box-pack-start vbox frame nil nil 0)

   (setq frame (gtk-frame-new "Left Justified Label")
	 label (gtk-label-new "This is a Left-Justified\nMulti-line label.\nThird      line"))
   (gtk-label-set-justify label 'left)
   (gtk-container-add frame label)
   (gtk-box-pack-start vbox frame nil nil 0)

   (setq frame (gtk-frame-new "Right Justified Label")
	 label (gtk-label-new "This is a Right-Justified\nMulti-line label.\nFourth line, (j/k)"))
   (gtk-label-set-justify label 'right)
   (gtk-container-add frame label)
   (gtk-box-pack-start vbox frame nil nil 0)

   ;; Start a second row so that we don't make a ridiculously tall window
   (setq vbox (gtk-vbox-new nil 5))
   (gtk-box-pack-start hbox vbox nil nil 0)

   (setq frame (gtk-frame-new "Line wrapped label")
	 label (gtk-label-new
		(concat "This is an example of a line-wrapped label.  It should not be taking "
			"up the entire             " ;;; big space to test spacing
			"width allocated to it, but automatically wraps the words to fit.  "
			"The time has come, for all good men, to come to the aid of their party.  "
			"The sixth sheik's six sheep's sick.\n"
			"     It supports multiple paragraphs correctly, and  correctly   adds "
			"many          extra  spaces. ")))
   (gtk-label-set-line-wrap label t)
   (gtk-container-add frame label)
   (gtk-box-pack-start vbox frame nil nil 0)

   (setq frame (gtk-frame-new "Filled, wrapped label")
	 label (gtk-label-new
		(concat
		 "This is an example of a line-wrapped, filled label.  It should be taking "
		 "up the entire              width allocated to it.  Here is a seneance to prove "
		 "my point.  Here is another sentence. "
		 "Here comes the sun, do de do de do.\n"
		 "    This is a new paragraph.\n"
		 "    This is another newer, longer, better paragraph.  It is coming to an end, "
		 "unfortunately.")))
   (gtk-label-set-justify label 'fill)
   (gtk-label-set-line-wrap label t)
   (gtk-container-add frame label)
   (gtk-box-pack-start vbox frame nil nil 0)

   (setq frame (gtk-frame-new "Underlined label")
	 label (gtk-label-new (concat "This label is underlined!\n"
				      "This one is underlined in 日本語の入用quite a funky fashion")))
   (gtk-label-set-justify label 'left)
   (gtk-label-set-pattern label "_________________________ _ _________ _ _____ _ __ __  ___ ____ _____")
   (gtk-container-add frame label)
   (gtk-box-pack-start vbox frame nil nil 0)))


;;;; Progress gauges
(gtk-define-test
 "Progress bars" basic progress nil
 (let* ((timer nil)
	(adj (gtk-adjustment-new 1 0 100 1 1 1))
	(label (gtk-label-new "progress..."))
	(pbar (gtk-progress-bar-new-with-adjustment adj))
	(button nil)
	(timer (make-itimer)))

   ;; The original test used GTK timers, but XEmacs already has
   ;; perfectly good timer support, that ends up mapping onto GTK
   ;; timers anyway, so we'll use those instead.
   (set-itimer-function
    timer
    (lambda (bar adj)
      (let ((val (gtk-adjustment-value adj)))
	(setq val (+ 1 (if (>= val 100) 0 val)))
	(gtk-adjustment-set-value adj val)
	(gtk-widget-queue-draw bar))))

   (set-itimer-function-arguments timer (list pbar adj))
   (set-itimer-uses-arguments timer t)
   (set-itimer-restart timer 0.1)
   (set-itimer-value timer 0.1)
   (set-itimer-is-idle timer nil)

   (gtk-progress-set-format-string pbar "%v%%")
   (gtk-signal-connect pbar 'destroy (lambda (obj timer) 
				       (delete-itimer timer)) timer)

   (gtk-misc-set-alignment label 0 0.5)
   (gtk-box-pack-start window label nil t 0)
   (gtk-widget-show label)
   (gtk-widget-set-usize pbar 200 20)
   (gtk-box-pack-start window pbar t t 0)

   (setq button (gtk-check-button-new-with-label "Show text"))
   (gtk-box-pack-start window button nil nil 0)
   (gtk-signal-connect button 'clicked
		       (lambda (button bar)
			 (gtk-progress-set-show-text
			  bar
			  (gtk-toggle-button-get-active button))) pbar)
   (gtk-widget-show button)

   (setq button (gtk-check-button-new-with-label "Discrete blocks"))
   (gtk-box-pack-start window button nil nil 0)
   (gtk-signal-connect button 'clicked
		       (lambda (button bar)
			 (gtk-progress-bar-set-bar-style
			  bar
			  (if (gtk-toggle-button-get-active button)
			      'discrete
			    'continuous))) pbar)
   (gtk-widget-show button)

   (gtk-widget-show pbar)

   (activate-itimer timer)))

(gtk-define-test
 "Gamma Curve" gimp gamma-curve nil
 (let ((curve (gtk-gamma-curve-new)))
   (gtk-container-add window curve)
   (gtk-widget-show-all curve)
   (gtk-curve-set-range (gtk-gamma-curve-curve curve) 0 255 0 255)
   (gtk-curve-set-gamma (gtk-gamma-curve-curve curve) 2)))


;;;; Testing various button boxes and layout strategies.
(gtk-define-test
 "Button Box" container button-box nil
 (let ((main-vbox (gtk-vbox-new nil 0))
       (vbox (gtk-vbox-new nil 0))
       (hbox (gtk-hbox-new nil 0))
       (frame-horz (gtk-frame-new "Horizontal Button Boxes"))
       (frame-vert (gtk-frame-new "Vertical Button Boxes"))
       (create-bbox (lambda (horizontal title spacing child-w child-h layout)
		      (let ((frame (gtk-frame-new title))
			    (bbox (if horizontal
				      (gtk-hbutton-box-new)
				    (gtk-vbutton-box-new))))
			(gtk-container-set-border-width bbox 5)
			(gtk-container-add frame bbox)
			(gtk-button-box-set-layout bbox layout)
			(gtk-button-box-set-spacing bbox spacing)
			(gtk-button-box-set-child-size bbox child-w child-h)
			(gtk-container-add bbox (gtk-button-new-with-label "OK"))
			(gtk-container-add bbox (gtk-button-new-with-label "Cancel"))
			(gtk-container-add bbox (gtk-button-new-with-label "Help"))
			frame))))

   (gtk-container-set-border-width window 10)
   (gtk-container-add window main-vbox)

   (gtk-box-pack-start main-vbox frame-horz t t 10)
   (gtk-container-set-border-width vbox 10)
   (gtk-container-add frame-horz vbox)

   (gtk-box-pack-start main-vbox frame-vert t t 10)
   (gtk-container-set-border-width hbox 10)
   (gtk-container-add frame-vert hbox)

   (gtk-box-pack-start vbox (funcall create-bbox t "Spread" 40 85 20 'spread) t t 0)
   (gtk-box-pack-start vbox (funcall create-bbox t "Edge" 40 85 20 'edge) t t 0)
   (gtk-box-pack-start vbox (funcall create-bbox t "Start" 40 85 20 'start) t t 0)
   (gtk-box-pack-start vbox (funcall create-bbox t "End" 40 85 20 'end) t t 0)

   (gtk-box-pack-start hbox (funcall create-bbox nil "Spread" 40 85 20 'spread) t t 0)
   (gtk-box-pack-start hbox (funcall create-bbox nil "Edge" 40 85 20 'edge) t t 0)
   (gtk-box-pack-start hbox (funcall create-bbox nil "Start" 40 85 20 'start) t t 0)
   (gtk-box-pack-start hbox (funcall create-bbox nil "End" 40 85 20 'end) t t 0)))


;;;; Cursors
'(gtk-define-test
  "Cursors" cursors nil
  (let ((cursors '(x-cursor arrow based-arrow-down based-arrow-up boat bogosity
			    bottom-left-corner bottom-right-corner bottom-side bottom-tee
			    box-spiral center-ptr circle clock coffee-mug cross cross-reverse
			    crosshair diamond-cross dot dotbox double-arrow draft-large
			    draft-small draped-box exchange fleur gobbler gumby hand1 hand2 heart
			    icon iron-cross left-ptr left-side left-tee leftbutton ll-angle
			    lr-angle man middlebutton mouse pencil pirate plus question-arrow
			    right-ptr right-side right-tee rightbutton rtl-logo sailboat
			    sb-down-arrow sb-h-double-arrow sb-left-arrow sb-right-arrow
			    sb-up-arrow sb-v-double-arrow shuttle sizing spider spraycan star
			    target tcross top-left-arrow top-left-corner top-right-corner top-side
			    top-tee trek ul-angle umbrella ur-angle watch xterm last-cursor))
	(cursor-area nil)
	(adjustment nil)
	(spinner nil))
    (setq cursor-area (gtk-event-box-new)
	  adjustment (gtk-adjustment-new 0 0 (length cursors) 1 1 1)
	  spinner (gtk-spin-button-new adjustment 1 3))
    (gtk-widget-set-usize cursor-area 200 100)
    (gtk-box-pack-start window cursor-area t t 0)
    (gtk-box-pack-start window spinner nil nil 0)))


;;;; Toolbar
(defun gtk-test-toolbar-create ()
  (let ((toolbar (gtk-toolbar-new 'horizontal 'both)))
    (gtk-toolbar-set-button-relief toolbar 'none)

    (gtk-toolbar-append-item toolbar
			     "Horizonal" "Horizontal toolbar layout" "Toolbar/Horizontal"
			     (gtk-pixmap-new gtk-test-open-glyph nil)
			     (lambda (tbar)
			       (gtk-toolbar-set-orientation tbar 'horizontal)) toolbar)
    (gtk-toolbar-append-item toolbar
			     "Vertical" "Vertical toolbar layout" "Toolbar/Vertical"
			     (gtk-pixmap-new gtk-test-open-glyph nil)
			     (lambda (tbar)
			       (gtk-toolbar-set-orientation tbar 'vertical)) toolbar)

    (gtk-toolbar-append-space toolbar)
    (gtk-toolbar-append-item toolbar
			     "Icons" "Only show toolbar icons" "Toolbar/IconsOnly"
			     (gtk-pixmap-new gtk-test-open-glyph nil)
			     (lambda (tbar)
			       (gtk-toolbar-set-style tbar 'icons)) toolbar)
    (gtk-toolbar-append-item toolbar
			     "Text" "Only show toolbar text" "Toolbar/TextOnly"
			     (gtk-pixmap-new gtk-test-open-glyph nil)
			     (lambda (tbar)
			       (gtk-toolbar-set-style tbar 'text)) toolbar)
    (gtk-toolbar-append-item toolbar
			     "Both" "Show toolbar icons and text" "Toolbar/Both"
			     (gtk-pixmap-new gtk-test-open-glyph nil)
			     (lambda (tbar)
			       (gtk-toolbar-set-style tbar 'both)) toolbar)

    (gtk-toolbar-append-space toolbar)
    (gtk-toolbar-append-item toolbar
			     "Small" "Use small spaces" ""
			     (gtk-pixmap-new gtk-test-open-glyph nil)
			     (lambda (tbar)
			       (gtk-toolbar-set-space-size tbar 5)) toolbar)
    (gtk-toolbar-append-item toolbar
			     "Big" "Use big spaces" ""
			     (gtk-pixmap-new gtk-test-open-glyph nil)
			     (lambda (tbar)
			       (gtk-toolbar-set-space-size tbar 10)) toolbar)

    (gtk-toolbar-append-space toolbar)
    (gtk-toolbar-append-item toolbar
			     "Enable" "Enable tooltips" ""
			     (gtk-pixmap-new gtk-test-open-glyph nil)
			     (lambda (tbar)
			       (gtk-toolbar-set-tooltips tbar t)) toolbar)
    (gtk-toolbar-append-item toolbar
			     "Disable" "Disable tooltips" ""
			     (gtk-pixmap-new gtk-test-open-glyph nil)
			     (lambda (tbar)
			       (gtk-toolbar-set-tooltips tbar nil)) toolbar)

    (gtk-toolbar-append-space toolbar)
    (gtk-toolbar-append-item toolbar
			     "Borders" "Show borders" ""
			     (gtk-pixmap-new gtk-test-open-glyph nil)
			     (lambda (tbar)
			       (gtk-toolbar-set-button-relief tbar 'normal)) toolbar)
    (gtk-toolbar-append-item toolbar
			     "Borderless" "Hide borders" ""
			     (gtk-pixmap-new gtk-test-open-glyph nil)
			     (lambda (tbar)
			       (gtk-toolbar-set-button-relief tbar 'none)) toolbar)

    (gtk-toolbar-append-space toolbar)
    (gtk-toolbar-append-item toolbar
			     "Empty" "Empty spaces" ""
			     (gtk-pixmap-new gtk-test-open-glyph nil)
			     (lambda (tbar)
			       (gtk-toolbar-set-space-style tbar 'empty)) toolbar)
    (gtk-toolbar-append-item toolbar
			     "Lines" "Lines in spaces" ""
			     (gtk-pixmap-new gtk-test-open-glyph nil)
			     (lambda (tbar)
			       (gtk-toolbar-set-space-style tbar 'line)) toolbar)
    (gtk-widget-show-all toolbar)
    toolbar))

(gtk-define-test
 "Toolbar" container toolbar nil
 (gtk-box-pack-start window (gtk-test-toolbar-create) t t 0))


;;;; Text
(gtk-define-test
 "Text" composite text nil
 (let ((text (gtk-text-new nil nil))
       (scrolled (gtk-scrolled-window-new nil nil))
       (bbox (gtk-hbutton-box-new))
       (button nil))
   (gtk-box-pack-start window scrolled t t 0)
   (gtk-box-pack-start window bbox nil nil 0)
   (gtk-widget-set-usize text 500 500)
   (gtk-container-add scrolled text)

   (setq button (gtk-check-button-new-with-label "Editable"))
   (gtk-signal-connect button 'toggled
		       (lambda (button text)
			 (gtk-text-set-editable text (gtk-toggle-button-get-active button))) text)
   (gtk-container-add bbox button)

   (setq button (gtk-check-button-new-with-label "Wrap words"))
   (gtk-signal-connect button 'toggled
		       (lambda (button text)
			 (gtk-text-set-word-wrap text (gtk-toggle-button-get-active button))) text)
   (gtk-container-add bbox button)

   ;; put some default text in there.
   (gtk-widget-set-style text 'default)
   (let ((faces '(blue bold bold-italic gtk-test-face-large red text-cursor))
	 (string nil))
     (mapc (lambda (face)
	     (setq string (format "Sample text in the `%s' face\n" face))
	     (gtk-text-insert text
			      (face-font face)
			      (face-foreground face)
			      (face-background face)
			      string (length string))) faces))


   ;; Tell the user their rights...
   (let ((file (locate-data-file "COPYING")))
     (gtk-text-freeze text)
     (save-excursion
       (set-buffer (get-buffer-create " *foo*"))
       (insert-file-contents file)
       (gtk-text-insert text nil nil nil (buffer-string) (point-max))
       (kill-buffer (current-buffer))))
     (gtk-text-thaw text)))


;;;; handle box
(gtk-define-test
 "Handle box" container handles nil
 (let ((handle nil)
       (hbox (gtk-hbox-new nil 0)))

   (gtk-box-pack-start window (gtk-label-new "Above") nil nil 0)
   (gtk-box-pack-start window (gtk-hseparator-new) nil nil 0)
   (gtk-box-pack-start window hbox t t 0)
   (gtk-box-pack-start window (gtk-hseparator-new) nil nil 0)
   (gtk-box-pack-start window (gtk-label-new "Below") nil nil 0)
   
   (setq handle (gtk-handle-box-new))
   (gtk-container-add handle (gtk-test-toolbar-create))
   (gtk-widget-show-all handle)
   (gtk-box-pack-start hbox handle nil nil 0)
   (gtk-signal-connect handle 'child_attached
		       (lambda (box child data)
			 (message "Child widget (%s) attached" child)))
   (gtk-signal-connect handle 'child_detached
		       (lambda (box child data)
			 (message "Child widget (%s) detached" child)))

   (setq handle (gtk-handle-box-new))
   (gtk-container-add handle (gtk-label-new "Fooo!!!"))
   (gtk-box-pack-start hbox handle nil nil 0)
   (gtk-signal-connect handle 'child_attached
		       (lambda (box child data)
			 (message "Child widget (%s) attached" child)))
   (gtk-signal-connect handle 'child_detached
		       (lambda (box child data)
			 (message "Child widget (%s) detached" child)))))


;;;; Menus
(gtk-define-test
 "Menus" basic menus nil
 (let ((menubar (gtk-menu-bar-new))
       (item nil)
       (right-justify nil))
   (gtk-box-pack-start window menubar nil nil 0)
   (mapc (lambda (menudesc)
	   (if (not menudesc)
	       (setq right-justify t)
	     (setq item (gtk-build-xemacs-menu menudesc))
	     (gtk-widget-show item)
	     (if right-justify
		 (gtk-menu-item-right-justify item))
	     (gtk-menu-bar-append menubar item)))
	 default-menubar)))


;;;; Spinbutton
(gtk-define-test
 "Spinbutton" composite spinbutton nil
 (let (frame vbox vbox2 hbox label spin adj spin2 button)

   (gtk-container-set-border-width window 5)

   (setq frame (gtk-frame-new "Not accelerated")
	 hbox (gtk-hbox-new nil 0))

   (gtk-box-pack-start window frame t t 0)
   (gtk-container-add frame hbox)

   (setq vbox (gtk-vbox-new nil 0)
	 label (gtk-label-new "Day:")
	 adj (gtk-adjustment-new 1.0 1.0 31.0 1.0 5.0 0.0)
	 spin (gtk-spin-button-new adj 0 0))

   (gtk-misc-set-alignment label 0 0.5)
   (gtk-spin-button-set-wrap spin t)
   (gtk-spin-button-set-shadow-type spin 'out)
   (gtk-box-pack-start hbox vbox t t 5)
   (gtk-box-pack-start vbox label nil t 0)
   (gtk-box-pack-start vbox spin nil t 0)

   (setq vbox (gtk-vbox-new nil 0)
	 label (gtk-label-new "Month:")
	 adj (gtk-adjustment-new 1.0 1.0 12.0 1.0 5.0 0.0)
	 spin (gtk-spin-button-new adj 0 0))
   (gtk-misc-set-alignment label 0 0.5)
   (gtk-spin-button-set-wrap spin t)
   (gtk-spin-button-set-shadow-type spin 'out)
   (gtk-box-pack-start hbox vbox t t 5)
   (gtk-box-pack-start vbox label nil t 0)
   (gtk-box-pack-start vbox spin nil t 0)

   (setq vbox (gtk-vbox-new nil 0)
	 label (gtk-label-new "Year:")
	 adj (gtk-adjustment-new 1998.0 0.0 2100.0 1.0 100.0 0.0)
	 spin (gtk-spin-button-new adj 0 0))
   (gtk-misc-set-alignment label 0 0.5)
   (gtk-spin-button-set-wrap spin t)
   (gtk-spin-button-set-shadow-type spin 'out)
   (gtk-widget-set-usize spin 55 0)
   (gtk-box-pack-start hbox vbox t t 5)
   (gtk-box-pack-start vbox label nil t 0)
   (gtk-box-pack-start vbox spin nil t 0)

   (setq frame (gtk-frame-new "Accelerated")
	 vbox (gtk-vbox-new nil 0))

   (gtk-box-pack-start window frame t t 0)
   (gtk-container-add frame vbox)

   (setq hbox (gtk-hbox-new nil 0))
   (gtk-box-pack-start vbox hbox nil t 5)

   (setq vbox2 (gtk-vbox-new nil 0)
	 label (gtk-label-new "Value:")
	 adj (gtk-adjustment-new 0.0 -10000.0 10000.0 0.5 100.0 0.0)
	 spin (gtk-spin-button-new adj 1.0 2))
   (gtk-misc-set-alignment label 0 0.5)
   (gtk-spin-button-set-wrap spin t)
   (gtk-widget-set-usize spin 100 0)
   (gtk-box-pack-start vbox2 label nil t 0)
   (gtk-box-pack-start vbox2 spin nil t 0)
   (gtk-box-pack-start hbox vbox2 t t 0)

   (setq vbox2 (gtk-vbox-new nil 0)
	 label (gtk-label-new "Digits:")
	 adj (gtk-adjustment-new 2 1 5 1 1 0)
	 spin2 (gtk-spin-button-new adj 0 0))
   (gtk-misc-set-alignment label 0 0.5)
   (gtk-spin-button-set-wrap spin2 t)
   (gtk-widget-set-usize spin2 100 0)
   (gtk-box-pack-start vbox2 label nil t 0)
   (gtk-box-pack-start vbox2 spin2 nil t 0)
   (gtk-box-pack-start hbox vbox2 t t 0)
   (gtk-signal-connect adj 'value_changed
		       (lambda (adj spinners)
			 (gtk-spin-button-set-digits
			  (car spinners)
			  (gtk-spin-button-get-value-as-int (cdr spinners))))
		       (cons spin spin2))

   (setq button (gtk-check-button-new-with-label "Snap to 0.5-ticks"))
   (gtk-signal-connect button 'clicked
		       (lambda (button spin)
			 (gtk-spin-button-set-snap-to-ticks
			  spin
			  (gtk-toggle-button-get-active button)))
		       spin)
   (gtk-box-pack-start vbox button t t 0)
   (gtk-toggle-button-set-active button t)

   (setq button (gtk-check-button-new-with-label "Numeric only input mode"))
   (gtk-signal-connect button 'clicked
		       (lambda (button spin)
			 (gtk-spin-button-set-numeric
			  spin
			  (gtk-toggle-button-get-active button)))
		       spin)
   (gtk-box-pack-start vbox button t t 0)
   (gtk-toggle-button-set-active button t)

   (setq label (gtk-label-new ""))

   (setq hbox (gtk-hbutton-box-new))
   (gtk-box-pack-start vbox hbox nil t 5)
   (gtk-box-pack-start vbox label nil nil 5)

   (setq button (gtk-button-new-with-label "Value as int"))
   (gtk-container-add hbox button)
   (gtk-signal-connect button 'clicked
		       (lambda (obj data)
			 (let ((spin (car data))
			       (label (cdr data)))
			   (gtk-label-set-text label
					       (format "%d"
						       (gtk-spin-button-get-value-as-int spin)))))
		       (cons spin label))

   (setq button (gtk-button-new-with-label "Value as float"))
   (gtk-container-add hbox button)
   (gtk-signal-connect button 'clicked
		       (lambda (obj data)
			 (let ((spin (car data))
			       (label (cdr data)))
			   (gtk-label-set-text label
					       (format "%g"
						       (gtk-spin-button-get-value-as-float spin)))))
		       (cons spin label))))


;;;; Reparenting
(gtk-define-test
 "Reparenting" misc reparenting nil
 (let ((label (gtk-label-new "Hello World"))
       (frame-1 (gtk-frame-new "Frame 1"))
       (frame-2 (gtk-frame-new "Frame 2"))
       (button nil)
       (hbox (gtk-hbox-new nil 5))
       (vbox-1 nil)
       (vbox-2 nil)
       (reparent-func (lambda (button data)
			 (let ((label (car data))
			       (new-parent (cdr data)))
			   (gtk-widget-reparent label new-parent)))))
	
   (gtk-box-pack-start window hbox t t 0)
   (gtk-box-pack-start hbox frame-1 t t 0)
   (gtk-box-pack-start hbox frame-2 t t 0)

   (setq vbox-1 (gtk-vbox-new nil 0))
   (gtk-container-add frame-1 vbox-1)
   (setq vbox-2 (gtk-vbox-new nil 0))
   (gtk-container-add frame-2 vbox-2)

   (setq button (gtk-button-new-with-label "switch"))
   (gtk-box-pack-start vbox-1 button nil nil 0)
   (gtk-signal-connect button 'clicked reparent-func (cons label vbox-2))
   
   (setq button (gtk-button-new-with-label "switch"))
   (gtk-box-pack-start vbox-2 button nil nil 0)
   (gtk-signal-connect button 'clicked reparent-func (cons label vbox-1))

   (gtk-box-pack-start vbox-2 label nil t 0)))


;;;; StatusBar
(defvar statusbar-counter 1)

(gtk-define-test
 "Statusbar" composite statusbar nil
 (let ((bar (gtk-statusbar-new))
       (vbox nil)
       (button nil))

   (setq vbox (gtk-vbox-new nil 0))
   (gtk-box-pack-start window vbox t t 0)
   (gtk-box-pack-end window bar t t 0)

   (setq button (gtk-button-new-with-label "push something"))
   (gtk-box-pack-start-defaults vbox button)
   (gtk-signal-connect button 'clicked
		       (lambda (button bar)
			 (gtk-statusbar-push bar 1 (format "something %d" (incf statusbar-counter))))
		       bar)

   (setq button (gtk-button-new-with-label "pop"))
   (gtk-box-pack-start-defaults vbox button)
   (gtk-signal-connect button 'clicked
		       (lambda (button bar)
			 (gtk-statusbar-pop bar 1)) bar)

   (setq button (gtk-button-new-with-label "steal #4"))
   (gtk-box-pack-start-defaults vbox button)
   (gtk-signal-connect button 'clicked
		       (lambda (button bar)
			 (gtk-statusbar-remove bar 1 4)) bar)

   (setq button (gtk-button-new-with-label "dump stack"))
   (gtk-box-pack-start-defaults vbox button)
   (gtk-widget-set-sensitive button nil)

   (setq button (gtk-button-new-with-label "test contexts"))
   (gtk-box-pack-start-defaults vbox button)
   (gtk-signal-connect button 'clicked
		       (lambda (button bar)
			 (let ((contexts '("any context" "idle messages" "some text"
					   "hit the mouse" "hit the mouse2")))
			   (message-box "%s"
					(mapconcat
					 (lambda (ctx)
					   (format "context=\"%s\", context_id=%d"
						   ctx (gtk-statusbar-get-context-id bar ctx)))
					 contexts "\n")))) bar)))


;;;; Columned List
(gtk-define-test
 "Columnar List" composite clist nil
 (let ((titles '("auto resize" "not resizeable" "max width 100" "min width 50"
		 "hide column" "Title 5" "Title 6" "Title 7" "Title 8" "Title 9"
		 "Title 10" "Title 11"))
       hbox clist button separator scrolled-win check undo-button label)

   (gtk-container-set-border-width window 0)

   (setq scrolled-win (gtk-scrolled-window-new nil nil))
   (gtk-container-set-border-width scrolled-win 5)
   (gtk-scrolled-window-set-policy scrolled-win 'automatic 'automatic)

   ;; create GtkCList here so we have a pointer to throw at the 
   ;; button callbacks -- more is done with it later
   (setq clist (gtk-clist-new-with-titles (length titles) titles))
   (gtk-container-add scrolled-win clist)

   ;; Make the columns live up to their titles.
   (gtk-clist-set-column-auto-resize clist 0 t)
   (gtk-clist-set-column-resizeable clist 1 nil)
   (gtk-clist-set-column-max-width clist 2 100)
   (gtk-clist-set-column-min-width clist 3 50)

   (gtk-signal-connect clist 'click-column
		       (lambda (clist column data)
			 (cond
			  ((= column 4)
			   (gtk-clist-set-column-visibility clist column nil))
			  ((= column (gtk-clist-sort-column clist))
			   (gtk-clist-set-sort-type
			    clist (if (eq (gtk-clist-sort-type clist) 'ascending)
				      'descending
				    'ascending)))
			  (t
			   (gtk-clist-set-sort-column clist column)))
			 (gtk-clist-sort clist)))

   ;; control buttons
   (setq hbox (gtk-hbox-new nil 5))
   (gtk-container-set-border-width hbox 5)
   (gtk-box-pack-start window hbox nil nil 0)

   (setq button (gtk-button-new-with-label "Insert Row"))
   (gtk-box-pack-start hbox button t t 0)
   (gtk-signal-connect button 'clicked
		       (lambda (button clist)
			 (gtk-clist-append clist
					   (list (format "CListRow %05d" (random 10000))
						 "Column 1"
						 "Column 2"
						 "Column 3"
						 "Column 4"
						 "Column 5"
						 "Column 6"
						 "Column 7"
						 "Column 8"
						 "Column 0"
						 "Column 10"
						 "Column 11"))) clist)

   (setq button (gtk-button-new-with-label "Add 1,000 Rows with Pixmaps"))
   (gtk-box-pack-start hbox button t t 0)
   (gtk-signal-connect button 'clicked
		       (lambda (button clist)
			 (let ((row 0) i)
			   (gtk-clist-freeze clist)
			   (loop for i from 0 to 1000 do
			     (setq row 
				   (gtk-clist-append clist
						     (list
						      (format "CListRow %05d" (random 10000))
						      "Column 1"
						      "Column 2"
						      "Column 3"
						      "Column 4"
						      "Column 5"
						      "Column 6"
						      "Column 7"
						      "Column 8"
						      "Column 0"
						      "Column 10"
						      "Column 11")))
			     (gtk-clist-set-pixtext clist row 3 "gtk+" 5
						    gtk-test-mini-gtk-glyph
						    nil))
			   (gtk-clist-thaw clist))) clist)

   (setq button (gtk-button-new-with-label "Add 10,000 Rows"))
   (gtk-box-pack-start hbox button t t 0)
   (gtk-signal-connect button 'clicked
		       (lambda (button clist)
			 (gtk-clist-freeze clist)
			 (loop for i from 0 to 10000 do
			   (gtk-clist-append clist
					     (list
					      (format "CListRow %05d" (random 10000))
					      "Column 1"
					      "Column 2"
					      "Column 3"
					      "Column 4"
					      "Column 5"
					      "Column 6"
					      "Column 7"
					      "Column 8"
					      "Column 0"
					      "Column 10"
					      "Column 11")))
			 (gtk-clist-thaw clist)) clist)

   ;; Second layer of buttons
   (setq hbox (gtk-hbox-new nil 5))
   (gtk-container-set-border-width hbox 5)
   (gtk-box-pack-start window hbox nil nil 0)

   (setq button (gtk-button-new-with-label "Clear List"))
   (gtk-box-pack-start hbox button t t 0)
   (gtk-signal-connect button 'clicked (lambda (button clist)
					 (gtk-clist-clear clist)) clist)

   (setq button (gtk-button-new-with-label "Remove Selection"))
   (gtk-box-pack-start hbox button t t 0)
   (gtk-signal-connect button 'clicked (lambda (button clist)
					 (error "Do not know how to do this yet.")))
   (gtk-widget-set-sensitive button nil)

   (setq button (gtk-button-new-with-label "Undo Selection"))
   (gtk-box-pack-start hbox button t t 0)
   (gtk-signal-connect button 'clicked
		       (lambda (button clist) (gtk-clist-undo-selection clist)))

   (setq button (gtk-button-new-with-label "Warning Test"))
   (gtk-box-pack-start hbox button t t 0)
   (gtk-signal-connect button 'clicked 'ignore)
   (gtk-widget-set-sensitive button nil)

   ;; Third layer of buttons
   (setq hbox (gtk-hbox-new nil 5))
   (gtk-container-set-border-width hbox 5)
   (gtk-box-pack-start window hbox nil nil 0)

   (setq button (gtk-check-button-new-with-label "Show Title Buttons"))
   (gtk-box-pack-start hbox button nil t 0)
   (gtk-signal-connect button 'clicked (lambda (button clist)
					 (if (gtk-toggle-button-get-active button)
					     (gtk-clist-column-titles-show clist)
					   (gtk-clist-column-titles-hide clist))) clist)
   (gtk-toggle-button-set-active button t)

   (setq button (gtk-check-button-new-with-label "Reorderable"))
   (gtk-box-pack-start hbox check nil t 0)
   (gtk-signal-connect button 'clicked (lambda (button clist)
					 (gtk-clist-set-reorderable
					  clist
					  (gtk-toggle-button-get-active button))) clist)
   (gtk-toggle-button-set-active button t)

   (setq label (gtk-label-new "Selection Mode :"))
   (gtk-box-pack-start hbox label nil t 0)

   (gtk-box-pack-start hbox (build-option-menu
			     '(("Single"   .
				(lambda (item clist)
				  (gtk-clist-set-selection-mode clist 'single)))
			       ("Browse"   . 
				(lambda (item clist)
				  (gtk-clist-set-selection-mode clist 'browse)))
			       ("Multiple" . 
				(lambda (item clist)
				  (gtk-clist-set-selection-mode clist 'multiple)))
			       ("Extended" . 
				(lambda (item clist)
				  (gtk-clist-set-selection-mode clist 'extended))))
			     3 clist) nil t 0)

   ;; The rest of the clist configuration
   (gtk-box-pack-start window scrolled-win t t 0)
   (gtk-clist-set-row-height clist 18)
   (gtk-widget-set-usize clist -1 300)

   (loop for i from 0 to 11 do
     (gtk-clist-set-column-width clist i 80))))


;;;; Notebook
(defun set-tab-label (notebook page selected-p)
  (if page
      (let (label label-box pixwid)
	(setq label-box (gtk-hbox-new nil 0))
	(setq pixwid (gtk-pixmap-new
		      (if selected-p gtk-test-open-glyph gtk-test-closed-glyph) nil))
	(gtk-box-pack-start label-box pixwid nil t 0)
	(gtk-misc-set-padding pixwid 3 1) ;
	(setq label (gtk-label-new
		     (format "Page %d" (1+ (gtk-notebook-page-num notebook page)))))
	(gtk-box-pack-start label-box label nil t 0)
	(gtk-widget-show-all label-box)
	(gtk-notebook-set-tab-label notebook page label-box))))

(defun page-switch (widget page page-num data)
  (let ((oldpage (gtk-notebook-get-current-page widget))
	(label nil)
	(label-box nil)
	(pixwid nil))
    (if (eq page-num oldpage)
	nil
      (set-tab-label widget (gtk-notebook-get-nth-page widget oldpage) nil)
      (set-tab-label widget (gtk-notebook-get-nth-page widget page-num) t))))

(defun create-pages (notebook start end)
  (let (child button label hbox vbox label-box menu-box pixwid i)
    (setq i start)
    (while (<= i end)
      (setq child (gtk-frame-new (format "Page %d" i)))
      (gtk-container-set-border-width child 10)

      (setq vbox (gtk-vbox-new t 0))
      (gtk-container-set-border-width vbox 10)
      (gtk-container-add child vbox)

      (setq hbox (gtk-hbox-new t 0))
      (gtk-box-pack-start vbox hbox nil t 5)

      (setq button (gtk-check-button-new-with-label "Fill Tab"))
      (gtk-box-pack-start hbox button t t 5)
      (gtk-toggle-button-set-active button t)
      (gtk-signal-connect
       button 'toggled
       (lambda (button data)
	 (let ((packing (gtk-notebook-query-tab-label-packing (car data) (cdr data))))
	   (gtk-notebook-set-tab-label-packing (car data) (cdr data)
					       (nth 0 packing)
					       (gtk-toggle-button-get-active button)
					       (nth 2 packing))))
       (cons notebook child))

      (setq button (gtk-check-button-new-with-label "Expand Tab"))
      (gtk-box-pack-start hbox button t t 5)
      (gtk-signal-connect
       button 'toggled
       (lambda (button data)
	 (let ((packing (gtk-notebook-query-tab-label-packing (car data) (cdr data))))
	   (gtk-notebook-set-tab-label-packing (car data) (cdr data)
					       (gtk-toggle-button-get-active button)
					       (nth 1 packing) (nth 2 packing))))
       (cons notebook child))

      (setq button (gtk-check-button-new-with-label "Pack End"))
      (gtk-box-pack-start hbox button t t 5)
      (gtk-signal-connect
       button 'toggled
       (lambda (button data)
	 (let ((packing (gtk-notebook-query-tab-label-packing (car data) (cdr data))))
	   (gtk-notebook-set-tab-label-packing (car data) (cdr data)
					       (nth 0 packing) (nth 1 packing)
					       (if (gtk-toggle-button-get-active button) 'end 'start))))
       (cons notebook child))

      (setq button (gtk-button-new-with-label "Hide Page"))
      (gtk-box-pack-end vbox button nil nil 5)
      (gtk-signal-connect button 'clicked
			  (lambda (ignored child) (gtk-widget-hide child)) child)

      (gtk-widget-show-all child)

      (setq label-box (gtk-hbox-new nil 0))
      (setq pixwid (gtk-pixmap-new gtk-test-closed-glyph nil))
      (gtk-box-pack-start label-box pixwid nil t 0)
      (gtk-misc-set-padding pixwid 3 1);
      (setq label (gtk-label-new (format "Page %d" i)))
      (gtk-box-pack-start label-box label nil t 0)
      (gtk-widget-show-all label-box)

      (setq menu-box (gtk-hbox-new nil 0))
      (setq pixwid (gtk-pixmap-new gtk-test-closed-glyph nil))
      (gtk-box-pack-start menu-box pixwid nil t 0)
      (gtk-misc-set-padding pixwid 3 1)
      (setq label (gtk-label-new (format "Page %d" i)))
      (gtk-box-pack-start menu-box label nil t 0)
      (gtk-widget-show-all menu-box)
      (gtk-notebook-append-page-menu notebook child label-box menu-box)
      (incf i))))

(gtk-define-test
 "Notebook" container notebook nil
 (let (box1 box2 button separator omenu transparent label sample-notebook)
   (gtk-container-set-border-width window 0)

   (setq sample-notebook (gtk-notebook-new))
   (gtk-signal-connect sample-notebook 'switch_page 'page-switch)
   (gtk-notebook-set-tab-pos sample-notebook 'top)
   (gtk-box-pack-start window sample-notebook t t 0)
   (gtk-container-set-border-width sample-notebook 10)

   (create-pages sample-notebook 1 5)

   (setq separator (gtk-hseparator-new))
   (gtk-box-pack-start window separator nil t 10)

   (setq box2 (gtk-hbox-new nil 5))
   (gtk-container-set-border-width box2 10)
   (gtk-box-pack-start window box2 nil t 0)

   (setq button (gtk-check-button-new-with-label "popup menu"))
   (gtk-box-pack-start box2 button t nil 0)
   (gtk-signal-connect button 'clicked
		       (lambda (button notebook)
			 (if (gtk-toggle-button-get-active button)
			     (gtk-notebook-popup-enable notebook)
			   (gtk-notebook-popup-disable notebook))) sample-notebook)

   (setq button (gtk-check-button-new-with-label "homogeneous tabs"))
   (gtk-box-pack-start box2 button t nil 0)
   (gtk-signal-connect button 'clicked
		       (lambda (button notebook)
			 (gtk-notebook-set-homogeneous-tabs
			  notebook
			  (gtk-toggle-button-get-active button))) sample-notebook)

   (setq box2 (gtk-hbox-new nil 5))
   (gtk-container-set-border-width box2 10)
   (gtk-box-pack-start window box2 nil t 0)

   (setq label (gtk-label-new "Notebook Style :"))
   (gtk-box-pack-start box2 label nil t 0)

   (setq omenu (build-option-menu '(("Standard" .
				     (lambda (b n)
				       (gtk-notebook-set-show-tabs n t)
				       (gtk-notebook-set-scrollable n nil)))
				    ("No tabs"  .
				     (lambda (b n)
				       (gtk-notebook-set-show-tabs n nil)))
				    ("Scrollable" .
				     (lambda (b n)
				       (gtk-notebook-set-show-tabs n t)
				       (gtk-notebook-set-scrollable n t))))
				  0
				  sample-notebook))
   (gtk-box-pack-start box2 omenu nil t 0)

   (setq button (gtk-button-new-with-label "Show all pages"))
   (gtk-box-pack-start box2 button nil t 0)
   (gtk-signal-connect
    button 'clicked (lambda (button notebook)
		      (mapc 'gtk-widget-show (gtk-container-children notebook)))
    sample-notebook)

   (setq box2 (gtk-hbox-new t 10))
   (gtk-container-set-border-width box2 10)
   (gtk-box-pack-start window box2 nil t 0)

   (setq button (gtk-button-new-with-label "prev"))
   (gtk-signal-connect button 'clicked
		       (lambda (button notebook)
			 (gtk-notebook-prev-page notebook)) sample-notebook)
   (gtk-box-pack-start box2 button t t 0)

   (setq button (gtk-button-new-with-label "next"))
   (gtk-signal-connect button 'clicked
		       (lambda (button notebook)
			 (gtk-notebook-next-page notebook)) sample-notebook)
   (gtk-box-pack-start box2 button t t 0)

   (setq button (gtk-button-new-with-label "rotate"))
   (gtk-signal-connect button 'clicked
		       (lambda (button notebook)
			 (gtk-notebook-set-tab-pos
			  notebook
			  (case (gtk-notebook-tab-pos notebook)
			   (top 'right)
			   (right 'bottom)
			   (bottom 'left)
			   (left 'top))))
		       sample-notebook)

   (gtk-box-pack-start box2 button t t 0)))


;;;; Glade interfaces
(if (and (featurep 'glade)
	 (file-exists-p (expand-file-name "gtk-test.glade" (gtk-test-directory))))
  (gtk-define-test
   "Glade Interface" misc libglade t
   (glade-init)
   (glade-xml-get-type)
   (let ((xml (glade-xml-new (expand-file-name "gtk-test.glade" (gtk-test-directory))
			     nil)))
     (setq window (glade-xml-get-widget xml "main_window"))
     (glade-xml-signal-autoconnect xml)))
  (fmakunbound 'gtk-test-libglade))


;;;; CTree
(defvar gtk-test-ctree-hash nil)

(defun gtk-test-ctree-expand-directory (ctree dir parent)
  (ignore-errors
    (let ((dirs (directory-files dir t nil nil 5))
	  (files (directory-files dir t nil nil t))
	  (node nil))
      (mapc (lambda (d)
	      (if (or (string-match "/\\.$" d)
		      (string-match "/\\.\\.$" d))
		  nil
		(setq node
		      (gtk-ctree-insert-node ctree parent nil
					     (list (file-name-nondirectory d) "")
					     0 nil nil nil nil nil t))
		(puthash node d gtk-test-ctree-hash)
		(gtk-ctree-insert-node ctree node nil
				       (list "" "")
				       0 nil nil nil nil nil nil)
		(gtk-ctree-collapse ctree node)))
	    dirs)
      (mapc (lambda (f)
	      (gtk-ctree-insert-node ctree parent nil
				     (list (file-name-nondirectory f)
					   (user-login-name (nth 2 (file-attributes f))))
				     0 nil nil nil nil t nil))
	    files)
      (gtk-clist-columns-autosize ctree))))

(defun gtk-spin-button-new-with-label (label adjustment climb-rate digits)
  (let ((box (gtk-hbox-new nil 2))
	(spin (gtk-spin-button-new adjustment climb-rate digits))
	(lbl (gtk-label-new label)))
    (gtk-box-pack-start box lbl nil nil 0)
    (gtk-box-pack-start box spin t t 0)
    (cons box spin)))

(gtk-define-test
 "Columnar Tree" composite ctree nil
 (let ((scrolled (gtk-scrolled-window-new nil nil))
       (ctree (gtk-ctree-new-with-titles 2 0 '("File" "Owner")))
       (box (gtk-hbutton-box-new))
       (button nil))
   (setq gtk-test-ctree-hash (make-hash-table :test 'equal))
   (put scrolled 'child ctree)
   (put scrolled 'height 400)
   (put ctree 'line_style 'solid)
   (put ctree 'expander_style 'square)

   (gtk-box-pack-start window scrolled t t 0)
   (gtk-box-pack-start window box nil nil 5)

   (gtk-clist-freeze ctree)
   (gtk-test-ctree-expand-directory ctree "/" nil)
   (gtk-clist-thaw ctree)

   (setq button (gtk-button-new-with-label "Expand all"))
   (put box 'child button)
   (gtk-signal-connect button 'clicked (lambda (button tree)
					 (gtk-ctree-expand-recursive tree nil)) ctree)

   (setq button (gtk-button-new-with-label "Collaps all"))
   (put box 'child button)
   (gtk-signal-connect button 'clicked (lambda (button tree)
					 (gtk-ctree-collapse-recursive tree nil)) ctree)

   (setq button (gtk-button-new-with-label "Change style"))
   (put box 'child button)
   (put button 'sensitive nil)

   (setq box (gtk-hbox-new t 5))
   (gtk-box-pack-start window box nil nil 0)

   (setq button (gtk-button-new-with-label "Select all"))
   (put box 'child button)
   (gtk-signal-connect button 'clicked (lambda (button tree)
					 (gtk-ctree-select-recursive tree nil)) ctree)

   (setq button (gtk-button-new-with-label "Unselect all"))
   (put box 'child button)
   (gtk-signal-connect button 'clicked (lambda (button tree)
					 (gtk-ctree-unselect-recursive tree nil)) ctree)

   (setq button (gtk-button-new-with-label "Remove all"))
   (put box 'child button)
   (gtk-signal-connect button 'clicked (lambda (button tree)
					 (gtk-clist-freeze tree)
					 (gtk-ctree-recurse
					  tree nil
					  (lambda (tree subnode data)
					    (gtk-ctree-remove-node tree subnode)))
					 (gtk-clist-thaw tree)) ctree)

   (setq button (gtk-check-button-new-with-label "Reorderable"))
   (put box 'child button)
   (gtk-signal-connect button 'clicked (lambda (button tree)
					 (put tree 'reorderable
					      (gtk-toggle-button-get-active button))) ctree)

   (setq box (gtk-hbox-new t 5))
   (gtk-box-pack-start window box nil nil 0)

   (gtk-box-pack-start box (build-option-menu
			    '(("Dotted" . (lambda (item ctree) (put ctree 'line_style 'dotted)))
			      ("Solid"  . (lambda (item ctree) (put ctree 'line_style 'solid)))
			      ("Tabbed" . (lambda (item ctree) (put ctree 'line_style 'tabbed)))
			      ("None"   . (lambda (item ctree) (put ctree 'line_style 'none))))
			    0 ctree) nil t 0)
   (gtk-box-pack-start box (build-option-menu
			    '(("Square"   . (lambda (item ctree) (put ctree 'expander_style 'square)))
			      ("Triangle" . (lambda (item ctree) (put ctree 'expander_style 'triangle)))
			      ("Circular" . (lambda (item ctree) (put ctree 'expander_style 'circular)))
			      ("None"     . (lambda (item ctree) (put ctree 'expander_style 'none))))
			    0 ctree) nil t 0)
   (gtk-box-pack-start box (build-option-menu
			    '(("Left" . (lambda (item ctree)
					  (gtk-clist-set-column-justification
					   ctree (get ctree 'tree_column) 'left)))
			      ("Right" . (lambda (item ctree)
					   (gtk-clist-set-column-justification
					    ctree (get ctree 'tree_column) 'right))))
			    0 ctree) nil t 0)
   (gtk-box-pack-start box (build-option-menu
			    '(("Single"   .
			       (lambda (item clist)
				 (gtk-clist-set-selection-mode clist 'single)))
			      ("Browse"   . 
			       (lambda (item clist)
				 (gtk-clist-set-selection-mode clist 'browse)))
			      ("Multiple" . 
			       (lambda (item clist)
				 (gtk-clist-set-selection-mode clist 'multiple)))
			      ("Extended" . 
			       (lambda (item clist)
				 (gtk-clist-set-selection-mode clist 'extended))))
			    3 ctree) nil t 0)

   (setq box (gtk-hbox-new t 5))
   (gtk-box-pack-start window box nil nil 0)

   (let (adj spinner)
     (setq adj (gtk-adjustment-new (get ctree 'indent) 0 999 1 5 5)
	   spinner (gtk-spin-button-new-with-label "Indent: " adj 1 3))
     (put box 'child (car spinner))
     (gtk-signal-connect adj 'value-changed
			 (lambda (adj tree)
			   (put tree 'indent (truncate (gtk-adjustment-value adj)))) ctree)

     (setq adj (gtk-adjustment-new (get ctree 'spacing) 0 999 1 5 5)
	   spinner (gtk-spin-button-new-with-label "Spacing: " adj 1 3))
     (put box 'child (car spinner))
     (gtk-signal-connect adj 'value-changed
			 (lambda (adj tree)
			   (put tree 'spacing (truncate (gtk-adjustment-value adj)))) ctree)

     (setq adj (gtk-adjustment-new (get ctree 'row_height) 0 999 1 5 5)
	   spinner (gtk-spin-button-new-with-label "Row Height: " adj 1 3))
     (put box 'child (car spinner))
     (gtk-signal-connect adj 'value-changed
			 (lambda (adj tree)
			   (put tree 'row_height (truncate (gtk-adjustment-value adj)))) ctree)

     (setq button (gtk-check-button-new-with-label "Show logical root"))
     (put box 'child button)
     (gtk-signal-connect button 'clicked
			 (lambda (button tree)
			   (put tree 'show_stub (gtk-toggle-button-get-active button))) ctree))

   (gtk-signal-connect ctree 'tree-expand
		       (lambda (ctree node user-data)
			 (gtk-clist-freeze ctree)
			 (gtk-ctree-recurse
			  ctree node
			  (lambda (tree subnode user-data)
			    (if (not (equal subnode node))
				(gtk-ctree-remove-node tree subnode))))
			 (gtk-test-ctree-expand-directory ctree
							  (gethash node gtk-test-ctree-hash)
							  node)
			 (gtk-clist-thaw ctree)))))


;;;; The main interface 

(defun gtk-test-view-source (test)
  ;; View the source for this test in a XEmacs window.
  (if test
      (let ((path (expand-file-name "gtk-test.el" (gtk-test-directory))))
	(if (not (file-exists-p path))
	    (error "Could not find source for gtk-test.el"))
	(find-file path)
	(widen)
	(goto-char (point-min))
	(if (not (re-search-forward (concat "(gtk-define-test[ \t\n]*\"" test "\"") nil t))
	    (error "Could not find test: %s" test)
	  (narrow-to-page)
	  (goto-char (point-min))))))

(defvar gtk-test-selected-test nil)

(defun gtk-test ()
  (interactive)
  (let ((items nil)
	(box nil)
	(window nil)
	(category-trees nil)
	(tree nil)
	(pane nil)
	(scrolled nil)
	(src-button nil)
	(gc-button nil)
	(standalone-p (not (default-gtk-device)))
	(close-button nil))
    (gtk-init (list invocation-name))
    (if standalone-p
	(progn
	  (gtk-object-destroy (gtk-adjustment-new 0 0 0 0 0 0))))
    (ignore-errors
      (or (fboundp 'gtk-test-gnome-pixmaps)
	  (load-file (expand-file-name "gnome-test.el" (gtk-test-directory))))
      (or (fboundp 'gtk-test-color-combo)
	  (load-file (expand-file-name "gtk-extra-test.el" (gtk-test-directory)))))
    (unwind-protect
	(progn
	  (setq window (gtk-dialog-new)
		box (gtk-vbox-new nil 5)
		pane (gtk-hpaned-new)
		scrolled (gtk-scrolled-window-new nil nil)
		tree (gtk-tree-new)
		src-button (gtk-button-new-with-label "View source")
		gc-button (gtk-button-new-with-label "Garbage Collect")
		close-button (gtk-button-new-with-label "Quit"))
	  (gtk-window-set-title window
				(format "%s/GTK %d.%d.%d"
					(if (featurep 'infodock) "InfoDock" "XEmacs")
					emacs-major-version emacs-minor-version
					(or emacs-patch-level emacs-beta-version)))

	  (gtk-scrolled-window-set-policy scrolled 'automatic 'automatic)
	  (gtk-scrolled-window-add-with-viewport scrolled tree)
	  (gtk-widget-set-usize scrolled 200 600)

	  (gtk-box-pack-start (gtk-dialog-vbox window) pane t t 5)
	  (gtk-paned-pack1 pane scrolled t nil)
	  (gtk-paned-pack2 pane box t nil)
	  (setq gtk-test-shell box)
	  (gtk-widget-show-all box)

	  (gtk-container-add (gtk-dialog-action-area window) close-button)
	  (gtk-container-add (gtk-dialog-action-area window) src-button)
	  (gtk-container-add (gtk-dialog-action-area window) gc-button)

	  (gtk-signal-connect gc-button 'clicked
			      (lambda (obj data)
				(garbage-collect)))
	  (gtk-signal-connect close-button 'clicked
			      (lambda (obj data)
				(gtk-widget-destroy data)) window)
	  (gtk-signal-connect src-button 'clicked
			      (lambda (obj data)
				(gtk-test-view-source gtk-test-selected-test)))

	  ;; Try to be a nice person and sort the tests
	  (setq gtk-defined-tests
		(sort gtk-defined-tests
		      (lambda (a b)
			(string-lessp (car a) (car b)))))

	  ;; This adds all of the buttons to the window.
	  (mapcar (lambda (test)
		    (let* ((desc (nth 0 test))
			   (type (nth 1 test))
			   (func (nth 2 test))
			   (parent (cdr-safe (assoc type category-trees)))
			   (item (gtk-tree-item-new-with-label desc)))
		      (put item 'test-function func)
		      (put item 'test-description desc)
		      (put item 'test-type type)
		      (gtk-widget-show item)
		      (if (not parent)
			  (let ((subtree (gtk-tree-new)))
			    (setq parent (gtk-tree-item-new-with-label
					  (or (cdr-safe (assoc type gtk-test-categories))
					      (symbol-name type))))
			    (gtk-signal-connect subtree 'select-child
						(lambda (tree widget data)
						  (setq gtk-test-selected-test (get widget 'test-description))
						  (funcall (get widget 'test-function))))
			    (gtk-tree-append tree parent)
			    (gtk-tree-item-set-subtree parent subtree)
			    (setq parent subtree)
			    (push (cons type parent) category-trees)))
		      (gtk-tree-append parent item)))
		  gtk-defined-tests)
	  (gtk-widget-show-all window)
	  (if standalone-p
	      (progn
		(gtk-signal-connect window 'destroy (lambda (w d)
						      (gtk-main-quit)))
		(gtk-main)))))))
