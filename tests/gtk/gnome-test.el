(require 'gnome)

(gtk-define-test
 "GNOME Stock Pixmaps" gnome gnome-pixmaps nil
 (let ((hbox nil)
       (vbox nil)
       (widget nil)
       (label nil)
       (i 0))
   (mapc (lambda (b)
	   (if (= (% i 5) 0)
	       (progn
		 (setq hbox (gtk-hbutton-box-new))
		 (gtk-box-set-spacing hbox 5)
		 (gtk-container-add window hbox)))

	   (setq widget (gnome-stock-pixmap-widget-new window (car b))
		 vbox (gtk-vbox-new t 0)
		 label (gtk-label-new (cdr b)))
	   (gtk-container-add hbox vbox)
	   (gtk-container-add vbox widget)
	   (gtk-container-add vbox label)
	   (gtk-widget-show-all vbox)
	   (setq i (1+ i)))
	 gnome-stock-pixmaps))) 

(gtk-define-test
 "GNOME Stock Buttons" gnome gnome-buttons nil
 (let ((hbbox nil)
       (button nil)
       (i 0))
   (mapc (lambda (b)
	   (setq button (gnome-stock-button (car b)))
	   (gtk-signal-connect button 'clicked (lambda (obj data)
						 (message "Stock GNOME Button: %s" data))
			       (cdr b))
	   (if (= (% i 3) 0)
	       (progn
		 (setq hbbox (gtk-hbutton-box-new))
		 (gtk-button-box-set-spacing hbbox 5)
		 (gtk-container-add window hbbox)))
	       
	   (gtk-container-add hbbox button)
	   (gtk-widget-show button)
	   (setq i (1+ i)))
	 gnome-stock-buttons)))
	 
(gtk-define-test
 "GNOME About" gnome gnome-about t
 (setq window (gnome-about-new "XEmacs/GTK Test Application"
			       "1.0a"
			       "Copyright (C) 2000 Free Software Foundation"
			       '("William M. Perry <wmperry@gnu.org>"
				 "Ichabod Crane")
			       "This is a comment string... what wonderful commentary you have my dear!"
			       "")))

(gtk-define-test
 "GNOME File Entry" gnome gnome-file-entry nil
 (let ((button (gnome-file-entry-new nil "Test browse dialog...")))
   (gtk-container-add window button)))
 
(gtk-define-test
 "GNOME Color Picker" gnome gnome-color-picker nil
 (let ((picker (gnome-color-picker-new))
       (hbox (gtk-hbox-new nil 0))
       (label (gtk-label-new "Please choose a color: ")))

   (gtk-box-pack-start hbox label nil nil 2)
   (gtk-box-pack-start hbox picker t t 2)
   (gtk-container-add window hbox)
   (gtk-widget-show-all hbox)))

(gtk-define-test
 "GNOME Desktop Entry Editor" gnome gnome-dentry-edit nil
 (let* ((notebook (gtk-notebook-new)))
   (gnome-dentry-edit-new-notebook notebook)
   (gtk-container-add window notebook)))

(gtk-define-test
 "GNOME Date Edit" gnome gnome-date-entry nil
 (let ((date (gnome-date-edit-new 0 t t))
       button)
   (gtk-box-pack-start window date t t 0)

   (setq button (gtk-check-button-new-with-label "Show time"))
   (gtk-signal-connect button 'clicked
		       (lambda (button date)
			 (let ((flags (gnome-date-edit-get-flags date)))
			   (if (gtk-toggle-button-get-active button)
			       (push 'show-time flags)
			     (setq flags (delq 'show-time flags)))
			   (gnome-date-edit-set-flags date flags))) date)
   (gtk-toggle-button-set-active button t)
   (gtk-box-pack-start window button nil nil 0)

   (setq button (gtk-check-button-new-with-label "24 Hour format"))
   (gtk-signal-connect button 'clicked
		       (lambda (button date)
			 (let ((flags (gnome-date-edit-get-flags date)))
			   (if (gtk-toggle-button-get-active button)
			       (push '24-hr flags)
			     (setq flags (delq '24-hr flags)))
			   (gnome-date-edit-set-flags date flags))) date)
   (gtk-toggle-button-set-active button t)
   (gtk-box-pack-start window button nil nil 0)

   (setq button (gtk-check-button-new-with-label "Week starts on monday"))
   (gtk-signal-connect button 'clicked
		       (lambda (button date)
			 (let ((flags (gnome-date-edit-get-flags date)))
			   (if (gtk-toggle-button-get-active button)
			       (push 'week-starts-on-monday flags)
			     (setq flags (delq 'week-starts-on-monday flags)))
			   (gnome-date-edit-set-flags date flags))) date)
   (gtk-toggle-button-set-active button t)
   (gtk-box-pack-start window button nil nil 0)))
   
(gtk-define-test
 "GNOME Font Picker" gnome gnome-font-picker nil
 (let ((hbox (gtk-hbox-new nil 5))
       (fp (gnome-font-picker-new))
       (label (gtk-label-new "Choose a font: "))
       (button nil))
   (gtk-box-pack-start hbox label t t 0)
   (gtk-box-pack-start hbox fp nil nil 2)
   (gnome-font-picker-set-title fp "Select a font...")
   (gnome-font-picker-set-mode fp 'font-info)
   (gtk-box-pack-start window hbox t t 0)

   (setq button (gtk-check-button-new-with-label "Use font in label"))
   (gtk-signal-connect button 'clicked
		       (lambda (button fp)
			 (gnome-font-picker-fi-set-use-font-in-label
			  fp (gtk-toggle-button-get-active button) 14))
		       fp)
   (gtk-box-pack-start window button nil nil 0)

   (setq button (gtk-check-button-new-with-label "Show size"))
   (gtk-signal-connect button 'clicked
		       (lambda (button fp)
			 (gnome-font-picker-fi-set-show-size
			  fp (gtk-toggle-button-get-active button)))
		       fp)
   (gtk-box-pack-start window button nil nil 0)))

(gtk-define-test
 "GNOME Application" gnome gnome-app t
 (setq window (gnome-app-new "XEmacs" "XEmacs/GNOME"))
 (let ((menubar (gtk-menu-bar-new))
       (contents nil)
       ;(toolbar-instance (specifier-instance top-toolbar))
       (toolbar nil)
       (item nil)
       (flushright nil))
   (mapc (lambda (node)
	   (if (not node)
	       (setq flushright t)
	     (setq item (gtk-build-xemacs-menu node))
	     (gtk-widget-show item)
	     (if flushright (gtk-menu-item-right-justify item))
	     (gtk-menu-append menubar item)))
	 current-menubar)

   (setq toolbar (gtk-toolbar-new 'horizontal 'both))
   (mapc (lambda (x)
	   (let ((button (gtk-button-new))
		 (pixmap (gnome-stock-pixmap-widget-new toolbar x)))
	     (gtk-container-add button pixmap)
	     (gtk-toolbar-append-widget toolbar button (symbol-name x) nil)))
	 '(open save print cut copy paste undo spellcheck srchrpl mail help))

   (setq contents (gtk-hbox-new nil 5))
   (let ((hbox contents)
	 (vbox (gtk-vbox-new nil 5))
	 (frame nil)
	 (label nil))
     (gtk-box-pack-start hbox vbox nil nil 0)

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
     (gtk-box-pack-start vbox frame nil nil 0))
 
   (gtk-widget-show-all toolbar)
   (gtk-widget-show-all menubar)
   (gtk-widget-show-all contents)
   (gnome-app-set-menus window menubar)
   (gnome-app-set-toolbar window toolbar)
   (gnome-app-set-contents window contents)))
