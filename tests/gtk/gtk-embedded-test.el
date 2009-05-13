(gtk-define-test
 "Embedded XEmacs frame" xemacs-frame t
 (setq window (gtk-window-new 'toplevel))
 (let ((table (gtk-table-new 5 3 nil))
       (label nil)
       (entry nil)
       (frame (gtk-frame-new "Type mail message here...")))
   (gtk-container-add window table)

   (setq label (gtk-label-new "To: ")
	 entry (gtk-entry-new))
   (gtk-table-attach table label 0 1 0 1 nil nil 0 0)
   (gtk-table-attach table entry 1 2 0 1 '(fill) '(fill) 0 0)

   (setq label (gtk-label-new "CC: ")
	 entry (gtk-entry-new))
   (gtk-table-attach table label 0 1 1 2 nil nil 0 0)
   (gtk-table-attach table entry 1 2 1 2 '(fill) '(fill) 0 0)

   (setq label (gtk-label-new "Subject: ")
	 entry (gtk-entry-new))
   (gtk-table-attach table label 0 1 2 3 nil nil 0 0)
   (gtk-table-attach table entry 1 2 2 3 '(fill) '(fill) 0 0)

   (gtk-table-attach table frame 0 2 3 4 '(expand fill) '(expand fill) 5 5)
   
   (gtk-widget-show-all window)
   (gdk-flush)
   (make-frame (list 'window-id frame
		     'unsplittable t
		     'menubar-visible-p nil
		     'default-toolbar-visible-p nil))))
