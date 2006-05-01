(gtk-import-function nil "gdk_flush")

(defun gtk-describe-enumerations ()
  "Show a list of all GtkEnum or GtkFlags objects available from lisp."
  (interactive)
  (set-buffer (get-buffer-create "*GTK Enumerations*"))
  (erase-buffer)
  (let ((separator (make-string (- (window-width) 3) ?-)))
    (maphash (lambda (key val)
	       (insert
		separator "\n"
		(if (stringp key)
		    key
		  (gtk-type-name key)) "\n")
	       (mapc (lambda (cell)
		       (insert (format "\t%40s == %d\n" (car cell) (cdr cell)))) val))
	     gtk-enumeration-info))
  (goto-char (point-min))
  (display-buffer (current-buffer)))
