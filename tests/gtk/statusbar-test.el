(defvar statusbar-hashtable (make-hashtable 29))
(defvar statusbar-gnome-p nil)

(defmacro get-frame-statusbar (frame)
  `(gethash (or ,frame (selected-frame)) statusbar-hashtable))

(defun add-frame-statusbar (frame)
  "Stick a GTK (or GNOME) statusbar at the bottom of the frame."
  (if (windowp (frame-property frame 'minibuffer))
      (puthash frame (get-frame-statusbar (window-frame (frame-property frame 'minibuffer)))
	       statusbar-hashtable)
    (let ((sbar nil)
	  (shell (frame-property frame 'shell-widget)))
      (if (string-match "Gnome" (gtk-type-name (gtk-object-type shell)))
	  (progn
	    (require 'gnome-widgets)
	    (setq sbar (gnome-appbar-new t t 0)
		  statusbar-gnome-p t)
	    (gtk-progress-set-format-string sbar "%p%%")
	    (gnome-app-set-statusbar shell sbar))
	(setq sbar (gtk-statusbar-new))
	(gtk-box-pack-end (frame-property frame 'container-widget)
			  sbar nil nil 0))
      (puthash frame sbar statusbar-hashtable))))

(add-hook 'create-frame-hook 'add-frame-statusbar)
(add-hook 'delete-frame-hook (lambda (f)
			       (remhash f statusbar-hashtable)))
			       

(defun clear-message (&optional label frame stdout-p no-restore)
  (let ((sbar (get-frame-statusbar frame)))
    (if sbar
	(if statusbar-gnome-p
	    (gnome-appbar-pop sbar)
	  (gtk-statusbar-pop sbar 1)))))

(defun append-message (label message &optional frame stdout-p)
  (let ((sbar (get-frame-statusbar frame)))
    (if sbar
	(if statusbar-gnome-p
	    (gnome-appbar-push sbar message)
	  (gtk-statusbar-push sbar 1 message)))))

(defun progress-display (fmt &optional value &rest args)
  "Print a progress gauge and message in the bottom gutter area of the frame.
The arguments are the same as to `format'.

If the only argument is nil, clear any existing progress gauge."
  (let ((sbar (get-frame-statusbar nil)))
    (apply 'message fmt args)
    (if statusbar-gnome-p
	(progn
	  (gtk-progress-set-show-text (gnome-appbar-get-progress sbar) t)
	  (gnome-appbar-set-progress sbar (/ value 100.0))
	  (gdk-flush)))))

(defun lprogress-display (label fmt &optional value &rest args)
  "Print a progress gauge and message in the bottom gutter area of the frame.
First argument LABEL is an identifier for this progress gauge.  The rest of the
arguments are the same as to `format'."
    (if (and (null fmt) (null args))
	(prog1 nil
	  (clear-progress-display label nil))
      (let ((str (apply 'format fmt args)))
	(progress-display str value)
	str)))

(defun clear-progress-display (&rest ignored)
  (if statusbar-gnome-p
      (let* ((sbar (get-frame-statusbar nil))
	     (progress (gnome-appbar-get-progress sbar)))
	(gnome-appbar-set-progress sbar 0)
	(gtk-progress-set-show-text progress nil))))
