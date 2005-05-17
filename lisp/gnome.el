(defvar gnome-init-called nil)

(defun gnome-init (app-id app-version argv)
  (mapc 'dll-load
	'("libgnomesupport.so"
	  "libgnome.so"
	  "libgnomeui.so"
	  "libesd.so"
	  "libaudiofile.so"
	  "libart_lgpl.so"))
  (if (and (not (noninteractive)) (not gnome-init-called)
	   (= (gtk-type-from-name "GnomeApp") 0))      
      (prog1
	  (gtk-call-function (gtk-import-function-internal
			      'gint "gnome_init" '(GtkString GtkString gint GtkArrayOfString))
			     (list app-id app-version (length argv) argv))
	(setq gnome-init-called t))))

(require 'gnome-widgets)
(provide 'gnome)
