;; A GTK version of package-ui.el

(globally-declare-fboundp
 '(gtk-window-new
   gtk-hbox-new gtk-container-add gtk-widget-show-all))

(require 'package-get)
(require 'package-ui)

(defun package-gtk-edit-sites ()
  (let ((window (gtk-window-new 'toplevel))
	(box (gtk-hbox-new nil 5)))
    (gtk-container-add window box)
    (gtk-widget-show-all window)))
