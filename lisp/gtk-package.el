;; A GTK version of package-ui.el

(require 'package-get)
(require 'package-ui)

(defun package-gtk-edit-sites ()
  (let ((window (gtk-window-new 'toplevel))
	(box (gtk-hbox-new nil 5)))
    (gtk-container-add window box)
    (gtk-widget-show-all window)))
