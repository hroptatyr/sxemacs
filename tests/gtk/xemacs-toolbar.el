(defvar gtk-torture-test-toolbar-open-active-p t)

(defvar gtk-torture-test-toolbar
  '([toolbar-file-icon
     (lambda ()
       (setq gtk-torture-test-toolbar-open-active-p (not gtk-torture-test-toolbar-open-active-p)))
     gtk-torture-test-toolbar-open-active-p
     "Dynamic enabled-p slot... broken in XEmacs 21.1.x"]
    [:size 35 :style 3d]
    [toolbar-folder-icon toolbar-dired t "Edit a directory"]
    [:size 35 :style 2d]
    [toolbar-news-icon toolbar-news t "Read news"]
    nil
    [toolbar-info-icon toolbar-info t "Info documentation"]
    ))

(defun gtk-torture-test-toolbar ()
  (interactive)
  (switch-to-buffer (get-buffer-create "Toolbar testing"))
  (set-specifier default-toolbar gtk-torture-test-toolbar (current-buffer))
  (set-specifier default-toolbar-visible-p t (current-buffer)))
