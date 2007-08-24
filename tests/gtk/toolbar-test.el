(require 'gtk-widgets)
(require 'gnome-widgets)

(defvar gnomeified-toolbar
  ;; [CAPTION TOOLTIP ICON CALLBACK ENABLED]
  '(["Open" "Open a file" new toolbar-open t]
    ["Dired" "Edit a directory" open toolbar-dired t]
    ["Save" "Save buffer" save toolbar-save t]
    ["Print" "Print Buffer" print toolbar-print t]
    ["Cut" "Kill region" cut toolbar-cut t]
    ["Copy" "Copy region" copy toolbar-copy t]
    ["Paste" "Paste from clipboard" paste toolbar-paste t]
    ["Undo" "Undo edit" undo toolbar-undo t]
    ["Spell" "Check spelling" spellcheck toolbar-ispell t]
    ["Replace" "Search & Replace" srchrpl toolbar-replace t]
    ["Mail" "Read mail" mail toolbar-mail t]
    ; info
    ; compile
    ; debug
    ; news
    ))

(setq x (gtk-toolbar-new 'horizontal 'both))
(gnome-app-set-toolbar (frame-property nil 'shell-widget) x)

(mapc (lambda (descr)
	(gtk-toolbar-append-item x
				 (aref descr 0)
				 (aref descr 1)
				 ""
				 (gnome-stock-pixmap-widget-new x (aref descr 2))
				 `(lambda (&rest ignored)
				    (,(aref descr 3)))))
      gnomeified-toolbar)
