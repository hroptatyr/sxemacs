(require 'gtk-extra)

(gtk-define-test
 "Color Combo" extra color-combo nil
 (let ((combo (gtk-color-combo-new)))
   (gtk-box-pack-start window combo nil nil 0)))

(gtk-define-test
 "Directory Tree" extra dirtree nil
 (let ((dir (gtk-dir-tree-new)))
   (gtk-box-pack-start window dir nil nil 0)
   (gtk-dir-tree-open-dir dir "/")))

(gtk-define-test
 "File List" extra filelist nil
 (let ((scrolled (gtk-scrolled-window-new nil nil))
       (list (gtk-file-list-new 32 2 "/")))
   (gtk-scrolled-window-add-with-viewport scrolled list)
   (put scrolled 'height 200)
   (gtk-box-pack-start window scrolled t t 0)))

(gtk-define-test
 "Font Combo" extra fontcombo nil
 (let ((fc (gtk-font-combo-new)))
   (gtk-box-pack-start window fc t t 0)))
   
