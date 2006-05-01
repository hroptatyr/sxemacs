;;; gtk-extra.el --- Import `GTK+ Extra' widgets into XEmacs

;; Copyright (C) 2000 Free Software Foundation

;; Maintainer: William Perry <wmperry@gnu.org>
;; Keywords: extensions, dumped

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; GTK+ Extra can be retrieved from http://magnet.fsu.edu/~feiguin/gtk

(eval-and-compile
  (require 'gtk-ffi))

;;; gtkbordercombo.h
(gtk-import-function GtkType gtk_border_combo_get_type)
(gtk-import-function GtkWidget gtk_border_combo_new)

;;; gtkcheckitem.h
(gtk-import-function GtkType gtk_check_item_get_type)
(gtk-import-function GtkWidget gtk_check_item_new)
(gtk-import-function GtkWidget gtk_check_item_new_with_label
		     (GtkString . label))

;;; gtkcolorcombo.h
(gtk-import-function GtkType gtk_color_combo_get_type)
(gtk-import-function GtkWidget gtk_color_combo_new)
(gtk-import-function GtkWidget gtk_color_combo_new_with_values
		     (gint . nrows)
		     (gint . ncols)
		     (GtkArrayOfString . color_names))
(gtk-import-function GtkString gtk_color_combo_get_color_at
		     (GtkColorCombo . combo)
		     (gint          . row)
		     (gint          . col))
;;;(gtk-import-function none gtk_color_combo_find_color
;;;		     (GtkColorCombo . combo)
;;;		     (GdkColor      . color)
;;;		     ((gint . out)  . row)
;;;		     ((gint . out)  . col))

;;; gtkcombobox.h
(gtk-import-function GtkType gtk_combobox_get_type)
(gtk-import-function GtkWidget gtk_combobox_new)
(gtk-import-function none gtk_combobox_hide_popdown_window)

;;; gtkdirtree.h
(gtk-import-function GtkType gtk_dir_tree_get_type)
(gtk-import-function GtkWidget gtk_dir_tree_new)
(gtk-import-function gint gtk_dir_tree_open_dir
		     (GtkDirTree . tree)
		     (GtkString  . path))

;;; gtkfilelist.h
(gtk-import-function GtkType gtk_file_list_get_type)
(gtk-import-function GtkWidget gtk_file_list_new
		     (guint . icon_width)
		     (gint  . mode)
		     (GtkString . path))
(gtk-import-function none gtk_file_list_set_filter
		     (GtkFileList . file_list)
		     (GtkString   . filter))
(gtk-import-function none gtk_file_list_open_dir
		     (GtkFileList . file_list)
		     (GtkString   . path))
(gtk-import-function GtkString gtk_file_list_get_path
		     (GtkFileList . file_list))
(gtk-import-function GtkString gtk_file_list_get_filename
		     (GtkFileList . file_list))

;;; gtkfontcombo.h
(gtk-import-function GtkType gtk_font_combo_get_type)
(gtk-import-function GtkWidget gtk_font_combo_new)
(gtk-import-function none gtk_font_combo_select
		     (GtkFontCombo . font_combo)
		     (GtkString    . family)
		     (gboolean     . bold)
		     (gboolean     . italic)
		     (gint         . height))
(gtk-import-function none gtk_font_combo_select_nth
		     (GtkFontCombo . font_combo)
		     (gint         . n)
		     (gboolean     . bold)
		     (gboolean     . italic)
		     (gint         . height))

;;; gtkiconfilesel.h
;;; gtkiconlist.h
;;; gtkitementry.h
;;; gtkplot.h
;;; gtkplotcanvas.h
;;; gtkplotpc.h
;;; gtkplotprint.h
;;; gtkplotps.h
;;; gtkpsfont.h
;;; gtksheet.h

(provide 'gtk-extra)
