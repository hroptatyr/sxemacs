;;; glade.el --- Import libglade functions into XEmacs

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

;; This file is dumped with XEmacs (if glade was detected)

(eval-and-compile
  (require 'gtk-ffi))

(gtk-import-function none glade_init)
(gtk-import-function none glade_gnome_init)
(gtk-import-function none glade_bonobo_init)
(gtk-import-function none glade_load_module (GtkString . module))
(gtk-import-function GtkType glade_xml_get_type)
(gtk-import-function GtkObject glade_xml_new
		     (GtkString . filename)
		     (GtkString . root))
(gtk-import-function GladeXML glade_xml_new_with_domain
		     (GtkString . filename)
		     (GtkString . root)
		     (GtkString . domain))
(gtk-import-function GladeXML glade_xml_new_from_memory
		     (GtkString . buffer)
		     (gint      . size)
		     (GtkString . root)
		     (GtkString . domain))
(gtk-import-function gboolean glade_xml_construct
		     (GladeXML . self)
		     (GtkString . filename)
		     (GtkString . root)
		     (GtkString . domain))
(gtk-import-function GtkWidget glade_xml_get_widget
		     (GladeXML . xml)
		     (GtkString . name))
(gtk-import-function GtkWidget glade_xml_get_widget_by_long_name
		     (GladeXML . xml)
		     (GtkString . longname))

(gtk-import-function GtkString glade_get_widget_name (GtkWidget . widget))
(gtk-import-function GtkString glade_get_widget_long_name (GtkWidget . widget))
(gtk-import-function GladeXML glade_get_widget_tree (GtkWidget . widget))
