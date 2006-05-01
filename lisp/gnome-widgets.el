;;; gnome-widgets.el --- Import GNOME functions into XEmacs

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

;; This file is dumped with XEmacs.

(eval-and-compile
  (require 'gtk-ffi))

(gtk-import-function GtkType gnome_about_get_type)
(gtk-import-function GtkWidget gnome_about_new
		     (GtkString . title)
		     (GtkString . version)
		     (GtkString . copyright)
		     (GtkArrayOfString  . authors)
		     (GtkString . comments)
		     (GtkString . logo))


(gtk-import-function GtkType gnome_app_get_type)

;; Create a new (empty) application window.  You must specify the
;; application's name (used internally as an identifier).  The window
;; title can be left as NULL, in which case the window's title will
;; not be set.
(gtk-import-function GtkWidget gnome_app_new
		     (GtkString . appname)
		     (GtkString . title))

;; Constructor for language bindings; you don't normally need this.
(gtk-import-function nil gnome_app_construct
		     (GnomeApp  . app)
		     (GtkString . appname)
		     (GtkString . title))

;; Sets the menu bar of the application window
(gtk-import-function nil gnome_app_set_menus
		     (GnomeApp   . app)
		     (GtkMenuBar . menubar))

;; Sets the main toolbar of the application window
(gtk-import-function nil gnome_app_set_toolbar
		     (GnomeApp   . app)
		     (GtkToolbar . toolbar))

;; Sets the status bar of the application window
(gtk-import-function nil gnome_app_set_statusbar
		     (GnomeApp   . app)
		     (GtkWidget  . statusbar))

;; Sets the status bar of the application window, but uses the given
;; container widget rather than creating a new one.
(gtk-import-function nil gnome_app_set_statusbar_custom
		     (GnomeApp   . app)
		     (GtkWidget  . container)
		     (GtkWidget  . statusbar))

;; Sets the content area of the application window
(gtk-import-function nil gnome_app_set_contents
		     (GnomeApp   . app)
		     (GtkWidget  . contents))

(gtk-import-function nil gnome_app_add_toolbar
		     (GnomeApp              . app)
		     (GtkToolbar            . toolbar)
		     (GtkString             . name)
		     (GnomeDockItemBehavior . behavior)
		     (GnomeDockPlacement    . placement)
		     (gint                  . band_num)
		     (gint                  . band_position)
		     (gint                  . offset))

(gtk-import-function nil gnome_app_add_docked
		     (GnomeApp              . app)
		     (GtkWidget             . toolbar)
		     (GtkString             . name)
		     (GnomeDockItemBehavior . behavior)
		     (GnomeDockPlacement    . placement)
		     (gint                  . band_num)
		     (gint                  . band_position)
		     (gint                  . offset))

(gtk-import-function nil gnome_app_add_dock_item
		     (GnomeApp              . app)
		     (GnomeDockItem         . item)
		     (GnomeDockPlacement    . placement)
		     (gint                  . band_num)
		     (gint                  . band_position)
		     (gint                  . offset))

(gtk-import-function nil gnome_app_enable_layout_config
		     (GnomeApp . app)
		     (gboolean . enable))

(gtk-import-function GnomeDock gnome_app_get_dock
		     (GnomeApp . app))
(gtk-import-function GnomeDockItem gnome_app_get_dock_item_by_name
		     (GnomeApp  . app)
		     (GtkString . name))


(gtk-import-function GtkType gnome_appbar_get_type)

(gtk-import-function GtkWidget gnome_appbar_new
		     (gboolean . has_progress)
		     (gboolean . has_status)
		     (GnomePreferencesType . interactivity))

;; Sets the status label without changing widget state; next set or push
;; will destroy this permanently.
(gtk-import-function nil gnome_appbar_set_status
		     (GnomeAppBar . appbar)
		     (GtkString   . status))

;; What to show when showing nothing else; defaults to nothing
(gtk-import-function nil gnome_appbar_set_default
		     (GnomeAppBar . appbar)
		     (GtkString   . default_status))

(gtk-import-function nil gnome_appbar_push
		     (GnomeAppBar . appbar)
		     (GtkString   . status))

;; OK to call on empty stack
(gtk-import-function nil gnome_appbar_pop
		     (GnomeAppBar . appbar))

;; Nuke the stack.
(gtk-import-function nil gnome_appbar_clear_stack
		     (GnomeAppBar . appbar))

;; pure sugar - with a bad name, in light of the get_progress name
;; which is not the opposite of set_progress. Maybe this function
;; should die
(gtk-import-function nil gnome_appbar_set_progress
		     (GnomeAppBar . appbar)
		     (gfloat      . percentage))

;; use GtkProgress functions on returned value
(gtk-import-function GtkProgress gnome_appbar_get_progress
		     (GnomeAppBar . appbar))

;; Reflect the current state of stack/default. Useful to force a set_status
;; to disappear.
(gtk-import-function nil gnome_appbar_refresh
		     (GnomeAppBar . appbar))

;; Put a prompt in the appbar and wait for a response. When the 
;; user responds or cancels, a user_response signal is emitted.
(gtk-import-function nil gnome_appbar_set_prompt
		     (GnomeAppBar . appbar)
		     (GtkString   . prompt)
		     (gboolean    . modal))

;; Remove any prompt
(gtk-import-function nil gnome_appbar_clear_prompt
		     (GnomeAppBar . appbar))

;; Get the response to the prompt, if any. Result must be g_free'd.
(gtk-import-function GtkString gnome_appbar_get_response
		     (GnomeAppBar . appbar))


(gtk-import-function GtkType gnome_calculator_get_type)
(gtk-import-function GtkWidget gnome_calculator_new)
(gtk-import-function nil gnome_calculator_clear
		     (GnomeCalculator . gc)
		     (gboolean        . reset))

(gtk-import-function nil gnome_calculator_set
		     (GnomeCalculator . gc)
		     (gdouble         . result))


;; Standard Gtk function
(gtk-import-function GtkType gnome_color_picker_get_type)

;; Creates a new color picker widget
(gtk-import-function GtkWidget gnome_color_picker_new)

;; Set/get the color in the picker.  Values are in [0.0, 1.0]
(gtk-import-function nil gnome_color_picker_set_d
		     (GnomeColorPicker . cp)
		     (gdouble . r)
		     (gdouble . g)
		     (gdouble . b)
		     (gdouble . a))

;; #### BILL!!!  Need multiple return values
;; void gnome_color_picker_get_d (GnomeColorPicker *cp, gdouble *r, gdouble *g, gdouble *b, gdouble *a)

;; Set/get the color in the picker.  Values are in [0, 255]
(gtk-import-function nil gnome_color_picker_set_i8
		     (GnomeColorPicker . cp)
		     (guint . r)
		     (guint . g)
		     (guint . b)
		     (guint . a))

;; #### BILL!!! Need multiple return values
;; void gnome_color_picker_get_i8 (GnomeColorPicker *cp, guint8 *r, guint8 *g, guint8 *b, guint8 *a);

;; Set/get the color in the picker.  Values are in [0, 65535]
(gtk-import-function nil gnome_color_picker_set_i16
		     (GnomeColorPicker . cp)
		     (guint . r)
		     (guint . g)
		     (guint . b)
		     (guint . a))

;; #### BILL!!! Need multiple return values
;; void gnome_color_picker_get_i16 (GnomeColorPicker *cp, gushort *r, gushort *g, gushort *b, gushort *a);

;; Sets whether the picker should dither the color sample or just paint a solid rectangle
(gtk-import-function nil gnome_color_picker_set_dither
		     (GnomeColorPicker . cp)
		     (gboolean         . dither))

;; Sets whether the picker should use the alpha channel or not
(gtk-import-function nil gnome_color_picker_set_use_alpha
		     (GnomeColorPicker . cp)
		     (gboolean         . use_alpha))

;; Sets the title for the color selection dialog
(gtk-import-function nil gnome_color_picker_set_title
		     (GnomeColorPicker . cp)
		     (GtkString        . title))


(gtk-import-function GtkType gnome_date_edit_get_type)
(gtk-import-function GtkWidget gnome_date_edit_new
		     (time_t   . the_time)
		     (gboolean . show_time)
		     (gboolean . use_24_format))

(gtk-import-function GtkWidget gnome_date_edit_new_flags
		     (time_t . the_time)
		     (GnomeDateEditFlags . flags))

(gtk-import-function nil gnome_date_edit_set_time
		     (GnomeDateEdit . gde)
		     (time_t        . the_time))

(gtk-import-function nil gnome_date_edit_set_popup_range
		     (GnomeDateEdit . gde)
		     (guint         . low_hour)
		     (guint         . up_hour))

(gtk-import-function 'time_t gnome_date_edit_get_date
		     (GnomeDateEdit . gde))

(gtk-import-function nil gnome_date_edit_set_flags
		     (GnomeDateEdit      . gde)
		     (GnomeDateEditFlags . flags))

(gtk-import-function GnomeDateEditFlags gnome_date_edit_get_flags
		     (GnomeDateEdit . gde))


(gtk-import-function GtkType gnome_dentry_edit_get_type)

;; create a new dentry and get the children using the below macros
;; or use the utility new_notebook below
(gtk-import-function GtkObject gnome_dentry_edit_new)

;;#define gnome_dentry_edit_child1(d) (GNOME_DENTRY_EDIT(d)->child1)
;;#define gnome_dentry_edit_child2(d) (GNOME_DENTRY_EDIT(d)->child2)

;; Create a new edit in this notebook - appends two pages to the 
;; notebook.
(gtk-import-function GtkObject gnome_dentry_edit_new_notebook
		     (GtkNotebook . notebook))

(gtk-import-function nil gnome_dentry_edit_clear
		     (GnomeDEntryEdit . dee))

;; The GnomeDEntryEdit does not store a dentry, and it does not keep
;; track of the location field of GnomeDesktopEntry which will always
;; be NULL.

;; Make the display reflect dentry at path
(gtk-import-function nil gnome_dentry_edit_load_file
		     (GnomeDEntryEdit . dee)
		     (GtkString       . path))

;; Copy the contents of this dentry into the display
'(gtk-import-function nil gnome_dentry_edit_set_dentry
		     (GnomeDEntryEdit . dee)
		     (GnomeDesktopEntry . dentry))

;; Generate a dentry based on the contents of the display
'(gtk-import-function GnomeDesktopEntry gnome_dentry_edit_get_dentry
		      (GnomeDEntryEdit . dee))

;; Return an allocated string, you need to g_free it.
(gtk-import-function GtkString gnome_dentry_edit_get_icon
		     (GnomeDEntryEdit . dee))
(gtk-import-function GtkString gnome_dentry_edit_get_name
		     (GnomeDEntryEdit . dee))

;; These are accessor functions for the widgets that make up the
;; GnomeDEntryEdit widget.
(gtk-import-function GtkWidget gnome_dentry_get_name_entry (GnomeDEntryEdit . dee))
(gtk-import-function GtkWidget gnome_dentry_get_comment_entry (GnomeDEntryEdit . dee))
(gtk-import-function GtkWidget gnome_dentry_get_exec_entry (GnomeDEntryEdit . dee))
(gtk-import-function GtkWidget gnome_dentry_get_tryexec_entry (GnomeDEntryEdit . dee))
(gtk-import-function GtkWidget gnome_dentry_get_doc_entry (GnomeDEntryEdit . dee))
(gtk-import-function GtkWidget gnome_dentry_get_icon_entry (GnomeDEntryEdit . dee))


;; The GtkWidget * return values were added in retrospect; sometimes
;; you might want to connect to the "close" signal of the dialog, or
;; something, the return value makes the functions more
;; flexible. However, there is nothing especially guaranteed about
;; these dialogs except that they will be dialogs, so don't count on
;; anything.

;; A little OK box
(gtk-import-function GtkWidget gnome_ok_dialog (GtkString . message))
(gtk-import-function GtkWidget gnome_ok_dialog_parented
		     (GtkString . message)
		     (GtkWindow . parent))

;; Operation failed fatally. In an OK dialog.
(gtk-import-function GtkWidget gnome_error_dialog '(GtkString . error))
(gtk-import-function GtkWidget gnome_error_dialog_parented
		     (GtkString . error)
		     (GtkWindow . parent))

;; Just a warning.
(gtk-import-function GtkWidget gnome_warning_dialog '(GtkString . warning))
(gtk-import-function GtkWidget gnome_warning_dialog_parented
		     (GtkString . warning)
		     (GtkWindow . parent))

;;;/* Look in gnome-types.h for the callback types. */

;;;/* Ask a yes or no question, and call the callback when it's answered. */
;;;GtkWidget * gnome_question_dialog                 (const gchar * question,
;;;						   GnomeReplyCallback callback,
;;;						   gpointer data);

;;;GtkWidget * gnome_question_dialog_parented        (const gchar * question,
;;;						   GnomeReplyCallback callback,
;;;						   gpointer data,
;;;						   GtkWindow * parent);

;;;GtkWidget * gnome_question_dialog_modal           (const gchar * question,
;;;						   GnomeReplyCallback callback,
;;;						   gpointer data);

;;;GtkWidget * gnome_question_dialog_modal_parented  (const gchar * question,
;;;						   GnomeReplyCallback callback,
;;;						   gpointer data,
;;;						   GtkWindow * parent);


;;;/* OK-Cancel question. */
;;;GtkWidget * gnome_ok_cancel_dialog                (const gchar * message,
;;;						   GnomeReplyCallback callback,
;;;						   gpointer data);

;;;GtkWidget * gnome_ok_cancel_dialog_parented       (const gchar * message,
;;;						   GnomeReplyCallback callback,
;;;						   gpointer data,
;;;						   GtkWindow * parent);

;;;GtkWidget * gnome_ok_cancel_dialog_modal          (const gchar * message,
;;;						   GnomeReplyCallback callback,
;;;						   gpointer data);

;;;GtkWidget * gnome_ok_cancel_dialog_modal_parented (const gchar * message,
;;;						   GnomeReplyCallback callback,
;;;						   gpointer data,
;;;						   GtkWindow * parent);


(gtk-import-function GtkType gnome_file_entry_get_type)
(gtk-import-function GtkWidget gnome_file_entry_new
		     (GtkString . history_id)
		     (GtkString . browse_dialog_title))

(gtk-import-function nil gnome_file_entry_construct
		     (GnomeFileEntry . fentry)
		     (GtkString . history_id)
		     (GtkString . browse_dialog_title))

(gtk-import-function GtkWidget gnome_file_entry_gnome_entry
		     (GnomeFileEntry .fentry))

(gtk-import-function GtkWidget gnome_file_entry_gtk_entry
		     (GnomeFileEntry . fentry))

(gtk-import-function nil gnome_file_entry_set_title
		     (GnomeFileEntry . fentry)
		     (GtkString      . browse_dialog_title))

;; set default path for the browse dialog
(gtk-import-function nil gnome_file_entry_set_default_path
		     (GnomeFileEntry . fentry)
		     (GtkString      . path))

;; sets up the file entry to be a directory picker rather then a file picker
(gtk-import-function nil gnome_file_entry_set_directory
		     (GnomeFileEntry . fentry)
		     (gboolean       . directory_entry))

;; returns a filename which is a full path with WD or the default
;; directory prepended if it's not an absolute path, returns
;; NULL on empty entry or if the file doesn't exist and that was
;; a requirement
(gtk-import-function GtkString gnome_file_entry_get_full_path
		     (GnomeFileEntry . fentry)
		     (gboolean       . file_must_exist))

;; set modality of the file browse dialog, only applies for the
;; next time a dialog is created
(gtk-import-function nil gnome_file_entry_set_modal
		     (GnomeFileEntry . fentry)
		     (gboolean       . is_modal))


;; Standard Gtk function
(gtk-import-function GtkType gnome_font_picker_get_type)

;; Creates a new font picker widget
(gtk-import-function GtkWidget gnome_font_picker_new)

;; Sets the title for the font selection dialog
(gtk-import-function nil gnome_font_picker_set_title
		     (GnomeFontPicker . gfp)
		     (GtkString       . title))

;; Button mode
(gtk-import-function GnomeFontPickerMode gnome_font_picker_get_mode
		     (GnomeFontPicker . gfp))

(gtk-import-function nil gnome_font_picker_set_mode
		     (GnomeFontPicker . gfp)
		     (GnomeFontPickerMode . mode))

;; With  GNOME_FONT_PICKER_MODE_FONT_INFO
;; If use_font_in_label is true, font name will be writen using font choosed by user and
;; using size passed to this function
(gtk-import-function nil gnome_font_picker_fi_set_use_font_in_label
		     (GnomeFontPicker . gfp)
		     (gboolean        . use_font_in_label)
		     (gint            . size))

(gtk-import-function nil gnome_font_picker_fi_set_show_size
		     (GnomeFontPicker . gfp)
		     (gboolean        . show_size))

;; With GNOME_FONT_PICKER_MODE_USER_WIDGET
(gtk-import-function nil gnome_font_picker_uw_set_widget
		     (GnomeFontPicker . gfp)
		     (GtkWidget       . widget))

;; Functions to interface with GtkFontSelectionDialog
(gtk-import-function GtkString gnome_font_picker_get_font_name
		     (GnomeFontPicker . gfp))

;;;GdkFont*   gnome_font_picker_get_font	      (GnomeFontPicker *gfp);

(gtk-import-function gboolean gnome_font_picker_set_font_name
		     (GnomeFontPicker . gfp)
		     (GtkString       . fontname))

(gtk-import-function GtkString gnome_font_picker_get_preview_text
		     (GnomeFontPicker . gfp))

(gtk-import-function nil gnome_font_picker_set_preview_text
		     (GnomeFontPicker . gfp)
		     (GtkString       . text))


(gtk-import-function GtkType gnome_href_get_type)
(gtk-import-function GtkWidget gnome_href_new
		     (GtkString . url)
		     (GtkString . label))

(gtk-import-function nil gnome_href_set_url
		     (GnomeHRef . href)
		     (GtkString . url))
(gtk-import-function GtkString gnome_href_get_url
		     (GnomeHRef . href))

(gtk-import-function nil gnome_href_set_label
		     (GnomeHRef . href)
		     (GtkString . label))

(gtk-import-function GtkString gnome_href_get_label
		     (GnomeHRef . href))


;; Stock icons, buttons, and menu items.

;; A short description:

;; These functions provide an applications programmer with default
;; icons for toolbars, menu pixmaps, etc. One such `icon' should have
;; at least three pixmaps to reflect it's state. There is a `regular'
;; pixmap, a `disabled' pixmap and a `focused' pixmap. You can get
;; either each of these pixmaps by calling gnome_stock_pixmap or you
;; can get a widget by calling gnome_stock_pixmap_widget. This widget
;; is a container which gtk_widget_shows the pixmap, that is
;; reflecting the current state of the widget. If for example you
;; gtk_container_add this widget to a button, which is currently not
;; sensitive, the widget will just show the `disabled' pixmap. If the
;; state of the button changes to sensitive, the widget will change to
;; the `regular' pixmap. The `focused' pixmap will be shown, when the
;; mouse pointer enters the widget.

;; To support themability, we use (char *) to call those functions. A
;; new theme might register new icons by calling
;; gnome_stock_pixmap_register, or may change existing icons by
;; calling gnome_stock_pixmap_change. An application should check (by
;; calling gnome_stock_pixmap_checkfor), if the current theme supports
;; an uncommon icon, before using it. The only icons an app can rely
;; on, are those defined in this header file.

;; We now have stock buttons too. To use them, just replace any
;; gtk_button_new{_with_label} with
;; gnome_stock_button(GNOME_STOCK_BUTTON_...).  This function returns
;; a GtkButton with a gettexted default text and an icon.

;; There's an additional feature, which might be interesting. If an
;; application calls gnome_stock_pixmap_register and uses it by
;; calling gnome_stock_pixmap_widget, it doesn't have to care about
;; the state_changed signal to display the appropriate pixmap
;; itself. Additionally gnome-stock generates a disabled version of a
;; pixmap automatically, when no pixmap for a disabled state is
;; provided.


;; State:

;;  currently implemented:
;;    - gnome_stock_pixmap
;;    - gnome_stock_pixmap_widget
;;    - gnome_stock_pixmap_checkfor
;;    - GnomeStockPixmapWidget
;;    - gnome_stock_button
;;    - gnome_stock_pixmap_register

;;  not implemented:
;;    - gnome_stock_pixmap_change

;; The names of `well known' icons. I define these strings mainly to
;; prevent errors due to typos.

(defvar gnome-stock-pixmaps '(
			      (new         . "New")
			      (open        . "Open")
			      (close       . "Close")
			      (revert      . "Revert")
			      (save        . "Save")
			      (save-as     . "Save As")
			      (cut         . "Cut")
			      (copy        . "Copy")
			      (paste       . "Paste")
			      (clear       . "Clear")
			      (properties  . "Properties")
			      (preferences . "Preferences")
			      (help        . "Help")
			      (scores      . "Scores")
			      (print       . "Print")
			      (search      . "Search")
			      (srchrpl     . "Search/Replace")
			      (back        . "Back")
			      (forward     . "Forward")
			      (first       . "First")
			      (last        . "Last")
			      (home        . "Home")
			      (stop        . "Stop")
			      (refresh     . "Refresh")
			      (undo        . "Undo")
			      (redo        . "Redo")
			      (timer       . "Timer")
			      (timer-stop  . "Timer Stopped")
			      (mail	   . "Mail")
			      (mail-rcv    . "Receive Mail")
			      (mail-snd    . "Send Mail")
			      (mail-rpl    . "Reply to Mail")
			      (mail-fwd    . "Forward Mail")
			      (mail-new    . "New Mail")
			      (trash       . "Trash")
			      (trash-full  . "Trash Full")
			      (undelete    . "Undelete")
			      (spellcheck  . "Spellchecker")
			      (mic         . "Microphone")
			      (line-in     . "Line In")
			      (cdrom       . "Cdrom")
			      (volume      . "Volume")
			      (midi        . "Midi")
			      (book-red    . "Book Red")
			      (book-green  . "Book Green")
			      (book-blue   . "Book Blue")
			      (BOOK-YELLOW . "Book Yellow")
			      (BOOK-OPEN   . "Book Open")
			      (ABOUT       . "About")
			      (QUIT        . "Quit")
			      (MULTIPLE    . "Multiple")
			      (NOT         . "Not")
			      (CONVERT     . "Convert")
			      (JUMP-TO     . "Jump To")
			      (UP          . "Up")
			      (DOWN        . "Down")
			      (TOP         . "Top")
			      (BOTTOM      . "Bottom")
			      (ATTACH      . "Attach")
			      (INDEX       . "Index")
			      (FONT        . "Font")
			      (EXEC        . "Exec")

			      (ALIGN-LEFT    . "Left")
			      (ALIGN-RIGHT   . "Right")
			      (ALIGN-CENTER  . "Center")
			      (ALIGN-JUSTIFY . "Justify")

			      (TEXT-BOLD      . "Bold")
			      (TEXT-ITALIC    . "Italic")
			      (TEXT-UNDERLINE . "Underline")
			      (TEXT-STRIKEOUT . "Strikeout")

			      (TEXT-INDENT   . "Text Indent")
			      (TEXT-UNINDENT . "Text Unindent")

			      (EXIT          . "Quit")

			      (COLORSELECTOR . "Color Select")

			      (ADD    . "Add")
			      (REMOVE . "Remove")

			      (TABLE-BORDERS . "Table Borders")
			      (TABLE-FILL    . "Table Fill")

			      (TEXT-BULLETED-LIST . "Text Bulleted List")
			      (TEXT-NUMBERED-LIST . "Text Numbered List")
			      ))

;; The basic pixmap version of an icon.

;;#define GNOME_STOCK_PIXMAP_REGULAR     "regular"
;;#define GNOME_STOCK_PIXMAP_DISABLED    "disabled"
;;#define GNOME_STOCK_PIXMAP_FOCUSED     "focused"

(defvar gnome-stock-pixmap-widget-new nil)

(defun gnome-stock-pixmap-widget-new (window symbol)
  "Load a stock pixmap named SYMBOL using WINDOW as the parent."
  (if (not gnome-stock-pixmap-widget-new)
      (setq gnome-stock-pixmap-widget-new (gtk-import-function-internal
					   'GtkWidget
					   "gnome_stock_pixmap_widget_new"
					   '(GtkWidget GtkString))))
  (let ((translation (assq symbol gnome-stock-pixmaps)))
    (if (not translation)
	(error "Unknown stock pixmap: %S" symbol))
    (gtk-call-function gnome-stock-pixmap-widget-new (list window (cdr translation)))))

(gtk-import-function GtkType gnome_stock_get_type)
(gtk-import-function GtkWidget gnome_stock_new)
(gtk-import-function GtkWidget gnome_stock_new_with_icon '(GtkString . icon))
(gtk-import-function gboolean gnome_stock_set_icon
		     (GnomeStock . stock)
		     (GtkString  . icon))

;; just fetch a GnomeStock(PixmapWidget)
;; It is possible to specify a filename instead of an icon name. Gnome stock
;; will use gnome_pixmap_file to find the pixmap and return a GnomeStock widget
;; from that file.
(gtk-import-function GtkWidget gnome_stock_pixmap_widget
		     (GtkWidget . window)
		     (GtkString . icon))

;; This function loads that file scaled to the specified size. Unlike
;; gnome_pixmap_new_from_file_at_size this function uses antializing and stuff
;; to scale the pixmap
(gtk-import-function GtkWidget gnome_stock_pixmap_widget_at_size
		     (GtkWidget . window)
		     (GtkString . icon)
		     (guint     . width)
		     (guint     . height))

(gtk-import-function nil gnome_stock_pixmap_widget_set_icon
		     (GnomeStock . widget)
		     (GtkString  . icon))

;;;gint                   gnome_stock_pixmap_register (const char *icon,
;;;						    const char *subtype,
;;;                                                    GnomeStockPixmapEntry *entry);

;; change an existing entry. returns non-zero on success
;;;gint                   gnome_stock_pixmap_change   (const char *icon,
;;;						    const char *subtype,
;;;                                                    GnomeStockPixmapEntry *entry);

;; check for the existance of an entry. returns the entry if it
;; exists, or NULL otherwise
;;;GnomeStockPixmapEntry *gnome_stock_pixmap_checkfor (const char *icon,
;;;						    const char *subtype);

;; buttons

(defvar gnome-stock-buttons '((ok     . "Button_Ok")
			      (cancel . "Button_Cancel")
			      (yes    . "Button_Yes")
			      (no     . "Button_No")
			      (close  . "Button_Close")
			      (apply  . "Button_Apply")
			      (help   . "Button_Help")
			      (next   . "Button_Next")
			      (prev   . "Button_Prev")
			      (up     . "Button_Up")
			      (down   . "Button_Down")
			      (font   . "Button_Font")))

;; this function returns a button with a pixmap (if ButtonUseIcons is enabled)
;; and the provided text

(gtk-import-function GtkWidget gnome_pixmap_button
		     (GtkWidget . pixmap)
		     (GtkString . text))
(gtk-import-function nil gnome_button_can_default
		     (GtkButton . button)
		     (gboolean  . can_default))

(defvar gnome-stock-button nil)

(defun gnome-stock-button (symbol)
  "Returns a default button widget for dialogs."
  (if (not gnome-stock-button)
      (setq gnome-stock-button (gtk-import-function-internal
				'GtkWidget "gnome_stock_button"
				'(GtkString))))
  (let ((translation (assq symbol gnome-stock-buttons)))
    (if (not translation)
	(error "Unknown stock button: %S" symbol))
    (gtk-call-function gnome-stock-button (list (cdr translation)))))

(defun gnome-stock-or-ordinary-button (type)
  "Returns a button widget.  If the TYPE argument matches a
GNOME_STOCK_BUTTON_* define, then a stock button is created.
Otherwise, an ordinary button is created, and TYPE is given as the
label."
  (if (stringp type) (setq type (intern type)))
  (condition-case ()
      (gnome-stock-button type)
    (error (gtk-button-new-with-label (symbol-name type)))))

;;/*  menus  */

;;#define GNOME_STOCK_MENU_BLANK        "Menu_"
;;#define GNOME_STOCK_MENU_NEW          "Menu_New"
;;#define GNOME_STOCK_MENU_SAVE         "Menu_Save"
;;#define GNOME_STOCK_MENU_SAVE_AS      "Menu_Save As"
;;#define GNOME_STOCK_MENU_REVERT       "Menu_Revert"
;;#define GNOME_STOCK_MENU_OPEN         "Menu_Open"
;;#define GNOME_STOCK_MENU_CLOSE        "Menu_Close"
;;#define GNOME_STOCK_MENU_QUIT         "Menu_Quit"
;;#define GNOME_STOCK_MENU_CUT          "Menu_Cut"
;;#define GNOME_STOCK_MENU_COPY         "Menu_Copy"
;;#define GNOME_STOCK_MENU_PASTE        "Menu_Paste"
;;#define GNOME_STOCK_MENU_PROP         "Menu_Properties"
;;#define GNOME_STOCK_MENU_PREF         "Menu_Preferences"
;;#define GNOME_STOCK_MENU_ABOUT        "Menu_About"
;;#define GNOME_STOCK_MENU_SCORES       "Menu_Scores"
;;#define GNOME_STOCK_MENU_UNDO         "Menu_Undo"
;;#define GNOME_STOCK_MENU_REDO         "Menu_Redo"
;;#define GNOME_STOCK_MENU_PRINT        "Menu_Print"
;;#define GNOME_STOCK_MENU_SEARCH       "Menu_Search"
;;#define GNOME_STOCK_MENU_SRCHRPL      "Menu_Search/Replace"
;;#define GNOME_STOCK_MENU_BACK         "Menu_Back"
;;#define GNOME_STOCK_MENU_FORWARD      "Menu_Forward"
;;#define GNOME_STOCK_MENU_FIRST        "Menu_First"
;;#define GNOME_STOCK_MENU_LAST         "Menu_Last"
;;#define GNOME_STOCK_MENU_HOME         "Menu_Home"
;;#define GNOME_STOCK_MENU_STOP         "Menu_Stop"
;;#define GNOME_STOCK_MENU_REFRESH      "Menu_Refresh"
;;#define GNOME_STOCK_MENU_MAIL         "Menu_Mail"
;;#define GNOME_STOCK_MENU_MAIL_RCV     "Menu_Receive Mail"
;;#define GNOME_STOCK_MENU_MAIL_SND     "Menu_Send Mail"
;;#define GNOME_STOCK_MENU_MAIL_RPL     "Menu_Reply to Mail"
;;#define GNOME_STOCK_MENU_MAIL_FWD     "Menu_Forward Mail"
;;#define GNOME_STOCK_MENU_MAIL_NEW     "Menu_New Mail"
;;#define GNOME_STOCK_MENU_TRASH        "Menu_Trash"
;;#define GNOME_STOCK_MENU_TRASH_FULL   "Menu_Trash Full"
;;#define GNOME_STOCK_MENU_UNDELETE     "Menu_Undelete"
;;#define GNOME_STOCK_MENU_TIMER        "Menu_Timer"
;;#define GNOME_STOCK_MENU_TIMER_STOP   "Menu_Timer Stopped"
;;#define GNOME_STOCK_MENU_SPELLCHECK   "Menu_Spellchecker"
;;#define GNOME_STOCK_MENU_MIC          "Menu_Microphone"
;;#define GNOME_STOCK_MENU_LINE_IN      "Menu_Line In"
;;#define GNOME_STOCK_MENU_CDROM	     "Menu_Cdrom"
;;#define GNOME_STOCK_MENU_VOLUME       "Menu_Volume"
;;#define GNOME_STOCK_MENU_MIDI         "Menu_Midi"
;;#define GNOME_STOCK_MENU_BOOK_RED     "Menu_Book Red"
;;#define GNOME_STOCK_MENU_BOOK_GREEN   "Menu_Book Green"
;;#define GNOME_STOCK_MENU_BOOK_BLUE    "Menu_Book Blue"
;;#define GNOME_STOCK_MENU_BOOK_YELLOW  "Menu_Book Yellow"
;;#define GNOME_STOCK_MENU_BOOK_OPEN    "Menu_Book Open"
;;#define GNOME_STOCK_MENU_CONVERT      "Menu_Convert"
;;#define GNOME_STOCK_MENU_JUMP_TO      "Menu_Jump To"
;;#define GNOME_STOCK_MENU_UP           "Menu_Up"
;;#define GNOME_STOCK_MENU_DOWN         "Menu_Down"
;;#define GNOME_STOCK_MENU_TOP          "Menu_Top"
;;#define GNOME_STOCK_MENU_BOTTOM       "Menu_Bottom"
;;#define GNOME_STOCK_MENU_ATTACH       "Menu_Attach"
;;#define GNOME_STOCK_MENU_INDEX        "Menu_Index"
;;#define GNOME_STOCK_MENU_FONT         "Menu_Font"
;;#define GNOME_STOCK_MENU_EXEC         "Menu_Exec"

;;#define GNOME_STOCK_MENU_ALIGN_LEFT     "Menu_Left"
;;#define GNOME_STOCK_MENU_ALIGN_RIGHT    "Menu_Right"
;;#define GNOME_STOCK_MENU_ALIGN_CENTER   "Menu_Center"
;;#define GNOME_STOCK_MENU_ALIGN_JUSTIFY  "Menu_Justify"

;;#define GNOME_STOCK_MENU_TEXT_BOLD      "Menu_Bold"
;;#define GNOME_STOCK_MENU_TEXT_ITALIC    "Menu_Italic"
;;#define GNOME_STOCK_MENU_TEXT_UNDERLINE "Menu_Underline"
;;#define GNOME_STOCK_MENU_TEXT_STRIKEOUT "Menu_Strikeout"

;;#define GNOME_STOCK_MENU_EXIT     GNOME_STOCK_MENU_QUIT


;;/* returns a GtkMenuItem with an stock icon and text */
;;GtkWidget             *gnome_stock_menu_item       (const char *type,
;;						    const char *text);


;; Creates a toplevel window with a shaped mask.  Useful for making the DnD
;; windows
;; GtkWidget *gnome_stock_transparent_window (const char *icon, const char *subtype);

;;;/*
;;; * Return a GdkPixmap and GdkMask for a stock pixmap
;;; */
;;;void gnome_stock_pixmap_gdk (const char *icon,
;;;			     const char *subtype,
;;;			     GdkPixmap **pixmap,
;;;			     GdkPixmap **mask);


(gtk-import-function GtkType gnome_druid_get_type)
(gtk-import-function GtkWidget gnome_druid_new)
(gtk-import-function void gnome_druid_set_buttons_sensitive
		     (GnomeDruid . druid)
		     (gboolean   . back_sensitive)
		     (gboolean   . next_sensitive)
		     (gboolean   . cancel_sensitive))
(gtk-import-function void gnome_druid_set_show_finish
		     (GnomeDruid . druid)
		     (gboolean   . show_finish))
(gtk-import-function void gnome_druid_prepend_page
		     (GnomeDruid . druid)
		     (GnomeDruidPage . page))
(gtk-import-function void gnome_druid_insert_page
		     (GnomeDruid . druid)
		     (GnomeDruidPage . back_page)
		     (GnomeDruidPage . page))
(gtk-import-function void gnome_druid_append_page
		     (GnomeDruid . druid)
		     (GnomeDruidPage . page))
(gtk-import-function void gnome_druid_set_page
		     (GnomeDruid . druid)
		     (GnomeDruidPage . page))

(gtk-import-function GtkType gnome_druid_page_get_type)
(gtk-import-function gboolean gnome_druid_page_next (GnomeDruidPage . druid_page))
(gtk-import-function gboolean gnome_druid_page_prepare (GnomeDruidPage . druid_page))
(gtk-import-function gboolean gnome_druid_page_back (GnomeDruidPage . druid_page))
(gtk-import-function gboolean gnome_druid_page_cancel (GnomeDruidPage . druid_page))
(gtk-import-function gboolean gnome_druid_page_finish (GnomeDruidPage . druid_page))


(gtk-import-function GtkType gnome_druid_page_start_get_type)
(gtk-import-function GtkWidget gnome_druid_page_start_new)

;; #### BOGUS!
'(gtk-import-function GtkWidget gnome_druid_page_start_new_with_vals
		     (GtkString . title)
		     (GtkString . text)
		     (GdkImlibImage . logo)
		     (GdkImlibImage . watermark))

(gtk-import-function void gnome_druid_page_start_set_bg_color
		     (GnomeDruidPageStart . druid_page_start)
		     (GdkColor . color))
(gtk-import-function void gnome_druid_page_start_set_textbox_color
		     (GnomeDruidPageStart . druid_page_start)
		     (GdkColor . color))
(gtk-import-function void gnome_druid_page_start_set_logo_bg_color
		     (GnomeDruidPageStart . druid_page_start)
		     (GdkColor . color))
(gtk-import-function void gnome_druid_page_start_set_title_color
		     (GnomeDruidPageStart . druid_page_start)
		     (GdkColor . color))
(gtk-import-function void gnome_druid_page_start_set_text_color
		     (GnomeDruidPageStart . druid_page_start)
		     (GdkColor . color))
(gtk-import-function void gnome_druid_page_start_set_text
		     (GnomeDruidPageStart . druid_page_start)
		     (GtkString . text))
(gtk-import-function void gnome_druid_page_start_set_title
		     (GnomeDruidPageStart . druid_page_start)
		     (GtkString . title))

;; #### BOGUS!
'(gtk-import-function void gnome_druid_page_start_set_logo
		     (GnomeDruidPageStart . druid_page_start)
		     (GdkImlibImage . logo_image))
;; #### BOGUS!
'(gtk-import-function void gnome_druid_page_start_set_watermark
		     (GnomeDruidPageStart . druid_page_start)
		     (GdkImlibImage . watermark))


(gtk-import-function GtkType gnome_druid_page_standard_get_type)
(gtk-import-function GtkWidget gnome_druid_page_standard_new)
;; #### BOGUS!
'(gtk-import-function GtkWidget gnome_druid_page_standard_new_with_vals
		     (GtkString . title)
		     (GdkImlibImage . logo))
(gtk-import-function void gnome_druid_page_standard_set_bg_color
		     (GnomeDruidPageStandard . druid_page_standard)
		     (GdkColor . color))
(gtk-import-function void gnome_druid_page_standard_set_logo_bg_color
		     (GnomeDruidPageStandard . druid_page_standard)
		     (GdkColor . color))
(gtk-import-function void gnome_druid_page_standard_set_title_color
		     (GnomeDruidPageStandard . druid_page_standard)
		     (GdkColor . color))
(gtk-import-function void gnome_druid_page_standard_set_title
		     (GnomeDruidPageStandard . druid_page_standard)
		     (GtkString . title))
;; #### BOGUS!
'(gtk-import-function void gnome_druid_page_standard_set_logo
		     (GnomeDruidPageStandard . druid_page_standard)
		     (GdkImlibImage . logo_image))


(gtk-import-function GtkType   gnome_druid_page_finish_get_type)
(gtk-import-function GtkWidget gnome_druid_page_finish_new)
(gtk-import-function GtkWidget gnome_druid_page_finish_new_with_vals
		     (GtkString . title)
		     (GtkString . text)
		     (GdkImlibImage . logo)
		     (GdkImlibImage . watermark))

(gtk-import-function void gnome_druid_page_finish_set_bg_color
		     (GnomeDruidPageFinish . druid_page_finish)
		     (GdkColor . color))
(gtk-import-function void gnome_druid_page_finish_set_textbox_color
		     (GnomeDruidPageFinish . druid_page_finish)
		     (GdkColor . color))
(gtk-import-function void gnome_druid_page_finish_set_logo_bg_color
		     (GnomeDruidPageFinish . druid_page_finish)
		     (GdkColor . color))
(gtk-import-function void gnome_druid_page_finish_set_title_color
		     (GnomeDruidPageFinish . druid_page_finish)
		     (GdkColor . color))
(gtk-import-function void gnome_druid_page_finish_set_text_color
		     (GnomeDruidPageFinish . druid_page_finish)
		     (GdkColor . color))
(gtk-import-function void gnome_druid_page_finish_set_text
		     (GnomeDruidPageFinish . druid_page_finish)
		     (GtkString . text))
(gtk-import-function void gnome_druid_page_finish_set_title
		     (GnomeDruidPageFinish . druid_page_finish)
		     (GtkString . title))
;; #### BOGUS!
'(gtk-import-function void gnome_druid_page_finish_set_logo
		     (GnomeDruidPageFinish . druid_page_finish)
		     (GdkImlibImage . logo_image))
;; #### BOGUS!
'(gtk-import-function void gnome_druid_page_finish_set_watermark
		     (GnomeDruidPageFinish . druid_page_finish)
		     (GdkImlibImage . watermark))

(provide 'gnome-widgets)
