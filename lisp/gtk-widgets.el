;;; gtk-widgets.el --- Import GTK functions into XEmacs

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

(gtk-import-function GtkAccelGroup gtk_accel_group_new)

(gtk-import-function GtkType gtk_accel_label_get_type)
(gtk-import-function GtkWidget gtk_accel_label_new GtkString)
(gtk-import-function guint gtk_accel_label_get_accel_width GtkAccelLabel)
(gtk-import-function nil gtk_accel_label_set_accel_widget GtkAccelLabel GtkWidget)
(gtk-import-function gboolean gtk_accel_label_refetch GtkAccelLabel)


(gtk-import-function GtkType gtk_adjustment_get_type)
(gtk-import-function GtkObject gtk_adjustment_new gfloat gfloat gfloat gfloat gfloat gfloat)
(gtk-import-function nil gtk_adjustment_changed GtkAdjustment)
(gtk-import-function nil gtk_adjustment_value_changed GtkAdjustment)
(gtk-import-function nil gtk_adjustment_clamp_page GtkAdjustment gfloat gfloat)
(gtk-import-function nil gtk_adjustment_set_value GtkAdjustment gfloat)


(gtk-import-function GtkType gtk_alignment_get_type)
(gtk-import-function GtkWidget gtk_alignment_new gfloat gfloat gfloat gfloat)
(gtk-import-function nil gtk_alignment_set GtkAlignment gfloat gfloat gfloat gfloat)


(gtk-import-function GtkType gtk_arrow_get_type)
(gtk-import-function GtkWidget gtk_arrow_new GtkArrowType GtkShadowType)
(gtk-import-function nil gtk_arrow_set GtkArrow GtkArrowType GtkShadowType)


(gtk-import-function GtkType gtk_aspect_frame_get_type)
(gtk-import-function GtkWidget gtk_aspect_frame_new GtkString gfloat gfloat gfloat gboolean)
(gtk-import-function nil gtk_aspect_frame_set GtkAspectFrame gfloat gfloat gfloat gboolean)


(gtk-import-function GtkType gtk_bin_get_type)


(gtk-import-function GtkType gtk_box_get_type)
(gtk-import-function nil gtk_box_pack_start
		     (GtkBox     . box)
		     (GtkWidget  . child)
		     (gboolean   . expand)
		     (gboolean   . fill)
		     (guint      . padding))

(gtk-import-function nil gtk_box_pack_end
		     (GtkBox     . box)
		     (GtkWidget  . child)
		     (gboolean   . expand)
		     (gboolean   . fill)
		     (guint      . padding))

(gtk-import-function nil gtk_box_pack_start_defaults
		     (GtkBox     . box)
		     (GtkWidget  . child))

(gtk-import-function nil gtk_box_pack_end_defaults
		     (GtkBox     . box)
		     (GtkWidget  . child))

(gtk-import-function nil gtk_box_set_homogeneous
		     (GtkBox     . box)
		     (gboolean   . homogeneous))

(gtk-import-function nil gtk_box_set_spacing
		     (GtkBox     . box)
		     (gint       . spacing))

(gtk-import-function nil gtk_box_reorder_child
		     (GtkBox	  . box)
		     (GtkWidget  . child)
		     (gint       . position))

;;;Handcoded in ui-byhand.c... #### FIXME
;;;void	   gtk_box_query_child_packing (GtkBox	     *box,
;;;					GtkWidget    *child,
;;;					gboolean     *expand,
;;;					gboolean     *fill,
;;;					guint	     *padding,
;;;					GtkPackType  *pack_type);

(gtk-import-function nil gtk_box_set_child_packing
		     (GtkBox      . box)
		     (GtkWidget   . child)
		     (gboolean    . expand)
		     (gboolean    . fill)
		     (guint       . padding)
		     (GtkPackType . pack_type))


(gtk-import-function GtkType gtk_button_get_type)
(gtk-import-function GtkWidget gtk_button_new)
(gtk-import-function GtkWidget gtk_button_new_with_label GtkString)
(gtk-import-function nil gtk_button_pressed GtkButton)
(gtk-import-function nil gtk_button_released GtkButton)
(gtk-import-function nil gtk_button_clicked GtkButton)
(gtk-import-function nil gtk_button_enter GtkButton)
(gtk-import-function nil gtk_button_leave GtkButton)
(gtk-import-function nil gtk_button_set_relief GtkButton GtkReliefStyle)
(gtk-import-function GtkReliefStyle gtk_button_get_relief GtkButton)

(defun gtk-button-new-with-pixmap (glyph)
  "Construct a new GtkButton object with a pixmap."
  (let ((button (gtk-button-new))
	(pixmap nil))
    (if (glyphp glyph)
	(setq pixmap (gtk-pixmap-new glyph nil))
      (setq pixmap glyph))
    (gtk-widget-show pixmap)
    (gtk-container-add button pixmap)
    button))


(gtk-import-function GtkType gtk_button_box_get_type)

;Handcoded in ui-byhand.c... #### FIXME
;;;void gtk_button_box_get_child_size_default (gint *min_width, gint *min_height);
;;;void gtk_button_box_get_child_ipadding_default (gint *ipad_x, gint *ipad_y);

(gtk-import-function nil gtk_button_box_set_child_size_default gint gint)
(gtk-import-function nil gtk_button_box_set_child_ipadding_default gint gint)
(gtk-import-function gint gtk_button_box_get_spacing GtkButtonBox)
(gtk-import-function GtkButtonBoxStyle gtk_button_box_get_layout GtkButtonBox)

;Handcoded in ui-byhand.c... #### FIXME
;;;void gtk_button_box_get_child_size (GtkButtonBox *widget,
;;;				    gint *min_width, gint *min_height);
;;;void gtk_button_box_get_child_ipadding (GtkButtonBox *widget, gint *ipad_x, gint *ipad_y);

(gtk-import-function nil gtk_button_box_set_spacing GtkButtonBox gint)
(gtk-import-function nil gtk_button_box_set_layout GtkButtonBox GtkButtonBoxStyle)
(gtk-import-function nil gtk_button_box_set_child_size GtkButtonBox gint gint)
(gtk-import-function nil gtk_button_box_set_child_ipadding GtkButtonBox gint gint)


(gtk-import-function GtkType gtk_calendar_get_type)
(gtk-import-function GtkWidget gtk_calendar_new)
(gtk-import-function gint gtk_calendar_select_month GtkCalendar guint guint)
(gtk-import-function nil gtk_calendar_select_day GtkCalendar guint)
(gtk-import-function gint gtk_calendar_mark_day GtkCalendar guint)
(gtk-import-function gint gtk_calendar_unmark_day GtkCalendar guint)
(gtk-import-function nil gtk_calendar_clear_marks GtkCalendar)
(gtk-import-function nil gtk_calendar_display_options GtkCalendar GtkCalendarDisplayOptions)

;Handcoded in ui-byhand.c... #### FIXME
;void	   gtk_calendar_get_date	(GtkCalendar *calendar, 
;					 guint	     *year,
;					 guint	     *month,
;					 guint	     *day);

(gtk-import-function nil gtk_calendar_freeze GtkCalendar)
(gtk-import-function nil gtk_calendar_thaw GtkCalendar)


(gtk-import-function GtkType gtk_check_button_get_type)
(gtk-import-function GtkWidget gtk_check_button_new)
(gtk-import-function GtkWidget gtk_check_button_new_with_label GtkString)


(gtk-import-function GtkType gtk_check_menu_item_get_type)
(gtk-import-function GtkWidget gtk_check_menu_item_new)
(gtk-import-function GtkWidget gtk_check_menu_item_new_with_label GtkString)
(gtk-import-function nil gtk_check_menu_item_set_active GtkCheckMenuItem gboolean)
(gtk-import-function nil gtk_check_menu_item_set_show_toggle GtkCheckMenuItem gboolean)
(gtk-import-function nil gtk_check_menu_item_toggled GtkCheckMenuItem)


(gtk-import-function GtkType gtk_clist_get_type)
(gtk-import-function GtkWidget gtk_clist_new gint)

(gtk-import-function GtkWidget gtk_clist_new_with_titles
		     (gint           . columns)
		     (GtkArrayOfString . titles))

;; set adjustments of clist
(gtk-import-function nil gtk_clist_set_hadjustment GtkCList GtkAdjustment)
(gtk-import-function nil gtk_clist_set_vadjustment GtkCList GtkAdjustment)

;; get adjustments of clist
(gtk-import-function GtkAdjustment gtk_clist_get_hadjustment GtkCList)
(gtk-import-function GtkAdjustment gtk_clist_get_vadjustment GtkCList)

;; set the border style of the clist
(gtk-import-function nil gtk_clist_set_shadow_type GtkCList GtkShadowType)

;; set the clist's selection mode
(gtk-import-function nil gtk_clist_set_selection_mode GtkCList GtkSelectionMode)

;; enable clists reorder ability
(gtk-import-function nil gtk_clist_set_reorderable GtkCList gboolean)
(gtk-import-function nil gtk_clist_set_use_drag_icons GtkCList gboolean)
(gtk-import-function nil gtk_clist_set_button_actions GtkCList guint guint)

;; freeze all visual updates of the list, and then thaw the list after
;; you have made a number of changes and the updates wil occure in a
;; more efficent mannor than if you made them on a unfrozen list
(gtk-import-function nil gtk_clist_freeze GtkCList)
(gtk-import-function nil gtk_clist_thaw GtkCList)

;; show and hide the column title buttons
(gtk-import-function nil gtk_clist_column_titles_show GtkCList)
(gtk-import-function nil gtk_clist_column_titles_hide GtkCList)

;; set the column title to be a active title (responds to button presses, 
;; prelights, and grabs keyboard focus), or passive where it acts as just
;; a title
(gtk-import-function nil gtk_clist_column_title_active GtkCList gint)
(gtk-import-function nil gtk_clist_column_title_passive GtkCList gint)
(gtk-import-function nil gtk_clist_column_titles_active GtkCList)
(gtk-import-function nil gtk_clist_column_titles_passive GtkCList)

;; set the title in the column title button
(gtk-import-function nil gtk_clist_set_column_title GtkCList gint GtkString)

;; returns the title of column. Returns NULL if title is not set */
(gtk-import-function GtkString gtk_clist_get_column_title GtkCList gint)

;; set a widget instead of a title for the column title button
(gtk-import-function nil gtk_clist_set_column_widget GtkCList gint GtkWidget)

;; returns the column widget
(gtk-import-function GtkWidget gtk_clist_get_column_widget GtkCList gint)

;; set the justification on a column
(gtk-import-function nil gtk_clist_set_column_justification GtkCList gint GtkJustification)

;; set visibility of a column
(gtk-import-function nil gtk_clist_set_column_visibility GtkCList gint gboolean)

;; enable/disable column resize operations by mouse
(gtk-import-function nil gtk_clist_set_column_resizeable GtkCList gint gboolean)

;; resize column automatically to its optimal width
(gtk-import-function nil gtk_clist_set_column_auto_resize GtkCList gint gboolean)
(gtk-import-function gint gtk_clist_columns_autosize GtkCList)

;; return the optimal column width, i.e. maximum of all cell widths
(gtk-import-function gint gtk_clist_optimal_column_width GtkCList gint)

;; set the pixel width of a column; this is a necessary step in
;; creating a CList because otherwise the column width is chozen from
;; the width of the column title, which will never be right

(gtk-import-function nil gtk_clist_set_column_width GtkCList gint gint)

;; set column minimum/maximum width. min/max_width < 0 => no restriction
(gtk-import-function nil gtk_clist_set_column_min_width GtkCList gint gint)
(gtk-import-function nil gtk_clist_set_column_max_width GtkCList gint gint)

;; change the height of the rows, the default (height=0) is
;; the hight of the current font.
(gtk-import-function nil gtk_clist_set_row_height GtkCList guint)

;; scroll the viewing area of the list to the given column and row;
;; row_align and col_align are between 0-1 representing the location the
;; row should appear on the screnn, 0.0 being top or left, 1.0 being
;; bottom or right; if row or column is -1 then then there is no change
(gtk-import-function nil gtk_clist_moveto GtkCList gint gint gfloat gfloat)

;; returns whether the row is visible
(gtk-import-function GtkVisibility gtk_clist_row_is_visible GtkCList gint)

;; returns the cell type
(gtk-import-function GtkCellType gtk_clist_get_cell_type GtkCList gint gint)

;; sets a given cell's text, replacing it's current contents
(gtk-import-function nil gtk_clist_set_text GtkCList gint gint GtkString)

;; for the "get" functions, any of the return pointer can be
;; NULL if you are not interested
;;
;;;Handcoded in ui-byhand.c... #### FIXME
;;;gint gtk_clist_get_text (GtkCList  *clist,
;;;			 gint       row,
;;;			 gint       column,
;;;			 gchar    **text);

;; #### BILL!!! Implement these!
;; (gtk-import-function nil gtk_clist_get_pixmap)
;; (gtk-import-function nil gtk_clist_get_pixtext)

(gtk-import-function nil gtk_clist_set_pixmap
		     (GtkCList . clist)
		     (gint     . row)
		     (gint     . column)
		     (GdkPixmap . pixmap)
		     (GdkBitmap . mask))
(gtk-import-function nil gtk_clist_set_pixtext
		     (GtkCList . clist)
		     (gint     . row)
		     (gint     . column)
		     (GtkString . text)
		     (gint      . spacing)
		     (GdkPixmap . pixmap)
		     (GdkBitmap . mask))

;; sets the foreground color of a row, the color must already
;; be allocated
(gtk-import-function nil gtk_clist_set_foreground GtkCList gint GdkColor)

;; sets the background color of a row, the color must already
;; be allocated
(gtk-import-function nil gtk_clist_set_background GtkCList gint GdkColor)

;; set / get cell styles
(gtk-import-function nil gtk_clist_set_cell_style GtkCList gint gint GtkStyle)
(gtk-import-function GtkStyle gtk_clist_get_cell_style GtkCList gint gint)
(gtk-import-function nil gtk_clist_set_row_style GtkCList gint GtkStyle)
(gtk-import-function GtkStyle gtk_clist_get_row_style GtkCList gint)

;; this sets a horizontal and vertical shift for drawing
;; the contents of a cell; it can be positive or negitive;
;; this is particulary useful for indenting items in a column
(gtk-import-function nil gtk_clist_set_shift GtkCList gint gint gint gint)

;; set/get selectable flag of a single row
(gtk-import-function nil gtk_clist_set_selectable GtkCList gint gboolean)
(gtk-import-function gboolean gtk_clist_get_selectable GtkCList gint)

;; prepend/append returns the index of the row you just added,
;; making it easier to append and modify a row

(gtk-import-function gint gtk_clist_prepend
		     (GtkCList         . clist)
		     (GtkArrayOfString . text))

(gtk-import-function gint gtk_clist_append
		     (GtkCList         . clist)
		     (GtkArrayOfString . text))

;; inserts a row at index row and returns the row where it was
;; actually inserted (may be different from "row" in auto_sort mode)
(gtk-import-function gint gtk_clist_insert
		     (GtkCList . clist)
		     (gint     . row)
		     (GtkArrayOfString . text))

;; removes row at index row
(gtk-import-function nil gtk_clist_remove GtkCList gint)

;; sets a arbitrary data pointer for a given row
(gtk-import-function nil gtk_clist_set_row_data GtkCList gint gpointer)

;; sets a data pointer for a given row with destroy notification
;; #### Need to handle callbacks.
;;;void gtk_clist_set_row_data_full (GtkCList         *clist,
;;;			          gint              row,
;;;			          gpointer          data,
;;;				  GtkDestroyNotify  destroy);

;; returns the data set for a row
(gtk-import-function gpointer gtk_clist_get_row_data GtkCList gint)

;; givin a data pointer, find the first (and hopefully only!)
;; row that points to that data, or -1 if none do
(gtk-import-function gint gtk_clist_find_row_from_data GtkCList gpointer)

;; force selection of a row
(gtk-import-function nil gtk_clist_select_row GtkCList gint gint)

;; force unselection of a row
(gtk-import-function nil gtk_clist_unselect_row GtkCList gint gint)

;; undo the last select/unselect operation
(gtk-import-function nil gtk_clist_undo_selection GtkCList)

;; clear the entire list -- this is much faster than removing
;; each item with gtk_clist_remove
(gtk-import-function nil gtk_clist_clear GtkCList)

;; return the row column corresponding to the x and y coordinates,
;; the returned values are only valid if the x and y coordinates
;; are respectively to a window == clist->clist_window
;;
;;;Handcoded in ui-byhand.c... #### FIXME
;;;gint gtk_clist_get_selection_info (GtkCList *clist,
;;;			     	   gint      x,
;;;			     	   gint      y,
;;;			     	   gint     *row,
;;;			     	   gint     *column);

;; in multiple or extended mode, select all rows
(gtk-import-function nil gtk_clist_select_all GtkCList)

;; in all modes except browse mode, deselect all rows
(gtk-import-function nil gtk_clist_unselect_all GtkCList)

;; swap the position of two rows
(gtk-import-function nil gtk_clist_swap_rows GtkCList gint gint)

;; move row from source_row position to dest_row position
(gtk-import-function nil gtk_clist_row_move GtkCList gint gint)

;; sets a compare function different to the default
;;;void gtk_clist_set_compare_func (GtkCList            *clist,
;;;				 GtkCListCompareFunc  cmp_func);

;; the column to sort by
(gtk-import-function nil gtk_clist_set_sort_column GtkCList gint)

;; how to sort : ascending or descending
(gtk-import-function nil gtk_clist_set_sort_type GtkCList GtkSortType)

;; sort the list with the current compare function
(gtk-import-function nil gtk_clist_sort GtkCList)

;; Automatically sort upon insertion
(gtk-import-function nil gtk_clist_set_auto_sort GtkCList gboolean)
		     

;; ColorSelection

(gtk-import-function GtkType gtk_color_selection_get_type)
(gtk-import-function GtkWidget gtk_color_selection_new)
(gtk-import-function nil gtk_color_selection_set_update_policy GtkColorSelection GtkUpdateType)
(gtk-import-function nil gtk_color_selection_set_opacity GtkColorSelection gint)
(gtk-import-function nil gtk_color_selection_set_color GtkColorSelection gdouble)

;;;Handcoded in ui-byhand.c... #### FIXME
;void       gtk_color_selection_get_color         (GtkColorSelection     *colorsel,
;                                                  gdouble               *color);

;; ColorSelectionDialog
(gtk-import-function GtkType gtk_color_selection_dialog_get_type)
(gtk-import-function GtkWidget gtk_color_selection_dialog_new GtkString)


(gtk-import-function GtkType gtk_combo_get_type)
(gtk-import-function GtkWidget gtk_combo_new)

;; the text in the entry must be or not be in the list
(gtk-import-function nil gtk_combo_set_value_in_list GtkCombo gint gint)

;; set/unset arrows working for changing the value (can be annoying)
(gtk-import-function nil gtk_combo_set_use_arrows GtkCombo gint)

;; up/down arrows change value if current value not in list
(gtk-import-function nil gtk_combo_set_use_arrows_always GtkCombo gint)

;; perform case-sensitive compares
(gtk-import-function nil gtk_combo_set_case_sensitive GtkCombo gint)

;; call this function on an item if it isn't a label or you
;; want it to have a different value to be displayed in the entry
(gtk-import-function nil gtk_combo_set_item_string GtkCombo GtkItem GtkString)

(gtk-import-function nil gtk_combo_set_popdown_strings
		     (GtkCombo . combo)
		     (GtkListOfString . strings))

(gtk-import-function nil gtk_combo_disable_activate GtkCombo)


(gtk-import-function GtkType gtk_container_get_type)
(gtk-import-function nil gtk_container_set_border_width GtkContainer guint)
(gtk-import-function nil gtk_container_add GtkContainer GtkWidget)
(gtk-import-function nil gtk_container_remove GtkContainer GtkWidget)
(gtk-import-function nil gtk_container_set_resize_mode GtkContainer GtkResizeMode)
(gtk-import-function nil gtk_container_check_resize GtkContainer)

;; You can emulate this with (mapcar (lambda (x) ..) (gtk-container-children))

;;(gtk-import-function nil gtk_container_foreach GtkContainer GtkCallback)

; I don't think we really want to deal with this... ever.  #### FIXME?
;void    gtk_container_foreach_full	 (GtkContainer	   *container,
;					  GtkCallback	    callback,
;					  GtkCallbackMarshal marshal,
;					  gpointer	    callback_data,
;					  GtkDestroyNotify  notify);

(gtk-import-function GtkListOfObject gtk_container_children
		     (GtkContainer . container))

(gtk-import-function gint gtk_container_focus GtkContainer GtkDirectionType)

;;; Widget-level methods
(gtk-import-function nil gtk_container_set_reallocate_redraws GtkContainer gboolean)
(gtk-import-function nil gtk_container_set_focus_child GtkContainer GtkWidget)
(gtk-import-function nil gtk_container_set_focus_vadjustment GtkContainer GtkAdjustment)
(gtk-import-function nil gtk_container_set_focus_hadjustment GtkContainer GtkAdjustment)
(gtk-import-function nil gtk_container_register_toplevel GtkContainer)
(gtk-import-function nil gtk_container_unregister_toplevel GtkContainer)

(gtk-import-function GtkListOfObject gtk_container_get_toplevels)

(gtk-import-function nil gtk_container_resize_children GtkContainer)
(gtk-import-function guint gtk_container_child_type GtkContainer)

; the `arg_name' argument needs to be a const static string */
;void    gtk_container_add_child_arg_type   (const gchar      *arg_name,
;					    GtkType           arg_type,
;					    guint             arg_flags,
;					    guint             arg_id);
     
;/* Allocate a GtkArg array of size nargs that hold the
; * names and types of the args that can be used with
; * gtk_container_child_getv/gtk_container_child_setv.
; * if (arg_flags!=NULL),
; * (*arg_flags) will be set to point to a newly allocated
; * guint array that holds the flags of the args.
; * It is the callers response to do a
; * g_free (returned_args); g_free (*arg_flags).
; */
;GtkArg* gtk_container_query_child_args	   (GtkType	       class_type,
;					    guint32          **arg_flags,
;					    guint             *nargs);

;/* gtk_container_child_getv() sets an arguments type and value, or just
; * its type to GTK_TYPE_INVALID.
; * if GTK_FUNDAMENTAL_TYPE (arg->type) == GTK_TYPE_STRING, it's the callers
; * response to do a g_free (GTK_VALUE_STRING (arg));
; */
;void    gtk_container_child_getv	   (GtkContainer      *container,
;					    GtkWidget	      *child,
;					    guint	       n_args,
;					    GtkArg	      *args);
;void    gtk_container_child_setv   	   (GtkContainer      *container,
;					    GtkWidget	      *child,
;					    guint	       n_args,
;					    GtkArg	      *args);

;/* gtk_container_add_with_args() takes a variable argument list of the form:
; * (..., gchar *arg_name, ARG_VALUES, [repeatedly name/value pairs,] NULL)
; * where ARG_VALUES type depend on the argument and can consist of
; * more than one c-function argument.
; */
;void    gtk_container_add_with_args	   (GtkContainer      *container,
;					    GtkWidget	      *widget,
;					    const gchar	      *first_arg_name,
;					    ...);
;void    gtk_container_addv		   (GtkContainer      *container,
;					    GtkWidget	      *widget,
;					    guint	       n_args,
;					    GtkArg	      *args);
;void	gtk_container_child_set		   (GtkContainer      *container,
;					    GtkWidget         *child,
;					    const gchar	      *first_arg_name,
;					    ...);


(gtk-import-function GtkType gtk_curve_get_type)
(gtk-import-function GtkWidget gtk_curve_new)
(gtk-import-function nil gtk_curve_reset GtkCurve)
(gtk-import-function nil gtk_curve_set_gamma GtkCurve gfloat)
(gtk-import-function nil gtk_curve_set_range GtkCurve gfloat gfloat gfloat gfloat)

;Handcoded in ui-byhand.c... #### FIXME
;;void		gtk_curve_get_vector	(GtkCurve *curve,
;;					 int veclen, gfloat vector[]);
;;
;;void		gtk_curve_set_vector	(GtkCurve *curve,
;;					 int veclen, gfloat vector[]);

(gtk-import-function nil gtk_curve_set_curve_type GtkCurve GtkCurveType)


(gtk-import-function GtkType gtk_data_get_type)


(gtk-import-function GtkType gtk_dialog_get_type)
(gtk-import-function GtkWidget gtk_dialog_new)


(gtk-import-function GtkType gtk_drawing_area_get_type)
(gtk-import-function GtkWidget gtk_drawing_area_new)
(gtk-import-function nil gtk_drawing_area_size GtkDrawingArea gint gint)


(gtk-import-function GtkType gtk_editable_get_type)
(gtk-import-function nil gtk_editable_select_region GtkEditable gint gint)

;;;Handcoded in ui-byhand.c... #### FIXME
;;;(gtk-import-function nil gtk_editable_insert_text GtkEditable GtkString gint pointer-to-gint)

(gtk-import-function nil gtk_editable_delete_text GtkEditable gint gint)
(gtk-import-function GtkString gtk_editable_get_chars GtkEditable gint gint)
(gtk-import-function nil gtk_editable_cut_clipboard GtkEditable)
(gtk-import-function nil gtk_editable_copy_clipboard GtkEditable)
(gtk-import-function nil gtk_editable_paste_clipboard GtkEditable)
(gtk-import-function nil gtk_editable_claim_selection GtkEditable gboolean guint)
(gtk-import-function nil gtk_editable_delete_selection GtkEditable)
(gtk-import-function nil gtk_editable_changed GtkEditable)
(gtk-import-function nil gtk_editable_set_position GtkEditable gint)
(gtk-import-function gint gtk_editable_get_position GtkEditable)
(gtk-import-function nil gtk_editable_set_editable GtkEditable gboolean)


(gtk-import-function GtkType gtk_entry_get_type)
(gtk-import-function GtkWidget gtk_entry_new)
(gtk-import-function GtkWidget gtk_entry_new_with_max_length guint)
(gtk-import-function nil gtk_entry_set_text GtkEntry GtkString)
(gtk-import-function nil gtk_entry_append_text GtkEntry GtkString)
(gtk-import-function nil gtk_entry_prepend_text GtkEntry GtkString)
(gtk-import-function nil gtk_entry_set_position GtkEntry gint)

;; returns a reference to the text
(gtk-import-function GtkString gtk_entry_get_text GtkEntry)
(gtk-import-function nil gtk_entry_select_region GtkEntry gint gint)
(gtk-import-function nil gtk_entry_set_visibility GtkEntry gboolean)
(gtk-import-function nil gtk_entry_set_editable GtkEntry gboolean)

;; text is truncated if needed
(gtk-import-function nil gtk_entry_set_max_length GtkEntry guint)


(gtk-import-function GtkType gtk_event_box_get_type)
(gtk-import-function GtkWidget gtk_event_box_new)


(gtk-import-function GtkType gtk_file_selection_get_type)
(gtk-import-function GtkWidget gtk_file_selection_new GtkString)
(gtk-import-function nil gtk_file_selection_set_filename GtkFileSelection GtkString)
(gtk-import-function GtkString gtk_file_selection_get_filename GtkFileSelection)
(gtk-import-function nil gtk_file_selection_complete GtkFileSelection GtkString)
(gtk-import-function nil gtk_file_selection_show_fileop_buttons GtkFileSelection)
(gtk-import-function nil gtk_file_selection_hide_fileop_buttons GtkFileSelection)


(gtk-import-function GtkType gtk_fixed_get_type)
(gtk-import-function GtkWidget gtk_fixed_new)
(gtk-import-function nil gtk_fixed_put GtkFixed GtkWidget gint gint)
(gtk-import-function nil gtk_fixed_move  GtkFixed GtkWidget gint gint)


(gtk-import-function GtkType gtk_font_selection_get_type)
(gtk-import-function GtkWidget gtk_font_selection_new)
(gtk-import-function GtkString gtk_font_selection_get_font_name GtkFontSelection)
;(gtk-import-function GdkFont gtk_font_selection_get_font GtkFontSelection)
(gtk-import-function gboolean gtk_font_selection_set_font_name GtkFontSelection GtkString)


(gtk-import-function nil gtk_font_selection_set_filter
		     (GtkFontSelection  . fontsel)
		     (GtkFontFilterType . filter_type)
		     (GtkFontType       . font_type)
		     (GtkArrayOfString  . foundries)
		     (GtkArrayOfString  . weights)
		     (GtkArrayOfString  . slants)
		     (GtkArrayOfString  . setwidths)
		     (GtkArrayOfString  . spacings)
		     (GtkArrayOfString  . charsets))

(gtk-import-function GtkString gtk_font_selection_get_preview_text GtkFontSelection)
(gtk-import-function nil gtk_font_selection_set_preview_text GtkFontSelection GtkString)

;; GtkFontSelectionDialog functions.
;;   most of these functions simply call the corresponding function in the
;;   GtkFontSelection.

(gtk-import-function GtkType gtk_font_selection_dialog_get_type)
(gtk-import-function GtkWidget gtk_font_selection_dialog_new GtkString)

;; This returns the X Logical Font Description fontname, or NULL if no font
;; is selected. Note that there is a slight possibility that the font might not
;; have been loaded OK. You should call gtk_font_selection_dialog_get_font()
;; to see if it has been loaded OK.
(gtk-import-function GtkString gtk_font_selection_dialog_get_font_name GtkFontSelectionDialog)

;; This will return the current GdkFont, or NULL if none is selected or there
;; was a problem loading it. Remember to use gdk_font_ref/unref() if you want
;; to use the font (in a style, for example)
;; GdkFont* gtk_font_selection_dialog_get_font	    (GtkFontSelectionDialog *fsd);

;; This sets the currently displayed font. It should be a valid X Logical
;; Font Description font name (anything else will be ignored), e.g.
;; "-adobe-courier-bold-o-normal--25-*-*-*-*-*-*-*" 
;; It returns TRUE on success.
(gtk-import-function gboolean gtk_font_selection_dialog_set_font_name GtkFontSelectionDialog GtkString)

;; This sets one of the font filters, to limit the fonts shown. The filter_type
;; is GTK_FONT_FILTER_BASE or GTK_FONT_FILTER_USER. The font type is a
;; combination of the bit flags GTK_FONT_BITMAP, GTK_FONT_SCALABLE and
;; GTK_FONT_SCALABLE_BITMAP (or GTK_FONT_ALL for all font types).
;; The foundries, weights etc. are arrays of strings containing property
;; values, e.g. 'bold', 'demibold', and *MUST* finish with a NULL.
;; Standard long names are also accepted, e.g. 'italic' instead of 'i'.
;;
;; e.g. to allow only fixed-width fonts ('char cell' or 'monospaced') to be
;; selected use:
;;
;;gchar *spacings[] = { "c", "m", NULL };
;;gtk_font_selection_dialog_set_filter (GTK_FONT_SELECTION_DIALOG (fontsel),
;;				       GTK_FONT_FILTER_BASE, GTK_FONT_ALL,
;;				       NULL, NULL, NULL, NULL, spacings, NULL);
;;
;;  to allow only true scalable fonts to be selected use:
;;
;;  gtk_font_selection_dialog_set_filter (GTK_FONT_SELECTION_DIALOG (fontsel),
;;				       GTK_FONT_FILTER_BASE, GTK_FONT_SCALABLE,
;;				       NULL, NULL, NULL, NULL, NULL, NULL);

;;; #### BILL!!! You can do this by just call
;;; gtk_font_selection_set_filter on the appropriate slot of the
;;; dialog.  Why bother with another function?
;;;void	   gtk_font_selection_dialog_set_filter	(GtkFontSelectionDialog *fsd,
;;;						 GtkFontFilterType filter_type,
;;;						 GtkFontType	   font_type,
;;;						 gchar		 **foundries,
;;;						 gchar		 **weights,
;;;						 gchar		 **slants,
;;;						 gchar		 **setwidths,
;;;						 gchar		 **spacings,
;;;						 gchar		 **charsets);

;; This returns the text in the preview entry.
(gtk-import-function GtkString gtk_font_selection_dialog_get_preview_text GtkFontSelectionDialog)

;; This sets the text in the preview entry. It will be copied by the entry,
;; so there's no need to g_strdup() it first.
(gtk-import-function nil gtk_font_selection_dialog_set_preview_text GtkFontSelectionDialog GtkString)


(gtk-import-function GtkType gtk_frame_get_type)
(gtk-import-function GtkWidget gtk_frame_new GtkString)
(gtk-import-function nil gtk_frame_set_label GtkFrame GtkString)
(gtk-import-function nil gtk_frame_set_label_align GtkFrame gfloat gfloat)
(gtk-import-function nil gtk_frame_set_shadow_type GtkFrame GtkShadowType)


(gtk-import-function GtkType gtk_gamma_curve_get_type)
(gtk-import-function GtkWidget gtk_gamma_curve_new)


(gtk-import-function GtkType gtk_handle_box_get_type)
(gtk-import-function GtkWidget gtk_handle_box_new)
(gtk-import-function nil gtk_handle_box_set_shadow_type GtkHandleBox GtkShadowType)
(gtk-import-function nil gtk_handle_box_set_handle_position GtkHandleBox GtkPositionType)
(gtk-import-function nil gtk_handle_box_set_snap_edge GtkHandleBox GtkPositionType)


(gtk-import-function GtkType gtk_hbox_get_type)
(gtk-import-function GtkWidget gtk_hbox_new gboolean gint)


(gtk-import-function GtkType gtk_hbutton_box_get_type)
(gtk-import-function GtkWidget gtk_hbutton_box_new)

;; buttons can be added by gtk_container_add()
(gtk-import-function gint gtk_hbutton_box_get_spacing_default)
(gtk-import-function nil gtk_hbutton_box_set_spacing_default gint)

(gtk-import-function GtkButtonBoxStyle gtk_hbutton_box_get_layout_default)
(gtk-import-function nil gtk_hbutton_box_set_layout_default GtkButtonBoxStyle)


(gtk-import-function GtkType gtk_hpaned_get_type)
(gtk-import-function GtkWidget gtk_hpaned_new)


(gtk-import-function GtkType gtk_hruler_get_type)
(gtk-import-function GtkWidget gtk_hruler_new)


(gtk-import-function GtkType gtk_hscale_get_type)
(gtk-import-function GtkWidget gtk_hscale_new GtkAdjustment)


(gtk-import-function GtkType gtk_hscrollbar_get_type)
(gtk-import-function GtkWidget gtk_hscrollbar_new GtkAdjustment)


(gtk-import-function GtkType gtk_hseparator_get_type)
(gtk-import-function GtkWidget gtk_hseparator_new)


(gtk-import-function GtkType gtk_input_dialog_get_type)
(gtk-import-function GtkWidget gtk_input_dialog_new)


(gtk-import-function GtkType gtk_invisible_get_type)
(gtk-import-function GtkWidget gtk_invisible_new)


(gtk-import-function GtkType gtk_item_get_type)
(gtk-import-function nil gtk_item_select GtkItem)
(gtk-import-function nil gtk_item_deselect GtkItem)
(gtk-import-function nil gtk_item_toggle GtkItem)


(gtk-import-function GtkType gtk_label_get_type)
(gtk-import-function GtkWidget gtk_label_new GtkString)
(gtk-import-function nil gtk_label_set_text GtkLabel GtkString)
(gtk-import-function nil gtk_label_set_justify GtkLabel GtkJustification)
(gtk-import-function nil gtk_label_set_pattern GtkLabel GtkString)
(gtk-import-function nil gtk_label_set_line_wrap GtkLabel gboolean)

;;;Handcoded in ui-byhand.c... #### FIXME
;void       gtk_label_get           (GtkLabel          *label,
;                                    gchar            **str);

;; Convenience function to set the name and pattern by parsing
;; a string with embedded underscores, and return the appropriate
;; key symbol for the accelerator.
(gtk-import-function guint gtk_label_parse_uline GtkLabel GtkString)


(gtk-import-function GtkType gtk_layout_get_type)
(gtk-import-function GtkWidget gtk_layout_new GtkAdjustment GtkAdjustment)
(gtk-import-function nil gtk_layout_put GtkLayout GtkWidget gint gint)
(gtk-import-function nil gtk_layout_move GtkLayout GtkWidget gint gint)
(gtk-import-function nil gtk_layout_set_size GtkLayout guint guint)

(gtk-import-function GtkAdjustment gtk_layout_get_hadjustment GtkLayout)
(gtk-import-function GtkAdjustment gtk_layout_get_vadjustment GtkLayout)
(gtk-import-function nil gtk_layout_set_hadjustment GtkLayout GtkAdjustment)
(gtk-import-function nil gtk_layout_set_vadjustment GtkLayout GtkAdjustment)

;; These disable and enable moving and repainting the scrolling window
;; of the GtkLayout, respectively.  If you want to update the layout's
;; offsets but do not want it to repaint itself, you should use these
;; functions.

;; - I don't understand these are supposed to work, so I suspect
;; - they don't now.                    OWT 1/20/98

(gtk-import-function nil gtk_layout_freeze GtkLayout)
(gtk-import-function nil gtk_layout_thaw GtkLayout)


(gtk-import-function GtkType gtk_list_get_type)
(gtk-import-function GtkWidget gtk_list_new)

(gtk-import-function nil gtk_list_insert_items
		     (GtkList         . list)
		     (GtkListOfObject . items)
		     (gint            . position))

(gtk-import-function nil gtk_list_append_items
		     (GtkList         . list)
		     (GtkListOfObject . items))
(gtk-import-function nil gtk_list_prepend_items
		     (GtkList         . list)
		     (GtkListOfObject . items))
(gtk-import-function nil gtk_list_remove_items
		     (GtkList         . list)
		     (GtkListOfObject . items))
(gtk-import-function nil gtk_list_remove_items_no_unref
		     (GtkList         . list)
		     (GtkListOfObject . items))

(gtk-import-function nil gtk_list_clear_items GtkList gint gint)
(gtk-import-function nil gtk_list_select_item GtkList gint)
(gtk-import-function nil gtk_list_unselect_item GtkList gint)
(gtk-import-function nil gtk_list_select_child GtkList GtkWidget)
(gtk-import-function nil gtk_list_unselect_child GtkList GtkWidget)
(gtk-import-function gint gtk_list_child_position GtkList GtkWidget)
(gtk-import-function nil gtk_list_set_selection_mode GtkList GtkSelectionMode)
(gtk-import-function nil gtk_list_extend_selection GtkList GtkScrollType gfloat gboolean)
(gtk-import-function nil gtk_list_start_selection GtkList)
(gtk-import-function nil gtk_list_end_selection GtkList)
(gtk-import-function nil gtk_list_select_all GtkList)
(gtk-import-function nil gtk_list_unselect_all GtkList)
(gtk-import-function nil gtk_list_scroll_horizontal GtkList GtkScrollType gfloat)
(gtk-import-function nil gtk_list_scroll_vertical GtkList  GtkScrollType gfloat)
(gtk-import-function nil gtk_list_toggle_add_mode GtkList)
(gtk-import-function nil gtk_list_toggle_focus_row GtkList)
(gtk-import-function nil gtk_list_toggle_row GtkList GtkWidget)
(gtk-import-function nil gtk_list_undo_selection GtkList)
(gtk-import-function nil gtk_list_end_drag_selection GtkList)


(gtk-import-function GtkType gtk_list_item_get_type)
(gtk-import-function GtkWidget gtk_list_item_new)
(gtk-import-function GtkWidget gtk_list_item_new_with_label GtkString)
(gtk-import-function nil gtk_list_item_select GtkListItem)
(gtk-import-function nil gtk_list_item_deselect GtkListItem)


(gtk-import-variable guint gtk_major_version)
(gtk-import-variable guint gtk_minor_version)
(gtk-import-variable guint gtk_micro_version)
(gtk-import-variable guint gtk_interface_age)
(gtk-import-variable guint gtk_binary_age)

(gtk-import-function GtkString gtk_check_version
		     (guint . required_major)
		     (guint . required_minor)
		     (guint . required_micro))

(gtk-import-function gboolean gtk_events_pending)
(gtk-import-function guint gtk_main_level)
(gtk-import-function nil gtk_main)
(gtk-import-function nil gtk_main_quit)
(gtk-import-function gint gtk_main_iteration)
(gtk-import-function gint gtk_main_iteration_do (gboolean . blocking))
(gtk-import-function gint gtk_true)
(gtk-import-function gint gtk_false)


(gtk-import-function GtkType gtk_menu_get_type)
(gtk-import-function GtkWidget gtk_menu_new)

;; Wrappers for the Menu Shell operations
(gtk-import-function nil gtk_menu_append GtkMenu GtkWidget)
(gtk-import-function nil gtk_menu_prepend GtkMenu GtkWidget)
(gtk-import-function nil gtk_menu_insert GtkMenu GtkWidget gint)

;; Display the menu onscreen
(gtk-import-function nil gtk_menu_popup GtkMenu GtkWidget GtkWidget
		     gpointer   ;; GtkMenuPositionFunc	func
		     gpointer
		     guint
		     guint)

;; Position the menu according to it's position function. Called
;; from gtkmenuitem.c when a menu-item changes its allocation
(gtk-import-function nil gtk_menu_reposition GtkMenu)
(gtk-import-function nil gtk_menu_popdown GtkMenu)

;; Keep track of the last menu item selected. (For the purposes
;; of the option menu
(gtk-import-function GtkWidget gtk_menu_get_active GtkMenu)
(gtk-import-function nil gtk_menu_set_active GtkMenu guint)

;; set/get the acclerator group that holds global accelerators (should
;; be added to the corresponding toplevel with gtk_window_add_accel_group().
(gtk-import-function nil gtk_menu_set_accel_group GtkMenu GtkAccelGroup)
(gtk-import-function GtkAccelGroup gtk_menu_get_accel_group GtkMenu)

;; get the accelerator group that is used internally by the menu for
;; underline accelerators while the menu is popped up.
(gtk-import-function GtkAccelGroup gtk_menu_get_uline_accel_group GtkMenu)
(gtk-import-function GtkAccelGroup gtk_menu_ensure_uline_accel_group GtkMenu)

;; A reference count is kept for a widget when it is attached to
;; a particular widget. This is typically a menu item; it may also
;; be a widget with a popup menu - for instance, the Notebook widget.
(gtk-import-function nil gtk_menu_attach_to_widget GtkMenu GtkWidget gpointer)
(gtk-import-function nil gtk_menu_detach GtkMenu)

;; This should be dumped in favor of data set when the menu is popped
;; up - that is currently in the ItemFactory code, but should be
;; in the Menu code.
(gtk-import-function GtkWidget gtk_menu_get_attach_widget GtkMenu)
(gtk-import-function nil gtk_menu_set_tearoff_state GtkMenu gboolean)

;; This sets the window manager title for the window that
;; appears when a menu is torn off
(gtk-import-function nil gtk_menu_set_title GtkMenu GtkString)

(gtk-import-function nil gtk_menu_reorder_child GtkMenu GtkWidget gint)


(gtk-import-function GtkType gtk_menu_bar_get_type)
(gtk-import-function GtkWidget gtk_menu_bar_new)
(gtk-import-function nil gtk_menu_bar_append GtkMenuBar GtkWidget)
(gtk-import-function nil gtk_menu_bar_prepend GtkMenuBar GtkWidget)
(gtk-import-function nil gtk_menu_bar_insert GtkMenuBar GtkWidget gint)
(gtk-import-function nil gtk_menu_bar_set_shadow_type GtkMenuBar GtkShadowType)


(gtk-import-function GtkType gtk_menu_item_get_type)
(gtk-import-function GtkWidget gtk_menu_item_new)
(gtk-import-function GtkWidget gtk_menu_item_new_with_label GtkString)
(gtk-import-function nil gtk_menu_item_set_submenu GtkMenuItem GtkWidget)
(gtk-import-function nil gtk_menu_item_remove_submenu GtkMenuItem)
(gtk-import-function nil gtk_menu_item_set_placement GtkMenuItem GtkSubmenuPlacement)
(gtk-import-function nil gtk_menu_item_configure GtkMenuItem gint gint)
(gtk-import-function nil gtk_menu_item_select GtkMenuItem)
(gtk-import-function nil gtk_menu_item_deselect GtkMenuItem)
(gtk-import-function nil gtk_menu_item_activate GtkMenuItem)
(gtk-import-function nil gtk_menu_item_right_justify GtkMenuItem)


(gtk-import-function GtkType gtk_misc_get_type)
(gtk-import-function nil gtk_misc_set_alignment
		     (GtkMisc . misc)
		     (gfloat  . xalign)
		     (gfloat  . yalign))

(gtk-import-function nil gtk_misc_set_padding
		     (GtkMisc . misc)
		     (gint    . xpad)
		     (gint    . ypad))


(gtk-import-function GtkType gtk_notebook_get_type)
(gtk-import-function GtkWidget gtk_notebook_new)
(gtk-import-function nil gtk_notebook_append_page GtkNotebook GtkWidget GtkWidget)
(gtk-import-function nil gtk_notebook_append_page_menu GtkNotebook GtkWidget GtkWidget GtkWidget)
(gtk-import-function nil gtk_notebook_prepend_page GtkNotebook GtkWidget GtkWidget)
(gtk-import-function nil gtk_notebook_prepend_page_menu GtkNotebook GtkWidget GtkWidget GtkWidget)
(gtk-import-function nil gtk_notebook_insert_page GtkNotebook GtkWidget GtkWidget gint)
(gtk-import-function nil gtk_notebook_insert_page_menu GtkNotebook GtkWidget GtkWidget GtkWidget gint)
(gtk-import-function nil gtk_notebook_remove_page GtkNotebook gint)

;;query, set current NoteebookPage
(gtk-import-function gint gtk_notebook_get_current_page GtkNotebook)
(gtk-import-function GtkWidget gtk_notebook_get_nth_page GtkNotebook gint)
(gtk-import-function gint gtk_notebook_page_num GtkNotebook GtkWidget)
(gtk-import-function nil gtk_notebook_set_page GtkNotebook gint)
(gtk-import-function nil gtk_notebook_next_page GtkNotebook)
(gtk-import-function nil gtk_notebook_prev_page GtkNotebook)

;; set Notebook, NotebookTab style
(gtk-import-function nil gtk_notebook_set_show_border GtkNotebook gboolean)
(gtk-import-function nil gtk_notebook_set_show_tabs GtkNotebook gboolean)
(gtk-import-function nil gtk_notebook_set_tab_pos GtkNotebook GtkPositionType)
(gtk-import-function nil gtk_notebook_set_homogeneous_tabs GtkNotebook gboolean)
(gtk-import-function nil gtk_notebook_set_tab_border GtkNotebook guint)
(gtk-import-function nil gtk_notebook_set_tab_hborder GtkNotebook guint)
(gtk-import-function nil gtk_notebook_set_tab_vborder GtkNotebook guint)
(gtk-import-function nil gtk_notebook_set_scrollable GtkNotebook gboolean)

;; enable/disable PopupMenu
(gtk-import-function nil gtk_notebook_popup_enable GtkNotebook)
(gtk-import-function nil gtk_notebook_popup_disable GtkNotebook)

;; query/set NotebookPage Properties
(gtk-import-function GtkWidget gtk_notebook_get_tab_label GtkNotebook GtkWidget)
(gtk-import-function nil gtk_notebook_set_tab_label GtkNotebook GtkWidget GtkWidget)
(gtk-import-function nil gtk_notebook_set_tab_label_text GtkNotebook GtkWidget GtkString)
(gtk-import-function GtkWidget gtk_notebook_get_menu_label GtkNotebook GtkWidget)
(gtk-import-function nil gtk_notebook_set_menu_label GtkNotebook GtkWidget GtkWidget)
(gtk-import-function nil gtk_notebook_set_menu_label_text GtkNotebook GtkWidget GtkString)

;;;Handcoded in ui-byhand.c... #### FIXME
;;;void gtk_notebook_query_tab_label_packing (GtkNotebook *notebook,
;;;					   GtkWidget   *child,
;;;					   gboolean    *expand,
;;;					   gboolean    *fill,
;;;					   GtkPackType *pack_type);
(gtk-import-function nil gtk_notebook_set_tab_label_packing GtkNotebook GtkWidget gboolean gboolean GtkPackType)

(gtk-import-function nil gtk_notebook_reorder_child GtkNotebook GtkWidget gint)


(gtk-import-function GtkType gtk_object_get_type)
;(gtk-import-function 'GtkObject gtk_object_newv 'guint 'guint 'GtkArg)
(gtk-import-function nil gtk_object_sink GtkObject)
(gtk-import-function nil gtk_object_ref GtkObject)
(gtk-import-function nil gtk_object_unref GtkObject)

;; Need to implement callbacks better before I can do this.
;;void gtk_object_weakref	  (GtkObject	    *object,
;;			   GtkDestroyNotify  notify,
;;			   gpointer	     data);
;;void gtk_object_weakunref (GtkObject	    *object,
;;			   GtkDestroyNotify  notify,
;;			   gpointer	     data);

(gtk-import-function nil gtk_object_destroy GtkObject)

;; gtk_object_[gs]etv* () are handled by our generic 'get' and 'put'
;; handlers for types of GtkObject


(gtk-import-function GtkType gtk_option_menu_get_type)
(gtk-import-function GtkWidget gtk_option_menu_new)
(gtk-import-function GtkWidget gtk_option_menu_get_menu GtkOptionMenu)
(gtk-import-function nil gtk_option_menu_set_menu GtkOptionMenu GtkWidget)
(gtk-import-function nil gtk_option_menu_remove_menu GtkOptionMenu)
(gtk-import-function nil gtk_option_menu_set_history GtkOptionMenu guint)


(gtk-import-function GtkType gtk_packer_get_type)
(gtk-import-function GtkWidget gtk_packer_new)
(gtk-import-function nil gtk_packer_add_defaults GtkPacker GtkWidget
		     GtkSideType GtkAnchorType GtkPackerOptions)
(gtk-import-function nil gtk_packer_add GtkPacker
		     GtkWidget
		     GtkSideType
		     GtkAnchorType
		     GtkPackerOptions
		     guint
		     guint
		     guint
		     guint
		     guint)

(gtk-import-function nil gtk_packer_set_child_packing GtkPacker
		     GtkWidget
		     GtkSideType
		     GtkAnchorType
		     GtkPackerOptions
		     guint
		     guint
		     guint
		     guint
		     guint)

(gtk-import-function nil gtk_packer_reorder_child GtkPacker GtkWidget gint)
(gtk-import-function nil gtk_packer_set_spacing GtkPacker guint)
(gtk-import-function nil gtk_packer_set_default_border_width GtkPacker guint)
(gtk-import-function nil gtk_packer_set_default_pad GtkPacker guint guint)
(gtk-import-function nil gtk_packer_set_default_ipad GtkPacker guint guint)


(gtk-import-function GtkType gtk_paned_get_type)
(gtk-import-function nil gtk_paned_add1 GtkPaned GtkWidget)
(gtk-import-function nil gtk_paned_add2 GtkPaned GtkWidget)
(gtk-import-function nil gtk_paned_pack1 GtkPaned GtkWidget gboolean gboolean)
(gtk-import-function nil gtk_paned_pack2 GtkPaned GtkWidget gboolean gboolean)
(gtk-import-function nil gtk_paned_set_position GtkPaned gint)
(gtk-import-function nil gtk_paned_set_handle_size GtkPaned guint)
(gtk-import-function nil gtk_paned_set_gutter_size GtkPaned guint)

;; Internal function... do we need to expose this?
(gtk-import-function nil gtk_paned_compute_position GtkPaned gint gint gint)


(gtk-import-function GtkType gtk_pixmap_get_type)
(gtk-import-function GtkWidget gtk_pixmap_new
		     (GdkPixmap . pixmap)
		     (GdkPixmap . mask))
(gtk-import-function nil gtk_pixmap_set
		     (GtkPixmap . object)
		     (GdkPixmap . pixmap)
		     (GdkPixmap . mask))

;Handcoded in ui-byhand.c... #### FIXME
;;;void	   gtk_pixmap_get	 (GtkPixmap  *pixmap,
;;;				  GdkPixmap **val,
;;;				  GdkBitmap **mask);

(gtk-import-function nil gtk_pixmap_set_build_insensitive
		     (GtkPixmap . pixmap)
		     (guint     . build))


(gtk-import-function GtkType gtk_plug_get_type)
(gtk-import-function GtkWidget gtk_plug_new guint)
(gtk-import-function nil gtk_plug_construct GtkPlug guint)


(gtk-import-function GtkType gtk_progress_get_type)
(gtk-import-function nil gtk_progress_set_show_text GtkProgress gint)
(gtk-import-function nil gtk_progress_set_text_alignment GtkProgress gfloat gfloat)
(gtk-import-function nil gtk_progress_set_format_string GtkProgress GtkString)
(gtk-import-function nil gtk_progress_set_adjustment GtkProgress GtkAdjustment)
(gtk-import-function nil gtk_progress_configure GtkProgress gfloat gfloat gfloat)
(gtk-import-function nil gtk_progress_set_percentage GtkProgress gfloat)
(gtk-import-function nil gtk_progress_set_value GtkProgress gfloat)
(gtk-import-function gfloat gtk_progress_get_value GtkProgress)
(gtk-import-function nil gtk_progress_set_activity_mode GtkProgress guint)
(gtk-import-function GtkString gtk_progress_get_current_text GtkProgress)
(gtk-import-function GtkString gtk_progress_get_text_from_value GtkProgress gfloat)
(gtk-import-function gfloat gtk_progress_get_current_percentage GtkProgress)
(gtk-import-function gfloat gtk_progress_get_percentage_from_value GtkProgress gfloat)


(gtk-import-function GtkType gtk_progress_bar_get_type)
(gtk-import-function GtkWidget gtk_progress_bar_new)
(gtk-import-function GtkWidget gtk_progress_bar_new_with_adjustment GtkAdjustment)
(gtk-import-function nil gtk_progress_bar_set_bar_style GtkProgressBar GtkProgressBarStyle)
(gtk-import-function nil gtk_progress_bar_set_discrete_blocks GtkProgressBar guint)
(gtk-import-function nil gtk_progress_bar_set_activity_step GtkProgressBar guint)
(gtk-import-function nil gtk_progress_bar_set_activity_blocks GtkProgressBar guint)
(gtk-import-function nil gtk_progress_bar_set_orientation GtkProgressBar GtkProgressBarOrientation)
(gtk-import-function nil gtk_progress_bar_update GtkProgressBar gfloat)


;; All of the gpointers below really need to be `GSList *'
;; For now, need to create the first radio button with 'nil' and then use
;; (gtk-radio-button-group first-radio) for the rest.
(gtk-import-function GtkType gtk_radio_button_get_type)
(gtk-import-function GtkWidget gtk_radio_button_new gpointer)
(gtk-import-function GtkWidget gtk_radio_button_new_from_widget GtkRadioButton)
(gtk-import-function GtkWidget gtk_radio_button_new_with_label gpointer GtkString)
(gtk-import-function GtkWidget gtk_radio_button_new_with_label_from_widget GtkRadioButton GtkString)
(gtk-import-function gpointer gtk_radio_button_group GtkRadioButton)
(gtk-import-function nil gtk_radio_button_set_group GtkRadioButton gpointer)


(gtk-import-function GtkType gtk_radio_menu_item_get_type)

;; #### BILLL!!
;; All of these gpointer args should be GList *
(gtk-import-function GtkWidget gtk_radio_menu_item_new gpointer)
(gtk-import-function GtkWidget gtk_radio_menu_item_new_with_label gpointer GtkString)
(gtk-import-function gpointer gtk_radio_menu_item_group GtkRadioMenuItem)
(gtk-import-function nil gtk_radio_menu_item_set_group GtkRadioMenuItem gpointer)


(gtk-import-function GtkType gtk_range_get_type)
(gtk-import-function GtkAdjustment gtk_range_get_adjustment GtkRange)
(gtk-import-function nil gtk_range_set_update_policy GtkRange GtkUpdateType)
(gtk-import-function nil gtk_range_set_adjustment GtkRange GtkAdjustment)

(gtk-import-function nil gtk_range_draw_background GtkRange)
(gtk-import-function nil gtk_range_clear_background GtkRange)
(gtk-import-function nil gtk_range_draw_trough GtkRange)
(gtk-import-function nil gtk_range_draw_slider GtkRange)
(gtk-import-function nil gtk_range_draw_step_forw GtkRange)
(gtk-import-function nil gtk_range_draw_step_back GtkRange)
(gtk-import-function nil gtk_range_slider_update GtkRange)

;;; #### BILL!!! I think all of these are just for subclassing
;;; widgets, which we will not be able to do.  Maybe much later.
;;;gint           gtk_range_trough_click           (GtkRange      *range,
;;;						 gint           x,
;;;						 gint           y,
;;;						 gfloat	       *jump_perc);

(gtk-import-function nil gtk_range_default_hslider_update GtkRange)
(gtk-import-function nil gtk_range_default_vslider_update GtkRange)

;;;gint           gtk_range_default_htrough_click  (GtkRange      *range,
;;;						 gint           x,
;;;						 gint           y,
;;;						 gfloat	       *jump_perc);
;;;gint           gtk_range_default_vtrough_click  (GtkRange      *range,
;;;						 gint           x,
;;;						 gint           y,
;;;						 gfloat	       *jump_perc);

(gtk-import-function nil gtk_range_default_hmotion GtkRange gint gint)
(gtk-import-function nil gtk_range_default_vmotion GtkRange gint gint)


(gtk-import-function GtkType gtk_ruler_get_type)
(gtk-import-function nil gtk_ruler_set_metric GtkRuler GtkMetricType)
(gtk-import-function nil gtk_ruler_set_range GtkRuler gfloat gfloat gfloat gfloat)
(gtk-import-function nil gtk_ruler_draw_ticks GtkRuler)
(gtk-import-function nil gtk_ruler_draw_pos GtkRuler)


(gtk-import-function GtkType gtk_scale_get_type)
(gtk-import-function nil gtk_scale_set_digits GtkScale gint)
(gtk-import-function nil gtk_scale_set_draw_value GtkScale gboolean)
(gtk-import-function nil gtk_scale_set_value_pos GtkScale GtkPositionType)
(gtk-import-function gint gtk_scale_get_value_width GtkScale)
(gtk-import-function nil gtk_scale_draw_value GtkScale)


(gtk-import-function GtkType gtk_scrollbar_get_type)


(gtk-import-function GtkType gtk_scrolled_window_get_type)
(gtk-import-function GtkWidget gtk_scrolled_window_new GtkAdjustment GtkAdjustment)
(gtk-import-function nil gtk_scrolled_window_set_hadjustment GtkScrolledWindow GtkAdjustment)
(gtk-import-function nil gtk_scrolled_window_set_vadjustment GtkScrolledWindow GtkAdjustment)
(gtk-import-function GtkAdjustment gtk_scrolled_window_get_hadjustment GtkScrolledWindow)
(gtk-import-function GtkAdjustment gtk_scrolled_window_get_vadjustment GtkScrolledWindow)
(gtk-import-function nil gtk_scrolled_window_set_policy GtkScrolledWindow GtkPolicyType GtkPolicyType)
(gtk-import-function nil gtk_scrolled_window_set_placement GtkScrolledWindow GtkCornerType)
(gtk-import-function nil gtk_scrolled_window_add_with_viewport GtkScrolledWindow GtkWidget)


(gtk-import-function GtkType gtk_separator_get_type)


(gtk-import-function GtkType gtk_socket_get_type)
(gtk-import-function GtkWidget gtk_socket_new)
(gtk-import-function nil gtk_socket_steal GtkSocket guint)


(gtk-import-function GtkType gtk_table_get_type)
(gtk-import-function GtkWidget gtk_table_new guint guint gboolean)
(gtk-import-function nil gtk_table_resize GtkTable guint guint)

(gtk-import-function nil gtk_table_attach GtkTable GtkWidget
		     guint guint guint guint GtkAttachOptions GtkAttachOptions guint
		     guint)

(gtk-import-function nil gtk_table_attach_defaults GtkTable GtkWidget guint guint guint guint)
(gtk-import-function nil gtk_table_set_row_spacing GtkTable guint guint)
(gtk-import-function nil gtk_table_set_col_spacing GtkTable guint guint)
(gtk-import-function nil gtk_table_set_row_spacings GtkTable guint)
(gtk-import-function nil gtk_table_set_col_spacings GtkTable guint)
(gtk-import-function nil gtk_table_set_homogeneous GtkTable gboolean)


(gtk-import-function GtkType gtk_tearoff_menu_item_get_type)
(gtk-import-function GtkWidget gtk_tearoff_menu_item_new)


(gtk-import-function GtkType gtk_text_get_type)
(gtk-import-function GtkWidget gtk_text_new GtkAdjustment GtkAdjustment)
(gtk-import-function nil gtk_text_set_editable GtkText gboolean)
(gtk-import-function nil gtk_text_set_word_wrap GtkText gint)
(gtk-import-function nil gtk_text_set_line_wrap GtkText gint)
(gtk-import-function nil gtk_text_set_adjustments GtkText GtkAdjustment GtkAdjustment)
(gtk-import-function nil gtk_text_set_point GtkText guint)
(gtk-import-function guint gtk_text_get_point GtkText)
(gtk-import-function guint gtk_text_get_length GtkText)
(gtk-import-function nil gtk_text_freeze GtkText)
(gtk-import-function nil gtk_text_thaw GtkText)
(gtk-import-function nil gtk_text_insert GtkText GdkFont GdkColor GdkColor GtkString gint)
(gtk-import-function nil gtk_text_backward_delete GtkText guint)
(gtk-import-function nil gtk_text_forward_delete GtkText guint)


(gtk-import-function GtkType gtk_tips_query_get_type)
(gtk-import-function GtkWidget gtk_tips_query_new)
(gtk-import-function nil gtk_tips_query_start_query GtkTipsQuery)
(gtk-import-function nil gtk_tips_query_stop_query GtkTipsQuery)
(gtk-import-function nil gtk_tips_query_set_caller GtkTipsQuery GtkWidget)
(gtk-import-function nil gtk_tips_query_set_labels GtkTipsQuery GtkString GtkString)


(gtk-import-function GtkType gtk_toggle_button_get_type)
(gtk-import-function GtkWidget gtk_toggle_button_new)
(gtk-import-function GtkWidget gtk_toggle_button_new_with_label GtkString)
(gtk-import-function nil gtk_toggle_button_set_mode GtkToggleButton gboolean)
(gtk-import-function nil gtk_toggle_button_set_active GtkToggleButton gboolean)
(gtk-import-function gboolean gtk_toggle_button_get_active GtkToggleButton)
(gtk-import-function nil gtk_toggle_button_toggled GtkToggleButton)


(gtk-import-function GtkType gtk_toolbar_get_type)
(gtk-import-function GtkWidget gtk_toolbar_new GtkOrientation GtkToolbarStyle)

;; Simple button items
;;; Handcoded in ui-byhand.c... #### FIXME
;;;GtkWidget* gtk_toolbar_append_item     (GtkToolbar      *toolbar,
;;;					const char      *text,
;;;					const char      *tooltip_text,
;;;					const char      *tooltip_private_text,
;;;					GtkWidget       *icon,
;;;					GtkSignalFunc    callback,
;;;					gpointer         user_data);
;;;GtkWidget* gtk_toolbar_prepend_item    (GtkToolbar      *toolbar,
;;;					const char      *text,
;;;					const char      *tooltip_text,
;;;					const char      *tooltip_private_text,
;;;					GtkWidget       *icon,
;;;					GtkSignalFunc    callback,
;;;					gpointer         user_data);
;;;GtkWidget* gtk_toolbar_insert_item     (GtkToolbar      *toolbar,
;;;					const char      *text,
;;;					const char      *tooltip_text,
;;;					const char      *tooltip_private_text,
;;;					GtkWidget       *icon,
;;;					GtkSignalFunc    callback,
;;;					gpointer         user_data,
;;;					gint             position);

;; Space Items
(gtk-import-function nil gtk_toolbar_append_space GtkToolbar)
(gtk-import-function nil gtk_toolbar_prepend_space GtkToolbar)
(gtk-import-function nil gtk_toolbar_insert_space GtkToolbar gint)

;; Any element type
;; Cannot currently do this!  Need to have something similar to
;; GtkCallback in order to deal with this.
;; Of what possible use are these functions?  I don't see the
;; difference between them and the _item functions.
;;
;; From looking at the code in gtktoolbar.c, the GtkWidget argument
;; here is ignored!!!
'(gtk-import-function GtkWidget gtk_toolbar_append_element GtkToolbar
		      GtkToolbarChildType
		      GtkWidget
		      GtkString
		      GtkString
		      GtkString
		      GtkWidget
		      GtkSignal
		      gpointer)

'(gtk-import-function GtkWidget gtk_toolbar_prepend_element GtkToolbar
		      GtkToolbarChildType
		      GtkWidget
		      GtkString
		      GtkString
		      GtkString
		      GtkWidget
		      GtkSignal
		      gpointer)

'(gtk-import-function GtkWidget gtk_toolbar_insert_element GtkToolbar
		      GtkToolbarChildType
		      GtkWidget
		      GtkString
		      GtkString
		      GtkString
		      GtkWidget
		      GtkSignal
		      gpointer
		      gint)

;; Generic Widgets
(gtk-import-function nil gtk_toolbar_append_widget GtkToolbar GtkWidget GtkString GtkString)
(gtk-import-function nil gtk_toolbar_prepend_widget GtkToolbar GtkWidget GtkString GtkString)
(gtk-import-function nil gtk_toolbar_insert_widget GtkToolbar GtkWidget GtkString GtkString gint)

;; Style functions
(gtk-import-function nil gtk_toolbar_set_orientation GtkToolbar GtkOrientation)
(gtk-import-function nil gtk_toolbar_set_style GtkToolbar GtkToolbarStyle)
(gtk-import-function nil gtk_toolbar_set_space_size GtkToolbar gint)
(gtk-import-function nil gtk_toolbar_set_space_style GtkToolbar GtkToolbarSpaceStyle)
(gtk-import-function nil gtk_toolbar_set_tooltips GtkToolbar gint)
(gtk-import-function nil gtk_toolbar_set_button_relief GtkToolbar GtkReliefStyle)
(gtk-import-function GtkReliefStyle gtk_toolbar_get_button_relief GtkToolbar)


(gtk-import-function GtkType gtk_tooltips_get_type)
(gtk-import-function GtkObject gtk_tooltips_new)
(gtk-import-function nil gtk_tooltips_enable GtkTooltips)
(gtk-import-function nil gtk_tooltips_disable GtkTooltips)
(gtk-import-function nil gtk_tooltips_set_delay GtkTooltips guint)
(gtk-import-function nil gtk_tooltips_set_tip GtkTooltips GtkWidget GtkString GtkString)
(gtk-import-function nil gtk_tooltips_set_colors GtkTooltips GdkColor GdkColor)

;;;GtkTooltipsData* gtk_tooltips_data_get	   (GtkWidget	  *widget);

(gtk-import-function nil gtk_tooltips_force_window GtkTooltips)


(gtk-import-function GtkType gtk_tree_get_type)
(gtk-import-function GtkWidget gtk_tree_new)

(gtk-import-function nil gtk_tree_append
		     (GtkTree    . tree)
		     (GtkWidget  . tree_item))
(gtk-import-function nil gtk_tree_prepend
		     (GtkTree    . tree)
		     (GtkWidget  . tree_item))

(gtk-import-function nil gtk_tree_insert
		     (GtkTree    . tree)
		     (GtkWidget  . tree_item)
		     (gint       . position))

(gtk-import-function nil gtk_tree_remove_items
		     (GtkTree         . tree)
		     (GtkListOfObject . items))

(gtk-import-function nil gtk_tree_clear_items
		     (GtkTree . tree)
		     (gint    . start)
		     (gint    . end))

(gtk-import-function nil gtk_tree_select_item
		     (GtkTree . tree)
		     (gint    . item))

(gtk-import-function nil gtk_tree_unselect_item
		     (GtkTree . tree)
		     (gint    . item))

(gtk-import-function nil gtk_tree_select_child
		     (GtkTree    . tree)
		     (GtkWidget  . tree_item))

(gtk-import-function nil gtk_tree_unselect_child
		     (GtkTree    . tree)
		     (GtkWidget  . tree_item))

(gtk-import-function gint gtk_tree_child_position
		     (GtkTree    . tree)
		     (GtkWidget  . child))

(gtk-import-function nil gtk_tree_set_selection_mode
		     (GtkTree          . tree)
		     (GtkSelectionMode . mode))

(gtk-import-function nil gtk_tree_set_view_mode
		     (GtkTree         . tree)
		     (GtkTreeViewMode . mode))

(gtk-import-function nil gtk_tree_set_view_lines
		     (GtkTree  . tree)
		     (gboolean . flag))

;; deprecated function, use gtk_container_remove instead.
(gtk-import-function nil gtk_tree_remove_item
		     (GtkTree   . tree)
		     (GtkWidget . child))


(gtk-import-function GtkType gtk_tree_item_get_type)
(gtk-import-function GtkWidget gtk_tree_item_new)
(gtk-import-function GtkWidget gtk_tree_item_new_with_label GtkString)
(gtk-import-function nil gtk_tree_item_set_subtree GtkTreeItem GtkWidget)
(gtk-import-function nil gtk_tree_item_remove_subtree GtkTreeItem)
(gtk-import-function nil gtk_tree_item_select GtkTreeItem)
(gtk-import-function nil gtk_tree_item_deselect GtkTreeItem)
(gtk-import-function nil gtk_tree_item_expand GtkTreeItem)
(gtk-import-function nil gtk_tree_item_collapse GtkTreeItem)


(gtk-import-function GtkString gtk_type_name GtkType)
(gtk-import-function guint gtk_type_from_name GtkString)


(gtk-import-function GtkType gtk_vbox_get_type)
(gtk-import-function GtkWidget gtk_vbox_new gboolean gint)


(gtk-import-function GtkType gtk_vbutton_box_get_type)
(gtk-import-function GtkWidget gtk_vbutton_box_new)

;; buttons can be added by gtk_container_add()
(gtk-import-function gint gtk_vbutton_box_get_spacing_default)
(gtk-import-function nil gtk_vbutton_box_set_spacing_default gint)

(gtk-import-function GtkButtonBoxStyle gtk_vbutton_box_get_layout_default)
(gtk-import-function nil gtk_vbutton_box_set_layout_default GtkButtonBoxStyle)


(gtk-import-function GtkType gtk_viewport_get_type)
(gtk-import-function GtkWidget gtk_viewport_new GtkAdjustment GtkAdjustment)
(gtk-import-function GtkAdjustment gtk_viewport_get_hadjustment GtkViewport)
(gtk-import-function GtkAdjustment gtk_viewport_get_vadjustment GtkViewport)
(gtk-import-function nil gtk_viewport_set_hadjustment GtkViewport GtkAdjustment)
(gtk-import-function nil gtk_viewport_set_vadjustment GtkViewport GtkAdjustment)
(gtk-import-function nil gtk_viewport_set_shadow_type GtkViewport GtkShadowType)


(gtk-import-function GtkType gtk_vpaned_get_type)
(gtk-import-function GtkWidget gtk_vpaned_new)


(gtk-import-function GtkType gtk_vruler_get_type)
(gtk-import-function GtkWidget gtk_vruler_new)


(gtk-import-function GtkType gtk_vscale_get_type)
(gtk-import-function GtkWidget gtk_vscale_new GtkAdjustment)


(gtk-import-function GtkType gtk_vscrollbar_get_type)
(gtk-import-function GtkWidget gtk_vscrollbar_new GtkAdjustment)


(gtk-import-function GtkType gtk_vseparator_get_type)
(gtk-import-function GtkWidget gtk_vseparator_new)


(gtk-import-function GtkType gtk_widget_get_type)
(gtk-import-function nil gtk_widget_ref GtkWidget)
(gtk-import-function nil gtk_widget_unref GtkWidget)
(gtk-import-function nil gtk_widget_destroy GtkWidget)
(gtk-import-function nil gtk_widget_unparent GtkWidget)
(gtk-import-function nil gtk_widget_show GtkWidget)
(gtk-import-function nil gtk_widget_show_now GtkWidget)
(gtk-import-function nil gtk_widget_hide GtkWidget)
(gtk-import-function nil gtk_widget_show_all GtkWidget)
(gtk-import-function nil gtk_widget_hide_all GtkWidget)
(gtk-import-function nil gtk_widget_map GtkWidget)
(gtk-import-function nil gtk_widget_unmap GtkWidget)
(gtk-import-function nil gtk_widget_realize GtkWidget)
(gtk-import-function nil gtk_widget_unrealize GtkWidget)

(gtk-import-function nil gtk_widget_queue_draw GtkWidget)
(gtk-import-function nil gtk_widget_queue_draw_area GtkWidget gint gint gint gint)
(gtk-import-function nil gtk_widget_queue_clear GtkWidget)
(gtk-import-function nil gtk_widget_queue_clear_area GtkWidget gint gint gint gint)
(gtk-import-function nil gtk_widget_queue_resize GtkWidget)

;;; #### BILL!!!
;(gtk-import-function nil gtk_widget_draw 'GtkWidget 'GdkRectangle)
;(gtk-import-function nil gtk_widget_size_request 'GtkWidget 'GtkRequisition)
;(gtk-import-function nil gtk_widget_size_allocate 'GtkWidget 'GtkAllocation)
;(gtk-import-function nil gtk_widget_get_child_requisition 'GtkWidget 'GtkRequisition)
;(gtk-import-function 'gint gtk_widget_intersect 'GtkWidget 'GdkRectangle 'GdkRectangle)

(gtk-import-function nil gtk_widget_draw_focus GtkWidget)
(gtk-import-function nil gtk_widget_draw_default GtkWidget)
(gtk-import-function nil gtk_widget_add_accelerator GtkWidget GtkString GtkAccelGroup
		     guint guint GtkAccelFlags)
(gtk-import-function nil gtk_widget_remove_accelerator GtkWidget GtkAccelGroup guint guint)
(gtk-import-function nil gtk_widget_remove_accelerators GtkWidget GtkString gboolean)
(gtk-import-function guint gtk_widget_accelerator_signal GtkWidget GtkAccelGroup guint guint)
(gtk-import-function nil gtk_widget_lock_accelerators GtkWidget)
(gtk-import-function nil gtk_widget_unlock_accelerators GtkWidget)
(gtk-import-function gboolean gtk_widget_accelerators_locked GtkWidget)
(gtk-import-function gint gtk_widget_event GtkWidget GdkEvent)
(gtk-import-function gboolean gtk_widget_activate GtkWidget)
(gtk-import-function gboolean gtk_widget_set_scroll_adjustments GtkWidget GtkAdjustment GtkAdjustment)
(gtk-import-function nil gtk_widget_reparent GtkWidget GtkWidget)
(gtk-import-function nil gtk_widget_popup GtkWidget gint gint)
(gtk-import-function nil gtk_widget_grab_focus GtkWidget)
(gtk-import-function nil gtk_widget_grab_default GtkWidget)
(gtk-import-function nil gtk_widget_set_name GtkWidget GtkString)
(gtk-import-function GtkString gtk_widget_get_name GtkWidget)
(gtk-import-function nil gtk_widget_set_state GtkWidget GtkStateType)
(gtk-import-function nil gtk_widget_set_sensitive GtkWidget gboolean)
(gtk-import-function nil gtk_widget_set_app_paintable GtkWidget gboolean)
(gtk-import-function nil gtk_widget_set_parent GtkWidget GtkWidget)
(gtk-import-function nil gtk_widget_set_parent_window GtkWindow GdkWindow)
(gtk-import-function GdkWindow gtk_widget_get_parent_window GtkWidget)
(gtk-import-function nil gtk_widget_set_uposition GtkWidget gint gint)
(gtk-import-function nil gtk_widget_set_usize GtkWidget gint gint)
(gtk-import-function nil gtk_widget_set_events GtkWidget GdkEventMask)
(gtk-import-function nil gtk_widget_add_events GtkWidget GdkEventMask)
(gtk-import-function nil gtk_widget_set_extension_events GtkWidget GdkExtensionMode)
(gtk-import-function GdkExtensionMode gtk_widget_get_extension_events GtkWidget)
(gtk-import-function GtkWidget gtk_widget_get_toplevel GtkWidget)
(gtk-import-function GtkWidget gtk_widget_get_ancestor GtkWidget guint)
(gtk-import-function GdkColormap gtk_widget_get_colormap GtkWidget)
(gtk-import-function GdkVisual gtk_widget_get_visual GtkWidget)

(gtk-import-function nil gtk_widget_set_colormap GtkWidget GdkColormap)
(gtk-import-function nil gtk_widget_set_visual GtkWidget GdkVisual)
(gtk-import-function GdkEventMask gtk_widget_get_events GtkWidget)

;;; Hrm - this should return a cons cell.
;;; Handcoded in ui-byhand.c... #### FIXME
;;void	     gtk_widget_get_pointer	(GtkWidget	*widget,
;;					 gint		*x,
;;					 gint		*y);

(gtk-import-function gboolean gtk_widget_is_ancestor GtkWidget GtkWidget)
(gtk-import-function gboolean gtk_widget_hide_on_delete GtkWidget)

;;; Widget styles
(gtk-import-function nil gtk_widget_set_style GtkWidget GtkStyle)
(gtk-import-function nil gtk_widget_set_rc_style GtkWidget)
(gtk-import-function nil gtk_widget_ensure_style GtkWidget)
(gtk-import-function GtkStyle gtk_widget_get_style GtkWidget)
(gtk-import-function nil gtk_widget_restore_default_style GtkWidget)
(gtk-import-function nil gtk_widget_modify_style GtkWidget GtkStyle)

(gtk-import-function nil gtk_widget_set_composite_name GtkWidget GtkString)
(gtk-import-function GtkString gtk_widget_get_composite_name GtkWidget)
(gtk-import-function nil gtk_widget_reset_rc_styles GtkWidget)

;; Push/pop pairs, to change default values upon a widget's creation.
;; This will override the values that got set by the
;; gtk_widget_set_default_* () functions.
(gtk-import-function nil gtk_widget_push_style GtkStyle)
(gtk-import-function nil gtk_widget_push_colormap GdkColormap)
(gtk-import-function nil gtk_widget_push_visual GdkVisual)
(gtk-import-function nil gtk_widget_push_composite_child)
(gtk-import-function nil gtk_widget_pop_composite_child)
(gtk-import-function nil gtk_widget_pop_style)
(gtk-import-function nil gtk_widget_pop_colormap)
(gtk-import-function nil gtk_widget_pop_visual)

;; Set certain default values to be used at widget creation time.
(gtk-import-function nil gtk_widget_set_default_style GtkStyle)
(gtk-import-function nil gtk_widget_set_default_colormap GdkColormap)
(gtk-import-function nil gtk_widget_set_default_visual GdkVisual)
(gtk-import-function GtkStyle gtk_widget_get_default_style)
(gtk-import-function GdkColormap gtk_widget_get_default_colormap)
(gtk-import-function GdkVisual gtk_widget_get_default_visual)

;; Counterpart to gdk_window_shape_combine_mask.
(gtk-import-function nil gtk_widget_shape_combine_mask GtkWidget GdkBitmap gint gint)

;; internal function
(gtk-import-function nil gtk_widget_reset_shapes GtkWidget)

;; Compute a widget's path in the form "GtkWindow.MyLabel", and
;; return newly alocated strings.
;; Ignored for now #### BILL!!!
;void	     gtk_widget_path		   (GtkWidget *widget,
;					    guint     *path_length,
;					    gchar    **path,
;					    gchar    **path_reversed);
;void	     gtk_widget_class_path	   (GtkWidget *widget,
;					    guint     *path_length,
;					    gchar    **path,
;					    gchar    **path_reversed);


(gtk-import-function GtkType gtk_window_get_type)
(gtk-import-function GtkWidget gtk_window_new GtkWindowType)
(gtk-import-function nil gtk_window_set_title GtkWindow GtkString)
(gtk-import-function nil gtk_window_set_wmclass GtkWindow GtkString GtkString)
(gtk-import-function nil gtk_window_set_policy GtkWindow gint gint gint)
(gtk-import-function nil gtk_window_add_accel_group GtkWindow GtkAccelGroup)
(gtk-import-function nil gtk_window_remove_accel_group GtkWindow GtkAccelGroup)
(gtk-import-function nil gtk_window_set_position GtkWindow GtkWindowPosition)
(gtk-import-function gint gtk_window_activate_focus GtkWindow)
(gtk-import-function gint gtk_window_activate_default GtkWindow)
(gtk-import-function nil gtk_window_set_transient_for GtkWindow GtkWindow)
;(gtk-import-function nil gtk_window_set_geometry_hints GtkWindow GtkWidget GdkGeometry GdkWindowHints)
(gtk-import-function nil gtk_window_set_default_size GtkWindow gint gint)
(gtk-import-function nil gtk_window_set_modal GtkWindow gboolean)

;; Internal functions - do we really want to expose these?
;; NO
'(gtk-import-function nil gtk_window_set_focus GtkWindow GtkWidget)
'(gtk-import-function nil gtk_window_set_default GtkWindow GtkWidget)
'(gtk-import-function nil gtk_window_remove_embedded_xid GtkWindow guint)
'(gtk-import-function nil gtk_window_add_embedded_xid GtkWindow guint)
'(gtk-import-function nil gtk_window_reposition GtkWindow gint gint)


(gtk-import-function GtkType gtk_spin_button_get_type)
(gtk-import-function nil gtk_spin_button_configure
		     (GtkSpinButton . spin_button)
		     (GtkAdjustment . adjustment)
		     (gfloat        . climb_rate)
		     (guint         . digits))
(gtk-import-function GtkWidget gtk_spin_button_new
		     (GtkAdjustment . adjustment)
		     (gfloat        . climb_rate)
		     (guint         . digits))
(gtk-import-function nil gtk_spin_button_set_adjustment
		     (GtkSpinButton . spin_button)
		     (GtkAdjustment . adjustment))
(gtk-import-function GtkAdjustment gtk_spin_button_get_adjustment
		     (GtkSpinButton . spin_button))
(gtk-import-function nil gtk_spin_button_set_digits
		     (GtkSpinButton . spin_button)
		     (guint         . digits))
(gtk-import-function gfloat gtk_spin_button_get_value_as_float
		     (GtkSpinButton . spin_button))
(gtk-import-function gint gtk_spin_button_get_value_as_int
		     (GtkSpinButton . spin_button))
(gtk-import-function nil gtk_spin_button_set_value
		     (GtkSpinButton . spin_button)
		     (gfloat        . value))
(gtk-import-function nil gtk_spin_button_set_update_policy
		     (GtkSpinButton . spin_button)
		     (GtkSpinButtonUpdatePolicy . policy))
(gtk-import-function nil gtk_spin_button_set_numeric
		     (GtkSpinButton . spin_button)
		     (gboolean      . numeric))
(gtk-import-function nil gtk_spin_button_spin
		     (GtkSpinButton . spin_button)
		     (GtkSpinType   . direction)
		     (gfloat        . increment))
(gtk-import-function nil gtk_spin_button_set_wrap
		     (GtkSpinButton . spin_button)
		     (gboolean      . wrap))
(gtk-import-function nil gtk_spin_button_set_shadow_type
		     (GtkSpinButton . spin_button)
		     (GtkShadowType . shadow_type))
(gtk-import-function nil gtk_spin_button_set_snap_to_ticks
		     (GtkSpinButton . spin_button)
		     (gboolean      . snap_to_ticks))
(gtk-import-function  nil gtk_spin_button_update
		      (GtkSpinButton . spin_button))


(gtk-import-function GtkType gtk_statusbar_get_type)
(gtk-import-function GtkWidget gtk_statusbar_new)
(gtk-import-function guint gtk_statusbar_get_context_id
		     (GtkStatusbar . statusbar)
		     (GtkString    . context_description))

;; Returns message_id used for gtk_statusbar_remove
(gtk-import-function guint gtk_statusbar_push
		     (GtkStatusbar . statusbar)
		     (guint        . context_id)
		     (GtkString    . text))
(gtk-import-function nil gtk_statusbar_pop
		     (GtkStatusbar . statusbar)
		     (guint        . context_id))
(gtk-import-function nil gtk_statusbar_remove
		     (GtkStatusbar . statusbar)
		     (guint        . context_id)
		     (guint        . message_id))


(gtk-import-function GtkType gtk_ctree_get_type)
(gtk-import-function none gtk_ctree_construct
		     (GtkCTree . ctree)
		     (gint     . columns)
		     (gint     . tree_column)
		     (GtkArrayOfString . titles))
(gtk-import-function GtkWidget gtk_ctree_new_with_titles
		     (gint . columns)
		     (gint . tree_column)
		     (GtkArrayOfString . titles))
(gtk-import-function GtkWidget gtk_ctree_new
		     (gint . columns)
		     (gint . tree_column))

(gtk-import-function GtkCTreeNode gtk_ctree_insert_node
		     (GtkCTree . ctree)
		     (GtkCTreeNode . parent)
		     (GtkCTreeNode . sibling)
		     (GtkArrayOfString . text)
		     (guint . spacing)
		     (GdkPixmap . pixmap_closed)
		     (GdkBitmap . mask_closed)
		     (GdkPixmap . pixmap_opened)
		     (GdkBitmap . mask_opened)
		     (gboolean . is_leaf)
		     (gboolean . expanded))

(gtk-import-function none gtk_ctree_remove_node
		     (GtkCTree . ctree)
		     (GtkCTreeNode . node))

(gtk-import-function none gtk_ctree_expand
		     (GtkCTree . ctree)
		     (GtkCTreeNode . node))

(gtk-import-function none gtk_ctree_move
		     (GtkCTree  . ctree)
		     (GtkCTreeNode . node)
		     (GtkCTreeNode . new_parent)
		     (GtkCTreeNode . new_sibling))

(gtk-import-function void gtk_ctree_expand_recursive
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node))

(gtk-import-function void gtk_ctree_expand_to_depth
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node)
		     (gint         . depth))

(gtk-import-function void gtk_ctree_collapse
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node))

(gtk-import-function void gtk_ctree_collapse_recursive
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node))

(gtk-import-function void gtk_ctree_collapse_to_depth
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node)
		     (gint         . depth))

(gtk-import-function void gtk_ctree_toggle_expansion
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node))

(gtk-import-function void gtk_ctree_toggle_expansion_recursive
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node))

(gtk-import-function void gtk_ctree_select
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node))

(gtk-import-function void gtk_ctree_select_recursive
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node))

(gtk-import-function void gtk_ctree_unselect
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node))

(gtk-import-function void gtk_ctree_unselect_recursive
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node))

;; NOTE!!! The header file here was WRONG!  It had a third arg 'gint state'
(gtk-import-function void gtk_ctree_real_select_recursive
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node))

;; Analogs of GtkCList functions
(gtk-import-function void gtk_ctree_node_set_text
                     (GtkCTree . ctree)
		     (GtkCTreeNode . node)
		     (gint . column)
		     (GtkString . text))

(gtk-import-function void gtk_ctree_node_set_pixmap
		     (GtkCTree . ctree)
		     (GtkCTreeNode . node)
		     (gint . column)
		     (GdkPixmap . pixmap)
		     (GdkBitmap . mask))

(gtk-import-function void gtk_ctree_node_set_pixtext
		     (GtkCTree . ctree)
		     (GtkCTreeNode . node)
		     (gint . column)
		     (GtkString . text)
		     (guint . spacing)
		     (GdkPixmap . pixmap)
		     (GdkBitmap . mask))

(gtk-import-function void gtk_ctree_set_node_info
                     (GtkCTree . ctree)
		     (GtkCTreeNode . node)
		     (GtkString . text)
		     (guint . spacing)
		     (GdkPixmap . pixmap_closed)
		     (GdkBitmap . mask_closed)
		     (GdkPixmap . pixmap_opened)
		     (GdkBitmap . mask_opened)
		     (gboolean  . is_leaf)
		     (gboolean  . expanded))

(gtk-import-function void gtk_ctree_node_set_shift
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node)
		     (gint         . column)
		     (gint         . vertical)
		     (gint         . horizontal))

(gtk-import-function void gtk_ctree_node_set_selectable
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node)
		     (gboolean     . selectable))

(gtk-import-function gboolean gtk_ctree_node_get_selectable
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node))

(gtk-import-function GtkCellType gtk_ctree_node_get_cell_type
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node)
		     (gint         . column))

(gtk-import-function void gtk_ctree_node_set_row_style
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node)
		     (GtkStyle     . style))

(gtk-import-function GtkStyle gtk_ctree_node_get_row_style
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node))

(gtk-import-function void gtk_ctree_node_set_cell_style
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node)
		     (gint         . column)
		     (GtkStyle     . style))

(gtk-import-function GtkStyle gtk_ctree_node_get_cell_style
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node)
		     (gint         . column))

(gtk-import-function void gtk_ctree_node_set_foreground
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node)
		     (GdkColor     . color))

(gtk-import-function void gtk_ctree_node_set_background
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node)
		     (GdkColor     . color))

(gtk-import-function void gtk_ctree_node_moveto
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node)
		     (gint         . column)
		     (gfloat       . row_align)
		     (gfloat       . col_align))

(gtk-import-function GtkVisibility gtk_ctree_node_is_visible
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node))

;; GtkCTree specific functions
(gtk-import-function void gtk_ctree_set_indent
		     (GtkCTree                . ctree)
		     (gint                    . indent))

(gtk-import-function void gtk_ctree_set_spacing
		     (GtkCTree                . ctree)
		     (gint                    . spacing))

(gtk-import-function void gtk_ctree_set_show_stub
		     (GtkCTree                . ctree)
		     (gboolean                . show_stub))

(gtk-import-function void gtk_ctree_set_line_style
		     (GtkCTree                . ctree)
		     (GtkCTreeLineStyle       . line_style))

(gtk-import-function void gtk_ctree_set_expander_style
		     (GtkCTree                . ctree)
		     (GtkCTreeExpanderStyle   . expander_style))

;; Tree sorting functions
(gtk-import-function void gtk_ctree_sort_node
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node))

(gtk-import-function void gtk_ctree_sort_recursive
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node))

;; Finding tree information
(gtk-import-function gboolean gtk_ctree_is_viewable
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node))
(gtk-import-function GtkCTreeNode gtk_ctree_last
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node))
(gtk-import-function GtkCTreeNode gtk_ctree_find_node_ptr
		     (GtkCTree     . ctree)
		     (GtkCTreeRow  . ctree_row))
(gtk-import-function GtkCTreeNode gtk_ctree_node_nth
		     (GtkCTree     . ctree)
		     (guint        . row))
(gtk-import-function gboolean gtk_ctree_find
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node)
		     (GtkCTreeNode . child))
(gtk-import-function gboolean gtk_ctree_is_ancestor
		     (GtkCTree     . ctree)
		     (GtkCTreeNode . node)
		     (GtkCTreeNode . child))
(gtk-import-function gboolean gtk_ctree_is_hot_spot
		     (GtkCTree     . ctree)
		     (gint         . x)
		     (gint         . y))

(defun gtk-ctree-post-recursive (ctree node func data)
  (gtk-ctree-recurse ctree node func data t nil))

(defun gtk-ctree-post-recursive-to-depth (ctree node depth func data)
  (gtk-ctree-recurse ctree node func data t depth))

(defun gtk-ctree-pre-recursive (ctree node func data)
  (gtk-ctree-recurse ctree node func data nil nil))

(defun gtk-ctree-pre-recursive-to-depth (ctree node depth func data)
  (gtk-ctree-recurse ctree node func data nil depth))


(gtk-import-function GtkType gtk_preview_get_type)
(gtk-import-function void gtk_preview_uninit)
(gtk-import-function GtkWidget gtk_preview_new
		     (GtkPreviewType . type))
(gtk-import-function void gtk_preview_size
		     (GtkPreview      . preview)
		     (gint            . width)
		     (gint            . height))
(gtk-import-function void gtk_preview_put
		     (GtkPreview      . preview)
		     (GdkWindow       . window)
		     (GdkGC           . gc)
		     (gint            . srcx)
		     (gint            . srcy)
		     (gint            . destx)
		     (gint            . desty)
		     (gint            . width)
		     (gint            . height))
(gtk-import-function void gtk_preview_draw_row
		     (GtkPreview      . preview)
		     (GtkString       . data)
		     (gint            . x)
		     (gint            . y)
		     (gint            . w))
(gtk-import-function void gtk_preview_set_expand
		     (GtkPreview      . preview)
		     (gboolean        . expand))
(gtk-import-function void gtk_preview_set_gamma
		     (double          . gamma))
(gtk-import-function void gtk_preview_set_color_cube
		     (guint           . nred_shades)
		     (guint           . ngreen_shades)
		     (guint           . nblue_shades)
		     (guint           . ngray_shades))
(gtk-import-function void gtk_preview_set_install_cmap
		     (gboolean        . install_cmap))
(gtk-import-function void gtk_preview_set_reserved
		     (gint            . nreserved))
;;;(gtk-import-function void gtk_preview_set_dither
;;;		     (GtkPreview      . preview)
;;;		     (GdkRgbDither    . dither))

(gtk-import-function GdkVisual gtk_preview_get_visual)
(gtk-import-function GdkColormap gtk_preview_get_cmap)
(gtk-import-function GtkPreviewInfo gtk_preview_get_info)

;; This function reinitializes the preview colormap and visual from
;; the current gamma/color_cube/install_cmap settings. It must only
;; be called if there are no previews or users's of the preview
;; colormap in existence.
(gtk-import-function void gtk_preview_reset)
