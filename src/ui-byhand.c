/* I really wish this entire file could go away, but there is
   currently no way to do the following in the Foreign Function
   Interface:

   1) Deal with return values in the parameter list (ie: int *foo)

   So we have to code a few functions by hand.  Ick.

   William M. Perry 5/8/00
*/

#include "gui.h"

DEFUN ("gtk-box-query-child-packing", Fgtk_box_query_child_packing, 2, 2,0, /*
Returns information about how CHILD is packed into BOX.
Return value is a list of (EXPAND FILL PADDING PACK_TYPE).
*/
       (box, child))
{
  gboolean expand, fill;
  guint padding;
  GtkPackType pack_type;
  Lisp_Object result = Qnil;
  
  CHECK_GTK_OBJECT (box);
  CHECK_GTK_OBJECT (child);

  if (!GTK_IS_BOX (XGTK_OBJECT (box)->object))
    {
      signal_simple_error ("Object is not a GtkBox", box);
    }

  if (!GTK_IS_WIDGET (XGTK_OBJECT (child)->object))
    {
      signal_simple_error ("Child is not a GtkWidget", child);
    }

  gtk_box_query_child_packing (GTK_BOX (XGTK_OBJECT (box)->object),
			       GTK_WIDGET (XGTK_OBJECT (child)->object),
			       &expand, &fill, &padding, &pack_type);

  result = Fcons (make_int (pack_type), result);
  result = Fcons (make_int (padding), result);
  result = Fcons (fill ? Qt : Qnil, result);
  result = Fcons (expand ? Qt : Qnil, result);

  return (result);
}

/* void gtk_button_box_get_child_size_default (gint *min_width, gint *min_height); */
DEFUN ("gtk-button-box-get-child-size-default",
       Fgtk_button_box_get_child_size_default, 0, 0, 0, /*
Return a cons cell (WIDTH . HEIGHT) of the default button box child size.
*/
       ())
{
  gint width, height;

  gtk_button_box_get_child_size_default (&width, &height);

  return (Fcons (make_int (width), make_int (height)));
}

/* void gtk_button_box_get_child_ipadding_default (gint *ipad_x, gint *ipad_y);  */
DEFUN ("gtk-button-box-get-child-ipadding-default",
       Fgtk_button_box_get_child_ipadding_default, 0, 0, 0, /*
Return a cons cell (X . Y) of the default button box ipadding.
*/
       ())
{
  gint x, y;

  gtk_button_box_get_child_ipadding_default (&x, &y);

  return (Fcons (make_int (x), make_int (y)));
}

/* void gtk_button_box_get_child_size (GtkButtonBox *widget,
   gint *min_width, gint *min_height); */
DEFUN ("gtk-button-box-get-child-size", Fgtk_button_box_get_child_size, 1, 1, 0, /*
Get the current size of a child in the buttonbox BOX.
*/
       (box))
{
  gint width, height;

  CHECK_GTK_OBJECT (box);

  if (!GTK_IS_BUTTON_BOX (XGTK_OBJECT (box)->object))
    {
      signal_simple_error ("Not a GtkBox object", box);
    }

  gtk_button_box_get_child_size (GTK_BUTTON_BOX (XGTK_OBJECT (box)->object),
				 &width, &height);

  return (Fcons (make_int (width), make_int (height)));
}

/* void gtk_button_box_get_child_ipadding (GtkButtonBox *widget, gint *ipad_x, gint *ipad_y); */
DEFUN ("gtk-button-box-get-child-ipadding",
       Fgtk_button_box_get_child_ipadding, 1, 1, 0, /*
Return a cons cell (X . Y) of the current buttonbox BOX ipadding.
*/
       (box))
{
  gint x, y;

  CHECK_GTK_OBJECT (box);

  if (!GTK_IS_BUTTON_BOX (XGTK_OBJECT (box)->object))
    {
      signal_simple_error ("Not a GtkBox object", box);
    }

  gtk_button_box_get_child_ipadding (GTK_BUTTON_BOX (XGTK_OBJECT (box)->object),
				     &x, &y);

  return (Fcons (make_int (x), make_int (y)));
}

/*void	   gtk_calendar_get_date	(GtkCalendar *calendar, 
					 guint	     *year,
					 guint	     *month,
					 guint	     *day);
*/
DEFUN ("gtk-calendar-get-date", Fgtk_calendar_get_date, 1, 1, 0, /*
Return a list of (YEAR MONTH DAY) from the CALENDAR object.
*/
       (calendar))
{
  guint year, month, day;

  CHECK_GTK_OBJECT (calendar);

  if (!GTK_IS_CALENDAR (XGTK_OBJECT (calendar)->object))
    {
      signal_simple_error ("Not a GtkCalendar object", calendar);
    }

  gtk_calendar_get_date (GTK_CALENDAR (XGTK_OBJECT (calendar)->object),
			 &year, &month, &day);

  return (list3 (make_int (year), make_int (month), make_int (day)));
}

/* gint gtk_clist_get_text (GtkCList  *clist,
			 gint       row,
			 gint       column,
			 gchar    **text);
*/
DEFUN ("gtk-clist-get-text", Fgtk_clist_get_text, 3, 3, 0, /*
Returns the text from GtkCList OBJ cell at coordinates ROW, COLUMN.
*/
       (obj, row, column))
{
  gchar *text = NULL;
  Lisp_Object rval = Qnil;

  CHECK_GTK_OBJECT (obj);
  CHECK_INT (row);
  CHECK_INT (column);

  if (!GTK_IS_CLIST (XGTK_OBJECT (obj)->object))
    {
      signal_simple_error ("Object is not a GtkCList", obj);
    }

  gtk_clist_get_text (GTK_CLIST (XGTK_OBJECT (obj)->object), XINT (row), XINT (column), &text);

  if (text)
    {
      rval = build_string (text);
      /* NOTE: This is NOT a memory leak.  GtkCList returns a pointer
	 to internally used memory, not a copy of it.
	 g_free (text);
      */
    }

  return (rval);
}

/* gint gtk_clist_get_selection_info (GtkCList *clist,
			     	   gint      x,
			     	   gint      y,
			     	   gint     *row,
			     	   gint *column); */
DEFUN ("gtk-clist-get-selection-info", Fgtk_clist_get_selection, 3, 3, 0, /*
Returns a cons cell of (ROW . COLUMN) of the GtkCList OBJ at coordinates X, Y.
*/
       (obj, x, y))
{
  gint row, column;

  CHECK_GTK_OBJECT (obj);
  CHECK_INT (x);
  CHECK_INT (y);

  if (!GTK_IS_CLIST (XGTK_OBJECT (obj)->object))
    {
      signal_simple_error ("Object is not a GtkCList", obj);
    }

  gtk_clist_get_selection_info (GTK_CLIST (XGTK_OBJECT (obj)->object),
				XINT (x), XINT (y), &row, &column);

  return (Fcons (make_int (row), make_int (column)));
}

DEFUN ("gtk-clist-get-pixmap", Fgtk_clist_get_pixmap, 3, 3, 0, /*
Return a cons of (pixmap . mask) at ROW,COLUMN in CLIST.
*/
       (clist, row, column))
{
  GdkPixmap *pixmap = NULL;
  GdkBitmap *mask = NULL;

  CHECK_GTK_OBJECT (clist);
  CHECK_INT (row);
  CHECK_INT (column);

  if (!GTK_IS_CLIST (XGTK_OBJECT (clist)->object))
    {
      signal_simple_error ("Object is not a GtkCList", clist);
    }

  gtk_clist_get_pixmap (GTK_CLIST (XGTK_OBJECT (clist)->object),
			XINT (row), XINT (column),
			&pixmap, &mask);

  return (Fcons (pixmap ? build_gtk_boxed (pixmap, GTK_TYPE_GDK_WINDOW) : Qnil,
		 mask ? build_gtk_boxed (mask, GTK_TYPE_GDK_WINDOW) : Qnil));
}

DEFUN ("gtk-clist-get-pixtext", Fgtk_clist_get_pixtext, 3, 3, 0, /*
Return a list of (pixmap mask text) at ROW,COLUMN in CLIST.
*/
       (clist, row, column))
{
  GdkPixmap *pixmap = NULL;
  GdkBitmap *mask = NULL;
  char *text = NULL;
  guint8 spacing;

  CHECK_GTK_OBJECT (clist);
  CHECK_INT (row);
  CHECK_INT (column);

  if (!GTK_IS_CLIST (XGTK_OBJECT (clist)->object))
    {
      signal_simple_error ("Object is not a GtkCList", clist);
    }

  gtk_clist_get_pixtext (GTK_CLIST (XGTK_OBJECT (clist)->object),
			 XINT (row), XINT (column), &text, &spacing,
			 &pixmap, &mask);

  return (list3 (pixmap ? build_gtk_boxed (pixmap, GTK_TYPE_GDK_WINDOW) : Qnil,
		 mask ? build_gtk_boxed (mask, GTK_TYPE_GDK_WINDOW) : Qnil,
		 (text && text[0]) ? build_string (text) : Qnil));
}

/* void gtk_color_selection_get_color(GtkColorSelection *colorsel, gdouble *color); */
DEFUN ("gtk-color-selection-get-color", Fgtk_color_selection_get_color, 1, 1, 0, /*
Return a list of (RED GREEN BLUE ALPHA) from the GtkColorSelection OBJECT.
*/
       (object))
{
  gdouble rgba[4];

  CHECK_GTK_OBJECT (object);

  if (!GTK_IS_COLOR_SELECTION (XGTK_OBJECT (object)->object))
    {
      signal_simple_error ("Object is not a GtkColorSelection", object);
    }

  gtk_color_selection_get_color (GTK_COLOR_SELECTION (XGTK_OBJECT (object)), rgba);

  return (list4 (make_float (rgba[0]),
		 make_float (rgba[1]),
		 make_float (rgba[2]),
		 make_float (rgba[3])));
}

/* (gtk-import-function nil "gtk_editable_insert_text" 'GtkEditable 'GtkString 'gint 'pointer-to-gint) */
DEFUN ("gtk-editable-insert-text", Fgtk_editable_insert_text, 3, 3, 0, /*
Insert text STRINT at POS in GtkEditable widget OBJ.
Returns the new position of the cursor in the widget.
*/
       (obj, string, pos))
{
  gint the_pos;

  CHECK_GTK_OBJECT (obj);
  CHECK_STRING (string);
  CHECK_INT (pos);

  the_pos = XINT (pos);

  if (!GTK_IS_EDITABLE (XGTK_OBJECT (obj)->object))
    {
      signal_simple_error ("Object is not a GtkEditable", obj);
    }

  gtk_editable_insert_text (GTK_EDITABLE (XGTK_OBJECT (obj)->object),
			    (char *) XSTRING_DATA (string),
			    XSTRING_LENGTH (string),
			    &the_pos);

  return (make_int (the_pos));
}

DEFUN ("gtk-pixmap-get", Fgtk_pixmap_get, 1, 1, 0, /*
Return a cons cell of (PIXMAP . MASK) from GtkPixmap OBJECT.
*/
	 (object))
{
  GdkPixmap *pixmap, *mask;

  CHECK_GTK_OBJECT (object);

  if (!GTK_IS_PIXMAP (XGTK_OBJECT (object)->object))
    {
      signal_simple_error ("Object is not a GtkPixmap", object);
    }

  gtk_pixmap_get (GTK_PIXMAP (XGTK_OBJECT (object)->object), &pixmap, &mask);

  return (Fcons (pixmap ? build_gtk_object (GTK_OBJECT (pixmap)) : Qnil,
		 mask ? build_gtk_object (GTK_OBJECT (mask)) : Qnil));
}

DEFUN ("gtk-curve-get-vector", Fgtk_curve_get_vector, 2, 2, 0, /*
Returns a vector of LENGTH points representing the curve of CURVE.
*/
       (curve, length))
{
  gfloat *vector = NULL;
  Lisp_Object lisp_vector = Qnil;
  int i;

  CHECK_GTK_OBJECT (curve);
  CHECK_INT (length);

  if (!GTK_IS_CURVE (XGTK_OBJECT (curve)->object))
    {
      signal_simple_error ("Object is not a GtkCurve", curve);
    }

  vector = (gfloat *) alloca (sizeof (gfloat) * XINT (length));

  gtk_curve_get_vector (GTK_CURVE (XGTK_OBJECT (curve)->object), XINT (length), vector);
  lisp_vector = make_vector (XINT (length), Qnil);

  for (i = 0; i < XINT (length); i++)
    {
      XVECTOR_DATA (lisp_vector)[i] = make_float (vector[i]);
    }

  return (lisp_vector);
}

DEFUN ("gtk-curve-set-vector", Fgtk_curve_set_vector, 2, 2, 0, /*
Set the vector of points on CURVE to VECTOR.
*/
       (curve, vector))
{
  gfloat *c_vector = NULL;
  int vec_length = 0;
  int i;

  CHECK_GTK_OBJECT (curve);
  CHECK_VECTOR (vector);

  vec_length = XVECTOR_LENGTH (vector);

  if (!GTK_IS_CURVE (XGTK_OBJECT (curve)->object))
    {
      signal_simple_error ("Object is not a GtkCurve", curve);
    }

  c_vector = (gfloat *) alloca (sizeof (gfloat) * vec_length);

  for (i = 0; i < vec_length; i++)
    {
      CHECK_FLOAT (XVECTOR_DATA (vector)[i]);
      c_vector[i] = extract_float (XVECTOR_DATA (vector)[i]);
    }

  gtk_curve_set_vector (GTK_CURVE (XGTK_OBJECT (curve)->object), vec_length, c_vector);
  return (Qt);
}

DEFUN ("gtk-label-get", Fgtk_label_get, 1, 1, 0, /*
Return the text of LABEL.
*/
       (label))
{
  gchar *string;

  CHECK_GTK_OBJECT (label);

  if (!GTK_IS_LABEL (XGTK_OBJECT (label)->object))
    {
      signal_simple_error ("Object is not a GtkLabel", label);
    }

  gtk_label_get (GTK_LABEL (XGTK_OBJECT (label)->object), &string);

  return (build_string (string));
}

DEFUN ("gtk-notebook-query-tab-label-packing", Fgtk_notebook_query_tab_label_packing, 2, 2, 0, /*
Return a list of packing information (EXPAND FILL PACK_TYPE) for CHILD in NOTEBOOK.
*/
       (notebook, child))
{
  gboolean expand, fill;
  GtkPackType pack_type;

  CHECK_GTK_OBJECT (notebook);
  CHECK_GTK_OBJECT (child);

  if (!GTK_IS_NOTEBOOK (XGTK_OBJECT (notebook)->object))
    {
      signal_simple_error ("Object is not a GtkLabel", notebook);
    }

  if (!GTK_IS_WIDGET (XGTK_OBJECT (child)->object))
    {
      signal_simple_error ("Object is not a GtkWidget", child);
    }

  gtk_notebook_query_tab_label_packing (GTK_NOTEBOOK (XGTK_OBJECT (notebook)->object),
					GTK_WIDGET (XGTK_OBJECT (child)->object),
					&expand, &fill, &pack_type);

  return (list3 (expand ? Qt : Qnil, fill ? Qt : Qnil, make_int (pack_type)));
}

DEFUN ("gtk-widget-get-pointer", Fgtk_widget_get_pointer, 1, 1, 0, /*
Return the pointer position relative to WIDGET as a cons of (X . Y).
*/
       (widget))
{
  gint x,y;
  CHECK_GTK_OBJECT (widget);

  if (!GTK_IS_WIDGET (XGTK_OBJECT (widget)->object))
    {
      signal_simple_error ("Object is not a GtkWidget", widget);
    }

  gtk_widget_get_pointer (GTK_WIDGET (XGTK_OBJECT (widget)->object), &x, &y);

  return (Fcons (make_int (x), make_int (y)));
}

/* This is called whenever an item with a GUI_ID associated with it is
   destroyed.  This allows us to remove the references in gui-gtk.c
   that made sure callbacks and such were GCPRO-ed
*/
static void
__remove_gcpro_by_id (gpointer user_data)
{
  ungcpro_popup_callbacks ((GUI_ID) user_data);
}

static void
__generic_toolbar_callback (GtkWidget *item, gpointer user_data)
{
  Lisp_Object callback;
  Lisp_Object lisp_user_data;

  VOID_TO_LISP (callback, user_data);

  lisp_user_data = XCAR (callback);
  callback = XCDR (callback);

  signal_special_gtk_user_event (Qnil, callback, lisp_user_data);
}

static Lisp_Object
generic_toolbar_insert_item (Lisp_Object toolbar,
			     Lisp_Object text,
			     Lisp_Object tooltip_text,
			     Lisp_Object tooltip_private_text,
			     Lisp_Object icon,
			     Lisp_Object callback,
			     Lisp_Object data,
			     Lisp_Object prepend_p,
			     Lisp_Object position)
{
  GUI_ID id;
  GtkWidget *w = NULL;

  CHECK_GTK_OBJECT (toolbar);
  CHECK_GTK_OBJECT (icon);
  CHECK_STRING (text);
  CHECK_STRING (tooltip_text);
  CHECK_STRING (tooltip_private_text);

  if (!SYMBOLP (callback) && !LISTP (callback))
    {
      signal_simple_error ("Callback must be symbol or eval-able form", callback);
    }

  if (!GTK_IS_TOOLBAR (XGTK_OBJECT (toolbar)->object))
    {
      signal_simple_error ("Object is not a GtkToolbar", toolbar);
    }

  if (!GTK_IS_WIDGET (XGTK_OBJECT (icon)->object))
    {
      signal_simple_error ("Object is not a GtkWidget", icon);
    }

  callback = Fcons (data, callback);

  id = new_gui_id ();
  gcpro_popup_callbacks (id, callback);
  gtk_object_weakref (XGTK_OBJECT (toolbar)->object, __remove_gcpro_by_id,
		      (gpointer) id);

  if (NILP (position))
    {
      w = (NILP (prepend_p) ? gtk_toolbar_append_item : gtk_toolbar_prepend_item)
	(GTK_TOOLBAR (XGTK_OBJECT (toolbar)->object),
	 XSTRING_DATA (text),
	 XSTRING_DATA (tooltip_text),
	 XSTRING_DATA (tooltip_private_text),
	 GTK_WIDGET (XGTK_OBJECT (icon)->object),
	 GTK_SIGNAL_FUNC (__generic_toolbar_callback),
	 LISP_TO_VOID (callback));
    }
  else
    {
      w = gtk_toolbar_insert_item (GTK_TOOLBAR (XGTK_OBJECT (toolbar)->object),
				   XSTRING_DATA (text),
				   XSTRING_DATA (tooltip_text),
				   XSTRING_DATA (tooltip_private_text),
				   GTK_WIDGET (XGTK_OBJECT (icon)->object),
				   GTK_SIGNAL_FUNC (__generic_toolbar_callback),
				   LISP_TO_VOID (callback),
				   XINT (position));
    }


  return (w ? build_gtk_object (GTK_OBJECT (w)) : Qnil);
}

DEFUN ("gtk-toolbar-append-item", Fgtk_toolbar_append_item, 6, 7, 0, /*
Appends a new button to the given toolbar.
*/
	   (toolbar, text, tooltip_text, tooltip_private_text, icon, callback, data))
{
  return (generic_toolbar_insert_item (toolbar,text,tooltip_text,tooltip_private_text,icon,callback,data,Qnil,Qnil));
}

DEFUN ("gtk-toolbar-prepend-item", Fgtk_toolbar_prepend_item, 6, 7, 0, /*
Adds a new button to the beginning (left or top edges) of the given toolbar.
*/
	   (toolbar, text, tooltip_text, tooltip_private_text, icon, callback, data))
{
  return (generic_toolbar_insert_item (toolbar,text,tooltip_text,tooltip_private_text,icon,callback,data,Qt,Qnil));
}

DEFUN ("gtk-toolbar-insert-item", Fgtk_toolbar_insert_item, 7, 8, 0, /*
Adds a new button to the beginning (left or top edges) of the given toolbar.
*/
	   (toolbar, text, tooltip_text, tooltip_private_text, icon, callback, position, data))
{
  CHECK_INT (position);

  return (generic_toolbar_insert_item (toolbar,text,tooltip_text,tooltip_private_text,icon,callback,data,Qnil,position));
}

/* GtkCTree is an abomination in the eyes of the object system. */
static void
__emacs_gtk_ctree_recurse_internal (GtkCTree *ctree, GtkCTreeNode *node, gpointer user_data)
{
  Lisp_Object closure;

  VOID_TO_LISP (closure, user_data);

  call3 (XCAR (closure),
	 build_gtk_object (GTK_OBJECT (ctree)),
	 build_gtk_boxed (node, GTK_TYPE_CTREE_NODE),
	 XCDR (closure));
}

DEFUN ("gtk-ctree-recurse", Fgtk_ctree_recurse, 3, 6, 0, /*
Recursively apply FUNC to all nodes of CTREE at or below NODE.
FUNC is called with three arguments: CTREE, a GtkCTreeNode, and DATA.
The return value of FUNC is ignored.

If optional 5th argument CHILDFIRSTP is non-nil, then
the function is called for each node after it has been
called for that node's children.

Optional 6th argument DEPTH limits how deeply to recurse.

This function encompasses all the following Gtk functions:

void gtk_ctree_post_recursive                    (GtkCTree     *ctree, 
						  GtkCTreeNode *node,
						  GtkCTreeFunc  func,
						  gpointer      data);
void gtk_ctree_post_recursive_to_depth           (GtkCTree     *ctree, 
						  GtkCTreeNode *node,
						  gint          depth,
						  GtkCTreeFunc  func,
						  gpointer      data);
void gtk_ctree_pre_recursive                     (GtkCTree     *ctree, 
						  GtkCTreeNode *node,
						  GtkCTreeFunc  func,
						  gpointer      data);
void gtk_ctree_pre_recursive_to_depth            (GtkCTree     *ctree, 
						  GtkCTreeNode *node,
						  gint          depth,
						  GtkCTreeFunc  func,
						  gpointer      data);
*/
       (ctree, node, func, data, childfirstp, depth))
{
  struct gcpro gcpro1, gcpro2, gcpro3;
  Lisp_Object closure = Qnil;

  CHECK_GTK_OBJECT (ctree);

  if (!NILP (node))
    {
      CHECK_GTK_BOXED (node);
    }

  if (!NILP (depth))
    {
      CHECK_INT (depth);
    }

  closure = Fcons (func, data);

  GCPRO3 (ctree, node, closure);

  if (NILP (depth))
    {
      (NILP (childfirstp) ? gtk_ctree_post_recursive : gtk_ctree_pre_recursive)
	(GTK_CTREE (XGTK_OBJECT (ctree)->object),
	 NILP (node) ? NULL : (GtkCTreeNode *) XGTK_BOXED (node)->object,
	 __emacs_gtk_ctree_recurse_internal,
	 LISP_TO_VOID (closure));
    }
  else
    {
      (NILP (childfirstp) ? gtk_ctree_post_recursive_to_depth : gtk_ctree_pre_recursive_to_depth)
	(GTK_CTREE (XGTK_OBJECT (ctree)->object),
	 NILP (node) ? NULL : (GtkCTreeNode *) XGTK_BOXED (node)->object,
	 XINT (depth),
	 __emacs_gtk_ctree_recurse_internal,
	 LISP_TO_VOID (closure));
    }

  UNGCPRO;
  return (Qnil);
}

void syms_of_ui_byhand (void)
{
  DEFSUBR (Fgtk_toolbar_append_item);
  DEFSUBR (Fgtk_toolbar_insert_item);
  DEFSUBR (Fgtk_toolbar_prepend_item);
  DEFSUBR (Fgtk_box_query_child_packing);
  DEFSUBR (Fgtk_button_box_get_child_size_default);
  DEFSUBR (Fgtk_button_box_get_child_ipadding_default);
  DEFSUBR (Fgtk_button_box_get_child_size);
  DEFSUBR (Fgtk_button_box_get_child_ipadding);
  DEFSUBR (Fgtk_calendar_get_date);
  DEFSUBR (Fgtk_clist_get_text);
  DEFSUBR (Fgtk_clist_get_selection);
  DEFSUBR (Fgtk_clist_get_pixmap);
  DEFSUBR (Fgtk_clist_get_pixtext);
  DEFSUBR (Fgtk_color_selection_get_color);
  DEFSUBR (Fgtk_editable_insert_text);
  DEFSUBR (Fgtk_pixmap_get);
  DEFSUBR (Fgtk_curve_get_vector);
  DEFSUBR (Fgtk_curve_set_vector);
  DEFSUBR (Fgtk_label_get);
  DEFSUBR (Fgtk_notebook_query_tab_label_packing);
  DEFSUBR (Fgtk_widget_get_pointer);
  DEFSUBR (Fgtk_ctree_recurse);
}
