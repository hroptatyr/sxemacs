DEFUN ("gtk-adjustment-lower", Fgtk_adjustment_lower, 1, 1, 0, /*
Access the `lower' slot of OBJ, a GtkAdjustment object.
*/
	(obj))
{
	GtkAdjustment *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_ADJUSTMENT (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkAdjustment", obj);
	};

	the_obj = GTK_ADJUSTMENT (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gfloat");
	GTK_VALUE_FLOAT (arg) = the_obj->lower;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-adjustment-upper", Fgtk_adjustment_upper, 1, 1, 0, /*
Access the `upper' slot of OBJ, a GtkAdjustment object.
*/
	(obj))
{
	GtkAdjustment *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_ADJUSTMENT (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkAdjustment", obj);
	};

	the_obj = GTK_ADJUSTMENT (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gfloat");
	GTK_VALUE_FLOAT (arg) = the_obj->upper;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-adjustment-value", Fgtk_adjustment_value, 1, 1, 0, /*
Access the `value' slot of OBJ, a GtkAdjustment object.
*/
	(obj))
{
	GtkAdjustment *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_ADJUSTMENT (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkAdjustment", obj);
	};

	the_obj = GTK_ADJUSTMENT (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gfloat");
	GTK_VALUE_FLOAT (arg) = the_obj->value;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-adjustment-step-increment", Fgtk_adjustment_step_increment, 1, 1, 0, /*
Access the `step_increment' slot of OBJ, a GtkAdjustment object.
*/
	(obj))
{
	GtkAdjustment *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_ADJUSTMENT (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkAdjustment", obj);
	};

	the_obj = GTK_ADJUSTMENT (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gfloat");
	GTK_VALUE_FLOAT (arg) = the_obj->step_increment;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-adjustment-page-increment", Fgtk_adjustment_page_increment, 1, 1, 0, /*
Access the `page_increment' slot of OBJ, a GtkAdjustment object.
*/
	(obj))
{
	GtkAdjustment *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_ADJUSTMENT (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkAdjustment", obj);
	};

	the_obj = GTK_ADJUSTMENT (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gfloat");
	GTK_VALUE_FLOAT (arg) = the_obj->page_increment;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-adjustment-page-size", Fgtk_adjustment_page_size, 1, 1, 0, /*
Access the `page_size' slot of OBJ, a GtkAdjustment object.
*/
	(obj))
{
	GtkAdjustment *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_ADJUSTMENT (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkAdjustment", obj);
	};

	the_obj = GTK_ADJUSTMENT (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gfloat");
	GTK_VALUE_FLOAT (arg) = the_obj->page_size;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-widget-style", Fgtk_widget_style, 1, 1, 0, /*
Access the `style' slot of OBJ, a GtkWidget object.
*/
	(obj))
{
	GtkWidget *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_WIDGET (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkWidget", obj);
	};

	the_obj = GTK_WIDGET (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkStyle");
	GTK_VALUE_BOXED (arg) = (void *)the_obj->style;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-widget-window", Fgtk_widget_window, 1, 1, 0, /*
Access the `window' slot of OBJ, a GtkWidget object.
*/
	(obj))
{
	GtkWidget *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_WIDGET (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkWidget", obj);
	};

	the_obj = GTK_WIDGET (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GdkWindow");
	GTK_VALUE_BOXED (arg) = (void *)the_obj->window;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-widget-state", Fgtk_widget_state, 1, 1, 0, /*
Access the `state' slot of OBJ, a GtkWidget object.
*/
	(obj))
{
	GtkWidget *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_WIDGET (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkWidget", obj);
	};

	the_obj = GTK_WIDGET (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkStateType");
	GTK_VALUE_ENUM (arg) = the_obj->state;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-widget-name", Fgtk_widget_name, 1, 1, 0, /*
Access the `name' slot of OBJ, a GtkWidget object.
*/
	(obj))
{
	GtkWidget *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_WIDGET (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkWidget", obj);
	};

	the_obj = GTK_WIDGET (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkString");
	GTK_VALUE_STRING (arg) = the_obj->name;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-widget-parent", Fgtk_widget_parent, 1, 1, 0, /*
Access the `parent' slot of OBJ, a GtkWidget object.
*/
	(obj))
{
	GtkWidget *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_WIDGET (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkWidget", obj);
	};

	the_obj = GTK_WIDGET (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->parent);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-button-child", Fgtk_button_child, 1, 1, 0, /*
Access the `child' slot of OBJ, a GtkButton object.
*/
	(obj))
{
	GtkButton *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_BUTTON (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkButton", obj);
	};

	the_obj = GTK_BUTTON (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->child);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-button-in-button", Fgtk_button_in_button, 1, 1, 0, /*
Access the `in_button' slot of OBJ, a GtkButton object.
*/
	(obj))
{
	GtkButton *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_BUTTON (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkButton", obj);
	};

	the_obj = GTK_BUTTON (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gboolean");
	GTK_VALUE_BOOL (arg) = the_obj->in_button;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-button-button-down", Fgtk_button_button_down, 1, 1, 0, /*
Access the `button_down' slot of OBJ, a GtkButton object.
*/
	(obj))
{
	GtkButton *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_BUTTON (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkButton", obj);
	};

	the_obj = GTK_BUTTON (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gboolean");
	GTK_VALUE_BOOL (arg) = the_obj->button_down;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-combo-entry", Fgtk_combo_entry, 1, 1, 0, /*
Access the `entry' slot of OBJ, a GtkCombo object.
*/
	(obj))
{
	GtkCombo *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_COMBO (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkCombo", obj);
	};

	the_obj = GTK_COMBO (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->entry);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-combo-button", Fgtk_combo_button, 1, 1, 0, /*
Access the `button' slot of OBJ, a GtkCombo object.
*/
	(obj))
{
	GtkCombo *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_COMBO (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkCombo", obj);
	};

	the_obj = GTK_COMBO (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->button);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-combo-popup", Fgtk_combo_popup, 1, 1, 0, /*
Access the `popup' slot of OBJ, a GtkCombo object.
*/
	(obj))
{
	GtkCombo *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_COMBO (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkCombo", obj);
	};

	the_obj = GTK_COMBO (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->popup);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-combo-popwin", Fgtk_combo_popwin, 1, 1, 0, /*
Access the `popwin' slot of OBJ, a GtkCombo object.
*/
	(obj))
{
	GtkCombo *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_COMBO (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkCombo", obj);
	};

	the_obj = GTK_COMBO (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->popwin);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-combo-list", Fgtk_combo_list, 1, 1, 0, /*
Access the `list' slot of OBJ, a GtkCombo object.
*/
	(obj))
{
	GtkCombo *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_COMBO (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkCombo", obj);
	};

	the_obj = GTK_COMBO (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->list);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-gamma-curve-table", Fgtk_gamma_curve_table, 1, 1, 0, /*
Access the `table' slot of OBJ, a GtkGammaCurve object.
*/
	(obj))
{
	GtkGammaCurve *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_GAMMA_CURVE (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkGammaCurve", obj);
	};

	the_obj = GTK_GAMMA_CURVE (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->table);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-gamma-curve-curve", Fgtk_gamma_curve_curve, 1, 1, 0, /*
Access the `curve' slot of OBJ, a GtkGammaCurve object.
*/
	(obj))
{
	GtkGammaCurve *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_GAMMA_CURVE (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkGammaCurve", obj);
	};

	the_obj = GTK_GAMMA_CURVE (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->curve);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-gamma-curve-gamma", Fgtk_gamma_curve_gamma, 1, 1, 0, /*
Access the `gamma' slot of OBJ, a GtkGammaCurve object.
*/
	(obj))
{
	GtkGammaCurve *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_GAMMA_CURVE (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkGammaCurve", obj);
	};

	the_obj = GTK_GAMMA_CURVE (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gfloat");
	GTK_VALUE_FLOAT (arg) = the_obj->gamma;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-gamma-curve-gamma-dialog", Fgtk_gamma_curve_gamma_dialog, 1, 1, 0, /*
Access the `gamma_dialog' slot of OBJ, a GtkGammaCurve object.
*/
	(obj))
{
	GtkGammaCurve *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_GAMMA_CURVE (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkGammaCurve", obj);
	};

	the_obj = GTK_GAMMA_CURVE (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->gamma_dialog);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-gamma-curve-gamma-text", Fgtk_gamma_curve_gamma_text, 1, 1, 0, /*
Access the `gamma_text' slot of OBJ, a GtkGammaCurve object.
*/
	(obj))
{
	GtkGammaCurve *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_GAMMA_CURVE (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkGammaCurve", obj);
	};

	the_obj = GTK_GAMMA_CURVE (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->gamma_text);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-check-menu-item-active", Fgtk_check_menu_item_active, 1, 1, 0, /*
Access the `active' slot of OBJ, a GtkCheckMenuItem object.
*/
	(obj))
{
	GtkCheckMenuItem *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_CHECK_MENU_ITEM (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkCheckMenuItem", obj);
	};

	the_obj = GTK_CHECK_MENU_ITEM (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gboolean");
	GTK_VALUE_BOOL (arg) = the_obj->active;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-notebook-tab-pos", Fgtk_notebook_tab_pos, 1, 1, 0, /*
Access the `tab_pos' slot of OBJ, a GtkNotebook object.
*/
	(obj))
{
	GtkNotebook *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_NOTEBOOK (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkNotebook", obj);
	};

	the_obj = GTK_NOTEBOOK (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkPositionType");
	GTK_VALUE_ENUM (arg) = the_obj->tab_pos;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-text-hadj", Fgtk_text_hadj, 1, 1, 0, /*
Access the `hadj' slot of OBJ, a GtkText object.
*/
	(obj))
{
	GtkText *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_TEXT (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkText", obj);
	};

	the_obj = GTK_TEXT (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkAdjustment");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->hadj);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-text-vadj", Fgtk_text_vadj, 1, 1, 0, /*
Access the `vadj' slot of OBJ, a GtkText object.
*/
	(obj))
{
	GtkText *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_TEXT (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkText", obj);
	};

	the_obj = GTK_TEXT (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkAdjustment");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->vadj);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-file-selection-dir-list", Fgtk_file_selection_dir_list, 1, 1, 0, /*
Access the `dir_list' slot of OBJ, a GtkFileSelection object.
*/
	(obj))
{
	GtkFileSelection *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_FILE_SELECTION (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkFileSelection", obj);
	};

	the_obj = GTK_FILE_SELECTION (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->dir_list);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-file-selection-file-list", Fgtk_file_selection_file_list, 1, 1, 0, /*
Access the `file_list' slot of OBJ, a GtkFileSelection object.
*/
	(obj))
{
	GtkFileSelection *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_FILE_SELECTION (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkFileSelection", obj);
	};

	the_obj = GTK_FILE_SELECTION (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->file_list);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-file-selection-selection-entry", Fgtk_file_selection_selection_entry, 1, 1, 0, /*
Access the `selection_entry' slot of OBJ, a GtkFileSelection object.
*/
	(obj))
{
	GtkFileSelection *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_FILE_SELECTION (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkFileSelection", obj);
	};

	the_obj = GTK_FILE_SELECTION (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->selection_entry);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-file-selection-selection-text", Fgtk_file_selection_selection_text, 1, 1, 0, /*
Access the `selection_text' slot of OBJ, a GtkFileSelection object.
*/
	(obj))
{
	GtkFileSelection *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_FILE_SELECTION (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkFileSelection", obj);
	};

	the_obj = GTK_FILE_SELECTION (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->selection_text);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-file-selection-main-vbox", Fgtk_file_selection_main_vbox, 1, 1, 0, /*
Access the `main_vbox' slot of OBJ, a GtkFileSelection object.
*/
	(obj))
{
	GtkFileSelection *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_FILE_SELECTION (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkFileSelection", obj);
	};

	the_obj = GTK_FILE_SELECTION (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->main_vbox);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-file-selection-ok-button", Fgtk_file_selection_ok_button, 1, 1, 0, /*
Access the `ok_button' slot of OBJ, a GtkFileSelection object.
*/
	(obj))
{
	GtkFileSelection *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_FILE_SELECTION (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkFileSelection", obj);
	};

	the_obj = GTK_FILE_SELECTION (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->ok_button);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-file-selection-cancel-button", Fgtk_file_selection_cancel_button, 1, 1, 0, /*
Access the `cancel_button' slot of OBJ, a GtkFileSelection object.
*/
	(obj))
{
	GtkFileSelection *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_FILE_SELECTION (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkFileSelection", obj);
	};

	the_obj = GTK_FILE_SELECTION (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->cancel_button);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-file-selection-help-button", Fgtk_file_selection_help_button, 1, 1, 0, /*
Access the `help_button' slot of OBJ, a GtkFileSelection object.
*/
	(obj))
{
	GtkFileSelection *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_FILE_SELECTION (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkFileSelection", obj);
	};

	the_obj = GTK_FILE_SELECTION (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->help_button);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-file-selection-action-area", Fgtk_file_selection_action_area, 1, 1, 0, /*
Access the `action_area' slot of OBJ, a GtkFileSelection object.
*/
	(obj))
{
	GtkFileSelection *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_FILE_SELECTION (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkFileSelection", obj);
	};

	the_obj = GTK_FILE_SELECTION (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->action_area);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-font-selection-dialog-fontsel", Fgtk_font_selection_dialog_fontsel, 1, 1, 0, /*
Access the `fontsel' slot of OBJ, a GtkFontSelectionDialog object.
*/
	(obj))
{
	GtkFontSelectionDialog *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_FONT_SELECTION_DIALOG (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkFontSelectionDialog", obj);
	};

	the_obj = GTK_FONT_SELECTION_DIALOG (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->fontsel);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-font-selection-dialog-main-vbox", Fgtk_font_selection_dialog_main_vbox, 1, 1, 0, /*
Access the `main_vbox' slot of OBJ, a GtkFontSelectionDialog object.
*/
	(obj))
{
	GtkFontSelectionDialog *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_FONT_SELECTION_DIALOG (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkFontSelectionDialog", obj);
	};

	the_obj = GTK_FONT_SELECTION_DIALOG (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->main_vbox);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-font-selection-dialog-action-area", Fgtk_font_selection_dialog_action_area, 1, 1, 0, /*
Access the `action_area' slot of OBJ, a GtkFontSelectionDialog object.
*/
	(obj))
{
	GtkFontSelectionDialog *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_FONT_SELECTION_DIALOG (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkFontSelectionDialog", obj);
	};

	the_obj = GTK_FONT_SELECTION_DIALOG (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->action_area);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-font-selection-dialog-ok-button", Fgtk_font_selection_dialog_ok_button, 1, 1, 0, /*
Access the `ok_button' slot of OBJ, a GtkFontSelectionDialog object.
*/
	(obj))
{
	GtkFontSelectionDialog *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_FONT_SELECTION_DIALOG (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkFontSelectionDialog", obj);
	};

	the_obj = GTK_FONT_SELECTION_DIALOG (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->ok_button);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-font-selection-dialog-apply-button", Fgtk_font_selection_dialog_apply_button, 1, 1, 0, /*
Access the `apply_button' slot of OBJ, a GtkFontSelectionDialog object.
*/
	(obj))
{
	GtkFontSelectionDialog *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_FONT_SELECTION_DIALOG (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkFontSelectionDialog", obj);
	};

	the_obj = GTK_FONT_SELECTION_DIALOG (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->apply_button);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-font-selection-dialog-cancel-button", Fgtk_font_selection_dialog_cancel_button, 1, 1, 0, /*
Access the `cancel_button' slot of OBJ, a GtkFontSelectionDialog object.
*/
	(obj))
{
	GtkFontSelectionDialog *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_FONT_SELECTION_DIALOG (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkFontSelectionDialog", obj);
	};

	the_obj = GTK_FONT_SELECTION_DIALOG (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->cancel_button);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-color-selection-dialog-colorsel", Fgtk_color_selection_dialog_colorsel, 1, 1, 0, /*
Access the `colorsel' slot of OBJ, a GtkColorSelectionDialog object.
*/
	(obj))
{
	GtkColorSelectionDialog *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_COLOR_SELECTION_DIALOG (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkColorSelectionDialog", obj);
	};

	the_obj = GTK_COLOR_SELECTION_DIALOG (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->colorsel);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-color-selection-dialog-main-vbox", Fgtk_color_selection_dialog_main_vbox, 1, 1, 0, /*
Access the `main_vbox' slot of OBJ, a GtkColorSelectionDialog object.
*/
	(obj))
{
	GtkColorSelectionDialog *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_COLOR_SELECTION_DIALOG (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkColorSelectionDialog", obj);
	};

	the_obj = GTK_COLOR_SELECTION_DIALOG (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->main_vbox);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-color-selection-dialog-ok-button", Fgtk_color_selection_dialog_ok_button, 1, 1, 0, /*
Access the `ok_button' slot of OBJ, a GtkColorSelectionDialog object.
*/
	(obj))
{
	GtkColorSelectionDialog *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_COLOR_SELECTION_DIALOG (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkColorSelectionDialog", obj);
	};

	the_obj = GTK_COLOR_SELECTION_DIALOG (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->ok_button);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-color-selection-dialog-reset-button", Fgtk_color_selection_dialog_reset_button, 1, 1, 0, /*
Access the `reset_button' slot of OBJ, a GtkColorSelectionDialog object.
*/
	(obj))
{
	GtkColorSelectionDialog *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_COLOR_SELECTION_DIALOG (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkColorSelectionDialog", obj);
	};

	the_obj = GTK_COLOR_SELECTION_DIALOG (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->reset_button);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-color-selection-dialog-cancel-button", Fgtk_color_selection_dialog_cancel_button, 1, 1, 0, /*
Access the `cancel_button' slot of OBJ, a GtkColorSelectionDialog object.
*/
	(obj))
{
	GtkColorSelectionDialog *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_COLOR_SELECTION_DIALOG (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkColorSelectionDialog", obj);
	};

	the_obj = GTK_COLOR_SELECTION_DIALOG (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->cancel_button);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-color-selection-dialog-help-button", Fgtk_color_selection_dialog_help_button, 1, 1, 0, /*
Access the `help_button' slot of OBJ, a GtkColorSelectionDialog object.
*/
	(obj))
{
	GtkColorSelectionDialog *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_COLOR_SELECTION_DIALOG (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkColorSelectionDialog", obj);
	};

	the_obj = GTK_COLOR_SELECTION_DIALOG (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->help_button);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-dialog-vbox", Fgtk_dialog_vbox, 1, 1, 0, /*
Access the `vbox' slot of OBJ, a GtkDialog object.
*/
	(obj))
{
	GtkDialog *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_DIALOG (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkDialog", obj);
	};

	the_obj = GTK_DIALOG (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->vbox);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-dialog-action-area", Fgtk_dialog_action_area, 1, 1, 0, /*
Access the `action_area' slot of OBJ, a GtkDialog object.
*/
	(obj))
{
	GtkDialog *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_DIALOG (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkDialog", obj);
	};

	the_obj = GTK_DIALOG (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->action_area);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-input-dialog-close-button", Fgtk_input_dialog_close_button, 1, 1, 0, /*
Access the `close_button' slot of OBJ, a GtkInputDialog object.
*/
	(obj))
{
	GtkInputDialog *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_INPUT_DIALOG (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkInputDialog", obj);
	};

	the_obj = GTK_INPUT_DIALOG (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->close_button);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-input-dialog-save-button", Fgtk_input_dialog_save_button, 1, 1, 0, /*
Access the `save_button' slot of OBJ, a GtkInputDialog object.
*/
	(obj))
{
	GtkInputDialog *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_INPUT_DIALOG (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkInputDialog", obj);
	};

	the_obj = GTK_INPUT_DIALOG (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->save_button);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-plug-socket-window", Fgtk_plug_socket_window, 1, 1, 0, /*
Access the `socket_window' slot of OBJ, a GtkPlug object.
*/
	(obj))
{
	GtkPlug *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_PLUG (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkPlug", obj);
	};

	the_obj = GTK_PLUG (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GdkWindow");
	GTK_VALUE_BOXED (arg) = (void *)the_obj->socket_window;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-plug-same-app", Fgtk_plug_same_app, 1, 1, 0, /*
Access the `same_app' slot of OBJ, a GtkPlug object.
*/
	(obj))
{
	GtkPlug *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_PLUG (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkPlug", obj);
	};

	the_obj = GTK_PLUG (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gint");
	GTK_VALUE_INT (arg) = the_obj->same_app;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-object-flags", Fgtk_object_flags, 1, 1, 0, /*
Access the `flags' slot of OBJ, a GtkObject object.
*/
	(obj))
{
	GtkObject *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_OBJECT (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkObject", obj);
	};

	the_obj = GTK_OBJECT (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("guint");
	GTK_VALUE_UINT (arg) = the_obj->flags;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-object-ref-count", Fgtk_object_ref_count, 1, 1, 0, /*
Access the `ref_count' slot of OBJ, a GtkObject object.
*/
	(obj))
{
	GtkObject *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_OBJECT (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkObject", obj);
	};

	the_obj = GTK_OBJECT (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("guint");
	GTK_VALUE_UINT (arg) = the_obj->ref_count;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-paned-child1", Fgtk_paned_child1, 1, 1, 0, /*
Access the `child1' slot of OBJ, a GtkPaned object.
*/
	(obj))
{
	GtkPaned *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_PANED (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkPaned", obj);
	};

	the_obj = GTK_PANED (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->child1);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-paned-child2", Fgtk_paned_child2, 1, 1, 0, /*
Access the `child2' slot of OBJ, a GtkPaned object.
*/
	(obj))
{
	GtkPaned *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_PANED (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkPaned", obj);
	};

	the_obj = GTK_PANED (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->child2);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-paned-child1-resize", Fgtk_paned_child1_resize, 1, 1, 0, /*
Access the `child1_resize' slot of OBJ, a GtkPaned object.
*/
	(obj))
{
	GtkPaned *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_PANED (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkPaned", obj);
	};

	the_obj = GTK_PANED (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gboolean");
	GTK_VALUE_BOOL (arg) = the_obj->child1_resize;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-paned-child2-resize", Fgtk_paned_child2_resize, 1, 1, 0, /*
Access the `child2_resize' slot of OBJ, a GtkPaned object.
*/
	(obj))
{
	GtkPaned *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_PANED (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkPaned", obj);
	};

	the_obj = GTK_PANED (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gboolean");
	GTK_VALUE_BOOL (arg) = the_obj->child2_resize;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-paned-child1-shrink", Fgtk_paned_child1_shrink, 1, 1, 0, /*
Access the `child1_shrink' slot of OBJ, a GtkPaned object.
*/
	(obj))
{
	GtkPaned *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_PANED (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkPaned", obj);
	};

	the_obj = GTK_PANED (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gboolean");
	GTK_VALUE_BOOL (arg) = the_obj->child1_shrink;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-paned-child2-shrink", Fgtk_paned_child2_shrink, 1, 1, 0, /*
Access the `child2_shrink' slot of OBJ, a GtkPaned object.
*/
	(obj))
{
	GtkPaned *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_PANED (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkPaned", obj);
	};

	the_obj = GTK_PANED (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gboolean");
	GTK_VALUE_BOOL (arg) = the_obj->child2_shrink;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-clist-rows", Fgtk_clist_rows, 1, 1, 0, /*
Access the `rows' slot of OBJ, a GtkCList object.
*/
	(obj))
{
	GtkCList *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_CLIST (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkCList", obj);
	};

	the_obj = GTK_CLIST (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gint");
	GTK_VALUE_INT (arg) = the_obj->rows;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-clist-columns", Fgtk_clist_columns, 1, 1, 0, /*
Access the `columns' slot of OBJ, a GtkCList object.
*/
	(obj))
{
	GtkCList *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_CLIST (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkCList", obj);
	};

	the_obj = GTK_CLIST (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gint");
	GTK_VALUE_INT (arg) = the_obj->columns;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-clist-hadjustment", Fgtk_clist_hadjustment, 1, 1, 0, /*
Access the `hadjustment' slot of OBJ, a GtkCList object.
*/
	(obj))
{
	GtkCList *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_CLIST (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkCList", obj);
	};

	the_obj = GTK_CLIST (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkAdjustment");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->hadjustment);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-clist-vadjustment", Fgtk_clist_vadjustment, 1, 1, 0, /*
Access the `vadjustment' slot of OBJ, a GtkCList object.
*/
	(obj))
{
	GtkCList *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_CLIST (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkCList", obj);
	};

	the_obj = GTK_CLIST (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkAdjustment");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->vadjustment);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-clist-sort-type", Fgtk_clist_sort_type, 1, 1, 0, /*
Access the `sort_type' slot of OBJ, a GtkCList object.
*/
	(obj))
{
	GtkCList *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_CLIST (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkCList", obj);
	};

	the_obj = GTK_CLIST (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkSortType");
	GTK_VALUE_ENUM (arg) = the_obj->sort_type;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-clist-focus-row", Fgtk_clist_focus_row, 1, 1, 0, /*
Access the `focus_row' slot of OBJ, a GtkCList object.
*/
	(obj))
{
	GtkCList *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_CLIST (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkCList", obj);
	};

	the_obj = GTK_CLIST (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gint");
	GTK_VALUE_INT (arg) = the_obj->focus_row;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-clist-sort-column", Fgtk_clist_sort_column, 1, 1, 0, /*
Access the `sort_column' slot of OBJ, a GtkCList object.
*/
	(obj))
{
	GtkCList *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_CLIST (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkCList", obj);
	};

	the_obj = GTK_CLIST (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gint");
	GTK_VALUE_INT (arg) = the_obj->sort_column;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-list-children", Fgtk_list_children, 1, 1, 0, /*
Access the `children' slot of OBJ, a GtkList object.
*/
	(obj))
{
	GtkList *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_LIST (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkList", obj);
	};

	the_obj = GTK_LIST (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkListOfObject");
	GTK_VALUE_POINTER (arg) = the_obj->children;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-list-selection", Fgtk_list_selection, 1, 1, 0, /*
Access the `selection' slot of OBJ, a GtkList object.
*/
	(obj))
{
	GtkList *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_LIST (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkList", obj);
	};

	the_obj = GTK_LIST (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkListOfObject");
	GTK_VALUE_POINTER (arg) = the_obj->selection;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-tree-children", Fgtk_tree_children, 1, 1, 0, /*
Access the `children' slot of OBJ, a GtkTree object.
*/
	(obj))
{
	GtkTree *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_TREE (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkTree", obj);
	};

	the_obj = GTK_TREE (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkListOfObject");
	GTK_VALUE_POINTER (arg) = the_obj->children;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-tree-root-tree", Fgtk_tree_root_tree, 1, 1, 0, /*
Access the `root_tree' slot of OBJ, a GtkTree object.
*/
	(obj))
{
	GtkTree *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_TREE (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkTree", obj);
	};

	the_obj = GTK_TREE (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkTree");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->root_tree);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-tree-tree-owner", Fgtk_tree_tree_owner, 1, 1, 0, /*
Access the `tree_owner' slot of OBJ, a GtkTree object.
*/
	(obj))
{
	GtkTree *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_TREE (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkTree", obj);
	};

	the_obj = GTK_TREE (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->tree_owner);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-tree-selection", Fgtk_tree_selection, 1, 1, 0, /*
Access the `selection' slot of OBJ, a GtkTree object.
*/
	(obj))
{
	GtkTree *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_TREE (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkTree", obj);
	};

	the_obj = GTK_TREE (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkListOfObject");
	GTK_VALUE_POINTER (arg) = the_obj->selection;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-tree-item-subtree", Fgtk_tree_item_subtree, 1, 1, 0, /*
Access the `subtree' slot of OBJ, a GtkTreeItem object.
*/
	(obj))
{
	GtkTreeItem *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_TREE_ITEM (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkTreeItem", obj);
	};

	the_obj = GTK_TREE_ITEM (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->subtree);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-scrolled-window-hscrollbar", Fgtk_scrolled_window_hscrollbar, 1, 1, 0, /*
Access the `hscrollbar' slot of OBJ, a GtkScrolledWindow object.
*/
	(obj))
{
	GtkScrolledWindow *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_SCROLLED_WINDOW (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkScrolledWindow", obj);
	};

	the_obj = GTK_SCROLLED_WINDOW (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->hscrollbar);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-scrolled-window-vscrollbar", Fgtk_scrolled_window_vscrollbar, 1, 1, 0, /*
Access the `vscrollbar' slot of OBJ, a GtkScrolledWindow object.
*/
	(obj))
{
	GtkScrolledWindow *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_SCROLLED_WINDOW (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkScrolledWindow", obj);
	};

	the_obj = GTK_SCROLLED_WINDOW (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("GtkWidget");
	GTK_VALUE_OBJECT (arg) = GTK_OBJECT (the_obj->vscrollbar);
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-scrolled-window-hscrollbar-visible", Fgtk_scrolled_window_hscrollbar_visible, 1, 1, 0, /*
Access the `hscrollbar_visible' slot of OBJ, a GtkScrolledWindow object.
*/
	(obj))
{
	GtkScrolledWindow *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_SCROLLED_WINDOW (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkScrolledWindow", obj);
	};

	the_obj = GTK_SCROLLED_WINDOW (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gboolean");
	GTK_VALUE_BOOL (arg) = the_obj->hscrollbar_visible;
	return (gtk_type_to_lisp (&arg));
}

DEFUN ("gtk-scrolled-window-vscrollbar-visible", Fgtk_scrolled_window_vscrollbar_visible, 1, 1, 0, /*
Access the `vscrollbar_visible' slot of OBJ, a GtkScrolledWindow object.
*/
	(obj))
{
	GtkScrolledWindow *the_obj = NULL;
	GtkArg arg;

	CHECK_GTK_OBJECT (obj);

	if (!GTK_IS_SCROLLED_WINDOW (XGTK_OBJECT (obj)->object))
	{
		signal_simple_error ("Object is not a GtkScrolledWindow", obj);
	};

	the_obj = GTK_SCROLLED_WINDOW (XGTK_OBJECT (obj)->object);
	arg.type = gtk_type_from_name ("gboolean");
	GTK_VALUE_BOOL (arg) = the_obj->vscrollbar_visible;
	return (gtk_type_to_lisp (&arg));
}

void syms_of_widget_accessors  (void)
{
	DEFSUBR (Fgtk_scrolled_window_vscrollbar_visible);
	DEFSUBR (Fgtk_scrolled_window_hscrollbar_visible);
	DEFSUBR (Fgtk_scrolled_window_vscrollbar);
	DEFSUBR (Fgtk_scrolled_window_hscrollbar);
	DEFSUBR (Fgtk_tree_item_subtree);
	DEFSUBR (Fgtk_tree_selection);
	DEFSUBR (Fgtk_tree_tree_owner);
	DEFSUBR (Fgtk_tree_root_tree);
	DEFSUBR (Fgtk_tree_children);
	DEFSUBR (Fgtk_list_selection);
	DEFSUBR (Fgtk_list_children);
	DEFSUBR (Fgtk_clist_sort_column);
	DEFSUBR (Fgtk_clist_focus_row);
	DEFSUBR (Fgtk_clist_sort_type);
	DEFSUBR (Fgtk_clist_vadjustment);
	DEFSUBR (Fgtk_clist_hadjustment);
	DEFSUBR (Fgtk_clist_columns);
	DEFSUBR (Fgtk_clist_rows);
	DEFSUBR (Fgtk_paned_child2_shrink);
	DEFSUBR (Fgtk_paned_child1_shrink);
	DEFSUBR (Fgtk_paned_child2_resize);
	DEFSUBR (Fgtk_paned_child1_resize);
	DEFSUBR (Fgtk_paned_child2);
	DEFSUBR (Fgtk_paned_child1);
	DEFSUBR (Fgtk_object_ref_count);
	DEFSUBR (Fgtk_object_flags);
	DEFSUBR (Fgtk_plug_same_app);
	DEFSUBR (Fgtk_plug_socket_window);
	DEFSUBR (Fgtk_input_dialog_save_button);
	DEFSUBR (Fgtk_input_dialog_close_button);
	DEFSUBR (Fgtk_dialog_action_area);
	DEFSUBR (Fgtk_dialog_vbox);
	DEFSUBR (Fgtk_color_selection_dialog_help_button);
	DEFSUBR (Fgtk_color_selection_dialog_cancel_button);
	DEFSUBR (Fgtk_color_selection_dialog_reset_button);
	DEFSUBR (Fgtk_color_selection_dialog_ok_button);
	DEFSUBR (Fgtk_color_selection_dialog_main_vbox);
	DEFSUBR (Fgtk_color_selection_dialog_colorsel);
	DEFSUBR (Fgtk_font_selection_dialog_cancel_button);
	DEFSUBR (Fgtk_font_selection_dialog_apply_button);
	DEFSUBR (Fgtk_font_selection_dialog_ok_button);
	DEFSUBR (Fgtk_font_selection_dialog_action_area);
	DEFSUBR (Fgtk_font_selection_dialog_main_vbox);
	DEFSUBR (Fgtk_font_selection_dialog_fontsel);
	DEFSUBR (Fgtk_file_selection_action_area);
	DEFSUBR (Fgtk_file_selection_help_button);
	DEFSUBR (Fgtk_file_selection_cancel_button);
	DEFSUBR (Fgtk_file_selection_ok_button);
	DEFSUBR (Fgtk_file_selection_main_vbox);
	DEFSUBR (Fgtk_file_selection_selection_text);
	DEFSUBR (Fgtk_file_selection_selection_entry);
	DEFSUBR (Fgtk_file_selection_file_list);
	DEFSUBR (Fgtk_file_selection_dir_list);
	DEFSUBR (Fgtk_text_vadj);
	DEFSUBR (Fgtk_text_hadj);
	DEFSUBR (Fgtk_notebook_tab_pos);
	DEFSUBR (Fgtk_check_menu_item_active);
	DEFSUBR (Fgtk_gamma_curve_gamma_text);
	DEFSUBR (Fgtk_gamma_curve_gamma_dialog);
	DEFSUBR (Fgtk_gamma_curve_gamma);
	DEFSUBR (Fgtk_gamma_curve_curve);
	DEFSUBR (Fgtk_gamma_curve_table);
	DEFSUBR (Fgtk_combo_list);
	DEFSUBR (Fgtk_combo_popwin);
	DEFSUBR (Fgtk_combo_popup);
	DEFSUBR (Fgtk_combo_button);
	DEFSUBR (Fgtk_combo_entry);
	DEFSUBR (Fgtk_button_button_down);
	DEFSUBR (Fgtk_button_in_button);
	DEFSUBR (Fgtk_button_child);
	DEFSUBR (Fgtk_widget_parent);
	DEFSUBR (Fgtk_widget_name);
	DEFSUBR (Fgtk_widget_state);
	DEFSUBR (Fgtk_widget_window);
	DEFSUBR (Fgtk_widget_style);
	DEFSUBR (Fgtk_adjustment_page_size);
	DEFSUBR (Fgtk_adjustment_page_increment);
	DEFSUBR (Fgtk_adjustment_step_increment);
	DEFSUBR (Fgtk_adjustment_value);
	DEFSUBR (Fgtk_adjustment_upper);
	DEFSUBR (Fgtk_adjustment_lower);
}
