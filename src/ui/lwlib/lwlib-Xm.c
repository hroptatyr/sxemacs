/* The lwlib interface to Motif widgets.
   Copyright (C) 1992, 1993, 1994 Lucid, Inc.
   Copyright (C) 1995 Tinker Systems and INS Engineering Corp.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software: you can redistribute it
and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

The Lucid Widget Library is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */

#include <config.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/ObjectP.h>
#include <X11/CoreP.h>
#include <X11/CompositeP.h>

#include "lwlib-Xm.h"
#include "lwlib-utils.h"

#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <Xm/CascadeB.h>
#include <Xm/DrawingA.h>
#include <Xm/FileSB.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/ArrowB.h>
#include <Xm/ScrollBar.h>
#include <Xm/SelectioB.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/Separator.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#ifdef LWLIB_WIDGETS_MOTIF
#include <Xm/Scale.h>
#if XmVERSION > 1
#include <Xm/ComboBoxP.h>
#endif
#endif

#ifdef LWLIB_MENUBARS_MOTIF
static void xm_pull_down_callback(Widget, XtPointer, XtPointer);
#endif
static void xm_internal_update_other_instances(Widget, XtPointer, XtPointer);
static void xm_pop_down_callback(Widget, XtPointer, XtPointer);
static void xm_generic_callback(Widget, XtPointer, XtPointer);
static void mark_dead_instance_destroyed(Widget widget, XtPointer closure,
					 XtPointer call_data);
#if defined (LWLIB_DIALOGS_MOTIF) || defined (LWLIB_WIDGETS_MOTIF)
static void xm_nosel_callback(Widget, XtPointer, XtPointer);
#endif
#ifdef LWLIB_SCROLLBARS_MOTIF
static void xm_scrollbar_callback(Widget, XtPointer, XtPointer);
#endif

#ifdef LWLIB_MENUBARS_MOTIF
static void
xm_update_menu(widget_instance * instance, Widget widget, widget_value * val,
	       Boolean deep_p);
#endif

/* Structures to keep destroyed instances */
typedef struct _destroyed_instance {
	char *name;
	char *type;
	Widget widget;
	Widget parent;
	Boolean pop_up_p;
	struct _destroyed_instance *next;
} destroyed_instance;

static destroyed_instance *all_destroyed_instances = NULL;

/* Utility function. */
static char *safe_strdup(char *s)
{
	char *result;
	if (!s)
		return 0;
	result = (char *)malloc(strlen(s) + 1);
	if (!result)
		return 0;
	strcpy(result, s);
	return result;
}

static destroyed_instance *make_destroyed_instance(char *name, char *type,
						   Widget widget, Widget parent,
						   Boolean pop_up_p)
{
	destroyed_instance *instance =
	    (destroyed_instance *) malloc(sizeof(destroyed_instance));
	instance->name = safe_strdup(name);
	instance->type = safe_strdup(type);
	instance->widget = widget;
	instance->parent = parent;
	instance->pop_up_p = pop_up_p;
	instance->next = NULL;
	return instance;
}

static void free_destroyed_instance(destroyed_instance * instance)
{
	free(instance->name);
	free(instance->type);
	free(instance);
}

/* motif utility functions */
Widget first_child(Widget widget)
{
	return ((CompositeWidget) widget)->composite.children[0];
}

Boolean lw_motif_widget_p(Widget widget)
{
	return
#ifdef LWLIB_DIALOGS_MOTIF
	    XtClass(widget) == xmDialogShellWidgetClass ||
#endif
	    XmIsPrimitive(widget) || XmIsManager(widget) || XmIsGadget(widget);
}

static char *resource_string(Widget widget, char *name)
{
	XtResource resource;
	char *result = NULL;

	resource.resource_name = "labelString";
	resource.resource_class = "LabelString";	/* #### should be Xmsomething... */
	resource.resource_type = XtRString;
	resource.resource_size = sizeof(String);
	resource.resource_offset = 0;
	resource.default_type = XtRImmediate;
	resource.default_addr = 0;

	XtGetSubresources(widget, (XtPointer) & result, name,
			  name, &resource, 1, NULL, 0);
	return result;
}

#ifdef LWLIB_DIALOGS_MOTIF

static Boolean is_in_dialog_box(Widget w)
{
	Widget wmshell;

	wmshell = XtParent(w);
	while (wmshell && (XtClass(wmshell) != xmDialogShellWidgetClass))
		wmshell = XtParent(wmshell);

	if (wmshell && XtClass(wmshell) == xmDialogShellWidgetClass)
		return True;
	else
		return False;
}

#endif				/* LWLIB_DIALOGS_MOTIF */

#if defined (LWLIB_DIALOGS_MOTIF) || defined (LWLIB_MENUBARS_MOTIF) || defined (LWLIB_WIDGETS_MOTIF)

/* update the label of anything subclass of a label */
static void
xm_update_label(widget_instance * instance, Widget widget, widget_value * val)
{
	XmString built_string = NULL;
	XmString key_string = NULL;
	XmString val_string = NULL;
	XmString name_string = NULL;
	Arg al[20];
	int ac = 0;
	int type;

	/* Don't clobber pixmap types. */
	XtSetArg(al[0], XmNlabelType, &type);
	XtGetValues(widget, al, 1);

	if (type == XmPIXMAP)
		return;

	if (val->value) {
		/* #### Temporary fix. I though Motif was supposed to grok %_
		   type things. */
		lw_remove_accelerator_spec(val->value);

#ifdef LWLIB_DIALOGS_MOTIF
		/*
		 * Sigh.  The main text of a label is the name field for menubar
		 * entries.  The value field is a possible additional field to be
		 * concatenated on to the name field.  HOWEVER, with dialog boxes
		 * the value field is the complete text which is supposed to be
		 * displayed as the label.  Yuck.
		 */
		if (is_in_dialog_box(widget)) {
			char *value_name = NULL;

			value_name = resource_string(widget, val->value);
			if (!value_name)
				value_name = val->value;

			built_string =
			    XmStringCreateLtoR(value_name,
					       XmSTRING_DEFAULT_CHARSET);
		} else
#endif				/* LWLIB_DIALOGS_MOTIF */
		{
			char *value_name = NULL;
			char *res_name = NULL;

			res_name = resource_string(widget, val->name);
			/* Concatenating the value with itself seems just plain daft. */
			if (!res_name) {
				built_string =
				    XmStringCreateLtoR(val->value,
						       XmSTRING_DEFAULT_CHARSET);
			} else {
				name_string =
				    XmStringCreateLtoR(res_name,
						       XmSTRING_DEFAULT_CHARSET);

				value_name = XtMalloc(strlen(val->value) + 2);
				*value_name = 0;
				strcat(value_name, " ");
				strcat(value_name, val->value);

				val_string =
				    XmStringCreateLtoR(value_name,
						       XmSTRING_DEFAULT_CHARSET);

				built_string =
				    XmStringConcat(name_string, val_string);

				XtFree(value_name);
			}
		}

		XtSetArg(al[ac], XmNlabelString, built_string);
		ac++;
		XtSetArg(al[ac], XmNlabelType, XmSTRING);
		ac++;
	}

	if (val->key) {
		key_string =
		    XmStringCreateLtoR(val->key, XmSTRING_DEFAULT_CHARSET);
		XtSetArg(al[ac], XmNacceleratorText, key_string);
		ac++;
	}

	if (ac)
		XtSetValues(widget, al, ac);

	if (built_string)
		XmStringFree(built_string);

	if (key_string)
		XmStringFree(key_string);

	if (name_string)
		XmStringFree(name_string);

	if (val_string)
		XmStringFree(val_string);
}

static void
xm_safe_update_label(widget_instance * instance, Widget widget,
		     widget_value * val)
{
	/* Don't clobber non-labels. */
	if (XtIsSubclass(widget, xmLabelWidgetClass))
		xm_update_label(instance, widget, val);
}

#endif				/* defined (LWLIB_DIALOGS_MOTIF) || defined (LWLIB_MENUBARS_MOTIF) */

/* update of list */
static void
xm_update_list(widget_instance * instance, Widget widget, widget_value * val)
{
	widget_value *cur;
	int i;
	XtRemoveAllCallbacks(widget, XmNsingleSelectionCallback);
	XtAddCallback(widget, XmNsingleSelectionCallback, xm_generic_callback,
		      instance);
	for (cur = val->contents, i = 0; cur; cur = cur->next)
		if (cur->value) {
			XmString xmstr =
			    XmStringCreate(cur->value,
					   XmSTRING_DEFAULT_CHARSET);
			i += 1;
			XmListAddItem(widget, xmstr, 0);
			if (cur->selected)
				XmListSelectPos(widget, i, False);
			XmStringFree(xmstr);
		}
}

/* update of buttons */
static void
xm_update_pushbutton(widget_instance * instance, Widget widget,
		     widget_value * val)
{
	Arg al[1];
	XtSetArg(al[0], XmNalignment, XmALIGNMENT_CENTER);
	XtSetValues(widget, al, 1);
	XtRemoveAllCallbacks(widget, XmNactivateCallback);
	XtAddCallback(widget, XmNactivateCallback, xm_generic_callback,
		      instance);
}

#ifdef LWLIB_WIDGETS_MOTIF
static void
xm_update_progress(widget_instance * instance, Widget scale, widget_value * val)
{
	Arg al[20];
	int ac = 0;
	Dimension height = 0;
	Dimension width = 0;
	if (!val->call_data) {
		XtSetArg(al[ac], XmNeditable, False);
		ac++;
	} else {
		XtSetArg(al[ac], XmNeditable, val->enabled);
		ac++;
	}
	height = (Dimension) lw_get_value_arg(val, XtNheight);
	width = (Dimension) lw_get_value_arg(val, XtNwidth);
	if (height > 0) {
		XtSetArg(al[ac], XmNscaleHeight, height);
		ac++;
	}
	if (width > 0) {
		XtSetArg(al[ac], XmNscaleWidth, width);
		ac++;
	}

	XtSetValues(scale, al, 1);
}
#endif				/* LWLIB_WIDGETS_MOTIF */

#ifdef LWLIB_MENUBARS_MOTIF

static void
xm_update_cascadebutton(widget_instance * instance, Widget widget,
			widget_value * val)
{
	/* Should also rebuild the menu by calling ...update_menu... */
	if (val
	    && val->type == CASCADE_TYPE
	    && val->contents && val->contents->type == INCREMENTAL_TYPE) {
		/* okay, we're now doing a lisp callback to incrementally generate
		   more of the menu. */
		XtRemoveAllCallbacks(widget, XmNcascadingCallback);
		XtAddCallback(widget, XmNcascadingCallback,
			      xm_pull_down_callback, instance);
		XtCallCallbacks((Widget) widget, XmNcascadingCallback,
				(XtPointer) val->contents);

	} else {
		XtRemoveAllCallbacks(widget, XmNcascadingCallback);
		XtAddCallback(widget, XmNcascadingCallback,
			      xm_pull_down_callback, instance);
	}
}

#endif				/* LWLIB_MENUBARS_MOTIF */

/* update toggle and radiobox */
static void
xm_update_toggle(widget_instance * instance, Widget widget, widget_value * val)
{
	Arg al[2];
	XtRemoveAllCallbacks(widget, XmNvalueChangedCallback);
	XtAddCallback(widget, XmNvalueChangedCallback, xm_generic_callback,
		      instance);
	XtSetArg(al[0], XmNset, val->selected);
	XtSetArg(al[1], XmNalignment, XmALIGNMENT_BEGINNING);
	XtSetValues(widget, al, 1);
}

static void
xm_update_radiobox(widget_instance * instance, Widget widget,
		   widget_value * val)
{
	Widget toggle;
	widget_value *cur;

	/* update the callback */
	XtRemoveAllCallbacks(widget, XmNentryCallback);
	XtAddCallback(widget, XmNentryCallback, xm_generic_callback, instance);

	/* first update all the toggles */
	/* Energize kernel interface is currently bad.  It sets the selected widget
	   with the selected flag but returns it by its name.  So we currently
	   have to support both setting the selection with the selected slot
	   of val contents and setting it with the "value" slot of val.  The latter
	   has a higher priority.  This to be removed when the kernel is fixed. */
	for (cur = val->contents; cur; cur = cur->next) {
		toggle = XtNameToWidget(widget, cur->value);
		if (toggle) {
			Arg al[2];
			XtSetArg(al[0], XmNsensitive, cur->enabled);
			XtSetArg(al[1], XmNset,
				 (!val->value
				  && cur->selected ? cur->selected : False));
			XtSetValues(toggle, al, 2);
		}
	}

	/* The selected was specified by the value slot */
	if (val->value) {
		toggle = XtNameToWidget(widget, val->value);
		if (toggle) {
			Arg al[1];
			XtSetArg(al[0], XmNset, True);
			XtSetValues(toggle, al, 1);
		}
	}
}

#if defined (LWLIB_WIDGETS_MOTIF) && XmVERSION > 1
/* update of combo box */
static void
xm_update_combo_box(widget_instance * instance, Widget widget,
		    widget_value * val)
{
	widget_value *cur;
	int i;
	XtRemoveAllCallbacks(widget, XmNselectionCallback);
	XtAddCallback(widget, XmNselectionCallback, xm_generic_callback,
		      instance);
	for (cur = val->contents, i = 0; cur; cur = cur->next)
		if (cur->value) {
			XmString xmstr =
			    XmStringCreate(cur->value,
					   XmSTRING_DEFAULT_CHARSET);
			i += 1;
			XmListAddItem(CB_List(widget), xmstr, 0);
			if (cur->selected)
				XmListSelectPos(CB_List(widget), i, False);
			XmStringFree(xmstr);
		}
}
#endif

#ifdef LWLIB_MENUBARS_MOTIF

/* update a popup menu, pulldown menu or a menubar */
static void
make_menu_in_widget(widget_instance * instance, Widget widget,
		    widget_value * val)
{
	Widget *children = 0;
	int num_children;
	int child_index;
	widget_value *cur;
	Widget button = 0;
	Widget menu;
	Arg al[256];
	int ac;
	Boolean menubar_p = False;

	/* Allocate the children array */
	for (num_children = 0, cur = val; cur;
	     num_children++, cur = cur->next) ;
	children = (Widget *) XtMalloc(num_children * sizeof(Widget));

	/* tricky way to know if this RowColumn is a menubar or a pulldown... */
	XtSetArg(al[0], XmNisHomogeneous, &menubar_p);
	XtGetValues(widget, al, 1);

	/* add the unmap callback for popups and pulldowns */
  /*** this sounds bogus ***/
	/* probably because it is -- cet */
/*
  if (!menubar_p)
    XtAddCallback (XtParent (widget), XmNpopdownCallback,
		   xm_pop_down_callback, (XtPointer)instance);
*/

	num_children = 0;
	for (child_index = 0, cur = val; cur; child_index++, cur = cur->next) {
		ac = 0;
		button = 0;
		XtSetArg(al[ac], XmNsensitive, cur->enabled);
		ac++;
		XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING);
		ac++;
		XtSetArg(al[ac], XmNuserData, cur->call_data);
		ac++;

		switch (cur->type) {
		case PUSHRIGHT_TYPE:
			/* A pushright marker which is not needed for the real Motif
			   menubar. */
			break;
		case SEPARATOR_TYPE:
			ac = 0;
			if (cur->value) {
				/* #### - xlwmenu.h supports several types that motif does
				   not.  Also, motif supports pixmaps w/ type NO_LINE and
				   lwlib provides no way to access that functionality. --Stig */
				XtSetArg(al[ac], XmNseparatorType, cur->value),
				    ac++;
			}
			button = XmCreateSeparator(widget, "separator", al, ac);
			break;
		case CASCADE_TYPE:
			menu =
			    XmCreatePulldownMenu(widget, "pulldown", NULL, 0);
			make_menu_in_widget(instance, menu, cur->contents);
			XtSetArg(al[ac], XmNsubMenuId, menu);
			ac++;
			button =
			    XmCreateCascadeButton(widget, cur->name, al, ac);

			xm_safe_update_label(instance, button, cur);

			XtAddCallback(button, XmNcascadingCallback,
				      xm_pull_down_callback,
				      (XtPointer) instance);
			break;
		default:
			if (menubar_p)
				button =
				    XmCreateCascadeButton(widget, cur->name, al,
							  ac);
			else if (!cur->call_data)
				button =
				    XmCreateLabel(widget, cur->name, al, ac);
			else if (cur->type == TOGGLE_TYPE
				 || cur->type == RADIO_TYPE) {
				XtSetArg(al[ac], XmNindicatorType,
					 (cur->type ==
					  TOGGLE_TYPE ? XmN_OF_MANY :
					  XmONE_OF_MANY));
				ac++;
				XtSetArg(al[ac], XmNvisibleWhenOff, True);
				ac++;
				button =
				    XmCreateToggleButtonGadget(widget,
							       cur->name, al,
							       ac);
			} else
				button =
				    XmCreatePushButtonGadget(widget, cur->name,
							     al, ac);

			xm_safe_update_label(instance, button, cur);

			/* don't add a callback to a simple label */
			if (cur->type == TOGGLE_TYPE || cur->type == RADIO_TYPE)
				xm_update_toggle(instance, button, cur);
			else if (cur->call_data)
				XtAddCallback(button, XmNactivateCallback,
					      xm_generic_callback,
					      (XtPointer) instance);
		}		/* switch (cur->type) */

		if (button)
			children[num_children++] = button;
	}

	/* Last entry is the help button.  This used be done after managing
	   the buttons.  The comment claimed that it had to be done this way
	   otherwise the menubar ended up only 4 pixels high.  That must
	   have been in the Old World.  In the New World it stays the proper
	   height if you don't manage them until after you set this and as a
	   bonus the Help menu ends up where it is supposed to. */
	if (button) {
		ac = 0;
		XtSetArg(al[ac], XmNmenuHelpWidget, button);
		ac++;
		XtSetValues(widget, al, ac);
	}

	if (num_children)
		XtManageChildren(children, num_children);

	XtFree((char *)children);
}

static void
update_one_menu_entry(widget_instance * instance, Widget widget,
		      widget_value * val, Boolean deep_p)
{
	Arg al[2];
	int ac;
	Widget menu;
	widget_value *contents;

	if (val->change == NO_CHANGE)
		return;

	/* update the sensitivity and userdata */
	/* Common to all widget types */
	XtSetArg(al[0], XmNsensitive, val->enabled);
	XtSetArg(al[1], XmNuserData, val->call_data);
	XtSetValues(widget, al, 2);

	/* update the menu button as a label. */
	if (val->change >= VISIBLE_CHANGE) {
		xm_safe_update_label(instance, widget, val);

		if (XtClass(widget) == xmToggleButtonWidgetClass
		    || XtClass(widget) == xmToggleButtonGadgetClass) {
			xm_update_toggle(instance, widget, val);
		}
	}

	/* update the pulldown/pullaside as needed */
	menu = NULL;
	XtSetArg(al[0], XmNsubMenuId, &menu);
	XtGetValues(widget, al, 1);

	contents = val->contents;

	if (!menu) {
		if (contents) {
			menu =
			    XmCreatePulldownMenu(widget, "pulldown", NULL, 0);
			make_menu_in_widget(instance, menu, contents);
			ac = 0;
			XtSetArg(al[ac], XmNsubMenuId, menu);
			ac++;
			XtSetValues(widget, al, ac);
		}
	} else if (!contents) {
		ac = 0;
		XtSetArg(al[ac], XmNsubMenuId, NULL);
		ac++;
		XtSetValues(widget, al, ac);
		XtDestroyWidget(menu);
	} else if (deep_p && contents->change != NO_CHANGE)
		xm_update_menu(instance, menu, val, 1);
}

static void
xm_update_menu(widget_instance * instance, Widget widget, widget_value * val,
	       Boolean deep_p)
{
	/* Widget is a RowColumn widget whose contents have to be updated
	 * to reflect the list of items in val->contents */
	if (val->contents->change == STRUCTURAL_CHANGE) {
		destroy_all_children(widget);
		make_menu_in_widget(instance, widget, val->contents);
	} else {
		/* Update all the buttons of the RowColumn in order. */
		Widget *children;
		unsigned int num_children;
		int i;
		widget_value *cur = 0;

		children = XtCompositeChildren(widget, &num_children);
		if (children) {
			for (i = 0, cur = val->contents; i < num_children; i++) {
				if (!cur)
					abort();
				/* skip if this is a pushright marker or a separator */
				if (cur->type == PUSHRIGHT_TYPE
				    || cur->type == SEPARATOR_TYPE) {
					cur = cur->next;
#if 0
					/* #### - this could puke if you have a separator as the
					   last item on a pullright menu. */
					if (!cur)
						abort();
#else
					if (!cur)
						continue;
#endif
				}
				if (children[i]->core.being_destroyed
				    || strcmp(XtName(children[i]), cur->name))
					continue;
				update_one_menu_entry(instance, children[i],
						      cur, deep_p);
				cur = cur->next;
			}
			XtFree((char *)children);
		}
		if (cur)
			abort();
	}
}

#endif				/* LWLIB_MENUBARS_MOTIF */

/* update text widgets */

static void
xm_update_text(widget_instance * instance, Widget widget, widget_value * val)
{
	XmTextSetString(widget, val->value ? val->value : (char *)"");
	XtRemoveAllCallbacks(widget, XmNactivateCallback);
	XtAddCallback(widget, XmNactivateCallback, xm_generic_callback,
		      instance);
	XtRemoveAllCallbacks(widget, XmNvalueChangedCallback);
	XtAddCallback(widget, XmNvalueChangedCallback,
		      xm_internal_update_other_instances, instance);
}

static void
xm_update_text_field(widget_instance * instance, Widget widget,
		     widget_value * val)
{
	XmTextFieldSetString(widget, val->value ? val->value : (char *)"");
	XtRemoveAllCallbacks(widget, XmNactivateCallback);
	XtAddCallback(widget, XmNactivateCallback, xm_generic_callback,
		      instance);
	XtRemoveAllCallbacks(widget, XmNvalueChangedCallback);
	XtAddCallback(widget, XmNvalueChangedCallback,
		      xm_internal_update_other_instances, instance);
}

#ifdef LWLIB_SCROLLBARS_MOTIF

/*
 * If this function looks like it does a lot more work than it needs to,
 * you're right.  Blame the Motif scrollbar for not being smart about
 * updating its appearance.
 */
static void
xm_update_scrollbar(widget_instance * instance, Widget widget,
		    widget_value * val)
{
	if (val->scrollbar_data) {
		scrollbar_values *data = val->scrollbar_data;
		int widget_sliderSize, widget_val;
		int new_sliderSize, new_value;
		double percent;
		double h_water, l_water;
		Arg al[4];

		/* First size and position the scrollbar widget. */
		XtSetArg(al[0], XtNx, data->scrollbar_x);
		XtSetArg(al[1], XtNy, data->scrollbar_y);
		XtSetArg(al[2], XtNwidth, data->scrollbar_width);
		XtSetArg(al[3], XtNheight, data->scrollbar_height);
		XtSetValues(widget, al, 4);

		/* Now size the scrollbar's slider. */
		XtSetArg(al[0], XmNsliderSize, &widget_sliderSize);
		XtSetArg(al[1], XmNvalue, &widget_val);
		XtGetValues(widget, al, 2);

		percent = (double)data->slider_size /
		    (double)(data->maximum - data->minimum);
		new_sliderSize = (int)((double)(INT_MAX - 1) * percent);

		percent = (double)(data->slider_position - data->minimum) /
		    (double)(data->maximum - data->minimum);
		new_value = (int)((double)(INT_MAX - 1) * percent);

		if (new_sliderSize > (INT_MAX - 1))
			new_sliderSize = INT_MAX - 1;
		else if (new_sliderSize < 1)
			new_sliderSize = 1;

		if (new_value > (INT_MAX - new_sliderSize))
			new_value = INT_MAX - new_sliderSize;
		else if (new_value < 1)
			new_value = 1;

		h_water = 1.05;
		l_water = 0.95;
		if (new_sliderSize != widget_sliderSize
		    || new_value != widget_val) {
			int force = ((INT_MAX - widget_sliderSize - widget_val)
				     ? 0
				     : (INT_MAX - new_sliderSize - new_value));

			if (force
			    || (double)new_sliderSize <
			    (l_water * (double)widget_sliderSize)
			    || (double)new_sliderSize >
			    (h_water * (double)widget_sliderSize)
			    || (double)new_value <
			    (l_water * (double)widget_val)
			    || (double)new_value >
			    (h_water * (double)widget_val)) {
				XmScrollBarSetValues(widget, new_value,
						     new_sliderSize, 1, 1,
						     False);
			}
		}
	}
}

#endif				/* LWLIB_SCROLLBARS_MOTIF */

/* update a motif widget */

void
xm_update_one_widget(widget_instance * instance, Widget widget,
		     widget_value * val, Boolean deep_p)
{
	WidgetClass class;
	Arg al[20];
	int ac = 0;

	/* Mark as not edited */
	val->edited = False;

	/* Common to all widget types */
	XtSetArg(al[ac], XmNsensitive, val->enabled);
	ac++;
	XtSetArg(al[ac], XmNuserData, val->call_data);
	ac++;
	XtSetValues(widget, al, ac);

#if defined (LWLIB_DIALOGS_MOTIF) || defined (LWLIB_MENUBARS_MOTIF) || defined (LWLIB_WIDGETS_MOTIF)
	/* Common to all label like widgets */
	xm_safe_update_label(instance, widget, val);
#endif
	class = XtClass(widget);
	/* Class specific things */
	if (class == xmPushButtonWidgetClass ||
	    class == xmArrowButtonWidgetClass) {
		xm_update_pushbutton(instance, widget, val);
	}
#ifdef LWLIB_MENUBARS_MOTIF
	else if (class == xmCascadeButtonWidgetClass) {
		xm_update_cascadebutton(instance, widget, val);
	}
#endif
	else if (class == xmToggleButtonWidgetClass
		 || class == xmToggleButtonGadgetClass) {
		xm_update_toggle(instance, widget, val);
	} else if (class == xmRowColumnWidgetClass) {
		Boolean radiobox = 0;

		XtSetArg(al[0], XmNradioBehavior, &radiobox);
		XtGetValues(widget, al, 1);

		if (radiobox)
			xm_update_radiobox(instance, widget, val);
#ifdef LWLIB_MENUBARS_MOTIF
		else
			xm_update_menu(instance, widget, val, deep_p);
#endif
	} else if (class == xmTextWidgetClass) {
		xm_update_text(instance, widget, val);
	} else if (class == xmTextFieldWidgetClass) {
		xm_update_text_field(instance, widget, val);
	} else if (class == xmListWidgetClass) {
		xm_update_list(instance, widget, val);
	}
#if defined (LWLIB_WIDGETS_MOTIF) && XmVERSION > 1
	else if (class == xmComboBoxWidgetClass) {
		xm_update_combo_box(instance, widget, val);
	}
#endif
#ifdef LWLIB_SCROLLBARS_MOTIF
	else if (class == xmScrollBarWidgetClass) {
		xm_update_scrollbar(instance, widget, val);
	}
#endif
#ifdef LWLIB_WIDGETS_MOTIF
	else if (class == xmScaleWidgetClass) {
		xm_update_progress(instance, widget, val);
	}
#endif
	/* Lastly update our global arg values. */
	if (val->args && val->args->nargs)
		XtSetValues(widget, val->args->args, val->args->nargs);
}

/* getting the value back */
void
xm_update_one_value(widget_instance * instance, Widget widget,
		    widget_value * val)
{
	WidgetClass class = XtClass(widget);
	widget_value *old_wv;

	/* copy the call_data slot into the "return" widget_value */
	for (old_wv = instance->info->val->contents; old_wv;
	     old_wv = old_wv->next)
		if (!strcmp(val->name, old_wv->name)) {
			val->call_data = old_wv->call_data;
			break;
		}

	if (class == xmToggleButtonWidgetClass
	    || class == xmToggleButtonGadgetClass) {
		Arg al[1];
		XtSetArg(al[0], XmNset, &val->selected);
		XtGetValues(widget, al, 1);
		val->edited = True;
	} else if (class == xmTextWidgetClass) {
		if (val->value)
			XtFree(val->value);
		val->value = XmTextGetString(widget);
		val->edited = True;
	} else if (class == xmTextFieldWidgetClass) {
		if (val->value)
			XtFree(val->value);
		val->value = XmTextFieldGetString(widget);
		val->edited = True;
	} else if (class == xmRowColumnWidgetClass) {
		Boolean radiobox = 0;
		{
			Arg al[1];
			XtSetArg(al[0], XmNradioBehavior, &radiobox);
			XtGetValues(widget, al, 1);
		}

		if (radiobox) {
			CompositeWidget radio = (CompositeWidget) widget;
			unsigned int i;
			for (i = 0; i < radio->composite.num_children; i++) {
				int set = False;
				Widget toggle = radio->composite.children[i];
				Arg al[1];

				XtSetArg(al[0], XmNset, &set);
				XtGetValues(toggle, al, 1);
				if (set) {
					if (val->value)
						free(val->value);
					val->value =
					    safe_strdup(XtName(toggle));
				}
			}
			val->edited = True;
		}
	} else if (class == xmListWidgetClass
#if defined (LWLIB_WIDGETS_MOTIF) && XmVERSION > 1
		   || class == xmComboBoxWidgetClass
#endif
	    ) {
		int pos_cnt;
		int *pos_list;
		Widget list = widget;
#if defined (LWLIB_WIDGETS_MOTIF) && XmVERSION > 1
		if (class == xmComboBoxWidgetClass)
			list = CB_List(widget);
#endif
		if (XmListGetSelectedPos(list, &pos_list, &pos_cnt)) {
			int i;
			widget_value *cur;
			for (cur = val->contents, i = 0; cur; cur = cur->next)
				if (cur->value) {
					int j;
					cur->selected = False;
					i += 1;
					for (j = 0; j < pos_cnt; j++)
						if (pos_list[j] == i) {
							cur->selected = True;
							val->value =
							    safe_strdup(cur->
									name);
						}
				}
			val->edited = 1;
			XtFree((char *)pos_list);
		}
	}
#ifdef LWLIB_SCROLLBARS_MOTIF
	else if (class == xmScrollBarWidgetClass) {
		/* This function is not used by the scrollbar. */
		return;
	}
#endif
}

/* This function is for activating a button from a program.  It's wrong because
   we pass a NULL argument in the call_data which is not Motif compatible.
   This is used from the XmNdefaultAction callback of the List widgets to
   have a double-click put down a dialog box like the button would do.
   I could not find a way to do that with accelerators.
 */
static void
activate_button(Widget widget, XtPointer closure, XtPointer call_data)
{
	Widget button = (Widget) closure;
	XtCallCallbacks(button, XmNactivateCallback, NULL);
}

/* creation functions */

#ifdef LWLIB_DIALOGS_MOTIF

/* dialogs */

#if (XmVersion >= 1002)
# define ARMANDACTIVATE_KLUDGE
# define DND_KLUDGE
#endif

#ifdef ARMANDACTIVATE_KLUDGE
 /* We want typing Return at a dialog box to select the default button; but
    we're satisfied with having it select the leftmost button instead.

    In Motif 1.1.5 we could do this by putting this resource in the
    app-defaults file:

    *dialog*button1.accelerators:#override\
    <KeyPress>Return: ArmAndActivate()\n\
    <KeyPress>KP_Enter: ArmAndActivate()\n\
    Ctrl<KeyPress>m: ArmAndActivate()\n

    but that doesn't work with 1.2.1 and I don't understand why. However,
    doing the equivalent C code does work, with the notable disadvantage that
    the user can't override it.  So that's what we do until we figure out
    something better....
  */
static char button_trans[] = "\
<KeyPress>Return: ArmAndActivate()\n\
<KeyPress>KP_Enter: ArmAndActivate()\n\
Ctrl<KeyPress>m: ArmAndActivate()\n";

#endif				/* ARMANDACTIVATE_KLUDGE */

#ifdef DND_KLUDGE
 /* This is a kludge to disable drag-and-drop in dialog boxes.  The symptom
    was a segv down in libXm somewhere if you used the middle button on a
    dialog box to begin a drag; when you released the button to make a drop
    things would lose if you were not over the button where you started the
    drag (canceling the operation).  This was probably due to the fact that
    the dialog boxes were not set up to handle a drag but were trying to do
    so anyway for some reason.

    So we disable drag-and-drop in dialog boxes by turning off the binding for
    Btn2Down which, by default, initiates a drag.  Clearly this is a shitty
    solution as it only works in default configurations, but...
  */
static char disable_dnd_trans[] = "<Btn2Down>: ";
#endif				/* DND_KLUDGE */

static Widget
make_dialog(char *name, Widget parent, Boolean pop_up_p,
	    const char *shell_title, const char *icon_name,
	    Boolean text_input_slot, Boolean radio_box, Boolean list,
	    int left_buttons, int right_buttons)
{
	Widget result;
	Widget form;
	Widget row;
	Widget icon;
	Widget icon_separator;
	Widget message;
	Widget value = 0;
	Widget separator;
	Widget button = 0;
	Widget children[16];	/* for the final XtManageChildren */
	int n_children;
	Arg al[64];		/* Arg List */
	int ac;			/* Arg Count */
	int i;

#ifdef DND_KLUDGE
	XtTranslations dnd_override =
	    XtParseTranslationTable(disable_dnd_trans);
# define DO_DND_KLUDGE(widget) XtOverrideTranslations ((widget), dnd_override)
#else				/* ! DND_KLUDGE */
# define DO_DND_KLUDGE(widget)
#endif				/* ! DND_KLUDGE */

	if (pop_up_p) {
		ac = 0;
		XtSetArg(al[ac], XmNtitle, shell_title);
		ac++;
		XtSetArg(al[ac], XtNallowShellResize, True);
		ac++;
		XtSetArg(al[ac], XmNdeleteResponse, XmUNMAP);
		ac++;
		result = XmCreateDialogShell(parent, "dialog", al, ac);

		XtSetArg(al[ac], XmNautoUnmanage, FALSE);
		ac++;
		/*      XtSetArg(al[ac], XmNautoUnmanage, TRUE); ac++; *//* ####is this ok? */
		XtSetArg(al[ac], XmNnavigationType, XmTAB_GROUP);
		ac++;
		form = XmCreateForm(result, (char *)shell_title, al, ac);
	} else {
		ac = 0;
		XtSetArg(al[ac], XmNautoUnmanage, FALSE);
		ac++;
		XtSetArg(al[ac], XmNnavigationType, XmTAB_GROUP);
		ac++;
		form = XmCreateForm(parent, (char *)shell_title, al, ac);
		result = form;
	}

	ac = 0;
	XtSetArg(al[ac], XmNpacking, XmPACK_COLUMN);
	ac++;
	XtSetArg(al[ac], XmNorientation, XmVERTICAL);
	ac++;
	XtSetArg(al[ac], XmNnumColumns, left_buttons + right_buttons + 1);
	ac++;
	XtSetArg(al[ac], XmNmarginWidth, 0);
	ac++;
	XtSetArg(al[ac], XmNmarginHeight, 0);
	ac++;
	XtSetArg(al[ac], XmNspacing, 13);
	ac++;
	XtSetArg(al[ac], XmNadjustLast, False);
	ac++;
	XtSetArg(al[ac], XmNalignment, XmALIGNMENT_CENTER);
	ac++;
	XtSetArg(al[ac], XmNisAligned, True);
	ac++;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE);
	ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM);
	ac++;
	XtSetArg(al[ac], XmNbottomOffset, 13);
	ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM);
	ac++;
	XtSetArg(al[ac], XmNleftOffset, 13);
	ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM);
	ac++;
	XtSetArg(al[ac], XmNrightOffset, 13);
	ac++;
	row = XmCreateRowColumn(form, "row", al, ac);

	n_children = 0;
	for (i = 0; i < left_buttons; i++) {
		char button_name[16];
		int sz = snprintf(button_name, sizeof(button_name), "button%d", i + 1);
		assert(sz >= 0 && sz < sizeof(button_name));
		ac = 0;
		if (i == 0) {
			XtSetArg(al[ac], XmNhighlightThickness, 1);
			ac++;
			XtSetArg(al[ac], XmNshowAsDefault, TRUE);
			ac++;
		}
		XtSetArg(al[ac], XmNnavigationType, XmTAB_GROUP);
		ac++;
		children[n_children] =
		    XmCreatePushButton(row, button_name, al, ac);
		DO_DND_KLUDGE(children[n_children]);

		if (i == 0) {
			button = children[n_children];
			ac = 0;
			XtSetArg(al[ac], XmNdefaultButton, button);
			ac++;
			XtSetValues(row, al, ac);

#ifdef ARMANDACTIVATE_KLUDGE	/* See comment above */
			{
				XtTranslations losers =
				    XtParseTranslationTable(button_trans);
				XtOverrideTranslations(button, losers);
				XtFree((char *)losers);
			}
#endif				/* ARMANDACTIVATE_KLUDGE */
		}

		n_children++;
	}

	/* invisible separator button */
	ac = 0;
	XtSetArg(al[ac], XmNmappedWhenManaged, FALSE);
	ac++;
	children[n_children] = XmCreateLabel(row, "separator_button", al, ac);
	DO_DND_KLUDGE(children[n_children]);
	n_children++;

	for (i = 0; i < right_buttons; i++) {
		char button_name[16];
		int sz = snprintf(button_name, sizeof(button_name),
				  "button%d", left_buttons + i + 1);
		assert(sz >= 0 && sz < sizeof(button_name));
		ac = 0;
		XtSetArg(al[ac], XmNnavigationType, XmTAB_GROUP);
		ac++;
		children[n_children] =
		    XmCreatePushButton(row, button_name, al, ac);
		DO_DND_KLUDGE(children[n_children]);
		if (!button)
			button = children[n_children];
		n_children++;
	}

	XtManageChildren(children, n_children);

	ac = 0;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE);
	ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET);
	ac++;
	XtSetArg(al[ac], XmNbottomOffset, 13);
	ac++;
	XtSetArg(al[ac], XmNbottomWidget, row);
	ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM);
	ac++;
	XtSetArg(al[ac], XmNleftOffset, 0);
	ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM);
	ac++;
	XtSetArg(al[ac], XmNrightOffset, 0);
	ac++;
	separator = XmCreateSeparator(form, "", al, ac);

	ac = 0;
	XtSetArg(al[ac], XmNlabelType, XmPIXMAP);
	ac++;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM);
	ac++;
	XtSetArg(al[ac], XmNtopOffset, 13);
	ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_NONE);
	ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM);
	ac++;
	XtSetArg(al[ac], XmNleftOffset, 13);
	ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE);
	ac++;
	icon = XmCreateLabel(form, (char *)icon_name, al, ac);
	DO_DND_KLUDGE(icon);

	ac = 0;
	XtSetArg(al[ac], XmNmappedWhenManaged, FALSE);
	ac++;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET);
	ac++;
	XtSetArg(al[ac], XmNtopOffset, 6);
	ac++;
	XtSetArg(al[ac], XmNtopWidget, icon);
	ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET);
	ac++;
	XtSetArg(al[ac], XmNbottomOffset, 6);
	ac++;
	XtSetArg(al[ac], XmNbottomWidget, separator);
	ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_NONE);
	ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_NONE);
	ac++;
	icon_separator = XmCreateLabel(form, "", al, ac);
	DO_DND_KLUDGE(icon_separator);

	if (text_input_slot) {
		ac = 0;
		XtSetArg(al[ac], XmNcolumns, 50);
		ac++;
		XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE);
		ac++;
		XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET);
		ac++;
		XtSetArg(al[ac], XmNbottomOffset, 13);
		ac++;
		XtSetArg(al[ac], XmNbottomWidget, separator);
		ac++;
		XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET);
		ac++;
		XtSetArg(al[ac], XmNleftOffset, 13);
		ac++;
		XtSetArg(al[ac], XmNleftWidget, icon);
		ac++;
		XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM);
		ac++;
		XtSetArg(al[ac], XmNrightOffset, 13);
		ac++;
		value = XmCreateTextField(form, "value", al, ac);
		DO_DND_KLUDGE(value);
	} else if (radio_box) {
		Widget radio_butt;
		ac = 0;
		XtSetArg(al[ac], XmNmarginWidth, 0);
		ac++;
		XtSetArg(al[ac], XmNmarginHeight, 0);
		ac++;
		XtSetArg(al[ac], XmNspacing, 13);
		ac++;
		XtSetArg(al[ac], XmNalignment, XmALIGNMENT_CENTER);
		ac++;
		XtSetArg(al[ac], XmNorientation, XmHORIZONTAL);
		ac++;
		XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET);
		ac++;
		XtSetArg(al[ac], XmNbottomOffset, 13);
		ac++;
		XtSetArg(al[ac], XmNbottomWidget, separator);
		ac++;
		XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET);
		ac++;
		XtSetArg(al[ac], XmNleftOffset, 13);
		ac++;
		XtSetArg(al[ac], XmNleftWidget, icon);
		ac++;
		XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM);
		ac++;
		XtSetArg(al[ac], XmNrightOffset, 13);
		ac++;
		value = XmCreateRadioBox(form, "radiobutton1", al, ac);
		ac = 0;
		i = 0;
		radio_butt =
		    XmCreateToggleButtonGadget(value, "radio1", al, ac);
		children[i++] = radio_butt;
		radio_butt =
		    XmCreateToggleButtonGadget(value, "radio2", al, ac);
		children[i++] = radio_butt;
		radio_butt =
		    XmCreateToggleButtonGadget(value, "radio3", al, ac);
		children[i++] = radio_butt;
		XtManageChildren(children, i);
	} else if (list) {
		ac = 0;
		XtSetArg(al[ac], XmNvisibleItemCount, 5);
		ac++;
		XtSetArg(al[ac], XmNtopAttachment, XmATTACH_NONE);
		ac++;
		XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET);
		ac++;
		XtSetArg(al[ac], XmNbottomOffset, 13);
		ac++;
		XtSetArg(al[ac], XmNbottomWidget, separator);
		ac++;
		XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET);
		ac++;
		XtSetArg(al[ac], XmNleftOffset, 13);
		ac++;
		XtSetArg(al[ac], XmNleftWidget, icon);
		ac++;
		XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM);
		ac++;
		XtSetArg(al[ac], XmNrightOffset, 13);
		ac++;
		value = XmCreateScrolledList(form, "list", al, ac);

		/* this is the easiest way I found to have the double click in the
		   list activate the default button */
		XtAddCallback(value, XmNdefaultActionCallback, activate_button,
			      button);
	}
	/* else add nothing; it's a separator */

	ac = 0;
	XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING);
	ac++;
	XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM);
	ac++;
	XtSetArg(al[ac], XmNtopOffset, 13);
	ac++;
	XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET);
	ac++;
	XtSetArg(al[ac], XmNbottomOffset, 13);
	ac++;
	XtSetArg(al[ac], XmNbottomWidget,
		 text_input_slot || radio_box || list ? value : separator);
	ac++;
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET);
	ac++;
	XtSetArg(al[ac], XmNleftOffset, 13);
	ac++;
	XtSetArg(al[ac], XmNleftWidget, icon);
	ac++;
	XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM);
	ac++;
	XtSetArg(al[ac], XmNrightOffset, 13);
	ac++;
	message = XmCreateLabel(form, "message", al, ac);
	DO_DND_KLUDGE(message);

	if (list)
		XtManageChild(value);

	i = 0;
	children[i] = row;
	i++;
	children[i] = separator;
	i++;
	if (text_input_slot || radio_box) {
		children[i] = value;
		i++;
	}
	children[i] = message;
	i++;
	children[i] = icon;
	i++;
	children[i] = icon_separator;
	i++;
	XtManageChildren(children, i);

	if (text_input_slot || list) {
		XtInstallAccelerators(value, button);
		XmProcessTraversal(value, XmTRAVERSE_CURRENT);
	} else if (radio_box) {
		XtInstallAccelerators(form, button);
		XmProcessTraversal(value, XmTRAVERSE_CURRENT);
	}
	/* else we don' need no STEENKIN' assellerators. */

#ifdef DND_KLUDGE
	XtFree((char *)dnd_override);
#endif
#undef DO_DND_KLUDGE

	return result;
}

static destroyed_instance *find_matching_instance(widget_instance * instance)
{
	destroyed_instance *cur;
	destroyed_instance *prev;
	char *type = instance->info->type;
	char *name = instance->info->name;

	for (prev = NULL, cur = all_destroyed_instances;
	     cur; prev = cur, cur = cur->next) {
		if (!strcmp(cur->name, name)
		    && !strcmp(cur->type, type)
		    && cur->parent == instance->parent
		    && cur->pop_up_p == instance->pop_up_p) {
			if (prev)
				prev->next = cur->next;
			else
				all_destroyed_instances = cur->next;
			return cur;
		}
		/* do some cleanup */
		else if (!cur->widget) {
			if (prev)
				prev->next = cur->next;
			else
				all_destroyed_instances = cur->next;
			free_destroyed_instance(cur);
			cur = prev ? prev : all_destroyed_instances;
		}
	}
	return NULL;
}

static void recenter_widget(Widget widget)
{
	Widget parent = XtParent(widget);
	Screen *screen = XtScreen(widget);
	Dimension screen_width = WidthOfScreen(screen);
	Dimension screen_height = HeightOfScreen(screen);
	Dimension parent_width = 0;
	Dimension parent_height = 0;
	Dimension child_width = 0;
	Dimension child_height = 0;
	Position x;
	Position y;
	Arg al[2];

	XtSetArg(al[0], XtNwidth, &child_width);
	XtSetArg(al[1], XtNheight, &child_height);
	XtGetValues(widget, al, 2);

	XtSetArg(al[0], XtNwidth, &parent_width);
	XtSetArg(al[1], XtNheight, &parent_height);
	XtGetValues(parent, al, 2);

	x = (Position) ((parent_width - child_width) / 2);
	y = (Position) ((parent_height - child_height) / 2);

	XtTranslateCoords(parent, x, y, &x, &y);

	if ((Dimension) (x + child_width) > screen_width)
		x = screen_width - child_width;
	if (x < 0)
		x = 0;

	if ((Dimension) (y + child_height) > screen_height)
		y = screen_height - child_height;
	if (y < 0)
		y = 0;

	XtSetArg(al[0], XtNx, x);
	XtSetArg(al[1], XtNy, y);
	XtSetValues(widget, al, 2);
}

static Widget recycle_instance(destroyed_instance * instance)
{
	Widget widget = instance->widget;

	/* widget is NULL if the parent was destroyed. */
	if (widget) {
		Widget focus;
		Widget separator;

		/* Remove the destroy callback as the instance is not in the list
		   anymore */
		XtRemoveCallback(instance->parent, XtNdestroyCallback,
				 mark_dead_instance_destroyed,
				 (XtPointer) instance);

		/* Give the focus to the initial item */
		focus = XtNameToWidget(widget, "*value");
		if (!focus)
			focus = XtNameToWidget(widget, "*button1");
		if (focus)
			XmProcessTraversal(focus, XmTRAVERSE_CURRENT);

		/* shrink the separator label back to their original size */
		separator = XtNameToWidget(widget, "*separator_button");
		if (separator) {
			Arg al[2];
			XtSetArg(al[0], XtNwidth, 5);
			XtSetArg(al[1], XtNheight, 5);
			XtSetValues(separator, al, 2);
		}

		/* Center the dialog in its parent */
		recenter_widget(widget);
	}
	free_destroyed_instance(instance);
	return widget;
}

Widget xm_create_dialog(widget_instance * instance)
{
	char *name = instance->info->type;
	Widget parent = instance->parent;
	Widget widget;
	Boolean pop_up_p = instance->pop_up_p;
	const char *shell_name = 0;
	const char *icon_name = 0;
	Boolean text_input_slot = False;
	Boolean radio_box = False;
	Boolean list = False;
	int total_buttons;
	int left_buttons = 0;
	int right_buttons = 1;
	destroyed_instance *dead_one;

	/* try to find a widget to recycle */
	dead_one = find_matching_instance(instance);
	if (dead_one) {
		Widget recycled_widget = recycle_instance(dead_one);
		if (recycled_widget)
			return recycled_widget;
	}

	switch (name[0]) {
	case 'E':
	case 'e':
		icon_name = "dbox-error";
		shell_name = "Error";
		break;

	case 'I':
	case 'i':
		icon_name = "dbox-info";
		shell_name = "Information";
		break;

	case 'L':
	case 'l':
		list = True;
		icon_name = "dbox-question";
		shell_name = "Prompt";
		break;

	case 'P':
	case 'p':
		text_input_slot = True;
		icon_name = "dbox-question";
		shell_name = "Prompt";
		break;

	case 'Q':
	case 'q':
		icon_name = "dbox-question";
		shell_name = "Question";
		break;
	}

	total_buttons = name[1] - '0';

	if (name[3] == 'T' || name[3] == 't') {
		text_input_slot = False;
		radio_box = True;
	} else if (name[3])
		right_buttons = name[4] - '0';

	left_buttons = total_buttons - right_buttons;

	widget = make_dialog(name, parent, pop_up_p,
			     shell_name, icon_name, text_input_slot, radio_box,
			     list, left_buttons, right_buttons);

	XtAddCallback(widget, XmNpopdownCallback, xm_nosel_callback,
		      (XtPointer) instance);
	return widget;
}

#endif				/* LWLIB_DIALOGS_MOTIF */

#ifdef LWLIB_MENUBARS_MOTIF
static Widget make_menubar(widget_instance * instance)
{
	Arg al[10];
	int ac = 0;

	XtSetArg(al[ac], XmNmarginHeight, 0);
	ac++;
	XtSetArg(al[ac], XmNshadowThickness, 3);
	ac++;

	return XmCreateMenuBar(instance->parent, instance->info->name, al, ac);
}

static void remove_grabs(Widget shell, XtPointer closure, XtPointer call_data)
{
	Widget menu = (Widget) closure;
	XmRemoveFromPostFromList(menu, XtParent(XtParent((Widget) menu)));
}

static Widget make_popup_menu(widget_instance * instance)
{
	Widget parent = instance->parent;
	Window parent_window = parent->core.window;
	Widget result;

	/* sets the parent window to 0 to fool Motif into not generating a grab */
	parent->core.window = 0;
	result = XmCreatePopupMenu(parent, instance->info->name, NULL, 0);
	XtAddCallback(XtParent(result), XmNpopdownCallback, remove_grabs,
		      (XtPointer) result);
	parent->core.window = parent_window;
	return result;
}
#endif				/* LWLIB_MENUBARS_MOTIF */

#ifdef LWLIB_SCROLLBARS_MOTIF
static Widget make_scrollbar(widget_instance * instance, int vertical)
{
	Arg al[20];
	int ac = 0;
	static XtCallbackRec callbacks[2] =
	    { {xm_scrollbar_callback, NULL}, {NULL, NULL} };

	callbacks[0].closure = (XtPointer) instance;

	XtSetArg(al[ac], XmNminimum, 1);
	ac++;
	XtSetArg(al[ac], XmNmaximum, INT_MAX);
	ac++;
	XtSetArg(al[ac], XmNincrement, 1);
	ac++;
	XtSetArg(al[ac], XmNpageIncrement, 1);
	ac++;
	XtSetArg(al[ac], XmNborderWidth, 0);
	ac++;
	XtSetArg(al[ac], XmNorientation, vertical ? XmVERTICAL : XmHORIZONTAL);
	ac++;

	XtSetArg(al[ac], XmNdecrementCallback, callbacks);
	ac++;
	XtSetArg(al[ac], XmNdragCallback, callbacks);
	ac++;
	XtSetArg(al[ac], XmNincrementCallback, callbacks);
	ac++;
	XtSetArg(al[ac], XmNpageDecrementCallback, callbacks);
	ac++;
	XtSetArg(al[ac], XmNpageIncrementCallback, callbacks);
	ac++;
	XtSetArg(al[ac], XmNtoBottomCallback, callbacks);
	ac++;
	XtSetArg(al[ac], XmNtoTopCallback, callbacks);
	ac++;
	XtSetArg(al[ac], XmNvalueChangedCallback, callbacks);
	ac++;

	return XmCreateScrollBar(instance->parent, instance->info->name, al,
				 ac);
}

static Widget make_vertical_scrollbar(widget_instance * instance)
{
	return make_scrollbar(instance, 1);
}

static Widget make_horizontal_scrollbar(widget_instance * instance)
{
	return make_scrollbar(instance, 0);
}

#endif				/* LWLIB_SCROLLBARS_MOTIF */

#ifdef LWLIB_WIDGETS_MOTIF
/* glyph widgets */
static Widget xm_create_button(widget_instance * instance)
{
	Arg al[20];
	int ac = 0;
	Widget button = 0;
	widget_value *val = instance->info->val;

	XtSetArg(al[ac], XmNsensitive, val->enabled);
	ac++;
	XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING);
	ac++;
	XtSetArg(al[ac], XmNuserData, val->call_data);
	ac++;
	XtSetArg(al[ac], XmNmappedWhenManaged, FALSE);
	ac++;
	/* The highlight doesn't appear to be dynamically set which makes it
	   look ugly.  I think this may be a LessTif bug but for now we just
	   get rid of it. */
	XtSetArg(al[ac], XmNhighlightThickness, (Dimension) 0);
	ac++;

	/* add any args the user supplied for creation time */
	lw_add_value_args_to_args(val, al, &ac);

	if (!val->call_data)
		button = XmCreateLabel(instance->parent, val->name, al, ac);

	else if (val->type == TOGGLE_TYPE || val->type == RADIO_TYPE) {
		XtSetArg(al[ac], XmNset, val->selected);
		ac++;
		XtSetArg(al[ac], XmNindicatorType,
			 (val->type == TOGGLE_TYPE ?
			  XmN_OF_MANY : XmONE_OF_MANY));
		ac++;
		XtSetArg(al[ac], XmNvisibleWhenOff, True);
		ac++;
		button =
		    XmCreateToggleButton(instance->parent, val->name, al, ac);
		XtRemoveAllCallbacks(button, XmNvalueChangedCallback);
		XtAddCallback(button, XmNvalueChangedCallback,
			      xm_generic_callback, (XtPointer) instance);
	} else {
		button =
		    XmCreatePushButton(instance->parent, val->name, al, ac);
		XtAddCallback(button, XmNactivateCallback, xm_generic_callback,
			      (XtPointer) instance);
	}

	XtManageChild(button);

	return button;
}

static Widget xm_create_progress(widget_instance * instance)
{
	Arg al[20];
	int ac = 0;
	Dimension height = 0;
	Dimension width = 0;
	Widget scale = 0;
	widget_value *val = instance->info->val;
	if (!val->call_data) {
		XtSetArg(al[ac], XmNeditable, False);
		ac++;
	} else {
		XtSetArg(al[ac], XmNeditable, val->enabled);
		ac++;
	}
	XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING);
	ac++;
	XtSetArg(al[ac], XmNuserData, val->call_data);
	ac++;
	XtSetArg(al[ac], XmNmappedWhenManaged, FALSE);
	ac++;
	XtSetArg(al[ac], XmNorientation, XmHORIZONTAL);
	ac++;
	/* The highlight doesn't appear to be dynamically set which makes it
	   look ugly.  I think this may be a LessTif bug but for now we just
	   get rid of it. */
	XtSetArg(al[ac], XmNhighlightThickness, (Dimension) 0);
	ac++;

	height = (Dimension) lw_get_value_arg(val, XtNheight);
	width = (Dimension) lw_get_value_arg(val, XtNwidth);
	if (height > 0) {
		XtSetArg(al[ac], XmNscaleHeight, height);
		ac++;
	}
	if (width > 0) {
		XtSetArg(al[ac], XmNscaleWidth, width);
		ac++;
	}

	/* add any args the user supplied for creation time */
	lw_add_value_args_to_args(val, al, &ac);

	scale = XmCreateScale(instance->parent, val->name, al, ac);
	if (val->call_data)
		XtAddCallback(scale, XmNvalueChangedCallback,
			      xm_generic_callback, (XtPointer) instance);

	XtManageChild(scale);

	return scale;
}

static Widget xm_create_text_field(widget_instance * instance)
{
	Arg al[20];
	int ac = 0;
	Widget text = 0;
	widget_value *val = instance->info->val;

	XtSetArg(al[ac], XmNsensitive, val->enabled);
	ac++;
	XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING);
	ac++;
	XtSetArg(al[ac], XmNuserData, val->call_data);
	ac++;
	XtSetArg(al[ac], XmNmappedWhenManaged, FALSE);
	ac++;
	/* The highlight doesn't appear to be dynamically set which makes it
	   look ugly.  I think this may be a LessTif bug but for now we just
	   get rid of it. */
	XtSetArg(al[ac], XmNhighlightThickness, (Dimension) 0);
	ac++;

	/* add any args the user supplied for creation time */
	lw_add_value_args_to_args(val, al, &ac);

	text = XmCreateTextField(instance->parent, val->name, al, ac);
	if (val->call_data)
		XtAddCallback(text, XmNvalueChangedCallback,
			      xm_generic_callback, (XtPointer) instance);

	XtManageChild(text);

	return text;
}

static Widget xm_create_label_field(widget_instance * instance)
{
	return xm_create_label(instance->parent, instance->info->val);
}

Widget xm_create_label(Widget parent, widget_value * val)
{
	Arg al[20];
	int ac = 0;
	Widget label = 0;

	XtSetArg(al[ac], XmNsensitive, val->enabled);
	ac++;
	XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING);
	ac++;
	XtSetArg(al[ac], XmNmappedWhenManaged, FALSE);
	ac++;
	/* The highlight doesn't appear to be dynamically set which makes it
	   look ugly.  I think this may be a LessTif bug but for now we just
	   get rid of it. */
	XtSetArg(al[ac], XmNhighlightThickness, (Dimension) 0);
	ac++;

	/* add any args the user supplied for creation time */
	lw_add_value_args_to_args(val, al, &ac);

	label = XmCreateLabel(parent, val->name, al, ac);

	XtManageChild(label);

	/* Do it again for arguments that have no effect until the widget is realized. */
	ac = 0;
	lw_add_value_args_to_args(val, al, &ac);
	XtSetValues(label, al, ac);

	return label;
}

#if XmVERSION > 1
static Widget xm_create_combo_box(widget_instance * instance)
{
	Arg al[20];
	int ac = 0;
	Widget combo = 0;
	widget_value *val = instance->info->val;

	XtSetArg(al[ac], XmNsensitive, val->enabled);
	ac++;
	XtSetArg(al[ac], XmNalignment, XmALIGNMENT_BEGINNING);
	ac++;
	XtSetArg(al[ac], XmNuserData, val->call_data);
	ac++;
	XtSetArg(al[ac], XmNmappedWhenManaged, FALSE);
	ac++;
	/* The highlight doesn't appear to be dynamically set which makes it
	   look ugly.  I think this may be a LessTif bug but for now we just
	   get rid of it. */
	XtSetArg(al[ac], XmNhighlightThickness, (Dimension) 0);
	ac++;

	/* add any args the user supplied for creation time */
	lw_add_value_args_to_args(val, al, &ac);

	combo = XmCreateDropDownComboBox(instance->parent, val->name, al, ac);
	if (val->call_data)
		XtAddCallback(combo, XmNselectionCallback, xm_generic_callback,
			      (XtPointer) instance);

	XtManageChild(combo);

	return combo;
}
#endif
#endif				/* LWLIB_WIDGETS_MOTIF */

/* Table of functions to create widgets */

const widget_creation_entry xm_creation_table[] = {
#ifdef LWLIB_MENUBARS_MOTIF
	{"menubar", make_menubar},
	{"popup", make_popup_menu},
#endif
#ifdef LWLIB_SCROLLBARS_MOTIF
	{"vertical-scrollbar", make_vertical_scrollbar},
	{"horizontal-scrollbar", make_horizontal_scrollbar},
#endif
#ifdef LWLIB_WIDGETS_MOTIF
	{"button", xm_create_button},
	{"progress", xm_create_progress},
	{"text-field", xm_create_text_field},
	{"label", xm_create_label_field},
#if XmVERSION > 1
	{"combo-box", xm_create_combo_box},
#endif
#endif
	{NULL, NULL}
};

/* Destruction of instances */
void xm_destroy_instance(widget_instance * instance)
{
#if defined (LWLIB_DIALOGS_MOTIF) || defined (LWLIB_WIDGETS_MOTIF)
	/* It appears that this is used only for dialog boxes. */
	Widget widget = instance->widget;
	/* recycle the dialog boxes */
	/* Disable the recycling until we can find a way to have the dialog box
	   get reasonable layout after we modify its contents. */
	if (0 && XtClass(widget) == xmDialogShellWidgetClass) {
		destroyed_instance *dead_instance =
		    make_destroyed_instance(instance->info->name,
					    instance->info->type,
					    instance->widget,
					    instance->parent,
					    instance->pop_up_p);
		dead_instance->next = all_destroyed_instances;
		all_destroyed_instances = dead_instance;
		XtUnmanageChild(first_child(instance->widget));
		XFlush(XtDisplay(instance->widget));
		XtAddCallback(instance->parent, XtNdestroyCallback,
			      mark_dead_instance_destroyed,
			      (XtPointer) dead_instance);
	} else {
		/* This might not be necessary now that the nosel is attached to
		   popdown instead of destroy, but it can't hurt. */
		XtRemoveCallback(instance->widget, XtNdestroyCallback,
				 xm_nosel_callback, (XtPointer) instance);

		XtDestroyWidget(instance->widget);
	}
#endif				/* LWLIB_DIALOGS_MOTIF || LWLIB_WIDGETS_MOTIF */
}

/* popup utility */
#ifdef LWLIB_MENUBARS_MOTIF

void xm_popup_menu(Widget widget, XEvent * event)
{
	if (event->type == ButtonPress || event->type == ButtonRelease) {
		/* This is so totally ridiculous: there's NO WAY to tell Motif
		   that *any* button can select a menu item.  Only one button
		   can have that honor.
		 */
		char *trans = 0;
		if (event->xbutton.state & Button5Mask)
			trans = "<Btn5Down>";
		else if (event->xbutton.state & Button4Mask)
			trans = "<Btn4Down>";
		else if (event->xbutton.state & Button3Mask)
			trans = "<Btn3Down>";
		else if (event->xbutton.state & Button2Mask)
			trans = "<Btn2Down>";
		else if (event->xbutton.state & Button1Mask)
			trans = "<Btn1Down>";
		if (trans) {
			Arg al[1];
			XtSetArg(al[0], XmNmenuPost, trans);
			XtSetValues(widget, al, 1);
		}
		XmMenuPosition(widget, (XButtonPressedEvent *) event);
	}
	XtManageChild(widget);
}

#endif

#ifdef LWLIB_DIALOGS_MOTIF

static void set_min_dialog_size(Widget w)
{
	short width;
	short height;
	Arg al[2];

	XtSetArg(al[0], XmNwidth, &width);
	XtSetArg(al[1], XmNheight, &height);
	XtGetValues(w, al, 2);

	XtSetArg(al[0], XmNminWidth, width);
	XtSetArg(al[1], XmNminHeight, height);
	XtSetValues(w, al, 2);
}

#endif

void xm_pop_instance(widget_instance * instance, Boolean up)
{
	Widget widget = instance->widget;

#ifdef LWLIB_DIALOGS_MOTIF
	if (XtClass(widget) == xmDialogShellWidgetClass) {
		Widget widget_to_manage = first_child(widget);
		if (up) {
			XtManageChild(widget_to_manage);
			set_min_dialog_size(widget);
			XmProcessTraversal(widget, XmTRAVERSE_CURRENT);
		} else
			XtUnmanageChild(widget_to_manage);
	} else
#endif
	{
		if (up)
			XtManageChild(widget);
		else
			XtUnmanageChild(widget);
	}
}

/* motif callback */

enum do_call_type { pre_activate, selection, no_selection, post_activate };

static void do_call(Widget widget, XtPointer closure, enum do_call_type type)
{
	XtPointer user_data;
	widget_instance *instance = (widget_instance *) closure;
	Widget instance_widget;
	LWLIB_ID id;
	Arg al[1];

	if (!instance)
		return;
	if (widget->core.being_destroyed)
		return;

	instance_widget = instance->widget;
	if (!instance_widget)
		return;

	id = instance->info->id;
	user_data = NULL;
	XtSetArg(al[0], XmNuserData, &user_data);
	XtGetValues(widget, al, 1);
	switch (type) {
	case pre_activate:
		if (instance->info->pre_activate_cb)
			instance->info->pre_activate_cb(widget, id, user_data);
		break;
	case selection:
		if (instance->info->selection_cb)
			instance->info->selection_cb(widget, id, user_data);
		break;
	case no_selection:
		if (instance->info->selection_cb)
			instance->info->selection_cb(widget, id,
						     (XtPointer) - 1);
		break;
	case post_activate:
		if (instance->info->post_activate_cb)
			instance->info->post_activate_cb(widget, id, user_data);
		break;
	default:
		abort();
	}
}

/* Like lw_internal_update_other_instances except that it does not do
   anything if its shell parent is not managed.  This is to protect
   lw_internal_update_other_instances to dereference freed memory
   if the widget was ``destroyed'' by caching it in the all_destroyed_instances
   list */
static void
xm_internal_update_other_instances(Widget widget, XtPointer closure,
				   XtPointer call_data)
{
	Widget parent;
	for (parent = widget; parent; parent = XtParent(parent))
		if (XtIsShell(parent))
			break;
		else if (!XtIsManaged(parent))
			return;
	lw_internal_update_other_instances(widget, closure, call_data);
}

static void
xm_generic_callback(Widget widget, XtPointer closure, XtPointer call_data)
{
#if (defined (LWLIB_MENUBARS_MOTIF) || defined (LWLIB_DIALOGS_MOTIF) || defined (LWLIB_WIDGETS_MOTIF))
	/* We want the selected status to change only when we decide it
	   should change.  Yuck but correct. */
	if (XtClass(widget) == xmToggleButtonWidgetClass
	    || XtClass(widget) == xmToggleButtonGadgetClass) {
		Boolean check;
		Arg al[1];

		XtSetArg(al[0], XmNset, &check);
		XtGetValues(widget, al, 1);

		XtSetArg(al[0], XmNset, !check);
		XtSetValues(widget, al, 1);
	}
#endif
	lw_internal_update_other_instances(widget, closure, call_data);
	do_call(widget, closure, selection);
}

static void
xm_pop_down_callback(Widget widget, XtPointer closure, XtPointer call_data)
{
	do_call(widget, closure, post_activate);
}

#ifdef LWLIB_MENUBARS_MOTIF

static void
xm_pull_down_callback(Widget widget, XtPointer closure, XtPointer call_data)
{
#if 0
	if (call_data) {
		/* new behavior for incremental menu construction */

	} else
#endif
		do_call(widget, closure, pre_activate);
}

#endif				/* LWLIB_MENUBARS_MOTIF */

#ifdef LWLIB_SCROLLBARS_MOTIF
static void
xm_scrollbar_callback(Widget widget, XtPointer closure, XtPointer call_data)
{
	widget_instance *instance = (widget_instance *) closure;
	LWLIB_ID id;
	XmScrollBarCallbackStruct *data =
	    (XmScrollBarCallbackStruct *) call_data;
	scroll_event event_data;
	scrollbar_values *val =
	    (scrollbar_values *) instance->info->val->scrollbar_data;
	double percent;

	if (!instance || widget->core.being_destroyed)
		return;

	id = instance->info->id;

	percent = (double)(data->value - 1) / (double)(INT_MAX - 1);
	event_data.slider_value =
	    (int)(percent * (double)(val->maximum - val->minimum)) +
	    val->minimum;

	if (event_data.slider_value > (val->maximum - val->slider_size))
		event_data.slider_value = val->maximum - val->slider_size;
	else if (event_data.slider_value < 1)
		event_data.slider_value = 1;

	if (data->event) {
		switch (data->event->xany.type) {
		case KeyPress:
		case KeyRelease:
			event_data.time = data->event->xkey.time;
			break;
		case ButtonPress:
		case ButtonRelease:
			event_data.time = data->event->xbutton.time;
			break;
		case MotionNotify:
			event_data.time = data->event->xmotion.time;
			break;
		case EnterNotify:
		case LeaveNotify:
			event_data.time = data->event->xcrossing.time;
			break;
		default:
			event_data.time = 0;
			break;
		}
	} else
		event_data.time = 0;

	switch (data->reason) {
	case XmCR_DECREMENT:
		event_data.action = SCROLLBAR_LINE_UP;
		break;
	case XmCR_INCREMENT:
		event_data.action = SCROLLBAR_LINE_DOWN;
		break;
	case XmCR_PAGE_DECREMENT:
		event_data.action = SCROLLBAR_PAGE_UP;
		break;
	case XmCR_PAGE_INCREMENT:
		event_data.action = SCROLLBAR_PAGE_DOWN;
		break;
	case XmCR_TO_TOP:
		event_data.action = SCROLLBAR_TOP;
		break;
	case XmCR_TO_BOTTOM:
		event_data.action = SCROLLBAR_BOTTOM;
		break;
	case XmCR_DRAG:
		event_data.action = SCROLLBAR_DRAG;
		break;
	case XmCR_VALUE_CHANGED:
		event_data.action = SCROLLBAR_CHANGE;
		break;
	default:
		event_data.action = SCROLLBAR_CHANGE;
		break;
	}

	if (instance->info->pre_activate_cb)
		instance->info->pre_activate_cb(widget, id,
						(XtPointer) & event_data);
}
#endif				/* LWLIB_SCROLLBARS_MOTIF */

#if defined (LWLIB_DIALOGS_MOTIF) || defined (LWLIB_WIDGETS_MOTIF)
static void
mark_dead_instance_destroyed(Widget widget, XtPointer closure,
			     XtPointer call_data)
{
	destroyed_instance *instance = (destroyed_instance *) closure;
	instance->widget = NULL;
}

static void
xm_nosel_callback(Widget widget, XtPointer closure, XtPointer call_data)
{
	/* This callback is only called when a dialog box is dismissed with the wm's
	   destroy button (WM_DELETE_WINDOW.)  We want the dialog box to be destroyed
	   in that case, not just unmapped, so that it releases its keyboard grabs.
	   But there are problems with running our callbacks while the widget is in
	   the process of being destroyed, so we set XmNdeleteResponse to XmUNMAP
	   instead of XmDESTROY and then destroy it ourself after having run the
	   callback.
	 */
	do_call(widget, closure, no_selection);
	XtDestroyWidget(widget);
}
#endif

/* set the keyboard focus */
void xm_set_keyboard_focus(Widget parent, Widget w)
{
	XmProcessTraversal(w, XmTRAVERSE_CURRENT);
	/* At some point we believed that it was necessary to use XtSetKeyboardFocus
	   instead of XmProcessTraversal when using Motif >= 1.2.1, but that's bogus.
	   Presumably the problem was elsewhere, and is now gone...
	 */
}
