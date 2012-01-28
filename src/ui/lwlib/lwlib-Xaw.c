/* The lwlib interface to Athena widgets.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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
#include <stdio.h>

#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#include "lwlib-Xaw.h"

#include <X11/StringDefs.h>
#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>
#include <X11/Shell.h>

#ifdef LWLIB_SCROLLBARS_ATHENA
#include ATHENA_Scrollbar_h_
#endif
#ifdef LWLIB_DIALOGS_ATHENA
#include ATHENA_Dialog_h_
#include ATHENA_Form_h_
#include ATHENA_Command_h_
#include ATHENA_Label_h_
#endif
#ifdef LWLIB_WIDGETS_ATHENA
#include ATHENA_Toggle_h_
#include "xlwradio.h"
#include "xlwcheckbox.h"
#include "xlwgauge.h"
#include ATHENA_AsciiText_h_
#endif
#include <X11/Xatom.h>

static void xaw_generic_callback(Widget, XtPointer, XtPointer);

Boolean lw_xaw_widget_p(Widget widget)
{
	return (0
#ifdef LWLIB_SCROLLBARS_ATHENA
		|| XtIsSubclass(widget, scrollbarWidgetClass)
#endif
#ifdef LWLIB_DIALOGS_ATHENA
		|| XtIsSubclass(widget, dialogWidgetClass)
#endif
#ifdef LWLIB_WIDGETS_ATHENA
		|| XtIsSubclass(widget, labelWidgetClass)
		|| XtIsSubclass(widget, toggleWidgetClass)
		|| XtIsSubclass(widget, gaugeWidgetClass)
#ifndef NEED_MOTIF
		|| XtIsSubclass(widget, asciiTextWidgetClass)
#endif
#endif
	    );
}

#ifdef LWLIB_SCROLLBARS_ATHENA
static void
xaw_update_scrollbar(widget_instance * instance, Widget widget,
		     widget_value * val)
{
	if (val->scrollbar_data) {
		scrollbar_values *data = val->scrollbar_data;
		float widget_shown, widget_topOfThumb;
		float new_shown, new_topOfThumb;
		Arg al[10];

		/* First size and position the scrollbar widget. */
		XtSetArg(al[0], XtNx, data->scrollbar_x);
		XtSetArg(al[1], XtNy, data->scrollbar_y);
		XtSetArg(al[2], XtNwidth, data->scrollbar_width);
		XtSetArg(al[3], XtNheight, data->scrollbar_height);
		XtSetValues(widget, al, 4);

		/* Now size the scrollbar's slider. */
		XtSetArg(al[0], XtNtopOfThumb, &widget_topOfThumb);
		XtSetArg(al[1], XtNshown, &widget_shown);
		XtGetValues(widget, al, 2);

		new_shown = (double)data->slider_size /
		    (double)(data->maximum - data->minimum);

		new_topOfThumb =
		    (double)(data->slider_position -
			     data->minimum) / (double)(data->maximum -
						       data->minimum);

		if (new_shown > 1.0)
			new_shown = 1.0;
		else if (new_shown < 0)
			new_shown = 0;

		if (new_topOfThumb > 1.0)
			new_topOfThumb = 1.0;
		else if (new_topOfThumb < 0)
			new_topOfThumb = 0;

		if (new_shown != widget_shown
		    || new_topOfThumb != widget_topOfThumb)
			XawScrollbarSetThumb(widget, new_topOfThumb, new_shown);
	}
}
#endif				/* LWLIB_SCROLLBARS_ATHENA */

void
xaw_update_one_widget(widget_instance * instance, Widget widget,
		      widget_value * val, Boolean deep_p)
{
	if (0) ;
#ifdef LWLIB_SCROLLBARS_ATHENA
	else if (XtIsSubclass(widget, scrollbarWidgetClass)) {
		xaw_update_scrollbar(instance, widget, val);
	}
#endif
#ifdef LWLIB_WIDGETS_ATHENA
#ifndef NEED_MOTIF
	else if (XtIsSubclass(widget, asciiTextWidgetClass)) {
	}
#endif
#endif
#ifdef LWLIB_DIALOGS_ATHENA
	else if (XtIsSubclass(widget, dialogWidgetClass)) {
		Arg al[1];
		XtSetArg(al[0], XtNlabel, val->contents->value);
		XtSetValues(widget, al, 1);
	}
#endif				/* LWLIB_DIALOGS_ATHENA */
#ifdef LWLIB_WIDGETS_ATHENA
	else if (XtClass(widget) == labelWidgetClass) {
		Arg al[1];
		XtSetArg(al[0], XtNlabel, val->value);
		XtSetValues(widget, al, 1);
	}
#endif				/* LWLIB_WIDGETS_ATHENA */
#if defined (LWLIB_DIALOGS_ATHENA) || defined (LWLIB_WIDGETS_ATHENA)
	else if (XtIsSubclass(widget, commandWidgetClass)) {
		Dimension bw = 0;
		Arg al[3];
		XtSetArg(al[0], XtNborderWidth, &bw);
		XtGetValues(widget, al, 1);

#ifndef LWLIB_DIALOGS_ATHENA3D
		if (bw == 0)
			/* Don't let buttons end up with 0 borderwidth, that's ugly...
			   Yeah, all this should really be done through app-defaults files
			   or fallback resources, but that's a whole different can of worms
			   that I don't feel like opening right now.  Making Athena widgets
			   not look like shit is just entirely too much work.
			 */
		{
			XtSetArg(al[0], XtNborderWidth, 1);
			XtSetValues(widget, al, 1);
		}
#endif				/* ! LWLIB_DIALOGS_ATHENA3D */

		lw_remove_accelerator_spec(val->value);
		XtSetArg(al[0], XtNlabel, val->value);
		XtSetArg(al[1], XtNsensitive, val->enabled);
		/* Force centered button text.  See above. */
		XtSetArg(al[2], XtNjustify, XtJustifyCenter);
		XtSetValues(widget, al, 3);

		XtRemoveAllCallbacks(widget, XtNcallback);
		XtAddCallback(widget, XtNcallback, xaw_generic_callback,
			      instance);
#ifdef LWLIB_WIDGETS_ATHENA
		/* set the selected state */
		if (XtIsSubclass(widget, toggleWidgetClass)) {
			XtSetArg(al[0], XtNstate, val->selected);
			XtSetValues(widget, al, 1);
		}
#endif				/* LWLIB_WIDGETS_ATHENA */
	}
#endif				/* LWLIB_DIALOGS_ATHENA */
	/* Lastly update our global arg values. */
	if (val->args && val->args->nargs)
		XtSetValues(widget, val->args->args, val->args->nargs);
}

void
xaw_update_one_value(widget_instance * instance, Widget widget,
		     widget_value * val)
{
#ifdef LWLIB_WIDGETS_ATHENA
	widget_value *old_wv;

	/* copy the call_data slot into the "return" widget_value */
	for (old_wv = instance->info->val->contents; old_wv;
	     old_wv = old_wv->next)
		if (!strcmp(val->name, old_wv->name)) {
			val->call_data = old_wv->call_data;
			break;
		}

	if (XtIsSubclass(widget, toggleWidgetClass)) {
		Arg al[1];
		XtSetArg(al[0], XtNstate, &val->selected);
		XtGetValues(widget, al, 1);
		val->edited = True;
	}
#ifndef NEED_MOTIF
	else if (XtIsSubclass(widget, asciiTextWidgetClass)) {
		Arg al[2];
		String buf = 0;
		XtSetArg(al[0], XtNstring, &buf);
		XtGetValues(widget, al, 1);

		if (val->value) {
			free(val->value);
			val->value = 0;
		}
		/* I don't think this causes a leak. */
		if (buf)
			val->value = strdup(buf);
		val->edited = True;
	}
#endif
#endif				/* LWLIB_WIDGETS_ATHENA */
}

void xaw_destroy_instance(widget_instance * instance)
{
#ifdef LWLIB_DIALOGS_ATHENA
	if (XtIsSubclass(instance->widget, dialogWidgetClass))
		/* Need to destroy the Shell too. */
		XtDestroyWidget(XtParent(instance->widget));
	else
#endif
		XtDestroyWidget(instance->widget);
}

void xaw_popup_menu(Widget widget, XEvent * event)
{
	/* An Athena menubar has not been implemented. */
	return;
}

void xaw_pop_instance(widget_instance * instance, Boolean up)
{
	Widget widget = instance->widget;

	if (up) {
#ifdef LWLIB_DIALOGS_ATHENA
		if (XtIsSubclass(widget, dialogWidgetClass)) {
			/* For dialogs, we need to call XtPopup on the parent instead
			   of calling XtManageChild on the widget.
			   Also we need to hack the shell's WM_PROTOCOLS to get it to
			   understand what the close box is supposed to do!!
			 */
			Display *dpy = XtDisplay(widget);
			Widget shell = XtParent(widget);
			Atom props[2];
			int i = 0;
			props[i++] =
			    XInternAtom(dpy, "WM_DELETE_WINDOW", False);
			XChangeProperty(dpy, XtWindow(shell),
					XInternAtom(dpy, "WM_PROTOCOLS", False),
					XA_ATOM, 32, PropModeAppend,
					(unsigned char *)props, i);

			/* Center the widget in its parent.  Why isn't this kind of crap
			   done automatically?  I thought toolkits were supposed to make
			   life easier?
			 */
			{
				unsigned int x, y, w, h;
				Widget topmost = instance->parent;
				w = shell->core.width;
				h = shell->core.height;
				while (topmost->core.parent &&
				       XtIsRealized(topmost->core.parent) &&
				       /* HAVE_SESSION adds an unmapped parent widget that
					  we should ignore here. */
				       topmost->core.parent->core.
				       mapped_when_managed)
					topmost = topmost->core.parent;
				if (topmost->core.width < w)
					x = topmost->core.x;
				else
					x = topmost->core.x +
					    ((topmost->core.width - w) / 2);
				if (topmost->core.height < h)
					y = topmost->core.y;
				else
					y = topmost->core.y +
					    ((topmost->core.height - h) / 2);
				XtMoveWidget(shell, x, y);
			}

			/* Finally, pop it up. */
			XtPopup(shell, XtGrabNonexclusive);
		} else
#endif				/* LWLIB_DIALOGS_ATHENA */
			XtManageChild(widget);
	} else {
#ifdef LWLIB_DIALOGS_ATHENA
		if (XtIsSubclass(widget, dialogWidgetClass))
			XtUnmanageChild(XtParent(widget));
		else
#endif
			XtUnmanageChild(widget);
	}
}

#ifdef LWLIB_DIALOGS_ATHENA
/* Dialog boxes */

static char overrideTrans[] = "<Message>WM_PROTOCOLS: lwlib_delete_dialog()";
static XtActionProc wm_delete_window(Widget shell, XtPointer closure,
				     XtPointer call_data);
static XtActionsRec xaw_actions[] = {
	{"lwlib_delete_dialog", (XtActionProc) wm_delete_window}
};
static Boolean actions_initted = False;

static Widget
make_dialog(const char *name, Widget parent, Boolean pop_up_p,
	    const char *shell_title, const char *icon_name,
	    Boolean text_input_slot,
	    Boolean radio_box, Boolean list,
	    int left_buttons, int right_buttons)
{
	Arg av[20];
	int ac = 0;
	int i, bc;
	char button_name[255];
	Widget shell;
	Widget dialog;
	Widget button;
	XtTranslations override;

	if (!pop_up_p)
		abort();	/* not implemented */
	if (text_input_slot)
		abort();	/* not implemented */
	if (radio_box)
		abort();	/* not implemented */
	if (list)
		abort();	/* not implemented */

	if (!actions_initted) {
		XtAppContext app = XtWidgetToApplicationContext(parent);
		XtAppAddActions(app, xaw_actions,
				sizeof(xaw_actions) / sizeof(xaw_actions[0]));
		actions_initted = True;
	}

	override = XtParseTranslationTable(overrideTrans);

	ac = 0;
	XtSetArg(av[ac], XtNtitle, shell_title);
	ac++;
	XtSetArg(av[ac], XtNallowShellResize, True);
	ac++;
	XtSetArg(av[ac], XtNtransientFor, parent);
	ac++;
	shell = XtCreatePopupShell("dialog", transientShellWidgetClass,
				   parent, av, ac);
	XtOverrideTranslations(shell, override);

	ac = 0;
	dialog = XtCreateManagedWidget(name, dialogWidgetClass, shell, av, ac);

	bc = 0;
	button = 0;
	for (i = 0; i < left_buttons; i++) {
		int sz;
		ac = 0;
		XtSetArg(av[ac], XtNfromHoriz, button);
		ac++;
		XtSetArg(av[ac], XtNleft, XtChainLeft);
		ac++;
		XtSetArg(av[ac], XtNright, XtChainLeft);
		ac++;
		XtSetArg(av[ac], XtNtop, XtChainBottom);
		ac++;
		XtSetArg(av[ac], XtNbottom, XtChainBottom);
		ac++;
		XtSetArg(av[ac], XtNresizable, True);
		ac++;
		sz = snprintf(button_name, sizeof(button_name), "button%d", ++bc);
		assert(sz >= 0 && (size_t)sz < sizeof(button_name));
		button = XtCreateManagedWidget(button_name, commandWidgetClass,
					       dialog, av, ac);
	}
	if (right_buttons) {
		/* Create a separator

		   I want the separator to take up the slack between the buttons on
		   the right and the buttons on the left (that is I want the buttons
		   after the separator to be packed against the right edge of the
		   window) but I can't seem to make it do it.
		 */
		ac = 0;
		XtSetArg(av[ac], XtNfromHoriz, button);
		ac++;
/*  XtSetArg (av [ac], XtNfromVert, XtNameToWidget (dialog, "label")); ac++; */
		XtSetArg(av[ac], XtNleft, XtChainLeft);
		ac++;
		XtSetArg(av[ac], XtNright, XtChainRight);
		ac++;
		XtSetArg(av[ac], XtNtop, XtChainBottom);
		ac++;
		XtSetArg(av[ac], XtNbottom, XtChainBottom);
		ac++;
		XtSetArg(av[ac], XtNlabel, "");
		ac++;
		XtSetArg(av[ac], XtNwidth, 30);
		ac++;		/* #### aaack!! */
		XtSetArg(av[ac], XtNborderWidth, 0);
		ac++;
		XtSetArg(av[ac], XtNshapeStyle, XmuShapeRectangle);
		ac++;
		XtSetArg(av[ac], XtNresizable, False);
		ac++;
		XtSetArg(av[ac], XtNsensitive, False);
		ac++;
		button = XtCreateManagedWidget("separator",
					       /* labelWidgetClass, */
					       /* This has to be Command to fake out
						  the Dialog widget... */
					       commandWidgetClass,
					       dialog, av, ac);
	}
	for (i = 0; i < right_buttons; i++) {
		int sz;
		ac = 0;
		XtSetArg(av[ac], XtNfromHoriz, button);
		ac++;
		XtSetArg(av[ac], XtNleft, XtChainRight);
		ac++;
		XtSetArg(av[ac], XtNright, XtChainRight);
		ac++;
		XtSetArg(av[ac], XtNtop, XtChainBottom);
		ac++;
		XtSetArg(av[ac], XtNbottom, XtChainBottom);
		ac++;
		XtSetArg(av[ac], XtNresizable, True);
		ac++;
		sz = snprintf(button_name, sizeof(button_name), "button%d", ++bc);
		assert(sz >= 0 && (size_t)sz < sizeof(button_name));
		button = XtCreateManagedWidget(button_name, commandWidgetClass,
					       dialog, av, ac);
	}

	return dialog;
}

Widget xaw_create_dialog(widget_instance * instance)
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
	default:
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

	return widget;
}
#endif				/* LWLIB_DIALOGS_ATHENA */

static void
xaw_generic_callback(Widget widget, XtPointer closure, XtPointer call_data)
{
	widget_instance *instance = (widget_instance *) closure;
	Widget instance_widget;
	LWLIB_ID id;
	XtPointer user_data = NULL;
#ifdef LWLIB_WIDGETS_ATHENA
	/* We want the selected status to change only when we decide it
	   should change.  Yuck but correct. */
	if (XtIsSubclass(widget, toggleWidgetClass)) {
		Boolean check;
		Arg al[1];

		XtSetArg(al[0], XtNstate, &check);
		XtGetValues(widget, al, 1);

		XtSetArg(al[0], XtNstate, !check);
		XtSetValues(widget, al, 1);
	}
#endif				/* LWLIB_WIDGETS_ATHENA */
	lw_internal_update_other_instances(widget, closure, call_data);

	if (!instance)
		return;
	if (widget->core.being_destroyed)
		return;

	instance_widget = instance->widget;
	if (!instance_widget)
		return;

	id = instance->info->id;

#if 0
	user_data = NULL;
	{
		Arg al[1];
		XtSetArg(al[0], XtNuserData, &user_data);
		XtGetValues(widget, al, 1);
	}
#else
	/* Damn!  Athena doesn't give us a way to hang our own data on the
	   buttons, so we have to go find it...  I guess this assumes that
	   all instances of a button have the same call data.

	   ... Which is a totally bogus assumption --andyp */
	{
		widget_value *val = instance->info->val;
		/* If the widget is a buffer/gutter widget then we already have
		   the one we are looking for, so don't try and descend the widget
		   tree. */
		if (val && val->contents) {
			char *name = XtName(widget);
			val = val->contents;
			while (val) {
				if (val->name && !strcmp(val->name, name))
					break;
				val = val->next;
			}
		}
		if (val)
			user_data = val->call_data;
		else
			abort();
	}
#endif

	if (instance->info->selection_cb && user_data)
		instance->info->selection_cb(widget, id, user_data);
}

#ifdef LWLIB_DIALOGS_ATHENA

static XtActionProc
wm_delete_window(Widget shell, XtPointer closure, XtPointer call_data)
{
	LWLIB_ID id;
	Widget *kids = 0;
	Widget widget;
	Arg al[1];
	if (!XtIsSubclass(shell, shellWidgetClass))
		abort();
	XtSetArg(al[0], XtNchildren, &kids);
	XtGetValues(shell, al, 1);
	if (!kids || !*kids)
		abort();
	else {
		widget = kids[0];
		if (!XtIsSubclass(widget, dialogWidgetClass))
			abort();
		id = lw_get_widget_id(widget);
		if (!id)
			abort();
		else {
			widget_info *info = lw_get_widget_info(id);
			if (!info)
				abort();
			else if (info->selection_cb)
				info->selection_cb(widget, id, (XtPointer) - 1);
		}

		lw_destroy_all_widgets(id);
	}
	return NULL;
}

#endif				/* LWLIB_DIALOGS_ATHENA */

/* Scrollbars */

#ifdef LWLIB_SCROLLBARS_ATHENA
static void
xaw_scrollbar_scroll(Widget widget, XtPointer closure, XtPointer call_data)
{
	widget_instance *instance = (widget_instance *) closure;
	LWLIB_ID id;
	scroll_event event_data;

	if (!instance || widget->core.being_destroyed)
		return;

	id = instance->info->id;
	event_data.slider_value = (int)call_data;
	event_data.time = 0;

	if ((int)call_data > 0)
		/* event_data.action = SCROLLBAR_PAGE_DOWN; */
		event_data.action = SCROLLBAR_LINE_DOWN;
	else
		/* event_data.action = SCROLLBAR_PAGE_UP; */
		event_data.action = SCROLLBAR_LINE_UP;

	if (instance->info->pre_activate_cb)
		instance->info->pre_activate_cb(widget, id,
						(XtPointer) & event_data);
}

static void
xaw_scrollbar_jump(Widget widget, XtPointer closure, XtPointer call_data)
{
	widget_instance *instance = (widget_instance *) closure;
	LWLIB_ID id;
	scroll_event event_data;
	scrollbar_values *val =
	    (scrollbar_values *) instance->info->val->scrollbar_data;
	float percent;

	if (!instance || widget->core.being_destroyed)
		return;

	id = instance->info->id;

	percent = *(float *)call_data;
	event_data.slider_value =
	    (int)(percent * (float)(val->maximum - val->minimum)) +
	    val->minimum;

	event_data.time = 0;
	event_data.action = SCROLLBAR_DRAG;

	if (instance->info->pre_activate_cb)
		instance->info->pre_activate_cb(widget, id,
						(XtPointer) & event_data);
}

static Widget xaw_create_scrollbar(widget_instance * instance, int vertical)
{
	Arg av[10];
	int ac = 0;

	static XtCallbackRec jumpCallbacks[2] =
	    { {xaw_scrollbar_jump, NULL}, {NULL, NULL} };

	static XtCallbackRec scrollCallbacks[2] =
	    { {xaw_scrollbar_scroll, NULL}, {NULL, NULL} };

	jumpCallbacks[0].closure = scrollCallbacks[0].closure =
	    (XtPointer) instance;

	/* #### This is tacked onto the with and height and completely
	   screws our geometry management.  We should probably make the
	   top-level aware of this so that people could have a border but so
	   few people use the Athena scrollbar now that it really isn't
	   worth the effort, at least not at the moment. */
	XtSetArg(av[ac], XtNborderWidth, 0);
	ac++;
	XtSetArg(av[ac], XtNorientation,
		 vertical ? XtorientVertical : XtorientHorizontal);
	ac++;
	XtSetArg(av[ac], "jumpProc", jumpCallbacks);
	ac++;
	XtSetArg(av[ac], "scrollProc", scrollCallbacks);
	ac++;

	return XtCreateWidget(instance->info->name, scrollbarWidgetClass,
			      instance->parent, av, ac);
}

static Widget xaw_create_vertical_scrollbar(widget_instance * instance)
{
	return xaw_create_scrollbar(instance, 1);
}

static Widget xaw_create_horizontal_scrollbar(widget_instance * instance)
{
	return xaw_create_scrollbar(instance, 0);
}
#endif				/* LWLIB_SCROLLBARS_ATHENA */

#ifdef LWLIB_WIDGETS_ATHENA
/* glyph widgets */
static Widget xaw_create_button(widget_instance * instance)
{
	Arg al[20];
	int ac = 0;
	Widget button = 0;
	widget_value *val = instance->info->val;

	XtSetArg(al[ac], XtNsensitive, val->enabled);
	ac++;
	XtSetArg(al[ac], XtNmappedWhenManaged, FALSE);
	ac++;
	XtSetArg(al[ac], XtNjustify, XtJustifyCenter);
	ac++;
	/* The highlight doesn't appear to be dynamically set which makes it
	   look ugly.  I think this may be a LessTif bug but for now we just
	   get rid of it. */
	XtSetArg(al[ac], XtNhighlightThickness, (Dimension) 0);
	ac++;

	/* add any args the user supplied for creation time */
	lw_add_value_args_to_args(val, al, &ac);

	if (!val->call_data)
		button = XtCreateManagedWidget(val->name, labelWidgetClass,
					       instance->parent, al, ac);

	else {
		if (val->type == TOGGLE_TYPE || val->type == RADIO_TYPE) {
			XtSetArg(al[ac], XtNstate, val->selected);
			ac++;
			button = XtCreateManagedWidget
			    (val->name,
			     val->type ==
			     TOGGLE_TYPE ? checkboxWidgetClass :
			     radioWidgetClass, instance->parent, al, ac);
		} else {
			button =
			    XtCreateManagedWidget(val->name, commandWidgetClass,
						  instance->parent, al, ac);
		}
		XtRemoveAllCallbacks(button, XtNcallback);
		XtAddCallback(button, XtNcallback, xaw_generic_callback,
			      (XtPointer) instance);
	}

	XtManageChild(button);

	return button;
}

static Widget xaw_create_label_field(widget_instance * instance)
{
	return xaw_create_label(instance->parent, instance->info->val);
}

Widget xaw_create_label(Widget parent, widget_value * val)
{
	Arg al[20];
	int ac = 0;
	Widget label = 0;

	XtSetArg(al[ac], XtNsensitive, val->enabled);
	ac++;
	XtSetArg(al[ac], XtNmappedWhenManaged, FALSE);
	ac++;
	XtSetArg(al[ac], XtNjustify, XtJustifyCenter);
	ac++;

	/* add any args the user supplied for creation time */
	lw_add_value_args_to_args(val, al, &ac);

	label = XtCreateManagedWidget(val->name, labelWidgetClass,
				      parent, al, ac);

	/* Do it again for arguments that have no effect until the widget is realized. */
	ac = 0;
	lw_add_value_args_to_args(val, al, &ac);
	if (ac > 20)
		abort();	/* #### need assert macro in lwlib */
	XtSetValues(label, al, ac);

	return label;
}

static Widget xaw_create_progress(widget_instance * instance)
{
	Arg al[20];
	int ac = 0;
	Widget scale = 0;
	widget_value *val = instance->info->val;
#if 0				/* This looks too awful, although more correct. */
	if (!val->call_data) {
		XtSetArg(al[ac], XtNsensitive, False);
		ac++;
	} else {
		XtSetArg(al[ac], XtNsensitive, val->enabled);
		ac++;
	}
#else
	XtSetArg(al[ac], XtNsensitive, True);
	ac++;
#endif

	XtSetArg(al[ac], XtNmappedWhenManaged, FALSE);
	ac++;
	XtSetArg(al[ac], XtNorientation, XtorientHorizontal);
	ac++;
	XtSetArg(al[ac], XtNhighlightThickness, (Dimension) 0);
	ac++;
	XtSetArg(al[ac], XtNntics, (Cardinal) 10);
	ac++;

	/* add any args the user supplied for creation time */
	lw_add_value_args_to_args(val, al, &ac);

	scale = XtCreateManagedWidget(val->name, gaugeWidgetClass,
				      instance->parent, al, ac);
	/* add the callback */
	if (val->call_data)
		XtAddCallback(scale, XtNgetValue, xaw_generic_callback,
			      (XtPointer) instance);

	XtManageChild(scale);

	return scale;
}

#if defined(LWLIB_WIDGETS_ATHENA)
#define TEXT_BUFFER_SIZE 128
static Widget xaw_create_text_field(widget_instance * instance)
{
	Arg al[20];
	int ac = 0;
	Widget text = 0;
	widget_value *val = instance->info->val;

	XtSetArg(al[ac], XtNsensitive, val->enabled);
	ac++;
	XtSetArg(al[ac], XtNmappedWhenManaged, FALSE);
	ac++;
	XtSetArg(al[ac], XtNhighlightThickness, (Dimension) 0);
	ac++;
	XtSetArg(al[ac], XtNtype, XawAsciiString);
	ac++;
	XtSetArg(al[ac], XtNeditType, XawtextEdit);
	ac++;
	XtSetArg(al[ac], XtNuseStringInPlace, False);
	ac++;
#if 0
	XtSetArg(al[ac], XtNlength, TEXT_BUFFER_SIZE);
	ac++;
#endif
	if (val->value) {
		XtSetArg(al[ac], XtNstring, val->value);
		ac++;
	}

	/* add any args the user supplied for creation time */
	lw_add_value_args_to_args(val, al, &ac);

	text = XtCreateManagedWidget(val->name, asciiTextWidgetClass,
				     instance->parent, al, ac);

	/* add the callback */
	if (val->call_data)
		XtAddCallback(text, XtNgetValue, xaw_generic_callback,
			      (XtPointer) instance);

	XtManageChild(text);

	return text;
}
#endif

#endif				/* LWLIB_WIDGETS_ATHENA */

const widget_creation_entry xaw_creation_table[] = {
#ifdef LWLIB_SCROLLBARS_ATHENA
	{"vertical-scrollbar", xaw_create_vertical_scrollbar},
	{"horizontal-scrollbar", xaw_create_horizontal_scrollbar},
#endif
#ifdef LWLIB_WIDGETS_ATHENA
	{"button", xaw_create_button},
	{"label", xaw_create_label_field},
	{"text-field", xaw_create_text_field},
	{"progress", xaw_create_progress},
#endif
	{NULL, NULL}
};
