#ifndef INCLUDED_lwlib_Xm_h_
#define INCLUDED_lwlib_Xm_h_

#include "lwlib-internal.h"

extern const widget_creation_entry xm_creation_table[];

Widget xm_create_dialog(widget_instance * instance);

Widget xm_create_label(Widget parent, widget_value * val);

Boolean lw_motif_widget_p(Widget widget);

void
xm_update_one_widget(widget_instance * instance, Widget widget,
		     widget_value * val, Boolean deep_p);

void
xm_update_one_value(widget_instance * instance, Widget widget,
		    widget_value * val);

void xm_destroy_instance(widget_instance * instance);

void xm_set_keyboard_focus(Widget parent, Widget w);

void xm_popup_menu(Widget widget, XEvent * event);

void xm_pop_instance(widget_instance * instance, Boolean up);

extern Widget first_child(Widget);	/* garbage */

#endif				/* INCLUDED_lwlib_Xm_h_ */
