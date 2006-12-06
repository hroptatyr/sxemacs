#ifndef INCLUDED_lwlib_Xaw_h_
#define INCLUDED_lwlib_Xaw_h_

#include "lwlib-internal.h"

extern const widget_creation_entry xaw_creation_table[];

Widget xaw_create_dialog(widget_instance * instance);

Widget xaw_create_label(Widget parent, widget_value * val);

Boolean lw_xaw_widget_p(Widget widget);

void
xaw_update_one_widget(widget_instance * instance, Widget widget,
		      widget_value * val, Boolean deep_p);

void
xaw_update_one_value(widget_instance * instance, Widget widget,
		     widget_value * val);

void xaw_destroy_instance(widget_instance * instance);

void xaw_popup_menu(Widget widget, XEvent * event);

void xaw_pop_instance(widget_instance * instance, Boolean up);

#endif				/* INCLUDED_lwlib_Xaw_h_ */
