/* Primitives for work of the "widget" library.
   Copyright (C) 1997 Free Software Foundation, Inc.

This file is part of SXEmacs

SXEmacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SXEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */


/* Synched up with: Not in FSF. */

/* In an ideal world, this file would not have been necessary.
   However, elisp function calls being as slow as they are, it turns
   out that some functions in the widget library (wid-edit.el) are the
   bottleneck of Widget operation.  Here is their translation to C,
   for the sole reason of efficiency.  */

#include <config.h>
#include "lisp.h"
#include "buffer.h"

Lisp_Object Qwidget_type;

DEFUN("widget-plist-member", Fwidget_plist_member, 2, 2, 0,	/*
Like `plist-get', but returns the tail of PLIST whose car is PROP.
*/
      (plist, prop))
{
	while (!NILP(plist) && !EQ(Fcar(plist), prop)) {
		/* Check for QUIT, so a circular plist doesn't lock up the
		   editor. */
		QUIT;
		plist = Fcdr(Fcdr(plist));
	}
	return plist;
}

DEFUN("widget-put", Fwidget_put, 3, 3, 0,	/*
In WIDGET set PROPERTY to VALUE.
The value can later be retrieved with `widget-get'.
*/
      (widget, property, value))
{
	CHECK_CONS(widget);
	XCDR(widget) = Fplist_put(XCDR(widget), property, value);
	return widget;
}

DEFUN("widget-get", Fwidget_get, 2, 2, 0,	/*
In WIDGET, get the value of PROPERTY.
The value could either be specified when the widget was created, or
later with `widget-put'.
*/
      (widget, property))
{
	Lisp_Object value = Qnil;

	while (1) {
		Lisp_Object tmp = Fwidget_plist_member(Fcdr(widget), property);
		if (!NILP(tmp)) {
			value = Fcar(Fcdr(tmp));
			break;
		}
		tmp = Fcar(widget);
		if (!NILP(tmp)) {
			widget = Fget(tmp, Qwidget_type, Qnil);
			continue;
		}
		break;
	}
	return value;
}

DEFUN("widget-apply", Fwidget_apply, 2, MANY, 0,	/*
Apply the value of WIDGET's PROPERTY to the widget itself.
ARGS are passed as extra arguments to the function.
*/
      (int nargs, Lisp_Object * args))
{
	/* This function can GC */
	Lisp_Object newargs[3];
	struct gcpro gcpro1;

	newargs[0] = Fwidget_get(args[0], args[1]);
	newargs[1] = args[0];
	newargs[2] = Flist(nargs - 2, args + 2);
	GCPRO1(newargs[2]);
	RETURN_UNGCPRO(Fapply(3, newargs));
}

void syms_of_widget(void)
{
	defsymbol(&Qwidget_type, "widget-type");

	DEFSUBR(Fwidget_plist_member);
	DEFSUBR(Fwidget_put);
	DEFSUBR(Fwidget_get);
	DEFSUBR(Fwidget_apply);
}
