/* Implements elisp-programmable dialog boxes -- generic.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
   Copyright (C) 2000 Ben Wing.

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

#include <config.h>
#include "lisp.h"

#include "frame.h"
#include "device.h"

Lisp_Object Vdelete_dialog_box_hook;
Lisp_Object Qdelete_dialog_box_hook;

DEFUN("make-dialog-box-internal", Fmake_dialog_box_internal, 2, 2, 0,	/*
Internal helper function for `make-dialog-box'.
This handles all dialog-box types except `general'.
TYPE is the same as the first argument to `make-dialog-box', and KEYS
a list of the remaining arguments.
*/
      (type, keys))
{
	struct frame *f = selected_frame();
	struct device *d = XDEVICE(f->device);

	CHECK_SYMBOL(type);

	if (!HAS_DEVMETH_P(d, make_dialog_box_internal))
		signal_type_error(Qunimplemented,
				  "Device does not support dialogs", f->device);

	return DEVMETH(d, make_dialog_box_internal, (f, type, keys));
}

void syms_of_dialog(void)
{
	DEFSUBR(Fmake_dialog_box_internal);

	DEFSYMBOL(Qdelete_dialog_box_hook);
}

void vars_of_dialog(void)
{
	Fprovide(intern("dialog"));

	DEFVAR_LISP("delete-dialog-box-hook", &Vdelete_dialog_box_hook	/*
Function or functions to call when a dialog box is about to be deleted.
One arg, the dialog box id.
									 */ );
	Vdelete_dialog_box_hook = Qnil;
}
