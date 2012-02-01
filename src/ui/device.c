/* Generic device functions.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.
   Copyright (C) 1995, 1996 Ben Wing

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

/* Original version by Chuck Thompson;
   rewritten and expanded by Ben Wing. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "console.h"
#include "device.h"
#include "elhash.h"
#include "events/events.h"
#include "faces.h"
#include "frame.h"
#include "keymap.h"
#include "redisplay.h"
#include "specifier.h"
#include "sysdep.h"
#include "window.h"

#ifdef HAVE_SCROLLBARS
#include "scrollbar.h"
#endif

#include "syssignal.h"

/* Vdefault_device is the firstly-created non-stream device that's still
   around.  We don't really use it anywhere currently, but it might
   be used for resourcing at some point.  (Currently we use
   Vdefault_x_device.) */
Lisp_Object Vdefault_device;

Lisp_Object Vcreate_device_hook, Vdelete_device_hook;

/* Device classes */
/* Qcolor defined in general.c */
Lisp_Object Qgrayscale, Qmono;

/* Device metrics symbols */
Lisp_Object
    Qcolor_default, Qcolor_select, Qcolor_balloon, Qcolor_3d_face,
    Qcolor_3d_light, Qcolor_3d_dark, Qcolor_menu, Qcolor_menu_highlight,
    Qcolor_menu_button, Qcolor_menu_disabled, Qcolor_toolbar,
    Qcolor_scrollbar, Qcolor_desktop, Qcolor_workspace, Qfont_default,
    Qfont_menubar, Qfont_dialog, Qsize_cursor, Qsize_scrollbar,
    Qsize_menu, Qsize_toolbar, Qsize_toolbar_button,
    Qsize_toolbar_border, Qsize_icon, Qsize_icon_small, Qsize_device,
    Qsize_workspace, Qoffset_workspace, Qsize_device_mm, Qdevice_dpi,
    Qnum_bit_planes, Qnum_color_cells, Qmouse_buttons, Qswap_buttons,
    Qshow_sounds, Qslow_device, Qsecurity;

Lisp_Object Qdevicep, Qdevice_live_p;
Lisp_Object Qcreate_device_hook;
Lisp_Object Qdelete_device_hook;
Lisp_Object Vdevice_class_list;

static Lisp_Object mark_device(Lisp_Object obj)
{
	struct device *d = XDEVICE(obj);

	mark_object(d->name);
	mark_object(d->connection);
	mark_object(d->canon_connection);
	mark_object(d->console);
	mark_object(d->selected_frame);
	mark_object(d->frame_with_focus_real);
	mark_object(d->frame_with_focus_for_hooks);
	mark_object(d->frame_that_ought_to_have_focus);
	mark_object(d->device_class);
	mark_object(d->user_defined_tags);
	mark_object(d->pixel_to_glyph_cache.obj1);
	mark_object(d->pixel_to_glyph_cache.obj2);

	mark_object(d->color_instance_cache);
	mark_object(d->font_instance_cache);
#ifdef MULE
	mark_object(d->charset_font_cache);
#endif
	mark_object(d->image_instance_cache);

	if (d->devmeths) {
		mark_object(d->devmeths->symbol);
		MAYBE_DEVMETH(d, mark_device, (d));
	}

	return (d->frame_list);
}

static void
print_device(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	struct device *d = XDEVICE(obj);

	if (print_readably)
		error("printing unreadable object #<device %s 0x%x>",
		      XSTRING_DATA(d->name), d->header.uid);

	write_fmt_string(printcharfun, "#<%s-device",
			 (!DEVICE_LIVE_P(d) ? "dead" :DEVICE_TYPE_NAME(d)));
	if (DEVICE_LIVE_P(d) && !NILP(DEVICE_CONNECTION(d))) {
		write_c_string(" on ", printcharfun);
		print_internal(DEVICE_CONNECTION(d), printcharfun, 1);
	}
	write_fmt_str(printcharfun, " 0x%x>", d->header.uid);
}

DEFINE_LRECORD_IMPLEMENTATION("device", device,
			      mark_device, print_device, 0, 0, 0, 0,
			      struct device);

int valid_device_class_p(Lisp_Object class)
{
	return !NILP(memq_no_quit(class, Vdevice_class_list));
}

DEFUN("valid-device-class-p", Fvalid_device_class_p, 1, 1, 0,	/*
Given a DEVICE-CLASS, return t if it is valid.
Valid classes are 'color, 'grayscale, and 'mono.
*/
      (device_class))
{
	return valid_device_class_p(device_class) ? Qt : Qnil;
}

DEFUN("device-class-list", Fdevice_class_list, 0, 0, 0,	/*
Return a list of valid device classes.
*/
      ())
{
	return Fcopy_sequence(Vdevice_class_list);
}

static struct device *allocate_device(Lisp_Object console)
{
	Lisp_Object device;
	struct device *d = alloc_lcrecord_type(struct device, &lrecord_device);
	struct gcpro gcpro1;

	zero_lcrecord(d);

	XSETDEVICE(device, d);
	GCPRO1(device);

	d->name = Qnil;
	d->console = console;
	d->connection = Qnil;
	d->canon_connection = Qnil;
	d->frame_list = Qnil;
	d->selected_frame = Qnil;
	d->frame_with_focus_real = Qnil;
	d->frame_with_focus_for_hooks = Qnil;
	d->frame_that_ought_to_have_focus = Qnil;
	d->device_class = Qnil;
	d->user_defined_tags = Qnil;
	d->pixel_to_glyph_cache.obj1 = Qnil;
	d->pixel_to_glyph_cache.obj2 = Qnil;

	d->infd = d->outfd = -1;

	/* #### is 20 reasonable? */
	d->color_instance_cache =
	    make_lisp_hash_table(20, HASH_TABLE_KEY_WEAK, HASH_TABLE_EQUAL);
	d->font_instance_cache =
	    make_lisp_hash_table(20, HASH_TABLE_KEY_WEAK, HASH_TABLE_EQUAL);
#ifdef MULE
	/* Note that the following table is bi-level. */
	d->charset_font_cache =
	    make_lisp_hash_table(20, HASH_TABLE_NON_WEAK, HASH_TABLE_EQ);
#endif
	/*
	   Note that the image instance cache is actually bi-level.
	   See device.h.  We use a low number here because most of the
	   time there aren't very many different masks that will be used.
	 */
	d->image_instance_cache =
	    make_lisp_hash_table(5, HASH_TABLE_NON_WEAK, HASH_TABLE_EQ);

	UNGCPRO;
	return d;
}

struct device *decode_device(Lisp_Object device)
{
	if (NILP(device))
		device = Fselected_device(Qnil);
	/* quietly accept frames for the device arg */
	else if (FRAMEP(device))
		device = FRAME_DEVICE(decode_frame(device));
	CHECK_LIVE_DEVICE(device);
	return XDEVICE(device);
}

DEFUN("dfw-device", Fdfw_device, 1, 1, 0,	/*
Given a device, frame, or window, return the associated device.
Return nil otherwise.
*/
      (object))
{
	return DFW_DEVICE(object);
}

DEFUN("selected-device", Fselected_device, 0, 1, 0,	/*
Return the device which is currently active.
If optional CONSOLE is non-nil, return the device that would be currently
active if CONSOLE were the selected console.
*/
      (console))
{
	if (NILP(console) && NILP(Vselected_console))
		return Qnil;	/* happens early in temacs */
	return CONSOLE_SELECTED_DEVICE(decode_console(console));
}

/* Called from selected_frame_1(), called from Fselect_window() */
void select_device_1(Lisp_Object device)
{
	struct device *dev = XDEVICE(device);
	Lisp_Object old_selected_device = Fselected_device(Qnil);

	if (EQ(device, old_selected_device))
		return;

	/* now select the device's console */
	CONSOLE_SELECTED_DEVICE(XCONSOLE(DEVICE_CONSOLE(dev))) = device;
	select_console_1(DEVICE_CONSOLE(dev));
}

DEFUN("select-device", Fselect_device, 1, 1, 0,	/*
Select the device DEVICE.
Subsequent editing commands apply to its console, selected frame,
and selected window.
The selection of DEVICE lasts until the next time the user does
something to select a different device, or until the next time this
function is called.
*/
      (device))
{
	CHECK_LIVE_DEVICE(device);

	/* select the device's selected frame's selected window.  This will call
	   selected_frame_1()->selected_device_1()->selected_console_1(). */
	if (!NILP(DEVICE_SELECTED_FRAME(XDEVICE(device))))
		Fselect_window(FRAME_SELECTED_WINDOW
			       (XFRAME(DEVICE_SELECTED_FRAME(XDEVICE(device)))),
			       Qnil);
	else
		error("Can't select a device with no frames");
	return Qnil;
}

void set_device_selected_frame(struct device *d, Lisp_Object frame)
{
	if (!NILP(frame) && !FRAME_MINIBUF_ONLY_P(XFRAME(frame)))
		set_console_last_nonminibuf_frame(XCONSOLE(DEVICE_CONSOLE(d)),
						  frame);
	d->selected_frame = frame;
}

DEFUN("set-device-selected-frame", Fset_device_selected_frame, 2, 2, 0,	/*
Set the selected frame of device object DEVICE to FRAME.
If DEVICE is nil, the selected device is used.
If DEVICE is the selected device, this makes FRAME the selected frame.
*/
      (device, frame))
{
	XSETDEVICE(device, decode_device(device));
	CHECK_LIVE_FRAME(frame);

	if (!EQ(device, FRAME_DEVICE(XFRAME(frame))))
		error("In `set-device-selected-frame', FRAME is not on DEVICE");

	if (EQ(device, Fselected_device(Qnil)))
		return Fselect_frame(frame);

	set_device_selected_frame(XDEVICE(device), frame);
	return frame;
}

DEFUN("devicep", Fdevicep, 1, 1, 0,	/*
Return non-nil if OBJECT is a device.
*/
      (object))
{
	return DEVICEP(object) ? Qt : Qnil;
}

DEFUN("device-live-p", Fdevice_live_p, 1, 1, 0,	/*
Return non-nil if OBJECT is a device that has not been deleted.
*/
      (object))
{
	return DEVICEP(object) && DEVICE_LIVE_P(XDEVICE(object)) ? Qt : Qnil;
}

DEFUN("device-name", Fdevice_name, 0, 1, 0,	/*
Return the name of the specified device.
DEVICE defaults to the selected device if omitted.
*/
      (device))
{
	return DEVICE_NAME(decode_device(device));
}

DEFUN("device-connection", Fdevice_connection, 0, 1, 0,	/*
Return the connection of the specified device.
DEVICE defaults to the selected device if omitted.
*/
      (device))
{
	return DEVICE_CONNECTION(decode_device(device));
}

DEFUN("device-console", Fdevice_console, 0, 1, 0,	/*
Return the console of the specified device.
DEVICE defaults to the selected device if omitted.
*/
      (device))
{
	return DEVICE_CONSOLE(decode_device(device));
}

#ifdef HAVE_WINDOW_SYSTEM

static void init_global_resources(struct device *d)
{
	init_global_faces(d);
#ifdef HAVE_SCROLLBARS
	init_global_scrollbars(d);
#endif
#ifdef HAVE_TOOLBARS
	init_global_toolbars(d);
#endif
}

#endif

static void init_device_resources(struct device *d)
{
	init_device_faces(d);
#ifdef HAVE_SCROLLBARS
	init_device_scrollbars(d);
#endif
#ifdef HAVE_TOOLBARS
	init_device_toolbars(d);
#endif
}

static Lisp_Object
semi_canonicalize_device_connection(struct console_methods *meths,
				    Lisp_Object name, Error_behavior errb)
{
	if (HAS_CONTYPE_METH_P(meths, semi_canonicalize_device_connection))
		return CONTYPE_METH(meths, semi_canonicalize_device_connection,
				    (name, errb));
	else
		return CONTYPE_METH_OR_GIVEN(meths,
					     canonicalize_device_connection,
					     (name, errb), name);
}

static Lisp_Object
canonicalize_device_connection(struct console_methods *meths,
			       Lisp_Object name, Error_behavior errb)
{
	if (HAS_CONTYPE_METH_P(meths, canonicalize_device_connection))
		return CONTYPE_METH(meths, canonicalize_device_connection,
				    (name, errb));
	else
		return CONTYPE_METH_OR_GIVEN(meths,
					     semi_canonicalize_device_connection,
					     (name, errb), name);
}

static Lisp_Object
find_device_of_type(struct console_methods *meths, Lisp_Object canon)
{
	Lisp_Object devcons, concons;

	DEVICE_LOOP_NO_BREAK(devcons, concons) {
		Lisp_Object device = XCAR(devcons);

		if (EQ(CONMETH_TYPE(meths), DEVICE_TYPE(XDEVICE(device)))
		    && internal_equal(DEVICE_CANON_CONNECTION(XDEVICE(device)),
				      canon, 0))
			return device;
	}

	return Qnil;
}

DEFUN("find-device", Ffind_device, 1, 2, 0,	/*
Look for an existing device attached to connection CONNECTION.
Return the device if found; otherwise, return nil.

If TYPE is specified, only return devices of that type; otherwise,
return devices of any type. (It is possible, although unlikely,
that two devices of different types could have the same connection
name; in such a case, the first device found is returned.)
*/
      (connection, type))
{
	Lisp_Object canon = Qnil;
	struct gcpro gcpro1;

	GCPRO1(canon);

	if (!NILP(type)) {
		struct console_methods *conmeths =
		    decode_console_type(type, ERROR_ME);
		canon =
		    canonicalize_device_connection(conmeths, connection,
						   ERROR_ME_NOT);
		if (UNBOUNDP(canon))
			RETURN_UNGCPRO(Qnil);

		RETURN_UNGCPRO(find_device_of_type(conmeths, canon));
	} else {
		int i;

		for (i = 0; i < Dynarr_length(the_console_type_entry_dynarr);
		     i++) {
			struct console_methods *conmeths =
			    Dynarr_at(the_console_type_entry_dynarr, i).meths;
			canon =
			    canonicalize_device_connection(conmeths, connection,
							   ERROR_ME_NOT);
			if (!UNBOUNDP(canon)) {
				Lisp_Object device =
				    find_device_of_type(conmeths, canon);
				if (!NILP(device))
					RETURN_UNGCPRO(device);
			}
		}

		RETURN_UNGCPRO(Qnil);
	}
}

DEFUN("get-device", Fget_device, 1, 2, 0,	/*
Look for an existing device attached to connection CONNECTION.
Return the device if found; otherwise, signal an error.

If TYPE is specified, only return devices of that type; otherwise,
return devices of any type. (It is possible, although unlikely,
that two devices of different types could have the same connection
name; in such a case, the first device found is returned.)
*/
      (connection, type))
{
	Lisp_Object device = Ffind_device(connection, type);
	if (NILP(device)) {
		if (NILP(type))
			signal_simple_error("No such device", connection);
		else
			signal_simple_error_2("No such device", type,
					      connection);
	}
	return device;
}

static Lisp_Object delete_deviceless_console(Lisp_Object console)
{
	if (NILP(XCONSOLE(console)->device_list))
		Fdelete_console(console, Qnil);
	return Qnil;
}

DEFUN("make-device", Fmake_device, 2, 3, 0,	/*
Return a new device of type TYPE, attached to connection CONNECTION.

The valid values for CONNECTION are device-specific; however,
CONNECTION is generally a string. (Specifically, for X devices,
CONNECTION should be a display specification such as "foo:0", and
for TTY devices, CONNECTION should be the filename of a TTY device
file, such as "/dev/ttyp4", or nil to refer to SXEmacs' standard
input/output.)

PROPS, if specified, should be a plist of properties controlling
device creation.

If CONNECTION specifies an already-existing device connection, that
device is simply returned; no new device is created, and PROPS
have no effect.
*/
      (type, connection, props))
{
	/* This function can GC */
	struct device *d;
	struct console *con;
	Lisp_Object device = Qnil;
	Lisp_Object console = Qnil;
	Lisp_Object name = Qnil;
	struct console_methods *conmeths;
	int speccount = specpdl_depth();

	struct gcpro gcpro1, gcpro2, gcpro3;
#ifdef HAVE_X_WINDOWS
	/* #### icky-poo.  If this is the first X device we are creating,
	   then retrieve the global face resources.  We have to do it
	   here, at the same time as (or just before) the device face
	   resources are retrieved; specifically, it needs to be done
	   after the device has been created but before any frames have
	   been popped up or much anything else has been done.  It's
	   possible for other devices to specify different global
	   resources (there's a property on each X server's root window
	   that holds some resources); tough luck for the moment.

	   This is a nasty violation of device independence, but
	   there's not a whole lot I can figure out to do about it.
	   The real problem is that the concept of resources is not
	   generalized away from X.  Similar resource-related
	   device-independence violations occur in faces.el. */
	int first_x_device = NILP(Vdefault_x_device) && EQ(type, Qx);
#endif

	GCPRO3(device, console, name);

	conmeths = decode_console_type(type, ERROR_ME_NOT);
	if (!conmeths)
		signal_simple_error("Invalid device type", type);

	device = Ffind_device(connection, type);
	if (!NILP(device))
		RETURN_UNGCPRO(device);

	name = Fplist_get(props, Qname, Qnil);

	{
		Lisp_Object conconnect =
		    (HAS_CONTYPE_METH_P(conmeths, device_to_console_connection))
		    ? CONTYPE_METH(conmeths, device_to_console_connection,
				   (connection, ERROR_ME)) : connection;
		console = create_console(name, type, conconnect, props);
	}

	record_unwind_protect(delete_deviceless_console, console);

	con = XCONSOLE(console);
	d = allocate_device(console);
	XSETDEVICE(device, d);

	d->devmeths = con->conmeths;

	DEVICE_NAME(d) = name;
	DEVICE_CONNECTION(d) =
	    semi_canonicalize_device_connection(conmeths, connection, ERROR_ME);
	DEVICE_CANON_CONNECTION(d) =
	    canonicalize_device_connection(conmeths, connection, ERROR_ME);

	MAYBE_DEVMETH(d, init_device, (d, props));

	/* Do it this way so that the device list is in order of creation */
	con->device_list = nconc2(con->device_list, Fcons(device, Qnil));
	RESET_CHANGED_SET_FLAGS;
	if (NILP(Vdefault_device) || DEVICE_STREAM_P(XDEVICE(Vdefault_device)))
		Vdefault_device = device;

#ifdef HAVE_X_WINDOWS
	if (first_x_device)
		init_global_resources(d);
#endif
	init_device_resources(d);

	MAYBE_DEVMETH(d, finish_init_device, (d, props));

	/* If this is the first device on the console, make it the selected one. */
	if (NILP(CONSOLE_SELECTED_DEVICE(con)))
		CONSOLE_SELECTED_DEVICE(con) = device;

	/* #### the following should trap errors. */
	setup_device_initial_specifier_tags(d);

	UNGCPRO;
	unbind_to(speccount, Qnil);
	return device;
}

/* find a device other than the selected one.  Prefer non-stream
   devices over stream devices.  Maybe stay on the same console. */

static Lisp_Object find_other_device(Lisp_Object device, int on_same_console)
{
	Lisp_Object devcons = Qnil, concons;
	Lisp_Object console = DEVICE_CONSOLE(XDEVICE(device));

	/* look for a non-stream device */
	DEVICE_LOOP_NO_BREAK(devcons, concons) {
		Lisp_Object dev = XCAR(devcons);
		if (on_same_console
		    && !EQ(console, DEVICE_CONSOLE(XDEVICE(dev))))
			continue;
		if (!DEVICE_STREAM_P(XDEVICE(dev)) && !EQ(dev, device) &&
		    !NILP(DEVICE_SELECTED_FRAME(XDEVICE(dev))))
			goto double_break_1;
	}

      double_break_1:
	if (!NILP(devcons))
		return XCAR(devcons);

	/* OK, now look for a stream device */
	DEVICE_LOOP_NO_BREAK(devcons, concons) {
		Lisp_Object dev = XCAR(devcons);
		if (on_same_console
		    && !EQ(console, DEVICE_CONSOLE(XDEVICE(dev))))
			continue;
		if (!EQ(dev, device)
		    && !NILP(DEVICE_SELECTED_FRAME(XDEVICE(dev))))
			goto double_break_2;
	}
      double_break_2:
	if (!NILP(devcons))
		return XCAR(devcons);

	/* Sorry, there ain't none */
	return Qnil;
}

static int
find_nonminibuffer_frame_not_on_device_predicate(Lisp_Object frame,
						 void *closure)
{
	Lisp_Object device;

	VOID_TO_LISP(device, closure);
	if (FRAME_MINIBUF_ONLY_P(XFRAME(frame)))
		return 0;
	if (EQ(device, FRAME_DEVICE(XFRAME(frame))))
		return 0;
	return 1;
}

Lisp_Object find_nonminibuffer_frame_not_on_device(Lisp_Object device)
{
	return find_some_frame(find_nonminibuffer_frame_not_on_device_predicate,
			       LISP_TO_VOID(device));
}

/* Delete device D.

   If FORCE is non-zero, allow deletion of the only frame.

   If CALLED_FROM_DELETE_CONSOLE is non-zero, then, if
   deleting the last device on a console, just delete it,
   instead of calling `delete-console'.

   If FROM_IO_ERROR is non-zero, then the device is gone due
   to an I/O error.  This affects what happens if we exit
   (we do an emergency exit instead of `save-buffers-kill-emacs'.)
*/

void
delete_device_internal(struct device *d, int force,
		       int called_from_delete_console, int from_io_error)
{
	/* This function can GC */
	struct console *c;
	Lisp_Object device;
	struct gcpro gcpro1;

	/* OK to delete an already-deleted device. */
	if (!DEVICE_LIVE_P(d))
		return;

	XSETDEVICE(device, d);
	GCPRO1(device);

	c = XCONSOLE(DEVICE_CONSOLE(d));

	if (!called_from_delete_console) {
		int delete_console = 0;
		/* If we're deleting the only device on the console,
		   delete the console. */
		if ((XINT(Flength(CONSOLE_DEVICE_LIST(c))) == 1)
		    /* if we just created the device, it might not be listed,
		       or something ... */
		    && !NILP(memq_no_quit(device, CONSOLE_DEVICE_LIST(c))))
			delete_console = 1;
		/* Or if there aren't any nonminibuffer frames that would be
		   left, delete the console (this will make SXEmacs exit). */
		else if (NILP(find_nonminibuffer_frame_not_on_device(device)))
			delete_console = 1;

		if (delete_console) {
			delete_console_internal(c, force, 0, from_io_error);
			UNGCPRO;
			return;
		}
	}

	reset_one_device(d);

	{
		Lisp_Object frmcons;

		/* First delete all frames without their own minibuffers,
		   to avoid errors coming from attempting to delete a frame
		   that is a surrogate for another frame. */
		DEVICE_FRAME_LOOP(frmcons, d) {
			struct frame *f = XFRAME(XCAR(frmcons));
			/* delete_frame_internal() might do anything such as run hooks,
			   so be defensive. */
			if (FRAME_LIVE_P(f) && !FRAME_HAS_MINIBUF_P(f))
				delete_frame_internal(f, 1, 1, from_io_error);

			if (!DEVICE_LIVE_P(d)) {	/* make sure the delete-*-hook didn't
							   go ahead and delete anything */
				UNGCPRO;
				return;
			}
		}

		/* #### This should probably be a device method but it is time for
		   19.14 to go out the door. */
		/* #### BILL!!! Should this deal with HAVE_MSWINDOWS as well? */
#ifdef HAVE_X_WINDOWS
		/* Next delete all frames which have the popup property to avoid
		   deleting a child after its parent. */
		DEVICE_FRAME_LOOP(frmcons, d) {
			struct frame *f = XFRAME(XCAR(frmcons));

			if (FRAME_LIVE_P(f)) {
				Lisp_Object popup =
				    Fframe_property(XCAR(frmcons), Qpopup,
						    Qnil);
				if (!NILP(popup))
					delete_frame_internal(f, 1, 1,
							      from_io_error);

				if (!DEVICE_LIVE_P(d)) {	/* make sure the delete-*-hook didn't
								   go ahead and delete anything */
					UNGCPRO;
					return;
				}
			}
		}
#endif				/* HAVE_X_WINDOWS */

		DEVICE_FRAME_LOOP(frmcons, d) {
			struct frame *f = XFRAME(XCAR(frmcons));
			/* delete_frame_internal() might do anything such as run hooks,
			   so be defensive. */
			if (FRAME_LIVE_P(f))
				delete_frame_internal(f, 1, 1, from_io_error);

			if (!DEVICE_LIVE_P(d)) {	/* make sure the delete-*-hook didn't
							   go ahead and delete anything */
				UNGCPRO;
				return;
			}
		}
	}

	set_device_selected_frame(d, Qnil);

	/* try to select another device */

	if (EQ(device, Fselected_device(DEVICE_CONSOLE(d)))) {
		Lisp_Object other_dev = find_other_device(device, 1);
		if (!NILP(other_dev))
			Fselect_device(other_dev);
	}

	if (EQ(device, Vdefault_device))
		Vdefault_device = find_other_device(device, 0);

	MAYBE_DEVMETH(d, delete_device, (d));

	CONSOLE_DEVICE_LIST(c) = delq_no_quit(device, CONSOLE_DEVICE_LIST(c));
	RESET_CHANGED_SET_FLAGS;
	d->devmeths = dead_console_methods;
	UNGCPRO;
}

/* delete a device as a result of an I/O error.  Called from
   an enqueued magic-eval event. */

void io_error_delete_device(Lisp_Object device)
{
	/* Note: it's the console that should get deleted, but
	   delete_device_internal() contains a hack that also deletes the
	   console when called from this function.  */
	delete_device_internal(XDEVICE(device), 1, 0, 1);
}

DEFUN("delete-device", Fdelete_device, 1, 2, 0,	/*
Delete DEVICE, permanently eliminating it from use.
Normally, you cannot delete the last non-minibuffer-only frame (you must
use `save-buffers-kill-emacs' or `kill-emacs').  However, if optional
second argument FORCE is non-nil, you can delete the last frame. (This
will automatically call `save-buffers-kill-emacs'.)
*/
      (device, force))
{
	CHECK_DEVICE(device);
	delete_device_internal(XDEVICE(device), !NILP(force), 0, 0);
	return Qnil;
}

DEFUN("device-frame-list", Fdevice_frame_list, 0, 1, 0,	/*
Return a list of all frames on DEVICE.
If DEVICE is nil, the selected device will be used.
*/
      (device))
{
	return Fcopy_sequence(DEVICE_FRAME_LIST(decode_device(device)));
}

DEFUN("device-class", Fdevice_class, 0, 1, 0,	/*
Return the class (color behavior) of DEVICE.
This will be one of 'color, 'grayscale, or 'mono.
*/
      (device))
{
	return DEVICE_CLASS(decode_device(device));
}

DEFUN("set-device-class", Fset_device_class, 2, 2, 0,	/*
Set the class (color behavior) of DEVICE.
CLASS should be one of 'color, 'grayscale, or 'mono.
This is only allowed on device such as TTY devices, where the color
behavior cannot necessarily be determined automatically.
*/
      (device, class))
{
	struct device *d = decode_device(device);
	XSETDEVICE(device, d);
	if (!DEVICE_TTY_P(d))
		signal_simple_error("Cannot change the class of this device",
				    device);
	if (!EQ(class, Qcolor) && !EQ(class, Qmono) && !EQ(class, Qgrayscale))
		signal_simple_error("Must be color, mono, or grayscale", class);
	if (!EQ(DEVICE_CLASS(d), class)) {
		Lisp_Object frmcons;
		DEVICE_CLASS(d) = class;
		DEVICE_FRAME_LOOP(frmcons, d) {
			struct frame *f = XFRAME(XCAR(frmcons));

			recompute_all_cached_specifiers_in_frame(f);
			MARK_FRAME_FACES_CHANGED(f);
			MARK_FRAME_GLYPHS_CHANGED(f);
			MARK_FRAME_SUBWINDOWS_CHANGED(f);
			MARK_FRAME_TOOLBARS_CHANGED(f);
			MARK_FRAME_GUTTERS_CHANGED(f);
			f->menubar_changed = 1;
		}
	}
	return Qnil;
}

DEFUN("set-device-baud-rate", Fset_device_baud_rate, 2, 2, 0,	/*
Set the output baud rate of DEVICE to RATE.
On most systems, changing this value will affect the amount of padding
and other strategic decisions made during redisplay.
*/
      (device, rate))
{
	CHECK_INT(rate);

	DEVICE_BAUD_RATE(decode_device(device)) = XINT(rate);

	return rate;
}

DEFUN("device-baud-rate", Fdevice_baud_rate, 0, 1, 0,	/*
Return the output baud rate of DEVICE.
*/
      (device))
{
	return make_int(DEVICE_BAUD_RATE(decode_device(device)));
}

DEFUN("device-printer-p", Fdevice_printer_p, 0, 1, 0,	/*
Return t if DEVICE is a printer, nil if it is a display. DEVICE defaults
to selected device if omitted, and must be live if specified.
*/
      (device))
{
	return DEVICE_PRINTER_P(decode_device(device)) ? Qt : Qnil;
}

DEFUN("device-system-metric", Fdevice_system_metric, 1, 3, 0,	/*
Get a metric for DEVICE as provided by the system.

METRIC must be a symbol specifying requested metric.  Note that the metrics
returned are these provided by the system internally, not read from resources,
so obtained from the most internal level.

If a metric is not provided by the system, then DEFAULT is returned.

When DEVICE is nil, selected device is assumed

Metrics, by group, are:

COLORS.  Colors are returned as valid color instantiators.  No other assumption
on the returned value should be made (i.e. it can be a string on one system but
a color instance on another).  For colors, returned value is a cons of
foreground and background colors.  Note that if the system provides only one
color of the pair, the second one may be nil.

color-default         Standard window text foreground and background.
color-select          Selection highlight text and background colors.
color-balloon         Balloon popup text and background colors.
color-3d-face         3-D object (button, modeline) text and surface colors.
color-3d-light        Fore and back colors for 3-D edges facing light source.
color-3d-dark         Fore and back colors for 3-D edges facing away from
light source.
color-menu            Text and background for menus
color-menu-highlight  Selected menu item colors
color-menu-button     Menu button colors
color-menu-disabled   Unselectable menu item colors
color-toolbar         Toolbar foreground and background colors
color-scrollbar       Scrollbar foreground and background colors
color-desktop         Desktop window colors
color-workspace       Workspace window colors

FONTS. Fonts are returned as valid font instantiators.  No other assumption on
the returned value should be made (i.e. it can be a string on one system but
font instance on another).

font-default          Default fixed width font.
font-menubar          Menubar font
font-dialog           Dialog boxes font

GEOMETRY. These metrics are returned as conses of (X . Y).  As with colors,
either car or cdr of the cons may be nil if the system does not provide one
of the corresponding dimensions.

size-cursor           Mouse cursor size.
size-scrollbar        Scrollbars (WIDTH . HEIGHT)
size-menu             Menubar height, as (nil . HEIGHT)
size-toolbar          Toolbar width and height.
size-toolbar-button   Toolbar button size.
size-toolbar-border   Toolbar border width and height.
size-icon             Icon dimensions.
size-icon-small       Small icon dimensions.
size-device           Device screen or paper size in pixels.
size-workspace        Workspace size in pixels. This can be less than or
equal to the above. For displays, this is the area
available to applications less window manager
decorations. For printers, this is the size of
printable area.
offset-workspace      Offset of workspace area from the top left corner
of screen or paper, in pixels.
size-device-mm        Device screen size in millimeters.
device-dpi            Device resolution, in dots per inch.
num-bit-planes        Integer, number of device bit planes.
num-color-cells       Integer, number of device color cells.

FEATURES.  This group reports various device features.  If a feature is
present, integer 1 (one) is returned, if it is not present, then integer
0 (zero) is returned.  If the system is unaware of the feature, then
DEFAULT is returned.

mouse-buttons         Integer, number of mouse buttons, or zero if no mouse.
swap-buttons          Non-zero if left and right mouse buttons are swapped.
show-sounds           User preference for visual over audible bell.
slow-device           Device is slow, avoid animation.
security              Non-zero if user environment is secure.
*/
      (device, metric, default_))
{
	struct device *d = decode_device(device);
	enum device_metrics m;
	Lisp_Object res;

	/* Decode metric */
#define FROB(met)				\
  else if (EQ (metric, Q##met))			\
    m = DM_##met

	if (0) ;
	FROB(color_default);
	FROB(color_select);
	FROB(color_balloon);
	FROB(color_3d_face);
	FROB(color_3d_light);
	FROB(color_3d_dark);
	FROB(color_menu);
	FROB(color_menu_highlight);
	FROB(color_menu_button);
	FROB(color_menu_disabled);
	FROB(color_toolbar);
	FROB(color_scrollbar);
	FROB(color_desktop);
	FROB(color_workspace);
	FROB(font_default);
	FROB(font_menubar);
	FROB(font_dialog);
	FROB(size_cursor);
	FROB(size_scrollbar);
	FROB(size_menu);
	FROB(size_toolbar);
	FROB(size_toolbar_button);
	FROB(size_toolbar_border);
	FROB(size_icon);
	FROB(size_icon_small);
	FROB(size_device);
	FROB(size_workspace);
	FROB(offset_workspace);
	FROB(size_device_mm);
	FROB(device_dpi);
	FROB(num_bit_planes);
	FROB(num_color_cells);
	FROB(mouse_buttons);
	FROB(swap_buttons);
	FROB(show_sounds);
	FROB(slow_device);
	FROB(security);
	else
	signal_simple_error("Invalid device metric symbol", metric);

	res = DEVMETH_OR_GIVEN(d, device_system_metrics, (d, m), Qunbound);
	return UNBOUNDP(res) ? default_ : res;

#undef FROB
}

DEFUN("device-system-metrics", Fdevice_system_metrics, 0, 1, 0,	/*
Get a property list of device metric for DEVICE.

See `device-system-metric' for the description of available metrics.
DEVICE defaults to selected device when omitted.
*/
      (device))
{
	struct device *d = decode_device(device);
	Lisp_Object plist = Qnil, one_metric;

#define FROB(m)								\
  if (!UNBOUNDP ((one_metric =						\
		  DEVMETH_OR_GIVEN (d, device_system_metrics,	\
				    (d, DM_##m), Qunbound))))		\
    plist = Fcons (Q##m, Fcons (one_metric, plist));

	FROB(color_default);
	FROB(color_select);
	FROB(color_balloon);
	FROB(color_3d_face);
	FROB(color_3d_light);
	FROB(color_3d_dark);
	FROB(color_menu);
	FROB(color_menu_highlight);
	FROB(color_menu_button);
	FROB(color_menu_disabled);
	FROB(color_toolbar);
	FROB(color_scrollbar);
	FROB(color_desktop);
	FROB(color_workspace);
	FROB(font_default);
	FROB(font_menubar);
	FROB(font_dialog);
	FROB(size_cursor);
	FROB(size_scrollbar);
	FROB(size_menu);
	FROB(size_toolbar);
	FROB(size_toolbar_button);
	FROB(size_toolbar_border);
	FROB(size_icon);
	FROB(size_icon_small);
	FROB(size_device);
	FROB(size_workspace);
	FROB(offset_workspace);
	FROB(size_device_mm);
	FROB(device_dpi);
	FROB(num_bit_planes);
	FROB(num_color_cells);
	FROB(mouse_buttons);
	FROB(swap_buttons);
	FROB(show_sounds);
	FROB(slow_device);
	FROB(security);

	return plist;

#undef FROB
}

Lisp_Object domain_device_type(Lisp_Object domain)
{
	/* This cannot GC */
	assert(WINDOWP(domain) || FRAMEP(domain)
	       || DEVICEP(domain) || CONSOLEP(domain));

	if (WINDOWP(domain)) {
		if (!WINDOW_LIVE_P(XWINDOW(domain)))
			return Qdead;
		domain = WINDOW_FRAME(XWINDOW(domain));
	}
	if (FRAMEP(domain)) {
		if (!FRAME_LIVE_P(XFRAME(domain)))
			return Qdead;
		domain = FRAME_DEVICE(XFRAME(domain));
	}
	if (DEVICEP(domain)) {
		if (!DEVICE_LIVE_P(XDEVICE(domain)))
			return Qdead;
		domain = DEVICE_CONSOLE(XDEVICE(domain));
	}
	return CONSOLE_TYPE(XCONSOLE(domain));
}

/*
 * Determine whether window system bases window geometry on character
 * or pixel counts.
 * Return non-zero for pixel-based geometry, zero for character-based.
 */
int window_system_pixelated_geometry(Lisp_Object domain)
{
	/* This cannot GC */
	Lisp_Object winsy = domain_device_type(domain);
	struct console_methods *meth = decode_console_type(winsy, ERROR_ME_NOT);
	assert(meth);
	return CONMETH_IMPL_FLAG(meth, XDEVIMPF_PIXEL_GEOMETRY);
}

DEFUN("domain-device-type", Fdomain_device_type, 0, 1, 0,	/*
Return the device type symbol for a DOMAIN, e.g. 'x or 'tty.
DOMAIN can be either a window, frame, device or console.
*/
      (domain))
{
	if (!WINDOWP(domain) && !FRAMEP(domain)
	    && !DEVICEP(domain) && !CONSOLEP(domain))
		signal_simple_error
		    ("Domain must be either a window, frame, device or console",
		     domain);

	return domain_device_type(domain);
}

void handle_asynch_device_change(void)
{
	int i;
	int old_asynch_device_change_pending = asynch_device_change_pending;
	for (i = 0; i < Dynarr_length(the_console_type_entry_dynarr); i++) {
		if (Dynarr_at(the_console_type_entry_dynarr, i).meths->
		    asynch_device_change_method)
			(Dynarr_at(the_console_type_entry_dynarr, i).meths->
			 asynch_device_change_method) ();
	}
	/* reset the flag to 0 unless another notification occurred while
	   we were processing this one.  Block SIGWINCH during this
	   check to prevent a possible race condition. */
#ifdef SIGWINCH
	EMACS_BLOCK_SIGNAL(SIGWINCH);
#endif
	if (old_asynch_device_change_pending == asynch_device_change_pending)
		asynch_device_change_pending = 0;
#ifdef SIGWINCH
	EMACS_UNBLOCK_SIGNAL(SIGWINCH);
#endif
}

void
call_critical_lisp_code(struct device *d, Lisp_Object function,
			Lisp_Object object)
{
	int old_gc_currently_forbidden = gc_currently_forbidden;
	Lisp_Object old_inhibit_quit = Vinhibit_quit;

	/* There's no reason to bother doing specbinds here, because if
	   initialize-*-faces signals an error, emacs is going to crash
	   immediately.
	 */
	gc_currently_forbidden = 1;
	Vinhibit_quit = Qt;
	LOCK_DEVICE(d);

	/* But it's useful to have an error handler; otherwise an infinite
	   loop may result. */
	if (!NILP(object))
		call1_with_handler(Qreally_early_error_handler, function,
				   object);
	else
		call0_with_handler(Qreally_early_error_handler, function);

	UNLOCK_DEVICE(d);
	Vinhibit_quit = old_inhibit_quit;
	gc_currently_forbidden = old_gc_currently_forbidden;
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void syms_of_device(void)
{
	INIT_LRECORD_IMPLEMENTATION(device);

	DEFSUBR(Fvalid_device_class_p);
	DEFSUBR(Fdevice_class_list);

	DEFSUBR(Fdfw_device);
	DEFSUBR(Fselected_device);
	DEFSUBR(Fselect_device);
	DEFSUBR(Fset_device_selected_frame);
	DEFSUBR(Fdevicep);
	DEFSUBR(Fdevice_live_p);
	DEFSUBR(Fdevice_name);
	DEFSUBR(Fdevice_connection);
	DEFSUBR(Fdevice_console);
	DEFSUBR(Ffind_device);
	DEFSUBR(Fget_device);
	DEFSUBR(Fmake_device);
	DEFSUBR(Fdelete_device);
	DEFSUBR(Fdevice_frame_list);
	DEFSUBR(Fdevice_class);
	DEFSUBR(Fset_device_class);
	DEFSUBR(Fdevice_system_metrics);
	DEFSUBR(Fdevice_system_metric);
	DEFSUBR(Fset_device_baud_rate);
	DEFSUBR(Fdevice_baud_rate);
	DEFSUBR(Fdomain_device_type);
	DEFSUBR(Fdevice_printer_p);

	defsymbol(&Qdevicep, "devicep");
	defsymbol(&Qdevice_live_p, "device-live-p");

	defsymbol(&Qcreate_device_hook, "create-device-hook");
	defsymbol(&Qdelete_device_hook, "delete-device-hook");

	/* Qcolor defined in general.c */
	defsymbol(&Qgrayscale, "grayscale");
	defsymbol(&Qmono, "mono");

	/* Device metrics symbols */
	defsymbol(&Qcolor_default, "color-default");
	defsymbol(&Qcolor_select, "color-select");
	defsymbol(&Qcolor_balloon, "color-balloon");
	defsymbol(&Qcolor_3d_face, "color-3d-face");
	defsymbol(&Qcolor_3d_light, "color-3d-light");
	defsymbol(&Qcolor_3d_dark, "color-3d-dark");
	defsymbol(&Qcolor_menu, "color-menu");
	defsymbol(&Qcolor_menu_highlight, "color-menu-highlight");
	defsymbol(&Qcolor_menu_button, "color-menu-button");
	defsymbol(&Qcolor_menu_disabled, "color-menu-disabled");
	defsymbol(&Qcolor_toolbar, "color-toolbar");
	defsymbol(&Qcolor_scrollbar, "color-scrollbar");
	defsymbol(&Qcolor_desktop, "color-desktop");
	defsymbol(&Qcolor_workspace, "color-workspace");
	defsymbol(&Qfont_default, "font-default");
	defsymbol(&Qfont_menubar, "font-menubar");
	defsymbol(&Qfont_dialog, "font-dialog");
	defsymbol(&Qsize_cursor, "size-cursor");
	defsymbol(&Qsize_scrollbar, "size-scrollbar");
	defsymbol(&Qsize_menu, "size-menu");
	defsymbol(&Qsize_toolbar, "size-toolbar");
	defsymbol(&Qsize_toolbar_button, "size-toolbar-button");
	defsymbol(&Qsize_toolbar_border, "size-toolbar-border");
	defsymbol(&Qsize_icon, "size-icon");
	defsymbol(&Qsize_icon_small, "size-icon-small");
	defsymbol(&Qsize_device, "size-device");
	defsymbol(&Qsize_workspace, "size-workspace");
	defsymbol(&Qoffset_workspace, "offset-workspace");
	defsymbol(&Qsize_device_mm, "size-device-mm");
	defsymbol(&Qnum_bit_planes, "num-bit-planes");
	defsymbol(&Qnum_color_cells, "num-color-cells");
	defsymbol(&Qdevice_dpi, "device-dpi");
	defsymbol(&Qmouse_buttons, "mouse-buttons");
	defsymbol(&Qswap_buttons, "swap-buttons");
	defsymbol(&Qshow_sounds, "show-sounds");
	defsymbol(&Qslow_device, "slow-device");
	defsymbol(&Qsecurity, "security");
}

void reinit_vars_of_device(void)
{
	staticpro_nodump(&Vdefault_device);
	Vdefault_device = Qnil;
	asynch_device_change_pending = 0;
}

void vars_of_device(void)
{
	reinit_vars_of_device();

	DEFVAR_LISP("create-device-hook", &Vcreate_device_hook	/*
Function or functions to call when a device is created.
One argument, the newly-created device.
This is called after the first frame has been created, but before
calling the `create-frame-hook'.
Note that in general the device will not be selected.
								 */ );
	Vcreate_device_hook = Qnil;

	DEFVAR_LISP("delete-device-hook", &Vdelete_device_hook	/*
Function or functions to call when a device is deleted.
One argument, the to-be-deleted device.
								 */ );
	Vdelete_device_hook = Qnil;

	Vdevice_class_list = list3(Qcolor, Qgrayscale, Qmono);
	staticpro(&Vdevice_class_list);

	/* Death to devices.el !!! */
	Fprovide(intern("devices"));
}
