/* Console functions for X windows.
   Copyright (C) 1996 Ben Wing.

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

/* This file Mule-ized by Ben Wing, 7-10-00. */

/* Authorship:

   Ben Wing: January 1996, for 19.14.
 */

#include <config.h>
#include "lisp.h"

#include "console-x.h"
#include "buffer.h"
#include "process.h"		/* canonicalize_host_name */
#include "ui/redisplay.h"		/* for display_arg */

DEFINE_CONSOLE_TYPE(x);

static int x_initially_selected_for_input(struct console *con)
{
	return 1;
}

/* Parse a DISPLAY specification like "host:10.0" or ":0" */
static void
split_up_display_spec(Lisp_Object display, int *hostname_length,
		      int *display_length, int *screen_length)
{
	Bufbyte *beg = XSTRING_DATA(display);
	Bufbyte *end = beg + XSTRING_LENGTH(display);
	Bufbyte const *p = end;

	while (p > beg) {
		DEC_CHARPTR(p);
		if (charptr_emchar(p) == ':') {
			*hostname_length = p - beg;

			while (p < end - 1) {
				INC_CHARPTR(p);
				if (charptr_emchar(p) == '.') {
					*display_length =
					    p - beg - *hostname_length;
					*screen_length = end - p;
					return;
				}
			}
			/* No '.' found. */
			*display_length =
			    XSTRING_LENGTH(display) - *hostname_length;
			*screen_length = 0;
			return;
		}
	}

	/* No ':' found. */
	*hostname_length = XSTRING_LENGTH(display);
	*display_length = 0;
	*screen_length = 0;
}

/* Remember, in all of the following functions, we have to verify
   the integrity of our input, because the generic functions don't. */

static Lisp_Object
x_device_to_console_connection(Lisp_Object connection, Error_behavior errb)
{
	/* Strip the trailing .# off of the connection, if it's there. */

	if (NILP(connection))
		return Qnil;
	else {
		int hostname_length, display_length, screen_length;

		if (!ERRB_EQ(errb, ERROR_ME)) {
			if (!STRINGP(connection))
				return Qunbound;
		} else
			CHECK_STRING(connection);

		split_up_display_spec(connection, &hostname_length,
				      &display_length, &screen_length);
		connection =
		    make_string(XSTRING_DATA(connection),
				hostname_length + display_length);
	}

	return connection;
}

static Lisp_Object get_display_arg_connection(void)
{
	const Extbyte *disp_name;

	/* If the user didn't explicitly specify a display to use when
	   they called make-x-device, then we first check to see if a
	   display was specified on the command line with -display.  If
	   so, we set disp_name to it.  Otherwise we use XDisplayName to
	   see what DISPLAY is set to.  XtOpenDisplay knows how to do
	   both of these things, but we need to know the name to use. */
	if (display_arg) {
		int elt;
		int argc;
		Extbyte **argv;
		Lisp_Object conn;

		make_argc_argv(Vx_initial_argv_list, &argc, &argv);

		disp_name = NULL;
		for (elt = 0; elt < argc; elt++) {
			if (!strcmp(argv[elt], "-d")
			    || !strcmp(argv[elt], "-display")) {
				if (elt + 1 == argc) {
					suppress_early_error_handler_backtrace =
					    1;
					type_error(Qinvalid_argument,
						   "-display specified with no arg");
				} else {
					disp_name = argv[elt + 1];
					break;
				}
			}
		}

		/* assert: display_arg is only set if we found the display
		   arg earlier so we can't fail to find it now. */
		assert(disp_name != NULL);
		conn = build_ext_string(disp_name, Qcommand_argument_encoding);
		free_argc_argv(argv);
		return conn;
	} else
		return build_ext_string(XDisplayName(0),
					Qx_display_name_encoding);
}

/* "semi-canonicalize" means convert to a nicer form for printing, but
   don't completely canonicalize (into some likely ugly form) */

static Lisp_Object
x_semi_canonicalize_console_connection(Lisp_Object connection,
				       Error_behavior errb)
{
	struct gcpro gcpro1;

	GCPRO1(connection);

	if (NILP(connection))
		connection = get_display_arg_connection();
	else {
		if (!ERRB_EQ(errb, ERROR_ME)) {
			if (!STRINGP(connection))
				RETURN_UNGCPRO(Qunbound);
		} else
			CHECK_STRING(connection);
	}

	/* Be lenient, allow people to specify a device connection instead of
	   a console connection -- e.g. "foo:0.0" instead of "foo:0".  This
	   only happens in `find-console' and `get-console'. */
	connection = x_device_to_console_connection(connection, errb);

	/* Check for a couple of standard special cases */
	if (string_char(XSTRING(connection), 0) == ':') {
		connection = concat2(build_string("localhost"), connection);
	} else {
		/* connection =~ s/^unix:/localhost:/; */
		Bufbyte *p = XSTRING_DATA(connection);
		const Bufbyte *end =
			XSTRING_DATA(connection) + XSTRING_LENGTH(connection);
		size_t i;

		for (i = 0; i < sizeof("unix:") - 1; i++) {
			if (p == end || charptr_emchar(p) != "unix:"[i])
				goto ok;
			INC_CHARPTR(p);
		}

		connection = concat2(build_string("localhost:"),
				     make_string(p, end - p));
	}
ok:
	RETURN_UNGCPRO(connection);
}

static Lisp_Object
x_canonicalize_console_connection(Lisp_Object connection, Error_behavior errb)
{
	Lisp_Object hostname = Qnil;
	struct gcpro gcpro1, gcpro2;

	GCPRO2(connection, hostname);

	connection = x_semi_canonicalize_console_connection(connection, errb);
	if (UNBOUNDP(connection))
		RETURN_UNGCPRO(Qunbound);

	{
		int hostname_length, display_length, screen_length;

		split_up_display_spec(connection, &hostname_length,
				      &display_length, &screen_length);
		hostname =
		    Fsubstring(connection, Qzero, make_int(hostname_length));
		hostname = canonicalize_host_name(hostname);
		connection = concat2(hostname,
				     make_string(XSTRING_DATA(connection)
						 + hostname_length,
						 display_length));
	}

	RETURN_UNGCPRO(connection);
}

static Lisp_Object
x_semi_canonicalize_device_connection(Lisp_Object connection,
				      Error_behavior errb)
{
	int hostname_length, display_length, screen_length;
	struct gcpro gcpro1;

	GCPRO1(connection);
	if (NILP(connection))
		connection = get_display_arg_connection();
	else {
		if (!ERRB_EQ(errb, ERROR_ME)) {
			if (!STRINGP(connection))
				RETURN_UNGCPRO(Qunbound);
		} else
			CHECK_STRING(connection);
	}

	split_up_display_spec(connection, &hostname_length, &display_length,
			      &screen_length);

	if (!screen_length)
		connection = concat2(connection, build_string(".0"));
	RETURN_UNGCPRO(connection);
}

static Lisp_Object
x_canonicalize_device_connection(Lisp_Object connection, Error_behavior errb)
{
	int hostname_length, display_length, screen_length;
	Lisp_Object screen_str = Qnil;
	struct gcpro gcpro1, gcpro2;

	GCPRO2(screen_str, connection);
	connection = x_semi_canonicalize_device_connection(connection, errb);
	if (UNBOUNDP(connection))
		RETURN_UNGCPRO(Qunbound);

	split_up_display_spec(connection, &hostname_length, &display_length,
			      &screen_length);

	screen_str = make_string(XSTRING_DATA(connection)
				 + hostname_length + display_length,
				 screen_length);
	connection = x_canonicalize_console_connection(connection, errb);

	RETURN_UNGCPRO(concat2(connection, screen_str));
}

void console_type_create_x(void)
{
	INITIALIZE_CONSOLE_TYPE(x, "x", "console-x-p");

	CONSOLE_HAS_METHOD(x, semi_canonicalize_console_connection);
	CONSOLE_HAS_METHOD(x, canonicalize_console_connection);
	CONSOLE_HAS_METHOD(x, semi_canonicalize_device_connection);
	CONSOLE_HAS_METHOD(x, canonicalize_device_connection);
	CONSOLE_HAS_METHOD(x, device_to_console_connection);
	CONSOLE_HAS_METHOD(x, initially_selected_for_input);
}

void reinit_console_type_create_x(void)
{
	REINITIALIZE_CONSOLE_TYPE(x);
}
