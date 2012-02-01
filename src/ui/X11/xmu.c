/* This file contains compatibility routines for systems without Xmu.
 * You would be better served by installing Xmu on your machine or
 * yelling at your vendor to ship it.
 */

/* XEmacs changes: rindex -> strrchr */

/* Synched up with: Not in FSF. */

#include <config.h>

#ifndef HAVE_XMU
/*
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#include <X11/cursorfont.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>

/* for XmuCopyISOLatin1Lowered */
#define XK_LATIN1
#include <X11/keysymdef.h>
#undef XK_LATIN1

#if (XtSpecificationRelease >= 5)
/*
 * Don't know why, but this works with X11R5, not X11R4.
 * Anyway, _XExtension is defined in Xlib.h in X11R4, so we do not need
 * Xlibint in that case...
 */
#include <X11/Xlibint.h>
#endif
#include <X11/Xproto.h>
#include <stdio.h>
#include <ctype.h>

int XmuCursorNameToIndex(const char *name)
{
	static const struct _CursorName {
		const char *name;
		unsigned int shape;
	} cursor_names[] = {
		{
		"x_cursor", XC_X_cursor}, {
		"arrow", XC_arrow}, {
		"based_arrow_down", XC_based_arrow_down}, {
		"based_arrow_up", XC_based_arrow_up}, {
		"boat", XC_boat}, {
		"bogosity", XC_bogosity}, {
		"bottom_left_corner", XC_bottom_left_corner}, {
		"bottom_right_corner", XC_bottom_right_corner}, {
		"bottom_side", XC_bottom_side}, {
		"bottom_tee", XC_bottom_tee}, {
		"box_spiral", XC_box_spiral}, {
		"center_ptr", XC_center_ptr}, {
		"circle", XC_circle}, {
		"clock", XC_clock}, {
		"coffee_mug", XC_coffee_mug}, {
		"cross", XC_cross}, {
		"cross_reverse", XC_cross_reverse}, {
		"crosshair", XC_crosshair}, {
		"diamond_cross", XC_diamond_cross}, {
		"dot", XC_dot}, {
		"dotbox", XC_dotbox}, {
		"double_arrow", XC_double_arrow}, {
		"draft_large", XC_draft_large}, {
		"draft_small", XC_draft_small}, {
		"draped_box", XC_draped_box}, {
		"exchange", XC_exchange}, {
		"fleur", XC_fleur}, {
		"gobbler", XC_gobbler}, {
		"gumby", XC_gumby}, {
		"hand1", XC_hand1}, {
		"hand2", XC_hand2}, {
		"heart", XC_heart}, {
		"icon", XC_icon}, {
		"iron_cross", XC_iron_cross}, {
		"left_ptr", XC_left_ptr}, {
		"left_side", XC_left_side}, {
		"left_tee", XC_left_tee}, {
		"leftbutton", XC_leftbutton}, {
		"ll_angle", XC_ll_angle}, {
		"lr_angle", XC_lr_angle}, {
		"man", XC_man}, {
		"middlebutton", XC_middlebutton}, {
		"mouse", XC_mouse}, {
		"pencil", XC_pencil}, {
		"pirate", XC_pirate}, {
		"plus", XC_plus}, {
		"question_arrow", XC_question_arrow}, {
		"right_ptr", XC_right_ptr}, {
		"right_side", XC_right_side}, {
		"right_tee", XC_right_tee}, {
		"rightbutton", XC_rightbutton}, {
		"rtl_logo", XC_rtl_logo}, {
		"sailboat", XC_sailboat}, {
		"sb_down_arrow", XC_sb_down_arrow}, {
		"sb_h_double_arrow", XC_sb_h_double_arrow}, {
		"sb_left_arrow", XC_sb_left_arrow}, {
		"sb_right_arrow", XC_sb_right_arrow}, {
		"sb_up_arrow", XC_sb_up_arrow}, {
		"sb_v_double_arrow", XC_sb_v_double_arrow}, {
		"shuttle", XC_shuttle}, {
		"sizing", XC_sizing}, {
		"spider", XC_spider}, {
		"spraycan", XC_spraycan}, {
		"star", XC_star}, {
		"target", XC_target}, {
		"tcross", XC_tcross}, {
		"top_left_arrow", XC_top_left_arrow}, {
		"top_left_corner", XC_top_left_corner}, {
		"top_right_corner", XC_top_right_corner}, {
		"top_side", XC_top_side}, {
		"top_tee", XC_top_tee}, {
		"trek", XC_trek}, {
		"ul_angle", XC_ul_angle}, {
		"umbrella", XC_umbrella}, {
		"ur_angle", XC_ur_angle}, {
		"watch", XC_watch}, {
	"xterm", XC_xterm},};
	const struct _CursorName *table;
	int i;
	char tmp[40];

	if (strlen(name) >= sizeof tmp)
		return -1;
	for (i = 0; i < strlen(name); i++)
		if (isupper((unsigned char)name[i]))
			tmp[i] = tolower((unsigned char)name[i]);
		else
			tmp[i] = name[i];
	tmp[i] = 0;

	for (i = 0, table = cursor_names; i < XtNumber(cursor_names);
	     i++, table++) {
		if (strcmp(tmp, table->name) == 0)
			return table->shape;
	}

	return -1;
}

/*
 * Based on an optimized version provided by Jim Becker, August 5, 1988.
 */

#define MAX_SIZE 255

/* shared data for the image read/parse logic */
static short hexTable[256];	/* conversion value */
static int hex_initialized;	/* easier to fill in at run time */

/*
 *	Table index for the hex values. Initialized once, first time.
 *	Used for translation value or delimiter significance lookup.
 */
static void initHexTable(void)
{
	/*
	 * We build the table at run time for several reasons:
	 *
	 *     1.  portable to non-ASCII machines.
	 *     2.  still reentrant since we set the init flag after setting table.
	 *     3.  easier to extend.
	 *     4.  less prone to bugs.
	 */
	hexTable['0'] = 0;
	hexTable['1'] = 1;
	hexTable['2'] = 2;
	hexTable['3'] = 3;
	hexTable['4'] = 4;
	hexTable['5'] = 5;
	hexTable['6'] = 6;
	hexTable['7'] = 7;
	hexTable['8'] = 8;
	hexTable['9'] = 9;
	hexTable['A'] = 10;
	hexTable['B'] = 11;
	hexTable['C'] = 12;
	hexTable['D'] = 13;
	hexTable['E'] = 14;
	hexTable['F'] = 15;
	hexTable['a'] = 10;
	hexTable['b'] = 11;
	hexTable['c'] = 12;
	hexTable['d'] = 13;
	hexTable['e'] = 14;
	hexTable['f'] = 15;

	/* delimiters of significance are flagged w/ negative value */
	hexTable[' '] = -1;
	hexTable[','] = -1;
	hexTable['}'] = -1;
	hexTable['\n'] = -1;
	hexTable['\t'] = -1;

	hex_initialized = 1;
}

/*
 *	read next hex value in the input stream, return -1 if EOF
 */
static int NextInt(FILE * fstream)
{
	int ch;
	int value = 0;
	int gotone = 0;
	int done = 0;

	/* loop, accumulate hex value until find delimiter  */
	/* skip any initial delimiters found in read stream */

	while (!done) {
		ch = getc(fstream);
		if (ch == EOF) {
			value = -1;
			done++;
		} else {
			/* trim high bits, check type and accumulate */
			ch &= 0xff;
			if (isascii(ch) && isxdigit(ch)) {
				value = (value << 4) + hexTable[ch];
				gotone++;
			} else if ((hexTable[ch]) < 0 && gotone)
				done++;
		}
	}
	return value;
}

/*
 * The data returned by the following routine is always in left-most byte
 * first and left-most bit first.  If it doesn't return BitmapSuccess then
 * its arguments won't have been touched.  This routine should look as much
 * like the Xlib routine XReadBitmapfile as possible.
 */
int XmuReadBitmapData(FILE * fstream,	/* handle on file  */
		      unsigned int *width,	/* RETURNED */
		      unsigned int *height,	/* RETURNED */
		      unsigned char **datap,	/* RETURNED */
		      int *x_hot, int *y_hot)
{				/* RETURNED */
	unsigned char *data = NULL;	/* working variable */
	char line[MAX_SIZE];	/* input line from file */
	int size;		/* number of bytes of data */
	char name_and_type[MAX_SIZE];	/* an input line */
	char *type;		/* for parsing */
	int value;		/* from an input line */
	int version10p;		/* boolean, old format */
	int padding;		/* to handle alignment */
	int bytes_per_line;	/* per scanline of data */
	unsigned int ww = 0;	/* width */
	unsigned int hh = 0;	/* height */
	int hx = -1;		/* x hotspot */
	int hy = -1;		/* y hotspot */

#ifndef Xmalloc
#define Xmalloc(size) malloc(size)
#endif

	/* first time initialization */
	if (!hex_initialized)
		initHexTable();

	/* error cleanup and return macro   */
#define	RETURN(code) { if (data) free (data); return code; }

	while (fgets(line, MAX_SIZE, fstream)) {
		if (strlen(line) == MAX_SIZE - 1) {
			RETURN(BitmapFileInvalid);
		}
		if (sscanf(line, "#define %s %d", name_and_type, &value) == 2) {
			if (!(type = strrchr(name_and_type, '_')))
				type = name_and_type;
			else
				type++;

			if (!strcmp("width", type))
				ww = (unsigned int)value;
			if (!strcmp("height", type))
				hh = (unsigned int)value;
			if (!strcmp("hot", type)) {
				if (type-- == name_and_type
				    || type-- == name_and_type)
					continue;
				if (!strcmp("x_hot", type))
					hx = value;
				if (!strcmp("y_hot", type))
					hy = value;
			}
			continue;
		}

		if (sscanf(line, "static short %s = {", name_and_type) == 1)
			version10p = 1;
		else if (sscanf
			 (line, "static unsigned char %s = {",
			  name_and_type) == 1)
			version10p = 0;
		else if (sscanf(line, "static char %s = {", name_and_type) == 1)
			version10p = 0;
		else
			continue;

		if (!(type = strrchr(name_and_type, '_')))
			type = name_and_type;
		else
			type++;

		if (strcmp("bits[]", type))
			continue;

		if (!ww || !hh)
			RETURN(BitmapFileInvalid);

		if ((ww % 16) && ((ww % 16) < 9) && version10p)
			padding = 1;
		else
			padding = 0;

		bytes_per_line = (ww + 7) / 8 + padding;

		size = bytes_per_line * hh;
		data = (unsigned char *)Xmalloc((unsigned int)size);
		if (!data)
			RETURN(BitmapNoMemory);

		if (version10p) {
			unsigned char *ptr;
			int bytes;

			for (bytes = 0, ptr = data; bytes < size; (bytes += 2)) {
				if ((value = NextInt(fstream)) < 0)
					RETURN(BitmapFileInvalid);
				*(ptr++) = value;
				if (!padding || ((bytes + 2) % bytes_per_line))
					*(ptr++) = value >> 8;
			}
		} else {
			unsigned char *ptr;
			int bytes;

			for (bytes = 0, ptr = data; bytes < size;
			     bytes++, ptr++) {
				if ((value = NextInt(fstream)) < 0)
					RETURN(BitmapFileInvalid);
				*ptr = value;
			}
		}
		break;
	}			/* end while */

	if (data == NULL) {
		RETURN(BitmapFileInvalid);
	}

	*datap = data;
	data = NULL;
	*width = ww;
	*height = hh;
	if (x_hot)
		*x_hot = hx;
	if (y_hot)
		*y_hot = hy;

	RETURN(BitmapSuccess);
}

int XmuReadBitmapDataFromFile(const char *filename,
			      /* Remaining args are RETURNED */
			      unsigned int *width,
			      unsigned int *height,
			      unsigned char **datap, int *x_hot, int *y_hot)
{
	FILE *fstream;
	int status;

	if ((fstream = fopen(filename, "r")) == NULL) {
		return BitmapOpenFailed;
	}
	status = XmuReadBitmapData(fstream, width, height, datap, x_hot, y_hot);
	fclose(fstream);
	return status;
}

/*
 * XmuPrintDefaultErrorMessage - print a nice error that looks like the usual
 * message.  Return 1 if the caller should consider exiting, else 0.
 */
int XmuPrintDefaultErrorMessage(Display * dpy, XErrorEvent * event, FILE * fp)
{
	char buffer[BUFSIZ];
	char mesg[BUFSIZ];
	char number[32];
	char *mtype = "XlibMessage";
	_XExtension *ext = (_XExtension *) NULL;
	XGetErrorText(dpy, event->error_code, buffer, BUFSIZ);
	XGetErrorDatabaseText(dpy, mtype, "XError", "X Error", mesg, BUFSIZ);
	fprintf(fp, "%s:  %s\n  ", mesg, buffer);
	XGetErrorDatabaseText(dpy, mtype, "MajorCode", "Request Major code %d",
			      mesg, BUFSIZ);
	fprintf(fp, mesg, event->request_code);
	if (event->request_code < 128) {
		int sz = snprintf(number, sizeof(number), "%d", event->request_code);
		assert(sz >= 0 && sz < sizeof(number));
		XGetErrorDatabaseText(dpy, "XRequest", number, "", buffer,
				      BUFSIZ);
	} else {
		/* XXX this is non-portable */
		for (ext = dpy->ext_procs;
		     ext && (ext->codes.major_opcode != event->request_code);
		     ext = ext->next) ;
		if (ext) {
			strncpy(buffer, ext->name, sizeof(buffer));
			buffer[sizeof(buffer)-1] = '\0';
		} else
			buffer[0] = '\0';
	}
	fprintf(fp, " (%s)", buffer);
	fputs("\n  ", fp);
#if (XtSpecificationRelease >= 5)
	if (event->request_code >= 128) {
		XGetErrorDatabaseText(dpy, mtype, "MinorCode",
				      "Request Minor code %d", mesg, BUFSIZ);
		fprintf(fp, mesg, event->minor_code);
		if (ext) {
			in sz = snprintf(mesg, sizeof(mesg), "%s.%d", ext->name,
					 event->minor_code);
			assert(sz >= 0 && sz < sizeof(mesg));
			XGetErrorDatabaseText(dpy, "XRequest", mesg, "", buffer,
					      BUFSIZ);
			fprintf(fp, " (%s)", buffer);
		}
		fputs("\n  ", fp);
	}
	if (event->error_code >= 128) {
		/* let extensions try to print the values */
		/* XXX this is non-portable code */
		for (ext = dpy->ext_procs; ext; ext = ext->next) {
			if (ext->error_values)
				(*ext->error_values) (dpy, event, fp);
		}
		/* the rest is a fallback, providing a simple default */
		/* kludge, try to find the extension that caused it */
		buffer[0] = '\0';
		for (ext = dpy->ext_procs; ext; ext = ext->next) {
			if (ext->error_string)
				(*ext->error_string) (dpy, event->error_code,
						      &ext->codes, buffer,
						      BUFSIZ);
			if (buffer[0])
				break;
		}
		if (buffer[0]) {
			int sz = snprintf(buffer, sizeof(buffer), "%s.%d",
					  ext->name,
					  event->error_code - ext->codes.first_error);
			assert(sz >= 0 && sz < sizeof(buffer));
		} else
			strncpy(buffer, "Value", sizeof(buffer));
		XGetErrorDatabaseText(dpy, mtype, buffer, "", mesg, BUFSIZ);
		if (*mesg) {
			fprintf(fp, mesg, event->resourceid);
			fputs("\n  ", fp);
		}
	} else if ((event->error_code == BadWindow) ||
		   (event->error_code == BadPixmap) ||
		   (event->error_code == BadCursor) ||
		   (event->error_code == BadFont) ||
		   (event->error_code == BadDrawable) ||
		   (event->error_code == BadColor) ||
		   (event->error_code == BadGC) ||
		   (event->error_code == BadIDChoice) ||
		   (event->error_code == BadValue) ||
		   (event->error_code == BadAtom)) {
		if (event->error_code == BadValue)
			XGetErrorDatabaseText(dpy, mtype, "Value", "Value 0x%x",
					      mesg, BUFSIZ);
		else if (event->error_code == BadAtom)
			XGetErrorDatabaseText(dpy, mtype, "AtomID",
					      "AtomID 0x%x", mesg, BUFSIZ);
		else
			XGetErrorDatabaseText(dpy, mtype, "ResourceID",
					      "ResourceID 0x%x", mesg, BUFSIZ);
		fprintf(fp, mesg, event->resourceid);
		fputs("\n  ", fp);
	}
#elif (XtSpecificationRelease == 4)
	XGetErrorDatabaseText(dpy, mtype, "MinorCode", "Request Minor code %d",
			      mesg, BUFSIZ);
	fprintf(fp, mesg, event->minor_code);
	fputs("\n  ", fp);
	if (ext) {
		int sz = snprintf(mesg, sizeof(mesg), "%s.%d", ext->name, event->minor_code);
		assert(sz >= 0 && sz < sizeof(mesg));
		XGetErrorDatabaseText(dpy, "XRequest", mesg, "", buffer,
				      BUFSIZ);
		fprintf(fp, " (%s)", buffer);
	}
	XGetErrorDatabaseText(dpy, mtype, "ResourceID", "ResourceID 0x%x",
			      mesg, BUFSIZ);
	fprintf(fp, mesg, event->resourceid);
	fputs("\n  ", fp);
#else
	ERROR ! Unsupported release of X11
#endif
	    XGetErrorDatabaseText(dpy, mtype, "ErrorSerial", "Error Serial #%d",
				  mesg, BUFSIZ);
	fprintf(fp, mesg, event->serial);
	fputs("\n  ", fp);
	XGetErrorDatabaseText(dpy, mtype, "CurrentSerial", "Current Serial #%d",
			      mesg, BUFSIZ);
	fprintf(fp, mesg, NextRequest(dpy) - 1);
	fputs("\n", fp);
	if (event->error_code == BadImplementation)
		return 0;
	return 1;
}

/*
 * XmuSimpleErrorHandler - ignore errors for XQueryTree, XGetWindowAttributes,
 * and XGetGeometry; print a message for everything else.  In all case, do
 * not exit.
 */
int XmuSimpleErrorHandler(Display * dpy, XErrorEvent * errorp)
{
	switch (errorp->request_code) {
	case X_QueryTree:
	case X_GetWindowAttributes:
		if (errorp->error_code == BadWindow)
			return 0;
		break;
	case X_GetGeometry:
		if (errorp->error_code == BadDrawable)
			return 0;
		break;
	}
	/* got a "real" X error */
	return XmuPrintDefaultErrorMessage(dpy, errorp, stderr);
}

void XmuCopyISOLatin1Lowered(char *dst, const char *src)
{
	unsigned char *dest = (unsigned char *)dst;
	unsigned char *source = (unsigned char *)src;

	for (; *source; source++, dest++) {
		if ((*source >= XK_A) && (*source <= XK_Z))
			*dest = *source + (XK_a - XK_A);
		else if ((*source >= XK_Agrave) && (*source <= XK_Odiaeresis))
			*dest = *source + (XK_agrave - XK_Agrave);
		else if ((*source >= XK_Ooblique) && (*source <= XK_Thorn))
			*dest = *source + (XK_oslash - XK_Ooblique);
		else
			*dest = *source;
	}
	*dest = '\0';
}
#endif				/* !HAVE_XMU */
