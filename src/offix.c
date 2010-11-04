/*
This is a modified DND 1.0 library which does not depend on Xt
event handling.
Modifications Copyright (c) 1997 Oliver Graf <ograf@fga.de>

Original DND lib
Copyright (C) 1996 César Crusius

This file is part of the DND Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/

/* #define DEBUG */

#include "ui/X11/offix.h"
#include <X11/cursorfont.h>
#include <X11/Xatom.h>
#include <X11/Xmu/WinUtil.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

/* Local variables */
static Display *dpy;		/* current display              */
static int DragPrecision;	/* minimum dx,dy to start drag  */
static int Dragging;		/* Drag state flag              */
static int DataOK;		/* Non-zero if data registered  */
static Atom DndProtocol;	/* ClientMessage identifier     */
static Atom DndSelection;	/* For the data transfers       */
static Atom OldDndProtocol;	/* Version 0 atom               */
static Atom OldDndSelection;	/* Version 0 atom               */
static Atom WM_STATE;		/* Needed for icon stuff        */
static Window Target;		/* Drop window                  */
static Widget MainWidget;	/* Main widget of application   */
static int DataType;		/* Current drag data type       */
static int RootFlag;		/* Non-zero if dropped on root  */
static XColor Black, White;	/* For the cursors              */

/*=========================================================================
 * Data for the standard Dnd cursors
 *=========================================================================*/
#include "ui/X11/offix-cursors.h"

/*=============================================================== CursorData
 * CursorData contains all the data for the cursors bitmaps
 *==========================================================================*/
typedef struct {
	int Width, Height;
	unsigned char *ImageData, *MaskData;
	int HotSpotX, HotSpotY;
	Pixmap ImagePixmap, MaskPixmap;
	Cursor CursorID;
} CursorData;

static CursorData DndCursor[DndEND] = {
	{0, 0, NULL, NULL, 0, 0, 0},
	{grey_width, grey_height, grey_bits, grey_mask_bits,
	 grey_x_hot, grey_y_hot},
	{file_width, file_height, file_bits, file_mask_bits,
	 file_x_hot, file_y_hot},
	{files_width, files_height, files_bits, files_mask_bits,
	 files_x_hot, files_y_hot},
	{text_width, text_height, text_bits, text_mask_bits,
	 text_x_hot, text_y_hot},
	{dir_width, dir_height, dir_bits, dir_mask_bits,
	 dir_x_hot, dir_y_hot},
	{link_width, link_height, link_bits, link_mask_bits,
	 link_x_hot, link_y_hot},
	{app_width, app_height, app_bits, app_mask_bits,
	 app_x_hot, app_y_hot},
	{url_width, url_height, url_bits, url_mask_bits,
	 url_x_hot, url_y_hot},
	{mime_width, mime_height, mime_bits, mime_mask_bits,
	 mime_x_hot, mime_y_hot}
};

/* Local prototypes */
int DndIsDragging(void);
void DndStartAction(Widget widget, XtPointer data, XEvent * event, Boolean * p);
void DndPropertyHandler(Widget widget,
			XtPointer data, XEvent * event, Boolean * p);

/*======================================================== DndHandleDragging
 * Takes care of the drag and drop process. Wait until the pointer had moved
 * a little. Then takes control over the pointer until the buttons are
 * released. After that send a Drag And Drop ClientMessage event. Returns
 * non-zero if a drop did take place.
 *===========================================================================*/
int DndHandleDragging(Widget widget, XEvent * event)
{
	XEvent Event;
	Window root = RootWindowOfScreen(XtScreenOfObject(widget));
	XtAppContext app = XtWidgetToApplicationContext(widget);
	Window DispatchWindow;
	int DropX, DropY;

	if (Dragging)
		return 0;

	XUngrabPointer(dpy, CurrentTime);
	/* Take control over the pointer */
	XGrabPointer(dpy, root, False,
		     ButtonMotionMask | ButtonPressMask | ButtonReleaseMask,
		     GrabModeSync, GrabModeAsync, root,
		     DndCursor[DataType].CursorID, CurrentTime);

	/* Wait for button release */
	Dragging = 1;
	RootFlag = 0;
	while (Dragging) {
		XAllowEvents(dpy, SyncPointer, CurrentTime);
		XtAppNextEvent(app, &Event);
		switch (Event.type) {
		case ButtonRelease:
			if (Event.xbutton.subwindow)
				RootFlag = 0;
			else
				RootFlag = 1;
			Dragging = 0;
			break;
		default:
			XtDispatchEvent(&Event);
			break;
		}
	}
	DataOK = 0;
	/* Now release the pointer */
	XUngrabPointer(dpy, CurrentTime);
	/* Try to guess if the drop occurred in the root window */
	if (!RootFlag) {
		Target = XmuClientWindow(dpy, Event.xbutton.subwindow);
		if (Target == Event.xbutton.subwindow)
			DispatchWindow = Target;
		else
			DispatchWindow = PointerWindow;
	} else
		Target = DispatchWindow = XtWindow(MainWidget);

	/* Now build the event structure */
	DropX = Event.xbutton.x_root;
	DropY = Event.xbutton.y_root;
	Event.xclient.type = ClientMessage;
	Event.xclient.display = dpy;
	Event.xclient.message_type = DndProtocol;
	Event.xclient.format = 32;
	Event.xclient.window = Target;
	Event.xclient.data.l[0] = DataType;
	Event.xclient.data.l[1] = (long)event->xbutton.state;
	Event.xclient.data.l[2] = (long)XtWindow(widget);
	Event.xclient.data.l[3] = DropX + 65536L * (long)DropY;
	Event.xclient.data.l[4] = 1;

	/* Send the drop message */
	XSendEvent(dpy, DispatchWindow, True, NoEventMask, &Event);
	/* Send an old style version of the message just in case */
	Event.xclient.message_type = OldDndProtocol;
	XSendEvent(dpy, DispatchWindow, True, NoEventMask, &Event);

#ifdef DEBUG
	fprintf(stderr, "ClientMessage sent to 0x%x(0x%x).\n",
		DispatchWindow, Target);
	fprintf(stderr, "The drop coordinates are (%d,%d).\n", DropX, DropY);
#endif

	return 1;
}

/*=============================================================== DndIsIcon
 * Return non-zero if the application is iconic (widget=toplevel)
 *========================================================================*/
int DndIsIcon(Widget widget)
{
	Atom JunkAtom;
	int JunkInt;
	unsigned long WinState, JunkLong;
	unsigned char *Property;

	XGetWindowProperty(dpy, XtWindow(widget), WM_STATE,
			   0L, 2L, False, AnyPropertyType,
			   &JunkAtom, &JunkInt, &WinState, &JunkLong,
			   &Property);
	WinState = (unsigned long)(*((long *)Property));
	return (WinState == 3);
}

/*============================================================ DndInitialize
 * Must be called anywhere before the top level widget creation and the
 * main loop. Initialize global variables and bind the DndDispatch function
 * to the top level widget. Creates the cursors to be used in drag actions.
 *=========================================================================*/
void DndInitialize(Widget shell)
{
	int screen, i;
	Colormap colormap;
	Window root;

	dpy = XtDisplayOfObject(shell);
	screen = DefaultScreen(dpy);
	colormap = DefaultColormap(dpy, screen);
	root = DefaultRootWindow(dpy);

	Black.pixel = BlackPixel(dpy, screen);
	White.pixel = WhitePixel(dpy, screen);
	XQueryColor(dpy, colormap, &Black);
	XQueryColor(dpy, colormap, &White);

	for (i = 1; i != DndEND; i++) {
		DndCursor[i].ImagePixmap =
		    XCreateBitmapFromData(dpy, root,
					  (char *)DndCursor[i].ImageData,
					  DndCursor[i].Width,
					  DndCursor[i].Height);
		DndCursor[i].MaskPixmap =
		    XCreateBitmapFromData(dpy, root,
					  (char *)DndCursor[i].MaskData,
					  DndCursor[i].Width,
					  DndCursor[i].Height);
		DndCursor[i].CursorID =
		    XCreatePixmapCursor(dpy, DndCursor[i].ImagePixmap,
					DndCursor[i].MaskPixmap,
					&Black, &White,
					DndCursor[i].HotSpotX,
					DndCursor[i].HotSpotY);
	}

	DndCursor[0].CursorID = XCreateFontCursor(dpy, XC_question_arrow);

	/* These two are for older versions */
	OldDndProtocol = XInternAtom(dpy, "DndProtocol", FALSE);
	OldDndSelection = XInternAtom(dpy, "DndSelection", FALSE);
	/* Now the correct stuff */
	DndProtocol = XInternAtom(dpy, "_DND_PROTOCOL", FALSE);
	DndSelection = XInternAtom(dpy, "_DND_SELECTION", FALSE);

	WM_STATE = XInternAtom(dpy, "WM_STATE", True);
	Dragging = 0;
	DragPrecision = 10;
	RootFlag = 0;
	MainWidget = shell;
}

int DndIsDragging(void)
{
	return Dragging;
}

/*================================================================= DndSetData
 * Updates the selection data.
 *===========================================================================*/
void DndSetData(int Type, unsigned char *Data, unsigned long Size)
{
	Window root = DefaultRootWindow(dpy);
	int AuxSize;
	unsigned char *AuxData;
	unsigned long BackSize = Size;

	if (DataOK)
		return;

	/* Set the data type -- allow any type */
	DataType = Type;

	/* Set the data */
	AuxData = Data;
	AuxSize = (Size <= INT_MAX ? (int)Size : INT_MAX);
	XChangeProperty(dpy, root, DndSelection, XA_STRING, 8,
			PropModeReplace, Data, AuxSize);
	for (Size -= (unsigned long)AuxSize; Size;
	     Size -= (unsigned long)AuxSize) {
		Data += AuxSize;
		AuxSize = ((Size <= (INT_MAX)) ? (int)Size : (INT_MAX));
		XChangeProperty(dpy, root, DndSelection, XA_STRING, 8,
				PropModeAppend, Data, AuxSize);
	}

	/* Set the data for old DND version */
	Size = BackSize;
	AuxData = Data;
	AuxSize = (Size <= INT_MAX ? (int)Size : INT_MAX);
	XChangeProperty(dpy, root, OldDndSelection, XA_STRING, 8,
			PropModeReplace, Data, AuxSize);
	for (Size -= (unsigned long)AuxSize; Size;
	     Size -= (unsigned long)AuxSize) {
		Data += AuxSize;
		AuxSize = ((Size <= (INT_MAX)) ? (int)Size : (INT_MAX));
		XChangeProperty(dpy, root, OldDndSelection, XA_STRING, 8,
				PropModeAppend, Data, AuxSize);
	}

	/* Everything is now ok */
	DataOK = 1;
}

/*================================================================== DndGetData
 * Return a pointer to the current data. See HOWTO for more details.
 *===========================================================================*/
void DndGetData(XEvent * event, unsigned char **Data, unsigned long *Size)
{
	Window root = DefaultRootWindow(dpy);

	Atom ActualType, ActualDndSelection;
	int ActualFormat;
	unsigned long RemainingBytes;

	ActualDndSelection = (DndProtocolVersion(event) == 0L ?
			      OldDndSelection : DndSelection);

	XGetWindowProperty(dpy, root, ActualDndSelection,
			   0L, 1000000L,
			   FALSE, AnyPropertyType,
			   &ActualType, &ActualFormat,
			   Size, &RemainingBytes, Data);
}

/*================================== DndDataType DndDragButtons DndSourceWidget
 *
 * Return information about the Dnd event received. If a non-dnd event is
 * passed, the function DndDataType returns DndNotDnd, and the others
 * return zero.
 *===========================================================================*/
int DndDataType(XEvent * event)
{
	int Type;

	if (!DndIsDropMessage(event))
		return DndNotDnd;
	Type = (int)(event->xclient.data.l[0]);
	if (Type >= DndEND)
		Type = DndUnknown;
	return Type;
}

unsigned int DndDragButtons(XEvent * event)
{
	if (!DndIsDropMessage(event))
		return 0;
	return (unsigned int)(event->xclient.data.l[1]);
}

Window DndSourceWindow(XEvent * event)
{
	if (!DndIsDropMessage(event))
		return 0;
	if (DndProtocolVersion(event) < __DragAndDropH__)
		/* We will try to do something about it, but nothing is certain */
		return XtWindow((Widget) (event->xclient.data.l[2]));
	return (Window) (event->xclient.data.l[2]);
}

void DndDropRootCoordinates(XEvent * event, int *x, int *y)
{
	if (!DndIsDropMessage(event)) {
		*x = 0;
		*y = 0;
		return;
	}

	/* If it is an old protocol version we try to get the coordinates
	   using the current pointer position. Of course, the pointer may have
	   moved since the drop, but there's nothing we can do about it.
	 */
	if (DndProtocolVersion(event) < 1L) {
		Window root_return, child_return;
		int win_x_return, win_y_return;
		unsigned int mask_return;

		XQueryPointer(dpy, DefaultRootWindow(dpy),
			      &root_return, &child_return, x, y,
			      &win_x_return, &win_y_return, &mask_return);
		return;
	}
	/* Thanks god you are using a decent protocol version */
	*x = (int)((long)(event->xclient.data.l[3]) & 0xffff);
	*y = (int)((long)(event->xclient.data.l[3]) / 65536);
}

void DndDropCoordinates(Widget widget, XEvent * event, int *x, int *y)
{
	int root_x, root_y;
	Window child_return;

	DndDropRootCoordinates(event, &root_x, &root_y);
	XTranslateCoordinates(dpy, DefaultRootWindow(dpy),
			      XtWindow(widget),
			      root_x, root_y, x, y, &child_return);
}

long DndProtocolVersion(XEvent * event)
{
	if (!DndIsDropMessage(event))
		return -1L;
	return event->xclient.data.l[4];
}

int DndIsDropMessage(XEvent * event)
{
	if (event->xclient.type != ClientMessage)
		return 0;
	if (event->xclient.message_type == OldDndProtocol &&
	    event->xclient.data.l[4] == 0)
		return 1;
	if (event->xclient.message_type == DndProtocol)
		return 1;
	return 0;
}

void
DndChangeCursor(int Type, int width, int height, char *image, char *mask,
		int hot_x, int hot_y)
{
	DndCursor[Type].ImagePixmap =
	    XCreateBitmapFromData(dpy, DefaultRootWindow(dpy),
				  image, width, height);
	DndCursor[Type].MaskPixmap =
	    XCreateBitmapFromData(dpy, DefaultRootWindow(dpy),
				  mask, width, height);
	DndCursor[Type].CursorID =
	    XCreatePixmapCursor(dpy, DndCursor[Type].ImagePixmap,
				DndCursor[Type].MaskPixmap,
				&Black, &White, hot_x, hot_y);
}
