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

#ifndef __DragAndDropH__
#define __DragAndDropH__ 1L

/* The standard DND types are defined here */
#include "offix-types.h"

/* Xt stuff is defined here */
#include "xintrinsic.h"

void DndInitialize(Widget shell);

int DndHandleDragging(Widget widget, XEvent * event);

void DndSetData(int Type, unsigned char *Data, unsigned long Size);
void DndGetData(XEvent * event, unsigned char **Data, unsigned long *Size);

int DndIsIcon(Widget widget);
int DndDataType(XEvent * event);
unsigned int DndDragButtons(XEvent * event);
Window DndSourceWindow(XEvent * event);

void DndDropCoordinates(Widget widget, XEvent * event, int *x, int *y);
void DndDropRootCoordinates(XEvent * event, int *x, int *y);

long DndProtocolVersion(XEvent * event);

int DndIsDropMessage(XEvent * event);

void
DndChangeCursor(int Type,
		int width, int height,
		char *image, char *mask, int hot_x, int hot_y);
#endif
