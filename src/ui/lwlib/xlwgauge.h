/* Gauge Widget for SXEmacs.
   Copyright (C) 1999 Edward A. Falk

This file is part of SXEmacs.

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

/* Synched up with: Gauge.h 1.1 */

/*
 * Gauge.h - Gauge widget
 *
 * Author: Edward A. Falk
 *         falk@falconer.vip.best.com
 *
 * Date:   July 8, 1997
 */

#ifndef _XawGauge_h
#define _XawGauge_h

/***********************************************************************
 *
 * Gauge Widget
 *
 * The Gauge widget looks something like a thermometer.  Application
 * defines the values at the ends of the range and the current value
 * and Gauge draws accordingly.  Gauge does not accept input.
 *
 ***********************************************************************/

#include ATHENA_Label_h_

/* Resources:

 Name			Class		RepType		Default Value
 ----			-----		-------		-------------
 value			Value		Cardinal	0
 minValue		MinValue	Cardinal	0
 maxValue		MaxValue	Cardinal	100
 ntics			NTics		Cardinal	0	+
 nlabels		NLabels		Cardinal	0	++
 labels			Labels		String *	NULL	+++
 orientation		Orientation	XtOrientation	horizontal
 autoScaleUp		AutoScaleUp	Boolean		FALSE	++++
 autoScaleDown		AutoScaleDown	Boolean		FALSE	++++
 getValue		Callback	XtCallbackList	NULL	+++++
 update			Interval	int		0 (seconds) = disabled

 encoding		Encoding	unsigned char	XawTextEncoding8bit
 font			Font		XFontStruct*	XtDefaultFont
 foreground		Foreground	Pixel		XtDefaultForeground
 internalHeight		Height		Dimension	2
 internalWidth		Width		Dimension	4
 resize			Resize		Boolean		True
 background		Background	Pixel		XtDefaultBackground
 bitmap			Pixmap		Pixmap		None
 border			BorderColor	Pixel		XtDefaultForeground
 borderWidth		BorderWidth	Dimension	1
 cursor			Cursor		Cursor		None
 cursorName		Cursor		String		NULL
 destroyCallback	Callback	XtCallbackList	NULL
 height			Height		Dimension	varies
 insensitiveBorder	Insensitive	Pixmap		Gray
 mappedWhenManaged	MappedWhenManaged Boolean		True
 pointerColor		Foreground	Pixel		XtDefaultForeground
 pointerColorBackground	Background	Pixel		XtDefaultBackground
 sensitive		Sensitive	Boolean		True
 width			Width		Dimension	text width
 x			Position	Position	0
 y			Position	Position	0

 +   Ntics sets the number of tic marks next to the gauge.  If 0, no
     tic marks will be drawn.
 ++  Nlabels sets the number of labels next to the gauge.
 +++ Labels is an array of nul-terminated strings to be used as labels.
     If this field is NULL but nlabels is > 0, then numeric labels will be
     provided.  NOTE: the labels are not copied to any internal memory; they
     must be stored in static memory provided by the application.
 ++++ AutoScale allows the gauge to set its own value limits.  Default is
      False unless upper & lower limits are both 0.

 +++++ The GetValue() callback proc is called with these arguments:
	static void
	myGetValue(gauge, client, rval)
		Widget	gauge ;
		XtPointer client ;
		XtPointer rval ;
	{
	  *(Cardinal *)rval = value ;
	}

*/

/*
 * Resource names not provided in StringDefs.h
 */

#ifndef	XtNvalue
#define	XtNvalue	"value"
#define	XtCValue	"Value"
#endif

#ifndef	XtNorientation
#define	XtNorientation	"orientation"
#define	XtCOrientation	"Orientation"
#endif

#define	XtNntics	"ntics"
#define	XtCNTics	"NTics"

#ifndef	XtNnlabels
#define	XtNnlabels	"nlabels"
#define	XtCNLabels	"NLabels"
#endif
#ifndef	XtNlabels
#define	XtNlabels	"labels"
#define	XtCLabels	"Labels"
#endif

#ifndef	XtNminValue
#define	XtNminValue	"minValue"
#define	XtCMinValue	"MinValue"
#endif
#ifndef	XtNmaxValue
#define	XtNmaxValue	"maxValue"
#define	XtCMaxValue	"MaxValue"
#endif

#ifndef	XtNautoScaleUp
#define	XtNautoScaleUp		"autoScaleUp"
#define	XtCAutoScaleUp		"AutoScaleUp"
#define	XtNautoScaleDown	"autoScaleDown"
#define	XtCAutoScaleDown	"AutoScaleDown"
#endif

#ifndef	XtNupdate
#define	XtNupdate	"update"
#endif

#ifndef	XtNgetValue
#define	XtNgetValue	"getValue"
#endif

/* Class record constants */

extern WidgetClass gaugeWidgetClass;

typedef struct _GaugeClassRec *GaugeWidgetClass;
typedef struct _GaugeRec *GaugeWidget;

_XFUNCPROTOBEGIN extern void XawGaugeSetValue(
#if NeedFunctionPrototypes
						     Widget gauge,
						     Cardinal value
#endif
    );

extern Cardinal XawGaugeGetValue(
#if NeedFunctionPrototypes
					Widget gauge
#endif
    );

_XFUNCPROTOEND
#endif				/* _XawGauge_h */
