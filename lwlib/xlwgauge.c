/* Gauge Widget for XEmacs.
   Copyright (C) 1999 Edward A. Falk

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Gauge.c 1.2 */

/*
 * Gauge.c - Gauge widget
 *
 * Author: Edward A. Falk
 *         falk@falconer.vip.best.com
 *
 * Date:   July 9, 1997
 *
 * Note: for fun and demonstration purposes, I have added selection
 * capabilities to this widget.  If you select the widget, you create
 * a primary selection containing the current value of the widget in
 * both integer and string form.  If you copy into the widget, the
 * primary selection is converted to an integer value and the gauge is
 * set to that value.
 */

/* TODO:  display time instead of value
 */

#define	DEF_LEN	50	/* default width (or height for vertical gauge) */
#define	MIN_LEN	10	/* minimum reasonable width (height) */
#define	TIC_LEN	6	/* length of tic marks */
#define	GA_WID	3	/* width of gauge */
#define	MS_PER_SEC 1000

#include <config.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <X11/IntrinsicP.h>
#include <X11/Xatom.h>
#include <X11/StringDefs.h>
#include ATHENA_XawInit_h_
#include "xlwgaugeP.h"
#include "../src/xmu.h"
#ifdef HAVE_XMU
#include <X11/Xmu/Atoms.h>
#include <X11/Xmu/Drawing.h>
#include <X11/Xmu/StdSel.h>
#endif


/****************************************************************
 *
 * Gauge resources
 *
 ****************************************************************/


static	char	defaultTranslations[] =
	"<Btn1Up>:	select()\n\
	 <Key>F1:	select(CLIPBOARD)\n\
	 <Btn2Up>:	paste()\n\
	 <Key>F2:	paste(CLIPBOARD)" ;



#define offset(field) XtOffsetOf(GaugeRec, field)
static XtResource resources[] = {
    {XtNvalue, XtCValue, XtRInt, sizeof(int),
	offset(gauge.value), XtRImmediate, (XtPointer)0},
    {XtNminValue, XtCMinValue, XtRInt, sizeof(int),
	offset(gauge.v0), XtRImmediate, (XtPointer)0},
    {XtNmaxValue, XtCMaxValue, XtRInt, sizeof(int),
	offset(gauge.v1), XtRImmediate, (XtPointer)100},
    {XtNntics, XtCNTics, XtRInt, sizeof(int),
	offset(gauge.ntics), XtRImmediate, (XtPointer) 0},
    {XtNnlabels, XtCNLabels, XtRInt, sizeof(int),
	offset(gauge.nlabels), XtRImmediate, (XtPointer) 0},
    {XtNlabels, XtCLabels, XtRStringArray, sizeof(String *),
	offset(gauge.labels), XtRStringArray, NULL},
    {XtNautoScaleUp, XtCAutoScaleUp, XtRBoolean, sizeof(Boolean),
	offset(gauge.autoScaleUp), XtRImmediate, FALSE},
    {XtNautoScaleDown, XtCAutoScaleDown, XtRBoolean, sizeof(Boolean),
	offset(gauge.autoScaleDown), XtRImmediate, FALSE},
    {XtNorientation, XtCOrientation, XtROrientation, sizeof(XtOrientation),
	offset(gauge.orientation), XtRImmediate, (XtPointer)XtorientHorizontal},
    {XtNupdate, XtCInterval, XtRInt, sizeof(int),
	offset(gauge.update), XtRImmediate, (XtPointer)0},
    {XtNgetValue, XtCCallback, XtRCallback, sizeof(XtPointer),
	offset(gauge.getValue), XtRImmediate, (XtPointer)NULL},
};
#undef offset



	/* member functions */

static void GaugeClassInit (void);
static void GaugeInit (Widget, Widget, ArgList, Cardinal *);
static void GaugeDestroy (Widget);
static void GaugeResize (Widget);
static void GaugeExpose (Widget, XEvent *, Region);
static Boolean GaugeSetValues (Widget, Widget, Widget, ArgList, Cardinal *);
static XtGeometryResult GaugeQueryGeometry (Widget, XtWidgetGeometry *,
					    XtWidgetGeometry *);

	/* action procs */

static void GaugeSelect (Widget, XEvent *, String *, Cardinal *);
static void GaugePaste  (Widget, XEvent *, String *, Cardinal *);

	/* internal privates */

static void GaugeSize (GaugeWidget, Dimension *, Dimension *, Dimension);
static void MaxLabel  (GaugeWidget, Dimension *, Dimension *,
		       Dimension *, Dimension *);
static void AutoScale     (GaugeWidget);
static void EnableUpdate  (GaugeWidget);
static void DisableUpdate (GaugeWidget);

static void GaugeGetValue (XtPointer, XtIntervalId *);
static void GaugeMercury (Display *, Window, GC, GaugeWidget, Cardinal, Cardinal);

static Boolean GaugeConvert (Widget, Atom *, Atom *, Atom *,
			     XtPointer *, unsigned long *, int *);
static void GaugeLoseSel (Widget, Atom *);
static void GaugeDoneSel (Widget, Atom *, Atom *);
static void GaugeGetSelCB (Widget, XtPointer, Atom *, Atom *,
			   XtPointer, unsigned long *, int *);

static GC Get_GC (GaugeWidget, Pixel);


static	XtActionsRec	actionsList[] =
{
  {"select",	GaugeSelect},
  {"paste",	GaugePaste},
} ;



/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

GaugeClassRec gaugeClassRec = {
  {
/* core_class fields */
    /* superclass	  	*/	(WidgetClass) &labelClassRec,
    /* class_name	  	*/	"Gauge",
    /* widget_size	  	*/	sizeof(GaugeRec),
    /* class_initialize   	*/	GaugeClassInit,
    /* class_part_initialize	*/	NULL,
    /* class_inited       	*/	FALSE,
    /* initialize	  	*/	GaugeInit,
    /* initialize_hook		*/	NULL,
    /* realize		  	*/	XtInheritRealize,	/* TODO? */
    /* actions		  	*/	actionsList,
    /* num_actions	  	*/	XtNumber(actionsList),
    /* resources	  	*/	resources,
    /* num_resources	  	*/	XtNumber(resources),
    /* xrm_class	  	*/	NULLQUARK,
    /* compress_motion	  	*/	TRUE,
    /* compress_exposure  	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest	  	*/	FALSE,
    /* destroy		  	*/	GaugeDestroy,
    /* resize		  	*/	GaugeResize,
    /* expose		  	*/	GaugeExpose,
    /* set_values	  	*/	GaugeSetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus	 	*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private   	*/	NULL,
    /* tm_table		   	*/	defaultTranslations,
    /* query_geometry		*/	GaugeQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
/* Simple class fields initialization */
  {
    /* change_sensitive		*/	XtInheritChangeSensitive
  },
#ifdef	_ThreeDP_h
/* ThreeD class fields initialization */
  {
    XtInheritXaw3dShadowDraw	/* shadowdraw 		*/
  },
#endif
/* Label class fields initialization */
  {
    /* ignore 			*/	0
  },
/* Gauge class fields initialization */
  {
    /* extension		*/	NULL
  },
};

WidgetClass gaugeWidgetClass = (WidgetClass)&gaugeClassRec;




/****************************************************************
 *
 * Member Procedures
 *
 ****************************************************************/

static void
GaugeClassInit (void)
{
    XawInitializeWidgetSet();
#ifdef HAVE_XMU
    XtAddConverter(XtRString, XtROrientation, XmuCvtStringToOrientation,
    		NULL, 0) ;
#endif
}



/* ARGSUSED */
static void
GaugeInit (Widget   request,
	   Widget   new,
	   ArgList  args,
	   Cardinal *num_args)
{
    GaugeWidget gw = (GaugeWidget) new;

    if( gw->gauge.v0 == 0  &&  gw->gauge.v1 == 0 ) {
      gw->gauge.autoScaleUp = gw->gauge.autoScaleDown = TRUE ;
      AutoScale(gw) ;
    }

    /* If size not explicitly set, set it to our preferred size now.  */

    if( request->core.width == 0  ||  request->core.height == 0 )
    {
      Dimension w,h ;
      GaugeSize(gw, &w,&h, DEF_LEN) ;
      if( request->core.width == 0 )
	new->core.width = w ;
      if( request->core.height == 0 )
	new->core.height = h ;
      gw->core.widget_class->core_class.resize(new) ;
    }

    gw->gauge.selected = None ;
    gw->gauge.selstr = NULL ;

    if( gw->gauge.update > 0 )
      EnableUpdate(gw) ;

    gw->gauge.inverse_GC = Get_GC(gw, gw->core.background_pixel) ;
}

static void
GaugeDestroy (Widget w)
{
	GaugeWidget gw = (GaugeWidget)w;

	if( gw->gauge.selstr != NULL )
	  XtFree(gw->gauge.selstr) ;

	if( gw->gauge.selected != None )
	  XtDisownSelection(w, gw->gauge.selected, CurrentTime) ;

	XtReleaseGC(w, gw->gauge.inverse_GC) ;

	if( gw->gauge.update > 0 )
	  DisableUpdate(gw) ;
}


/* React to size change from manager.  Label widget will compute some
 * internal stuff, but we need to override.
 */

static void
GaugeResize (Widget w)
{
	GaugeWidget gw = (GaugeWidget)w;
	int	size ;		/* height (width) of gauge */
	int	vmargin ;	/* vertical (horizontal) margin */
	int	hmargin ;	/* horizontal (vertical) margin */

	vmargin = gw->gauge.orientation == XtorientHorizontal ?
	  gw->label.internal_height : gw->label.internal_width ;
	hmargin = gw->gauge.orientation == XtorientHorizontal ?
	  gw->label.internal_width : gw->label.internal_height ;

	/* TODO: need to call parent resize proc?  I don't think so since
	 * we're recomputing everything from scratch anyway.
	 */

	/* find total height (width) of contents */

	size = GA_WID+2 ;			/* gauge itself + edges */

	if( gw->gauge.ntics > 1 )		/* tic marks */
	  size += vmargin + TIC_LEN ;

	if( gw->gauge.nlabels > 1 )
	{
	  Dimension	lwm, lw0, lw1 ;	/* width of max, left, right labels */
	  Dimension	lh ;

	  MaxLabel(gw,&lwm,&lh, &lw0,&lw1) ;

	  if( gw->gauge.orientation == XtorientHorizontal )
	  {
	    gw->gauge.margin0 = lw0 / 2 ;
	    gw->gauge.margin1 = lw1 / 2 ;
	    size += lh + vmargin ;
	  }
	  else
	  {
	    gw->gauge.margin0 =
	    gw->gauge.margin1 = lh / 2 ;
	    size += lwm + vmargin ;
	  }
	}
	else
	  gw->gauge.margin0 = gw->gauge.margin1 = 0 ;

	gw->gauge.margin0 += hmargin ;
	gw->gauge.margin1 += hmargin ;

	/* Now distribute height (width) over components */

	if( gw->gauge.orientation == XtorientHorizontal )
	  gw->gauge.gmargin = (gw->core.height-size)/2 ;
	else
	  gw->gauge.gmargin = (gw->core.width-size)/2 ;

	gw->gauge.tmargin = gw->gauge.gmargin + GA_WID+2 + vmargin ;
	if( gw->gauge.ntics > 1 )
	  gw->gauge.lmargin = gw->gauge.tmargin + TIC_LEN + vmargin ;
	else
	  gw->gauge.lmargin = gw->gauge.tmargin ;
}

/*
 * Repaint the widget window
 */

/* ARGSUSED */
static void
GaugeExpose (Widget w,
	     XEvent *event,
	     Region region)
{
	GaugeWidget gw = (GaugeWidget) w;
register Display *dpy = XtDisplay(w) ;
register Window	win = XtWindow(w) ;
	GC	gc;	/* foreground, background */
	GC	gctop, gcbot ;	/* dark, light shadows */

	int	len ;		/* length (width or height) of widget */
	int	hgt ;		/* height (width) of widget */
	int	e0,e1 ;		/* ends of the gauge */
	int	x ;
	int	y ;		/* vertical (horizontal) position */
	int	i ;
	int	v0 = gw->gauge.v0 ;
	int	v1 = gw->gauge.v1 ;
	int	value = gw->gauge.value ;

	gc = XtIsSensitive(w) ? gw->label.normal_GC : gw->label.gray_GC ;


#ifdef	_ThreeDP_h
	gctop = gw->threeD.bot_shadow_GC ;
	gcbot = gw->threeD.top_shadow_GC ;
#else
	gctop = gcbot = gc ;
#endif

	if( gw->gauge.orientation == XtorientHorizontal ) {
	  len = gw->core.width ;
	  hgt = gw->core.height ;
	} else {
	  len = gw->core.height ;
	  hgt = gw->core.width ;
	}

	/* if the gauge is selected, signify by drawing the background
	 * in a contrasting color.
	 */

	if( gw->gauge.selected )
	{
	  XFillRectangle(dpy,win, gc, 0,0, w->core.width,w->core.height) ;
	  gc = gw->gauge.inverse_GC ;
	}

	e0 = gw->gauge.margin0 ;		/* left (top) end */
	e1 = len - gw->gauge.margin1 -1 ;	/* right (bottom) end */

	/* Draw the Gauge itself */

	y = gw->gauge.gmargin ;

	if( gw->gauge.orientation == XtorientHorizontal )	/* horizontal */
	{
	  XDrawLine(dpy,win,gctop, e0+1,y, e1-1,y) ;
	  XDrawLine(dpy,win,gctop, e0,y+1, e0,y+GA_WID) ;
	  XDrawLine(dpy,win,gcbot, e0+1, y+GA_WID+1, e1-1, y+GA_WID+1) ;
	  XDrawLine(dpy,win,gcbot, e1,y+1, e1,y+GA_WID) ;
	}
	else							/* vertical */
	{
	  XDrawLine(dpy,win,gctop, y,e0+1, y,e1-1) ;
	  XDrawLine(dpy,win,gctop, y+1,e0, y+GA_WID,e0) ;
	  XDrawLine(dpy,win,gcbot, y+GA_WID+1,e0+1, y+GA_WID+1, e1-1) ;
	  XDrawLine(dpy,win,gcbot, y+1,e1, y+GA_WID,e1) ;
	}


		/* draw the mercury */

	GaugeMercury(dpy, win, gc, gw, 0,value) ;


	if( gw->gauge.ntics > 1 )
	{
	  y = gw->gauge.tmargin ;
	  for(i=0; i<gw->gauge.ntics; ++i)
	  {
	    x = e0 + i*(e1-e0-1)/(gw->gauge.ntics-1) ;
	    if( gw->gauge.orientation == XtorientHorizontal ) {
	      XDrawLine(dpy,win,gcbot, x,y+1, x,y+TIC_LEN-2) ;
	      XDrawLine(dpy,win,gcbot, x,y, x+1,y) ;
	      XDrawLine(dpy,win,gctop, x+1,y+1, x+1,y+TIC_LEN-2) ;
	      XDrawLine(dpy,win,gctop, x,y+TIC_LEN-1, x+1,y+TIC_LEN-1) ;
	    }
	    else {
	      XDrawLine(dpy,win,gcbot, y+1,x, y+TIC_LEN-2,x) ;
	      XDrawLine(dpy,win,gcbot, y,x, y,x+1) ;
	      XDrawLine(dpy,win,gctop, y+1,x+1, y+TIC_LEN-2,x+1) ;
	      XDrawLine(dpy,win,gctop, y+TIC_LEN-1,x, y+TIC_LEN-1,x+1) ;
	    }
	  }
	}

	/* draw labels */
	if( gw->gauge.nlabels > 1 )
	{
	  char	label[20], *s = label ;
	  int	xlen, wd,h =0 ;

	  if( gw->gauge.orientation == XtorientHorizontal )
	    y = gw->gauge.lmargin + gw->label.font->max_bounds.ascent - 1 ;
	  else {
	    y = gw->gauge.lmargin ;
	    h = gw->label.font->max_bounds.ascent / 2 ;
	  }

	  for(i=0; i<gw->gauge.nlabels; ++i)
	  {
	    if( gw->gauge.labels == NULL )
	      sprintf(label, "%d", v0+i*(v1 - v0)/(gw->gauge.nlabels - 1)) ;
	    else
	      s = gw->gauge.labels[i] ;
	    if( s != NULL ) {
	      x = e0 + i*(e1-e0-1)/(gw->gauge.nlabels-1) ;
	      xlen = strlen(s) ;
	      if( gw->gauge.orientation == XtorientHorizontal ) {
		wd = XTextWidth(gw->label.font, s, xlen) ;
		XDrawString(dpy,win,gc, x-wd/2,y, s,xlen) ;
	      }
	      else {
		XDrawString(dpy,win,gc, y,x+h, s,xlen) ;
	      }
	    }
	  }
	}
}


/*
 * Set specified arguments into widget
 */

static Boolean
GaugeSetValues (Widget   old,
		Widget   request,
		Widget   new,
		ArgList  args,
		Cardinal *num_args)
{
	GaugeWidget oldgw = (GaugeWidget) old;
	GaugeWidget gw = (GaugeWidget) new;
	Boolean was_resized = False;

	if( gw->gauge.selected != None ) {
	  XtDisownSelection(new, gw->gauge.selected, CurrentTime) ;
	  gw->gauge.selected = None ;
	}

	/* Changes to v0,v1,labels, ntics, nlabels require resize & redraw. */
	/* Change to value requires redraw and possible resize if autoscale */

	was_resized =
	  gw->gauge.v0 != oldgw->gauge.v0  ||
	  gw->gauge.v1 != oldgw->gauge.v1  ||
	  gw->gauge.ntics != oldgw->gauge.ntics  ||
	  gw->gauge.nlabels != oldgw->gauge.nlabels  ||
	  gw->gauge.labels != oldgw->gauge.labels ;

	if( (gw->gauge.autoScaleUp && gw->gauge.value > gw->gauge.v1) ||
	    (gw->gauge.autoScaleDown && gw->gauge.value < gw->gauge.v1/3 ))
	{
	  AutoScale(gw) ;
	  was_resized = TRUE ;
	}

	if( was_resized ) {
	  if( gw->label.resize )
	    GaugeSize(gw, &gw->core.width, &gw->core.height, DEF_LEN) ;
	  else
	    GaugeResize(new) ;
	}

	if( gw->gauge.update != oldgw->gauge.update )
	  {
	    if( gw->gauge.update > 0 )
	      EnableUpdate(gw) ;
	    else
	      DisableUpdate(gw) ;
	  }

	if( gw->core.background_pixel != oldgw->core.background_pixel )
	{
	  XtReleaseGC(new, gw->gauge.inverse_GC) ;
	  gw->gauge.inverse_GC = Get_GC(gw, gw->core.background_pixel) ;
	}

	return was_resized || gw->gauge.value != oldgw->gauge.value  ||
	   XtIsSensitive(old) != XtIsSensitive(new);
}


static XtGeometryResult
GaugeQueryGeometry (Widget w,
		    XtWidgetGeometry *intended,
		    XtWidgetGeometry *preferred)
{
    register GaugeWidget gw = (GaugeWidget)w;

    if( intended->width == w->core.width  &&
	intended->height == w->core.height )
      return XtGeometryNo ;

    preferred->request_mode = CWWidth | CWHeight;
    GaugeSize(gw, &preferred->width, &preferred->height, DEF_LEN) ;

    if( (!(intended->request_mode & CWWidth) ||
	  intended->width >= preferred->width)  &&
	(!(intended->request_mode & CWHeight) ||
	  intended->height >= preferred->height) )
      return XtGeometryYes;
    else
      return XtGeometryAlmost;
}




/****************************************************************
 *
 * Action Procedures
 *
 ****************************************************************/

static void
GaugeSelect (Widget   w,
	     XEvent   *event,
	     String   *params,
	     Cardinal *num_params)
{
	GaugeWidget	gw = (GaugeWidget)w ;
	Atom		seln = XA_PRIMARY ;

	if( gw->gauge.selected != None ) {
	  XtDisownSelection(w, gw->gauge.selected, CurrentTime) ;
	  gw->gauge.selected = None ;
	}

	if( *num_params > 0 ) {
	  seln = XInternAtom(XtDisplay(w), params[0], False) ;
	  printf("atom %s is %ld\n", params[0], seln) ;
	}

	if( ! XtOwnSelection(w, seln, event->xbutton.time, GaugeConvert,
			GaugeLoseSel, GaugeDoneSel) )
	{
	  /* in real code, this error message would be replaced by
	   * something more elegant, or at least deleted
	   */

	  fprintf(stderr, "Gauge failed to get selection, try again\n") ;
	}
	else
	{
	  gw->gauge.selected = TRUE ;
	  gw->gauge.selstr = (String)XtMalloc(4*sizeof(int)) ;
	  sprintf(gw->gauge.selstr, "%d", gw->gauge.value) ;
	  GaugeExpose(w,0,0) ;
	}
}


static	Boolean
GaugeConvert (Widget	w,
	      Atom	*selection,	/* usually XA_PRIMARY */
	      Atom	*target,	/* requested target */
	      Atom	*type,		/* returned type */
	      XtPointer *value,		/* returned value */
	      unsigned long	*length,	/* returned length */
	      int	*format)	/* returned format */
{
	GaugeWidget	gw = (GaugeWidget)w ;
	XSelectionRequestEvent *req ;

	printf( "requesting selection %s:%s\n",
	    XGetAtomName(XtDisplay(w),*selection),
	    XGetAtomName(XtDisplay(w),*target));

#ifdef HAVE_XMU
	if( *target == XA_TARGETS(XtDisplay(w)) )
	{
	  Atom *rval, *stdTargets ;
	  unsigned long stdLength ;

	  /* XmuConvertStandardSelection can handle this.  This function
	   * will return a list of standard targets.  We prepend TEXT,
	   * STRING and INTEGER to the list and return it.
	   */

	  req = XtGetSelectionRequest(w, *selection, NULL) ;
	  XmuConvertStandardSelection(w, req->time, selection, target,
	  	type, (XPointer*)&stdTargets, &stdLength, format) ;

	  *type = XA_ATOM ;		/* TODO: needed? */
	  *length = stdLength + 3 ;
	  rval = (Atom *) XtMalloc(sizeof(Atom)*(stdLength+3)) ;
	  *value = (XtPointer) rval ;
	  *rval++ = XA_INTEGER ;
	  *rval++ = XA_STRING ;
	  *rval++ = XA_TEXT(XtDisplay(w)) ;
	  memcpy((char *)rval, (char *)stdTargets, stdLength*sizeof(Atom)) ;
	  XtFree((char*) stdTargets) ;
	  *format = 8*sizeof(Atom) ;	/* TODO: needed? */
	  return True ;
	}

	else
#endif
	  if( *target == XA_INTEGER )
	{
	  *type = XA_INTEGER ;
	  *length = 1 ;
	  *value = (XtPointer) &gw->gauge.value ;
	  *format = 8*sizeof(int) ;
	  return True ;
	}

	else if( *target == XA_STRING
#ifdef HAVE_XMU
		 ||
		 *target == XA_TEXT(XtDisplay(w))
#endif
		 )
	{
	  *type = *target ;
	  *length = strlen(gw->gauge.selstr)*sizeof(char) ;
	  *value = (XtPointer) gw->gauge.selstr ;
	  *format = 8 ;
	  return True ;
	}

	else
	{
	  /* anything else, we just give it to XmuConvertStandardSelection() */
#ifdef HAVE_XMU
	  req = XtGetSelectionRequest(w, *selection, NULL) ;
	  if( XmuConvertStandardSelection(w, req->time, selection, target,
	  	type, (XPointer *) value, length, format) )
	    return True ;
	  else
#endif
	    {
	    printf(
		"Gauge: requestor is requesting unsupported selection %s:%s\n",
	    	XGetAtomName(XtDisplay(w),*selection),
		XGetAtomName(XtDisplay(w),*target));
	    return False ;
	  }
	}
}



static	void
GaugeLoseSel (Widget w,
	      Atom   *selection)	/* usually XA_PRIMARY */
{
	GaugeWidget	gw = (GaugeWidget)w ;
	Display *dpy = XtDisplay(w) ;
	Window	win = XtWindow(w) ;

	if( gw->gauge.selstr != NULL ) {
	  XtFree(gw->gauge.selstr) ;
	  gw->gauge.selstr = NULL ;
	}

	gw->gauge.selected = False ;
	XClearWindow(dpy,win) ;
	GaugeExpose(w,0,0) ;
}


static	void
GaugeDoneSel (Widget w,
	      Atom   *selection,	/* usually XA_PRIMARY */
	      Atom   *target)		/* requested target */
{
	/* selection done, anything to do? */
}


static void
GaugePaste (Widget   w,
	    XEvent   *event,
	    String   *params,
	    Cardinal *num_params)
{
	Atom		seln = XA_PRIMARY ;

	if( *num_params > 0 ) {
	  seln = XInternAtom(XtDisplay(w), params[0], False) ;
	  printf("atom %s is %ld\n", params[0], seln) ;
	}

	/* try for integer value first */
	XtGetSelectionValue(w, seln, XA_INTEGER,
		GaugeGetSelCB, (XtPointer)XA_INTEGER,
		event->xbutton.time) ;
}

static	void
GaugeGetSelCB (Widget    w,
	       XtPointer client,
	       Atom      *selection,
	       Atom      *type,
	       XtPointer value,
	       unsigned long    *length,
	       int       *format)
{
	Display	*dpy = XtDisplay(w) ;
	Atom	target = (Atom)client ;
	int	*iptr ;
	char	*cptr ;

	if( *type == XA_INTEGER ) {
	  iptr = (int *)value ;
	  XawGaugeSetValue(w, *iptr) ;
	}

	else if( *type == XA_STRING
#ifdef HAVE_XMU
		 ||
		 *type == XA_TEXT(dpy)
#endif
		 )
	  {
	  cptr = (char *)value ;
	  XawGaugeSetValue(w, atoi(cptr)) ;
	}

	/* failed, try string */
	else if( *type == None && target == XA_INTEGER )
	  XtGetSelectionValue(w, *selection, XA_STRING,
		GaugeGetSelCB, (XtPointer)XA_STRING,
		CurrentTime) ;
}



/****************************************************************
 *
 * Public Procedures
 *
 ****************************************************************/


	/* Change gauge value.  Only undraw or draw what needs to be
	 * changed.
	 */

void
XawGaugeSetValue (Widget   w,
		  Cardinal value)
{
	GaugeWidget gw = (GaugeWidget)w ;
	int	oldvalue ;
	GC	gc ;

	if( gw->gauge.selected != None ) {
	  XtDisownSelection(w, gw->gauge.selected, CurrentTime) ;
	  gw->gauge.selected = None ;
	}

	if( !XtIsRealized(w) ) {
	  gw->gauge.value = value ;
	  return ;
	}

	/* need to rescale? */
	if(( gw->gauge.autoScaleUp && value > gw->gauge.v1) ||
	   (gw->gauge.autoScaleDown && value < gw->gauge.v1/3 ))
	{
	  XtVaSetValues(w, XtNvalue, value, 0) ;
	  return ;
	}

	oldvalue = gw->gauge.value ;
	gw->gauge.value = value ;

	gc = XtIsSensitive(w) ? gw->label.normal_GC : gw->label.gray_GC ;
	GaugeMercury(XtDisplay(w), XtWindow(w), gc, gw, oldvalue,value) ;
}


Cardinal
XawGaugeGetValue (Widget w)
{
	GaugeWidget gw = (GaugeWidget)w ;
	return gw->gauge.value ;
}




/****************************************************************
 *
 * Private Procedures
 *
 ****************************************************************/

	/* draw the mercury over a specific region */

static	void
GaugeMercury (Display     *dpy,
	      Window      win,
	      GC          gc,
	      GaugeWidget gw,
	      Cardinal    val0,
	      Cardinal    val1)
{
	int	v0 = gw->gauge.v0 ;
	int	v1 = gw->gauge.v1 ;
	int	vd = v1 - v0 ;
	Dimension len ;		/* length (width or height) of gauge */
	Position e0, e1 ;	/* gauge ends */
	Position p0, p1 ;	/* mercury ends */
	int	y ;		/* vertical (horizontal) position */
	Boolean	undraw = FALSE ;

	len = gw->gauge.orientation == XtorientHorizontal ?
	  gw->core.width : gw->core.height ;

	e0 = gw->gauge.margin0 ;		/* left (top) end */
	e1 = len - gw->gauge.margin1 -1 ;	/* right (bottom) end */

	if( vd <= 0 ) vd = 1 ;

	if( val0 < v0 ) val0 = v0 ;
	else if( val0 > v1 ) val0 = v1 ;
	if( val1 < v0 ) val1 = v0 ;
	else if( val1 > v1 ) val1 = v1 ;

	p0 = (val0-v0)*(e1-e0-1)/vd ;
	p1 = (val1-v0)*(e1-e0-1)/vd ;

	if( p1 == p0 )
	  return ;

	y = gw->gauge.gmargin ;

	if( p1 < p0 )
	{
	  Position tmp = p0 ;
	  p0 = p1 ;
	  p1 = tmp ;
	  gc = gw->label.normal_GC ;
	  XSetForeground(dpy,gc, gw->core.background_pixel) ;
	  undraw = TRUE ;
	}

	if( gw->gauge.orientation == XtorientHorizontal )
	  XFillRectangle(dpy,win,gc, e0+p0+1,y+1, p1-p0,GA_WID) ;
	else
	  XFillRectangle(dpy,win,gc, y+1,e1-p1, GA_WID,p1-p0) ;

	if( undraw )
	  XSetForeground(dpy,gc, gw->label.foreground) ;
}



/* Search the labels, find the largest one. */
/* TODO: handle vertical fonts? */

static void
MaxLabel (GaugeWidget	gw,
	  Dimension	*wid,	/* max label width */
	  Dimension	*hgt,	/* max label height */
	  Dimension	*w0,	/* width of first label */
	  Dimension	*w1)	/* width of last label */
{
	char	lstr[80], *lbl ;
	int	w ;
	XFontStruct *font = gw->label.font ;
	int	i ;
	int	lw = 0;
	int	v0 = gw->gauge.v0 ;
	int	dv = gw->gauge.v1 - v0 ;
	int	n = gw->gauge.nlabels ;

	if( n > 0 )
	{
	  if( --n <= 0 ) {n = 1 ; v0 += dv/2 ;}

	  /* loop through all labels, figure out how much room they
	   * need.
	   */
	  w = 0 ;
	  for(i=0; i<gw->gauge.nlabels; ++i)
	  {
	    if( gw->gauge.labels == NULL )	/* numeric labels */
	      sprintf(lbl = lstr,"%d", v0 + i*dv/n) ;
	    else
	      lbl = gw->gauge.labels[i] ;

	    if( lbl != NULL ) {
	      lw = XTextWidth(font, lbl, strlen(lbl)) ;
	      w = Max( w, lw ) ;
	    }
	    else
	      lw = 0 ;

	    if( i == 0 && w0 != NULL ) *w0 = lw ;
	  }
	  if( w1 != NULL ) *w1 = lw ;

	  *wid = w ;
	  *hgt = font->max_bounds.ascent + font->max_bounds.descent ;
	}
	else
	  *wid = *hgt = 0 ;
}


/* Determine the preferred size for this widget.  choose 100x100 for
 * debugging.
 */

static void
GaugeSize (GaugeWidget gw,
	   Dimension   *wid,
	   Dimension   *hgt,
	   Dimension   min_len)
{
	int	w,h ;		/* width, height of gauge */
	int	vmargin ;	/* vertical margin */
	int	hmargin ;	/* horizontal margin */

	hmargin = gw->label.internal_width ;
	vmargin = gw->label.internal_height ;

	/* find total height (width) of contents */


	/* find minimum size for undecorated gauge */

	if( gw->gauge.orientation == XtorientHorizontal )
	{
	  w = min_len ;
	  h = GA_WID+2 ;			/* gauge itself + edges */
	}
	else
	{
	  w = GA_WID+2 ;
	  h = min_len ;
	}

	if( gw->gauge.ntics > 0 )
	{
	  if( gw->gauge.orientation == XtorientHorizontal )
	  {
	    w = Max(w, gw->gauge.ntics*3) ;
	    h += vmargin + TIC_LEN ;
	  }
	  else
	  {
	    w += hmargin + TIC_LEN ;
	    h = Max(h, gw->gauge.ntics*3) ;
	  }
	}


	/* If labels are requested, this gets a little interesting.
	 * We want the end labels centered on the ends of the gauge and
	 * the centers of the labels evenly spaced.  The labels at the ends
	 * will not be the same width, meaning that the gauge itself need
	 * not be centered in the widget.
	 *
	 * First, determine the spacing.  This is the width of the widest
	 * label, plus the internal margin.  Total length of the gauge is
	 * spacing * (nlabels-1).  To this, we add half the width of the
	 * left-most label and half the width of the right-most label
	 * to get the entire desired width of the widget.
	 */
	if( gw->gauge.nlabels > 0 )
	{
	  Dimension	lwm, lw0, lw1 ;	/* width of max, left, right labels */
	  Dimension	lh ;

	  MaxLabel(gw,&lwm,&lh, &lw0,&lw1) ;

	  if( gw->gauge.orientation == XtorientHorizontal )
	  {
	    lwm = (lwm+hmargin) * (gw->gauge.nlabels-1) + (lw0+lw1)/2 ;
	    w = Max(w, lwm) ;
	    h += lh + vmargin ;
	  }
	  else
	  {
	    lh = lh*gw->gauge.nlabels + (gw->gauge.nlabels - 1)*vmargin ;
	    h = Max(h, lh) ;
	    w += lwm + hmargin ;
	  }
	}

	w += hmargin*2 ;
	h += vmargin*2 ;

	*wid = w ;
	*hgt = h ;
}



static void
AutoScale (GaugeWidget gw)
{
	static int scales[3] = {1,2,5} ;
	int sptr = 0, smult=1 ;

	if( gw->gauge.autoScaleDown )
	  gw->gauge.v1 = 0 ;
	while( gw->gauge.value > gw->gauge.v1 )
	{
	  if( ++sptr > 2 ) {
	    sptr = 0 ;
	    smult *= 10 ;
	  }
	  gw->gauge.v1 = scales[sptr] * smult ;
	}
}

static	void
EnableUpdate (GaugeWidget gw)
{
	gw->gauge.intervalId =
	  XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)gw),
	  	gw->gauge.update * MS_PER_SEC, GaugeGetValue,
		(XtPointer)gw) ;
}

static	void
DisableUpdate (GaugeWidget gw)
{
	XtRemoveTimeOut(gw->gauge.intervalId) ;
}

static	void
GaugeGetValue (XtPointer    clientData,
	       XtIntervalId *intervalId)
{
	GaugeWidget	gw = (GaugeWidget)clientData ;
	Cardinal	value ;

	if( gw->gauge.update > 0 )
	  EnableUpdate(gw) ;

	if( gw->gauge.getValue != NULL )
	{
	  XtCallCallbackList((Widget)gw, gw->gauge.getValue, (XtPointer)&value);
	  XawGaugeSetValue((Widget)gw, value) ;
	}
}


static	GC
Get_GC (GaugeWidget gw,
	Pixel       fg)
{
	XGCValues	values ;
#define	vmask	GCForeground
#define	umask	(GCBackground|GCSubwindowMode|GCGraphicsExposures|GCDashOffset\
		|GCFont|GCDashList|GCArcMode)

	values.foreground = fg ;

	return XtAllocateGC((Widget)gw, 0, vmask, &values, 0L, umask) ;
}
