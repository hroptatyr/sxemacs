 /* Tabs Widget for XEmacs.
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

 /* Synched up with: Tabs.c 1.27.  

 #### This file contains essential XEmacs related fixes to the original
 verison of the Tabs widget. Be VERY careful about syncing if you ever
 update to a more recent version. In general this is probably now a
 bad idea. */

 /*
 * Tabs.c - Index Tabs composite widget
 *
 * Author: Edward A. Falk
 *	   falk@falconer.vip.best.com
 *
 * Date: July 29, 1997
 *
 *
 * Overall layout of this widget is as follows:
 *
 *   ________ ,---------. _________
 *  |  label ||  Label  ||  Label  |  \ tabs
 *  |________||         ||_________|  /
 *  |+----------------------------+|  \
 *  ||                            ||  |
 *  ||  child widget window       ||   > frame
 *  |+----------------------------+|  |
 *  +------------------------------+  /
 *
 * The height of the tabs includes the shadow width, top and bottom
 * margins, and the height of the text.
 *
 * The height of the frame includes the top and bottom shadow width and the
 * size of the child widget window.
 *
 * The tabs overlap the frame and each other vertically by the shadow
 * width, so that when the topmost tab is drawn, it obliterates part of
 * the frame.
 */

/*
 * TODO: min child height = tab height
 */

#include	<config.h>
#include	<stdio.h>

#include	<X11/Xlib.h>
#include	<X11/IntrinsicP.h>
#include	<X11/StringDefs.h>

#include	"lwlib-internal.h"
#include	"../src/xmu.h"
#include	"xlwtabsP.h"
#include	"xlwgcs.h"

#define	MIN_WID		10
#define	MIN_HGT		10
#define	INDENT		3	/* tabs indented from edge by this much */
#define	SPACING		0	/* distance between tabs */
#define	SHADWID		1	/* default shadow width */
#define	TABDELTA	2	/* top tab grows this many pixels */
#define	TABLDELTA	2	/* top tab label offset this many pixels */


/****************************************************************
 *
 * IndexTabs Resources
 *
 ****************************************************************/

static	char	defaultTranslations[] = "\
	<BtnUp>:		select()	\n\
	<FocusIn>:		highlight()	\n\
	<FocusOut>:		unhighlight()	\n\
	<Key>Page_Up:		page(up)	\n\
	<Key>KP_Page_Up:	page(up)	\n\
	<Key>Prior:		page(up)	\n\
	<Key>KP_Prior:		page(up)	\n\
	<Key>Page_Down:		page(down)	\n\
	<Key>KP_Page_Down:	page(down)	\n\
	<Key>Next:		page(down)	\n\
	<Key>KP_Next:		page(down)	\n\
	<Key>Home:		page(home)	\n\
	<Key>KP_Home:		page(home)	\n\
	<Key>End:		page(end)	\n\
	<Key>KP_End:		page(end)	\n\
	<Key>Up:		highlight(up)	\n\
	<Key>KP_Up:		highlight(up)	\n\
	<Key>Down:		highlight(down)	\n\
	<Key>KP_Down:		highlight(down)	\n\
	<Key> :			page(select)	\n\
	 " ;

static	char	accelTable[] = "	#augment\n\
	<Key>Page_Up:		page(up)	\n\
	<Key>KP_Page_Up:	page(up)	\n\
	<Key>Prior:		page(up)	\n\
	<Key>KP_Prior:		page(up)	\n\
	<Key>Page_Down:		page(down)	\n\
	<Key>KP_Page_Down:	page(down)	\n\
	<Key>Next:		page(down)	\n\
	<Key>KP_Next:		page(down)	\n\
	<Key>Home:		page(home)	\n\
	<Key>KP_Home:		page(home)	\n\
	<Key>End:		page(end)	\n\
	<Key>KP_End:		page(end)	\n\
	<Key>Up:		highlight(up)	\n\
	<Key>KP_Up:		highlight(up)	\n\
	<Key>Down:		highlight(down)	\n\
	<Key>KP_Down:		highlight(down)	\n\
	<Key> :			page(select)	\n\
	 " ;
static	XtAccelerators	defaultAccelerators ; /* #### Never used */

#define	offset(field)	XtOffsetOf(TabsRec, tabs.field)
static XtResource resources[] = {

  {XtNselectInsensitive, XtCSelectInsensitive, XtRBoolean, sizeof(Boolean),
	offset(selectInsensitive), XtRImmediate, (XtPointer) True},
  {XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
	offset(font), XtRString, (XtPointer) XtDefaultFont},
  {XtNinternalWidth, XtCWidth, XtRDimension, sizeof(Dimension),
	offset(internalWidth), XtRImmediate, (XtPointer)4 },
  {XtNinternalHeight, XtCHeight, XtRDimension, sizeof(Dimension),
	offset(internalHeight), XtRImmediate, (XtPointer)4 },
  {XtNborderWidth, XtCBorderWidth, XtRDimension, sizeof(Dimension),
	XtOffsetOf(RectObjRec,rectangle.border_width), XtRImmediate, (XtPointer)0},
  {XtNtopWidget, XtCTopWidget, XtRWidget, sizeof(Widget),
	offset(topWidget), XtRImmediate, NULL},
  {XtNhighlightWidget, XtCHighlightWidget, XtRWidget, sizeof(Widget),
	offset(hilight), XtRImmediate, NULL},
  {XtNcallback, XtCCallback, XtRCallback, sizeof(XtPointer),
	offset(callbacks), XtRCallback, NULL},
  {XtNpopdownCallback, XtCCallback, XtRCallback, sizeof(XtPointer),
	offset(popdownCallbacks), XtRCallback, NULL},
  {XtNbeNiceToColormap, XtCBeNiceToColormap, XtRBoolean, sizeof(Boolean),
	offset(be_nice_to_cmap), XtRImmediate, (XtPointer) True},
  {XtNtopShadowContrast, XtCTopShadowContrast, XtRInt, sizeof(int),
	offset(top_shadow_contrast), XtRImmediate, (XtPointer) 20},
  {XtNbottomShadowContrast, XtCBottomShadowContrast, XtRInt, sizeof(int),
	offset(bot_shadow_contrast), XtRImmediate, (XtPointer) 40},
  {XtNinsensitiveContrast, XtCInsensitiveContrast, XtRInt, sizeof(int),
	offset(insensitive_contrast), XtRImmediate, (XtPointer) 33},
  {XtNaccelerators, XtCAccelerators, XtRAcceleratorTable,sizeof(XtTranslations),
	XtOffsetOf(TabsRec,core.accelerators), XtRString, accelTable},
};
#undef	offset



	/* constraint resources */

#define	offset(field)	XtOffsetOf(TabsConstraintsRec, tabs.field)
static XtResource tabsConstraintResources[] = {
  {XtNtabLabel, XtCLabel, XtRString, sizeof(String),
	offset(label), XtRString, NULL},
  {XtNtabLeftBitmap, XtCLeftBitmap, XtRBitmap, sizeof(Pixmap),
	offset(left_bitmap), XtRImmediate, None},
  {XtNtabForeground, XtCForeground, XtRPixel, sizeof(Pixel),
	offset(foreground), XtRString, (XtPointer) XtDefaultForeground},
  {XtNresizable, XtCResizable, XtRBoolean, sizeof(Boolean),
	offset(resizable), XtRImmediate, (XtPointer) True},
} ;
#undef	offset




#if	!NeedFunctionPrototypes

	/* FORWARD REFERENCES: */

	/* member functions */

static	void	TabsClassInit();
static	void	TabsInit();
static	void	TabsResize();
static	void	TabsExpose();
static	void	TabsDestroy();
static	void	TabsRealize();
static	Boolean	TabsSetValues();
static	Boolean	TabsAcceptFocus();
static	XtGeometryResult	TabsQueryGeometry();
static	XtGeometryResult	TabsGeometryManager();
static	void	TabsChangeManaged();
static	void	TabsConstraintInitialize() ;
static	Boolean	TabsConstraintSetValues() ;

	/* action procs */

static	void	TabsSelect() ;
static	void	TabsPage() ;
static	void	TabsHighlight() ;
static	void	TabsUnhighlight() ;

	/* internal privates */

static	void	TabsAllocGCs() ;	/* get rendering GCs */
static	void	TabsFreeGCs() ;		/* return rendering GCs */
static	void	DrawTabs() ;		/* draw all tabs */
static	void	DrawTab() ;		/* draw one index tab */
static	void	DrawFrame() ;		/* draw frame around contents */
static	void	DrawTrim() ;		/* draw trim around a tab */
static	void	DrawBorder() ;		/* draw border */
static	void	DrawHighlight() ;	/* draw highlight */
static	void	UndrawTab() ;		/* undraw interior of a tab */
static	void	TabWidth() ;		/* recompute tab size */
static	void	GetPreferredSizes() ;	/* query all children for their sizes */
static	void	MaxChild() ;		/* find max preferred child size */
static	int	PreferredSize() ;	/* compute preferred size */
static	int	PreferredSize2() ;	/* compute preferred size */
static	int	PreferredSize3() ;	/* compute preferred size */
static	void	MakeSizeRequest() ;	/* try to change size */
static	void	getBitmapInfo() ;
static	int	TabLayout() ;		/* lay out tabs */
static	void	TabsShuffleRows() ;	/* bring current tab to bottom row */

static	void	TabsAllocFgGC() ;
static	void	TabsAllocGreyGC() ;

#else

static	void	TabsClassInit(void) ;
static	void	TabsInit( Widget req, Widget new, ArgList, Cardinal *nargs) ;
static	void	TabsConstraintInitialize(Widget, Widget, ArgList, Cardinal *) ;
static	void	TabsRealize(Widget, Mask *, XSetWindowAttributes *) ;
static	void	TabsDestroy( Widget w) ;
static	void	TabsResize( Widget w) ;
static	void	TabsExpose( Widget w, XEvent *event, Region region) ;
static	Boolean	TabsSetValues(Widget, Widget, Widget, ArgList, Cardinal *) ;
static	Boolean	TabsAcceptFocus(Widget, Time *);
static	Boolean	TabsConstraintSetValues(Widget, Widget, Widget,
			ArgList, Cardinal *) ;
static	XtGeometryResult TabsQueryGeometry(Widget,
				XtWidgetGeometry *, XtWidgetGeometry *) ;
static	XtGeometryResult TabsGeometryManager(Widget,
				XtWidgetGeometry *, XtWidgetGeometry *) ;
static	void	TabsChangeManaged( Widget w) ;

static	void	TabsSelect(Widget, XEvent *, String *, Cardinal *) ;
static	void	TabsPage(Widget, XEvent *, String *, Cardinal *) ;
static	void	TabsHighlight(Widget, XEvent *, String *, Cardinal *) ;
static	void	TabsUnhighlight(Widget, XEvent *, String *, Cardinal *) ;

static	void	DrawTabs( TabsWidget tw, Bool labels) ;
static	void	DrawTab( TabsWidget tw, Widget child, Bool labels) ;
static	void	DrawFrame( TabsWidget tw) ;
static	void	DrawTrim( TabsWidget, int x, int y,
		  int wid, int hgt, Bool bottom, Bool undraw) ;
static	void	DrawBorder( TabsWidget tw, Widget child, Bool undraw) ;
static	void	DrawHighlight( TabsWidget tw, Widget child, Bool undraw) ;
static	void	UndrawTab( TabsWidget tw, Widget child) ;

static	void	TabWidth( Widget w) ;
static	int	TabLayout( TabsWidget, Dimension wid, Dimension hgt, Dimension *r_hgt,
			Bool query_only) ;
static	void	GetPreferredSizes(TabsWidget) ;
static	void	MaxChild(TabsWidget, Widget except, Dimension, Dimension) ;
static	void	TabsShuffleRows( TabsWidget tw) ;
static	int	PreferredSize( TabsWidget,
			Dimension *reply_width, Dimension *reply_height,
			Dimension *reply_cw, Dimension *reply_ch) ;
static	int	PreferredSize2( TabsWidget, Dimension cw, Dimension ch,
			Dimension *rw, Dimension *rh) ;
static	int	PreferredSize3( TabsWidget, Dimension wid, Dimension hgt,
			Dimension *rw, Dimension *rh) ;
static	void	MakeSizeRequest(TabsWidget) ;

static	void	TabsAllocGCs(TabsWidget) ;
static	void	TabsFreeGCs(TabsWidget) ;
static	void	getBitmapInfo( TabsWidget tw, TabsConstraints tab) ;
static	void	TabsAllocFgGC( TabsWidget tw) ;
static	void	TabsAllocGreyGC( TabsWidget tw) ;

#endif

#define	AddRect(i,xx,yy,w,h)	\
  do{rects[(i)].x=(xx); rects[i].y=(yy);	\
     rects[i].width=(w); rects[i].height=(h);}while(0)

static	XtActionsRec	actionsList[] =
  {
    {"select",	TabsSelect},
    {"page",	TabsPage},
    {"highlight", TabsHighlight},
    {"unhighlight", TabsUnhighlight},
  } ;


/****************************************************************
*
* Full class record constant
*
****************************************************************/

#ifndef	NEED_MOTIF
#define	SuperClass	(&constraintClassRec)
#else
#define	SuperClass	(&xmManagerClassRec)
#endif

TabsClassRec tabsClassRec = {
  {
/* core_class fields      */
    /* superclass         */    (WidgetClass) SuperClass,
    /* class_name         */    "Tabs",
    /* widget_size        */    sizeof(TabsRec),
    /* class_initialize   */    TabsClassInit,
    /* class_part_init    */	NULL,			/* TODO? */
    /* class_inited       */	FALSE,
    /* initialize         */    TabsInit,
    /* initialize_hook    */	NULL,
    /* realize            */    TabsRealize,
    /* actions            */    actionsList,
    /* num_actions	  */	XtNumber(actionsList),
    /* resources          */    resources,
    /* num_resources      */    XtNumber(resources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion	  */	TRUE,
#if XtSpecificationRelease < 6
    /* compress_exposure  */	XtExposeCompressMaximal,
#else
    /* compress_exposure  */	XtExposeCompressMaximal|XtExposeNoRegion,
#endif
    /* compress_enterleave*/	TRUE,
    /* visible_interest   */    TRUE,
    /* destroy            */    TabsDestroy,
    /* resize             */    TabsResize,
    /* expose             */    TabsExpose,
    /* set_values         */    TabsSetValues,
    /* set_values_hook    */	NULL,
    /* set_values_almost  */    XtInheritSetValuesAlmost,
    /* get_values_hook    */	NULL,
    /* accept_focus       */    TabsAcceptFocus,
    /* version            */	XtVersion,
    /* callback_private   */    NULL,
    /* tm_table           */    defaultTranslations,
    /* query_geometry     */	TabsQueryGeometry,
    /* display_accelerator*/	XtInheritDisplayAccelerator,
    /* extension          */	NULL
  },
  {
/* composite_class fields */
    /* geometry_manager   */    TabsGeometryManager,
    /* change_managed     */    TabsChangeManaged,
    /* insert_child	  */	XtInheritInsertChild,	/* TODO? */
    /* delete_child	  */	XtInheritDeleteChild,	/* TODO? */
    /* extension          */	NULL
  },
  {
/* constraint_class fields */
    /* subresources	  */	tabsConstraintResources,
    /* subresource_count  */	XtNumber(tabsConstraintResources),
    /* constraint_size	  */	sizeof(TabsConstraintsRec),
    /* initialize	  */	TabsConstraintInitialize,
    /* destroy		  */	NULL,
    /* set_values	  */	TabsConstraintSetValues,
    /* extension	  */	NULL,
  },
#ifdef	NEED_MOTIF
/* Manager Class fields */
  {
    /* translations		*/	NULL,
    /* syn_resources		*/	NULL,
    /* num_syn_resources	*/	0,
    /* syn_constraint_resources	*/	NULL,
    /* num_syn_constraint_resources */	0,
    /* parent_process		*/	XmInheritParentProcess,
    /* extension		*/	NULL
  },
#endif
  {
/* Tabs class fields */
    /* extension	  */	NULL,
  }
};

WidgetClass tabsWidgetClass = (WidgetClass)&tabsClassRec;

#define TabsNumChildren(tw) (((TabsWidget)tw)->composite.num_children)
#define TabVisible(tab) \
	(XtIsManaged(tab) && \
	 ((TabsConstraints)((tab)->core.constraints))->tabs.visible)


/****************************************************************
 *
 * Member Procedures
 *
 ****************************************************************/

static void
TabsClassInit(void)
{
	defaultAccelerators = XtParseAcceleratorTable(accelTable) ;
	/* TODO: register converter for labels? */
}



	/* Init a newly created tabs widget.  Compute height of tabs
	 * and optionally compute size of widget. */

/* ARGSUSED */

static void
TabsInit(Widget request, Widget new, ArgList args, Cardinal *num_args)
{
    TabsWidget newTw = (TabsWidget)new;

    newTw->tabs.numRows = 0 ;
    newTw->tabs.realRows = 0;

    GetPreferredSizes(newTw) ;

    /* height is easy, it's the same for all tabs:
     *  TODO: font height + height of tallest bitmap.
     */
    newTw->tabs.tab_height = 2 * newTw->tabs.internalHeight + SHADWID ;

    if( newTw->tabs.font != NULL )
      newTw->tabs.tab_height += newTw->tabs.font->max_bounds.ascent +
				newTw->tabs.font->max_bounds.descent ;

    /* GC allocation is deferred until XtRealize() */

    /* if size not explicitly set, set it to our preferred size now. */

    if( request->core.width == 0 || request->core.height == 0 )
    {
      Dimension	w,h ;
      PreferredSize(newTw, &w, &h, NULL,NULL) ;
      if( request->core.width == 0 ) new->core.width = w ;
      if( request->core.height == 0 ) new->core.height = h ;
      XtClass(new)->core_class.resize(new) ;
    }

    /* defer GC allocation, etc., until Realize() time. */
    newTw->tabs.foregroundGC =
    newTw->tabs.backgroundGC =
    newTw->tabs.greyGC =
    newTw->tabs.topGC =
    newTw->tabs.botGC = None ;

    newTw->tabs.grey50 = None ;

    newTw->tabs.needs_layout = False ;

    newTw->tabs.hilight = NULL ;

#ifdef	NEED_MOTIF
    newTw->manager.navigation_type = XmTAB_GROUP ;
    newTw->manager.traversal_on = True ;
#endif
}


	/* Init the constraint part of a new tab child.  Compute the
	 * size of the tab.
	 */
/* ARGSUSED */
static	void
TabsConstraintInitialize(Widget request, Widget new,
	ArgList args, Cardinal *num_args)
{
	TabsConstraints tab = (TabsConstraints) new->core.constraints ;
	tab->tabs.greyAlloc = False ;	/* defer allocation of pixel */
	tab->tabs.visible = False ;

	getBitmapInfo((TabsWidget)XtParent(new), tab) ;
	TabWidth(new) ;
}



	/* Called when tabs widget first realized.  Create the window
	 * and allocate the GCs
	 */

static	void
TabsRealize(Widget w, Mask *valueMask, XSetWindowAttributes *attributes)
{
	TabsWidget tw = (TabsWidget) w;

	attributes->bit_gravity = NorthWestGravity;
	*valueMask |= CWBitGravity;

	SuperClass->core_class.realize(w, valueMask, attributes);

	TabsAllocGCs(tw) ;
}



static	void
TabsDestroy(Widget w)
{
	TabsFreeGCs((TabsWidget)w) ;
}


	/* Parent has resized us.  This will require that the tabs be
	 * laid out again.
	 */

static void
TabsResize(Widget w)
{
	TabsWidget	tw = (TabsWidget) w;
	int		i ;
	int		num_children = tw->composite.num_children ;
	Widget		*childP ;
	TabsConstraints tab ; /* #### unused */
	Dimension	cw,ch,bw ;

	/* Our size has now been dictated by the parent.  Lay out the
	 * tabs, lay out the frame, lay out the children.  Remember
	 * that the tabs overlap each other and the frame by shadowWidth.
	 * Also, the top tab is larger than the others, so if there's only
	 * one row, the widget must be made taller to accommodate this.
	 *
	 * Once the tabs are laid out, if there is more than one
	 * row, we may need to shuffle the rows to bring the top tab
	 * to the bottom row.
	 */

	tw->tabs.needs_layout = False ;

	if( num_children > 0 && tw->composite.children != NULL )
	{
	  /* Loop through the tabs and assign rows & x positions */
	  (void) TabLayout(tw, tw->core.width, tw->core.height, NULL, False) ;
	  num_children = TabsNumChildren (tw);

	  /* assign a top widget, bring it to bottom row. */
	  TabsShuffleRows(tw) ;

	  /* now assign child positions & sizes.  Positions are all the
	   * same: just inside the frame.  Sizes are also all the same.
	   */

	  tw->tabs.child_width = cw = tw->core.width - 2 * SHADWID ;
	  tw->tabs.child_height = ch =
	    tw->core.height < (tw->tabs.tab_total + 2 * SHADWID) ? 0 :
	    tw->core.height - tw->tabs.tab_total - 2 * SHADWID ;

	  for(i=0, childP=tw->composite.children;
		i < num_children;
		++i, ++childP)
	    if( XtIsManaged(*childP) )
	    {
	      tab = (TabsConstraints) (*childP)->core.constraints ;
	      bw = (*childP)->core.border_width ;
	      /* Don't do anything if we can't see any of the child. */
	      if (ch >= bw*2 && ch > 0 && cw >= bw*2 && cw > 0)
		XtConfigureWidget(*childP, SHADWID,tw->tabs.tab_total+SHADWID,
				  cw-bw*2,ch-bw*2, bw) ;
	    }
	  if( XtIsRealized(w) ) {
	    XClearWindow(XtDisplay((Widget)tw), XtWindow((Widget)tw)) ;
	    /* should not be necessary to explicitly repaint after a
	     * resize, but XEmacs folks tell me it is.
	     */
	    XtClass(tw)->core_class.expose((Widget)tw,NULL,None) ;
	  }
	}
} /* Resize */



	/* Redraw entire Tabs widget */

/* ARGSUSED */
static	void
TabsExpose(Widget w, XEvent *event, Region region)
{
	TabsWidget	tw = (TabsWidget) w;

	if( tw->tabs.needs_layout )
	  XtClass(w)->core_class.resize(w) ;

	DrawTabs(tw, True) ;
}


	/* Called when any Tabs widget resources are changed. */

/* ARGSUSED */
static	Boolean
TabsSetValues(Widget current, Widget request, Widget new,
	ArgList args, Cardinal *num_args)
{
	TabsWidget curtw = (TabsWidget) current ;
	TabsWidget tw = (TabsWidget) new ;
	Boolean	needRedraw = False ;
	Widget	*childP ;
	int	i ;


	if( tw->tabs.font != curtw->tabs.font  ||
	    tw->tabs.internalWidth != curtw->tabs.internalWidth ||
	    tw->tabs.internalHeight != curtw->tabs.internalHeight )
	{
	  tw->tabs.tab_height = 2 * tw->tabs.internalHeight + SHADWID ;

	  if( tw->tabs.font != NULL )
	    tw->tabs.tab_height += tw->tabs.font->max_bounds.ascent +
				   tw->tabs.font->max_bounds.descent ;

	  /* Tab size has changed.  Resize all tabs and request a new size */
	  for(i=0, childP=tw->composite.children;
		i < tw->composite.num_children;
		++i, ++childP)
	    if( XtIsManaged(*childP) )
	      TabWidth(*childP) ;
	  PreferredSize(tw, &tw->core.width, &tw->core.height, NULL,NULL) ;
	  needRedraw = True ;
	  tw->tabs.needs_layout = True ;
	}

	/* TODO: if any color changes, need to recompute GCs and redraw */

	if( tw->core.background_pixel != curtw->core.background_pixel ||
	    tw->core.background_pixmap != curtw->core.background_pixmap ||
	    tw->tabs.font != curtw->tabs.font )
	  if( XtIsRealized(new) )
	  {
	    TabsFreeGCs(tw) ;
	    TabsAllocGCs(tw) ;
	    needRedraw = True ;
	  }

	if( tw->core.sensitive != curtw->core.sensitive )
	  needRedraw = True ;

	/* Highlit widget changed */
	if ( tw->tabs.hilight != curtw->tabs.hilight )
	{
		needRedraw = True ;
	}

	/* If top widget changes, need to change stacking order, redraw tabs.
	 * Window system will handle the redraws.
	 */

	if( tw->tabs.topWidget != curtw->tabs.topWidget )
	{
	  if( XtIsRealized(tw->tabs.topWidget) )
	  {
	    Widget		w = tw->tabs.topWidget ;
	    TabsConstraints	tab = (TabsConstraints) w->core.constraints ;

	    XRaiseWindow(XtDisplay(w), XtWindow(w)) ;
#ifdef	NEED_MOTIF
	    XtVaSetValues(curtw->tabs.topWidget, XmNtraversalOn, False, 0) ;
	    XtVaSetValues(w, XmNtraversalOn, True, 0) ;
#endif

	    if( tab->tabs.row != tw->tabs.numRows-1 )
	      TabsShuffleRows(tw) ;

		
	    needRedraw = True ;
	  }
	  else
	    tw->tabs.needs_layout = True ;
	}

	return needRedraw ;
}


	/* Called when any child constraint resources change. */

/* ARGSUSED */
static	Boolean
TabsConstraintSetValues(Widget current, Widget request, Widget new,
	ArgList args, Cardinal *num_args)
{
	TabsWidget tw = (TabsWidget) XtParent(new) ;
	TabsConstraints ctab = (TabsConstraints) current->core.constraints ;
	TabsConstraints tab = (TabsConstraints) new->core.constraints ;


	/* if label changes, need to re-layout the entire widget */
	/* if foreground changes, need to redraw tab label */

	/* TODO: only need resize of new bitmap has different dimensions
	 * from old bitmap.
	 */

	if( tab->tabs.label != ctab->tabs.label ||  /* Tab size has changed. */
	    tab->tabs.left_bitmap != ctab->tabs.left_bitmap )
	{
	  TabWidth(new) ;
	  tw->tabs.needs_layout = True ;

	  if( tab->tabs.left_bitmap != ctab->tabs.left_bitmap )
	    getBitmapInfo(tw, tab) ;

	  /* If there are no subclass ConstraintSetValues procedures remaining
	   * to be invoked, and if the preferred size has changed, ask
	   * for a resize.
	   */
	  if( XtClass((Widget)tw) == tabsWidgetClass )
	    MakeSizeRequest(tw) ;
	}


	/* The child widget itself never needs a redisplay, but the parent
	 * Tabs widget might.
	 */

	if( XtIsRealized(new) )
	{
	  if( tw->tabs.needs_layout ) {
	    XClearWindow(XtDisplay((Widget)tw), XtWindow((Widget)tw)) ;
	    XtClass(tw)->core_class.expose((Widget)tw,NULL,None) ;
	  }

	  else if( tab->tabs.foreground != ctab->tabs.foreground )
	    DrawTab(tw, new, True) ;
	}

	return False ;
}


static	Boolean
TabsAcceptFocus(Widget w, Time *t)
{
	if( !w->core.being_destroyed && XtIsRealized(w) &&
	    XtIsSensitive(w) && XtIsManaged(w) && w->core.visible )
	{
	  Widget p ;
	  for(p = XtParent(w); !XtIsShell(p); p = XtParent(p)) ;
	  XtSetKeyboardFocus(p,w) ;
	  return True ;
	}
	else
	  return False ;
}



/*
 * Return preferred size.  Happily accept anything >= our preferred size.
 * (TODO: is that the right thing to do?  Should we always return "almost"
 * if offered more than we need?)
 */

static XtGeometryResult
TabsQueryGeometry(Widget w,
	XtWidgetGeometry *intended, XtWidgetGeometry *preferred)
{
    register TabsWidget tw = (TabsWidget)w ;
    XtGeometryMask mode = intended->request_mode ;

    preferred->request_mode = CWWidth | CWHeight ;
    PreferredSize(tw, &preferred->width, &preferred->height, NULL,NULL) ;

    if( (!(mode & CWWidth) || intended->width == w->core.width)  &&
        (!(mode & CWHeight) || intended->height == w->core.height) )
      return XtGeometryNo ;

    if( (!(mode & CWWidth) || intended->width >= preferred->width)  &&
	(!(mode & CWHeight) || intended->height >= preferred->height) )
      return XtGeometryYes;

    return XtGeometryAlmost;
}



/*
 * Geometry Manager; called when a child wants to be resized.
 */

static XtGeometryResult
TabsGeometryManager(Widget w, XtWidgetGeometry *req, XtWidgetGeometry *reply)
{
	TabsWidget		tw = (TabsWidget) XtParent(w);
	Dimension		s = SHADWID ;
	TabsConstraints		tab = (TabsConstraints)w->core.constraints;
	XtGeometryResult	result ;
	Dimension		rw, rh ;

	/* Position request always denied */

	if( ((req->request_mode & CWX) && req->x != w->core.x) ||
	    ((req->request_mode & CWY) && req->y != w->core.y) ||
	    !tab->tabs.resizable )
	  return XtGeometryNo ;

	/* Make all three fields in the request valid */
	if( !(req->request_mode & CWWidth) )
	    req->width = w->core.width;
	if( !(req->request_mode & CWHeight) )
	    req->height = w->core.height;
	if( !(req->request_mode & CWBorderWidth) )
	    req->border_width = w->core.border_width;

	if( req->width == w->core.width &&
	    req->height == w->core.height &&
	    req->border_width == w->core.border_width )
	  return XtGeometryNo ;

	rw = req->width + 2 * req->border_width ;
	rh = req->height + 2 * req->border_width ;

	/* find out how big the children want to be now */
	MaxChild(tw, w, rw, rh) ;


	/* Size changes must see if the new size can be accommodated.
	 * The Tabs widget keeps all of its children the same
	 * size.  A request to shrink will be accepted only if the
	 * new size is still big enough for all other children.  A
	 * request to shrink that is not big enough for all children
	 * returns an "almost" response with the new proposed size
	 * or a "no" response if unable to shrink at all.
	 *
	 * A request to grow will be accepted only if the Tabs parent can
	 * grow to accommodate.
	 *
	 * TODO:
	 * We could get fancy here and re-arrange the tabs if it is
	 * necessary to compromise with the parent, but we'll save that
	 * for another day.
	 */

	if (req->request_mode & (CWWidth | CWHeight | CWBorderWidth))
	{
	  Dimension	cw,ch ;		/* children's preferred size */
	  Dimension	aw,ah ;		/* available size we can give child */
	  Dimension	th ;		/* space used by tabs */
	  Dimension	wid,hgt ;	/* Tabs widget size */

	  cw = tw->tabs.max_cw ;
	  ch = tw->tabs.max_ch ;

	  /* find out what *my* resulting preferred size would be */

	  PreferredSize2(tw, cw, ch, &wid, &hgt) ;

	  /* Would my size change?  If so, ask to be resized. */

	  if( wid != tw->core.width || hgt != tw->core.height )
	  {
	    Dimension	oldWid = tw->core.width, oldHgt = tw->core.height ;
	    XtWidgetGeometry	myrequest, myreply ;

	    myrequest.width = wid ;
	    myrequest.height = hgt ;
	    myrequest.request_mode = CWWidth | CWHeight ;
	    
	    assert (wid > 0 && hgt > 0);
	    /* If child is only querying, or if we're going to have to
	     * offer the child a compromise, then make this a query only.
	     */

	    if( (req->request_mode & XtCWQueryOnly)  || rw < cw || rh < ch )
	      myrequest.request_mode |= XtCWQueryOnly ;

	    result = XtMakeGeometryRequest((Widget)tw, &myrequest, &myreply) ;

	    /* !$@# Athena Box widget changes the core size even if QueryOnly
	     * is set.  I'm convinced this is a bug.  At any rate, to work
	     * around the bug, we need to restore the core size after every
	     * query geometry request.  This is only partly effective,
	     * as there may be other boxes further up the tree.
	     */
	    if( myrequest.request_mode & XtCWQueryOnly ) {
	      tw->core.width = oldWid ;
	      tw->core.height = oldHgt ;
	    }

	    /* based on the parent's response, determine what the
	     * resulting Tabs widget size would be.
	     */

	    switch( result ) {
	      case XtGeometryYes:
	      case XtGeometryDone:
		tw->tabs.needs_layout = True ;
		break ;

	      case XtGeometryNo:
		wid = tw->core.width ;
		hgt = tw->core.height ;
		break ;

	      case XtGeometryAlmost:
		wid = myreply.width ;
		hgt = myreply.height ;
		tw->tabs.needs_layout = True ;
		break ;
	    }
	  }

	  /* Within the constraints imposed by the parent, what is
	   * the max size we can give the child?
	   */
	  (void) TabLayout(tw, wid, hgt, &th, True) ;
	  aw = wid - 2*s ;
	  ah = hgt - th - 2*s ;

	  /* OK, make our decision.  If requested size is >= max sibling
	   * preferred size, AND requested size <= available size, then
	   * we accept.  Otherwise, we offer a compromise.
	   */

	  if( rw == aw && rh == ah )
	  {
	    /* Acceptable.  If this wasn't a query, change *all* children
	     * to this size.
	     */
	    if( req->request_mode & XtCWQueryOnly )
	      return XtGeometryYes ;
	    else
	    {
	      Widget	*childP = tw->composite.children ;
	      int	i,bw ;
	      w->core.border_width = req->border_width ;
	      for(i=TabsNumChildren (tw); --i >= 0; ++childP)
		if( TabVisible(*childP) )
		{
		  bw = (*childP)->core.border_width ;
		  XtConfigureWidget(*childP, s,tw->tabs.tab_total+s,
			    rw-2*bw, rh-2*bw, bw) ;
		}
#ifdef	COMMENT
	      /* TODO: under what conditions will we need to redraw? */
	      XClearWindow(XtDisplay((Widget)tw), XtWindow((Widget)tw)) ;
	      XtClass(tw)->core_class.expose((Widget)tw,NULL,NULL) ;
#endif	/* COMMENT */
	      return XtGeometryDone ;
	    }
	  }

	  /* Cannot grant child's request.  Describe what we *can* do
	   * and return counter-offer.
	   */
	  reply->width  = aw - 2 * req->border_width ;
	  reply->height = ah - 2 * req->border_width ;
	  reply->border_width = req->border_width ;
	  reply->request_mode = CWWidth | CWHeight | CWBorderWidth ;
	  return XtGeometryAlmost ;
	}

	return XtGeometryYes ;
}




	/* The number of children we manage has changed; recompute
	 * size from scratch.
	 */

static	void
TabsChangeManaged(Widget w)
{
    TabsWidget	tw = (TabsWidget)w ;
    Widget	*childP = tw->composite.children ;
    int		i ;

    if( tw->tabs.topWidget != NULL &&
        ( !XtIsManaged(tw->tabs.topWidget) ||
	  tw->tabs.topWidget->core.being_destroyed ) )
      tw->tabs.topWidget = NULL ;

    /* Check whether the highlight tab is still valid. */
    if( tw->tabs.hilight != NULL &&
        ( !XtIsManaged(tw->tabs.hilight) ||
	  tw->tabs.hilight->core.being_destroyed ) )
      tw->tabs.hilight = NULL ;

    GetPreferredSizes(tw) ;
    MakeSizeRequest(tw) ;

    XtClass(w)->core_class.resize(w) ;
    if( XtIsRealized(w) )
    {
      Display *dpy = XtDisplay(w) ;
      XClearWindow(dpy, XtWindow(w)) ;
      XtClass(w)->core_class.expose(w,NULL,NULL) ;

      /* make sure the top widget stays on top.  This requires
       * making sure that all new children are realized first.
       */
      if( tw->tabs.topWidget != NULL && XtIsRealized(tw->tabs.topWidget) )
      {
	for(i=TabsNumChildren (tw); --i >= 0; ++childP)
	  if( !XtIsRealized(*childP) )
	    XtRealizeWidget(*childP) ;

	XRaiseWindow(dpy, XtWindow(tw->tabs.topWidget)) ;
      }
    }

#ifdef	NEED_MOTIF
    /* Only top widget may receive input */

    for(childP = tw->composite.children, i=tw->composite.num_children;
        --i >= 0;
	++childP)
    {
      XtVaSetValues(*childP, XmNtraversalOn, False, 0) ;
    }

    if( tw->tabs.topWidget != NULL )
      XtVaSetValues(tw->tabs.topWidget, XmNtraversalOn, True, 0) ;
#endif
}




/****************************************************************
 *
 * Action Procedures
 *
 ****************************************************************/


	/* User clicks on a tab, figure out which one it was. */

/* ARGSUSED */
static	void
TabsSelect(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	TabsWidget	tw = (TabsWidget) w ;
	Widget	*childP ;
	Position x,y ;
	Dimension h = tw->tabs.tab_height ;
	int	i ;

#ifdef	NEED_MOTIF
	XmProcessTraversal (w, XmTRAVERSE_CURRENT) ;
#endif

	/* TODO: is there an Xmu function or something to do this instead? */
	switch( event->type ) {
	  case ButtonPress:
	  case ButtonRelease:
	    x = event->xbutton.x ; y = event->xbutton.y ; break ;
	  case KeyPress:
	  case KeyRelease:
	    x = event->xkey.x ; y = event->xkey.y ; break ;
	  default:
	    return ;
	}

	/* TODO: determine which tab was clicked, if any.  Set that
	 * widget to be top of stacking order with XawTabsSetTop().
	 */
	for(i=0, childP=tw->composite.children;
	      i < TabsNumChildren (tw);
	      ++i, ++childP)
	  if( TabVisible(*childP) )
	  {
	    TabsConstraints tab = (TabsConstraints)(*childP)->core.constraints;
	    if( x > tab->tabs.x  &&  x < tab->tabs.x + tab->tabs.width  &&
		y > tab->tabs.y  &&  y < tab->tabs.y + h )
	    {
	      if( *childP != tw->tabs.topWidget &&
		  (XtIsSensitive(*childP) || tw->tabs.selectInsensitive) )
		XawTabsSetTop(*childP, True) ;
	      break ;
	    }
	  }
}


	/* User hits a key */

static	void
TabsPage(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	TabsWidget	tw = (TabsWidget) w ;
	Widget		newtop = NULL;
	Widget		*childP ;
	int		idx ;
	int		nc = TabsNumChildren (tw) ;

	if( nc <= 0 )
	  return ;

	if( *num_params < 1 ) {
	  XtAppWarning(XtWidgetToApplicationContext(w),
	    "Tabs: page() action called with no arguments") ;
	  return ;
	}

	if( tw->tabs.topWidget == NULL )
	  tw->tabs.topWidget = tw->composite.children[0] ;

	for(idx=0, childP=tw->composite.children; idx < nc; ++idx, ++childP )
	  if( tw->tabs.topWidget == *childP )
	    break ;

	switch( params[0][0] ) {
	  case 'u':		/* up */
	  case 'U':
	    if( --idx < 0 )
	      idx = nc-1 ;
	    newtop = tw->composite.children[idx] ;
	    break ;

	  case 'd':		/* down */
	  case 'D':
	    if( ++idx >= nc )
	      idx = 0 ;
	    newtop = tw->composite.children[idx] ;
	    break ;

	  case 'h':
	  case 'H':
	  default:
	      newtop = tw->composite.children[0] ;
	      break ;

	  case 'e':
	  case 'E':
	      newtop = tw->composite.children[nc-1] ;
	      break ;

	  case 's':		/* selected */
	  case 'S':
	      if( (newtop = tw->tabs.hilight) == NULL )
		return ;
	      break ;
	}

	XawTabsSetTop(newtop, True) ;
}


	/* User hits up/down key */

static	void
TabsHighlight(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	TabsWidget	tw = (TabsWidget) w ;
	Widget		newhl = NULL;
	Widget		*childP ;
	int		idx ;
	int		nc = TabsNumChildren (tw) ;

	if( nc <= 0 )
	  return ;

	if( *num_params < 1 )
	{
	  if( tw->tabs.hilight != NULL )
	    DrawHighlight(tw, tw->tabs.hilight, False) ;
	  return ;
	}

	if( tw->tabs.hilight == NULL )
	  newhl = tw->composite.children[0] ;

	else
	{
	  /* find index of currently highlit child */
	  for(idx=0, childP=tw->composite.children; idx < nc; ++idx, ++childP )
	    if( tw->tabs.hilight == *childP )
	      break ;

	  switch( params[0][0] ) {
	    case 'u':		/* up */
	    case 'U':
	      if( --idx < 0 )
		idx = nc-1 ;
	      newhl = tw->composite.children[idx] ;
	      break ;

	    case 'd':		/* down */
	    case 'D':
	      if( ++idx >= nc )
		idx = 0 ;
	      newhl = tw->composite.children[idx] ;
	      break ;

	    case 'h':
	    case 'H':
		newhl = tw->composite.children[0] ;
		break ;

	    case 'e':
	    case 'E':
		newhl = tw->composite.children[nc-1] ;
		break ;

	    default:
		newhl = tw->tabs.hilight ;
		break ;
	  }
	}

	XawTabsSetHighlight(w, newhl) ;
}



static	void
TabsUnhighlight(Widget w, XEvent *event, String *params, Cardinal *num_params)
{
	TabsWidget	tw = (TabsWidget) w ;
	int		nc = tw->composite.num_children ;

	if( nc <= 0 )
	  return ;

	if( tw->tabs.hilight != NULL )
	  DrawHighlight(tw, tw->tabs.hilight, True) ;
}





/****************************************************************
 *
 * Public Procedures
 *
 ****************************************************************/


	/* Set the top tab, optionally call all callbacks. */
void
XawTabsSetTop(Widget w, Bool callCallbacks)
{
	TabsWidget	tw = (TabsWidget)w->core.parent ;
	TabsConstraints tab ;
	Widget		oldtop = tw->tabs.topWidget ;

	if( !XtIsSubclass(w->core.parent, tabsWidgetClass) )
	{
	  char line[256] ;
	  sprintf(line, "XawTabsSetTop: widget \"%.64s\" is not the child of a tabs widget.", XtName(w)) ;
	  XtAppWarning(XtWidgetToApplicationContext(w), line) ;
	  return ;
	}

	if( callCallbacks )
	  XtCallCallbackList(w, tw->tabs.popdownCallbacks,
		(XtPointer)tw->tabs.topWidget) ;

	if( !XtIsRealized(w) ) {
	  tw->tabs.topWidget = w ;
	  tw->tabs.needs_layout = True ;
	  tw->tabs.hilight = NULL; /* The highlight tab might disappear. */
	  return ;
	}

	XRaiseWindow(XtDisplay(w), XtWindow(w)) ;
#ifdef	NEED_MOTIF
	XtVaSetValues(oldtop, XmNtraversalOn, False, 0) ;
	XtVaSetValues(w, XmNtraversalOn, True, 0) ;
#endif

	tab = (TabsConstraints) w->core.constraints ;

	/* Unhighlight before we start messing with the stacking order. */
	if( tw->tabs.hilight != NULL )
	  {
	    DrawHighlight(tw, tw->tabs.hilight, True) ;
	    tw->tabs.hilight = NULL;
	  }

	if( tab->tabs.row == 0 )
	{
	  /* Easy case; undraw current top, undraw new top, assign new
	   * top, redraw all borders.
	   * We *could* just erase and execute a full redraw, but I like to
	   * reduce screen flicker.
	   */
	  UndrawTab(tw, oldtop) ;		/* undraw old */
	  DrawBorder(tw, oldtop, True) ;
	  UndrawTab(tw, w) ;			/* undraw new */
	  DrawBorder(tw, w, True) ;
	  tw->tabs.topWidget = w ;
	  DrawTab(tw, oldtop, True) ;		/* redraw old */
	  DrawTab(tw, w, True) ;		/* redraw new */
	  DrawTabs(tw, False) ;
	}
	else
	{
	  tw->tabs.topWidget = w ;
	  TabsShuffleRows(tw) ;
	  XClearWindow(XtDisplay((Widget)tw), XtWindow((Widget)tw)) ;
	  XtClass(tw)->core_class.expose((Widget)tw,NULL,None) ;
	}

	XawTabsSetHighlight((Widget)tw, w) ;

	if( callCallbacks )
	  XtCallCallbackList(w, tw->tabs.callbacks, (XtPointer)w) ;
}


	/* Set the top tab, optionally call all callbacks. */
void
XawTabsSetHighlight(Widget t, Widget w)
{
	TabsWidget	tw = (TabsWidget)t ;

	if( !XtIsSubclass(t, tabsWidgetClass) )
	  return ;

	if( XtIsRealized(t) && w != tw->tabs.hilight )
	{
	  if( w != NULL )
	    DrawHighlight(tw, w, False) ;
	}

	tw->tabs.hilight = w ;
}




/****************************************************************
 *
 * Private Procedures
 *
 ****************************************************************/


static	void
TabsAllocGCs(TabsWidget tw)
{
	TabsAllocFgGC(tw) ;
	TabsAllocGreyGC(tw) ;
	tw->tabs.backgroundGC = AllocBackgroundGC((Widget)tw, None) ;
	tw->tabs.topGC = AllocTopShadowGC((Widget)tw,
		tw->tabs.top_shadow_contrast, tw->tabs.be_nice_to_cmap) ;
	tw->tabs.botGC = AllocBotShadowGC((Widget)tw,
		tw->tabs.bot_shadow_contrast, tw->tabs.be_nice_to_cmap) ;
}


static	void
TabsFreeGCs(TabsWidget tw)
{
	Widget w = (Widget) tw;

	XtReleaseGC(w, tw->tabs.foregroundGC) ;
	XtReleaseGC(w, tw->tabs.greyGC) ;
	XtReleaseGC(w, tw->tabs.backgroundGC) ;
	XtReleaseGC(w, tw->tabs.topGC) ;
	XtReleaseGC(w, tw->tabs.botGC) ;
#ifdef HAVE_XMU
	XmuReleaseStippledPixmap(XtScreen(w), tw->tabs.grey50) ;
#endif
}





	/* Redraw entire Tabs widget */

static	void
DrawTabs(TabsWidget tw, Bool labels)
{
	Widget		*childP ;
	int		i,j ;
	Dimension	s = SHADWID ;
	Dimension	th = tw->tabs.tab_height ;
	Position	y ;
	TabsConstraints	tab ;

 	if( !XtIsRealized((Widget)tw))
 	  return ;

	/* draw tabs and frames by row except for the top tab, which
	 * is drawn last.  (This is inefficiently written, but should not
	 * be too slow as long as there are not a lot of rows.)
	 */

	y = tw->tabs.numRows == 1 ? TABDELTA : 0 ;
	for(i=0; i<tw->tabs.numRows; ++i, y += th)
	{
 	  for( j=TabsNumChildren (tw), childP=tw->composite.children;
  	      --j >= 0; ++childP )
	    if( TabVisible(*childP) )
	    {
	      tab = (TabsConstraints)(*childP)->core.constraints;
	      if( tab->tabs.row == i && *childP != tw->tabs.topWidget )
		DrawTab(tw, *childP, labels) ;
	    }
	  if( i != tw->tabs.numRows -1 )
	    DrawTrim(tw, 0,y+th, tw->core.width, th+s, False,False) ;
	}

	DrawFrame(tw) ;

	/* and now the top tab */
	if( tw->tabs.topWidget != NULL )
	  DrawTab(tw, tw->tabs.topWidget, labels) ;
}



/* Draw one tab.  Corners are rounded very slightly. */

static	void
DrawTab(TabsWidget tw, Widget child, Bool labels)
{
	GC	gc ;
	int	x,y ;

 	if( !XtIsRealized((Widget)tw))
 	  return ;

	DrawBorder(tw, child, False) ;

	if( labels )
	{
	  TabsConstraints tab = (TabsConstraints)child->core.constraints;
	  Display	*dpy = XtDisplay((Widget)tw) ;
	  Window	win = XtWindow((Widget)tw) ;
	  String	lbl = tab->tabs.label != NULL ?
			      tab->tabs.label : XtName(child) ;

	  if( XtIsSensitive(child) )
	  {
	    gc = tw->tabs.foregroundGC ;
	    XSetForeground(dpy, gc, tab->tabs.foreground) ;
	  }
	  else
	  {
	    /* grey pixel allocation deferred until now */
	    if( !tab->tabs.greyAlloc )
	    {
	      if( tw->tabs.be_nice_to_cmap || tw->core.depth == 1 )
		tab->tabs.grey = tab->tabs.foreground ;
	      else
		tab->tabs.grey = AllocGreyPixel((Widget)tw,
					tab->tabs.foreground,
					tw->core.background_pixel,
					tw->tabs.insensitive_contrast ) ;
	      tab->tabs.greyAlloc = True ;
	    }
	    gc = tw->tabs.greyGC ;
	    XSetForeground(dpy, gc, tab->tabs.grey) ;
	  }

	  x = tab->tabs.x ;
	  y = tab->tabs.y ;
	  if( child == tw->tabs.topWidget )
	    y -= TABLDELTA ;

	  if( tab->tabs.left_bitmap != None && tab->tabs.lbm_width > 0 )
	  {
	    if( tab->tabs.lbm_depth == 1 )
	      XCopyPlane(dpy, tab->tabs.left_bitmap, win,gc,
		0,0, tab->tabs.lbm_width, tab->tabs.lbm_height,
		x+tab->tabs.lbm_x, y+tab->tabs.lbm_y, 1L) ;
	    else
	      XCopyArea(dpy, tab->tabs.left_bitmap, win,gc,
		0,0, tab->tabs.lbm_width, tab->tabs.lbm_height,
		x+tab->tabs.lbm_x, y+tab->tabs.lbm_y) ;
	  }

	  if( lbl != NULL && tw->tabs.font != NULL )
	    XDrawString(dpy,win,gc,
	      x+tab->tabs.l_x, y+tab->tabs.l_y,
	      lbl, (int)strlen(lbl)) ;
	}

	if( child == tw->tabs.hilight )
	  DrawHighlight(tw, child, False) ;
}


	/* draw frame all the way around the child windows. */

static	void
DrawFrame(TabsWidget tw)
{
	GC		topgc = tw->tabs.topGC ;
	GC		botgc = tw->tabs.botGC ;
	Dimension	s = SHADWID ;
	Dimension	ch = tw->tabs.child_height ;
	if (ch > 0)
	  Draw3dBox((Widget)tw, 0,tw->tabs.tab_total,
		    tw->core.width, ch+2*s, s, topgc, botgc) ;
	else
	  {
	    Widget		w = tw->tabs.topWidget ;
	    if (w != NULL)
	      {
		TabsConstraints	tab = (TabsConstraints) w->core.constraints ;
		Draw3dBox((Widget)tw, 0,tw->core.height - 2*s,
			  tab->tabs.x, 2*s, s, topgc, botgc);
		Draw3dBox((Widget)tw, tab->tabs.x + tab->tabs.width, 
			  tw->core.height - 2*s,
			  tw->core.width - tab->tabs.x - tab->tabs.width, 2*s, s, 
			  topgc, botgc);
	      }
	    else
	      Draw3dBox((Widget)tw, 0,tw->core.height - 2*s,
			tw->core.width, 2*s, s, topgc, botgc) ;
	  }
}


	/* draw trim around a tab or underneath a row of tabs */

static	void
DrawTrim(TabsWidget tw,		/* widget */
	int	x,		/* upper-left corner */
	int	y,
	int	wid,		/* total size */
	int	hgt,
	Bool	bottom,		/* draw bottom? */
	Bool	undraw)		/* undraw all */
{
	Display		*dpy = XtDisplay((Widget)tw) ;
	Window		win = XtWindow((Widget)tw) ;
	GC		bggc = tw->tabs.backgroundGC ;
	GC		topgc = undraw ? bggc : tw->tabs.topGC ;
	GC		botgc = undraw ? bggc : tw->tabs.botGC ;
	if( bottom )
	  XDrawLine(dpy,win,bggc, x,y+hgt-1, x+wid-1,y+hgt-1) ;	/* bottom */
	XDrawLine(dpy,win,topgc, x,y+2, x,y+hgt-2) ;		/* left */
	XDrawPoint(dpy,win,topgc, x+1,y+1) ;			/* corner */
	XDrawLine(dpy,win,topgc, x+2,y, x+wid-3,y) ;		/* top */
	XDrawLine(dpy,win,botgc, x+wid-2,y+1, x+wid-2,y+hgt-2) ; /* right */
	XDrawLine(dpy,win,botgc, x+wid-1,y+2, x+wid-1,y+hgt-2) ; /* right */
}


/* Draw one tab border. */

static	void
DrawBorder(TabsWidget tw, Widget child, Bool undraw)
{
	TabsConstraints tab = (TabsConstraints)child->core.constraints;
	Position	x = tab->tabs.x ;
	Position	y = tab->tabs.y ;
	Dimension	twid = tab->tabs.width ;
	Dimension	thgt = tw->tabs.tab_height ;

	/* top tab requires a little special attention; it overlaps
	 * neighboring tabs slightly, so the background must be cleared
	 * in the region of the overlap to partially erase those neighbors.
	 * TODO: is this worth doing with regions instead?
	 */
	if( child == tw->tabs.topWidget )
	{
	  Display	*dpy = XtDisplay((Widget)tw) ;
	  Window	win = XtWindow((Widget)tw) ;
	  GC		bggc = tw->tabs.backgroundGC ;
	  XRectangle	rects[3] ;
	  x -= TABDELTA ;
	  y -= TABDELTA ;
	  twid += TABDELTA*2 ;
	  thgt += TABDELTA ;
	  AddRect(0, x,y+1,twid,TABDELTA) ;
	  AddRect(1, x+1,y,TABDELTA,thgt) ;
	  AddRect(2, x+twid-TABDELTA-1,y,TABDELTA,thgt) ;
	  XFillRectangles(dpy,win,bggc, rects, 3) ;
	}

	DrawTrim(tw, x,y,twid,thgt+1, child == tw->tabs.topWidget, undraw) ;
}


/* Draw highlight around tab that has focus */

static	void
DrawHighlight(TabsWidget tw, Widget child, Bool undraw)
{
	TabsConstraints tab = (TabsConstraints)child->core.constraints;
	Display		*dpy = XtDisplay((Widget)tw) ;
	Window		win = XtWindow((Widget)tw) ;
	GC		gc ;
	Position	x = tab->tabs.x ;
	Position	y = tab->tabs.y ;
	Dimension	wid = tab->tabs.width ;
	Dimension	hgt = tw->tabs.tab_height ;
	XPoint		points[6] ;

	/* top tab does not have a highlight */

	if( child == tw->tabs.topWidget )
	  return ;

	if( undraw )
	  gc = tw->tabs.backgroundGC ;

	else if( XtIsSensitive(child) )
	{
	  gc = tw->tabs.foregroundGC ;
	  XSetForeground(dpy, gc, tab->tabs.foreground) ;
	}
	else
	{
	  gc = tw->tabs.greyGC ;
	  XSetForeground(dpy, gc, tab->tabs.grey) ;
	}

	points[0].x = x+1 ; points[0].y = y+hgt-1 ;
	points[1].x = x+1 ; points[1].y = y+2 ;
	points[2].x = x+2 ; points[2].y = y+1 ;
	points[3].x = x+wid-4 ; points[3].y = y+1 ;
	points[4].x = x+wid-3 ; points[4].y = y+2 ;
	points[5].x = x+wid-3 ; points[5].y = y+hgt-1 ;

	XDrawLines(dpy,win,gc, points,6, CoordModeOrigin) ;
}


/* Undraw one tab interior */

static	void
UndrawTab(TabsWidget tw, Widget child)
{
	TabsConstraints tab = (TabsConstraints)child->core.constraints;
	Position	x = tab->tabs.x ;
	Position	y = tab->tabs.y ;
	Dimension	twid = tab->tabs.width ;
	Dimension	thgt = tw->tabs.tab_height ;
	Display		*dpy = XtDisplay((Widget)tw) ;
	Window		win = XtWindow((Widget)tw) ;
	GC		bggc = tw->tabs.backgroundGC ;

	XFillRectangle(dpy,win,bggc, x,y, twid,thgt) ;
}





	/* GEOMETRY UTILITIES */

	/* Overview:
	 *
	 *  MaxChild(): ask all children (except possibly one) their
	 *  preferred sizes, set max_cw, max_ch accordingly.
	 *
	 *  GetPreferredSizes(): ask all children their preferred sizes,
	 *  set max_cw, max_ch accordingly.
	 *
	 *  PreferredSize(): given max_cw, max_ch, return tabs widget
	 *  preferred size.  Iterate with other widths in order to get
	 *  a reasonable aspect ratio.
	 *
	 *  PreferredSize2(): Given child dimensions, return Tabs
	 *  widget dimensions.
	 *
	 *  PreferredSize3(): Same, except given child dimensions plus
	 *  shadow.
	 */


	/* Compute the width of one child's tab.  Positions will be computed
	 * elsewhere.
	 *
	 *	height: font height + vertical_space*2 + shadowWid*2
	 *	width:	string width + horizontal_space*2 + shadowWid*2
	 *
	 * All tabs are the same height, so that is computed elsewhere.
	 */

static	void
TabWidth(Widget w)
{
	TabsConstraints tab = (TabsConstraints) w->core.constraints ;
	TabsWidget	tw = (TabsWidget)XtParent(w) ;
	String		lbl = tab->tabs.label != NULL ?
				tab->tabs.label : XtName(w) ;
	XFontStruct	*font = tw->tabs.font ;
	int		iw = tw->tabs.internalWidth ;

	tab->tabs.width = iw + SHADWID*2 ;
	tab->tabs.l_x = tab->tabs.lbm_x = SHADWID + iw ;

	if( tab->tabs.left_bitmap != None )
	{
	  tab->tabs.width += tab->tabs.lbm_width + iw ;
	  tab->tabs.l_x += tab->tabs.lbm_width + iw ;
	  tab->tabs.lbm_y = (tw->tabs.tab_height - tab->tabs.lbm_height)/2 ;
	}

	if( lbl != NULL && font != NULL )
	{
	  tab->tabs.width += XTextWidth( font, lbl, (int)strlen(lbl) ) + iw ;
	  tab->tabs.l_y = (tw->tabs.tab_height +
		 tw->tabs.font->max_bounds.ascent -
		 tw->tabs.font->max_bounds.descent)/2 ;
	}
}



	/* Lay out tabs to fit in given width.  Compute x,y position and
	 * row number for each tab.  Return number of rows and total height
	 * required by all tabs.  If there is only one row, add TABDELTA
	 * height to the total.  Rows are assigned bottom to top.
	 *
	 * Tabs are indented from the edges by INDENT.
	 *
	 * TODO: if they require more than two rows and the total height:width
	 * ratio is more than 2:1, then try something else.
	 */

static	int
TabLayout(TabsWidget tw, 
	  Dimension wid, 
	  Dimension hgt, 
	  Dimension *reply_height, Bool query_only)
{
	int		i, row, done = 0, display_rows = 0 ;
	int		num_children = tw->composite.num_children ;
	Widget		*childP ;
	Dimension	w ;
	Position	x,y ;
	TabsConstraints	tab ;

	/* Algorithm: loop through children, assign X positions.  If a tab
	 * would extend beyond the right edge, start a new row.  After all
	 * rows are assigned, make a second pass and assign Y positions.
	 */

	if( num_children > 0 )
	{
	  /* Loop through the tabs and see how much space they need. */

	  row = 0 ;
	  x = INDENT ;
	  y = 0 ;
	  wid -= INDENT ;
	  for(i=num_children, childP=tw->composite.children; --i >= 0; ++childP)
	    if( XtIsManaged(*childP) )
	    {
	      tab = (TabsConstraints) (*childP)->core.constraints ;
	      w = tab->tabs.width ;

	      if( x + w > wid ) {			/* new row */
 		if (y + tw->tabs.tab_height > hgt && !done)
		  {
		    display_rows = row;
		    done = 1;
		  }
		++row;
		x = INDENT ;
		y += tw->tabs.tab_height ;
	      }
	      if( !query_only ) {
		tab->tabs.x = x ;
		tab->tabs.y = y ;
		tab->tabs.row = row ;
	      }
	      x += w + SPACING ;
	      if (!query_only && !done)
		tab->tabs.visible = 1;

	    }
	  /* If there was only one row, increase the height by TABDELTA */
	  if( ++display_rows == 1 )
	  {
	    row++;
	    y = TABDELTA ;
	    if( !query_only )
	      for(i=num_children, childP=tw->composite.children;
		    --i >= 0 ; ++childP)
		if( XtIsManaged(*childP) )
		{
		  tab = (TabsConstraints) (*childP)->core.constraints ;
		  tab->tabs.y = y ;
		}
	  }
	  y += tw->tabs.tab_height ;
	}
	else
	  display_rows = row = y = 0 ;

	if( !query_only ) {
	  tw->tabs.tab_total = y ;
	  tw->tabs.numRows = display_rows ;
	  tw->tabs.realRows = row;
	}

	if( reply_height != NULL )
	  *reply_height = y ;

	return display_rows ;
}



	/* Find max preferred child size.  Returned sizes include child
	 * border widths.
	 */

static	void
GetPreferredSizes(TabsWidget tw)
{
	MaxChild(tw, NULL, 0,0) ;
}



	/* Find max preferred child size.  Returned sizes include child
	 * border widths.  If except is non-null, don't ask that one.
	 */

static	void
MaxChild(TabsWidget tw, Widget except, Dimension cw, Dimension ch)
{
	int			i ;
	Widget			*childP = tw->composite.children ;
	XtWidgetGeometry	preferred ;

	for(i=tw->composite.num_children; --i >=0; ++childP)
	  if( TabVisible (*childP) /*XtIsManaged(*childP)*/  &&  *childP != except )
	  {
	    (void) XtQueryGeometry(*childP, NULL, &preferred) ;
	    cw = Max(cw, preferred.width + preferred.border_width * 2 ) ;
	    ch = Max(ch, preferred.height + preferred.border_width * 2 ) ;
	  }

	tw->tabs.max_cw = cw ;
	tw->tabs.max_ch = ch ;
}



	/* rotate row numbers to bring current widget to bottom row,
	 * compute y positions for all tabs
	 */

static	void
TabsShuffleRows(TabsWidget tw)
{
	TabsConstraints	tab ;
	int		move ;
	int		real_rows, display_rows ;
	Widget		*childP ;
	Dimension	th = tw->tabs.tab_height ;
	Position	bottom ;
	int		i ;

	/* There must be a top widget.  If not, assign one. */
	if( tw->tabs.topWidget == NULL && tw->composite.children != NULL )
	  for(i=TabsNumChildren (tw), childP=tw->composite.children;
	      --i >= 0;
	      ++childP)
	    if( XtIsManaged(*childP) ) {
	      tw->tabs.topWidget = *childP ;
	      break ;
	    }

	if( tw->tabs.topWidget != NULL )
	{
	  display_rows = tw->tabs.numRows ;
	  real_rows = tw->tabs.realRows ;
	  assert( display_rows <= real_rows ) ;

	  if( real_rows > 1 )
	  {
	    tab = (TabsConstraints) tw->tabs.topWidget->core.constraints ;
	    assert( tab != NULL ) ;

	    /* How far to move top row. The selected tab must be on
	       the bottom row of the *visible* rows. */
	    move = (real_rows + 1 - display_rows) - tab->tabs.row ;
	    if (move < 0) 
	      move = real_rows - move;
	    bottom = tw->tabs.tab_total - th ;

	    for(i=tw->composite.num_children, childP=tw->composite.children;
		  --i >= 0;
		  ++childP)
	      if( XtIsManaged(*childP) )
	      {
		tab = (TabsConstraints) (*childP)->core.constraints ;
		tab->tabs.row = (tab->tabs.row + move) % real_rows ;
		tab->tabs.y = bottom - tab->tabs.row * th ;
		tab->tabs.visible = (tab->tabs.row < display_rows);
	      }
	  }
	}
}


	/* Find preferred size.  Ask children, find size of largest,
	 * add room for tabs & return.  This can get a little involved,
	 * as we don't want to have too many rows of tabs; we may widen
	 * the widget to reduce # of rows.
	 *
	 * This function requires that max_cw, max_ch already be set.
	 */
static	int
PreferredSize(
	TabsWidget	tw,
	Dimension	*reply_width,		/* total widget size */
	Dimension	*reply_height,
	Dimension	*reply_cw,		/* child widget size */
	Dimension	*reply_ch)
{
	Dimension	cw,ch ;		/* child width, height */
	Dimension	wid,hgt ;
	Dimension	rwid,rhgt ;
	int		nrow ;

	wid = cw = tw->tabs.max_cw ;
	hgt = ch = tw->tabs.max_ch ;

	nrow = PreferredSize2(tw, wid,hgt, &rwid, &rhgt) ;

	/* Check for absurd results (more than 2 rows, high aspect
	 * ratio).  Try wider size if needed.
	 * TODO: make sure this terminates.
	 */

	if( nrow > 2 && rhgt > rwid )
	{
	  Dimension w0, w1 ;
	  int maxloop = 20 ;

	  /* step 1: start doubling size until it's too big */
	  do {
	    w0 = wid ;
	    wid = Max(wid*2, wid+20) ;
	    nrow = PreferredSize2(tw, wid,hgt, &rwid,&rhgt) ;
	  } while( nrow > 2 && rhgt > rwid ) ;
	  w1 = wid ;

	  /* step 2: use Newton's method to find ideal size.  Stop within
	   * 8 pixels.
	   */
	  while( --maxloop > 0 && w1 > w0 + 8 )
	  {
	    wid = (w0+w1)/2 ;
	    nrow = PreferredSize2(tw, wid,hgt, &rwid,&rhgt) ;
	    if( nrow > 2 && rhgt > rwid )
	      w0 = wid ;
	    else
	      w1 = wid ;
	  }
	  wid = w1 ;
	}

	*reply_width = rwid ;
	*reply_height = rhgt ;
	if( reply_cw != NULL ) *reply_cw = cw ;
	if( reply_ch != NULL ) *reply_ch = ch ;
	return nrow ;
}


	/* Find preferred size, given size of children. */

static	int
PreferredSize2(
	TabsWidget	tw,
	Dimension	cw,		/* child width, height */
	Dimension	ch,
	Dimension	*reply_width,	/* total widget size */
	Dimension	*reply_height)
{
	Dimension	s = SHADWID ;
	int ret;

	/* make room for shadow frame */
	cw += s*2 ;
	ch += s*2 ;

	ret = PreferredSize3(tw, cw, ch, reply_width, reply_height) ;

	assert (*reply_width > 0 && *reply_height > 0);
	return ret;
}


	/* Find preferred size, given size of children+shadow. */

static	int
PreferredSize3(
	TabsWidget	tw,
	Dimension	wid,		/* child width, height */
	Dimension	hgt,
	Dimension	*reply_width,	/* total widget size */
	Dimension	*reply_height)
{
	Dimension	th ;		/* space used by tabs */
	int		nrows ;

	if( tw->composite.num_children > 0 )
	  nrows = TabLayout(tw, wid, hgt, &th, True) ;
	else {
	  th = 0 ;
	  nrows = 0 ;
	}

	*reply_width = Max(wid, MIN_WID) ;
	*reply_height = Max(th+hgt, MIN_HGT) ;

	return nrows ;
}


static	void
MakeSizeRequest(TabsWidget tw)
{
	Widget			w = (Widget)tw ;
	XtWidgetGeometry	request, reply ;
	XtGeometryResult	result ;
	Dimension		cw,ch ;

	request.request_mode = CWWidth | CWHeight ;
	PreferredSize(tw, &request.width, &request.height, &cw, &ch) ;

	if( request.width == tw->core.width &&
	    request.height == tw->core.height )
	  return ;

	result = XtMakeGeometryRequest(w, &request, &reply) ;

	if( result == XtGeometryAlmost )
	{
	  /* Bugger.  Didn't get what we want, but were offered a
	   * compromise.  If the width was too small, recompute
	   * based on the too-small width and try again.
	   * If the height was too small, make a wild-ass guess
	   * at a wider width and try again.
	   */

	  if( reply.width < request.width && reply.height >= request.height )
	  {
	    Dimension s = SHADWID ;
	    ch += s*2 ;
	    PreferredSize3(tw, reply.width,ch, &request.width, &request.height);
	    result = XtMakeGeometryRequest(w, &request, &reply) ;
	    if( result == XtGeometryAlmost )
	      (void) XtMakeGeometryRequest(w, &reply, NULL) ;
	  }

	  else
	    (void) XtMakeGeometryRequest(w, &reply, NULL) ;
	}
}


static	void
getBitmapInfo(TabsWidget tw, TabsConstraints tab)
{
	Window root ;
	int	x,y ;
	unsigned int bw ;

	if( tab->tabs.left_bitmap == None  ||
	    !XGetGeometry(XtDisplay(tw), tab->tabs.left_bitmap, &root, &x, &y,
		&tab->tabs.lbm_width, &tab->tabs.lbm_height,
		&bw, &tab->tabs.lbm_depth) )
	  tab->tabs.lbm_width = tab->tabs.lbm_height = 0 ;
}




	/* Code copied & modified from Gcs.c.  This version has dynamic
	 * foreground.
	 */

static	void
TabsAllocFgGC(TabsWidget tw)
{
	Widget		w = (Widget) tw;
	XGCValues	values ;

	values.background = tw->core.background_pixel ;
	values.font = tw->tabs.font->fid ;
	values.line_style = LineOnOffDash ;
	values.line_style = LineSolid ;

	tw->tabs.foregroundGC =
	  XtAllocateGC(w, w->core.depth,
	    GCBackground|GCFont|GCLineStyle, &values,
	    GCForeground,
	    GCSubwindowMode|GCGraphicsExposures|GCDashOffset|
		GCDashList|GCArcMode) ;
}

static	void
TabsAllocGreyGC(TabsWidget tw)
{
	Widget		w = (Widget) tw;
	XGCValues	values ;

	values.background = tw->core.background_pixel ;
	values.font = tw->tabs.font->fid ;
#ifdef HAVE_XMU
	if( tw->tabs.be_nice_to_cmap || w->core.depth == 1)
	{
	  values.fill_style = FillStippled ;
	  tw->tabs.grey50 =
	  values.stipple = XmuCreateStippledPixmap(XtScreen(w), 1L, 0L, 1) ;

	  tw->tabs.greyGC =
	    XtAllocateGC(w, w->core.depth,
	      GCBackground|GCFont|GCStipple|GCFillStyle, &values,
	      GCForeground,
	      GCSubwindowMode|GCGraphicsExposures|GCDashOffset|
		  GCDashList|GCArcMode) ;
	}
	else
#endif
	{
	  tw->tabs.greyGC =
	    XtAllocateGC(w, w->core.depth,
	      GCFont, &values,
	      GCForeground,
	      GCBackground|GCSubwindowMode|GCGraphicsExposures|GCDashOffset|
		  GCDashList|GCArcMode) ;
	}
}
