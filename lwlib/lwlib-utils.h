#ifndef INCLUDED_lwlib_utils_h_
#define INCLUDED_lwlib_utils_h_

void destroy_all_children(Widget widget);
void XtNoClearRefreshWidget(Widget);

typedef void (*XtApplyToWidgetsProc) (Widget, XtPointer);
typedef void *(*XtApplyUntilToWidgetsProc) (Widget, XtPointer);

void XtApplyToWidgets(Widget, XtApplyToWidgetsProc, XtPointer);
void *XtApplyUntilToWidgets(Widget, XtApplyUntilToWidgetsProc, XtPointer);

Widget *XtCompositeChildren(Widget, unsigned int *);

/* returns True is the widget is being destroyed, False otherwise */
Boolean XtWidgetBeingDestroyedP(Widget widget);

void XtSafelyDestroyWidget(Widget);

#ifdef USE_DEBUG_MALLOC
#include <dmalloc.h>
#endif

#endif				/* INCLUDED_lwlib_utils_h_ */
