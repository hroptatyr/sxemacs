/* gtk-xemacs.h
**
** Description: A widget to encapsulate a XEmacs 'text widget'
**
** Created by: William M. Perry
** Copyright (c) 2000 William M. Perry <wmperry@gnu.org>
**
*/

#ifndef __GTK_XEMACS_H__
#define __GTK_XEMACS_H__

#include <config.h>
#include "frame.h"
#include <gdk/gdk.h>
#include <gtk/gtkfixed.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#define GTK_XEMACS(obj)			GTK_CHECK_CAST (obj, gtk_xemacs_get_type (), GtkXEmacs)
#define GTK_XEMACS_CLASS(klass)	GTK_CHECK_CLASS_CAST (klass, gtk_xemacs_get_type (), GtkXEmacsClass)
#define GTK_IS_XEMACS(obj)		GTK_CHECK_TYPE (obj, gtk_xemacs_get_type ())
#define GTK_XEMACS_FRAME(obj)	GTK_XEMACS (obj)->f

	typedef struct _GtkXEmacs GtkXEmacs;
	typedef struct _GtkXEmacsClass GtkXEmacsClass;

	struct _GtkXEmacs
	{
		GtkFixed fixed;
		struct frame *f;
	};

	struct _GtkXEmacsClass
	{
		GtkFixedClass parent_class;
	};

	guint gtk_xemacs_get_type (void);
	GtkWidget *gtk_xemacs_new (struct frame *f);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GTK_XEMACS_H__ */
