/* ui-gtk.h
**
** Description: 
**
** Created by: William M. Perry
** Copyright (c) 2000 Aventail Corporation
**
*/

#ifndef __UI_GTK_H__
#define __UI_GTK_H__

/* Encapsulate a foreign function call */
#include <gtk/gtk.h>
#include "sysdll.h"
#include "lrecord.h"

typedef void (*ffi_actual_function) (void);
typedef void (*ffi_marshalling_function) (ffi_actual_function, GtkArg *);

#define MAX_GTK_ARGS 100

typedef struct {
  struct lcrecord_header header;
  GtkType return_type;
  GtkType args[MAX_GTK_ARGS];
  gint n_args;
  Lisp_Object function_name;
  dll_func function_ptr;
  ffi_marshalling_function marshal;
} emacs_ffi_data;

DECLARE_LRECORD (emacs_ffi, emacs_ffi_data);

#define XFFI(x) XRECORD (x, emacs_ffi, emacs_ffi_data)
#define XSETFFI(x,p) XSETRECORD (x, p, emacs_ffi)
#define FFIP(x) RECORDP (x, emacs_ffi)
#define CHECK_FFI(x) CHECK_RECORD (x, emacs_ffi)

/* Encapsulate a GtkObject in Lisp */
typedef struct {
  struct lcrecord_header header;
  gboolean alive_p;
  GtkObject *object;
  Lisp_Object plist;
} emacs_gtk_object_data;

DECLARE_LRECORD (emacs_gtk_object, emacs_gtk_object_data);

#define XGTK_OBJECT(x) XRECORD (x, emacs_gtk_object, emacs_gtk_object_data)
#define XSETGTK_OBJECT(x,p) XSETRECORD (x, p, emacs_gtk_object)
#define GTK_OBJECTP(x) RECORDP (x, emacs_gtk_object)
#define CHECK_GTK_OBJECT(x) CHECK_RECORD (x, emacs_gtk_object)

extern Lisp_Object build_gtk_object (GtkObject *obj);

/* Encapsulate a GTK_TYPE_BOXED in lisp */
typedef struct {
  struct lcrecord_header header;
  GtkType object_type;
  void *object;
} emacs_gtk_boxed_data;

DECLARE_LRECORD (emacs_gtk_boxed, emacs_gtk_boxed_data);

#define XGTK_BOXED(x) XRECORD (x, emacs_gtk_boxed, emacs_gtk_boxed_data)
#define XSETGTK_BOXED(x,p) XSETRECORD (x, p, emacs_gtk_boxed)
#define GTK_BOXEDP(x) RECORDP (x, emacs_gtk_boxed)
#define CHECK_GTK_BOXED(x) CHECK_RECORD (x, emacs_gtk_boxed)

#endif /* __UI_GTK_H__ */
