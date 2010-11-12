/* ui-gtk.c
**
** Description: Creating 'real' UIs from lisp.
**
** Created by: William M. Perry <wmperry@gnu.org>
** Copyright (c) 2000 William M. Perry <wmperry@gnu.org>
**
*/

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "console-gtk.h"
#include "device.h"
#include "window.h"
#include "glyphs-gtk.h"
#include "objects-gtk.h"
#include "ui-gtk.h"
#include "faces.h"
#include "gui-gtk.h"
#include "sysdll.h"
#include "hash.h"
#include "events.h"
#include "elhash.h"

/* XEmacs specific GTK types */
#include "gtk-glue.c"

Lisp_Object Qemacs_ffip;
Lisp_Object Qemacs_gtk_objectp;
Lisp_Object Qemacs_gtk_boxedp;
Lisp_Object Qvoid;
Lisp_Object Venumeration_info;

static GHashTable *dll_cache;

Lisp_Object gtk_type_to_lisp(GtkArg * arg);
int lisp_to_gtk_type(Lisp_Object obj, GtkArg * arg);
int lisp_to_gtk_ret_type(Lisp_Object obj, GtkArg * arg);
void describe_gtk_arg(GtkArg * arg);
guint symbol_to_enum(Lisp_Object obj, GtkType t);
static guint lisp_to_flag(Lisp_Object obj, GtkType t);
static Lisp_Object flags_to_list(guint value, GtkType t);
static Lisp_Object enum_to_symbol(guint value, GtkType t);

#define NIL_OR_VOID_P(x) (NILP (x) || EQ (x, Qvoid))

static void initialize_dll_cache(void)
{
	if (!dll_cache) {
		dll_cache = g_hash_table_new(g_str_hash, g_str_equal);

		g_hash_table_insert(dll_cache, "---XEmacs Internal Handle---",
				    dll_open(NULL));
	}
}

DEFUN("dll-load", Fdll_load, 1, 1, 0,	/*
<<<<<<< HEAD
					   Load a shared library DLL into XEmacs.  No initialization routines are required.
					   This is for loading dependency DLLs into XEmacs.
					 */
=======
Load a shared library DLL into XEmacs.  No initialization routines are required.
This is for loading dependency DLLs into XEmacs.
*/
>>>>>>> origin/master
      (dll))
{
	dll_handle h;

	CHECK_STRING(dll);

	initialize_dll_cache();

	/* If the dll name has a directory component in it, then we should
	   expand it. */
	if (!NILP(Fstring_match(build_string("/"), dll, Qnil, Qnil)))
		dll = Fexpand_file_name(dll, Qnil);

	/* Check if we have already opened it first */
	h = g_hash_table_lookup(dll_cache, XSTRING_DATA(dll));

	if (!h) {
		h = dll_open((char *)XSTRING_DATA(dll));

		if (h) {
			g_hash_table_insert(dll_cache,
					    g_strdup(XSTRING_DATA(dll)), h);
		} else {
			signal_simple_error("dll_open error",
					    build_string(dll_error(NULL)));
		}
	}
	return (h ? Qt : Qnil);
}

/* Gtk object importing */
EXFUN(Fgtk_import_type, 1);

static struct hash_table *internal_type_hash;

static int type_hash_equal(const void *arg1, const void *arg2)
{
	return ((GtkType) arg1 == (GtkType) arg2);
}

static unsigned long type_hash_hash(const void *arg)
{
	return ((unsigned long)arg);
}

static int type_already_imported_p(GtkType t)
{
	void *retval = NULL;

	/* These are cases that we don't need to import */
	switch (GTK_FUNDAMENTAL_TYPE(t)) {
	case GTK_TYPE_CHAR:
	case GTK_TYPE_UCHAR:
	case GTK_TYPE_BOOL:
	case GTK_TYPE_INT:
	case GTK_TYPE_UINT:
	case GTK_TYPE_LONG:
	case GTK_TYPE_ULONG:
	case GTK_TYPE_FLOAT:
	case GTK_TYPE_DOUBLE:
	case GTK_TYPE_STRING:
	case GTK_TYPE_BOXED:
	case GTK_TYPE_POINTER:
	case GTK_TYPE_SIGNAL:
	case GTK_TYPE_ARGS:
	case GTK_TYPE_CALLBACK:
	case GTK_TYPE_C_CALLBACK:
	case GTK_TYPE_FOREIGN:
		return (1);
	}

	if (!internal_type_hash) {
		internal_type_hash =
		    make_general_hash_table(163, type_hash_hash,
					    type_hash_equal);
		return (0);
	}

	if (gethash((void *)t, internal_type_hash, (const void **)&retval)) {
		return (1);
	}
	return (0);
}

static void mark_type_as_imported(GtkType t)
{
	if (type_already_imported_p(t))
		return;

	puthash((void *)t, (void *)1, internal_type_hash);
}

static void import_gtk_type(GtkType t);

static void import_gtk_object_internal(GtkType the_type)
{
	GtkType original_type = the_type;
	int first_time = 1;

	do {
		GtkArg *args;
		guint32 *flags;
		guint n_args;
		guint i;
#if 0
		GtkObjectClass *klass;
		GtkSignalQuery *query;
		guint32 *signals;
		guint n_signals;
#endif

		/* Register the type before we do anything else with it... */
		if (!first_time) {
			if (!type_already_imported_p(the_type)) {
				import_gtk_type(the_type);
			}
		} else {
			/* We need to mark the object type as imported here or we
			   run the risk of SERIOUS recursion when we do automatic
			   argument type importing.  mark_type_as_imported() is
			   smart enough to be a noop if we attempt to register
			   things twice.  */
			first_time = 0;
			mark_type_as_imported(the_type);
		}

		args = gtk_object_query_args(the_type, &flags, &n_args);

		/* First get the arguments the object can accept */
		for (i = 0; i < n_args; i++) {
			if ((args[i].type != original_type)
			    && !type_already_imported_p(args[i].type)) {
				import_gtk_type(args[i].type);
			}
		}

		g_free(args);
		g_free(flags);

#if 0
		/* Now lets publish the signals */
		klass = (GtkObjectClass *) gtk_type_class(the_type);
		signals = klass->signals;
		n_signals = klass->nsignals;

		for (i = 0; i < n_signals; i++) {
			query = gtk_signal_query(signals[i]);
			/* What do we want to do here? */
			g_free(query);
		}
#endif

		the_type = gtk_type_parent(the_type);
	} while (the_type != GTK_TYPE_INVALID);
}

static void import_gtk_enumeration_internal(GtkType the_type)
{
	GtkEnumValue *vals = gtk_type_enum_get_values(the_type);
	Lisp_Object assoc = Qnil;

	if (NILP(Venumeration_info)) {
		Venumeration_info =
		    call2(intern("make-hashtable"), make_int(100), Qequal);
	}

	while (vals && vals->value_name) {
		assoc =
		    Fcons(Fcons
			  (intern(vals->value_nick), make_int(vals->value)),
			  assoc);
		assoc =
		    Fcons(Fcons
			  (intern(vals->value_name), make_int(vals->value)),
			  assoc);
		vals++;
	}

	assoc = Fnreverse(assoc);

	Fputhash(make_int(the_type), assoc, Venumeration_info);
}

static void import_gtk_type(GtkType t)
{
	if (type_already_imported_p(t)) {
		return;
	}

	switch (GTK_FUNDAMENTAL_TYPE(t)) {
	case GTK_TYPE_ENUM:
	case GTK_TYPE_FLAGS:
		import_gtk_enumeration_internal(t);
		break;
	case GTK_TYPE_OBJECT:
		import_gtk_object_internal(t);
		break;
	default:
		break;
	}

	mark_type_as_imported(t);
}

/* Foreign function calls */
static emacs_ffi_data *allocate_ffi_data(void)
{
	emacs_ffi_data *data =
	    alloc_lcrecord_type(emacs_ffi_data, &lrecord_emacs_ffi);

	data->return_type = GTK_TYPE_NONE;
	data->n_args = 0;
	data->function_name = Qnil;
	data->function_ptr = 0;
	data->marshal = 0;

	return (data);
}

static Lisp_Object mark_ffi_data(Lisp_Object obj)
{
	emacs_ffi_data *data = (emacs_ffi_data *) XFFI(obj);

	mark_object(data->function_name);
	return (Qnil);
}

static void
ffi_object_printer(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	char buf[200];

	if (print_readably)
		error("printing unreadable object #<ffi %p",
		      XFFI(obj)->function_ptr);

	write_c_string("#<ffi ", printcharfun);
	print_internal(XFFI(obj)->function_name, printcharfun, 1);
	if (XFFI(obj)->n_args) {
		sprintf(buf, " %d arguments", XFFI(obj)->n_args);
		write_c_string(buf, printcharfun);
	}
	sprintf(buf, " %p>", (void *)XFFI(obj)->function_ptr);
	write_c_string(buf, printcharfun);
}

DEFINE_LRECORD_IMPLEMENTATION("ffi", emacs_ffi,
			      mark_ffi_data, ffi_object_printer,
			      0, 0, 0, NULL, emacs_ffi_data);

typedef GtkObject *(*__OBJECT_fn) ();
typedef gint(*__INT_fn) ();
typedef void (*__NONE_fn) ();
typedef gchar *(*__STRING_fn) ();
typedef gboolean(*__BOOL_fn) ();
typedef gfloat(*__FLOAT_fn) ();
typedef void *(*__POINTER_fn) ();
typedef GList *(*__LIST_fn) ();

/* An auto-generated file of marshalling functions. */
#include "emacs-marshals.c"

#define CONVERT_SINGLE_TYPE(var,nam,tp) case GTK_TYPE_##nam: GTK_VALUE_##nam (var) = * (tp *) v; break;
#define CONVERT_RETVAL(a,freep) 			\
  do {							\
    void *v = GTK_VALUE_POINTER(a);			\
    switch (GTK_FUNDAMENTAL_TYPE (a.type))		\
    {							\
	CONVERT_SINGLE_TYPE(a,CHAR,gchar);		\
	CONVERT_SINGLE_TYPE(a,UCHAR,guchar);		\
	CONVERT_SINGLE_TYPE(a,BOOL,gboolean);		\
	CONVERT_SINGLE_TYPE(a,INT,gint);		\
	CONVERT_SINGLE_TYPE(a,UINT,guint);		\
	CONVERT_SINGLE_TYPE(a,LONG,glong);		\
	CONVERT_SINGLE_TYPE(a,ULONG,gulong);		\
	CONVERT_SINGLE_TYPE(a,FLOAT,gfloat);		\
	CONVERT_SINGLE_TYPE(a,DOUBLE,gdouble);		\
	CONVERT_SINGLE_TYPE(a,STRING,gchar *);		\
	CONVERT_SINGLE_TYPE(a,ENUM,gint);		\
	CONVERT_SINGLE_TYPE(a,FLAGS,guint);		\
	CONVERT_SINGLE_TYPE(a,BOXED,void *);		\
	CONVERT_SINGLE_TYPE(a,POINTER,void *);		\
	CONVERT_SINGLE_TYPE(a,OBJECT,GtkObject *);	\
	default:					\
	GTK_VALUE_POINTER (a) = * (void **) v;	\
	break;						\
    }							\
    if (freep) xfree(v);				\
  } while (0)

gpointer __allocate_object_storage(GtkType t)
{
	size_t s = 0;
	void *rval = NULL;

	switch (GTK_FUNDAMENTAL_TYPE(t)) {
		/* flag types */
	case GTK_TYPE_CHAR:
		s = (sizeof(gchar));
		break;
	case GTK_TYPE_UCHAR:
		s = (sizeof(guchar));
		break;
	case GTK_TYPE_BOOL:
		s = (sizeof(gboolean));
		break;
	case GTK_TYPE_INT:
		s = (sizeof(gint));
		break;
	case GTK_TYPE_UINT:
		s = (sizeof(guint));
		break;
	case GTK_TYPE_LONG:
		s = (sizeof(glong));
		break;
	case GTK_TYPE_ULONG:
		s = (sizeof(gulong));
		break;
	case GTK_TYPE_FLOAT:
		s = (sizeof(gfloat));
		break;
	case GTK_TYPE_DOUBLE:
		s = (sizeof(gdouble));
		break;
	case GTK_TYPE_STRING:
		s = (sizeof(gchar *));
		break;
	case GTK_TYPE_ENUM:
	case GTK_TYPE_FLAGS:
		s = (sizeof(guint));
		break;
	case GTK_TYPE_BOXED:
	case GTK_TYPE_POINTER:
		s = (sizeof(void *));
		break;

		/* base type of the object system */
	case GTK_TYPE_OBJECT:
		s = (sizeof(GtkObject *));
		break;

	default:
		if (GTK_FUNDAMENTAL_TYPE(t) == GTK_TYPE_LISTOF) {
			s = (sizeof(void *));
		}
		rval = NULL;
		break;
	}

	if (s) {
		rval = xmalloc(s);
		memset(rval, '\0', s);
	}

	return (rval);
}

Lisp_Object type_to_marshaller_type(GtkType t)
{
	switch (GTK_FUNDAMENTAL_TYPE(t)) {
	case GTK_TYPE_NONE:
		return (build_string("NONE"));
		/* flag types */
	case GTK_TYPE_CHAR:
	case GTK_TYPE_UCHAR:
		return (build_string("CHAR"));
	case GTK_TYPE_BOOL:
		return (build_string("BOOL"));
	case GTK_TYPE_ENUM:
	case GTK_TYPE_FLAGS:
	case GTK_TYPE_INT:
	case GTK_TYPE_UINT:
		return (build_string("INT"));
	case GTK_TYPE_LONG:
	case GTK_TYPE_ULONG:
		return (build_string("LONG"));
	case GTK_TYPE_FLOAT:
	case GTK_TYPE_DOUBLE:
		return (build_string("FLOAT"));
	case GTK_TYPE_STRING:
		return (build_string("STRING"));
	case GTK_TYPE_BOXED:
	case GTK_TYPE_POINTER:
		return (build_string("POINTER"));
	case GTK_TYPE_OBJECT:
		return (build_string("OBJECT"));
	case GTK_TYPE_CALLBACK:
		return (build_string("CALLBACK"));
	default:
		/* I can't put this in the main switch statement because it is a
		   new fundamental type that is not fixed at compile time.
		   *sigh*
		 */
		if (GTK_FUNDAMENTAL_TYPE(t) == GTK_TYPE_ARRAY)
			return (build_string("ARRAY"));

		if (GTK_FUNDAMENTAL_TYPE(t) == GTK_TYPE_LISTOF)
			return (build_string("LIST"));
		return (Qnil);
	}
}

struct __dll_mapper_closure {
	void *(*func) (dll_handle, const char *);
	const char *obj_name;
	void **storage;
};

static void __dll_mapper(gpointer key, gpointer value, gpointer user_data)
{
	struct __dll_mapper_closure *closure =
	    (struct __dll_mapper_closure *)user_data;

	if (*(closure->storage) == NULL) {
		/* Need to see if it is in this one */
		*(closure->storage) =
		    closure->func((dll_handle) value, closure->obj_name);
	}
}

DEFUN("gtk-import-variable-internal", Fgtk_import_variable_internal, 2, 2, 0,	/*
<<<<<<< HEAD
										   Import a variable into the XEmacs namespace.
										 */
=======
Import a variable into the XEmacs namespace.
*/
>>>>>>> origin/master
      (type, name))
{
	void *var = NULL;
	GtkArg arg;

	if (SYMBOLP(type))
		type = Fsymbol_name(type);

	CHECK_STRING(type);
	CHECK_STRING(name);

	initialize_dll_cache();
	xemacs_init_gtk_classes();

	arg.type = gtk_type_from_name((char *)XSTRING_DATA(type));

	if (arg.type == GTK_TYPE_INVALID) {
		signal_simple_error("Unknown type", type);
	}

	/* Need to look thru the already-loaded dlls */
	{
		struct __dll_mapper_closure closure;

		closure.func = dll_variable;
		closure.obj_name = XSTRING_DATA(name);
		closure.storage = &var;

		g_hash_table_foreach(dll_cache, __dll_mapper, &closure);
	}

	if (!var) {
		signal_simple_error("Could not locate variable", name);
	}

	GTK_VALUE_POINTER(arg) = var;
	CONVERT_RETVAL(arg, 0);
	return (gtk_type_to_lisp(&arg));
}

DEFUN("gtk-import-function-internal", Fgtk_import_function_internal, 2, 3, 0,	/*
<<<<<<< HEAD
										   Import a function into the XEmacs namespace.
										 */
=======
Import a function into the XEmacs namespace.
*/
>>>>>>> origin/master
      (rettype, name, args))
{
	Lisp_Object rval = Qnil;
	Lisp_Object marshaller = Qnil;
	emacs_ffi_data *data = NULL;
	gint n_args = 0;
#if 0
	dll_handle h = NULL;
#endif
	ffi_marshalling_function marshaller_func = NULL;
	ffi_actual_function name_func = NULL;

	CHECK_SYMBOL(rettype);
	CHECK_STRING(name);
	CHECK_LIST(args);

	initialize_dll_cache();
	xemacs_init_gtk_classes();

	/* Need to look thru the already-loaded dlls */
	{
		struct __dll_mapper_closure closure;

		closure.func = dll_function;
		closure.obj_name = XSTRING_DATA(name);
		closure.storage = (void **)&name_func;

		g_hash_table_foreach(dll_cache, __dll_mapper, &closure);
	}

	if (!name_func) {
		signal_simple_error("Could not locate function", name);
	}

	data = allocate_ffi_data();

	if (NILP(rettype)) {
		rettype = Qvoid;
	}

	if (!NILP(args)) {
		Lisp_Object tail = Qnil;
		Lisp_Object value = args;
		Lisp_Object type = Qnil;

		EXTERNAL_LIST_LOOP(tail, value) {
			GtkType the_type;
			Lisp_Object marshaller_type = Qnil;

			CHECK_SYMBOL(XCAR(tail));

			type = Fsymbol_name(XCAR(tail));

			the_type =
			    gtk_type_from_name((char *)XSTRING_DATA(type));

			if (the_type == GTK_TYPE_INVALID) {
				signal_simple_error("Unknown argument type",
						    type);
			}

			/* All things must be reduced to their basest form... */
			import_gtk_type(the_type);
			data->args[n_args] = the_type;	/* GTK_FUNDAMENTAL_TYPE (the_type); */

			/* Now lets build up another chunk of our marshaller function name */
			marshaller_type =
			    type_to_marshaller_type(data->args[n_args]);

			if (NILP(marshaller_type)) {
				signal_simple_error
				    ("Do not know how to marshal", type);
			}
			marshaller =
			    concat3(marshaller, build_string("_"),
				    marshaller_type);
			n_args++;
		}
	} else {
		marshaller =
		    concat3(marshaller, build_string("_"),
			    type_to_marshaller_type(GTK_TYPE_NONE));
	}

	rettype = Fsymbol_name(rettype);
	data->return_type = gtk_type_from_name((char *)XSTRING_DATA(rettype));

	if (data->return_type == GTK_TYPE_INVALID) {
		signal_simple_error("Unknown return type", rettype);
	}

	import_gtk_type(data->return_type);

	marshaller =
	    concat3(type_to_marshaller_type(data->return_type),
		    build_string("_"), marshaller);
	marshaller = concat2(build_string("emacs_gtk_marshal_"), marshaller);

	marshaller_func =
	    (ffi_marshalling_function) find_marshaller((char *)
						       XSTRING_DATA
						       (marshaller));

	if (!marshaller_func) {
		signal_simple_error("Could not locate marshaller function",
				    marshaller);
	}

	data->n_args = n_args;
	data->function_name = name;
	data->function_ptr = name_func;
	data->marshal = marshaller_func;

	XSETFFI(rval, data);
	return (rval);
}

DEFUN("gtk-call-function", Fgtk_call_function, 1, 2, 0,	/*
<<<<<<< HEAD
							   Call an external function.
							 */
=======
Call an external function.
*/
>>>>>>> origin/master
      (func, args))
{
	GtkArg the_args[MAX_GTK_ARGS];
	gint n_args = 0;
	Lisp_Object retval = Qnil;

	CHECK_FFI(func);
	CHECK_LIST(args);

	n_args = XINT(Flength(args));

#ifdef XEMACS_IS_SMARTER_THAN_THE_PROGRAMMER
	/* #### I think this is too dangerous to enable by default.
	 ** #### Genuine program bugs would probably be allowed to
	 ** #### slip by, and not be very easy to find.
	 ** #### Bill Perry July 9, 2000
	 */
	if (n_args != XFFI(func)->n_args) {
		Lisp_Object for_append[3];

		/* Signal an error if they pass in too many arguments */
		if (n_args > XFFI(func)->n_args) {
			return Fsignal(Qwrong_number_of_arguments,
				       list2(func, make_int(n_args)));
		}

		/* If they did not provide enough arguments, be nice and assume
		 ** they wanted `nil' in there.
		 */
		for_append[0] = args;
		for_append[1] =
		    Fmake_list(make_int(XFFI(func)->n_args - n_args), Qnil);

		args = Fappend(2, for_append);
	}
#else
	if (n_args != XFFI(func)->n_args) {
		/* Signal an error if they do not pass in the correct # of arguments */
		return Fsignal(Qwrong_number_of_arguments,
			       list2(func, make_int(n_args)));
	}
#endif

	if (!NILP(args)) {
		Lisp_Object tail = Qnil;
		Lisp_Object value = args;

		CHECK_LIST(args);
		n_args = 0;

		/* First we convert all of the arguments from Lisp to GtkArgs */
		EXTERNAL_LIST_LOOP(tail, value) {
			the_args[n_args].type = XFFI(func)->args[n_args];

			if (lisp_to_gtk_type(XCAR(tail), &the_args[n_args])) {
				/* There was some sort of an error */
				signal_simple_error
				    ("Error converting arguments", args);
			}
			n_args++;
		}
	}

	/* Now we need to tack on space for a return value, if they have
	   asked for one */
	if (XFFI(func)->return_type != GTK_TYPE_NONE) {
		the_args[n_args].type = XFFI(func)->return_type;
		GTK_VALUE_POINTER(the_args[n_args]) =
		    __allocate_object_storage(the_args[n_args].type);
		n_args++;
	}

	XFFI(func)->marshal((ffi_actual_function) (XFFI(func)->function_ptr),
			    the_args);

	if (XFFI(func)->return_type != GTK_TYPE_NONE) {
		CONVERT_RETVAL(the_args[n_args - 1], 1);
		retval = gtk_type_to_lisp(&the_args[n_args - 1]);
	}

	/* Need to free any array or list pointers */
	{
		int i;
		for (i = 0; i < n_args; i++) {
			if (GTK_FUNDAMENTAL_TYPE(the_args[i].type) ==
			    GTK_TYPE_ARRAY) {
				g_free(GTK_VALUE_POINTER(the_args[i]));
			} else if (GTK_FUNDAMENTAL_TYPE(the_args[i].type) ==
				   GTK_TYPE_LISTOF) {
				/* g_list_free (GTK_VALUE_POINTER (the_args[i])); */
			}
		}
	}

	return (retval);
}

/* GtkObject wrapping for Lisp */
static void
emacs_gtk_object_printer(Lisp_Object obj, Lisp_Object printcharfun,
			 int escapeflag)
{
	char buf[200];

	if (print_readably)
		error("printing unreadable object #<GtkObject %p>",
		      XGTK_OBJECT(obj)->object);

	write_c_string("#<GtkObject (", printcharfun);
	if (XGTK_OBJECT(obj)->alive_p)
		write_c_string(gtk_type_name
			       (GTK_OBJECT_TYPE(XGTK_OBJECT(obj)->object)),
			       printcharfun);
	else
		write_c_string("dead", printcharfun);
	sprintf(buf, ") %p>", (void *)XGTK_OBJECT(obj)->object);
	write_c_string(buf, printcharfun);
}

static Lisp_Object object_getprop(Lisp_Object obj, Lisp_Object prop)
{
	Lisp_Object rval = Qnil;
	Lisp_Object prop_name = Qnil;
	GtkArgInfo *info = NULL;
	char *err;
	GtkArg args[2];

	CHECK_SYMBOL(prop);	/* Shouldn't need to ever do this, but I'm paranoid */

	prop_name = Fsymbol_name(prop);

	args[0].name = (char *)XSTRING_DATA(prop_name);

	err = gtk_object_arg_get_info(GTK_OBJECT_TYPE(XGTK_OBJECT(obj)->object),
				      args[0].name, &info);

	if (err) {
		/* Not a magic symbol, fall back to just looking in our real plist */
		g_free(err);

		return (Fplist_get(XGTK_OBJECT(obj)->plist, prop, Qunbound));
	}

	if (!(info->arg_flags & GTK_ARG_READABLE)) {
		signal_simple_error("Attempt to get write-only property", prop);
	}

	gtk_object_getv(XGTK_OBJECT(obj)->object, 1, args);

	if (args[0].type == GTK_TYPE_INVALID) {
		/* If we can't get the attribute, then let the code in Fget know
		   so it can use the default value supplied by the caller */
		return (Qunbound);
	}

	rval = gtk_type_to_lisp(&args[0]);

	/* Free up any memory.  According to the documentation and Havoc's
	   book, if the fundamental type of the returned value is
	   GTK_TYPE_STRING, GTK_TYPE_BOXED, or GTK_TYPE_ARGS, you are
	   responsible for freeing it. */
	switch (GTK_FUNDAMENTAL_TYPE(args[0].type)) {
	case GTK_TYPE_STRING:
		g_free(GTK_VALUE_STRING(args[0]));
		break;
	case GTK_TYPE_BOXED:
		g_free(GTK_VALUE_BOXED(args[0]));
		break;
	case GTK_TYPE_ARGS:
		g_free(GTK_VALUE_ARGS(args[0]).args);
	default:
		break;
	}

	return (rval);
}

static int object_putprop(Lisp_Object obj, Lisp_Object prop, Lisp_Object value)
{
	GtkArgInfo *info = NULL;
	Lisp_Object prop_name = Qnil;
	GtkArg args[2];
	char *err = NULL;

	prop_name = Fsymbol_name(prop);

	args[0].name = (char *)XSTRING_DATA(prop_name);

	err = gtk_object_arg_get_info(GTK_OBJECT_TYPE(XGTK_OBJECT(obj)->object),
				      args[0].name, &info);

	if (err) {
		/* Not a magic symbol, fall back to just storing in our real plist */
		g_free(err);

		XGTK_OBJECT(obj)->plist =
		    Fplist_put(XGTK_OBJECT(obj)->plist, prop, value);
		return (1);
	}

	args[0].type = info->type;

	if (lisp_to_gtk_type(value, &args[0])) {
		signal_simple_error("Error converting to GtkType", value);
	}

	if (!(info->arg_flags & GTK_ARG_WRITABLE)) {
		signal_simple_error("Attemp to set read-only argument", prop);
	}

	gtk_object_setv(XGTK_OBJECT(obj)->object, 1, args);

	return (1);
}

static Lisp_Object mark_gtk_object_data(Lisp_Object obj)
{
	return (XGTK_OBJECT(obj)->plist);
}

static void emacs_gtk_object_finalizer(void *header, int for_disksave)
{
	emacs_gtk_object_data *data = (emacs_gtk_object_data *) header;

	if (for_disksave) {
		Lisp_Object obj;
		XSETGTK_OBJECT(obj, data);

		signal_simple_error
		    ("Can't dump an emacs containing GtkObject objects", obj);
	}

	if (data->alive_p) {
		gtk_object_unref(data->object);
	}
}

DEFINE_LRECORD_IMPLEMENTATION_WITH_PROPS("GtkObject", emacs_gtk_object, mark_gtk_object_data,	/* marker function */
					 emacs_gtk_object_printer,	/* print function */
					 emacs_gtk_object_finalizer,	/* finalizer */
					 0,	/* equality */
					 0,	/* hash */
					 NULL,	/* desc */
					 object_getprop,	/* get prop */
					 object_putprop,	/* put prop */
					 0,	/* rem prop */
					 0,	/* plist */
					 emacs_gtk_object_data);

static emacs_gtk_object_data *allocate_emacs_gtk_object_data(void)
{
	emacs_gtk_object_data *data = alloc_lcrecord_type(emacs_gtk_object_data,
							  &lrecord_emacs_gtk_object);

	data->object = NULL;
	data->alive_p = FALSE;
	data->plist = Qnil;

	return (data);
}

/* We need to keep track of when the object is destroyed so that we
   can mark it as dead, otherwise even our print routine (which calls
   GTK_OBJECT_TYPE) will crap out and die.  This is also used in the
   lisp_to_gtk_type() routine to defend against passing dead objects
   to GTK routines. */
static void __notice_object_destruction(GtkObject * obj, gpointer user_data)
{
	ungcpro_popup_callbacks((GUI_ID) user_data);
}

Lisp_Object build_gtk_object(GtkObject * obj)
{
	Lisp_Object retval = Qnil;
	emacs_gtk_object_data *data = NULL;
	GUI_ID id = 0;

	id = (GUI_ID) gtk_object_get_data(obj, GTK_DATA_GUI_IDENTIFIER);

	if (id) {
		retval = get_gcpro_popup_callbacks(id);
	}

	if (NILP(retval)) {
		data = allocate_emacs_gtk_object_data();

		data->object = obj;
		data->alive_p = TRUE;
		XSETGTK_OBJECT(retval, data);

		id = new_gui_id();
		gtk_object_set_data(obj, GTK_DATA_GUI_IDENTIFIER,
				    (gpointer) id);
		gcpro_popup_callbacks(id, retval);
		gtk_object_ref(obj);
		gtk_signal_connect(obj, "destroy",
				   GTK_SIGNAL_FUNC(__notice_object_destruction),
				   (gpointer) id);
	}

	return (retval);
}

static void __internal_callback_destroy(gpointer data)
{
	Lisp_Object lisp_data;

	VOID_TO_LISP(lisp_data, data);

	ungcpro_popup_callbacks(XINT(XCAR(lisp_data)));
}

static void
__internal_callback_marshal(GtkObject * obj, gpointer data, guint n_args,
			    GtkArg * args)
{
	Lisp_Object arg_list = Qnil;
	Lisp_Object callback_fn = Qnil;
	Lisp_Object callback_data = Qnil;
	Lisp_Object newargs[3];
	Lisp_Object rval = Qnil;
	struct gcpro gcpro1;
	int i;

	VOID_TO_LISP(callback_fn, data);

	/* Nuke the GUI_ID off the front */
	callback_fn = XCDR(callback_fn);

	callback_data = XCAR(callback_fn);
	callback_fn = XCDR(callback_fn);

	/* The callback data goes at the very end of the argument list */
	arg_list = Fcons(callback_data, Qnil);

	/* Build up the argument list, lisp style */
	for (i = n_args - 1; i >= 0; i--) {
		arg_list = Fcons(gtk_type_to_lisp(&args[i]), arg_list);
	}

	/* We always pass the widget as the first parameter at the very least */
	arg_list = Fcons(build_gtk_object(obj), arg_list);

	GCPRO1((arg_list));

	newargs[0] = callback_fn;
	newargs[1] = arg_list;

	rval = Fapply(2, newargs);
	signal_fake_event();

	if (args[n_args].type != GTK_TYPE_NONE)
		lisp_to_gtk_ret_type(rval, &args[n_args]);

	UNGCPRO;
}

DEFUN("gtk-signal-connect", Fgtk_signal_connect, 3, 6, 0,	/*
<<<<<<< HEAD
								 */
=======
*/
>>>>>>> origin/master
      (obj, name, func, cb_data, object_signal, after_p))
{
	int c_after;
	int c_object_signal;
	GUI_ID id = 0;

	CHECK_GTK_OBJECT(obj);

	if (SYMBOLP(name))
		name = Fsymbol_name(name);

	CHECK_STRING(name);

	if (NILP(object_signal))
		c_object_signal = 0;
	else
		c_object_signal = 1;

	if (NILP(after_p))
		c_after = 0;
	else
		c_after = 1;

	id = new_gui_id();
	func = Fcons(cb_data, func);
	func = Fcons(make_int(id), func);

	gcpro_popup_callbacks(id, func);

	gtk_signal_connect_full(XGTK_OBJECT(obj)->object,
				(char *)XSTRING_DATA(name), NULL,
				__internal_callback_marshal, LISP_TO_VOID(func),
				__internal_callback_destroy, c_object_signal,
				c_after);
	return (Qt);
}

/* GTK_TYPE_BOXED wrapper for Emacs lisp */
static void
emacs_gtk_boxed_printer(Lisp_Object obj, Lisp_Object printcharfun,
			int escapeflag)
{
	char buf[200];

	if (print_readably)
		error("printing unreadable object #<GtkBoxed %p>",
		      XGTK_BOXED(obj)->object);

	write_c_string("#<GtkBoxed (", printcharfun);
	write_c_string(gtk_type_name(XGTK_BOXED(obj)->object_type),
		       printcharfun);
	sprintf(buf, ") %p>", (void *)XGTK_BOXED(obj)->object);
	write_c_string(buf, printcharfun);
}

static int emacs_gtk_boxed_equality(Lisp_Object o1, Lisp_Object o2, int depth)
{
	emacs_gtk_boxed_data *data1 = XGTK_BOXED(o1);
	emacs_gtk_boxed_data *data2 = XGTK_BOXED(o2);

	return ((data1->object == data2->object) &&
		(data1->object_type == data2->object_type));
}

static unsigned long emacs_gtk_boxed_hash(Lisp_Object obj, int depth)
{
	emacs_gtk_boxed_data *data = XGTK_BOXED(obj);
	return (HASH2((unsigned long)data->object, data->object_type));
}

DEFINE_LRECORD_IMPLEMENTATION_WITH_PROPS("GtkBoxed", emacs_gtk_boxed, 0,	/* marker function */
					 emacs_gtk_boxed_printer,	/* print function */
					 0,	/* nuker */
					 emacs_gtk_boxed_equality,	/* equality */
					 emacs_gtk_boxed_hash,	/* hash */
					 NULL,	/* desc */
					 0,	/* get prop */
					 0,	/* put prop */
					 0,	/* rem prop */
					 0,	/* plist */
					 emacs_gtk_boxed_data);

/* Currently defined GTK_TYPE_BOXED structures are:

   GtkAccelGroup -
   GtkSelectionData -
   GtkStyle -
   GtkCTreeNode - 
   GdkColormap -
   GdkVisual -
   GdkFont -
   GdkWindow -
   GdkDragContext -
   GdkEvent -
   GdkColor - 
*/
static emacs_gtk_boxed_data *allocate_emacs_gtk_boxed_data(void)
{
	emacs_gtk_boxed_data *data = alloc_lcrecord_type(emacs_gtk_boxed_data,
							 &lrecord_emacs_gtk_boxed);

	data->object = NULL;
	data->object_type = GTK_TYPE_INVALID;

	return (data);
}

Lisp_Object build_gtk_boxed(void *obj, GtkType t)
{
	Lisp_Object retval = Qnil;
	emacs_gtk_boxed_data *data = NULL;

	if (GTK_FUNDAMENTAL_TYPE(t) != GTK_TYPE_BOXED)
		abort();

	data = allocate_emacs_gtk_boxed_data();
	data->object = obj;
	data->object_type = t;

	XSETGTK_BOXED(retval, data);

	return (retval);
}

/* The automatically generated structure access routines */
#include "emacs-widget-accessors.c"

/* The hand generated funky functions that we can't just import using the FFI */
#include "ui-byhand.c"

/* The glade support */
#include "glade.c"

/* Type manipulation */
DEFUN("gtk-fundamental-type", Fgtk_fundamental_type, 1, 1, 0,	/*
<<<<<<< HEAD
								   Load a shared library DLL into XEmacs.  No initialization routines are required.
								   This is for loading dependency DLLs into XEmacs.
								 */
=======
Load a shared library DLL into XEmacs.  No initialization routines are required.
This is for loading dependency DLLs into XEmacs.
*/
>>>>>>> origin/master
      (type))
{
	GtkType t;

	if (SYMBOLP(type))
		type = Fsymbol_name(type);

	CHECK_STRING(type);

	t = gtk_type_from_name((char *)XSTRING_DATA(type));

	if (t == GTK_TYPE_INVALID) {
		signal_simple_error("Not a GTK type", type);
	}
	return (make_int(GTK_FUNDAMENTAL_TYPE(t)));
}

DEFUN("gtk-object-type", Fgtk_object_type, 1, 1, 0,	/*
<<<<<<< HEAD
							   Return the GtkType of OBJECT.
							 */
=======
Return the GtkType of OBJECT.
*/
>>>>>>> origin/master
      (object))
{
	CHECK_GTK_OBJECT(object);
	return (make_int(GTK_OBJECT_TYPE(XGTK_OBJECT(object)->object)));
}

DEFUN("gtk-describe-type", Fgtk_describe_type, 1, 1, 0,	/*
<<<<<<< HEAD
							   Returns a cons of two lists describing the Gtk object TYPE.
							   The car is a list of all the signals that it will emit.
							   The cdr is a list of all the magic properties it has.
							 */
=======
Returns a cons of two lists describing the Gtk object TYPE.
The car is a list of all the signals that it will emit.
The cdr is a list of all the magic properties it has.
*/
>>>>>>> origin/master
      (type))
{
	Lisp_Object rval, signals, props;
	GtkType t;

	props = signals = rval = Qnil;

	if (SYMBOLP(type)) {
		type = Fsymbol_name(type);
	}

	if (STRINGP(type)) {
		t = gtk_type_from_name(XSTRING_DATA(type));
		if (t == GTK_TYPE_INVALID) {
			signal_simple_error("Not a GTK type", type);
		}
	} else {
		CHECK_INT(type);
		t = XINT(type);
	}

	if (GTK_FUNDAMENTAL_TYPE(t) != GTK_TYPE_OBJECT) {
		signal_simple_error("Not a GtkObject", type);
	}

	/* Need to do stupid shit like this to get the args
	 ** registered... damn GTK and its lazy loading
	 */
	{
		GtkArg args[3];
		GtkObject *obj = gtk_object_newv(t, 0, args);

		gtk_object_destroy(obj);
	}

	do {
		guint i;

		/* Do the magic arguments first */
		{
			GtkArg *args;
			guint32 *flags;
			guint n_args;

			args = gtk_object_query_args(t, &flags, &n_args);

			for (i = 0; i < n_args; i++) {
				props =
				    Fcons(Fcons
					  (intern(gtk_type_name(args[i].type)),
					   intern(args[i].name)), props);
			}

			g_free(args);
			g_free(flags);
		}

		/* Now the signals */
		{
			GtkObjectClass *klass;
			GtkSignalQuery *query;
			guint32 *gtk_signals;
			guint n_signals;

			klass = (GtkObjectClass *) gtk_type_class(t);
			gtk_signals = klass->signals;
			n_signals = klass->nsignals;

			for (i = 0; i < n_signals; i++) {
				Lisp_Object params = Qnil;

				query = gtk_signal_query(gtk_signals[i]);

				if (query) {
					if (query->nparams) {
						int j;

						for (j = query->nparams - 1;
						     j >= 0; j--) {
							params =
							    Fcons(intern
								  (gtk_type_name
								   (query->
								    params[j])),
								  params);
						}
					}

					signals =
					    Fcons(Fcons
						  (intern
						   (gtk_type_name
						    (query->return_val)),
						   Fcons(intern
							 (query->signal_name),
							 params)), signals);

					g_free(query);
				}
			}
		}
		t = gtk_type_parent(t);
	} while (t != GTK_TYPE_INVALID);

	rval = Fcons(signals, props);

	return (rval);
}

void syms_of_ui_gtk(void)
{
	INIT_LRECORD_IMPLEMENTATION(emacs_ffi);
	INIT_LRECORD_IMPLEMENTATION(emacs_gtk_object);
	INIT_LRECORD_IMPLEMENTATION(emacs_gtk_boxed);
	defsymbol(&Qemacs_ffip, "emacs-ffi-p");
	defsymbol(&Qemacs_gtk_objectp, "emacs-gtk-object-p");
	defsymbol(&Qemacs_gtk_boxedp, "emacs-gtk-boxed-p");
	defsymbol(&Qvoid, "void");
	DEFSUBR(Fdll_load);
	DEFSUBR(Fgtk_import_function_internal);
	DEFSUBR(Fgtk_import_variable_internal);
	DEFSUBR(Fgtk_signal_connect);
	DEFSUBR(Fgtk_call_function);
	DEFSUBR(Fgtk_fundamental_type);
	DEFSUBR(Fgtk_object_type);
	DEFSUBR(Fgtk_describe_type);
	syms_of_widget_accessors();
	syms_of_ui_byhand();
	syms_of_glade();
}

void vars_of_ui_gtk(void)
{
	Fprovide(intern("gtk-ui"));
	DEFVAR_LISP("gtk-enumeration-info", &Venumeration_info	/*
								   A hashtable holding type information about GTK enumerations and flags.
								   Do NOT modify unless you really understand ui-gtk.c.
								 */ );

	Venumeration_info = Qnil;
	vars_of_glade();
}

/* Various utility functions */
void describe_gtk_arg(GtkArg * arg)
{
	GtkArg a = *arg;

	switch (GTK_FUNDAMENTAL_TYPE(a.type)) {
		/* flag types */
	case GTK_TYPE_CHAR:
		stderr_out("char: %c\n", GTK_VALUE_CHAR(a));
		break;
	case GTK_TYPE_UCHAR:
		stderr_out("uchar: %c\n", GTK_VALUE_CHAR(a));
		break;
	case GTK_TYPE_BOOL:
		stderr_out("uchar: %s\n", GTK_VALUE_BOOL(a) ? "true" : "false");
		break;
	case GTK_TYPE_INT:
		stderr_out("int: %d\n", GTK_VALUE_INT(a));
		break;
	case GTK_TYPE_UINT:
		stderr_out("uint: %du\n", GTK_VALUE_UINT(a));
		break;
	case GTK_TYPE_LONG:
		stderr_out("long: %ld\n", GTK_VALUE_LONG(a));
		break;
	case GTK_TYPE_ULONG:
		stderr_out("ulong: %lu\n", GTK_VALUE_ULONG(a));
		break;
	case GTK_TYPE_FLOAT:
		stderr_out("float: %g\n", GTK_VALUE_FLOAT(a));
		break;
	case GTK_TYPE_DOUBLE:
		stderr_out("double: %f\n", GTK_VALUE_DOUBLE(a));
		break;
	case GTK_TYPE_STRING:
		stderr_out("string: %s\n", GTK_VALUE_STRING(a));
		break;
	case GTK_TYPE_ENUM:
	case GTK_TYPE_FLAGS:
		stderr_out("%s: ", (a.type == GTK_TYPE_ENUM) ? "enum" : "flag");
		{
			GtkEnumValue *vals = gtk_type_enum_get_values(a.type);

			while (vals && vals->value_name
			       && (vals->value != GTK_VALUE_ENUM(a)))
				vals++;

			stderr_out("%s\n",
				   vals ? vals->
				   value_name : "!!! UNKNOWN ENUM VALUE !!!");
		}
		break;
	case GTK_TYPE_BOXED:
		stderr_out("boxed: %p\n", GTK_VALUE_BOXED(a));
		break;
	case GTK_TYPE_POINTER:
		stderr_out("pointer: %p\n", GTK_VALUE_BOXED(a));
		break;

		/* structured types */
	case GTK_TYPE_SIGNAL:
	case GTK_TYPE_ARGS:	/* This we can do as a list of values */
		abort();
	case GTK_TYPE_CALLBACK:
		stderr_out("callback fn: ...\n");
		break;
	case GTK_TYPE_C_CALLBACK:
	case GTK_TYPE_FOREIGN:
		abort();

		/* base type of the object system */
	case GTK_TYPE_OBJECT:
		if (GTK_VALUE_OBJECT(a))
			stderr_out("object: %s\n",
				   gtk_type_name(GTK_OBJECT_TYPE
						 (GTK_VALUE_OBJECT(a))));
		else
			stderr_out("object: NULL\n");
		break;

	default:
		abort();
	}
}

Lisp_Object gtk_type_to_lisp(GtkArg * arg)
{
	switch (GTK_FUNDAMENTAL_TYPE(arg->type)) {
	case GTK_TYPE_NONE:
		return (Qnil);
	case GTK_TYPE_CHAR:
		return (make_char(GTK_VALUE_CHAR(*arg)));
	case GTK_TYPE_UCHAR:
		return (make_char(GTK_VALUE_UCHAR(*arg)));
	case GTK_TYPE_BOOL:
		return (GTK_VALUE_BOOL(*arg) ? Qt : Qnil);
	case GTK_TYPE_INT:
		return (make_int(GTK_VALUE_INT(*arg)));
	case GTK_TYPE_UINT:
		return (make_int(GTK_VALUE_INT(*arg)));
	case GTK_TYPE_LONG:	/* I think these are wrong! */
		return (make_int(GTK_VALUE_INT(*arg)));
	case GTK_TYPE_ULONG:	/* I think these are wrong! */
		return (make_int(GTK_VALUE_INT(*arg)));
	case GTK_TYPE_FLOAT:
		return (make_float(GTK_VALUE_FLOAT(*arg)));
	case GTK_TYPE_DOUBLE:
		return (make_float(GTK_VALUE_DOUBLE(*arg)));
	case GTK_TYPE_STRING:
		return (build_string(GTK_VALUE_STRING(*arg)));
	case GTK_TYPE_FLAGS:
		return (flags_to_list(GTK_VALUE_FLAGS(*arg), arg->type));
	case GTK_TYPE_ENUM:
		return (enum_to_symbol(GTK_VALUE_ENUM(*arg), arg->type));
	case GTK_TYPE_BOXED:
		if (arg->type == GTK_TYPE_GDK_EVENT) {
			return (gdk_event_to_emacs_event
				((GdkEvent *) GTK_VALUE_BOXED(*arg)));
		}

		if (GTK_VALUE_BOXED(*arg))
			return (build_gtk_boxed
				(GTK_VALUE_BOXED(*arg), arg->type));
		else
			return (Qnil);
	case GTK_TYPE_POINTER:
		if (GTK_VALUE_POINTER(*arg)) {
			Lisp_Object rval;

			VOID_TO_LISP(rval, GTK_VALUE_POINTER(*arg));
			return (rval);
		} else
			return (Qnil);
	case GTK_TYPE_OBJECT:
		if (GTK_VALUE_OBJECT(*arg))
			return (build_gtk_object(GTK_VALUE_OBJECT(*arg)));
		else
			return (Qnil);

	case GTK_TYPE_CALLBACK:
		{
			Lisp_Object rval;

			VOID_TO_LISP(rval, GTK_VALUE_CALLBACK(*arg).data);

			return (rval);
		}

	default:
		if (GTK_FUNDAMENTAL_TYPE(arg->type) == GTK_TYPE_LISTOF) {
			if (!GTK_VALUE_POINTER(*arg))
				return (Qnil);
			else {
				return (xemacs_gtklist_to_list(arg));
			}
		}
		stderr_out("Do not know how to convert `%s' to lisp!\n",
			   gtk_type_name(arg->type));
		abort();
	}
	/* This is chuck reminding GCC to... SHUT UP! */
	return (Qnil);
}

int lisp_to_gtk_type(Lisp_Object obj, GtkArg * arg)
{
	switch (GTK_FUNDAMENTAL_TYPE(arg->type)) {
		/* flag types */
	case GTK_TYPE_NONE:
		return (0);
	case GTK_TYPE_CHAR:
		{
			Emchar c;

			CHECK_CHAR_COERCE_INT(obj);
			c = XCHAR(obj);
			GTK_VALUE_CHAR(*arg) = c;
		}
		break;
	case GTK_TYPE_UCHAR:
		{
			Emchar c;

			CHECK_CHAR_COERCE_INT(obj);
			c = XCHAR(obj);
			GTK_VALUE_CHAR(*arg) = c;
		}
		break;
	case GTK_TYPE_BOOL:
		GTK_VALUE_BOOL(*arg) = NILP(obj) ? FALSE : TRUE;
		break;
	case GTK_TYPE_INT:
	case GTK_TYPE_UINT:
		if (NILP(obj) || EQ(Qt, obj)) {
			/* For we are a kind mistress and allow sending t/nil for
			   1/0 to stupid GTK functions that say they take guint or
			   gint in the header files, but actually treat it like a
			   bool.  *sigh*
			 */
			GTK_VALUE_INT(*arg) = NILP(obj) ? 0 : 1;
		} else {
			CHECK_INT(obj);
			GTK_VALUE_INT(*arg) = XINT(obj);
		}
		break;
	case GTK_TYPE_LONG:
	case GTK_TYPE_ULONG:
		abort();
	case GTK_TYPE_FLOAT:
#ifdef WITH_NUMBER_TYPES
		CHECK_NUMBER(obj);
#else
		CHECK_INT_OR_FLOAT(obj);
#endif
		GTK_VALUE_FLOAT(*arg) = extract_float(obj);
		break;
	case GTK_TYPE_DOUBLE:
#ifdef WITH_NUMBER_TYPES
		CHECK_NUMBER(obj);
#else
		CHECK_INT_OR_FLOAT(obj);
#endif
		GTK_VALUE_DOUBLE(*arg) = extract_float(obj);
		break;
	case GTK_TYPE_STRING:
		if (NILP(obj))
			GTK_VALUE_STRING(*arg) = NULL;
		else {
			CHECK_STRING(obj);
			GTK_VALUE_STRING(*arg) = (char *)XSTRING_DATA(obj);
		}
		break;
	case GTK_TYPE_ENUM:
	case GTK_TYPE_FLAGS:
		/* Convert a lisp symbol to a GTK enum */
		GTK_VALUE_ENUM(*arg) = lisp_to_flag(obj, arg->type);
		break;
	case GTK_TYPE_BOXED:
		if (NILP(obj)) {
			GTK_VALUE_BOXED(*arg) = NULL;
		} else if (GTK_BOXEDP(obj)) {
			GTK_VALUE_BOXED(*arg) = XGTK_BOXED(obj)->object;
		} else if (arg->type == GTK_TYPE_STYLE) {
			obj = Ffind_face(obj);
			CHECK_FACE(obj);
			GTK_VALUE_BOXED(*arg) = face_to_style(obj);
		} else if (arg->type == GTK_TYPE_GDK_GC) {
			obj = Ffind_face(obj);
			CHECK_FACE(obj);
			GTK_VALUE_BOXED(*arg) = face_to_gc(obj);
		} else if (arg->type == GTK_TYPE_GDK_WINDOW) {
			if (GLYPHP(obj)) {
				Lisp_Object window = Fselected_window(Qnil);
				Lisp_Object instance =
				    glyph_image_instance(obj, window,
							 ERROR_ME_NOT, 1);
				struct Lisp_Image_Instance *p =
				    XIMAGE_INSTANCE(instance);

				switch (XIMAGE_INSTANCE_TYPE(instance)) {
				case IMAGE_TEXT:
				case IMAGE_POINTER:
				case IMAGE_SUBWINDOW:
				case IMAGE_NOTHING:
					GTK_VALUE_BOXED(*arg) = NULL;
					break;

				case IMAGE_MONO_PIXMAP:
				case IMAGE_COLOR_PIXMAP:
					GTK_VALUE_BOXED(*arg) =
					    IMAGE_INSTANCE_GTK_PIXMAP(p);
					break;
				}
			} else if (GTK_OBJECTP(obj)
				   && GTK_IS_WIDGET(XGTK_OBJECT(obj)->object)) {
				GTK_VALUE_BOXED(*arg) =
				    GTK_WIDGET(XGTK_OBJECT(obj))->window;
			} else {
				signal_simple_error
				    ("Don't know how to convert object to GDK_WINDOW",
				     obj);
			}
			break;
		} else if (arg->type == GTK_TYPE_GDK_COLOR) {
			if (COLOR_SPECIFIERP(obj)) {
				/* If it is a specifier, we just convert it to an
				   instance, and let the ifs below handle it.
				 */
				obj =
				    Fspecifier_instance(obj, Qnil, Qnil, Qnil);
			}

			if (COLOR_INSTANCEP(obj)) {
				/* Easiest one */
				GTK_VALUE_BOXED(*arg) =
				    COLOR_INSTANCE_GTK_COLOR(XCOLOR_INSTANCE
							     (obj));
			} else if (STRINGP(obj)) {
				signal_simple_error
				    ("Please use a color specifier or instance, not a string",
				     obj);
			} else {
				signal_simple_error
				    ("Don't know hot to convert to GdkColor",
				     obj);
			}
		} else if (arg->type == GTK_TYPE_GDK_FONT) {
			if (SYMBOLP(obj)) {
				/* If it is a symbol, we treat that as a face name */
				obj = Ffind_face(obj);
			}

			if (FACEP(obj)) {
				/* If it is a face, we just grab the font specifier, and
				   cascade down until we finally reach a FONT_INSTANCE
				 */
				obj = Fget(obj, Qfont, Qnil);
			}

			if (FONT_SPECIFIERP(obj)) {
				/* If it is a specifier, we just convert it to an
				   instance, and let the ifs below handle it
				 */
				obj =
				    Fspecifier_instance(obj, Qnil, Qnil, Qnil);
			}

			if (FONT_INSTANCEP(obj)) {
				/* Easiest one */
				GTK_VALUE_BOXED(*arg) =
				    FONT_INSTANCE_GTK_FONT(XFONT_INSTANCE(obj));
			} else if (STRINGP(obj)) {
				signal_simple_error
				    ("Please use a font specifier or instance, not a string",
				     obj);
			} else {
				signal_simple_error
				    ("Don't know hot to convert to GdkColor",
				     obj);
			}
		} else {
			/* Unknown type to convert to boxed */
			stderr_out("Don't know how to convert to boxed!\n");
			GTK_VALUE_BOXED(*arg) = NULL;
		}
		break;

	case GTK_TYPE_POINTER:
		if (NILP(obj))
			GTK_VALUE_POINTER(*arg) = NULL;
		else
			GTK_VALUE_POINTER(*arg) = LISP_TO_VOID(obj);
		break;

		/* structured types */
	case GTK_TYPE_SIGNAL:
	case GTK_TYPE_ARGS:	/* This we can do as a list of values */
	case GTK_TYPE_C_CALLBACK:
	case GTK_TYPE_FOREIGN:
		stderr_out("Do not know how to convert `%s' from lisp!\n",
			   gtk_type_name(arg->type));
		return (-1);

#if 0
		/* #### BILL! */
		/* This is not used, and does not work with union type */
	case GTK_TYPE_CALLBACK:
		{
			GUI_ID id;

			id = new_gui_id();
			obj = Fcons(Qnil, obj);	/* Empty data */
			obj = Fcons(make_int(id), obj);

			gcpro_popup_callbacks(id, obj);

			GTK_VALUE_CALLBACK(*arg).marshal =
			    __internal_callback_marshal;
			GTK_VALUE_CALLBACK(*arg).data = (gpointer) obj;
			GTK_VALUE_CALLBACK(*arg).notify =
			    __internal_callback_destroy;
		}
		break;
#endif

		/* base type of the object system */
	case GTK_TYPE_OBJECT:
		if (NILP(obj))
			GTK_VALUE_OBJECT(*arg) = NULL;
		else {
			CHECK_GTK_OBJECT(obj);
			if (XGTK_OBJECT(obj)->alive_p)
				GTK_VALUE_OBJECT(*arg) =
				    XGTK_OBJECT(obj)->object;
			else
				signal_simple_error
				    ("Attempting to pass dead object to GTK function",
				     obj);
		}
		break;

	default:
		if (GTK_FUNDAMENTAL_TYPE(arg->type) == GTK_TYPE_ARRAY) {
			if (NILP(obj))
				GTK_VALUE_POINTER(*arg) = NULL;
			else {
				xemacs_list_to_array(obj, arg);
			}
		} else if (GTK_FUNDAMENTAL_TYPE(arg->type) == GTK_TYPE_LISTOF) {
			if (NILP(obj))
				GTK_VALUE_POINTER(*arg) = NULL;
			else {
				xemacs_list_to_gtklist(obj, arg);
			}
		} else {
			stderr_out
			    ("Do not know how to convert `%s' from lisp!\n",
			     gtk_type_name(arg->type));
			abort();
		}
		break;
	}

	return (0);
}

/* Convert lisp types to GTK return types.  This is identical to
   lisp_to_gtk_type() except that the macro used to set the value is
   different.

   ### There should be some way of combining these two functions.
*/
int lisp_to_gtk_ret_type(Lisp_Object obj, GtkArg * arg)
{
	switch (GTK_FUNDAMENTAL_TYPE(arg->type)) {
		/* flag types */
	case GTK_TYPE_NONE:
		return (0);
	case GTK_TYPE_CHAR:
		{
			Emchar c;

			CHECK_CHAR_COERCE_INT(obj);
			c = XCHAR(obj);
			*(GTK_RETLOC_CHAR(*arg)) = c;
		}
		break;
	case GTK_TYPE_UCHAR:
		{
			Emchar c;

			CHECK_CHAR_COERCE_INT(obj);
			c = XCHAR(obj);
			*(GTK_RETLOC_CHAR(*arg)) = c;
		}
		break;
	case GTK_TYPE_BOOL:
		*(GTK_RETLOC_BOOL(*arg)) = NILP(obj) ? FALSE : TRUE;
		break;
	case GTK_TYPE_INT:
	case GTK_TYPE_UINT:
		if (NILP(obj) || EQ(Qt, obj)) {
			/* For we are a kind mistress and allow sending t/nil for
			   1/0 to stupid GTK functions that say they take guint or
			   gint in the header files, but actually treat it like a
			   bool.  *sigh*
			 */
			*(GTK_RETLOC_INT(*arg)) = NILP(obj) ? 0 : 1;
		} else {
			CHECK_INT(obj);
			*(GTK_RETLOC_INT(*arg)) = XINT(obj);
		}
		break;
	case GTK_TYPE_LONG:
	case GTK_TYPE_ULONG:
		abort();
	case GTK_TYPE_FLOAT:
#ifdef WITH_NUMBER_TYPES
		CHECK_NUMBER(obj);
#else
		CHECK_INT_OR_FLOAT(obj);
#endif
		*(GTK_RETLOC_FLOAT(*arg)) = extract_float(obj);
		break;
	case GTK_TYPE_DOUBLE:
#ifdef WITH_NUMBER_TYPES
		CHECK_NUMBER(obj);
#else
		CHECK_INT_OR_FLOAT(obj);
#endif
		*(GTK_RETLOC_DOUBLE(*arg)) = extract_float(obj);
		break;
	case GTK_TYPE_STRING:
		if (NILP(obj))
			*(GTK_RETLOC_STRING(*arg)) = NULL;
		else {
			CHECK_STRING(obj);
			*(GTK_RETLOC_STRING(*arg)) = (char *)XSTRING_DATA(obj);
		}
		break;
	case GTK_TYPE_ENUM:
	case GTK_TYPE_FLAGS:
		/* Convert a lisp symbol to a GTK enum */
		*(GTK_RETLOC_ENUM(*arg)) = lisp_to_flag(obj, arg->type);
		break;
	case GTK_TYPE_BOXED:
		if (NILP(obj)) {
			*(GTK_RETLOC_BOXED(*arg)) = NULL;
		} else if (GTK_BOXEDP(obj)) {
			*(GTK_RETLOC_BOXED(*arg)) = XGTK_BOXED(obj)->object;
		} else if (arg->type == GTK_TYPE_STYLE) {
			obj = Ffind_face(obj);
			CHECK_FACE(obj);
			*(GTK_RETLOC_BOXED(*arg)) = face_to_style(obj);
		} else if (arg->type == GTK_TYPE_GDK_GC) {
			obj = Ffind_face(obj);
			CHECK_FACE(obj);
			*(GTK_RETLOC_BOXED(*arg)) = face_to_gc(obj);
		} else if (arg->type == GTK_TYPE_GDK_WINDOW) {
			if (GLYPHP(obj)) {
				Lisp_Object window = Fselected_window(Qnil);
				Lisp_Object instance =
				    glyph_image_instance(obj, window,
							 ERROR_ME_NOT, 1);
				struct Lisp_Image_Instance *p =
				    XIMAGE_INSTANCE(instance);

				switch (XIMAGE_INSTANCE_TYPE(instance)) {
				case IMAGE_TEXT:
				case IMAGE_POINTER:
				case IMAGE_SUBWINDOW:
				case IMAGE_NOTHING:
					*(GTK_RETLOC_BOXED(*arg)) = NULL;
					break;

				case IMAGE_MONO_PIXMAP:
				case IMAGE_COLOR_PIXMAP:
					*(GTK_RETLOC_BOXED(*arg)) =
					    IMAGE_INSTANCE_GTK_PIXMAP(p);
					break;
				}
			} else if (GTK_OBJECTP(obj)
				   && GTK_IS_WIDGET(XGTK_OBJECT(obj)->object)) {
				*(GTK_RETLOC_BOXED(*arg)) =
				    GTK_WIDGET(XGTK_OBJECT(obj))->window;
			} else {
				signal_simple_error
				    ("Don't know how to convert object to GDK_WINDOW",
				     obj);
			}
			break;
		} else if (arg->type == GTK_TYPE_GDK_COLOR) {
			if (COLOR_SPECIFIERP(obj)) {
				/* If it is a specifier, we just convert it to an
				   instance, and let the ifs below handle it.
				 */
				obj =
				    Fspecifier_instance(obj, Qnil, Qnil, Qnil);
			}

			if (COLOR_INSTANCEP(obj)) {
				/* Easiest one */
				*(GTK_RETLOC_BOXED(*arg)) =
				    COLOR_INSTANCE_GTK_COLOR(XCOLOR_INSTANCE
							     (obj));
			} else if (STRINGP(obj)) {
				signal_simple_error
				    ("Please use a color specifier or instance, not a string",
				     obj);
			} else {
				signal_simple_error
				    ("Don't know hot to convert to GdkColor",
				     obj);
			}
		} else if (arg->type == GTK_TYPE_GDK_FONT) {
			if (SYMBOLP(obj)) {
				/* If it is a symbol, we treat that as a face name */
				obj = Ffind_face(obj);
			}

			if (FACEP(obj)) {
				/* If it is a face, we just grab the font specifier, and
				   cascade down until we finally reach a FONT_INSTANCE
				 */
				obj = Fget(obj, Qfont, Qnil);
			}

			if (FONT_SPECIFIERP(obj)) {
				/* If it is a specifier, we just convert it to an
				   instance, and let the ifs below handle it
				 */
				obj =
				    Fspecifier_instance(obj, Qnil, Qnil, Qnil);
			}

			if (FONT_INSTANCEP(obj)) {
				/* Easiest one */
				*(GTK_RETLOC_BOXED(*arg)) =
				    FONT_INSTANCE_GTK_FONT(XFONT_INSTANCE(obj));
			} else if (STRINGP(obj)) {
				signal_simple_error
				    ("Please use a font specifier or instance, not a string",
				     obj);
			} else {
				signal_simple_error
				    ("Don't know hot to convert to GdkColor",
				     obj);
			}
		} else {
			/* Unknown type to convert to boxed */
			stderr_out("Don't know how to convert to boxed!\n");
			*(GTK_RETLOC_BOXED(*arg)) = NULL;
		}
		break;

	case GTK_TYPE_POINTER:
		if (NILP(obj))
			*(GTK_RETLOC_POINTER(*arg)) = NULL;
		else
			*(GTK_RETLOC_POINTER(*arg)) = LISP_TO_VOID(obj);
		break;

		/* structured types */
	case GTK_TYPE_SIGNAL:
	case GTK_TYPE_ARGS:	/* This we can do as a list of values */
	case GTK_TYPE_C_CALLBACK:
	case GTK_TYPE_FOREIGN:
		stderr_out("Do not know how to convert `%s' from lisp!\n",
			   gtk_type_name(arg->type));
		return (-1);

#if 0
		/* #### BILL! */
		/* This is not used, and does not work with union type */
	case GTK_TYPE_CALLBACK:
		{
			GUI_ID id;

			id = new_gui_id();
			obj = Fcons(Qnil, obj);	/* Empty data */
			obj = Fcons(make_int(id), obj);

			gcpro_popup_callbacks(id, obj);

			*(GTK_RETLOC_CALLBACK(*arg)).marshal =
			    __internal_callback_marshal;
			*(GTK_RETLOC_CALLBACK(*arg)).data = (gpointer) obj;
			*(GTK_RETLOC_CALLBACK(*arg)).notify =
			    __internal_callback_destroy;
		}
		break;
#endif

		/* base type of the object system */
	case GTK_TYPE_OBJECT:
		if (NILP(obj))
			*(GTK_RETLOC_OBJECT(*arg)) = NULL;
		else {
			CHECK_GTK_OBJECT(obj);
			if (XGTK_OBJECT(obj)->alive_p)
				*(GTK_RETLOC_OBJECT(*arg)) =
				    XGTK_OBJECT(obj)->object;
			else
				signal_simple_error
				    ("Attempting to pass dead object to GTK function",
				     obj);
		}
		break;

	default:
		if (GTK_FUNDAMENTAL_TYPE(arg->type) == GTK_TYPE_ARRAY) {
			if (NILP(obj))
				*(GTK_RETLOC_POINTER(*arg)) = NULL;
			else {
				xemacs_list_to_array(obj, arg);
			}
		} else if (GTK_FUNDAMENTAL_TYPE(arg->type) == GTK_TYPE_LISTOF) {
			if (NILP(obj))
				*(GTK_RETLOC_POINTER(*arg)) = NULL;
			else {
				xemacs_list_to_gtklist(obj, arg);
			}
		} else {
			stderr_out
			    ("Do not know how to convert `%s' from lisp!\n",
			     gtk_type_name(arg->type));
			abort();
		}
		break;
	}

	return (0);
}

/* This is used in glyphs-gtk.c as well */
static Lisp_Object get_enumeration(GtkType t)
{
	Lisp_Object alist;

	if (NILP(Venumeration_info)) {
		Venumeration_info =
		    call2(intern("make-hashtable"), make_int(100), Qequal);
	}

	alist = Fgethash(make_int(t), Venumeration_info, Qnil);

	if (NILP(alist)) {
		import_gtk_enumeration_internal(t);
		alist = Fgethash(make_int(t), Venumeration_info, Qnil);
	}
	return (alist);
}

guint symbol_to_enum(Lisp_Object obj, GtkType t)
{
	Lisp_Object alist = get_enumeration(t);
	Lisp_Object value = Qnil;

	if (NILP(alist)) {
		signal_simple_error("Unkown enumeration",
				    build_string(gtk_type_name(t)));
	}

	value = Fassq(obj, alist);

	if (NILP(value)) {
		signal_simple_error("Unknown value", obj);
	}

	CHECK_INT(XCDR(value));

	return (XINT(XCDR(value)));
}

static guint lisp_to_flag(Lisp_Object obj, GtkType t)
{
	guint val = 0;

	if (NILP(obj)) {
		/* Do nothing */
	} else if (SYMBOLP(obj)) {
		val = symbol_to_enum(obj, t);
	} else if (LISTP(obj)) {
		while (!NILP(obj)) {
			val |= symbol_to_enum(XCAR(obj), t);
			obj = XCDR(obj);
		}
	} else {
		/* abort ()? */
	}
	return (val);
}

static Lisp_Object flags_to_list(guint value, GtkType t)
{
	Lisp_Object rval = Qnil;
	Lisp_Object alist = get_enumeration(t);

	while (!NILP(alist)) {
		if (value & XINT(XCDR(XCAR(alist)))) {
			rval = Fcons(XCAR(XCAR(alist)), rval);
			value &= ~(XINT(XCDR(XCAR(alist))));
		}
		alist = XCDR(alist);
	}
	return (rval);
}

static Lisp_Object enum_to_symbol(guint value, GtkType t)
{
	Lisp_Object alist = get_enumeration(t);
	Lisp_Object cell = Qnil;

	if (NILP(alist)) {
		signal_simple_error("Unkown enumeration",
				    build_string(gtk_type_name(t)));
	}

	cell = Frassq(make_int(value), alist);

	return (NILP(cell) ? Qnil : XCAR(cell));
}
