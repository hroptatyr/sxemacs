#define GTK_VALUE_ARRAY(x) GTK_VALUE_POINTER(x)

#define GTK_VALUE_LIST(x) GTK_VALUE_POINTER(x)


static void
emacs_gtk_marshal_BOOL__OBJECT_INT (ffi_actual_function func, GtkArg *args)
{
  __BOOL_fn rfunc = (__BOOL_fn) func;
  gboolean *return_val;

  return_val = GTK_RETLOC_BOOL (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]));
}

static void
emacs_gtk_marshal_BOOL__OBJECT_OBJECT_OBJECT (ffi_actual_function func, GtkArg *args)
{
  __BOOL_fn rfunc = (__BOOL_fn) func;
  gboolean *return_val;

  return_val = GTK_RETLOC_BOOL (args[3]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_OBJECT (args[2]));
}

static void
emacs_gtk_marshal_BOOL__OBJECT_OBJECT (ffi_actual_function func, GtkArg *args)
{
  __BOOL_fn rfunc = (__BOOL_fn) func;
  gboolean *return_val;

  return_val = GTK_RETLOC_BOOL (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]));
}

static void
emacs_gtk_marshal_BOOL__OBJECT_POINTER (ffi_actual_function func, GtkArg *args)
{
  __BOOL_fn rfunc = (__BOOL_fn) func;
  gboolean *return_val;

  return_val = GTK_RETLOC_BOOL (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]));
}

static void
emacs_gtk_marshal_BOOL__OBJECT_STRING (ffi_actual_function func, GtkArg *args)
{
  __BOOL_fn rfunc = (__BOOL_fn) func;
  gboolean *return_val;

  return_val = GTK_RETLOC_BOOL (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_STRING (args[1]));
}

static void
emacs_gtk_marshal_BOOL__OBJECT (ffi_actual_function func, GtkArg *args)
{
  __BOOL_fn rfunc = (__BOOL_fn) func;
  gboolean *return_val;

  return_val = GTK_RETLOC_BOOL (args[1]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]));
}

static void
emacs_gtk_marshal_BOOL__POINTER_BOOL (ffi_actual_function func, GtkArg *args)
{
  __BOOL_fn rfunc = (__BOOL_fn) func;
  gboolean *return_val;

  return_val = GTK_RETLOC_BOOL (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_POINTER (args[0]), GTK_VALUE_BOOL (args[1]));
}

static void
emacs_gtk_marshal_BOOL__POINTER (ffi_actual_function func, GtkArg *args)
{
  __BOOL_fn rfunc = (__BOOL_fn) func;
  gboolean *return_val;

  return_val = GTK_RETLOC_BOOL (args[1]);
  *return_val = (*rfunc) (GTK_VALUE_POINTER (args[0]));
}

static void
emacs_gtk_marshal_BOOL__NONE (ffi_actual_function func, GtkArg *args)
{
  __BOOL_fn rfunc = (__BOOL_fn) func;
  gboolean *return_val;

  return_val = GTK_RETLOC_BOOL (args[0]);
  *return_val = (*rfunc) ();
}
typedef gfloat (*__FLOAT__OBJECT_FLOAT_fn)(GtkObject *, gfloat);

static void
emacs_gtk_marshal_FLOAT__OBJECT_FLOAT (ffi_actual_function func, GtkArg *args)
{
  __FLOAT__OBJECT_FLOAT_fn rfunc = (__FLOAT__OBJECT_FLOAT_fn) func;
  gfloat *return_val;

  return_val = GTK_RETLOC_FLOAT (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_FLOAT (args[1]));
}

static void
emacs_gtk_marshal_FLOAT__OBJECT (ffi_actual_function func, GtkArg *args)
{
  __FLOAT_fn rfunc = (__FLOAT_fn) func;
  gfloat *return_val;

  return_val = GTK_RETLOC_FLOAT (args[1]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]));
}

static void
emacs_gtk_marshal_INT__BOOL (ffi_actual_function func, GtkArg *args)
{
  __INT_fn rfunc = (__INT_fn) func;
  guint *return_val;

  return_val = GTK_RETLOC_INT (args[1]);
  *return_val = (*rfunc) (GTK_VALUE_BOOL (args[0]));
}

static void
emacs_gtk_marshal_INT__OBJECT_ARRAY (ffi_actual_function func, GtkArg *args)
{
  __INT_fn rfunc = (__INT_fn) func;
  guint *return_val;

  return_val = GTK_RETLOC_INT (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_ARRAY (args[1]));
}

static void
emacs_gtk_marshal_INT__OBJECT_INT_ARRAY (ffi_actual_function func, GtkArg *args)
{
  __INT_fn rfunc = (__INT_fn) func;
  guint *return_val;

  return_val = GTK_RETLOC_INT (args[3]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_ARRAY (args[2]));
}

static void
emacs_gtk_marshal_INT__OBJECT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __INT_fn rfunc = (__INT_fn) func;
  guint *return_val;

  return_val = GTK_RETLOC_INT (args[3]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_INT (args[2]));
}

static void
emacs_gtk_marshal_INT__OBJECT_INT_STRING (ffi_actual_function func, GtkArg *args)
{
  __INT_fn rfunc = (__INT_fn) func;
  guint *return_val;

  return_val = GTK_RETLOC_INT (args[3]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_STRING (args[2]));
}

static void
emacs_gtk_marshal_INT__OBJECT_INT (ffi_actual_function func, GtkArg *args)
{
  __INT_fn rfunc = (__INT_fn) func;
  guint *return_val;

  return_val = GTK_RETLOC_INT (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]));
}

static void
emacs_gtk_marshal_INT__OBJECT_OBJECT (ffi_actual_function func, GtkArg *args)
{
  __INT_fn rfunc = (__INT_fn) func;
  guint *return_val;

  return_val = GTK_RETLOC_INT (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]));
}

static void
emacs_gtk_marshal_INT__OBJECT_POINTER_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __INT_fn rfunc = (__INT_fn) func;
  guint *return_val;

  return_val = GTK_RETLOC_INT (args[4]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_INT (args[3]));
}

static void
emacs_gtk_marshal_INT__OBJECT_POINTER_INT (ffi_actual_function func, GtkArg *args)
{
  __INT_fn rfunc = (__INT_fn) func;
  guint *return_val;

  return_val = GTK_RETLOC_INT (args[3]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_INT (args[2]));
}

static void
emacs_gtk_marshal_INT__OBJECT_POINTER (ffi_actual_function func, GtkArg *args)
{
  __INT_fn rfunc = (__INT_fn) func;
  guint *return_val;

  return_val = GTK_RETLOC_INT (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]));
}

static void
emacs_gtk_marshal_INT__OBJECT_STRING (ffi_actual_function func, GtkArg *args)
{
  __INT_fn rfunc = (__INT_fn) func;
  guint *return_val;

  return_val = GTK_RETLOC_INT (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_STRING (args[1]));
}

static void
emacs_gtk_marshal_INT__OBJECT (ffi_actual_function func, GtkArg *args)
{
  __INT_fn rfunc = (__INT_fn) func;
  guint *return_val;

  return_val = GTK_RETLOC_INT (args[1]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]));
}

static void
emacs_gtk_marshal_INT__POINTER (ffi_actual_function func, GtkArg *args)
{
  __INT_fn rfunc = (__INT_fn) func;
  guint *return_val;

  return_val = GTK_RETLOC_INT (args[1]);
  *return_val = (*rfunc) (GTK_VALUE_POINTER (args[0]));
}

static void
emacs_gtk_marshal_INT__STRING_STRING_INT_ARRAY (ffi_actual_function func, GtkArg *args)
{
  __INT_fn rfunc = (__INT_fn) func;
  guint *return_val;

  return_val = GTK_RETLOC_INT (args[4]);
  *return_val = (*rfunc) (GTK_VALUE_STRING (args[0]), GTK_VALUE_STRING (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_ARRAY (args[3]));
}

static void
emacs_gtk_marshal_INT__STRING (ffi_actual_function func, GtkArg *args)
{
  __INT_fn rfunc = (__INT_fn) func;
  guint *return_val;

  return_val = GTK_RETLOC_INT (args[1]);
  *return_val = (*rfunc) (GTK_VALUE_STRING (args[0]));
}

static void
emacs_gtk_marshal_INT__NONE (ffi_actual_function func, GtkArg *args)
{
  __INT_fn rfunc = (__INT_fn) func;
  guint *return_val;

  return_val = GTK_RETLOC_INT (args[0]);
  *return_val = (*rfunc) ();
}

static void
emacs_gtk_marshal_LIST__OBJECT (ffi_actual_function func, GtkArg *args)
{
  __LIST_fn rfunc = (__LIST_fn) func;
  void * *return_val;

  return_val = GTK_RETLOC_POINTER (args[1]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]));
}

static void
emacs_gtk_marshal_LIST__NONE (ffi_actual_function func, GtkArg *args)
{
  __LIST_fn rfunc = (__LIST_fn) func;
  void * *return_val;

  return_val = GTK_RETLOC_POINTER (args[0]);
  *return_val = (*rfunc) ();
}

static void
emacs_gtk_marshal_NONE__BOOL (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_BOOL (args[0]));
}

static void
emacs_gtk_marshal_NONE__INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_INT (args[0]), GTK_VALUE_INT (args[1]));
}

static void
emacs_gtk_marshal_NONE__INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_INT (args[0]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_BOOL_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_BOOL (args[1]), GTK_VALUE_INT (args[2]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_BOOL (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_BOOL (args[1]));
}
typedef void (*__NONE__OBJECT_FLOAT_FLOAT_FLOAT_BOOL_fn)(GtkObject *, gfloat, gfloat, gfloat, gboolean);

static void
emacs_gtk_marshal_NONE__OBJECT_FLOAT_FLOAT_FLOAT_BOOL (ffi_actual_function func, GtkArg *args)
{
  __NONE__OBJECT_FLOAT_FLOAT_FLOAT_BOOL_fn rfunc = (__NONE__OBJECT_FLOAT_FLOAT_FLOAT_BOOL_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_FLOAT (args[1]), GTK_VALUE_FLOAT (args[2]), GTK_VALUE_FLOAT (args[3]), GTK_VALUE_BOOL (args[4]));
}
typedef void (*__NONE__OBJECT_FLOAT_FLOAT_FLOAT_FLOAT_fn)(GtkObject *, gfloat, gfloat, gfloat, gfloat);

static void
emacs_gtk_marshal_NONE__OBJECT_FLOAT_FLOAT_FLOAT_FLOAT (ffi_actual_function func, GtkArg *args)
{
  __NONE__OBJECT_FLOAT_FLOAT_FLOAT_FLOAT_fn rfunc = (__NONE__OBJECT_FLOAT_FLOAT_FLOAT_FLOAT_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_FLOAT (args[1]), GTK_VALUE_FLOAT (args[2]), GTK_VALUE_FLOAT (args[3]), GTK_VALUE_FLOAT (args[4]));
}
typedef void (*__NONE__OBJECT_FLOAT_FLOAT_FLOAT_fn)(GtkObject *, gfloat, gfloat, gfloat);

static void
emacs_gtk_marshal_NONE__OBJECT_FLOAT_FLOAT_FLOAT (ffi_actual_function func, GtkArg *args)
{
  __NONE__OBJECT_FLOAT_FLOAT_FLOAT_fn rfunc = (__NONE__OBJECT_FLOAT_FLOAT_FLOAT_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_FLOAT (args[1]), GTK_VALUE_FLOAT (args[2]), GTK_VALUE_FLOAT (args[3]));
}
typedef void (*__NONE__OBJECT_FLOAT_FLOAT_fn)(GtkObject *, gfloat, gfloat);

static void
emacs_gtk_marshal_NONE__OBJECT_FLOAT_FLOAT (ffi_actual_function func, GtkArg *args)
{
  __NONE__OBJECT_FLOAT_FLOAT_fn rfunc = (__NONE__OBJECT_FLOAT_FLOAT_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_FLOAT (args[1]), GTK_VALUE_FLOAT (args[2]));
}
typedef void (*__NONE__OBJECT_FLOAT_fn)(GtkObject *, gfloat);

static void
emacs_gtk_marshal_NONE__OBJECT_FLOAT (ffi_actual_function func, GtkArg *args)
{
  __NONE__OBJECT_FLOAT_fn rfunc = (__NONE__OBJECT_FLOAT_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_FLOAT (args[1]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_INT_BOOL (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_BOOL (args[2]));
}
typedef void (*__NONE__OBJECT_INT_FLOAT_BOOL_fn)(GtkObject *, guint, gfloat, gboolean);

static void
emacs_gtk_marshal_NONE__OBJECT_INT_FLOAT_BOOL (ffi_actual_function func, GtkArg *args)
{
  __NONE__OBJECT_INT_FLOAT_BOOL_fn rfunc = (__NONE__OBJECT_INT_FLOAT_BOOL_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_FLOAT (args[2]), GTK_VALUE_BOOL (args[3]));
}
typedef void (*__NONE__OBJECT_INT_FLOAT_fn)(GtkObject *, guint, gfloat);

static void
emacs_gtk_marshal_NONE__OBJECT_INT_FLOAT (ffi_actual_function func, GtkArg *args)
{
  __NONE__OBJECT_INT_FLOAT_fn rfunc = (__NONE__OBJECT_INT_FLOAT_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_FLOAT (args[2]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_INT_INT_ARRAY_ARRAY_ARRAY_ARRAY_ARRAY_ARRAY (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_ARRAY (args[3]), GTK_VALUE_ARRAY (args[4]), GTK_VALUE_ARRAY (args[5]), GTK_VALUE_ARRAY (args[6]), GTK_VALUE_ARRAY (args[7]), GTK_VALUE_ARRAY (args[8]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_INT_INT_ARRAY (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_ARRAY (args[3]));
}
typedef void (*__NONE__OBJECT_INT_INT_FLOAT_FLOAT_fn)(GtkObject *, guint, guint, gfloat, gfloat);

static void
emacs_gtk_marshal_NONE__OBJECT_INT_INT_FLOAT_FLOAT (ffi_actual_function func, GtkArg *args)
{
  __NONE__OBJECT_INT_INT_FLOAT_FLOAT_fn rfunc = (__NONE__OBJECT_INT_INT_FLOAT_FLOAT_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_FLOAT (args[3]), GTK_VALUE_FLOAT (args[4]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_INT_INT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_INT (args[3]), GTK_VALUE_INT (args[4]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_INT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_INT (args[3]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_INT_INT_POINTER_POINTER (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_POINTER (args[3]), GTK_VALUE_POINTER (args[4]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_INT_INT_POINTER (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_POINTER (args[3]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_INT_INT_STRING_INT_POINTER_POINTER (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_STRING (args[3]), GTK_VALUE_INT (args[4]), GTK_VALUE_POINTER (args[5]), GTK_VALUE_POINTER (args[6]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_INT_INT_STRING (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_STRING (args[3]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_INT (args[2]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_INT_OBJECT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_OBJECT (args[2]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_INT_POINTER (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_POINTER (args[2]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_INT_STRING (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_STRING (args[2]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_LIST_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_LIST (args[1]), GTK_VALUE_INT (args[2]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_LIST (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_LIST (args[1]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_BOOL_BOOL_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_BOOL (args[2]), GTK_VALUE_BOOL (args[3]), GTK_VALUE_INT (args[4]), GTK_VALUE_INT (args[5]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_BOOL_BOOL_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_BOOL (args[2]), GTK_VALUE_BOOL (args[3]), GTK_VALUE_INT (args[4]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_BOOL_BOOL (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_BOOL (args[2]), GTK_VALUE_BOOL (args[3]));
}
typedef void (*__NONE__OBJECT_OBJECT_FLOAT_INT_fn)(GtkObject *, GtkObject *, gfloat, guint);

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_FLOAT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE__OBJECT_OBJECT_FLOAT_INT_fn rfunc = (__NONE__OBJECT_OBJECT_FLOAT_INT_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_FLOAT (args[2]), GTK_VALUE_INT (args[3]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_INT_INT_INT_INT_INT_INT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_INT (args[3]), GTK_VALUE_INT (args[4]), GTK_VALUE_INT (args[5]), GTK_VALUE_INT (args[6]), GTK_VALUE_INT (args[7]), GTK_VALUE_INT (args[8]), GTK_VALUE_INT (args[9]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_INT_INT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_INT (args[3]), GTK_VALUE_INT (args[4]), GTK_VALUE_INT (args[5]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_INT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_INT (args[3]), GTK_VALUE_INT (args[4]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_INT (args[3]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_INT (args[2]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_OBJECT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_OBJECT (args[2]), GTK_VALUE_INT (args[3]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_OBJECT_OBJECT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_OBJECT (args[2]), GTK_VALUE_OBJECT (args[3]), GTK_VALUE_INT (args[4]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_OBJECT_OBJECT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_OBJECT (args[2]), GTK_VALUE_OBJECT (args[3]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_OBJECT_POINTER_POINTER_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_OBJECT (args[2]), GTK_VALUE_POINTER (args[3]), GTK_VALUE_POINTER (args[4]), GTK_VALUE_INT (args[5]), GTK_VALUE_INT (args[6]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_OBJECT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_OBJECT (args[2]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_POINTER (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_POINTER (args[2]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_STRING_INT_INT_INT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_STRING (args[2]), GTK_VALUE_INT (args[3]), GTK_VALUE_INT (args[4]), GTK_VALUE_INT (args[5]), GTK_VALUE_INT (args[6]), GTK_VALUE_INT (args[7]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_STRING_STRING_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_STRING (args[2]), GTK_VALUE_STRING (args[3]), GTK_VALUE_INT (args[4]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_STRING_STRING (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_STRING (args[2]), GTK_VALUE_STRING (args[3]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT_STRING (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]), GTK_VALUE_STRING (args[2]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_OBJECT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_POINTER_BOOL (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_BOOL (args[2]));
}
typedef void (*__NONE__OBJECT_POINTER_INT_FLOAT_FLOAT_fn)(GtkObject *, void *, guint, gfloat, gfloat);

static void
emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_FLOAT_FLOAT (ffi_actual_function func, GtkArg *args)
{
  __NONE__OBJECT_POINTER_INT_FLOAT_FLOAT_fn rfunc = (__NONE__OBJECT_POINTER_INT_FLOAT_FLOAT_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_FLOAT (args[3]), GTK_VALUE_FLOAT (args[4]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_INT (args[3]), GTK_VALUE_INT (args[4]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_INT (args[3]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_POINTER_POINTER (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_POINTER (args[3]), GTK_VALUE_POINTER (args[4]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_POINTER (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_POINTER (args[3]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_STRING_INT_POINTER_POINTER (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_STRING (args[3]), GTK_VALUE_INT (args[4]), GTK_VALUE_POINTER (args[5]), GTK_VALUE_POINTER (args[6]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_STRING (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_STRING (args[3]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_POINTER_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_INT (args[2]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_POINTER_POINTER_POINTER_STRING_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_POINTER (args[2]), GTK_VALUE_POINTER (args[3]), GTK_VALUE_STRING (args[4]), GTK_VALUE_INT (args[5]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_POINTER_POINTER_POINTER (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_POINTER (args[2]), GTK_VALUE_POINTER (args[3]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_POINTER_POINTER (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_POINTER (args[2]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_POINTER_STRING_INT_POINTER_POINTER_POINTER_POINTER_BOOL_BOOL (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_STRING (args[2]), GTK_VALUE_INT (args[3]), GTK_VALUE_POINTER (args[4]), GTK_VALUE_POINTER (args[5]), GTK_VALUE_POINTER (args[6]), GTK_VALUE_POINTER (args[7]), GTK_VALUE_BOOL (args[8]), GTK_VALUE_BOOL (args[9]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_POINTER (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_STRING_BOOL (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_STRING (args[1]), GTK_VALUE_BOOL (args[2]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_STRING_POINTER_INT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_STRING (args[1]), GTK_VALUE_POINTER (args[2]), GTK_VALUE_INT (args[3]), GTK_VALUE_INT (args[4]), GTK_VALUE_INT (args[5]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_STRING_POINTER_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_STRING (args[1]), GTK_VALUE_POINTER (args[2]), GTK_VALUE_INT (args[3]), GTK_VALUE_INT (args[4]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_STRING_STRING (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_STRING (args[1]), GTK_VALUE_STRING (args[2]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_STRING (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_STRING (args[1]));
}

static void
emacs_gtk_marshal_NONE__OBJECT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]));
}

static void
emacs_gtk_marshal_NONE__POINTER_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_POINTER (args[0]), GTK_VALUE_INT (args[1]));
}

static void
emacs_gtk_marshal_NONE__POINTER_POINTER_BOOL_INT_INT_INT_INT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_POINTER (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_BOOL (args[2]), GTK_VALUE_INT (args[3]), GTK_VALUE_INT (args[4]), GTK_VALUE_INT (args[5]), GTK_VALUE_INT (args[6]), GTK_VALUE_INT (args[7]), GTK_VALUE_INT (args[8]));
}

static void
emacs_gtk_marshal_NONE__POINTER_POINTER_BOOL_INT_INT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_POINTER (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_BOOL (args[2]), GTK_VALUE_INT (args[3]), GTK_VALUE_INT (args[4]), GTK_VALUE_INT (args[5]), GTK_VALUE_INT (args[6]));
}

static void
emacs_gtk_marshal_NONE__POINTER_POINTER_INT_INT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_POINTER (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_INT (args[3]), GTK_VALUE_INT (args[4]), GTK_VALUE_INT (args[5]));
}

static void
emacs_gtk_marshal_NONE__POINTER_POINTER_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_POINTER (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_INT (args[3]));
}

static void
emacs_gtk_marshal_NONE__POINTER_POINTER_POINTER_INT_INT_STRING_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_POINTER (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_POINTER (args[2]), GTK_VALUE_INT (args[3]), GTK_VALUE_INT (args[4]), GTK_VALUE_STRING (args[5]), GTK_VALUE_INT (args[6]));
}

static void
emacs_gtk_marshal_NONE__POINTER_POINTER_POINTER_INT_INT_STRING (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_POINTER (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_POINTER (args[2]), GTK_VALUE_INT (args[3]), GTK_VALUE_INT (args[4]), GTK_VALUE_STRING (args[5]));
}

static void
emacs_gtk_marshal_NONE__POINTER_POINTER_POINTER_POINTER (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_POINTER (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_POINTER (args[2]), GTK_VALUE_POINTER (args[3]));
}

static void
emacs_gtk_marshal_NONE__POINTER_POINTER (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_POINTER (args[0]), GTK_VALUE_POINTER (args[1]));
}

static void
emacs_gtk_marshal_NONE__POINTER_STRING (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_POINTER (args[0]), GTK_VALUE_STRING (args[1]));
}

static void
emacs_gtk_marshal_NONE__POINTER (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_POINTER (args[0]));
}

static void
emacs_gtk_marshal_NONE__NONE (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) ();
}

static void
emacs_gtk_marshal_OBJECT__BOOL_BOOL_INT (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[3]);
  *return_val = (*rfunc) (GTK_VALUE_BOOL (args[0]), GTK_VALUE_BOOL (args[1]), GTK_VALUE_INT (args[2]));
}

static void
emacs_gtk_marshal_OBJECT__BOOL_INT (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_BOOL (args[0]), GTK_VALUE_INT (args[1]));
}
typedef GtkObject * (*__OBJECT__FLOAT_FLOAT_FLOAT_FLOAT_FLOAT_FLOAT_fn)(gfloat, gfloat, gfloat, gfloat, gfloat, gfloat);

static void
emacs_gtk_marshal_OBJECT__FLOAT_FLOAT_FLOAT_FLOAT_FLOAT_FLOAT (ffi_actual_function func, GtkArg *args)
{
  __OBJECT__FLOAT_FLOAT_FLOAT_FLOAT_FLOAT_FLOAT_fn rfunc = (__OBJECT__FLOAT_FLOAT_FLOAT_FLOAT_FLOAT_FLOAT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[6]);
  *return_val = (*rfunc) (GTK_VALUE_FLOAT (args[0]), GTK_VALUE_FLOAT (args[1]), GTK_VALUE_FLOAT (args[2]), GTK_VALUE_FLOAT (args[3]), GTK_VALUE_FLOAT (args[4]), GTK_VALUE_FLOAT (args[5]));
}
typedef GtkObject * (*__OBJECT__FLOAT_FLOAT_FLOAT_FLOAT_FLOAT_fn)(gfloat, gfloat, gfloat, gfloat, gfloat);

static void
emacs_gtk_marshal_OBJECT__FLOAT_FLOAT_FLOAT_FLOAT_FLOAT (ffi_actual_function func, GtkArg *args)
{
  __OBJECT__FLOAT_FLOAT_FLOAT_FLOAT_FLOAT_fn rfunc = (__OBJECT__FLOAT_FLOAT_FLOAT_FLOAT_FLOAT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[5]);
  *return_val = (*rfunc) (GTK_VALUE_FLOAT (args[0]), GTK_VALUE_FLOAT (args[1]), GTK_VALUE_FLOAT (args[2]), GTK_VALUE_FLOAT (args[3]), GTK_VALUE_FLOAT (args[4]));
}
typedef GtkObject * (*__OBJECT__FLOAT_FLOAT_FLOAT_FLOAT_fn)(gfloat, gfloat, gfloat, gfloat);

static void
emacs_gtk_marshal_OBJECT__FLOAT_FLOAT_FLOAT_FLOAT (ffi_actual_function func, GtkArg *args)
{
  __OBJECT__FLOAT_FLOAT_FLOAT_FLOAT_fn rfunc = (__OBJECT__FLOAT_FLOAT_FLOAT_FLOAT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[4]);
  *return_val = (*rfunc) (GTK_VALUE_FLOAT (args[0]), GTK_VALUE_FLOAT (args[1]), GTK_VALUE_FLOAT (args[2]), GTK_VALUE_FLOAT (args[3]));
}

static void
emacs_gtk_marshal_OBJECT__INT_ARRAY (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_INT (args[0]), GTK_VALUE_ARRAY (args[1]));
}

static void
emacs_gtk_marshal_OBJECT__INT_BOOL_BOOL (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[3]);
  *return_val = (*rfunc) (GTK_VALUE_INT (args[0]), GTK_VALUE_BOOL (args[1]), GTK_VALUE_BOOL (args[2]));
}

static void
emacs_gtk_marshal_OBJECT__INT_INT_ARRAY (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[3]);
  *return_val = (*rfunc) (GTK_VALUE_INT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_ARRAY (args[2]));
}

static void
emacs_gtk_marshal_OBJECT__INT_INT_BOOL (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[3]);
  *return_val = (*rfunc) (GTK_VALUE_INT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_BOOL (args[2]));
}

static void
emacs_gtk_marshal_OBJECT__INT_INT_STRING (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[3]);
  *return_val = (*rfunc) (GTK_VALUE_INT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_STRING (args[2]));
}

static void
emacs_gtk_marshal_OBJECT__INT_INT (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_INT (args[0]), GTK_VALUE_INT (args[1]));
}

static void
emacs_gtk_marshal_OBJECT__INT (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[1]);
  *return_val = (*rfunc) (GTK_VALUE_INT (args[0]));
}
typedef GtkObject * (*__OBJECT__OBJECT_FLOAT_INT_fn)(GtkObject *, gfloat, guint);

static void
emacs_gtk_marshal_OBJECT__OBJECT_FLOAT_INT (ffi_actual_function func, GtkArg *args)
{
  __OBJECT__OBJECT_FLOAT_INT_fn rfunc = (__OBJECT__OBJECT_FLOAT_INT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[3]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_FLOAT (args[1]), GTK_VALUE_INT (args[2]));
}

static void
emacs_gtk_marshal_OBJECT__OBJECT_INT (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]));
}

static void
emacs_gtk_marshal_OBJECT__OBJECT_OBJECT (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_OBJECT (args[1]));
}

static void
emacs_gtk_marshal_OBJECT__OBJECT_STRING_INT_INT_INT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[7]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_STRING (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_INT (args[3]), GTK_VALUE_INT (args[4]), GTK_VALUE_INT (args[5]), GTK_VALUE_INT (args[6]));
}

static void
emacs_gtk_marshal_OBJECT__OBJECT_STRING_INT_INT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[6]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_STRING (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_INT (args[3]), GTK_VALUE_INT (args[4]), GTK_VALUE_INT (args[5]));
}

static void
emacs_gtk_marshal_OBJECT__OBJECT_STRING_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[4]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_STRING (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_INT (args[3]));
}

static void
emacs_gtk_marshal_OBJECT__OBJECT_STRING (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_STRING (args[1]));
}

static void
emacs_gtk_marshal_OBJECT__OBJECT (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[1]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]));
}

static void
emacs_gtk_marshal_OBJECT__POINTER_POINTER (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_POINTER (args[0]), GTK_VALUE_POINTER (args[1]));
}

static void
emacs_gtk_marshal_OBJECT__POINTER_STRING (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_POINTER (args[0]), GTK_VALUE_STRING (args[1]));
}

static void
emacs_gtk_marshal_OBJECT__POINTER (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[1]);
  *return_val = (*rfunc) (GTK_VALUE_POINTER (args[0]));
}
typedef GtkObject * (*__OBJECT__STRING_FLOAT_FLOAT_FLOAT_BOOL_fn)(gchar *, gfloat, gfloat, gfloat, gboolean);

static void
emacs_gtk_marshal_OBJECT__STRING_FLOAT_FLOAT_FLOAT_BOOL (ffi_actual_function func, GtkArg *args)
{
  __OBJECT__STRING_FLOAT_FLOAT_FLOAT_BOOL_fn rfunc = (__OBJECT__STRING_FLOAT_FLOAT_FLOAT_BOOL_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[5]);
  *return_val = (*rfunc) (GTK_VALUE_STRING (args[0]), GTK_VALUE_FLOAT (args[1]), GTK_VALUE_FLOAT (args[2]), GTK_VALUE_FLOAT (args[3]), GTK_VALUE_BOOL (args[4]));
}

static void
emacs_gtk_marshal_OBJECT__STRING_INT_STRING_STRING (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[4]);
  *return_val = (*rfunc) (GTK_VALUE_STRING (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_STRING (args[2]), GTK_VALUE_STRING (args[3]));
}

static void
emacs_gtk_marshal_OBJECT__STRING_OBJECT (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_STRING (args[0]), GTK_VALUE_OBJECT (args[1]));
}

static void
emacs_gtk_marshal_OBJECT__STRING_STRING_STRING_ARRAY_STRING_STRING (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[6]);
  *return_val = (*rfunc) (GTK_VALUE_STRING (args[0]), GTK_VALUE_STRING (args[1]), GTK_VALUE_STRING (args[2]), GTK_VALUE_ARRAY (args[3]), GTK_VALUE_STRING (args[4]), GTK_VALUE_STRING (args[5]));
}

static void
emacs_gtk_marshal_OBJECT__STRING_STRING (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_STRING (args[0]), GTK_VALUE_STRING (args[1]));
}

static void
emacs_gtk_marshal_OBJECT__STRING (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[1]);
  *return_val = (*rfunc) (GTK_VALUE_STRING (args[0]));
}

static void
emacs_gtk_marshal_OBJECT__NONE (ffi_actual_function func, GtkArg *args)
{
  __OBJECT_fn rfunc = (__OBJECT_fn) func;
  GtkObject * *return_val;

  return_val = GTK_RETLOC_OBJECT (args[0]);
  *return_val = (*rfunc) ();
}

static void
emacs_gtk_marshal_POINTER__INT_INT (ffi_actual_function func, GtkArg *args)
{
  __POINTER_fn rfunc = (__POINTER_fn) func;
  void * *return_val;

  return_val = GTK_RETLOC_POINTER (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_INT (args[0]), GTK_VALUE_INT (args[1]));
}

static void
emacs_gtk_marshal_POINTER__INT (ffi_actual_function func, GtkArg *args)
{
  __POINTER_fn rfunc = (__POINTER_fn) func;
  void * *return_val;

  return_val = GTK_RETLOC_POINTER (args[1]);
  *return_val = (*rfunc) (GTK_VALUE_INT (args[0]));
}

static void
emacs_gtk_marshal_POINTER__OBJECT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __POINTER_fn rfunc = (__POINTER_fn) func;
  void * *return_val;

  return_val = GTK_RETLOC_POINTER (args[3]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_INT (args[2]));
}

static void
emacs_gtk_marshal_POINTER__OBJECT_INT (ffi_actual_function func, GtkArg *args)
{
  __POINTER_fn rfunc = (__POINTER_fn) func;
  void * *return_val;

  return_val = GTK_RETLOC_POINTER (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]));
}

static void
emacs_gtk_marshal_POINTER__OBJECT_POINTER_INT (ffi_actual_function func, GtkArg *args)
{
  __POINTER_fn rfunc = (__POINTER_fn) func;
  void * *return_val;

  return_val = GTK_RETLOC_POINTER (args[3]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_INT (args[2]));
}

static void
emacs_gtk_marshal_POINTER__OBJECT_POINTER_POINTER_ARRAY_INT_POINTER_POINTER_POINTER_POINTER_BOOL_BOOL (ffi_actual_function func, GtkArg *args)
{
  __POINTER_fn rfunc = (__POINTER_fn) func;
  void * *return_val;

  return_val = GTK_RETLOC_POINTER (args[11]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_POINTER (args[2]), GTK_VALUE_ARRAY (args[3]), GTK_VALUE_INT (args[4]), GTK_VALUE_POINTER (args[5]), GTK_VALUE_POINTER (args[6]), GTK_VALUE_POINTER (args[7]), GTK_VALUE_POINTER (args[8]), GTK_VALUE_BOOL (args[9]), GTK_VALUE_BOOL (args[10]));
}

static void
emacs_gtk_marshal_POINTER__OBJECT_POINTER (ffi_actual_function func, GtkArg *args)
{
  __POINTER_fn rfunc = (__POINTER_fn) func;
  void * *return_val;

  return_val = GTK_RETLOC_POINTER (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]));
}

static void
emacs_gtk_marshal_POINTER__OBJECT (ffi_actual_function func, GtkArg *args)
{
  __POINTER_fn rfunc = (__POINTER_fn) func;
  void * *return_val;

  return_val = GTK_RETLOC_POINTER (args[1]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]));
}

static void
emacs_gtk_marshal_POINTER__POINTER (ffi_actual_function func, GtkArg *args)
{
  __POINTER_fn rfunc = (__POINTER_fn) func;
  void * *return_val;

  return_val = GTK_RETLOC_POINTER (args[1]);
  *return_val = (*rfunc) (GTK_VALUE_POINTER (args[0]));
}

static void
emacs_gtk_marshal_POINTER__NONE (ffi_actual_function func, GtkArg *args)
{
  __POINTER_fn rfunc = (__POINTER_fn) func;
  void * *return_val;

  return_val = GTK_RETLOC_POINTER (args[0]);
  *return_val = (*rfunc) ();
}

static void
emacs_gtk_marshal_STRING__INT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __STRING_fn rfunc = (__STRING_fn) func;
  gchar * *return_val;

  return_val = GTK_RETLOC_STRING (args[3]);
  *return_val = (*rfunc) (GTK_VALUE_INT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_INT (args[2]));
}

static void
emacs_gtk_marshal_STRING__INT (ffi_actual_function func, GtkArg *args)
{
  __STRING_fn rfunc = (__STRING_fn) func;
  gchar * *return_val;

  return_val = GTK_RETLOC_STRING (args[1]);
  *return_val = (*rfunc) (GTK_VALUE_INT (args[0]));
}

static void
emacs_gtk_marshal_STRING__OBJECT_BOOL (ffi_actual_function func, GtkArg *args)
{
  __STRING_fn rfunc = (__STRING_fn) func;
  gchar * *return_val;

  return_val = GTK_RETLOC_STRING (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_BOOL (args[1]));
}
typedef gchar * (*__STRING__OBJECT_FLOAT_fn)(GtkObject *, gfloat);

static void
emacs_gtk_marshal_STRING__OBJECT_FLOAT (ffi_actual_function func, GtkArg *args)
{
  __STRING__OBJECT_FLOAT_fn rfunc = (__STRING__OBJECT_FLOAT_fn) func;
  gchar * *return_val;

  return_val = GTK_RETLOC_STRING (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_FLOAT (args[1]));
}

static void
emacs_gtk_marshal_STRING__OBJECT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __STRING_fn rfunc = (__STRING_fn) func;
  gchar * *return_val;

  return_val = GTK_RETLOC_STRING (args[3]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_INT (args[2]));
}

static void
emacs_gtk_marshal_STRING__OBJECT_INT (ffi_actual_function func, GtkArg *args)
{
  __STRING_fn rfunc = (__STRING_fn) func;
  gchar * *return_val;

  return_val = GTK_RETLOC_STRING (args[2]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_INT (args[1]));
}

static void
emacs_gtk_marshal_STRING__OBJECT (ffi_actual_function func, GtkArg *args)
{
  __STRING_fn rfunc = (__STRING_fn) func;
  gchar * *return_val;

  return_val = GTK_RETLOC_STRING (args[1]);
  *return_val = (*rfunc) (GTK_VALUE_OBJECT (args[0]));
}

static void
emacs_gtk_marshal_STRING__NONE (ffi_actual_function func, GtkArg *args)
{
  __STRING_fn rfunc = (__STRING_fn) func;
  gchar * *return_val;

  return_val = GTK_RETLOC_STRING (args[0]);
  *return_val = (*rfunc) ();
}

static void
emacs_gtk_marshal_NONE__OBJECT_POINTER_POINTER_INT_INT_INT_INT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_POINTER (args[1]), GTK_VALUE_POINTER (args[2]), GTK_VALUE_INT (args[3]), GTK_VALUE_INT (args[4]), GTK_VALUE_INT (args[5]), GTK_VALUE_INT (args[6]), GTK_VALUE_INT (args[7]), GTK_VALUE_INT (args[8]));
}

static void
emacs_gtk_marshal_NONE__OBJECT_STRING_INT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_OBJECT (args[0]), GTK_VALUE_STRING (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_INT (args[3]), GTK_VALUE_INT (args[4]));
}

static void
emacs_gtk_marshal_NONE__INT_INT_INT_INT (ffi_actual_function func, GtkArg *args)
{
  __NONE_fn rfunc = (__NONE_fn) func;
  (*rfunc) (GTK_VALUE_INT (args[0]), GTK_VALUE_INT (args[1]), GTK_VALUE_INT (args[2]), GTK_VALUE_INT (args[3]));
}


#include "hash.h"
static struct hash_table * marshaller_hashtable;

extern unsigned long string_hash (const char *xv);

static int
our_string_eq (const void *st1, const void *st2)
{
  if (!st1)
    return st2 ? 0 : 1;
  else if (!st2)
    return 0;
  else
    return !strcmp ( (const char *) st1, (const char *) st2);
}

unsigned long
our_string_hash (const void *xv)
{
  unsigned int h = 0;
  unsigned const char *x = (unsigned const char *) xv;

  if (!x) return 0;

  while (*x)
    {
      unsigned int g;
      h = (h << 4) + *x++;
      if ((g = h & 0xf0000000) != 0)
	h = (h ^ (g >> 24)) ^ g;
    }

  return h;
}

static void initialize_marshaller_storage (void)
{
	if (!marshaller_hashtable)
	{
		marshaller_hashtable = make_general_hash_table (100, our_string_hash, our_string_eq);
		puthash ("emacs_gtk_marshal_BOOL__OBJECT_INT", (void *) emacs_gtk_marshal_BOOL__OBJECT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_BOOL__OBJECT_OBJECT_OBJECT", (void *) emacs_gtk_marshal_BOOL__OBJECT_OBJECT_OBJECT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_BOOL__OBJECT_OBJECT", (void *) emacs_gtk_marshal_BOOL__OBJECT_OBJECT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_BOOL__OBJECT_POINTER", (void *) emacs_gtk_marshal_BOOL__OBJECT_POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_BOOL__OBJECT_STRING", (void *) emacs_gtk_marshal_BOOL__OBJECT_STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_BOOL__OBJECT", (void *) emacs_gtk_marshal_BOOL__OBJECT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_BOOL__POINTER_BOOL", (void *) emacs_gtk_marshal_BOOL__POINTER_BOOL, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_BOOL__POINTER", (void *) emacs_gtk_marshal_BOOL__POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_BOOL__NONE", (void *) emacs_gtk_marshal_BOOL__NONE, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_FLOAT__OBJECT_FLOAT", (void *) emacs_gtk_marshal_FLOAT__OBJECT_FLOAT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_FLOAT__OBJECT", (void *) emacs_gtk_marshal_FLOAT__OBJECT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_INT__BOOL", (void *) emacs_gtk_marshal_INT__BOOL, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_INT__OBJECT_ARRAY", (void *) emacs_gtk_marshal_INT__OBJECT_ARRAY, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_INT__OBJECT_INT_ARRAY", (void *) emacs_gtk_marshal_INT__OBJECT_INT_ARRAY, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_INT__OBJECT_INT_INT", (void *) emacs_gtk_marshal_INT__OBJECT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_INT__OBJECT_INT_STRING", (void *) emacs_gtk_marshal_INT__OBJECT_INT_STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_INT__OBJECT_INT", (void *) emacs_gtk_marshal_INT__OBJECT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_INT__OBJECT_OBJECT", (void *) emacs_gtk_marshal_INT__OBJECT_OBJECT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_INT__OBJECT_POINTER_INT_INT", (void *) emacs_gtk_marshal_INT__OBJECT_POINTER_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_INT__OBJECT_POINTER_INT", (void *) emacs_gtk_marshal_INT__OBJECT_POINTER_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_INT__OBJECT_POINTER", (void *) emacs_gtk_marshal_INT__OBJECT_POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_INT__OBJECT_STRING", (void *) emacs_gtk_marshal_INT__OBJECT_STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_INT__OBJECT", (void *) emacs_gtk_marshal_INT__OBJECT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_INT__POINTER", (void *) emacs_gtk_marshal_INT__POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_INT__STRING_STRING_INT_ARRAY", (void *) emacs_gtk_marshal_INT__STRING_STRING_INT_ARRAY, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_INT__STRING", (void *) emacs_gtk_marshal_INT__STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_INT__NONE", (void *) emacs_gtk_marshal_INT__NONE, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_LIST__OBJECT", (void *) emacs_gtk_marshal_LIST__OBJECT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_LIST__NONE", (void *) emacs_gtk_marshal_LIST__NONE, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__BOOL", (void *) emacs_gtk_marshal_NONE__BOOL, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__INT_INT", (void *) emacs_gtk_marshal_NONE__INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__INT", (void *) emacs_gtk_marshal_NONE__INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_BOOL_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_BOOL_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_BOOL", (void *) emacs_gtk_marshal_NONE__OBJECT_BOOL, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_FLOAT_FLOAT_FLOAT_BOOL", (void *) emacs_gtk_marshal_NONE__OBJECT_FLOAT_FLOAT_FLOAT_BOOL, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_FLOAT_FLOAT_FLOAT_FLOAT", (void *) emacs_gtk_marshal_NONE__OBJECT_FLOAT_FLOAT_FLOAT_FLOAT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_FLOAT_FLOAT_FLOAT", (void *) emacs_gtk_marshal_NONE__OBJECT_FLOAT_FLOAT_FLOAT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_FLOAT_FLOAT", (void *) emacs_gtk_marshal_NONE__OBJECT_FLOAT_FLOAT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_FLOAT", (void *) emacs_gtk_marshal_NONE__OBJECT_FLOAT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_INT_BOOL", (void *) emacs_gtk_marshal_NONE__OBJECT_INT_BOOL, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_INT_FLOAT_BOOL", (void *) emacs_gtk_marshal_NONE__OBJECT_INT_FLOAT_BOOL, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_INT_FLOAT", (void *) emacs_gtk_marshal_NONE__OBJECT_INT_FLOAT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_INT_INT_ARRAY_ARRAY_ARRAY_ARRAY_ARRAY_ARRAY", (void *) emacs_gtk_marshal_NONE__OBJECT_INT_INT_ARRAY_ARRAY_ARRAY_ARRAY_ARRAY_ARRAY, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_INT_INT_ARRAY", (void *) emacs_gtk_marshal_NONE__OBJECT_INT_INT_ARRAY, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_INT_INT_FLOAT_FLOAT", (void *) emacs_gtk_marshal_NONE__OBJECT_INT_INT_FLOAT_FLOAT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_INT_INT_INT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_INT_INT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_INT_INT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_INT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_INT_INT_POINTER_POINTER", (void *) emacs_gtk_marshal_NONE__OBJECT_INT_INT_POINTER_POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_INT_INT_POINTER", (void *) emacs_gtk_marshal_NONE__OBJECT_INT_INT_POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_INT_INT_STRING_INT_POINTER_POINTER", (void *) emacs_gtk_marshal_NONE__OBJECT_INT_INT_STRING_INT_POINTER_POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_INT_INT_STRING", (void *) emacs_gtk_marshal_NONE__OBJECT_INT_INT_STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_INT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_INT_OBJECT", (void *) emacs_gtk_marshal_NONE__OBJECT_INT_OBJECT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_INT_POINTER", (void *) emacs_gtk_marshal_NONE__OBJECT_INT_POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_INT_STRING", (void *) emacs_gtk_marshal_NONE__OBJECT_INT_STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_LIST_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_LIST_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_LIST", (void *) emacs_gtk_marshal_NONE__OBJECT_LIST, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_BOOL_BOOL_INT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_BOOL_BOOL_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_BOOL_BOOL_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_BOOL_BOOL_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_BOOL_BOOL", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_BOOL_BOOL, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_FLOAT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_FLOAT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_INT_INT_INT_INT_INT_INT_INT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_INT_INT_INT_INT_INT_INT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_INT_INT_INT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_INT_INT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_INT_INT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_INT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_INT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_OBJECT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_OBJECT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_OBJECT_OBJECT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_OBJECT_OBJECT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_OBJECT_OBJECT", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_OBJECT_OBJECT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_OBJECT_POINTER_POINTER_INT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_OBJECT_POINTER_POINTER_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_OBJECT", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_OBJECT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_POINTER", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_STRING_INT_INT_INT_INT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_STRING_INT_INT_INT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_STRING_STRING_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_STRING_STRING_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_STRING_STRING", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_STRING_STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT_STRING", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT_STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_OBJECT", (void *) emacs_gtk_marshal_NONE__OBJECT_OBJECT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_POINTER_BOOL", (void *) emacs_gtk_marshal_NONE__OBJECT_POINTER_BOOL, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_FLOAT_FLOAT", (void *) emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_FLOAT_FLOAT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_INT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_POINTER_POINTER", (void *) emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_POINTER_POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_POINTER", (void *) emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_STRING_INT_POINTER_POINTER", (void *) emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_STRING_INT_POINTER_POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_STRING", (void *) emacs_gtk_marshal_NONE__OBJECT_POINTER_INT_STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_POINTER_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_POINTER_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_POINTER_POINTER_POINTER_STRING_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_POINTER_POINTER_POINTER_STRING_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_POINTER_POINTER_POINTER", (void *) emacs_gtk_marshal_NONE__OBJECT_POINTER_POINTER_POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_POINTER_POINTER", (void *) emacs_gtk_marshal_NONE__OBJECT_POINTER_POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_POINTER_STRING_INT_POINTER_POINTER_POINTER_POINTER_BOOL_BOOL", (void *) emacs_gtk_marshal_NONE__OBJECT_POINTER_STRING_INT_POINTER_POINTER_POINTER_POINTER_BOOL_BOOL, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_POINTER", (void *) emacs_gtk_marshal_NONE__OBJECT_POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_STRING_BOOL", (void *) emacs_gtk_marshal_NONE__OBJECT_STRING_BOOL, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_STRING_POINTER_INT_INT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_STRING_POINTER_INT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_STRING_POINTER_INT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_STRING_POINTER_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_STRING_STRING", (void *) emacs_gtk_marshal_NONE__OBJECT_STRING_STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_STRING", (void *) emacs_gtk_marshal_NONE__OBJECT_STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT", (void *) emacs_gtk_marshal_NONE__OBJECT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__POINTER_INT", (void *) emacs_gtk_marshal_NONE__POINTER_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__POINTER_POINTER_BOOL_INT_INT_INT_INT_INT_INT", (void *) emacs_gtk_marshal_NONE__POINTER_POINTER_BOOL_INT_INT_INT_INT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__POINTER_POINTER_BOOL_INT_INT_INT_INT", (void *) emacs_gtk_marshal_NONE__POINTER_POINTER_BOOL_INT_INT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__POINTER_POINTER_INT_INT_INT_INT", (void *) emacs_gtk_marshal_NONE__POINTER_POINTER_INT_INT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__POINTER_POINTER_INT_INT", (void *) emacs_gtk_marshal_NONE__POINTER_POINTER_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__POINTER_POINTER_POINTER_INT_INT_STRING_INT", (void *) emacs_gtk_marshal_NONE__POINTER_POINTER_POINTER_INT_INT_STRING_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__POINTER_POINTER_POINTER_INT_INT_STRING", (void *) emacs_gtk_marshal_NONE__POINTER_POINTER_POINTER_INT_INT_STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__POINTER_POINTER_POINTER_POINTER", (void *) emacs_gtk_marshal_NONE__POINTER_POINTER_POINTER_POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__POINTER_POINTER", (void *) emacs_gtk_marshal_NONE__POINTER_POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__POINTER_STRING", (void *) emacs_gtk_marshal_NONE__POINTER_STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__POINTER", (void *) emacs_gtk_marshal_NONE__POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__NONE", (void *) emacs_gtk_marshal_NONE__NONE, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__BOOL_BOOL_INT", (void *) emacs_gtk_marshal_OBJECT__BOOL_BOOL_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__BOOL_INT", (void *) emacs_gtk_marshal_OBJECT__BOOL_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__FLOAT_FLOAT_FLOAT_FLOAT_FLOAT_FLOAT", (void *) emacs_gtk_marshal_OBJECT__FLOAT_FLOAT_FLOAT_FLOAT_FLOAT_FLOAT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__FLOAT_FLOAT_FLOAT_FLOAT_FLOAT", (void *) emacs_gtk_marshal_OBJECT__FLOAT_FLOAT_FLOAT_FLOAT_FLOAT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__FLOAT_FLOAT_FLOAT_FLOAT", (void *) emacs_gtk_marshal_OBJECT__FLOAT_FLOAT_FLOAT_FLOAT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__INT_ARRAY", (void *) emacs_gtk_marshal_OBJECT__INT_ARRAY, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__INT_BOOL_BOOL", (void *) emacs_gtk_marshal_OBJECT__INT_BOOL_BOOL, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__INT_INT_ARRAY", (void *) emacs_gtk_marshal_OBJECT__INT_INT_ARRAY, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__INT_INT_BOOL", (void *) emacs_gtk_marshal_OBJECT__INT_INT_BOOL, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__INT_INT_STRING", (void *) emacs_gtk_marshal_OBJECT__INT_INT_STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__INT_INT", (void *) emacs_gtk_marshal_OBJECT__INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__INT", (void *) emacs_gtk_marshal_OBJECT__INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__OBJECT_FLOAT_INT", (void *) emacs_gtk_marshal_OBJECT__OBJECT_FLOAT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__OBJECT_INT", (void *) emacs_gtk_marshal_OBJECT__OBJECT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__OBJECT_OBJECT", (void *) emacs_gtk_marshal_OBJECT__OBJECT_OBJECT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__OBJECT_STRING_INT_INT_INT_INT_INT", (void *) emacs_gtk_marshal_OBJECT__OBJECT_STRING_INT_INT_INT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__OBJECT_STRING_INT_INT_INT_INT", (void *) emacs_gtk_marshal_OBJECT__OBJECT_STRING_INT_INT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__OBJECT_STRING_INT_INT", (void *) emacs_gtk_marshal_OBJECT__OBJECT_STRING_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__OBJECT_STRING", (void *) emacs_gtk_marshal_OBJECT__OBJECT_STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__OBJECT", (void *) emacs_gtk_marshal_OBJECT__OBJECT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__POINTER_POINTER", (void *) emacs_gtk_marshal_OBJECT__POINTER_POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__POINTER_STRING", (void *) emacs_gtk_marshal_OBJECT__POINTER_STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__POINTER", (void *) emacs_gtk_marshal_OBJECT__POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__STRING_FLOAT_FLOAT_FLOAT_BOOL", (void *) emacs_gtk_marshal_OBJECT__STRING_FLOAT_FLOAT_FLOAT_BOOL, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__STRING_INT_STRING_STRING", (void *) emacs_gtk_marshal_OBJECT__STRING_INT_STRING_STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__STRING_OBJECT", (void *) emacs_gtk_marshal_OBJECT__STRING_OBJECT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__STRING_STRING_STRING_ARRAY_STRING_STRING", (void *) emacs_gtk_marshal_OBJECT__STRING_STRING_STRING_ARRAY_STRING_STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__STRING_STRING", (void *) emacs_gtk_marshal_OBJECT__STRING_STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__STRING", (void *) emacs_gtk_marshal_OBJECT__STRING, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_OBJECT__NONE", (void *) emacs_gtk_marshal_OBJECT__NONE, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_POINTER__INT_INT", (void *) emacs_gtk_marshal_POINTER__INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_POINTER__INT", (void *) emacs_gtk_marshal_POINTER__INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_POINTER__OBJECT_INT_INT", (void *) emacs_gtk_marshal_POINTER__OBJECT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_POINTER__OBJECT_INT", (void *) emacs_gtk_marshal_POINTER__OBJECT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_POINTER__OBJECT_POINTER_INT", (void *) emacs_gtk_marshal_POINTER__OBJECT_POINTER_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_POINTER__OBJECT_POINTER_POINTER_ARRAY_INT_POINTER_POINTER_POINTER_POINTER_BOOL_BOOL", (void *) emacs_gtk_marshal_POINTER__OBJECT_POINTER_POINTER_ARRAY_INT_POINTER_POINTER_POINTER_POINTER_BOOL_BOOL, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_POINTER__OBJECT_POINTER", (void *) emacs_gtk_marshal_POINTER__OBJECT_POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_POINTER__OBJECT", (void *) emacs_gtk_marshal_POINTER__OBJECT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_POINTER__POINTER", (void *) emacs_gtk_marshal_POINTER__POINTER, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_POINTER__NONE", (void *) emacs_gtk_marshal_POINTER__NONE, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_STRING__INT_INT_INT", (void *) emacs_gtk_marshal_STRING__INT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_STRING__INT", (void *) emacs_gtk_marshal_STRING__INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_STRING__OBJECT_BOOL", (void *) emacs_gtk_marshal_STRING__OBJECT_BOOL, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_STRING__OBJECT_FLOAT", (void *) emacs_gtk_marshal_STRING__OBJECT_FLOAT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_STRING__OBJECT_INT_INT", (void *) emacs_gtk_marshal_STRING__OBJECT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_STRING__OBJECT_INT", (void *) emacs_gtk_marshal_STRING__OBJECT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_STRING__OBJECT", (void *) emacs_gtk_marshal_STRING__OBJECT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_STRING__NONE", (void *) emacs_gtk_marshal_STRING__NONE, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_POINTER_POINTER_INT_INT_INT_INT_INT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_POINTER_POINTER_INT_INT_INT_INT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__OBJECT_STRING_INT_INT_INT", (void *) emacs_gtk_marshal_NONE__OBJECT_STRING_INT_INT_INT, marshaller_hashtable);
		puthash ("emacs_gtk_marshal_NONE__INT_INT_INT_INT", (void *) emacs_gtk_marshal_NONE__INT_INT_INT_INT, marshaller_hashtable);
	};
}

static void *find_marshaller (const char *func_name)
{
	void *fn = NULL;
	initialize_marshaller_storage ();

	if (gethash (func_name, marshaller_hashtable, (const void **)&fn))
	{
		return (fn);
	}

	return (NULL);
}
