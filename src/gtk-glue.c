GtkType GTK_TYPE_ARRAY = 0;
GtkType GTK_TYPE_STRING_ARRAY = 0;
GtkType GTK_TYPE_FLOAT_ARRAY = 0;
GtkType GTK_TYPE_INT_ARRAY = 0;
GtkType GTK_TYPE_LISTOF = 0;
GtkType GTK_TYPE_STRING_LIST = 0;
GtkType GTK_TYPE_OBJECT_LIST = 0;
GtkType GTK_TYPE_GDK_GC = 0;

static GtkType
xemacs_type_register (gchar *name, GtkType parent)
{
  GtkType type_id;
  GtkTypeInfo info;

  info.type_name = name;
  info.object_size = 0;
  info.class_size = 0;
  info.class_init_func = NULL;
  info.object_init_func = NULL;
  info.reserved_1 = NULL;
  info.reserved_2 = NULL;

  type_id = gtk_type_unique (parent, &info);

  return (type_id);
}

static void
xemacs_init_gtk_classes (void)
{
  if (!GTK_TYPE_ARRAY)
    {
      GTK_TYPE_ARRAY = xemacs_type_register ("GtkArrayOf", 0);
      GTK_TYPE_STRING_ARRAY = xemacs_type_register ("GtkArrayOfString", GTK_TYPE_ARRAY);
      GTK_TYPE_FLOAT_ARRAY = xemacs_type_register ("GtkArrayOfFloat", GTK_TYPE_ARRAY);
      GTK_TYPE_INT_ARRAY = xemacs_type_register ("GtkArrayOfInteger", GTK_TYPE_ARRAY);
      GTK_TYPE_LISTOF = xemacs_type_register ("GtkListOf", 0);
      GTK_TYPE_STRING_LIST = xemacs_type_register ("GtkListOfString", GTK_TYPE_LISTOF);
      GTK_TYPE_OBJECT_LIST = xemacs_type_register ("GtkListOfObject", GTK_TYPE_LISTOF);
      GTK_TYPE_GDK_GC = xemacs_type_register ("GdkGC", GTK_TYPE_BOXED);
  }
}

static void
xemacs_list_to_gtklist (Lisp_Object obj, GtkArg *arg)
{
  CHECK_LIST (obj);

  if (arg->type == GTK_TYPE_STRING_LIST)
    {
      Lisp_Object temp = obj;
      GList *strings = NULL;

      while (!NILP (temp))
	{
	  CHECK_STRING (XCAR (temp));
	  temp = XCDR (temp);
	}

      temp = obj;

      while (!NILP (temp))
	{
	  strings = g_list_append (strings, XSTRING_DATA (XCAR (temp)));
	  temp = XCDR (temp);
	}

      GTK_VALUE_POINTER(*arg) = strings;
    }
  else if (arg->type == GTK_TYPE_OBJECT_LIST)
    {
      Lisp_Object temp = obj;
      GList *objects = NULL;

      while (!NILP (temp))
	{
	  CHECK_GTK_OBJECT (XCAR (temp));
	  temp = XCDR (temp);
	}

      temp = obj;

      while (!NILP (temp))
	{
	  objects = g_list_append (objects, XGTK_OBJECT (XCAR (temp))->object);
	  temp = XCDR (temp);
	}

      GTK_VALUE_POINTER(*arg) = objects;
    }
  else
    {
      abort();
    }
}

static void
__make_gtk_object_mapper (gpointer data, gpointer user_data)
{
  Lisp_Object *rv = (Lisp_Object *) user_data;

  *rv = Fcons (build_gtk_object (GTK_OBJECT (data)), *rv);
}

static void
__make_string_mapper (gpointer data, gpointer user_data)
{
  Lisp_Object *rv = (Lisp_Object *) user_data;

  *rv = Fcons (build_string ((char *)data), *rv);
}

static Lisp_Object
xemacs_gtklist_to_list (GtkArg *arg)
{
  Lisp_Object rval = Qnil;

  if (GTK_VALUE_POINTER (*arg))
    {
      if (arg->type == GTK_TYPE_STRING_LIST)
	{
	  g_list_foreach (GTK_VALUE_POINTER (*arg), __make_string_mapper, &rval);
	}
      else if (arg->type == GTK_TYPE_OBJECT_LIST)
	{
	  g_list_foreach (GTK_VALUE_POINTER (*arg), __make_gtk_object_mapper, &rval);
	}
      else
	{
	  abort();
	}
    }
  return (rval);
}

static void
xemacs_list_to_array (Lisp_Object obj, GtkArg *arg)
{
  CHECK_LIST (obj);

#define FROB(ret_type,check_fn,extract_fn) \
  do {								\
    Lisp_Object temp = obj;					\
    int length = 0;						\
    ret_type *array = NULL;					\
								\
    while (!NILP (temp))					\
      {								\
	check_fn (XCAR (temp));					\
	length++;						\
	temp = XCDR (temp);					\
      }								\
								\
    array = xnew_array_and_zero (ret_type, length + 2);		\
    temp = obj;							\
    length = 0;							\
								\
    while (!NILP (temp))					\
      {								\
	array[length++] = extract_fn (XCAR (temp));		\
	temp = XCDR (temp);					\
      }								\
								\
    GTK_VALUE_POINTER(*arg) = array;				\
  } while (0);
  
  if (arg->type == GTK_TYPE_STRING_ARRAY)
    {
      FROB(gchar *, CHECK_STRING, XSTRING_DATA);
    }
  else if (arg->type == GTK_TYPE_FLOAT_ARRAY)
    {
      FROB(gfloat, CHECK_FLOAT, extract_float);
    }
  else if (arg->type == GTK_TYPE_INT_ARRAY)
    {
      FROB(gint, CHECK_INT, XINT);
    }
  else
    {
      abort();
    }
#undef FROB
}

extern GdkGC *gtk_get_gc (struct device *d, Lisp_Object font, Lisp_Object fg, Lisp_Object bg,
			  Lisp_Object bg_pmap, Lisp_Object lwidth);

static GdkGC *
face_to_gc (Lisp_Object face)
{
  Lisp_Object device = Fselected_device (Qnil);

  return (gtk_get_gc (XDEVICE (device),
		      Fspecifier_instance (Fget (face, Qfont, Qnil), device, Qnil, Qnil),
		      Fspecifier_instance (Fget (face, Qforeground, Qnil), device, Qnil, Qnil),
		      Fspecifier_instance (Fget (face, Qbackground, Qnil), device, Qnil, Qnil),
		      Fspecifier_instance (Fget (face, Qbackground_pixmap, Qnil), device, Qnil, Qnil),
		      Qnil));
}

static GtkStyle *
face_to_style (Lisp_Object face)
{
  Lisp_Object device = Fselected_device (Qnil);
  GtkStyle *style = gtk_style_new ();
  int i;

  Lisp_Object font = Fspecifier_instance (Fget (face, Qfont, Qnil), device, Qnil, Qnil);
  Lisp_Object fg = Fspecifier_instance (Fget (face, Qforeground, Qnil), device, Qnil, Qnil);
  Lisp_Object bg = Fspecifier_instance (Fget (face, Qbackground, Qnil), device, Qnil, Qnil);
  Lisp_Object pm = Fspecifier_instance (Fget (face, Qbackground_pixmap, Qnil), device, Qnil, Qnil);

  for (i = 0; i < 5; i++) style->fg[i] = * COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (fg));
  for (i = 0; i < 5; i++) style->bg[i] = * COLOR_INSTANCE_GTK_COLOR (XCOLOR_INSTANCE (bg));

  if (IMAGE_INSTANCEP (pm))
    {
      for (i = 0; i < 5; i++) style->bg_pixmap[i] = XIMAGE_INSTANCE_GTK_PIXMAP (pm);
    }

  style->font = FONT_INSTANCE_GTK_FONT (XFONT_INSTANCE (font));

  return (style);
}

extern int gtk_event_to_emacs_event (struct frame *, GdkEvent *, struct Lisp_Event *);

static Lisp_Object
gdk_event_to_emacs_event(GdkEvent *ev)
{
  Lisp_Object emacs_event = Qnil;

  if (ev)
    {
      emacs_event = Fmake_event (Qnil, Qnil);  
      if (!gtk_event_to_emacs_event (NULL, ev, XEVENT (emacs_event)))
	{
	  /* We need to handle a few more cases than the normal event
	  ** loop does.  Mainly the double/triple click events.
	  */
	  if ((ev->type == GDK_2BUTTON_PRESS) || (ev->type == GDK_3BUTTON_PRESS))
	    {
	      struct Lisp_Event *le = XEVENT (emacs_event);

	      le->event_type = misc_user_event;
	      le->event.misc.button = ev->button.button;
	      le->event.misc.modifiers = 0;
	      le->event.misc.x = ev->button.x;
	      le->event.misc.y = ev->button.y;
	      if (ev->type == GDK_2BUTTON_PRESS)
		le->event.misc.function = intern ("double-click");
	      else
		le->event.misc.function = intern ("triple-click");
	    }
	  else
	    {
	      Fdeallocate_event (emacs_event);
	      emacs_event = Qnil;
	    }
	}
    }
  return (emacs_event);
}
