/* Generic specifier list implementation
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995 Ben Wing

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

/* Synched up with: Not in FSF. */

#ifndef INCLUDED_specifier_h_
#define INCLUDED_specifier_h_

/*
  MAGIC SPECIFIERS
  ================

  Magic specifiers are used to provide fallback values for window
  system provided specifications, reflecting user preferences on the
  window system, such as default fonts, colors, scrollbar thickness
  etc.

  A magic specifier consists of two specifier objects. The first one
  behaves like a normal specifier in all senses. The second one, a
  ghost specifier, is a fallback value for the first one, and contains
  values provided by window system, resources etc. which reflect
  default settings for values being specified.

  A magic specifier has an "ultimate" fallback value, as any usual
  specifier does. This value, an inst-list, is stored in the fallback
  slot of the ghost specifier object.

  Ghost specifiers have the following properties:
  - Have back pointers to their parent specifiers.
  - Do not have instance data. Instead, they share parent's instance
    data.
  - Have the same methods structure pointer.
  - Share parent's caching scheme.
  - Store fallback value instead of their parents.

  Ghost specifiers normally are not modifiable at the lisp level, and
  only used to supply fallback instance values. They are accessible
  via (specifier-fallback), but are read-only.  Although, under
  certain rare conditions, modification of ghost objects is allowed.
  This behavior is controlled by the global variable
  Vunlock_ghost_specifiers. It is not exposed to lisp, and is set
  during calls to lisp functions which initialize global, device and
  frame defaults, such as
  init-{global,frame,device}-{faces,toolbars,etc}.

  Thus, values supplied by resources or other means of a window system
  stored in externally unmodifiable ghost objects. Regular lisp code
  may thus freely modify the normal part of a magic specifier, and
  removing a specification for a particular domain causes the
  specification to consider ghost-provided fallback values, or its own
  fallback value.

  Rules of conduct for magic specifiers
  -------------------------------------
  1. recompute_*() functions always operate on the whole specifier
     when passed only a ghost object, by substituting it with their
     parent bodily object.
  2. All specifier methods, except for instantiate method, are passed
     the bodily object of the magic specifier. Instantiate method is
     passed the specifier being instantiated.
  3. Only bodily objects are passed to set_specifier_caching function,
     and only these may be cached.
  4. All specifiers are added to Vall_specifiers list, both bodily and
     ghost. The pair of objects is always removed from the list at the
     same time.
*/

extern const struct struct_description specifier_methods_description;

struct specifier_methods
{
  const char *name;
  Lisp_Object predicate_symbol;

  /* Implementation specific methods: */

  /* Create method: Initialize specifier data. Optional. */
  void (*create_method) (Lisp_Object specifier);

  /* Mark method: Mark any lisp object within specifier data
     structure. Not required if no specifier data are Lisp_Objects. */
  void (*mark_method) (Lisp_Object specifier);

  /* Equal method: Compare two specifiers. This is called after
     ensuring that the two specifiers are of the same type, and have
     the same specs.  Quit is inhibited during the call so it is safe
     to call internal_equal().

     If this function is not present, specifiers considered equal when
     the above conditions are met, i.e. as if the method returned
     non-zero. */
  int (*equal_method) (Lisp_Object sp1, Lisp_Object sp2, int depth);

  /* Hash method: Hash specifier instance data. This has to hash only
    data structure of the specifier, as specs are hashed by the core
    code.

     If this function is not present, hashing behaves as if it
     returned zero. */
  unsigned long (*hash_method) (Lisp_Object specifier, int depth);

  /* Validate method: Given an instantiator, verify that it's
     valid for this specifier type.  If not, signal an error.

     If this function is not present, all instantiators are considered
     valid. */
  void (*validate_method) (Lisp_Object instantiator);


  /* Copy method: Given an instantiator, copy the bits that we need to
     for this specifier type.

     If this function is not present, then Fcopy_tree is used. */
  Lisp_Object (*copy_instantiator_method) (Lisp_Object instantiator);

  /* Validate-matchspec method: Given a matchspec, verify that it's
     valid for this specifier type.  If not, signal an error.

     If this function is not present, *no* matchspecs are considered
     valid.  Note that this differs from validate_method(). */
  void (*validate_matchspec_method) (Lisp_Object matchspec);

  /* Instantiate method: Return SPECIFIER instance in DOMAIN,
     specified by INSTANTIATOR.  MATCHSPEC specifies an additional
     constraints on the instance value (see the docstring for
     Fspecifier_matching_instance function). MATCHSPEC is passed
     Qunbound when no matching constraints are imposed. The method is
     called via call_with_suspended_errors(), so allowed to eval
     safely.

     DEPTH is a lisp integer denoting current depth of instantiation
     calls. This parameter should be passed as the initial depth value
     to functions which also instantiate specifiers (of which I can
     name specifier_instance) to avoid creating "external"
     specification loops.

     This method must presume that both INSTANTIATOR and MATCHSPEC are
     already validated by the corresponding validate_* methods, and
     may abort if they are invalid.

     Return value is an instance, which is returned immediately to the
     caller, or Qunbound to continue instantiation lookup chain.

     If this function is not present, INSTANTIATOR is used as the
     specifier instance.  This is the usual case for "simple"
     specifiers, like integer and boolean. */
  Lisp_Object (*instantiate_method) (Lisp_Object specifier,
				     Lisp_Object matchspec,
				     Lisp_Object domain,
				     Lisp_Object instantiator,
				     Lisp_Object depth);

  /* Going-to-add method: Called when an instantiator is about
     to be added to a specifier.  This function can specify
     that different instantiators should be used instead by
     returning an inst-list (possibly containing zero elements).
     If the instantiator is fine as-is, return Qt.  The
     instantiator has been copied with copy-tree, so feel
     free to reuse parts of it to create a new instantiator.
     The tag-set, however, is not copied and is not canonicalized
     (that will be done on the result of this function). */
  Lisp_Object (*going_to_add_method) (Lisp_Object specifier,
				      Lisp_Object locale,
				      Lisp_Object tag_set,
				      Lisp_Object instantiator);

  /* After-change method: Called when the SPECIFIER has just been
     changed in LOCALE.  The method is called upon:
     * Removing and adding specs to/from the specifier;
     * Changing the specifier fallback.

     #### The method may have called more than once per each specifier
     change.

     #### Do not still know if this can safely eval. */
  void (*after_change_method) (Lisp_Object specifier,
			       Lisp_Object locale);

  const struct lrecord_description *extra_description;
  int extra_data_size;
};

struct Lisp_Specifier
{
  struct lcrecord_header header;
  struct specifier_methods *methods;

  /* we keep a chained list of all current specifiers, for GC cleanup
     purposes.  Do NOT mark through this, or specifiers will never
     be GC'd. */
  Lisp_Object next_specifier;

  /* This is a straight list of instantiators. */
  Lisp_Object global_specs;

  /* These are all assoc lists where the key is the type of object the
     list represents (buffer, window, etc.) and the associated list is
     the actual list of instantiators. */
  Lisp_Object device_specs;
  Lisp_Object frame_specs;
  /* window_specs is actually a key-assoc weak list.  See specifier.c
     for an explanation of why (it boils down to the fact that
     dead windows can become live again through window configurations).
     */
  Lisp_Object window_specs;
  Lisp_Object buffer_specs;

  struct specifier_caching *caching;

  /* This can be either nil, for a plain, non-magic specifier object,
     t for the normal part of the magic specifier, or #<specifier> for
     the ghost part of the magic specifier, a pointer to its parent
     object */
  Lisp_Object magic_parent;

  /* Fallback value. For magic specifiers, it is a pointer to the ghost. */
  Lisp_Object fallback;

  /* type-specific extra data attached to a specifier */
  max_align_t data[1];
};
typedef struct Lisp_Specifier Lisp_Specifier;

DECLARE_LRECORD (specifier, Lisp_Specifier);
#define XSPECIFIER(x) XRECORD (x, specifier, Lisp_Specifier)
#define XSETSPECIFIER(x, p) XSETRECORD (x, p, specifier)
#define SPECIFIERP(x) RECORDP (x, specifier)
#define CHECK_SPECIFIER(x) CHECK_RECORD (x, specifier)
#define CONCHECK_SPECIFIER(x) CONCHECK_RECORD (x, specifier)

/***** Calling a specifier method *****/

#define RAW_SPECMETH(sp, m) ((sp)->methods->m##_method)
#define HAS_SPECMETH_P(sp, m) (!!RAW_SPECMETH (sp, m))
#define SPECMETH(sp, m, args) (((sp)->methods->m##_method) args)

/* Call a void-returning specifier method, if it exists.  */
#define MAYBE_SPECMETH(sp, m, args) do {	\
  Lisp_Specifier *maybe_specmeth_sp = (sp);	\
  if (HAS_SPECMETH_P (maybe_specmeth_sp, m))	\
    SPECMETH (maybe_specmeth_sp, m, args);	\
} while (0)

/***** Defining new specifier types *****/

#define specifier_data_offset offsetof (Lisp_Specifier, data)
extern const struct lrecord_description specifier_empty_extra_description[];

#ifdef ERROR_CHECK_TYPECHECK
#define DECLARE_SPECIFIER_TYPE(type)					\
extern struct specifier_methods * type##_specifier_methods;		\
INLINE_HEADER struct type##_specifier *					\
error_check_##type##_specifier_data (Lisp_Specifier *sp);		\
INLINE_HEADER struct type##_specifier *					\
error_check_##type##_specifier_data (Lisp_Specifier *sp)		\
{									\
  if (SPECIFIERP (sp->magic_parent))					\
    {									\
      assert (SPECIFIER_TYPE_P (sp, type));				\
      sp = XSPECIFIER (sp->magic_parent);				\
    }									\
  else									\
    assert (NILP (sp->magic_parent) || EQ (sp->magic_parent, Qt));	\
  assert (SPECIFIER_TYPE_P (sp, type));					\
  return (struct type##_specifier *) sp->data;				\
}									\
INLINE_HEADER Lisp_Specifier *						\
error_check_##type##_specifier_type (Lisp_Object obj);			\
INLINE_HEADER Lisp_Specifier *						\
error_check_##type##_specifier_type (Lisp_Object obj)			\
{									\
  Lisp_Specifier *sp = XSPECIFIER (obj);				\
  assert (SPECIFIER_TYPE_P (sp, type));					\
  return sp;								\
}									\
DECLARE_NOTHING
#else
#define DECLARE_SPECIFIER_TYPE(type)					\
extern struct specifier_methods * type##_specifier_methods
#endif /* ERROR_CHECK_TYPECHECK */

#define DEFINE_SPECIFIER_TYPE(type)					\
struct specifier_methods * type##_specifier_methods

#define INITIALIZE_SPECIFIER_TYPE(type, obj_name, pred_sym) do {		\
  type##_specifier_methods = xnew_and_zero (struct specifier_methods);		\
  type##_specifier_methods->name = obj_name;					\
  type##_specifier_methods->extra_description =					\
    specifier_empty_extra_description;						\
  defsymbol_nodump (&type##_specifier_methods->predicate_symbol, pred_sym);	\
  add_entry_to_specifier_type_list (Q##type, type##_specifier_methods);		\
  dump_add_root_struct_ptr (&type##_specifier_methods,				\
			    &specifier_methods_description);			\
} while (0)

#define REINITIALIZE_SPECIFIER_TYPE(type) do {				\
  staticpro_nodump (&type##_specifier_methods->predicate_symbol);	\
} while (0)

#define INITIALIZE_SPECIFIER_TYPE_WITH_DATA(type, obj_name, pred_sym)	\
do {									\
  INITIALIZE_SPECIFIER_TYPE (type, obj_name, pred_sym);			\
  type##_specifier_methods->extra_data_size =				\
    sizeof (struct type##_specifier);					\
  type##_specifier_methods->extra_description = 			\
    type##_specifier_description;					\
} while (0)

/* Declare that specifier-type TYPE has method METH; used in
   initialization routines */
#define SPECIFIER_HAS_METHOD(type, meth) \
  (type##_specifier_methods->meth##_method = type##_##meth)

/***** Macros for accessing specifier types *****/

#define SPECIFIER_TYPE_P(sp, type) \
  ((sp)->methods == type##_specifier_methods)

/* Any of the two of the magic spec */
#define MAGIC_SPECIFIER_P(sp) (!NILP((sp)->magic_parent))
/* Normal part of the magic specifier */
#define BODILY_SPECIFIER_P(sp) EQ ((sp)->magic_parent, Qt)
/* Ghost part of the magic specifier */
#define GHOST_SPECIFIER_P(sp) SPECIFIERP((sp)->magic_parent)

#define GHOST_SPECIFIER(sp) XSPECIFIER ((sp)->fallback)

#ifdef ERROR_CHECK_TYPECHECK
# define SPECIFIER_TYPE_DATA(sp, type) \
   error_check_##type##_specifier_data (sp)
#else
# define SPECIFIER_TYPE_DATA(sp, type)		\
  ((struct type##_specifier *)			\
    (GHOST_SPECIFIER_P(sp)			\
     ? XSPECIFIER((sp)->magic_parent)->data	\
     : (sp)->data))
#endif

#ifdef ERROR_CHECK_TYPECHECK
# define XSPECIFIER_TYPE(x, type)	\
   error_check_##type##_specifier_type (x)
# define XSETSPECIFIER_TYPE(x, p, type)	do		\
{							\
  XSETSPECIFIER (x, p);					\
  assert (SPECIFIER_TYPEP (XSPECIFIER(x), type));	\
} while (0)
#else
# define XSPECIFIER_TYPE(x, type) XSPECIFIER (x)
# define XSETSPECIFIER_TYPE(x, p, type) XSETSPECIFIER (x, p)
#endif /* ERROR_CHECK_TYPE_CHECK */

#define SPECIFIER_TYPEP(x, type)			\
  (SPECIFIERP (x) && SPECIFIER_TYPE_P (XSPECIFIER (x), type))
#define CHECK_SPECIFIER_TYPE(x, type) do {		\
  CHECK_SPECIFIER (x);					\
  if (!SPECIFIER_TYPE_P (XSPECIFIER (x), type))		\
    dead_wrong_type_argument				\
      (type##_specifier_methods->predicate_symbol, x);	\
} while (0)
#define CONCHECK_SPECIFIER_TYPE(x, type) do {		\
  CONCHECK_SPECIFIER (x);				\
  if (!(SPECIFIER_TYPEP (x, type)))			\
    x = wrong_type_argument				\
      (type##_specifier_methods->predicate_symbol, x);	\
} while (0)

/***** Miscellaneous structures *****/

enum spec_locale_type
{
  LOCALE_GLOBAL,
  LOCALE_DEVICE,
  LOCALE_FRAME,
  LOCALE_WINDOW,
  LOCALE_BUFFER
};

enum spec_add_meth
{
  SPEC_PREPEND,
  SPEC_APPEND,
  SPEC_REMOVE_TAG_SET_PREPEND,
  SPEC_REMOVE_TAG_SET_APPEND,
  SPEC_REMOVE_LOCALE,
  SPEC_REMOVE_LOCALE_TYPE,
  SPEC_REMOVE_ALL
};

struct specifier_caching
{
  int offset_into_struct_window;
  void (*value_changed_in_window) (Lisp_Object specifier, struct window *w,
				   Lisp_Object oldval);
  int offset_into_struct_frame;
  void (*value_changed_in_frame) (Lisp_Object specifier, struct frame *f,
				  Lisp_Object oldval);
  int always_recompute;
};

/* #### get image instances out of domains! */

/* #### I think the following should abort() rather than return nil
   when an invalid domain is given; much more likely we'll catch design
   errors early. --ben */

/* This turns out to be used heavily so we make it a macro to make it
   inline.  Also, the majority of the time the object will turn out to
   be a window so we move it from being checked last to being checked
   first. */
#define DOMAIN_DEVICE(obj)					\
   (WINDOWP (obj) ? WINDOW_DEVICE (XWINDOW (obj))		\
  : (FRAMEP  (obj) ? FRAME_DEVICE (XFRAME (obj))		\
  : (DEVICEP (obj) ? obj					\
  : (IMAGE_INSTANCEP (obj) ? image_instance_device (obj)	\
  : Qnil))))

#define DOMAIN_FRAME(obj)				\
   (WINDOWP (obj) ? WINDOW_FRAME (XWINDOW (obj))	\
  : (FRAMEP  (obj) ? obj				\
  : (IMAGE_INSTANCEP (obj) ? image_instance_frame (obj)	\
  : Qnil)))

#define DOMAIN_WINDOW(obj)					\
   (WINDOWP (obj) ? obj						\
  : (IMAGE_INSTANCEP (obj) ? image_instance_window (obj)	\
  : Qnil))

#define DOMAIN_LIVE_P(obj)					\
   (WINDOWP (obj) ? WINDOW_LIVE_P (XWINDOW (obj))		\
  : (FRAMEP  (obj) ? FRAME_LIVE_P (XFRAME (obj))		\
  : (DEVICEP (obj) ? DEVICE_LIVE_P (XDEVICE (obj))		\
  : (IMAGE_INSTANCEP (obj) ? image_instance_live_p (obj)	\
  : 0))))

#define DOMAIN_XDEVICE(obj)			\
  (XDEVICE (DOMAIN_DEVICE (obj)))
#define DOMAIN_XFRAME(obj)			\
  (XFRAME (DOMAIN_FRAME (obj)))
#define DOMAIN_XWINDOW(obj)			\
  (XWINDOW (DOMAIN_WINDOW (obj)))

EXFUN (Fcopy_specifier, 6);
EXFUN (Fmake_specifier, 1);
EXFUN (Fset_specifier_dirty_flag, 1);
EXFUN (Fspecifier_instance, 4);
EXFUN (Fvalid_specifier_locale_p, 1);

extern Lisp_Object Qfallback, Qnatnum;

Lisp_Object make_magic_specifier (Lisp_Object type);
Lisp_Object decode_locale_list (Lisp_Object locale);
extern enum spec_add_meth
decode_how_to_add_specification (Lisp_Object how_to_add);
Lisp_Object decode_specifier_tag_set (Lisp_Object tag_set);
Lisp_Object decode_domain (Lisp_Object domain);

void add_entry_to_specifier_type_list (Lisp_Object symbol,
				       struct specifier_methods *meths);
void set_specifier_caching (Lisp_Object specifier,
			    int struct_window_offset,
			    void (*value_changed_in_window)
			    (Lisp_Object specifier, struct window *w,
			     Lisp_Object oldval),
			    int struct_frame_offset,
			    void (*value_changed_in_frame)
			    (Lisp_Object specifier, struct frame *f,
			     Lisp_Object oldval),
			    int always_recompute);
void set_specifier_fallback (Lisp_Object specifier,
			     Lisp_Object fallback);
void recompute_all_cached_specifiers_in_window (struct window *w);
void recompute_all_cached_specifiers_in_frame (struct frame *f);

/* Counterparts of Fadd_spec_to_specifier and Fremove_specifier, which
   operate directly on ghost objects given a magic specifier. */
void add_spec_to_ghost_specifier (Lisp_Object specifier, Lisp_Object instantiator,
				  Lisp_Object locale, Lisp_Object tag_set,
				  Lisp_Object how_to_add);
void remove_ghost_specifier (Lisp_Object specifier, Lisp_Object locale,
			     Lisp_Object tag_set, Lisp_Object exact_p);

int unlock_ghost_specifiers_protected (void);

void cleanup_specifiers (void);
void prune_specifiers (void);
void setup_device_initial_specifier_tags (struct device *d);
void kill_specifier_buffer_locals (Lisp_Object buffer);

DECLARE_SPECIFIER_TYPE (generic);
#define XGENERIC_SPECIFIER(x) XSPECIFIER_TYPE (x, generic)
#define XSETGENERIC_SPECIFIER(x, p) XSETSPECIFIER_TYPE (x, p, generic)
#define GENERIC_SPECIFIERP(x) SPECIFIER_TYPEP (x, generic)
#define CHECK_GENERIC_SPECIFIER(x) CHECK_SPECIFIER_TYPE (x, generic)
#define CONCHECK_GENERIC_SPECIFIER(x) CONCHECK_SPECIFIER_TYPE (x, generic)

DECLARE_SPECIFIER_TYPE (integer);
#define XINTEGER_SPECIFIER(x) XSPECIFIER_TYPE (x, integer)
#define XSETINTEGER_SPECIFIER(x, p) XSETSPECIFIER_TYPE (x, p, integer)
#define INTEGER_SPECIFIERP(x) SPECIFIER_TYPEP (x, integer)
#define CHECK_INTEGER_SPECIFIER(x) CHECK_SPECIFIER_TYPE (x, integer)
#define CONCHECK_INTEGER_SPECIFIER(x) CONCHECK_SPECIFIER_TYPE (x, integer)

DECLARE_SPECIFIER_TYPE (natnum);
#define XNATNUM_SPECIFIER(x) XSPECIFIER_TYPE (x, natnum)
#define XSETNATNUM_SPECIFIER(x, p) XSETSPECIFIER_TYPE (x, p, natnum)
#define NATNUM_SPECIFIERP(x) SPECIFIER_TYPEP (x, natnum)
#define CHECK_NATNUM_SPECIFIER(x) CHECK_SPECIFIER_TYPE (x, natnum)
#define CONCHECK_NATNUM_SPECIFIER(x) CONCHECK_SPECIFIER_TYPE (x, natnum)

DECLARE_SPECIFIER_TYPE (boolean);
#define XBOOLEAN_SPECIFIER(x) XSPECIFIER_TYPE (x, boolean)
#define XSETBOOLEAN_SPECIFIER(x, p) XSETSPECIFIER_TYPE (x, p, boolean)
#define BOOLEAN_SPECIFIERP(x) SPECIFIER_TYPEP (x, boolean)
#define CHECK_BOOLEAN_SPECIFIER(x) CHECK_SPECIFIER_TYPE (x, boolean)
#define CONCHECK_BOOLEAN_SPECIFIER(x) CONCHECK_SPECIFIER_TYPE (x, boolean)

DECLARE_SPECIFIER_TYPE (display_table);
#define XDISPLAYTABLE_SPECIFIER(x) XSPECIFIER_TYPE (x, display_table)
#define XSETDISPLAYTABLE_SPECIFIER(x, p) XSETSPECIFIER_TYPE (x, p, display_table)
#define DISPLAYTABLE_SPECIFIERP(x) SPECIFIER_TYPEP (x, display_table)
#define CHECK_DISPLAYTABLE_SPECIFIER(x) CHECK_SPECIFIER_TYPE (x, display_table)
#define CONCHECK_DISPLAYTABLE_SPECIFIER(x) CONCHECK_SPECIFIER_TYPE (x, display_table)

#endif /* INCLUDED_specifier_h_ */
