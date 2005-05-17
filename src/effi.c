/*
 * effi.c --- Foreign Function Interface for SXEmacs.
 *
 * Copyright (C) 2004 by XWEM Org.
 *
 * This file is part of SXEmacs.
 * 
 * SXEmacs is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 * 
 * SXEmacs is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with SXEmacs; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <config.h>
#include "lisp.h"

#include <dlfcn.h>

#include "effi.h"

#ifdef HAVE_LIBFFI
#undef ALIGN
#include <ffi.h>
#endif  /* HAVE_LIBFFI */

/*
 * Built-in types:
 *   byte, ubyte, char, uchar,
 *   short, ushort, int, uint,
 *   long, ulong,
 *   float, double,
 *   void, pointer, c-string
 *
 * Function type:
 *
 *   (function RET-TYPE IN-TYPE .. IN-TYPE)
 *
 * Array types:
 *
 *   (array TYPE SIZE)
 *
 * Structures and unions types:
 * 
 *   (struct|union NAME
 *     (SLOT-NAME TYPE)
 *     (SLOT-NAME TYPE)
 *     ...
 *     (SLOT-NAME TYPE))
 *
 * Pointers:
 *
 *   (pointer TYPE)
 */

/* Foreign types */
Lisp_Object Q_byte, Q_unsigned_byte;
Lisp_Object Q_char, Q_unsigned_char;
Lisp_Object Q_short, Q_unsigned_short;
Lisp_Object Q_int, Q_unsigned_int;
Lisp_Object Q_long, Q_unsigned_long;
Lisp_Object Q_float, Q_double;
Lisp_Object Q_void;
Lisp_Object Q_array, Q_pointer;
Lisp_Object Q_union, Q_struct;
Lisp_Object Q_function;
Lisp_Object Q_c_string;

Lisp_Object Qffiobjectp;

/* Alist with elements in form (NAME . TYPE) */
Lisp_Object Vffi_loaded_libraries;
Lisp_Object Vffi_named_types;

Lisp_Object Vffi_type_checker;

static Lisp_Object
mark_ffiobject(Lisp_Object obj)
{
	Lisp_EffiObject *ffio = XEFFIO(obj);
	mark_object(ffio->type);
	mark_object(ffio->size);
        mark_object(ffio->plist);
	return (ffio->plist);
}

static void
print_ffiobject(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	/* This function can GC */
	Lisp_EffiObject *ffio = XEFFIO(obj);
	char buf[256];

	if (print_readably)
		error("printing unreadable object #<ffiobject 0x%x>",
		      ffio->header.uid);
	write_c_string("#<ffiobject ", printcharfun);
        /* Print FFIO type */
        if (!NILP(ffio->type)) {
                write_c_string("type=", printcharfun);
                print_internal(ffio->type, printcharfun, 1);
                write_c_string(" ", printcharfun);
        }
	snprintf(buf, 255, "size=%ld fotype=%d foptr=%p>",
                 (long)XINT(ffio->size), ffio->fotype, ffio->fop.generic);
	write_c_string(buf, printcharfun);
}

static const struct lrecord_description ffiobject_description[] = {
	{XD_LISP_OBJECT, offsetof(Lisp_EffiObject, type)},
	{XD_LISP_OBJECT, offsetof(Lisp_EffiObject, size)},
	{XD_LISP_OBJECT, offsetof(Lisp_EffiObject, plist)},
        {XD_INT, offsetof(Lisp_EffiObject, fotype)},
        {XD_OPAQUE_PTR, offsetof(Lisp_EffiObject, fop)},
	{XD_END}
};

static Lisp_Object
ffi_getprop(Lisp_Object fo, Lisp_Object property)
{
	return external_plist_get(&XEFFIO(fo)->plist, property, 0, ERROR_ME);
}

static int
ffi_putprop(Lisp_Object fo, Lisp_Object property, Lisp_Object value)
{
	external_plist_put(&XEFFIO(fo)->plist, property, value, 0, ERROR_ME);
	return 1;
}

static int
ffi_remprop(Lisp_Object fo, Lisp_Object property)
{
	return external_remprop(&XEFFIO(fo)->plist, property, 0, ERROR_ME);
}

DEFINE_LRECORD_IMPLEMENTATION_WITH_PROPS("ffiobject", ffiobject,
                                         mark_ffiobject, print_ffiobject,
                                         0, 0, 0, ffiobject_description,
                                         ffi_getprop,
                                         ffi_putprop,
                                         ffi_remprop,
                                         Fffi_plist, Lisp_EffiObject);

/** alignment in union and structures **/
/*
 * x86:
 *
 *   - An entire structure or union is aligned on the same boundary as
 *     its most strictly aligned member.
 *
 *   - Each member is assigned to the lowest available offset with the
 *     appropriate alignment.  This may require /internal padding/,
 *     depending on the previous member.
 *
 *   - A structure's size is increased, if necessary, to make it a
 *     multiple of the alignment.  This may require /tail padding/,
 *     depending on the last member.
 *
 *  Internal padding:
 *
 *    struct {
 *     char c;            .-------2+---1+---0.
 *     short s;           |  s     |pad |  c |
 *    }                   `--------+----+----'
 *
 *  Internal and Tail padding:
 *
 *    struct {            .------------1+---0.
 *     char c;            |     pad     |  c |
 *     double d;          |-------------+---4|
 *     short s;           |         d        |
 *    }                   |-----------------8|
 *                        |         d        |
 *                        |------14+-------12|
 *                        |   pad  |    s    |
 *                        `--------+---------'
 *
 *  Union allocation:
 *
 *    union {             .------------1+---0.
 *     char c;            |     pad     |  c |
 *     short s;           |-------2+----+---0|
 *     int j;             |  pad   |    s    |
 *    }                   |--------+--------0|
 *                        |        j         |
 *                        `------------------'
 */
DEFUN("ffi-get-type", Fffi_get_type, 1, 1, 0, /*
Return
                                               */
      (type_name))
{
        if (SYMBOLP(type_name))
                return Fassq(type_name, Vffi_named_types);
        return type_name;
}

static void
ffi_check_type(Lisp_Object type)
{
        apply1(Vffi_type_checker, Fcons(type, Fcons(Qt, Qnil)));
}

DEFUN("ffi-size-of-type", Fffi_size_of_type, 1, 1, 0,	/*
Return size of foreign type TYPE.

Valid foreign types are: `byte', `unsigned-byte', `char',
`unsigned-char', `short', `unsigned-short', `int', `unsigned-int',
`long', `unsigned-long', `pointer-void', `float', `double', `object'.
                                                        */
      (type))
{
        int tsize;

        if (EQ(type, Q_void))
                tsize = 0;
        else if (EQ(type, Q_byte))
                tsize = sizeof(int8_t);
        else if (EQ(type, Q_unsigned_byte))
                tsize = sizeof(uint8_t);
        else if (EQ(type, Q_char))
                tsize = sizeof(char);
        else if (EQ(type, Q_unsigned_char))
                tsize = sizeof(unsigned char);
        else if (EQ(type, Q_short))
                tsize = sizeof(short);
        else if (EQ(type, Q_unsigned_short))
                tsize = sizeof(unsigned short);
        else if (EQ(type, Q_int))
                tsize = sizeof(int);
        else if (EQ(type, Q_unsigned_int))
                tsize = sizeof(unsigned int);
        else if (EQ(type, Q_long))
                tsize = sizeof(long);
        else if (EQ(type, Q_unsigned_long))
                tsize = sizeof(unsigned long);
        else if (EQ(type, Q_float))
                tsize = sizeof(float);
        else if (EQ(type, Q_double))
                tsize = sizeof(double);
        else if (EQ(type, Q_c_string))
                tsize = sizeof(char *);
        else if (CONSP(type) && EQ(XCAR(type), Q_function))
                tsize = sizeof(void(*));
        else if (CONSP(type) && EQ(XCAR(type), Q_pointer))
                tsize = sizeof(void *);
        else if (CONSP(type) && EQ(XCAR(type), Q_array)) {
                Lisp_Object atype = Fcar(XCDR(type));
                Lisp_Object asize = Fcdr(Fcdr(XCDR(type)));

                tsize = XINT(asize) * XINT(Fffi_size_of_type(atype));
        } else if (CONSP(type) && EQ(XCAR(type), Q_struct)) {
                return Fffi_slot_offset(type, Qnil);
        } else if (CONSP(type) && EQ(XCAR(type), Q_union)) {
                Lisp_Object slots = Fcdr(XCDR(type));

                CHECK_CONS(slots);

                tsize = 0;
                while (!NILP(slots)) {
                        Lisp_Object slot_type = Fcar(Fcdr(Fcar(slots)));
                        int slot_size = XINT(Fffi_size_of_type(slot_type));
                        if (slot_size > tsize)
                                tsize = slot_size;
                        slots = XCDR(slots);
                }
        } else
                signal_simple_error("Unrecognized foreign type", type);

        return make_int(tsize);
}

DEFUN("make-ffi-object", Fmake_ffi_object, 1, 1, 0, /*
Create new ffi object.
                                                     */
      (type))
{
	Lisp_Object result;
	Lisp_EffiObject *ffio;

        ffi_check_type(type);

        ffio = alloc_lcrecord_type(Lisp_EffiObject, &lrecord_ffiobject);
	XSETEFFIO(result, ffio);

        ffio->size = Fffi_size_of_type(type);
        ffio->type = type;
        ffio->plist = Qnil;

        /* Initialize foreign pointer */
        ffio->fotype = EFFI_FOT_NONE;
        ffio->fop.ptr = ffio->fostorage;

        return result;
}

DEFUN("ffi-object-p", Fffi_object_p, 1, 1, 0, /*
Return non-nil if FO is ffi object.
                                              */
      (fo))
{
        return (EFFIOP(fo) ? Qt : Qnil);
}

DEFUN("ffi-object-type", Fffi_object_type, 1, 1, 0, /*
Return FO's type.
                                                     */
      (fo))
{
        CHECK_EFFIO(fo);
        return (XEFFIO(fo)->type);
}

DEFUN("ffi-set-object-type", Fffi_set_object_type, 2, 2, 0, /*
Cast FO's foreign value to now TYPE.
                                                            */
      (fo, type))
{
        CHECK_EFFIO(fo);

        ffi_check_type(type);
        XEFFIO(fo)->type = type;

        return type;
}

DEFUN("ffi-object-size", Fffi_object_size, 1, 1, 0, /*
Return FO's name.
                                                    */
      (fo))
{
        CHECK_EFFIO(fo);
        return (XEFFIO(fo)->size);
}

DEFUN("ffi-load-library", Fffi_load_library, 1, 1, 0, /*
Load library.
                                                       */
      (lib_name))
{
        void *handler;
        Lisp_Object fo;
        Lisp_EffiObject *ffio;

        CHECK_STRING(lib_name);

        handler = dlopen(XSTRING_DATA(lib_name), RTLD_GLOBAL|RTLD_NOW);
        if (handler == NULL)
                return Qnil;
        
        fo = Fmake_ffi_object(Fcons(Q_pointer, Fcons(Q_void, Qnil)));
        ffio = XEFFIO(fo);

        ffio->fotype = EFFI_FOT_BIND;
        ffio->fop.ptr = handler;
        
        return fo;
}

DEFUN("ffi-bind", Fffi_bind, 2, 2, 0, /*
Make foreign object of TYPE and bind it external symbol SYM.
                                       */
      (type, sym))
{
        Lisp_Object fo;
        Lisp_EffiObject *ffio;

        ffi_check_type(type);
        CHECK_STRING(sym);

        fo = Fmake_ffi_object(type);
        ffio = XEFFIO(fo);
        ffio->fop.ptr = dlsym(RTLD_NEXT, XSTRING_DATA(sym));
        if (ffio->fop.ptr == NULL)
                return Qnil;

        ffio->fotype = EFFI_FOT_BIND;

        return fo;
}

DEFUN("ffi-dlerror", Fffi_dlerror, 0, 0, 0, /*
Return dl error string.
                                            */
      ())
{
        const char *dles = dlerror();

        return make_string(dles, strlen(dles));
}

DEFUN("ffi-defun", Fffi_defun, 2, 2, 0, /*
Like `ffi-bind', but for function objects.
                                        */
      (type, sym))
{
        Lisp_Object fo;
        Lisp_EffiObject *ffio;

        ffi_check_type(type);
        CHECK_STRING(sym);

        fo = Fmake_ffi_object(type);
        ffio = XEFFIO(fo);
        ffio->fop.fun = dlsym(RTLD_NEXT, XSTRING_DATA(sym));
        if (ffio->fop.fun == NULL)
                return Qnil;

        ffio->fotype = EFFI_FOT_FUNC;

        return fo;
}

DEFUN("ffi-alloc", Fffi_alloc, 2, 2, 0, /*
Allocate space for foreign object of TYPE.
                                              */
      (type, size))
{
        Lisp_Object fo;
        Lisp_EffiObject *ffio;

        ffi_check_type(type);
        CHECK_INT(size);

        fo = Fmake_ffi_object(type);
        ffio = XEFFIO(fo);
        ffio->fop.ptr = malloc(XINT(size));
        if (ffio->fop.ptr == NULL)
                return Qnil;

        ffio->fotype = EFFI_FOT_ALLOC;

        return fo;
}

DEFUN("ffi-free", Fffi_free, 1, 1, 0, /*
Release space allocated for foreign object OBJ.
                                             */
      (fo))
{
        struct Lisp_EffiObject *ffio;

        CHECK_EFFIO(fo);

        ffio = XEFFIO(fo);
        if (EFFIO_HAS_FOP(ffio)) {
                free(ffio->fop.ptr);
                ffio->fotype = EFFI_FOT_NONE;
                return Qt;
        }

        return Qnil;
}

/*
 * Return alignment policy for struct or union FFI_SU.
 * x86: Return 1, 2 or 4.
 * mips: Return 1, 2, 4 or 8.
 */
static int
ffi_type_align(Lisp_Object type)
{
        int al;
        
        if (SYMBOLP(type)) {
                if (EQ(type, Q_byte) || EQ(type, Q_unsigned_byte)
                    || EQ(type, Q_char) || EQ(type, Q_unsigned_char))
                        return 1;
                if (EQ(type, Q_short) || EQ(type, Q_unsigned_short))
                        return 2;
#ifdef FFI_MIPS
                if (EQ(type, Q_double))
                        return 8;
#endif  /* FFI_MIPS */
                return 4;
                /* NOT REACHED */
        }
              
        for (al = 0, type = Fcdr(Fcdr(type));
             !NILP(type);
             type = Fcdr(type))
        {
                Lisp_Object stype = Fcar(Fcdr(Fcar(type)));
                int tmp_al = ffi_type_align(stype);

                if (tmp_al > al)
                        al = tmp_al;
        }

        return al;
}

DEFUN("ffi-slot-offset", Fffi_slot_offset, 2, 2, 0, /*
Calculate offset to the SLOT.
SLOT can be either valid SLOT in TYPE or `nil'.
In case if SLOT is `nil' - size of struct is returned.
                                                     */
      (type, slot))
{
        Lisp_Object fotype = Fffi_get_type(type);
        Lisp_Object slots;
        int lpad, align, retoff;

        if (!CONSP(fotype))
                error("Not struct or union");

        retoff = 0;
        lpad = align = ffi_type_align(fotype);
        slots = Fcdr(XCDR(fotype));
        CHECK_CONS(slots);
        while (!NILP(slots)) {
                Lisp_Object tmp_slot = Fcar(Fcdr(XCAR(slots)));
                int tmp_align;
                int tmp_size;

                /*
                 * NOTE:
                 *  - for basic types TMP_ALIGN and TMP_SIZE are equal
                 */
                tmp_align = ffi_type_align(tmp_slot);

                if (EQ(XCAR(XCAR(slots)), slot)) {
                        /* SLOT found */
                        if (lpad < tmp_align) {
                                retoff += lpad;
                                lpad = 0;
                        } else
                                lpad -= tmp_align;
                        break;
                }

                tmp_size = XINT(Fffi_size_of_type(tmp_slot));
                while (tmp_size > 0) {
                        if (lpad < tmp_align) {
                                retoff += lpad;
                                lpad = align;
                        }
                        tmp_size -= tmp_align;
                        lpad -= tmp_align;
                        retoff += tmp_align;
                }

                slots = XCDR(slots);
        }
        return make_int(retoff + lpad);
}

DEFUN("ffi-fetch", Fffi_fetch, 3, 3, 0, /*
Get FO value.
                                    */
      (fo, offset, type))
{
        Lisp_Object retval = Qnil;
        Lisp_EffiObject *ffio;
        void *ptr;

        CHECK_EFFIO(fo);
        CHECK_INT(offset);

        ffio = XEFFIO(fo);
        ptr = (void*)((char*)ffio->fop.ptr + XINT(offset));

        /* Import from foreign level */
        if (EQ(type, Q_byte) || EQ(type, Q_char))
                retval = make_char(*(char*)ptr);
        else if (EQ(type, Q_unsigned_byte) || EQ(type, Q_unsigned_char))
                retval = make_char(*(char unsigned*)ptr);
        else if (EQ(type, Q_short))
                retval = make_int(*(short*)ptr);
        else if (EQ(type, Q_unsigned_short))
                retval = make_int(*(unsigned short*)ptr);
        else if (EQ(type, Q_int))
                retval = make_int(*(int*)ptr);
        else if (EQ(type, Q_unsigned_int))
                retval = make_int(*(unsigned int*)ptr);
        else if (EQ(type, Q_long))
                retval = make_int(*(long*)ptr);
        else if (EQ(type, Q_unsigned_long))
                retval = make_int(*(unsigned long*)ptr);
        else if (EQ(type, Q_float))
                retval = make_float(*(float*)ptr);
        else if (EQ(type, Q_double))
                retval = make_float(*(double*)ptr);
        else if (EQ(type, Q_c_string))
                retval = make_string((char*)ptr, strlen((char*)ptr));
        else
                error("non basic type");

        return retval;
}

DEFUN("ffi-store", Fffi_store, 4, 4, 0, /*
In FO's foreign space at OFFSET store value VAL of type VAL_TYPE.
                                     */
      (fo, offset, val_type, val))
{
        Lisp_EffiObject *ffio;
        void *ptr;

        CHECK_EFFIO(fo);
        CHECK_INT(offset);

        ffio = XEFFIO(fo);
        ptr = (void*)((char*)ffio->fop.ptr + XINT(offset));

        if (EQ(val_type, Q_byte) || EQ(val_type, Q_char)
            || EQ(val_type, Q_unsigned_byte) || EQ(val_type, Q_unsigned_char))
        {
                if (!CHARP(val))
                        error("type");
                *(char*)ptr = XCHAR(val);
        } else if (EQ(val_type, Q_short) || EQ(val_type, Q_unsigned_short)) {
                if (!INTP(val))
                        error("type");
                *(short*)ptr = (short)XINT(val);
        } else if (EQ(val_type, Q_int) || EQ(val_type, Q_unsigned_int)) {
                if (!INTP(val))
                        error("type");
                *(int*)ptr = XINT(val);
        } else if (EQ(val_type, Q_long) || EQ(val_type, Q_unsigned_long)) {
                if (!INTP(val))
                        error("type");
                *(long*)ptr = (long)XINT(val);
        } else if (EQ(val_type, Q_float)) {
                if (!FLOATP(val))
                        error("type");
                *(float*)ptr = XFLOATINT(val);
        } else if (EQ(val_type, Q_double)) {
                if (!FLOATP(val))
                        error("type");
                *(double*)ptr = XFLOAT_DATA(val);
        } else if (EQ(val_type, Q_c_string)) {
                if (!STRINGP(val))
                        error("type");
                strcpy((char*)ptr, XSTRING_DATA(val));
        } else
                error("non basic type");

        return val;
}

DEFUN("ffi-address-of", Fffi_address_of, 1, 1, 0, /*
Return FFI object that stores address of given FFI object FO.
                                     */
      (fo))
{
        Lisp_Object newfo;
        Lisp_EffiObject *ffio, *newffio;

        CHECK_EFFIO(fo);
        ffio = XEFFIO(fo);

        newfo = Fmake_ffi_object(Fcons(Q_pointer, Fcons(ffio->type, Qnil)));
        newffio = XEFFIO(newfo);

        newffio->fotype = EFFI_FOT_BIND;
        if (EQ(ffio->type, Q_c_string)
            || (CONSP(ffio->type) && EQ(XCAR(ffio->type), Q_pointer)))
                newffio->fop.ptr = (void*)&ffio->fop.ptr;
        else
                newffio->fop.ptr = ffio->fop.ptr;

        return newfo;
}

DEFUN("ffi-plist", Fffi_plist, 1, 1, 0, /*
Return properties list for FFI object FO.
                                        */
      (fo))
{
        CHECK_EFFIO(fo);
        return (XEFFIO(fo)->plist);
}

#ifdef HAVE_LIBFFI

static int lf_cindex = 0;

#define MAX_TYPES_VALUES 1024
static ffi_type *ex_ffitypes[MAX_TYPES_VALUES + 1];
static void *ex_values[MAX_TYPES_VALUES + 1];

static void
extffi_setup_argument(Lisp_Object type, ffi_type **ft)
{
        if (EQ(type, Q_char) || EQ(type, Q_byte))
                *ft = &ffi_type_schar;
        else if (EQ(type, Q_unsigned_char) || EQ(type, Q_unsigned_byte))
                *ft = &ffi_type_uchar;
        else if (EQ(type, Q_short))
                *ft = &ffi_type_sshort;
        else if (EQ(type, Q_unsigned_short))
                *ft = &ffi_type_ushort;
        else if (EQ(type, Q_int))
                *ft = &ffi_type_sint;
        else if (EQ(type, Q_unsigned_int))
                *ft = &ffi_type_uint;
        else if (EQ(type, Q_float))
                *ft = &ffi_type_float;
        else if (EQ(type, Q_double))
                *ft = &ffi_type_double;
        else if (EQ(type, Q_void))
                *ft = &ffi_type_void;
        else if (EQ(type, Q_c_string))
                *ft = &ffi_type_pointer;
        else if (CONSP(type) && EQ(XCAR(type), Q_pointer))
                *ft = &ffi_type_pointer;
        else if (CONSP(type) && EQ(XCAR(type), Q_array))
                *ft = &ffi_type_pointer;
        else if (CONSP(type) && EQ(XCAR(type), Q_struct)) {
                Lisp_Object slots = Fcdr(XCDR(type));
                ffi_type **ntypes;
                int nt_size, i;

                CHECK_CONS(slots);

                nt_size = XINT(Flength(slots)) + 1;
                if (nt_size + lf_cindex > MAX_TYPES_VALUES) {
                        lf_cindex = 0;  /* reset cindex */
                        error("cindex overflow");
                }
                ntypes = &ex_ffitypes[lf_cindex];

                /* BUG BUG BUG */
                (*ft)->type = FFI_TYPE_STRUCT;
                (*ft)->alignment = 0;
                (*ft)->elements = ntypes;

                for (i = 0; (i < nt_size) && !NILP(slots); slots = XCDR(slots), i++)
                        extffi_setup_argument(Fcar(Fcdr(XCAR(slots))), &ntypes[i]);
                ntypes[i] = NULL;
        }
}

static int
ffi_call_using_libffi(Lisp_Object fo_fun, Lisp_Object ret_fo,
                      int in_nargs, Lisp_Object *in_args)
{
        Lisp_EffiObject *ffio;
        ffi_cif cif;
        ffi_type *rtype;
        void *rvalue;
        int i;
        
        lf_cindex = in_nargs;           /* reserve */
        for (i = 0; i < in_nargs; i++) {
                ffio = XEFFIO(in_args[i]);

                extffi_setup_argument(ffio->type, &ex_ffitypes[i]);
                if (EQ(ffio->type, Q_c_string)
                    || (CONSP(ffio->type) && EQ(XCAR(ffio->type), Q_pointer)))
                        ex_values[i] = &ffio->fop.ptr;
                else
                        ex_values[i] = ffio->fop.ptr;
        }

        ffio = XEFFIO(ret_fo);
        extffi_setup_argument(ffio->type, &rtype);
        if (EQ(ffio->type, Q_c_string)
            || (CONSP(ffio->type) && EQ(XCAR(ffio->type), Q_pointer)))
                rvalue = &ffio->fop.ptr;
        else
                rvalue = ffio->fop.ptr;

        if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, in_nargs,
                         rtype, ex_ffitypes) == FFI_OK)
        {
                ffi_call(&cif, XEFFIO(fo_fun)->fop.fun, rvalue, ex_values);
                return 0;
        }

        /* FAILURE */
        return 1;
}
#endif  /* HAVE_LIBFFI */

DEFUN("ffi-call-function", Fffi_call_function, 1, MANY, 0, /*
Call a function reffered by FO with arguments ARGS.
                                                           */
      (int nargs, Lisp_Object * args))
{
        Lisp_EffiObject *ffio = XEFFIO(args[0]);
        Lisp_Object retfo = Fmake_ffi_object(XCAR(XCDR(ffio->type)));
        int ret = -1;

#ifdef HAVE_LIBFFI
        ret = ffi_call_using_libffi(args[0], retfo, nargs-1, &args[1]);
#endif  /* HAVE_LIBFFI */

        return (ret == 0 ? retfo : Qnil);
}

DEFUN("ffi-null-p", Fffi_null_p, 1, 1, 0, /*
Return non-nil if FO is null pointer.
Non-nil may be returned only for c-string or pointer types.
                                          */
      (fo))
{
        Lisp_EffiObject *ffio;

        CHECK_EFFIO(fo);
        ffio = XEFFIO(fo);
        if ((EQ(ffio->type, Q_c_string)
             || (CONSP(ffio->type) && EQ(XCAR(ffio->type), Q_pointer)))
            && (ffio->fop.ptr == NULL))
                return Qt;
        else
                return Qnil;
}

void
syms_of_ffi(void)
{
	INIT_LRECORD_IMPLEMENTATION(ffiobject);

	defsymbol(&Q_byte, "byte");
	defsymbol(&Q_unsigned_byte, "unsigned-byte");
	defsymbol(&Q_char, "char");
	defsymbol(&Q_unsigned_char, "unsigned-char");
	defsymbol(&Q_short, "short");
	defsymbol(&Q_unsigned_short, "unsigned-short");
	defsymbol(&Q_int, "int");
	defsymbol(&Q_unsigned_int, "unsigned-int");
	defsymbol(&Q_long, "long");
	defsymbol(&Q_unsigned_long, "unsigned-long");
	defsymbol(&Q_float, "float");
	defsymbol(&Q_double, "double");
	defsymbol(&Q_void, "void");
	defsymbol(&Q_pointer, "pointer");
	defsymbol(&Q_struct, "struct");
	defsymbol(&Q_union, "union");
	defsymbol(&Q_array, "array");
	defsymbol(&Q_function, "function");
	defsymbol(&Q_c_string, "c-string");

	defsymbol(&Qffiobjectp, "ffiobjectp");

	DEFSUBR(Fffi_size_of_type);
	DEFSUBR(Fffi_get_type);
	DEFSUBR(Fmake_ffi_object);
	DEFSUBR(Fffi_object_p);
	DEFSUBR(Fffi_object_type);
	DEFSUBR(Fffi_object_size);
        DEFSUBR(Fffi_set_object_type);
	DEFSUBR(Fffi_alloc);
	DEFSUBR(Fffi_free);
	DEFSUBR(Fffi_fetch);
	DEFSUBR(Fffi_store);
	DEFSUBR(Fffi_address_of);
        DEFSUBR(Fffi_slot_offset);
	DEFSUBR(Fffi_load_library);
        DEFSUBR(Fffi_bind);
	DEFSUBR(Fffi_dlerror);
	DEFSUBR(Fffi_defun);
        DEFSUBR(Fffi_call_function);
        DEFSUBR(Fffi_null_p);

        DEFSUBR(Fffi_plist);
}

void
vars_of_ffi(void)
{
	DEFVAR_LISP("ffi-named-types", &Vffi_named_types	/*
Alist of named FFI types with elements in form (NAME . FFI-TYPE).
						 */ );
	Vffi_named_types = Qnil;

	DEFVAR_LISP("ffi-loaded-libraries", &Vffi_loaded_libraries /*
Alist of loaded libraries with elements in form (LIB-NAME . FFIO).
						 */ );
	Vffi_loaded_libraries = Qnil;

	DEFVAR_LISP("ffi-type-checker", &Vffi_type_checker /*
Function that calls to check ffi type validity.
						 */ );
	Vffi_type_checker = Qnil;
}
