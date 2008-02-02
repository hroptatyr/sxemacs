/*
 * effi.c --- Foreign Function Interface for SXEmacs.
 *
 * Copyright (C) 2004 Evgeny Zajcev
 *
This file is part of SXEmacs

SXEmacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SXEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */


#include <config.h>
#include "lisp.h"

#include <dlfcn.h>
#include <math.h>

#include "effi.h"

#include "buffer.h"
#ifdef FILE_CODING
#include "file-coding.h"
#endif

#ifdef HAVE_LIBFFI
#undef ALIGN
#include <ffi.h>
#endif  /* HAVE_LIBFFI */

#ifdef EF_USE_ASYNEQ
#include "workers.h"
#include "worker-asyneq.h"
#endif	/* EF_USE_ASYNEQ */

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
Lisp_Object Q_c_string, Q_c_data;
Lisp_Object Q_boolean;

#define FFI_TPTR(type) (EQ(type, Q_c_string)                                    \
                        || EQ(type, Q_c_data)                                   \
                        || (CONSP(type) && ((EQ(XCAR(type), Q_c_data))          \
                                            || EQ(XCAR(type), Q_pointer)        \
                                            || EQ(XCAR(type), Q_array))))

Lisp_Object Qffiobjectp;

/* Alist with elements in form (NAME . TYPE) */
Lisp_Object Vffi_loaded_libraries;
Lisp_Object Vffi_named_types;

Lisp_Object Vffi_type_checker;

static Lisp_Object Vffi_all_objects;

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

        escapeflag = escapeflag;        /* shutup compiler */

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
        {XD_SIZE_T, offsetof(Lisp_EffiObject, storage_size)},
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

static size_t
sizeof_ffiobject(const void *header)
{
        const Lisp_EffiObject *effio = (const Lisp_EffiObject *)header;
        return (sizeof(Lisp_EffiObject) + effio->storage_size);
}

/* Define ffiobject implementation */
const struct lrecord_implementation lrecord_ffiobject = {
        .name = "ffiobject",
        .marker = mark_ffiobject,
        .printer = print_ffiobject,
        .finalizer = 0,
        .equal = 0,
        .hash = 0,
        .description = ffiobject_description,
        .getprop = ffi_getprop,
        .putprop = ffi_putprop,
        .remprop = ffi_remprop,
        .plist = Fffi_plist,
        .static_size = 0,
        .size_in_bytes_method = sizeof_ffiobject,
        .lrecord_type_index = lrecord_type_ffiobject,
        .basic_p = 0
};


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
static void
ffi_check_type(Lisp_Object type)
{
        apply1(Vffi_type_checker, Fcons(type, Fcons(Qt, Qnil)));
}

DEFUN("ffi-basic-type-p", Fffi_basic_type_p, 1, 1, 0, /*
Return non-nil if TYPE is a basic FFI type.

A type is said to be basic, if it is neither a pointer nor a
function, and there is a corresponding built-in type in C.
                                                      */
      (type))
{
        if (EQ(type, Q_byte) || EQ(type, Q_unsigned_byte) || EQ(type, Q_char)
	    || EQ(type, Q_unsigned_char) || EQ(type, Q_short)
	    || EQ(type, Q_unsigned_short) || EQ(type, Q_int)
	    || EQ(type, Q_unsigned_int) || EQ(type, Q_long)
	    || EQ(type, Q_unsigned_long) || EQ(type, Q_float)
	    || EQ(type, Q_double) || EQ(type, Q_void) || EQ(type, Q_c_string)
	    || EQ(type, Q_c_data) || (CONSP(type) && EQ(XCAR(type), Q_c_data))
            || EQ(type, Q_boolean))
                return Qt;
        else
                return Qnil;
}


Lisp_Object ffi_canonicalise_type(Lisp_Object type)
{
	/* this function can GC */
        Lisp_Object stype = type;
	struct gcpro gcpro1, gcpro2;
	GCPRO2(stype, type);

        while (!NILP(type) &&
	       NILP(Fffi_basic_type_p(type)) &&
	       SYMBOLP(type))
                type = Fcdr(Fassq(type, Vffi_named_types));

	UNGCPRO;
        return type;
}

DEFUN("ffi-canonicalise-type", Fffi_canonicalise_type, 1, 1, 0, /*
Return FFI type TYPE in a canonical form.
						  */
      (type))
{
	Lisp_Object canon_type = ffi_canonicalise_type(type);
	if (NILP(canon_type))
		signal_simple_error("No such FFI type", type);

	return canon_type;
}

DEFUN("ffi-size-of-type", Fffi_size_of_type, 1, 1, 0,	/*
Return the size of the foreign type TYPE.

Valid foreign types are: `byte', `unsigned-byte', `char',
`unsigned-char', `short', `unsigned-short', `int', `unsigned-int',
`long', `unsigned-long', `pointer-void', `float', `double', 
`object', and `c-string'.
                                                        */
      (type))
{
        int tsize;

        type = ffi_canonicalise_type(type);
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
        else if (EQ(type, Q_boolean))
                tsize = sizeof(int);
        else if (EQ(type, Q_c_string))
                tsize = sizeof(char *);
        else if (EQ(type, Q_c_data) ||
		 (CONSP(type) && EQ(XCAR(type), Q_c_data)))
                tsize = sizeof(void *);
        else if (CONSP(type) && EQ(XCAR(type), Q_function))
                tsize = sizeof(void(*));
        else if (CONSP(type) && EQ(XCAR(type), Q_pointer))
                tsize = sizeof(void *);
        else if (CONSP(type) && EQ(XCAR(type), Q_array)) {
                Lisp_Object atype = Fcar(XCDR(type));
                Lisp_Object asize = Fcar(Fcdr(XCDR(type)));

                CHECK_INT(asize);
                tsize = XINT(asize) * XINT(Fffi_size_of_type(atype));
        } else if (CONSP(type) && EQ(XCAR(type), Q_struct)) {
                return Fffi_slot_offset(type, Qnil);
        } else if (CONSP(type) && EQ(XCAR(type), Q_union)) {
                Lisp_Object slots = Fcdr(XCDR(type));

                CHECK_CONS(slots);

                tsize = 0;
                while (!NILP(slots)) {
                        Lisp_Object slot_type = Fcar(Fcdr(XCAR(slots)));
                        int slot_size = XINT(Fffi_size_of_type(slot_type));
                        if (slot_size > tsize)
                                tsize = slot_size;
                        slots = XCDR(slots);
                }
        } else
                signal_simple_error("Unrecognized foreign type", type);

        return make_int(tsize);
}

DEFUN("make-ffi-object", Fmake_ffi_object, 1, 2, 0, /*
Create a new FFI object of type TYPE.
If optional argument SIZE is non-`nil' it should be an
integer, in this case additional storage size to hold data 
of at least length SIZE is allocated.
                                                     */
      (type, size))
{
	Lisp_Object result = Qnil;
	Lisp_EffiObject *ffio;
	struct gcpro gcpro1;

        GCPRO1(result);

        ffi_check_type(type);
        if (NILP(size))
                size = Fffi_size_of_type(type);
        CHECK_INT(size);

	if (CONSP(type) && EQ(XCAR(type), Q_c_data) && INTP(XCDR(type)))
		size = XCDR(type);

        if ((EQ(type, Q_c_string) && (XINT(size) < 1))
            || (!(EQ(type, Q_c_string) ||
		  (CONSP(type) && EQ(XCAR(type), Q_pointer)))
                && (XINT(size) < XINT(Fffi_size_of_type(type)))))
                signal_simple_error("storage size too small to store type",
				    list2(size, type));

        ffio = alloc_lcrecord(sizeof(Lisp_EffiObject)+XINT(size),
			      &lrecord_ffiobject);
	XSETEFFIO(result, ffio);

        ffio->size = Fffi_size_of_type(type);
        ffio->type = type;
        ffio->plist = Qnil;

        /* Initialize foreign pointer */
        ffio->fotype = EFFI_FOT_NONE;
	ffio->storage_size = XINT(size);
        ffio->fop.ptr = ffio->fostorage;

        if (!NILP(Vffi_all_objects))
                XWEAK_LIST_LIST(Vffi_all_objects) =
			Fcons(result, XWEAK_LIST_LIST(Vffi_all_objects));

        RETURN_UNGCPRO(result);
}

DEFUN("ffi-object-p", Fffi_object_p, 1, 1, 0, /*
Return non-nil if FO is an FFI object, nil otherwise.
                                              */
      (fo))
{
        return (EFFIOP(fo) ? Qt : Qnil);
}

DEFUN("ffi-object-canonical-type", Fffi_object_canonical_type, 1, 1, 0, /*
Return FO's real type, that is after resolving user defined types.
									*/
      (fo))
{
	CHECK_EFFIO(fo);
	return ffi_canonicalise_type(XEFFIO(fo)->type);
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
Cast FO to type TYPE and reassign the cast value.
                                                            */
      (fo, type))
{
        CHECK_EFFIO(fo);

        ffi_check_type(type);
        XEFFIO(fo)->type = type;

        return type;
}

DEFUN("ffi-object-size", Fffi_object_size, 1, 1, 0, /*
Return the size of the allocated space of FO.
                                                    */
      (fo))
{
        CHECK_EFFIO(fo);
        return (XEFFIO(fo)->size);
}

DEFUN("ffi-set-storage-size", Fffi_set_storage_size, 2, 2, 0, /*
Set the size of the allocated space of FO.
							      */
      (fo, size))
{
	CHECK_EFFIO(fo);
	CHECK_INT(size);
	XEFFIO(fo)->storage_size = XUINT(size);
	return Qt;
}

DEFUN("ffi-load-library", Fffi_load_library, 1, 1, 0, /*
Load library LIBNAME and return a foreign object handle if successful,
or `nil' if the library cannot be loaded.

The argument LIBNAME should be the file-name string of a shared
object library (usual extension is `.so').

The library should reside in one of the directories specified by the
$LD_LIBRARY_PATH environment variable or the more global ld.so.cache.
						      */
      (libname))
{
        void *handler;
        Lisp_Object fo = Qnil;
        Lisp_EffiObject *ffio;
	struct gcpro gcpro1;

        CHECK_STRING(libname);

        handler = dlopen((const char *)XSTRING_DATA(libname), RTLD_GLOBAL|RTLD_NOW);
        if (handler == NULL)
                return Qnil;
        
        GCPRO1(fo);
        fo = Fmake_ffi_object(Fcons(Q_pointer, Fcons(Q_void, Qnil)), Qnil);
        ffio = XEFFIO(fo);

        ffio->fotype = EFFI_FOT_BIND;
        ffio->fop.ptr = handler;
        
        RETURN_UNGCPRO(fo);
}

DEFUN("ffi-bind", Fffi_bind, 2, 2, 0, /*
Make and return a foreign object of type TYPE and bind it to the
external symbol SYM.

The argument TYPE can be any type-cell.
The argument SYM should be a string naming an arbitrary symbol
in one of the loaded libraries.

If SYM does not exist in any of the loaded libraries, `nil' is
returned.
                                       */
      (type, sym))
{
        Lisp_Object fo = Qnil;
        Lisp_EffiObject *ffio;
	struct gcpro gcpro1;

        ffi_check_type(type);
        CHECK_STRING(sym);

        GCPRO1(fo);
        fo = Fmake_ffi_object(type, Qnil);
        ffio = XEFFIO(fo);
        ffio->fop.ptr = dlsym(RTLD_NEXT, (const char*)XSTRING_DATA(sym));
        if (ffio->fop.ptr == NULL) {
                UNGCPRO;
                return Qnil;
        }

        ffio->fotype = EFFI_FOT_BIND;

        RETURN_UNGCPRO(fo);
}

DEFUN("ffi-dlerror", Fffi_dlerror, 0, 0, 0, /*
Return dl error string.
                                            */
      ())
{
        const char *dles = dlerror();

	if (dles)
		return make_string((const Bufbyte*)dles, strlen(dles));
	else
		return Qnil;
}

DEFUN("ffi-defun", Fffi_defun, 2, 2, 0, /*
Make and return a foreign object of type TYPE and bind it to the
external symbol SYM.

The argument TYPE should be a function type-cell.
The argument SYM should be a string naming a function in one of
the loaded libraries.

If SYM does not exist in any of the loaded libraries, an error
is indicated.

This is like `ffi-bind' but for function objects.
                                        */
      (type, sym))
{
        Lisp_Object fo = Qnil;
        Lisp_EffiObject *ffio;
	struct gcpro gcpro1;

        ffi_check_type(type);
        CHECK_STRING(sym);
        
        GCPRO1(fo);

        fo = Fmake_ffi_object(type, Qnil);
        ffio = XEFFIO(fo);
        ffio->fop.fun = dlsym(RTLD_NEXT, (const char *)XSTRING_DATA(sym));
        if (ffio->fop.fun == NULL)
                signal_simple_error("Can't define function", sym);

        ffio->fotype = EFFI_FOT_FUNC;

        RETURN_UNGCPRO(fo);
}

/*
 * Return alignment policy for struct or union FFI_SU.
 * x86: Return 1, 2 or 4.
 * mips: Return 1, 2, 4 or 8.
 */
static int
ffi_type_align(Lisp_Object type)
{
        type = ffi_canonicalise_type(type);
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
        } else if (CONSP(type)
                   && (EQ(XCAR(type), Q_struct) || EQ(XCAR(type), Q_union))) {
                int al;

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

        return 4;
}

DEFUN("ffi-slot-offset", Fffi_slot_offset, 2, 2, 0, /*
Return the offset of SLOT in TYPE.
SLOT can be either a valid (named) slot in TYPE or `nil'.
If SLOT is `nil' return the size of the struct.
                                                     */
      (type, slot))
{
        Lisp_Object slots;
        int lpad, align, retoff;

        type = ffi_canonicalise_type(type);
        if (!CONSP(type))
                error("Not struct or union");

        retoff = 0;
        lpad = align = ffi_type_align(type);
        slots = Fcdr(XCDR(type));
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
Fetch value from the foreign object FO from OFFSET position.
TYPE specifies value for data to be fetched.
					*/
      (fo, offset, type))
{
        Lisp_Object retval = Qnil;
        Lisp_EffiObject *ffio;
        void *ptr;
	struct gcpro gcpro1;

        CHECK_EFFIO(fo);
        CHECK_INT(offset);

        ffio = XEFFIO(fo);
        ptr = (void*)((char*)ffio->fop.ptr + XINT(offset));

        type = ffi_canonicalise_type(type);

        GCPRO1(retval);
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
        else if (EQ(type, Q_boolean))
                retval = (*(int*)ptr) ? Qt : Qnil;
        else if (EQ(type, Q_c_string)) {
		size_t tlen;
		tlen = strlen((char*)ptr);
#if 0
		/* dangerous if ptr has a non-native encoding, like UTF8 */
                retval = make_string((const Bufbyte*)ptr, tlen);
#elif defined(MULE)
		TO_INTERNAL_FORMAT(DATA, (ptr, tlen),
				   LISP_STRING, retval, Qnil);
#else
		retval = make_ext_string((char*)ptr, tlen, Qbinary);
#endif
	} else if (EQ(type, Q_c_data) ||
		   (CONSP(type) && EQ(XCAR(type), Q_c_data))) {
                size_t tlen;
                if (EQ(type, Q_c_data)) {
                        tlen = ffio->storage_size - XINT(offset);
                } else {
                        CHECK_INT(XCDR(type));
                        tlen = XUINT(XCDR(type));
                }
                retval = make_ext_string((char*)ptr, tlen, Qbinary);
        } else if (EQ(type, Q_void)) {
                retval = Qnil;
        } else if (CONSP(type) && EQ(XCAR(type), Q_pointer)) {
                retval = Fmake_ffi_object(type, Qnil);
                XEFFIO(retval)->fop.ptr = *(void**)ptr;
        } else if (CONSP(type) && EQ(XCAR(type), Q_function)) {
                retval = Fmake_ffi_object(type, Qnil);
                XEFFIO(retval)->fop.fun = (void*)ptr;
                XEFFIO(retval)->fotype = EFFI_FOT_FUNC;
        } else
                signal_simple_error("Can't fetch for this type", type);

        RETURN_UNGCPRO(retval);
}

DEFUN("ffi-aref", Fffi_aref, 2, 2, 0, /*
Return the element of FARRAY at index IDX (starting with 0).
                                      */
      (farray, idx))
{
        Lisp_Object type;
        
        CHECK_EFFIO(farray);
        CHECK_INT(idx);
        
        type = ffi_canonicalise_type(XEFFIO(farray)->type);
        if (!FFI_TPTR(type))
                signal_simple_error("Not an array type", type);
        if (EQ(type, Q_c_string))
                type = Q_char;
        else
                type = Fcar(XCDR(type));

        return Fffi_fetch(farray, make_int(XINT(Fffi_size_of_type(type)) * XINT(idx)), type);
}

DEFUN("ffi-store", Fffi_store, 4, 4, 0, /*
For foreign object FO at specified OFFSET store data.
Type of data is specified by VAL-TYPE and data itself specified in VAL.

VAL-TYPE can be either a basic FFI type or an FFI pointer.
If VAL-TYPE is a basic FFI type, then VAL can be an
ordinary, but suitable Emacs lisp object.
If VAL-TYPE is an FFI pointer then VAL _must_ be an FFI
object of the underlying type pointed to.
					*/
      (fo, offset, val_type, val))
{
        Lisp_EffiObject *ffio;
        void *ptr;

        CHECK_EFFIO(fo);
        CHECK_INT(offset);

        ffio = XEFFIO(fo);
        ptr = (void*)((char*)ffio->fop.ptr + XINT(offset));

        val_type = ffi_canonicalise_type(val_type);

        if (EQ(val_type, Q_byte) || EQ(val_type, Q_char)
            || EQ(val_type, Q_unsigned_byte) || EQ(val_type, Q_unsigned_char)) {
                if (!CHARP(val))
                        signal_error(
				Qwrong_type_argument, list2(Qcharacterp, val));
                *(char*)ptr = XCHAR(val);
        } else if (EQ(val_type, Q_short) || EQ(val_type, Q_unsigned_short)) {
                if (!INTP(val))
                        signal_error(
				Qwrong_type_argument, list2(Qintegerp, val));
                *(short*)ptr = (short)XINT(val);
        } else if (EQ(val_type, Q_int) || EQ(val_type, Q_unsigned_int)) {
                if (INTP(val))
                        *(int*)ptr = XINT(val);
                else if (FLOATP(val))
                        *(int*)ptr = (int)XFLOATINT(val);
                else
                        signal_error(
				Qwrong_type_argument, list2(Qfloatp, val));
        } else if (EQ(val_type, Q_long) || EQ(val_type, Q_unsigned_long)) {
                if (INTP(val))
                        *(long*)ptr = (long)XINT(val);
                else if (FLOATP(val))
                        *(long*)ptr = (long)XFLOATINT(val);
                else
                        signal_error(
				Qwrong_type_argument, list2(Qfloatp, val));
        } else if (EQ(val_type, Q_float)) {
                if (!FLOATP(val))
                        signal_error(
				Qwrong_type_argument, list2(Qfloatp, val));
                *(float*)ptr = XFLOATINT(val);
        } else if (EQ(val_type, Q_double)) {
                if (!FLOATP(val))
                        signal_error(
				Qwrong_type_argument, list2(Qfloatp, val));
                *(double*)ptr = XFLOAT_DATA(val);
        } else if (EQ(val_type, Q_boolean)) {
                *(int*)ptr = !NILP(val_type);
        } else if (EQ(val_type, Q_c_string)) {
		char *tmp;
		int tmplen;
                if (!STRINGP(val))
                        signal_error(
				Qwrong_type_argument, list2(Qstringp, val));
#if defined(MULE)
		TO_EXTERNAL_FORMAT(LISP_STRING, val,
				   ALLOCA, (tmp, tmplen), Qnil);
		memcpy((char*)ptr, tmp, tmplen + 1);
#else
                memcpy((char*)ptr,
		       (const char *)XSTRING_DATA(val),
		       XSTRING_LENGTH(val) + 1);
#endif
        } else if (EQ(val_type, Q_c_data) ||
		   (CONSP(val_type) &&
		    EQ(XCAR(val_type), Q_c_data) && INTP(XCDR(val_type)))) {
		char *val_ext;
		unsigned int val_ext_len;
                if (!STRINGP(val))
                        signal_error(Qwrong_type_argument, list2(Qstringp, val));
                if (CONSP(val_type) &&
		    XSTRING_LENGTH(val) > XINT(XCDR(val_type)) - 1)
                        error("storage size too small");
		TO_EXTERNAL_FORMAT(LISP_STRING, val, ALLOCA,
				   (val_ext, val_ext_len), Qbinary);
                memcpy((char*)ptr, (const char *)val_ext, val_ext_len+1);
        } else if (CONSP(val_type) && EQ(XCAR(val_type), Q_pointer)) {
                if (!EFFIOP(val))
                        error("type");
                *(void**)ptr = (void*)XEFFIO(val)->fop.ptr;
        } else
                error("non basic or pointer type");

        return val;
}

DEFUN("ffi-aset", Fffi_aset, 3, 3, 0, /*
Store the element VALUE in FARRAY at index IDX (starting with 0).
                                      */
      (farray, idx, value))
{
        Lisp_Object type;
        
        CHECK_EFFIO(farray);
        CHECK_INT(idx);
        
        type = ffi_canonicalise_type(XEFFIO(farray)->type);
        if (!FFI_TPTR(type))
                signal_simple_error("Not an array type", type);
        if (EQ(type, Q_c_string))
                type = Q_char;
        else
                type = Fcar(XCDR(type));

        return Fffi_store(farray, make_int(XINT(Fffi_size_of_type(type)) * XINT(idx)), type, value);
}

DEFUN("ffi-address-of", Fffi_address_of, 1, 1, 0, /*
Return the FFI object that stores the address of given FFI object FO.

This is the equivalent of the `&' operator in C.
						  */
      (fo))
{
        Lisp_Object newfo = Qnil;
        Lisp_EffiObject *ffio, *newffio;
	struct gcpro gcpro1;

        CHECK_EFFIO(fo);
        ffio = XEFFIO(fo);

        GCPRO1(newfo);
        newfo = Fmake_ffi_object(Fcons(Q_pointer, Fcons(ffio->type, Qnil)), Qnil);
        newffio = XEFFIO(newfo);

        newffio->fotype = EFFI_FOT_BIND;
        if (FFI_TPTR(ffio->type))
                newffio->fop.ptr = (void*)&ffio->fop.ptr;
        else
                newffio->fop.ptr = ffio->fop.ptr;

        RETURN_UNGCPRO(newfo);
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

#if SIZEOF_LONG == 4
#define effi_type_ulong ffi_type_uint32
#define effi_type_slong ffi_type_sint32
#elif SIZEOF_LONG == 8
#define effi_type_ulong ffi_type_uint64
#define effi_type_slong ffi_type_sint64
#endif

static void
extffi_setup_argument(Lisp_Object type, ffi_type **ft)
{
        type = ffi_canonicalise_type(type);
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
        else if (EQ(type, Q_unsigned_long))
                *ft = &effi_type_ulong;
        else if (EQ(type, Q_long))
                *ft = &effi_type_slong;
        else if (EQ(type, Q_float))
                *ft = &ffi_type_float;
        else if (EQ(type, Q_double))
                *ft = &ffi_type_double;
        else if (EQ(type, Q_boolean))
                *ft = &ffi_type_sint;
        else if (EQ(type, Q_void))
                *ft = &ffi_type_void;
        else if (FFI_TPTR(type))
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
        } else
                signal_simple_error("Can't setup argument for type", type);
}

static int
ffi_call_using_libffi(Lisp_Object fo_fun, Lisp_Object ret_fo,
                      int in_nargs, Lisp_Object *in_args)
{
        Lisp_EffiObject *ffio;
        Lisp_Object fft;
        ffi_cif cif;
        ffi_type *rtype;
        void *rvalue;
        int i;

        lf_cindex = in_nargs;           /* reserve */
        for (i = 0; i < in_nargs; i++) {
                ffio = XEFFIO(in_args[i]);
                fft = Fffi_canonicalise_type(ffio->type);
                extffi_setup_argument(fft, &ex_ffitypes[i]);
                if (FFI_TPTR(fft))
                        ex_values[i] = &ffio->fop.ptr;
                else
                        ex_values[i] = ffio->fop.ptr;
        }

        ffio = XEFFIO(ret_fo);
        fft = Fffi_canonicalise_type(ffio->type);
        extffi_setup_argument(fft, &rtype);
        if (FFI_TPTR(fft))
                rvalue = &ffio->fop.ptr;
        else
                rvalue = ffio->fop.ptr;

        if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, in_nargs,
                         rtype, ex_ffitypes) == FFI_OK)
        {
                ffi_call(&cif, (void(*)(void))XEFFIO(fo_fun)->fop.fun, rvalue, ex_values);
                return 0;
        }

        /* FAILURE */
        return 1;
}
#endif  /* HAVE_LIBFFI */

DEFUN("ffi-call-function", Fffi_call_function, 1, MANY, 0, /*
Call a function referred to by FO with arguments ARGS, maybe
return a foreign object with the result or nil if there is
none.

Arguments are: FO &rest FO-ARGS

FO should be a foreign binding initiated by `ffi-defun', and
ARGS should be foreign data objects or pointers to these.
                                                           */
      (int nargs, Lisp_Object * args))
{
        Lisp_Object faf = Qnil, retfo = Qnil;
        Lisp_EffiObject *ffio;
        int ret = -1;
	struct gcpro gcpro1, gcpro2;

        GCPRO2(faf, retfo);

        faf =  args[0];
        ffio = XEFFIO(faf);
        retfo = Fmake_ffi_object(XCAR(XCDR(ffio->type)), Qnil);

#ifdef HAVE_LIBFFI
        ret = ffi_call_using_libffi(args[0], retfo, nargs-1, &args[1]);
#endif  /* HAVE_LIBFFI */

        RETURN_UNGCPRO(ret == 0 ? retfo : Qnil);
}

#ifdef EF_USE_ASYNEQ
/* handler for asynchronously calling ffi code */
Lisp_Object Qffi_jobp;
#define EFFI_DEBUG_JOB(args...)
static Lisp_Object
exec_sentinel_unwind(Lisp_Object UNUSED(datum))
{
	return Qnil;
}

static inline void
exec_sentinel(void *job, ffi_job_t ffij)
	__attribute__((always_inline));
static inline void
exec_sentinel(void *job, ffi_job_t ffij)
{
	/* This function can GC */
	/* called from main thread */
	Lisp_Object *funcell, sntnl = ffij->sntnl, dummy = Qnil;
	int speccount = specpdl_depth(), nargs = ffij->sntnl_nargs, i;
	struct gcpro gcpro1, gcpro2;

	GCPRO2(dummy, sntnl);
	funcell = alloca_array(Lisp_Object, nargs+2);
	funcell[0] = ffij->sntnl;
	funcell[1] = (Lisp_Object)job;
	for (i = 0; i < nargs; i++) {
		funcell[2+i] = ffij->sntnl_args[i];
	}
	lock_allocator();
	gcpro1.var = funcell;
	gcpro1.nvars = nargs+2;
	unlock_allocator();

	record_unwind_protect(exec_sentinel_unwind, Qnil);
	/* call the funcell */
	Ffuncall(nargs+2, funcell);
	/* reset to previous state */
	restore_match_data();
	UNGCPRO;
	unbind_to(speccount, Qnil);
	return;
}

static inline ffi_job_t
allocate_ffi_job(void)
{
	ffi_job_t ffij = xnew(struct ffi_job_s);
	EFFI_DEBUG_JOB("allocated: 0x%lx\n", (long unsigned int)ffij);
	return ffij;
}

static inline ffi_job_t
make_ffi_job(Lisp_Object fof, int fof_nargs, Lisp_Object *fof_args,
	     Lisp_Object sntnl, int sntnl_nargs, Lisp_Object *sntnl_args)
{
/* exec'd in the main thread */
	ffi_job_t ffij = allocate_ffi_job();
	int i;

	SXE_MUTEX_INIT(&ffij->mtx);
	ffij->fof = fof;
	if (fof_nargs > 0) {
		ffij->fof_nargs = fof_nargs;
		ffij->fof_args = xnew_array(Lisp_Object, fof_nargs);
		for (i = 0; i < fof_nargs; i++) {
			ffij->fof_args[i] = fof_args[i];
		}
	} else {
		ffij->fof_nargs = 0;
		ffij->fof_args = NULL;
	}

	ffij->sntnl = sntnl;
	if (sntnl_nargs > 0) {
		ffij->sntnl_nargs = sntnl_nargs;
		ffij->sntnl_args = xnew_array(Lisp_Object, sntnl_nargs);
		for (i = 0; i < sntnl_nargs; i++) {
			ffij->sntnl_args[i] = sntnl_args[i];
		}
	} else {
		ffij->sntnl_nargs = 0;
		ffij->sntnl_args = NULL;
	}

	ffij->result = Qnil;
	ffij->retfo = Fmake_ffi_object(XCAR(XCDR(XEFFIO(fof)->type)), Qnil);
	return ffij;
}

static void
mark_ffi_job(worker_job_t job)
{
	ffi_job_t ffij = ffi_job(job);
	int i;

	if (!ffij)
		return;

	SXE_MUTEX_LOCK(&ffij->mtx);
	mark_object(ffij->fof);
	for (i = 0; i < ffij->fof_nargs; i++) {
		mark_object(ffij->fof_args[i]);
	}
	mark_object(ffij->sntnl);
	for (i = 0; i < ffij->sntnl_nargs; i++) {
		mark_object(ffij->sntnl_args[i]);
	}
	mark_object(ffij->retfo);
	mark_object(ffij->result);
	SXE_MUTEX_UNLOCK(&ffij->mtx);
	return;
}

static void
print_ffi_job(worker_job_t job, Lisp_Object pcf)
{
	ffi_job_t ffij = ffi_job(job);
	char *str = alloca(64);

	SXE_MUTEX_LOCK(&ffij->mtx);
	write_c_string(" carrying ", pcf);
	snprintf(str, 63, " #<ffi-job 0x%lx>", (long unsigned int)ffij);
	write_c_string(str, pcf);
	SXE_MUTEX_UNLOCK(&ffij->mtx);
	return;
}

static inline void
finish_ffi_job_data(ffi_job_t ffij)
{
	SXE_MUTEX_LOCK(&ffij->mtx);
	xfree(ffij->fof_args);
	xfree(ffij->sntnl_args);
	SXE_MUTEX_UNLOCK(&ffij->mtx);
	SXE_MUTEX_FINI(&ffij->mtx);

	EFFI_DEBUG_JOB("finished: 0x%lx\n", (long unsigned int)ffij);
	xfree(ffij);
}

static void
finish_ffi_job(worker_job_t job)
{
	ffi_job_t ffij;

	lock_worker_job(job);
	ffij = ffi_job(job);

	if (ffij) {
		finish_ffi_job_data(ffij);
	}
	worker_job_data(job) = NULL;
	unlock_worker_job(job);
	return;
}

static void
ffi_job_handle(worker_job_t job)
{
	/* thread-safe */
	/* usually called from aux threads */
	ffi_job_t ffij;
        Lisp_Object fof = Qnil, retfo = Qnil, *args = NULL;
	int nargs, ret = -1;

	lock_worker_job(job);
	ffij = ffi_job(job);
	unlock_worker_job(job);
	SXE_MUTEX_LOCK(&ffij->mtx);
	fof = ffij->fof;
	nargs = ffij->fof_nargs;
	args = ffij->fof_args;
	SXE_MUTEX_UNLOCK(&ffij->mtx);

	/* can't ... Fmake_ffi_object is not mt-safe */
	/* retfo = Fmake_ffi_object(XCAR(XCDR(XEFFIO(fof)->type)), Qnil); */
	retfo = ffij->retfo;

#ifdef HAVE_LIBFFI
        ret = ffi_call_using_libffi(fof, retfo, nargs, args);
#endif  /* HAVE_LIBFFI */
	if (ret == 0) {
		SXE_MUTEX_LOCK(&ffij->mtx);
		ffij->result = retfo;
		SXE_MUTEX_UNLOCK(&ffij->mtx);
	}

	EFFI_DEBUG_JOB("job 0x%lx succeeded\n", (long unsigned int)ffij);
	return;
}

static void
ffi_job_finished(worker_job_t job)
{
	if (NILP(ffi_job_sentinel(job) /* sentinel */)) {
		return;
	}
	/* called from main thread */
	exec_sentinel(job, ffi_job(job));
	return;
}

static struct work_handler_s ffi_job_handler = {
	mark_ffi_job, print_ffi_job, finish_ffi_job,
	ffi_job_handle, NULL, ffi_job_finished
};

static Lisp_Object
make_ffi_asyneq_job(ffi_job_t ffij)
{
	/* create a job digestible by the asyneq */
	Lisp_Object job = Qnil;
	struct gcpro gcpro1;

	GCPRO1(job);
	job = (Lisp_Object)make_worker_job(&ffi_job_handler);
	XWORKER_JOB_DATA(job) = ffij;
	/* the scratch buffer thingie */
	UNGCPRO;
	return job;
}

DEFUN("ffi-call-function&", Fffi_call_functionX, 1, MANY, 0, /*
Call a function referred to by FO with arguments ARGS asynchronously,
return a job object.

Arguments are: FO &rest FO-ARGS &aux SENTINEL &rest SENTINEL-ARGS

FO should be a foreign binding initiated by `ffi-defun'.
FO-ARGS should be exactly as many foreign data objects as FO needs.
SENTINEL is a lisp sentinel function called when the job finished,
  the function should take at least one argument JOB, further arguments
  may be specified by passing further SENTINEL-ARGS.
								       */
      (int nargs, Lisp_Object *args))
{
	Lisp_Object job = Qnil;
	Lisp_Object sntnl, fof, *sntnl_args, *fof_args;
	int sntnl_nargs, fof_nargs;
	ffi_job_t ffij;
	struct gcpro gcpro1, gcpro2;

	CHECK_EFFIO(args[0]);
	GCPRO2(args[0], job);
	gcpro1.nvars = nargs;

	fof = args[0];
	/* determine how many args belong to the fof */
        fof_nargs = XINT(Flength(XCDR(XEFFIO(fof)->type)))-1;
	fof_args = &args[1];

	if ((sntnl_nargs = nargs - fof_nargs - 2) >= 0) {
		sntnl = args[fof_nargs+1];
		sntnl_args = &args[fof_nargs+2];
	} else {
		sntnl = Qnil;
		sntnl_args = NULL;
	}

	/* create the job data object */
	ffij = make_ffi_job(fof, fof_nargs, fof_args,
			    sntnl, sntnl_nargs, sntnl_args);
	/* now prepare the job to dispatch */
	job = make_ffi_asyneq_job(ffij);
	/* ... and dispatch it, change its state to queued */
	XWORKER_JOB_STATE(job) = WORKER_JOB_QUEUED;
	eq_enqueue(delegate_eq, job);
	/* brag about new jobs in the queue */
	eq_queue_trigger_all(delegate_eq);

	UNGCPRO;
	return job;
}
#endif	/* EF_USE_ASYNEQ */

DEFUN("ffi-null-p", Fffi_null_p, 1, 1, 0, /*
Return non-nil if FO is a null pointer, nil otherwise.
Non-nil may be returned only for pointer types or the type c-string.
                                          */
      (fo))
{
        Lisp_EffiObject *ffio;
	Lisp_Object real_type;

        CHECK_EFFIO(fo);
        ffio = XEFFIO(fo);
	real_type = ffi_canonicalise_type(ffio->type);

        if ((EQ(real_type, Q_c_string)
             || EQ(real_type, Q_c_data)
             || (CONSP(real_type) && EQ(XCAR(real_type), Q_pointer)))
            && (ffio->fop.ptr == NULL))
                return Qt;
        else
                return Qnil;
}

DEFUN("ffi-null-pointer", Fffi_null_pointer, 0, 0, 0, /*
Return ffi object that represents null pointer.
This is the equivalent of `NULL' in C.
                                                      */
      ())
{
        Lisp_Object npfo = Fmake_ffi_object(Fcons(Q_pointer, Fcons(Q_void, Qnil)), Qnil);

        XEFFIO(npfo)->fop.ptr = NULL;
        return npfo;
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
	defsymbol(&Q_c_data, "c-data");
	defsymbol(&Q_boolean, "boolean");

	defsymbol(&Qffiobjectp, "ffiobjectp");

        DEFSUBR(Fffi_basic_type_p);
        DEFSUBR(Fffi_canonicalise_type);
	DEFSUBR(Fffi_size_of_type);
	DEFSUBR(Fmake_ffi_object);
	DEFSUBR(Fffi_object_p);
	DEFSUBR(Fffi_object_canonical_type);
	DEFSUBR(Fffi_object_type);
	DEFSUBR(Fffi_object_size);
	DEFSUBR(Fffi_set_storage_size);
	DEFSUBR(Fffi_set_object_type);
	DEFSUBR(Fffi_fetch);
	DEFSUBR(Fffi_aref);
	DEFSUBR(Fffi_store);
	DEFSUBR(Fffi_aset);
	DEFSUBR(Fffi_address_of);
        DEFSUBR(Fffi_slot_offset);
	DEFSUBR(Fffi_load_library);
        DEFSUBR(Fffi_bind);
	DEFSUBR(Fffi_dlerror);
	DEFSUBR(Fffi_defun);
        DEFSUBR(Fffi_call_function);
        DEFSUBR(Fffi_null_p);
        DEFSUBR(Fffi_null_pointer);

        DEFSUBR(Fffi_plist);

#ifdef EF_USE_ASYNEQ
        DEFSUBR(Fffi_call_functionX);
	defsymbol(&Qffi_jobp, "ffi-job-p");
#endif
}

void
reinit_vars_of_ffi(void)
{
        staticpro_nodump(&Vffi_all_objects);
        Vffi_all_objects = make_weak_list(WEAK_LIST_SIMPLE);
}

void
vars_of_ffi(void)
{
        reinit_vars_of_ffi();

	DEFVAR_LISP("ffi-named-types", &Vffi_named_types	/*
Alist of named FFI types with elements of the form (NAME . FFI-TYPE).
						 */ );
	Vffi_named_types = Qnil;

	DEFVAR_LISP("ffi-loaded-libraries", &Vffi_loaded_libraries /*
Alist of loaded libraries with elements of the form (LIB-NAME . FFIO).
						 */ );
	Vffi_loaded_libraries = Qnil;

	DEFVAR_LISP("ffi-type-checker", &Vffi_type_checker /*
Function to call when the validity of an FFI type shall be checked.
							   */ );
	Vffi_type_checker = intern("ffi-type-p");
}
