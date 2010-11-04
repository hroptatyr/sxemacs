/* C support for testing SXEmacs - see tests/automated/c-tests.el
   Copyright (C) 2000 Martin Buchholz

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


/* Author: Martin Buchholz

   This file provides support for running tests for SXEmacs that cannot
   be written entirely in Lisp.  These tests are run automatically via
   tests/automated/c-tests.el, or can be run by hand using M-x */

#include <config.h>
#include "lisp.h"
#include "buffer.h"
#include "lstream.h"
#include "opaque.h"

static Lisp_Object Vtest_function_list;

DEFUN("test-data-format-conversion", Ftest_data_format_conversion, 0, 0, "",	/*
Test TO_EXTERNAL_FORMAT() and TO_INTERNAL_FORMAT()
*/
      ())
{
	void *ptr;
	size_t len;
	Lisp_Object string, opaque;

	Bufbyte int_foo[] = "\n\nfoo\nbar";
	Extbyte ext_unix[] = "\n\nfoo\nbar";

	Extbyte ext_dos[] = "\r\n\r\nfoo\r\nbar";
	Extbyte ext_mac[] = "\r\rfoo\rbar";
	Lisp_Object opaque_dos = make_opaque(ext_dos, sizeof(ext_dos) - 1);
	Lisp_Object string_foo = make_string(int_foo, sizeof(int_foo) - 1);

	Extbyte ext_latin[] = "f\372b\343\340";
	Bufbyte int_latin1[] = "f\201\372b\201\343\201\340";
	Bufbyte int_latin2[] = "f\202\372b\202\343\202\340";
#ifdef MULE
	Extbyte ext_latin12[] = "f\033-A\372b\343\340\033-B";
	Extbyte ext_tilde[] = "f~b~~";
	Lisp_Object string_latin2 =
	    make_string(int_latin2, sizeof(int_latin2) - 1);
#endif
	Lisp_Object opaque_latin =
	    make_opaque(ext_latin, sizeof(ext_latin) - 1);
	Lisp_Object opaque0_latin = make_opaque(ext_latin, sizeof(ext_latin));
	Lisp_Object string_latin1 =
	    make_string(int_latin1, sizeof(int_latin1) - 1);

	/* Check for expected strings before and after conversion.
	   Conversions depend on whether MULE is defined,
	   and on whether FILE_CODING is defined. */
#ifdef MULE
#define DFC_CHECK_DATA_COND_MULE(ptr,len,			\
				 constant_string_mule,		\
				 constant_string_non_mule)	\
    DFC_CHECK_DATA (ptr, len, constant_string_mule)
#define DFC_CHECK_DATA_COND_MULE_NUL(ptr,len,			\
				     constant_string_mule,	\
				     constant_string_non_mule)	\
    DFC_CHECK_DATA_NUL (ptr, len, constant_string_mule)
#else
#define DFC_CHECK_DATA_COND_MULE(ptr,len,			\
				 constant_string_mule,		\
				 constant_string_non_mule)	\
    DFC_CHECK_DATA (ptr, len, constant_string_non_mule)
#define DFC_CHECK_DATA_COND_MULE_NUL(ptr,len,			\
				     constant_string_mule,	\
				     constant_string_non_mule)	\
    DFC_CHECK_DATA_NUL (ptr, len, constant_string_non_mule)
#endif

#ifdef FILE_CODING
#define DFC_CHECK_DATA_COND_EOL(ptr,len,			\
				 constant_string_eol,		\
				 constant_string_non_eol)	\
    DFC_CHECK_DATA (ptr, len, constant_string_eol)
#define DFC_CHECK_DATA_COND_EOL_NUL(ptr,len,			\
				     constant_string_eol,	\
				     constant_string_non_eol)	\
    DFC_CHECK_DATA_NUL (ptr, len, constant_string_eol)
#else
#define DFC_CHECK_DATA_COND_EOL(ptr,len,			\
				 constant_string_eol,		\
				 constant_string_non_eol)	\
    DFC_CHECK_DATA (ptr, len, constant_string_non_eol)
#define DFC_CHECK_DATA_COND_EOL_NUL(ptr,len,			\
				     constant_string_eol,	\
				     constant_string_non_eol)	\
    DFC_CHECK_DATA_NUL (ptr, len, constant_string_non_eol)
#endif

	/* Check for expected strings before and after conversion. */
#define DFC_CHECK_DATA(ptr,len, constant_string) do {	\
    assert ((len) == sizeof (constant_string) - 1);	\
    assert (!memcmp (ptr, constant_string, len));	\
  } while (0)

	/* Macro version that includes the trailing NULL byte. */
#define DFC_CHECK_DATA_NUL(ptr,len,constant_string) do {\
    assert ((len) == sizeof (constant_string));		\
    assert (!memcmp (ptr, constant_string, len));	\
  } while (0)

#ifdef MULE
	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(DATA, (int_latin2, sizeof(int_latin2)),
			   ALLOCA, (ptr, len),
			   Fget_coding_system(intern("iso-8859-2")));
	DFC_CHECK_DATA_NUL(ptr, len, ext_latin);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(LISP_STRING, string_latin2,
			   ALLOCA, (ptr, len),
			   Fget_coding_system(intern("iso-8859-2")));
	DFC_CHECK_DATA(ptr, len, ext_latin);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(LISP_STRING, string_latin1,
			   ALLOCA, (ptr, len),
			   Fget_coding_system(intern("iso-8859-2")));
	DFC_CHECK_DATA(ptr, len, ext_latin12);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(DATA, (int_latin2, sizeof(int_latin2) - 1),
			   MALLOC, (ptr, len),
			   Fget_coding_system(intern("iso-8859-2")));
	DFC_CHECK_DATA(ptr, len, ext_latin);
	xfree(ptr);

	TO_EXTERNAL_FORMAT(DATA, (int_latin2, sizeof(int_latin2) - 1),
			   LISP_OPAQUE, opaque,
			   Fget_coding_system(intern("iso-8859-2")));
	DFC_CHECK_DATA(XOPAQUE_DATA(opaque), XOPAQUE_SIZE(opaque), ext_latin);

	ptr = NULL, len = rand();
	TO_INTERNAL_FORMAT(DATA, (ext_latin, sizeof(ext_latin) - 1),
			   ALLOCA, (ptr, len), intern("iso-8859-2"));
	DFC_CHECK_DATA(ptr, len, int_latin2);

	ptr = NULL, len = rand();
	TO_INTERNAL_FORMAT(DATA, (ext_latin, sizeof(ext_latin) - 1),
			   MALLOC, (ptr, len), intern("iso-8859-2"));
	DFC_CHECK_DATA(ptr, len, int_latin2);
	xfree(ptr);

	TO_INTERNAL_FORMAT(DATA, (ext_latin, sizeof(ext_latin) - 1),
			   LISP_STRING, string, intern("iso-8859-2"));
	DFC_CHECK_DATA(XSTRING_DATA(string), XSTRING_LENGTH(string),
		       int_latin2);

	TO_INTERNAL_FORMAT(LISP_OPAQUE, opaque_latin,
			   LISP_STRING, string, intern("iso-8859-2"));
	DFC_CHECK_DATA(XSTRING_DATA(string), XSTRING_LENGTH(string),
		       int_latin2);

	TO_INTERNAL_FORMAT(LISP_OPAQUE, opaque0_latin,
			   LISP_STRING, string, intern("iso-8859-2"));
	DFC_CHECK_DATA_NUL(XSTRING_DATA(string), XSTRING_LENGTH(string),
			   int_latin2);

	{
		Lisp_Object tmp = Fcurrent_buffer();
		TO_INTERNAL_FORMAT(LISP_OPAQUE, opaque0_latin,
				   LISP_BUFFER, tmp,
				   intern("iso-8859-2"));
		DFC_CHECK_DATA_NUL(BUF_BYTE_ADDRESS
				   (current_buffer, BUF_PT(current_buffer)),
				   sizeof(int_latin2), int_latin2);

		TO_INTERNAL_FORMAT(LISP_OPAQUE, opaque_latin,
				   LISP_BUFFER, tmp,
				   intern("iso-8859-1"));
		DFC_CHECK_DATA(BUF_BYTE_ADDRESS(current_buffer,
						BUF_PT(current_buffer)),
			       sizeof(int_latin1) - 1, int_latin1);
	}
	TO_INTERNAL_FORMAT(DATA, (ext_latin12, sizeof(ext_latin12) - 1),
			   ALLOCA, (ptr, len), intern("iso-8859-2"));
	DFC_CHECK_DATA(ptr, len, int_latin1);

#endif				/* MULE */

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(DATA, (int_latin1, sizeof(int_latin1) - 1),
			   ALLOCA, (ptr, len), Qbinary);
	DFC_CHECK_DATA_COND_MULE(ptr, len, ext_latin, int_latin1);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(DATA, (int_latin1, sizeof(int_latin1)),
			   ALLOCA, (ptr, len), Qbinary);
	DFC_CHECK_DATA_COND_MULE_NUL(ptr, len, ext_latin, int_latin1);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(DATA, (int_latin2, sizeof(int_latin2) - 1),
			   ALLOCA, (ptr, len), Fget_coding_system(Qbinary));
	DFC_CHECK_DATA_COND_MULE(ptr, len, ext_tilde, int_latin2);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(DATA, (int_latin1, sizeof(int_latin1) - 1),
			   ALLOCA, (ptr, len), intern("iso-8859-1"));
	DFC_CHECK_DATA_COND_MULE(ptr, len, ext_latin, int_latin1);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(LISP_STRING, string_latin1,
			   ALLOCA, (ptr, len), Qbinary);
	DFC_CHECK_DATA_COND_MULE(ptr, len, ext_latin, int_latin1);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(LISP_STRING, string_latin1,
			   ALLOCA, (ptr, len), Fget_coding_system(Qbinary));
	DFC_CHECK_DATA_COND_MULE(ptr, len, ext_latin, int_latin1);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(LISP_STRING, string_latin1,
			   ALLOCA, (ptr, len), intern("iso-8859-1"));
	DFC_CHECK_DATA_COND_MULE(ptr, len, ext_latin, int_latin1);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(DATA, (int_latin1, sizeof(int_latin1) - 1),
			   MALLOC, (ptr, len), Qbinary);
	DFC_CHECK_DATA_COND_MULE(ptr, len, ext_latin, int_latin1);
	xfree(ptr);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(DATA, (int_latin2, sizeof(int_latin2)),
			   MALLOC, (ptr, len), Fget_coding_system(Qbinary));
	DFC_CHECK_DATA_COND_MULE_NUL(ptr, len, ext_tilde, int_latin2);
	xfree(ptr);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(DATA, (int_latin1, sizeof(int_latin1) - 1),
			   MALLOC, (ptr, len), intern("iso-8859-1"));
	DFC_CHECK_DATA_COND_MULE(ptr, len, ext_latin, int_latin1);
	xfree(ptr);

	TO_EXTERNAL_FORMAT(DATA, (int_latin1, sizeof(int_latin1) - 1),
			   LISP_OPAQUE, opaque, Qbinary);
	DFC_CHECK_DATA_COND_MULE(XOPAQUE_DATA(opaque),
				 XOPAQUE_SIZE(opaque), ext_latin, int_latin1);

	TO_EXTERNAL_FORMAT(DATA, (int_latin2, sizeof(int_latin2)),
			   LISP_OPAQUE, opaque, Fget_coding_system(Qbinary));
	DFC_CHECK_DATA_COND_MULE_NUL(XOPAQUE_DATA(opaque),
				     XOPAQUE_SIZE(opaque), ext_tilde,
				     int_latin2);

	TO_EXTERNAL_FORMAT(DATA, (int_latin1, sizeof(int_latin1) - 1),
			   LISP_OPAQUE, opaque, intern("iso-8859-1"));
	DFC_CHECK_DATA_COND_MULE(XOPAQUE_DATA(opaque),
				 XOPAQUE_SIZE(opaque), ext_latin, int_latin1);

	ptr = NULL, len = rand();
	TO_INTERNAL_FORMAT(DATA, (ext_latin, sizeof(ext_latin) - 1),
			   ALLOCA, (ptr, len), Qbinary);
	DFC_CHECK_DATA_COND_MULE(ptr, len, int_latin1, ext_latin);

	ptr = NULL, len = rand();
	TO_INTERNAL_FORMAT(DATA, (ext_latin, sizeof(ext_latin)),
			   ALLOCA, (ptr, len), intern("iso-8859-1"));
	DFC_CHECK_DATA_COND_MULE_NUL(ptr, len, int_latin1, ext_latin);

	ptr = NULL, len = rand();
	TO_INTERNAL_FORMAT(DATA, (ext_latin, sizeof(ext_latin)),
			   MALLOC, (ptr, len), intern("iso-8859-1"));
	DFC_CHECK_DATA_COND_MULE_NUL(ptr, len, int_latin1, ext_latin);
	xfree(ptr);

	ptr = NULL, len = rand();
	TO_INTERNAL_FORMAT(DATA, (ext_latin, sizeof(ext_latin)),
			   MALLOC, (ptr, len), Qnil);
	DFC_CHECK_DATA_COND_MULE_NUL(ptr, len, int_latin1, ext_latin);
	xfree(ptr);

	TO_INTERNAL_FORMAT(DATA, (ext_latin, sizeof(ext_latin) - 1),
			   LISP_STRING, string, intern("iso-8859-1"));
	DFC_CHECK_DATA_COND_MULE(XSTRING_DATA(string),
				 XSTRING_LENGTH(string), int_latin1, ext_latin);

	TO_INTERNAL_FORMAT(LISP_OPAQUE, opaque_latin,
			   LISP_STRING, string, intern("iso-8859-1"));
	DFC_CHECK_DATA_COND_MULE(XSTRING_DATA(string),
				 XSTRING_LENGTH(string), int_latin1, ext_latin);

	TO_INTERNAL_FORMAT(LISP_OPAQUE, opaque0_latin,
			   LISP_STRING, string, intern("iso-8859-1"));
	DFC_CHECK_DATA_COND_MULE_NUL(XSTRING_DATA(string),
				     XSTRING_LENGTH(string), int_latin1,
				     ext_latin);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(DATA, (int_foo, sizeof(int_foo)),
			   MALLOC, (ptr, len), Fget_coding_system(Qbinary));
	DFC_CHECK_DATA_COND_EOL_NUL(ptr, len, ext_unix, int_foo);
	xfree(ptr);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(DATA, (int_foo, sizeof(int_foo) - 1),
			   LISP_OPAQUE, opaque, intern("raw-text-mac"));
	DFC_CHECK_DATA_COND_EOL(XOPAQUE_DATA(opaque),
				XOPAQUE_SIZE(opaque), ext_mac, int_foo);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(LISP_STRING, string_foo,
			   ALLOCA, (ptr, len), intern("raw-text-dos"));
	DFC_CHECK_DATA_COND_EOL(ptr, len, ext_dos, int_foo);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(DATA, (int_foo, sizeof(int_foo) - 1),
			   ALLOCA, (ptr, len), intern("raw-text-unix"));
	DFC_CHECK_DATA_COND_EOL(ptr, len, ext_unix, int_foo);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(LISP_STRING, string_foo,
			   MALLOC, (ptr, len), intern("no-conversion-mac"));
	DFC_CHECK_DATA_COND_EOL(ptr, len, ext_mac, int_foo);
	xfree(ptr);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(DATA, (int_foo, sizeof(int_foo) - 1),
			   ALLOCA, (ptr, len),
			   Fget_coding_system(intern("no-conversion-dos")));
	DFC_CHECK_DATA_COND_EOL(ptr, len, ext_dos, int_foo);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(DATA, (int_foo, sizeof(int_foo)),
			   ALLOCA, (ptr, len), intern("no-conversion-unix"));
	DFC_CHECK_DATA_COND_EOL_NUL(ptr, len, ext_unix, int_foo);

#ifdef FILE_CODING
	{
		Lisp_Object tmp = Fcurrent_buffer();
		TO_INTERNAL_FORMAT(LISP_OPAQUE, opaque_dos,
				   LISP_BUFFER, tmp, intern("undecided"));
		DFC_CHECK_DATA(BUF_BYTE_ADDRESS(current_buffer,
						BUF_PT(current_buffer)),
			       sizeof(int_foo) - 1, int_foo);
	}
#endif	/* FILE_CODING */

	TO_INTERNAL_FORMAT(DATA, (ext_mac, sizeof(ext_mac) - 1),
			   LISP_STRING, string, intern("iso-8859-1"));
	DFC_CHECK_DATA_COND_EOL(XSTRING_DATA(string),
				XSTRING_LENGTH(string), int_foo, ext_mac);

	{
		Lisp_Object stream =
		    make_fixed_buffer_input_stream(ext_dos,
						   sizeof(ext_dos) - 1);
		TO_INTERNAL_FORMAT(LISP_LSTREAM, stream, LISP_STRING, string,
				   intern("iso-8859-1"));
		DFC_CHECK_DATA_COND_EOL(XSTRING_DATA(string),
					XSTRING_LENGTH(string), int_foo,
					ext_dos);
	}

	TO_INTERNAL_FORMAT(DATA, (ext_unix, sizeof(ext_unix) - 1),
			   LISP_STRING, string, intern("no-conversion"));
	DFC_CHECK_DATA_COND_EOL(XSTRING_DATA(string),
				XSTRING_LENGTH(string), int_foo, ext_unix);

	ptr = NULL, len = rand();
	TO_EXTERNAL_FORMAT(LISP_OPAQUE, opaque_dos,
			   ALLOCA, (ptr, len), Qbinary);
	DFC_CHECK_DATA(ptr, len, ext_dos);

	return intern("PASS");
}

#define TESTS_DEFSUBR(Fname) do {		\
  DEFSUBR (Fname);				\
  Vtest_function_list =				\
    Fcons (intern (subr_name (&S##Fname)),	\
	   Vtest_function_list);		\
} while (0)

void syms_of_tests(void)
{
	Vtest_function_list = Qnil;

	TESTS_DEFSUBR(Ftest_data_format_conversion);
	/* Add other test functions here with TESTS_DEFSUBR */
}

void vars_of_tests(void)
{
	DEFVAR_LISP("test-function-list", &Vtest_function_list	/*
List of all test functions defined in tests.c.
For use by the automated test suite.  See tests/automated/c-tests.
								 */ );
}
