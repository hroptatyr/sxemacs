/* sysdll.h --- system dependent support for dynamic linked libraries
   Copyright (C) 1998 Free Software Foundation, Inc.
   Author:  William Perry <wmperry@aventail.com>

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
along with XEmacs; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef INCLUDED_sysdll_h_
#define INCLUDED_sysdll_h_

#ifdef __cplusplus
extern "C" {
#endif

#if defined(WIN32_NATIVE)
#define DLLEXPORT __declspec(dllexport)
#elif defined(WIN16)
#define DLLEXPORT FAR PASCAL _EXPORT
#else
#define DLLEXPORT
#endif

typedef void * dll_handle;
typedef void * dll_func;
typedef void * dll_var;

int dll_init(const char *);
int dll_shutdown(void);
dll_handle dll_open(const char *);
int dll_close(dll_handle);
dll_func dll_function(dll_handle,const char *);
dll_var dll_variable(dll_handle,const char *);
const char *dll_error(dll_handle);

#ifdef __cplusplus
}
#endif

/* More stand-ins ... */

#define Qdll_filename_encoding Qfile_name
#define Qdll_function_name_encoding Qnative

#endif /* INCLUDED_sysdll_h_ */
