/* sysdll.c --- system dependent support for dynamic linked libraries
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include "lisp.h"
#include "sysdll.h"

/* This whole file is conditional upon HAVE_SHLIB */
#ifdef HAVE_SHLIB

/* Thankfully, most systems follow the ELFish dlopen() method.
*/

#if defined(HAVE_DLOPEN)
#include <dlfcn.h>

#ifndef RTLD_LAZY
# define RTLD_LAZY 1
#endif /* RTLD_LAZY isn't defined under FreeBSD - ick */

#ifndef RTLD_NOW
# define RTLD_NOW 2
#endif

int
dll_init (const char *arg)
{
  return 0;
}


dll_handle
dll_open (const char *fname)
{
  return (dll_handle) dlopen (fname, RTLD_NOW);
}

int
dll_close (dll_handle h)
{
  return dlclose ((void *) h);
}

dll_func
dll_function (dll_handle h, const char *n)
{
#ifdef DLSYM_NEEDS_UNDERSCORE
  char *buf = alloca_array (char, strlen (n) + 2);
  *buf = '_';
  strcpy (buf + 1, n);
  n = buf;
#endif
  return (dll_func) dlsym ((void *) h, n);
}

dll_var
dll_variable (dll_handle h, const char *n)
{
#ifdef DLSYM_NEEDS_UNDERSCORE
  char *buf = alloca_array (char, strlen (n) + 2);
  *buf = '_';
  strcpy (buf + 1, n);
  n = buf;
#endif
  return (dll_var)dlsym ((void *)h, n);
}

const char *
dll_error (dll_handle h)
{
#if defined(HAVE_DLERROR) || defined(dlerror)
  return (const char *) dlerror ();
#elif defined(HAVE__DLERROR)
  return (const char *) _dlerror();
#else
  return "Shared library error";
#endif
}

#elif defined(HAVE_SHL_LOAD)
/* This is the HP/UX version */
#include <dl.h>
int
dll_init (const char *arg)
{
  return 0;
}

dll_handle
dll_open (const char *fname)
{
  /* shl_load will hang hard if passed a NULL fname. */
  if (fname == NULL) return NULL;

  return (dll_handle) shl_load (fname, BIND_DEFERRED,0L);
}

int
dll_close (dll_handle h)
{
  return shl_unload ((shl_t) h);
}

dll_func
dll_function (dll_handle h, const char *n)
{
  long handle = 0L;

  if (shl_findsym ((shl_t *) &h, n, TYPE_PROCEDURE, &handle))
    return NULL;

  return (dll_func) handle;
}

dll_var
dll_variable (dll_handle h, const char *n)
{
  long handle = 0L;

  if (shl_findsym ((shl_t *) &h, n, TYPE_DATA, &handle))
    return NULL;

  return (dll_var) handle;
}

const char *
dll_error (dll_handle h)
{
  /* #### WTF?!  Shouldn't this at least attempt to get strerror or
     something?  --hniksic */
  return "Generic shared library error";
}

#elif defined(HAVE_DLD_INIT)
#include <dld.h>
int
dll_init (const char *arg)
{
  char *real_exe = dld_find_executable (arg);
  int rc;

  rc = dld_init (real_exe);
  if (rc)
    {
      dld_perror (exe);
      return -1;
    }
  return 0;
}

dll_handle
dll_open (const char *fname)
{
  rc = dld_link (fname);
  if (rc)
    return NULL;

  return (dll_handle) 1;
}

int
dll_close (dll_handle h)
{
  /* *sigh* DLD is pretty lame and doesn't return a handle that you can use
  ** later on to free the file - you have to remember the filename and
  ** use that as the unlinker.  We should eventually keep a linked list
  ** of loaded modules and then use the node pointer as the unique id
  ** for the shared library.  Wheeee.  But not now.
  */
  return 1;
}

DLL_FUNC
dll_function (dll_handle h, const char *n)
{
  return dld_get_func (n);
}

DLL_FUNC
dll_variable (dll_handle h, const char *n)
{
  return dld_get_symbol (n);
}
#elif defined (WIN32_NATIVE)

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN

int
dll_init (const char *arg)
{
  return 0;
}

dll_handle
dll_open (const char *fname)
{
  return (dll_handle) LoadLibrary (fname);
}

int
dll_close (dll_handle h)
{
  return FreeLibrary (h);
}

dll_func
dll_function (dll_handle h, const char *n)
{
  return (dll_func) GetProcAddress (h, n);
}

dll_func
dll_variable (dll_handle h, const char *n)
{
  return (dll_func) GetProcAddress (h, n);
}

const char *
dll_error (dll_handle h)
{
  return "Windows DLL Error";
}
#elif defined(HAVE_DYLD) 

/* 
   This section supports MacOSX dynamic libraries. Dynamically
   loadable libraries must be compiled as bundles, not dynamiclibs. 
*/

#include <mach-o/dyld.h>

int
dll_init (const char *arg)
{
  return 0;
}

dll_handle
dll_open (const char *fname)
{
  NSObjectFileImage file;
  NSObjectFileImageReturnCode ret = 
    NSCreateObjectFileImageFromFile(fname, &file);
  if (ret != NSObjectFileImageSuccess) {
    return NULL;
  }
  return (dll_handle)NSLinkModule(file, fname, 
				  NSLINKMODULE_OPTION_BINDNOW |
				  NSLINKMODULE_OPTION_PRIVATE |
				  NSLINKMODULE_OPTION_RETURN_ON_ERROR);
}

int
dll_close (dll_handle h)
{
  return NSUnLinkModule((NSModule)h, NSUNLINKMODULE_OPTION_NONE);
}

/* Given an address, return the mach_header for the image containing it
 * or zero if the given address is not contained in any loaded images.
 *
 * Note: image_for_address(), my_find_image() and search_linked_libs() are
 * based on code from the dlcompat library
 * (http://www.opendarwin.org/projects/dlcompat).
 */

static struct mach_header*
image_for_address(void *address)
{
  unsigned long i;
  unsigned long count = _dyld_image_count();
  struct mach_header *mh = 0;

  for (i = 0; i < count; i++)
    {
      unsigned long addr = (unsigned long)address -
	_dyld_get_image_vmaddr_slide(i);
      mh = _dyld_get_image_header(i);

      if (mh)
	{
	  struct load_command *lc =
	    (struct load_command *)((char *)mh + sizeof(struct mach_header));
	  unsigned long j;

	  for (j = 0; j < mh->ncmds;
	       j++, lc = (struct load_command *)((char *)lc + lc->cmdsize))
	    {
	      if (LC_SEGMENT == lc->cmd &&
		  addr >= ((struct segment_command *)lc)->vmaddr &&
		  addr <
		  ((struct segment_command *)lc)->vmaddr +
		  ((struct segment_command *)lc)->vmsize)
		{
		  goto image_found;
		}
	    }
	}

      mh = 0;
    }

 image_found:
  return mh;
}

static const struct mach_header*
my_find_image(const char *name)
{
  const struct mach_header *mh = (struct mach_header *)
    NSAddImage(name, NSADDIMAGE_OPTION_RETURN_ONLY_IF_LOADED |
	       NSADDIMAGE_OPTION_RETURN_ON_ERROR);

  if (!mh)
    {
      int count = _dyld_image_count();
      int j;

      for (j = 0; j < count; j++)
	{
	  const char *id = _dyld_get_image_name(j);

	  if (!strcmp(id, name))
	    {
	      mh = _dyld_get_image_header(j);
	      break;
	    }
	}
    }

  return mh;
}

/*
 * dyld adds libraries by first adding the directly dependant libraries in
 * link order, and then adding the dependencies for those libraries, so we
 * should do the same... but we don't bother adding the extra dependencies, if
 * the symbols are neither in the loaded image nor any of it's direct
 * dependencies, then it probably isn't there.
 */
static NSSymbol
search_linked_libs(const struct mach_header * mh, const char *symbol)
{
  int n;
  NSSymbol nssym = 0;

  struct load_command *lc =
    (struct load_command *)((char *)mh + sizeof(struct mach_header));

  for (n = 0; n < mh->ncmds;
       n++, lc = (struct load_command *)((char *)lc + lc->cmdsize))
    {
      if ((LC_LOAD_DYLIB == lc->cmd) || (LC_LOAD_WEAK_DYLIB == lc->cmd))
	{
	  struct mach_header *wh;

	  if ((wh = (struct mach_header *)
	       my_find_image((char *)(((struct dylib_command *)lc)->dylib.name.offset +
				      (char *)lc))))
	    {
	      if (NSIsSymbolNameDefinedInImage(wh, symbol))
		{
		  nssym =
		    NSLookupSymbolInImage(wh,
					  symbol,
					  NSLOOKUPSYMBOLINIMAGE_OPTION_BIND |
					  NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR);
		  break;
		}
	    }
	}
    }

  return nssym;
}

dll_func
dll_function (dll_handle h, const char *n)
{
  NSSymbol sym = 0;
#ifdef DLSYM_NEEDS_UNDERSCORE
  char *buf = alloca_array (char, strlen (n) + 2);
  *buf = '_';
  strcpy (buf + 1, n);
  n = buf;
#endif

  /* NULL means the program image and shared libraries, not bundles. */

  if (h == NULL)
    {
      /* NOTE: This assumes that this function is included in the main program
	 and not in a shared library. */
      const struct mach_header* my_mh = image_for_address(&dll_function);

      if (NSIsSymbolNameDefinedInImage(my_mh, n))
	{
	  sym =
	    NSLookupSymbolInImage(my_mh,
				  n,
				  NSLOOKUPSYMBOLINIMAGE_OPTION_BIND |
				  NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR);
	}

      if (!sym)
	{
	  sym = search_linked_libs(my_mh, n);
	}
    }
  else
    {
      sym = NSLookupSymbolInModule((NSModule)h, n);
    }

   if (sym == 0) return 0;
   return (dll_func)NSAddressOfSymbol(sym);
}

dll_var
dll_variable (dll_handle h, const char *n)
{
  NSSymbol sym;
#ifdef DLSYM_NEEDS_UNDERSCORE
  char *buf = alloca_array (char, strlen (n) + 2);
  *buf = '_';
  strcpy (buf + 1, n);
  n = buf;
#endif
  sym = NSLookupSymbolInModule((NSModule)h, n);
  if (sym == 0) return 0;
  return (dll_var)NSAddressOfSymbol(sym);
}

const char *
dll_error (dll_handle h)
{
  NSLinkEditErrors c;
  int errorNumber;
  const char *fileNameWithError, *errorString;
  NSLinkEditError(&c, &errorNumber, &fileNameWithError, &errorString);
  return errorString;
}


#else
/* Catchall if we don't know about this system's method of dynamic loading */
int
dll_init (const char *arg)
{
  return -1;
}

dll_handle
dll_open (const char *fname)
{
  return NULL;
}

int
dll_close (dll_handle h)
{
  return 0;
}

dll_func
dll_function (dll_handle h, const char *n)
{
  return NULL;
}

dll_func
dll_variable (dll_handle h, const char *n)
{
  return NULL;
}

const char *
dll_error (dll_handle h)
{
  return "Shared libraries not implemented on this system";
}
#endif /* System conditionals */

#endif /* HAVE_SHLIB */
