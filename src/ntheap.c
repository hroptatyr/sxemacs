/* Heap management routines for XEmacs on Windows NT.
   Copyright (C) 1994 Free Software Foundation, Inc.

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
02111-1307, USA.

   Geoff Voelker (voelker@cs.washington.edu) 7-29-94 */

/* Adapted for XEmacs by David Hobley <david@spook-le0.cia.com.au> */
/* Synced with FSF Emacs 19.34.6 by Marc Paquette <marcpa@cam.org> */

#include <config.h>
#include "lisp.h"  /* for VALMASK */

#include <stdlib.h>

#include "ntheap.h"

/* This gives us the page size and the size of the allocation unit on NT.  */
SYSTEM_INFO sysinfo_cache;
unsigned long syspage_mask = 0;

/* These are defined to get Emacs to compile, but are not used.  */
int edata;
int etext;

/* Cache information describing the NT system for later use.  */
void
cache_system_info (void)
{
  /* Cache page size, allocation unit, processor type, etc.  */
  GetSystemInfo (&sysinfo_cache);
  syspage_mask = sysinfo_cache.dwPageSize - 1;
}

/* Round ADDRESS up to be aligned with ALIGN.  */
unsigned char *
round_to_next (unsigned char *address, unsigned long align)
{
  unsigned long tmp;

  tmp = (unsigned long) address;
  tmp = (tmp + align - 1) / align;

  return (unsigned char *) (tmp * align);
}

/* Info for keeping track of our heap.  */
unsigned char *data_region_base = UNINIT_PTR;
unsigned char *data_region_end = UNINIT_PTR;
unsigned char *real_data_region_end = UNINIT_PTR;
unsigned long  data_region_size = UNINIT_LONG;
unsigned long  reserved_heap_size = UNINIT_LONG;

/* The start of the data segment.  */
unsigned char *
get_data_start (void)
{
  return data_region_base;
}

/* The end of the data segment.  */
unsigned char *
get_data_end (void)
{
  return data_region_end;
}

static unsigned char *
allocate_heap (void)
{
  /* The base address for our GNU malloc heap is chosen in conjunction
     with the link settings for temacs.exe which control the stack size,
     the initial default process heap size and the executable image base
     address.  The link settings and the malloc heap base below must all
     correspond; the relationship between these values depends on how NT
     and Win95 arrange the virtual address space for a process (and on
     the size of the code and data segments in temacs.exe).

     The most important thing is to make base address for the executable
     image high enough to leave enough room between it and the 4MB floor
     of the process address space on Win95 for the primary thread stack,
     the process default heap, and other assorted odds and ends
     (eg. environment strings, private system dll memory etc) that are
     allocated before temacs has a chance to grab its malloc arena.  The
     malloc heap base can then be set several MB higher than the
     executable image base, leaving enough room for the code and data
     segments.

     Because some parts of Emacs can use rather a lot of stack space
     (for instance, the regular expression routines can potentially
     allocate several MB of stack space) we allow 8MB for the stack.

     Allowing 1MB for the default process heap, and 1MB for odds and
     ends, we can base the executable at 16MB and still have a generous
     safety margin.  At the moment, the executable has about 810KB of
     code (for x86) and about 550KB of data - on RISC platforms the code
     size could be roughly double, so if we allow 4MB for the executable
     we will have plenty of room for expansion.

     Thus we would like to set the malloc heap base to 20MB.  However,
     Win95 refuses to allocate the heap starting at this address, so we
     set the base to 27MB to make it happy.  Since Emacs now leaves
     28 bits available for pointers, this lets us use the remainder of
     the region below the 256MB line for our malloc arena - 229MB is
     still a pretty decent arena to play in!  */

  unsigned long base = 0x01B00000;   /*  27MB */
  /* Temporary hack for the non-starting problem - use 28 (256Mb) rather than VALBITS (1Gb) */
  unsigned long end  = 1 << 28;      /* 256MB */
  void *ptr = NULL;

#define NTHEAP_PROBE_BASE 1
#if NTHEAP_PROBE_BASE /* This is never normally defined */
  /* Try various addresses looking for one the kernel will let us have.  */
  while (!ptr && (base < end))
    {
      reserved_heap_size = end - base;
      ptr = VirtualAlloc ((void *) base,
			  get_reserved_heap_size (),
			  MEM_RESERVE,
			  PAGE_NOACCESS);
      base += 0x00100000;  /* 1MB increment */
    }
#else
  reserved_heap_size = end - base;
  ptr = VirtualAlloc ((void *) base,
		      get_reserved_heap_size (),
		      MEM_RESERVE,
		      PAGE_NOACCESS);
#endif

  return (unsigned char*) ptr;
}


/* Emulate Unix sbrk.  */
void *
sbrk (unsigned long increment)
{
  void *result;
  long size = (long) increment;
  
  /* Allocate our heap if we haven't done so already.  */
  if (data_region_base == UNINIT_PTR) 
    {
      data_region_base = allocate_heap ();
      if (!data_region_base)
	return NULL;

      data_region_end = data_region_base;
      real_data_region_end = data_region_end;
      data_region_size = get_reserved_heap_size ();
    }
  
  result = data_region_end;
  
  /* If size is negative, shrink the heap by decommitting pages.  */
  if (size < 0) 
    {
      int new_size;
      unsigned char *new_data_region_end;

      size = -size;

      /* Sanity checks.  */
      if ((data_region_end - size) < data_region_base)
	return NULL;

      /* We can only decommit full pages, so allow for 
	 partial deallocation [cga].  */
      new_data_region_end = (data_region_end - size);
      new_data_region_end = (unsigned char *)
	((long) (new_data_region_end + syspage_mask) & ~syspage_mask);
      new_size = real_data_region_end - new_data_region_end;
      real_data_region_end = new_data_region_end;
      if (new_size > 0) 
	{
	  /* Decommit size bytes from the end of the heap.  */
	  if (!VirtualFree (real_data_region_end, new_size, MEM_DECOMMIT))
	    return NULL;
 	}

      data_region_end -= size;
    } 
  /* If size is positive, grow the heap by committing reserved pages.  */
  else if (size > 0) 
    {
      /* Sanity checks.  */
      if ((data_region_end + size) >
	  (data_region_base + get_reserved_heap_size ()))
	return NULL;

      /* Commit more of our heap. */
      if (VirtualAlloc (data_region_end, size, MEM_COMMIT,
			PAGE_READWRITE) == NULL)
	return NULL;
      data_region_end += size;

      /* We really only commit full pages, so record where
	 the real end of committed memory is [cga].  */
      real_data_region_end = (unsigned char *)
	  ((long) (data_region_end + syspage_mask) & ~syspage_mask);
    }
  
  return result;
}

#if !defined (CANNOT_DUMP) && !defined(HEAP_IN_DATA) && !defined(PDUMP)

/* Recreate the heap from the data that was dumped to the executable.
   EXECUTABLE_PATH tells us where to find the executable.  */
void
recreate_heap (char *executable_path)
{
  /* First reserve the upper part of our heap.  (We reserve first
	 because there have been problems in the past where doing the
	 mapping first has loaded DLLs into the VA space of our heap.)  */

  /* Query the region at the end of the committed heap */
  void *tmp;
  MEMORY_BASIC_INFORMATION info;
  DWORD size;
  unsigned char* base = get_heap_end ();
  unsigned char* end  = base + get_reserved_heap_size () - get_committed_heap_size ();
  VirtualQuery (base, &info, sizeof info);
  if (info.State != MEM_FREE)
	{
	  /* Oops, something has already reserved or commited it, nothing we can do but exit */
	  char buf[256];
	  wsprintf(buf,
			   "XEmacs cannot start because the memory region required by the heap is not available.\n"
			   "(BaseAddress = 0x%lx, AllocationBase = 0x%lx, Size = 0x%lx, State = %s, Type = %s)",
			   info.BaseAddress, info.AllocationBase, info.RegionSize,
			   info.State == MEM_COMMIT ? "COMMITED" : "RESERVED",
			   info.Type == MEM_IMAGE ? "IMAGE" : info.Type == MEM_MAPPED ? "MAPPED" : "PRIVATE");
	  MessageBox(NULL, buf, "XEmacs", MB_OK | MB_ICONSTOP);
	  exit(1);
	}

  /* Now try and reserve as much as possible */
  size = min (info.RegionSize, end - base);
  tmp = VirtualAlloc (base, size, MEM_RESERVE, PAGE_NOACCESS);
  if (!tmp)
	{
	  /* Can't reserve it, nothing we can do but exit */
	  char buf[256];
	  wsprintf(buf,
			   "XEmacs cannot start because it couldn't reserve space required for the heap.\n"
			   "(VirtualAlloc at 0x%lx of 0x%lx failed (%d))", base, size, GetLastError());
	  MessageBox(NULL, buf, "XEmacs", MB_OK | MB_ICONSTOP);
	  exit (1);
	}

  /* We read in the data for the .bss section from the executable
     first and map in the heap from the executable second to prevent
     any funny interactions between file I/O and file mapping.  */
  read_in_bss (executable_path);
  map_in_heap (executable_path);

  /* Update system version information to match current system.  */
  cache_system_info ();
}
#endif /* CANNOT_DUMP */

/* Round the heap up to the given alignment.  */
void
round_heap (unsigned long align)
{
  unsigned long needs_to_be;
  unsigned long need_to_alloc;
  
  needs_to_be = (unsigned long) round_to_next (get_heap_end (), align);
  need_to_alloc = needs_to_be - (unsigned long) get_heap_end ();
  
  if (need_to_alloc) 
    sbrk (need_to_alloc);
}

#if ((_MSC_VER >= 1000) && (_MSC_VER < 1300))

/* MSVC 4.2 invokes these functions from mainCRTStartup to initialize
   a heap via HeapCreate.  They are normally defined by the runtime,
   but we override them here so that the unnecessary HeapCreate call
   is not performed.  */

/* MSVC 7.0 does not allow you to redefine _heap_init or _heap_term. */

int __cdecl
_heap_init (void)
{
  /* Stepping through the assembly indicates that mainCRTStartup is
     expecting a nonzero success return value.  */
  return 1;
}

void __cdecl
_heap_term (void)
{
  return;
}

#endif
