/* Heap management routines (including unexec) for XEmacs on Windows NT.
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

#ifndef INCLUDED_ntheap_h_
#define INCLUDED_ntheap_h_

#include "syswindows.h"

/*
 * Heap related stuff.
 */
#define get_reserved_heap_size()	reserved_heap_size
#define get_committed_heap_size()	(get_data_end () - get_data_start ())
#define get_heap_start()		get_data_start ()
#define get_heap_end()			get_data_end ()
#define get_page_size()			sysinfo_cache.dwPageSize
#define get_allocation_unit()		sysinfo_cache.dwAllocationGranularity
#define get_processor_type()		sysinfo_cache.dwProcessorType
#define get_nt_major_version()  	nt_major_version
#define get_nt_minor_version()  	nt_minor_version

extern unsigned char *get_data_start();
extern unsigned char *get_data_end();
extern unsigned long  data_region_size;
extern unsigned long  reserved_heap_size;
extern SYSTEM_INFO    sysinfo_cache;
extern int    	      nt_major_version;
extern int    	      nt_minor_version;

/* To prevent zero-initialized variables from being placed into the bss
   section, use non-zero values to represent an uninitialized state.  */
#define UNINIT_PTR ((unsigned char*) 0xF0A0F0A0)
#define UNINIT_LONG (0xF0A0F0A0L)

/* Emulation of Unix sbrk().  */
extern void *sbrk (unsigned long size);

/* Recreate the heap created during dumping.  */
extern void recreate_heap (char *executable_path);

/* Round the heap to this size.  */
extern void round_heap (unsigned long size);

/* Load in the dumped .bss section.  */
extern void read_in_bss (char *name);

/* Map in the dumped heap.  */
extern void map_in_heap (char *name);

/* Cache system info, e.g., the NT page size.  */
extern void cache_system_info (void);

/* Round ADDRESS up to be aligned with ALIGN.  */
extern unsigned char *round_to_next (unsigned char *address, 
				     unsigned long align);

#endif /* INCLUDED_ntheap_h_ */
