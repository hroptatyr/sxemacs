/* Efficient caching of X GCs (graphics contexts).
   Copyright (C) 1993 Free Software Foundation, Inc.


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

/* Written by jwz, 14 jun 93 */
/* Hacked by wmperry, apr 2000 */

#ifndef _GCCACHE_GTK_H_
#define _GCCACHE_GTK_H_

struct gc_cache;
struct gc_cache *make_gc_cache (GtkWidget *);
void free_gc_cache (struct gc_cache *cache);
GdkGC *gc_cache_lookup (struct gc_cache *, GdkGCValues *, GdkGCValuesMask mask);

#endif /* _XGCCACHE_H_ */
