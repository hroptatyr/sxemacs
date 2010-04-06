/* Efficient caching of X GCs (graphics contexts).
   Copyright (C) 1993 Free Software Foundation, Inc.

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


/* Synched up with: Not in FSF. */

/* Written by jwz, 14 jun 93 */

#ifndef INCLUDED_xgccache_h_
#define INCLUDED_xgccache_h_

struct gc_cache;
struct gc_cache *make_gc_cache(Display *, Window);
void free_gc_cache(struct gc_cache *cache);
GC gc_cache_lookup(struct gc_cache *, XGCValues *, unsigned long mask);

#endif				/* INCLUDED_xgccache_h_ */
