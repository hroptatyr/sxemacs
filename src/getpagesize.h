/*
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


/* Synched up with: FSF 19.30. */

#ifndef INCLUDED_getpagesize_h_
#define INCLUDED_getpagesize_h_

/* Emulate getpagesize on systems that lack it.  */

#if 0
#ifdef __hpux
#include <sys/types.h>
static size_t getpagesize()
{
	return (4096);
}

#define HAVE_GETPAGESIZE
#endif
#endif

#ifndef HAVE_GETPAGESIZE

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef _SC_PAGESIZE
#define getpagesize() sysconf(_SC_PAGESIZE)
#else

#include <sys/param.h>

#ifdef EXEC_PAGESIZE
#define getpagesize() EXEC_PAGESIZE
#else
#ifdef NBPG
#define getpagesize() NBPG * CLSIZE
#ifndef CLSIZE
#define CLSIZE 1
#endif				/* no CLSIZE */
#else				/* no NBPG */
#if (defined (sparc) && defined (USG)) || defined (SOLARIS2)
#define getpagesize() PAGESIZE
#else				/* not Solaris 2 */
#ifdef NBPC
#define getpagesize() NBPC
#else				/* no NBPC */
#ifdef PAGESIZE
#define getpagesize() PAGESIZE
#endif
#endif				/* NBPC */
#endif				/* not Solaris 2 */
#endif				/* no NBPG */
#endif				/* no EXEC_PAGESIZE */
#endif				/* _SC_PAGESIZE */
#endif				/* not HAVE_GETPAGESIZE */

#endif				/* INCLUDED_getpagesize_h_ */
