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


/* Synched up with: Not in FSF. */

/* If you are using SunOS 4.1.1 and X11r5, then you need this patch.
   There is a stupid bug in the SunOS libc.a: two functions which X11r5
   uses, mbstowcs() and wcstombs(), are unusable when programs are
   statically linked (as Emacs must be) because the static version of
   libc.a contains the *dynamic* versions of these functions.  These
   functions don't seem to be called when Emacs is running, so it's
   enough to define stubs for them.

   This appears to be fixed in SunOS 4.1.2.
 */

#include <config.h>

#ifndef I18N4			/* we actually need these from the library in this case. */

#ifdef __STDC__

#include <stdlib.h>

size_t mbstowcs(wchar_t * foo, const char *bar, size_t baz)
{
	abort();
	return 0;
}

size_t wcstombs(char *foo, const wchar_t * bar, size_t baz)
{
	abort();
	return 0;
}

#else

void mbstowcs()
{
	abort();
}

void wcstombs()
{
	abort();
}

#endif				/* __STDC__ */

#endif				/* !I18N4 */
