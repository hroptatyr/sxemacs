/* Copyright (C) 1991 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

/* Synched up with: Not in FSF. */

# include <config.h>
# ifndef REGISTER	/* Strictly enforced in 20.3 */
# define REGISTER
# endif

/* In HPUX 10 the strcat function references memory past the last byte of 
   the string!  This will core dump if the memory following the last byte is 
   not mapped.

   Here is a correct version from, glibc 1.09.
*/

char *strcat (char *dest, const char *src);

/* Append SRC on the end of DEST.  */
char *
strcat (char *dest, const char *src)
{
  REGISTER char *s1 = dest;
  REGISTER const char *s2 = src;
  char c;

  /* Find the end of the string.  */
  do
    c = *s1++;
  while (c != '\0');

  /* Make S1 point before the next character, so we can increment
     it while memory is read (wins on pipelined cpus).  */
  s1 -= 2;

  do
    {
      c = *s2++;
      *++s1 = c;
    }
  while (c != '\0');

  return dest;
}
