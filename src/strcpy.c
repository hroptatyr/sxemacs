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

/* In SunOS 4.1.1 the strcpy function references memory past the last byte of
   the string!  This will core dump if the memory following the last byte is
   not mapped.

   Here are correct versions by hbs@lucid.com.
*/

# include <config.h>
# ifndef REGISTER		/* Strictly enforced in 20.3 */
# define REGISTER
# endif

#define ALIGNED(x) (!(((unsigned long) (x)) & (sizeof (unsigned long) - 1)))

#define MAGIC    0x7efefeff
#define HIGH_BIT_P(c) ((c) & hi_bit)
#define HAS_ZERO(c) (((((c) + magic) ^ (c)) & not_magic) != not_magic)

char *strcpy(char *to, const char *from)
{
	char *return_value = to;
	if (to == from)
		return to;
	else if (ALIGNED(to) && ALIGNED(from)) {
		unsigned long *to1 = (unsigned long *)to;
		const unsigned long *from1 = (const unsigned long *)from;
		unsigned long c;
		unsigned long magic = MAGIC;
		unsigned long not_magic = ~magic;
/*      unsigned long hi_bit = 0x80000000; */

		while ((c = *from1) != 0) {
			if (HAS_ZERO(c)) {
				to = (char *)to1;
				from = (const char *)from1;
				goto slow_loop;
			} else {
				*to1 = c;
				to1++;
				from1++;
			}
		}

		to = (char *)to1;
		*to = (char)0;
		return return_value;
	} else {
		char c;

	      slow_loop:

		while ((c = *from) != 0) {
			*to = c;
			to++;
			from++;
		}
		*to = (char)0;
	}
	return return_value;
}
