/* This file is part of XEmacs.

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

/* In SunOS 4.1.1 the strcmp and strncmp functions reference memory
   past the last byte of the string! This will core dump if the memory 
   following the last byte is not mapped.

   Here are correct versions by hbs@lucid.com.
*/

# include <config.h>
# ifndef REGISTER	/* Strictly enforced in 20.3 */
# define REGISTER
# endif

#include <string.h>
#define ALIGNED(x) (!(((unsigned long) (x)) & (sizeof (unsigned long) - 1)))

#define MAGIC    0x7efefeff
#define HIGH_BIT_P(c) ((c) & hi_bit)
#define HAS_ZERO(c) (((((c) + magic) ^ (c)) & not_magic) != not_magic)

int
strcmp (const char *x, const char *y)
{
  if (x == y)
    return 0;
  else if (ALIGNED (x) && ALIGNED (y))
    {
      const unsigned long *x1 = (const unsigned long *) x;
      const unsigned long *y1 = (const unsigned long *) y;
      unsigned long c;
      unsigned long magic = MAGIC;
      unsigned long not_magic = ~magic;
      unsigned long hi_bit = 0x80000000;

      while ((c = *x1) == *y1)
        {
          if (HAS_ZERO(c)) 
            {
              if (!HIGH_BIT_P (c))
                return 0;
              else
                {
                  x = (const char *) x1;
                  y = (const char *) y1;
                  goto slow_loop;
                }
            }
              
          x1++; 
          y1++;
        }

      x = (const char *) x1;
      y = (const char *) y1;
      goto slow_loop;
    }
  else
    {
      char c;

    slow_loop:

      while ((c = *x) == *y)
        {
          if (c == (char) 0) return 0;
          x++; 
          y++;
        }
      return (*x - *y);
    }
}


int
strncmp (const char *x, const char *y, size_t n)
{
  if ((x == y) || (n <= 0))
    return 0;
  else if (ALIGNED (x) && ALIGNED (y))
    {
      const unsigned long *x1 = (const unsigned long *) x;
      const unsigned long *y1 = (const unsigned long *) y;
      unsigned long c;
      unsigned long magic = MAGIC;
      unsigned long not_magic = ~magic;
      unsigned long hi_bit = 0x80000000;

      while ((c = *x1) == *y1)
        {
          n -= sizeof (unsigned long);
          if (n <= 0)
            return 0;
          
          if (HAS_ZERO(c)) 
            {
              if (!HIGH_BIT_P (c))
                return 0;
              else
                {
                  x = (const char *) x1;
                  y = (const char *) y1;
                  goto slow_loop;
                }
            }
              
          x1++; 
          y1++;
        }

      x = (const char *) x1;
      y = (const char *) y1;
      goto slow_loop;
    }
  else
    {
      char c;

    slow_loop:

      while ((c = *x) == *y)
        {
          n--;
          if (n <= 0)
            return 0;
          if (c == (char) 0) 
            return 0;
          x++; 
          y++;
        }
      return (*x - *y);
    }
}
