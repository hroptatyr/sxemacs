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

#ifndef INCLUDED_iso_wide_h_
#define INCLUDED_iso_wide_h_

/* The following macros are designed for SunOS 5.0 wide characters,
   in which the single byte ISO Latin-1 character 1xxxxxxx are represented

		00110000 00000000 00000000 0xxxxxxx

   For wide character systems which maintain the numeric value of all
   single-byte characters, IN_TABLE_DOMAIN can simply be defined

		(0 <= (c) && (c) <= 0xff)

   and no funky ISO_WIDE_TO_BYTE conversions are needed. */

/* Can't use isascii() because we want wide char argument */
#define IS_ASCII(c)     (0 <= (c) && (c) <= 0x7f)

#define IS_ISO_WIDE(c)  (0x30000000 <= (c) && (c) <= 0x3000007f)
#define IS_ISO_BYTE(c)  (0x80 <= (c) && (c) <= 0xff)

#define IN_TABLE_DOMAIN(c)  (IS_ASCII (c) || IS_ISO_WIDE (c))

#define ISO_WIDE_TO_BYTE(c)  ((c) & 0x0000007f | 0x80)
#define ISO_BYTE_TO_WIDE(c)  ((c) & 0x7f | 0x30000000)

#define WIDE_TO_BYTE(c)  (IS_ISO_WIDE (c) ? ISO_WIDE_TO_BYTE (c) : (c))
#define BYTE_TO_WIDE(c)  (IS_ISO_BYTE (c) ? ISO_BYTE_TO_WIDE (c) : (c))

#endif				/* INCLUDED_iso_wide_h_ */
