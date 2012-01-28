/* Buffer insertion/deletion and gap motion for SXEmacs.
   Copyright (C) 1985, 1986, 1991, 1992, 1993, 1994, 1995
   Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.

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


/* Synched up with: Mule 2.0, FSF 19.30.  Diverges significantly. */

/* This file has been Mule-ized. */

/* Overhauled by Ben Wing, December 1994, for Mule implementation. */

/*
   There are three possible ways to specify positions in a buffer.  All
   of these are one-based: the beginning of the buffer is position or
   index 1, and 0 is not a valid position.

   As a "buffer position" (typedef Bufpos):

      This is an index specifying an offset in characters from the
      beginning of the buffer.  Note that buffer positions are
      logically *between* characters, not on a character.  The
      difference between two buffer positions specifies the number of
      characters between those positions.  Buffer positions are the
      only kind of position externally visible to the user.

   As a "byte index" (typedef Bytind):

      This is an index over the bytes used to represent the characters
      in the buffer.  If there is no Mule support, this is identical
      to a buffer position, because each character is represented
      using one byte.  However, with Mule support, many characters
      require two or more bytes for their representation, and so a
      byte index may be greater than the corresponding buffer
      position.

   As a "memory index" (typedef Memind):

      This is the byte index adjusted for the gap.  For positions
      before the gap, this is identical to the byte index.  For
      positions after the gap, this is the byte index plus the gap
      size.  There are two possible memory indices for the gap
      position; the memory index at the beginning of the gap should
      always be used, except in code that deals with manipulating the
      gap, where both indices may be seen.  The address of the
      character "at" (i.e. following) a particular position can be
      obtained from the formula

	buffer_start_address + memory_index(position) - 1

      except in the case of characters at the gap position.

   Other typedefs:
   ===============

      Emchar:
      -------
	This typedef represents a single Emacs character, which can be
	ASCII, ISO-8859, or some extended character, as would typically
	be used for Kanji.  Note that the representation of a character
	as an Emchar is *not* the same as the representation of that
	same character in a string; thus, you cannot do the standard
	C trick of passing a pointer to a character to a function that
	expects a string.

	An Emchar takes up 19 bits of representation and (for code
	compatibility and such) is compatible with an int.  This
	representation is visible on the Lisp level.  The important
	characteristics	of the Emchar representation are

	  -- values 0x00 - 0x7f represent ASCII.
	  -- values 0x80 - 0xff represent the right half of ISO-8859-1.
	  -- values 0x100 and up represent all other characters.

	This means that Emchar values are upwardly compatible with
	the standard 8-bit representation of ASCII/ISO-8859-1.

      Bufbyte:
      --------
	The data in a buffer or string is logically made up of Bufbyte
	objects, where a Bufbyte takes up the same amount of space as a
	char. (It is declared differently, though, to catch invalid
	usages.) Strings stored using Bufbytes are said to be in
	"internal format".  The important characteristics of internal
	format are

	  -- ASCII characters are represented as a single Bufbyte,
	     in the range 0 - 0x7f.
	  -- All other characters are represented as a Bufbyte in
	     the range 0x80 - 0x9f followed by one or more Bufbytes
	     in the range 0xa0 to 0xff.

	This leads to a number of desirable properties:

	  -- Given the position of the beginning of a character,
	     you can find the beginning of the next or previous
	     character in constant time.
	  -- When searching for a substring or an ASCII character
	     within the string, you need merely use standard
	     searching routines.

      array of char:
      --------------
	Strings that go in or out of Emacs are in "external format",
	typedef'ed as an array of char or a char *.  There is more
	than one external format (JIS, EUC, etc.) but they all
	have similar properties.  They are modal encodings,
	which is to say that the meaning of particular bytes is
	not fixed but depends on what "mode" the string is currently
	in (e.g. bytes in the range 0 - 0x7f might be
	interpreted as ASCII, or as Hiragana, or as 2-byte Kanji,
	depending on the current mode).  The mode starts out in
	ASCII/ISO-8859-1 and is switched using escape sequences --
	for example, in the JIS encoding, 'ESC $ B' switches to a
	mode where pairs of bytes in the range 0 - 0x7f
	are interpreted as Kanji characters.

	External-formatted data is generally desirable for passing
	data between programs because it is upwardly compatible
	with standard ASCII/ISO-8859-1 strings and may require
	less space than internal encodings such as the one
	described above.  In addition, some encodings (e.g. JIS)
	keep all characters (except the ESC used to switch modes)
	in the printing ASCII range 0x20 - 0x7e, which results in
	a much higher probability that the data will avoid being
	garbled in transmission.  Externally-formatted data is
	generally not very convenient to work with, however, and
	for this reason is usually converted to internal format
	before any work is done on the string.

	NOTE: filenames need to be in external format so that
	ISO-8859-1 characters come out correctly.

      Charcount:
      ----------
	This typedef represents a count of characters, such as
	a character offset into a string or the number of
	characters between two positions in a buffer.  The
	difference between two Bufpos's is a Charcount, and
	character positions in a string are represented using
	a Charcount.

      Bytecount:
      ----------
	Similar to a Charcount but represents a count of bytes.
	The difference between two Bytind's is a Bytecount.

   Usage of the various representations:
   =====================================

   Memory indices are used in low-level functions in insdel.c and for
   extent endpoints and marker positions.  The reason for this is that
   this way, the extents and markers don't need to be updated for most
   insertions, which merely shrink the gap and don't move any
   characters around in memory.

   (The beginning-of-gap memory index simplifies insertions w.r.t.
   markers, because text usually gets inserted after markers.  For
   extents, it is merely for consistency, because text can get
   inserted either before or after an extent's endpoint depending on
   the open/closedness of the endpoint.)

   Byte indices are used in other code that needs to be fast,
   such as the searching, redisplay, and extent-manipulation code.

   Buffer positions are used in all other code.  This is because this
   representation is easiest to work with (especially since Lisp
   code always uses buffer positions), necessitates the fewest
   changes to existing code, and is the safest (e.g. if the text gets
   shifted underneath a buffer position, it will still point to a
   character; if text is shifted under a byte index, it might point
   to the middle of a character, which would be bad).

   Similarly, Charcounts are used in all code that deals with strings
   except for code that needs to be fast, which used Bytecounts.

   Strings are always passed around internally using internal format.
   Conversions between external format are performed at the time
   that the data goes in or out of Emacs.

   Working with the various representations:
   ========================================= */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "device.h"
#include "frame.h"
#include "extents.h"
#include "insdel.h"
#include "lstream.h"
#include "redisplay.h"
#include "line-number.h"

/* We write things this way because it's very important the
   MAX_BYTIND_GAP_SIZE_3 is a multiple of 3. (As it happens,
   65535 is a multiple of 3, but this may not always be the
   case.) */

#define MAX_BUFPOS_GAP_SIZE_3 (65535/3)
#define MAX_BYTIND_GAP_SIZE_3 (3 * MAX_BUFPOS_GAP_SIZE_3)

short three_to_one_table[1 + MAX_BYTIND_GAP_SIZE_3];

/* Various macros modelled along the lines of those in buffer.h.
   Purposefully omitted from buffer.h because files other than this
   one should not be using them. */

/* Address of beginning of buffer.  This is an lvalue because
   BUFFER_ALLOC needs it to be. */
#define BUF_BEG_ADDR(buf) ((buf)->text->beg)

/* Set the address of beginning of buffer. */
#define SET_BUF_BEG_ADDR(buf, addr) do { (buf)->text->beg = (addr); } while (0)

/* Gap size.  */
#define BUF_GAP_SIZE(buf) ((buf)->text->gap_size + 0)
#define BUF_END_GAP_SIZE(buf) ((buf)->text->end_gap_size + 0)
/* Set gap size.  */
#define SET_BUF_GAP_SIZE(buf, value) \
  do { (buf)->text->gap_size = (value); } while (0)
#define SET_BUF_END_GAP_SIZE(buf, value) \
  do { (buf)->text->end_gap_size = (value); } while (0)

/* Gap location.  */
#define BI_BUF_GPT(buf) ((buf)->text->gpt + 0)
#define BUF_GPT_ADDR(buf) (BUF_BEG_ADDR (buf) + BI_BUF_GPT (buf) - 1)

/* Set gap location.  */
#define SET_BI_BUF_GPT(buf, value) do { (buf)->text->gpt = (value); } while (0)

/* Set end of buffer.  */
#define SET_BOTH_BUF_Z(buf, val, bival)		\
do						\
{						\
  (buf)->text->z = (bival);			\
  (buf)->text->bufz = (val);			\
} while (0)

/* Under Mule, we maintain two sentinels in the buffer: one at the
   beginning of the gap, and one at the end of the buffer.  This
   allows us to move forward, examining bytes looking for the
   end of a character, and not worry about running off the end.
   We do not need corresponding sentinels when moving backwards
   because we do not have to look past the beginning of a character
   to find the beginning of the character.

   Every time we change the beginning of the gap, we have to
   call SET_GAP_SENTINEL().

   Every time we change the total size (characters plus gap)
   of the buffer, we have to call SET_END_SENTINEL().
 */

#ifdef MULE
# define GAP_CAN_HOLD_SIZE_P(buf, len) (BUF_GAP_SIZE (buf) >= (len) + 1)
# define SET_GAP_SENTINEL(buf) (*BUF_GPT_ADDR (buf) = 0)
# define BUF_END_SENTINEL_SIZE 1
# define SET_END_SENTINEL(buf) \
  (*(BUF_BEG_ADDR (buf) + BUF_GAP_SIZE (buf) + BI_BUF_Z (buf) - 1) = 0)
#else
# define GAP_CAN_HOLD_SIZE_P(buf, len) (BUF_GAP_SIZE (buf) >= (len))
# define SET_GAP_SENTINEL(buf)
# define BUF_END_SENTINEL_SIZE 0
# define SET_END_SENTINEL(buf)
#endif

/************************************************************************/
/*                    Charcount/Bytecount conversion                    */
/************************************************************************/

/* Optimization.  Do it.  Live it.  Love it.  */

#ifdef MULE

/* We include the basic functions here that require no specific
   knowledge of how data is Mule-encoded into a buffer other
   than the basic (00 - 7F), (80 - 9F), (A0 - FF) scheme.
   Anything that requires more specific knowledge goes into
   mule-charset.c. */

/* Given a pointer to a text string and a length in bytes, return
   the equivalent length in characters. */

Charcount bytecount_to_charcount(const Bufbyte * ptr, Bytecount len)
{
	Charcount count = 0;
	const Bufbyte *end = ptr + len;

#if SIZEOF_LONG == 8
# define STRIDE_TYPE long
# define HIGH_BIT_MASK 0x8080808080808080UL
#elif SIZEOF_LONG_LONG_INT == 8 && !(defined (i386) || defined (__i386__))
# define STRIDE_TYPE long long
# define HIGH_BIT_MASK 0x8080808080808080ULL
#elif SIZEOF_LONG == 4
# define STRIDE_TYPE long
# define HIGH_BIT_MASK 0x80808080UL
#else
# error Add support for 128-bit systems here
#endif

#define ALIGN_BITS ((EMACS_UINT) (ALIGNOF (STRIDE_TYPE) - 1))
#define ALIGN_MASK (~ ALIGN_BITS)
#define ALIGNED(ptr) ((((EMACS_UINT) ptr) & ALIGN_BITS) == 0)
#define STRIDE sizeof (STRIDE_TYPE)

	while (ptr < end) {
		if (BYTE_ASCII_P(*ptr)) {
			/* optimize for long stretches of ASCII */
			if (!ALIGNED(ptr)) {
				ptr++, count++;
			} else {
				const unsigned STRIDE_TYPE *ascii_end =
					(const unsigned STRIDE_TYPE *)ptr;
				/* This loop screams, because we can typically
				   detect ASCII characters 8 at a time. */
				while ((const Bufbyte *)ascii_end + STRIDE <=
				       end && !(*ascii_end & HIGH_BIT_MASK))
					ascii_end++;
				if ((const Bufbyte *) ascii_end == ptr) {
					ptr++, count++;
				} else {
					count += (const Bufbyte*)
						ascii_end - ptr;
					ptr = (const Bufbyte*)ascii_end;
				}
			}
		} else {
			/* optimize for successive characters from the same charset */
			Bufbyte leading_byte = *ptr;
			size_t bytes = REP_BYTES_BY_FIRST_BYTE(leading_byte);
			while ((ptr < end) && (*ptr == leading_byte))
				ptr += bytes, count++;
		}
	}

#ifdef ERROR_CHECK_BUFPOS
	/* Bomb out if the specified substring ends in the middle
	   of a character.  Note that we might have already gotten
	   a core dump above from an invalid reference, but at least
	   we will get no farther than here. */
	assert(ptr == end);
#endif

	return count;
}

/* Given a pointer to a text string and a length in characters, return
   the equivalent length in bytes. */

Bytecount charcount_to_bytecount(const Bufbyte * ptr, Charcount len)
{
	const Bufbyte *newptr = ptr;

	while (len > 0) {
		INC_CHARPTR(newptr);
		len--;
	}
	return newptr - ptr;
}

/* The next two functions are the actual meat behind the
   bufpos-to-bytind and bytind-to-bufpos conversions.  Currently
   the method they use is fairly unsophisticated; see buffer.h.

   Note that bufpos_to_bytind_func() is probably the most-called
   function in all of SXEmacs.  Therefore, it must be FAST FAST FAST.
   This is the reason why so much of the code is duplicated.

   Similar considerations apply to bytind_to_bufpos_func(), although
   less so because the function is not called so often.

   #### At some point this should use a more sophisticated method;
   see buffer.h. */

static int not_very_random_number;

Bytind bufpos_to_bytind_func(struct buffer *buf, Bufpos x)
{
	Bufpos bufmin;
	Bufpos bufmax;
	Bytind bytmin;
	Bytind bytmax;
	int size;
	int forward_p;
	Bytind retval;
	int diff_so_far;
	int add_to_cache = 0;

	/* Check for some cached positions, for speed. */
	if (x == BUF_PT(buf))
		return BI_BUF_PT(buf);
	if (x == BUF_ZV(buf))
		return BI_BUF_ZV(buf);
	if (x == BUF_BEGV(buf))
		return BI_BUF_BEGV(buf);

	bufmin = buf->text->mule_bufmin;
	bufmax = buf->text->mule_bufmax;
	bytmin = buf->text->mule_bytmin;
	bytmax = buf->text->mule_bytmax;
	size = (1 << buf->text->mule_shifter) + !!buf->text->mule_three_p;

	/* The basic idea here is that we shift the "known region" up or down
	   until it overlaps the specified position.  We do this by moving
	   the upper bound of the known region up one character at a time,
	   and moving the lower bound of the known region up as necessary
	   when the size of the character just seen changes.

	   We optimize this, however, by first shifting the known region to
	   one of the cached points if it's close by. (We don't check BEG or
	   Z, even though they're cached; most of the time these will be the
	   same as BEGV and ZV, and when they're not, they're not likely
	   to be used.) */

	if (x > bufmax) {
		Bufpos diffmax = x - bufmax;
		Bufpos diffpt = x - BUF_PT(buf);
		Bufpos diffzv = BUF_ZV(buf) - x;
		/* #### This value could stand some more exploration. */
		Charcount heuristic_hack = (bufmax - bufmin) >> 2;

		/* Check if the position is closer to PT or ZV than to the
		   end of the known region. */

		if (diffpt < 0)
			diffpt = -diffpt;
		if (diffzv < 0)
			diffzv = -diffzv;

		/* But also implement a heuristic that favors the known region
		   over PT or ZV.  The reason for this is that switching to
		   PT or ZV will wipe out the knowledge in the known region,
		   which might be annoying if the known region is large and
		   PT or ZV is not that much closer than the end of the known
		   region. */

		diffzv += heuristic_hack;
		diffpt += heuristic_hack;
		if (diffpt < diffmax && diffpt <= diffzv) {
			bufmax = bufmin = BUF_PT(buf);
			bytmax = bytmin = BI_BUF_PT(buf);
			/* We set the size to 1 even though it doesn't really
			   matter because the new known region contains no
			   characters.  We do this because this is the most
			   likely size of the characters around the new known
			   region, and we avoid potential yuckiness that is
			   done when size == 3. */
			size = 1;
		}
		if (diffzv < diffmax) {
			bufmax = bufmin = BUF_ZV(buf);
			bytmax = bytmin = BI_BUF_ZV(buf);
			size = 1;
		}
	}
#ifdef ERROR_CHECK_BUFPOS
	else if (x >= bufmin)
		abort();
#endif
	else {
		Bufpos diffmin = bufmin - x;
		Bufpos diffpt = BUF_PT(buf) - x;
		Bufpos diffbegv = x - BUF_BEGV(buf);
		/* #### This value could stand some more exploration. */
		Charcount heuristic_hack = (bufmax - bufmin) >> 2;

		if (diffpt < 0)
			diffpt = -diffpt;
		if (diffbegv < 0)
			diffbegv = -diffbegv;

		/* But also implement a heuristic that favors the known region --
		   see above. */

		diffbegv += heuristic_hack;
		diffpt += heuristic_hack;

		if (diffpt < diffmin && diffpt <= diffbegv) {
			bufmax = bufmin = BUF_PT(buf);
			bytmax = bytmin = BI_BUF_PT(buf);
			/* We set the size to 1 even though it doesn't really
			   matter because the new known region contains no
			   characters.  We do this because this is the most
			   likely size of the characters around the new known
			   region, and we avoid potential yuckiness that is
			   done when size == 3. */
			size = 1;
		}
		if (diffbegv < diffmin) {
			bufmax = bufmin = BUF_BEGV(buf);
			bytmax = bytmin = BI_BUF_BEGV(buf);
			size = 1;
		}
	}

	diff_so_far = x > bufmax ? x - bufmax : bufmin - x;
	if (diff_so_far > 50) {
		/* If we have to move more than a certain amount, then look
		   into our cache. */
		int minval = INT_MAX;
		int found = 0;
		int i;

		add_to_cache = 1;
		/* I considered keeping the positions ordered.  This would speed
		   up this loop, but updating the cache would take longer, so
		   it doesn't seem like it would really matter. */
		for (i = 0; i < 16; i++) {
			int diff = buf->text->mule_bufpos_cache[i] - x;

			if (diff < 0)
				diff = -diff;
			if (diff < minval) {
				minval = diff;
				found = i;
			}
		}

		if (minval < diff_so_far) {
			bufmax = bufmin = buf->text->mule_bufpos_cache[found];
			bytmax = bytmin = buf->text->mule_bytind_cache[found];
			size = 1;
		}
	}

	/* It's conceivable that the caching above could lead to X being
	   the same as one of the range edges. */
	if (x >= bufmax) {
		Bytind newmax;
		Bytecount newsize;

		forward_p = 1;
		while (x > bufmax) {
			newmax = bytmax;

			INC_BYTIND(buf, newmax);
			newsize = newmax - bytmax;
			if (newsize != size) {
				bufmin = bufmax;
				bytmin = bytmax;
				size = newsize;
			}
			bytmax = newmax;
			bufmax++;
		}
		retval = bytmax;

		/* #### Should go past the found location to reduce the number
		   of times that this function is called */
	} else {		/* x < bufmin */

		Bytind newmin;
		Bytecount newsize;

		forward_p = 0;
		while (x < bufmin) {
			newmin = bytmin;

			DEC_BYTIND(buf, newmin);
			newsize = bytmin - newmin;
			if (newsize != size) {
				bufmax = bufmin;
				bytmax = bytmin;
				size = newsize;
			}
			bytmin = newmin;
			bufmin--;
		}
		retval = bytmin;

		/* #### Should go past the found location to reduce the number
		   of times that this function is called
		 */
	}

	/* If size is three, than we have to max sure that the range we
	   discovered isn't too large, because we use a fixed-length
	   table to divide by 3. */

	if (size == 3) {
		int gap = bytmax - bytmin;
		buf->text->mule_three_p = 1;
		buf->text->mule_shifter = 1;

		if (gap > MAX_BYTIND_GAP_SIZE_3) {
			if (forward_p) {
				bytmin = bytmax - MAX_BYTIND_GAP_SIZE_3;
				bufmin = bufmax - MAX_BUFPOS_GAP_SIZE_3;
			} else {
				bytmax = bytmin + MAX_BYTIND_GAP_SIZE_3;
				bufmax = bufmin + MAX_BUFPOS_GAP_SIZE_3;
			}
		}
	} else {
		buf->text->mule_three_p = 0;
		if (size == 4)
			buf->text->mule_shifter = 2;
		else
			buf->text->mule_shifter = size - 1;
	}

	buf->text->mule_bufmin = bufmin;
	buf->text->mule_bufmax = bufmax;
	buf->text->mule_bytmin = bytmin;
	buf->text->mule_bytmax = bytmax;

	if (add_to_cache) {
		int replace_loc;

		/* We throw away a "random" cached value and replace it with
		   the new value.  It doesn't actually have to be very random
		   at all, just evenly distributed.

		   #### It would be better to use a least-recently-used algorithm
		   or something that tries to space things out, but I'm not sure
		   it's worth it to go to the trouble of maintaining that. */
		not_very_random_number += 621;
		replace_loc = not_very_random_number & 15;
		buf->text->mule_bufpos_cache[replace_loc] = x;
		buf->text->mule_bytind_cache[replace_loc] = retval;
	}

	return retval;
}

/* The logic in this function is almost identical to the logic in
   the previous function. */

Bufpos bytind_to_bufpos_func(struct buffer * buf, Bytind x)
{
	Bufpos bufmin;
	Bufpos bufmax;
	Bytind bytmin;
	Bytind bytmax;
	int size;
	int forward_p;
	Bufpos retval;
	int diff_so_far;
	int add_to_cache = 0;

	/* Check for some cached positions, for speed. */
	if (x == BI_BUF_PT(buf))
		return BUF_PT(buf);
	if (x == BI_BUF_ZV(buf))
		return BUF_ZV(buf);
	if (x == BI_BUF_BEGV(buf))
		return BUF_BEGV(buf);

	bufmin = buf->text->mule_bufmin;
	bufmax = buf->text->mule_bufmax;
	bytmin = buf->text->mule_bytmin;
	bytmax = buf->text->mule_bytmax;
	size = (1 << buf->text->mule_shifter) + !!buf->text->mule_three_p;

	/* The basic idea here is that we shift the "known region" up or down
	   until it overlaps the specified position.  We do this by moving
	   the upper bound of the known region up one character at a time,
	   and moving the lower bound of the known region up as necessary
	   when the size of the character just seen changes.

	   We optimize this, however, by first shifting the known region to
	   one of the cached points if it's close by. (We don't check BI_BEG or
	   BI_Z, even though they're cached; most of the time these will be the
	   same as BI_BEGV and BI_ZV, and when they're not, they're not likely
	   to be used.) */

	if (x > bytmax) {
		Bytind diffmax = x - bytmax;
		Bytind diffpt = x - BI_BUF_PT(buf);
		Bytind diffzv = BI_BUF_ZV(buf) - x;
		/* #### This value could stand some more exploration. */
		Bytecount heuristic_hack = (bytmax - bytmin) >> 2;

		/* Check if the position is closer to PT or ZV than to the
		   end of the known region. */

		if (diffpt < 0)
			diffpt = -diffpt;
		if (diffzv < 0)
			diffzv = -diffzv;

		/* But also implement a heuristic that favors the known region
		   over BI_PT or BI_ZV.  The reason for this is that switching to
		   BI_PT or BI_ZV will wipe out the knowledge in the known region,
		   which might be annoying if the known region is large and
		   BI_PT or BI_ZV is not that much closer than the end of the known
		   region. */

		diffzv += heuristic_hack;
		diffpt += heuristic_hack;
		if (diffpt < diffmax && diffpt <= diffzv) {
			bufmax = bufmin = BUF_PT(buf);
			bytmax = bytmin = BI_BUF_PT(buf);
			/* We set the size to 1 even though it doesn't really
			   matter because the new known region contains no
			   characters.  We do this because this is the most
			   likely size of the characters around the new known
			   region, and we avoid potential yuckiness that is
			   done when size == 3. */
			size = 1;
		}
		if (diffzv < diffmax) {
			bufmax = bufmin = BUF_ZV(buf);
			bytmax = bytmin = BI_BUF_ZV(buf);
			size = 1;
		}
	}
#ifdef ERROR_CHECK_BUFPOS
	else if (x >= bytmin)
		abort();
#endif
	else {
		Bytind diffmin = bytmin - x;
		Bytind diffpt = BI_BUF_PT(buf) - x;
		Bytind diffbegv = x - BI_BUF_BEGV(buf);
		/* #### This value could stand some more exploration. */
		Bytecount heuristic_hack = (bytmax - bytmin) >> 2;

		if (diffpt < 0)
			diffpt = -diffpt;
		if (diffbegv < 0)
			diffbegv = -diffbegv;

		/* But also implement a heuristic that favors the known region --
		   see above. */

		diffbegv += heuristic_hack;
		diffpt += heuristic_hack;

		if (diffpt < diffmin && diffpt <= diffbegv) {
			bufmax = bufmin = BUF_PT(buf);
			bytmax = bytmin = BI_BUF_PT(buf);
			/* We set the size to 1 even though it doesn't really
			   matter because the new known region contains no
			   characters.  We do this because this is the most
			   likely size of the characters around the new known
			   region, and we avoid potential yuckiness that is
			   done when size == 3. */
			size = 1;
		}
		if (diffbegv < diffmin) {
			bufmax = bufmin = BUF_BEGV(buf);
			bytmax = bytmin = BI_BUF_BEGV(buf);
			size = 1;
		}
	}

	diff_so_far = x > bytmax ? x - bytmax : bytmin - x;
	if (diff_so_far > 50) {
		/* If we have to move more than a certain amount, then look
		   into our cache. */
		int minval = INT_MAX;
		int found = 0;
		int i;

		add_to_cache = 1;
		/* I considered keeping the positions ordered.  This would speed
		   up this loop, but updating the cache would take longer, so
		   it doesn't seem like it would really matter. */
		for (i = 0; i < 16; i++) {
			int diff = buf->text->mule_bytind_cache[i] - x;

			if (diff < 0)
				diff = -diff;
			if (diff < minval) {
				minval = diff;
				found = i;
			}
		}

		if (minval < diff_so_far) {
			bufmax = bufmin = buf->text->mule_bufpos_cache[found];
			bytmax = bytmin = buf->text->mule_bytind_cache[found];
			size = 1;
		}
	}

	/* It's conceivable that the caching above could lead to X being
	   the same as one of the range edges. */
	if (x >= bytmax) {
		Bytind newmax;
		Bytecount newsize;

		forward_p = 1;
		while (x > bytmax) {
			newmax = bytmax;

			INC_BYTIND(buf, newmax);
			newsize = newmax - bytmax;
			if (newsize != size) {
				bufmin = bufmax;
				bytmin = bytmax;
				size = newsize;
			}
			bytmax = newmax;
			bufmax++;
		}
		retval = bufmax;

		/* #### Should go past the found location to reduce the number
		   of times that this function is called */
	} else {		/* x <= bytmin */

		Bytind newmin;
		Bytecount newsize;

		forward_p = 0;
		while (x < bytmin) {
			newmin = bytmin;

			DEC_BYTIND(buf, newmin);
			newsize = bytmin - newmin;
			if (newsize != size) {
				bufmax = bufmin;
				bytmax = bytmin;
				size = newsize;
			}
			bytmin = newmin;
			bufmin--;
		}
		retval = bufmin;

		/* #### Should go past the found location to reduce the number
		   of times that this function is called
		 */
	}

	/* If size is three, than we have to max sure that the range we
	   discovered isn't too large, because we use a fixed-length
	   table to divide by 3. */

	if (size == 3) {
		int gap = bytmax - bytmin;
		buf->text->mule_three_p = 1;
		buf->text->mule_shifter = 1;

		if (gap > MAX_BYTIND_GAP_SIZE_3) {
			if (forward_p) {
				bytmin = bytmax - MAX_BYTIND_GAP_SIZE_3;
				bufmin = bufmax - MAX_BUFPOS_GAP_SIZE_3;
			} else {
				bytmax = bytmin + MAX_BYTIND_GAP_SIZE_3;
				bufmax = bufmin + MAX_BUFPOS_GAP_SIZE_3;
			}
		}
	} else {
		buf->text->mule_three_p = 0;
		if (size == 4)
			buf->text->mule_shifter = 2;
		else
			buf->text->mule_shifter = size - 1;
	}

	buf->text->mule_bufmin = bufmin;
	buf->text->mule_bufmax = bufmax;
	buf->text->mule_bytmin = bytmin;
	buf->text->mule_bytmax = bytmax;

	if (add_to_cache) {
		int replace_loc;

		/* We throw away a "random" cached value and replace it with
		   the new value.  It doesn't actually have to be very random
		   at all, just evenly distributed.

		   #### It would be better to use a least-recently-used algorithm
		   or something that tries to space things out, but I'm not sure
		   it's worth it to go to the trouble of maintaining that. */
		not_very_random_number += 621;
		replace_loc = not_very_random_number & 15;
		buf->text->mule_bufpos_cache[replace_loc] = retval;
		buf->text->mule_bytind_cache[replace_loc] = x;
	}

	return retval;
}

/* Text of length BYTELENGTH and CHARLENGTH (in different units)
   was inserted at bufpos START. */

static void
buffer_mule_signal_inserted_region(struct buffer *buf, Bufpos start,
				   Bytecount bytelength, Charcount charlength)
{
	int size = (1 << buf->text->mule_shifter) + !!buf->text->mule_three_p;
	int i;

	/* Adjust the cache of known positions. */
	for (i = 0; i < 16; i++) {

		if (buf->text->mule_bufpos_cache[i] > start) {
			buf->text->mule_bufpos_cache[i] += charlength;
			buf->text->mule_bytind_cache[i] += bytelength;
		}
	}

	if (start >= buf->text->mule_bufmax)
		return;

	/* The insertion is either before the known region, in which case
	   it shoves it forward; or within the known region, in which case
	   it shoves the end forward. (But it may make the known region
	   inconsistent, so we may have to shorten it.) */

	if (start <= buf->text->mule_bufmin) {
		buf->text->mule_bufmin += charlength;
		buf->text->mule_bufmax += charlength;
		buf->text->mule_bytmin += bytelength;
		buf->text->mule_bytmax += bytelength;
	} else {
		Bufpos end = start + charlength;
		/* the insertion point divides the known region in two.
		   Keep the longer half, at least, and expand into the
		   inserted chunk as much as possible. */

		if (start - buf->text->mule_bufmin >
		    buf->text->mule_bufmax - start) {
			Bytind bytestart =
			    (buf->text->mule_bytmin +
			     size * (start - buf->text->mule_bufmin));
			Bytind bytenew;

			while (start < end) {
				bytenew = bytestart;
				INC_BYTIND(buf, bytenew);
				if (bytenew - bytestart != size)
					break;
				start++;
				bytestart = bytenew;
			}
			if (start != end) {
				buf->text->mule_bufmax = start;
				buf->text->mule_bytmax = bytestart;
			} else {
				buf->text->mule_bufmax += charlength;
				buf->text->mule_bytmax += bytelength;
			}
		} else {
			Bytind byteend = (buf->text->mule_bytmin
					  + size * (start -
						    buf->text->mule_bufmin)
					  + bytelength);
			Bytind bytenew;

			buf->text->mule_bufmax += charlength;
			buf->text->mule_bytmax += bytelength;

			while (end > start) {
				bytenew = byteend;
				DEC_BYTIND(buf, bytenew);
				if (byteend - bytenew != size)
					break;
				end--;
				byteend = bytenew;
			}
			if (start != end) {
				buf->text->mule_bufmin = end;
				buf->text->mule_bytmin = byteend;
			}
		}
	}
}

/* Text from START to END (equivalent in Bytinds: from BI_START to
   BI_END) was deleted. */

static void
buffer_mule_signal_deleted_region(struct buffer *buf, Bufpos start,
				  Bufpos end, Bytind bi_start, Bytind bi_end)
{
	int i;

	/* Adjust the cache of known positions. */
	for (i = 0; i < 16; i++) {
		/* After the end; gets shoved backward */
		if (buf->text->mule_bufpos_cache[i] > end) {
			buf->text->mule_bufpos_cache[i] -= end - start;
			buf->text->mule_bytind_cache[i] -= bi_end - bi_start;
		}
		/* In the range; moves to start of range */
		else if (buf->text->mule_bufpos_cache[i] > start) {
			buf->text->mule_bufpos_cache[i] = start;
			buf->text->mule_bytind_cache[i] = bi_start;
		}
	}

	/* We don't care about any text after the end of the known region. */

	end = min(end, buf->text->mule_bufmax);
	bi_end = min(bi_end, buf->text->mule_bytmax);
	if (start >= end)
		return;

	/* The end of the known region offsets by the total amount of deletion,
	   since it's all before it. */

	buf->text->mule_bufmax -= end - start;
	buf->text->mule_bytmax -= bi_end - bi_start;

	/* Now we don't care about any text after the start of the known region. */

	end = min(end, buf->text->mule_bufmin);
	bi_end = min(bi_end, buf->text->mule_bytmin);
	if (start >= end)
		return;

	buf->text->mule_bufmin -= end - start;
	buf->text->mule_bytmin -= bi_end - bi_start;
}

#endif				/* MULE */

#ifdef ERROR_CHECK_BUFPOS

Bytind bufpos_to_bytind(struct buffer * buf, Bufpos x)
{
	Bytind retval = real_bufpos_to_bytind(buf, x);
	ASSERT_VALID_BYTIND_UNSAFE(buf, retval);
	return retval;
}

Bufpos bytind_to_bufpos(struct buffer * buf, Bytind x)
{
	ASSERT_VALID_BYTIND_UNSAFE(buf, x);
	return real_bytind_to_bufpos(buf, x);
}

#endif				/* ERROR_CHECK_BUFPOS */

/************************************************************************/
/*                verifying buffer and string positions                 */
/************************************************************************/

/* Functions below are tagged with either _byte or _char indicating
   whether they return byte or character positions.  For a buffer,
   a character position is a "Bufpos" and a byte position is a "Bytind".
   For strings, these are sometimes typed using "Charcount" and
   "Bytecount". */

/* Flags for the functions below are:

   GB_ALLOW_PAST_ACCESSIBLE

     Allow positions to range over the entire buffer (BUF_BEG to BUF_Z),
     rather than just the accessible portion (BUF_BEGV to BUF_ZV).
     For strings, this flag has no effect.

   GB_COERCE_RANGE

     If the position is outside the allowable range, return the lower
     or upper bound of the range, whichever is closer to the specified
     position.

   GB_NO_ERROR_IF_BAD

     If the position is outside the allowable range, return -1.

   GB_NEGATIVE_FROM_END

     If a value is negative, treat it as an offset from the end.
     Only applies to strings.

   The following additional flags apply only to the functions
   that return ranges:

   GB_ALLOW_NIL

     Either or both positions can be nil.  If FROM is nil,
     FROM_OUT will contain the lower bound of the allowed range.
     If TO is nil, TO_OUT will contain the upper bound of the
     allowed range.

   GB_CHECK_ORDER

     FROM must contain the lower bound and TO the upper bound
     of the range.  If the positions are reversed, an error is
     signalled.

   The following is a combination flag:

   GB_HISTORICAL_STRING_BEHAVIOR

     Equivalent to (GB_NEGATIVE_FROM_END | GB_ALLOW_NIL).
 */

/* Return a buffer position stored in a Lisp_Object.  Full
   error-checking is done on the position.  Flags can be specified to
   control the behavior of out-of-range values.  The default behavior
   is to require that the position is within the accessible part of
   the buffer (BEGV and ZV), and to signal an error if the position is
   out of range.

*/

Bufpos
get_buffer_pos_char(struct buffer * b, Lisp_Object pos, unsigned int flags)
{
	/* Does not GC */
	Bufpos ind;
	Bufpos min_allowed, max_allowed;

	CHECK_INT_COERCE_MARKER(pos);
	ind = XINT(pos);
	min_allowed =
	    flags & GB_ALLOW_PAST_ACCESSIBLE ? BUF_BEG(b) : BUF_BEGV(b);
	max_allowed = flags & GB_ALLOW_PAST_ACCESSIBLE ? BUF_Z(b) : BUF_ZV(b);

	if (ind < min_allowed || ind > max_allowed) {
		if (flags & GB_COERCE_RANGE)
			ind = ind < min_allowed ? min_allowed : max_allowed;
		else if (flags & GB_NO_ERROR_IF_BAD)
			ind = -1;
		else {
			Lisp_Object buffer;
			XSETBUFFER(buffer, b);
			args_out_of_range(buffer, pos);
		}
	}

	return ind;
}

Bytind
get_buffer_pos_byte(struct buffer * b, Lisp_Object pos, unsigned int flags)
{
	Bufpos bpos = get_buffer_pos_char(b, pos, flags);
	if (bpos < 0)		/* could happen with GB_NO_ERROR_IF_BAD */
		return -1;
	return bufpos_to_bytind(b, bpos);
}

/* Return a pair of buffer positions representing a range of text,
   taken from a pair of Lisp_Objects.  Full error-checking is
   done on the positions.  Flags can be specified to control the
   behavior of out-of-range values.  The default behavior is to
   allow the range bounds to be specified in either order
   (however, FROM_OUT will always be the lower bound of the range
   and TO_OUT the upper bound),to require that the positions
   are within the accessible part of the buffer (BEGV and ZV),
   and to signal an error if the positions are out of range.
*/

void
get_buffer_range_char(struct buffer *b, Lisp_Object from, Lisp_Object to,
		      Bufpos * from_out, Bufpos * to_out, unsigned int flags)
{
	/* Does not GC */
	Bufpos min_allowed, max_allowed;

	min_allowed = (flags & GB_ALLOW_PAST_ACCESSIBLE) ?
	    BUF_BEG(b) : BUF_BEGV(b);
	max_allowed = (flags & GB_ALLOW_PAST_ACCESSIBLE) ? BUF_Z(b) : BUF_ZV(b);

	if (NILP(from) && (flags & GB_ALLOW_NIL))
		*from_out = min_allowed;
	else
		*from_out =
		    get_buffer_pos_char(b, from, flags | GB_NO_ERROR_IF_BAD);

	if (NILP(to) && (flags & GB_ALLOW_NIL))
		*to_out = max_allowed;
	else
		*to_out =
		    get_buffer_pos_char(b, to, flags | GB_NO_ERROR_IF_BAD);

	if ((*from_out < 0 || *to_out < 0) && !(flags & GB_NO_ERROR_IF_BAD)) {
		Lisp_Object buffer;
		XSETBUFFER(buffer, b);
		args_out_of_range_3(buffer, from, to);
	}

	if (*from_out >= 0 && *to_out >= 0 && *from_out > *to_out) {
		if (flags & GB_CHECK_ORDER)
			signal_simple_error_2("start greater than end", from,
					      to);
		else {
			Bufpos temp = *from_out;
			*from_out = *to_out;
			*to_out = temp;
		}
	}
}

void
get_buffer_range_byte(struct buffer *b, Lisp_Object from, Lisp_Object to,
		      Bytind * from_out, Bytind * to_out, unsigned int flags)
{
	Bufpos s, e;

	get_buffer_range_char(b, from, to, &s, &e, flags);
	if (s >= 0)
		*from_out = bufpos_to_bytind(b, s);
	else			/* could happen with GB_NO_ERROR_IF_BAD */
		*from_out = -1;
	if (e >= 0)
		*to_out = bufpos_to_bytind(b, e);
	else
		*to_out = -1;
}

static Charcount
get_string_pos_char_1(Lisp_Object string, Lisp_Object pos, unsigned int flags,
		      Charcount known_length)
{
	Charcount ccpos;
	Charcount min_allowed = 0;
	Charcount max_allowed = known_length;

	/* Computation of KNOWN_LENGTH is potentially expensive so we pass
	   it in. */
	CHECK_INT(pos);
	ccpos = XINT(pos);
	if (ccpos < 0 && flags & GB_NEGATIVE_FROM_END)
		ccpos += max_allowed;

	if (ccpos < min_allowed || ccpos > max_allowed) {
		if (flags & GB_COERCE_RANGE)
			ccpos = ccpos < min_allowed ? min_allowed : max_allowed;
		else if (flags & GB_NO_ERROR_IF_BAD)
			ccpos = -1;
		else
			args_out_of_range(string, pos);
	}

	return ccpos;
}

Charcount
get_string_pos_char(Lisp_Object string, Lisp_Object pos, unsigned int flags)
{
	return get_string_pos_char_1(string, pos, flags,
				     XSTRING_CHAR_LENGTH(string));
}

Bytecount
get_string_pos_byte(Lisp_Object string, Lisp_Object pos, unsigned int flags)
{
	Charcount ccpos = get_string_pos_char(string, pos, flags);
	if (ccpos < 0)		/* could happen with GB_NO_ERROR_IF_BAD */
		return -1;
	return charcount_to_bytecount(XSTRING_DATA(string), ccpos);
}

void
get_string_range_char(Lisp_Object string, Lisp_Object from, Lisp_Object to,
		      Charcount * from_out, Charcount * to_out,
		      unsigned int flags)
{
	Charcount min_allowed = 0;
	Charcount max_allowed = XSTRING_CHAR_LENGTH(string);

	if (NILP(from) && (flags & GB_ALLOW_NIL))
		*from_out = min_allowed;
	else
		*from_out = get_string_pos_char_1(string, from,
						  flags | GB_NO_ERROR_IF_BAD,
						  max_allowed);

	if (NILP(to) && (flags & GB_ALLOW_NIL))
		*to_out = max_allowed;
	else
		*to_out = get_string_pos_char_1(string, to,
						flags | GB_NO_ERROR_IF_BAD,
						max_allowed);

	if ((*from_out < 0 || *to_out < 0) && !(flags & GB_NO_ERROR_IF_BAD))
		args_out_of_range_3(string, from, to);

	if (*from_out >= 0 && *to_out >= 0 && *from_out > *to_out) {
		if (flags & GB_CHECK_ORDER)
			signal_simple_error_2("start greater than end", from,
					      to);
		else {
			Bufpos temp = *from_out;
			*from_out = *to_out;
			*to_out = temp;
		}
	}
}

void
get_string_range_byte(Lisp_Object string, Lisp_Object from, Lisp_Object to,
		      Bytecount * from_out, Bytecount * to_out,
		      unsigned int flags)
{
	Charcount s, e;

	get_string_range_char(string, from, to, &s, &e, flags);
	if (s >= 0)
		*from_out = charcount_to_bytecount(XSTRING_DATA(string), s);
	else			/* could happen with GB_NO_ERROR_IF_BAD */
		*from_out = -1;
	if (e >= 0)
		*to_out = charcount_to_bytecount(XSTRING_DATA(string), e);
	else
		*to_out = -1;

}

Bufpos
get_buffer_or_string_pos_char(Lisp_Object object, Lisp_Object pos,
			      unsigned int flags)
{
	return STRINGP(object) ?
	    get_string_pos_char(object, pos, flags) :
	    get_buffer_pos_char(XBUFFER(object), pos, flags);
}

Bytind
get_buffer_or_string_pos_byte(Lisp_Object object, Lisp_Object pos,
			      unsigned int flags)
{
	return STRINGP(object) ?
	    get_string_pos_byte(object, pos, flags) :
	    get_buffer_pos_byte(XBUFFER(object), pos, flags);
}

void
get_buffer_or_string_range_char(Lisp_Object object, Lisp_Object from,
				Lisp_Object to, Bufpos * from_out,
				Bufpos * to_out, unsigned int flags)
{
	if (STRINGP(object))
		get_string_range_char(object, from, to, from_out, to_out,
				      flags);
	else
		get_buffer_range_char(XBUFFER(object), from, to, from_out,
				      to_out, flags);
}

void
get_buffer_or_string_range_byte(Lisp_Object object, Lisp_Object from,
				Lisp_Object to, Bytind * from_out,
				Bytind * to_out, unsigned int flags)
{
	if (STRINGP(object))
		get_string_range_byte(object, from, to, from_out, to_out,
				      flags);
	else
		get_buffer_range_byte(XBUFFER(object), from, to, from_out,
				      to_out, flags);
}

Bufpos buffer_or_string_accessible_begin_char(Lisp_Object object)
{
	return STRINGP(object) ? 0 : BUF_BEGV(XBUFFER(object));
}

Bufpos buffer_or_string_accessible_end_char(Lisp_Object object)
{
	return STRINGP(object) ?
	    XSTRING_CHAR_LENGTH(object) : BUF_ZV(XBUFFER(object));
}

Bytind buffer_or_string_accessible_begin_byte(Lisp_Object object)
{
	return STRINGP(object) ? 0 : BI_BUF_BEGV(XBUFFER(object));
}

Bytind buffer_or_string_accessible_end_byte(Lisp_Object object)
{
	return STRINGP(object) ?
	    XSTRING_LENGTH(object) : BI_BUF_ZV(XBUFFER(object));
}

Bufpos buffer_or_string_absolute_begin_char(Lisp_Object object)
{
	return STRINGP(object) ? 0 : BUF_BEG(XBUFFER(object));
}

Bufpos buffer_or_string_absolute_end_char(Lisp_Object object)
{
	return STRINGP(object) ?
	    XSTRING_CHAR_LENGTH(object) : BUF_Z(XBUFFER(object));
}

Bytind buffer_or_string_absolute_begin_byte(Lisp_Object object)
{
	return STRINGP(object) ? 0 : BI_BUF_BEG(XBUFFER(object));
}

Bytind buffer_or_string_absolute_end_byte(Lisp_Object object)
{
	return STRINGP(object) ?
	    XSTRING_LENGTH(object) : BI_BUF_Z(XBUFFER(object));
}

/************************************************************************/
/*                     point and marker adjustment                      */
/************************************************************************/

/* just_set_point() is the only place `PT' is an lvalue in all of emacs.
   This function is called from set_buffer_point(), which is the function
   that the SET_PT and BUF_SET_PT macros expand into, and from the
   routines below that insert and delete text. (This is in cases where
   the point marker logically doesn't move but PT (being a byte index)
   needs to get adjusted.) */

/* Set point to a specified value.  This is used only when the value
   of point changes due to an insert or delete; it does not represent
   a conceptual change in point as a marker.  In particular, point is
   not crossing any interval boundaries, so there's no need to use the
   usual SET_PT macro.  In fact it would be incorrect to do so, because
   either the old or the new value of point is out of synch with the
   current set of intervals.  */

/* This gets called more than enough to make the function call
   overhead a significant factor so we've turned it into a macro. */
#define JUST_SET_POINT(buf, bufpos, ind)	\
do						\
{						\
  buf->bufpt = (bufpos);			\
  buf->pt = (ind);				\
} while (0)

/* Set a buffer's point. */

void set_buffer_point(struct buffer *buf, Bufpos bufpos, Bytind bytpos)
{
	assert(bytpos >= BI_BUF_BEGV(buf) && bytpos <= BI_BUF_ZV(buf));
	if (bytpos == BI_BUF_PT(buf))
		return;
	JUST_SET_POINT(buf, bufpos, bytpos);
	MARK_POINT_CHANGED;
	assert(MARKERP(buf->point_marker));
	XMARKER(buf->point_marker)->memind = bytind_to_memind(buf, bytpos);

	/* FSF makes sure that PT is not being set within invisible text.
	   However, this is the wrong place for that check.  The check
	   should happen only at the next redisplay. */

	/* Some old coder said:

	   "If there were to be hooks which were run when point entered/left an
	   extent, this would be the place to put them.

	   However, it's probably the case that such hooks should be implemented
	   using a post-command-hook instead, to avoid running the hooks as a
	   result of intermediate motion inside of save-excursions, for example."

	   I definitely agree with this.  PT gets moved all over the place
	   and it would be a Bad Thing for any hooks to get called, both for
	   the reason above and because many callers are not prepared for
	   a GC within this function. --ben
	 */
}

/* Do the correct marker-like adjustment on MPOS (see below).  FROM, TO,
   and AMOUNT are as in adjust_markers().  If MPOS doesn't need to be
   adjusted, nothing will happen. */
Memind
do_marker_adjustment(Memind mpos, Memind from, Memind to, Bytecount amount)
{
	if (amount > 0) {
		if (mpos > to && mpos < to + amount)
			mpos = to + amount;
	} else {
		if (mpos > from + amount && mpos <= from)
			mpos = from + amount;
	}
	if (mpos > from && mpos <= to)
		mpos += amount;
	return mpos;
}

/* Do the following:

   (1) Add `amount' to the position of every marker in the current buffer
   whose current position is between `from' (exclusive) and `to' (inclusive).

   (2) Also, any markers past the outside of that interval, in the direction
   of adjustment, are first moved back to the near end of the interval
   and then adjusted by `amount'.

   This function is called in two different cases: when a region of
   characters adjacent to the gap is moved, causing the gap to shift
   to the other side of the region (in this case, `from' and `to'
   point to the old position of the region and there should be no
   markers affected by (2) because they would be inside the gap),
   or when a region of characters adjacent to the gap is wiped out,
   causing the gap to increase to include the region (in this case,
   `from' and `to' are the same, both pointing to the boundary
   between the gap and the deleted region, and there are no markers
   affected by (1)).

   The reason for the use of exclusive and inclusive is that markers at
   the gap always sit at the beginning, not at the end.
*/

static void
adjust_markers(struct buffer *buf, Memind from, Memind to, Bytecount amount)
{
	Lisp_Marker *m;

	for (m = BUF_MARKERS(buf); m; m = marker_next(m))
		m->memind = do_marker_adjustment(m->memind, from, to, amount);
}

/* Adjust markers whose insertion-type is t
   for an insertion of AMOUNT characters at POS.  */

static void
adjust_markers_for_insert(struct buffer *buf, Memind ind, Bytecount amount)
{
	Lisp_Marker *m;

	for (m = BUF_MARKERS(buf); m; m = marker_next(m)) {
		if (m->insertion_type && m->memind == ind)
			m->memind += amount;
	}
}

/************************************************************************/
/*                  Routines for dealing with the gap                   */
/************************************************************************/

/* maximum amount of memory moved in a single chunk.  Increasing this
   value improves gap-motion efficiency but decreases QUIT responsiveness
   time.  Was 32000 but today's processors are faster and files are
   bigger.  --ben */
#define GAP_MOVE_CHUNK 300000

/* Move the gap to POS, which is less than the current GPT. */

static void gap_left(struct buffer *buf, Bytind pos)
{
	Bufbyte *to, *from;
	Bytecount i;
	Bytind new_s1;
	struct buffer *mbuf;
	Lisp_Object bufcons;

	from = BUF_GPT_ADDR(buf);
	to = from + BUF_GAP_SIZE(buf);
	new_s1 = BI_BUF_GPT(buf);

	/* Now copy the characters.  To move the gap down,
	   copy characters up.  */

	while (1) {
		/* I gets number of characters left to copy.  */
		i = new_s1 - pos;
		if (i == 0)
			break;
		/* If a quit is requested, stop copying now.
		   Change POS to be where we have actually moved the gap to.  */
		if (QUITP) {
			pos = new_s1;
			break;
		}
		/* Move at most GAP_MOVE_CHUNK chars before checking again for a quit. */
		if (i > GAP_MOVE_CHUNK)
			i = GAP_MOVE_CHUNK;

		if (i >= 128) {
			new_s1 -= i;
			from -= i;
			to -= i;
			memmove(to, from, i);
		} else {
			new_s1 -= i;
			while (--i >= 0)
				*--to = *--from;
		}
	}

	/* Adjust markers, and buffer data structure, to put the gap at POS.
	   POS is where the loop above stopped, which may be what was specified
	   or may be where a quit was detected.  */
	MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
		adjust_markers(mbuf, pos, BI_BUF_GPT(mbuf), BUF_GAP_SIZE(mbuf));
	}
	MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
		adjust_extents(make_buffer(mbuf), pos, BI_BUF_GPT(mbuf),
			       BUF_GAP_SIZE(mbuf));
	}
	SET_BI_BUF_GPT(buf, pos);
	SET_GAP_SENTINEL(buf);
#ifdef ERROR_CHECK_EXTENTS
	MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
		sledgehammer_extent_check(make_buffer(mbuf));
	}
#endif
	QUIT;
}

static void gap_right(struct buffer *buf, Bytind pos)
{
	Bufbyte *to, *from;
	Bytecount i;
	Bytind new_s1;
	struct buffer *mbuf;
	Lisp_Object bufcons;

	to = BUF_GPT_ADDR(buf);
	from = to + BUF_GAP_SIZE(buf);
	new_s1 = BI_BUF_GPT(buf);

	/* Now copy the characters.  To move the gap up,
	   copy characters down.  */

	while (1) {
		/* I gets number of characters left to copy.  */
		i = pos - new_s1;
		if (i == 0)
			break;
		/* If a quit is requested, stop copying now.
		   Change POS to be where we have actually moved the gap to.  */
		if (QUITP) {
			pos = new_s1;
			break;
		}
		/* Move at most GAP_MOVE_CHUNK chars before checking again for a quit. */
		if (i > GAP_MOVE_CHUNK)
			i = GAP_MOVE_CHUNK;

		if (i >= 128) {
			new_s1 += i;
			memmove(to, from, i);
			from += i;
			to += i;
		} else {
			new_s1 += i;
			while (--i >= 0)
				*to++ = *from++;
		}
	}

	{
		int gsize = BUF_GAP_SIZE(buf);
		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			adjust_markers(mbuf, BI_BUF_GPT(mbuf) + gsize,
				       pos + gsize, -gsize);
		}
		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			adjust_extents(make_buffer(mbuf),
				       BI_BUF_GPT(mbuf) + gsize, pos + gsize,
				       -gsize);
		}
		SET_BI_BUF_GPT(buf, pos);
		SET_GAP_SENTINEL(buf);
#ifdef ERROR_CHECK_EXTENTS
		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			sledgehammer_extent_check(make_buffer(mbuf));
		}
#endif
	}
	if (pos == BI_BUF_Z(buf)) {
		/* merge gap with end gap */

		SET_BUF_GAP_SIZE(buf,
				 BUF_GAP_SIZE(buf) + BUF_END_GAP_SIZE(buf));
		SET_BUF_END_GAP_SIZE(buf, 0);
		SET_END_SENTINEL(buf);
	}

	QUIT;
}

/* Move gap to position `pos'.
   Note that this can quit!  */

static void move_gap(struct buffer *buf, Bytind pos)
{
	if (!BUF_BEG_ADDR(buf))
		abort();
	if (pos < BI_BUF_GPT(buf))
		gap_left(buf, pos);
	else if (pos > BI_BUF_GPT(buf))
		gap_right(buf, pos);
}

/* Merge the end gap into the gap */

static void merge_gap_with_end_gap(struct buffer *buf)
{
	Lisp_Object tem;
	Bytind real_gap_loc;
	Bytecount old_gap_size;
	Bytecount increment;

	increment = BUF_END_GAP_SIZE(buf);
	SET_BUF_END_GAP_SIZE(buf, 0);

	if (increment > 0) {
		/* Prevent quitting in move_gap.  */
		tem = Vinhibit_quit;
		Vinhibit_quit = Qt;

		real_gap_loc = BI_BUF_GPT(buf);
		old_gap_size = BUF_GAP_SIZE(buf);

		/* Pretend the end gap is the gap */
		SET_BI_BUF_GPT(buf, BI_BUF_Z(buf) + BUF_GAP_SIZE(buf));
		SET_BUF_GAP_SIZE(buf, increment);

		/* Move the new gap down to be consecutive with the end of the old one.
		   This adjusts the markers properly too.  */
		gap_left(buf, real_gap_loc + old_gap_size);

		/* Now combine the two into one large gap.  */
		SET_BUF_GAP_SIZE(buf, BUF_GAP_SIZE(buf) + old_gap_size);
		SET_BI_BUF_GPT(buf, real_gap_loc);
		SET_GAP_SENTINEL(buf);

		/* We changed the total size of the buffer (including gap),
		   so we need to fix up the end sentinel. */
		SET_END_SENTINEL(buf);

		Vinhibit_quit = tem;
	}
}

/* Make the gap INCREMENT bytes longer.  */

static void make_gap(struct buffer *buf, Bytecount increment)
{
	Bufbyte *result;
	Lisp_Object tem;
	Bytind real_gap_loc;
	Bytecount old_gap_size;

	/* If we have to get more space, get enough to last a while.  We use
	   a geometric progression that saves on realloc space. */
	increment += 2000 + ((BI_BUF_Z(buf) - BI_BUF_BEG(buf)) / 8);

	if (increment > BUF_END_GAP_SIZE(buf)) {
		/* Don't allow a buffer size that won't fit in an int
		   even if it will fit in a Lisp integer.
		   That won't work because so many places use `int'.  */

		if (BUF_Z(buf) - BUF_BEG(buf) + BUF_GAP_SIZE(buf) + increment
		    > EMACS_INT_MAX)
			error("Maximum buffer size exceeded");

		result = BUFFER_REALLOC(buf->text->beg,
					BI_BUF_Z(buf) - BI_BUF_BEG(buf) +
					BUF_GAP_SIZE(buf) + increment +
					BUF_END_SENTINEL_SIZE);
		if (result == 0)
			memory_full();

		SET_BUF_BEG_ADDR(buf, result);
	} else
		increment = BUF_END_GAP_SIZE(buf);

	/* Prevent quitting in move_gap.  */
	tem = Vinhibit_quit;
	Vinhibit_quit = Qt;

	real_gap_loc = BI_BUF_GPT(buf);
	old_gap_size = BUF_GAP_SIZE(buf);

	/* Call the newly allocated space a gap at the end of the whole space.  */
	SET_BI_BUF_GPT(buf, BI_BUF_Z(buf) + BUF_GAP_SIZE(buf));
	SET_BUF_GAP_SIZE(buf, increment);

	SET_BUF_END_GAP_SIZE(buf, 0);

	/* Move the new gap down to be consecutive with the end of the old one.
	   This adjusts the markers properly too.  */
	gap_left(buf, real_gap_loc + old_gap_size);

	/* Now combine the two into one large gap.  */
	SET_BUF_GAP_SIZE(buf, BUF_GAP_SIZE(buf) + old_gap_size);
	SET_BI_BUF_GPT(buf, real_gap_loc);
	SET_GAP_SENTINEL(buf);

	/* We changed the total size of the buffer (including gap),
	   so we need to fix up the end sentinel. */
	SET_END_SENTINEL(buf);

	Vinhibit_quit = tem;
}

/************************************************************************/
/*                     Before/after-change processing                   */
/************************************************************************/

/* Those magic changes ... */

static void
buffer_signal_changed_region(struct buffer *buf, Bufpos start, Bufpos end)
{
	/* The changed region is recorded as the number of unchanged
	   characters from the beginning and from the end of the
	   buffer.  This obviates much of the need of shifting the
	   region around to compensate for insertions and deletions.
	 */
	if (buf->changes->begin_unchanged < 0 ||
	    buf->changes->begin_unchanged > start - BUF_BEG(buf))
		buf->changes->begin_unchanged = start - BUF_BEG(buf);
	if (buf->changes->end_unchanged < 0 ||
	    buf->changes->end_unchanged > BUF_Z(buf) - end)
		buf->changes->end_unchanged = BUF_Z(buf) - end;
}

void
buffer_extent_signal_changed_region(struct buffer *buf, Bufpos start,
				    Bufpos end)
{
	if (buf->changes->begin_extent_unchanged < 0 ||
	    buf->changes->begin_extent_unchanged > start - BUF_BEG(buf))
		buf->changes->begin_extent_unchanged = start - BUF_BEG(buf);
	if (buf->changes->end_extent_unchanged < 0 ||
	    buf->changes->end_extent_unchanged > BUF_Z(buf) - end)
		buf->changes->end_extent_unchanged = BUF_Z(buf) - end;
}

void buffer_reset_changes(struct buffer *buf)
{
	buf->changes->begin_unchanged = -1;
	buf->changes->end_unchanged = -1;
	buf->changes->begin_extent_unchanged = -1;
	buf->changes->end_extent_unchanged = -1;
	buf->changes->newline_was_deleted = 0;
}

static void
signal_after_change(struct buffer *buf, Bufpos start, Bufpos orig_end,
		    Bufpos new_end);

/* Call the after-change-functions according to the changes made so far
   and treat all further changes as single until the outermost
   multiple change exits.  This is called when the outermost multiple
   change exits and when someone is trying to make a change that violates
   the constraints specified in begin_multiple_change(), typically
   when nested multiple-change sessions occur. (There are smarter ways of
   dealing with nested multiple changes, but these rarely occur so there's
   probably no point in it.) */

/* #### This needs to keep track of what actually changed and only
   call the after-change functions on that region. */

static void cancel_multiple_change(struct buffer *buf)
{
	/* This function can GC */
	/* Call the after-change-functions except when they've already been
	   called or when there were no changes made to the buffer at all. */
	if (buf->text->changes->mc_begin != 0 &&
	    buf->text->changes->mc_begin_signaled) {
		Bufpos real_mc_begin = buf->text->changes->mc_begin;
		buf->text->changes->mc_begin = 0;

		signal_after_change(buf, real_mc_begin,
				    buf->text->changes->mc_orig_end,
				    buf->text->changes->mc_new_end);
	} else {
		buf->text->changes->mc_begin = 0;
	}
}

/* this is an unwind_protect, to ensure that the after-change-functions
   get called even in a non-local exit. */

static Lisp_Object multiple_change_finish_up(Lisp_Object buffer)
{
	struct buffer *buf = XBUFFER(buffer);

	/* #### I don't know whether or not it should even be possible to
	   get here with a dead buffer (though given how it is called I can
	   see how it might be).  In any case, there isn't time before 19.14
	   to find out. */
	if (!BUFFER_LIVE_P(buf))
		return Qnil;

	/* This function can GC */
	buf->text->changes->in_multiple_change = 0;	/* do this first so that
							   errors in the after-change
							   functions don't mess things
							   up. */
	cancel_multiple_change(buf);
	return Qnil;
}

/* Call this function when you're about to make a number of buffer changes
   that should be considered a single change. (e.g. `replace-match' calls
   this.) You need to specify the START and END of the region that is
   going to be changed so that the before-change-functions are called
   with the correct arguments.  The after-change region is calculated
   automatically, however, and if changes somehow or other happen outside
   of the specified region, that will also be handled correctly.

   begin_multiple_change() returns a number (actually a specpdl depth)
   that you must pass to end_multiple_change() when you are done.

   FSF Emacs 20 implements a similar feature, accessible from Lisp
   through a `combine-after-change-calls' special form, which is
   essentially equivalent to this function.  We should consider
   whether we want to introduce a similar Lisp form.  */

int begin_multiple_change(struct buffer *buf, Bufpos start, Bufpos end)
{
	/* This function can GC */
	int count = -1;
	if (buf->text->changes->in_multiple_change) {
		if (buf->text->changes->mc_begin != 0 &&
		    (start < buf->text->changes->mc_begin ||
		     end > buf->text->changes->mc_new_end))
			cancel_multiple_change(buf);
	} else {
		Lisp_Object buffer;

		buf->text->changes->mc_begin = start;
		buf->text->changes->mc_orig_end =
		    buf->text->changes->mc_new_end = end;
		buf->text->changes->mc_begin_signaled = 0;
		count = specpdl_depth();
		XSETBUFFER(buffer, buf);
		record_unwind_protect(multiple_change_finish_up, buffer);
	}
	buf->text->changes->in_multiple_change++;
	/* We don't call before-change-functions until signal_before_change()
	   is called, in case there is a read-only or other error. */
	return count;
}

void end_multiple_change(struct buffer *buf, int count)
{
	assert(buf->text->changes->in_multiple_change > 0);
	buf->text->changes->in_multiple_change--;
	if (!buf->text->changes->in_multiple_change)
		unbind_to(count, Qnil);
}

static int inside_change_hook;

static Lisp_Object change_function_restore(Lisp_Object buffer)
{
	/* We should first reset the variable and then change the buffer,
	   because Fset_buffer() can throw.  */
	inside_change_hook = 0;
	if (XBUFFER(buffer) != current_buffer)
		Fset_buffer(buffer);
	return Qnil;
}

static int in_first_change;

static Lisp_Object first_change_hook_restore(Lisp_Object buffer)
{
	in_first_change = 0;
	Fset_buffer(buffer);
	return Qnil;
}

/* Signal an initial modification to the buffer.  */

static void signal_first_change(struct buffer *buf)
{
	/* This function can GC */
	Lisp_Object buffer;
	XSETBUFFER(buffer, current_buffer);

	if (!in_first_change) {
		if (!NILP(symbol_value_in_buffer(Qfirst_change_hook, buffer))) {
			int speccount = specpdl_depth();
			record_unwind_protect(first_change_hook_restore,
					      buffer);
			set_buffer_internal(buf);
			in_first_change = 1;
			run_hook(Qfirst_change_hook);
			unbind_to(speccount, Qnil);
		}
	}
}

/* Signal a change to the buffer immediately before it happens.
   START and END are the bounds of the text to be changed. */

static void signal_before_change(struct buffer *buf, Bufpos start, Bufpos end)
{
	/* This function can GC */
	struct buffer *mbuf;
	Lisp_Object bufcons;

	if (!inside_change_hook) {
		Lisp_Object buffer;
		int speccount;

		/* Are we in a multiple-change session? */
		if (buf->text->changes->in_multiple_change &&
		    buf->text->changes->mc_begin != 0) {
			/* If we're violating the constraints of the session,
			   call the after-change-functions as necessary for the
			   changes already made and treat further changes as
			   single. */
			if (start < buf->text->changes->mc_begin ||
			    end > buf->text->changes->mc_new_end)
				cancel_multiple_change(buf);
			/* Do nothing if this is not the first change in the session. */
			else if (buf->text->changes->mc_begin_signaled)
				return;
			else {
				/* First time through; call the before-change-functions
				   specifying the entire region to be changed. (Note that
				   we didn't call before-change-functions in
				   begin_multiple_change() because the buffer might be
				   read-only, etc.) */
				start = buf->text->changes->mc_begin;
				end = buf->text->changes->mc_new_end;
			}
		}

		/* If buffer is unmodified, run a special hook for that case.  */
		if (BUF_SAVE_MODIFF(buf) >= BUF_MODIFF(buf)) {
			MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
				signal_first_change(mbuf);
			}
		}

		/* Now in any case run the before-change-functions if any.  */
		speccount = specpdl_depth();
		record_unwind_protect(change_function_restore,
				      Fcurrent_buffer());
		inside_change_hook = 1;

		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			XSETBUFFER(buffer, mbuf);
			if (!NILP
			    (symbol_value_in_buffer
			     (Qbefore_change_functions, buffer))
			    /* Obsolete, for compatibility */
			    ||
			    !NILP(symbol_value_in_buffer
				  (Qbefore_change_function, buffer))) {
				set_buffer_internal(buf);
				va_run_hook_with_args(Qbefore_change_functions,
						      2, make_int(start),
						      make_int(end));
				/* Obsolete, for compatibility */
				va_run_hook_with_args(Qbefore_change_function,
						      2, make_int(start),
						      make_int(end));
			}
		}

		/* Make sure endpoints remain valid.  before-change-functions
		   might have modified the buffer. */
		if (start < BUF_BEGV(buf))
			start = BUF_BEGV(buf);
		if (start > BUF_ZV(buf))
			start = BUF_ZV(buf);
		if (end < BUF_BEGV(buf))
			end = BUF_BEGV(buf);
		if (end > BUF_ZV(buf))
			end = BUF_ZV(buf);

		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			XSETBUFFER(buffer, mbuf);
			report_extent_modification(buffer, start, end, 0);
		}
		unbind_to(speccount, Qnil);

		/* Only now do we indicate that the before-change-functions have
		   been called, in case some function throws out. */
		buf->text->changes->mc_begin_signaled = 1;
	}
}

/* Signal a change immediately after it happens.
   START is the bufpos of the start of the changed text.
   ORIG_END is the bufpos of the end of the before-changed text.
   NEW_END is the bufpos of the end of the after-changed text.
 */

static void
signal_after_change(struct buffer *buf, Bufpos start, Bufpos orig_end,
		    Bufpos new_end)
{
	/* This function can GC */
	struct buffer *mbuf;
	Lisp_Object bufcons;

	MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
		/* always do this. */
		buffer_signal_changed_region(mbuf, start, new_end);
	}
	MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
		/* #### This seems inefficient.  Wouldn't it be better to just
		   keep one cache per base buffer?  */
		font_lock_maybe_update_syntactic_caches(mbuf, start, orig_end,
							new_end);
	}

	if (!inside_change_hook) {
		Lisp_Object buffer;
		int speccount;

		if (buf->text->changes->in_multiple_change &&
		    buf->text->changes->mc_begin != 0) {
			assert(start >= buf->text->changes->mc_begin &&
			       start <= buf->text->changes->mc_new_end);
			assert(orig_end >= buf->text->changes->mc_begin &&
			       orig_end <= buf->text->changes->mc_new_end);
			buf->text->changes->mc_new_end += new_end - orig_end;
			return;	/* after-change-functions signalled when all changes done */
		}

		speccount = specpdl_depth();
		record_unwind_protect(change_function_restore,
				      Fcurrent_buffer());
		inside_change_hook = 1;
		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			XSETBUFFER(buffer, mbuf);

			if (!NILP
			    (symbol_value_in_buffer
			     (Qafter_change_functions, buffer))
			    /* Obsolete, for compatibility */
			    ||
			    !NILP(symbol_value_in_buffer
				  (Qafter_change_function, buffer))) {
				set_buffer_internal(buf);
				/* The actual after-change functions take slightly
				   different arguments than what we were passed. */
				va_run_hook_with_args(Qafter_change_functions,
						      3, make_int(start),
						      make_int(new_end),
						      make_int(orig_end -
							       start));
				/* Obsolete, for compatibility */
				va_run_hook_with_args(Qafter_change_function, 3,
						      make_int(start),
						      make_int(new_end),
						      make_int(orig_end -
							       start));
			}
		}

		/* Make sure endpoints remain valid.  after-change-functions
		   might have modified the buffer. */
		if (start < BUF_BEGV(buf))
			start = BUF_BEGV(buf);
		if (start > BUF_ZV(buf))
			start = BUF_ZV(buf);
		if (new_end < BUF_BEGV(buf))
			new_end = BUF_BEGV(buf);
		if (new_end > BUF_ZV(buf))
			new_end = BUF_ZV(buf);
		if (orig_end < BUF_BEGV(buf))
			orig_end = BUF_BEGV(buf);
		if (orig_end > BUF_ZV(buf))
			orig_end = BUF_ZV(buf);

		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			XSETBUFFER(buffer, mbuf);
			report_extent_modification(buffer, start, new_end, 1);
		}
		unbind_to(speccount, Qnil);	/* sets inside_change_hook back to 0 */
	}
}

/* Call this if you're about to change the region of BUFFER from START
   to END.  This checks the read-only properties of the region, calls
   the necessary modification hooks, and warns the next redisplay that
   it should pay attention to that area.  */

static void
prepare_to_modify_buffer(struct buffer *buf, Bufpos start, Bufpos end,
			 int lockit)
{
	/* This function can GC */
	/* dmoore - This function can also kill the buffer buf, the current
	   buffer, and do anything it pleases.  So if you call it, be
	   careful. */
	struct buffer *mbuf;
	Lisp_Object buffer, bufcons;
	struct gcpro gcpro1;

	MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
		barf_if_buffer_read_only(mbuf, start, end);
	}

	/* if this is the first modification, see about locking the buffer's
	   file */
	XSETBUFFER(buffer, buf);
	GCPRO1(buffer);
	if (!NILP(buf->filename) && lockit &&
	    BUF_SAVE_MODIFF(buf) >= BUF_MODIFF(buf)) {
		/* At least warn if this file has changed on disk since it was visited. */
		if (NILP(Fverify_visited_file_modtime(buffer))
		    && !NILP(Ffile_exists_p(buf->filename)))
			call1_in_buffer(buf,
					intern
					("ask-user-about-supersession-threat"),
					buf->filename);
#ifdef CLASH_DETECTION
		if (!NILP(buf->file_truename))
			/* Make binding buffer-file-name to nil effective.  */
			lock_file(buf->file_truename);
#endif				/* not CLASH_DETECTION */
	}
	UNGCPRO;

	/* #### dmoore - is this reasonable in case of buf being killed above? */
	if (!BUFFER_LIVE_P(buf))
		return;

	signal_before_change(buf, start, end);

#ifdef REGION_CACHE_NEEDS_WORK
	if (buf->newline_cache)
		invalidate_region_cache(buf,
					buf->newline_cache,
					start - BUF_BEG(buf), BUF_Z(buf) - end);
	if (buf->width_run_cache)
		invalidate_region_cache(buf,
					buf->width_run_cache,
					start - BUF_BEG(buf), BUF_Z(buf) - end);
#endif

#if 0				/* FSFmacs */
	Vdeactivate_mark = Qt;
#endif

	MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
		mbuf->point_before_scroll = Qnil;
	}
}

/************************************************************************/
/*                        Insertion of strings                          */
/************************************************************************/

void
fixup_internal_substring(const Bufbyte * nonreloc, Lisp_Object reloc,
			 Bytecount offset, Bytecount * len)
{
	assert((nonreloc && NILP(reloc)) || (!nonreloc && STRINGP(reloc)));

	if (*len < 0) {
		if (nonreloc)
			*len = strlen((const char *)nonreloc) - offset;
		else
			*len = XSTRING_LENGTH(reloc) - offset;
	}
#ifdef ERROR_CHECK_BUFPOS
	assert(*len >= 0);
	if (STRINGP(reloc)) {
		assert(offset >= 0 && offset <= XSTRING_LENGTH(reloc));
		assert(offset + *len <= XSTRING_LENGTH(reloc));
	}
#endif
}

/* Insert a string into BUF at Bufpos POS.  The string data comes
   from one of two sources: constant, non-relocatable data (specified
   in NONRELOC), or a Lisp string object (specified in RELOC), which
   is relocatable and may have extent data that needs to be copied
   into the buffer.  OFFSET and LENGTH specify the substring of the
   data that is actually to be inserted.  As a special case, if POS
   is -1, insert the string at point and move point to the end of the
   string.

   Normally, markers at the insertion point end up before the
   inserted string.  If INSDEL_BEFORE_MARKERS is set in flags, however,
   they end up after the string.

   INSDEL_NO_LOCKING is kludgy and is used when insert-file-contents is
   visiting a new file; it inhibits the locking checks normally done
   before modifying a buffer.  Similar checks were already done
   in the higher-level Lisp functions calling insert-file-contents. */

Charcount
buffer_insert_string_1(struct buffer *buf, Bufpos pos,
		       const Bufbyte * nonreloc, Lisp_Object reloc,
		       Bytecount offset, Bytecount length, int flags)
{
	/* This function can GC */
	struct gcpro gcpro1;
	Bytind ind;
	Charcount cclen;
	int move_point = 0;
	struct buffer *mbuf;
	Lisp_Object bufcons;

	/* Defensive steps just in case a buffer gets deleted and a calling
	   function doesn't notice it. */
	if (!BUFFER_LIVE_P(buf))
		return 0;

	fixup_internal_substring(nonreloc, reloc, offset, &length);

	if (pos == -1) {
		pos = BUF_PT(buf);
		move_point = 1;
	}
#ifdef I18N3
	/* #### See the comment in print_internal().  If this buffer is marked
	   as translatable, then Fgettext() should be called on obj if it
	   is a string. */
#endif

	/* Make sure that point-max won't exceed the size of an emacs int. */
	if ((length + BUF_Z(buf)) > EMACS_INT_MAX)
		error("Maximum buffer size exceeded");

	/* theoretically not necessary -- caller should GCPRO.
	   #### buffer_insert_from_buffer_1() doesn't!  */
	GCPRO1(reloc);

	prepare_to_modify_buffer(buf, pos, pos, !(flags & INSDEL_NO_LOCKING));

	/* Defensive steps in case the before-change-functions fuck around */
	if (!BUFFER_LIVE_P(buf)) {
		UNGCPRO;
		/* Bad bad pre-change function. */
		return 0;
	}

	/* Make args be valid again.  prepare_to_modify_buffer() might have
	   modified the buffer. */
	if (pos < BUF_BEGV(buf))
		pos = BUF_BEGV(buf);
	if (pos > BUF_ZV(buf))
		pos = BUF_ZV(buf);

	/* string may have been relocated up to this point */
	if (STRINGP(reloc))
		nonreloc = XSTRING_DATA(reloc);

	ind = bufpos_to_bytind(buf, pos);
	cclen = bytecount_to_charcount(nonreloc + offset, length);

	if (ind != BI_BUF_GPT(buf))
		/* #### if debug-on-quit is invoked and the user changes the
		   buffer, bad things can happen.  This is a rampant problem
		   in Emacs. */
		move_gap(buf, ind);	/* may QUIT */
	if (!GAP_CAN_HOLD_SIZE_P(buf, length)) {
		if (BUF_END_GAP_SIZE(buf) >= length)
			merge_gap_with_end_gap(buf);
		else
			make_gap(buf, length - BUF_GAP_SIZE(buf));
	}

	insert_invalidate_line_number_cache(buf, pos, nonreloc + offset,
					    length);

	MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
		record_insert(mbuf, pos, cclen);
	}

	BUF_MODIFF(buf)++;
	MARK_BUFFERS_CHANGED;

	/* string may have been relocated up to this point */
	if (STRINGP(reloc))
		nonreloc = XSTRING_DATA(reloc);

	memcpy(BUF_GPT_ADDR(buf), nonreloc + offset, length);

	SET_BUF_GAP_SIZE(buf, BUF_GAP_SIZE(buf) - length);
	SET_BI_BUF_GPT(buf, BI_BUF_GPT(buf) + length);
	MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
		SET_BOTH_BUF_ZV(mbuf, BUF_ZV(mbuf) + cclen,
				BI_BUF_ZV(mbuf) + length);
	}
	SET_BOTH_BUF_Z(buf, BUF_Z(buf) + cclen, BI_BUF_Z(buf) + length);
	SET_GAP_SENTINEL(buf);

#ifdef MULE
	buffer_mule_signal_inserted_region(buf, pos, length, cclen);
#endif

	MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
		process_extents_for_insertion(make_buffer(mbuf), ind, length);
	}

	MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
		/* We know the gap is at IND so the cast is OK. */
		adjust_markers_for_insert(mbuf, (Memind) ind, length);
	}

	/* Point logically doesn't move, but may need to be adjusted because
	   it's a byte index.  point-marker doesn't change because it's a
	   memory index. */
	MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
		if (BI_BUF_PT(mbuf) > ind)
			JUST_SET_POINT(mbuf, BUF_PT(mbuf) + cclen,
				       BI_BUF_PT(mbuf) + length);
	}

	/* Well, point might move. */
	if (move_point)
		BI_BUF_SET_PT(buf, ind + length);

	if (STRINGP(reloc)) {
		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			splice_in_string_extents(reloc, mbuf, ind, length,
						 offset);
		}
	}

	if (flags & INSDEL_BEFORE_MARKERS) {
		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			/* ind - 1 is correct because the FROM argument is exclusive.
			   I formerly used DEC_BYTIND() but that caused problems at the
			   beginning of the buffer. */
			adjust_markers(mbuf, ind - 1, ind, length);
		}
	}

	signal_after_change(buf, pos, pos, pos + cclen);

	UNGCPRO;

	return cclen;
}

/* The following functions are interfaces onto the above function,
   for inserting particular sorts of data.  In all the functions,
   BUF and POS specify the buffer and location where the insertion is
   to take place. (If POS is -1, text is inserted at point and point
   moves forward past the text.) FLAGS is as above. */

Charcount
buffer_insert_raw_string_1(struct buffer * buf, Bufpos pos,
			   const Bufbyte * nonreloc, Bytecount length,
			   int flags)
{
	/* This function can GC */
	return buffer_insert_string_1(buf, pos, nonreloc, Qnil, 0, length,
				      flags);
}

Charcount
buffer_insert_lisp_string_1(struct buffer * buf, Bufpos pos, Lisp_Object str,
			    int flags)
{
	/* This function can GC */
#ifdef ERROR_CHECK_TYPECHECK
	assert(STRINGP(str));
#endif
	return buffer_insert_string_1(buf, pos, 0, str, 0,
				      XSTRING_LENGTH(str), flags);
}

/* Insert the null-terminated string S (in external format). */

Charcount
buffer_insert_c_string_1(struct buffer * buf, Bufpos pos, const char *s,
			 int flags)
{
	/* This function can GC */
	const char *translated = GETTEXT(s);
	return buffer_insert_string_1(buf, pos, (const Bufbyte *)translated,
				      Qnil, 0, strlen(translated), flags);
}

Charcount
buffer_insert_emacs_char_1(struct buffer *buf, Bufpos pos, Emchar ch, int flags)
{
	/* This function can GC */
	Bufbyte str[MAX_EMCHAR_LEN];
	Bytecount len = set_charptr_emchar(str, ch);
	return buffer_insert_string_1(buf, pos, str, Qnil, 0, len, flags);
}

Charcount
buffer_insert_c_char_1(struct buffer * buf, Bufpos pos, char c, int flags)
{
	/* This function can GC */
	return buffer_insert_emacs_char_1(buf, pos, (Emchar) (unsigned char)c,
					  flags);
}

Charcount
buffer_insert_from_buffer_1(struct buffer *buf, Bufpos pos,
			    struct buffer *buf2, Bufpos pos2,
			    Charcount length, int flags)
{
	/* This function can GC */
	Lisp_Object str = make_string_from_buffer(buf2, pos2, length);
	return buffer_insert_string_1(buf, pos, 0, str, 0,
				      XSTRING_LENGTH(str), flags);
}

/************************************************************************/
/*                        Deletion of ranges                            */
/************************************************************************/

/* Delete characters in buffer from FROM up to (but not including) TO.  */

void buffer_delete_range(struct buffer *buf, Bufpos from, Bufpos to, int flags)
{
	/* This function can GC */
	Charcount numdel;
	Bytind bi_from, bi_to;
	Bytecount bc_numdel;
	EMACS_INT shortage;
	struct buffer *mbuf;
	Lisp_Object bufcons;

	/* Defensive steps just in case a buffer gets deleted and a calling
	   function doesn't notice it. */
	if (!BUFFER_LIVE_P(buf))
		return;

	/* Make args be valid */
	if (from < BUF_BEGV(buf))
		from = BUF_BEGV(buf);
	if (to > BUF_ZV(buf))
		to = BUF_ZV(buf);
	if ((numdel = to - from) <= 0)
		return;

	prepare_to_modify_buffer(buf, from, to, !(flags & INSDEL_NO_LOCKING));

	/* Defensive steps in case the before-change-functions fuck around */
	if (!BUFFER_LIVE_P(buf))
		/* Bad bad pre-change function. */
		return;

	/* Make args be valid again.  prepare_to_modify_buffer() might have
	   modified the buffer. */
	if (from < BUF_BEGV(buf))
		from = BUF_BEGV(buf);
	if (to > BUF_ZV(buf))
		to = BUF_ZV(buf);
	if ((numdel = to - from) <= 0)
		return;

	/* Redisplay needs to know if a newline was in the deleted region.
	   If we've already marked the changed region as having a deleted
	   newline there is no use in performing the check. */
	if (!buf->changes->newline_was_deleted) {
		scan_buffer(buf, '\n', from, to, 1, &shortage, 1);
		if (!shortage) {
			MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
				mbuf->changes->newline_was_deleted = 1;
			}
		}
	}

	bi_from = bufpos_to_bytind(buf, from);
	bi_to = bufpos_to_bytind(buf, to);
	bc_numdel = bi_to - bi_from;

	delete_invalidate_line_number_cache(buf, from, to);

	if (to == BUF_Z(buf) && bi_from > BI_BUF_GPT(buf)) {
		/* avoid moving the gap just to delete from the bottom. */

		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			record_delete(mbuf, from, numdel);
		}
		BUF_MODIFF(buf)++;
		MARK_BUFFERS_CHANGED;

		/* #### Point used to be modified here, but this causes problems
		   with MULE, as point is used to calculate bytinds, and if the
		   offset in bc_numdel causes point to move to a non first-byte
		   location, causing some other function to throw an assertion
		   in ASSERT_VALID_BYTIND. I've moved the code to right after
		   the other movements and adjustments, but before the gap is
		   moved.  -- jh 970813 */

		/* Detach any extents that are completely within the range [FROM, TO],
		   if the extents are detachable.

		   This must come AFTER record_delete(), so that the appropriate
		   extents will be present to be recorded, and BEFORE the gap
		   size is increased, as otherwise we will be confused about
		   where the extents end. */
		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			process_extents_for_deletion(make_buffer(mbuf), bi_from,
						     bi_to, 0);
		}

		/* Relocate all markers pointing into the new, larger gap to
		   point at the end of the text before the gap.  */
		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			adjust_markers(mbuf,
				       (bi_to + BUF_GAP_SIZE(mbuf)),
				       (bi_to + BUF_GAP_SIZE(mbuf)),
				       (-bc_numdel));
		}

		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			/* Relocate any extent endpoints just like markers. */
			adjust_extents_for_deletion(make_buffer(mbuf), bi_from,
						    bi_to, BUF_GAP_SIZE(mbuf),
						    bc_numdel, 0);
		}

		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			/* Relocate point as if it were a marker.  */
			if (bi_from < BI_BUF_PT(mbuf)) {
				if (BI_BUF_PT(mbuf) < bi_to)
					JUST_SET_POINT(mbuf, from, bi_from);
				else
					JUST_SET_POINT(mbuf,
						       BUF_PT(mbuf) - numdel,
						       BI_BUF_PT(mbuf) -
						       bc_numdel);
			}
		}

		SET_BUF_END_GAP_SIZE(buf, BUF_END_GAP_SIZE(buf) + bc_numdel);

		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			SET_BOTH_BUF_ZV(mbuf, BUF_ZV(mbuf) - numdel,
					BI_BUF_ZV(mbuf) - bc_numdel);
		}
		SET_BOTH_BUF_Z(buf, BUF_Z(buf) - numdel,
			       BI_BUF_Z(buf) - bc_numdel);
		SET_GAP_SENTINEL(buf);
	} else {
		/* Make sure the gap is somewhere in or next to what we are deleting.  */
		if (bi_to < BI_BUF_GPT(buf))
			gap_left(buf, bi_to);
		if (bi_from > BI_BUF_GPT(buf))
			gap_right(buf, bi_from);

		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			record_delete(mbuf, from, numdel);
		}
		BUF_MODIFF(buf)++;
		MARK_BUFFERS_CHANGED;

		/* #### Point used to be modified here, but this causes problems
		   with MULE, as point is used to calculate bytinds, and if the
		   offset in bc_numdel causes point to move to a non first-byte
		   location, causing some other function to throw an assertion
		   in ASSERT_VALID_BYTIND. I've moved the code to right after
		   the other movements and adjustments, but before the gap is
		   moved.  -- jh 970813 */

		/* Detach any extents that are completely within the range [FROM, TO],
		   if the extents are detachable.

		   This must come AFTER record_delete(), so that the appropriate extents
		   will be present to be recorded, and BEFORE the gap size is increased,
		   as otherwise we will be confused about where the extents end. */
		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			process_extents_for_deletion(make_buffer(mbuf), bi_from,
						     bi_to, 0);
		}

		/* Relocate all markers pointing into the new, larger gap to
		   point at the end of the text before the gap.  */
		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			adjust_markers(mbuf,
				       (bi_to + BUF_GAP_SIZE(mbuf)),
				       (bi_to + BUF_GAP_SIZE(mbuf)),
				       (-bc_numdel - BUF_GAP_SIZE(mbuf)));
		}

		/* Relocate any extent endpoints just like markers. */
		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			adjust_extents_for_deletion(make_buffer(mbuf), bi_from,
						    bi_to, BUF_GAP_SIZE(mbuf),
						    bc_numdel,
						    BUF_GAP_SIZE(mbuf));
		}

		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			/* Relocate point as if it were a marker.  */
			if (bi_from < BI_BUF_PT(mbuf)) {
				if (BI_BUF_PT(mbuf) < bi_to)
					JUST_SET_POINT(mbuf, from, bi_from);
				else
					JUST_SET_POINT(mbuf,
						       BUF_PT(mbuf) - numdel,
						       BI_BUF_PT(mbuf) -
						       bc_numdel);
			}
		}

		SET_BUF_GAP_SIZE(buf, BUF_GAP_SIZE(buf) + bc_numdel);
		MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
			SET_BOTH_BUF_ZV(mbuf, BUF_ZV(mbuf) - numdel,
					BI_BUF_ZV(mbuf) - bc_numdel);
		}
		SET_BOTH_BUF_Z(buf, BUF_Z(buf) - numdel,
			       BI_BUF_Z(buf) - bc_numdel);
		SET_BI_BUF_GPT(buf, bi_from);
		SET_GAP_SENTINEL(buf);
	}

#ifdef MULE
	buffer_mule_signal_deleted_region(buf, from, to, bi_from, bi_to);
#endif

#ifdef ERROR_CHECK_EXTENTS
	MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
		sledgehammer_extent_check(make_buffer(mbuf));
	}
#endif

	signal_after_change(buf, from, to, from);
}

/************************************************************************/
/*                    Replacement of characters                         */
/************************************************************************/

/* Replace the character at POS in buffer B with CH. */

void
buffer_replace_char(struct buffer *buf, Bufpos pos, Emchar ch,
		    int not_real_change, int force_lock_check)
{
	/* This function can GC */
	Bufbyte curstr[MAX_EMCHAR_LEN];
	Bufbyte newstr[MAX_EMCHAR_LEN];
	Bytecount curlen, newlen;

	/* Defensive steps just in case a buffer gets deleted and a calling
	   function doesn't notice it. */
	if (!BUFFER_LIVE_P(buf))
		return;

	curlen = BUF_CHARPTR_COPY_CHAR(buf, pos, curstr);
	newlen = set_charptr_emchar(newstr, ch);

	if (curlen == newlen) {
		struct buffer *mbuf;
		Lisp_Object bufcons;

		/* then we can just replace the text. */
		prepare_to_modify_buffer(buf, pos, pos + 1,
					 !not_real_change || force_lock_check);
		/* Defensive steps in case the before-change-functions fuck around */
		if (!BUFFER_LIVE_P(buf))
			/* Bad bad pre-change function. */
			return;

		/* Make args be valid again.  prepare_to_modify_buffer() might have
		   modified the buffer. */
		if (pos < BUF_BEGV(buf))
			pos = BUF_BEGV(buf);
		if (pos >= BUF_ZV(buf))
			pos = BUF_ZV(buf) - 1;
		if (pos < BUF_BEGV(buf))
			/* no more characters in buffer! */
			return;

		if (BUF_FETCH_CHAR(buf, pos) == '\n') {
			MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
				mbuf->changes->newline_was_deleted = 1;
			}
		}
		MARK_BUFFERS_CHANGED;
		if (!not_real_change) {
			MAP_INDIRECT_BUFFERS(buf, mbuf, bufcons) {
				record_change(mbuf, pos, 1);
			}
			BUF_MODIFF(buf)++;
		}
		memcpy(BUF_BYTE_ADDRESS(buf, pos), newstr, newlen);

		signal_after_change(buf, pos, pos + 1, pos + 1);

		/* We do not have to adjust the Mule data; we just replaced a
		   character with another of the same number of bytes. */
	} else {
		/*
		 * Must implement as deletion followed by insertion.
		 *
		 * Make a note to move point forward later in the one situation
		 * where it is needed, a delete/insert one position behind
		 * point.  Point will drift backward by one position and stay
		 * there otherwise.
		 */
		int movepoint = (pos == BUF_PT(buf) - 1);

		buffer_delete_range(buf, pos, pos + 1, 0);
		/* Defensive steps in case the before-change-functions fuck around */
		if (!BUFFER_LIVE_P(buf))
			/* Bad bad pre-change function. */
			return;

		/* Make args be valid again.  prepare_to_modify_buffer() might have
		   modified the buffer. */
		if (pos < BUF_BEGV(buf))
			pos = BUF_BEGV(buf);
		if (pos >= BUF_ZV(buf))
			pos = BUF_ZV(buf) - 1;
		if (pos < BUF_BEGV(buf))
			/* no more characters in buffer! */
			return;
		/*
		 * -1 as the pos argument means to move point forward with the
		 * insertion, which we must do if the deletion moved point
		 * backward so that it now equals the insertion point.
		 */
		buffer_insert_string_1(buf, (movepoint ? -1 : pos),
				       newstr, Qnil, 0, newlen, 0);
	}
}

/************************************************************************/
/*                            Other functions                           */
/************************************************************************/

/* Make a string from a buffer.  This needs to take into account the gap,
   and add any necessary extents from the buffer. */

static Lisp_Object
make_string_from_buffer_1(struct buffer *buf, Bufpos pos, Charcount length,
			  int no_extents)
{
	/* This function can GC */
	Bytind bi_ind = bufpos_to_bytind(buf, pos);
	Bytecount bi_len = bufpos_to_bytind(buf, pos + length) - bi_ind;
	Lisp_Object val = make_uninit_string(bi_len);

	struct gcpro gcpro1;
	GCPRO1(val);

	if (!no_extents)
		add_string_extents(val, buf, bi_ind, bi_len);

	{
		Bytecount len1 = BI_BUF_GPT(buf) - bi_ind;
		Bufbyte *start1 = BI_BUF_BYTE_ADDRESS(buf, bi_ind);
		Bufbyte *dest = XSTRING_DATA(val);

		if (len1 < 0) {
			/* Completely after gap */
			memcpy(dest, start1, bi_len);
		} else if (bi_len <= len1) {
			/* Completely before gap */
			memcpy(dest, start1, bi_len);
		} else {
			/* Spans gap */
			Bytind pos2 = bi_ind + len1;
			Bufbyte *start2 = BI_BUF_BYTE_ADDRESS(buf, pos2);

			memcpy(dest, start1, len1);
			memcpy(dest + len1, start2, bi_len - len1);
		}
	}

	UNGCPRO;
	return val;
}

Lisp_Object
make_string_from_buffer(struct buffer * buf, Bufpos pos, Charcount length)
{
	return make_string_from_buffer_1(buf, pos, length, 0);
}

Lisp_Object
make_string_from_buffer_no_extents(struct buffer * buf, Bufpos pos,
				   Charcount length)
{
	return make_string_from_buffer_1(buf, pos, length, 1);
}

void barf_if_buffer_read_only(struct buffer *buf, Bufpos from, Bufpos to)
{
	Lisp_Object buffer;
	Lisp_Object iro;

	XSETBUFFER(buffer, buf);
      back:
	iro = (buf == current_buffer ? Vinhibit_read_only :
	       symbol_value_in_buffer(Qinhibit_read_only, buffer));
	if (!LISTP(iro))
		return;
	if (NILP(iro) && !NILP(buf->read_only)) {
		Fsignal(Qbuffer_read_only, (list1(buffer)));
		goto back;
	}
	if (from > 0) {
		if (to < 0)
			to = from;
		verify_extent_modification(buffer,
					   bufpos_to_bytind(buf, from),
					   bufpos_to_bytind(buf, to), iro);
	}
}

void
find_charsets_in_bufbyte_string(unsigned char *charsets, const Bufbyte * str,
				Bytecount len)
{
#ifndef MULE
	/* Telescope this. */
	charsets[0] = 1;
#else
	const Bufbyte *strend = str + len;
	memset(charsets, 0, NUM_LEADING_BYTES);

	/* #### SJT doesn't like this. */
	if (len == 0) {
		charsets[XCHARSET_LEADING_BYTE(Vcharset_ascii) - 128] = 1;
		return;
	}

	while (str < strend) {
		charsets[CHAR_LEADING_BYTE(charptr_emchar(str)) - 128] = 1;
		INC_CHARPTR(str);
	}
#endif
}

void
find_charsets_in_emchar_string(unsigned char *charsets, const Emchar * str,
			       Charcount len)
{
#ifndef MULE
	/* Telescope this. */
	charsets[0] = 1;
#else
	int i;

	memset(charsets, 0, NUM_LEADING_BYTES);

	/* #### SJT doesn't like this. */
	if (len == 0) {
		charsets[XCHARSET_LEADING_BYTE(Vcharset_ascii) - 128] = 1;
		return;
	}

	for (i = 0; i < len; i++) {
		charsets[CHAR_LEADING_BYTE(str[i]) - 128] = 1;
	}
#endif
}

int bufbyte_string_displayed_columns(const Bufbyte * str, Bytecount len)
{
	int cols = 0;
	const Bufbyte *end = str + len;

	while (str < end) {
#ifdef MULE
		Emchar ch = charptr_emchar(str);
		Lisp_Object tmp = CHAR_CHARSET(ch);
		cols += XCHARSET_COLUMNS(tmp);
#else
		cols++;
#endif
		INC_CHARPTR(str);
	}

	return cols;
}

int emchar_string_displayed_columns(const Emchar * str, Charcount len)
{
#ifdef MULE
	int cols = 0;
	int i;

	for (i = 0; i < len; i++) {
		Lisp_Object tmp = CHAR_CHARSET(str[i]);
		cols += XCHARSET_COLUMNS(tmp);
	}
	return cols;
#else				/* not MULE */
	return len;
#endif
}

/* NOTE: Does not reset the Dynarr. */

void
convert_bufbyte_string_into_emchar_dynarr(const Bufbyte * str, Bytecount len,
					  Emchar_dynarr * dyn)
{
	const Bufbyte *strend = str + len;

	while (str < strend) {
		Emchar ch = charptr_emchar(str);
		Dynarr_add(dyn, ch);
		INC_CHARPTR(str);
	}
}

Charcount
convert_bufbyte_string_into_emchar_string(const Bufbyte * str, Bytecount len,
					  Emchar * arr)
{
	const Bufbyte *strend = str + len;
	Charcount newlen = 0;
	while (str < strend) {
		Emchar ch = charptr_emchar(str);
		arr[newlen++] = ch;
		INC_CHARPTR(str);
	}
	return newlen;
}

/* Convert an array of Emchars into the equivalent string representation.
   Store into the given Bufbyte dynarr.  Does not reset the dynarr.
   Does not add a terminating zero. */

void
convert_emchar_string_into_bufbyte_dynarr(Emchar * arr, int nels,
					  Bufbyte_dynarr * dyn)
{
	Bufbyte str[MAX_EMCHAR_LEN];
	int i;

	for (i = 0; i < nels; i++) {
		Bytecount len = set_charptr_emchar(str, arr[i]);
		Dynarr_add_many(dyn, str, len);
	}
}

/* Convert an array of Emchars into the equivalent string representation.
   Malloc the space needed for this and return it.  If LEN_OUT is not a
   NULL pointer, store into LEN_OUT the number of Bufbytes in the
   malloc()ed string.  Note that the actual number of Bufbytes allocated
   is one more than this: the returned string is zero-terminated. */

Bufbyte *convert_emchar_string_into_malloced_string(Emchar * arr, int nels,
						    Bytecount * len_out)
{
	/* Damn zero-termination. */
	Bufbyte *str = (Bufbyte *) alloca(nels * MAX_EMCHAR_LEN + 1);
	Bufbyte *strorig = str;
	Bytecount len;

	int i;

	for (i = 0; i < nels; i++) {
		str += set_charptr_emchar(str, arr[i]);
	}
	*str = '\0';
	len = str - strorig;
	str = (Bufbyte *) xmalloc_atomic(1 + len);
	memcpy(str, strorig, 1 + len);
	if (len_out) {
		*len_out = len;
	}
	return str;
}

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void reinit_vars_of_insdel(void)
{
	int i;

	inside_change_hook = 0;
	in_first_change = 0;

	for (i = 0; i <= MAX_BYTIND_GAP_SIZE_3; i++)
		three_to_one_table[i] = i / 3;
}

void vars_of_insdel(void)
{
	reinit_vars_of_insdel();
}

void init_buffer_text(struct buffer *b)
{
	if (!b->base_buffer) {
		SET_BUF_GAP_SIZE(b, 20);
		(void)BUFFER_ALLOC(
			b->text->beg, BUF_GAP_SIZE(b) + BUF_END_SENTINEL_SIZE);
		if (!BUF_BEG_ADDR(b))
			memory_full();

		SET_BUF_END_GAP_SIZE(b, 0);
		SET_BI_BUF_GPT(b, 1);
		SET_BOTH_BUF_Z(b, 1, 1);
		SET_GAP_SENTINEL(b);
		SET_END_SENTINEL(b);
#ifdef MULE
		{
			int i;

			b->text->mule_bufmin = b->text->mule_bufmax = 1;
			b->text->mule_bytmin = b->text->mule_bytmax = 1;
			b->text->mule_shifter = 0;
			b->text->mule_three_p = 0;

			for (i = 0; i < 16; i++) {
				b->text->mule_bufpos_cache[i] = 1;
				b->text->mule_bytind_cache[i] = 1;
			}
		}
#endif				/* MULE */
		b->text->line_number_cache = Qnil;

		BUF_MODIFF(b) = 1;
		BUF_SAVE_MODIFF(b) = 1;

		JUST_SET_POINT(b, 1, 1);
		SET_BOTH_BUF_BEGV(b, 1, 1);
		SET_BOTH_BUF_ZV(b, 1, 1);

		b->text->changes =
		    xnew_and_zero(struct buffer_text_change_data);
	} else {
		JUST_SET_POINT(b, BUF_PT(b->base_buffer),
			       BI_BUF_PT(b->base_buffer));
		SET_BOTH_BUF_BEGV(b, BUF_BEGV(b->base_buffer),
				  BI_BUF_BEGV(b->base_buffer));
		SET_BOTH_BUF_ZV(b, BUF_ZV(b->base_buffer),
				BI_BUF_ZV(b->base_buffer));
	}

	b->changes = xnew_and_zero(struct each_buffer_change_data);
	BUF_FACECHANGE(b) = 1;

#ifdef REGION_CACHE_NEEDS_WORK
	b->newline_cache = 0;
	b->width_run_cache = 0;
	b->width_table = Qnil;
#endif
}

void uninit_buffer_text(struct buffer *b)
{
	if (!b->base_buffer) {
		BUFFER_FREE(b->text->beg);
		xfree(b->text->changes);
	}
	xfree(b->changes);

#ifdef REGION_CACHE_NEEDS_WORK
	if (b->newline_cache) {
		free_region_cache(b->newline_cache);
		b->newline_cache = 0;
	}
	if (b->width_run_cache) {
		free_region_cache(b->width_run_cache);
		b->width_run_cache = 0;
	}
	b->width_table = Qnil;
#endif
}
