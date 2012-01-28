/* Header file for the buffer manipulation primitives.
   Copyright (C) 1985, 1986, 1992, 1993, 1994, 1995
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


/* Synched up with: FSF 19.30. */

/* Authorship:

   FSF: long ago.
   JWZ: separated out bufslots.h, early in Lemacs.
   Ben Wing: almost completely rewritten for Mule, 19.12.
 */

#ifndef INCLUDED_buffer_h_
#define INCLUDED_buffer_h_

#ifdef MULE
#include "mule/mule-charset.h"
#endif

#include "casetab.h"
#include "chartab.h"

/************************************************************************/
/*                                                                      */
/*                    definition of Lisp buffer object                  */
/*                                                                      */
/************************************************************************/

/* Note: we keep both Bytind and Bufpos versions of some of the
   important buffer positions because they are accessed so much.
   If we didn't do this, we would constantly be invalidating the
   bufpos<->bytind cache under Mule.

   Note that under non-Mule, both versions will always be the
   same so we don't really need to keep track of them.  But it
   simplifies the logic to go ahead and do so all the time and
   the memory loss is insignificant. */

/* Formerly, it didn't much matter what went inside the struct buffer_text
   and what went outside it.  Now it does, with the advent of "indirect
   buffers" that share text with another buffer.  An indirect buffer
   shares the same *text* as another buffer, but has its own buffer-local
   variables, its own accessible region, and its own markers and extents.
   (Due to the nature of markers, it doesn't actually matter much whether
   we stick them inside or out of the struct buffer_text -- the user won't
   notice any difference -- but we go ahead and put them outside for
   consistency and overall saneness of algorithm.)

   FSFmacs gets away with not maintaining any "children" pointers from
   a buffer to the indirect buffers that refer to it by putting the
   markers inside of the struct buffer_text, using markers to keep track
   of BEGV and ZV in indirect buffers, and relying on the fact that
   all intervals (text properties and overlays) use markers for their
   start and end points.  We don't do this for extents (markers are
   inefficient anyway and take up space), so we have to maintain
   children pointers.  This is not terribly hard, though, and the
   code to maintain this is just like the code already present in
   extent-parent and extent-children.
   */

struct buffer_text {
	Bufbyte *beg;		/* Actual address of buffer contents. */
	Bytind gpt;		/* Index of gap in buffer. */
	Bytind z;		/* Index of end of buffer. */
	Bufpos bufz;		/* Equivalent as a Bufpos. */
	int gap_size;		/* Size of buffer's gap */
	int end_gap_size;	/* Size of buffer's end gap */
	long modiff;		/* This counts buffer-modification events
				   for this buffer.  It is incremented for
				   each such event, and never otherwise
				   changed.  */
	long save_modiff;	/* Previous value of modiff, as of last
				   time buffer visited or saved a file.  */

#ifdef MULE
	/* We keep track of a "known" region for very fast access.
	   This information is text-only so it goes here. */
	Bufpos mule_bufmin, mule_bufmax;
	Bytind mule_bytmin, mule_bytmax;
	int mule_shifter, mule_three_p;

	/* And we also cache 16 positions for fairly fast access near those
	   positions. */
	Bufpos mule_bufpos_cache[16];
	Bytind mule_bytind_cache[16];
#endif

	/* Similar to the above, we keep track of positions for which line
	   number has last been calculated.  See line-number.c. */
	Lisp_Object line_number_cache;

	/* Change data that goes with the text. */
	struct buffer_text_change_data *changes;

};

struct buffer {
	struct lcrecord_header header;

	/* This structure holds the coordinates of the buffer contents
	   in ordinary buffers.  In indirect buffers, this is not used.  */
	struct buffer_text own_text;

	/* This points to the `struct buffer_text' that is used for this buffer.
	   In an ordinary buffer, this is the own_text field above.
	   In an indirect buffer, this is the own_text field of another buffer.  */
	struct buffer_text *text;

	Bytind pt;		/* Position of point in buffer. */
	Bufpos bufpt;		/* Equivalent as a Bufpos. */
	Bytind begv;		/* Index of beginning of accessible range. */
	Bufpos bufbegv;		/* Equivalent as a Bufpos. */
	Bytind zv;		/* Index of end of accessible range. */
	Bufpos bufzv;		/* Equivalent as a Bufpos. */

	int face_change;	/* This is set when a change in how the text should
				   be displayed (e.g., font, color) is made. */

	/* Whether buffer specific face is specified. */
	int buffer_local_face_property;

	/* change data indicating what portion of the text has changed
	   since the last time this was reset.  Used by redisplay.
	   Logically we should keep this with the text structure, but
	   redisplay resets it for each buffer individually and we don't
	   want interference between an indirect buffer and its base
	   buffer. */
	struct each_buffer_change_data *changes;

#ifdef REGION_CACHE_NEEDS_WORK
	/* If the long line scan cache is enabled (i.e. the buffer-local
	   variable cache-long-line-scans is non-nil), newline_cache
	   points to the newline cache, and width_run_cache points to the
	   width run cache.

	   The newline cache records which stretches of the buffer are
	   known *not* to contain newlines, so that they can be skipped
	   quickly when we search for newlines.

	   The width run cache records which stretches of the buffer are
	   known to contain characters whose widths are all the same.  If
	   the width run cache maps a character to a value > 0, that value
	   is the character's width; if it maps a character to zero, we
	   don't know what its width is.  This allows compute_motion to
	   process such regions very quickly, using algebra instead of
	   inspecting each character.  See also width_table, below.  */
	struct region_cache *newline_cache;
	struct region_cache *width_run_cache;
#endif				/* REGION_CACHE_NEEDS_WORK */

	/* The markers that refer to this buffer.  This is actually a single
	   marker -- successive elements in its marker `chain' are the other
	   markers referring to this buffer */
	Lisp_Marker *markers;

	/* The buffer's extent info.  This is its own type, an extent-info
	   object (done this way for ease in marking / finalizing). */
	Lisp_Object extent_info;

	/* ----------------------------------------------------------------- */
	/* All the stuff above this line is the responsibility of insdel.c,
	   with some help from marker.c and extents.c.
	   All the stuff below this line is the responsibility of buffer.c. */

	/* In an indirect buffer, this points to the base buffer.
	   In an ordinary buffer, it is 0.
	   We DO mark through this slot. */
	struct buffer *base_buffer;

	/* List of indirect buffers whose base is this buffer.
	   If we are an indirect buffer, this will be nil.
	   Do NOT mark through this. */
	Lisp_Object indirect_children;

	/* Flags saying which DEFVAR_PER_BUFFER variables
	   are local to this buffer.  */
	int local_var_flags;

	/* Set to the modtime of the visited file when read or written.
	   -1 means visited file was nonexistent.
	   0  means visited file modtime unknown; in no case complain
	   about any mismatch on next save attempt.  */
	int modtime;

	/* the value of text->modiff at the last auto-save.  */
	long auto_save_modified;

	/* The time at which we detected a failure to auto-save,
	   Or -1 if we didn't have a failure.  */
	int auto_save_failure_time;

	/* Position in buffer at which display started
	   the last time this buffer was displayed.  */
	int last_window_start;

	/* Everything from here down must be a Lisp_Object */

#define MARKED_SLOT(x) Lisp_Object x
#include "bufslots.h"
#undef MARKED_SLOT
};

DECLARE_LRECORD(buffer, struct buffer);
#define XBUFFER(x) XRECORD (x, buffer, struct buffer)
#define XSETBUFFER(x, p) XSETRECORD (x, p, buffer)
#define BUFFERP(x) RECORDP (x, buffer)
#define CHECK_BUFFER(x) CHECK_RECORD (x, buffer)
#define CONCHECK_BUFFER(x) CONCHECK_RECORD (x, buffer)

#define BUFFER_LIVE_P(b) (!NILP ((b)->name))

#define CHECK_LIVE_BUFFER(x) do {			\
  CHECK_BUFFER (x);					\
  if (!BUFFER_LIVE_P (XBUFFER (x)))			\
    dead_wrong_type_argument (Qbuffer_live_p, (x));	\
} while (0)

#define CONCHECK_LIVE_BUFFER(x) do {			\
  CONCHECK_BUFFER (x);					\
  if (!BUFFER_LIVE_P (XBUFFER (x)))			\
    x = wrong_type_argument (Qbuffer_live_p, (x));	\
} while (0)

#define BUFFER_BASE_BUFFER(b) ((b)->base_buffer ? (b)->base_buffer : (b))

/* Map over buffers sharing the same text as MPS_BUF.  MPS_BUFVAR is a
   variable that gets the buffer values (beginning with the base
   buffer, then the children), and MPS_BUFCONS should be a temporary
   Lisp_Object variable.  */
#define MAP_INDIRECT_BUFFERS(mps_buf, mps_bufvar, mps_bufcons)			\
for (mps_bufcons = Qunbound,							\
     mps_bufvar = BUFFER_BASE_BUFFER (mps_buf);					\
     UNBOUNDP (mps_bufcons) ?							\
	(mps_bufcons = mps_bufvar->indirect_children,				\
	1)									\
       : (!NILP (mps_bufcons)							\
	  && (mps_bufvar = XBUFFER (XCAR (mps_bufcons)), 1)			\
	  && (mps_bufcons = XCDR (mps_bufcons), 1));				\
     )

/************************************************************************/
/*									*/
/*		   working with raw internal-format data		*/
/*									*/
/************************************************************************/

/* NOTE: In all the following macros, we follow these rules concerning
   multiple evaluation of the arguments:

   1) Anything that's an lvalue can be evaluated more than once.
   2) Anything that's a Lisp Object can be evaluated more than once.
      This should probably be changed, but this follows the way
      that all the macros in lisp.h do things.
   3) 'struct buffer *' arguments can be evaluated more than once.
   4) Nothing else can be evaluated more than once.  Use inline
      functions, if necessary, to prevent multiple evaluation.
   5) An exception to (4) is that there are some macros below that
      may evaluate their arguments more than once.  They are all
      denoted with the word "unsafe" in their name and are generally
      meant to be called only by other macros that have already
      stored the calling values in temporary variables.

   Use the following functions/macros on contiguous strings of data.
   If the text you're operating on is known to come from a buffer, use
   the buffer-level functions below -- they know about the gap and may
   be more efficient.

  (A) For working with charptr's (pointers to internally-formatted text):
  -----------------------------------------------------------------------

   VALID_CHARPTR_P (ptr):
	Given a charptr, does it point to the beginning of a character?

   ASSERT_VALID_CHARPTR (ptr):
	If error-checking is enabled, assert that the given charptr
	points to the beginning of a character.	 Otherwise, do nothing.

   INC_CHARPTR (ptr):
	Given a charptr (assumed to point at the beginning of a character),
	modify that pointer so it points to the beginning of the next
	character.

   DEC_CHARPTR (ptr):
	Given a charptr (assumed to point at the beginning of a
	character or at the very end of the text), modify that pointer
	so it points to the beginning of the previous character.

   VALIDATE_CHARPTR_BACKWARD (ptr):
	Make sure that PTR is pointing to the beginning of a character.
	If not, back up until this is the case.	  Note that there are not
	too many places where it is legitimate to do this sort of thing.
	It's an error if you're passed an "invalid" char * pointer.
	NOTE: PTR *must* be pointing to a valid part of the string (i.e.
	not the very end, unless the string is zero-terminated or
	something) in order for this function to not cause crashes.

   VALIDATE_CHARPTR_FORWARD (ptr):
	Make sure that PTR is pointing to the beginning of a character.
	If not, move forward until this is the case.  Note that there
	are not too many places where it is legitimate to do this sort
	of thing.  It's an error if you're passed an "invalid" char *
	pointer.

   (B) For working with the length (in bytes and characters) of a
       section of internally-formatted text:
   --------------------------------------------------------------

   bytecount_to_charcount (ptr, nbi):
	Given a pointer to a text string and a length in bytes,
	return the equivalent length in characters.

   charcount_to_bytecount (ptr, nch):
	Given a pointer to a text string and a length in characters,
	return the equivalent length in bytes.

   charptr_n_addr (ptr, n):
	Return a pointer to the beginning of the character offset N
	(in characters) from PTR.

   (C) For retrieving or changing the character pointed to by a charptr:
   ---------------------------------------------------------------------

   charptr_emchar (ptr):
	Retrieve the character pointed to by PTR as an Emchar.

   charptr_emchar_n (ptr, n):
	Retrieve the character at offset N (in characters) from PTR,
	as an Emchar.

   set_charptr_emchar (ptr, ch):
	Store the character CH (an Emchar) as internally-formatted
	text starting at PTR.  Return the number of bytes stored.

   charptr_copy_char (ptr, ptr2):
	Retrieve the character pointed to by PTR and store it as
	internally-formatted text in PTR2.

   (D) For working with Emchars:
   -----------------------------

   [Note that there are other functions/macros for working with Emchars
    in mule-charset.h, for retrieving the charset of an Emchar
    and such.  These are only valid when MULE is defined.]

   valid_char_p (ch):
	Return whether the given Emchar is valid.

   CHARP (ch):
	Return whether the given Lisp_Object is a character.

   CHECK_CHAR_COERCE_INT (ch):
	Signal an error if CH is not a valid character or integer Lisp_Object.
	If CH is an integer Lisp_Object, convert it to a character Lisp_Object,
	but merely by repackaging, without performing tests for char validity.

   MAX_EMCHAR_LEN:
	Maximum number of buffer bytes per Emacs character.

*/

/* ---------------------------------------------------------------------- */
/* (A) For working with charptr's (pointers to internally-formatted text) */
/* ---------------------------------------------------------------------- */

#ifdef MULE
# define VALID_CHARPTR_P(ptr) BUFBYTE_FIRST_BYTE_P(*(const unsigned char*)ptr)
#else
# define VALID_CHARPTR_P(ptr) 1
#endif

#ifdef ERROR_CHECK_BUFPOS
# define ASSERT_VALID_CHARPTR(ptr) assert (VALID_CHARPTR_P (ptr))
#else
# define ASSERT_VALID_CHARPTR(ptr)
#endif

/* Note that INC_CHARPTR() and DEC_CHARPTR() have to be written in
   completely separate ways.  INC_CHARPTR() cannot use the DEC_CHARPTR()
   trick of looking for a valid first byte because it might run off
   the end of the string.  DEC_CHARPTR() can't use the INC_CHARPTR()
   method because it doesn't have easy access to the first byte of
   the character it's moving over. */

#define REAL_INC_CHARPTR(ptr)						\
	((void)((ptr) +=						\
		REP_BYTES_BY_FIRST_BYTE (*(const unsigned char*)(ptr))))

#define REAL_INC_CHARBYTIND(ptr, pos)					\
	(pos += REP_BYTES_BY_FIRST_BYTE(*(const unsigned char*)(ptr)))

#define REAL_DEC_CHARPTR(ptr)			\
	do {					\
		(ptr)--;			\
	} while (!VALID_CHARPTR_P(ptr))

#ifdef ERROR_CHECK_BUFPOS
#define INC_CHARPTR(ptr)			\
	do {					\
		ASSERT_VALID_CHARPTR (ptr);	\
		REAL_INC_CHARPTR (ptr);		\
	} while (0)

#define INC_CHARBYTIND(ptr, pos)		\
	do {					\
		ASSERT_VALID_CHARPTR (ptr);	\
		REAL_INC_CHARBYTIND (ptr, pos);	\
	} while (0)

#define DEC_CHARPTR(ptr)					\
	do {							\
		const Bufbyte *const dc_ptr1 = (ptr);		\
		const Bufbyte *dc_ptr2 = dc_ptr1;		\
		REAL_DEC_CHARPTR(dc_ptr2);			\
		assert(dc_ptr1 - dc_ptr2 ==			\
		       REP_BYTES_BY_FIRST_BYTE(*dc_ptr2));	\
		(ptr) = (const Bufbyte*)dc_ptr2;		\
	} while (0)

#else				/* ! ERROR_CHECK_BUFPOS */
#define INC_CHARBYTIND(ptr, pos) REAL_INC_CHARBYTIND (ptr, pos)
#define INC_CHARPTR(ptr) REAL_INC_CHARPTR (ptr)
#define DEC_CHARPTR(ptr) REAL_DEC_CHARPTR (ptr)
#endif				/* ! ERROR_CHECK_BUFPOS */

#ifdef MULE

#define VALIDATE_CHARPTR_BACKWARD(ptr)			\
	do {						\
		while (!VALID_CHARPTR_P (ptr)) ptr--;	\
	} while (0)

/* This needs to be trickier to avoid the possibility of running off
   the end of the string. */

#define VALIDATE_CHARPTR_FORWARD(ptr)		\
	do {						\
		Bufbyte *vcf_ptr = (ptr);		\
		VALIDATE_CHARPTR_BACKWARD (vcf_ptr);	\
		if (vcf_ptr != (ptr))			\
		{					\
			(ptr) = vcf_ptr;		\
			INC_CHARPTR (ptr);		\
		}					\
	} while (0)

#else				/* not MULE */
#define VALIDATE_CHARPTR_BACKWARD(ptr)
#define VALIDATE_CHARPTR_FORWARD(ptr)
#endif				/* not MULE */

/* -------------------------------------------------------------- */
/* (B) For working with the length (in bytes and characters) of a */
/*     section of internally-formatted text			  */
/* -------------------------------------------------------------- */

static inline const Bufbyte*
charptr_n_addr(const Bufbyte * ptr, Charcount offset);
static inline const Bufbyte*
charptr_n_addr(const Bufbyte * ptr, Charcount offset)
{
	return ptr + charcount_to_bytecount(ptr, offset);
}

/* -------------------------------------------------------------------- */
/* (C) For retrieving or changing the character pointed to by a charptr */
/* -------------------------------------------------------------------- */

#define simple_charptr_emchar(ptr)		((Emchar) (ptr)[0])
#define simple_set_charptr_emchar(ptr, x)	((ptr)[0] = (Bufbyte) (x), 1)
#define simple_charptr_copy_char(ptr, ptr2)	((ptr2)[0] = *(ptr), 1)

#ifdef MULE

Emchar non_ascii_charptr_emchar(const Bufbyte * ptr);
Bytecount non_ascii_set_charptr_emchar(Bufbyte * ptr, Emchar c);
Bytecount non_ascii_charptr_copy_char(const Bufbyte * src, Bufbyte * dst);

extern_inline Emchar charptr_emchar(const Bufbyte * ptr);
extern_inline Emchar charptr_emchar(const Bufbyte * ptr)
{
	return BYTE_ASCII_P(*ptr) ?
	    simple_charptr_emchar(ptr) : non_ascii_charptr_emchar(ptr);
}

extern_inline Bytecount set_charptr_emchar(Bufbyte * ptr, Emchar x);
extern_inline Bytecount set_charptr_emchar(Bufbyte * ptr, Emchar x)
{
	return !CHAR_MULTIBYTE_P(x) ?
	    simple_set_charptr_emchar(ptr, x) :
	    non_ascii_set_charptr_emchar(ptr, x);
}

/* Copy the character pointed to by SRC into DST.
   Return the number of bytes copied.  */
extern_inline Bytecount charptr_copy_char(const Bufbyte * src, Bufbyte * dst);
extern_inline Bytecount charptr_copy_char(const Bufbyte * src, Bufbyte * dst)
{
	return BYTE_ASCII_P(*src) ?
	    simple_charptr_copy_char(src, dst) :
	    non_ascii_charptr_copy_char(src, dst);
}

#else				/* not MULE */

# define charptr_emchar(ptr)		simple_charptr_emchar (ptr)
# define set_charptr_emchar(ptr, x)	simple_set_charptr_emchar (ptr, x)
# define charptr_copy_char(ptr, ptr2)	simple_charptr_copy_char (ptr, ptr2)

#endif				/* not MULE */

#define charptr_emchar_n(ptr, offset) \
  charptr_emchar (charptr_n_addr (ptr, offset))

/* ---------------------------- */
/* (D) For working with Emchars */
/* ---------------------------- */

#ifdef MULE

int non_ascii_valid_char_p(Emchar ch);

extern_inline int valid_char_p(Emchar ch);
extern_inline int valid_char_p(Emchar ch)
{
	return ((unsigned int)(ch) <= 0xff) || non_ascii_valid_char_p(ch);
}

#else				/* not MULE */

#define valid_char_p(ch) ((unsigned int) (ch) <= 0xff)

#endif				/* not MULE */

#define CHAR_INTP(x) (INTP (x) && valid_char_p (XINT (x)))

#define CHAR_OR_CHAR_INTP(x) (CHARP (x) || CHAR_INTP (x))

extern_inline Emchar XCHAR_OR_CHAR_INT(Lisp_Object obj);
extern_inline Emchar XCHAR_OR_CHAR_INT(Lisp_Object obj)
{
	return CHARP(obj) ? XCHAR(obj) : XINT(obj);
}

#define CHECK_CHAR_COERCE_INT(x) do {		\
  if (CHARP (x))				\
     ;						\
  else if (CHAR_INTP (x))			\
    x = make_char (XINT (x));			\
  else						\
    x = wrong_type_argument (Qcharacterp, x);	\
} while (0)

#ifdef MULE
# define MAX_EMCHAR_LEN 4
#else
# define MAX_EMCHAR_LEN 1
#endif

/*----------------------------------------------------------------------*/
/*	    Accessor macros for important positions in a buffer		*/
/*----------------------------------------------------------------------*/

/* We put them here because some stuff below wants them before the
   place where we would normally put them. */

/* None of these are lvalues.  Use the settor macros below to change
   the positions. */

/* Beginning of buffer.  */
#define BI_BUF_BEG(buf) ((Bytind) 1)
#define BUF_BEG(buf) ((Bufpos) 1)

/* Beginning of accessible range of buffer.  */
#define BI_BUF_BEGV(buf) ((buf)->begv + 0)
#define BUF_BEGV(buf) ((buf)->bufbegv + 0)

/* End of accessible range of buffer.  */
#define BI_BUF_ZV(buf) ((buf)->zv + 0)
#define BUF_ZV(buf) ((buf)->bufzv + 0)

/* End of buffer.  */
#define BI_BUF_Z(buf) ((buf)->text->z + 0)
#define BUF_Z(buf) ((buf)->text->bufz + 0)

/* Point. */
#define BI_BUF_PT(buf) ((buf)->pt + 0)
#define BUF_PT(buf) ((buf)->bufpt + 0)

/*----------------------------------------------------------------------*/
/*		Converting between positions and addresses		*/
/*----------------------------------------------------------------------*/

/* Convert the address of a byte in the buffer into a position.  */
extern_inline Bytind BI_BUF_PTR_BYTE_POS(struct buffer * buf, const Bufbyte * ptr);
extern_inline Bytind BI_BUF_PTR_BYTE_POS(struct buffer *buf, const Bufbyte * ptr)
{
	return (ptr - buf->text->beg + 1
		- ((ptr - buf->text->beg + 1) > buf->text->gpt
		   ? buf->text->gap_size : 0));
}

#define BUF_PTR_BYTE_POS(buf, ptr) \
  bytind_to_bufpos (buf, BI_BUF_PTR_BYTE_POS (buf, ptr))

/* Address of byte at position POS in buffer. */
extern_inline Bufbyte *BI_BUF_BYTE_ADDRESS(struct buffer * buf, Bytind pos);
extern_inline Bufbyte *BI_BUF_BYTE_ADDRESS(struct buffer *buf, Bytind pos)
{
	return (buf->text->beg +
		((pos >= buf->text->gpt ? (pos + buf->text->gap_size) : pos)
		 - 1));
}

#define BUF_BYTE_ADDRESS(buf, pos) \
  BI_BUF_BYTE_ADDRESS (buf, bufpos_to_bytind (buf, pos))

/* Address of byte before position POS in buffer. */
extern_inline Bufbyte *BI_BUF_BYTE_ADDRESS_BEFORE(struct buffer * buf,
						  Bytind pos);
extern_inline Bufbyte *BI_BUF_BYTE_ADDRESS_BEFORE(struct buffer *buf,
						  Bytind pos)
{
	return (buf->text->beg +
		((pos > buf->text->gpt ? (pos + buf->text->gap_size) : pos)
		 - 2));
}

#define BUF_BYTE_ADDRESS_BEFORE(buf, pos) \
  BI_BUF_BYTE_ADDRESS_BEFORE (buf, bufpos_to_bytind (buf, pos))

/*----------------------------------------------------------------------*/
/*	    Converting between byte indices and memory indices		*/
/*----------------------------------------------------------------------*/

extern_inline int valid_memind_p(struct buffer *buf, Memind x);
extern_inline int valid_memind_p(struct buffer *buf, Memind x)
{
	return ((x >= 1 && x <= (Memind) buf->text->gpt) ||
		(x > (Memind) (buf->text->gpt + buf->text->gap_size) &&
		 x <= (Memind) (buf->text->z + buf->text->gap_size)));
}

extern_inline Memind bytind_to_memind(struct buffer * buf, Bytind x);
extern_inline Memind bytind_to_memind(struct buffer *buf, Bytind x)
{
	return (Memind) ((x > buf->text->gpt) ? (x + buf->text->gap_size) : x);
}

extern_inline Bytind memind_to_bytind(struct buffer * buf, Memind x);
extern_inline Bytind memind_to_bytind(struct buffer *buf, Memind x)
{
#ifdef ERROR_CHECK_BUFPOS
	assert(valid_memind_p(buf, x));
#endif
	return (Bytind) ((x > (Memind) buf->text->gpt) ?
			 x - buf->text->gap_size : x);
}

#define memind_to_bufpos(buf, x) \
  bytind_to_bufpos (buf, memind_to_bytind (buf, x))
#define bufpos_to_memind(buf, x) \
  bytind_to_memind (buf, bufpos_to_bytind (buf, x))

/* These macros generalize many standard buffer-position functions to
   either a buffer or a string. */

/* Converting between Meminds and Bytinds, for a buffer-or-string.
   For strings, this is a no-op.  For buffers, this resolves
   to the standard memind<->bytind converters. */

#define buffer_or_string_bytind_to_memind(obj, ind) \
  (BUFFERP (obj) ? bytind_to_memind (XBUFFER (obj), ind) : (Memind) ind)

#define buffer_or_string_memind_to_bytind(obj, ind) \
  (BUFFERP (obj) ? memind_to_bytind (XBUFFER (obj), ind) : (Bytind) ind)

/* Converting between Bufpos's and Bytinds, for a buffer-or-string.
   For strings, this maps to the bytecount<->charcount converters. */

#define buffer_or_string_bufpos_to_bytind(obj, pos)		\
  (BUFFERP (obj) ? bufpos_to_bytind (XBUFFER (obj), pos) :	\
   (Bytind) charcount_to_bytecount (XSTRING_DATA (obj), pos))

#define buffer_or_string_bytind_to_bufpos(obj, ind)		\
  (BUFFERP (obj) ? bytind_to_bufpos (XBUFFER (obj), ind) :	\
   (Bufpos) bytecount_to_charcount (XSTRING_DATA (obj), ind))

/* Similar for Bufpos's and Meminds. */

#define buffer_or_string_bufpos_to_memind(obj, pos)		\
  (BUFFERP (obj) ? bufpos_to_memind (XBUFFER (obj), pos) :	\
   (Memind) charcount_to_bytecount (XSTRING_DATA (obj), pos))

#define buffer_or_string_memind_to_bufpos(obj, ind)		\
  (BUFFERP (obj) ? memind_to_bufpos (XBUFFER (obj), ind) :	\
   (Bufpos) bytecount_to_charcount (XSTRING_DATA (obj), ind))

/************************************************************************/
/*                                                                      */
/*                    working with buffer-level data                    */
/*                                                                      */
/************************************************************************/

/*

   (A) Working with byte indices:
   ------------------------------

   VALID_BYTIND_P(buf, bi):
	Given a byte index, does it point to the beginning of a character?

   ASSERT_VALID_BYTIND_UNSAFE(buf, bi):
	If error-checking is enabled, assert that the given byte index
	is within range and points to the beginning of a character
	or to the end of the buffer.  Otherwise, do nothing.

   ASSERT_VALID_BYTIND_BACKWARD_UNSAFE(buf, bi):
	If error-checking is enabled, assert that the given byte index
	is within range and satisfies ASSERT_VALID_BYTIND() and also
	does not refer to the beginning of the buffer. (i.e. movement
	backwards is OK.) Otherwise, do nothing.

   ASSERT_VALID_BYTIND_FORWARD_UNSAFE(buf, bi):
	If error-checking is enabled, assert that the given byte index
	is within range and satisfies ASSERT_VALID_BYTIND() and also
	does not refer to the end of the buffer. (i.e. movement
	forwards is OK.) Otherwise, do nothing.

   VALIDATE_BYTIND_BACKWARD(buf, bi):
	Make sure that the given byte index is pointing to the beginning
	of a character.  If not, back up until this is the case.  Note
	that there are not too many places where it is legitimate to do
	this sort of thing.  It's an error if you're passed an "invalid"
	byte index.

   VALIDATE_BYTIND_FORWARD(buf, bi):
	Make sure that the given byte index is pointing to the beginning
	of a character.  If not, move forward until this is the case.
	Note that there are not too many places where it is legitimate
	to do this sort of thing.  It's an error if you're passed an
	"invalid" byte index.

   INC_BYTIND(buf, bi):
	Given a byte index (assumed to point at the beginning of a
	character), modify that value so it points to the beginning
	of the next character.

   DEC_BYTIND(buf, bi):
	Given a byte index (assumed to point at the beginning of a
	character), modify that value so it points to the beginning
	of the previous character.  Unlike for DEC_CHARPTR(), we can
	do all the assert()s because there are sentinels at the
	beginning of the gap and the end of the buffer.

   BYTIND_INVALID:
	A constant representing an invalid Bytind.  Valid Bytinds
	can never have this value.

   (B) Converting between Bufpos's and Bytinds:
   --------------------------------------------

    bufpos_to_bytind(buf, bu):
	Given a Bufpos, return the equivalent Bytind.

    bytind_to_bufpos(buf, bi):
	Given a Bytind, return the equivalent Bufpos.

    make_bufpos(buf, bi):
	Given a Bytind, return the equivalent Bufpos as a Lisp Object.
 */

/*----------------------------------------------------------------------*/
/*			 working with byte indices			*/
/*----------------------------------------------------------------------*/

#ifdef MULE
# define VALID_BYTIND_P(buf, x) \
  BUFBYTE_FIRST_BYTE_P (*BI_BUF_BYTE_ADDRESS (buf, x))
#else
# define VALID_BYTIND_P(buf, x) 1
#endif

#ifdef ERROR_CHECK_BUFPOS

# define ASSERT_VALID_BYTIND_UNSAFE(buf, x) do {		\
  assert (BUFFER_LIVE_P (buf));					\
  assert ((x) >= BI_BUF_BEG (buf) && x <= BI_BUF_Z (buf));	\
  assert (VALID_BYTIND_P (buf, x));				\
} while (0)
# define ASSERT_VALID_BYTIND_BACKWARD_UNSAFE(buf, x) do {	\
  assert (BUFFER_LIVE_P (buf));					\
  assert ((x) > BI_BUF_BEG (buf) && x <= BI_BUF_Z (buf));	\
  assert (VALID_BYTIND_P (buf, x));				\
} while (0)
# define ASSERT_VALID_BYTIND_FORWARD_UNSAFE(buf, x) do {	\
  assert (BUFFER_LIVE_P (buf));					\
  assert ((x) >= BI_BUF_BEG (buf) && x < BI_BUF_Z (buf));	\
  assert (VALID_BYTIND_P (buf, x));				\
} while (0)

#else				/* not ERROR_CHECK_BUFPOS */
# define ASSERT_VALID_BYTIND_UNSAFE(buf, x)
# define ASSERT_VALID_BYTIND_BACKWARD_UNSAFE(buf, x)
# define ASSERT_VALID_BYTIND_FORWARD_UNSAFE(buf, x)

#endif				/* not ERROR_CHECK_BUFPOS */

/* Note that, although the Mule version will work fine for non-Mule
   as well (it should reduce down to nothing), we provide a separate
   version to avoid compilation warnings and possible non-optimal
   results with stupid compilers. */

#ifdef MULE
# define VALIDATE_BYTIND_BACKWARD(buf, x) do {		\
  Bufbyte *VBB_ptr = BI_BUF_BYTE_ADDRESS (buf, x);	\
  while (!BUFBYTE_FIRST_BYTE_P (*VBB_ptr))		\
    VBB_ptr--, (x)--;					\
} while (0)
#else
# define VALIDATE_BYTIND_BACKWARD(buf, x)
#endif

/* Note that, although the Mule version will work fine for non-Mule
   as well (it should reduce down to nothing), we provide a separate
   version to avoid compilation warnings and possible non-optimal
   results with stupid compilers. */

#ifdef MULE
# define VALIDATE_BYTIND_FORWARD(buf, x) do {		\
  Bufbyte *VBF_ptr = BI_BUF_BYTE_ADDRESS (buf, x);	\
  while (!BUFBYTE_FIRST_BYTE_P (*VBF_ptr))		\
    VBF_ptr++, (x)++;					\
} while (0)
#else
# define VALIDATE_BYTIND_FORWARD(buf, x)
#endif

/* Note that in the simplest case (no MULE, no ERROR_CHECK_BUFPOS),
   this crap reduces down to simply (x)++. */

#define INC_BYTIND(buf, x) do				\
{							\
  ASSERT_VALID_BYTIND_FORWARD_UNSAFE (buf, x);		\
  /* Note that we do the increment first to		\
     make sure that the pointer in			\
     VALIDATE_BYTIND_FORWARD() ends up on		\
     the correct side of the gap */			\
  (x)++;						\
  VALIDATE_BYTIND_FORWARD (buf, x);			\
} while (0)

/* Note that in the simplest case (no MULE, no ERROR_CHECK_BUFPOS),
   this crap reduces down to simply (x)--. */

#define DEC_BYTIND(buf, x) do				\
{							\
  ASSERT_VALID_BYTIND_BACKWARD_UNSAFE (buf, x);		\
  /* Note that we do the decrement first to		\
     make sure that the pointer in			\
     VALIDATE_BYTIND_BACKWARD() ends up on		\
     the correct side of the gap */			\
  (x)--;						\
  VALIDATE_BYTIND_BACKWARD (buf, x);			\
} while (0)

extern_inline Bytind prev_bytind(struct buffer * buf, Bytind x);
extern_inline Bytind prev_bytind(struct buffer *buf, Bytind x)
{
	DEC_BYTIND(buf, x);
	return x;
}

extern_inline Bytind next_bytind(struct buffer * buf, Bytind x);
extern_inline Bytind next_bytind(struct buffer *buf, Bytind x)
{
	INC_BYTIND(buf, x);
	return x;
}

#define BYTIND_INVALID ((Bytind) -1)

/*----------------------------------------------------------------------*/
/*	   Converting between buffer positions and byte indices		*/
/*----------------------------------------------------------------------*/

#ifdef MULE

Bytind bufpos_to_bytind_func(struct buffer * buf, Bufpos x);
Bufpos bytind_to_bufpos_func(struct buffer *buf, Bytind x);

/* The basic algorithm we use is to keep track of a known region of
   characters in each buffer, all of which are of the same width.  We
   keep track of the boundaries of the region in both Bufpos and
   Bytind coordinates and also keep track of the char width, which
   is 1 - 4 bytes.  If the position we're translating is not in
   the known region, then we invoke a function to update the known
   region to surround the position in question.  This assumes
   locality of reference, which is usually the case.

   Note that the function to update the known region can be simple
   or complicated depending on how much information we cache.
   For the moment, we don't cache any information, and just move
   linearly forward or back from the known region, with a few
   shortcuts to catch all-ASCII buffers. (Note that this will
   thrash with bad locality of reference.) A smarter method would
   be to keep some sort of pseudo-extent layer over the buffer;
   maybe keep track of the bufpos/bytind correspondence at the
   beginning of each line, which would allow us to do a binary
   search over the pseudo-extents to narrow things down to the
   correct line, at which point you could use a linear movement
   method.  This would also mesh well with efficiently
   implementing a line-numbering scheme.

   Note also that we have to multiply or divide by the char width
   in order to convert the positions.  We do some tricks to avoid
   ever actually having to do a multiply or divide, because that
   is typically an expensive operation (esp. divide).  Multiplying
   or dividing by 1, 2, or 4 can be implemented simply as a
   shift left or shift right, and we keep track of a shifter value
   (0, 1, or 2) indicating how much to shift.  Multiplying by 3
   can be implemented by doubling and then adding the original
   value.  Dividing by 3, alas, cannot be implemented in any
   simple shift/subtract method, as far as I know; so we just
   do a table lookup.  For simplicity, we use a table of size
   128K, which indexes the "divide-by-3" values for the first
   64K non-negative numbers. (Note that we can increase the
   size up to 384K, i.e. indexing the first 192K non-negative
   numbers, while still using shorts in the array.) This also
   means that the size of the known region can be at most
   64K for width-three characters.
   */

extern short three_to_one_table[];

extern_inline int real_bufpos_to_bytind(struct buffer *buf, Bufpos x);
extern_inline int real_bufpos_to_bytind(struct buffer *buf, Bufpos x)
{
	if (x >= buf->text->mule_bufmin && x <= buf->text->mule_bufmax)
		return (buf->text->mule_bytmin +
			((x -
			  buf->text->mule_bufmin) << buf->text->mule_shifter) +
			(buf->text->
			 mule_three_p ? (x - buf->text->mule_bufmin) : 0));
	else
		return bufpos_to_bytind_func(buf, x);
}

extern_inline int real_bytind_to_bufpos(struct buffer *buf, Bytind x);
extern_inline int real_bytind_to_bufpos(struct buffer *buf, Bytind x)
{
	if (x >= buf->text->mule_bytmin && x <= buf->text->mule_bytmax)
		return (buf->text->mule_bufmin +
			((buf->text->mule_three_p
			  ? three_to_one_table[x - buf->text->mule_bytmin]
			  : (x -
			     buf->text->mule_bytmin) >> buf->text->
			  mule_shifter)));
	else
		return bytind_to_bufpos_func(buf, x);
}

#else				/* not MULE */

# define real_bufpos_to_bytind(buf, x)	((Bytind) x)
# define real_bytind_to_bufpos(buf, x)	((Bufpos) x)

#endif				/* not MULE */

#ifdef ERROR_CHECK_BUFPOS

Bytind bufpos_to_bytind(struct buffer * buf, Bufpos x);
Bufpos bytind_to_bufpos(struct buffer *buf, Bytind x);

#else				/* not ERROR_CHECK_BUFPOS */

#define bufpos_to_bytind real_bufpos_to_bytind
#define bytind_to_bufpos real_bytind_to_bufpos

#endif				/* not ERROR_CHECK_BUFPOS */

#define make_bufpos(buf, ind) make_int (bytind_to_bufpos (buf, ind))

/*----------------------------------------------------------------------*/
/*         Converting between buffer bytes and Emacs characters         */
/*----------------------------------------------------------------------*/

/* The character at position POS in buffer. */
#define BI_BUF_FETCH_CHAR(buf, pos) \
  charptr_emchar (BI_BUF_BYTE_ADDRESS (buf, pos))
#define BUF_FETCH_CHAR(buf, pos) \
  BI_BUF_FETCH_CHAR (buf, bufpos_to_bytind (buf, pos))

/* The character at position POS in buffer, as a string.  This is
   equivalent to set_charptr_emchar (str, BUF_FETCH_CHAR (buf, pos))
   but is faster for Mule. */

# define BI_BUF_CHARPTR_COPY_CHAR(buf, pos, str) \
  charptr_copy_char (BI_BUF_BYTE_ADDRESS (buf, pos), str)
#define BUF_CHARPTR_COPY_CHAR(buf, pos, str) \
  BI_BUF_CHARPTR_COPY_CHAR (buf, bufpos_to_bytind (buf, pos), str)

/************************************************************************/
/*                                                                      */
/*         Converting between internal and external format              */
/*                                                                      */
/************************************************************************/
/*
  All client code should use only the two macros

  TO_EXTERNAL_FORMAT (source_type, source, sink_type, sink, coding_system)
  TO_INTERNAL_FORMAT (source_type, source, sink_type, sink, coding_system)

  Typical use is

  TO_EXTERNAL_FORMAT (DATA, (ptr, len),
		      LISP_BUFFER, buffer,
		      Qfile_name);

  The source or sink can be specified in one of these ways:

  DATA,   (ptr, len),    // input data is a fixed buffer of size len
  ALLOCA, (ptr, len),    // output data is in a alloca()ed buffer of size len
  MALLOC, (ptr, len),    // output data is in a malloc()ed buffer of size len
  C_STRING_ALLOCA, ptr,  // equivalent to ALLOCA (ptr, len_ignored) on output
  C_STRING_MALLOC, ptr,  // equivalent to MALLOC (ptr, len_ignored) on output
  C_STRING,     ptr,     // equivalent to DATA, (ptr, strlen (ptr) + 1) on input
  LISP_STRING,  string,  // input or output is a Lisp_Object of type string
  LISP_BUFFER,  buffer,  // output is written to (point) in lisp buffer
  LISP_LSTREAM, lstream, // input or output is a Lisp_Object of type lstream
  LISP_OPAQUE,  object,  // input or output is a Lisp_Object of type opaque

  When specifying the sink, use lvalues, since the macro will assign to them,
  except when the sink is an lstream or a lisp buffer.

  The macros accept the kinds of sources and sinks appropriate for
  internal and external data representation.  See the type_checking_assert
  macros below for the actual allowed types.

  Since some sources and sinks use one argument (a Lisp_Object) to
  specify them, while others take a (pointer, length) pair, we use
  some C preprocessor trickery to allow pair arguments to be specified
  by parenthesizing them, as in the examples above.

  Anything prefixed by dfc_ (`data format conversion') is private.
  They are only used to implement these macros.

  Using C_STRING* is appropriate for using with external APIs that take
  null-terminated strings.  For internal data, we should try to be
  '\0'-clean - i.e. allow arbitrary data to contain embedded '\0'.

  Sometime in the future we might allow output to C_STRING_ALLOCA or
  C_STRING_MALLOC _only_ with TO_EXTERNAL_FORMAT(), not
  TO_INTERNAL_FORMAT().  */

#define TO_EXTERNAL_FORMAT(source_type, source, sink_type, sink, coding_system)	\
do {										\
  dfc_conversion_type dfc_simplified_source_type;				\
  dfc_conversion_type dfc_simplified_sink_type;					\
  dfc_conversion_data dfc_source;						\
  dfc_conversion_data dfc_sink;							\
										\
  type_checking_assert								\
    ((DFC_TYPE_##source_type == DFC_TYPE_DATA ||				\
      DFC_TYPE_##source_type == DFC_TYPE_C_STRING ||				\
      DFC_TYPE_##source_type == DFC_TYPE_LISP_STRING ||				\
      DFC_TYPE_##source_type == DFC_TYPE_LISP_OPAQUE ||				\
      DFC_TYPE_##source_type == DFC_TYPE_LISP_LSTREAM)				\
    &&										\
     (DFC_TYPE_##sink_type == DFC_TYPE_ALLOCA ||				\
      DFC_TYPE_##sink_type == DFC_TYPE_MALLOC ||				\
      DFC_TYPE_##sink_type == DFC_TYPE_C_STRING_ALLOCA ||			\
      DFC_TYPE_##sink_type == DFC_TYPE_C_STRING_MALLOC ||			\
      DFC_TYPE_##sink_type == DFC_TYPE_LISP_LSTREAM ||				\
      DFC_TYPE_##sink_type == DFC_TYPE_LISP_OPAQUE));				\
										\
  DFC_SOURCE_##source_type##_TO_ARGS (source);					\
  DFC_SINK_##sink_type##_TO_ARGS     (sink);					\
										\
  DFC_CONVERT_TO_EXTERNAL_FORMAT (dfc_simplified_source_type, &dfc_source,	\
				  coding_system,				\
				  dfc_simplified_sink_type,   &dfc_sink);	\
										\
  DFC_##sink_type##_USE_CONVERTED_DATA (sink);					\
} while (0)

#define TO_INTERNAL_FORMAT(source_type, source, sink_type, sink, coding_system)	\
do {										\
  dfc_conversion_type dfc_simplified_source_type;				\
  dfc_conversion_type dfc_simplified_sink_type;					\
  dfc_conversion_data dfc_source;						\
  dfc_conversion_data dfc_sink;							\
										\
  type_checking_assert								\
    ((DFC_TYPE_##source_type == DFC_TYPE_DATA ||				\
      DFC_TYPE_##source_type == DFC_TYPE_C_STRING ||				\
      DFC_TYPE_##source_type == DFC_TYPE_LISP_OPAQUE ||				\
      DFC_TYPE_##source_type == DFC_TYPE_LISP_LSTREAM)				\
     &&										\
     (DFC_TYPE_##sink_type == DFC_TYPE_ALLOCA ||				\
      DFC_TYPE_##sink_type == DFC_TYPE_MALLOC ||				\
      DFC_TYPE_##sink_type == DFC_TYPE_C_STRING_ALLOCA ||			\
      DFC_TYPE_##sink_type == DFC_TYPE_C_STRING_MALLOC ||			\
      DFC_TYPE_##sink_type == DFC_TYPE_LISP_STRING ||				\
      DFC_TYPE_##sink_type == DFC_TYPE_LISP_LSTREAM ||				\
      DFC_TYPE_##sink_type == DFC_TYPE_LISP_BUFFER));				\
										\
  DFC_SOURCE_##source_type##_TO_ARGS (source);					\
  DFC_SINK_##sink_type##_TO_ARGS     (sink);					\
										\
  DFC_CONVERT_TO_INTERNAL_FORMAT (dfc_simplified_source_type, &dfc_source,	\
				  coding_system,				\
				  dfc_simplified_sink_type,   &dfc_sink);	\
										\
  DFC_##sink_type##_USE_CONVERTED_DATA (sink);					\
} while (0)

#ifdef FILE_CODING
#define DFC_CONVERT_TO_EXTERNAL_FORMAT dfc_convert_to_external_format
#define DFC_CONVERT_TO_INTERNAL_FORMAT dfc_convert_to_internal_format
#else
/* ignore coding_system argument */
#define DFC_CONVERT_TO_EXTERNAL_FORMAT(a, b, coding_system, c, d) \
 dfc_convert_to_external_format (a, b, c, d)
#define DFC_CONVERT_TO_INTERNAL_FORMAT(a, b, coding_system, c, d) \
 dfc_convert_to_internal_format (a, b, c, d)
#endif

typedef union {
	struct {
		const void *ptr;
		size_t len;
	} data;
	Lisp_Object lisp_object;
} dfc_conversion_data;

enum dfc_conversion_type {
	DFC_TYPE_DATA,
	DFC_TYPE_ALLOCA,
	DFC_TYPE_MALLOC,
	DFC_TYPE_C_STRING,
	DFC_TYPE_C_STRING_ALLOCA,
	DFC_TYPE_C_STRING_MALLOC,
	DFC_TYPE_LISP_STRING,
	DFC_TYPE_LISP_LSTREAM,
	DFC_TYPE_LISP_OPAQUE,
	DFC_TYPE_LISP_BUFFER
};
typedef enum dfc_conversion_type dfc_conversion_type;

/* WARNING: These use a static buffer.  This can lead to disaster if
   these functions are not used *very* carefully.  Another reason to only use
   TO_EXTERNAL_FORMAT() and TO_INTERNAL_FORMAT(). */
void
dfc_convert_to_external_format(dfc_conversion_type source_type,
			       dfc_conversion_data * source,
#ifdef FILE_CODING
			       Lisp_Object coding_system,
#endif
			       dfc_conversion_type sink_type,
			       dfc_conversion_data * sink);
void
dfc_convert_to_internal_format(dfc_conversion_type source_type,
			       dfc_conversion_data * source,
#ifdef FILE_CODING
			       Lisp_Object coding_system,
#endif
			       dfc_conversion_type sink_type,
			       dfc_conversion_data * sink);
/* CPP Trickery */
#define DFC_CPP_CAR(x,y) (x)
#define DFC_CPP_CDR(x,y) (y)

/* Convert `source' to args for dfc_convert_to_*_format() */
#define DFC_SOURCE_DATA_TO_ARGS(val)				\
	do {							\
		dfc_source.data.ptr = DFC_CPP_CAR val;		\
		dfc_source.data.len = DFC_CPP_CDR val;		\
		assert(dfc_source.data.ptr != NULL);            \
		dfc_simplified_source_type = DFC_TYPE_DATA;	\
	} while (0)
#define DFC_SOURCE_C_STRING_TO_ARGS(val)				\
	do {								\
		dfc_source.data.ptr = (val);				\
		assert(dfc_source.data.ptr != NULL);            \
		dfc_source.data.len = strlen((const char*)		\
					     (dfc_source.data.ptr));	\
		dfc_simplified_source_type = DFC_TYPE_DATA;		\
	} while (0)
#define DFC_SOURCE_LISP_STRING_TO_ARGS(val)				\
	do {								\
		Lisp_Object dfc_slsta = (val);				\
		type_checking_assert (STRINGP (dfc_slsta));		\
		dfc_source.lisp_object = dfc_slsta;			\
		dfc_simplified_source_type = DFC_TYPE_LISP_STRING;	\
	} while (0)
#define DFC_SOURCE_LISP_LSTREAM_TO_ARGS(val)				\
	do {								\
		Lisp_Object dfc_sllta = (val);				\
		type_checking_assert (LSTREAMP (dfc_sllta));		\
		dfc_source.lisp_object = dfc_sllta;			\
		dfc_simplified_source_type = DFC_TYPE_LISP_LSTREAM;	\
	} while (0)
#define DFC_SOURCE_LISP_OPAQUE_TO_ARGS(val)			\
	do {							\
		Lisp_Opaque *dfc_slota = XOPAQUE (val);		\
		dfc_source.data.ptr = OPAQUE_DATA (dfc_slota);	\
		dfc_source.data.len = OPAQUE_SIZE (dfc_slota);	\
		assert(dfc_source.data.ptr != NULL);            \
		dfc_simplified_source_type = DFC_TYPE_DATA;	\
	} while (0)

/* Convert `sink' to args for dfc_convert_to_*_format() */
#define DFC_SINK_ALLOCA_TO_ARGS(val)			\
	dfc_simplified_sink_type = DFC_TYPE_DATA
#define DFC_SINK_C_STRING_ALLOCA_TO_ARGS(val)		\
	dfc_simplified_sink_type = DFC_TYPE_DATA
#define DFC_SINK_MALLOC_TO_ARGS(val)			\
	dfc_simplified_sink_type = DFC_TYPE_DATA
#define DFC_SINK_C_STRING_MALLOC_TO_ARGS(val)		\
	dfc_simplified_sink_type = DFC_TYPE_DATA
#define DFC_SINK_LISP_STRING_TO_ARGS(val)		\
	dfc_simplified_sink_type = DFC_TYPE_DATA
#define DFC_SINK_LISP_OPAQUE_TO_ARGS(val)		\
	dfc_simplified_sink_type = DFC_TYPE_DATA
#define DFC_SINK_LISP_LSTREAM_TO_ARGS(val)				\
	do {								\
		Lisp_Object dfc_sllta = (val);				\
		type_checking_assert (LSTREAMP (dfc_sllta));		\
		dfc_sink.lisp_object = dfc_sllta;			\
		dfc_simplified_sink_type = DFC_TYPE_LISP_LSTREAM;	\
	} while (0)
#define DFC_SINK_LISP_BUFFER_TO_ARGS(val)				\
	do {								\
		struct buffer *dfc_slbta = XBUFFER (val);		\
		dfc_sink.lisp_object =					\
			make_lisp_buffer_output_stream			\
			(dfc_slbta, BUF_PT (dfc_slbta), 0);		\
		dfc_simplified_sink_type = DFC_TYPE_LISP_LSTREAM;	\
	} while (0)

/* Assign to the `sink' lvalue(s) using the converted data. */
typedef union {
	char c;
	void *p;
} *dfc_aliasing_voidpp;
#define DFC_ALLOCA_USE_CONVERTED_DATA(sink) do {			\
		void *dfc_sink_ret = alloca(dfc_sink.data.len + 1);	\
		assert(dfc_sink_ret != NULL);				\
		memcpy(dfc_sink_ret, dfc_sink.data.ptr,			\
		       dfc_sink.data.len + 1);				\
		((dfc_aliasing_voidpp)&(DFC_CPP_CAR sink))->p =		\
			dfc_sink_ret;					\
		(DFC_CPP_CDR sink) = dfc_sink.data.len;			\
	} while (0)
#define DFC_MALLOC_USE_CONVERTED_DATA(sink)				\
	do {								\
		void *dfc_sink_ret = xmalloc_atomic(dfc_sink.data.len + 1); \
		assert(dfc_sink_ret != NULL);				\
		memcpy(dfc_sink_ret, dfc_sink.data.ptr,			\
		       dfc_sink.data.len + 1);				\
		((dfc_aliasing_voidpp)&(DFC_CPP_CAR sink))->p =		\
			dfc_sink_ret;					\
		(DFC_CPP_CDR sink) = dfc_sink.data.len;			\
	} while (0)
#define DFC_C_STRING_ALLOCA_USE_CONVERTED_DATA(sink)			\
	do {								\
		void *dfc_sink_ret = alloca (dfc_sink.data.len + 1);	\
		assert(dfc_sink_ret != NULL);				\
		memcpy(dfc_sink_ret, dfc_sink.data.ptr,			\
		       dfc_sink.data.len + 1);				\
		(sink) =  dfc_sink_ret;					\
	} while (0)
#define DFC_C_STRING_MALLOC_USE_CONVERTED_DATA(sink)			\
	do {								\
		void *dfc_sink_ret = xmalloc_atomic(dfc_sink.data.len + 1); \
		assert(dfc_sink_ret != NULL);				\
		memcpy(dfc_sink_ret, dfc_sink.data.ptr,			\
		       dfc_sink.data.len + 1);				\
		(sink) = dfc_sink_ret;					\
	} while (0)
#define DFC_LISP_STRING_USE_CONVERTED_DATA(sink)			\
	do {								\
		sink = make_string((const Bufbyte*)dfc_sink.data.ptr,	\
				   dfc_sink.data.len);			\
		assert(!NILP(sink));					\
	} while (0)
#define DFC_LISP_OPAQUE_USE_CONVERTED_DATA(sink)			\
	do {								\
		sink = make_opaque(dfc_sink.data.ptr,			\
				   dfc_sink.data.len);			\
		assert(!NILP(sink));					\
	} while (0)

#define DFC_LISP_LSTREAM_USE_CONVERTED_DATA(sink)	/* data already used */
#define DFC_LISP_BUFFER_USE_CONVERTED_DATA(sink)		\
	Lstream_delete (XLSTREAM (dfc_sink.lisp_object))

/* Someday we might want to distinguish between Qnative and Qfile_name
   by using coding-system aliases, but for now it suffices to have
   these be identical.  Qnative can be used as the coding_system
   argument to TO_EXTERNAL_FORMAT() and TO_INTERNAL_FORMAT(). */
#define Qnative Qfile_name


/* More stand-ins */
#define Qcommand_argument_encoding Qnative
#define Qenvironment_variable_encoding Qnative

/* Convenience macros for extremely common invocations */
#define C_STRING_TO_EXTERNAL(in, out, coding_system)			\
	TO_EXTERNAL_FORMAT(C_STRING, in,				\
			   C_STRING_ALLOCA, out, coding_system)
#define C_STRING_TO_EXTERNAL_MALLOC(in, out, coding_system)		\
	TO_EXTERNAL_FORMAT(C_STRING, in,				\
			   C_STRING_MALLOC, out, coding_system)
#define EXTERNAL_TO_C_STRING(in, out, coding_system)			\
	TO_INTERNAL_FORMAT(C_STRING, in,				\
			   C_STRING_ALLOCA, out, coding_system)
#define EXTERNAL_TO_C_STRING_MALLOC(in, out, coding_system)		\
	TO_INTERNAL_FORMAT(C_STRING, in,				\
			   C_STRING_MALLOC, out, coding_system)
#define LISP_STRING_TO_EXTERNAL(in, out, coding_system)			\
	TO_EXTERNAL_FORMAT(LISP_STRING, in,				\
			   C_STRING_ALLOCA, out, coding_system)
#define LISP_STRING_TO_EXTERNAL_MALLOC(in, out, coding_system)		\
	TO_EXTERNAL_FORMAT(LISP_STRING, in,				\
			   C_STRING_MALLOC, out, coding_system)

/************************************************************************/
/*                                                                      */
/*                          fake charset functions                      */
/*                                                                      */
/************************************************************************/

/* used when MULE is not defined, so that Charset-type stuff can still
   be done */

#ifndef MULE

#define Vcharset_ascii Qnil

#define CHAR_CHARSET(ch) Vcharset_ascii
#define CHAR_LEADING_BYTE(ch) LEADING_BYTE_ASCII
#define LEADING_BYTE_ASCII 0x80
#define NUM_LEADING_BYTES 1
#define MIN_LEADING_BYTE 0x80
#define CHARSETP(cs) 1
#define CHARSET_BY_LEADING_BYTE(lb) Vcharset_ascii
#define XCHARSET_LEADING_BYTE(cs) LEADING_BYTE_ASCII
#define XCHARSET_GRAPHIC(cs) -1
#define XCHARSET_COLUMNS(cs) 1
#define XCHARSET_DIMENSION(cs) 1
#define REP_BYTES_BY_FIRST_BYTE(fb) 1
#define BREAKUP_CHAR(ch, charset, byte1, byte2) do {	\
  (charset) = Vcharset_ascii;				\
  (byte1) = (ch);					\
  (byte2) = 0;						\
} while (0)
#define BYTE_ASCII_P(byte) 1

#endif				/* ! MULE */

/************************************************************************/
/*                                                                      */
/*                  higher-level buffer-position functions              */
/*                                                                      */
/************************************************************************/

/*----------------------------------------------------------------------*/
/*           Settor macros for important positions in a buffer          */
/*----------------------------------------------------------------------*/

/* Set beginning of accessible range of buffer.  */
#define SET_BOTH_BUF_BEGV(buf, val, bival)	\
do						\
{						\
  (buf)->begv = (bival);			\
  (buf)->bufbegv = (val);			\
} while (0)

/* Set end of accessible range of buffer.  */
#define SET_BOTH_BUF_ZV(buf, val, bival)	\
do						\
{						\
  (buf)->zv = (bival);				\
  (buf)->bufzv = (val);				\
} while (0)

/* Set point. */
/* Since BEGV and ZV are almost never set, it's reasonable to enforce
   the restriction that the Bufpos and Bytind values must both be
   specified.  However, point is set in lots and lots of places.  So
   we provide the ability to specify both (for efficiency) or just
   one. */
#define BOTH_BUF_SET_PT(buf, val, bival) set_buffer_point (buf, val, bival)
#define BI_BUF_SET_PT(buf, bival) \
  BOTH_BUF_SET_PT (buf, bytind_to_bufpos (buf, bival), bival)
#define BUF_SET_PT(buf, value) \
  BOTH_BUF_SET_PT (buf, value, bufpos_to_bytind (buf, value))

#if 0				/* FSFmacs */
/* These macros exist in FSFmacs because SET_PT() in FSFmacs incorrectly
   does too much stuff, such as moving out of invisible extents. */
#define TEMP_SET_PT(position) (temp_set_point ((position), current_buffer))
#define SET_BUF_PT(buf, value) ((buf)->pt = (value))
#endif				/* FSFmacs */

/*----------------------------------------------------------------------*/
/*                      Miscellaneous buffer values                     */
/*----------------------------------------------------------------------*/

/* Number of characters in buffer */
#define BUF_SIZE(buf) (BUF_Z (buf) - BUF_BEG (buf))

/* Is this buffer narrowed? */
#define BUF_NARROWED(buf) \
   ((BI_BUF_BEGV (buf) != BI_BUF_BEG (buf)) || \
    (BI_BUF_ZV   (buf) != BI_BUF_Z   (buf)))

/* Modification count.  */
#define BUF_MODIFF(buf) ((buf)->text->modiff)

/* Saved modification count.  */
#define BUF_SAVE_MODIFF(buf) ((buf)->text->save_modiff)

/* Face changed.  */
#define BUF_FACECHANGE(buf) ((buf)->face_change)

#define POINT_MARKER_P(marker) \
   (XMARKER (marker)->buffer != 0 && \
    EQ (marker, XMARKER (marker)->buffer->point_marker))

#define BUF_MARKERS(buf) ((buf)->markers)

/* WARNING:

   The new definitions of CEILING_OF() and FLOOR_OF() differ semantically
   from the old ones (in FSF Emacs and XEmacs 19.11 and before).
   Conversion is as follows:

   OLD_BI_CEILING_OF(n) = NEW_BI_CEILING_OF(n) - 1
   OLD_BI_FLOOR_OF(n) = NEW_BI_FLOOR_OF(n + 1)

   The definitions were changed because the new definitions are more
   consistent with the way everything else works in Emacs.
 */

/* Properties of CEILING_OF and FLOOR_OF (also apply to BI_ variants):

   1) FLOOR_OF (CEILING_OF (n)) = n
      CEILING_OF (FLOOR_OF (n)) = n

   2) CEILING_OF (n) = n if and only if n = ZV
      FLOOR_OF (n) = n if and only if n = BEGV

   3) CEILING_OF (CEILING_OF (n)) = ZV
      FLOOR_OF (FLOOR_OF (n)) = BEGV

   4) The bytes in the regions

      [BYTE_ADDRESS (n), BYTE_ADDRESS_BEFORE (CEILING_OF (n))]

      and

      [BYTE_ADDRESS (FLOOR_OF (n)), BYTE_ADDRESS_BEFORE (n)]

      are contiguous.
   */

/*  Return the maximum index in the buffer it is safe to scan forwards
    past N to.  This is used to prevent buffer scans from running into
    the gap (e.g. search.c).  All characters between N and CEILING_OF(N)
    are located contiguous in memory.  Note that the character *at*
    CEILING_OF(N) is not contiguous in memory. */
#define BI_BUF_CEILING_OF(b, n)						\
  ((n) < (b)->text->gpt && (b)->text->gpt < BI_BUF_ZV (b) ?		\
   (b)->text->gpt : BI_BUF_ZV (b))
#define BUF_CEILING_OF(b, n)						\
  bytind_to_bufpos (b, BI_BUF_CEILING_OF (b, bufpos_to_bytind (b, n)))

/*  Return the minimum index in the buffer it is safe to scan backwards
    past N to.  All characters between FLOOR_OF(N) and N are located
    contiguous in memory.  Note that the character *at* N may not be
    contiguous in memory. */
#define BI_BUF_FLOOR_OF(b, n)						\
	(BI_BUF_BEGV (b) < (b)->text->gpt && (b)->text->gpt < (n) ?	\
	 (b)->text->gpt : BI_BUF_BEGV (b))
#define BUF_FLOOR_OF(b, n)						\
  bytind_to_bufpos (b, BI_BUF_FLOOR_OF (b, bufpos_to_bytind (b, n)))

#define BI_BUF_CEILING_OF_IGNORE_ACCESSIBLE(b, n)			\
  ((n) < (b)->text->gpt && (b)->text->gpt < BI_BUF_Z (b) ?		\
   (b)->text->gpt : BI_BUF_Z (b))
#define BUF_CEILING_OF_IGNORE_ACCESSIBLE(b, n)				\
  bytind_to_bufpos							\
   (b, BI_BUF_CEILING_OF_IGNORE_ACCESSIBLE (b, bufpos_to_bytind (b, n)))

#define BI_BUF_FLOOR_OF_IGNORE_ACCESSIBLE(b, n)				\
	(BI_BUF_BEG (b) < (b)->text->gpt && (b)->text->gpt < (n) ?	\
	 (b)->text->gpt : BI_BUF_BEG (b))
#define BUF_FLOOR_OF_IGNORE_ACCESSIBLE(b, n)				\
  bytind_to_bufpos							\
   (b, BI_BUF_FLOOR_OF_IGNORE_ACCESSIBLE (b, bufpos_to_bytind (b, n)))

extern struct buffer *current_buffer;

/* This is the initial (startup) directory, as used for the *scratch* buffer.
   We're making this a global to make others aware of the startup directory.
   `initial_directory' is stored in external format.
 */
extern char initial_directory[];
extern void init_initial_directory(void);	/* initialize initial_directory */

EXFUN(Fbuffer_disable_undo, 1);
EXFUN(Fbuffer_modified_p, 1);
EXFUN(Fbuffer_name, 1);
EXFUN(Fcurrent_buffer, 0);
EXFUN(Ferase_buffer, 1);
EXFUN(Fget_buffer, 1);
EXFUN(Fget_buffer_create, 1);
EXFUN(Fget_file_buffer, 1);
EXFUN(Fkill_buffer, 1);
EXFUN(Fother_buffer, 3);
EXFUN(Frecord_buffer, 1);
EXFUN(Fset_buffer, 1);
EXFUN(Fset_buffer_modified_p, 2);
EXFUN(Fgenerate_new_buffer_name, 2);

extern Lisp_Object QSscratch, Qafter_change_function, Qafter_change_functions;
extern Lisp_Object Qbefore_change_function, Qbefore_change_functions;
extern Lisp_Object Qbuffer_or_string_p, Qdefault_directory, Qfirst_change_hook;
extern Lisp_Object Qpermanent_local, Vafter_change_function;
extern Lisp_Object Vafter_change_functions, Vbefore_change_function;
extern Lisp_Object Vbefore_change_functions, Vbuffer_alist, Vbuffer_defaults;
extern Lisp_Object Vinhibit_read_only, Vtransient_mark_mode;

/* This structure marks which slots in a buffer have corresponding
   default values in Vbuffer_defaults.
   Each such slot has a nonzero value in this structure.
   The value has only one nonzero bit.

   When a buffer has its own local value for a slot,
   the bit for that slot (found in the same slot in this structure)
   is turned on in the buffer's local_var_flags slot.

   If a slot in this structure is zero, then even though there may
   be a DEFVAR_BUFFER_LOCAL for the slot, there is no default value for it;
   and the corresponding slot in Vbuffer_defaults is not used.  */

extern struct buffer buffer_local_flags;

/* Allocation of buffer data. */

#if defined REL_ALLOC

char *r_alloc(unsigned char **, size_t);
char *r_re_alloc(unsigned char **, size_t);
void r_alloc_free(unsigned char **);

#define BUFFER_ALLOC(data, size)					\
	((Bufbyte *)r_alloc((unsigned char **)&data,			\
			    (size) * sizeof(Bufbyte)))
#define BUFFER_REALLOC(data, size)					\
	((Bufbyte *)r_re_alloc((unsigned char **)&data,			\
			       (size) * sizeof(Bufbyte)))
#define BUFFER_FREE(data) r_alloc_free((unsigned char **)&(data))
#define R_ALLOC_DECLARE(var,data) r_alloc_declare (&(var), data)

#else				/* !REL_ALLOC */
#define BUFFER_ALLOC(data,size)					\
	(data = xmalloc_atomic(sizeof(Bufbyte) * (size)))
static inline Bufbyte*
BUFFER_REALLOC(Bufbyte *data, size_t size)
{
	Bufbyte *foo = xrealloc(data, size * sizeof(Bufbyte));
	return foo;
}
/* Avoid excess parentheses, or syntax errors may rear their heads. */
#define BUFFER_FREE(data)	xfree(data)
#define R_ALLOC_DECLARE(var,data)

#endif				/* !REL_ALLOC */

extern Lisp_Object Vbuffer_alist;
void set_buffer_internal(struct buffer *b);
struct buffer *decode_buffer(Lisp_Object buffer, int allow_string);

/* from editfns.c */
void widen_buffer(struct buffer *b, int no_clip);
int beginning_of_line_p(struct buffer *b, Bufpos pt);

/* from insdel.c */
void set_buffer_point(struct buffer *buf, Bufpos pos, Bytind bipos);
void find_charsets_in_bufbyte_string(unsigned char *charsets,
				     const Bufbyte * str, Bytecount len);
void find_charsets_in_emchar_string(unsigned char *charsets,
				    const Emchar * str, Charcount len);
int bufbyte_string_displayed_columns(const Bufbyte * str, Bytecount len);
int emchar_string_displayed_columns(const Emchar * str, Charcount len);
void convert_bufbyte_string_into_emchar_dynarr(const Bufbyte * str,
					       Bytecount len,
					       Emchar_dynarr * dyn);
Charcount convert_bufbyte_string_into_emchar_string(const Bufbyte * str,
						    Bytecount len,
						    Emchar * arr);
void convert_emchar_string_into_bufbyte_dynarr(Emchar * arr, int nels,
					       Bufbyte_dynarr * dyn);
Bufbyte *convert_emchar_string_into_malloced_string(Emchar * arr, int nels,
						    Bytecount * len_out);
/* from marker.c */
void init_buffer_markers(struct buffer *b);
void uninit_buffer_markers(struct buffer *b);

/* flags for get_buffer_pos_char(), get_buffer_range_char(), etc. */
/* At most one of GB_COERCE_RANGE and GB_NO_ERROR_IF_BAD should be
   specified.  At most one of GB_NEGATIVE_FROM_END and GB_NO_ERROR_IF_BAD
   should be specified. */

#define GB_ALLOW_PAST_ACCESSIBLE	(1 << 0)
#define GB_ALLOW_NIL			(1 << 1)
#define GB_CHECK_ORDER			(1 << 2)
#define GB_COERCE_RANGE			(1 << 3)
#define GB_NO_ERROR_IF_BAD		(1 << 4)
#define GB_NEGATIVE_FROM_END		(1 << 5)
#define GB_HISTORICAL_STRING_BEHAVIOR	(GB_NEGATIVE_FROM_END | GB_ALLOW_NIL)

Bufpos get_buffer_pos_char(struct buffer *b, Lisp_Object pos,
			   unsigned int flags);
Bytind get_buffer_pos_byte(struct buffer *b, Lisp_Object pos,
			   unsigned int flags);
void get_buffer_range_char(struct buffer *b, Lisp_Object from, Lisp_Object to,
			   Bufpos * from_out, Bufpos * to_out,
			   unsigned int flags);
void get_buffer_range_byte(struct buffer *b, Lisp_Object from, Lisp_Object to,
			   Bytind * from_out, Bytind * to_out,
			   unsigned int flags);
Charcount get_string_pos_char(Lisp_Object string, Lisp_Object pos,
			      unsigned int flags);
Bytecount get_string_pos_byte(Lisp_Object string, Lisp_Object pos,
			      unsigned int flags);
void get_string_range_char(Lisp_Object string, Lisp_Object from,
			   Lisp_Object to, Charcount * from_out,
			   Charcount * to_out, unsigned int flags);
void get_string_range_byte(Lisp_Object string, Lisp_Object from,
			   Lisp_Object to, Bytecount * from_out,
			   Bytecount * to_out, unsigned int flags);
Bufpos get_buffer_or_string_pos_char(Lisp_Object object, Lisp_Object pos,
				     unsigned int flags);
Bytind get_buffer_or_string_pos_byte(Lisp_Object object, Lisp_Object pos,
				     unsigned int flags);
void get_buffer_or_string_range_char(Lisp_Object object, Lisp_Object from,
				     Lisp_Object to, Bufpos * from_out,
				     Bufpos * to_out, unsigned int flags);
void get_buffer_or_string_range_byte(Lisp_Object object, Lisp_Object from,
				     Lisp_Object to, Bytind * from_out,
				     Bytind * to_out, unsigned int flags);
Bufpos buffer_or_string_accessible_begin_char(Lisp_Object object);
Bufpos buffer_or_string_accessible_end_char(Lisp_Object object);
Bytind buffer_or_string_accessible_begin_byte(Lisp_Object object);
Bytind buffer_or_string_accessible_end_byte(Lisp_Object object);
Bufpos buffer_or_string_absolute_begin_char(Lisp_Object object);
Bufpos buffer_or_string_absolute_end_char(Lisp_Object object);
Bytind buffer_or_string_absolute_begin_byte(Lisp_Object object);
Bytind buffer_or_string_absolute_end_byte(Lisp_Object object);
void record_buffer(Lisp_Object buf);
Lisp_Object emacs_get_buffer(Lisp_Object name,
			     int error_if_deleted_or_does_not_exist);
int map_over_sharing_buffers(struct buffer *buf,
			     int (*mapfun) (struct buffer * buf,
					    void *closure), void *closure);

/************************************************************************/
/*                         Case conversion                              */
/************************************************************************/

/* A "trt" table is a mapping from characters to other characters,
   typically used to convert between uppercase and lowercase.  For
   compatibility reasons, trt tables are currently in the form of
   a Lisp string of 256 characters, specifying the conversion for each
   of the first 256 Emacs characters (i.e. the 256 Latin-1 characters).
   This should be generalized at some point to support conversions for
   all of the allowable Mule characters.
   */

/* The _1 macros are named as such because they assume that you have
   already guaranteed that the character values are all in the range
   0 - 255.  Bad lossage will happen otherwise. */

#define MAKE_TRT_TABLE() Fmake_char_table (Qgeneric)
extern_inline Emchar TRT_TABLE_CHAR_1(Lisp_Object table, Emchar c);
extern_inline Emchar TRT_TABLE_CHAR_1(Lisp_Object table, Emchar ch)
{
	Lisp_Object TRT_char;
	TRT_char = get_char_table(ch, XCHAR_TABLE(table));
	if (NILP(TRT_char))
		return ch;
	else
		return XCHAR(TRT_char);
}

#define SET_TRT_TABLE_CHAR_1(table, ch1, ch2)	\
  Fput_char_table (make_char (ch1), make_char (ch2), table);

extern_inline Emchar TRT_TABLE_OF(Lisp_Object trt, Emchar c);
extern_inline Emchar TRT_TABLE_OF(Lisp_Object trt, Emchar c)
{
	return TRT_TABLE_CHAR_1(trt, c);
}

/* Macros used below. */
#define DOWNCASE_TABLE_OF(buf, c)	\
  TRT_TABLE_OF (XCASE_TABLE_DOWNCASE (buf->case_table), c)
#define UPCASE_TABLE_OF(buf, c)		\
  TRT_TABLE_OF (XCASE_TABLE_UPCASE (buf->case_table), c)

/* 1 if CH is upper case.  */

extern_inline int UPPERCASEP(struct buffer *buf, Emchar ch);
extern_inline int UPPERCASEP(struct buffer *buf, Emchar ch)
{
	return DOWNCASE_TABLE_OF(buf, ch) != ch;
}

/* 1 if CH is lower case.  */

extern_inline int LOWERCASEP(struct buffer *buf, Emchar ch);
extern_inline int LOWERCASEP(struct buffer *buf, Emchar ch)
{
	return (UPCASE_TABLE_OF(buf, ch) != ch &&
		DOWNCASE_TABLE_OF(buf, ch) == ch);
}

/* 1 if CH is neither upper nor lower case.  */

extern_inline int NOCASEP(struct buffer *buf, Emchar ch);
extern_inline int NOCASEP(struct buffer *buf, Emchar ch)
{
	return UPCASE_TABLE_OF(buf, ch) == ch;
}

/* Upcase a character, or make no change if that cannot be done.  */

extern_inline Emchar UPCASE(struct buffer * buf, Emchar ch);
extern_inline Emchar UPCASE(struct buffer *buf, Emchar ch)
{
	return (DOWNCASE_TABLE_OF(buf, ch) == ch) ? UPCASE_TABLE_OF(buf,
								    ch) : ch;
}

/* Upcase a character known to be not upper case.  Unused. */

#define UPCASE1(buf, ch) UPCASE_TABLE_OF (buf, ch)

/* Downcase a character, or make no change if that cannot be done. */

#define DOWNCASE(buf, ch) DOWNCASE_TABLE_OF (buf, ch)

/************************************************************************/
/*		Lisp string representation convenience functions	*/
/************************************************************************/
/* Because the representation of internally formatted data is subject to change,
   It's bad style to do something like strcmp (XSTRING_DATA (s), "foo")
   Instead, use the portable: bufbyte_strcmp (XSTRING_DATA (s), "foo")
   or bufbyte_memcmp (XSTRING_DATA (s), "foo", 3) */

/* Like strcmp, except first arg points at internally formatted data,
   while the second points at a string of only ASCII chars. */
/* const means constant! */
extern_inline int bufbyte_strcmp(Bufbyte * bp, const char *ascii_string);
extern_inline int bufbyte_strcmp(Bufbyte * bp, const char *ascii_string)
{
#ifdef MULE
	while (1) {
		int diff;
		type_checking_assert(BYTE_ASCII_P(*ascii_string));
		if ((diff =
		     charptr_emchar(bp) - *(const Bufbyte *)ascii_string) != 0)
			return diff;
		if (*ascii_string == '\0')
			return 0;
		ascii_string++;
		INC_CHARPTR(bp);
	}
#else
	return strcmp((char *)bp, ascii_string);
#endif
}

/* Like memcmp, except first arg points at internally formatted data,
   while the second points at a string of only ASCII chars. */
/* const means constant! */
extern_inline int
bufbyte_memcmp(Bufbyte * bp, const char *ascii_string, size_t len);
extern_inline int
bufbyte_memcmp(Bufbyte * bp, const char *ascii_string, size_t len)
{
#ifdef MULE
	while (len--) {
		int diff = charptr_emchar(bp) - *(const Bufbyte *)ascii_string;
		type_checking_assert(BYTE_ASCII_P(*ascii_string));
		if (diff != 0)
			return diff;
		ascii_string++;
		INC_CHARPTR(bp);
	}
	return 0;
#else
	return memcmp(bp, ascii_string, len);
#endif
}

#endif  /* INCLUDED_buffer_h_ */
