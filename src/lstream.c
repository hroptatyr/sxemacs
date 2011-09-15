/* Generic stream implementation.
   Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 1996 Ben Wing.

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

/* Written by Ben Wing. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "ui/insdel.h"
#include "lstream.h"

#include "sysfile.h"
#include <errno.h>

/*! \page lstream Lstream
 *
 * <P>
 * A lot to tell here ...
 * </P>
 */

/* This function provides a generic buffering stream implementation.
   Conceptually, you send data to the stream or read data from the
   stream, not caring what's on the other end of the stream.  The
   other end could be another stream, a file descriptor, a stdio
   stream, a fixed block of memory, a reallocating block of memory,
   etc.  The main purpose of the stream is to provide a standard
   interface and to do buffering.  Macros are defined to read
   or write characters, so the calling functions do not have to
   worry about blocking data together in order to achieve efficiency.
   */

/* Note that this object is called "stream" in Lisp but "lstream"
   in C.  The reason for this is that "stream" is too generic a name
   for C; too much likelihood of conflict/confusion with C++, etc. */

/* Functions are as follows:

Lstream *Lstream_new (Lstream_implementation *imp, const char *mode)
	Allocate and return a new Lstream.  This function is not
	really meant to be called directly; rather, each stream type
	should provide its own stream creation function, which
	creates the stream and does any other necessary creation
	stuff (e.g. opening a file).

void Lstream_set_buffering (Lstream *lstr, Lstream_buffering buffering,
			    int buffering_size)
	Change the buffering of a stream.  See lstream.h.  By default
	the buffering is STREAM_BLOCK_BUFFERED.

int Lstream_flush (Lstream *lstr)
	Flush out any pending unwritten data in the stream.  Clear
	any buffered input data.  Returns 0 on success, -1 on error.

int Lstream_putc (Lstream *stream, int c)
	Write out one byte to the stream.  This is a macro and so
	it is very efficient.  The C argument is only evaluated once
	but the STREAM argument is evaluated more than once.  Returns
	0 on success, -1 on error.

int Lstream_getc (Lstream *stream)
	Read one byte from the stream and returns it as an unsigned
	char cast to an int, or EOF on end of file or error.
	This is a macro and so it is very efficient.  The STREAM
	argument is evaluated more than once.

void Lstream_ungetc (Lstream *stream, int c)
	Push one byte back onto the input queue, cast to unsigned char.
	This will be the next byte read from the stream.  Any number
	of bytes can be pushed back and will be read in the reverse
	order they were pushed back -- most recent first. (This is
	necessary for consistency -- if there are a number of bytes
	that have been unread and I read and unread a byte, it needs
	to be the first to be read again.) This is a macro and so it
	is very efficient.  The C argument is only evaluated once but
	the STREAM argument is evaluated more than once.

int Lstream_fputc (Lstream *stream, int c)
int Lstream_fgetc (Lstream *stream)
void Lstream_fungetc (Lstream *stream, int c)
	Function equivalents of the above macros.

Lstream_data_count Lstream_read (Lstream *stream, void *data,
                                 Lstream_data_count size)
	Read SIZE bytes of DATA from the stream.  Return the number of
	bytes read.  0 means EOF. -1 means an error occurred and no
	bytes were read.

Lstream_data_count Lstream_write (Lstream *stream, void *data,
                                  Lstream_data_count size)
	Write SIZE bytes of DATA to the stream.  Return the number of
	bytes written.  -1 means an error occurred and no bytes were
	written.

void Lstream_unread (Lstream *stream, void *data, Lstream_data_count size)
	Push back SIZE bytes of DATA onto the input queue.  The
	next call to Lstream_read() with the same size will read the
	same bytes back.  Note that this will be the case even if
	there is other pending unread data.

int Lstream_delete (Lstream *stream)
	Frees all memory associated with the stream is freed.  Calling
	this is not strictly necessary, but it is much more efficient
	than having the Lstream be garbage-collected.

int Lstream_close (Lstream *stream)
	Close the stream.  All data will be flushed out.

int Lstream_get_fd (Lstream *stream)
	Return the underlying filedescriptor or -1.

void Lstream_reopen (Lstream *stream)
	Reopen a closed stream.  This enables I/O on it again.
	This is not meant to be called except from a wrapper routine
	that reinitializes variables and such -- the close routine
	may well have freed some necessary storage structures, for
	example.

void Lstream_rewind (Lstream *stream)
	Rewind the stream to the beginning.
*/

#define DEFAULT_BLOCK_BUFFERING_SIZE 512
#define MAX_READ_SIZE 512

static Lisp_Object mark_lstream(Lisp_Object obj)
{
	lstream_t lstr = XLSTREAM(obj);
	return lstr->imp->marker ? (lstr->imp->marker) (obj) : Qnil;
}

static void
print_lstream(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	lstream_t lstr = XLSTREAM(obj);
	char buf[200];

	sprintf(buf, "#<INTERNAL OBJECT (SXEmacs bug?) (%s lstream) 0x%lx>",
		lstr->imp->name, (long)lstr);
	write_c_string(buf, printcharfun);
}

static void finalize_lstream(void *header, int for_disksave)
{
	/* WARNING WARNING WARNING.  This function (and all finalize functions)
	   may get called more than once on the same object, and may get called
	   (at dump time) on objects that are not being released. */
	lstream_t lstr = header;

#if 0				/* this may cause weird Broken Pipes? */
	if (for_disksave) {
		Lstream_pseudo_close(lstr);
		return;
	}
#endif
	if (lstr->flags & LSTREAM_FL_IS_OPEN) {
		if (for_disksave) {
			if (lstr->flags & LSTREAM_FL_CLOSE_AT_DISKSAVE)
				Lstream_close(lstr);
		} else
			/* Just close. */
			Lstream_close(lstr);
	}
}

static inline size_t
aligned_sizeof_lstream(size_t lstream_type_specific_size)
{
	return ALIGN_SIZE(offsetof(struct lstream_s, data) +
			  lstream_type_specific_size,
			  ALIGNOF(max_align_t));
}

static inline size_t
sizeof_lstream(const void *header)
{
	REGISTER size_t imp_size = ((const struct lstream_s*)header)->imp->size;
	return aligned_sizeof_lstream(imp_size);
}

DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION("stream", lstream,
				       mark_lstream, print_lstream,
				       finalize_lstream, 0, 0, 0,
				       sizeof_lstream, struct lstream_s);

/**
 * Replaces DO_REALLOC from lisp.h. */
#define LSTR_ALLOC_TO	DO_REALLOC_ATOMIC


void
Lstream_set_buffering(lstream_t lstr, Lstream_buffering buffering, int bsz)
{
	lstr->buffering = buffering;
	switch (buffering) {
	case LSTREAM_UNBUFFERED:
		lstr->buffering_size = 0;
		break;
	case LSTREAM_BLOCK_BUFFERED:
		lstr->buffering_size = DEFAULT_BLOCK_BUFFERING_SIZE;
		break;
	case LSTREAM_BLOCKN_BUFFERED:
		lstr->buffering_size = bsz;
		break;
	case LSTREAM_LINE_BUFFERED:
	case LSTREAM_UNLIMITED:
		lstr->buffering_size = INT_MAX;
		break;
	default:
		break;
	}
}

#if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
static const Lstream_implementation *lstream_types[32];
static Lisp_Object Vlstream_free_list[32];
static int lstream_type_count;
#endif	/* !BDWGC */

#if defined HAVE_BDWGC && defined EF_USE_BDWGC
static void
lstr_finaliser(void *obj, void *UNUSED(data))
{
	finalize_lstream(obj, 0);
	return;
}
#endif	/* BDWGC */

lstream_t
Lstream_new(const Lstream_implementation *imp, const char *mode)
{
	lstream_t p;

#if defined HAVE_BDWGC && defined EF_USE_BDWGC
	GC_finalization_proc *foo = NULL;
	void **bar = NULL;

	p = xmalloc(aligned_sizeof_lstream(imp->size));
	set_lheader_implementation(&p->header.lheader, &lrecord_lstream);

	GC_REGISTER_FINALIZER(p, lstr_finaliser, NULL, foo, bar);
#else  /* !BDWGC */
	int i;
	Lisp_Object tmp;

	for (i = 0; i < lstream_type_count; i++) {
		if (lstream_types[i] == imp)
			break;
	}

	if (i == lstream_type_count) {
		assert(lstream_type_count < countof(lstream_types));
		lstream_types[lstream_type_count] = imp;
		Vlstream_free_list[lstream_type_count] =
			make_lcrecord_list(aligned_sizeof_lstream(imp->size),
					   &lrecord_lstream);
		lstream_type_count++;
	}

	tmp = allocate_managed_lcrecord(Vlstream_free_list[i]);
	p = XLSTREAM(tmp);
	/* Zero it out, except the header. */
	memset((char *)p + sizeof(p->header), '\0',
	       aligned_sizeof_lstream(imp->size) - sizeof(p->header));
#endif	/* BDWCG */

	p->imp = imp;
	Lstream_set_buffering(p, LSTREAM_BLOCK_BUFFERED, 0);
	p->flags = LSTREAM_FL_IS_OPEN;

	/* convert mode (one of "r", "w", "rc", "wc") to p->flags */
	assert(mode[0] == 'r' || mode[0] == 'w');
	assert(mode[1] == 'c' || mode[1] == '\0');
	p->flags |= (mode[0] == 'r' ? LSTREAM_FL_READ : LSTREAM_FL_WRITE);
	if (mode[1] == 'c') {
		p->flags |= LSTREAM_FL_NO_PARTIAL_CHARS;
	}
	return p;
}

void
Lstream_set_character_mode(lstream_t lstr)
{
	lstr->flags |= LSTREAM_FL_NO_PARTIAL_CHARS;
}

void
Lstream_delete(lstream_t lstr)
{
#if defined HAVE_BDWGC && defined EF_USE_BDWGC
	xfree(lstr);
	return;

#else  /* !BDWGC */
	Lisp_Object val;

	XSETLSTREAM(val, lstr);
	for (int i = 0; i < lstream_type_count; i++) {
		if (lstream_types[i] == lstr->imp) {
			free_managed_lcrecord(Vlstream_free_list[i], val);
			return;
		}
	}

	abort();
#endif	/* BDWGC */
}

#define Lstream_internal_error(reason, lstr)				\
	Lstream_signal_simple_error(lstr, "Internal error: " reason)

static void
Lstream_signal_simple_error(lstream_t lstr, const char *reason)
{
	Lisp_Object obj;
	XSETLSTREAM(obj, lstr);
	signal_simple_error(reason, obj);
}

void
Lstream_reopen(lstream_t lstr)
{
	if (lstr->flags & LSTREAM_FL_IS_OPEN) {
		Lstream_internal_error("lstream already open", lstr);
	}
	lstr->flags |= LSTREAM_FL_IS_OPEN;
}

/* Attempt to flush out all of the buffered data for writing. */

int
Lstream_flush_out(lstream_t lstr)
{
	Lstream_data_count num_written;

	while (lstr->out_buffer_ind > 0) {
		Lstream_data_count size = lstr->out_buffer_ind;
		if (!(lstr->flags & LSTREAM_FL_IS_OPEN)) {
			Lstream_internal_error("lstream not open", lstr);
		}
		if (!(lstr->flags & LSTREAM_FL_WRITE)) {
			Lstream_internal_error("lstream not open for writing",
					       lstr);
		}
		if (!lstr->imp->writer) {
			Lstream_internal_error("lstream has no writer", lstr);
		}
		if (lstr->flags & LSTREAM_FL_NO_PARTIAL_CHARS)
			/* It's quite possible for us to get passed an
			   incomplete character at the end.  We need to spit
			   back that incomplete character. */
		{
			const unsigned char *data = lstr->out_buffer;
			const unsigned char *dataend = data + size - 1;
			assert(size > 0);	/* safety check ... */
			/* Optimize the most common case. */
			if (!BYTE_ASCII_P(*dataend)) {
				/* Go back to the beginning of the last (and
				   possibly partial) character, and bump forward
				   to see if the character is complete. */
				VALIDATE_CHARPTR_BACKWARD(dataend);
				if (dataend +
				    REP_BYTES_BY_FIRST_BYTE(*dataend) !=
				    data + size) {
					/* If not, chop the size down to ignore
					   the last char and stash it away for
					   next time. */
					size = dataend - data;
				}
				/* If we don't even have one character to write,
				   then just skip out. */
				if (size == 0) {
					break;
				}
			}
		}

		num_written = lstr->imp->writer(lstr, lstr->out_buffer, size);
		if (num_written == 0) {
			/* If nothing got written, then just hold the data.
			   This may occur, for example, if this stream does
			   non-blocking I/O; the attempt to write the data might
			   have resulted in an EWOULDBLOCK error. */
			return 0;
		} else if (num_written >= lstr->out_buffer_ind) {
			lstr->out_buffer_ind = 0;
		} else if (num_written > 0) {
			memmove(lstr->out_buffer,
				lstr->out_buffer + num_written,
				lstr->out_buffer_ind - num_written);
			lstr->out_buffer_ind -= num_written;
		} else {
			/* If error, just hold the data, for similar reasons as
			   above. */
			return -1;
		}
	}

	if (lstr->imp->flusher) {
		return (lstr->imp->flusher) (lstr);
	}
	return 0;
}

int
Lstream_flush(lstream_t lstr)
{
	if (Lstream_flush_out(lstr) < 0) {
		return -1;
	}
	/* clear out buffered data */
	lstr->in_buffer_current = lstr->in_buffer_ind = 0;
	lstr->unget_buffer_ind = 0;

	return 0;
}

/* We want to add NUM characters.  This function ensures that the
   buffer is large enough for this (per the buffering size specified
   in the stream) and returns the number of characters we can
   actually write.  If FORCE is set, ignore the buffering size
   and go ahead and make space for all the chars even if it exceeds
   the buffering size. (This is used to deal with the possibility
   that the stream writer might refuse to write any bytes now, e.g.
   if it's getting EWOULDBLOCK errors.   We have to keep stocking them
   up until they can be written, so as to avoid losing data. */

static Lstream_data_count
Lstream_adding(lstream_t lstr, Lstream_data_count num, int force)
{
	Lstream_data_count size = num + lstr->out_buffer_ind;

	if (size <= lstr->out_buffer_size) {
		return num;
	}
	/* Maybe chop it down so that we don't buffer more characters
	   than our advertised buffering size. */
	if ((size > lstr->buffering_size) && !force) {
		size = lstr->buffering_size;
		/* There might be more data buffered than the buffering size. */
		if (size <= lstr->out_buffer_ind) {
			return 0;
		}
	}

	LSTR_ALLOC_TO(lstr->out_buffer,
		      lstr->out_buffer_size, size, unsigned char);

	return size - lstr->out_buffer_ind;
}

/* Like Lstream_write(), but does not handle line-buffering correctly. */

static Lstream_data_count
Lstream_write_1(lstream_t lstr, const void *data, Lstream_data_count size)
{
	const unsigned char *p = data;
	Lstream_data_count off = 0;
	if (!(lstr->flags & LSTREAM_FL_IS_OPEN)) {
		Lstream_internal_error("lstream not open", lstr);
	}
	if (!(lstr->flags & LSTREAM_FL_WRITE)) {
		Lstream_internal_error("lstream not open for writing", lstr);
	}
	{
		bool couldnt_write_last_time = false;

		while (1) {
			/* Figure out how much we can add to the buffer */
			Lstream_data_count chunk =
				Lstream_adding(lstr, size, 0);
			if (chunk == 0) {
				if (couldnt_write_last_time) {
					/* Ung, we ran out of space and tried to
					   flush the buffer, but it didn't work
					   because the stream writer is refusing
					   to accept any data.  So we just have
					   to squirrel away all the rest of the
					   stuff. */
					chunk = Lstream_adding(lstr, size, 1);
				} else {
					couldnt_write_last_time = true;
				}
			}
			/* Do it. */
			if (chunk > 0) {
				memcpy(lstr->out_buffer + lstr->out_buffer_ind,
				       p + off, chunk);
				lstr->out_buffer_ind += chunk;
				lstr->byte_count += chunk;
				size -= chunk;
				off += chunk;
			}
			/* If the buffer is full and we have more to add, flush
			   it out. */
			if (size > 0) {
				if (Lstream_flush_out(lstr) < 0) {
					if (off == 0) {
						return -1;
					} else {
						return off;
					}
				}
			} else {
				break;
			}
		}
	}
	return off;
}

/* If the stream is not line-buffered, then we can just call
   Lstream_write_1(), which writes in chunks.  Otherwise, we
   repeatedly call Lstream_putc(), which knows how to handle
   line buffering.  Returns number of bytes written. */

Lstream_data_count
Lstream_write(lstream_t lstr, const void *data, Lstream_data_count size)
{
	Lstream_data_count i;
	const unsigned char *p = data;

	if (size == 0) {
		return size;
	}
	if (lstr->buffering != LSTREAM_LINE_BUFFERED) {
		return Lstream_write_1(lstr, data, size);
	}
	for (i = 0; i < size; i++) {
		if (Lstream_putc(lstr, p[i]) < 0) {
			break;
		}
	}
	return i == 0 ? -1 : i;
}

int
Lstream_was_blocked_p(lstream_t lstr)
{
	return lstr->imp->was_blocked_p ? lstr->imp->was_blocked_p(lstr) : 0;
}

static Lstream_data_count
Lstream_raw_read(lstream_t lstr, unsigned char *buffer, Lstream_data_count size)
{
	if (!(lstr->flags & LSTREAM_FL_IS_OPEN)) {
		Lstream_internal_error("lstream not open", lstr);
	}
	if (!(lstr->flags & LSTREAM_FL_READ)) {
		Lstream_internal_error("lstream not open for reading", lstr);
	}
	if (!lstr->imp->reader) {
		Lstream_internal_error("lstream has no reader", lstr);
	}
	return lstr->imp->reader(lstr, buffer, size);
}

/* Assuming the buffer is empty, fill it up again. */

static Lstream_data_count
Lstream_read_more(lstream_t lstr)
{
	/* If someone requested a larger buffer size, so be it! */
	Lstream_data_count size_needed = max(1, lstr->buffering_size);
	Lstream_data_count size_gotten;

	LSTR_ALLOC_TO(lstr->in_buffer, lstr->in_buffer_size,
		      size_needed, unsigned char);
	size_gotten = Lstream_raw_read(lstr, lstr->in_buffer, size_needed);
	lstr->in_buffer_current = max(0, size_gotten);
	lstr->in_buffer_ind = 0;
	return size_gotten < 0 ? -1 : size_gotten;
}

Lstream_data_count
Lstream_read(lstream_t lstr, void *data, Lstream_data_count size)
{
	unsigned char *p = data;
	Lstream_data_count off = 0;
	Lstream_data_count chunk;
	int error_occurred = 0;

	/* trivial cases first */
	if (UNLIKELY(size == 0)) {
		return 0;
	}

	/* First try to get some data from the unget buffer */
	chunk = min(size, lstr->unget_buffer_ind);
	if (chunk > 0) {
		/* The bytes come back in reverse order. */
		for (; off < chunk; off++) {
			p[off] = lstr->unget_buffer[--lstr->unget_buffer_ind];
		}
		lstr->byte_count += chunk;
		size -= chunk;
	}

	while (size > 0) {
		/* Take whatever we can from the in buffer */
		chunk = min(size,
			    lstr->in_buffer_current - lstr->in_buffer_ind);
		if (chunk > 0) {
			memcpy(p + off, lstr->in_buffer + lstr->in_buffer_ind,
			       chunk);
			lstr->in_buffer_ind += chunk;
			lstr->byte_count += chunk;
			size -= chunk;
			off += chunk;
		}

		/* If we need some more, try to get some more from the stream's
		   end */
		if (size > 0) {
			Lstream_data_count retval = Lstream_read_more(lstr);
			if (retval < 0) {
				error_occurred = 1;
			}
			if (retval <= 0) {
				break;
			}
		}
	}

	/* #### Beware of OFF ending up 0. */
	if ((lstr->flags & LSTREAM_FL_NO_PARTIAL_CHARS) && off > 0) {
		/* It's quite possible for us to get passed an incomplete
		   character at the end.  We need to spit back that
		   incomplete character. */
		const unsigned char *dataend = p + off - 1;
		/* Optimize the most common case. */
		if (!BYTE_ASCII_P(*dataend)) {
			/* Go back to the beginning of the last (and possibly
			   partial) character, and bump forward to see if the
			   character is complete. */
			VALIDATE_CHARPTR_BACKWARD(dataend);
			if (dataend + REP_BYTES_BY_FIRST_BYTE(*dataend) !=
			    p + off) {
				Lstream_data_count newoff = dataend - p;
				/* If not, chop the size down to ignore the last
				   char and stash it away for next time. */
				Lstream_unread(lstr, dataend, off - newoff);
				off = newoff;
			}
		}
	}

	return off == 0 && error_occurred ? -1 : off;
}

void
Lstream_unread(lstream_t lstr, const void *data, Lstream_data_count size)
{
	const unsigned char *p = data;

	/* Make sure buffer is big enough */
	LSTR_ALLOC_TO(lstr->unget_buffer, lstr->unget_buffer_size,
		      lstr->unget_buffer_ind + size, unsigned char);

	lstr->byte_count -= size;

	/* Bytes have to go on in reverse order -- they are reversed
	   again when read back. */
	while (size--) {
		lstr->unget_buffer[lstr->unget_buffer_ind++] = p[size];
	}
	return;
}

int
Lstream_rewind(lstream_t lstr)
{
	if (!lstr->imp->rewinder) {
		Lstream_internal_error("lstream has no rewinder", lstr);
	}
	if (Lstream_flush(lstr) < 0) {
		return -1;
	}
	lstr->byte_count = 0;
	return lstr->imp->rewinder(lstr);
}

int
Lstream_seekable_p(lstream_t lstr)
{
	if (!lstr->imp->rewinder) {
		return 0;
	}
	if (!lstr->imp->seekable_p) {
		return 1;
	}
	return lstr->imp->seekable_p(lstr);
}

static int
Lstream_pseudo_close(lstream_t lstr)
{
	if (!(lstr->flags & LSTREAM_FL_IS_OPEN)) {
		Lstream_internal_error("lstream is not open", lstr);
	}
	/* don't check errors here -- best not to risk file descriptor loss */
	return Lstream_flush(lstr);
}

int
Lstream_close(lstream_t lstr)
{
	int rc = 0;

	if (lstr->flags & LSTREAM_FL_IS_OPEN) {
		rc = Lstream_pseudo_close(lstr);
		/*
		 * We used to return immediately if the closer method reported
		 * failure, leaving the stream open.  But this is no good, for
		 * the following reasons.
		 *
		 * 1. The finalizer method used in GC makes no provision for
		 *    failure, so we must not return without freeing buffer
		 *    memory.
		 *
		 * 2. The closer method may have already freed some memory
		 *    used for I/O in this stream.  E.g. encoding_closer frees
		 *    ENCODING_STREAM_DATA(stream)->runoff.  If a writer method
		 *    tries to use this buffer later, it will write into memory
		 *    that may have been allocated elsewhere.  Sometime later
		 *    you will see a sign that says "Welcome to Crash City."
		 *
		 * 3. The closer can report failure if a flush fails in the
		 *    other stream in a MULE encoding/decoding stream pair.
		 *    The other stream in the pair is closed, but returning
		 *    early leaves the current stream open.  If we try to
		 *    flush the current stream later, we will crash when the
		 *    flusher notices that the other end stream is closed.
		 *
		 * So, we no longer abort the close if the closer method
		 * reports some kind of failure.  We still report the failure
		 * to the caller.
		 */
		if (lstr->imp->closer) {
			if ((lstr->imp->closer) (lstr) < 0) {
				rc = -1;
			}
		}
	}

	lstr->flags &= ~LSTREAM_FL_IS_OPEN;
	lstr->byte_count = 0;
	/* Note that Lstream_flush() reset all the buffer indices.  That way,
	   the next call to Lstream_putc(), Lstream_getc(), or Lstream_ungetc()
	   on a closed stream will call into the function equivalents, which
	   will cause an error. */

	/* We set the pointers to 0 so that we don't lose when this function is
	   called more than once on the same object */
	if (lstr->out_buffer) {
		xfree(lstr->out_buffer);
		lstr->out_buffer = 0;
	}
	if (lstr->in_buffer) {
		xfree(lstr->in_buffer);
		lstr->in_buffer = 0;
	}
	if (lstr->unget_buffer) {
		xfree(lstr->unget_buffer);
		lstr->unget_buffer = 0;
	}

	return rc;
}

int
Lstream_fputc(lstream_t lstr, int c)
{
	unsigned char ch = (unsigned char)c;
	Lstream_data_count retval = Lstream_write_1(lstr, &ch, 1);
	if (retval >= 0 && lstr->buffering == LSTREAM_LINE_BUFFERED
	    && ch == '\n') {
		return Lstream_flush_out(lstr);
	}
	return retval < 0 ? -1 : 0;
}

int
Lstream_fgetc(lstream_t lstr)
{
	unsigned char ch;
	if (Lstream_read(lstr, &ch, 1) <= 0) {
		return -1;
	}
	return ch;
}

void
Lstream_fungetc(lstream_t lstr, int c)
{
	unsigned char ch = (unsigned char)c;
	Lstream_unread(lstr, &ch, 1);
}

int
Lstream_get_fd(lstream_t lstr)
{
	int rc = -1;

	if (lstr->imp->get_fd && (rc = (lstr->imp->get_fd)(lstr)) < 0) {
		rc = -1;
	}
	return rc;
}

/************************ some stream implementations *********************/

/*********** a stdio stream ***********/

typedef struct stdio_stream_s *stdio_stream_t;
struct stdio_stream_s {
	FILE *file;
	int closing;
};

#define STDIO_STREAM_DATA(stream) LSTREAM_TYPE_DATA (stream, stdio)

DEFINE_LSTREAM_IMPLEMENTATION("stdio", lstream_stdio,
			      sizeof(struct stdio_stream_s));

static Lisp_Object
make_stdio_stream_1(FILE * stream, int flags, const char *mode)
{
	Lisp_Object obj;
	lstream_t lstr = Lstream_new(lstream_stdio, mode);
	stdio_stream_t str = STDIO_STREAM_DATA(lstr);
	str->file = stream;
	str->closing = flags & LSTR_CLOSING;
	lstr->flags |= LSTREAM_FL_CLOSE_AT_DISKSAVE;
	XSETLSTREAM(obj, lstr);
	return obj;
}

Lisp_Object make_stdio_input_stream(FILE * stream, int flags)
{
	return make_stdio_stream_1(stream, flags, "r");
}

Lisp_Object make_stdio_output_stream(FILE * stream, int flags)
{
	return make_stdio_stream_1(stream, flags, "w");
}

/* #### From reading the Unix 98 specification, it appears that if we
   want stdio_reader() to be completely correct, we should check for
   0 < val < size and if so, check to see if an error has occurred.
   If an error has occurred, but val is non-zero, we should go ahead
   and act as if the read was successful, but remember in some fashion
   or other, that an error has occurred, and report that on the next
   call to stdio_reader instead of calling fread() again.

   Currently, in such a case, we end up calling fread() twice and we
   assume that

   1) this is not harmful, and
   2) the error will still be reported on the second read.

   This is probably reasonable, so I don't think we should change this
   code (it could even be argued that the error might have fixed
   itself, so we should do the fread() again.  */

static Lstream_data_count
stdio_reader(lstream_t stream, unsigned char *data, Lstream_data_count size)
{
	stdio_stream_t str = STDIO_STREAM_DATA(stream);
	Lstream_data_count val = fread(data, 1, size, str->file);
	if (!val && ferror(str->file)) {
		return -1;
	}
	return val;
}

static Lstream_data_count
stdio_writer(lstream_t stream, const unsigned char *data,
	     Lstream_data_count size)
{
	stdio_stream_t str = STDIO_STREAM_DATA(stream);
	Lstream_data_count val = fwrite(data, 1, size, str->file);
	if (!val && ferror(str->file)) {
		return -1;
	}
	return val;
}

static int
stdio_rewinder(lstream_t stream)
{
	stdio_stream_t p = STDIO_STREAM_DATA(stream);
	rewind(p->file);
	return 0;
}

static int
stdio_seekable_p(lstream_t stream)
{
	struct stat lestat;
	stdio_stream_t str = STDIO_STREAM_DATA(stream);

	if (fstat(fileno(str->file), &lestat) < 0) {
		return 0;
	}
	return S_ISREG(lestat.st_mode);
}

static int
stdio_flusher(lstream_t stream)
{
	stdio_stream_t str = STDIO_STREAM_DATA(stream);

	if (stream->flags & LSTREAM_FL_WRITE) {
		return fflush(str->file);
	} else {
		return 0;
	}
}

static int
stdio_closer(lstream_t stream)
{
	stdio_stream_t str = STDIO_STREAM_DATA(stream);

	if (str->closing) {
		return fclose(str->file);
	} else if (stream->flags & LSTREAM_FL_WRITE) {
		return fflush(str->file);
	} else {
		return 0;
	}
}

/*********** a file descriptor ***********/

typedef struct filedesc_stream_s *filedesc_stream_t;
struct filedesc_stream_s {
	int fd;
	int pty_max_bytes;
	Bufbyte eof_char;
	int starting_pos;
	int current_pos;
	int end_pos;
	int chars_sans_newline;
	bool closing:1;
	bool allow_quit:1;
	bool blocked_ok:1;
	bool pty_flushing:1;
	bool blocking_error_p:1;
};

#define FILEDESC_STREAM_DATA(stream) LSTREAM_TYPE_DATA (stream, filedesc)

DEFINE_LSTREAM_IMPLEMENTATION("filedesc", lstream_filedesc,
			      sizeof(struct filedesc_stream_s));

/* Make a stream that reads from or writes to a file descriptor FILEDESC.
   OFFSET is the offset from the *current* file pointer that the reading
   should start at.  COUNT is the number of bytes to be read (it is
   ignored when writing); -1 for unlimited. */
static Lisp_Object
make_filedesc_stream_1(int filedesc, int offset, int count, int flags,
		       const char *mode)
{
	Lisp_Object obj = Qnil;
        if (filedesc < 0)
		return obj;
	lstream_t lstr = Lstream_new(lstream_filedesc, mode);
	filedesc_stream_t fstr = FILEDESC_STREAM_DATA(lstr);

	fstr->fd = filedesc;
	fstr->closing = !!(flags & LSTR_CLOSING);
	fstr->allow_quit = !!(flags & LSTR_ALLOW_QUIT);
	fstr->blocked_ok = !!(flags & LSTR_BLOCKED_OK);
	fstr->pty_flushing = !!(flags & LSTR_PTY_FLUSHING);
	fstr->blocking_error_p = 0;
	fstr->chars_sans_newline = 0;
	fstr->starting_pos = lseek(filedesc, offset, SEEK_CUR);
	fstr->current_pos = max(fstr->starting_pos, 0);

	if (count < 0) {
		fstr->end_pos = -1;
	} else {
		fstr->end_pos = fstr->starting_pos + count;
	}
	lstr->flags |= LSTREAM_FL_CLOSE_AT_DISKSAVE;
	XSETLSTREAM(obj, lstr);
	return obj;
}

Lisp_Object
make_filedesc_input_stream(int filedesc, int offset, int count, int flags)
{
	return make_filedesc_stream_1(filedesc, offset, count, flags, "r");
}

Lisp_Object
make_filedesc_output_stream(int filedesc, int offset, int count, int flags)
{
	return make_filedesc_stream_1(filedesc, offset, count, flags, "w");
}

static Lstream_data_count
filedesc_reader(lstream_t stream, unsigned char *data, Lstream_data_count size)
{
	Lstream_data_count nread;
	filedesc_stream_t str = FILEDESC_STREAM_DATA(stream);

	if (str->end_pos >= 0) {
		size = min(size,
			   (Lstream_data_count)
			   (str->end_pos - str->current_pos));
	}
	nread = str->allow_quit
		? read_allowing_quit(str->fd, data, size)
		: read(str->fd, data, size);
	if (nread > 0) {
		str->current_pos += nread;
	}
	return nread;
}

static int
errno_would_block_p(int val)
{
#ifdef EWOULDBLOCK
	if (val == EWOULDBLOCK) {
		return 1;
	}
#endif
#ifdef EAGAIN
	if (val == EAGAIN) {
		return 1;
	}
#endif
	return 0;
}

static Lstream_data_count
filedesc_writer(lstream_t stream, const unsigned char *data,
		Lstream_data_count size)
{
	filedesc_stream_t str = FILEDESC_STREAM_DATA(stream);
	Lstream_data_count retval;
	int need_newline = 0;

	/* This function would be simple if it were not for the blasted
	   PTY max-bytes stuff.  Why the hell can't they just have written
	   the PTY drivers right so this problem doesn't exist?

	   Maybe all the PTY crap here should be moved into another stream
	   that does nothing but periodically insert EOF's as necessary. */
	if (str->pty_flushing) {
		/* To make life easy, only send out one line at the most. */
		const unsigned char *ptr;

		ptr = (const unsigned char *)memchr(data, '\n', size);
		if (ptr) {
			need_newline = 1;
		} else {
			ptr = data + size;
		}
		if (ptr - data >=
		    str->pty_max_bytes - str->chars_sans_newline) {
			ptr = data +
				str->pty_max_bytes - str->chars_sans_newline;
			need_newline = 0;
		}
		size = ptr - data;
	}

	/**** start of non-PTY-crap ****/
	if (size > 0) {
		retval = str->allow_quit
			? write_allowing_quit(str->fd, data, size)
			: write(str->fd, data, size);
	} else {
		retval = 0;
	}
	if (retval < 0 && errno_would_block_p(errno) && str->blocked_ok) {
		str->blocking_error_p = 1;
		return 0;
	}
	str->blocking_error_p = 0;
	if (retval < 0) {
		return retval;
	}
	/**** end non-PTY-crap ****/

	if (str->pty_flushing) {
		str->chars_sans_newline += retval;
		/* Note that a newline was not among the bytes written out.
		   Add to the number of non-newline bytes written out,
		   and flush with an EOF if necessary.  Be careful to
		   keep track of write errors as we go along and look
		   out for EWOULDBLOCK. */
		if (str->chars_sans_newline >= str->pty_max_bytes) {
			Lstream_data_count retval2 = str->allow_quit
				? write_allowing_quit(
					str->fd, &str->eof_char, 1)
				: write(str->fd, &str->eof_char, 1);

			if (retval2 > 0) {
				str->chars_sans_newline = 0;
			}
			else if (retval2 < 0) {
				/* Error writing the EOF char.  If nothing got
				   written, then treat this as an error --
				   either return an error condition or set the
				   blocking-error flag. */
				if (retval == 0) {
					if (errno_would_block_p(errno)
					    && str->blocked_ok) {
						str->blocking_error_p = 1;
						return 0;
					} else
						return retval2;
				} else {
					return retval;
				}
			}
		}
	}

	/* The need_newline flag is necessary because otherwise when the
	   first byte is a newline, we'd get stuck never writing anything
	   in pty-flushing mode. */
	if (need_newline) {
		Bufbyte nl = '\n';
		Lstream_data_count retval2 = str->allow_quit
			? write_allowing_quit(str->fd, &nl, 1)
			: write(str->fd, &nl, 1);

		if (retval2 > 0) {
			str->chars_sans_newline = 0;
			retval++;
		} else if (retval2 < 0) {
			/* Error writing the newline char.  If nothing got
			   written, then treat this as an error -- either return
			   an error condition or set the blocking-error flag. */
			if (retval == 0) {
				if (errno_would_block_p(errno)
				    && str->blocked_ok) {
					str->blocking_error_p = 1;
					return 0;
				} else {
					return retval2;
				}
			} else {
				return retval;
			}
		}
	}

	return retval;
}

static int
filedesc_rewinder(lstream_t stream)
{
	filedesc_stream_t str = FILEDESC_STREAM_DATA(stream);
	if (str->starting_pos < 0 ||
	    lseek(FILEDESC_STREAM_DATA(stream)->fd, str->starting_pos,
		  SEEK_SET) == -1) {
		return -1;
	} else {
		str->current_pos = str->starting_pos;
		return 0;
	}
}

static int
filedesc_seekable_p(lstream_t stream)
{
	filedesc_stream_t str = FILEDESC_STREAM_DATA(stream);

	if (str->starting_pos < 0) {
		return 0;
	} else {
		struct stat lestat;

		if (fstat(str->fd, &lestat) < 0) {
			return 0;
		}
		return S_ISREG(lestat.st_mode);
	}
}

static int
filedesc_closer(lstream_t stream)
{
	filedesc_stream_t str = FILEDESC_STREAM_DATA(stream);

	if (str->closing) {
		return close(str->fd);
	} else {
		return 0;
	}
}

static int
filedesc_was_blocked_p(lstream_t stream)
{
	filedesc_stream_t str = FILEDESC_STREAM_DATA(stream);
	return str->blocking_error_p;
}

void
filedesc_stream_set_pty_flushing(lstream_t stream, int pty_max_bytes,
				 Bufbyte eof_char)
{
	filedesc_stream_t str = FILEDESC_STREAM_DATA(stream);
	str->pty_max_bytes = pty_max_bytes;
	str->eof_char = eof_char;
	str->pty_flushing = 1;
	return;
}

static int
filedesc_get_fd(lstream_t stream)
{
	filedesc_stream_t str = FILEDESC_STREAM_DATA(stream);
	return str->fd;
}

/*********** read from a Lisp string ***********/

#define LISP_STRING_STREAM_DATA(stream) LSTREAM_TYPE_DATA (stream, lisp_string)

typedef struct lisp_string_stream_s *lisp_string_stream_t;
struct lisp_string_stream_s {
	Lisp_Object obj;
	Bytecount init_offset;
	Bytecount offset, end;
};

DEFINE_LSTREAM_IMPLEMENTATION("lisp-string", lstream_lisp_string,
			      sizeof(struct lisp_string_stream_s));

Lisp_Object
make_lisp_string_input_stream(Lisp_Object string, Bytecount offset,
			      Bytecount len)
{
	Lisp_Object obj;
	lstream_t lstr;
	lisp_string_stream_t str;

	CHECK_STRING(string);
	if (len < 0) {
		len = XSTRING_LENGTH(string) - offset;
	}
	assert(offset >= 0);
	assert(len >= 0);
	assert(offset + len <= XSTRING_LENGTH(string));

	lstr = Lstream_new(lstream_lisp_string, "r");
	str = LISP_STRING_STREAM_DATA(lstr);
	str->offset = offset;
	str->end = offset + len;
	str->init_offset = offset;
	str->obj = string;
	XSETLSTREAM(obj, lstr);
	return obj;
}

static Lstream_data_count
lisp_string_reader(lstream_t stream, unsigned char *data,
		   Lstream_data_count size)
{
	lisp_string_stream_t str = LISP_STRING_STREAM_DATA(stream);
	/* Don't lose if the string shrank past us ... */
	Bytecount offset = min(str->offset, XSTRING_LENGTH(str->obj));
	Bufbyte *strstart = XSTRING_DATA(str->obj);
	Bufbyte *start = strstart + offset;

	/* ... or if someone changed the string and we ended up in the
	   middle of a character. */
	/* Being in the middle of a character is `normal' unless
	   LSTREAM_NO_PARTIAL_CHARS - mrb */
	if (stream->flags & LSTREAM_FL_NO_PARTIAL_CHARS) {
		VALIDATE_CHARPTR_BACKWARD(start);
	}
	offset = start - strstart;
	size = min(size, (Lstream_data_count) (str->end - offset));
	memcpy(data, start, size);
	str->offset = offset + size;
	return size;
}

static int
lisp_string_rewinder(lstream_t stream)
{
	lisp_string_stream_t str = LISP_STRING_STREAM_DATA(stream);
	int pos = str->init_offset;

	if (pos > str->end) {
		pos = str->end;
	}
	/* Don't lose if the string shrank past us ... */
	pos = min(pos, XSTRING_LENGTH(str->obj));
	/* ... or if someone changed the string and we ended up in the
	   middle of a character. */
	{
		Bufbyte *strstart = XSTRING_DATA(str->obj);
		Bufbyte *start = strstart + pos;
		VALIDATE_CHARPTR_BACKWARD(start);
		pos = start - strstart;
	}
	str->offset = pos;
	return 0;
}

static Lisp_Object
lisp_string_marker(Lisp_Object stream)
{
	lisp_string_stream_t str = LISP_STRING_STREAM_DATA(XLSTREAM(stream));
	return str->obj;
}

/*********** a fixed buffer ***********/

#define FIXED_BUFFER_STREAM_DATA(stream)		\
	LSTREAM_TYPE_DATA (stream, fixed_buffer)

typedef struct fixed_buffer_stream_s *fixed_buffer_stream_t;
struct fixed_buffer_stream_s {
	const unsigned char *inbuf;
	unsigned char *outbuf;
	Lstream_data_count size;
	Lstream_data_count offset;
};

DEFINE_LSTREAM_IMPLEMENTATION("fixed-buffer", lstream_fixed_buffer,
			      sizeof(struct fixed_buffer_stream_s));

Lisp_Object
make_fixed_buffer_input_stream(const void *buf, Lstream_data_count size)
{
	Lisp_Object obj;
	lstream_t lstr = Lstream_new(lstream_fixed_buffer, "r");
	fixed_buffer_stream_t str = FIXED_BUFFER_STREAM_DATA(lstr);
	str->inbuf = (const unsigned char *)buf;
	str->size = size;
	XSETLSTREAM(obj, lstr);
	return obj;
}

Lisp_Object
make_fixed_buffer_output_stream(void *buf, Lstream_data_count size)
{
	Lisp_Object obj;
	lstream_t lstr = Lstream_new(lstream_fixed_buffer, "w");
	fixed_buffer_stream_t str = FIXED_BUFFER_STREAM_DATA(lstr);
	str->outbuf = (unsigned char *)buf;
	str->size = size;
	XSETLSTREAM(obj, lstr);
	return obj;
}

static Lstream_data_count
fixed_buffer_reader(lstream_t stream, unsigned char *data,
		    Lstream_data_count size)
{
	fixed_buffer_stream_t str = FIXED_BUFFER_STREAM_DATA(stream);
	size = min(size, str->size - str->offset);
	memcpy(data, str->inbuf + str->offset, size);
	str->offset += size;
	return size;
}

static Lstream_data_count
fixed_buffer_writer(lstream_t stream, const unsigned char *data,
		    Lstream_data_count size)
{
	fixed_buffer_stream_t str = FIXED_BUFFER_STREAM_DATA(stream);
	if (str->offset == str->size) {
		/* If we're at the end, just throw away the data and pretend
		   we wrote all of it.  If we return 0, then the lstream routines
		   will try again and again to write it out. */
		return size;
	}
	size = min(size, str->size - str->offset);
	memcpy(str->outbuf + str->offset, data, size);
	str->offset += size;
	return size;
}

static int
fixed_buffer_rewinder(lstream_t stream)
{
	fixed_buffer_stream_t p = FIXED_BUFFER_STREAM_DATA(stream);
	p->offset = 0;
	return 0;
}

const unsigned char*
fixed_buffer_input_stream_ptr(lstream_t stream)
{
	fixed_buffer_stream_t p = FIXED_BUFFER_STREAM_DATA(stream);
	assert(stream->imp == lstream_fixed_buffer);
	return p->inbuf;
}

unsigned char*
fixed_buffer_output_stream_ptr(lstream_t stream)
{
	fixed_buffer_stream_t p = FIXED_BUFFER_STREAM_DATA(stream);
	assert(stream->imp == lstream_fixed_buffer);
	return p->outbuf;
}

/*********** write to a resizing buffer ***********/

#define RESIZING_BUFFER_STREAM_DATA(stream)		\
	LSTREAM_TYPE_DATA (stream, resizing_buffer)

typedef struct resizing_buffer_stream_s *resizing_buffer_stream_t;
struct resizing_buffer_stream_s {
	unsigned char *buf;
	Lstream_data_count allocked;
	int max_stored;
	int stored;
};

DEFINE_LSTREAM_IMPLEMENTATION("resizing-buffer", lstream_resizing_buffer,
			      sizeof(struct resizing_buffer_stream_s));

Lisp_Object make_resizing_buffer_output_stream(void)
{
	Lisp_Object obj;
	XSETLSTREAM(obj, Lstream_new(lstream_resizing_buffer, "w"));
	return obj;
}

static Lstream_data_count
resizing_buffer_writer(lstream_t stream, const unsigned char *data,
		       Lstream_data_count size)
{
	resizing_buffer_stream_t str = RESIZING_BUFFER_STREAM_DATA(stream);

	LSTR_ALLOC_TO(str->buf, str->allocked, str->stored + size,
		      unsigned char);
	memcpy(str->buf + str->stored, data, size);
	str->stored += size;
	str->max_stored = max(str->max_stored, str->stored);
	return size;
}

static int
resizing_buffer_rewinder(lstream_t stream)
{
	resizing_buffer_stream_t p = RESIZING_BUFFER_STREAM_DATA(stream);
	p->stored = 0;
	return 0;
}

static int
resizing_buffer_closer(lstream_t stream)
{
	resizing_buffer_stream_t str = RESIZING_BUFFER_STREAM_DATA(stream);

	if (str->buf) {
		xfree(str->buf);
		str->buf = 0;
	}
	return 0;
}

unsigned char*
resizing_buffer_stream_ptr(lstream_t stream)
{
	resizing_buffer_stream_t p = RESIZING_BUFFER_STREAM_DATA(stream);
	return p->buf;
}

/*********** write to an unsigned-char dynarr ***********/

/* Note: If you have a dynarr whose type is not unsigned_char_dynarr
   but which is really just an unsigned_char_dynarr (e.g. its type
   is Bufbyte or Extbyte), just cast to unsigned_char_dynarr. */

#define DYNARR_STREAM_DATA(stream)		\
	LSTREAM_TYPE_DATA (stream, dynarr)

typedef struct dynarr_stream_s *dynarr_stream_t;
struct dynarr_stream_s {
	unsigned_char_dynarr *dyn;
};

DEFINE_LSTREAM_IMPLEMENTATION("dynarr", lstream_dynarr,
			      sizeof(struct dynarr_stream_s));

Lisp_Object
make_dynarr_output_stream(unsigned_char_dynarr * dyn)
{
	Lisp_Object obj;
	dynarr_stream_t p;

	XSETLSTREAM(obj, Lstream_new(lstream_dynarr, "w"));
	p = DYNARR_STREAM_DATA(XLSTREAM(obj));
	p->dyn = dyn;
	return obj;
}

static Lstream_data_count
dynarr_writer(lstream_t stream, const unsigned char *data,
	      Lstream_data_count size)
{
	dynarr_stream_t str = DYNARR_STREAM_DATA(stream);
	Dynarr_add_many(str->dyn, data, size);
	return size;
}

static int
dynarr_rewinder(lstream_t stream)
{
	dynarr_stream_t p = DYNARR_STREAM_DATA(stream);
	Dynarr_reset(p->dyn);
	return 0;
}

static int
dynarr_closer(lstream_t stream)
{
	return 0;
}

/************ read from or write to a Lisp buffer ************/

/* Note: Lisp-buffer read streams never return partial characters,
   and Lisp-buffer write streams expect to never get partial
   characters. */

#define LISP_BUFFER_STREAM_DATA(stream)		\
	LSTREAM_TYPE_DATA (stream, lisp_buffer)

typedef struct lisp_buffer_stream_s *lisp_buffer_stream_t;
struct lisp_buffer_stream_s {
	Lisp_Object buffer;
	Lisp_Object orig_start;
	/* we use markers to properly deal with insertion/deletion */
	Lisp_Object start, end;
	int flags;
};

DEFINE_LSTREAM_IMPLEMENTATION("lisp-buffer", lstream_lisp_buffer,
			      sizeof(struct lisp_buffer_stream_s));

static Lisp_Object
make_lisp_buffer_stream_1(struct buffer *buf, Bufpos start, Bufpos end,
			  int flags, const char *mode)
{
	Lisp_Object obj;
	lstream_t lstr;
	lisp_buffer_stream_t str;
	Bufpos bmin, bmax;
	int reading = !strcmp(mode, "r");

	/* Make sure the luser didn't pass "w" in. */
	if (!strcmp(mode, "w")) {
		abort();
	}

	if (flags & LSTR_IGNORE_ACCESSIBLE) {
		bmin = BUF_BEG(buf);
		bmax = BUF_Z(buf);
	} else {
		bmin = BUF_BEGV(buf);
		bmax = BUF_ZV(buf);
	}

	if (start == -1) {
		start = bmin;
	}
	if (end == -1) {
		end = bmax;
	}
	assert(bmin <= start);
	assert(start <= bmax);
	if (reading) {
		assert(bmin <= end);
		assert(end <= bmax);
		assert(start <= end);
	}

	lstr = Lstream_new(lstream_lisp_buffer, mode);
	str = LISP_BUFFER_STREAM_DATA(lstr);
	{
		Lisp_Object marker;
		Lisp_Object buffer;

		XSETBUFFER(buffer, buf);
		marker = Fmake_marker();
		Fset_marker(marker, make_int(start), buffer);
		str->start = marker;
		marker = Fmake_marker();
		Fset_marker(marker, make_int(start), buffer);
		str->orig_start = marker;
		if (reading) {
			marker = Fmake_marker();
			Fset_marker(marker, make_int(end), buffer);
			str->end = marker;
		} else {
			str->end = Qnil;
		}
		str->buffer = buffer;
	}
	str->flags = flags;
	XSETLSTREAM(obj, lstr);
	return obj;
}

Lisp_Object
make_lisp_buffer_input_stream(struct buffer *buf, Bufpos start, Bufpos end,
			      int flags)
{
	return make_lisp_buffer_stream_1(buf, start, end, flags, "r");
}

Lisp_Object
make_lisp_buffer_output_stream(struct buffer *buf, Bufpos pos, int flags)
{
	Lisp_Object lstr = make_lisp_buffer_stream_1(buf, pos, 0, flags, "wc");

	Lstream_set_character_mode(XLSTREAM(lstr));
	return lstr;
}

static Lstream_data_count
lisp_buffer_reader(lstream_t stream, unsigned char *data,
		   Lstream_data_count size)
{
	lisp_buffer_stream_t str = LISP_BUFFER_STREAM_DATA(stream);
	unsigned char *orig_data = data;
	Bytind start;
	Bytind end;
	struct buffer *buf = XBUFFER(str->buffer);

	if (!BUFFER_LIVE_P(buf)) {
		/* Fut. */
		return 0;
	}

	/* NOTE: We do all our operations in Bytind's.
	   Keep in mind that SIZE is a value in bytes, not chars. */

	start = bi_marker_position(str->start);
	end = bi_marker_position(str->end);
	if (!(str->flags & LSTR_IGNORE_ACCESSIBLE)) {
		start = bytind_clip_to_bounds(BI_BUF_BEGV(buf), start,
					      BI_BUF_ZV(buf));
		end = bytind_clip_to_bounds(BI_BUF_BEGV(buf), end,
					    BI_BUF_ZV(buf));
	}

	size = min(size, (Lstream_data_count) (end - start));
	end = start + size;
	/* We cannot return a partial character. */
	VALIDATE_BYTIND_BACKWARD(buf, end);

	while (start < end) {
		Bytind _ceil_;
		Bytecount chunk;

		if (str->flags & LSTR_IGNORE_ACCESSIBLE) {
			_ceil_ = BI_BUF_CEILING_OF_IGNORE_ACCESSIBLE(
				buf, start);
		} else {
			_ceil_ = BI_BUF_CEILING_OF(buf, start);
		}
		chunk = min(_ceil_, end) - start;
		memcpy(data, BI_BUF_BYTE_ADDRESS(buf, start), chunk);
		data += chunk;
		start += chunk;
	}

	if (EQ(buf->selective_display, Qt) && str->flags & LSTR_SELECTIVE) {
		/* What a kludge.  What a kludge.  What a kludge. */
		unsigned char *p;
		for (p = orig_data; p < data; p++) {
			if (*p == '\r') {
				*p = '\n';
			}
		}
	}

	set_bi_marker_position(str->start, end);
	return data - orig_data;
}

static Lstream_data_count
lisp_buffer_writer(lstream_t stream, const unsigned char *data,
		   Lstream_data_count size)
{
	lisp_buffer_stream_t str = LISP_BUFFER_STREAM_DATA(stream);
	Bufpos pos;
	struct buffer *buf = XBUFFER(str->buffer);

	if (!BUFFER_LIVE_P(buf)) {
		/* Fut. */
		return 0;
	}

	pos = marker_position(str->start);
	pos += buffer_insert_raw_string_1(buf, pos, data, size, 0);
	set_marker_position(str->start, pos);
	return size;
}

static int
lisp_buffer_rewinder(lstream_t stream)
{
	lisp_buffer_stream_t str = LISP_BUFFER_STREAM_DATA(stream);
	struct buffer *buf = XBUFFER(str->buffer);
	long pos = marker_position(str->orig_start);

	if (!BUFFER_LIVE_P(buf)) {
		/* Fut. */
		return -1;
	}
	if (pos > BUF_ZV(buf)) {
		pos = BUF_ZV(buf);
	}
	if (pos < marker_position(str->orig_start)) {
		pos = marker_position(str->orig_start);
	}
	if (MARKERP(str->end) && pos > marker_position(str->end)) {
		pos = marker_position(str->end);
	}
	set_marker_position(str->start, pos);
	return 0;
}

static Lisp_Object
lisp_buffer_marker(Lisp_Object stream)
{
	lisp_buffer_stream_t str = LISP_BUFFER_STREAM_DATA(XLSTREAM(stream));

	mark_object(str->start);
	mark_object(str->end);
	return str->buffer;
}

Bufpos
lisp_buffer_stream_startpos(lstream_t stream)
{
	lisp_buffer_stream_t p = LISP_BUFFER_STREAM_DATA(stream);
	return marker_position(p->start);
}

#if defined(HAVE_OPENSSL) && defined(OPENSSL_SSL)
#include "openssl.h"
/* SSL things */
typedef struct ssl_stream_s *ssl_stream_t;
struct ssl_stream_s {
	SSL *conn;
	bool closing:1;
	bool allow_quit:1;
	bool blocked_ok:1;
	bool blocking_error_p:1;
};

#define SSL_STREAM_DATA(stream) LSTREAM_TYPE_DATA (stream, ssl)

DEFINE_LSTREAM_IMPLEMENTATION("ssl", lstream_ssl, sizeof(struct ssl_stream_s));

static Lisp_Object
make_ssl_stream_1(SSL *conn, int flags, const char *mode)
{
	Lisp_Object obj;
	lstream_t lstr = Lstream_new(lstream_ssl, mode);
	ssl_stream_t str = SSL_STREAM_DATA(lstr);
	str->conn = conn;
	XSETLSTREAM(obj, lstr);
	return obj;
}

Lisp_Object
make_ssl_input_stream(SSL *conn, int flags)
{
	return make_ssl_stream_1(conn, flags, "r");
}

Lisp_Object
make_ssl_output_stream(SSL *conn, int flags)
{
	return make_ssl_stream_1(conn, flags, "w");
}

static Lstream_data_count
ssl_reader(lstream_t stream, unsigned char *data, Lstream_data_count size)
{
	Lstream_data_count nread;
	ssl_stream_t str = SSL_STREAM_DATA(stream);

	nread = SSL_read(str->conn, data, size);
	if (nread < 0) {
		return -1;
	}
	return nread;
}

static Lstream_data_count
ssl_writer(lstream_t stream, const unsigned char *data,
	   Lstream_data_count size)
{
	Lstream_data_count nwrite;
	ssl_stream_t str = SSL_STREAM_DATA(stream);

	nwrite = SSL_write(str->conn, data, size);
	if (nwrite < 0) {
		return -1;
	}
	return nwrite;
}

static int
ssl_closer(lstream_t stream)
{
	ssl_stream_t str = SSL_STREAM_DATA(stream);
	if (str->closing) {
		return SSL_shutdown(str->conn);
	} else {
		return 0;
	}
}

static int
ssl_get_fd(lstream_t stream)
{
	ssl_stream_t str = SSL_STREAM_DATA(stream);
	return SSL_get_rfd(str->conn);
}
#endif

/************************************************************************/
/*                            initialization                            */
/************************************************************************/

void lstream_type_create(void)
{
	LSTREAM_HAS_METHOD(stdio, reader);
	LSTREAM_HAS_METHOD(stdio, writer);
	LSTREAM_HAS_METHOD(stdio, rewinder);
	LSTREAM_HAS_METHOD(stdio, seekable_p);
	LSTREAM_HAS_METHOD(stdio, flusher);
	LSTREAM_HAS_METHOD(stdio, closer);

	LSTREAM_HAS_METHOD(filedesc, reader);
	LSTREAM_HAS_METHOD(filedesc, writer);
	LSTREAM_HAS_METHOD(filedesc, was_blocked_p);
	LSTREAM_HAS_METHOD(filedesc, rewinder);
	LSTREAM_HAS_METHOD(filedesc, seekable_p);
	LSTREAM_HAS_METHOD(filedesc, closer);
	LSTREAM_HAS_METHOD(filedesc, get_fd);

	LSTREAM_HAS_METHOD(lisp_string, reader);
	LSTREAM_HAS_METHOD(lisp_string, rewinder);
	LSTREAM_HAS_METHOD(lisp_string, marker);

	LSTREAM_HAS_METHOD(fixed_buffer, reader);
	LSTREAM_HAS_METHOD(fixed_buffer, writer);
	LSTREAM_HAS_METHOD(fixed_buffer, rewinder);

	LSTREAM_HAS_METHOD(resizing_buffer, writer);
	LSTREAM_HAS_METHOD(resizing_buffer, rewinder);
	LSTREAM_HAS_METHOD(resizing_buffer, closer);

	LSTREAM_HAS_METHOD(dynarr, writer);
	LSTREAM_HAS_METHOD(dynarr, rewinder);
	LSTREAM_HAS_METHOD(dynarr, closer);

	LSTREAM_HAS_METHOD(lisp_buffer, reader);
	LSTREAM_HAS_METHOD(lisp_buffer, writer);
	LSTREAM_HAS_METHOD(lisp_buffer, rewinder);
	LSTREAM_HAS_METHOD(lisp_buffer, marker);

#if defined(HAVE_OPENSSL) && defined(OPENSSL_SSL)
	LSTREAM_HAS_METHOD(ssl, reader);
	LSTREAM_HAS_METHOD(ssl, writer);
	LSTREAM_HAS_METHOD(ssl, closer);
	LSTREAM_HAS_METHOD(ssl, get_fd);
#endif
}

void reinit_vars_of_lstream(void)
{
#if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
	for (int i = 0; i < countof(Vlstream_free_list); i++) {
		Vlstream_free_list[i] = Qnil;
		staticpro_nodump(&Vlstream_free_list[i]);
	}
#endif	/* !BDWGC */
}

void vars_of_lstream(void)
{
	INIT_LRECORD_IMPLEMENTATION(lstream);

	reinit_vars_of_lstream();
}

/* lstream.c ends here */
