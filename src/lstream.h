/* Generic stream implementation -- header file.
   Copyright (C) 1995 Free Software Foundation, Inc.
   Copyright (C) 1996 Ben Wing.

This file is part of XEmacs.

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

/* Written by Ben Wing. */

#ifndef INCLUDED_lstream_h_
#define INCLUDED_lstream_h_

/************************************************************************/
/*                     definition of Lstream object                     */
/************************************************************************/

DECLARE_LRECORD (lstream, struct lstream);
#define XLSTREAM(x) XRECORD (x, lstream, struct lstream)
#define XSETLSTREAM(x, p) XSETRECORD (x, p, lstream)
#define LSTREAMP(x) RECORDP (x, lstream)
/* #define CHECK_LSTREAM(x) CHECK_RECORD (x, lstream)
   Lstream pointers should never escape to the Lisp level, so
   functions should not be doing this. */

#ifndef EOF
#define EOF (-1)
#endif

/* Typedef specifying a count of bytes in a data block to be written
   out or read in, using Lstream_read(), Lstream_write(), and related
   functions.  This MUST BE SIGNED, since it also is used in functions
   that return the number of bytes actually read to or written from in
   an operation, and these functions can return -1 to signal error.
   
   Note that the standard Unix read() and write() functions define the
   count going in as a size_t, which is UNSIGNED, and the count going
   out as an ssize_t, which is SIGNED.  This is a horrible design
   flaw.  Not only is it highly likely to lead to logic errors when a
   -1 gets interpreted as a large positive number, but operations are
   bound to fail in all sorts of horrible ways when a number in the
   upper-half of the size_t range is passed in -- this number is
   unrepresentable as an ssize_t, so code that checks to see how many
   bytes are actually written (which is mandatory if you are dealing
   with certain types of devices) will get completely screwed up.
*/

typedef EMACS_INT Lstream_data_count;

typedef enum lstream_buffering
{
  /* No buffering. */
  LSTREAM_UNBUFFERED,
  /* Buffer until a '\n' character is reached. */
  LSTREAM_LINE_BUFFERED,
  /* Buffer in standard-size (i.e. 512-byte) blocks. */
  LSTREAM_BLOCK_BUFFERED,
  /* Buffer in blocks of a specified size. */
  LSTREAM_BLOCKN_BUFFERED,
  /* Buffer until the stream is closed (only applies to write-only
     streams).  Only one call to the stream writer will be made,
     and that is when the stream is closed. */
  LSTREAM_UNLIMITED
} Lstream_buffering;

/* Methods defining how this stream works.  Some may be undefined. */

/* We do not implement the seek/tell paradigm.  I tried to do that,
   but getting the semantics right in the presence of buffering is
   extremely tricky and very error-prone and basically not worth it.
   This is especially the case with complicated streams like
   decoding streams -- the seek pointer in this case can't be a single
   integer but has to be a whole complicated structure that records
   all of the stream's state at the time.

   Rewind semantics are generally easy to implement, so we do provide
   a rewind method.  Even rewind() may not be available on a stream,
   however -- e.g. on process output. */

typedef struct lstream_implementation
{
  const char *name;
  Lstream_data_count size; /* Number of additional bytes to be
			      allocated with this stream.  Access this
			      data using Lstream_data(). */
  /* Read some data from the stream's end and store it into DATA, which
     can hold SIZE bytes.  Return the number of bytes read.  A return
     value of 0 means no bytes can be read at this time.  This may
     be because of an EOF, or because there is a granularity greater
     than one byte that the stream imposes on the returned data, and
     SIZE is less than this granularity. (This will happen frequently
     for streams that need to return whole characters, because
     Lstream_read() calls the reader function repeatedly until it
     has the number of bytes it wants or until 0 is returned.)
     The lstream functions do not treat a 0 return as EOF or do
     anything special; however, the calling function will interpret
     any 0 it gets back as EOF.  This will normally not happen unless
     the caller calls Lstream_read() with a very small size.

     This function can be NULL if the stream is output-only. */
  /* The omniscient mly, blinded by the irresistible thrall of Common
     Lisp, thinks that it is bogus that the types and implementations
     of input and output streams are the same. */
  Lstream_data_count (*reader) (Lstream *stream, unsigned char *data,
				Lstream_data_count size);
  /* Send some data to the stream's end.  Data to be sent is in DATA
     and is SIZE bytes.  Return the number of bytes sent.  This
     function can send and return fewer bytes than is passed in; in
     that case, the function will just be called again until there is
     no data left or 0 is returned.  A return value of 0 means that no
     more data can be currently stored, but there is no error; the
     data will be squirrelled away until the writer can accept
     data. (This is useful, e.g., of you're dealing with a
     non-blocking file descriptor and are getting EWOULDBLOCK errors.)
     This function can be NULL if the stream is input-only. */
  Lstream_data_count (*writer) (Lstream *stream, const unsigned char *data,
				Lstream_data_count size);
  /* Return non-zero if the last write operation on the stream resulted
     in an attempt to block (EWOULDBLOCK). If this method does not
     exists, the implementation returns 0 */
  int (*was_blocked_p) (Lstream *stream);
  /* Rewind the stream.  If this is NULL, the stream is not seekable. */
  int (*rewinder) (Lstream *stream);
  /* Indicate whether this stream is seekable -- i.e. it can be rewound.
     This method is ignored if the stream does not have a rewind
     method.  If this method is not present, the result is determined
     by whether a rewind method is present. */
  int (*seekable_p) (Lstream *stream);
  /* Perform any additional operations necessary to flush the
     data in this stream. */
  int (*flusher) (Lstream *stream);
  /* Perform any additional operations necessary to close this
     stream down.  May be NULL.  This function is called when
     Lstream_close() is called or when the stream is garbage-
     collected.  When this function is called, all pending data
     in the stream will already have been written out. */
  int (*closer) (Lstream *stream);
  /* Mark this object for garbage collection.  Same semantics as
     a standard Lisp_Object marker.  This function can be NULL. */
  Lisp_Object (*marker) (Lisp_Object lstream);
} Lstream_implementation;

#define DEFINE_LSTREAM_IMPLEMENTATION(name,c_name,size)	\
 Lstream_implementation c_name[1] =			\
   { { (name), (size) } }

#define LSTREAM_FL_IS_OPEN		1
#define LSTREAM_FL_READ			2
#define LSTREAM_FL_WRITE		4
#define LSTREAM_FL_NO_PARTIAL_CHARS	8
#define LSTREAM_FL_CLOSE_AT_DISKSAVE	16

struct lstream
{
  struct lcrecord_header header;
  const Lstream_implementation *imp; /* methods for this stream */
  Lstream_buffering buffering; /* type of buffering in use */
  Lstream_data_count buffering_size; /* number of bytes buffered */

  unsigned char *in_buffer; /* holds characters read from stream end */
  Lstream_data_count in_buffer_size; /* allocated size of buffer */
  Lstream_data_count in_buffer_current; /* number of characters in buffer */
  Lstream_data_count in_buffer_ind; /* pointer to next character to
				       take from buffer */

  unsigned char *out_buffer; /* holds characters to write to stream end */
  Lstream_data_count out_buffer_size; /* allocated size of buffer */
  Lstream_data_count out_buffer_ind; /* pointer to next buffer spot to
					write a character */

  /* The unget buffer is more or less a stack -- things get pushed
     onto the end and read back from the end.  Lstream_read()
     basically reads backwards from the end to get stuff; Lstream_unread()
     similarly has to push the data on backwards. */
  unsigned char *unget_buffer; /* holds characters pushed back onto input */
  Lstream_data_count unget_buffer_size; /* allocated size of buffer */
  Lstream_data_count unget_buffer_ind; /* pointer to next buffer spot
					  to write a character */

  Lstream_data_count byte_count;
  int flags;
  max_align_t data[1];
};

#define LSTREAM_TYPE_P(lstr, type) \
  ((lstr)->imp == lstream_##type)

#ifdef ERROR_CHECK_TYPECHECK
INLINE_HEADER struct lstream *
error_check_lstream_type (struct lstream *stream,
			  const Lstream_implementation *imp);
INLINE_HEADER struct lstream *
error_check_lstream_type (struct lstream *stream,
			  const Lstream_implementation *imp)
{
  assert (stream->imp == imp);
  return stream;
}
# define LSTREAM_TYPE_DATA(lstr, type) \
  ((struct type##_stream *) \
    Lstream_data (error_check_lstream_type(lstr, lstream_##type)))
#else
# define LSTREAM_TYPE_DATA(lstr, type)		\
  ((struct type##_stream *) Lstream_data (lstr))
#endif

/* Declare that lstream-type TYPE has method M; used in
   initialization routines */
#define LSTREAM_HAS_METHOD(type, m) \
  (lstream_##type->m = type##_##m)


Lstream *Lstream_new (const Lstream_implementation *imp,
		      const char *mode);
void Lstream_reopen (Lstream *lstr);
void Lstream_set_buffering (Lstream *lstr, Lstream_buffering buffering,
			    int buffering_size);
int Lstream_flush (Lstream *lstr);
int Lstream_flush_out (Lstream *lstr);
int Lstream_fputc (Lstream *lstr, int c);
int Lstream_fgetc (Lstream *lstr);
void Lstream_fungetc (Lstream *lstr, int c);
Lstream_data_count Lstream_read (Lstream *lstr, void *data,
				 Lstream_data_count size);
Lstream_data_count Lstream_write (Lstream *lstr, const void *data,
				  Lstream_data_count size);
int Lstream_was_blocked_p (Lstream *lstr);
void Lstream_unread (Lstream *lstr, const void *data, Lstream_data_count size);
int Lstream_rewind (Lstream *lstr);
int Lstream_seekable_p (Lstream *lstr);
int Lstream_close (Lstream *lstr);
void Lstream_delete (Lstream *lstr);
void Lstream_set_character_mode (Lstream *str);

/* Call the function equivalent if the out buffer is full.  Otherwise,
   add to the end of the out buffer and, if line buffering is called for
   and the character marks the end of a line, write out the buffer. */

#define Lstream_putc(stream, c) 					\
  ((stream)->out_buffer_ind >= (stream)->out_buffer_size ?		\
   Lstream_fputc (stream, c) :						\
   ((stream)->out_buffer[(stream)->out_buffer_ind++] =			\
    (unsigned char) (c),						\
    (stream)->byte_count++,						\
    (stream)->buffering == LSTREAM_LINE_BUFFERED &&			\
    (stream)->out_buffer[(stream)->out_buffer_ind - 1] == '\n' ?	\
    Lstream_flush_out (stream) : 0))

/* Retrieve from unget buffer if there are any characters there;
   else retrieve from in buffer if there's anything there;
   else call the function equivalent */
#define Lstream_getc(stream) 						\
  ((stream)->unget_buffer_ind > 0 ?					\
   ((stream)->byte_count++,						\
    (stream)->unget_buffer[--(stream)->unget_buffer_ind]) :		\
   (stream)->in_buffer_ind < (stream)->in_buffer_current ?		\
    ((stream)->byte_count++,						\
     (stream)->in_buffer[(stream)->in_buffer_ind++]) :			\
    Lstream_fgetc (stream))

/* Add to the end if it won't overflow buffer; otherwise call the
   function equivalent */
#define Lstream_ungetc(stream, c)					\
  ((stream)->unget_buffer_ind >= (stream)->unget_buffer_size ?		\
   Lstream_fungetc (stream, c) :					\
   (void) ((stream)->byte_count--,					\
   ((stream)->unget_buffer[(stream)->unget_buffer_ind++] =		\
    (unsigned char) (c))))

#define Lstream_data(stream) ((void *) ((stream)->data))
#define Lstream_byte_count(stream) ((stream)->byte_count)


/************************************************************************/
/*             working with an Lstream as a stream of Emchars           */
/************************************************************************/

#ifdef MULE

#ifndef BYTE_ASCII_P
#include "mule-charset.h"
#endif

INLINE_HEADER Emchar Lstream_get_emchar (Lstream *stream);
INLINE_HEADER Emchar
Lstream_get_emchar (Lstream *stream)
{
  int c = Lstream_getc (stream);
  return (c < 0x80		/* c == EOF || BYTE_ASCII_P (c) */
	  ? (Emchar) c
	  : Lstream_get_emchar_1 (stream, c));
}

INLINE_HEADER int Lstream_put_emchar (Lstream *stream, Emchar ch);
INLINE_HEADER int
Lstream_put_emchar (Lstream *stream, Emchar ch)
{
  return CHAR_ASCII_P (ch) ?
    Lstream_putc (stream, ch) :
    Lstream_fput_emchar (stream, ch);
}

INLINE_HEADER void Lstream_unget_emchar (Lstream *stream, Emchar ch);
INLINE_HEADER void
Lstream_unget_emchar (Lstream *stream, Emchar ch)
{
  if (CHAR_ASCII_P (ch))
    Lstream_ungetc (stream, ch);
  else
    Lstream_funget_emchar (stream, ch);
}
#else /* not MULE */

# define Lstream_get_emchar(stream) Lstream_getc (stream)
# define Lstream_put_emchar(stream, ch) Lstream_putc (stream, ch)
# define Lstream_unget_emchar(stream, ch) Lstream_ungetc (stream, ch)

#endif /* not MULE */


/************************************************************************/
/*                        Lstream implementations                       */
/************************************************************************/

/* Flags we can pass to the filedesc and stdio streams. */

/* If set, close the descriptor or FILE * when the stream is closed. */
#define LSTR_CLOSING 1

/* If set, allow quitting out of the actual I/O. */
#define LSTR_ALLOW_QUIT 2

/* If set and filedesc_stream_set_pty_flushing() has been called
   on the stream, do not send more than pty_max_bytes on a single
   line without flushing the data out using the eof_char. */
#define LSTR_PTY_FLUSHING 4

/* If set, an EWOULDBLOCK error is not treated as an error but
   simply causes the write function to return 0 as the number
   of bytes written out. */
#define LSTR_BLOCKED_OK 8

Lisp_Object make_stdio_input_stream (FILE *stream, int flags);
Lisp_Object make_stdio_output_stream (FILE *stream, int flags);
Lisp_Object make_filedesc_input_stream (int filedesc, int offset, int count,
					int flags);
Lisp_Object make_filedesc_output_stream (int filedesc, int offset, int count,
					 int flags);
void filedesc_stream_set_pty_flushing (Lstream *stream,
				       int pty_max_bytes,
				       Bufbyte eof_char);
int filedesc_stream_fd (Lstream *stream);
Lisp_Object make_lisp_string_input_stream (Lisp_Object string,
					   Bytecount offset,
					   Bytecount len);
Lisp_Object make_fixed_buffer_input_stream (const void *buf,
					    Lstream_data_count size);
Lisp_Object make_fixed_buffer_output_stream (void *buf,
					     Lstream_data_count size);
const unsigned char *fixed_buffer_input_stream_ptr (Lstream *stream);
unsigned char *fixed_buffer_output_stream_ptr (Lstream *stream);
Lisp_Object make_resizing_buffer_output_stream (void);
unsigned char *resizing_buffer_stream_ptr (Lstream *stream);
Lisp_Object make_dynarr_output_stream (unsigned_char_dynarr *dyn);
#define LSTR_SELECTIVE 1
#define LSTR_IGNORE_ACCESSIBLE 2
Lisp_Object make_lisp_buffer_input_stream (struct buffer *buf, Bufpos start,
					   Bufpos end, int flags);
Lisp_Object make_lisp_buffer_output_stream (struct buffer *buf, Bufpos pos,
					    int flags);
Bufpos lisp_buffer_stream_startpos (Lstream *stream);

#endif /* INCLUDED_lstream_h_ */
