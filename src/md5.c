/* md5.c - Functions to compute MD5 message digest of files or memory blocks
   according to the definition of MD5 in RFC 1321 from April 1992.
   Copyright (C) 1995, 1996 Free Software Foundation, Inc.
   NOTE: The canonical source of this file is maintained with the GNU C
   Library.  Bugs can be reported to bug-glibc@prep.ai.mit.edu.

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


/* Written by Ulrich Drepper <drepper@gnu.ai.mit.edu>, 1995.  */

/* XEmacs frontend written by Ben Wing, Jareth Hein and Hrvoje Niksic.  */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <sys/types.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>

/* The following contortions are an attempt to use the C preprocessor
   to determine an unsigned integral type that is 32 bits wide.  An
   alternative approach is to use autoconf's AC_CHECK_SIZEOF macro, but
   doing that would require that the configure script compile and *run*
   the resulting executable.  Locally running cross-compiled executables
   is usually not possible.  */

#ifdef _LIBC
# include <sys/types.h>
typedef u_int32_t md5_uint32;
#else
# if defined __STDC__ && __STDC__
#  define UINT_MAX_32_BITS 4294967295U
# else
#  define UINT_MAX_32_BITS 0xFFFFFFFF
# endif

/* If UINT_MAX isn't defined, assume it's a 32-bit type.
   This should be valid for all systems GNU cares about because
   that doesn't include 16-bit systems, and only modern systems
   (that certainly have <limits.h>) have 64+-bit integral types.  */

# ifndef UINT_MAX
#  define UINT_MAX UINT_MAX_32_BITS
# endif

# if UINT_MAX == UINT_MAX_32_BITS
typedef unsigned int md5_uint32;
# else
#  if USHRT_MAX == UINT_MAX_32_BITS
typedef unsigned short md5_uint32;
#  else
#   if ULONG_MAX == UINT_MAX_32_BITS
typedef unsigned long md5_uint32;
#   else
     /* The following line is intended to evoke an error.
        Using #error is not portable enough.  */
"Cannot determine unsigned 32-bit data type."
#   endif
#  endif
# endif
#endif
#include "lisp.h"
#include "buffer.h"
#include "lstream.h"
#ifdef FILE_CODING
<<<<<<< HEAD
<<<<<<< HEAD
# include "file-coding.h"
=======
# include "mule/file-coding.h"
>>>>>>> origin/master
=======
# include "mule/file-coding.h"
>>>>>>> master
#endif
/* Structure to save state of computation between the single steps.  */
    struct md5_ctx {
	md5_uint32 A;
	md5_uint32 B;
	md5_uint32 C;
	md5_uint32 D;

	md5_uint32 total[2];
	md5_uint32 buflen;
	char buffer[128];
};

#ifdef WORDS_BIGENDIAN
# define SWAP(n)							\
    (((n) << 24) | (((n) & 0xff00) << 8) | (((n) >> 8) & 0xff00) | ((n) >> 24))
#else
# define SWAP(n) (n)
#endif

/* This array contains the bytes used to pad the buffer to the next
   64-byte boundary.  (RFC 1321, 3.1: Step 1)  */
static const unsigned char fillbuf[64] = { 0x80, 0 /* , 0, 0, ...  */  };

static void md5_process_block(const void *, size_t, struct md5_ctx *);

/* Initialize structure containing state of computation.
   (RFC 1321, 3.3: Step 3)  */
static void md5_init_ctx(struct md5_ctx *ctx)
{
	ctx->A = 0x67452301;
	ctx->B = 0xefcdab89;
	ctx->C = 0x98badcfe;
	ctx->D = 0x10325476;

	ctx->total[0] = ctx->total[1] = 0;
	ctx->buflen = 0;
}

/* Put result from CTX in first 16 bytes following RESBUF.  The result
   must be in little endian byte order.

   IMPORTANT: On some systems it is required that RESBUF is correctly
   aligned for a 32 bits value.  */
static void *md5_read_ctx(const struct md5_ctx *ctx, void *resbuf)
{
	((md5_uint32 *) resbuf)[0] = SWAP(ctx->A);
	((md5_uint32 *) resbuf)[1] = SWAP(ctx->B);
	((md5_uint32 *) resbuf)[2] = SWAP(ctx->C);
	((md5_uint32 *) resbuf)[3] = SWAP(ctx->D);

	return resbuf;
}

/* Process the remaining bytes in the internal buffer and the usual
   prolog according to the standard and write the result to RESBUF.

   IMPORTANT: On some systems it is required that RESBUF is correctly
   aligned for a 32 bits value.  */
static void *md5_finish_ctx(struct md5_ctx *ctx, void *resbuf)
{
	/* Take yet unprocessed bytes into account.  */
	md5_uint32 bytes = ctx->buflen;
	size_t pad;

	/* Now count remaining bytes.  */
	ctx->total[0] += bytes;
	if (ctx->total[0] < bytes)
		++ctx->total[1];

	pad = bytes >= 56 ? 64 + 56 - bytes : 56 - bytes;
	memcpy(&ctx->buffer[bytes], fillbuf, pad);

	/* Put the 64-bit file length in *bits* at the end of the buffer.  */
	*(md5_uint32 *) & ctx->buffer[bytes + pad] = SWAP(ctx->total[0] << 3);
	*(md5_uint32 *) & ctx->buffer[bytes + pad + 4] =
	    SWAP((ctx->total[1] << 3) | (ctx->total[0] >> 29));

	/* Process last bytes.  */
	md5_process_block(ctx->buffer, bytes + pad + 8, ctx);

	return md5_read_ctx(ctx, resbuf);
}

#ifndef emacs			/* unused in Emacs */
/* Compute MD5 message digest for bytes read from STREAM.  The
   resulting message digest number will be written into the 16 bytes
   beginning at RESBLOCK.  */
int md5_stream(FILE * stream, void *resblock)
{
	/* Important: BLOCKSIZE must be a multiple of 64.  */
#define BLOCKSIZE 4096
	struct md5_ctx ctx;
	char buffer[BLOCKSIZE + 72];
	size_t sum;

	/* Initialize the computation context.  */
	md5_init_ctx(&ctx);

	/* Iterate over full file contents.  */
	while (1) {
		/* We read the file in blocks of BLOCKSIZE bytes.  One call of the
		   computation function processes the whole buffer so that with the
		   next round of the loop another block can be read.  */
		size_t n;
		sum = 0;

		/* Read block.  Take care for partial reads.  */
		do {
			n = fread(buffer + sum, 1, BLOCKSIZE - sum, stream);

			sum += n;
		}
		while (sum < BLOCKSIZE && n != 0);
		if (n == 0 && ferror(stream))
			return 1;

		/* If end of file is reached, end the loop.  */
		if (n == 0)
			break;

		/* Process buffer with BLOCKSIZE bytes.  Note that
		   BLOCKSIZE % 64 == 0
		 */
		md5_process_block(buffer, BLOCKSIZE, &ctx);
	}

	/* Add the last bytes if necessary.  */
	if (sum > 0)
		md5_process_bytes(buffer, sum, &ctx);

	/* Construct result in desired memory.  */
	md5_finish_ctx(&ctx, resblock);
	return 0;
}

/* Compute MD5 message digest for LEN bytes beginning at BUFFER.  The
   result is always in little endian byte order, so that a byte-wise
   output yields to the wanted ASCII representation of the message
   digest.  */
void *md5_buffer(const char *buffer, size_t len, void *resblock)
{
	struct md5_ctx ctx;

	/* Initialize the computation context.  */
	md5_init_ctx(&ctx);

	/* Process whole buffer but last len % 64 bytes.  */
	md5_process_bytes(buffer, len, &ctx);

	/* Put result in desired memory area.  */
	return md5_finish_ctx(&ctx, resblock);
}
#endif				/* not emacs */

static void
md5_process_bytes(const void *buffer, size_t len, struct md5_ctx *ctx)
{
	/* When we already have some bits in our internal buffer concatenate
	   both inputs first.  */
	if (ctx->buflen != 0) {
		size_t left_over = ctx->buflen;
		size_t add = 128 - left_over > len ? len : 128 - left_over;

		memcpy(&ctx->buffer[left_over], buffer, add);
		ctx->buflen += add;

		if (left_over + add > 64) {
			md5_process_block(ctx->buffer, (left_over + add) & ~63,
					  ctx);
			/* The regions in the following copy operation cannot overlap.  */
			memcpy(ctx->buffer,
			       &ctx->buffer[(left_over + add) & ~63],
			       (left_over + add) & 63);
			ctx->buflen = (left_over + add) & 63;
		}

		buffer = (const char *)buffer + add;
		len -= add;
	}

	/* Process available complete blocks.  */
	if (len > 64) {
		md5_process_block(buffer, len & ~63, ctx);
		buffer = (const char *)buffer + (len & ~63);
		len &= 63;
	}

	/* Move remaining bytes in internal buffer.  */
	if (len > 0) {
		memcpy(ctx->buffer, buffer, len);
		ctx->buflen = len;
	}
}

/* These are the four functions used in the four steps of the MD5 algorithm
   and defined in the RFC 1321.  The first function is a little bit optimized
   (as found in Colin Plumbs public domain implementation).  */
/* #define FF(b, c, d) ((b & c) | (~b & d)) */
#define FF(b, c, d) (d ^ (b & (c ^ d)))
#define FG(b, c, d) FF (d, b, c)
#define FH(b, c, d) (b ^ c ^ d)
#define FI(b, c, d) (c ^ (b | ~d))

/* Process LEN bytes of BUFFER, accumulating context into CTX.
   It is assumed that LEN % 64 == 0.  */

static void
md5_process_block(const void *buffer, size_t len, struct md5_ctx *ctx)
{
	md5_uint32 correct_words[16];
	const md5_uint32 *words = (const md5_uint32 *)buffer;
	size_t nwords = len / sizeof(md5_uint32);
	const md5_uint32 *endp = words + nwords;
	md5_uint32 A = ctx->A;
	md5_uint32 B = ctx->B;
	md5_uint32 C = ctx->C;
	md5_uint32 D = ctx->D;

	/* First increment the byte count.  RFC 1321 specifies the possible
	   length of the file up to 2^64 bits.  Here we only compute the
	   number of bytes.  Do a double word increment.  */
	ctx->total[0] += len;
	if (ctx->total[0] < len)
		++ctx->total[1];

	/* Process all bytes in the buffer with 64 bytes in each round of
	   the loop.  */
	while (words < endp) {
		md5_uint32 *cwp = correct_words;
		md5_uint32 A_save = A;
		md5_uint32 B_save = B;
		md5_uint32 C_save = C;
		md5_uint32 D_save = D;

		/* First round: using the given function, the context and a constant
		   the next context is computed.  Because the algorithms processing
		   unit is a 32-bit word and it is determined to work on words in
		   little endian byte order we perhaps have to change the byte order
		   before the computation.  To reduce the work for the next steps
		   we store the swapped words in the array CORRECT_WORDS.  */

#define OP(a, b, c, d, s, T)						\
      do								\
        {								\
	  a += FF (b, c, d) + (*cwp++ = SWAP (*words)) + T;		\
	  ++words;							\
	  CYCLIC (a, s);						\
	  a += b;							\
        }								\
      while (0)

		/* It is unfortunate that C does not provide an operator for
		   cyclic rotation.  Hope the C compiler is smart enough.  */
#define CYCLIC(w, s) (w = (w << s) | (w >> (32 - s)))

		/* Before we start, one word to the strange constants.
		   They are defined in RFC 1321 as

		   T[i] = (int) (4294967296.0 * fabs (sin (i))), i=1..64
		 */

		/* Round 1.  */
		OP(A, B, C, D, 7, 0xd76aa478);
		OP(D, A, B, C, 12, 0xe8c7b756);
		OP(C, D, A, B, 17, 0x242070db);
		OP(B, C, D, A, 22, 0xc1bdceee);
		OP(A, B, C, D, 7, 0xf57c0faf);
		OP(D, A, B, C, 12, 0x4787c62a);
		OP(C, D, A, B, 17, 0xa8304613);
		OP(B, C, D, A, 22, 0xfd469501);
		OP(A, B, C, D, 7, 0x698098d8);
		OP(D, A, B, C, 12, 0x8b44f7af);
		OP(C, D, A, B, 17, 0xffff5bb1);
		OP(B, C, D, A, 22, 0x895cd7be);
		OP(A, B, C, D, 7, 0x6b901122);
		OP(D, A, B, C, 12, 0xfd987193);
		OP(C, D, A, B, 17, 0xa679438e);
		OP(B, C, D, A, 22, 0x49b40821);

		/* For the second to fourth round we have the possibly swapped words
		   in CORRECT_WORDS.  Redefine the macro to take an additional first
		   argument specifying the function to use.  */
#undef OP
#define OP(f, a, b, c, d, k, s, T)					\
      do 								\
	{								\
	  a += f (b, c, d) + correct_words[k] + T;			\
	  CYCLIC (a, s);						\
	  a += b;							\
	}								\
      while (0)

		/* Round 2.  */
		OP(FG, A, B, C, D, 1, 5, 0xf61e2562);
		OP(FG, D, A, B, C, 6, 9, 0xc040b340);
		OP(FG, C, D, A, B, 11, 14, 0x265e5a51);
		OP(FG, B, C, D, A, 0, 20, 0xe9b6c7aa);
		OP(FG, A, B, C, D, 5, 5, 0xd62f105d);
		OP(FG, D, A, B, C, 10, 9, 0x02441453);
		OP(FG, C, D, A, B, 15, 14, 0xd8a1e681);
		OP(FG, B, C, D, A, 4, 20, 0xe7d3fbc8);
		OP(FG, A, B, C, D, 9, 5, 0x21e1cde6);
		OP(FG, D, A, B, C, 14, 9, 0xc33707d6);
		OP(FG, C, D, A, B, 3, 14, 0xf4d50d87);
		OP(FG, B, C, D, A, 8, 20, 0x455a14ed);
		OP(FG, A, B, C, D, 13, 5, 0xa9e3e905);
		OP(FG, D, A, B, C, 2, 9, 0xfcefa3f8);
		OP(FG, C, D, A, B, 7, 14, 0x676f02d9);
		OP(FG, B, C, D, A, 12, 20, 0x8d2a4c8a);

		/* Round 3.  */
		OP(FH, A, B, C, D, 5, 4, 0xfffa3942);
		OP(FH, D, A, B, C, 8, 11, 0x8771f681);
		OP(FH, C, D, A, B, 11, 16, 0x6d9d6122);
		OP(FH, B, C, D, A, 14, 23, 0xfde5380c);
		OP(FH, A, B, C, D, 1, 4, 0xa4beea44);
		OP(FH, D, A, B, C, 4, 11, 0x4bdecfa9);
		OP(FH, C, D, A, B, 7, 16, 0xf6bb4b60);
		OP(FH, B, C, D, A, 10, 23, 0xbebfbc70);
		OP(FH, A, B, C, D, 13, 4, 0x289b7ec6);
		OP(FH, D, A, B, C, 0, 11, 0xeaa127fa);
		OP(FH, C, D, A, B, 3, 16, 0xd4ef3085);
		OP(FH, B, C, D, A, 6, 23, 0x04881d05);
		OP(FH, A, B, C, D, 9, 4, 0xd9d4d039);
		OP(FH, D, A, B, C, 12, 11, 0xe6db99e5);
		OP(FH, C, D, A, B, 15, 16, 0x1fa27cf8);
		OP(FH, B, C, D, A, 2, 23, 0xc4ac5665);

		/* Round 4.  */
		OP(FI, A, B, C, D, 0, 6, 0xf4292244);
		OP(FI, D, A, B, C, 7, 10, 0x432aff97);
		OP(FI, C, D, A, B, 14, 15, 0xab9423a7);
		OP(FI, B, C, D, A, 5, 21, 0xfc93a039);
		OP(FI, A, B, C, D, 12, 6, 0x655b59c3);
		OP(FI, D, A, B, C, 3, 10, 0x8f0ccc92);
		OP(FI, C, D, A, B, 10, 15, 0xffeff47d);
		OP(FI, B, C, D, A, 1, 21, 0x85845dd1);
		OP(FI, A, B, C, D, 8, 6, 0x6fa87e4f);
		OP(FI, D, A, B, C, 15, 10, 0xfe2ce6e0);
		OP(FI, C, D, A, B, 6, 15, 0xa3014314);
		OP(FI, B, C, D, A, 13, 21, 0x4e0811a1);
		OP(FI, A, B, C, D, 4, 6, 0xf7537e82);
		OP(FI, D, A, B, C, 11, 10, 0xbd3af235);
		OP(FI, C, D, A, B, 2, 15, 0x2ad7d2bb);
		OP(FI, B, C, D, A, 9, 21, 0xeb86d391);

		/* Add the starting values of the context.  */
		A += A_save;
		B += B_save;
		C += C_save;
		D += D_save;
	}

	/* Put checksum in context given as argument.  */
	ctx->A = A;
	ctx->B = B;
	ctx->C = C;
	ctx->D = D;
}

#ifdef emacs
#ifdef FILE_CODING
/* Find out what format the buffer will be saved in, so we can make
   the digest based on what it will look like on disk.  */
static Lisp_Object
md5_coding_system(Lisp_Object object, Lisp_Object coding, Lisp_Object istream,
		  int error_me_not)
{
	Lisp_Object coding_system;

	if (NILP(coding)) {
		if (BUFFERP(object)) {
			/* Use the file coding for this buffer by default.  */
			coding_system =
			    XBUFFER(object)->buffer_file_coding_system;
		} else {
			/* Attempt to autodetect the coding of the string.  This is
			   VERY hit-and-miss.  */
			eol_type_t eol = EOL_AUTODETECT;
			coding_system = Fget_coding_system(Qundecided);
			determine_real_coding_system(XLSTREAM(istream),
						     &coding_system, &eol);
		}
		if (NILP(coding_system))
			coding_system = Fget_coding_system(Qbinary);
		else {
			coding_system = Ffind_coding_system(coding_system);
			if (NILP(coding_system))
				coding_system = Fget_coding_system(Qbinary);
		}
	} else {
		coding_system = Ffind_coding_system(coding);
		if (NILP(coding_system)) {
			if (error_me_not)
				/* Default to binary.  */
				coding_system = Fget_coding_system(Qbinary);
			else
				signal_simple_error("No such coding system",
						    coding);
		}
	}
	return coding_system;
}
#endif				/* FILE_CODING */

DEFUN("md5", Fmd5, 1, 5, 0,	/*
Return the MD5 message digest of OBJECT, a buffer or string.

Optional arguments START and END denote positions for computing the
digest of a portion of OBJECT.

The optional CODING argument specifies the coding system the text is to be
represented in while computing the digest.  If unspecified, it defaults
to the current format of the data, or is guessed.

If NOERROR is non-nil, silently assume binary coding if the guesswork
fails.  Normally, an error is signaled in such case.

CODING and NOERROR arguments are meaningful only in XEmacsen with
file-coding or Mule support.  Otherwise, they are ignored.
<<<<<<< HEAD
<<<<<<< HEAD
				 */
=======
*/
>>>>>>> origin/master
=======
*/
>>>>>>> master
      (object, start, end, coding, noerror))
{
	/* This function can GC */
	/* Can this really GC?  How?  */
	struct md5_ctx ctx;
	unsigned char digest[16];
	unsigned char thehash[33];
	int i;

	Lisp_Object instream;
	struct gcpro gcpro1;
#ifdef FILE_CODING
	Lisp_Object raw_instream;
	struct gcpro ngcpro1;
#endif

	/* Set up the input stream.  */
	if (BUFFERP(object)) {
		struct buffer *b;
		Bufpos begv, endv;
		CHECK_LIVE_BUFFER(object);
		b = XBUFFER(object);
		/* Figure out where we need to get info from */
		get_buffer_range_char(b, start, end, &begv, &endv,
				      GB_ALLOW_NIL);

		instream = make_lisp_buffer_input_stream(b, begv, endv, 0);
	} else {
		Bytecount bstart, bend;
		CHECK_STRING(object);
		get_string_range_byte(object, start, end, &bstart, &bend,
				      GB_HISTORICAL_STRING_BEHAVIOR);
		instream =
		    make_lisp_string_input_stream(object, bstart,
						  bend - bstart);
	}
	GCPRO1(instream);

#ifdef FILE_CODING
	/* Determine the coding and set up the conversion stream.  */
	coding = md5_coding_system(object, coding, instream, !NILP(noerror));
	raw_instream = instream;
	instream = make_encoding_input_stream(XLSTREAM(instream), coding);
	NGCPRO1(raw_instream);
#endif

	/* Initialize MD5 context.  */
	md5_init_ctx(&ctx);

	/* Get the data while doing the conversion.  */
	while (1) {
		Bufbyte tempbuf[1024];	/* some random amount */
		Lstream_data_count size_in_bytes =
		    Lstream_read(XLSTREAM(instream), tempbuf, sizeof(tempbuf));
		if (size_in_bytes<=0)
			break;

		/* Process the bytes.  */
		md5_process_bytes(tempbuf, size_in_bytes, &ctx);
	}
	Lstream_delete(XLSTREAM(instream));
#ifdef FILE_CODING
	Lstream_delete(XLSTREAM(raw_instream));
	NUNGCPRO;
#endif
	UNGCPRO;

	md5_finish_ctx(&ctx, digest);
<<<<<<< HEAD
	for (i = 0; i < 16; i++)
		sprintf((char *)(thehash + (i * 2)), "%02x", digest[i]);
=======
	for (i = 0; i < 16; i++) {
		int n = snprintf((char *)(thehash + (i * 2)), 3, "%02x", digest[i]);
		assert(n>=0 && n < 3);
	}
>>>>>>> master

	return make_string(thehash, 32);
}

void syms_of_md5(void)
{
	DEFSUBR(Fmd5);
}

void vars_of_md5(void)
{
	Fprovide(intern("md5"));
}
#endif				/* emacs */
