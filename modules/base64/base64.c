/* base64 interface for XEmacs.
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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

/* Author: William Perry <wmperry@aventail.com> */

#include <emodules.h>

unsigned char alphabet[64] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

DEFUN ("base64-encode", Fbase64_encode, 1, 5, 0, /*
Return the base64 encoding of an object.
OBJECT is either a string or a buffer.
Optional arguments START and END denote buffer positions for computing the
hash of a portion of OBJECT.  The optional CODING argument specifies the coding
system the text is to be represented in while computing the digest.  This only
has meaning with MULE, and defaults to the current format of the data.
If ERROR-ME-NOT is nil, report an error if the coding system can't be
determined.  Else assume binary coding if all else fails.
*/
       (object, start, end, coding, error_me_not))
{
	int cols,bits,char_count;
	Lisp_Object instream, outstream,deststream;
	Lstream *istr, *ostr, *dstr;
	static Extbyte_dynarr *conversion_out_dynarr;
	static Extbyte_dynarr *out_dynarr;
	char tempbuf[1024]; /* some random amount */
	struct gcpro gcpro1, gcpro2;
#ifdef FILE_CODING
	Lisp_Object conv_out_stream, coding_system;
	Lstream *costr;
	struct gcpro gcpro3;
#endif

	if (!conversion_out_dynarr)
		conversion_out_dynarr = Dynarr_new (Extbyte);
	else
		Dynarr_reset (conversion_out_dynarr);

	if (!out_dynarr)
		out_dynarr = Dynarr_new(Extbyte);
	else
		Dynarr_reset (out_dynarr);

	char_count = bits = cols = 0;

	/* set up the in stream */
	if (BUFFERP (object))
	{
		struct buffer *b = XBUFFER (object);
		Bufpos begv, endv;
		/* Figure out where we need to get info from */
		get_buffer_range_char (b, start, end, &begv, &endv, GB_ALLOW_NIL);

		instream = make_lisp_buffer_input_stream (b, begv, endv, 0);
	}
	else
	{
		Bytecount bstart, bend;
		CHECK_STRING (object);
		get_string_range_byte (object, start, end, &bstart, &bend,
							   GB_HISTORICAL_STRING_BEHAVIOR);
		instream = make_lisp_string_input_stream (object, bstart, bend);
	}
	istr = XLSTREAM (instream);

#ifdef FILE_CODING
	/* Find out what format the buffer will be saved in, so we can make
	   the digest based on what it will look like on disk */
	if (NILP(coding))
	{
		if (BUFFERP(object)) 
	    {
			/* Use the file coding for this buffer by default */
			coding_system = XBUFFER(object)->buffer_file_coding_system;
	    }
		else
	    {
			/* attempt to autodetect the coding of the string.  Note: this VERY hit-and-miss */
			enum eol_type eol = EOL_AUTODETECT;
			coding_system = Fget_coding_system(Qundecided);
			determine_real_coding_system(istr, &coding_system, &eol);
	    }
		if (NILP(coding_system)) 
			coding_system = Fget_coding_system(Qbinary);
		else
	    {
			coding_system = Ffind_coding_system (coding_system);
			if (NILP(coding_system))
				coding_system = Fget_coding_system(Qbinary);
	    }
	}
	else
	{
		coding_system = Ffind_coding_system (coding);
		if (NILP(coding_system))
	    {
			if (NILP(error_me_not))
				signal_simple_error("No such coding system", coding);
			else
				coding_system = Fget_coding_system(Qbinary); /* default to binary */
	    }
	}
#endif

	/* setup the out stream */
	outstream = make_dynarr_output_stream((unsigned_char_dynarr *)conversion_out_dynarr);
	ostr = XLSTREAM (outstream);
	deststream = make_dynarr_output_stream((unsigned_char_dynarr *)out_dynarr);
	dstr = XLSTREAM (deststream);
#ifdef FILE_CODING
	/* setup the conversion stream */
	conv_out_stream = make_encoding_output_stream (ostr, coding_system);
	costr = XLSTREAM (conv_out_stream);
	GCPRO3 (instream, outstream, conv_out_stream);
#else
	GCPRO2 (instream, outstream);
#endif

	/* Get the data while doing the conversion */
	while (1) {
		int size_in_bytes = Lstream_read (istr, tempbuf, sizeof (tempbuf));
		int l;
		if (!size_in_bytes)
			break;
		/* It does seem the flushes are necessary... */
#ifdef FILE_CODING
		Lstream_write (costr, tempbuf, size_in_bytes);
		Lstream_flush (costr);
#else
		Lstream_write (ostr, tempbuf, size_in_bytes);
#endif
		Lstream_flush (ostr);

		/* Update the base64 output buffer */
		for (l = 0; l < size_in_bytes; l++) {
			bits += Dynarr_at(conversion_out_dynarr,l);
			char_count++;
			if (char_count == 3) {
				static char obuf[4];
				obuf[0] = alphabet[(bits >> 18)];
				obuf[1] = alphabet[(bits >> 12) & 0x3f];
				obuf[2] = alphabet[(bits >>  6) & 0x3f];
				obuf[3] = alphabet[bits & 0x3f];

				Lstream_write(dstr,obuf,sizeof(obuf));
				cols += 4;
				if (cols == 72) {
					Lstream_write(dstr,"\n",sizeof(unsigned char));
					cols = 0;
				}
				bits = char_count = 0;
			} else {
				bits <<= 8;
			}
		}
		/* reset the dynarr */
		Lstream_rewind(ostr);
	}
	Lstream_close (istr);
#ifdef FILE_CODING
	Lstream_close (costr);
#endif
	Lstream_close (ostr);

	if (char_count != 0) {
		bits <<= 16 - (8 * char_count);
		Lstream_write(dstr,&alphabet[bits >> 18],sizeof(unsigned char));
		Lstream_write(dstr,&alphabet[(bits >> 12) & 0x3f],sizeof(unsigned char));
		if (char_count == 1) {
			Lstream_write(dstr,"==",2 * sizeof(unsigned char));
		} else {
			Lstream_write(dstr,&alphabet[(bits >> 6) & 0x3f],sizeof(unsigned char));
			Lstream_write(dstr,"=",sizeof(unsigned char));
		}
	}
#if 0
	if (cols > 0) {
		Lstream_write(dstr,"\n",sizeof(unsigned char));
	}
#endif
	UNGCPRO;
	Lstream_delete (istr);
	Lstream_delete (ostr);
#ifdef FILE_CODING
	Lstream_delete (costr);
#endif
	Lstream_flush(dstr);
	Lstream_delete(dstr);

	return(make_string(Dynarr_atp(out_dynarr,0),Dynarr_length(out_dynarr)));
}

DEFUN ("base64-decode", Fbase64_decode, 1, 5, 0, /*
Undo the base64 encoding of an object.
OBJECT is either a string or a buffer.
Optional arguments START and END denote buffer positions for computing the
hash of a portion of OBJECT.  The optional CODING argument specifies the coding
system the text is to be represented in while computing the digest.  This only
has meaning with MULE, and defaults to the current format of the data.
If ERROR-ME-NOT is nil, report an error if the coding system can't be
determined.  Else assume binary coding if all else fails.
*/
       (object, start, end, coding, error_me_not))
{
    static char inalphabet[256], decoder[256];
	int i,cols,bits,char_count,hit_eof;
	Lisp_Object instream, outstream,deststream;
	Lstream *istr, *ostr, *dstr;
	static Extbyte_dynarr *conversion_out_dynarr;
	static Extbyte_dynarr *out_dynarr;
	char tempbuf[1024]; /* some random amount */
	struct gcpro gcpro1, gcpro2;
#ifdef FILE_CODING
	Lisp_Object conv_out_stream, coding_system;
	Lstream *costr;
	struct gcpro gcpro3;
#endif

    for (i = (sizeof alphabet) - 1; i >= 0 ; i--) {
		inalphabet[alphabet[i]] = 1;
		decoder[alphabet[i]] = i;
    }

	if (!conversion_out_dynarr)
		conversion_out_dynarr = Dynarr_new (Extbyte);
	else
		Dynarr_reset (conversion_out_dynarr);

	if (!out_dynarr)
		out_dynarr = Dynarr_new(Extbyte);
	else
		Dynarr_reset (out_dynarr);

	char_count = bits = cols = hit_eof = 0;

	/* set up the in stream */
	if (BUFFERP (object))
	{
		struct buffer *b = XBUFFER (object);
		Bufpos begv, endv;
		/* Figure out where we need to get info from */
		get_buffer_range_char (b, start, end, &begv, &endv, GB_ALLOW_NIL);

		instream = make_lisp_buffer_input_stream (b, begv, endv, 0);
	}
	else
	{
		Bytecount bstart, bend;
		CHECK_STRING (object);
		get_string_range_byte (object, start, end, &bstart, &bend,
							   GB_HISTORICAL_STRING_BEHAVIOR);
		instream = make_lisp_string_input_stream (object, bstart, bend);
	}
	istr = XLSTREAM (instream);

#ifdef FILE_CODING
	/* Find out what format the buffer will be saved in, so we can make
	   the digest based on what it will look like on disk */
	if (NILP(coding))
	{
		if (BUFFERP(object)) 
	    {
			/* Use the file coding for this buffer by default */
			coding_system = XBUFFER(object)->buffer_file_coding_system;
	    }
		else
	    {
			/* attempt to autodetect the coding of the string.  Note: this VERY hit-and-miss */
			enum eol_type eol = EOL_AUTODETECT;
			coding_system = Fget_coding_system(Qundecided);
			determine_real_coding_system(istr, &coding_system, &eol);
	    }
		if (NILP(coding_system)) 
			coding_system = Fget_coding_system(Qbinary);
		else
	    {
			coding_system = Ffind_coding_system (coding_system);
			if (NILP(coding_system))
				coding_system = Fget_coding_system(Qbinary);
	    }
	}
	else
	{
		coding_system = Ffind_coding_system (coding);
		if (NILP(coding_system))
	    {
			if (NILP(error_me_not))
				signal_simple_error("No such coding system", coding);
			else
				coding_system = Fget_coding_system(Qbinary); /* default to binary */
	    }
	}
#endif

	/* setup the out stream */
	outstream = make_dynarr_output_stream((unsigned_char_dynarr *)conversion_out_dynarr);
	ostr = XLSTREAM (outstream);
	deststream = make_dynarr_output_stream((unsigned_char_dynarr *)out_dynarr);
	dstr = XLSTREAM (deststream);
#ifdef FILE_CODING
	/* setup the conversion stream */
	conv_out_stream = make_encoding_output_stream (ostr, coding_system);
	costr = XLSTREAM (conv_out_stream);
	GCPRO3 (instream, outstream, conv_out_stream);
#else
	GCPRO2 (instream, outstream);
#endif

	/* Get the data while doing the conversion */
	while (1) {
		int size_in_bytes = Lstream_read (istr, tempbuf, sizeof (tempbuf));
		int l;
		if (!size_in_bytes) {
			hit_eof = 1;
			break;
		}
		/* It does seem the flushes are necessary... */
#ifdef FILE_CODING
		Lstream_write (costr, tempbuf, size_in_bytes);
		Lstream_flush (costr);
#else
		Lstream_write (ostr, tempbuf, size_in_bytes);
#endif
		Lstream_flush (ostr);

		/* Update the base64 output buffer */
		for (l = 0; l < size_in_bytes; l++) {
			if (Dynarr_at(conversion_out_dynarr,l) == '=')
				goto decoder_out;
			bits += decoder[Dynarr_at(conversion_out_dynarr,l)];
			fprintf(stderr,"%d\n",bits);
			char_count++;
			if (char_count == 4) {
				static unsigned char obuf[3];
				obuf[0] = (bits >> 16);
				obuf[1] = (bits >> 8) & 0xff;
				obuf[2] = (bits & 0xff);

				Lstream_write(dstr,obuf,sizeof(obuf));
				bits = char_count = 0;
			} else {
				bits <<= 6;
			}
		}
		/* reset the dynarr */
		Lstream_rewind(ostr);
	}
 decoder_out:
	Lstream_close (istr);
#ifdef FILE_CODING
	Lstream_close (costr);
#endif
	Lstream_close (ostr);

	if (hit_eof) {
		if (char_count) {
			error_with_frob(object,"base64-decode failed: at least %d bits truncated",((4 - char_count) * 6));
		}
	}
	switch(char_count) {
	case 1:
		error_with_frob(object, "base64 encoding incomplete: at least 2 bits missing");
		break;
	case 2:
		char_count = bits >> 10;
		Lstream_write(dstr,&char_count,sizeof(char_count));
		break;
	case 3:
	{
		unsigned char buf[2];
		buf[0] = (bits >> 16);
		buf[1] = (bits >> 8) & 0xff;
		Lstream_write(dstr,buf,sizeof(buf));
		break;
	}
	}

	UNGCPRO;
	Lstream_delete (istr);
	Lstream_delete (ostr);
#ifdef FILE_CODING
	Lstream_delete (costr);
#endif
	Lstream_flush(dstr);
	Lstream_delete(dstr);

	return(make_string(Dynarr_atp(out_dynarr,0),Dynarr_length(out_dynarr)));
}

void
syms_of_base64 (void)
{
  DEFSUBR(Fbase64_encode);
  DEFSUBR(Fbase64_decode);
}

void
vars_of_base64 (void)
{
  Fprovide (intern ("base64"));
}
