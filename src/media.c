/* Media functions.
   Copyright (C) 2006 Sebastian Freundt

This file is part of SXEmacs.

SXEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

SXEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with SXEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Not in FSF. */

#include <config.h>
#include "lisp.h"

#include "buffer.h"

#include "media.h"
#ifdef HAVE_FFMPEG
#include "media-ffmpeg.h"
#endif
#ifdef HAVE_SNDFILE
#include "media-sndfile.h"
#endif
#ifdef HAVE_NATIVE_MEDIA
#include "media-native.h"
#endif
#ifdef HAVE_MAD
#include "media-mad.h"
#endif
#ifdef HAVE_GSTREAMER
#include "media-gstreamer.h"
#endif
#ifdef HAVE_SOX
#include "media-sox.h"
#endif
#ifdef HAVE_XINE
#include "media-xine.h"
#endif

Lisp_Object Qmedia_streamp;

static void determine_stream_type(Lisp_Media_Stream *ms);
static void media_stream_print(Lisp_Object, Lisp_Object, int);
static void media_substream_print(media_substream *, Lisp_Object, int);


static void determine_stream_type(Lisp_Media_Stream *ms)
{
#ifdef HAVE_FFMPEG
	if (media_stream_driver(ms) == MDRIVER_UNDECIDED)
		media_ffmpeg_analyse_stream(ms);
#endif
#ifdef HAVE_MAD
	if (media_stream_driver(ms) == MDRIVER_UNDECIDED)
		media_mad_analyse_stream(ms);
#endif
#ifdef HAVE_SOX
	if (media_stream_driver(ms) == MDRIVER_UNDECIDED)
		media_sox_analyse_stream(ms);
#endif
#ifdef HAVE_SNDFILE
	if (media_stream_driver(ms) == MDRIVER_UNDECIDED)
		media_sndfile_analyse_stream(ms);
#endif
/* not working stuff here :) */
#if 0
#ifdef HAVE_GSTREAMER
	if (media_stream_driver(ms) == MDRIVER_UNDECIDED)
		media_gstreamer_analyse_stream(ms);
#endif
#endif
#ifdef HAVE_XINE
	if (media_stream_driver(ms) == MDRIVER_UNDECIDED)
		media_xine_analyse_stream(ms);
#endif
#ifdef HAVE_NATIVE_MEDIA
	if (media_stream_driver(ms) == MDRIVER_UNDECIDED)
		media_native_analyse_stream(ms);
#endif
	return;
}


/*****************************************************************/
/* 			    media streams			 */
/*****************************************************************/
static Lisp_Object media_stream_mark(Lisp_Object obj)
{
	return Qnil;
}

static void
media_stream_finalise(void *header, int for_disksave)
{
	Lisp_Media_Stream *ms = (Lisp_Media_Stream*)header;

	switch (media_stream_driver(ms)) {
	case MDRIVER_FFMPEG:
#ifdef HAVE_FFMPEG
		if (media_stream_data(ms))
			media_ffmpeg_close_context(media_stream_data(ms));
#endif
		break;
	case MDRIVER_XINE:
#ifdef HAVE_XINE
		if (media_stream_data(ms))
			media_xine_close_context(media_stream_data(ms));
#endif
		break;
	case MDRIVER_SNDFILE:
#ifdef HAVE_SNDFILE
		if (media_stream_data(ms))
			sf_close(media_stream_data(ms));
#endif
		break;
	case MDRIVER_MAD:
#ifdef HAVE_MAD
		if (media_stream_data(ms))
			mad_decoder_finish(media_stream_data(ms));
#endif
		break;
	case MDRIVER_SOX:
#ifdef HAVE_SOX
		if (media_stream_data(ms))
			st_close(media_stream_data(ms));
#endif
		break;
	default:
		break;
	}

	switch (media_stream_kind(ms)) {
	case MKIND_FILE: {
		mkind_file_properties *mkfp;
		mkfp = media_stream_kind_properties(ms).fprops;
		if (mkfp) {
			if (mkfp->filename)
				xfree(mkfp->filename);
			xfree(mkfp);
		}
		break;
	}
	case MKIND_STRING: {
		mkind_string_properties *mksp;
		mksp = media_stream_kind_properties(ms).sprops;
		if (mksp) {
			if (mksp->name)
				xfree(mksp->name);
#if 0
			if (mksp->stream_data)
				xfree(mksp->stream_data);
#endif
			xfree(mksp);
		}
		break;
	}
	default:
		break;
	}

	/* avoid some warning */
	if (for_disksave || ms == NULL);
}

static void
media_stream_print(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Lisp_Media_Stream *ms = XMEDIA_STREAM(obj);
	media_substream *mss;
	char *streaminfo = NULL;

	write_c_string("#<media-stream", printcharfun);

	write_c_string(" :kind ", printcharfun);

	switch (media_stream_kind(ms)) {
	case MKIND_FILE: {
		char *filename = media_stream_kind_properties(ms).fprops->\
			filename;

		write_c_string("#<file \"", printcharfun);
		if (filename)
			write_c_string(filename, printcharfun);
		else
			write_c_string("???", printcharfun);
		write_c_string("\">", printcharfun);
		break;
	}
	case MKIND_STRING:
		write_c_string("#<string>", printcharfun);
		break;
	case MKIND_FIFO:
		write_c_string("#<fifo>", printcharfun);
		break;
	case MKIND_STREAM:
		write_c_string("#<stream>", printcharfun);
		break;
	default:
	case MKIND_UNDECIDED:
		write_c_string("#<unknown>", printcharfun);
		break;
	}

	mss = media_stream_first(ms);
	while (mss) {
		write_c_string(" ", printcharfun);
		media_substream_print(mss, printcharfun, escapeflag);
		mss = media_substream_next(mss);
	}

	write_c_string(" driven by ", printcharfun);
	switch (media_stream_driver(ms)) {
	case MDRIVER_NATIVE:
		write_c_string("native", printcharfun);
		break;
	case MDRIVER_FFMPEG:
		write_c_string("ffmpeg", printcharfun);
#ifdef HAVE_FFMPEG
		streaminfo = media_ffmpeg_streaminfo(ms);
#endif
		break;
	case MDRIVER_SNDFILE:
		write_c_string("sndfile", printcharfun);
		break;
	case MDRIVER_MAD:
		write_c_string("mad", printcharfun);
		break;
	case MDRIVER_SOX:
		write_c_string("sox", printcharfun);
		break;
	case MDRIVER_XINE:
		write_c_string("xine", printcharfun);
		break;
	case MDRIVER_GSTREAMER:
		write_c_string("gstreamer", printcharfun);
		break;
	default:
		write_c_string("unknown", printcharfun);
		break;
	}

	if (streaminfo) {
		write_c_string(streaminfo, printcharfun);
		xfree(streaminfo);
	}

	write_c_string(">", printcharfun);
}

static int
media_stream_equal(Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	if (XMEDIA_STREAM_DATA(obj1) == XMEDIA_STREAM_DATA(obj2))
		return Qt;
	else
		return Qnil;

	/* less warnings */
	if (depth);
}

static unsigned long
media_stream_hash (Lisp_Object obj, int depth)
{
	return (unsigned long)obj;

	/* less warnings */
	if (depth);
}

static const struct lrecord_description media_stream_description[] = {
        { XD_LISP_OBJECT, offsetof(Lisp_Media_Stream, first) },
        { XD_LISP_OBJECT, offsetof(Lisp_Media_Stream, last) },
	{ XD_INT, offsetof(Lisp_Media_Stream, kind) },
	{ XD_INT, offsetof(Lisp_Media_Stream, driver) },
        { XD_OPAQUE_PTR, offsetof(Lisp_Media_Stream, kind_properties) },
        { XD_OPAQUE_PTR, offsetof(Lisp_Media_Stream, stream_data) },
	{ XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION("media_stream", media_stream,
			      media_stream_mark, media_stream_print,
			      media_stream_finalise,
			      media_stream_equal, media_stream_hash,
			      media_stream_description,
			      Lisp_Media_Stream);


/*****************************************************************/
/* 			    media substreams			 */
/*****************************************************************/

static void
media_substream_finalise(void *header, int for_disksave)
{
	media_substream *mss = (media_substream*)header;

	switch (media_substream_type(mss)) {
	case MTYPE_AUDIO:
		if (media_substream_type_properties(mss).aprops)
			xfree(media_substream_type_properties(mss).aprops);
		break;
	case MTYPE_VIDEO:
		if (media_substream_type_properties(mss).vprops)
			xfree(media_substream_type_properties(mss).vprops);
		break;
	default:
		break;
	}

#ifdef HAVE_THREADS
	pthread_mutex_destroy(&mss->substream_mutex);
#endif

	media_substream_data(mss) = NULL;

	/* avoid some warning */
	if (for_disksave);
}

static void
media_substream_print_audio(media_substream *mss, Lisp_Object printcharfun)
{
	mtype_audio_properties *mtap =
		media_substream_type_properties(mss).aprops;

	write_c_string("#<audio ", printcharfun);
	if (mtap->name || mtap->codec_name) {
		char *buf = alloca(64);
		if (mtap->name && mtap->codec_name)
			snprintf(buf, 63, "%s (%s)",
				 mtap->name, mtap->codec_name);
		else if (mtap->name)
			snprintf(buf, 63, "%s [???]", mtap->name);
		else if (mtap->codec_name)
			snprintf(buf, 63, "??? (%s)", mtap->codec_name);

		write_c_string(buf, printcharfun);
	} else
		write_c_string("???", printcharfun);

	switch (mtap->channels) {
	case 1:
		write_c_string(", mono", printcharfun);
		break;
	case 2:
		write_c_string(", stereo", printcharfun);
		break;
	case 5:
		write_c_string(", chn:5", printcharfun);
		break;
	case 6:
		write_c_string(", 5.1", printcharfun);
		break;
	default:
		write_c_string(", chn:???", printcharfun);
		break;
	}

	if (mtap->samplerate) {
		char *buf;
		buf = alloca(48);
		snprintf(buf, 47, ", %d Hz, %d Bit",
			 mtap->samplerate,
			 mtap->samplewidth);
		write_c_string(buf, printcharfun);
	}

	if (mtap->bitrate) {
		char *buf;
		buf = alloca(24);
		snprintf(buf, 23, ", %d kb/s", mtap->bitrate/1000);
		write_c_string(buf, printcharfun);
	}

	write_c_string(">", printcharfun);
}

static void
media_substream_print_video(media_substream *mss, Lisp_Object printcharfun)
{
	mtype_video_properties *mtvp =
		media_substream_type_properties(mss).vprops;

	write_c_string("#<video ", printcharfun);
	if (mtvp->name || mtvp->codec_name) {
		char *buf = alloca(64);
		if (mtvp->name && mtvp->codec_name)
			snprintf(buf, 63, "%s (%s)",
				 mtvp->name, mtvp->codec_name);
		else if (mtvp->name)
			snprintf(buf, 63, "%s [???]", mtvp->name);
		else if (mtvp->codec_name)
			snprintf(buf, 63, "??? (%s)", mtvp->codec_name);

		write_c_string(buf, printcharfun);
	} else
		write_c_string("???", printcharfun);

	if (mtvp->bitrate) {
		char *buf = alloca(24);
		snprintf(buf, 23, ", %d kb/s", mtvp->bitrate);
		write_c_string(buf, printcharfun);
	}

	if (mtvp->width && mtvp->height) {
		char *buf = alloca(48);
		if (mtvp->aspect_num > 1 && mtvp->aspect_den >= 1)
			snprintf(buf, 47, ", %dx%d (%d/%d)",
				 mtvp->width, mtvp->height,
				 mtvp->aspect_num, mtvp->aspect_den);
		else
			snprintf(buf, 47, ", %dx%d (%.2f/1)",
				 mtvp->width, mtvp->height,
				 (double)mtvp->width/(double)mtvp->height);
		write_c_string(buf, printcharfun);
	}
	write_c_string(">", printcharfun);
}

static void
media_substream_print(media_substream *mss,
		      Lisp_Object printcharfun, int escapeflag)
{
	write_c_string("#<media-substream :type ", printcharfun);

	switch (media_substream_type(mss)) {
	case MTYPE_AUDIO: {
		media_substream_print_audio(mss, printcharfun);
		break;
	}
	case MTYPE_VIDEO:
		media_substream_print_video(mss, printcharfun);
		break;
	case MTYPE_IMAGE:
		write_c_string("#<image>", printcharfun);
		break;
	default:
	case MTYPE_UNDECIDED:
		write_c_string("#<unknown>", printcharfun);
		break;
	}

	write_c_string(">", printcharfun);
}

static Lisp_Media_Stream *
media_stream_allocate(void)
{
	Lisp_Media_Stream *ms;

	ms = alloc_lcrecord_type(Lisp_Media_Stream, &lrecord_media_stream);
	return ms;
}


Lisp_Object make_media_stream()
{
	Lisp_Media_Stream *ms;
	Lisp_Object lms;

	ms = media_stream_allocate();
	media_stream_kind(ms) = MKIND_UNDECIDED;
	media_stream_driver(ms) = MDRIVER_UNDECIDED;
	media_stream_data(ms) = NULL;

	/* now set the navigation */
	media_stream_first(ms) = NULL;
	media_stream_last(ms) = NULL;

	XSETMEDIA_STREAM(lms, ms);

	return lms;
}

media_substream *make_media_substream(void)
{
/* this allocates and conses to the back of ms */
	media_substream *mss;

	mss = xnew_and_zero(media_substream);
	media_substream_type(mss) = MTYPE_UNDECIDED;
	media_substream_sread(mss) = NULL;
	media_substream_srewind(mss) = NULL;
	media_substream_data(mss) = NULL;

	/* set next/prev */
	media_substream_next(mss) = NULL;
	media_substream_prev(mss) = NULL;

#ifdef HAVE_THREADS
	pthread_mutex_init(&mss->substream_mutex, NULL);
#endif

	return mss;
}

media_substream *make_media_substream_append(Lisp_Media_Stream *ms)
{
	media_substream *mss;

	mss = make_media_substream();

	/* set next/prev */
	media_substream_next(mss) = NULL;
	if (!(media_stream_last(ms))) {
		media_substream_prev(mss) = NULL;
		media_stream_first(ms) = mss;
	} else {
		media_substream_prev(mss) = media_stream_last(ms);
		media_substream_next(media_stream_last(ms)) = mss;
	}

	media_stream_last(ms) = mss;
	media_substream_up(mss) = ms;

	return mss;
}

media_substream *make_media_substream_prepend(Lisp_Media_Stream *ms)
{
	media_substream *mss;

	mss = make_media_substream();

	/* set next/prev */
	media_substream_prev(mss) = NULL;
	if (!(media_stream_first(ms))) {
		media_substream_next(mss) = NULL;
		media_stream_last(ms) = mss;
	} else {
		media_substream_next(mss) = media_stream_first(ms);
		media_substream_prev(media_stream_first(ms)) = mss;
	}

	media_stream_first(ms) = mss;
	media_substream_up(mss) = ms;

	return mss;
}

DEFUN("make-media-stream", Fmake_media_stream, 0, 2, 0,	/*
Create a new media stream from DATA.
FROM is a keyword and defines how DATA is interpreted:
:file - DATA is the name of a file
:data - DATA is a string with the stream data
							*/
      (from, data))
{
	Lisp_Object lms;
	Lisp_Media_Stream *ms;
	enum whats_data {
		DATA_IS_BULLSHIT,
		DATA_IS_FILE,
		DATA_IS_DATA } datatype = DATA_IS_BULLSHIT;

	if (EQ(from, Q_file))
		datatype = DATA_IS_FILE;
	else if (EQ(from, Q_data))
		datatype = DATA_IS_DATA;
	else {
		datatype = DATA_IS_BULLSHIT;
		return Qnil;	/* in this case, why bother? stupid user :) */
	}

	/* hm, maybe data could be a symbol from the sound-alist? */
	CHECK_STRING(data);

	lms = make_media_stream();
	ms = XMEDIA_STREAM(lms);

	switch (datatype) {
	case DATA_IS_FILE: {
		mkind_file_properties *fprops;
		char *file_ext;
		int data_len = 0;

		/* expand-file-name first and check for existence*/
		data = Fexpand_file_name(data, Qnil);
		if (!NILP(Ffile_directory_p(data)) ||
		    NILP(Ffile_readable_p(data)))
			break;

		media_stream_kind(ms) = MKIND_FILE;

		/* initialise a new file properties structure */
		fprops = xnew_and_zero(mkind_file_properties);

		/* copy the filename also as C string */
		TO_EXTERNAL_FORMAT(LISP_STRING, data,
				   MALLOC, (file_ext, data_len),
				   Qbinary);
		file_ext[data_len] = '\0';
		fprops->filename = file_ext;

		/* assign the file properties */
		media_stream_kind_properties(ms).fprops = fprops;

		determine_stream_type(ms);
		break;
	}
	case DATA_IS_DATA: {
		mkind_string_properties *sprops;
		char *data_ext;
		int data_len = 0;

		media_stream_kind(ms) = MKIND_STRING;

		/* initialise a new file properties structure */
		sprops = xnew_and_zero(mkind_string_properties);

		/* copy the filename also as C string */
		TO_EXTERNAL_FORMAT(LISP_STRING, data,
				   MALLOC, (data_ext, data_len),
				   Qbinary);
		data_ext[data_len] = '\0';
		sprops->name = NULL;
		sprops->stream_data = data_ext;
		sprops->size = data_len;

		/* assign the file properties */
		media_stream_kind_properties(ms).sprops = sprops;

		determine_stream_type(ms);
		break;
	}
	case DATA_IS_BULLSHIT:
	default:
		break;
	}

	return lms;
}

static Lisp_Object recons(Lisp_Object to, Lisp_Object from)
{
	Lisp_Object result;

	result = to;

	while (!NILP(from)) {
		result = Fcons(XCAR(from), result);
		from = XCDR(from);
	}
	return result;
}

DEFUN("media-stream-p", Fmedia_stream_p, 1, 1, 0, /*
Return non-`nil' if object is a media-stream, `nil' otherwise.
						  */
      (object))
{
	if (MEDIA_STREAMP(object))
		return Qt;
	else
		return Qnil;
}

#if 0
DEFUN("audio-substream-p", Faudio_substream_p, 1, 1, 0, /*
Return non-`nil' if object is a media-substream with audio data,
`nil' otherwise.
						  */
      (object))
{
	if (MEDIA_SUBSTREAMP(object) &&
	    XMEDIA_SUBSTREAM_TYPE(object) == MTYPE_AUDIO)
		return Qt;
	else
		return Qnil;
}

DEFUN("video-substream-p", Fvideo_substream_p, 1, 1, 0, /*
Return non-`nil' if object is a media-substream with video data,
`nil' otherwise.
						  */
      (object))
{
	if (MEDIA_SUBSTREAMP(object) &&
	    XMEDIA_SUBSTREAM_TYPE(object) == MTYPE_VIDEO)
		return Qt;
	else
		return Qnil;
}
#endif

DEFUN("media-available-formats", Fmedia_available_formats, 0, 0, 0, /*
Return a list of input formats in the underlying media libraries.
								    */
      ())
{
	Lisp_Object formats;
	Lisp_Object temp;

	formats = Qnil;
	temp = Qnil;

#ifdef HAVE_FFMPEG
	temp = media_ffmpeg_available_formats();
#endif
	formats = recons(formats, temp);
	
#ifdef HAVE_SNDFILE
	temp = Qnil;
#endif
	formats = recons(formats, temp);

	return formats;
}

static void
media_substream_props(media_substream *mss, Lisp_Dllist *resdl)
{
	switch (media_substream_type(mss)) {
	case MTYPE_AUDIO: {
		mtype_audio_properties *mtap =
			media_substream_type_properties(mss).aprops;
		Lisp_Object type_cons =
			Fcons(intern("type"), intern("audio"));
		dllist_append(resdl, type_cons);

		if (mtap->name) {
			Lisp_Object demux_cons =
				Fcons(intern("demux"),
				      build_string(mtap->name));
			dllist_append(resdl, demux_cons);
		}
		if (mtap->codec_name) {
			Lisp_Object codec_cons =
				Fcons(intern("codec"),
				      build_string(mtap->codec_name));
			dllist_append(resdl, codec_cons);
		}

		if (mtap->channels) {
			Lisp_Object chan_cons =
				Fcons(intern("channels"),
				      make_int(mtap->channels));
			dllist_append(resdl, chan_cons);
		}

		if (mtap->samplerate) {
			Lisp_Object rate_cons =
				Fcons(intern("samplerate"),
				      make_int(mtap->samplerate));
			dllist_append(resdl, rate_cons);
		}

		if (mtap->bitrate) {
			Lisp_Object rate_cons =
				Fcons(intern("bitrate"),
				      make_int(mtap->bitrate));
			dllist_append(resdl, rate_cons);
		}

		break;
	}

	case MTYPE_VIDEO: {
		mtype_video_properties *mtvp =
			media_substream_type_properties(mss).vprops;
		Lisp_Object type_cons =
			Fcons(intern("type"), intern("video"));
		dllist_append(resdl, type_cons);

		if (mtvp->name) {
			Lisp_Object demux_cons =
				Fcons(intern("demux"),
				      build_string(mtvp->name));
			dllist_append(resdl, demux_cons);
		}
		if (mtvp->codec_name) {
			Lisp_Object codec_cons =
				Fcons(intern("codec"),
				      build_string(mtvp->codec_name));
			dllist_append(resdl, codec_cons);
		}

		if (mtvp->bitrate) {
			Lisp_Object rate_cons =
				Fcons(intern("bitrate"),
				      make_int(mtvp->bitrate));
			dllist_append(resdl, rate_cons);
		}

		if (mtvp->width) {
			Lisp_Object wid_cons =
				Fcons(intern("width"),
				      make_int(mtvp->width));
			dllist_append(resdl, wid_cons);
		}

		if (mtvp->height) {
			Lisp_Object hei_cons =
				Fcons(intern("height"),
				      make_int(mtvp->height));
			dllist_append(resdl, hei_cons);
		}

#ifdef HAVE_GMP
		if (mtvp->aspect_num > 1 && mtvp->aspect_den >= 1) {
			Lisp_Object asp_cons =
				Fcons(intern("aspect"),
				      make_bigq(mtvp->aspect_num,
						mtvp->aspect_den));
			dllist_append(resdl, asp_cons);
		}
#endif

		break;
	}
	case MTYPE_IMAGE: {
		Lisp_Object type_cons =
			Fcons(intern("type"), intern("image"));
		dllist_append(resdl, type_cons);

		break;
	}
	default:
	case MTYPE_UNDECIDED:
		break;
	}
}

EXFUN(Fdllist_to_list, 1);
DEFUN("media-properties", Fmedia_properties, 1, 1, 0, /*
Return an alist of available properties of media-stream STREAM.

Depending on the underlying stream this alist may be made of
several of the following keys, grouped by media contents.

general:
-------
'file (string) the stream's filename
'uri (string) the stream's URI
'fifo (string) the stream's FIFO socket
'driver (symbol) the stream's demuxer/decoder driver
'kind (symbol) the stream's kind (file, fifo, string, uri, etc.)
'type (symbol) the stream's type (audio, video, image, subtitle, lyrics, etc.)

audio:
-----
'codec (string) the suitable audio codec
'demux (string) the suitable demuxer
'title (string) the title of an audio track
'artist (string) the performing artist(s)
'album (string) the title of the album
'comment (string) an arbitrary comment
'genre (string) the genre identifier string
'year (integer) the year of release
'track (integer) the track number on the album
'length (integer) the length of the track in seconds
'bitrate (integer) the average bitrate of the track in kb/s
'samplerate (integer) the samplerate of the track in Hz
'channels (integer) the number of distinguished channels

video:
-----
'codec (string) the suitable audio codec
'demux (string) the suitable demuxer
'title (string) the title of a video track
'comment (string) an arbitrary comment
'year (integer) the year of release
'bitrate (integer) the average bitrate of the track in kb/s
'width (integer) the x-resolution in pixels
'height (integer) the y-resolution in pixels
'aspect (bigq) the aspect quotient

Keys which do not apply to the underlying stream or are not
defined in the tag section of the stream are simply left out
in the result alist.
						      */
      (stream))
{
	Lisp_Media_Stream *ms = XMEDIA_STREAM(stream);
	media_substream *mss;
	Lisp_Object result = make_dllist();
	Lisp_Dllist *resdl = XDLLIST(result);
	char *streaminfo = NULL;
	Lisp_Object driver_cons = Fcons(intern("driver"), Qnil);
	Lisp_Object kind_cons = Fcons(intern("kind"), Qnil);

	switch (media_stream_kind(ms)) {
	case MKIND_FILE: {
		char *filename = media_stream_kind_properties(ms).fprops->\
			filename;
		Lisp_Object file_cons =
			Fcons(intern("file"),
			      make_ext_string(filename, strlen(filename),
					      Qfile_name));

		XCDR(kind_cons) = intern("file");
		dllist_append(resdl, file_cons);
		break;
	}
	case MKIND_FIFO: {
		Lisp_Object fifo_cons =
			Fcons(intern("fifo"), Qnil);

		XCDR(kind_cons) = intern("fifo");
 		dllist_append(resdl, fifo_cons);
		break;
	}
	case MKIND_STREAM: {
		Lisp_Object uri_cons =
			Fcons(intern("uri"), Qnil);

		XCDR(kind_cons) = intern("uri");
 		dllist_append(resdl, uri_cons);
		break;
	}
	case MKIND_STRING:
		XCDR(kind_cons) = intern("string");
		break;
	default:
		XCDR(kind_cons) = intern("unknown");
		break;
	}

	dllist_append(resdl, kind_cons);

	switch (media_stream_driver(ms)) {
	case MDRIVER_NATIVE:
		XCDR(driver_cons) = Qnative;
		break;
	case MDRIVER_FFMPEG:
		XCDR(driver_cons) = intern("ffmpeg");
#if 0
		streaminfo = media_ffmpeg_streaminfo(ms);
#endif
		break;
	case MDRIVER_SNDFILE:
		XCDR(driver_cons) = intern("sndfile");
		break;
	case MDRIVER_MAD:
		XCDR(driver_cons) = intern("mad");
		break;
	case MDRIVER_SOX:
		XCDR(driver_cons) = intern("sox");
		break;
	case MDRIVER_XINE:
		XCDR(driver_cons) = intern("xine");
		break;
	case MDRIVER_GSTREAMER:
		XCDR(driver_cons) = intern("gstreamer");
		break;
	default:
		XCDR(driver_cons) = intern("unknown");
		break;
	}

	dllist_append(resdl, driver_cons);

#if 0
	if (streaminfo) {
		xfree(streaminfo);
	}
#endif

	mss = media_stream_first(ms);
	while (mss) {
		media_substream_props(mss, resdl);
		mss = media_substream_next(mss);
	}

	return Fdllist_to_list(result);
}


void syms_of_media(void)
{
	INIT_LRECORD_IMPLEMENTATION(media_stream);

	defsymbol(&Qmedia_streamp, "media-stream-p");

	DEFSUBR(Fmake_media_stream);
	DEFSUBR(Fmedia_stream_p);
#if 0
	DEFSUBR(Faudio_substream_p);
	DEFSUBR(Fvideo_substream_p);
#endif

	DEFSUBR(Fmedia_available_formats);
	DEFSUBR(Fmedia_properties);
}

void vars_of_media(void)
{
	Fprovide(intern("media"));

#ifdef HAVE_FFMPEG
	Fprovide(intern("media-ffmpeg"));
#endif
#ifdef HAVE_SNDFILE
	Fprovide(intern("media-sndfile"));
#endif
#ifdef HAVE_MAD
	Fprovide(intern("media-mad"));
#endif
#ifdef HAVE_SOX
	Fprovide(intern("media-sox"));
#endif
#ifdef HAVE_XINE
	Fprovide(intern("media-xine"));
#endif
#ifdef HAVE_GSTREAMER
	Fprovide(intern("media-gstreamer"));
#endif
#ifdef HAVE_NATIVE_MEDIA
	Fprovide(intern("media-native"));
#endif
}

/* media.c ends here */
