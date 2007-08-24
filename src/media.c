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
#include "sound.h"
#ifdef HAVE_FFMPEG
#include "media-ffmpeg.h"
#endif
#ifdef HAVE_SNDFILE
#include "media-sndfile.h"
#endif
#ifdef HAVE_INTERNAL_MEDIA
#include "media-internal.h"
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
Lisp_Object Qunknown;

static void determine_stream_type(Lisp_Media_Stream *ms, media_driver);
static void media_stream_print(Lisp_Object, Lisp_Object, int);
static void media_substream_print(media_substream *, Lisp_Object, int);
static void media_substream_finalise(void*, int);


static void determine_stream_type(Lisp_Media_Stream *ms, media_driver preferred)
{
#ifdef HAVE_FFMPEG
	if ((media_stream_driver(ms) == MDRIVER_UNKNOWN) &&
	    (preferred == MDRIVER_UNKNOWN ||
	     preferred == MDRIVER_FFMPEG)) {
		MEDIA_DEBUG("trying ffmpeg.\n");
		media_stream_set_meths(ms, media_ffmpeg);
		media_stream_meth(ms, open)(ms);
	}
#endif
#ifdef HAVE_MAD
	if ((media_stream_driver(ms) == MDRIVER_UNKNOWN) &&
	    (preferred == MDRIVER_UNKNOWN ||
	     preferred == MDRIVER_MAD)) {
		MEDIA_DEBUG("trying mad.\n");
		media_stream_set_meths(ms, media_mad);
		media_stream_meth(ms, open)(ms);
	}
#endif
#ifdef HAVE_SOX
	if ((media_stream_driver(ms) == MDRIVER_UNKNOWN) &&
	    (preferred == MDRIVER_UNKNOWN ||
	     preferred == MDRIVER_SOX)) {
		MEDIA_DEBUG("trying sox.\n");
		media_stream_set_meths(ms, media_sox);
		media_stream_meth(ms, open)(ms);
	}
#endif
#ifdef HAVE_SNDFILE
	if ((media_stream_driver(ms) == MDRIVER_UNKNOWN) &&
	    (preferred == MDRIVER_UNKNOWN ||
	     preferred == MDRIVER_SNDFILE)) {
		MEDIA_DEBUG("trying sndfile.\n");
		media_stream_set_meths(ms, media_sndfile);
		media_stream_meth(ms, open)(ms);
	}
#endif
/* not working stuff here :) */
#if 0
#ifdef HAVE_GSTREAMER
	if ((media_stream_driver(ms) == MDRIVER_UNKNOWN) &&
	    (preferred == MDRIVER_UNKNOWN ||
	     preferred == MDRIVER_GSTREAMER)) {
		MEDIA_DEBUG("trying gstreamer.\n");
		media_gstreamer_analyse_stream(ms);
	}
#endif
#endif
#ifdef HAVE_XINE
	if ((media_stream_driver(ms) == MDRIVER_UNKNOWN) &&
	    (preferred == MDRIVER_UNKNOWN ||
	     preferred == MDRIVER_XINE)) {
		MEDIA_DEBUG("trying xine.\n");
		media_stream_set_meths(ms, media_xine);
		media_stream_meth(ms, open)(ms);
	}
#endif
#ifdef HAVE_INTERNAL_MEDIA
	if ((media_stream_driver(ms) == MDRIVER_UNKNOWN) &&
	    (preferred == MDRIVER_UNKNOWN ||
	     preferred == MDRIVER_INTERNAL)) {
		MEDIA_DEBUG("trying internal.\n");
		media_internal_analyse_stream(ms);
	}
#endif
	if (media_stream_driver(ms) == MDRIVER_UNKNOWN) {
		MEDIA_DEBUG("giving up\n");
		media_stream_set_meths(ms, NULL);
	}
	return;
}


/*****************************************************************/
/* 			    media streams			 */
/*****************************************************************/
static Lisp_Object media_stream_mark(Lisp_Object obj)
{
	switch (XMEDIA_STREAM_KIND(obj)) {
	case MKIND_FILE:
		mark_object(XMEDIA_STREAM(obj)->
			    kind_properties.fprops->filename);
		break;
	case MKIND_STRING:
	case MKIND_FIFO:
	case MKIND_STREAM:
	case MKIND_UNKNOWN:
	default:
		break;
	}

	if (XMEDIA_STREAM_METHS(obj) &&
	    XMEDIA_STREAM_METH(obj, mark))
		XMEDIA_STREAM_METH(obj, mark)(XMEDIA_STREAM_DATA(obj));

	return Qnil;
}

static void
media_stream_finalise(void *header, int for_disksave)
{
	Lisp_Media_Stream *ms = (Lisp_Media_Stream*)header;
	media_substream *mss = NULL;

	if (media_stream_meths(ms) &&
	    media_stream_meth(ms, close))
		media_stream_meth(ms, close)(media_stream_data(ms));

	mss = media_stream_first(ms);
	while (mss) {
		media_substream_finalise(mss, for_disksave);
		mss = media_substream_next(mss);
	}

#if 0
	switch (media_stream_driver(ms)) {
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
#endif

	switch (media_stream_kind(ms)) {
	case MKIND_FILE: {
		mkind_file_properties *mkfp;
		mkfp = media_stream_kind_properties(ms).fprops;
		if (mkfp) {
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
media_stream_print(Lisp_Object obj, Lisp_Object printcharfun, int ef)
{
	Lisp_Media_Stream *ms = XMEDIA_STREAM(obj);
	media_substream *mss;

	write_c_string("#<media-stream", printcharfun);

	write_c_string(" :kind ", printcharfun);

	switch (media_stream_kind(ms)) {
	case MKIND_FILE: {
		Lisp_Object file =
			media_stream_kind_properties(ms).fprops->filename;

		write_c_string("#<file ", printcharfun);
		print_internal(file, printcharfun, ef);
		write_c_string(">", printcharfun);
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
	case MKIND_UNKNOWN:
		write_c_string("#<unknown>", printcharfun);
		break;
	}

	mss = media_stream_first(ms);
	while (mss) {
		write_c_string(" ", printcharfun);
		media_substream_print(mss, printcharfun, ef);
		mss = media_substream_next(mss);
	}

	write_c_string(" driven by ", printcharfun);
	switch (media_stream_driver(ms)) {
	case MDRIVER_INTERNAL:
		write_c_string("internal", printcharfun);
		break;
	case MDRIVER_FFMPEG:
		write_c_string("ffmpeg", printcharfun);
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
	case MDRIVER_UNKNOWN:
	default:
		XMEDIA_STREAM_SET_METHS(obj, NULL);
		write_c_string("unknown", printcharfun);
		break;
	}

	if (XMEDIA_STREAM_METHS(obj) &&
	    XMEDIA_STREAM_METH(obj, print)) {
		XMEDIA_STREAM_METH(obj, print)(obj, printcharfun, ef);
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
	case MTYPE_UNKNOWN:
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
	media_stream_kind(ms) = MKIND_UNKNOWN;
	media_stream_driver(ms) = MDRIVER_UNKNOWN;
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
	media_substream_type(mss) = MTYPE_UNKNOWN;
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

DEFUN("make-media-stream", Fmake_media_stream, 2, 3, 0,	/*
Create a new media stream from DATA.

FROM is a keyword and defines how DATA is interpreted:
:file - DATA is the name of a file
:data - DATA is a string with the stream data
:url  - DATA is a url (string) for streamed media contents

Optional argument DRIVER (a symbol) may be used to force
the use of a certain driver instead of automatically
detecting a suitable one.  It is one of `ffmpeg', `sndfile',
`sox', `mad', `xine', `gstreamer', or `internal'.
							*/
      (from, data, driver))
{
	Lisp_Object lms;
	Lisp_Media_Stream *ms;
	enum whats_data {
		DATA_IS_BULLSHIT,
		DATA_IS_FILE,
		DATA_IS_URL,
		DATA_IS_DATA } datatype = DATA_IS_BULLSHIT;
	media_driver pref = MDRIVER_UNKNOWN;

	if (0);
	else if (EQ(from, Q_file))
		datatype = DATA_IS_FILE;
	else if (EQ(from, Q_data))
		datatype = DATA_IS_DATA;
	else if (EQ(from, Q_url))
		datatype = DATA_IS_URL;
	else {
		datatype = DATA_IS_BULLSHIT;
		return Qnil;	/* in this case, why bother? stupid user :) */
	}

	if (NILP(driver))
		pref = MDRIVER_UNKNOWN;
#ifdef HAVE_MAD
	else if (EQ(driver, Qmad))
		pref = MDRIVER_MAD;
#endif
#ifdef HAVE_FFMPEG
	else if (EQ(driver, Qffmpeg))
		pref = MDRIVER_FFMPEG;
#endif
#ifdef HAVE_SOX
	else if (EQ(driver, Qsox))
		pref = MDRIVER_SOX;
#endif
	else if (EQ(driver, intern("xine")))
		pref = MDRIVER_XINE;
#ifdef HAVE_SNDFILE
	else if (EQ(driver, Qsndfile))
		pref = MDRIVER_SNDFILE;
#endif
	else if (EQ(driver, intern("internal")))
		pref = MDRIVER_INTERNAL;
	else if (EQ(driver, intern("gstreamer")))
		pref = MDRIVER_GSTREAMER;
	else
		pref = MDRIVER_UNKNOWN;

	/* hm, maybe data could be a symbol from the sound-alist?
	 * or a buffer or a network socket?
	 */
	CHECK_STRING(data);

	lms = make_media_stream();
	ms = XMEDIA_STREAM(lms);

	switch (datatype) {
	case DATA_IS_FILE: {
		mkind_file_properties *fprops;

		/* expand-file-name first and check for existence*/
		data = Fexpand_file_name(data, Qnil);
		if (!NILP(Ffile_directory_p(data)) ||
		    NILP(Ffile_readable_p(data)))
			break;

		media_stream_kind(ms) = MKIND_FILE;

		/* initialise a new file properties structure */
		fprops = xnew_and_zero(mkind_file_properties);

		/* copy the filename also as C string */
		fprops->filename = data;

		/* assign the file properties */
		media_stream_kind_properties(ms).fprops = fprops;

		determine_stream_type(ms, pref);
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

		determine_stream_type(ms, pref);
		break;
	}
	case DATA_IS_URL: {
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
media_substream_props(media_substream *mss, dllist_t resdl)
{
	switch (media_substream_type(mss)) {
	case MTYPE_AUDIO: {
		mtype_audio_properties *mtap =
			media_substream_type_properties(mss).aprops;
		Lisp_Object type_cons =
			Fcons(intern("type"), intern("audio"));
		dllist_append(resdl, (void*)type_cons);

		if (mtap->name) {
			Lisp_Object demux_cons =
				Fcons(intern("demux"),
				      build_string(mtap->name));
			dllist_append(resdl, (void*)demux_cons);
		}
		if (mtap->codec_name) {
			Lisp_Object codec_cons =
				Fcons(intern("codec"),
				      build_string(mtap->codec_name));
			dllist_append(resdl, (void*)codec_cons);
		}

		if (mtap->channels) {
			Lisp_Object chan_cons =
				Fcons(intern("channels"),
				      make_int(mtap->channels));
			dllist_append(resdl, (void*)chan_cons);
		}

		if (mtap->samplerate) {
			Lisp_Object rate_cons =
				Fcons(intern("samplerate"),
				      make_int(mtap->samplerate));
			dllist_append(resdl, (void*)rate_cons);
		}

		if (mtap->bitrate) {
			Lisp_Object rate_cons =
				Fcons(intern("bitrate"),
				      make_int(mtap->bitrate));
			dllist_append(resdl, (void*)rate_cons);
		}

		break;
	}

	case MTYPE_VIDEO: {
		mtype_video_properties *mtvp =
			media_substream_type_properties(mss).vprops;
		Lisp_Object type_cons =
			Fcons(intern("type"), intern("video"));
		dllist_append(resdl, (void*)type_cons);

		if (mtvp->name) {
			Lisp_Object demux_cons =
				Fcons(intern("demux"),
				      build_string(mtvp->name));
			dllist_append(resdl, (void*)demux_cons);
		}
		if (mtvp->codec_name) {
			Lisp_Object codec_cons =
				Fcons(intern("codec"),
				      build_string(mtvp->codec_name));
			dllist_append(resdl, (void*)codec_cons);
		}

		if (mtvp->bitrate) {
			Lisp_Object rate_cons =
				Fcons(intern("bitrate"),
				      make_int(mtvp->bitrate));
			dllist_append(resdl, (void*)rate_cons);
		}

		if (mtvp->width) {
			Lisp_Object wid_cons =
				Fcons(intern("width"),
				      make_int(mtvp->width));
			dllist_append(resdl, (void*)wid_cons);
		}

		if (mtvp->height) {
			Lisp_Object hei_cons =
				Fcons(intern("height"),
				      make_int(mtvp->height));
			dllist_append(resdl, (void*)hei_cons);
		}

#ifdef HAVE_GMP
		if (mtvp->aspect_num > 1 && mtvp->aspect_den >= 1) {
			Lisp_Object asp_cons =
				Fcons(intern("aspect"),
				      make_bigq(mtvp->aspect_num,
						mtvp->aspect_den));
			dllist_append(resdl, (void*)asp_cons);
		}
#endif

		break;
	}
	case MTYPE_IMAGE: {
		Lisp_Object type_cons =
			Fcons(intern("type"), intern("image"));
		dllist_append(resdl, (void*)type_cons);

		break;
	}
	default:
	case MTYPE_UNKNOWN:
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
	dllist_t resdl = make_dllist();
	Lisp_Object driver_cons = Fcons(intern("driver"), Qnil);
	Lisp_Object kind_cons = Fcons(intern("kind"), Qnil);

	switch (media_stream_kind(ms)) {
	case MKIND_FILE: {
		Lisp_Object file =
			media_stream_kind_properties(ms).fprops->filename;
		Lisp_Object file_cons =
			Fcons(intern("file"), file);

		XCDR(kind_cons) = Qfile;
		dllist_append(resdl, (void*)file_cons);
		break;
	}
	case MKIND_FIFO: {
		Lisp_Object fifo_cons =
			Fcons(intern("fifo"), Qnil);

		XCDR(kind_cons) = intern("fifo");
 		dllist_append(resdl, (void*)fifo_cons);
		break;
	}
	case MKIND_STREAM: {
		Lisp_Object uri_cons =
			Fcons(intern("uri"), Qnil);

		XCDR(kind_cons) = intern("uri");
 		dllist_append(resdl, (void*)uri_cons);
		break;
	}
	case MKIND_STRING:
		XCDR(kind_cons) = intern("string");
		break;
	default:
		XCDR(kind_cons) = intern("unknown");
		break;
	}

	dllist_append(resdl, (void*)kind_cons);

	switch (media_stream_driver(ms)) {
	case MDRIVER_INTERNAL:
		XCDR(driver_cons) = Qinternal;
		break;
	case MDRIVER_FFMPEG:
#ifdef HAVE_FFMPEG
		XCDR(driver_cons) = Qffmpeg;
#if 0
		streaminfo = media_ffmpeg_streaminfo(ms);
#endif
#endif
		break;
	case MDRIVER_SNDFILE:
#ifdef HAVE_SNDFILE
		XCDR(driver_cons) = Qsndfile;
#endif
		break;
	case MDRIVER_MAD:
#ifdef HAVE_MAD
		XCDR(driver_cons) = Qmad;
#endif
		break;
	case MDRIVER_SOX:
#ifdef HAVE_SOX
		XCDR(driver_cons) = Qsox;
#endif
		break;
	case MDRIVER_XINE:
		XCDR(driver_cons) = intern("xine");
		break;
	case MDRIVER_GSTREAMER:
		XCDR(driver_cons) = intern("gstreamer");
		break;
	default:
		XCDR(driver_cons) = Qunknown;
		break;
	}

	dllist_append(resdl, (void*)driver_cons);

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

	return Fdllist_to_list(wrap_dllist(resdl));
}

/* Audio Coercion */
/* SXEmacs works internally with samples in 24bit resolution */

DEFINE_MEDIA_SAMPLE_FORMAT_SIMPLE(sxe_msf_U8);

static void
sxe_msf_U8_up(void *d, void *s, size_t len)
{
	/* convert U8 samples to internal format (S24in32) */
	int i;
	int32_t *dst = d;
	uint8_t *src = s;

	/* len is the number of samples (== #frame * #channels) */
	MEDIA_DEBUG_FMT("upsampling U8->internal: %u samples\n", len);

	for (i = len-1; i >= 0; i--)
		dst[i] = (int32_t)(src[i] ^ 0x80) << 16;

	return;
}

static void
sxe_msf_U8_down(void *d, void *s, size_t len)
{
	/* convert samples from internal format (S24in32) to U8 */
	size_t i;
	uint8_t *dst = d;
	int32_t *src = s;

	/* len is the number of samples (== #frame * #channels) */
	MEDIA_DEBUG_FMT("downsampling internal->U8: %u samples\n", len);

	for (i = 0; i < len; i++)
		dst[i] = (uint8_t)(src[i] >> 16) ^ 0x80;

	return;
}

DEFINE_MEDIA_SAMPLE_FORMAT_SIMPLE(sxe_msf_S16);

static void
sxe_msf_S16_up(void *d, void *s, size_t len)
{
	/* convert S16 samples to internal format (S24in32) */
	int i;
	int32_t *dst = d;
	int16_t *src = s;

	/* len is the number of samples (== #frame * #channels) */
	MEDIA_DEBUG_FMT("upsampling S16->internal: %u samples\n", len);

	for (i = len-1; i >= 0; i--)
		dst[i] = (int32_t)(src[i]) << 8;
	MEDIA_DEBUG_FMT("d00:%d  d01:%d\n", dst[0], dst[1]);

	return;
}

static void
sxe_msf_S16_down(void *d, void *s, size_t len)
{
	/* convert samples from internal format (S24in32) to S16 */
	size_t i;
	int16_t *dst = d;
	int32_t *src = s;

	/* len is the number of samples (== #frame * #channels) */
	MEDIA_DEBUG_FMT("downsampling internal->S16: %u samples\n", len);

	for (i = 0; i < len; i++)
		dst[i] = (int16_t)(src[i] >> 8);

	return;
}

DEFINE_MEDIA_SAMPLE_FORMAT_SIMPLE(sxe_msf_S24);	/* format internally used */

static void
sxe_msf_S24_up(void *d, void *s, size_t len)
{
	MEDIA_DEBUG_FMT("upsampling S24->internal: %u samples\n", len);

	/* S24 _is_ the internal format */
	return;
}

static void
sxe_msf_S24_down(void *d, void *s, size_t len)
{
	MEDIA_DEBUG_FMT("downsampling internal->S24: %u samples\n", len);

	/* S24 _is_ the internal format */
	return;
}

DEFINE_MEDIA_SAMPLE_FORMAT_SIMPLE(sxe_msf_S32);

static void
sxe_msf_S32_up(void *d, void *s, size_t len)
{
	/* convert S32 samples to internal format (S24in32) */
	size_t i;
	int32_t *dst = d;
	int32_t *src = s;

	/* len is the number of samples (== #frame * #channels) */
	MEDIA_DEBUG_FMT("upsampling S32->internal: %u samples\n", len);

	for (i = 0; i < len; i++)
		dst[i] = src[i] >> 8;

	return;
}

static void
sxe_msf_S32_down(void *d, void *s, size_t len)
{
	/* convert samples from internal format (S24in32) to S32 */
	size_t i;
	int32_t *dst = d;
	int32_t *src = s;

	/* len is the number of samples (== #frame * #channels) */
	MEDIA_DEBUG_FMT("downsampling internal->S32: %u samples\n", len);

	for (i = 0; i < len; i++)
		dst[i] = src[i] << 8;

	return;
}

DEFINE_MEDIA_SAMPLE_FORMAT_SIMPLE(sxe_msf_FLT);

static void
sxe_msf_FLT_up(void *d, void *s, size_t len)
{
	/* convert float samples to internal format (S24in32) */
	size_t i;
	int32_t *dst = d;
	float *src = s;

	/* len is the number of samples (== #frame * #channels) */
	MEDIA_DEBUG_FMT("upsampling FLT->internal: %u samples\n", len);

	for (i = 0; i < len; i++) {
		dst[i] = (int32_t)(src[i] * SXE_MAX_S24);
	}
	MEDIA_DEBUG_FMT("s00:%f d00:%d  s01:%f d01:%d\n",
			src[0], dst[0], src[1], dst[1]);

	return;
}

static void
sxe_msf_FLT_down(void *d, void *s, size_t len)
{
	/* convert samples from internal format (S24in32) to float */
	int i;
	float *dst = d;
	int32_t *src = s;

	/* len is the number of samples (== #frame * #channels) */
	MEDIA_DEBUG_FMT("downsampling internal->FLT: %u samples\n", len);

	for (i = len-1; i >= 0; i--) {
		dst[i] = (float)(src[i]) / SXE_MAX_S24;
	}
	MEDIA_DEBUG_FMT("d00:%f  d01:%f\n", dst[0], dst[1]);

	return;
}

/* `effects' */
DEFINE_MEDIA_SAMPLE_EFFECT(sxe_mse_1ch_to_2ch, _sxe_mse_1ch_to_2ch);

static size_t
_sxe_mse_1ch_to_2ch(sxe_media_sample_t *dst, sxe_media_sample_t *src,
		    size_t len, void *ignored)
{
	/* mono to stereo converter */
	int i;

	MEDIA_DEBUG_COE("mono->stereo: %u samples\n", len);

	/* len is the number of samples */
	for (i = len-1; i >= 0; i--) {
		dst[2*i] = src[i];
		dst[2*i+1] = src[i];
	}

	return len * 2;
}

DEFINE_MEDIA_SAMPLE_EFFECT(sxe_mse_2ch_to_1ch, _sxe_mse_2ch_to_1ch);

static size_t
_sxe_mse_2ch_to_1ch(sxe_media_sample_t *dst, sxe_media_sample_t *src,
		    size_t len, void *args)
{
	/* stereo to mono converter */
	size_t i;
	sxe_mse_2ch_to_1ch_args *_args = args;
	int c = 0;

	MEDIA_DEBUG_COE("stereo->mono: %u samples\n", len);

	if (_args) {
		c = _args->chan;
	}

	/* len is the number of samples */
	for (i = 0; i < len/2; i++) {
		dst[i] = src[2*i+c];
	}

	return len / 2;
}

DEFINE_MEDIA_SAMPLE_EFFECT(sxe_mse_5ch_to_2ch, _sxe_mse_5ch_to_2ch);

static size_t
_sxe_mse_5ch_to_2ch(sxe_media_sample_t *dst, sxe_media_sample_t *src,
		    size_t len, void *args)
{
	/* 5 channel to stereo converter */
	size_t i;
	sxe_mse_5ch_to_2ch_args *_args = args;
	int c1 = 0, c2 = 1;

	MEDIA_DEBUG_COE("5ch->stereo: %u samples\n", len);

	if (_args) {
		c1 = _args->chan1;
		c2 = _args->chan1;
	}

	/* len is the number of samples */
	for (i = 0; i < len/5; i++) {
		dst[2*i] = src[5*i+c1];
		dst[2*i+1] = src[5*i+c2];
	}

	return len * 2 / 5;
}

DEFINE_MEDIA_SAMPLE_EFFECT(sxe_mse_volume, _sxe_mse_volume);

static size_t
_sxe_mse_volume(sxe_media_sample_t *dst, sxe_media_sample_t *src,
		size_t len, void *args)
{
	/* stereo to mono converter */
	size_t i, j;
	sxe_mse_volume_args *_args = args;

	MEDIA_DEBUG_COE("volume: %u samples\n", len);

	/* len is the number of samples */
	for (i = 0; i < len; i+=_args->num_channels) {
		for (j = 0; j < _args->num_channels; j++) {
			uint8_t vol = _args->volume[j];
			dst[i+j] = (src[i+j] * vol) >> 7;
		}
	}

	return len;
}

DEFINE_MEDIA_SAMPLE_EFFECT(sxe_mse_rerate, _sxe_mse_rerate);

static size_t
_sxe_mse_rerate(sxe_media_sample_t *dst, sxe_media_sample_t *src,
		size_t len, void *args)
{
	/* rate converter */
	int i;
	sxe_mse_rerate_args *_args = args;
	float trafo = (float)_args->srcrate / (float)_args->tgtrate
		* _args->tweak;
	int chans = _args->num_channels;
	int bound = len/chans/trafo;

	MEDIA_DEBUG_COE("rerate: %u samples, final trafo: %f, bound is: %d\n",
			len, trafo, bound);

	if (trafo < 1.0) {
		for (i = bound-1; i >= 0; i--) {
			int frame = i * trafo;
			dst[chans*i] = (src[chans*frame]);
			dst[chans*i+1] = (src[chans*frame+1]);
		}
	} else if (trafo > 1.0) {
		for (i = 0; i < bound-1; i++) {
			int frame = i * trafo;
			dst[chans*i] = (src[chans*frame]);
			dst[chans*i+1] = (src[chans*frame+1]);
		}
	}

	return bound * chans;
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

#ifdef HAVE_FFMPEG
	defsymbol(&Qffmpeg, "ffmpeg");
#endif
#ifdef HAVE_MAD
	defsymbol(&Qmad, "mad");
#endif
#ifdef HAVE_SOX
	defsymbol(&Qsox, "sox");
#endif
#ifdef HAVE_SNDFILE
	defsymbol(&Qsndfile, "sndfile");
#endif
	defsymbol(&Qunknown, "unknown");
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
#ifdef HAVE_INTERNAL_MEDIA
	Fprovide(intern("media-internal"));
#endif
}

/* media.c ends here */
