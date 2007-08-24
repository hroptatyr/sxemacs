/* media-sndfile.c - analyse audio files or streams

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

#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#include "media-sndfile.h"

Lisp_Object Qsndfile;

#define MYSELF MDRIVER_SNDFILE

static sf_count_t sndfile_vio_get_filelen(void*);
static sf_count_t sndfile_vio_seek(sf_count_t, int, void*);
static sf_count_t sndfile_vio_read(void*, sf_count_t, void*);
static sf_count_t sndfile_vio_write(const void*, sf_count_t, void*);
static sf_count_t sndfile_vio_tell(void*);

#define __SNDFILE_DEBUG__(args...)	fprintf(stderr, "SNDFILE " args)
#ifndef SNDFILE_DEBUG_FLAG
#define SNDFILE_DEBUG(args...)
#else
#define SNDFILE_DEBUG(args...)		__SNDFILE_DEBUG__(args)
#endif
#define SNDFILE_DEBUG_S(args...)	SNDFILE_DEBUG("[stream]: " args)
#define SNDFILE_CRITICAL(args...)	__SNDFILE_DEBUG__("CRITICAL: " args)


DECLARE_MEDIA_DRIVER_OPEN_METH(media_sndfile);
DECLARE_MEDIA_DRIVER_CLOSE_METH(media_sndfile);
DECLARE_MEDIA_DRIVER_READ_METH(media_sndfile);
DECLARE_MEDIA_DRIVER_REWIND_METH(media_sndfile);

DEFINE_MEDIA_DRIVER_CUSTOM(media_sndfile,
			   media_sndfile_open, media_sndfile_close,
			   NULL, NULL,
			   media_sndfile_read, NULL,
			   media_sndfile_rewind, NULL);


static void
media_sndfile_close(ms_driver_data_t arg)
{
	media_sndfile_data *sfd = arg;

	SNDFILE_DEBUG("closing sndfile handle: 0x%x\n",
		      (unsigned int)sfd->sf);

	if (sfd->sf)
		sf_close(sfd->sf);
	sfd->sf = NULL;

	if (sfd->sfinfo)
		xfree(sfd->sfinfo);
	sfd->sfinfo = NULL;

	return;
}

static ms_driver_data_t
media_sndfile_open(Lisp_Media_Stream *ms)
{
	mtype_audio_properties *mtap;
	media_substream *mss;
	/* libsndfile stuff */
	media_sndfile_data *sfd = NULL;
	SNDFILE *sf = NULL;
	SF_INFO *sfinfo;

	/* initialise */
	sfinfo = xnew_and_zero(SF_INFO);

	switch (media_stream_kind(ms)) {
	case MKIND_FILE: {
		mkind_file_properties *mkfp = NULL;
		const char *file;
		int file_len = 0;

		/* open the file */
		mkfp = media_stream_kind_properties(ms).fprops;
		TO_EXTERNAL_FORMAT(LISP_STRING, mkfp->filename,
				   ALLOCA, (file, file_len), Qnil);
		sf = sf_open(file, SFM_READ, sfinfo);
		break;
	}
	case MKIND_STRING: {
		mkind_string_properties *mksp = NULL;
		SF_VIRTUAL_IO *sfvio = NULL;
		/* our container for sfvio */
		media_data *sd = NULL;

		/* prepare sndfile's virtual-I/O */
		sfvio = xnew_and_zero(SF_VIRTUAL_IO);
		sfvio->get_filelen = &sndfile_vio_get_filelen;
		sfvio->seek = &sndfile_vio_seek;
		sfvio->read = &sndfile_vio_read;
		sfvio->write = &sndfile_vio_write;
		sfvio->tell = &sndfile_vio_tell;

		/* prepare our user_data */
		mksp = media_stream_kind_properties(ms).sprops;
		sd = xnew_and_zero(media_data);
		sd->length = mksp->size;
		sd->seek = 0;
		sd->data = mksp->stream_data;

		/* retrieve the main handle */
		sf = sf_open_virtual(sfvio, SFM_READ, sfinfo, (void*)sd);
		break;
	}
	default:
		break;
	}

	if (!sf) {
		xfree(sfinfo);
		media_stream_set_meths(ms, NULL);
		media_stream_driver(ms) = MDRIVER_UNKNOWN;
		return NULL;
	}

	/* now create a substream and fill it with information */
	mss = make_media_substream_append(ms);
	media_substream_type(mss) = MTYPE_AUDIO;
	mtap = xnew_and_zero(mtype_audio_properties);

	mtap->channels = sfinfo->channels;
	mtap->samplerate = sfinfo->samplerate;

	/* try to find a read function */
	switch (sfinfo->format & 0xFF) {
        case SF_FORMAT_ULAW:
        case SF_FORMAT_ALAW:
        case SF_FORMAT_PCM_U8:
	case SF_FORMAT_PCM_S8:
		mtap->samplewidth = 8;
		mtap->framesize = mtap->channels;
		/* stuff is read in as S16 values anyway */
		mtap->msf = sxe_msf_S16;
		break;
        case SF_FORMAT_PCM_16:
		mtap->samplewidth = 16;
		mtap->framesize = mtap->channels * 2;
		mtap->msf = sxe_msf_S16;
		break;
	case SF_FORMAT_PCM_24:
		mtap->samplewidth = 32;
		mtap->framesize = mtap->channels * 4;
		mtap->msf = sxe_msf_S24;
		break;
	case SF_FORMAT_PCM_32:
		mtap->samplewidth = 32;
		mtap->framesize = mtap->channels * 4;
		mtap->msf = sxe_msf_S32;
		break;
        case SF_FORMAT_FLOAT:
		mtap->samplewidth = 32;
		mtap->framesize = mtap->channels * 4;
		mtap->msf = sxe_msf_FLT;
		break;
        default:
		xfree(sfinfo);
		media_stream_set_meths(ms, NULL);
		media_stream_driver(ms) = MDRIVER_UNKNOWN;
		return NULL;
		break;
	}
	mtap->name = NULL;
	mtap->endianness = 0;

	/* now assign */
	media_substream_type_properties(mss).aprops = mtap;
	media_stream_set_meths(ms, media_sndfile);

	/* keep the SNDFILE context */
	sfd = xnew_and_zero(media_sndfile_data);
	sfd->sf = sf;
	sfd->sfinfo = sfinfo;
	media_substream_data(mss) = sfd;
	media_stream_data(ms) = sfd;

	/* set me as driver indicator */
	media_stream_driver(ms) = MYSELF;

	return sf;
}


static sf_count_t
sndfile_vio_get_filelen(void* user_data)
{
	media_data *sd = (media_data*)user_data;

	return sd->length;
}
static sf_count_t
sndfile_vio_seek(sf_count_t offset, int whence, void* user_data)
{
	media_data *sd = (media_data*)user_data;

	switch (whence) {
	case SEEK_SET:
		sd->seek = offset;
		break;
	case SEEK_CUR:
		sd->seek = sd->seek+offset;
		break;
	case SEEK_END:
		sd->seek = sd->length + offset;
		break;
	}
	return sd->seek;
}
static sf_count_t
sndfile_vio_read(void* ptr, sf_count_t count, void* user_data)
{
	media_data *sd = (media_data*)user_data;

	if (count > sd->length - sd->seek)
		count = sd->length - sd->seek;
	if (count < 0)
		count = 0;

	memcpy(ptr, sd->data+sd->seek, count);
	sd->seek += count;
	return count;
}
static sf_count_t
sndfile_vio_write(const void* ptr, sf_count_t count, void* user_data)
{
	media_data *sd = (media_data*)user_data;

	memcpy(sd->data+sd->seek, ptr, count);
	sd->seek += count;
	return count;
}
static sf_count_t
sndfile_vio_tell(void* user_data)
{
	media_data *sd = (media_data*)user_data;

	return sd->seek;
}


static size_t
media_sndfile_read(media_substream *mss, void *outbuf, size_t length)
{
/* read at most `length' frames into `outbuf' */
	/* libsndfile stuff */
	media_sndfile_data *sfd;
	SNDFILE *sf;
	/* media stream stuff */
	Lisp_Media_Stream *ms = mss->up;
	mtype_audio_properties *mtap;
	media_sample_format_t *fmt;
	sf_count_t read = 0;

	/* check the integrity of the media stream */
	if (media_stream_driver(ms) != MYSELF)
		return 0;
	if (media_substream_type(mss) != MTYPE_AUDIO)
		return 0;

	/* fetch the SNDFILE context and our audio props */
	if ((sfd = media_stream_data(ms)) == NULL ||
	    (sf = sfd->sf) == NULL)
		return 0;
	if (!(mtap = media_substream_type_properties(mss).aprops))
		return 0;

	fmt = mtap->msf;

	switch (sfd->sfinfo->format & 0xFF) {
        case SF_FORMAT_ULAW:
        case SF_FORMAT_ALAW:
        case SF_FORMAT_PCM_U8:
	case SF_FORMAT_PCM_S8:
        case SF_FORMAT_PCM_16:
		read = sf_readf_short(sf, outbuf, length);
		break;
	case SF_FORMAT_PCM_24:
	case SF_FORMAT_PCM_32:
		read = sf_readf_int(sf, outbuf, length);
		break;
        case SF_FORMAT_FLOAT:
		read = sf_readf_float(sf, outbuf, length);
		break;
	}

	/* always convert to internal format */
	MEDIA_SAMPLE_FORMAT_UPSAMPLE(fmt)(outbuf, outbuf, read*mtap->channels);

	SNDFILE_DEBUG_S("read %d frames\n", read);

	return read;
}

static void
media_sndfile_rewind(media_substream *mss)
{
/* rewind the stream to the first frame */
	/* libsndfile stuff */
	media_sndfile_data *sfd;
	SNDFILE *sf;
	/* media stream stuff */
	Lisp_Media_Stream *ms = mss->up;

	/* check the integrity of the media stream */
	if (media_stream_driver(ms) != MYSELF)
		return;
	if (media_substream_type(mss) != MTYPE_AUDIO)
		return;

	/* fetch the SNDFILE context and our audio props */
	if ((sfd = media_stream_data(ms)) == NULL ||
	    (sf = sfd->sf) == NULL)
		return;

	SNDFILE_DEBUG_S("rewind stream 0x%x\n", (unsigned int)sf);
	sf_seek(sf, 0, SEEK_SET);
}

#undef MYSELF
