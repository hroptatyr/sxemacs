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

static sf_count_t sndfile_vio_get_filelen(void*);
static sf_count_t sndfile_vio_seek(sf_count_t, int, void*);
static sf_count_t sndfile_vio_read(void*, sf_count_t, void*);
static sf_count_t sndfile_vio_write(const void*, sf_count_t, void*);
static sf_count_t sndfile_vio_tell(void*);

static uint32_t media_sndfile_sread_audio(Lisp_Media_Stream*, void*, uint32_t);
static void media_sndfile_srewind_audio(Lisp_Media_Stream*);

#define MYSELF MDRIVER_SNDFILE


void media_sndfile_analyse_stream(Lisp_Media_Stream *ms)
{
	mtype_audio_properties *mtap;
	media_substream *mss;
	char *name = NULL;
	/* libsndfile stuff */
	SNDFILE *sf = NULL;
	SF_INFO *sfinfo;

	/* initialise */
	sfinfo = xnew_and_zero(SF_INFO);

	switch (media_stream_kind(ms)) {
	case MKIND_FILE: {
		mkind_file_properties *mkfp = NULL;
		const char *file;

		/* open the file */
		mkfp = media_stream_kind_properties(ms).fprops;
		file = mkfp->filename;
		sf = sf_open(file, SFM_READ, sfinfo);
		break;
	}
	case MKIND_STRING: {
		mkind_string_properties *mksp = NULL;
		SF_VIRTUAL_IO *sfvio = NULL;
		/* our container for sfvio */
		sound_data *sd = NULL;

		/* prepare sndfile's virtual-I/O */
		sfvio = xnew_and_zero(SF_VIRTUAL_IO);
		sfvio->get_filelen = &sndfile_vio_get_filelen;
		sfvio->seek = &sndfile_vio_seek;
		sfvio->read = &sndfile_vio_read;
		sfvio->write = &sndfile_vio_write;
		sfvio->tell = &sndfile_vio_tell;

		/* prepare our user_data */
		mksp = media_stream_kind_properties(ms).sprops;
		sd = xnew_and_zero(sound_data);
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
		return;
	}

	/* now create a substream and fill it with information */
	mss = make_media_substream_append(ms);
	media_substream_type(mss) = MTYPE_AUDIO;
	mtap = xnew_and_zero(mtype_audio_properties);

	mtap->channels = sfinfo->channels;
	mtap->samplerate = sfinfo->samplerate;

	/* try to find a read function */
	switch (sfinfo->format & 0xFF) {
        case SF_FORMAT_PCM_16:
        case SF_FORMAT_PCM_U8:
        case SF_FORMAT_ULAW:
        case SF_FORMAT_ALAW:
		mtap->samplewidth = 16;
		mtap->framesize = mtap->channels * 2;
		/* copy in some name */
		name = xmalloc(20);
		memcpy(name, "PCM", 3);
		name[3] = '\0';
		break;
        case SF_FORMAT_FLOAT:
        default:
		mtap->samplewidth = 32;
		mtap->framesize = mtap->channels * 4;
		/* copy in some name */
		name = xmalloc(20);
		memcpy(name, "PCM32", 5);
		name[5] = '\0';
		break;
	}
	mtap->name = name;
	mtap->endianness = 0;

	/* now assign */
	media_substream_type_properties(mss).aprops = mtap;
	media_substream_sread(mss) = media_sndfile_sread_audio;
	media_substream_srewind(mss) = media_sndfile_srewind_audio;

	/* keep the SNDFILE context */
	media_substream_data(mss) = sf;
	media_stream_data(ms) = sf;

	/* set me as driver indicator */
	media_stream_driver(ms) = MYSELF;
}


static sf_count_t
sndfile_vio_get_filelen(void* user_data)
{
	sound_data *sd = (sound_data*)user_data;

	return sd->length;
}
static sf_count_t
sndfile_vio_seek(sf_count_t offset, int whence, void* user_data)
{
	sound_data *sd = (sound_data*)user_data;

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
	sound_data *sd = (sound_data*)user_data;

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
	sound_data *sd = (sound_data*)user_data;

	memcpy(sd->data+sd->seek, ptr, count);
	sd->seek += count;
	return count;
}
static sf_count_t
sndfile_vio_tell(void* user_data)
{
	sound_data *sd = (sound_data*)user_data;

	return sd->seek;
}


static uint32_t
media_sndfile_sread_audio(Lisp_Media_Stream *ms, void *outbuf, uint32_t length)
{
/* read at most `length' frames into `outbuf' */
	/* libsndfile stuff */
	SNDFILE *sf;
	/* media stream stuff */
	media_substream *mss = media_stream_1sub(ms);
	mtype_audio_properties *mtap;

	/* check the integrity of the media stream */
	if (media_stream_driver(ms) != MYSELF)
		return 0;
	if (media_substream_type(mss) != MTYPE_AUDIO)
		return 0;

	/* fetch the SNDFILE context and our audio props */
	if (!(sf = media_stream_data(ms)))
		return 0;
	if (!(mtap = media_substream_type_properties(mss).aprops))
		return 0;

	if (mtap->samplewidth > 16)
		return sf_readf_float(sf, outbuf, length);
	else
		return sf_readf_short(sf, outbuf, length);
}

static void
media_sndfile_srewind_audio(Lisp_Media_Stream *ms)
{
/* rewind the stream to the first frame */
	/* libsndfile stuff */
	SNDFILE *sf;
	/* media stream stuff */
	media_substream *mss = media_stream_1sub(ms);

	/* check the integrity of the media stream */
	if (media_stream_driver(ms) != MYSELF)
		return;
	if (media_substream_type(mss) != MTYPE_AUDIO)
		return;

	/* fetch the SNDFILE context and our audio props */
	if (!(sf = media_stream_data(ms)))
		return;

	sf_seek(sf, 0, SEEK_SET);
}

#undef MYSELF
