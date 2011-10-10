/* play.c - play a sound file on the speaker
 **
 ** Copyright (C) 1989 by Jef Poskanzer.
 **
 ** Modified 24-May-91 by Jamie Zawinski (for Lucid Emacs).
 ** Modified 17-Dec-92 by Jamie Zawinski (largely rewritten for SunOS 4.1.3).
 **
 ** Permission to use, copy, modify, and distribute this software and its
 ** documentation for any purpose and without fee is hereby granted, provided
 ** that the above copyright notice appear in all copies and that both that
 ** copyright notice and this permission notice appear in supporting
 ** documentation.  This software is provided "as is" without express or
 ** implied warranty.
 */

/* Synched up with: Not in FSF. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if __STDC__ || defined(STDC_HEADERS)
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>		/* for open() */
#endif

#include <stdio.h>
#include <string.h>
#include <sys/fcntl.h>
#include <sys/file.h>

#include <multimedia/libaudio.h>
#include <multimedia/audio_device.h>

# include <config.h>
# include "lisp.h"
# include "sysdep.h"
# include <errno.h>
#include "syssignal.h"
# define perror(string) \
    message("audio: %s, %s ", string, strerror (errno))
# define warn(str) message ("audio: %s ", GETTEXT (str))

#include "nativesound.h"
#include "media-native.h"
#include "sound-sunplay.h"

Lisp_Object Qsunplay;		/* cannot be Qnative */

#define MYSELF ADRIVER_NATIVE

#if 0
static SIGTYPE(*sighup_handler) (int sig);
static SIGTYPE(*sigint_handler) (int sig);
static SIGTYPE sighandler(int sig);
#endif

#if 0
static int audio_fd;
#endif

#define audio_open()	open ("/dev/audio", (O_WRONLY | O_NONBLOCK), 0)

static int initialized_device_p;
static int reset_volume_p, reset_device_p;
static double old_volume;
static Audio_hdr dev_hdr;

#if 0
static int
init_device(int volume, unsigned char *data, int fd,
	    unsigned int *header_length)
{
#ifdef SUNOS4_0_3
	if (header_length)
		*header_length = 0;
	return 0;
#else
	Audio_hdr file_hdr;

	reset_volume_p = 0;
	reset_device_p = 0;

	if (data && fd)
		abort();	/* one or the other */

	if (AUDIO_SUCCESS != audio_get_play_config(audio_fd, &dev_hdr)) {
		perror("Not a valid audio device");
		return 1;
	}

	if (AUDIO_SUCCESS != (data
			      ? audio_decode_filehdr(data, &file_hdr,
						     header_length)
			      : audio_read_filehdr(fd, &file_hdr, 0, 0))) {
		if (data)
			perror("invalid audio data");
		else
			perror("invalid audio file");
		return 1;
	}

	audio_flush_play(audio_fd);

	if (!initialized_device_p || (0 != audio_cmp_hdr(&dev_hdr, &file_hdr))) {
		Audio_hdr new_hdr;
		new_hdr = file_hdr;
		reset_device_p = 1;
		initialized_device_p = 1;
		if (AUDIO_SUCCESS != audio_set_play_config(audio_fd, &new_hdr)) {
			char buf1[100], buf2[100], buf3[250];
			audio_enc_to_str(&file_hdr, buf1);
			audio_enc_to_str(&new_hdr, buf2);
			(void)snprintf(buf3, sizeof(buf3), "wanted %s, got %s", buf1, buf2);
			warn(buf3);
			return 1;
		}
	}

	if (volume < 0 || volume > 100) {
		char buf[255];
		int sz = sprintf(buf, sizeof(buf), "volume must be between 0 and 100 (not %d)",
				 volume);
		assert(sz>=0 && sz<sizeof(buf));
		warn(buf);
		return 1;
	}
	{
		/* set the volume; scale it to 0.0 - 1.0 */
		double V = (volume / 100.0);
		audio_get_play_gain(audio_fd, &old_volume);
		reset_volume_p = 1;
		audio_set_play_gain(audio_fd, &V);
	}

	return 0;
#endif
}

static void reset_device(int wait_p)
{
	if (wait_p)
		audio_drain(audio_fd, 1);
	else
		audio_flush_play(audio_fd);
	if (reset_device_p)
		audio_set_play_config(audio_fd, &dev_hdr);
	if (reset_volume_p)
		audio_set_play_gain(audio_fd, &old_volume);
}

void play_sound_file(char *sound_file, int volume)
{
	int rrtn, wrtn;
	unsigned char buf[255];
	int file_fd;

	audio_fd = audio_open();

	if (audio_fd < 0) {
		perror("open /dev/audio");
		return;
	}

	/* where to find the proto for signal()... */
	sighup_handler = (SIGTYPE(*)(int))signal(SIGHUP, sighandler);
	sigint_handler = (SIGTYPE(*)(int))signal(SIGINT, sighandler);

	file_fd = open(sound_file, O_RDONLY, 0);
	if (file_fd < 0) {
		perror(sound_file);
		goto END_OF_PLAY;
	}

	if (init_device(volume, (unsigned char *)0, file_fd, (unsigned int *)0))
		goto END_OF_PLAY;

	while (1) {
		rrtn = read(file_fd, (char *)buf, sizeof(buf));
		if (rrtn < 0) {
			perror("read");
			goto END_OF_PLAY;
		}
		if (rrtn == 0)
			break;

		while (1) {
			wrtn = write(audio_fd, (char *)buf, rrtn);
			if (wrtn < 0) {
				perror("write");
				goto END_OF_PLAY;
			}
			if (wrtn != 0)
				break;

			if (AUDIO_ERR_INTERRUPTED == audio_drain(audio_fd, 1))
				goto END_OF_PLAY;
		}
		if (wrtn != rrtn) {
			char warn_buf[255];
			int sz = sprintf(warn_buf, sizeof(warn_buf), "play: rrtn = %d, wrtn = %d", rrtn,
					 wrtn);
			assert(warn_buf>=0 && warn_buf<sizeof(warn_buf));
			warn(warn_buf);
			goto END_OF_PLAY;
		}
	}

      END_OF_PLAY:

	if (file_fd > 0)
		close(file_fd);

	if (audio_fd > 0) {
		reset_device(1);
		close(audio_fd);
	}

	signal(SIGHUP, sighup_handler);
	signal(SIGINT, sigint_handler);
}

int play_sound_data(unsigned char *data, int length, int volume)
{
	int wrtn, start = 0;
	unsigned int ilen;
	int result = 0;

	audio_fd = -1;

	if (length == 0)
		return 0;

	/* this is just to get a better error message */
	if (strncmp(".snd\0", (char *)data, 4)) {
		warn("Not valid audio data (bad magic number)");
		goto END_OF_PLAY;
	}
	if (length <= sizeof(Audio_hdr)) {
		warn("Not valid audio data (too short)");
		goto END_OF_PLAY;
	}

	audio_fd = audio_open();
	if (audio_fd < 0)
		return 0;

	/* where to find the proto for signal()... */
	sighup_handler = (SIGTYPE(*)(int))signal(SIGHUP, sighandler);
	sigint_handler = (SIGTYPE(*)(int))signal(SIGINT, sighandler);

	if (init_device(volume, data, 0, &ilen))
		goto END_OF_PLAY;

	data += (ilen << 2);
	length -= (ilen << 2);
	if (length <= 1)
		goto END_OF_PLAY;

	while (1) {
		wrtn = write(audio_fd, (char *)(data + start), length - start);
		if (wrtn < 0) {
			perror("write");
			goto END_OF_PLAY;
		}
		if (wrtn != 0) {
			start += wrtn;
			break;
		}
		if (AUDIO_ERR_INTERRUPTED == audio_drain(audio_fd, 1))
			goto END_OF_PLAY;
	}
	if (wrtn != length) {
		char buf[255];
		int sz = snprintf(buf, sizeof(buf),
				  "play: rrtn = %d, wrtn = %d", length, wrtn);
		assert(sz>=0 && sz < sizeof(buf));
		warn(buf);
		goto END_OF_PLAY;
	}

	result = 1;

      END_OF_PLAY:

	if (audio_fd > 0) {
		reset_device(1);
		close(audio_fd);
	}

	signal(SIGHUP, sighup_handler);
	signal(SIGINT, sigint_handler);

	return result;
}
#endif

#if 0
/* #### sigcontext doesn't exist in Solaris.  This should be updated
   to be correct for Solaris. */
static SIGTYPE sighandler(int sig)
{
	if (audio_fd > 0) {
		reset_device(0);
		close(audio_fd);
	}
	if (sig == SIGHUP && sighup_handler)
		sighup_handler(sig);
	else if (sig == SIGINT && sigint_handler)
		sigint_handler(sig);
	else
		exit(1);
}
#endif


static int
sound_native_audio_init(int audio_fd)
{
#ifdef SUNOS4_0_3
	return 1;
#else
	Audio_hdr file_hdr;

	reset_volume_p = 0;
	reset_device_p = 0;

	if (AUDIO_SUCCESS != audio_get_play_config(audio_fd, &dev_hdr)) {
		perror("Not a valid audio device");
		return 0;
	}

#if 0				/* eerks */
	if (AUDIO_SUCCESS != (data
			      ? audio_decode_filehdr(data, &file_hdr,
						     header_length)
			      : audio_read_filehdr(fd, &file_hdr, 0, 0))) {
		if (data)
			perror("invalid audio data");
		else
			perror("invalid audio file");
		return 0;
	}
#endif

	audio_flush_play(audio_fd);

#if 0
	if (!initialized_device_p || (0 != audio_cmp_hdr(&dev_hdr, &file_hdr))) {
		Audio_hdr new_hdr;
		new_hdr = file_hdr;
		reset_device_p = 1;
		initialized_device_p = 1;
		if (AUDIO_SUCCESS != audio_set_play_config(audio_fd, &new_hdr)) {
			char buf1[100], buf2[100], buf3[250];
			audio_enc_to_str(&file_hdr, buf1);
			audio_enc_to_str(&new_hdr, buf2);
			(void)snprintf(buf3, sizeof(buf3), "wanted %s, got %s", buf1, buf2);
			warn(buf3);
			return 0;
		}
	}
#endif

#if 0
	if (volume < 0 || volume > 100) {
		char buf[255];
		int sz = snprintf(buf, sizeof(buf),
				  "volume must be between 0 and 100 (not %d)",
				  volume);
		assert(sz>=0 && sz<sizeof(buf));
		warn(buf);
		return 0;
	}
#endif
	{
		/* set the volume; scale it to 0.0 - 1.0 */
		double V = 1.0; /* (volume / 100.0); */
		audio_get_play_gain(audio_fd, &old_volume);
		reset_volume_p = 1;
		audio_set_play_gain(audio_fd, &V);
	}

	return 1;
#endif
}

int sound_native_play_stream(Lisp_Object mt)
{
	/* stream stuff */
	mtype_audio_properties *mtap;
	Lisp_Media_Stream *ms;
	/* device stuff */
	Lisp_Object device;
	Lisp_Audio_Device *lad = NULL;
	sound_native_data *saod = NULL;
	/* native stuff */
	int audio_fd = -1;
	/* buffering */
	char *buffer = NULL;
	char *bptr;
	uint32_t len;
	int32_t natlen;
	int32_t written;

	int wrtn, start = 0;
	unsigned int ilen;
	int result = 0;

	/* unpack the media thread */
	device = XMEDIA_THREAD(mt)->device;
	ms = XMEDIA_STREAM(XMEDIA_THREAD(mt)->stream);

	/* unpack device */
	lad = get_audio_device(device);
	saod = get_audio_device_data(device);

	/* cannot use AO on incomplete or corrupt audio devices */
	if (lad == NULL ||
	    XAUDIO_DEVICE_DRIVER(device) != MYSELF)
		return 0;

	/* refuse to play non-audio */
	if (media_stream_type(ms) != MTYPE_AUDIO)
		return 0;
	mtap = media_stream_type_properties(ms).aprops;

	audio_fd = -1;
	audio_fd = audio_open();
	if (audio_fd < 0)
		return 0;

#if 0
	/* where to find the proto for signal()... */
	sighup_handler = (SIGTYPE(*)(int))signal(SIGHUP, sighandler);
	sigint_handler = (SIGTYPE(*)(int))signal(SIGINT, sighandler);
#endif

	if (!sound_native_audio_init(audio_fd))
		goto END_OF_PLAY;

	/* rewind the stream */
	media_stream_srewind(ms)(ms);

	/* play chunks of the stream */
	buffer = xmalloc_atomic(SOUND_MAX_AUDIO_FRAME_SIZE+1);
	while ((len = media_stream_sread(ms)(
			ms, buffer, mtap->samplerate)) > 0) {

		natlen = len * mtap->framesize;
		bptr = buffer;

		while (natlen > 0) {
			if ((written = write(audio_fd, bptr, natlen)) < 0) {
				perror("error in write");
				natlen = 0;
			} else if (written) {
				natlen -= written;
				bptr += written;
			}
			if (AUDIO_ERR_INTERRUPTED == audio_drain(audio_fd, 1))
				goto END_OF_PLAY;
		}
	}

END_OF_PLAY:

	if (audio_fd > 0) {
		reset_device(1);
		close(audio_fd);
	}

#if 0
	signal(SIGHUP, sighup_handler);
	signal(SIGINT, sigint_handler);
#endif

	return 1;
}

#undef MYSELF
