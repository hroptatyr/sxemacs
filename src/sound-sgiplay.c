/* Play sound using the SGI audio library
   written by Simon Leinen <simon@lia.di.epfl.ch>
   Copyright (C) 1992 Free Software Foundation, Inc.

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

#include <config.h>
#include "lisp.h"

#include <string.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <audio.h>
#include <netinet/in.h>		/* for ntohl() etc. */

/* Configuration options */

/* ability to parse Sun/NeXT (.au or .snd) audio file headers.  The
   .snd format supports all sampling rates and sample widths that are
   commonly used, as well as stereo.  It is also easy to parse. */
#ifndef HAVE_SND_FILES
#define HAVE_SND_FILES	1
#endif

/* support for eight-but mu-law encoding.  This is a useful compaction
   technique, and most sounds from the Sun universe are in this
   format. */
#ifndef HAVE_MULAW_8
#define HAVE_MULAW_8	1
#endif

/* if your machine is very slow, you have to use a table lookup to
   convert mulaw samples to linear.  This makes Emacs bigger so try to
   avoid it. */
#ifndef USE_MULAW_DECODE_TABLE
#define USE_MULAW_DECODE_TABLE	0
#endif

/* support for linear encoding -- useful if you want better quality.
   This enables 8, 16 and 24 bit wide samples. */
#ifndef HAVE_LINEAR
#define HAVE_LINEAR	1
#endif

/* support for 32 bit wide samples.  If you notice the difference
   between 32 and 24 bit samples, you must have very good ears.  Since
   the SGI audio library only supports 24 bit samples, each sample has
   to be shifted right by 8 bits anyway.  So you should probably just
   convert all your 32 bit audio files to 24 bit. */
#ifndef HAVE_LINEAR_32
#define HAVE_LINEAR_32	0
#endif

/* support for stereo sound.  Imagine the cool applications of this:
   finally you don't just hear a beep -- you also know immediately
   *where* something went wrong! Unfortunately the programming
   interface only takes a single volume argument so far. */
#ifndef HAVE_STEREO
#define HAVE_STEREO	1
#endif

/* the play routine can be interrupted between chunks, so we choose a
   small chunksize to keep the system responsive (2000 samples
   correspond to a quarter of a second for .au files.  If you
   HAVE_STEREO, the chunksize should probably be even. */
#define CHUNKSIZE 8000

/* the format assumed for header-less audio data.  The following
   assumes ".au" format (8000 samples/sec mono 8-bit mulaw). */
#define DEFAULT_SAMPLING_RATE	  8000
#define DEFAULT_CHANNEL_COUNT	     1
#define DEFAULT_FORMAT	      AFmulaw8

/* Exports */

/* all compilers on machines that have the SGI audio library
   understand prototypes, right? */

extern void play_sound_file(char *, int);
extern void play_sound_data(unsigned char *, int, int);

/* Data structures */

/* an AudioContext describes everything we want to know about how a
   particular sound snippet should be played.  It is split into three
   parts (device, port and buffer) for implementation reasons.  The
   device part corresponds to the state of the output device and must
   be reverted after playing the samples.  The port part corresponds
   to an ALport; we want to allocate a minimal number of these since
   there are only four of them system-wide, but on the other hand we
   can't use the same port for mono and stereo.  The buffer part
   corresponds to the sound data itself. */

typedef struct _AudioContextRec *AudioContext;

typedef struct {
	long device;
	int left_speaker_gain;
	int right_speaker_gain;
	long output_rate;
} AudioDeviceRec, *AudioDevice;

/* supported sound data formats */

typedef enum {
	AFunknown,
#if HAVE_MULAW_8
	AFmulaw8,
#endif
#if HAVE_LINEAR
	AFlinear8,
	AFlinear16,
	AFlinear24,
#if HAVE_LINEAR_32
	AFlinear32,
#endif
#endif
	AFillegal
} AudioFormat;

typedef struct {
	ALport port;
	AudioFormat format;
	unsigned nchan;
	unsigned queue_size;
} AudioPortRec, *AudioPort;

typedef struct {
	void *data;
	unsigned long size;
	void (*write_chunk_function) (void *, void *, AudioContext);
} AudioBufferRec, *AudioBuffer;

typedef struct _AudioContextRec {
	AudioDeviceRec device;
	AudioPortRec port;
	AudioBufferRec buffer;
} AudioContextRec;

#define ac_device		device.device
#define ac_left_speaker_gain	device.left_speaker_gain
#define ac_right_speaker_gain	device.right_speaker_gain
#define ac_output_rate		device.output_rate
#define ac_port			port.port
#define ac_format		port.format
#define ac_nchan		port.nchan
#define ac_queue_size		port.queue_size
#define ac_data			buffer.data
#define ac_size			buffer.size
#define ac_write_chunk_function	buffer.write_chunk_function

/* Forward declarations */

static Lisp_Object close_sound_file(Lisp_Object);
static AudioContext audio_initialize(unsigned char *, int, int);
static void play_internal(unsigned char *, int, AudioContext);
static void drain_audio_port(AudioContext);
static void write_mulaw_8_chunk(void *, void *, AudioContext);
static void write_linear_chunk(void *, void *, AudioContext);
static void write_linear_32_chunk(void *, void *, AudioContext);
static Lisp_Object restore_audio_port(Lisp_Object);
static AudioContext initialize_audio_port(AudioContext);
static int open_audio_port(AudioContext, AudioContext);
static void adjust_audio_volume(AudioDevice);
static void get_current_volumes(AudioDevice);
static int set_channels(ALconfig, unsigned);
static int set_output_format(ALconfig, AudioFormat);
static int parse_snd_header(void *, long, AudioContext);

/* are we looking at an NeXT/Sun audio header? */
#define LOOKING_AT_SND_HEADER_P(address) \
  (!strncmp(".snd", (char *)(address), 4))

static Lisp_Object close_sound_file(Lisp_Object closure)
{
	close(XINT(closure));
	return Qnil;
}

void play_sound_file(char *sound_file, int volume)
{
	int count = specpdl_depth();
	int input_fd;
	unsigned char buffer[CHUNKSIZE];
	int bytes_read;
	AudioContext ac = (AudioContext) 0;

	input_fd = open(sound_file, O_RDONLY);
	if (input_fd == -1)
		/* no error message -- this can't happen
		   because Fplay_sound_file has checked the
		   file for us. */
		return;

	record_unwind_protect(close_sound_file, make_int(input_fd));

	while ((bytes_read = read(input_fd, buffer, CHUNKSIZE)) > 0) {
		if (ac == (AudioContext) 0) {
			ac = audio_initialize(buffer, bytes_read, volume);
			if (ac == 0)
				return;
		} else {
			ac->ac_data = buffer;
			ac->ac_size = bytes_read;
		}
		play_internal(buffer, bytes_read, ac);
	}
	drain_audio_port(ac);
	unbind_to(count, Qnil);
}

static long saved_device_state[] = {
	AL_OUTPUT_RATE, 0,
	AL_LEFT_SPEAKER_GAIN, 0,
	AL_RIGHT_SPEAKER_GAIN, 0,
};

static Lisp_Object restore_audio_port(Lisp_Object closure)
{
	Lisp_Object *contents = XVECTOR_DATA(closure);
	saved_device_state[1] = XINT(contents[0]);
	saved_device_state[3] = XINT(contents[1]);
	saved_device_state[5] = XINT(contents[2]);
	ALsetparams(AL_DEFAULT_DEVICE, saved_device_state, 6);
	return Qnil;
}

void play_sound_data(unsigned char *data, int length, int volume)
{
	int count = specpdl_depth();
	AudioContext ac;

	ac = audio_initialize(data, length, volume);
	if (ac == (AudioContext) 0)
		return;
	play_internal(data, length, ac);
	drain_audio_port(ac);
	unbind_to(count, Qnil);
}

static AudioContext
audio_initialize(unsigned char *data, int length, int volume)
{
	Lisp_Object audio_port_state[3];
	static AudioContextRec desc;
	AudioContext ac;

	desc.ac_right_speaker_gain
	    = desc.ac_left_speaker_gain = volume * 256 / 100;
	desc.ac_device = AL_DEFAULT_DEVICE;

#if HAVE_SND_FILES
	if (LOOKING_AT_SND_HEADER_P(data)) {
		if (parse_snd_header(data, length, &desc) == -1)
			report_file_error("decoding .snd header", Qnil);
	} else
#endif
	{
		desc.ac_data = data;
		desc.ac_size = length;
		desc.ac_output_rate = DEFAULT_SAMPLING_RATE;
		desc.ac_nchan = DEFAULT_CHANNEL_COUNT;
		desc.ac_format = DEFAULT_FORMAT;
		desc.ac_write_chunk_function = write_mulaw_8_chunk;
	}

	/* Make sure that the audio port is reset to
	   its initial characteristics after exit */
	ALgetparams(desc.ac_device, saved_device_state,
		    sizeof(saved_device_state) / sizeof(long));
	audio_port_state[0] = make_int(saved_device_state[1]);
	audio_port_state[1] = make_int(saved_device_state[3]);
	audio_port_state[2] = make_int(saved_device_state[5]);
	record_unwind_protect(restore_audio_port,
			      Fvector(3, &audio_port_state[0]));

	ac = initialize_audio_port(&desc);
	desc = *ac;
	return ac;
}

static void play_internal(unsigned char *data, int length, AudioContext ac)
{
	unsigned char *limit;
	if (ac == (AudioContext) 0)
		return;

	data = (unsigned char *)ac->ac_data;
	limit = data + ac->ac_size;
	while (data < limit) {
		unsigned char *chunklimit = data + CHUNKSIZE;

		if (chunklimit > limit)
			chunklimit = limit;

		QUIT;

		(*ac->ac_write_chunk_function) (data, chunklimit, ac);
		data = chunklimit;
	}
}

static void drain_audio_port(AudioContext ac)
{
	while (ALgetfilled(ac->ac_port) > 0)
		sginap(1);
}

/* Methods to write a "chunk" from a buffer containing audio data to
   an audio port.  This may involve some conversion if the output
   device doesn't directly support the format the audio data is in. */

#if HAVE_MULAW_8

#if USE_MULAW_DECODE_TABLE
#include "libst.h"
#else				/* not USE_MULAW_DECODE_TABLE */
static int st_ulaw_to_linear(int u)
{
	static const short table[] =
	    { 0, 132, 396, 924, 1980, 4092, 8316, 16764 };
	int u1 = ~u;
	short exponent = (u1 >> 4) & 0x07;
	int mantissa = u1 & 0x0f;
	int unsigned_result = table[exponent] + (mantissa << (exponent + 3));
	return u1 & 0x80 ? -unsigned_result : unsigned_result;
}
#endif				/* not USE_MULAW_DECODE_TABLE */

static void write_mulaw_8_chunk(void *buffer, void *chunklimit, AudioContext ac)
{
	unsigned char *data = (unsigned char *)buffer;
	unsigned char *limit = (unsigned char *)chunklimit;
	short *obuf, *bufp;
	long n_samples = limit - data;

	obuf = alloca_array(short, n_samples);
	bufp = &obuf[0];

	while (data < limit)
		*bufp++ = st_ulaw_to_linear(*data++);
	ALwritesamps(ac->ac_port, obuf, n_samples);
}
#endif				/* HAVE_MULAW_8 */

#if HAVE_LINEAR
static void write_linear_chunk(void *data, void *limit, AudioContext ac)
{
	unsigned n_samples;

	switch (ac->ac_format) {
	case AFlinear16:
		n_samples = (short *)limit - (short *)data;
		break;
	case AFlinear8:
		n_samples = (char *)limit - (char *)data;
		break;
	default:
		n_samples = (long *)limit - (long *)data;
		break;
	}
	ALwritesamps(ac->ac_port, data, (long)n_samples);
}

#if HAVE_LINEAR_32
static void
write_linear_32_chunk(void *buffer, void *chunklimit, AudioContext ac)
{
	long *data = (long *)buffer;
	long *limit = (long *)chunklimit;
	long *obuf, *bufp;
	long n_samples = limit - data;

	obuf = alloca_array(long, n_samples);
	bufp = &obuf[0];

	while (data < limit)
		*bufp++ = *data++ >> 8;
	ALwritesamps(ac->ac_port, obuf, n_samples);
}
#endif				/* HAVE_LINEAR_32 */
#endif				/* HAVE_LINEAR */

static AudioContext initialize_audio_port(AudioContext desc)
{
	/* we can't use the same port for mono and stereo */
	static AudioContextRec mono_port_state = { {0, 0, 0, 0},
	{(ALport) 0, AFunknown, 1, 0},
	{(void *)0, (unsigned long)0}
	};
#if HAVE_STEREO
	static AudioContextRec stereo_port_state = { {0, 0, 0, 0},
	{(ALport) 0, AFunknown, 2, 0},
	{(void *)0, (unsigned long)0}
	};
	static AudioContext return_ac;

	switch (desc->ac_nchan) {
	case 1:
		return_ac = &mono_port_state;
		break;
	case 2:
		return_ac = &stereo_port_state;
		break;
	default:
		return (AudioContext) 0;
	}
#else				/* not HAVE_STEREO */
	static AudioContext return_ac = &mono_port_state;
#endif				/* not HAVE_STEREO */

	return_ac->device = desc->device;
	return_ac->buffer = desc->buffer;
	return_ac->ac_format = desc->ac_format;
	return_ac->ac_queue_size = desc->ac_queue_size;

	if (return_ac->ac_port == (ALport) 0) {
		if ((open_audio_port(return_ac, desc)) == -1) {
			report_file_error("Open audio port", Qnil);
			return (AudioContext) 0;
		}
	} else {
		ALconfig config = ALgetconfig(return_ac->ac_port);
		int changed = 0;
		long params[2];

		params[0] = AL_OUTPUT_RATE;
		ALgetparams(return_ac->ac_device, params, 2);
		return_ac->ac_output_rate = params[1];

		if (return_ac->ac_output_rate != desc->ac_output_rate) {
			return_ac->ac_output_rate = params[1] =
			    desc->ac_output_rate;
			ALsetparams(return_ac->ac_device, params, 2);
		}
		if ((changed =
		     set_output_format(config, return_ac->ac_format)) == -1)
			return (AudioContext) 0;
		return_ac->ac_format = desc->ac_format;
		if (changed)
			ALsetconfig(return_ac->ac_port, config);
	}
	return_ac->ac_write_chunk_function = desc->ac_write_chunk_function;
	get_current_volumes(&return_ac->device);
	if (return_ac->ac_left_speaker_gain != desc->ac_left_speaker_gain
	    || return_ac->ac_right_speaker_gain != desc->ac_right_speaker_gain)
		adjust_audio_volume(&desc->device);
	return return_ac;
}

static int open_audio_port(AudioContext return_ac, AudioContext desc)
{
	ALconfig config = ALnewconfig();
	long params[2];

	adjust_audio_volume(&desc->device);
	return_ac->ac_left_speaker_gain = desc->ac_left_speaker_gain;
	return_ac->ac_right_speaker_gain = desc->ac_right_speaker_gain;
	params[0] = AL_OUTPUT_RATE;
	params[1] = desc->ac_output_rate;
	ALsetparams(desc->ac_device, params, 2);
	return_ac->ac_output_rate = desc->ac_output_rate;
	if (set_channels(config, desc->ac_nchan) == -1)
		return -1;
	return_ac->ac_nchan = desc->ac_nchan;
	if (set_output_format(config, desc->ac_format) == -1)
		return -1;
	return_ac->ac_format = desc->ac_format;
	ALsetqueuesize(config, (long)CHUNKSIZE);
	return_ac->ac_port = ALopenport("SXEmacs audio output", "w", config);
	ALfreeconfig(config);
	if (return_ac->ac_port == 0) {
		report_file_error("Opening audio output port", Qnil);
		return -1;
	}
	return 0;
}

static int set_channels(ALconfig config, unsigned int nchan)
{
	switch (nchan) {
	case 1:
		ALsetchannels(config, AL_MONO);
		break;
#if HAVE_STEREO
	case 2:
		ALsetchannels(config, AL_STEREO);
		break;
#endif				/* HAVE_STEREO */
	default:
		report_file_error("Unsupported channel count",
				  Fcons(make_int(nchan), Qnil));
		return -1;
	}
	return 0;
}

static int set_output_format(ALconfig config, AudioFormat format)
{
	long samplesize;
	long old_samplesize;

	switch (format) {
#if HAVE_MULAW_8
	case AFmulaw8:
#endif
#if HAVE_LINEAR
	case AFlinear16:
#endif
#if HAVE_MULAW_8 || HAVE_LINEAR
		samplesize = AL_SAMPLE_16;
		break;
#endif
#if HAVE_LINEAR
	case AFlinear8:
		samplesize = AL_SAMPLE_8;
		break;
	case AFlinear24:
#if HAVE_LINEAR_32
	case AFlinear32:
		samplesize = AL_SAMPLE_24;
		break;
#endif
#endif
	default:
		report_file_error("Unsupported audio format",
				  Fcons(make_int(format), Qnil));
		return -1;
	}
	old_samplesize = ALgetwidth(config);
	if (old_samplesize == samplesize)
		return 0;
	ALsetwidth(config, samplesize);
	return 1;
}

static void adjust_audio_volume(AudioDevice device)
{
	long params[4];
	params[0] = AL_LEFT_SPEAKER_GAIN;
	params[1] = device->left_speaker_gain;
	params[2] = AL_RIGHT_SPEAKER_GAIN;
	params[3] = device->right_speaker_gain;
	ALsetparams(device->device, params, 4);
}

static void get_current_volumes(AudioDevice device)
{
	long params[4];
	params[0] = AL_LEFT_SPEAKER_GAIN;
	params[2] = AL_RIGHT_SPEAKER_GAIN;
	ALgetparams(device->device, params, 4);
	device->left_speaker_gain = params[1];
	device->right_speaker_gain = params[3];
}

#if HAVE_SND_FILES

/* Parsing .snd (NeXT/Sun) headers */

typedef struct {
	int magic;
	int dataLocation;
	int dataSize;
	int dataFormat;
	int samplingRate;
	int channelCount;
	char info[4];
} SNDSoundStruct;
#define SOUND_TO_HOST_INT(x) ntohl(x)

typedef enum {
	SND_FORMAT_FORMAT_UNSPECIFIED,
	SND_FORMAT_MULAW_8,
	SND_FORMAT_LINEAR_8,
	SND_FORMAT_LINEAR_16,
	SND_FORMAT_LINEAR_24,
	SND_FORMAT_LINEAR_32,
	SND_FORMAT_FLOAT,
	SND_FORMAT_DOUBLE,
	SND_FORMAT_INDIRECT,
	SND_FORMAT_NESTED,
	SND_FORMAT_DSP_CODE,
	SND_FORMAT_DSP_DATA_8,
	SND_FORMAT_DSP_DATA_16,
	SND_FORMAT_DSP_DATA_24,
	SND_FORMAT_DSP_DATA_32,
	SND_FORMAT_DSP_unknown_15,
	SND_FORMAT_DISPLAY,
	SND_FORMAT_MULAW_SQUELCH,
	SND_FORMAT_EMPHASIZED,
	SND_FORMAT_COMPRESSED,
	SND_FORMAT_COMPRESSED_EMPHASIZED,
	SND_FORMAT_DSP_COMMANDS,
	SND_FORMAT_DSP_COMMANDS_SAMPLES
} SNDFormatCode;

static int parse_snd_header(void *header, long length, AudioContext desc)
{
#define hp ((SNDSoundStruct *) (header))
	long limit;

#if HAVE_LINEAR
	desc->ac_write_chunk_function = write_linear_chunk;
#endif
	switch ((SNDFormatCode) SOUND_TO_HOST_INT(hp->dataFormat)) {
#if HAVE_MULAW_8
	case SND_FORMAT_MULAW_8:
		desc->ac_format = AFmulaw8;
		desc->ac_write_chunk_function = write_mulaw_8_chunk;
		break;
#endif
#if HAVE_LINEAR
	case SND_FORMAT_LINEAR_8:
		desc->ac_format = AFlinear8;
		break;
	case SND_FORMAT_LINEAR_16:
		desc->ac_format = AFlinear16;
		break;
	case SND_FORMAT_LINEAR_24:
		desc->ac_format = AFlinear24;
		break;
#endif
#if HAVE_LINEAR_32
	case SND_FORMAT_LINEAR_32:
		desc->ac_format = AFlinear32;
		desc->ac_write_chunk_function = write_linear_32_chunk;
		break;
#endif
	default:
		desc->ac_format = AFunknown;
	}
	desc->ac_output_rate = SOUND_TO_HOST_INT(hp->samplingRate);
	desc->ac_nchan = SOUND_TO_HOST_INT(hp->channelCount);
	desc->ac_data = (char *)header + SOUND_TO_HOST_INT(hp->dataLocation);
	limit = (char *)header + length - (char *)desc->ac_data;
	desc->ac_size = SOUND_TO_HOST_INT(hp->dataSize);
	if (desc->ac_size > limit)
		desc->ac_size = limit;
	return 0;
#undef hp
}
#endif				/* HAVE_SND_FILES */
