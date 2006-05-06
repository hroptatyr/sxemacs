/* nas.c --- XEmacs support for the Network Audio System server.
 *
 * Author: Richard Caley <R.Caley@ed.ac.uk>
 *
 * Copyright 1994 Free Software Foundation, Inc.
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this 
 * software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED 'AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/* Synched up with: Not in FSF. */

/* There are four compile-time options.
 *
 * XTOOLKIT	This will be part of an Xt program.
 * 
 * XTEVENTS	The playing will be supervised asynchronously by the Xt event
 *		loop.  If not set, playing will be completed within the call
 *		to play_file etc. 
 *
 * ROBUST_PLAY	Causes errors in nas to be caught.  This means that the
 *		program will attempt not to die if the nas server does.
 *
 * CACHE_SOUNDS	Causes the sounds to be played in buckets in the NAS
 *		server.  They are named by their comment field, or if that is
 *		empty by the filename, or for play_sound_data by a name made up
 *		from the sample itself.
 */

/* CHANGES:
 *	10/8/94, rjc	Changed names from netaudio to nas
 *			Added back asynchronous play if nas library has
 *			correct error facilities.
 *      4/11/94, rjc    Added wait_for_sounds to be called when user wants to
 *			be sure all play has finished.
 *      1998-10-01 rlt  Added support for WAVE files.
 *      2002-10-16      Jon Trulson modifed this to work with NAS releases
 *                      1.5f and higher.  We were using the private variable
 *                      SoundFileInfo that doesn't exist anymore.  But preserve
 *                      backward compatibility.  This will not work for some
 *                      versions of NAS around 1.5b to 1.5f or so.  Known to
 *                      work on 1.2p5 and 1.6.
 */

#ifdef emacs
#include <config.h>
#include "lisp.h"
#include "sysdep.h"
#include "syssignal.h"
#endif

#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stdio.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* NAS <= 1.2p5 defines {BIG,LITTLE}_ENDIAN in <audio/fileutil.h>,
   conflicting with GNU libc (at least); newer versions avoid this
   name space pollution.

   DO NOT USE THOSE MACROS in this file.  Use NAS_{BIG,LITTLE}_ENDIAN.

   It would be slightly more reliable to do this via configure, but that
   seems unnecessarily complex.
*/
#undef LITTLE_ENDIAN
#undef BIG_ENDIAN

#include "media.h"
#include "sound-nas.h"

#include <audio/audiolib.h>
#include <audio/soundlib.h>
#include <audio/snd.h>
#include <audio/wave.h>
#include <audio/fileutil.h>

#if 0				/* we do not support such old NASes */
/* NAS <= 1.2p5 <audio/fileutil.h> doesn't define the NAS_ versions */
#ifndef NAS_LITTLE_ENDIAN
#define NAS_LITTLE_ENDIAN LITTLE_ENDIAN
#define NAS_BIG_ENDIAN BIG_ENDIAN
#endif
#endif

#ifdef emacs

#    define XTOOLKIT
#    define XTEVENTS
#    define ROBUST_PLAY
#    define CACHE_SOUNDS

    /*
     * For old NAS libraries, force playing to be synchronous
     * and declare the long jump point locally.
     */

#    if defined (NAS_NO_ERROR_JUMP)

#	undef XTEVENTS

#	include <setjmp.h>
jmp_buf AuXtErrorJump;
#    endif

     /* The GETTEXT is correct. --ben */
#    define warn(str) warn_when_safe (Qnas, Qwarning, "nas: %s ", GETTEXT (str))

#    define play_sound_file nas_play_sound_file
#    define play_sound_data nas_play_sound_data
#    define wait_for_sounds nas_wait_for_sounds
#    define init_play       nas_init_play
#    define close_down_play nas_close_down_play

#else				/* !emacs */
#    define warn(str) fprintf (stderr, "%s\n", (str))
#endif				/* emacs */

#ifdef XTOOLKIT
#    include <X11/Intrinsic.h>
#    include <audio/Xtutil.h>
#endif

#if defined (ROBUST_PLAY)
static AuBool CatchIoErrorAndJump(AuServer * aud);
static AuBool CatchErrorAndJump(AuServer * aud, AuErrorEvent * event);
SIGTYPE sigpipe_handle(int signo);
#endif

extern Lisp_Object Vsynchronous_sounds;

static AuServer *aud;

/* count of sounds currently being played. */
static int sounds_in_play;

#ifdef XTOOLKIT
static Display *aud_server;
static XtInputId input_id;
#else
static char *aud_server;
#endif				/* XTOOLKIT */

Lisp_Object Qnas;
#define MYSELF ADRIVER_NAS


char *init_play(
#ifdef XTOOLKIT
		       Display * display
#else
		       char *server
#endif
    );
char *init_play(
#ifdef XTOOLKIT
		       Display * display
#else
		       char *server
#endif
    )
{
	char *err_message;
	SIGTYPE(*old_sigpipe) (int);

#ifdef XTOOLKIT
	char *server = DisplayString(display);
	XtAppContext app_context = XtDisplayToApplicationContext(display);

	aud_server = display;
#else

	aud_server = server;
#endif

#ifdef ROBUST_PLAY
	old_sigpipe = signal(SIGPIPE, sigpipe_handle);
	if (setjmp(AuXtErrorJump)) {
		signal(SIGPIPE, old_sigpipe);
#ifdef emacs
		start_interrupts();
#endif
		return "error in NAS";
	}
#endif

#if defined (ROBUST_PLAY) && !defined (NAS_NO_ERROR_JUMP)
	AuDefaultIOErrorHandler = CatchIoErrorAndJump;
	AuDefaultErrorHandler = CatchErrorAndJump;
#endif

#ifdef emacs
	stop_interrupts();
#endif
	aud = AuOpenServer(server, 0, NULL, 0, NULL, &err_message);
#ifdef emacs
	start_interrupts();
#endif
	if (!aud) {
#ifdef ROBUST_PLAY
		signal(SIGPIPE, old_sigpipe);
#endif
		if (err_message == NULL)
			return "Can't connect to audio server";
		else
			return err_message;
	}
#if defined (ROBUST_PLAY)
# if defined (NAS_NO_ERROR_JUMP)
	aud->funcs.ioerror_handler = CatchIoErrorAndJump;
	aud->funcs.error_handler = CatchErrorAndJump;
# else				/* !NAS_NO_ERROR_JUMP */
	AuDefaultIOErrorHandler = NULL;
	AuDefaultErrorHandler = NULL;
# endif
#endif

#ifdef XTEVENTS
	input_id = AuXtAppAddAudioHandler(app_context, aud);
#endif

#ifdef CACHE_SOUNDS
	AuSetCloseDownMode(aud, AuCloseDownRetainPermanent, NULL);
#endif

#ifdef ROBUST_PLAY
	signal(SIGPIPE, old_sigpipe);
#endif

	sounds_in_play = 0;

	return NULL;
}

#if 0
static void close_down_play(void)
{
	AuCloseServer(aud);
	warn("disconnected from audio server");
}
#endif

 /********************************************************************\
 *                                                                    *
 * Callback which is run when the sound finishes playing.             *
 *                                                                    *
 \********************************************************************/

#if 0
static void
doneCB(AuServer * auserver,
       AuEventHandlerRec * handler, AuEvent * ev, AuPointer data)
{
	int *in_play_p = (int *)data;

	(*in_play_p)--;
}
#endif

#ifdef CACHE_SOUNDS

 /********************************************************************\
 *                                                                    *
 * Play a sound by playing the relevant bucket, if any or             *
 * downloading it if not.                                             *
 *                                                                    *
 \********************************************************************/

#if 0
static void do_caching_play(Sound s, int volume, unsigned char *buf)
{
	AuBucketAttributes *list, b;
	AuBucketID id;
	int n;

	AuSetString(AuBucketDescription(&b),
		    AuStringLatin1, strlen(SoundComment(s)), SoundComment(s));

	list = AuListBuckets(aud, AuCompCommonDescriptionMask, &b, &n, NULL);

	if (list == NULL) {
		AuPointer my_buf;

		if (buf == NULL) {
			if ((my_buf =
			     (AuPointer) malloc(SoundNumBytes(s))) == NULL) {
				return;
			}

			if (SoundReadFile((char *)my_buf, SoundNumBytes(s), s)
			    != SoundNumBytes(s)) {
				free(my_buf);
				return;
			}
		} else
			my_buf = (AuPointer) buf;

		id = AuSoundCreateBucketFromData(aud,
						 s,
						 my_buf,
						 AuAccessAllMasks, NULL, NULL);
		if (buf == NULL)
			free(my_buf);
	} else {		/* found cached sound */

		id = AuBucketIdentifier(list);
		AuFreeBucketAttributes(aud, n, list);
	}

	sounds_in_play++;

	AuSoundPlayFromBucket(aud,
			      id,
			      AuNone,
			      AuFixedPointFromFraction(volume, 100),
			      doneCB, (AuPointer) & sounds_in_play,
			      1, NULL, NULL, NULL, NULL);

}
#endif	/* 0 */
#endif	/* CACHE_SOUNDS */

void wait_for_sounds(void);
void wait_for_sounds(void)
{
	AuEvent ev;

	while (sounds_in_play > 0) {
		AuNextEvent(aud, AuTrue, &ev);
		AuDispatchEvent(aud, &ev);
	}
}

#if 0
int play_sound_file(char *sound_file, int volume)
{
	SIGTYPE(*old_sigpipe) (int);

#ifdef ROBUST_PLAY
	old_sigpipe = signal(SIGPIPE, sigpipe_handle);
	if (setjmp(AuXtErrorJump)) {
		signal(SIGPIPE, old_sigpipe);
		return 0;
	}
#endif

	if (aud == NULL) {
		if (aud_server != NULL) {
			char *m;
			/* attempt to reconect */
			if ((m = init_play(aud_server)) != NULL) {

#ifdef ROBUST_PLAY
				signal(SIGPIPE, old_sigpipe);
#endif
				return 0;
			}
		} else {
			warn("Attempt to play with no audio init\n");
#ifdef ROBUST_PLAY
			signal(SIGPIPE, old_sigpipe);
#endif
			return 0;
		}
	}
#ifndef CACHE_SOUNDS
	sounds_in_play++;
	AuSoundPlayFromFile(aud,
			    sound_file,
			    AuNone,
			    AuFixedPointFromFraction(volume, 100),
			    doneCB, (AuPointer) & sounds_in_play,
			    NULL, NULL, NULL, NULL);
#else
	/* Cache the sounds in buckets on the server */

	{
		Sound s;

		if ((s = SoundOpenFileForReading(sound_file)) == NULL) {
#ifdef ROBUST_PLAY
			signal(SIGPIPE, old_sigpipe);
#endif
			return 0;
		}

		if (SoundComment(s) == NULL || SoundComment(s)[0] == '\0') {
			SoundComment(s) = FileCommentFromFilename(sound_file);
		}

		do_caching_play(s, volume, NULL);

		SoundCloseFile(s);

	}
#endif				/* CACHE_SOUNDS */

#ifndef XTEVENTS
	wait_for_sounds();
#else
	if (!NILP(Vsynchronous_sounds)) {
		wait_for_sounds();
	}
#endif

#ifdef ROBUST_PLAY
	signal(SIGPIPE, old_sigpipe);
#endif

	return 1;
}
#endif


#if defined (ROBUST_PLAY)

 /********************************************************************\
 *                                                                    *
 * Code to protect the client from server shutdowns.                  *
 *                                                                    *
 * This is unbelievably horrible.                                     *
 *                                                                    *
 \********************************************************************/

static AuBool CatchIoErrorAndJump(AuServer * old_aud)
{
	if (old_aud)
		warn("Audio Server connection broken");
	else
		warn("Audio Server connection broken because of signal");

#ifdef XTEVENTS
#ifdef XTOOLKIT
	{
		AuXtAppRemoveAudioHandler(aud, input_id);
	}
#endif

	if (aud)
		AuCloseServer(aud);
	aud = NULL;
	sounds_in_play = 0;

	longjmp(AuXtErrorJump, 1);

#else				/* not XTEVENTS */

	if (aud)
		AuCloseServer(aud);
	aud = NULL;
	sounds_in_play = 0;
	longjmp(AuXtErrorJump, 1);

#endif				/* XTEVENTS */
	return 0;
}

SIGTYPE sigpipe_handle(int signo)
{
	CatchIoErrorAndJump(NULL);
}

static AuBool CatchErrorAndJump(AuServer * old_aud, AuErrorEvent * event)
{
	return CatchIoErrorAndJump(old_aud);
}

#endif				/* ROBUST_PLAY */

 /********************************************************************\
 *                                                                    *
 * This code is here because the nas Sound library doesn't            *
 * support playing from a file buffered in memory. It's a fairly      *
 * direct translation of the file-based equivalent.                   *
 *                                                                    *
 * Since we don't have a filename, samples with no comment field      *
 * are named by a section of their content.                           *
 *                                                                    *
 \********************************************************************/

/* Create a name from the sound. */

#if 0
static char *NameFromData(const char *buf, int len)
{
	char name[9];
	int i;
	char *s;

	buf += len / 2;
	len -= len / 2;

	i = 0;
	while (i < 8 && len > 0) {
		while (*buf < 32 && len > 0) {
			buf++;
			len--;
		}
		name[i] = *buf;
		i++;
		buf++;
		len--;
	}

	name[i] = '\0';

	if (i == 8) {
		strcpy(s = (char *)malloc(10), name);
	} else {
		strcpy(s = (char *)malloc(15), "short sound");
	}

	return s;
}
#endif	/* 0 */

/* Stuff taken from wave.c from NAS.  Just like snd files, NAS can't
   read wave data from memory, so these functions do that for us. */

#define Err()		{ return NULL; }
#define readFourcc(_f)	dread(_f, sizeof(RIFF_FOURCC), 1)
#define cmpID(_x, _y)							      \
    strncmp((char *) (_x), (char *) (_y), sizeof(RIFF_FOURCC))
#define PAD2(_x)	(((_x) + 1) & ~1)

/* These functions here are for faking file I/O from buffer. */

#if 0
/* The "file" position */
static size_t file_posn;
/* The length of the "file" */
static size_t file_len;
/* The actual "file" data. */
static const void *file_data;

/* Like fopen, but for a buffer in memory */
static void dopen(const void *data, size_t length)
{
	file_data = data;
	file_len = length;
	file_posn = 0;
}

/* Like fread, but for a buffer in memory */
static int dread(void *buf, size_t size, size_t nitems)
{
	size_t nread = size * nitems;

	if (file_posn + nread <= file_len) {
		memcpy(buf, (char *)file_data + file_posn, size * nitems);
		file_posn += nread;
		return nitems;
	} else {
		return EOF;
	}
}

/* Like fgetc, but for a buffer in memory */
static int dgetc(void)
{
	if (file_posn < file_len)
		return ((char *)file_data)[file_posn++];
	else
		return -1;
}

/* Like fseek, but for a buffer in memory */
static int dseek(long offset, int from)
{
	if (from == 0)
		file_posn = offset;
	else if (from == 1)
		file_posn += offset;
	else if (from == 2)
		file_posn = file_len + offset;

	return 0;
}

/* Like ftell, but for a buffer in memory */
static long dtell(void)
{
	return file_posn;
}

/* Data buffer analogs for FileReadS and FileReadL in NAS. */

static unsigned short DataReadS(int swapit)
{
	unsigned short us;

	dread(&us, 2, 1);
	if (swapit)
		us = FileSwapS(us);
	return us;
}

static AuUint32 DataReadL(int swapit)
{
	AuUint32 ul;

	dread(&ul, 4, 1);
	if (swapit)
		ul = FileSwapL(ul);
	return ul;
}

static int readChunk(RiffChunk * c)
{
	int status;
	char n;

	if ((status = dread(c, sizeof(RiffChunk), 1)))
		if (NAS_BIG_ENDIAN)
			swapl(&c->ckSize, n);

	return status;
}
#endif	/* 0 */


/* NAS meets the new stream handlers */
sound_nas_data *sound_nas_create(Lisp_Object ignore)
{
	sound_nas_data *snd;

	/* -- initialise -- */
	snd = xnew_and_zero(sound_nas_data);

	snd->host = NULL;	/* could be an option */
	snd->buf_size = SOUND_NAS_BUF_SIZE;
	snd->buf_free = -1;

	/* open server */
	snd->aud = AuOpenServer(snd->host, 0, 0, 0, 0, 0);
	if (!snd->aud) {
		xfree(snd);
		return NULL; /* Could not contact NAS server */
	}

#if defined(NAS_NO_ERROR_JUMP)
	snd->aud->funcs.ioerror_handler = CatchIoErrorAndJump;
	snd->aud->funcs.error_handler = CatchErrorAndJump;
#else	/* !NAS_NO_ERROR_JUMP */
	AuDefaultIOErrorHandler = NULL;
	AuDefaultErrorHandler = NULL;
#endif

	return snd;
}

void sound_nas_finish(sound_nas_data *snd)
{
	AuCloseServer(snd->aud);
}

static void
sound_nas_wait_for_room(sound_nas_data *snd)
{
	AuEvent ev;

	while (snd->buf_free <= 0) {
		AuNextEvent(snd->aud, AuTrue, &ev);
		if (ev.type == AuEventTypeElementNotify) {
			AuElementNotifyEvent* event =
				(AuElementNotifyEvent*)(&ev);
			if (event->kind == AuElementNotifyKindLowWater)
				snd->buf_free = event->num_bytes;
			else if ((event->kind == AuElementNotifyKindState) &&
				 (event->cur_state == AuStatePause) &&
				 (event->reason != AuReasonUser))
				snd->buf_free = event->num_bytes;
		}
	}
}

int sound_nas_play_stream(media_subthread *mst)
{
	/* stream stuff */
	mtype_audio_properties *mtap;
	Lisp_Media_Stream *ms;
	media_substream *mss;
	Lisp_Media_Thread *mt;
	/* thread stuff */
	media_thread_play_state mtp;
	/* device stuff */
	Lisp_Object device;
	Lisp_Audio_Device *lad = NULL;
	sound_nas_data *snd = NULL;
	/* buffering */
	char *buffer;
	char *bptr;
	uint32_t len = 0;
	uint32_t naslen = 0;
	int resolution;
	/* nas stuff */
	AuElement elms[2];
	int i;

	/* unpack the media thread */
	mt = mst->up;
	device = mt->device;
	ms = XMEDIA_STREAM(mt->stream);
	mss = mst->substream;;

	/* unpack device */
	lad = get_audio_device(device);
	snd = get_audio_device_data(device);

	/* cannot use NAS on incomplete or corrupt audio devices */
	if (lad == NULL || snd == NULL ||
	    XAUDIO_DEVICE_DRIVER(device) != MYSELF)
		return 0;

	/* refuse to play non-audio */
	if (media_substream_type(mss) != MTYPE_AUDIO)
		return 0;
	mtap = media_substream_type_properties(mss).aprops;


	/* find physical output device */
	for (i = 0; i < AuServerNumDevices(snd->aud); i++)
	    if ((AuDeviceKind(AuServerDevice(snd->aud, i)) ==
		 AuComponentKindPhysicalOutput) &&
		(AuDeviceNumTracks(AuServerDevice(snd->aud, i)) ==
		 mtap->channels))
		    break;
	  
	if ((i == AuServerNumDevices(snd->aud)) || 
	    (!(snd->flow = AuCreateFlow(snd->aud, 0)))) {
		/* No physical output device found or flow creation failed. */
		fprintf(stderr, "no physical devices\n");
		AuCloseServer(snd->aud);
		return 0;
	}
	snd->dev = AuDeviceIdentifier(AuServerDevice(snd->aud, i));

	/* set up flow */
	AuMakeElementImportClient(&elms[0], mtap->samplerate,
				  AuFormatLinearSigned16LSB, mtap->channels,
				  AuTrue, snd->buf_size, snd->buf_size / 2,
				  0, 0);
	AuMakeElementExportDevice(&elms[1], 0, snd->dev,
				  mtap->samplerate, AuUnlimitedSamples, 0, 0);
	AuSetElements(snd->aud, snd->flow, AuTrue, 2, elms, 0);
	AuStartFlow(snd->aud, snd->flow, 0);
	snd->buf_free = -1;

	/* rewind the stream */
	media_substream_rewind(mss);

	/* play chunks of the stream */
	buffer = xmalloc(SOUND_MAX_AUDIO_FRAME_SIZE+1);
	resolution = (mtap->samplerate * MTPSTATE_REACT_TIME) / 1000000;
	mtp = mt->play_state;
	while (mtp != MTPSTATE_STOP &&
	       (len = media_substream_read(mss, buffer, resolution)) > 0) {

		naslen = len * mtap->framesize;
		bptr = buffer;

		while (naslen > 0) {
			/* Wait for room in buffer */
			sound_nas_wait_for_room(snd);
	  
			/* Partial transfer */
			if ((int32_t)naslen > snd->buf_free) {
				AuWriteElement(snd->aud, snd->flow, 0,
					       snd->buf_free, bptr, AuFalse, 0);
				naslen -= snd->buf_free;
				bptr += snd->buf_free;
				snd->buf_free = 0;
			}

			/* Final transfer */
			else {
				AuWriteElement(snd->aud, snd->flow, 0,
					       naslen, bptr, AuFalse, 0);
				snd->buf_free -= naslen;
				break;
			}
		}

		/* check if we changed state to pause */
		mtp = mt->play_state;
		while (mtp == MTPSTATE_PAUSE) {
			usleep(MTPSTATE_REACT_TIME);
			mtp = mt->play_state;
		}
	}

	/* -- Close and shutdown -- */
	AuStopFlow(snd->aud, snd->flow, 0);
	AuSync(snd->aud, AuTrue);
	xfree(buffer);
	return 1;
}

#undef MYSELF
