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

#include <audio/audiolib.h>
#include <audio/soundlib.h>
#include <audio/snd.h>
#include <audio/wave.h>
#include <audio/fileutil.h>

/* NAS <= 1.2p5 <audio/fileutil.h> doesn't define the NAS_ versions */
#ifndef NAS_LITTLE_ENDIAN
#define NAS_LITTLE_ENDIAN LITTLE_ENDIAN
#define NAS_BIG_ENDIAN BIG_ENDIAN
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

#else /* !emacs */
#    define warn(str) fprintf (stderr, "%s\n", (str))
#endif /* emacs */

#ifdef XTOOLKIT
#    include <X11/Intrinsic.h>
#    include <audio/Xtutil.h>
#endif

#if defined (ROBUST_PLAY)
static AuBool CatchIoErrorAndJump (AuServer *aud);
static AuBool CatchErrorAndJump (AuServer *aud, AuErrorEvent *event);
SIGTYPE sigpipe_handle (int signo);
#endif

extern Lisp_Object Vsynchronous_sounds;

static Sound SoundOpenDataForReading (unsigned char *data, int length);

static AuServer       *aud;

/* count of sounds currently being played. */
static int sounds_in_play;


#ifdef XTOOLKIT
static Display *aud_server;
static XtInputId input_id;
#else
static char *aud_server;
#endif /* XTOOLKIT */

char *
init_play (
#ifdef XTOOLKIT
	   Display *display
#else
	   char *server
#endif
	   );
char *
init_play (
#ifdef XTOOLKIT
	   Display *display
#else
	   char *server
#endif
	   )
{
  char *err_message;
  SIGTYPE (*old_sigpipe) (int);

#ifdef XTOOLKIT
  char * server = DisplayString (display);
  XtAppContext app_context = XtDisplayToApplicationContext (display);

  aud_server = display;
#else

  aud_server = server;
#endif

#ifdef ROBUST_PLAY
  old_sigpipe = signal (SIGPIPE, sigpipe_handle);
  if (setjmp (AuXtErrorJump))
    {
      signal (SIGPIPE, old_sigpipe);
#ifdef emacs
      start_interrupts ();
#endif  
      return "error in NAS";
    }
#endif

#if defined (ROBUST_PLAY) && !defined (NAS_NO_ERROR_JUMP)
  AuDefaultIOErrorHandler = CatchIoErrorAndJump;
  AuDefaultErrorHandler = CatchErrorAndJump;
#endif

#ifdef emacs
  stop_interrupts ();
#endif  
  aud = AuOpenServer (server, 0, NULL, 0, NULL, &err_message);
#ifdef emacs
  start_interrupts ();
#endif  
  if (!aud)
    {
#ifdef ROBUST_PLAY
      signal (SIGPIPE, old_sigpipe);
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
# else /* !NAS_NO_ERROR_JUMP */
  AuDefaultIOErrorHandler = NULL;
  AuDefaultErrorHandler = NULL;
# endif
#endif

#ifdef XTEVENTS
  input_id = AuXtAppAddAudioHandler (app_context, aud); 
#endif

#ifdef CACHE_SOUNDS
  AuSetCloseDownMode (aud, AuCloseDownRetainPermanent, NULL);
#endif

#ifdef ROBUST_PLAY
  signal (SIGPIPE, old_sigpipe);
#endif

  sounds_in_play = 0;

  return NULL;
}

static void
close_down_play (void)

{
  AuCloseServer (aud);
  warn ("disconnected from audio server");
}

 /********************************************************************\
 *                                                                    *
 * Callback which is run when the sound finishes playing.             *
 *                                                                    *
 \********************************************************************/

static void
doneCB (AuServer       *auserver,
	AuEventHandlerRec *handler,
	AuEvent        *ev,
	AuPointer       data)
{
  int         *in_play_p = (int *) data;

  (*in_play_p) --;
}

#ifdef CACHE_SOUNDS

 /********************************************************************\
 *                                                                    *
 * Play a sound by playing the relevant bucket, if any or             *
 * downloading it if not.                                             *
 *                                                                    *
 \********************************************************************/

static void
do_caching_play (Sound s,
		 int volume,
		 unsigned char *buf)

{
  AuBucketAttributes *list, b;
  AuBucketID      id;
  int n;

  AuSetString (AuBucketDescription (&b),
	       AuStringLatin1, strlen (SoundComment (s)), SoundComment (s));

  list = AuListBuckets (aud, AuCompCommonDescriptionMask, &b, &n, NULL);

  if (list == NULL)
    {
      AuPointer my_buf;

      if (buf==NULL)
	{
	  if ((my_buf= (AuPointer) malloc (SoundNumBytes (s)))==NULL)
	    {
	      return;
	    }

	  if (SoundReadFile ((char *) my_buf, SoundNumBytes (s), s) != SoundNumBytes (s))
	    {
	      free (my_buf);
	      return;
	    }
	}
      else
	my_buf = (AuPointer) buf;

      id = AuSoundCreateBucketFromData (aud, 
					s,
					my_buf,
					AuAccessAllMasks, 
					NULL,
					NULL);
      if (buf == NULL)
	free (my_buf);
    }
  else /* found cached sound */
    {
      id = AuBucketIdentifier (list);
      AuFreeBucketAttributes (aud, n, list);
    }

  sounds_in_play++;

  AuSoundPlayFromBucket (aud, 
			 id, 
			 AuNone,
			 AuFixedPointFromFraction (volume, 100), 
			 doneCB, (AuPointer) &sounds_in_play,
			 1,
			 NULL, NULL,
			 NULL, NULL);

}
#endif /* CACHE_SOUNDS */


void wait_for_sounds (void);
void 
wait_for_sounds (void)

{
  AuEvent         ev;

  while (sounds_in_play>0)
    {
      AuNextEvent (aud, AuTrue, &ev);
      AuDispatchEvent (aud, &ev);
    }
}

int play_sound_file (char *sound_file, int volume);
int
play_sound_file (char *sound_file,
		 int volume)
{
  SIGTYPE (*old_sigpipe) (int);

#ifdef ROBUST_PLAY
  old_sigpipe=signal (SIGPIPE, sigpipe_handle);
  if (setjmp (AuXtErrorJump))
    {
      signal (SIGPIPE, old_sigpipe);
      return 0;
    }
#endif

  if (aud==NULL) {
    if (aud_server != NULL)
      {
	char *m;
	/* attempt to reconect */
	if ((m=init_play (aud_server))!= NULL)
	  {

#ifdef ROBUST_PLAY
	    signal (SIGPIPE, old_sigpipe);
#endif
	    return 0;
	  }
      }
    else
      {
	warn ("Attempt to play with no audio init\n");
#ifdef ROBUST_PLAY
	signal (SIGPIPE, old_sigpipe);
#endif
	return 0;
      }
  }

#ifndef CACHE_SOUNDS
  sounds_in_play++;
  AuSoundPlayFromFile (aud,
		       sound_file,
		       AuNone,
		       AuFixedPointFromFraction (volume,100),
		       doneCB, (AuPointer) &sounds_in_play,
		       NULL,
		       NULL,
		       NULL,
		       NULL);
#else
  /* Cache the sounds in buckets on the server */

  {
    Sound s;

    if ((s = SoundOpenFileForReading (sound_file))==NULL)
      {
#ifdef ROBUST_PLAY
	signal (SIGPIPE, old_sigpipe);
#endif
	return 0;
      }

    if (SoundComment (s) == NULL || SoundComment (s)[0] == '\0')
      {
	SoundComment (s) = FileCommentFromFilename (sound_file);
      }

    do_caching_play (s, volume, NULL);

    SoundCloseFile (s);

  }
#endif /* CACHE_SOUNDS */

#ifndef XTEVENTS
  wait_for_sounds ();
#else
  if (!NILP (Vsynchronous_sounds))
    {
      wait_for_sounds ();
    }
#endif

#ifdef ROBUST_PLAY
  signal (SIGPIPE, old_sigpipe);
#endif

  return 1;
}

int play_sound_data (unsigned char *data, int length, int volume);
int
play_sound_data (unsigned char *data,
		 int length, 
		 int volume)
{
  Sound s;
  int offset;
  SIGTYPE (*old_sigpipe) (int);

#if !defined (XTEVENTS)
  AuEvent         ev;
#endif

#ifdef ROBUST_PLAY
  old_sigpipe = signal (SIGPIPE, sigpipe_handle);
  if (setjmp (AuXtErrorJump) !=0)
    {
      signal (SIGPIPE, old_sigpipe);
      return 0;
    }
#endif


  if (aud == NULL) {
    if (aud_server != NULL)
      {
	char *m;
	/* attempt to reconect */
	if ((m = init_play (aud_server)) != NULL)
	  {
#ifdef ROBUST_PLAY
	    signal (SIGPIPE, old_sigpipe);
#endif
	    return 0;
	  }
      }
    else
      {
	warn ("Attempt to play with no audio init\n");
#ifdef ROBUST_PLAY
	signal (SIGPIPE, old_sigpipe);
#endif
	return 0;
      }
  }

  if ((s=SoundOpenDataForReading (data, length))==NULL)
    {
      warn ("unknown sound type");
#ifdef ROBUST_PLAY
      signal (SIGPIPE, old_sigpipe);
#endif
      return 0;
    }

  if (SoundFileFormat (s) == SoundFileFormatSnd)
    {
      /* hack, hack */
      offset = ((SndInfo *) (s->formatInfo))->h.dataOffset;
    }
  else if (SoundFileFormat (s) == SoundFileFormatWave)
    {
      offset = ((WaveInfo *) (s->formatInfo))->dataOffset;
    }
  else
    {
      warn ("only understand snd and wave files at the moment");
      SoundCloseFile (s);
#ifdef ROBUST_PLAY
      signal (SIGPIPE, old_sigpipe);
#endif
      return 0;
    }

#ifndef CACHE_SOUNDS
  sounds_in_play++;
  AuSoundPlayFromData (aud,
		       s,
		       data+offset,
		       AuNone,
		       AuFixedPointFromFraction (volume,100),
		       doneCB, (AuPointer) &sounds_in_play,
		       NULL,
		       NULL,
		       NULL,
		       NULL);
#else
  /* Cache the sounds in buckets on the server */

  {
    do_caching_play (s, volume, data+offset);
  }
#endif /* CACHE_SOUNDS */


#ifndef XTEVENTS
  wait_for_sounds ();
#else
  if (!NILP (Vsynchronous_sounds))
    {
      wait_for_sounds ();
    }
#endif

  SoundCloseFile (s); 

#ifdef ROBUST_PLAY
  signal (SIGPIPE, old_sigpipe);
#endif

  return 1;
}

#if defined (ROBUST_PLAY)

 /********************************************************************\
 *                                                                    *
 * Code to protect the client from server shutdowns.                  *
 *                                                                    *
 * This is unbelievably horrible.                                     *
 *                                                                    *
 \********************************************************************/

static AuBool
CatchIoErrorAndJump (AuServer *old_aud)
{
  if (old_aud)
    warn ("Audio Server connection broken"); 
  else
    warn ("Audio Server connection broken because of signal");

#ifdef XTEVENTS
#ifdef XTOOLKIT
  {
    AuXtAppRemoveAudioHandler (aud, input_id); 
  }
#endif

  if (aud)
    AuCloseServer (aud);
  aud = NULL;
  sounds_in_play = 0;

  longjmp (AuXtErrorJump, 1);

#else /* not XTEVENTS */

  if (aud)
    AuCloseServer (aud);
  aud = NULL;
  sounds_in_play = 0;
  longjmp (AuXtErrorJump, 1);
 
#endif /* XTEVENTS */
  return 0;
}

SIGTYPE
sigpipe_handle (int signo)
{
  CatchIoErrorAndJump (NULL);
}

static AuBool
CatchErrorAndJump (AuServer *old_aud,
		   AuErrorEvent *event)
{
  return CatchIoErrorAndJump (old_aud);
}

#endif /* ROBUST_PLAY */

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

static char *
NameFromData (const char *buf,
	      int len)

{
  char name[9];
  int i;
  char *s;

  buf+=len/2;
  len -= len/2;

  i=0;
  while (i<8 && len >0)
    {
      while (*buf < 32 && len>0)
	{
	  buf++;
	  len--;
	}
      name[i]= *buf;
      i++;
      buf++;
      len--;
    }

  name[i]='\0';

  if (i==8)
    {
      strcpy (s = (char *) malloc (10), name);
    }
  else 
    {
      strcpy (s = (char *) malloc (15), "short sound");
    }

  return s;
}

/* Code to do a pseudo-open on a data buffer. Only for snd files at the
   moment. 
 */

static SndInfo *
SndOpenDataForReading (const char *data,
		       int length)

{
  SndInfo        *si;
  int             size;

  if (!(si = (SndInfo *) malloc (sizeof (SndInfo))))
    return NULL;

  si->comment = NULL;
  si->writing = 0;

  memcpy (&si->h, data, sizeof (SndHeader));

  if (NAS_LITTLE_ENDIAN)
    {
      char            n;
    
      swapl (&si->h.magic, n);
      swapl (&si->h.dataOffset, n);
      swapl (&si->h.dataSize, n);
      swapl (&si->h.format, n);
      swapl (&si->h.sampleRate, n);
      swapl (&si->h.tracks, n);
    }

  if (si->h.magic != SND_MAGIC_NUM)
    {
      free (si);
      return NULL;
    }

  size = si->h.dataOffset - sizeof (SndHeader);

  if (size)
    {
      if (!(si->comment = (char *) malloc (size + 1)))
	{
	  free (si);
	  return NULL;
	}

      memcpy (si->comment,  data+sizeof (SndHeader), size);

      *(si->comment + size) = 0;
      if (*si->comment == '\0')
	si->comment =
	  NameFromData (data+si->h.dataOffset, length-si->h.dataOffset);
    }
  else
    si->comment = NameFromData (data+si->h.dataOffset, length-si->h.dataOffset);

  si->h.dataSize = length-si->h.dataOffset;

  si->fp=NULL;

  return si;
}

/* Stuff taken from wave.c from NAS.  Just like snd files, NAS can't
   read wave data from memory, so these functions do that for us. */

#define Err()		{ return NULL; }
#define readFourcc(_f)	dread(_f, sizeof(RIFF_FOURCC), 1)
#define cmpID(_x, _y)							      \
    strncmp((char *) (_x), (char *) (_y), sizeof(RIFF_FOURCC))
#define PAD2(_x)	(((_x) + 1) & ~1)

/* These functions here are for faking file I/O from buffer. */

/* The "file" position */
static size_t file_posn;
/* The length of the "file" */
static size_t file_len;
/* The actual "file" data. */
static const void* file_data;

/* Like fopen, but for a buffer in memory */
static void
dopen (const void* data, size_t length)
{
   file_data = data;
   file_len = length;
   file_posn = 0;
}

/* Like fread, but for a buffer in memory */
static int
dread (void* buf, size_t size, size_t nitems)
{
  size_t nread = size * nitems;
  
  if (file_posn + nread <= file_len)
    {
      memcpy(buf, (char *) file_data + file_posn, size * nitems);
      file_posn += nread;
      return nitems;
    }
  else
    {
      return EOF;
    }
}

/* Like fgetc, but for a buffer in memory */
static int
dgetc (void)
{
  if (file_posn < file_len)
    return ((char *)file_data)[file_posn++];
  else
    return -1;
}

/* Like fseek, but for a buffer in memory */
static int
dseek (long offset, int from)
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
static long
dtell (void)
{
  return file_posn;
}

/* Data buffer analogs for FileReadS and FileReadL in NAS. */

static unsigned short
DataReadS (int swapit)
{
    unsigned short us;

    dread(&us, 2, 1);
    if (swapit)
	us = FileSwapS(us);
    return us;
}

static AuUint32
DataReadL (int swapit)
{
    AuUint32 ul;

    dread(&ul, 4, 1);
    if (swapit)
	ul = FileSwapL(ul);
    return ul;
}

static int
readChunk (RiffChunk *c)
{
    int             status;
    char            n;

    if ((status = dread(c, sizeof(RiffChunk), 1)))
	if (NAS_BIG_ENDIAN)
	    swapl(&c->ckSize, n);

    return status;
}

/* A very straight-forward translation of WaveOpenFileForReading to
   read the wave data from a buffer in memory. */

static WaveInfo *
WaveOpenDataForReading (const char *data,
			int length)
{
    RiffChunk       ck;
    RIFF_FOURCC     fourcc;
    AuInt32            fileSize;
    WaveInfo       *wi;

    
    if (!(wi = (WaveInfo *) malloc(sizeof(WaveInfo))))
	return NULL;

    wi->comment = NULL;
    wi->dataOffset = wi->format = wi->writing = 0;

    dopen(data, length);
    
    if (!readChunk(&ck) ||
	cmpID(&ck.ckID, RIFF_RiffID) ||
	!readFourcc(&fourcc) ||
	cmpID(&fourcc, RIFF_WaveID))
	Err();

    fileSize = PAD2(ck.ckSize) - sizeof(RIFF_FOURCC);

    while (fileSize >= (AuInt32) sizeof(RiffChunk))
    {
	if (!readChunk(&ck))
	    Err();

	fileSize -= sizeof(RiffChunk) + PAD2(ck.ckSize);

	/* LIST chunk */
	if (!cmpID(&ck.ckID, RIFF_ListID))
	{
	    if (!readFourcc(&fourcc))
		Err();

	    /* INFO chunk */
	    if (!cmpID(&fourcc, RIFF_ListInfoID))
	    {
		ck.ckSize -= sizeof(RIFF_FOURCC);

		while (ck.ckSize)
		{
		    RiffChunk       c;

		    if (!readChunk(&c))
			Err();

		    /* ICMT chunk */
		    if (!cmpID(&c.ckID, RIFF_InfoIcmtID))
		    {
			if (!(wi->comment = (char *) malloc(c.ckSize)) ||
			    !dread(wi->comment, c.ckSize, 1))
			    Err();

			if (c.ckSize & 1)
			    dgetc();	/* eat the pad byte */
		    }
		    else
			/* skip unknown chunk */
			dseek(PAD2(c.ckSize), 1);

		    ck.ckSize -= sizeof(RiffChunk) + PAD2(c.ckSize);
		}
	    }
	    else
		/* skip unknown chunk */
		dseek(PAD2(ck.ckSize) - sizeof(RIFF_FOURCC), 1);
	}
	/* wave format chunk */
	else if (!cmpID(&ck.ckID, RIFF_WaveFmtID) && !wi->format)
	{
	    AuInt32            dummy;

	    wi->format = DataReadS(NAS_BIG_ENDIAN);
	    wi->channels = DataReadS(NAS_BIG_ENDIAN);
	    wi->sampleRate = DataReadL(NAS_BIG_ENDIAN);

	    /* we don't care about the next two fields */
	    dummy = DataReadL(NAS_BIG_ENDIAN);
	    dummy = DataReadS(NAS_BIG_ENDIAN);

	    if (wi->format != RIFF_WAVE_FORMAT_PCM)
		Err();

	    wi->bitsPerSample = DataReadS(NAS_BIG_ENDIAN);

	    /* skip any other format specific fields */
	    dseek(PAD2(ck.ckSize - 16), 1);
	}
	/* wave data chunk */
	else if (!cmpID(&ck.ckID, RIFF_WaveDataID) && !wi->dataOffset)
	{
	    long endOfFile;

	    wi->dataOffset = dtell();
	    wi->dataSize = ck.ckSize;
	    dseek(0, 2);
	    endOfFile = dtell();

	    /* seek past the data */
	    if (dseek(wi->dataOffset + PAD2(ck.ckSize), 0) ||
		dtell() > endOfFile)
	    {
		/* the seek failed, assume the size is bogus */
		dseek(0, 2);
		wi->dataSize = dtell() - wi->dataOffset;
	    }

	    wi->dataOffset -= sizeof(long);
	}
	else
	    /* skip unknown chunk */
	    dseek(PAD2(ck.ckSize), 1);
    }

    if (!wi->dataOffset)
	Err();

    wi->numSamples = wi->dataSize / wi->channels / (wi->bitsPerSample >> 3);

    if (!wi->comment)
       wi->comment = NameFromData (data + wi->dataOffset,
                                   length - wi->dataOffset);

    wi->fp = NULL;
    
    return wi;
}


static Sound
SoundOpenDataForReading (unsigned char *data,
			 int length)

{
  Sound s;
#if (AudioLibraryVersionMajor >= 2 ) && (AudioLibraryVersionMinor >= 3)
  SoundFileInfoProc toProc;
#endif

  if (!(s = (Sound) malloc (sizeof (SoundRec))))
    return NULL;

  if ((s->formatInfo = SndOpenDataForReading ((char *) data, length)) != NULL)
    {
#if (AudioLibraryVersionMajor >= 2 ) && (AudioLibraryVersionMinor >= 3)
      if ((toProc = SoundFileGetProc(SoundFileFormatSnd, 
				     SoundFileInfoProcTo)) == NULL)
	{
	  SndCloseFile ((SndInfo *) (s->formatInfo));
	  free (s);

	  return NULL;
	}
      if (!((*toProc)(s)))
#else
      if (!((int(*)(Sound))(SoundFileInfo[SoundFileFormatSnd].toSound)) (s))
#endif
	{
	  SndCloseFile ((SndInfo *) (s->formatInfo));
	  free (s);
	  return NULL;
	}
    }
  else if ((s->formatInfo = WaveOpenDataForReading ((char *) data, length)) != NULL)
    {
#if (AudioLibraryVersionMajor >= 2 ) && (AudioLibraryVersionMinor >= 3)
      if ((toProc = SoundFileGetProc(SoundFileFormatWave, 
				     SoundFileInfoProcTo)) == NULL)
	{
	  WaveCloseFile ((WaveInfo *) (s->formatInfo));
	  free (s);

	  return NULL;
	}
      if (!((*toProc)(s)))
#else
      if (!((int(*)(Sound))(SoundFileInfo[SoundFileFormatWave].toSound)) (s))
#endif
	{
	  WaveCloseFile ((WaveInfo *) (s->formatInfo));
	  free (s);
	  return NULL;
	}
    }

  return s;
}
