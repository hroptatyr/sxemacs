/* linuxplay.c - play a sound file on the speaker
 **
 ** Copyright (C) 1995,96 by Markus Gutschke (gutschk@math.uni-muenster.de)
 ** This is version 1.3 of linuxplay.c, with platform-independent functions
 ** moved to a different file by Robert Bihlmeyer <robbe@orcus.priv.at>.
 **
 ** Parts of this code were inspired by sunplay.c, which is copyright 1989 by
 ** Jef Poskanzer and 1991,92 by Jamie Zawinski; c.f. sunplay.c for further
 ** information.
 **
 ** Permission to use, copy, modify, and distribute this software and its
 ** documentation for any purpose and without fee is hereby granted, provided
 ** that the above copyright notice appear in all copies and that both that
 ** copyright notice and this permission notice appear in supporting
 ** documentation.  This software is provided "as is" without express or
 ** implied warranty.
 **
 ** Changelog:
 **  1.0  --  first release; supports SunAudio, Wave and RAW file formats
 **           detects (and rejects) VOC file format
 **           tested with PC-Speaker driver only
 **  1.1  --  fixed bug with playback of stereo Wave files
 **           fixed VOC file detection
 **           fixed mono/8bit conversion
 **           cleaned up mixer programming (c.f. VoxWare-SDK)
 **           tested with PC-Speaker driver and with PAS16 soundcard
 **  1.2  --  first (incompatible) attempt at fixing reliable signal handling
 **  1.3  --  changed signal handling to use reliable signals; this is done
 **           by including "syssignal.h"; it fixes nasty program crashes
 **           when using native sound in TTY mode.
 **           added support for DEC audio file format (this is basically the
 **           same as Sun audio, but uses little endian format, instead).
 **           strip the header from Sun audio and DEC audio files in order to
 **           prevent noise at beginning of samples (thanks to Thomas Pundt
 **           <pundtt@math.uni-muenster.de> for pointing out this bug and
 **           providing information on the file format).
 **           added a few more conversion routines.
 **           made the code even more tolerant to the limits imposed by some
 **           soundcards and try to accept soundfiles even if they are not
 **           fully conformant to the standard.
 **  1.4  --  increased header size to 256; I hope there is no sample software
 **           that requires this much.
 **           added code for converting from signed to unsigned format as
 **           some soundcards cannot handle signed 8bit data.
 */

/* Synched up with: Not in FSF. */

/* XEmacs beta testers say:  undef this by default. */
#undef NOVOLUMECTRLFORMULAW /* Changing the volume for uLaw-encoded
			       samples sounds very poor; possibly,
			       this is true only for the PC-Snd
			       driver, so undefine this symbol at your
			       discretion */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "miscplay.h"
#include "nativesound.h"

#include <errno.h>
#include <fcntl.h>
#include SOUNDCARD_H_FILE /* Path computed by configure */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/signal.h>
#include <unistd.h>

#ifdef LINUXPLAYSTANDALONE
#define perror(str) fprintf(stderr,"audio: %s %s\n",str,strerror(errno));
#define warn(str)   fprintf(stderr,"audio: %s\n",str);
#else
#include "lisp.h"
#include "syssignal.h"
#include "sysfile.h"
#define perror(str) message("audio: %s, %s ",str,strerror(errno))
#define warn(str)   message("audio: %s ",GETTEXT(str))
#endif

static  SIGTYPE (*sighup_handler) (int);
static  SIGTYPE (*sigint_handler) (int);

static int           mix_fd;
static int           audio_vol;
static int           audio_fd;
static char	     *audio_dev = "/dev/dsp";

/* Intercept SIGINT and SIGHUP in order to close the audio and mixer
   devices before terminating sound output; this requires reliable
   signals as provided by "syssignal.h" */
static SIGTYPE
sighandler (int sig)
{
  if (mix_fd > 0) {
    if (audio_vol >= 0) {
      ioctl(mix_fd,SOUND_MIXER_WRITE_PCM,&audio_vol);
      audio_vol = -1; }
    if (mix_fd != audio_fd)
      close(mix_fd);
    mix_fd = -1; }
  if (audio_fd > 0) {
    ioctl(audio_fd,SNDCTL_DSP_SYNC,NULL);
    ioctl(audio_fd,SNDCTL_DSP_RESET,NULL);
    close(audio_fd);
    audio_fd = -1; }
  if (sig == SIGHUP && sighup_handler)      sighup_handler(sig);
  else if (sig == SIGINT && sigint_handler) sigint_handler(sig);
  else exit(1);
}

/* Initialize the soundcard and mixer device with the parameters that we
   found in the header of the sound file. If the soundcard is not capable of
   natively supporting the required parameters, then try to set up conversion
   routines.
   The difficulty with setting up the sound card is that the parameters are
   not fully orthogonal; changing one of them might affect some of the
   others, too. Thus we do quite a lot of double checking; actually most of
   this is not needed right now, but it will come in handy, if the kernel's
   sounddriver ever changes or if third-party sounddrivers are used. */
static int audio_init(int mixx_fd, int auddio_fd, int fmt, int speed,
		      int tracks, int *volume,
		      size_t (**sndcnv) (void **, size_t *sz, void **))
{
  int i,the_speed,the_stereo,the_fmt;

  *sndcnv = sndcnvnop;

  if (ioctl(auddio_fd,SNDCTL_DSP_SYNC,NULL) < 0) {
    perror("SNDCTL_DSP_SYNC");
    return(0); }

  /* Initialize sound hardware with preferred parameters */

  /* If the sound hardware cannot support 16 bit format or requires a
     different byte sex then try to drop to 8 bit format */

  the_fmt = fmt;
  if(ioctl(audio_fd,SNDCTL_DSP_SETFMT,&the_fmt) < 0) {
  	perror("SNDCTL_DSP_SETFMT");
  	return(0);
  }

  if (fmt != the_fmt) {
    if (fmt == AFMT_S16_LE || fmt == AFMT_S16_BE) {
      *sndcnv = fmt == AFMT_S16_BE ? sndcnv2byteBE : sndcnv2byteLE;
      if (((i=fmt=AFMT_U8),ioctl(audio_fd,SNDCTL_DSP_SETFMT,&i)) < 0 ||
	  fmt != i || ioctl(audio_fd,SNDCTL_DSP_SETFMT,&the_fmt) < 0 ||
	  fmt != the_fmt) {
  	perror("SNDCTL_DSP_SETFMT");
  	return(0); } }
    else if (fmt == AFMT_MU_LAW && the_fmt == AFMT_U8 ) {
      /* the kernel will convert for us */ }
    else {
      perror("SNDCTL_DSP_SETFMT");
      return(0); } }
  else if (fmt == AFMT_S8) {
    *sndcnv = sndcnv2unsigned;
    if (((i=fmt=AFMT_U8),ioctl(audio_fd,SNDCTL_DSP_SETFMT,&i)) < 0 ||
        fmt != i || ioctl(audio_fd,SNDCTL_DSP_SETFMT,&the_fmt) < 0 ||
        fmt != the_fmt) {
      perror("SNDCTRL_DSP_SETFMT");
      return(0); } }

  /* The PCSP driver does not support reading of the sampling rate via the
     SOUND_PCM_READ_RATE ioctl; determine "the_speed" here */
  the_speed = speed; ioctl(audio_fd,SNDCTL_DSP_SPEED,&the_speed);
  /* The PCSP driver does not support reading of the mono/stereo flag, thus
     we assume, that failure to change this mode means we are in mono mode  */
  if (((i = (the_stereo = tracks)-1),ioctl(audio_fd,SNDCTL_DSP_STEREO,&i)) < 0)
    the_stereo = 1;

  /* Try to request stereo playback (if needed); if this cannot be supported
     by the hardware, then install conversion routines for mono playback */

  /* This ioctl will fail if we use the PCSP driver; thus the value of
     "the_stereo" is still unchanged */
  ioctl(audio_fd,SOUND_PCM_READ_CHANNELS,&the_stereo);
  if (tracks != the_stereo) {
    if (tracks == 2) {
      tracks = 1;
      *sndcnv = *sndcnv == sndcnv2byteLE   ? sndcnv2monobyteLE :
              *sndcnv == sndcnv2byteBE   ? sndcnv2monobyteBE :
              *sndcnv == sndcnv2unsigned ? sndcnv2monounsigned :
	the_fmt == AFMT_S16_LE ? sndcnv16_2monoLE :
	the_fmt == AFMT_S16_BE ? sndcnv16_2monoBE :
	the_fmt == AFMT_S8     ? sndcnv8S_2mono :
	the_fmt == AFMT_U8     ? sndcnv8U_2mono :
	the_fmt == AFMT_MU_LAW ? sndcnvULaw_2mono : NULL;
      if (*sndcnv == NULL) { /* this should not happen */
	perror("SNDCTL_DSP_STEREO");
	return(0); }
      /* Switch to mono mode */
      if (((i = 0),ioctl(audio_fd,SNDCTL_DSP_STEREO,&i)) < 0 || i) {
  	perror("SNDCTL_DSP_STEREO");
	return(0); }
      /* Now double check that everything is set as expected */
      if (((i = AFMT_QUERY),ioctl(audio_fd,SNDCTL_DSP_SETFMT,&i)) < 0 ||
	  (i != the_fmt &&
	   (((i=the_fmt),ioctl(audio_fd,SNDCTL_DSP_SETFMT,&i)) < 0 ||
	    i != the_fmt ||
	    ((i = AFMT_QUERY),ioctl(audio_fd,SNDCTL_DSP_SETFMT,&i)) < 0 ||
	    i != the_fmt)) ||
	  (ioctl(audio_fd,SOUND_PCM_READ_CHANNELS,&i) >= 0 &&
	   i != 1)) {
	/* There was no way that we could set the soundcard to a meaningful
           mode */
 	perror("SNDCTL_DSP_SETFMT and SNDCTL_DSP_STEREO");
  	return(0); } }
    else {
      /* Somebody set the soundcard to stereo even though we requested
         mono; this should not happen... */
      if (((i = the_stereo = tracks),ioctl(audio_fd,SNDCTL_DSP_STEREO,&i))<0 ||
	  i != the_stereo-1) {
	perror("SNDCTL_DSP_STEREO");
	return(0); }
      if (((i = AFMT_QUERY),ioctl(audio_fd,SNDCTL_DSP_SETFMT,&i)) < 0 ||
	  i != the_fmt) {
	perror("SNDCTL_DSP_SETFMT");
	return(0); } } }

  /* Fail if deviations from desired sampling frequency are too big */

  /* This ioctl will fail if we use the PCSP driver; thus the value of
     "the_speed" is still unchanged */
  ioctl(audio_fd,SOUND_PCM_READ_RATE,&the_speed);
  if (speed*14 < the_speed*10 || speed*6 > the_speed*10) {
    char buffer[256];
    sprintf(buffer,"SNDCTL_DSP_SPEED (req: %d, rtn: %d)",speed,the_speed);
    perror(buffer);
    return(0); }

  /* Use the mixer device for setting the playback volume */
  if (mixx_fd > 0) {
    int vol = *volume & 0xFF;
    if (ioctl(mixx_fd,SOUND_MIXER_READ_PCM,volume) < 0)
      *volume = -1;
    if (vol < 0) vol = 0; else if (vol > 100) vol = 100;
#ifdef NOVOLUMECTRLFORMULAW
    if (fmt == AFMT_MU_LAW)
      vol = 100;
#endif
    vol |= 256*vol;
    /* Do not signal an error, if volume control is unavailable! */
    ioctl(mixx_fd,SOUND_MIXER_WRITE_PCM,&vol); }

#if defined(LINUXPLAYSTANDALONE) && 1
  /* Debugging output is displayed only when compiled as stand-alone version */
  {int the_volume;
  the_fmt = AFMT_QUERY;
  ioctl(audio_fd,SNDCTL_DSP_SETFMT,&the_fmt);
  ioctl(auddio_fd,SOUND_PCM_READ_CHANNELS,&the_stereo);
  ioctl(auddio_fd,SOUND_PCM_READ_RATE,&the_speed);
  ioctl(mixx_fd,SOUND_MIXER_READ_PCM,&the_volume);
  fprintf(stderr,"%s, %s, %dHz, L:%d/R:%d\n",
	  the_fmt == AFMT_MU_LAW ? "AFMT_MU_LAW" :
	  the_fmt == AFMT_A_LAW ? "AFMT_A_LAW" :
	  the_fmt == AFMT_IMA_ADPCM ? "AFMT_IMA_ADPCM" :
	  the_fmt == AFMT_U8 ? "AFMT_U8" :
	  the_fmt == AFMT_S16_LE ? "AFMT_S16_LE" :
	  the_fmt == AFMT_S16_BE ? "AFMT_S16_BE" :
	  the_fmt == AFMT_S8 ? "AFMT_S8" :
	  the_fmt == AFMT_U16_LE ? "AFMT_U16_LE" :
	  the_fmt == AFMT_U16_BE ? "AFMT_U16_BE" :
	  the_fmt == AFMT_MPEG ? "AFMT_MPEG" :
	  "AFMT_???",
	  the_stereo == 2 ? "stereo" : "mono",
	  the_speed,
	  the_volume / 256, the_volume % 256); }
#endif

  return(1);
}

/* XEmacs requires code both for playback of pre-loaded data and for playback
   from a soundfile; we use one function for both cases.

   Returns 1 on succes. 0 otherwise.
*/
static int linux_play_data_or_file(int fd,unsigned char *data,
				    int length,int volume)
{
  size_t         (*parsesndfile)(void **dayta,size_t *sz,void **outbuf);
  size_t         (*sndcnv)(void **dayta,size_t *sz,void **);
  fmtType        ffmt;
  int            fmt,speed,tracks;
  unsigned char *pptr,*optr,*cptr,*sptr;
  int            wrtn,rrtn,crtn,prtn;
  unsigned char         sndbuf[SNDBUFSZ];

  /* We need to read at least the header information before we can start
     doing anything */
  if (!data || length < HEADERSZ) {
    if (fd < 0) return 0;
    else {
      length = read(fd,sndbuf,SNDBUFSZ);
      if (length < HEADERSZ)
	return 0;
      data   = sndbuf;
      length = SNDBUFSZ; }
  }

  ffmt = analyze_format(data,&fmt,&speed,&tracks,&parsesndfile);

  if (ffmt != fmtRaw && ffmt != fmtSunAudio && ffmt != fmtWave) {
    warn("Unsupported file format (neither RAW, nor Sun/DECAudio, nor WAVE)");
      return 0; }

  /* The VoxWare-SDK discourages opening /dev/audio; opening /dev/dsp and
     properly initializing it via ioctl() is preferred */
  if ((audio_fd=open(audio_dev, O_WRONLY | O_NONBLOCK, 0)) < 0) {
    /* JV. Much too verbose. In addition this can crash. See NOTE: in
       Fplay_sound 
       perror(audio_dev); */
    if (mix_fd > 0 && mix_fd != audio_fd) { close(mix_fd); mix_fd = -1; }
    return 0; }

  /* The VoxWare-SDK discourages direct manipulation of the mixer device as
     this could lead to problems, when multiple sound cards are installed */
  mix_fd = audio_fd;

  sighup_handler = signal(SIGHUP, sighandler);
  sigint_handler = signal(SIGINT, sighandler);

  if (!audio_init(mix_fd,audio_fd,fmt,speed,tracks,&volume,&sndcnv))
    goto END_OF_PLAY;
  audio_vol = volume;

  reset_parsestate();

  /* Mainloop: read a block of data, parse its contents, perform all
               the necessary conversions and output it to the sound
               device; repeat until all data has been processed */
  rrtn = length;
  do {
    for (pptr = data; (prtn = parsesndfile((void **)&pptr,(size_t *)&rrtn,
					   (void **)&optr)) > 0; )
      for (cptr = optr; (crtn = sndcnv((void **)&cptr,(size_t *) &prtn,
				       (void **)&sptr)) > 0; ) {
	for (;;) {
	  if ((wrtn = write(audio_fd,sptr,crtn)) < 0) {
	    perror("write"); goto END_OF_PLAY; }
	  else if (wrtn) break;
	  else if (ioctl(audio_fd,SNDCTL_DSP_SYNC,NULL) < 0) {
	    perror("SNDCTL_DSP_SYNC"); goto END_OF_PLAY; } }
	if (wrtn != crtn) {
	  char buf[255];
	  sprintf(buf,"play: crtn = %d, wrtn = %d",crtn,wrtn);
	  warn(buf);
	  goto END_OF_PLAY; } }
    if (fd >= 0) {
      if ((rrtn = read(fd,sndbuf,SNDBUFSZ)) < 0) {
	perror("read"); goto END_OF_PLAY; } }
    else
      break;
  } while (rrtn > 0);

  if (ffmt == fmtWave)
    parse_wave_complete();


END_OF_PLAY:
  /* Now cleanup all used resources */

  ioctl(audio_fd,SNDCTL_DSP_SYNC,NULL);
  ioctl(audio_fd,SNDCTL_DSP_RESET,NULL);

  signal(SIGHUP,sighup_handler);
  signal(SIGINT,sigint_handler);

  if (mix_fd > 0) {
    if (audio_vol >= 0) {
      ioctl(mix_fd,SOUND_MIXER_WRITE_PCM,&audio_vol);
      audio_vol = -1; }
    if (mix_fd != audio_fd)
      close(mix_fd);
    mix_fd = -1; }

  close(audio_fd);
  audio_fd = -1;

  return 1;
}

/* Call "linux_play_data_or_file" with the appropriate parameters for
   playing a soundfile */
void play_sound_file (char *sound_file, int volume)
{
  int fd;

  if ((fd=open(sound_file,O_RDONLY,0)) < 0) {
    perror(sound_file);
    return; }
  linux_play_data_or_file(fd,NULL,0,volume);
  close(fd);
  return;
}

/* Call "linux_play_data_or_file" with the appropriate parameters for
   playing pre-loaded data */
int play_sound_data (unsigned char *data, int length, int volume)
{
  return linux_play_data_or_file(-1,data,length,volume);
}
