/* miscplay.c - general routines related to playing sounds
 **
 ** Copyright (C) 1995,96 by Markus Gutschke (gutschk@math.uni-muenster.de)
 ** This was sawed out from version 1.3 of linuxplay.c by
 ** Robert Bihlmeyer <robbe@orcus.priv.at>.
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
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "miscplay.h"
#include "lisp.h"
#include "syssignal.h"
#include "sysfile.h"
#define warn(str)   message("audio: %s ",GETTEXT(str))

#include <stdlib.h>

#ifdef __GNUC__
#define UNUSED(x) ((void)(x))
#else
#define UNUSED(x)
#endif

/* Maintain global variable for keeping parser state information; this struct
   is set to zero before the first invocation of the parser. The use of a
   global variable prevents multiple concurrent executions of this code, but
   this does not happen anyways... */
enum wvState
{ wvMain,
  wvSubchunk,
  wvOutOfBlock,
  wvSkipChunk,
  wvSoundChunk,
  wvFatal,
  wvFatalNotify
};

static union {
  struct {
    int           align;
    enum wvState state;
    size_t        left;
    unsigned char leftover[HEADERSZ];
    signed long   chunklength;
  } wave;
  struct {
    int           align;
    int           isdata;
    int           skipping;
    size_t        left;
    unsigned char leftover[HEADERSZ];
  } audio;
} parsestate;

/* Use a global buffer as scratch-pad for possible conversions of the
   sampling format */
unsigned char miscplay_sndbuf[SNDBUFSZ];

/* Initialize global parser state information to zero */
void reset_parsestate()
{
  memset(&parsestate,0,sizeof(parsestate));
}

/* Verify that we could fully parse the entire soundfile; this is needed
   only for files in WAVE format */
int parse_wave_complete()
{
  if (parsestate.wave.state != wvOutOfBlock &&
      parsestate.wave.state != wvFatal) {
    warn("Unexpected end of WAVE file");
    return 0;
  } else
    return 1;
}

/* There is no special treatment required for parsing raw data files; we
   assume that these files contain data in 8bit unsigned format that
   has been sampled at 8kHz; there is no extra header */
static size_t parseraw(void **data,size_t *sz,void **outbuf)
{
  int rc = *sz;

  *outbuf = *data;
  *sz = 0;
  return(rc);
}

/* Currently we cannot cope with files in VOC format; if you really need
   to play these files, they should be converted by using SOX */
static size_t parsevoc(void **data,size_t *sz,void **outbuf)
{
  UNUSED(data);
  UNUSED(sz);
  UNUSED(outbuf);
  return(0);
}

/* We need to perform some look-ahead in order to parse files in WAVE format;
   this might require re-partioning of the data segments if headers cross the
   boundaries between two read operations. This is done in a two-step way:
   first we request a certain amount of bytes... */
static inline int waverequire(void **data,size_t *sz,size_t rq)
{
  int rc = 1;

  if (rq > HEADERSZ) {
    warn("Header size exceeded while parsing WAVE file");
    parsestate.wave.state = wvFatal;
    *sz = 0;
    return(0); }
  if ((rq -= parsestate.wave.left) <= 0)
    return(rc);
  if (rq > *sz) {rq = *sz; rc = 0;}
  memcpy(parsestate.wave.leftover+parsestate.wave.left,
        *data,rq);
  parsestate.wave.left      += rq;
  (*(unsigned char **)data) += rq;
  *sz                       -= rq;
  return(rc);
}

/* ...and next we remove this many bytes from the buffer */
static inline void waveremove(size_t rq)
{
  if (parsestate.wave.left <= rq)
    parsestate.wave.left = 0;
  else {
    parsestate.wave.left -= rq;
    memmove(parsestate.wave.leftover,
           parsestate.wave.leftover+rq,
           parsestate.wave.left); }
  return;
}

/* Sound files in WAVE format can contain an arbitrary amount of tagged
   chunks; this requires quite some effort for parsing the data */
static size_t parsewave(void **data,size_t *sz,void **outbuf)
{
  for (;;)
    switch (parsestate.wave.state) {
    case wvMain:
      if (!waverequire(data,sz,20))
       return(0);
      /* Keep compatibility with Linux 68k, etc. by not relying on byte-sex  */
      parsestate.wave.chunklength = parsestate.wave.leftover[16] +
       256*(parsestate.wave.leftover[17] +
            256*(parsestate.wave.leftover[18] +
                 256*parsestate.wave.leftover[19]));
      waveremove(20);
      parsestate.wave.state = wvSubchunk;
      break;
    case wvSubchunk:
      if (!waverequire(data,sz,parsestate.wave.chunklength))
       return(0);
      parsestate.wave.align = parsestate.wave.chunklength < 14 ? 1
       : parsestate.wave.leftover[12];
      if (parsestate.wave.align != 1 &&
         parsestate.wave.align != 2 &&
         parsestate.wave.align != 4) {
       warn("Illegal datawidth detected while parsing WAVE file");
       parsestate.wave.state = wvFatal; }
      else
       parsestate.wave.state = wvOutOfBlock;
      waveremove(parsestate.wave.chunklength);
      break;
    case wvOutOfBlock:
      if (!waverequire(data,sz,8))
       return(0);
      /* Keep compatibility with Linux 68k, etc. by not relying on byte-sex  */
      parsestate.wave.chunklength = parsestate.wave.leftover[4] +
       256*(parsestate.wave.leftover[5] +
            256*(parsestate.wave.leftover[6] +
                 256*(parsestate.wave.leftover[7] & 0x7F)));
      if (memcmp(parsestate.wave.leftover,"data",4))
       parsestate.wave.state = wvSkipChunk;
      else
       parsestate.wave.state = wvSoundChunk;
      waveremove(8);
      break;
    case wvSkipChunk:
      if (parsestate.wave.chunklength > 0 && *sz > 0 &&
         (signed long)*sz < (signed long)parsestate.wave.chunklength) {
       parsestate.wave.chunklength -= *sz;
       *sz = 0; }
      else {
       if (parsestate.wave.chunklength > 0 && *sz > 0) {
         *sz -= parsestate.wave.chunklength;
         (*(unsigned char **)data) += parsestate.wave.chunklength; }
       parsestate.wave.state = wvOutOfBlock; }
      break;
    case wvSoundChunk: {
      size_t count,rq;
      if (parsestate.wave.left) { /* handle leftover bytes from last
                                    alignment operation */
       count = parsestate.wave.left;
       rq    = HEADERSZ-count;
       if (rq > (size_t) parsestate.wave.chunklength)
         rq = parsestate.wave.chunklength;
       if (!waverequire(data,sz,rq)) {
         parsestate.wave.chunklength -= parsestate.wave.left - count;
         return(0); }
       parsestate.wave.chunklength -= rq;
       *outbuf                      = parsestate.wave.leftover;
       parsestate.wave.left         = 0;
       return(rq); }
      if (*sz >= (size_t) parsestate.wave.chunklength) {
       count  = parsestate.wave.chunklength;
       rq     = 0; }
      else {
       count  = *sz;
       count -= rq = count % parsestate.wave.align; }
      *outbuf                   = *data;
      (*(unsigned char **)data) += count;
      *sz                       -= count;
      if ((parsestate.wave.chunklength -= count) < parsestate.wave.align) {
       parsestate.wave.state = wvOutOfBlock;
       /* Some broken software (e.g. SOX) attaches junk to the end of a sound
          chunk; so, let's ignore this... */
       if (parsestate.wave.chunklength)
         parsestate.wave.state = wvSkipChunk; }
      else if (rq)
       /* align data length to a multiple of datasize; keep additional data
          in "leftover" buffer --- this is necessary to ensure proper
          functioning of the sndcnv... routines */
       waverequire(data,sz,rq);
      return(count); }
    case wvFatalNotify:
      warn("Irrecoverable error while parsing WAVE file");
      parsestate.wave.state = wvFatal;
      break;
    case wvFatal:
    default:
      *sz = 0;
      return(0); }
}

/* Strip the header from files in Sun/DEC audio format; this requires some
   extra processing as the header can be an arbitrary size and it might
   result in alignment errors for subsequent conversions --- thus we do
   some buffering, where needed */
static size_t parsesundecaudio(void **data,size_t *sz,void **outbuf)
{
  /* There is data left over from the last invocation of this function; join
     it with the new data and return a sound chunk that is as big as a
     single entry */
  if (parsestate.audio.left) {
    if (parsestate.audio.left + *sz > (size_t) parsestate.audio.align) {
      int  count;
      memmove(parsestate.audio.leftover + parsestate.audio.left,
             *data,
             count = parsestate.audio.align - parsestate.audio.left);
      *outbuf = parsestate.audio.leftover;
      *sz    -= count;
      *data   = (*(char **)data) + count;
      parsestate.audio.left = 0;
      return(parsestate.audio.align); }
    else {
      /* We need even more data in order to get one complete single entry! */
      memmove(parsestate.audio.leftover + parsestate.audio.left,
             *data,
             *sz);
      *data = (*(char **)data) + *sz;
      parsestate.audio.left += *sz;
      *sz   = 0;
      return(0); } }

  /* This is the main sound chunk, strip of any extra data that does not fit
     the alignment requirements and move these bytes into the leftover buffer*/
  if (parsestate.audio.isdata) {
    int rc = *sz;
    *outbuf = *data;
    if ((parsestate.audio.left = rc % parsestate.audio.align) != 0) {
      memmove(parsestate.audio.leftover,
             (char *)*outbuf + rc - parsestate.audio.left,
             parsestate.audio.left);
      rc -= parsestate.audio.left; }
    *sz = 0;
    return(rc); }

  /* This is the first invocation of this function; we need to parse the
     header information and determine how many bytes we need to skip until
     the start of the sound chunk */
  if (!parsestate.audio.skipping) {
    unsigned char *header = (unsigned char *) *data;
    if (*sz < 8) {
      warn("Irrecoverable error while parsing Sun/DEC audio file");
      return(0); }
    /* Keep compatibility with Linux 68k, etc. by not relying on byte-sex  */
    if (header[3]) { /* Sun audio (big endian) */
      parsestate.audio.align = ((header[15] > 2)+1)*header[23];
      parsestate.audio.skipping = header[7]+256*(header[6]+256*
                                                (header[5]+256*header[4])); }
    else { /* DEC audio (little endian) */
      parsestate.audio.align = ((header[12] > 2)+1)*header[20];
      parsestate.audio.skipping = header[4]+256*(header[5]+256*
                                                (header[6]+256*header[7])); }}

  /* We are skipping extra data that has been attached to header; most usually
     this will be just a comment, such as the original filename and/or the
     creation date. Make sure that we do not return less than one single sound
     sample entry to the caller; if this happens, rather decide to move those
     few bytes into the leftover buffer and deal with it later */
  if (*sz >= (size_t) parsestate.audio.skipping) {
    /* Skip just the header information and return the sound chunk */
    int rc = *sz - parsestate.audio.skipping;
    *outbuf = (char *)*data + parsestate.audio.skipping;
    if ((parsestate.audio.left = rc % parsestate.audio.align) != 0) {
      memmove(parsestate.audio.leftover,
             (char *)*outbuf + rc - parsestate.audio.left,
             parsestate.audio.left);
      rc -= parsestate.audio.left; }
    *sz = 0;
    parsestate.audio.skipping = 0;
    parsestate.audio.isdata++;
    return(rc); }
  else {
    /* Skip everything */
    parsestate.audio.skipping -= *sz;
    return(0); }
}

/* If the soundcard could not be set to natively support the data format, we
   try to do some limited on-the-fly conversion to a different format; if
   no conversion is needed, though, we can output directly */
size_t sndcnvnop(void **data,size_t *sz,void **outbuf)
{
  int rc = *sz;

  *outbuf = *data;
  *sz = 0;
  return(rc);
}

/* Convert 8 bit unsigned stereo data to 8 bit unsigned mono data */
size_t sndcnv8U_2mono(void **data,size_t *sz,void **outbuf)
{
  REGISTER unsigned char *src;
  REGISTER unsigned char *dest;
  int rc,count;

  count = *sz / 2;
  if (count > SNDBUFSZ) { *sz  -= 2*SNDBUFSZ; count = SNDBUFSZ; }
  else                    *sz   = 0;
  rc      = count;
  src     = (unsigned char *) *data;
  *outbuf =
  dest    = miscplay_sndbuf;
  while (count--)
    {
      *dest++ = (unsigned char)(((int)*(src) +
				 (int)*(src+1)) / 2);
      src += 2;
    }
  *data   = src;
  return(rc);
}

/* Convert 8 bit signed stereo data to 8 bit signed mono data */
size_t sndcnv8S_2mono(void **data,size_t *sz,void **outbuf)
{
  REGISTER unsigned char *src;
  REGISTER unsigned char *dest;
  int rc, count;

  count = *sz / 2;
  if (count > SNDBUFSZ) { *sz  -= 2*SNDBUFSZ; count = SNDBUFSZ; }
  else                    *sz   = 0;
  rc      = count;
  src     = (unsigned char *) *data;
  *outbuf =
  dest    = miscplay_sndbuf;
  while (count--)
    {
      *dest++ = (unsigned char)(((int)*((signed char *)(src)) +
				 (int)*((signed char *)(src+1))) / 2);
      src  += 2;
    }
  *data   = src;
  return(rc);
}

/* Convert 8 bit signed stereo data to 8 bit unsigned mono data */
size_t sndcnv2monounsigned(void **data,size_t *sz,void **outbuf)
{
  REGISTER unsigned char *src;
  REGISTER unsigned char *dest;
  int rc,count;

  count = *sz / 2;
  if (count > SNDBUFSZ) { *sz  -= 2*SNDBUFSZ; count = SNDBUFSZ; }
  else                    *sz   = 0;
  rc      = count;
  src     = (unsigned char *) *data;
  *outbuf =
  dest    = miscplay_sndbuf;
  while (count--)
    {
      *dest++ = (unsigned char)(((int)*((signed char *)(src)) +
				 (int)*((signed char *)(src+1))) / 2) ^ 0x80;
      src += 2;
    }
  *data   = src;
  return(rc);
}

/* Convert 8 bit signed mono data to 8 bit unsigned mono data */
size_t sndcnv2unsigned(void **data,size_t *sz,void **outbuf)
{
  REGISTER unsigned char *src;
  REGISTER unsigned char *dest;
  int rc,count;

  count = *sz;
  if (count > SNDBUFSZ) { *sz  -= SNDBUFSZ; count = SNDBUFSZ; }
  else                    *sz   = 0;
  rc      = count;
  src     = (unsigned char *) *data;
  *outbuf =
  dest    = miscplay_sndbuf;
  while (count--)
    *dest++ = *(src)++ ^ 0x80;
  *data   = src;
  return(rc);
}

/* Convert a number in the range -32768..32767 to an 8 bit ulaw encoded
   number --- I hope, I got this conversion right :-) */
static inline signed char int2ulaw(int i)
{
    /* Lookup table for fast calculation of number of bits that need shifting*/
    static short int t_bits[128] = {
      0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
      6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
      7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
      7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7};
    REGISTER int bits,logi;

    /* unrolling this condition (hopefully) improves execution speed */
    if (i < 0) {
      if ((i = (132-i)) > 0x7FFF) i = 0x7FFF;
      logi = (i >> ((bits = t_bits[i/256])+4));
      return((bits << 4 | logi) ^ 0x7F); }
    else {
      if ((i = 132+i) > 0x7FFF) i = 0x7FFF;
      logi = (i >> ((bits = t_bits[i/256])+4));
      return(~(bits << 4 | logi)); }
}

/* Convert from 8 bit ulaw mono to 8 bit linear mono */
size_t sndcnvULaw_2linear(void **data,size_t *sz,void **outbuf)
{
  /* conversion table stolen from Linux's ulaw.h */
  static unsigned char ulaw_dsp[] = {
     3,    7,   11,   15,   19,   23,   27,   31,
    35,   39,   43,   47,   51,   55,   59,   63,
    66,   68,   70,   72,   74,   76,   78,   80,
    82,   84,   86,   88,   90,   92,   94,   96,
    98,   99,  100,  101,  102,  103,  104,  105,
   106,  107,  108,  109,  110,  111,  112,  113,
   113,  114,  114,  115,  115,  116,  116,  117,
   117,  118,  118,  119,  119,  120,  120,  121,
   121,  121,  122,  122,  122,  122,  123,  123,
   123,  123,  124,  124,  124,  124,  125,  125,
   125,  125,  125,  125,  126,  126,  126,  126,
   126,  126,  126,  126,  127,  127,  127,  127,
   127,  127,  127,  127,  127,  127,  127,  127,
   128,  128,  128,  128,  128,  128,  128,  128,
   128,  128,  128,  128,  128,  128,  128,  128,
   128,  128,  128,  128,  128,  128,  128,  128,
   253,  249,  245,  241,  237,  233,  229,  225,
   221,  217,  213,  209,  205,  201,  197,  193,
   190,  188,  186,  184,  182,  180,  178,  176,
   174,  172,  170,  168,  166,  164,  162,  160,
   158,  157,  156,  155,  154,  153,  152,  151,
   150,  149,  148,  147,  146,  145,  144,  143,
   143,  142,  142,  141,  141,  140,  140,  139,
   139,  138,  138,  137,  137,  136,  136,  135,
   135,  135,  134,  134,  134,  134,  133,  133,
   133,  133,  132,  132,  132,  132,  131,  131,
   131,  131,  131,  131,  130,  130,  130,  130,
   130,  130,  130,  130,  129,  129,  129,  129,
   129,  129,  129,  129,  129,  129,  129,  129,
   128,  128,  128,  128,  128,  128,  128,  128,
   128,  128,  128,  128,  128,  128,  128,  128,
   128,  128,  128,  128,  128,  128,  128,  128,
  };
  unsigned char *p=(unsigned char *)*data;

  *outbuf = *data;
  while ((*sz)--)
    {
      *p = ulaw_dsp[*p];
      p++;
    }
  *sz = 0;
  *data = p;
  return p - (unsigned char *)*outbuf;
}

/* Convert 8 bit ulaw stereo data to 8 bit ulaw mono data */
size_t sndcnvULaw_2mono(void **data,size_t *sz,void **outbuf)
{

  static short int ulaw2int[256] = {
    /* Precomputed lookup table for conversion from ulaw to 15 bit signed */
    -16062,-15550,-15038,-14526,-14014,-13502,-12990,-12478,
    -11966,-11454,-10942,-10430, -9918, -9406, -8894, -8382,
     -7998, -7742, -7486, -7230, -6974, -6718, -6462, -6206,
     -5950, -5694, -5438, -5182, -4926, -4670, -4414, -4158,
     -3966, -3838, -3710, -3582, -3454, -3326, -3198, -3070,
     -2942, -2814, -2686, -2558, -2430, -2302, -2174, -2046,
     -1950, -1886, -1822, -1758, -1694, -1630, -1566, -1502,
     -1438, -1374, -1310, -1246, -1182, -1118, -1054,  -990,
      -942,  -910,  -878,  -846,  -814,  -782,  -750,  -718,
      -686,  -654,  -622,  -590,  -558,  -526,  -494,  -462,
      -438,  -422,  -406,  -390,  -374,  -358,  -342,  -326,
      -310,  -294,  -278,  -262,  -246,  -230,  -214,  -198,
      -186,  -178,  -170,  -162,  -154,  -146,  -138,  -130,
      -122,  -114,  -106,   -98,   -90,   -82,   -74,   -66,
       -60,   -56,   -52,   -48,   -44,   -40,   -36,   -32,
       -28,   -24,   -20,   -16,   -12,    -8,    -4,    +0,
    +16062,+15550,+15038,+14526,+14014,+13502,+12990,+12478,
    +11966,+11454,+10942,+10430, +9918, +9406, +8894, +8382,
     +7998, +7742, +7486, +7230, +6974, +6718, +6462, +6206,
     +5950, +5694, +5438, +5182, +4926, +4670, +4414, +4158,
     +3966, +3838, +3710, +3582, +3454, +3326, +3198, +3070,
     +2942, +2814, +2686, +2558, +2430, +2302, +2174, +2046,
     +1950, +1886, +1822, +1758, +1694, +1630, +1566, +1502,
     +1438, +1374, +1310, +1246, +1182, +1118, +1054,  +990,
      +942,  +910,  +878,  +846,  +814,  +782,  +750,  +718,
      +686,  +654,  +622,  +590,  +558,  +526,  +494,  +462,
      +438,  +422,  +406,  +390,  +374,  +358,  +342,  +326,
      +310,  +294,  +278,  +262,  +246,  +230,  +214,  +198,
      +186,  +178,  +170,  +162,  +154,  +146,  +138,  +130,
      +122,  +114,  +106,   +98,   +90,   +82,   +74,   +66,
       +60,   +56,   +52,   +48,   +44,   +40,   +36,   +32,
       +28,   +24,   +20,   +16,   +12,    +8,    +4,    +0};

  REGISTER unsigned char *src;
  REGISTER unsigned char *dest;
  int rc,count;

  count = *sz / 2;
  if (count > SNDBUFSZ) { *sz  -= 2*SNDBUFSZ; count = SNDBUFSZ; }
  else                    *sz   = 0;
  rc      = count;
  src     = (unsigned char *) *data;
  *outbuf =
  dest    = miscplay_sndbuf;
  while (count--)
    {
      /* it is not possible to directly interpolate between two ulaw encoded
	 data bytes, thus we need to convert to linear format first and later
	 we convert back to ulaw format */
      *dest++ = int2ulaw(ulaw2int[*src] +
			 ulaw2int[*(src+1)]);
      src  += 2;
    }
  *data = src;
  return(rc);
}

size_t sndcnv16swap(void **data,size_t *sz,void **outbuf)
{
  size_t cnt = *sz / 2;
  unsigned short *p;

  *outbuf = *data;
  p = (unsigned short *) *outbuf;
  while (cnt--)
    {
      *p = ((*p & 0x00ff) << 8) | (*p >> 8);
      p++;
    }
  *data = p;
  cnt = *sz;
  *sz = 0;
  return cnt;
}

/* Convert 16 bit little endian signed stereo data to 16 bit little endian
   signed mono data */
size_t sndcnv16_2monoLE(void **data,size_t *sz,void **outbuf)
{
  REGISTER unsigned char *src;
  REGISTER unsigned char *dest;
  int rc,count;
  signed short i;

  count = *sz / 2;
  if (count > SNDBUFSZ) { *sz  -= 2*SNDBUFSZ; count = SNDBUFSZ; }
  else                    *sz   = 0;
  rc      = count;
  src     = (unsigned char *) *data;
  *outbuf =
  dest    = miscplay_sndbuf;
  for (count /= 2; count--; ) {
    i = ((int)(src[0]) +
        256*(int)(src[1]) +
       (int)(src[2]) +
       256*(int)(src[3])) / 2;
    src += 4;
    *dest++ = (unsigned char)(i & 0xFF);
    *dest++ = (unsigned char)((i / 256) & 0xFF); }
  *data = src;
  return(rc);
}

/* Convert 16 bit big endian signed stereo data to 16 bit big endian
   signed mono data */
size_t sndcnv16_2monoBE(void **data,size_t *sz,void **outbuf)
{
  REGISTER unsigned char *src;
  REGISTER unsigned char *dest;
  int rc,count;
  signed short i;

  count = *sz / 2;
  if (count > SNDBUFSZ) { *sz  -= 2*SNDBUFSZ; count = SNDBUFSZ; }
  else                    *sz   = 0;
  rc      = count;
  src     = (unsigned char *) *data;
  *outbuf =
  dest    = miscplay_sndbuf;
  for (count /= 2; count--; ) {
    i = ((int)(src[1]) +
        256*(int)(src[0]) +
       (int)(src[3]) +
       256*(int)(src[2])) / 2;
    src += 4;
    *dest++ = (unsigned char)((i / 256) & 0xFF);
    *dest++ = (unsigned char)(i & 0xFF); }
  *data = src;
  return(rc);
}

/* Convert 16 bit little endian signed data to 8 bit unsigned data */
size_t sndcnv2byteLE(void **data,size_t *sz,void **outbuf)
{
  REGISTER unsigned char *src;
  REGISTER unsigned char *dest;
  int rc,count;

  count = *sz / 2;
  if (count > SNDBUFSZ) { *sz  -= 2*SNDBUFSZ; count = SNDBUFSZ; }
  else                    *sz   = 0;
  rc      = count;
  src     = (unsigned char *) *data;
  *outbuf =
  dest    = miscplay_sndbuf;
  while (count--) {
    *dest++ = (unsigned char)(((signed char *)src)[1] ^ (signed char)0x80);
    src += 2;
  }
  *data = src;
  return(rc);
}

/* Convert 16 bit big endian signed data to 8 bit unsigned data */
size_t sndcnv2byteBE(void **data,size_t *sz,void **outbuf)
{
  REGISTER unsigned char *src;
  REGISTER unsigned char *dest;
  int rc,count;

  count = *sz / 2;
  if (count > SNDBUFSZ) { *sz  -= 2*SNDBUFSZ; count = SNDBUFSZ; }
  else                    *sz   = 0;
  rc      = count;
  src     = (unsigned char *) *data;
  *outbuf =
  dest    = miscplay_sndbuf;
  while (count--) {
    *dest++ = (unsigned char)(((signed char *)src)[0] ^ (signed char)0x80);
    src += 2;
  }
  *data = src;
  return(rc);
}

/* Convert 16 bit little endian signed stereo data to 8 bit unsigned
   mono data */
size_t sndcnv2monobyteLE(void **data,size_t *sz,void **outbuf)
{
  REGISTER unsigned char *src;
  REGISTER unsigned char *dest;
  int rc,count;

  count = *sz / 4;
  if (count > SNDBUFSZ) { *sz  -= 4*SNDBUFSZ; count = SNDBUFSZ; }
  else                    *sz   = 0;
  rc      = count;
  src     = (unsigned char *) *data;
  *outbuf =
  dest    = miscplay_sndbuf;
  while (count--) {
    *dest++ = (unsigned char)(((int)((signed char *)src)[1] +
                              (int)((signed char *)src)[3]) / 2 ^ 0x80);
    src += 4;
  }
  *data = src;
  return(rc);
}

/* Convert 16 bit big endian signed stereo data to 8 bit unsigned
   mono data */
size_t sndcnv2monobyteBE(void **data,size_t *sz,void **outbuf)
{
  REGISTER unsigned char *src;
  REGISTER unsigned char *dest;
  int rc,count;

  count = *sz / 4;
  if (count > SNDBUFSZ) { *sz  -= 4*SNDBUFSZ; count = SNDBUFSZ; }
  else                    *sz   = 0;
  rc      = count;
  src     = (unsigned char *) *data;
  *outbuf =
  dest    = miscplay_sndbuf;
  while (count--) {
    *dest++ = (unsigned char)(((int)((signed char *)src)[0] +
                              (int)((signed char *)src)[2]) / 2 ^ 0x80);
    src += 4;
  }
  *data = src;
  return(rc);
}

/* Look at the header of the sound file and try to determine the format;
   we can recognize files in VOC, WAVE, and, Sun/DEC-audio format--- everything
   else is assumed to be raw 8 bit unsigned data sampled at 8kHz */
fmtType analyze_format(unsigned char *format,int *fmt,int *speed,
                             int *tracks,
                             size_t (**parsesndfile)(void **,size_t *sz,
                                                     void **))
{
  /* Keep compatibility with Linux 68k, etc. by not relying on byte-sex  */
  if (!memcmp(format,"Creative Voice File\x1A\x1A\x00",22) &&
              (format[22]+256*format[23]) ==
      ((0x1233-format[24]-256*format[25])&0xFFFF)) { /* VOC */
    *fmt          = AFMT_U8;
    *speed        = 8000;
    *tracks       = 2;
    *parsesndfile = parsevoc;
    return(fmtVoc); }
  else if (!memcmp(format,"RIFF",4) &&
          !memcmp(format+8,"WAVEfmt ",8)) { /* WAVE */
    if (memcmp(format+20,"\001\000\001"/* PCM mono */,4) &&
       memcmp(format+20,"\001\000\002"/* PCM stereo */,4))
      return(fmtIllegal);
    *fmt          = (format[32]/(*tracks = format[22])) == 1 ?
                    AFMT_U8 : AFMT_S16_LE;
    /* Keep compatibility with Linux 68k, etc. by not relying on byte-sex  */
    *speed        = format[24]+256*(format[25]+256*
                                   (format[26]+256*format[27]));
    *parsesndfile = parsewave;
    return(fmtWave); }
  else if (!memcmp(format,".snd",4)) { /* Sun Audio (big endian) */
    if (format[7]+256*(format[6]+256*(format[5]+256*format[4])) < 24) {
      *fmt          = AFMT_MU_LAW;
      *speed        = 8000;
      *tracks       = 1;
      *parsesndfile = parsesundecaudio;
      return(fmtSunAudio); }
    if      (!memcmp(format+12,"\000\000\000\001",4)) *fmt = AFMT_MU_LAW;
    else if (!memcmp(format+12,"\000\000\000\002",4)) *fmt = AFMT_S8;
    else if (!memcmp(format+12,"\000\000\000\003",4)) *fmt = AFMT_S16_BE;
    else return(fmtIllegal);
    /* Keep compatibility with Linux 68k, etc. by not relying on byte-sex  */
    *speed        = format[19]+256*(format[18]+256*
                                   (format[17]+256*format[16]));
    *tracks       = format[23];
    *parsesndfile = parsesundecaudio;
    return(fmtSunAudio); }
  else if (!memcmp(format,".sd",4)) { /* DEC Audio (little endian) */
    if (format[4]+256*(format[5]+256*(format[6]+256*format[7])) < 24) {
      *fmt          = AFMT_MU_LAW;
      *speed        = 8000;
      *tracks       = 1;
      *parsesndfile = parsesundecaudio;
      return(fmtSunAudio); }
    if      (!memcmp(format+12,"\001\000\000",4)) *fmt = AFMT_MU_LAW;
    else if (!memcmp(format+12,"\002\000\000",4)) *fmt = AFMT_S8;
    else if (!memcmp(format+12,"\003\000\000",4)) *fmt = AFMT_S16_LE;
    else return(fmtIllegal);
    /* Keep compatibility with Linux 68k, etc. by not relying on byte-sex  */
    *speed        = format[16]+256*(format[17]+256*
                                   (format[18]+256*format[19]));
    *tracks       = format[20];
    *parsesndfile = parsesundecaudio;
    return(fmtSunAudio); }
  else {
    *fmt          = AFMT_U8;
    *speed        = 8000;
    *tracks       = 1;
    *parsesndfile = parseraw;
    return(fmtRaw); }
}
