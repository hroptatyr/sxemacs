/* media-internal.h - general routines related to playing sounds
 **
 ** Copyright (C) 1995,96 by Markus Gutschke (gutschk@math.uni-muenster.de)
 ** This was sawed out from version 1.3 of linuxplay.c by
 ** Robert Bihlmeyer <robbe@orcus.priv.at>.
 **
 ** Copyright (C) 2006 Sebastian Freundt
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

#ifndef INCLUDED_media_internal_h_
#define INCLUDED_media_internal_h_

#include <stdlib.h>
#include "media.h"

#define HEADERSZ  256		/* has to be at least as big as the biggest header   */
#define SNDBUFSZ  2048		/* has to be at least as big as HEADERSZ             */

/* Audio data formats from <linux/soundcard.h> */
#define AFMT_MU_LAW            0x00000001
#define AFMT_A_LAW             0x00000002
#define AFMT_IMA_ADPCM         0x00000004
#define AFMT_U8                        0x00000008
#define AFMT_S16_LE            0x00000010	/* Little endian signed 16 */
#define AFMT_S16_BE            0x00000020	/* Big endian signed 16 */
#define AFMT_S8                        0x00000040
#define AFMT_U16_LE            0x00000080	/* Little endian U16 */
#define AFMT_U16_BE            0x00000100	/* Big endian U16 */
#define AFMT_MPEG              0x00000200	/* MPEG (2) audio */

typedef enum { fmtIllegal, fmtRaw, fmtVoc, fmtWave, fmtSunAudio } fmtType;

size_t sndcnvnop(void **data, size_t * sz, void **outbuf);
size_t sndcnv8U_2mono(void **data, size_t * sz, void **outbuf);
size_t sndcnv8S_2mono(void **data, size_t * sz, void **outbuf);
size_t sndcnv2monounsigned(void **data, size_t * sz, void **outbuf);
size_t sndcnv2unsigned(void **data, size_t * sz, void **outbuf);
size_t sndcnvULaw_2linear(void **data, size_t * sz, void **outbuf);
size_t sndcnvULaw_2mono(void **data, size_t * sz, void **outbuf);
size_t sndcnv16swap(void **data, size_t * sz, void **outbuf);
size_t sndcnv16_2monoLE(void **data, size_t * sz, void **outbuf);
size_t sndcnv16_2monoBE(void **data, size_t * sz, void **outbuf);
size_t sndcnv2byteLE(void **data, size_t * sz, void **outbuf);
size_t sndcnv2byteBE(void **data, size_t * sz, void **outbuf);
size_t sndcnv2monobyteLE(void **data, size_t * sz, void **outbuf);
size_t sndcnv2monobyteBE(void **data, size_t * sz, void **outbuf);

fmtType analyze_format(unsigned char *format, int *fmt, int *speed,
		       int *tracks,
		       size_t(**parsesndfile) (void **, size_t * sz, void **));
void reset_parsestate(void);
int parse_wave_complete(void);

extern void media_internal_analyse_stream(Lisp_Media_Stream*);

#endif	/* INCLUDED_media_internal_h_ */
