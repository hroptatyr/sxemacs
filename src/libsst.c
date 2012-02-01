/* libsst.c - SPARC sound tools library
**
** Copyright (C) 1989 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.

** Hacked on by jwz for emacs.

*/

/* Synched up with: Not in FSF. */

#ifdef emacs
#include <config.h>
#include "lisp.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "libsst.h"

#define AUDBUF 1024

int sst_open(play_level, record_level)
int play_level, record_level;
{
	int fd, i, gr, ger, gx;
	struct audio_ioctl ai;
	char *ep;

	fd = open("/dev/audio", O_RDWR);
	if (fd < 0) {
		perror("sst_open: open /dev/audio");
		return (fd);
	}
#ifdef AUDIOSETQSIZE		/* This no longer exists as of 4.1.2. */

	/* Shrink audio device's queue size, to cut down time delay. */
	i = AUDBUF;
	if (ioctl(fd, AUDIOSETQSIZE, &i) < 0) {
		perror("sst_open: SETQSIZE");
		return (fd);
	}
#endif				/* AUDIOSETQSIZE */

	/* Set gains.  -10 <= ger <= 18,  -18 <= gr <= 12,  -18 <= gx <= 12. */
	if (!play_level) {
		play_level = 75;
		if ((ep = getenv("SST_PLAY")) != NULL) {
			play_level = atoi(ep);
			if (play_level < 0 || play_level > 99) {
				warn("sst_open: SST_PLAY must be between 0 and 99");
				return (-1);
			}
		}
	}
	if (!record_level) {
		record_level = 75;
		if ((ep = getenv("SST_RECORD")) != NULL) {
			record_level = atoi(ep);
			if (record_level < 0 || record_level > 99) {
				warn("sst_open: SST_RECORD must be between 0 and 99");
				return (-1);
			}
		}
	}

	play_level = play_level * 59 / 100 - 28;
	ger = play_level / 2;
	gr = play_level - ger;
	if (ger < -10) {
		ger = -10;
		gr = play_level - ger;
	}
	if (gr > 12) {
		gr = 12;
		ger = play_level - gr;
	}
	gx = record_level * 31 / 100 - 18;
	sst_set_gr(fd, gr);
	sst_set_ger(fd, ger);
	sst_set_gx(fd, gx);

	/*  Initialize the MMR2 register to send the output to either
	 **  the speaker or the earphone jack, depending on SST_EARPHONES.
	 */
	ai.control = AUDIO_MAP_MMR2;
	if (ioctl(fd, AUDIOGETREG, &ai) < 0) {
		perror("sst_open: GETREG MMR2");
		return (-1);
	}
	if ((ep = getenv("SST_EARPHONES")) != NULL)
		ai.data[0] &= ~AUDIO_MMR2_BITS_LS;
	else
		ai.data[0] |= AUDIO_MMR2_BITS_LS;
	if (ioctl(fd, AUDIOSETREG, &ai) < 0) {
		perror("sst_open: SETREG MMR2");
		return (fd);
	}

	return fd;
}

void sst_close(fd)
int fd;
{
	struct audio_ioctl ai;

	ai.control = AUDIO_MAP_MMR1;
	ai.data[0] = 0;
	if (ioctl(fd, AUDIOSETREG, &ai) < 0) {
		perror("sst_close: SETREG MMR1");
	}
	ai.control = AUDIO_MAP_MMR2;
	ai.data[0] = 0;
	if (ioctl(fd, AUDIOSETREG, &ai) < 0) {
		perror("sst_close: SETREG MMR2");
	}
	close(fd);
}

/* These are tables of values to be loaded into various gain registers.
*/

static unsigned char ger_table[][2] = {
	0xaa, 0xaa,		/* -10db */
	0x79, 0xac,
	0x41, 0x99,
	0x9c, 0xde,
	0x74, 0x9c,		/* -6db */
	0x6a, 0xae,
	0xab, 0xdf,
	0x64, 0xab,
	0x2a, 0xbd,
	0x5c, 0xce,
	0x00, 0x99,		/* 0db */
	0x43, 0xdd,
	0x52, 0xef,
	0x55, 0x42,
	0x31, 0xdd,
	0x43, 0x1f,
	0x40, 0xdd,		/* 6db */
	0x44, 0x0f,
	0x31, 0x1f,
	0x10, 0xdd,
	0x41, 0x0f,
	0x60, 0x0b,
	0x42, 0x10,		/* 12db */
	0x11, 0x0f,
	0x72, 0x00,
	0x21, 0x10,
	0x22, 0x00,
	0x00, 0x0b,
	0x00, 0x0f,		/* 18db */
};

static unsigned char gr_gx_table[][2] = {
	0x8b, 0x7c,		/* -18db */
	0x8b, 0x35,
	0x8b, 0x24,
	0x91, 0x23,
	0x91, 0x2a,
	0x91, 0x3b,
	0x91, 0xf9,		/* -12db */
	0x91, 0xb6,
	0x91, 0xa4,
	0x92, 0x32,
	0x92, 0xaa,
	0x93, 0xb3,
	0x9f, 0x91,		/* -6db */
	0x9b, 0xf9,
	0x9a, 0x4a,
	0xa2, 0xa2,
	0xaa, 0xa3,
	0xbb, 0x52,
	0x08, 0x08,		/* 0db */
	0x3d, 0xac,
	0x25, 0x33,
	0x21, 0x22,
	0x12, 0xa2,
	0x11, 0x3b,
	0x10, 0xf2,		/* 6db */
	0x02, 0xca,
	0x01, 0x5a,
	0x01, 0x12,
	0x00, 0x32,
	0x00, 0x13,
	0x00, 0x0e,		/* 12db */
};

void sst_set_ger(fd, value)
int fd, value;
{
	struct audio_ioctl ai;

	if ((value < -10) || (value > 18)) {
		char buf[255];
		int sz = snprintf(buf, sizeof(buf), "sst_set_ger: GER %d out of range", value);
		assert(sz >= 0 && sz < sizeof(buf));
		warn(buf);
		return;
	}

	/*  Add 10 to the value to get the index into the table.  */
	ai.control = AUDIO_MAP_GER;
	ai.data[0] = ger_table[value + 10][1];
	ai.data[1] = ger_table[value + 10][0];

	if (ioctl(fd, AUDIOSETREG, &ai) < 0) {
		perror("sst_set_ger: SETREG GER");
	}

	ai.control = AUDIO_MAP_MMR1;
	if (ioctl(fd, AUDIOGETREG, &ai) < 0) {
		perror("sst_set_ger: GETREG MMR1");
	}
	ai.data[0] |= AUDIO_MMR1_BITS_LOAD_GER;
	if (ioctl(fd, AUDIOSETREG, &ai) < 0) {
		perror("sst_set_ger: SETREG MMR1");
	}
}

void sst_set_gr(fd, value)
int fd, value;
{
	struct audio_ioctl ai;

	if ((value < -18) || (value > 12)) {
		char buf[255];
		int sz = sprintf(buf, sizeof(buf), "sst_set_gr: GR %d out of range", value);
		assert(sz >= 0 && sz < sizeof(buf));
		warn(buf);
		return;
	}

	ai.control = AUDIO_MAP_GR;
	ai.data[0] = gr_gx_table[value + 18][1];
	ai.data[1] = gr_gx_table[value + 18][0];

	if (ioctl(fd, AUDIOSETREG, &ai) < 0) {
		perror("sst_set_gr: SETREG GR");
	}

	ai.control = AUDIO_MAP_MMR1;
	if (ioctl(fd, AUDIOGETREG, &ai) < 0) {
		perror("sst_set_gr: GETREG MMR1");
	}
	ai.data[0] |= AUDIO_MMR1_BITS_LOAD_GR;
	if (ioctl(fd, AUDIOSETREG, &ai) < 0) {
		perror("sst_set_gr: SETREG MMR1");
	}
}

void sst_set_gx(fd, value)
int fd, value;
{
	struct audio_ioctl ai;
	char buf[255];

	if ((value < -18) || (value > 12)) {
		int sz = snprintf(buf, sizeof(buf), "sst_set_gx: GX %d out of range", value);
		assert(sz >= 0 && sz < sizeof(buf));
		warn(buf);
		return;
	}

	/*  We add 18 to get the index into the table, since entry 0 represents
	 *  -18db.
	 */
	ai.control = AUDIO_MAP_GX;
	ai.data[0] = gr_gx_table[value + 18][1];
	ai.data[1] = gr_gx_table[value + 18][0];

	if (ioctl(fd, AUDIOSETREG, &ai) < 0) {
		perror("sst_set_gx: SETREG GX");
	}

	ai.control = AUDIO_MAP_MMR1;
	if (ioctl(fd, AUDIOGETREG, &ai) < 0) {
		perror("sst_set_gx: GETREG MMR1");
	}
	ai.data[0] |= AUDIO_MMR1_BITS_LOAD_GX;
	if (ioctl(fd, AUDIOSETREG, &ai) < 0) {
		perror("sst_set_gx: SETREG MMR1");
	}
}

void sst_tones(fd, dhz1, dhz2, thz, rhz, usec)
int fd, dhz1, dhz2, thz, rhz, usec;
{
	char buf[255];
	struct audio_ioctl ai;
	int dval1, dval2, tval, rval;
	unsigned char oldmmr2, newmmr2;

	if (dhz1 == 0)
		dval1 = 0;
	else {
		dval1 = (dhz1 * 128 + 63) / 1000;
		if ((dval1 < 1) || (dval1 > 255)) {
			int sz = snprintf(buf, sizeof(buf),
					  "sst_tones: dhz1 %d out of range", dhz1);
			assert(sz >= 0 && sz < sizeof(buf));
			warn(buf);
			return;
		}
	}

	if (dhz2 == 0)
		dval2 = 0;
	else {
		dval2 = (dhz2 * 128 + 63) / 1000;
		if ((dval2 < 1) || (dval2 > 255)) {
			int sz = snprintf(buf, sizeof(buf),
					  "sst_tones: dhz2 %d out of range", dhz2);
			assert(sz >= 0 && sz < sizeof(buf));
			warn(buf);
			return;
		}
	}

	if (thz == 0)
		tval = 0;
	else {
		tval = (thz * 128 + 63) / 2000;
		if ((tval < 1) || (tval > 255)) {
			int sz = snprintf(buf, sizeof(buf),
					  "sst_tones: thz %d out of range", thz);
			assert(sz >= 0 && sz < sizeof(buf));
			warn(buf);
			return;
		}
	}

	if (rhz == 0)
		rval = 0;
	else {
		rval = (rhz * 128 + 63) / 2000;
		if ((rval < 1) || (rval > 255)) {
			int sz = snprintf(buf, sizeof(buf),
					  "sst_tones: rhz %d out of range", dhz2);
			assert(sz >= 0 && sz < sizeof(buf));
			warn(buf);
			return;
		}
	}

	if ((dval1 != 0 || dval2 != 0) && (tval != 0 || rval != 0)) {
		int sz = snprintf(buf, sizeof(buf),
				  "sst_tones: cannot use DTMF and TONE or RINGER "
				  "at the same time",  dhz2);
		assert(sz >= 0 && sz < sizeof(buf));
		warn(buf);
		return;
	}

	if (tval != 0 && rval != 0) {
		int sz = sprintf(buf, sizeof(buf),
				 "sst_tones: cannot use TONE and RINGER at the same time",
				 dhz2);
		assert(sz >= 0 && sz < sizeof(buf));
		warn(buf);
		return;
	}

	ai.control = AUDIO_MAP_MMR2;
	if (ioctl(fd, AUDIOGETREG, &ai) < 0) {
		perror("sst_tones: GETREG MMR2");
	}
	oldmmr2 = newmmr2 = ai.data[0];

	if (dval1 != 0 || dval2 != 0) {
		newmmr2 |= AUDIO_MMR2_BITS_DTMF;
		ai.control = AUDIO_MAP_FTGR;
		ai.data[0] = dval1;
		ai.data[1] = dval2;
		if (ioctl(fd, AUDIOSETREG, &ai) < 0) {
			perror("sst_tones: SETREG FTGR");
		}
	}

	if (tval != 0) {
		newmmr2 |= AUDIO_MMR2_BITS_TONE;
		ai.control = AUDIO_MAP_FTGR;
		ai.data[0] = tval;
		ai.data[1] = 0;
		if (ioctl(fd, AUDIOSETREG, &ai) < 0) {
			perror("sst_tones: SETREG FTGR");
		}
	}

	if (rval != 0) {
		newmmr2 |= AUDIO_MMR2_BITS_RINGER;
		ai.control = AUDIO_MAP_FTGR;
		ai.data[0] = rval;
		ai.data[1] = 0;
		if (ioctl(fd, AUDIOSETREG, &ai) < 0) {
			perror("sst_tones: SETREG FTGR");
		}
	}

	ai.control = AUDIO_MAP_MMR2;
	ai.data[0] = newmmr2;
	if (ioctl(fd, AUDIOSETREG, &ai) < 0) {
		perror("sst_tones: SETREG MMR2");
	}

	usleep(usec);

	ai.data[0] = oldmmr2;
	if (ioctl(fd, AUDIOSETREG, &ai) < 0) {
		perror("sst_tones: SETREG MMR2");
	}
}

void sst_dtmf(fd, dial, usecper, usecpause)
int fd, usecper, usecpause;
char *dial;
{
	char *cp;

	for (cp = dial; *cp != '\0'; cp++) {
		switch (*cp) {
		case '1':
			sst_tones(fd, 703, 1211, 0, 0, usecper);
			break;
		case '2':
			sst_tones(fd, 703, 1336, 0, 0, usecper);
			break;
		case '3':
			sst_tones(fd, 703, 1492, 0, 0, usecper);
			break;
		case 'A':
			sst_tones(fd, 703, 1648, 0, 0, usecper);
			break;
		case '4':
			sst_tones(fd, 773, 1211, 0, 0, usecper);
			break;
		case '5':
			sst_tones(fd, 773, 1336, 0, 0, usecper);
			break;
		case '6':
			sst_tones(fd, 773, 1492, 0, 0, usecper);
			break;
		case 'B':
			sst_tones(fd, 773, 1648, 0, 0, usecper);
			break;
		case '7':
			sst_tones(fd, 859, 1211, 0, 0, usecper);
			break;
		case '8':
			sst_tones(fd, 859, 1336, 0, 0, usecper);
			break;
		case '9':
			sst_tones(fd, 859, 1492, 0, 0, usecper);
			break;
		case 'C':
			sst_tones(fd, 859, 1648, 0, 0, usecper);
			break;
		case '*':
			sst_tones(fd, 945, 1211, 0, 0, usecper);
			break;
		case '0':
			sst_tones(fd, 945, 1336, 0, 0, usecper);
			break;
		case '#':
			sst_tones(fd, 945, 1492, 0, 0, usecper);
			break;
		case 'D':
			sst_tones(fd, 945, 1648, 0, 0, usecper);
			break;

		case ' ':
		case '-':
		case '(':
		case ')':
		case '+':
			continue;	/* ignore */

		case ',':
			usleep(usecper);
			break;	/* big pause */

		default:
			{
				char buf[255];
				int sz = snprintf(buf, sizeof(buf),
						  "sst_dtmf: unknown dialing code '%c'",
						  *cp);
				assert(sz >= 0 && sz < sizeof(buf));
				warn(buf);
			}
		}
		usleep(usecpause);
	}
}
