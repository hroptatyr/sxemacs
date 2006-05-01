/* sound-nas.c - play a sound over NAS

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

/* based largely on the AO NAS plugin by Antoine Mathys */

#ifndef INCLUDED_sound_nas_h_
#define INCLUDED_sound_nas_h_

#include "sound.h"
#include <audio/audiolib.h>

#define SOUND_NAS_BUF_SIZE 4096
struct sound_nas_data {
	AuServer* aud;
	AuFlowID flow;
	AuDeviceID dev;
	char *host;
	int buf_size;
	int buf_free;
};
typedef struct sound_nas_data sound_nas_data;

extern char *sound_nas_subprint(Lisp_Object);
extern sound_nas_data *sound_nas_create(Lisp_Object);
extern void sound_nas_finish(sound_nas_data*);

extern void nas_wait_for_sounds(void);

extern int sound_nas_play_stream(media_subthread*);

extern Lisp_Object Qnas;		/* initialised in sound.c */

#endif	/* INCLUDED_sound_nas_h_ */
