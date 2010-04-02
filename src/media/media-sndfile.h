/* media-sndfile.h - analyse audio files or streams

   Copyright (C) 2006 Sebastian Freundt

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

#ifndef INCLUDED_media_sndfile_h_
#define INCLUDED_media_sndfile_h_

#include "media.h"
#include <sndfile.h>

extern Lisp_Object Qsndfile;

typedef struct {
	SNDFILE *sf;
	SF_INFO *sfinfo;
} media_sndfile_data;

/* funs */
extern void media_sndfile_analyse_stream(Lisp_Media_Stream*);

DECLARE_MEDIA_DRIVER(media_sndfile);

#endif	/* INCLUDED_media_sndfile_h_ */
