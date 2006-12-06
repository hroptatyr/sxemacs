/* media-mad.h - analyse audio files or streams

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

#ifndef INCLUDED_media_mad_h_
#define INCLUDED_media_mad_h_

#include "media.h"
#include <mad.h>

extern Lisp_Object Qmad;

typedef struct mad_decoder_s {

	struct mad_synth  *synth; 
	struct mad_stream *stream;
	struct mad_frame  *frame;
  
	int have_frame;

	int output_sampling_rate;
	int output_open;
	int output_mode;

	FILE *fp;
	media_data *sd;

} mad_decoder_t;

DECLARE_MEDIA_DRIVER(media_mad);

#endif	/* INCLUDED_media_mad_h_ */
