/* media-ffmpeg.h - analyse all kinds of streams

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

#ifndef INCLUDED_media_ffmpeg_h_
#define INCLUDED_media_ffmpeg_h_

#include "media.h"
#include <avformat.h>

extern char *media_ffmpeg_streaminfo(Lisp_Media_Stream*);

extern Lisp_Object Qffmpeg;
extern Lisp_Object media_ffmpeg_available_formats(void);

DECLARE_MEDIA_DRIVER(media_ffmpeg);

#endif	/* INCLUDED_media_ffmpeg_h_ */
