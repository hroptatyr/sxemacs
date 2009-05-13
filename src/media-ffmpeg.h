/* media-ffmpeg.h - analyse all kinds of streams

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

#ifndef INCLUDED_media_ffmpeg_h_
#define INCLUDED_media_ffmpeg_h_

#include "media.h"
#if defined HAVE_LIBAVFORMAT_AVFORMAT_H
# include <libavformat/avformat.h>
#elif defined HAVE_FFMPEG_AVFORMAT_H
# include <ffmpeg/avformat.h>
#elif defined HAVE_AVFORMAT_H
# include <avformat.h>
#endif

/* Newer ffmpeg do not declare this macro... */
#ifndef DECLARE_ALIGNED
#ifdef __ICC
    #define DECLARE_ALIGNED(n,t,v)      t v __attribute__ ((aligned (n)))
#elif defined(__GNUC__)
    #define DECLARE_ALIGNED(n,t,v)      t v __attribute__ ((aligned (n)))
#elif defined(HAVE_INLINE_ASM)
    #error The asm code needs alignment, but we do not know how to do it for this compiler.
#else
    #define DECLARE_ALIGNED(n,t,v)      t v
#endif
#endif

extern char *media_ffmpeg_streaminfo(Lisp_Media_Stream*);

extern Lisp_Object Qffmpeg;
extern Lisp_Object media_ffmpeg_available_formats(void);

DECLARE_MEDIA_DRIVER(media_ffmpeg);
DECLARE_MEDIA_DRIVER(new_media_ffmpeg);

#endif	/* INCLUDED_media_ffmpeg_h_ */
