/* media-sox.h - analyse audio files or streams via sox

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

#ifndef INCLUDED_media_sox_h_
#define INCLUDED_media_sox_h_

#if defined HAVE_SOX_H
# include <sox.h>
#else
# error "How on earth did you get here?"
#endif

#if defined HAVE_SOX_FORMAT_T
# define sxe_sox_t	sox_format_t*
#else
# error "How could you ever reach this?"
#endif

#if defined HAVE_SOX_SIGNALINFO_T
# define sxe_sox_signalinfo_t	sox_signalinfo_t
#else
# error "Congrats! Y0u won 6 packs of V|AGRA! Go and have fun!"
#endif

#if defined HAVE_SOX_SSIZE_T
# define sxe_sox_ssize_t	sox_ssize_t
#else
# define sxe_sox_ssize_t        size_t
#endif

#if defined HAVE_SOX_SAMPLE_T
# define sxe_sox_sample_t	sox_sample_t
#else
# error "Thou nutter!  Serves thee right!"
#endif

#if defined HAVE_SOX_H
# define sxe_sox_open_read	sox_open_read
# define sxe_sox_close		sox_close
# define sxe_sox_read		sox_read
# define sxe_sox_seek		sox_seek
#else
# error "Nope! I'm upset now.  Gimme a pint or another to cheer me up!"
#endif

#include "media.h"

#if defined SOX_SIZE_BYTE &&			\
	defined SOX_SIZE_8BIT &&		\
	defined SOX_SIZE_16BIT &&		\
	defined SOX_SIZE_24BIT &&		\
	defined SOX_SIZE_32BIT &&		\
	defined SOX_SIZE_64BIT
# define SXE_SIZE_BYTE		SOX_SIZE_BYTE
# define SXE_SIZE_8BIT		SOX_SIZE_8BIT
# define SXE_SIZE_16BIT		SOX_SIZE_16BIT
# define SXE_SIZE_24BIT		SOX_SIZE_24BIT
# define SXE_SIZE_32BIT		SOX_SIZE_32BIT
# define SXE_SIZE_64BIT		SOX_SIZE_64BIT
#elif defined HAVE_SOX_SIGNALINFO_T_PRECISION
/* we don't need this bugger at all */
#else
# error "Guess what, I will compile myself as rootkit! It's too boring with you"
#endif

void cleanup(void);

extern Lisp_Object Qsox;

DECLARE_MEDIA_DRIVER(media_sox);

#endif	/* INCLUDED_media_sox_h_ */
