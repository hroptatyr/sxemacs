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

#include "media.h"
#if defined HAVE_SOX_H
# include <sox.h>
#elif defined HAVE_ST_H
# include <st.h>
#else
# error "How on earth did you get here?"
#endif

#if defined HAVE_SOX_FORMAT_T
# define sxe_sox_t	sox_format_t*
#elif defined HAVE_FT_T
# define sxe_sox_t	ft_t
#else
# error "How could you ever reach this?"
#endif

#if defined HAVE_SOX_SIGNALINFO_T
# define sxe_sox_signalinfo_t	sox_signalinfo_t
#elif defined HAVE_ST_SIGNALINFO_T
# define sxe_sox_signalinfo_t	st_signalinfo_t
#else
# error "Congrats! Y0u won 6 packs of V|AGRA! Go and have fun!"
#endif

#if defined HAVE_SOX_SSIZE_T
# define sxe_sox_ssize_t	sox_ssize_t
#elif defined HAVE_ST_SSIZE_T
# define sxe_sox_ssize_t	st_ssize_t
#else
# error "Uhoh! Crap crap crap.  I won't tell you what I've seen, but it's ..."
#endif

#if defined HAVE_SOX_SAMPLE_T
# define sxe_sox_sample_t	sox_sample_t
#elif defined HAVE_ST_SAMPLE_T
# define sxe_sox_sample_t	st_sample_t
#else
# error "Thou nutter!  Serves thee right!"
#endif

#if defined HAVE_SOX_H
# define sxe_sox_open_read	sox_open_read
# define sxe_sox_close		sox_close
# define sxe_sox_read		sox_read
# define sxe_sox_seek		sox_seek
#elif defined HAVE_ST_H
# define sxe_sox_open_read	st_open_read
# define sxe_sox_close		st_close
# define sxe_sox_read		st_read
# define sxe_sox_seek		st_seek
#else
# error "Nope! I'm upset now.  Gimme a pint or another to cheer me up!"
#endif

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
#elif defined ST_SIZE_BYTE &&			\
	defined ST_SIZE_8BIT &&		\
	defined ST_SIZE_16BIT &&		\
	defined ST_SIZE_32BIT &&		\
	defined ST_SIZE_64BIT
# define SXE_SIZE_BYTE		ST_SIZE_BYTE
# define SXE_SIZE_8BIT		ST_SIZE_8BIT
# define SXE_SIZE_16BIT		ST_SIZE_16BIT
# define SXE_SIZE_24BIT		ST_SIZE_24BIT
# define SXE_SIZE_32BIT		ST_SIZE_32BIT
# define SXE_SIZE_64BIT		ST_SIZE_64BIT
#else
# error "Guess what, I will compile myself as rootkit! It's too boring with you"
#endif

void cleanup(void);

extern Lisp_Object Qsox;

DECLARE_MEDIA_DRIVER(media_sox);

#endif	/* INCLUDED_media_sox_h_ */
