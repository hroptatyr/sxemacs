## SXEmacs - Media Makefile.am

## Copyright (C) 2010 Steve Youngs

## This file is part of SXEmacs.

## SXEmacs is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## SXEmacs is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

## Process this file with automake to produce Makefile.in

# Help the Developers and yourself. Just use the C locale and settings
# for the compilation. They can still be overriden by make LANG=<whatever>
# but that is general a not very good idea
LANG=C
LC_ALL=C

builddir = @builddir@
srcdir = @srcdir@
top_builddir = @top_builddir@
top_build_prefix = $(top_builddir)/
top_srcdir = @top_srcdir@
abs_builddir = @abs_builddir@
abs_top_builddir = @abs_top_builddir@
abs_top_srcdir = @abs_top_srcdir@

AM_CFLAGS = -Demacs $(OPENSSL_CFLAGS) -DUSE_SXEMACS_CONFIG_H -DXTSTRINGDEFINES
AM_CPPFLAGS = -I$(srcdir) -I$(builddir) -I$(top_srcdir)/src -I$(top_builddir)/src -I$(top_builddir) $(c_switch_general) $(LTDLINCL)

headerdir = $(includedir)/$(instvarsepdir)
archlibdir = ${libdir}/${instvardir}/${configuration}


ETAGS = $(top_builddir)/lib-src/etags
TAGS_DEPENDENCIES = $(ETAGS)

all_sources=

libmm_a_headers = media.h sound.h \
	media-ffmpeg.h media-internal.h	media-mad.h media-sndfile.h \
	media-sox.h media-magic.h \
	sound-alsa.h sound-ao.h sound-esd.h sound-jack.h \
	sound-nas.h sound-oss.h sound-pulse.h
## what about these?: nativesound.h sound-sunplay.h libsst.h libst.h
libmm_a_sources = \
	media.c sound.c
libmm_a_SOURCES = $(libmm_a_headers) $(libmm_a_sources)
EXTRA_libmm_a_SOURCES = \
	media-ffmpeg.c media-internal.c media-mad.c media-sndfile.c \
	media-sox.c media-magic.c \
	sound-alsa.c sound-ao.c sound-esd.c sound-jack.c \
	sound-nas.c sound-oss.c sound-pulse.c \
	dgif_lib.c gif_io.c
libmm_a_CFLAGS = $(AM_CFLAGS) -Wall
libmm_a_CPPFLAGS = $(AM_CPPFLAGS) $(c_switch_general) $(MM_CPPFLAGS) $(X_CFLAGS)
libmm_a_LIBADD = $(libmm_objs)
libmm_a_DEPENDENCIES = $(libmm_a_LIBADD)
libmm_a_ldflags = $(MM_LIBS)
all_sources += $(libmm_a_sources) $(libmm_objs:.o=.c)

header_HEADERS= $(acgen_headers) $(libmm_a_headers)

noinst_LIBRARIES=libmm.a


#
# Help the SXEmacs developers get nice post-processed source files

## Create preprocessor output (debugging purposes only)
.c.i:
	$(COMPILE) -E -o $@ $<

## Create assembler output (debugging purposes only)
.c.s:
	$(COMPILE) -S -c $(cflags) $<
