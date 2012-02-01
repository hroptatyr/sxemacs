## SXEmacs - Ent Makefile.am

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

libsxeevents_a_headers =						\
	events-mod.h events.h event-queue.h workers.h worker-asyneq.h
libsxeevents_a_sources =						\
	events.c event-stream.c event-unixoid.c
EXTRA_libsxeevents_a_SOURCES =						\
	event-queue.c workers.c worker-asyneq.c
libsxeevents_a_SOURCES = $(libsxeevents_a_headers) $(libsxeevents_a_sources)
libsxeevents_a_CPPFLAGS = $(AM_CPPFLAGS) $(X_CFLAGS)
libsxeevents_a_LIBADD = $(libsxeevents_objs)
libsxeevents_a_DEPENDENCIES = $(libsxeevents_a_LIBADD)
all_sources += $(libsxeevents_a_sources) $(libsxeevents_objs:.o=.c)

header_HEADERS = $(acgen_headers) $(libsxeevents_a_headers)


noinst_LIBRARIES=libsxeevents.a

#
# Help the SXEmacs developers get nice post-processed source files

## Create preprocessor output (debugging purposes only)
.c.i:
	$(COMPILE) -E -o $@ $<

## Create assembler output (debugging purposes only)
.c.s:
	$(COMPILE) -S -c $(cflags) $<
