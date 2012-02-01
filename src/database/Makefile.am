## SXEmacs - database Makefile.am

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

libsxedb_a_headers = database.h eldap.h postgresql.h
libsxedb_a_sources =
libsxedb_a_SOURCES = $(libsxedb_a_headers) $(libsxedb_a_sources)
EXTRA_libsxedb_a_SOURCES = \
	database.c eldap.c postgresql.c
libsxedb_a_CPPFLAGS = $(AM_CPPFLAGS) $(DB_CPPFLAGS) $(OPENSSL_CPPFLAGS)
libsxedb_a_LIBADD = $(libsxedb_objs)
libsxedb_a_DEPENDENCIES = $(libsxedb_a_LIBADD)
libsxedb_a_ldflags = $(DB_LDFLAGS) $(DB_LIBS)
all_sources += $(libsxedb_a_sources) $(libsxedb_objs:.o=.c)

header_HEADERS= $(acgen_headers) $(libsxedb_a_headers)

noinst_LIBRARIES=libsxedb.a

#
# Help the SXEmacs developers get nice post-processed source files

## Create preprocessor output (debugging purposes only)
.c.i:
	$(COMPILE) -E -o $@ $<

## Create assembler output (debugging purposes only)
.c.s:
	$(COMPILE) -S -c $(cflags) $<
