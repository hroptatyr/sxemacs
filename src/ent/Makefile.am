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

libent_a_headers = \
	ent.h ent-int.h ent-float.h ent-indef.h ent-inf.h \
	ent-ecm.h ent-gaussian.h ent-gmp.h ent-mpc.h \
	ent-mpfr.h ent-pseumpc.h ent-quatern.h \
	ent-strflt.h \
	ent-nullary-op.h ent-unary-op.h ent-binary-op.h \
	ent-unary-rel.h ent-binary-rel.h ent-lift.h \
	ent-optable.h
libent_a_sources = \
	ent.c ent-int.c ent-float.c ent-indef.c floatfns.c \
	ent-nullary-op.c ent-unary-op.c ent-binary-op.c \
	ent-unary-rel.c ent-binary-rel.c ent-lift.c
libent_a_SOURCES = $(libent_a_headers) $(libent_a_sources)
EXTRA_libent_a_SOURCES = \
	ent-ecm.c  ent-gaussian.c \
	ent-gmp.c ent-mpc.c ent-mpfr.c ent-pseumpc.c \
	ent-quatern.c
libent_a_CPPFLAGS = $(AM_CPPFLAGS) $(ENT_CPPFLAGS)
libent_a_LIBADD = $(libent_objs)
libent_a_DEPENDENCIES = $(libent_a_LIBADD)
libent_a_ldflags = $(ENT_LIBS)
all_sources += $(libent_a_sources) $(libent_objs:.o=.c)

header_HEADERS = $(acgen_headers) $(libent_a_headers)


noinst_LIBRARIES=libent.a

#
# Help the SXEmacs developers get nice post-processed source files

## Create preprocessor output (debugging purposes only)
.c.i:
	$(COMPILE) -E -o $@ $<

## Create assembler output (debugging purposes only)
.c.s:
	$(COMPILE) -S -c $(cflags) $<
