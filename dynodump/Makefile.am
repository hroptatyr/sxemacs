## SXEmacs Makefile.am

## Copyright (C) 2007 Steve Youngs.

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

SUBDIRS =
if DESCEND_DYNO_I386
SUBDIRS += i386
endif
if DESCEND_DYNO_PPC
SUBDIRS += ppc
endif
if DESCEND_DYNO_SPARC
SUBDIRS += sparc
endif

archlib_PROGRAMS = dynodump.so
dynodump_so_SOURCES = dynodump.c syms.c
dynodump_so_CPPFLAGS = -I${srcdir} -I${srcdir}/$(dynodump_arch)
noinst_HEADERS = _dynodump.h i386/machdep.h ppc/machdep.h sparc/machdep.h
dynodump_so_LDADD = $(dynodump_arch)/libdynostuff.a
dynodump_so_DEPENDENCIES = $(dynodump_LDADD)
dynodump_so_LINK = PATH=/usr/ccs/bin:/bin:$$PATH \
	ld -lelf -lmapmalloc -G 

# 
# Help the SXEmacs developers get nice post-processed source files

## Create preprocessor output (debugging purposes only)
.c.i:
	$(COMPILE) -E -o $@ $<

## Create assembler output (debugging purposes only)
.c.s:
	$(COMPILE) -S -c $(cflags) $<
