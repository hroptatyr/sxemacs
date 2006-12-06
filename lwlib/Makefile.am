## This file is part of SXEmacs.

## SXEmacs is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.

## SXEmacs is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.

## You should have received a copy of the GNU General Public License
## along with SXEmacs; see the file COPYING.  If not, write to
## the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
## Boston, MA 02111-1307, USA.

## Process this file with automake to produce Makefile.in

sxesrcdir = $(top_srcdir)/src

lwlib_objs = @lwlib_objs@

AM_CFLAGS = 
AM_CPPFLAGS = $(c_switch_all)
AM_LDFLAGS = @ld_switch_general@

top_builddir = @top_builddir@

ETAGS = $(top_builddir)/lib-src/etags
TAGS_DEPENDENCIES = $(ETAGS)

REGEX_C = $(sxesrcdir)/regex.c $(sxesrcdir)/regex.h
REGEX_CF = -DINHIBIT_STRING_HEADER
ALLOCA_C = $(sxesrcdir)/alloca.c
GETOPT_C = getopt.c getopt1.c getopt.h

## stuff we create here
noinst_LIBRARIES = liblw.a
liblw_a_SOURCES = lwlib-internal.h lwlib.h lwlib-utils.h \
	lwlib.c lwlib-utils.c lwlib-config.c
EXTRA_liblw_a_SOURCES = \
	lwlib-Xaw.h lwlib-Xaw.c \
	lwlib-Xlw.h lwlib-Xlw.c \
	lwlib-Xm.h lwlib-Xm.c \
	xlwmenu.h xlwmenuP.h xlwmenu.c \
	xlwscrollbar.h xlwscrollbarP.h xlwscrollbar.c \
	xlwtabs.h xlwtabsP.h xlwtabs.c \
	xlwradio.h xlwradioP.h xlwradio.c \
	xlwcheckbox.h xlwcheckboxP.h xlwcheckbox.c \
	xlwgauge.h xlwgaugeP.h xlwgauge.c \
	xlwgcs.h xlwgcs.c
liblw_a_DEPENDENCIES = $(lwlib_objs)
liblw_a_LIBADD = $(lwlib_objs)

CLEANFILES = core* stage1 stage2

stage1: $(noinst_LIBRARIES)
	@touch $@

stage2: stage1
	@touch $@

all: stage1 stage2

# 
# Help the SXEmacs developers get nice post-processed source files

## Create preprocessor output (debugging purposes only)
.c.i:
	$(COMPILE) -E -o $@ $<

## Create assembler output (debugging purposes only)
.c.s:
	$(COMPILE) -S -c $(cflags) $<
