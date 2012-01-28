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

top_build_prefix = $(top_builddir)/
sxesrcdir = $(top_srcdir)/src

AM_CFLAGS = -DXTSTRINGDEFINES
AM_CPPFLAGS = -I$(sxesrcdir) $(c_switch_all)
AM_LDFLAGS = $(ld_switch_all)

ETAGS = $(top_builddir)/lib-src/etags
TAGS_DEPENDENCIES = $(ETAGS)

REGEX_C = $(sxesrcdir)/regex.c $(sxesrcdir)/regex.h
REGEX_CF = -DINHIBIT_STRING_HEADER
ALLOCA_C = $(sxesrcdir)/alloca.c
GETOPT_C = getopt.c getopt1.c getopt.h

## stuff we create here
lib_LTLIBRARIES = libsxelw-Xt.la
libsxelw_Xt_la_SOURCES = lwlib-internal.h lwlib.h lwlib-utils.h \
	lwlib.c lwlib-utils.c lwlib-config.c
EXTRA_libsxelw_Xt_la_SOURCES = \
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
libsxelw_Xt_la_DEPENDENCIES = $(lwlib_objs:.o=.lo)
libsxelw_Xt_la_LIBADD = $(lwlib_objs:.o=.lo) \
	$(X_LIBS) $(ld_libs_window_system)
libsxelw_Xt_la_CPPFLAGS = $(AM_CPPFLAGS)
libsxelw_Xt_la_LDFLAGS = $(AM_LDFLAGS) $(lib_gcc) $(XCCLDFLAGS)
if WITH_BDWGC
libsxelw_Xt_la_CPPFLAGS += $(BDWGC_CPPFLAGS)
libsxelw_Xt_la_LDFLAGS += $(BDWGC_LDFLAGS) $(BDWGC_LIBS)
endif

CLEANFILES = core* stage1 stage2 $(lwlib_objs)

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
