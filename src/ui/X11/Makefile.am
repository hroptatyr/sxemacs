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
sxelibsrcdir = $(abs_top_srcdir)/lib-src

AM_CFLAGS = -Demacs $(OPENSSL_CFLAGS) -DUSE_SXEMACS_CONFIG_H -DXTSTRINGDEFINES
AM_CPPFLAGS = -I$(srcdir) -I$(builddir) -I$(top_srcdir)/src -I$(top_builddir)/src -I$(top_builddir) $(c_switch_general) $(LTDLINCL)

headerdir = $(includedir)/$(instvarsepdir)
archlibdir = ${libdir}/${instvardir}/${configuration}


ETAGS = $(top_builddir)/lib-src/etags
TAGS_DEPENDENCIES = $(ETAGS)

all_sources=
noinst_LIBRARIES=
header_HEADERS=$(acgen_headers)

## just a trick :( we create these libraries to get the object files
if DESCEND_LWLIB
noinst_LIBRARIES += libTopLevelEmacsShell.a libTransientEmacsShell.a
libTopLevelEmacsShell_a_SOURCES = EmacsShell-sub.c
libTopLevelEmacsShell_a_CFLAGS = -DDEFINE_TOP_LEVEL_EMACS_SHELL
libTopLevelEmacsShell_a_CPPFLAGS = $(AM_CPPFLAGS) $(X_CFLAGS)
libTransientEmacsShell_a_SOURCES = EmacsShell-sub.c
libTransientEmacsShell_a_CFLAGS = -DDEFINE_TRANSIENT_EMACS_SHELL
libTransientEmacsShell_a_CPPFLAGS = $(AM_CPPFLAGS) $(X_CFLAGS)
endif
libsxeX_a_headers = \
	EmacsFrame.h EmacsFrameP.h EmacsManager.h EmacsManagerP.h	   \
	EmacsShell.h EmacsShellP.h ExternalClient.h ExternalClientP.h	   \
	ExternalShell.h ExternalShellP.h xintrinsic.h xintrinsicp.h
libsxeX_a_sources = \
	EmacsFrame.c EmacsShell.c EmacsManager.c
if WITH_EXTW
libsxeX_a_sources += ExternalShell.c
## extw-Xt-nonshared.c extw-Xlib-nonshared.c where are these????
endif
libsxeX_a_SOURCES = $(libsxeX_a_headers) $(libsxeX_a_sources)
libsxeX_a_LIBADD =				\
	$(libTransientEmacsShell_a_OBJECTS)	\
	$(libTopLevelEmacsShell_a_OBJECTS)
libsxeX_a_CPPFLAGS = $(AM_CPPFLAGS) $(X_CFLAGS)
libsxeX_a_DEPENDENCIES = libTopLevelEmacsShell.a libTransientEmacsShell.a
noinst_LIBRARIES += libsxeX.a
all_sources += $(libsxeX_a_sources) $(libsxeX_a_LIBADD:.o=.c)

libsxeuiX11_a_headers = \
	balloon_help.h						\
	console-x.h extw-Xlib.h extw-Xt.h			\
	glyphs-x.h gui-x.h objects-x.h xgccache.h		\
	offix-cursors.h offix-types.h offix.h scrollbar-x.h	\
	xmmanagerp.h xmprimitivep.h xmu.h
libsxeuiX11_a_sources =  \
	console-x.c device-x.c event-Xt.c frame-x.c			\
	glyphs-x.c objects-x.c redisplay-x.c select-x.c xgccache.c	\
	balloon_help.c balloon-x.c				\
	menubar-x.c scrollbar-x.c dialog-x.c toolbar-x.c gui-x.c
libsxeuiX11_a_SOURCES = $(libsxeuiX11_a_headers) $(libsxeuiX11_a_sources)
EXTRA_libsxeuiX11_a_SOURCES=xmu.c
libsxeuiX11_a_CPPFLAGS = $(AM_CPPFLAGS) $(c_switch_general) $(X_CFLAGS)
libsxeuiX11_a_LIBADD = $(libsxeuiX11_objs)
libsxeuiX11_a_DEPENDENCIES = $(libsxeuiX11_a_LIBADD)
all_sources += $(libsxeuiX11_a_sources) $(libsxeuiX11_objs:.o=.c)

noinst_LIBRARIES += libsxeuiX11.a

#
# Help the SXEmacs developers get nice post-processed source files

## Create preprocessor output (debugging purposes only)
.c.i:
	$(COMPILE) -E -o $@ $<

## Create assembler output (debugging purposes only)
.c.s:
	$(COMPILE) -S -c $(cflags) $<
