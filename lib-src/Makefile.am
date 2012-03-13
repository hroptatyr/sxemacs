## SXEmacs Makefile.am

## Copyright (C) 2007 Steve Youngs

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
abs_top_builddir = @abs_top_builddir@
abs_top_srcdir = @abs_top_srcdir@

ETAGS = $(builddir)/etags
TAGS_DEPENDENCIES = $(ETAGS)

headerdir = $(includedir)/$(instvarsepdir)
archlibdir = ${libdir}/${instvardir}/${configuration}

sxesrcdir = $(top_srcdir)/src
bldsrcdir = $(top_builddir)/src
sxelispdir = $(abs_top_srcdir)/lisp
bldlispdir = $(abs_top_builddir)/lisp
sxelibsrcdir = $(top_srcdir)/lib-src
bldlibsrcdir = $(top_builddir)/lib-src

AM_CFLAGS =
AM_CPPFLAGS = -I$(sxesrcdir) $(c_switch_all)
AM_LDFLAGS = $(ld_switch_general) $(ld_libs_general)

REGEX_C = $(sxesrcdir)/regex.c $(sxesrcdir)/regex.h
REGEX_CF = -DINHIBIT_STRING_HEADER
ALLOCA_C = $(sxesrcdir)/alloca.c

## stuff we create here
bin_PROGRAMS = gnuclient etags ctags2 ootags
archlib_PROGRAMS = gnuserv fakemail wakeup profile make-docfile \
	sorted-doc movemail yow hexl mmencode
noinst_PROGRAMS = make-path make-dump-id

## how do we create
gnuclient_SOURCES = gnuserv.h gnuslib.c gnuclient.c
gnuserv_SOURCES = gnuserv.h gnuslib.c gnuserv.c
fakemail_SOURCES = fakemail.c
wakeup_SOURCES = wakeup.c
profile_SOURCES = profile.c
make_docfile_SOURCES = make-docfile.c
sorted_doc_SOURCES = sorted-doc.c
yow_SOURCES = yow.c
hexl_SOURCES = hexl.c
mmencode_SOURCES = mmencode.c
make_dump_id_SOURCES = make-dump-id.c

make_path_SOURCES = make-path.c
make_path_CFLAGS = -Demacs

movemail_SOURCES = movemail.c pop.c pop.h $(REGEX_C)
movemail_CFLAGS = $(AM_CFLAGS) $(REGEX_CF)
movemail_CPPFLAGS = $(AM_CPPFLAGS)
if WITH_BDWGC
movemail_CPPFLAGS += $(BDWGC_CPPFLAGS)
movemail_LDADD = $(BDWGC_LDFLAGS) $(BDWGC_LIBS)
endif

ctags2_SOURCES = $(REGEX_C) etags.c
EXTRA_ctags2_SOURCES = $(ALLOCA_C)
ctags2_CFLAGS = $(REGEX_CF) -DCTAGS
ctags2_LDADD =
if NEED_ALLOCA_C
ctags2_LDADD += $(ALLOCA_C)
endif
ctags2_CPPFLAGS = $(AM_CPPFLAGS)
if WITH_BDWGC
ctags2_CPPFLAGS += $(BDWGC_CPPFLAGS)
ctags2_LDADD += $(BDWGC_LDFLAGS) $(BDWGC_LIBS)
endif
ctags2_DEPENDENCIES = $(ALLOCA)

etags_SOURCES = $(REGEX_C) etags.c
EXTRA_etags_SOURCES = $(ALLOCA_C)
etags_CFLAGS = $(REGEX_CF) -DETAGS -DEMACS_NAME="\"SXEmacs\""
etags_LDADD =
if NEED_ALLOCA_C
etags_LDADD += $(ALLOCA_C)
endif
etags_CPPFLAGS = $(AM_CPPFLAGS)
if WITH_BDWGC
etags_CPPFLAGS += $(BDWGC_CPPFLAGS)
etags_LDADD += $(BDWGC_LDFLAGS) $(BDWGC_LIBS)
endif
etags_DEPENDENCIES = $(ALLOCA)

ootags_SOURCES = $(REGEX_C) ootags.c
ootags_CFLAGS = $(REGEX_CF) -DOOTAGS
ootags_CPPFLAGS = $(AM_CPPFLAGS)
if WITH_BDWGC
ootags_CPPFLAGS += $(BDWGC_CPPFLAGS)
ootags_LDADD = $(BDWGC_LDFLAGS) $(BDWGC_LIBS)
endif

## custom rules
RM = rm -f

stage1: $(bin_PROGRAMS) $(archlib_PROGRAMS)  \
	$(archlib_SCRIPTS) $(noinst_PROGRAMS) $(dist_archlib_DATA)
	@touch $@

stage2: stage1 ##DOC
	@touch $@

### dont use staged build
##all: stage1 stage2

CLEANFILES = core* stage1 stage2

install-exec-hook:
	cd $(DESTDIR)$(bindir) && \
	mv -f ctags2 ctags

uninstall-hook:
	cd $(DESTDIR)$(bindir) && \
	$(RM) ctags

#
# Help the SXEmacs developers get nice post-processed source files

## Create preprocessor output (debugging purposes only)
.c.i:
	$(COMPILE) -E -o $@ $<

## Create assembler output (debugging purposes only)
.c.s:
	$(COMPILE) -S -c $(cflags) $<
