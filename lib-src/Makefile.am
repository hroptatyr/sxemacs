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

builddir = @builddir@
srcdir = @srcdir@
top_builddir = @top_builddir@
top_srcdir = @top_srcdir@
abs_top_builddir = @abs_top_builddir@
abs_top_srcdir = @abs_top_srcdir@

ETAGS = $(top_builddir)/lib-src/etags
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
GETOPT_C = getopt.c getopt1.c getopt.h

## stuff we create here
bin_PROGRAMS = gnuclient ellcc etags ctags2 b2m ootags
archlib_PROGRAMS = gnuserv fakemail wakeup profile make-docfile digest-doc \
	sorted-doc movemail cvtmail yow hexl mmencode
dist_bin_SCRIPTS = gnudoit gnuattach rcs-checkin
dist_archlib_SCRIPTS = rcs2log vcdiff gzip-el.sh add-big-package.sh
noinst_PROGRAMS = make-path make-dump-id
dist_noinst_SCRIPTS = ad2c
dist_header_HEADERS = ellcc.h

## how do we create
gnuclient_SOURCES = gnuserv.h gnuslib.c gnuclient.c
ellcc_SOURCES = ellcc.h ellcc.c
b2m_SOURCES = b2m.c
gnuserv_SOURCES = gnuserv.h gnuslib.c gnuserv.c
fakemail_SOURCES = fakemail.c
wakeup_SOURCES = wakeup.c
profile_SOURCES = profile.c
make_docfile_SOURCES = make-docfile.c
sorted_doc_SOURCES = sorted-doc.c
cvtmail_SOURCES = cvtmail.c
yow_SOURCES = yow.c
hexl_SOURCES = hexl.c
mmencode_SOURCES = mmencode.c
make_dump_id_SOURCES = make-dump-id.c

make_path_SOURCES = make-path.c
make_path_CFLAGS = -Demacs

digest_doc_SOURCES = digest-doc.c
digest_doc_CFLAGS = -Demacs

movemail_SOURCES = movemail.c pop.c pop.h $(GETOPT_C) $(REGEX_C)
movemail_CFLAGS = $(REGEX_CF)

ctags2_SOURCES = $(GETOPT_C) $(REGEX_C) etags.c
EXTRA_ctags2_SOURCES = $(ALLOCA_C)
ctags2_CFLAGS = $(REGEX_CF) -DCTAGS
if NEED_ALLOCA_C
ctags2_LDADD = $(ALLOCA_C)
endif
ctags2_DEPENDENCIES = $(ALLOCA)

etags_SOURCES = $(GETOPT_C) $(REGEX_C) etags.c
EXTRA_etags_SOURCES = $(ALLOCA_C)
etags_CFLAGS = $(REGEX_CF) -DETAGS
if NEED_ALLOCA_C
etags_LDADD = $(ALLOCA_C)
endif
etags_DEPENDENCIES = $(ALLOCA)

ootags_SOURCES = $(REGEX_C) ootags.c
ootags_CFLAGS = $(REGEX_CF) -DOOTAGS

## custom rules
RM = rm -f

stage1: $(bin_PROGRAMS) $(archlib_PROGRAMS) $(dist_bin_SCRIPTS) \
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
