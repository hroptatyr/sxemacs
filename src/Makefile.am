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
abs_builddir = @abs_builddir@
abs_top_builddir = @abs_top_builddir@
abs_top_srcdir = @abs_top_srcdir@

AM_CFLAGS = -Demacs $(OPENSSL_CFLAGS) -DUSE_SXEMACS_CONFIG_H -DXTSTRINGDEFINES
AM_CPPFLAGS = -I$(srcdir) -I$(builddir) -I$(top_builddir) $(c_switch_general) $(LTDLINCL)
AM_LDFLAGS = $(OPENSSL_LDFLAGS) $(ENT_LDFLAGS) $(LIBLTDL) $(XCCLDFLAGS)

headerdir = $(includedir)/$(instvarsepdir)
sheaderdir = $(headerdir)/s
mheaderdir = $(headerdir)/m
archlibdir = ${libdir}/${instvardir}/${configuration}

sxesrcdir = $(sxe_srcdir)/src
bldsrcdir = $(builddir)
sxelispdir = $(top_srcdir)/lisp
bldlispdir = $(top_builddir)/lisp
sxelwlibdir = $(top_srcdir)/src/ui/lwlib
bldlwlibdir = $(top_builddir)/src/ui/lwlib
sxeemoddir = $(top_srcdir)/modules
bldemoddir = $(top_builddir)/modules
sxelibsrcdir = $(abs_top_srcdir)/lib-src
bldlibsrcdir = $(abs_top_builddir)/lib-src

ETAGS = $(top_builddir)/lib-src/etags
TAGS_DEPENDENCIES = $(ETAGS)

bin_PROGRAMS = sxemacs
noinst_DATA = sxemacs.dmp
archlib_DATA = config.values DOC

pdumplibs =
noinst_pdumplibs=
nodumplibs=
noinst_nodumplibs=
pdump_ldadd =

SUBDIRS=
sxe_subdirs=

pdumplibs        += libsxecore.a
noinst_pdumplibs += libsxecore.a
pdump_ldadd      += $(libsxecore_a_ldflags)

# Inverse order of dependency for order dependendant
# linkers...

if NEED_LIBSXEUIX11
pdumplibs += $(builddir)/ui/X11/libsxeuiX11.a
# sxe_subdirs += ui
endif

if NEED_LIBSXEUITTY
pdumplibs += $(builddir)/ui/TTY/libsxeuiTTY.a
# sxe_subdirs += ui
endif

pdumplibs += $(builddir)/ui/libsxeui.a
# pdump_ldadd +=
sxe_subdirs += ui

pdumplibs += $(builddir)/events/libsxeevents.a
# pdump_ldadd +=
sxe_subdirs += events

# @FIXME@ Should probably be conditional on whether we really have ent
# things or not :-)
pdumplibs += $(builddir)/ent/libent.a
pdump_ldadd += $(ENT_LIBS)
sxe_subdirs += ent

# @FIXME@ Should probably be conditional on whether we really have media
# things or not :-)
pdumplibs += $(builddir)/media/libmm.a
pdump_ldadd += $(MM_LIBS)
sxe_subdirs += media


pdumplibs   += $(builddir)/mem/libsxemem.a
sxe_subdirs += mem
pdump_ldadd += $(libsxemem_a_ldflags)

if NEED_LIBMULE
pdumplibs += $(builddir)/mule/libmule.a
# pdump_ldadd +=
sxe_subdirs += mule
endif

if NEED_LIBSXEDB
pdumplibs += $(builddir)/database/libsxedb.a
pdump_ldadd += $(DB_LDFLAGS) $(DB_LIBS)
sxe_subdirs += database
endif

nodumplibs += libcruft.a
noinst_nodumplibs += libcruft.a
if USE_MEMALLOC
nodumplibs  += $(builddir)/mem/libmemalloc.a
# No need to add
# sxe_subdirs += mem
# because libsxemem already is there
endif
if DESCEND_LWLIB
nodumplibs += $(builddir)/ui/X11/libsxeX.a
endif

SUBDIRS += $(sxe_subdirs)

noinst_LIBRARIES = $(noinst_pdumplibs) $(noinst_nodumplibs)


acgen_headers = config.h sxe-paths.h
header_HEADERS = $(acgen_headers)		\
	$(libsxecore_a_headers)			\
	emodules-ng.h				\
	sxe-utils.h

sheader_HEADERS = $(srcdir)/s/*.h
mheader_HEADERS = $(srcdir)/m/*.h

BUILT_SOURCES = dump-id.c
EXTRA_DIST =
CLEANFILES = core* stage1 stage2 *.stamp $(BUILT_SOURCES) \
	sxemacs.dmp DOC config.values

## binaries
sxemacs_SOURCES = $(header_HEADERS)
##$(LOCK_OBJ) $(RTC_patch_objs)
EXTRA_sxemacs_SOURCES = lastfile.c profile.c pre-crt0.c ecrt0.c
sxemacs_CPPFLAGS = $(AM_CPPFLAGS)
sxemacs_lddep = $(start_files) $(pdumplibs) $(nodumplibs)
if WITH_PDUMP
sxemacs_lddep += dump-id.o
endif
if HAVE_SETITIMER
sxemacs_lddep += profile.o
endif
sxemacs_lddep += $(statmod_archives)
sxemacs_lddep += lastfile.o
if DESCEND_LWLIB
sxemacs_lddep += $(bldlwlibdir)/libsxelw-Xt.la
endif
sxemacs_LDFLAGS = $(AM_LDFLAGS) $(start_flags)
## hm, rpath is gross, says njsf
#sxemacs_LDFLAGS += -Wl,-rpath,$(abs_top_builddir)/lwlib/$(LT_OBJDIR)
sxemacs_LDADD = $(sxemacs_lddep) $(OPENSSL_LIBS)
sxemacs_LDADD += $(sxemacs_ldadd)		\
		 $(ld_switch_all) $(ld_libs_all)\
		 $(lib_gcc) $(FFI_LIBS)		\
		 $(LIBLTDL)
if WITH_BDWGC
sxemacs_CPPFLAGS += $(BDWGC_CPPFLAGS)
sxemacs_LDFLAGS += $(BDWGC_LDFLAGS) $(BDWGC_LIBS)
endif
sxemacs_DEPENDENCIES = $(sxemacs_lddep)

all_sources =

## library stuff
libsxecore_a_headers =							\
	backtrace.h bloom.h broken-sun.h buffer.h dllist.h		\
	bufslots.h bytecode.h casetab.h category.h			\
	chartab.h lisp-disunion.h					\
	lisp.h lrecord.h lstream.h commands.h dict.h dumper.h elhash.h	\
	dynacat.h extents.h general-slots.h hash.h iso-wide.h		\
	line-number.h macros.h map.h mem-limits.h ndir.h opaque.h	\
	process.h							\
	procimpl.h rangetab.h regex.h semaphore.h seq.h skiplist.h	\
	specifier.h symeval.h symsinit.h syntax.h syscommctrl.h		\
	sysdep.h sysdir.h sysfile.h sysproc.h syspwd.h sysgrp.h 	\
	syssignal.h systime.h syswait.h \
	sxemacs.h
libsxecore_a_sources =							\
	abbrev.c alloc.c bloom.c buffer.c bytecode.c			\
	callint.c callproc.c casefiddle.c casetab.c category.c		\
	chartab.c							\
	cmdloop.c cmds.c data.c dict.c dired.c dllist.c doc.c doprnt.c	\
	dynarr.c editfns.c elhash.c emacs.c eval.c dynacat.c		\
	extents.c fileio.c filemode.c fns.c general.c hash.c indent.c	\
	intl.c line-number.c lread.c lstream.c macros.c map.c marker.c	\
	md5.c opaque.c print.c process.c process-unix.c rangetab.c	\
	regex.c search.c semaphore.c seq.c signal.c skiplist.c		\
	strftime.c symbols.c syntax.c sysdep.c undo.c
libsxecore_a_SOURCES = $(libsxecore_a_headers) $(libsxecore_a_sources)
EXTRA_libsxecore_a_SOURCES = dumper.c realpath.c
libsxecore_a_CPPFLAGS = $(AM_CPPFLAGS) $(X_CFLAGS)
libsxecore_a_LIBADD = $(libsxecore_objs)
libsxecore_a_DEPENDENCIES = $(libsxecore_a_LIBADD)
all_sources += $(libsxecore_a_sources) $(libsxecore_objs:.o=.c)

libcruft_a_headers = debug.h effi.h emodules-ng.h openssl.h
libcruft_a_sources =
libcruft_a_SOURCES = $(libcruft_a_headers) $(libcruft_a_sources)
EXTRA_libcruft_a_SOURCES =						\
	alloca.c debug.c effi.c emodules-ng.c				\
	filelock.c getloadavg.c inline.c openssl.c strcat.c tests.c
libcruft_a_CPPFLAGS =							\
	$(AM_CPPFLAGS) $(DB_CPPFLAGS) $(FFI_CPPFLAGS) $(X_CFLAGS)
libcruft_a_LIBADD = $(libcruft_objs)
libcruft_a_DEPENDENCIES = $(libcruft_a_LIBADD)
all_sources += $(libcruft_a_sources) $(extra_objs:.o=.c)


### custom rules
SXEMACS = $(bldsrcdir)/sxemacs
AALFILE = $(bldlispdir)/auto-autoloads.el
AALCFILE = $(bldlispdir)/auto-autoloads.elc
BITCH = $(SXEMACS) -batch
SXE_DYLD_PATH = $(bldsrcdir):$(bldsrcdir)/$(LT_OBJDIR):$(bldlwlibdir):$(bldlwlibdir)/$(LT_OBJDIR):$(DESTDIR)$(libdir)
BATCHENV = $(EXTRA_BATCHENV) \
	DYLD_LIBRARY_PATH=$(SXE_DYLD_PATH):$$DYLD_LIBRARY_PATH:$(DYLD_LIBRARY_PATH) \
	LD_LIBRARY_PATH=$(SXE_DYLD_PATH):$$LD_LIBRARY_PATH:$(LD_LIBRARY_PATH) \
	SHLIB_PATH=$(SXE_DYLD_PATH):$$SHLIB_PATH:$(SHLIB_PATH)
BITCHENV = $(BATCHENV) \
	EMACSDEBUGPATHS=1 EMACSPACKAGEPATH= \
	SOURCE_TREE_ROOT=$(sxe_srcdir) \
	BUILD_TREE_ROOT=$(sxe_blddir)
BATCH = $(BITCHENV) $(BITCH)
BATCHGDB = $(BITCHENV) gdb --args $(BITCH)
RM = rm -f
CROSSMAKE = (cd $$(dirname $@) && $(MAKE) $(AM_MAKEFLAGS) $$(basename $@))


dump-id.c: $(bldlibsrcdir)/make-dump-id
	$(bldlibsrcdir)/make-dump-id

#$(bldlispdir)/auto-autoloads.el \
#$(bldlispdir)/auto-autoloads.elc \
#$(bldlispdir)/update-elc.FORCE.stamp \
#$(bldlispdir)/update-elc.stamp: $(SXEMACS)
#	echo $(CROSSMAKE)
#
#$(emodblddir)/auto-autoloads.el:
#	echo $(CROSSMAKE)

DOC: $(SXEMACS) $(bldlispdir)/auto.stamp
	if test -e $(bldlispdir)/auto.stamp; then \
		$(BATCH) -nd  -l $(sxelispdir)/make-docfile.el -- \
			-o $(bldsrcdir)/DOC -d $(sxesrcdir) \
			-i $(sxe_srcdir)/site-packages \
			$(sxesrcdir)/*.c \
			$(sxesrcdir)/*/*.c \
			$(sxesrcdir)/*/*/*.c \
			$(sxeemoddir)/*/*.c; \
		touch $@; \
	fi

## The `grep -v 'lt_lt_'' here is just a quick and dirty bandaid fix,
## pure and simple.  These are duplicate entries that cause problems
## for #'config-value.  The real fix lies in #'config-values-hash-table
## or maybe in libtool somewhere. --SY.
EGREP = @EGREP@
config.values: config.h
	@echo "Building config.values ..."
	@echo ";;; Do not edit this file!" > $@
	grep '^[^ ]*='\''.*'\''$$\|^#define ' $(top_builddir)/config.log | \
		grep -v 'lt_lt_' | \
		sed \
		-e 's/='\''\(.*\)'\''/ "\1"/g' \
		-e 's/^#define //' \
		-e '/^[^ ]*(/d' \
		-e 's/^\([^ ]*\) \([^"].*[^"]\)$$/\1 "\2"/' \
		-e 's/^\([^ ]*\) "\([0-9]*\)"$$/\1 \2/' \
		-e 's/^\(HAVE\|WITH\|ERROR\|LWLIB\)_\([^ ]*\) 1$$/\1_\2 t/' \
		-e 's/^\([^ ]*\) *$$/\1 ""/' \
		-e 's/\\"/\\\\"/g' \
		| sort | uniq >> $@
## this is roughly what we do above
## transform foo=bar to foo "bar"
## remove #define's
## foo <anything but a single digit> -> foo "<anything>"
## foo "1234567890" -> foo 1234567890 (numbers != strings)
## HAVE_foo 1 -> HAVE_foo t (<- t is more useful in elisp)
## `foo ' -> `foo ""'

### DUMP PHASE
## another idea would be to create the real dump file upon installation,
## as in use the install-exec-hook and create a new file from the newly
## relinked sxemacs
## one day (when SXE is linked with its own shared libraries) we HAVE to
## go that way but at the moment it's okay the way it is
SXE_SHOW_DUMP = $$($(BITCHENV) $(DESTDIR)$(bindir)/sxemacs -sd)
SXEDMP_FILENAME=sxemacs.dmp

sxemacs.dmp: $(SXEMACS) $(archlib_DATA) $(bldlispdir)/update-elc.stamp
	if test -e "$(bldlispdir)/update-elc.stamp"; then \
		$(BITCHENV) $(SXEMACS) -nd -batch \
				-l $(sxelispdir)/loadup.el --dump $@; \
		if test -e "$(LT_OBJDIR)/$(LT_CONVENIENCE_PREFIX)sxemacs"; then \
			(cd $(LT_OBJDIR); \
			$(LN_S) -f "../$@" "$(LT_CONVENIENCE_PREFIX)$@"); \
		fi; \
		if test -e "$(LT_OBJDIR)/sxemacs"; then \
			(cd $(LT_OBJDIR); \
			$(LN_S) -f ../$@ $@); \
		fi; \
		$(BATCHENV) \
		SOURCE_TREE_ROOT= BUILD_TREE_ROOT= \
			$(BITCH) -l shadow -f list-load-path-shadows; \
	fi

## alibi targets
$(bldlispdir)/update-elc.stamp $(bldlispdir)/auto.stamp:
	:

## improve me, make me (the dump file) independent from the binary
## because of historical reasons we also delete $(bindir)/sxemacs.dmp here
install-exec-hook:
	(cd $(DESTDIR)$(bindir) && $(RM) sxemacs.dmp)
	(cd $(DESTDIR)$(archlibdir) && $(RM) sxemacs.dmp)
	(cd $(abs_builddir); \
	$(BITCHENV) $(DESTDIR)$(bindir)/sxemacs -nd -batch \
		-l $(sxelispdir)/loadup.el \
		--dump $(DESTDIR)$(archlibdir)/$(SXEDMP_FILENAME))

uninstall-hook:
	(cd $(DESTDIR)$(archlibdir) && $(RM) $(SXEDMP_FILENAME))


## someone who wants to use this actually needs repeat the fuss above
sxemacs.dmp.gdb: $(SXEMACS) $(AALFILE) \
		$(bldlispdir)/update-elc.stamp \
		$(archlib_DATA) DOC config.values
	$(RM) $@
	$(BATCHGDB) -l $(sxelispdir)/loadup.el dump
	SOURCE_TREE_ROOT= BUILD_TREE_ROOT= \
		$(BITCH) -f list-load-path-shadows


#stage1: $(bin_PROGRAMS)
#	@touch $@
#
#stage2: stage1 $(DUMP_TARGET)
#	@touch $@
#
#all: stage1 stage2

#
# Help the SXEmacs developers get nice post-processed source files

## Create preprocessor output (debugging purposes only)
.c.i:
	$(COMPILE) -E -o $@ $<

## Create assembler output (debugging purposes only)
.c.s:
	$(COMPILE) -S -c $(cflags) $<
