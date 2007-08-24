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

# Help the Developers and yourself. Just use the C locale and settings
# for the compilation. They can still be overriden by make LANG=<whatever>
# but that is general a not very good idea
LANG=C
LC_ALL=C

builddir = @builddir@
srcdir = @srcdir@
top_builddir = @top_builddir@
top_srcdir = @top_srcdir@
abs_top_builddir = @abs_top_builddir@
abs_top_srcdir = @abs_top_srcdir@

AM_CFLAGS = -Demacs $(OPENSSL_CFLAGS)
AM_CPPFLAGS = -I$(srcdir) -I$(builddir) -I$(top_builddir) $(c_switch_all) \
	$(DB_CPPFLAGS) $(ENT_CPPFLAGS) $(FFI_CPPFLAGS) $(MM_CPPFLAGS) \
	$(OPENSSL_CPPFLAGS) $(X_CFLAGS)
AM_LDFLAGS = $(OPENSSL_LDFLAGS) $(ENT_LDFLAGS)
## can't add MM_CPPFLAGS to libmm_a_CPPFLAGS only because EXTRA_SOURCES
## are still built with AM_CPPFLAGS

headerdir = $(includedir)/$(instvarsepdir)
sheaderdir = $(headerdir)/s
mheaderdir = $(headerdir)/m
archlibdir = ${libdir}/${instvardir}/${configuration}

sxesrcdir = $(sxe_srcdir)/src
bldsrcdir = $(builddir)
sxelispdir = $(top_srcdir)/lisp
bldlispdir = $(top_builddir)/lisp
sxelwlibdir = $(top_srcdir)/lwlib
bldlwlibdir = $(top_builddir)/lwlib
sxeemoddir = $(top_srcdir)/modules
bldemoddir = $(top_builddir)/modules
sxelibsrcdir = $(abs_top_srcdir)/lib-src
bldlibsrcdir = $(abs_top_builddir)/lib-src

ETAGS = $(top_builddir)/lib-src/etags
TAGS_DEPENDENCIES = $(ETAGS)

bin_PROGRAMS = sxemacs
bin_SCRIPTS = sxemacs.dmp
archlib_DATA = config.values DOC

pdumplibs = libsxecore.a libsxeui.a libsxeevents.a libent.a libmule.a libmm.a libsxemem.a libdb.a
nodumplibs = libcruft.a
if USE_MEMALLOC
nodumplibs += libmemalloc.a
endif
if DESCEND_LWLIB
nodumplibs += libsxeX.a
endif
noinst_LIBRARIES = $(pdumplibs) $(nodumplibs)

acgen_headers = config.h sxe-paths.h
header_HEADERS = $(acgen_headers)		\
	$(libsxecore_a_headers)			\
	$(libsxeui_a_headers)			\
	$(libent_a_headers)			\
	$(libmule_a_headers)			\
	$(libmm_a_headers)			\
	$(libmemalloc_a_headers)		\
	$(libsxeevents_a_headers)		\
	emodules.h

sheader_HEADERS = s/*.h
mheader_HEADERS = m/*.h

BUILT_SOURCES = Emacs.ad.h sheap-adjust.h dump-id.c
EXTRA_DIST = Emacs.ad.h sheap-adjust.h
CLEANFILES = core* stage1 stage2 *.stamp $(BUILT_SOURCES) temacs \
	sxemacs.dmp DOC config.values

## binaries
sxemacs_SOURCES = $(header_HEADERS)
##$(LOCK_OBJ) $(RTC_patch_objs)
EXTRA_sxemacs_SOURCES = lastfile.c profile.c sheap.c pre-crt0.c ecrt0.c
sxemacs_lddep = $(start_files) $(pdumplibs) $(pdumplibs) $(nodumplibs)
if WITH_PDUMP
sxemacs_lddep += dump-id.o
endif
if BUILD_SHEAP
sxemacs_lddep += sheap.o
endif
if HAVE_SETITIMER
sxemacs_lddep += profile.o
endif
sxemacs_lddep += $(statmod_archives)
sxemacs_lddep += lastfile.o
if DESCEND_LWLIB
sxemacs_lddep += $(bldlwlibdir)/liblw.a
endif
sxemacs_LDFLAGS = $(AM_LDFLAGS) $(start_flags)
sxemacs_LDADD = $(sxemacs_lddep) $(OPENSSL_LIBS)
sxemacs_LDADD +=							\
	$(libsxecore_a_ldflags) $(libsxeui_a_ldflags)			\
	$(libent_a_ldflags) $(libmm_a_ldflags) $(libdb_a_ldflags)	\
	$(ld_switch_all) $(ld_libs_all) $(lib_gcc) $(FFI_LIBS) $(X_LIBS)
sxemacs_DEPENDENCIES = $(sxemacs_lddep)

all_sources =

## library stuff
libsxeevents_a_headers =						\
	events-mod.h events.h event-queue.h workers.h worker-asyneq.h
libsxeevents_a_sources =						\
	events.c event-stream.c event-unixoid.c
EXTRA_libsxeevents_a_SOURCES =						\
	event-queue.c workers.c worker-asyneq.c
libsxeevents_a_SOURCES = $(libsxeevents_a_headers) $(libsxeevents_a_sources)
libsxeevents_a_LIBADD = $(libsxeevents_objs)
libsxeevents_a_DEPENDENCIES = $(libsxeevents_a_LIBADD)
all_sources += $(libsxeevents_a_sources) $(libsxeevents_objs:.o=.c)

libsxemem_a_headers =							\
	blocktype.h
libsxemem_a_sources =							\
	blocktype.c
libsxemem_a_SOURCES = $(libsxemem_a_headers) $(libsxemem_a_sources)
all_sources += $(libsxemem_a_sources) $(libsxemem_objs:.o=.c)

libsxecore_a_headers =							\
	backtrace.h bloom.h broken-sun.h buffer.h dllist.h		\
	bufslots.h bytecode.h casetab.h chartab.h lisp-disunion.h	\
	lisp.h lrecord.h lstream.h commands.h dumper.h elhash.h		\
	dynacat.h extents.h general-slots.h hash.h iso-wide.h		\
	line-number.h macros.h mem-limits.h ndir.h opaque.h process.h	\
	procimpl.h rangetab.h regex.h semaphore.h skiplist.h		\
	specifier.h symeval.h symsinit.h syntax.h syscommctrl.h		\
	sysdep.h sysdir.h sysdll.h sysfile.h sysproc.h syspwd.h		\
	syssignal.h systime.h syswait.h \
	sxemacs.h
libsxecore_a_sources =							\
	abbrev.c alloc.c bloom.c buffer.c bytecode.c			\
	callint.c callproc.c casefiddle.c casetab.c chartab.c		\
	cmdloop.c cmds.c data.c dired.c dllist.c doc.c doprnt.c		\
	dynarr.c editfns.c elhash.c emacs.c eval.c dynacat.c		\
	extents.c fileio.c filemode.c fns.c general.c hash.c indent.c	\
	intl.c line-number.c lread.c lstream.c macros.c marker.c md5.c	\
	objects.c opaque.c print.c process.c process-unix.c rangetab.c	\
	regex.c search.c semaphore.c signal.c skiplist.c strftime.c	\
	symbols.c syntax.c sysdep.c undo.c
libsxecore_a_SOURCES = $(libsxecore_a_headers) $(libsxecore_a_sources)
EXTRA_libsxecore_a_SOURCES =					     \
	dumper.c realpath.c					     \
	unexaix.c unexalpha.c unexapollo.c unexconvex.c unexec.c     \
	unexelf.c unexelfsgi.c unexencap.c unexenix.c unexfreebsd.c  \
	unexfx2800.c unexhp9k3.c unexhp9k800.c unexmips.c unexnext.c \
	unexsni.c unexsol2-6.c unexsol2.c unexsunos4.c
libsxecore_a_LIBADD = $(libsxecore_objs)
libsxecore_a_DEPENDENCIES = $(libsxecore_a_LIBADD)
all_sources += $(libsxecore_a_sources) $(libsxecore_objs:.o=.c)

libsxeui_a_headers =							   \
	balloon_help.h bitmaps.h					   \
	cm.h conslots.h console-gtk.h console-stream.h console-tty.h	   \
	console-x.h console.h device.h dragdrop.h extw-Xlib.h extw-Xt.h	   \
	faces.h frame.h frameslots.h gccache-gtk.h getpagesize.h	   \
	gifrlib.h glyphs-gtk.h glyphs-x.h glyphs.h gpmevent.h gtk-xemacs.h \
	gui-gtk.h gui-x.h gui.h gutter.h imgproc.h insdel.h keymap.h	   \
	menubar.h objects-gtk.h objects-tty.h objects-x.h objects.h	   \
	offix-cursors.h offix-types.h offix.h redisplay.h scrollbar-gtk.h  \
	scrollbar-x.h scrollbar.h select.h systty.h toolbar.h tooltalk.h   \
	ui-gtk.h universe.h window.h winslots.h xgccache.h		   \
	xmmanagerp.h xmprimitivep.h xmu.h
libsxeui_a_sources = 							\
	console.c console-stream.c device.c faces.c font-lock.c         \
	frame.c glyphs.c glyphs-eimage.c glyphs-widget.c gui.c gutter.c \
	imgproc.c keymap.c insdel.c minibuf.c redisplay.c               \
	redisplay-output.c select.c specifier.c widget.c window.c
if DESCEND_LWLIB
libsxeui_a_headers += Emacs.ad.h
endif
libsxeui_a_SOURCES = $(libsxeui_a_headers) $(libsxeui_a_sources)
EXTRA_libsxeui_a_SOURCES =						\
	console-gtk.c device-gtk.c event-gtk.c frame-gtk.c		\
	objects-gtk.c redisplay-gtk.c glyphs-gtk.c select-gtk.c		\
	gccache-gtk.c gtk-xemacs.c ui-gtk.c				\
	menubar-gtk.c scrollbar-gtk.c dialog-gtk.c toolbar-gtk.c	\
	gui-gtk.c							\
	console-x.c device-x.c event-Xt.c frame-x.c			\
	glyphs-x.c objects-x.c redisplay-x.c select-x.c xgccache.c	\
	xmu.c balloon_help.c balloon-x.c				\
	menubar-x.c scrollbar-x.c dialog-x.c toolbar-x.c gui-x.c	\
	cm.c console-tty.c device-tty.c event-tty.c frame-tty.c		\
	objects-tty.c redisplay-tty.c gpmevent.c			\
	termcap.c terminfo.c tparam.c ui-byhand.c			\
	menubar.c scrollbar.c dialog.c toolbar.c
libsxeui_a_LIBADD = $(libsxeui_objs)
libsxeui_a_DEPENDENCIES = $(libsxeui_a_LIBADD)
all_sources += $(libsxeui_a_sources) $(libsxeui_objs:.o=.c)

libent_a_headers = \
	ent.h ent-int.h ent-float.h ent-indef.h ent-inf.h \
	ent-ecm.h ent-gaussian.h ent-gmp.h ent-mp.h ent-mpc.h \
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
	ent-gmp.c ent-mp.c ent-mpc.c ent-mpfr.c ent-pseumpc.c \
	ent-quatern.c
libent_a_CPPFLAGS = $(AM_CPPFLAGS) $(ENT_CPPFLAGS)
libent_a_LIBADD = $(libent_objs)
libent_a_DEPENDENCIES = $(libent_a_LIBADD)
libent_a_ldflags = $(ENT_LIBS)
all_sources += $(libent_a_sources) $(libent_objs:.o=.c)

libmule_a_headers = \
	file-coding.h mule-ccl.h mule-charset.h
libmule_a_sources =
libmule_a_SOURCES = $(libmule_a_headers) $(libmule_a_sources)
EXTRA_libmule_a_SOURCES =						\
	file-coding.c mule.c mule-canna.c mule-ccl.c mule-charset.c	\
	mule-wnnfns.c							\
	input-method-xlib.c input-method-motif.c
libmule_a_LIBADD = $(libmule_objs)
libmule_a_DEPENDENCIES = $(libmule_a_LIBADD)
all_sources += $(libmule_a_sources) $(libmule_objs:.o=.c)

libmm_a_headers = media.h sound.h \
	media-ffmpeg.h media-internal.h	media-mad.h media-sndfile.h \
	media-sox.h media-magic.h \
	sound-alsa.h sound-ao.h sound-arts.h sound-esd.h \
	sound-jack.h sound-nas.h sound-oss.h sound-pulse.h
## what about these?: nativesound.h sound-sunplay.h libsst.h libst.h
libmm_a_sources = \
	media.c sound.c
libmm_a_SOURCES = $(libmm_a_headers) $(libmm_a_sources)
EXTRA_libmm_a_SOURCES = \
	media-ffmpeg.c media-internal.c media-mad.c media-sndfile.c \
	media-sox.c media-magic.c \
	sound-alsa.c sound-ao.c sound-arts.c sound-esd.c \
	sound-jack.c sound-nas.c sound-oss.c sound-pulse.c \
	dgif_lib.c gif_io.c
libmm_a_CFLAGS = $(AM_CFLAGS) -Wall
libmm_a_CPPFLAGS = $(AM_CPPFLAGS)
libmm_a_LIBADD = $(libmm_objs)
libmm_a_DEPENDENCIES = $(libmm_a_LIBADD)
libmm_a_ldflags = $(MM_LIBS)
all_sources += $(libmm_a_sources) $(libmm_objs:.o=.c)

libdb_a_headers = database.h eldap.h postgresql.h
libdb_a_sources =
libdb_a_SOURCES = $(libdb_a_headers) $(libdb_a_sources)
EXTRA_libdb_a_SOURCES = \
	database.c eldap.c postgresql.c
libdb_a_CPPFLAGS = $(AM_CPPFLAGS)
libdb_a_LIBADD = $(libdb_objs)
libdb_a_DEPENDENCIES = $(libdb_a_LIBADD)
libdb_a_ldflags = $(DB_LDFLAGS) $(DB_LIBS)
all_sources += $(libdb_a_sources) $(libdb_objs:.o=.c)

libmemalloc_a_headers = 
libmemalloc_a_sources =
libmemalloc_a_SOURCES = $(libmemalloc_a_headers) $(libmemalloc_a_sources)
EXTRA_libmemalloc_a_SOURCES = \
	free-hook.c gmalloc.c malloc.c vm-limit.c ralloc.c
libmemalloc_a_LIBADD = $(libmemalloc_objs)
libmemalloc_a_DEPENDENCIES = $(libmemalloc_a_LIBADD)
libmemalloc_a_ldflags = $(MEMALLOC_LIBS)
all_sources += $(libmemalloc_a_sources) $(libmemalloc_objs:.o=.c)

libcruft_a_headers = debug.h effi.h emodules.h openssl.h
libcruft_a_sources =
libcruft_a_SOURCES = $(libcruft_a_headers) $(libcruft_a_sources)
EXTRA_libcruft_a_SOURCES =						\
	alloca.c debug.c effi.c emodules.c filelock.c getloadavg.c	\
	inline.c openssl.c strcat.c sysdll.c tests.c
libcruft_a_LIBADD = $(extra_objs)
libcruft_a_DEPENDENCIES = $(libcruft_a_LIBADD)
all_sources += $(libcruft_a_sources) $(extra_objs:.o=.c)

## just a trick :( we create these libraries to get the object files
if DESCEND_LWLIB
noinst_LIBRARIES += libTopLevelEmacsShell.a libTransientEmacsShell.a
libTopLevelEmacsShell_a_SOURCES = EmacsShell-sub.c
libTopLevelEmacsShell_a_CFLAGS = -DDEFINE_TOP_LEVEL_EMACS_SHELL
libTransientEmacsShell_a_SOURCES = EmacsShell-sub.c
libTransientEmacsShell_a_CFLAGS = -DDEFINE_TRANSIENT_EMACS_SHELL
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
libsxeX_a_DEPENDENCIES = libTopLevelEmacsShell.a libTransientEmacsShell.a
all_sources += $(libsxeX_a_sources) $(libsxeX_a_LIBADD:.o=.c)

### custom rules
if WITH_PDUMP
SXEMACS = $(bldsrcdir)/sxemacs
else
SXEMACS = $(bldsrcdir)/sxemacs.dmp
endif
PUREMACS = $(bldsrcdir)/temacs
AALFILE = $(bldlispdir)/auto-autoloads.el
AALCFILE = $(bldlispdir)/auto-autoloads.elc
BITCH = $(SXEMACS) -batch
PITCH = $(PUREMACS) -batch
BITCHENV = EMACSDEBUGPATHS=1 EMACSPACKAGEPATH= SOURCE_TREE_ROOT=$(sxe_srcdir) BUILD_TREE_ROOT=$(sxe_blddir)
BATCH = $(BITCHENV) $(BITCH)
PUREBATCH = $(BITCHENV) $(PITCH)
PUREBATCHGDB = $(BITCHENV) gdb --args $(PITCH)
RM = rm -f
CROSSMAKE = (cd $$(dirname $@) && $(MAKE) $(AM_MAKEFLAGS) $$(basename $@))

Emacs.ad.h: $(top_srcdir)/etc/Emacs.ad
	@echo "Producing \`src/Emacs.ad.h' from \`etc/Emacs.ad'."
	@(echo "/*	Do not edit this file!" ; \
	  echo "  	Automatically generated from $(top_srcdir)/etc/Emacs.ad" ; \
	  echo " */" ; \
	  $(SHELL) $(sxelibsrcdir)/ad2c $(top_srcdir)/etc/Emacs.ad ) > $@

sheap-adjust.h:
	@echo "Resetting \`src/sheap-adjust.h'."; \
	(echo "/*	Do not edit this file!" ; \
	 echo "  	Automatically generated by SXEmacs */" ; \
	 echo "#define SHEAP_ADJUSTMENT 0") > $@

dump-id.c: $(bldlibsrcdir)/make-dump-id
	$(bldlibsrcdir)/make-dump-id

$(bldlispdir)/auto-autoloads.el \
$(bldlispdir)/auto-autoloads.elc \
$(bldlispdir)/update-elc.FORCE.stamp \
$(bldlispdir)/update-elc.stamp: $(PUREMACS)
	$(CROSSMAKE)

$(emodblddir)/auto-autoloads.el:
	$(CROSSMAKE)

DOC: $(PUREMACS) $(AALFILE)
	$(PUREBATCH)  -l $(sxelispdir)/make-docfile.el -- \
		-o $(bldsrcdir)/DOC -d $(sxesrcdir) \
		-i $(sxe_srcdir)/site-packages \
		$(sxesrcdir)/*.c $(sxeemoddir)/*/*.c
	@touch $@

config.values: config.h
	@echo "Building config.values ..."
	@echo ";;; Do not edit this file!" > $@
	grep -e '^[^ ]*='\''.*'\''$$\|^#define ' $(top_builddir)/config.log | \
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
temacs: sxemacs
	test -f sxemacs && cp -f sxemacs $@

sxemacs.dmp: sxemacs $(PUREMACS) $(AALFILE) \
		$(bldlispdir)/update-elc.stamp \
		$(dist_archlib_DATA) DOC config.values
	$(RM) $@
	$(PUREBATCH) -l $(sxelispdir)/loadup.el dump
	SOURCE_TREE_ROOT= BUILD_TREE_ROOT= \
		$(BITCH) -f list-load-path-shadows

sxemacs.dmp.gdb: $(PUREMACS) $(AALFILE) $(bldlispdir)/update-elc.stamp \
		$(dist_archlib_DATA) DOC config.values
	$(RM) $@
	$(PUREBATCHGDB) -l $(sxelispdir)/loadup.el dump
	SOURCE_TREE_ROOT= BUILD_TREE_ROOT= \
		$(BITCH) -f list-load-path-shadows


### Produces strange banner
testdir = $(top_srcdir)/tests
testautomdir = $(testdir)/automated
tests_environment = $(BITCH) -l ${testautomdir}/test-harness.el \
	-f batch-test-emacs $(testautomdir)
##check_PROGRAMS = sxemacs
##TESTS_ENVIRONMENT = ${tests_environment}
##TESTS = .

## Use our former check target
check-am:
	$(tests_environment)

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
