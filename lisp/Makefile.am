## SXEmacs Makefile.am

## Copyright (C) 2007 Steve Youngs.

## This file is part of SXEmacs.

## SXEmacs is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation, either version 3 of the License, or (at your
## option) any later version.

## SXEmacs is distributed in the hope that it will be
## useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.

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
top_srcdir = @top_srcdir@
abs_top_builddir = @abs_top_builddir@
abs_top_srcdir = @abs_top_srcdir@

sxesrcdir = $(top_srcdir)/src
bldsrcdir = $(top_builddir)/src
sxelispdir = $(abs_top_srcdir)/lisp
bldlispdir = $(abs_top_builddir)/lisp
bldffidir = $(bldlispdir)/ffi
bldmuledir = $(bldlispdir)/mule
bldtermdir = $(bldlispdir)/term
sxelwlibdir = $(top_srcdir)/src/ui/lwlib
bldlwlibdir = $(top_builddir)/src/ui/lwlib

lispffidir = $(lispdir)/ffi
lispmuledir = $(lispdir)/mule
lisptermdir = $(lispdir)/term

statlispdir = ${datadir}/${inststaticdir}
sxepkgdir = $(statlispdir)/sxemacs-packages
xepkgdir = $(statlispdir)/xemacs-packages
mulepkgdir = $(statlispdir)/mule-packages
sitepkgdir = $(statlispdir)/site-packages

ETAGS = $(top_builddir)/lib-src/etags
TAGS_DEPENDENCIES = $(ETAGS)
ETAGS_ARGS =

sxepkg_DATA =
xepkg_DATA =
mulepkg_DATA =
sitepkg_DATA =

subdirs = ffi mule term

nocompile_bldchainels =							\
	build-autoloads.el compile-core.el loadup-el.el loadup.el       \
	site-load.el update-elc.el update-elc-2.el very-early-lisp.el
compile_bldchainels =							\
	autoload.el dump-paths.el dumped-lisp.el find-paths.el          \
	finder.el make-docfile.el setup-paths.el startup.el
bldchainels = $(nocompile_bldchainels) $(compile_bldchainels)
corelispels =								\
	abbrev.el about.el alist.el apropos.el auto-save.el		\
	auto-show.el backquote.el blessmail.el buff-menu.el buffer.el	\
	build-rpt.el byte-optimize.el bytecomp-runtime.el bytecomp.el	\
	callers-of-rpt.el cl-compat.el cl-extra.el cl-macs.el cl-seq.el \
	cl.el cmdloop.el code-cmds.el code-files.el code-process.el	\
	coding.el compat.el config.el console.el cus-dep.el		\
	cus-edit.el cus-face.el cus-file.el cus-load.el cus-start.el	\
	custom.el derived.el device.el dialog-items.el dialog.el	\
	disass.el disp-table.el dragdrop.el easy-mmode.el easymenu.el	\
	emod-utils.el etags.el events.el extents.el faces.el ffi.el	\
	files-nomule.el files.el fill.el float-sup.el font-lock.el	\
	font-menu.el font.el fontl-hooks.el format.el frame.el          \
	glyphs.el gnuserv.el gpm.el gui.el gutter-items.el gutter.el	\
	help-macro.el help-nomule.el help.el hyper-apropos.el indent.el	\
	info.el isearch-mode.el iso8859-1.el issue-tracker.el		\
	itimer-autosave.el itimer.el keydefs.el keymap.el ldap.el	\
	lib-complete.el lisp-initd.el lisp-mnt.el lisp-mode.el lisp.el	\
	list-mode.el loaddefs.el loadhist.el map-ynp.el			\
	menubar-items.el menubar.el minibuf.el misc.el mode-motion.el	\
	modeline.el mouse.el movemail.el multicast.el mwheel.el		\
	number.el objects.el obsolete.el package-admin.el		\
	package-get.el package-info.el package-ui.el packages.el	\
	page.el paragraphs.el paths.el picture.el printer.el		\
	process.el rect.el regexp-opt.el register.el replace.el         \
	resize-minibuffer.el scrollbar.el select.el shadow.el simple.el \
	sound.el specifier.el subr.el symbol-syntax.el symbols.el       \
	syntax.el text-mode.el text-props.el toolbar-items.el           \
	toolbar.el tty-init.el undo-stack.el userlock.el version.el     \
	view-less.el wid-browse.el wid-edit.el widget.el		\
	window-xemacs.el window.el x-color.el x-compose.el x-faces.el	\
	x-font-menu.el x-init.el x-iso8859-1.el x-misc.el x-mouse.el	\
	x-scrollbar.el x-select.el x-win-sun.el x-win-xfree86.el

ffilispels =								\
	ffi/ffi-curl.el ffi/ffi-gcrypt.el ffi/ffi-libc.el		\
	ffi/ffi-taglib.el ffi/ffi-wand.el ffi/ffi-magic.el		\
	ffi/ffi-dbus.el ffi/ffi-sqlite.el
mulelispels =								\
	mule/arabic.el mule/canna-leim.el mule/chinese.el		\
	mule/cyrillic.el mule/english.el mule/ethiopic.el		\
	mule/european.el mule/greek.el mule/hebrew.el mule/japanese.el	\
	mule/kinsoku.el mule/korean.el mule/latin.el mule/misc-lang.el	\
	mule/mule-category.el mule/ccl.el mule/mule-charset.el	\
	mule/mule-cmds.el mule/mule-coding.el mule/mule-help.el		\
	mule/mule-init.el mule/mule-misc.el mule/mule-tty-init.el	\
	mule/mule-x-init.el mule/thai-xtis-chars.el mule/thai-xtis.el	\
	mule/viet-chars.el mule/vietnamese.el

termlispels =								\
	term/AT386.el term/apollo.el term/bg-mouse.el term/bobcat.el	\
	term/internal.el term/keyswap.el term/linux.el term/lk201.el	\
	term/news.el term/scoansi.el term/sun-mouse.el term/sun.el	\
	term/sup-mouse.el term/tvi970.el term/vt-control.el		\
	term/vt100-led.el term/vt100.el term/vt102.el term/vt125.el	\
	term/vt200.el term/vt201.el term/vt220.el term/vt240.el		\
	term/vt300.el term/vt320.el term/vt400.el term/vt420.el		\
	term/wyse50.el term/xterm.el

builtels =                                                              \
	auto-autoloads.el custom-load.el finder-inf.el                  \
	custom-defines.el

ffibuiltels =                                                           \
	ffi/auto-autoloads.el ffi/custom-load.el                        \
	ffi/custom-defines.el

mulebuiltels =                                                          \
	mule/auto-autoloads.el mule/custom-load.el                      \
	mule/custom-defines.el


if HAVE_FFI
builtels += $(ffibuiltels)
endif
if WITH_MULE
builtels += $(mulebuiltels)
endif

nocompile_elfiles =
compile_elfiles = $(compile_bldchainels) $(corelispels) $(termlispels)
if HAVE_FFI
compile_elfiles += $(ffilispels)
endif
if WITH_MULE
compile_elfiles +=  $(mulelispels)
endif
elfiles = $(nocompile_elfiles) $(compile_elfiles)

nobase_dist_lisp_DATA = $(elfiles)
nobase_lisp_DATA = $(builtels) $(elfiles:.el=.elc) $(builtels:.el=.elc)
noinst_DATA = auto.stamp update-elc.stamp autoc.stamp

EXTRA_DIST = $(nocompile_bldchainels) $(compile_bldchainels)

BUILT_FILES = auto.stamp update-elc.stamp autoc.stamp

CLEANFILES = core* stage1 stage2 $(builtels) $(builtels:.el=.elc)	\
	$(lisphier:=/*.el[c~]) *.stamp ffi/$(builtels)                  \
	ffi/$(builtels:.el=.elc)                                        \
	mule/$(builtels) mule/$(builtels:.el=.elc)                      \
	$(BUILT_FILES)

## custom rules
SXEMACS = $(bldsrcdir)/sxemacs
SXEMACS_DMP = $(SXEMACS).dmp
AALFILE = $(bldlispdir)/auto-autoloads.el
BITCH = $(SXEMACS) -batch
SXE_DYLD_PATH = $(bldsrcdir):$(bldsrcdir)/$(LT_OBJDIR):$(bldlwlibdir):$(bldlwlibdir)/$(LT_OBJDIR):$(DESTDIR)$(libdir)
BATCHENV = DYLD_LIBRARY_PATH=$(SXE_DYLD_PATH):$$DYLD_LIBRARY_PATH:$(DYLD_LIBRARY_PATH) \
	LD_LIBRARY_PATH=$(SXE_DYLD_PATH):$$LD_LIBRARY_PATH:$(LD_LIBRARY_PATH) \
	SHLIB_PATH=$(SXE_DYLD_PATH):$$SHLIB_PATH:$(SHLIB_PATH)
BITCHENV = $(BATCHENV) \
	EMACSPACKAGEPATH= \
	SOURCE_TREE_ROOT=$(abs_top_srcdir) \
	BUILD_TREE_ROOT=$(abs_top_builddir)
BATCH = $(BITCHENV) $(BITCH)
PUREBATCH = $(BATCH) -nd
RM = rm -f

lisphier = . term
if HAVE_FFI
lisphier += ffi
endif
if WITH_MULE
lisphier += mule
endif

stamps = bldchn.stamp core.stamp term.stamp
if HAVE_FFI
stamps += ffi.stamp
endif
if WITH_MULE
stamps += mule.stamp
endif

tags: TAGS
TAGS: $(compile_elfiles) $(TAGS_DEPENDENCIES)
	for i in $(compile_elfiles); do unique="$$unique ${srcdir}/$$i"; done; \
		$(ETAGS) $$unique

bldchn.stamp: $(SXEMACS) $(compile_bldchainels) auto.stamp
	$(PUREBATCH) -l compile-core.el $(compile_bldchainels)
	@touch $@

core.stamp: $(SXEMACS) $(corelispels) auto.stamp
	$(PUREBATCH) -l compile-core.el $(corelispels)
	@touch $@

ffi.stamp: $(SXEMACS) $(ffilispels) auto.stamp
	$(PUREBATCH) -l compile-core.el $(ffilispels)
	@touch $@

mule.stamp: $(SXEMACS) $(mulelispels) auto.stamp
	$(PUREBATCH) -l compile-core.el $(mulelispels)
	@touch $@

term.stamp: $(SXEMACS) $(termlispels) auto.stamp
	$(PUREBATCH) -l compile-core.el $(termlispels)
	@touch $@

autoc.stamp: $(SXEMACS) auto.stamp update-elc.stamp
@make_ifeq@ (yes,$(shell test -e $(bldsrcdir)/sxemacs.dmp && echo "yes"))
	@echo "Building finder database ..."
	$(BATCH) -vanilla -no-autoloads \
		-l finder -f finder-compile-keywords
	@echo "Building finder database ...(done)"
	$(BATCHENV) $(BITCH) -no-autoloads -l config \
		-l bytecomp -f batch-byte-compile $(builtels)
	@touch $@
@make_endif@

$(builtels): auto.stamp
$(builtels:.el=.elc): autoc.stamp
$(elfiles:.el=.elc): update-elc.stamp

update-elc.stamp: $(SXEMACS) $(compile_elfiles) auto.stamp $(stamps)
	@touch $@

auto.stamp: $(SXEMACS) $(compile_elfiles)
	for i in $(lisphier); do mkdir -p $$i; done
	$(PUREBATCH) -l build-autoloads.el
	@touch $@

#SUFFIXES = .el .elc
#.el.elc: $(SXEMACS) $(SXEMACS_DMP) auto.stamp update-elc.stamp
#	$(BATCH) -l bytecomp -f batch-byte-compile-one-file-here $<

#stage1: auto-autoloads.el
#	@touch $@
#
#stage2: stage1 ##$(builtels)
#	@touch $@
#
#all: stage1 stage2

.NOTPARALLEL:
