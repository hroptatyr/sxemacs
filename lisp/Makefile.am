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

EXTRA_DIST = ffi/Makefile.in mule/Makefile.in
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
	callers-of-rpt.el check-features.el cl-compat.el cl-extra.el	\
	cl-macs.el cl-seq.el cl.el cmdloop.el code-cmds.el		\
	code-files.el code-process.el coding.el compat.el config.el	\
	console.el cus-dep.el cus-edit.el cus-face.el cus-file.el	\
	cus-load.el cus-start.el custom.el derived.el device.el		\
	dialog-gtk.el dialog-items.el dialog.el disass.el		\
	disp-table.el dragdrop.el easymenu.el etags.el events.el	\
	extents.el faces.el ffi.el files-nomule.el files.el fill.el	\
	float-sup.el font-lock.el font-menu.el font.el fontl-hooks.el	\
	format.el frame.el gdk.el generic-widgets.el glade.el		\
	glyphs.el gnome-widgets.el gnome.el gnuserv.el gpm.el		\
	gtk-compose.el gtk-extra.el gtk-faces.el gtk-ffi.el		\
	gtk-file-dialog.el gtk-font-menu.el gtk-glyphs.el gtk-init.el	\
	gtk-iso8859-1.el gtk-marshal.el gtk-mouse.el gtk-package.el	\
	gtk-password-dialog.el gtk-select.el gtk-widget-accessors.el	\
	gtk-widgets.el gtk.el gui.el gutter-items.el gutter.el		\
	help-macro.el help-nomule.el help.el hyper-apropos.el		\
	indent.el info.el isearch-mode.el iso8859-1.el issue-tracker.el	\
	itimer-autosave.el itimer.el keydefs.el keymap.el ldap.el	\
	lib-complete.el lisp-initd.el lisp-mnt.el lisp-mode.el lisp.el	\
	list-mode.el loaddefs.el loadhist.el map-ynp.el			\
	menubar-items.el menubar.el minibuf.el misc.el mode-motion.el	\
	modeline.el mouse.el movemail.el multicast.el mwheel.el		\
	number.el objects.el obsolete.el package-admin.el		\
	package-get.el package-info.el package-ui.el packages.el	\
	page.el paragraphs.el paths.el picture.el printer.el		\
	process.el rect.el register.el replace.el resize-minibuffer.el  \
	scrollbar.el select.el shadow.el simple.el sound.el             \
	specifier.el subr.el symbol-syntax.el symbols.el syntax.el      \
	text-mode.el text-props.el toolbar-items.el toolbar.el          \
	tty-init.el undo-stack.el userlock.el version.el view-less.el	\
	wid-browse.el wid-edit.el widget.el widgets-gtk.el		\
	window-xemacs.el window.el x-color.el x-compose.el x-faces.el	\
	x-font-menu.el x-init.el x-iso8859-1.el x-misc.el x-mouse.el	\
	x-scrollbar.el x-select.el x-win-sun.el x-win-xfree86.el

ffilispels =								\
	ffi/ffi-curl.el ffi/ffi-gcrypt.el ffi/ffi-libc.el		\
	ffi/ffi-taglib.el ffi/ffi-wand.el
mulelispels =								\
	mule/arabic.el mule/canna-leim.el mule/chinese.el		\
	mule/cyrillic.el mule/english.el mule/ethiopic.el		\
	mule/european.el mule/greek.el mule/hebrew.el mule/japanese.el	\
	mule/kinsoku.el mule/korean.el mule/latin.el mule/misc-lang.el	\
	mule/mule-category.el mule/ccl.el mule/mule-charset.el  	\
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
	auto-autoloads.el finder-inf.el custom-load.el                  \
	custom-defines.el ffi/auto-autoloads.el ffi/custom-load.el      \
	ffi/custom-defines.el mule/auto-autoloads.el                    \
	mule/custom-load.el mule/custom-defines.el

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

CLEANFILES = core* stage1 stage2 $(builtels) $(builtels:.el=.elc)	\
	$(lisphier:=/*.el[c~]) *.stamp ffi/$(builtels)                  \
	ffi/$(builtels:.el=.elc)                                        \
	mule/$(builtels) mule/$(builtels:.el=.elc)

## custom rules
SXEMACS = $(bldsrcdir)/sxemacs
AALFILE = $(bldlispdir)/auto-autoloads.el
BITCH = $(SXEMACS) -batch
BITCHENV = EMACSPACKAGEPATH= SOURCE_TREE_ROOT=$(abs_top_srcdir) BUILD_TREE_ROOT=$(abs_top_builddir)
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

tags: TAGS
TAGS: $(compile_elfiles) $(TAGS_DEPENDENCIES)
	for i in $(compile_elfiles); do unique="$$unique ${srcdir}/$$i"; done; \
		$(ETAGS) $$unique

auto-autoloads.el: $(EMACS) $(compile_elfiles)
	$(PUREBATCH) -l build-autoloads.el
	@$(RM) $@~

# These next targets don't do anything.  The real work is done in
# auto-autoloads.el target
custom-load.el: $(EMACS) $(compile_elfiles)
	@touch $@

custom-defines.el: $(EMACS) $(compile_elfiles)
	@touch $@

ffi/auto-autoloads.el: $(EMACS) $(compile_elfiles)
	@touch $@

ffi/custom-load.el: $(EMACS) $(compile_elfiles)
	@touch $@

ffi/custom-defines.el: $(EMACS) $(compile_elfiles)
	@touch $@

mule/auto-autoloads.el: $(EMACS) $(compile_elfiles)
	@touch $@

mule/custom-load.el: $(EMACS) $(compile_elfiles)
	@touch $@

mule/custom-defines.el: $(EMACS) $(compile_elfiles)
	@touch $@

.PHONY: update-elc.FORCE.stamp
update-elc.FORCE.stamp \
update-elc.stamp: auto-autoloads.el $(compile_elfiles)
	for i in $(lisphier); do mkdir -p $$i; done
	$(PUREBATCH) -l compile-core.el $(compile_elfiles)
	@touch $@

SUFFIXES = .el .elc
.el.elc: $(SXEMACS) auto-autoloads.el
	$(BATCH) -l bytecomp -f batch-byte-compile-one-file-here $<

finder-inf.el:
	@echo "Building finder database ..."
	$(BATCH) -vanilla -no-autoloads \
		-l finder -f finder-compile-keywords
	@echo "Building finder database ...(done)"

#stage1: auto-autoloads.el
#	@touch $@
#
#stage2: stage1 ##$(builtels)
#	@touch $@
#
#all: stage1 stage2
