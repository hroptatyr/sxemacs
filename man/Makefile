#  Makefile for man subdirectory in XEmacs
#   Copyright (C) 1995 Board of Trustees, University of Illinois
#   Copyright (C) 1994, 1995 Sun Microsystems.

# This file is part of XEmacs.

# XEmacs is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.

# XEmacs is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

# You should have received a copy of the GNU General Public License
# along with XEmacs; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

SHELL    = /bin/sh
MAKEINFO = makeinfo
TEXI2DVI = texi2dvi
RM = rm -f

.SUFFIXES:
.SUFFIXES: .info .texi .dvi

RECURSIVE_MAKE = $(MAKE) $(MFLAGS) MAKEINFO='$(MAKEINFO)' TEXI2DVI='$(TEXI2DVI)'

all : info

# Ughhh!  The things we do to have portable makefiles...

INFODIR = ../info

info_files = \
	$(INFODIR)/cl.info \
	$(INFODIR)/custom.info \
	$(INFODIR)/emodules.info \
	$(INFODIR)/external-widget.info \
	$(INFODIR)/info.info \
	$(INFODIR)/lispref.info \
	$(INFODIR)/internals.info \
	$(INFODIR)/new-users-guide.info \
	$(INFODIR)/standards.info \
	$(INFODIR)/term.info \
	$(INFODIR)/termcap.info \
	$(INFODIR)/texinfo.info \
	$(INFODIR)/widget.info \
	$(INFODIR)/xemacs.info \
	$(INFODIR)/xemacs-faq.info

dvi_files = \
	cl.dvi \
	custom.dvi \
	emodules.dvi \
	external-widget.dvi \
	info.dvi \
	lispref.dvi \
	internals.dvi \
	new-users-guide.dvi \
	standards.dvi \
	term.dvi \
	termcap.dvi \
	texinfo.dvi \
	widget.dvi \
	xemacs.dvi \
	xemacs-faq.dvi

xemacs-srcs = \
	xemacs/abbrevs.texi \
	xemacs/basic.texi \
	xemacs/buffers.texi \
	xemacs/building.texi \
	xemacs/calendar.texi \
	xemacs/cmdargs.texi \
	xemacs/custom.texi \
	xemacs/display.texi \
	xemacs/entering.texi \
	xemacs/files.texi \
	xemacs/fixit.texi \
	xemacs/frame.texi \
	xemacs/glossary.texi \
	xemacs/gnu.texi \
	xemacs/help.texi \
	xemacs/indent.texi \
	xemacs/keystrokes.texi \
	xemacs/killing.texi \
	xemacs/m-x.texi \
	xemacs/major.texi \
	xemacs/mark.texi \
	xemacs/menus.texi \
	xemacs/mini.texi \
	xemacs/misc.texi \
	xemacs/mouse.texi \
	xemacs/mule.texi \
	xemacs/new.texi \
	xemacs/packages.texi \
	xemacs/picture.texi \
	xemacs/programs.texi \
	xemacs/reading.texi \
	xemacs/regs.texi \
	xemacs/search.texi \
	xemacs/sending.texi \
	xemacs/startup.texi \
	xemacs/text.texi \
	xemacs/trouble.texi \
	xemacs/undo.texi \
	xemacs/windows.texi \
	xemacs/xemacs.texi

lispref-srcs = \
	lispref/abbrevs.texi \
	lispref/annotations.texi \
	lispref/back.texi \
	lispref/backups.texi \
	lispref/buffers.texi \
	lispref/building.texi \
	lispref/commands.texi \
	lispref/compile.texi \
	lispref/consoles-devices.texi \
	lispref/control.texi \
	lispref/customize.texi \
	lispref/databases.texi \
	lispref/debugging.texi \
	lispref/dialog.texi \
	lispref/display.texi \
	lispref/dragndrop.texi \
	lispref/edebug-inc.texi \
	lispref/edebug.texi \
	lispref/errors.texi \
	lispref/eval.texi \
	lispref/extents.texi \
	lispref/faces.texi \
	lispref/files.texi \
	lispref/frames.texi \
	lispref/functions.texi \
	lispref/glyphs.texi \
	lispref/hash-tables.texi \
	lispref/help.texi \
	lispref/hooks.texi \
	lispref/index.texi \
	lispref/internationalization.texi \
	lispref/intro.texi \
	lispref/keymaps.texi \
	lispref/ldap.texi \
	lispref/lispref.texi \
	lispref/lists.texi \
	lispref/loading.texi \
	lispref/locals.texi \
	lispref/macros.texi \
	lispref/maps.texi \
	lispref/markers.texi \
	lispref/menus.texi \
	lispref/minibuf.texi \
	lispref/modes.texi \
	lispref/mouse.texi \
	lispref/mule.texi \
	lispref/numbers.texi \
	lispref/objects.texi \
	lispref/os.texi \
	lispref/packaging.texi \
	lispref/positions.texi \
	lispref/processes.texi \
	lispref/range-tables.texi \
	lispref/scrollbars.texi \
	lispref/searching.texi \
	lispref/sequences.texi \
	lispref/specifiers.texi \
	lispref/streams.texi \
	lispref/strings.texi \
	lispref/symbols.texi \
	lispref/syntax.texi \
	lispref/text.texi \
	lispref/tips.texi \
	lispref/toolbar.texi \
	lispref/tooltalk.texi \
	lispref/variables.texi \
	lispref/windows.texi \
	lispref/x-windows.texi

internals-srcs = \
	internals/internals.texi

new-users-guide-srcs = \
	new-users-guide/custom1.texi \
	new-users-guide/custom2.texi \
	new-users-guide/edit.texi \
	new-users-guide/enter.texi \
	new-users-guide/files.texi \
	new-users-guide/help.texi \
	new-users-guide/modes.texi \
	new-users-guide/new-users-guide.texi \
	new-users-guide/region.texi \
	new-users-guide/search.texi \
	new-users-guide/xmenu.texi

$(INFODIR)/cl.info : cl.texi
	$(MAKEINFO) -o $(INFODIR)/cl.info cl.texi

$(INFODIR)/custom.info : custom.texi
	$(MAKEINFO) -o $(INFODIR)/custom.info custom.texi

$(INFODIR)/emodules.info : emodules.texi
	$(MAKEINFO) -o $(INFODIR)/emodules.info emodules.texi

$(INFODIR)/external-widget.info : external-widget.texi
	$(MAKEINFO) -o $(INFODIR)/external-widget.info external-widget.texi

$(INFODIR)/info.info : info.texi
	$(MAKEINFO) -o $(INFODIR)/info.info info.texi

$(INFODIR)/standards.info : standards.texi
	$(MAKEINFO) -o $(INFODIR)/standards.info standards.texi

$(INFODIR)/term.info : term.texi
	$(MAKEINFO) -o $(INFODIR)/term.info term.texi

$(INFODIR)/termcap.info : termcap.texi
	$(MAKEINFO) -o $(INFODIR)/termcap.info termcap.texi

$(INFODIR)/texinfo.info : texinfo.texi
	$(MAKEINFO) -o $(INFODIR)/texinfo.info texinfo.texi

$(INFODIR)/widget.info : widget.texi
	$(MAKEINFO) -o $(INFODIR)/widget.info widget.texi

$(INFODIR)/xemacs-faq.info : xemacs-faq.texi
	$(MAKEINFO) -o $(INFODIR)/xemacs-faq.info xemacs-faq.texi

# Manuals with their own subdirectory
$(INFODIR)/xemacs.info : $(xemacs-srcs)
	$(MAKEINFO) -P xemacs -o $(INFODIR)/xemacs.info xemacs/xemacs.texi

$(INFODIR)/lispref.info : $(lispref-srcs)
	$(MAKEINFO) -P lispref -o $(INFODIR)/lispref.info lispref/lispref.texi

$(INFODIR)/internals.info : $(internals-srcs)
	$(MAKEINFO) -P internals -o $(INFODIR)/internals.info internals/internals.texi

$(INFODIR)/new-users-guide.info : $(new-users-guide-srcs)
	$(MAKEINFO) -P new-users-guide -o $(INFODIR)/new-users-guide.info new-users-guide/new-users-guide.texi

# $(INFODIR)/w3.info : w3.texi
#	$(MAKEINFO) -o $(INFODIR)/w3.info w3.texi

# EMACS = ../src/xemacs
# EMACSFLAGS = -batch -q -no-site-file

# $(INFODIR)/vm.info : vm.texi
#	-$(EMACS) $(EMACSFLAGS) -insert vm.texi -l texinfmt \
#		-f texinfo-format-buffer -f save-buffer
#	-mv vm.info* $(INFODIR)/.

# special = # $(INFODIR)/vm.info $(INFODIR)/texinfo.info

xemacs : $(INFODIR)/xemacs.info
lispref : $(INFODIR)/lispref.info
internals : $(INFODIR)/internals.info
new-users-guide.info : $(INFODIR)/new-users-guide.info

.PHONY : xemacs lispref internals new-users-guide info dvi

info : $(info_files)

# tm: FRC.tm
#	cd ./tm && $(RECURSIVE_MAKE)
# gnats: FRC.gnats
#	cd ./gnats && $(RECURSIVE_MAKE)
# FRC.xemacs FRC.lispref FRC.new-users-guide FRC.internals FRC.tm FRC.gnats:

.PHONY: info dvi

.texi.dvi:
	-$(TEXI2DVI) $<

xemacs.dvi : $(xemacs-srcs)
	$(TEXI2DVI) -I xemacs xemacs/xemacs.texi

lispref.dvi : $(lispref-srcs)
	$(TEXI2DVI) -I lispref lispref/lispref.texi

internals.dvi : $(internals-srcs)
	$(TEXI2DVI) -I internals internals/internals.texi

new-users-guide.dvi : $(new-users-guide-srcs)
	$(TEXI2DVI) -I new-users-guide new-users-guide/new-users-guide.texi

dvi : $(dvi_files)

.PHONY: mostlyclean clean distclean realclean extraclean
mostlyclean:
	$(RM) *.toc *.aux *.log *.op *.cp *.cps *.fn *.fns
	$(RM) *.ky *.kys *.pg *.pgs *.tp *.tps *.vr *.vrs
clean: mostlyclean
	$(RM) core *.dvi
distclean: clean
realclean: distclean
extraclean: distclean
	$(RM) *~ \#* */*~ */\#*
