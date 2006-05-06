#  Makefile for man subdirectory in SXEmacs
#   Copyright (C) 1995 Board of Trustees, University of Illinois
#   Copyright (C) 1994, 1995 Sun Microsystems.

# This file is part of SXEmacs.

# SXEmacs is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.

# SXEmacs is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.

# You should have received a copy of the GNU General Public License
# along with SXEmacs; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

SHELL    = /bin/sh
MAKEINFO = makeinfo
TEXI2DVI = texi2dvi
DVI2PS   = dvips
DVI2PDF  = dvipdf
TEXI2HTML     = texi2html

RM = rm -f

.SUFFIXES:
.SUFFIXES: .info .texi .dvi

RECURSIVE_MAKE_ARGS = $(MAKE) $(MFLAGS) MAKEINFO='$(MAKEINFO)' TEXI2DVI='$(TEXI2DVI)'

all : info

# Ughhh!  The things we do to have portable makefiles...

INFODIR = ../info
HTMLDIR = ../html

info_files = \
	$(INFODIR)/cl.info \
	$(INFODIR)/custom.info \
	$(INFODIR)/emodules.info \
	$(INFODIR)/external-widget.info \
	$(INFODIR)/info.info \
	$(INFODIR)/lispref.info \
	$(INFODIR)/internals.info \
	$(INFODIR)/new-users-guide.info \
	$(INFODIR)/sppm.info \
	$(INFODIR)/standards.info \
	$(INFODIR)/term.info \
	$(INFODIR)/termcap.info \
	$(INFODIR)/texinfo.info \
	$(INFODIR)/widget.info \
	$(INFODIR)/sxemacs.info \
	$(INFODIR)/sxemacs-faq.info

html_files = \
	$(HTMLDIR)/cl.html \
	$(HTMLDIR)/custom.html \
	$(HTMLDIR)/emodules.html \
	$(HTMLDIR)/external-widget.html \
	$(HTMLDIR)/info.html \
	$(HTMLDIR)/lispref.html \
	$(HTMLDIR)/internals.html \
	$(HTMLDIR)/new-users-guide.html \
	$(HTMLDIR)/sppm.html \
	$(HTMLDIR)/standards.html \
	$(HTMLDIR)/term.html \
	$(HTMLDIR)/termcap.html \
	$(HTMLDIR)/texinfo.html \
	$(HTMLDIR)/widget.html \
	$(HTMLDIR)/sxemacs.html \
	$(HTMLDIR)/sxemacs-faq.html

dvi_files = \
	cl.dvi \
	custom.dvi \
	emodules.dvi \
	external-widget.dvi \
	info.dvi \
	lispref.dvi \
	internals.dvi \
	new-users-guide.dvi \
	sppm.dvi \
	standards.dvi \
	term.dvi \
	termcap.dvi \
	texinfo.dvi \
	widget.dvi \
	sxemacs.dvi \
	sxemacs-faq.dvi

ps_files = \
	cl.ps \
	custom.ps \
	emodules.ps \
	external-widget.ps \
	info.ps \
	lispref.ps \
	internals.ps \
	new-users-guide.ps \
	sppm.ps \
	standards.ps \
	term.ps \
	termcap.ps \
	texinfo.ps \
	widget.ps \
	sxemacs.ps \
	sxemacs-faq.ps

pdf_files = \
	cl.pdf \
	custom.pdf \
	emodules.pdf \
	external-widget.pdf \
	info.pdf \
	lispref.pdf \
	internals.pdf \
	new-users-guide.pdf \
	sppm.pdf \
	standards.pdf \
	term.pdf \
	termcap.pdf \
	texinfo.pdf \
	widget.pdf \
	sxemacs.pdf \
	sxemacs-faq.pdf

sxemacs-srcs = \
	sxemacs/abbrevs.texi \
	sxemacs/basic.texi \
	sxemacs/buffers.texi \
	sxemacs/building.texi \
	sxemacs/calendar.texi \
	sxemacs/cmdargs.texi \
	sxemacs/custom.texi \
	sxemacs/display.texi \
	sxemacs/entering.texi \
	sxemacs/files.texi \
	sxemacs/fixit.texi \
	sxemacs/frame.texi \
	sxemacs/glossary.texi \
	sxemacs/gnu.texi \
	sxemacs/help.texi \
	sxemacs/indent.texi \
	sxemacs/keystrokes.texi \
	sxemacs/killing.texi \
	sxemacs/m-x.texi \
	sxemacs/major.texi \
	sxemacs/mark.texi \
	sxemacs/menus.texi \
	sxemacs/mini.texi \
	sxemacs/misc.texi \
	sxemacs/mouse.texi \
	sxemacs/mule.texi \
	sxemacs/new.texi \
	sxemacs/packages.texi \
	sxemacs/picture.texi \
	sxemacs/programs.texi \
	sxemacs/reading.texi \
	sxemacs/regs.texi \
	sxemacs/search.texi \
	sxemacs/sending.texi \
	sxemacs/startup.texi \
	sxemacs/text.texi \
	sxemacs/trouble.texi \
	sxemacs/undo.texi \
	sxemacs/windows.texi \
	sxemacs/sxemacs.texi

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
	lispref/ent.texi \
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
	lispref/openssl.texi \
	lispref/os.texi \
	lispref/packaging.texi \
	lispref/positions.texi \
	lispref/postgresql.texi \
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

$(INFODIR)/sppm.info : sppm.texi
	$(MAKEINFO) -o $(INFODIR)/sppm.info sppm.texi

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

$(INFODIR)/sxemacs-faq.info : sxemacs-faq.texi
	$(MAKEINFO) -o $(INFODIR)/sxemacs-faq.info sxemacs-faq.texi

# Manuals with their own subdirectory
$(INFODIR)/sxemacs.info : $(sxemacs-srcs)
	$(MAKEINFO) -P sxemacs -o $(INFODIR)/sxemacs.info sxemacs/sxemacs.texi

$(INFODIR)/lispref.info : $(lispref-srcs)
	$(MAKEINFO) -P lispref -o $(INFODIR)/lispref.info lispref/lispref.texi

$(INFODIR)/internals.info : $(internals-srcs)
	$(MAKEINFO) -P internals -o $(INFODIR)/internals.info internals/internals.texi

$(INFODIR)/new-users-guide.info : $(new-users-guide-srcs)
	$(MAKEINFO) -P new-users-guide -o $(INFODIR)/new-users-guide.info new-users-guide/new-users-guide.texi

# $(INFODIR)/w3.info : w3.texi
#	$(MAKEINFO) -o $(INFODIR)/w3.info w3.texi


$(HTMLDIR)/cl.html : cl.texi
	$(TEXI2HTML)  -menu -number -split=section -subdir=../html $<

$(HTMLDIR)/custom.html : custom.texi
	$(TEXI2HTML)  -menu -number -split=section -subdir=../html $<

$(HTMLDIR)/emodules.html : emodules.texi
	$(TEXI2HTML)  -menu -number -split=section -subdir=../html $<

$(HTMLDIR)/external-widget.html : external-widget.texi
	$(TEXI2HTML)  -menu -number -split=section -subdir=../html $<

$(HTMLDIR)/info.html : info.texi
	$(TEXI2HTML)  -menu -number -split=section -subdir=../html $<

$(HTMLDIR)/sppm.html : sppm.texi
	$(TEXI2HTML)  -menu -number -split=section -subdir=../html $<

$(HTMLDIR)/standards.html : standards.texi
	$(TEXI2HTML)  -menu -number -split=section -subdir=../html $<

$(HTMLDIR)/term.html : term.texi
	$(TEXI2HTML)  -menu -number -split=section -subdir=../html $<

$(HTMLDIR)/termcap.html : termcap.texi
	$(TEXI2HTML)  -menu -number -split=section -subdir=../html $<

$(HTMLDIR)/texinfo.html : texinfo.texi
	$(TEXI2HTML)  -menu -number -split=section -subdir=../html $<

$(HTMLDIR)/widget.html : widget.texi
	$(TEXI2HTML)  -menu -number -split=section -subdir=../html $<

$(HTMLDIR)/sxemacs-faq.html : sxemacs-faq.texi
	$(TEXI2HTML)  -menu -number -split=section -subdir=../html $<

# Manuals with their own subdirectory
$(HTMLDIR)/sxemacs.html : $(sxemacs-srcs)
	$(TEXI2HTML)  -menu -number -split=section -subdir=../html sxemacs/sxemacs.texi

$(HTMLDIR)/lispref.html : $(lispref-srcs)
	$(TEXI2HTML)  -menu -number -split=section -subdir=../html lispref/lispref.texi

$(HTMLDIR)/internals.html : $(internals-srcs)
	$(TEXI2HTML)  -menu -number -split=section -subdir=../html internals/internals.texi

$(HTMLDIR)/new-users-guide.html : $(new-users-guide-srcs)
	$(TEXI2HTML)  -menu -number -split=section -subdir=../html new-users-guide/new-users-guide.texi


# EMACS = ../src/sxemacs
# EMACSFLAGS = -batch -q -no-site-file

# $(INFODIR)/vm.info : vm.texi
#	-$(EMACS) $(EMACSFLAGS) -insert vm.texi -l texinfmt \
#		-f texinfo-format-buffer -f save-buffer
#	-mv vm.info* $(INFODIR)/.

# special = # $(INFODIR)/vm.info $(INFODIR)/texinfo.info

sxemacs : $(INFODIR)/sxemacs.info
lispref : $(INFODIR)/lispref.info
internals : $(INFODIR)/internals.info
new-users-guide.info : $(INFODIR)/new-users-guide.info

.PHONY : sxemacs lispref internals new-users-guide info dvi

info : $(info_files)

# tm: FRC.tm
#	cd ./tm && $(MAKE) $(RECURSIVE_MAKE_ARGS)
# gnats: FRC.gnats
#	cd ./gnats && $(MAKE) $(RECURSIVE_MAKE_ARGS)
# FRC.xemacs FRC.lispref FRC.new-users-guide FRC.internals FRC.tm FRC.gnats:

.PHONY: info dvi html

.texi.dvi: 
	$(TEXI2DVI) -c -s $< 

sxemacs.dvi : $(sxemacs-srcs)
	$(TEXI2DVI) -I sxemacs -c -s sxemacs/sxemacs.texi

lispref.dvi : $(lispref-srcs)
	$(TEXI2DVI) -I lispref -c -s lispref/lispref.texi

internals.dvi : $(internals-srcs)
	$(TEXI2DVI) -I internals -c -s internals/internals.texi

new-users-guide.dvi : $(new-users-guide-srcs)
	$(TEXI2DVI) -I new-users-guide -c -s new-users-guide/new-users-guide.texi 

dvi :  $(dvi_files)


%.ps: %.dvi
	$(DVI2PS) $< -o $@

ps : $(ps_files)



%.pdf: %.dvi 
	$(DVI2PDF) $< $@


pdf : $(pdf_files)


html: $(html_files)




.PHONY: mostlyclean clean distclean realclean extraclean
mostlyclean:
	$(RM) *.toc *.aux *.log *.op *.cp *.cps *.fn *.fns
	$(RM) *.ky *.kys *.pg *.pgs *.tp *.tps *.vr *.vrs
clean: mostlyclean
	$(RM) core *.dvi *.pdf *.ps
distclean: clean
realclean: distclean
extraclean: distclean
	$(RM) *~ \#* */*~ */\#*
