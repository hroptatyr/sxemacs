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
top_build_prefix = $(top_builddir)/
top_srcdir = @top_srcdir@
abs_builddir = @abs_builddir@
abs_top_builddir = @abs_top_builddir@
abs_top_srcdir = @abs_top_srcdir@

ETAGS = $(top_builddir)/lib-src/etags
TAGS_DEPENDENCIES = $(ETAGS)

sxesrcdir = $(top_srcdir)/src
bldsrcdir = $(top_builddir)/src
sxelispdir = $(abs_top_srcdir)/lisp
bldlispdir = $(abs_top_builddir)/lisp

sitemodule_DATA = 

EXTRA_DIST = autogen.sh
SUBDIRS = 
if DESCEND_ASE
SUBDIRS += ase
endif
if DESCEND_CL
SUBDIRS += cl
endif
## ldap is fucking broken, we dont add it

### handled in master Makefile.am
##if !AUTARKIC
#module_DATA = auto-autoloads.el auto-autoloads.elc
##endif

CLEANFILES = auto-autoloads.el* stage1 stage2

SXEMACS = $(top_builddir)/src/sxemacs
BITCH = $(SXEMACS) -batch
BITCHENV = EMACSPACKAGEPATH= SOURCE_TREE_ROOT=$(abs_top_srcdir) BUILD_TREE_ROOT=$(abs_top_builddir)
BATCH = $(BITCHENV) $(BITCH)
PUREBATCH = $(BITCHENV) $(PITCH)

auto-autoloads.el: $(SXEMACS)
	@touch $@


SUFFIXES = .el .elc
.el.elc: $(SXEMACS)
	$(BATCH) -vanilla -l bytecomp -f batch-byte-compile-one-file-here $<

#stage1: all-am
#	@touch $@
#
#stage2: stage1 auto-autoloads.el
#	@touch $@
#
#all: stage1 stage2
