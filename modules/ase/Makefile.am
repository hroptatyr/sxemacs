## This file is part of SXEmacs.

## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions
## are met:
##
## 1. Redistributions of source code must retain the above copyright
##    notice, this list of conditions and the following disclaimer.
##
## 2. Redistributions in binary form must reproduce the above copyright
##    notice, this list of conditions and the following disclaimer in the
##    documentation and/or other materials provided with the distribution.
##
## 3. Neither the name of the author nor the names of any contributors
##    may be used to endorse or promote products derived from this
##    software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
## IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
## WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
## FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
## CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
## SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
## BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
## WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
## OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
## IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

## Process this file with automake to produce Makefile.in

# Help the Developers and yourself. Just use the C locale and settings
# for the compilation. They can still be overriden by make LANG=<whatever>
# but that is general a not very good idea
LANG=C
LC_ALL=C

MODVER=0.0.0

builddir = @builddir@
srcdir = @srcdir@
top_builddir = @top_builddir@
top_build_prefix = $(top_builddir)/
top_srcdir = @top_srcdir@
abs_builddir = @abs_builddir@
abs_top_builddir = @abs_top_builddir@
abs_top_srcdir = @abs_top_srcdir@
headerdir = $(includedir)/$(instvarsepdir)

AM_CFLAGS = $(dll_cflags) -DIMA_MODULE -DUSE_SXEMACS_CONFIG_H
AM_CPPFLAGS = -I$(srcdir) -I$(builddir)  -I$(top_srcdir)/src \
	-I$(top_builddir)/src $(SXEMACS_INCLUDES) $(c_switch_all)
AM_LDFLAGS = -module $(XCCLDFLAGS)

## all the mods
module_LTLIBRARIES =

BUILT_SOURCES =

header_HEADERS = ase.h ase-cartesian.h ase-interval.h ase-metric.h \
	ase-neighbourhood.h ase-permutation.h ase-resclass.h \
	ase-digraph.h ase-heap.h

module_LTLIBRARIES += ase-mono.la
ase_mono_la_SOURCES = ase.c ase-cartesian.c ase-interval.c ase-metric.c \
	ase-neighbourhood.c ase-permutation.c ase-resclass.c ase-digraph.c \
	ase-heap.c
nodist_ase_mono_la_SOURCES = ase.doc.c ase-cartesian.doc.c		\
	ase-interval.doc.c ase-metric.doc.c ase-neighbourhood.doc.c	\
	ase-permutation.doc.c ase-resclass.doc.c ase-digraph.doc.c	\
	ase-heap.doc.c
ase_mono_la_CFLAGS = $(AM_CFLAGS) -DEMOD_ASE_MONOMOD

module_LTLIBRARIES += ase.la
ase_la_SOURCES = ase.c
nodist_ase_la_SOURCES = ase.doc.c
BUILT_SOURCES += ase.doc.c

module_LTLIBRARIES += ase-set.la
ase_set_la_SOURCES = ase-set.c ase-set.h
ase_set_la_SOURCES += ase-cartesian.c ase-cartesian.h
ase_set_la_SOURCES += ase-interval.c ase-interval.h
ase_set_la_SOURCES += ase-metric.c ase-metric.h
ase_set_la_SOURCES += ase-neighbourhood.c ase-neighbourhood.h
nodist_ase_set_la_SOURCES = ase-set.doc.c				\
	ase-cartesian.doc.c ase-interval.doc.c ase-metric.doc.c		\
	ase-neighbourhood.doc.c
BUILT_SOURCES += ase-set.doc.c ase-cartesian.doc.c ase-interval.doc.c	\
	ase-neighbourhood.doc.c ase-metric.doc.c

module_LTLIBRARIES += ase-permutation.la
ase_permutation_la_SOURCES = ase-permutation.c
nodist_ase_permutation_la_SOURCES = ase-permutation.doc.c
ase_permutation_la_DEPENDENCIES = ase.la
BUILT_SOURCES += ase-permutation.doc.c

module_LTLIBRARIES += ase-resclass.la
ase_resclass_la_SOURCES = ase-resclass.c
nodist_ase_resclass_la_SOURCES = ase-resclass.doc.c
ase_resclass_la_DEPENDENCIES = ase.la
BUILT_SOURCES += ase-resclass.doc.c

module_LTLIBRARIES += ase-digraph.la
ase_digraph_la_SOURCES = ase-digraph.c
nodist_ase_digraph_la_SOURCES = ase-digraph.doc.c
ase_digraph_la_DEPENDENCIES = ase.la
BUILT_SOURCES += ase-digraph.doc.c

module_LTLIBRARIES += ase-heap.la
ase_heap_la_SOURCES = ase-heap.c
nodist_ase_heap_la_SOURCES = ase-heap.doc.c
ase_heap_la_DEPENDENCIES = ase.la
BUILT_SOURCES += ase-heap.doc.c

CLEANFILES = $(BUILT_SOURCES) auto-autoloads.el{,c} libase.a

## custom rules
SXEMACS = $(top_builddir)/src/sxemacs
make_docfile = $(MAKE_DOCFILE)
BITCH = $(SXEMACS) -batch
BITCHENV = EMACSPACKAGEPATH= SOURCE_TREE_ROOT=$(abs_top_srcdir) BUILD_TREE_ROOT=$(abs_top_builddir)
BATCH = $(BITCHENV) $(BITCH)
RM = rm -f

#auto-autoloads.el: $(SXEMACS)
#	$(BATCH) -no-autoloads \
#		-l autoload -f batch-create-autoloads \
#		--relative-to $(srcdir) \
#		--autoload-dir-name . \
#		$(ase_la_SOURCES)
#
#auto-autoloads.elc: auto-autoloads.el
#	$(BATCH) -f batch-byte-compile auto-autoloads.el

## The benchmark environment
testbmdir = $(top_srcdir)/tests/benchmark
bm_environment = $(BATCH) -batch -l ${testbmdir}/benchmark.el

_ase-heap_benchmarks: ase-heap.la
	$(bm_environment) -l $(srcdir)/ase-heap-profs.el

benchmarks: _ase-heap_benchmarks

#
# Help the SXEmacs developers get nice post-processed source files

## the libtool approach
SUFFIXES = .doc.c
.c.doc.c: $(MAKE_DOCFILE)
	$(make_docfile) --modname $* -E $@ $<

## Create preprocessor output (debugging purposes only)
.c.i:
	$(COMPILE) -E -o $@ $<

## Create assembler output (debugging purposes only)
.c.s:
	$(COMPILE) -S -c $(cflags) $<
