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
top_srcdir = @top_srcdir@
abs_builddir = @abs_builddir@
abs_top_builddir = @abs_top_builddir@
abs_top_srcdir = @abs_top_srcdir@
headerdir = $(includedir)/$(instvarsepdir)

AM_CFLAGS = 
AM_CPPFLAGS = -I$(srcdir) -I$(builddir)  -I$(top_srcdir)/src \
	-I$(top_builddir)/src $(SXEMACS_INCLUDES) $(c_switch_all)

MKINIT = $(ELLCC) --mode=init --mode=verbose
MKINITENV = ELLMAKEDOC=$(ELLMAKEDOC)
ELLCCLD = $(ELLCC) --mode=link --mode=verbose $(ld_switch_general)

if STATIC_ASE
noinst_LIBRARIES = libase.a
libase_a_CFLAGS = $(AM_CFLAGS) -DEMOD_ASE_MONOMOD
libase_a_SOURCES = $(ase_mono_ell_SOURCES)
nodist_libase_a_SOURCES = $(nodist_ase_mono_ell_SOURCES)
endif

module_PROGRAMS = ase-mono.ell \
	ase.ell ase-cartesian.ell ase-interval.ell ase-metric.ell \
	ase-neighbourhood.ell ase-permutation.ell ase-resclass.ell \
	ase-digraph.ell ase-heap.ell
BUILT_SOURCES = ase-mono_i.c \
	ase_i.c ase-cartesian_i.c ase-interval_i.c ase-metric_i.c \
	ase-neighbourhood_i.c ase-permutation_i.c ase-resclass_i.c \
	ase-digraph_i.c ase-heap_i.c
if AUTARKIC
module_DATA = auto-autoloads.elc
endif
header_HEADERS = ase.h ase-cartesian.h ase-interval.h ase-metric.h \
	ase-neighbourhood.h ase-permutation.h ase-resclass.h \
	ase-digraph.h ase-heap.h

ase_mono_ell_SOURCES = ase.c ase-cartesian.c ase-interval.c ase-metric.c \
	ase-neighbourhood.c ase-permutation.c ase-resclass.c ase-digraph.c \
	ase-heap.c
nodist_ase_mono_ell_SOURCES = ase-mono_i.c
ase_mono_ell_CFLAGS = $(AM_CFLAGS) $(dll_cflags) -DIMA_MODULE -DEMOD_ASE_MONOMOD
ase_mono_ell_LINK = $(ELLCCLD) --mod-output=$@
ase-mono_i.c: MODNAME=ase_mono
ase-mono_i.c: MODTITLE="SXEmacs module: ase"
ase-mono_i.c: $(ase_mono_ell_SOURCES)

ase_ell_SOURCES = ase.c
nodist_ase_ell_SOURCES = ase_i.c
ase_ell_CFLAGS = $(AM_CFLAGS) $(dll_cflags) -DIMA_MODULE
ase_ell_LINK = $(ELLCCLD) --mod-output=$@
ase_i.c: MODNAME=ase
ase_i.c: MODTITLE="SXEmacs module: ase"
ase_i.c: $(ase_ell_SOURCES)

ase_neighbourhood_ell_SOURCES = ase-neighbourhood.c
nodist_ase_neighbourhood_ell_SOURCES = ase-neighbourhood_i.c
ase_neighbourhood_ell_CFLAGS = $(AM_CFLAGS) $(dll_cflags) -DIMA_MODULE
ase_neighbourhood_ell_LINK = \
	$(ELLCCLD) ase.ell ase-interval.ell ase-cartesian.ell ase-metric.ell \
	--mod-output=$@
ase_neighbourhood_ell_DEPENDENCIES = \
	ase.ell ase-interval.ell ase-cartesian.ell ase-metric.ell
ase-neighbourhood_i.c: MODNAME=ase_neighbourhood
ase-neighbourhood_i.c: MODTITLE="SXEmacs module: ase-neighbourhood"
ase-neighbourhood_i.c: $(ase_neighbourhood_ell_SOURCES)

ase_cartesian_ell_SOURCES = ase-cartesian.c
nodist_ase_cartesian_ell_SOURCES = ase-cartesian_i.c
ase_cartesian_ell_CFLAGS = $(AM_CFLAGS) $(dll_cflags) -DIMA_MODULE
ase_cartesian_ell_LINK = \
	$(ELLCCLD) ase.ell --mod-output=$@
ase_cartesian_ell_DEPENDENCIES = ase.ell
ase-cartesian_i.c: MODNAME=ase_cartesian
ase-cartesian_i.c: MODTITLE="SXEmacs module: ase-cartesian"
ase-cartesian_i.c: $(ase_cartesian_ell_SOURCES)

ase_interval_ell_SOURCES = ase-interval.c
nodist_ase_interval_ell_SOURCES = ase-interval_i.c
ase_interval_ell_CFLAGS = $(AM_CFLAGS) $(dll_cflags) -DIMA_MODULE
ase_interval_ell_LINK = \
	$(ELLCCLD) ase.ell ase-cartesian.ell --mod-output=$@
ase_interval_ell_DEPENDENCIES = ase.ell ase-cartesian.ell
ase-interval_i.c: MODNAME=ase_interval
ase-interval_i.c: MODTITLE="SXEmacs module: ase-interval"
ase-interval_i.c: $(ase_interval_ell_SOURCES)

ase_metric_ell_SOURCES = ase-metric.c
nodist_ase_metric_ell_SOURCES = ase-metric_i.c
ase_metric_ell_CFLAGS = $(AM_CFLAGS) $(dll_cflags) -DIMA_MODULE
ase_metric_ell_LINK = \
	$(ELLCCLD) ase.ell ase-cartesian.ell --mod-output=$@
ase_metric_ell_DEPENDENCIES = ase.ell ase-cartesian.ell
ase-metric_i.c: MODNAME=ase_metric
ase-metric_i.c: MODTITLE="SXEmacs module: ase-metric"
ase-metric_i.c: $(ase_metric_ell_SOURCES)

ase_permutation_ell_SOURCES = ase-permutation.c
nodist_ase_permutation_ell_SOURCES = ase-permutation_i.c
ase_permutation_ell_CFLAGS = $(AM_CFLAGS) $(dll_cflags) -DIMA_MODULE
ase_permutation_ell_LINK = \
	$(ELLCCLD) ase.ell --mod-output=$@
ase_permutation_ell_DEPENDENCIES = ase.ell
ase-permutation_i.c: MODNAME=ase_permutation
ase-permutation_i.c: MODTITLE="SXEmacs module: ase-permutation"
ase-permutation_i.c: $(ase_permutation_ell_SOURCES)

ase_resclass_ell_SOURCES = ase-resclass.c
nodist_ase_resclass_ell_SOURCES = ase-resclass_i.c
ase_resclass_ell_CFLAGS = $(AM_CFLAGS) $(dll_cflags) -DIMA_MODULE
ase_resclass_ell_LINK = \
	$(ELLCCLD) ase.ell --mod-output=$@
ase_resclass_ell_DEPENDENCIES = ase.ell
ase-resclass_i.c: MODNAME=ase_resclass
ase-resclass_i.c: MODTITLE="SXEmacs module: ase-resclass"
ase-resclass_i.c: $(ase_resclass_ell_SOURCES)

ase_digraph_ell_SOURCES = ase-digraph.c
nodist_ase_digraph_ell_SOURCES = ase-digraph_i.c
ase_digraph_ell_CFLAGS = $(AM_CFLAGS) $(dll_cflags) -DIMA_MODULE
ase_digraph_ell_LINK = $(ELLCCLD) ase.ell --mod-output=$@
ase_digraph_ell_DEPENDENCIES = ase.ell
ase-digraph_i.c: MODNAME=ase_digraph
ase-digraph_i.c: MODTITLE="SXEmacs module: ase-digraph"
ase-digraph_i.c: $(ase_digraph_ell_SOURCES)

ase_heap_ell_SOURCES = ase-heap.c
nodist_ase_heap_ell_SOURCES = ase-heap_i.c
ase_heap_ell_CFLAGS = $(AM_CFLAGS) $(dll_cflags) -DIMA_MODULE
ase_heap_ell_LINK = $(ELLCCLD) ase.ell --mod-output=$@
ase_heap_ell_DEPENDENCIES = ase.ell
ase-heap_i.c: MODNAME=ase_heap
ase-heap_i.c: MODTITLE="SXEmacs module: ase-heap"
ase-heap_i.c: $(ase_heap_ell_SOURCES)

CLEANFILES = $(BUILT_SOURCES) auto-autoloads.el{,c} libase.a

## custom rules
if WITH_PDUMP
SXEMACS = $(top_builddir)/src/sxemacs
else
SXEMACS = $(top_builddir)/src/sxemacs.dmp
endif
PUREMACS = $(top_builddir)/src/temacs
BITCH = $(SXEMACS) -batch
PITCH = $(PUREMACS) -batch
BITCHENV = EMACSPACKAGEPATH= SOURCE_TREE_ROOT=$(abs_top_srcdir) BUILD_TREE_ROOT=$(abs_top_builddir)
BATCH = $(BITCHENV) $(BITCH)
PUREBATCH = $(BITCHENV) $(PITCH)
RM = rm -f

$(BUILT_SOURCES): $(INFILES)
	$(MKINITENV) $(MKINIT) --mod-output=$@ \
	--mod-name=$(MODNAME) --mod-version=$(MODVER) \
	--mod-title=$(MODTITLE) $^

auto-autoloads.el: $(SXEMACS)
	$(BATCH) -no-autoloads \
		-l autoload -f batch-create-autoloads \
		--relative-to $(srcdir) \
		--autoload-dir-name . \
		$(ase_ell_SOURCES)

auto-autoloads.elc: auto-autoloads.el
	$(BATCH) -f batch-byte-compile auto-autoloads.el

## The test and benchmark environments
testdir = $(top_srcdir)/tests
testautomdir = $(testdir)/automated
testbmdir = $(testdir)/benchmark
tests_environment = $(SXEMACS) -batch -l ${testautomdir}/test-harness.el
bm_environment = $(SXEMACS) -batch -l ${testbmdir}/benchmark.el

check-am:
	$(tests_environment) -f batch-test-emacs $(srcdir)/*-tests.el

_ase-heap_benchmarks: ase-heap.ell
	$(bm_environment) -l $(srcdir)/ase-heap-profs.el

benchmarks: _ase-heap_benchmarks

# 
# Help the SXEmacs developers get nice post-processed source files

## Create preprocessor output (debugging purposes only)
.c.i:
	$(COMPILE) -E -o $@ $<

## Create assembler output (debugging purposes only)
.c.s:
	$(COMPILE) -S -c $(cflags) $<
