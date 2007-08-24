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

MODVER=0.0.0

# Help the Developers and yourself. Just use the C locale and settings
# for the compilation. They can still be overriden by make LANG=<whatever>
# but that is general a not very good idea
LANG=C
LC_ALL=C

builddir = @builddir@
srcdir = @srcdir@
top_builddir = @top_builddir@
top_srcdir = @top_srcdir@
abs_builddir = @abs_builddir@
abs_top_builddir = @abs_top_builddir@
abs_top_srcdir = @abs_top_srcdir@
headerdir = $(includedir)/$(instvarsepdir)

AM_CFLAGS = -g -ggdb -O3 -Wall -Winline $(dll_cflags)
AM_CPPFLAGS = -I$(srcdir) -I$(builddir)  -I$(top_srcdir)/src \
	-I$(top_builddir)/src $(SXEMACS_INCLUDES) $(c_switch_all)
AM_YFLAGS = -d

MKINIT = $(ELLCC) --mode=init --mode=verbose
MKINITENV = ELLMAKEDOC=$(ELLMAKEDOC)
ELLCCLD = $(ELLCC) --mode=link --mode=verbose $(ld_switch_general)

if STATIC_CL
noinst_LIBRARIES = libcl.a
libcl_a_CFLAGS = $(AM_CFLAGS) -DEMOD_CL_MONOMOD
libcl_a_SOURCES = $(cl_ell_SOURCES)
nodist_libcl_a_SOURCES = $(nodist_cl_ell_SOURCES)
endif

module_PROGRAMS =
init_files =
BUILT_SOURCES =
if AUTARKIC
module_DATA = auto-autoloads.elc
endif
header_HEADERS = cl.h cl-loop.h

## Now the subemods
module_PROGRAMS += cl.ell
init_files += cl_i.c
BUILT_SOURCES += $(init_files) cl-loop-parser.h cl-loop-parser.c
cl_ell_SOURCES = cl.c cl-loop.c cl-loop-parser.y
nodist_cl_ell_SOURCES = cl_i.c
cl_ell_CFLAGS = $(AM_CFLAGS) $(dll_cflags) -DIMA_MODULE -DEMOD_CL_MONOMOD
cl_ADD_LIBS = $(cl_loop_ADD_LIBS)
cl_ell_LINK = $(ELLCCLD) $(cl_ADD_LIBS) --mod-output=$@
cl_i.c: MODNAME=cl
cl_i.c: MODTITLE="SXEmacs module: cl"
cl_i.c: $(cl_ell_SOURCES)

module_PROGRAMS += cl-loop.ell
init_files += cl-loop_i.c
BUILT_SOURCES += $(init_files) cl-loop-parser.h cl-loop-parser.c
header_HEADERS += cl-loop.h
cl_loop_ell_SOURCES = cl-loop.c cl-loop-parser.y
nodist_cl_loop_ell_SOURCES = cl-loop_i.c
cl_loop_ell_CFLAGS = $(AM_CFLAGS) -DIMA_MODULE
cl_loop_ADD_LIBS =
cl_loop_ell_LDADD = $(cl_loop_ADD_LIBS) $(cl_loop_ADD_LIBS)
cl_loop_ell_LINK = $(ELLCCLD) --mod-output=$@
cl_loop_ell_DEPENDENCIES =
cl-loop_i.c: MODNAME=cl_loop
cl-loop_i.c: MODTITLE="SXEmacs module: cl-loop"
cl-loop_i.c: $(cl_loop_ell_SOURCES)

#cl-loop.tab.c: cl-loop.y
#	$(BISON) -o$@ $<

BUILT_SOURCES += $(init_files)
CLEANFILES = $(BUILT_SOURCES) auto-autoloads.el{,c} core* libcl.a

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

$(init_files): $(cl_loop_ell_SOURCES) $(cl_ell_SOURCES)
	$(MKINITENV) $(MKINIT) --mod-output=$@ \
	--mod-name=$(MODNAME) --mod-version=$(MODVER) \
	--mod-title=$(MODTITLE) $^

auto-autoloads.el: $(SXEMACS)
	$(BATCH) -no-autoloads \
		-l autoload -f batch-create-autoloads \
		--relative-to $(srcdir) \
		--autoload-dir-name . \
		$(cl_ell_SOURCES)

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

_cl-loop_benchmarks: cl-loop.ell
	$(bm_environment) -l $(srcdir)/cl-loop-profs.el

benchmarks: _cl-loop_benchmarks

# 
# Help the SXEmacs developers get nice post-processed source files

## Create preprocessor output (debugging purposes only)
.c.i:
	$(COMPILE) -E -o $@ $<

## Create assembler output (debugging purposes only)
.c.s:
	$(COMPILE) -S -c $(cflags) $<
