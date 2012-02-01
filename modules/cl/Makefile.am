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
top_build_prefix = $(top_builddir)/
top_srcdir = @top_srcdir@
abs_builddir = @abs_builddir@
abs_top_builddir = @abs_top_builddir@
abs_top_srcdir = @abs_top_srcdir@
headerdir = $(includedir)/$(instvarsepdir)

AM_CFLAGS = $(dll_cflags) -DIMA_MODULE
AM_CPPFLAGS = -I$(srcdir) -I$(builddir)  -I$(top_srcdir)/src \
	-I$(top_builddir)/src $(SXEMACS_INCLUDES) $(c_switch_all)
AM_LDFLAGS = -module $(XCCLDFLAGS)
AM_YFLAGS = -d

## all the mods
module_LTLIBRARIES =
BUILT_SOURCES = cl.doc.c cl-loop.doc.c
header_HEADERS = cl.h cl-loop.h

## Now the subemods
module_LTLIBRARIES += cl.la
cl_la_SOURCES = cl.c cl-loop.c cl-loop-parser.y
nodist_cl_la_SOURCES = cl.doc.c
cl_ell_CFLAGS = $(AM_CFLAGS) -DEMOD_CL_MONOMOD

module_LTLIBRARIES += cl-loop.la
BUILT_SOURCES += cl-loop-parser.h cl-loop-parser.c
cl_loop_la_SOURCES = cl-loop.c cl-loop-parser.y
nodist_cl_loop_la_SOURCES = cl-loop.doc.c

#cl-loop.tab.c: cl-loop.y
#	$(BISON) -o$@ $<

CLEANFILES = $(BUILT_SOURCES) auto-autoloads.el{,c} core* libcl.a

## custom rules
SXEMACS = $(top_builddir)/src/sxemacs
make_docfile = $(MAKE_DOCFILE)
BITCH = $(SXEMACS) -batch
BITCHENV = EMACSPACKAGEPATH= SOURCE_TREE_ROOT=$(abs_top_srcdir) BUILD_TREE_ROOT=$(abs_top_builddir)
BATCH = $(BITCHENV) $(BITCH)
RM = rm -f

## The benchmark environment
testbmdir = $(top_srcdir)/tests/benchmark
bm_environment = $(BATCH) -l ${testbmdir}/benchmark.el

_cl-loop_benchmarks: cl-loop.la
	$(bm_environment) -l $(srcdir)/cl-loop-profs.el

benchmarks: _cl-loop_benchmarks

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
