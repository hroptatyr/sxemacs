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

MODNAME=sample
MODVER=0.0.0
MODTITLE="SXEmacs module example"

builddir = @builddir@
srcdir = @srcdir@
top_builddir = @top_builddir@
top_srcdir = @top_srcdir@
abs_top_builddir = @abs_top_builddir@
abs_top_srcdir = @abs_top_srcdir@

ETAGS = $(top_builddir)/lib-src/etags
TAGS_DEPENDENCIES = $(ETAGS)

AM_CPPFLAGS = -I$(srcdir) -I$(builddir)  -I$(top_srcdir)/src \
	-I$(top_builddir)/src $(SXEMACS_INCLUDES) $(c_switch_all)

MKINIT = $(ELLCC) --mode=init --mode=verbose
MKINITENV = ELLMAKEDOC=$(ELLMAKEDOC)
ELLCCLD = $(ELLCC) --mode=link --mode=verbose $(ld_switch_general)

if STATIC_SAMPLE
noinst_LIBRARIES = libsample.a
libsample_a_CFLAGS = -ggdb -Wall
libsample_a_SOURCES = sample.c
nodist_libsample_a_SOURCES = sample_i.c
endif

module_PROGRAMS = sample.ell
if AUTARKIC
module_DATA = auto-autoloads.elc
endif
sample_ell_SOURCES = sample.c
nodist_sample_ell_SOURCES = sample_i.c
BUILT_SOURCES = sample_i.c
sample_ell_CFLAGS = -ggdb -Wall $(dll_cflags) -DIMA_MODULE
sample_ell_LINK = $(ELLCCLD) --mod-output=$@

CLEANFILES = sample_i.c auto-autoloads.el{,c} libsample.a

$(MODNAME)_i.c: $(sample_ell_SOURCES)
	$(MKINITENV) $(MKINIT) --mod-output=$@ \
	--mod-name=$(MODNAME) --mod-version=$(MODVER) \
	--mod-title=$(MODTITLE) $?

auto-autoloads.el:
	$(sxebatch) -no-autoloads \
		-l autoload -f batch-create-autoloads \
		--relative-to $(srcdir) \
		--autoload-dir-name . \
		$(base64_ell_SOURCES)

auto-autoloads.elc: auto-autoloads.el
	$(sxebatch) -f batch-byte-compile auto-autoloads.el

# 
# Help the SXEmacs developers get nice post-processed source files

## Create preprocessor output (debugging purposes only)
.c.i:
	$(COMPILE) -E -o $@ $<

## Create assembler output (debugging purposes only)
.c.s:
	$(COMPILE) -S -c $(cflags) $<
