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

sxesrcdir = $(top_srcdir)/src
bldsrcdir = $(top_builddir)/src
sxelwlibdir = $(top_srcdir)/src/ui/lwlib
bldlwlibdir = $(top_builddir)/src/ui/lwlib

misc_tests = frame.el glyph-test.el gutter-test.el			\
	redisplay-tests.el reproduce-bugs.el
mule_tests = mule/match.el
DLL_tests = DLL/dltest.c
automated_tests = automated/ase-cartesian-tests.el                      \
	automated/ase-digraph-tests.el automated/ase-heap-tests.el      \
	automated/ase-interval-tests.el automated/ase-metric-tests.el   \
	automated/ase-neighbourhood-tests.el                            \
	automated/ase-permutation-tests.el				\
	automated/ase-resclass-tests.el automated/base64-tests.el	\
	automated/bloom-tests.el automated/byte-compiler-tests.el       \
	automated/case-tests.el	automated/ccl-tests.el			\
	automated/cl-loop-tests.el automated/c-tests.el			\
	automated/database-tests.el automated/ent-tests.el		\
	automated/extent-tests.el automated/hash-table-tests.el		\
	automated/format-tests.el					\
	automated/inplace-tests.el automated/lisp-reader-tests.el	\
	automated/lisp-tests.el automated/md5-tests.el			\
	automated/mule-tests.el automated/openssl-tests.el		\
	automated/map-tests.el						\
	automated/os-tests.el automated/regexp-tests.el			\
	automated/region-tests.el automated/skiplist-tests.el		\
	automated/symbol-tests.el automated/syntax-tests.el		\
	automated/tag-tests.el automated/test-harness.el		\
	automated/weak-tests.el automated/README
benchmark_tests = benchmark/benchmark.el

EXTRA_DIST = $(misc_tests) $(mule_tests)		\
	$(DLL_tests) $(automated_tests) $(benchmark_tests)

CLEANFILES = stage1 stage2


SXE_DYLD_PATH = $(bldsrcdir):$(bldsrcdir)/$(LT_OBJDIR):$(bldlwlibdir):$(bldlwlibdir)/$(LT_OBJDIR):$(DESTDIR)$(libdir)
BATCHENV = DYLD_LIBRARY_PATH=$(SXE_DYLD_PATH):$$DYLD_LIBRARY_PATH:$(DYLD_LIBRARY_PATH) \
	LD_LIBRARY_PATH=$(SXE_DYLD_PATH):$$LD_LIBRARY_PATH:$(LD_LIBRARY_PATH) \
	SHLIB_PATH=$(SXE_DYLD_PATH):$$SHLIB_PATH:$(SHLIB_PATH) \
	MALLOC_PERTURB_=69
### Produces strange banner
SXEMACS = $(bldsrcdir)/sxemacs
SXEDMP = $(SXEMACS).dmp
testdir = $(srcdir)
testautomdir = $(testdir)/automated
tests_environment = $(BATCHENV) \
	$(SXEMACS) -batch -l ${testautomdir}/test-harness.el \
		-f batch-test-emacs $(testautomdir)

## Use our former check target
check-am: $(SXEMACS) $(SXEDMP)
	$(tests_environment)

check-formats: $(SXEMACS) $(SXEDMP)
	$(BATCHENV) $(SXEMACS) -l ${testautomdir}/test-harness.el \
	-f batch-test-emacs $(testautomdir)/format-tests.el

check-map: $(SXEMACS) $(SXEDMP)
	$(BATCHENV) $(SXEMACS) -l ${testautomdir}/test-harness.el \
	-f batch-test-emacs $(testautomdir)/map-tests.el

check-skiplists: $(SXEMACS) $(SXEDMP)
	$(BATCHENV) $(SXEMACS) -l ${testautomdir}/test-harness.el \
	-f batch-test-emacs $(testautomdir)/skiplist-tests.el

check-%: $(SXEMACS) $(SXEDMP)
	$(BATCHENV) $(SXEMACS) -l ${testautomdir}/test-harness.el \
	-f batch-test-emacs $(testautomdir)/$*-tests.el


.PHONY: stage1 stage2
stage1:
	@echo
	@echo "$@ in progress ..."
	@$(RECURSE)
	@echo "$@ finished"

stage2: stage1
	@echo
	@echo "$@ in progress ..."
	@$(RECURSE)
	@echo "$@ finished"

### dont use staged build
##all: stage1 stage2
