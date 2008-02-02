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

misc_tests = frame.el glyph-test.el gutter-test.el			\
	redisplay-tests.el reproduce-bugs.el
gtk_tests = gtk/event-stream-tests.el gtk/gnome-test.el			\
	gtk/gtk-embedded-test.el gtk/gtk-extra-test.el gtk/gtk-test.el	\
	gtk/gtk-test.glade gtk/statusbar-test.el gtk/toolbar-test.el	\
	gtk/xemacs-toolbar.el gtk/UNIMPLEMENTED
mule_tests = mule/match.el
tooltalk_tests = tooltalk/beeps.el tooltalk/emacs-eval.c		\
	tooltalk/load-file.c tooltalk/make-client-frame.c		\
	tooltalk/Makefile tooltalk/simple.el
DLL_tests = DLL/dltest.c
automated_tests = automated/base64-tests.el automated/bloom-tests.el	\
	automated/byte-compiler-tests.el automated/case-tests.el	\
	automated/ccl-tests.el automated/c-tests.el			\
	automated/database-tests.el automated/ent-tests.el		\
	automated/extent-tests.el automated/hash-table-tests.el		\
	automated/inplace-tests.el automated/lisp-reader-tests.el	\
	automated/lisp-tests.el automated/md5-tests.el			\
	automated/mule-tests.el automated/openssl-tests.el		\
	automated/os-tests.el automated/regexp-tests.el			\
	automated/region-tests.el automated/skiplist-tests.el		\
	automated/symbol-tests.el automated/syntax-tests.el		\
	automated/tag-tests.el automated/test-harness.el		\
	automated/weak-tests.el automated/README

EXTRA_DIST = $(misc_tests) $(gtk_tests) $(mule_tests)		\
	$(tooltalk_tests) $(DLL_tests) $(automated_tests)

CLEANFILES = stage1 stage2

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

