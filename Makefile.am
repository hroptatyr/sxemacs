## SXEmacs toplevel Makefile.am

## Copyright (C) 2007 Steve Youngs

## This file is part of SXEmacs.

## SXEmacs is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## SXEmacs is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

## Process this file with automake to produce Makefile.in

# Help the Developers and yourself. Just use the C locale and settings
# for the compilation. They can still be overriden by make LANG=<whatever>
# but that is general a not very good idea
LANG=C
LC_ALL=C

ACLOCAL_AMFLAGS = -I m4

builddir = @builddir@
srcdir = @srcdir@
top_builddir = .
top_build_prefix = $(top_builddir)/
top_srcdir = @top_srcdir@
abs_top_builddir = @abs_top_builddir@
abs_top_srcdir = @abs_top_srcdir@

archlibdir = ${libdir}/${instvardir}/${configuration}
etcdir = ${datadir}/${instvardir}/etc

ETAGS = $(top_builddir)/lib-src/etags
TAGS_DEPENDENCIES = $(ETAGS)

EXTRA_DIST = autogen.sh sxemacs_version.m4
EXTRA_DIST += configfsf.guess configfsf.sub

REPORT_VARS=PATH LD_LIBRARY_PATH LIBRARY_PATH SHLIB_PATH DYLD_LIBRARY_PATH CC CPATH CPPFLAGS CFLAGS LDFLAGS INCLUDE_PATH C_INCLUDE_PATH SHELL GCC_EXEC_PREFIX COMPILER_PATH DEPENDENCIES_OUTPUT GNUTARGET LDEMULATION

## we do not use automake's SUBDIRS thingie
sxe_subdirs =
if DESCEND_LIBLTDL
sxe_subdirs += libltdl
endif
sxe_subdirs += lib-src
if DESCEND_MODULES
sxe_subdirs += modules
endif
sxe_subdirs += src lisp src lisp info etc tests

SUBDIRS =
SUBDIRS += $(sxe_subdirs)

m4datadir = $(datadir)/aclocal
dist_m4data_DATA = m4/sxemacs.m4 m4/sxe-pkgconfig.m4
dist_etc_DATA = PROBLEMS

pcdir = ${libdir}/pkgconfig
pc_DATA = sxemacs.pc

CLEANFILES = stage1 stage2 ,,build.done

## custom rules
MV = mv
TOUCH = touch

RECURSE =								\
	@for subdir in $(sxe_subdirs); do				\
		test "$$subdir" = . ||					\
			(cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) $@);	\
	done ;                                                          \
	@$(TOUCH) $@

CROSSMAKE = (cd `dirname $@` && $(MAKE) $(AM_MAKEFLAGS) `basename $@`)

if DESCEND_MODULES
module_DATA = modules/auto-autoloads.el modules/auto-autoloads.elc

modules/auto-autoloads.el \
modules/auto-autoloads.elc: $(EMACS)
	$(CROSSMAKE)
endif

SXE_SHOW_DUMP = $(DESTDIR)$(bindir)/sxemacs -sd)

install-exec-hook:
	mv -f $(DESTDIR)$(bindir)/sxemacs $(DESTDIR)$(bindir)/sxemacs-$(old_version)
	(cd $(DESTDIR)$(bindir) && $(LN_S) sxemacs-$(old_version) sxemacs)

uninstall-hook:
	cd $(DESTDIR)$(bindir) && \
	$(RM) sxemacs-$(old_version)

check-recursive: all-recursive

distclean-local:
	$(RM) $(builddir)/Installation
	$(RM) $(builddir)/cpuid

$(ETAGS):
	cd $(top_builddir)/lib-src
	$(MAKE) $(AM_MAKEFLAGS) etags

.PHONY: stage1 stage2 build-report time-build-report time-check beta
## Convenience target for SXEmacs beta testers
beta:
	$(MAKE) $(AM_MAKEFLAGS) clean
	$(MAKE) $(AM_MAKEFLAGS) all
	@ touch ,,build.done

## Convenience target for SXEmacs build reporters
env-report:
	@(echo "Environment Variables" ;\
	  echo "---------------------"  ) | tee ,,vars.out
	@(echo "${REPORT_VARS}" | \
		${AWK} '{ for(f=1;f<=NF;f++) if( ENVIRON[$$f] ) \
			{ print $$f "=" ENVIRON[$$f] } }') \
		| tee -a ,,vars.out
	@(echo "Make Variables" ; \
	echo "--------------" ; \
	echo MAKE="${MAKE}" ; \
	echo MAKEFILES="${MAKEFILES}" ; \
	echo MAKEFLAGS="${MAKEFLAGS}" ; \
	echo AM_MAKEFLAGS="${AM_MAKEFLAGS}" ;  \
	echo RECURSIVE_MAKE_ARGS="${RECURSIVE_MAKE_ARGS}"  ) | tee -a ,,vars.out

time-beta:
	(time $(MAKE) $(AM_MAKEFLAGS) beta )2>&1 | tee ,,beta.out

time-all:
	time $(MAKE) $(AM_MAKEFLAGS) all

time-check:
	(time $(MAKE) $(AM_MAKEFLAGS) check ) 2>&1 | tee ,,make-check.out

build-report: env-report
	$(MAKE) $(AM_MAKEFLAGS) beta 2>&1 | tee ,,beta.out
	@ [ -f ,,build.done ] || exit 1
	$(MAKE) -w $(MAKEFLAGS) check 2>&1 | tee ,,make-check.out

time-build-report: env-report
	$(MAKE) $(AM_MAKEFLAGS) time-beta
	@ [ -f ,,build.done ] || exit 1
	$(MAKE) $(AM_MAKEFLAGS) time-check 

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

