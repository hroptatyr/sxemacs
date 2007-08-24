## This file is part of SXEmacs.

## SXEmacs is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.

## SXEmacs is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.

## You should have received a copy of the GNU General Public License
## along with SXEmacs; see the file COPYING.  If not, write to
## the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
## Boston, MA 02111-1307, USA.

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
top_srcdir = @top_srcdir@
abs_top_builddir = @abs_top_builddir@
abs_top_srcdir = @abs_top_srcdir@

archlibdir = ${libdir}/${instvardir}/${configuration}
etcdir = ${datadir}/${instvardir}/etc

ETAGS = $(top_builddir)/lib-src/etags

EXTRA_DIST = autogen.sh sxemacs_version.m4

REPORT_VARS=PATH LD_LIBRARY_PATH LIBRARY_PATH CC CPATH CPPFLAGS CFLAGS LDFLAGS INCLUDE_PATH C_INCLUDE_PATH SHELL GCC_EXEC_PREFIX COMPILER_PATH DEPENDENCIES_OUTPUT GNUTARGET LDEMULATION

## we do not use automake's SUBDIRS thingie
sxe_subdirs =
sxe_subdirs += lib-src
if DESCEND_DYNODUMP
sxe_subdirs += dynodump
endif
if DESCEND_LWLIB
sxe_subdirs += lwlib
endif
if DESCEND_MODULES
sxe_subdirs += modules
endif
sxe_subdirs += src lisp info etc tests

SUBDIRS = $(sxe_subdirs)

m4datadir = $(datadir)/aclocal
dist_m4data_DATA = m4/sxemacs.m4
dist_etc_DATA = PROBLEMS

pcdir = ${libdir}/pkgconfig
pc_DATA = sxemacs.pc

CLEANFILES = stage1 stage2

## custom rules
MV = mv
TOUCH = touch

RECURSE =								\
	@for subdir in $(sxe_subdirs); do				\
		test "$$subdir" = . ||					\
			(cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) $@);	\
        done
	@$(TOUCH) $@

CROSSMAKE = (cd `dirname $@` && $(MAKE) $(AM_MAKEFLAGS) `basename $@`)

if DESCEND_MODULES
module_DATA = modules/auto-autoloads.el modules/auto-autoloads.elc

modules/auto-autoloads.el \
modules/auto-autoloads.elc: $(EMACS)
	$(CROSSMAKE)
endif

install-exec-hook:
	cd $(DESTDIR)$(bindir) && \
	mv -f sxemacs sxemacs-$(old_version) && \
	$(LN_S) sxemacs-$(old_version) sxemacs
if WITH_PDUMP
	cd $(DESTDIR)$(bindir) && \
	chmod a-x sxemacs.dmp && \
	mv -f sxemacs.dmp sxemacs-$(old_version)-`./sxemacs -sd`.dmp && \
	$(LN_S) sxemacs-$(old_version)-`./sxemacs -sd`.dmp sxemacs.dmp
else
	cd $(DESTDIR)$(bindir) && \
	mv -f sxemacs.dmp sxemacs-$(old_version)
endif

uninstall-hook:
	cd $(DESTDIR)$(bindir) && \
	$(RM) sxemacs-$(old_version)

.PHONY: stage1 stage2 build-report beta
## Convenience target for SXEmacs beta testers
beta: clean all

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


build-report: env-report
	$(MAKE) $(RECURSIVE_MAKE_ARGS) beta 2>&1 | tee ,,beta.out
	$(MAKE) $(RECURSIVE_MAKE_ARGS) check 2>&1 | tee ,,make-check.out

time-build-report: env-report
	(time $(MAKE) $(RECURSIVE_MAKE_ARGS) beta ) 2>&1 | tee ,,beta.out
	(time $(MAKE) $(RECURSIVE_MAKE_ARGS) check ) 2>&1 | tee ,,make-check.out

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

