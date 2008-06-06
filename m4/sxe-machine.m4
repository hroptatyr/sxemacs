dnl machine.m4 --- machine characteristics
dnl
dnl Copyright (C) 2005, 2006, 2007, 2008 Sebastian Freundt
dnl
dnl Author: Sebastian Freundt <hroptatyr@sxemacs.org>
dnl
dnl This file is part of ASE.


AC_DEFUN([SXE_PROC_VENDORID], [dnl
	## arg 1 is the entire name (cpuid_0)
	AC_REQUIRE([AC_PROG_CC])
	AC_LANG_PUSH([C])

	AC_CHECK_HEADERS([stdio.h])
	AC_CACHE_CHECK([for vendorid], [sxe_cv_proc_vendorid], [
		AC_RUN_IFELSE([AC_LANG_PROGRAM([
#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

void print_reg(FILE *f, long unsigned int reg);

void
print_reg(FILE *f, long unsigned int reg)
{
	int i;
	for (i = 0; i < 4; i++, reg >>= 8) {
		fputc((int)(reg & 0xff), f);
	}
	return;
}
		], [
	long unsigned int eax, ebx, ecx, edx;
	long unsigned int reg = 0;
	FILE *f;

	f = fopen("conftest_vendorid", "w");
	if (!f) {
		return 1;
	}

	__asm__("cpuid"
		: "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx)
		: "a" (reg));
	print_reg(f, ebx);
	print_reg(f, edx);
	print_reg(f, ecx);
	fputc('\n', f);
	fclose(f);
	return 0;
			])], [
			## then case
			sxe_cv_proc_vendorid=$(cat "conftest_vendorid")
			rm -f "conftest_vendorid"
			], [
			## else case
			sxe_cv_proc_vendorid="no"
			rm -f "conftest_vendorid"], [
			## cross compiling case
			sxe_cv_proc_vendorid="no"])])

	AC_LANG_POP([C])
])dnl SXE_PROC_VENDORID

AC_DEFUN([__SXE_PROC_CPUID], [dnl
	## arg 1 is the file name into which to put the stuff
	AC_REQUIRE([AC_PROG_CC])
	AC_LANG_PUSH([C])

	AC_CHECK_HEADERS([stdio.h])
	AC_CACHE_CHECK([for cpuid $1], [sxe_cv_proc_cpuid_$1], [
		AC_RUN_IFELSE([AC_LANG_PROGRAM([
#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif
#define FMT	"%08lx:%08lx:%08lx:%08lx:%08lx\n"
			], [
	long unsigned int eax, ebx, ecx, edx;
	long unsigned int reg = $1, max;
	FILE *f;

	f = fopen("conftest_cpuid", "w");
	if (!f) {
		return 1;
	}

	max = 0;
	__asm__("cpuid"
		: "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx)
		: "a" (reg));
		fprintf(f, FMT, reg, eax, ebx, ecx, edx);

	fclose(f);
	return 0;
		])], [
		## then case
		sxe_cv_proc_cpuid_$1=$(cat "conftest_cpuid" | \
			cut -d":" -f2-5)
		rm -f "conftest_cpuid"
		], [
		## else case
		sxe_cv_proc_cpuid_$1="no"
		rm -f "conftest_cpuid"], [
		## cross compiling case
		sxe_cv_proc_cpuid_$1="no"])])

	AC_LANG_POP([C])
])dnl __SXE_PROC_CPUID

AC_DEFUN([SXE_PROC_CPUID], [dnl
	## arg 1 is the file name into which to put the stuff
	AC_REQUIRE([AC_PROG_CC])
	AC_LANG_PUSH([C])

	cpuid_file=m4_default([$1],[cpuid])
	AC_CHECK_HEADERS([stdio.h])
	AC_CACHE_CHECK([for cpuid], [sxe_cv_proc_cpuid], [
		AC_RUN_IFELSE([AC_LANG_PROGRAM([
#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif
#define FMT	"%08lx:%08lx:%08lx:%08lx:%08lx\n"

			], [

	long unsigned int eax, ebx, ecx, edx;
	long unsigned int reg, max;
	FILE *f;

	f = fopen("$cpuid_file", "w");
	if (!f) {
		return 1;
	}

	max = 0;
	for (reg = max; reg <= max; reg++) {
	__asm__("cpuid"
		: "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx)
		: "a" (reg));
		if (reg == 0) {
			max = eax;
		}
		fprintf(f, FMT, reg, eax, ebx, ecx, edx);
	}

	max = 0x80000000;
	for (reg = max; reg <= max; reg++) {
	__asm__("cpuid"
		: "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx)
		: "a" (reg));
		if (reg == 0x80000000) {
			max = eax;
		}
		fprintf(f, FMT, reg, eax, ebx, ecx, edx);
	}

	max = 0x80860000;
	for (reg = max; reg <= max; reg++) {
	__asm__("cpuid"
		: "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx)
		: "a" (reg));
		if (reg == 0x80860000) {
			max = eax;
		}
		fprintf(f, FMT, reg, eax, ebx, ecx, edx);
	}

	max = 0xc0000000;
	for (reg = max; reg <= max; reg++) {
	__asm__("cpuid"
		: "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx)
		: "a" (reg));
		if (reg == 0xc0000000) {
			max = eax;
		}
		fprintf(f, FMT, reg, eax, ebx, ecx, edx);
	}

	fclose(f);
	return 0;
		])], [
		## then case
		sxe_cv_proc_cpuid="$cpuid_file"
		], [
		## else case
		sxe_cv_proc_cpuid="no"], [
		## cross compiling case
		sxe_cv_proc_cpuid="no"])])
	AC_LANG_POP([C])

	if test -r "$sxe_cv_proc_cpuid"; then
		for line in $(cat "$sxe_cv_proc_cpuid"); do
			dnl AS_MESSAGE([$line])
			flag=$(echo "$line" | cut -d":" -f1)
			AC_CACHE_CHECK([for cpuid $flag],
				[sxe_cv_proc_cpuid_$flag], [
				eval sxe_cv_proc_cpuid_$flag=$(echo "$line" | \
					cut -d":" -f2-5)
				])
		done
	fi
])dnl SXE_CPUID

AC_DEFUN([SXE_CHECK_PROC_CPUINFO], [dnl
	AC_CACHE_CHECK([for /proc/cpuinfo], [sxe_cv_file_proc_cpuinfo], [
		if test -f "/proc/cpuinfo" -a \
			-r "/proc/cpuinfo"; then
			sxe_cv_file_proc_cpuinfo="yes"
		else
			sxe_cv_file_proc_cpuinfo="no"
		fi])
])dnl SXE_CHECK_PROC_CPUINFO

## inspired by a snippet of Michael Paul Bailey <jinxidoru@byu.net>
AC_DEFUN([SXE_NPROCESSORS], [dnl
	AC_REQUIRE([AC_PROG_EGREP])
	AC_REQUIRE([SXE_CHECK_PROC_CPUINFO])

	AC_CACHE_CHECK([for the number of processors], [sxe_cv_proc_number], [
		if test "$sxe_cv_file_proc_cpuinfo" = "yes"; then
			sxe_cv_proc_number=$($EGREP -c "^processor" "/proc/cpuinfo")
		else
			sxe_cv_proc_number="no"
		fi
		## convenience var
		sxe_nprocessors="$sxe_cv_proc_number"
		])
])dnl SXE_NPROCESSORS

AC_DEFUN([SXE_PROC_FLAGS], [dnl
	AC_REQUIRE([AC_PROG_EGREP])
	AC_REQUIRE([SXE_CHECK_PROC_CPUINFO])
	
	AC_CACHE_CHECK([for processor flags], [sxe_cv_proc_flags], [
		if test "$sxe_cv_file_proc_cpuinfo" = "yes"; then
			sxe_cv_proc_flags=$($EGREP "^flags" "/proc/cpuinfo" | \
				head -n1 | \
				cut -d":" -f2 | \
				cut -d" " -f2-)
		else
			sxe_cv_proc_flags="no"
		fi
		## convenience var
		sxe_proc_flags="$sxe_cv_proc_flags"
		])
])dnl SXE_PROC_FLAGS

AC_DEFUN([SXE_CHECK_PROC_FLAG], [dnl
	## arg #1 is the cpu flag
	## arg #2 the action if supported
	## arg #3 the action if not supported
	## i.e. a call looks like
	## SXE_PROC_SUPPORTS_P([<FLAG>], [<ACTION-IF-TRUE>], [<ACTION-IF-FALSE>])
	##
	## by side-effect this defines sxe_cv_proc_flag_<FLAG>
	pushdef([FLAG], [$1])
	pushdef([ACTION_IF_TRUE], [$2])
	pushdef([ACTION_IF_FALSE], [$3])

	AC_REQUIRE([AC_PROG_EGREP])
	AC_REQUIRE([SXE_PROC_FLAGS])

	AC_MSG_CHECKING([whether cpu has ]FLAG[ flag])
	sxe_cv_proc_flag_[]FLAG[]=$(\
		echo "$sxe_cv_proc_flags" | $EGREP -c "\b[]FLAG[]\b")

	if test "$sxe_cv_proc_flag_[]FLAG[]" -gt "0"; then
		sxe_cv_proc_flag_[]FLAG[]="yes"
		ACTION_IF_TRUE
	else
		sxe_cv_proc_flag_[]FLAG[]="no"
		ACTION_IF_FALSE
	fi
	AC_MSG_RESULT([$sxe_cv_proc_flag_]FLAG)

	popdef([ACTION_IF_FALSE])
	popdef([ACTION_IF_TRUE])
	popdef([FLAG])
])dnl SXE_CHECK_PROC_FLAG

dnl sxe-machine.m4 ends here
