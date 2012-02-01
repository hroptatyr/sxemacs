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

AC_DEFUN([_SXE_CHECK_PROC_FLAG], [dnl
	## arg #1 is the cpu flag
	## arg #2 the action if supported
	## arg #3 the action if not supported
	## i.e. a call looks like
	## SXE_CHECK_PROC_FLAG([<FLAG>], [<ACTION-IF-TRUE>], [<ACTION-IF-FALSE>])
	##
	## by side-effect this defines sxe_cv_proc_flag_<FLAG>
	pushdef([FLAG], [$1])
	pushdef([ACTION_IF_TRUE], [$2])
	pushdef([ACTION_IF_FALSE], [$3])

	AC_REQUIRE([AC_PROG_EGREP])
	AC_REQUIRE([SXE_PROC_FLAGS])

	SXE_MSG_CHECKING([whether cpu has ]FLAG[ flag])
	sxe_cv_proc_flag_[]FLAG[]=$(\
		echo "$sxe_cv_proc_flags" | $EGREP -c "\b[]FLAG[]\b")

	if test "$sxe_cv_proc_flag_[]FLAG[]" -gt "0"; then
		sxe_cv_proc_flag_[]FLAG[]="yes"
		ACTION_IF_TRUE
	else
		sxe_cv_proc_flag_[]FLAG[]="no"
		ACTION_IF_FALSE
	fi
	SXE_MSG_RESULT([$sxe_cv_proc_flag_]FLAG)

	popdef([ACTION_IF_FALSE])
	popdef([ACTION_IF_TRUE])
	popdef([FLAG])
])dnl _SXE_CHECK_PROC_FLAG

AC_DEFUN([SXE_CHECK_PROC_FLAG], [dnl
	## a mitigating variant that checks if the cpuid stuff
	## is actually available
	if test -n "${sxe_cv_proc_flags}" -a \
		"${sxe_cv_proc_flags}" != "no"; then
		## expand the real macro now
		_SXE_CHECK_PROC_FLAG($*)
	fi
])dnl SXE_CHECK_PROC_FLAG

AC_DEFUN([_SXE_CHECK_PROC_CODE], [dnl
	## arg #1 is the cpuid
	## arg #2 is the register to query
	## arg #3 is the bit to query
	## arg #4 is a nick name for the defined cache variable
	## arg #5 the action if supported
	## arg #6 the action if not supported
	##
	## by side-effect this defines sxe_cv_proc_code_<ID>_<REG>_<BIT>
	## which is yes if 1 and no when 0
	## if nick name (arg 4) is defined this one is used instead
	pushdef([ID], [$1])
	pushdef([REG], [$2])
	pushdef([BIT], [$3])
	pushdef([cv_nick], ifelse([$4], [],
		[sxe_cv_proc_code_]ID[_]REG[_]BIT, [$4]))
	pushdef([ACTION_IF_TRUE], [$5])
	pushdef([ACTION_IF_FALSE], [$6])

	AC_REQUIRE([SXE_PROC_FLAGS])

	SXE_MSG_CHECKING([whether cpu has bit ]BIT[ in reg ]REG[ set in id ]ID)

	case "[]REG[]" in
	*ax | *AX )
		i=1
		;;
	*bx | *BX )
		i=2
		;;
	*cx | *CX )
		i=3
		;;
	*dx | *DX )
		i=4
		;;
	* )
		i=[]REG[]
		;;
	esac

	sxe_cv_tmp="0x"$(echo $sxe_cv_proc_cpuid_[]ID[] | cut -d ":" -f $i)

	if test "$(($sxe_cv_tmp >> []BIT[] & 1))" = "1"; then
		[]cv_nick[]="yes"
		ACTION_IF_TRUE
	else
		[]cv_nick[]="no"
		ACTION_IF_FALSE
	fi
	SXE_MSG_RESULT([$]cv_nick[])

	popdef([ACTION_IF_FALSE])
	popdef([ACTION_IF_TRUE])
	popdef([cv_nick])
	popdef([BIT])
	popdef([REG])
	popdef([ID])
])dnl _SXE_CHECK_PROC_CODE

AC_DEFUN([SXE_CHECK_PROC_CODE], [dnl
	## a mitigating variant that checks if the cpuid stuff
	## is actually available

	if test -n "${sxe_cv_proc_cpuid}" -a \
		"${sxe_cv_proc_cpuid}" != "no"; then
		## expand the real macro now
		_SXE_CHECK_PROC_CODE($*)
	fi
])dnl SXE_CHECK_PROC_CODE


dnl simd extensions
AC_DEFUN([SXE_CHECK_PROC_MMX], [
	SXE_MSG_CHECKING([for MMX extensions])
	SXE_CHECK_PROC_CODE([00000001], [edx], [23], [sxe_cv_proc_mmx])
	SXE_MSG_RESULT([$sxe_cv_proc_mmx])
])dnl SXE_CHECK_PROC_MMX

AC_DEFUN([SXE_CHECK_PROC_AMDMMX], [
	SXE_MSG_CHECKING([for AMD MMX extensions])
	SXE_CHECK_PROC_CODE([80000001], [edx], [22], [sxe_cv_proc_amdmmx])
	SXE_MSG_RESULT([$sxe_cv_proc_amdmmx])
])dnl SXE_CHECK_PROC_MMX

AC_DEFUN([SXE_CHECK_PROC_3DNOW], [
	SXE_MSG_CHECKING([for 3DNow! extensions])
	SXE_CHECK_PROC_CODE([80000001], [edx], [31], [sxe_cv_proc_3dnow])
	SXE_MSG_RESULT([$sxe_cv_proc_3dnow])
])dnl SXE_CHECK_PROC_MMX

AC_DEFUN([SXE_CHECK_PROC_3DNOW2], [
	SXE_MSG_CHECKING([for 3DNowExt extensions])
	SXE_CHECK_PROC_CODE([80000001], [edx], [30], [sxe_cv_proc_3dnow2])
	SXE_MSG_RESULT([$sxe_cv_proc_3dnow2])
])dnl SXE_CHECK_PROC_MMX

AC_DEFUN([SXE_CHECK_PROC_SSE], [
	SXE_MSG_CHECKING([for SSE extensions])
	SXE_CHECK_PROC_CODE([00000001], [edx], [25], [sxe_cv_proc_sse])
	SXE_MSG_RESULT([$sxe_cv_proc_sse])
])dnl SXE_CHECK_PROC_SSE

AC_DEFUN([SXE_CHECK_PROC_SSE2], [
	SXE_MSG_CHECKING([for SSE2 extensions])
	SXE_CHECK_PROC_CODE([00000001], [edx], [26], [sxe_cv_proc_sse2])
	SXE_MSG_RESULT([$sxe_cv_proc_sse2])
])dnl SXE_CHECK_PROC_SSE2

AC_DEFUN([SXE_CHECK_PROC_SSE3], [
	SXE_MSG_CHECKING([for SSE3 extensions])
	SXE_CHECK_PROC_CODE([00000001], [ecx], [0], [sxe_cv_proc_sse3])
	SXE_MSG_RESULT([$sxe_cv_proc_sse3])
])dnl SXE_CHECK_PROC_SSE3

AC_DEFUN([SXE_CHECK_PROC_SSSE3], [
	SXE_MSG_CHECKING([for SSSE3 extensions])
	SXE_CHECK_PROC_CODE([00000001], [ecx], [9], [sxe_cv_proc_ssse3])
	SXE_MSG_RESULT([$sxe_cv_proc_ssse3])
])dnl SXE_CHECK_PROC_SSSE3

AC_DEFUN([SXE_CHECK_PROC_SSE41], [
	SXE_MSG_CHECKING([for SSE4.1 extensions])
	SXE_CHECK_PROC_CODE([00000001], [ecx], [19], [sxe_cv_proc_sse41])
	SXE_MSG_RESULT([$sxe_cv_proc_sse41])
])dnl SXE_CHECK_PROC_SSE41

AC_DEFUN([SXE_CHECK_PROC_SSE42], [
	SXE_MSG_CHECKING([for SSE4.2 extensions])
	SXE_CHECK_PROC_CODE([00000001], [ecx], [20], [sxe_cv_proc_sse42])
	SXE_MSG_RESULT([$sxe_cv_proc_sse42])
])dnl SXE_CHECK_PROC_SSE42

AC_DEFUN([SXE_CHECK_PROC_SSE4A], [
	SXE_MSG_CHECKING([for SSE4A extensions])
	SXE_CHECK_PROC_CODE([80000001], [ecx], [6], [sxe_cv_proc_sse4A])
	SXE_MSG_RESULT([$sxe_cv_proc_sse4A])
])dnl SXE_CHECK_PROC_SSE4A

AC_DEFUN([SXE_CHECK_PROC_SSE5], [
	SXE_MSG_CHECKING([for SSE5 extensions])
	SXE_CHECK_PROC_CODE([80000001], [ecx], [11], [sxe_cv_proc_sse5])
	SXE_MSG_RESULT([$sxe_cv_proc_sse5])
])dnl SXE_CHECK_PROC_SSE5

AC_DEFUN([SXE_CHECK_SIMD_EXTENSIONS], [
	## note that we enable "unsafe" fp optimisation with other compilers, too

	SXE_CHECK_PROC_MMX
	SXE_CHECK_PROC_AMDMMX
	SXE_CHECK_PROC_3DNOW
	SXE_CHECK_PROC_3DNOW2
	SXE_CHECK_PROC_SSE
	SXE_CHECK_PROC_SSE2
	SXE_CHECK_PROC_SSE3
	SXE_CHECK_PROC_SSSE3
	SXE_CHECK_PROC_SSE41
	SXE_CHECK_PROC_SSE42
	SXE_CHECK_PROC_SSE4A
	SXE_CHECK_PROC_SSE5

	SXE_CHECK_COMPILER_FLAGS([-mmmx])
	SXE_CHECK_COMPILER_FLAGS([-maltivec])
	SXE_CHECK_COMPILER_FLAGS([-m3dnow])
	SXE_CHECK_COMPILER_FLAGS([-m3dnow -march=athlon])
	SXE_CHECK_COMPILER_FLAGS([-msse])
	SXE_CHECK_COMPILER_FLAGS([-msse2])
	SXE_CHECK_COMPILER_FLAGS([-msse3])
	SXE_CHECK_COMPILER_FLAGS([-mssse3])
	SXE_CHECK_COMPILER_FLAGS([-msse4])
	SXE_CHECK_COMPILER_FLAGS([-msse4.1])
	SXE_CHECK_COMPILER_FLAGS([-msse4.2])
	SXE_CHECK_COMPILER_FLAGS([-msse4a])
	SXE_CHECK_COMPILER_FLAGS([-msse5])
	SXE_CHECK_COMPILER_FLAGS([-mfpmath=sse])
	SXE_CHECK_COMPILER_FLAGS([-msse -mfpmath=sse])
	SXE_CHECK_COMPILER_FLAGS([-maes])
	SXE_CHECK_COMPILER_FLAGS([-mabm])
	SXE_CHECK_COMPILER_FLAGS([-mpclmul])
	SXE_CHECK_COMPILER_FLAGS([-mavx])

	## ssssssssss
	simdflags=""

	## actually the SSE sets contain each other, but this'll change
	## with the new generation stuff, SSE4.1, SSE4.2, SSE4a, SSE5
	## THUS we just include all them flags
	sxe_cv_c_flags__msse_added="no"
	if test "$sxe_cv_proc_sse5" = "yes" -a \
		"$sxe_cv_c_flags__msse5"; then
		sxe_cv_c_flags__msse_added="yes"
		simdflags="$simdflags -msse5"
	fi

	if test "$sxe_cv_proc_sse4A" = "yes" -a \
		"$sxe_cv_c_flags__msse4a"; then
		sxe_cv_c_flags__msse_added="yes"
		simdflags="$simdflags -msse4a"
	fi

	if test "$sxe_cv_proc_sse41" = "yes" -a \
		"$sxe_cv_c_flags__msse4_1"; then
		sxe_cv_c_flags__msse_added="yes"
		simdflags="$simdflags -msse4.1"
	fi

	if test "$sxe_cv_proc_sse42" = "yes" -a \
		"$sxe_cv_c_flags__msse4_2"; then
		sxe_cv_c_flags__msse_added="yes"
		simdflags="$simdflags -msse4.2"
	fi

	if test "$sxe_cv_proc_ssse3" = "yes" -a \
		"$sxe_cv_c_flags__mssse3"; then
		sxe_cv_c_flags__msse_added="yes"
		simdflags="$simdflags -mssse3"
	fi

	if test "$sxe_cv_proc_sse3" = "yes" -a \
		"$sxe_cv_c_flags__msse3"; then
		sxe_cv_c_flags__msse_added="yes"
		simdflags="$simdflags -msse3"
	fi

	if test "$sxe_cv_proc_sse2" = "yes" -a \
		"$sxe_cv_c_flags__msse2" = "yes"; then
			sxe_cv_c_flags__msse_added="yes"
			simdflags="$simdflags -msse2"
	fi

	if test "$sxe_cv_proc_sse" = "yes" -a \
		"$sxe_cv_c_flags__msse" = "yes"; then
			sxe_cv_c_flags__msse_added="yes"
			simdflags="$simdflags -msse"
	fi

	if test "$sxe_cv_c_flags__mfpmath_sse" = "yes" -a \
		"$sxe_cv_c_flags__msse_added" = "yes"; then
			simdflags="$simdflags -mfpmath=sse"
	elif test "$sxe_cv_c_flags__msse__mfpmath_sse" = "yes" -a \
		"$sxe_cv_c_flags__msse_added" = "no"; then
			simdflags="$simdflags -mfpmath=sse"
	fi

	## also care about this new stuff ... what's their CPU proc equivalent?
	if test "$sxe_cv_c_flags__maes" = "yes"; then
			simdflags="$simdflags -maes"
	fi
	if test "$sxe_cv_c_flags__mabm" = "yes"; then
			simdflags="$simdflags -mabm"
	fi
	if test "$sxe_cv_c_flags__mpclmul" = "yes"; then
			simdflags="$simdflags -mpclmul"
	fi

	AC_SUBST([simdflags])
])dnl SXE_CHECK_SIMD_EXTENSIONS

dnl the FPU itself
AC_DEFUN([SXE_CHECK_PROC_X87FPU], [
	SXE_MSG_CHECKING([whether x87 fpu is available])
	SXE_CHECK_PROC_CODE([00000001], [edx], [0], [sxe_cv_proc_x87fpu])
	SXE_MSG_RESULT([$sxe_cv_proc_x87fpu])
])dnl SXE_CHECK_PROC_X87FPU

dnl conditional moves
AC_DEFUN([SXE_CHECK_PROC_CMOV], [
	SXE_MSG_CHECKING([whether CMOV instructions are available])
	SXE_CHECK_PROC_CODE([00000001], [edx], [15], [sxe_cv_proc_cmov])
	SXE_MSG_RESULT([$sxe_cv_proc_cmov])
])dnl SXE_CHECK_PROC_CMOV

dnl fast float storing and restoring
AC_DEFUN([SXE_CHECK_PROC_FXSR], [
	SXE_MSG_CHECKING([whether FXSAVE/FXRSTOR instructions are available])
	SXE_CHECK_PROC_CODE([00000001], [edx], [24], [sxe_cv_proc_fxsr])
	SXE_MSG_RESULT([$sxe_cv_proc_fxsr])
])dnl SXE_CHECK_PROC_FXSR

dnl AMD's idea of it
AC_DEFUN([SXE_CHECK_PROC_FFXSR], [
	SXE_MSG_CHECKING([whether FXSAVE/FXRSTOR instruction optimisations are available])
	SXE_CHECK_PROC_CODE([80000001], [edx], [25], [sxe_cv_proc_ffxsr])
	SXE_MSG_RESULT([$sxe_cv_proc_ffxsr])
])dnl SXE_CHECK_PROC_FFXSR

dnl AMD's bit fiddlers, VERY useful
AC_DEFUN([SXE_CHECK_PROC_ABM], [
	SXE_MSG_CHECKING([whether advanced bit manipulation instructions (ABM) are available])
	SXE_CHECK_PROC_CODE([80000001], [ecx], [5], [sxe_cv_proc_abm])
	SXE_MSG_RESULT([$sxe_cv_proc_abm])
])dnl SXE_CHECK_PROC_ABM

dnl time stamp counter
AC_DEFUN([SXE_CHECK_PROC_TSC], [
	SXE_MSG_CHECKING([whether time stamp counter (TSC) is available])
	SXE_CHECK_PROC_CODE([00000001], [edx], [4], [sxe_cv_proc_tsc])
	SXE_MSG_RESULT([$sxe_cv_proc_tsc])
])dnl SXE_CHECK_PROC_TSC

AC_DEFUN([SXE_CHECK_PROC_GOODNESS], [
	SXE_CHECK_PROC_X87FPU
	SXE_CHECK_PROC_FXSR
	SXE_CHECK_PROC_FFXSR
	SXE_CHECK_PROC_TSC
	SXE_CHECK_PROC_ABM
	SXE_CHECK_PROC_CMOV
])dnl SXE_CHECK_PROC_GOODNESS

dnl sxe-machine.m4 ends here
