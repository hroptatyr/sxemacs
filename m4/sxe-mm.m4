dnl sxe-mm.m4 -- Multimedia goodness

dnl MM tests
dnl ========

AC_DEFUN([SXE_MM_CHECK_XPM], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for xpm support])
	AC_MSG_RESULT([])

	MM_SUCC
	if test "$window_system" = "none"; then
		MM_FAIL
	fi

	SXE_DUMP_LIBS
	ac_c_werror_flag=
	CPPFLAGS="$CPPFLAGS $X_CFLAGS"
	LDFLAGS="$LDFLAGS $X_PRE_LIBS $X_LIBS $libs_x"
	LIBS="$LIBS -lXpm -lX11"

	SXE_CHECK_HEADERS([X11/xpm.h], [:], [MM_FAIL])

	AC_MSG_CHECKING([for Xpm (more recent than 3.4f)])
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#define XPM_NUMBERS
#include <X11/xpm.h>
int main(int c, char **v)
{
	return c == 1 ? 0 :
		XpmIncludeVersion != XpmLibraryVersion() ? 1 :
		XpmIncludeVersion < 30406 ? 2 : 0 ;
}]])], [./conftest dummy_arg; xpm_status=$?;
		if test "$xpm_status" = "0"; then
			AC_MSG_RESULT([yes])
		else
			AC_MSG_RESULT([no])
			MM_FAIL
			if test "$xpm_status" = "1"; then
				AC_MSG_WARN([dnl
Xpm library version and header file version don't match!
I have disabled xpm on your behalf.])
			elif test "$xpm_status" = "2"; then
				AC_MSG_WARN([dnl
Xpm library version is too old!
I have disabled xpm on your behalf.])
			else
				AC_MSG_WARN([dnl
Internal xpm detection logic error!])
			fi
		fi], [dnl
		AC_MSG_RESULT([no])
		MM_FAIL], [dnl
		AC_MSG_RESULT([no])
		MM_FAIL])
	xe_check_libs=
	SXE_RESTORE_LIBS
])dnl SXE_MM_CHECK_XPM

AC_DEFUN([SXE_MM_CHECK_XFACE], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for xface support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	ac_c_werror_flag=
	dnl SXE_PREPEND([-I$x_includes], [c_switch_site])
	dnl SXE_PREPEND([-L$x_libraries], [ld_switch_site])
	CPPFLAGS="$CPPFLAGS $X_CFLAGS"
	LDFLAGS="$LDFLAGS $X_LIBS"

	MM_SUCC
	if test "$window_system" = "none"; then
		MM_FAIL
	fi

	SXE_CHECK_HEADERS([compface.h], [:], [MM_FAIL])
	AC_CHECK_LIB([compface], [UnGenFace], [:], [MM_FAIL])

	SXE_RESTORE_LIBS
])dnl SXE_MM_CHECK_XFACE

AC_DEFUN([SXE_MM_CHECK_GIF], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for gif support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS

	MM_SUCC
	if test "$window_system" = "none"; then
		MM_FAIL
	fi

	SXE_RESTORE_LIBS
])dnl SXE_MM_CHECK_GIF

AC_DEFUN([SXE_MM_CHECK_JPEG], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for jpeg support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	ac_c_werror_flag=
	dnl SXE_PREPEND([-I$x_includes], [c_switch_site])
	dnl SXE_PREPEND([-L$x_libraries], [ld_switch_site])
	CPPFLAGS="$CPPFLAGS $X_CFLAGS"
	LDFLAGS="$LDFLAGS $X_LIBS"

	MM_SUCC
	if test "$window_system" = "none"; then
		MM_FAIL
	fi

	SXE_CHECK_HEADERS([jpeglib.h], [:], [MM_FAIL])
	AC_CHECK_LIB([jpeg], [jpeg_destroy_decompress], [:], [MM_FAIL], [$INFLATE_LIB])

	SXE_RESTORE_LIBS
])dnl SXE_MM_CHECK_JPEG

AC_DEFUN([SXE_MM_CHECK_PNG], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for PNG support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	ac_c_werror_flag=
	dnl SXE_PREPEND([-I$x_includes], [c_switch_site])
	dnl SXE_PREPEND([-L$x_libraries], [ld_switch_site])
	CPPFLAGS="$CPPFLAGS $X_CFLAGS"
	LDFLAGS="$LDFLAGS $X_LIBS"

	MM_SUCC
	if test "$window_system" = "none"; then
		MM_FAIL
	fi

	SXE_CHECK_HEADERS([png.h], [:], [MM_FAIL])
	AC_CHECK_FUNC([pow], [:], [MM_FAIL]) dnl someone explain?
	AC_CHECK_LIB([png], [png_read_image], [:], [MM_FAIL], [$INFLATE_LIB])

	AC_MSG_CHECKING(for workable png version information)
	dnl xe_check_libs="-lpng $INFLATE_LIB"
	LIBS="$LIBS -lpng $INFLATE_LIB"
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <png.h>
int main(int c, char **v)
{
	if (c == 1)
		return 0;
	if (strcmp(png_libpng_ver, PNG_LIBPNG_VER_STRING) != 0)
		return 1;
	return (PNG_LIBPNG_VER < 10002) ? 2 : 0 ;
}]])], [./conftest dummy_arg; png_status=$?;
		if test "$png_status" = "0"; then
			AC_MSG_RESULT([yes])
		else
			AC_MSG_RESULT([no])
			MM_FAIL
			if test "$png_status" = "1"; then
				AC_MSG_WARN([dnl
PNG library version and header file don't match!
I have disabled PNG support on your behalf.])
			elif test "$png_status" = "2"; then
				AC_MSG_WARN([dnl
PNG library version too old (pre 1.0.2)!
I have disabled PNG support on your behalf.])
			fi
		fi], [dnl
		AC_MSG_RESULT([no])
		MM_FAIL], [dnl
		AC_MSG_RESULT([no])
		MM_FAIL])
	xe_check_libs=

	SXE_RESTORE_LIBS
])dnl SXE_MM_CHECK_PNG

AC_DEFUN([SXE_MM_CHECK_TIFF], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for TIFF support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	ac_c_werror_flag=
	dnl SXE_PREPEND([-I$x_includes], [c_switch_site])
	dnl SXE_PREPEND([-L$x_libraries], [ld_switch_site])
	CPPFLAGS="$CPPFLAGS $X_CFLAGS"
	LDFLAGS="$LDFLAGS $X_LIBS"

	MM_SUCC
	if test "$window_system" = "none"; then
		MM_FAIL
	fi

	SXE_CHECK_HEADERS([tiffio.h], [:], [MM_FAIL])
	AC_CHECK_LIB([tiff], [TIFFClientOpen], [:], [MM_FAIL], [$INFLATE_LIB])

	SXE_RESTORE_LIBS
])dnl SXE_MM_CHECK_TIFF

AC_DEFUN([SXE_MM_SEARCH_INFLATE], [
	dnl Too many stupid linkers can't detect cascaded lib dependencies
	dnl  until runtime. So we always search for libz compression support.
	AC_SEARCH_LIBS([inflate], [c z gz], [
		if test "$ac_cv_lib_c_inflate" = "yes"; then
			INFLATE_LIB="c"
		elif test "$ac_cv_lib_z_inflate" = "yes"; then
			INFLATE_LIB="z"
		elif test "$ac_cv_lib_gz_inflate" = "yes"; then
			INFLATE_LIB="gz"
		fi], [INFLATE_LIB=])
	if test -n "$INFLATE_LIB"; then
		SXE_PREPEND([$INFLATE_LIB], [MM_LIBS])
	fi
])dnl SXE_MM_SEARCH_INFLATE

AC_DEFUN([SXE_MM_CHECK_SNDFILE], [
	## assumes $PKG_CONFIG is defined
	## arg #1: action on success
	## arg #2: action on failure

	_SXE_MM_CHECK_pkgconfig_based([sndfile], [sndfile], [1.0.12], [dnl
		sf_open sf_close sf_readf_short sf_readf_int dnl
		sf_readf_float sf_seek sf_open_virtual],
		[sndfile.h], [$1], [$2])
])dnl SXE_MM_CHECK_SNDFILE

AC_DEFUN([SXE_MM_CHECK_FFMPEG], [
	## assumes $PKG_CONFIG is defined
	## arg #1: action on success
	## arg #2: action on failure

	sxe_cv_feat_ffmpeg=
	_SXE_MM_CHECK_pkgconfig_based([ffmpeg], [libavformat], [49.0.0], [dnl
		av_open_input_file av_close_input_file av_find_stream_info dnl
		url_fopen av_probe_input_format avcodec_find_decoder dnl
		avcodec_open av_read_frame av_seek_frame av_register_all dnl
		avcodec_decode_audio avcodec_decode_audio2], [avformat.h],
		[sxe_cv_feat_ffmpeg=yes], [sxe_cv_feat_ffmpeg=no])

	## newer ffmpegs want a bioctx** in url_fopen, check that
	AC_MSG_CHECKING([what url_fopen() needs])
	sxe_cv_tmp_ffmpeg_url_fopen="uncertain"

	save_ac_c_werror_flag="$ac_c_werror_flag"
	SXE_DUMP_LIBS
	CPPFLAGS="$CPPFLAGS $($PKG_CONFIG --cflags libavformat)"
	AC_LANG_WERROR([on])
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#if defined HAVE_ERRNO_H
# include <errno.h>
#endif
#include <avformat.h>

extern int foobar(void);
int foobar(void)
{
	ByteIOContext *bioctx = 0;

	url_fopen(&bioctx, "/foobar", URL_RDONLY);
	return 0;
}
		]])], [sxe_cv_tmp_ffmpeg_url_fopen="ByteIOContext**"], [:])
	SXE_RESTORE_LIBS

	SXE_DUMP_LIBS
	CPPFLAGS="$CPPFLAGS $($PKG_CONFIG --cflags libavformat)"
	AC_LANG_WERROR([on])
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#if defined HAVE_ERRNO_H
# include <errno.h>
#endif
#include <avformat.h>

extern int foobar(void);
int foobar(void)
{
	ByteIOContext *bioctx = 0;

	url_fopen(bioctx, "/foobar", URL_RDONLY);
	return 0;
}
		]])], [sxe_cv_tmp_ffmpeg_url_fopen="ByteIOContext*"], [:])
	SXE_RESTORE_LIBS

	## reset -Werror flag
	ac_c_werror_flag=$save_ac_c_werror_flag
	## post the result
	AC_MSG_RESULT([$sxe_cv_tmp_ffmpeg_url_fopen])

	if test "$sxe_cv_tmp_ffmpeg_url_fopen" = "ByteIOContext**"; then
		AC_DEFINE([FFMPEG_URL_FOPEN_BIOCTX_STAR_STAR], [1],
			[Whether url_fopen want a ByteIOContext**])
	elif test "$sxe_cv_tmp_ffmpeg_url_fopen" = "needs_star_star"; then
		AC_DEFINE([FFMPEG_URL_FOPEN_BIOCTX_STAR], [1],
			[Whether url_fopen want a ByteIOContext*])
	else
		sxe_cv_feat_ffmpeg="no"
	fi

	if test "$sxe_cv_feat_ffmpeg" = "yes"; then
		:
		$1
	else
		:
		$2
	fi
])dnl SXE_MM_CHECK_FFMPEG

dnl
dnl SoX
dnl ===
AC_DEFUN([SXE_MM_CHECK_SOX], [dnl
	## call like this SXE_MM_CHECK_SOX([<if-found>], [<if-not-found>])
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([ACTION_IF_FOUND], [$1])
	pushdef([ACTION_IF_NOT_FOUND], [$2])

	AC_CACHE_CHECK([for SoX support], [sxe_cv_feat_sox], [_SXE_CHECK_SOX])

	if test "$sox_too_old" = "yes"; then
		AS_MESSAGE([*** Detected SoX, but it is too old.])
		AS_MESSAGE([*** Consider upgrading, see http://sox.sourceforge.net])
		ACTION_IF_NOT_FOUND
		:
	elif test "$sxe_cv_feat_sox" = "yes"; then
		ACTION_IF_FOUND
		:
	else
		ACTION_IF_NOT_FOUND
		:
	fi

	popdef([ACTION_IF_FOUND])
	popdef([ACTION_IF_NOT_FOUND])
])dnl SXE_MM_CHECK_SOX

AC_DEFUN([_SXE_CHECK_SOX], [dnl
	AC_REQUIRE([SXE_CHECK_SOX_LOCATOR])

	if test "$have_libst_config" = "no" -o -z "$LIBST_CONFIG"; then
		AS_MESSAGE([*** libst-config not found.])
		AS_MESSAGE([*** Cannot check for SoX.])
		have_libst_config=no
		LIBST_CONFIG=
	else
		AC_REQUIRE([SXE_CHECK_SOX_LOCATIONS])
		AC_REQUIRE([SXE_CHECK_SOX_HEADERS])
		AC_REQUIRE([SXE_CHECK_SOX_LIBS])
		AC_REQUIRE([SXE_CHECK_SOX_STRUCTS])
		:
	fi

	if test "$ac_cv_lib_sox_sox_open_read" = "yes" -a \
		"$ac_cv_lib_sox_sox_close" = "yes" -a \
		"$ac_cv_lib_sox_sox_seek" = "yes" -a \
		"$ac_cv_header_sox_h" = "yes" -a \
		"$ac_cv_type_sox_format_t" = "yes" -a \
		"$ac_cv_type_struct_sox_format" = "yes"; then
		sox_libs="$sox_libs -lsox"
		sxe_cv_feat_sox="yes"
	elif test "$ac_cv_lib_st_st_close" = "yes" -a \
		"$ac_cv_lib_st_st_read" = "yes" -a \
		"$ac_cv_lib_st_st_seek" = "yes" -a \
		"$ac_cv_header_st_h" = "yes" -a \
		"$ac_cv_type_ft_t" = "yes" -a \
		"$ac_cv_type_struct_st_soundstream" = "yes"; then
		sox_libs="$sox_libs -lst"
		sxe_cv_feat_sox="yes"
	else
		sxe_cv_feat_sox="no"
	fi
])dnl _SXE_CHECK_SOX

AC_DEFUN([SXE_CHECK_SOX_LOCATOR], [dnl
	SXE_SEARCH_CONFIG_PROG([libst-config])
])dnl SXE_CHECK_SOX_LOCATOR

AC_DEFUN([SXE_CHECK_SOX_LOCATIONS], [dnl
	AC_REQUIRE([SXE_CHECK_SOX_LOCATOR])
	if test "$have_libst_config" = "no" -o -z "$LIBST_CONFIG"; then
		sox_cppflags=
		sox_ldflags=
		sox_libs=
	else
		sox_cppflags="$($LIBST_CONFIG --cflags)"
		sox_ldflags="-L$($LIBST_CONFIG --libdir)"
		sox_libs="$($LIBST_CONFIG --libs)"
	fi
])dnl SXE_CHECK_SOX_PLACES

AC_DEFUN([SXE_PUMP_SOX_LOCATIONS], [dnl
	SXE_DUMP_LIBS
	CPPFLAGS="$CPPFLAGS $sox_cppflags"
	LDFLAGS="$LDFLAGS $sox_ldflags"
	LIBS="$LIBS $sox_libs"
])dnl SXE_PUMP_SOX_LOCATIONS

AC_DEFUN([SXE_DUMP_SOX_LOCATIONS], [dnl
	SXE_RESTORE_LIBS
])dnl SXE_DUMP_SOX_LOCATIONS

AC_DEFUN([SXE_CHECK_SOX_HEADERS], [dnl
	AC_REQUIRE([SXE_CHECK_SOX_LOCATIONS])
	SXE_PUMP_SOX_LOCATIONS
	AC_CHECK_HEADERS([st.h])
	AC_CHECK_HEADERS([sox.h])
	SXE_DUMP_SOX_LOCATIONS
])dnl SXE_CHECK_SOX_HEADERS

AC_DEFUN([SXE_CHECK_SOX_LIBS], [dnl
	AC_REQUIRE([SXE_CHECK_SOX_LOCATIONS])

	echo "void cleanup(void) {}" > cleanup.c
	$CC -c -o cleanup.o cleanup.c

	SXE_PUMP_SOX_LOCATIONS
	## we need 12.17.9 with st_open_read
	AC_CHECK_LIB([st], [st_open_read], [:], [:],
		[cleanup.o $sox_ldflags $sox_libs])
	AC_CHECK_LIB([st], [st_close], [:], [:], [cleanup.o $sox_ldflags $sox_libs])
	AC_CHECK_LIB([st], [st_read], [:], [:], [cleanup.o $sox_ldflags $sox_libs])
	AC_CHECK_LIB([st], [st_seek], [:], [:], [cleanup.o $sox_ldflags $sox_libs])

	## checks for the spankin' new sox
	AC_CHECK_LIB([sox], [sox_open_read], [:], [:],
		[cleanup.o $sox_ldflags $sox_libs])
	AC_CHECK_LIB([sox], [sox_close], [:], [:], [cleanup.o $sox_ldflags $sox_libs])
	AC_CHECK_LIB([sox], [sox_read], [:], [:], [cleanup.o $sox_ldflags $sox_libs])
	AC_CHECK_LIB([sox], [sox_seek], [:], [:], [cleanup.o $sox_ldflags $sox_libs])

	SXE_DUMP_SOX_LOCATIONS

	## clean up our cleanup snack
	rm -f cleanup.c cleanup.o
])dnl SXE_CHECK_SOX_LIBS

AC_DEFUN([SXE_CHECK_SOX_STRUCTS], [dnl
	## the old structs
	AC_CHECK_TYPES([ft_t], [:], [:], [
#if defined HAVE_ST_H
# include <st.h>
#endif
		])
	AC_CHECK_TYPES([st_signalinfo_t], [:], [:], [
#if defined HAVE_ST_H
# include <st.h>
#endif
		])
	AC_CHECK_TYPES([st_ssize_t], [:], [:], [
#ifdef HAVE_ST_H
# include <st.h>
#endif
		])
	AC_CHECK_TYPES([st_sample_t], [:], [:], [
#ifdef HAVE_ST_H
# include <st.h>
#endif
		])
	AC_CHECK_TYPES([struct st_soundstream], [:], [:], [
#if defined HAVE_ST_H
# include <st.h>
#endif
		])
	AC_CHECK_MEMBERS([struct st_soundstream.info], [:], [:], [
#if defined HAVE_ST_H
# include <st.h>
#endif
		])
	AC_CHECK_MEMBERS([struct st_soundstream.signal], [:], [:], [
#if defined HAVE_ST_H
# include <st.h>
#endif
		])

	## evaluating the results
	if test "$ac_cv_member_struct_st_soundstream_info" = "yes"; then
		AC_DEFINE([MEMBER_STRUCT_ST_SOUNDSTREAM_INFO], [1],
			[Whether we have the `info' slot])
	fi
	if test "$ac_cv_member_struct_st_soundstream_signal" = "yes"; then
		AC_DEFINE([MEMBER_STRUCT_ST_SOUNDSTREAM_SIGNAL], [1],
			[Whether we have the `signal' slot])
	fi

	## the new structs
	AC_CHECK_TYPES([sox_format_t], [:], [:], [
#ifdef HAVE_SOX_H
# include <sox.h>
#endif
		])
	AC_CHECK_TYPES([sox_ssize_t], [:], [:], [
#ifdef HAVE_SOX_H
# include <sox.h>
#endif
		])
	AC_CHECK_TYPES([sox_sample_t], [:], [:], [
#ifdef HAVE_SOX_H
# include <sox.h>
#endif
		])
	AC_CHECK_TYPES([sox_signalinfo_t], [:], [:], [
#if defined HAVE_SOX_H
# include <sox.h>
#endif
		])
	AC_CHECK_TYPES([struct sox_format], [:], [:], [
#ifdef HAVE_SOX_H
# include <sox.h>
#endif
		])
	AC_CHECK_MEMBERS([struct sox_format.signal], [:], [:], [
#ifdef HAVE_SOX_H
# include <sox.h>
#endif
		])
])dnl SXE_CHECK_SOX_STRUCTS


AC_DEFUN([SXE_MM_CHECK_MAD], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for mad support])
	AC_MSG_RESULT([])

	MM_SUCC
	SXE_CHECK_HEADERS([mad.h], [:], [MM_FAIL])
	AC_CHECK_LIB([mad], [mad_synth_init], [:], [MM_FAIL])
	AC_CHECK_LIB([mad], [mad_stream_init], [:], [MM_FAIL])
	AC_CHECK_LIB([mad], [mad_frame_init], [:], [MM_FAIL])
	AC_CHECK_LIB([mad], [mad_synth_frame], [:], [MM_FAIL])
	AC_CHECK_LIB([mad], [mad_stream_buffer], [:], [MM_FAIL])
	AC_CHECK_LIB([mad], [mad_frame_decode], [:], [MM_FAIL])

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_MAD

AC_DEFUN([SXE_MM_CHECK_MAGIC], [
	## assumes $PKG_CONFIG is defined
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for libmagic/file support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	MM_SUCC
	AC_CHECK_LIB([magic], [magic_open], [:], [MM_FAIL])
	SXE_CHECK_HEADERS([magic.h], [:], [MM_FAIL])
	SXE_CHECK_HEADERS([file.h])
	SXE_RESTORE_LIBS

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_MAGIC


AC_DEFUN([SXE_MM_CHECK_OSS], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for OSS support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	MM_FAIL
	SXE_CHECK_HEADERS([machine/soundcard.h sys/soundcard.h linux/soundcard.h],
		[MM_SUCC])
	SXE_RESTORE_LIBS

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_OSS

AC_DEFUN([SXE_MM_CHECK_PULSE], [
	## assumes $PKG_CONFIG is defined
	## arg #1: action on success
	## arg #2: action on failure

	PULSE_REQUIRED_VERSION=0.9.3
	_SXE_MM_CHECK_pkgconfig_based(dnl
		[pulse], [libpulse], [$PULSE_REQUIRED_VERSION], [dnl
		pa_mainloop_new pa_threaded_mainloop_new pa_mainloop_iterate dnl
		pa_threaded_mainloop_lock pa_threaded_mainloop_unlock dnl
		pa_mainloop_get_api pa_threaded_mainloop_get_api dnl
		pa_mainloop_free pa_threaded_mainloop_free dnl
		pa_threaded_mainloop_stop dnl
		pa_context_new pa_context_get_state pa_context_is_pending dnl
		pa_context_disconnect dnl
		pa_operation_unref dnl
		pa_stream_new pa_stream_get_state pa_stream_write dnl
		pa_stream_set_state_callback pa_stream_set_write_callback dnl
		pa_stream_unref pa_stream_connect_playback pa_stream_disconnect dnl
		pa_stream_cork],
		[pulse/pulseaudio.h], [$1], [$2])
])dnl SXE_MM_CHECK_PULSE

AC_DEFUN([SXE_MM_CHECK_JACK], [
	## assumes $PKG_CONFIG is defined
	## arg #1: action on success
	## arg #2: action on failure

	_SXE_MM_CHECK_pkgconfig_based([jack], [jack], [0.98.0], [dnl
		jack_client_open jack_get_ports jack_port_register dnl
		jack_set_process_callback jack_set_error_function dnl
		jack_on_shutdown jack_activate jack_connect jack_disconnect dnl
		jack_client_close jack_port_get_buffer], [jack/jack.h], [$1], [$2])
])dnl SXE_MM_CHECK_JACK

AC_DEFUN([SXE_MM_CHECK_AO], [
	## assumes $PKG_CONFIG is defined
	## arg #1: action on success
	## arg #2: action on failure

	_SXE_MM_CHECK_pkgconfig_based([ao], [ao], [0.6], [dnl
		ao_initialize ao_driver_id ao_default_driver_id dnl
		ao_open_live ao_close ao_shutdown ao_play], [ao/ao.h], [$1], [$2])
])dnl SXE_MM_CHECK_AO

AC_DEFUN([SXE_MM_CHECK_ARTS], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	SXE_SEARCH_CONFIG_PROG([artsc-config])

	AC_MSG_CHECKING([for aRts support])
	AC_MSG_RESULT([])

	if test "$have_artsc_config" = "no" -o -z "$ARTSC_CONFIG"; then
		AS_MESSAGE([*** artsc-config not found.])
		AS_MESSAGE([*** Cannot check for aRts.])
		have_artsc_config=no
		ARTSC_CONFIG=
		MM_FAIL
	fi

	if test "$have_artsc_config" = "yes"; then
		ARTS_VERSION=`$ARTSC_CONFIG --arts-version`
		ARTS_MAJOR_VERSION=`echo $ARTS_VERSION | awk -F. '{print $1}'`
		ARTS_MINOR_VERSION=`echo $ARTS_VERSION | awk -F. '{print $2}'`

		dnl since we are not using most of the arts features, it suffices
		dnl to have a version 1.x (x >= 0)
		dnl if test "$ARTS_MAJOR_VERSION" -eq 1 -a \
		dnl   "$ARTS_MINOR_VERSION" -ge 0; then

		SXE_DUMP_LIBS
		ARTS_CPPFLAGS="`$ARTSC_CONFIG --cflags`"
		ARTS_LDFLAGS="-L`$ARTSC_CONFIG --libs`"
		CPPFLAGS="$CPPFLAGS $ARTS_CPPFLAGS"
		LDFLAGS="$CPPFLAGS $ARTS_LDFLAGS"

		MM_SUCC
		SXE_CHECK_HEADERS([artsc.h], [:], [MM_FAIL])
		AC_CHECK_LIB([artsc], [arts_init], [:], [MM_FAIL], [$arts_libs])
		AC_CHECK_LIB([artsc], [arts_free], [:], [MM_FAIL], [$arts_libs])
		AC_CHECK_LIB([artsc], [arts_write], [:], [MM_FAIL], [$arts_libs])
		AC_CHECK_LIB([artsc], [arts_play_stream], [:], [MM_FAIL], [$arts_libs])
		AC_CHECK_LIB([artsc], [arts_stream_set], [:], [MM_FAIL], [$arts_libs])
		AC_CHECK_LIB([artsc], [arts_close_stream], [:], [MM_FAIL], [$arts_libs])

		## restore anything
		SXE_RESTORE_LIBS
	fi

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_ARTS

AC_DEFUN([SXE_MM_CHECK_ESD], [
	## arg #1: action on success
	## arg #2: action on failure
	SXE_SEARCH_CONFIG_PROG([esd-config])

	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for ESD support])
	AC_MSG_RESULT([])

	if test "$have_esd_config" = "no" -o -z "$ESD_CONFIG"; then
		AS_MESSAGE([*** esd-config not found.])
		AS_MESSAGE([*** Cannot check for ESD.])
		have_esd_config=no
		ESD_CONFIG=
		MM_FAIL
	fi

	if test "$have_esd_config" = "yes"; then
		SXE_DUMP_LIBS
		ESD_CPPFLAGS="`$ESD_CONFIG --cflags`"
		ESD_LDFLAGS="-L`$ESD_CONFIG --libs`"
		dnl SXE_APPEND([$esd_c_switch], [c_switch_site])
		dnl SXE_PREPEND([$esd_libs], [LIBS])
		CPPFLAGS="$CPPFLAGS $ESD_CPPFLAGS"
		LDFLAGS="$LDFLAGS $ESD_LDFLAGS"

		MM_SUCC
		SXE_CHECK_HEADERS([esd.h], [:], [MM_FAIL])
		AC_CHECK_LIB([esd], [esd_play_stream], [:], [MM_FAIL], [$esd_libs])

		## restore anything
		SXE_RESTORE_LIBS
	fi

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_ESD

AC_DEFUN([SXE_MM_CHECK_ALSA], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for ALSA support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS
	MM_SUCC
	ac_c_werror_flag=$save_ac_c_werror_flag
	SXE_CHECK_HEADERS([alsa/input.h alsa/output.h alsa/global.h], [:], [MM_FAIL])
	SXE_CHECK_HEADERS([alsa/conf.h], [:], [MM_FAIL], [[
#include <stdlib.h>
#include <stdio.h>
#include <alsa/input.h>
#include <alsa/output.h>
#include <alsa/global.h>
]])
	SXE_CHECK_HEADERS([alsa/pcm.h alsa/error.h alsa/version.h], [:], [MM_FAIL], [[
#include <stdlib.h>
#include <stdio.h>
#include <alsa/input.h>
#include <alsa/output.h>
#include <alsa/global.h>
#include <alsa/conf.h>
]])

	ac_c_werror_flag=
	AC_MSG_CHECKING(for alsa version)
	AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <stdlib.h>
#include <stdio.h>
#include <alsa/input.h>
#include <alsa/output.h>
#include <alsa/conf.h>
#include <alsa/global.h>
#include <alsa/pcm.h>
#include <alsa/error.h>
#include <alsa/version.h>
int main(int c, char *v[]) 
{
	fprintf(stdout, SND_LIB_VERSION_STR);
	return 0;
}]])], dnl [./conftest; alsa_subminor=$?],[alsa_subminor=$?],[alsa_subminor=0]
	[alsa_ver=`./conftest`], [:], [alsa_ver=])

	case "$alsa_ver" in
	0.*.* | 1.0.3* | 1.0.9* )
		AC_MSG_RESULT([ (known to break)])
		AC_MSG_WARN([Your ALSA version is _KNOWN_ to fail! Do not say we have not warned you!])
		;;
	1.0.2* | 1.0.4* | 1.0.5* | 1.0.6* | 1.0.7* | 1.0.8* ) 
		AC_MSG_RESULT([ (suspicious to break)])
		AC_MSG_WARN([Your ALSA version has not been tested. Do not be surprised if it fails!])
		;;
	1.0.10* | 1.0.11* | 1.0.12* | 1.0.13* | 1.0.14* | 1.0.15* )
		AC_MSG_RESULT([ (sane)])
		;;
	* )
		AC_MSG_RESULT([ (unknown)])
		AC_MSG_WARN([Your ALSA version is unknown and hence not supported!])
		MM_FAIL
		;;
	esac

	AC_CHECK_LIB([asound], [snd_pcm_open], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_close], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_hw_free], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_any], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_set_access], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_free], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_test_channels], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_test_format], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_set_channels], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_set_format], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_hw_params_set_rate_near], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_prepare], [:], [MM_FAIL])
	AC_CHECK_LIB([asound], [snd_pcm_writei], [:], [MM_FAIL])

	## restore anything
	SXE_RESTORE_LIBS

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_ALSA

AC_DEFUN([SXE_MM_CHECK_NAS], [
	## arg #1: action on success
	## arg #2: action on failure
	pushdef([MM_SUCC], [$1])
	pushdef([MM_FAIL], [$2])

	AC_MSG_CHECKING([for NAS support])
	AC_MSG_RESULT([])

	SXE_DUMP_LIBS

	MM_SUCC
	## NAS is often stored inside the X hierarchy, so ...
	CPPFLAGS="$CPPFLAGS $X_CFLAGS"
	LDFLAGS="$LDFLAGS $X_LIBS"
	SXE_CHECK_HEADERS([audio/audiolib.h], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuOpenServer], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuCloseServer], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuCreateFlow], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuStartFlow], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuStopFlow], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuScanForTypedEvent], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuDispatchEvent], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuSetErrorHandler], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuSetIOErrorHandler], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuWriteElement], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuSetElements], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuRegisterEventHandler], [:], [MM_FAIL])
	AC_CHECK_LIB([audio], [AuSetElementParameters], [:], [MM_FAIL])

	dnl If the nas library does not contain the error jump point,
	dnl then we force safer behavior.
	AC_EGREP_HEADER([AuXtErrorJump], [audio/Xtutil.h], [], [old_nas=yes])
	if test "$old_nas" = "yes"; then
		AC_DEFINE([NAS_NO_ERROR_JUMP], [1], [Description here!])
	fi

	## restore anything
	SXE_RESTORE_LIBS

	popdef([MM_SUCC])
	popdef([MM_FAIL])
])dnl SXE_MM_CHECK_NAS

dnl sxe-mm.m4 ends here
