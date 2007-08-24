dnl sxe-sockets.m4 -- socket and network functions

AC_DEFUN([SXE_CHECK_SOCKLEN_T], [dnl

	SXE_CHECK_HEADERS([sys/types.h sys/socket.h])
	AC_CHECK_TYPES([socklen_t], [], [], [
#ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
#endif
		])

	AC_MSG_CHECKING([whether `socklen_t' is actually `size_t'])
	AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
#endif
		]], [[
int accept(int, struct sockaddr *, size_t *);
		]])], [socklen_t_is_size_t="yes"], [socklen_t_is_size_t="no"])
	AC_MSG_RESULT([$socklen_t_is_size_t])

	AC_MSG_CHECKING([whether `socklen_t' is actually `int'])
	AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
#endif
		]], [[
int accept(int, struct sockaddr *, int *);
		]])], [socklen_t_is_int="yes"], [socklen_t_is_int="no"])
	AC_MSG_RESULT([$socklen_t_is_int])

	AC_MSG_CHECKING([what socklen_t is an alias for])
	if test "$ac_cv_type_socklen_t" = "yes"; then
		socklen_t_is_actually="socklen_t"
	elif test "$socklen_t_is_size_t" = "yes"; then
		socklen_t_is_actually="size_t"
		AC_DEFINE([socklen_t], [size_t],
			[A simple alias for Unix98's socklen_t])
	elif test "$socklen_t_is_int" = "yes"; then
		socklen_t_is_actually="int"
		AC_DEFINE([socklen_t], [int],
			[A simple alias for Unix98's socklen_t])
	else
		socklen_t_is_actually="bullcrap"
	fi
	AC_MSG_RESULT([$socklen_t_is_actually])

])dnl SXE_CHECK_SOCKLEN_T


AC_DEFUN([_SXE_CHECK_GENERIC_SOCKFUN], [dnl
	## arg1 is the function to crawl for
	pushdef([SOCKFUN], [$1])

	SXE_CHECK_HEADERS([sys/types.h sys/socket.h])
	AC_CHECK_FUNCS(SOCKFUN)
	AC_CHECK_DECLS(SOCKFUN, [], [], [
#ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
#endif
		])
	## libnet is for BeOS
	SXE_CHECK_LIB_FUNCS([nsl socket net], SOCKFUN)

	if test "$ac_cv_func_[]SOCKFUN" = "no" -a \
		"$ac_cv_lib_nsl___[]SOCKFUN" = "yes"; then
		## whether we need to add -lnsl to LIBS
		SOCKET_LIBS="$SOCKET_LIBS -lnsl"
		ac_sxe_func_[]SOCKFUN="yes"

	elif test "$ac_cv_func_[]SOCKFUN" = "no" -a \
		"$ac_cv_lib_socket___[]SOCKFUN" = "yes"; then
		## whether we need to add -lsocket to LIBS
		SOCKET_LIBS="$SOCKET_LIBS -lsocket"
		ac_sxe_func_[]SOCKFUN="yes"

	elif test "$ac_cv_func_[]SOCKFUN" = "no" -a \
		"$ac_cv_lib_net___[]SOCKFUN" = "yes"; then
		## whether we need to add -lsocket to LIBS
		SOCKET_LIBS="$SOCKET_LIBS -lnet"
		ac_sxe_func_[]SOCKFUN="yes"

	elif test "$ac_cv_func_[]SOCKFUN" = "yes"; then
		## nothing to add to LIBS
		ac_sxe_func_[]SOCKFUN="yes"

	else
		ac_sxe_func_[]SOCKFUN="no"

	fi

dnl curl sez:
dnl   At least one system has been identified to require BOTH nsl and socket
dnl   libs at the same time to link properly.
dnl Howbeit, we currently do not provide a check for this very tragedy^Wstrategy

	popdef([SOCKFUN])
])dnl _SXE_CHECK_GENERIC_SOCKFUN


AC_DEFUN([SXE_CHECK_UNIX_DOMAIN_SOCKETS], [dnl
	## defines have_unix and HAVE_UNIX
	have_unix="uncertain"

	SXE_CHECK_HEADERS([sys/types.h sys/socket.h sys/un.h])
	AC_CHECK_TYPE([struct sockaddr_un], [], [], [
#ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#  include <sys/un.h>
#endif
		])

	AC_CHECK_MEMBER([struct sockaddr_un.sun_len], [], [], [
#ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#  include <sys/un.h>
#endif
		])

	## evaluating the results
	if test "$ac_cv_type_struct_sockaddr_un" = "yes" -a \
		"$ac_cv_header_sys_socket_h" = "yes" -a \
		"$ac_cv_header_sys_un_h" = "yes"; then
		have_unix="yes"
	else
		have_unix="no"
	fi

	## legacy
	if test "$ac_cv_member_struct_sockaddr_un_sun_len" = "yes"; then
		AC_DEFINE([HAVE_SOCKADDR_SUN_LEN], [1],
			[Legacy shit, use HAVE_STRUCT_SOCKADDR_UN_SUN_LEN!])
	fi
])dnl SXE_CHECK_UNIX_DOMAIN_SOCKETS

AC_DEFUN([SXE_CHECK_GETHOSTBYNAME], [_SXE_CHECK_GENERIC_SOCKFUN([gethostbyname])])
AC_DEFUN([SXE_CHECK_CONNECT], [_SXE_CHECK_GENERIC_SOCKFUN([connect])])
AC_DEFUN([SXE_CHECK_ACCEPT], [_SXE_CHECK_GENERIC_SOCKFUN([accept])])
AC_DEFUN([SXE_CHECK_SOCKET], [_SXE_CHECK_GENERIC_SOCKFUN([socket])])
AC_DEFUN([SXE_CHECK_INET_NTOA], [_SXE_CHECK_GENERIC_SOCKFUN([inet_ntoa])])
AC_DEFUN([SXE_CHECK_GETHOSTNAME], [_SXE_CHECK_GENERIC_SOCKFUN([gethostname])])
AC_DEFUN([SXE_CHECK_GETNAMEINFO], [_SXE_CHECK_GENERIC_SOCKFUN([getnameinfo])])
AC_DEFUN([SXE_CHECK_GETADDRINFO], [_SXE_CHECK_GENERIC_SOCKFUN([getaddrinfo])])

AC_DEFUN([SXE_CHECK_SOCKET_IPV4], [dnl
	SXE_CHECK_HEADERS([sys/types.h sys/socket.h])
	AC_MSG_CHECKING([for IPv4])
	AC_RUN_IFELSE([AC_LANG_SOURCE([[ /* is AF_INET available? */
#if defined HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#if defined HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
#endif
main()
{
	if (socket(AF_INET, SOCK_STREAM, 0) < 0)
		exit(1);
	else
		exit(0);
}
]])], [have_ipv4=yes], [have_ipv4=no], [have_ipv4=no])

	AC_MSG_RESULT([$have_ipv4])
])dnl SXE_CHECK_SOCKET_IPV4

AC_DEFUN([SXE_CHECK_SOCKET_IPV6], [dnl
	SXE_CHECK_HEADERS([sys/types.h sys/socket.h])
	AC_MSG_CHECKING([for IPv6])
	AC_RUN_IFELSE([AC_LANG_SOURCE([[ /* is AF_INET6 available? */
#if defined HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#if defined HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
#endif
main()
{
	if (socket(AF_INET6, SOCK_STREAM, 0) < 0)
		exit(1);
	else
		exit(0);
}
]])], [have_ipv6=yes], [have_ipv6=no], [have_ipv6=no])

	AC_MSG_RESULT([$have_ipv6])
])dnl SXE_CHECK_SOCKET_IPV6


dnl CURL_CHECK_NONBLOCKING_SOCKET
dnl -------------------------------------------------
dnl Check for how to set a socket to non-blocking state. There seems to exist
dnl four known different ways, with the one used almost everywhere being POSIX
dnl and XPG3, while the other different ways for different systems (old BSD,
dnl Windows and Amiga).
dnl
dnl There are two known platforms (AIX 3.x and SunOS 4.1.x) where the
dnl O_NONBLOCK define is found but does not work. This condition is attempted
dnl to get caught in this script by using an excessive number of #ifdefs...
dnl
AC_DEFUN([SXE_CHECK_NONBLOCKING_SOCKET], [dnl

	SXE_CHECK_NONBLOCKING_SOCKET_O_NONBLOCK([nonblock])
	SXE_CHECK_NONBLOCKING_SOCKET_FIONBIO([nonblock])
	SXE_CHECK_NONBLOCKING_SOCKET_IOCTLSOCKET([nonblock])
	SXE_CHECK_NONBLOCKING_SOCKET_SO_NONBLOCK([nonblock])

	AC_MSG_CHECKING([for preferred non-blocking sockets style])
	if test "$sxe_have_nonblocking_socket_O_NONBLOCK" = "yes"; then
		nonblock="O_NONBLOCK"
		AC_DEFINE([WITH_NONBLOCKING_SOCKET_O_NONBLOCK], [1],
			[Whether to use fnctl() with O_NONBLOCK])
	elif test "$sxe_have_nonblocking_socket_FIONBIO" = "yes"; then
		nonblock="FIONBIO"
		AC_DEFINE([WITH_NONBLOCKING_SOCKET_O_NONBLOCK], [1],
			[Whether to use ioctl() with FIONBIO])
	elif test "$sxe_have_nonblocking_socket_IoctlSocket" = "yes"; then
		nonblock="IoctlSocket"
		AC_DEFINE([WITH_NONBLOCKING_SOCKET_IOCTLSOCKET], [1],
			[Whether to use IoctlSocket() with FIONBIO])
	elif test "$sxe_have_nonblocking_socket_SO_NONBLOCK" = "yes"; then
		nonblock="SO_NONBLOCK"
		AC_DEFINE([WITH_NONBLOCKING_SOCKET_SO_NONBLOCK], [1],
			[Whether to use setsockopt() with SO_NONBLOCK])
	else
		nonblock="zilch"
		AC_DEFINE([WITH_NONBLOCKING_SOCKET_ZILCH], [1],
			[Whether not to use a non-blocking socket])
	fi
	AC_MSG_RESULT([$nonblock])
])dnl SXE_CHECK_NONBLOCKING_SOCKET

AC_DEFUN([SXE_CHECK_NONBLOCKING_SOCKET_O_NONBLOCK], [dnl
	## arg1 - varname to put the socket implementation into
	pushdef([VARNAME], [$1])

	SXE_CHECK_HEADERS([sys/types.h unistd.h fcntl.h])
	AC_CHECK_FUNCS([fcntl])
	AC_CHECK_DECLS([fcntl], [], [], [
#if defined HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if defined HAVE_UNISTD_H
#include <unistd.h>
#endif
#if defined HAVE_FCNTL_H
#include <fcntl.h>
#endif
		])
	AC_MSG_CHECKING([whether fnctl() with O_NONBLOCK works])
	AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
/* headers for O_NONBLOCK test */
#if defined HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if defined HAVE_UNISTD_H
#include <unistd.h>
#endif
#if defined HAVE_FCNTL_H
#include <fcntl.h>
#endif
		]], [[
/* try to compile O_NONBLOCK */
#if defined(sun) || defined(__sun__) || defined(__SUNPRO_C) || defined(__SUNPRO_CC)
# if defined(__SVR4) || defined(__srv4__)
#  define PLATFORM_SOLARIS
# else
#  define PLATFORM_SUNOS4
# endif
#endif
#if (defined(_AIX) || defined(__xlC__)) && !defined(_AIX4)
# define PLATFORM_AIX_V3
#endif

#if defined(PLATFORM_SUNOS4) || defined(PLATFORM_AIX_V3) || defined(__BEOS__)
#error "O_NONBLOCK does not work on this platform"
#endif
int socket;
int flags = fcntl(socket, F_SETFL, flags | O_NONBLOCK);
		]])],
		[sxe_have_nonblocking_socket_O_NONBLOCK="yes"],
		[sxe_have_nonblocking_socket_O_NONBLOCK="no"])
	AC_MSG_RESULT([$sxe_have_nonblocking_socket_O_NONBLOCK])

	_SXE_EVALUATE_NONBLOCKING_SOCKET(VARNAME, [O_NONBLOCK])
	popdef([VARNAME])
])dnl SXE_CHECK_NONBLOCKING_SOCKET_O_NONBLOCK

AC_DEFUN([SXE_CHECK_NONBLOCKING_SOCKET_FIONBIO], [dnl
	## arg1 - varname to put the socket implementation into
	pushdef([VARNAME], [$1])

	SXE_CHECK_HEADERS([unistd.h stropts.h])
	AC_CHECK_FUNCS([ioctl])
	AC_CHECK_DECLS([ioctl], [], [], [
#if defined HAVE_UNISTD_H
#include <unistd.h>
#endif
#if defined HAVE_STROPTS_H
#include <stropts.h>
#endif
		])
	AC_MSG_CHECKING([whether ioctl() with FIONBIO works])
	AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
/* headers for FIONBIO test */
#if defined HAVE_UNISTD_H
#include <unistd.h>
#endif
#if defined HAVE_STROPTS_H
#include <stropts.h>
#endif
		]],[[
/* FIONBIO source test (old-style unix) */
int socket;
int flags = ioctl(socket, FIONBIO, &flags);
		]])],
		[sxe_have_nonblocking_socket_FIONBIO="yes"],
		[sxe_have_nonblocking_socket_FIONBIO="no"])
	AC_MSG_RESULT([$sxe_have_nonblocking_socket_FIONBIO])

	_SXE_EVALUATE_NONBLOCKING_SOCKET(VARNAME, [FIONBIO])
	popdef([VARNAME])
])dnl SXE_CHECK_NONBLOCKING_SOCKET_FIONBIO

AC_DEFUN([SXE_CHECK_NONBLOCKING_SOCKET_IOCTLSOCKET], [dnl
	## arg1 - varname to put the socket implementation into
	pushdef([VARNAME], [$1])

	SXE_CHECK_HEADERS([sys/ioctl.h])
	AC_CHECK_FUNCS([IoctlSocket])
	AC_CHECK_DECLS([IoctlSocket], [], [], [
#if defined HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
		])
	AC_MSG_CHECKING([whether IoctlSocket() with FIONBIO works])
	AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
/* headers for IoctlSocket test (Amiga?) */
#if defined HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
		]], [[
/* IoctlSocket source code */
int socket;
int flags = IoctlSocket(socket, FIONBIO, (long)1);
		]])],
		[sxe_have_nonblocking_socket_IoctlSocket="yes"],
		[sxe_have_nonblocking_socket_IoctlSocket="no"])
	AC_MSG_RESULT([$sxe_have_nonblocking_socket_IoctlSocket])

	_SXE_EVALUATE_NONBLOCKING_SOCKET(VARNAME, [IoctlSocket])
	popdef([VARNAME])
])dnl SXE_CHECK_NONBLOCKING_SOCKET_IOCTLSOCKET

AC_DEFUN([SXE_CHECK_NONBLOCKING_SOCKET_SO_NONBLOCK], [dnl
	## arg1 - varname to put the socket implementation into
	pushdef([VARNAME], [$1])

	SXE_CHECK_HEADERS([socket.h])
	AC_CHECK_FUNCS([setsockopt])
	AC_CHECK_DECLS([setsockopt], [], [], [
#if defined HAVE_SOCKET_H
#include <socket.h>
#endif
		])
	AC_MSG_CHECKING([whether setsockopt() with SO_NONBLOCK works])
	AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
/* headers for SO_NONBLOCK test (BeOS) */
#if defined HAVE_SOCKET_H
#include <socket.h>
#endif
		]], [[
/* SO_NONBLOCK source code */
long b = 1;
int socket;
int flags = setsockopt(socket, SOL_SOCKET, SO_NONBLOCK, &b, sizeof(b));
		]])],
		[sxe_have_nonblocking_socket_SO_NONBLOCK="yes"],
		[sxe_have_nonblocking_socket_SO_NONBLOCK="no"])
	AC_MSG_RESULT([$sxe_have_nonblocking_socket_SO_NONBLOCK])

	_SXE_EVALUATE_NONBLOCKING_SOCKET(VARNAME, [SO_NONBLOCK])
	popdef([VARNAME])
])dnl SXE_CHECK_NONBLOCKING_SOCKET_SO_NONBLOCK

AC_DEFUN([_SXE_EVALUATE_NONBLOCKING_SOCKET], [dnl
	pushdef([VARNAME], [$1])
	pushdef([TSTNAME], [$2])
	if test "$sxe_have_nonblocking_socket_[]TSTNAME[]" = "yes"; then
		AC_DEFINE([HAVE_NONBLOCKING_SOCKET]TSTNAME, [1],
			[use ]TSTNAME[ for non-blocking sockets])
		if test -n "[]VARNAME[]"; then
			VARNAME="[]TSTNAME[]"
		fi
	else
		:
	fi
	popdef([VARNAME])
	popdef([TSTNAME])
])dnl _SXE_EVALUATE_NONBLOCKING_SOCKET


AC_DEFUN([SXE_CHECK_TCPUDP_SOCKETS], [dnl
	## defines have_sockets and have_tcpudp
	## and HAVE_SOCKETS and HAVE_TCPUDP
	have_sockets="uncertain"
	have_tcpudp="uncertain"

	SXE_CHECK_HEADERS([netdb.h arpa/inet.h netinet/in.h])
	SXE_CHECK_GETHOSTBYNAME
	SXE_CHECK_GETHOSTNAME
	SXE_CHECK_GETNAMEINFO
	SXE_CHECK_GETADDRINFO
	SXE_CHECK_CONNECT
	SXE_CHECK_ACCEPT
	SXE_CHECK_SOCKET
	SXE_CHECK_INET_NTOA

	SXE_CHECK_SOCKET_IPV4
	SXE_CHECK_SOCKET_IPV6
	SXE_CHECK_NONBLOCKING_SOCKET

	## evaluating the results
	if test "$ac_cv_header_netinet_in_h" = "yes" -a \
		"$ac_cv_header_arpa_inet_h" = "yes" -a \
		"$ac_cv_header_netdb_h" = "yes"; then
		if test "$ac_sxe_func_socket" = "yes" -a \
			"$ac_sxe_func_inet_ntoa" = "yes" -a \
			"$ac_sxe_func_gethostbyname" = "yes" -a \
			"$ac_sxe_func_gethostname" = "yes" -a \
			"$ac_sxe_func_getnameinfo" = "yes" -a \
			"$ac_sxe_func_getaddrinfo" = "yes"; then
			have_sockets="yes"
			have_tcpudp="yes"
		else
			have_sockets="no"
			have_tcpudp="no"
		fi
	else
		have_sockets="no"
		have_tcpudp="no"
	fi
])dnl SXE_CHECK_TCPUDP_SOCKETS

AC_DEFUN([SXE_CHECK_SYSVIPC_MESSAGES], [dnl
	## defines have_sysvipc and HAVE_SYSVIPC
	have_sysvipc="uncertain"

	SXE_CHECK_HEADERS([sys/ipc.h sys/msg.h])
	AC_CHECK_FUNCS([msgget])

	## evaluating the results
	if test "$ac_cv_func_msgget" = "yes" -a \
		"$ac_cv_header_sys_ipc_h" = "yes" -o \
		"$ac_cv_func_msgget" = "yes" -a \
		"$ac_cv_header_sys_msg_h" = "yes"; then
		have_sysvipc="yes"
	else
		have_sysvipc="no"
	fi
])dnl SXE_CHECK_SYSVIPC_MESSAGES

AC_DEFUN([SXE_CHECK_MULTICAST], [dnl
	## defines have_multicast and HAVE_MULTICAST
	have_multicast="uncertain"

	AC_CHECK_TYPE([struct ip_mreq], [], [], [
#ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#ifdef HAVE_NETINET_IN_H
#  include <netinet/in.h>
#endif
		])

	## evaluating the results
	have_multicast="no"
	## ... now test
	if test "$ac_cv_type_struct_ip_mreq" = "yes"; then
		have_multicast="yes"
	else
		have_multicast="no"
	fi
])dnl SXE_CHECK_MULTICAST

AC_DEFUN([SXE_CHECK_SOCKETS], [dnl
	AC_DEFUN([SXE_CHECK_SOCKETS], [])

	dnl Check for Internet sockets.
	SXE_CHECK_HEADERS([sys/types.h sys/socket.h])
	AC_HEADER_RESOLV

	## check for Unix98 socklen_t
	SXE_CHECK_SOCKLEN_T

	## some real checks
	SXE_CHECK_UNIX_DOMAIN_SOCKETS
	SXE_CHECK_TCPUDP_SOCKETS
	SXE_CHECK_SYSVIPC_MESSAGES
	SXE_CHECK_MULTICAST
])dnl SXE_CHECK_SOCKETS

dnl This isn't all that clever, it doesn't really properly test if
dnl -lbind is needed, just if it is present and defines inet_ntoa().
dnl There is also nothing preventing `-lbind' being added to $LDFLAGS
dnl _every_ time this macro is called. For the moment, it is only called
dnl once, so `-lbind' is only added once (or less) --SY.
AC_DEFUN([SXE_CHECK_LIBBIND], [dnl
	AC_MSG_CHECKING([whether libbind is needed for inet_*()])
	AC_MSG_RESULT([])

	AC_CHECK_LIB([bind], [inet_ntoa])
	if test "$ac_cv_lib_bind_inet_ntoa" = "yes"; then
		SXE_APPEND([-lbind], [LDFLAGS])
	fi
])dnl SXE_CHECK_LIBBIND

dnl sxe-sockets.m4 ends here
