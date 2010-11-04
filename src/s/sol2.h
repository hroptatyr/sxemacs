/* Synched up with: Completely divergent from FSF. */
#define SOLARIS2 1
#define POSIX 1

#ifndef USG
#define USG
#endif

#ifndef USG5_4
#define USG5_4
#endif

/* Fix understandable GCC lossage on Solaris 2.6 */
#if defined(__GNUC__) && OS_RELEASE >= 506 && !defined(NOT_C_CODE)

#if NEED_GNUC_VA_LIST
/* GCC va_list munging is a little messed up */
#define __GNUC_VA_LIST
#define _VA_LIST_
#define _VA_LIST va_list
typedef void *__gnuc_va_list;
typedef __gnuc_va_list va_list;
#endif

/* Missing prototypes for functions added in Solaris 2.6 */
#include <sys/types.h>
struct msghdr;
struct sockaddr;
extern int __xnet_bind(int, const struct sockaddr *, size_t);
extern int __xnet_listen(int, int);
extern int __xnet_connect(int, const struct sockaddr *, size_t);
extern ssize_t __xnet_recvmsg(int, struct msghdr *, int);
extern ssize_t __xnet_sendmsg(int, const struct msghdr *, int);
extern ssize_t __xnet_sendto(int, const void *, size_t, int,
			     const struct sockaddr *, size_t);
extern int __xnet_socket(int, int, int);
extern int __xnet_socketpair(int, int, int, int *);
extern int __xnet_getsockopt(int, int, int, void *, size_t *);
#endif				/* GCC && >= Solaris 2.6 && C code */

#include "usg5-4-2.h"		/* XEmacs change from 5-4 to 5-4-2 */
#undef PC			/* Defined in x86 /usr/include/sys/reg.h */

/* SIGIO seems to be working under Solaris and it makes ^G work better... */
#undef BROKEN_SIGIO

/* eggert@twinsun.com said these work in Solaris.
   Perhaps they work in all kinds of SVR4, but this is more conservative.  */
#undef BROKEN_TIOCGETC
#undef BROKEN_TIOCGWINSZ

#ifdef NOT_C_CODE
#define ORDINARY_LINK
/* XEmacs change -- some Motif packages need -lgen to get regex and regcmp */

#undef LIBS_SYSTEM
#define LIBS_SYSTEM "-lsocket -lnsl -lelf -lgen -ldl"

/* SYSTEM_MALLOC must be defined if dbx/RTC is going to be used.  dbx/RTC does
   not work with a static definition of malloc(). */
/* We want to be able to test out ralloc.c. */
/* #define SYSTEM_MALLOC */

/* XEmacs: there used to be a special definition of
   PTY_TTY_NAME_SPRINTF here that was identical to the
   other SYSV R4 definitions except that it didn't
   block SIGCHLD around the call to grantpt().  This
   is *not* in 19.29 and is almost certainly incorrect.
 */

#undef UNEXEC

/*
 * everythign is pdump now. --SY
 * #if OS_RELEASE < 506
 * #define UNEXEC "unexsol2.o"
 * #else
 * #define UNEXEC "unexsol2-6.o"
 * #endif
 */

#else				/* C_CODE */

#if OS_RELEASE <= 503
/* Solaris 2.3 has a bug in XListFontsWithInfo.  */
#define BROKEN_XLISTFONTSWITHINFO
#endif

/* XEmacs addition: Raymond Toy says XEmacs completely misses SIGCHLD
   when compiled with GCC 2.7.0 (but not, apparently, with SunPro C?),
   X11R6, and Solaris 2.4.

   Someone else submitted a simple test program that duplicates this
   behavior, and says it has something to do with the fact that X11R6
   links with the threads library. */

#if X11_RELEASE == 6
#define BROKEN_SIGCHLD
#endif

#if OS_RELEASE < 505

#if __STDC__ == 1 && defined(__SUNPRO_C)
#define _POSIX_C_SOURCE 1
#include <setjmp.h>
#undef _POSIX_C_SOURCE
#endif				/* cc -Xc */

/* Missing prototype, added in Solaris 2.5 */
extern void *__builtin_alloca(size_t);
#endif				/* before SunOS 5.5 */

#if OS_RELEASE == 505
/* The following functions were added in Solaris 2.5,
   but they forgot to add prototypes to the system header files. */
int getpagesize(void);
long random(void);
void srandom(unsigned int seed);
int usleep(unsigned int useconds);
#endif				/* SunOS 5.5 */

/* 2.5 now has `random' back in libc but we don't want to use it. */
#if OS_RELEASE >= 505
#undef HAVE_RANDOM
/* Apparently not necessary here, and it causes 10% CPU chewage. */
#undef BROKEN_SIGCHLD
#endif				/* >= SunOS 5.5 */

#if OS_RELEASE < 506
/* Missing prototypes, added in Solaris 2.6 */
struct timeval;
int utimes(char *file, struct timeval *tvp);
int gethostname(char *name, int namelen);
#endif				/* before SunOS 5.6 */

#include <sys/utsname.h>	/* warning: macro redefined: SYS_NMLN */

/* XEmacs: Solaris has sigsetjmp but using it leads to core dumps at
   least under 2.4 */
#undef _setjmp
#define _setjmp setjmp

#endif				/* C_CODE */
