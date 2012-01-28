/* Utility definitions for C code of SXEmacs

   Copyright (C) 1985-1987, 1992-1995 Free Software Foundation, Inc.
   Copyright (C) 1993-1996 Richard Mlynarik.
   Copyright (C) 1995, 1996, 2000 Ben Wing.
   Copyright (C) 2004 Steve Youngs.
   Copyright (C) 2011 Nelson Ferreira.

This file is part of SXEmacs

SXEmacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SXEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */

/* NOT synched with FSF */
#ifndef INCLUDED_sxe_utils_h_
#define INCLUDED_sxe_utils_h_

/* ------------------------ include files ------------------- */

/* We include the following generally useful header files so that you
   don't have to worry about prototypes when using the standard C
   library functions and macros.  These files shouldn't be excessively
   large so they shouldn't cause that much of a slowdown. */

#include <stdlib.h>
#include <string.h>		/* primarily for memcpy, etc. */
#include <stdio.h>		/* NULL, etc. */
#include <ctype.h>
#include <stdarg.h>
#include <stddef.h>		/* offsetof */
#include <sys/types.h>
#include <limits.h>
#if defined HAVE_STDBOOL_H
# include <stdbool.h>
#endif


/* goodies */
#ifdef SXE_UNUSED
#elif defined(__GNUC__)
# define SXE_UNUSED(x) UNUSED_ ## x __attribute__((unused))
#elif defined(__LCLINT__)
# define SXE_UNUSED(x) /*@unused@*/ x
#else
# define SXE_UNUSED(x) x
#endif
#ifdef UNUSED
#undef UNUSED
#define UNUSED(x) SXE_UNUSED(x)
#endif

#ifdef WEAK_EXTERN
#elif defined(__GNUC__)
# define WEAK_EXTERN	extern __attribute__((weak))
#else
# error "Grrr, bloody 'ell, can't figure out how to create weak symbols"
#endif

#ifdef WEAK
#elif defined(__GNUC__)
# define WEAK		__attribute__((weak))
#else
# error "Grrr, bloody 'ell, can't figure out how to create weak symbols"
#endif


#ifdef LIKELY
#else
#define LIKELY(_x)	__builtin_expect(!!(_x), 1)
#endif
#ifdef UNLIKELY
#else
#define UNLIKELY(_x)	__builtin_expect(!!(_x), 0)
#endif


#define extern_inline	static inline

#ifdef ALL_DEBUG_FLAGS
#undef SXE_DEBUG_FLAG
#define SXE_DEBUG_FLAG
#endif

#define __SXE_DEBUG__(args...)		fprintf(stderr, "SXE " args)
#ifndef SXE_DEBUG_FLAG
#define SXE_DEBUG(args...)
#else
#define SXE_DEBUG(args...)		__SXE_DEBUG__(args)
#endif
#define SXE_DEBUG_PT(args...)		SXE_DEBUG("[pthread]: " args)
#define SXE_CRITICAL(args...)		__SXE_DEBUG__("CRITICAL: " args)



/* We define assert iff USE_ASSERTIONS or DEBUG_SXEMACS is defined.
   Otherwise we define it to be empty.  Quantify has shown that the
   time the assert checks take is measurable so let's not include them
   in production binaries. */

#ifdef USE_ASSERTIONS
/* Highly dubious kludge */
/*   (thanks, Jamie, I feel better now -- ben) */
void assert_failed(const char *, int, const char *);
# define abort() (assert_failed (__FILE__, __LINE__, "abort()"))
# define assert(x) ((x) ? (void) 0 : assert_failed (__FILE__, __LINE__, #x))
#else
# ifdef DEBUG_SXEMACS
#  define assert(x) ((x) ? (void) 0 : (void) abort ())
# else
#  define assert(x)
# endif
#endif

/* from c.ac */
#ifndef BITS_PER_CHAR
#define BITS_PER_CHAR 8
#endif
#define SXE_SHORTBITS (SIZEOF_SHORT * BITS_PER_CHAR)
#define SXE_INTBITS (SIZEOF_INT * BITS_PER_CHAR)
#define SXE_LONGBITS (SIZEOF_LONG * BITS_PER_CHAR)
#define SXE_LONG_LONG_BITS (SIZEOF_LONG_LONG_INT * BITS_PER_CHAR)
#define SXE_VOID_P_BITS (SIZEOF_VOID_P * BITS_PER_CHAR)

/* Also define min() and max(). (Some compilers put them in strange
   places that won't be referenced by the above include files, such
   as 'macros.h' under Solaris.) */

#ifndef min
#define min(a,b) (((a) <= (b)) ? (a) : (b))
#endif
#ifndef max
#define max(a,b) (((a) > (b)) ? (a) : (b))
#endif


/* generally useful */
#define countof(x) ((int) (sizeof(x)/sizeof((x)[0])))
#define xnew(type) ((type *) xmalloc (sizeof (type)))
#define xnew_atomic(type) ((type *) xmalloc_atomic (sizeof (type)))
#define xnew_array(type, len) ((type *) xmalloc ((len) * sizeof (type)))
#define xnew_atomic_array(type, len)			\
	((type*)xmalloc_atomic((len) * sizeof(type)))
#define xnew_and_zero(type) ((type *) xmalloc_and_zero (sizeof (type)))
#define xzero(lvalue) ((void) memset (&(lvalue), '\0', sizeof (lvalue)))
#define xnew_array_and_zero(type, len)				\
	((type*)xmalloc_and_zero ((len) * sizeof (type)))
#define xrealloc_array(ptr, type, len)				\
	((void) (ptr = (type *) xrealloc (ptr, (len) * sizeof (type))))
#define XREALLOC_ARRAY		xrealloc_array
#define alloca_array(type, len) ((type *) alloca ((len) * sizeof (type)))

#if !defined HAVE_DECL_STRDUP
extern char *strdup(const char *s);
#endif	/* HAVE_DECL_STRDUP */


#ifndef PRINTF_ARGS
# if defined (__GNUC__) && (__GNUC__ >= 2)
#  define PRINTF_ARGS(string_index,first_to_check) \
	  __attribute__ ((format (printf, string_index, first_to_check)))
# else
#  define PRINTF_ARGS(string_index,first_to_check)
# endif				/* GNUC */
#endif

#ifndef DOESNT_RETURN
# if defined __GNUC__
#  if ((__GNUC__ > 2) || (__GNUC__ == 2) && (__GNUC_MINOR__ >= 5))
#   define DOESNT_RETURN void
#   define DECLARE_DOESNT_RETURN(decl) \
	   extern void decl __attribute__ ((noreturn))
#   define DECLARE_DOESNT_RETURN_GCC_ATTRIBUTE_SYNTAX_SUCKS(decl,str,idx) \
     /* Should be able to state multiple independent __attribute__s, but  \
	the losing syntax doesn't work that way, and screws losing cpp */ \
	   extern void decl \
		  __attribute__ ((noreturn, format (printf, str, idx)))
#  else
#   define DOESNT_RETURN void volatile
#   define DECLARE_DOESNT_RETURN(decl) extern void volatile decl
#   define DECLARE_DOESNT_RETURN_GCC_ATTRIBUTE_SYNTAX_SUCKS(decl,str,idx) \
	   extern void volatile decl PRINTF_ARGS(str,idx)
#  endif			/* GNUC 2.5 */
# else
#  define DOESNT_RETURN void
#  define DECLARE_DOESNT_RETURN(decl) extern void decl
#  define DECLARE_DOESNT_RETURN_GCC_ATTRIBUTE_SYNTAX_SUCKS(decl,str,idx) \
	  extern void decl PRINTF_ARGS(str,idx)
# endif				/* GNUC */
#endif


/* No type has a greater alignment requirement than max_align_t.
   (except perhaps for types we don't use, like long double) */
typedef union {
	struct {
		long l;
	} l;
	struct {
		void *p;
	} p;
	struct {
		void (*f) (void);
	} f;
	struct {
		double d;
	} d;
} max_align_t;

#ifndef ALIGNOF
# if defined (__GNUC__) && (__GNUC__ >= 2)
/* gcc has an extension that gives us exactly what we want. */
#  define ALIGNOF(type) __alignof__ (type)
# elif ! defined (__cplusplus)
/* The following is mostly portable, except that:
   - it doesn't work for inside out declarations like void (*) (void).
     (so just call ALIGNOF with a typedef'ed name)
   - it doesn't work with C++.  The C++ committee has decided,
     in its infinite wisdom, that:
     "Types must be declared in declarations, not in expressions." */
#  define ALIGNOF(type) offsetof (struct { char c; type member; }, member)
# else
/* C++ is annoying, but it has a big bag of tricks.
   The following doesn't have the "inside out" declaration bug C does. */
template < typename T > struct alignment_trick {
	char c;
	T member;
};
#  define ALIGNOF(type) offsetof (alignment_trick<type>, member)
# endif
#endif				/* ALIGNOF */

#define ALIGN_SIZE(len, unit) \
  ((((len) + (unit) - 1) / (unit)) * (unit))

/* #### Yuck, this is kind of evil */
#define ALIGN_PTR(ptr, unit) \
  ((void *) ALIGN_SIZE ((size_t) (ptr), unit))

#ifndef DO_NOTHING
#define DO_NOTHING do {} while (0)
#endif

#ifndef DECLARE_NOTHING
#define DECLARE_NOTHING struct nosuchstruct
#endif


/************************************************************************/
/*				  Memory				*/
/************************************************************************/

#ifdef ALL_DEBUG_FLAGS
#undef GC_DEBUG_FLAG
#define GC_DEBUG_FLAG
#endif

#if !defined GC_DEBUG_FLAG
# define SXE_DEBUG_GC(args...)
#else
# define SXE_DEBUG_GC(args...)		__SXE_DEBUG__("[gc] " args)
#endif
#define SXE_DEBUG_GC_PT(args...)	SXE_DEBUG_GC("[pthread]: " args)
#define SXE_CRITICAL_GC(args...)	__SXE_DEBUG__("[gc] CRITICAL: " args)

void malloc_warning(const char *);

#if defined HAVE_BDWGC && defined EF_USE_BDWGC
# if defined HAVE_THREADS
#  if !defined GC_PTHREADS
#   define GC_PTHREADS	1
#  endif  /* !GC_PTHREADS */
#  if !defined GC_THREADS
#   define GC_THREADS	1
#  endif  /* !GC_THREADS */
# endif	 /* HAVE_THREADS */

# undef GC_DEBUG
# if defined GC_DEBUG_FLAG
#  define GC_DEBUG	1
# endif	 /* GC_DEBUG_FLAG */
# if defined HAVE_GC_GC_H
#  include <gc/gc.h>
# elif defined HAVE_GC_H
#  include <gc.h>
# else
#  error "Take less of those pills!"
# endif

# if defined GC_DEBUG_FLAG
#  define zmalloc		GC_MALLOC_IGNORE_OFF_PAGE
#  define zcalloc(n, m)	GC_MALLOC((n)*(m))
#  define zmalloc_atomic	GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE
#  define zmalloc_and_zero	GC_MALLOC
#  define zrealloc		GC_REALLOC
#  define zstrdup		GC_STRDUP
#  undef zfree
#  define zfree(x)		GC_FREE(x)
# else	/* !GC_DEBUG_FLAG */
#  define zmalloc		GC_malloc_ignore_off_page
#  define zcalloc(n, m)		GC_malloc((n)*(m))
#  define zmalloc_atomic	GC_malloc_atomic_ignore_off_page
#  define zmalloc_and_zero	GC_malloc
#  define zrealloc		GC_realloc
#  define zstrdup		GC_strdup
#  undef zfree
#  define zfree(x)
# endif	/* GC_DEBUG_FLAG */

#else  /* !BDWGC */
#define zmalloc		F&^!
#define zcalloc		F&^!
#define zmalloc_atomic	F&^!
#define zmalloc_and_zero	F&^!
#define zrealloc	F&^!
#define zstrdrup	F&^!
#endif	/* BDWGC */

/* also define libc mem funs */
#define ymalloc		malloc
#define ycalloc(n, m)	calloc(n, m)
#define ymalloc_atomic(n)	ycalloc(1, n)
#define ymalloc_and_zero(x)	ycalloc(1, x)
#define yrealloc	realloc
#define ystrdup		strdup
#define yfree(x)	free(x)
/* and their convenience companions */
#define ynew(type)		((type*)ymalloc(sizeof(type)))
#define ynew_array(type, len)	((type*)ymalloc((len) * sizeof(type)))
#define ynew_and_zero(type)	((type*)ymalloc_and_zero(sizeof(type)))
#define ynew_array_and_zero(type, len)			\
	((type*)ymalloc_and_zero((len) * sizeof(type)))
#define YREALLOC_ARRAY(ptr, type, len)					\
	((void)(ptr = (type *)yrealloc(ptr, (len) * sizeof (type))))

#if defined HAVE_BDWGC && defined EF_USE_BDWGC
/* and now the x* series */
# define xmalloc		zmalloc
# define xcalloc		zcalloc
# define xmalloc_atomic		zmalloc_atomic
# define xmalloc_and_zero	zmalloc_and_zero
# define xrealloc		zrealloc
# define xstrdup		zstrdup
# if defined ERROR_CHECK_MALLOC
#  define xfree(args...)	zfree(args)
# else	/* !ERROR_CHECK_MALLOC */
#  define xfree(args...)
# endif	 /* ERROR_CHECK_MALLOC */

#else  /* !BDWGC */
void *xmalloc(size_t size);
void *xmalloc_atomic(size_t size);
void *xmalloc_and_zero(size_t size);
void *xrealloc(void *, size_t size);
char *xstrdup(const char *);
# if defined ERROR_CHECK_MALLOC
#  if SIZEOF_VOID_P == 4
#   define xfree(lvalue)					\
	do {							\
		void *volatile *xfree_ptr =			\
			(void *volatile*)			\
			((volatile void*)&(lvalue));		\
		assert(*xfree_ptr != (void*)0xB00BB4BE);	\
		yfree(*xfree_ptr);				\
		*xfree_ptr = (void*)0xB00BB4BE;			\
	} while (0)
#  elif SIZEOF_VOID_P == 8
#   define xfree(lvalue)							\
	do {								\
		void *volatile *xfree_ptr =				\
			(void *volatile*)				\
			((volatile void*)&(lvalue));			\
		assert(*xfree_ptr != (void*)0xCAFEBABEDEADBEEF);	\
		yfree(*xfree_ptr);					\
		*xfree_ptr = (void*)0xCAFEBABEDEADBEEF;			\
	} while (0)
#  else  /* huh? */
#   error "Strange-arse system detected.  Watch a movie, it\'s more fun!"
#  endif
# else	/* !ERROR_CHECK_MALLOC */
#  define xfree(args...)	yfree(args)
# endif	 /* ERROR_CHECK_MALLOC */
#endif	/* BDWGC */

#endif
