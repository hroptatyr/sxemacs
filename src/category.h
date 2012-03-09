/*** category.h -- categorial view on objects, this is NOT OO
 *
 * Copyright (C) 2008 Sebastian Freundt
 *
 * Author:  Sebastian Freundt <hroptatyr@sxemacs.org>
 *
 * This file is part of SXEmacs.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the author nor the names of any contributors
 *    may be used to endorse or promote products derived from this
 *    software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 ***/

#ifndef INCLUDED_category_h_
#define INCLUDED_category_h_

#include "lrecord.h"

enum cat_morphism_kind_e {
	cat_mk_none = -1,
	cat_mk_lc = 0,		/* whether or not lcrecord header is used */
	cat_mk_seq,
	cat_mk_set,
	cat_mk_aseq,		/* associative seqs */
	cat_mk_aset,		/* associative sets aka dicts */
	number_of_cat_morphism_kinds,
};

typedef struct cat_morphism_s *cat_morphism_t;
typedef enum cat_morphism_kind_e cat_morphism_kind_t;

extern_inline bool __bit_set_p(int number, char bit);
extern_inline char __nbits_right_of(int number, char bit);
extern cat_morphism_t morphisms;


/* a double lookup approach
 * this is the complete list of morphisms (views) that an object can have */
struct cat_morphism_s {
	void *seq_impl;
	void *set_impl;
	void *aseq_impl;
	void *aset_impl;
};


struct __cat_morphism_lrec_s {
	struct lrecord_header lh;
	void *foo;
};

struct __cat_morphism_lcrec_s {
	struct lcrecord_header lch;
	void *foo;
};

/* inlines */
extern_inline bool
__bit_set_p(int number, char bit)
{
/* return whether bit BIT is set in NUMBER
 * (10010101, 5) => t
 * the right most bit has bit number 0 */
	return ((1<<bit) & number) != 0;
}

#if defined HAVE_ASM_EAX && defined HAVE_ASM_CL &&	\
	defined HAVE_ASM_ADCB && defined HAVE_ASM_SARL
extern_inline char
__nbits_right_of(int number, char bit)
{
/* return the number of set bits in NUMBER right of BIT
 * (10010101, 4) => 2
 * bit count starts at 0 and on the right */
	/* we just stuff `BIT' into CNT which goes to cx */
	register char cnt = bit;

	__asm__ volatile (
#if !defined HAVE_ASM_RETVAL_IN_EBX
		"	pushl %%ebx		/* save EBX ([nmsk]) */\n"
#endif	/* HAVE_ASM_RETVAL_IN_EBX */
		/* initialise AX directly */
		"	movl $1, %%eax\n"
		"	sall %[cnt], %%eax	/* CL has value of BIT */\n"
		"	subl $1, %%eax\n"
		"	andl %%eax, %[nmsk]	/* nmsk <- 1<<bit - 1*/\n"
		"	xorb %[cnt], %[cnt]	/* cnt <- 0 */\n"
		/* main body */
		"\n"
		"	sarl $1, %[nmsk]	/* shift right and */\n"
		"	adcb $0, %[cnt]		/* add 1 if carry was set*/\n"
		"\n"
		"	sarl $1, %[nmsk]	/* shift right and */\n"
		"	adcb $0, %[cnt]		/* add 1 if carry was set*/\n"
		"\n"
		"	sarl $1, %[nmsk]	/* shift right and */\n"
		"	adcb $0, %[cnt]		/* add 1 if carry was set*/\n"
		"\n"
		"	sarl $1, %[nmsk]	/* shift right and */\n"
		/* maybe %[nmsk] is zero meanwhile, check for it and
		 * return if so */
		"	jz 0f\n"
		"	adcb $0, %[cnt]		/* add 1 if carry was set*/\n"
		"\n"
		"	sarl $1, %[nmsk]	/* shift right and */\n"
		"	adcb $0, %[cnt]		/* add 1 if carry was set*/\n"
		"\n"
		"	sarl $1, %[nmsk]	/* shift right and */\n"
		/* maybe %[nmsk] is zero meanwhile, check for it and
		 * return if so */
		"	jz 0f\n"
		"	adcb $0, %[cnt]		/* add 1 if carry was set*/\n"
		"\n"
		"	sarl $1, %[nmsk]	/* shift right and */\n"
		/* maybe %[nmsk] is zero meanwhile, check for it and
		 * return if so */
		"	jz 0f\n"
		"	adcb $0, %[cnt]		/* add 1 if carry was set*/\n"
		"\n"
		"	sarl $1, %[nmsk]	/* shift right and */\n"
		"0:\n"
		"	adcb $0, %[cnt]		/* add 1 if carry was set*/\n"
#if !defined HAVE_ASM_RETVAL_IN_EBX
		"	popl %%ebx		/* restore EBX */\n"
#endif
		: [cnt] "+c" (cnt) /* <- bit is in here */
		: [nmsk] "d" (number)
		: "%eax", "cc");
	return cnt;
}
#else  /* !all the above */
extern_inline char
__nbits_right_of(int number, char bit)
{
  register char cnt = 0;
  register unsigned n;
  for(n = number & ((1<<bit)-1); n ; n >>= 1)
    if( n%2 ) cnt++;
  return cnt;
}
#endif

#if 1				/* using the global shit */
static inline void*const*
cat_morphisms(const void *obj)
{
/* returns a pointer to the array of implementations or
 * NULL if there are none */
	if (((const struct lrecord_header*)obj)->morphisms & cat_mk_lc) {
		return &((const struct __cat_morphism_lcrec_s*)obj)->foo;
	} else {
		return &((const struct __cat_morphism_lrec_s*)obj)->foo;
	}
}

static inline void*
cat_morphism(const void *obj, cat_morphism_kind_t kind)
{
	unsigned int flags = ((const struct lrecord_header*)obj)->morphisms;

	if (!__bit_set_p(flags, kind)) {
		int type = ((const struct lrecord_header*)obj)->type;
		switch (kind) {
		case cat_mk_seq:
			return morphisms[type].seq_impl;
		case cat_mk_set:
			return morphisms[type].set_impl;
		case cat_mk_aseq:
			return morphisms[type].aseq_impl;
		case cat_mk_aset:
			return morphisms[type].aset_impl;
			/* list them all here */
		case cat_mk_none:
		case cat_mk_lc:
		case number_of_cat_morphism_kinds:
		default:
			return NULL;
		}
	} else {
		void *const*mph = cat_morphisms(obj);
		return mph[__nbits_right_of(flags, kind)-(flags&1)];
	}
}
#else  /* use definition in category.c */
extern void *cat_morphism(const void*, cat_morphism_kind_t);
extern void *const*cat_morphisms(const void*);
#endif
#endif	/* INCLUDED_category_h_ */
