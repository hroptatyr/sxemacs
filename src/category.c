/*** category.c -- categorial view on objects, this is NOT OO
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

#include "config.h"
#include "lisp.h"
#include "category.h"
#include "lrecord.h"

struct cat_morphism_s __morphisms[lrecord_type_last_built_in_type];
cat_morphism_t morphisms = __morphisms;

#if 0
void*const*
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

void*
cat_morphism(const void *obj, cat_morphism_kind_t kind)
{
	unsigned int flags = ((const struct lrecord_header*)obj)->morphisms;

	if (!__bit_set_p(flags, kind)) {
		int type = ((const struct lrecord_header*)obj)->type;
		switch (kind) {
		case cat_mk_seq:
			GC_CRITICAL("here %x %x\n", flags, kind);
			return morphisms[type].seq_impl;
		case cat_mk_set:
			return morphisms[type].set_impl;
		case cat_mk_aseq:
			return morphisms[type].aseq_impl;
		case cat_mk_aset:
			return morphisms[type].aset_impl;
		default:
			return NULL;
		}
	} else {
		void *const*mph = cat_morphisms(obj);
		return mph[__nbits_right_of(flags, kind)-(flags&1)];
	}
}
#endif

/* category.c ends here */
