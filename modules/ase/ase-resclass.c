/*** ase-resclass.c -- Residue Class Rings for SXEmacs
 *
 * Copyright (C) 2006 - 2008 Sebastian Freundt
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

/* Synched up with: Not in FSF. */

#include "config.h"
#include "sxemacs.h"
#include "ent/ent.h"
#include "ase-resclass.h"

#define EMOD_ASE_DEBUG_RESC(args...)	EMOD_ASE_DEBUG("[RESC]: " args)

PROVIDE(ase_resclass);
REQUIRE(ase_resclass, "ase");

Lisp_Object Qase_resclass;
Lisp_Object Qase_resc_rng, Qase_resc_rng_p, Qase_resc_elm, Qase_resc_elm_p;
static int sane_small;

#if 0
static ase_nullary_operation_f Qase_resclass_zero, Qase_resclass_one;
#endif


static inline int
_resc_rng_buffer_size(ase_resc_rng_t a)
{
	/* returns a sane size for buffer allocation */
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	if (ase_resc_rng_smallp(a))
		return sane_small;
	else
		return (mpz_sizeinbase(ase_resc_rng_ring(a), 10) + 7) & -3;
#else
	return sane_small;
#endif
}

static inline void
_ase_resc_rng_to_string(char *buf, int len, ase_resc_rng_t a)
{
	if (ase_resc_rng_smallp(a)) {
		int sz = snprintf(buf, len, "%ld", a->small_ring);
		assert(sz >= 0 && sz < len);
	} else
		resc_rng_to_string(buf, len, ase_resc_rng_ring(a));
	return;
}

void
_ase_resc_rng_prnt(ase_resc_rng_t a, Lisp_Object pcf)
{
	int sane_sz = sizeof(char)*_resc_rng_buffer_size(a);
	char *fstr = alloca(sane_sz);
	_ase_resc_rng_to_string(fstr, sane_sz, a);
	write_c_string("Z/", pcf);
	write_c_string(fstr, pcf);
	write_c_string("Z", pcf);
	return;
}

static void
ase_resc_rng_prnt(Lisp_Object obj, Lisp_Object pcf, int unused)
{
	EMOD_ASE_DEBUG_RESC("i:0x%016lx@0x%016lx (rc:%d)\n",
			    (long unsigned int)(XASE_RESC_RNG(obj)),
			    (long unsigned int)obj, 1);
	write_c_string("#<", pcf);
	print_internal(XDYNACAT_TYPE(obj), pcf, unused);
	write_c_string(" ", pcf);
	_ase_resc_rng_prnt(XASE_RESC_RNG(obj), pcf);
	if (XASE_RESC_RNG_SMALLP(obj))
		write_c_string(", small", pcf);
	write_c_string(">", pcf);
}

static inline int
_resc_elm_buffer_size(ase_resc_elm_t a)
{
	/* returns a sane size for buffer allocation */
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	if (ase_resc_rng_smallp(a))
		return sane_small;
	else
		return (mpz_sizeinbase(ase_resc_elm_data(a), 10) + 7) & -3;
#else
	return sane_small;
#endif
}

static inline void
_ase_resc_elm_to_string(char *buf, int len, ase_resc_elm_t a)
{
	if (ase_resc_elm_smallp(a)) {
		int sz = snprintf(buf, len, "%ld", a->small_data);
		assert(sz>=0 && sz < len);
	} else
		resc_elm_to_string(buf, len, ase_resc_elm_data(a));
	return;
}

void
_ase_resc_elm_prnt(ase_resc_elm_t a, Lisp_Object pcf)
{
	int sane_sz_rng = sizeof(char)*_resc_rng_buffer_size(
		XASE_RESC_RNG(ase_resc_elm_ring(a)));
	int sane_sz_elm = sizeof(char)*_resc_elm_buffer_size(a);
	char *rng_str = alloca(sane_sz_rng);
	char *elm_str = alloca(sane_sz_elm);

	_ase_resc_rng_to_string(rng_str, sane_sz_rng,
				XASE_RESC_RNG(ase_resc_elm_ring(a)));
	_ase_resc_elm_to_string(elm_str, sane_sz_elm, a);

	write_c_string(elm_str, pcf);
	write_c_string("+", pcf);
	write_c_string(rng_str, pcf);
	write_c_string("Z", pcf);
	return;
}

static void
ase_resc_elm_prnt(Lisp_Object obj, Lisp_Object pcf, int unused)
{
	EMOD_ASE_DEBUG_RESC("i:0x%016lx@0x%016lx (rc:%d)\n",
			    (long unsigned int)(XASE_RESC_ELM(obj)),
			    (long unsigned int)obj, 1);
	write_c_string("#<", pcf);
	print_internal(XDYNACAT_TYPE(obj), pcf, unused);
	write_c_string(" ", pcf);
	_ase_resc_elm_prnt(XASE_RESC_ELM(obj), pcf);
	if (XASE_RESC_ELM_SMALLP(obj))
		write_c_string(", small", pcf);
	write_c_string(">", pcf);
}

/* stuff for the dynacat, markers */
static inline void
_ase_resc_rng_mark(ase_resc_rng_t a)
{
	if (a == NULL)
		return;
	return;
}

static void
ase_resc_rng_mark(Lisp_Object obj)
{
	EMOD_ASE_DEBUG_RESC("i:0x%016lx@0x%016lx (rc:%d) shall be marked...\n",
			    (long unsigned int)(XASE_RESC_RNG(obj)),
			    (long unsigned int)obj, 1);
	_ase_resc_rng_mark(XASE_RESC_RNG(obj));
	return;
}

static inline void
_ase_resc_elm_mark(ase_resc_elm_t a)
{
	mark_object(ase_resc_elm_ring(a));
}

static void
ase_resc_elm_mark(Lisp_Object obj)
{
	EMOD_ASE_DEBUG_RESC("i:0x%016lx@0x%016lx (rc:%d) shall be marked...\n",
			    (long unsigned int)(XASE_RESC_ELM(obj)),
			    (long unsigned int)obj, 1);
	_ase_resc_elm_mark(XASE_RESC_ELM(obj));
	return;
}

/* stuff for the dynacat, finalisers */
static inline void
_ase_resc_rng_fini(ase_resc_rng_t a)
{
	if (!ase_resc_rng_smallp(a))
		resc_rng_fini(ase_resc_rng_ring(a));
	return;
}

static void
ase_resc_rng_fini(Lisp_Object obj, int unused)
{
	ase_resc_rng_t a = XASE_RESC_RNG(obj);

	EMOD_ASE_DEBUG_GC("i:0x%016lx@0x%016lx (rc:%d) shall be freed...\n",
			  (long unsigned int)(a), obj, 1);

	_ase_resc_rng_fini(a);
	xfree(a);
	return;
}

static inline void
_ase_resc_elm_fini(ase_resc_elm_t a)
{
	if (!ase_resc_elm_smallp(a))
		resc_elm_fini(ase_resc_elm_data(a));
	return;
}

static void
ase_resc_elm_fini(Lisp_Object obj, int unused)
{
	ase_resc_elm_t a = XASE_RESC_ELM(obj);

	EMOD_ASE_DEBUG_GC("i:0x%016lx@0x%016lx (rc:%d) shall be freed...\n",
			  (long unsigned int)(a), obj, 1);

	_ase_resc_elm_fini(a);
	xfree(a);
	return;
}


static inline Lisp_Object
_ase_wrap_resc_rng(ase_resc_rng_t a)
{
	Lisp_Object result;

	result = make_dynacat(a);
	XDYNACAT(result)->type = Qase_resc_rng;

#if 0
	if (a)
		ase_interval_incref(a);
#endif

	set_dynacat_printer(result, ase_resc_rng_prnt);
	set_dynacat_marker(result, ase_resc_rng_mark);
	set_dynacat_finaliser(result, ase_resc_rng_fini);
	set_dynacat_intprinter(
		result, (dynacat_intprinter_f)_ase_resc_rng_prnt);

	EMOD_ASE_DEBUG_RESC("i:0x%016lx (rc:%d) "
			    "shall be wrapped to 0x%016lx...\n",
			    (long unsigned int)a, 1,
			    (long unsigned int)result);

	return result;
}

ase_resc_rng_t
_ase_make_resc_rng(Lisp_Object modulus)
{
	ase_resc_rng_t a = xnew(struct ase_resc_rng_s);

	if (INTP(modulus)) {
		a->smallp = 1;
		a->small_ring = XINT(modulus);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else if (BIGZP(modulus)) {
		a->smallp = 0;
		resc_rng_init(ase_resc_rng_ring(a));
		resc_rng_set_bigz(ase_resc_rng_ring(a), XBIGZ_DATA(modulus));
#endif
	}

	EMOD_ASE_DEBUG_RESC("i:0x%016lx (rc:0) shall be created...\n",
			    (long unsigned int)a);
	return a;
}

/* specialised versions for the lisp reader */
static inline ase_resc_rng_t
__ase_make_resc_rng_eint(EMACS_INT modulus)
{
	ase_resc_rng_t a = xnew(struct ase_resc_rng_s);

	a->smallp = 1;
	a->small_ring = modulus;
	EMOD_ASE_DEBUG_RESC("i:0x%016lx (rc:0) shall be created...\n",
			    (long unsigned int)a);
	return a;
}
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static inline ase_resc_rng_t
__ase_make_resc_rng_bigz(resc_rng modulus)
{
	ase_resc_rng_t a = xnew(struct ase_resc_rng_s);

	a->smallp = 0;
	*a->ring = *modulus;
	EMOD_ASE_DEBUG_RESC("i:0x%016lx (rc:0) shall be created...\n",
			    (long unsigned int)a);
	return a;
}
#endif

Lisp_Object
ase_make_resc_rng(Lisp_Object modulus)
{
	ase_resc_rng_t a = NULL;
	Lisp_Object result = Qnil;

	a = _ase_make_resc_rng(modulus);
	XSETASE_RESC_RNG(result, a);

	return result;
}

Lisp_Object
_ase_wrap_resc_elm(ase_resc_elm_t a)
{
	Lisp_Object result;

	result = make_dynacat(a);
	XDYNACAT(result)->type = Qase_resc_elm;

#if 0
	if (a)
		ase_interval_incref(a);
#endif

	set_dynacat_printer(result, ase_resc_elm_prnt);
	set_dynacat_marker(result, ase_resc_elm_mark);
	set_dynacat_finaliser(result, ase_resc_elm_fini);
	set_dynacat_intprinter(
		result, (dynacat_intprinter_f)_ase_resc_elm_prnt);

	EMOD_ASE_DEBUG_RESC("i:0x%016lx (rc:%d) "
			    "shall be wrapped to 0x%016lx...\n",
			    (long unsigned int)a, 1,
			    (long unsigned int)result);

	return result;
}

static inline void
_ase_resc_elm_canonicalise_small(ase_resc_elm_t a)
{
	if ((a->small_data = a->small_data %
	     XASE_RESC_RNG(ase_resc_elm_ring(a))->small_ring) < 0)
		a->small_data +=
			XASE_RESC_RNG(ase_resc_elm_ring(a))->small_ring;
}

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static void
_ase_resc_elm_canonicalise_big(ase_resc_elm_t a)
{
	bigz_mod(ase_resc_elm_data(a), ase_resc_elm_data(a),
		 XASE_RESC_RNG_RING(ase_resc_elm_ring(a)));
}
#endif

static inline void
_ase_resc_elm_canonicalise(ase_resc_elm_t a)
{
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	if (LIKELY(ase_resc_elm_smallp(a))) {
		_ase_resc_elm_canonicalise_small(a);
	} else {
		_ase_resc_elm_canonicalise_big(a);
	}
#else
	ase_resc_elm_sdata(a) =
		ase_resc_elm_sdata(a) %
		XASE_RESC_RNG_SRING(ase_resc_elm_ring(a));
#endif
	return;
}

ase_resc_elm_t
_ase_make_resc_elm(Lisp_Object class, Lisp_Object ring)
{
	ase_resc_elm_t a = xnew(struct ase_resc_elm_s);

	ase_resc_elm_ring(a) = ring;

	if (!(a->smallp = XASE_RESC_RNG(ring)->smallp)) {
		resc_elm_init(ase_resc_elm_data(a));
	}

	EMOD_ASE_DEBUG_RESC("i:0x%016lx (rc:0) shall be created...\n",
			    (long unsigned int)a);

	if (INTP(class) && a->smallp) {
		a->small_data = XINT(class);
	} else if (INTP(class)) {
		resc_elm_set_eint(ase_resc_elm_data(a), XINT(class));
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else if (BIGZP(class) && a->smallp) {
		Lisp_Object newcl = _ent_binop(
			ASE_BINARY_OP_MOD,
			BIGZ_T, class,
			INT_T, make_int(XASE_RESC_RNG(ring)->small_ring));
		a->small_data = XINT(newcl);
		return a;
	} else if (BIGZP(class)) {
		resc_elm_set_bigz(ase_resc_elm_data(a),
				  XBIGZ_DATA(class));
#endif
	}

	_ase_resc_elm_canonicalise(a);
	return a;
}

/* specialised versions for the lisp reader */
static ase_resc_elm_t
__ase_make_resc_elm_eint(EMACS_INT class)
{
	ase_resc_elm_t a = xnew(struct ase_resc_elm_s);

	a->smallp = 1;
	a->small_data = class;
	EMOD_ASE_DEBUG_RESC("i:%p (rc:0) shall be created...\n", a);
	return a;
}
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static ase_resc_elm_t
__ase_make_resc_elm_bigz(resc_elm class)
{
	ase_resc_elm_t a = xnew(struct ase_resc_elm_s);

	a->smallp = 0;
	*a->data = *class;
	EMOD_ASE_DEBUG_RESC("i:%p (rc:0) shall be created...\n", a);
	return a;
}
#endif

Lisp_Object
ase_make_resc_elm(Lisp_Object class, Lisp_Object ring)
{
	ase_resc_elm_t a = NULL;
	Lisp_Object result = Qnil;

	a = _ase_make_resc_elm(class, ring);
	XSETASE_RESC_ELM(result, a);

	return result;
}


/* basic functions */
/* read a resclass off the wire */
/* the next 4 funs are hooked in the lisp reader (lread.c) */
static Lisp_Object
ase_resc_rng_from_string(char *cp)
{
	ase_resc_rng_t r = NULL;
	char *start, *tail;
	char tmp;
	EMACS_INT small_ring;

	/* Jump over Z */
	cp++;
	/* Jump over / */
	cp++;

	start = cp;

	while ((*cp >= '0' && *cp <= '9'))
		cp++;

	/* MPZ cannot read numbers with characters after them.
	 * See limitations of GMP-MPZ strings
	 */
	tmp = (Bufbyte)*cp;
	*cp = '\0';
	errno = 0;
	small_ring = strtol(start, &tail, 10);
	if (errno == 0) {
		r = __ase_make_resc_rng_eint(small_ring);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else if (errno == ERANGE) {
		resc_rng ring;
		resc_rng_init(ring);
		resc_rng_set_string(ring, start);
		r = __ase_make_resc_rng_bigz(ring);
#endif
	} else {
		/* panic */
	}
	*cp = tmp;

	/* generate and return the ring */
	return _ase_wrap_resc_rng(r);
}

static Lisp_Object
ase_resc_elm_from_string(char *cp)
{
	ase_resc_elm_t e = NULL;
	ase_resc_rng_t r = NULL;
	char *start, *tail;
	char tmp;
	EMACS_INT small_ring;
	EMACS_INT small_elm;

	/* MPZ bigz_set_string has no effect
	 * with initial + sign */
	if (*cp == '+')
		cp++;

	start = cp;

	if (*cp == '-') {
		/* jump over a leading minus */
		cp++;
	}

	while ((*cp >= '0' && *cp <= '9'))
		cp++;

	/* MPZ cannot read numbers with characters after them.
	 * See limitations of GMP-MPZ strings
	 */
	tmp = *cp;
	*cp = '\0';
	errno = 0;
	small_elm = strtol(start, &tail, 10);
	if (errno == 0) {
		e = __ase_make_resc_elm_eint(small_elm);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else if (errno == ERANGE) {
		resc_elm elm;
		resc_elm_init(elm);
		resc_elm_set_string(elm, start);
		e = __ase_make_resc_elm_bigz(elm);
#endif
	} else {
		/* panic */
	}
	*cp = tmp;

	/* read the modulus */
	if (*cp == '+')
		cp++;
	start = cp;
	while ((*cp >= '0' && *cp <= '9'))
		cp++;
	tmp = *cp;
	*cp = '\0';
	errno = 0;
	small_ring = strtol(start, &tail, 10);
	if (errno == 0) {
		r = __ase_make_resc_rng_eint(small_ring);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else if (errno == ERANGE) {
		resc_rng ring;
		resc_rng_init(ring);
		resc_rng_set_string(ring, start);
		r = __ase_make_resc_rng_bigz(ring);
#endif
	} else {
		/* panic */
	}
	*cp = tmp;

	/* now we have 4 possibilites: */
	if (e->smallp && r->smallp) {
		e->ring = _ase_wrap_resc_rng(r);
		_ase_resc_elm_canonicalise_small(e);
		return _ase_wrap_resc_elm(e);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else if (e->smallp) {
		/* the class is small, the ring is not,
		 * thus we have to promote the class now */
		resc_elm_init(ase_resc_elm_data(e));
		resc_elm_set_eint(ase_resc_elm_data(e), e->small_data);
		e->smallp = 0;
		e->ring = _ase_wrap_resc_rng(r);
		_ase_resc_elm_canonicalise_big(e);
		return _ase_wrap_resc_elm(e);
	} else if (r->smallp) {
		/* we're in deep shit, the class is bigz, the ring is small
		 * and to make it worse, we can't use the ENT mod table */
		bigz_set_long(ent_scratch_bigz, r->small_ring);
		bigz_mod(ent_scratch_bigz,
			 ase_resc_elm_data(e), ent_scratch_bigz);
		/* now ent_scratch_bigz should fit into a long */
		e->small_data = bigz_to_long(ent_scratch_bigz);
		e->smallp = 1;
		/* finish the temporarily assigned big data slot */
		resc_elm_fini(ase_resc_elm_data(e));
		e->ring = _ase_wrap_resc_rng(r);
		/* no need to canonicalise */
		return _ase_wrap_resc_elm(e);
	} else {
		/* phew, finally an easy case */
		e->ring = _ase_wrap_resc_rng(r);
		_ase_resc_elm_canonicalise_big(e);
		return _ase_wrap_resc_elm(e);
#endif
	}
	return Qnil;
}

#define LEAD_INT 1
#define DOT_CHAR 2
#define TRAIL_INT 4
#define E_CHAR 8
#define EXP_INT 16
/* for complex numbers */
#define INTERMEDIATE_UNARY_SYMBOL 32
#define LEAD_INT2 64
#define DOT_CHAR2 128
#define TRAIL_INT2 256
#define E_CHAR2 512
#define EXP_INT2 1024
#define I_CHAR 2048
#define LEAD_Z 2
#define Z_CHAR 4096

static int
ase_resc_rng_string_p(const char *cp)
{
	int state;
	const Bufbyte *ucp = (const Bufbyte *)cp;


	/* parse the residue class */
	state = 0;
	if (*ucp++ == 'Z' && *ucp++ == '/')
		state |= LEAD_Z;

	/* check if we had a int number until here */
	if (!(state == (LEAD_Z)))
		return 0;

	/* now look for the modulus */
	state = 0;
	if (*ucp >= '1' && *ucp <= '9') {
		state |= LEAD_INT2;
		while (*ucp >= '0' && *ucp <= '9')
			ucp++;
	}
	if (*ucp == 'Z') {
		state |= Z_CHAR;
		ucp++;
	}
	return (((*ucp == 0) || (*ucp == ' ') || (*ucp == '\t') ||
		 (*ucp == '\n') || (*ucp == '\r') || (*ucp == '\f')) &&
		(state == (LEAD_INT2 | Z_CHAR)));
}

static int
ase_resc_elm_string_p(const char *cp)
{
	int state;
	const Bufbyte *ucp = (const Bufbyte *)cp;


	/* parse the residue class */
	state = 0;
	if (*ucp == '+' || *ucp == '-')
		ucp++;

	if (*ucp >= '0' && *ucp <= '9') {
		state |= LEAD_INT;
		while (*ucp >= '0' && *ucp <= '9')
			ucp++;
	}

	/* check if we had a int number until here */
	if (!(state == (LEAD_INT)))
		return 0;

	/* now look for the residue class ring */
	state = 0;
	if (*ucp == '+') {
		state |= INTERMEDIATE_UNARY_SYMBOL;
		ucp++;
	}

	if (*ucp >= '1' && *ucp <= '9') {
		state |= LEAD_INT2;
		while (*ucp >= '0' && *ucp <= '9')
			ucp++;
	}
	if (*ucp == 'Z') {
		state |= Z_CHAR;
		ucp++;
	}
	return (((*ucp == 0) || (*ucp == ' ') || (*ucp == '\t') ||
		 (*ucp == '\n') || (*ucp == '\r') || (*ucp == '\f')) &&
		(state == (INTERMEDIATE_UNARY_SYMBOL | LEAD_INT2 | Z_CHAR)));
}


#if 0
static Lisp_Object
ent_intersection_factor_module(Lisp_Object *l, Lisp_Object *r)
{
	Lisp_Object result_ring;

	/* return a resulting ring by intersection of the rings in l and r and
	 * coerce l and r to that ring.
	 */

	if (!bigz_eql(XRESC_RNG_DATA(XRESC_ELM_RING(*l)),
		      XRESC_RNG_DATA(XRESC_ELM_RING(*r)))) {

		/* find a ring by intersection */
		bigz_lcm(ent_scratch_bigz,
			 XRESC_RNG_DATA(XRESC_ELM_RING(*l)),
			 XRESC_RNG_DATA(XRESC_ELM_RING(*r)));
		result_ring = make_resc_rng_bz(ent_scratch_bigz);

		/* coerce the left ring element to the lcm-ring */
		bigz_div(ent_scratch_bigz,
			 XRESC_RNG_DATA(result_ring),
			 XRESC_RNG_DATA(XRESC_ELM_RING(*l)));
		bigz_mul(ent_scratch_bigz,
			 XRESC_ELM_DATA(*l),
			 ent_scratch_bigz);
		*l = make_resc_elm_bz(ent_scratch_bigz, result_ring);

		/* coerce the right ring element to the lcm-ring */
		bigz_div(ent_scratch_bigz,
			 XRESC_RNG_DATA(result_ring),
			 XRESC_RNG_DATA(XRESC_ELM_RING(*r)));
		bigz_mul(ent_scratch_bigz,
			 XRESC_ELM_DATA(*r),
			 ent_scratch_bigz);
		*r = make_resc_elm_bz(ent_scratch_bigz, result_ring);

	} else
		result_ring = XRESC_ELM_RING(*l);

	return result_ring;
}
#endif

static int
ase_resclass_check_rings(Lisp_Object l, Lisp_Object r)
{
	if (XASE_RESC_ELM_SMALLP(l) ^ XASE_RESC_ELM_SMALLP(r)) {
	domain_error:
		Fsignal(Qdomain_error, list2(
				XASE_RESC_ELM_RING(l), XASE_RESC_ELM_RING(r)));
		return 0;
	} else if (XASE_RESC_ELM_SMALLP(l) &&
		   XASE_RESC_RNG_SRING(XASE_RESC_ELM_RING(l)) ==
		   XASE_RESC_RNG_SRING(XASE_RESC_ELM_RING(r))) {
		return 1;
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else if (!XASE_RESC_ELM_SMALLP(l) &&
		   bigz_eql(XASE_RESC_RNG_RING(XASE_RESC_ELM_RING(l)),
			    XASE_RESC_RNG_RING(XASE_RESC_ELM_RING(r)))) {
		return 1;
#endif
	}
	goto domain_error;
	return 0;
}

static inline Lisp_Object
ase_resclass_sum_small(Lisp_Object l, Lisp_Object r, Lisp_Object result_ring)
{
	EMACS_INT sum =
		XASE_RESC_ELM(l)->small_data + XASE_RESC_ELM(r)->small_data;
	return ase_make_resc_elm(make_int(sum), result_ring);
}

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static inline Lisp_Object
ase_resclass_sum_big(Lisp_Object l, Lisp_Object r, Lisp_Object result_ring)
{
	ase_resc_elm_t e = NULL;
	bigz_add(ent_scratch_bigz,
		 XASE_RESC_ELM_DATA(l), XASE_RESC_ELM_DATA(r));
	e = __ase_make_resc_elm_bigz(ent_scratch_bigz);
	e->smallp = 0;
	e->ring = result_ring;
	_ase_resc_elm_canonicalise_big(e);
	return _ase_wrap_resc_elm(e);
}
#endif

static Lisp_Object
ase_resclass_sum(Lisp_Object l, Lisp_Object r)
{
	ase_resclass_check_rings(l, r);

#if 0
	result_ring = ent_intersection_factor_module(&l, &r);
#endif

	if (XASE_RESC_ELM_SMALLP(l) && XASE_RESC_ELM_SMALLP(r)) {
		return ase_resclass_sum_small(l, r, XASE_RESC_ELM_RING(l));
	} else {
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
		return ase_resclass_sum_big(l, r, XASE_RESC_ELM_RING(l));
#endif
	}
	return Qnil;
}


static inline Lisp_Object
ase_resclass_sum_intg_small(
	Lisp_Object l, Lisp_Object intg, Lisp_Object result_ring)
{
	if (INTP(intg)) {
		EMACS_INT sum =
			XASE_RESC_ELM(l)->small_data + XINT(intg);
		return ase_make_resc_elm(make_int(sum), result_ring);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else if (BIGZP(intg)) {
		EMACS_INT sum;
		bigz_set_long(ent_scratch_bigz,
			      XASE_RESC_RNG(XASE_RESC_ELM_RING(l))->small_ring);
		bigz_mod(ent_scratch_bigz, XBIGZ_DATA(intg), ent_scratch_bigz);
		sum = XASE_RESC_ELM(l)->small_data +
			bigz_to_long(ent_scratch_bigz);
		return ase_make_resc_elm(make_int(sum), result_ring);
#endif
	}
	return Qnil;
}

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static inline Lisp_Object
ase_resclass_sum_intg_big(
	Lisp_Object l, Lisp_Object intg, Lisp_Object result_ring)
{
	if (INTP(intg)) {
		ase_resc_elm_t e = NULL;
		bigz_set_long(ent_scratch_bigz, XINT(intg));
		bigz_add(ent_scratch_bigz,
			 XASE_RESC_ELM_DATA(l), ent_scratch_bigz);
		e = __ase_make_resc_elm_bigz(ent_scratch_bigz);
		e->smallp = 0;
		e->ring = result_ring;
		_ase_resc_elm_canonicalise_big(e);
		return _ase_wrap_resc_elm(e);
	} else if (BIGZP(intg)) {
		ase_resc_elm_t e = NULL;
		bigz_add(ent_scratch_bigz,
			 XASE_RESC_ELM_DATA(l), XBIGZ_DATA(intg));
		e = __ase_make_resc_elm_bigz(ent_scratch_bigz);
		e->smallp = 0;
		e->ring = result_ring;
		_ase_resc_elm_canonicalise_big(e);
		return _ase_wrap_resc_elm(e);
	}
	return Qnil;
}
#endif

static Lisp_Object
ase_resclass_sum_intg(Lisp_Object l, Lisp_Object r)
{
	if (INTEGERP(l) && XASE_RESC_ELM_SMALLP(r)) {
		return ase_resclass_sum_intg_small(
			r, l, XASE_RESC_ELM_RING(r));
	} else if (INTEGERP(r) && XASE_RESC_ELM_SMALLP(l)) {
		return ase_resclass_sum_intg_small(
			l, r, XASE_RESC_ELM_RING(l));
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else if (INTEGERP(l)) {
		return ase_resclass_sum_intg_big(
			r, l, XASE_RESC_ELM_RING(r));
	} else if (INTEGERP(r)) {
		return ase_resclass_sum_intg_big(
			l, r, XASE_RESC_ELM_RING(l));
#endif
	}
	return Qnil;
}


static inline Lisp_Object
ase_resclass_diff_small(Lisp_Object l, Lisp_Object r, Lisp_Object result_ring)
{
	EMACS_INT diff =
		XASE_RESC_ELM(l)->small_data - XASE_RESC_ELM(r)->small_data;
	return ase_make_resc_elm(make_int(diff), result_ring);
}

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static inline Lisp_Object
ase_resclass_diff_big(Lisp_Object l, Lisp_Object r, Lisp_Object result_ring)
{
	ase_resc_elm_t e = NULL;
	bigz_sub(ent_scratch_bigz,
		 XASE_RESC_ELM_DATA(l), XASE_RESC_ELM_DATA(r));
	e = __ase_make_resc_elm_bigz(ent_scratch_bigz);
	e->smallp = 0;
	e->ring = result_ring;
	_ase_resc_elm_canonicalise_big(e);
	return _ase_wrap_resc_elm(e);
}
#endif

static Lisp_Object
ase_resclass_diff(Lisp_Object l, Lisp_Object r)
{
	ase_resclass_check_rings(l, r);

#if 0
	result_ring = ent_intersection_factor_module(&l, &r);
#endif

	if (XASE_RESC_ELM_SMALLP(l) && XASE_RESC_ELM_SMALLP(r)) {
		return ase_resclass_diff_small(l, r, XASE_RESC_ELM_RING(l));
	} else {
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
		return ase_resclass_diff_big(l, r, XASE_RESC_ELM_RING(l));
#endif
	}
	return Qnil;
}


static inline Lisp_Object
ase_resclass_diff_intg_small(
	Lisp_Object l, Lisp_Object intg, Lisp_Object result_ring)
{
	if (INTP(intg)) {
		EMACS_INT diff =
			XASE_RESC_ELM(l)->small_data - XINT(intg);
		return ase_make_resc_elm(make_int(diff), result_ring);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else if (BIGZP(intg)) {
		EMACS_INT diff;
		bigz_set_long(ent_scratch_bigz,
			      XASE_RESC_RNG(XASE_RESC_ELM_RING(l))->small_ring);
		bigz_mod(ent_scratch_bigz, XBIGZ_DATA(intg), ent_scratch_bigz);
		diff = XASE_RESC_ELM(l)->small_data -
			bigz_to_long(ent_scratch_bigz);
		return ase_make_resc_elm(make_int(diff), result_ring);
#endif
	}
	return Qnil;
}

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static inline Lisp_Object
ase_resclass_diff_intg_big(
	Lisp_Object l, Lisp_Object intg, Lisp_Object result_ring)
{
	if (INTP(intg)) {
		ase_resc_elm_t e = NULL;
		bigz_set_long(ent_scratch_bigz, XINT(intg));
		bigz_sub(ent_scratch_bigz,
			 XASE_RESC_ELM_DATA(l), ent_scratch_bigz);
		e = __ase_make_resc_elm_bigz(ent_scratch_bigz);
		e->smallp = 0;
		e->ring = result_ring;
		_ase_resc_elm_canonicalise_big(e);
		return _ase_wrap_resc_elm(e);
	} else if (BIGZP(intg)) {
		ase_resc_elm_t e = NULL;
		bigz_sub(ent_scratch_bigz,
			 XASE_RESC_ELM_DATA(l), XBIGZ_DATA(intg));
		e = __ase_make_resc_elm_bigz(ent_scratch_bigz);
		e->smallp = 0;
		e->ring = result_ring;
		_ase_resc_elm_canonicalise_big(e);
		return _ase_wrap_resc_elm(e);
	}
	return Qnil;
}
#endif

static Lisp_Object
ase_resclass_diff_intg(Lisp_Object l, Lisp_Object r)
{
	if (XASE_RESC_ELM_SMALLP(l)) {
		return ase_resclass_diff_intg_small(
			l, r, XASE_RESC_ELM_RING(l));
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else {
		return ase_resclass_diff_intg_big(
			l, r, XASE_RESC_ELM_RING(l));
#endif
	}
	return Qnil;
}


static inline Lisp_Object
ase_resclass_prod_small(Lisp_Object l, Lisp_Object r, Lisp_Object result_ring)
{
	EMACS_INT prod =
		XASE_RESC_ELM(l)->small_data * XASE_RESC_ELM(r)->small_data;
	return ase_make_resc_elm(make_int(prod), result_ring);
}

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static inline Lisp_Object
ase_resclass_prod_big(Lisp_Object l, Lisp_Object r, Lisp_Object result_ring)
{
	ase_resc_elm_t e = NULL;
	bigz_mul(ent_scratch_bigz,
		 XASE_RESC_ELM_DATA(l), XASE_RESC_ELM_DATA(r));
	e = __ase_make_resc_elm_bigz(ent_scratch_bigz);
	e->smallp = 0;
	e->ring = result_ring;
	_ase_resc_elm_canonicalise_big(e);
	return _ase_wrap_resc_elm(e);
}
#endif

static Lisp_Object
ase_resclass_prod(Lisp_Object l, Lisp_Object r)
{
	ase_resclass_check_rings(l, r);

#if 0
	result_ring = ent_intersection_factor_module(&l, &r);
#endif

	if (XASE_RESC_ELM_SMALLP(l) && XASE_RESC_ELM_SMALLP(r)) {
		return ase_resclass_prod_small(l, r, XASE_RESC_ELM_RING(l));
	} else {
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
		return ase_resclass_prod_big(l, r, XASE_RESC_ELM_RING(l));
#endif
	}
	return Qnil;
}

static inline Lisp_Object
ase_resclass_prod_intg_small(
	Lisp_Object l, Lisp_Object intg, Lisp_Object result_ring)
{
	if (INTP(intg)) {
		EMACS_INT prod =
			XASE_RESC_ELM(l)->small_data * XINT(intg);
		return ase_make_resc_elm(make_int(prod), result_ring);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else if (BIGZP(intg)) {
		EMACS_INT prod;
		bigz_set_long(ent_scratch_bigz,
			      XASE_RESC_RNG(XASE_RESC_ELM_RING(l))->small_ring);
		bigz_mod(ent_scratch_bigz, XBIGZ_DATA(intg), ent_scratch_bigz);
		prod = XASE_RESC_ELM(l)->small_data *
			bigz_to_long(ent_scratch_bigz);
		return ase_make_resc_elm(make_int(prod), result_ring);
#endif
	}
	return Qnil;
}

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static inline Lisp_Object
ase_resclass_prod_intg_big(
	Lisp_Object l, Lisp_Object intg, Lisp_Object result_ring)
{
	if (INTP(intg)) {
		ase_resc_elm_t e = NULL;
		bigz_set_long(ent_scratch_bigz, XINT(intg));
		bigz_mul(ent_scratch_bigz,
			 XASE_RESC_ELM_DATA(l), ent_scratch_bigz);
		e = __ase_make_resc_elm_bigz(ent_scratch_bigz);
		e->smallp = 0;
		e->ring = result_ring;
		_ase_resc_elm_canonicalise_big(e);
		return _ase_wrap_resc_elm(e);
	} else if (BIGZP(intg)) {
		ase_resc_elm_t e = NULL;
		bigz_mul(ent_scratch_bigz,
			 XASE_RESC_ELM_DATA(l), XBIGZ_DATA(intg));
		e = __ase_make_resc_elm_bigz(ent_scratch_bigz);
		e->smallp = 0;
		e->ring = result_ring;
		_ase_resc_elm_canonicalise_big(e);
		return _ase_wrap_resc_elm(e);
	}
	return Qnil;
}
#endif

static Lisp_Object
ase_resclass_prod_intg(Lisp_Object l, Lisp_Object r)
{
	if (INTEGERP(l) && XASE_RESC_ELM_SMALLP(r)) {
		return ase_resclass_prod_intg_small(
			r, l, XASE_RESC_ELM_RING(r));
	} else if (INTEGERP(r) && XASE_RESC_ELM_SMALLP(l)) {
		return ase_resclass_prod_intg_small(
			l, r, XASE_RESC_ELM_RING(l));
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else if (INTEGERP(l)) {
		return ase_resclass_prod_intg_big(
			r, l, XASE_RESC_ELM_RING(r));
	} else if (INTEGERP(r)) {
		return ase_resclass_prod_intg_big(
			l, r, XASE_RESC_ELM_RING(l));
#endif
	}
	return Qnil;
}


static Lisp_Object
ase_resclass_div(Lisp_Object l, Lisp_Object r)
{
	ase_object_type_t idx = ase_optable_index_typesym(Qase_resc_elm);
	Lisp_Object inv = _ent_unop(ASE_UNARY_OP_INV, idx, r);
	Lisp_Object mul = _ent_binop(ASE_BINARY_OP_PROD, idx, l, idx, inv);
	return mul;
}

static Lisp_Object
ase_resclass_div_INT_T(Lisp_Object l, Lisp_Object r)
{
	ase_object_type_t idx = ase_optable_index_typesym(Qase_resc_elm);
	Lisp_Object inv = _ent_unop(ASE_UNARY_OP_INV, idx, l);
	Lisp_Object mul = _ent_binop(ASE_BINARY_OP_PROD, idx, inv, INT_T, r);
	return mul;
}

static Lisp_Object
ase_resclass_INT_T_div(Lisp_Object l, Lisp_Object r)
{
	ase_object_type_t idx = ase_optable_index_typesym(Qase_resc_elm);
	Lisp_Object inv = _ent_unop(ASE_UNARY_OP_INV, idx, r);
	Lisp_Object mul = _ent_binop(ASE_BINARY_OP_PROD, idx, l, INT_T, inv);
	return mul;
}

static Lisp_Object
ase_resclass_div_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	ase_object_type_t idx = ase_optable_index_typesym(Qase_resc_elm);
	Lisp_Object inv = _ent_unop(ASE_UNARY_OP_INV, idx, l);
	Lisp_Object mul = _ent_binop(ASE_BINARY_OP_PROD, idx, inv, BIGZ_T, r);
	return mul;
}

static Lisp_Object
ase_resclass_BIGZ_T_div(Lisp_Object l, Lisp_Object r)
{
	ase_object_type_t idx = ase_optable_index_typesym(Qase_resc_elm);
	Lisp_Object inv = _ent_unop(ASE_UNARY_OP_INV, idx, r);
	Lisp_Object mul = _ent_binop(ASE_BINARY_OP_PROD, idx, l, BIGZ_T, inv);
	return mul;
}

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static Lisp_Object
ase_resclass_pow(Lisp_Object l, Lisp_Object r)
{
	Lisp_Object rng = XASE_RESC_ELM_RING(l);
	bigz expo;

	bigz_init(expo);
	if (INTP(r)) {
		bigz_set_long(expo, ent_int(r));
	} else if (BIGZP(r)) {
		bigz_set(expo, XBIGZ_DATA(r));
	} else {
		Fsignal(Qoperation_error, r);
	}

	if (XASE_RESC_ELM_SMALLP(l)) {
		long res = 0;
		bigz tmp;

		bigz_init(tmp);
		bigz_set_long(tmp, XASE_RESC_RNG_SRING(rng));
		bigz_set_long(ent_scratch_bigz, XASE_RESC_ELM_SDATA(l));
		mpz_powm(ent_scratch_bigz, ent_scratch_bigz, expo, tmp);
		res = bigz_to_long(ent_scratch_bigz);
		bigz_fini(tmp);
		bigz_fini(expo);
		return ase_make_resc_elm(make_int(res), rng);
	} else {
		ase_resc_elm_t e = NULL;
		mpz_powm(ent_scratch_bigz, XASE_RESC_ELM_DATA(l),
			 expo, XASE_RESC_RNG_RING(l));
		e = __ase_make_resc_elm_bigz(ent_scratch_bigz);
		e->smallp = 0;
		e->ring = XASE_RESC_ELM_RING(l);
		_ase_resc_elm_canonicalise_big(e);
		bigz_fini(expo);
		return _ase_wrap_resc_elm(e);
	}
	bigz_fini(expo);
	return Qnil;
}
#endif

static Lisp_Object
ase_resclass_neg(Lisp_Object l)
{
	Lisp_Object rng = XASE_RESC_ELM_RING(l);
	if (XASE_RESC_ELM_SMALLP(l)) {
		EMACS_INT sum =
			XASE_RESC_RNG_SRING(rng) - XASE_RESC_ELM_SDATA(l);
		return ase_make_resc_elm(make_int(sum), rng);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else {
		ase_resc_elm_t e = NULL;
		bigz_set(ent_scratch_bigz, XASE_RESC_RNG_RING(rng));
		bigz_sub(ent_scratch_bigz,
			 ent_scratch_bigz, XASE_RESC_ELM_DATA(l));
		e = __ase_make_resc_elm_bigz(ent_scratch_bigz);
		e->smallp = 0;
		e->ring = rng;
		_ase_resc_elm_canonicalise_big(e);
		return _ase_wrap_resc_elm(e);
#endif
	}
	return Qnil;
}

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static Lisp_Object
ase_resclass_inv(Lisp_Object r)
{
	Lisp_Object rng = XASE_RESC_ELM_RING(r);
	int state = 0;


	if (XASE_RESC_ELM_SMALLP(r)) {
		bigz tmp;
		bigz_init(tmp);
		bigz_set_long(tmp, XASE_RESC_ELM_SDATA(r));
		bigz_set_long(ent_scratch_bigz, XASE_RESC_RNG_SRING(rng));
		state = mpz_invert(ent_scratch_bigz, tmp, ent_scratch_bigz);
		bigz_fini(tmp);
	} else {
		state = mpz_invert(ent_scratch_bigz,
				   XASE_RESC_ELM_DATA(r),
				   XASE_RESC_RNG_RING(rng));
	}

	if (!state) {
		error("cannot operate on zero divisor");
		return Qzero;
	}

	if (XASE_RESC_ELM_SMALLP(r)) {
		return ase_make_resc_elm(
			make_int(bigz_to_long(ent_scratch_bigz)), rng);
	} else {
		ase_resc_elm_t e = __ase_make_resc_elm_bigz(ent_scratch_bigz);
		e->smallp = 0;
		e->ring = rng;
		_ase_resc_elm_canonicalise_big(e);
		return _ase_wrap_resc_elm(e);
	}
	return Qnil;
}
#endif

/* relations */
static int
ase_resclass_eq(Lisp_Object l, Lisp_Object r)
{
	ase_resclass_check_rings(l, r);

	if (XASE_RESC_ELM_SMALLP(l)) {
		return (XASE_RESC_ELM_SDATA(l) == XASE_RESC_ELM_SDATA(r));
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else if (!XASE_RESC_ELM_SMALLP(l)) {
		return bigz_eql(XASE_RESC_ELM_DATA(l), XASE_RESC_ELM_DATA(r));
#endif
	}
	return 0;
}

static int
ase_resclass_ne(Lisp_Object l, Lisp_Object r)
{
	ase_resclass_check_rings(l, r);

	if (XASE_RESC_ELM_SMALLP(l)) {
		return (XASE_RESC_ELM_SDATA(l) != XASE_RESC_ELM_SDATA(r));
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else if (!XASE_RESC_ELM_SMALLP(l)) {
		return !bigz_eql(XASE_RESC_ELM_DATA(l), XASE_RESC_ELM_DATA(r));
#endif
	}
	return 1;
}

static int
ase_resclass_zerop(Lisp_Object elm)
{
	if (XASE_RESC_ELM_SMALLP(elm)) {
		return (XASE_RESC_ELM_SDATA(elm) == 0);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else if (!XASE_RESC_ELM_SMALLP(elm)) {
#		define __d	XASE_RESC_ELM_DATA(elm)
		return (bigz_fits_long_p(__d) && bigz_to_long(__d) == 0);
#		undef __d
#endif
	}
	return 0;
}

static int
ase_resclass_onep(Lisp_Object elm)
{
	if (XASE_RESC_ELM_SMALLP(elm)) {
		return (XASE_RESC_ELM_SDATA(elm) == 1);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else if (!XASE_RESC_ELM_SMALLP(elm)) {
#		define __d	XASE_RESC_ELM_DATA(elm)
		return (bigz_fits_long_p(__d) && bigz_to_long(__d) == 1);
#		undef __d
#endif
	}
	return 0;
}

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static int
ase_resclass_unitp(Lisp_Object elm)
{
	Lisp_Object rng = XASE_RESC_ELM_RING(elm);
	int state = 0;


	if (XASE_RESC_ELM_SMALLP(elm)) {
		bigz tmp;
		bigz_set_long(tmp, XASE_RESC_ELM_SDATA(elm));
		bigz_set_long(ent_scratch_bigz, XASE_RESC_RNG_SRING(rng));
		state = mpz_invert(ent_scratch_bigz, tmp, ent_scratch_bigz);
		bigz_init(tmp);
	} else {
		state = mpz_invert(ent_scratch_bigz,
				   XASE_RESC_ELM_DATA(elm),
				   XASE_RESC_RNG_RING(rng));
	}
	return state;
}
#endif


#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
static Lisp_Object
ase_resclass_lift_to_BIGZ_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	if (XASE_RESC_ELM_SMALLP(number)) {
		make_bigz(XASE_RESC_ELM_SDATA(number));
	} else {
		return make_bigz_bz(XASE_RESC_ELM_DATA(number));
	}
	return Qnil;
}
#endif

static Lisp_Object
ase_resclass_lift_to_INT_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	if (XASE_RESC_ELM_SMALLP(number)) {
		return make_int(XASE_RESC_ELM_SDATA(number));
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else {
		return make_int(bigz_to_long(XASE_RESC_ELM_DATA(number)));
#endif
	}
	return Qnil;
}

#ifdef HAVE_FPFLOAT
static Lisp_Object
ase_resclass_lift_to_FLOAT_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	if (XASE_RESC_ELM_SMALLP(number)) {
		return make_float(XASE_RESC_ELM_SDATA(number));
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	} else {
		return make_float(bigz_to_fpfloat(XASE_RESC_ELM_DATA(number)));
#endif
	}
	return Qnil;
}
#endif


static inline void
ent_resclass_nullary_optable_init(void)
{
	ent_nullop_register(ASE_NULLARY_OP_ZERO, INDEF_T, Qzero);
	ent_nullop_register(ASE_NULLARY_OP_ONE, INDEF_T, Qone);
}

static inline void
ent_resclass_unary_optable_init(void)
{
	ase_object_type_t idx = ase_optable_index_typesym(Qase_resc_elm);
	ent_unop_register(ASE_UNARY_OP_NEG, idx, ase_resclass_neg);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	ent_unop_register(ASE_UNARY_OP_INV, idx, ase_resclass_inv);
#endif
}

static inline void
ent_resclass_binary_optable_init(void)
{
	ase_object_type_t idx = ase_optable_index_typesym(Qase_resc_elm);

	ent_binop_register(ASE_BINARY_OP_SUM,
			   idx, idx, ase_resclass_sum);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   idx, INT_T, ase_resclass_sum_intg);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   INT_T, idx, ase_resclass_sum_intg);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   idx, BIGZ_T, ase_resclass_sum_intg);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGZ_T, idx, ase_resclass_sum_intg);

	ent_binop_register(ASE_BINARY_OP_DIFF,
			   idx, idx, ase_resclass_diff);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   idx, INT_T, ase_resclass_diff_intg);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   idx, BIGZ_T, ase_resclass_diff_intg);

	ent_binop_register(ASE_BINARY_OP_PROD,
			   idx, idx, ase_resclass_prod);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   idx, INT_T, ase_resclass_prod_intg);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   INT_T, idx, ase_resclass_prod_intg);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   idx, BIGZ_T, ase_resclass_prod_intg);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGZ_T, idx, ase_resclass_prod_intg);

	ent_binop_register(ASE_BINARY_OP_DIV,
			   idx, idx, ase_resclass_div);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   idx, idx, ase_resclass_div);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   idx, INT_T, ase_resclass_div_INT_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   idx, INT_T, ase_resclass_div_INT_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   INT_T, idx, ase_resclass_INT_T_div);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   INT_T, idx, ase_resclass_INT_T_div);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   idx, BIGZ_T, ase_resclass_div_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   idx, BIGZ_T, ase_resclass_div_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGZ_T, idx, ase_resclass_BIGZ_T_div);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   BIGZ_T, idx, ase_resclass_BIGZ_T_div);

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	ent_binop_register(ASE_BINARY_OP_POW,
			   idx, INT_T, ase_resclass_pow);
	ent_binop_register(ASE_BINARY_OP_POW,
			   idx, BIGZ_T, ase_resclass_pow);
#endif
}

static inline void
ent_resclass_unary_reltable_init(void)
{
	ase_object_type_t idx = ase_optable_index_typesym(Qase_resc_elm);
	ent_unrel_register(ASE_UNARY_REL_ZEROP, idx, ase_resclass_zerop);
	ent_unrel_register(ASE_UNARY_REL_ONEP, idx, ase_resclass_onep);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	ent_unrel_register(ASE_UNARY_REL_UNITP, idx, ase_resclass_unitp);
#endif
}

static inline void
ent_resclass_binary_reltable_init(void)
{
	ase_object_type_t idx = ase_optable_index_typesym(Qase_resc_elm);
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    idx, idx, ase_resclass_eq);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    idx, idx, ase_resclass_ne);
}

static inline void
ent_resclass_lifttable_init(void)
{
	ase_object_type_t idx = ase_optable_index_typesym(Qase_resc_elm);
	ent_lift_register(idx, INT_T, ase_resclass_lift_to_INT_T);
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
	ent_lift_register(idx, BIGZ_T, ase_resclass_lift_to_BIGZ_T);
#endif
#ifdef HAVE_FPFLOAT
	ent_lift_register(idx, FLOAT_T, ase_resclass_lift_to_FLOAT_T);
#endif
}


/* ###autoload */
DEFUN("make-residue-class-ring", Fmake_residue_class_ring, 1, 1, 0, /*
Return a residue class ring of size MODULUS (>= 2).
*/
       (modulus))
{
	CHECK_INTEGER(modulus);
	if (NILP(Fnonnegativep(modulus)))
		error("cannot create ring with negative modulus");
	if (ent_unrel_zerop(modulus))
		error("cannot create ring of size 0");
	if (ent_unrel_onep(modulus))
		error("ring is identical to Z");

	return ase_make_resc_rng(modulus);
}

/* ###autoload */
DEFUN("make-residue-class", Fmake_residue_class, 2, 2, 0, /*
Return the residue class of ELEMENT in RING.
*/
      (element, ring))
{
	CHECK_ASE_RESC_RNG(ring);
	CHECK_INTEGER(element);

	return ase_make_resc_elm(element, ring);
}

/* ###autoload */
DEFUN("residue-class-ring", Fresidue_class_ring, 1, 1, 0, /*
Return the parental residue class ring (the world) of RESCLASS.
*/
      (resclass))
{
	CHECK_ASE_RESC_ELM(resclass);

	return XASE_RESC_ELM_RING(resclass);
}

#if 0
/* ###autoload */
D3FUN("residue-class-modulus", Fresidue_class_modulus, 1, 1, 0, /*
Return the modulus of the residue class ring RING-OR-ELEMENT,
or the modulus of a residue class, respectively.
								*/
       (ring_or_element))
{
	Lisp_Object rng;

	if (!ASE_RESC_ELM_P(ring_or_element) &&
	    !ASE_RESC_RNG_P(ring_or_element)) {
		return wrong_type_argument(Qase_resc_elm_p, ring_or_element);
	}

	if (ASE_RESC_ELM_P(ring_or_element))
		rng = XASE_RESC_ELM_RING(ring_or_element);
	else if (ASE_RESC_RNG_P(ring_or_element))
		rng = ring_or_element;
	else
		return Qzero;

	return make_bigz_bz(XASE_RESC_RNG_RING(rng));
}

/* ###autoload */
D3FUN("residue-class-representant", Fresidue_class_representant, 1, 1, 0, /*
Return the representant of the residue class ELEMENT lifted
to the ring of rational integers.
									  */
       (element))
{
	CHECK_ASE_RESC_ELM(element);

	return make_bigz_bz(XASE_RESC_ELM_DATA(element));
}
#endif

/* ###autoload */
DEFUN ("residue-class-ring-p", Fresidue_class_ring_p, 1, 1, 0, /*
Return t if OBJECT is a residue class ring, nil otherwise.
*/
       (object))
{
	return ASE_RESC_RNG_P(object) ? Qt : Qnil;
}

/* ###autoload */
DEFUN ("residue-class-p", Fresidue_class_p, 1, 1, 0, /*
Return t if OBJECT is a residue class, nil otherwise.
*/
       (object))
{
	return ASE_RESC_ELM_P(object) ? Qt : Qnil;
}

/* from number-to-string */
#ifdef HAVE_RESCLASS
	if (RESC_ELMP(number)) {
		char *estr = (char*)resc_elm_to_string(
			XRESC_ELM_DATA(number), 10);
		char *rstr = (char*)resc_rng_to_string(
			XRESC_RNG_DATA(XRESC_ELM_RING(number)), 10);
		int elen = strlen(estr);
		int rlen = strlen(rstr);
		Lisp_Object result;

		XREALLOC_ARRAY(estr, char, elen+1+rlen+1 + 1);
		strncat(estr, "+", 1);
		strncat(estr, rstr, rlen);
		strncat(estr, "Z", 1);
		result = build_string(estr);
		free(estr);
		free(rstr);
		return result;
	}
#endif

/* from zero-divisor-p */
#ifdef HAVE_RESCLASS
	case RESC_ELM_T: {
		bigz bz;

		bigz_init(bz);
		if (mpz_invert(bz, XRESC_ELM_DATA(number),
			       XRESC_RNG_DATA(XRESC_ELM_RING(number))))
			result = Qnil;
		else
			result = Qt;

		bigz_fini(bz);
		break;
	}
#endif


/* initialiser code */
#define EMODNAME	ase_resclass

static inline void
ase_resclass_binary_optable_init(void)
{
	ent_resclass_nullary_optable_init();
	ent_resclass_unary_optable_init();
	ent_resclass_binary_optable_init();
	ent_resclass_unary_reltable_init();
	ent_resclass_binary_reltable_init();
	ent_resclass_lifttable_init();
}

void
EMOD_PUBINIT(void)
{
	DEFSUBR(Fmake_residue_class_ring);
	DEFSUBR(Fmake_residue_class);
	DEFSUBR(Fresidue_class_ring);
#if 0
	DEFSUBR(Fresidue_class_modulus);
	DEFSUBR(Fresidue_class_representant);
#endif
	DEFSUBR(Fresidue_class_ring_p);
	DEFSUBR(Fresidue_class_p);

	DEFSYMBOL(Qase_resclass);
	DEFASETYPE_WITH_OPS(Qase_resc_rng, "ase:residue-class-ring");
	defsymbol(&Qase_resc_rng_p, "ase:residue-class-ring-p");
	DEFASETYPE_WITH_OPS(Qase_resc_elm, "ase:residue-class");
	defsymbol(&Qase_resc_elm_p, "ase:residue-class-p");

	ase_resclass_binary_optable_init();

	Fprovide(Qase_resclass);
	Fprovide(intern("resclass"));

	EMOD_PUBREINIT();
}

void
EMOD_PUBREINIT(void)
{
	sane_small = (snprintf(NULL, 0, "%ld", EMACS_INT_MAX) + 7) & -3;
	/* defined in lread.c, declared in ent.h */
	ase_resc_rng_pred_f = ase_resc_rng_string_p;
	ase_resc_rng_f = ase_resc_rng_from_string;
	ase_resc_elm_pred_f = ase_resc_elm_string_p;
	ase_resc_elm_f = ase_resc_elm_from_string;
}

void
EMOD_PUBDEINIT(void)
{
	Frevoke(Qase_resclass);
	Frevoke(intern("resclass"));
}

/* ent-resclass.c ends here */
