/*** ase-resclass.h -- Residue Class Rings for SXEmacs
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

#ifndef INCLUDED_ase_resclass_h_
#define INCLUDED_ase_resclass_h_ 1

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

#include "ase.h"
#ifdef HAVE_MPZ
#include "gmp.h"
#endif

#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
typedef bigz resc_rng;
typedef bigz resc_elm;
#else
typedef EMACS_INT resc_rng;
typedef EMACS_INT resc_elm;
#endif

extern Lisp_Object Qase_resc_rng, Qase_resc_rng_p;
extern Lisp_Object Qase_resc_elm, Qase_resc_elm_p;

extern void LTX_PUBINIT(ase_resclass)(void);
extern void LTX_PUBREINIT(ase_resclass)(void);
extern void LTX_PUBDEINIT(ase_resclass)(void);

typedef struct ase_resc_rng_s *ase_resc_rng_t;
typedef struct ase_resc_elm_s *ase_resc_elm_t;


struct ase_resc_rng_s {
	int smallp;
	resc_rng ring;
	EMACS_INT small_ring;
};

struct ase_resc_elm_s {
	int smallp;
	resc_elm data;
	EMACS_INT small_data;
	Lisp_Object ring;
};


#define ASE_RESC_RNG_P(_i)					\
	(DYNACATP(_i) && EQ(XDYNACAT_TYPE(_i), Qase_resc_rng))
#define CHECK_ASE_RESC_RNG(x)						\
	do {								\
		if (!ASE_RESC_RNG_P(x))					\
			dead_wrong_type_argument(Qase_resc_rng_p, x);	\
	} while (0)
#define CONCHECK_ASE_RESC_RNG(x)					\
	do {								\
		if (!ASE_RESC_RNG_P(x))					\
			x = wrong_type_argument(Qase_resc_rng_p, x);	\
	} while (0)
#define XSETASE_RESC_RNG(_res, _rng)	(_res) = _ase_wrap_resc_rng((_rng))
#define XASE_RESC_RNG(_x)		((ase_resc_rng_t)get_dynacat(_x))

#define ASE_RESC_ELM_P(_i)					\
	(DYNACATP(_i) && EQ(XDYNACAT_TYPE(_i), Qase_resc_elm))
#define CHECK_ASE_RESC_ELM(x)						\
	do {								\
		if (!ASE_RESC_ELM_P(x))					\
			dead_wrong_type_argument(Qase_resc_elm_p, x);	\
	} while (0)
#define CONCHECK_ASE_RESC_ELM(x)					\
	do {								\
		if (!ASE_RESC_ELM_P(x))					\
			x = wrong_type_argument(Qase_resc_elm_p, x);	\
	} while (0)
extern Lisp_Object _ase_wrap_resc_elm(ase_resc_elm_t);
#define XSETASE_RESC_ELM(_res, _elm)	(_res) = _ase_wrap_resc_elm((_elm))
#define XASE_RESC_ELM(_x)		((ase_resc_elm_t)get_dynacat(_x))

#define ase_resc_rng_smallp(_r)		((_r)->smallp)
#define ase_resc_rng_ring(_r)		((_r)->ring)
#define ase_resc_rng_sring(_r)		((_r)->small_ring)
#define XASE_RESC_RNG_RING(_r)		ase_resc_rng_ring(XASE_RESC_RNG(_r))
#define XASE_RESC_RNG_SRING(_r)		ase_resc_rng_sring(XASE_RESC_RNG(_r))
#define XASE_RESC_RNG_SMALLP(_r)	ase_resc_rng_smallp(XASE_RESC_RNG(_r))

#define ase_resc_elm_smallp(_e)		((_e)->smallp)
#define ase_resc_elm_sdata(_e)		((_e)->small_data)
#define ase_resc_elm_data(_e)		((_e)->data)
#define ase_resc_elm_ring(_e)		((_e)->ring)
#define XASE_RESC_ELM_DATA(_e)		ase_resc_elm_data(XASE_RESC_ELM(_e))
#define XASE_RESC_ELM_SDATA(_e)		ase_resc_elm_sdata(XASE_RESC_ELM(_e))
#define XASE_RESC_ELM_RING(_e)		ase_resc_elm_ring(XASE_RESC_ELM(_e))
#define XASE_RESC_ELM_SMALLP(_e)	ase_resc_elm_smallp(XASE_RESC_ELM(_e))


/***** basic functions *****/
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
#define resc_rng_init(r)		bigz_init(r)
#define resc_rng_fini(r)		bigz_fini(r)
#define resc_elm_init(e)		bigz_init(e)
#define resc_elm_fini(e)		bigz_fini(e)
#define resc_rng_set_string(_to, _cp)	bigz_set_string(_to, _cp, 0)
#define resc_elm_set_string(_to, _cp)	bigz_set_string(_to, _cp, 0)
#define resc_rng_set_eint(_r, _m)	bigz_set_long(_r, _m)
#define resc_rng_set_bigz(_r, _m)	bigz_set(_r, _m)
#define resc_elm_set_eint(_r, _m)	bigz_set_long(_r, _m)
#define resc_elm_set_bigz(_r, _m)	bigz_set(_r, _m)
#else
#define resc_rng_init(r)
#define resc_rng_fini(r)
#define resc_elm_init(e)
#define resc_elm_fini(e)
#define resc_rng_set_string(_to, _cp)	_to = strtol(_cp, NULL, 10)
#define resc_elm_set_string(_to, _cp)	_to = strtol(_cp, NULL, 10)
#define resc_rng_set_eint(_r, _m)	_r = _m
#define resc_rng_set_bigz(_r, _m)
#define resc_elm_set_eint(_r, _m)	_r = _m
#define resc_elm_set_bigz(_r, _m)
#endif

/***** conversions *****/
#if defined HAVE_MPZ && (defined WITH_GMP || defined WITH_MP)
#define resc_rng_to_string(_p, _n, _r)	mpz_get_str(_p, 10, _r)
#define resc_elm_to_string(_p, _n, _e)	mpz_get_str(_p, 10, _e)
#else
#define resc_rng_to_string(_p, _n, _r)	do { int sz = snprintf(_p, _n, "%ld", _r); assert(sz>=0 && sz<_n); } while(0)
#define resc_elm_to_string(_p, _n, _e)	do { int sz = snprintf(_p, _n, "%ld", _e); assert(sz>=0 && sz<_n); } while(0)
#endif


extern Lisp_Object read_resclass_string(char*);
extern Lisp_Object read_resclassring_string(char*);
extern Lisp_Object internal_coerce_to_RESC_ELM_T(Lisp_Object, Lisp_Object);

extern Lisp_Object ase_make_resc_rng(Lisp_Object);
extern void _ase_resc_rng_prnt(ase_resc_rng_t, Lisp_Object);
extern void _ase_resc_elm_prnt(ase_resc_elm_t, Lisp_Object);

extern ase_resc_rng_t _ase_make_resc_rng(Lisp_Object);
extern Lisp_Object ase_make_resc_rng(Lisp_Object);
extern ase_resc_elm_t _ase_make_resc_elm(Lisp_Object, Lisp_Object);
extern Lisp_Object ase_make_resc_elm(Lisp_Object, Lisp_Object);


#endif /* INCLUDED_ase_resclass_h_ */
