/*
  ent-resclass.c -- Residue Class Rings for SXEmacs
  Copyright (C) 2005 Sebastian Freundt

  Author:  Sebastian Freundt

  * This file is part of SXEmacs.
  * 
  * SXEmacs is free software; you can redistribute it and/or modify it
  * under the terms of the GNU General Public License as published by the
  * Free Software Foundation; either version 2, or (at your option) any
  * later version.
  * 
  * SXEmacs is distributed in the hope that it will be useful, but WITHOUT
  * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  * for more details.
  * 
  * You should have received a copy of the GNU General Public License
  * along with SXEmacs; see the file COPYING.  If not, write to
  * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  * Boston, MA 02111-1307, USA.
  */

#include <config.h>
#include <limits.h>
#include <math.h>
#include "lisp.h"
#include "sysproc.h"    /* For qxe_getpid */

#include "ent-resclass.h"


/* basic functions */

/* read a resclass off the wire */
Lisp_Object read_resclass_string(char *cp)
{
	resc_elm rc_e;
	resc_rng rc_r;
	Lisp_Object result;
	Bufbyte *end;
	Bufbyte tmp;

	resc_elm_init(rc_e);
	resc_rng_init(rc_r);

	/* MPZ bigz_set_string has no effect
	 * with initial + sign */
	if (*cp == '+')
		cp++;

	end = (Bufbyte *)cp;

	if (*cp == '-') {
		/* jump over a leading minus */
		cp++;
	}
		
	while ((*cp >= '0' && *cp <= '9'))
		cp++;

	/* MPZ cannot read numbers with characters after them.
	 * See limitations of GMP-MPZ strings
	 */
	tmp = (Bufbyte)*cp;
	*cp = '\0';
	bigz_set_string(rc_e, (char *)end, 0);
	*cp = tmp;

	/* read the modulus */
	if (*cp == '+')
		cp++;
	end = (Bufbyte*)cp;
	while ((*cp >= '0' &&
		*cp <= '9'))
		cp++;
	tmp = (Bufbyte)*cp;
	*cp = '\0';
	bigz_set_string(rc_r, (char *)end, 0);
	*cp = tmp;

	/* generate the ring */
	result = make_resc_rng_bz(rc_r);
	/* generate the class (using the ring just created) */
	result = make_resc_elm_bz(rc_e, result);

	resc_rng_fini(rc_r);
	resc_elm_fini(rc_e);
	return result;
}

Lisp_Object read_resclassring_string(char *cp)
{
	resc_rng rc_r;
	Lisp_Object result;
	Bufbyte *end;
	Bufbyte tmp;

	resc_rng_init(rc_r);

	/* Jump over Z */
	cp++;
	/* Jump over / */
	cp++;

	end = (Bufbyte *)cp;

	while ((*cp >= '0' && *cp <= '9'))
		cp++;

	/* MPZ cannot read numbers with characters after them.
	 * See limitations of GMP-MPZ strings
	 */
	tmp = (Bufbyte)*cp;
	*cp = '\0';
	bigz_set_string(rc_r, (char *)end, 0);
	*cp = tmp;

	/* generate the ring */
	result = make_resc_rng_bz(rc_r);

	resc_rng_fini(rc_r);
	return result;
}


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

static Lisp_Object ent_sum_RESC_ELM_T(Lisp_Object l, Lisp_Object r)
{
	Lisp_Object result_ring;

	result_ring = ent_intersection_factor_module(&l, &r);

	bigz_add(ent_scratch_bigz,
		 XRESC_ELM_DATA(l), XRESC_ELM_DATA(r));
	return make_resc_elm_bz(ent_scratch_bigz, result_ring);
}
static Lisp_Object ent_sum_RESC_ELM_T_integer(Lisp_Object l, Lisp_Object r)
{
	if (INTP(r))
		bigz_set_long(ent_scratch_bigz, XINT(r));
	else if (BIGZP(r))
		bigz_set(ent_scratch_bigz, XBIGZ_DATA(r));
	else
		Fsignal(Qoperation_error, r);

	bigz_add(ent_scratch_bigz,
		 XRESC_ELM_DATA(l), ent_scratch_bigz);
	return make_resc_elm_bz(ent_scratch_bigz, XRESC_ELM_RING(l));
}
static Lisp_Object ent_diff_RESC_ELM_T(Lisp_Object l, Lisp_Object r)
{
	Lisp_Object result_ring;

	result_ring = ent_intersection_factor_module(&l, &r);

	bigz_sub(ent_scratch_bigz,
		 XRESC_ELM_DATA(l), XRESC_ELM_DATA(r));
	return make_resc_elm_bz(ent_scratch_bigz, result_ring);
}
static Lisp_Object ent_diff_RESC_ELM_T_integer(Lisp_Object l, Lisp_Object r)
{
	if (INTP(r))
		bigz_set_long(ent_scratch_bigz, XINT(r));
	else if (BIGZP(r))
		bigz_set(ent_scratch_bigz, XBIGZ_DATA(r));
	else
		Fsignal(Qoperation_error, r);

	bigz_sub(ent_scratch_bigz,
		 XRESC_ELM_DATA(l), ent_scratch_bigz);
	return make_resc_elm_bz(ent_scratch_bigz, XRESC_ELM_RING(l));
}
static Lisp_Object ent_neg_RESC_ELM_T(Lisp_Object l)
{
	bigz_neg(ent_scratch_bigz, XRESC_ELM_DATA(l));
	return make_resc_elm_bz(ent_scratch_bigz, XRESC_ELM_RING(l));
}
static Lisp_Object ent_prod_RESC_ELM_T(Lisp_Object l, Lisp_Object r)
{
	Lisp_Object result_ring;

	result_ring = ent_intersection_factor_module(&l, &r);

	bigz_mul(ent_scratch_bigz,
		 XRESC_ELM_DATA(l), XRESC_ELM_DATA(r));
	return make_resc_elm_bz(ent_scratch_bigz, result_ring);
}
static Lisp_Object ent_div_RESC_ELM_T(Lisp_Object l, Lisp_Object r)
{
	Lisp_Object ring;
	Lisp_Object result_ring;

	result_ring = ent_intersection_factor_module(&l, &r);

	ring = XRESC_ELM_RING(r);
	if (!mpz_invert(ent_scratch_bigz,
			XRESC_ELM_DATA(r), XRESC_RNG_DATA(ring))) {
		/* Fsignal(Qarith_error, l2); */
		error("cannot operate on zero divisor");
		return Qzero;
	}
	bigz_mul(ent_scratch_bigz, XRESC_ELM_DATA(l), ent_scratch_bigz);
	return make_resc_elm_bz(ent_scratch_bigz, result_ring);
}
static Lisp_Object ent_inv_RESC_ELM_T(Lisp_Object r)
{
	Lisp_Object ring;

	ring = XRESC_ELM_RING(r);
	if (!mpz_invert(ent_scratch_bigz,
			XRESC_ELM_DATA(r), XRESC_RNG_DATA(ring))) {
		/* Fsignal(Qarith_error, l); */
		error("cannot operate on zero divisor");
		return Qzero;
	}
	return make_resc_elm_bz(ent_scratch_bigz, ring);
}
static Lisp_Object ent_pow_RESC_ELM_T_integer(Lisp_Object l, Lisp_Object r)
{
	unsigned long expo = 0;

	if (INTP(r))
		expo = XREALINT(r);
	else if (BIGZP(r)) {
		if (bigz_fits_ulong_p(XBIGZ_DATA(r)))
			expo = bigz_to_ulong(XBIGZ_DATA(r));
		else
			Fsignal(Qarith_error, r);
	} else
		Fsignal(Qoperation_error, r);

	bigz_pow(ent_scratch_bigz, XRESC_ELM_DATA(l), expo);
	return make_resc_elm_bz(ent_scratch_bigz, XRESC_ELM_RING(l));
}

/* relations */
static Lisp_Object ent_eq_RESC_ELM_T(Lisp_Object l, Lisp_Object r)
{
	if (!bigz_eql(XRESC_RNG_DATA(XRESC_ELM_RING(l)),
		      XRESC_RNG_DATA(XRESC_ELM_RING(r))))
		Fsignal(Qrelation_error, Qnil);

	return bigz_eql(XRESC_ELM_DATA(l), XRESC_ELM_DATA(r))
		? Qt : Qnil;
}
static Lisp_Object ent_ne_RESC_ELM_T(Lisp_Object l, Lisp_Object r)
{
	if (!bigz_eql(XRESC_RNG_DATA(XRESC_ELM_RING(l)),
		      XRESC_RNG_DATA(XRESC_ELM_RING(r))))
		Fsignal(Qrelation_error, Qnil);

	return bigz_eql(XRESC_ELM_DATA(l), XRESC_ELM_DATA(r))
		? Qnil : Qt;
}
static Lisp_Object ent_valeq_RESC_ELM_T(Lisp_Object l, Lisp_Object r)
{
	if (!bigz_eql(XRESC_RNG_DATA(XRESC_ELM_RING(l)),
		      XRESC_RNG_DATA(XRESC_ELM_RING(r))))
		Fsignal(Qrelation_error, Qnil);

	/* trivially true */
	return Qt;
}
static Lisp_Object ent_valne_RESC_ELM_T(Lisp_Object l, Lisp_Object r)
{
	if (!bigz_eql(XRESC_RNG_DATA(XRESC_ELM_RING(l)),
		      XRESC_RNG_DATA(XRESC_ELM_RING(r))))
		Fsignal(Qrelation_error, Qnil);

	/* trivially nil */
	return Qnil;
}


static Lisp_Object
ent_lift_RESC_ELM_T_BIGZ_T(Lisp_Object number, unsigned long precision)
{
	return make_bigz_bz(XRESC_ELM_DATA(number));
}

static Lisp_Object
ent_lift_RESC_ELM_T_INT_T(Lisp_Object number, unsigned long precision)
{
	return make_int(bigz_to_long(XRESC_ELM_DATA(number)));
}

#ifdef LISP_FLOAT_TYPE
static Lisp_Object
ent_lift_RESC_ELM_T_FLOAT_T(Lisp_Object number, unsigned long precision)
{
	return make_float(bigz_to_double(XRESC_ELM_DATA(number)));
}
#endif


void init_optables_RESC_ELM_T(void)
{
	ent_optable_sum[RESC_ELM_T][RESC_ELM_T] = ent_sum_RESC_ELM_T;
	ent_optable_sum[RESC_ELM_T][INT_T] = ent_sum_RESC_ELM_T_integer;
	ent_optable_sum[RESC_ELM_T][BIGZ_T] = ent_sum_RESC_ELM_T_integer;
	ent_optable_diff[RESC_ELM_T][RESC_ELM_T] = ent_diff_RESC_ELM_T;
	ent_optable_diff[RESC_ELM_T][INT_T] = ent_diff_RESC_ELM_T_integer;
	ent_optable_diff[RESC_ELM_T][BIGZ_T] = ent_diff_RESC_ELM_T_integer;
	ent_optable_neg[RESC_ELM_T] = ent_neg_RESC_ELM_T;
	ent_optable_prod[RESC_ELM_T][RESC_ELM_T] = ent_prod_RESC_ELM_T;
	ent_optable_div[RESC_ELM_T][RESC_ELM_T] = ent_div_RESC_ELM_T;
	ent_optable_inv[RESC_ELM_T] = ent_inv_RESC_ELM_T;
	ent_optable_quo[RESC_ELM_T][RESC_ELM_T] = ent_div_RESC_ELM_T;
	ent_optable_pow[RESC_ELM_T][INT_T] = ent_pow_RESC_ELM_T_integer;
	ent_optable_pow[RESC_ELM_T][BIGZ_T] = ent_pow_RESC_ELM_T_integer;

	ent_optable_eq[RESC_ELM_T][RESC_ELM_T] = ent_eq_RESC_ELM_T;
	ent_optable_ne[RESC_ELM_T][RESC_ELM_T] = ent_ne_RESC_ELM_T;
	ent_optable_valeq[RESC_ELM_T][RESC_ELM_T] = ent_valeq_RESC_ELM_T;
	ent_optable_valne[RESC_ELM_T][RESC_ELM_T] = ent_valne_RESC_ELM_T;

	/* lift tables (coercion) */
	ent_optable_lift[RESC_ELM_T][INT_T] = ent_lift_RESC_ELM_T_INT_T;
	ent_optable_lift[RESC_ELM_T][BIGZ_T] = ent_lift_RESC_ELM_T_BIGZ_T;
#ifdef LISP_FLOAT_TYPE
	ent_optable_lift[RESC_ELM_T][FLOAT_T] = ent_lift_RESC_ELM_T_FLOAT_T;
#endif
}

void init_ent_resclass(void)
{
}


DEFUN("make-residue-class-ring", Fmake_residue_class_ring, 1, 1, 0, /*
Return a residue class ring of size MODULUS (>= 2).
								    */
       (modulus))
{
	CHECK_INTEGER(modulus);
	if (NILP(Fnonnegativep(modulus)))
		error("cannot create ring with negative modulus");
	if (!NILP(Fzerop(modulus)))
		error("cannot create ring of size 0");
	if (!NILP(Fonep(modulus)))
		error("ring is identical to Z");

	if (INTP(modulus))
		return make_resc_rng(XINT(modulus));
	else if (BIGZP(modulus))
		return make_resc_rng_bz(XBIGZ_DATA(modulus));
	else
		error("cannot create ring");
}

DEFUN("make-residue-class", Fmake_residue_class, 2, 2, 0, /*
Return the residue class of ELEMENT in RING.
							  */
      (element, ring))
{
	CHECK_RESC_RNG(ring);
	CHECK_INTEGER(element);

	if (INTP(element))
		return make_resc_elm(XINT(element), ring);
	else if (BIGZP(element))
		return make_resc_elm_bz(XBIGZ_DATA(element), ring);
	else
		error("cannot create class");
}

DEFUN("residue-class-ring", Fresidue_class_ring, 1, 1, 0, /*
Return the parental residue class ring (the world) of RESCLASS.
							  */
      (resclass))
{
	CHECK_RESC_ELM(resclass);

	return XRESC_ELM_RING(resclass);
}

DEFUN("residue-class-modulus", Fresidue_class_modulus, 1, 1, 0, /*
Return the modulus of the residue class ring RING-OR-ELEMENT,
or the modulus of a residue class, respectively.
								*/
       (ring_or_element))
{
	Lisp_Object rng;

	if (!(RESC_ELMP(ring_or_element) ||
	      RESC_RNGP(ring_or_element)))
		return wrong_type_argument(Qresc_elmp, ring_or_element);

	if (RESC_ELMP(ring_or_element))
		rng = XRESC_ELM_RING(ring_or_element);
	else if (RESC_RNGP(ring_or_element))
		rng = ring_or_element;
	else
		return Qzero;

	return make_bigz_bz(XRESC_RNG_DATA(rng));
}

DEFUN("residue-class-representant", Fresidue_class_representant, 1, 1, 0, /*
Return the representant of the residue class ELEMENT lifted 
to the ring of rational integers.
									  */
       (element))
{
	CHECK_RESC_ELM(element);

	return make_bigz_bz(XRESC_ELM_DATA(element));
}



void syms_of_ent_resclass(void)
{
	DEFSUBR(Fmake_residue_class_ring);
	DEFSUBR(Fmake_residue_class);
	DEFSUBR(Fresidue_class_ring);
	DEFSUBR(Fresidue_class_modulus);
	DEFSUBR(Fresidue_class_representant);
}


/* ent-resclass.c ends here */
