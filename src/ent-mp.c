/*
  number.c -- Numeric types for SXEmacs
  Copyright (C) 2004 Jerry James

  Author:  Jerry James
  Backport:  Sebastian Freundt

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

static MINT *bigz_bytesize, *bigz_long_sign_bit, *bigz_one, *bigz_two;
MINT *bigz_zero, *intern_bigz;
MINT *bigz_min_int, *bigz_max_int, *bigz_max_uint;
MINT *bigz_min_long, *bigz_max_long, *bigz_max_ulong;
short div_rem;

bigz ent_scratch_bigz;


/************************* Big Rational Integers ****************************/
static void
bigz_print (Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Bufbyte *bstr;

	bstr = (Bufbyte*)bigz_to_string(XBIGZ_DATA(obj), 10);
	write_c_string((char *)bstr, printcharfun);
	free(bstr);
	bstr = (Bufbyte *)NULL;

	/* less warnings */
	if (escapeflag);
}

static int
bigz_equal (Lisp_Object obj1, Lisp_Object obj2, int depth)
{
	return bigz_eql(XBIGZ_DATA(obj1), XBIGZ_DATA(obj2));

	/* less warnings */
	if (depth);
}

static unsigned long
bigz_hash (Lisp_Object obj, int depth)
{
	return (unsigned long)bigz_hashcode(XBIGZ_DATA(obj));

	/* less warnings */
	if (depth);
}

static const struct lrecord_description bigz_description[] = {
        { XD_OPAQUE_DATA_PTR, offsetof(Lisp_Bigz, data) },
	{ XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("bigz", bigz,
				    NULL, bigz_print, NULL,
				    bigz_equal, bigz_hash,
				    bigz_description, Lisp_Bigz);


char *bigz_to_string (bigz b, int base)
{
	REGISTER unsigned int i;
	unsigned int bufsize = 128U, idx = 0U;
	int sign;
	char *buffer = xnew_array (char, 128), *retval;
	MINT *quo = MP_ITOM(0);
	short rem;

	/* FIXME: signal something if base is < 2 or doesn't fit into a short. */

	/* Save the sign for later */
	sign = MP_MCMP(b, bigz_zero);

	if (sign == 0) {
		XREALLOC_ARRAY(buffer, char, 2);
		buffer[0] = '0';
		buffer[1] = '\0';
		return buffer;
	}
	/* Copy abs(b) into quo for destructive modification */
	else if (sign < 0)
		MP_MSUB(bigz_zero, b, quo);
	else
		MP_MOVE(b, quo);

	quo = MP_ITOM(0);

	/* Loop over the digits of b (in BASE) and place each one into buffer */
	for (i = 0U; MP_MCMP(quo, bigz_zero) > 0; i++) {
		MP_SDIV(quo, base, quo, &rem);
		if (idx == bufsize) {
			bufsize <<= 1;
			XREALLOC_ARRAY(buffer, char, bufsize);
		}
		buffer[idx++] = rem < 10 ? rem + '0' : rem - 10 + 'a';
	}
	MP_MFREE (quo);

	/* Reverse the digits, maybe add a minus sign, and add a null terminator */
	bufsize = idx + (sign < 0 ? 1 : 0) + 1;
	retval = xnew_array (char, bufsize);
	if (sign < 0) {
		retval[0] = '-';
		i = 1;
	} else
		i = 0;
	for (; i < bufsize - 1; i++)
		retval[i] = buffer[--idx];
	retval[bufsize - 1] = '\0';
	xfree(buffer);
	return retval;
}

#define BIGZ_TO_TYPE(type,accumtype) do {			\
		MP_MULT(b, quo, quo);				\
		for (i = 0U; i < sizeof(type); i++) {		\
			MP_SDIV(quo, 256, quo, &rem);		\
			retval |= ((accumtype) rem) << (8 * i);	\
		}						\
		MP_MFREE(quo);					\
	} while (0)

int
bigz_to_int (bigz b)
{
	short rem, sign;
	unsigned int retval = 0;
	REGISTER unsigned int i;
	MINT *quo;

	sign = MP_MCMP (b, bigz_zero) < 0 ? -1 : 1;
	quo = MP_ITOM (sign);
	BIGZ_TO_TYPE (int, unsigned int);
	return ((int) retval) * sign;
}

unsigned int
bigz_to_uint (bigz b)
{
	short rem;
	unsigned int retval = 0U;
	REGISTER unsigned int i;
	MINT *quo;

	quo = MP_ITOM (MP_MCMP (b, bigz_zero) < 0 ? -1 : 1);
	BIGZ_TO_TYPE (unsigned int, unsigned int);
	return retval;
}

long
bigz_to_long (bigz b)
{
	short rem, sign;
	unsigned long retval = 0L;
	REGISTER unsigned int i;
	MINT *quo;

	sign = MP_MCMP (b, bigz_zero) < 0 ? -1 : 1;
	quo = MP_ITOM (sign);
	BIGZ_TO_TYPE (long, unsigned long);
	return ((long) retval) * sign;
}

unsigned long
bigz_to_ulong (bigz b)
{
	short rem;
	unsigned long retval = 0UL;
	REGISTER unsigned int i;
	MINT *quo;

	quo = MP_ITOM (MP_MCMP (b, bigz_zero) < 0 ? -1 : 1);
	BIGZ_TO_TYPE (unsigned long, unsigned long);
	return retval;
}

fpfloat
bigz_to_fpfloat(bigz b)
{
	short rem, sign;
	fpfloat retval = 0.0, factor = 1.0;
	REGISTER unsigned int i;
	MINT *quo;

	sign = MP_MCMP (b, bigz_zero) < 0 ? -1 : 1;
	quo = MP_ITOM (sign);
	MP_MULT (b, quo, quo);
	for (i = 0U; MP_MCMP (quo, bigz_zero) > 0; i++)
	{
		MP_SDIV (quo, 256, quo, &rem);
		retval += rem * factor;
		factor *= 256.0;
	}
	MP_MFREE (quo);
	return retval * sign;
}

static short
char_to_number (char c)
{
	if (c >= '0' && c <= '9')
		return c - '0';
	if (c >= 'a' && c <= 'z')
		return c - 'a' + 10;
	if (c >= 'A' && c <= 'Z')
		return c - 'A' + 10;
	return -1;
}

int
bigz_set_string (bigz b, const char *s, int base)
{
	MINT *mbase;
	short digit;
	int neg = 0;

	if (base == 0) {
		if (s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
			base = 16;
			s += 2;
		} else if (*s == '0') {
			base = 8;
			s++;
		} else
			base = 10;
	}

	/* FIXME: signal something if base is < 2 or doesn't fit into a short. */

	if (*s == '-') {
		s++;
		neg = 1;
	}

	mbase = MP_ITOM((short)base);
	MP_MOVE(bigz_zero, b);
  
	for (digit = char_to_number(*s); digit >= 0 && digit < base;
	     digit = char_to_number(*++s)) {
		MINT *temp;

		MP_MULT(b, mbase, b);
		temp = MP_ITOM(digit);
		MP_MADD(b, temp, b);
		MP_MFREE(temp);
	}

	if (neg)
		MP_MSUB(bigz_zero, b, b);

	return (digit >= 0) ? -1 : 0;
}

void
bigz_set_long (MINT *b, long l)
{
	/* Negative l is hard, not least because -LONG_MIN == LONG_MIN.  We pretend
	   that l is unsigned, then subtract off the amount equal to the sign bit. */
	bigz_set_ulong (b, (unsigned long) l);
	if (l < 0L)
		MP_MSUB (b, bigz_long_sign_bit, b);
}

void
bigz_set_ulong (bigz b, unsigned long l)
{
	REGISTER unsigned int i;
	MINT *multiplier = MP_ITOM (1);

	MP_MOVE (bigz_zero, b);
	for (i = 0UL; l > 0UL; l >>= 8, i++)
	{
		MINT *temp = MP_ITOM ((short) (l & 255));
		MP_MULT (multiplier, temp, temp);
		MP_MADD (b, temp, b);
		MP_MULT (multiplier, bigz_bytesize, multiplier);
		MP_MFREE (temp);
	}
	MP_MFREE (multiplier);
}

void
bigz_set_fpfloat(bigz b, fpfloat d)
{
	REGISTER unsigned int i;
	int negative = (d < 0) ? 1 : 0;
	MINT *multiplier = MP_ITOM (1);

	MP_MOVE (bigz_zero, b);
	if (negative)
		d = -d;
	for (i = 0UL; d > 0.0; d /= 256, i++)
	{
		MINT *temp = MP_ITOM ((short) fmod (d, 256.0));
		MP_MULT (multiplier, temp, temp);
		MP_MADD (b, temp, b);
		MP_MULT (multiplier, bigz_bytesize, multiplier);
		MP_MFREE (temp);
	}
	MP_MFREE (multiplier);
	if (negative)
		MP_MSUB (bigz_zero, b, b);
}

/* Return nonzero if b1 is exactly divisible by b2 */
int
bigz_divisible_p (bigz b1, bigz b2)
{
	int retval;
	MINT *rem = MP_ITOM (0);
	MP_MDIV (b1, b2, intern_bigz, rem);
	retval = (MP_MCMP (rem, bigz_zero) == 0);
	MP_MFREE (rem);
	return retval;
}

void bigz_ceil (bigz quotient, bigz N, bigz D)
{
	MP_MDIV (N, D, quotient, intern_bigz);
	if (MP_MCMP (intern_bigz, bigz_zero) > 0 &&
	    MP_MCMP (quotient, bigz_zero) > 0)
		MP_MADD (quotient, bigz_one, quotient);
}

void bigz_floor (bigz quotient, bigz N, bigz D)
{
	MP_MDIV (N, D, quotient, intern_bigz);
	if (MP_MCMP (intern_bigz, bigz_zero) > 0 &&
	    MP_MCMP (quotient, bigz_zero) < 0)
		MP_MSUB (quotient, bigz_one, quotient);
}

/* RESULT = N to the POWth power */
void
bigz_pow (bigz result, bigz n, unsigned long pow)
{
	MP_MOVE (bigz_one, result);
	for ( ; pow > 0UL; pow--)
		MP_MULT (result, n, result);
}
void
bigz_ui_pow (bigz result, unsigned long n, unsigned long pow)
{
	MP_MOVE (bigz_one, result);
	for ( ; pow > 0UL; pow--)
		MP_MULT (result, n, result);
}

/* lcm(b1,b2) = b1 * b2 / gcd(b1, b2) */
void
bigz_lcm (bigz result, bigz b1, bigz b2)
{
	MP_MULT (b1, b2, result);
	MP_GCD (b1, b2, intern_bigz);
	MP_MDIV (result, intern_bigz, result, intern_bigz);
}

/* FIXME: We can't handle negative args, so right now we just make them
   positive before doing anything else.  How should we really handle negative
   args? */
#define bigz_bit_op(result, b1, b2, op)					\
	REGISTER unsigned int i;					\
	MINT *multiplier = MP_ITOM (1), *n1 = MP_ITOM (0), *n2 = MP_ITOM (0); \
									\
	if (MP_MCMP (bigz_zero, b1) > 0)				\
		MP_MSUB (bigz_zero, b1, n1);				\
	else								\
		MP_MOVE (b1, n1);					\
	if (MP_MCMP (bigz_zero, b2) > 0)				\
		MP_MSUB (bigz_zero, b2, n2);				\
	else								\
		MP_MOVE (b2, n2);					\
									\
	MP_MOVE (bigz_zero, result);					\
									\
	for (i = 0UL; MP_MCMP (bigz_zero, n1) < 0 &&			\
		     MP_MCMP (bigz_zero, n2) < 0; i++)			\
	{								\
		short byte1, byte2;					\
		MINT *temp;						\
									\
		MP_SDIV (n1, 256, n1, &byte1);				\
		MP_SDIV (n2, 256, n2, &byte2);				\
		temp = MP_ITOM (byte1 op byte2);			\
		MP_MULT (multiplier, temp, temp);			\
		MP_MADD (result, temp, result);				\
		MP_MULT (multiplier, bigz_bytesize, multiplier);	\
		MP_MFREE (temp);					\
	}								\
	MP_MFREE (n2);							\
	MP_MFREE (n1);							\
	MP_MFREE (multiplier)

void
bigz_and (bigz result, bigz b1, bigz b2)
{
	bigz_bit_op (result, b1, b2, &);
}

void
bigz_ior (bigz result, bigz b1, bigz b2)
{
	bigz_bit_op (result, b1, b2, |);
}

void
bigz_xor (bigz result, bigz b1, bigz b2)
{
	bigz_bit_op (result, b1, b2, ^);
}

/* NOT is not well-defined for bigzs ... where do you stop flipping bits?
   We just flip until we see the last one.  This is probably a bad idea. */
void
bigz_not (bigz result, bigz b)
{
	REGISTER unsigned int i;
	MINT *multiplier = MP_ITOM (1), *n = MP_ITOM (0);

	if (MP_MCMP (bigz_zero, b) > 0)
		MP_MSUB (bigz_zero, b, n);
	else
		MP_MOVE (b, n);

	MP_MOVE (bigz_zero, result);

	for (i = 0UL; MP_MCMP (bigz_zero, n) < 0; i++)
	{
		short byte;
		MINT *temp;

		MP_SDIV (n, 256, n, &byte);
		temp = MP_ITOM (~byte);
		MP_MULT (multiplier, temp, temp);
		MP_MADD (result, temp, result);
		MP_MULT (multiplier, bigz_bytesize, multiplier);
		MP_MFREE (temp);
	}
	MP_MFREE (n);
	MP_MFREE (multiplier);
}

void
bigz_setbit (bigz b, unsigned long bit)
{
	bigz_pow (intern_bigz, bigz_two, bit);
	bigz_ior (b, b, intern_bigz);
}

/* This is so evil, even I feel queasy. */
void
bigz_clrbit (bigz b, unsigned long bit)
{
	MINT *num = MP_ITOM (0);

	/* See if the bit is already set, and subtract it off if not */
	MP_MOVE (b, intern_bigz);
	bigz_pow (num, bigz_two, bit);
	bigz_ior (intern_bigz, intern_bigz, num);
	if (MP_MCMP (b, intern_bigz) == 0)
		MP_MSUB (b, num, b);
	MP_MFREE (num);
}

int
bigz_testbit (bigz b, unsigned long bit)
{
	bigz_pow (intern_bigz, bigz_two, bit);
	bigz_and (intern_bigz, b, intern_bigz);
	return MP_MCMP (intern_bigz, bigz_zero);
}

void
bigz_lshift (bigz result, bigz b, unsigned long bits)
{
	bigz_pow (intern_bigz, bigz_two, bits);
	MP_MULT (b, intern_bigz, result);
}

void
bigz_rshift (bigz result, bigz b, unsigned long bits)
{
	bigz_pow (intern_bigz, bigz_two, bits);
	MP_MDIV (b, intern_bigz, result, intern_bigz);
}

void bigz_random_seed(unsigned long seed)
{
	/* FIXME: Implement me */
}

void bigz_random(bigz result, bigz limit)
{
	/* FIXME: Implement me */
	MP_MOVE (bigz_zero, result);
}



/* bigz ops */
static Lisp_Object ent_sum_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigz_add(ent_scratch_bigz, XBIGZ_DATA(l), XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_sum_BIGZ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, XINT(r));
	bigz_add(ent_scratch_bigz, XBIGZ_DATA(l), ent_scratch_bigz);
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_sum_INT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, XINT(l));
	bigz_add(ent_scratch_bigz, ent_scratch_bigz, XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object ent_sum_BIGZ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return ent_optable_sum[FLOAT_T][FLOAT_T](
		make_float(bigz_to_fpfloat(XBIGZ_DATA(l))), r);
}
static Lisp_Object ent_sum_FLOAT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return ent_optable_sum[FLOAT_T][FLOAT_T](
		l, make_float(bigz_to_fpfloat(XBIGZ_DATA(r))));
}
#endif

static Lisp_Object ent_diff_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigz_sub(ent_scratch_bigz, XBIGZ_DATA(l), XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_diff_BIGZ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, XINT(r));
	bigz_sub(ent_scratch_bigz, XBIGZ_DATA(l), ent_scratch_bigz);
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_diff_INT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, XINT(l));
	bigz_sub(ent_scratch_bigz, ent_scratch_bigz, XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object ent_diff_BIGZ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return ent_optable_diff[FLOAT_T][FLOAT_T](
		make_float(bigz_to_fpfloat(XBIGZ_DATA(l))), r);
}
static Lisp_Object ent_diff_FLOAT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return ent_optable_diff[FLOAT_T][FLOAT_T](
		l, make_float(bigz_to_fpfloat(XBIGZ_DATA(r))));
}
#endif

static Lisp_Object ent_neg_BIGZ_T(Lisp_Object l)
{
	bigz_neg(ent_scratch_bigz, XBIGZ_DATA(l));
	return make_bigz_bz(ent_scratch_bigz);
}

static Lisp_Object ent_prod_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigz_mul(ent_scratch_bigz, XBIGZ_DATA(l), XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_prod_BIGZ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, XINT(r));
	bigz_mul(ent_scratch_bigz, XBIGZ_DATA(l), ent_scratch_bigz);
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_prod_INT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	bigz_set_long(ent_scratch_bigz, XINT(l));
	bigz_mul(ent_scratch_bigz, ent_scratch_bigz, XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object ent_prod_BIGZ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	return ent_optable_prod[FLOAT_T][FLOAT_T](
		make_float(bigz_to_fpfloat(XBIGZ_DATA(l))), r);
}
static Lisp_Object ent_prod_FLOAT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return ent_optable_prod[FLOAT_T][FLOAT_T](
		l, make_float(bigz_to_fpfloat(XBIGZ_DATA(r))));
}
#endif

static Lisp_Object ent_div_BIGZ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	if (XINT(r) == 0)
		Fsignal(Qarith_error, Qnil);

	bigz_set_long(ent_scratch_bigz, XINT(r));
	bigz_div(ent_scratch_bigz, XBIGZ_DATA(l), ent_scratch_bigz);
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_div_INT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);

	bigz_set_long(ent_scratch_bigz, XINT(l));
	bigz_div(ent_scratch_bigz, ent_scratch_bigz, XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
#ifdef HAVE_FPFLOAT
static Lisp_Object ent_div_BIGZ_T_FLOAT_T(Lisp_Object l, Lisp_Object r)
{
	if (XFLOAT_DATA(r) == 0.0)
		Fsignal(Qarith_error, Qnil);

	return ent_optable_div[FLOAT_T][FLOAT_T](
		make_float(bigz_to_fpfloat(XBIGZ_DATA(l))), r);
}
static Lisp_Object ent_div_FLOAT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);

	return ent_optable_div[FLOAT_T][FLOAT_T](
		l, make_float(bigz_to_fpfloat(XBIGZ_DATA(r))));
}
#endif
#ifdef HAVE_MPQ
static Lisp_Object ent_quo_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	return make_bigq_bz(XBIGZ_DATA(l), XBIGZ_DATA(r));
}
static Lisp_Object ent_quo_BIGZ_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	if (XINT(r) == 0)
		Fsignal(Qarith_error, Qnil);

	bigz_set_long(ent_scratch_bigz, XINT(r));
	return make_bigq_bz(XBIGZ_DATA(l), ent_scratch_bigz);
}
static Lisp_Object ent_quo_INT_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);

	bigz_set_long(ent_scratch_bigz, XINT(l));
	return make_bigq_bz(ent_scratch_bigz, XBIGZ_DATA(r));
}
#endif

static Lisp_Object ent_inv_BIGZ_T(Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigz_set_long(ent_scratch_bigz, 1L);
	bigz_div(ent_scratch_bigz, ent_scratch_bigz, XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}
static Lisp_Object ent_rem_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	if (bigz_sign(XBIGZ_DATA(r)) == 0)
		Fsignal(Qarith_error, Qnil);
	bigz_mod(ent_scratch_bigz, XBIGZ_DATA(l), XBIGZ_DATA(r));
	return make_bigz_bz(ent_scratch_bigz);
}

static Lisp_Object ent_lt_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigz_lt(XBIGZ_DATA(l), XBIGZ_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_gt_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigz_gt(XBIGZ_DATA(l), XBIGZ_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_eq_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigz_eql(XBIGZ_DATA(l), XBIGZ_DATA(r)))
		? Qt : Qnil;
}
static Lisp_Object ent_ne_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	return (bigz_eql(XBIGZ_DATA(l), XBIGZ_DATA(r)))
		? Qnil : Qt;
}


static Lisp_Object ent_lift_INT_T_BIGZ_T(Lisp_Object number, ...)
{
	number = ent_normalise_number(number);
	return make_bigz(XREALINT(number));
}

static Lisp_Object ent_lift_BIGZ_T_INT_T(Lisp_Object number, ...)
{
	return make_int(bigz_to_long(XBIGZ_DATA(number)));
}

#ifdef HAVE_FPFLOAT
static Lisp_Object ent_lift_FLOAT_T_BIGZ_T(Lisp_Object number, ...)
{
	bigz_set_fpfloat(ent_scratch_bigz, XFLOAT_DATA(number));
	return make_bigz_bz(ent_scratch_bigz);
}

static Lisp_Object ent_lift_BIGZ_T_FLOAT_T(Lisp_Object number, ...)
{
	return make_float(bigz_to_fpfloat(XBIGZ_DATA(number)));
}
#endif


void init_optables_BIGZ_T (void)
{
	/* sums */
	ent_optable_sum[BIGZ_T][BIGZ_T] = ent_sum_BIGZ_T;
	ent_optable_sum[BIGZ_T][INT_T] = ent_sum_BIGZ_T_INT_T;
	ent_optable_sum[INT_T][BIGZ_T] = ent_sum_INT_T_BIGZ_T;
#ifdef HAVE_FPFLOAT
	ent_optable_sum[FLOAT_T][BIGZ_T] = ent_sum_FLOAT_T_BIGZ_T;
	ent_optable_sum[BIGZ_T][FLOAT_T] = ent_sum_BIGZ_T_FLOAT_T;
#endif
	/* diffs */
	ent_optable_diff[BIGZ_T][BIGZ_T] = ent_diff_BIGZ_T;
	ent_optable_diff[BIGZ_T][INT_T] = ent_diff_BIGZ_T_INT_T;
	ent_optable_diff[INT_T][BIGZ_T] = ent_diff_INT_T_BIGZ_T;
#ifdef HAVE_FPFLOAT
	ent_optable_diff[FLOAT_T][BIGZ_T] = ent_diff_FLOAT_T_BIGZ_T;
	ent_optable_diff[BIGZ_T][FLOAT_T] = ent_diff_BIGZ_T_FLOAT_T;
#endif
	/* prods */
	ent_optable_prod[BIGZ_T][BIGZ_T] = ent_prod_BIGZ_T;
	ent_optable_prod[BIGZ_T][INT_T] = ent_prod_BIGZ_T_INT_T;
	ent_optable_prod[INT_T][BIGZ_T] = ent_prod_INT_T_BIGZ_T;
#ifdef HAVE_FPFLOAT
	ent_optable_prod[FLOAT_T][BIGZ_T] = ent_prod_FLOAT_T_BIGZ_T;
	ent_optable_prod[BIGZ_T][FLOAT_T] = ent_prod_BIGZ_T_FLOAT_T;
#endif

	ent_optable_neg[BIGZ_T] = ent_neg_BIGZ_T;

	/* div/quo/invert code */
	ent_optable_div[BIGZ_T][BIGZ_T] = ent_div_BIGZ_T;
	ent_optable_div[BIGZ_T][INT_T] = ent_div_BIGZ_T_INT_T;
	ent_optable_div[INT_T][BIGZ_T] = ent_div_INT_T_BIGZ_T;
#ifdef HAVE_FPFLOAT
	ent_optable_div[FLOAT_T][BIGZ_T] = ent_div_FLOAT_T_BIGZ_T;
	ent_optable_div[BIGZ_T][FLOAT_T] = ent_div_BIGZ_T_FLOAT_T;
#endif

	ent_optable_inv[BIGZ_T] = ent_inv_BIGZ_T;
#ifdef HAVE_MPQ
	ent_optable_quo[BIGZ_T][BIGZ_T] = ent_quo_BIGZ_T;
	ent_optable_quo[BIGZ_T][INT_T] = ent_quo_BIGZ_T_INT_T;
	ent_optable_quo[INT_T][BIGZ_T] = ent_quo_INT_T_BIGZ_T;
#  ifdef HAVE_FPFLOAT
	ent_optable_quo[FLOAT_T][BIGZ_T] = ent_div_FLOAT_T_BIGZ_T;
	ent_optable_quo[BIGZ_T][FLOAT_T] = ent_div_BIGZ_T_FLOAT_T;
#  endif
#else
	ent_optable_quo[BIGZ_T][BIGZ_T] = ent_div_BIGZ_T;
	ent_optable_quo[BIGZ_T][INT_T] = ent_div_BIGZ_T_INT_T;
	ent_optable_quo[INT_T][BIGZ_T] = ent_div_INT_T_BIGZ_T;
#  ifdef HAVE_FPFLOAT
	ent_optable_quo[FLOAT_T][BIGZ_T] = ent_div_FLOAT_T_BIGZ_T;
	ent_optable_quo[BIGZ_T][FLOAT_T] = ent_div_BIGZ_T_FLOAT_T;
#  endif
#endif
	/* remainder code */
	ent_optable_rem[BIGZ_T][BIGZ_T] = ent_rem_BIGZ_T;
	ent_optable_mod[BIGZ_T][BIGZ_T] = ent_rem_BIGZ_T;
	ent_optable_lt[BIGZ_T][BIGZ_T] = ent_lt_BIGZ_T;
	ent_optable_gt[BIGZ_T][BIGZ_T] = ent_gt_BIGZ_T;
	ent_optable_eq[BIGZ_T][BIGZ_T] = ent_eq_BIGZ_T;
	ent_optable_ne[BIGZ_T][BIGZ_T] = ent_ne_BIGZ_T;
	ent_optable_vallt[BIGZ_T][BIGZ_T] = ent_lt_BIGZ_T;
	ent_optable_valgt[BIGZ_T][BIGZ_T] = ent_gt_BIGZ_T;
	ent_optable_valeq[BIGZ_T][BIGZ_T] = ent_eq_BIGZ_T;
	ent_optable_valne[BIGZ_T][BIGZ_T] = ent_ne_BIGZ_T;

	/* lift tables (coercion) */
	ent_optable_lift[INT_T][BIGZ_T] = ent_lift_INT_T_BIGZ_T;
	ent_optable_lift[BIGZ_T][INT_T] = ent_lift_BIGZ_T_INT_T;
	ent_optable_lift[INDEF_T][BIGZ_T] = ent_lift_INDEF_T_COMPARABLE;
#ifdef HAVE_FPFLOAT
	ent_optable_lift[FLOAT_T][BIGZ_T] = ent_lift_FLOAT_T_BIGZ_T;
	ent_optable_lift[BIGZ_T][FLOAT_T] = ent_lift_BIGZ_T_FLOAT_T;
#endif
}

void init_ent_mpz(void)
{
	REGISTER unsigned int i;

	bigz_zero = MP_ITOM(0);
	bigz_one = MP_ITOM(1);
	bigz_two = MP_ITOM(2);

	/* intern_bigz holds throwaway values from macro expansions in
	   number-mp.h.  Its value is immaterial. */
	intern_bigz = MP_ITOM(0);

	/* bigz_bytesize holds the number of bits in a byte. */
	bigz_bytesize = MP_ITOM(256);

	/* bigz_long_sign_bit holds an adjustment for negative longs. */
	bigz_long_sign_bit = MP_ITOM(256);
	for (i = 1UL; i < sizeof (long); i++)
		MP_MULT(bigz_bytesize, bigz_long_sign_bit, bigz_long_sign_bit);

	/* The MP interface only supports turning short ints into MINTs, so we have
	   to set these the hard way. */

	bigz_min_int = MP_ITOM(0);
	bigz_set_long(bigz_min_int, INT_MIN);

	bigz_max_int = MP_ITOM(0);
	bigz_set_long(bigz_max_int, INT_MAX);

	bigz_max_uint = MP_ITOM(0);
	bigz_set_ulong(bigz_max_uint, UINT_MAX);

	bigz_min_long = MP_ITOM(0);
	bigz_set_long(bigz_min_long, LONG_MIN);

	bigz_max_long = MP_ITOM(0);
	bigz_set_long(bigz_max_long, LONG_MAX);

	bigz_max_ulong = MP_ITOM(0);
	bigz_set_ulong(bigz_max_ulong, ULONG_MAX);

	bigz_init(ent_scratch_bigz);
}

void syms_of_ent_mpz(void)
{
	INIT_LRECORD_IMPLEMENTATION(bigz);
}

void vars_of_ent_mpz(void)
{
	Fprovide(intern("bignum")); /* for XE compatibility */
	Fprovide(intern("bigz"));
}
