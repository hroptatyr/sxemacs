/*
  ent-quatern.c -- Numeric types for SXEmacs
  Copyright (C) 2005, 2006 Sebastian Freundt

  Author:  Sebastian Freundt

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


#include <config.h>
#include <limits.h>
#include <math.h>
#include "lisp.h"
#include "sysproc.h"    /* For qxe_getpid */

#include "ent.h"
#include "ent-quatern.h"

quatern ent_scratch_quatern;
Lisp_Object Qquatern;
static ase_nullary_operation_f Qent_quatern_zero, Qent_quatern_one;


static void
quatern_print(Lisp_Object obj, Lisp_Object printcharfun, int SXE_UNUSED(escapeflag))
{
	Bufbyte *fstr = quatern_to_string(XQUATERN_DATA(obj), 10);
	write_c_string((char*)fstr, printcharfun);
	xfree(fstr);
	fstr = (Bufbyte *)NULL;
	return;
}

static int
quatern_equal(Lisp_Object obj1, Lisp_Object obj2, int SXE_UNUSED(depth))
{
	return quatern_eql(XQUATERN_DATA(obj1), XQUATERN_DATA(obj2));
}

static unsigned long
quatern_hash(Lisp_Object obj, int SXE_UNUSED(depth))
{
	return quatern_hashcode(XQUATERN_DATA(obj));
}

static Lisp_Object
quatern_mark(Lisp_Object SXE_UNUSED(obj))
{
	return Qnil;
}

static void
quatern_finalise(void *unused, int for_disksave)
{
	if (for_disksave) {
		signal_simple_error(
			"Can't dump an emacs containing "
			"quaternionic objects", Qt);
	}
}

static const struct lrecord_description quatern_description[] = {
	{ XD_OPAQUE_DATA_PTR, offsetof(Lisp_Quatern, data) },
	{ XD_END }
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("quatern", quatern,
				    quatern_mark, quatern_print,
				    quatern_finalise,
				    quatern_equal, quatern_hash,
				    quatern_description, Lisp_Quatern);


/* basic functions */
void quatern_init(quatern g)
{
	bigz_init(quatern_z(g));
	bigz_init(quatern_i(g));
	bigz_init(quatern_j(g));
	bigz_init(quatern_k(g));
}

void quatern_fini(quatern g)
{
	bigz_fini(quatern_z(g));
	bigz_fini(quatern_i(g));
	bigz_fini(quatern_j(g));
	bigz_fini(quatern_k(g));
}

unsigned long quatern_hashcode(quatern g)
{
	return (bigz_hashcode(quatern_z(g)) ^
		bigz_hashcode(quatern_i(g)) ^
		bigz_hashcode(quatern_j(g)) ^
		bigz_hashcode(quatern_k(g)));
}

Bufbyte *quatern_to_string(quatern g, int base)
{
	Bufbyte *z_str, *i_str, *j_str, *k_str;
	int z_len, i_len, j_len, k_len;
	int sign_i, sign_j, sign_k, neg_i, neg_j, neg_k;

	z_str = (Bufbyte*)bigz_to_string(quatern_z(g), base);
	i_str = (Bufbyte*)bigz_to_string(quatern_i(g), base);
	j_str = (Bufbyte*)bigz_to_string(quatern_j(g), base);
	k_str = (Bufbyte*)bigz_to_string(quatern_k(g), base);

	z_len = strlen((char*)z_str);
	i_len = strlen((char*)i_str);
	j_len = strlen((char*)j_str);
	k_len = strlen((char*)k_str);

	sign_i = bigz_sign(quatern_i(g));
	sign_j = bigz_sign(quatern_j(g));
	sign_k = bigz_sign(quatern_k(g));
	neg_i = (sign_i >= 0) ? 1 : 0;
	neg_j = (sign_j >= 0) ? 1 : 0;
	neg_k = (sign_k >= 0) ? 1 : 0;

	/* now append the imaginary string */
	XREALLOC_ARRAY(z_str, Bufbyte, z_len +
		       neg_i + i_len +
		       neg_j + j_len +
		       neg_k + k_len + 4);
	if (neg_i)
		z_str[z_len] = '+';
	if (neg_j)
		z_str[z_len+neg_i+i_len+1] = '+';
	if (neg_k)
		z_str[z_len+neg_i+i_len+1+neg_j+j_len+1] = '+';
	memmove(&z_str[z_len + neg_i],
		&i_str[0],
		i_len);
	memmove(&z_str[z_len + neg_i+i_len+1 + neg_j],
		&j_str[0],
		j_len);
	memmove(&z_str[z_len + neg_i+i_len+1 + neg_j+j_len+1 + neg_k],
		&k_str[0],
		k_len);
	z_str[z_len+neg_i+i_len] = 'i';
	z_str[z_len+neg_i+i_len+1+neg_j+j_len] = 'j';
	z_str[z_len+neg_i+i_len+1+neg_j+j_len+1+neg_k+k_len] = 'k';
	z_str[z_len+neg_i+i_len+1+neg_j+j_len+1+neg_k+k_len+1] = '\0';
	free(i_str);

	return z_str;
}

/***** Quatern: converting assignments *****/
void quatern_set(quatern g1,quatern g2)
{
	bigz_set(quatern_z(g1), quatern_z(g2));
	bigz_set(quatern_i(g1), quatern_i(g2));
	bigz_set(quatern_j(g1), quatern_j(g2));
	bigz_set(quatern_k(g1), quatern_k(g2));
}

void quatern_set_long(quatern g, long l)
{
	bigz_set_long(quatern_z(g), l);
	bigz_set_long(quatern_i(g), 0L);
	bigz_set_long(quatern_j(g), 0L);
	bigz_set_long(quatern_k(g), 0L);
}

void quatern_set_long_long_long_long(
	quatern g, long l1, long l2, long l3, long l4)
{
	bigz_set_long(quatern_z(g), l1);
	bigz_set_long(quatern_i(g), l2);
	bigz_set_long(quatern_j(g), l3);
	bigz_set_long(quatern_k(g), l4);
}

void quatern_set_ulong(quatern g, unsigned long ul)
{
	bigz_set_ulong(quatern_z(g), ul);
	bigz_set_ulong(quatern_i(g), 0UL);
	bigz_set_ulong(quatern_j(g), 0UL);
	bigz_set_ulong(quatern_k(g), 0UL);
}

void quatern_set_ulong_ulong_ulong_ulong(
	quatern g, unsigned long ul1, unsigned long ul2,
	unsigned long ul3, unsigned long ul4)
{
	bigz_set_ulong(quatern_z(g), ul1);
	bigz_set_ulong(quatern_i(g), ul2);
	bigz_set_ulong(quatern_j(g), ul3);
	bigz_set_ulong(quatern_k(g), ul4);
}

void quatern_set_bigz(quatern g, bigz z)
{
	bigz_set(quatern_z(g), z);
	bigz_set_long(quatern_i(g), 0L);
	bigz_set_long(quatern_j(g), 0L);
	bigz_set_long(quatern_k(g), 0L);
}

void quatern_set_bigz_bigz_bigz_bigz(
	quatern g, bigz z1, bigz z2, bigz z3, bigz z4)
{
	bigz_set(quatern_z(g), z1);
	bigz_set(quatern_i(g), z2);
	bigz_set(quatern_j(g), z3);
	bigz_set(quatern_k(g), z4);
}

/* void bigc_set_quatern(bigc c, quatern g)
 * {
 *	bigc_set_bigfr_bigfr(quatern_z(g), z1);
 * }
 */

/***** Quatern: comparisons *****/
int quatern_eql(quatern g1, quatern g2)
{
	return ((bigz_eql(quatern_z(g1), quatern_z(g2))) &&
		(bigz_eql(quatern_i(g1), quatern_i(g2))) &&
		(bigz_eql(quatern_j(g1), quatern_j(g2))) &&
		(bigz_eql(quatern_k(g1), quatern_k(g2))));
}

/***** Quatern: arithmetic *****/
#ifdef HAVE_MPFR
void quatern_abs(bigfr res, quatern g)
{
/* NOT DONE */
	/* the absolute archimedean valuation of a+bi is defined as:
	 * (a^2 + b^2 + c^2 + d^2)^(1/2)
	 */
	bigz accu1, accu2, bz;
	bigz_init(accu1);
	bigz_init(accu2);
	bigz_init(bz);

	bigz_mul(accu1, quatern_z(g), quatern_z(g));
	bigz_mul(accu2, quatern_i(g), quatern_i(g));
	bigz_add(bz, accu1, accu2);

	bigfr_set_bigz(res, bz);
	bigfr_sqrt(res, res);

	bigz_fini(accu1);
	bigz_fini(accu2);
	bigz_fini(bz);
}
#endif

void quatern_norm(bigz res, quatern g)
{
	/* norm is the product of g and conj(g) */
	quatern_conj(ent_scratch_quatern, g);
	quatern_mul(ent_scratch_quatern, g, ent_scratch_quatern);
	bigz_set(res, quatern_z(ent_scratch_quatern));
}

void quatern_neg(quatern res, quatern g)
{
	/* negation is defined point-wise */
	bigz_neg(quatern_z(res), quatern_z(g));
	bigz_neg(quatern_i(res), quatern_i(g));
	bigz_neg(quatern_j(res), quatern_j(g));
	bigz_neg(quatern_k(res), quatern_k(g));
}

void quatern_conj(quatern res, quatern g)
{
	bigz_set(quatern_z(res), quatern_z(g));
	bigz_neg(quatern_i(res), quatern_i(g));
	bigz_neg(quatern_j(res), quatern_j(g));
	bigz_neg(quatern_k(res), quatern_k(g));
}

void quatern_add(quatern res, quatern g1, quatern g2)
{
	quatern accu;
	quatern_init(accu);

	/* addition is defined point-wise */
	bigz_add(quatern_z(accu), quatern_z(g1), quatern_z(g2));
	bigz_add(quatern_i(accu), quatern_i(g1), quatern_i(g2));
	bigz_add(quatern_j(accu), quatern_j(g1), quatern_j(g2));
	bigz_add(quatern_k(accu), quatern_k(g1), quatern_k(g2));

	quatern_set(res, accu);
	quatern_fini(accu);
}

void quatern_sub(quatern res, quatern g1, quatern g2)
{
	quatern accu;
	quatern_init(accu);

	/* subtraction is defined point-wise */
	bigz_sub(quatern_z(accu), quatern_z(g1), quatern_z(g2));
	bigz_sub(quatern_i(accu), quatern_i(g1), quatern_i(g2));
	bigz_sub(quatern_j(accu), quatern_j(g1), quatern_j(g2));
	bigz_sub(quatern_k(accu), quatern_k(g1), quatern_k(g2));

	quatern_set(res, accu);
	quatern_fini(accu);
}

void quatern_mul(quatern res, quatern g1, quatern g2)
{
	/* multiplication is defined as:
	 * (a + bi + cj + dk)*(e + fi + gj + hk) = <too complex ;)>
	 */
	bigz accu1, accu2, accu3, accu4;
	quatern accu;
	bigz_init(accu1);
	bigz_init(accu2);
	bigz_init(accu3);
	bigz_init(accu4);
	quatern_init(accu);

	/* compute the integral part */
	bigz_mul(accu1, quatern_z(g1), quatern_z(g2));
	bigz_mul(accu2, quatern_i(g1), quatern_i(g2));
	bigz_mul(accu3, quatern_j(g1), quatern_j(g2));
	bigz_mul(accu4, quatern_k(g1), quatern_k(g2));

	bigz_sub(accu1, accu1, accu2);
	bigz_sub(accu1, accu1, accu3);
	bigz_sub(accu1, accu1, accu4);
	bigz_set(quatern_z(accu), accu1);

	/* compute the i part */
	bigz_mul(accu1, quatern_z(g1), quatern_i(g2));
	bigz_mul(accu2, quatern_i(g1), quatern_z(g2));
	bigz_mul(accu3, quatern_j(g1), quatern_k(g2));
	bigz_mul(accu4, quatern_k(g1), quatern_j(g2));

	bigz_add(accu1, accu1, accu2);
	bigz_add(accu1, accu1, accu3);
	bigz_sub(accu1, accu1, accu4);
	bigz_set(quatern_i(accu), accu1);

	/* compute the j part */
	bigz_mul(accu1, quatern_z(g1), quatern_j(g2));
	bigz_mul(accu2, quatern_i(g1), quatern_k(g2));
	bigz_mul(accu3, quatern_j(g1), quatern_z(g2));
	bigz_mul(accu4, quatern_k(g1), quatern_i(g2));

	bigz_sub(accu1, accu1, accu2);
	bigz_add(accu1, accu1, accu3);
	bigz_add(accu1, accu1, accu4);
	bigz_set(quatern_j(accu), accu1);

	/* compute the k part */
	bigz_mul(accu1, quatern_z(g1), quatern_k(g2));
	bigz_mul(accu2, quatern_i(g1), quatern_j(g2));
	bigz_mul(accu3, quatern_j(g1), quatern_i(g2));
	bigz_mul(accu4, quatern_k(g1), quatern_z(g2));

	bigz_add(accu1, accu1, accu2);
	bigz_sub(accu1, accu1, accu3);
	bigz_add(accu1, accu1, accu4);
	bigz_set(quatern_k(accu), accu1);

	quatern_set(res, accu);

	quatern_fini(accu);
	bigz_fini(accu1);
	bigz_fini(accu2);
	bigz_fini(accu3);
	bigz_fini(accu4);
}

void quatern_div(quatern res, quatern g1, quatern g2)
{
	/* division is defined as:
	 * (a + bi + cj + dk) div (a'+b'i+c'j+d'k) =
	 * ((a+bi+cj+dk)*conj(a'+b'i+c'j+d'k)) div (a'^2 + b^2 + c^2 + d^2)
	 */
	/* frob the norm */
	quatern_norm(ent_scratch_bigz, g2);

	/* do normal multiplication with conjugate of g2 */
	quatern_conj(ent_scratch_quatern, g2);
	quatern_mul(res, g1, ent_scratch_quatern);

	/* now divide (g1*conj(g2)) by |g2| (point-wise) */
	bigz_div(quatern_z(res), quatern_z(res), ent_scratch_bigz);
	bigz_div(quatern_i(res), quatern_i(res), ent_scratch_bigz);
	bigz_div(quatern_j(res), quatern_j(res), ent_scratch_bigz);
	bigz_div(quatern_k(res), quatern_k(res), ent_scratch_bigz);
}

void quatern_mod(quatern res, quatern g1, quatern g2)
{
/* NOT DONE */
	/* the modulo relation is defined as:
	 * (a + bi) mod (c + di) ~
	 * (a+bi) - ((a+bi) div (c-di)) * (c+di)
	 */
	quatern accug;
	quatern_init(accug);

	/* do normal division */
	quatern_div(accug, g1, g2);

	/* now re-multiply g2 */
	quatern_mul(accug, accug, g2);

	/* and find the difference */
	quatern_sub(res, g1, accug);

	quatern_fini(accug);
}

void quatern_pow(quatern res, quatern g1, unsigned long g2)
{
#if defined(HAVE_MPZ) && defined(WITH_GMP)
/* NOT DONE */
	unsigned long i;
	bigz bin, resintg, resimag, tmpbz1, tmpbz2, tmpbz3, intg, imag;

	bigz_init(bin);
	bigz_init(resintg);
	bigz_init(resimag);
	bigz_init(intg);
	bigz_init(imag);
	bigz_init(tmpbz1);
	bigz_init(tmpbz2);
	bigz_init(tmpbz3);

	bigz_set_long(resintg, 0L);
	bigz_set_long(resimag, 0L);

	bigz_set(intg, quatern_z(g1));
	bigz_set(imag, quatern_i(g1));

	/* we compute using the binomial coefficients */
	for (i=0; i<=g2; i++) {
		mpz_bin_uiui(bin, g2, i);
		if (i % 2 == 0) {
			/* real part changes */
			bigz_pow(tmpbz1, intg, g2-i);
			bigz_pow(tmpbz2, imag, i);
			bigz_mul(tmpbz3, tmpbz1, tmpbz2);
			bigz_mul(bin, bin, tmpbz3);
			if (i % 4 == 0) {
				bigz_add(resintg, resintg, bin);
			} else if (i % 4 == 2) {
				bigz_sub(resintg, resintg, bin);
			}
		} else {
			/* imag part changes */
			bigz_pow(tmpbz1, intg, g2-i);
			bigz_pow(tmpbz2, imag, i);
			bigz_mul(tmpbz3, tmpbz1, tmpbz2);
			bigz_mul(bin, bin, tmpbz3);
			if (i % 4 == 1) {
				bigz_add(resimag, resimag, bin);
			} else if (i % 4 == 3) {
				bigz_sub(resimag, resimag, bin);
			}
		}
	}

	quatern_set_bigz_bigz_bigz_bigz(res, resintg, resimag, resimag, resimag);

	bigz_fini(bin);
	bigz_fini(intg);
	bigz_fini(imag);
	bigz_init(resintg);
	bigz_init(resimag);
	bigz_fini(tmpbz1);
	bigz_fini(tmpbz2);
	bigz_fini(tmpbz3);
#else
	quatern_set_long_long(res, 0L, 0L);
#endif
}



#define Z_INT 1
#define I_UNARY_SYMBOL 2
#define I_INT 4
#define I_CHAR 8
#define J_UNARY_SYMBOL 16
#define J_INT 32
#define J_CHAR 64
#define K_UNARY_SYMBOL 128
#define K_INT 256
#define K_CHAR 512

int isquatern_string (const char *cp)
{
	int state;
	const Bufbyte *ucp = (const Bufbyte *)cp;


	/* parse the z-part */
	state = 0;
	if (*ucp == '+' || *ucp == '-')
		ucp++;

	if (*ucp >= '0' && *ucp <= '9') {
		state |= Z_INT;
		while (*ucp >= '0' && *ucp <= '9')
			ucp++;
	}

	/* check if we had a int number until here */
	if (!(state == (Z_INT)))
		return 0;

	/* now parse i-part */
	state = 0;
	if (*ucp == '+' || *ucp == '-') {
		state |= I_UNARY_SYMBOL;
		ucp++;
	}

	if (*ucp >= '0' && *ucp <= '9') {
		state |= I_INT;
		while (*ucp >= '0' && *ucp <= '9')
			ucp++;
	}
	if (*ucp == 'i' || *ucp == 'I') {
		state |= I_CHAR;
		ucp++;
	}
	/* check if we had a quatern number until here */
	if (!(state == (I_UNARY_SYMBOL | I_INT | I_CHAR) ||
	      state == (I_UNARY_SYMBOL | I_CHAR)))
		return 0;

	/* now parse j-part */
	state = 0;
	if (*ucp == '+' || *ucp == '-') {
		state |= J_UNARY_SYMBOL;
		ucp++;
	}

	if (*ucp >= '0' && *ucp <= '9') {
		state |= J_INT;
		while (*ucp >= '0' && *ucp <= '9')
			ucp++;
	}
	if (*ucp == 'j' || *ucp == 'J') {
		state |= J_CHAR;
		ucp++;
	}
	/* check if we had a quatern number until here */
	if (!(state == (J_UNARY_SYMBOL | J_INT | J_CHAR) ||
	      state == (J_UNARY_SYMBOL | J_CHAR)))
		return 0;

	/* now parse k-part */
	state = 0;
	if (*ucp == '+' || *ucp == '-') {
		state |= K_UNARY_SYMBOL;
		ucp++;
	}

	if (*ucp >= '0' && *ucp <= '9') {
		state |= K_INT;
		while (*ucp >= '0' && *ucp <= '9')
			ucp++;
	}
	if (*ucp == 'k' || *ucp == 'K') {
		state |= K_CHAR;
		ucp++;
	}
	/* check if we had a quatern number until here */
	if (!(state == (K_UNARY_SYMBOL | K_INT | K_CHAR) ||
	      state == (K_UNARY_SYMBOL | K_CHAR)))
		return 0;

	return (((*ucp == 0) || (*ucp == ' ') || (*ucp == '\t') ||
		 (*ucp == '\n') || (*ucp == '\r') || (*ucp == '\f')));
}

Lisp_Object read_quatern_string(char *cp)
{
	bigz bz_z, bz_i, bz_j, bz_k;
	int sign;
	Lisp_Object result;
	Bufbyte *end;
	Bufbyte tmp;

	bigz_init(bz_z);
	bigz_init(bz_i);
	bigz_init(bz_j);
	bigz_init(bz_k);

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
	 * See limitations below in convert GMP-MPZ strings
	 */
	tmp = (Bufbyte)*cp;
	*cp = '\0';
	bigz_set_string(bz_z, (char *)end, 0);
	*cp = tmp;

	/* read the i-part */
	sign = 0;
	if (*cp == '+') {
		cp++;
		sign = 1;
	}
	if (*cp == '-') {
		cp++;
		sign = -1;
	}
	if ((*cp == 'i' || *cp == 'I') && (sign == 1)) {
		/* expand +i to +1i */
		bigz_set_long(bz_i, 1L);
	} else if ((*cp == 'i' || *cp == 'I') && (sign == -1)) {
		/* expand -i to -1i */
		bigz_set_long(bz_i, -1L);
	} else {
		end = (Bufbyte*)cp;
		if (sign == -1)
			end--;
		while ((*cp >= '0' && *cp <= '9'))
			cp++;
		tmp = (Bufbyte)*cp;
		*cp = '\0';
		bigz_set_string(bz_i, (char *)end, 0);
		*cp = tmp;
	}
	/* read over i */
	if (*cp == 'i' || *cp == 'I')
		cp++;

	/* read the j-part */
	sign = 0;
	if (*cp == '+') {
		cp++;
		sign = 1;
	}
	if (*cp == '-') {
		cp++;
		sign = -1;
	}
	if ((*cp == 'j' || *cp == 'J') && (sign == 1)) {
		/* expand +j to +1j */
		bigz_set_long(bz_j, 1L);
	} else if ((*cp == 'j' || *cp == 'J') && (sign == -1)) {
		/* expand -j to -1j */
		bigz_set_long(bz_j, -1L);
	} else {
		end = (Bufbyte*)cp;
		if (sign == -1)
			end--;
		while ((*cp >= '0' && *cp <= '9'))
			cp++;
		tmp = (Bufbyte)*cp;
		*cp = '\0';
		bigz_set_string(bz_j, (char *)end, 0);
		*cp = tmp;
	}
	/* read over j */
	if (*cp == 'j' || *cp == 'J')
		cp++;

	/* read the k-part */
	sign = 0;
	if (*cp == '+') {
		cp++;
		sign = 1;
	}
	if (*cp == '-') {
		cp++;
		sign = -1;
	}
	if ((*cp == 'k' || *cp == 'K') && (sign == 1)) {
		/* expand +k to +1k */
		bigz_set_long(bz_k, 1L);
	} else if ((*cp == 'k' || *cp == 'K') && (sign == -1)) {
		/* expand -k to -1k */
		bigz_set_long(bz_k, -1L);
	} else {
		end = (Bufbyte*)cp;
		if (sign == -1)
			end--;
		while ((*cp >= '0' && *cp <= '9'))
			cp++;
		tmp = (Bufbyte)*cp;
		*cp = '\0';
		bigz_set_string(bz_k, (char *)end, 0);
		*cp = tmp;
	}
	/* read over k */
	if (*cp == 'k' || *cp == 'K')
		cp++;

	result = make_quatern_bz(bz_z, bz_i, bz_j, bz_k);

	bigz_fini(bz_z);
	bigz_fini(bz_i);
	bigz_fini(bz_j);
	bigz_fini(bz_k);
	return result;
}

/* quatern ops */
static inline int
ent_quatern_zerop(Lisp_Object o)
{
	return (bigz_sign(quatern_z(XQUATERN_DATA(o))) == 0 &&
		bigz_sign(quatern_i(XQUATERN_DATA(o))) == 0 &&
		bigz_sign(quatern_j(XQUATERN_DATA(o))) == 0 &&
		bigz_sign(quatern_k(XQUATERN_DATA(o))) == 0);
}

static inline int
ent_quatern_onep(Lisp_Object o)
{
	return ((bigz_fits_long_p(quatern_z(XQUATERN_DATA(o))) &&
		 bigz_to_long(quatern_z(XQUATERN_DATA(o))) == 1L) &&
		bigz_sign(quatern_i(XQUATERN_DATA(o))) == 0 &&
		bigz_sign(quatern_j(XQUATERN_DATA(o))) == 0 &&
		bigz_sign(quatern_k(XQUATERN_DATA(o))) == 0);
}

static inline int
ent_quatern_unitp(Lisp_Object o)
{
	return (!ent_quatern_zerop(o) &&
		(bigz_fits_long_p(quatern_z(XQUATERN_DATA(o))) &&
		 (bigz_to_long(quatern_z(XQUATERN_DATA(o))) == 0L ||
		  bigz_to_long(quatern_z(XQUATERN_DATA(o))) == 1L ||
		  bigz_to_long(quatern_z(XQUATERN_DATA(o))) == -1L)) &&
		(bigz_fits_long_p(quatern_i(XQUATERN_DATA(o))) &&
		 (bigz_to_long(quatern_i(XQUATERN_DATA(o))) == 0L ||
		  bigz_to_long(quatern_i(XQUATERN_DATA(o))) == 1L ||
		  bigz_to_long(quatern_i(XQUATERN_DATA(o))) == -1L)) &&
		(bigz_fits_long_p(quatern_j(XQUATERN_DATA(o))) &&
		 (bigz_to_long(quatern_j(XQUATERN_DATA(o))) == 0L ||
		  bigz_to_long(quatern_j(XQUATERN_DATA(o))) == 1L ||
		  bigz_to_long(quatern_j(XQUATERN_DATA(o))) == -1L)) &&
		(bigz_fits_long_p(quatern_k(XQUATERN_DATA(o))) &&
		 (bigz_to_long(quatern_k(XQUATERN_DATA(o))) == 0L ||
		  bigz_to_long(quatern_k(XQUATERN_DATA(o))) == 1L ||
		  bigz_to_long(quatern_k(XQUATERN_DATA(o))) == -1L)));
}

static inline Lisp_Object
ent_sum_QUATERN_T(Lisp_Object l, Lisp_Object r)
{
	quatern_add(ent_scratch_quatern, XQUATERN_DATA(l), XQUATERN_DATA(r));
	return make_quatern_qu(ent_scratch_quatern);
}
static inline Lisp_Object
ent_sum_QUATERN_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	quatern_set_long(ent_scratch_quatern, ent_int(r));
	quatern_add(ent_scratch_quatern, XQUATERN_DATA(l), ent_scratch_quatern);
	return make_quatern_qu(ent_scratch_quatern);
}
static inline Lisp_Object
ent_sum_INT_T_QUATERN_T(Lisp_Object l, Lisp_Object r)
{
	quatern_set_long(ent_scratch_quatern, ent_int(l));
	quatern_add(ent_scratch_quatern, ent_scratch_quatern, XQUATERN_DATA(r));
	return make_quatern_qu(ent_scratch_quatern);
}
static inline Lisp_Object
ent_sum_QUATERN_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	quatern_set_bigz(ent_scratch_quatern, XBIGZ_DATA(r));
	quatern_add(ent_scratch_quatern, XQUATERN_DATA(l), ent_scratch_quatern);
	return make_quatern_qu(ent_scratch_quatern);
}
static inline Lisp_Object
ent_sum_BIGZ_T_QUATERN_T(Lisp_Object l, Lisp_Object r)
{
	quatern_set_bigz(ent_scratch_quatern, XBIGZ_DATA(l));
	quatern_add(ent_scratch_quatern, ent_scratch_quatern, XQUATERN_DATA(r));
	return make_quatern_qu(ent_scratch_quatern);
}

static inline Lisp_Object
ent_diff_QUATERN_T(Lisp_Object l, Lisp_Object r)
{
	quatern_sub(ent_scratch_quatern, XQUATERN_DATA(l), XQUATERN_DATA(r));
	return make_quatern_qu(ent_scratch_quatern);
}
static inline Lisp_Object
ent_diff_QUATERN_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	quatern_set_long(ent_scratch_quatern, ent_int(r));
	quatern_sub(ent_scratch_quatern, XQUATERN_DATA(l), ent_scratch_quatern);
	return make_quatern_qu(ent_scratch_quatern);
}
static inline Lisp_Object
ent_diff_INT_T_QUATERN_T(Lisp_Object l, Lisp_Object r)
{
	quatern_set_long(ent_scratch_quatern, ent_int(l));
	quatern_sub(ent_scratch_quatern, ent_scratch_quatern, XQUATERN_DATA(r));
	return make_quatern_qu(ent_scratch_quatern);
}
static inline Lisp_Object
ent_diff_QUATERN_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	quatern_set_bigz(ent_scratch_quatern, XBIGZ_DATA(r));
	quatern_sub(ent_scratch_quatern, XQUATERN_DATA(l), ent_scratch_quatern);
	return make_quatern_qu(ent_scratch_quatern);
}
static inline Lisp_Object
ent_diff_BIGZ_T_QUATERN_T(Lisp_Object l, Lisp_Object r)
{
	quatern_set_bigz(ent_scratch_quatern, XBIGZ_DATA(l));
	quatern_sub(ent_scratch_quatern, ent_scratch_quatern, XQUATERN_DATA(r));
	return make_quatern_qu(ent_scratch_quatern);
}

static inline Lisp_Object
ent_neg_QUATERN_T(Lisp_Object l)
{
	quatern_neg(ent_scratch_quatern, XQUATERN_DATA(l));
	return make_quatern_qu(ent_scratch_quatern);
}
static inline Lisp_Object
ent_prod_QUATERN_T(Lisp_Object l, Lisp_Object r)
{
	quatern_mul(ent_scratch_quatern, XQUATERN_DATA(l), XQUATERN_DATA(r));
	return make_quatern_qu(ent_scratch_quatern);
}
static inline Lisp_Object
ent_prod_QUATERN_T_INT_T(Lisp_Object l, Lisp_Object r)
{
	quatern_set_long(ent_scratch_quatern, ent_int(r));
	quatern_mul(ent_scratch_quatern, XQUATERN_DATA(l), XQUATERN_DATA(r));
	return make_quatern_qu(ent_scratch_quatern);
}
static inline Lisp_Object
ent_prod_INT_T_QUATERN_T(Lisp_Object l, Lisp_Object r)
{
	quatern_set_long(ent_scratch_quatern, ent_int(l));
	quatern_mul(ent_scratch_quatern, XQUATERN_DATA(l), XQUATERN_DATA(r));
	return make_quatern_qu(ent_scratch_quatern);
}
static inline Lisp_Object
ent_prod_QUATERN_T_BIGZ_T(Lisp_Object l, Lisp_Object r)
{
	quatern_set_bigz(ent_scratch_quatern, XBIGZ_DATA(r));
	quatern_mul(ent_scratch_quatern, XQUATERN_DATA(l), XQUATERN_DATA(r));
	return make_quatern_qu(ent_scratch_quatern);
}
static inline Lisp_Object
ent_prod_BIGZ_T_QUATERN_T(Lisp_Object l, Lisp_Object r)
{
	quatern_set_bigz(ent_scratch_quatern, XBIGZ_DATA(l));
	quatern_mul(ent_scratch_quatern, XQUATERN_DATA(l), XQUATERN_DATA(r));
	return make_quatern_qu(ent_scratch_quatern);
}

static inline Lisp_Object
ent_div_QUATERN_T(Lisp_Object l, Lisp_Object r)
{
	if (ent_quatern_zerop(r)) {
		if (!ent_quatern_zerop(l)) {
			return make_indef(COMPLEX_INFINITY);
		} else {
			return make_indef(NOT_A_NUMBER);
		}
	}
	quatern_div(ent_scratch_quatern, XQUATERN_DATA(l), XQUATERN_DATA(r));
	return make_quatern_qu(ent_scratch_quatern);
}
#if defined(HAVE_MPC) && 0
/* this does not work yet, our quaternions are the integral ring of the division
   algebra usually known as quaternions
*/
static inline Lisp_Object
ent_quo_QUATERN_T(Lisp_Object l, Lisp_Object r)
{
	if (ent_quatern_zerop(r)) {
		if (!ent_quatern_zerop(l)) {
			return make_indef(COMPLEX_INFINITY);
		} else {
			return make_indef(NOT_A_NUMBER);
		}
	}
	bigc_set_prec(ent_scratch_bigc, internal_get_precision(Qnil));
	bigc_div(ent_scratch_bigc,
		 XBIGC_DATA(Fcoerce_number(l, Qbigc, Qnil)),
		 XBIGC_DATA(Fcoerce_number(r, Qbigc, Qnil)));
	return make_bigc_bc(ent_scratch_bigc);
}
#endif
static inline Lisp_Object
ent_inv_QUATERN_T(Lisp_Object r)
{
	if (ent_quatern_zerop(r)) {
		return make_indef(COMPLEX_INFINITY);
	}
	quatern_set_long(ent_scratch_quatern, 1L);
	quatern_div(ent_scratch_quatern, ent_scratch_quatern, XQUATERN_DATA(r));
	return make_quatern_qu(ent_scratch_quatern);
}
static inline Lisp_Object
ent_rem_QUATERN_T(Lisp_Object l, Lisp_Object r)
{
	if (ent_quatern_zerop(r)) {
		return Qent_quatern_zero;
	}
	quatern_mod(ent_scratch_quatern, XQUATERN_DATA(l), XQUATERN_DATA(r));
	return make_quatern_qu(ent_scratch_quatern);
}
static inline Lisp_Object
ent_pow_QUATERN_T_integer(Lisp_Object l, Lisp_Object r)
{
	unsigned long expo;

	if (INTP(r))
		expo = ent_int(r);
	else if (BIGZP(r)) {
		if (bigz_fits_ulong_p(XBIGZ_DATA(r)))
			expo = bigz_to_ulong(XBIGZ_DATA(r));
		else
			Fsignal(Qarith_error, r);
	} else
		Fsignal(Qdomain_error, r);

	quatern_pow(ent_scratch_quatern, XQUATERN_DATA(l), expo);
	return make_quatern_qu(ent_scratch_quatern);
}

/* relations */
static inline int
ent_eq_quatern(Lisp_Object l, Lisp_Object r)
{
	return (bigz_eql(quatern_z(XQUATERN_DATA(l)),
			 quatern_z(XQUATERN_DATA(r))) &&
		bigz_eql(quatern_i(XQUATERN_DATA(l)),
			 quatern_i(XQUATERN_DATA(r))) &&
		bigz_eql(quatern_j(XQUATERN_DATA(l)),
			 quatern_j(XQUATERN_DATA(r))) &&
		bigz_eql(quatern_k(XQUATERN_DATA(l)),
			 quatern_k(XQUATERN_DATA(r))));
}

static inline int
ent_ne_quatern(Lisp_Object l, Lisp_Object r)
{
	return (bigz_eql(quatern_z(XQUATERN_DATA(l)),
			 quatern_z(XQUATERN_DATA(r))) &&
		bigz_eql(quatern_i(XQUATERN_DATA(l)),
			 quatern_i(XQUATERN_DATA(r))) &&
		bigz_eql(quatern_j(XQUATERN_DATA(l)),
			 quatern_j(XQUATERN_DATA(r))) &&
		bigz_eql(quatern_k(XQUATERN_DATA(l)),
			 quatern_k(XQUATERN_DATA(r))));
}

#if 0
static inline Lisp_Object
ent_vallt_QUATERN_T(Lisp_Object l, Lisp_Object r)
{
	bigz b2;
	int result;

	bigz_init(b2);
	quatern_norm(ent_scratch_bigz, XQUATERN_DATA(l));
	quatern_norm(b2, XQUATERN_DATA(r));
	result = bigz_lt(ent_scratch_bigz, b2);

	bigz_fini(b2);
	return (result) ? Qt : Qnil;
}
static inline Lisp_Object
ent_valgt_QUATERN_T(Lisp_Object l, Lisp_Object r)
{
	bigz b2;
	int result;

	bigz_init(b2);
	quatern_norm(ent_scratch_bigz, XQUATERN_DATA(l));
	quatern_norm(b2, XQUATERN_DATA(r));
	result = bigz_gt(ent_scratch_bigz, b2);

	bigz_fini(b2);
	return (result) ? Qt : Qnil;
}
static inline Lisp_Object
ent_valeq_QUATERN_T(Lisp_Object l, Lisp_Object r)
{
	bigz b2;
	int result;

	bigz_init(b2);
	quatern_norm(ent_scratch_bigz, XQUATERN_DATA(l));
	quatern_norm(b2, XQUATERN_DATA(r));
	result = bigz_eql(ent_scratch_bigz, b2);

	bigz_fini(b2);
	return (result) ? Qt : Qnil;
}
static inline Lisp_Object
ent_valne_QUATERN_T(Lisp_Object l, Lisp_Object r)
{
	bigz b2;
	int result;

	bigz_init(b2);
	quatern_norm(ent_scratch_bigz, XQUATERN_DATA(l));
	quatern_norm(b2, XQUATERN_DATA(r));
	result = bigz_eql(ent_scratch_bigz, b2);

	bigz_fini(b2);
	return (result) ? Qnil : Qt;
}
#endif

/* lifts */
static inline Lisp_Object
ent_lift_all_QUATERN_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	number = ent_lift(number, BIGZ_T, NULL);
	bigz_set_long(ent_scratch_bigz, 0L);
	return make_quatern_bz(XBIGZ_DATA(number),
			       ent_scratch_bigz,
			       ent_scratch_bigz,
			       ent_scratch_bigz);
}

#ifdef HAVE_MPZ
static Lisp_Object
ent_lift_COMPLEX_QUATERN_T(Lisp_Object number, ent_lift_args_t SXE_UNUSED(la))
{
	Lisp_Object z, i;

	z = Freal_part(number);
	i = Fimaginary_part(number);

	z = ent_lift(z, BIGZ_T, NULL);
	i = ent_lift(i, BIGZ_T, NULL);

	bigz_set_long(ent_scratch_bigz, 0L);
	return make_quatern_bz(XBIGZ_DATA(z), XBIGZ_DATA(i),
			       ent_scratch_bigz,
			       ent_scratch_bigz);
}
#endif


static inline void
ent_quatern_nullary_optable_init(void)
{
	Qent_quatern_zero = make_quatern(0L, 0L, 0L, 0L);
	Qent_quatern_one = make_quatern(1L, 0L, 0L, 0L);
	staticpro(&Qent_quatern_zero);
	staticpro(&Qent_quatern_one);

	ent_nullop_register(ASE_NULLARY_OP_ZERO, QUATERN_T, Qent_quatern_zero);
	ent_nullop_register(ASE_NULLARY_OP_ONE, QUATERN_T, Qent_quatern_one);
}

static inline void
ent_quatern_unary_optable_init(void)
{
	ent_unop_register(ASE_UNARY_OP_NEG, QUATERN_T, ent_neg_QUATERN_T);
	ent_unop_register(ASE_UNARY_OP_INV, QUATERN_T, ent_inv_QUATERN_T);
}

static inline void
ent_quatern_binary_optable_init(void)
{
	/* sums */
	ent_binop_register(ASE_BINARY_OP_SUM,
			   QUATERN_T, QUATERN_T, ent_sum_QUATERN_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   QUATERN_T, INT_T, ent_sum_QUATERN_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   INT_T, QUATERN_T, ent_sum_INT_T_QUATERN_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   QUATERN_T, BIGZ_T, ent_sum_QUATERN_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_SUM,
			   BIGZ_T, QUATERN_T, ent_sum_BIGZ_T_QUATERN_T);
	/* diffs */
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   QUATERN_T, QUATERN_T, ent_diff_QUATERN_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   QUATERN_T, INT_T, ent_diff_QUATERN_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   INT_T, QUATERN_T, ent_diff_INT_T_QUATERN_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   QUATERN_T, BIGZ_T, ent_diff_QUATERN_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_DIFF,
			   BIGZ_T, QUATERN_T, ent_diff_BIGZ_T_QUATERN_T);
	/* prods */
	ent_binop_register(ASE_BINARY_OP_PROD,
			   QUATERN_T, QUATERN_T, ent_prod_QUATERN_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   QUATERN_T, INT_T, ent_prod_QUATERN_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   INT_T, QUATERN_T, ent_prod_INT_T_QUATERN_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   QUATERN_T, BIGZ_T, ent_prod_QUATERN_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_PROD,
			   BIGZ_T, QUATERN_T, ent_prod_BIGZ_T_QUATERN_T);

	/* divisions and quotients */
	ent_binop_register(ASE_BINARY_OP_DIV,
			   QUATERN_T, QUATERN_T, ent_div_QUATERN_T);
#if 0
	ent_binop_register(ASE_BINARY_OP_DIV,
			   QUATERN_T, INT_T, ent_div_QUATERN_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   INT_T, QUATERN_T, ent_div_INT_T_QUATERN_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   QUATERN_T, BIGZ_T, ent_div_QUATERN_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGZ_T, QUATERN_T, ent_div_BIGZ_T_QUATERN_T);
#endif

	ent_binop_register(ASE_BINARY_OP_QUO,
			   QUATERN_T, QUATERN_T, ent_div_QUATERN_T);
#if 0
	ent_binop_register(ASE_BINARY_OP_QUO,
			   QUATERN_T, INT_T, ent_quo_QUATERN_T_INT_T);
	ent_binop_register(ASE_BINARY_OP_QUO,
			   INT_T, QUATERN_T, ent_quo_INT_T_QUATERN_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   QUATERN_T, BIGZ_T, ent_div_QUATERN_T_BIGZ_T);
	ent_binop_register(ASE_BINARY_OP_DIV,
			   BIGZ_T, QUATERN_T, ent_div_BIGZ_T_QUATERN_T);
#endif

#if 0				/* not implemented yet */
	ent_binop_register(ASE_BINARY_OP_REM,
			   QUATERN_T, QUATERN_T, ent_rem_QUATERN_T);
	ent_binop_register(ASE_BINARY_OP_MOD,
			   QUATERN_T, QUATERN_T, ent_rem_QUATERN_T);

	ent_binop_register(ASE_BINARY_OP_POW,
			   QUATERN_T, INT_T, ent_pow_QUATERN_T_integer);
	ent_binop_register(ASE_BINARY_OP_POW,
			   QUATERN_T, BIGZ_T, ent_pow_QUATERN_T_integer);
#endif
}

static inline void
ent_quatern_unary_reltable_init(void)
{
	ent_unrel_register(ASE_UNARY_REL_ZEROP, QUATERN_T, ent_quatern_zerop);
	ent_unrel_register(ASE_UNARY_REL_ONEP, QUATERN_T, ent_quatern_onep);
	ent_unrel_register(ASE_UNARY_REL_UNITP, QUATERN_T, ent_quatern_unitp);
}

static inline void
ent_quatern_binary_reltable_init(void)
{
	ent_binrel_register(ASE_BINARY_REL_EQUALP,
			    QUATERN_T, QUATERN_T, ent_eq_quatern);
	ent_binrel_register(ASE_BINARY_REL_NEQP,
			    QUATERN_T, QUATERN_T, ent_ne_quatern);
}

static inline void
ent_quatern_lifttable_init(void)
{
	ent_lift_register(INT_T, QUATERN_T, ent_lift_all_QUATERN_T);
	ent_lift_register(BIGZ_T, BIGC_T, ent_lift_all_QUATERN_T);
#ifdef HAVE_MPQ
	ent_lift_register(BIGQ_T, QUATERN_T, ent_lift_all_QUATERN_T);
#endif
#ifdef HAVE_MPF
	ent_lift_register(BIGF_T, QUATERN_T, ent_lift_all_QUATERN_T);
#endif
#ifdef HAVE_MPFR
	ent_lift_register(BIGFR_T, QUATERN_T, ent_lift_all_QUATERN_T);
#endif
#ifdef HAVE_FPFLOAT
	ent_lift_register(FLOAT_T, QUATERN_T, ent_lift_all_QUATERN_T);
#endif
#ifdef HAVE_PSEUG
	ent_lift_register(BIGG_T, QUATERN_T, ent_lift_COMPLEX_QUATERN_T);
#endif
#ifdef HAVE_MPC
	ent_lift_register(BIGC_T, QUATERN_T, ent_lift_COMPLEX_QUATERN_T);
#endif
}

void init_optables_QUATERN_T(void)
{
	ent_quatern_nullary_optable_init();
	ent_quatern_unary_optable_init();
	ent_quatern_binary_optable_init();
	ent_quatern_unary_reltable_init();
	ent_quatern_binary_reltable_init();
	ent_quatern_lifttable_init();
}


DEFUN ("make-quatern", Fmake_quatern, 4, 4, 0, /*
Return the Quaternion whose z-component is Z,
whose i-, j-, and k-components are I, J and K, respectively.
*/
       (z, i, j, k))
{
	Lisp_Object tmp_z, tmp_i, tmp_j, tmp_k;

	CHECK_COMPARABLE(z);
	CHECK_COMPARABLE(i);
	CHECK_COMPARABLE(j);
	CHECK_COMPARABLE(k);

	tmp_z = Fcoerce_number(z, Qbigz, Qnil);
	tmp_i = Fcoerce_number(i, Qbigz, Qnil);
	tmp_j = Fcoerce_number(j, Qbigz, Qnil);
	tmp_k = Fcoerce_number(k, Qbigz, Qnil);
	return make_quatern_bz(XBIGZ_DATA(tmp_z), XBIGZ_DATA(tmp_i),
			       XBIGZ_DATA(tmp_j), XBIGZ_DATA(tmp_k));
}

DEFUN ("quatern-z", Fquatern_z, 1, 1, 0, /*
Return QUATERNION's z-component.
*/
       (quaternion))
{
	CHECK_NUMBER(quaternion);

	if (COMPARABLEP(quaternion))
		return quaternion;
	else if (COMPLEXP(quaternion))
		return Freal_part(quaternion);
	else if (QUATERNP(quaternion))
		return make_bigz_bz(XQUATERN_Z(quaternion));
	else
		return wrong_type_argument(Qquaternp, quaternion);
}
DEFUN ("quatern-i", Fquatern_i, 1, 1, 0, /*
Return QUATERNION's i-component.
*/
       (quaternion))
{
	CHECK_NUMBER(quaternion);

	if (COMPARABLEP(quaternion))
		return Qzero;
	else if (COMPLEXP(quaternion))
		return Fimaginary_part(quaternion);
	else if (QUATERNP(quaternion))
		return make_bigz_bz(XQUATERN_I(quaternion));
	else
		return wrong_type_argument(Qquaternp, quaternion);
}
DEFUN ("quatern-j", Fquatern_j, 1, 1, 0, /*
Return QUATERNION's j-component.
*/
       (quaternion))
{
	CHECK_NUMBER(quaternion);

	if (COMPARABLEP(quaternion) || COMPLEXP(quaternion))
		return Qzero;
	else if (QUATERNP(quaternion))
		return make_bigz_bz(XQUATERN_J(quaternion));
	else
		return wrong_type_argument(Qquaternp, quaternion);
}
DEFUN ("quatern-k", Fquatern_k, 1, 1, 0, /*
Return QUATERNION's k-component.
*/
       (quaternion))
{
	CHECK_NUMBER(quaternion);

	if (COMPARABLEP(quaternion) || COMPLEXP(quaternion))
		return Qzero;
	else if (QUATERNP(quaternion))
		return make_bigz_bz(XQUATERN_K(quaternion));
	else
		return wrong_type_argument(Qquaternp, quaternion);
}

void init_ent_quatern(void)
{
	quatern_init(ent_scratch_quatern);
}

void syms_of_ent_quatern(void)
{
	INIT_LRECORD_IMPLEMENTATION(quatern);

	DEFSYMBOL(Qquatern);

	DEFSUBR(Fmake_quatern);
	DEFSUBR(Fquatern_z);
	DEFSUBR(Fquatern_i);
	DEFSUBR(Fquatern_j);
	DEFSUBR(Fquatern_k);
}

void vars_of_ent_quatern(void)
{
	Fprovide(intern("quatern"));
}
