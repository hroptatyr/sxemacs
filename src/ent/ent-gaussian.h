/*
  ent-gaussian.h -- Numeric types for SXEmacs
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


#ifndef INCLUDED_number_gaussian_h_
#define INCLUDED_number_gaussian_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

#if defined HAVE_GMP
# include "ent-gmp.h"
#elif defined HAVE_BSDMP
# include "ent-mp.h"
#else
# error "Sod it! You can't get here without my permission!"
#endif

struct bigg
{
	bigz intg;
	bigz imag;
};
typedef struct bigg bigg[1];

extern bigg ent_scratch_bigg;


/******************************** Biggs ********************************/
struct Lisp_Bigg
{
	struct lrecord_header lheader;
	bigg data;
};
typedef struct Lisp_Bigg Lisp_Bigg;

DECLARE_LRECORD(bigg, Lisp_Bigg);
#define XBIGG(x) XRECORD(x, bigg, Lisp_Bigg)
#define wrap_bigg(p) wrap_object(p)
#define BIGGP(x) RECORDP(x, bigg)
#define CHECK_BIGG(x) CHECK_RECORD(x, bigg)
#define CONCHECK_BIGG(x) CONCHECK_RECORD(x, bigg)

#define bigg_data(f) ((f)->data)
#define XBIGG_DATA(x) bigg_data(XBIGG(x))

#define XBIGG_RE(x) bigg_re(XBIGG_DATA(x))
#define XBIGG_IM(x) bigg_im(XBIGG_DATA(x))

#define BIGG_ARITH_RETURN(f,op) do					\
{									\
	Lisp_Object retval = make_bigg(0L, 0L);				\
	bigg_##op(XBIGG_DATA(retval), XBIGG_DATA(f));			\
	return retval;							\
} while (0)

#define BIGG_ARITH_RETURN1(f,op,arg) do					\
{									\
	Lisp_Object retval = make_bigg(0L, 0L);				\
	bigg_##op(XBIGG_DATA(retval), XBIGG_DATA(f), arg);		\
	return retval;							\
} while (0)

extern Lisp_Object make_bigg(long, long);
extern Lisp_Object make_bigg_bz(bigz, bigz);
extern Lisp_Object make_bigg_bg(bigg);

/***** Bigc: basic functions *****/
extern void bigg_init(bigg);
extern void bigg_fini(bigg);
extern unsigned long bigg_hashcode(bigg);

/***** Bigg: conversions *****/
extern Bufbyte *bigg_to_string(bigg, int);

/***** Bigg: converting assignments *****/
extern void bigg_set(bigg, bigg);
extern void bigg_set_long(bigg, long);
extern void bigg_set_ulong(bigg, unsigned long);
extern void bigg_set_bigz(bigg, bigz);
extern void bigg_set_long_long(bigg, long, long);
extern void bigg_set_ulong_ulong(bigg, unsigned long, unsigned long);
extern void bigg_set_bigz_bigz(bigg, bigz, bigz);

#define bigg_re(z)                  ((z)->intg)
#define bigg_im(z)                  ((z)->imag)

/***** Bigg: comparisons *****/
extern int bigg_eql(bigg, bigg);
#define bigg_cmp(f1,f2)             (bigg_eql(f1,f2) ? 0 : 1)

/***** Bigg: arithmetic *****/
extern void bigg_neg(bigg, bigg);
#if defined HAVE_MPFR && defined WITH_MPFR
extern void bigg_abs(bigfr, bigg);
#endif
extern void bigg_norm(bigz, bigg);
extern void bigg_conj(bigg, bigg);
extern void bigg_add(bigg, bigg, bigg);
extern void bigg_sub(bigg, bigg, bigg);
extern void bigg_mul(bigg, bigg, bigg);
extern void bigg_div(bigg, bigg, bigg);
extern void bigg_mod(bigg, bigg, bigg);
extern void bigg_pow(bigg, bigg, unsigned long);

extern Lisp_Object read_bigg_string(char *cp);

extern void init_optables_BIGG_T(void);
extern void init_ent_gaussian(void);
extern void syms_of_ent_gaussian(void);
extern void vars_of_ent_gaussian(void);

#endif /* INCLUDED_number_gaussian_h_ */
