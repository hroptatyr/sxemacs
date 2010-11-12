/*
  ent-quatern.h -- Numeric types for SXEmacs
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


#ifndef INCLUDED_ent_quatern_h_
#define INCLUDED_ent_quatern_h_

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
# error "Bollocks, in your dreams, twit!"
#endif

extern Lisp_Object Qquatern;

struct quatern
{
	bigz z;
	bigz i;
	bigz j;
	bigz k;
};
typedef struct quatern quatern[1];

extern quatern ent_scratch_quatern;


/******************************** Quaterns ********************************/
struct Lisp_Quatern
{
	struct lrecord_header lheader;
	quatern data;
};
typedef struct Lisp_Quatern Lisp_Quatern;

DECLARE_LRECORD(quatern, Lisp_Quatern);
#define XQUATERN(x) XRECORD(x, quatern, Lisp_Quatern)
#define wrap_quatern(p) wrap_object(p)
#define QUATERNP(x) RECORDP(x, quatern)
#define CHECK_QUATERN(x) CHECK_RECORD(x, quatern)
#define CONCHECK_QUATERN(x) CONCHECK_RECORD(x, quatern)

#define quatern_data(f) ((f)->data)
#define XQUATERN_DATA(x) quatern_data(XQUATERN(x))

#define XQUATERN_Z(x) quatern_z(XQUATERN_DATA(x))
#define XQUATERN_I(x) quatern_i(XQUATERN_DATA(x))
#define XQUATERN_J(x) quatern_j(XQUATERN_DATA(x))
#define XQUATERN_K(x) quatern_k(XQUATERN_DATA(x))

#define QUATERN_ARITH_RETURN(f,op) do					\
{									\
	Lisp_Object retval = make_quatern(0L, 0L);			\
	quatern_##op(XQUATERN_DATA(retval), XQUATERN_DATA(f));		\
	return retval;							\
} while (0)

#define QUATERN_ARITH_RETURN1(f,op,arg) do				\
{									\
	Lisp_Object retval = make_quatern(0L, 0L);			\
	quatern_##op(XQUATERN_DATA(retval), XQUATERN_DATA(f), arg);	\
	return retval;							\
} while (0)

extern Lisp_Object make_quatern(long, long, long, long);
extern Lisp_Object make_quatern_bz(bigz, bigz, bigz, bigz);
extern Lisp_Object make_quatern_qu(quatern);


/***** quatern: basic functions *****/
extern void quatern_init(quatern);
extern void quatern_fini(quatern);
extern unsigned long quatern_hashcode(quatern);

/***** Quatern: conversions *****/
extern Bufbyte *quatern_to_string(quatern, int);

/***** Quatern: converting assignments *****/
extern void quatern_set(quatern, quatern);
extern void quatern_set_long(quatern, long);
extern void quatern_set_ulong(quatern, unsigned long);
extern void quatern_set_bigz(quatern, bigz);
extern void quatern_set_long_long_long_long(quatern, long, long, long, long);
extern void quatern_set_ulong_ulong_ulong_ulong
(quatern, unsigned long, unsigned long, unsigned long, unsigned long);
extern void quatern_set_bigz_bigz_bigz_bigz(quatern, bigz, bigz, bigz, bigz);

#define quatern_z(q)                  ((q)->z)
#define quatern_i(q)                  ((q)->i)
#define quatern_j(q)                  ((q)->j)
#define quatern_k(q)                  ((q)->k)

/***** Quatern: comparisons *****/
extern int quatern_eql(quatern, quatern);
#define quatern_cmp(f1,f2)             (quatern_eql(f1,f2) ? 0 : 1)

/***** Quatern: arithmetic *****/
extern void quatern_neg(quatern, quatern);
#ifdef HAVE_MPFR
extern void quatern_abs(bigfr, quatern);
#endif
extern void quatern_norm(bigz, quatern);
extern void quatern_conj(quatern, quatern);
extern void quatern_add(quatern, quatern, quatern);
extern void quatern_sub(quatern, quatern, quatern);
extern void quatern_mul(quatern, quatern, quatern);
extern void quatern_div(quatern, quatern, quatern);
extern void quatern_mod(quatern, quatern, quatern);
extern void quatern_pow(quatern, quatern, unsigned long);

extern Lisp_Object read_quatern_string(char *);
extern int isquatern_string(const char *);

extern void init_optables_QUATERN_T(void);
extern void init_ent_quatern(void);
extern void syms_of_ent_quatern(void);
extern void vars_of_ent_quatern(void);

#endif /* INCLUDED_ent_quatern_h_ */
