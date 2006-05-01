#ifndef INCLUDED_ent_quatern_h_
#define INCLUDED_ent_quatern_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

#include "gmp.h"

struct quatern
{
	bigz z;
	bigz i;
	bigz j;
	bigz k;
};
typedef struct quatern quatern[1];


/******************************** Quaterns ********************************/

#define HAVE_QUATERN 1

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

#endif /* INCLUDED_ent_quatern_h_ */
