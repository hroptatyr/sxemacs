#ifndef INCLUDED_number_gaussian_h_
#define INCLUDED_number_gaussian_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

#include "gmp.h"

struct bigg
{
	bigz intg;
	bigz imag;
};
typedef struct bigg bigg[1];


/******************************** Biggs ********************************/

#define HAVE_PSEUG 1

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
#ifdef HAVE_MPFR
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
extern void init_number_gaussian(void);

#endif /* INCLUDED_number_gaussian_h_ */
