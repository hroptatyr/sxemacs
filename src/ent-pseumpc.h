#ifndef INCLUDED_number_pseumpc_h_
#define INCLUDED_number_pseumpc_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

#include "gmp.h"
#include "mpfr.h"

struct bigc
{
	bigfr real;
	bigfr imag;
};
typedef struct bigc bigc[1];


/******************************** Bigcs ********************************/

#define HAVE_PSEUC 1

/***** Bigc: basic functions *****/
extern void bigc_init(bigc);
extern void bigc_init_prec(bigc, unsigned long);
extern void bigc_init_2prec(bigc, unsigned long, unsigned long);
extern void bigc_fini(bigc);
#define bigc_hashcode(f)		(bigfr_hashcode(bigc_re(f)) ^ \
					 bigfr_hashcode(bigc_im(f)))
#define bigc_get_prec(f)		max(bigfr_get_prec(bigc_re(f)), \
					    bigfr_get_prec(bigc_im(f)))
extern void bigc_set_prec(bigc, unsigned long);
#define bigc_set_default_prec(prec) 
#define bigc_get_default_prec()		bigfr_get_default_prec()

/***** Bigc: conversions *****/
extern Bufbyte *bigc_to_string(bigc, int);

/***** Bigc: converting assignments *****/
extern void bigc_set(bigc, bigc);
extern void bigc_set_long(bigc, long);
extern void bigc_set_long_long(bigc, long, long);
extern void bigc_set_ulong(bigc, unsigned long);
extern void bigc_set_ulong_ulong(bigc, unsigned long, unsigned long);
extern void bigc_set_double(bigc, double);
extern void bigc_set_double_double(bigc, double, double);
extern void bigc_set_bigfr(bigc, bigfr);
extern void bigc_set_bigfr_bigfr(bigc, bigfr, bigfr);

#define bigc_re(z)			((z)->real)
#define bigc_im(z)			((z)->imag)

/***** Bigc: comparisons *****/
extern int bigc_eql(bigc, bigc);
#define bigc_cmp(f1,f2)             (bigc_eql(f1,f2) ? 0 : 1)

/***** Bigc: arithmetic *****/
extern void bigc_neg(bigc, bigc);
extern void bigc_abs(bigfr, bigc);
extern void bigc_norm(bigfr, bigc);
extern void bigc_conj(bigc, bigc);
extern void bigc_add(bigc, bigc, bigc);
extern void bigc_sub(bigc, bigc, bigc);
extern void bigc_mul(bigc, bigc, bigc);
extern void bigc_div(bigc, bigc, bigc);
#ifdef HAVE_MPZ
extern void bigc_pow(bigc, bigc, unsigned long);
#endif

extern void bigc_sqrt(bigc, bigc);

/* Advanced functions */
#define bigc_exp(res, f)

/* indefinite handling */
extern int bigc_nan_p(bigc);
extern int bigc_inf_p(bigc);

extern void init_optables_BIGC_T(void);
extern void init_number_mpc(void);


#ifdef HAVE_MPC
#undef HAVE_MPC
#endif
#define HAVE_MPC 1

#endif /* INCLUDED_number_pseumpc_h_ */
