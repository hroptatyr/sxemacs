#ifndef INCLUDED_number_mpc_h_
#define INCLUDED_number_mpc_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

#include "gmp.h"
#include "mpfr.h"
#include "mpc.h"


typedef mpc_t bigc;


/********************************* Bigcs ********************************/

#define HAVE_MPC 1

/***** Bigc: basic functions *****/
#define bigc_init(f)                mpc_init(f)
#define bigc_init_prec(f,prec)      mpc_init2(f, prec)
#define bigc_init_2prec(f,p1,p2)    mpc_init3(f, p1, p2)
#define bigc_fini(f)                mpc_clear(f)
#define bigc_hashcode(f)            (bigfr_hashcode(bigc_re(f)) ^ \
				     bigfr_hashcode(bigc_im(f)))
#define bigc_get_prec(f)            max(bigfr_get_prec(bigc_re(f)), \
					bigfr_get_prec(bigc_im(f)))
#define bigc_set_prec(f, prec)      mpc_set_prec(f, prec)
#define bigc_set_default_prec(prec) mpc_set_default_prec(prec)
#define bigc_get_default_prec()     mpc_get_default_prec()

/***** Bigc: conversions *****/
extern Bufbyte *bigc_to_string(bigc, int);

/***** Bigc: converting assignments *****/
#define bigc_set(f1,f2)             mpc_set(f1, f2, GMP_RNDN)
#define bigc_set_long(f,l)          mpc_set_si(f, l, GMP_RNDN)
#define bigc_set_ulong(f,l)         mpc_set_ui(f, l, GMP_RNDN)
#define bigc_set_double(f,d)        mpc_set_d(f, d, GMP_RNDN)
#define bigc_set_double_double(f,d1,d2) mpc_set_d_d(f, d1, d2, GMP_RNDN)
#define bigc_set_ulong_ulong(f,u1,u2)   mpc_set_ui_ui(f, u1, u2, GMP_RNDN)
#define bigc_set_long_long(f, l1, l2)   mpc_set_si_si(f, l1, l2, GMP_RNDN)
#define bigc_set_bigfr(c, bf) do					\
{									\
	mpc_set_ui_ui(c, 0UL, 0UL, GMP_RNDN);				\
	mpc_add_fr(c, c, bf, GMP_RNDN);					\
} while (0)
#define bigc_set_bigfr_bigfr(c, bf1, bf2) do				\
{									\
	mpc_set_ui_fr(c, 0UL, bf2, GMP_RNDN);				\
	mpc_add_fr(c, c, bf1, GMP_RNDN);				\
} while (0)

#define bigc_re(z)                  MPC_RE(z)
#define bigc_im(z)                  MPC_IM(z)

/***** Bigc: comparisons *****/
#define bigc_cmp(f1,f2)             mpc_cmp(f1, f2)
#define bigc_eql(f1,f2)             (bigfr_eql(bigc_re(f1), bigc_re(f2)) && \
				     bigfr_eql(bigc_im(f1), bigc_im(f2)))

/***** Bigc: arithmetic *****/
#define bigc_neg(f,f2)              mpc_neg(f, f2, GMP_RNDN)
#define bigc_abs(f,f2)              mpc_abs(f, f2, GMP_RNDN)
#define bigc_norm(fr,c)             mpc_norm(fr, c, GMP_RNDN)
#define bigc_conj(c,c2)             mpc_conj(c, c2, GMP_RNDN)
#define bigc_add(res,f1,f2)         mpc_add(res, f1, f2, GMP_RNDN)
#define bigc_sub(res,f1,f2)         mpc_sub(res, f1, f2, GMP_RNDN)
#define bigc_mul(res,f1,f2)         mpc_mul(res, f1, f2, GMP_RNDN)
#define bigc_div(res,f1,f2)         mpc_div(res, f1, f2, GMP_RNDN)

#define bigc_sqrt(res,f)            mpc_sqrt(res, f, GMP_RNDN)

extern void bigc_pow(bigc, bigc, unsigned long);

/* Advanced functions */
#define bigc_exp(res, f)            mpc_exp(res, f, GMP_RNDN);

/* indefinite handling */
extern int bigc_nan_p(bigc);
extern int bigc_inf_p(bigc);

extern Lisp_Object read_bigc_string(char*);

extern void init_optables_BIGC_T(void);
extern void init_number_mpc(void);


#endif /* INCLUDED_number_mpc_h_ */
