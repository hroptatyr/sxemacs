#ifndef INCLUDED_ent_resclass_h_
#define INCLUDED_ent_resclass_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

#include "gmp.h"

#define HAVE_RESCLASS 1

typedef bigz resc_rng;
typedef bigz resc_elm;


/******************************** Rescs ********************************/


/***** basic functions *****/
#define resc_rng_init(r)		bigz_init(r)
#define resc_rng_fini(r)		bigz_fini(r)
#define resc_rng_hashcode(r)		bigz_hashcode(r)
#define resc_elm_init(e)		bigz_init(e)
#define resc_elm_fini(e)		bigz_fini(e)
#define resc_elm_hashcode(e)		bigz_hashcode(e)

/***** conversions *****/
#define resc_rng_to_string(r, b)	bigz_to_string(r, b)
#define resc_elm_to_string(e, b)	bigz_to_string(e, b)


extern Lisp_Object read_resclass_string(char*);
extern Lisp_Object read_resclassring_string(char*);

extern void init_optables_RESC_ELM_T(void);
extern void init_ent_resclass(void);


#endif /* INCLUDED_ent_resclass_h_ */
