#ifndef INCLUDED_number_ecm_h_
#define INCLUDED_number_ecm_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

#include "ecm.h"


extern void init_number_ecm(void);


/********************************* ECM **************************************/


#endif /* INCLUDED_number_ecm_h_ */
