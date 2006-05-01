#ifndef INCLUDED_ent_int_h_
#define INCLUDED_ent_int_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif



extern void init_optables_INT_T(void);
extern void init_ent_int(void);


#endif /* INCLUDED_ent_int_h_ */
