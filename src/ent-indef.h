#ifndef INCLUDED_ent_indef_h_
#define INCLUDED_ent_indef_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

#define HAVE_INDEF 1

enum indefinite_symbols {
	POS_INFINITY,
	NEG_INFINITY,
	END_OF_COMPARABLE_INFINITIES,
	COMPLEX_INFINITY,
	END_OF_INFINITIES,
	NOT_A_NUMBER,
	NUMBER_INDEFS
};
typedef enum indefinite_symbols indef;


/***************************** Indefinites *****************************/


extern Bufbyte *indef_to_string(indef);

extern void init_optables_INDEF_T(void);
extern void init_ent_indef(void);

extern Lisp_Object Vnot_a_number;
extern Lisp_Object Vpinfinity;
extern Lisp_Object Vninfinity;
extern Lisp_Object Vcomplex_infinity;

#endif /* INCLUDED_ent_indef_h_ */
