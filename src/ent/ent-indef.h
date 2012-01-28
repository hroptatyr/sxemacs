/*
  ent-indef.h -- Indefinite symbols for SXEmacs
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


#ifndef INCLUDED_ent_indef_h_
#define INCLUDED_ent_indef_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

extern Lisp_Object ent_lift_INDEF_T_COMPARABLE(Lisp_Object, ent_lift_args_t);

typedef enum indefinite_symbols indef;
typedef struct Lisp_Indef Lisp_Indef;


enum indefinite_symbols {
	POS_INFINITY,
	NEG_INFINITY,
	END_OF_COMPARABLE_INFINITIES,
	COMPLEX_INFINITY,
	END_OF_INFINITIES,
	NOT_A_NUMBER,
	NUMBER_INDEFS
};

struct Lisp_Indef
{
	struct lrecord_header lheader;
	indef data;
};

DECLARE_LRECORD(indef, Lisp_Indef);
#define XINDEF(x) XRECORD(x, indef, Lisp_Indef)
#define XSETINDEF(x, p) XSETRECORD(x, p, indef)
#define wrap_indef(p) wrap_object(p)
#define INDEFP(x) RECORDP(x, indef)
#define CHECK_INDEF(x) CHECK_RECORD(x, indef)
#define CONCHECK_INDEF(x) CONCHECK_RECORD(x, indef)

#define indef_data(f) ((f)->data)
#define XINDEF_DATA(x) indef_data(XINDEF(x))

#define INFINITYP(x)							\
	(INDEFP(x) && (XINDEF_DATA(x) < END_OF_INFINITIES))
#define COMPARABLE_INDEF_P(x)						\
	(INFINITYP(x) && (XINDEF_DATA(x) < END_OF_COMPARABLE_INFINITIES))
#define INFINITE_POINT_P(x)						\
	(INFINITYP(x) && (XINDEF_DATA(x) > END_OF_COMPARABLE_INFINITIES))
#define NOT_A_NUMBER_P(x)				\
	(INDEFP(x) && (XINDEF_DATA(x) > END_OF_INFINITIES))

extern Lisp_Object make_indef_internal(indef);
extern Lisp_Object make_indef(indef);


/***************************** Indefinites *****************************/


extern Bufbyte *indef_to_string(indef);

extern void init_optables_INDEF_T(void);
extern void init_ent_indef(void);
extern void vars_of_ent_indef(void);
extern void syms_of_ent_indef(void);

extern Lisp_Object Vnot_a_number;
extern Lisp_Object Vpinfinity;
extern Lisp_Object Vninfinity;
extern Lisp_Object Vcomplex_infinity;

#endif /* INCLUDED_ent_indef_h_ */
