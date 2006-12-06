/*
  ent-resclass.h -- Residue Class Rings for SXEmacs
  Copyright (C) 2005, 2006 Sebastian Freundt

  Author:  Sebastian Freundt

  * This file is part of SXEmacs.
  * 
  * SXEmacs is free software; you can redistribute it and/or modify it
  * under the terms of the GNU General Public License as published by the
  * Free Software Foundation; either version 2, or (at your option) any
  * later version.
  * 
  * SXEmacs is distributed in the hope that it will be useful, but WITHOUT
  * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  * for more details.
  * 
  * You should have received a copy of the GNU General Public License
  * along with SXEmacs; see the file COPYING.  If not, write to
  * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  * Boston, MA 02111-1307, USA.
  */

#ifndef INCLUDED_ent_resclass_h_
#define INCLUDED_ent_resclass_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

#include "gmp.h"

typedef bigz resc_rng;
typedef bigz resc_elm;


/******************************** Rescs ********************************/
struct Lisp_Resc_Rng
{
	struct lrecord_header lheader;
	resc_rng data;
};
typedef struct Lisp_Resc_Rng Lisp_Resc_Rng;

struct Lisp_Resc_Elm
{
	struct lrecord_header lheader;
	resc_elm data;
	Lisp_Object ring;
};
typedef struct Lisp_Resc_Elm Lisp_Resc_Elm;

DECLARE_LRECORD(resc_rng, Lisp_Resc_Rng);
DECLARE_LRECORD(resc_elm, Lisp_Resc_Elm);
#define XRESC_RNG(x) XRECORD(x, resc_rng, Lisp_Resc_Rng)
#define XSETRES_RNG(x, p) XSETRECORD(x, p, resc_rng)
#define wrap_resc_rng(p) wrap_object(p)
#define RESC_RNGP(x) RECORDP(x, resc_rng)
#define CHECK_RESC_RNG(x) CHECK_RECORD(x, resc_rng)
#define CONCHECK_RESC_RNG(x) CONCHECK_RECORD(x, resc_rng)

#define XRESC_ELM(x) XRECORD(x, resc_elm, Lisp_Resc_Elm)
#define wrap_resc_elm(p) wrap_object(p)
#define RESC_ELMP(x) RECORDP(x, resc_elm)
#define CHECK_RESC_ELM(x) CHECK_RECORD(x, resc_elm)
#define CONCHECK_RESC_ELM(x) CONCHECK_RECORD(x, resc_elm)

#define resc_rng_data(f) ((f)->data)
#define XRESC_RNG_DATA(x) resc_rng_data(XRESC_RNG(x))

#define resc_elm_data(f) ((f)->data)
#define resc_elm_ring(f) ((f)->ring)
#define XRESC_ELM_DATA(x) resc_elm_data(XRESC_ELM(x))
#define XRESC_ELM_RING(x) resc_elm_ring(XRESC_ELM(x))

#if 0
#define RESC_ELM_ARITH_RETURN(f,op) do					\
{									\
	Lisp_Object retval = make_bigc_bc(XBIGC_DATA(f));		\
	bigc_##op(XBIGC_DATA(retval), XBIGC_DATA(f));			\
	return retval;							\
} while (0)

#define BIGC_ARITH_RETURN1(f,op,arg) do					\
{									\
	Lisp_Object retval = make_bigc_bc(XBIGC_DATA(f));		\
	bigc_##op(XBIGC_DATA(retval), XBIGC_DATA(f), arg);		\
	return retval;							\
} while (0)
#endif	/* 0 */

extern Lisp_Object make_resc_rng(unsigned long);
extern Lisp_Object make_resc_rng_bz(bigz);
extern Lisp_Object make_resc_elm(long, Lisp_Object);
extern Lisp_Object make_resc_elm_bz(bigz, Lisp_Object);


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
extern Lisp_Object internal_coerce_to_RESC_ELM_T(Lisp_Object, Lisp_Object);

extern void init_optables_RESC_ELM_T(void);
extern void init_ent_resclass(void);
extern void syms_of_ent_resclass(void);
extern void vars_of_ent_resclass(void);

#endif /* INCLUDED_ent_resclass_h_ */
