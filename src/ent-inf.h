/*
  ent-inf.h -- Infinite and Nan Predicates
  Copyright (C) 2005, 2006 Sebastian Freundt
  Copyright (C) 2006 Nelson Ferreira

  Author:  Sebastian Freundt
           Nelson Ferreira

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
#ifndef INCLUDED_ent_inf_h_
#define INCLUDED_ent_inf_h_

#if defined(HAVE_FPCLASSIFY)
#define ENT_FLOAT_INF_P(_val)	(fpclassify(_val)==FP_INFINITE)
#elif defined(HAVE_INFINITY)
#define ENT_FLOAT_INF_P(_val)	((_val) == INFINITY || (_val) == -INFINITY)
#elif defined(HAVE_ISINF)
#define ENT_FLOAT_INF_P(_val)	(isinf(_val))
#else
#define ENT_FLOAT_INF_P(_val)	(0)
#warning infinity detection does not work
#warning assuming there are no infinities on your system
#endif

#if defined(HAVE_FPCLASSIFY)
#define ENT_FLOAT_PINF_P(_val)	((fpclassify(_val)==FP_INFINITE) && (_val) > 0)
#elif defined(HAVE_INFINITY)
#define ENT_FLOAT_PINF_P(_val)	((_val) == INFINITY)
#elif defined(HAVE_ISINF)
#define ENT_FLOAT_PINF_P(_val)	(isinf(_val) && (_val) > 0)
#else
#define ENT_FLOAT_PINF_P(_val)	(0)
#endif

#if defined(HAVE_FPCLASSIFY)
#define ENT_FLOAT_NINF_P(_val)	((fpclassify(_val)==FP_INFINITE) && (_val) < 0)
#elif defined(HAVE_INFINITY)
#define ENT_FLOAT_NINF_P(_val)	((_val) == -INFINITY)
#elif defined(HAVE_ISINF)
#define ENT_FLOAT_NINF_P(_val)	(isinf(_val) && (_val) < 0)
#else
#define ENT_FLOAT_NINF_P(_val)	(0)
#endif

#if defined(HAVE_FPCLASSIFY)
#define ENT_FLOAT_NAN_P(_val)	(fpclassify(_val)==FP_NAN)
#elif defined(HAVE_NAN)
#define ENT_FLOAT_NAN_P(_val)	((_val) == NAN)
#elif defined(HAVE_ISNAN)
#define ENT_FLOAT_NAN_P(_val)	(isnan(_val))
#else
#define ENT_FLOAT_NAN_P(_val)	((_val) != (_val))
#warning NAN detection possibly broken
#warning Hate mails please to your system vendor.
#endif

#if defined(HAVE_FPCLASSIFY)
#define ENT_FLOAT_INDEFINITE_P(_val)	(fpclassify(_val)!=FP_NORMAL)
#elif defined(HAVE_NAN) && defined(HAVE_INFINITY)
#define ENT_FLOAT_INDEFINITE_P(_val)	(((_val) == NAN)||((_val) == INFINITY))
#elif defined(HAVE_ISNAN) && defined(HAVE_ISINF)
#define ENT_FLOAT_INDEFINITE_P(_val)	(isnan(_val)||isinf(_val))
#else
#define ENT_FLOAT_INDEFINITE_P(_val)	(ENT_FLOAT_INF_P(_val)||\
					 ENT_FLOAT_NAN_P(_val))
#if ! ((defined(HAVE_NAN) || defined(HAVE_INFINITY)) &&	\
       (defined(HAVE_ISNAN) || defined(HAVE_ISINF)))
#warning Indefinites detection possibly broken
#warning Hate mails please to your system vendor.
#endif
#endif

#endif /* INCLUDED_ent_inf_h_ */
