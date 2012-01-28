/*
  ase-metric.h -- Metrical Spaces and Distances
  Copyright (C) 2006, 2007, 2008 Sebastian Freundt

  Author:  Sebastian Freundt <hroptatyr@sxemacs.org>

  * This file is part of SXEmacs.
  *
  * Redistribution and use in source and binary forms, with or without
  * modification, are permitted provided that the following conditions
  * are met:
  *
  * 1. Redistributions of source code must retain the above copyright
  *    notice, this list of conditions and the following disclaimer.
  *
  * 2. Redistributions in binary form must reproduce the above copyright
  *    notice, this list of conditions and the following disclaimer in the
  *    documentation and/or other materials provided with the distribution.
  *
  * 3. Neither the name of the author nor the names of any contributors
  *    may be used to endorse or promote products derived from this
  *    software without specific prior written permission.
  *
  * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
  * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  * DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
  * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
  * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
  * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  */

/* Synched up with: Not in FSF. */

#ifndef INCLUDED_ase_metric_h_
#define INCLUDED_ase_metric_h_ 1

#include "ase.h"

typedef struct ase_metric_s *ase_metric_t;
typedef struct ase_pmetric_data_s *ase_pmetric_data_t;
typedef Lisp_Object(*ase_distance_f)(void*, Lisp_Object, Lisp_Object);

#define EMOD_ASE_DEBUG_METR(args...)	EMOD_ASE_DEBUG("[METR]: " args)

extern Lisp_Object Qase_metric, Qase_metricp;
extern Lisp_Object Qase_euclidean_metric, Qase_supremum_metric;

extern void LTX_PUBINIT(ase_metric)(void);
extern void LTX_PUBREINIT(ase_metric)(void);
extern void LTX_PUBDEINIT(ase_metric)(void);


struct ase_metric_s {
	ase_distance_f dist;
	Lisp_Object ldist;
	Lisp_Object colour;
	/* helper data (e.g. for p-metrics) */
	void *data;
};

struct ase_pmetric_data_s {
	unsigned int p;
};

EXFUN(Fabs, 1);
EXFUN(Fsqrt, 2);
EXFUN(Fpow, 2);
#ifdef HAVE_MPFR
EXFUN(Froot, 3);
#endif


#define ASE_METRICP(_i)							\
	(DYNACATP(_i) && EQ(XDYNACAT_TYPE(_i), Qase_metric))
#define CHECK_ASE_METRIC(x)						\
	do {								\
		if (!ASE_METRICP(x))					\
			dead_wrong_type_argument(Qase_metricp, x);	\
	} while (0)
#define CONCHECK_ASE_METRIC(x)					\
	do {								\
		if (!ASE_METRICP(x))					\
			x = wrong_type_argument(Qase_metricp, x);	\
	} while (0)
extern Lisp_Object _ase_wrap_metric(ase_metric_t);
#define XSETASE_METRIC(_res, _int)	(_res) = _ase_wrap_metric((_int))
#define XASE_METRIC(_x)			((ase_metric_t)get_dynacat(_x))

#define ase_metric_dist(_m)	((_m)->dist)
#define XASE_METRIC_DIST(_m)	(ase_metric_dist(XASE_METRIC(_m)))
#define ase_metric_ldist(_m)	((_m)->ldist)
#define XASE_METRIC_LDIST(_m)	(ase_metric_ldist(XASE_METRIC(_m)))
#define ase_metric_data(_m)	((_m)->data)
#define XASE_METRIC_DATA(_m)	(ase_metric_data(XASE_METRIC(_m)))

/* stuff we provide */
extern Lisp_Object
ase_make_metric(ase_distance_f fn, void *data, Lisp_Object lambda);

#endif	/* INCLUDED_ase_metric_h_ */
