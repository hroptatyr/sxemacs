/*
  ase-digraph.h -- Directed 2-ary Graphs
  Copyright (C) 2006, 2007 Sebastian Freundt

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

#ifndef INCLUDED_ase_digraph_h_
#define INCLUDED_ase_digraph_h_ 1

#include "ase.h"
#include "skiplist.h"

typedef struct ase_digraph_s *ase_digraph_t;

extern Lisp_Object Qase_digraph, Qase_digraphp;

extern void LTX_PUBINIT(ase_digraph)(void);
extern void LTX_PUBREINIT(ase_digraph)(void);
extern void LTX_PUBDEINIT(ase_digraph)(void);


struct ase_digraph_s {
	Lisp_Object edges;
	Lisp_Object redges;

	/* options */
	int auto_add_nodes;
	int auto_remove_nodes;
};


#define ASE_DIGRAPHP(_i)						\
	(DYNACATP(_i) && EQ(XDYNACAT(_i)->type, Qase_digraph))
#define CHECK_ASE_DIGRAPH(x)						\
	do {								\
		if (!ASE_DIGRAPHP(x))					\
			dead_wrong_type_argument(Qase_digraphp, x);	\
	} while (0)
#define CONCHECK_ASE_DIGRAPH(x)						\
	do {								\
		if (!ASE_DIGRAPHP(x))					\
			x = wrong_type_argument(Qase_digraphp, x);		\
	} while (0)
#define XSETASE_DIGRAPH(_res, _int)	(_res) = _ase_wrap_digraph((_int))
#define XASE_DIGRAPH(_x)		((ase_digraph_t)get_dynacat(_x))

#define ase_digraph_nodes(_x)	(_x)->edges
#define ase_digraph_edges(_x)	(_x)->edges
#define ase_digraph_redges(_x)	(_x)->redges

extern Lisp_Object _ase_wrap_digraph(ase_digraph_t a);
extern Lisp_Object ase_make_digraph(void);
extern Lisp_Object ase_digraph_add_node(ase_digraph_t, Lisp_Object);
extern void ase_digraph_add_edge_aa(ase_digraph_t, Lisp_Object, Lisp_Object);
extern void ase_digraph_add_edge(ase_digraph_t, Lisp_Object, Lisp_Object);
extern void ase_digraph_remove_edge_ar(ase_digraph_t, Lisp_Object, Lisp_Object);
extern void ase_digraph_remove_edge(ase_digraph_t, Lisp_Object, Lisp_Object);
extern int ase_digraph_has_edge_p(ase_digraph_t, Lisp_Object, Lisp_Object);


#endif	/* INCLUDED_ase_digraph_h_ */
