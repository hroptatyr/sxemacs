/*
  ase-digraph.c -- Directed 2-ary Graphs
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

#include "config.h"
#include "sxemacs.h"
#include "ent/ent.h"
#include "ase-digraph.h"

#ifdef ALL_DEBUG_FLAGS
#undef EMOD_ASE_DEBUG_FLAG
#define EMOD_ASE_DEBUG_FLAG
#endif

#define EMOD_ASE_DEBUG_DIGRAPH(args...)	EMOD_ASE_DEBUG("[DIGRAPH]: " args)

PROVIDE(ase_digraph);
REQUIRE(ase_digraph, "ase");

Lisp_Object Qase_digraph, Qase_digraphp;
static Lisp_Object Q_auto_add_nodes;


/* stuff for the dynacat, printers */
static inline Lisp_Object
nodf(Lisp_Object k, Lisp_Object v, void *ptr)
{
	Lisp_Object pcf = (Lisp_Object)ptr;
	write_c_string(" ", pcf);
	print_internal(k, pcf, 0);
	return Qnil;
}

static inline Lisp_Object
edgf(Lisp_Object k, Lisp_Object v, void *ptr)
{
	Lisp_Object pcf = (Lisp_Object)ptr;
	skiplist_t esl;

	if (!SKIPLISTP(v)) {
		return Qnil;
	}
	esl = XSKIPLIST(v);
	write_c_string(" (", pcf);
	print_internal(k, pcf, 0);
	write_c_string("->", pcf);
	map2_skiplist(esl, nodf, ptr);
	write_c_string(")", pcf);
	return Qnil;
}

static void
_ase_digraph_prnt(ase_digraph_t a, Lisp_Object pcf)
{
	skiplist_t sl = XSKIPLIST(ase_digraph_edges(a));

	write_c_string(" :nodes", pcf);
	if (skiplist_empty_p(sl)) {
		write_c_string(" none", pcf);
		return;
	} else {
		map2_skiplist(sl, nodf, (void*)pcf);
		write_c_string(" :edges", pcf);
		map2_skiplist(sl, edgf, (void*)pcf);
	}
	return;
}

static void
ase_digraph_prnt(Lisp_Object obj, Lisp_Object pcf, int SXE_UNUSED(foo))
{
	EMOD_ASE_DEBUG_DIGRAPH("h:0x%016lx@0x%016lx\n",
			       (long unsigned int)(XASE_DIGRAPH(obj)),
			       (long unsigned int)obj);
	write_c_string("#<ase:digraph", pcf);
	_ase_digraph_prnt(XASE_DIGRAPH(obj), pcf);
	write_c_string(">", pcf);
}

static void
ase_digraph_fini(Lisp_Object SXE_UNUSED(obj), int SXE_UNUSED(foo))
{
}

static void
ase_digraph_mark(Lisp_Object obj)
{
	ase_digraph_t a = XASE_DIGRAPH(obj);

	EMOD_ASE_DEBUG_DIGRAPH("g:0x%016lx@0x%016lx shall be marked...\n",
			       (long unsigned int)(XASE_DIGRAPH(obj)),
			       (long unsigned int)obj);

	mark_object(ase_digraph_edges(a));
	mark_object(ase_digraph_redges(a));
	return;
}


static inline ase_digraph_t
_ase_make_digraph(void)
{
	ase_digraph_t a = xnew(struct ase_digraph_s);

	ase_digraph_edges(a) = make_skiplist();
	ase_digraph_redges(a) = make_skiplist();

	a->auto_add_nodes = 0;
	a->auto_remove_nodes = 0;

	EMOD_ASE_DEBUG_DIGRAPH("h:0x%08x shall be created...\n",
			    (unsigned int)a);
	return a;
}

Lisp_Object
_ase_wrap_digraph(ase_digraph_t a)
{
	Lisp_Object result;

	result = make_dynacat(a);
	XDYNACAT(result)->type = Qase_digraph;

	set_dynacat_printer(result, ase_digraph_prnt);
	set_dynacat_marker(result, ase_digraph_mark);
	set_dynacat_finaliser(result, ase_digraph_fini);
	set_dynacat_intprinter(
		result, (dynacat_intprinter_f)_ase_digraph_prnt);

	EMOD_ASE_DEBUG_DIGRAPH("h:0x%016lx shall be wrapped to 0x%016lx...\n",
			       (long unsigned int)a,
			       (long unsigned int)result);

	return result;
}

Lisp_Object ase_make_digraph(void)
{
	ase_digraph_t a = _ase_make_digraph();
	return _ase_wrap_digraph(a);
}


static inline skiplist_t
_ase_digraph_get_edges(ase_digraph_t dg, Lisp_Object node)
{
	skiplist_t sle = XSKIPLIST(ase_digraph_edges(dg));
	Lisp_Object e = get_skiplist(sle, node, Qnil);
	if (!NILP(e))
		return XSKIPLIST(e);
	return NULL;
}

static inline skiplist_t
_ase_digraph_get_redges(ase_digraph_t dg, Lisp_Object node)
{
	skiplist_t slr = XSKIPLIST(ase_digraph_redges(dg));
	Lisp_Object e = get_skiplist(slr, node, Qnil);
	if (!NILP(e))
		return XSKIPLIST(e);
	return NULL;
}

static inline Lisp_Object
__ase_digraph_add_node(skiplist_t sl, Lisp_Object node)
{
	Lisp_Object n = get_skiplist(sl, node, Qnil);

	if (!NILP(n))
		return n;

	n = make_skiplist();
	put_skiplist(sl, node, n);
	return n;
}

static inline void
_ase_digraph_add_node(Lisp_Object *e, Lisp_Object *r,
		      ase_digraph_t dg, Lisp_Object node)
{
	skiplist_t sle = XSKIPLIST(ase_digraph_edges(dg));
	skiplist_t slr = XSKIPLIST(ase_digraph_redges(dg));

	*e = __ase_digraph_add_node(sle, node);
	*r = __ase_digraph_add_node(slr, node);
	return;
}

inline Lisp_Object
ase_digraph_add_node(ase_digraph_t dg, Lisp_Object node)
{
	skiplist_t sle = XSKIPLIST(ase_digraph_edges(dg));
	skiplist_t slr = XSKIPLIST(ase_digraph_redges(dg));

	__ase_digraph_add_node(slr, node);
	return __ase_digraph_add_node(sle, node);
}

void
ase_digraph_add_edge_aa(ase_digraph_t dg, Lisp_Object n1, Lisp_Object n2)
{
/* auto-add nodes if necessary */
	Lisp_Object n1sle, n2slr;
	skiplist_t n1e = NULL, n2r = NULL;

	_ase_digraph_add_node(&n1sle, &n2slr, dg, n1);
	n1e = XSKIPLIST(n1sle);
	_ase_digraph_add_node(&n1sle, &n2slr, dg, n2);
	n2r = XSKIPLIST(n2slr);

	put_skiplist(n1e, n2, Qt);
	put_skiplist(n2r, n1, Qt);
	return;
}

void
ase_digraph_add_edge(ase_digraph_t dg, Lisp_Object n1, Lisp_Object n2)
{
	skiplist_t n1e = NULL, n2r = NULL;

	if (!(n1e = _ase_digraph_get_edges(dg, n1)) ||
	    !(n2r = _ase_digraph_get_redges(dg, n2))) {
		error("no such nodes");
	}
	put_skiplist(n1e, n2, Qt);
	put_skiplist(n2r, n1, Qt);
	return;
}

void
ase_digraph_remove_edge_ar(ase_digraph_t dg, Lisp_Object n1, Lisp_Object n2)
{
	return;
}

void
ase_digraph_remove_edge(ase_digraph_t dg, Lisp_Object n1, Lisp_Object n2)
{
	skiplist_t n1e = NULL, n2r = NULL;

	if ((n1e = _ase_digraph_get_edges(dg, n1)) == NULL ||
	    (n2r = _ase_digraph_get_redges(dg, n2)) == NULL) {
		error("no such edge");
	}
	remove_skiplist(n1e, n2);
	remove_skiplist(n2r, n1);
	return;
}

int
ase_digraph_has_edge_p(ase_digraph_t dg, Lisp_Object n1, Lisp_Object n2)
{
	skiplist_t sl = XSKIPLIST(ase_digraph_edges(dg));
	Lisp_Object n1e = get_skiplist(sl, n1, Qnil);

	if (NILP(n1e)) {
		return 0;
	}
	if (!NILP(get_skiplist(XSKIPLIST(n1e), n2, Qnil))) {
		return 1;
	}
	return 0;
}


/* ###autoload */
DEFUN("ase-digraph", Fase_digraph, 0, 1, 0, /*
Return an empty directed graph.
*/
      (options))
{
	return ase_make_digraph();
}

DEFUN("ase-digraph-add-node", Fase_digraph_add_node, 2, 2, 0, /*
Add NODE to DIGRAPH.
*/
      (digraph, node))
{
	CHECK_ASE_DIGRAPH(digraph);
	ase_digraph_add_node(XASE_DIGRAPH(digraph), node);
	return digraph;
}

DEFUN("ase-digraph-add-edge", Fase_digraph_add_edge, 3, 3, 0, /*
Add edge between NODE1 and NODE2 (in that direction) to DIGRAPH.
*/
      (digraph, node1, node2))
{
	ase_digraph_t dg = NULL;

	CHECK_ASE_DIGRAPH(digraph);

	dg = XASE_DIGRAPH(digraph);
	if (dg->auto_add_nodes) {
		ase_digraph_add_edge_aa(dg, node1, node2);
	} else {
		ase_digraph_add_edge(dg, node1, node2);
	}
	return digraph;
}

DEFUN("ase-digraph-remove-edge", Fase_digraph_remove_edge, 3, 3, 0, /*
Remove edge NODE1->NODE2 from DIGRAPH.
*/
      (digraph, node1, node2))
{
	ase_digraph_t dg = NULL;

	CHECK_ASE_DIGRAPH(digraph);

	dg = XASE_DIGRAPH(digraph);
	if (dg->auto_remove_nodes) {
		ase_digraph_remove_edge_ar(dg, node1, node2);
	} else {
		ase_digraph_remove_edge(dg, node1, node2);
	}
	return digraph;
}

DEFUN("ase-digraph-has-edge-p", Fase_digraph_has_edge_p, 3, 3, 0, /*
Return non-`nil' if an edge between NODE1 and NODE2 exists in DIGRAPH.
*/
      (digraph, node1, node2))
{
	CHECK_ASE_DIGRAPH(digraph);
	if (ase_digraph_has_edge_p(XASE_DIGRAPH(digraph), node1, node2))
		return Qt;
	else
		return Qnil;
}


/* initialiser code */
#define EMODNAME	ase_digraph

void
EMOD_PUBINIT(void)
{
	DEFSUBR(Fase_digraph);
	DEFSUBR(Fase_digraph_add_node);
	DEFSUBR(Fase_digraph_add_edge);
	DEFSUBR(Fase_digraph_remove_edge);
	DEFSUBR(Fase_digraph_has_edge_p);

	DEFASETYPE_WITH_OPS(Qase_digraph, "ase:digraph");
	defsymbol(&Qase_digraphp, "ase:digraphp");
	DEFKEYWORD(Q_auto_add_nodes);

	Fprovide(intern("ase-digraph"));
}

void
EMOD_PUBREINIT(void)
{
}

void
EMOD_PUBDEINIT(void)
{
	Frevoke(intern("ase-digraph"));
}

/* ase-digraph.c ends here */
