/*
  cl-loop.h -- Common Lisp Goodness, the fast version
  Copyright (C) 2006-2012 Sebastian Freundt

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

#ifndef INCLUDED_cl_loop_h_
#define INCLUDED_cl_loop_h_ 1

#include "cl.h"
#include "ent/ent-optable.h"
#include "ent/ent-binary-op.h"
#include "ent/ent-binary-rel.h"
#include "ent/ent-int.h"
#include "ent/ent-indef.h"

#define EMOD_CL_DEBUG_LOOP(args...)	EMOD_CL_DEBUG("[loop]: " args)

extern Lisp_Object Qcl_loop_sentence, Qcl_loop_sentence_p;
extern Lisp_Object Qcl_loop_for_clause, Qcl_loop_for_clause_p;
extern Lisp_Object Qcl_loop_do_clause, Qcl_loop_do_clause_p;
extern Lisp_Object Qcl_loop_with_clause, Qcl_loop_with_clause_p;
extern Lisp_Object Qcl_loop_repeat_clause, Qcl_loop_repeat_clause_p;
extern Lisp_Object Qcl_loop_append_clause, Qcl_loop_append_clause_p;
extern Lisp_Object Qcl_loop_collect_clause, Qcl_loop_collect_clause_p;
extern Lisp_Object Qcl_loop_nconc_clause, Qcl_loop_nconc_clause_p;
extern Lisp_Object Qcl_loop_return_clause, Qcl_loop_return_clause_p;
extern Lisp_Object Qcl_loop_initally_clause, Qcl_loop_initially_clause_p;
extern Lisp_Object Qcl_loop_finally_clause, Qcl_loop_finally_clause_p;
extern Lisp_Object Qcl_loop_count_clause, Qcl_loop_count_clause_p;
extern Lisp_Object Qcl_loop_sum_clause, Qcl_loop_sum_clause_p;
extern Lisp_Object Qcl_loop_maximise_clause, Qcl_loop_maximise_clause_p;
extern Lisp_Object Qcl_loop_minimise_clause, Qcl_loop_minimise_clause_p;

extern Lisp_Object Qfor, Qas;
extern Lisp_Object Qfrom, Qdownfrom, Qupfrom;
extern Lisp_Object Qto, Qdownto, Qupto, Qabove, Qbelow, Qby;
extern Lisp_Object Qin, Qon, Qthen, Qacross, Qeach, Qthe, Qbeing, Qof;
extern Lisp_Object Qhash_key, Qhash_keys, Qhash_value, Qhash_values, Qusing;
extern Lisp_Object Qdo, Qdoing;
extern Lisp_Object Qtoken;
extern Lisp_Object Qwith, Qequals, Qand;
extern Lisp_Object Qrepeat;
extern Lisp_Object Qappend, Qappending, Qcollect, Qcollecting;
extern Lisp_Object Qnconc, Qnconcing, Qinto;
extern Lisp_Object Qcount, Qcounting, Qsum, Qsumming;
extern Lisp_Object Qmaximise, Qmaximising;
extern Lisp_Object Qminimise, Qminimising;
extern Lisp_Object Qinitially, Qfinally;

/* a whole loop sentence made up from various clauses */
typedef struct cl_loop_sentence_s cl_loop_sentence_t;
/* clauses */
typedef struct cl_loop_for_clause_s cl_loop_for_clause_t;
typedef struct cl_loop_do_clause_s cl_loop_do_clause_t;
typedef struct cl_loop_with_clause_s cl_loop_with_clause_t;
typedef struct cl_loop_repeat_clause_s cl_loop_repeat_clause_t;
typedef struct cl_loop_append_clause_s cl_loop_append_clause_t;
typedef struct cl_loop_accu_clause_s cl_loop_accu_clause_t;
typedef struct cl_loop_inifinret_clause_s cl_loop_inifinret_clause_t;

#include "cl-loop-parser.h"

/* nasty nasty nasty
 * tg reported that bison 2.4.1 may not define YYSTYPE, we go the
 * utmost safe way and just define it here */
#undef YYSTYPE
#define YYSTYPE	int

/* bison stuff */
extern int
cl_loop_yylex(YYSTYPE*, Lisp_Object*,
	      cl_loop_sentence_t*, Lisp_Object*, Lisp_Object*);
extern void
cl_loop_yyerror(Lisp_Object*,
		cl_loop_sentence_t*, Lisp_Object*, Lisp_Object*, char*);
extern int
cl_loop_yyparse(Lisp_Object*,
		cl_loop_sentence_t*, Lisp_Object*, Lisp_Object*);


/* a loop sentence is a set of loop clauses */
struct cl_loop_sentence_s {
	Lisp_Object prologue;
	Lisp_Object epilogue;
	Lisp_Object iteration;

	/* during runtime */
	int state;
	Lisp_Object result;
#if 0
	sxe_mutex_t lsen_mtx;
#endif	/* not yet */
};

struct cl_loop_for_clause_s {
	Lisp_Object form1;

	enum {
		FOR_INVALID_CLAUSE,
		FOR_ARITHMETIC_CLAUSE,
		FOR_IN_SUBLIST_CLAUSE,
		FOR_ON_SUBLIST_CLAUSE,
		FOR_ACROSS_ARRAY_CLAUSE,
		FOR_EQUALS_THEN_CLAUSE,
		FOR_OF_HASHTABLE_CLAUSE
	} for_subclause;

	/* for the arith subclause */
	Lisp_Object from;
	Lisp_Object to;
	Lisp_Object by;
	ase_binary_operation_t byop;
	ase_binary_relation_t torel;
	int torel_strictp;

	/* for the in, on, and across subclauses */
	Lisp_Object inonacross;

	/* for the =-then subclause */
	Lisp_Object equals;
	Lisp_Object then;

	/* for the being-each-hash-key/value subclause */
	Lisp_Object hash_keyvar;
	Lisp_Object hash_valvar;

	/* for parallel stepping for clauses */
	Lisp_Object next;
	int depth;

	/* during runtime */
	Lisp_Object curval;
	Lisp_Object curbound;
	Lisp_Object curstep;
	long counter;
	long bound;
	void *ptr1;
	void *ptr2;
};

struct cl_loop_do_clause_s {
	Lisp_Object form;
};

struct cl_loop_repeat_clause_s {
	Lisp_Object form;
	long counter;
};

struct cl_loop_with_clause_s {
	Lisp_Object varform;
	Lisp_Object valform;
	/* for parallel with clauses */
	Lisp_Object next;
	int depth;
};

struct cl_loop_accu_clause_s {
	Lisp_Object form;
	Lisp_Object into;
	/* state */
	Lisp_Object cur;
};

struct cl_loop_inifinret_clause_s {
	Lisp_Object form;
};


#define CL_LOOP_SENTENCEP(_i)						\
	(DYNACATP(_i) && EQ(XDYNACAT_TYPE(_i), Qcl_loop_sentence))
#define CHECK_CL_LOOP_SENTENCE(x)					\
	do {								\
		if (!CL_LOOP_SENTENCEP(x))				\
			dead_wrong_type_argument(Qcl_loop_sentence_p, x); \
	} while (0)
#define CONCHECK_CL_LOOP_SENTENCE(x)					\
	do {								\
		if (!CL_LOOP_SENTENCEP(x))				\
			x = wrong_type_argument(Qcl_loop_sentence_p, x); \
	} while (0)
#define XCL_LOOP_SENTENCE(_x)	((cl_loop_sentence_t*)get_dynacat(_x))


extern Lisp_Object cl_loop_make_for_clause(Lisp_Object);
extern Lisp_Object cl_loop_make_do_clause(Lisp_Object);
extern Lisp_Object cl_loop_make_with_clause(Lisp_Object);
extern Lisp_Object cl_loop_make_repeat_clause(Lisp_Object);
extern Lisp_Object cl_loop_make_append_clause(Lisp_Object);
extern Lisp_Object cl_loop_make_collect_clause(Lisp_Object);
extern Lisp_Object cl_loop_make_nconc_clause(Lisp_Object);
extern Lisp_Object cl_loop_make_return_clause(Lisp_Object);
extern Lisp_Object cl_loop_make_initially_clause(Lisp_Object);
extern Lisp_Object cl_loop_make_finally_clause(Lisp_Object);
extern Lisp_Object cl_loop_make_count_clause(Lisp_Object);
extern Lisp_Object cl_loop_make_sum_clause(Lisp_Object);
extern Lisp_Object cl_loop_make_maximise_clause(Lisp_Object);
extern Lisp_Object cl_loop_make_minimise_clause(Lisp_Object);


extern void cl_loop_LTX_init(void);
extern void cl_loop_LTX_deinit(void);
extern void cl_loop_LTX_reinit(void);

#endif	/* INCLUDED_cl_loop_h_ */
