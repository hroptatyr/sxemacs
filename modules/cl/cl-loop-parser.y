%{
/* format.y - SXEmacs format function

   Copyright (C) 2006 Sebastian Freundt.

This file is part of SXEmacs.

SXEmacs is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

SXEmacs is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */

/* Synched up with: Not in FSF. */

#include <config.h>
#include <sxemacs.h>
#include "cl-loop.h"
#include "cl-loop-parser.h"

#define YYENABLE_NLS		0
#define YYLTYPE_IS_TRIVIAL	1

static inline Lisp_Object
cl_loop_frob_scanner(Lisp_Object *scanner)
	__attribute__((always_inline));
static inline Lisp_Object
cl_loop_frob_scanner(Lisp_Object *scanner)
{
	if (!NILP(*scanner)) {
		return XCAR(*scanner);
	} else {
		Fsignal(Qinvalid_read_syntax, *scanner);
		return Qnil;
	}
}

static inline void
cl_loop_step_scanner(Lisp_Object *scanner)
	__attribute__((always_inline));
static inline void
cl_loop_step_scanner(Lisp_Object *scanner)
{
	if (!NILP(*scanner)) {
		*scanner = XCDR(*scanner);
	}
}

%}

%name-prefix="cl_loop_yy"
%pure_parser
%defines

%parse-param {Lisp_Object *yyscanner}
%lex-param {Lisp_Object *yyscanner}
%parse-param {cl_loop_sentence_t *lsen}
%lex-param {cl_loop_sentence_t *lsen}
%parse-param {Lisp_Object *context}
%lex-param {Lisp_Object *context}
%parse-param {Lisp_Object *token}
%lex-param {Lisp_Object *token}

%token /* <forkeys> */
	FROM
	TO
	BELOW
	ABOVE
	BY
	IN
	ON
	THEN
	ACROSS
	EACH
	BEING
	HASH_KEY
	HASH_VALUE
	USING

%token /* <withkeys> */
	WITH
	EQUALS

%token /* <gatherers> */
	APPEND
	COLLECT
	NCONC
	SUM
	COUNT
	MINIMISE
	MAXIMISE

%token /* <terminators> */
	FOR
	REPEAT
	WHILE
	UNTIL
	ALWAYS
	NEVER
	THEREIS

%token /* <terminators> */
	DO
	IF
	WHEN
	UNLESS
	ELSE
	END

%token /* <misc> */
	AND
	RETURN
	INITIALLY
	FINALLY
	NAMED
	FORM
	INTO

%expect 0

%%

/* RULES */
loop:
/* EMPTY */ |
clause loop;

clause:
for_clauses |
do_clause {
	dllist_t iter = XDLLIST(lsen->iteration);
	Lisp_Object do_clause = *context;

	EMOD_CL_DEBUG_LOOP("accepting DO clause\n");
	dllist_append(iter, (void*)do_clause);
} | 
with_clause {
	dllist_t pro = XDLLIST(lsen->prologue);
	Lisp_Object with_clause = *context;

	EMOD_CL_DEBUG_LOOP("accepting WITH clause\n");
	dllist_append(pro, (void*)with_clause);
} |
repeat_clause {
	dllist_t pro = XDLLIST(lsen->prologue);
	dllist_t iter = XDLLIST(lsen->iteration);
	Lisp_Object repeat_clause = *context;

	EMOD_CL_DEBUG_LOOP("accepting REPEAT clause\n");
	dllist_append(pro, (void*)repeat_clause);
	dllist_append(iter, (void*)repeat_clause);
} |
append_clause {
	dllist_t pro = XDLLIST(lsen->prologue);
	dllist_t iter = XDLLIST(lsen->iteration);
	dllist_t epi = XDLLIST(lsen->epilogue);
	Lisp_Object append_clause = *context;

	EMOD_CL_DEBUG_LOOP("accepting APPEND clause\n");
	dllist_append(pro, (void*)append_clause);
	dllist_append(iter, (void*)append_clause);
	dllist_append(epi, (void*)append_clause);
} |
collect_clause {
	dllist_t pro = XDLLIST(lsen->prologue);
	dllist_t iter = XDLLIST(lsen->iteration);
	dllist_t epi = XDLLIST(lsen->epilogue);
	Lisp_Object collect_clause = *context;

	EMOD_CL_DEBUG_LOOP("accepting COLLECT clause\n");
	dllist_append(pro, (void*)collect_clause);
	dllist_append(iter, (void*)collect_clause);
	dllist_append(epi, (void*)collect_clause);
} |
nconc_clause {
	dllist_t pro = XDLLIST(lsen->prologue);
	dllist_t iter = XDLLIST(lsen->iteration);
	dllist_t epi = XDLLIST(lsen->epilogue);
	Lisp_Object nconc_clause = *context;

	EMOD_CL_DEBUG_LOOP("accepting NCONC clause\n");
	dllist_append(pro, (void*)nconc_clause);
	dllist_append(iter, (void*)nconc_clause);
	dllist_append(epi, (void*)nconc_clause);
} |
sum_clause {
	dllist_t pro = XDLLIST(lsen->prologue);
	dllist_t iter = XDLLIST(lsen->iteration);
	dllist_t epi = XDLLIST(lsen->epilogue);
	Lisp_Object accu_clause = *context;

	EMOD_CL_DEBUG_LOOP("accepting SUM clause\n");
	dllist_append(pro, (void*)accu_clause);
	dllist_append(iter, (void*)accu_clause);
	dllist_append(epi, (void*)accu_clause);
} |
count_clause {
	dllist_t pro = XDLLIST(lsen->prologue);
	dllist_t iter = XDLLIST(lsen->iteration);
	dllist_t epi = XDLLIST(lsen->epilogue);
	Lisp_Object accu_clause = *context;

	EMOD_CL_DEBUG_LOOP("accepting count clause\n");
	dllist_append(pro, (void*)accu_clause);
	dllist_append(iter, (void*)accu_clause);
	dllist_append(epi, (void*)accu_clause);
} |
maximise_clause {
	dllist_t pro = XDLLIST(lsen->prologue);
	dllist_t iter = XDLLIST(lsen->iteration);
	dllist_t epi = XDLLIST(lsen->epilogue);
	Lisp_Object accu_clause = *context;

	EMOD_CL_DEBUG_LOOP("accepting MAXIMISE clause\n");
	dllist_append(pro, (void*)accu_clause);
	dllist_append(iter, (void*)accu_clause);
	dllist_append(epi, (void*)accu_clause);
} |
minimise_clause {
	dllist_t pro = XDLLIST(lsen->prologue);
	dllist_t iter = XDLLIST(lsen->iteration);
	dllist_t epi = XDLLIST(lsen->epilogue);
	Lisp_Object accu_clause = *context;

	EMOD_CL_DEBUG_LOOP("accepting MINIMISE clause\n");
	dllist_append(pro, (void*)accu_clause);
	dllist_append(iter, (void*)accu_clause);
	dllist_append(epi, (void*)accu_clause);
} |
initially_clause {
	dllist_t pro = XDLLIST(lsen->prologue);
	Lisp_Object initially_clause = *context;

	EMOD_CL_DEBUG_LOOP("accepting INITIALLY clause\n");
	dllist_append(pro, (void*)initially_clause);
} |
finally_clause {
	dllist_t epi = XDLLIST(lsen->epilogue);
	Lisp_Object finally_clause = *context;

	EMOD_CL_DEBUG_LOOP("accepting FINALLY clause\n");
	dllist_append(epi, (void*)finally_clause);
} |
return_clause {
	dllist_t epi = XDLLIST(lsen->epilogue);
	Lisp_Object return_clause = *context;

	EMOD_CL_DEBUG_LOOP("accepting RETURN clause\n");
	dllist_append(epi, (void*)return_clause);
}
/* | while | until | always | never | thereis
 * 	if | when | unless | else | end | named
 */
;

/* clauses in greater detail */
do_clause:
DO
{
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("DO clause found\n");
	*context = cl_loop_make_do_clause(form);
	cl_loop_step_scanner(yyscanner);
};

append_clause:
append | append into;

collect_clause:
collect | collect into;

nconc_clause:
nconc | nconc into;

count_clause:
count | count into;

sum_clause:
sum | sum into;

minimise_clause:
minimise | minimise into;

maximise_clause:
maximise | maximise into;

append:
APPEND
{
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("APPEND clause found\n");
	*context = cl_loop_make_append_clause(form);
	cl_loop_step_scanner(yyscanner);
};

collect:
COLLECT
{
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("COLLECT clause found\n");
	*context = cl_loop_make_collect_clause(form);
	cl_loop_step_scanner(yyscanner);
};

nconc:
NCONC
{
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("NCONC clause found\n");
	*context = cl_loop_make_nconc_clause(form);
	cl_loop_step_scanner(yyscanner);
};

count:
COUNT
{
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("COUNT clause found\n");
	*context = cl_loop_make_count_clause(form);
	cl_loop_step_scanner(yyscanner);
};

sum:
SUM
{
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("SUM clause found\n");
	*context = cl_loop_make_sum_clause(form);
	cl_loop_step_scanner(yyscanner);
};

maximise:
MAXIMISE
{
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("MAXIMISE clause found\n");
	*context = cl_loop_make_maximise_clause(form);
	cl_loop_step_scanner(yyscanner);
};

minimise:
MINIMISE
{
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("MINIMISE clause found\n");
	*context = cl_loop_make_minimise_clause(form);
	cl_loop_step_scanner(yyscanner);
};

into:
INTO
{
	Lisp_Object acn_clause = *context;
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("INTO found\n");
	if (EQ(get_dynacat_type(acn_clause), Qcl_loop_collect_clause)) {
		cl_loop_accu_clause_t *cc = get_dynacat(acn_clause);
		cc->into = form;
	} else if (EQ(get_dynacat_type(acn_clause), Qcl_loop_append_clause)) {
		cl_loop_accu_clause_t *ac = get_dynacat(acn_clause);
		ac->into = form;
	} else if (EQ(get_dynacat_type(acn_clause), Qcl_loop_nconc_clause)) {
		cl_loop_accu_clause_t *nc = get_dynacat(acn_clause);
		nc->into = form;
	} else if (EQ(get_dynacat_type(acn_clause), Qcl_loop_count_clause)) {
		cl_loop_accu_clause_t *nc = get_dynacat(acn_clause);
		nc->into = form;
	} else if (EQ(get_dynacat_type(acn_clause), Qcl_loop_sum_clause)) {
		cl_loop_accu_clause_t *nc = get_dynacat(acn_clause);
		nc->into = form;
	} else if (EQ(get_dynacat_type(acn_clause), Qcl_loop_minimise_clause)) {
		cl_loop_accu_clause_t *nc = get_dynacat(acn_clause);
		nc->into = form;
	} else if (EQ(get_dynacat_type(acn_clause), Qcl_loop_maximise_clause)) {
		cl_loop_accu_clause_t *nc = get_dynacat(acn_clause);
		nc->into = form;
	} else {
		/* uh oh */
	}
	cl_loop_step_scanner(yyscanner);
};

repeat_clause:
REPEAT
{
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("REPEAT clause found\n");
	*context = cl_loop_make_repeat_clause(form);
	cl_loop_step_scanner(yyscanner);
};

return_clause:
RETURN
{
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("RETURN clause found\n");
	*context = cl_loop_make_return_clause(form);
	cl_loop_step_scanner(yyscanner);
};

initially_clause:
INITIALLY
{
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("INITIALLY clause found\n");
	*context = cl_loop_make_initially_clause(form);
	cl_loop_step_scanner(yyscanner);
};

finally_clause:
FINALLY
{
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("FINALLY clause found\n");
	*context = cl_loop_make_finally_clause(form);
	cl_loop_step_scanner(yyscanner);
};

with_clause:
with equals |
with equals with_and_clause;

with_and_clause:
and equals |
and equals with_and_clause;

for_clauses:
for_clause |
for_clause for_and_clause;

for_and_clause:
and_clause |
and_clause for_and_clause;

for_clause:
for_arith_clause {
	dllist_t pro = XDLLIST(lsen->prologue);
	dllist_t iter = XDLLIST(lsen->iteration);
	Lisp_Object for_clause = *context;
	cl_loop_for_clause_t *fc = get_dynacat(for_clause);

	EMOD_CL_DEBUG_LOOP("accepting FOR clause, arithmetic form\n");
	fc->for_subclause = FOR_ARITHMETIC_CLAUSE;
	dllist_append(iter, (void*)for_clause);
	dllist_append(pro, (void*)for_clause);
} | 
for_in_clause {
	dllist_t pro = XDLLIST(lsen->prologue);
	dllist_t iter = XDLLIST(lsen->iteration);
	Lisp_Object for_clause = *context;
	cl_loop_for_clause_t *fc = get_dynacat(for_clause);

	EMOD_CL_DEBUG_LOOP("accepting FOR clause, in-list form\n");
	fc->for_subclause = FOR_IN_SUBLIST_CLAUSE;
	dllist_append(iter, (void*)for_clause);
	dllist_append(pro, (void*)for_clause);
} |
for_on_clause {
	dllist_t pro = XDLLIST(lsen->prologue);
	dllist_t iter = XDLLIST(lsen->iteration);
	Lisp_Object for_clause = *context;
	cl_loop_for_clause_t *fc = get_dynacat(for_clause);

	EMOD_CL_DEBUG_LOOP("accepting FOR clause, on-list form\n");
	fc->for_subclause = FOR_ON_SUBLIST_CLAUSE;
	dllist_append(iter, (void*)for_clause);
	dllist_append(pro, (void*)for_clause);
} |
for_across_clause {
	dllist_t pro = XDLLIST(lsen->prologue);
	dllist_t iter = XDLLIST(lsen->iteration);
	Lisp_Object for_clause = *context;
	cl_loop_for_clause_t *fc = get_dynacat(for_clause);

	EMOD_CL_DEBUG_LOOP("accepting FOR clause, across-array form\n");
	fc->for_subclause = FOR_ACROSS_ARRAY_CLAUSE;
	dllist_append(iter, (void*)for_clause);
	dllist_append(pro, (void*)for_clause);
} |
for_equals_then_clause {
	dllist_t pro = XDLLIST(lsen->prologue);
	dllist_t iter = XDLLIST(lsen->iteration);
	Lisp_Object for_clause = *context;
	cl_loop_for_clause_t *fc = get_dynacat(for_clause);

	EMOD_CL_DEBUG_LOOP("accepting FOR clause, =-then form\n");
	fc->for_subclause = FOR_EQUALS_THEN_CLAUSE;
	dllist_append(pro, (void*)for_clause);
	dllist_append(iter, (void*)for_clause);
} | 
for_hash_clause {
	dllist_t pro = XDLLIST(lsen->prologue);
	dllist_t iter = XDLLIST(lsen->iteration);
	Lisp_Object for_clause = *context;
	cl_loop_for_clause_t *fc = get_dynacat(for_clause);

	EMOD_CL_DEBUG_LOOP("accepting FOR clause, of-hash-table form\n");
	fc->for_subclause = FOR_OF_HASHTABLE_CLAUSE;
	dllist_append(pro, (void*)for_clause);
	dllist_append(iter, (void*)for_clause);
};

and_clause:
and_arith_clause {
	cl_loop_for_clause_t *fc = get_dynacat(*context);
	EMOD_CL_DEBUG_LOOP("accepting FOR+AND clause, arithmetic form\n");
	fc->for_subclause = FOR_ARITHMETIC_CLAUSE;
} |
and_in_clause {
	cl_loop_for_clause_t *fc = get_dynacat(*context);
	EMOD_CL_DEBUG_LOOP("accepting FOR+AND clause, in-list form\n");
	fc->for_subclause = FOR_IN_SUBLIST_CLAUSE;
} |
and_on_clause {
	cl_loop_for_clause_t *fc = get_dynacat(*context);
	EMOD_CL_DEBUG_LOOP("accepting FOR+AND clause, on-list form\n");
	fc->for_subclause = FOR_ON_SUBLIST_CLAUSE;
} |
and_across_clause {
	cl_loop_for_clause_t *fc = get_dynacat(*context);
	EMOD_CL_DEBUG_LOOP("accepting FOR+AND clause, across-array form\n");
	fc->for_subclause = FOR_ACROSS_ARRAY_CLAUSE;
} |
and_equals_then_clause {
	cl_loop_for_clause_t *fc = get_dynacat(*context);
	EMOD_CL_DEBUG_LOOP("accepting FOR+AND clause, =-then form\n");
	fc->for_subclause = FOR_EQUALS_THEN_CLAUSE;
} |
and_hash_clause {
	cl_loop_for_clause_t *fc = get_dynacat(*context);
	EMOD_CL_DEBUG_LOOP("accepting FOR+AND clause, of-hash-table form\n");
	fc->for_subclause = FOR_OF_HASHTABLE_CLAUSE;
};

for_in_clause:
for in;

and_in_clause:
and in;

for_on_clause:
for on;

and_on_clause:
and on;

for_across_clause:
for across;

and_across_clause:
and across;

for_equals_then_clause:
for equals then;

and_equals_then_clause:
and equals then;

for_hash_clause:
for being_each_hash_key_in |
for being_each_hash_value_in |
for being_each_hash_key_in using |
for being_each_hash_value_in using;

and_hash_clause:
and being_each_hash_key_in |
and being_each_hash_value_in |
and being_each_hash_key_in using |
and being_each_hash_value_in using;

for_arith_clause:
for fromtoby |
for frombelowby |
for fromaboveby |
for fromto |
for fromby |
for frombelow |
for fromabove |
for toby |
for belowby |
for aboveby |
for from |
for to |
for by |
for below |
for above;

and_arith_clause:
and fromtoby |
and frombelowby |
and fromaboveby |
and fromto |
and fromby |
and frombelow |
and fromabove |
and toby |
and belowby |
and aboveby |
and from |
and to |
and by |
and below |
and above;


for:
FOR
{
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("FOR clause found\n");
	*context = cl_loop_make_for_clause(form);
	cl_loop_step_scanner(yyscanner);
};

fromtoby:
from to by | from by to | to from by | to by from | by from to | by to from;

frombelowby:
from below by | from by below | below from by | below by from |
by from below | by below from;

fromaboveby:
from above by | from by above | above from by | above by from |
by from above | by above from;

fromto:
from to | to from;

fromby:
from by | by from;

frombelow:
from below | below from;

fromabove:
from above | above from;

toby:
to by | by to;

belowby:
below by | by below;

aboveby:
above by | by above;

from:
FROM
{
	Lisp_Object for_clause = *context;
	cl_loop_for_clause_t *fc = get_dynacat(for_clause);
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("FROM found\n");
	fc->from = form;
	if (EQ(*token, Qdownfrom)) {
		EMOD_CL_DEBUG_LOOP("FROM found is even a DOWNFROM\n");
		fc->torel = ASE_BINARY_REL_GREATERP;
		fc->byop = ASE_BINARY_OP_DIFF;
	}
	cl_loop_step_scanner(yyscanner);
};

to:
TO
{
	Lisp_Object for_clause = *context;
	cl_loop_for_clause_t *fc = get_dynacat(for_clause);
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("TO found\n");
	fc->to = form;
	if (EQ(*token, Qdownto)) {
		EMOD_CL_DEBUG_LOOP("TO found is even a DOWNTO\n");
		fc->torel = ASE_BINARY_REL_GREATERP;
		fc->byop = ASE_BINARY_OP_DIFF;
	}
	cl_loop_step_scanner(yyscanner);
};

by:
BY 
{
	Lisp_Object for_clause = *context;
	cl_loop_for_clause_t *fc = get_dynacat(for_clause);
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("BY found\n");
	fc->by = form;
	cl_loop_step_scanner(yyscanner);
};

below:
BELOW 
{
	Lisp_Object for_clause = *context;
	cl_loop_for_clause_t *fc = get_dynacat(for_clause);
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("BELOW found\n");
	fc->torel = ASE_BINARY_REL_LESSP;
	fc->torel_strictp = 1;
	fc->byop = ASE_BINARY_OP_SUM;
	fc->to = form;
	cl_loop_step_scanner(yyscanner);
};


above:
ABOVE 
{
	Lisp_Object for_clause = *context;
	cl_loop_for_clause_t *fc = get_dynacat(for_clause);
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("ABOVE found\n");
	fc->torel = ASE_BINARY_REL_GREATERP;
	fc->torel_strictp = 1;
	fc->byop = ASE_BINARY_OP_DIFF;
	fc->to = form;
	cl_loop_step_scanner(yyscanner);
};

in:
IN
{
	Lisp_Object for_clause = *context;
	cl_loop_for_clause_t *fc = get_dynacat(for_clause);
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("IN found\n");
	fc->inonacross = form;
	cl_loop_step_scanner(yyscanner);
};

on:
ON
{
	Lisp_Object for_clause = *context;
	cl_loop_for_clause_t *fc = get_dynacat(for_clause);
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("ON found\n");
	fc->inonacross = form;
	cl_loop_step_scanner(yyscanner);
};

across:
ACROSS
{
	Lisp_Object for_clause = *context;
	cl_loop_for_clause_t *fc = get_dynacat(for_clause);
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("ACROSS found\n");
	fc->inonacross = form;
	cl_loop_step_scanner(yyscanner);
};

with:
WITH
{
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("WITH clause found\n");
	*context = cl_loop_make_with_clause(form);
	cl_loop_step_scanner(yyscanner);
};

and:
AND
{
	Lisp_Object clause = *context;
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	if (EQ(get_dynacat_type(clause), Qcl_loop_with_clause)) {
		Lisp_Object newwf = cl_loop_make_with_clause(form);
		cl_loop_with_clause_t *newwc = get_dynacat(newwf);
		cl_loop_with_clause_t *oldwc = get_dynacat(clause);

		EMOD_CL_DEBUG_LOOP("WITH+AND clause found\n");
		newwc->next = *context;
		newwc->depth += oldwc->depth;
		*context = newwf;
	} else if (EQ(get_dynacat_type(clause), Qcl_loop_for_clause)) {
		Lisp_Object newff = cl_loop_make_for_clause(form);
		cl_loop_for_clause_t *oldfc = get_dynacat(*context);

		EMOD_CL_DEBUG_LOOP("FOR+AND clause found\n");
		oldfc->next = newff;
		oldfc->depth++;
		*context = newff;
	} else {
		Fsignal(Qinvalid_read_syntax, *yyscanner);
		return Qnil;
	}
	cl_loop_step_scanner(yyscanner);
};

equals:
EQUALS
{
	Lisp_Object clause = *context;
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	if (EQ(get_dynacat_type(clause), Qcl_loop_with_clause)) {
		cl_loop_with_clause_t *wc = get_dynacat(clause);
		EMOD_CL_DEBUG_LOOP("= found in WITH context\n");
		wc->valform = form;
	} else if (EQ(get_dynacat_type(clause), Qcl_loop_for_clause)) {
		cl_loop_for_clause_t *fc = get_dynacat(clause);
		EMOD_CL_DEBUG_LOOP("= found in FOR context\n");
		fc->equals = form;
	} else {
		Fsignal(Qinvalid_read_syntax, *yyscanner);
		return Qnil;
	}
	cl_loop_step_scanner(yyscanner);
};

then:
THEN
{
	Lisp_Object for_clause = *context;
	cl_loop_for_clause_t *fc = get_dynacat(for_clause);
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("THEN found\n");
	fc->then = form;
	cl_loop_step_scanner(yyscanner);
};

being_each_hash_key_in:
BEING EACH HASH_KEY IN
{
	Lisp_Object for_clause = *context;
	cl_loop_for_clause_t *fc = get_dynacat(for_clause);
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("BEING EACH HASH_KEY found\n");
	fc->inonacross = form;
	fc->hash_keyvar = fc->form1;
	cl_loop_step_scanner(yyscanner);
};

being_each_hash_value_in:
BEING EACH HASH_VALUE IN
{
	Lisp_Object for_clause = *context;
	cl_loop_for_clause_t *fc = get_dynacat(for_clause);
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("BEING EACH HASH_VALUE found\n");
	fc->inonacross = form;
	fc->hash_valvar = fc->form1;
	cl_loop_step_scanner(yyscanner);
};

using:
USING
{
	Lisp_Object for_clause = *context;
	cl_loop_for_clause_t *fc = get_dynacat(for_clause);
	Lisp_Object form = cl_loop_frob_scanner(yyscanner);

	EMOD_CL_DEBUG_LOOP("USING found\n");
	if (CONSP(form) && CONSP(XCDR(form)) &&
	    EQ(XCAR(form), Qhash_key)) {
		EMOD_CL_DEBUG_LOOP("USING specifies HASH_KEY symbol\n");
		fc->hash_keyvar = XCAR(XCDR(form));
	} else if (CONSP(form) && CONSP(XCDR(form)) &&
		   EQ(XCAR(form), Qhash_value)) {
		EMOD_CL_DEBUG_LOOP("USING specifies HASH_VALUE symbol\n");
		fc->hash_valvar = XCAR(XCDR(form));
	}
	cl_loop_step_scanner(yyscanner);
};

%%

#undef yyscan_t
#undef yylval
#undef yylloc

