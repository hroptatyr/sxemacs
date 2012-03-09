/*
  ase-heap.h -- Heaps
  Copyright (C) 2007 Sebastian Freundt

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

#ifndef INCLUDED_ase_heap_h_
#define INCLUDED_ase_heap_h_ 1

#include "ase.h"

typedef enum ase_heap_kind_e ase_heap_kind_t;
typedef struct ase_heap_options_s *ase_heap_options_t;
typedef struct ase_dheap_s *ase_dheap_t;
typedef struct ase_wheap_s *ase_wheap_t;
typedef struct ase_yheap_cell_s *ase_yheap_cell_t;
typedef struct ase_yheap_s *ase_yheap_t;

typedef struct ase_heap_ops_s *ase_heap_ops_t;
typedef void*(*ase_heap_constr_f)(ase_heap_options_t);
typedef Lisp_Object(*ase_heap_wrap_f)(void*);
typedef void(*ase_heap_add_f)(void*, Lisp_Object, Lisp_Object);
typedef Lisp_Object(*ase_heap_pop_f)(void*);

extern Lisp_Object Qase_heap, Qase_heapp;
extern Lisp_Object Qase_yheap, Qase_yheapp;
extern Lisp_Object Qase_dheap, Qase_dheapp;
extern Lisp_Object Qase_wheap, Qase_wheapp;
extern Lisp_Object Qase_heap_default_kind;

extern void LTX_PUBINIT(ase_heap)(void);
extern void LTX_PUBREINIT(ase_heap)(void);
extern void LTX_PUBDEINIT(ase_heap)(void);


enum ase_heap_kind_e {
	ASE_HEAP_DYNAMIC,
	ASE_HEAP_DENSE,
	ASE_HEAP_WEAK,
	NUMBER_OF_ASE_HEAP_KINDS
};

struct ase_heap_ops_s {
	ase_heap_constr_f constrf;
	ase_heap_wrap_f wrapf;
	ase_heap_add_f addf;
	ase_heap_pop_f popf;
};

struct ase_heap_options_s {
	ase_binary_relation_t po; /* partial order to look for
				   * in ent_binary_reltable */
	ase_binary_relation_f pof; /* partial order function */
	ase_heap_kind_t kind;	/* representation of the heap */
	size_t min_size;	/* minimum size of the dense array */

	int coloured;		/* whether we store coloured items */

	/* just a ref counter for those nifty recycled items */
	sxe_refcounter_t refcnt;
};

/* that way we can use a global macro */
#define	ASE_HEAP_MUTEX_SORCERY		sxe_mutex_t mtx
#define	ASE_HEAP_OPTIONS_SORCERY	ase_heap_options_t options

struct ase_dheap_s {
	Lisp_Object *cells;
	Lisp_Object *colours;

	/* status and mutex for the entire heap */
	ASE_HEAP_MUTEX_SORCERY;
	int heapp;		/* if heap obeys the heap property */
	size_t alloc;		/* allocated size of the heap */
	size_t size;		/* size of the heap (used cells) */

	/* options to control the heap's behaviour */
	ASE_HEAP_OPTIONS_SORCERY;
};

struct ase_wheap_s {
	Lisp_Object *cells;
	Lisp_Object *colours;
	int *rbits;

	/* status and mutex for the entire heap */
	ASE_HEAP_MUTEX_SORCERY;
	int heapp;		/* if heap obeys the heap property */
	size_t alloc;		/* allocated size of the heap */
	size_t size;		/* size of the heap (used cells) */

	/* options to control the heap's behaviour */
	ASE_HEAP_OPTIONS_SORCERY;
};

struct ase_yheap_s {
	ase_yheap_cell_t root;
	ase_yheap_cell_t first_free;
	ase_yheap_cell_t last_free;

	/* status and mutex for the entire heap */
	ASE_HEAP_MUTEX_SORCERY;
	int heapp;		/* if heap obeys the heap property */
	size_t size;
	size_t alloc;

	/* options to control the heap's behaviour */
	ASE_HEAP_OPTIONS_SORCERY;
};


/* general heap thing (independent from underlying representation) */
#define ASE_HEAPP(_i)						\
	(DYNACATP(_i) &&					\
	 (EQ(XDYNACAT(_i)->type, Qase_wheap) ||			\
	  EQ(XDYNACAT(_i)->type, Qase_dheap) ||			\
	  EQ(XDYNACAT(_i)->type, Qase_yheap)))
#define CHECK_ASE_HEAP(x)						\
	do {								\
		if (!ASE_HEAPP(x))					\
			dead_wrong_type_argument(Qase_heapp, x);	\
	} while (0)
#define CONCHECK_ASE_HEAP(x)						\
	do {								\
		if (!ASE_HEAPP(x))					\
			x = wrong_type_argument(Qase_heapp, x);		\
	} while (0)

/* options and mutexes are generic */
/* options */
#define ase_heap_options(_x)		((_x)->options)
#define ase_heap_options_po(_x)		((_x)->po)
#define ase_heap_options_pof(_x)	((_x)->pof)
#define ase_heap_options_kind(_x)	((_x)->kind)
#define ase_heap_options_min_size(_x)	((_x)->min_size)
#define ase_heap_options_coloured(_x)	((_x)->coloured)
#define ase_heap_options_refcnt(_x)	((_x)->refcnt)
#define ase_heap_opts_po(_x)		(ase_heap_options(_x)->po)
#define ase_heap_opts_pof(_x)		(ase_heap_options(_x)->pof)
#define ase_heap_opts_kind(_x)		(ase_heap_options(_x)->kind)
#define ase_heap_opts_min_size(_x)	(ase_heap_options(_x)->min_size)
#define ase_heap_opts_coloured(_x)	(ase_heap_options(_x)->coloured)
#define ase_heap_opts_refcnt(_x)	(ase_heap_options(_x)->refcnt)
/* mutex */
#define ase_heap_mtx(_x)		((_x)->mtx)
#define ase_heap_init_mutex(_x)		(SXE_MUTEX_INIT(&(ase_heap_mtx(_x))))
#define ase_heap_fini_mutex(_x)		(SXE_MUTEX_FINI(&(ase_heap_mtx(_x))))
#define ase_heap_lock(_x)		(SXE_MUTEX_LOCK(&(ase_heap_mtx(_x))))
#define ase_heap_unlock(_x)		(SXE_MUTEX_UNLOCK(&(ase_heap_mtx(_x))))

/* yheaps */
#define ASE_YHEAPP(_i)						\
	(DYNACATP(_i) && EQ(XDYNACAT(_i)->type, Qase_yheap))
#define CHECK_ASE_YHEAP(x)						\
	do {								\
		if (!ASE_YHEAPP(x))					\
			dead_wrong_type_argument(Qase_yheapp, x);	\
	} while (0)
#define CONCHECK_ASE_YHEAP(x)						\
	do {								\
		if (!ASE_YHEAPP(x))					\
			x = wrong_type_argument(Qase_yheapp, x);	\
	} while (0)
#define XSETASE_YHEAP(_res, _int)	(_res) = _ase_wrap_heap((_int))
#define XASE_YHEAP(_x)			((ase_yheap_t)get_dynacat(_x))

#define ase_yheap_root(_x)		((_x)->root)
#define ase_yheap_first_free(_x)	((_x)->first_free)
#define ase_yheap_last_free(_x)		((_x)->last_free)
/* status */
#define ase_yheap_heapp(_x)		((_x)->heapp)
#define ase_yheap_size(_x)		((_x)->size)
#define ase_yheap_alloc(_x)		((_x)->alloc)

#define ase_yheap_cell_data(_x)		((_x)->data)
#define ase_yheap_cell_colour(_x)	((_x)->colour)
#define ase_yheap_cell_left(_x)		((_x)->left)
#define ase_yheap_cell_right(_x)		((_x)->right)
#define ase_yheap_cell_mother(_x)	((_x)->mother)
#define ase_yheap_cell_father(_x)	((_x)->father)
#define ase_yheap_cell_next(_x)		((_x)->next)
#define ase_yheap_cell_prev(_x)		((_x)->prev)

extern void ase_yheapify(ase_yheap_t);
extern Lisp_Object ase_yheap_to_listX(ase_yheap_t);
extern Lisp_Object ase_yheap_to_list(ase_yheap_t);
extern Lisp_Object ase_yheap_to_vectorX(ase_yheap_t);
extern Lisp_Object ase_yheap_to_vector(ase_yheap_t);
extern Lisp_Object ase_yheap_to_dllistX(ase_yheap_t);
extern Lisp_Object ase_yheap_to_dllist(ase_yheap_t);

/* dense heaps */
#define ASE_DHEAPP(_i)						\
	(DYNACATP(_i) && EQ(XDYNACAT(_i)->type, Qase_dheap))
#define CHECK_ASE_DHEAP(x)						\
	do {								\
		if (!ASE_DHEAPP(x))					\
			dead_wrong_type_argument(Qase_dheapp, x);	\
	} while (0)
#define CONCHECK_ASE_DHEAP(x)						\
	do {								\
		if (!ASE_DHEAPP(x))					\
			x = wrong_type_argument(Qase_dheapp, x);		\
	} while (0)
#define XSETASE_DHEAP(_res, _int)	(_res) = _ase_wrap_dheap((_int))
#define XASE_DHEAP(_x)			((ase_dheap_t)get_dynacat(_x))

#define ase_dheap_cells(_x)		((_x)->cells)
#define ase_dheap_colours(_x)		((_x)->colours)
/* status */
#define ase_dheap_heapp(_x)		((_x)->heapp)
#define ase_dheap_size(_x)		((_x)->size)
#define ase_dheap_alloc(_x)		((_x)->alloc)

extern void ase_dheap_sort(ase_dheap_t);
extern Lisp_Object ase_dheap_to_listX(ase_dheap_t);
extern Lisp_Object ase_dheap_to_list(ase_dheap_t);
extern Lisp_Object ase_dheap_to_vectorX(ase_dheap_t);
extern Lisp_Object ase_dheap_to_vector(ase_dheap_t);
extern Lisp_Object ase_dheap_to_dllistX(ase_dheap_t);
extern Lisp_Object ase_dheap_to_dllist(ase_dheap_t);

/* weak heaps */
#define ASE_WHEAPP(_i)						\
	(DYNACATP(_i) && EQ(XDYNACAT(_i)->type, Qase_wheap))
#define CHECK_ASE_WHEAP(x)						\
	do {								\
		if (!ASE_WHEAPP(x))					\
			dead_wrong_type_argument(Qase_wheapp, x);	\
	} while (0)
#define CONCHECK_ASE_WHEAP(x)						\
	do {								\
		if (!ASE_WHEAPP(x))					\
			x = wrong_type_argument(Qase_wheapp, x);		\
	} while (0)
#define XSETASE_WHEAP(_res, _int)	(_res) = _ase_wrap_wheap((_int))
#define XASE_WHEAP(_x)			((ase_wheap_t)get_dynacat(_x))

#define ase_wheap_cells(_x)		((_x)->cells)
#define ase_wheap_colours(_x)		((_x)->colours)
#define ase_wheap_rbits(_x)		((_x)->rbits)
/* status */
#define ase_wheap_heapp(_x)		((_x)->heapp)
#define ase_wheap_size(_x)		((_x)->size)
#define ase_wheap_alloc(_x)		((_x)->alloc)

extern void _ase_wheapify(ase_wheap_t);
extern void ase_wheap_sort(ase_wheap_t);
extern Lisp_Object ase_wheap_to_listX(ase_wheap_t);
extern Lisp_Object ase_wheap_to_list(ase_wheap_t);
extern Lisp_Object ase_wheap_to_vectorX(ase_wheap_t);
extern Lisp_Object ase_wheap_to_vector(ase_wheap_t);
extern Lisp_Object ase_wheap_to_dllistX(ase_wheap_t);
extern Lisp_Object ase_wheap_to_dllist(ase_wheap_t);


/* dyna heaps */
extern Lisp_Object ase_make_yheap(ase_heap_options_t opts);
extern void ase_add_yheap(ase_yheap_t h, Lisp_Object o, Lisp_Object colour);
extern Lisp_Object ase_pop_yheap(ase_yheap_t h);
extern Lisp_Object ase_yheap_top(ase_yheap_t h);
extern Lisp_Object ase_yheap_top_rank(ase_yheap_t h);

/* dense heaps */
extern Lisp_Object ase_make_dheap(ase_heap_options_t opts);
extern void ase_add_dheap(ase_dheap_t h, Lisp_Object o, Lisp_Object colour);
extern Lisp_Object ase_pop_dheap(ase_dheap_t h);
extern Lisp_Object ase_dheap_top(ase_dheap_t h);
extern Lisp_Object ase_dheap_top_rank(ase_dheap_t h);

/* weak heaps */
extern Lisp_Object ase_make_wheap(ase_heap_options_t opts);
extern void ase_add_wheap(ase_wheap_t h, Lisp_Object o, Lisp_Object colour);
extern Lisp_Object ase_pop_wheap(ase_wheap_t h);
extern Lisp_Object ase_wheap_top(ase_wheap_t h);
extern Lisp_Object ase_wheap_top_rank(ase_wheap_t h);

/* misc */
EXFUN(Fase_heap, MANY);
EXFUN(Fase_add_heap, 3);
EXFUN(Fase_pop_heap, 1);

extern struct ase_heap_ops_s ase_heap_ops[NUMBER_OF_ASE_HEAP_KINDS];

#endif	/* INCLUDED_ase_heap_h_ */
