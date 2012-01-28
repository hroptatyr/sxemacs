/* The lisp stack.
   Copyright (C) 1985, 1986, 1987, 1992, 1993 Free Software Foundation, Inc.

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


/* Synched up with: FSF 19.30.  Contained redundantly in various C files
   in FSFmacs. */

/* Authorship:

   FSF: Original version; a long time ago.
   XEmacs: split out of some C files. (For some obscure reason, a header
	   file couldn't be used in FSF Emacs, but XEmacs doesn't have
	   that problem.)
   Mly (probably) or JWZ: Some changes.
 */

#ifndef INCLUDED_backtrace_h_
#define INCLUDED_backtrace_h_

#include <setjmp.h>

/* These definitions are used in eval.c and alloc.c */

struct backtrace {
	struct backtrace *next;
	Lisp_Object *function;
	Lisp_Object *args;	/* Points to vector of args. */
	int nargs;		/* Length of vector.
				   If nargs is UNEVALLED, args points to
				   slot holding list of unevalled args */
	int pdlcount;		/* specpdl_depth () when invoked */
	char evalargs;
	/* Nonzero means call value of debugger when done with this operation. */
	char debug_on_exit;
};

/* This structure helps implement the `catch' and `throw' control
   structure.  A struct catchtag contains all the information needed
   to restore the state of the interpreter after a non-local jump.

   Handlers for error conditions (represented by `struct handler'
   structures) just point to a catch tag to do the cleanup required
   for their jumps.

   catchtag structures are chained together in the C calling stack;
   the `next' member points to the next outer catchtag.

   A call like (throw TAG VAL) searches for a catchtag whose `tag'
   member is TAG, and then unbinds to it.  The `val' member is used to
   hold VAL while the stack is unwound; `val' is returned as the value
   of the catch form.

   All the other members are concerned with restoring the interpreter
   state.  */

struct catchtag {
	Lisp_Object tag;
	Lisp_Object val;
	struct catchtag *next;
	struct gcpro *gcpro;
	JMP_BUF jmp;
	struct backtrace *backlist;
#if 0				/* FSFmacs */
	/* #### */
	struct handler *handlerlist;
#endif
	int lisp_eval_depth;
	int pdlcount;
#if 0				/* FSFmacs */
	/* This is the equivalent of async_timer_suppress_count.
	   We probably don't have to bother with this. */
	int poll_suppress_count;
#endif
};

/* Dynamic-binding-o-rama */

/* Structure for recording Lisp call stack for backtrace purposes.  */

/* The special binding stack holds the outer values of variables while
   they are bound by a function application or a let form, stores the
   code to be executed for Lisp unwind-protect forms, and stores the C
   functions to be called for record_unwind_protect.

   If func is non-zero, undoing this binding applies func to old_value;
      This implements record_unwind_protect.
   If func is zero and symbol is nil, undoing this binding evaluates
      the list of forms in old_value; this implements Lisp's unwind-protect
      form.
   Otherwise, undoing this binding stores old_value as symbol's value; this
      undoes the bindings made by a let form or function call.  */

struct specbinding {
	Lisp_Object symbol;
	Lisp_Object old_value;
	Lisp_Object(*func)(Lisp_Object);	/* for unwind-protect */
};

#if 0				/* FSFmacs */
/* #### */
/* Everything needed to describe an active condition case.  */
struct handler {
	/* The handler clauses and variable from the condition-case form.  */
	Lisp_Object handler;
	Lisp_Object var;
	/* Fsignal stores here the condition-case clause that applies,
	   and Fcondition_case thus knows which clause to run.  */
	Lisp_Object chosen_clause;

	/* Used to effect the longjmp() out to the handler.  */
	struct catchtag *tag;

	/* The next enclosing handler.  */
	struct handler *next;
};

extern struct handler *handlerlist;

#endif

/* These are extern because GC needs to mark them */
extern struct specbinding *specpdl;
extern struct specbinding *specpdl_ptr;
extern struct catchtag *catchlist;
extern struct backtrace *backtrace_list;

/* Most callers should simply use specbind() and unbind_to(), but if
   speed is REALLY IMPORTANT, you can use the faster macros below */
void specbind_magic(Lisp_Object, Lisp_Object);
void grow_specpdl(EMACS_INT reserved);
void unbind_to_hairy(int);
extern int specpdl_size;

/* Inline version of specbind().
   Use this instead of specbind() if speed is sufficiently important
   to save the overhead of even a single function call. */
#define SPECBIND(symbol_object, value_object) do {			\
  Lisp_Object SB_symbol = (symbol_object);				\
  Lisp_Object SB_newval = (value_object);				\
  Lisp_Object SB_oldval;						\
  Lisp_Symbol *SB_sym;							\
									\
  SPECPDL_RESERVE (1);							\
									\
  CHECK_SYMBOL (SB_symbol);						\
  SB_sym = XSYMBOL (SB_symbol);						\
  SB_oldval = SB_sym->value;						\
									\
  if (!SYMBOL_VALUE_MAGIC_P (SB_oldval) || UNBOUNDP (SB_oldval))	\
    {									\
      /* #### the following test will go away when we have a constant	\
	 symbol magic object */						\
      if (EQ (SB_symbol, Qnil) ||					\
	  EQ (SB_symbol, Qt)   ||					\
	  SYMBOL_IS_KEYWORD (SB_symbol))				\
	reject_constant_symbols (SB_symbol, SB_newval, 0,		\
				 UNBOUNDP (SB_newval) ?			\
				 Qmakunbound : Qset);			\
									\
      specpdl_ptr->symbol    = SB_symbol;				\
      specpdl_ptr->old_value = SB_oldval;				\
      specpdl_ptr->func      = 0;					\
      specpdl_ptr++;							\
      specpdl_depth_counter++;						\
									\
      SB_sym->value = (SB_newval);					\
    }									\
  else									\
    specbind_magic (SB_symbol, SB_newval);				\
} while (0)

/* An even faster, but less safe inline version of specbind().
   Caller guarantees that:
   - SYMBOL is a non-constant symbol (i.e. not Qnil, Qt, or keyword).
   - specpdl_depth_counter >= specpdl_size.
   Else we crash.  */
#define SPECBIND_FAST_UNSAFE(symbol_object, value_object) do {		\
  Lisp_Object SFU_symbol = (symbol_object);				\
  Lisp_Object SFU_newval = (value_object);				\
  Lisp_Symbol *SFU_sym   = XSYMBOL (SFU_symbol);			\
  Lisp_Object SFU_oldval = SFU_sym->value;				\
  if (!SYMBOL_VALUE_MAGIC_P (SFU_oldval) || UNBOUNDP (SFU_oldval))	\
    {									\
      specpdl_ptr->symbol    = SFU_symbol;				\
      specpdl_ptr->old_value = SFU_oldval;				\
      specpdl_ptr->func      = 0;					\
      specpdl_ptr++;							\
      specpdl_depth_counter++;						\
									\
      SFU_sym->value = (SFU_newval);					\
    }									\
  else									\
    specbind_magic (SFU_symbol, SFU_newval);				\
} while (0)

/* Request enough room for SIZE future entries on special binding stack */
/* SR_size will typically be compared to an unsigned short */
#define SPECPDL_RESERVE(size) do {			\
  EMACS_INT SR_size = (size);				\
  if (specpdl_depth() + SR_size >= specpdl_size)	\
    grow_specpdl (SR_size);				\
} while (0)

/* Inline version of unbind_to().
   Use this instead of unbind_to() if speed is sufficiently important
   to save the overhead of even a single function call.

   Most of the time, unbind_to() is called only on ordinary
   variables, so optimize for that.  */
#define UNBIND_TO_GCPRO(count, value) do {		\
  int UNBIND_TO_count = (count);			\
  while (specpdl_depth_counter != UNBIND_TO_count)	\
    {							\
      Lisp_Symbol *sym;					\
      --specpdl_ptr;					\
      --specpdl_depth_counter;				\
							\
      if (specpdl_ptr->func != 0 ||			\
	  ((sym = XSYMBOL (specpdl_ptr->symbol)),	\
	   SYMBOL_VALUE_MAGIC_P (sym->value)))		\
	{						\
	  struct gcpro gcpro1;				\
	  GCPRO1 (value);				\
	  unbind_to_hairy (UNBIND_TO_count);		\
	  UNGCPRO;					\
	  break;					\
	}						\
							\
      sym->value = specpdl_ptr->old_value;		\
    }							\
} while (0)

/* A slightly faster inline version of unbind_to,
   that doesn't offer GCPROing services. */
#define UNBIND_TO(count) do {				\
  int UNBIND_TO_count = (count);			\
  while (specpdl_depth_counter != UNBIND_TO_count)	\
    {							\
      Lisp_Symbol *sym;					\
      --specpdl_ptr;					\
      --specpdl_depth_counter;				\
							\
      if (specpdl_ptr->func != 0 ||			\
	  ((sym = XSYMBOL (specpdl_ptr->symbol)),	\
	   SYMBOL_VALUE_MAGIC_P (sym->value)))		\
	{						\
	  unbind_to_hairy (UNBIND_TO_count);		\
	  break;					\
	}						\
							\
      sym->value = specpdl_ptr->old_value;		\
    }							\
} while (0)

#ifdef ERROR_CHECK_TYPECHECK
#define CHECK_SPECBIND_VARIABLE assert (specpdl_ptr->func == 0)
#else
#define CHECK_SPECBIND_VARIABLE DO_NOTHING
#endif

#if 0
/* Unused.  It's too hard to guarantee that the current bindings
   contain only variables.  */
/* Another inline version of unbind_to().  VALUE is GC-protected.
   Caller guarantees that:
   - all of the elements on the binding stack are variable bindings.
   Else we crash.  */
#define UNBIND_TO_GCPRO_VARIABLES_ONLY(count, value) do {	\
  int UNBIND_TO_count = (count);				\
  while (specpdl_depth_counter != UNBIND_TO_count)		\
    {								\
      Lisp_Symbol *sym;						\
      --specpdl_ptr;						\
      --specpdl_depth_counter;					\
								\
      CHECK_SPECBIND_VARIABLE;					\
      sym = XSYMBOL (specpdl_ptr->symbol);			\
      if (!SYMBOL_VALUE_MAGIC_P (sym->value))			\
	sym->value = specpdl_ptr->old_value;			\
      else							\
	{							\
	  struct gcpro gcpro1;					\
	  GCPRO1 (value);					\
	  unbind_to_hairy (UNBIND_TO_count);			\
	  UNGCPRO;						\
	  break;						\
	}							\
    }								\
} while (0)
#endif				/* unused */

/* A faster, but less safe inline version of Fset().
   Caller guarantees that:
   - SYMBOL is a non-constant symbol (i.e. not Qnil, Qt, or keyword).
   Else we crash.  */
#define FSET_FAST_UNSAFE(sym, newval) do {				\
  Lisp_Object FFU_sym = (sym);						\
  Lisp_Object FFU_newval = (newval);					\
  Lisp_Symbol *FFU_symbol = XSYMBOL (FFU_sym);				\
  Lisp_Object FFU_oldval = FFU_symbol->value;				\
  if (!SYMBOL_VALUE_MAGIC_P (FFU_oldval) || UNBOUNDP (FFU_oldval))	\
    FFU_symbol->value = FFU_newval;					\
  else									\
    Fset (FFU_sym, FFU_newval);						\
} while (0)

#endif				/* INCLUDED_backtrace_h_ */
