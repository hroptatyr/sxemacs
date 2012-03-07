/* Evaluator for SXEmacs Lisp interpreter.
   Copyright (C) 1985-1987, 1992-1994 Free Software Foundation, Inc.
   Copyright (C) 1995 Sun Microsystems, Inc.
   Copyright (C) 2000 Ben Wing.

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


/* Synched up with: FSF 19.30 (except for Fsignal), Mule 2.0. */

#include <config.h>
#include "lisp.h"

#include "commands.h"
#include "backtrace.h"
#include "bytecode.h"
#include "buffer.h"
#include "ui/console.h"
#include "opaque.h"

#ifdef ERROR_CHECK_GC
int always_gc;			/* Debugging hack */
#else
#define always_gc 0
#endif

struct backtrace *backtrace_list;

/* Note: you must always fill in all of the fields in a backtrace structure
   before pushing them on the backtrace_list.  The profiling code depends
   on this. */

#define PUSH_BACKTRACE(bt) do {		\
  (bt).next = backtrace_list;		\
  backtrace_list = &(bt);		\
} while (0)

#define POP_BACKTRACE(bt) do {		\
  backtrace_list = (bt).next;		\
} while (0)


/* This is the list of current catches (and also condition-cases).
   This is a stack: the most recent catch is at the head of the
   list.  Catches are created by declaring a 'struct catchtag'
   locally, filling the .TAG field in with the tag, and doing
   a setjmp() on .JMP.  Fthrow() will store the value passed
   to it in .VAL and longjmp() back to .JMP, back to the function
   that established the catch.  This will always be either
   internal_catch() (catches established internally or through
   `catch') or condition_case_1 (condition-cases established
   internally or through `condition-case').

   The catchtag also records the current position in the
   call stack (stored in BACKTRACE_LIST), the current position
   in the specpdl stack (used for variable bindings and
   unwind-protects), the value of LISP_EVAL_DEPTH, and the
   current position in the GCPRO stack.  All of these are
   restored by Fthrow().
   */

struct catchtag *catchlist;

Lisp_Object Qautoload, Qmacro, Qexit;
Lisp_Object Qinteractive, Qcommandp, Qdefun, Qprogn, Qvalues;
Lisp_Object Vquit_flag, Vinhibit_quit;
Lisp_Object Qand_rest, Qand_optional;
Lisp_Object Qdebug_on_error, Qstack_trace_on_error;
Lisp_Object Qdebug_on_signal, Qstack_trace_on_signal;
Lisp_Object Qdebugger;
Lisp_Object Qinhibit_quit;
Lisp_Object Qrun_hooks;
Lisp_Object Qsetq;
Lisp_Object Qdisplay_warning;
Lisp_Object Vpending_warnings, Vpending_warnings_tail;
Lisp_Object Qif;

/* Records whether we want errors to occur.  This will be a boolean,
   nil (errors OK) or t (no errors).  If t, an error will cause a
   throw to Qunbound_suspended_errors_tag.

   See call_with_suspended_errors(). */
Lisp_Object Vcurrent_error_state;

/* Current warning class when warnings occur, or nil for no warnings.
   Only meaningful when Vcurrent_error_state is non-nil.
   See call_with_suspended_errors(). */
Lisp_Object Vcurrent_warning_class;

/* Special catch tag used in call_with_suspended_errors(). */
Lisp_Object Qunbound_suspended_errors_tag;

/* Non-nil means record all fset's and provide's, to be undone
   if the file being autoloaded is not fully loaded.
   They are recorded by being consed onto the front of Vautoload_queue:
   (FUN . ODEF) for a defun, (OFEATURES . nil) for a provide.  */
Lisp_Object Vautoload_queue;

/* Current number of specbindings allocated in specpdl.  */
int specpdl_size;

/* Pointer to beginning of specpdl.  */
struct specbinding *specpdl;

/* Pointer to first unused element in specpdl.  */
struct specbinding *specpdl_ptr;

/* specpdl_ptr - specpdl */
int specpdl_depth_counter;

/* Maximum size allowed for specpdl allocation */
Fixnum max_specpdl_size;

/* Depth in Lisp evaluations and function calls.  */
static int lisp_eval_depth;

/* Maximum allowed depth in Lisp evaluations and function calls.  */
Fixnum max_lisp_eval_depth;

/* Nonzero means enter debugger before next function call */
static int debug_on_next_call;

/* List of conditions (non-nil atom means all) which cause a backtrace
   if an error is handled by the command loop's error handler.  */
Lisp_Object Vstack_trace_on_error;

/* List of conditions (non-nil atom means all) which enter the debugger
   if an error is handled by the command loop's error handler.  */
Lisp_Object Vdebug_on_error;

/* List of conditions and regexps specifying error messages which
   do not enter the debugger even if Vdebug_on_error says they should.  */
Lisp_Object Vdebug_ignored_errors;

/* List of conditions (non-nil atom means all) which cause a backtrace
   if any error is signalled.  */
Lisp_Object Vstack_trace_on_signal;

/* List of conditions (non-nil atom means all) which enter the debugger
   if any error is signalled.  */
Lisp_Object Vdebug_on_signal;

/* Nonzero means enter debugger if a quit signal
   is handled by the command loop's error handler.

   From lisp, this is a boolean variable and may have the values 0 and 1.
   But, eval.c temporarily uses the second bit of this variable to indicate
   that a critical_quit is in progress.  The second bit is reset immediately
   after it is processed in signal_call_debugger().  */
int debug_on_quit;

#if 0				/* FSFmacs */
/* entering_debugger is basically equivalent */
/* The value of num_nonmacro_input_chars as of the last time we
   started to enter the debugger.  If we decide to enter the debugger
   again when this is still equal to num_nonmacro_input_chars, then we
   know that the debugger itself has an error, and we should just
   signal the error instead of entering an infinite loop of debugger
   invocations.  */
int when_entered_debugger;
#endif

/* Nonzero means we are trying to enter the debugger.
   This is to prevent recursive attempts.
   Cleared by the debugger calling Fbacktrace */
static int entering_debugger;

/* Function to call to invoke the debugger */
Lisp_Object Vdebugger;

/* Chain of condition handlers currently in effect.
   The elements of this chain are contained in the stack frames
   of Fcondition_case and internal_condition_case.
   When an error is signaled (by calling Fsignal, below),
   this chain is searched for an element that applies.

   Each element of this list is one of the following:

   A list of a handler function and possibly args to pass to
   the function.  This is a handler established with
   `call-with-condition-handler' (q.v.).

   A list whose car is Qunbound and whose cdr is Qt.
   This is a special condition-case handler established
   by C code with condition_case_1().  All errors are
   trapped; the debugger is not invoked even if
   `debug-on-error' was set.

   A list whose car is Qunbound and whose cdr is Qerror.
   This is a special condition-case handler established
   by C code with condition_case_1().  It is like Qt
   except that the debugger is invoked normally if it is
   called for.

   A list whose car is Qunbound and whose cdr is a list
   of lists (CONDITION-NAME BODY ...) exactly as in
   `condition-case'.  This is a normal `condition-case'
   handler.

   Note that in all cases *except* the first, there is a
   corresponding catch, whose TAG is the value of
   Vcondition_handlers just after the handler data just
   described is pushed onto it.  The reason is that
   `condition-case' handlers need to throw back to the
   place where the handler was installed before invoking
   it, while `call-with-condition-handler' handlers are
   invoked in the environment that `signal' was invoked
   in.
*/
static Lisp_Object Vcondition_handlers;

#define DEFEND_AGAINST_THROW_RECURSION

#ifdef DEFEND_AGAINST_THROW_RECURSION
/* Used for error catching purposes by throw_or_bomb_out */
static int throw_level;
#endif

#ifdef ERROR_CHECK_TYPECHECK
void check_error_state_sanity(void);
#endif

/************************************************************************/
/*			The subr object type				*/
/************************************************************************/

static void
print_subr(Lisp_Object obj, Lisp_Object printcharfun, int escapeflag)
{
	Lisp_Subr *subr = XSUBR(obj);
	const char *header =
	    (subr->max_args == UNEVALLED) ? "#<special-form " : "#<subr ";
	const char *name = subr_name(subr);
	const char *trailer = subr->prompt ? " (interactive)>" : ">";

	if (print_readably)
		error("printing unreadable object %s%s%s", header, name,
		      trailer);

	write_c_string(header, printcharfun);
	write_c_string(name, printcharfun);
	write_c_string(trailer, printcharfun);
}

static const struct lrecord_description subr_description[] = {
	{XD_DOC_STRING, offsetof(Lisp_Subr, doc)},
	{XD_END}
};

DEFINE_BASIC_LRECORD_IMPLEMENTATION("subr", subr,
				    0, print_subr, 0, 0, 0,
				    subr_description, Lisp_Subr);

/************************************************************************/
/*			 Entering the debugger				*/
/************************************************************************/

/* unwind-protect used by call_debugger() to restore the value of
   entering_debugger. (We cannot use specbind() because the
   variable is not Lisp-accessible.) */

static Lisp_Object restore_entering_debugger(Lisp_Object arg)
{
	entering_debugger = !NILP(arg);
	return arg;
}

/* Actually call the debugger.  ARG is a list of args that will be
   passed to the debugger function, as follows;

If due to frame exit, args are `exit' and the value being returned;
 this function's value will be returned instead of that.
If due to error, args are `error' and a list of the args to `signal'.
If due to `apply' or `funcall' entry, one arg, `lambda'.
If due to `eval' entry, one arg, t.

*/

static Lisp_Object call_debugger_259(Lisp_Object arg)
{
	return apply1(Vdebugger, arg);
}

/* Call the debugger, doing some encapsulation.  We make sure we have
   some room on the eval and specpdl stacks, and bind entering_debugger
   to 1 during this call.  This is used to trap errors that may occur
   when entering the debugger (e.g. the value of `debugger' is invalid),
   so that the debugger will not be recursively entered if debug-on-error
   is set. (Otherwise, SXEmacs would infinitely recurse, attempting to
   enter the debugger.) entering_debugger gets reset to 0 as soon
   as a backtrace is displayed, so that further errors can indeed be
   handled normally.

   We also establish a catch for 'debugger.  If the debugger function
   throws to this instead of returning a value, it means that the user
   pressed 'c' (pretend like the debugger was never entered).  The
   function then returns Qunbound. (If the user pressed 'r', for
   return a value, then the debugger function returns normally with
   this value.)

   The difference between 'c' and 'r' is as follows:

   debug-on-call:
     No difference.  The call proceeds as normal.
   debug-on-exit:
     With 'r', the specified value is returned as the function's
     return value.  With 'c', the value that would normally be
     returned is returned.
   signal:
     With 'r', the specified value is returned as the return
     value of `signal'. (This is the only time that `signal'
     can return, instead of making a non-local exit.) With `c',
     `signal' will continue looking for handlers as if the
     debugger was never entered, and will probably end up
     throwing to a handler or to top-level.
*/

static Lisp_Object call_debugger(Lisp_Object arg)
{
	int threw;
	Lisp_Object val;
	int speccount;

	if (lisp_eval_depth + 20 > max_lisp_eval_depth)
		max_lisp_eval_depth = lisp_eval_depth + 20;
	if (specpdl_size + 40 > max_specpdl_size)
		max_specpdl_size = specpdl_size + 40;
	debug_on_next_call = 0;

	speccount = specpdl_depth();
	record_unwind_protect(restore_entering_debugger,
			      (entering_debugger ? Qt : Qnil));
	entering_debugger = 1;
	val = internal_catch(Qdebugger, call_debugger_259, arg, &threw);

	return unbind_to(speccount, ((threw)
				     ? Qunbound	/* Not returning a value */
				     : val));
}

/* Called when debug-on-exit behavior is called for.  Enter the debugger
   with the appropriate args for this.  VAL is the exit value that is
   about to be returned. */

static Lisp_Object do_debug_on_exit(Lisp_Object val)
{
	/* This is falsified by call_debugger */
	Lisp_Object v = call_debugger(list2(Qexit, val));

	return !UNBOUNDP(v) ? v : val;
}

/* Called when debug-on-call behavior is called for.  Enter the debugger
   with the appropriate args for this.  VAL is either t for a call
   through `eval' or 'lambda for a call through `funcall'.

   #### The differentiation here between EVAL and FUNCALL is bogus.
   FUNCALL can be defined as

   (defmacro func (fun &rest args)
     (cons (eval fun) args))

   and should be treated as such.
 */

static void do_debug_on_call(Lisp_Object code)
{
	debug_on_next_call = 0;
	backtrace_list->debug_on_exit = 1;
	call_debugger(list1(code));
}

/* LIST is the value of one of the variables `debug-on-error',
   `debug-on-signal', `stack-trace-on-error', or `stack-trace-on-signal',
   and CONDITIONS is the list of error conditions associated with
   the error being signalled.  This returns non-nil if LIST
   matches CONDITIONS. (A nil value for LIST does not match
   CONDITIONS.  A non-list value for LIST does match CONDITIONS.
   A list matches CONDITIONS when one of the symbols in LIST is the
   same as one of the symbols in CONDITIONS.) */

static int wants_debugger(Lisp_Object list, Lisp_Object conditions)
{
	if (NILP(list))
		return 0;
	if (!CONSP(list))
		return 1;

	while (CONSP(conditions)) {
		Lisp_Object this, tail;
		this = XCAR(conditions);
		for (tail = list; CONSP(tail); tail = XCDR(tail))
			if (EQ(XCAR(tail), this))
				return 1;
		conditions = XCDR(conditions);
	}
	return 0;
}

/* Return 1 if an error with condition-symbols CONDITIONS,
   and described by SIGNAL-DATA, should skip the debugger
   according to debugger-ignore-errors.  */

static int skip_debugger(Lisp_Object conditions, Lisp_Object data)
{
	/* This function can GC */
	Lisp_Object tail;
	int first_string = 1;
	Lisp_Object error_message = Qnil;

	for (tail = Vdebug_ignored_errors; CONSP(tail); tail = XCDR(tail)) {
		if (STRINGP(XCAR(tail))) {
			if (first_string) {
				error_message = Ferror_message_string(data);
				first_string = 0;
			}
			if (fast_lisp_string_match(XCAR(tail), error_message) >=
			    0)
				return 1;
		} else {
			Lisp_Object contail;

			for (contail = conditions; CONSP(contail);
			     contail = XCDR(contail))
				if (EQ(XCAR(tail), XCAR(contail)))
					return 1;
		}
	}

	return 0;
}

/* Actually generate a backtrace on STREAM. */

static Lisp_Object backtrace_259(Lisp_Object stream)
{
	return Fbacktrace(stream, Qt);
}

/* An error was signaled.  Maybe call the debugger, if the `debug-on-error'
   etc. variables call for this.  CONDITIONS is the list of conditions
   associated with the error being signalled.  SIG is the actual error
   being signalled, and DATA is the associated data (these are exactly
   the same as the arguments to `signal').  ACTIVE_HANDLERS is the
   list of error handlers that are to be put in place while the debugger
   is called.  This is generally the remaining handlers that are
   outside of the innermost handler trapping this error.  This way,
   if the same error occurs inside of the debugger, you usually don't get
   the debugger entered recursively.

   This function returns Qunbound if it didn't call the debugger or if
   the user asked (through 'c') that SXEmacs should pretend like the
   debugger was never entered.  Otherwise, it returns the value
   that the user specified with `r'. (Note that much of the time,
   the user will abort with C-], and we will never have a chance to
   return anything at all.)

   SIGNAL_VARS_ONLY means we should only look at debug-on-signal
   and stack-trace-on-signal to control whether we do anything.
   This is so that debug-on-error doesn't make handled errors
   cause the debugger to get invoked.

   STACK_TRACE_DISPLAYED and DEBUGGER_ENTERED are used so that
   those functions aren't done more than once in a single `signal'
   session. */

static Lisp_Object
signal_call_debugger(Lisp_Object conditions,
		     Lisp_Object sig, Lisp_Object data,
		     Lisp_Object active_handlers,
		     int signal_vars_only,
		     int *stack_trace_displayed, int *debugger_entered)
{
	/* This function can GC */
	Lisp_Object val = Qunbound;
	Lisp_Object all_handlers = Vcondition_handlers;
	Lisp_Object temp_data = Qnil;
	int speccount = specpdl_depth();
	struct gcpro gcpro1, gcpro2;
	GCPRO2(all_handlers, temp_data);

	Vcondition_handlers = active_handlers;

	temp_data = Fcons(sig, data);	/* needed for skip_debugger */

	if (!entering_debugger && !*stack_trace_displayed && !signal_vars_only
	    && wants_debugger(Vstack_trace_on_error, conditions)
	    && !skip_debugger(conditions, temp_data)) {
		specbind(Qdebug_on_error, Qnil);
		specbind(Qstack_trace_on_error, Qnil);
		specbind(Qdebug_on_signal, Qnil);
		specbind(Qstack_trace_on_signal, Qnil);

		if (!noninteractive)
			internal_with_output_to_temp_buffer(build_string
							    ("*Backtrace*"),
							    backtrace_259, Qnil,
							    Qnil);
		else		/* in batch mode, we want this going to stderr. */
			backtrace_259(Qnil);
		unbind_to(speccount, Qnil);
		*stack_trace_displayed = 1;
	}

	if (!entering_debugger && !*debugger_entered && !signal_vars_only
	    && (EQ(sig, Qquit)
		? debug_on_quit : wants_debugger(Vdebug_on_error, conditions))
	    && !skip_debugger(conditions, temp_data)) {
		debug_on_quit &= ~2;	/* reset critical bit */
		specbind(Qdebug_on_error, Qnil);
		specbind(Qstack_trace_on_error, Qnil);
		specbind(Qdebug_on_signal, Qnil);
		specbind(Qstack_trace_on_signal, Qnil);

		val = call_debugger(list2(Qerror, (Fcons(sig, data))));
		*debugger_entered = 1;
	}

	if (!entering_debugger && !*stack_trace_displayed
	    && wants_debugger(Vstack_trace_on_signal, conditions)) {
		specbind(Qdebug_on_error, Qnil);
		specbind(Qstack_trace_on_error, Qnil);
		specbind(Qdebug_on_signal, Qnil);
		specbind(Qstack_trace_on_signal, Qnil);

		if (!noninteractive)
			internal_with_output_to_temp_buffer(build_string
							    ("*Backtrace*"),
							    backtrace_259, Qnil,
							    Qnil);
		else		/* in batch mode, we want this going to stderr. */
			backtrace_259(Qnil);
		unbind_to(speccount, Qnil);
		*stack_trace_displayed = 1;
	}

	if (!entering_debugger && !*debugger_entered && (EQ(sig, Qquit)
							 ? debug_on_quit
							 :
							 wants_debugger
							 (Vdebug_on_signal,
							  conditions))) {
		debug_on_quit &= ~2;	/* reset critical bit */
		specbind(Qdebug_on_error, Qnil);
		specbind(Qstack_trace_on_error, Qnil);
		specbind(Qdebug_on_signal, Qnil);
		specbind(Qstack_trace_on_signal, Qnil);

		val = call_debugger(list2(Qerror, (Fcons(sig, data))));
		*debugger_entered = 1;
	}

	UNGCPRO;
	Vcondition_handlers = all_handlers;
	return unbind_to(speccount, val);
}

/************************************************************************/
/*		       The basic special forms				*/
/************************************************************************/

/* Except for Fprogn(), the basic special forms below are only called
   from interpreted code.  The byte compiler turns them into bytecodes. */

DEFUN("or", For, 0, UNEVALLED, 0,	/*
Eval args until one of them yields non-nil, then return that value.
The remaining args are not evalled at all.
If all args return nil, return nil.
*/
      (args))
{
	/* This function can GC */
	REGISTER Lisp_Object val;

	LIST_LOOP_2(arg, args) {
		if (!NILP(val = Feval(arg)))
			return val;
	}

	return Qnil;
}

DEFUN("and", Fand, 0, UNEVALLED, 0,	/*
Eval args until one of them yields nil, then return nil.
The remaining args are not evalled at all.
If no arg yields nil, return the last arg's value.
*/
      (args))
{
	/* This function can GC */
	REGISTER Lisp_Object val = Qt;

	LIST_LOOP_2(arg, args) {
		if (NILP(val = Feval(arg)))
			return val;
	}

	return val;
}

DEFUN("if", Fif, 2, UNEVALLED, 0,	/*
\(if COND THEN ELSE...): if COND yields non-nil, do THEN, else do ELSE...
Returns the value of THEN or the value of the last of the ELSE's.
THEN must be one expression, but ELSE... can be zero or more expressions.
If COND yields nil, and there are no ELSE's, the value is nil.
*/
      (args))
{
	/* This function can GC */
	Lisp_Object condition = XCAR(args);
	Lisp_Object then_form = XCAR(XCDR(args));
	Lisp_Object else_forms = XCDR(XCDR(args));

	if (!NILP(Feval(condition)))
		return Feval(then_form);
	else
		return Fprogn(else_forms);
}

/* Macros `when' and `unless' are trivially defined in Lisp,
   but it helps for bootstrapping to have them ALWAYS defined. */

DEFUN("when", Fwhen, 1, MANY, 0,	/*
\(when COND BODY...): if COND yields non-nil, do BODY, else return nil.
BODY can be zero or more expressions.  If BODY is nil, return nil.
*/
      (int nargs, Lisp_Object * args))
{
	Lisp_Object cond = args[0];
	Lisp_Object body;

	switch (nargs) {
	case 1:
		body = Qnil;
		break;
	case 2:
		body = args[1];
		break;
	default:
		body = Fcons(Qprogn, Flist(nargs - 1, args + 1));
		break;
	}

	return list3(Qif, cond, body);
}

DEFUN("unless", Funless, 1, MANY, 0,	/*
\(unless COND BODY...): if COND yields nil, do BODY, else return nil.
BODY can be zero or more expressions.  If BODY is nil, return nil.
*/
      (int nargs, Lisp_Object * args))
{
	Lisp_Object cond = args[0];
	Lisp_Object body = Flist(nargs - 1, args + 1);
	return Fcons(Qif, Fcons(cond, Fcons(Qnil, body)));
}

DEFUN("cond", Fcond, 0, UNEVALLED, 0,	/*
\(cond CLAUSES...): try each clause until one succeeds.
Each clause looks like (CONDITION BODY...).  CONDITION is evaluated
and, if the value is non-nil, this clause succeeds:
then the expressions in BODY are evaluated and the last one's
value is the value of the cond-form.
If no clause succeeds, cond returns nil.
If a clause has one element, as in (CONDITION),
CONDITION's value if non-nil is returned from the cond-form.
*/
      (args))
{
	/* This function can GC */
	REGISTER Lisp_Object val;

	LIST_LOOP_2(clause, args) {
		CHECK_CONS(clause);
		if (!NILP(val = Feval(XCAR(clause)))) {
			if (!NILP(clause = XCDR(clause))) {
				CHECK_TRUE_LIST(clause);
				val = Fprogn(clause);
			}
			return val;
		}
	}

	return Qnil;
}

DEFUN("progn", Fprogn, 0, UNEVALLED, 0,	/*
\(progn BODY...): eval BODY forms sequentially and return value of last one.
*/
      (args))
{
	/* This function can GC */
	/* Caller must provide a true list in ARGS */
	REGISTER Lisp_Object val = Qnil;
	struct gcpro gcpro1;

	GCPRO1(args);

	{
		LIST_LOOP_2(form, args)
		    val = Feval(form);
	}

	UNGCPRO;
	return val;
}

/* Fprog1() is the canonical example of a function that must GCPRO a
   Lisp_Object across calls to Feval(). */

DEFUN("prog1", Fprog1, 1, UNEVALLED, 0,	/*
Similar to `progn', but the value of the first form is returned.
\(prog1 FIRST BODY...): All the arguments are evaluated sequentially.
The value of FIRST is saved during evaluation of the remaining args,
whose values are discarded.
*/
      (args))
{
	/* This function can GC */
	REGISTER Lisp_Object val;
	struct gcpro gcpro1;

	val = Feval(XCAR(args));

	GCPRO1(val);

	{
		LIST_LOOP_2(form, XCDR(args))
		    Feval(form);
	}

	UNGCPRO;
	return val;
}

DEFUN("prog2", Fprog2, 2, UNEVALLED, 0,	/*
Similar to `progn', but the value of the second form is returned.
\(prog2 FIRST SECOND BODY...): All the arguments are evaluated sequentially.
The value of SECOND is saved during evaluation of the remaining args,
whose values are discarded.
*/
      (args))
{
	/* This function can GC */
	REGISTER Lisp_Object val;
	struct gcpro gcpro1;

	Feval(XCAR(args));
	args = XCDR(args);
	val = Feval(XCAR(args));
	args = XCDR(args);

	GCPRO1(val);

	{
		LIST_LOOP_2(form, args)
		    Feval(form);
	}

	UNGCPRO;
	return val;
}

DEFUN("let*", FletX, 1, UNEVALLED, 0,	/*
\(let* VARLIST BODY...): bind variables according to VARLIST then eval BODY.
The value of the last form in BODY is returned.
Each element of VARLIST is a symbol (which is bound to nil)
or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).
Each VALUEFORM can refer to the symbols already bound by this VARLIST.
*/
      (args))
{
	/* This function can GC */
	Lisp_Object varlist = XCAR(args);
	Lisp_Object body = XCDR(args);
	int speccount = specpdl_depth();

	EXTERNAL_LIST_LOOP_3(var, varlist, tail) {
		Lisp_Object symbol, value, tem;
		if (SYMBOLP(var))
			symbol = var, value = Qnil;
		else {
			CHECK_CONS(var);
			symbol = XCAR(var);
			tem = XCDR(var);
			if (NILP(tem))
				value = Qnil;
			else {
				CHECK_CONS(tem);
				value = Feval(XCAR(tem));
				if (!NILP(XCDR(tem)))
					signal_simple_error
					    ("`let' bindings can have only one value-form",
					     var);
			}
		}
		specbind(symbol, value);
	}
	return unbind_to(speccount, Fprogn(body));
}

DEFUN("let", Flet, 1, UNEVALLED, 0,	/*
\(let VARLIST BODY...): bind variables according to VARLIST then eval BODY.
The value of the last form in BODY is returned.
Each element of VARLIST is a symbol (which is bound to nil)
or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).
All the VALUEFORMs are evalled before any symbols are bound.
*/
      (args))
{
	/* This function can GC */
	Lisp_Object varlist = XCAR(args);
	Lisp_Object body = XCDR(args);
	int speccount = specpdl_depth();
	int idx;
	int varcount;

	GET_EXTERNAL_LIST_LENGTH(varlist, varcount);

	/* Make space to hold the values to give the bound variables. */
	{
		Lisp_Object temps[varcount];
		struct gcpro gcpro1;

		/* clean sweep */
		memset(temps, 0, sizeof(Lisp_Object)*varcount);

		/* Compute the values and store them in `temps' */
		GCPROn(temps, varcount);

		idx = 0;
		LIST_LOOP_2(var, varlist) {
			Lisp_Object *value = &temps[idx++];
			if (SYMBOLP(var))
				*value = Qnil;
			else {
				Lisp_Object tem;
				CHECK_CONS(var);
				tem = XCDR(var);
				if (NILP(tem))
					*value = Qnil;
				else {
					CHECK_CONS(tem);
					*value = Feval(XCAR(tem));

					if (!NILP(XCDR(tem))) {
						signal_simple_error(
							"`let' bindings can "
							"have only one "
							"value-form", var);
					}
				}
			}
		}

		idx = 0;
		LIST_LOOP_2(var, varlist) {
			specbind(SYMBOLP(var) ? var : XCAR(var), temps[idx++]);
		}

		UNGCPRO;
	}

	return unbind_to(speccount, Fprogn(body));
}

DEFUN("while", Fwhile, 1, UNEVALLED, 0,	/*
\(while TEST BODY...): if TEST yields non-nil, eval BODY... and repeat.
The order of execution is thus TEST, BODY, TEST, BODY and so on
until TEST returns nil.
*/
      (args))
{
	/* This function can GC */
	Lisp_Object test = XCAR(args);
	Lisp_Object body = XCDR(args);

	while (!NILP(Feval(test))) {
		QUIT;
		Fprogn(body);
	}

	return Qnil;
}

DEFUN("setq", Fsetq, 0, UNEVALLED, 0,	/*
\(setq SYM VAL SYM VAL ...): set each SYM to the value of its VAL.
The symbols SYM are variables; they are literal (not evaluated).
The values VAL are expressions; they are evaluated.
Thus, (setq x (1+ y)) sets `x' to the value of `(1+ y)'.
The second VAL is not computed until after the first SYM is set, and so on;
each VAL can use the new value of variables set earlier in the `setq'.
The return value of the `setq' form is the value of the last VAL.
*/
      (args))
{
	/* This function can GC */
	Lisp_Object symbol, tail, val = Qnil;
	int nargs;
	struct gcpro gcpro1;

	GET_LIST_LENGTH(args, nargs);

	if (nargs & 1)		/* Odd number of arguments? */
		Fsignal(Qwrong_number_of_arguments,
			list2(Qsetq, make_int(nargs)));

	GCPRO1(val);

	PROPERTY_LIST_LOOP(tail, symbol, val, args) {
		val = Feval(val);
		Fset(symbol, val);
	}

	UNGCPRO;
	return val;
}

DEFUN("quote", Fquote, 1, UNEVALLED, 0,	/*
Return the argument, without evaluating it.  `(quote x)' yields `x'.
*/
      (args))
{
	return XCAR(args);
}

DEFUN("function", Ffunction, 1, UNEVALLED, 0,	/*
Like `quote', but preferred for objects which are functions.
In byte compilation, `function' causes its argument to be compiled.
`quote' cannot do that.
*/
      (args))
{
	return XCAR(args);
}

/************************************************************************/
/*			Defining functions/variables			*/
/************************************************************************/
static Lisp_Object define_function(Lisp_Object name, Lisp_Object defn)
{
	Ffset(name, defn);
	LOADHIST_ATTACH (Fcons (Qdefun, name));
	return name;
}

DEFUN("defun", Fdefun, 2, UNEVALLED, 0,	/*
\(defun NAME ARGLIST [DOCSTRING] BODY...): define NAME as a function.
The definition is (lambda ARGLIST [DOCSTRING] BODY...).
See also the function `interactive'.
*/
      (args))
{
	/* This function can GC */
	return define_function(XCAR(args), Fcons(Qlambda, XCDR(args)));
}

DEFUN("defmacro", Fdefmacro, 2, UNEVALLED, 0,	/*
\(defmacro NAME ARGLIST [DOCSTRING] BODY...): define NAME as a macro.
The definition is (macro lambda ARGLIST [DOCSTRING] BODY...).
When the macro is called, as in (NAME ARGS...),
the function (lambda ARGLIST BODY...) is applied to
the list ARGS... as it appears in the expression,
and the result should be a form to be evaluated instead of the original.
*/
      (args))
{
	/* This function can GC */
	return define_function(XCAR(args),
			       Fcons(Qmacro, Fcons(Qlambda, XCDR(args))));
}

DEFUN("defvar", Fdefvar, 1, UNEVALLED, 0,	/*
\(defvar SYMBOL INITVALUE DOCSTRING): define SYMBOL as a variable.
You are not required to define a variable in order to use it,
but the definition can supply documentation and an initial value
in a way that tags can recognize.

INITVALUE is evaluated, and used to set SYMBOL, only if SYMBOL's value is
void. (However, when you evaluate a defvar interactively, it acts like a
defconst: SYMBOL's value is always set regardless of whether it's currently
void.)
If SYMBOL is buffer-local, its default value is what is set;
buffer-local values are not affected.
INITVALUE and DOCSTRING are optional.
If DOCSTRING starts with *, this variable is identified as a user option.
This means that M-x set-variable recognizes it.
If INITVALUE is missing, SYMBOL's value is not set.

In lisp-interaction-mode defvar is treated as defconst.
*/
      (args))
{
	/* This function can GC */
	Lisp_Object sym = XCAR(args);

	if (!NILP(args = XCDR(args))) {
		Lisp_Object val = XCAR(args);

		if (NILP(Fdefault_boundp(sym))) {
			struct gcpro gcpro1;
			GCPRO1(val);
			val = Feval(val);
			Fset_default(sym, val);
			UNGCPRO;
		}

		if (!NILP(args = XCDR(args))) {
			Lisp_Object doc = XCAR(args);
			Fput(sym, Qvariable_documentation, doc);
			if (!NILP(args = XCDR(args)))
				error("too many arguments");
		}
	}
#ifdef I18N3
	if (!NILP(Vfile_domain))
		Fput(sym, Qvariable_domain, Vfile_domain);
#endif

	LOADHIST_ATTACH(sym);
	return sym;
}

DEFUN("defconst", Fdefconst, 2, UNEVALLED, 0,	/*
\(defconst SYMBOL INITVALUE DOCSTRING): define SYMBOL as a constant
variable.
The intent is that programs do not change this value, but users may.
Always sets the value of SYMBOL to the result of evalling INITVALUE.
If SYMBOL is buffer-local, its default value is what is set;
buffer-local values are not affected.
DOCSTRING is optional.
If DOCSTRING starts with *, this variable is identified as a user option.
This means that M-x set-variable recognizes it.

Note: do not use `defconst' for user options in libraries that are not
normally loaded, since it is useful for users to be able to specify
their own values for such variables before loading the library.
Since `defconst' unconditionally assigns the variable,
it would override the user's choice.
*/
      (args))
{
	/* This function can GC */
	Lisp_Object sym = XCAR(args);
	Lisp_Object val = Feval(XCAR(args = XCDR(args)));
	struct gcpro gcpro1;

	GCPRO1(val);

	Fset_default(sym, val);

	UNGCPRO;

	if (!NILP(args = XCDR(args))) {
		Lisp_Object doc = XCAR(args);
		Fput(sym, Qvariable_documentation, doc);
		if (!NILP(args = XCDR(args)))
			error("too many arguments");
	}
#ifdef I18N3
	if (!NILP(Vfile_domain))
		Fput(sym, Qvariable_domain, Vfile_domain);
#endif

	LOADHIST_ATTACH(sym);
	return sym;
}

DEFUN("user-variable-p", Fuser_variable_p, 1, 1, 0,	/*
Return t if VARIABLE is intended to be set and modified by users.
\(The alternative is a variable used internally in a Lisp program.)
Determined by whether the first character of the documentation
for the variable is `*'.
*/
      (variable))
{
	Lisp_Object documentation =
	    Fget(variable, Qvariable_documentation, Qnil);

	return
	    ((INTP(documentation) && XINT(documentation) < 0) ||
	     (STRINGP(documentation) &&
	      (string_byte(XSTRING(documentation), 0) == '*')) ||
	     /* If (STRING . INTEGER), a negative integer means a user variable. */
	     (CONSP(documentation)
	      && STRINGP(XCAR(documentation))
	      && INTP(XCDR(documentation))
	      && XINT(XCDR(documentation)) < 0)) ? Qt : Qnil;
}

DEFUN("macroexpand-internal", Fmacroexpand_internal, 1, 2, 0,	/*
Return result of expanding macros at top level of FORM.
If FORM is not a macro call, it is returned unchanged.
Otherwise, the macro is expanded and the expansion is considered
in place of FORM.  When a non-macro-call results, it is returned.

The second optional arg ENVIRONMENT specifies an environment of macro
definitions to shadow the loaded ones for use in file byte-compilation.
*/
      (form, environment))
{
	/* This function can GC */
	/* With cleanups from Hallvard Furuseth.  */
	REGISTER Lisp_Object expander, sym, def, tem;

	while (1) {
		/* Come back here each time we expand a macro call,
		   in case it expands into another macro call.  */
		if (!CONSP(form))
			break;
		/* Set SYM, give DEF and TEM right values in case SYM is not a symbol. */
		def = sym = XCAR(form);
		tem = Qnil;
		/* Trace symbols aliases to other symbols
		   until we get a symbol that is not an alias.  */
		while (SYMBOLP(def)) {
			QUIT;
			sym = def;
			tem = Fassq(sym, environment);
			if (NILP(tem)) {
				def = XSYMBOL(sym)->function;
				if (!UNBOUNDP(def))
					continue;
			}
			break;
		}
		/* Right now TEM is the result from SYM in ENVIRONMENT,
		   and if TEM is nil then DEF is SYM's function definition.  */
		if (NILP(tem)) {
			/* SYM is not mentioned in ENVIRONMENT.
			   Look at its function definition.  */
			if (UNBOUNDP(def)
			    || !CONSP(def))
				/* Not defined or definition not suitable */
				break;
			if (EQ(XCAR(def), Qautoload)) {
				/* Autoloading function: will it be a macro when loaded?  */
				tem = Felt(def, make_int(4));
				if (EQ(tem, Qt) || EQ(tem, Qmacro)) {
					/* Yes, load it and try again.  */
					/* do_autoload GCPROs both arguments */
					do_autoload(def, sym);
					continue;
				} else
					break;
			} else if (!EQ(XCAR(def), Qmacro))
				break;
			else
				expander = XCDR(def);
		} else {
			expander = XCDR(tem);
			if (NILP(expander))
				break;
		}
		form = apply1(expander, XCDR(form));
	}
	return form;
}

/************************************************************************/
/*			    Non-local exits				*/
/************************************************************************/

DEFUN("catch", Fcatch, 1, UNEVALLED, 0,	/*
\(catch TAG BODY...): eval BODY allowing nonlocal exits using `throw'.
TAG is evalled to get the tag to use.  Then the BODY is executed.
Within BODY, (throw TAG) with same tag exits BODY and exits this `catch'.
If no throw happens, `catch' returns the value of the last BODY form.
If a throw happens, it specifies the value to return from `catch'.
*/
      (args))
{
	/* This function can GC */
	Lisp_Object tag = Feval(XCAR(args));
	Lisp_Object body = XCDR(args);
	return internal_catch(tag, Fprogn, body, 0);
}

/* Set up a catch, then call C function FUNC on argument ARG.
   FUNC should return a Lisp_Object.
   This is how catches are done from within C code. */

Lisp_Object
internal_catch(Lisp_Object tag,
	       Lisp_Object(*func) (Lisp_Object arg),
	       Lisp_Object arg, int *volatile threw)
{
	/* This structure is made part of the chain `catchlist'.  */
	struct catchtag c;

	/* Fill in the components of c, and put it on the list.  */
	c.next = catchlist;
	c.tag = tag;
	c.val = Qnil;
	c.backlist = backtrace_list;
#if 0				/* FSFmacs */
	/* #### */
	c.handlerlist = handlerlist;
#endif
	c.lisp_eval_depth = lisp_eval_depth;
	c.pdlcount = specpdl_depth();
#if 0				/* FSFmacs */
	c.poll_suppress_count = async_timer_suppress_count;
#endif
	c.gcpro = _get_gcprolist();
	catchlist = &c;

	/* Call FUNC.  */
	if (SETJMP(c.jmp)) {
		/* Throw works by a longjmp that comes right here.  */
		if (threw)
			*threw = 1;
		return c.val;
	}
	c.val = (*func) (arg);
	if (threw)
		*threw = 0;
	catchlist = c.next;
#ifdef ERROR_CHECK_TYPECHECK
	check_error_state_sanity();
#endif
	return c.val;
}

/* Unwind the specbind, catch, and handler stacks back to CATCH, and
   jump to that CATCH, returning VALUE as the value of that catch.

   This is the guts Fthrow and Fsignal; they differ only in the way
   they choose the catch tag to throw to.  A catch tag for a
   condition-case form has a TAG of Qnil.

   Before each catch is discarded, unbind all special bindings and
   execute all unwind-protect clauses made above that catch.  Unwind
   the handler stack as we go, so that the proper handlers are in
   effect for each unwind-protect clause we run.  At the end, restore
   some static info saved in CATCH, and longjmp to the location
   specified in the

   This is used for correct unwinding in Fthrow and Fsignal.  */

static void unwind_to_catch(struct catchtag *c, Lisp_Object val)
{
#if 0				/* FSFmacs */
	/* #### */
	REGISTER int last_time;
#endif

	/* Unwind the specbind, catch, and handler stacks back to CATCH
	   Before each catch is discarded, unbind all special bindings
	   and execute all unwind-protect clauses made above that catch.
	   At the end, restore some static info saved in CATCH,
	   and longjmp to the location specified.
	 */

	/* Save the value somewhere it will be GC'ed.
	   (Can't overwrite tag slot because an unwind-protect may
	   want to throw to this same tag, which isn't yet invalid.) */
	c->val = val;

#if 0				/* FSFmacs */
	/* Restore the polling-suppression count.  */
	set_poll_suppress_count(catch->poll_suppress_count);
#endif

#if 0				/* FSFmacs */
	/* #### FSFmacs has the following loop.  Is it more correct? */
	do {
		last_time = catchlist == c;

		/* Unwind the specpdl stack, and then restore the proper set of
		   handlers.  */
		unbind_to(catchlist->pdlcount, Qnil);
		handlerlist = catchlist->handlerlist;
		catchlist = catchlist->next;
#ifdef ERROR_CHECK_TYPECHECK
		check_error_state_sanity();
#endif
	}
	while (!last_time);
#else				/* Actual SXEmacs code */
	/* Unwind the specpdl stack */
	unbind_to(c->pdlcount, Qnil);
	catchlist = c->next;
#ifdef ERROR_CHECK_TYPECHECK
	check_error_state_sanity();
#endif
#endif

	_set_gcprolist(c->gcpro);
	backtrace_list = c->backlist;
	lisp_eval_depth = c->lisp_eval_depth;

#ifdef DEFEND_AGAINST_THROW_RECURSION
	throw_level = 0;
#endif
	LONGJMP(c->jmp, 1);
}

static DOESNT_RETURN
throw_or_bomb_out(Lisp_Object tag, Lisp_Object val, int bomb_out_p,
		  Lisp_Object sig, Lisp_Object data)
{
#ifdef DEFEND_AGAINST_THROW_RECURSION
	/* die if we recurse more than is reasonable */
	if (++throw_level > 20)
		abort();
#endif

	/* If bomb_out_p is t, this is being called from Fsignal as a
	   "last resort" when there is no handler for this error and
	   the debugger couldn't be invoked, so we are throwing to
	   'top-level.  If this tag doesn't exist (happens during the
	   initialization stages) we would get in an infinite recursive
	   Fsignal/Fthrow loop, so instead we bomb out to the
	   really-early-error-handler.

	   Note that in fact the only time that the "last resort"
	   occurs is when there's no catch for 'top-level -- the
	   'top-level catch and the catch-all error handler are
	   established at the same time, in initial_command_loop/
	   top_level_1.

	   #### Fix this horrifitude!
	 */

	while (1) {
		REGISTER struct catchtag *c;

#if 0				/* FSFmacs */
		if (!NILP(tag))	/* #### */
#endif
			for (c = catchlist; c; c = c->next) {
				if (EQ(c->tag, tag))
					unwind_to_catch(c, val);
			}
		if (!bomb_out_p)
			tag = Fsignal(Qno_catch, list2(tag, val));
		else
			call1(Qreally_early_error_handler, Fcons(sig, data));
	}

	/* can't happen.  who cares? - (Sun's compiler does) */
	/* throw_level--; */
	/* getting tired of compilation warnings */
	/* return Qnil; */
}

/* See above, where CATCHLIST is defined, for a description of how
   Fthrow() works.

   Fthrow() is also called by Fsignal(), to do a non-local jump
   back to the appropriate condition-case handler after (maybe)
   the debugger is entered.  In that case, TAG is the value
   of Vcondition_handlers that was in place just after the
   condition-case handler was set up.  The car of this will be
   some data referring to the handler: Its car will be Qunbound
   (thus, this tag can never be generated by Lisp code), and
   its CDR will be the HANDLERS argument to condition_case_1()
   (either Qerror, Qt, or a list of handlers as in `condition-case').
   This works fine because Fthrow() does not care what TAG was
   passed to it: it just looks up the catch list for something
   that is EQ() to TAG.  When it finds it, it will longjmp()
   back to the place that established the catch (in this case,
   condition_case_1).  See below for more info.
*/

DEFUN("throw", Fthrow, 2, 2, 0,	/*
Throw to the catch for TAG and return VALUE from it.
Both TAG and VALUE are evalled.
*/
      (tag, value))
{
	throw_or_bomb_out(tag, value, 0, Qnil, Qnil);	/* Doesn't return */
	return Qnil;
}

DEFUN("unwind-protect", Funwind_protect, 1, UNEVALLED, 0,	/*
Do BODYFORM, protecting with UNWINDFORMS.
Usage looks like (unwind-protect BODYFORM UNWINDFORMS...).
If BODYFORM completes normally, its value is returned
after executing the UNWINDFORMS.
If BODYFORM exits nonlocally, the UNWINDFORMS are executed anyway.
*/
      (args))
{
	/* This function can GC */
	int speccount = specpdl_depth();

	record_unwind_protect(Fprogn, XCDR(args));
	return unbind_to(speccount, Feval(XCAR(args)));
}

/************************************************************************/
/*		      Signalling and trapping errors			*/
/************************************************************************/

static Lisp_Object condition_bind_unwind(Lisp_Object loser)
{
	Lisp_Cons *victim;
	/* ((handler-fun . handler-args) ... other handlers) */
	Lisp_Object tem = XCAR(loser);

	while (CONSP(tem)) {
		victim = XCONS(tem);
		tem = victim->cdr;
		free_cons(victim);
	}
	victim = XCONS(loser);

	if (EQ(loser, Vcondition_handlers))	/* may have been rebound to some tail */
		Vcondition_handlers = victim->cdr;

	free_cons(victim);
	return Qnil;
}

static Lisp_Object condition_case_unwind(Lisp_Object loser)
{
	Lisp_Cons *victim;

	/* ((<unbound> . clauses) ... other handlers */
	victim = XCONS(XCAR(loser));
	free_cons(victim);

	victim = XCONS(loser);
	if (EQ(loser, Vcondition_handlers))	/* may have been rebound to some tail */
		Vcondition_handlers = victim->cdr;

	free_cons(victim);
	return Qnil;
}

/* Split out from condition_case_3 so that primitive C callers
   don't have to cons up a lisp handler form to be evaluated. */

/* Call a function BFUN of one argument BARG, trapping errors as
   specified by HANDLERS.  If no error occurs that is indicated by
   HANDLERS as something to be caught, the return value of this
   function is the return value from BFUN.  If such an error does
   occur, HFUN is called, and its return value becomes the
   return value of condition_case_1().  The second argument passed
   to HFUN will always be HARG.  The first argument depends on
   HANDLERS:

   If HANDLERS is Qt, all errors (this includes QUIT, but not
   non-local exits with `throw') cause HFUN to be invoked, and VAL
   (the first argument to HFUN) is a cons (SIG . DATA) of the
   arguments passed to `signal'.  The debugger is not invoked even if
   `debug-on-error' was set.

   A HANDLERS value of Qerror is the same as Qt except that the
   debugger is invoked if `debug-on-error' was set.

   Otherwise, HANDLERS should be a list of lists (CONDITION-NAME BODY ...)
   exactly as in `condition-case', and errors will be trapped
   as indicated in HANDLERS.  VAL (the first argument to HFUN) will
   be a cons whose car is the cons (SIG . DATA) and whose CDR is the
   list (BODY ...) from the appropriate slot in HANDLERS.

   This function pushes HANDLERS onto the front of Vcondition_handlers
   (actually with a Qunbound marker as well -- see Fthrow() above
   for why), establishes a catch whose tag is this new value of
   Vcondition_handlers, and calls BFUN.  When Fsignal() is called,
   it calls Fthrow(), setting TAG to this same new value of
   Vcondition_handlers and setting VAL to the same thing that will
   be passed to HFUN, as above.  Fthrow() longjmp()s back to the
   jump point we just established, and we in turn just call the
   HFUN and return its value.

   For a real condition-case, HFUN will always be
   run_condition_case_handlers() and HARG is the argument VAR
   to condition-case.  That function just binds VAR to the cons
   (SIG . DATA) that is the CAR of VAL, and calls the handler
   (BODY ...) that is the CDR of VAL.  Note that before calling
   Fthrow(), Fsignal() restored Vcondition_handlers to the value
   it had *before* condition_case_1() was called.  This maintains
   consistency (so that the state of things at exit of
   condition_case_1() is the same as at entry), and implies
   that the handler can signal the same error again (possibly
   after processing of its own), without getting in an infinite
   loop. */

Lisp_Object
condition_case_1(Lisp_Object handlers,
		 Lisp_Object(*bfun) (Lisp_Object barg),
		 Lisp_Object barg,
		 Lisp_Object(*hfun) (Lisp_Object val, Lisp_Object harg),
		 Lisp_Object harg)
{
	int speccount = specpdl_depth();
	struct catchtag c;
	struct gcpro gcpro1;

#if 0				/* FSFmacs */
	c.tag = Qnil;
#else
	/* Do consing now so out-of-memory error happens up front */
	/* (unbound . stuff) is a special condition-case kludge marker
	   which is known specially by Fsignal.
	   This is an abomination, but to fix it would require either
	   making condition_case cons (a union of the conditions of the clauses)
	   or changing the byte-compiler output (no thanks). */
	c.tag = noseeum_cons(noseeum_cons(Qunbound, handlers),
			     Vcondition_handlers);
#endif
	c.val = Qnil;
	c.backlist = backtrace_list;
#if 0				/* FSFmacs */
	/* #### */
	c.handlerlist = handlerlist;
#endif
	c.lisp_eval_depth = lisp_eval_depth;
	c.pdlcount = specpdl_depth();
#if 0				/* FSFmacs */
	c.poll_suppress_count = async_timer_suppress_count;
#endif
	c.gcpro = _get_gcprolist();
	/* #### FSFmacs does the following statement *after* the setjmp(). */
	c.next = catchlist;

	if (SETJMP(c.jmp)) {
		/* throw does ungcpro, etc */
		return (*hfun) (c.val, harg);
	}

	record_unwind_protect(condition_case_unwind, c.tag);

	catchlist = &c;
#if 0				/* FSFmacs */
	h.handler = handlers;
	h.var = Qnil;
	h.next = handlerlist;
	h.tag = &c;
	handlerlist = &h;
#else
	Vcondition_handlers = c.tag;
#endif
	GCPRO1(harg);		/* Somebody has to gc-protect */

	c.val = ((*bfun) (barg));

	/* The following is *not* true: (ben)

	   ungcpro, restoring catchlist and condition_handlers are actually
	   redundant since unbind_to now restores them.  But it looks funny not to
	   have this code here, and it doesn't cost anything, so I'm leaving it. */
	UNGCPRO;
	catchlist = c.next;
#ifdef ERROR_CHECK_TYPECHECK
	check_error_state_sanity();
#endif
	Vcondition_handlers = XCDR(c.tag);

	return unbind_to(speccount, c.val);
}

static Lisp_Object run_condition_case_handlers(Lisp_Object val, Lisp_Object var)
{
	/* This function can GC */
#if 0				/* FSFmacs */
	if (!NILP(h.var))
		specbind(h.var, c.val);
	val = Fprogn(Fcdr(h.chosen_clause));

	/* Note that this just undoes the binding of h.var; whoever
	   longjmp()ed to us unwound the stack to c.pdlcount before
	   throwing. */
	unbind_to(c.pdlcount, Qnil);
	return val;
#else
	int speccount;

	CHECK_TRUE_LIST(val);
	if (NILP(var))
		return Fprogn(Fcdr(val));	/* tail call */

	speccount = specpdl_depth();
	specbind(var, Fcar(val));
	val = Fprogn(Fcdr(val));
	return unbind_to(speccount, val);
#endif
}

/* Here for bytecode to call non-consfully.  This is exactly like
   condition-case except that it takes three arguments rather
   than a single list of arguments. */
Lisp_Object
condition_case_3(Lisp_Object bodyform, Lisp_Object var, Lisp_Object handlers)
{
	/* This function can GC */
	EXTERNAL_LIST_LOOP_2(handler, handlers) {
		if (NILP(handler)) ;
		else if (CONSP(handler)) {
			Lisp_Object conditions = XCAR(handler);
			/* CONDITIONS must a condition name or a list of condition names */
			if (SYMBOLP(conditions)) ;
			else {
				EXTERNAL_LIST_LOOP_2(condition, conditions)
				    if (!SYMBOLP(condition))
					goto invalid_condition_handler;
			}
		} else {
		      invalid_condition_handler:
			signal_simple_error("Invalid condition handler",
					    handler);
		}
	}

	CHECK_SYMBOL(var);

	return condition_case_1(handlers,
				Feval, bodyform,
				run_condition_case_handlers, var);
}

DEFUN("condition-case", Fcondition_case, 2, UNEVALLED, 0,	/*
Regain control when an error is signalled.
Usage looks like (condition-case VAR BODYFORM HANDLERS...).
Executes BODYFORM and returns its value if no error happens.
Each element of HANDLERS looks like (CONDITION-NAME BODY...)
where the BODY is made of Lisp expressions.

A handler is applicable to an error if CONDITION-NAME is one of the
error's condition names.  If an error happens, the first applicable
handler is run.  As a special case, a CONDITION-NAME of t matches
all errors, even those without the `error' condition name on them
\(e.g. `quit').

The car of a handler may be a list of condition names
instead of a single condition name.

When a handler handles an error,
control returns to the condition-case and the handler BODY... is executed
with VAR bound to (SIGNALED-CONDITIONS . SIGNAL-DATA).
VAR may be nil; then you do not get access to the signal information.

The value of the last BODY form is returned from the condition-case.
See also the function `signal' for more info.

Note that at the time the condition handler is invoked, the Lisp stack
and the current catches, condition-cases, and bindings have all been
popped back to the state they were in just before the call to
`condition-case'.  This means that resignalling the error from
within the handler will not result in an infinite loop.

If you want to establish an error handler that is called with the
Lisp stack, bindings, etc. as they were when `signal' was called,
rather than when the handler was set, use `call-with-condition-handler'.
*/
      (args))
{
	/* This function can GC */
	Lisp_Object var = XCAR(args);
	Lisp_Object bodyform = XCAR(XCDR(args));
	Lisp_Object handlers = XCDR(XCDR(args));
	return condition_case_3(bodyform, var, handlers);
}

DEFUN("call-with-condition-handler", Fcall_with_condition_handler, 2, MANY, 0,	/*
Regain control when an error is signalled, without popping the stack.
Usage looks like (call-with-condition-handler HANDLER FUNCTION &rest ARGS).
This function is similar to `condition-case', but the handler is invoked
with the same environment (Lisp stack, bindings, catches, condition-cases)
that was current when `signal' was called, rather than when the handler
was established.

HANDLER should be a function of one argument, which is a cons of the args
\(SIG . DATA) that were passed to `signal'.  It is invoked whenever
`signal' is called (this differs from `condition-case', which allows
you to specify which errors are trapped).  If the handler function
returns, `signal' continues as if the handler were never invoked.
\(It continues to look for handlers established earlier than this one,
and invokes the standard error-handler if none is found.)
*/
      (int nargs, Lisp_Object * args))
{				/* Note!  Args side-effected! */
	/* This function can GC */
	int speccount = specpdl_depth();
	Lisp_Object tem;

	/* #### If there were a way to check that args[0] were a function
	   which accepted one arg, that should be done here ... */

	/* (handler-fun . handler-args) */
	tem = noseeum_cons(list1(args[0]), Vcondition_handlers);
	record_unwind_protect(condition_bind_unwind, tem);
	Vcondition_handlers = tem;

	/* Caller should have GC-protected args */
	return unbind_to(speccount, Ffuncall(nargs - 1, args + 1));
}

static int condition_type_p(Lisp_Object type, Lisp_Object conditions)
{
	if (EQ(type, Qt))
		/* (condition-case c # (t c)) catches -all- signals
		 *   Use with caution! */
		return 1;

	if (SYMBOLP(type))
		return !NILP(Fmemq(type, conditions));

	for (; CONSP(type); type = XCDR(type))
		if (!NILP(Fmemq(XCAR(type), conditions)))
			return 1;

	return 0;
}

static Lisp_Object return_from_signal(Lisp_Object value)
{
#if 1
	/* Most callers are not prepared to handle gc if this
	   returns.  So, since this feature is not very useful,
	   take it out.  */
	/* Have called debugger; return value to signaller  */
	return value;
#else				/* But the reality is that that stinks, because: */
	/* GACK!!! Really want some way for debug-on-quit errors
	   to be continuable!! */
	error("Returning a value from an error is no longer supported");
#endif
}

extern int in_display;

/************************************************************************/
/*		 the workhorse error-signaling function			*/
/************************************************************************/

/* #### This function has not been synched with FSF.  It diverges
   significantly. */

static Lisp_Object signal_1(Lisp_Object sig, Lisp_Object data)
{
	/* This function can GC */
	struct gcpro gcpro1, gcpro2;
	Lisp_Object conditions;
	Lisp_Object handlers;
	/* signal_call_debugger() could get called more than once
	   (once when a call-with-condition-handler is about to
	   be dealt with, and another when a condition-case handler
	   is about to be invoked).  So make sure the debugger and/or
	   stack trace aren't done more than once. */
	int stack_trace_displayed = 0;
	int debugger_entered = 0;
	GCPRO2(conditions, handlers);

	if (!initialized) {
		/* who knows how much has been initialized?  Safest bet is
		   just to bomb out immediately. */
		/* let's not use stderr_out() here, because that does a bunch of
		   things that might not be safe yet. */
		fprintf(stderr, "Error before initialization is complete!\n");
		abort();
	}

	if (gc_in_progress || in_display)
		/* This is one of many reasons why you can't run lisp code from redisplay.
		   There is no sensible way to handle errors there. */
		abort();

	conditions = Fget(sig, Qerror_conditions, Qnil);

	for (handlers = Vcondition_handlers;
	     CONSP(handlers); handlers = XCDR(handlers)) {
		Lisp_Object handler_fun = XCAR(XCAR(handlers));
		Lisp_Object handler_data = XCDR(XCAR(handlers));
		Lisp_Object outer_handlers = XCDR(handlers);

		if (!UNBOUNDP(handler_fun)) {
			/* call-with-condition-handler */
			Lisp_Object tem;
			Lisp_Object all_handlers = Vcondition_handlers;
			struct gcpro ngcpro1;
			NGCPRO1(all_handlers);
			Vcondition_handlers = outer_handlers;

			tem = signal_call_debugger(conditions, sig, data,
						   outer_handlers, 1,
						   &stack_trace_displayed,
						   &debugger_entered);
			if (!UNBOUNDP(tem))
				RETURN_NUNGCPRO(return_from_signal(tem));

			tem = Fcons(sig, data);
			if (NILP(handler_data))
				tem = call1(handler_fun, tem);
			else {
				/* (This code won't be used (for now?).) */
				struct gcpro nngcpro1;
				Lisp_Object args[3] = {
					handler_fun, tem, handler_data};

				NNGCPROn(args, 3);
				tem = Fapply(3, args);
				NNUNGCPRO;
			}
			NUNGCPRO;
#if 0
			if (!EQ(tem, Qsignal))
				return return_from_signal(tem);
#endif
			/* If handler didn't throw, try another handler */
			Vcondition_handlers = all_handlers;
		}

		/* It's a condition-case handler */

		/* t is used by handlers for all conditions, set up by C code.
		 *  debugger is not called even if debug_on_error */
		else if (EQ(handler_data, Qt)) {
			UNGCPRO;
			return Fthrow(handlers, Fcons(sig, data));
		}
		/* `error' is used similarly to the way `t' is used, but in
		   addition it invokes the debugger if debug_on_error.
		   This is normally used for the outer command-loop error
		   handler. */
		else if (EQ(handler_data, Qerror)) {
			Lisp_Object tem =
			    signal_call_debugger(conditions, sig, data,
						 outer_handlers, 0,
						 &stack_trace_displayed,
						 &debugger_entered);

			UNGCPRO;
			if (!UNBOUNDP(tem))
				return return_from_signal(tem);

			tem = Fcons(sig, data);
			return Fthrow(handlers, tem);
		} else {
			/* handler established by real (Lisp) condition-case */
			Lisp_Object h;

			for (h = handler_data; CONSP(h); h = Fcdr(h)) {
				Lisp_Object clause = Fcar(h);
				Lisp_Object tem = Fcar(clause);

				if (condition_type_p(tem, conditions)) {
					tem =
					    signal_call_debugger(conditions,
								 sig, data,
								 outer_handlers,
								 1,
								 &stack_trace_displayed,
								 &debugger_entered);
					UNGCPRO;
					if (!UNBOUNDP(tem))
						return return_from_signal(tem);

					/* Doesn't return */
					tem =
					    Fcons(Fcons(sig, data),
						  Fcdr(clause));
					return Fthrow(handlers, tem);
				}
			}
		}
	}

	/* If no handler is present now, try to run the debugger,
	   and if that fails, throw to top level.

	   #### The only time that no handler is present is during
	   temacs or perhaps very early in SXEmacs.  In both cases,
	   there is no 'top-level catch. (That's why the
	   "bomb-out" hack was added.)

	   #### Fix this horrifitude!
	 */
	signal_call_debugger(conditions, sig, data, Qnil, 0,
			     &stack_trace_displayed, &debugger_entered);
	UNGCPRO;
	throw_or_bomb_out(Qtop_level, Qt, 1, sig, data);	/* Doesn't return */
	return Qnil;
}

/****************** Error functions class 1 ******************/

/* Class 1: General functions that signal an error.
   These functions take an error type and a list of associated error
   data. */

/* The simplest external error function: it would be called
   signal_continuable_error() in the terminology below, but it's
   Lisp-callable. */

DEFUN("signal", Fsignal, 2, 2, 0,	/*
Signal a continuable error.  Args are ERROR-SYMBOL, and associated DATA.
An error symbol is a symbol defined using `define-error'.
DATA should be a list.  Its elements are printed as part of the error message.
If the signal is handled, DATA is made available to the handler.
See also the function `signal-error', and the functions to handle errors:
`condition-case' and `call-with-condition-handler'.

Note that this function can return, if the debugger is invoked and the
user invokes the "return from signal" option.
*/
      (error_symbol, data))
{
	/* Fsignal() is one of these functions that's called all the time
	   with newly-created Lisp objects.  We allow this; but we must GC-
	   protect the objects because all sorts of weird stuff could
	   happen. */

	struct gcpro gcpro1;

	GCPRO1(data);
	if (!NILP(Vcurrent_error_state)) {
		if (!NILP(Vcurrent_warning_class))
			warn_when_safe_lispobj(Vcurrent_warning_class, Qwarning,
					       Fcons(error_symbol, data));
		Fthrow(Qunbound_suspended_errors_tag, Qnil);
		abort();	/* Better not get here! */
	}
	RETURN_UNGCPRO(signal_1(error_symbol, data));
}

/* Signal a non-continuable error. */

DOESNT_RETURN signal_error(Lisp_Object sig, Lisp_Object data)
{
	for (;;)
		Fsignal(sig, data);
}

#ifdef ERROR_CHECK_TYPECHECK
void check_error_state_sanity(void)
{
	struct catchtag *c;
	int found_error_tag = 0;

	for (c = catchlist; c; c = c->next) {
		if (EQ(c->tag, Qunbound_suspended_errors_tag)) {
			found_error_tag = 1;
			break;
		}
	}

	assert(found_error_tag || NILP(Vcurrent_error_state));
}
#endif

static Lisp_Object restore_current_warning_class(Lisp_Object warning_class)
{
	Vcurrent_warning_class = warning_class;
	return Qnil;
}

static Lisp_Object restore_current_error_state(Lisp_Object error_state)
{
	Vcurrent_error_state = error_state;
	return Qnil;
}

static Lisp_Object call_with_suspended_errors_1(Lisp_Object opaque_arg)
{
	Lisp_Object val;
	Lisp_Object *kludgy_args = (Lisp_Object*)get_opaque_ptr(opaque_arg);
	Lisp_Object no_error = kludgy_args[2];
	int speccount = specpdl_depth();

	if (!EQ(Vcurrent_error_state, no_error)) {
		record_unwind_protect(restore_current_error_state,
				      Vcurrent_error_state);
		Vcurrent_error_state = no_error;
	}
	PRIMITIVE_FUNCALL(val, get_opaque_ptr(kludgy_args[0]),
			  kludgy_args + 3, XINT(kludgy_args[1]));
	return unbind_to(speccount, val);
}

/* Many functions would like to do one of three things if an error
   occurs:

   (1) signal the error, as usual.
   (2) silently fail and return some error value.
   (3) do as (2) but issue a warning in the process.

   Currently there's lots of stuff that passes an Error_behavior
   value and calls maybe_signal_error() and other such functions.
   This approach is inherently error-prone and broken.  A much
   more robust and easier approach is to use call_with_suspended_errors().
   Wrap this around any function in which you might want errors
   to not be errors.
*/

Lisp_Object
call_with_suspended_errors(lisp_fn_t fun, volatile Lisp_Object retval,
			   Lisp_Object class, Error_behavior errb,
			   int nargs, ...)
{
	va_list vargs;
	int speccount;
	Lisp_Object kludgy_args[23];
	Lisp_Object *args = kludgy_args + 3;
	int i;
	Lisp_Object no_error;

	assert(SYMBOLP(class));	/* sanity-check */
	assert(!NILP(class));
	assert(nargs >= 0 && nargs < 20);

	/* ERROR_ME means don't trap errors. (However, if errors are
	   already trapped, we leave them trapped.)

	   Otherwise, we trap errors, and trap warnings if ERROR_ME_WARN.

	   If ERROR_ME_NOT, it causes no warnings even if warnings
	   were previously enabled.  However, we never change the
	   warning class from one to another. */
	if (!ERRB_EQ(errb, ERROR_ME)) {
		if (ERRB_EQ(errb, ERROR_ME_NOT))	/* person wants no warnings */
			class = Qnil;
		errb = ERROR_ME_NOT;
		no_error = Qt;
	} else
		no_error = Qnil;

	va_start(vargs, nargs);
	for (i = 0; i < nargs; i++)
		args[i] = va_arg(vargs, Lisp_Object);
	va_end(vargs);

	/* If error-checking is not disabled, just call the function.
	   It's important not to override disabled error-checking with
	   enabled error-checking. */

	if (ERRB_EQ(errb, ERROR_ME)) {
		Lisp_Object val;
		PRIMITIVE_FUNCALL(val, fun, args, nargs);
		return val;
	}

	speccount = specpdl_depth();
	if (NILP(class) || NILP(Vcurrent_warning_class)) {
		/* If we're currently calling for no warnings, then make it so.
		   If we're currently calling for warnings and we weren't
		   previously, then set our warning class; otherwise, leave
		   the existing one alone. */
		record_unwind_protect(restore_current_warning_class,
				      Vcurrent_warning_class);
		Vcurrent_warning_class = class;
	}

	{
		int threw;
		Lisp_Object the_retval;
		Lisp_Object opaque1 = make_opaque_ptr(kludgy_args);
		Lisp_Object opaque2 = make_opaque_ptr((void *)fun);
		struct gcpro gcpro1, gcpro2;

		GCPRO2(opaque1, opaque2);
		kludgy_args[0] = opaque2;
		kludgy_args[1] = make_int(nargs);
		kludgy_args[2] = no_error;
		the_retval = internal_catch(Qunbound_suspended_errors_tag,
					    call_with_suspended_errors_1,
					    opaque1, &threw);
		free_opaque_ptr(opaque1);
		free_opaque_ptr(opaque2);
		UNGCPRO;
		/* Use the returned value except in non-local exit, when
		   RETVAL applies. */
		/* Some perverse compilers require the perverse cast below.  */
		return unbind_to(speccount,
				 threw ? *((volatile Lisp_Object*)&(retval)) :
				 the_retval);
	}
}

/* Signal a non-continuable error or display a warning or do nothing,
   according to ERRB.  CLASS is the class of warning and should
   refer to what sort of operation is being done (e.g. Qtoolbar,
   Qresource, etc.). */

void
maybe_signal_error(Lisp_Object sig, Lisp_Object data, Lisp_Object class,
		   Error_behavior errb)
{
	if (ERRB_EQ(errb, ERROR_ME_NOT))
		return;
	else if (ERRB_EQ(errb, ERROR_ME_WARN))
		warn_when_safe_lispobj(class, Qwarning, Fcons(sig, data));
	else
		for (;;)
			Fsignal(sig, data);
}

/* Signal a continuable error or display a warning or do nothing,
   according to ERRB. */

Lisp_Object
maybe_signal_continuable_error(Lisp_Object sig, Lisp_Object data,
			       Lisp_Object class, Error_behavior errb)
{
	if (ERRB_EQ(errb, ERROR_ME_NOT))
		return Qnil;
	else if (ERRB_EQ(errb, ERROR_ME_WARN)) {
		warn_when_safe_lispobj(class, Qwarning, Fcons(sig, data));
		return Qnil;
	} else
		return Fsignal(sig, data);
}

/****************** Error functions class 2 ******************/

/* Class 2: Printf-like functions that signal an error.
   These functions signal an error of a specified type, whose data
   is a single string, created using the arguments. */

/* dump an error message; called like printf */

DOESNT_RETURN type_error(Lisp_Object type, const char *fmt, ...)
{
	Lisp_Object obj;
	va_list args;

	va_start(args, fmt);
	obj = emacs_doprnt_string_va((const Bufbyte *)GETTEXT(fmt), Qnil, -1,
				     args);
	va_end(args);

	/* Fsignal GC-protects its args */
	signal_error(type, list1(obj));
}

void
maybe_type_error(Lisp_Object type, Lisp_Object class, Error_behavior errb,
		 const char *fmt, ...)
{
	Lisp_Object obj;
	va_list args;

	/* Optimization: */
	if (ERRB_EQ(errb, ERROR_ME_NOT))
		return;

	va_start(args, fmt);
	obj = emacs_doprnt_string_va((const Bufbyte *)GETTEXT(fmt), Qnil, -1,
				     args);
	va_end(args);

	/* Fsignal GC-protects its args */
	maybe_signal_error(type, list1(obj), class, errb);
}

Lisp_Object continuable_type_error(Lisp_Object type, const char *fmt, ...)
{
	Lisp_Object obj;
	va_list args;

	va_start(args, fmt);
	obj = emacs_doprnt_string_va((const Bufbyte *)GETTEXT(fmt), Qnil, -1,
				     args);
	va_end(args);

	/* Fsignal GC-protects its args */
	return Fsignal(type, list1(obj));
}

Lisp_Object
maybe_continuable_type_error(Lisp_Object type, Lisp_Object class,
			     Error_behavior errb, const char *fmt, ...)
{
	Lisp_Object obj;
	va_list args;

	/* Optimization: */
	if (ERRB_EQ(errb, ERROR_ME_NOT))
		return Qnil;

	va_start(args, fmt);
	obj = emacs_doprnt_string_va((const Bufbyte *)GETTEXT(fmt), Qnil, -1,
				     args);
	va_end(args);

	/* Fsignal GC-protects its args */
	return maybe_signal_continuable_error(type, list1(obj), class, errb);
}

/****************** Error functions class 3 ******************/

/* Class 3: Signal an error with a string and an associated object.
   These functions signal an error of a specified type, whose data
   is two objects, a string and a related Lisp object (usually the object
   where the error is occurring). */

DOESNT_RETURN
signal_type_error(Lisp_Object type, const char *reason, Lisp_Object frob)
{
	if (UNBOUNDP(frob))
		signal_error(type, list1(build_translated_string(reason)));
	else
		signal_error(type,
			     list2(build_translated_string(reason), frob));
}

void
maybe_signal_type_error(Lisp_Object type, const char *reason,
			Lisp_Object frob, Lisp_Object class,
			Error_behavior errb)
{
	/* Optimization: */
	if (ERRB_EQ(errb, ERROR_ME_NOT))
		return;
	maybe_signal_error(type, list2(build_translated_string(reason), frob),
			   class, errb);
}

Lisp_Object
signal_type_continuable_error(Lisp_Object type, const char *reason,
			      Lisp_Object frob)
{
	return Fsignal(type, list2(build_translated_string(reason), frob));
}

Lisp_Object
maybe_signal_type_continuable_error(Lisp_Object type, const char *reason,
				    Lisp_Object frob, Lisp_Object class,
				    Error_behavior errb)
{
	/* Optimization: */
	if (ERRB_EQ(errb, ERROR_ME_NOT))
		return Qnil;
	return maybe_signal_continuable_error
	    (type, list2(build_translated_string(reason), frob), class, errb);
}

/****************** Error functions class 4 ******************/

/* Class 4: Printf-like functions that signal an error.
   These functions signal an error of a specified type, whose data
   is a two objects, a string (created using the arguments) and a
   Lisp object.
*/

DOESNT_RETURN
type_error_with_frob(Lisp_Object type, Lisp_Object frob, const char *fmt, ...)
{
	Lisp_Object obj;
	va_list args;

	va_start(args, fmt);
	obj = emacs_doprnt_string_va((const Bufbyte *)GETTEXT(fmt), Qnil, -1,
				     args);
	va_end(args);

	/* Fsignal GC-protects its args */
	signal_error(type, list2(obj, frob));
}

void
maybe_type_error_with_frob(Lisp_Object type, Lisp_Object frob,
			   Lisp_Object class, Error_behavior errb,
			   const char *fmt, ...)
{
	Lisp_Object obj;
	va_list args;

	/* Optimization: */
	if (ERRB_EQ(errb, ERROR_ME_NOT))
		return;

	va_start(args, fmt);
	obj = emacs_doprnt_string_va((const Bufbyte *)GETTEXT(fmt), Qnil, -1,
				     args);
	va_end(args);

	/* Fsignal GC-protects its args */
	maybe_signal_error(type, list2(obj, frob), class, errb);
}

Lisp_Object
continuable_type_error_with_frob(Lisp_Object type, Lisp_Object frob,
				 const char *fmt, ...)
{
	Lisp_Object obj;
	va_list args;

	va_start(args, fmt);
	obj = emacs_doprnt_string_va((const Bufbyte *)GETTEXT(fmt), Qnil, -1,
				     args);
	va_end(args);

	/* Fsignal GC-protects its args */
	return Fsignal(type, list2(obj, frob));
}

Lisp_Object
maybe_continuable_type_error_with_frob(Lisp_Object type, Lisp_Object frob,
				       Lisp_Object class, Error_behavior errb,
				       const char *fmt, ...)
{
	Lisp_Object obj;
	va_list args;

	/* Optimization: */
	if (ERRB_EQ(errb, ERROR_ME_NOT))
		return Qnil;

	va_start(args, fmt);
	obj = emacs_doprnt_string_va((const Bufbyte *)GETTEXT(fmt), Qnil, -1,
				     args);
	va_end(args);

	/* Fsignal GC-protects its args */
	return maybe_signal_continuable_error(type, list2(obj, frob),
					      class, errb);
}

/****************** Error functions class 5 ******************/

/* Class 5: Signal an error with a string and two associated objects.
   These functions signal an error of a specified type, whose data
   is three objects, a string and two related Lisp objects. */

DOESNT_RETURN
signal_type_error_2(Lisp_Object type, const char *reason,
		    Lisp_Object frob0, Lisp_Object frob1)
{
	signal_error(type, list3(build_translated_string(reason), frob0,
				 frob1));
}

void
maybe_signal_type_error_2(Lisp_Object type, const char *reason,
			  Lisp_Object frob0, Lisp_Object frob1,
			  Lisp_Object class, Error_behavior errb)
{
	/* Optimization: */
	if (ERRB_EQ(errb, ERROR_ME_NOT))
		return;
	maybe_signal_error(type, list3(build_translated_string(reason), frob0,
				       frob1), class, errb);
}

Lisp_Object
signal_type_continuable_error_2(Lisp_Object type, const char *reason,
				Lisp_Object frob0, Lisp_Object frob1)
{
	return Fsignal(type, list3(build_translated_string(reason), frob0,
				   frob1));
}

Lisp_Object
maybe_signal_type_continuable_error_2(Lisp_Object type, const char *reason,
				      Lisp_Object frob0, Lisp_Object frob1,
				      Lisp_Object class, Error_behavior errb)
{
	/* Optimization: */
	if (ERRB_EQ(errb, ERROR_ME_NOT))
		return Qnil;
	return maybe_signal_continuable_error
	    (type, list3(build_translated_string(reason), frob0,
			 frob1), class, errb);
}

/****************** Simple error functions class 2 ******************/

/* Simple class 2: Printf-like functions that signal an error.
   These functions signal an error of type Qerror, whose data
   is a single string, created using the arguments. */

/* dump an error message; called like printf */

DOESNT_RETURN error(const char *fmt, ...)
{
	Lisp_Object obj;
	va_list args;

	va_start(args, fmt);
	obj = emacs_doprnt_string_va((const Bufbyte *)GETTEXT(fmt), Qnil, -1,
				     args);
	va_end(args);

	/* Fsignal GC-protects its args */
	signal_error(Qerror, list1(obj));
}

void maybe_error(Lisp_Object class, Error_behavior errb, const char *fmt, ...)
{
	Lisp_Object obj;
	va_list args;

	/* Optimization: */
	if (ERRB_EQ(errb, ERROR_ME_NOT))
		return;

	va_start(args, fmt);
	obj = emacs_doprnt_string_va((const Bufbyte *)GETTEXT(fmt), Qnil, -1,
				     args);
	va_end(args);

	/* Fsignal GC-protects its args */
	maybe_signal_error(Qerror, list1(obj), class, errb);
}

Lisp_Object continuable_error(const char *fmt, ...)
{
	Lisp_Object obj;
	va_list args;

	va_start(args, fmt);
	obj = emacs_doprnt_string_va((const Bufbyte *)GETTEXT(fmt), Qnil, -1,
				     args);
	va_end(args);

	/* Fsignal GC-protects its args */
	return Fsignal(Qerror, list1(obj));
}

Lisp_Object
maybe_continuable_error(Lisp_Object class, Error_behavior errb,
			const char *fmt, ...)
{
	Lisp_Object obj;
	va_list args;

	/* Optimization: */
	if (ERRB_EQ(errb, ERROR_ME_NOT))
		return Qnil;

	va_start(args, fmt);
	obj = emacs_doprnt_string_va((const Bufbyte *)GETTEXT(fmt), Qnil, -1,
				     args);
	va_end(args);

	/* Fsignal GC-protects its args */
	return maybe_signal_continuable_error(Qerror, list1(obj), class, errb);
}

/****************** Simple error functions class 3 ******************/

/* Simple class 3: Signal an error with a string and an associated object.
   These functions signal an error of type Qerror, whose data
   is two objects, a string and a related Lisp object (usually the object
   where the error is occurring). */

DOESNT_RETURN signal_simple_error(const char *reason, Lisp_Object frob)
{
	signal_error(Qerror, list2(build_translated_string(reason), frob));
}

void
maybe_signal_simple_error(const char *reason, Lisp_Object frob,
			  Lisp_Object class, Error_behavior errb)
{
	/* Optimization: */
	if (ERRB_EQ(errb, ERROR_ME_NOT))
		return;
	maybe_signal_error(Qerror, list2(build_translated_string(reason), frob),
			   class, errb);
}

Lisp_Object
signal_simple_continuable_error(const char *reason, Lisp_Object frob)
{
	return Fsignal(Qerror, list2(build_translated_string(reason), frob));
}

Lisp_Object
maybe_signal_simple_continuable_error(const char *reason, Lisp_Object frob,
				      Lisp_Object class, Error_behavior errb)
{
	/* Optimization: */
	if (ERRB_EQ(errb, ERROR_ME_NOT))
		return Qnil;
	return maybe_signal_continuable_error
	    (Qerror, list2(build_translated_string(reason), frob), class, errb);
}

/****************** Simple error functions class 4 ******************/

/* Simple class 4: Printf-like functions that signal an error.
   These functions signal an error of type Qerror, whose data
   is a two objects, a string (created using the arguments) and a
   Lisp object.
*/

DOESNT_RETURN error_with_frob(Lisp_Object frob, const char *fmt, ...)
{
	Lisp_Object obj;
	va_list args;

	va_start(args, fmt);
	obj = emacs_doprnt_string_va((const Bufbyte *)GETTEXT(fmt), Qnil, -1,
				     args);
	va_end(args);

	/* Fsignal GC-protects its args */
	signal_error(Qerror, list2(obj, frob));
}

void
maybe_error_with_frob(Lisp_Object frob, Lisp_Object class,
		      Error_behavior errb, const char *fmt, ...)
{
	Lisp_Object obj;
	va_list args;

	/* Optimization: */
	if (ERRB_EQ(errb, ERROR_ME_NOT))
		return;

	va_start(args, fmt);
	obj = emacs_doprnt_string_va((const Bufbyte *)GETTEXT(fmt), Qnil, -1,
				     args);
	va_end(args);

	/* Fsignal GC-protects its args */
	maybe_signal_error(Qerror, list2(obj, frob), class, errb);
}

Lisp_Object continuable_error_with_frob(Lisp_Object frob, const char *fmt, ...)
{
	Lisp_Object obj;
	va_list args;

	va_start(args, fmt);
	obj = emacs_doprnt_string_va((const Bufbyte *)GETTEXT(fmt), Qnil, -1,
				     args);
	va_end(args);

	/* Fsignal GC-protects its args */
	return Fsignal(Qerror, list2(obj, frob));
}

Lisp_Object
maybe_continuable_error_with_frob(Lisp_Object frob, Lisp_Object class,
				  Error_behavior errb, const char *fmt, ...)
{
	Lisp_Object obj;
	va_list args;

	/* Optimization: */
	if (ERRB_EQ(errb, ERROR_ME_NOT))
		return Qnil;

	va_start(args, fmt);
	obj = emacs_doprnt_string_va((const Bufbyte *)GETTEXT(fmt), Qnil, -1,
				     args);
	va_end(args);

	/* Fsignal GC-protects its args */
	return maybe_signal_continuable_error(Qerror, list2(obj, frob),
					      class, errb);
}

/****************** Simple error functions class 5 ******************/

/* Simple class 5: Signal an error with a string and two associated objects.
   These functions signal an error of type Qerror, whose data
   is three objects, a string and two related Lisp objects. */

DOESNT_RETURN
signal_simple_error_2(const char *reason, Lisp_Object frob0, Lisp_Object frob1)
{
	signal_error(Qerror, list3(build_translated_string(reason), frob0,
				   frob1));
}

void
maybe_signal_simple_error_2(const char *reason, Lisp_Object frob0,
			    Lisp_Object frob1, Lisp_Object class,
			    Error_behavior errb)
{
	/* Optimization: */
	if (ERRB_EQ(errb, ERROR_ME_NOT))
		return;
	maybe_signal_error(Qerror, list3(build_translated_string(reason), frob0,
					 frob1), class, errb);
}

Lisp_Object
signal_simple_continuable_error_2(const char *reason, Lisp_Object frob0,
				  Lisp_Object frob1)
{
	return Fsignal(Qerror, list3(build_translated_string(reason), frob0,
				     frob1));
}

Lisp_Object
maybe_signal_simple_continuable_error_2(const char *reason, Lisp_Object frob0,
					Lisp_Object frob1, Lisp_Object class,
					Error_behavior errb)
{
	/* Optimization: */
	if (ERRB_EQ(errb, ERROR_ME_NOT))
		return Qnil;
	return maybe_signal_continuable_error
	    (Qerror, list3(build_translated_string(reason), frob0,
			   frob1), class, errb);
}

/* This is what the QUIT macro calls to signal a quit */
void signal_quit(void)
{
	/* This function can GC */
	if (EQ(Vquit_flag, Qcritical))
		debug_on_quit |= 2;	/* set critical bit. */
	Vquit_flag = Qnil;
	/* note that this is continuable. */
	Fsignal(Qquit, Qnil);
}

/* Used in core lisp functions for efficiency */
Lisp_Object signal_void_function_error(Lisp_Object function)
{
	return Fsignal(Qvoid_function, list1(function));
}

Lisp_Object signal_invalid_function_error(Lisp_Object function)
{
	return Fsignal(Qinvalid_function, list1(function));
}

Lisp_Object
signal_wrong_number_of_arguments_error(Lisp_Object function, int nargs)
{
	return Fsignal(Qwrong_number_of_arguments,
		       list2(function, make_int(nargs)));
}

/* Used in list traversal macros for efficiency. */
DOESNT_RETURN signal_malformed_list_error(Lisp_Object list)
{
	signal_error(Qmalformed_list, list1(list));
}

DOESNT_RETURN signal_malformed_property_list_error(Lisp_Object list)
{
	signal_error(Qmalformed_property_list, list1(list));
}

DOESNT_RETURN signal_circular_list_error(Lisp_Object list)
{
	signal_error(Qcircular_list, list1(list));
}

DOESNT_RETURN signal_circular_property_list_error(Lisp_Object list)
{
	signal_error(Qcircular_property_list, list1(list));
}

DOESNT_RETURN syntax_error(const char *reason, Lisp_Object frob)
{
	signal_type_error(Qsyntax_error, reason, frob);
}

DOESNT_RETURN
syntax_error_2(const char *reason, Lisp_Object frob1, Lisp_Object frob2)
{
	signal_type_error_2(Qsyntax_error, reason, frob1, frob2);
}

DOESNT_RETURN invalid_argument(const char *reason, Lisp_Object frob)
{
	signal_type_error(Qinvalid_argument, reason, frob);
}

DOESNT_RETURN
invalid_argument_2(const char *reason, Lisp_Object frob1, Lisp_Object frob2)
{
	signal_type_error_2(Qinvalid_argument, reason, frob1, frob2);
}

DOESNT_RETURN invalid_operation(const char *reason, Lisp_Object frob)
{
	signal_type_error(Qinvalid_operation, reason, frob);
}

DOESNT_RETURN
invalid_operation_2(const char *reason, Lisp_Object frob1, Lisp_Object frob2)
{
	signal_type_error_2(Qinvalid_operation, reason, frob1, frob2);
}

DOESNT_RETURN invalid_change(const char *reason, Lisp_Object frob)
{
	signal_type_error(Qinvalid_change, reason, frob);
}

DOESNT_RETURN
invalid_change_2(const char *reason, Lisp_Object frob1, Lisp_Object frob2)
{
	signal_type_error_2(Qinvalid_change, reason, frob1, frob2);
}

/************************************************************************/
/*			      User commands				*/
/************************************************************************/

DEFUN("commandp", Fcommandp, 1, 1, 0,	/*
Return t if FUNCTION makes provisions for interactive calling.
This means it contains a description for how to read arguments to give it.
The value is nil for an invalid function or a symbol with no function
definition.

Interactively callable functions include

-- strings and vectors (treated as keyboard macros)
-- lambda-expressions that contain a top-level call to `interactive'
-- autoload definitions made by `autoload' with non-nil fourth argument
(i.e. the interactive flag)
-- compiled-function objects with a non-nil `compiled-function-interactive'
value
-- subrs (built-in functions) that are interactively callable

Also, a symbol satisfies `commandp' if its function definition does so.
*/
      (function))
{
	Lisp_Object fun = indirect_function(function, 0);

	if (COMPILED_FUNCTIONP(fun))
		return XCOMPILED_FUNCTION(fun)->flags.interactivep ? Qt : Qnil;

	/* Lists may represent commands.  */
	if (CONSP(fun)) {
		Lisp_Object funcar = XCAR(fun);
		if (EQ(funcar, Qlambda))
			return Fassq(Qinteractive, Fcdr(Fcdr(fun)));
		if (EQ(funcar, Qautoload))
			return Fcar(Fcdr(Fcdr(Fcdr(fun))));
		else
			return Qnil;
	}

	/* Emacs primitives are interactive if their DEFUN specifies an
	   interactive spec.  */
	if (SUBRP(fun))
		return XSUBR(fun)->prompt ? Qt : Qnil;

	/* Strings and vectors are keyboard macros.  */
	if (VECTORP(fun) || STRINGP(fun))
		return Qt;

	/* Everything else (including Qunbound) is not a command.  */
	return Qnil;
}

DEFUN("command-execute", Fcommand_execute, 1, 3, 0,	/*
Execute CMD as an editor command.
CMD must be an object that satisfies the `commandp' predicate.
Optional second arg RECORD-FLAG is as in `call-interactively'.
The argument KEYS specifies the value to use instead of (this-command-keys)
when reading the arguments.
*/
      (cmd, record_flag, keys))
{
	/* This function can GC */
	Lisp_Object prefixarg;
	Lisp_Object final = cmd;
	struct backtrace backtrace;
	struct console *con = XCONSOLE(Vselected_console);

	prefixarg = con->prefix_arg;
	con->prefix_arg = Qnil;
	Vcurrent_prefix_arg = prefixarg;
	debug_on_next_call = 0;	/* #### from FSFmacs; correct? */

	if (SYMBOLP(cmd) && !NILP(Fget(cmd, Qdisabled, Qnil)))
		return run_hook(Vdisabled_command_hook);

	for (;;) {
		final = indirect_function(cmd, 1);
		if (CONSP(final) && EQ(Fcar(final), Qautoload)) {
			/* do_autoload GCPROs both arguments */
			do_autoload(final, cmd);
		} else
			break;
	}

	if (CONSP(final) || SUBRP(final) || COMPILED_FUNCTIONP(final)) {
		backtrace.function = &Qcall_interactively;
		backtrace.args = &cmd;
		backtrace.nargs = 1;
		backtrace.evalargs = 0;
		backtrace.pdlcount = specpdl_depth();
		backtrace.debug_on_exit = 0;
		PUSH_BACKTRACE(backtrace);

		final = Fcall_interactively(cmd, record_flag, keys);

		POP_BACKTRACE(backtrace);
		return final;
	} else if (STRINGP(final) || VECTORP(final)) {
		return Fexecute_kbd_macro(final, prefixarg);
	} else {
		Fsignal(Qwrong_type_argument, Fcons(Qcommandp, (EQ(cmd, final)
								? list1(cmd)
								: list2(cmd,
									final))));
		return Qnil;
	}
}

DEFUN("interactive-p", Finteractive_p, 0, 0, 0,	/*
Return t if function in which this appears was called interactively.
This means that the function was called with call-interactively (which
includes being called as the binding of a key)
and input is currently coming from the keyboard (not in keyboard macro).
*/
      ())
{
	REGISTER struct backtrace *btp = NULL;
	REGISTER Lisp_Object fun = Qnil;

	if (!INTERACTIVE)
		return Qnil;

	/*  Unless the object was compiled, skip the frame of interactive-p itself
	   (if interpreted) or the frame of byte-code (if called from a compiled
	   function).  Note that *btp->function may be a symbol pointing at a
	   compiled function. */
	btp = backtrace_list;

#if 0				/* FSFmacs */

	/* #### FSFmacs does the following instead.  I can't figure
	   out which one is more correct. */
	/* If this isn't a byte-compiled function, there may be a frame at
	   the top for Finteractive_p itself.  If so, skip it.  */
	fun = Findirect_function(*btp->function);
	if (SUBRP(fun) && XSUBR(fun) == &Sinteractive_p)
		btp = btp->next;

	/* If we're running an Emacs 18-style byte-compiled function, there
	   may be a frame for Fbyte_code.  Now, given the strictest
	   definition, this function isn't really being called
	   interactively, but because that's the way Emacs 18 always builds
	   byte-compiled functions, we'll accept it for now.  */
	if (EQ(*btp->function, Qbyte_code))
		btp = btp->next;

	/* If this isn't a byte-compiled function, then we may now be
	   looking at several frames for special forms.  Skip past them.  */
	while (btp && btp->nargs == UNEVALLED)
		btp = btp->next;

#else

	/* argh, COMPILED_FUNCTIONP evals its argument multiple times,
	 * so put it into a var first ... gosh I wish all those macros were
	 * inlines! -hrop */
	fun = Findirect_function(*btp->function);
	if (!(COMPILED_FUNCTIONP(fun))) {
		btp = btp->next;
	}
	for (;
	     btp && (btp->nargs == UNEVALLED
		     || EQ(*btp->function, Qbyte_code)); btp = btp->next) {
	}
	/* btp now points at the frame of the innermost function
	   that DOES eval its args.
	   If it is a built-in function (such as load or eval-region)
	   return nil.  */
	/* Beats me why this is necessary, but it is */
	if (btp && EQ(*btp->function, Qcall_interactively))
		return Qt;

#endif

	if (btp)
		fun = Findirect_function(*btp->function);
	if (SUBRP(fun))
		return Qnil;
	/* btp points to the frame of a Lisp function that called interactive-p.
	   Return t if that function was called interactively.  */
	if (btp && btp->next && EQ(*btp->next->function, Qcall_interactively))
		return Qt;
	return Qnil;
}

/************************************************************************/
/*			      Autoloading				*/
/************************************************************************/

DEFUN("autoload", Fautoload, 2, 5, 0,	/*
Define FUNCTION to autoload from FILENAME.
FUNCTION is a symbol; FILENAME is a file name string to pass to `load'.
The remaining optional arguments provide additional info about the
real definition.
DOCSTRING is documentation for FUNCTION.
INTERACTIVE, if non-nil, says FUNCTION can be called interactively.
TYPE indicates the type of the object:
nil or omitted says FUNCTION is a function,
`keymap' says FUNCTION is really a keymap, and
`macro' or t says FUNCTION is really a macro.
If FUNCTION already has a non-void function definition that is not an
autoload object, this function does nothing and returns nil.
*/
      (function, filename, docstring, interactive, type))
{
	/* This function can GC */
	CHECK_SYMBOL(function);
	CHECK_STRING(filename);

	/* If function is defined and not as an autoload, don't override */
	{
		Lisp_Object f = XSYMBOL(function)->function;
		if (!UNBOUNDP(f) && !(CONSP(f) && EQ(XCAR(f), Qautoload)))
			return Qnil;
	}

	if (purify_flag) {
		/* Attempt to avoid consing identical (string=) pure strings. */
		filename = Fsymbol_name(Fintern(filename, Qnil));
	}

	return Ffset(function, Fcons(Qautoload, list4(filename,
						      docstring,
						      interactive, type)));
}

Lisp_Object un_autoload(Lisp_Object oldqueue)
{
	/* This function can GC */
	REGISTER Lisp_Object queue, first, second;

	/* Queue to unwind is current value of Vautoload_queue.
	   oldqueue is the shadowed value to leave in Vautoload_queue.  */
	queue = Vautoload_queue;
	Vautoload_queue = oldqueue;
	while (CONSP(queue)) {
		first = XCAR(queue);
		second = Fcdr(first);
		first = Fcar(first);
		if (NILP(second))
			Vfeatures = first;
		else
			Ffset(first, second);
		queue = Fcdr(queue);
	}
	return Qnil;
}

void do_autoload(Lisp_Object fundef, Lisp_Object funname)
{
	/* This function can GC */
	int speccount = specpdl_depth();
	Lisp_Object fun = funname;
	struct gcpro gcpro1, gcpro2, gcpro3;

	CHECK_SYMBOL(funname);
	GCPRO3(fun, funname, fundef);

	/* Value saved here is to be restored into Vautoload_queue */
	record_unwind_protect(un_autoload, Vautoload_queue);
	Vautoload_queue = Qt;
	call4(Qload, Fcar(Fcdr(fundef)), Qnil, noninteractive ? Qt : Qnil,
	      Qnil);

	{
		Lisp_Object queue;

		/* Save the old autoloads, in case we ever do an unload. */
		for (queue = Vautoload_queue; CONSP(queue); queue = XCDR(queue)) {
			Lisp_Object first = XCAR(queue);
			Lisp_Object second = Fcdr(first);

			first = Fcar(first);

			/* Note: This test is subtle.  The cdr of an autoload-queue entry
			   may be an atom if the autoload entry was generated by a defalias
			   or fset. */
			if (CONSP(second))
				Fput(first, Qautoload, (XCDR(second)));
		}
	}

	/* Once loading finishes, don't undo it.  */
	Vautoload_queue = Qt;
	unbind_to(speccount, Qnil);

	fun = indirect_function(fun, 0);

#if 0				/* FSFmacs */
	if (!NILP(Fequal(fun, fundef)))
#else
	if (UNBOUNDP(fun)
	    || (CONSP(fun)
		&& EQ(XCAR(fun), Qautoload)))
#endif
		error("Autoloading failed to define function %s",
		      string_data(XSYMBOL(funname)->name));
	UNGCPRO;
}

/************************************************************************/
/*			   eval, funcall, apply				*/
/************************************************************************/

static Lisp_Object funcall_lambda(Lisp_Object fun,
				  int nargs, Lisp_Object args[]);
static int in_warnings;

static Lisp_Object in_warnings_restore(Lisp_Object minimus)
{
	in_warnings = 0;
	return Qnil;
}

DEFUN("eval", Feval, 1, 1, 0,	/*
Evaluate FORM and return its value.
*/
      (form))
{
	/* This function can GC */
	Lisp_Object fun, val, original_fun, original_args;
	int nargs;
	struct backtrace backtrace;

	if (!CONSP(form)) {
		if (SYMBOLP(form))
			return Fsymbol_value(form);
		else
			return form;
	}

	/* I think this is a pretty safe place to call Lisp code, don't you? */
	while (!in_warnings && !NILP(Vpending_warnings)) {
		struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
		int speccount = specpdl_depth();
		Lisp_Object this_warning_cons, this_warning, class, level,
		    messij;

		record_unwind_protect(in_warnings_restore, Qnil);
		in_warnings = 1;
		this_warning_cons = Vpending_warnings;
		this_warning = XCAR(this_warning_cons);
		/* in case an error occurs in the warn function, at least
		   it won't happen infinitely */
		Vpending_warnings = XCDR(Vpending_warnings);
		free_cons(XCONS(this_warning_cons));
		class = XCAR(this_warning);
		level = XCAR(XCDR(this_warning));
		messij = XCAR(XCDR(XCDR(this_warning)));
		free_list(this_warning);

		if (NILP(Vpending_warnings))
			Vpending_warnings_tail = Qnil;	/* perhaps not strictly necessary,
							   but safer */

		GCPRO4(form, class, level, messij);
		if (!STRINGP(messij))
			messij = Fprin1_to_string(messij, Qnil);
		call3(Qdisplay_warning, class, messij, level);
		UNGCPRO;
		unbind_to(speccount, Qnil);
	}

	QUIT;
#if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
	if ((consing_since_gc > gc_cons_threshold) || always_gc) {
		struct gcpro gcpro1;
		GCPRO1(form);
		garbage_collect_1();
		UNGCPRO;
	}
#endif	/* !bDWGC */

	if (++lisp_eval_depth > max_lisp_eval_depth) {
		if (max_lisp_eval_depth < 100)
			max_lisp_eval_depth = 100;
		if (lisp_eval_depth > max_lisp_eval_depth)
			error("Lisp nesting exceeds `max-lisp-eval-depth'");
	}

	/* We guaranteed CONSP (form) above */
	original_fun = XCAR(form);
	original_args = XCDR(form);

	GET_EXTERNAL_LIST_LENGTH(original_args, nargs);

	backtrace.pdlcount = specpdl_depth();
	backtrace.function = &original_fun;	/* This also protects them from gc */
	backtrace.args = &original_args;
	backtrace.nargs = UNEVALLED;
	backtrace.evalargs = 1;
	backtrace.debug_on_exit = 0;
	PUSH_BACKTRACE(backtrace);

	if (debug_on_next_call)
		do_debug_on_call(Qt);

	if (profiling_active)
		profile_increase_call_count(original_fun);

	/* At this point, only original_fun and original_args
	   have values that will be used below. */
retry:
	/* Optimise for no indirection.  */
	fun = original_fun;
	if (SYMBOLP (fun) && !EQ (fun, Qunbound)
	    && (fun = XSYMBOL (fun)->function, SYMBOLP (fun)))
		fun = indirect_function(original_fun, 1);

	if (SUBRP(fun)) {
		Lisp_Subr *subr = XSUBR(fun);
		int max_args = subr->max_args;

		if (nargs < subr->min_args)
			goto wrong_number_of_arguments;

		if (max_args == UNEVALLED) {	/* Optimize for the common case */
			backtrace.evalargs = 0;
			val =
			    (((Lisp_Object(*)(Lisp_Object)) subr_function(subr))
			     (original_args));
		} else if (nargs <= max_args) {
			struct gcpro gcpro1;
			Lisp_Object args[SUBR_MAX_ARGS];
			REGISTER Lisp_Object *p = args;

			/* clean sweep */
			memset(args, 0, sizeof(Lisp_Object)*SUBR_MAX_ARGS);

			GCPROn(args, countof(args));

			LIST_LOOP_2(arg, original_args) {
				*p++ = Feval(arg);
			}

			/* &optional args default to nil. */
			while (p - args < max_args)
				*p++ = Qnil;

			backtrace.args = args;
			backtrace.nargs = nargs;

			FUNCALL_SUBR(val, subr, args, max_args);

			UNGCPRO;
		} else if (max_args == MANY) {
			/* Pass a vector of evaluated arguments */
			struct gcpro gcpro1;
			Lisp_Object args[nargs];
			REGISTER Lisp_Object *p = args;

			/* clean sweep */
			memset(args, 0, sizeof(Lisp_Object)*nargs);

			GCPROn(args, nargs);

			LIST_LOOP_2(arg, original_args) {
				*p++ = Feval(arg);
			}

			backtrace.args = args;
			backtrace.nargs = nargs;

			val =
			    (((Lisp_Object(*)(int, Lisp_Object *))subr_function
			      (subr))
			     (nargs, args));

			UNGCPRO;
		} else {
		      wrong_number_of_arguments:
			val =
			    signal_wrong_number_of_arguments_error(original_fun,
								   nargs);
		}
	} else if (COMPILED_FUNCTIONP(fun)) {
		struct gcpro gcpro1;
		Lisp_Object args[nargs];
		REGISTER Lisp_Object *p = args;

		/* clean sweep */
		memset(args, 0, sizeof(Lisp_Object)*nargs);

		GCPROn(args, nargs);

		LIST_LOOP_2(arg, original_args) {
			*p++ = Feval(arg);
		}

		backtrace.args = args;
		backtrace.nargs = nargs;
		backtrace.evalargs = 0;

		val = funcall_compiled_function(fun, nargs, args);

		/* Do the debug-on-exit now, while args is still GCPROed.  */
		if (backtrace.debug_on_exit)
			val = do_debug_on_exit(val);
		/* Don't do it again when we return to eval.  */
		backtrace.debug_on_exit = 0;

		UNGCPRO;
	} else if (CONSP(fun)) {
		Lisp_Object funcar = XCAR(fun);

		if (EQ(funcar, Qautoload)) {
			/* do_autoload GCPROs both arguments */
			do_autoload(fun, original_fun);
			goto retry;
		} else if (EQ(funcar, Qmacro)) {
			val = Feval(apply1(XCDR(fun), original_args));
		} else if (EQ(funcar, Qlambda)) {
			struct gcpro gcpro1;
			Lisp_Object args[nargs];
			REGISTER Lisp_Object *p = args;

			/* clean sweep */
			memset(args, 0, sizeof(Lisp_Object)*nargs);

			GCPROn(args, nargs);

			LIST_LOOP_2(arg, original_args) {
				*p++ = Feval(arg);
			}

			UNGCPRO;

			backtrace.args = args;	/* this also GCPROs `args' */
			backtrace.nargs = nargs;
			backtrace.evalargs = 0;

			val = funcall_lambda(fun, nargs, args);

			/* Do the debug-on-exit now, while args is still GCPROed.  */
			if (backtrace.debug_on_exit)
				val = do_debug_on_exit(val);
			/* Don't do it again when we return to eval.  */
			backtrace.debug_on_exit = 0;
		} else {
			goto invalid_function;
		}
	} else if (UNBOUNDP(fun)) {
		val = signal_void_function_error(original_fun);
	} else {
	invalid_function:
		val = signal_invalid_function_error(original_fun);
	}

	lisp_eval_depth--;
	if (backtrace.debug_on_exit)
		val = do_debug_on_exit(val);
	POP_BACKTRACE(backtrace);
	return val;
}


/* #### Why is Feval so anal about GCPRO, Ffuncall so cavalier? */
DEFUN("funcall", Ffuncall, 1, MANY, 0,	/*
Call first argument as a function, passing the remaining arguments to it.
Thus, (funcall 'cons 'x 'y) returns (x . y).
*/
      (int nargs, Lisp_Object * args))
{
	/* This function can GC */
	Lisp_Object fun;
	Lisp_Object val;
	struct backtrace backtrace;
	int fun_nargs = nargs - 1;
	Lisp_Object *fun_args = args + 1;

	QUIT;
#if !defined HAVE_BDWGC || !defined EF_USE_BDWGC
	if ((consing_since_gc > gc_cons_threshold) || always_gc) {
		/* Callers should gcpro lexpr args */
		garbage_collect_1();
	}
#endif	/* !BDWGC */

	if (++lisp_eval_depth > max_lisp_eval_depth) {
		if (max_lisp_eval_depth < 100)
			max_lisp_eval_depth = 100;
		if (lisp_eval_depth > max_lisp_eval_depth)
			error("Lisp nesting exceeds `max-lisp-eval-depth'");
	}

	backtrace.pdlcount = specpdl_depth();
	backtrace.function = &args[0];
	backtrace.args = fun_args;
	backtrace.nargs = fun_nargs;
	backtrace.evalargs = 0;
	backtrace.debug_on_exit = 0;
	PUSH_BACKTRACE(backtrace);

	if (debug_on_next_call)
		do_debug_on_call(Qlambda);

      retry:

	fun = args[0];

	/* It might be useful to place this *after* all the checks.  */
	if (profiling_active)
		profile_increase_call_count(fun);

	/* We could call indirect_function directly, but profiling shows
	   this is worth optimizing by partially unrolling the loop.  */
	if (SYMBOLP(fun)) {
		fun = XSYMBOL(fun)->function;
		if (SYMBOLP(fun)) {
			fun = XSYMBOL(fun)->function;
			if (SYMBOLP(fun))
				fun = indirect_function(fun, 1);
		}
	}

	if (SUBRP(fun)) {
		Lisp_Subr *subr = XSUBR(fun);
		int max_args = subr->max_args;
		Lisp_Object spacious_args[SUBR_MAX_ARGS];

		if (fun_nargs == max_args) {	/* Optimize for the common case */
		      funcall_subr:
			{
				/* The "extra" braces placate GCC 2.95.4. */
				FUNCALL_SUBR(val, subr, fun_args, max_args);
			}
		} else if (fun_nargs < subr->min_args) {
			goto wrong_number_of_arguments;
		} else if (fun_nargs < max_args) {
			Lisp_Object *p = spacious_args;

			/* Default optionals to nil */
			while (fun_nargs--)
				*p++ = *fun_args++;
			while (p - spacious_args < max_args)
				*p++ = Qnil;

			fun_args = spacious_args;
			goto funcall_subr;
		} else if (max_args == MANY) {
			val = SUBR_FUNCTION(subr, MANY) (fun_nargs, fun_args);
		} else if (max_args == UNEVALLED) {	/* Can't funcall a special form */
			goto invalid_function;
		} else {
		      wrong_number_of_arguments:
			val =
			    signal_wrong_number_of_arguments_error(fun,
								   fun_nargs);
		}
	} else if (COMPILED_FUNCTIONP(fun)) {
		val = funcall_compiled_function(fun, fun_nargs, fun_args);
	} else if (CONSP(fun)) {
		Lisp_Object funcar = XCAR(fun);

		if (EQ(funcar, Qlambda)) {
			val = funcall_lambda(fun, fun_nargs, fun_args);
		} else if (EQ(funcar, Qautoload)) {
			/* do_autoload GCPROs both arguments */
			do_autoload(fun, args[0]);
			goto retry;
		} else {	/* Can't funcall a macro */

			goto invalid_function;
		}
	} else if (UNBOUNDP(fun)) {
		val = signal_void_function_error(args[0]);
	} else {
	      invalid_function:
		val = signal_invalid_function_error(fun);
	}

	lisp_eval_depth--;
	if (backtrace.debug_on_exit)
		val = do_debug_on_exit(val);
	POP_BACKTRACE(backtrace);
	return val;
}

DEFUN("functionp", Ffunctionp, 1, 1, 0,	/*
Return t if OBJECT can be called as a function, else nil.
A function is an object that can be applied to arguments,
using for example `funcall' or `apply'.
*/
      (object))
{
	if (SYMBOLP(object))
		object = indirect_function(object, 0);

	return
	    (SUBRP(object) ||
	     COMPILED_FUNCTIONP(object) ||
	     (CONSP(object) &&
	      (EQ(XCAR(object), Qlambda) || EQ(XCAR(object), Qautoload))))
	    ? Qt : Qnil;
}

static Lisp_Object
function_argcount(Lisp_Object function, int function_min_args_p)
{
	Lisp_Object orig_function = function;
	Lisp_Object arglist;

      retry:

	if (SYMBOLP(function))
		function = indirect_function(function, 1);

	if (SUBRP(function)) {
		/* Using return with the ?: operator tickles a DEC CC compiler bug. */
		if (function_min_args_p)
			return Fsubr_min_args(function);
		else
			return Fsubr_max_args(function);
	} else if (COMPILED_FUNCTIONP(function)) {
		arglist =
		    compiled_function_arglist(XCOMPILED_FUNCTION(function));
	} else if (CONSP(function)) {
		Lisp_Object funcar = XCAR(function);

		if (EQ(funcar, Qmacro)) {
			function = XCDR(function);
			goto retry;
		} else if (EQ(funcar, Qautoload)) {
			/* do_autoload GCPROs both arguments */
			do_autoload(function, orig_function);
			function = orig_function;
			goto retry;
		} else if (EQ(funcar, Qlambda)) {
			arglist = Fcar(XCDR(function));
		} else {
			goto invalid_function;
		}
	} else {
	      invalid_function:
		return signal_invalid_function_error(orig_function);
	}

	{
		int argcount = 0;

		EXTERNAL_LIST_LOOP_2(arg, arglist) {
			if (EQ(arg, Qand_optional)) {
				if (function_min_args_p)
					break;
			} else if (EQ(arg, Qand_rest)) {
				if (function_min_args_p)
					break;
				else
					return Qnil;
			} else {
				argcount++;
			}
		}

		return make_int(argcount);
	}
}

DEFUN("function-min-args", Ffunction_min_args, 1, 1, 0,	/*
Return the number of arguments a function may be called with.
The function may be any form that can be passed to `funcall',
any special form, or any macro.
*/
      (function))
{
	return function_argcount(function, 1);
}

DEFUN("function-max-args", Ffunction_max_args, 1, 1, 0,	/*
Return the number of arguments a function may be called with.
The function may be any form that can be passed to `funcall',
any special form, or any macro.
If the function takes an arbitrary number of arguments or is
a built-in special form, nil is returned.
*/
      (function))
{
	return function_argcount(function, 0);
}

DEFUN("apply", Fapply, 2, MANY, 0,	/*
Call FUNCTION with the remaining args, using the last arg as a list of args.
Thus, (apply '+ 1 2 '(3 4)) returns 10.
*/
      (int nargs, Lisp_Object * args))
{
	/* This function can GC */
	Lisp_Object fun = args[0];
	Lisp_Object spread_arg = args[nargs - 1];
	int numargs;
	int funcall_nargs;

	GET_EXTERNAL_LIST_LENGTH(spread_arg, numargs);

	if (numargs == 0)
		/* (apply foo 0 1 '()) */
		return Ffuncall(nargs - 1, args);
	else if (numargs == 1) {
		/* (apply foo 0 1 '(2)) */
		args[nargs - 1] = XCAR(spread_arg);
		return Ffuncall(nargs, args);
	}

	/* -1 for function, -1 for spread arg */
	numargs = nargs - 2 + numargs;
	/* +1 for function */
	funcall_nargs = 1 + numargs;

	if (SYMBOLP(fun))
		fun = indirect_function(fun, 0);

	if (SUBRP(fun)) {
		Lisp_Subr *subr = XSUBR(fun);
		int max_args = subr->max_args;

		if (numargs < subr->min_args
		    || (max_args >= 0 && max_args < numargs)) {
			/* Let funcall get the error */
		} else if (max_args > numargs) {
			/* Avoid having funcall cons up yet another new vector of arguments
			   by explicitly supplying nil's for optional values */
			funcall_nargs += (max_args - numargs);
		}
	} else if (UNBOUNDP(fun)) {
		/* Let funcall get the error */
		fun = args[0];
	}

	{
		REGISTER int i;
		Lisp_Object funcall_args[funcall_nargs];
		struct gcpro gcpro1;

		/* clean sweep */
		memset(funcall_args, 0, sizeof(Lisp_Object)*funcall_nargs);

		GCPROn(funcall_args, funcall_nargs);

		/* Copy in the unspread args */
		memcpy(funcall_args, args, (nargs - 1) * sizeof(Lisp_Object));
		/* Spread the last arg we got.  Its first element goes in
		   the slot that it used to occupy, hence this value of I.  */
		for (i = nargs - 1; !NILP(spread_arg);	/* i < 1 + numargs */
		     i++, spread_arg = XCDR(spread_arg)) {
			funcall_args[i] = XCAR(spread_arg);
		}
		/* Supply nil for optional args (to subrs) */
		for (; i < funcall_nargs; i++) {
			funcall_args[i] = Qnil;
		}

		RETURN_UNGCPRO(Ffuncall(funcall_nargs, funcall_args));
	}
}

/* Apply lambda list FUN to the NARGS evaluated arguments in ARGS and
   return the result of evaluation. */

static Lisp_Object
funcall_lambda(Lisp_Object fun, int nargs, Lisp_Object args[])
{
	/* This function can GC */
	Lisp_Object arglist, body, tail;
	int speccount = specpdl_depth();
	REGISTER int i = 0;

	tail = XCDR(fun);

	if (!CONSP(tail))
		goto invalid_function;

	arglist = XCAR(tail);
	body = XCDR(tail);

	{
		int optional = 0, rest = 0;

		EXTERNAL_LIST_LOOP_2(symbol, arglist) {
			if (!SYMBOLP(symbol))
				goto invalid_function;
			if (EQ(symbol, Qand_rest))
				rest = 1;
			else if (EQ(symbol, Qand_optional))
				optional = 1;
			else if (rest) {
				specbind(symbol, Flist(nargs - i, &args[i]));
				i = nargs;
			} else if (i < nargs)
				specbind(symbol, args[i++]);
			else if (!optional)
				goto wrong_number_of_arguments;
			else
				specbind(symbol, Qnil);
		}
	}

	if (i < nargs)
		goto wrong_number_of_arguments;

	return unbind_to(speccount, Fprogn(body));

      wrong_number_of_arguments:
	return signal_wrong_number_of_arguments_error(fun, nargs);

      invalid_function:
	return signal_invalid_function_error(fun);
}

/************************************************************************/
/*		     Run hook variables in various ways.		*/
/************************************************************************/
int changing_major_mode = 0;
Lisp_Object Qafter_change_major_mode_hook, Vafter_change_major_mode_hook;
Lisp_Object Qafter_change_before_major_mode_hook, Vafter_change_before_major_mode_hook;

Lisp_Object run_hook(Lisp_Object hook);

DEFUN("run-hooks", Frun_hooks, 1, MANY, 0,	/*
Run each hook in HOOKS.  Major mode functions use this.
Each argument should be a symbol, a hook variable.
These symbols are processed in the order specified.
If a hook symbol has a non-nil value, that value may be a function
or a list of functions to be called to run the hook.
If the value is a function, it is called with no arguments.
If it is a list, the elements are called, in order, with no arguments.

To make a hook variable buffer-local, use `make-local-hook',
not `make-local-variable'.
*/
      (int nargs, Lisp_Object * args))
{
	REGISTER int i;

	if (changing_major_mode) {
		Lisp_Object Qhook = Qafter_change_before_major_mode_hook;
		run_hook_with_args( 1, &Qhook,
				    RUN_HOOKS_TO_COMPLETION);
	}

	for (i = 0; i < nargs; i++)
		run_hook_with_args(1, args + i, RUN_HOOKS_TO_COMPLETION);

	if (changing_major_mode) {
		Lisp_Object Qhook = Qafter_change_major_mode_hook;
		changing_major_mode = 0;
		run_hook_with_args( 1, &Qhook,
				    RUN_HOOKS_TO_COMPLETION);
	}

	return Qnil;
}

DEFUN("run-hook-with-args", Frun_hook_with_args, 1, MANY, 0,	/*
Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  If HOOK has a non-nil
value, that value may be a function or a list of functions to be
called to run the hook.  If the value is a function, it is called with
the given arguments and its return value is returned.  If it is a list
of functions, those functions are called, in order,
with the given arguments ARGS.
It is best not to depend on the value returned by `run-hook-with-args',
as that may change.

To make a hook variable buffer-local, use `make-local-hook',
not `make-local-variable'.
*/
      (int nargs, Lisp_Object * args))
{
	return run_hook_with_args(nargs, args, RUN_HOOKS_TO_COMPLETION);
}

DEFUN("run-hook-with-args-until-success", Frun_hook_with_args_until_success, 1, MANY, 0,	/*
Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  Its value should
be a list of functions.  We call those functions, one by one,
passing arguments ARGS to each of them, until one of them
returns a non-nil value.  Then we return that value.
If all the functions return nil, we return nil.

To make a hook variable buffer-local, use `make-local-hook',
not `make-local-variable'.
*/
      (int nargs, Lisp_Object * args))
{
	return run_hook_with_args(nargs, args, RUN_HOOKS_UNTIL_SUCCESS);
}

DEFUN("run-hook-with-args-until-failure", Frun_hook_with_args_until_failure, 1, MANY, 0,	/*
Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  Its value should
be a list of functions.  We call those functions, one by one,
passing arguments ARGS to each of them, until one of them
returns nil.  Then we return nil.
If all the functions return non-nil, we return non-nil.

To make a hook variable buffer-local, use `make-local-hook',
not `make-local-variable'.
*/
      (int nargs, Lisp_Object * args))
{
	return run_hook_with_args(nargs, args, RUN_HOOKS_UNTIL_FAILURE);
}

Lisp_Object Qcurrent_running_hook, Vcurrent_running_hook;

/* ARGS[0] should be a hook symbol.
   Call each of the functions in the hook value, passing each of them
   as arguments all the rest of ARGS (all NARGS - 1 elements).
   COND specifies a condition to test after each call
   to decide whether to stop.
   The caller (or its caller, etc) must gcpro all of ARGS,
   except that it isn't necessary to gcpro ARGS[0].  */

Lisp_Object
run_hook_with_args_in_buffer(struct buffer * buf, int nargs, Lisp_Object * args,
			     enum run_hooks_condition cond)
{
	Lisp_Object sym, val, ret;

	if (!initialized || preparing_for_armageddon)
		/* We need to bail out of here pronto. */
		return Qnil;

	/* Whenever gc_in_progress is true, preparing_for_armageddon
	   will also be true unless something is really hosed. */
	assert(!gc_in_progress);

	sym = args[0];
	val = symbol_value_in_buffer(sym, make_buffer(buf));
	ret = (cond == RUN_HOOKS_UNTIL_FAILURE ? Qt : Qnil);

	if (UNBOUNDP(val) || NILP(val)) {
		return ret;
	} else if (!CONSP(val) || EQ(XCAR(val), Qlambda)) {
		Lisp_Object old_running_hook = Qnil;
		struct gcpro gcpro1;

		ret = Qnil;
		GCPRO1(old_running_hook);
		{
			args[0] = val;
			old_running_hook = symbol_value_in_buffer(
				Qcurrent_running_hook,
				make_buffer(buf));
			Fset(Qcurrent_running_hook,sym);
			ret = Ffuncall(nargs, args);
			Fset(Qcurrent_running_hook,old_running_hook);
		}
		UNGCPRO;
		return ret;
	} else {
		struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
		Lisp_Object globals = Qnil;
		Lisp_Object old_running_hook = Qnil;
		GCPRO4(sym, val, globals, old_running_hook);

		old_running_hook = symbol_value_in_buffer(
			Qcurrent_running_hook,
			make_buffer(buf));
		Fset(Qcurrent_running_hook,sym);

		for (; CONSP(val) && ((cond == RUN_HOOKS_TO_COMPLETION)
				      || (cond ==
					  RUN_HOOKS_UNTIL_SUCCESS ? NILP(ret)
					  : !NILP(ret))); val = XCDR(val)) {
			if (EQ(XCAR(val), Qt)) {
				/* t indicates this hook has a local binding;
				   it means to run the global binding too.  */
				globals = Fdefault_value(sym);

				if ((!CONSP(globals)
				     || EQ(XCAR(globals), Qlambda))
				    && !NILP(globals)) {
					args[0] = globals;
					ret = Ffuncall(nargs, args);
				} else {
					for (;
					     CONSP(globals)
					     &&
					     ((cond == RUN_HOOKS_TO_COMPLETION)
					      || (cond ==
						  RUN_HOOKS_UNTIL_SUCCESS ?
						  NILP(ret)
						  : !NILP(ret)));
					     globals = XCDR(globals)) {
						args[0] = XCAR(globals);
						/* In a global value, t should not occur.  If it does, we
						   must ignore it to avoid an endless loop.  */
						if (!EQ(args[0], Qt))
							ret =
							    Ffuncall(nargs,
								     args);
					}
				}
			} else {
				args[0] = XCAR(val);
				ret = Ffuncall(nargs, args);
			}
		}

		Fset(Qcurrent_running_hook,old_running_hook);
		UNGCPRO;
		return ret;
	}
}

Lisp_Object
run_hook_with_args(int nargs, Lisp_Object * args, enum run_hooks_condition cond)
{
	return run_hook_with_args_in_buffer(current_buffer, nargs, args, cond);
}

#if 0

/* From FSF 19.30, not currently used */

/* Run a hook symbol ARGS[0], but use FUNLIST instead of the actual
   present value of that symbol.
   Call each element of FUNLIST,
   passing each of them the rest of ARGS.
   The caller (or its caller, etc) must gcpro all of ARGS,
   except that it isn't necessary to gcpro ARGS[0].  */

Lisp_Object
run_hook_list_with_args(Lisp_Object funlist, int nargs, Lisp_Object * args)
{
	Lisp_Object sym = args[0];
	Lisp_Object val;
	struct gcpro gcpro1, gcpro2;

	GCPRO2(sym, val);

	for (val = funlist; CONSP(val); val = XCDR(val)) {
		if (EQ(XCAR(val), Qt)) {
			/* t indicates this hook has a local binding;
			   it means to run the global binding too.  */
			Lisp_Object globals;

			for (globals = Fdefault_value(sym);
			     CONSP(globals); globals = XCDR(globals)) {
				args[0] = XCAR(globals);
				/* In a global value, t should not occur.  If it does, we
				   must ignore it to avoid an endless loop.  */
				if (!EQ(args[0], Qt))
					Ffuncall(nargs, args);
			}
		} else {
			args[0] = XCAR(val);
			Ffuncall(nargs, args);
		}
	}
	UNGCPRO;
	return Qnil;
}

#endif				/* 0 */

void va_run_hook_with_args(Lisp_Object hook_var, int nargs, ...)
{
	/* This function can GC */
	struct gcpro gcpro1;
	int i;
	va_list vargs;
	Lisp_Object funcall_args[1+nargs];

	va_start(vargs, nargs);
	funcall_args[0] = hook_var;
	for (i = 0; i < nargs; i++) {
		funcall_args[i + 1] = va_arg(vargs, Lisp_Object);
	}
	va_end(vargs);

	GCPROn(funcall_args, 1+nargs);
	run_hook_with_args(nargs + 1, funcall_args, RUN_HOOKS_TO_COMPLETION);
	UNGCPRO;
}

void
va_run_hook_with_args_in_buffer(struct buffer *buf, Lisp_Object hook_var,
				int nargs, ...)
{
	/* This function can GC */
	struct gcpro gcpro1;
	int i;
	va_list vargs;
	Lisp_Object funcall_args[1+nargs];

	va_start(vargs, nargs);
	funcall_args[0] = hook_var;
	for (i = 0; i < nargs; i++) {
		funcall_args[i + 1] = va_arg(vargs, Lisp_Object);
	}
	va_end(vargs);

	GCPROn(funcall_args, 1+nargs);
	run_hook_with_args_in_buffer(buf, nargs + 1, funcall_args,
				     RUN_HOOKS_TO_COMPLETION);
	UNGCPRO;
}

Lisp_Object run_hook(Lisp_Object hook)
{
	Frun_hooks(1, &hook);
	return Qnil;
}

/************************************************************************/
/*		    Front-ends to eval, funcall, apply			*/
/************************************************************************/

/* Apply fn to arg */
Lisp_Object apply1(Lisp_Object fn, Lisp_Object arg)
{
	/* This function can GC */
	struct gcpro gcpro1;
	Lisp_Object args[2];

	if (NILP(arg)) {
		return Ffuncall(1, &fn);
	}
	args[0] = fn;
	args[1] = arg;
	GCPROn(args, countof(args));
	RETURN_UNGCPRO(Fapply(2, args));
}

/* Call function fn on no arguments */
Lisp_Object call0(Lisp_Object fn)
{
	/* This function can GC */
	struct gcpro gcpro1;

	GCPRO1(fn);
	RETURN_UNGCPRO(Ffuncall(1, &fn));
}

/* Call function fn with argument arg0 */
Lisp_Object call1(Lisp_Object fn, Lisp_Object arg0)
{
	/* This function can GC */
	struct gcpro gcpro1;
	Lisp_Object args[2] = {fn, arg0};

	GCPROn(args, countof(args));
	RETURN_UNGCPRO(Ffuncall(2, args));
}

/* Call function fn with arguments arg0, arg1 */
Lisp_Object call2(Lisp_Object fn, Lisp_Object arg0, Lisp_Object arg1)
{
	/* This function can GC */
	struct gcpro gcpro1;
	Lisp_Object args[3] = {fn, arg0, arg1};

	GCPROn(args, countof(args));
	RETURN_UNGCPRO(Ffuncall(3, args));
}

/* Call function fn with arguments arg0, arg1, arg2 */
Lisp_Object
call3(Lisp_Object fn, Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2)
{
	/* This function can GC */
	struct gcpro gcpro1;
	Lisp_Object args[4] = {fn, arg0, arg1, arg2};

	GCPROn(args, countof(args));
	RETURN_UNGCPRO(Ffuncall(4, args));
}

/* Call function fn with arguments arg0, arg1, arg2, arg3 */
Lisp_Object
call4(Lisp_Object fn,
      Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2, Lisp_Object arg3)
{
	/* This function can GC */
	struct gcpro gcpro1;
	Lisp_Object args[5] = {fn, arg0, arg1, arg2, arg3};

	GCPROn(args, countof(args));
	RETURN_UNGCPRO(Ffuncall(5, args));
}

/* Call function fn with arguments arg0, arg1, arg2, arg3, arg4 */
Lisp_Object
call5(Lisp_Object fn,
      Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2,
      Lisp_Object arg3, Lisp_Object arg4)
{
	/* This function can GC */
	struct gcpro gcpro1;
	Lisp_Object args[6] = {fn, arg0, arg1, arg2, arg3, arg4};

	GCPROn(args, countof(args));
	RETURN_UNGCPRO(Ffuncall(6, args));
}

Lisp_Object
call6(Lisp_Object fn,
      Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2,
      Lisp_Object arg3, Lisp_Object arg4, Lisp_Object arg5)
{
	/* This function can GC */
	struct gcpro gcpro1;
	Lisp_Object args[7] = {fn, arg0, arg1, arg2, arg3, arg4, arg5};

	GCPROn(args, countof(args));
	RETURN_UNGCPRO(Ffuncall(7, args));
}

Lisp_Object
call7(Lisp_Object fn,
      Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2,
      Lisp_Object arg3, Lisp_Object arg4, Lisp_Object arg5, Lisp_Object arg6)
{
	/* This function can GC */
	struct gcpro gcpro1;
	Lisp_Object args[8] = {fn, arg0, arg1, arg2, arg3, arg4, arg5, arg6};

	GCPROn(args, countof(args));
	RETURN_UNGCPRO(Ffuncall(8, args));
}

Lisp_Object
call8(Lisp_Object fn,
      Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2,
      Lisp_Object arg3, Lisp_Object arg4, Lisp_Object arg5,
      Lisp_Object arg6, Lisp_Object arg7)
{
	/* This function can GC */
	struct gcpro gcpro1;
	Lisp_Object args[9] = {
		fn, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7};

	GCPROn(args, countof(args));
	RETURN_UNGCPRO(Ffuncall(9, args));
}

Lisp_Object call0_in_buffer(struct buffer *buf, Lisp_Object fn)
{
	if (current_buffer == buf) {
		return call0(fn);
	} else {
		Lisp_Object val;
		int speccount = specpdl_depth();
		record_unwind_protect(Fset_buffer, Fcurrent_buffer());
		set_buffer_internal(buf);
		val = call0(fn);
		unbind_to(speccount, Qnil);
		return val;
	}
}

Lisp_Object
call1_in_buffer(struct buffer * buf, Lisp_Object fn, Lisp_Object arg0)
{
	if (current_buffer == buf) {
		return call1(fn, arg0);
	} else {
		Lisp_Object val;
		int speccount = specpdl_depth();
		record_unwind_protect(Fset_buffer, Fcurrent_buffer());
		set_buffer_internal(buf);
		val = call1(fn, arg0);
		unbind_to(speccount, Qnil);
		return val;
	}
}

Lisp_Object
call2_in_buffer(struct buffer * buf, Lisp_Object fn,
		Lisp_Object arg0, Lisp_Object arg1)
{
	if (current_buffer == buf) {
		return call2(fn, arg0, arg1);
	} else {
		Lisp_Object val;
		int speccount = specpdl_depth();
		record_unwind_protect(Fset_buffer, Fcurrent_buffer());
		set_buffer_internal(buf);
		val = call2(fn, arg0, arg1);
		unbind_to(speccount, Qnil);
		return val;
	}
}

Lisp_Object
call3_in_buffer(struct buffer * buf, Lisp_Object fn,
		Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2)
{
	if (current_buffer == buf) {
		return call3(fn, arg0, arg1, arg2);
	} else {
		Lisp_Object val;
		int speccount = specpdl_depth();
		record_unwind_protect(Fset_buffer, Fcurrent_buffer());
		set_buffer_internal(buf);
		val = call3(fn, arg0, arg1, arg2);
		unbind_to(speccount, Qnil);
		return val;
	}
}

Lisp_Object
call4_in_buffer(struct buffer * buf, Lisp_Object fn,
		Lisp_Object arg0, Lisp_Object arg1, Lisp_Object arg2,
		Lisp_Object arg3)
{
	if (current_buffer == buf) {
		return call4(fn, arg0, arg1, arg2, arg3);
	} else {
		Lisp_Object val;
		int speccount = specpdl_depth();
		record_unwind_protect(Fset_buffer, Fcurrent_buffer());
		set_buffer_internal(buf);
		val = call4(fn, arg0, arg1, arg2, arg3);
		unbind_to(speccount, Qnil);
		return val;
	}
}

Lisp_Object eval_in_buffer(struct buffer * buf, Lisp_Object form)
{
	if (current_buffer == buf) {
		return Feval(form);
	} else {
		Lisp_Object val;
		int speccount = specpdl_depth();
		record_unwind_protect(Fset_buffer, Fcurrent_buffer());
		set_buffer_internal(buf);
		val = Feval(form);
		unbind_to(speccount, Qnil);
		return val;
	}
}

/************************************************************************/
/*	   Error-catching front-ends to eval, funcall, apply		*/
/************************************************************************/

/* Call function fn on no arguments, with condition handler */
Lisp_Object call0_with_handler(Lisp_Object handler, Lisp_Object fn)
{
	/* This function can GC */
	struct gcpro gcpro1;
	Lisp_Object args[2] = {handler, fn};

	GCPROn(args, countof(args));
	RETURN_UNGCPRO(Fcall_with_condition_handler(2, args));
}

/* Call function fn with argument arg0, with condition handler */
Lisp_Object
call1_with_handler(Lisp_Object handler, Lisp_Object fn, Lisp_Object arg0)
{
	/* This function can GC */
	struct gcpro gcpro1;
	Lisp_Object args[3] = {handler, fn, arg0};

	GCPROn(args, countof(args));
	RETURN_UNGCPRO(Fcall_with_condition_handler(3, args));
}

/* The following functions provide you with error-trapping versions
   of the various front-ends above.  They take an additional
   "warning_string" argument; if non-zero, a warning with this
   string and the actual error that occurred will be displayed
   in the *Warnings* buffer if an error occurs.  In all cases,
   QUIT is inhibited while these functions are running, and if
   an error occurs, Qunbound is returned instead of the normal
   return value.
   */

/* #### This stuff needs to catch throws as well.  We need to
   improve internal_catch() so it can take a "catch anything"
   argument similar to Qt or Qerror for condition_case_1(). */

static Lisp_Object caught_a_squirmer(Lisp_Object errordata, Lisp_Object arg)
{
	if (!NILP(errordata)) {
		Lisp_Object args[2];

		if (!NILP(arg)) {
			char *str = (char *)get_opaque_ptr(arg);
			args[0] = build_string(str);
		} else
			args[0] = build_string("error");
		/* #### This should call
		   (with-output-to-string (display-error errordata))
		   but that stuff is all in Lisp currently. */
		args[1] = errordata;
		warn_when_safe_lispobj
		    (Qerror, Qwarning,
		     emacs_doprnt_string_lisp((const Bufbyte *)"%s: %s",
					      Qnil, -1, 2, args));
	}
	return Qunbound;
}

static Lisp_Object
allow_quit_caught_a_squirmer(Lisp_Object errordata, Lisp_Object arg)
{
	if (CONSP(errordata) && EQ(XCAR(errordata), Qquit))
		return Fsignal(Qquit, XCDR(errordata));
	return caught_a_squirmer(errordata, arg);
}

static Lisp_Object
safe_run_hook_caught_a_squirmer(Lisp_Object errordata, Lisp_Object arg)
{
	Lisp_Object hook = Fcar(arg);
	arg = Fcdr(arg);
	/* Clear out the hook. */
	Fset(hook, Qnil);
	return caught_a_squirmer(errordata, arg);
}

static Lisp_Object
allow_quit_safe_run_hook_caught_a_squirmer(Lisp_Object errordata,
					   Lisp_Object arg)
{
	Lisp_Object hook = Fcar(arg);
	arg = Fcdr(arg);
	if (!CONSP(errordata) || !EQ(XCAR(errordata), Qquit))
		/* Clear out the hook. */
		Fset(hook, Qnil);
	return allow_quit_caught_a_squirmer(errordata, arg);
}

static Lisp_Object catch_them_squirmers_eval_in_buffer(Lisp_Object cons)
{
	return eval_in_buffer(XBUFFER(XCAR(cons)), XCDR(cons));
}

Lisp_Object
eval_in_buffer_trapping_errors(char *warning_string,
			       struct buffer *buf, Lisp_Object form)
{
	int speccount = specpdl_depth();
	Lisp_Object tem;
	Lisp_Object buffer;
	Lisp_Object cons;
	Lisp_Object opaque;
	struct gcpro gcpro1, gcpro2;

	XSETBUFFER(buffer, buf);

	specbind(Qinhibit_quit, Qt);
	/* gc_currently_forbidden = 1; Currently no reason to do this; */

	cons = noseeum_cons(buffer, form);
	opaque = warning_string
		? make_opaque_ptr(warning_string)
		: Qnil;
	GCPRO2(cons, opaque);
	/* Qerror not Qt, so you can get a backtrace */
	tem = condition_case_1(Qerror,
			       catch_them_squirmers_eval_in_buffer, cons,
			       caught_a_squirmer, opaque);
	free_cons(XCONS(cons));
	if (OPAQUE_PTRP(opaque))
		free_opaque_ptr(opaque);
	UNGCPRO;

	/* gc_currently_forbidden = 0; */
	return unbind_to(speccount, tem);
}

static Lisp_Object catch_them_squirmers_run_hook(Lisp_Object hook_symbol)
{
	/* This function can GC */
	run_hook(hook_symbol);
	return Qnil;
}

Lisp_Object
run_hook_trapping_errors(char *warning_string, Lisp_Object hook_symbol)
{
	int speccount;
	Lisp_Object tem;
	Lisp_Object opaque;
	struct gcpro gcpro1;

	if (!initialized || preparing_for_armageddon)
		return Qnil;
	tem = find_symbol_value(hook_symbol);
	if (NILP(tem) || UNBOUNDP(tem))
		return Qnil;

	speccount = specpdl_depth();
	specbind(Qinhibit_quit, Qt);

	opaque = warning_string
		? make_opaque_ptr((void*)warning_string)
		: Qnil;
	GCPRO1(opaque);
	/* Qerror not Qt, so you can get a backtrace */
	tem = condition_case_1(Qerror,
			       catch_them_squirmers_run_hook, hook_symbol,
			       caught_a_squirmer, opaque);
	if (OPAQUE_PTRP(opaque))
		free_opaque_ptr(opaque);
	UNGCPRO;

	return unbind_to(speccount, tem);
}

/* Same as run_hook_trapping_errors() but also set the hook to nil
   if an error occurs. */

Lisp_Object
safe_run_hook_trapping_errors(char *warning_string,
			      Lisp_Object hook_symbol, int allow_quit)
{
	int speccount = specpdl_depth();
	Lisp_Object tem;
	Lisp_Object cons = Qnil;
	struct gcpro gcpro1;

	if (!initialized || preparing_for_armageddon)
		return Qnil;
	tem = find_symbol_value(hook_symbol);
	if (NILP(tem) || UNBOUNDP(tem))
		return Qnil;

	if (!allow_quit)
		specbind(Qinhibit_quit, Qt);

	cons = noseeum_cons(hook_symbol,
			    warning_string
			    ? make_opaque_ptr((void*)warning_string)
			    : Qnil);
	GCPRO1(cons);
	/* Qerror not Qt, so you can get a backtrace */
	tem = condition_case_1(Qerror,
			       catch_them_squirmers_run_hook,
			       hook_symbol,
			       allow_quit ?
			       allow_quit_safe_run_hook_caught_a_squirmer :
			       safe_run_hook_caught_a_squirmer, cons);
	if (OPAQUE_PTRP(XCDR(cons)))
		free_opaque_ptr(XCDR(cons));
	free_cons(XCONS(cons));
	UNGCPRO;

	return unbind_to(speccount, tem);
}

static Lisp_Object catch_them_squirmers_call0(Lisp_Object function)
{
	/* This function can GC */
	return call0(function);
}

Lisp_Object
call0_trapping_errors(char *warning_string, Lisp_Object function)
{
	int speccount;
	Lisp_Object tem;
	Lisp_Object opaque = Qnil;
	struct gcpro gcpro1, gcpro2;

	if (SYMBOLP(function)) {
		tem = XSYMBOL(function)->function;
		if (NILP(tem) || UNBOUNDP(tem))
			return Qnil;
	}

	GCPRO2(opaque, function);
	speccount = specpdl_depth();
	specbind(Qinhibit_quit, Qt);
	/* gc_currently_forbidden = 1; Currently no reason to do this; */

	opaque = warning_string
		? make_opaque_ptr((void *)warning_string)
		: Qnil;
	/* Qerror not Qt, so you can get a backtrace */
	tem = condition_case_1(Qerror,
			       catch_them_squirmers_call0, function,
			       caught_a_squirmer, opaque);
	if (OPAQUE_PTRP(opaque))
		free_opaque_ptr(opaque);
	UNGCPRO;

	/* gc_currently_forbidden = 0; */
	return unbind_to(speccount, tem);
}

static Lisp_Object catch_them_squirmers_call1(Lisp_Object cons)
{
	/* This function can GC */
	return call1(XCAR(cons), XCDR(cons));
}

static Lisp_Object catch_them_squirmers_call2(Lisp_Object cons)
{
	/* This function can GC */
	return call2(XCAR(cons), XCAR(XCDR(cons)), XCAR(XCDR(XCDR(cons))));
}

static Lisp_Object catch_them_squirmers_call3(Lisp_Object cons)
{
	/* This function can GC */
	return call3(XCAR(cons), XCAR(XCDR(cons)), XCAR(XCDR(XCDR(cons))), XCAR(XCDR(XCDR(XCDR(cons)))));
}

Lisp_Object
call1_trapping_errors(char *warning_string, Lisp_Object function,
		      Lisp_Object object)
{
	int speccount = specpdl_depth();
	Lisp_Object tem;
	Lisp_Object cons = Qnil;
	Lisp_Object opaque = Qnil;
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

	if (SYMBOLP(function)) {
		tem = XSYMBOL(function)->function;
		if (NILP(tem) || UNBOUNDP(tem))
			return Qnil;
	}

	GCPRO4(cons, opaque, function, object);

	specbind(Qinhibit_quit, Qt);
	/* gc_currently_forbidden = 1; Currently no reason to do this; */

	cons = noseeum_cons(function, object);
	opaque = warning_string
		? make_opaque_ptr((void *)warning_string)
		: Qnil;
	/* Qerror not Qt, so you can get a backtrace */
	tem = condition_case_1(Qerror,
			       catch_them_squirmers_call1, cons,
			       caught_a_squirmer, opaque);
	if (OPAQUE_PTRP(opaque))
		free_opaque_ptr(opaque);
	free_cons(XCONS(cons));
	UNGCPRO;

	/* gc_currently_forbidden = 0; */
	return unbind_to(speccount, tem);
}

Lisp_Object
call2_trapping_errors(char *warning_string, Lisp_Object function,
		      Lisp_Object object1, Lisp_Object object2)
{
	int speccount = specpdl_depth();
	Lisp_Object tem;
	Lisp_Object cons = Qnil;
	Lisp_Object opaque = Qnil;
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5;

	if (SYMBOLP(function)) {
		tem = XSYMBOL(function)->function;
		if (NILP(tem) || UNBOUNDP(tem))
			return Qnil;
	}

	GCPRO5(cons, opaque, function, object1, object2);
	specbind(Qinhibit_quit, Qt);
	/* gc_currently_forbidden = 1; Currently no reason to do this; */

	cons = list3(function, object1, object2);
	opaque = warning_string
		? make_opaque_ptr((void *)warning_string)
		: Qnil;
	/* Qerror not Qt, so you can get a backtrace */
	tem = condition_case_1(Qerror,
			       catch_them_squirmers_call2, cons,
			       caught_a_squirmer, opaque);
	if (OPAQUE_PTRP(opaque))
		free_opaque_ptr(opaque);
	free_list(cons);
	UNGCPRO;

	/* gc_currently_forbidden = 0; */
	return unbind_to(speccount, tem);
}

Lisp_Object
call3_trapping_errors(char *warning_string, Lisp_Object function,
		      Lisp_Object object1, Lisp_Object object2, Lisp_Object object3)
{
	int speccount = specpdl_depth();
	Lisp_Object tem;
	Lisp_Object cons = Qnil;
	Lisp_Object opaque = Qnil;
	struct gcpro gcpro1, gcpro2, gcpro3, gcpro4, gcpro5, gcpro6;

	if (SYMBOLP(function)) {
		tem = XSYMBOL(function)->function;
		if (NILP(tem) || UNBOUNDP(tem))
			return Qnil;
	}

	GCPRO6(cons, opaque, function, object1, object2, object3);
	specbind(Qinhibit_quit, Qt);
	/* gc_currently_forbidden = 1; Currently no reason to do this; */

	cons = list4(function, object1, object2, object3);
	opaque = warning_string
		? make_opaque_ptr((void *)warning_string)
		: Qnil;
	/* Qerror not Qt, so you can get a backtrace */
	tem = condition_case_1(Qerror,
			       catch_them_squirmers_call3, cons,
			       caught_a_squirmer, opaque);
	if (OPAQUE_PTRP(opaque))
		free_opaque_ptr(opaque);
	free_list(cons);
	UNGCPRO;

	/* gc_currently_forbidden = 0; */
	return unbind_to(speccount, tem);
}

/************************************************************************/
/*		       The special binding stack			*/
/* Most C code should simply use specbind() and unbind_to().		*/
/* When performance is critical, use the macros in backtrace.h.		*/
/************************************************************************/

#define min_max_specpdl_size 400

void grow_specpdl(EMACS_INT reserved)
{
	EMACS_INT size_needed = specpdl_depth() + reserved;
	if (specpdl_size == 0)
		specpdl_size = 1;
	if (size_needed >= max_specpdl_size) {
		if (max_specpdl_size < min_max_specpdl_size)
			max_specpdl_size = min_max_specpdl_size;
		if (size_needed >= max_specpdl_size) {
			if (!NILP(Vdebug_on_error) || !NILP(Vdebug_on_signal))
				/* Leave room for some specpdl in the debugger.  */
				max_specpdl_size = size_needed + 100;
			continuable_error
			    ("Variable binding depth exceeds max-specpdl-size");
		}
	}
	while (specpdl_size < size_needed) {
		specpdl_size *= 2;
		if (specpdl_size > max_specpdl_size)
			specpdl_size = max_specpdl_size;
	}
	XREALLOC_ARRAY(specpdl, struct specbinding, specpdl_size);
	specpdl_ptr = specpdl + specpdl_depth();
}

/* Handle unbinding buffer-local variables */
static Lisp_Object specbind_unwind_local(Lisp_Object ovalue)
{
	Lisp_Object current = Fcurrent_buffer();
	Lisp_Object symbol = specpdl_ptr->symbol;
	Lisp_Cons *victim = XCONS(ovalue);
	Lisp_Object buf = emacs_get_buffer(victim->car, 0);
	ovalue = victim->cdr;

	free_cons(victim);

	if (NILP(buf)) {
		/* Deleted buffer -- do nothing */
	} else if (symbol_value_buffer_local_info(symbol, XBUFFER(buf)) == 0) {
		/* Was buffer-local when binding was made, now no longer is.
		 *  (kill-local-variable can do this.)
		 * Do nothing in this case.
		 */
	} else if (EQ(buf, current))
		Fset(symbol, ovalue);
	else {
		/* Urk! Somebody switched buffers */
		struct gcpro gcpro1;
		GCPRO1(current);
		Fset_buffer(buf);
		Fset(symbol, ovalue);
		Fset_buffer(current);
		UNGCPRO;
	}
	return symbol;
}

static Lisp_Object specbind_unwind_wasnt_local(Lisp_Object buffer)
{
	Lisp_Object current = Fcurrent_buffer();
	Lisp_Object symbol = specpdl_ptr->symbol;

	buffer = emacs_get_buffer(buffer, 0);
	if (NILP(buffer)) {
		/* Deleted buffer -- do nothing */
	} else if (symbol_value_buffer_local_info(symbol, XBUFFER(buffer)) == 0) {
		/* Was buffer-local when binding was made, now no longer is.
		 *  (kill-local-variable can do this.)
		 * Do nothing in this case.
		 */
	} else if (EQ(buffer, current))
		Fkill_local_variable(symbol);
	else {
		/* Urk! Somebody switched buffers */
		struct gcpro gcpro1;
		GCPRO1(current);
		Fset_buffer(buffer);
		Fkill_local_variable(symbol);
		Fset_buffer(current);
		UNGCPRO;
	}
	return symbol;
}

void specbind(Lisp_Object symbol, Lisp_Object value)
{
	SPECBIND(symbol, value);
}

void specbind_magic(Lisp_Object symbol, Lisp_Object value)
{
	int buffer_local =
	    symbol_value_buffer_local_info(symbol, current_buffer);

	if (buffer_local == 0) {
		specpdl_ptr->old_value = find_symbol_value(symbol);
		specpdl_ptr->func = 0;	/* Handled specially by unbind_to */
	} else if (buffer_local > 0) {
		/* Already buffer-local */
		specpdl_ptr->old_value = noseeum_cons(Fcurrent_buffer(),
						      find_symbol_value
						      (symbol));
		specpdl_ptr->func = specbind_unwind_local;
	} else {
		/* About to become buffer-local */
		specpdl_ptr->old_value = Fcurrent_buffer();
		specpdl_ptr->func = specbind_unwind_wasnt_local;
	}

	specpdl_ptr->symbol = symbol;
	specpdl_ptr++;
	specpdl_depth_counter++;

	Fset(symbol, value);
}

/* Note: As long as the unwind-protect exists, its arg is automatically
   GCPRO'd. */

void
record_unwind_protect(Lisp_Object(*function) (Lisp_Object arg), Lisp_Object arg)
{
	SPECPDL_RESERVE(1);
	specpdl_ptr->func = function;
	specpdl_ptr->symbol = Qnil;
	specpdl_ptr->old_value = arg;
	specpdl_ptr++;
	specpdl_depth_counter++;
}

extern int check_sigio(void);

/* Unwind the stack till specpdl_depth() == COUNT.
   VALUE is not used, except that, purely as a convenience to the
   caller, it is protected from garbage-protection. */
Lisp_Object unbind_to(int count, Lisp_Object value)
{
	UNBIND_TO_GCPRO(count, value);
	return value;
}

/* Don't call this directly.
   Only for use by UNBIND_TO* macros in backtrace.h */
void unbind_to_hairy(int count)
{
	int quitf;

	++specpdl_ptr;
	++specpdl_depth_counter;

	check_quit();		/* make Vquit_flag accurate */
	quitf = !NILP(Vquit_flag);
	Vquit_flag = Qnil;

	while (specpdl_depth_counter != count) {
		--specpdl_ptr;
		--specpdl_depth_counter;

		if (specpdl_ptr->func != 0)
			/* An unwind-protect */
			(*specpdl_ptr->func) (specpdl_ptr->old_value);
		else {
			/* We checked symbol for validity when we specbound it,
			   so only need to call Fset if symbol has magic value.  */
			Lisp_Symbol *sym = XSYMBOL(specpdl_ptr->symbol);
			if (!SYMBOL_VALUE_MAGIC_P(sym->value))
				sym->value = specpdl_ptr->old_value;
			else
				Fset(specpdl_ptr->symbol,
				     specpdl_ptr->old_value);
		}

#if 0				/* martin */
#ifndef EXCEEDINGLY_QUESTIONABLE_CODE
		/* There should never be anything here for us to remove.
		   If so, it indicates a logic error in Emacs.  Catches
		   should get removed when a throw or signal occurs, or
		   when a catch or condition-case exits normally.  But
		   it's too dangerous to just remove this code. --ben */

		/* Furthermore, this code is not in FSFmacs!!!
		   Braino on mly's part? */
		/* If we're unwound past the pdlcount of a catch frame,
		   that catch can't possibly still be valid. */
		while (catchlist && catchlist->pdlcount > specpdl_depth_counter) {
			catchlist = catchlist->next;
			/* Don't mess with gcprolist, backtrace_list here */
		}
#endif
#endif
	}
	if (quitf)
		Vquit_flag = Qt;
}

/* Get the value of symbol's global binding, even if that binding is
   not now dynamically visible.  May return Qunbound or magic values. */

Lisp_Object top_level_value(Lisp_Object symbol)
{
	REGISTER struct specbinding *ptr = specpdl;

	CHECK_SYMBOL(symbol);
	for (; ptr != specpdl_ptr; ptr++) {
		if (EQ(ptr->symbol, symbol))
			return ptr->old_value;
	}
	return XSYMBOL(symbol)->value;
}

#if 0

Lisp_Object top_level_set(Lisp_Object symbol, Lisp_Object newval)
{
	REGISTER struct specbinding *ptr = specpdl;

	CHECK_SYMBOL(symbol);
	for (; ptr != specpdl_ptr; ptr++) {
		if (EQ(ptr->symbol, symbol)) {
			ptr->old_value = newval;
			return newval;
		}
	}
	return Fset(symbol, newval);
}

#endif				/* 0 */

/************************************************************************/
/*			      Backtraces				*/
/************************************************************************/

DEFUN("backtrace-debug", Fbacktrace_debug, 2, 2, 0,	/*
Set the debug-on-exit flag of eval frame LEVEL levels down to FLAG.
The debugger is entered when that frame exits, if the flag is non-nil.
*/
      (level, flag))
{
	REGISTER struct backtrace *backlist = backtrace_list;
	REGISTER int i;

	CHECK_INT(level);

	for (i = 0; backlist && i < XINT(level); i++) {
		backlist = backlist->next;
	}

	if (backlist)
		backlist->debug_on_exit = !NILP(flag);

	return flag;
}

static void backtrace_specials(int speccount, int speclimit, Lisp_Object stream)
{
	int printing_bindings = 0;

	for (; speccount > speclimit; speccount--) {
		if (specpdl[speccount - 1].func == 0
		    || specpdl[speccount - 1].func == specbind_unwind_local
		    || specpdl[speccount - 1].func ==
		    specbind_unwind_wasnt_local) {
			write_c_string(((!printing_bindings) ? "  # bind (" :
					" "), stream);
			Fprin1(specpdl[speccount - 1].symbol, stream);
			printing_bindings = 1;
		} else {
			if (printing_bindings)
				write_c_string(")\n", stream);
			write_c_string("  # (unwind-protect ...)\n", stream);
			printing_bindings = 0;
		}
	}
	if (printing_bindings)
		write_c_string(")\n", stream);
}

DEFUN("backtrace", Fbacktrace, 0, 2, "",	/*
Print a trace of Lisp function calls currently active.
Optional arg STREAM specifies the output stream to send the backtrace to,
and defaults to the value of `standard-output'.
Optional second arg DETAILED non-nil means show places where currently
active variable bindings, catches, condition-cases, and
unwind-protects, as well as function calls, were made.
*/
      (stream, detailed))
{
	/* This function can GC */
	struct backtrace *backlist = backtrace_list;
	struct catchtag *catches = catchlist;
	int speccount = specpdl_depth();

	int old_nl = print_escape_newlines;
	int old_pr = print_readably;
	Lisp_Object old_level = Vprint_level;
	Lisp_Object oiq = Vinhibit_quit;
	struct gcpro gcpro1, gcpro2;

	/* We can't allow quits in here because that could cause the values
	   of print_readably and print_escape_newlines to get screwed up.
	   Normally we would use a record_unwind_protect but that would
	   screw up the functioning of this function. */
	Vinhibit_quit = Qt;

	entering_debugger = 0;

	Vprint_level = make_int(3);
	print_readably = 0;
	print_escape_newlines = 1;

	GCPRO2(stream, old_level);

	if (NILP(stream))
		stream = Vstandard_output;
	if (!noninteractive && (NILP(stream) || EQ(stream, Qt)))
		stream = Fselected_frame(Qnil);

	for (;;) {
		if (!NILP(detailed) && catches && catches->backlist == backlist) {
			int catchpdl = catches->pdlcount;
			if (speccount > catchpdl
			    && specpdl[catchpdl].func == condition_case_unwind)
				/* This is a condition-case catchpoint */
				catchpdl = catchpdl + 1;

			backtrace_specials(speccount, catchpdl, stream);

			speccount = catches->pdlcount;
			if (catchpdl == speccount) {
				write_c_string("  # (catch ", stream);
				Fprin1(catches->tag, stream);
				write_c_string(" ...)\n", stream);
			} else {
				write_c_string("  # (condition-case ... . ",
					       stream);
				Fprin1(Fcdr(Fcar(catches->tag)), stream);
				write_c_string(")\n", stream);
			}
			catches = catches->next;
		} else if (!backlist)
			break;
		else {
			if (!NILP(detailed) && backlist->pdlcount < speccount) {
				backtrace_specials(speccount,
						   backlist->pdlcount, stream);
				speccount = backlist->pdlcount;
			}
			write_c_string(((backlist->
					 debug_on_exit) ? "* " : "  "), stream);
			if (backlist->nargs == UNEVALLED) {
				Fprin1(Fcons
				       (*backlist->function, *backlist->args),
				       stream);
				write_c_string("\n", stream);	/* from FSFmacs 19.30 */
			} else {
				Lisp_Object tem = *backlist->function;
				Fprin1(tem, stream);	/* This can QUIT */
				write_c_string("(", stream);
				if (backlist->nargs == MANY) {
					int i;
					Lisp_Object tail = Qnil;
					struct gcpro ngcpro1;

					NGCPRO1(tail);
					for (tail = *backlist->args, i = 0;
					     !NILP(tail);
					     tail = Fcdr(tail), i++) {
						if (i != 0)
							write_c_string(" ",
								       stream);
						Fprin1(Fcar(tail), stream);
					}
					NUNGCPRO;
				} else {
					int i;
					for (i = 0; i < backlist->nargs; i++) {
						if (!i && EQ(tem, Qbyte_code)) {
							write_c_string
							    ("\"...\"", stream);
							continue;
						}
						if (i != 0)
							write_c_string(" ",
								       stream);
						Fprin1(backlist->args[i],
						       stream);
					}
				}
				write_c_string(")\n", stream);
			}
			backlist = backlist->next;
		}
	}
	Vprint_level = old_level;
	print_readably = old_pr;
	print_escape_newlines = old_nl;
	UNGCPRO;
	Vinhibit_quit = oiq;
	return Qnil;
}

DEFUN("backtrace-frame", Fbacktrace_frame, 1, 1, 0,	/*
Return the function and arguments NFRAMES up from current execution point.
If that frame has not evaluated the arguments yet (or is a special form),
the value is (nil FUNCTION ARG-FORMS...).
If that frame has evaluated its arguments and called its function already,
the value is (t FUNCTION ARG-VALUES...).
A &rest arg is represented as the tail of the list ARG-VALUES.
FUNCTION is whatever was supplied as car of evaluated list,
or a lambda expression for macro calls.
If NFRAMES is more than the number of frames, the value is nil.
*/
      (nframes))
{
	REGISTER struct backtrace *backlist = backtrace_list;
	REGISTER int i;
	Lisp_Object tem;

	CHECK_NATNUM(nframes);

	/* Find the frame requested.  */
	for (i = XINT(nframes); backlist && (i-- > 0);)
		backlist = backlist->next;

	if (!backlist)
		return Qnil;
	if (backlist->nargs == UNEVALLED)
		return Fcons(Qnil, Fcons(*backlist->function, *backlist->args));
	else {
		if (backlist->nargs == MANY)
			tem = *backlist->args;
		else
			tem = Flist(backlist->nargs, backlist->args);

		return Fcons(Qt, Fcons(*backlist->function, tem));
	}
}

/************************************************************************/
/*			      Warnings					*/
/************************************************************************/

void
warn_when_safe_lispobj(Lisp_Object class, Lisp_Object level, Lisp_Object obj)
{
	obj = list1(list3(class, level, obj));
	if (NILP(Vpending_warnings))
		Vpending_warnings = Vpending_warnings_tail = obj;
	else {
		Fsetcdr(Vpending_warnings_tail, obj);
		Vpending_warnings_tail = obj;
	}
}

/* #### This should probably accept Lisp objects; but then we have
   to make sure that Feval() isn't called, since it might not be safe.

   An alternative approach is to just pass some non-string type of
   Lisp_Object to warn_when_safe_lispobj(); `prin1-to-string' will
   automatically be called when it is safe to do so. */

void warn_when_safe(Lisp_Object class, Lisp_Object level, const char *fmt, ...)
{
	Lisp_Object obj;
	va_list args;

	va_start(args, fmt);
	obj = emacs_doprnt_string_va((const Bufbyte *)GETTEXT(fmt),
				     Qnil, -1, args);
	va_end(args);

	warn_when_safe_lispobj(class, level, obj);
}

/************************************************************************/
/*			    Initialization				*/
/************************************************************************/

void syms_of_eval(void)
{
	INIT_LRECORD_IMPLEMENTATION(subr);

	defsymbol(&Qinhibit_quit, "inhibit-quit");
	defsymbol(&Qautoload, "autoload");
	defsymbol(&Qdebug_on_error, "debug-on-error");
	defsymbol(&Qstack_trace_on_error, "stack-trace-on-error");
	defsymbol(&Qdebug_on_signal, "debug-on-signal");
	defsymbol(&Qstack_trace_on_signal, "stack-trace-on-signal");
	defsymbol(&Qdebugger, "debugger");
	defsymbol(&Qmacro, "macro");
	defsymbol(&Qand_rest, "&rest");
	defsymbol(&Qand_optional, "&optional");
	/* Note that the process code also uses Qexit */
	defsymbol(&Qexit, "exit");
	defsymbol(&Qsetq, "setq");
	defsymbol(&Qinteractive, "interactive");
	defsymbol(&Qcommandp, "commandp");
	defsymbol(&Qdefun, "defun");
	defsymbol(&Qprogn, "progn");
	defsymbol(&Qvalues, "values");
	defsymbol(&Qdisplay_warning, "display-warning");
	defsymbol(&Qrun_hooks, "run-hooks");
	defsymbol(&Qafter_change_major_mode_hook, "after-change-major-mode-hook");
	defsymbol(&Qafter_change_before_major_mode_hook, "after-change-before-major-mode-hook");
	defsymbol(&Qcurrent_running_hook, "current-running-hook");
	defsymbol(&Qif, "if");

	DEFSUBR(For);
	DEFSUBR(Fand);
	DEFSUBR(Fif);
	DEFSUBR_MACRO(Fwhen);
	DEFSUBR_MACRO(Funless);
	DEFSUBR(Fcond);
	DEFSUBR(Fprogn);
	DEFSUBR(Fprog1);
	DEFSUBR(Fprog2);
	DEFSUBR(Fsetq);
	DEFSUBR(Fquote);
	DEFSUBR(Ffunction);
	DEFSUBR(Fdefun);
	DEFSUBR(Fdefmacro);
	DEFSUBR(Fdefvar);
	DEFSUBR(Fdefconst);
	DEFSUBR(Fuser_variable_p);
	DEFSUBR(Flet);
	DEFSUBR(FletX);
	DEFSUBR(Fwhile);
	DEFSUBR(Fmacroexpand_internal);
	DEFSUBR(Fcatch);
	DEFSUBR(Fthrow);
	DEFSUBR(Funwind_protect);
	DEFSUBR(Fcondition_case);
	DEFSUBR(Fcall_with_condition_handler);
	DEFSUBR(Fsignal);
	DEFSUBR(Finteractive_p);
	DEFSUBR(Fcommandp);
	DEFSUBR(Fcommand_execute);
	DEFSUBR(Fautoload);
	DEFSUBR(Feval);
	DEFSUBR(Fapply);
	DEFSUBR(Ffuncall);
	DEFSUBR(Ffunctionp);
	DEFSUBR(Ffunction_min_args);
	DEFSUBR(Ffunction_max_args);
	DEFSUBR(Frun_hooks);
	DEFSUBR(Frun_hook_with_args);
	DEFSUBR(Frun_hook_with_args_until_success);
	DEFSUBR(Frun_hook_with_args_until_failure);
	DEFSUBR(Fbacktrace_debug);
	DEFSUBR(Fbacktrace);
	DEFSUBR(Fbacktrace_frame);
}

void reinit_eval(void)
{
	specpdl_ptr = specpdl;
	specpdl_depth_counter = 0;
	catchlist = 0;
	Vcondition_handlers = Qnil;
	backtrace_list = 0;
	Vquit_flag = Qnil;
	debug_on_next_call = 0;
	lisp_eval_depth = 0;
	entering_debugger = 0;
	changing_major_mode = 0;
}

void reinit_vars_of_eval(void)
{
	preparing_for_armageddon = 0;
	in_warnings = 0;
	Qunbound_suspended_errors_tag =
	    make_opaque_ptr(&Qunbound_suspended_errors_tag);
	staticpro_nodump(&Qunbound_suspended_errors_tag);

	specpdl_size = 50;
	specpdl = xnew_array(struct specbinding, specpdl_size);
	/* XEmacs change: increase these values. */
	max_specpdl_size = 3000;
	max_lisp_eval_depth = 1000;
#ifdef DEFEND_AGAINST_THROW_RECURSION
	throw_level = 0;
#endif
}

void vars_of_eval(void)
{
	reinit_vars_of_eval();

	DEFVAR_INT("max-specpdl-size", &max_specpdl_size	/*
Limit on number of Lisp variable bindings & unwind-protects before error.
								 */ );

	DEFVAR_INT("max-lisp-eval-depth", &max_lisp_eval_depth	/*
Limit on depth in `eval', `apply' and `funcall' before error.
This limit is to catch infinite recursions for you before they cause
actual stack overflow in C, which would be fatal for Emacs.
You can safely make it considerably larger than its default value,
if that proves inconveniently small.
								 */ );

	DEFVAR_LISP("quit-flag", &Vquit_flag	/*
Non-nil causes `eval' to abort, unless `inhibit-quit' is non-nil.
Typing C-G sets `quit-flag' non-nil, regardless of `inhibit-quit'.
						 */ );
	Vquit_flag = Qnil;

	DEFVAR_LISP("inhibit-quit", &Vinhibit_quit	/*
Non-nil inhibits C-g quitting from happening immediately.
Note that `quit-flag' will still be set by typing C-g,
so a quit will be signalled as soon as `inhibit-quit' is nil.
To prevent this happening, set `quit-flag' to nil
before making `inhibit-quit' nil.  The value of `inhibit-quit' is
ignored if a critical quit is requested by typing control-shift-G in
an X frame.
							 */ );
	Vinhibit_quit = Qnil;

	DEFVAR_LISP("stack-trace-on-error", &Vstack_trace_on_error	/*
*Non-nil means automatically display a backtrace buffer
after any error that is not handled by a `condition-case'.
If the value is a list, an error only means to display a backtrace
if one of its condition symbols appears in the list.
See also variable `stack-trace-on-signal'.
									 */ );
	Vstack_trace_on_error = Qnil;

	DEFVAR_LISP("stack-trace-on-signal", &Vstack_trace_on_signal	/*
*Non-nil means automatically display a backtrace buffer
after any error that is signalled, whether or not it is handled by
a `condition-case'.
If the value is a list, an error only means to display a backtrace
if one of its condition symbols appears in the list.
See also variable `stack-trace-on-error'.
									 */ );
	Vstack_trace_on_signal = Qnil;

	DEFVAR_LISP("debug-ignored-errors", &Vdebug_ignored_errors	/*
*List of errors for which the debugger should not be called.
Each element may be a condition-name or a regexp that matches error messages.
If any element applies to a given error, that error skips the debugger
and just returns to top level.
This overrides the variable `debug-on-error'.
It does not apply to errors handled by `condition-case'.
									 */ );
	Vdebug_ignored_errors = Qnil;

	DEFVAR_LISP("debug-on-error", &Vdebug_on_error	/*
*Non-nil means enter debugger if an unhandled error is signalled.
The debugger will not be entered if the error is handled by
a `condition-case'.
If the value is a list, an error only means to enter the debugger
if one of its condition symbols appears in the list.
This variable is overridden by `debug-ignored-errors'.
See also variables `debug-on-quit' and `debug-on-signal'.
							 */ );
	Vdebug_on_error = Qnil;

	DEFVAR_LISP("debug-on-signal", &Vdebug_on_signal	/*
*Non-nil means enter debugger if an error is signalled.
The debugger will be entered whether or not the error is handled by
a `condition-case'.
If the value is a list, an error only means to enter the debugger
if one of its condition symbols appears in the list.
See also variable `debug-on-quit'.
								 */ );
	Vdebug_on_signal = Qnil;

	DEFVAR_BOOL("debug-on-quit", &debug_on_quit	/*
*Non-nil means enter debugger if quit is signalled (C-G, for example).
Does not apply if quit is handled by a `condition-case'.  Entering the
debugger can also be achieved at any time (for X11 console) by typing
control-shift-G to signal a critical quit.
							 */ );
	debug_on_quit = 0;

	DEFVAR_BOOL("debug-on-next-call", &debug_on_next_call	/*
Non-nil means enter debugger before next `eval', `apply' or `funcall'.
								 */ );

	DEFVAR_LISP("debugger", &Vdebugger	/*
Function to call to invoke debugger.
If due to frame exit, args are `exit' and the value being returned;
this function's value will be returned instead of that.
If due to error, args are `error' and a list of the args to `signal'.
If due to `apply' or `funcall' entry, one arg, `lambda'.
If due to `eval' entry, one arg, t.
						 */ );
	DEFVAR_LISP("after-change-major-mode-hook", &Vafter_change_major_mode_hook	/*
Normal hook run at the very end of major mode functions.
						*/);
	Vafter_change_major_mode_hook = Qnil;

	DEFVAR_LISP("after-change-before-major-mode-hook", &Vafter_change_before_major_mode_hook	/*
Normal hook run before a major mode hook is run.
						*/);
	Vafter_change_before_major_mode_hook = Qnil;

	DEFVAR_LISP("current-running-hook", &Vcurrent_running_hook	/*
Symbol of the current running hook. nil if no hook is running.
						*/);
	Vcurrent_running_hook = Qnil;

	Vdebugger = Qnil;

	staticpro(&Vpending_warnings);
	Vpending_warnings = Qnil;
	dump_add_root_object(&Vpending_warnings_tail);
	Vpending_warnings_tail = Qnil;

	staticpro(&Vautoload_queue);
	Vautoload_queue = Qnil;

	staticpro(&Vcondition_handlers);

	staticpro(&Vcurrent_warning_class);
	Vcurrent_warning_class = Qnil;

	staticpro(&Vcurrent_error_state);
	Vcurrent_error_state = Qnil;	/* errors as normal */

	reinit_eval();
}
