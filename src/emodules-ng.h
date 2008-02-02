/*** emodules-ng.h -- Dynamic Loader routines (via ltdl)
 *
 * Copyright (C) 2007 Sebastian Freundt
 *
 * Author:  Sebastian Freundt <hroptatyr@sxemacs.org>
 *
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
 *
 ***/

#ifndef INCLUDED_emodules_ng_h_
#define INCLUDED_emodules_ng_h_

#include <stdbool.h>


#define EMODNG_INIT		"init"
#define EMODNG_REINIT		"reinit"
#define EMODNG_DEINIT		"deinit"

#define LTX_PREFIX(_name)	_name##_LTX_
#define LTX_PUBFUN(_name, _fun)	_name##_LTX_##_fun
#define LTX_PUBINIT(_name)	LTX_PUBFUN(_name, init)
#define LTX_PUBREINIT(_name)	LTX_PUBFUN(_name, reinit)
#define LTX_PUBDEINIT(_name)	LTX_PUBFUN(_name, deinit)

#define EMOD_PUBFUN(_fun)	LTX_PUBFUN(EMODNAME, _fun)
#define EMOD_PUBINIT		LTX_PUBINIT(EMODNAME)
#define EMOD_PUBREINIT		LTX_PUBREINIT(EMODNAME)
#define EMOD_PUBDEINIT		LTX_PUBDEINIT(EMODNAME)

#define PROVIDE(_name)					\
	static bool _inittedp = false;			\
	bool *_name##_LTX_inittedp = &_inittedp;
#define REQUIRE(_name, args...)						\
	const char *_name##_LTX_dependencies[] = { args, NULL };	\
	const size_t _name##_LTX_ndependencies =			\
		countof(_name##_LTX_dependencies);

#ifdef ALL_DEBUG_FLAGS
#undef EMOD_DEBUG_FLAG
#define EMOD_DEBUG_FLAG
#endif

#define __EMOD_DEBUG__(args...)		fprintf(stderr, "EMOD " args)
#ifndef EMOD_DEBUG_FLAG
#define EMOD_DEBUG(args...)
#else
#define EMOD_DEBUG(args...)		__EMOD_DEBUG__(args)
#endif
#define EMOD_DEBUG_LOADER(args...)	EMOD_DEBUG("[loader]: " args)
#define EMOD_CRITICAL(args...)		__EMOD_DEBUG__("CRITICAL: " args)

typedef struct emodng_s *emodng_t;
typedef bool (*emodng_init_f)(const emodng_t);
typedef bool (*emodng_reinit_f)(const emodng_t);
typedef bool (*emodng_deinit_f)(const emodng_t);
typedef bool (*emodng_docs_f)(const emodng_t);

struct emodng_state_s {
	bool opened;
	bool initialised;
};

/*
 * Because subrs and symbols added by a dynamic module are not part of
 * the make-docfile process, we need a clean way to get the variables
 * and functions documented. Since people don't like the idea of making
 * shared modules use different versions of DEFSUBR() and DEFVAR_LISP()
 * and friends, we need these two functions to insert the documentation
 * into the right place. These functions will be called by the module
 * init code, generated by ellcc during initialization mode.
 */
extern void emodng_doc_subr(const char *objname, const char *docstr);
extern void emodng_doc_sym(const char *objname, const char *docstr);

#define CDOCSUBR(Fname, DOC)	emodng_doc_subr(Fname, DOC)
#define CDOCSYM(Sname, DOC)	emodng_doc_sym(Sname, DOC)


extern void syms_of_emodng(void);
extern void reinit_vars_of_emodng(void);
extern void vars_of_emodng(void);

#endif	/* INCLUDED_emodules_ng_h_ */
