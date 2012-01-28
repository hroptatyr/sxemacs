/*** cl.h -- Common Lisp Goodness, the fast version
 *
 * Copyright (C) 2006 - 2008 Sebastian Freundt
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

/* Synched up with: Not in FSF. */

#ifndef INCLUDED_cl_h_
#define INCLUDED_cl_h_ 1

#if IMA_MODULE
#define EMODCL_PREFIX		"cl:"
#else
#define EMODCL_PREFIX		""
#endif

#ifdef ALL_DEBUG_FLAGS
#undef EMOD_CL_DEBUG_FLAG
#define EMOD_CL_DEBUG_FLAG
#endif

#define __EMOD_CL_DEBUG__(args...)	fprintf(stderr, "emodule(CL) " args)
#ifndef EMOD_CL_DEBUG_FLAG
#define EMOD_CL_DEBUG(args...)
#else
#define EMOD_CL_DEBUG(args...)		__EMOD_CL_DEBUG__(args)
#endif
#define EMOD_CL_CRITICAL(args...)	__EMOD_CL_DEBUG__("CRITICAL: " args)

#ifdef SXE_UNUSED
#elif defined(__GNUC__)
#  define SXE_UNUSED(x) UNUSED_ ## x __attribute__((unused))
#elif defined(__LCLINT__)
#  define SXE_UNUSED(x) /*@unused@*/ x
#else
#  define SXE_UNUSED(x) x
#endif

extern void cl_LTX_init(void);
extern void cl_LTX_deinit(void);
extern void cl_LTX_reinit(void);

#endif	/* INCLUDED_cl_h_ */
