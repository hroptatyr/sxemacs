/*** dynacat.h -- dynamic categories (`types' on top of types) for SXEmacs
 *
 * Copyright (C) 2005-2008 Sebastian Freundt <hroptatyr@sxemacs.org>
 *
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
 * Alternatively, for testing purposes, this file can be redistributed under
 * the following licence:
 * You can do whatever the fuck you want with this, except stop anyone else
 * doing whatever the fuck they want.
 *
 **/

#ifndef INCLUDED_dynacat_h_
#define INCLUDED_dynacat_h_

typedef struct dynacat_s *dynacat_t;
typedef void(*dynacat_marker_f)(Lisp_Object);
typedef void(*dynacat_finaliser_f)(Lisp_Object, int);
typedef void(*dynacat_printer_f)(Lisp_Object, Lisp_Object, int);
typedef void(*dynacat_intprinter_f)(void*, Lisp_Object);

struct dynacat_s {
	struct lrecord_header lheader;

	Lisp_Object type;               /* type of emod object */
	Lisp_Object plist;              /* properties list */

	void *ptr;			/* pointer to foreign data */
	dynacat_marker_f mrkfun;	/* marker function */
	dynacat_finaliser_f finfun;	/* finaliser function */
	dynacat_printer_f prfun;	/* print function */
	dynacat_intprinter_f intprfun;	/* internal print function */
};

extern void dynacat_fini(dynacat_t);
extern Lisp_Object make_dynacat(void *ptr);


DECLARE_LRECORD(dynacat, struct dynacat_s);
#define XDYNACAT(x)		XRECORD(x, dynacat, struct dynacat_s)
#define XSETDYNACAT(x, p)	XSETRECORD(x, p, dynacat)
#define DYNACATP(x)		RECORDP(x, dynacat)
#define CHECK_DYNACAT(x)	CHECK_RECORD(x, dynacat)
#define CONCHECK_DYNACAT(x)	CONCHECK_RECORD(x, dynacat)
#define XDYNACAT_TYPE(x)	(XDYNACAT(x)->type)
#define XDYNACAT_PLIST(x)	(XDYNACAT(x)->plist)

extern Lisp_Object make_dynacat(void*);

extern_inline void *get_dynacat(Lisp_Object);
extern_inline void set_dynacat(Lisp_Object, void *p);
extern_inline void set_dynacat_marker(Lisp_Object, dynacat_marker_f);
extern_inline void set_dynacat_finaliser(Lisp_Object, dynacat_finaliser_f);
extern_inline void set_dynacat_printer(Lisp_Object, dynacat_printer_f);
extern_inline dynacat_intprinter_f get_dynacat_intprinter(Lisp_Object);
extern_inline void set_dynacat_intprinter(Lisp_Object, dynacat_intprinter_f);
extern_inline Lisp_Object get_dynacat_type(Lisp_Object);
extern_inline void set_dynacat_type(Lisp_Object, Lisp_Object);


extern_inline void *
get_dynacat(Lisp_Object ptr)
{
	CHECK_DYNACAT(ptr);
	return XDYNACAT(ptr)->ptr;
}
extern_inline void
set_dynacat(Lisp_Object ptr, void *p)
{
	CHECK_DYNACAT(ptr);
	XDYNACAT(ptr)->ptr = p;
	return;
}
extern_inline void
set_dynacat_marker(Lisp_Object ptr, dynacat_marker_f mrkfun)
{
	CHECK_DYNACAT(ptr);
	XDYNACAT(ptr)->mrkfun = mrkfun;
}
extern_inline void
set_dynacat_finaliser(Lisp_Object ptr, dynacat_finaliser_f finfun)
{
	CHECK_DYNACAT(ptr);
	XDYNACAT(ptr)->finfun = finfun;
}
extern_inline void
set_dynacat_printer(Lisp_Object ptr, dynacat_printer_f prfun)
{
	CHECK_DYNACAT(ptr);
	XDYNACAT(ptr)->prfun = prfun;
}
extern_inline dynacat_intprinter_f
get_dynacat_intprinter(Lisp_Object ptr)
{
	CHECK_DYNACAT(ptr);
	return XDYNACAT(ptr)->intprfun;
}
extern_inline void
set_dynacat_intprinter(Lisp_Object ptr, dynacat_intprinter_f ipf)
{
	CHECK_DYNACAT(ptr);
	XDYNACAT(ptr)->intprfun = ipf;
}

extern_inline Lisp_Object
get_dynacat_type(Lisp_Object ptr)
{
	CHECK_DYNACAT(ptr);
	return XDYNACAT(ptr)->type;
}

extern_inline void
set_dynacat_type(Lisp_Object ptr, Lisp_Object type)
{
	CHECK_DYNACAT(ptr);
	XDYNACAT(ptr)->type = type;
}

#endif	/* INCLUDED_dynacat_h_ */
