/* Copyright (C) 1995 Free Software Foundation.

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


/* Synched up with: Mule 2.3.  Not in FSF. */

#include <config.h>
#include "lisp.h"

#include "regex.h"

#ifdef MULE_REGEXP

Lisp_Object Vre_word;

int re_short_flag;

DEFUN("define-word-pattern", Fdefine_word_pattern, 1, 1, 0,	/*
Don't call this function directly, instead use 'define-word' which
accept a pattern compiled by 'regexp-compile' with word-option t.
*/
      (pattern))
{
	int i, len;
	char *p;
	Lisp_Object temp;
	Lisp_String *s;

	CHECK_CONS(pattern);
	len = XINT(Flength(pattern));
	if (len > MAXWORDBUF)
		error("Too complicated regular expression for word!");
	for (i = 0; i < len; i++) {
		temp = XCAR(pattern);
		CHECK_VECTOR(temp);
		CHECK_STRING(XVECTOR_DATA(temp)[0]);
		s = XSTRING(XVECTOR_DATA(temp)[0]);
		if (!wordbuf[i])
			wordbuf[i] = xnew(struct re_pattern_buffer);
		else if (wordbuf[i]->buffer)
			xfree(wordbuf[i]->buffer);
		wordbuf[i]->buffer = (char *)xmalloc_atomic(s->size + 1);
		wordbuf[i]->used = s->size;
		memcpy(wordbuf[i]->buffer, s->data, s->size + 1);
#ifdef EMACS19_REGEXP
		wordbuf[i]->translate = 0;
		wordbuf[i]->fastmap_accurate = 0;
		wordbuf[i]->fastmap = 0;
		wordbuf[i]->can_be_null = 1;

		wordbuf[i]->mc_flag = 1;
		wordbuf[i]->short_flag = 0;
		wordbuf[i]->no_empty = 0;

		wordbuf[i]->syntax_version = 0;
		wordbuf[i]->category_version = 0;

		wordbuf[i]->regs_allocated = REGS_UNALLOCATED;
		wordbuf[i]->re_nsub = 0;
		wordbuf[i]->no_sub = 0;
		wordbuf[i]->newline_anchor = 1;

		wordbuf[i]->syntax = 0;
		wordbuf[i]->not_bol = wordbuf[i]->not_eol = 0;
#endif				/* EMACS19_REGEXP */
		pattern = XCDR(pattern);
	}
	for (; i < MAXWORDBUF && wordbuf[i]; i++) {
		if (wordbuf[i]->buffer)
			xfree(wordbuf[i]->buffer);
		xfree(wordbuf[i]);
		wordbuf[i] = (struct re_pattern_buffer *)0;
	}
	return Qnil;
}

#endif				/* MULE_REGEXP */

void syms_of_mule(void)
{
#ifdef MULE_REGEXP
	DEFSUBR(Fdefine_word_pattern);
#endif
}

void vars_of_mule(void)
{
#ifdef MULE_REGEXP
	DEFVAR_BOOL("re-short-flag", &re_short_flag	/*
*T means regexp search success when the shortest match is found.
							 */ );
	re_short_flag = 0;
#endif				/* MULE_REGEXP */

	Fprovide(intern("mule"));

#ifdef HAVE_EGG
	Fprovide(intern("egg"));
#endif
#ifdef HAVE_WNN
	Fprovide(intern("wnn"));
#endif
}
