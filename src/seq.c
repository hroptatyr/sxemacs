/*** seq.c -- generic sequence service
 *
 * Copyright (C) 2008 Sebastian Freundt
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

#include "config.h"
#include "lisp.h"
#include "seq.h"

Lisp_Object Qseqp;

/* an error throwing seq impl */
static size_t
seq_unsupp_length(const seq_t s)
{
	Lisp_Object sequence = (Lisp_Object)(long int)s;
	check_losing_bytecode("length", sequence);
	sequence = wrong_type_argument(Qsequencep, sequence);
	return 0UL;
}

static size_t
seq_empty_length(const seq_t s)
{
	return 0UL;
}

static struct seq_impl_s __seq_unsupp = {
	.length_f = seq_unsupp_length,
};

static struct seq_impl_s __seq_empty = {
	.length_f = seq_empty_length,
};

seq_impl_t seq_unsupp = &__seq_unsupp;
seq_impl_t seq_empty = &__seq_empty;


/* we are smart, we use the names desired by libtool, making a module out of
   this one day is easier than peeing^Wbreathing */
void
seq_LTX_init(void)
{
	DEFSYMBOL(Qseqp);
	Fprovide(intern("seq"));
}

void
seq_LTX_reinit(void)
{
}

void
seq_LTX_deinit(void)
{
	Frevoke(intern("seq"));
}

/* seq.c ends here */
