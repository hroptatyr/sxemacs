/** media-magic.h -- file/libmagic file analysis
 *
 * Copyright (C) 2007 Sebastian Freundt
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
 */

/* Synched up with: Not in FSF. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "lisp.h"

#include "media-magic.h"

Lisp_Object Qmagic;

/* just a dummy declaration */
void magic_process(magic_t magic, const char *inname);

void
magic_process(magic_t magic, const char *inname)
{
	const char *type;

	magic = magic_open(0);
	if (magic == NULL) {
		fprintf(stderr, "ERROR\n");
	}

	if (magic_load(magic, "/usr/local/share/file/magic") == -1) {
		fprintf(stderr, "magic: %s\n", magic_error(magic));
		return;
	}

	type = magic_file(magic, inname);
	if (type == NULL) {
		fprintf(stderr, "ERROR: %s\n", magic_error(magic));
	} else {
		fprintf(stderr, "%s\n", type);
	}
}


/* media-magic.c ends here */
