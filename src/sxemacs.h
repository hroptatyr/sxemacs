/*** sxemacs.h -- meta include file for external emodule sources
 *
 * Copyright (C) 2004 Steve Youngs.
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
 *
 ***/

#ifndef INCLUDED_sxemacs_h_
#define INCLUDED_sxemacs_h_

/***
 * This header is just a convenience thing. Projects using SXEmacs will solely
 * need to include this one and will not have to care about additional stuff.
 * In particular this simplifies include paths since we might not be the only
 * project on this earth with a lisp.h header and we are definitely not the
 * only ones with a process.h file.  Thus, to avoid confusion because of
 * mistakenly included files from other projects we provide this meta-header.
 *
 * To use this in your emodules (or wherever) install SXEmacs (>= 22.1.7)
 * properly (or assume it has been done already), then invoke pkg-config to
 * determine the include path of the latest and greatest SXEmacs on your/that
 * box, like so:
 *
 *	pkg-config --cflags sxemacs
 *
 * and save that value somehow, say to SXE_CPPFLAGS, then as your final
 * CPPFLAGS you'd use "${YOUR_CPPFLAGS} ${SXE_CPPFLAGS}" in the Makefile (or
 * whatever system you're using).  It's probably not the fastest way to do but
 * of course you could just invoke your C compiler like so:
 *
 *	gcc $(pkg-config --cflags sxemacs) ...
 *
 * In your project, thence, just use
 *
 *	#include <sxemacs.h>
 *
 * and forget about all the other include files.  Of course, you can include
 * sxemacs.h as many times as you like.
 *
 * Easy, innit? :)
 *
 ***/

/* pretend to be an emacs
 * some stuff is suitable for other than emacsen modules too
 * however to avoid one more CPPFLAG we simply define `emacs' here
 */
#ifndef emacs
# define emacs
#endif

#if defined USE_SXEMACS_CONFIG_H
# include "config.h"
#endif
#include "lisp.h"

#include "sysfile.h"
#include "sysdir.h"
#include "systime.h"
#include "sysdep.h"
#include "syspwd.h"
#include "buffer.h"
#include "commands.h"
#include "elhash.h"
#include "regex.h"
#include "opaque.h"
#include "syntax.h"
#include "dllist.h"
#include "bloom.h"
#include "dynacat.h"

#if defined EF_USE_ASYNEQ
# include "events/worker-asyneq.h"
#endif

#include "emodules-ng.h"

#if defined USE_SXEMACS_CONFIG_H
/* clean up, if someone else wants to use autoheader,
 * we shouldn't leave this stuff here */
# undef PACKAGE
# undef PACKAGE_VERSION
# undef PACKAGE_STRING
# undef PACKAGE_NAME
# undef PACKAGE_BUGREPORT
# undef PACKAGE_TARNAME
# undef VERSION
#endif	/* USE_SXEMACS_CONFIG_H */

#endif	/* INCLUDED_sxemacs_h_ */
