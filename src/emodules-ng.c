/*** emodules-ng.c -- Dynamic Loader routines (via ltdl)
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

#include "config.h"
#include <stdbool.h>
#include "lisp.h"
#include "sysdep.h"
#include "emodules-ng.h"
#include "dynacat.h"

#if defined HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

#if defined HAVE_LTDL_H  /* go ltdl */
# include <ltdl.h>
#else
# error "Bugger! No code to dlopen() things"
#endif

#define EMODNG_INIT		"init"
#define EMODNG_REINIT		"reinit"
#define EMODNG_DEINIT		"deinit"

/* this is internal administration */
static lt_dlhandle meself = NULL;
static emodng_t emods = NULL;

typedef struct emod_epcell_s /* endpoint cell */ *emod_epcell_t;
static emod_epcell_t emod_endpoints = NULL;

struct emod_epcell_s {
	emodng_t emod;
	emod_epcell_t next;
};

/* Do we do our work quietly? */
int load_modules_quietly;

/* Load path */
Lisp_Object Vmodule_load_path, Vmodule_extensions;

/* lt_dlsym() violates ISO C, so confide the breakage into this function to
 * avoid warnings. */
typedef void(*emodng_f)(void);

struct emodng_lby_s {
	lt_dlhandle loaded_by;
	struct emodng_lby_s *next;
};

struct emodng_s {
	const char *name;
	const char *truename;
	const char *filename;
	lt_dlhandle dl;

	struct emodng_lby_s *lby;

	emodng_init_f initf;
	emodng_deinit_f deinitf;
	emodng_reinit_f reinitf;
	emodng_docs_f docsf;

	emodng_t *deps;
	size_t ndeps;

	bool openedp;
	bool initialisedp;
	bool docsloadedp;

	/* navigation info */
	emodng_t next;
};

static Lisp_Object emodng_load_unwind(Lisp_Object unwind_ptr);
static Lisp_Object emodng_unload_unwind(Lisp_Object unwind_ptr);
static emodng_t emodng_load(const char *name, lt_dlhandle);
static bool emodng_unload(const char *name);
static void _emodng_open(emodng_t emod);
static void _emodng_close(emodng_t emod);
static void _emodng_init(emodng_t emod);
static void _emodng_deinit(emodng_t emod);
static void _emodng_docs(emodng_t emod);
static inline emodng_t _emodng_open_dep(emodng_t emod, const char *dep)
	__attribute__((always_inline));
static inline void _emodng_close_dep(emodng_t emod, emodng_t dep)
	__attribute__((always_inline));


/* some inlines and helper funs */
static inline emodng_f
lt_dlfun(lt_dlhandle handle, const char *symbol)
	__attribute__((always_inline));
static inline emodng_f
lt_dlfun(lt_dlhandle handle, const char *symbol)
{
	void *tmp = lt_dlsym(handle, symbol);
	return (emodng_f)(long int)tmp;
}

#if defined HAVE_STAT || 1
static inline bool
__file_exists_p(const char *filename)
{
	struct stat _stbuf;
	return stat(filename, &_stbuf) == 0;
}
#endif	/* HAVE_STAT */

static lt_dlhandle
__emodng_open_append_exts(const char *filename)
{
	/* append all specified extensions */
	size_t nname = strlen(filename);
	char name[nname + /*length of longest extension:*/24];
	/* note to myself: better check that length! */
	char *p = xstpncpy(name, filename, nname+1);
	size_t remain = name + sizeof(name) - p;

	for (Lisp_Object ext = Vmodule_extensions;
	     CONSP(ext); ext = XCDR(ext)) {
		if (UNLIKELY(!STRINGP(XCAR(ext)))) {
			continue;
		}

		xstrncpy(p, (const char*)XSTRING_DATA(XCAR(ext)),
			 remain);

		if (__file_exists_p(name)) {
			EMOD_DEBUG_LOADER("trying %s\n", name);
			return lt_dlopen(name);
		}
	}
	return NULL;
}

static lt_dlhandle
__emodng_open_prepend_paths(const char *filename)
{
	/* append all specified extensions */
	size_t nfilename = strlen(filename);

	/* we used to use Vmodule_load_path here */
	for (Lisp_Object path = Vload_path;
	     CONSP(path); path = XCDR(path)) {
		if (LIKELY((STRINGP(XCAR(path))))) {
			char name[(size_t)XSTRING_LENGTH(XCAR(path))
				  + 1 /*for the slash*/
				  + nfilename /*for the module's actual name*/
				  + 1 /*for the terminating \0 char*/];
			/* note that we assume here that filename already
			 * carries the correct extension
			 * see __emodng_open_prepend_paths_append_exts for
			 * cope with all the combinations */
			char *p;
			p = xstpncpy(name,
				     (const char*)XSTRING_DATA(XCAR(path)),
				     sizeof(name)-2);
			*p++ = '/';
			*p = '\0';
			xstrncpy(p, filename, name + sizeof(name) - p);

			if (__file_exists_p(name)) {
				EMOD_DEBUG_LOADER("trying %s\n", name);
				return lt_dlopen(name);
			}
		}
	}
	return NULL;
}

static lt_dlhandle
__emodng_open_prepend_paths_append_exts(const char *filename)
{
	/* append all specified extensions */
	size_t nfilename = strlen(filename);

	/* we used to use Vmodule_load_path here */
	for (Lisp_Object path = Vload_path;
	     CONSP(path); path = XCDR(path)) {
		if (LIKELY((STRINGP(XCAR(path))))) {
			char name[(size_t)XSTRING_LENGTH(XCAR(path))
				  + 1 /*for the slash*/
				  + nfilename /*for the module's actual name*/
				  + 24 /* for the length of the extension
					* BETTER CHECK THAT */
				  + 1 /*for the terminating \0 char*/];
			/* note that we assume here that filename already
			 * carries the correct extension
			 * see __emodng_open_prepend_paths_append_exts for
			 * cope with all the combinations */
			char *p = xstpncpy(name,
				(const char*)XSTRING_DATA(XCAR(path)),
				sizeof(name)-2);
			int remain;
			if (*(p-1) != '/') {
				*p++ = '/';
				*p = '\0';
			}
			remain = name+sizeof(name)-p;
			p = xstpncpy(p, filename, remain);
			remain = name+sizeof(name)-p;

			/* append all extensions now */
			for (Lisp_Object ext = Vmodule_extensions;
			     CONSP(ext); ext = XCDR(ext)) {
				if (UNLIKELY(!STRINGP(XCAR(ext)))) {
					continue;
				}

				xstrncpy(p,(const char*)XSTRING_DATA(XCAR(ext)),
					 remain);

				if (__file_exists_p(name)) {
					EMOD_DEBUG_LOADER("trying \"%s\"\n",
							  name);
					return lt_dlopen(name);
				}
			}
		}
	}
	return NULL;
}

static void
__emodng_open(emodng_t emod)
{
	/* obtain some info to massage the name of the emod */
	/* determine if emod->name is an absolute filename */
	bool abso_path_p = emod->name[0] == '/';
	/* determine if emod->name is a relative filename */
	bool rel_path_p =
		strncmp(emod->name, "./", 2) == 0 ||
		strncmp(emod->name, "../", 3) == 0;
	/* determine if emod->name carries an extension already
	 * we search backward to an occurrence of '.' and forward for a '/'
	 * to make sure it really belongs to the last path component */
	char *ext_compo = strrchr(emod->name, '.');
	bool has_ext_p = ext_compo != NULL && strchr(ext_compo, '/') == NULL;

	if (LIKELY(!abso_path_p && !rel_path_p && !has_ext_p)) {
		/* append both extension and paths */
		emod->dl = __emodng_open_prepend_paths_append_exts(emod->name);
	} else if (!abso_path_p && !rel_path_p) {
		/* just append paths */
		emod->dl = __emodng_open_prepend_paths(emod->name);
	} else if (has_ext_p) {
		/* fully featured file name, just dlopen() it */
		emod->dl = lt_dlopen(emod->name);
	} else if (!has_ext_p) {
		/* file name with path but sans extension */
		emod->dl = __emodng_open_append_exts(emod->name);
	}
	/* actually we should cater with those relative file names
	 * more thoroughly, I'm not sure what './' means in libltdl speak */
	return;
}

static emodng_t
__emodng_find_or_create(emodng_t in)
{
/* try to find the emodule specified by in or create one
 * if it cannot be found */
	const lt_dlinfo *info;
	emodng_t new;

	/* get module info */
	info = lt_dlgetinfo(in->dl);

	EMOD_DEBUG_LOADER("modinfo %s %s %d\n",
			  info->filename, info->name, info->ref_count);

	for (emodng_t e = emods; e; e = e->next) {
		if (info->name &&
		    strcmp(info->name, e->truename) == 0) {
			/* bingo */
			return e;
		}
	}

	/* otherwise we have to create this */
	new = xnew(struct emodng_s);

	new->truename = xstrdup(info->name);
	new->filename = xstrdup(info->filename);
	new->name = xstrdup(in->name);
	new->dl = in->dl;
	new->openedp =	new->initialisedp = new->docsloadedp = false;

	new->deps = in->deps;
	new->ndeps = in->ndeps;

	new->next = emods;
	return emods = new;
}

static emodng_t
__emodng_find(const char *name)
{
/* try to find the emodule named `name', return its pointer or NULL */
	emodng_t e;

	for (e = emods; e; e = e->next) {
		if (strcmp(name, e->truename) == 0) {
			/* bingo */
			break;
		}
	}

	if (e == NULL) {
		return NULL;
	}

	/* get module info */
#ifdef EMOD_DEBUG_FLAG
	{
		const lt_dlinfo *info = lt_dlgetinfo(e->dl);

		EMOD_DEBUG_LOADER("modinfo %s %s %d\n",
				  info->filename, info->name, info->ref_count);
	}
#endif
	return e;
}

static void
__emodng_remove(emodng_t emod)
{
	/* dehorstify this ... */
	if (UNLIKELY(emod == NULL)) {
		return;
	}

	/* if emod was the top of the list, just kick it */
	if (emods && emod == emods) {
		emods = emod->next;
		return;
	}

	for (emodng_t e = emods; e; e = e->next) {
		if (e->next && e->next->dl == emod->dl) {
			e->next = emod->next;
			return;
		}
	}
	return;
}

static inline bool
emod_endpoint_p(emodng_t emod)
{
	/* a VERY sloppy approach */
	for (emod_epcell_t ee = emod_endpoints; ee; ee = ee->next) {
		if (ee->emod == emod) {
			return true;
		}
	}
	return false;
}

static inline void
_register_endpoint(emodng_t emod)
{
	emod_epcell_t new = xnew(struct emod_epcell_s);

	new->emod = emod;
	new->next = emod_endpoints;
	emod_endpoints = new;
	return;
}

static inline void
_unregister_endpoint(emodng_t emod)
{
	/* very unlikely, but dehorstify anyway */
	if (UNLIKELY(emod == NULL)) {
		return;
	}

	/* maybe we're lucky today and deal with the topmost entry ... */
	if (emod_endpoints && emod_endpoints->emod == emod) {
		emod_epcell_t free_me = emod_endpoints;
		emod_endpoints = emod_endpoints->next;
		xfree(free_me);
		return;
	}

	/* ... gah, what a fooking mess, traverse the list */
	for (emod_epcell_t e = emod_endpoints; e && e->next; e = e->next) {
		if (e->next->emod == emod) {
			emod_epcell_t free_me = e->next;
			e->next = e->next->next;
			xfree(free_me);
			return;
		}
	}
	return;
}

static inline void
free_emodng(emodng_t emod)
{
	xfree(emod->truename);
	xfree(emod->filename);
	xfree(emod->name);
	if (emod->deps) {
		xfree(emod->deps);
	}
	xfree(emod);
}


/* we divide the load phase into two actual phases, open and init
 * similarly unload is actually deinit and close (yes, in that order, horst)
 * it is just because we make sure about dependencies and things */
static emodng_t
emodng_load(const char *name, lt_dlhandle caller)
{
/* this is the loader chain which opens->inits->imports docs */
	struct emodng_s _emod = {.name = name};
	emodng_t emod;

	lt_dlinit();
	_emodng_open(&_emod);
	if (UNLIKELY(_emod.dl == NULL)) {
		return NULL;
	}

	/* set up a real emodng_t object and put it into our global emod list */
	emod = __emodng_find_or_create(&_emod);
	emod->openedp = true;

	_emodng_init(emod);
	_emodng_docs(emod);

	return emod;
}

static void
_emodng_open(emodng_t emod)
{
	const char **emoddeps;

	EMOD_DEBUG_LOADER("loading %s\n", emod->name);
	__emodng_open(emod);

	/* check that we're really using a valid handle */
	if (UNLIKELY(emod->dl == NULL)) {
		error("Opening dynamic module \"%s\": %s",
		      emod->name, lt_dlerror());
	}

	emod->deps = NULL;
	emod->ndeps = 0;

	/* checking dependencies */
	if (LIKELY((emoddeps =
		    (const char**)lt_dlsym(emod->dl, "dependencies")) ||
		   (emoddeps =
		    (const char**)lt_dlsym(emod->dl, "deps")))) {
		emodng_t *ed;
		size_t ndeps = 0;

		/* count the deps manually */
		for (const char **deps = emoddeps, *dep = *deps;
		     dep; dep = *++deps, ndeps++);

		if (ndeps == 0) {
			return;
		}

		/* create a big array of deps */
		EMOD_DEBUG_LOADER("found %lu deps\n",
				  (long unsigned int)ndeps);
		ed = emod->deps = xnew_array(emodng_t, emod->ndeps = ndeps);

		/* load the other modules (deps) first */
		for (const char **deps = emoddeps, *dep = *deps;
		     dep; dep = *++deps) {
			*ed++ = _emodng_open_dep(emod, dep);
		}
	} else {
		EMOD_DEBUG_LOADER("no deps found ... "
				  "hope this is right\n");
	}
	return;
}

static void
_emodng_init(emodng_t emod)
{
/* calls initialiser code in the module */
	if (emod->initialisedp) {
		EMOD_DEBUG_LOADER("already initialised\n");
		return;
	}

	EMOD_DEBUG_LOADER("initialising %s\n", emod->truename);
	if (LIKELY((emod->initf =
		    (emodng_init_f)lt_dlfun(emod->dl, "init")) != NULL)) {
		EMOD_DEBUG_LOADER("found init() ... calling now\n");
		(*emod->initf)(emod);
	} else {
		EMOD_DEBUG_LOADER("no init() function found ... "
				  "hope this is right\n");
	}
	emod->initialisedp = true;
	return;
}

static void
_emodng_docs(emodng_t emod)
{
/* because we're lisp, we have to care for documentation strings of our loaded
   stuff, thus call a function docs() if present */
	if (emod->docsloadedp) {
		EMOD_DEBUG_LOADER("already loaded doc strings\n");
		return;
	}

	EMOD_DEBUG_LOADER("loading docs from %s\n", emod->name);
	if (LIKELY((emod->docsf =
		    (emodng_docs_f)lt_dlfun(emod->dl, "docs")) != NULL)) {
		EMOD_DEBUG_LOADER("found docs() ... calling now\n");
		(*emod->docsf)(emod);
	} else {
		EMOD_DEBUG_LOADER("no docs() function found ... "
				  "hope this is right\n");
	}
	emod->docsloadedp = true;
	return;
}

static inline emodng_t
_emodng_open_dep(emodng_t emod, const char *dep)
{
	EMOD_DEBUG_LOADER("module \"%s\" depends upon \"%s\", "
			  "hence loading \"%s\"\n",
			  emod->name, dep, dep);
	return emodng_load(dep, emod->dl);
}

/* unloader code */
static bool
emodng_unload(const char *name)
{
	emodng_t e = __emodng_find(name);

	if (e == NULL) {
		return false;
	} else if (!emod_endpoint_p(e)) {
		error("Module %s is referenced by other modules\n",
		      e->truename);
		return false;
	}

	_unregister_endpoint(e);
	_emodng_deinit(e);
	_emodng_close(e);
	lt_dlexit();
	return true;
}

static void
_emodng_close(emodng_t emod)
{
	const lt_dlinfo *info;

	/* get module info */
	info = lt_dlgetinfo(emod->dl);

	EMOD_DEBUG_LOADER("trying to unload %s (%d)\n",
			  emod->name, info->ref_count);

	lt_dlclose(emod->dl);

	/* close all the dep modules too */
	for (size_t i = 0; i < emod->ndeps; i++) {
		_emodng_close_dep(emod, emod->deps[i]);
	}

	if (info->ref_count == 0) {
		EMOD_DEBUG_LOADER("kicking %s from the list of loaded mods\n",
				  emod->name);
		__emodng_remove(emod);
		free_emodng(emod);
	}
	return;
}

static void
_emodng_deinit(emodng_t emod)
{
/* calls deinitialiser code in the module */
	EMOD_DEBUG_LOADER("deinitialising %s\n", emod->truename);
	if (LIKELY((emod->deinitf =
		    (emodng_init_f)lt_dlfun(emod->dl, "deinit")) != NULL)) {
		EMOD_DEBUG_LOADER("found deinit() ... calling now\n");
		(*emod->deinitf)(emod);
	} else {
		EMOD_DEBUG_LOADER("no deinit() function found ... "
				  "hope this is right\n");
	}
	emod->initialisedp = false;
	return;
}

static inline void
_emodng_close_dep(emodng_t emod, emodng_t dep)
{
	if (UNLIKELY(dep == NULL)) {
		return;
	}

	EMOD_DEBUG_LOADER("module \"%s\" depended upon \"%s\", "
			  "hence unloading \"%s\"\n",
			  emod->name, dep->name, dep->truename);
	_emodng_close(dep);
	return;
}


/* unwind code in case something's wrong */
static Lisp_Object
emodng_load_unwind(Lisp_Object unw)
{
	lt_dlhandle mod_handle = get_dynacat(unw);

	/* if the handle's still vivid, drown him */
	if (mod_handle) {
		lt_dlclose(mod_handle);
	}
	return Qnil;
}

static Lisp_Object
emodng_unload_unwind(Lisp_Object unw)
{
	lt_dlhandle SXE_UNUSED(mod_handle) = get_dynacat(unw);

	/* if the handle's still vivid, drown him */
	return Qnil;
}


DEFUN("load-module-file", Fload_module_file, 1, 3, "FLoad dynamic module: ", /*
Load in a C Emacs Extension module named FILE.
The optional NAME and VERSION are used to identify specific modules.

This function is similar in intent to `load-file' except that it loads
in pre-compiled C or C++ code, using dynamic shared objects.  If NAME
is specified, then the module is only loaded if its internal name
matches the NAME specified.  If VERSION is specified, then the module
is only loaded if it matches that VERSION.  This function will check
to make sure that the same module is not loaded twice.  Modules are
searched for in the same way as Lisp files, except for the file
extension.  For a list of valid extensions, see: `module-extensions'

All symbols in the shared module must be completely resolved in order
for this function to be successful.  Any modules which the specified
FILE depends on will be automatically loaded.  You can determine which
modules have been loaded as dynamic shared objects by examining the
return value of the function `list-modules'.

It is possible, although unwise, to unload modules using `unload-module'.
The preferred mechanism for unloading or reloading modules is to quit
SXEmacs, and then reload those new or changed modules that are required.

Messages informing you of the progress of the load are displayed unless
the variable `load-modules-quietly' is non-NIL.
*/
      (file, unused1, unused2))
{
/* not mt-safe */
	const char *mod_name;
	int speccount = specpdl_depth();
	struct dynacat_s unw = {.ptr = NULL};
	Lisp_Object lunw = (Lisp_Object)(long)&unw;
	emodng_t result;

	CHECK_STRING(file);
	set_lheader_implementation(&unw.lheader, &lrecord_dynacat);
	file = Fexpand_file_name(file, Qnil);
	mod_name = (const char*)XSTRING_DATA(file);

	record_unwind_protect(emodng_load_unwind, lunw);
	result = emodng_load(mod_name, meself);
	unbind_to(speccount, Qnil);

	if (result) {
		_register_endpoint(result);
		return Qt;
	}
	return Qnil;
}

DEFUN("unload-module", Funload_module, 1, 1, 0, /*
BAD JU-JU! Attempt to unload the emodule named FOO.

This is a dangerous operation and you should think twice before doing
it.  Unloading an emodule can often result in a non-responsive,
non-working, or just plain dead SXEmacs session.  It is simply not
worth the risk.  Save your buffers and restart SXEmacs, it really is
the safest way.

One last little gotcha... FOO is the _internal_ name of the emodule.
The internal name is listed in `list-modules'.
*/
      (foo))
{
/* not mt-safe */
	const char *mod_name;
	int speccount = specpdl_depth();
	struct dynacat_s unw = {.ptr = NULL};
	Lisp_Object lunw = (Lisp_Object)(long)&unw;
	bool result;

	CHECK_STRING(foo);
	set_lheader_implementation(&unw.lheader, &lrecord_dynacat);
	mod_name = (const char*)XSTRING_DATA(foo);

	record_unwind_protect(emodng_unload_unwind, lunw);
	result = emodng_unload(mod_name);
	unbind_to(speccount, Qnil);

	return result ? Qt : Qnil;
}

DEFUN("list-loaded-modules", Flist_loaded_modules, 0, 0, 0,	/*
Return a list of loaded modules.
*/
      ())
{
	Lisp_Object mlist = Qnil;

	for (emodng_t e = emods; e; e = e->next) {
		mlist = Fcons(build_string(e->truename), mlist);
	}
	return mlist;
}

void
emodng_doc_subr(const char *symname, const char *doc)
{
	Bytecount len = xstrlen(symname);
	Lisp_Object sym = oblookup(Vobarray, (const Bufbyte *)symname, len);
	Lisp_Subr *subr;

	if (SYMBOLP(sym) && SUBRP(XSYMBOL(sym)->function)) {
		EMOD_DEBUG_LOADER("trying to install docs for #'%s\n", symname);
		subr = XSUBR(XSYMBOL(sym)->function);
#if 1
		subr->doc = xstrdup(doc);
#else
		subr->doc = doc;
#endif
	} else if (!SUBRP(sym)) {
		EMOD_CRITICAL("Bollocks! #'%s is not a subr\n", symname);
	} else if (!SYMBOLP(sym)) {
		EMOD_CRITICAL("Bloody 'ell, #'%s is not even a symbol\n",
			      symname);
	}
	/*
	 * FIXME: I wish there was some way to avoid the xstrdup(). Is it
	 * possible to just set a pointer to the string, or somehow create a
	 * symbol whose value we can point to the constant string? Can someone
	 * look into this?
	 */
	return;
}

void
emodng_doc_sym(const char *symname, const char *doc)
{
	Bytecount len = xstrlen(symname);
	Lisp_Object sym = oblookup(Vobarray, (const Bufbyte *)symname, len);
	Lisp_Object docstr;
	struct gcpro gcpro1;

	EMOD_DEBUG_LOADER("trying to install docs for '%s\n", symname);

	if (SYMBOLP(sym)) {
		docstr = build_string(doc);
		GCPRO1(docstr);
		Fput(sym, Qvariable_documentation, docstr);
		UNGCPRO;
	} else if (!SYMBOLP(sym)) {
		EMOD_CRITICAL("wrong doc specification\n");
	}
	return;
}

#if defined USE_LTDL_SEARCHPATH
static int
_adapt_load_path(Lisp_Object sym, Lisp_Object *val,
		 Lisp_Object in_object, int flags)
{
	Lisp_Object ls;
	size_t len = 0;

	/* traverse once to obtain the overall length */
	for (len = 0, ls = *val; CONSP(ls); ls = XCDR(ls)) {
		if (LIKELY((STRINGP(XCAR(ls))))) {
			len += XSTRING_LENGTH(XCAR(ls)) + /*for colon*/1;
		}
	}
	/* C99 we need ya! traverse to fill the searchpath */
	if (LIKELY(len > 0)) {
		char sp[len], *p = sp;

		for (ls = *val; CONSP(ls); ls = XCDR(ls)) {
			Lisp_Object lse = XCAR(ls);
			if (LIKELY((STRINGP(lse)))) {
				p = xstpncpy(p,
					     (const char*)XSTRING_DATA(lse),
					     sp+sizeof(sp)-p-1);
				*p++ = ':';
			}
		}
		*--p = '\0';
		lt_dlsetsearchpath(sp);
	} else {
		lt_dlsetsearchpath(NULL);
	}
	EMOD_DEBUG_LOADER("load path %s\n", lt_dlgetsearchpath());
	return 0;
}
#endif

void
syms_of_emodng(void)
{
	DEFSUBR(Fload_module_file);
	DEFSUBR(Funload_module);
	DEFSUBR(Flist_loaded_modules);
}

void
reinit_vars_of_emodng(void)
{
#if 0
	meself = lt_dlopen(NULL);

	EMOD_DEBUG_LOADER("load path %s\n", lt_dlgetsearchpath());
#ifdef LTDL_SYSSEARCHPATH
	EMOD_DEBUG_LOADER("sys path %s\n", LTDL_SYSSEARCHPATH);
#endif
#endif
}

void
vars_of_emodng(void)
{
	DEFVAR_BOOL("load-modules-quietly", &load_modules_quietly	/*
*Set to t if module loading is to be silent.

Normally, when loading dynamic modules, Emacs will inform you of its
progress, and will display the module name and version if the module
is loaded correctly.  Setting this variable to `t' will suppress these
messages.  This would normally only be done if `load-module' was being
called by a Lisp function.
									 */ );
	load_modules_quietly = 0;

#if defined USE_LTDL_SEARCHPATH
	DEFVAR_LISP_MAGIC("module-load-path", &Vmodule_load_path	/*
*List of directories to search for dynamic modules to load.
Each element is a string (directory name) or nil (try default directory).

Note that elements of this list *may not* begin with "~", so you must
call `expand-file-name' on them before adding them to this list.

Initialized based on EMACSMODULEPATH environment variable, if any, otherwise
to default specified the file `paths.h' when SXEmacs was built.  If there
were no paths specified in `paths.h', then SXEmacs chooses a default
value for this variable by looking around in the file-system near the
directory in which the SXEmacs executable resides.

Due to the nature of dynamic modules, the path names should almost always
refer to architecture-dependent directories.  It is unwise to attempt to
store dynamic modules in a heterogenous environment.  Some environments
are similar enough to each other that SXEmacs will be unable to determine
the correctness of a dynamic module, which can have unpredictable results
when a dynamic module is loaded.
								*/,
		_adapt_load_path);
#else  /* !USE_LTDL_SEARCHPATH */
	DEFVAR_LISP("module-load-path", &Vmodule_load_path	/*
*List of directories to search for dynamic modules to load.
Each element is a string (directory name) or nil (try default directory).

Note that elements of this list *may not* begin with "~", so you must
call `expand-file-name' on them before adding them to this list.

Initialized based on EMACSMODULEPATH environment variable, if any, otherwise
to default specified the file `paths.h' when SXEmacs was built.  If there
were no paths specified in `paths.h', then SXEmacs chooses a default
value for this variable by looking around in the file-system near the
directory in which the SXEmacs executable resides.

Due to the nature of dynamic modules, the path names should almost always
refer to architecture-dependent directories.  It is unwise to attempt to
store dynamic modules in a heterogenous environment.  Some environments
are similar enough to each other that SXEmacs will be unable to determine
the correctness of a dynamic module, which can have unpredictable results
when a dynamic module is loaded.
								*/ );
#endif
	Vmodule_load_path = Qnil;

	DEFVAR_LISP("module-extensions", &Vmodule_extensions /*
*List of filename extensions to use when searching for dynamic modules.
*/);
	Vmodule_extensions =
		list5(build_string(".la"),
		      build_string(".so"),
		      build_string(".ell"),
		      build_string(".dll"),
		      build_string(".dylib"));

	Fprovide(intern("modules"));

	reinit_vars_of_emodng();
}

/* emodules-ng.c ends here */
