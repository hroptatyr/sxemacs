/* emodules.c - Support routines for dynamic module loading
(C) Copyright 1998, 1999 J. Kean Johnston. All rights reserved.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "emodules.h"
#include "sysdll.h"

#ifdef HAVE_SHLIB

/* CE-Emacs version number */
Lisp_Object Vmodule_version;

/* Do we do our work quietly? */
int load_modules_quietly;

/* Load path */
Lisp_Object Vmodule_load_path;

typedef struct _emodules_list
{
  int used;             /* Is this slot used?                           */
  char *soname;         /* Name of the shared object loaded (full path) */
  char *modname;        /* The name of the module                       */
  char *modver;         /* The version that the module is at            */
  char *modtitle;       /* How the module announces itself              */
  dll_handle dlhandle;  /* Dynamic lib handle                           */
} emodules_list;

static Lisp_Object Vmodule_extensions;

static int emodules_depth;
static dll_handle dlhandle;
static emodules_list *modules;
static int modnum;

static int find_make_module (const char *mod, const char *name, const char *ver, int make_or_find);
static Lisp_Object module_load_unwind (Lisp_Object);
static void attempt_module_delete (int mod);

DEFUN ("load-module", Fload_module, 1, 3, "FLoad dynamic module: ", /*
Load in a C Emacs Extension module named FILE.
The optional NAME and VERSION are used to identify specific modules.

This function is similar in intent to `load' except that it loads in
pre-compiled C or C++ code, using dynamic shared objects.  If NAME is
specified, then the module is only loaded if its internal name matches
the NAME specified.  If VERSION is specified, then the module is only
loaded if it matches that VERSION.  This function will check to make
sure that the same module is not loaded twice.  Modules are searched
for in the same way as Lisp files, except that the valid file
extensions are `.so', `.dll' or `.ell'.

All symbols in the shared module must be completely resolved in order
for this function to be successful.  Any modules which the specified
FILE depends on will be automatically loaded.  You can determine which
modules have been loaded as dynamic shared objects by examining the
return value of the function `list-modules'.

It is possible, although unwise, to unload modules using `unload-module'.
The preferred mechanism for unloading or reloading modules is to quit
XEmacs, and then reload those new or changed modules that are required.

Messages informing you of the progress of the load are displayed unless
the variable `load-modules-quietly' is non-NIL.
*/
       (file, name, version))
{
  char *mod, *mname, *mver;
  int speccount = specpdl_depth();

  CHECK_STRING(file);

  mod = (char *)XSTRING_DATA (file);

  if (NILP (name))
    mname = "";
  else
    mname = (char *)XSTRING_DATA (name);

  if (NILP (version))
    mver = "";
  else
    mver = (char *)XSTRING_DATA (version);

  dlhandle = 0;
  record_unwind_protect (module_load_unwind, make_int(modnum));
  emodules_load (mod, mname, mver);
  unbind_to (speccount, Qnil);

  return Qt;
}

#ifdef DANGEROUS_NASTY_SCARY_MONSTER

DEFUN ("unload-module", Fmodule_unload, 1, 3, 0, /*
Unload a module previously loaded with load-module.

As with load-module, this function requires at least the module FILE, and
optionally the module NAME and VERSION to unload.  It may not be possible
for the module to be unloaded from memory, as there may be Lisp objects
referring to variables inside the module code.  However, once you have
requested a module to be unloaded, it will be unloaded from memory as
soon as the last reference to symbols within the module is destroyed.
*/
       (file, name, version))
{
  int x;
  char *mod, *mname, *mver;

  CHECK_STRING(file);

  mod = (char *)XSTRING_DATA (file);

  if (NILP (name))
    mname = "";
  else
    mname = (char *)XSTRING_DATA (name);

  if (NILP (version))
    mver = "";
  else
    mver = (char *)XSTRING_DATA (version);

  x = find_make_module (mod, mname, mver, 1);
  if (x != -1)
    attempt_module_delete (x);
  return Qt;
}
#endif /* DANGEROUS_NASTY_SCARY_MONSTER */

DEFUN ("list-modules", Flist_modules, 0, 0, "", /*
Produce a list of loaded dynamic modules.

This function will return a list of all the loaded dynamic modules.
Each element in the list is a list in the form (SONAME NAME VER DESC),
where SONAME is the name of the shared object that was loaded, NAME
is the internal module name, VER is the version of the module, and DESC
is how the module describes itself.

This function returns a list, so you will need to assign the return value
to a variable and then examine the variable with `describe-variable'.
For example:

  (setq mylist (list-modules))
  (describe-variable 'mylist)


NOTE: It is possible for the same module to be loaded more than once,
at different versions.  However, you should never see the same module,
with the same name and version, loaded more than once.  If you do, this
is a bug, and you are encouraged to report it.
*/
       ())
{
  Lisp_Object mlist = Qnil;
  int i;

  for (i = 0; i < modnum; i++)
    {
      if (modules[i].used == 1)
        mlist = Fcons (list4 (build_string (modules[i].soname),
                              build_string (modules[i].modname),
                              build_string (modules[i].modver),
                              build_string (modules[i].modtitle)), mlist);
    }

  return mlist;
}

static int
find_make_module (const char *mod, const char *name, const char *ver, int mof)
{
  int i, fs = -1;

  for (i = 0; i < modnum; i++)
    {
      if (fs == -1 && modules[i].used == 0)
        fs = i;
      if (strcmp (modules[i].soname, mod) == 0)
        {
          if (name && name[0] && strcmp (modules[i].modname, name))
            continue;
          if (ver && ver[0] && strcmp (modules[i].modver, ver))
            continue;
          return i; /* Found a match */
        }
    }

  if (mof)
    return fs;

  if (fs != -1)
    return fs; /* First free slot */

  /*
   * We only get here if we haven't found a free slot and the module was
   * not previously loaded.
   */
  if (modules == (emodules_list *)0)
    modules = (emodules_list *) xmalloc (sizeof (emodules_list));
  modnum++;
  modules = (emodules_list *) xrealloc (modules, modnum * sizeof (emodules_list));

  fs = modnum - 1;
  memset (&modules[fs], 0, sizeof(emodules_list));
  return fs;
}

static void
attempt_module_delete (int mod)
{
  if (dll_close (modules[mod].dlhandle) == 0)
    {
      xfree (modules[mod].soname);
      xfree (modules[mod].modname);
      xfree (modules[mod].modver);
      xfree (modules[mod].modtitle);
      modules[mod].dlhandle = 0;
      modules[mod].used = 0;
    }
  else if (modules[mod].used > 1)
    modules[mod].used = 1; /* We couldn't delete it - it stays */
}

static Lisp_Object
module_load_unwind (Lisp_Object upto)
{
  int x,l=0;

  /*
   * First close off the current handle if it is open.
   */
  if (dlhandle != 0)
    dll_close (dlhandle);
  dlhandle = 0;

  if (CONSP (upto))
    {
      if (INTP (XCAR (upto)))
        l = XINT (XCAR (upto));
      free_cons (XCONS (upto));
    }
  else
    l = XINT (upto);

  /*
   * Here we need to go through and dlclose() (IN REVERSE ORDER!) any
   * modules that were loaded as part of this load chain. We only mark
   * the slots as closed if the dlclose() succeeds.
   */
  for (x = modnum-1; x >= l; x--)
    {
      if (modules[x].used > 1)
        attempt_module_delete (x);
    }
  emodules_depth = 0;

  return Qnil;
}

/*
 * Do the actual grunt-work of loading in a module. We first try and
 * dlopen() the module. If that fails, we have an error and we bail
 * out immediately. If the dlopen() succeeds, we need to check for the
 * existence of certain special symbols.
 *
 * All modules will have complete access to the variables and functions
 * defined within XEmacs itself.  It is up to the module to declare any
 * variables or functions it uses, however.  Modules will also have access
 * to other functions and variables in other loaded modules, unless they
 * are defined as STATIC.
 *
 * We need to be very careful with how we load modules. If we encounter an
 * error along the way, we need to back out completely to the point at
 * which the user started. Since we can be called recursively, we need to
 * take care with marking modules as loaded. When we first start loading
 * modules, we set the counter to zero. As we enter the function each time,
 * we increment the counter, and before we leave we decrement it. When
 * we get back down to 0, we know we are at the end of the chain and we
 * can mark all the modules in the list as loaded.
 *
 * When we signal an error, we need to be sure to unwind all modules loaded
 * thus far (but only for this module chain). It is assumed that if any
 * modules in a chain fail, then they all do. This is logical, considering
 * that the only time we recurse is when we have dependent modules. So in
 * the error handler we take great care to close off the module chain before
 * we call "error" and let the Fmodule_load unwind_protect() function handle
 * the cleaning up.
 */
void
emodules_load(const char *module, const char *modname, const char *modver)
{
  Lisp_Object filename;
  Lisp_Object foundname;
  int fd, x, mpx;
  char *soname, *tmod;
  const char **f;
  const long *ellcc_rev;
  char *mver, *mname, *mtitle, *symname;
  void (*modload)(void) = 0;
  void (*modsyms)(void) = 0;
  void (*modvars)(void) = 0;
  void (*moddocs)(void) = 0;
  emodules_list *mp;
  struct gcpro gcpro1,gcpro2;

  filename = Qnil;
  foundname = Qnil;

  emodules_depth++;
  dlhandle = 0;

  if ((module == (const char *)0) || (module[0] == '\0'))
    error ("Empty module name");

  /* This is to get around the fact that build_string() is not declared
     as taking a const char * as an argument. I HATE compiler warnings. */
  tmod = (char *)alloca (strlen (module) + 1);
  strcpy (tmod, module);

  GCPRO2(filename, foundname);
  filename = build_string (tmod);
  fd = locate_file(Vmodule_load_path, filename, Vmodule_extensions,
		   &foundname, -1);
  UNGCPRO;

  if (fd < 0)
    signal_simple_error ("Cannot open dynamic module", filename);

  soname = (char *)alloca (XSTRING_LENGTH (foundname) + 1);
  strcpy (soname, (char *)XSTRING_DATA (foundname));

  dlhandle = dll_open (soname);
  if (dlhandle == (dll_handle)0)
    error ("Opening dynamic module: %s", dll_error (dlhandle));

  ellcc_rev = (const long *)dll_variable (dlhandle, "emodule_compiler");
  if ((ellcc_rev == (const long *)0) || (*ellcc_rev <= 0))
    error ("Missing symbol `emodule_compiler': Invalid dynamic module");
  if (*ellcc_rev > EMODULES_REVISION)
    error ("Unsupported version `%ld(%ld)': Invalid dynamic module",
           *ellcc_rev, EMODULES_REVISION);

  f = (const char **)dll_variable (dlhandle, "emodule_name");
  if ((f == (const char **)0) || (*f == (const char *)0))
    error ("Missing symbol `emodule_name': Invalid dynamic module");

  mname = (char *)alloca (strlen (*f) + 1);
  strcpy (mname, *f);
  if (mname[0] == '\0')
    error ("Empty value for `emodule_name': Invalid dynamic module");

  f = (const char **)dll_variable (dlhandle, "emodule_version");
  if ((f == (const char **)0) || (*f == (const char *)0))
    error ("Missing symbol `emodule_version': Invalid dynamic module");

  mver = (char *)alloca (strlen (*f) + 1);
  strcpy (mver, *f);

  f = (const char **)dll_variable (dlhandle, "emodule_title");
  if ((f == (const char **)0) || (*f == (const char *)0))
    error ("Missing symbol `emodule_title': Invalid dynamic module");

  mtitle = (char *)alloca (strlen (*f) + 1);
  strcpy (mtitle, *f);

  symname = (char *)alloca (strlen (mname) + 15);

  strcpy (symname, "modules_of_");
  strcat (symname, mname);
  modload = (void (*)(void))dll_function (dlhandle, symname);
  /*
   * modload is optional. If the module doesn't require other modules it can
   * be left out.
   */

  strcpy (symname, "syms_of_");
  strcat (symname, mname);
  modsyms = (void (*)(void))dll_function (dlhandle, symname);
  if (modsyms == (void (*)(void))0)
    error ("Missing symbol `%s': Invalid dynamic module", symname);

  strcpy (symname, "vars_of_");
  strcat (symname, mname);
  modvars = (void (*)(void))dll_function (dlhandle, symname);
  if (modvars == (void (*)(void))0)
    error ("Missing symbol `%s': Invalid dynamic module", symname);

  strcpy (symname, "docs_of_");
  strcat (symname, mname);
  moddocs = (void (*)(void))dll_function (dlhandle, symname);
  if (moddocs == (void (*)(void))0)
    error ("Missing symbol `%s': Invalid dynamic module", symname);

  if (modname && modname[0] && strcmp (modname, mname))
    error ("Module name mismatch");

  if (modver && modver[0] && strcmp (modver, mver))
    error ("Module version mismatch");

  /*
   * Attempt to make a new slot for this module. If this really is the
   * first time we are loading this module, the used member will be 0.
   * If that is non-zero, we know that we have a previously loaded module
   * of the same name and version, and we don't need to go any further.
   */
  mpx = find_make_module (soname, mname, mver, 0);
  mp = &modules[mpx];
  if (mp->used > 0)
    {
      emodules_depth--;
      dll_close (dlhandle);
      dlhandle = 0;  /* Zero this out before module_load_unwind runs */
      return;
    }

  if (!load_modules_quietly)
    message ("Loading %s v%s (%s)", mname, mver, mtitle);

  /*
   * We have passed the basic initialization, and can now add this
   * module to the list of modules.
   */
  mp->used = emodules_depth + 1;
  mp->soname = xstrdup (soname);
  mp->modname = xstrdup (mname);
  mp->modver = xstrdup (mver);
  mp->modtitle = xstrdup (mtitle);
  mp->dlhandle = dlhandle;
  dlhandle = 0;

  /*
   * Now we need to call the module init function and perform the various
   * startup tasks.
   */
  if (modload != 0)
    (*modload)();

  /*
   * Now we can get the module to initialize its symbols, and then its
   * variables, and lastly the documentation strings.
   */
  (*modsyms)();
  (*modvars)();
  (*moddocs)();

  if (!load_modules_quietly)
    message ("Loaded module %s v%s (%s)", mname, mver, mtitle);


  emodules_depth--;
  if (emodules_depth == 0)
    {
      /*
       * We have reached the end of the load chain. We now go through the
       * list of loaded modules and mark all the valid modules as just
       * that.
       */
      for (x = 0; x < modnum; x++)
        if (modules[x].used > 1)
          modules[x].used = 1;
    }
}

void
emodules_doc_subr(const char *symname, const char *doc)
{
  Bytecount len = strlen (symname);
  Lisp_Object sym = oblookup (Vobarray, (const Bufbyte *)symname, len);
  Lisp_Subr *subr;

  if (SYMBOLP(sym))
    {
      subr = XSUBR( XSYMBOL(sym)->function);
      subr->doc = xstrdup (doc);
    }
  /*
   * FIXME: I wish there was some way to avoid the xstrdup(). Is it
   * possible to just set a pointer to the string, or somehow create a
   * symbol whose value we can point to the constant string? Can someone
   * look into this?
   */
}

void
emodules_doc_sym (const char *symname, const char *doc)
{
  Bytecount len = strlen (symname);
  Lisp_Object sym = oblookup (Vobarray, (const Bufbyte *)symname, len);
  Lisp_Object docstr;
  struct gcpro gcpro1;

  if (SYMBOLP(sym))
    {
      docstr = build_string (doc);
      GCPRO1(docstr);
      Fput (sym, Qvariable_documentation, docstr);
      UNGCPRO;
    }
}


void
syms_of_module (void)
{
  DEFSUBR(Fload_module);
  DEFSUBR(Flist_modules);
#ifdef DANGEROUS_NASTY_SCARY_MONSTER
  DEFSUBR(Funload_module);
#endif
}

void
reinit_vars_of_module (void)
{
  emodules_depth = 0;
  modules = (emodules_list *)0;
  modnum = 0;
}

void
vars_of_module (void)
{
  reinit_vars_of_module ();

  DEFVAR_LISP ("module-version", &Vmodule_version /*
Emacs dynamic loading mechanism version, as a string.

This string is in the form XX.YY.ppp, where XX is the major version
number, YY is the minor version number, and ppp is the patch level.
This variable can be used to distinguish between different versions of
the dynamic loading technology used in Emacs, if required.  It is not
a given that this value will be the same as the Emacs version number.
*/ );
  Vmodule_version = build_string (EMODULES_VERSION);

  DEFVAR_BOOL ("load-modules-quietly", &load_modules_quietly /*
*Set to t if module loading is to be silent.

Normally, when loading dynamic modules, Emacs will inform you of its
progress, and will display the module name and version if the module
is loaded correctly.  Setting this variable to `t' will suppress these
messages.  This would normally only be done if `load-module' was being
called by a Lisp function.
*/);

  DEFVAR_LISP ("module-load-path", &Vmodule_load_path /*
*List of directories to search for dynamic modules to load.
Each element is a string (directory name) or nil (try default directory).

Note that elements of this list *may not* begin with "~", so you must
call `expand-file-name' on them before adding them to this list.

Initialized based on EMACSMODULEPATH environment variable, if any, otherwise
to default specified the file `paths.h' when XEmacs was built.  If there
were no paths specified in `paths.h', then XEmacs chooses a default
value for this variable by looking around in the file-system near the
directory in which the XEmacs executable resides.

Due to the nature of dynamic modules, the path names should almost always
refer to architecture-dependent directories.  It is unwise to attempt to
store dynamic modules in a heterogenous environment.  Some environments
are similar enough to each other that XEmacs will be unable to determine
the correctness of a dynamic module, which can have unpredictable results
when a dynamic module is loaded.
*/);

  /* #### Export this to Lisp */
  Vmodule_extensions = build_string (":.ell:.so:.dll:.dylib");
  staticpro (&Vmodule_extensions);

  load_modules_quietly = 0;
  Vmodule_load_path = Qnil;
  Fprovide (intern ("modules"));
}

#endif /* HAVE_SHLIB */

