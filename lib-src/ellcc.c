/* ellcc.c - front-end for compiling Emacs modules
Copyright (C) 1998, 1999 J. Kean Johnston.

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
Boston, MA 02111-1307, USA.

Author: J. Kean Johnston (jkj@sco.com).
Please mail bugs and suggestions to the XEmacs maintainer.
*/

/*
Here's the scoop. We would really like this to be a shell script, but
the various Windows platforms don't have reliable scripting that suits
our needs. We don't want to rely on perl or some other such language
so we have to roll our own executable to act as a front-end for the
compiler.

This program is used to invoke the compiler, the linker and to generate
the module specific documentation and initialization code.  We assume we
are in 'compile' mode unless we encounter an argument which tells us
that we're not.  We take all arguments and pass them on directly to the
compiler, except for a few which are specific to this program:

  --mode=VALUE      This sets the program mode. VALUE can be one of
                    compile, link, init or verbose.
  --mod-name=NAME   Sets the module name to the string NAME.
  --mod-title=TITLE Sets the module title to the string TITLE.
  --mod-version=VER Sets the module version to the string VER.

The idea is that Makefiles will use ellcc as the compiler for making
dynamic Emacs modules, and life should be as simple as:

  make CC=ellcc LD='ellcc --mode=link'

The only additional requirement is an entry in the Makefile to produce
the module initialization file, which will usually be something along
the lines of:

  modinit.c: $(SRCS)
             ellcc --mode=init --mod-name=\"$(MODNAME)\" \
               --mod-title=\"$(MODTITLE)\" --mod-version=\"$(MODVERSION)\" \
               -o $@ $(SRCS)

See the samples for more details.
*/

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif /* HAVE_UNISTD_H */

#define EMODULES_GATHER_VERSION

#include <emodules.h>
#include <ellcc.h> /* Generated files must be included using <...> */

#define DEBUG

#ifndef HAVE_SHLIB
int
main (int argc, char *argv[])
{
  fprintf (stderr, "Dynamic modules not supported on this platform\n");
  return EXIT_FAILURE;
}
#else

/*
 * Try to figure out the commands we need to use to create shared objects,
 * and how to compile for PIC mode.
 */

/*
 *	xnew, xrnew -- allocate, reallocate storage
 *
 * SYNOPSIS:	Type *xnew (int n, Type);
 *		Type *xrnew (OldPointer, int n, Type);
 */
#ifdef chkmalloc
# include "chkmalloc.h"
# define xnew(n,Type)	  ((Type *) trace_malloc (__FILE__, __LINE__, \
						  (n) * sizeof (Type)))
# define xrnew(op,n,Type) ((Type *) trace_realloc (__FILE__, __LINE__, \
						   (op), (n) * sizeof (Type)))
#else
# define xnew(n,Type)	  ((Type *) xmalloc ((n) * sizeof (Type)))
# define xrnew(op,n,Type) ((Type *) xrealloc ((op), (n) * sizeof (Type)))
#endif
static void *xmalloc (size_t);
static void fatal (char *, char *);
static void add_to_argv (const char *);
static void do_compile_mode (void);
static void do_link_mode (void);
static void do_init_mode (void);

#define SSTR(S) ((S)?(S):"")

#define ELLCC_COMPILE_MODE      0
#define ELLCC_LINK_MODE         1
#define ELLCC_INIT_MODE         2

static int ellcc_mode = ELLCC_COMPILE_MODE;
static char *progname;
static char *mod_name = NULL;
static char *mod_version = NULL;
static char *mod_title = NULL;
static char *mod_output = NULL;
static int verbose = 0;
static char **exec_argv;
static int exec_argc = 1;
static int *exec_args;
static int real_argc = 0;
static int prog_argc;
static char **prog_argv;

/*
 * We allow the user to over-ride things in the environment
 */
char *ellcc, *ellld, *ellcflags, *ellldflags, *ellpicflags, *elldllflags;
#define OVERENV(STR,EVAR,DFLT) \
  STR = getenv(EVAR); \
  if ((STR) == (char *)0) \
    STR = DFLT

int
main (int argc, char *argv[])
{
  char *tmp;
  int i, done_mode = 0;

  prog_argc = argc;
  prog_argv = argv;

#if defined(WIN32_NATIVE)
  tmp = strrchr (argv[0], '\\');
  if (tmp != (char *)0)
    tmp++;
#elif !defined (VMS)
  tmp = strrchr (argv[0], '/');
  if (tmp != (char *)0)
    tmp++;
#else
  tmp = argv[0];
#endif

  if (tmp != (char *)0)
    progname = tmp;
  else
    progname = argv[0];

  tmp = &progname[strlen(progname)-2];
  if (strcmp (tmp, "cc") == 0)
    ellcc_mode = ELLCC_COMPILE_MODE;
  else if (strcmp (tmp, "ld") == 0)
    ellcc_mode = ELLCC_LINK_MODE;
  else if (strcmp (tmp, "it") == 0)
    ellcc_mode = ELLCC_INIT_MODE;

  exec_argv = xnew(argc + 20, char *);
  exec_args = xnew(argc, int);
  for (i = 0; i < argc; i++)
    exec_args[i] = -1;

  if (argc < 2)
    fatal ("too few arguments", (char *)0);

  exec_args[0] = 0;

  for (i = 1; i < argc; i++)
    {
      if (strncmp (argv[i], "--mode=", 7) == 0)
        {
          char *modeopt = argv[i] + 7;

          if (done_mode && strcmp (modeopt, "verbose"))
            fatal ("more than one mode specified", (char *) 0);
          if (strcmp (modeopt, "link") == 0)
            {
              done_mode++;
              ellcc_mode = ELLCC_LINK_MODE;
            }
          else if (strcmp (modeopt, "compile") == 0)
            {
              done_mode++;
              ellcc_mode = ELLCC_COMPILE_MODE;
            }
          else if (strcmp (modeopt, "init") == 0)
            {
              done_mode++;
              ellcc_mode = ELLCC_INIT_MODE;
            }
          else if (strcmp (modeopt, "verbose") == 0)
            verbose += 1;
        }
      else if (strcmp (argv[i], "--mod-location") == 0)
        {
          printf ("%s\n", ELLCC_MODDIR);
          return 0;
        }
      else if (strcmp (argv[i], "--mod-site-location") == 0)
        {
          printf ("%s\n", ELLCC_SITEMODS);
          return 0;
        }
      else if (strcmp (argv[i], "--mod-archdir") == 0)
        {
          printf ("%s\n", ELLCC_ARCHDIR);
          return 0;
        }
      else if (strcmp (argv[i], "--mod-config") == 0)
        {
          printf ("%s\n", ELLCC_CONFIG);
          return 0;
        }
      else if (strncmp (argv[i], "--mod-name=", 11) == 0)
        mod_name = argv[i] + 11;
      else if (strncmp (argv[i], "--mod-title=", 12) == 0)
        mod_title = argv[i] + 12;
      else if (strncmp (argv[i], "--mod-version=", 14) == 0)
        mod_version = argv[i] + 14;
      else if (strncmp (argv[i], "--mod-output=", 13) == 0)
        mod_output = argv[i] + 13;
      else
        {
          exec_args[exec_argc] = i;
          exec_argc++;
        }
    }

  if (ellcc_mode == ELLCC_LINK_MODE && mod_output == (char *)0)
    fatal ("must specify --mod-output when linking", (char *)0);
  if (ellcc_mode == ELLCC_INIT_MODE && mod_output == (char *)0)
    fatal ("must specify --mod-output when creating init file", (char *)0);
  if (ellcc_mode == ELLCC_INIT_MODE && mod_name == (char *)0)
    fatal ("must specify --mod-name when creating init file", (char *)0);

  /*
   * We now have the list of arguments to pass to the compiler or
   * linker (or to process for doc files). We can do the real work
   * now.
   */
  if (verbose)
    printf ("ellcc driver version %s for EMODULES version %s (%ld)\n",
            ELLCC_EMACS_VER, EMODULES_VERSION, EMODULES_REVISION);
#ifdef DEBUG
  if (verbose >= 2)
    {
      printf ("              mode = %d (%s)\n", ellcc_mode,
              ellcc_mode == ELLCC_COMPILE_MODE ? "compile" :
              ellcc_mode == ELLCC_LINK_MODE ? "link" : "init");
      printf ("       module_name = \"%s\"\n", SSTR(mod_name));
      printf ("      module_title = \"%s\"\n", SSTR(mod_title));
      printf ("    module_version = \"%s\"\n", SSTR(mod_version));

      printf ("                CC = %s\n", ELLCC_CC);
      printf ("            CFLAGS = %s\n", ELLCC_CFLAGS);
      printf ("      CC PIC flags = %s\n", ELLCC_DLL_CFLAGS);
      printf ("                LD = %s\n", ELLCC_DLL_LD);
      printf ("           LDFLAGS = %s\n", ELLCC_DLL_LDFLAGS);
      printf ("      architecture = %s\n", ELLCC_CONFIG);
      printf (" Include directory = %s/include\n", ELLCC_ARCHDIR);
      printf ("\n");
    }
#endif

  if (exec_argc < 2)
    fatal ("too few arguments", (char *) 0);

  /*
   * Get the over-rides from the environment
   */
  OVERENV(ellcc, "ELLCC", ELLCC_CC);
  OVERENV(ellld, "ELLLD", ELLCC_DLL_LD);
  OVERENV(ellcflags, "ELLCFLAGS", ELLCC_CFLAGS);
  OVERENV(ellldflags, "ELLLDFLAGS", ELLCC_LDFLAGS);
  OVERENV(elldllflags, "ELLDLLFLAGS", ELLCC_DLL_LDFLAGS);
  OVERENV(ellpicflags, "ELLPICFLAGS", ELLCC_DLL_CFLAGS);

  if (ellcc_mode == ELLCC_COMPILE_MODE)
    do_compile_mode();
  else if (ellcc_mode == ELLCC_LINK_MODE)
    do_link_mode();
  else
    do_init_mode();

  /*
   * The arguments to pass on to the desired program have now been set
   * up and we can run the program.
   */
  if (verbose)
    {
      for (i = 0; i < real_argc; i++)
        printf ("%s ", exec_argv[i]);
      printf ("\n");
      fflush (stdout);
    }
  exec_argv[real_argc] = (char *)0; /* Terminate argument list */

  i = execvp (exec_argv[0], exec_argv);
  if (verbose)
    printf ("%s exited with status %d\n", exec_argv[0], i);
  return i;
}

/* Like malloc but get fatal error if memory is exhausted.  */
static void *
xmalloc (size_t size)
{
  void *result = malloc (size);
  if (result == NULL)
    fatal ("virtual memory exhausted", (char *)0);
  return result;
}

/* Print error message and exit.  */
static void
fatal (char *s1, char *s2)
{
  fprintf (stderr, "%s: ", progname);
  fprintf (stderr, s1, s2);
  fprintf (stderr, "\n");
  exit (EXIT_FAILURE);
}

/*
 * Add a string to the argument vector list that will be passed on down
 * to the compiler or linker. We need to split individual words into
 * arguments, taking quoting into account. This can get ugly.
 */
static void
add_to_argv (const char *str)
{
  int sm = 0;
  const char *s = (const char *)0;

  if ((str == (const char *)0) || (str[0] == '\0'))
    return;

  while (*str)
    {
      switch (sm)
        {
        case 0: /* Start of case - string leading whitespace */
          if (isspace ((unsigned char) *str))
            str++;
          else
            {
              sm = 1; /* Change state to non-whitespace */
              s = str; /* Mark the start of THIS argument */
            }
          break;

        case 1: /* Non-whitespace character. Mark the start */
          if (isspace ((unsigned char) *str))
            {
              /* Reached the end of the argument. Add it. */
              int l = str-s;
              exec_argv[real_argc] = xnew (l+2, char);
              strncpy (exec_argv[real_argc], s, l);
              exec_argv[real_argc][l] = '\0';
              real_argc++;
              sm = 0; /* Back to start state */
              s = (const char *)0;
              break;
            }
          else if (*str == '\\')
            {
              sm = 2; /* Escaped character */
              str++;
              break;
            }
          else if (*str == '\'')
            {
              /* Start of quoted string (single quotes) */
              sm = 3;
            }
          else if (*str == '"')
            {
              /* Start of quoted string (double quotes) */
              sm = 4;
            }
          else
            {
              /* This was just a normal character. Advance the pointer. */
              str++;
            }
          break;

        case 2: /* Escaped character */
          str++; /* Preserve the quoted character */
          sm = 1; /* Go back to gathering state */
          break;

        case 3: /* Inside single quoted string */
          if (*str == '\'')
            sm = 1;
          str++;
          break;

        case 4: /* inside double quoted string */
          if (*str == '"')
            sm = 1;
          str++;
          break;
        }
    }

  if (s != (const char *)0)
    {
      int l = str-s;
      exec_argv[real_argc] = xnew (l+2, char);
      strncpy (exec_argv[real_argc], s, l);
      exec_argv[real_argc][l] = '\0';
      real_argc++;
      s = (const char *)0;
    }
}

/*
 * For compile mode, things are pretty straight forward. All we need to do
 * is build up the argument vector and exec() it. We must just make sure
 * that we get all of the required arguments in place.
 */
static void
do_compile_mode (void)
{
  int i;
  char ts[4096]; /* Plenty big enough */

  add_to_argv (ellcc);
  add_to_argv (ellcflags);
  add_to_argv (ellpicflags);
  add_to_argv ("-DPIC");
  add_to_argv ("-DEMACS_MODULE");
#ifdef XEMACS
  add_to_argv ("-DXEMACS_MODULE"); /* Cover both cases */
  add_to_argv ("-Dxemacs");
#endif
  add_to_argv ("-Demacs");
  sprintf (ts, "-I%s/include", ELLCC_ARCHDIR);
  add_to_argv (ts);
  add_to_argv (ELLCC_CF_ALL);
  for (i = 1; i < exec_argc; i++)
    exec_argv[real_argc++] = strdup (prog_argv[exec_args[i]]);
}

/*
 * For link mode, things are a little bit more complicated. We need to
 * insert the linker commands first, replace any occurrence of ELLSONAME
 * with the desired output file name, insert the output arguments, then
 * all of the provided arguments, then the final post arguments. Once
 * all of this has been done, the argument vector is ready to run.
 */
static void
do_link_mode (void)
{
  int i,x;
  char *t, ts[4096]; /* Plenty big enough */

  add_to_argv (ellld);
  add_to_argv (ellldflags);
  add_to_argv (elldllflags);
  add_to_argv (ELLCC_DLL_LDO);
  add_to_argv (mod_output);
  for (i = 1; i < exec_argc; i++)
    exec_argv[real_argc++] = strdup (prog_argv[exec_args[i]]);
  add_to_argv (ELLCC_DLL_POST);

  /*
   * Now go through each argument and replace ELLSONAME with mod_output.
   */
  for (i = 0; i < real_argc; i++)
    {
      x = 0;
      ts[0] = '\0';

      t = exec_argv[i];
      while (*t)
        {
          if (*t == 'E')
            {
              if (strncmp (t, "ELLSONAME", 9) == 0)
                {
                  strcat (ts, mod_output);
                  t += 8;
                  x += strlen (mod_output);
                }
              else
                {
                  ts[x] = *t;
                  x++;
                  ts[x] = '\0';
                }
            }
          else
            {
              ts[x] = *t;
              x++;
              ts[x] = '\0';
            }
          t++;
        }
      free (exec_argv[i]);
      exec_argv[i] = strdup (ts);
    }
}

/*
 * In init mode, things are a bit easier. We assume that the only things
 * passed on the command line are the names of source files which the
 * make-doc program will be processing. We prepare the output file with
 * the header information first, as make-doc will append to the file by
 * special dispensation.
 */
static void
do_init_mode (void)
{
  int i;
  char ts[4096]; /* Plenty big enough */
  char *mdocprog;
  FILE *mout = fopen (mod_output, "w");

  if (mout == (FILE *)0)
    fatal ("failed to open output file", mod_output);
  fprintf (mout, "/* DO NOT EDIT - AUTOMATICALLY GENERATED */\n\n");
  fprintf (mout, "#include <emodules.h>\n\n");
  fprintf (mout, "const long emodule_compiler = %ld;\n", EMODULES_REVISION);
  fprintf (mout, "const char *emodule_name = \"%s\";\n", SSTR(mod_name));
  fprintf (mout, "const char *emodule_version = \"%s\";\n", SSTR(mod_version));
  fprintf (mout, "const char *emodule_title = \"%s\";\n", SSTR(mod_title));
  fprintf (mout, "\n\n");
  fprintf (mout, "void docs_of_%s()\n", SSTR(mod_name));
  fclose (mout);

  sprintf (ts, "%s/make-docfile", ELLCC_ARCHDIR);
  OVERENV(mdocprog, "ELLMAKEDOC", ts);
  add_to_argv (mdocprog);
  sprintf (ts, "-E %s", mod_output);
  add_to_argv (ts);
  for (i = 1; i < exec_argc; i++)
    exec_argv[real_argc++] = strdup (prog_argv[exec_args[i]]);
}

#endif /* HAVE_SHLIB */

