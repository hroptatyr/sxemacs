/*
 * Very simple sample module. Illustrates most of the salient features
 * of Emacs dynamic modules.
 * (C) Copyright 1998, 1999 J. Kean Johnston. All rights reserved.
 */

#include <emodules.h>

/*
 * This sample introduces three new Lisp objects to the Lisp reader.
 * The first, a simple boolean value, and the second a string. The
 * Third is a sample function that simply prints a message.
 */
int sample_bool;
Lisp_Object Vsample_string;

DEFUN ("sample-function", Fsample_function, 0, 0, "", /*
This is a sample function loaded dynamically.

You will notice in the source code for this module that the
declaration is identical to internal Emacs functions.  This
makes it possible to use the exact same code in a dumped
version of Emacs.
*/
        ())
{
  message ("Eureka! It worked");
  return Qt;
}

/*
 * Each dynamically loaded Emacs module is given a name at compile
 * time. This is a short name, and must be a valid part of a C
 * identifier.  This name is used to construct the name of several
 * functions which must appear in the module source code.
 * The first such function, modules_of_XXXX, should load in any dependent
 * modules. This function is optional, and the module will still load if
 * it is not present in the module.
 *
 * The second function, which is NOT optional, is syms_of_XXXX, in which
 * all functions that the module will be provided are declared. This
 * function will contain calls to DEFSUBR().
 *
 * The third function, which is also NOT optional, is vars_of_XXXX, in
 * which you declare all variables that the module provides. This
 * function will contain calls to DEFVAR_LISP(), DEFVAR_BOOL() etc.
 *
 * When declaring functions and variables in the syms_of_XXXX and
 * vars_of_XXXX functions, you use the exact same syntax that you
 * would as if this module were being compiled into the pure Emacs.
 *
 * All three of these functions are declared as void functions,
 * taking no parameters. Since this sample module is called 'sample',
 * the functions will be named 'modules_of_sample', 'syms_of_sample'
 * and 'vars_of_sample'.
 */

void
modules_of_sample()
{
  /*
   * This function isn't actually required as we will not be loading
   * in any dependent modules, but if we were, we would do something like:
   * emodules_load ("dependent.ell", "sample2", "1.0.0");
   */
}

void
syms_of_sample()
{
  DEFSUBR(Fsample_function);
}

void
vars_of_sample()
{
  DEFVAR_LISP ("sample-string", &Vsample_string /*
This is a sample string, declared in a dynamic module.

The syntax and conventions used for all normal Emacs variables
apply equally to modules, using an identical syntax.
*/ );

  DEFVAR_BOOL ("sample-boolean", &sample_bool /*
*Sample boolean value, in a dynamic module.

This is a user-settable variable, as indicated by the *
as the first character of the description. Declared in
a module exactly as it would be internally in Emacs.
*/ );
}

