/*
Make a symbolic link to ${blddir}/src/config.h in this directory and do:

gcc -shared -fPIC -Demacs -DHAVE_CONFIG_H -I. \
		-I/home/xemacs/xemacs-20.0/src -g dltest.c -o dltest
*/

# include <config.h>
# include "lisp.h"

Lisp_Object Qdltest_counter;

DEFUN("dltest", Fdltest, 0, 0, 0,	/*
					   Simple test function.
					 */
      ())
{
	Qdltest_counter = make_int(XINT(Qdltest_counter) + 1);

	return Qdltest_counter;
}

void vars_of()
{
	DEFVAR_LISP("dltest-counter", &Qdltest_counter	/*
							   counter.
							 */ );

	printf("Ten = %d\n", 10);

	Qdltest_counter = make_int(10);
}

void syms_of()
{
	DEFSUBR(Fdltest);
}
