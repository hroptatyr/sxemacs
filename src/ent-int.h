/*
  ent-int.c -- Ordinary Integers for SXEmacs
  Copyright (C) 2005, 2006 Sebastian Freundt

  Author:  Sebastian Freundt

  * This file is part of SXEmacs.
  * 
  * SXEmacs is free software; you can redistribute it and/or modify it
  * under the terms of the GNU General Public License as published by the
  * Free Software Foundation; either version 2, or (at your option) any
  * later version.
  * 
  * SXEmacs is distributed in the hope that it will be useful, but WITHOUT
  * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  * for more details.
  * 
  * You should have received a copy of the GNU General Public License
  * along with SXEmacs; see the file COPYING.  If not, write to
  * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  * Boston, MA 02111-1307, USA.
  */

#ifndef INCLUDED_ent_int_h_
#define INCLUDED_ent_int_h_

#ifdef UNO
/* Uno complains about several inline functions that include conditions with
   assignments and side effects if we don't do this */
#undef __GNUC__
#endif

extern EMACS_INT Vmost_negative_int, Vmost_positive_int;
extern Lisp_Object Qzero, Qone;

#ifdef HAVE_MPZ
#define make_integer(x)							\
	(NUMBER_FITS_IN_AN_EMACS_INT(x) ? make_int(x) : make_bigz(x))
#else
#define make_integer(x) make_int(x)
#endif

extern Bufpos marker_position(Lisp_Object);
extern inline EMACS_INT ent_int(Lisp_Object);


extern inline EMACS_INT
ent_int(Lisp_Object number)
{
	if (!CHARP(number) && !MARKERP(number))
		return XINT(number);
	else if (CHARP(number))
		return XCHAR(number);
	else if (MARKERP(number))
		return marker_position(number);
	else
		return 0;
}

extern void init_optables_INT_T(void);
extern void init_ent_int(void);
extern void syms_of_ent_int(void);
extern void vars_of_ent_int(void);

#endif /* INCLUDED_ent_int_h_ */
