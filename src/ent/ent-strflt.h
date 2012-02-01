/*
  ent-strflt.h -- String to float defines

  Copyright (C) 2005, 2006 Sebastian Freundt
  Copyright (C) 2006 Nelson Ferreira

  Author:  Sebastian Freundt
	   Nelson Ferreira

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

#ifndef INCLUDED_ent_strflt_h_
#define INCLUDED_ent_strflt_h_

#if fpfloat_double_p && defined(HAVE_STRTOD)
#define str_to_fpfloat(s)	strtod(s, NULL)
#elif fpfloat_double_p && defined(HAVE_STRTOLD)
#define str_to_fpfloat(s)	(fpfloat)strtold(s, NULL)
#elif fpfloat_double_p
#define str_to_fpfloat(s)	(fpfloat)atof(s)
#elif fpfloat_long_double_p && defined(HAVE_STRTOLD)
#define str_to_fpfloat(s)	strtold(s, NULL)
#elif fpfloat_long_double_p && defined(HAVE_STRTOD)
#define str_to_fpfloat(s)	(fpfloat)strtod(s, NULL)
#elif fpfloat_long_double_p
#define str_to_fpfloat(s)	(fpfloat)atof(s)
#else
#define str_to_fpfloat(s)	(fpfloat)atof(s)
#endif

#endif /* INCLUDED_ent_strflt_h_ */
