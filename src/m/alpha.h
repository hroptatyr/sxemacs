/* machine description file For the alpha chip.
   Copyright (C) 1994 Free Software Foundation, Inc.

This file is part of XEmacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#ifdef OSF1
# define ORDINARY_LINK
#endif

#ifndef __ELF__
  /* Describe layout of the address space in an executing process.  */
# define TEXT_START    0x120000000
# define DATA_START    0x140000000
  /* The program to be used for unexec. */
# define UNEXEC "unexalpha.o"
#endif
