/* This file is part of XEmacs.

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

/* Synched up with: FSF 19.30. */

/* This file is loaded before crt0.o on machines where we do not
   remap part of the data space into text space in unexec.
   On these machines, there is no problem with standard crt0.o's
   that make environ an initialized variable.  However, we do
   need to make sure the label data_start exists anyway.  */

/* Create a label to appear at the beginning of data space.  */

int data_start = 0;
