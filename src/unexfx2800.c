/* Unexec for the Alliant FX/2800.  */

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

/* Synched up with: FSF 19.31. */

#include <stdio.h>

unexec (new_name, a_name, data_start, bss_start, entry_address)
     char *new_name, *a_name;
     unsigned data_start, bss_start, entry_address;
{
  int stat;
    
  stat = elf_write_modified_data (a_name, new_name);
  if (stat < 0)
    perror ("emacs: elf_write_modified_data");
  else if (stat > 0)
    fprintf (stderr, "Unspecified error from elf_write_modified_data.\n");
}
