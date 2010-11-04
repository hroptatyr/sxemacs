/*
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


/* Synched up with: FSF 19.30. */

/* This file is loaded before crt0.o on machines where we do not
   remap part of the data space into text space in unexec.
   On these machines, there is no problem with standard crt0.o's
   that make environ an initialized variable.  However, we do
   need to make sure the label data_start exists anyway.  */

/* Create a label to appear at the beginning of data space.  */

int data_start = 0;
