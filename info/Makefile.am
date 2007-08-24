## This file is part of SXEmacs.

## SXEmacs is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.

## SXEmacs is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.

## You should have received a copy of the GNU General Public License
## along with SXEmacs; see the file COPYING.  If not, write to
## the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
## Boston, MA 02111-1307, USA.

## Process this file with automake to produce Makefile.in

# Help the Developers and yourself. Just use the C locale and settings
# for the compilation. They can still be overriden by make LANG=<whatever>
# but that is general a not very good idea
LANG=C
LC_ALL=C

info_TEXINFOS = cl.texi custom.texi emodules.texi sppm.texi		\
	external-widget.texi sxemacs-faq.texi term.texi widget.texi	\
	lispref/lispref.texi internals/internals.texi			\
	sxemacs/sxemacs.texi new-users-guide/new-users-guide.texi

TEXI2DVI_TEX = latex
TEXI2DVI = TEX=$(TEXI2DVI_TEX) texi2dvi -c -s
