#! /bin/sh
# add-big-package.sh --- Add multiple file package to Package Lisp Hierarchy
# Copyright (C) 1997 Free Software Foundation, Inc.

# Author:	SL Baur <steve@xemacs.org>
# Maintainer:	SL Baur <steve@xemacs.org>
# Keywords:	packages internal

# This file is part of XEmacs.

# XEmacs is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# XEmacs is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with XEmacs; see the file COPYING.  If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

### Commentary:

## This file copies a single lisp file into an XEmacs package hierarchy and
## performs the necessary magic so that it will be autoloaded at the next
## dump.

## Parameters:
##	$1 -- Full path to an XEmacsen later than 20.3
##	$2 -- Full path to a lisp package tarball to install
##	$3 -- Full path to a lisp directory in an XEmacs package hierarchy
##	      This directory will be created if it does not exist.
##	      NOTE: the directory name should *not* end in a trailing slash


### Code:

XEMACS="$1"			# Not used at present
LISP_FILE="$2"			# Should be a binary package tarball
DEST_DIR="$3"			# Should be a top level package directory

# Test for valid XEmacs executable and valid input file
if [ ! -f "${LISP_FILE}" -o ! -x "${XEMACS}" ]; then
	exit 1
fi

# Test for destination directory, creating if necessary
test -d "${DEST_DIR}" || mkdir "${DEST_DIR}"
test -d "${DEST_DIR}" || exit 1;

# Very simple minded extraction for the first cut
# We'll get more sophisticated later
cd "${DEST_DIR}"
gunzip -c "${LISP_FILE}" | tar xvf -

# Need to refresh the info/dir file, I don't know how to do that.

exit 0

### add-big-package.sh ends here
