#!/bin/sh
### update-custom.sh --- update Customize group dependencies

# Author: Hrvoje Niksic, based on update-autoloads.el by
#   Jamie Zawinski, Ben Wing, Martin Buchholz, and Steve Baur
# Maintainer: Hrvoje Niksic
# Keywords: internal

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

# This is much simpler than update-autoloads.el.  All we need to do is
# compute a list of directories we want to use, and feed it to
# Custom-make-dependencies.  End of story.

### Code:

set -eu

# get to the right directory
test ! -d ./lisp -a -d ../lisp && cd ..
if test ! -d ./lisp ; then
	echo $0: neither ./lisp/ nor ../lisp/ exist
	exit 1
fi

test -z "$EMACS" && EMACS="./src/xemacs"
echo " (using $EMACS)"

export EMACS

EMACS_DIR=`cd \`dirname $EMACS\` && pwd`;
# Account for various system automounter configurations
if test -d "/net"; then
  if test -d "/tmp_mnt/net"; then tdir="/tmp_mnt/net"; else tdir="/tmp_mnt"; fi
  EMACS_DIR=`echo "$EMACS_DIR" | \
   sed -e "s|^${tdir}/|/net/|" -e "s|^/a/|/net/|" -e "s|^/amd/|/net/|"`
fi
REAL="$EMACS_DIR/`basename $EMACS`"

echo "Rebuilding custom-loads with $REAL..."

if [ "`uname -r | sed 's/\(.\).*/\1/'`" -gt 4 ]; then
  echon()
  {
    /bin/echo $* '\c'
  }
else
  echon()
  {
    echo -n $*
  }
fi

# Compute patterns to ignore when searching for files
ignore_dirs=""

# Only use Mule XEmacs to build Mule-specific autoloads & custom-loads.
echon "Checking for Mule support..."
lisp_prog='(princ (featurep (quote mule)))'
mule_p="`$EMACS -batch -q -no-site-file -eval \"$lisp_prog\"`"
if test "$mule_p" = nil ; then
	echo No
	ignore_dirs="$ignore_dirs mule"
else
	echo Yes
fi

echon "Checking directories..."
dirs=lisp/
for dir in lisp/*; do
	if test -d $dir \
		-a $dir != lisp/CVS \
		-a $dir != lisp/SCCS; then
		for ignore in $ignore_dirs; do
			if test $dir = lisp/$ignore; then
				continue 2
			fi
		done
		rm -f "$dir/custom-load.elc"
		dirs="$dirs $dir"
	fi
done
echo done

$EMACS -batch -q -no-site-file -l cus-dep -f Custom-make-dependencies $dirs
