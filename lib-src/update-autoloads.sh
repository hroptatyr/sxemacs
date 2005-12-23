#!/bin/sh
### update-autoloads.sh --- update auto-autoloads.el as necessary

# Author: Jamie Zawinski, Ben Wing, Martin Buchholz, Steve Baur
# Maintainer: Steve Baur
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

### Code:

set -eu

# This means we're running in a Sun workspace
test -d ../era-specific && cd ../editor

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
CANON_PWD=`pwd`
# Account for various system automounter configurations
if test -d "/net"; then
  if test -d "/tmp_mnt/net"; then tdir="/tmp_mnt/net"; else tdir="/tmp_mnt"; fi
  EMACS_DIR=`echo "$EMACS_DIR" | \
   sed -e "s|^${tdir}/|/net/|" -e "s|^/a/|/net/|" -e "s|^/amd/|/net/|"`
  CANON_PWD=`echo "$CANON_PWD" | \
   sed -e "s|^${tdir}/|/net/|" -e "s|^/a/|/net/|" -e "s|^/amd/|/net/|"`
fi
REAL="$EMACS_DIR/`basename $EMACS`"

echo "Rebuilding autoloads in $CANON_PWD"
echo "          with $REAL..."

#### echon really sucks!
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
# These directories don't have autoloads or are partially broken.
ignore_dirs="egg eos ilisp its locale mel mu sunpro term tooltalk"

# Prepare for autoloading directories with directory-specific instructions
make_special_commands=''
make_special () {
	dir="$1"; shift;
	ignore_dirs="$ignore_dirs $dir"
	make_special_commands="$make_special_commands \
		(cd \"lisp/$dir\" && ${MAKE:-make} EMACS=$REAL ${1+$*});"
}

# Only use Mule XEmacs to build Mule-specific autoloads & custom-loads.
echon "Checking for Mule support..."
lisp_prog='(princ (featurep (quote mule)))'
mule_p="`$EMACS -batch -q -no-site-file -eval \"$lisp_prog\"`"
if test "$mule_p" = nil ; then
	echo No
	ignore_dirs="$ignore_dirs mule leim language skk"
else
	echo Yes
fi

## AUCTeX is a Package now
# if test "$mule_p" = nil ; then
# 	make_special auctex autoloads
# else
# 	make_special auctex autoloads MULE_EL=tex-jp.elc
# fi
#make_special cc-mode autoloads
# EFS is now packaged
#make_special efs autoloads
#make_special eos autoloads # EOS doesn't have custom or autoloads
# Hyperbole is now packaged
# make_special hyperbole autoloads
# make_special ilisp autoloads
# oobr is now packaged
# make_special oobr HYPB_ELC='' autoloads
## W3 is a package now
##make_special w3 autoloads

dirs=
for dir in lisp/*; do
	if test -d $dir \
		-a $dir != lisp/CVS \
		-a $dir != lisp/SCCS; then
		for ignore in $ignore_dirs; do
			if test $dir = lisp/$ignore; then
				continue 2
			fi
		done
		dirs="$dirs $dir"
	fi
done

$EMACS -batch -q -no-site-file -eval '(setq autoload-package-name "Standard")' \
	-l autoload -f batch-update-directory lisp

# set -x
for dir in $dirs; do
	$EMACS -batch -q -no-site-file -l autoload -f batch-update-directory $dir
done

# eval "$make_special_commands"
