#! /bin/sh
# emacs-fe --- front end driver for `emacs' and other programs

# Copyright (C) 1995, 1996 Noah S. Friedman

# Author: Noah Friedman <friedman@prep.ai.mit.edu>
# Created: 1995-09-11

# $.Id: emacs-fe,v 1.8 1996/03/07 04:32:33 friedman Exp $

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, you can either send email to this
# program's maintainer or write to: The Free Software Foundation,
# Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

# Commentary:

# Inspired by a similar set of scripts by Charles Sandel <sandel@cli.com>,
# but generalized into this single script.

# Front-end shell script for GNU Emacs, used to manage multiple versions of
# Emacs and its associated utilities.
#
# Strategy: Install this script as "$prefix/bin/[progname]", for each
# program named [progname], (e.g. "emacs", "ispell", "etags", etc).  These
# are the commands users would normally execute to run them.

# Give each version of emacs/xemacs/mule/ispell a separate hierarchy under
# $prefix/[emacs|xemacs|mule|ispell], with the name
# "[emacs|xemacs|mule|ispell]-NN.NN" where NN.NN is the version number.
# This script looks at what versions are available, and selects a version,
# currently whatever is specified by $DEFAULTLVERSION.

# However, users can specify their own choice to force the selection of a
# particular version by setting the environment variable PROGNAMEVERSION
# (e.g. EMACSVERSION, MULEVERSION, XEMACSVERSION, etc.) to have a value
# which is the version number of the program that they want to use (just
# the numeric value), or to specify either the NEWEST or OLDEST versions.

# Code:

# Name by which this script was invoked.
progname=`echo "$0" | sed -e 's/[^\/]*\///g'`

# To prevent hairy quoting and escaping later.
bq='`'
eq="'"

case "$progname" in
  emacs-fe-print )
    case $# in
      1 ) : ;;
      * )
        echo "$progname: Exactly one argument is required." 1>&2
        exit 1
       ;;
    esac

    # sed is more portable than `dirname'
    dir=`echo "$0" | sed -e 's/\/*$//' -e 's/\/[^\/]*$//'`
    if test -f "$dir/$1"; then
      EMACS_FE_PRINT=t
      export EMACS_FE_PRINT
      exec "$dir/$1"
    fi

    echo "$progname: $bq$dir/$1$eq does not seem to exist." 1>&2
    exit 1
  ;;
esac

DEFAULTVERSION="${DEFAULTVERSION-NEWEST}"
VARIANT="${EMACSVARIANT-emacs}"

if [ "$prefix" = "" ] ; then
  # root of the GNU installed tree
  prefix=/usr/local/gnu
fi

if [ ! -d "$prefix" ] ; then
  echo "Cannot find root of GNU tree ($prefix)."
  exit 1
fi

case "$progname" in
  emacs | lemacs | xemacs | mule | ispell )
    if [ "$eprefix" = "" ] ; then
      # prefix name of the subdirectory
      eprefix="${progname}/${progname}-"
    fi
   ;;
  * )
    eprefix="$VARIANT/${VARIANT}-"
   ;;
esac

# Find out which versions are available on the system and sort them
# in numeric order.
#
# The largish sed script prefixes all version numbers with a sort key.
# That key is constructed by padding out any single or double digits to 3
# digits from the version number, then converting all occurrences of `.' to
# `0', and prefixing and suffixing the entire result with an additional
# zero.  After sorting, the sort key is stripped from the output.
# We do all this because `sort' cannot numerically sort decimal numbers and
# will stop on the first `.'.
# This may not work correctly if the version number has more than 4 levels
# of minor versions (e.g. "1.2.3.4.5" may cause problems).
availversions=`ls -1d $prefix/${eprefix}*/. 2> /dev/null \
                | sed -n \
                      -e "s#^$prefix/$eprefix\([0-9.][0-9.]*\)/\.*#\1#" \
                      -e 'h
                          s/[^.]*[^0-9.][^.]*\.//g
                          :0
                          /[0-9.][0-9.]*\.[0-9.][0-9.]*\.[0-9.][0-9.]*\.[0-9.][0-9.]*/!{
                            s/$/.0/
                            b 0
                          }
                          s/^/./
                          s/$/./
                          :1
                          s/\.\([0-9]\)\./.00\1./g
                          s/\.\([0-9][0-9]\)\./.0\1./g
                          t 1
                          s/\./0/g
                          G
                          s/\n/ /' \
                       -e 'p' \
                 | sort -nu \
                 | sed -e 's/.* //'`

if [ "$availversions" = "" ] ; then
	echo "No version of $progname found in $prefix/$eprefix*."
	exit 1
fi

# This sets `oldest' to the oldest version available, and `newest'
# to the newest version available.
# On line 1, we save the original pattern in the hold space and restore it
# in case it is the only line of input.
eval `echo "$availversions" \
       | sed -ne '1{h;s/^/oldest=/p;g;}
                  ${s/^/newest=/p;}
                 '`

# The environment variable [progname]VERSION can have a value which specifies
# a version number, OR it can contain the values "NEWEST" or "OLDEST" to
# specify the newest or oldest version which was found.
sed_upcase='y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/'

PROGNAME=`echo "$progname" | sed -e "$sed_upcase" -e 's/-/_/g'`
eval version=\"\$${PROGNAME}VERSION\"

# If there is no ETAGSVERSION, EMACSCLIENTVERSION, etc, then look for
# EMACSVERSION, XEMACSVERSION, or whatever the current variant is.
case "$version" in
  '' )
    case "$progname" in
      ispell )
        # If this is ispell and ISPELLVERSION isn't set, just use 3.1.
        # We could run this script recursively with a flag indicating to
        # find the current emacs variant and version and just print it out,
        # but that is a very pathological case and is a lot of work.
        version=3.1 ;;
      * )
        variant=`echo "$VARIANT" | sed -e "$sed_upcase"`
        eval version=\"\$${variant}VERSION\"
        case "$version" in
          '' ) version="$DEFAULTVERSION" ;;
        esac
       ;;
    esac
esac

case "$version" in
  [Oo][Ll][Dd][Ee][Ss][Tt]) version="$oldest" ;;
  [Nn][Ee][Ww][Ee][Ss][Tt]) version="$newest" ;;
  '') version="$oldest" ;;
  *)
    if [ ! -d "$prefix/$eprefix$version" ] ;  then
      echo "$progname: $version: Cannot find requested version." 1>&2
      version=
    fi
   ;;
esac

# If we don't have a version by now, then give up.
if [ "$version" = "" ] ; then
  exec 1>&2
  echo "$progname: Cannot determine which version to use."
  case "$availversions" in
    */* )
      echo "Available versions are:"
      for f in $availversions; do
        echo "   $f"
      done | sort
     ;;
    * )
      echo "Available versions are:" $availversions
     ;;
  esac
  exit 1
fi

case "$progname" in
  emacs | lemacs | xemacs | mule )
    EMACSVARIANT=$progname
    eval ${PROGNAME}VERSION=$version
    eval export EMACSVARIANT ${PROGNAME}VERSION

    case "$EMACSVARIANT-$version" in
      emacs-18* )       ISPELLVERSION=4.0    ;;
      emacs-19.[0-9] )  ISPELLVERSION=4.0    ;;
      emacs-19.1[0-9] ) ISPELLVERSION=4.0    ;;
      emacs-19.2[0-2] ) ISPELLVERSION=4.0    ;;
      emacs-19.2[3-9] ) ISPELLVERSION=3.1    ;;
      emacs-* )         ISPELLVERSION=3.1    ;;

      lemacs-19.[0-9] )	ISPELLVERSION=3.0.09 ;;
      lemacs-19.10 )	ISPELLVERSION=3.1    ;;

      xemacs-* )	ISPELLVERSION=3.1    ;;

      mule-* )          ISPELLVERSION=3.1    ;;
    esac
    export ISPELLVERSION
   ;;
esac

case "$progname" in
  xemacs )
    # xemacs expects to use the keysym database in /usr/openwin, but that
    # database doesn't define many of the keysyms it uses.  Unless the user
    # has already defined their own, specify the keysym database in X11.
    XKEYSYMDB="${XKEYSYMDB-/usr/local/X11/lib/X11/XKeysymDB}"
    export XKEYSYMDB

    # Some versions of xemacs (e.g. 19.12) are dynamically linked against
    # the openwin tooltalk library (libtt.so), so add openwin to the
    # dynamic load path if necessary.
    case "$LD_LIBRARY_PATH" in
       *'/usr/openwin/lib'* ) : ;;
       '' )
         LD_LIBRARY_PATH=/usr/local/X11R5/lib:/usr/openwin/lib:/lib
         export LD_LIBRARY_PATH
        ;;
       * )
         LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/openwin/lib"
         export LD_LIBRARY_PATH
        ;;
    esac
 ;;
esac

# Set up the MANPATH so that the man pages for this version
# are searched first
if [ -d $prefix/$eprefix$version/man ] ; then
  MANPATH=$prefix/$eprefix$version/man:$MANPATH
  export MANPATH
fi

# There is no need to do this, and it can potentially cause problems,
# especially if a program like `xemacs' exists in that directory and gets
# run in subshells instead of this script.
#PATH=$prefix/$eprefix$version/bin:$PATH
#export PATH

searchdirs=`exec 2> /dev/null
            cd $prefix/$eprefix$version \
            && find bin \
                    libexec/$VARIANT/$version/* \
                    lib/$VARIANT/$version/* \
                    lib/$VARIANT-$version/* \
                    lib/$VARIANT/etc \
                    lib/etc \
                 -type d -print`

for dir in $searchdirs ; do
  for p in $progname-$version $progname ; do
    prog="$prefix/$eprefix$version/$dir/$p"

    if test -f "$prog" ; then
      case "${EMACS_FE_PRINT+set}" in
        set )
          echo "$prog"
          exit 0
         ;;
      esac

      exec "$prog" ${1+"$@"}
    fi
  done
done

exec 1>&2

echo "$progname: Cannot find $bq$progname-$version$eq or $bq$progname$eq in"

for d in $searchdirs ; do
  ls -1d $prefix/$eprefix$version/$d 2> /dev/null \
   | sed -e "s/^/$progname:   /"
done

exit 1

# emacs-fe ends here
