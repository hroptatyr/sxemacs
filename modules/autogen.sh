#!/bin/sh

olddir=`pwd`
srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

cd "$srcdir"
##rm -rf autom4te.cache aclocal.m4

if test -z "$FORCE"; then
    FORCE=
else
    FORCE=--force
fi

# aclocal -I . $FORCE
# autoheader $FORCE -Wall
# autoconf $FORCE -Wall
# automake $FORCE -ac -Wall

autoreconf $FORCE --install -Wall

cd $olddir
