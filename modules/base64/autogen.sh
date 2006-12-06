#!/bin/sh

olddir=`pwd`
srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

cd "$srcdir"
rm -rf autom4te.cache aclocal.m4

aclocal -I .
autoheader
autoconf -Wall
automake -ac

cd $olddir
