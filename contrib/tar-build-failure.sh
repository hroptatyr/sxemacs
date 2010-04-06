#!/bin/sh
#
type tar >/dev/null 2>&1 && TAR=tar
EXT=gz
type gzip >/dev/null 2>&1 && COMPRESS=gzip
if [ -z "$COMPRESS"  ]; then
    type compress >/dev/null 2>&1 && COMPRESS=compress
    EXT=Z
fi
if [ ! -f ./sxemacs.pc -o ! -f ./Installation -o ! -f ./config.log ]; then
    echo "Please run this script from the top of the sxemacs build directory.\n"
    exit 1
fi
if [ -z "$TAR" -o -z "$COMPRESS" ]; then
    echo "Could not find one or more of tar compress gzip"
    exit 1
fi
for f in config.log Installation sxemacs_version.m4 ,,beta.out ,,vars.out ,,make-check.out src/config.h ; do
    if [ -f $f ]; then
	FILES="$FILES $f"
    fi
done
attachment="build-failure.tar.${EXT}"
tar cf - $FILES  | $COMPRESS -c - > $attachment
