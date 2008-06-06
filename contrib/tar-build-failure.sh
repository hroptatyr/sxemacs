#!/bin/sh
#
type tla >/dev/null 2>&1  || TLA=tla
type grep >/dev/null 2>&1 || GREP=grep
type tar >/dev/null 2>&1  || TAR=tar
if [ ! -f ./sxemacs.pc -o ! -f ./Installation -o ! -f ./config.log ]; then
    echo "Please run this script from the top of the sxemacs build directory.\n"
    exit 1
fi
if [ "$TAR" = "" ]; then
    echo "Could not find one or more of sendmail tar base64"
    exit 1
fi
for f in config.log Installation sxemacs_version.m4 ,,beta.out ,,vars.out ,,make-check.out src/config.h ; do
    if [ -f $f ]; then
	FILES="$FILES $f"
    fi
done
attachment="build-failure.tar.gz"
tar czf $attachment $FILES  
