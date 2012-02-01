#!/bin/sh
#
# Build a tar of valuable information to figure out why the build failed.
#
# (C) 2008 Nelson Ferreira
#
# This program is free software; you can redistribute it and/or modify it
# under a BSD-like licence.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# Redistributions of source code must retain the above copyright notice, this
# list of conditions and the following disclaimer.
# Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
# Neither the name of the Technical University of Berlin nor the names of its
# contributors may be used to endorse or promote products derived from this
# software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
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
