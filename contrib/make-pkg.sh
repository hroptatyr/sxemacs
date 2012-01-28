#!/bin/sh
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
echo "Complain bitterly to njsf@sxemacs.org that he has still NOT updated this script to use git"
exit 1
if [ ! -n "$1" ]; then
    echo "Usage: $0 <package-name> [<package-location>]"
    echo "Example: $0 sxemacs-22.1.9"
    exit 1
fi
type tla >/dev/null 2>&1 && TLA=tla
if [ -z "${TLA}" ]; then
    echo "Cannot find tla."
    exit 1
fi
BASE=$(basename $1)
BASE=$(echo ${BASE} | sed -e 's/\.tgz$//g' -e 's/\.tar\.gz$//g' -e 's/\.tar$//g')
if [ ! -f ./autogen.sh -o ! -f ./sxemacs.pc.in ]; then
    echo "Please run this script from the top of the sxemacs source directory.\n"
    exit 1;
fi
STATUS=""
if [ -z "MAKEPKGYES" ]; then
    echo "The next step will erase ALL files not under source control from the source tree"
    echo -n "Continue [Y/N]"
    read YN
else
    YN="y"
fi
YN=$(echo ${YN} | tr [:upper:] [:lower:])
if [ "$YN" != "y" -a "$YN" != "yes" ]; then
    echo "Stopping."
    exit 1;
fi
${TLA} export "/tmp/${1}" || STATUS="FAIL_EXPORT"
if [ -n "$STATUS" ]; then
    echo "Either your tla does not support export or it failed to create the destination"
    exit 1
fi
HAMMER=BHFH ./autogen.sh || STATUS="FAIL_AUTOGEN"
if [ -n "$STATUS" ]; then
    echo "The autogen process failed"
    exit 1
fi
for f in $(${TLA} inventory -p -B); do
    cp -r -v --parents "${f}" "/tmp/${1}"
done
${TLA} changelog --untagged > "/tmp/${1}/ChangeLog" || STATUS="FAIL_CHANGELOG"
if [ -n "$STATUS" ]; then
    echo "The changelog generation failed. Continuing..."
    STATUS=""
fi
CURDIR="$(pwd)"
if [ -n "$2"  ]; then
    cd "$2"
else
    echo "Using ${CURDIR} as package destination"
fi
DEST="$(pwd)/${BASE}.tar.gz"
cd /tmp
tar --create --owner=0 --group=0 --gzip --file "${DEST}" "${BASE}"
md5sum "${DEST}" > "${DEST}.md5"
/bin/rm -rf "${BASE}"
cd "$CURDIR"
echo "Done"
