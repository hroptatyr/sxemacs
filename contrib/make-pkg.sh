#!/bin/sh
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

