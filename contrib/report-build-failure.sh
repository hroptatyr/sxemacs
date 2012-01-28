#!/bin/sh
#
# Send email to sxemacs-builds list about build failure.
# NOTE: At the time of last change, you HAVE to be a subscriber for
# your email to actually be delivered to the list
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
if [ -z "$1"  ]; then
    echo "Usage: $0 <from address>"
    echo "Example: $0 superhacker@sxemacs.org"
    exit 1
else
    FROM=$1
fi
if [ -z "$REPORT_DEST" ]; then
    REPORT_DEST=sxemacs-builds@sxemacs.org
fi
type sendmail >/dev/null 2>&1 && SENDMAIL=sendmail
type tla >/dev/null 2>&1 && TLA=tla
type egrep >/dev/null 2>&1 && GREP=egrep
type tail >/dev/null 2>&1 && TAIL=tail
type tar >/dev/null 2>&1 && TAR=tar
type cat >/dev/null 2>&1 && CAT=cat
type base64 >/dev/null 2>&1 && BASE64=base64
type sed >/dev/null 2>&1 && SED=sed
EXT=gz
type gzip >/dev/null 2>&1 && COMPRESS=gzip
if [ -z "$COMPRESS" ]; then
    type compress >/dev/null 2>&1 && COMPRESS=compress
    EXT=Z
fi
FILTER=cat
ENCODING=base64
attachment="build-failure-$FROM.tar.${EXT}"
if [ -z "${BASE64}" ]; then
    type uuencode >/dev/null 2>&1 && BASE64=uuencode
    if [ -n "${BASE64}" ]; then
	if [ -z "${SED}" ]; then
	    echo "sed was not found and is required when base64 is replaced by uuencode"
	    exit 1
	fi
	# Test if this uuencode can actually base64 encode like
	# in BSD's and MacOS
	FILTER="${SED} -e s/^begin.*644.*\$// -e s/^end\$// -e s/^====\$// -e /^\$/d "
	if [ -n "$(echo test | ${BASE64} -m test 2>/dev/null )" ]; then
	    BASE64="${BASE64} -m ${attachment}"
	else
	    ENCODING=uuencode
	    BASE64="${BASE64} ${attachment}"
	fi
    fi
fi
if [ -n "$2" ]; then
    REPORT_DEST=$2
fi
if [ -z "${SENDMAIL}" ]; then
    if [ -x /usr/lib/sendmail ]; then
	SENDMAIL=/usr/lib/sendmail
    elif [ -x /usr/sbin/sendmail ]; then
	SENDMAIL=/usr/sbin/sendmail
    elif [ -x /sw/sbin/sendmail ]; then
	SENDMAIL=/sw/sbin/sendmail
    else
	echo "Cannot find sendmail."
	exit 1
    fi
fi
if [ -z "${TLA}" ]; then
    echo "Cannot find tla."
    exit 1
fi
if [ -z "${BASE64}" -o -z "${TAR}" -o -z "${SENDMAIL}" -o -z "${GREP}" -o -z "${CAT}" -o -z "${COMPRESS}" ]; then
    echo "Could not find one or more of grep (${GREP}), sendmail (${SENDMAIL}),"
    echo "\t tar (${TAR}), base64 (${BASE64}), cat (${CAT})"
    exit 1
fi
if [ ! -f ./config.log -o -z "$(${GREP} -i sxemacs ./config.log)" ]; then
    echo "Please run this script from the top of the sxemacs build directory.\n"
    exit 1
fi
VERSION=$(${GREP} "^#define SXEMACS_ARCH_VERSION" ./config.log | awk '{ print $3 }')
if [ -z "$VERSION" ]; then
    VERSION="Unknown. Possible early configure failure"
else
    attachment="build-failure-$VERSION-$FROM.tar.${EXT}"
fi
MAIN_VERSION=$(${GREP} "^#define SXEMACS_MAIN_ARCH_VERSION" ./config.log | awk '{ print $3 }')
SYNC=""
if [ -n "$MAIN_VERSION" -a "$MAIN_VERSION" != "$VERSION" ]; then
    SYNC="(Last sync with main branch: $MAIN_VERSION)"
fi
CONFGUESS=$(${GREP} "^host=" ./config.log | awk -F= '{ print $2 }' )
if [ -z "$CONFGUESS" ]; then
    CONFGUESS="Unknown. Possible early configure failure"
fi
SRCDIR=$(${GREP} "^sxe_srcdir=" ./config.log | awk -F= '{ print $2 }' | sed -e "s/^'//" -e "s/'$//" )
BLDDIR=$(${GREP} "^sxe_blddir=" ./config.log | awk -F= '{ print $2 }' | sed -e "s/^'//" -e "s/'$//" )
COMPILER=$(${GREP} "^CC=" ./config.log | awk -F= '{ print $2 }' )
CONFIG_OPTS=$(${GREP} "^#define EMACS_CONFIG_OPTIONS " ./config.log | sed -e 's/^#define EMACS_CONFIG_OPTIONS //g')
if [ -z "$MACHTYPE"  ]; then
    MACHTYPE=$(uname -a)
fi
for f in config.log Installation sxemacs_version.m4 ,,beta.out ,,vars.out ,,make-check.out src/config.h ; do
    if [ -f $f ]; then
	FILES="$FILES $f"
    fi
done
STATUS=$3
if [ -z "${STATUS}" ]; then
    STATUS="BUILD FAILURE"
fi
SUBJECT="[${STATUS}] Version $VERSION on $CONFGUESS [$MACHTYPE] [@$(pwd)]"
MIME="application/x-gzip"
boundary="--sxemacs--failure--$$--"
KEEP=( \
    [0]="^(cd|n?make)[:blank:]" \
    [1]="errors?" \
    [2]="warnings?" \
    [3]="pure.*(space|size)" \
    [4]="hides\>" \
    [5]="strange" \
    [6]="shadowings" \
    [7]="^Compil(ing[:blank:]+in|ation)" \
    [8]="^Using" \
    [9]="not[:blank:]+found" \
    [10]="^While[:blank:]+compiling.*(\n[:blank:]+.+)*" \
    [11]="^Note:" \
    [12]="Installing" \
    [13]="[Ff]ile(s) copied" \
    [14]="^[A-Za-z_]+=" \
    [15]="[:blank:]+tests[:blank:]+" \
    [16]="^(real|user|sys)[:blank:]+[0-9]+m" \
    )
REMOVE=( \
    [0]="confl.*with.*auto-inlining" \
    [1]="^Formatting:" \
    [2]="\(100%\) tests successful" \
    [3]="errors that should" \
    [4]="wrong-error" \
    )
KREGEXP=""
for r in ${KEEP[*]}; do
    if [ -z "$KREGEXP" ]; then
	KREGEXP="$r"
    else
	KREGEXP="${KREGEXP}|${r}"
    fi
done
RREGEXP=""
for r in ${REMOVE[*]}; do
    if [ -z "$RREGEXP" ]; then
	RREGEXP="$r"
    else
	RREGEXP="${RREGEXP}|${r}"
    fi
done
(cat <<EOF
From: $FROM
To: $REPORT_DEST
Subject: $SUBJECT
Date: $(date +"%a, %e %Y %T %z")
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary="$boundary"
Content-Disposition: inline

--$boundary
Content-Type: text/plain; charset=us-ascii
Content-Disposition: inline

$STATUS while building $VERSION on $CONFGUESS ($MACHTYPE)

EOF
MSRCDIR="${SRCDIR}"
if [ -z "${MSRCDIR}" ]; then
    MSRCDIR="."
fi
if [ -n "${MSRCDIR}" -a -d "${MSRCDIR}/{arch}" -a -n "${TLA}"  -a -n "${GREP}"  -a -n "${TAIL}"  ]; then
    CURDIR=$(pwd)
    cd "${MSRCDIR}"
    TREE="$(${TLA} tree-version)"
    PATCH="$(${TLA} revisions | ${TAIL} -1)"
    echo "Tree : ${TREE}"
    echo "Patch: ${PATCH}"
    echo ""
    LAST_MAIN="$(${TLA} log-versions | ${GREP} -- '--main--' | ${TAIL} -1)"
    LAST_MAIN_PATCH="$(${TLA} revisions $LAST_MAIN | ${TAIL} -1)"
    if [ "${TREE}" != "${LAST_MAIN}" -o "${PATCH}" != "${LAST_MAIN_PATCH}" ]; then
	echo "Main : $LAST_MAIN"
	echo "Patch: $LAST_MAIN_PATCH"
	echo ""
    fi
    DIFF_NAME=/tmp/tla-changes-${USER}-$$-${attachment}.log
    ${TLA} changes -q --diffs > $DIFF_NAME
    if [ -s $DIFF_NAME ]; then
	echo "Changes:"
	echo ""
	cat $DIFF_NAME
	echo ""
	echo "End of Changes"
    fi
    /bin/rm $DIFF_NAME
    cd "${CURDIR}"
else
    echo $SYNC
fi

echo ""
if [ -n "${SRCDIR}" ]; then
    echo " Source in ${SRCDIR}"
fi
if [ -n "${BLDDIR}" -a "${SRCDIR}" != "${BLDDIR}" ]; then
    echo " Build  in ${BLDDIR}"
fi

if [ -f ./Installation ]; then
    echo ""
    ${CAT} ./Installation
else
    if [ -n "${COMPILER}" ]; then
	echo " Using compiler ${COMPILER}"
    fi
    if [ -n "${CONFIG_OPTS}" ]; then
	echo " Configured with options: ${CONFIG_OPTS}"
    fi
    echo ""
fi

for f in beta make-all make-check-temacs make-check make-install; do
    if [ -f "./,,${f}.out" ]; then
	echo "> Contents of $(pwd)/,,${f}.out"
	echo ""
	${GREP} "$KREGEXP" ./,,${f}.out | ${GREP} -v "$RREGEXP"
    fi
done


FIND_NAME=/tmp/find-ldd-${USER}-$$-${attachment}.log
find src -type f -a -perm +111  -a -name \*emacs\* -exec file {} \; -exec ldd {} \; > $FIND_NAME 2>&1
if [ -s $FIND_NAME ]; then
    echo " Shared library dependencies "
    echo ""
    cat $FIND_NAME
    echo ""
    echo ""
fi
/bin/rm $FIND_NAME

${CAT} <<EOF

--$boundary
Content-Type: $MIME;name="$attachment"
Content-Disposition: attachment;filename="$attachment"
Content-Transfer-Encoding: $ENCODING

EOF
${TAR} cf - $FILES | ${COMPRESS} -c - | ${BASE64} | ${FILTER}
echo ""
echo "--$boundary--" ) | $SENDMAIL -t
