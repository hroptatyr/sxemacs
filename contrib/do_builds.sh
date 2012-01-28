#!/bin/sh
#
# A script to do automatic builds with several different configurations
#
# (C) 2008 Nelson Ferreira
#
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

function do_log {
    if [ -n "$LOG_FILE" ]; then
	echo "$0 $(date): $@" >> $LOG_FILE
    fi
}

if [ ! -f ./,,conf ]; then
    cat > ./,,conf <<EOF
# --*-shell-script-*--
#
# The path to the SXEmacs source
SOURCE=/path/to/sxemacs
#
# Hammer value when doing autogen.sh [Check it for more details
HAMMER=""
#
# Autogen command [leave empty in order to skip it]
AUTOGEN="./autogen.sh"
#
# The command line arguments to pass to the make command
MAKE_ARGS="time-build-report"
#
# File to log progress to
LOG_FILE=",,build.log"
#
# Make a ++TAGS directory
MAKE_TAGS=""
#
# Remove the build directory when build is successful
# (and after reporting...) ?
# Yes or Y
REMOVE_ON_SUCCESS=""
#
# Report the build status with an email?
# Yes or Y (All)
# Failures or F (Failures only)
# Success or S  (Successes only)
# Anything else - No reports at all
REPORT_STATUS="Yes"
#
# Send success email to this address instead of sxemacs-devel@sxemacs.org
MAIL_SUCCESS_TO=""
#
# Send failure email to this address instead of sxemacs-devel@sxemacs.org
MAIL_FAILURE_TO=""
#
# The email should come from this email address
# [ $USER@$(hostname -f  2> /dev/null || (hostname ; domainname)) will be used otherwise
MAIL_FROM=""
EOF
    echo "Please review the generated ,,conf file and rerun."
    exit 1
fi
source ./,,conf
if [ -z "${SOURCE}" -o ! -d "${SOURCE}" -o ! -f ${SOURCE}/sxemacs.pc.in ]; then
    echo "No SXEmacs source found at ${SOURCE}"
    exit 1
fi
if [ -z "${MAIL_FROM}" ]; then
    MAIL_FROM="${USER}@$(hostname -f 2> /dev/null || (hostname ; domainname))"
    echo "WARNING: No mail from defined. Will use ${MAIL_FROM} instead."
fi
STATUS=""
if [ -n "${AUTOGEN}"  ]; then
    CURDIR="$(pwd)"
    cd "${SOURCE}"
    PREFIX=$(pwd)
    if [ -n "${HAMMER}" ]; then
	echo HAMMER: ${HAMMER}
    fi
    (export HAMMER ; ${AUTOGEN} ) || STATUS=FAIL_AUTOGEN
    cd "$CURDIR"
fi
if [ -n "${STATUS}" ]; then
    do_log "Failure: ${STATUS}"
    exit 1
fi
if [ -n "${BUILD_TAGS}" ]; then
    # Let's build ourselfs the tags for this source tree,
    mkdir -p ./++TAGS
    CURDIR="$(pwd)"
    cd ./++TAGS
    eval "${PREFIX}/configure" || STATUS="FAIL_CONFIGURE_$?"
    if [ -z "${STATUS}" ]; then
	make tags || STATUS="FAIL_TAGS_$?"
    fi
    cd "$CURDIR"
fi
if [ -n "${STATUS}"  ]; then
    do_log "Failure: ${STATUS}"
    exit 1
fi
for f in *.conf; do
    if [ "$f" = "*.conf" ]; then
	echo "Nothing to do - create some .conf files! "
	exit 1
    fi
    STATUS=""
    CONF_OPTS="$(cat $f)"
    build_name="$(echo ${f} |sed 's/\.conf$//')"
    do_log "Started building ${build_name}"
    if [ -d "./${build_name}" ]; then
	/bin/rm -rf "./${build_name}"
    fi
    mkdir -p "./${build_name}"
    CURDIR="$(pwd)"
    cd "./${build_name}"
    eval "${PREFIX}/configure ${CONF_OPTS}" || STATUS="FAIL_CONFIGURE_$?"
    if [ "${STATUS}" = "FAIL_CONFIGURE_127" -a -f "./Installation" -a -f "./config.log" ]; then
	if [ $(tail -1 ./config.log) = "configure: exit 0" ]; then
	    STATUS="FAIL_CONFIGURE_0"
	fi
    fi
    if [ -z "${STATUS}" -o "${STATUS}" = "FAIL_CONFIGURE_0" ]; then
	STATUS="Success"
	make ${MAKE_ARGS} || STATUS="FAIL_MAKE_$?"
	if [ "${STATUS}" = "FAIL_MAKE_0" ]; then
	    STATUS="Success"
	fi
	if egrep '^make.*:.*Error' ,,beta.out; then
	    STATUS="${STATUS}_ERROR_IN_BUILD"
	else
	    if [ -f ,,make-check.out ]; then
		if [ ! "${STATUS}" = "Success" ]; then
		    STATUS="Tests fail"
		elif [ -n "$(grep 'tests successful' ,,make-check.out | grep -v 100%)" ]; then
		    STATUS="Sucess(Some tests fail)"
		fi
	    fi
	fi
    fi
    DO_REPORT_FAILURE=""
    REPORT_THIS="No"
    if [ "${REPORT_STATUS}" = "Yes" -o  "${REPORT_STATUS}" = "Y" ]; then
	REPORT_THIS="Yes"
    elif [ "${STATUS}" = "Success" -o "${STATUS}" = "Sucess(Some tests fail)" ]; then
	if [ "${REPORT_STATUS}" = "Success" -o  "${REPORT_STATUS}" = "S" ]; then
	    REPORT_THIS="Yes"
	fi
    elif [ "${REPORT_STATUS}" = "Failures" -o  "${REPORT_STATUS}" = "F" ]; then
	REPORT_THIS="Yes"
    fi
    if [ "${REPORT_THIS}" = "Yes" ]; then
	if [ "${STATUS}" = "Success" -o "${STATUS}" = "Tests fail" -o "${STATUS}" = "Sucess(Some tests fail)" ]; then
	    # First test if can actually send email from SXEmacs, otherwise just give up :-)
	    CLI=" -eval \"(unless (fboundp 'mail-send-and-exit) (kill-emacs 2))\""
	    if [ -n "${MAIL_FROM}" ]; then
		CLI="${CLI} -eval '(setq user-mail-address \"$MAIL_FROM\")'"
	    fi
	    if [ -n "$MAIL_SUCCESS_TO" ]; then
		CLI="${CLI} -eval '(setq build-rpt-email \"${MAIL_SUCCESS_TO}\")'"
	    fi
	    CLI="${CLI} -eval '(if (send-build-rpt \"${STATUS}\") (kill-emacs 0) (kill-emacs 1))'"
	    eval "src/sxemacs -batch ${CLI}" || DO_REPORT_FAILURE="YES" STATUS="${STATUS}-Build-Rpt-Failed-$?"
	else
	    DO_REPORT_FAILURE="YES"
	fi
	if [ "${DO_REPORT_FAILURE}" = "YES" ]; then
	    ${PREFIX}/contrib/report-build-failure.sh "${MAIL_FROM}" "${MAIL_FAILURE_TO}" "${STATUS}"
	fi
    fi
    cd "$CURDIR"
    if [ "${STATUS}" = "Success" ]; then
	if [ "${REMOVE_ON_SUCCESS}" = "Yes" -o "${REMOVE_ON_SUCCESS}" = "Y" ]; then
	    /bin/rm -rf "./${build_name}"
	fi
    fi
    do_log "Finished building ${build_name}: ${STATUS}"
done
