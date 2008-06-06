#!/bin/sh
#

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
# Yes or Y
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
    if [ -z "${STATUS}" -o "${STATUS}" = "FAIL_CONFIGURE_0" ]; then
	STATUS=Success
	make ${MAKE_ARGS} || STATUS="FAIL_MAKE_$?"
	if egrep '^make.*:.*Error' ,,beta.out; then
	    STATUS="${STATUS}_ERROR_IN_BUILD"
	else
	    if [ -f ,,make-check.out -a ! "${STATUS}" = "Success" ]; then
		STATUS="Tests fail"
	    fi    
	fi
    fi
    if [ "${REPORT_STATUS}" = "Yes" -o "${REPORT_STATUS}" = "Y" ]; then
	if [ "${STATUS}" = "Success" -o "${STATUS}" = "Tests fail" ]; then
	    CLI=""
	    if [ -n "${MAIL_FROM}" ]; then
		CLI="${CLI} -eval '(setq user-mail-address \"$MAIL_FROM\")'"
	    fi
	    if [ -n "$MAIL_SUCCESS_TO" ]; then
		CLI="${CLI} -eval '(setq build-rpt-email \"${MAIL_SUCCESS_TO}\")'"
	    fi
	    CLI="${CLI} -eval '(send-build-rpt \"${STATUS}\")' -eval '(kill-emacs 0)'"
	    eval "src/sxemacs -batch ${CLI}" || DO_REPORT_FAILURE="YES"
	else
	    DO_REPORT_FAILURE="YES"
	fi
	if [ "${DO_REPORT_FAILURE}" = "YES" ]; then
	    ${PREFIX}/contrib/report-build-failure.sh "${MAIL_FROM}" "${MAIL_FAILURE_TO}" "${STATUS}"
	fi
    fi
    cd "$CURDIR"
    if [ "${STATUS}" == "Success" ]; then
	if [ "${REMOVE_ON_SUCCESS}" == "Yes" -o "${REMOVE_ON_SUCCESS}" == "Y" ]; then
	    /bin/rm -rf "./${build_name}"
	fi
    fi
    do_log "Finished building ${build_name}: ${STATUS}"
done
    
