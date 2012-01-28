dnl sxe-option-groups.m4 --- Option group miracle
dnl
dnl Copyright (C) 2005, 2006, 2007, 2008 Sebastian Freundt
dnl
dnl Author: Sebastian Freundt <hroptatyr@sxemacs.org>
dnl
dnl This file is part of SXEmacs.


AC_DEFUN([OG_ARG_POSSVAL], [dnl
[Possible arguments:]ifelse($2,,,[ for $2])[]dnl
ifelse(dnl
$1,[og_any], [dnl
[ anything]],
$1,[og_file], [dnl
[ ]any file names],
$1,,[dnl
[ ]either `yes' or `no'],[dnl
[ ]one of [`]patsubst([$1],[ ],['[,] `])[']])])

AC_DEFUN([OG_OPTION_GROUP], [dnl
AC_ARG_WITH(xyzoptiongroup,[
[$1]
patsubst([$1],[.],[=])])])

pushdef([OG_ERROR_ADDITIONAL_VERBOSITY], [dnl
- If you do not see or know what the option --with-$1 is all about
  consider to consult $sxe_srcdir/configure --help.
- Also SXEmacs comes with a sophisticated set of default values which
  run out of the box on most machines so retry your configure line
  WITHOUT this option at all.
- If you used to pass the option `--with-$1=$withval' and it formerly
  has been accepted please consult $sxe_srcdir/configure --help and perhaps
  update your scripts.
- If you are absolutely sure that `$withval' _should_ work[,] try again!
  .oO(and see me barf again, too)
])

dnl borrowed from the openldap crew, slightly modified
dnl --------------------------------------------------------------------
dnl Restricted form of AC_ARG_WITH that limits user options
dnl
dnl $1 = option name
dnl $2 = help-string
dnl $3 = default value, can be auto or none
dnl $4 = default value documentation
dnl $5 = allowed values [yes no] if empty, can be [og_any]
dnl $6 = actions in case option was set (old AC_ARG_WITH arg #3)
dnl $7 = actions in case option was not set (old AC_ARG_WITH arg #4)
AC_DEFUN([OG_ARG_WITH], [# Option Group --with-$1
	## announce this option as valid option
	pushdef([og_group], translit([$1], [-], [_]))

	sxe_with_options="$sxe_with_options $1"
	og_[]og_group[]_opts="$5"

	AC_ARG_WITH($1,[
[$2]ifelse($5,[og_any],,[
AS_HELP_STRING([], (OG_ARG_POSSVAL($5)))])[]ifelse($4,,,[
AS_HELP_STRING([], [@<:@Default: $4@:>@])])],[
	ifelse($5,og_any,,[
		og_arg=invalid
		for og_val in ifelse($5,,[yes no],[$5]) ; do
			if test "$withval" = "$og_val" ; then
				og_arg="$og_val"
			fi
		done
		if test "$og_arg" = "invalid" ; then
			AS_MESSAGE(Wait a moment ... Are you nuts?)
			if test "`echo $withval | grep ','`"; then
				AC_MSG_ERROR([dnl
Bad multiple value `$withval' for --with-$1.
ifelse($2,[og_any],,[
AS_ESCAPE(OG_ARG_POSSVAL($5,[--with-$1]), [`])
(and remember do NOT specify 2 or 3 or 4 or more values, ONE is enough! Believe me!)

AS_ESCAPE(OG_ERROR_ADDITIONAL_VERBOSITY($1), [`])])])

			elif test "$withval" = "foo"; then
				AC_MSG_ERROR([dnl
--with-$1=foo?! In your dreams, man!
			])
			else
				AC_MSG_ERROR([dnl
Bad value `$withval' for --with-$1.
ifelse($5,[og_any],,[
AS_ESCAPE(OG_ARG_POSSVAL($5,[--with-$1]), [`])
(and nothing else, and especially not `$withval'!)

AS_ESCAPE(OG_ERROR_ADDITIONAL_VERBOSITY($1), [`])])])
			fi
		fi])
	with_[]og_group[]="$withval"
	$6
],[dnl
	with_[]og_group[]=ifelse($3,,"",$3,none,"",$3,auto,"",["$3"])
	$7])dnl
dnl AC_MSG_RESULT([Option Group --with-$1 $og_with_$1])
# end --with-$1
	popdef([og_group])
])dnl

dnl helper funs for multi arg case
dnl ==============================
dnl $1 - possible option values
dnl $2 - option name
AC_DEFUN([OG_MULTIARG_POSSVAL], [dnl
[Possible arguments:]ifelse($2,,,[ for $2])[]dnl
ifelse(dnl
$1,[og_any], [dnl
[ anything]],
$1,[og_file], [dnl
[ ]any file names],
$1,,[dnl
[ ]either `all' or `none'],[dnl
[ ]`all' or `none' as the first argument[,] followed by dnl
any (comma-separated) combination out of [`(no)]patsubst([$1],[ ],['[,] `(no)])['].])
])dnl OG_MULTIARG_POSSVAL

AC_DEFUN([OG_MULTIARG_MORE_ON_POSSVALS], [dnl
The `no' prefix hereby means not to include the item in question. dnl
E.g. combinations like `all[,]nofoo[,]nobar' are allowed to select dnl
everything but foo and bar. dnl
Later options in the chain override earlier ones. dnl
If `all' or `none' (as first argument) in the chain are omitted dnl
all items are supposed to be specified relative to the default value.])

dnl
dnl $1 = option name
dnl $2 = default value, can be auto, all, none, or any combination of $3
dnl $3 = allowed values, can be [og_any]
AC_DEFUN([OG_MULTIARG_PROCESS], [dnl
	pushdef([og_group], [translit([$1],[-],[_])])
	new_default=
	for og_val in $2 `echo "$withval" | sed -e 's/,/ /g'`; do
		og_arg=invalid
		case "$og_val" in
		dnl all and none are only permitted as the first in the list.
		no | none )		new_default=no ;;
		yes | all | auto )	new_default=yes ;;
		esac
		ifelse($3,,,[
			case "$og_val" in
			translit([$3],[ ],[|]) )
				og_arg=valid
				eval with_[]og_group[]_${og_val}=yes;;
			[no]patsubst([$3],[ ],[|no]) )
				og_arg=valid
				eval `echo $og_val | sed -e 's/^no/with_[]og_group[]_/'`=no
			esac
		])
		if test -n "$new_default"; then
dnl			for og_item in "[$3]"; do
dnl				with_[]og_group[]_$og_item="$new_default"
dnl			done
			with_[]og_group[]_[]patsubst([$3],[ ],[=$new_default; with_[]og_group[]_])[]=$new_default
				new_default=
				og_arg=valid
			fi
		if test "$og_arg" = "invalid" ; then
			AS_MESSAGE(Wait a moment ... dnl
Find a coffee factory! Drink it! And now listen:)
			AC_MSG_ERROR(dnl
Bad value `$og_val' for --with-$1.

m4_text_wrap(OG_MULTIARG_POSSVAL($3, [--with-$1])
OG_MULTIARG_MORE_ON_POSSVALS)

OG_ERROR_ADDITIONAL_VERBOSITY($1)
)
		fi
	done
	popdef([og_group])
])

dnl helper for OG_MULTIARG_HELP_STINGS
dnl mimicking AS_HELP_STRING here
dnl we can't use AS_HELP_STRING (or m4_text_wrap) directly because
dnl we need $1 and $2 expanded and indirected, a bit like a lisp `
AC_DEFUN([_OG_MULTIARG_ITEM_EXPL], [dnl
[                          ][- $1 for $2]
])dnl _OG_MULTIARG_ITEM_EXPL

dnl automatically generated help string
AC_DEFUN([OG_MULTIARG_HELP_STRINGS], [dnl
	pushdef([OG_MULTIARG_ITEM],
		_OG_MULTIARG_ITEM_EXPL($[1], $[3]))
	pushdef([OG_MULTIARG_MUTEX], [])
AS_HELP_STRING([], [Explanation of the items:])
$1
	popdef([OG_MULTIARG_MUTEX])
	popdef([OG_MULTIARG_ITEM])
])dnl OG_MULTIARG_HELP_STRINGS

dnl multiple arg form of OG_ARG_WITH, that is it accepts any (comma separated)
dnl combination of args
dnl
dnl $1 = option name
dnl $2 = help-string
dnl $3 = default value, can be auto or none
dnl $3 = default value documentation
dnl $5 = allowed values [yes no] if empty, can be [og_any]
dnl $6 = actions in case option was set (old AC_ARG_WITH arg #3)
dnl $7 = actions in case option was not set (old AC_ARG_WITH arg #4)
dnl improved version
AC_DEFUN([OG_MULTIARG_WITH], [# Option Group --with-$1 (multiarg)
	## announce this option as valid option
	sxe_with_options="$sxe_with_options $1"

	pushdef([og_group], [translit([$1],[-],[_])])
	pushdef([og_DefVal],ifelse($3,,auto,$3))
	pushdef([OG_MULTIARG_ITEM], dnl
		_OG_MULTIARG_ITEM($1, $[1], $[2], $[3]))
	pushdef([OG_MULTIARG_MUTEX], dnl
		_OG_MULTIARG_MUTEX($1, $[1], $[2], $[3], $[4]))
	$5
	popdef([OG_MULTIARG_ITEM])
	popdef([OG_MULTIARG_MUTEX])
	pushdef([OG_MULTIARG_ITEM], $[1])
	pushdef([OG_MULTIARG_MUTEX], [])
	pushdef([og_items],
		patsubst(
			patsubst(
				patsubst(
					patsubst([$5], [[	 ]+], []),
			[
], [ ]), [^ +], []), [ +$], [])) dnl autoconf is soooo brilliant, is it not? :)
	popdef([OG_MULTIARG_MUTEX])
	popdef([OG_MULTIARG_ITEM])

	pushdef([OG_HELP_STRING], [AS_HELP_STRING($[1], $[2])])
	pushdef([og_desc], [dnl
patsubst([$2], [^[	]AS_HELP_STRING], [AS_HELP_STRING])])
	pushdef([og_helps], [dnl
patsubst(patsubst(patsubst([OG_MULTIARG_HELP_STRINGS($5)], [^[	]+], []), [[	]+$], []), [^[	]*
], [])])

	dnl I personally shoot everybody who fiddles with the whitespace here!!!!
	AC_ARG_WITH($1, [
og_desc[]ifelse($5,[og_any],,[
AS_HELP_STRING([], OG_MULTIARG_POSSVAL(og_items))
AS_HELP_STRING([], OG_MULTIARG_MORE_ON_POSSVALS)
og_helps])[]ifelse($4,,,[dnl
AS_HELP_STRING([], [@<:@Default: $4@:>@])])],[

	## now process all supplied options (prepend the default opts)
	OG_MULTIARG_PROCESS($1,og_DefVal,og_items)
	$6
],[
	withval=
	with_[]og_group[]="og_DefVal"
	OG_MULTIARG_PROCESS($1,og_DefVal,og_items)
	$7
	# end --with-$1
])

	dnl AC_MSG_RESULT([Option Group --with-$1 $og_with_$1])
	popdef([og_desc])
	popdef([og_helps])
	popdef([og_items])
	popdef([og_DefVal])
	popdef([og_group])
])dnl

dnl Print a summary for OG_ARG_WITH options
dnl
dnl $1 = option name
dnl $2 = headline for option
dnl $3 = per-line indentation
AC_DEFUN([OG_ARG_SUMMARY], [# Option Group --with-$1 (arg)
	result="uncertain"
	pushdef([og_group], [translit([$1],[-],[_])])
	pushdef([indent], [ifelse([$3],[],[],[$3])])

	if test -z "$og_[]og_group[]_opts"; then
		if eval "test \"\$have_[]og_group\" = \"yes\" -a \
			\"\$with_[]og_group\" = \"yes\""; then
			result="yes"
		elif eval "test \"\$have_[]og_group\" = \"yes\" -a \
			\"\$with_[]og_group\" != \"yes\""; then
			result="yes+"
		elif eval "test \"\$have_[]og_group\" != \"yes\" -a \
			\"\$with_[]og_group\" = \"yes\""; then
			result="no*"
		else
			result="no"
		fi
		echo "indent[$2]: $result"
	else
		echo "indent[$2]:"
		for og_val in `expr "$og_[]og_group[]_opts"`; do
			if test "$og_val" = "auto"; then
				## do not list auto as option
				:
			elif test "$have_[]og_group" = "$og_val" -a \
				"$with_[]og_group" = "auto"; then
				echo "indent[  ](X) $og_val (auto)"
			elif test "$have_[]og_group" = "$og_val"; then
				echo "indent[  ](X) $og_val"
			else
				echo "indent[  ]( ) $og_val"
			fi
		done
	fi

	popdef([indent])
	popdef([og_group])
])

dnl Print a summary for OG_MULTIARG_WITH options
dnl
dnl $1 = option name
dnl $2 = headline for option
AC_DEFUN([OG_MULTIARG_SUMMARY], [# Option Group --with-$1 (multiarg)
	tmp_enabled=
	tmp_enabled_verb=
	tmp_disabled=
	tmp_omitted=
	tmp_omitted_verb=
	pushdef([og_group], [translit([$1],[-],[_])])
	pushdef([og_items], [ifelse([$4],[],[$sxe_og_[]og_group[]_items],[$4])])
	pushdef([og_mutices], [$sxe_og_[]og_group[]_mutices])
	pushdef([indent], [ifelse([$3],[],[],[$3])])

	for og_val in og_items; do
		if eval "test \"\$have_[]og_group[]_${og_val}\" = \"yes\""; then
			if eval "test \"\$with_[]og_group[]_${og_val}\" \
			   != \"yes\""; then
				tmp_enabled="$tmp_enabled $og_val+"
			else
				tmp_enabled="$tmp_enabled $og_val"
			fi

			if eval "test -n \
			   \"\$sxe_og_[]og_group[]_${og_val}_mutexgroup\""; then
				tmp_mtx_grp=$(eval "echo \$sxe_og_[]og_group[]_${og_val}_mutexgroup")
				tmp_mtx_grp_desc=$(eval "echo \$sxe_og_[]og_group[]_${tmp_mtx_grp}_mutexdesc")
				tmp_enabled_verb="$tmp_enabled_verb
echo \"[]indent[  ]  + ${tmp_mtx_grp} (${tmp_mtx_grp_desc})\"
echo \"[]indent[  ]    (X) \$sxe_og_[]og_group[]_${og_val}_short \
(\$sxe_og_[]og_group[]_${og_val}_desc)\""
				tmp_enabled_verb="$tmp_enabled_verb
eval \"\${sum_[]og_group[]_omitted_${tmp_mtx_grp}_verb}\""
				tmp_mtx_grp=
				tmp_mtx_grp_desc=

			else
				tmp_enabled_verb="$tmp_enabled_verb
echo \"[]indent[  ]  - \$sxe_og_[]og_group[]_${og_val}_short \
(\$sxe_og_[]og_group[]_${og_val}_desc)\""
			fi
		elif eval "test \"\$with_[]og_group[]_${og_val}\" = \"yes\" \
			-a -n \"\$omit_[]og_group[]_${og_val}_in_favour_of\" \
			-a -n \"\$sxe_og_[]og_group[]_${og_val}_mutexgroup\""; then
			tmp_omitted="$tmp_omitted $og_val"
			tmp_mtx_grp=$(eval "echo \$sxe_og_[]og_group[]_${og_val}_mutexgroup")

			eval sum_[]og_group[]_omitted_${tmp_mtx_grp}_verb="\"\$sum_[]og_group[]_omitted_${tmp_mtx_grp}_verb
echo \\\"[]indent[  ]    ( ) \$sxe_og_[]og_group[]_${og_val}_short \
(\$sxe_og_[]og_group[]_${og_val}_desc) available but omitted\\\"\""
			tmp_mtx_grp=
		elif eval "test \"\$with_[]og_group[]_${og_val}\" = \"yes\""; then
			tmp_disabled="$tmp_disabled $og_val*"
		else
			tmp_disabled="$tmp_disabled $og_val"
		fi
	done

	if test -z "$tmp_enabled"; then
		tmp_enabled="None."
	fi
	if test -z "$tmp_disabled"; then
		tmp_disabled="None."
	fi
	if test -z "$tmp_omitted"; then
		tmp_omitted="None."
	fi

	echo "indent[$2]:"
	echo "indent  Enabled [$2]: $tmp_enabled"
	eval "$tmp_enabled_verb"
	if test -n "$sxe_og_[]og_group[]_mutices"; then
		echo "indent  Omitted [$2]: $tmp_omitted"
		dnl eval "$tmp_omitted_verb"
	fi
	echo "indent  Disabled [$2]: $tmp_disabled"

	popdef([indent])
	popdef([og_mutices])
	popdef([og_items])
	popdef([og_group])
])

AC_DEFUN([_OG_MULTIARG_ITEM], [dnl
	# $1 option group name,
	# $2 item name
	# $3 short description
	# $4 long description
	pushdef([og_group], [translit([$1],[-],[_])])
	sxe_og_[]og_group[]_items="$sxe_og_[]og_group[]_items $2"
	sxe_og_[]og_group[]_[]$2[]_short="[$3]"
	sxe_og_[]og_group[]_[]$2[]_desc="[$4]"
	popdef([og_group])
])

AC_DEFUN([_OG_MULTIARG_MUTEX], [dnl
	# $1 option group name,
	# $2 mutex name
	# $3 mutex description
	# $4 item names in descending order of favouriteness
	pushdef([og_group], [translit([$1],[-],[_])])
	sxe_og_[]og_group[]_mutices="$sxe_og_[]og_group[]_mutices $2"
	sxe_og_[]og_group[]_[]$2[]_mutexdesc="[$3]"
	sxe_og_[]og_group[]_[]$2[]_mutexitems="[$4]"
	for og_item in [$4]; do
		## bind each mutex item to name the mutex group it belongs to
		eval "sxe_og_[]og_group[]_${og_item}_mutexgroup=\"[$2]\""
	done
	popdef([og_group])
])

AC_DEFUN([OG_CHECK_OPTION], [## arg #1 is with or enable, arg #2 the option
	pushdef([og_group], [$1])
	__foundp="nil"
	for __opt in $sxe_[]og_group[]_options; do
		if test "[$2]" = "$__opt"; then
			__foundp="t"
			break
		fi
	done
	if test "$__foundp" != "nil"; then
		[$3]
		:
	else
		[$4]
		:
	fi
	popdef([og_group])
])


AC_DEFUN([OG_WARN_OBSOLETE_OPTION], [dnl
	## arg1 is the --with option
	## arg2 is the instead string
	## arg3 is the penalty time, 10 seconds by default
	pushdef([OG_OPT], [$1])
	pushdef([OG_INSTEAD], [$2])
	pushdef([OG_PENALTY_TIME], ifelse($3,,10,$3))
	pushdef([OG_PENALTY_TIME_UNIT], ifelse(OG_PENALTY_TIME,1,[second],[seconds]))

	AC_MSG_WARN([You've just been caught using an obsolete option.])
	AC_MSG_WARN([Listen, mate, thou shalt not use ]OG_OPT[,])
	AC_MSG_WARN([as it will disappear soon and thence cause more trouble.])
	AC_MSG_WARN([To migrate cleanly you should use instead:]
	OG_INSTEAD[])
	echo ""
	AC_MSG_WARN([To substantiate my seriousness I will go on strike for ]
OG_PENALTY_TIME[ ]OG_PENALTY_TIME_UNIT[ now!])
	sleep OG_PENALTY_TIME
	echo ""

	popdef([OG_OPT])
	popdef([OG_INSTEAD])
	popdef([OG_PENALTY_TIME])
	popdef([OG_PENALTY_TIME_UNIT])
])dnl OG_WARN_OBSOLETE_OPTION
