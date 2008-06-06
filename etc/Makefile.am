## SXEmacs Makefile.am

## Copyright (C) 2007 Steve Youngs.

## This file is part of SXEmacs.

## SXEmacs is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## SXEmacs is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.


## Process this file with automake to produce Makefile.in

# Help the Developers and yourself. Just use the C locale and settings
# for the compilation. They can still be overriden by make LANG=<whatever>
# but that is general a not very good idea
LANG=C
LC_ALL=C

builddir = @builddir@
srcdir = @srcdir@
top_builddir = @top_builddir@
top_srcdir = @top_srcdir@
abs_top_builddir = @abs_top_builddir@
abs_top_srcdir = @abs_top_srcdir@

etcdir = ${datadir}/${instvardir}/etc
icondir = ${datadir}/${instvardir}/etc
mandir = ${datadir}/man
manonedir = ${mandir}/man1
toolbardir = ${etcdir}/toolbar
customdir = ${etcdir}/custom
photosdir = ${etcdir}/photos
eosdir = ${etcdir}/eos

## missing stuff: aliases.ksh editclient.sh xemacs-fe.sh
##	ms-kermit ms-kermit-7bit
## missing dirs: sparcworks tests idd

dist_etc_DATA = BETA CHARSETS CODING-STANDARDS CODINGS			\
	COPYING.LIB DEBUG DISTRIB ETAGS.EBNF ETAGS.README Emacs.ad      \
	GNU HELLO InstallGuide LPF MACHINES MAILINGLISTS MORE.STUFF     \
	MOTIVATION NEWS NEWS.XEmacs ONEWS.XEmacs OONEWS.XEmacs PACKAGES \
	README README.HYPERBOLE README.OO-BROWSER SERVICE TERMS		\
	TUTORIAL TUTORIAL.de TUTORIAL.fr TUTORIAL.hr TUTORIAL.ja	\
	TUTORIAL.ko TUTORIAL.no TUTORIAL.pl TUTORIAL.ro TUTORIAL.ru	\
	TUTORIAL.se TUTORIAL.th						\
	XKeysymDB codenames emacskeys.sco emacsstrs.sco gnuserv.README	\
	gtkrc package-index.LATEST.gpg refcard.tex rgb.txt 		\
	sample.Xdefaults sample.init.el

dist_icon_DATA = cbsx.png cbsxe.png chr.png chrm.png			\
	gnu.xbm gnu.xpm gray1.xbm recycle.xpm recycle2.xpm sink.xbm	\
	sxemacs-beta.xpm sxemacs-icon.png sxemacs-icon.xpm		\
	sxemacs-icon2.png sxemacs-icon2.xpm sxemacs-icon3.png		\
	sxemacs-icon3.xpm sxemacs.xbm sxemacs.xpm trash.xpm

## do we really want to install this old (and unrevised) stuff?
dist_manone_DATA = ctags.1 etags.1 gnuattach.1 gnuclient.1 gnudoit.1	\
	gnuserv.1 sxemacs.1

dist_toolbar_DATA =				\
	toolbar/compile-cap-up.xpm		\
	toolbar/compile-cap-xx.xpm		\
	toolbar/compile-dn.xbm			\
	toolbar/compile-up.xbm			\
	toolbar/compile-up.xpm			\
	toolbar/compile-xx.xbm			\
	toolbar/compile-xx.xpm			\
	toolbar/copy-cap-up.xpm			\
	toolbar/copy-cap-xx.xpm			\
	toolbar/copy-dn.xbm			\
	toolbar/copy-up.xbm			\
	toolbar/copy-up.xpm			\
	toolbar/copy-xx.xbm			\
	toolbar/copy-xx.xpm			\
	toolbar/cut-cap-up.xpm			\
	toolbar/cut-cap-xx.xpm			\
	toolbar/cut-dn.xbm			\
	toolbar/cut-up.xbm			\
	toolbar/cut-up.xpm			\
	toolbar/cut-xx.xbm			\
	toolbar/cut-xx.xpm			\
	toolbar/debug-cap-up.xpm		\
	toolbar/debug-cap-xx.xpm		\
	toolbar/debug-dn.xbm			\
	toolbar/debug-up.xbm			\
	toolbar/debug-up.xpm			\
	toolbar/debug-xx.xbm			\
	toolbar/debug-xx.xpm			\
	toolbar/disk-cap-up.xpm			\
	toolbar/disk-cap-xx.xpm			\
	toolbar/disk-dn.xbm			\
	toolbar/disk-up.xbm			\
	toolbar/disk-up.xpm			\
	toolbar/disk-xx.xbm			\
	toolbar/disk-xx.xpm			\
	toolbar/file-cap-up.xpm			\
	toolbar/file-cap-xx.xpm			\
	toolbar/file-dn.xbm			\
	toolbar/file-up.xbm			\
	toolbar/file-up.xpm			\
	toolbar/file-xx.xbm			\
	toolbar/file-xx.xpm			\
	toolbar/folder-cap-up.xpm		\
	toolbar/folder-cap-xx.xpm		\
	toolbar/folder-dn.xbm			\
	toolbar/folder-up.xbm			\
	toolbar/folder-up.xpm			\
	toolbar/folder-xx.xbm			\
	toolbar/folder-xx.xpm			\
	toolbar/info-def-cap-up.xpm		\
	toolbar/info-def-cap-xx.xpm		\
	toolbar/info-def-dn.xbm			\
	toolbar/info-def-up.xbm			\
	toolbar/info-def-up.xpm			\
	toolbar/info-def-xx.xbm			\
	toolbar/info-def-xx.xpm			\
	toolbar/info-exit-cap-up.xpm		\
	toolbar/info-exit-cap-xx.xpm		\
	toolbar/info-exit-dn.xbm		\
	toolbar/info-exit-up.xbm		\
	toolbar/info-exit-up.xpm		\
	toolbar/info-exit-xx.xbm		\
	toolbar/info-exit-xx.xpm		\
	toolbar/info-exit.xbm			\
	toolbar/info-exit.xpm			\
	toolbar/info-next-cap-up.xpm		\
	toolbar/info-next-cap-xx.xpm		\
	toolbar/info-next-dn.xbm		\
	toolbar/info-next-up.xbm		\
	toolbar/info-next-up.xpm		\
	toolbar/info-next-xx.xbm		\
	toolbar/info-next-xx.xpm		\
	toolbar/info-next.xbm			\
	toolbar/info-next.xpm			\
	toolbar/info-prev-cap-up.xpm		\
	toolbar/info-prev-cap-xx.xpm		\
	toolbar/info-prev-dn.xbm		\
	toolbar/info-prev-up.xbm		\
	toolbar/info-prev-up.xpm		\
	toolbar/info-prev-xx.xbm		\
	toolbar/info-prev-xx.xpm		\
	toolbar/info-prev.xbm			\
	toolbar/info-prev.xpm			\
	toolbar/info-up-cap-up.xpm		\
	toolbar/info-up-cap-xx.xpm		\
	toolbar/info-up-dn.xbm			\
	toolbar/info-up-up.xbm			\
	toolbar/info-up-up.xpm			\
	toolbar/info-up-xx.xbm			\
	toolbar/info-up-xx.xpm			\
	toolbar/info-up.xbm			\
	toolbar/info-up.xpm			\
	toolbar/last-win-cap-up.xpm		\
	toolbar/last-win-cap-xx.xpm		\
	toolbar/last-win-dn.xbm			\
	toolbar/last-win-up.xbm			\
	toolbar/last-win-up.xpm			\
	toolbar/last-win-xx.xbm			\
	toolbar/last-win-xx.xpm			\
	toolbar/mail-cap-up.xpm			\
	toolbar/mail-cap-xx.xpm			\
	toolbar/mail-dn.xbm			\
	toolbar/mail-up.xbm			\
	toolbar/mail-up.xpm			\
	toolbar/mail-xx.xbm			\
	toolbar/mail-xx.xpm			\
	toolbar/news-cap-up.xpm			\
	toolbar/news-cap-xx.xpm			\
	toolbar/news-dn.xbm			\
	toolbar/news-up.xbm			\
	toolbar/news-up.xpm			\
	toolbar/news-xx.xbm			\
	toolbar/news-xx.xpm			\
	toolbar/next-win-cap-up.xpm		\
	toolbar/next-win-cap-xx.xpm		\
	toolbar/next-win-dn.xbm			\
	toolbar/next-win-up.xbm			\
	toolbar/next-win-up.xpm			\
	toolbar/next-win-xx.xbm			\
	toolbar/next-win-xx.xpm			\
	toolbar/paste-cap-up.xpm		\
	toolbar/paste-cap-xx.xpm		\
	toolbar/paste-dn.xbm			\
	toolbar/paste-up.xbm			\
	toolbar/paste-up.xpm			\
	toolbar/paste-xx.xbm			\
	toolbar/paste-xx.xpm			\
	toolbar/printer-cap-up.xpm		\
	toolbar/printer-cap-xx.xpm		\
	toolbar/printer-dn.xbm			\
	toolbar/printer-up.xbm			\
	toolbar/printer-up.xpm			\
	toolbar/printer-xx.xbm			\
	toolbar/printer-xx.xpm			\
	toolbar/replace-cap-up.xpm		\
	toolbar/replace-cap-xx.xpm		\
	toolbar/replace-dn.xbm			\
	toolbar/replace-up.xbm			\
	toolbar/replace-up.xpm			\
	toolbar/replace-xx.xbm			\
	toolbar/replace-xx.xpm			\
	toolbar/spell-cap-up.xpm		\
	toolbar/spell-cap-xx.xpm		\
	toolbar/spell-dn.xbm			\
	toolbar/spell-up.xbm			\
	toolbar/spell-up.xpm			\
	toolbar/spell-xx.xbm			\
	toolbar/spell-xx.xpm			\
	toolbar/undo-cap-up.xpm			\
	toolbar/undo-cap-xx.xpm			\
	toolbar/undo-dn.xbm			\
	toolbar/undo-up.xbm			\
	toolbar/undo-up.xpm			\
	toolbar/undo-xx.xbm			\
	toolbar/undo-xx.xpm			\
	toolbar/workshop-cap-up.xpm		\
	toolbar/workshop-cap-xx.xpm		\
	toolbar/workshop-up.xpm			\
	toolbar/workshop-xx.xpm

dist_photos_DATA =					\
	photos/adrian.png photos/adrianm.png		\
	photos/ajc.png photos/ajcm.png			\
	photos/baw.png photos/bawm.png			\
	photos/ben.png photos/benm.png			\
	photos/bw.png photos/bwm.png			\
	photos/cthomp.png photos/cthompm.png		\
	photos/dkindred.png photos/dkindredm.png	\
	photos/dmoore.png photos/dmoorem.png		\
	photos/dv.png photos/dvm.png			\
	photos/fabrice.png photos/fabricem.png		\
	photos/golubev.png photos/golubevm.png		\
	photos/heatxsink.png photos/heatxsinkm.png	\
	photos/hniksic.png photos/hniksicm.png		\
	photos/hroptatyr.png photos/hroptatyrm.png	\
	photos/jan.png photos/janm.png			\
	photos/jareth.png photos/jarethm.png		\
	photos/jens.png photos/jensm.png		\
	photos/jmiller.png photos/jmillerm.png		\
	photos/juhp.png photos/juhpm.png		\
	photos/jwz.png photos/jwzm.png			\
	photos/kazz.png photos/kazzm.png		\
	photos/kyle.png photos/kylem.png		\
	photos/larsi.png photos/larsim.png		\
	photos/lg.png photos/lgm.png			\
	photos/martin.png photos/martinm.png		\
	photos/mcook.png photos/mcookm.png		\
	photos/mly.png photos/mlym.png			\
	photos/morioka.png photos/moriokam.png		\
	photos/myrkraverk.png photos/myrkraverkm.png	\
	photos/njsf.png photos/njsfm.png		\
	photos/ograf.png photos/ografm.png		\
	photos/peanuthorst.png photos/peanuthorstm.png  \
	photos/pez.png photos/pezm.png			\
	photos/piper.png photos/piperm.png		\
	photos/rickc.png photos/rickcm.png		\
	photos/slb.png photos/slbm.png			\
	photos/sperber.png photos/sperberm.png		\
	photos/steve.png photos/stevem.png		\
	photos/stig.png photos/stigb.png		\
	photos/stigbm.png photos/stigm.png		\
	photos/thiessel.png photos/thiesselm.png	\
	photos/vin.png photos/vinm.png			\
	photos/vladimir.png photos/vladimirm.png	\
	photos/wmperry.png photos/wmperrym.png

dist_custom_DATA =				\
	custom/check0.xpm			\
	custom/check1.xpm			\
	custom/choose-down.png			\
	custom/choose-up.png			\
	custom/done-down.png			\
	custom/done-up.png			\
	custom/down-pushed.xpm			\
	custom/down.xpm				\
	custom/face.xpm				\
	custom/folder.xpm			\
	custom/open-down.png			\
	custom/open-up.png			\
	custom/option.xpm			\
	custom/radio0.xpm			\
	custom/radio1.xpm			\
	custom/reset-down.png			\
	custom/reset-up.png			\
	custom/right-pushed.xpm			\
	custom/right.xpm			\
	custom/save-down.png			\
	custom/save-up.png			\
	custom/set-down.png			\
	custom/set-up.png			\
	custom/state-down.png			\
	custom/state-up.png			\
	custom/toggle-off-down.png		\
	custom/toggle-off-up.png		\
	custom/toggle-on-down.png		\
	custom/toggle-on-up.png			\
	custom/example-themes/europe-theme.el	\
	custom/example-themes/ex-custom-file	\
	custom/example-themes/example-theme.el

dist_eos_DATA = eos/eos-build.xbm eos/eos-clear-at.xbm			\
	eos/eos-cont.xbm eos/eos-dismiss.xbm eos/eos-down.xbm		\
	eos/eos-evaluate-star.xbm eos/eos-evaluate.xbm eos/eos-fix.xbm	\
	eos/eos-intro.xbm eos/eos-introB.xbm eos/eos-introD.xbm		\
	eos/eos-introDB.xbm eos/eos-run.xbm eos/eos-run2.xbm		\
	eos/eos-step-into.xbm eos/eos-step-over.xbm eos/eos-stop-in.xbm	\
	eos/eos-stop.xbm eos/eos-type.xbm eos/eos-up.xbm

