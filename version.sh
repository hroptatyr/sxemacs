#!/bin/sh

oldpwd=$PWD

cd $srcdir

emacs_is_beta=t
emacs_major_version=$(tla versions|tail -n1|sed -e s/"^.*--"//|cut -d . -f1)
emacs_minor_version=$(tla versions|tail -n1|sed -e s/"^.*--"//|cut -d . -f2)
emacs_beta_version=$(tla versions|tail -n1|sed -e s/"^.*--"//|cut -d . -f3)
sxemacs_codename="BMW"
sxemacs_arch_version="$(tla logs -f|tail -n1)"
emacs_kit_version=
infodock_major_version=4
infodock_minor_version=0
infodock_build_version=8

cd $oldpwd
