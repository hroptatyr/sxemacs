#!/bin/sh

# This file builds the release kits for both cygwin and win32. You
# must have both environments configured for it to work properly. In
# particular you must provide a suitable value for NATIVE_ZLIB_DIR.

# configuration
NATIVE_ZLIB_DIR=/usr/local/mingw/lib
PROGRAM_FILES='c:/Program Files/XEmacs'
TMPINSTALL=/tmp/local
# no configuration past this point

INSTALL=
FILES=
BUILD=1

for OPT in $*
do
    case $OPT in
    --install) INSTALL=1;;
    --installonly) INSTALL=1; BUILD='';;
    --help) echo "usage: build-msw-release.sh [--install]" && exit;;
    --*) ;;
    *) FILES="$FILES $OPT";;
    esac
done

# pick up version info
. version.sh

# decide on names
emacs_ver=${emacs_major_version}.${emacs_minor_version}.${emacs_beta_version}
cygwin_tarball=xemacs-i686-pc-cygwin-${emacs_ver}${emacs_kit_version}.tar.gz
win32_tarball=xemacs-i586-pc-win32-${emacs_ver}${emacs_kit_version}.tar.gz

DISTDIR=`pwd`/windows

# check to see if we should build
if test "$BUILD" = "1"
then

echo "Building the mswindows ${emacs_ver} release"

# cleanup everything first
if [ -f Makefile ] ; then
    make distclean
fi

# nuke the dist dir.
rm -rf windows

# create a dist directory 
mkdir -p windows/cygwin32
mkdir -p windows/win32
mkdir -p /usr/local
mkdir -p ${TMPINSTALL}

# first build win32
(cd nt;
  nmake -f xemacs.mak clean;
  nmake -f xemacs.mak)
(cd "${PROGRAM_FILES}";
    rm -rf ./XEmacs-${emacs_ver})
(cd nt;
  nmake -f xemacs.mak install;
  nmake -f xemacs.mak clean)

# now build cygwin
./configure --with-dragndrop --with-postgresql=no --with-x=no \
    --bindir=/usr/local/bin/i686-pc-cygwin --with-site-lisp=yes \
    --with-ipv6-cname=no --with-netinstall
make CFLAGS=-O3 MINGW_ZLIB_DIR=${NATIVE_ZLIB_DIR} beta

# deal with the netinstaller
(cd netinstall;
    strip setup.exe)
cp netinstall/setup.exe windows

# the win32 tar ball needs setup.exe
cp netinstall/setup.exe \
    "${PROGRAM_FILES}"/XEmacs-${emacs_ver}/i586-pc-win32

# make the win32 tarball
(cd "${PROGRAM_FILES}";
    tar czvf ${DISTDIR}/win32/${win32_tarball} \
	./XEmacs-${emacs_ver})

# make the tarball
make prefix=${TMPINSTALL} bindir=${TMPINSTALL}/bin/i686-pc-cygwin install
(cd ${TMPINSTALL};
    tar czvf ${DISTDIR}/cygwin32/${cygwin_tarball} \
    ./bin/i686-pc-cygwin \
    ./lib/xemacs-${emacs_ver} \
    ./lib/xemacs/lock \
    ./man/man1/ctags.1 \
    ./man/man1/etags.1 \
    ./man/man1/gnuattach.1 \
    ./man/man1/gnuclient.1 \
    ./man/man1/gnudoit.1 \
    ./man/man1/gnuserv.1 \
    ./man/man1/xemacs.1;
    rm -rf bin lib man)

# figure out the ini file.
cygwin_tarball_size=`ls -l windows/cygwin32/${cygwin_tarball} | awk '{ print $5; }'`
win32_tarball_size=`ls -l windows/win32/${win32_tarball} | awk '{ print $5; }'`

(cd netinstall;
    make CYGWIN_SIZE=${cygwin_tarball_size} \
	WIN32_SIZE=${win32_tarball_size} \
	KIT_VERSION=${emacs_kit_version} setup-bin.ini )
cp netinstall/setup-bin.ini windows

# tidy up
make distclean

fi
# end of build

# optionally install to the ftp site
if test "$INSTALL" != ""
then
    echo "Installing the mswindows ${emacs_ver} release"
    scp -r -oUser=slb -oProtocol=1 windows/* \
	ftp.xemacs.org:/pub/xemacs/windows
    # update setup.ini
    ssh -1 -l slb ftp.xemacs.org 'cd /pub/xemacs/windows; sh makeini.sh'
fi

