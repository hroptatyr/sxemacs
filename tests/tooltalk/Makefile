### Makefile - Makefile for ToolTalk testing executables

# Author:	Vladimir Ivanovic <vladimir@Eng.Sun.COM>
# Maintainer:	Vladimir Ivanovic <vladimir@Eng.Sun.COM>
# Created:	08 March 1995
# Keywords:	ToolTalk, testing

# Copyright (C) 1995 Sun Microsystems, Inc

### Commentary:
#   This is SPARCworks & Solaris 2.x-centric.

### Code:

CC       = cc -Xc
CPPFLAGS = -I/usr/openwin/include
CFLAGS   = -g -v -DNeedFunctionPrototypes
LDFLAGS  = -L/usr/openwin/lib -R/usr/openwin/lib -ltt

all:	emacs-eval load-file make-client-frame

clean:
	rm -f *.o emacs-eval load-file make-client-frame

### Makefile ends here.
