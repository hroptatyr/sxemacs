#
# Sample makefile for a simple Emacs module.
# This is slightly more complicated than would normally be the case,
# as this makefile has been tailored to work in the Emacs source tree.
# For samples of how to compile modules outside of the source tree
# (as would be the case if a user had downloaded a module and wanted
# to compile it for use within Emacs), see the samples in the sub-directory
# 'installed'.
#

SHELL=/bin/sh
RM=rm -f
CC=../../lib-src/ellcc
CFLAGS=-I. -I../../src
LD=$(CC) --mode=link
MKINIT=$(CC) --mode=init

SRCS=zlib.c
OBJS=$(SRCS:.c=.o)

.c.o:
	$(CC) $(CFLAGS) -c $<

MODNAME=zlib
MODVER=1.0.4
MODTITLE="ZLIB compression library interface"

all: $(MODNAME).ell

distclean: clean

clean:
	$(RM) $(MODNAME).ell $(OBJS) zlib_i.o zlib_i.c

$(MODNAME).ell: $(OBJS) zlib_i.o
	$(LD) --mod-output=$@ $(OBJS) zlib_i.o

zlib_i.o: zlib_i.c
zlib_i.c: $(SRCS)
	ELLMAKEDOC=../../lib-src/make-docfile $(MKINIT) --mod-output=$@ \
	--mod-name=$(MODNAME) --mod-version=$(MODVER) \
	--mod-title=$(MODTITLE) $(SRCS)

