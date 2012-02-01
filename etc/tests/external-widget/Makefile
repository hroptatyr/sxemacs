CFLAGS += -Xc -g
EMACSHOME = ../../..
EMACSLIBDIR = $(EMACSHOME)/editor/src
EMACSINCLUDEDIR = $(EMACSHOME)/editor/src
MOTIFHOME = /opt/SUNWmotif

CPPFLAGS += -I$(OPENWINHOME)/include
LDFLAGS +=  -L$(OPENWINHOME)/lib -R$(OPENWINHOME)/lib

CPPFLAGS += -I$(MOTIFHOME)/include
LDFLAGS += -L$(MOTIFHOME)/lib -R$(MOTIFHOME)/lib

CPPFLAGS += -I$(EMACSINCLUDEDIR)
LDFLAGS += -L$(EMACSLIBDIR)

LDFLAGS += -R$(OPENWINHOME)/lib:$(EMACSLIBDIR) -g

TM_OBJS = test-ew-motif.o
TXL_OBJS = test-ew-xlib.o
MOTIF_LIBS = -Bstatic -lextcli_Xm -Bdynamic -lXm -lXt -ltt -lX11 -lgen -lm
XLIB_LIBS = -Bstatic -lextcli_Xlib -Bdynamic -ltt -lX11 -lgen -lm

all: test-ew-motif

test-ew-motif: $(TM_OBJS)
	$(CC) -o $@ $(LDFLAGS) $(TM_OBJS) $(MOTIF_LIBS)

test-ew-xlib: $(TXL_OBJS)
	$(CC) -o $@ $(LDFLAGS) $(TXL_OBJS) $(XLIB_LIBS)
