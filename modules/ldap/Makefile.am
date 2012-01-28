# Help the Developers and yourself. Just use the C locale and settings
# for the compilation. They can still be overriden by make LANG=<whatever>
# but that is general a not very good idea
LANG=C
LC_ALL=C


MODNAME=ldap
MODVER=0.0.0
MODTITLE="SXEmacs module example"

ELLMAKEDOC=@ELLMAKEDOC@
MKINIT=@MKINIT@

noinst_PROGRAMS = eldap.ell
eldap_ell_SOURCES = eldap.c
nodist_EXTRA_eldap_ell_SOURCES = eldap_i.c
eldap_ell_DEPENDENCIES = eldap_i.o eldap_i.c
eldap_ell_CPPFLAGS = @SXEMACS_INCLUDES@
eldap_ell_LINK = $(CCLD) --mod-output=$@
eldap_ell_LDADD = eldap_i.o
eldap_ell_LDFLAGS = @LDFLAGS@ $(XCCLDFLAGS)

top_builddir = @top_builddir@
top_build_prefix = $(top_builddir)/

ETAGS = $(top_builddir)/lib-src/etags
TAGS_DEPENDENCIES = $(ETAGS)

CLEANFILES = ldap_i.c

$(MODNAME)_i.c: $(ldap_ell_SOURCES)
	$(MKINIT) --mod-output=$@ \
	--mod-name=$(MODNAME) --mod-version=$(MODVER) \
	--mod-title=$(MODTITLE) $<

dist_modules_DATA = eldap.ell


#
# Help the SXEmacs developers get nice post-processed source files

## Create preprocessor output (debugging purposes only)
.c.i:
	$(COMPILE) -E -o $@ $<

## Create assembler output (debugging purposes only)
.c.s:
	$(COMPILE) -S -c $(cflags) $<
