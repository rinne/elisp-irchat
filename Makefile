# -*- makefile -*-
#
# Makefile for irchat
#
# $Id: Makefile,v 3.11 1998/05/25 15:22:36 tri Exp $
#

#
# If you don't have jwz's optimizing byte-compiler, uncomment the following
# lines.  A better idea would be to get it from archive.cis.ohio-state.edu
# in the file pub/gnu/emacs/elisp-archive/packages/bytecomp.tar.Z. It also
# comes with emacs19
#
# DEFSUBST_SRC=  defsubst.el
# DEFSUBST_BIN=  defsubst.elc
# DEFSUBST_LOAD= -l ./$(DEFSUBST_BIN)

EMACS    = emacs
EMACSCMD = $(EMACS) -batch -q -l ./setpath.el
TAR	 = gtar

# object order is important so compilation may take in order.
# if, for some strange reason, `irchat-inlines.elc' is needed, put
# it after `irchat-vars.el'

XOBJS 	=                       \
	irchat-globals.elc	\
	irchat-vars.elc		\
	irchat-inlines.elc	\
	irchat-filter.elc	\
	irchat-dcc.elc		\
	irchat-caesar.elc	\
	irchat-000.elc		\
	irchat-200.elc		\
	irchat-300.elc		\
	irchat-400.elc		\
	irchat-500.elc		\
	irchat-commands.elc	\
	irchat-copyright.elc	\
	irchat-cta.elc		\
	irchat-handle.elc	\
	irchat-misc.elc		\
	irchat-timer.elc	\
	irchat-main.elc		\
	irchat-uah-cache.elc	\
	irchat-global-kill.elc	\
	b64.elc			\
	crc32.elc		\
	idea.elc		\
	irchat-crypt.elc	\
	irchat-obsolete.elc

OBJS	= 			\
	irchat-version.elc	\
	$(XOBJS)

SRCS	= $(OBJS:.elc=.el)

SOBJS	= 			\
	irchat-snap-version.elc	\
	$(XOBJS)

SSRCS	= $(SOBJS:.elc=.el)


all:	irchat-build

irchat-build: irchat.elc irchat.info

gnuemacs:
	$(MAKE) EMACS=emacs irchat-build

xemacs:
	$(MAKE) EMACS=xemacs irchat-build

.SUFFIXES: .el .elc

.el.elc:
	$(EMACSCMD) -f batch-byte-compile $(DEFSUBST_SRC) $<

irchat.elc:	$(OBJS)
	rm -f $@
	cat $(OBJS) > $@


### Currently here as test entries..
xemacs-c:
	$(MAKE) EMACS=xemacs irchat-c.elc

gnuemacs-c:
	$(MAKE) EMACS=emacs irchat-c.elc

xemacs-s:
	$(MAKE) EMACS=xemacs irchat-s.elc

gnuemacs-s:
	$(MAKE) EMACS=emacs irchat-s.elc

xemacs-compact: xemacs-c
	-rm -f irchat-c.el
	-mv -f irchat-c.elc irchat.elc

gnuemacs-compact: gnuemacs-c
	-rm -f irchat-c.el
	-mv -f irchat-c.elc irchat.elc

xemacs-snap: xemacs-s
	-rm -f irchat-s.el
	-mv -f irchat-s.elc irchat.elc

gnuemacs-snap: gnuemacs-s
	-rm -f irchat-s.el
	-mv -f irchat-s.elc irchat.elc

irchat-c.elc:	irchat-c.el
	$(EMACSCMD) -f batch-byte-compile $(DEFSUBST_SRC) irchat-c.el

irchat-c.el: $(SRCS)
	cat $(SRCS) > irchat-c.el

irchat-s.elc:	irchat-s.el
	$(EMACSCMD) -f batch-byte-compile $(DEFSUBST_SRC) irchat-s.el

irchat-s.el: $(SSRCS)
	cat $(SSRCS) > irchat-s.el
	rm -f irchat-snap-version.el

irchat-snap-version.el: irchat-version.el
	rm -f irchat-snap-version.el
	sed 's,defconst *irchat-client-version-rcs-snap *nil,defconst irchat-client-version-rcs-snap "'"`date -u '+%Y/%m/%d %H:%M'`"'",' < irchat-version.el > irchat-snap-version.el

irchat.info:	irchat.texinfo
	-$(EMACSCMD) -q irchat.texinfo -f texinfo-format-buffer -f save-buffer

clean: 
	-rm -f $(OBJS) irchat-c.el irchat-s.el

tidy:
	$(MAKE) clean 
	-rm -f irchat.elc irchat-c.elc irchat-s.elc irchat.info

#EXTRAS = 	Makefile irchat-hooks.el			\
#		defsubst.el setpath.el 				\
#		irchat-inlines.el 				\
#		irchat.elc					\
#		irchat.texinfo irchat.info

#dist:	$(SRCS) $(EXTRAS)
#	-@mkdir irchat-dist
#	-@cp $(SRCS) $(EXTRAS) irchat-dist
#	$(TAR) cf dist-`date +%d.%b.%Y`.tar irchat-dist
#	gzip -9 dist-`date +%d.%b.%Y`.tar
#	-@rm -rf irchat-dist

# dependencies (needed ?)

#irchat-000.elc: 	irchat-globals.elc irchat-vars.elc irchat-inlines.el
#irchat-200.elc: 	irchat-globals.elc irchat-vars.elc irchat-inlines.el
#irchat-300.elc: 	irchat-globals.elc irchat-vars.elc irchat-inlines.el
#irchat-400.elc: 	irchat-globals.elc irchat-vars.elc irchat-inlines.el
#irchat-500.elc: 	irchat-globals.elc irchat-vars.elc irchat-inlines.el
#irchat-commands.elc:	irchat-globals.elc irchat-vars.elc irchat-inlines.el
#irchat-commands.elc:	irchat-dcc.elc irchat-caesar.elc
#irchat-cta.elc: 	irchat-globals.elc irchat-vars.elc irchat-inlines.el
#irchat-dcc.elc: 	irchat-globals.elc irchat-vars.elc irchat-inlines.el
#irchat-filter.elc:	irchat-globals.elc irchat-vars.elc irchat-inlines.el
#irchat-handle.elc:	irchat-globals.elc irchat-vars.elc irchat-inlines.el
#irchat-handle.elc:	irchat-filter.elc
#irchat-main.elc:	irchat-globals.elc irchat-vars.elc irchat-inlines.el
#irchat-main.elc:	irchat-filter.elc
#irchat-misc.elc:	irchat-globals.elc irchat-vars.elc irchat-inlines.el
#irchat-misc.elc:	irchat-filter.elc
