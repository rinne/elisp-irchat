# -*- makefile -*-
#
# Makefile for irchat
#
# $Id: Makefile,v 3.6 1997/03/04 22:24:10 tri Exp $
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

OBJS 	= \
	irchat-version.elc	\
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
	b64.elc			\
	crc32.elc		\
	idea.elc		\
	irchat-crypt.elc

SRCS	= $(OBJS:.elc=.el)

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
xemacs2:
	$(MAKE) EMACS=xemacs irchat2.elc

gnuemacs2:
	$(MAKE) EMACS=emacs irchat2.elc

xemacs3: xemacs2
	-rm -f irchat2.el
	-mv -f irchat2.elc irchat.elc

gnuemacs3: gnuemacs2
	-rm -f irchat2.el
	-mv -f irchat2.elc irchat.elc

irchat2.elc:	irchat2.el
	$(EMACSCMD) -f batch-byte-compile $(DEFSUBST_SRC) irchat2.el

irchat2.el: $(SRCS)
	cat $(SRCS) > irchat2.el

irchat.info:	irchat.texinfo
	-$(EMACSCMD) -q irchat.texinfo -f texinfo-format-buffer -f save-buffer

clean: 
	-rm -f $(OBJS) irchat2.el

tidy:
	$(MAKE) clean 
	-rm -f irchat.elc irchat2.elc irchat.info

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
