# -*- makefile -*-
#
# Makefile for irchat
#
# $Id: Makefile,v 1.1 1996/12/19 15:04:43 tri Exp $
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

EMACSCMD= emacs
EMACS	= $(EMACSCMD) -batch -q -l ./setpath.el
TAR	= gtar


# object order is important so compilation may take in order.
OBJS 	= \
	irchat-globals.elc	\
	irchat-vars.elc		\
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
	irchat-main.elc

SRCS	= $(OBJS:.elc=.el)

all:
	echo "Usage: make (gnuemacs|xemacs)"
	exit 1

irchat-build: irchat.elc irchat.info

gnuemacs:
	$(MAKE) EMACSCMD=emacs irchat-build

xemacs:
	$(MAKE) EMACSCMD=xemacs irchat-build

%.elc: %.el
	$(EMACS) -f batch-byte-compile $(DEFSUBST_SRC) $<

irchat.elc:	$(OBJS)
	rm -f $@
	cat $(OBJS) > $@

#irchat.texinfo:
#	cat i-irchat.texi i-etiq.texi i-overview.texi 	\
#	    i-setup.texi i-command.texi i-custom.texi 	\
#	    i-authors.texi 				> irchat.texinfo

irchat.info:	irchat.texinfo
	-$(EMACS) -q irchat.texinfo -f texinfo-format-buffer -f save-buffer

clean: 
	-rm -f $(OBJS)

tidy:
	$(MAKE) clean 
	-rm -f irchat.elc irchat.info

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
