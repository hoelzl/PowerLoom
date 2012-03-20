# Makefile for PowerLoom manual(s)
#
# Version: Makefile,v 1.5 1997/09/23 04:45:20 hans Exp
#
# Format the PowerLoom manual TeXINFO file and translate it into 
# DVI, PS, INFO, and HTML format.


MANUAL-MAKEFILE = ../../stella/doc/Makefile

manual.pdf manual.html manual.info manual.dvi all:
	$(MAKE) -f $(MANUAL-MAKEFILE) $(MFLAGS) $@

#
# Cleaning up
#

.PHONY: clean extraclean

clean:
	-$(MAKE) -f $(MANUAL-MAKEFILE) $(MFLAGS) clean

extraclean: clean
	-$(MAKE) -f $(MANUAL-MAKEFILE) $(MFLAGS) extraclean
