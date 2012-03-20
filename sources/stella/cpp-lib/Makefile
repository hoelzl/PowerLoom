# Makefile for primal C++ support

# Version: Makefile,v 1.17 2003/02/11 01:43:03 hans Exp

STELLA-ROOT = ..

# g++ settings:
CXX   = g++
CC    = gcc
CFLAGS = -w -O2 -g
CXXFLAGS = $(CFLAGS)

# Comment GC-SWITCHES and GC-LIB to not use the garbage collector:
GC-SWITCHES = -DSTELLA_USE_GC
GC-CFLAGS = $(GC-SWITCHES) -I$(STELLA-ROOT)/cpp-lib/gc/include
GC-LIB = gc

override INCLUDE += -I$(STELLA-ROOT)/..

CXX-SRCS = cpp-primal.cc
CXX-OBJS = $(CXX-SRCS:.cc=.o) 
HEADERS = $(CXX-SRCS:.cc=.hh)

all: $(CXX-OBJS) $(GC-LIB)

# Build the garbage collector if necessary (if you want to customize the
# garbage collector, cd to its directory, `make' it by hand with the
# customized switches and then `make' STELLA):
.PHONY: $(GC-LIB)
$(GC-LIB):
	if (test -n "$(GC-SWITCHES)"); then \
	  cd gc; \
	  if (test ! -e ./config.status); then \
	    ./configure; \
	  fi; \
	  $(MAKE) $(MFLAGS) MY_CFLAGS="$(CFLAGS)" CPPFLAGS="$(CFLAGS)"; \
	fi

#
# Default rules
# 

# default rule for files in CXX-SRCS
.cc.o: $(HEADERS)
	$(CXX) $(CXXFLAGS) $(GC-CFLAGS) -c $(INCLUDE) $<

#
# Cleaning up
#

.PHONY: clean extraclean

clean:
	/bin/rm -f $(PROG) $(LIB) *.o *.a core \#*

extraclean: clean
	/bin/rm -f *~ .*~ \#* .\#*
	cd gc; rm -f $(GC-LIB); $(MAKE) $(MFLAGS) clean
