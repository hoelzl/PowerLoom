# Makefile for primal C++ support

# Version: Makefile,v 1.22 2010/10/11 22:05:31 hans Exp

SYSTEMS-ROOT = ../..
STELLA-ROOT = ..

# g++ settings:
CXX   = g++
CC    = gcc
CFLAGS = -w -O2 -g -fno-common -fPIC
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
	  if (test ! -f ./config.status); then \
	    ./configure $(shell if [ $(OS) = "MINGW" ]; then echo --disable-threads; fi); \
	  fi; \
	  $(MAKE) $(MFLAGS) MY_CFLAGS="$(CFLAGS)" CPPFLAGS="$(CFLAGS)"; \
          cd ..; \
          cp -p gc/.libs/* $(SYSTEMS-ROOT)/lib; \
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
