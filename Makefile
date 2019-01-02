# Compiler directories
OBJDIR ?= bin/obj
BINDIR ?= bin
LIBDIR ?= bin

# Compiler selection
AR    ?= ar
CC    ?= gcc
CXX   ?= g++
CXXLD ?= $(CXX)

# Compiler flags
CPPFLAGS := $(CPPFLAGS) -Iinclude -Ibin/llvm/include -Illvm/include -Illvm/tools/lld/include
CPPFLAGS += -Wall -Wshadow -Wno-attributes -Wno-unknown-pragmas -Wno-missing-braces \
	    -Wno-unused-function -Wno-comment -Wno-char-subscripts -Wno-sign-compare \
	    -Wno-unused-variable -Wno-switch
LIBS     :=
LDFLAGS  := -L$(LIBDIR) -Lbin/llvm/lib

# Destination settings
PREFIX  ?= /usr/local
DESTDIR ?=

all: innative-env innative innative-cmd innative-test
clean: innative-env-clean innative-clean innative-cmd-clean innative-test-clean
	#$(RM) -r $(LIBDIR)
	#$(RM) -r $(BINDIR)
	$(RM) -r $(OBJDIR)

install: all
	mkdir -p $(DESTDIR)$(PREFIX)/include/innative/
	mkdir -p $(DESTDIR)$(PREFIX)/lib
	cp include/innative/*.h $(DESTDIR)$(PREFIX)/include/innative/
	cp $(LIBDIR)/libinnative.so $(DESTDIR)$(PREFIX)/lib/libinnative.so

uninstall:
	$(RM) -r $(DESTDIR)$(PREFIX)/include/innative
	$(RM) $(DESTDIR)$(PREFIX)/lib/libinnative.so

.PHONY: all clean install uninstall

include innative-env/Makefile
include innative/Makefile
include innative-cmd/Makefile
include innative-test/Makefile
