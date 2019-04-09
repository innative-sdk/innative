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

benchmarks:
	$(CXX) innative-test/benchmark_n-body.cpp wasm_malloc.c --target=wasm32-unknown-unknown-wasm -nostdlib --optimize=3 --output /scripts/benchmark_n-body.wasm -Xlinker --no-entry -Xlinker --export-dynamic
	$(CXX) innative-test/benchmark_fac.cpp wasm_malloc.c --target=wasm32-unknown-unknown-wasm -nostdlib --optimize=3 --output /scripts/benchmark_fac.wasm -Xlinker --no-entry -Xlinker --export-dynamic
	$(CXX) innative-test/benchmark_fannkuch-redux.cpp wasm_malloc.c --target=wasm32-unknown-unknown-wasm -nostdlib --optimize=3 --output /scripts/benchmark_fannkuch-redux.wasm -Xlinker --no-entry -Xlinker --export-dynamic

.PHONY: all clean install uninstall benchmarks

include innative-env/Makefile
include innative/Makefile
include innative-cmd/Makefile
include innative-test/Makefile
