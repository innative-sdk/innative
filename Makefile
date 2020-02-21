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
CPPFLAGS := $(CPPFLAGS) -Iinclude -Ibin/llvm/include -Ibin/lld/include
CPPFLAGS += -Wall -Wshadow -Wno-attributes -Wno-unknown-pragmas -Wno-missing-braces \
	    -Wno-unused-function -Wno-comment -Wno-char-subscripts -Wno-sign-compare \
	    -Wno-unused-variable -Wno-switch
LIBS     :=
LDFLAGS  := -L$(LIBDIR) -Lbin/llvm/lib -Lbin/lld/lib

# Destination settings
PREFIX  ?= /usr/local
DESTDIR ?=

ifeq ($(MAKECMDGOALS), debug)
CPPFLAGS += -DDEBUG -g
else
CPPFLAGS += -DNDEBUG -O3
endif

debug: innative-env innative-test-embedding innative innative-cmd innative-test innative-stub
all: innative-env innative-test-embedding innative innative-cmd innative-test innative-stub

clean: innative-env-clean innative-test-embedding-clean innative-clean innative-cmd-clean innative-test-clean innative-stub-clean
	#$(RM) -r $(LIBDIR)
	#$(RM) -r $(BINDIR)
	$(RM) -r $(OBJDIR)

install: all
	mkdir -p $(DESTDIR)$(PREFIX)/include/innative/
	mkdir -p $(DESTDIR)$(PREFIX)/lib
	cp include/innative/*.h $(DESTDIR)$(PREFIX)/include/innative/
	cp $(LIBDIR)/libinnative.so $(DESTDIR)$(PREFIX)/lib/libinnative.so

dist: all
	mkdir -p innative-posix-runtime-x64/
	cp bin/innative.a innative-posix-runtime-x64/
	cp bin/libinnative.so innative-posix-runtime-x64/
	cp bin/innative-test-embedding.a innative-posix-runtime-x64/
	cp bin/innative-env.a innative-posix-runtime-x64/
	cp bin/innative-env-d.a innative-posix-runtime-x64/
	cp bin/innative-cmd innative-posix-runtime-x64/
	tar -czf innative-posix-runtime-x64.tar.gz innative-posix-runtime-x64/
	mkdir -p innative-posix-sdk-x64/bin/
	mkdir -p innative-posix-sdk-x64/include/innative/
	mkdir -p innative-posix-sdk-x64/scripts/
	mkdir -p innative-posix-sdk-x64/spec/test/core/
	mv innative-posix-runtime-x64/* innative-posix-sdk-x64/bin/
	rm -r innative-posix-runtime-x64/
	cp bin/innative-test innative-posix-sdk-x64/bin/
	cp bin/innative-stub.a innative-posix-sdk-x64/bin/
	cp include/innative/*.h innative-posix-sdk-x64/include/innative/
	cp scripts/*.wat innative-posix-sdk-x64/scripts/
	cp scripts/*.wasm innative-posix-sdk-x64/scripts/
	cp spec/test/core/*.wast innative-posix-sdk-x64/spec/test/core/
	tar -czf innative-posix-sdk-x64.tar.gz innative-posix-sdk-x64/
	rm -r innative-posix-sdk-x64/

uninstall:
	$(RM) -r $(DESTDIR)$(PREFIX)/include/innative
	$(RM) $(DESTDIR)$(PREFIX)/lib/libinnative.so

benchmarks: benchmark_n-body.wasm benchmark_fib.wasm benchmark_fannkuch-redux.wasm debugging.wasm funcreplace.wasm

%.wasm: innative-test/%.cpp
	$(CC) $< -g -o scripts/$@ wasm_malloc.c --target=wasm32-unknown-unknown-wasm -nostdlib --optimize=3 -Xlinker --no-entry -Xlinker --export-dynamic
  
.PHONY: all clean install uninstall benchmarks debug

include innative-env/Makefile
include innative/Makefile
include innative-cmd/Makefile
include innative-test/Makefile
include innative-stub/Makefile
include innative-test-embedding/Makefile
