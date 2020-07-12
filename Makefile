# Compiler directories
OBJREL = bin/obj
BINREL = bin
LIBREL = bin
export OBJDIR ?= ../$(OBJREL)
export BINDIR ?= ../$(BINREL)
export LIBDIR ?= ../$(LIBREL)

# Compiler selection
export AR    ?= ar
export CC    ?= gcc
export CXX   ?= g++
export CXXLD ?= $(CXX)

# Destination settings
export PREFIX  ?= /usr/local
export DESTDIR ?=
export ARCHIVE_PREFIX ?= $(BINDIR)/llvm/lib/

# Compiler flags
export CPPFLAGS := $(CPPFLAGS) -I../include -I$(BINDIR)/llvm/include
export CPPFLAGS += -Wall -Wshadow -Wno-attributes -Wno-unknown-pragmas -Wno-missing-braces \
	    -Wno-unused-function -Wno-comment -Wno-char-subscripts -Wno-sign-compare \
	    -Wno-unused-variable -Wno-switch
export LDFLAGS  := -L$(LIBDIR) -L$(ARCHIVE_PREFIX)


ifeq ($(MAKECMDGOALS), debug)
export CPPFLAGS += -DDEBUG -g
else
export CPPFLAGS += -DNDEBUG -O3
endif

.PHONY: all clean install uninstall benchmarks debug

all: $(BINREL)/innative-cmd $(BINREL)/innative-test $(LIBREL)/innative-stub.a
debug: $(BINREL)/innative-cmd $(BINREL)/innative-test $(LIBREL)/innative-stub.a

$(BINREL)/innative-env.a: 
	$(MAKE) -C innative-env

$(LIBREL)/libinnative.so: $(BINREL)/innative-env.a $(BINREL)/innative-env-d.a
	$(MAKE) -C innative

$(LIBREL)/innative-stub.a:
	$(MAKE) -C innative-stub

$(LIBREL)/innative-test-embedding.a:
	$(MAKE) -C innative-test-embedding
  
$(BINREL)/innative-test: $(LIBREL)/libinnative.so $(LIBREL)/innative-test-embedding.a
	$(MAKE) -C innative-test
  
$(BINREL)/innative-cmd: $(LIBREL)/libinnative.so
	$(MAKE) -C innative-cmd

clean:
	$(MAKE) -C innative-env clean
	$(MAKE) -C innative clean
	$(MAKE) -C innative-cmd clean
	$(MAKE) -C innative-stub clean
	$(MAKE) -C innative-test clean
	$(MAKE) -C innative-test-embedding clean
	$(RM) -r $(OBJREL)

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
	$(CC) $< -o scripts/$@ wasm_malloc.c --target=wasm32-unknown-unknown-wasm -nostdlib --optimize=3 -Xlinker --no-entry -Xlinker --export-dynamic

#innative-cmd: make -f innative-cmd/Makefile
#innative-test: make -f innative-test/Makefile
#innative-stub: make -f innative-stub/Makefile
#innative-test-embedding: make -f innative-test-embedding/Makefile
