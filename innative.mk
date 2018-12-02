TARGET := libinnative.so
SRCDIR := innative
BUILDDIR := bin
OBJDIR := bin/obj
C_SRCS := $(wildcard $(SRCDIR)/*.c)
CXX_SRCS := $(wildcard $(SRCDIR)/*.cpp)
INCLUDE_DIRS := include llvm/include llvm/tools/lld/include bin/llvm/include bin/llvm/tools/lld/include
LIBRARY_DIRS := bin/llvm/lib
LIBRARIES := rt dl pthread LLVM-8svn lld-8svn

CPPFLAGS += -fPIC -std=c++11 -msse2 -mcx16 -Wall -Wno-attributes -Wno-unknown-pragmas -Wno-reorder -Wno-missing-braces -Wno-unused-function -Wno-char-subscripts -fsanitize=signed-integer-overflow -fuse-ld=gold -Wno-class-memaccess -Wno-parentheses
LDFLAGS += -msse2 -mcx16 -shared

include base.mk

distclean:
	@- $(RM) $(OBJS)
	@- $(RM) -r $(OBJDIR)

PREFIX = /usr

.PHONY: install
install:
	test -d $(PREFIX) || mkdir $(PREFIX)
	test -d $(PREFIX)/lib || mkdir $(PREFIX)/lib
	test -d $(PREFIX)/include || mkdir $(PREFIX)/include
	test -d $(PREFIX)/include/$(SRCDIR) || mkdir $(PREFIX)/include/$(SRCDIR)
	cp $(BUILDDIR)/$(TARGET) $(PREFIX)/lib/$(TARGET)
	cp include/$(SRCDIR)/*.h $(PREFIX)/include/$(SRCDIR)/

.PHONY: uninstall
uninstall:
	rm -f $(PREFIX)/lib/$(TARGET)
	rm -f -r $(PREFIX)/include/$(SRCDIR)