TARGET := innative-test
SRCDIR := innative-test
BUILDDIR := bin
OBJDIR := bin/obj
C_SRCS := $(wildcard $(SRCDIR)/*.c)
CXX_SRCS := $(wildcard $(SRCDIR)/*.cpp)
INCLUDE_DIRS := include
LIBRARY_DIRS := ./bin
LIBRARIES := innative rt pthread stdc++fs
ARCHIVES := bin/innative-env.a

CPPFLAGS += -std=c++17 -msse2 -mcx16 -pthread -DLIBICONV_PLUG -Wall -Wshadow -Wno-attributes -Wno-unknown-pragmas -Wno-reorder -Wno-missing-braces -Wno-unused-function -Wno-comment -Wno-char-subscripts -Wno-sign-compare -Wno-unused-variable -Wno-switch -fsanitize=signed-integer-overflow -fuse-ld=gold -Wno-class-memaccess
LDFLAGS += -msse2 -mcx16 
  
include base.mk
  
distclean:
	@- $(RM) $(OBJS)
	@- $(RM) -r $(OBJDIR)

