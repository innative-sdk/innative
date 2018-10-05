TARGET := innative-cmd
SRCDIR := innative-cmd
BUILDDIR := bin
OBJDIR := bin/obj
C_SRCS := $(wildcard $(SRCDIR)/*.c)
CXX_SRCS := $(wildcard $(SRCDIR)/*.cpp)
INCLUDE_DIRS := include
LIBRARY_DIRS := 
LIBRARIES := innative rt pthread

CPPFLAGS += -std=c++14 -pthread -DLIBICONV_PLUG -Wall -Wshadow -Wno-attributes -Wno-unknown-pragmas -Wno-reorder -Wno-missing-braces -Wno-unused-function -Wno-comment -Wno-char-subscripts -Wno-sign-compare -Wno-unused-variable -Wno-switch -fsanitize=signed-integer-overflow -fuse-ld=gold -Wno-class-memaccess
LDFLAGS += -L./bin/
  
include base.mk
  
distclean:
	@- $(RM) $(OBJS)
	@- $(RM) -r $(OBJDIR)

