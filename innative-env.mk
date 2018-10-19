TARGET := innative-env.a
SRCDIR := innative-env
BUILDDIR := bin
OBJDIR := bin/obj
C_SRCS := $(wildcard $(SRCDIR)/*.c)
INCLUDE_DIRS := include

CPPFLAGS += -fPIC -Wall -Wshadow -Wno-attributes -Wno-unknown-pragmas -Wno-missing-braces -Wno-unused-function -Wno-comment -Wno-char-subscripts -Wno-sign-compare -Wno-unused-variable -Wno-switch -nostdlib
  
C_OBJS := ${C_SRCS:.c=.o}
OBJS := $(addprefix $(OBJDIR)/c/,$(C_OBJS))
CPPFLAGS += $(foreach includedir,$(INCLUDE_DIRS),-I$(includedir)) -Wl,-rpath -Wl,. 

.PHONY: all clean distclean

all: $(BUILDDIR)/$(TARGET)
	
$(OBJDIR)/c/%.o : %.c
	+@[ -d $(OBJDIR) ] || mkdir -p $(OBJDIR)
	+@[ -d $(OBJDIR)/c ] || mkdir -p $(OBJDIR)/c
	+@[ -d $(OBJDIR)/c/$(SRCDIR) ] || mkdir -p $(OBJDIR)/c/$(SRCDIR)
	$(COMPILE.c) $< -o $@
	
$(BUILDDIR)/$(TARGET): $(OBJS)
	+@[ -d $(BUILDDIR) ] || mkdir -p $(BUILDDIR)
	$(AR) rcs $@ $(OBJS)

clean: distclean
	@- $(RM) $(BUILDDIR)/$(TARGET)

debug: CPPFLAGS += -ggdb -fvar-tracking-assignments
debug: all
  
distclean:
	@- $(RM) $(OBJS)
	@- $(RM) -r $(OBJDIR)

