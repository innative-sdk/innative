TARGET := innative-env.a
TARGET_DEBUG := innative-env_d.a
SRCDIR := innative-env
BUILDDIR := bin
OBJDIR := bin/obj
C_SRCS := $(wildcard $(SRCDIR)/*.c)
INCLUDE_DIRS := include

CPPFLAGS += -fPIC -Wall -Wshadow -Wno-attributes -Wno-unknown-pragmas -Wno-missing-braces -Wno-unused-function -Wno-comment -Wno-char-subscripts -Wno-sign-compare -Wno-unused-variable -Wno-switch -nostdlib

C_OBJS := ${C_SRCS:.c=.o}
OBJS := $(addprefix $(OBJDIR)/c/,$(C_OBJS))
OBJS_DEBUG := $(addprefix $(OBJDIR)/debug/c/,$(C_OBJS))
CPPFLAGS += $(foreach includedir,$(INCLUDE_DIRS),-I$(includedir)) -Wl,-rpath -Wl,. 

.PHONY: all clean distclean

all: $(BUILDDIR)/$(TARGET) $(BUILDDIR)/$(TARGET_DEBUG)
	
$(OBJDIR)/c/%.o : %.c
	+@[ -d $(OBJDIR) ] || mkdir -p $(OBJDIR)
	+@[ -d $(OBJDIR)/c ] || mkdir -p $(OBJDIR)/c
	+@[ -d $(OBJDIR)/c/$(SRCDIR) ] || mkdir -p $(OBJDIR)/c/$(SRCDIR)
	$(COMPILE.c) $< -o $@
	
$(OBJDIR)/debug/c/%.o : %.c
	+@[ -d $(OBJDIR) ] || mkdir -p $(OBJDIR)
	+@[ -d $(OBJDIR)/debug ] || mkdir -p $(OBJDIR)/debug
	+@[ -d $(OBJDIR)/debug/c ] || mkdir -p $(OBJDIR)/debug/c
	+@[ -d $(OBJDIR)/debug/c/$(SRCDIR) ] || mkdir -p $(OBJDIR)/debug/c/$(SRCDIR)
	$(COMPILE.c) $< -o $@
  
$(BUILDDIR)/$(TARGET): $(OBJS)
	+@[ -d $(BUILDDIR) ] || mkdir -p $(BUILDDIR)
	$(AR) rcs $@ $(OBJS)

$(BUILDDIR)/$(TARGET_DEBUG): CPPFLAGS += -ggdb -fvar-tracking-assignments
$(BUILDDIR)/$(TARGET_DEBUG): $(OBJS_DEBUG)
	+@[ -d $(BUILDDIR) ] || mkdir -p $(BUILDDIR)
	$(AR) rcs $@ $(OBJS_DEBUG)

clean: distclean
	@- $(RM) $(BUILDDIR)/$(TARGET)
	@- $(RM) $(BUILDDIR)/$(TARGET_DEBUG)
  
distclean:
	@- $(RM) $(OBJS)
	@- $(RM) $(OBJS_DEBUG)
	@- $(RM) -r $(OBJDIR)

