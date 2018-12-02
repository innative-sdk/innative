BUILDDIR := bin
OBJDIR := bin/obj
CPPFLAGS += -Wall -Wshadow -Wno-attributes -Wno-unknown-pragmas -Wno-missing-braces -Wno-unused-function -Wno-comment -Wno-char-subscripts -Wno-sign-compare -Wno-unused-variable -Wno-switch

# innative-env
INNATIVE_ENV := innative-env.a
INNATIVE_ENV_DEBUG := innative-env_d.a
INNATIVE_ENV_DIR := innative-env
INNATIVE_ENV_SRCS := $(wildcard $(INNATIVE_ENV_DIR)/*.c)
INNATIVE_ENV_INCLUDE := include

INNATIVE_ENV_C_OBJS := ${INNATIVE_ENV_SRCS:.c=.o}
INNATIVE_ENV_OBJS := $(addprefix $(OBJDIR)/c/,$(INNATIVE_ENV_C_OBJS))
INNATIVE_ENV_OBJS_DEBUG := $(addprefix $(OBJDIR)/debug/c/,$(INNATIVE_ENV_C_OBJS))

.PHONY: all clean distclean

all: $(BUILDDIR)/$(INNATIVE_ENV) $(BUILDDIR)/$(INNATIVE_ENV_DEBUG)
	
$(OBJDIR)/c/%.o : %.c
	+@[ -d $(OBJDIR) ] || mkdir -p $(OBJDIR)
	+@[ -d $(OBJDIR)/c ] || mkdir -p $(OBJDIR)/c
	+@[ -d $(OBJDIR)/c/$(INNATIVE_ENV_DIR) ] || mkdir -p $(OBJDIR)/c/$(INNATIVE_ENV_DIR)
	$(COMPILE.c) $< -o $@
	
$(OBJDIR)/debug/c/%.o : %.c
	+@[ -d $(OBJDIR) ] || mkdir -p $(OBJDIR)
	+@[ -d $(OBJDIR)/debug ] || mkdir -p $(OBJDIR)/debug
	+@[ -d $(OBJDIR)/debug/c ] || mkdir -p $(OBJDIR)/debug/c
	+@[ -d $(OBJDIR)/debug/c/$(INNATIVE_ENV_DIR) ] || mkdir -p $(OBJDIR)/debug/c/$(INNATIVE_ENV_DIR)
	$(COMPILE.c) $< -o $@
  
$(BUILDDIR)/$(INNATIVE_ENV): CPPFLAGS += -fPIC -nostdlib $(foreach includedir,$(INNATIVE_ENV_INCLUDE),-I$(includedir)) -Wl,-rpath -Wl,. 
$(BUILDDIR)/$(INNATIVE_ENV): $(INNATIVE_ENV_OBJS)
	+@[ -d $(BUILDDIR) ] || mkdir -p $(BUILDDIR)
	$(AR) rcs $@ $(INNATIVE_ENV_OBJS)

$(BUILDDIR)/$(INNATIVE_ENV_DEBUG): CPPFLAGS += -fPIC -nostdlib -ggdb -fvar-tracking-assignments $(foreach includedir,$(INNATIVE_ENV_INCLUDE),-I$(includedir)) -Wl,-rpath -Wl,. 
$(BUILDDIR)/$(INNATIVE_ENV_DEBUG): $(INNATIVE_ENV_OBJS_DEBUG)
	+@[ -d $(BUILDDIR) ] || mkdir -p $(BUILDDIR)
	$(AR) rcs $@ $(INNATIVE_ENV_OBJS_DEBUG)

clean: distclean
	@- $(RM) $(BUILDDIR)/$(INNATIVE_ENV)
	@- $(RM) $(BUILDDIR)/$(INNATIVE_ENV_DEBUG)
  
distclean:
	@- $(RM) $(INNATIVE_ENV_OBJS)
	@- $(RM) $(INNATIVE_ENV_OBJS_DEBUG)
	@- $(RM) -r $(OBJDIR)

# innative-cmd
