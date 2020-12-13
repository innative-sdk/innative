INNATIVE_AS_SRC            := .
INNATIVE_AS_FILES          := $(notdir $(wildcard $(INNATIVE_AS_SRC)/*.c))

INNATIVE_AS_OBJDIR         := $(OBJDIR)/innative-assemblyscript
INNATIVE_AS_OBJS           := $(foreach rule,$(INNATIVE_AS_FILES:.c=.o),$(INNATIVE_AS_OBJDIR)/$(rule))
INNATIVE_AS_DEBUG_OBJDIR   := $(OBJDIR)/innative-assemblyscript/debug
INNATIVE_AS_DEBUG_OBJS     := $(foreach rule,$(INNATIVE_AS_FILES:.c=.o),$(INNATIVE_AS_DEBUG_OBJDIR)/$(rule))
INNATIVE_AS_CPPFLAGS       := $(CPPFLAGS) -fPIC -fno-stack-protector
INNATIVE_AS_DEBUG_CPPFLAGS := $(CPPFLAGS) -g3 -fPIC -fno-stack-protector

all: $(LIBDIR)/innative-assemblyscript.a $(LIBDIR)/innative-assemblyscript-d.a
  
clean:
	$(RM) $(LIBDIR)/innative-assemblyscript.a $(LIBDIR)/innative-assemblyscript-d.a
	$(RM) -r $(INNATIVE_AS_DEBUG_OBJDIR)
	$(RM) -r $(INNATIVE_AS_OBJDIR)

$(LIBDIR)/innative-assemblyscript.a: $(INNATIVE_AS_OBJS)
	@mkdir -p $(LIBDIR)
	$(AR) rcs $@ $(INNATIVE_AS_OBJS)

$(INNATIVE_AS_OBJDIR)/%.o: $(INNATIVE_AS_SRC)/%.c
	@mkdir -p $(INNATIVE_AS_OBJDIR)
	$(CC) $(INNATIVE_AS_CPPFLAGS) -MMD -c $< -o $@

$(LIBDIR)/innative-assemblyscript-d.a: $(INNATIVE_AS_DEBUG_OBJS)
	@mkdir -p $(LIBDIR)
	$(AR) rcs $@ $(INNATIVE_AS_DEBUG_OBJS)

$(INNATIVE_AS_DEBUG_OBJDIR)/%.o: $(INNATIVE_AS_SRC)/%.c
	@mkdir -p $(INNATIVE_AS_DEBUG_OBJDIR)
	$(CC) $(INNATIVE_AS_DEBUG_CPPFLAGS) -MMD -c $< -o $@

.PHONY: all clean
