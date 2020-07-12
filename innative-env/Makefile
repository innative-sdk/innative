INNATIVE_ENV_SRC            := .
INNATIVE_ENV_FILES          := $(notdir $(wildcard $(INNATIVE_ENV_SRC)/*.c))

INNATIVE_ENV_OBJDIR         := $(OBJDIR)/innative-env
INNATIVE_ENV_OBJS           := $(foreach rule,$(INNATIVE_ENV_FILES:.c=.o),$(INNATIVE_ENV_OBJDIR)/$(rule))
INNATIVE_ENV_DEBUG_OBJDIR   := $(OBJDIR)/innative-env/debug
INNATIVE_ENV_DEBUG_OBJS     := $(foreach rule,$(INNATIVE_ENV_FILES:.c=.o),$(INNATIVE_ENV_DEBUG_OBJDIR)/$(rule))
INNATIVE_ENV_CPPFLAGS       := $(CPPFLAGS) -fPIC -fno-stack-protector
INNATIVE_ENV_DEBUG_CPPFLAGS := $(CPPFLAGS) -g3 -fPIC -fno-stack-protector

all: $(LIBDIR)/innative-env.a $(LIBDIR)/innative-env-d.a
  
clean:
	$(RM) $(LIBDIR)/innative-env.a $(LIBDIR)/innative-env-d.a
	$(RM) -r $(INNATIVE_ENV_DEBUG_OBJDIR)
	$(RM) -r $(INNATIVE_ENV_OBJDIR)

$(LIBDIR)/innative-env.a: $(INNATIVE_ENV_OBJS)
	@mkdir -p $(LIBDIR)
	$(AR) rcs $@ $(INNATIVE_ENV_OBJS)

$(INNATIVE_ENV_OBJDIR)/%.o: $(INNATIVE_ENV_SRC)/%.c
	@mkdir -p $(INNATIVE_ENV_OBJDIR)
	$(CC) $(INNATIVE_ENV_CPPFLAGS) -MMD -c $< -o $@

$(LIBDIR)/innative-env-d.a: $(INNATIVE_ENV_DEBUG_OBJS)
	@mkdir -p $(LIBDIR)
	$(AR) rcs $@ $(INNATIVE_ENV_DEBUG_OBJS)

$(INNATIVE_ENV_DEBUG_OBJDIR)/%.o: $(INNATIVE_ENV_SRC)/%.c
	@mkdir -p $(INNATIVE_ENV_DEBUG_OBJDIR)
	$(CC) $(INNATIVE_ENV_DEBUG_CPPFLAGS) -MMD -c $< -o $@

.PHONY: all clean
