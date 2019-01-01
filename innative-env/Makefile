INNATIVE_ENV_SRC            := innative-env
INNATIVE_ENV_FILES          := $(notdir $(wildcard $(INNATIVE_ENV_SRC)/*.c))

INNATIVE_ENV_OBJDIR         := $(OBJDIR)/innative-env
INNATIVE_ENV_OBJS           := $(foreach rule,$(INNATIVE_ENV_FILES:.c=.o),$(INNATIVE_ENV_OBJDIR)/$(rule))
INNATIVE_ENV_DEBUG_OBJDIR   := $(OBJDIR)/innative-env/debug
INNATIVE_ENV_DEBUG_OBJS     := $(foreach rule,$(INNATIVE_ENV_FILES:.c=.o),$(INNATIVE_ENV_DEBUG_OBJDIR)/$(rule))
INNATIVE_ENV_CPPFLAGS       := $(CPPFLAGS)
INNATIVE_ENV_DEBUG_CPPFLAGS := $(CPPFLAGS) -g3

# Automatically declare dependencies
-include $(INNATIVE_ENV_OBJS:.o=.d)
-include $(INNATIVE_ENV_DEBUG_OBJS:.o=.d)

.PHONY: innative-env innative-env-clean

innative-env: $(LIBDIR)/innative-env.a $(LIBDIR)/innative-env_d.a

innative-env-clean:
	$(RM) $(LIBDIR)/innative-env.a $(LIBDIR)/innative-env_d.a
	$(RM) -r $(INNATIVE_ENV_DEBUG_OBJDIR)
	$(RM) -r $(INNATIVE_ENV_OBJDIR)

$(LIBDIR)/innative-env.a: $(INNATIVE_ENV_OBJS)
	$(AR) rcs $@ $(INNATIVE_ENV_OBJS)

$(INNATIVE_ENV_OBJDIR)/%.o: innative-env/%.c
	@mkdir -p $(INNATIVE_ENV_OBJDIR)
	$(CC) $(INNATIVE_ENV_CPPFLAGS) -MMD -c $< -o $@

$(LIBDIR)/innative-env_d.a: $(INNATIVE_ENV_DEBUG_OBJS)
	$(AR) rcs $@ $(INNATIVE_ENV_DEBUG_OBJS)

$(INNATIVE_ENV_DEBUG_OBJDIR)/%.o: innative-env/%.c
	@mkdir -p $(INNATIVE_ENV_DEBUG_OBJDIR)
	$(CC) $(INNATIVE_ENV_DEBUG_CPPFLAGS) -MMD -c $< -o $@
