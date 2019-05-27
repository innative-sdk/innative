INNATIVE_STUB_SRC            := innative-stub
INNATIVE_STUB_FILES          := $(notdir $(wildcard $(INNATIVE_STUB_SRC)/*.c))

INNATIVE_STUB_OBJDIR         := $(OBJDIR)/innative-stub
INNATIVE_STUB_OBJS           := $(foreach rule,$(INNATIVE_STUB_FILES:.c=.o),$(INNATIVE_STUB_OBJDIR)/$(rule))
INNATIVE_STUB_CPPFLAGS       := $(CPPFLAGS) -fPIC -fno-stack-protector
INNATIVE_STUB_DEBUG_CPPFLAGS := $(CPPFLAGS) -g3 -fPIC -fno-stack-protector

# Automatically declare dependencies
-include $(INNATIVE_STUB_OBJS:.o=.d)

.PHONY: innative-stub innative-stub-clean

innative-stub: $(LIBDIR)/innative-stub.a

innative-stub-clean:
	$(RM) $(LIBDIR)/innative-stub.a
	$(RM) -r $(INNATIVE_STUB_OBJDIR)

$(LIBDIR)/innative-stub.a: $(INNATIVE_STUB_OBJS)
	$(AR) rcs $@ $(INNATIVE_STUB_OBJS)

$(INNATIVE_STUB_OBJDIR)/%.o: innative-stub/%.c
	@mkdir -p $(INNATIVE_STUB_OBJDIR)
	$(CXX) $(INNATIVE_STUB_CPPFLAGS) -MMD -c $< -o $@ -ldl
