INNATIVE_CMD_SRC      := innative-cmd
INNATIVE_CMD_FILES    := $(notdir $(wildcard $(INNATIVE_CMD_SRC)/*.cpp))

INNATIVE_CMD_OBJDIR   := $(OBJDIR)/innative-cmd
INNATIVE_CMD_OBJS     := $(foreach rule,$(INNATIVE_CMD_FILES:.cpp=.o),$(INNATIVE_CMD_OBJDIR)/$(rule))
INNATIVE_CMD_CPPFLAGS := $(CPPFLAGS) -std=c++17 -pthread -msse -msse2 -msse3 -mmmx -m3dnow -mcx16 -DLIBICONV_PLUG -Wno-reorder -Wno-class-memaccess
INNATIVE_CMD_LIBS     := -lz -linnative -lrt -lpthread -lstdc++fs
INNATIVE_CMD_LDFLAGS  := $(LDFLAGS) $(INNATIVE_CMD_LIBS) -Wl,-rpath -Wl,. # try to use rpath to pick up library in binary directory

# Automatically declare dependencies
-include $(INNATIVE_CMD_OBJS:.o=.d)

.PHONY: innative-cmd innative-cmd-clean

innative-cmd: $(BINDIR)/innative-cmd

innative-cmd-clean:
	$(RM) $(BINDIR)/innative-cmd
	$(RM) -r $(INNATIVE_CMD_OBJDIR)

$(BINDIR)/innative-cmd: $(LIBDIR)/libinnative.so $(INNATIVE_CMD_OBJS)
	$(CXXLD) $(INNATIVE_CMD_CPPFLAGS) $(INNATIVE_CMD_OBJS) $(INNATIVE_CMD_LDFLAGS) -o $@

$(INNATIVE_CMD_OBJDIR)/%.o: innative-cmd/%.cpp
	@mkdir -p $(INNATIVE_CMD_OBJDIR)
	$(CXX) $(INNATIVE_CMD_CPPFLAGS) -MMD -c $< -o $@
