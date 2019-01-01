INNATIVE_TEST_SRC      := innative-test
INNATIVE_TEST_FILES    := $(notdir $(wildcard $(INNATIVE_TEST_SRC)/*.cpp))

INNATIVE_TEST_OBJDIR   := $(OBJDIR)/innative-test
INNATIVE_TEST_OBJS     := $(foreach rule,$(INNATIVE_TEST_FILES:.cpp=.o),$(INNATIVE_TEST_OBJDIR)/$(rule))
INNATIVE_TEST_CPPFLAGS := $(CPPFLAGS) -std=c++17 -pthread -msse2 -mcx16 -DLIBICONV_PLUG -Wno-reorder -Wno-class-memaccess
INNATIVE_TEST_LIBS     := -linnative -lrt -lpthread -lstdc++fs
INNATIVE_TEST_LDFLAGS  := $(LDFLAGS) $(INNATIVE_TEST_LIBS) -Wl,-rpath -Wl,. # try to use rpath to pick up library in binary directory

# Automatically declare dependencies
-include $(INNATIVE_TEST_OBJS:.o=.d)

.PHONY: innative-test innative-test-clean

innative-test: $(BINDIR)/innative-test

innative-test-clean:
	$(RM) $(BINDIR)/innative-test
	$(RM) -r $(INNATIVE_TEST_OBJDIR)

$(BINDIR)/innative-test: $(LIBDIR)/libinnative.so $(LIBDIR)/innative-env_d.a $(INNATIVE_TEST_OBJS)
	$(CXXLD) $(INNATIVE_TEST_CPPFLAGS) $(INNATIVE_TEST_OBJS) $(LIBDIR)/innative-env_d.a $(INNATIVE_TEST_LDFLAGS) -o $@

$(INNATIVE_TEST_OBJDIR)/%.o: innative-test/%.cpp
	@mkdir -p $(INNATIVE_TEST_OBJDIR)
	$(CXX) $(INNATIVE_TEST_CPPFLAGS) -MMD -c $< -o $@
