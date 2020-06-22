INNATIVE_TEST_SRC      := innative-test
INNATIVE_TEST_FILES    := $(notdir $(wildcard $(INNATIVE_TEST_SRC)/*.cpp))

INNATIVE_TEST_OBJDIR   := $(OBJDIR)/innative-test
INNATIVE_TEST_OBJS     := $(foreach rule,$(INNATIVE_TEST_FILES:.cpp=.o),$(INNATIVE_TEST_OBJDIR)/$(rule))
INNATIVE_TEST_CPPFLAGS := $(CPPFLAGS) -std=c++17 -pthread -msse -msse2 -msse3 -mmmx -m3dnow -mcx16 -DLIBICONV_PLUG -DTESTING_WASM -Wno-reorder -Wno-class-memaccess
INNATIVE_TEST_LIBS     := -lz -linnative -lrt -lpthread -lstdc++fs
INNATIVE_TEST_LDFLAGS  := $(LDFLAGS) $(INNATIVE_TEST_LIBS) -Wl,-rpath -Wl,. # try to use rpath to pick up library in binary directory

# Automatically declare dependencies
-include $(INNATIVE_TEST_OBJS:.o=.d)

.PHONY: innative-test innative-test-clean

innative-test: $(BINDIR)/innative-test

innative-test-clean:
	$(RM) $(BINDIR)/innative-test
	$(RM) -r $(INNATIVE_TEST_OBJDIR)

$(BINDIR)/innative-test: $(LIBDIR)/libinnative.so $(LIBDIR)/innative-env-d.a $(INNATIVE_TEST_OBJS) $(INNATIVE_TEST_OBJDIR)/wasm_malloc.o
	$(CXXLD) $(INNATIVE_TEST_CPPFLAGS) $(INNATIVE_TEST_OBJS) $(INNATIVE_TEST_OBJDIR)/wasm_malloc.o $(LIBDIR)/innative-env-d.a $(INNATIVE_TEST_LDFLAGS) -o $@

$(INNATIVE_TEST_OBJDIR)/%.o: innative-test/%.cpp
	@mkdir -p $(INNATIVE_TEST_OBJDIR)
	$(CXX) $(INNATIVE_TEST_CPPFLAGS) -MMD -c $< -o $@

$(INNATIVE_TEST_OBJDIR)/wasm_malloc.o: wasm_malloc.c
	@mkdir -p $(INNATIVE_TEST_OBJDIR)
	$(CC) -msse -msse2 -msse3 -mmmx -m3dnow -mcx16 -DTESTING_WASM -MMD -c $< -o $@
