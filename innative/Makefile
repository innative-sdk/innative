INNATIVE_SRC      := innative
INNATIVE_FILES    := $(notdir $(wildcard $(INNATIVE_SRC)/*.cpp))

INNATIVE_OBJDIR   := $(OBJDIR)/innative
INNATIVE_OBJS     := $(foreach rule,$(INNATIVE_FILES:.cpp=.o),$(INNATIVE_OBJDIR)/$(rule))
INNATIVE_CPPFLAGS := $(CPPFLAGS) -fPIC -std=c++11 -msse2 -mcx16 -pthread -Wno-parentheses -Wno-reorder -Wno-shadow -Wno-class-memaccess
INNATIVE_LIBS     := $(LIBS) -lrt -ldl -lpthread -lLLVM-7 -llldCOFF -llldCommon -llldCore -llldDriver -llldELF -llldMachO -llldMinGW -llldReaderWriter -llldWasm -llldYAML
INNATIVE_LDFLAGS  := $(LDFLAGS) -Wl,-whole-archive $(INNATIVE_LIBS) -Wl,-no-whole-archive

# Automatically declare dependencies
-include $(INNATIVE_OBJS:.o=.d)

.PHONY: innative innative-clean

innative: $(LIBDIR)/libinnative.so

innative-clean:
	$(RM) $(LIBDIR)/libinnative.so
	$(RM) -r $(INNATIVE_OBJDIR)

$(LIBDIR)/libinnative.so: $(LIBDIR)/innative-env.a $(LIBDIR)/innative-env_d.a $(INNATIVE_OBJS)
	$(CXXLD) $(INNATIVE_CPPFLAGS) $(INNATIVE_OBJS) $(INNATIVE_LDFLAGS) -shared -o $@

$(INNATIVE_OBJDIR)/%.o: innative/%.cpp
	@mkdir -p $(INNATIVE_OBJDIR)
	$(CXX) $(INNATIVE_CPPFLAGS) -MMD -c $< -o $@
