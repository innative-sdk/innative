INNATIVE_TEST_EMBEDDING_SRC   := .
INNATIVE_TEST_EMBEDDING_FILES := $(notdir $(wildcard $(INNATIVE_TEST_EMBEDDING_SRC)/*.c))

INNATIVE_TEST_EMBEDDING_OBJDIR         := $(OBJDIR)/innative-test-embedding
INNATIVE_TEST_EMBEDDING_OBJS           := $(foreach rule,$(INNATIVE_TEST_EMBEDDING_FILES:.c=.o),$(INNATIVE_TEST_EMBEDDING_OBJDIR)/$(rule))
INNATIVE_TEST_EMBEDDING_CPPFLAGS       := $(CPPFLAGS) -fPIC -fno-stack-protector
INNATIVE_TEST_EMBEDDING_DEBUG_CPPFLAGS := $(CPPFLAGS) -g3 -fPIC -fno-stack-protector

# Automatically declare dependencies
#-include $(INNATIVE_TEST_EMBEDDING_OBJS:.o=.d)

.PHONY: all clean

all: $(LIBDIR)/innative-test-embedding.a

clean:
	$(RM) $(LIBDIR)/innative-test-embedding.a
	$(RM) -r $(INNATIVE_TEST_EMBEDDING_OBJDIR)

$(LIBDIR)/innative-test-embedding.a: $(INNATIVE_TEST_EMBEDDING_OBJS)
	@mkdir -p $(LIBDIR)
	$(AR) rcs $@ $(INNATIVE_TEST_EMBEDDING_OBJS)

$(INNATIVE_TEST_EMBEDDING_OBJDIR)/%.o: $(INNATIVE_TEST_EMBEDDING_SRC)/%.c
	@mkdir -p $(INNATIVE_TEST_EMBEDDING_OBJDIR)
	$(CC) $(INNATIVE_TEST_EMBEDDING_CPPFLAGS) -MMD -c $< -o $@ -ldl
