TARGET := libinnative.so
SRCDIR := innative
BUILDDIR := bin
OBJDIR := bin/obj
C_SRCS := $(wildcard $(SRCDIR)/*.c)
CXX_SRCS := $(wildcard $(SRCDIR)/*.cpp)
INCLUDE_DIRS := include llvm/include llvm/tools/lld/include bin/llvm/include bin/llvm/tools/lld/include
LIBRARY_DIRS := 
LIBRARIES := rt dl pthread
ARCHIVES := bin/llvm/lib/libLLVMSupport.a bin/llvm/lib/libLLVMCore.a bin/llvm/lib/libLLVMIRReader.a bin/llvm/lib/libLLVMAnalysis.a bin/llvm/lib/libLLVMX86AsmParser.a bin/llvm/lib/libLLVMX86AsmPrinter.a bin/llvm/lib/libLLVMX86CodeGen.a bin/llvm/lib/libLLVMAsmParser.a bin/llvm/lib/libLLVMX86Desc.a bin/llvm/lib/libLLVMX86AsmPrinter.a bin/llvm/lib/libLLVMMCDisassembler.a bin/llvm/lib/libLLVMX86Info.a bin/llvm/lib/libLLVMX86Utils.a bin/llvm/lib/libLLVMAsmPrinter.a bin/llvm/lib/libLLVMGlobalISel.a bin/llvm/lib/libLLVMSelectionDAG.a bin/llvm/lib/libLLVMCodeGen.a bin/llvm/lib/libLLVMBitWriter.a bin/llvm/lib/libLLVMScalarOpts.a bin/llvm/lib/libLLVMAggressiveInstCombine.a bin/llvm/lib/libLLVMInstCombine.a bin/llvm/lib/libLLVMTransformUtils.a bin/llvm/lib/libLLVMTarget.a bin/llvm/lib/libLLVMAnalysis.a bin/llvm/lib/libLLVMObject.a bin/llvm/lib/libLLVMBitReader.a bin/llvm/lib/libLLVMMCParser.a bin/llvm/lib/libLLVMProfileData.a bin/llvm/lib/libLLVMCore.a bin/llvm/lib/libLLVMMC.a bin/llvm/lib/libLLVMBinaryFormat.a bin/llvm/lib/libLLVMDebugInfoCodeView.a bin/llvm/lib/libLLVMDebugInfoMSF.a bin/llvm/lib/libLLVMSupport.a

CPPFLAGS += -fPIC -std=c++11 -msse2 -mcx16 -Wall -Wno-attributes -Wno-unknown-pragmas -Wno-reorder -Wno-missing-braces -Wno-unused-function -Wno-char-subscripts -fsanitize=signed-integer-overflow -fuse-ld=gold -Wno-class-memaccess -Wno-parentheses
LDFLAGS += -msse2 -mcx16 -shared

include base.mk

distclean:
	@- $(RM) $(OBJS)
	@- $(RM) -r $(OBJDIR)

PREFIX = /usr

.PHONY: install
install:
	test -d $(PREFIX) || mkdir $(PREFIX)
	test -d $(PREFIX)/lib || mkdir $(PREFIX)/lib
	test -d $(PREFIX)/include || mkdir $(PREFIX)/include
	test -d $(PREFIX)/include/$(SRCDIR) || mkdir $(PREFIX)/include/$(SRCDIR)
	cp $(BUILDDIR)/$(TARGET) $(PREFIX)/lib/$(TARGET)
	cp include/$(SRCDIR)/*.h $(PREFIX)/include/$(SRCDIR)/

.PHONY: uninstall
uninstall:
	rm -f $(PREFIX)/lib/$(TARGET)
	rm -f -r $(PREFIX)/include/$(SRCDIR)