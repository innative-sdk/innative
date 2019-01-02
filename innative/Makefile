INNATIVE_SRC      := innative
INNATIVE_FILES    := $(notdir $(wildcard $(INNATIVE_SRC)/*.cpp))

INNATIVE_OBJDIR   := $(OBJDIR)/innative
INNATIVE_OBJS     := $(foreach rule,$(INNATIVE_FILES:.cpp=.o),$(INNATIVE_OBJDIR)/$(rule))
INNATIVE_CPPFLAGS := $(CPPFLAGS) -fPIC -std=c++11 -msse2 -mcx16 -pthread -Wno-parentheses -Wno-reorder -Wno-shadow -Wno-class-memaccess
#INNATIVE_LIBS     := $(LIBS) -lrt -ldl -lpthread bin/llvm/lib/libLLVMIRReader.a bin/llvm/lib/libLLVMX86AsmParser.a bin/llvm/lib/libLLVMX86CodeGen.a bin/llvm/lib/libLLVMAsmParser.a bin/llvm/lib/libLLVMX86Desc.a bin/llvm/lib/libLLVMX86AsmPrinter.a bin/llvm/lib/libLLVMMCDisassembler.a bin/llvm/lib/libLLVMX86Info.a bin/llvm/lib/libLLVMX86Utils.a bin/llvm/lib/libLLVMAsmPrinter.a bin/llvm/lib/libLLVMGlobalISel.a bin/llvm/lib/libLLVMSelectionDAG.a bin/llvm/lib/libLLVMCodeGen.a bin/llvm/lib/libLLVMBitWriter.a bin/llvm/lib/libLLVMScalarOpts.a bin/llvm/lib/libLLVMAggressiveInstCombine.a bin/llvm/lib/libLLVMInstCombine.a bin/llvm/lib/libLLVMTransformUtils.a bin/llvm/lib/libLLVMTarget.a bin/llvm/lib/libLLVMAnalysis.a bin/llvm/lib/libLLVMObject.a bin/llvm/lib/libLLVMBitReader.a bin/llvm/lib/libLLVMMCParser.a bin/llvm/lib/libLLVMProfileData.a bin/llvm/lib/libLLVMCore.a bin/llvm/lib/libLLVMMC.a bin/llvm/lib/libLLVMBinaryFormat.a bin/llvm/lib/libLLVMDebugInfoCodeView.a bin/llvm/lib/libLLVMDebugInfoMSF.a bin/llvm/lib/libLLVMSupport.a bin/llvm/lib/libLLVMPasses.a bin/llvm/lib/libLLVMipo.a bin/llvm/lib/libLLVMInstrumentation.a bin/llvm/lib/libLLVMDemangle.a bin/llvm/lib/libLLVMDebugInfoDWARF.a bin/llvm/lib/liblldCOFF.a bin/llvm/lib/liblldCommon.a bin/llvm/lib/liblldCore.a bin/llvm/lib/liblldDriver.a bin/llvm/lib/liblldELF.a bin/llvm/lib/liblldMachO.a bin/llvm/lib/liblldMinGW.a bin/llvm/lib/liblldReaderWriter.a bin/llvm/lib/liblldWasm.a bin/llvm/lib/liblldYAML.a
INNATIVE_LIBS     := $(LIBS)  -lrt -ldl -lpthread -lLLVM-8svn -llld-8svn

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
