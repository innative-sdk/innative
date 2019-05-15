INNATIVE_SRC      := innative
INNATIVE_FILES    := $(notdir $(wildcard $(INNATIVE_SRC)/*.cpp))

INNATIVE_OBJDIR   := $(OBJDIR)/innative
INNATIVE_OBJS     := $(foreach rule,$(INNATIVE_FILES:.cpp=.o),$(INNATIVE_OBJDIR)/$(rule))
INNATIVE_CPPFLAGS := $(CPPFLAGS) -fPIC -std=c++11 -msse2 -mcx16 -pthread -Wno-parentheses -Wno-reorder -Wno-shadow -Wno-class-memaccess
INNATIVE_LLVM_ARCHIVES := liblldWasm.a libLLVMLibDriver.a libLLVMPasses.a libLLVMExegesis.a liblldELF.a libLLVMFuzzMutate.a libLLVMCore.a libLLVMXRay.a libLLVMSymbolize.a libLLVMX86AsmParser.a libLLVMX86Disassembler.a libLLVMVectorize.a libLLVMMCParser.a libLLVMOption.a libLLVMCFIVerify.a libLLVMDebugInfoMSF.a libLLVMDebugInfoDWARF.a libLLVMAsmPrinter.a liblldMinGW.a libLLVMCoroutines.a libLLVMCodeGen.a libLLVMTarget.a libLLVMOrcJIT.a libLLVMInstrumentation.a libLLVMLinker.a liblldCOFF.a libLLVMMIRParser.a libLLVMTransformUtils.a libLLVMMC.a libLLVMRuntimeDyld.a libLLVMLineEditor.a libLLVMMCJIT.a libLLVMDlltoolDriver.a libLLVMBinaryFormat.a libLLVMMCA.a libLLVMDebugInfoCodeView.a libLLVMX86Info.a libLLVMAggressiveInstCombine.a liblldCommon.a liblldCore.a libLLVMObject.a libLLVMIRReader.a libLLVMScalarOpts.a libLLVMCoverage.a libLLVMInterpreter.a liblldDriver.a libLLVMBitWriter.a libLLVMBitReader.a libLLVMLTO.a libLLVMAsmParser.a liblldYAML.a libLLVMInstCombine.a libLLVMAnalysis.a libLLVMExegesisX86.a libLLVMX86AsmPrinter.a libLLVMDemangle.a libLLVMGlobalISel.a libLLVMDebugInfoPDB.a libLLVMX86Desc.a libLLVMProfileData.a libLLVMTableGen.a libLLVMX86Utils.a libLLVMObjCARCOpts.a libLLVMExecutionEngine.a libLLVMMCDisassembler.a libLLVMWindowsManifest.a libLLVMSupport.a libLLVMSelectionDAG.a libLLVMObjectYAML.a liblldMachO.a libLLVMX86CodeGen.a liblldReaderWriter.a libLLVMipo.a libLLVMWebAssemblyAsmParser.a libLLVMWebAssemblyAsmPrinter.a libLLVMWebAssemblyCodeGen.a libLLVMWebAssemblyDesc.a libLLVMWebAssemblyDisassembler.a libLLVMWebAssemblyInfo.a
INNATIVE_ARCHIVES := $(foreach rule,$(INNATIVE_LLVM_ARCHIVES),bin/llvm/lib/$(rule))
INNATIVE_LIBS     := $(LIBS) -lrt -ldl -lpthread -lz

INNATIVE_LDFLAGS  := $(LDFLAGS) -Wl,-whole-archive $(INNATIVE_ARCHIVES) -Wl,-no-whole-archive $(INNATIVE_LIBS) -s

# Automatically declare dependencies
-include $(INNATIVE_OBJS:.o=.d)

.PHONY: innative innative-clean

innative: $(LIBDIR)/libinnative.so $(LIBDIR)/innative.a

innative-clean:
	$(RM) $(LIBDIR)/libinnative.so
	$(RM) $(LIBDIR)/innative.a
	$(RM) -r $(INNATIVE_OBJDIR)

$(LIBDIR)/libinnative.so: $(LIBDIR)/innative-env.a $(LIBDIR)/innative-env_d.a $(INNATIVE_OBJS)
	$(CXXLD) $(INNATIVE_CPPFLAGS) $(INNATIVE_OBJS) $(INNATIVE_LDFLAGS) -shared -o $@

$(LIBDIR)/innative.a: $(LIBDIR)/innative-env.a $(LIBDIR)/innative-env_d.a $(INNATIVE_OBJS)
	$(AR) rcs $@ $(INNATIVE_OBJS)

$(INNATIVE_OBJDIR)/%.o: innative/%.cpp
	@mkdir -p $(INNATIVE_OBJDIR)
	$(CXX) $(INNATIVE_CPPFLAGS) -MMD -c $< -o $@
