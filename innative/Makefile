INNATIVE_SRC      := innative
INNATIVE_FILES    := $(notdir $(wildcard $(INNATIVE_SRC)/*.cpp))

INNATIVE_OBJDIR   := $(OBJDIR)/innative
INNATIVE_OBJS     := $(foreach rule,$(INNATIVE_FILES:.cpp=.o),$(INNATIVE_OBJDIR)/$(rule))
INNATIVE_CPPFLAGS := $(CPPFLAGS) -fPIC -std=c++17 -msse -msse2 -msse3 -mmmx -m3dnow -mcx16 -pthread -Wno-parentheses -Wno-reorder -Wno-shadow -Wno-class-memaccess
INNATIVE_LLVM_ARCHIVES := libLLVMAggressiveInstCombine.a libLLVMAnalysis.a libLLVMAsmParser.a libLLVMAsmPrinter.a libLLVMBinaryFormat.a libLLVMBitReader.a libLLVMBitWriter.a libLLVMBitstreamReader.a libLLVMCFIVerify.a libLLVMCodeGen.a libLLVMCore.a libLLVMCoroutines.a libLLVMCoverage.a libLLVMDebugInfoCodeView.a libLLVMDebugInfoDWARF.a libLLVMDebugInfoGSYM.a libLLVMDebugInfoMSF.a libLLVMDebugInfoPDB.a libLLVMDemangle.a libLLVMDlltoolDriver.a libLLVMExecutionEngine.a libLLVMExegesis.a libLLVMExegesisX86.a libLLVMFuzzMutate.a libLLVMGlobalISel.a libLLVMIRReader.a libLLVMInstCombine.a libLLVMInstrumentation.a libLLVMInterpreter.a libLLVMJITLink.a libLLVMLTO.a libLLVMLibDriver.a libLLVMLineEditor.a libLLVMLinker.a libLLVMMC.a libLLVMMCA.a libLLVMMCDisassembler.a libLLVMMCJIT.a libLLVMMCParser.a libLLVMMIRParser.a libLLVMObjCARCOpts.a libLLVMObject.a libLLVMObjectYAML.a libLLVMOption.a libLLVMOrcJIT.a libLLVMPasses.a libLLVMProfileData.a libLLVMRemarks.a libLLVMRuntimeDyld.a libLLVMScalarOpts.a libLLVMSelectionDAG.a libLLVMSupport.a libLLVMSymbolize.a libLLVMTableGen.a libLLVMTarget.a libLLVMTextAPI.a libLLVMTransformUtils.a libLLVMVectorize.a libLLVMWebAssemblyAsmParser.a libLLVMWebAssemblyCodeGen.a libLLVMWebAssemblyDesc.a libLLVMWebAssemblyDisassembler.a libLLVMWebAssemblyInfo.a libLLVMWindowsManifest.a libLLVMX86AsmParser.a libLLVMX86CodeGen.a libLLVMX86Desc.a libLLVMX86Disassembler.a libLLVMX86Info.a libLLVMX86Utils.a libLLVMXRay.a libLLVMipo.a liblldCOFF.a liblldCommon.a liblldCore.a liblldDriver.a liblldELF.a liblldMachO.a liblldMinGW.a liblldReaderWriter.a liblldWasm.a liblldYAML.a
INNATIVE_ARCHIVES := $(foreach rule,$(INNATIVE_LLVM_ARCHIVES),bin/llvm/lib/$(rule))
INNATIVE_LIBS     := $(LIBS) -lrt -ldl -lpthread

INNATIVE_LDFLAGS  := $(LDFLAGS) -Wl,-whole-archive $(INNATIVE_ARCHIVES) -Wl,-no-whole-archive $(INNATIVE_LIBS) -s

# Automatically declare dependencies
-include $(INNATIVE_OBJS:.o=.d)

.PHONY: innative innative-clean

innative: $(LIBDIR)/libinnative.so $(LIBDIR)/innative.a

innative-clean:
	$(RM) $(LIBDIR)/libinnative.so
	$(RM) $(LIBDIR)/innative.a
	$(RM) -r $(INNATIVE_OBJDIR)

$(LIBDIR)/libinnative.so: $(LIBDIR)/innative-env.a $(LIBDIR)/innative-env-d.a $(INNATIVE_OBJS)
	$(CXXLD) $(INNATIVE_CPPFLAGS) $(INNATIVE_OBJS) $(INNATIVE_LDFLAGS) -shared -o $@

$(LIBDIR)/innative.a: $(LIBDIR)/innative-env.a $(LIBDIR)/innative-env-d.a $(INNATIVE_OBJS)
	$(AR) rcs $@ $(INNATIVE_OBJS)

$(INNATIVE_OBJDIR)/%.o: innative/%.cpp
	@mkdir -p $(INNATIVE_OBJDIR)
	$(CXX) $(INNATIVE_CPPFLAGS) -MMD -c $< -o $@
