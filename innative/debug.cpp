// Copyright (c)2020 Black Sphere Studios
// For conditions of distribution and use, see copyright notice in innative.h

#include "llvm.h"
#include "debug_pdb.h"
#include "debug_dwarf.h"
#include "debug_wat.h"
#include "constants.h"
#include "utility.h"
#include "compile.h"

using namespace innative;
using namespace utility;

Debugger::~Debugger() {}
Debugger::Debugger() : _dbuilder(0), _compiler(0) {}

Debugger::Debugger(Compiler* compiler, llvm::Module& m, const char* name, const char* filepath, char target) :
  _compiler(compiler), _dbuilder(new llvm::DIBuilder(m))
{
  if(target == ENV_DEBUG)
    target = llvm::Triple(m.getTargetTriple()).isOSWindows() ? ENV_DEBUG_PDB : ENV_DEBUG_DWARF;

  m.addModuleFlag(llvm::Module::Warning, "Debug Info Version", llvm::DEBUG_METADATA_VERSION);
  if(target == ENV_DEBUG_PDB)
  {
    m.addModuleFlag(llvm::Module::Warning, "CodeView", 1);
    m.addModuleFlag(llvm::Module::Warning, "CodeViewGHash", 1);
  }
  else if(llvm::Triple(m.getTargetTriple()).isOSDarwin())
    m.addModuleFlag(llvm::Module::Warning, "Dwarf Version", 2);

  path abspath = GetAbsolutePath(GetPath(filepath));

  size_t sz;
  auto debugfile = LoadFile(abspath, sz);
  if(debugfile.get())
  {
    llvm::SmallString<32> checksum;
    llvm::DIFile::ChecksumKind CSKind =
      ComputeChecksum(llvm::StringRef(reinterpret_cast<char*>(debugfile.get()), sz), checksum);
    llvm::DIFile::ChecksumInfo<llvm::StringRef> CSInfo(CSKind, checksum);
    dunit = _dbuilder->createFile(abspath.filename().u8string(), abspath.parent_path().u8string(), CSInfo);
  }
  else
    dunit = _dbuilder->createFile(abspath.filename().u8string(), abspath.parent_path().u8string());

  dcu = _dbuilder->createCompileUnit(llvm::dwarf::DW_LANG_C89, dunit, "inNative Runtime v" IN_VERSION_STRING,
                                     _compiler->env.optimize != 0, GenFlagString(_compiler->env), WASM_MAGIC_VERSION, name);

  diF32  = _dbuilder->createBasicType("f32", 32, llvm::dwarf::DW_ATE_float);
  diF64  = _dbuilder->createBasicType("f64", 64, llvm::dwarf::DW_ATE_float);
  diI1   = _dbuilder->createBasicType("i1", 1, llvm::dwarf::DW_ATE_boolean);
  diI8   = _dbuilder->createBasicType("i8", 8, llvm::dwarf::DW_ATE_signed);
  diI32  = _dbuilder->createBasicType("i32", 32, llvm::dwarf::DW_ATE_signed);
  diI64  = _dbuilder->createBasicType("i64", 64, llvm::dwarf::DW_ATE_signed);
  diVoid = _dbuilder->createUnspecifiedType("void");
}
void Debugger::FuncDecl(llvm::Function* fn, unsigned int offset, unsigned int line, bool optimized) {}
void Debugger::FuncBody(llvm::Function* fn, size_t indice, FunctionDesc& desc, FunctionBody& body) {}
void Debugger::FuncParam(llvm::Function* fn, size_t indice, FunctionDesc& desc) {}
void Debugger::FuncLocal(llvm::Function* fn, size_t indice, FunctionDesc& desc) {}
void Debugger::DebugGlobal(llvm::GlobalVariable* v, llvm::StringRef name, size_t line) {}
void Debugger::PostFuncBody(llvm::Function* fn, FunctionBody& body) {}

void Debugger::DebugIns(llvm::Function* fn, Instruction& i) {}

llvm::DIType* Debugger::CreateDebugType(llvm::Type* t)
{
  if(t->isPointerTy())
  {
    auto base = CreateDebugType(t->getPointerElementType());
    return _dbuilder->createPointerType(base, _compiler->intptrty->getBitWidth(), 0U, llvm::None,
                                        std::string(base->getName()) + "*");
  }

  if(t->isFloatTy())
    return diF32;
  if(t->isDoubleTy())
    return diF64;
  if(t->isVoidTy())
    return diVoid;
  if(t->isIntegerTy() && static_cast<llvm::IntegerType*>(t)->getBitWidth() == 1)
    return diI1;
  if(t->isIntegerTy() && static_cast<llvm::IntegerType*>(t)->getBitWidth() == 8)
    return diI8;
  if(t->isIntegerTy() && static_cast<llvm::IntegerType*>(t)->getBitWidth() == 32)
    return diI32;
  if(t->isIntegerTy() && static_cast<llvm::IntegerType*>(t)->getBitWidth() == 64)
    return diVoid;
  if(t->isFunctionTy())
    return CreateFunctionDebugType(llvm::cast<llvm::FunctionType>(t), llvm::CallingConv::Fast);
  if(t->isStructTy())
  {
    uint64_t offset    = 0;
    unsigned int align = 0;
    llvm::SmallVector<llvm::Metadata*, 16> members;
    for(unsigned int i = 0; i < t->getStructNumElements(); ++i)
    {
      auto elem = CreateDebugType(t->getStructElementType(i));
      offset    = Align<uint64_t>(offset, elem->getAlignInBits());
      members.push_back(_dbuilder->createMemberType(dcu, "m" + std::to_string(i), dunit, 0, elem->getSizeInBits(),
                                                    elem->getAlignInBits(), offset, llvm::DINode::FlagZero, elem));
      offset += elem->getSizeInBits();
      align = std::max(align, elem->getAlignInBits());
    }

    return _dbuilder->createStructType(dcu, t->getStructName(), dunit, 0, offset, align, llvm::DINode::FlagZero, nullptr,
                                       _dbuilder->getOrCreateArray(members));
  }
  if(t->isArrayTy())
  {
    auto elem               = CreateDebugType(t->getArrayElementType());
    llvm::Metadata* range[] = { _dbuilder->getOrCreateSubrange(0, t->getArrayNumElements()) };
    return _dbuilder->createArrayType(elem->getSizeInBits() * t->getArrayNumElements(), elem->getAlignInBits(), elem,
                                      _dbuilder->getOrCreateArray(range));
  }

  assert(false);
  return nullptr;
}

llvm::DISubroutineType* Debugger::CreateFunctionDebugType(llvm::FunctionType* fn, llvm::CallingConv::ID callconv)
{
  llvm::SmallVector<llvm::Metadata*, 8> dwarfTys = { CreateDebugType(fn->getReturnType()) };
  for(unsigned int i = 0; i < fn->getNumParams(); ++i)
    dwarfTys.push_back(CreateDebugType(fn->getParamType(i)));

  return _dbuilder->createSubroutineType(_dbuilder->getOrCreateTypeArray(dwarfTys), llvm::DINode::FlagZero,
                                         (callconv == llvm::CallingConv::C) ? llvm::dwarf::DW_CC_normal :
                                                                              llvm::dwarf::DW_CC_nocall);
}

void Debugger::FunctionDebugInfo(llvm::Function* fn, llvm::StringRef name, bool optimized, bool definition, bool artificial,
                                 llvm::DIFile* file, unsigned int line, unsigned int col, llvm::DISubroutineType* subtype)
{
  if(!_dbuilder)
    return;
  llvm::DISubprogram::DISPFlags spflags = llvm::DISubprogram::DISPFlags::SPFlagZero;
  llvm::DINode::DIFlags diflags         = llvm::DINode::FlagZero;

  if(!fn->hasValidDeclarationLinkage())
    spflags |= llvm::DISubprogram::DISPFlags::SPFlagLocalToUnit;

  if(definition)
    spflags |= llvm::DISubprogram::DISPFlags::SPFlagDefinition;

  if(optimized)
    spflags |= llvm::DISubprogram::DISPFlags::SPFlagOptimized;

  if(artificial)
    diflags |= llvm::DINode::FlagArtificial;

  if(!definition)
    diflags |= llvm::DINode::FlagFwdDecl;

  if(fn->doesNotReturn())
    diflags |= llvm::DINode::FlagNoReturn;

  if(name.empty())
    name = fn->getName();

  if(!file)
    file = dunit;
  if(!subtype)
    subtype = CreateFunctionDebugType(fn->getFunctionType(), fn->getCallingConv());
  fn->setSubprogram(_dbuilder->createFunction(file, name, fn->getName(), file, line, subtype, line, diflags, spflags));
}
void Debugger::PushBlock(llvm::DILocalScope* scope, const llvm::DebugLoc& loc) { _curscope = scope; }
void Debugger::PopBlock() {}
void Debugger::DebugSetGlobal(int index) {}
void Debugger::Finalize()
{
  if(_dbuilder)
    _dbuilder->finalize();
}

llvm::DIFile::ChecksumKind Debugger::ComputeChecksum(llvm::StringRef data, llvm::SmallString<32>& Checksum)
{
  Checksum.clear();

  llvm::MD5 Hash;
  llvm::MD5::MD5Result Result;

  Hash.update(data);
  Hash.final(Result);

  Hash.stringifyResult(Result, Checksum);
  return llvm::DIFile::CSK_MD5;
}

void Debugger::SetSPLocation(llvm::IRBuilder<>& builder, llvm::DISubprogram* sp)
{
  if(_dbuilder && sp)
    builder.SetCurrentDebugLocation(GetSPLocation(sp->getContext(), sp));
}

llvm::DILocation* Debugger::GetSPLocation(llvm::LLVMContext& context, llvm::DISubprogram* sp)
{
  return llvm::DILocation::get(context, !sp->getScopeLine() ? sp->getLine() : sp->getScopeLine(), 0, sp, nullptr,
                               sp->getFlags() & llvm::DINode::FlagArtificial);
}

std::string Debugger::GenFlagString(const Environment& env)
{
  std::string f = env.flags ? "-flag" : "";
  if(env.flags & ENV_DEBUG)
    f += " debug";
  if(env.flags & ENV_LIBRARY)
    f += " library";
  if(env.flags & ENV_WHITELIST)
    f += " whitelist";
  if(env.flags & ENV_ENABLE_WAT)
    f += " wat";
  if(env.flags & ENV_MULTITHREADED)
    f += " multithreaded";
  if(env.flags & ENV_EMIT_LLVM)
    f += " llvm";
  if(env.flags & ENV_HOMOGENIZE_FUNCTIONS)
    f += " homogenize";
  if(env.flags & ENV_NO_INIT)
    f += " noinit";
  if(env.flags & ENV_CHECK_STACK_OVERFLOW)
    f += " check_stack_overflow";
  if(env.flags & ENV_CHECK_FLOAT_TRUNC)
    f += " check_float_trunc";
  if(env.flags & ENV_CHECK_MEMORY_ACCESS)
    f += " check_memory_access";
  if(env.flags & ENV_CHECK_INDIRECT_CALL)
    f += " check_indirect_call";
  if(env.flags & ENV_CHECK_INT_DIVISION)
    f += " check_int_division";
  if(env.flags & ENV_DISABLE_TAIL_CALL)
    f += " disable_tail_call";

  if(env.optimize & ENV_OPTIMIZE_FAST_MATH_REASSOCIATE)
    f += " fast_math_reassociate";
  if(env.optimize & ENV_OPTIMIZE_FAST_MATH_NO_NAN)
    f += " fast_math_no_nan";
  if(env.optimize & ENV_OPTIMIZE_FAST_MATH_NO_INF)
    f += " fast_math_no_inf";
  if(env.optimize & ENV_OPTIMIZE_FAST_MATH_NO_SIGNED_ZERO)
    f += " fast_math_no_signed_zero";
  if(env.optimize & ENV_OPTIMIZE_FAST_MATH_ALLOW_RECIPROCAL)
    f += " fast_math_allow_reciprocal";
  if(env.optimize & ENV_OPTIMIZE_FAST_MATH_CONTRACT)
    f += " fast_math_contract";
  if(env.optimize & ENV_OPTIMIZE_FAST_MATH_ALLOW_APPROXIMATE_FUNCTIONS)
    f += " fast_math_approximate_functions";

  switch(env.optimize & ENV_OPTIMIZE_OMASK)
  {
  case ENV_OPTIMIZE_O0: f += " O0"; break;
  case ENV_OPTIMIZE_O1: f += " O1"; break;
  case ENV_OPTIMIZE_O2: f += " O2"; break;
  case ENV_OPTIMIZE_O3: f += " O3"; break;
  case ENV_OPTIMIZE_Os: f += " Os"; break;
  }

  if(env.features)
    f += " -feature";

  if(env.features == (uint64_t)~0)
    f += " all";
  else
  {
    if(env.features & ENV_FEATURE_MUTABLE_GLOBALS)
      f += " mutable_globals";
  }

  return f;
}

Debugger* Debugger::Create(Compiler& compiler)
{
  if(auto target = compiler.env.flags & ENV_DEBUG)
  {
    if(!compiler.m.filepath)
      return 0;
    if(compiler.m.sourcemap)
    {
      if(target == ENV_DEBUG)
        target = llvm::Triple(compiler.mod->getTargetTriple()).isOSWindows() ? ENV_DEBUG_PDB : ENV_DEBUG_DWARF;
      if(target == ENV_DEBUG_PDB)
        return new DebugPDB(compiler.m.sourcemap, &compiler, *compiler.mod, compiler.m.name.str(),
                                  compiler.m.filepath);
      return new DebugDWARF(compiler.m.sourcemap, &compiler, *compiler.mod, compiler.m.name.str(),
                                  compiler.m.filepath);
    }
    return new DebugWat(&compiler, *compiler.mod, compiler.m.name.str(), compiler.m.filepath);
  }
  return new Debugger();
}
