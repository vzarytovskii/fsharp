[IL]: Error [StackUnexpected]: : Internal.Utilities.Collections.HashMultiMap`2::.ctor(int32, [S.P.CoreLib]System.Collections.Generic.IEqualityComparer`1<!0>, [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<bool>)][offset 0x00000060][found ref 'object'][expected ref '[S.P.CoreLib]System.Collections.Generic.IDictionary`2<T0,Microsoft.FSharp.Collections.FSharpList`1<T1>>'] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Internal.Utilities.Collections.HashMultiMap`2::.ctor(int32, [S.P.CoreLib]System.Collections.Generic.IEqualityComparer`1<!0>, [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<bool>)][offset 0x00000035][found ref 'object'][expected ref '[S.P.CoreLib]System.Collections.Generic.IDictionary`2<T0,T1>'] Unexpected type on the stack.
[IL]: Error [UnmanagedPointer]: : FSharp.Compiler.IO.SafeUnmanagedMemoryStream::.ctor(uint8*, int64, object)][offset 0x00000001] Unmanaged pointers are not a verifiable type.
[IL]: Error [UnmanagedPointer]: : FSharp.Compiler.IO.SafeUnmanagedMemoryStream::.ctor(uint8*, int64, int64, [S.P.CoreLib]System.IO.FileAccess, object)][offset 0x00000001] Unmanaged pointers are not a verifiable type.
[IL]: Error [UnmanagedPointer]: : FSharp.Compiler.IO.RawByteMemory::.ctor(uint8*, int32, object)][offset 0x00000009] Unmanaged pointers are not a verifiable type.
[IL]: Error [StackByRef]: : FSharp.Compiler.IO.RawByteMemory::get_Item(int32)][offset 0x0000001E][found Native Int] Expected ByRef on the stack.
[IL]: Error [StackByRef]: : FSharp.Compiler.IO.RawByteMemory::set_Item(int32, uint8)][offset 0x00000025][found Native Int] Expected ByRef on the stack.
[IL]: Error [ReturnPtrToStack]: : Internal.Utilities.Text.Lexing.LexBuffer`1::get_LexemeView()][offset 0x00000019] Return type is ByRef, TypedReference, ArgHandle, or ArgIterator.
[IL]: Error [StackUnexpected]: : Internal.Utilities.Text.Lexing.UnicodeTables::scanUntilSentinel([FSharp.Compiler.Service]Internal.Utilities.Text.Lexing.LexBuffer`1<char>, int32)][offset 0x00000079][found Short] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.Xml.XmlDoc::processLines([FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<string>)][offset 0x00000031][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.FxResolver::getTfmNumber(string)][offset 0x0000000B][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.FxResolver::tryGetRunningTfm()][offset 0x00000011][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.DependencyManager.AssemblyResolveHandler::.ctor([FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<FSharp.Compiler.DependencyManager.AssemblyResolutionProbe>)][offset 0x0000003E][found ref 'object'][expected ref '[S.P.CoreLib]System.IDisposable'] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.DependencyManager.DependencyProvider::TryFindDependencyManagerInPath([S.P.CoreLib]System.Collections.Generic.IEnumerable`1<string>, string, [FSharp.Compiler.Service]FSharp.Compiler.DependencyManager.ResolvingErrorReport, string)][offset 0x00000061][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.Symbols.FSharpEntity::TryGetFullDisplayName()][offset 0x00000035][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.Symbols.FSharpEntity::TryGetFullCompiledName()][offset 0x00000035][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.Symbols.FSharpMemberOrFunctionOrValue::TryGetFullCompiledOperatorNameIdents()][offset 0x0000008A][found Char] Unexpected type on the stack.
[IL]: Error [ReturnPtrToStack]: : FSharp.Compiler.CodeAnalysis.ItemKeyStore::ReadKeyString([System.Reflection.Metadata]System.Reflection.Metadata.BlobReader&)][offset 0x00000026] Return type is ByRef, TypedReference, ArgHandle, or ArgIterator.
[IL]: Error [ReturnPtrToStack]: : FSharp.Compiler.CodeAnalysis.ItemKeyStore::ReadFirstKeyString()][offset 0x00000070] Return type is ByRef, TypedReference, ArgHandle, or ArgIterator.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CodeAnalysis.ItemKeyStoreBuilder::writeRange([FSharp.Compiler.Service]FSharp.Compiler.Text.Range)][offset 0x00000011][found address of '[FSharp.Compiler.Service]FSharp.Compiler.Text.Range'][expected Native Int] Unexpected type on the stack.
[IL]: Error [ExpectedNumericType]: : FSharp.Compiler.EditorServices.SemanticClassificationKeyStoreBuilder::WriteAll([FSharp.Compiler.Service]FSharp.Compiler.EditorServices.SemanticClassificationItem[])][offset 0x0000001D][found address of '[FSharp.Compiler.Service]FSharp.Compiler.EditorServices.SemanticClassificationItem'] Expected numeric type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.EditorServices.Parent::FormatEntityFullName([FSharp.Compiler.Service]FSharp.Compiler.Symbols.FSharpEntity)][offset 0x00000069][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CodeAnalysis.FSharpChecker::.ctor([FSharp.Compiler.Service]FSharp.Compiler.CodeAnalysis.LegacyReferenceResolver, int32, bool, bool, [FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`2<string,System.DateTime>,Microsoft.FSharp.Core.FSharpOption`1<System.Tuple`3<object,native int,int32>>>, bool, bool, bool, bool, [FSharp.Compiler.Service]FSharp.Compiler.CompilerConfig+ParallelReferenceResolution, bool, [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<Microsoft.FSharp.Core.FSharpFunc`2<string,Microsoft.FSharp.Control.FSharpAsync`1<Microsoft.FSharp.Core.FSharpOption`1<FSharp.Compiler.Text.ISourceText>>>>, bool, [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<bool>, [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<FSharp.Compiler.CodeAnalysis.TransparentCompiler.CacheSizes>)][offset 0x000000A4][found ref 'object'][expected ref '[FSharp.Compiler.Service]FSharp.Compiler.CodeAnalysis.IBackgroundCompiler'] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CodeAnalysis.FSharpChecker::TokenizeFile(string)][offset 0x0000000D][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CodeAnalysis.Hosted.CompilerHelpers::fscCompile([FSharp.Compiler.Service]FSharp.Compiler.CodeAnalysis.LegacyReferenceResolver, string, string[])][offset 0x0000005C][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CodeAnalysis.Hosted.CompilerHelpers::fscCompile([FSharp.Compiler.Service]FSharp.Compiler.CodeAnalysis.LegacyReferenceResolver, string, string[])][offset 0x00000065][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CodeAnalysis.Hosted.CompilerHelpers::fscCompile([FSharp.Compiler.Service]FSharp.Compiler.CodeAnalysis.LegacyReferenceResolver, string, string[])][offset 0x00000082][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CodeAnalysis.Hosted.CompilerHelpers::fscCompile([FSharp.Compiler.Service]FSharp.Compiler.CodeAnalysis.LegacyReferenceResolver, string, string[])][offset 0x0000008B][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.Interactive.Shell+FsiStdinSyphon::GetLine(string, int32)][offset 0x00000039][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.Interactive.Shell+MagicAssemblyResolution::ResolveAssemblyCore([FSharp.Compiler.Service]Internal.Utilities.Library.CompilationThreadToken, [FSharp.Compiler.Service]FSharp.Compiler.Text.Range, [FSharp.Compiler.Service]FSharp.Compiler.CompilerConfig+TcConfigBuilder, [FSharp.Compiler.Service]FSharp.Compiler.CompilerImports+TcImports, [FSharp.Compiler.Service]FSharp.Compiler.Interactive.Shell+FsiDynamicCompiler, [FSharp.Compiler.Service]FSharp.Compiler.Interactive.Shell+FsiConsoleOutput, string)][offset 0x00000015][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.Interactive.Shell+clo@3510-790::Invoke([S.P.CoreLib]System.Tuple`3<char[],int32,int32>)][offset 0x000001E5][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.Interactive.Shell+FsiInteractionProcessor::CompletionsForPartialLID([FSharp.Compiler.Service]FSharp.Compiler.Interactive.Shell+FsiDynamicCompilerState, string)][offset 0x0000001B][found Char] Unexpected type on the stack.
[IL]: Error [UnmanagedPointer]: : FSharp.Compiler.Interactive.Shell+Utilities+pointerToNativeInt@110::Invoke(object)][offset 0x00000007] Unmanaged pointers are not a verifiable type.
[IL]: Error [StackUnexpected]: : <StartupCode$FSharp-Compiler-Service>.$FSharpCheckerResults+dataTipOfReferences@2205::Invoke([FSharp.Core]Microsoft.FSharp.Core.Unit)][offset 0x00000084][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.EditorServices.AssemblyContent+traverseMemberFunctionAndValues@176::Invoke([FSharp.Compiler.Service]FSharp.Compiler.Symbols.FSharpMemberOrFunctionOrValue)][offset 0x00000059][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.EditorServices.AssemblyContent+traverseEntity@218::GenerateNext([S.P.CoreLib]System.Collections.Generic.IEnumerable`1<FSharp.Compiler.EditorServices.AssemblySymbol>&)][offset 0x000000DA][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.EditorServices.ParsedInput+visitor@1423-6::VisitExpr([FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<FSharp.Compiler.Syntax.SyntaxNode>, [FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<FSharp.Compiler.Syntax.SynExpr,Microsoft.FSharp.Core.FSharpOption`1<FSharp.Compiler.EditorServices.CompletionContext>>, [FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<FSharp.Compiler.Syntax.SynExpr,Microsoft.FSharp.Core.FSharpOption`1<FSharp.Compiler.EditorServices.CompletionContext>>, [FSharp.Compiler.Service]FSharp.Compiler.Syntax.SynExpr)][offset 0x00000605][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : <StartupCode$FSharp-Compiler-Service>.$ServiceLexing+clo@921-502::Invoke([FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`3<FSharp.Compiler.Parser+token,int32,int32>,Microsoft.FSharp.Core.Unit>)][offset 0x00000032][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : <StartupCode$FSharp-Compiler-Service>.$ServiceLexing+clo@921-502::Invoke([FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`3<FSharp.Compiler.Parser+token,int32,int32>,Microsoft.FSharp.Core.Unit>)][offset 0x0000003B][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : <StartupCode$FSharp-Compiler-Service>.$ServiceLexing+clo@921-502::Invoke([FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`3<FSharp.Compiler.Parser+token,int32,int32>,Microsoft.FSharp.Core.Unit>)][offset 0x00000082][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : <StartupCode$FSharp-Compiler-Service>.$ServiceLexing+clo@921-502::Invoke([FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`3<FSharp.Compiler.Parser+token,int32,int32>,Microsoft.FSharp.Core.Unit>)][offset 0x0000008B][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : <StartupCode$FSharp-Compiler-Service>.$ServiceLexing+clo@921-502::Invoke([FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<System.Tuple`3<FSharp.Compiler.Parser+token,int32,int32>,Microsoft.FSharp.Core.Unit>)][offset 0x00000094][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : <StartupCode$FSharp-Compiler-Service>.$Symbols+fullName@2490-1::Invoke([FSharp.Core]Microsoft.FSharp.Core.Unit)][offset 0x00000015][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CreateILModule+MainModuleBuilder::ConvertProductVersionToILVersionInfo(string)][offset 0x00000011][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.StaticLinking+TypeForwarding::followTypeForwardForILTypeRef([FSharp.Compiler.Service]FSharp.Compiler.AbstractIL.IL+ILTypeRef)][offset 0x00000010][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CompilerOptions::getCompilerOption([FSharp.Compiler.Service]FSharp.Compiler.CompilerOptions+CompilerOption, [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<int32>)][offset 0x000000E6][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CompilerOptions::AddPathMapping([FSharp.Compiler.Service]FSharp.Compiler.CompilerConfig+TcConfigBuilder, string)][offset 0x0000000B][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CompilerOptions::subSystemVersionSwitch([FSharp.Compiler.Service]FSharp.Compiler.CompilerConfig+TcConfigBuilder, string)][offset 0x00000030][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CompilerOptions+parseOption@269::Invoke(string)][offset 0x0000000B][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CompilerOptions+getOptionArgList@307::Invoke([FSharp.Compiler.Service]FSharp.Compiler.CompilerOptions+CompilerOption, string)][offset 0x00000035][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CompilerOptions+getOptionArgList@307::Invoke([FSharp.Compiler.Service]FSharp.Compiler.CompilerOptions+CompilerOption, string)][offset 0x0000003E][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CompilerOptions+getSwitch@325::Invoke(string)][offset 0x0000000B][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CompilerOptions+attempt@373::Invoke([FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<FSharp.Compiler.CompilerOptions+CompilerOption>)][offset 0x00000E9F][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CompilerOptions+processArg@333::Invoke([FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<string>)][offset 0x0000004D][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CompilerOptions+ResponseFile+parseLine@239::Invoke(string)][offset 0x00000031][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.ParseAndCheckInputs+Pipe #1 stage #1 at line 1865@1865::Invoke(int32)][offset 0x00000030][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.ParseAndCheckInputs+Pipe #1 stage #1 at line 1865@1865::Invoke(int32)][offset 0x00000039][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CompilerImports+line@560-1::Invoke(string)][offset 0x0000000B][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CompilerConfig+TcConfig::.ctor([FSharp.Compiler.Service]FSharp.Compiler.CompilerConfig+TcConfigBuilder, bool)][offset 0x0000062B][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CompilerConfig+TcConfig::.ctor([FSharp.Compiler.Service]FSharp.Compiler.CompilerConfig+TcConfigBuilder, bool)][offset 0x00000634][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.PatternMatchCompilation::isProblematicClause([FSharp.Compiler.Service]FSharp.Compiler.PatternMatchCompilation+MatchClause)][offset 0x00000065][found Byte] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : <StartupCode$FSharp-Compiler-Service>.$FSharp.Compiler.PatternMatchCompilation::.cctor()][offset 0x00000015][found Boolean] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.NicePrint+TastDefinitionPrinting+meths@2092-3::Invoke([FSharp.Compiler.Service]FSharp.Compiler.Infos+MethInfo)][offset 0x000000BE][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.NicePrint+PrintUtilities::layoutXmlDoc([FSharp.Compiler.Service]FSharp.Compiler.TypedTreeOps+DisplayEnv, bool, [FSharp.Compiler.Service]FSharp.Compiler.Xml.XmlDoc, [FSharp.Compiler.Service]FSharp.Compiler.Text.Layout)][offset 0x00000033][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.TypeProviders::ValidateNamespaceName(string, [FSharp.Compiler.Service]FSharp.Compiler.Tainted`1<Microsoft.FSharp.Core.CompilerServices.ITypeProvider>, [FSharp.Compiler.Service]FSharp.Compiler.Text.Range, string)][offset 0x00000063][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.TypeProviders::ValidateExpectedName([FSharp.Compiler.Service]FSharp.Compiler.Text.Range, string[], string, [FSharp.Compiler.Service]FSharp.Compiler.Tainted`1<FSharp.Compiler.TypeProviders+ProvidedType>)][offset 0x000000AD][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.Lexhelp::stringBufferAsString([FSharp.Compiler.Service]FSharp.Compiler.IO.ByteBuffer)][offset 0x00000099][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.Syntax.PrettyNaming::SplitNamesForILPath(string)][offset 0x00000057][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.Syntax.PrettyNaming::SplitNamesForILPath(string)][offset 0x00000060][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : <StartupCode$FSharp-Compiler-Service>.$FSharp.Compiler.Syntax.PrettyNaming::.cctor()][offset 0x00001220][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : <StartupCode$FSharp-Compiler-Service>.$FSharp.Compiler.Syntax.PrettyNaming::.cctor()][offset 0x00001229][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.AbstractIL.ILBinaryWriter::writeILMetadataAndCode(bool, [FSharp.Compiler.Service]FSharp.Compiler.AbstractIL.IL+ILVersionInfo, [FSharp.Compiler.Service]FSharp.Compiler.AbstractIL.IL+ILGlobals, bool, bool, bool, [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<FSharp.Compiler.AbstractIL.IL+ILAttribute>, [S.P.CoreLib]System.Collections.Generic.IEnumerable`1<FSharp.Compiler.AbstractIL.IL+ILSourceDocument>, [FSharp.Compiler.Service]FSharp.Compiler.AbstractIL.IL+ILModuleDef, int32, [FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<FSharp.Compiler.AbstractIL.IL+ILAssemblyRef,FSharp.Compiler.AbstractIL.IL+ILAssemblyRef>)][offset 0x0000063F][found Boolean] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.AbstractIL.ILBinaryWriter+bigness@3406::Invoke(int32)][offset 0x00000007][found Byte] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.AbstractIL.ILPdbWriter+PortablePdbGenerator::serializeDocumentName(string)][offset 0x00000090][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.AbstractIL.ILPdbWriter+pushShadowedLocals@959::Invoke([FSharp.Core]Microsoft.FSharp.Core.Unit)][offset 0x00000232][found Byte] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.AbstractIL.ILBinaryReader::seekReadUntaggedIdx([FSharp.Compiler.Service]FSharp.Compiler.AbstractIL.BinaryConstants+TableName, [FSharp.Compiler.Service]FSharp.Compiler.AbstractIL.ILBinaryReader+ILMetadataReader, [FSharp.Compiler.Service]FSharp.Compiler.IO.ReadOnlyByteMemory, int32&)][offset 0x0000000D][found Byte] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.AbstractIL.ILBinaryReader::openMetadataReader(string, [FSharp.Compiler.Service]FSharp.Compiler.AbstractIL.ILBinaryReader+BinaryFile, int32, [S.P.CoreLib]System.Tuple`8<uint16,System.Tuple`2<int32,int32>,bool,bool,bool,bool,bool,System.Tuple`5<Microsoft.FSharp.Core.FSharpOption`1<FSharp.Compiler.AbstractIL.IL+ILPlatform>,bool,int32,int32,int32>>, [FSharp.Compiler.Service]FSharp.Compiler.AbstractIL.ILBinaryReader+PEReader, [FSharp.Compiler.Service]FSharp.Compiler.IO.ReadOnlyByteMemory, [FSharp.Core]Microsoft.FSharp.Core.FSharpOption`1<FSharp.Compiler.AbstractIL.ILBinaryReader+PEReader>, bool)][offset 0x00000799][found Boolean] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.AbstractIL.ILBinaryReader+rowKindSize@4453::Invoke([FSharp.Compiler.Service]FSharp.Compiler.AbstractIL.ILBinaryReader+RowKind)][offset 0x00000128][found Byte] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.AbstractIL.NativeRes+VersionHelper::TryParse(string, bool, uint16, bool, [S.P.CoreLib]System.Version&)][offset 0x0000003D][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.AbstractIL.IL::parseILVersion(string)][offset 0x0000000B][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.AbstractIL.IL::parseILVersion(string)][offset 0x00000021][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.AbstractIL.IL+parseNamed@5291::Invoke([FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<System.Tuple`4<string,FSharp.Compiler.AbstractIL.IL+ILType,bool,FSharp.Compiler.AbstractIL.IL+ILAttribElem>>, int32, int32)][offset 0x00000087][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Internal.Utilities.Collections.Utils::shortPath(string)][offset 0x00000015][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Internal.Utilities.FSharpEnvironment+probePathForDotnetHost@321::Invoke([FSharp.Core]Microsoft.FSharp.Core.Unit)][offset 0x00000028][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.CodeAnalysis.SimulatedMSBuildReferenceResolver+Pipe #6 input at line 68@68::FSharp.Compiler.CodeAnalysis.ILegacyReferenceResolver.Resolve([FSharp.Compiler.Service]FSharp.Compiler.CodeAnalysis.LegacyResolutionEnvironment, [S.P.CoreLib]System.Tuple`2<string,string>[], string, [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<string>, string, string, [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<string>, string, [FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<string,Microsoft.FSharp.Core.Unit>, [FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<bool,Microsoft.FSharp.Core.FSharpFunc`2<string,Microsoft.FSharp.Core.FSharpFunc`2<string,Microsoft.FSharp.Core.Unit>>>)][offset 0x0000034D][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : <StartupCode$FSharp-Compiler-Service>.$FSharp.Compiler.DiagnosticsLogger::.cctor()][offset 0x000000CD][found Char] Unexpected type on the stack.
[IL]: Error [CallVirtOnValueType]: : FSharp.Compiler.Text.RangeModule+comparer@546::System.Collections.Generic.IEqualityComparer<FSharp.Compiler.Text.Range>.GetHashCode([FSharp.Compiler.Service]FSharp.Compiler.Text.Range)][offset 0x00000002] Callvirt on a value type method.
[IL]: Error [StackUnexpected]: : Internal.Utilities.PathMapModule::applyDir([FSharp.Compiler.Service]Internal.Utilities.PathMap, string)][offset 0x00000037][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Internal.Utilities.PathMapModule::applyDir([FSharp.Compiler.Service]Internal.Utilities.PathMap, string)][offset 0x00000043][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : <StartupCode$FSharp-Compiler-Service>.$Internal.Utilities.XmlAdapters::.cctor()][offset 0x0000000A][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : <StartupCode$FSharp-Compiler-Service>.$Internal.Utilities.XmlAdapters::.cctor()][offset 0x00000013][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : <StartupCode$FSharp-Compiler-Service>.$Internal.Utilities.XmlAdapters::.cctor()][offset 0x0000001C][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : <StartupCode$FSharp-Compiler-Service>.$Internal.Utilities.XmlAdapters::.cctor()][offset 0x00000025][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : <StartupCode$FSharp-Compiler-Service>.$Internal.Utilities.XmlAdapters::.cctor()][offset 0x0000002E][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.IO.FileSystemUtils::trimQuotes(string)][offset 0x0000000B][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : FSharp.Compiler.IO.FileSystemUtils::trimQuotes(string)][offset 0x00000014][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Internal.Utilities.Library.String::lowerCaseFirstChar(string)][offset 0x0000003F][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Internal.Utilities.Library.Array+loop@276-4::Invoke(int32)][offset 0x00000012][found Byte] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Microsoft.FSharp.Collections.ArrayModule+Parallel::Choose([FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<!!0,Microsoft.FSharp.Core.FSharpOption`1<!!1>>, !!0[])][offset 0x000000A0][found Byte] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Microsoft.FSharp.Collections.ArrayModule+Parallel::Filter([FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<!!0,bool>, !!0[])][offset 0x00000029][found Byte] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Microsoft.FSharp.Collections.ArrayModule+Parallel::Partition([FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<!!0,bool>, !!0[])][offset 0x00000038][found Byte] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Microsoft.FSharp.Collections.ArrayModule+Parallel+Pipe #2 input at line 2241@2245-1::Invoke(int32, [System.Threading.Tasks.Parallel]System.Threading.Tasks.ParallelLoopState, int32)][offset 0x00000030][found Boolean] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Microsoft.FSharp.Collections.ArrayModule+Parallel+Pipe #2 input at line 2571@2575-1::Invoke(int32, [System.Threading.Tasks.Parallel]System.Threading.Tasks.ParallelLoopState, int32)][offset 0x00000022][found Boolean] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Microsoft.FSharp.Core.StringModule::Map([FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<char,char>, string)][offset 0x00000020][found Short] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Microsoft.FSharp.Core.StringModule::Map([FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<char,char>, string)][offset 0x00000031][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Microsoft.FSharp.Core.StringModule::MapIndexed([FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<int32,Microsoft.FSharp.Core.FSharpFunc`2<char,char>>, string)][offset 0x00000029][found Short] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Microsoft.FSharp.Core.StringModule::MapIndexed([FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<int32,Microsoft.FSharp.Core.FSharpFunc`2<char,char>>, string)][offset 0x00000033][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Microsoft.FSharp.Core.StringModule::Filter([FSharp.Core]Microsoft.FSharp.Core.FSharpFunc`2<char,bool>, string)][offset 0x00000075][found Char] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Microsoft.FSharp.Core.Operators::castToString(!!0)][offset 0x00000001][found value 'T'][expected ref 'string'] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Microsoft.FSharp.Core.LanguagePrimitives::retype(!!0)][offset 0x00000001][found value 'T'][expected value 'TResult'] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Microsoft.FSharp.Core.LanguagePrimitives+HashCompare::GenericEqualityCharArray(char[], char[])][offset 0x0000001C][found Short] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Microsoft.FSharp.Core.LanguagePrimitives+HashCompare::GenericEqualityCharArray(char[], char[])][offset 0x00000023][found Short] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Microsoft.FSharp.Core.BasicInlinedOperations::castclassPrim(object)][offset 0x00000006][found ref 'T'][expected value 'T'] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Microsoft.FSharp.Core.BasicInlinedOperations::notnullPrim(!!0)][offset 0x00000002][found Nullobjref 'NullReference'][expected value 'T'] Unexpected type on the stack.
[IL]: Error [StackUnexpected]: : Microsoft.FSharp.Core.BasicInlinedOperations::iscastPrim(object)][offset 0x00000006][found ref 'T'][expected value 'T'] Unexpected type on the stack.
