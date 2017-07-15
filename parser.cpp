#define _CRT_SECURE_NO_WARNINGS
#define _SCL_SECURE_NO_WARNINGS

#pragma warning(disable:4146)
#pragma warning(disable:4291)

#if 0
#pragma comment(lib, "LLVMSupport.lib")
#pragma comment(lib, "LLVMMC.lib")
#pragma comment(lib, "LLVMMCParser.lib")
#pragma comment(lib, "LLVMCore.lib")
#pragma comment(lib, "LLVMMCJIT.lib")
#pragma comment(lib, "LLVMObject.lib")
#pragma comment(lib, "LLVMExecutionEngine.lib")
#pragma comment(lib, "LLVMBitReader.lib")
#pragma comment(lib, "LLVMOption.lib")
#pragma comment(lib, "LLVMTransformUtils.lib")
#pragma comment(lib, "LLVMAnalysis.lib")
#pragma comment(lib, "LLVMRuntimeDyld.lib")
#pragma comment(lib, "LLVMTarget.lib")
#pragma comment(lib, "LLVMProfileData.lib")
#pragma comment(lib, "LLVMScalarOpts.lib")
#pragma comment(lib, "LLVMInstCombine.lib")
#pragma comment(lib, "LLVMipo.lib")
//#pragma comment(lib, "LLVMipa.lib")
#pragma comment(lib, "LLVMVectorize.lib")
#pragma comment(lib, "LLVMObjCARCOpts.lib")
#pragma comment(lib, "LLVMInstrumentation.lib")
#pragma comment(lib, "LLVMBitWriter.lib")
#pragma comment(lib, "LLVMCodeGen.lib")
#pragma comment(lib, "LLVMLinker.lib")
#pragma comment(lib, "LLVMSelectionDAG.lib")
#pragma comment(lib, "LLVMAsmPrinter.lib")
#pragma comment(lib, "LLVMMCDisassembler.lib")
#pragma comment(lib, "LLVMIRReader.lib")
#pragma comment(lib, "LLVMAsmParser.lib")
/*
#pragma comment(lib, "LLVMX86AsmPrinter.lib")
#pragma comment(lib, "LLVMX86Info.lib")
#pragma comment(lib, "LLVMX86AsmPrinter.lib")
#pragma comment(lib, "LLVMX86CodeGen.lib")
#pragma comment(lib, "LLVMX86Utils.lib")
#pragma comment(lib, "LLVMX86Desc.lib")
*/
#pragma comment(lib, "clangFrontend.lib")
#pragma comment(lib, "clangSema.lib")
#pragma comment(lib, "clangAST.lib")
#pragma comment(lib, "clangBasic.lib")
#pragma comment(lib, "clangAnalysis.lib")
#pragma comment(lib, "clangLex.lib")
#pragma comment(lib, "clangEdit.lib")
#pragma comment(lib, "clangSerialization.lib")
#pragma comment(lib, "clangParse.lib")
#pragma comment(lib, "clangDriver.lib")
#pragma comment(lib, "clangCodeGen.lib")
#endif 

#pragma comment(lib, "Mincore.lib")
#pragma comment(lib, "version.lib")

#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclTemplate.h"

#include "clang/CodeGen/CodeGenAction.h"
#include "clang/Basic/DiagnosticOptions.h"
#include "clang/Driver/Compilation.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/Tool.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/CompilerInvocation.h"
#include "clang/Frontend/FrontendDiagnostic.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"

#include "llvm/ADT/SmallString.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>

#include <stdio.h>
#include <crtdbg.h>
#include <direct.h>
#include <stdlib.h>

#define CWD_MAX 2048
char currentPath[CWD_MAX];

using namespace clang;
using namespace clang::driver;

#define yASSERT assert
#define yFAIL(x) assert(0)
#define yWARNING(x)
typedef unsigned long int u32;

// This function isn't referenced outside its translation unit, but it
// can't use the "static" keyword because its address is used for
// GetMainExecutable (since some platforms don't support taking the
// address of main, and some platforms can't implement GetMainExecutable
// without being given the address of a function in the main executable).
std::string GetExecutablePath(const char *Argv0) {
	// This just needs to be some symbol in the binary; C++ doesn't
	// allow taking the address of ::main however.
	void *MainAddr = (void*)(intptr_t)GetExecutablePath;
	return llvm::sys::fs::getMainExecutable(Argv0, MainAddr);
}

static llvm::ExecutionEngine *
createExecutionEngine(std::unique_ptr<llvm::Module> M, std::string *ErrorStr) {
	return llvm::EngineBuilder(std::move(M))
		.setEngineKind(llvm::EngineKind::Either)
		.setErrorStr(ErrorStr)
		.create();
}

static int Execute(std::unique_ptr<llvm::Module> Mod, char *const *envp) {
	llvm::InitializeNativeTarget();
	llvm::InitializeNativeTargetAsmPrinter();

	llvm::Module &M = *Mod;
	std::string Error;
	std::unique_ptr<llvm::ExecutionEngine> EE(
		createExecutionEngine(std::move(Mod), &Error));
	if (!EE) {
		llvm::errs() << "unable to make execution engine: " << Error << "\n";
		return 255;
	}

	llvm::Function *EntryFn = M.getFunction("main");
	if (!EntryFn) {
		llvm::errs() << "'main' function not found in module.\n";
		return 255;
	}

	// FIXME: Support passing arguments.
	std::vector<std::string> Args;
	Args.push_back(M.getModuleIdentifier());

	EE->finalizeObject();
	return EE->runFunctionAsMain(EntryFn, Args, envp);
}

bool shouldSchematize(const CXXRecordDecl *decl)
{
	for (Decl::attr_iterator attributeIterator = decl->attr_begin(),
		attributeIteratorEnd = decl->attr_end();
		attributeIterator != attributeIteratorEnd; ++attributeIterator)
	{
		Attr *pAttribute = *attributeIterator;

		AnnotateAttr* annotateAttr = llvm::dyn_cast<AnnotateAttr>(pAttribute);
		if (annotateAttr)
		{
			StringRef annotationString = annotateAttr->getAnnotation();
			const std::string annotation = annotationString.str();

			if (strcmp(annotation.c_str(), "ySchematize") == 0)
			{
				return true;
			}
		}
	}

	// parse methods
	{
		clang::CXXRecordDecl::method_iterator it(decl->method_begin());
		const clang::CXXRecordDecl::method_iterator it_end(decl->method_end());

		while (it != it_end)
		{
			const std::string methodName(it->getNameAsString());
			/*						if (it->isVirtual())
			{
			//std::cout << name << ": virtual member: " << it->getNameAsString() << "\n";
			}*/

			if (strcmp(methodName.c_str(), "getReflectedInfo") == 0)
			{
				return true;
			}

			++it;
		}
	}


	return false;
}

class c_attribute
{
public:
	std::string m_name;
};

enum e_baseTypes
{
	TYPE_Unknown,
	TYPE_BuiltIn,
	TYPE_Pointer,
	TYPE_Array,
	TYPE_Record,
	TYPE_Custom,
};

class c_typeDescription 
{
public:
	c_typeDescription() :
		m_type(TYPE_Unknown),
		m_arraySize(0),
		m_subTypeIndex(-1)
	{

	}

	e_baseTypes m_type;
	u32 m_arraySize;
	std::string m_recordNameDescription;
	int m_subTypeIndex;

	clang::QualType m_qualifiedType;
};

class c_fieldDefinition
{
public:
	std::string m_name;
	int typeIndex;

	std::vector<c_attribute> m_attributes;
};

class c_schematizedClass
{
public:
	c_schematizedClass() :
		m_base(NULL),
		m_shouldDump(false)
	{
	}

	bool m_shouldDump;
	std::string m_name;
	c_schematizedClass* m_base;
	std::vector<c_attribute> m_attributes;

	std::vector<c_fieldDefinition> m_fields;
};

std::vector<c_typeDescription*> g_types;
std::vector<c_schematizedClass*> g_schematizedClass;

void processAttributes(std::vector<c_attribute>& attributes, Decl::attr_iterator begin, Decl::attr_iterator end)
{
	for (Decl::attr_iterator attributeIterator = begin,
		attributeIteratorEnd = end;
		attributeIterator != attributeIteratorEnd; ++attributeIterator)
	{
		Attr *pAttribute = *attributeIterator;

		AnnotateAttr* annotateAttr = llvm::dyn_cast<AnnotateAttr>(pAttribute);
		if (annotateAttr)
		{
			StringRef annotationString = annotateAttr->getAnnotation();
			const std::string annotation = annotationString.str();

			c_attribute newAttribute;
			newAttribute.m_name = annotation;
			attributes.push_back(newAttribute);
		}

	}
}

c_schematizedClass* findSchematizedClass(const char* name)
{
	for (u32 i = 0; i < g_schematizedClass.size(); i++)
	{
		if (strcmp(g_schematizedClass[i]->m_name.c_str(), name) == 0)
		{
			return g_schematizedClass[i];
		}
	}

	return NULL;
}

int processFieldType(c_schematizedClass* pClass, c_fieldDefinition* pNewField, const clang::QualType& qt)
{
	for (int i = 0; i < g_types.size(); i++)
	{
		if (g_types[i]->m_qualifiedType == qt)
			return i;
	}

	c_typeDescription* pTypeDescription = new c_typeDescription;
	pTypeDescription->m_qualifiedType = qt;

	const clang::Type* t = qt.getTypePtr();
	const char* typeClassName = t->getTypeClassName();

	if (const BuiltinType* builtinType = t->getAs<BuiltinType>())
	{
		BuiltinType::Kind eKind = builtinType->getKind();

		switch (eKind)
		{
		case BuiltinType::Bool:
			pTypeDescription->m_type = TYPE_BuiltIn;
			pTypeDescription->m_recordNameDescription = "TYPE_Bool";
			break;
		case BuiltinType::Char_S:
		case BuiltinType::SChar:
			pTypeDescription->m_type = TYPE_BuiltIn;
			pTypeDescription->m_recordNameDescription = "TYPE_S8";
			break;
		case BuiltinType::UChar:
			pTypeDescription->m_type = TYPE_BuiltIn;
			pTypeDescription->m_recordNameDescription = "TYPE_U8";
			break;
		case BuiltinType::UShort:
			pTypeDescription->m_type = TYPE_BuiltIn;
			pTypeDescription->m_recordNameDescription = "TYPE_U16";
			break;
		case BuiltinType::ULong:
			pTypeDescription->m_type = TYPE_BuiltIn;
			pTypeDescription->m_recordNameDescription = "TYPE_U32";
			break;
		case BuiltinType::ULongLong:
			pTypeDescription->m_type = TYPE_BuiltIn;
			pTypeDescription->m_recordNameDescription = "TYPE_U64";
			break;
		case BuiltinType::Float:
			pTypeDescription->m_type = TYPE_BuiltIn;
			pTypeDescription->m_recordNameDescription = "TYPE_Float";
			break;
		default:
			yWARNING("Unhandled builtinType");
			break;
		}
	}
	else if (const PointerType* pointerType = t->getAs<PointerType>())
	{
		QualType& pointeeType = pointerType->getPointeeType();
		pTypeDescription->m_type = TYPE_Pointer;
		pTypeDescription->m_subTypeIndex = processFieldType(pClass, pNewField, pointeeType);
	}
	else if (const TemplateSpecializationType* templateType = t->getAs<TemplateSpecializationType>())
	{
		CXXRecordDecl* recordDecl = templateType->getAsCXXRecordDecl();

		std::string name(recordDecl->getNameAsString());

		std::vector<c_attribute> attributes;
		processAttributes(attributes, recordDecl->attr_begin(), recordDecl->attr_end());

		for (u32 i = 0; i < attributes.size(); i++)
		{
			if (strstr(attributes[i].m_name.c_str(), "ySchematize_Custom:"))
			{
				pTypeDescription->m_type = TYPE_Custom;

				char handlerName[1024] = "";
				strcat(handlerName, name.c_str());
				strcat(handlerName, "<");

				for (unsigned long int argIndex = 0; argIndex < templateType->getNumArgs(); argIndex++)
				{
					const TemplateArgument& argument = templateType->getArg(argIndex);
					QualType argumentType = argument.getAsType();

					const char* typeClassName = argumentType->getTypeClassName();

					if (const TypedefType* typedefType = argumentType->getAs<TypedefType>())
					{
						TypedefNameDecl* pDecl = typedefType->getDecl();
						strcat(handlerName, pDecl->getQualifiedNameAsString().c_str());
					}
					else if (CXXRecordDecl* pRecordDecl = argumentType->getAsCXXRecordDecl())
					{
						strcat(handlerName, pRecordDecl->getNameAsString().c_str());
						pTypeDescription->m_subTypeIndex = processFieldType(pClass, pNewField, argumentType);
					}
					else if (const PointerType* pointerType = argumentType->getAs<PointerType>())
					{
						QualType pointeeType = pointerType->getPointeeType();
						CXXRecordDecl* pRecordDecl = pointeeType->getAsCXXRecordDecl();
						strcat(handlerName, pRecordDecl->getNameAsString().c_str());
						strcat(handlerName, "*");
					}
					else if (const TypedefType* typedefType = argumentType->getAs<TypedefType>())
					{
						TypedefNameDecl* pDecl = typedefType->getDecl();
						strcat(handlerName, pDecl->getNameAsString().c_str());
					}
					else
					{
						yFAIL("");
					}
				}

				strcat(handlerName, ">::");
				strcat(handlerName, strstr(attributes[i].m_name.c_str(), "ySchematize_Custom:") + strlen("ySchematize_Custom:"));

				pTypeDescription->m_recordNameDescription = handlerName;
			}
		}
	}
	else if (const RecordType* recordType = t->getAs<RecordType>())
	{
		RecordDecl* pDecl = recordType->getDecl();

		{
			std::string name(pDecl->getNameAsString());

			std::vector<c_attribute> attributes;

			processAttributes(attributes, pDecl->attr_begin(), pDecl->attr_end());

			for (u32 i = 0; i < attributes.size(); i++)
			{
				if (strstr(attributes[i].m_name.c_str(), "ySchematize_Custom:"))
				{
					pTypeDescription->m_type = TYPE_Custom;

					char handlerName[1024] = "";

					strcat(handlerName, name.c_str());
					strcat(handlerName, "::");
					strcat(handlerName, strstr(attributes[i].m_name.c_str(), "ySchematize_Custom:") + strlen("ySchematize_Custom:"));

					pTypeDescription->m_recordNameDescription = handlerName;
				}
			}
		}

		if (pTypeDescription->m_type == TYPE_Unknown)
		{
			const std::string name(pDecl->getNameAsString());
			pTypeDescription->m_type = TYPE_Record;

			pTypeDescription->m_recordNameDescription = name;

		}
	}
	else if (const EnumType* enumType = t->getAs<EnumType>())
	{
		int i = 0;
	}
	else if (const ConstantArrayType* pArrayType = (const ConstantArrayType*)t)
	{
		pTypeDescription->m_type = TYPE_Array;
		pTypeDescription->m_arraySize = pArrayType->getSize().getSExtValue();

		pTypeDescription->m_subTypeIndex = processFieldType(pClass, pNewField, pArrayType->getElementType());
	}
	else
	{
		yFAIL("");
	}

	int newTypeIndex = g_types.size();
	g_types.push_back(pTypeDescription);

	return newTypeIndex;
}

void processField(c_schematizedClass* pClass, clang::FieldDecl *pField)
{
	const std::string name(pField->getNameAsString());

	const clang::RecordDecl* rd = pField->getParent();
	const clang::QualType qt = pField->getType();

	c_fieldDefinition newField;
	newField.m_name = name;

	newField.typeIndex = processFieldType(pClass, &newField, qt);

	processAttributes(newField.m_attributes, pField->attr_begin(), pField->attr_end());

	pClass->m_fields.push_back(newField);
}

void writeType(FILE* fOutput, c_typeDescription* typeDescription)
{
	fprintf(fOutput, "{");
	//fprintf(fOutput, "%d, %d", pField->m_arraySize, pField->m_isPointer);
	switch (typeDescription->m_type)
	{
	case TYPE_Unknown:
		if (typeDescription->m_recordNameDescription.length())
			fprintf(fOutput, "TYPE_Unknown, 0, NULL, NULL", typeDescription->m_recordNameDescription.c_str());
		else
			fprintf(fOutput, "TYPE_Unknown, 0, NULL, NULL");
		break;
	case TYPE_BuiltIn:
		fprintf(fOutput, "%s, 0, NULL", typeDescription->m_recordNameDescription.c_str());
		break;
	case TYPE_Record:
		if (c_schematizedClass* pSchematizedClass = findSchematizedClass(typeDescription->m_recordNameDescription.c_str()))
			fprintf(fOutput, "TYPE_Record, 0, &%s::m_reflectedClassInfo, NULL", typeDescription->m_recordNameDescription.c_str());
		else
			fprintf(fOutput, "TYPE_Unknown, 0, NULL, NULL");
		break;
	case TYPE_Pointer:
		yASSERT(typeDescription->m_subTypeIndex >= 0);
		fprintf(fOutput, "TYPE_Pointer, 0, &g_reflectedTypeInfo_%d, NULL", typeDescription->m_subTypeIndex);
		break;
	case TYPE_Array:
		yASSERT(typeDescription->m_subTypeIndex >= 0);
		fprintf(fOutput, "TYPE_Array, %d, &g_reflectedTypeInfo_%d, NULL", typeDescription->m_arraySize, typeDescription->m_subTypeIndex);
		break;
	case TYPE_Custom:
		if (typeDescription->m_subTypeIndex >= 0)
		{
			fprintf(fOutput, "TYPE_Custom, 0, &g_reflectedTypeInfo_%d, &%s", typeDescription->m_subTypeIndex, typeDescription->m_recordNameDescription.c_str());
		}
		else
		{
			fprintf(fOutput, "TYPE_Custom, 0, NULL, &%s", typeDescription->m_recordNameDescription.c_str());
		}
		break;
	default:
		yFAIL("");
		break;
	}
	fprintf(fOutput, "};");
	fprintf(fOutput, " // %s", typeDescription->m_qualifiedType.getAsString().c_str());
}

static const clang::FileEntry * getFileEntryForDecl(const clang::Decl * decl, clang::SourceManager * sourceManager)
{
	if (!decl || !sourceManager) {
		return 0;
	}
	clang::SourceLocation sLoc = decl->getLocation();
	clang::FileID fileID = sourceManager->getFileID(sLoc);
	return sourceManager->getFileEntryForID(fileID);
}

static const char * getFileNameForDecl(const clang::Decl * decl, clang::SourceManager * sourceManager)
{
	const clang::FileEntry * fileEntry = getFileEntryForDecl(decl, sourceManager);
	if (!fileEntry) {
		return 0;
	}
	return fileEntry->getName().str().c_str();
}

void copyLower(char* dest, const char* source)
{
	const char* src = source;
	while (*src)
	{
		*(dest++) = tolower(*(src++));
	}
	*(dest) = 0;
}

bool IsInPath(const char* fileName, const char* currentPath)
{
	if (fileName[0] == '.')
		return true;

	char fileNameBuffer[2048];
	char currentPathBuffer[2048];

	copyLower(fileNameBuffer, fileName);
	copyLower(currentPathBuffer, currentPath);

	return strstr(fileNameBuffer, currentPathBuffer);
}

class CustomParseASTConsumer : public ASTConsumer {
public:

	CustomParseASTConsumer(SourceManager* pSourceManager) { m_sourceManager = pSourceManager; }

	SourceManager* m_sourceManager;

	virtual bool HandleTopLevelDecl(DeclGroupRef DG)
	{
		for (DeclGroupRef::iterator I = DG.begin(), E = DG.end(); I != E; ++I)
		{
			const Decl *D = *I;
			
			// If it is a class/struct/union
			if (const CXXRecordDecl *CXXdecl = dyn_cast<CXXRecordDecl>(D))
			{
				const NamedDecl* pNamedDeclaration = dyn_cast<NamedDecl>(D);
				std::string name;
				if (pNamedDeclaration)
				{
					name = pNamedDeclaration->getNameAsString();
				}

				const char* fileName = getFileNameForDecl(*I, m_sourceManager);

				if (!shouldSchematize(CXXdecl))
					break;

				c_schematizedClass* pNewSchema = new c_schematizedClass;
				g_schematizedClass.push_back(pNewSchema);
				pNewSchema->m_name = name.c_str();

				// is this declaration part of the path we care about?
				if (IsInPath(fileName, currentPath))
				{
					pNewSchema->m_shouldDump = true;
				}
				else
				{
					pNewSchema->m_shouldDump = false;
				}

				processAttributes(pNewSchema->m_attributes, CXXdecl->attr_begin(), CXXdecl->attr_end());

				// process base class
				if (CXXdecl->getNumBases())
				{
					yASSERT(CXXdecl->getNumBases() == 1);

					for (CXXRecordDecl::base_class_const_iterator baseClassIteratorIterator = CXXdecl->bases_begin(),
						baseClassIteratorIteratorEnd = CXXdecl->bases_end();
						baseClassIteratorIterator != baseClassIteratorIteratorEnd; ++baseClassIteratorIterator)
					{
						const CXXBaseSpecifier* pBase = baseClassIteratorIterator;
						std::string baseTypeName = pBase->getType().getBaseTypeIdentifier()->getName();

						for (int i = 0; i < g_schematizedClass.size(); i++)
						{
							if (strcmp(g_schematizedClass[i]->m_name.c_str(), baseTypeName.c_str()) == 0)
							{
								pNewSchema->m_base = g_schematizedClass[i];
								break;
							}
						}

						//yASSERT(pNewSchema->m_base);
					}
				}

				// process members
				for (clang::DeclContext::decl_iterator I = CXXdecl->decls_begin(),
					E = CXXdecl->decls_end();
					I != E; ++I)
				{
					clang::DeclaratorDecl *dd = llvm::dyn_cast<clang::DeclaratorDecl>(*I);
					if (!dd /*|| !is_attribute(dd)*/)
						continue;

					// static members are represented as VarDecls, the rest is FieldDecl.
					if (clang::VarDecl *vd = llvm::dyn_cast<clang::VarDecl>(dd))
					{
						// process var
					}
					else if (clang::FieldDecl *fd = llvm::dyn_cast<clang::FieldDecl>(dd))
					{
						processField(pNewSchema, fd);
					}
				}


				//const DeclContext* DC = D->getDeclContext();

				//RD->method_begin()
			}
			else if (const ClassTemplateDecl *pClassTemplateDecl = dyn_cast<ClassTemplateDecl>(D))
			{
				std::vector<c_attribute> attributes;
				processAttributes(attributes, pClassTemplateDecl->attr_begin(), pClassTemplateDecl->attr_end());

				//pClassTemplateDecl->redecls_begin
			}
			else if (const ClassTemplateDecl *pClassTemplateDecl = dyn_cast<ClassTemplateDecl>(D))
			{
				std::vector<c_attribute> attributes;
				processAttributes(attributes, pClassTemplateDecl->attr_begin(), pClassTemplateDecl->attr_end());
			}
		}
		return true;
	}
};

class CustomASTAction : public ASTFrontendAction
{
public:

	std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI, StringRef InFile)
	{
		m_consumer = new CustomParseASTConsumer(/*m_files, &m_DBObject, &CI.getFileManager(),*/ &CI.getSourceManager());
		return std::unique_ptr<ASTConsumer>(m_consumer);
	}

	CustomParseASTConsumer* m_consumer;
	std::vector<std::string> m_files;
};

int main(int argc, const char **argv, char * const *envp)
{
	_getcwd(currentPath, CWD_MAX);

	void *MainAddr = (void*)(intptr_t)GetExecutablePath;
	std::string Path = GetExecutablePath(argv[0]);
	IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts = new DiagnosticOptions();
	TextDiagnosticPrinter *DiagClient =
		new TextDiagnosticPrinter(llvm::errs(), &*DiagOpts);

	IntrusiveRefCntPtr<DiagnosticIDs> DiagID(new DiagnosticIDs());
	DiagnosticsEngine Diags(DiagID, &*DiagOpts, DiagClient);

	// Use ELF on windows for now.
	std::string TripleStr = llvm::sys::getProcessTriple();
	llvm::Triple T(TripleStr);
	if (T.isOSBinFormatCOFF())
		T.setObjectFormat(llvm::Triple::ELF);

	Driver TheDriver(Path, T.str(), Diags);
	TheDriver.setTitle("clang interpreter");
	TheDriver.setCheckInputsExist(false);

	SmallVector<const char *, 128> Args;// (argv, argv + argc);

	Args.push_back(argv[0]);
	Args.push_back(argv[1]);

	const char* outputFile = argv[2];

	FILE* fOutput = fopen(outputFile, "w+");
	// printf debug stuff
	fprintf(fOutput, "//debug command: \"%s\" \"%s\" \"%s\"\n", argv[1], argv[2], argv[3], argv[4]);
	fprintf(fOutput, "//CWD: %s\n", currentPath);


	int numUsedIncludes = 0;
	const char* decodeIncludePath = argv[3];
	char includePathBuffer[256][2048];

	if (strlen(decodeIncludePath) > 2048)
	{
		llvm::errs() << "Include path too long";
		return 1;
	}

	while (decodeIncludePath)
	{
		char* currentDecodeIncudePath = (char*)decodeIncludePath;

		if (char* end = (char*)strchr(currentDecodeIncudePath, ';'))
		{
			*end = 0;
			decodeIncludePath = end + 1;
		}
		else
		{
			decodeIncludePath = NULL;
		}

		if (strstr(currentDecodeIncudePath, "Program Files"))
		{
			if (char* visualVersion = strstr(currentDecodeIncudePath, "14.0"))
			{
				visualVersion[1] = '2';
			}
			strcpy(includePathBuffer[numUsedIncludes], "-isystem");
		}
		else
		{
			strcpy(includePathBuffer[numUsedIncludes], "-I");
		}

		strcat(includePathBuffer[numUsedIncludes], currentDecodeIncudePath);

		if (strlen(includePathBuffer[numUsedIncludes]))
		{
			Args.push_back(includePathBuffer[numUsedIncludes]);
		}

		numUsedIncludes++;
	}


	/*
	llvm::errs() << argv[2];
	llvm::errs() << argv[3];
	printf("output: %s\n", argv[2]);
	printf("include: %s\n", argv[3]);
	*/
	Args.push_back("-fsyntax-only");

	Args.push_back("-fms-compatibility-version=19.00");

	// Ignore warnings due to include windows headers
	Args.push_back("-Wno-ignored-attributes");
	Args.push_back("-Wno-microsoft");
	Args.push_back("-Wno-unused-value");
	Args.push_back("-Wno-comment");
	Args.push_back("-DyPARSER");

#ifdef _DEBUG
	Args.push_back("-v");
#endif

	//_putenv("VCINSTALLDIR=\"C:\\Program Files (x86)\\Microsoft Visual Studio 12.0\\VC\"");

	std::unique_ptr<Compilation> C(TheDriver.BuildCompilation(Args));
	if (!C)
		return 0;

	// FIXME: This is copied from ASTUnit.cpp; simplify and eliminate.

	// We expect to get back exactly one command job, if we didn't something
	// failed. Extract that job from the compilation.
	const driver::JobList &Jobs = C->getJobs();
	if (Jobs.size() != 1 || !isa<driver::Command>(*Jobs.begin())) {
		SmallString<256> Msg;
		llvm::raw_svector_ostream OS(Msg);
		Jobs.Print(OS, "; ", true);
		Diags.Report(diag::err_fe_expected_compiler_job) << OS.str();
		return 1;
	}

	CustomASTAction* parseAction = new CustomASTAction;
	std::unique_ptr<CustomASTAction> Act(parseAction);

	const driver::Command &Cmd = cast<driver::Command>(*Jobs.begin());
	if (llvm::StringRef(Cmd.getCreator().getName()) != "clang") {
		Diags.Report(diag::err_fe_expected_clang_command);
		return 1;
	}

	// Initialize a compiler invocation object from the clang (-cc1) arguments.
	const driver::ArgStringList &CCArgs = Cmd.getArguments();
	std::unique_ptr<CompilerInvocation> CI(new CompilerInvocation);
	CompilerInvocation::CreateFromArgs(*CI,
		const_cast<const char **>(CCArgs.data()),
		const_cast<const char **>(CCArgs.data()) +
		CCArgs.size(),
		Diags);

	// Show the invocation, with -v.
	if (CI->getHeaderSearchOpts().Verbose) {
		llvm::errs() << "clang invocation:\n";
		Jobs.Print(llvm::errs(), "\n", true);
		llvm::errs() << "\n";
	}

	// FIXME: This is copied from cc1_main.cpp; simplify and eliminate.

	// Create a compiler instance to handle the actual work.
	CompilerInstance Clang;
	Clang.setInvocation(std::move(CI));

	// Create the compilers actual diagnostics engine.
	Clang.createDiagnostics();
	if (!Clang.hasDiagnostics())
		return 1;

	// Infer the builtin include path if unspecified.
	if (Clang.getHeaderSearchOpts().UseBuiltinIncludes &&
		Clang.getHeaderSearchOpts().ResourceDir.empty())
		Clang.getHeaderSearchOpts().ResourceDir =
		CompilerInvocation::GetResourcesPath(argv[0], MainAddr);

	// Create and execute the frontend to generate an LLVM bitcode module.
	//std::unique_ptr<CodeGenAction> Act(new EmitLLVMOnlyAction());
	if (!Clang.ExecuteAction(*Act))
		return 1;

	// Dump

	// Types
	for (u32 typeIndex = 0; typeIndex < g_types.size(); typeIndex++)
	{
		c_typeDescription* pType = g_types[typeIndex];

		fprintf(fOutput, "static const s_reflectedTypeInfo g_reflectedTypeInfo_%d= ", typeIndex);
		writeType(fOutput, pType);

		fprintf(fOutput, "\n");
	}

	fprintf(fOutput, "\n");

	for (u32 i = 0; i < g_schematizedClass.size(); i++)
	{
		c_schematizedClass* pClass = g_schematizedClass[i];

		if (!pClass->m_shouldDump)
			continue;

		// Fields
		if (pClass->m_fields.size())
		{
			fprintf(fOutput, "const s_reflectedFieldInfo %s::m_reflectedFieldInfo[]=\n", pClass->m_name.c_str());
			fprintf(fOutput, "{\n");
			for (u32 fieldIndex = 0; fieldIndex < pClass->m_fields.size(); fieldIndex++)
			{
				c_fieldDefinition* pField = &pClass->m_fields[fieldIndex];

				fprintf(fOutput, "\t{");
				fprintf(fOutput, "\"%s\", offsetof(%s,%s), ", pField->m_name.c_str(), pClass->m_name.c_str(), pField->m_name.c_str());

				//write type
				fprintf(fOutput, "&g_reflectedTypeInfo_%d ", pField->typeIndex);

				fprintf(fOutput, "},\n");
			}
			fprintf(fOutput, "\t{");
			fprintf(fOutput, "NULL, 0");
			fprintf(fOutput, "},\n");
			fprintf(fOutput, "};\n");
		}

		// class itself
		fprintf(fOutput, "const s_reflectedClassInfo %s::m_reflectedClassInfo=\n", pClass->m_name.c_str());
		fprintf(fOutput, "{\n");
		fprintf(fOutput, "\t\"%s\", //class name\n", pClass->m_name.c_str()); // class name
		if (pClass->m_base)
		{
			fprintf(fOutput, "\t&%s::m_reflectedClassInfo, //base class\n", pClass->m_base->m_name.c_str()); // base class
		}
		else
		{
			fprintf(fOutput, "\tNULL, //base class\n");
		}
		if (pClass->m_fields.size())
		{
			fprintf(fOutput, "\tm_reflectedFieldInfo,\n");
		}
		else
		{
			fprintf(fOutput, "\tNULL,\n");
		}

		// attributes
		bool bHasCustomHandler = false;
		for (u32 i = 0; i < pClass->m_attributes.size(); i++)
		{
			c_attribute& attribute = pClass->m_attributes[i];

			if (strstr(attribute.m_name.c_str(), "ySchematize_Custom:"))
			{
				char handlerName[1024] = "";
				strcat(handlerName, pClass->m_name.c_str());
				strcat(handlerName, "::");
				strcat(handlerName, strstr(attribute.m_name.c_str(), "ySchematize_Custom:") + strlen("ySchematize_Custom:"));

				fprintf(fOutput, "\t%s,\n", handlerName);
				bHasCustomHandler = true;
			}
		}

		if (!bHasCustomHandler)
		{
			fprintf(fOutput, "\tNULL,\n");
		}


		fprintf(fOutput, "};\n");
	}

	fclose(fOutput);

	int Res = 0;
/*	if (std::unique_ptr<llvm::Module> Module = Act->takeModule())
		Res = Execute(std::move(Module), envp);
		*/

	// Shutdown.
	llvm::llvm_shutdown();

	return Res;
}

bool assert_handler(char const *, char const *, unsigned long)
{
	__debugbreak();
	return false;
}

void warning_handler(char const *, unsigned long, char const *, ...)
{

}
