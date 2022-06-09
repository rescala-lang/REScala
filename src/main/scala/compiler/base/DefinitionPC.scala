package compiler.base

import clangast.decl.{CDecl, CFunctionDecl, CParmVarDecl, CVarDecl}
import compiler.{CompilerCascade, PartialCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

trait DefinitionPC extends PartialCompiler {
  def compileDefinition(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Definition, CDecl] = PartialFunction.empty

  def compileDefDef(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.DefDef, CFunctionDecl] = PartialFunction.empty

  def compileValDefToCVarDecl(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CVarDecl] = PartialFunction.empty

  def compileValDefToCParmVarDecl(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CParmVarDecl] = PartialFunction.empty
}

extension (p: PartialCompiler) {
  def compileDefinition(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.Definition, CDecl] = PartialCompiler.ensurePC[DefinitionPC](p, _.compileDefinition)

  def compileDefDef(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.DefDef, CFunctionDecl] = PartialCompiler.ensurePC[DefinitionPC](p, _.compileDefDef)

  def compileValDefToCVarDecl(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CVarDecl] = PartialCompiler.ensurePC[DefinitionPC](p, _.compileValDefToCVarDecl)

  def compileValDefToCParmVarDecl(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CParmVarDecl] = PartialCompiler.ensurePC[DefinitionPC](p, _.compileValDefToCParmVarDecl)
}
