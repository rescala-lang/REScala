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
    PartialFunction[quotes.reflect.Definition, CDecl] = p match {
      case definitionPC: DefinitionPC => definitionPC.compileDefinition
      case _ => PartialFunction.empty
    }

  def compileDefDef(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.DefDef, CFunctionDecl] = p match {
      case definitionPC: DefinitionPC => definitionPC.compileDefDef
      case _ => PartialFunction.empty
    }

  def compileValDefToCVarDecl(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CVarDecl] = p match {
      case definitionPC: DefinitionPC => definitionPC.compileValDefToCVarDecl
      case _ => PartialFunction.empty
    }

  def compileValDefToCParmVarDecl(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.ValDef, CParmVarDecl] = p match {
      case definitionPC: DefinitionPC => definitionPC.compileValDefToCParmVarDecl
      case _ => PartialFunction.empty
    }
}
