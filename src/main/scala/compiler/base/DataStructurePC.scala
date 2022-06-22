package compiler.base

import clangast.*
import clangast.given
import clangast.decl.{CFunctionDecl, CRecordDecl}
import clangast.expr.{CCallExpr, CCastExpr, CExpr, CSizeofExpr}
import clangast.stmt.{CCompoundStmt, CStmt}
import clangast.stubs.StdLibH
import clangast.types.{CIntegerType, CPointerType}
import compiler.{CompilerCascade, PartialCompiler}
import compiler.context.{RecordDeclTC, TranslationContext}

import scala.quoted.*

trait DataStructurePC extends PartialCompiler {
  protected val refCountField = "refCount"

  protected def getRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: RecordDeclTC, cascade: CompilerCascade): CRecordDecl = {
    ctx.nameToRecordDecl.getOrElseUpdate(cascade.dispatch(_.typeName)(tpe), cascade.dispatch(_.compileTypeToCRecordDecl)(tpe))
  }

  protected def allocRefCount(using Quotes)(tpe: quotes.reflect.TypeRepr)(using ctx: TranslationContext, cascade: CompilerCascade): Option[(String, CExpr)] =
    if cascade.dispatch(_.usesRefCount)(tpe) then
      Some(refCountField ->
        CCastExpr(
          CCallExpr(
            StdLibH.calloc.ref,
            List(
              1.lit,
              CSizeofExpr(Left(CIntegerType))
            )
          ),
          CPointerType(CIntegerType)
        )
      )
    else None

  def compileTypeToCRecordDecl(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CRecordDecl] = PartialFunction.empty

  def usesRefCount(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, Boolean] = PartialFunction.empty

  def compileRetain(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = PartialFunction.empty

  def compileFree(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CCompoundStmt] = PartialFunction.empty

  def compileRelease(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = PartialFunction.empty
}

extension (p: PartialCompiler) {
  def compileTypeToCRecordDecl(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CRecordDecl] = PartialCompiler.ensurePC[DataStructurePC](p, _.compileTypeToCRecordDecl)

  def usesRefCount(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, Boolean] = PartialCompiler.ensurePC[DataStructurePC](p, _.usesRefCount)

  def compileRetain(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = PartialCompiler.ensurePC[DataStructurePC](p, _.compileRetain)

  def compileFree(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(CExpr, quotes.reflect.TypeRepr), CCompoundStmt] = PartialCompiler.ensurePC[DataStructurePC](p, _.compileFree)

  def compileRelease(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = PartialCompiler.ensurePC[DataStructurePC](p, _.compileRelease)
}
