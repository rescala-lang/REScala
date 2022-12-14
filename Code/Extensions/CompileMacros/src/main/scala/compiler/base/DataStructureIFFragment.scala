package compiler.base

import clangast.*
import clangast.given
import clangast.decl.{CFunctionDecl, CRecordDecl}
import clangast.expr.{CCallExpr, CCastExpr, CExpr, CSizeofExpr}
import clangast.stmt.CCompoundStmt
import clangast.stubs.StdLibH
import clangast.types.{CIntegerType, CPointerType}
import compiler.{CompilerFragment, FragmentedCompiler}
import compiler.FragmentedCompiler.dispatch
import compiler.context.{RecordDeclTC, TranslationContext}

import scala.quoted.*

trait DataStructureIFFragment extends CompilerFragment {
  protected val refCountFieldName = "refCount"

  protected val RETAIN    = "RETAIN"
  protected val RELEASE   = "RELEASE"
  protected val DEEP_COPY = "DEEP_COPY"

  protected def getRecordDecl(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      ctx: RecordDeclTC
  ): CRecordDecl = {
    ctx.nameToRecordDecl.getOrElseUpdate(
      dispatch[TypeIFFragment](_.typeName)(tpe),
      dispatch[DataStructureIFFragment](_.compileTypeToCRecordDecl)(tpe)
    )
  }

  def refCountField(using TranslationContext): (String, CCastExpr) = {
    refCountFieldName ->
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
  }

  protected def allocRefCount(using Quotes)(tpe: quotes.reflect.TypeRepr)(using FragmentedCompiler)(using
      TranslationContext
  ): Option[(String, CExpr)] =
    if dispatch[DataStructureIFFragment](_.usesRefCount)(tpe)
    then Some(refCountField)
    else None

  def compileTypeToCRecordDecl(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CRecordDecl] = PartialFunction.empty

  def usesRefCount(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, Boolean] = PartialFunction.empty

  def compileRetain(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = PartialFunction.empty

  def compileFree(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[(CExpr, quotes.reflect.TypeRepr), CCompoundStmt] = PartialFunction.empty

  def compileRelease(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = PartialFunction.empty

  def compileDeepCopy(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = PartialFunction.empty
}
