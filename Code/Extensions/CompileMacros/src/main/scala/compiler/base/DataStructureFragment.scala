package compiler.base

import clangast.*
import clangast.given
import clangast.decl.{CFunctionDecl, CParmVarDecl, CRecordDecl, CVarDecl}
import clangast.expr.binaryop.{CAndExpr, CLessEqualsExpr, CLessThanExpr}
import clangast.expr.unaryop.{CDecExpr, CDerefExpr, CIncExpr, CNotExpr}
import clangast.expr.{CArraySubscriptExpr, CCallExpr, CExpr, CFalseLiteral, CMemberExpr, CParenExpr}
import clangast.stmt.{CCompoundStmt, CDeclStmt, CForStmt, CIfStmt, CReturnStmt, CStmt}
import clangast.stubs.{StdBoolH, StdLibH}
import clangast.types.{CIntegerType, CQualType, CRecordType, CVoidType}
import compiler.FragmentedCompiler
import compiler.FragmentedCompiler.dispatch
import compiler.context.{RecordDeclTC, TranslationContext}

import scala.quoted.*

object DataStructureFragment extends DataStructureIFFragment {
  override def compileTypeToCRecordDecl(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CRecordDecl] = {
    import quotes.reflect.*

    {
      case MethodType(_, _, tpe) => dispatch[DataStructureIFFragment](_.compileTypeToCRecordDecl)(tpe)
    }
  }

  override def usesRefCount(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, Boolean] = {
    import quotes.reflect.*

    {
      case MethodType(_, _, tpe) => dispatch[DataStructureIFFragment](_.usesRefCount)(tpe)
      case _                     => false
    }
  }

  override def compileRetain(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC] { ctx ?=>
    import quotes.reflect.*

    {
      case tpe if dispatch[DataStructureIFFragment](_.usesRefCount)(tpe) =>
        ctx.recordFunMap.getOrElseUpdate(
          dispatch[TypeIFFragment](_.typeName)(tpe) -> RETAIN, {
            val recordDecl = getRecordDecl(tpe)

            val name = "retain_" + recordDecl.name

            val param = CParmVarDecl("rec", recordDecl.getTypeForDecl)

            val body = CCompoundStmt(List(
              CIncExpr(CParenExpr(CDerefExpr(CMemberExpr(param.ref, refCountField)))),
              CReturnStmt(Some(param.ref))
            ))

            CFunctionDecl(name, List(param), recordDecl.getTypeForDecl, Some(body))
          }
        )
    }
  }

  override def compileRelease(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.TypeRepr, CFunctionDecl] = ensureCtx[RecordDeclTC] { ctx ?=>
    import quotes.reflect.*

    {
      case tpe if dispatch[DataStructureIFFragment](_.usesRefCount)(tpe) =>
        ctx.recordFunMap.getOrElseUpdate(
          dispatch[TypeIFFragment](_.typeName)(tpe) -> RELEASE, {
            val recordDecl = getRecordDecl(tpe)

            val name = "release_" + recordDecl.name

            val param        = CParmVarDecl("rec", recordDecl.getTypeForDecl)
            val keepWithZero = CParmVarDecl("keep_with_zero", StdBoolH.bool)

            val body = CCompoundStmt(List(
              CDecExpr(CParenExpr(CDerefExpr(CMemberExpr(param.ref, refCountField)))),
              CIfStmt(
                CAndExpr(
                  CLessEqualsExpr(CDerefExpr(CMemberExpr(param.ref, refCountField)), 0.lit),
                  CNotExpr(keepWithZero.ref)
                ),
                dispatch[DataStructureIFFragment](_.compileFree)(param.ref, tpe)
              )
            ))

            CFunctionDecl(name, List(param, keepWithZero), CVoidType, Some(body))
          }
        )
    }
  }

  def retain(using Quotes)(expr: CExpr, tpe: quotes.reflect.TypeRepr)(using fc: FragmentedCompiler)(using
      TranslationContext
  ): CExpr =
    fc.dispatchLifted[DataStructureIFFragment](_.compileRetain)(tpe) match {
      case None    => expr
      case Some(f) => CCallExpr(f.ref, List(expr))
    }

  def release(using Quotes)(expr: CExpr, tpe: quotes.reflect.TypeRepr, keepWithZero: CExpr)(using
      fc: FragmentedCompiler
  )(using TranslationContext): Option[CStmt] =
    fc.dispatchLifted[DataStructureIFFragment](_.compileRelease)(tpe).map { f =>
      CCallExpr(f.ref, List(expr, keepWithZero))
    }

  def release(varDecl: CVarDecl, keepWithZero: CExpr)(using ctx: TranslationContext): Option[CStmt] =
    varDecl.declaredType match {
      case CQualType(CRecordType(declName), _) =>
        val funName = "release_" + declName
        ctx.valueDeclList.find {
          case CFunctionDecl(`funName`, _, _, _, _) => true
          case _                                    => false
        }.map(f => CCallExpr(f.ref, List(varDecl.ref, keepWithZero)))
      case _ => None
    }

  def releaseLocalVars(stmts: List[CStmt])(using TranslationContext): List[CStmt] = stmts.flatMap {
    case CDeclStmt(varDecl: CVarDecl) => release(varDecl, CFalseLiteral)
    case _                            => None
  }

  def deepCopy(using Quotes)(expr: CExpr, tpe: quotes.reflect.TypeRepr)(using fc: FragmentedCompiler)(using
      TranslationContext
  ): CExpr =
    fc.dispatchLifted[DataStructureIFFragment](_.compileDeepCopy)(tpe) match {
      case None    => expr
      case Some(f) => CCallExpr(f.ref, List(expr))
    }
}
