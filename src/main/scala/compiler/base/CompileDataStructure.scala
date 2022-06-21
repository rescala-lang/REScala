package compiler.base

import clangast.given
import clangast.decl.{CFunctionDecl, CVarDecl}
import clangast.expr.{CCallExpr, CExpr, CFalseLiteral}
import clangast.stmt.{CDeclStmt, CStmt}
import clangast.types.{CQualType, CRecordType}
import compiler.{CompilerCascade, PartialCompiler}
import compiler.context.TranslationContext

import scala.quoted.*

object CompileDataStructure extends DataStructurePC {
  override def usesRefCount(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[quotes.reflect.TypeRepr, Boolean] = _ => false

  def retain(using Quotes)(expr: CExpr, tpe: quotes.reflect.TypeRepr)(using ctx: TranslationContext, cascade: CompilerCascade): CExpr =
    cascade.dispatchLifted(_.compileRetain)(tpe) match {
      case None => expr
      case Some(f) => CCallExpr(f.ref, List(expr))
    }

  def release(using Quotes)(expr: CExpr, tpe: quotes.reflect.TypeRepr, keepWithZero: CExpr)(using ctx: TranslationContext, cascade: CompilerCascade): Option[CStmt] =
    cascade.dispatchLifted(_.compileRelease)(tpe).map { f =>
      CCallExpr(f.ref, List(expr, keepWithZero))
    }

  def release(varDecl: CVarDecl, keepWithZero: CExpr)(using ctx: TranslationContext): Option[CStmt] =
    varDecl.declaredType match {
      case CQualType(CRecordType(declName), _) =>
        val funName = "release_" + declName
        ctx.valueDeclList.find {
          case CFunctionDecl(`funName`, _, _, _, _) => true
          case _ => false
        }.map(f => CCallExpr(f.ref, List(varDecl.ref, keepWithZero)))
      case _ => None
    }

  def releaseLocalVars(stmts: List[CStmt])(using TranslationContext): List[CStmt] = stmts.flatMap {
    case CDeclStmt(varDecl: CVarDecl) => release(varDecl, CFalseLiteral)
    case _ => None
  }
}
