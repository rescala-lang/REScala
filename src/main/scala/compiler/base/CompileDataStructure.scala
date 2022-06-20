package compiler.base

import clangast.given
import clangast.decl.CFunctionDecl
import clangast.expr.{CCallExpr, CExpr}
import clangast.stmt.CStmt
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
}
