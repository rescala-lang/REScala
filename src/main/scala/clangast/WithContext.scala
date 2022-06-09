package clangast

import clangast.decl.{CFunctionDecl, CInclude, CRecordDecl}
import compiler.context.{FunctionDeclTC, IncludeTC, RecordDeclTC, TranslationContext}

import scala.quoted.*

case class WithContext[T <: CASTNode](
                                       node: T,
                                       includes: List[CInclude],
                                       recordDecls: List[CRecordDecl],
                                       functionDecls: List[CFunctionDecl]
                                     ) {
  def toExpr(using Quotes, Type[T]): Expr[WithContext[T]] = {
    val nodeExpr = node.toExpr.asInstanceOf[Expr[T]]
    val includesExpr = Expr.ofList(includes.map(_.toExpr))
    val recordDeclsExpr = Expr.ofList(recordDecls.map(_.toExpr))
    val functionDeclsExpr = Expr.ofList(functionDecls.map(_.toExpr))

    '{ WithContext($nodeExpr, $includesExpr, $recordDeclsExpr, $functionDeclsExpr) }
  }
}

object WithContext {
  type RequiredTC = IncludeTC & RecordDeclTC & FunctionDeclTC
  def apply[T <: CASTNode](node: T, ctx: RequiredTC, excludeFunction: String = ""): WithContext[T] = {
    val includes = ctx.includes.toList
    val recordDecls = ctx.orderedRecordDecls.toList
    val functionDecls =
      ctx.nameToRecordCreator.values.toList ++
        ctx.nameToRecordEquals.values ++
        ctx.nameToFunctionDecl.values.filterNot(_.name.equals(excludeFunction))

    WithContext(node, includes, recordDecls, functionDecls)
  }
}
