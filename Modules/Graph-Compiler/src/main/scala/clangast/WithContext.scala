package clangast

import clangast.decl.{CInclude, CTypeDecl, CValueDecl}
import compiler.context.TranslationContext

import scala.quoted.*

case class WithContext[T <: CASTNode](
    node: T,
    includes: List[CInclude],
    typeDecls: List[CTypeDecl],
    valueDecls: List[CValueDecl]
) {
  def toExpr(using Quotes, Type[T]): Expr[WithContext[T]] = {
    val nodeExpr       = node.toExpr.asInstanceOf[Expr[T]]
    val includesExpr   = Expr.ofList(includes.map(_.toExpr))
    val typeDeclsExpr  = Expr.ofList(typeDecls.map(_.toExpr))
    val valueDeclsExpr = Expr.ofList(valueDecls.map(_.toExpr))

    '{ WithContext($nodeExpr, $includesExpr, $typeDeclsExpr, $valueDeclsExpr) }
  }
}

object WithContext {
  def apply[T <: CASTNode](
      node: T,
      ctx: TranslationContext,
      excludeValue: PartialFunction[CValueDecl, Boolean] = _ => false
  ): WithContext[T] =
    WithContext(node, ctx.includesList, ctx.typeDeclList, ctx.valueDeclList.filterNot(excludeValue.orElse(_ => false)))
}
