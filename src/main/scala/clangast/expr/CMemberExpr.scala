package clangast.expr

import clangast.decl.CValueDecl
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CMemberExpr(base: CExpr, memberDecl: CValueDecl, isArrow: Boolean = false) extends CExpr {
  override def textgen: String =
    if isArrow then base.textgen + "->" + memberDecl.name
    else base.textgen + "." + memberDecl.name

  override def toExpr(using Quotes): Expr[CMemberExpr] = {
    val baseExpr = base.toExpr
    val memberDeclExpr = memberDecl.toExpr
    val isArrowExpr = Expr(isArrow)

    '{ CMemberExpr($baseExpr, $memberDeclExpr, $isArrowExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CMemberExpr =
    CMemberExpr(mapper.mapCExpr(base), mapper.mapCValueDecl(memberDecl), isArrow)
}
