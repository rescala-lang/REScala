package clangast.expr

import clangast.decl.CValueDecl
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CMemberExpr(base: CExpr, memberName: String, isArrow: Boolean = false) extends CExpr {
  override def textgen: String =
    if isArrow then base.textgen + "->" + memberName
    else base.textgen + "." + memberName

  override def toExpr(using Quotes): Expr[CMemberExpr] = {
    val baseExpr       = base.toExpr
    val memberNameExpr = Expr(memberName)
    val isArrowExpr    = Expr(isArrow)

    '{ CMemberExpr($baseExpr, $memberNameExpr, $isArrowExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CMemberExpr =
    CMemberExpr(mapper.mapCExpr(base), memberName, isArrow)
}
