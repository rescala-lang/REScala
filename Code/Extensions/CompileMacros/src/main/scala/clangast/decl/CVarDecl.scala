package clangast.decl

import clangast.toExpr
import clangast.expr.CExpr
import clangast.traversal.CASTMapper
import clangast.types.CQualType

import scala.quoted.{Expr, Quotes}

case class CVarDecl(name: String, declaredType: CQualType, init: Option[CExpr] = None, inHeader: Boolean = false)
    extends CValueDecl {
  override def getType: CQualType = declaredType

  override def textgen: String = {
    val decl = declaredType.typedVar(name)

    init match {
      case None =>
        if inHeader
        then s"extern $decl;"
        else s"$decl;"
      case Some(expr) => decl + s" = ${expr.textgen};"
    }
  }

  override def toExpr(using Quotes): Expr[CVarDecl] = {
    val nameExpr         = Expr(name)
    val declaredTypeExpr = declaredType.toExpr
    val initExpr         = init.map(_.toExpr).toExpr

    '{ CVarDecl($nameExpr, $declaredTypeExpr, $initExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CVarDecl =
    CVarDecl(name, mapper.mapCQualType(declaredType), init.map(mapper.mapCExpr))

  override def declOnly: CVarDecl = this.copy(init = None)
}
