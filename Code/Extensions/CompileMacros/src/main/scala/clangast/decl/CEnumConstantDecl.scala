package clangast.decl

import clangast.toExpr
import clangast.expr.CExpr
import clangast.traversal.CASTMapper
import clangast.types.CQualType

import scala.quoted.{Expr, Quotes}

case class CEnumConstantDecl(name: String, initExpr: Option[CExpr]) extends CValueDecl {
  override def getType: CQualType = ???

  override def textgen: String = initExpr match {
    case None       => name
    case Some(expr) => name + " = " + expr.textgen
  }

  override def toExpr(using Quotes): Expr[CEnumConstantDecl] = {
    val nameExpr     = Expr(name)
    val initExprExpr = initExpr.map(_.toExpr).toExpr

    '{ CEnumConstantDecl($nameExpr, $initExprExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CEnumConstantDecl =
    CEnumConstantDecl(name, initExpr.map(mapper.mapCExpr))
}
