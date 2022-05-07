package clangast.decl

import clangast.toExpr
import clangast.expr.CExpr
import clangast.types.CQualType

import scala.quoted.{Expr, Quotes}

case class CEnumConstantDeclaration(name: String, initExpr: Option[CExpr]) extends CValueDecl {
  override def getType: CQualType = ???
  
  override def textgen: String = initExpr match {
    case None => name
    case Some(expr) => name + " = " + expr.textgen
  }

  override def toExpr(using Quotes): Expr[CEnumConstantDeclaration] = {
    val nameExpr = Expr(name)
    val initExprExpr = initExpr.map(_.toExpr).toExpr

    '{ CEnumConstantDeclaration($nameExpr, $initExprExpr) }
  }
}
