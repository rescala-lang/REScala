package clangast.decl

import clangast.toExpr
import clangast.expr.CExpr
import clangast.types.CQualType

import scala.quoted.{Expr, Quotes}

case class CVarDecl(name: String, declaredType: CQualType, init: Option[CExpr] = None) extends CValueDecl {
  override def getType: CQualType = declaredType

  override def textgen: String = {
    val decl = declaredType.typedVar(name)
    
    init match {
      case None => decl + ";"
      case Some(expr) => decl + s" = ${expr.textgen};"
    }
  }

  override def toExpr(using Quotes): Expr[CVarDecl] = {
    val nameExpr = Expr(name)
    val declaredTypeExpr = declaredType.toExpr
    val initExpr = init.map(_.toExpr).toExpr

    '{ CVarDecl($nameExpr, $declaredTypeExpr, $initExpr) }
  }
}
