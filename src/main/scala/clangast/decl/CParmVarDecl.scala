package clangast.decl

import clangast.types.CQualType

import scala.quoted.{Expr, Quotes}

case class CParmVarDecl(name: String, declaredType: CQualType) extends CValueDecl {
  override def getType: CQualType = declaredType

  override def textgen: String = s"${declaredType.textgen} $name"

  override def toExpr(using Quotes): Expr[CParmVarDecl] = {
    val nameExpr = Expr(name)
    val declaredTypeExpr = declaredType.toExpr

    '{ CParmVarDecl($nameExpr, $declaredTypeExpr) }
  }
}
