package clangast.decl

import clangast.traversal.CASTMapper
import clangast.types.CQualType

import scala.quoted.{Expr, Quotes}

case class CParmVarDecl(name: String, declaredType: CQualType) extends CValueDecl {
  override def getType: CQualType = declaredType

  override def textgen: String = declaredType.typedVar(name)

  override def toExpr(using Quotes): Expr[CParmVarDecl] = {
    val nameExpr         = Expr(name)
    val declaredTypeExpr = declaredType.toExpr

    '{ CParmVarDecl($nameExpr, $declaredTypeExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CParmVarDecl =
    CParmVarDecl(name, mapper.mapCQualType(declaredType))
}
