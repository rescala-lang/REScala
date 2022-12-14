package clangast.decl

import clangast.traversal.CASTMapper
import clangast.types.CQualType

import scala.quoted.{Expr, Quotes}

case class CFieldDecl(name: String, declaredType: CQualType) extends CValueDecl {
  override def getType: CQualType = declaredType

  override def textgen: String = s"${declaredType.textgen} $name;"

  override def toExpr(using Quotes): Expr[CFieldDecl] = {
    val nameExpr         = Expr(name)
    val declaredTypeExpr = declaredType.toExpr

    '{ CFieldDecl($nameExpr, $declaredTypeExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CFieldDecl =
    CFieldDecl(name, mapper.mapCQualType(declaredType))
}
