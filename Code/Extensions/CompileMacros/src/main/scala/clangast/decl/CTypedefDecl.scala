package clangast.decl

import clangast.traversal.CASTMapper
import clangast.types.{CQualType, CType, CTypedefType}

import scala.quoted.{Expr, Quotes}

case class CTypedefDecl(name: String, underlyingType: CQualType) extends CTypeDecl {
  override def getTypeForDecl: CType = CTypedefType(name)

  override def textgen: String = s"typedef ${underlyingType.textgen} $name;"

  override def toExpr(using Quotes): Expr[CTypedefDecl] = {
    val nameExpr           = Expr(name)
    val underlyingTypeExpr = underlyingType.toExpr

    '{ CTypedefDecl($nameExpr, $underlyingTypeExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CTypedefDecl =
    CTypedefDecl(name, mapper.mapCQualType(underlyingType))
}
