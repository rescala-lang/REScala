package clangast.expr

import clangast.traversal.CASTMapper
import clangast.types.CType

import scala.quoted.*

case class CTypeArgExpr(tpe: CType) extends CExpr {
  override def textgen: String = tpe.textgen

  override def toExpr(using Quotes): Expr[CTypeArgExpr] = {
    val tpeExpr = tpe.toExpr

    '{ CTypeArgExpr($tpeExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CTypeArgExpr =
    CTypeArgExpr(mapper.mapCType(tpe))
}
