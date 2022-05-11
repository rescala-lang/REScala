package clangast.types
import clangast.CASTNode
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CPointerType(pointeeType: CQualType) extends CType {
  override def textgen: String = pointeeType.textgen + "*"

  override def toExpr(using Quotes): Expr[CPointerType] = {
    val pointeeTypeExpr = pointeeType.toExpr

    '{ CPointerType($pointeeTypeExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CPointerType =
    CPointerType(mapper.mapCQualType(pointeeType))
}
