package clangast.types

import clangast.CASTNode
import clangast.traversal.CASTMapper

import scala.quoted.*

case class CQualType(unqualType: CType, isConst: Boolean = false) extends CASTNode {
  override def textgen: String =
    (if isConst then "const " else "") + unqualType.textgen

  def typedVar(name: String): String =
    (if isConst then "const " else "") + unqualType.typedVar(name)

  override def toExpr(using Quotes): Expr[CQualType] = {
    val unqualTypeExpr = unqualType.toExpr
    val isConstExpr    = Expr(isConst)

    '{ CQualType($unqualTypeExpr, $isConstExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CQualType =
    CQualType(mapper.mapCType(unqualType), isConst)
}
