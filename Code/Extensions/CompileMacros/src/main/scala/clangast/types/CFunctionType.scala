package clangast.types
import clangast.CASTNode
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CFunctionType(paramTypes: List[CQualType], returnType: CQualType) extends CType {
  override def toExpr(using Quotes): Expr[CFunctionType] = {
    val paramTypesExpr = Expr.ofList(paramTypes.map(_.toExpr))
    val returnTypeExpr = returnType.toExpr

    '{ CFunctionType($paramTypesExpr, $returnTypeExpr) }
  }

  override def typedVar(name: String): String = s"$returnType (*$name)(${paramTypes.mkString(", ")})"

  override def mapChildren(mapper: CASTMapper): CFunctionType =
    CFunctionType(paramTypes.map(mapper.mapCQualType), mapper.mapCQualType(returnType))
}
