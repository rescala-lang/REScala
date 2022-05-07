package clangast.types
import scala.quoted.{Expr, Quotes}

case class CFunctionType(paramTypes: List[CQualType], returnType: CQualType) extends CType {
  override def toExpr(using Quotes): Expr[CFunctionType] = {
    val paramTypesExpr = Expr.ofList(paramTypes.map(_.toExpr))
    val returnTypeExpr = returnType.toExpr

    '{ CFunctionType($paramTypesExpr, $returnTypeExpr) }
  }
}
