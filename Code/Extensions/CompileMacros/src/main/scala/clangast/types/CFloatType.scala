package clangast.types
import scala.quoted.{Expr, Quotes}

case object CFloatType extends CBuiltinType {
  override def textgen: String = "float"

  override def toExpr(using Quotes): Expr[CFloatType.type] = '{ CFloatType }
}
