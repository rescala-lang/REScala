package clangast.types
import scala.quoted.{Expr, Quotes}

case object CDoubleType extends CBuiltinType {
  override def textgen: String = "double"

  override def toExpr(using Quotes): Expr[CDoubleType.type] = '{ CDoubleType }
}
