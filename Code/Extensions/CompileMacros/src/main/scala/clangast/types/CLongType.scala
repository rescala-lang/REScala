package clangast.types
import scala.quoted.{Expr, Quotes}

case object CLongType extends CBuiltinType {
  override def textgen: String = "long"

  override def toExpr(using Quotes): Expr[CLongType.type] = '{ CLongType }
}
