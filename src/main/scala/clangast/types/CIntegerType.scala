package clangast.types
import scala.quoted.{Expr, Quotes}

case object CIntegerType extends CBuiltinType {
  override def textgen: String = "int"

  override def toExpr(using Quotes): Expr[CIntegerType.type] = '{ CIntegerType }
}
