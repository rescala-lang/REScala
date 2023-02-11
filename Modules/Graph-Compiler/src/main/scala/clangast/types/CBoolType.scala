package clangast.types
import scala.quoted.{Expr, Quotes}

case object CBoolType extends CBuiltinType {
  override def textgen: String = "bool"

  override def toExpr(using Quotes): Expr[CBoolType.type] = '{ CBoolType }
}
