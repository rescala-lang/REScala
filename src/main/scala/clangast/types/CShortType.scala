package clangast.types
import scala.quoted.{Expr, Quotes}

case object CShortType extends CBuiltinType {
  override def textgen: String = "short"

  override def toExpr(using Quotes): Expr[CShortType.type] = '{ CShortType }
}
