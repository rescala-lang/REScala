package clangast.types
import scala.quoted.{Expr, Quotes}

case object CCharType extends CBuiltinType {
  override def textgen: String = "char"

  override def toExpr(using Quotes): Expr[CCharType.type] = '{ CCharType }
}
