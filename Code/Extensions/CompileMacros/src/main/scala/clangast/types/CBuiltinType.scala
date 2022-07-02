package clangast.types
import scala.quoted.{Expr, Quotes}

trait CBuiltinType extends CType {
  override def toExpr(using Quotes): Expr[CBuiltinType]
}
