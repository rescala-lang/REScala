package clangast.types
import scala.quoted.{Expr, Quotes}

case object CVoidType extends CBuiltinType {
  override def textgen: String = "void"

  override def toExpr(using Quotes): Expr[CVoidType.type] = '{ CVoidType }
}
