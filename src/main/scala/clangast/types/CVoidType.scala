package clangast.types
import scala.quoted.{Expr, Quotes}

case object CVoidType extends CType {
  override def textgen: String = "void"

  override def toExpr(using Quotes): Expr[CType] = '{ CVoidType }
}
