package clangast.expr
import scala.quoted.{Expr, Quotes}

case class CLongLiteral(v: Long) extends CExpr {
  override def textgen: String = v + "L"

  override def toExpr(using Quotes): Expr[CLongLiteral] = {
    val vExpr = Expr(v)

    '{ CLongLiteral($vExpr) }
  }
}
