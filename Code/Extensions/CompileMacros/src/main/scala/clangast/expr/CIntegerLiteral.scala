package clangast.expr
import scala.quoted.{Expr, Quotes}

case class CIntegerLiteral(v: Int) extends CExpr {
  override def textgen: String = v.toString

  override def toExpr(using Quotes): Expr[CIntegerLiteral] = {
    val vExpr = Expr(v)

    '{ CIntegerLiteral($vExpr) }
  }
}
