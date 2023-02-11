package clangast.expr
import scala.quoted.{Expr, Quotes}

case class CDoubleLiteral(d: Double) extends CExpr {
  override def textgen: String = d.toString

  override def toExpr(using Quotes): Expr[CDoubleLiteral] = {
    val dExpr = Expr(d)

    '{ CDoubleLiteral($dExpr) }
  }
}
