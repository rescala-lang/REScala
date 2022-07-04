package clangast.expr
import scala.quoted.{Expr, Quotes}

case class CStringLiteral(s: String) extends CExpr {
  override def textgen: String = "\"" + s + "\""

  override def toExpr(using Quotes): Expr[CStringLiteral] = {
    val sExpr = Expr(s)

    '{ CStringLiteral($sExpr) }
  }
}
