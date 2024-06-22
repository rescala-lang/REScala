package clangast.expr
import scala.quoted.{Expr, Quotes}

case class CFloatLiteral(f: Float) extends CExpr {
  override def textgen: String = f.toString + "f"

  override def toExpr(using Quotes): Expr[CFloatLiteral] = {
    val fExpr = Expr(f)

    '{ CFloatLiteral($fExpr) }
  }
}
