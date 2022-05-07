package clangast.expr
import scala.quoted.{Expr, Quotes}

case class CCharacterLiteral(char: Char) extends CExpr {
  override def textgen: String = s"'$char'"

  override def toExpr(using Quotes): Expr[CCharacterLiteral] = {
    val charExpr = Expr(char)

    '{ CCharacterLiteral($charExpr) }
  }
}
