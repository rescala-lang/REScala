package clangast.expr
import scala.quoted.{Expr, Quotes}

case class CInitListExpr(inits: List[CExpr]) extends CExpr {
  override def textgen: String = "{" + inits.map(_.textgen).mkString(", ") + "}"

  override def toExpr(using Quotes): Expr[CInitListExpr] = {
    val initsExpr = Expr.ofList(inits.map(_.toExpr))

    '{ CInitListExpr($initsExpr) }
  }
}
