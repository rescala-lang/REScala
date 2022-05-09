package clangast.expr
import scala.quoted.{Expr, Quotes}

case class CCallExpr(callee: CExpr, args: List[CExpr]) extends CExpr {
  override def textgen: String = s"${callee.textgen}(${args.map(_.textgen).mkString(", ")})"

  override def toExpr(using Quotes): Expr[CCallExpr] = {
    val calleeExpr = callee.toExpr
    val argsExpr = Expr.ofList(args.map(_.toExpr))

    '{ CCallExpr($calleeExpr, $argsExpr) }
  }
}
