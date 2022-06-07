package clangast.expr
import clangast.traversal.CASTMapper

import scala.quoted.*

case class CUnlinkedCallExpr(callee: String, args: List[CExpr]) extends CExpr {
  override def textgen: String = s"$callee(${args.map(_.textgen).mkString(", ")})"

  override def toExpr(using Quotes): Expr[CUnlinkedCallExpr] = {
    val calleeExpr = Expr(callee)
    val argsExpr = Expr.ofList(args.map(_.toExpr))
    
    '{ CUnlinkedCallExpr($calleeExpr, $argsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CExpr =
    CUnlinkedCallExpr(callee, args.map(mapper.mapCExpr))
}
