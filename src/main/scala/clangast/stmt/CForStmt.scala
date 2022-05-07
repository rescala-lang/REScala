package clangast.stmt

import clangast.toExpr
import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CForStmt(init: Option[CStmt], cond: Option[CExpr], inc: Option[CExpr], body: CStmt) extends CStmt {
  override def textgen: String =
    s"for (${init.fold("")(_.textgen)} ${cond.fold("")(_.textgen)}; ${inc.fold("")(_.textgen)}) ${body.textgen}"

  override def toExpr(using Quotes): Expr[CForStmt] = {
    val initExpr = init.map(_.toExpr).toExpr
    val condExpr = cond.map(_.toExpr).toExpr
    val incExpr = inc.map(_.toExpr).toExpr
    val bodyExpr = body.toExpr

    '{ CForStmt($initExpr, $condExpr, $incExpr, $bodyExpr) }
  }
}
