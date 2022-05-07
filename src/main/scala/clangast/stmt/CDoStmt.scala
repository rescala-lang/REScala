package clangast.stmt

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CDoStmt(cond: CExpr, body: List[CStmt]) extends CStmt {
  override def textgen: String =
    s"""
       |do {
       |${body.map(_.textgen).mkString("\n").indent(2).stripTrailing()}
       |} while(${cond.textgen})
    """.strip().stripMargin

  override def toExpr(using Quotes): Expr[CDoStmt] = {
    val condExpr = cond.toExpr
    val bodyExpr = Expr.ofList(body.map(_.toExpr))

    '{ CDoStmt($condExpr, $bodyExpr) }
  }
}
