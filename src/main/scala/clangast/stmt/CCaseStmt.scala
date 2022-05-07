package clangast.stmt

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CCaseStmt(cond: CExpr, body: List[CStmt]) extends CSwitchCase {
  override def textgen: String =
    s"""
       |case ${cond.textgen}:
       |${body.map(_.textgen).mkString("\n").indent(2).stripTrailing()}
    """.strip().stripMargin

  override def toExpr(using Quotes): Expr[CCaseStmt] = {
    val condExpr = cond.toExpr
    val bodyExpr = Expr.ofList(body.map(_.toExpr))

    '{ CCaseStmt($condExpr, $bodyExpr) }
  }
}
