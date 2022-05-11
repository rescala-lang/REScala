package clangast.stmt

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

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

  override def mapChildren(mapper: CASTMapper): CCaseStmt =
    CCaseStmt(mapper.mapCExpr(cond), body.map(mapper.mapCStmt))
}
