package clangast.stmt

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CDoStmt(cond: CExpr, body: List[CStmt]) extends CStmt {
  override def textgen: String =
    s"""
       |do {
       |${body.map(_.textgen).mkString("\n").linesIterator.map(l => s"  $l").mkString("\n").stripTrailing()}
       |} while(${cond.textgen})
    """.strip().stripMargin

  override def toExpr(using Quotes): Expr[CDoStmt] = {
    val condExpr = cond.toExpr
    val bodyExpr = Expr.ofList(body.map(_.toExpr))

    '{ CDoStmt($condExpr, $bodyExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CDoStmt =
    CDoStmt(mapper.mapCExpr(cond), body.map(mapper.mapCStmt))
}
