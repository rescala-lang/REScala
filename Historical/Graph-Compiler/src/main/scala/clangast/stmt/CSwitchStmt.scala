package clangast.stmt

import clangast.expr.CExpr
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CSwitchStmt(cond: CExpr, cases: List[CSwitchCase]) extends CStmt {
  override def textgen: String =
    s"""
       |switch (${cond.textgen}) {
       |${cases.map(_.textgen).mkString("\n\n").linesIterator.map(l => s"  $l").mkString("\n").stripTrailing()}
       |}
    """.strip().stripMargin

  override def toExpr(using Quotes): Expr[CSwitchStmt] = {
    val condExpr  = cond.toExpr
    val casesExpr = Expr.ofList(cases.map(_.toExpr))

    '{ CSwitchStmt($condExpr, $casesExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CSwitchStmt =
    CSwitchStmt(mapper.mapCExpr(cond), cases.map(mapper.mapCSwitchCase))
}
