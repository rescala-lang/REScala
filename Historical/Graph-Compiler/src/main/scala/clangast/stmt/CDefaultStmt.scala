package clangast.stmt
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CDefaultStmt(body: List[CStmt]) extends CSwitchCase {
  override def textgen: String =
    s"""
       |default:
       |${body.map(_.textgen).mkString("\n").linesIterator.map(l => s"  $l").mkString("\n").stripTrailing()}
    """.strip().stripMargin

  override def toExpr(using Quotes): Expr[CDefaultStmt] = {
    val bodyExpr = Expr.ofList(body.map(_.toExpr))

    '{ CDefaultStmt($bodyExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CDefaultStmt =
    CDefaultStmt(body.map(mapper.mapCStmt))
}
