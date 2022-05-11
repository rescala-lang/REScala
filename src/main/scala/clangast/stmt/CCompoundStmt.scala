package clangast.stmt
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CCompoundStmt(body: List[CStmt]) extends CStmt {
  override def textgen: String =
    s"""
       |{
       |${body.map(_.textgen).mkString("\n").indent(2).stripTrailing()}
       |}
    """.strip().stripMargin

  override def toExpr(using Quotes): Expr[CCompoundStmt] = {
    val bodyExpr = Expr.ofList(body.map(_.toExpr))

    '{ CCompoundStmt($bodyExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CCompoundStmt =
    CCompoundStmt(body.map(mapper.mapCStmt))
}
