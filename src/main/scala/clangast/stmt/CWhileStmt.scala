package clangast.stmt

import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CWhileStmt(cond: CExpr, body: CCompoundStmt) extends CStmt {
  override def textgen: String = s"while (${cond.textgen}) ${body.textgen}"

  override def toExpr(using Quotes): Expr[CWhileStmt] = {
    val condExpr = cond.toExpr
    val bodyExpr = body.toExpr

    '{ CWhileStmt($condExpr, $bodyExpr) }
  }
}
