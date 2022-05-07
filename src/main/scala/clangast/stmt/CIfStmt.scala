package clangast.stmt

import clangast.toExpr
import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CIfStmt(cond: CExpr, thenBranch: CStmt, elseBranch: Option[CStmt] = None) extends CStmt {
  override def textgen: String = {
    val thenPart = s"if (${cond.textgen}) ${thenBranch.textgen}"

    elseBranch match
      case None => thenPart
      case Some(stmt) => thenPart + " else " + stmt.textgen
  }

  override def toExpr(using Quotes): Expr[CIfStmt] = {
    val condExpr = cond.toExpr
    val thenBranchExpr = thenBranch.toExpr
    val elseBranchExpr = elseBranch.map(_.toExpr).toExpr

    '{ CIfStmt($condExpr, $thenBranchExpr, $elseBranchExpr) }
  }
}
