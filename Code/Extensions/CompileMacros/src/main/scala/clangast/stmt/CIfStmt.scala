package clangast.stmt

import clangast.{CASTNode, toExpr}
import clangast.expr.{CExpr, given}
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CIfStmt(cond: CExpr, thenBranch: CStmt, elseBranch: Option[CStmt] = None) extends CStmt {
  override def textgen: String = {
    val thenPart = s"if (${cond.textgen}) ${thenBranch.textgen}"

    elseBranch match
      case None       => thenPart
      case Some(stmt) => thenPart + " else " + stmt.textgen
  }

  override def toExpr(using Quotes): Expr[CIfStmt] = {
    val condExpr       = cond.toExpr
    val thenBranchExpr = thenBranch.toExpr
    val elseBranchExpr = elseBranch.map(_.toExpr).toExpr

    '{ CIfStmt($condExpr, $thenBranchExpr, $elseBranchExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CIfStmt =
    CIfStmt(
      mapper.mapCExpr(cond),
      mapper.mapCStmt(thenBranch),
      elseBranch.map(mapper.mapCStmt)
    )
}
