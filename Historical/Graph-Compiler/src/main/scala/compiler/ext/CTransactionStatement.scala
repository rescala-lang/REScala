package compiler.ext

import clangast.expr.CExpr
import clangast.stmt.CStmt
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CTransactionStatement(args: List[(String, CExpr)]) extends CStmt {
  override def textgen: String = ";"

  override def toExpr(using Quotes): Expr[CTransactionStatement] = {
    val argsExpr = Expr.ofList(args.map {
      case (str, expr) =>
        val strExpr  = Expr(str)
        val exprExpr = expr.toExpr
        Expr.ofTuple((strExpr, exprExpr))
    })

    '{ CTransactionStatement($argsExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CStmt =
    CTransactionStatement(args.map {
      case (sourceName, expr) => (sourceName, mapper.mapCExpr(expr))
    })
}
