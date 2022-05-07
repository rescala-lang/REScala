package clangast.stmt

import clangast.toExpr
import clangast.expr.CExpr

import scala.quoted.{Expr, Quotes}

case class CReturnStmt(retVal: Option[CExpr] = None) extends CStmt {
  override def textgen: String = retVal match
    case None => "return;"
    case Some(expr) => s"return ${expr.textgen};"

  override def toExpr(using Quotes): Expr[CReturnStmt] = {
    val retValExpr = retVal.map(_.toExpr).toExpr

    '{ CReturnStmt($retValExpr) }
  }
}
