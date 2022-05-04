package clangast.stmt

import clangast.expr.CExpr

case class CReturnStmt(retVal: Option[CExpr] = None) extends CStmt {
  override def textgen: String = retVal match
    case None => "return;"
    case Some(expr) => s"return ${expr.textgen};"
}
