package clangast.stmt

import clangast.expr.CExpr

case class CForStmt(init: Option[CStmt], cond: Option[CExpr], inc: Option[CExpr], body: CStmt) extends CStmt {
  override def textgen: String =
    s"for (${init.fold("")(_.textgen)} ${cond.fold("")(_.textgen)}; ${inc.fold("")(_.textgen)}) ${body.textgen}"
}
