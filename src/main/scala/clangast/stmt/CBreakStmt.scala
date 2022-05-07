package clangast.stmt
import scala.quoted.{Expr, Quotes}

case object CBreakStmt extends CStmt {
  override def textgen: String = "break;"

  override def toExpr(using Quotes): Expr[CBreakStmt.type] = '{ CBreakStmt }
}
