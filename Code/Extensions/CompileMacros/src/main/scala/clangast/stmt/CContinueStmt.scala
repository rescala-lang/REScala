package clangast.stmt
import scala.quoted.{Expr, Quotes}

case object CContinueStmt extends CStmt {
  override def textgen: String = "continue;"

  override def toExpr(using Quotes): Expr[CContinueStmt.type] = '{ CContinueStmt }
}
