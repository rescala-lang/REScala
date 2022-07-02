package clangast.stmt
import scala.quoted.{Expr, Quotes}

case object CEmptyStmt extends CStmt {
  override def textgen: String = ""

  override def toExpr(using Quotes): Expr[CEmptyStmt.type] = '{ CEmptyStmt }
}
