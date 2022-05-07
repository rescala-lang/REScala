package clangast.stmt
import scala.quoted.{Expr, Quotes}

case object CNullStmt extends CStmt {
  override def textgen: String = ";"

  override def toExpr(using Quotes): Expr[CNullStmt.type] = '{ CNullStmt }
}
