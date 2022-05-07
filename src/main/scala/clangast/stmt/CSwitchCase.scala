package clangast.stmt
import scala.quoted.{Expr, Quotes}

trait CSwitchCase extends CStmt {
  override def toExpr(using Quotes): Expr[CSwitchCase]
}
