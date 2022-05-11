package clangast.stmt
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

trait CSwitchCase extends CStmt {
  override def toExpr(using Quotes): Expr[CSwitchCase]

  override def mapChildren(mapper: CASTMapper): CSwitchCase = this
}
