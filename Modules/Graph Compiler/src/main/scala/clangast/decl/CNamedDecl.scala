package clangast.decl
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

trait CNamedDecl extends CDecl {
  val name: String

  override def toExpr(using Quotes): Expr[CNamedDecl]

  override def mapChildren(mapper: CASTMapper): CNamedDecl = this
}
