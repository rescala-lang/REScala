package clangast.stmt

import clangast.decl.CDecl
import clangast.traversal.CASTMapper

import scala.quoted.{Expr, Quotes}

case class CDeclStmt(decl: CDecl) extends CStmt {
  override def textgen: String = decl.textgen

  override def toExpr(using Quotes): Expr[CDeclStmt] = {
    val declExpr = decl.toExpr

    '{ CDeclStmt($declExpr) }
  }

  override def mapChildren(mapper: CASTMapper): CDeclStmt =
    CDeclStmt(mapper.mapCDecl(decl))
}
