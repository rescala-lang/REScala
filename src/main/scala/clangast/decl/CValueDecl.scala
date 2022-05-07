package clangast.decl

import clangast.types.CQualType

import scala.quoted.{Expr, Quotes}

trait CValueDecl extends CNamedDecl {
  def getType: CQualType

  override def toExpr(using Quotes): Expr[CValueDecl]
}
