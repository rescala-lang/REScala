package clangast.decl

import clangast.types.CType

import scala.quoted.{Expr, Quotes}

trait CTypeDecl extends CNamedDecl {
  def getTypeForDecl: CType

  override def toExpr(using Quotes): Expr[CTypeDecl]
}
