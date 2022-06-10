package clangast.decl

import clangast.traversal.CASTMapper
import clangast.types.CQualType

import scala.quoted.{Expr, Quotes}

trait CValueDecl extends CNamedDecl {
  def getType: CQualType
  
  def declOnly: CValueDecl = this

  override def toExpr(using Quotes): Expr[CValueDecl]

  override def mapChildren(mapper: CASTMapper): CValueDecl = this
}
