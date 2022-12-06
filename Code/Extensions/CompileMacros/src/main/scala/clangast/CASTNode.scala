package clangast

import clangast.traversal.*

import scala.quoted.*

trait CASTNode {
  def textgen: String = ""
  def toExpr(using Quotes): Expr[CASTNode]

  def mapChildren(mapper: CASTMapper): CASTNode = this
}
