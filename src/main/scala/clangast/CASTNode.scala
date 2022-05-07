package clangast

import scala.quoted.*

trait CASTNode {
  def textgen: String = ""
  def toExpr(using Quotes): Expr[CASTNode]
}
