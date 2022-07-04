package clangast.decl

import clangast.CASTNode

import scala.quoted.*

case class CInclude(name: String, isLocal: Boolean = false) extends CASTNode {
  override def textgen: String = "#include " + (if isLocal then "\"" + name + '"' else s"<$name>")

  override def toExpr(using Quotes): Expr[CInclude] = {
    val nameExpr    = Expr(name)
    val isLocalExpr = Expr(isLocal)

    '{ CInclude($nameExpr, $isLocalExpr) }
  }
}
