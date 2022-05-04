package clangast.types

import clangast.CASTNode

trait CType extends CASTNode {
  def typedVar(name: String): String = s"$textgen $name"
}
