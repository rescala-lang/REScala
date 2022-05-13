package macros

import clangast.CASTNode
import macros.ScalaToC.*
import macros.CompileStatement.*
import macros.CompileType.*

import scala.quoted.*

object CompileTree {
  def compileTree(using Quotes)(tree: quotes.reflect.Tree, ctx: TranslationContext): CASTNode = {
    import quotes.reflect.*

    tree match {
      case statement: Statement => compileStatement(statement, ctx)
      case typeTree: TypeTree => compileTypeRepr(typeTree.tpe, ctx)
      case _ => throw new MatchError(tree.show(using Printer.TreeStructure))
    }
  }
}
