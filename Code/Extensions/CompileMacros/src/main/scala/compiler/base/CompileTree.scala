package compiler.base

import clangast.CASTNode
import compiler.context.TranslationContext
import compiler.CompilerCascade

import scala.quoted.*

object CompileTree extends TreePC {
  override def compileTree(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Tree, CASTNode] = {
      import quotes.reflect.*

      {
        case statement: Statement => cascade.dispatch(_.compileStatement)(statement)
        case typeTree: TypeTree => cascade.dispatch(_.compileTypeRepr)(typeTree.tpe)
      }
    }
}
