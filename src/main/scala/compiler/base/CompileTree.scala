package compiler.base

import clangast.CASTNode
import compiler.{CompilerCascade, PartialCompiler, TranslationContext}

import scala.quoted.*

object CompileTree extends PartialCompiler {
  override def compileTree(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Tree, CASTNode] = {
      import quotes.reflect.*

      {
        case statement: Statement => cascade.compileStatement(statement)
        case typeTree: TypeTree => cascade.compileTypeRepr(typeTree.tpe)
      }
    }
}
