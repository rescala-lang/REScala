package compiler.base

import clangast.CASTNode
import compiler.context.TranslationContext
import compiler.FragmentedCompiler
import compiler.FragmentedCompiler.dispatch

import scala.quoted.*

object TreeFragment extends TreeIFFragment {
  override def compileTree(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Tree, CASTNode] = {
    import quotes.reflect.*

    {
      case statement: Statement => dispatch[StatementIFFragment](_.compileStatement)(statement)
      case typeTree: TypeTree   => dispatch[TypeIFFragment](_.compileTypeRepr)(typeTree.tpe)
    }
  }
}
