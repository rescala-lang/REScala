package compiler.debug

import clangast.CASTNode
import compiler.FragmentedCompiler
import compiler.FragmentedCompiler.dispatch
import compiler.base.*
import compiler.context.TranslationContext

import scala.quoted.*

object DebugFragment extends TermIFFragment {
  override def compileTerm(using Quotes)(using FragmentedCompiler)(using
      TranslationContext
  ): PartialFunction[quotes.reflect.Term, CASTNode] = {
    import quotes.reflect.*

    {
      case Apply(TypeApply(Select(Ident("Debug"), "showAST"), _), List(inner)) =>
        println("AST at " + posOf(inner) + ":")
        println(inner.show(using Printer.TreeStructure))
        dispatch[TermIFFragment](_.compileTerm)(inner)
      case Apply(TypeApply(Select(Ident("Debug"), "showType"), _), List(inner)) =>
        println("Type at " + posOf(inner) + ":")
        println(inner.tpe.show(using Printer.TypeReprStructure))
        dispatch[TermIFFragment](_.compileTerm)(inner)
      case Apply(TypeApply(Select(Ident("Debug"), "showASTandType"), _), List(inner)) =>
        println("AST and Type at " + posOf(inner) + ":")
        println(inner.show(using Printer.TreeStructure))
        println(inner.tpe.show(using Printer.TypeReprStructure))
        dispatch[TermIFFragment](_.compileTerm)(inner)
      case Apply(TypeApply(Select(Ident("Debug"), "showCompiled"), _), List(inner)) =>
        val compiled = dispatch[TermIFFragment](_.compileTerm)(inner)
        println("Compiled at " + posOf(inner) + ":")
        println(compiled.textgen)
        compiled
    }
  }

  private def posOf(using Quotes)(inner: quotes.reflect.Term): String = {
    val pos = inner.pos
    s"${pos.sourceFile.name} (${pos.startLine + 1}:${pos.startColumn + 1} - ${pos.endLine + 1}:${pos.endColumn + 1})"
  }
}
