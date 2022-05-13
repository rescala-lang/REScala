package macros

import clangast.CASTNode
import clangast.stmt.{CDeclStmt, CStmt}
import macros.ScalaToC.*
import macros.CompileDefinition.*
import macros.CompileTerm.*

import scala.quoted.*

object CompileStatement {  
  def compileStatement(using Quotes)(statement: quotes.reflect.Statement, ctx: TranslationContext): CASTNode = {
    import quotes.reflect.*

    statement match {
      case definition: Definition => compileDefinition(definition, ctx)
      case term: Term => compileTerm(term, ctx)
      case _ => throw new MatchError(statement.show(using Printer.TreeStructure))
    }
  }

  def compileStatementToCStmt(using Quotes)(statement: quotes.reflect.Statement, ctx: TranslationContext): CStmt = {
    import quotes.reflect.*

    statement match {
      case definition: Definition => CDeclStmt(compileDefinition(definition, ctx))
      case term: Term => compileTermToCStmt(term, ctx)
      case _ => throw new MatchError(statement.show(using Printer.TreeStructure))
    }
  }
}
