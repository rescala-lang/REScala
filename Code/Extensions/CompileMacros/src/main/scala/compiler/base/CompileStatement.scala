package compiler.base

import clangast.CASTNode
import clangast.stmt.{CDeclStmt, CStmt}
import compiler.context.TranslationContext
import compiler.CompilerCascade

import scala.quoted.*

object CompileStatement extends StatementPC {
  override def compileStatement(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Statement, CASTNode] = {
      import quotes.reflect.*
    
      {
        case definition: Definition => cascade.dispatch(_.compileDefinition)(definition)
        case term: Term => cascade.dispatch(_.compileTerm)(term)
      }
    }

  override def compileStatementToCStmt(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Statement, CStmt] = {
      import quotes.reflect.*
    
      {
        case definition: Definition => CDeclStmt(cascade.dispatch(_.compileDefinition)(definition))
        case term: Term => cascade.dispatch(_.compileTermToCStmt)(term)
      }
    }
}
