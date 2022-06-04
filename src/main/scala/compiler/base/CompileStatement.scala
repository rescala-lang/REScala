package compiler.base

import clangast.CASTNode
import clangast.stmt.{CDeclStmt, CStmt}
import compiler.{CompilerCascade, PartialCompiler, TranslationContext}

import scala.quoted.*

object CompileStatement extends PartialCompiler {
  override def compileStatement(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Statement, CASTNode] = {
      import quotes.reflect.*
    
      {
        case definition: Definition => cascade.compileDefinition(definition)
        case term: Term => cascade.compileTerm(term)
      }
    }

  override def compileStatementToCStmt(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Statement, CStmt] = {
      import quotes.reflect.*
    
      {
        case definition: Definition => CDeclStmt(cascade.compileDefinition(definition))
        case term: Term => cascade.compileTermToCStmt(term)
      }
    }
}
