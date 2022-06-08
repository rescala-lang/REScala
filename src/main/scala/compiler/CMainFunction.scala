package compiler

import clangast.*
import clangast.given
import clangast.WithContext
import clangast.decl.CFunctionDecl
import clangast.stmt.{CCompoundStmt, CReturnStmt}
import clangast.types.CIntegerType

class CMainFunction(val f: WithContext[CFunctionDecl])

object CMainFunction {
  inline def apply[C <: MacroCompiler](inline block: Any)(using mc: C, hfc: HelperFunCollection): CMainFunction = {
    mc.compileTree(block) match {
      case WithContext(CCompoundStmt(stmts), includes, recordDecls, functionDecls) =>
        val body = CCompoundStmt(stmts.appended(CReturnStmt(Some(0.lit))))

        val mainDecl = CFunctionDecl("main", List(), CIntegerType, Some(body))

        new CMainFunction(WithContext(mainDecl, includes, recordDecls, functionDecls))
      case WithContext(cast, _, _, _) => throw new MatchError(cast)
    }
  }
  
  def empty: CMainFunction = {
    new CMainFunction(
      WithContext(
        CFunctionDecl("main", List(), CIntegerType, Some(CCompoundStmt(List(CReturnStmt(Some(0.lit)))))),
        List(),
        List(),
        List()
      )
    )
  }

  def startTransaction(assignments: Any*): Unit = ???
}
