package compiler.base

import clangast.given
import clangast.decl.CVarDecl
import clangast.expr.binaryop.{CAndExpr, CAssignmentExpr, CEqualsExpr}
import clangast.expr.*
import clangast.stmt.*
import clangast.traversal.CASTMapper
import compiler.{CompilerCascade, PartialCompiler, TranslationContext}

import scala.quoted.*

object CompileMatch extends PartialCompiler {
  override def compileMatchToCIfStmt(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Match, CIfStmt] = {
      import quotes.reflect.*

      {
        case Match(scrutinee, cases) =>
          val (lastCond, lastDecls, lastStmtList) = cascade.compileCaseDef(cases.last, scrutinee)
          val lastIf = CIfStmt(
            lastCond.getOrElse(CTrueLiteral),
            CCompoundStmt(
              lastDecls.map(CDeclStmt(_)) ++ lastStmtList
            )
          )

          cases.init.foldRight(lastIf) {
            case (cd, nextIf) =>
              val (cond, decls, stmtList) = cascade.compileCaseDef(cd, scrutinee)

              CIfStmt(
                cond.getOrElse(CTrueLiteral),
                CCompoundStmt(
                  decls.map(CDeclStmt(_)) ++ stmtList
                ),
                Some(nextIf)
              )
          }
      }
    }

  override def compileMatchToCStmtExpr(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Match, CStmtExpr] = {
      import quotes.reflect.*

      {
        case matchTerm @ Match(scrutinee, cases) =>
          val resDecl = CVarDecl("_res", cascade.compileTypeRepr(matchTerm.tpe), None)

          def convertLastToAssign(stmts: List[CStmt]): List[CStmt] = {
            stmts.last match {
              case CExprStmt(expr) =>
                val assign = CExprStmt(CAssignmentExpr(CDeclRefExpr(resDecl), expr))
                stmts.init.appended(assign)
            }
          }

          val (lastCond, lastDecls, lastStmtList) = cascade.compileCaseDef(cases.last, scrutinee)
          val lastIf = CIfStmt(
            lastCond.getOrElse(CTrueLiteral),
            CCompoundStmt(
              lastDecls.map(CDeclStmt(_)) ++ convertLastToAssign(lastStmtList)
            )
          )

          val outerIf = cases.init.foldRight(lastIf) {
            case (cd, nextIf) =>
              val (cond, decls, stmtList) = cascade.compileCaseDef(cd, scrutinee)

              CIfStmt(
                cond.getOrElse(CTrueLiteral),
                CCompoundStmt(
                  decls.map(CDeclStmt(_)) ++ convertLastToAssign(stmtList)
                ),
                Some(nextIf)
              )
          }

          CStmtExpr(CCompoundStmt(List(
            resDecl,
            outerIf,
            CExprStmt(CDeclRefExpr(resDecl))
          )))
      }
    }

  override def compileCaseDef(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[(quotes.reflect.CaseDef, quotes.reflect.Term), (Option[CExpr], List[CVarDecl], List[CStmt])] = {
      import quotes.reflect.*

      {
        case (CaseDef(pattern, guard, rhs), scrutinee) =>
          val (patternCond, bindings) = cascade.compilePattern(pattern, cascade.compileTermToCExpr(scrutinee), scrutinee.tpe)

          bindings.foreach { decl => ctx.nameToDecl.put(decl.name, decl) }

          val stmtsList = cascade.compileTermToCStmt(rhs) match {
            case CNullStmt => List()
            case CCompoundStmt(stmts) => stmts
            case stmt => List(stmt)
          }

          guard match {
            case None => (patternCond, bindings, stmtsList)
            case Some(guardExpr) =>
              val replaceBoundIdentifiers = new CASTMapper {
                override protected val mapCExprHook: PartialFunction[CExpr, CExpr] = {
                  case CDeclRefExpr(decl: CVarDecl) if bindings.contains(decl) => decl.init.get
                }
              }

              val guardCompiled = replaceBoundIdentifiers.mapCExpr(cascade.compileTermToCExpr(guardExpr))

              val combinedCond = patternCond.fold(guardCompiled) { c => CAndExpr(c, guardCompiled) }

              (Some(combinedCond), bindings, stmtsList)
          }
      }
    }

  override def compilePattern(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[(quotes.reflect.Tree, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl])] = {
      import quotes.reflect.*

      {
        case (Wildcard(), _, _) => (None, List())
        case (term: Term, prefix, _) =>
          (Some(CEqualsExpr(prefix, cascade.compileTermToCExpr(term))), Nil)
        case (Bind(name, subPattern), prefix, prefixType) =>
          val (subCond, subDecls) = cascade.compilePattern(subPattern, prefix, prefixType)

          (subCond, CVarDecl(name, cascade.compileTypeRepr(prefixType), Some(prefix)) :: subDecls)
      }
    }
}
