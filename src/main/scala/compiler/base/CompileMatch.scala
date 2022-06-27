package compiler.base

import clangast.given
import clangast.decl.CVarDecl
import clangast.expr.binaryop.{CAndExpr, CAssignmentExpr, CEqualsExpr}
import clangast.expr.*
import clangast.stmt.*
import clangast.traversal.CASTMapper
import compiler.context.{TranslationContext, ValueDeclTC}
import compiler.CompilerCascade
import compiler.base.CompileDataStructure.{retain, release}

import scala.quoted.*

object CompileMatch extends MatchPC {
  override def compileMatchToCStmt(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Match, CStmt] = {
      import quotes.reflect.*

      {
        case Match(scrutinee, cases) =>
          val scrutineeName = ctx.uniqueValueName("_scrutinee")
          val scrutineeDecl = CVarDecl(
            scrutineeName,
            cascade.dispatch(_.compileTypeRepr)(scrutinee.tpe),
            Some(
              retain(
                cascade.dispatch(_.compileTermToCExpr)(scrutinee),
                scrutinee.tpe
              )
            )
          )

          val (lastCond, lastDecls, lastStmtList) = cascade.dispatch(_.compileCaseDef)(cases.last, scrutineeDecl.ref, scrutinee.tpe)
          val lastIf = CIfStmt(
            lastCond.getOrElse(CTrueLiteral),
            CCompoundStmt(
              lastDecls.map(CDeclStmt(_)) ++ lastStmtList
            )
          )

          val ifStmt = cases.init.foldRight(lastIf) {
            case (cd, nextIf) =>
              val (cond, decls, stmtList) = cascade.dispatch(_.compileCaseDef)(cd, scrutineeDecl.ref, scrutinee.tpe)

              CIfStmt(
                cond.getOrElse(CTrueLiteral),
                CCompoundStmt(
                  decls.map(CDeclStmt(_)) ++ stmtList
                ),
                Some(nextIf)
              )
          }

          CCompoundStmt(List[CStmt](
            scrutineeDecl,
            CEmptyStmt,
            ifStmt,
            CEmptyStmt
          ) ++ release(scrutineeDecl.ref, scrutinee.tpe, CFalseLiteral))
      }
    }

  override def compileMatchToCExpr(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[quotes.reflect.Match, CExpr] = {
      import quotes.reflect.*

      {
        case matchTerm @ Match(scrutinee, cases) =>
          val resName = ctx.uniqueValueName("_res")
          val resDecl = CVarDecl(resName, cascade.dispatch(_.compileTypeRepr)(matchTerm.tpe), None)
          
          val scrutineeName = ctx.uniqueValueName("_scrutinee")
          val scrutineeDecl = CVarDecl(
            scrutineeName,
            cascade.dispatch(_.compileTypeRepr)(scrutinee.tpe),
            Some(
              retain(
                cascade.dispatch(_.compileTermToCExpr)(scrutinee),
                scrutinee.tpe
              )
            )
          )

          def convertLastToAssign(stmts: List[CStmt]): List[CStmt] = {
            val valueIdx = stmts.lastIndexWhere {
              case CExprStmt(CCallExpr(CDeclRefExpr(funName), _)) if funName.startsWith("release_") => false
              case CExprStmt(_) => true
              case _ => false
            }

            val (body, releases) = stmts.splitAt(valueIdx + 1)

            body.last match {
              case CExprStmt(expr) =>
                val assign = CExprStmt(CAssignmentExpr(
                  resDecl.ref,
                  retain(expr, matchTerm.tpe)
                ))
                body.init.appended(assign) ++ releases
            }
          }

          val (lastCond, lastDecls, lastStmtList) = cascade.dispatch(_.compileCaseDef)(cases.last, scrutineeDecl.ref, scrutinee.tpe)

          val lastDeclStmts = lastDecls.map(CDeclStmt(_))
          val lastIf = CIfStmt(
            lastCond.getOrElse(CTrueLiteral),
            CCompoundStmt(
              (if lastDeclStmts.isEmpty then Nil else lastDeclStmts :+ CEmptyStmt) ++
                convertLastToAssign(lastStmtList)
            )
          )

          val outerIf = cases.init.foldRight(lastIf) {
            case (cd, nextIf) =>
              val (cond, decls, stmtList) = cascade.dispatch(_.compileCaseDef)(cd, scrutineeDecl.ref, scrutinee.tpe)

              val declStmts = decls.map(CDeclStmt(_))

              CIfStmt(
                cond.getOrElse(CTrueLiteral),
                CCompoundStmt(
                  (if declStmts.isEmpty then Nil else declStmts :+ CEmptyStmt) ++
                    convertLastToAssign(stmtList)
                ),
                Some(nextIf)
              )
          }

          CStmtExpr(CCompoundStmt(
            List[CStmt](
              resDecl,
              scrutineeDecl,
              CEmptyStmt,
              outerIf,
              CEmptyStmt
            ) ++
            release(scrutineeDecl.ref, scrutinee.tpe, CFalseLiteral) ++
            release(resDecl.ref, matchTerm.tpe, CTrueLiteral) ++
            List(CExprStmt(resDecl.ref))
          ))
      }
    }

  private def compileCaseDefImpl(using Quotes)(using ctx: ValueDeclTC, cascade: CompilerCascade):
    PartialFunction[(quotes.reflect.CaseDef, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl], List[CStmt])] = {
      import quotes.reflect.*

      {
        case (CaseDef(pattern, guard, rhs), scrutineeExpr, scrutineeType) =>
          val (patternCond, bindings) = cascade.dispatch(_.compilePattern)(pattern, scrutineeExpr, scrutineeType)

          bindings.foreach { decl => ctx.nameToDecl.put(decl.name, decl) }

          val stmtsList = cascade.dispatch(_.compileTermToCStmt)(rhs) match {
            case CNullStmt => List()
            case CCompoundStmt(stmts) => stmts
            case stmt => List(stmt)
          }

          guard match {
            case None => (patternCond, bindings, stmtsList)
            case Some(guardExpr) =>
              val replaceBoundIdentifiers = new CASTMapper {
                override protected val mapCExprHook: PartialFunction[CExpr, CExpr] = {
                  case CDeclRefExpr(declName) if bindings.exists(_.name.equals(declName)) =>
                    bindings.find(_.name.equals(declName)).get.init.get
                }
              }

              val guardCompiled = replaceBoundIdentifiers.mapCExpr(cascade.dispatch(_.compileTermToCExpr)(guardExpr))

              val combinedCond = patternCond.fold(guardCompiled) { c => CAndExpr(c, guardCompiled) }

              (Some(combinedCond), bindings, stmtsList)
          }
      }
    }

  override def compileCaseDef(using Quotes)(using TranslationContext, CompilerCascade):
    PartialFunction[(quotes.reflect.CaseDef, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl], List[CStmt])] =
      ensureCtx[ValueDeclTC](compileCaseDefImpl)

  override def compilePattern(using Quotes)(using ctx: TranslationContext, cascade: CompilerCascade):
    PartialFunction[(quotes.reflect.Tree, CExpr, quotes.reflect.TypeRepr), (Option[CExpr], List[CVarDecl])] = {
      import quotes.reflect.*

      {
        case (Wildcard(), _, _) => (None, List())
        case (term: Term, prefix, _) =>
          (Some(CEqualsExpr(prefix, cascade.dispatch(_.compileTermToCExpr)(term))), Nil)
        case (Bind(name, subPattern), prefix, prefixType) =>
          val (subCond, subDecls) = cascade.dispatch(_.compilePattern)(subPattern, prefix, prefixType)

          ctx.registerValueName(name)
          (subCond, CVarDecl(name, cascade.dispatch(_.compileTypeRepr)(prefixType), Some(prefix)) :: subDecls)
      }
    }
  
  def combineCond(left: Option[CExpr], right: Option[CExpr]): Option[CExpr] = (left, right) match {
    case (None, _) => right
    case (_, None) => left
    case (Some(c1), Some(c2)) => Some(CAndExpr(c1, c2))
  }
}
