package macros

import clangast.given
import clangast.decl.CVarDecl
import clangast.expr.binaryop.*
import clangast.expr.*
import clangast.stmt.*
import clangast.traversal.CASTMapper
import macros.ScalaToC.*
import macros.CompileTerm.*
import macros.CompileType.*
import macros.CompileProduct.getProductRecordDecl

import scala.quoted.*

object CompileMatch {
  def compileMatchToCIfStmt(using Quotes)(matchTerm: quotes.reflect.Match, ctx: TranslationContext): CIfStmt = {
    import quotes.reflect.*

    val Match(scrutinee, cases) = matchTerm

    val (lastCond, lastDecls, lastStmtList) = compileCaseDef(cases.last, scrutinee, ctx)
    val lastIf = CIfStmt(
      lastCond.getOrElse(CTrueLiteral),
      CCompoundStmt(
        lastDecls.map(CDeclStmt(_)) ++ lastStmtList
      )
    )

    cases.init.foldRight(lastIf) {
      case (cd, nextIf) =>
        val (cond, decls, stmtList) = compileCaseDef(cd, scrutinee, ctx)

        CIfStmt(
          cond.getOrElse(CTrueLiteral),
          CCompoundStmt(
            decls.map(CDeclStmt(_)) ++ stmtList
          ),
          Some(nextIf)
        )
    }
  }

  def compileMatchToCStmtExpr(using Quotes)(matchTerm: quotes.reflect.Match, ctx: TranslationContext): CStmtExpr = {
    import quotes.reflect.*

    val Match(scrutinee, cases) = matchTerm

    val resDecl = CVarDecl("_res", compileTypeRepr(matchTerm.tpe, ctx), None)

    def convertLastToAssign(stmts: List[CStmt]): List[CStmt] = {
      stmts.last match {
        case CExprStmt(expr) =>
          val assign = CExprStmt(CAssignmentExpr(CDeclRefExpr(resDecl), expr))
          stmts.init.appended(assign)
      }
    }

    val (lastCond, lastDecls, lastStmtList) = compileCaseDef(cases.last, scrutinee, ctx)
    val lastIf = CIfStmt(
      lastCond.getOrElse(CTrueLiteral),
      CCompoundStmt(
        lastDecls.map(CDeclStmt(_)) ++ convertLastToAssign(lastStmtList)
      )
    )

    val outerIf = cases.init.foldRight(lastIf) {
      case (cd, nextIf) =>
        val (cond, decls, stmtList) = compileCaseDef(cd, scrutinee, ctx)

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

  def compileCaseDef(using Quotes)(caseDef: quotes.reflect.CaseDef, scrutinee: quotes.reflect.Term, ctx: TranslationContext): (Option[CExpr], List[CVarDecl], List[CStmt]) = {
    import quotes.reflect.*

    val CaseDef(pattern, guard, rhs) = caseDef

    val (patternCond, bindings) = compilePattern(pattern, compileTermToCExpr(scrutinee, ctx), scrutinee.tpe, ctx)

    bindings.foreach { decl => ctx.nameToDecl.put(decl.name, decl) }

    val stmtsList = compileTermToCStmt(rhs, ctx) match {
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

        val guardCompiled = replaceBoundIdentifiers.mapCExpr(compileTermToCExpr(guardExpr, ctx))

        val combinedCond = patternCond.fold(guardCompiled) { c => CAndExpr(c, guardCompiled) }

        (Some(combinedCond), bindings, stmtsList)
    }
  }

  def compilePattern(using Quotes)(pattern: quotes.reflect.Tree, prefix: CExpr, prefixType: quotes.reflect.TypeRepr, ctx: TranslationContext): (Option[CExpr], List[CVarDecl]) = {
    import quotes.reflect.*

    pattern match {
      case Wildcard() => (None, List())
      case term: Term =>
        (Some(CEqualsExpr(prefix, compileTermToCExpr(term, ctx))), Nil)
      case Bind(name, subPattern) =>
        val (subCond, subDecls) = compilePattern(subPattern, prefix, prefixType, ctx)

        (subCond, CVarDecl(name, compileTypeRepr(prefixType, ctx), Some(prefix)) :: subDecls)
      case Unapply(_, _, subPatterns) =>
        val fieldSymbols = prefixType.classSymbol.get.caseFields.filter(_.isValDef)
        val recordDecl = getProductRecordDecl(prefixType, ctx)
        val subPrefixes = fieldSymbols.map(fs => CMemberExpr(prefix, recordDecl.fields.find(f => fs.name.strip().equals(f.name)).get))
        val subPrefixTypes = fieldSymbols.map(prefixType.memberType)

        (subPatterns zip (subPrefixes zip subPrefixTypes)).foldLeft((Option.empty[CExpr], List.empty[CVarDecl])) {
          case ((cond, decls), (subPattern, (subPrefix, subPrefixType))) =>
            val (subCond, subDecls) = compilePattern(subPattern, subPrefix, subPrefixType, ctx)

            val combinedCond = (cond, subCond) match {
              case (None, _) => subCond
              case (_, None) => cond
              case (Some(c1), Some(c2)) => Some(CAndExpr(c1, c2))
            }

            (combinedCond, subDecls ++ decls)
        }
    }
  }
}
