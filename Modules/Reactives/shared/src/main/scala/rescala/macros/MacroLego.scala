package rescala.macros

import rescala.core.ScopeSearch
import rescala.macros.MacroAccess
import rescala.operator.Operators

import scala.quoted.*

inline def getDependencies[Res, ReSource, Ticket, ForceStatic <: Boolean](inline expr: Res)
    : (List[ReSource], Ticket => Res, Boolean) =
  ${ rescala.macros.reactiveMacro[Res, ReSource, Ticket, ForceStatic]('{ expr }) }

def reactiveMacro[Res: Type, ReSource: Type, Ticket: Type, ForceStatic <: Boolean: Type](
    expr: Expr[Res]
)(using q: Quotes): Expr[(List[ReSource], Ticket => Res, Boolean)] =
  import q.reflect.*
  val forceStatic =
    Type.valueOfConstant[ForceStatic].getOrElse(report.errorAndAbort("requires literal type for force static"))
  MacroLego[ReSource, Ticket](forceStatic)
    .makeReactive[Res](expr).asInstanceOf[Expr[(List[ReSource], Ticket => Res, Boolean)]]

class MacroLego[ReSource: Type, Ticket: Type](
    forceStatic: Boolean
)(using val quotes: Quotes) {

  import quotes.reflect.*

  class FindDefs extends TreeAccumulator[List[Symbol]] {
    override def foldTree(acc: List[Symbol], tree: Tree)(owner: Symbol): List[Symbol] =
      val accd = tree match {
        case d: Definition => d.symbol :: acc
        case b: Bind       => b.symbol :: acc
        case other         => acc
      }
      foldOverTree(accd, tree)(owner)
  }

  class ContainsSymbol(defs: List[quotes.reflect.Symbol]) extends TreeAccumulator[Boolean] {
    import quotes.reflect.*

    override def foldTree(x: Boolean, tree: Tree)(owner: Symbol): Boolean =
      if defs.contains(tree.symbol) then true
      else foldOverTree(x, tree)(owner)
  }

  class FindInterp() extends TreeAccumulator[(List[Term], Boolean)] {

    override def foldTree(
        acc: (List[quotes.reflect.Term], Boolean),
        tree: quotes.reflect.Tree
    )(owner: quotes.reflect.Symbol): (List[quotes.reflect.Term], Boolean) = {

      def handleFind(x: Term): (List[Term], Boolean) =
        val before = acc._1
        val res    = foldTree((Nil, true), x)(owner)
        // we do not find things with nested things inside
        if (res._1.nonEmpty) then (before, false)
        else (x :: before, acc._2)

      if !tree.isExpr then foldOverTree(acc, tree)(owner)
      else
        tree.asExpr match
          case '{ (${ x }: MacroAccess[_]).value } => handleFind(x.asTerm)
          case _                                   => foldOverTree(acc, tree)(owner)

    }
  }

  class ReplaceInterp(replacement: Map[Term, Term], ticket: Tree) extends TreeMap {

    override def transformTerm(tree: quotes.reflect.Term)(owner: quotes.reflect.Symbol): quotes.reflect.Term = {
      def accessTree(a: TypeTree)(static: Boolean, accessed: Term): Term = Apply(
        TypeApply(
          Select.unique(ticket.asExpr.asTerm, if static then "dependStatic" else "depend"),
          List(a)
        ),
        List(accessed)
      )

      def replaceAccess(a: TypeTree, xy: Term): Term = {
        replacement.get(xy) match
          case Some(replaced) => accessTree(a)(true, replaced.asExpr.asTerm)
          case None =>
            val xye = transformTree(xy)(owner).asInstanceOf[Term]
            accessTree(a)(false, xye)
        end match
      }

      val res = if !tree.isExpr then super.transformTerm(tree)(owner)
      else
        tree.asExpr match {
          case '{ (${ xy }: MacroAccess[α]).value } => replaceAccess(TypeTree.of[α], xy.asTerm)
          case _                                    => super.transformTerm(tree)(owner)
        }
      res
    }
  }

  class ReplaceImplicitTickets(ticket: Term) extends TreeMap {

    override def transformTerm(tree: quotes.reflect.Term)(owner: quotes.reflect.Symbol): quotes.reflect.Term = {
      tree match
        // the first case is from a prior encoding of modules in REScala, we keep it just in case …
        case Apply(Select(ss, "fromSchedulerImplicit"), _) =>
          Apply(Select.unique(ss, "fromTicketImplicit"), List(ticket))
        case Apply(TypeApply(Ident("fromSchedulerImplicit"), ta), _) =>
          Apply(TypeApply(Ident(TermRef(TypeRepr.of[ScopeSearch.type], "fromTicketImplicit")), ta), List(ticket))
        case other => super.transformTerm(tree)(owner)
    }
  }

  def makeReactive[Res: Type](expr: Expr[Res]): Expr[Any] = {
    val fi                = FindInterp().foldTree((Nil, true), expr.asTerm)(Symbol.spliceOwner)
    val foundAbstractions = fi._1
    val foundStatic       = fi._2
    val definitions       = FindDefs().foldTree(Nil, expr.asTerm)(Symbol.spliceOwner)

    val found = foundAbstractions.filterNot { fa =>
      val defInside      = FindDefs().foldTree(Nil, fa)(Symbol.spliceOwner)
      val containsSymbol = ContainsSymbol(definitions.diff(defInside))
      containsSymbol.foldTree(false, fa)(Symbol.spliceOwner)
    }
    val isStatic = (foundStatic && found == foundAbstractions)
    if (forceStatic && !isStatic) {
      report.error(
        "dynamic access in static reactive",
        foundAbstractions.diff(found).headOption.map(_.asExpr).getOrElse(expr)
      )
    }

    val funType = MethodType.apply(List("ticket"))(
      (_: MethodType) => List(TypeRepr.of[Ticket]),
      (_: MethodType) => TypeRepr.of[Res]
    )

    val res = ValDef.let(Symbol.spliceOwner, found) { defs =>
      val replacementMap = found.zip(defs).toMap
      // val rdef = DefDef(exprSym, {params =>
      val rdef = Lambda(
        Symbol.spliceOwner,
        funType,
        { (sym, params) =>
          val staticTicket = params.head
          val cutOut       = ReplaceInterp(replacementMap, staticTicket).transformTree(expr.asTerm)(sym).asExprOf[Res]
          val res =
            new ReplaceImplicitTickets(staticTicket.asInstanceOf[Term]).transformTerm(cutOut.asTerm)(
              Symbol.spliceOwner
            ).changeOwner(sym)
          res
        }
      )

      '{
        (
          List.from(${ Expr.ofList(defs.map(_.asExprOf[ReSource])) }),
          ${ rdef.asExprOf[Ticket => Res] },
          ${ Expr(isStatic) }
        )
      }.asTerm
    }.asExpr

    res
  }

}
