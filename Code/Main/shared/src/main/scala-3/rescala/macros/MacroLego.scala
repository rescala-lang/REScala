package rescala.macros

import rescala.macros.MacroAccess
import rescala.operator.Operators
import rescala.core.Core

import scala.quoted.*

def reactiveMacro[T: Type, F[_]: Type, Ops <: Operators: Type, Reactive[_]](
    expr: Expr[F[T]],
    api: Expr[Ops],
    creation: Expr[Core#CreationTicket],
    reactiveType: Expr[String],
    forceStatic: Expr[Boolean]
)(using q: Quotes): Expr[Reactive[T]] =
  val rt = if reactiveType.valueOrAbort == "Signal" then ReactiveType.Signal else ReactiveType.Event
  MacroLego[Ops](null.asInstanceOf[Ops], api.asInstanceOf, creation, forceStatic.valueOrAbort)
    .makeReactive[F, T](expr, rt).asInstanceOf[Expr[Reactive[T]]]

enum ReactiveType:
  case Event, Signal

class MacroLego[Ops <: Operators: Type](
    val fakeApi: Ops,
    api: Expr[fakeApi.type],
    outerCreation: Expr[Core#CreationTicket],
    forceStatic: Boolean
)(using
    val quotes: Quotes
)(using
    Type[fakeApi.StaticTicket],
    Type[fakeApi.DynamicTicket],
    Type[fakeApi.ReSource],
) {

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
        val res = foldTree((Nil, false), x)(owner)
        // we do not find things with nested things inside
        if (res._1.nonEmpty) then (res._1, false)
        else (x :: acc._1, acc._2)

      if !tree.isExpr then foldOverTree(acc, tree)(owner)
      else
        tree.asExpr match
          case '{ (${ x }: MacroAccess[_, _]).value }   => handleFind(x.asTerm)
          case '{ (${ x }: MacroAccess[_, _]).apply() } => handleFind(x.asTerm)
          case _                                        => foldOverTree(acc, tree)(owner)

    }
  }

  class ReplaceInterp(replacement: Map[Term, Term], ticket: Tree) extends TreeMap {

    val matpe = TypeTree.of[MacroAccess[_, _]]

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

      val res = if (!tree.isExpr) then super.transformTerm(tree)(owner)
      else
        tree.asExpr match {
          case '{ (${ xy }: MacroAccess[α, β]).value }   => replaceAccess(TypeTree.of[α], xy.asTerm)
          case '{ (${ xy }: MacroAccess[α, β]).apply() } => replaceAccess(TypeTree.of[α], xy.asTerm)
          case _                                         => super.transformTerm(tree)(owner)
        }
      res
    }
  }

  class ReplaceImplicitTickets(ticket: Term) extends TreeMap {

    override def transformTerm(tree: quotes.reflect.Term)(owner: quotes.reflect.Symbol): quotes.reflect.Term = {
      tree match
        // case '{(${ss}: fakeApi.ScopeSearch.type).fromSchedulerImplicit(using ${_}: fakeApi.DynamicScope)} =>
        //  '{$ss.fromTicketImplicit($ticket)}.asExprOf[T]
        case Apply(Select(ss, "fromSchedulerImplicit"), _) =>
          Apply(Select.unique(ss, "fromTicketImplicit"), List(ticket))
        case other => super.transformTerm(tree)(owner)
    }
  }

  def makeReactive[F[_]: Type, T: Type](expr: Expr[F[T]], rtype: ReactiveType): Expr[Any] = {
    val fi = FindInterp().foldTree((Nil, true), expr.asTerm)(Symbol.spliceOwner)
    val foundAbstractions = fi._1
    val foundStatic = fi._2
    val definitions = FindDefs().foldTree(Nil, expr.asTerm)(Symbol.spliceOwner)

    // println(s"contains symbols: ${definitions}")
    val found = foundAbstractions.filterNot { fa =>
      val defInside      = FindDefs().foldTree(Nil, fa)(Symbol.spliceOwner)
      val containsSymbol = ContainsSymbol(definitions.diff(defInside))
      containsSymbol.foldTree(false, fa)(Symbol.spliceOwner)
    }
    val isStatic = (foundStatic && found == foundAbstractions)
    if (forceStatic && !isStatic)
      report.error("dynamic access in static reactive", foundAbstractions.diff(found).head.asExpr)

    val funType = MethodType.apply(List("ticket"))(
      (_: MethodType) =>
        List(if isStatic then TypeRepr.of[fakeApi.StaticTicket] else TypeRepr.of[fakeApi.DynamicTicket]),
      (_: MethodType) => TypeRepr.of[F[T]]
    )

    val res = ValDef.let(Symbol.spliceOwner, found) { defs =>
      val replacementMap = found.zip(defs).toMap
      // val rdef = DefDef(exprSym, {params =>
      val rdef = Lambda(
        Symbol.spliceOwner,
        funType,
        { (sym, params) =>
          val staticTicket = params.head
          val cutOut       = ReplaceInterp(replacementMap, staticTicket).transformTree(expr.asTerm)(sym).asExprOf[F[T]]
          val res =
            new ReplaceImplicitTickets(staticTicket.asInstanceOf[Term]).transformTerm(cutOut.asTerm)(
              Symbol.spliceOwner
            ).changeOwner(sym)
          res
        }
      )

      Apply(
        Apply(
          Apply(
            TypeApply(
              Select.unique(
                Select.unique(api.asTerm, if rtype == ReactiveType.Event then "Events" else "Signals"),
                if isStatic then "staticNoVarargs" else "dynamicNoVarargs"
              ),
              List(TypeTree.of[T])
            ),
            List(
              Repeated(defs, TypeTree.of[fakeApi.ReSource])
            )
          ),
          List(Block(
            Nil,
            Inlined(
              None,
              Nil,
              rdef
            )
          ))
        ),
        List(Inlined(None, Nil, outerCreation.asTerm))
      )
    }.asExpr
    // println(s"res ${res.show}")

    res
  }

}
