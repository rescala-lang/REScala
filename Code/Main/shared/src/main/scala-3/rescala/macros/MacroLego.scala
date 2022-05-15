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

  class ContainsSymbol(defs: List[Symbol]) extends ExprMap {
    private var result = false
    def takeResult() =
      val r = result
      result = false
      r
    override def transform[T](e: Expr[T])(using Type[T])(using q: Quotes): Expr[T] =
      if defs.contains(e.asTerm.symbol.asInstanceOf[Symbol]) then result = true
      transformChildren(e)

    def inTree[T](e: Expr[T])(using Type[T], Quotes): Boolean =
      transform(e)
      takeResult()
  }

  class FindInterp() extends ExprMap {

    var foundAbstractions: List[Expr[MacroAccess[_, _]]] = Nil
    var static                                           = true

    override def transform[T](e: Expr[T])(using Type[T])(using q: Quotes): Expr[T] = {
      import q.reflect.*

      def handleFind(x: Expr[MacroAccess[_, _]]) =
        val before = foundAbstractions
        transform(x)
        // we do not find things with nested things inside
        if before == foundAbstractions then
          foundAbstractions ::= x
        else
          static = false
          if (static) report.error("dynamic access in static reactive", foundAbstractions.head)
        e

      e match
        case '{ (${ x }: MacroAccess[_, _]).value }   => handleFind(x)
        case '{ (${ x }: MacroAccess[_, _]).apply() } => handleFind(x)
        case _                                        => transformChildren(e)

    }
  }

  class ReplaceInterp(replacement: Map[Expr[MacroAccess[_, _]], Term], ticket: Tree) extends ExprMap {

    override def transform[T](e: Expr[T])(using Type[T])(using Quotes): Expr[T] = {
      import quotes.reflect.*

      def accessTree[A: Type, B: Type](static: Boolean, accessed: Term) = Apply(
        TypeApply(
          Select.unique(ticket.asExpr.asTerm, if static then "dependStatic" else "depend"),
          List(TypeTree.of[A])
        ),
        List(accessed)
      ).asExpr

      def replaceAccess[A: Type, B: Type](xy: Expr[MacroAccess[A, B]]) = {
        replacement.get(xy) match
          case Some(replaced) => accessTree[A, B](true, replaced.asExpr.asTerm)
          case None =>
            val xye = transform(xy)
            accessTree[A, B](false, xye.asTerm)
        end match
      }

      val res = e match
        case '{ (${ xy }: MacroAccess[a, b]).value }   => replaceAccess[a, b](xy)
        case '{ (${ xy }: MacroAccess[a, b]).apply() } => replaceAccess[a, b](xy)
        case _                                         => transformChildren(e)
      res.asInstanceOf[Expr[T]]
    }
  }

  class ReplaceImplicitTickets(ticket: Term) extends ExprMap {

    override def transform[T](e: Expr[T])(using Type[T])(using Quotes): Expr[T] = {
      import quotes.reflect.*

      e.asTerm match
        // case '{(${ss}: fakeApi.ScopeSearch.type).fromSchedulerImplicit(using ${_}: fakeApi.DynamicScope)} =>
        //  '{$ss.fromTicketImplicit($ticket)}.asExprOf[T]
        case Apply(Select(ss, "fromSchedulerImplicit"), _) =>
          Apply(Select.unique(ss, "fromTicketImplicit"), List(ticket)).asExprOf[T]
        case other => transformChildren(e)
    }
  }

  def makeReactive[F[_]: Type, T: Type](expr: Expr[F[T]], rtype: ReactiveType): Expr[Any] = {
    val fi = FindInterp()
    fi.transform(expr)
    val definitions = FindDefs().foldTree(Nil, expr.asTerm)(Symbol.spliceOwner)

    // println(s"contains symbols: ${definitions}")
    val found = fi.foundAbstractions.filterNot { fa =>
      val defInside      = FindDefs().foldTree(Nil, fa.asTerm)(Symbol.spliceOwner)
      val containsSymbol = ContainsSymbol(definitions.diff(defInside))
      containsSymbol.inTree(fa)
    }
    val isStatic = (fi.static && found == fi.foundAbstractions)
    if (forceStatic && !isStatic)
      report.error("dynamic access in static reactive", fi.foundAbstractions.diff(found).head)

    val funType = MethodType.apply(List("ticket"))(
      (_: MethodType) =>
        List(if isStatic then TypeRepr.of[fakeApi.StaticTicket] else TypeRepr.of[fakeApi.DynamicTicket]),
      (_: MethodType) => TypeRepr.of[F[T]]
    )

    val res = ValDef.let(Symbol.spliceOwner, found.map(_.asTerm)) { defs =>
      val replacementMap = found.zip(defs).toMap
      // val rdef = DefDef(exprSym, {params =>
      val rdef = Lambda(
        Symbol.spliceOwner,
        funType,
        { (sym, params) =>
          val staticTicket = params.head
          val cutOut       = ReplaceInterp(replacementMap, staticTicket).transform(expr)
          val res =
            new ReplaceImplicitTickets(staticTicket.asInstanceOf[Term]).transform(cutOut).asTerm.changeOwner(sym)
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
