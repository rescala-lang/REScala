package rescala.macros

import rescala.macros.MacroAccess
import rescala.operator.Operators
import rescala.core.Core

import scala.quoted.*

//def eventMacro[T: Type](expr: Expr[Option[T]])(using q: Quotes): Expr[Event[T]] = MacroLego.makeEvent(expr)

def signalMacro[T: Type, Ops <: Operators: Type, Signal[_]](
    expr: Expr[T],
    api: Expr[Ops],
    creation: Expr[Core#CreationTicket]
)(using q: Quotes): Expr[Signal[T]] =
  MacroLego[Ops](null.asInstanceOf[Ops], api.asInstanceOf, creation)
    .makeSignal[T](expr).asInstanceOf

class MacroLego[Ops <: Operators: Type](
    val fakeApi: Ops,
    api: Expr[fakeApi.type],
    outerCreation: Expr[Core#CreationTicket]
)(using
    val quotes: Quotes
)(using
    Type[fakeApi.StaticTicket],
    Type[fakeApi.ReSource],
    Type[fakeApi.Signal],
    Type[fakeApi.Event],
    Type[fakeApi.ReadAs],
    Type[fakeApi.DynamicTicket]
) {

  import quotes.reflect.*

  class FindDefs() extends ExprMapHack {
    var foundDefinitions: List[Symbol] = Nil

    override def transform[T](e: Expr[T])(using Type[T])(using Quotes): Expr[T] = transformChildren(e)

    override def touchDefinition(using q2: Quotes)(definition: q2.reflect.Definition): Unit =
      foundDefinitions = definition.symbol.asInstanceOf[Symbol] :: foundDefinitions
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

    override def transform[T](e: Expr[T])(using Type[T])(using q: Quotes): Expr[T] = {
      import q.reflect.*
      e match {
        case '{ (${ x }: MacroAccess[_, _]).value } =>
          foundAbstractions ::= x
          e
        case '{ (${ x }: MacroAccess[_, _]).apply() } =>
          foundAbstractions ::= x
          e
        case _ => transformChildren(e)
      }

    }
  }

  class ReplaceInterp(replacement: Map[Expr[MacroAccess[_, _]], Term], ticket: Tree) extends ExprMap {

    override def transform[T](e: Expr[T])(using Type[T])(using Quotes): Expr[T] = {
      import quotes.reflect.*

      def replaceAccess(xy: Expr[MacroAccess[_, _]]) = {
        val wideType = TypeRepr.of[T].widen.asType
        replacement.get(xy) match
          case Some(replaced) =>
            val term = replaced.asExpr
            val res = Apply(
              TypeApply(Select.unique(ticket.asExpr.asTerm, "dependStatic"), List(Inferred(TypeRepr.of[T].widen))),
              List(term.asTerm)
              ).asExpr
            // '{ (${ ticket.asExprOf[fakeApi.StaticTicket] }.dependStatic[T](${ term })) }
            res.asInstanceOf[Expr[T]]
          case None =>
            val term = xy.asExprOf[fakeApi.ReadAs[T]]
            '{ (${ ticket.asExprOf[fakeApi.DynamicTicket] }.depend[T](${ term })) }
        end match
      }

      e match {
        case '{ (${ xy }: MacroAccess[_, _]).value } => replaceAccess(xy)
        case '{ (${ xy }: MacroAccess[_, _]).apply() } => replaceAccess(xy)
        case _ => transformChildren(e)
      }
    }
  }

  def makeSignal[T: Type](expr: Expr[T]): Expr[fakeApi.Signal[T]] = {
    val fi = FindInterp()
    fi.transform(expr)
    val definitions = FindDefs()
    definitions.transform(expr)
    val containsSymbol = ContainsSymbol(definitions.foundDefinitions)
    println(s"contains symbols: ${definitions.foundDefinitions}")
    val found    = fi.foundAbstractions.filterNot(containsSymbol.inTree)
    val isStatic = (found == fi.foundAbstractions)

    val funType = MethodType.apply(List("ticket"))(
      (_: MethodType) =>
        List(if isStatic then TypeRepr.of[fakeApi.StaticTicket] else TypeRepr.of[Core#DynamicTicket]),
      (_: MethodType) => TypeRepr.of[T]
    )

    val res = ValDef.let(Symbol.spliceOwner, found.map(_.asTerm)) { defs =>
      val replacementMap = found.zip(defs).toMap
      // val rdef = DefDef(exprSym, {params =>
      val rdef = Lambda(
        Symbol.spliceOwner,
        funType,
        { (sym, params) =>
          val staticTicket = params.head
          ReplaceInterp(replacementMap, staticTicket).transform(expr).asTerm.changeOwner(sym)
        }
      )
      if isStatic then
        val res = '{
          ${ api.asInstanceOf[Expr[fakeApi.type]] }.Signals.static[T](
            ${ Expr.ofSeq[Nothing](defs.toSeq.map(_.asExprOf[fakeApi.ReSource].asInstanceOf[Expr[Nothing]])) }: _*
          ) {
            ${ rdef.asExprOf[fakeApi.StaticTicket => T].asInstanceOf[Expr[Nothing]] }
          }(using ${ outerCreation.asInstanceOf[Expr[Nothing]] })
        }.asTerm


        val resOther = {
          Apply(
            Apply(
              Apply(
                TypeApply(
                  Select.unique(
                    Select.unique(api.asTerm, "Signals"),
                    "staticNoVarargs"
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
        }

        resOther
      else
        '{
          $api.Signals.dynamic[T](${
            Expr.ofSeq(defs.toSeq.map(_.asExprOf[Core#ReSource].asInstanceOf[Expr[Nothing]]))
          }: _*) {
            ${ rdef.asExprOf[Core#DynamicTicket => T] }
          }(using ${ outerCreation.asInstanceOf[Expr[Nothing]] })
        }.asTerm
    }.asExpr.asInstanceOf[Expr[fakeApi.Signal[T]]]
    println(s"res ${res.show}")
    res
  }

  def makeEvent[T: Type](expr: Expr[Option[T]]) = {
    val fi = FindInterp()
    fi.transform(expr)
    val definitions = FindDefs()
    definitions.transform(expr)
    val containsSymbol = ContainsSymbol(definitions.foundDefinitions)
    println(s"contains symbols: ${definitions.foundDefinitions}")
    val found    = fi.foundAbstractions.filterNot(containsSymbol.inTree)
    val isStatic = (found == fi.foundAbstractions)

    val funType = MethodType.apply(List("ticket"))(
      (_: MethodType) =>
        List(if isStatic then TypeRepr.of[fakeApi.StaticTicket] else TypeRepr.of[Core#DynamicTicket]),
      (_: MethodType) => TypeRepr.of[Option[T]]
    )

    val res = ValDef.let(Symbol.spliceOwner, found.map(_.asTerm)) { defs =>
      val replacementMap = found.zip(defs).toMap
      // val rdef = DefDef(exprSym, {params =>
      val rdef = Lambda(
        Symbol.spliceOwner,
        funType,
        { (sym, params) =>
          val staticTicket = params.head
          ReplaceInterp(replacementMap, staticTicket).transform(expr).asTerm.changeOwner(sym)
        }
      )
      if isStatic then
        '{
          $api.Events.static(
            ${ Expr.ofSeq(defs.toSeq.map(_.asExprOf[Core#ReSource].asInstanceOf[Expr[Nothing]])) }: _*
          ) {
            ${ rdef.asExprOf[fakeApi.StaticTicket => Option[T]].asInstanceOf[Expr[Nothing]] }
          }(using ${ outerCreation.asInstanceOf[Expr[Nothing]] })
        }.asTerm
      else
        '{
          $api.Events.dynamic(${
            Expr.ofSeq(defs.toSeq.map(_.asExprOf[Core#ReSource].asInstanceOf[Expr[Nothing]]))
          }: _*) {
            ${ rdef.asExprOf[Core#DynamicTicket => Option[T]] }
          }(using ${ outerCreation.asInstanceOf[Expr[Nothing]] })
        }.asTerm
    }.asExprOf[fakeApi.Event[T]]
    println(s"res ${res.show}")
    res
  }

}
