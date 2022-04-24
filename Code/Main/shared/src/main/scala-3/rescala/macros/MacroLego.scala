package rescala.macros

import rescala.macros.MacroAccess
import rescala.operator.Operators

import scala.quoted.*


//def eventMacro[T: Type](expr: Expr[Option[T]])(using q: Quotes): Expr[Event[T]] = MacroLego.makeEvent(expr)

def signalMacro[T: Type, Ops <: Operators: Type, StaticTicket <: Operators#StaticTicket, Signal[_]](
    expr: Expr[T],
    api: Expr[Ops],
    creation: Expr[Operators#CreationTicket]
)(using q: Quotes): Expr[Signal[T]] =
  MacroLego[Ops, StaticTicket](api, creation).makeSignal[T](expr).asInstanceOf[Signal[T]].asInstanceOf[Expr[Signal[T]]]

class MacroLego[Ops <: Operators: Type, StaticTicket <: Operators#StaticTicket](api: Expr[Ops], outerCreation: Expr[Operators#CreationTicket])(using
    val quotes: Quotes
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
        case _ => transformChildren(e)
      }

    }
  }


  class ReplaceInterp(replacement: Map[Expr[MacroAccess[_, _]], Term], ticket: Tree) extends ExprMap {

    override def transform[T](e: Expr[T])(using Type[T])(using Quotes): Expr[T] = {
      import quotes.reflect.*
      e match {
        case '{ (${ xy }: MacroAccess[vt, interp]).value } =>
          val wideType = TypeRepr.of[T].widen.asType
          replacement.get(xy) match
            case Some(replaced) =>
              println(replaced)
              val term = replaced.asExprOf[Operators#ReadAs[T]]
              '{ (${ ticket.asExprOf[StaticTicket] }.dependStatic[T](${ term.asInstanceOf[Expr[Nothing]] })) }
            case None =>
              val term = xy.asExprOf[Operators#ReadAs[T]]
              '{ (${ ticket.asExprOf[Operators#DynamicTicket] }.depend[T](${ term.asInstanceOf[Expr[Nothing]] })) }
          end match
        case _ => transformChildren(e)
      }
    }
  }

  def makeSignal[T: Type](expr: Expr[T]): Expr[Operators#Signal[T]] = {
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
        List(if isStatic then TypeRepr.of[StaticTicket] else TypeRepr.of[Operators#DynamicTicket]),
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
        '{
          $api.Signals.static(
            ${ Expr.ofSeq(defs.toSeq.map(_.asExprOf[Operators#ReSource].asInstanceOf[Expr[Nothing]])) }: _*
          ) {
            ${ rdef.asExprOf[StaticTicket => T] }
          }(using ${ outerCreation.asInstanceOf[Expr[Nothing]] })
        }.asTerm
      else
        '{
          $api.Signals.dynamic(${
            Expr.ofSeq(defs.toSeq.map(_.asExprOf[Operators#ReSource].asInstanceOf[Expr[Nothing]]))
          }: _*) {
            ${ rdef.asExprOf[Operators#DynamicTicket => T] }
          }(using ${ outerCreation.asInstanceOf[Expr[Nothing]] })
        }.asTerm
    }.asExprOf[Operators#Signal[T]]
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
        List(if isStatic then TypeRepr.of[StaticTicket] else TypeRepr.of[Operators#DynamicTicket]),
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
            ${ Expr.ofSeq(defs.toSeq.map(_.asExprOf[Operators#ReSource].asInstanceOf[Expr[Nothing]])) }: _*
          ) {
            ${ rdef.asExprOf[StaticTicket => Option[T]] }
          }(using ${ outerCreation.asInstanceOf[Expr[Nothing]] })
        }.asTerm
      else
        '{
          $api.Events.dynamic(${
            Expr.ofSeq(defs.toSeq.map(_.asExprOf[Operators#ReSource].asInstanceOf[Expr[Nothing]]))
          }: _*) {
            ${ rdef.asExprOf[Operators#DynamicTicket => Option[T]] }
          }(using ${ outerCreation.asInstanceOf[Expr[Nothing]] })
        }.asTerm
    }.asExprOf[Operators#Event[T]]
    println(s"res ${res.show}")
    res
  }

}
