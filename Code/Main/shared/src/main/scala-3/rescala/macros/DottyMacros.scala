package rescala.macros

import rescala.macros.MacroAccess

import scala.quoted.*
import rescala.default.*

inline def dottyEventExpression[T](inline expr: Option[T]): Event[T] =
  ${ detectImpl('expr) }

def detectImpl[T: Type](expr: Expr[Option[T]])(using q: Quotes): Expr[Event[T]] =
  import q.reflect.*

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
        case '{ (${ x }: MacroAccess[_, interp]).value } =>
          val wideType = TypeRepr.of[T].widen.asType
          replacement.get(x) match
            case Some(replaced) =>
              val term = replaced.asExprOf[Readable[T]]
              '{ (${ ticket.asExprOf[StaticTicket] }.dependStatic[T](${ term })) }
            case None =>
              val term = x.asExprOf[Readable[T]]
              '{ (${ ticket.asExprOf[DynamicTicket] }.depend[T](${ term })) }
        case _ => transformChildren(e)
      }
    }
  }

  // val exprSym = Symbol.newMethod(Symbol.spliceOwner, "reactiveExpr", TypeRepr.of[StaticTicket => Option[Any]])

  // println(Printer.TreeStructure.show(expr.asTerm))

  val fi = FindInterp()
  fi.transform(expr)
  val definitions = FindDefs()
  definitions.transform(expr)
  val containsSymbol = ContainsSymbol(definitions.foundDefinitions)
  println(s"contains symbols: ${definitions.foundDefinitions}")
  val found    = fi.foundAbstractions.filterNot(containsSymbol.inTree)
  val isStatic = (found == fi.foundAbstractions)

  val funType = MethodType.apply(List("ticket"))(
    (_: MethodType) => List(if isStatic then TypeRepr.of[StaticTicket] else TypeRepr.of[DynamicTicket]),
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
        Events.static(${ Expr.ofSeq(defs.toSeq.map(_.asExprOf[ReSource])) }: _*) {
          ${ rdef.asExprOf[StaticTicket => Option[T]] }
        }
      }.asTerm
    else
      '{
        Events.dynamic(${ Expr.ofSeq(defs.toSeq.map(_.asExprOf[ReSource])) }: _*) {
          ${ rdef.asExprOf[DynamicTicket => Option[T]] }
        }
      }.asTerm
  }.asExprOf[Event[T]]
  println(s"res ${res.show}")
  res
