package rescala.macros

import rescala.macros.MacroAccess

import scala.quoted.*
import rescala.default.*

class FindInterp extends ExprMap {

  var found: List[Expr[MacroAccess[_, _]]] = Nil

  override def transform[T](e: Expr[T])(using Type[T])(using Quotes): Expr[T] = {
    import quotes.reflect.*
    e match {
      case '{(${x}: MacroAccess[_, _]).value} =>
        found ::= x
        e
      case _ => transformChildren(e)
    }

  }
}

inline def dottyEventExpression[T](inline expr: Option[T]): Event[T] =
  ${ detectImpl('expr) }

def detectImpl[T: Type](expr: Expr[Option[T]])(using Quotes): Expr[Event[T]] =
  import quotes.reflect.*

  class ReplaceInterp(replacement: Map[Expr[MacroAccess[_, _]], Term], staticTicket: Tree) extends ExprMap {

    override def transform[T](e: Expr[T])(using Type[T])(using Quotes): Expr[T] = {
      import quotes.reflect.*
      e match {
        case '{(${x}: MacroAccess[_, _]).value} =>
          val wideType = TypeRepr.of[T].widen.asType
          val term = replacement(x).asExprOf[Interp[T]]
          println(s"term: ${term.show}")
          println(s"type: ${TypeRepr.of[T].widen}")
          '{(${staticTicket.asExprOf[StaticTicket]}.dependStatic(${term}))}
        case _ => transformChildren(e)
      }
    }
  }

  //val exprSym = Symbol.newMethod(Symbol.spliceOwner, "reactiveExpr", TypeRepr.of[StaticTicket => Option[Any]])

  val exprType = MethodType.apply(List("staticTicket"))((_: MethodType) => List(TypeRepr.of[StaticTicket]), (_ : MethodType) => TypeRepr.of[Option[T]])

  //println(Printer.TreeStructure.show(expr.asTerm))


  println(expr.show)

  println("transforming!")
  val fi = FindInterp()
  fi.transform(expr)
  val found = fi.found
  val res = ValDef.let(Symbol.spliceOwner, found.map(_.asTerm)){defs =>
    defs.foreach(d => println(Printer.TreeStructure.show(d)))
    val replacementMap = found.zip(defs).toMap
    //val rdef = DefDef(exprSym, {params =>
    val rdef = Lambda(Symbol.spliceOwner, exprType, {(sym, params) =>
      println(params)
      val staticTicket = params.head
      ReplaceInterp(replacementMap, staticTicket).transform(expr).asTerm
    })
    ('{Events.static(${Expr.ofSeq(defs.toSeq.map(_.asExprOf[ReSource]))} : _*){${rdef.asExprOf[StaticTicket => Option[T]]}}}).asTerm
  }.asExprOf[Event[T]]
  println(res.show)
  res
