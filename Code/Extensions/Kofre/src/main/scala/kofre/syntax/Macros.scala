package kofre.syntax

import scala.quoted.{Expr, Quotes, Type}
import scala.language.{dynamics, reflectiveCalls}

transparent inline def refinementTest[O, T](id: String, ops: O, init: T): O = ${ refinementImpl('id, 'ops, 'init) }

def refinementImpl[O: Type, T: Type](id: Expr[String], ops: Expr[O], init: Expr[T])(using quotes: Quotes): Expr[O] =
  import quotes.reflect.*
  val methods: List[Symbol] = TypeRepr.of[O].typeSymbol.declaredMethods
  // println(methods.map(sym =>
  //  s"""
  //    name      ${sym.name}
  //    is DefDef ${sym.isDefDef}
  //    signature ${sym.signature}
  //    params    ${sym.paramSymss}
  //    ref tpe   ${Ref(sym).tpe.show}
  //    wide tpe  ${Ref(sym).tpe.widenTermRefByName.show}
  //  """
  // ))

  val refinement = methods.foldLeft(TypeRepr.of[Object]) { (cur, inc) =>
    Refinement(cur, inc.name, Ref(inc).tpe.widenTermRefByName)
  }

  val res = refinement.asType match
    case '[t] => '{
        type Ref = t
        new CRDTDynamicMacro[O, T](${ id }, ${ ops }, ${ init }).asInstanceOf[Ref & CRDTDynamicMacro[O, T]]
      }
  // res is actually not of type O, but because the outer inline def is transparent, the compiler will ignore this
  // however, intellij uses the O for autocompletion, which is what we want because the CRDTDynamicMacro pretends to be an O
  res.asInstanceOf[Expr[O]]

class Container[T](var value: T)

class CRDTDynamicMacro[O, T](replicaId: String, ops: O, init: T) extends Selectable {
  val container = Container(init)

  transparent inline def applyDynamic(inline name: String)(inline arg: Any*): Any =
    ${ CRDTDynamicMacro.applyMacro[O, T]('container, 'ops, 'replicaId, 'name, 'arg) }
}

object CRDTDynamicMacro {
  def applyMacro[O, T](
      container: Expr[Container[T]],
      ops: Expr[O],
      id: Expr[String],
      name: Expr[String],
      arguments: Expr[Any]
  )(
      using
      quotes: Quotes,
      oType: Type[O],
      tType: Type[T]
  ): Expr[Any] =
    import quotes.reflect.*

    println(arguments.show)

    val Inlined(_, _, Typed(Repeated(args, _), _)) = arguments.asTerm

    val selected: Select = Select.unique(ops.asTerm, name.valueOrError)
    val mutator          = selected.appliedToArgs(args).asExpr
    println(s"mutator ${mutator.show}")
    val res2 = '{
      val mut = ${ mutator }
      mut match
        case m: DeltaMutator[T @unchecked] => ${ container }.value = m.apply($id, ${ container }.value)
        case _                             =>
      mut
    }
    println("final: " + res2.show)
    res2
}
