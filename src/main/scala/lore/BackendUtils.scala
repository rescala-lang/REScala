package lore
import AST._

// def traverseAST(
//     ast: Seq[Term],
//     transformer: ParsedExpression => ParsedExpression
// ) = ???

// def traverseFromNode[A <: Term](
//     node: A,
//     transformer: Term => Term
// ): A =
//   val transformed = transformer(node)
//   val result =
//     // check if transformer did anything
//     if transformed != node then transformed
//     // if not, go deeper
//     else
//       transformed match
//         case Precondition(index, body) =>
//           Precondition(index, traverseFromNode(body, transformer))
//         case Postcondition(index, body) =>
//           Postcondition(index, traverseFromNode(body, transformer))
//         case Invariant(index, body) =>
//           Invariant(index, traverseFromNode(body, transformer))
//         case Transaction(index, name, reactives, pre, post, args, body) =>
//           Transaction(
//             index,
//             traverseFromNode(name, transformer),
//             reactives.map(_.map(traverseFromNode(_, transformer))),
//             pre.map(traverseFromNode(_, transformer)),
//             post.map(traverseFromNode(_, transformer)),
//             args.map { case (id, typeAnn) =>
//               (traverseFromNode(id, transformer), typeAnn)
//             },
//             traverseFromNode(body, transformer)
//           )
//         case SourceReactive(index, name, body, typeAnn) =>
//           SourceReactive(
//             index,
//             traverseFromNode(name, transformer),
//             traverseFromNode(body, transformer),
//             typeAnn
//           )
//         case Binding(index, name, body, typeAnn) =>
//           Binding(
//             index,
//             traverseFromNode(name, transformer),
//             traverseFromNode(body, transformer),
//             typeAnn
//           )
//         case Call(index, name, args) =>
//           Call(
//             index,
//             traverseFromNode(name, transformer),
//             args.map(traverseFromNode(_, transformer))
//           )
//         case MethodCall(index, parent, method, args) =>
//           MethodCall(
//             index,
//             traverseFromNode(parent, transformer),
//             traverseFromNode(method, transformer),
//             args.map(traverseFromNode(_, transformer))
//           )
//         case Parens(index, inner) =>
//           Parens(index, traverseFromNode(inner, transformer))
//         case Division(index, l, r) =>
//           Division(
//             index,
//             traverseFromNode(l, transformer),
//             traverseFromNode(r, transformer)
//           )
//         case Multiplication(index, l, r) =>
//           Multiplication(
//             index,
//             traverseFromNode(l, transformer),
//             traverseFromNode(r, transformer)
//           )
//         case Addition(index, l, r) =>
//           Addition(
//             index,
//             traverseFromNode(l, transformer),
//             traverseFromNode(r, transformer)
//           )
//         case Substraction(index, l, r) =>
//           Substraction(
//             index,
//             traverseFromNode(l, transformer),
//             traverseFromNode(r, transformer)
//           )
//         case BoolParens(index, inner) =>
//           BoolParens(index, traverseFromNode(inner, transformer))
//         case Lt(index, l, r) =>
//           Lt(
//             index,
//             traverseFromNode(l, transformer),
//             traverseFromNode(r, transformer)
//           )
//         case Gt(index, l, r) =>
//           Gt(
//             index,
//             traverseFromNode(l, transformer),
//             traverseFromNode(r, transformer)
//           )
//         case Leq(index, l, r) =>
//           Leq(
//             index,
//             traverseFromNode(l, transformer),
//             traverseFromNode(r, transformer)
//           )
//         case Geq(index, l, r) =>
//           Geq(
//             index,
//             traverseFromNode(l, transformer),
//             traverseFromNode(r, transformer)
//           )
//         case Equality(index, l, r) =>
//           Equality(
//             index,
//             traverseFromNode(l, transformer),
//             traverseFromNode(r, transformer)
//           )
//         case Inequality(index, l, r) =>
//           Inequality(
//             index,
//             traverseFromNode(l, transformer),
//             traverseFromNode(r, transformer)
//           )
//         case Disjunction(index, l, r) =>
//           Disjunction(
//             index,
//             traverseFromNode(l, transformer),
//             traverseFromNode(r, transformer)
//           )
//         case Conjunction(index, l, r) =>
//           Conjunction(
//             index,
//             traverseFromNode(l, transformer),
//             traverseFromNode(r, transformer)
//           )
//         case Implication(index, l, r) =>
//           Implication(
//             index,
//             traverseFromNode(l, transformer),
//             traverseFromNode(r, transformer)
//           )
//         case InSet(index, l, r) =>
//           InSet(
//             index,
//             traverseFromNode(l, transformer),
//             traverseFromNode(r, transformer)
//           )
//         case Forall(index, vars, triggers, body) =>
//           Forall(
//             index,
//             vars.map { case (id, typeName) =>
//               (traverseFromNode(id, transformer), typeName)
//             },
//             triggers.map(traverseFromNode(_, transformer)),
//             traverseFromNode(body, transformer)
//           )
//         case AST.Exists(index, vars, body) =>
//           AST.Exists(
//             index,
//             vars.map { case (id, typeName) =>
//               (traverseFromNode(id, transformer), typeName)
//             },
//             traverseFromNode(body, transformer)
//           )
//         case e => e // don't traverse in cases without children
//   try result.asInstanceOf[A]
//   catch
//     case c: ClassCastException =>
//       throw new Exception("AST transformation led to invalid AST.")

def allReactives(ast: Seq[Term]): Map[String, TReactive] =
  ast.collect({ case TAbs(name, _type, r: TReactive) => (name, r) }).toMap

/** Returns the names of all ids that are used throughout the expression.
  *
  * @param e
  *   the expression
  * @return
  *   a set of all used IDs
  */
def uses(e: Term): Set[ID] = e match
  case TInvariant(body) => uses(body)
  case t: TInteraction =>
    t.modifies.toSet ++ t.requires.flatMap(uses).toSet ++
      t.ensures.flatMap(uses).toSet ++ t.executes.map(uses).getOrElse(Set.empty)
  case t: TAbs                      => uses(t.body)
  case TVar("_")                    => Set.empty
  case TVar(name)                   => Set(name)
  case t: TReactive                 => uses(t.body)
  case TFunC(name, args)            => args.flatMap(uses).toSet
  case TFCall(parent, field, args)  => uses(parent) ++ args.flatMap(uses)
  case TFCurly(parent, field, body) => uses(parent) ++ uses(body)
  case TParens(inner)               => uses(inner)
  case TNum(_)                      => Set()
  case TDiv(l, r)                   => uses(l) ++ uses(r)
  case TMul(l, r)                   => uses(l) ++ uses(r)
  case TAdd(l, r)                   => uses(l) ++ uses(r)
  case TSub(l, r)                   => uses(l) ++ uses(r)
  case TTrue                        => Set()
  case TFalse                       => Set()
  case TLt(l, r)                    => uses(l) ++ uses(r)
  case TGt(l, r)                    => uses(l) ++ uses(r)
  case TLeq(l, r)                   => uses(l) ++ uses(r)
  case TGeq(l, r)                   => uses(l) ++ uses(r)
  case TEq(l, r)                    => uses(l) ++ uses(r)
  case TIneq(l, r)                  => uses(l) ++ uses(r)
  case TDisj(l, r)                  => uses(l) ++ uses(r)
  case TConj(l, r)                  => uses(l) ++ uses(r)
  case TImpl(l, r)                  => uses(l) ++ uses(r)
  case TForall(vars, triggers, body) =>
    triggers.flatMap(uses).toSet ++ uses(body)
  case TExists(vars, body) => uses(body)
  case TInSet(l, r)        => uses(l) ++ uses(r)
//   case e: StringExpr                 => Set()

// def getDependants(
//     reactive: Reactive,
//     ast: Seq[ParsedExpression]
// ): Set[Reactive] =
//   val directChildren: Set[DerivedReactive] = ast
//     .collect({
//       case d: DerivedReactive if uses(d).contains(reactive.name.name) => d
//     })
//     .toSet
//   Set(reactive) ++ directChildren.flatMap(c => getDependants(c, ast))

/** Given the name of a reactive and the registry of all reactives, this
  * function returns all reactives that this one (transitively) depends on.
  */
def getSubgraph(
    reactiveName: ID,
    graph: Map[String, TReactive]
): Set[ID] =
  val reactive = graph.get(reactiveName)
  reactive match
    case Some(s: TSource) => Set(reactiveName)
    case Some(d: TDerived) =>
      val usedReactives =
        uses(d).filter(name => graph.keys.toSet.contains(name))
      Set(reactiveName) ++ usedReactives.flatMap(getSubgraph(_, graph))
    case None =>
      throw new Exception(s"Reactive ${reactiveName} not found in AST.")
