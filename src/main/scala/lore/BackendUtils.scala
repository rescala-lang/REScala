package lore
import AST._

// def traverseAST(
//     ast: Seq[Term],
//     transformer: ParsedExpression => ParsedExpression
// ) = ???

def traverseFromNode[A <: Term](
    node: A,
    transformer: Term => Term
): A =
  // apply transformer to start node
  val transformed = transformer(node)
  val result =
    // if node has children, traverse
    transformed match
      // basic terms
      case TInvariant(body) =>
        TInvariant(traverseFromNode(body, transformer))
      case TInteraction(
            reactiveTypes,
            argumentTypes,
            modifies,
            requires,
            ensures,
            executes
          ) =>
        TInteraction(
          reactiveTypes = reactiveTypes,
          argumentTypes = argumentTypes,
          modifies = modifies,
          requires = requires.map(traverseFromNode(_, transformer)),
          ensures = ensures.map(traverseFromNode(_, transformer)),
          executes = executes.map(traverseFromNode(_, transformer))
        )
      case TSource(body) =>
        TSource(traverseFromNode(body, transformer))
      case TDerived(body) =>
        TDerived(traverseFromNode(body, transformer))
      case TAbs(name, _type, body) =>
        TAbs(name, _type, traverseFromNode(body, transformer))
      case TArrow(l, r) =>
        TArrow(
          traverseFromNode(l, transformer),
          traverseFromNode(r, transformer)
        )
      case TDiv(l, r) =>
        TDiv(traverseFromNode(l, transformer), traverseFromNode(r, transformer))
      case TMul(l, r) =>
        TMul(traverseFromNode(l, transformer), traverseFromNode(r, transformer))
      case TAdd(l, r) =>
        TAdd(traverseFromNode(l, transformer), traverseFromNode(r, transformer))
      case TSub(l, r) =>
        TSub(traverseFromNode(l, transformer), traverseFromNode(r, transformer))
      case TLt(l, r) =>
        TLt(traverseFromNode(l, transformer), traverseFromNode(r, transformer))
      case TGt(l, r) =>
        TGt(traverseFromNode(l, transformer), traverseFromNode(r, transformer))
      case TLeq(l, r) =>
        TLeq(traverseFromNode(l, transformer), traverseFromNode(r, transformer))
      case TGeq(l, r) =>
        TGeq(traverseFromNode(l, transformer), traverseFromNode(r, transformer))
      case TEq(l, r) =>
        TEq(traverseFromNode(l, transformer), traverseFromNode(r, transformer))
      case TIneq(l, r) =>
        TIneq(
          traverseFromNode(l, transformer),
          traverseFromNode(r, transformer)
        )
      case TDisj(l, r) =>
        TDisj(
          traverseFromNode(l, transformer),
          traverseFromNode(r, transformer)
        )
      case TConj(l, r) =>
        TConj(
          traverseFromNode(l, transformer),
          traverseFromNode(r, transformer)
        )
      case TImpl(l, r) =>
        TImpl(
          traverseFromNode(l, transformer),
          traverseFromNode(r, transformer)
        )
      case TInSet(l, r) =>
        TInSet(
          traverseFromNode(l, transformer),
          traverseFromNode(r, transformer)
        )
      case TForall(vars, triggers, body) =>
        TForall(vars, triggers, traverseFromNode(body, transformer))
      case TExists(vars, body) =>
        TExists(vars = vars, body = traverseFromNode(body, transformer))
      case TParens(inner) => TParens(traverseFromNode(inner, transformer))
      case TFCall(parent, field, args) =>
        TFCall(
          traverseFromNode(parent, transformer),
          field,
          args.map(traverseFromNode(_, transformer))
        )
      case TFCurly(parent, field, body) =>
        TFCurly(
          traverseFromNode(parent, transformer),
          field,
          traverseFromNode(body, transformer)
        )
      case TFunC(name, args) =>
        TFunC(name, args.map(traverseFromNode(_, transformer)))
      case TIf(cond, _then, _else) =>
        TIf(
          traverseFromNode(cond, transformer),
          traverseFromNode(_then, transformer),
          _else.map(traverseFromNode(_, transformer))
        )
      case TArgT(_, _) | TVar(_) | TTypeAl(_, _) | TNum(_) | TTrue | TFalse |
          TString(_) =>
        transformed // don't traverse in cases without children
  try result.asInstanceOf[A]
  catch
    case c: ClassCastException =>
      throw new Exception("AST transformation led to invalid AST.")

def allReactives(ast: Seq[Term]): Map[String, (TReactive, Type)] =
  ast
    .collect({ case TAbs(name, _type, r: TReactive) => (name, (r, _type)) })
    .toMap

/** Returns all ids that are used in an expression.
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
  case TArgT(_, _)         => Set.empty
  case TArrow(l, r)        => uses(l) ++ uses(r)
  case TTypeAl(_, _)       => Set.empty
  case TString(_)          => Set.empty
  case TIf(cond, _then, _else) =>
    uses(cond) ++ uses(_then) ++ _else.map(uses).getOrElse(Set())
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
