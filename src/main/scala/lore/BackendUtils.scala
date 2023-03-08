package lore
import AST._

// type definitions
private type Graph[R] = Map[String, (R, Type)]
private type CompilationResult = (CompilationContext, Seq[String])
private case class CompilationContext(
    ast: Seq[Term]
):
  val graph: Graph[TReactive] = allReactives(ast)
  val sources: Graph[TSource] = graph.collect {
    case (name: String, (s: TSource, t: Type)) => (name, (s, t))
  }
  val derived: Graph[TDerived] = graph.collect {
    case (name: String, (d: TDerived, t: Type)) => (name, (d, t))
  }
  val invariants: Seq[TInvariant] = ast.collect({ case i: TInvariant =>
    i
  })
  val interactions: Map[String, TInteraction] = allInteractions(ast)
  val typeAliases: Map[String, Type] = ast.collect {
    case TTypeAl(name, _type) =>
      (name, _type)
  }.toMap

  def viperImports: List[TViperImport] = ast.collect {
    case i @ TViperImport(_) => i
  }.toList

  def reactivesPerInvariant: Map[TInvariant, Set[ID]] = invariants
    .map(i =>
      // get reactives that this invariant mentions
      val directDeps = uses(i).filter(name => graph.keySet.contains(name))
      // collect (transitive) inputs of these reactives
      val allDeps =
        directDeps.flatMap(name =>
          getSubgraph(name, graph.view.mapValues(_._1).toMap)
        )
      (i, allDeps)
    )
    .toMap

// flattenInteractions until the result does not change anymore
def flattenInteractions(ctx: CompilationContext): CompilationContext =
  def flatten(t: Term, ctx: CompilationContext): Term =
    val interactionKeywords =
      List("requires", "ensures", "executes")
    t match
      // is requires/ensures/executes call
      case TFCurly(parent, field, body)
          if interactionKeywords.contains(field) =>
        flatten(parent, ctx) match
          // parent is an interaction
          case i: TInteraction =>
            field match
              case "requires" => i.copy(requires = i.requires :+ body)
              case "ensures"  => i.copy(ensures = i.ensures :+ body)
              case "executes" => i.copy(executes = Some(body))
              case wrongField =>
                throw Exception(s"Invalid call on Interaction: $wrongField")
          // parent is variable that refers to an interaction
          case TVar(name) if ctx.interactions.keys.toList.contains(name) =>
            flatten(
              TFCurly(ctx.interactions(name), field, body),
              ctx
            )
          // else -> leave term untouched
          case _ => t
      // is modifies call
      case TFCall(parent, "modifies", args) =>
        flatten(parent, ctx) match
          // parent is an interaction
          case i: TInteraction =>
            args.foldLeft(i) {
              case (i, TVar(id)) => i.copy(modifies = i.modifies :+ id)
              case e =>
                throw Exception(
                  s"Invalid argumeViperCompilationExceptionnt for modifies statement: $e"
                )
            }
          // parent is variable that refers to an interaction
          case TVar(name) if ctx.interactions.keys.toList.contains(name) =>
            flatten(
              TFCall(ctx.interactions(name), "modifies", args),
              ctx
            )
          case _ => t
      case _ => t

  val res = ctx.copy(ast = ctx.ast.map(traverseFromNode(_, flatten(_, ctx))))
  if res == ctx then return res
  else return flattenInteractions(res)

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
      case TNeg(body) => TNeg(traverseFromNode(body, transformer))
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
      case TTuple(factors) =>
        TTuple(factors.map(traverseFromNode(_, transformer)))
      case TArgT(_, _) | TVar(_) | TTypeAl(_, _) | TNum(_) | TTrue | TFalse |
          TString(_) | TViperImport(_) =>
        transformed // don't traverse in cases without children
  try result.asInstanceOf[A]
  catch
    case c: ClassCastException =>
      throw new Exception("AST transformation led to invalid AST.")

/** Replaces all occurences of the ID `from` with the ID `to` in the given term.
  * This function currently does not consider scoping.
  * @param from
  *   the id to be renamed
  * @param to
  *   the new name
  * @param term
  *   the term to be modifier
  * @return
  *   a version of the term where the ID is renamed
  */
def rename(from: ID, to: ID, term: Term): Term =
  val transformer: Term => Term =
    case t @ TArgT(name, _type) => if name == from then TArgT(to, _type) else t
    case t @ TVar(name)         => if name == from then TVar(to) else t
    case TAbs(name, _type, body) =>
      val newName = if name == from then to else name
      TAbs(newName, _type, rename(from, to, body))
    case t => t
  traverseFromNode(term, transformer)

def allReactives(ast: Seq[Term]): Map[String, (TReactive, Type)] =
  ast.collect { case TAbs(name, _type, r: TReactive) =>
    (name, (r, _type))
  }.toMap

def allInteractions(ast: Seq[Term]): Map[String, TInteraction] =
  ast.collect { case TAbs(name, _type, t: TInteraction) =>
    (name, t)
  }.toMap

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
  case TTuple(factors)              => factors.toList.flatMap(uses).toSet
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
  case TNeg(inner)                  => uses(inner)
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
  case TViperImport(_) => Set.empty
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
