package lore.backends

import lore.AST.*
import lore.optics.given

import scala.annotation.tailrec

// type definitions
private type Graph[R] = Map[String, (R, Type)]
private type CompilationResult = (CompilationContext, Seq[String])

private case class CompilationContext(
                                       ast: Seq[Term]
                                     ) {
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
    case TTypeAl(name, _type, _) =>
      (name, _type)
  }.toMap

  def viperImports: List[TViperImport] = ast.collect {
    case i@TViperImport(_, _) => i
  }.toList

  def reactivesPerInvariant: Map[TInvariant, Set[ID]] = invariants
    .map { (i: TInvariant) =>
      // get reactives that this invariant mentions
      val directDeps = uses(i).intersect(graph.keySet)
      // collect (transitive) inputs of these reactives
      val allDeps =
        directDeps.flatMap(name =>
          getSubgraph(name, graph.view.mapValues(_._1).toMap)
        )
      (i, allDeps)
    }
    .toMap
}

// flattenInteractions until the result does not change anymore
@tailrec
def flattenInteractions(ctx: CompilationContext): CompilationContext = {
  def flatten(t: Term, ctx: CompilationContext): Term = {
    val interactionKeywords =
      List("requires", "ensures", "executes")
    t match {
      // is requires/ensures/executes call
      case curly: TFCurly if interactionKeywords.contains(curly.field) =>
        flatten(curly.parent, ctx) match {
          // parent is an interaction
          case i: TInteraction =>
            curly.field match {
              case "requires" => i.copy(requires = i.requires :+ curly.body)
              case "ensures" => i.copy(ensures = i.ensures :+ curly.body)
              case "executes" => i.copy(executes = Some(curly.body))
              case wrongField =>
                throw Exception(s"Invalid call on Interaction: $wrongField")
            }
          // parent is variable that refers to an interaction
          case t: TVar if ctx.interactions.keys.toList.contains(t.name) =>
            flatten(
              TFCurly(ctx.interactions(t.name), curly.field, curly.body),
              ctx
            )
          // else -> leave term untouched
          case _ => t
        }
      // is modifies call
      case TFCall(parent, "modifies", args, _) =>
        flatten(parent, ctx) match {
          // parent is an interaction
          case i: TInteraction =>
            args.foldLeft(i) {
              case (i, TVar(id, _)) => i.copy(modifies = i.modifies :+ id)
              case e =>
                throw Exception(
                  s"Invalid argumeViperCompilationExceptionnt for modifies statement: $e"
                )
            }
          // parent is variable that refers to an interaction
          case TVar(name, _) if ctx.interactions.keys.toList.contains(name) =>
            flatten(
              TFCall(ctx.interactions(name), "modifies", args),
              ctx
            )
          case _ => t
        }
      case _ => t
    }
  }

  val res = ctx.copy(ast = ctx.ast.map(traverseFromNode(_, flatten(_, ctx))))
  if res == ctx then return res
  else return flattenInteractions(res)
}

/** Applies a transformer function to every node in the subgraph, starting from
 * a given node and traversing downwards from there.
 *
 * @param node
 * @param transformer
 * @return
 */
def traverseFromNode[A <: Term](
                                 node: A,
                                 transformer: Term => Term
                               ): A = {
  // apply transformer to start node
  val transformed = transformer(node)
  val result =
    // if node has children, traverse
    transformed match {
      // basic terms
      case t: TInvariant =>
        t.copy(condition = traverseFromNode(t.condition, transformer))
      case t: TInteraction =>
        t.copy(
          requires = t.requires.map(traverseFromNode(_, transformer)),
          ensures = t.ensures.map(traverseFromNode(_, transformer)),
          executes = t.executes.map(traverseFromNode(_, transformer))
        )
      case t: TSource =>
        t.copy(body = traverseFromNode(t.body, transformer))
      case t: TDerived =>
        t.copy(body = traverseFromNode(t.body, transformer))
      case t: TAbs =>
        t.copy(body = traverseFromNode(t.body, transformer))
      case t: TArrow =>
        t.copy(
          left = traverseFromNode(t.left, transformer),
          right = traverseFromNode(t.right, transformer)
        )
      case t: TDiv =>
        t.copy(
          left = traverseFromNode(t.left, transformer),
          right = traverseFromNode(t.right, transformer)
        )
      case t: TMul =>
        t.copy(
          left = traverseFromNode(t.left, transformer),
          right = traverseFromNode(t.right, transformer)
        )
      case t: TAdd =>
        t.copy(
          left = traverseFromNode(t.left, transformer),
          right = traverseFromNode(t.right, transformer)
        )
      case t: TSub =>
        t.copy(
          left = traverseFromNode(t.left, transformer),
          right = traverseFromNode(t.right, transformer)
        )
      case t: TNeg => t.copy(body = traverseFromNode(t.body, transformer))
      case t: TLt =>
        t.copy(
          left = traverseFromNode(t.left, transformer),
          right = traverseFromNode(t.right, transformer)
        )
      case t: TGt =>
        t.copy(
          left = traverseFromNode(t.left, transformer),
          right = traverseFromNode(t.right, transformer)
        )
      case t: TLeq =>
        t.copy(
          left = traverseFromNode(t.left, transformer),
          right = traverseFromNode(t.right, transformer)
        )
      case t: TGeq =>
        t.copy(
          left = traverseFromNode(t.left, transformer),
          right = traverseFromNode(t.right, transformer)
        )
      case t: TEq =>
        t.copy(
          left = traverseFromNode(t.left, transformer),
          right = traverseFromNode(t.right, transformer)
        )
      case t: TIneq =>
        t.copy(
          left = traverseFromNode(t.left, transformer),
          right = traverseFromNode(t.right, transformer)
        )
      case t: TDisj =>
        t.copy(
          left = traverseFromNode(t.left, transformer),
          right = traverseFromNode(t.right, transformer)
        )
      case t: TConj =>
        t.copy(
          left = traverseFromNode(t.left, transformer),
          right = traverseFromNode(t.right, transformer)
        )
      case t: TBImpl =>
        t.copy(
          left = traverseFromNode(t.left, transformer),
          right = traverseFromNode(t.right, transformer)
        )
      case t: TImpl =>
        t.copy(
          left = traverseFromNode(t.left, transformer),
          right = traverseFromNode(t.right, transformer)
        )
      case t: TInSet =>
        t.copy(
          left = traverseFromNode(t.left, transformer),
          right = traverseFromNode(t.right, transformer)
        )
      case t: TForall =>
        t.copy(body = traverseFromNode(t.body, transformer))
      case t: TExists =>
        t.copy(body = traverseFromNode(t.body, transformer))
      case t: TParens =>
        t.copy(inner = traverseFromNode(t.inner, transformer))
      case t: TFCall =>
        t.copy(
          parent = traverseFromNode(t.parent, transformer),
          args = t.args.map(traverseFromNode(_, transformer))
        )
      case t: TFCurly =>
        t.copy(
          parent = traverseFromNode(t.parent, transformer),
          body = traverseFromNode(t.body, transformer)
        )
      case t: TFunC =>
        t.copy(args = t.args.map(traverseFromNode(_, transformer)))
      case t: TIf =>
        t.copy(
          cond = traverseFromNode(t.cond, transformer),
          _then = traverseFromNode(t._then, transformer),
          _else = t._else.map(traverseFromNode(_, transformer))
        )
      case t: TTuple =>
        t.copy(factors = t.factors.map(traverseFromNode(_, transformer)))
      case t: TSeq =>
        t.copy(body = t.body.map(traverseFromNode(_, transformer)))
      case t: TAssert =>
        t.copy(body = traverseFromNode(t.body, transformer))
      case t: TAssume =>
        t.copy(body = traverseFromNode(t.body, transformer))
      case t: (TArgT | TVar | TTypeAl | TNum | TTrue | TFalse | TString |
        TViperImport) =>
        transformed // don't traverse in cases without children
      // case TArgT(_, _, _) | TVar(_, _) | TTypeAl(_, _, _) | TNum(_) | TTrue |
      //     TFalse | TString(_) | TViperImport(_) =>
      //   transformed
    }
  try result.asInstanceOf[A]
  catch {
    case c: ClassCastException =>
      throw new Exception("AST transformation led to invalid AST.")
  }
}

/** Replaces all occurences of the ID `from` with the ID `to` in the given term.
 * This function currently does not consider scoping.
 *
 * @param from
 * the id to be renamed
 * @param to
 * the new name
 * @param term
 * the term to be modifier
 * @return
 * a version of the term where the ID is renamed
 */
def rename(from: ID, to: ID, term: Term): Term = {
  val transformer: Term => Term = {
    case t: TArgT => if t.name == from then TArgT(to, t._type) else t
    case t: TVar => if t.name == from then TVar(to) else t
    case t: TAbs =>
      val newName = if t.name == from then to else t.name
      TAbs(newName, t._type, rename(from, to, t.body))
    case t => t
  }
  traverseFromNode(term, transformer)
}

private def allReactives(ast: Seq[Term]): Map[String, (TReactive, Type)] =
  ast.collect { case TAbs(name, _type, r: TReactive, _) =>
    (name, (r, _type))
  }.toMap

private def allInteractions(ast: Seq[Term]): Map[String, TInteraction] =
  ast.collect { case TAbs(name, _type, t: TInteraction, _) =>
    (name, t)
  }.toMap

/** Returns all ids that are used in a term.
 */
def uses: Term => Set[ID] = {
  // these cases mark usage of an id
  case t: TInteraction =>
    t.modifies.toSet ++ t.children.flatMap(uses).toSet
  case TVar("_", _) => Set.empty
  case TVar(name, _) => Set(name)
  case t: TFunC => t.args.flatMap(uses).toSet
  // these cases do not
  case t: (TNum | TString | TTrue | TFalse | TArgT | TTypeAl | TViperImport) =>
    Set.empty
  // in these cases we have to traverse into the child nodes
  case t: (TInvariant | TAbs | TTuple | TReactive | TFCall | TFCurly | TParens |
    TForall | TExists | TNeg | BinaryOp | TIf | TSeq | TAssert | TAssume) =>
    t.children.flatMap(uses).toSet
}

/** Given the name of a reactive and the registry of all reactives, this
 * function returns all reactives that this one (transitively) depends on.
 */
def getSubgraph(
                 reactiveName: ID,
                 graph: Map[String, TReactive]
               ): Set[ID] = {
  val reactive = graph.get(reactiveName)
  reactive match {
    case Some(s: TSource) => Set(reactiveName)
    case Some(d: TDerived) =>
      val usedReactives =
        uses(d).filter(name => graph.keys.toSet.contains(name))
      Set(reactiveName) ++ usedReactives.flatMap(getSubgraph(_, graph))
    case None =>
      throw new Exception(s"Reactive ${reactiveName} not found in AST.")
  }
}
