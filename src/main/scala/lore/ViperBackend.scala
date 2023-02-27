package lore

import AST._
import cats.implicits._

object ViperBackend:
  case class ViperCompilationException(message: String)
      extends Exception(message)

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

  def toViper(ast: Seq[Term]): String =
    //// step 1: build context object that we pass around to subfunctions
    val ctx = CompilationContext(ast)

    //// new step: Viper specific transformations/transform field calls to function calls
    val ctxy = viperTransformations(ctx)

    //// step 2: compile source reactives
    val (ctx3, sourcesCompiled) = compileSources(ctxy)

    // compile derived reactives
    val (ctx4, derivedCompiled) = compileDerived(ctx3)

    //// step 4: compile invariants
    val (ctx5, invariantsCompiled) = compileInvariants(ctx4)

//     // step 4: compile interactions
    val (ctx6, interactionsCompiled) = compileInteractions(ctx5)

    s"""|// sources
					|${sourcesCompiled.mkString("\n")}
					|// derived
					|${derivedCompiled.mkString("\n")}
					|// invariants
					|${invariantsCompiled.mkString("\n")}
					|
					|// interactions
					|${interactionsCompiled.mkString("\n")}
					|""".stripMargin

  private def viperTransformations(
      ctx: CompilationContext
  ): CompilationContext =
    // step 1: flatten interactions
    def flattenInteractions(t: Term, ctx: CompilationContext): Term =
      val interactionKeywords =
        List("requires", "ensures", "executes")
      t match
        // is requires/ensures/executes call
        case TFCurly(parent, field, body)
            if interactionKeywords.contains(field) =>
          flattenInteractions(parent, ctx) match
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
              flattenInteractions(
                TFCurly(ctx.interactions(name), field, body),
                ctx
              )
            // else -> leave term untouched
            case _ => t
        // is modifies call
        case TFCall(parent, "modifies", args) =>
          flattenInteractions(parent, ctx) match
            // parent is an interaction
            case i: TInteraction =>
              args.foldLeft(i) {
                case (i, TVar(id)) => i.copy(modifies = i.modifies :+ id)
                case e =>
                  throw ViperCompilationException(
                    s"Invalid argument for modifies statement: $e"
                  )
              }
            // parent is variable that refers to an interaction
            case TVar(name) if ctx.interactions.keys.toList.contains(name) =>
              flattenInteractions(
                TFCall(ctx.interactions(name), "modifies", args),
                ctx
              )
            case _ => t
        case _ => t
    // flattenInteractions until the result does not change anymore
    def flattenRecursively(ctx: CompilationContext): CompilationContext =
      val res = ctx.copy(ast =
        ctx.ast.map(traverseFromNode(_, flattenInteractions(_, ctx)))
      )
      if res == ctx then return res
      else return flattenRecursively(res)
    val ctx2 = flattenRecursively(ctx)

    // step 2: field calls as function calls
    def fieldCallToFunCall: Term => Term =
      case TFCall(parent, field, args) =>
        TFunC(field, parent +: args)
      case t => t

    return ctx2.copy(ast =
      ctx2.ast.map(traverseFromNode(_, fieldCallToFunCall))
    )

  private def compileSources(ctx: CompilationContext): CompilationResult =
    def sourceToField(name: ID, _type: Type): String =
      s"field $name: ${typeToViper(_type)(using ctx)}"
    return (
      ctx,
      ctx.sources.map { case (name: ID, (source: TSource, _type: Type)) =>
        sourceToField(name, _type)
      }.toSeq
    )

  private def compileDerived(ctx: CompilationContext): CompilationResult =
    def derivedToMacro(
        graph: Map[String, (TReactive, Type)],
        name: ID
    ): String =
      val (d, _) = graph(name)
      val usedReactives = uses(d).filter(r => graph.keys.toSet.contains(r))
      val body = expressionToViper(d.body)(using ctx)
      val argsString = usedReactives.toSeq.sorted.mkString(", ")

      s"define $name($argsString) $body"

    return (
      ctx,
      ctx.derived.map { case (name: String, (_, _)) =>
        derivedToMacro(ctx.graph, name)
      }.toSeq
    )

  private def compileInvariants(ctx: CompilationContext): CompilationResult =
    // collect relevant reactives per invariant
    val reactivesPerInvariant: Map[TInvariant, Set[ID]] = ctx.invariants
      .map(i =>
        // get reactives that this invariant mentions
        val directDeps = uses(i).filter(name => ctx.graph.keySet.contains(name))
        // collect (transitive) inputs of these reactives
        val allDeps =
          directDeps.flatMap(name =>
            getSubgraph(name, ctx.graph.view.mapValues(_._1).toMap)
          )
        (i, allDeps)
      )
      .toMap

    def invariantToMacro(
        invariant: TInvariant,
        inputs: Set[ID],
        id: String
    ): String =
      val inputsString = inputs.toSeq.sorted.mkString(", ")
      s"define inv_$id($inputsString) ${expressionToViper(invariant.condition)(using ctx)}"

    return (
      ctx,
      ctx.invariants
        .zip(1 to ctx.invariants.length + 1)
        .map((inv: TInvariant, id: Int) =>
          // replace references to reactives with macro calls
          invariantToMacro(
            invariant = inv,
            inputs = reactivesPerInvariant(inv)
              .map(name => (name, ctx.graph(name)))
              .collect({ case (name: ID, (_: TSource, _: Type)) => name }),
            id = id.toString
          )
        )
    )

  private def compileInteractions(ctx: CompilationContext): CompilationResult =
    val interactions = ctx.interactions
      // only compile interactions that have some guarantees and modify some reactives
      .filter((_, i) => !i.ensures.isEmpty && !i.modifies.isEmpty)
      .map(interactionToMethod(_, _, ctx))
      .toList
    return (ctx, interactions)

  private def interactionToMethod(
      name: ID,
      interaction: TInteraction,
      ctx: CompilationContext
  ): String =
    // collect arguments and their types
    // extract names from body
    def collectArgNames(acc: List[ID], term: TArrow): List[ID] =
      term match
        case TArrow(TVar(name), t @ TArrow(_, _)) =>
          collectArgNames(acc :+ name, t)
        case TArrow(TVar(name), _) =>
          acc :+ name
        case _ =>
          throw ViperCompilationException(
            s"Tried to compile interaction $interaction to Viper but received invalid function in body: $term"
          )
    val allNames = interaction.executes match
      case None                      => List()
      case Some(term @ TArrow(_, _)) => collectArgNames(List(), term)
      case Some(term)                => List()
    val argNames =
      allNames.drop(interaction.reactiveTypes.length)
    val argTypes = interaction.argumentTypes.map(t => typeToViper(t)(using ctx))

    if argNames.length != argTypes.length then
      throw ViperCompilationException(
        s"Tried to compile interaction but number of arguments does not match interaction body. argnames: $argNames, argTypes: $argTypes"
      )
    val argsString =
      (argNames zip argTypes)
        .map((name, `type`) => s"$name: ${`type`}")

    // val argsString = transaction.args.map{
    // 		case (name, Some(typeName)) => s"${name.name}: ${typeName.name}"
    // 		case (name, None) => throw new Exception(s"Transaction $transaction needs type annotations to be compiled to Viper.")
    // }

    // val graph = allReactives(ast)

    // // divide transaction into subtransactions
    // val subtransactions: Seq[String] = transaction.reactives.map(names =>
    // 		val reactives: Seq[Reactive] = names.map(n => graph.get(n.name) match
    // 				case Some(r) => r
    // 				case None => throw Exception(s"Reactive ${n.name} not found in graph."))
    // 		val subGraph: Set[Reactive] = reactives.map(r => getDependants(r, ast))
    // 				.toSet.flatten
    // 		val subGraphNames: Set[String] = subGraph.map(r => r.name.name)

    // 		val sourceReactives: Seq[SourceReactive] =
    // 				reactives.collect({case s: SourceReactive => s})
    // 		val derivedReactives: Seq[DerivedReactive] =
    // 				subGraph.collect({case d: DerivedReactive => d}).toSeq

    // 		val invariants: Set[Invariant] = ast.collect({
    // 				case i: Invariant if !uses(i).intersect(subGraphNames).isEmpty => i}).toSet
    // 		val invariantsStrings: Set[String] = invariants.map(i => expressionToViper(i.body))
    // 		val invariantsAfter: Set[String] =
    // 				invariantsStrings.map(s =>
    // 						derivedReactives.foldl(s){case (acc, derivedReactive) =>
    // 								acc.replaceAll(derivedReactive.name.name, s"${derivedReactive.name.name}_")}
    // 				)

    // 		val graphString = sourceReactives.map{
    // 				case (SourceReactive(_, id, body, Some(typeAnn))) => s"${id.name}: $typeAnn"
    // 				case (r @ SourceReactive(_, name, body, None)) => throw new Exception(s"Reactive $r needs type annotation to be compiled to Viper.")
    // 		}
    // 		val graphStringAfter = sourceReactives.map{
    // 				case (SourceReactive(_, id, body, Some(typeAnn))) => s"${id.name}_: $typeAnn"
    // 				case (r @ SourceReactive(_, name, body, None)) => throw new Exception(s"Reactive $r needs type annotation to be compiled to Viper.")
    // 		}

    // 		val preconditions = transaction.preconditions.map(p =>
    // 				"requires " + expressionToViper(p.body))
    // 		val postconditions = transaction.postconditions.map(p =>
    // 				"ensures " + expressionToViper(p.body))

    s"""|method $name (
        |  // arguments
        |${argsString.mkString(",\n").indent(2)})
        |returns (
        |)
        |// preconditions
        |// relevant invariants
        |// postconditions
        |// relevant invariants
        |{
        |}
        """.stripMargin

  private def typeToViper(t: Type)(using ctx: CompilationContext): String =
    t match
      // replace type aliases
      case SimpleType(name, _) if ctx.typeAliases.contains(name) =>
        typeToViper(ctx.typeAliases(name))
      case SimpleType(name, Nil) => name
      case SimpleType("Source", inner) =>
        s"${inner.map(typeToViper).mkString(" ,")}"
      case SimpleType(name, inner) =>
        s"$name[${inner.map(typeToViper).mkString(" ,")}]"
      case TupleType(inner) =>
        s"(${inner.toList.map(typeToViper).mkString(" ,")})"

  private def expressionToViper(expression: Term)(using
      ctx: CompilationContext
  ): String = expression match
    case v: TViper =>
      v match
        case TVar(id)  => id
        case TTrue     => "true"
        case TFalse    => "false"
        case TEq(l, r) => s"${expressionToViper(l)} == ${expressionToViper(r)}"
        case TIneq(l, r) =>
          s"${expressionToViper(l)} != ${expressionToViper(r)}"
        case TDisj(l, r) =>
          s"${expressionToViper(l)} || ${expressionToViper(r)}"
        case TConj(l, r) =>
          s"${expressionToViper(l)} && ${expressionToViper(r)}"
        case TImpl(l, r) =>
          s"${expressionToViper(l)} ==> ${expressionToViper(r)}"
        case TLt(l, r)  => s"${expressionToViper(l)} < ${expressionToViper(r)}"
        case TGt(l, r)  => s"${expressionToViper(l)} > ${expressionToViper(r)}"
        case TLeq(l, r) => s"${expressionToViper(l)} <= ${expressionToViper(r)}"
        case TGeq(l, r) => s"${expressionToViper(l)} >= ${expressionToViper(r)}"
        case TForall(vars, triggers, body) =>
          val varString =
            vars
              .map { case TArgT(name, t) => s"$name: ${typeToViper(t)}" }
              .toList
              .mkString(", ")
          triggers match
            case Nil => s"forall $varString :: ${expressionToViper(body)}"
            case t =>
              s"forall $varString :: {${t.map(expressionToViper).mkString(", ")}} ${expressionToViper(body)}"
        case TExists(vars, body) =>
          val varString =
            vars
              .map { case TArgT(name, t) => s"$name: ${typeToViper(t)}" }
              .toList
              .mkString(", ")
          s"exists $varString :: ${expressionToViper(body)}"
        case TAdd(l, r) => s"${expressionToViper(l)} + ${expressionToViper(r)}"
        case TSub(l, r) => s"${expressionToViper(l)} - ${expressionToViper(r)}"
        case TDiv(l, r) => s"${expressionToViper(l)} / ${expressionToViper(r)}"
        case TMul(l, r) => s"${expressionToViper(l)} * ${expressionToViper(r)}"
        case TNum(i)    => s"$i"
        case TParens(inner) => s"(${expressionToViper(inner)})"
        case TFunC(
              "union",
              x :: y :: Nil
            ) => // handle set arithmetics. TODO: support more cases
          s"${expressionToViper(x)} union ${expressionToViper(y)}"
        case TFunC(name, args) =>
          val argsString = args.map(a => expressionToViper(a)).mkString(", ")
          s"$name($argsString)"
        case TFCall(parent, field, List()) => // field call
          s"${expressionToViper(parent)}.$field"
        case TFCall(parent, field, args) => // field call
          throw new ViperCompilationException(
            s"Viper does only allow field but not method access! $parent.$field(${args.mkString(", ")})"
          )
          s"${expressionToViper(parent)}.$field"
        case TInSet(l, r) =>
          s"${expressionToViper(l)} in ${expressionToViper(r)}"
    case exp =>
      throw new IllegalArgumentException(
        s"Expression $exp not allowed in Viper expressions!"
      )
