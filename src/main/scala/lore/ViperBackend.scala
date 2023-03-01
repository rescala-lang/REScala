package lore

import AST._
import cats.implicits._

object ViperBackend:
  case class ViperCompilationException(message: String)
      extends Exception(message)

  def toViper(ast: Seq[Term]): String =
    //// step 1: build context object that we pass around to subfunctions
    val ctx = CompilationContext(ast)

    //// new step: Viper specific AST transformations
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
    val ctx3 =
      ctx2.copy(ast = ctx2.ast.map(traverseFromNode(_, fieldCallToFunCall)))

    // step 3: TODO turn references to derived reactives to macro calls
    def transformer: Term => Term =
      case TVar(name) if ctx2.derived.contains(name) =>
        val (derived, _) = ctx2.graph(name)
        val usedReactives =
          uses(derived).filter(r => ctx2.graph.keys.toSet.contains(r))
        TFunC(name, usedReactives.toList.sorted.map(TVar(_)))
      case t => t

    // return ???
    return ctx3.copy(ast = ctx3.ast.map(traverseFromNode(_, transformer)))

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
      .map(interactionToMethod(_, _)(using ctx))
      .toList
    return (ctx, interactions)

  private def interactionToMethod(
      name: ID,
      interaction: TInteraction
  )(using ctx: CompilationContext): String =
    // collect arguments and their types
    // extract names from body
    val allNames = interaction.executes match
      case None                      => List()
      case Some(term @ TArrow(_, _)) => term.args
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

    val preconditions =
      interaction.requires.map(p =>
        s"requires ${curlyToViper(interaction.modifies, argNames, p)}"
      )
    val postconditions =
      interaction.ensures.map(p =>
        s"ensures ${curlyToViper(interaction.modifies, argNames, p)}"
      )
    val body =
      interaction.executes.map(b =>
        s"${curlyToViper(interaction.modifies, argNames, b)}"
      )

    s"""|method $name (
        |  // graph
        |  graph: Ref,
        |  // arguments
        |${argsString.mkString(",\n").indent(2)})
        |returns ()
        |// preconditions
        |${preconditions.mkString("\n")}
        |// TODO relevant invariants
        |// postconditions
        |${postconditions.mkString("\n")}
        |// TODO relevant invariants
        |{
        |${body.getOrElse("").indent(2)}}
        """.stripMargin

  private def curlyToViper(
      modifies: List[ID],
      methodArgs: List[ID],
      body: Term
  )(using ctx: CompilationContext): String =
    // check if curlyBody is a function, if yes, insert reactives and method arguments
    val transformedBody = body match
      case tarrow @ TArrow(_, _) =>
        // extract argument names
        val allNames = tarrow.args
        val (reactiveNames, argumentNames) = allNames.splitAt(modifies.length)
        // insert reactives names
        val reactivesInserted =
          (reactiveNames zip modifies).foldLeft(tarrow.body) {
            case (body: Term, (from: ID, to: ID)) => rename(from, to, body)
          }
        // insert method args
        if argumentNames.length != methodArgs.length then
          throw ViperCompilationException(
            s"Interaction body $body has wrong arity. It should accept the same number of arguments as the executes part of the interaction: $methodArgs"
          )
        (argumentNames zip methodArgs).foldLeft(reactivesInserted) {
          case (body, (from, to)) => rename(from, to, body)
        }
      case _ => // no function, use body as is
        body

    // replace reactive names with field accesses on the graph object
    def transformer: Term => Term =
      case TVar(name) if ctx.sources.contains(name) =>
        TFCall(TVar("graph"), name, List())
      case t => t

    return expressionToViper(traverseFromNode(transformedBody, transformer))
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
        case TVar(id)    => id
        case TTrue       => "true"
        case TFalse      => "false"
        case TNeg(inner) => s"!${expressionToViper(inner)}"
        case TEq(l, r) => s"${expressionToViper(l)} == ${expressionToViper(r)}"
        case TIneq(l, r) =>
          s"${expressionToViper(l)}) != (${expressionToViper(r)}"
        case TDisj(l, r) =>
          s"${expressionToViper(l)}) || (${expressionToViper(r)}"
        case TConj(l, r) =>
          s"${expressionToViper(l)}) && (${expressionToViper(r)}"
        case TImpl(l, r) =>
          s"${expressionToViper(l)}) ==> (${expressionToViper(r)}"
        case TLt(l, r) =>
          s"${expressionToViper(l)}) < (${expressionToViper(r)}"
        case TGt(l, r) =>
          s"${expressionToViper(l)}) > (${expressionToViper(r)}"
        case TLeq(l, r) =>
          s"${expressionToViper(l)}) <=(${expressionToViper(r)}"
        case TGeq(l, r) =>
          s"${expressionToViper(l)}) >= (${expressionToViper(r)}"
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
