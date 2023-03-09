package lore

import AST._
import cats.implicits._

object ViperBackend:
  case class ViperCompilationException(message: String)
      extends Exception(message)

  def toViper(ast: Seq[Term]): String =
    //// step 1: build context object that we pass around to subfunctions
    val ctx = flattenInteractions(CompilationContext(ast))

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

    s"""|// imports
        |${ctx6.viperImports
         .map(i => s"import \"${i.path.toString}\"")
         .mkString("\n")}
        |// sources
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
    // TODO: step 0: find field calls and function calls that use anonymous functions as arguments and transform them to synthetic functions
    // step 1: field calls as function calls
    def fieldCallToFunCall: Term => Term =
      case TFCall(parent, field, args) =>
        TFunC(field, parent +: args)
      case t => t
    val ctx2 =
      ctx.copy(ast = ctx.ast.map(traverseFromNode(_, fieldCallToFunCall)))

    // step 2: turn references to derived reactives to macro calls
    def transformer: Term => Term =
      case TVar(name) if ctx2.derived.contains(name) =>
        val (derived, _) = ctx2.graph(name)
        val usedReactives =
          uses(derived).filter(r => ctx2.graph.keys.toSet.contains(r))
        TFunC(name, usedReactives.toList.sorted.map(TVar(_)))
      case t => t

    return ctx2.copy(ast = ctx2.ast.map(traverseFromNode(_, transformer)))

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
            inputs = ctx
              .reactivesPerInvariant(inv)
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
        s"requires ${expressionToViper(insertArgs(interaction.modifies, argNames, p))}"
      )
    val postconditions =
      interaction.ensures.map(p =>
        s"ensures ${expressionToViper(insertArgs(interaction.modifies, argNames, p))}"
      )

    val invariantsNumbered = ctx.invariants.zip(1 to ctx.invariants.length + 1)
    val overlappingInvariants =
      OverlapAnalysis.overlappingInvariants(interaction)
    val relevantInvariants: Seq[String] =
      invariantsNumbered
        .filter((inv, num) => overlappingInvariants.contains(inv))
        .map((inv, num) =>
          s"inv_$num(${ctx.reactivesPerInvariant(inv).toList.sorted.map("graph." + _).mkString(", ")})"
        )

    val body =
      interaction.executes.map(insertArgs(interaction.modifies, argNames, _))

    // FIXME: support tuple types
    val bodyAsAssignments =
      body.map(b =>
        s"graph.${interaction.modifies.head} := ${expressionToViper(b)}"
      )

    val overlapppingSources =
      OverlapAnalysis
        .reaches(interaction)
        .filter(ctx.sources.keySet.contains(_))

    val writes = interaction.modifies
    val reads =
      overlappingInvariants.flatMap(ctx.reactivesPerInvariant(_)) -- writes

    s"""|method $name (
        |  // graph
        |  graph: Ref,
        |  // arguments
        |${argsString.mkString(",\n").indent(2)})
        |returns ()
        |// permissions
        |${writes.toList.sorted
         .map(id => s"requires acc(graph.$id)")
         .mkString("\n")}
        |${reads.toList.sorted
         .map(id => s"requires acc(graph.$id, 1/2)")
         .mkString("\n")}
        |// preconditions
        |${preconditions.mkString("\n")}
        |// relevant invariants
        |${relevantInvariants.map("requires " + _).mkString("\n")}
        |// permissions
        |${writes.toList.sorted
         .map(id => s"ensures acc(graph.$id)")
         .mkString("\n")}
        |${reads.toList.sorted
         .map(id => s"ensures acc(graph.$id, 1/2)")
         .mkString("\n")}
        |// postconditions
        |${postconditions.mkString("\n")}
        |// relevant invariants
        |${relevantInvariants.map("ensures " + _).mkString("\n")}
        |{
        |${bodyAsAssignments.getOrElse("").indent(2)}}
        """.stripMargin

  private def insertArgs(
      modifies: List[ID],
      methodArgs: List[ID],
      term: Term
  )(using ctx: CompilationContext): Term =
    // check if term is a function, if yes, insert reactives and method arguments
    val transformed = term match
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
            s"Interaction body $term has wrong arity. It should accept the same number of arguments as the executes part of the interaction: $methodArgs"
          )
        (argumentNames zip methodArgs).foldLeft(reactivesInserted) {
          case (body, (from, to)) => rename(from, to, body)
        }
      case _ => // no function, use body as is
        term

    // replace reactive names with field accesses on the graph object
    def transformer: Term => Term =
      case TVar(name) if ctx.sources.contains(name) =>
        TFCall(TVar("graph"), name, List())
      case t => t

    return traverseFromNode(transformed, transformer)

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
          s"${expressionToViper(l)} != ${expressionToViper(r)}"
        case TDisj(l, r) =>
          s"${expressionToViper(l)} || ${expressionToViper(r)}"
        case TConj(l, r) =>
          s"${expressionToViper(l)} && ${expressionToViper(r)}"
        case TBImpl(l, r) =>
          s"${expressionToViper(l)} <==> ${expressionToViper(r)}"
        case TImpl(l, r) =>
          s"${expressionToViper(l)} ==> ${expressionToViper(r)}"
        case TLt(l, r) =>
          s"${expressionToViper(l)} < ${expressionToViper(r)}"
        case TGt(l, r) =>
          s"${expressionToViper(l)} > ${expressionToViper(r)}"
        case TLeq(l, r) =>
          s"${expressionToViper(l)} <=${expressionToViper(r)}"
        case TGeq(l, r) =>
          s"${expressionToViper(l)} >= ${expressionToViper(r)}"
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
        case TAbs(name, _type, body) =>
          s"""|var $name: ${typeToViper(_type)}
              |$name := ${expressionToViper(body)}""".stripMargin
        case TSeq(body) => body.map(expressionToViper).toList.mkString("\n")
    case exp =>
      throw new IllegalArgumentException(
        s"Expression $exp not allowed in Viper expressions!"
      )
