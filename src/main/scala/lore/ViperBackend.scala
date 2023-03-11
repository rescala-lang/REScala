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
    // step 0: insert type aliases
    def replaceType: Type => Type =
      case SimpleType(name, List()) if ctx.typeAliases.contains(name) =>
        ctx.typeAliases(name)
      case SimpleType(name, inner) => SimpleType(name, inner.map(replaceType))
      case TupleType(inner)        => TupleType(inner.map(replaceType))
    def insertTypes: Term => Term =
      case TArgT(name, _type) => TArgT(name, replaceType(_type))
      case TAbs(name, _type, body) =>
        TAbs(name, replaceType(_type), body)
      case TInteraction(rt, at, m, r, en, ex) =>
        TInteraction(replaceType(rt), replaceType(at), m, r, en, ex)
      case t => t
    val ctx1 =
      ctx.copy(ast = ctx.ast.map(traverseFromNode(_, insertTypes)))
    // TODO: step 0: find field calls and function calls that use anonymous functions as arguments and transform them to synthetic functions
    // step 1: field calls as function calls
    def fieldCallToFunCall: Term => Term =
      case TFCall(parent, field, args) =>
        TFunC(field, parent +: args)
      case t => t
    val ctx2 =
      ctx1.copy(ast = ctx1.ast.map(traverseFromNode(_, fieldCallToFunCall)))

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
      // only compile interactions that modify some reactives and have some guarantees or affect an invariant
      .filter((_, i) =>
        !i.modifies.isEmpty &&
          (!i.ensures.isEmpty || !OverlapAnalysis
            .overlappingInvariants(i)(using ctx)
            .isEmpty)
      )
      .map(interactionToMethod(_, _)(using ctx))
      .toList
    return (ctx, interactions)

  private def interactionToMethod(
      name: ID,
      interaction: TInteraction
  )(using ctx: CompilationContext): String =
    // collect arguments and their types
    def countTypes: Type => Int =
      case SimpleType(_, _) => 1
      case TupleType(inner) => inner.length
    val numReactiveTypes = countTypes(interaction.reactiveType)
    // extract names from body
    val allNames = interaction.executes match
      case None                      => List()
      case Some(term @ TArrow(_, _)) => term.args
      case Some(term)                => List()
    val argNames =
      allNames.drop(numReactiveTypes)
    val argTypes: List[String] =
      interaction.argumentType match
        case s @ SimpleType(_, _) => List(typeToViper(s)(using ctx))
        case TupleType(inner)     => inner.toList.map(typeToViper(_)(using ctx))
    if argNames.length != argTypes.length then
      throw ViperCompilationException(
        s"Tried to compile interaction but number of arguments does not match interaction body (expected ${argTypes.length} based on type signature but got ${argNames.length}). argTypes: $argTypes, argnames: $argNames"
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

    def isAssertAssume: Term => Boolean =
      case TAssert(_) => true
      case TAssume(_) => true
      case _          => false

    def bodyAssignment(lastExpression: Term): String =
      lastExpression match
        case tuple @ TTuple(factors) =>
          if (factors.length != interaction.modifies.length) then
            throw ViperCompilationException(
              s"Interaction $name has invalid executes part. Expected tuple with ${interaction.modifies.length} entries as result but only ${factors.length} were given: $tuple"
            )
          (interaction.modifies zip factors.toList)
            .map((reactive, assignment) =>
              s"graph.$reactive := ${expressionToViper(assignment)}"
            )
            .mkString("\n")
        case t =>
          if interaction.modifies.length != 1 then
            throw ViperCompilationException(
              s"Interaction $name has invalid executes part. Expected tuple with ${interaction.modifies.length} entries as result but only a simple value was given: $t"
            )
          s"graph.${interaction.modifies.head} := ${expressionToViper(t)}"

    val bodyCompiled: Option[String] =
      body.map {
        case TSeq(sequence) =>
          // ignore assertions and assumptions when looking for last expression
          val inner: List[Term] =
            sequence.reverse.dropWhile_(isAssertAssume).reverse
          val end: List[Term] =
            sequence.reverse.takeWhile_(isAssertAssume).reverse

          val assignment = inner.last

          ((inner
            .take(inner.length - 1)
            .map(expressionToViper) :+
            bodyAssignment(assignment)) ++
            end.map(expressionToViper)).mkString("\n")
        case t => bodyAssignment(t)
        // case t => (List(), t)
      }

    // collect read/write permissions
    // collect explicitly mentioned source reactives
    val mentioned = uses(interaction).filter(ctx.sources.keySet.contains)
    val writes = interaction.modifies
    val reads =
      (overlappingInvariants.flatMap(
        ctx.reactivesPerInvariant(_)
      ) ++ mentioned) -- writes

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
        |${bodyCompiled.getOrElse("").indent(2)}} 
        |""".stripMargin

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
      case SimpleType(name, Nil) => name
      case SimpleType("Source", inner) =>
        s"${inner.map(typeToViper).mkString(", ")}"
      case SimpleType(name, inner) =>
        s"$name[${inner.map(typeToViper).mkString(", ")}]"
      case TupleType(inner) =>
        s"(${inner.toList.map(typeToViper).mkString(", ")})"

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
              setOp,
              x :: y :: Nil
            )
            if Set("setminus", "union", "subset", "intersection").contains(
              setOp
            ) => // handle set arithmetics.
          s"${expressionToViper(x)} $setOp ${expressionToViper(y)}"
        case TFunC(
              name,
              x :: Nil
            )
            if name == "size" || name == "length" => // handle set arithmetics. TODO: support more cases
          s"|${expressionToViper(x)}|"
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
        case TSeq(body)    => body.map(expressionToViper).toList.mkString("\n")
        case TAssert(body) => s"assert ${expressionToViper(body)}"
        case TAssume(body) => s"assume ${expressionToViper(body)}"
    case exp =>
      throw new IllegalArgumentException(
        s"Expression $exp not allowed in Viper expressions!"
      )
