package lore.backends

import lore.AST._
import cats.implicits._

object ViperBackend:
  case class ViperCompilationException(message: String)
      extends Exception(message)

  case class ViperCompilationResult(
      ctx: CompilationContext,
      sources: Seq[String],
      derived: Seq[String],
      invariants: Seq[String],
      interactions: Seq[(String, String)]
  )

  /** Compiles a given ast to Viper. This function returns a list of (filename,
    * sourcecode) tuples in order to allow compilation to multiple target files.
    *
    * @param ast
    * @return
    */
  def compileAsSeparateFiles(
      ast: Seq[Term],
      benchMarkMode: Boolean = false
  ): Seq[(String, String)] =
    val res = toViper(ast)
    res.interactions.map((name, code) =>
      (
        name,
        s"""|// imports
            |${res.ctx.viperImports
             .map(i => s"import \"${i.path.toString.replace("\\", "/")}\"")
             .mkString("\n")}
            |// sources
            |${res.sources.mkString("\n")}
            |// derived
            |${res.derived.mkString("\n")}
            |// invariants
            |${res.invariants.mkString("\n")}
            |
            |$code
            |""".stripMargin
      )
    )

  def compileAsSingleFile(ast: Seq[Term]): String =
    val res = toViper(ast)

    s"""|// imports
          |${res.ctx.viperImports
         .map(i => s"import \"${i.path.toString}\"")
         .mkString("\n")}
          |// sources
          |${res.sources.mkString("\n")}
          |// derived
          |${res.derived.mkString("\n")}
          |// invariants
          |${res.invariants.mkString("\n")}
          |
          |// interactions
          |${res.interactions.map(_._2).mkString("\n")}
          |""".stripMargin

  def toViper(
      ast: Seq[Term]
  ): ViperCompilationResult =
    //// step 1: build context object that we pass around to subfunctions
    val ctx = flattenInteractions(CompilationContext(ast))

    //// step 2: Viper specific AST transformations
    val ctxy = viperTransformations(ctx)

    //// step 3: compile source reactives
    val (ctx3, sourcesCompiled) = compileSources(ctxy)

    //// step 4: compile derived reactives
    val (ctx4, derivedCompiled) = compileDerived(ctx3)

    //// step 5: compile invariants
    val (ctx5, invariantsCompiled) = compileInvariants(ctx4)

    //// step 6: compile interactions
    val (ctx6, interactionsCompiled) = compileInteractions(ctx5)

    return ViperCompilationResult(
      ctx6,
      sourcesCompiled,
      derivedCompiled,
      invariantsCompiled,
      interactionsCompiled
    )

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
      case t: TArgT => TArgT(t.name, replaceType(t._type))
      case t: TAbs =>
        TAbs(t.name, replaceType(t._type), t.body)
      case TInteraction(rt, at, m, r, en, ex, _) =>
        TInteraction(replaceType(rt), replaceType(at), m, r, en, ex)
      case t => t
    val ctx1 =
      ctx.copy(ast = ctx.ast.map(traverseFromNode(_, insertTypes)))
    // step 1: field calls as function calls
    def fieldCallToFunCall: Term => Term =
      case t: TFCall =>
        TFunC(t.field, t.parent +: t.args)
      case t => t
    val ctx2 =
      ctx1.copy(ast = ctx1.ast.map(traverseFromNode(_, fieldCallToFunCall)))

    // step 2: turn references to derived reactives to macro calls
    def transformer: Term => Term =
      case t: TVar if ctx2.derived.contains(t.name) =>
        val (derived, _) = ctx2.graph(t.name)
        val usedReactives =
          uses(derived).filter(r => ctx2.graph.keys.toSet.contains(r))
        TFunC(t.name, usedReactives.toList.sorted.map(TVar(_)))
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

  private def compileInteractions(
      ctx: CompilationContext
  ): (CompilationContext, List[(String, String)]) =
    val interactions = ctx.interactions
      // only compile interactions that modify some reactives and have some guarantees or affect an invariant
      .filter((_, i) =>
        !i.modifies.isEmpty &&
          (!i.ensures.isEmpty || !OverlapAnalysis
            .overlappingInvariants(i)(using ctx)
            .isEmpty)
      )
      .map((name, i) => (name, interactionToMethod(name, i)(using ctx)))
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
      case None                         => List()
      case Some(term @ TArrow(_, _, _)) => term.args
      case Some(term)                   => List()
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
        .filter((inv, _) => overlappingInvariants.contains(inv))
        .map((inv, num) =>
          s"inv_$num(${ctx.reactivesPerInvariant(inv).toList.sorted.map("graph." + _).mkString(", ")})"
        )

    val body =
      interaction.executes.map(insertArgs(interaction.modifies, argNames, _))

    def isAssertAssume: Term => Boolean =
      case t: TAssert => true
      case t: TAssume => true
      case _          => false

    def bodyAssignment(lastExpression: Term): String =
      lastExpression match
        case t: TTuple =>
          if (t.factors.length != interaction.modifies.length) then
            throw ViperCompilationException(
              s"Interaction $name has invalid executes part. Expected tuple with ${interaction.modifies.length} entries as result but only ${t.factors.length} were given: $t"
            )
          (interaction.modifies zip t.factors.toList)
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
        case t: TSeq =>
          // ignore assertions and assumptions when looking for last expression
          val inner: List[Term] =
            t.body.reverse.dropWhile_(isAssertAssume).reverse
          val end: List[Term] =
            t.body.reverse.takeWhile_(isAssertAssume).reverse

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
      case t: TArrow =>
        // extract argument names
        val allNames = t.args
        val (reactiveNames, argumentNames) = allNames.splitAt(modifies.length)
        // insert reactives names
        val reactivesInserted =
          (reactiveNames zip modifies).foldLeft(t.body) {
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
      case t: TVar if ctx.sources.contains(t.name) =>
        TFCall(TVar("graph"), t.name, List())
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
        case b: BinaryOp =>
          val operator: String = b match
            case t: TDiv   => "/"
            case t: TMul   => "*"
            case t: TAdd   => "+"
            case t: TSub   => "-"
            case t: TLt    => "<"
            case t: TGt    => ">"
            case t: TLeq   => "<="
            case t: TGeq   => ">="
            case t: TEq    => "=="
            case t: TIneq  => "!="
            case t: TDisj  => "||"
            case t: TConj  => "&&"
            case t: TImpl  => "==>"
            case t: TBImpl => "<==>"
            case t: TInSet => "in"
          s"${expressionToViper(b.left)} $operator ${expressionToViper(b.right)}"
        case t: TVar   => t.name
        case t: TTrue  => "true"
        case t: TFalse => "false"
        case t: TNeg   => s"!${expressionToViper(t.body)}"
        case t: TQuantifier =>
          val varString =
            t.vars
              .map { case a: TArgT => s"${a.name}: ${typeToViper(a._type)}" }
              .toList
              .mkString(", ")
          val (keyword, triggers) = t match
            case f: TForall =>
              (
                "forall",
                s"{${f.triggers.map(expressionToViper).mkString(", ")}}"
              )
            case e: TExists => ("exists", "")
          s"$keyword $varString :: $triggers ${expressionToViper(t.body)}"
        case t: TNum    => t.value.toString
        case t: TParens => s"(${expressionToViper(t.inner)})"
        case TFunC(
              setOp,
              x :: y :: Nil,
              _
            )
            if Set("setminus", "union", "subset", "intersection").contains(
              setOp
            ) => // handle set arithmetics.
          s"${expressionToViper(x)} $setOp ${expressionToViper(y)}"
        case TFunC(
              name,
              x :: Nil,
              _
            )
            if name == "size" || name == "length" => // handle set arithmetics. TODO: support more cases
          s"|${expressionToViper(x)}|"
        case t: TFunC =>
          val argsString = t.args.map(a => expressionToViper(a)).mkString(", ")
          s"${t.name}($argsString)"
        case t: TFCall if t.args.isEmpty => // field call
          s"${expressionToViper(t.parent)}.${t.field}"
        case t: TFCall => // field call
          throw new ViperCompilationException(
            s"Viper does only allow field but not method access! ${t.parent}.${t.field}(${t.args.mkString(", ")})"
          )
          s"${expressionToViper(t.parent)}.${t.field}"
        case t: TAbs =>
          s"""|var ${t.name}: ${typeToViper(t._type)}
              |${t.name} := ${expressionToViper(t.body)}""".stripMargin
        case t: TSeq    => t.body.map(expressionToViper).toList.mkString("\n")
        case t: TAssert => s"assert ${expressionToViper(t.body)}"
        case t: TAssume => s"assume ${expressionToViper(t.body)}"
    case exp =>
      throw new IllegalArgumentException(
        s"Expression $exp not allowed in Viper expressions!"
      )
