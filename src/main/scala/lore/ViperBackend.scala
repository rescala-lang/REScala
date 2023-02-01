package lore

import AST._
import cats.implicits._

object ViperBackend:
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

  def toViper(ast: Seq[Term]): String =
    //// step 1: build context object that we pass around to subfunctions
    val ctx = CompilationContext(ast)

    //// step 2: compile source reactives
    val (ctx2, sourcesCompiled) = compileSources(ctx)

    //// step 3: prepare macros and compile derived reactives
    // replace references to derived reactives with macro calls
    def derived2macro(term: Term): Term = term match
      case _var @ TVar(name) =>
        // check if is reference to derived reactive
        ctx2.graph.get(name).map(_._1) match
          case Some(d: TDerived) =>
            val args: Seq[TVar] = uses(d)
              // find every reactive that is used in d
              .filter(r => ctx2.graph.keys.toSet.contains(r))
              .toSeq
              .sorted
              .map(TVar(_))
            TFunC(name, args)
          case _ => _var
      case _ => term
    val ctx3 = ctx2.copy(ast = ctx.ast.map(traverseFromNode(_, derived2macro)))

    // compile derived reactives
    val (ctx4, derivedCompiled) = compileDerived(ctx3)

    //// step 4: compile invariants
    val (ctx5, invariantsCompiled) = compileInvariants(ctx4)

//     // step 4: compile transactions
//     val transactions: Seq[Transaction] = ast.collect({ case t: Transaction =>
//       t
//     })
//     val transactionsCompiled = transactions.map(transactionToMethods(ast, _))
    val transactionsCompiled = List("TODO: Transactions")

    s"""|// sources
          |${sourcesCompiled.mkString("\n")}
          |// derived
          |${derivedCompiled.mkString("\n")}
          |// invariants
          |${invariantsCompiled.mkString("\n")}
          |
          |// transactions
          |${transactionsCompiled.mkString("\n")}
          |""".stripMargin

  private def compileSources(ctx: CompilationContext): CompilationResult =
    def sourceToField(name: ID, _type: Type): String =
      s"field $name: ${typeToViper(_type)}"
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
      val body = expressionToViper(d.body)
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
      s"define inv_$id($inputsString) ${expressionToViper(invariant.condition)}"

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

  def typeToViper(t: Type): String =
    t match
      case SimpleType(name, Nil) => name
      case SimpleType("Source", inner) =>
        s"${inner.map(typeToViper).mkString(" ,")}"
      case SimpleType(name, inner) =>
        s"$name[${inner.map(typeToViper).mkString(" ,")}]"
      case TupleType(inner) =>
        s"(${inner.toList.map(typeToViper).mkString(" ,")})"

  // def transactionToMethods(ast: Seq[ParsedExpression], transaction: AST.Transaction): String =
  //     val argsString = transaction.args.map{
  //         case (name, Some(typeName)) => s"${name.name}: ${typeName.name}"
  //         case (name, None) => throw new Exception(s"Transaction $transaction needs type annotations to be compiled to Viper.")
  //     }

  //     val graph = allReactives(ast)

  //     // divide transaction into subtransactions
  //     val subtransactions: Seq[String] = transaction.reactives.map(names =>
  //         val reactives: Seq[Reactive] = names.map(n => graph.get(n.name) match
  //             case Some(r) => r
  //             case None => throw Exception(s"Reactive ${n.name} not found in graph."))
  //         val subGraph: Set[Reactive] = reactives.map(r => getDependants(r, ast))
  //             .toSet.flatten
  //         val subGraphNames: Set[String] = subGraph.map(r => r.name.name)

  //         val sourceReactives: Seq[SourceReactive] =
  //             reactives.collect({case s: SourceReactive => s})
  //         val derivedReactives: Seq[DerivedReactive] =
  //             subGraph.collect({case d: DerivedReactive => d}).toSeq

  //         val invariants: Set[Invariant] = ast.collect({
  //             case i: Invariant if !uses(i).intersect(subGraphNames).isEmpty => i}).toSet
  //         val invariantsStrings: Set[String] = invariants.map(i => expressionToViper(i.body))
  //         val invariantsAfter: Set[String] =
  //             invariantsStrings.map(s =>
  //                 derivedReactives.foldl(s){case (acc, derivedReactive) =>
  //                     acc.replaceAll(derivedReactive.name.name, s"${derivedReactive.name.name}_")}
  //             )

  //         val graphString = sourceReactives.map{
  //             case (SourceReactive(_, id, body, Some(typeAnn))) => s"${id.name}: $typeAnn"
  //             case (r @ SourceReactive(_, name, body, None)) => throw new Exception(s"Reactive $r needs type annotation to be compiled to Viper.")
  //         }
  //         val graphStringAfter = sourceReactives.map{
  //             case (SourceReactive(_, id, body, Some(typeAnn))) => s"${id.name}_: $typeAnn"
  //             case (r @ SourceReactive(_, name, body, None)) => throw new Exception(s"Reactive $r needs type annotation to be compiled to Viper.")
  //         }

  //         val preconditions = transaction.preconditions.map(p =>
  //             "requires " + expressionToViper(p.body))
  //         val postconditions = transaction.postconditions.map(p =>
  //             "ensures " + expressionToViper(p.body))

  //         s"""|method ${transaction.name.name}_${reactives.map(_.name.name).mkString("_")}(
  //             |  // graph state:
  //             |  ${graphString.mkString(",\n  ")},
  //             |  // transaction arguments:
  //             |  ${argsString.mkString(",\n  ")})
  //             |returns (
  //             |  ${graphStringAfter.mkString(",\n  ")}
  //             |)
  //             |// preconditions
  //             |${preconditions.mkString("\n")}
  //             |// relevant invariants
  //             |${invariantsStrings.map(i => "requires " + i).mkString("\n")}
  //             |// postconditions
  //             |${postconditions.mkString("\n")}
  //             |// relevant invariants
  //             |${invariantsAfter.map(i => "ensures " + i).mkString("\n")}
  //             |{
  //             |${expressionToViper(transaction.body)}
  //             |}
  //             |""".stripMargin
  //     )

  //     subtransactions.mkString("\n")

  def expressionToViper(expression: Term): String = expression match
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
        case TFunC(name, args) =>
          val argsString = args.map(a => expressionToViper(a)).mkString(", ")
          s"$name($argsString)"
        case TFCall(parent, field, args) => // field call
          val argsString = args.map(a => expressionToViper(a)).mkString(", ")
          s"${expressionToViper(parent)}.$field($argsString)"
        case TInSet(l, r) =>
          s"${expressionToViper(l)} in ${expressionToViper(r)}"
    case exp =>
      throw new IllegalArgumentException(
        s"Expression $exp not allowed in Viper expressions!"
      )
