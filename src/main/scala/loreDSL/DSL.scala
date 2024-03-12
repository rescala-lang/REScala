package loreDSL

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.report
import dotty.tools.dotc.transform.{Pickler, Staging}

import scala.util.matching.Regex

class DSL extends StandardPlugin:
  val name: String = "DSL"
  val description: String = "Constructs a LoRe AST from the given Scala AST"

  override def init(options: List[String]): List[PluginPhase] =
    (new DSLPhase) :: Nil
end DSL

class DSLPhase extends PluginPhase:
  import tpd.*
  val phaseName: String = "DSL"

  override val runsAfter: Set[String] = Set(Pickler.name)
  override val runsBefore: Set[String] = Set(Staging.name)

  private var loreTerms: List[Term] = List()
  private val loreReactives: List[String] = List(
    "rescala.default.Var", // Source
    "rescala.default.Signal", // Derived
    "lore.DSL.InteractionWithTypes",
    "lore.DSL.InteractionWithRequires",
    "lore.DSL.InteractionWithRequiresAndModifies"
  )
  private val reactiveSourcePattern: Regex = """rescala\.default\.Var\[(\w+)\]""".r
  private val reactiveDerivedPattern: Regex = """rescala\.default\.Signal\[(\w+)\]""".r
  private val reactiveInteractionPattern: Regex = """lore\.DSL\.InteractionWithTypes\[(\w+), (\w+)\]""".r

  override def transformValDef(tree: tpd.ValDef)(using Context): tpd.Tree =
    tree match
      // Match value definitions for base types Int, String and Boolean, as these also exist in LoRe
      // and are likely to be referenced in the definition of LoRe reactives (specifically Sources)
      case ValDef(name, tpt, Literal(Constant(num: Int))) if tpt.tpe <:< defn.IntType =>
        // TODO: Values other than simple int literals (operators, as seen below)
        println(s"Detected Int definition with name \"$name\" and value $num, adding to term list")
        loreTerms = loreTerms :+ TAbs(
          name.toString,
          SimpleType("Int", List()),
          TNum(num)
        )
      case ValDef(name, tpt, Literal(Constant(str: String))) if tpt.tpe <:< defn.StringType =>
        println(s"Detected String definition with name \"$name\" and value \"$str\", adding to term list")
        loreTerms = loreTerms :+ TAbs(
          name.toString,
          SimpleType("String", List()),
          TString(str)
        )
      case ValDef(name, tpt, Literal(Constant(bool: Boolean))) if tpt.tpe <:< defn.BooleanType =>
        // TODO: Values other than simple true and false literals (operators, as seen below)
        println(s"Detected Boolean definition with name \"$name\" and value $bool, adding to term list")
        loreTerms = loreTerms :+ TAbs(
          name.toString,
          SimpleType("String", List()),
          if (bool) TTrue() else TFalse()
        )
      // Match ValDefs for LoRe reactives (Source, Derived, Interaction)
      case ValDef(name, tpt, rhs) if loreReactives.exists(t => tpt.tpe.show.startsWith(t)) =>
        // Match which reactive it actually is, and what its type arguments are
        tpt.tpe.show match
          case reactiveSourcePattern(typeArg) =>
            println(s"Detected Source definition with $typeArg type parameter")
            typeArg match
              case "Int" =>
                rhs match
                  // Source gets inlined to Rescala Var, so this needs to match for Inlined first of all
                  // Additionally, because the RHS is wrapped in a Source, there's one layer of an Apply call
                  // wrapping the RHS we want, and the actual RHS tree we want is inside the Apply parameter list
                  // This parameter list always has length 1, because Sources only have 1 parameter ("Source(foo)", not "Source(foo, bar)")
                  case Inlined(Apply(_, List(properRhs)), _, _) =>
                    // Match for different kinds of RHS, like value literals or operator applications
                    // Typechecking for whether the arguments are actually of the expected type (in this case integers)
                    // is already handled by the type-checker prior to this, so we can just assume it's correct at this point
                    properRhs match
                      case Literal(Constant(literalValue: Int)) => // E.g. "foo: Source[Int] = Source(0)"
                        println(s"Adding Source reactive with name \"$name\" and literal int value $literalValue to term list")
                        loreTerms = loreTerms :+ TAbs(
                          name.toString, // foo
                          SimpleType("Source", List(SimpleType("Int", List()))), // Source[Int]
                          TSource(TNum(literalValue)) // Source(literalValue)
                        )
                      case Ident(referenceName: Name) => // E.g. "foo: Source[Int] = Source(bar)" where "bar" is a reference
                        println(s"Adding Source reactive with name \"$name\" and variable reference to \"$referenceName\" to term list")
                        // No need to check whether the reference specified here actually exists, because if it didn't
                        // then the original Scala code would not have compiled due to invalid reference and this
                        // point would not have been reached either way, so just pass on the reference name to a TVar
                        loreTerms = loreTerms :+ TAbs(
                          name.toString, // foo
                          SimpleType("Source", List(SimpleType("Int", List()))), // Source[Int]
                          TSource(TVar(referenceName.toString)) // Source(referenceName)
                        )
                      case Apply(Select(leftArg, op), List(rightArg)) => // E.g. "foo: Source[Int] = Source(4 ◯ 2")
                        println(s"Adding Source reactive with name \"$name\" and the form \"left $op right\" to term list")
                        // Set left and right value terms depending on if they are literals or references
                        val leftValueTerm: Term = leftArg match
                          case Literal(Constant(leftValue: Int)) =>
                            println(s"The left operand is the literal value $leftValue")
                            TNum(leftValue)
                          case Ident(leftReference: Name) =>
                            println(s"The left operand is a reference to \"$leftReference\"")
                            TVar(leftReference.toString)
                          // TODO: If the value is not a simple literal or reference, recurse to build term
                          // TODO: Fix the case and output value here when implementing the above
                          case _ =>
                            TNum(0)
                        val rightValueTerm: Term = rightArg match
                          case Literal(Constant(rightValue: Int)) =>
                            println(s"The right operand is the literal value $rightValue")
                            TNum(rightValue)
                          case Ident(rightReference: Name) =>
                            println(s"The right operand is a reference to \"$rightReference\"")
                            TVar(rightReference.toString)
                          // TODO: If the value is not a simple literal or reference, recurse to build term
                          // TODO: Fix the case and output value here when implementing the above
                          case _ =>
                            TNum(0)
                        // Create wrapper terms depending on used operator
                        op match
                          case nme.ADD => // E.g. "foo: Source[Int] = Source(2 + 3)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString, // foo
                              SimpleType("Source", List(SimpleType("Int", List()))), // Source[Int]
                              TSource(TAdd(leftValueTerm, rightValueTerm)) // Source(left + right)
                            )
                          case nme.SUB => // E.g. "foo: Source[Int] = Source(2 - 3)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString, // foo
                              SimpleType("Source", List(SimpleType("Int", List()))), // Source[Int]
                              TSource(TSub(leftValueTerm, rightValueTerm)) // Source(left - right)
                            )
                          case nme.MUL => // E.g. "foo: Source[Int] = Source(2 * 3)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString, // foo
                              SimpleType("Source", List(SimpleType("Int", List()))), // Source[Int]
                              TSource(TMul(leftValueTerm, rightValueTerm)) // Source(left * right)
                            )
                          case nme.DIV => // E.g. "foo: Source[Int] = Source(2 / 3)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString, // foo
                              SimpleType("Source", List(SimpleType("Int", List()))), // Source[Int]
                              TSource(TDiv(leftValueTerm, rightValueTerm)) // Source(left / right)
                            )
                          case _ => // Unsupported binary operator
                            report.error(s"Unsupported binary operator used: $op", tree.sourcePos)
                      case _ => // Unsupported type of integer RHS
                        report.error(s"Unsupported RHS: $rhs", tree.sourcePos)
                  case _ => () // Anything that's not Inlined and not wrapped with Source, can't happen because of the type checker
              case "String" =>
                rhs match
                  // Source gets inlined to Rescala Var, so this needs to match for Inlined first of all
                  // Additionally, because the RHS is wrapped in a Source, there's one layer of an Apply call
                  // wrapping the RHS we want, and the actual RHS tree we want is inside the Apply parameter list
                  // This parameter list always has length 1, because Sources only have 1 parameter ("Source(foo)", not "Source(foo, bar)")
                  case Inlined(Apply(_, List(properRhs)), _, _) =>
                    // Match for different kinds of RHS, like value literals or operator applications
                    // Typechecking for whether the arguments are actually of the expected type (in this case strings)
                    // is already handled by the type-checker prior to this, so we can just assume it's correct at this point
                    properRhs match
                      case Literal(Constant(value: String)) => // E.g. "foo: Source[String] = Source("abc")"
                        println(s"Adding Source reactive with name \"$name\" and string value $value to term list")
                        loreTerms = loreTerms :+ TAbs(
                          name.toString, // foo
                          SimpleType("Source", List(SimpleType("String", List()))), // Source[String]
                          TSource(TString(value))) // Source(value) where value is a string
                      case Ident(referenceName: Name) => // E.g. "foo: Source[String] = Source(bar)" where "bar" is a reference
                        println(s"Adding Source reactive with name \"$name\" and variable reference to \"$referenceName\" to term list")
                        // No need to check whether the reference specified here actually exists, because if it didn't
                        // then the original Scala code would not have compiled due to invalid reference and this
                        // point would not have been reached either way, so just pass on the reference name to a TVar
                        loreTerms = loreTerms :+ TAbs(
                          name.toString, // foo
                          SimpleType("Source", List(SimpleType("String", List()))), // Source[String]
                          TSource(TVar(referenceName.toString)) // Source(referenceName)
                        )
                      case _ => // Unsupported type of string RHS
                        report.error(s"Unsupported RHS: $rhs", tree.sourcePos)
                  case _ => () // Anything that's not Inlined and not wrapped with Source, can't happen because of the type checker
              case "Boolean" =>
                rhs match
                  // Source gets inlined to Rescala Var, so this needs to match for Inlined first of all
                  // Additionally, because the RHS is wrapped in a Source, there's one layer of an Apply call
                  // wrapping the RHS we want, and the actual RHS tree we want is inside the Apply parameter list
                  // This parameter list always has length 1, because Sources only have 1 parameter ("Source(foo)", not "Source(foo, bar)")
                  case Inlined(Apply(_, List(properRhs)), _, _) =>
                    // Match for different kinds of RHS, like value literals or operator applications
                    // Typechecking for whether the arguments are actually of the expected type (in this case booleans)
                    // is already handled by the type-checker prior to this, so we can just assume it's correct at this point
                    properRhs match
                      case Literal(Constant(literalValue: Boolean)) => // E.g. "foo: Source[Boolean] = Source(true)"
                        println(s"Adding Source reactive with name \"$name\" and boolean value $literalValue to term list")
                        loreTerms = loreTerms :+ TAbs(
                          name.toString, // foo
                          SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                          TSource(if (literalValue) TTrue() else TFalse()) // Source(literalValue)
                        )
                      case Ident(referenceName: Name) => // E.g. "foo: Source[Boolean] = Source(bar)" where "bar" is a reference
                        println(s"Adding Source reactive with name \"$name\" and variable reference to \"$referenceName\" to term list")
                        // No need to check whether the reference specified here actually exists, because if it didn't
                        // then the original Scala code would not have compiled due to invalid reference and this
                        // point would not have been reached either way, so just pass on the reference name to a TVar
                        loreTerms = loreTerms :+ TAbs(
                          name.toString, // foo
                          SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                          TSource(TVar(referenceName.toString)) // Source(referenceName)
                        )
                      case Select(arg, op) => // E.g. "Source(◯true)" (unary operators)
                        println(s"Adding Source reactive with name \"$name\" and unary operator boolean form ${op}<operand> to term list")
                        // Set operand term depending on if it is a literal or reference
                        val unaryTerm: Term = arg match
                          case Literal(Constant(literalValue: Boolean)) =>
                            println(s"The operand is the literal value $literalValue")
                            if (literalValue) TTrue() else TFalse()
                          case Ident(referenceName: Name) =>
                            println(s"The operand is a reference to \"$referenceName\"")
                            TVar(referenceName.toString)
                          // TODO: If the value is not a simple literal or reference, recurse to build term
                          // TODO: Fix the case and output value here when implementing the above
                          case _ =>
                            TTrue()
                        op match
                          // This specifically has to be nme.UNARY_! and not e.g. nme.NOT
                          case nme.UNARY_! => // E.g. "foo: Source[Boolean] = Source(!false)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString, // foo
                              SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                              TSource(TNeg(unaryTerm)) // Source(!value)
                            )
                          case _ => // Unsupported unary operator
                            report.error(s"Unsupported unary boolean operator used: $op", tree.sourcePos)
                      case Apply(Select(leftArg, op), List(rightArg)) => // E.g. "foo: Source[Boolean] = Source(true ◯ false)" or "Source(3 ◯ 9)"
                        println(s"Adding Source reactive with name \"$name\" and the form \"left $op right\" to term list")
                        // Set left and right value terms depending on if they are literals or references
                        val leftValueTerm: Term = leftArg match
                          case Literal(Constant(leftValue: Boolean)) =>
                            println(s"The left operand is the literal bool value $leftValue")
                            if (leftValue) TTrue() else TFalse()
                          case Literal(Constant(leftValue: Int)) =>
                            println(s"The left operand is the literal int value $leftValue")
                            TNum(leftValue)
                          case Ident(leftReference: Name) =>
                            println(s"The left operand is a reference to \"$leftReference\"")
                            TVar(leftReference.toString)
                          // TODO: If the value is not a simple literal or reference, recurse to build term
                          // TODO: Fix the case and output value here when implementing the above
                          case _ =>
                            TNum(0)
                        val rightValueTerm: Term = rightArg match
                          case Literal(Constant(rightValue: Boolean)) =>
                            println(s"The right operand is the literal bool value $rightValue")
                            if (rightValue) TTrue() else TFalse()
                          case Literal(Constant(rightValue: Int)) =>
                            println(s"The right operand is the literal int value $rightValue")
                            TNum(rightValue)
                          case Ident(rightReference: Name) =>
                            println(s"The right operand is a reference to \"$rightReference\"")
                            TVar(rightReference.toString)
                          // TODO: If the value is not a simple literal or reference, recurse to build term
                          // TODO: Fix the case and output value here when implementing the above
                          case _ =>
                            TNum(0)
                        op match
                          // Important: nme.AND is & and nme.And is &&
                          case nme.And => // E.g. "foo: Source[Boolean] = Source(true && true)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString, // foo
                              SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                              TSource(TConj(leftValueTerm, rightValueTerm)) // Source(left && right)
                            )
                          // Important: nme.OR is | and nme.Or is ||
                          case nme.Or => // E.g. "foo: Source[Boolean] = Source(true || false)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString, // foo
                              SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                              TSource(TDisj(leftValueTerm, rightValueTerm)) // Source(left || right)
                            )
                          case nme.LT => // E.g. "foo: Source[Boolean] = Source(1 < 2)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString, // foo
                              SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                              TSource(TLt(leftValueTerm, rightValueTerm)) // Source(left < right)
                            )
                          case nme.GT => // E.g. "foo: Source[Boolean] = Source(2 > 1)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString, // foo
                              SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                              TSource(TGt(leftValueTerm, rightValueTerm)) // Source(left > right)
                            )
                          case nme.LE => // E.g. "foo: Source[Boolean] = Source(2 <= 1)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString, // foo
                              SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                              TSource(TLeq(leftValueTerm, rightValueTerm)) // Source(left <= right)
                            )
                          case nme.GE => // E.g. "foo: Source[Boolean] = Source(2 >= 1)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString, // foo
                              SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                              TSource(TGeq(leftValueTerm, rightValueTerm)) // Source(left >= right)
                            )
                          case nme.EQ => // E.g. "foo: Source[Boolean] = Source(2 == 1)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString, // foo
                              SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                              TSource(TEq(leftValueTerm, rightValueTerm)) // Source(left == right)
                            )
                          case nme.NE => // E.g. "foo: Source[Boolean] = Source(2 != 1)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString, // foo
                              SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                              TSource(TIneq(leftValueTerm, rightValueTerm)) // Source(left != right)
                            )
                          case _ => // Unsupported binary boolean operator
                            report.error(s"Unsupported binary operator with bool outcome used: $op", tree.sourcePos)
                      case _ => // Unsupported type of boolean RHS
                        report.error(s"Unsupported RHS: $rhs", tree.sourcePos)
                  case _ => () // Anything that's not Inlined and not wrapped with Source, can't happen because of the type checker
              case _ => // Unsupported LHS type argument
                report.error(s"Unsupported LHS type parameter used: $typeArg", tree.sourcePos)
          case reactiveDerivedPattern(typeArg) =>
            println(s"Detected Derived definition with $typeArg type parameter")
            // TODO: Implement
          case reactiveInteractionPattern(typeArg1, typeArg2) =>
            println(s"Detected Interaction definition with $typeArg1 and $typeArg2 type parameters")
            // TODO: Implement
      case _ => () // All other ValDefs not covered above are ignored
      tree // Return the original tree to further compiler phases
  end transformValDef
end DSLPhase
