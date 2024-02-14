package loreDSL

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.TermName
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
      // Only match ValDefs for LoRe reactives (Source, Derived, Interaction)
      case ValDef(name, tpt, rhs) if loreReactives.exists(t => tpt.tpe.show.startsWith(t)) =>
        // Match which reactive it actually is, and what its type arguments are
        tpt.tpe.show match
          case reactiveSourcePattern(typeArg) =>
            println(s"Detected Source definition with $typeArg type parameter")
            // Process the RHS of the definition
            typeArg match
              case "Int" =>
                rhs match
                  case Inlined(call, _, _) => // Source gets inlined to Rescala Var, so this needs to match for Inlined
                    // Match for different kinds of RHS, like value literals or operator applications
                    // The fun value of the (outer) Apply will be the type application of Source ("Source(x)" for x), so it can be ignored
                    call match
                      case Apply(_, List(Literal(Constant(value: Number)))) => // E.g. "Source(0)"
                        println(s"Adding Source reactive with number value $value to term list")
                        loreTerms = loreTerms :+ TAbs(
                          name.toString,
                          SimpleType("Source", List(SimpleType("Int", List()))), // Source[Int]
                          TSource(TNum(value)) // Source(value)
                        )
                      case Apply(_, List(Apply(Select(Literal(Constant(lhs: Number)), op), List(Literal(Constant(rhs: Number)))))) => // E.g. "Source(4 ◯ 2")
                        println(s"Adding Source reactive with integer value $lhs $op $rhs to term list")
                        op match
                          case nme.ADD => // E.g. "Source(2 + 3)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString,
                              SimpleType("Source", List(SimpleType("Int", List()))), // Source[Int]
                              TSource(TAdd(TNum(lhs), TNum(rhs))) // Source(lhs + rhs)
                            )
                          case nme.SUB => // E.g. "Source(2 - 3)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString,
                              SimpleType("Source", List(SimpleType("Int", List()))), // Source[Int]
                              TSource(TSub(TNum(lhs), TNum(rhs))) // Source(lhs - rhs)
                            )
                          case nme.MUL => // E.g. "Source(2 * 3)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString,
                              SimpleType("Source", List(SimpleType("Int", List()))), // Source[Int]
                              TSource(TMul(TNum(lhs), TNum(rhs))) // Source(lhs * rhs)
                            )
                          case nme.DIV => // E.g. "Source(2 / 3)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString,
                              SimpleType("Source", List(SimpleType("Int", List()))), // Source[Int]
                              TSource(TDiv(TNum(lhs), TNum(rhs))) // Source(lhs / rhs)
                            )
                          case _ => // Unsupported binary operator
                            report.error(s"Unsupported binary operator used: $op", tree.sourcePos)
                      case _ => // Unsupported type of integer RHS
                        report.error(s"Unsupported RHS: $rhs", tree.sourcePos)
                  case _ => () // Non-Inlined RHS (i.e. non-Source) can't happen because it would be caught by the type checker
              case "String" =>
                rhs match
                  case Inlined(call, _, _) => // Source gets inlined to Rescala Var, so this needs to match for Inlined
                    // Match for different kinds of RHS, like value literals or string operators
                    // The fun value of the (outer) Apply will be the type application of Source ("Source(x)" for x), so it can be ignored
                    call match
                      case Apply(_, List(Literal(Constant(value: String)))) => // E.g. "Source("abc")"
                        println(s"Adding Source reactive with string value $value to term list")
                        loreTerms = loreTerms :+ TAbs(name.toString, SimpleType("Source", List(SimpleType("String", List()))), TSource(TString(value)))
                      case _ => // Unsupported type of string RHS
                        report.error(s"Unsupported RHS: $rhs", tree.sourcePos)
                  case _ => () // Non-Inlined RHS (i.e. non-Source) can't happen because it would be caught by the type checker
              case "Boolean" =>
                rhs match
                  case Inlined(call, _, _) => // Source gets inlined to Rescala Var, so this needs to match for Inlined
                    // Match for different kinds of RHS, like literals or boolean operators
                    // The fun value of the (outer) Apply will be the type application of Source ("Source(x)" for x), so it can be ignored
                    call match
                      case Apply(_, List(Literal(Constant(value: Boolean)))) => // E.g. "Source(true)"
                        println(s"Adding Source reactive with boolean value $value to term list")
                        loreTerms = loreTerms :+ TAbs(
                          name.toString,
                          SimpleType("Source", List(SimpleType("Boolean", List()))),
                          TSource(if (value) TTrue() else TFalse())
                        )
                      case Apply(_, List(Select(Literal(Constant(value: Boolean)), op))) => // E.g. "Source(◯true)" (unary operator)
                        println(s"Adding source reactive with boolean value $op$value to term list")
                        op match
                          // This specifically has to be nme.UNARY_! and not e.g. nme.NOT
                          case nme.UNARY_! => // E.g. "Source(!false)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString,
                              SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                              TSource(TNeg(if (value) TTrue() else TFalse())) // Source(!value)
                            )
                          case _ => // Unsupported unary operator
                            report.error(s"Unsupported unary boolean operator used: $op", tree.sourcePos)
                      case Apply(_, List(Apply(Select(Literal(Constant(lhs: Boolean)), op), List(Literal(Constant(rhs: Boolean)))))) => // E.g. "Source(true ◯ false")
                        println(s"Adding Source reactive with boolean value $lhs $op $rhs to term list")
                        op match
                          // Important: nme.AND is & and nme.And is &&
                          case nme.And => // E.g. "Source(true && true)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString,
                              SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                              TSource(TConj( // Source(lhs && rhs)
                                if (lhs) TTrue() else TFalse(),
                                if (rhs) TTrue() else TFalse()
                              ))
                            )
                          // Important: nme.OR is | and nme.Or is ||
                          case nme.Or => // E.g. "Source(true || false)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString,
                              SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                              TSource(TDisj( // Source(lhs || rhs)
                                if (lhs) TTrue() else TFalse(),
                                if (rhs) TTrue() else TFalse()
                              ))
                            )
                          case _ => // Unsupported binary boolean operator
                            report.error(s"Unsupported binary boolean operator used: $op", tree.sourcePos)
                      case Apply(_, List(Apply(Select(Literal(Constant(lhs: Number)), op), List(Literal(Constant(rhs: Number)))))) => // E.g. "Source(3 ◯ 9")
                        println(s"Adding Source reactive with boolean value $lhs $op $rhs to term list")
                        op match
                          case nme.LT => // E.g. "Source(1 < 2)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString,
                              SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                              TSource(TLt(TNum(lhs), TNum(rhs))) // Source(lhs < rhs)
                            )
                          case nme.GT => // E.g. "Source(2 > 1)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString,
                              SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                              TSource(TGt(TNum(lhs), TNum(rhs))) // Source(lhs > rhs)
                            )
                          case nme.LE => // E.g. "Source(2 <= 1)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString,
                              SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                              TSource(TLeq(TNum(lhs), TNum(rhs))) // Source(lhs <= rhs)
                            )
                          case nme.GE => // E.g. "Source(2 >= 1)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString,
                              SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                              TSource(TGeq(TNum(lhs), TNum(rhs))) // Source(lhs >= rhs)
                            )
                          case nme.EQ => // E.g. "Source(2 == 1)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString,
                              SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                              TSource(TEq(TNum(lhs), TNum(rhs))) // Source(lhs == rhs)
                            )
                          case nme.NE => // E.g. "Source(2 != 1)"
                            loreTerms = loreTerms :+ TAbs(
                              name.toString,
                              SimpleType("Source", List(SimpleType("Boolean", List()))), // Source[Boolean]
                              TSource(TIneq(TNum(lhs), TNum(rhs))) // Source(lhs != rhs)
                            )
                          case _ => // Unsupported binary numerical operator with bool outcome
                            report.error(s"Unsupported binary numerical operator with bool outcome used: $op", tree.sourcePos)
                      case _ => // Unsupported type of boolean RHS
                        report.error(s"Unsupported RHS: $rhs", tree.sourcePos)
                  case _ => () // Non-Inlined RHS (i.e. non-Source) can't happen because it would be caught by the type checker
              case _ => // Unsupported LHS type argument
                report.error(s"Unsupported LHS type parameter used: $typeArg", tree.sourcePos)
          case reactiveDerivedPattern(typeArg) =>
            println(s"Detected Derived definition with $typeArg type parameter")
            // todo
          case reactiveInteractionPattern(typeArg1, typeArg2) =>
            println(s"Detected Interaction definition with $typeArg1 and $typeArg2 type parameters")
            // todo
      case ValDef(name, tpt, rhs) if name.toString.equals("thirdRealVariable") =>
        // Debug case, remove at some point
        println("----------")
        println(s"name: $name")
        println("----------------")
        println(s"tpt: $tpt")
        println(s"tpt.tpe: ${tpt.tpe}")
        println(s"tpt.tpe.argTypes: ${tpt.tpe.argTypes.map(t => t.show).mkString(", ")}")
        println("----------------")
        println(s"rhs: $rhs")

        report.warning("Third variable", tree.sourcePos)
      case _ => // ValDefs of non-reactives (i.e. not Source, Derived or Interaction) are ignored
        () // Do nothing
      tree
  end transformValDef
end DSLPhase
