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

  var loreTerms: List[Term] = List()
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
            rhs match
              case Inlined(call, _, _) =>
                // Match for different kinds of RHS, like literals or math operators
                // fun value of the (outer) Apply will be the type application for Source ("Source(x)"), so it can be ignored
                call match
                  case Apply(_, List(Literal(Constant(value: Number)))) => // E.g. "Source(0)"
                    println(s"Adding Source reactive with number value $value to term list")
                    loreTerms = loreTerms :+ TAbs(name.toString, SimpleType("Source", List(SimpleType(typeArg, List()))), TNum(value))
                  case Apply(_, List(Literal(Constant(value: String)))) => // E.g. "Source("foo")"
                    println(s"Adding Source reactive with string value $value to term list")
                    loreTerms = loreTerms :+ TAbs(name.toString, SimpleType("Source", List(SimpleType(typeArg, List()))), TString(value))
                  case Apply(_, List(Apply(Select(Literal(Constant(lhs: Number)), op), List(Literal(Constant(rhs: Number)))))) => // E.g. "Source(4 ◯ 2")
                    println(s"Adding Source reactive with value $lhs $op $rhs to term list")
                    op match
                      case nme.ADD => // E.g. "Source(2 + 3)"
                        loreTerms = loreTerms :+ TAbs(name.toString, SimpleType("Source", List(SimpleType(typeArg, List()))), TAdd(TNum(lhs), TNum(rhs)))
                      case nme.SUB => // E.g. "Source(2 - 3)"
                        loreTerms = loreTerms :+ TAbs(name.toString, SimpleType("Source", List(SimpleType(typeArg, List()))), TSub(TNum(lhs), TNum(rhs)))
                      case nme.MUL => // E.g. "Source(2 * 3)"
                        loreTerms = loreTerms :+ TAbs(name.toString, SimpleType("Source", List(SimpleType(typeArg, List()))), TMul(TNum(lhs), TNum(rhs)))
                      case nme.DIV => // E.g. "Source(2 / 3)"
                        loreTerms = loreTerms :+ TAbs(name.toString, SimpleType("Source", List(SimpleType(typeArg, List()))), TDiv(TNum(lhs), TNum(rhs)))
                  case Apply(fun, args) =>
                    println(s"fun: $fun")
                    println(s"args: $args")
                  // todo: other cases
              case _ => println(s"Source rhs: $rhs")
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
      case _ =>
        ()
      tree
  end transformValDef
end DSLPhase
