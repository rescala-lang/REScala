package loreCompilerPlugin

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.report
import dotty.tools.dotc.transform.{Pickler, Inlining}
import lore.ast.*

import scala.util.matching.Regex

class CompilerPlugin extends StandardPlugin:
  val name: String        = "LoRe Compiler Plugin"
  val description: String = "Constructs a LoRe AST from the given Scala AST"

  override def initialize(options: List[String])(using Context): List[PluginPhase] = List(new LoRePhase)
end CompilerPlugin

class LoRePhase extends PluginPhase:
  import tpd.*
  val phaseName: String = "LoRe"

  override val runsAfter: Set[String]  = Set(Pickler.name)
  override val runsBefore: Set[String] = Set(Inlining.name)

  private var loreTerms: List[Term] = List()
  private val reactiveClasses: List[String] = List(
    "reactives.operator.Var",    // Source
    "reactives.operator.Signal", // Derived
    "lore.dsl.InteractionWithTypes",
    "lore.dsl.InteractionWithRequires",
    "lore.dsl.InteractionWithRequiresAndModifies"
  )
  private val reactiveSourcePattern: Regex      = """reactives\.operator\.Var\[(\w+)\]""".r
  private val reactiveDerivedPattern: Regex     = """reactives\.operator\.Signal\[(\w+)\]""".r
  private val reactiveInteractionPattern: Regex = """lore\.dsl\.InteractionWithTypes\[(\w+), (\w+)\]""".r

  private def logRhsInfo(indentLevel: Integer, operandSide: String, rhsType: String, rhsValue: String): Unit =
    if operandSide.nonEmpty then
      println(s"${"\t".repeat(indentLevel)}The $operandSide parameter is a $rhsType $rhsValue")
    else
      println(s"${"\t".repeat(indentLevel)}The parameter is a $rhsType $rhsValue")
  end logRhsInfo

  /** Takes the tree for a Scala RHS value and builds a LoRe term for it.
    * @param tree The Scala AST Tree node for a RHS expression to convert
    * @param indentLevel How many tabs to add before the logs of this call (none by default)
    * @param operandSide Which side to refer to in logs if expressing binary expressions (none by default)
    * @return The corresponding LoRe AST Tree node of the RHS
    */
  private def buildLoreRhsTerm(tree: tpd.LazyTree, indentLevel: Integer = 0, operandSide: String = "")(using
      Context
  ): Term =
    tree match
      case Literal(Constant(num: Int)) => // Basic int values like 0 or 1
        logRhsInfo(indentLevel, operandSide, "literal integer value", num.toString)
        TNum(num)
      case Literal(Constant(str: String)) => // Basic string values like "foo"
        logRhsInfo(indentLevel, operandSide, "literal string value", str)
        TString(str)
      case Literal(Constant(bool: Boolean)) => // Basic boolean values true or false
        logRhsInfo(indentLevel, operandSide, "literal boolean value", bool.toString)
        if bool then TTrue() else TFalse()
      case Ident(referenceName: Name) => // References to variables (any type)
        logRhsInfo(indentLevel, operandSide, "reference to variable", referenceName.toString)
        // No need to check whether the reference specified here actually exists, because if it didn't
        // then the original Scala code would not have compiled due to invalid reference and this
        // point would not have been reached either way, so just pass on the reference name to a TVar
        TVar(referenceName.toString)
      case Select(arg, opOrField) => // Field access and unary operator applications
        opOrField match
          case nme.UNARY_! => // Overall case catching supported unary operators, add other unary operators via |s here
            logRhsInfo(indentLevel, operandSide, "unary operator application of", opOrField.show)
            opOrField match // Match individual unary operators
              // This specifically has to be nme.UNARY_! and not e.g. nme.NOT
              case nme.UNARY_! => TNeg(buildLoreRhsTerm(arg, indentLevel + 1, operandSide)) // !operand
              case _ => // Unsupported unary operators
                report.error(
                  // No access to sourcePos here due to LazyTree
                  s"${"\t".repeat(indentLevel)}Unsupported unary operator ${opOrField.show} used:\n$tree"
                )
                TVar("") // Have to return a dummy Term value even on error to satisfy the compiler
          case field => // Field access, like "operand.value" and so forth (no parameter lists)
            // TODO: Unary operators that aren't explicitly supported will also land here, not sure what to do about that
            logRhsInfo(indentLevel, operandSide, "field access to field", opOrField.show)
            TFCall( // foo.bar
              buildLoreRhsTerm(arg, indentLevel + 1, operandSide), // foo (might be a more complex expression)
              field.toString, // bar
              List() // Always empty as these are field accesses
            )
      case Apply(Select(leftArg, opOrMethod), params: List[_]) => // Method calls and binary operator applications
        opOrMethod match
          case nme.ADD | nme.SUB | nme.MUL | nme.DIV | nme.And | nme.Or | nme.LT | nme.GT | nme.LE | nme.GE | nme.EQ | nme.NE =>
            // Supported Binary operator applications (as operator applications are methods on types, like left.+(right), etc)
            logRhsInfo(indentLevel, operandSide, "operator application of operator", opOrMethod.show)
            val rightArg = params.head
            opOrMethod match
              case nme.ADD => TAdd( // left + right
                buildLoreRhsTerm(leftArg, indentLevel + 1, "left"),
                buildLoreRhsTerm(rightArg, indentLevel + 1, "right")
              )
              case nme.SUB => TSub( // left - right
                buildLoreRhsTerm(leftArg, indentLevel + 1, "left"),
                buildLoreRhsTerm(rightArg, indentLevel + 1, "right")
              )
              case nme.MUL => TMul( // left * right
                buildLoreRhsTerm(leftArg, indentLevel + 1, "left"),
                buildLoreRhsTerm(rightArg, indentLevel + 1, "right")
              )
              case nme.DIV => TDiv( // left / right
                buildLoreRhsTerm(leftArg, indentLevel + 1, "left"),
                buildLoreRhsTerm(rightArg, indentLevel + 1, "right")
              )
              case nme.And => TConj( // left && right, Important: nme.AND is & and nme.And is &&
                buildLoreRhsTerm(leftArg, indentLevel + 1, "left"),
                buildLoreRhsTerm(rightArg, indentLevel + 1, "right")
              )
              case nme.Or => TDisj( // left || right, Important: nme.OR is | and nme.Or is ||
                buildLoreRhsTerm(leftArg, indentLevel + 1, "left"),
                buildLoreRhsTerm(rightArg, indentLevel + 1, "right")
              )
              case nme.LT => TLt( // left < right
                buildLoreRhsTerm(leftArg, indentLevel + 1, "left"),
                buildLoreRhsTerm(rightArg, indentLevel + 1, "right")
              )
              case nme.GT => TGt( // left > right
                buildLoreRhsTerm(leftArg, indentLevel + 1, "left"),
                buildLoreRhsTerm(rightArg, indentLevel + 1, "right")
              )
              case nme.LE => TLeq( // left <= right
                buildLoreRhsTerm(leftArg, indentLevel + 1, "left"),
                buildLoreRhsTerm(rightArg, indentLevel + 1, "right")
              )
              case nme.GE => TGeq( // left >= right
                buildLoreRhsTerm(leftArg, indentLevel + 1, "left"),
                buildLoreRhsTerm(rightArg, indentLevel + 1, "right")
              )
              case nme.EQ => TEq( // left == right
                buildLoreRhsTerm(leftArg, indentLevel + 1, "left"),
                buildLoreRhsTerm(rightArg, indentLevel + 1, "right")
              )
              case nme.NE => TIneq( // left != right
                buildLoreRhsTerm(leftArg, indentLevel + 1, "left"),
                buildLoreRhsTerm(rightArg, indentLevel + 1, "right")
              )
              case _ => // Unsupported binary operators
                report.error(
                  // No access to sourcePos here due to LazyTree
                  s"${"\t".repeat(indentLevel)}Unsupported binary operator ${opOrMethod.show} used:\n${"\t".repeat(indentLevel)}$tree"
                )
                TVar("") // Have to return a dummy Term value even on error to satisfy the compiler
          case methodName => // Method calls outside of explicitly supported binary operators
            logRhsInfo(indentLevel, operandSide, s"call to a method with ${params.size} parameters:", methodName.toString)
            TFCall( // foo.bar(baz, qux, ...)
              buildLoreRhsTerm(leftArg, indentLevel + 1, operandSide), // foo (might be a more complex term)
              methodName.toString, // bar
              params.map(p => buildLoreRhsTerm(p, indentLevel + 1, operandSide)) // baz, qux, ... (might each be more complex terms)
            )
      case Apply(Ident(name: Name), params: List[_]) => // Function calls
        logRhsInfo(indentLevel, operandSide, s"call to a function with ${params.size} parameters:", name.toString)
        TFunC( // foo(bar, baz)
          name.toString, // foo
          params.map(p => buildLoreRhsTerm(p, indentLevel + 1, operandSide)) // bar, baz, ... (might each be more complex terms)
        )
      case _ => // Unsupported RHS forms
        report.error(
          // No access to sourcePos here due to LazyTree
          s"${"\t".repeat(indentLevel)}Unsupported RHS form used:\n${"\t".repeat(indentLevel)}$tree"
        )
        TVar("") // Have to return a dummy Term value even on error to satisfy the compiler
  end buildLoreRhsTerm

  override def transformValDef(tree: tpd.ValDef)(using Context): tpd.Tree =
    tree match
      // Match value definitions for base types Int, String, Boolean, these also exist in LoRe, e.g. used to feed Reactives
      case ValDef(name, tpt, rhs) if tpt.tpe =:= defn.IntType || tpt.tpe =:= defn.StringType || tpt.tpe =:= defn.BooleanType =>
        println(s"Detected ${tpt.tpe.show} definition with name \"$name\", adding to term list")
        val term: Term = rhs match
          case EmptyTree => // Function arguments, not actual variables, e.g. "foo: Bar"
            TArgT(
              name.toString,                    // foo (any valid Scala identifier)
              SimpleType(tpt.tpe.show, List()), // Bar (one of Int, String, Boolean)
            )
          case _ => // Actually defined variables
            // Construct LoRe term AST node from Scala term of the form "foo: Bar = baz"
            TAbs(
              name.toString,                    // foo (any valid Scala identifier)
              SimpleType(tpt.tpe.show, List()), // Bar (one of Int, String, Boolean)
              buildLoreRhsTerm(rhs, 1)             // baz (e.g. 0, 1 + 2, "test", true, 2 > 1, bar as a reference, etc)
            )
        // TODO: Actually build a tree structure for the terms instead of just slamming them all into a flat list
        loreTerms = loreTerms :+ term
      // Match ValDefs for LoRe reactives (Source, Derived, Interaction)
      case ValDef(name, tpt, rhs) if reactiveClasses.exists(t => tpt.tpe.show.startsWith(t)) =>
        // Match which reactive it actually is, and what its type arguments are
        tpt.tpe.show match
          case reactiveSourcePattern(typeArg) =>
            println(s"Detected Source reactive with name \"$name\" and type parameter $typeArg, adding to term list")
            // Only Int, String and Boolean type parameters are supported
            if !typeArg.equals("Int") && !typeArg.equals("String") && !typeArg.equals("Boolean") then {
              report.error(s"Unsupported LHS type parameter used: $typeArg", tree.sourcePos)
            }
            rhs match
              // Several notes to make here for future reference:
              // * There's an Apply around the whole RHS whose significance I'm not exactly sure of.
              //   Maybe it's related to a call for Inlining or such, as this plugin runs before that phase
              //   and the expressions being handled here use types that get inlined in REScala.
              // * Because the RHS is wrapped in a call to the Source type, within that unknown Apply layer,
              //   there's one layer of an Apply call to the REScala type wrapping the RHS we want, and the
              //   actual RHS tree we want is inside the Apply parameter list (i.e. real RHS is 2 Apply layers deep).
              // * The Source parameter list always has length 1, because Sources only have 1 parameter ("Source(foo)", not "Source(foo, bar)").
              // * Typechecking for whether the arguments both in the Source type call as well as within the expression
              //   contained within any part of that call are of the expected type are handled by the Scala type-checker already
              case Apply(Apply(_, List(properRhs)), _) => // E.g. "foo: Source[bar] = Source(baz)"
                // TODO: Actually build a tree structure for the terms instead of just slamming them all into a flat list
                loreTerms = loreTerms :+ TAbs(
                  name.toString, // foo (any valid Scala identifier)
                  SimpleType( // Source[bar], where bar is one of Int, String, Boolean
                    "Source",
                    List(SimpleType(typeArg, List()))
                  ),
                  TSource(buildLoreRhsTerm(properRhs, 1)) // Source(baz), where baz is any recognized expression, see above
                )
              case _ =>
                // Anything that's not wrapped with Source, should not be possible at this point because of the Scala type-checker
                println(
                  s"A Source definition not wrapped in the Source type has been detected. This should not be possible, please investigate:\n$rhs"
                )
                report.error("Source definition not wrapped in Source call", tree.sourcePos)
          // TODO: Implement
          case reactiveDerivedPattern(typeArg) =>
            println(s"Detected Derived reactive with name \"$name\" and type parameter $typeArg, adding to term list")
            // Only Int, String and Boolean type parameters are supported
            if !typeArg.equals("Int") && !typeArg.equals("String") && !typeArg.equals("Boolean") then {
              report.error(s"Unsupported LHS type parameter used: $typeArg", tree.sourcePos)
            }
            rhs match
              // Several notes to make here for future reference:
              // * There's an Apply around the whole RHS whose significance I'm not exactly sure of.
              //   Maybe it's related to a call for Inlining or such, as this plugin runs before that phase
              //   and the expressions being handled here use types that get inlined in REScala.
              // * Because the RHS is wrapped in a call to the Derived type, within that unknown Apply layer,
              //   there's one layer of an Apply call to the REScala type wrapping the RHS we want, and the
              //   actual RHS tree we want is inside the Apply parameter list (i.e. real RHS is 2 Apply layers deep).
              // * The Derived parameter list always has length 1, because you feed one expression to the Derived type
              // * As the Derived type uses curly brackets, it wraps its contents in a Block type as well
              // * Typechecking for whether the arguments both in the Derived type call as well as within the expression
              //   contained within any part of that call are of the expected type are handled by the Scala type-checker already
              case Apply(Apply(_, List(Block(_, properRhs))), _) => // E.g. "foo: Derived[bar] = Derived { baz }"
                // TODO: Actually build a tree structure for the terms instead of just slamming them all into a flat list
                loreTerms = loreTerms :+ TAbs(
                  name.toString, // foo (any valid Scala identifier)
                  SimpleType( // Derived[bar], where bar is one of Int, String, Boolean
                    "Derived",
                    List(SimpleType(typeArg, List()))
                  ),
                  TDerived(buildLoreRhsTerm(properRhs, 1)) // Derived { baz } , where baz is any recognized expression, see above
                )
              case _ =>
                // Anything that's not wrapped with Derived, should not be possible at this point because of the Scala type-checker
                println(
                  s"A Derived definition not wrapped in the Derived type has been detected. This should not be possible, please investigate:\n$rhs"
                )
                report.error("Derived definition not wrapped in Source call", tree.sourcePos)
          // TODO: Implement
          case reactiveInteractionPattern(typeArg1, typeArg2) =>
            println(s"Detected Interaction definition with $typeArg1 and $typeArg2 type parameters")
      case _ => () // All other ValDefs not covered above are ignored
    tree // Return the original tree to further compiler phases
  end transformValDef
end LoRePhase
