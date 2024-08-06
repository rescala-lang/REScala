package loreCompilerPlugin

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.report
import dotty.tools.dotc.transform.{Inlining, Pickler}
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.core.Symbols.Symbol
import lore.ast.*
import cats.data.NonEmptyList

import scala.annotation.nowarn

class CompilerPlugin extends StandardPlugin:
  val name: String        = "LoRe Compiler Plugin"
  val description: String = "Constructs a LoRe AST from the given Scala AST"

  @nowarn // which variant to override depends on the scala version, use the old one until 3.5 is more stable
  override def init(options: List[String]): List[PluginPhase] = List(new LoRePhase)
end CompilerPlugin

class LoRePhase extends PluginPhase:
  val phaseName: String = "LoRe"

  override val runsAfter: Set[String]  = Set(Pickler.name)
  override val runsBefore: Set[String] = Set(Inlining.name)

  private var loreTerms: Map[(SourceFile, Symbol), List[Term]] = Map()

  /** Logs info about a RHS value. Not very robust, rather a (temporary?) solution to prevent large logging code duplication.
    * @param indentLevel How many tabs should be placed before the text that will be logged
    * @param operandSide Additional text to indicate more information about the parameter (e.g. "left" will result in "left parameter")
    * @param rhsType The type of the RHS in question
    * @param rhsValue The value of the RHS in question
    */
  private def logRhsInfo(indentLevel: Integer, operandSide: String, rhsType: String, rhsValue: String): Unit =
    if operandSide.nonEmpty then
      println(s"${"\t".repeat(indentLevel)}The $operandSide parameter is a $rhsType $rhsValue")
    else
      println(s"${"\t".repeat(indentLevel)}The parameter is a $rhsType $rhsValue")
  end logRhsInfo

  /** Builds a LoRe Type node based on a scala type tree
    * @param typeTree The scala type tree
    * @return The LoRe Type node
    */
  private def buildLoreTypeNode(typeTree: tpd.Tree)(using Context): Type =
    // May need to also support LoRe TupleTypes at one point in the future
    // Object definitions seems to use "SingletonTypeTrees" instead, but those are ignored in this plugin
    typeTree match
      case Ident(typeName) => // Plain type names
        SimpleType(typeName.toString, List())
      case AppliedTypeTree(outerTypeTree, innerTypeArgs) => // Parameterized type names
        outerTypeTree match
          case Ident(outerTypeName) => // Plain type name of parameterized type
            SimpleType(
              outerTypeName.toString,
              innerTypeArgs.map(innerType => buildLoreTypeNode(innerType)) // Type parameters of parameterized type
            )
          case _ =>
            report.error(
              s"An error occurred building the LoRe type tree for the following tree:\n$outerTypeTree",
              typeTree.sourcePos
            )
            SimpleType("<error>", List())
      case _ =>
        report.error(
          s"An error occurred building the LoRe type tree for the following tree:\n$typeTree",
          typeTree.sourcePos
        )
        SimpleType("<error>", List())

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
                // No access to sourcePos here due to LazyTree
                report.error(s"${"\t".repeat(indentLevel)}Unsupported unary operator ${opOrField.show} used:\n$tree")
                TVar("<error>")
          case field => // Field access, like "operand.value" and so forth (no parameter lists)
            // TODO: Unary operators that aren't explicitly supported will also land here, not sure what to do about that
            logRhsInfo(indentLevel, operandSide, "field access to field", opOrField.show)
            TFCall(                                                // foo.bar
              buildLoreRhsTerm(arg, indentLevel + 1, operandSide), // foo (might be a more complex expression)
              field.toString,                                      // bar
              List()                                               // Always empty as these are field accesses
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
                // No access to sourcePos here due to LazyTree
                report.error(
                  s"${"\t".repeat(indentLevel)}Unsupported binary operator ${opOrMethod.show} used:\n${"\t".repeat(indentLevel)}$tree"
                )
                TVar("<error>")
          case methodName => // Method calls outside of explicitly supported binary operators
            logRhsInfo(
              indentLevel,
              operandSide,
              s"call to a method with ${params.size} parameters:",
              methodName.toString
            )
            TFCall(                                                    // foo.bar(baz, qux, ...)
              buildLoreRhsTerm(leftArg, indentLevel + 1, operandSide), // foo (might be a more complex term)
              methodName.toString,                                     // bar
              params.map(p =>                                          // baz, qux, ... (each can be more complex terms)
                buildLoreRhsTerm(p, indentLevel + 1, operandSide)
              )
            )
      case Apply(Ident(name: Name), params: List[_]) => // Function calls
        logRhsInfo(indentLevel, operandSide, s"call to a function with ${params.size} parameters:", name.toString)
        TFunC(            // foo(bar, baz)
          name.toString,  // foo
          params.map(p => // bar, baz, ... (might each be more complex terms)
            buildLoreRhsTerm(p, indentLevel + 1, operandSide)
          )
        )
      case Apply(TypeApply(Select(Ident(typeName: Name), _), _), List(Typed(SeqLiteral(params: List[_], _), _))) =>
        // Type instantiations like Lists etc
        logRhsInfo(indentLevel, operandSide, s"type call to the ${typeName.toString} type with ${params.length} parameters", "")
        TFunC(
          typeName.toString,
          params.map(p => buildLoreRhsTerm(p, indentLevel + 1, operandSide))
        )
      case Apply(TypeApply(Select(Ident(typeName: Name), _), _), params: List[_]) =>
        // Tuple definitions, may also catch currently unknown other cases (and has to stay below type instant. case)
        logRhsInfo(indentLevel, operandSide, s"type call to the ${typeName.toString} type with ${params.length} parameters", " ")
        TFunC(
          typeName.toString,
          params.map(p => buildLoreRhsTerm(p, indentLevel + 1, operandSide))
        )
      case Apply(Apply(TypeApply(Select(Ident(typeName: Name), _), _), params: List[_]), _) =>
        // Type instantiations for Source/Var and Derived/Signal reactives
        logRhsInfo(indentLevel, operandSide, s"definition of a ${typeName.toString} reactive", "")
        typeName.toString match
          case "Source" | "Var"  => TSource(buildLoreRhsTerm(params.head, indentLevel + 1))
          case "Derived" | "Signal" => TDerived(buildLoreRhsTerm(params.head, indentLevel + 1))
          case _         => // Unsupported reactive
            // No access to sourcePos here due to LazyTree
            report.error(
              s"${"\t".repeat(indentLevel)}Unsupported reactive used in RHS:\n${"\t".repeat(indentLevel)}$tree"
            )
            TVar("<error>")
      case Block(List(fun), Closure(_, Ident(name), _)) if name.toString == "$anonfun" => // Arrow functions
        fun match // Arrow function def is a DefDef, arrowLhs is a list of ValDefs
          case DefDef(_, List(arrowLhs), _, arrowRhs) =>
            TArrow( // (foo: Int) => foo + 1
              TSeq(NonEmptyList.fromList(arrowLhs.map { // (foo: Int)
                // This breaks when the parameter list of the arrow function
                // is empty, but I don't know what lore node I am supposed
                // to use otherwise as TArrow only allows one node for the lhs
                // and all available lore nodes with lists require NonEmptyLists
                case ValDef(paramName, paramType, tpd.EmptyTree) =>
                  TArgT(
                    paramName.toString,
                    buildLoreTypeNode(paramType)
                  )
                case _ =>
                  report.error(
                    s"${"\t".repeat(indentLevel)}Error building LHS term for arrow function:\n${"\t".repeat(indentLevel)}$tree"
                  )
                  TVar("<error>")
              }).get), // Error would show up in the get on empty param list, see above
              buildLoreRhsTerm(arrowRhs) // foo + 1
            )
          case _ =>
            // No access to sourcePos here due to LazyTree
            report.error(
              s"${"\t".repeat(indentLevel)}Error building RHS term for arrow function:\n${"\t".repeat(indentLevel)}$tree"
            )
            TVar("<error>")
      case _ => // Unsupported RHS forms
        // No access to sourcePos here due to LazyTree
        report.error(
          s"${"\t".repeat(indentLevel)}Unsupported RHS form used:\n${"\t".repeat(indentLevel)}$tree"
        )
        TVar("<error>")
  end buildLoreRhsTerm

  override def transformValDef(tree: tpd.ValDef)(using ctx: Context): tpd.Tree =
    var newLoreTerm: Option[Term] = None // Placeholder, value is defined in below individual cases to avoid code dupe

    tree match
      case ValDef(name, tpt, rhs) =>
        rhs match
          case tpd.EmptyTree => () // Function parameter and Part 1 of object/package definitions, these are ignored
          case Apply(Select(_, n), _) if n.toString.equals("<init>") => () // Part 2 of Object and package definitions
          case _ => // Other definitions, these are the ones we care about
            val loreTypeNode = buildLoreTypeNode(tpt)
            // Several notes to make here regarding handling the RHS of reactives for future reference:
            // * There's an Apply around the whole RHS whose significance I'm not exactly sure of.
            //   Maybe it's related to a call for Inlining or such, as this plugin runs before that phase
            //   and the expressions being handled here use types that get inlined in REScala.
            // * Because the RHS is wrapped in a call to the respective reactive type, within that unknown Apply layer,
            //   there's one layer of an Apply call to the REScala type wrapping the RHS we want, and the
            //   actual RHS tree we want is inside the second Apply parameter list (i.e. real RHS is 2 Apply layers deep).
            // * As the Derived type uses curly brackets in definitions on top, it additionally wraps its RHS in a Block type.
            // * The Source and Derived parameter lists always have length 1, those of Interactions always have length 2.
            // * Typechecking for whether all of this is correct is already done by the Scala type-checker before this phase,
            //   so we can assume everything we see here is of suitable types instead of doing any further checks.
            loreTypeNode match
              case SimpleType(typeName, typeArgs) =>
                println(s"Detected $typeName definition with name \"$name\"")
                rhs match
                  case tpd.EmptyTree => () // Ignore func args (ArgT) for now
                  case Apply(Apply(_, List(properRhs)), _)
                      if typeName == "Source" || typeName == "Var" => // E.g. "foo: Source[bar] = Source(baz)"
                    newLoreTerm = Some(TAbs(                  // foo: Source[Bar] = Source(baz)
                      name.toString,                          // foo
                      loreTypeNode,                           // Source[Bar]
                      TSource(buildLoreRhsTerm(properRhs, 1)) // Source(baz)
                    ))
                  case Apply(Apply(_, List(Block(_, properRhs))), _)
                      if typeName == "Derived" | typeName == "Signal" => // E.g. "foo: Derived[bar] = Derived { baz } "
                    newLoreTerm = Some(TAbs(                   // foo: Derived[Bar] = Derived { baz }
                      name.toString,                           // foo
                      loreTypeNode,                            // Derived[Bar]
                      TDerived(buildLoreRhsTerm(properRhs, 1)) // Derived { baz }
                    ))
                  case TypeApply(Select(Ident(tp), _), List(reactiveType, argumentType)) if typeName == "UnboundInteraction" =>
                    newLoreTerm = Some(TAbs(
                      name.toString,
                      loreTypeNode,
                      TInteraction(
                        buildLoreTypeNode(reactiveType),
                        buildLoreTypeNode(argumentType)
                      )
                    ))
                  // TODO: Interactions with requires/ensures/modifies
                  case _ => // Any non-reactive values (Int, String, Bool, ...)
                    newLoreTerm = Some(TAbs(   // foo: Bar = baz
                      name.toString,           // foo (any valid Scala identifier)
                      loreTypeNode,            // Bar
                      buildLoreRhsTerm(rhs, 1) // baz (e.g. 0, 1 + 2, "test", true, 2 > 1, bar as a reference, etc)
                    ))
              case TupleType(_) => // TODO tuple types?
                println("surprise tuple type")
                report.error("Tuple Types are not currently supported", tree.sourcePos)

    // Add the newly generated LoRe term to the respective source file's term list (if one was generated)
    newLoreTerm match
      case None =>
        tree // No term created, return tree to further compiler phases
      case Some(loreTerm) =>
        // Find term list for this source file and append new term to it
        // Alternatively, make new term list for this file if it doesn't exist
        val fileTermList: Option[List[Term]] = loreTerms.get((tree.source, ctx.owner))
        fileTermList match
          case Some(list) =>
            val newList: List[Term] = list :+ loreTerm
            loreTerms = loreTerms.updated((tree.source, ctx.owner), newList)
          case None =>
            println(s"Adding new term list to Map for the ${ctx.owner.toString} in ${tree.source.name}")
            val newList: List[Term] = List(loreTerm)
            loreTerms = loreTerms.updated((tree.source, ctx.owner), newList)
        tree // Return the original tree to further compiler phases
  end transformValDef
end LoRePhase
