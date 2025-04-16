package loreCompilerPlugin.codegen

import lore.ast.*

object DafnyGen {

  /** Takes a Scala type name and returns the corresponding Dafny type name, if one exists.
    * E.g. the corresponding Dafny type for the Scala type "String" is "string" (note the casing),
    * and the corresponding type for both the Float and Double types in Scala is the Dafny "real" type.
    * @param typeName The name of the Scala type.
    * @return The name of the corresponding Dafny type, or the original parameter if no such correspondence exists.
    */
  private def getDafnyType(typeName: String): String = {
    typeName match
      case "Boolean"          => "bool"
      case "Char"             => "char"
      case "Int"              => "int"
      case "Float" | "Double" => "real"   // Dafny "real" follows SMT-Lib "Real" theory, so should be fine to map onto.
      case "String"           => "string" // Technically seq<char> (String is Seq[Char]), syntactic sugar respectively.
      case "Map"              => "map"
      case "List"             => "seq"    // This does confer some changes in semantics regarding mutability etc.
      // TODO: Add others...
      case _ => typeName
  }

  /** Recursively gathers the names of all references used in the given LoRe Term node.
    * @param node The node to search.
    * @return The names of all references used in this node.
    */
  private def usedReferences(node: Term): Set[String] = {
    // This uses sets to avoid duplicate entries.

    node match
      // Direct reference term (recursion anchor 1)
      case n: TVar => Set(n.name)
      // Branches into "left" and "right" terms
      case n: TDiv   => usedReferences(n.left) ++ usedReferences(n.right)
      case n: TMul   => usedReferences(n.left) ++ usedReferences(n.right)
      case n: TAdd   => usedReferences(n.left) ++ usedReferences(n.right)
      case n: TSub   => usedReferences(n.left) ++ usedReferences(n.right)
      case n: TLt    => usedReferences(n.left) ++ usedReferences(n.right)
      case n: TGt    => usedReferences(n.left) ++ usedReferences(n.right)
      case n: TLeq   => usedReferences(n.left) ++ usedReferences(n.right)
      case n: TGeq   => usedReferences(n.left) ++ usedReferences(n.right)
      case n: TEq    => usedReferences(n.left) ++ usedReferences(n.right)
      case n: TIneq  => usedReferences(n.left) ++ usedReferences(n.right)
      case n: TDisj  => usedReferences(n.left) ++ usedReferences(n.right)
      case n: TConj  => usedReferences(n.left) ++ usedReferences(n.right)
      case n: TImpl  => usedReferences(n.left) ++ usedReferences(n.right)
      case n: TBImpl => usedReferences(n.left) ++ usedReferences(n.right)
      case n: TInSet => usedReferences(n.left) ++ usedReferences(n.right)
      // Has a "body" term
      case n: TNeg        => usedReferences(n.body)
      case n: TQuantifier => usedReferences(n.body) // Quantifier vars are ArgTs, so not relevant
      // Has a "parent" term
      case n: TFCall  => usedReferences(n.parent) // TODO: might need to also recurse into n.args
      case n: TFCurly => usedReferences(n.parent) ++ usedReferences(n.body)
      // Anything else is not and does not have references (recursion anchor 2)
      case _ => Set()
  }

  /** Compiles a list of LoRe terms into Dafny code.
    * @param ast The list of LoRe terms to compile to Dafny.
    * @return The generated Dafny code.
    */
  def generate(ast: List[Term]): String = {
    var compilationContext: Map[String, String] = Map()

    val dafnyCode: List[String] = ast.map(term => {
      // Record name and type of new definitions
      term match
        case TAbs(name, _type, _, _) =>
          val tp: String = generate(_type, compilationContext)
          compilationContext = compilationContext.updated(name, tp)
        case _ => ()

      // Map term actually generated Dafny code
      generate(term, compilationContext)
    })

    dafnyCode.mkString("\n\n")
  }

  /** Generates Dafny code for the given LoRe Term node.
    *
    * @param node The LoRe Term node.
    * @return The generated Dafny code.
    */
  private def generate(node: Term, ctx: Map[String, String]) = {
    node match
      // Cases ordered by order in LoRe AST definition.
      case n: TViperImport => generateFromTViperImport(n, ctx)
      case n: TArgT        => generateFromTArgT(n, ctx)
      case n: TVar         => generateFromTVar(n, ctx)
      case n: TAbs         => generateFromTAbs(n, ctx)
      case n: TTuple       => generateFromTTuple(n, ctx)
      case n: TIf          => generateFromTIf(n, ctx)
      case n: TSeq         => generateFromTSeq(n, ctx)
      case n: TArrow       => generateFromTArrow(n, ctx)
      case n: TTypeAl      => generateFromTTypeAl(n, ctx)
      case n: TAssert      => generateFromTAssert(n, ctx)
      case n: TAssume      => generateFromTAssume(n, ctx)
      case n: TReactive    => generateFromTReactive(n, ctx)
      case n: TInteraction => generateFromTInteraction(n, ctx)
      case n: TInvariant   => generateFromTInvariant(n, ctx)
      case n: TArith       => generateFromTArith(n, ctx)
      case n: TBoolean     => generateFromTBoolean(n, ctx)
      case n: TParens      => generateFromTParens(n, ctx)
      case n: TString      => generateFromTString(n, ctx)
      case n: TFAcc        => generateFromTFAcc(n, ctx)
      case n: TFunC        => generateFromTFunC(n, ctx)
  }

  /** Generates a Dafny type annotation for the given LoRe Type node.
    *
    * @param node The LoRe Type node.
    * @return The generated Dafny type annotation.
    */
  private def generate(node: Type, ctx: Map[String, String]): String = {
    node match
      case n: SimpleType => generateFromSimpleType(n, ctx)
      case n: TupleType  => generateFromTupleType(n, ctx)
  }

  /** Generates a Dafny Type annotation for the given LoRe SimpleType node.
    *
    * @param node The LoRe SimpleType node.
    * @return The generated Dafny Type annotation.
    */
  private def generateFromSimpleType(node: SimpleType, ctx: Map[String, String]): String = {
    val dafnyType: String = getDafnyType(node.name)

    if dafnyType == "Source" || dafnyType == "Var" then {
      // Source terms are modeled as Dafny fields typed after the inner type of the Source in LoRe.
      // That is to say, a "Source[Int]" is just an "int" type field in Dafny - the Source types does not appear.
      // A Source always only has one type parameter, so generate the annotation for it and return that value.
      generate(node.inner.head, ctx)
    } else if dafnyType == "Derived" || dafnyType == "Signal" then {
      // Derived terms are modeled as Dafny functions whose return type is the type parameter of the Derived in LoRe.
      // That is to say, a "foo: Derived[Int]" is a "function foo(...): int { ... }", with no mention of a Derived type.
      // A Derived always only has one type parameter, so generate the annotation for it and return that value.
      generate(node.inner.head, ctx)
    } else if dafnyType.matches("Tuple\\d+") then {
      // The name of Dafny's tuple type is blank, and instead of angled brackets uses parens, i.e. (string, int).
      // That means we just concat the inner types surrounded by parens for building tuple type annotations.
      // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-tuple-types
      s"(${node.inner.map(t => generate(t, ctx)).mkString(", ")})"
    } else if dafnyType.matches("Function\\d+") then {
      // Anonymous functions

      // The input and output types for FunctionN types aren't split in its parameter list.
      // The name of the type however tells you the number of inputs, and there's always only one output in Scala/LoRe.
      // Therefore, grab the number from the type name and that many elements, and then the last element as output.
      val functionArity: Int   = dafnyType.split("Function").last.toInt
      val inputs: List[String] = node.inner.take(functionArity).map(p => generate(p, ctx))
      val output: String       = generate(node.inner.last, ctx)

      // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-arrow-types
      // TODO: Can be one of three arrow types: "->", "-->" or "~>". Look into these.
      s"(${inputs.mkString(", ")}) -> $output"
    } else { // All other types are simply output according to regular Dafny type syntax
      val inner: String =
        if node.inner.isEmpty then "" else s"<${node.inner.map(t => generate(t, ctx)).mkString(", ")}>"
      s"$dafnyType$inner"
    }
  }

  /** Generates a Dafny Type annotation for the given LoRe TupleType node.
    *
    * @param node The LoRe TupleType node.
    * @return The generated Dafny Type annotation.
    */
  private def generateFromTupleType(node: TupleType, ctx: Map[String, String]): String = {
    val tupleElements: List[String] = node.inner.map(t => generate(t, ctx)).toList

    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-tuple-types
    s"(${tupleElements.mkString(", ")})"
  }

  /** Generates Dafny code for the given LoRe TArgT.
    *
    * @param node The LoRe TArgT node.
    * @return The generated Dafny code.
    */
  private def generateFromTArgT(node: TArgT, ctx: Map[String, String]): String = {
    val typeAnnot: String = generate(node._type, ctx)

    s"${node.name}: $typeAnnot"
  }

  /** Generates Dafny code for the given LoRe TVar.
    *
    * @param node The LoRe TVar node.
    * @return The generated Dafny code.
    */
  private def generateFromTVar(node: TVar, ctx: Map[String, String]): String = {
    // Just place the variable name in the code.
    // TODO: Depending on implementation of the reactives, this may need branching output depending on type of the var
    node.name
  }

  /** Generates Dafny code for the given LoRe TAbs.
    *
    * @param node The LoRe TAbs node.
    * @return The generated Dafny code.
    */
  private def generateFromTAbs(node: TAbs, ctx: Map[String, String]): String = {
    // TODO: Finish implementing. Depending on the type, output must differ, debug for now
    node.body match
      case n: TSource =>
        // Source terms are realized as Dafny fields, which can be modified post-definition.
        // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-field-declaration
        s"var ${node.name}: ${generate(node._type, ctx)} := ${generateFromTSource(n, ctx)};"
      case n: TDerived =>
        // Derived terms are realized as Dafny functions. Functions do not have side-effects.
        // The return type of these functions is the type parameter of the Derived, while any
        // references included in the body of the Derived are turned into function parameters.
        val references: Set[String] = usedReferences(n.body)
        val parameters: Set[String] = references.map(ref => s"$ref: ${ctx(ref)}")

        // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-function-declaration
        s"""function ${node.name}(${parameters.mkString(", ")}): ${generate(node._type, ctx)} {
           |  ${generateFromTDerived(n, ctx)}
           |}
           |""".stripMargin
      case n: TInteraction =>
        ""
      case _ =>
        // Default into generic field declarations for other types
        val typeAnnot: String = generate(node._type, ctx)
        val body: String      = generate(node.body, ctx)
        s"var ${node.name}: $typeAnnot := $body;"
  }

  /** Generates Dafny code for the given LoRe TTuple.
    *
    * @param node The LoRe TTuple node.
    * @return The generated Dafny code.
    */
  private def generateFromTTuple(node: TTuple, ctx: Map[String, String]): String = {
    val elems: List[String] = node.factors.map(t => generate(t, ctx))

    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-tuple-types
    s"(${elems.mkString(", ")})"
  }

  /** Generates Dafny code for the given LoRe TIf.
    *
    * @param node The LoRe TIf node.
    * @return The generated Dafny code.
    */
  private def generateFromTIf(node: TIf, ctx: Map[String, String]): String = {
    val cond: String     = generate(node.cond, ctx)
    val thenExpr: String = generate(node._then, ctx)
    val elseExpr: String = if node._else.isDefined then generate(node._else.get, ctx) else ""

    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-if-statement
    if elseExpr.isEmpty then {
      s"""if $cond {
         |  $thenExpr
         |}""".stripMargin
    } else {
      s"""if $cond {
         |  $thenExpr
         |} else {
         |  $elseExpr
         |}""".stripMargin
    }
  }

  /** Generates Dafny code for the given LoRe TSeq.
    *
    * @param node The LoRe TSeq node.
    * @return The generated Dafny code.
    */
  private def generateFromTSeq(node: TSeq, ctx: Map[String, String]): String = {
    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-sequences
    s"[${node.body.map(t => generate(t, ctx)).toList.mkString(", ")}]"
  }

  /** Generates Dafny code for the given LoRe TArrow.
    *
    * @param node The LoRe TArrow node.
    * @return The generated Dafny code.
    */
  private def generateFromTArrow(node: TArrow, ctx: Map[String, String]): String = {
    val arrowHead: String = generate(node.left, ctx)
    val arrowBody: String = generate(node.right, ctx)

    // Anonymous function body always uses "=>" in Dafny. Differentiation between "->", "-->" and "~>" is for types.
    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-lambda-expression
    s"$arrowHead => $arrowBody"
  }

  /** Generates Dafny code for the given LoRe TReactive.
    *
    * @param node The LoRe TReactive node.
    * @return The generated Dafny code.
    */
  private def generateFromTReactive(node: TReactive, ctx: Map[String, String]): String = {
    // These methods only generate the body of the reactive terms, not the surrounding structure.
    // E.g. for a Source(1+2), this only processes the 1+2 part. For a Derived { foo }, only the foo part.
    // For the structure, such as the function modelling of Derived, see the generation of TAbs.
    val reactive: String = node match
      case n: TSource  => generateFromTSource(n, ctx)
      case n: TDerived => generateFromTDerived(n, ctx)

    reactive
  }

  /** Generates Dafny code for the given LoRe TSource.
    *
    * @param node The LoRe TSource node.
    * @return The generated Dafny code.
    */
  private def generateFromTSource(node: TSource, ctx: Map[String, String]): String = {
    // Sources are realized as Dafny fields. This method however just generates the body,
    // since the TSource node only contains that information (see TABs for the field definition).
    generate(node.body, ctx)
  }

  /** Generates Dafny code for the given LoRe TDerived.
    *
    * @param node The LoRe TDerived node.
    * @return The generated Dafny code.
    */
  private def generateFromTDerived(node: TDerived, ctx: Map[String, String]): String = {
    // Derived terms are realized as Dafny functions. This method however just generates the body,
    // since the TDerived node only contains that information (see TABs for the function definition).
    generate(node.body, ctx)
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TInteraction.
    *
    * @param node The LoRe TInteraction node.
    * @return The generated Dafny code.
    */
  private def generateFromTInteraction(node: TInteraction, ctx: Map[String, String]): String = {
    ""
  }

  /** Generates Dafny code for the given LoRe TArith.
    *
    * @param node The LoRe TArith node.
    * @return The generated Dafny code.
    */
  private def generateFromTArith(node: TArith, ctx: Map[String, String]): String = {
    // FYI: Modulo and Unary Minus do not exist in LoRe, but do in Dafny
    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-numeric-types
    val expr: String = node match
      case n: TNum => generateFromTNum(n, ctx)
      case n: TDiv => generateFromTDiv(n, ctx)
      case n: TMul => generateFromTMul(n, ctx)
      case n: TAdd => generateFromTAdd(n, ctx)
      case n: TSub => generateFromTSub(n, ctx)

    node match
      case n: TNum => expr // Simple numbers don't need parens as there is no nesting at this level.
      case _ => s"($expr)" // Surround with parens to respect expression nesting as instructed by the AST node nesting.
  }

  /** Generates Dafny code for the given LoRe TNum.
    *
    * @param node The LoRe TNum node.
    * @return The generated Dafny code.
    */
  private def generateFromTNum(node: TNum, ctx: Map[String, String]): String = {
    // Transforming an integer into a string to output a number may seem odd,
    // but in reality it'll be a number in code as it's not surrounded by quotes.
    node.value.toString
  }

  /** Generates Dafny code for the given LoRe TDiv.
    *
    * @param node The LoRe TDiv node.
    * @return The generated Dafny code.
    */
  private def generateFromTDiv(node: TDiv, ctx: Map[String, String]): String = {
    s"${generate(node.left, ctx)} / ${generate(node.right, ctx)}"
  }

  /** Generates Dafny code for the given LoRe TMul.
    *
    * @param node The LoRe TMul node.
    * @return The generated Dafny code.
    */
  private def generateFromTMul(node: TMul, ctx: Map[String, String]): String = {
    s"${generate(node.left, ctx)} * ${generate(node.right, ctx)}"
  }

  /** Generates Dafny code for the given LoRe TAdd.
    *
    * @param node The LoRe TAdd node.
    * @return The generated Dafny code.
    */
  private def generateFromTAdd(node: TAdd, ctx: Map[String, String]): String = {
    s"${generate(node.left, ctx)} + ${generate(node.right, ctx)}"
  }

  /** Generates Dafny code for the given LoRe TSub.
    *
    * @param node The LoRe TSub node.
    * @return The generated Dafny code.
    */
  private def generateFromTSub(node: TSub, ctx: Map[String, String]): String = {
    s"${generate(node.left, ctx)} - ${generate(node.right, ctx)}"
  }

  /** Generates Dafny code for the given LoRe TBoolean.
    *
    * @param node The LoRe TBoolean node.
    * @return The generated Dafny code.
    */
  private def generateFromTBoolean(node: TBoolean, ctx: Map[String, String]): String = {
    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-booleans
    val expr: String = node match
      case n: TTrue       => generateFromTTrue(n, ctx)
      case n: TFalse      => generateFromTFalse(n, ctx)
      case n: TNeg        => generateFromTNeg(n, ctx)
      case n: TLt         => generateFromTLt(n, ctx)
      case n: TGt         => generateFromTGt(n, ctx)
      case n: TLeq        => generateFromTLeq(n, ctx)
      case n: TGeq        => generateFromTGeq(n, ctx)
      case n: TEq         => generateFromTEq(n, ctx)
      case n: TIneq       => generateFromTIneq(n, ctx)
      case n: TDisj       => generateFromTDisj(n, ctx)
      case n: TConj       => generateFromTConj(n, ctx)
      case n: TImpl       => generateFromTImpl(n, ctx)
      case n: TBImpl      => generateFromTBImpl(n, ctx)
      case n: TInSet      => generateFromTInSet(n, ctx)
      case n: TQuantifier => generateFromTQuantifier(n, ctx)

    node match
      case n: (TTrue | TFalse) => expr // Simple booleans don't need parens because there is no nesting at this level.
      case _ => s"($expr)" // Surround with parens to respect expression nesting as instructed by the AST node nesting.
  }

  /** Generates Dafny code for the given LoRe TTrue.
    *
    * @param node The LoRe TTrue node.
    * @return The generated Dafny code.
    */
  private def generateFromTTrue(node: TTrue, ctx: Map[String, String]): String = {
    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-booleans
    "true"
  }

  /** Generates Dafny code for the given LoRe TFalse.
    *
    * @param node The LoRe TFalse node.
    * @return The generated Dafny code.
    */
  private def generateFromTFalse(node: TFalse, ctx: Map[String, String]): String = {
    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-booleans
    "false"
  }

  /** Generates Dafny code for the given LoRe TNeg.
    *
    * @param node The LoRe TNeg node.
    * @return The generated Dafny code.
    */
  private def generateFromTNeg(node: TNeg, ctx: Map[String, String]): String = {
    s"!${generate(node.body, ctx)}"
  }

  /** Generates Dafny code for the given LoRe TLt.
    *
    * @param node The LoRe TLt node.
    * @return The generated Dafny code.
    */
  private def generateFromTLt(node: TLt, ctx: Map[String, String]): String = {
    s"${generate(node.left, ctx)} < ${generate(node.right, ctx)}"
  }

  /** Generates Dafny code for the given LoRe TGt.
    *
    * @param node The LoRe TGt node.
    * @return The generated Dafny code.
    */
  private def generateFromTGt(node: TGt, ctx: Map[String, String]): String = {
    s"${generate(node.left, ctx)} > ${generate(node.right, ctx)}"
  }

  /** Generates Dafny code for the given LoRe TLeq.
    *
    * @param node The LoRe TLeq node.
    * @return The generated Dafny code.
    */
  private def generateFromTLeq(node: TLeq, ctx: Map[String, String]): String = {
    s"${generate(node.left, ctx)} <= ${generate(node.right, ctx)}"
  }

  /** Generates Dafny code for the given LoRe TGeq.
    *
    * @param node The LoRe TGeq node.
    * @return The generated Dafny code.
    */
  private def generateFromTGeq(node: TGeq, ctx: Map[String, String]): String = {
    s"${generate(node.left, ctx)} >= ${generate(node.right, ctx)}"
  }

  /** Generates Dafny code for the given LoRe TEq.
    *
    * @param node The LoRe TEq node.
    * @return The generated Dafny code.
    */
  private def generateFromTEq(node: TEq, ctx: Map[String, String]): String = {
    s"${generate(node.left, ctx)} == ${generate(node.right, ctx)}"
  }

  /** Generates Dafny code for the given LoRe TIneq.
    *
    * @param node The LoRe TIneq node.
    * @return The generated Dafny code.
    */
  private def generateFromTIneq(node: TIneq, ctx: Map[String, String]): String = {
    s"${generate(node.left, ctx)} != ${generate(node.right, ctx)}"
  }

  /** Generates Dafny code for the given LoRe TDisj.
    *
    * @param node The LoRe TDisj node.
    * @return The generated Dafny code.
    */
  private def generateFromTDisj(node: TDisj, ctx: Map[String, String]): String = {
    s"${generate(node.left, ctx)} || ${generate(node.right, ctx)}"
  }

  /** Generates Dafny code for the given LoRe TConj.
    *
    * @param node The LoRe TConj node.
    * @return The generated Dafny code.
    */
  private def generateFromTConj(node: TConj, ctx: Map[String, String]): String = {
    s"${generate(node.left, ctx)} && ${generate(node.right, ctx)}"
  }

  /** Generates Dafny code for the given LoRe TImpl.
    *
    * @param node The LoRe TImpl node.
    * @return The generated Dafny code.
    */
  private def generateFromTImpl(node: TImpl, ctx: Map[String, String]): String = {
    // FYI: Dafny also supports a "reverse implication", i.e. right implies left, but this doesn't exist in LoRe.
    s"${generate(node.left, ctx)} ==> ${generate(node.right, ctx)}"
  }

  /** Generates Dafny code for the given LoRe TBImpl.
    *
    * @param node The LoRe TBImpl node.
    * @return The generated Dafny code.
    */
  private def generateFromTBImpl(node: TBImpl, ctx: Map[String, String]): String = {
    s"${generate(node.left, ctx)} <==> ${generate(node.right, ctx)}"
  }

  /** Generates Dafny code for the given LoRe TInSet.
    *
    * @param node The LoRe TInSet node.
    * @return The generated Dafny code.
    */
  private def generateFromTInSet(node: TInSet, ctx: Map[String, String]): String = {
    // FYI: Dafny has a syntactic shorthand for in-set negation: "x !in y". This does not exist in LoRe.
    s"${generate(node.left, ctx)} in ${generate(node.right, ctx)}"
  }

  /** Generates Dafny code for the given LoRe TQuantifier.
    *
    * @param node The LoRe TQuantifier node.
    * @return The generated Dafny code.
    */
  private def generateFromTQuantifier(node: TQuantifier, ctx: Map[String, String]): String = {
    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-quantifier-expression
    val expr: String = node match
      case n: TForall => generateFromTForall(n, ctx)
      case n: TExists => generateFromTExists(n, ctx)

    s"($expr)" // Surround with parens to respect expression nesting as instructed by the AST node nesting.
  }

  /** Generates Dafny code for the given LoRe TParens.
    *
    * @param node The LoRe TParens node.
    * @return The generated Dafny code.
    */
  private def generateFromTParens(node: TParens, ctx: Map[String, String]): String = {
    // This node simply surrounds the contained expression with parens.
    s"(${generate(node.inner, ctx)})"
  }

  /** Generates Dafny code for the given LoRe TString.
    *
    * @param node The LoRe TString node.
    * @return The generated Dafny code.
    */
  private def generateFromTString(node: TString, ctx: Map[String, String]): String = {
    // Surround by quotes so it's an actual string within the resulting Dafny code.
    // Could technically also be output as a sequence of chars, if this was desired.
    // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-strings
    s"\"${node.value}\""
  }

  /** Generates Dafny code for the given LoRe TFAcc.
    *
    * @param node The LoRe TFAcc node.
    * @return The generated Dafny code.
    */
  private def generateFromTFAcc(node: TFAcc, ctx: Map[String, String]): String = {
    val fieldAccess: String = node match
      case n: TFCall  => generateFromTFCall(n, ctx)
      case n: TFCurly => generateFromTFCurly(n, ctx)

    fieldAccess
  }

  /** Generates Dafny code for the given LoRe TFCall.
    *
    * @param node The LoRe TFCall node.
    * @return The generated Dafny code.
    */
  private def generateFromTFCall(node: TFCall, ctx: Map[String, String]): String = {
    if node.args == null then {
      // Property (field) access

      // References:
      // https://dafny.org/dafny/DafnyRef/DafnyRef#sec-field-declaration
      // https://dafny.org/dafny/DafnyRef/DafnyRef#sec-class-types
      s"${generate(node.parent, ctx)}.${node.field}"
    } else {
      // Method access

      // Reference: https://dafny.org/dafny/DafnyRef/DafnyRef#sec-method-declaration
      val args: List[String] = node.args.map(arg => generate(arg, ctx))
      s"${generate(node.parent, ctx)}.${node.field}(${args.mkString(", ")})"
    }
  }

  /** Generates Dafny code for the given LoRe TFunC.
    *
    * @param node The LoRe TFunc node.
    * @return The generated Dafny code.
    */
  private def generateFromTFunC(node: TFunC, ctx: Map[String, String]): String = {
    // References:
    // https://dafny.org/dafny/DafnyRef/DafnyRef#sec-function-declaration
    // https://dafny.org/dafny/DafnyRef/DafnyRef#sec-maps
    // https://dafny.org/dafny/DafnyRef/DafnyRef#sec-sets
    node.name match
      case "Map" =>
        // Map instantiations differ from regular function calls.
        // Each map pair is a 2-tuple (i.e. length 2 TTuple in LoRe).
        val mapKeyValues: Seq[String] = node.args.map(kv => {
          // Simply throwing these TTuples to generate would give us tuple syntax, not map syntax.
          // Therefore, generate key and value separately and combine them with appropriate Dafny syntax.
          val keyValueTuple: TTuple = kv.asInstanceOf[TTuple]
          val key: String           = generate(keyValueTuple.factors.head, ctx)
          val value: String         = generate(keyValueTuple.factors.last, ctx)
          s"$key := $value"
        })
        s"map[${mapKeyValues.mkString(", ")}]"
      case "List" =>
        // List instantiations also differ, these are turned into Dafny sets.
        val items: Seq[String] = node.args.map(i => generate(i, ctx))
        s"[${items.mkString(", ")}]"
      case _ =>
        val args: Seq[String] = node.args.map(arg => generate(arg, ctx))
        s"${node.name}(${args.mkString(", ")})"
  }

  /* Term types that are not covered currently, and should error */

  /** Generates Dafny code for the given LoRe TViperImport.
    *
    * @param node The LoRe TViperImport node.
    * @return The generated Dafny code.
    */
  private def generateFromTViperImport(node: TViperImport, ctx: Map[String, String]): String = {
    // Viper-specific (by name at least). Leave out for now.
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TTypeAl.
    *
    * @param node The LoRe TTypeAl node.
    * @return The generated Dafny code.
    */
  private def generateFromTTypeAl(node: TTypeAl, ctx: Map[String, String]): String = {
    // Leave out for now. Maybe in a later work.
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TAssert.
    *
    * @param node The LoRe TAssert node.
    * @return The generated Dafny code.
    */
  private def generateFromTAssert(node: TAssert, ctx: Map[String, String]): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TAssume.
    *
    * @param node The LoRe TAssume node.
    * @return The generated Dafny code.
    */
  private def generateFromTAssume(node: TAssume, ctx: Map[String, String]): String = {
    throw new Error("Term type not implemented")
  }

  // TODO: Implement. These are _not_ Dafny's loop invariants.
  /** Generates Dafny code for the given LoRe TInvariant.
    *
    * @param node The LoRe TInvariant node.
    * @return The generated Dafny code.
    */
  private def generateFromTInvariant(node: TInvariant, ctx: Map[String, String]): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TForall.
    *
    * @param node The LoRe TForall node.
    * @return The generated Dafny code.
    */
  private def generateFromTForall(node: TForall, ctx: Map[String, String]): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TExists.
    *
    * @param node The LoRe TExists node.
    * @return The generated Dafny code.
    */
  private def generateFromTExists(node: TExists, ctx: Map[String, String]): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TFCurly.
    *
    * @param node The LoRe TFCurly node.
    * @return The generated Dafny code.
    */
  private def generateFromTFCurly(node: TFCurly, ctx: Map[String, String]): String = {
    // Probably not needed for Dafny.
    throw new Error("Term type not implemented")
  }
}
