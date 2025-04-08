package loreCompilerPlugin.codegen

import lore.ast.*

object DafnyGen {

  /** Takes a Scala/LoRe type name and returns the corresponding Dafny type name, if one exists.
    * E.g. the corresponding Dafny type for the Scala/LoRe type "String" is "string" (note the casing).
    * @param typeName The name of the Scala/LoRe type.
    * @return The name of the corresponding Dafny type, or the original parameter if no such correspondence exists.
    */
  private def getDafnyType(typeName: String): String = {
    typeName match
      case "Boolean"          => "bool"
      case "Char"             => "char"
      case "Int"              => "int"
      case "Float" | "Double" => "real"   // Dafny "real" follows SMT-Lib "Real" theory, so should be fine to map onto
      case "String"           => "string" // Technically seq<char> (String is Seq[Char]), syntactic sugar respectively
      case "Map"              => "map"
      case "List"             => "seq"    // This does confer some changes in semantics regarding mutability etc.
      // TODO: Add others...
      case _ => typeName
  }

  /** Generates Dafny code for the given LoRe term.
    *
    * @param node The LoRe AST node.
    * @return The generated Dafny code.
    */
  def generate(node: Term): String = {
    node match
      // Cases ordered by order in LoRe AST definition
      case n: TViperImport => generateFromTViperImport(n) // Viper
      case n: TArgT        => generateFromTArgT(n)
      case n: TVar         => generateFromTVar(n)
      case n: TAbs         => generateFromTAbs(n)
      case n: TTuple       => generateFromTTuple(n)
      case n: TIf          => generateFromTIf(n)
      case n: TSeq         => generateFromTSeq(n)
      case n: TArrow       => generateFromTArrow(n)
      case n: TTypeAl      => generateFromTTypeAl(n)
      case n: TAssert      => generateFromTAssert(n)      // Viper
      case n: TAssume      => generateFromTAssume(n)      // Viper
      case n: TReactive    => generateFromTReactive(n)
      case n: TInteraction => generateFromTInteraction(n) // Attention: Not a part of TReactive
      case n: TInvariant   => generateFromTInvariant(n)
      case n: TArith       => generateFromTArith(n)
      case n: TBoolean     => generateFromTBoolean(n)
      case n: TParens      => generateFromTParens(n)
      case n: TString      => generateFromTString(n)
      case n: TFAcc        => generateFromTFAcc(n)
      case n: TFunC        => generateFromTFunC(n)
  }

  /** Generates a Dafny Type annotation for the given LoRe Type node.
    *
    * @param node The LoRe Type node.
    * @return The generated Dafny Type annotation.
    */
  private def generateFromTypeNode(node: Type): String = {
    node match
      case n: SimpleType => generateFromSimpleType(n)
      case n: TupleType  => generateFromTupleType(n)
  }

  /** Generates a Dafny Type annotation for the given LoRe SimpleType node.
    *
    * @param node The LoRe SimpleType node.
    * @return The generated Dafny Type annotation.
    */
  private def generateFromSimpleType(node: SimpleType): String = {
    val dafnyType: String       = getDafnyType(node.name)
    val innerList: List[String] = node.inner.map(t => generateFromTypeNode(t))

    if dafnyType.matches("Tuple\\d+") then {
      // The name of Dafny's tuple type is blank, and instead of angled brackets uses square ones, e.g. (string, int).
      // That means we just concat the inner types surrounded by parens for building tuple type annotations.
      s"(${innerList.mkString(", ")})"
    } else {
      val inner: String = if innerList.isEmpty then "" else s"<${innerList.mkString(", ")}>"
      s"$dafnyType$inner"
    }
  }

  /** Generates Dafny code for the given LoRe TArgT.
    *
    * @param node The LoRe TArgT node.
    * @return The generated Dafny code.
    */
  private def generateFromTArgT(node: TArgT): String = {
    val typeAnnot: String = generateFromTypeNode(node._type)

    s"${node.name}: $typeAnnot"
  }

  /** Generates Dafny code for the given LoRe TVar.
    *
    * @param node The LoRe TVar node.
    * @return The generated Dafny code.
    */
  private def generateFromTVar(node: TVar): String = {
    // Just place the variable name in the code
    // TODO: Depending on implementation of the reactives, this may need branching output depending on type of the var
    node.name
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TAbs.
    *
    * @param node The LoRe TAbs node.
    * @return The generated Dafny code.
    */
  private def generateFromTAbs(node: TAbs): String = {
    val typeAnnot: String = generateFromTypeNode(node._type)
    val body: String      = generate(node.body)

    // TODO: Depending on the type, output must differ, debug for now
    node.body match
      case n: TReactive    => ""
      case n: TInteraction => ""
      case _ => if body.isEmpty then s"var ${node.name}: $typeAnnot;" else s"var ${node.name}: $typeAnnot := $body;"
  }

  /** Generates Dafny code for the given LoRe TTuple.
    *
    * @param node The LoRe TTuple node.
    * @return The generated Dafny code.
    */
  private def generateFromTTuple(node: TTuple): String = {
    val elems: List[String] = node.factors.map(t => generate(t))

    s"(${elems.mkString(", ")})"
  }

  /** Generates Dafny code for the given LoRe TIf.
    *
    * @param node The LoRe TIf node.
    * @return The generated Dafny code.
    */
  private def generateFromTIf(node: TIf): String = {
    // TODO: Test
    val cond: String     = generate(node.cond)
    val thenExpr: String = generate(node._then)
    val elseExpr: String = if node._else.isDefined then generate(node._else.get) else ""

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

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TArrow.
    *
    * @param node The LoRe TArrow node.
    * @return The generated Dafny code.
    */
  private def generateFromTArrow(node: TArrow): String = {
    ""
  }

  /** Generates Dafny code for the given LoRe TReactive.
    *
    * @param node The LoRe TReactive node.
    * @return The generated Dafny code.
    */
  private def generateFromTReactive(node: TReactive): String = {
    // TInteraction isn't considered a TReactive so it has to be separate
    val reactive: String = node match
      case n: TSource  => generateFromTSource(n)
      case n: TDerived => generateFromTDerived(n)

    // TODO: Check if it's fine to simply return the reactive like this
    reactive
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TSource.
    *
    * @param node The LoRe TSource node.
    * @return The generated Dafny code.
    */
  private def generateFromTSource(node: TSource): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TDerived.
    *
    * @param node The LoRe TDerived node.
    * @return The generated Dafny code.
    */
  private def generateFromTDerived(node: TDerived): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TInteraction.
    *
    * @param node The LoRe TInteraction node.
    * @return The generated Dafny code.
    */
  private def generateFromTInteraction(node: TInteraction): String = {
    // TInteraction is not a part of TReactive
    ""
  }

  /** Generates Dafny code for the given LoRe TArith.
    *
    * @param node The LoRe TArith node.
    * @return The generated Dafny code.
    */
  private def generateFromTArith(node: TArith): String = {
    val expr: String = node match
      case n: TNum => generateFromTNum(n)
      case n: TAdd => generateFromTAdd(n)
      case n: TSub => generateFromTSub(n)
      case n: TMul => generateFromTMul(n)
      case n: TDiv => generateFromTDiv(n)

    node match
      case n: TNum => expr // Simple numbers don't need braces
      case _ => s"($expr)" // Surround with braces to respect expression nesting as instructed by the AST node nesting
  }

  /** Generates Dafny code for the given LoRe TNum.
    *
    * @param node The LoRe TNum node.
    * @return The generated Dafny code.
    */
  private def generateFromTNum(node: TNum): String = {
    // Transforming to string may seem odd but in reality it'll be a number
    // in code because it's not surrounded by quotes, like a string would be
    node.value.toString
  }

  /** Generates Dafny code for the given LoRe TDiv.
    *
    * @param node The LoRe TDiv node.
    * @return The generated Dafny code.
    */
  private def generateFromTDiv(node: TDiv): String = {
    s"${generate(node.left)} / ${generate(node.right)}"
  }

  /** Generates Dafny code for the given LoRe TMul.
    *
    * @param node The LoRe TMul node.
    * @return The generated Dafny code.
    */
  private def generateFromTMul(node: TMul): String = {
    s"${generate(node.left)} * ${generate(node.right)}"
  }

  /** Generates Dafny code for the given LoRe TAdd.
    *
    * @param node The LoRe TAdd node.
    * @return The generated Dafny code.
    */
  private def generateFromTAdd(node: TAdd): String = {
    s"${generate(node.left)} + ${generate(node.right)}"
  }

  /** Generates Dafny code for the given LoRe TSub.
    *
    * @param node The LoRe TSub node.
    * @return The generated Dafny code.
    */
  private def generateFromTSub(node: TSub): String = {
    s"${generate(node.left)} - ${generate(node.right)}"
  }

  /** Generates Dafny code for the given LoRe TBoolean.
    *
    * @param node The LoRe TBoolean node.
    * @return The generated Dafny code.
    */
  private def generateFromTBoolean(node: TBoolean): String = {
    val expr: String = node match
      case n: TTrue       => generateFromTTrue(n)
      case n: TFalse      => generateFromTFalse(n)
      case n: TNeg        => generateFromTNeg(n)
      case n: TLt         => generateFromTLt(n)
      case n: TGt         => generateFromTGt(n)
      case n: TLeq        => generateFromTLeq(n)
      case n: TGeq        => generateFromTGeq(n)
      case n: TEq         => generateFromTEq(n)
      case n: TIneq       => generateFromTIneq(n)
      case n: TDisj       => generateFromTDisj(n)
      case n: TConj       => generateFromTConj(n)
      case n: TImpl       => generateFromTImpl(n)
      case n: TBImpl      => generateFromTBImpl(n)
      case n: TInSet      => generateFromTInSet(n)
      case n: TQuantifier => generateFromTQuantifier(n)

    node match
      case n: (TTrue | TFalse) => expr // Simple booleans don't need braces
      case _ => s"($expr)" // Surround with braces to respect expression nesting as instructed by the AST node nesting
  }

  /** Generates Dafny code for the given LoRe TTrue.
    *
    * @param node The LoRe TTrue node.
    * @return The generated Dafny code.
    */
  private def generateFromTTrue(node: TTrue): String = {
    true.toString
  }

  /** Generates Dafny code for the given LoRe TFalse.
    *
    * @param node The LoRe TFalse node.
    * @return The generated Dafny code.
    */
  private def generateFromTFalse(node: TFalse): String = {
    false.toString
  }

  /** Generates Dafny code for the given LoRe TNeg.
    *
    * @param node The LoRe TNeg node.
    * @return The generated Dafny code.
    */
  private def generateFromTNeg(node: TNeg): String = {
    s"!${generate(node.body)}"
  }

  /** Generates Dafny code for the given LoRe TLt.
    *
    * @param node The LoRe TLt node.
    * @return The generated Dafny code.
    */
  private def generateFromTLt(node: TLt): String = {
    s"${generate(node.left)} < ${generate(node.right)}"
  }

  /** Generates Dafny code for the given LoRe TGt.
    *
    * @param node The LoRe TGt node.
    * @return The generated Dafny code.
    */
  private def generateFromTGt(node: TGt): String = {
    s"${generate(node.left)} > ${generate(node.right)}"
  }

  /** Generates Dafny code for the given LoRe TLeq.
    *
    * @param node The LoRe TLeq node.
    * @return The generated Dafny code.
    */
  private def generateFromTLeq(node: TLeq): String = {
    s"${generate(node.left)} <= ${generate(node.right)}"
  }

  /** Generates Dafny code for the given LoRe TGeq.
    *
    * @param node The LoRe TGeq node.
    * @return The generated Dafny code.
    */
  private def generateFromTGeq(node: TGeq): String = {
    s"${generate(node.left)} >= ${generate(node.right)}"
  }

  /** Generates Dafny code for the given LoRe TEq.
    *
    * @param node The LoRe TEq node.
    * @return The generated Dafny code.
    */
  private def generateFromTEq(node: TEq): String = {
    s"${generate(node.left)} == ${generate(node.right)}"
  }

  /** Generates Dafny code for the given LoRe TIneq.
    *
    * @param node The LoRe TIneq node.
    * @return The generated Dafny code.
    */
  private def generateFromTIneq(node: TIneq): String = {
    s"${generate(node.left)} != ${generate(node.right)}"
  }

  /** Generates Dafny code for the given LoRe TDisj.
    *
    * @param node The LoRe TDisj node.
    * @return The generated Dafny code.
    */
  private def generateFromTDisj(node: TDisj): String = {
    s"${generate(node.left)} || ${generate(node.right)}"
  }

  /** Generates Dafny code for the given LoRe TConj.
    *
    * @param node The LoRe TConj node.
    * @return The generated Dafny code.
    */
  private def generateFromTConj(node: TConj): String = {
    s"${generate(node.left)} && ${generate(node.right)}"
  }

  /** Generates Dafny code for the given LoRe TImpl.
    *
    * @param node The LoRe TImpl node.
    * @return The generated Dafny code.
    */
  private def generateFromTImpl(node: TImpl): String = {
    // TODO: Test
    s"${generate(node.left)} ==> ${generate(node.right)}"
  }

  /** Generates Dafny code for the given LoRe TBImpl.
    *
    * @param node The LoRe TBImpl node.
    * @return The generated Dafny code.
    */
  private def generateFromTBImpl(node: TBImpl): String = {
    // TODO: Test
    s"${generate(node.left)} <==> ${generate(node.right)}"
  }

  /** Generates Dafny code for the given LoRe TInSet.
    *
    * @param node The LoRe TInSet node.
    * @return The generated Dafny code.
    */
  private def generateFromTInSet(node: TInSet): String = {
    // TODO: Test
    s"${generate(node.left)} in ${generate(node.right)}"
  }

  /** Generates Dafny code for the given LoRe TQuantifier.
    *
    * @param node The LoRe TQuantifier node.
    * @return The generated Dafny code.
    */
  private def generateFromTQuantifier(node: TQuantifier): String = {
    val expr: String = node match
      case n: TForall => generateFromTForall(n)
      case n: TExists => generateFromTExists(n)

    s"(${expr})" // Surround with braces to respect expression nesting as instructed by the AST node nesting
  }

  /** Generates Dafny code for the given LoRe TParens.
    *
    * @param node The LoRe TParens node.
    * @return The generated Dafny code.
    */
  private def generateFromTParens(node: TParens): String = {
    // TODO: Test
    s"(${generate(node.inner)})"
  }

  /** Generates Dafny code for the given LoRe TString.
    *
    * @param node The LoRe TString node.
    * @return The generated Dafny code.
    */
  private def generateFromTString(node: TString): String = {
    // Surround by quotes so it's an actual string within the code
    s"\"${node.value}\""
  }

  /** Generates Dafny code for the given LoRe TFAcc.
    *
    * @param node The LoRe TFAcc node.
    * @return The generated Dafny code.
    */
  private def generateFromTFAcc(node: TFAcc): String = {
    val fieldAccess: String = node match
      case n: TFCall  => generateFromTFCall(n)
      case n: TFCurly => generateFromTFCurly(n)

    // TODO: Check if it's fine to simply return the field access like this
    fieldAccess
  }

  /** Generates Dafny code for the given LoRe TFCall.
    *
    * @param node The LoRe TFCall node.
    * @return The generated Dafny code.
    */
  private def generateFromTFCall(node: TFCall): String = {
    if node.args == null then {
      // Property access
      s"${generate(node.parent)}.${node.field}"
    } else {
      // Method access
      val args: List[String] = node.args.map(arg => generate(arg))
      s"${generate(node.parent)}.${node.field}(${args.mkString(", ")})"
    }
  }

  /** Generates Dafny code for the given LoRe TFCurly.
    *
    * @param node The LoRe TFCurly node.
    * @return The generated Dafny code.
    */
  private def generateFromTFCurly(node: TFCurly): String = {
    // TODO: Test
    s"${generate(node.parent)}.${node.field} { ${generate(node.body)} }"
  }

  /** Generates Dafny code for the given LoRe TFunC.
    *
    * @param node The LoRe TFunc node.
    * @return The generated Dafny code.
    */
  private def generateFromTFunC(node: TFunC): String = {
    node.name match
      case "Map" =>
        // Map instantiations differ from regular function calls
        // Each map pair is a 2-tuple (i.e. length 2 TTuple in LoRe)
        val mapKeyValues: Seq[String] = node.args.map(kv => {
          // Simply throwing these TTuples to generate would give us tuple syntax, not map syntax
          // Therefore, generate key and value separately and combine them with appropriate Dafny syntax
          val keyValueTuple: TTuple = kv.asInstanceOf[TTuple]
          val key: String           = generate(keyValueTuple.factors.head)
          val value: String         = generate(keyValueTuple.factors.last)
          s"$key := $value"
        })
        s"map[${mapKeyValues.mkString(", ")}]"
      case "List" =>
        // List instantiations also differ, these are turned into Dafny sets
        val items: Seq[String] = node.args.map(i => generate(i))
        s"[${items.mkString(", ")}]"
      case _ =>
        val args: Seq[String] = node.args.map(arg => generate(arg))
        s"${node.name}(${args.mkString(", ")})"
  }

  /* Term types that are not covered currently, and should error */

  /** Generates Dafny code for the given LoRe TViperImport.
    *
    * @param node The LoRe TViperImport node.
    * @return The generated Dafny code.
    */
  private def generateFromTViperImport(node: TViperImport): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates a Dafny Type annotation for the given LoRe TupleType node.
    *
    * @param node The LoRe TupleType node.
    * @return The generated Dafny Type annotation.
    */
  private def generateFromTupleType(node: TupleType): String = {
    throw new Error("Tuples types not implemented")
  }

  /** Generates Dafny code for the given LoRe TSeq.
    *
    * @param node The LoRe TSeq node.
    * @return The generated Dafny code.
    */
  private def generateFromTSeq(node: TSeq): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TTypeAl.
    *
    * @param node The LoRe TTypeAl node.
    * @return The generated Dafny code.
    */
  private def generateFromTTypeAl(node: TTypeAl): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TAssert.
    *
    * @param node The LoRe TAssert node.
    * @return The generated Dafny code.
    */
  private def generateFromTAssert(node: TAssert): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TAssume.
    *
    * @param node The LoRe TAssume node.
    * @return The generated Dafny code.
    */
  private def generateFromTAssume(node: TAssume): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TInvariant.
    *
    * @param node The LoRe TInvariant node.
    * @return The generated Dafny code.
    */
  private def generateFromTInvariant(node: TInvariant): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TForall.
    *
    * @param node The LoRe TForall node.
    * @return The generated Dafny code.
    */
  private def generateFromTForall(node: TForall): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TExists.
    *
    * @param node The LoRe TExists node.
    * @return The generated Dafny code.
    */
  private def generateFromTExists(node: TExists): String = {
    throw new Error("Term type not implemented")
  }
}
