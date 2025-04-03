package loreCompilerPlugin.codegen

import lore.ast.*

object DafnyGen {

  /* Code Generation entry point method */

  /** Generates Dafny code for the given LoRe term.
    *
    * @param node The LoRe AST node.
    * @return The generated Dafny code.
    */
  def generate(node: Term): String = {
    node match
      case n: TAbs         => generateFromTAbs(n)
      case n: TNum         => generateFromTNum(n)
      case n: TString      => generateFromTString(n)
      case n: TTrue        => generateFromTTrue(n)
      case n: TFalse       => generateFromTFalse(n)
      case n: TVar         => generateFromTVar(n)
      case n: TNeg         => generateFromTNeg(n)
      case n: TFCall       => generateFromTFCall(n)
      case n: TAdd         => generateFromTAdd(n)
      case n: TSub         => generateFromTSub(n)
      case n: TMul         => generateFromTMul(n)
      case n: TDiv         => generateFromTDiv(n)
      case n: TConj        => generateFromTConj(n)
      case n: TDisj        => generateFromTDisj(n)
      case n: TLt          => generateFromTLt(n)
      case n: TGt          => generateFromTGt(n)
      case n: TLeq         => generateFromTLeq(n)
      case n: TGeq         => generateFromTGeq(n)
      case n: TEq          => generateFromTEq(n)
      case n: TIneq        => generateFromTIneq(n)
      case n: TFunC        => generateFromTFunC(n)
      case n: TInteraction => generateFromTInteraction(n)
      case n: TSource      => generateFromTSource(n)
      case n: TDerived     => generateFromTDerived(n)
      case n: TArrow       => generateFromTArrow(n)
      case n: TTuple       => generateFromTTuple(n)
      case n: TArgT        => generateFromTArgT(n)
      case n: TInvariant   => generateFromTInvariant(n)
      case n: TViperImport => generateFromTViperImport(n)
      case n: TAssert      => generateFromTAssert(n)
      case n: TAssume      => generateFromTAssume(n)
      case n: TImpl        => generateFromTImpl(n)
      case n: TBImpl       => generateFromTBImpl(n)
      case n: TInSet       => generateFromTInSet(n)
      case n: TForall      => generateFromTForall(n)
      case n: TExists      => generateFromTExists(n)
      case n: TTypeAl      => generateFromTTypeAl(n)
      case n: TIf          => generateFromTIf(n)
      case n: TSeq         => generateFromTSeq(n)
      case n: TParens      => generateFromTParens(n)
      case n: TFCurly      => generateFromTFCurly(n)
  }

  /* Method overloads for covered term types */

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TAbs.
    *
    * @param node The LoRe TAbs node.
    * @return The generated Dafny code.
    */
  private def generateFromTAbs(node: TAbs): String = {
    // TODO: This is not properly functional, as it only generates the body, but not the definition!
    generate(node.body)
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

  /** Generates Dafny code for the given LoRe TString.
    *
    * @param node The LoRe TString node.
    * @return The generated Dafny code.
    */
  private def generateFromTString(node: TString): String = {
    // Surround by quotes so it's an actual string within the code
    s"\"${node.value}\""
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

  /** Generates Dafny code for the given LoRe TVar.
    *
    * @param node The LoRe TVar node.
    * @return The generated Dafny code.
    */
  private def generateFromTVar(node: TVar): String = {
    // Just place the variable name in the code
    node.name
  }

  /** Generates Dafny code for the given LoRe TNeg.
    *
    * @param node The LoRe TNeg node.
    * @return The generated Dafny code.
    */
  private def generateFromTNeg(node: TNeg): String = {
    // Surround with braces to respect nesting as instructed by the AST node nesting
    s"(!${generate(node.body)})"
  }

  /** Generates Dafny code for the given LoRe TFCall.
    *
    * @param node The LoRe TFCall node.
    * @return The generated Dafny code.
    */
  private def generateFromTFCall(node: TFCall): String = {
    // TODO: 0-argument methods vs properties?
    if node.args.nonEmpty then {
      val args: Seq[String] = node.args.map(arg => generate(arg))
      s"${generate(node.parent)}.${node.field}(${args.mkString(", ")})"
    } else {
      s"${generate(node.parent)}.${node.field}"
    }
  }

  /** Generates Dafny code for the given LoRe TAdd.
    *
    * @param node The LoRe TAdd node.
    * @return The generated Dafny code.
    */
  private def generateFromTAdd(node: TAdd): String = {
    // Surround with braces to respect nesting as instructed by the AST node nesting
    s"(${generate(node.left)} + ${generate(node.right)})"
  }

  /** Generates Dafny code for the given LoRe TSub.
    *
    * @param node The LoRe TSub node.
    * @return The generated Dafny code.
    */
  private def generateFromTSub(node: TSub): String = {
    // Surround with braces to respect nesting as instructed by the AST node nesting
    s"(${generate(node.left)} - ${generate(node.right)})"
  }

  /** Generates Dafny code for the given LoRe TMul.
    *
    * @param node The LoRe TMul node.
    * @return The generated Dafny code.
    */
  private def generateFromTMul(node: TMul): String = {
    // Surround with braces to respect nesting as instructed by the AST node nesting
    s"(${generate(node.left)} * ${generate(node.right)})"
  }

  /** Generates Dafny code for the given LoRe TDiv.
    *
    * @param node The LoRe TDiv node.
    * @return The generated Dafny code.
    */
  private def generateFromTDiv(node: TDiv): String = {
    // Surround with braces to respect nesting as instructed by the AST node nesting
    s"(${generate(node.left)} / ${generate(node.right)})"
  }

  /** Generates Dafny code for the given LoRe TConj.
    *
    * @param node The LoRe TConj node.
    * @return The generated Dafny code.
    */
  private def generateFromTConj(node: TConj): String = {
    // Surround with braces to respect nesting as instructed by the AST node nesting
    s"(${generate(node.left)} && ${generate(node.right)})"
  }

  /** Generates Dafny code for the given LoRe TDisj.
    *
    * @param node The LoRe TDisj node.
    * @return The generated Dafny code.
    */
  private def generateFromTDisj(node: TDisj): String = {
    // Surround with braces to respect nesting as instructed by the AST node nesting
    s"(${generate(node.left)} || ${generate(node.right)})"
  }

  /** Generates Dafny code for the given LoRe TLt.
    *
    * @param node The LoRe TLt node.
    * @return The generated Dafny code.
    */
  private def generateFromTLt(node: TLt): String = {
    // Surround with braces to respect nesting as instructed by the AST node nesting
    s"(${generate(node.left)} < ${generate(node.right)})"
  }

  /** Generates Dafny code for the given LoRe TGt.
    *
    * @param node The LoRe TGt node.
    * @return The generated Dafny code.
    */
  private def generateFromTGt(node: TGt): String = {
    // Surround with braces to respect nesting as instructed by the AST node nesting
    s"(${generate(node.left)} > ${generate(node.right)})"
  }

  /** Generates Dafny code for the given LoRe TLeq.
    *
    * @param node The LoRe TLeq node.
    * @return The generated Dafny code.
    */
  private def generateFromTLeq(node: TLeq): String = {
    // Surround with braces to respect nesting as instructed by the AST node nesting
    s"(${generate(node.left)} <= ${generate(node.right)})"
  }

  /** Generates Dafny code for the given LoRe TGeq.
    *
    * @param node The LoRe TGeq node.
    * @return The generated Dafny code.
    */
  private def generateFromTGeq(node: TGeq): String = {
    // Surround with braces to respect nesting as instructed by the AST node nesting
    s"(${generate(node.left)} >= ${generate(node.right)})"
  }

  /** Generates Dafny code for the given LoRe TEq.
    *
    * @param node The LoRe TEq node.
    * @return The generated Dafny code.
    */
  private def generateFromTEq(node: TEq): String = {
    // Surround with braces to respect nesting as instructed by the AST node nesting
    s"(${generate(node.left)} == ${generate(node.right)})"
  }

  /** Generates Dafny code for the given LoRe TIneq.
    *
    * @param node The LoRe TIneq node.
    * @return The generated Dafny code.
    */
  private def generateFromTIneq(node: TIneq): String = {
    // Surround with braces to respect nesting as instructed by the AST node nesting
    s"(${generate(node.left)} != ${generate(node.right)})"
  }

  /** Generates Dafny code for the given LoRe TFunC.
    *
    * @param node The LoRe TFunc node.
    * @return The generated Dafny code.
    */
  private def generateFromTFunC(node: TFunC): String = {
    val args: Seq[String] = node.args.map(arg => generate(arg))
    s"${node.name}(${args.mkString(", ")})"
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TInteraction.
    *
    * @param node The LoRe TInteraction node.
    * @return The generated Dafny code.
    */
  private def generateFromTInteraction(node: TInteraction): String = {
    ""
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
  /** Generates Dafny code for the given LoRe TArrow.
    *
    * @param node The LoRe TArrow node.
    * @return The generated Dafny code.
    */
  private def generateFromTArrow(node: TArrow): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TTuple.
    *
    * @param node The LoRe TTuple node.
    * @return The generated Dafny code.
    */
  private def generateFromTTuple(node: TTuple): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TArgT.
    *
    * @param node The LoRe TArgT node.
    * @return The generated Dafny code.
    */
  private def generateFromTArgT(node: TArgT): String = {
    ""
  }

  /* Term types that are not covered currently, and should error */

  /** Generates Dafny code for the given LoRe TInvariant.
    *
    * @param node The LoRe TInvariant node.
    * @return The generated Dafny code.
    */
  private def generateFromTInvariant(node: TInvariant): String = {
    throw new Error("Term type not implemented")
  }

  /* ---------- Viper-specific term types ----------  */

  /** Generates Dafny code for the given LoRe TViperImport.
    *
    * @param node The LoRe TViperImport node.
    * @return The generated Dafny code.
    */
  private def generateFromTViperImport(node: TViperImport): String = {
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

  /* ---------- Operator term types ----------  */

  /** Generates Dafny code for the given LoRe TImpl.
    *
    * @param node The LoRe TImpl node.
    * @return The generated Dafny code.
    */
  private def generateFromTImpl(node: TImpl): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TBImpl.
    *
    * @param node The LoRe TBImpl node.
    * @return The generated Dafny code.
    */
  private def generateFromTBImpl(node: TBImpl): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TInSet.
    *
    * @param node The LoRe TInSet node.
    * @return The generated Dafny code.
    */
  private def generateFromTInSet(node: TInSet): String = {
    throw new Error("Term type not implemented")
  }

  /* ---------- Quantifier term types ----------  */

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

  /* ---------- Other term types ----------  */

  /** Generates Dafny code for the given LoRe TTypeAl.
    *
    * @param node The LoRe TTypeAl node.
    * @return The generated Dafny code.
    */
  private def generateFromTTypeAl(node: TTypeAl): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TIf.
    *
    * @param node The LoRe TIf node.
    * @return The generated Dafny code.
    */
  private def generateFromTIf(node: TIf): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TSeq.
    *
    * @param node The LoRe TSeq node.
    * @return The generated Dafny code.
    */
  private def generateFromTSeq(node: TSeq): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TParens.
    *
    * @param node The LoRe TParens node.
    * @return The generated Dafny code.
    */
  private def generateFromTParens(node: TParens): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TFCurly.
    *
    * @param node The LoRe TFCurly node.
    * @return The generated Dafny code.
    */
  private def generateFromTFCurly(node: TFCurly): String = {
    throw new Error("Term type not implemented")
  }
}
