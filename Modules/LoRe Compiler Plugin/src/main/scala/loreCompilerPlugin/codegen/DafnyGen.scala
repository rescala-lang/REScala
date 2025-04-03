package loreCompilerPlugin.codegen

import lore.ast.*

import scala.annotation.tailrec

object DafnyGen {

  /* Code Generation entry point method */

  /** Generates Dafny code for the given LoRe term.
    *
    * @param node The LoRe AST node.
    * @return The generated Dafny code.
    */
  def generate(node: Term): String = {
    generateFromNode(node)
  }

  /* Method overloads for covered term types */

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TAbs.
    *
    * @param node The LoRe TAbs node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TAbs): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TNum.
    *
    * @param node The LoRe TNum node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TNum): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TString.
    *
    * @param node The LoRe TString node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TString): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TTrue.
    *
    * @param node The LoRe TTrue node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TTrue): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TFalse.
    *
    * @param node The LoRe TFalse node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TFalse): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TVar.
    *
    * @param node The LoRe TVar node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TVar): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TNeg.
    *
    * @param node The LoRe TNeg node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TNeg): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TFCall.
    *
    * @param node The LoRe TFCall node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TFCall): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TAdd.
    *
    * @param node The LoRe TAdd node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TAdd): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TSub.
    *
    * @param node The LoRe TSub node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TSub): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TMul.
    *
    * @param node The LoRe TMul node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TMul): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TDiv.
    *
    * @param node The LoRe TDiv node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TDiv): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TConj.
    *
    * @param node The LoRe TConj node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TConj): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TDisj.
    *
    * @param node The LoRe TDisj node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TDisj): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TLt.
    *
    * @param node The LoRe TLt node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TLt): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TGt.
    *
    * @param node The LoRe TGt node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TGt): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TLeq.
    *
    * @param node The LoRe TLeq node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TLeq): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TGeq.
    *
    * @param node The LoRe TGeq node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TGeq): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TEq.
    *
    * @param node The LoRe TEq node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TEq): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TIneq.
    *
    * @param node The LoRe TIneq node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TIneq): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TFunC.
    *
    * @param node The LoRe TFunc node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TFunC): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TInteraction.
    *
    * @param node The LoRe TInteraction node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TInteraction): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TSource.
    *
    * @param node The LoRe TSource node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TSource): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TDerived.
    *
    * @param node The LoRe TDerived node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TDerived): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TArrow.
    *
    * @param node The LoRe TArrow node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TArrow): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TTuple.
    *
    * @param node The LoRe TTuple node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TTuple): String = {
    ""
  }

  // TODO: Implement
  /** Generates Dafny code for the given LoRe TArgT.
    *
    * @param node The LoRe TArgT node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TArgT): String = {
    ""
  }

  /* Term types that are not covered currently, and should error */

  /** Generates Dafny code for the given LoRe TInvariant.
    *
    * @param node The LoRe TInvariant node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TInvariant): String = {
    throw new Error("Term type not implemented")
  }

  /* ---------- Viper-specific term types ----------  */

  /** Generates Dafny code for the given LoRe TViperImport.
    *
    * @param node The LoRe TViperImport node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TViperImport): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TAssert.
    *
    * @param node The LoRe TAssert node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TAssert): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TAssume.
    *
    * @param node The LoRe TAssume node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TAssume): String = {
    throw new Error("Term type not implemented")
  }

  /* ---------- Terms for types ----------  */

  /** Generates Dafny code for the given LoRe SimpleType.
    *
    * @param node The LoRe SimpleType node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: SimpleType): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TupleType.
    *
    * @param node The LoRe TupleType node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TupleType): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TTypeAl.
    *
    * @param node The LoRe TTypeAl node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TTypeAl): String = {
    throw new Error("Term type not implemented")
  }

  /* ---------- Operator term types ----------  */

  /** Generates Dafny code for the given LoRe TImpl.
    *
    * @param node The LoRe TImpl node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TImpl): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TBImpl.
    *
    * @param node The LoRe TBImpl node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TBImpl): String = {
    throw new Error("Term type not implemented")
  }

  /* ---------- Quantifier term types ----------  */

  /** Generates Dafny code for the given LoRe TInSet.
    *
    * @param node The LoRe TInSet node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TInSet): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TForall.
    *
    * @param node The LoRe TForall node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TForall): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TExists.
    *
    * @param node The LoRe TExists node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TExists): String = {
    throw new Error("Term type not implemented")
  }

  /* ---------- Other term types ----------  */

  /** Generates Dafny code for the given LoRe TIf.
    *
    * @param node The LoRe TIf node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TIf): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TSeq.
    *
    * @param node The LoRe TSeq node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TSeq): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TParens.
    *
    * @param node The LoRe TParens node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TParens): String = {
    throw new Error("Term type not implemented")
  }

  /** Generates Dafny code for the given LoRe TFCurly.
    *
    * @param node The LoRe TFCurly node.
    * @return The generated Dafny code.
    */
  def generateFromNode(node: TFCurly): String = {
    throw new Error("Term type not implemented")
  }
}
