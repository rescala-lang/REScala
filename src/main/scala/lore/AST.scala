package lore

import cats.data.NonEmptyList

/** The abstract syntax of the LoRe language.
  */
object AST:
  sealed trait Term

  // helper types
  type ID = String
  sealed trait Type
  case class SimpleType(name: String, inner: List[Type]) extends Type
  case class TupleType(inner: NonEmptyList[Type]) extends Type
  type Number = Int
  case class TArgT(name: ID, _type: Type)
      extends Term // argument with type annotation

  // basic terms
  case class TVar(name: ID) extends Term // variable
  case class TAbs(name: ID, _type: Type, body: Term)
      extends Term // abstractions

  case class TIf(cond: Term, _then: Term, _else: Option[Term]) extends Term
  // case class TApp(left: Term, right: Term) extends Term // application
  // case class TUnit() extends Term // unit

  // derived forms
  // case class TSeq(left: Term, right: Term) extends Term // sequence
  case class TArrow(left: Term, right: Term) extends Term // anonymous functions
  case class TTypeAl(name: ID, _type: Type) extends Term // type aliases

  // Viper terms
  sealed trait TViper() extends Term

  // reactives
  sealed trait TReactive extends Term:
    val body: Term
  case class TSource(body: Term) extends TReactive
  case class TDerived(body: Term) extends TReactive

  // interactions
  case class TInteraction(
      reactiveTypes: List[Type],
      argumentTypes: List[Type],
      modifies: List[ID] = List(),
      requires: List[TViper] = List(),
      ensures: List[TViper] = List(),
      executes: Option[Term] = None
  ) extends Term

  // invariants
  case class TInvariant(
      condition: TBoolean
  ) extends Term

  // arithmetic expressions
  sealed trait TArith extends Term with TViper
  case class TNum(value: Number) extends TArith // numbers
  case class TDiv(left: Term, right: Term) extends TArith // division
  case class TMul(left: Term, right: Term) extends TArith // multiplication
  case class TAdd(left: Term, right: Term) extends TArith // addition
  case class TSub(left: Term, right: Term) extends TArith // substraction

  // boolean expressions
  sealed trait TBoolean extends Term with TViper
  // trait TwoChildren:
  //   val left: Term
  //   val right: Term
  // trait Traversable[A]:
  //   extension (a: A) def traverse(fun: Term => Term): A

  // given Traversable[TBoolean with TwoChildren] with
  //   extension (t: TBoolean with TwoChildren)
  //     def traverse(fun: Term => Term): TBoolean with TwoChildren =
  //       t.copy(left = t.left, right = t.right)

  case object TTrue extends TBoolean
  case object TFalse extends TBoolean
  case class TLt(left: Term, right: Term) extends TBoolean
  case class TGt(left: Term, right: Term) extends TBoolean
  case class TLeq(left: Term, right: Term) extends TBoolean
  case class TGeq(left: Term, right: Term) extends TBoolean
  case class TEq(left: Term, right: Term) extends TBoolean // equality
  case class TIneq(left: Term, right: Term) extends TBoolean // inequality
  case class TDisj(left: Term, right: Term) extends TBoolean // disjunction
  case class TConj(left: Term, right: Term) extends TBoolean // conjunction
  case class TImpl(left: Term, right: Term) extends TBoolean // implication
  case class TInSet(left: Term, right: Term) extends TBoolean // in set

  sealed trait TQuantifier extends TBoolean
  case class TForall(
      vars: NonEmptyList[TArgT],
      triggers: Seq[TViper],
      body: Term
  ) extends TQuantifier
  case class TExists(vars: NonEmptyList[TArgT], body: Term) extends TQuantifier

  // parantheses
  case class TParens(inner: Term) extends Term with TViper

  // strings
  case class TString(value: String) extends Term

  // Scala stuff
  // field access
  sealed trait TFAcc extends Term:
    val parent: Term
    val field: ID
  case class TFCall(parent: Term, field: ID, args: List[Term])
      extends TFAcc
      with TViper
  case class TFCurly(parent: Term, field: ID, body: Term) extends TFAcc
  // function call
  case class TFunC(name: ID, args: Seq[Term]) extends Term with TViper

  // interaction enrichments are special curly accesses
  // sealed trait TInEn extends TFAcc
  // case class TReq(parent: Term, inner: Term) extends TInEn:
  //   final val field = "requires"
  // case class TEns(parent: Term, inner: Term) extends TInEn:
  //   final val field = "ensures"
  // case class TMod(parent: Term, inner: Term) extends TInEn:
  //   final val field = "modifies"
  // case class TExec(parent: Term, inner: Term) extends TInEn:
  //   final val field = "executes"
