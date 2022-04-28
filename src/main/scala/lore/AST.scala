package lore

import cats.data.NonEmptyList

/**
 * The abstract syntax of the LoRe language.
 */
object AST:
    sealed trait Term

    // helper types
    type ID = String
    type Type = String
    type Number = Int
    case class TArgT(name: ID, _type: Type) extends Term // arguments with type annotation

    // basic terms
    case class TVar(name: ID) extends Term // variable
    case class TAbs(name: ID, _type: Type, body: Term) extends Term // abstractions
    case class TApp(left: Term, right: Term) extends Term // application
    case class TUnit() extends Term // unit

    // derived forms
    case class TSeq(left: Term, right: Term) extends Term // sequence
    case class TArrow(left: Term, right: Term) extends Term // anonymous functions
    
    // reactives
    sealed trait TReactive extends Term
    case class TSource(body: Term) extends TReactive
    case class TDerived(body: Term) extends TReactive

    // Viper terms
    case class TViper() extends Term

    // interactions
    case class TPre(body: TViper) extends Term
    case class TPost(body: TViper) extends Term
    case class TInt(reType: Seq[Type], argType: Seq[Type],
        mo: Set[ID], pr: Seq[TPre], po: Seq[TPost], ex: Term) extends Term

    // arithmetic expressions
    // TODO
    sealed trait TArith extends Term
    case class TNum(value: Number) extends TArith // numbers
    case class TDiv(left: Term, right: Term) extends TArith // division
    case class TMul(left: Term, right: Term) extends TArith // multiplication
    case class TAdd(left: Term, right: Term) extends TArith // addition
    case class TSub(left: Term, right: Term) extends TArith // substraction

    // boolean expressions
    sealed trait TBoolean extends Term
    case class TTrue() extends TBoolean
    case class TFalse() extends TBoolean
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
    case class TForall(vars: NonEmptyList[TArgT], triggers: Seq[TViper], body: Term) extends TQuantifier
    case class TExists(vars: NonEmptyList[TArgT], body: Term) extends TQuantifier

    // parantheses
    case class TParens(inner: Term) extends Term
    
    // strings
    case class TString(value: String) extends Term

    // Scala stuff
    // field access
    case class TFAcc(parent: Term, field: ID, args: Seq[Term])
    // function call
    case class TFunC(name: ID, args: Seq[Term])