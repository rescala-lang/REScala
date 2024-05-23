// Source: Copied over from LoRe for testing purposes only
// Modified to remove "derives Codec.AsObject" aspects as they seemed non-crucial but caused errors when just copy+pasted
// See https://github.com/stg-tud/LoRe/blob/main/src/main/scala/lore/AST.scala

package loreDSL

import io.circe._
import cats.data.NonEmptyList
import cats.parse.Caret
import io.circe.{Decoder, Encoder}
import io.circe.generic.auto._
import java.nio.file.Path
import scala.util.Try

sealed trait SourceType
case object Unknown extends SourceType
case object Inline extends SourceType
case class FromFile(path: Path) extends SourceType

case class SourcePos(start: Caret, end: Caret, _type: SourceType = Unknown)

/** The abstract syntax of the LoRe language.
 */
sealed trait Term:
  def sourcePos: Option[SourcePos]

// helper trait for expressions with two sides
sealed trait BinaryOp:
  def left: Term
  def right: Term

// imports
case class TViperImport(path: Path, sourcePos: Option[SourcePos] = None)
  extends TViper
implicit val pathEncoder: Encoder[Path] =
  Encoder.encodeString.contramap[Path](_.toString)
implicit val pathDecoder: Decoder[Path] =
  Decoder.decodeString.emapTry(str => Try(Path.of(str)))

// helper types
type ID = String
sealed trait Type derives Codec.AsObject
case class SimpleType(name: String, inner: List[Type]) extends Type
case class TupleType(inner: NonEmptyList[Type]) extends Type
type Number = Int

case class TArgT( // argument with type annotation
                  name: ID,
                  _type: Type,
                  sourcePos: Option[SourcePos] = None
                ) extends Term

// basic terms
case class TVar( // variable
                 name: ID,
                 sourcePos: Option[SourcePos] = None
               ) extends Term
  with TViper
case class TAbs( // abstractions
                 name: ID,
                 _type: Type,
                 body: Term,
                 sourcePos: Option[SourcePos] = None
               ) extends Term
  with TViper
case class TTuple( // tuples
                   factors: NonEmptyList[Term],
                   sourcePos: Option[SourcePos] = None
                 ) extends Term

case class TIf( // if clauses
                cond: Term,
                _then: Term,
                _else: Option[Term],
                sourcePos: Option[SourcePos] = None
              ) extends Term
// case class TApp(left: Term, right: Term) extends Term // application
// case class TUnit() extends Term // unit

// derived forms
case class TSeq( // sequences
                 body: NonEmptyList[Term],
                 sourcePos: Option[SourcePos] = None
               ) extends Term
  with TViper
case class TArrow( // anonymous functions
                   left: Term,
                   right: Term,
                   sourcePos: Option[SourcePos] = None
                 ) extends Term
  with BinaryOp:
  private def findBody: Term => Term =
    case t: TArrow => findBody(t.right)
    case t         => t
  def body: Term = findBody(right)
  private def collectArgNames: (acc: List[ID], term: Term) => List[ID] =
    case (acc, TArrow(TVar(name, _), t: TArrow, _)) =>
      collectArgNames(acc :+ name, t)
    case (acc, TArrow(TVar(name, _), _, _)) =>
      acc :+ name
    case (acc, t) => acc
  def args: List[ID] = collectArgNames(List(), this)

case class TTypeAl(
                    name: ID,
                    _type: Type,
                    sourcePos: Option[SourcePos] = None
                  ) extends Term // type aliases

// Viper terms
sealed trait TViper extends Term

case class TAssert(
                    body: Term,
                    sourcePos: Option[SourcePos] = None
                  ) extends Term
  with TViper
case class TAssume(body: Term, sourcePos: Option[SourcePos] = None)
  extends Term
    with TViper

// reactives
sealed trait TReactive extends Term:
  def body: Term
case class TSource(body: Term, sourcePos: Option[SourcePos] = None)
  extends TReactive
case class TDerived(body: Term, sourcePos: Option[SourcePos] = None)
  extends TReactive

// interactions
case class TInteraction(
                         reactiveType: Type,
                         argumentType: Type,
                         modifies: List[ID] = List(),
                         requires: List[Term] = List(),
                         ensures: List[Term] = List(),
                         executes: Option[Term] = None,
                         sourcePos: Option[SourcePos] = None
                       ) extends Term

// invariants
case class TInvariant(
                       condition: TBoolean,
                       sourcePos: Option[SourcePos] = None
                     ) extends Term

// arithmetic expressions
sealed trait TArith extends Term with TViper
// numbers
case class TNum(value: Number, sourcePos: Option[SourcePos] = None)
  extends TArith
// division
case class TDiv(left: Term, right: Term, sourcePos: Option[SourcePos] = None)
  extends TArith
    with BinaryOp
// multiplication
case class TMul(left: Term, right: Term, sourcePos: Option[SourcePos] = None)
  extends TArith
    with BinaryOp
// addition
case class TAdd(left: Term, right: Term, sourcePos: Option[SourcePos] = None)
  extends TArith
    with BinaryOp
// subtraction
case class TSub(left: Term, right: Term, sourcePos: Option[SourcePos] = None)
  extends TArith // substraction
    with BinaryOp

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

case class TTrue(sourcePos: Option[SourcePos] = None) extends TBoolean
case class TFalse(sourcePos: Option[SourcePos] = None) extends TBoolean
case class TNeg(body: Term, sourcePos: Option[SourcePos] = None)
  extends TBoolean
case class TLt(left: Term, right: Term, sourcePos: Option[SourcePos] = None)
  extends TBoolean
    with BinaryOp
case class TGt(left: Term, right: Term, sourcePos: Option[SourcePos] = None)
  extends TBoolean
    with BinaryOp
case class TLeq(left: Term, right: Term, sourcePos: Option[SourcePos] = None)
  extends TBoolean
    with BinaryOp
case class TGeq(left: Term, right: Term, sourcePos: Option[SourcePos] = None)
  extends TBoolean
    with BinaryOp
// equality
case class TEq(left: Term, right: Term, sourcePos: Option[SourcePos] = None)
  extends TBoolean
    with BinaryOp
// inequality
case class TIneq(left: Term, right: Term, sourcePos: Option[SourcePos] = None)
  extends TBoolean
    with BinaryOp
// disjunction
case class TDisj(left: Term, right: Term, sourcePos: Option[SourcePos] = None)
  extends TBoolean
    with BinaryOp
// conjunction
case class TConj(left: Term, right: Term, sourcePos: Option[SourcePos] = None)
  extends TBoolean
    with BinaryOp
// implication
case class TImpl(left: Term, right: Term, sourcePos: Option[SourcePos] = None)
  extends TBoolean
    with BinaryOp
// bi-implication

case class TBImpl(left: Term, right: Term, sourcePos: Option[SourcePos] = None)
  extends TBoolean
    with BinaryOp
// in set
case class TInSet(left: Term, right: Term, sourcePos: Option[SourcePos] = None)
  extends TBoolean
    with BinaryOp

sealed trait TQuantifier extends TBoolean:
  def vars: NonEmptyList[TArgT]
  def body: Term
case class TForall(
                    vars: NonEmptyList[TArgT],
                    triggers: List[NonEmptyList[Term]],
                    body: Term,
                    sourcePos: Option[SourcePos] = None
                  ) extends TQuantifier
case class TExists(
                    vars: NonEmptyList[TArgT],
                    body: Term,
                    sourcePos: Option[SourcePos] = None
                  ) extends TQuantifier

// parantheses
case class TParens(inner: Term, sourcePos: Option[SourcePos] = None)
  extends Term
    with TViper

// strings
case class TString(value: String, sourcePos: Option[SourcePos] = None)
  extends Term

// Scala stuff
// field access
sealed trait TFAcc extends Term:
  def parent: Term
  def field: ID
case class TFCall( // field call
                   parent: Term,
                   field: ID,
                   args: List[Term],
                   sourcePos: Option[SourcePos] = None
                 ) extends TFAcc
  with TViper
case class TFCurly( // field call with curly braces
                    parent: Term,
                    field: ID,
                    body: Term,
                    sourcePos: Option[SourcePos] = None
                  ) extends TFAcc

// function call
case class TFunC(name: ID, args: Seq[Term], sourcePos: Option[SourcePos] = None)
  extends Term
    with TViper
