package lore

import AST._
import cats.parse.{Parser => P, Parser0 => P0, Rfc5234}
import cats.parse.Rfc5234.{alpha, digit, sp}
import cats.implicits._
import cats._
import cats.data.NonEmptyList

object Parser:
  // helpers
  val ws: P0[Unit] = sp.rep0.void // whitespace
  val id: P[ID] = alpha.rep.string
  val number: P[Number] = digit.rep.string.map(Integer.parseInt(_))
  val argT: P[TArgT] =
    (((id <* sp.? ~ P.char(':')) <* sp.rep0) ~ id).map { // args with type
      (l: ID, r: Type) => TArgT(l, r)
    }

  // basic terms
  val _var: P[TVar] = id.map(TVar(_)) // variables

  // boolean expressions
  val booleanExpr: P[Term] = P.defer(quantifier | implication)
  val implication: P[Term] =
    P.defer(equality ~ (P.string("==>").surroundedBy(ws) *> equality).?).map {
      case (left, None)        => left
      case (left, Some(right)) => TImpl(left = left, right = right)
    }
  val equality: P[Term] =
    P.defer(inequality ~ (P.string("==").surroundedBy(ws) *> inequality).?).map {
      case (left, None)        => left
      case (left, Some(right)) => TEq(left = left, right = right)
    }
  val inequality: P[Term] =
    P.defer(conjunction ~ (P.string("!=").surroundedBy(ws) *> conjunction).?).map {
      case (left, None)        => left
      case (left, Some(right)) => TIneq(left = left, right = right)
    }
  val conjunction: P[Term] =
    P.defer(disjunction ~ (P
      .string("&&")
      .surroundedBy(ws)
      .as("&&").soft ~ disjunction).rep0).map(evalBoolSeq)
  val disjunction: P[Term] =
    P.defer(boolFactor ~
    (((ws.soft *> P.string("||") <* ws)).as("||").with1 ~ boolFactor).rep0
    ).map(evalBoolSeq)
  def evalBoolSeq(seq: (Term, Seq[(String, Term)])): Term =
    seq match
      case (root, Nil) => root
      case (root, ("||", x) :: xs) =>
        TDisj(left = root, right = evalBoolSeq(x, xs))
      case (root, ("&&", x) :: xs) =>
        TConj(left = root, right = evalBoolSeq(x, xs))
      case sth =>
        throw new IllegalArgumentException(s"Not a boolean expression: $sth")

  val tru: P[TBoolean] = P.string("true").surroundedBy(ws).as(TTrue)
  val fls: P[TBoolean] = P.string("false").surroundedBy(ws).as(TFalse)
  val parens: P[Term] = // parantheses
    (ws.soft ~ P.char('(') ~ ws).with1 *> P.defer(implication) <* P.char(')').surroundedBy(ws)
  val boolFactor: P[Term] =
    P.defer(tru | fls | parens | inSet.backtrack | numComp.backtrack | methodCall.backtrack | call.backtrack | _var)

  val inSet: P[TBoolean] = // set expressions
    P.defer((term.surroundedBy(ws) <* P.string("in")) ~ term.surroundedBy(ws)).map {
      (left, right) => TInSet(left, right)
    }

  val numComp: P[Term] = P.string("TODO").as(TTrue) // TODO: number comparison

  // quantifiers
  val quantifierVars: P[NonEmptyList[TArgT]] =
    (argT <* ws).repSep(P.char(',') <* ws)
  val triggers: P0[List[TViper]] = P.unit.as(List[TViper]())
  val forall: P[TForall] =
    (((P.string("forall") ~ ws *> quantifierVars) <* P.string(
      "::"
    ) ~ ws) ~ triggers ~ booleanExpr).map { case ((vars, triggers), body) =>
      TForall(vars = vars, triggers = triggers, body = body)
    }
  val exists: P[TExists] =
    ((P.string("exists") ~ ws *> quantifierVars <* P
      .string("::")
      .surroundedBy(ws)) ~ booleanExpr).map { case (vars, body) =>
      TExists(vars = vars, body = body)
    }
  val quantifier: P[TQuantifier] = forall | exists

  // object orientation
  val methodCall = P.string("TODO").as(TTrue)
  val call = P.string("TODO").as(TTrue)

  // programs are sequences of terms
  val term: P[Term] = _var
  val prog: P[NonEmptyList[Term]] = term.repSep(ws).between(ws, P.end)
