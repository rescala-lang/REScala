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
  val booleanExpr: P[TBoolean] = quantifier | implication
  val implication: P[TBoolean] =
    (equality ~ (P.string("==>").surroundedBy(ws) *> equality).?).map {
      case (left, None)        => left
      case (left, Some(right)) => TImpl(left = left, right = right)
    }
  val equality: P[TBoolean] =
    (inequality ~ (P.string("==").surroundedBy(ws) *> inequality).?).map {
      case (left, None)        => left
      case (left, Some(right)) => TEq(left = left, right = right)
    }
  val inequality: P[TBoolean] =
    (conjunction ~ (P.string("=/=") *> conjunction).?).map {
      case (left, None)        => left
      case (left, Some(right)) => TIneq(left = left, right = right)
    }
  val conjunction: P[TBoolean] = ???
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

  // programs are sequences of terms
  val term: P[Term] = _var
  val prog: P[NonEmptyList[Term]] = term.repSep(ws).between(ws, P.end)
