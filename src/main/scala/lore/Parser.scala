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
      case (l: ID, r: Type) => TArgT(l, r)
    }

  // basic terms
  val _var: P[TVar] = id.map(TVar(_)) // variables

  // bolean expressions
  val quantifierVars: P[NonEmptyList[TArgT]] =
    (argT <* ws).repSep(P.char(',') <* ws)
  val triggers: P0[List[TViper]] = P.unit.as(List[TViper]())
  val forall: P[TForall] =
    (((P.string("forall") ~ ws *> quantifierVars) <* P.string(
      "::"
    ) ~ ws) ~ triggers ~ booleanExpr).map { case ((vars, triggers), body) =>
      TForall(vars = vars, triggers = triggers, body = body)
    }
  val exists: P[TExists] = ???
  val quantifier: P[TQuantifier] = forall | exists
  val implication: P[TImpl] = ???
  val booleanExpr: P[TBoolean] = quantifier | implication

  // programs are sequences of terms
  val term: P[Term] = _var
  val prog: P[NonEmptyList[Term]] = term.repSep(ws).between(ws, P.end)
