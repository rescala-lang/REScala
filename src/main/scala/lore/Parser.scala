package lore

import AST._
import cats.parse.{Parser => P, Parser0 => P0, Rfc5234}
import cats.parse.Rfc5234.{alpha, char, digit, lf, wsp}
import cats.implicits._
import cats.data.NonEmptyList
import scala.annotation.tailrec
import java.nio.file.Path

object Parser:
  final case class ParsingException(message: String) extends Exception(message)

  // helpers
  private val infixOperators = Set("-", "+", "*", "/", ".")
  val ws: P0[Unit] = wsp.rep0.void // whitespace
  // val wsOrNl = (wsp | lf).rep0 // any amount of whitespace or newlines
  val wsOrNl = (wsp | P.defer(comment) | lf).rep0
  // val wsOrNl =
  //   ((wsp.rep).? ~ comment.? ~ lf.?).rep0 // any amount of whitespace or newlines
  val id: P[ID] = (alpha ~ (alpha | digit | P.char('_')).rep0).string
  // val underscore: P[ID] = P.char('_').as("_")
  val number: P[TNum] = digit.rep.string.map(i => TNum(Integer.parseInt(i)))
  val argT: P[TArgT] = // args with type
    ((id <* P.char(':').surroundedBy(ws)) ~ P.defer(typeName)).map((id, typ) =>
      TArgT(id, typ)
    )
  val typeName: P[Type] = P.recursive { rec =>
    (
      tupleType(rec).map(TupleType(_)) | // tuple
        (id ~ (P.char('[') *> innerType(rec) <* P.char(
          ']'
        )).?) // type with optional params
    )
      .map {
        case (outer: ID, Some(inner): Some[NonEmptyList[Type]]) =>
          SimpleType(outer, inner.toList)
        case (outer: ID, none) => SimpleType(outer, List.empty)
        case t @ TupleType(_)  => t
      }
  }

  lazy val innerType = (t: P[Type]) =>
    (ws.with1 *> t.repSep(P.char(',') ~ ws) <* ws)
  lazy val tupleType =
    (t: P[Type]) => P.char('(') *> innerType(t) <* P.char(')')

  // helper definition for parsing sequences of expressions
  val parseSeq = (factor: P[Term], separator: P[String]) =>
    (factor ~
      (((wsOrNl.with1.soft *> separator <* wsOrNl))
        ~ factor).rep0)

  //// basic terms
  val _var: P[TVar] = (id).map(TVar(_)) // variables

  // tuples
  val tuple: P[TTuple] =
    (P.char('(')
      .soft ~ wsOrNl *> P.defer(term).repSep(2, ws ~ P.char(',') ~ wsOrNl) <* P
      .char(')'))
      .map(TTuple(_))

  //// arithmetic expressions
  val arithmExpr: P[Term] = P.defer(addSub)
  val addSub: P[Term] =
    P.defer(parseSeq(divMul, P.stringIn(List("+", "-")))).map(evalArithm)
  val divMul: P[Term] =
    P.defer(parseSeq(arithFactor, P.stringIn(List("/", "*")))).map(evalArithm)
  val parens: P[Term] =
    ((P.char('(').soft ~ ws).with1 *> arithmExpr <* ws ~ P.char(')'))
      .map(TParens(_))
  val arithFactor: P[Term] =
    P.defer(
      parens | fieldAcc | functionCall | number.backtrack | _var
    )
  def evalArithm(seq: (Term, List[(String, Term)])): Term = seq match
    case (x, Nil)            => x
    case (l, ("*", r) :: xs) => TMul(left = l, right = evalArithm(r, xs))
    case (l, ("/", r) :: xs) => TDiv(left = l, right = evalArithm(r, xs))
    case (l, ("+", r) :: xs) => TAdd(left = l, right = evalArithm(r, xs))
    case (l, ("-", r) :: xs) => TSub(left = l, right = evalArithm(r, xs))
    case sth =>
      throw new ParsingException(s"Not an arithmetic expression: $sth")

  // boolean expressions
  val booleanExpr: P[Term] = P.defer(quantifier | biImplication)

  // primitives
  val tru: P[TBoolean] = P.string("true").as(TTrue)
  val fls: P[TBoolean] = P.string("false").as(TFalse)
  val neg: P[Term] = (P.char('!') ~ ws) *> P.defer(biImplication).map(TNeg(_))
  val boolParens: P[Term] = // parantheses
    ((P.char('(').soft ~ wsOrNl).with1 *> P
      .defer(biImplication) <* wsOrNl ~ P.char(')')).map(TParens(_))
  val boolFactor: P[Term] =
    boolParens.backtrack
      | neg
      | P.defer(inSet)
      | P.defer(numComp)
      | tru.backtrack
      | fls.backtrack
      | P.defer(arithmExpr)
      | P.defer(fieldAcc)
      | P.defer(functionCall)
      | _var

  // helper for boolean expressions with two sides
  val boolTpl = (factor: P[Term], separator: P[Unit]) =>
    factor ~ ((wsOrNl.soft ~ separator.backtrack ~ wsOrNl) *> factor).?

  val biImplication: P[Term] =
    P.defer(boolTpl(implication, P.string("<==>"))).map {
      case (left, None)        => left
      case (left, Some(right)) => TBImpl(left = left, right = right)
    }

  val implication: P[Term] =
    P.defer(boolTpl(disjunction, P.string("==>"))).map {
      case (left, None)        => left
      case (left, Some(right)) => TImpl(left = left, right = right)
    }
  val disjunction: P[Term] =
    P.defer(boolSeq(conjunction, "||"))
  val conjunction: P[Term] =
    P.defer(boolSeq(equality, "&&"))
  val equality: P[Term] =
    P.defer(boolTpl(inequality, P.string("==") <* P.char('>').unary_!))
      .map {
        case (left, None)        => left
        case (left, Some(right)) => TEq(left = left, right = right)
      }
  val inequality: P[Term] =
    P.defer(boolTpl(boolFactor, P.string("!="))).map {
      case (left, None)        => left
      case (left, Some(right)) => TIneq(left = left, right = right)
    }
  // helper for boolean expressions with arbitrarily long sequences like && and ||
  val boolSeq = (factor: P[Term], separator: String) =>
    parseSeq(factor, P.string(separator).as(separator)).map(evalBoolSeq)
  def evalBoolSeq(seq: (Term, Seq[(String, Term)])): Term =
    seq match
      case (root, Nil) => root
      case (root, ("||", x) :: xs) =>
        TDisj(left = root, right = evalBoolSeq(x, xs))
      case (root, ("&&", x) :: xs) =>
        TConj(left = root, right = evalBoolSeq(x, xs))
      case sth =>
        throw new ParsingException(s"Not a boolean expression: $sth")

  // set expressions
  val inSetFactor: P[Term] =
    P.defer(fieldAcc | functionCall.backtrack | tuple | number.backtrack | _var)
  val inSet: P[TBoolean] = P
    .defer(
      ((inSetFactor <* ws).soft <* P
        .string("in") ~ ws) ~ inSetFactor
    )
    .map { (left, right) =>
      TInSet(left, right)
    }

  // number comparisons
  val numComp: P[TBoolean] = (
    arithmExpr.soft ~ (ws.soft.with1 *> (P
      .stringIn(List("<=", ">=", "<", ">")) <* P.not(
      P.char('=')
    )).backtrack <* ws) ~ arithmExpr
  )
    .map { case ((l, op), r) =>
      op match
        case "<=" => TLeq(left = l, right = r)
        case ">=" => TGeq(left = l, right = r)
        case "<"  => TLt(left = l, right = r)
        case ">"  => TGt(left = l, right = r)
    }

  // quantifiers
  val quantifierVars: P[NonEmptyList[TArgT]] =
    (argT).repSep(ws.soft ~ P.char(',') ~ wsOrNl)
  val triggers: P0[List[TViper]] = P.unit.as(List[TViper]())
  val forall: P[TForall] =
    (((P.string("forall") ~ ws *> quantifierVars) <* wsOrNl ~ P.string(
      "::"
    ) ~ wsOrNl) ~ triggers ~ booleanExpr).map { case ((vars, triggers), body) =>
      TForall(vars = vars, triggers = triggers, body = body)
    }
  val exists: P[TExists] =
    (((P.string("exists") ~ ws *> quantifierVars) <* wsOrNl ~ P.string(
      "::"
    ) ~ wsOrNl) ~ triggers ~ booleanExpr).map { case ((vars, triggers), body) =>
      TExists(vars = vars, body = body)
    }
  val quantifier: P[TQuantifier] = forall | exists

  // reactives
  val reactive: P[TReactive] = P.defer(source | derived)
  val source: P[TSource] =
    (P.string("Source") ~ ws ~ P.char('(') ~ wsOrNl *> P.defer(
      term
    ) <* wsOrNl ~ P.char(')')).map((body) => TSource(body))
  val derived: P[TDerived] =
    (P.string("Derived") ~ ws ~ P.char('{') ~ wsOrNl *> P.defer(
      term
    ) <* wsOrNl ~ P.char('}'))
      .map((body) => TDerived(body))

  // interactions
  val typeParam: P[List[Type]] =
    P.char('[') ~ ws ~ P.char('(').? *> P.defer0(
      typeName.repSep0(P.char(',').surroundedBy(ws))
    ) <* P.char(')').? ~ P
      .char(']')

  val interaction: P[TInteraction] =
    (P.string("Interaction") ~ ws *> typeParam ~ (ws *> typeParam))
      .map((r, a) => TInteraction(reactiveTypes = r, argumentTypes = a))

  // invariants
  val invariant: P[TInvariant] =
    (P.string("invariant") ~ wsOrNl *> booleanExpr).map {
      case b: TBoolean => TInvariant(b)
      case x =>
        throw ParsingException(
          s"Expected a boolean expression as invariant body but got: $x"
        )
    }

  // bindings
  val bindable =
    P.defer(
      fieldAcc | functionCall | typeAlias | reactive | booleanExpr | number.backtrack | _var
    )
  val bindingLeftSide: P[TArgT] =
    (P.string("val") ~ ws *> P.defer(argT))
  val binding: P[TAbs] =
    P.defer((bindingLeftSide <* P.char('=').surroundedBy(ws)) ~ term)
      .map { case (TArgT(name, _type), term) =>
        TAbs(name = name, _type = _type, body = term)
      }

  // object orientation (e.g. dot syntax)
  val args = P.defer0(term.repSep0(P.char(',') ~ ws))
  val objFactor = P.defer(interaction | functionCall | _var)
  val fieldAcc: P[TFAcc] =
    P.defer(
      objFactor.soft ~ (round | curly | field).rep
    ).map((obj, calls) => evalFieldAcc(obj, calls.toList))

  // field accesses
  enum callType:
    case round, curly, field
  inline def callBuilder[A](open: P[Char], close: P[Unit], inner: P0[A]) =
    ((wsOrNl.with1.soft ~ P.char('.') *> id <* wsOrNl).soft <* open) ~
      (wsOrNl *> inner <* wsOrNl) <* close
  val round =
    callBuilder(P.char('(').as('('), P.char(')'), args).map((f, a) =>
      (callType.round, f, a)
    )
  val curly =
    callBuilder(P.char('{').as('{'), P.char('}'), P.defer(term)).map((f, b) =>
      (callType.curly, f, List(b))
    )
  val field =
    ((wsOrNl.with1.soft ~ P.char('.')) *> id)
      .map((callType.field, _, List[Term]()))
  @tailrec
  def evalFieldAcc(s: (Term, List[(callType, ID, List[Term])])): TFAcc =
    s match
      case (parent: TFAcc, Nil) => parent
      case (parent, (callType.curly, field, b :: Nil) :: rest) =>
        evalFieldAcc(
          TFCurly(parent = parent, field = field, body = b),
          rest
        )
      case (parent, (_, field, args) :: rest) =>
        evalFieldAcc(
          TFCall(parent = parent, field = field, args = args),
          rest
        )
      case _ =>
        throw new ParsingException(s"Not a valid field access: $s")

  // functions
  val functionCall: P[TFunC] = (id.soft ~ (P.char('(') *> args) <* P.char(')'))
    .map { (id, arg) =>
      TFunC(name = id, args = arg)
    }

  private val lambdaVars: P[NonEmptyList[TVar]] =
    (P.char('(') ~ ws *> _var.repSep(ws ~ P.char(',') ~ ws) <* P.char(
      ')'
    )) |
      _var.map(NonEmptyList.one(_))
  val lambdaFun: P[TArrow] =
    ((((lambdaVars.backtrack <* wsOrNl).soft <* P.string("=>")) <* wsOrNl).rep ~
      P.defer(term))
      .map((args, r) => rewriteLambda(args.flatten.toList.reverse, r))
  def rewriteLambda(params: List[TVar], right: Term): TArrow =
    (params, right) match
      case (Nil, r: TArrow) => r
      case (x :: xs, r)     => rewriteLambda(xs, TArrow(x, r))
      case (Nil, r) => throw ParsingException(s"Not a valid lambda term: $r")

  // type aliases
  val typeAlias: P[TTypeAl] =
    (P.string("type") ~ ws *> id ~ (P.char('=').surroundedBy(ws) *> typeName))
      .map((n, t) => TTypeAl(name = n, _type = t))

  // comments
  val comment: P[Unit] =
    (P.string("//").soft ~ P.not(P.char('>')) ~ P.anyChar.repUntil(lf)).void

  // imports
  val filePath: P[Path] =
    (P.char('/').?.with1 ~ id ~ (P.char('/') ~ id).rep0 ~ P.char(
      '.'
    ) ~ id).string.map(Path.of(_))

  val viperImport: P[TViperImport] =
    ((P.string("//>") ~ ws ~ P.string("viper").? ~ P.string(
      "import"
    )) ~ ws *> filePath).map(TViperImport(_))

  // if then else expressions
  val ifThenElse: P[TIf] =
    ((P.string("if") ~ wsOrNl *> P.defer(term) <* wsOrNl) ~
      (P.string("then") ~ wsOrNl *> P.defer(term)) ~
      (wsOrNl.soft ~ P.string("else") *> P.defer(term)).?)
      .map { case ((cond, _then), _else) =>
        TIf(cond, _then, _else)
      }

  // a block is a sequence of terms surrounded by curly braces
  val block: P[TSeq] =
    (P.char('{') ~ wsOrNl *> P.defer(term.repSep(wsOrNl)) <* wsOrNl ~ P.char(
      '}'
    )).map(TSeq(_))

  // programs are sequences of terms
  val term: P[Term] =
    P.defer(
      viperImport | typeAlias | binding | reactive | invariant | ifThenElse | lambdaFun | booleanExpr.backtrack | fieldAcc | block | tuple | number.backtrack | _var
    )
  val prog: P[NonEmptyList[Term]] =
    term.repSep(wsOrNl).surroundedBy(wsOrNl) <* P.end

  def parse(p: String) = prog.parseAll(p)
