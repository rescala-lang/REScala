package lore

import ast._
import cats.parse.{Parser => P, Parser0 => P0, Rfc5234}
import cats.parse.Rfc5234.{alpha, crlf, digit, lf, wsp}
import cats.data.NonEmptyList
import scala.annotation.tailrec
import java.nio.file.Path
import cats.syntax.all._

object Parser {
  final case class ParsingException(message: String) extends Exception(message)

  // helper functions
  private def withSourcePos[A](parser: P[A]): P[(A, SourcePos)] =
    (P.caret.with1 ~ parser ~ P.caret).map { case ((c1, a), c2) =>
      (a, SourcePos(c1, c2))
    }

  /** Helper for calculating the source position of composed terms where the
    * first and last term are already known.
    */
  private def calcSourcePos(l: Term, r: Term): Option[SourcePos] =
    for {
      sl <- l.sourcePos
      sr <- r.sourcePos
    }
    yield sl.copy(end = sr.end)
  // helper definition for parsing sequences of expressions with operators as strings
  private def parseSeq(factor: P[Term], separator: P[String]) =
    (factor ~
      (((wsOrNl.with1.soft *> separator <* wsOrNl))
        ~ factor).rep0)
  // helper for boolean expressions with two sides
  private def boolTpl(factor: P[Term], separator: P[Unit]) =
    factor ~ ((wsOrNl.soft ~ separator.backtrack ~ wsOrNl) *> factor).?

  // helper parsers
  // private val infixOperators = Set("-", "+", "*", "/", ".")
  private val keywords =
    Set("Interaction", "assert", "assume", "true", "false", "forall", "exists")
  val ws: P0[Unit] = wsp.rep0.void // whitespace
  // any amount of whitespace, newlines or comments
  val wsOrNl = (wsp | P.defer(comment) | lf | crlf).rep0
  val id: P[ID] = (alpha ~ (alpha | digit | P.char('_').as('_')).rep0).string
  // types
  val typeName: P[Type] = P.recursive { rec =>
    (
      tupleType(rec).map(TupleType(_)) | // tuple
        (id ~ (P.char('[') *> innerType(rec) <* P.char(
          ']'
        )).?) // type with optional params
    )
      .map {
        case (outer, Some(inner)) =>
          SimpleType(outer, inner.toList)
        case (outer: ID, None) => SimpleType(outer, List.empty)
        case t @ TupleType(_)  => t
      }
  }
  lazy val innerType = (t: P[Type]) =>
    (ws.with1 *> t.repSep(P.char(',') ~ ws) <* ws)
  lazy val tupleType =
    (t: P[Type]) => P.char('(') *> innerType(t) <* P.char(')')
  // val underscore: P[ID] = P.char('_').as("_")

  // begin ast Nodes
  val number: P[TNum] =
    withSourcePos(digit.rep.string)
      .map((i, s) => TNum(Integer.parseInt(i), sourcePos = Some(s)))
      .withContext("Expected a number.")
  val argT: P[TArgT] = // args with type
    withSourcePos((id <* P.char(':').surroundedBy(ws)) ~ P.defer(typeName))
      .map { case ((id, typ), s) => TArgT(id, typ) }
      .withContext("Expected name and type.")

  //// basic terms
  val _var: P[TVar] = // variables
    withSourcePos(P.not(P.stringIn(keywords)).with1 *> id)
      .map((v, s) => TVar(v, Some(s)))
      .withContext("Expected a variable.")

  // tuples
  val tuple: P[TTuple] =
    withSourcePos(
      P.char('(').soft ~ wsOrNl *> P
        .defer(term)
        .repSep(2, ws.soft ~ P.char(',') ~ wsOrNl) <* wsOrNl ~ P
        .char(')')
    )
      .map((t, s) => TTuple(t, Some(s)))
      .withContext("Expected a tuple.")

  //// arithmetic expressions
  val arithmExpr: P[Term] =
    P.defer(addSub).withContext("Expected an arithmetic expression.")
  val addSub: P[Term] =
    P.defer(parseSeq(divMul, P.stringIn(List("+", "-")))).map(evalArithm)
  val divMul: P[Term] =
    P.defer(parseSeq(arithFactor, P.stringIn(List("/", "*")))).map(evalArithm)
  val parens: P[Term] =
    withSourcePos(
      (P.char('(').soft ~ ws).with1 *> arithmExpr <* ws ~ P.char(')')
    )
      .map((t, s) => TParens(inner = t, sourcePos = Some(s)))
      .withContext("Expected a term in parantheses.")
  val arithFactor: P[Term] =
    P.defer(
      parens | fieldAcc | functionCall | number.backtrack | _var
    )
  def evalArithm(seq: (Term, List[(String, Term)])): Term = seq match {
    case (x, Nil) => x
    case (l, (op, x) :: xs) if List("-", "+", "*", "/").contains(op) =>
      val r = evalArithm(x, xs)
      val s = calcSourcePos(l, r)
      op match {
        case "*" => TMul(left = l, right = r, sourcePos = s)
        case "/" => TDiv(left = l, right = r, sourcePos = s)
        case "+" => TAdd(left = l, right = r, sourcePos = s)
        case "-" => TSub(left = l, right = r, sourcePos = s)
      }
    case sth =>
      throw new ParsingException(s"Not an arithmetic expression: $sth")
  }

  // boolean expressions
  val booleanExpr: P[Term] = P
    .defer(quantifier | biImplication)
    .withContext("Expected a boolean expression.")

  // primitives
  val tru: P[TBoolean] =
    withSourcePos(P.string("true")).map { (_, s) =>
      TTrue(sourcePos = Some(s))
    }
  val fls: P[TBoolean] =
    withSourcePos(P.string("false")).map { (_, s) =>
      TFalse(sourcePos = Some(s))
    }
  val neg: P[Term] =
    withSourcePos((P.char('!') ~ ws) *> P.defer(biImplication)).map((t, s) =>
      TNeg(t, Some(s))
    )
  val boolParens: P[Term] = // parantheses
    withSourcePos(
      (P.char('(').soft ~ wsOrNl).with1 *> P
        .defer(booleanExpr) <* wsOrNl ~ P.char(')')
    ).map((i, s) => TParens(inner = i, sourcePos = Some(s)))
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

  val biImplication: P[Term] =
    P.defer(boolTpl(implication, P.string("<==>"))).map {
      case (left, None) => left
      case (left, Some(right)) =>
        TBImpl(
          left = left,
          right = right,
          sourcePos = calcSourcePos(left, right)
        )
    }

  val implication: P[Term] =
    P.defer(boolTpl(disjunction, P.string("==>"))).map {
      case (left, None) => left
      case (left, Some(right)) =>
        TImpl(
          left = left,
          right = right,
          sourcePos = calcSourcePos(left, right)
        )
    }
  val disjunction: P[Term] =
    P.defer(boolSeq(conjunction, "||"))
  val conjunction: P[Term] =
    P.defer(boolSeq(equality, "&&"))
  val equality: P[Term] =
    P.defer(boolTpl(inequality, P.string("==") <* P.char('>').unary_!))
      .map {
        case (left, None) => left
        case (left, Some(right)) =>
          TEq(
            left = left,
            right = right,
            sourcePos = calcSourcePos(left, right)
          )
      }
  val inequality: P[Term] =
    P.defer(boolTpl(boolFactor, P.string("!="))).map {
      case (left, None) => left
      case (left, Some(right)) =>
        TIneq(
          left = left,
          right = right,
          sourcePos = calcSourcePos(left, right)
        )
    }
  // helper for boolean expressions with arbitrarily long sequences like && and ||
  val boolSeq = (factor: P[Term], separator: String) =>
    parseSeq(factor, P.string(separator).as(separator)).map(evalBoolSeq)
  def evalBoolSeq(seq: (Term, Seq[(String, Term)])): Term =
    seq match {
      case (root, Nil) => root
      case (root, (op, x) :: xs) if List("||", "&&").contains(op) =>
        val r = evalBoolSeq(x, xs)
        val s = calcSourcePos(root, r)
        op match {
          case "||" => TDisj(left = root, right = r, sourcePos = s)
          case "&&" => TConj(left = root, right = r, sourcePos = s)
        }
      case sth =>
        throw new ParsingException(s"Not a boolean expression: $sth")
    }

  // set expressions
  val inSetFactor: P[Term] =
    P.defer(
      boolParens.backtrack | fieldAcc | functionCall.backtrack | tuple | number.backtrack | _var
    )
  val inSet: P[TBoolean] = withSourcePos(
    P
      .defer(
        ((inSetFactor <* ws).soft <* P
          .string("in") ~ ws) ~ inSetFactor
      )
  )
    .map { case ((left, right), s) =>
      TInSet(left, right, Some(s))
    }

  // number comparisons
  val numComp: P[TBoolean] = (
    arithmExpr.soft ~ (ws.soft.with1 *> (P
      .stringIn(List("<=", ">=", "<", ">")) <* P.not(
      P.char('=')
    )).backtrack <* ws) ~ arithmExpr
  )
    .map { case ((l, op), r) =>
      val s = calcSourcePos(l, r)
      op match {
        case "<=" => TLeq(left = l, right = r, sourcePos = s)
        case ">=" => TGeq(left = l, right = r, sourcePos = s)
        case "<"  => TLt(left = l, right = r, sourcePos = s)
        case ">"  => TGt(left = l, right = r, sourcePos = s)
      }
    }

  // quantifiers
  private val quantifierVars: P[NonEmptyList[TArgT]] =
    (argT).repSep(ws.soft ~ P.char(',') ~ wsOrNl)
  val trigger: P[NonEmptyList[Term]] =
    P.char('{') ~ wsOrNl *> inSetFactor.repSep(
      wsOrNl ~ P.char(',') ~ wsOrNl
    ) <* wsOrNl ~ P.char('}')
  private val triggers: P0[List[NonEmptyList[Term]]] = trigger.repSep0(wsOrNl)
  val forall: P[TForall] =
    withSourcePos(
      ((P.string("forall") ~ ws *> quantifierVars) <* wsOrNl ~ P.string(
        "::"
      ) ~ wsOrNl) ~ (triggers <* wsOrNl) ~ booleanExpr
    ).map { case (((vars, triggers), body), s) =>
      TForall(
        vars = vars,
        triggers = triggers,
        body = body,
        sourcePos = Some(s)
      )
    }
  val exists: P[TExists] =
    withSourcePos(
      ((P.string("exists") ~ ws *> quantifierVars) <* wsOrNl ~ P.string(
        "::"
      ) ~ wsOrNl) ~ triggers ~ booleanExpr
    ).map { case (((vars, triggers), body), s) =>
      TExists(vars = vars, body = body, sourcePos = Some(s))
    }
  val quantifier: P[TQuantifier] = forall | exists

  // reactives
  val reactive: P[TReactive] = P.defer(source | derived)
  val source: P[TSource] =
    withSourcePos(
      P.string("Source") ~ ws ~ P.char('(') ~ wsOrNl *> P.defer(
        term
      ) <* wsOrNl ~ P.char(')')
    ).map((body, s) => TSource(body, Some(s)))
  val derived: P[TDerived] =
    withSourcePos(
      P.string("Derived") ~ ws ~ P.char('{') ~ wsOrNl *> P.defer(
        term
      ) <* wsOrNl ~ P.char('}')
    )
      .map((body, s) => TDerived(body, Some(s)))

  // interactions
  val typeParam: P[Type] =
    P.char('[') ~ ws *> P.defer0(typeName.surroundedBy(ws))
      <* P
        .char(']')

  val interaction: P[TInteraction] =
    withSourcePos(P.string("Interaction") ~ ws *> typeParam ~ (ws *> typeParam))
      .map { case ((r, a), s) =>
        TInteraction(reactiveType = r, argumentType = a, sourcePos = Some(s))
      }

  // invariants
  val invariant: P[TInvariant] =
    withSourcePos(P.string("invariant") ~ wsOrNl *> booleanExpr).map {
      case (b: TBoolean, s) => TInvariant(b, sourcePos = Some(s))
      case (x, s) =>
        throw ParsingException(
          s"Expected a boolean expression as invariant body but got: $x at $s"
        )
    }

  // bindings
  private[lore] val bindingLeftSide: P[TArgT] =
    (P.string("val") ~ ws *> P.defer(argT))
  val binding: P[TAbs] =
    withSourcePos(
      P.defer((bindingLeftSide <* ws ~ P.char('=') ~ wsOrNl) ~ term)
    )
      .map { case ((TArgT(name, _type, _), term), s) =>
        TAbs(name = name, _type = _type, body = term, sourcePos = Some(s))
      }

  // object orientation (e.g. dot syntax)
  private val args = P.defer0(term.repSep0(P.char(',') ~ wsOrNl))
  private val objFactor = P.defer(interaction | functionCall | _var)
  val fieldAcc: P[TFAcc] =
    P.defer(
      objFactor.soft ~ withSourcePos(round | curly | field).rep
    ).map { case (obj, calls) => evalFieldAcc(obj, calls.toList) }

  // field accesses
  private enum callType {
    case round, curly, field
  }
  private inline def callBuilder[A](
      open: P[Char],
      close: P[Unit],
      inner: P0[A]
  ) =
    ((wsOrNl.with1.soft ~ P.char('.') *> id <* wsOrNl).soft <* open) ~
      (wsOrNl *> inner <* wsOrNl) <* close
  private val round =
    callBuilder(P.char('(').as('('), P.char(')'), args).map((f, a) =>
      (callType.round, f, a)
    )
  private val curly =
    callBuilder(P.char('{').as('{'), P.char('}'), P.defer(term)).map((f, b) =>
      (callType.curly, f, List(b))
    )
  private val field =
    ((wsOrNl.with1.soft ~ P.char('.')) *> id)
      .map((callType.field, _, List[Term]()))
  @tailrec
  private def evalFieldAcc(
      obj: Term,
      calls: List[((callType, ID, List[Term]), SourcePos)]
  ): TFAcc =
    (obj, calls) match {
      case (o: TFAcc, Nil) => o
      case (o, ((callType.curly, field, b :: Nil), s) :: rest) =>
        val pos = Some(SourcePos(start = o.sourcePos.get.start, end = s.end))
        evalFieldAcc(
          TFCurly(parent = o, field = field, body = b, sourcePos = pos),
          rest
        )
      case (o, ((_, field, args), s) :: rest) =>
        val pos = Some(SourcePos(start = o.sourcePos.get.start, end = s.end))
        evalFieldAcc(
          TFCall(parent = o, field = field, args = args, sourcePos = pos),
          rest
        )
      case x =>
        throw new ParsingException(s"Not a valid field access: $x")
    }

  // functions
  val functionCall: P[TFunC] =
    withSourcePos(
      P.not(P.stringIn(keywords)).with1
        *> (id.soft ~ (P.char('(') ~ wsOrNl *> args) <* wsOrNl ~ P.char(')'))
    )
      .map { case ((id, arg), s) =>
        TFunC(name = id, args = arg, sourcePos = Some(s))
      }

  val assertAssume: P[Term] =
    withSourcePos(
      ((P.string("assert").string | P.string("assume").string)
        <* P.char('(') ~ wsOrNl) ~ P.defer(booleanExpr) <* (wsOrNl ~ P.char(
        ')'
      ))
    )
      .map {
        case (("assert", t), s) => TAssert(t, sourcePos = Some(s))
        case (("assume", t), s) => TAssume(t, sourcePos = Some(s))
        case sth =>
          throw ParsingException(s"not a valid assertion/assumption: $sth")
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
    (params, right) match {
      case (Nil, r: TArrow) => r
      case (x :: xs, r) =>
        val s = calcSourcePos(params.head, right)
        rewriteLambda(xs, TArrow(x, r, sourcePos = s))
      case (Nil, r) => throw ParsingException(s"Not a valid lambda term: $r")
    }

  // type aliases
  val typeAlias: P[TTypeAl] =
    withSourcePos(
      P.string("type") ~ ws *> id ~ (P.char('=').surroundedBy(ws) *> typeName)
    )
      .map { case ((n, t), s) =>
        TTypeAl(name = n, _type = t, sourcePos = Some(s))
      }

  // comments
  val comment: P[Unit] =
    (P.string("//").soft ~ P.not(P.char('>')) ~ P.anyChar.repUntil(lf)).void

  // imports
  private val filePath: P[Path] =
    ((P.string("..") ~ P.char('/')).rep0.with1 ~ id ~ (P.charIn(
      List('/', '-')
    ) ~ id).rep0 ~ P.char(
      '.'
    ) ~ id).string.map(Path.of(_))

  val viperImport: P[TViperImport] =
    withSourcePos(
      (P.string("//>") ~ ws ~ P.string("viper").? ~ P.string(
        "import"
      )) ~ ws *> filePath
    ).map((i, s) => TViperImport(i, Some(s)))

  // if then else expressions
  val ifThenElse: P[TIf] =
    withSourcePos(
      (P.string("if") ~ wsOrNl *> P.defer(term) <* wsOrNl) ~
        (P.string("then") ~ wsOrNl *> P.defer(term)) ~
        (wsOrNl.soft ~ P.string("else") *> P.defer(term)).?
    )
      .map { case (((cond, _then), _else), s) =>
        TIf(cond, _then, _else, Some(s))
      }

  // a block is a sequence of terms surrounded by curly braces
  val block: P[TSeq] =
    withSourcePos(
      P.char('{') ~ wsOrNl *> P.defer(term.repSep(wsOrNl)) <* wsOrNl ~ P.char(
        '}'
      )
    ).map((seq, pos) => TSeq(seq, Some(pos)))

  // programs are sequences of terms
  val term: P[Term] =
    P.defer(
      viperImport | typeAlias | binding | reactive | invariant | ifThenElse | lambdaFun | assertAssume | booleanExpr.backtrack | fieldAcc | interaction | block | tuple | number.backtrack | _var
    )
  val prog: P[NonEmptyList[Term]] =
    term.repSep(wsOrNl).surroundedBy(wsOrNl) <* P.end

  def parse(p: String) = prog.parseAll(p)
}
