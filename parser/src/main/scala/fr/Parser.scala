package fr

import AST._
import fastparse._, ScalaWhitespace._
import fastparse.Parsed.Failure

object Parser {
  def ScalaKeywords[_: P] = P("def" | "val")
  def ReactiveKeywords[_: P] = P("reactive" | "transaction" | "Source" | "Derived")
  def ViperKeywords[_: P] = P("@requires" | "@ensures" | "@transaction" | "@invariant")
  def Keywords[_: P] = P(ScalaKeywords | ReactiveKeywords | ViperKeywords)

  def reactiveExpr[_: P]: P[Reactive] = P(sourceReactiveValSyntax | sourceReactive | derivedReactiveValSyntax | derivedReactive)
  def sourceReactiveValSyntax[_: P]: P[SourceReactive] = P(
    Index ~ "val" ~ id ~ (":" ~ "Reactive[" ~ typeName ~ "]") ~ "=" ~ "Source" ~/ "(" ~/ expression ~ ")"
  ).map {
    case (index, name, typeAnn, body) =>
      SourceReactive(index = index, name = name, body = body, typeAnn = Some(typeAnn))
  }
  def sourceReactive[_: P]: P[SourceReactive] = P(
    Index ~ "reactive" ~ id ~ (":" ~ typeName).? ~ "=" ~ "Source" ~/ ("[" ~/ typeName ~ "]").? ~ "(" ~/ expression ~ ")"
  ).map {
    case (index, name, Some(typeAnn), None, body) =>
     SourceReactive(index = index, name = name, body = body, typeAnn = Some(typeAnn))
    case (index, name, None, typeAnn, body) =>
     SourceReactive(index = index, name = name, body = body, typeAnn = typeAnn)
    case (index, name, Some(t1), Some(t2), body) =>
      throw new Exception(s"Source expression at $index contains two type annotations but only one is allowed.")
  }
  def derivedReactiveValSyntax[_: P]: P[DerivedReactive] = P(
    Index ~ "val" ~ id ~ (":" ~ ("Reactive"|"Derived") ~ "[" ~/ typeName ~ "]") ~ "=" ~ "Derived" ~/ "{" ~/ expression ~ "}"
  ).map {
    case (index, name, typeAnn, body) =>
      DerivedReactive(index = index, name = name, body = body, typeAnn = Some(typeAnn))
  }
  def derivedReactive[_: P]: P[DerivedReactive] = P(
    Index ~ "reactive" ~ id ~ "=" ~ "Derived" ~/ ("[" ~/ typeName ~ "]").? ~ "{" ~/ expression ~ "}"
  ).map { case (index, name, typeAnn, body) =>
    DerivedReactive(index = index, name = name, body = body, typeAnn = typeAnn)
  }

  def precondition[_: P]: P[Precondition] =
    P(Index ~ "@requires" ~/ booleanExpression).map {
      case (i, s) =>
        Precondition(body = s, index = i)
    }
  def postcondition[_: P]: P[Postcondition] =
    P(Index ~ "@ensures" ~/ booleanExpression).map {
      case (i, s) =>
        Postcondition(body = s, index = i)
    }
  def invariant[_: P]: P[Invariant] = P(
    Index ~ "@invariant" ~/  booleanExpression).map {
      case (index, body: ViperExpression) => Invariant(
        index = index,
        body = body
      )
      case (index, expr) =>
        throw new IllegalArgumentException(s"Not a valid Viper expression: $expr")
    }
  
  def viperExpression[_: P]: P[ViperExpression] = P(
    booleanExpression | arithmeticExpression | call | 
    // oldExpr | fieldAcc | letExpr | conditionalExpr |
    id
  )

  // quantifiers
  def quantifier[_: P]: P[BooleanExpression] = P(
    forall | exists
  )

  def forall[_: P]: P[Forall] = P(
    Index ~ "forall" ~/ quantifierVars ~ "::" ~/ triggers.? ~ booleanExpression
  ).map{
    case (index, vars, triggers, body) =>
      Forall(index=index, vars=vars, triggers=triggers, body=body)
  }

  def exists[_: P]: P[Exists] = P(
    Index ~ "exists" ~/ quantifierVars ~ "::" ~/ booleanExpression
  ).map{
    case (index, vars, body) =>
      Exists(index=index,vars=vars,body=body)
  }

  def quantifierVars[_: P] = P(
    (id ~ typeAnnotation).rep(min=1, sep=",")
  )

  def triggers[_: P] = P(
    "{" ~/ viperExpression ~ "}"
  )

  // booleanExpressions
  def booleanExpression[_: P]: P[BooleanExpression] = P(
    NoCut(quantifier) | implication
  )
  def implication[_: P] = P(Index ~ equality ~
    ("==>" ~/ equality).?).map{
      case (index, factor, None) => factor
      case (index, left, Some(right)) => Implication(index=index, left=left,right=right)
  }
  def equality[_: P] = P(Index ~ inequality ~ ("==" ~ inequality).?).map{
    case (index, factor, None) => factor
    case (index, left, Some(right)) => Equality(index=index, left=left, right=right)
  }
  def inequality[_: P] = P(Index ~ conjunction ~ ("!=" ~/ conjunction).?).map{
    case (index, factor, None) => factor
    case (index, left, Some(right)) => Inequality(index=index, left=left, right=right)
  }
  def conjunction[_: P] = P(disjunction ~ ("&&".! ~/ disjunction).rep).map(evalBool(_))
  def disjunction[_: P] = P(boolFactor ~ ("||".! ~/ boolFactor).rep).map(evalBool(_))
  def numComp[_: P]: P[BooleanExpression] = P(arithmeticExpression ~ ("<=" | ">=" | "<" | ">").! ~/ arithmeticExpression).map{
    case (left: ArithmeticExpression, op, right: ArithmeticExpression) => op match {
      case "<" => Lt(index = left.index, left=left, right=right)
      case ">" => Gt(index = left.index, left=left, right=right)
      case "<=" => Leq(index = left.index, left=left, right=right)
      case ">=" => Geq(index = left.index, left=left, right=right)
    }
    case (left,op,right) => throw new IllegalArgumentException(s"Operator $op is only defined for arithmetic expressions. Got $left and $right instead.")
  }
  def inSet[_: P]: P[BooleanExpression] = P(Index ~ expression ~ "in" ~/ expression).map{
    case (index, left, right) => InSet(index=index,left=left,right=right)
  }
  def boolParens[_: P]: P[BooleanExpression] = P("(" ~ implication ~ ")")
  def boolFactor[_: P] = P((trueFalse | boolParens | NoCut(inSet) | NoCut(numComp) | NoCut(arithmeticExpression) | methodCall | 
    call | id))
  def trueFalse[_: P]: P[BooleanExpression] = P(Index ~ ("true".! | "false".!)).map{
    case (index,"true") => True(index=index)
    case (index,"false") => False(index=index)
  }

  def evalBool(tree: (BooleanExpression, Seq[(String, BooleanExpression)])): BooleanExpression =
    tree match {
      case (root, Nil) => root
      case (root, ("||", rest) :: xs) =>
        Disjunction(index = root.index, left=root, right=evalBool((rest, xs)))
      case (root, ("&&", rest) :: xs) =>
        Conjunction(index = root.index, left=root, right=evalBool((rest,xs)))
      case x => throw new IllegalArgumentException(s"Not a boolean expression: $x")
  }


  def args[_: P]: P[Seq[(ID, Option[TypeName])]] =
    P("(" ~ (id ~ typeAnnotation.?).rep(min=0, sep=",") ~ ")")

    // P("(" ~/ (id ~ ("," ~ " ".? ~ id).rep).? ~ ")").map {
    // case Some((id, seq)) =>
    //   id +: seq
    // case _ => Seq.empty
    // }

  def reactiveTuple[_: P]: P[Seq[ID]] = P(
    "(" ~/ id.rep(min = 1, sep = ",") ~ ")"
  )

  def reactiveList[_: P]: P[Seq[Seq[ID]]] = P(id.map(Seq(_)) | reactiveTuple).rep(
      min = 1,
      sep = ","
    )

  def transactionDef[_: P]: P[Transaction] = P(
    Index ~ "@transaction[" ~/ reactiveList ~ "]" ~/ precondition.rep ~ postcondition.rep ~ 
      ("transaction" | "def") ~/ id ~ args ~ "=" ~/ expression
  ).map {
    case (index, reactives, preconditions, postconditions, name, args, body) =>
      Transaction(
        name = name,
        reactives = reactives,
        preconditions = preconditions,
        postconditions = postconditions,
        args = args,
        index = index,
        body = body
      )
  }
  def expression[_: P]: P[Expression] = P(
    binding | methodCall | call | arithmeticExpression | id | underScore | string
  )
  def id[_: P]: P[ID] =
    P(!Keywords ~ Index ~ (!" " ~ CharIn("a-zA-Z0-9_")).repX(1).!).map {
      case (index, name) => ID(index = index, name = name)
    }
  def underScore[_: P]: P[UnderScore] = P(Index ~ "_").map(UnderScore(_))
  def binding[_: P]: P[Binding] =
    P(Index ~ "val" ~/ id ~/ typeAnnotation.? ~ "=" ~ expression).map {
      case (index, name, typeAnn, body) =>
       Binding(index = index, name = name, body = body, typeAnn=typeAnn)
    }

  def typeAnnotation[_: P] = P(":" ~/ typeName)
  def typeName[_: P]: P[TypeName] = P(CharIn("A-Z").! ~ CharIn("a-zA-Z").rep(1).! ~ ("[" ~ typeName.rep(min=1, sep=",") ~ "]").?).map{
    case (a,b, inner) => TypeName(name=a+b, inner=inner)
  }

  def callArgs[_: P] = P("(" ~/ expression.rep(min = 0, sep = ",") ~ ")")
  def call[_: P]: P[Call] = P(Index ~ id ~ callArgs).map {
    case (index, name, args) => Call(index = index, name = name, args = args)
  }

  def methodCall[_: P]: P[MethodCall] = P(
    Index ~ (NoCut(call)|id) ~ (!" " ~ "." ~/ id ~ callArgs.?).rep(1)
  ).map(evalMethodCall) 
  def evalMethodCall(seq:(Int, Expression, Seq[(ID,Option[Seq[Expression]])])): MethodCall =
    seq match {
      case (index, parent: MethodCall, Nil) => parent
      case (index, parent, (method, args) :: Nil) =>
        MethodCall(index = index, parent=parent, method=method, args=args.getOrElse(Seq()))
      case (index, parent, (method, args) :: rest) =>
        evalMethodCall(index,
        MethodCall(index = index, parent=parent, method=method, args=args.getOrElse(Seq())),
        rest)
      case _ => throw new IllegalArgumentException(s"Not a valid method call: $seq")
    }
  
  // def methodCall[_: P]: P[MethodCall] = P(
  //   Index ~ id ~ !" " ~ "." ~/ id ~ callArgs.?
  // ).map {
  //   case (index, parent, method, Some(args)) =>
  //     MethodCall(index = index, parent = parent, method = method, args = args)
  //   case (index, parent, method, None) =>
  //     MethodCall(index = index, parent = parent, method = method, args = Seq())
  // }

  def string[_: P]: P[StringExpr] = P(Index ~ "\"" ~/ (!"\"" ~ !"\n" ~ AnyChar).rep.! ~"\"").map{
    case (index, string) => StringExpr(index=index, content=string)
  }

  def number[_: P]: P[Number] =
    P(Index ~ CharIn("0-9").rep(1).!).map { case (index, num) =>
      Number(index = index, num = num.toInt)
    }
  def parens[_: P]: P[ArithmeticExpression] = P("(" ~ addSub ~ ")")
  def factor[_: P]: P[ArithmeticExpression] = P(
    number | parens | methodCall | call | id | underScore
  )

  def divMul[_: P]: P[ArithmeticExpression] =
    P(factor ~ (CharIn("*/").! ~/ factor).rep).map(evalArithm)
  def addSub[_: P]: P[ArithmeticExpression] =
    P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map(evalArithm)
  def arithmeticExpression[_: P]: P[ArithmeticExpression] = P(addSub)

  def evalArithm(tree: (ArithmeticExpression, Seq[(String, ArithmeticExpression)])): ArithmeticExpression =
    tree match {
      case (root, Nil) => root
      case (root, ("*", rest) :: xs) =>
        Multiplication(
          index = root.index,
          left = root,
          right = evalArithm(rest, xs)
        )
      case (root, ("/", rest) :: xs) =>
        Division(index = root.index, left = root, right = evalArithm(rest, xs))
      case (root, ("+", rest) :: xs) =>
        Addition(index = root.index, left = root, right = evalArithm(rest, xs))
      case (root, ("-", rest) :: xs) =>
        Substraction(
          index = root.index,
          left = root,
          right = evalArithm(rest, xs)
        )
      case _ =>
        throw new IllegalArgumentException(
          s"Not an arithmetic Expression: $tree"
        )
    }

  private def program[_: P]: P[Seq[ParsedExpression]] = P(
    (transactionDef | invariant | reactiveExpr | expression).rep(1) ~ End
  )

  def parse(input: String): Seq[ParsedExpression] = {
    fastparse.parse(input, program(_)) match {
      case Parsed.Success(ast, _) => ast
      case Parsed.Failure(_, _, extra) =>
        throw new Exception(extra.trace().longAggregateMsg)
      case f: Failure => throw new Exception(f.toString())
    }
  }

  def getLinePos(input: String, index: Int) = {
    val lines = input.substring(0, index + 1).split("\n")
    val lineNo = lines.length
    val linePos = index - lines.slice(0, lines.length - 1).mkString(" ").length
    s"$lineNo:$linePos"
  }
}
