package fr

object AST {
  sealed trait ParsedExpression {
    val index: Int // position in source code
  }

  // viper preconditions
  sealed trait Annotation
  
  // e.g. @requires x > y ==> x > z
  case class Precondition(index: Int, body: ViperExpression)
      extends Annotation
      with ParsedExpression
  // e.g. @ensures x > y ==> x > z
  case class Postcondition(index: Int, body: ViperExpression)
      extends Annotation
      with ParsedExpression
  // e.g. @invariant balance >= 0
  case class Invariant(index: Int, body: ViperExpression)
      extends Annotation
      with ParsedExpression

  // e.g.
  // @transaction[reactives]
  // @requires ...
  // @ensures ...
  // def id(x) = x
  case class Transaction(
      index: Int,
      name: ID,
      reactives: Seq[Seq[ID]],
      preconditions: Seq[Precondition],
      postconditions: Seq[Postcondition],
      args: Seq[(ID, Option[TypeName])],
      body: Expression // can contain statements and simple expressions
  ) extends ParsedExpression

  sealed trait ViperExpression extends Expression

  // everything that modifies the dataflow graph
  sealed trait Reactive extends ParsedExpression {
      val name: ID
      val body: Expression
      val typeAnn: Option[TypeName]
  }
  // e.g. Source(0)
  case class SourceReactive(index: Int, name: ID, body: Expression, typeAnn: Option[TypeName])
      extends Reactive
  // e.g. Derived{a+b}
  case class DerivedReactive(index: Int, name: ID, body: Expression, typeAnn: Option[TypeName])
      extends Reactive

  // everything that can be reduced to a value
  sealed trait Expression extends ParsedExpression
  // e.g. a
  case class ID(index: Int, name: String) extends Expression
    with ViperExpression with ArithmeticExpression with BooleanExpression
  // _
  case class UnderScore(index: Int) extends Expression with ArithmeticExpression
  // e.g. val a = 5
  case class Binding(index: Int, name: ID, body: Expression, typeAnn: Option[TypeName])
      extends ParsedExpression
      with Expression
  // e.g. 1+10
  // case class ArithmeticExpression(index: Int, body: String) extends ParsedExpression with Expression
  // e.g. print(5)
  case class Call(index: Int, name: ID, args: Seq[Expression])
      extends ParsedExpression
      with ArithmeticExpression
      with BooleanExpression
  // e.g. a.toString()
  case class MethodCall(
      index: Int,
      parent: Expression,
      method: ID,
      args: Seq[Expression]
  ) extends ParsedExpression
      with ArithmeticExpression
      with BooleanExpression

  // arithmetic expressions
  // e.g. 10 + _ * 24
  sealed trait ArithmeticExpression extends BooleanExpression
  case class Number(index: Int, num: Int) extends ArithmeticExpression{
      override def toString() = num.toString()
  }
  case class Parens(index: Int, inner: ArithmeticExpression)
      extends ArithmeticExpression
  case class Division(index: Int, left: Expression, right: Expression)
      extends ArithmeticExpression
  case class Multiplication(index: Int, left: Expression, right: Expression)
      extends ArithmeticExpression
  case class Addition(index: Int, left: Expression, right: Expression)
      extends ArithmeticExpression
  case class Substraction(index: Int, left: Expression, right: Expression)
      extends ArithmeticExpression

  sealed trait BooleanExpression extends Expression with ViperExpression
  case class True(index: Int) extends BooleanExpression
  case class False(index: Int) extends BooleanExpression
  case class BoolParens(index: Int, inner: BooleanExpression) extends BooleanExpression
  
  case class Lt(index: Int, left: ArithmeticExpression, right: ArithmeticExpression) extends BooleanExpression
  case class Gt(index: Int, left: ArithmeticExpression, right: ArithmeticExpression) extends BooleanExpression
  case class Leq(index: Int, left: ArithmeticExpression, right: ArithmeticExpression) extends BooleanExpression
  case class Geq(index: Int, left: ArithmeticExpression, right: ArithmeticExpression) extends BooleanExpression
  case class Equality(index: Int, left: Expression, right: Expression) extends BooleanExpression
  case class Inequality(index: Int, left: Expression, right: Expression) extends BooleanExpression
  case class Disjunction(index: Int, left: Expression, right: Expression) extends BooleanExpression
  case class Conjunction(index: Int, left: Expression, right: Expression) extends BooleanExpression
  case class Implication(index: Int, left: Expression, right: Expression) extends BooleanExpression

  case class InSet(index: Int, left: Expression, right: Expression) extends BooleanExpression
  
  sealed trait Quantification extends BooleanExpression
  case class Forall (index: Int, vars: Seq[(ID, TypeName)], triggers: Option[ViperExpression],
    body: BooleanExpression) extends Quantification 
  case class Exists (index: Int, vars: Seq[(ID, TypeName)],
    body: BooleanExpression) extends Quantification
  case class StringExpr(index: Int, content: String) extends Expression

  case class TypeName(name: String, inner: Option[Seq[TypeName]]){
      override def toString() = inner match {
        case Some(i) => s"$name[${i.mkString(", ")}]"
        case None => name
      }
  }
}
