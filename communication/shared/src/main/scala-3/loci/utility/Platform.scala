package loci
package utility

import utility.reflectionExtensions.*

import scala.quoted.*

object platform:
  def apply[T: Type](cond: Expr[Boolean])(body: Expr[T])(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    if (evaluateBooleanExpr(cond.asTerm.underlyingArgument)) '{ $body; () } else '{ () }

  def value[T: Type](selection: Expr[Seq[(Boolean, T) | T]])(using Quotes): Expr[T] =
    import quotes.reflect.*

    val selectionElements = (selection.asTerm.underlyingArgument: @unchecked) match
      case Repeated(elems, _) => elems
      case Typed(Repeated(elems, _), _) => elems

    val `T` = TypeRepr.of[T]
    val `(Boolean, T)` = TypeRepr.of[(Boolean, T)]

    val selections = selectionElements map { selection =>
      if selection.tpe <:< `(Boolean, T)` &&
         selection.tpe.typeSymbol != defn.NullClass &&
         selection.tpe.typeSymbol != defn.NothingClass then
        val conditionalExpr = selection.asExpr match
          case '{ ArrowAssoc($condition: Boolean) -> ($expr: T) } =>
            Right(condition -> expr)
          case '{ new ArrowAssoc($condition: Boolean) -> ($expr: T) } =>
            Right(condition -> expr)
          case '{ Tuple2($condition: Boolean, $expr: T) } =>
            Right(condition -> expr)
          case '{ new Tuple2($condition: Boolean, $expr: T) } =>
            Right(condition -> expr)
          case _ =>
            Left(selection.pos)

        conditionalExpr map { (condition, expr) =>
          evaluateBooleanExpr(condition.asTerm) -> expr
        }

      else if selection.tpe <:< `T` then
        Right(true -> selection.asExprOf[T])

      else
        Left(selection.pos)
    }

    selections foreach {
      case Left(pos) => report.errorAndAbort("unexpected platform selection", pos)
      case _ =>
    }

    (selections
      collectFirst { case Right(true -> expr) => expr }
      getOrElse report.errorAndAbort("no selection matches the current platform"))
  end value

  private def evaluateBooleanExpr(using Quotes)(expr: quotes.reflect.Term): Boolean =
    import quotes.reflect.*

    val platform = TypeRepr.of[loci.platform.type].typeSymbol

    def isStable(tree: Term): Boolean = tree match
      case Select(qualifier, _) if tree.symbol.isStable =>
        isStable(qualifier)
      case Ident(_) if tree.symbol.isStable =>
        true
      case Literal(_) =>
        true
      case _ =>
        false

    expr match
      case Apply(Select(qualifier, name), args) =>
        val lhs = evaluateBooleanExpr(qualifier)

        def rhs =
          if args.size != 1 then
            report.errorAndAbort(s"unexpected number of arguments for operator $name: ${args.size}", expr.pos)
          evaluateBooleanExpr(args.head)

        name match
          case "==" => lhs == rhs
          case "!=" => lhs != rhs
          case "&" | "&&" => lhs & rhs
          case "|" | "||" => lhs | rhs
          case "^" => lhs ^ rhs
          case _ => report.errorAndAbort(s"unknown operator: $name", expr.pos)

      case Select(qualifier, name)
        if !expr.symbol.isStable &&
           name == "unary_!" =>
        !evaluateBooleanExpr(qualifier)

      case _
        if expr.symbol.isField &&
           expr.symbol.owner == platform &&
           expr.tpe <:< TypeRepr.of[Boolean] &&
           isStable(expr) =>
        try
          loci.platform.getClass.getMethod(expr.symbol.name).invoke(loci.platform).asInstanceOf[Boolean]
        catch
          case _: NoSuchMethodException | _: IllegalArgumentException | _: ClassCastException =>
            report.errorAndAbort(s"failed to read value: ${expr.safeShow("")}", expr.pos)

      case _ if isStable(expr) =>
        if !(expr.tpe <:< TypeRepr.of[Boolean]) then
          report.errorAndAbort(s"not a boolean expression: ${expr.safeShow("")}", expr.pos)

        expr.tpe.widenTermRefByName match
          case ConstantType(BooleanConstant(value)) =>
            value
          case _ =>
            report.errorAndAbort(s"constant value not known at compile time: ${expr.safeShow("")}", expr.pos)

      case _ =>
        report.errorAndAbort(s"not a constant expression: ${expr.safeShow("")}", expr.pos)
  end evaluateBooleanExpr
end platform
