package loci
package utility

import scala.reflect.NameTransformer
import scala.reflect.macros.{TypecheckException, blackbox}

object platform {
  def annotation(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    val q"new $expr(...$exprss).macroTransform(...$_)" = c.macroApplication: @unchecked

    if (exprss.size != 1 || exprss.head.size != 1)
      c.abort(expr.pos, "wrong number of arguments")

    val arg =
      try
        exprss.head.head match {
          case q"cond = $arg" => c typecheck arg
          case arg @ q"$name = $_" => c.abort(arg.pos, s"unknown parameter name: $name")
          case arg => c typecheck arg
        }
      catch {
        case TypecheckException(pos: Position @unchecked, msg) =>
          c.abort(pos, msg)
      }

    val keep = evaluateBooleanExpr(c)(arg)

    def compileTimeOnly(mods: Modifiers) = mods mapAnnotations {
      q"""new ${termNames.ROOTPKG}.scala.annotation.compileTimeOnly("Not available on current platform")""" :: _
    }

    def clearStatements(impl: Template): Template = {
      val body = impl.body flatMap {
        case tree @ ClassDef(mods, name, tparams, impl) =>
          Some(treeCopy.ClassDef(tree, mods, name, tparams, clearStatements(impl)))
        case tree @ ModuleDef(mods, name, impl) =>
          Some(treeCopy.ModuleDef(tree, mods, name, clearStatements(impl)))
        case tree: MemberDef =>
          Some(tree)
        case _ =>
          None
      }
      treeCopy.Template(impl, impl.parents, impl.self, body)
    }

    annottees match {
      case (annottee @ ClassDef(mods, name, tparams, impl)) :: companion =>
        val tree = if (keep) annottee else treeCopy.ClassDef(annottee, compileTimeOnly(mods), name, tparams, clearStatements(impl))
        companion.headOption.fold[Tree](tree) { companion => q"$tree; $companion" }

      case (annottee @ ModuleDef(mods, name, impl)) :: Nil =>
        if (keep) annottee else treeCopy.ModuleDef(annottee, compileTimeOnly(mods), name, clearStatements(impl))

      case (annottee @ DefDef(mods, name, tparams, vparamss, tpt, rhs)) :: Nil =>
        if (keep) annottee else treeCopy.DefDef(annottee, compileTimeOnly(mods), name, tparams, vparamss, tpt, rhs)

      case (annottee @ ValDef(mods, name, tpt, rhs)) :: Nil =>
        if (keep) annottee else treeCopy.ValDef(annottee, compileTimeOnly(mods), name, tpt, rhs)

      case _ =>
        c.abort(c.enclosingPosition,
          "platform annotation only applicable to classes, traits, objects or member definitions")
    }
  }

  def apply[T](c: blackbox.Context)(cond: c.Tree)(body: c.Tree): c.Tree = {
    import c.universe._
    if (evaluateBooleanExpr(c)(cond)) body else q"()"
  }

  def value[T](c: blackbox.Context)(selection: c.Tree*): c.Tree = {
    import c.universe._

    val unconditionalSelection = symbolOf[loci.platform.UnconditionalSelection]
    val conditionalSelection = symbolOf[loci.platform.Selection.type]
    val arrowAssoc = symbolOf[ArrowAssoc[_]]
    val tuple2 = symbolOf[Tuple2[_, _]]

    val selections = selection map {
      case q"$conditional[..$_]($selection)"
          if conditional.symbol.owner == conditionalSelection &&
             conditional.symbol.name.toString == "conditional" =>
        val conditionalExpr = selection match {
          case q"$arrow[..$_]($condition).->[..$_]($expr)"
              if arrow.symbol.isMethod &&
                 arrow.symbol.asMethod.returnType.typeSymbol == arrowAssoc =>
            Right(condition -> expr)
          case q"new $arrow[..$_]($condition).->[..$_]($expr)"
              if arrow.symbol == arrowAssoc =>
            Right(condition -> expr)
          case q"$pair[..$_]($condition, $expr)"
              if pair.symbol.isMethod &&
                 pair.symbol.asMethod.returnType.typeSymbol == tuple2 =>
            Right(condition -> expr)
          case q"new $pair[..$_]($condition, $expr)"
              if pair.symbol == tuple2 =>
            Right(condition -> expr)
          case q"($condition, $expr)" =>
            Right(condition -> expr)
          case _ =>
            Left(selection.pos)
        }

        compatibility.either.map(conditionalExpr) { case (condition, expr) =>
          evaluateBooleanExpr(c)(condition) -> expr
        }

      case q"$unconditional[..$_]($expr)"
          if unconditional.symbol.owner == unconditionalSelection &&
             unconditional.symbol.name.toString == "unconditional" =>
        Right(true -> expr)

      case selection
        if selection.tpe != null &&
           (selection.tpe <:< definitions.NothingTpe ||
            selection.tpe <:< definitions.NullTpe) =>
        Right(true -> selection)

      case selection =>
        Left(selection.pos)
    }

    selections foreach {
      case Left(pos) => c.abort(pos, "unexpected platform selection")
      case _ =>
    }

    (selections
      collectFirst { case Right((true, expr)) => expr }
      getOrElse c.abort(c.enclosingPosition, "no selection matches the current platform"))
  }

  private def evaluateBooleanExpr(c: blackbox.Context)(expr: c.Tree): Boolean = {
    import c.universe._

    val platform = symbolOf[loci.platform.type]

    def isStable(tree: Tree): Boolean = tree match {
      case Select(qualifier, _) if tree.symbol.isTerm && tree.symbol.asTerm.isStable =>
        isStable(qualifier)
      case Ident(_) if tree.symbol.isTerm && tree.symbol.asTerm.isStable =>
        true
      case Literal(_) =>
        true
      case _ =>
        false
    }

    expr match {
      case Apply(Select(qualifier, name), args) =>
        val operator = NameTransformer.decode(name.toString)
        val lhs = evaluateBooleanExpr(c)(qualifier)

        def rhs = {
          if (args.size != 1)
            c.abort(expr.pos, s"unexpected number of arguments for operator $operator: ${args.size}")
          evaluateBooleanExpr(c)(args.head)
        }

        operator match {
          case "==" => lhs == rhs
          case "!=" => lhs != rhs
          case "&" | "&&" => lhs & rhs
          case "|" | "||" => lhs | rhs
          case "^" => lhs ^ rhs
          case _ => c.abort(expr.pos, s"unknown operator: $operator")
        }

      case Select(qualifier, name)
        if expr.symbol.isTerm &&
           !expr.symbol.asTerm.isStable &&
           NameTransformer.decode(name.toString) == "unary_!" =>
        !evaluateBooleanExpr(c)(qualifier)

      case _
        if expr.symbol != null &&
           expr.symbol.isMethod &&
           expr.symbol.owner == platform &&
           expr.tpe != null &&
           expr.tpe <:< typeOf[Boolean] &&
           isStable(expr) =>
        try
          loci.platform.getClass.getMethod(expr.symbol.name.toString).invoke(loci.platform).asInstanceOf[Boolean]
        catch {
          case _: NoSuchMethodException | _: IllegalArgumentException | _: ClassCastException =>
            c.abort(expr.pos, s"failed to read value: $expr")
        }

      case _ if isStable(expr) =>
        if (!(expr.tpe <:< typeOf[Boolean]))
          c.abort(expr.pos, s"not a boolean expression: $expr")

        expr.tpe match {
          case ConstantType(Constant(value: Boolean)) =>
            value
          case _ =>
            c.abort(expr.pos, s"constant value not known at compile time: $expr")
        }

      case _ =>
        c.abort(expr.pos, s"not a constant expression: $expr")
    }
  }
}
