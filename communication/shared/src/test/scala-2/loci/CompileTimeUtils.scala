package loci

import org.scalatest.exceptions.TestFailedException

import scala.annotation.compileTimeOnly
import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.api.Position
import scala.reflect.macros.{ParseException, TypecheckException, whitebox}

object CompileTimeUtils {
  def replace(value: String, from: String, to: String): String =
    macro replaceImpl

  def replaceImpl(c: whitebox.Context)(value: c.Tree, from: c.Tree, to: c.Tree): c.Tree = {
    import c.universe._

    (value, from, to) match {
      case (Literal(Constant(value: String)), Literal(Constant(from: String)), Literal(Constant(to: String))) =>
        Literal(Constant(value.replace(from, to)))
      case _ =>
        c.abort(c.enclosingPosition, "string literal expected")
    }
  }

  def assertType[T](value: Any): Unit =
    macro assertTypeImpl[T]

  def assertTypeImpl[T: c.WeakTypeTag](c: whitebox.Context)(value: c.Tree): c.Tree = {
    import c.universe._

    val tpe = weakTypeOf[T]

    if (tpe =:= value.tpe)
      q"()"
    else
      failTest(c)(s"$value has type `${value.tpe}`; type `$tpe` expected")
  }

  def assertExactType[T](value: Any): Unit =
    macro assertExactTypeImpl[T]

  def assertExactTypeImpl[T: c.WeakTypeTag](c: whitebox.Context)(value: c.Tree): c.Tree = {
    import c.universe._

    def normalize(tpe: Type) = tpe map {
      case SingleType(ThisType(preSym), sym) if sym.owner == preSym =>
        if (sym.isType)
          internal.thisType(sym)
        else if (sym.isModule)
          internal.thisType(sym.asModule.moduleClass)
        else
          tpe
      case tpe =>
        tpe
    }

    val tpe = weakTypeOf[T]

    if (normalize(tpe) == normalize(value.tpe))
      q"()"
    else
      failTest(c)(s"$value has type of form `${value.tpe}`; exact type `$tpe` expected")
  }

  def assertNoFailedAssertion(expr: Any): Unit =
    macro assertNoFailedAssertionImpl

  def assertNoFailedAssertionImpl(c: whitebox.Context)(expr: c.Tree): c.Tree = {
    import c.universe._

    val testFailedException = typeOf[TestFailedException]

    val messages = expr collect {
      case tree @ Apply(Select(New(_), _), args) if tree.tpe <:< testFailedException =>
        args match {
          case Function(_, Apply(_, List(Literal(Constant(message: String))))) :: _ => message
          case _ => "Assertion in compiled code failed"
        }
    }

    messages.headOption map { failTest(c)(_) } getOrElse q"()"
  }

  def abstractValuesInInstantiation(expr: Any): List[String] =
    macro abstractValuesInInstantiationImpl

  def abstractValuesInInstantiationImpl(c: whitebox.Context)(expr: c.Tree): c.Tree = {
    import c.universe._

    val abstractValues = mutable.ListBuffer.empty[String]

    expr foreach {
      case New(tpt) =>
        val values = tpt.tpe.members collect {
          case symbol if symbol.isTerm && symbol.isAbstract =>
            symbol.name.decodedName.toString
        }
        abstractValues ++= values

      case _ =>
    }

    q"${abstractValues.result()}"
  }

  def containsCompileTimeOnly(expr: Any): Boolean =
    macro containsCompileTimeOnlyImpl

  def containsCompileTimeOnlyImpl(c: whitebox.Context)(expr: c.Tree): c.Tree = {
    import c.universe._

    val compileTimeOnlyAnnotation = typeOf[compileTimeOnly]

    val compileTimeOnlyFound = expr exists {
      case tree: RefTree =>
        tree.symbol.annotations exists { _.tree.tpe <:< compileTimeOnlyAnnotation }
      case _ =>
        false
    }

    q"$compileTimeOnlyFound"
  }

  def containsValueOfType[T, U]: Boolean =
    macro containsValueOfTypeImpl[T, U]

  def containsValueOfTypeImpl[T: c.WeakTypeTag, U: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val T = weakTypeOf[T]
    val U = weakTypeOf[U]

    q"${T.decls exists { decl => decl.isTerm && decl.info.finalResultType <:< U } }"
  }

  def compile(expr: String): Any =
    macro compileImpl

  def compileImpl(c: whitebox.Context)(expr: c.Tree) = {
    def reportException(pos: Position, msg: String) = pos match {
      case pos: c.universe.Position @unchecked => c.abort(pos, msg)
      case _ => c.abort(c.enclosingPosition, msg)
    }

    import c.universe._

    expr match {
      case Literal(Constant(expr: String)) =>
        try {
          val tree = c.typecheck(c.parse(expr))
          if (tree.tpe == NoType)
            internal.setType(tree, definitions.UnitTpe)
          tree
        }
        catch {
          case e: ParseException => reportException(e.pos, e.msg)
          case e: TypecheckException => reportException(e.pos, e.msg)
        }

      case _ =>
        c.abort(c.enclosingPosition, "string literal expected")
    }
  }

  private def failTest(c: whitebox.Context)(message: String) = {
    import c.universe._

    q"""throw new ${termNames.ROOTPKG}.org.scalatest.exceptions.TestFailedException(
      _ => ${termNames.ROOTPKG}.scala.Some($message),
      ${termNames.ROOTPKG}.scala.None,
      ${termNames.ROOTPKG}.scala.Left(${termNames.ROOTPKG}.org.scalactic.source.Position.here),
      ${termNames.ROOTPKG}.scala.None,
      ${termNames.ROOTPKG}.scala.Vector.empty)"""
  }
}
