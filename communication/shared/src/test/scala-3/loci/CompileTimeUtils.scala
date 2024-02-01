package loci

import utility.reflectionExtensions.*

import org.scalatest.exceptions.TestFailedException
import org.scalactic.source

import scala.annotation.compileTimeOnly
import scala.quoted.*

object CompileTimeUtils:
  inline def replace(inline value: String, inline from: String, inline to: String): String =
    ${ replaceImpl('value, 'from, 'to) }

  def replaceImpl(value: Expr[String], from: Expr[String], to: Expr[String])(using Quotes): Expr[String] =
    Expr(value.valueOrAbort.replace(from.valueOrAbort, to.valueOrAbort))

  inline def assertType[T](inline value: Any): Unit =
    ${ assertTypeImpl[T]('value) }

  def assertTypeImpl[T: Type](value: Expr[Any])(using Quotes): Expr[Unit] =
    import quotes.reflect.*

    object normalizer extends SimpleTypeMap(quotes):
      override def transform(tpe: TypeRepr) = tpe.dealias match
        case tpe: TypeRef => TypeIdent(tpe.typeSymbol).tpe
        case tpe => super.transform(tpe)

    val tpe = value.asTerm.tpe.widenTermRefByName

    if normalizer.transform(TypeRepr.of[T]).show == normalizer.transform(tpe).show then
      '{ () }
    else
      failTest(s"${value.show} has type `${tpe.show}`; type `${TypeRepr.of[T].show}` expected")

  inline def assertExactType[T](inline value: Any): Unit =
    ${ assertExactTypeImpl[T]('value) }

  def assertExactTypeImpl[T: Type](value: Expr[Any])(using Quotes): Expr[Unit] =
    import quotes.reflect.*

    object normalizer extends SimpleTypeMap(quotes):
      override def transform(tpe: TypeRepr) = tpe match
        case tpe: TypeRef => TypeIdent(tpe.typeSymbol).tpe
        case _ => super.transform(tpe)

    val tpe = value.asTerm.tpe.widenTermRefByName

    if normalizer.transform(TypeRepr.of[T]).show == normalizer.transform(tpe).show then
      '{ () }
    else
      failTest(s"${value.show} has type of form `${tpe.show}`; exact type `${TypeRepr.of[T].show}` expected")

  inline def assertNoFailedAssertion(inline expr: Any): Unit =
    ${ assertNoFailedAssertionImpl('expr) }

  def assertNoFailedAssertionImpl(expr: Expr[Any])(using Quotes): Expr[Unit] =
    import quotes.reflect.*

    val testFailedException = TypeRepr.of[TestFailedException]

    object failedAssertionsFinder extends TreeAccumulator[List[String]]:
      def foldTree(messages: List[String], tree: Tree)(owner: Symbol) = tree match
        case tree @ Apply(Select(New(_), _), args) if tree.tpe <:< testFailedException =>
           args match
             case Block(List(DefDef(_, _, _, Some(Apply(_, List(Literal(StringConstant(message))))))), Closure(_, _)) :: _ =>
               message :: messages
             case _ =>
               "Assertion in compiled code failed" :: messages
        case _ =>
          foldOverTree(messages, tree)(owner)

    (failedAssertionsFinder.foldTree(List.empty, expr.asTerm.underlyingArgument)(Symbol.spliceOwner).lastOption
      map failTest
      getOrElse '{ () })

  inline def abstractValuesInInstantiation(inline expr: Any): List[String] =
    ${ abstractValuesInInstantiationImpl('expr) }

  def abstractValuesInInstantiationImpl(expr: Expr[Any])(using Quotes): Expr[List[String]] =
    import quotes.reflect.*

    object instantiationOfAbstractValuesFinder extends TreeAccumulator[List[String]]:
      def foldTree(values: List[String], tree: Tree)(owner: Symbol) = tree match
        case New(tpt) =>
          val members = tpt.symbol.fieldMembers ++ tpt.symbol.methodMembers collect {
            case symbol if symbol.isTerm && symbol.isAbstract =>
              symbol.name
          }
          foldOverTree(members ++ values, tree)(owner)
        case _ =>
          foldOverTree(values, tree)(owner)

    Expr(instantiationOfAbstractValuesFinder.foldOverTree(List.empty, expr.asTerm.underlyingArgument)(Symbol.spliceOwner))

  inline def containsCompileTimeOnly(inline expr: Any): Boolean =
    ${ containsCompileTimeOnlyImpl('expr) }

  def containsCompileTimeOnlyImpl(expr: Expr[Any])(using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    val compileTimeOnlyAnnotation = TypeRepr.of[compileTimeOnly]

    object compileTimeOnlyAnnotationFinder extends TreeAccumulator[Boolean]:
      def foldTree(compileTimeOnlyFound: Boolean, tree: Tree)(owner: Symbol) =
        compileTimeOnlyFound ||
        (tree.symbol.annotations exists { _.tpe <:< compileTimeOnlyAnnotation }) ||
        foldOverTree(false, tree)(owner)

    Expr(compileTimeOnlyAnnotationFinder.foldTree(false, expr.asTerm)(Symbol.spliceOwner))

  inline def containsValueOfType[T, U]: Boolean =
    ${ containsValueOfTypeImpl[T, U] }

  def containsValueOfTypeImpl[T: Type, U: Type](using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    val T = TypeRepr.of[T].typeSymbol
    val U = TypeRepr.of[U]

    Expr(T.declaredFields ++ T.declaredMethods exists { Ref(_).tpe.finalResultType <:< U })

  private def failTest(message: String)(using Quotes) =
    import quotes.reflect.*
    '{ throw new TestFailedException(_ => Some(${Expr(message)}), None, Left(source.Position.here), None, Vector.empty) }
end CompileTimeUtils
