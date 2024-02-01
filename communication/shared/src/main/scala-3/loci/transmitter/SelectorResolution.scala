package loci
package transmitter

import utility.reflectionExtensions.*

import scala.quoted.*

object SelectorResolution:
  type TypeMember[+T]

  final class Index[M <: AnyKind, D <: AnyKind, / <: AnyKind, T <: TypeMember[Any], V, S, N <: Int]

  object Index:
    transparent inline given [M <: AnyKind, D <: AnyKind, / <: AnyKind, T <: TypeMember[Any], V, S, N <: Int](
      using IndexComputation[M, D, /, T, V, S, N])
      : Index[M, D, /, T, V, S, N] = Index()


  final class IndexComputation[M <: AnyKind, D <: AnyKind, / <: AnyKind, T <: TypeMember[Any], V, S, -N]

  object IndexComputation:
    transparent inline given [M <: AnyKind, D <: AnyKind, / <: AnyKind, T <: TypeMember[Any], V, S]
      : IndexComputation[M, D, /, T, V, S, Nothing] = ${ indexComputationImpl[M, D, /, T, V, S] }


  def indexComputationImpl[M <: AnyKind: Type, D <: AnyKind: Type, / <: AnyKind: Type, T <: TypeMember[Any]: Type, V: Type, S: Type](using Quotes) =
    import quotes.reflect.*

    val message = TypeRepr.of[M].typeSymbol
    val delegates = TypeRepr.of[D].typeSymbol
    val list = TypeRepr.of[/].typeSymbol

    val transmittableMember = TypeRepr.of[T].typeArgs.head.typeSymbol
    val transmittableClass = transmittableMember.owner

    val V = TypeRepr.of[V]
    val S = TypeRepr.of[S]

    val (checkLow, checkHigh) =
      val transmittableType = TypeIdent(transmittableClass).tpe.appliedTo(
        transmittableClass.declaredTypes collect { case symbol if symbol.isTypeParam => TypeIdent(symbol).tpe })

      transmittableType.memberType(transmittableMember) match
        case TypeBounds(low, hi) =>
          (low.typeSymbol != defn.NothingClass || !hi.typeSymbol.flags.is(Flags.Covariant)) ->
          (hi.typeSymbol != defn.AnyClass || !low.typeSymbol.flags.is(Flags.Contravariant))
        case _ =>
          true -> true

    def matchesMember(transmittable: TypeRepr) =
      if transmittable.derivesFrom(transmittableClass) then
        transmittable.memberType(transmittableMember) match
          case TypeBounds(low, hi) =>
            (checkLow && checkHigh && low == hi && low =:= V) ||
            ((!checkLow || V <:< low) && (!checkHigh || hi <:< V))
          case tpe =>
            tpe =:= V
      else
        false

    def fail =
      val transmittableOwner = transmittableClass.owner
      val transmittableCompanion = transmittableOwner.companionModule match
        case companion if companion.exists => companion
        case _ => transmittableOwner

      report.errorAndAbort(
        s"${transmittableCompanion.name}.${transmittableClass.name} " +
        s"with $transmittableMember = ${V.safeShow} not specified in: ${S.dealias.safeShow}")

    def result(n: Int) =
      ConstantType(IntConstant(n)).asType match
        case '[n] =>
          '{ IndexComputation[M, D, /, T, V, S, n] }

    def index(delegating: TypeRepr)(n: Int): Expr[IndexComputation[M, D, /, T, V, S, Nothing]] =
      delegating.baseType(list) match
        case AppliedType(_, List(tail, head)) =>
          if matchesMember(head) then result(n)
          else index(tail)(n + 1)
        case _ if matchesMember(delegating) =>
          result(n)
        case _ =>
          fail

    S.baseType(message) match
      case AppliedType(_, List(transmittable)) =>
        if matchesMember(transmittable) then result(0)
        else fail
      case _ =>
        S.baseType(delegates) match
          case AppliedType(_, List(delegating)) => index(delegating)(0)
          case _ => fail
  end indexComputationImpl
end SelectorResolution
