package loci
package transmitter

import utility.reflectionExtensions.*

import scala.collection.mutable
import scala.quoted.*
import scala.util.DynamicVariable

object TransmittableResolution:
  private val optimizedTransmittableResolutionInProgress = DynamicVariable(false)

  def optimizedTransmittableResolution[B: Type, I: Type, R: Type, P: Type, T <: Transmittables: Type](using Quotes) =
    import quotes.reflect.*

    if optimizedTransmittableResolutionInProgress.value then
      report.errorAndAbort("Skipping transmittable resolution macro for recursive invocation")

    optimizedTransmittableResolutionInProgress.withValue(true) {
      val resolutionDefault = TypeRepr.of[Transmittable.ResolutionDefault].typeSymbol
      val resolutionAlternation = TypeRepr.of[Transmittable.ResolutionAlternation].typeSymbol
      val transmittable = TypeRepr.of[Transmittable.Any[?, ?, ?]].typeSymbol
      val identicallyTransmittable = TypeRepr.of[IdenticallyTransmittable[?]].typeSymbol
      val surrogateNothing = TypeRepr.of[Transmittable.SurrogateNothing].typeSymbol

      val transmittableParameters = (List("Base", "Intermediate", "Result", "Proxy", "Transmittables")
        map transmittable.typeMember)

      val aux = TypeIdent(TypeRepr.of[Transmittable.type].typeSymbol.typeMember("Aux")).tpe

      def boundsAsAlias(tpe: TypeRepr) = tpe match
        case TypeBounds(low, hi) if low =:= hi => low
        case TypeBounds(low, hi) if low.typeSymbol == defn.NothingClass => hi
        case TypeBounds(low, hi) if hi.typeSymbol == defn.AnyClass => low
        case _ => tpe

      def inferredOrWildcard(tpe: TypeRepr) =
        if IsInferred(tpe) then tpe else TypeBounds.empty

      object approximator extends SimpleTypeMap(quotes):
        override def transform(tpe: TypeRepr) = tpe match
          case _ if tpe.typeSymbol == defn.AnyClass || tpe.typeSymbol == defn.NothingClass =>
            TypeBounds.empty
          case tpe: AppliedType =>
            val bounds = tpe.tycon.typeSymbol.declaredTypes collect { case symbol if symbol.isTypeParam => tpe.tycon.memberType(symbol) }
            if tpe.args.size == bounds.size then
              val tycon = transform(tpe.tycon)
              val args = tpe.args zip bounds map {
                case (tpe, TypeBounds(low, hi)) if tpe =:= low || tpe =:= hi => TypeBounds.empty
                case (tpe, _) => transform(tpe)
              }
              if tycon != tpe.tycon || args != tpe.args then tycon.appliedTo(args) else tpe
            else
              super.transform(tpe)
          case _ =>
            super.transform(tpe)
      end approximator

      object surrogator extends SimpleTypeMap(quotes):
        override def transform(tpe: TypeRepr) = tpe match
          case TypeBounds(low, hi) if low.typeSymbol == defn.NothingClass =>
            TypeBounds(low, super.transform(hi))
          case _ if tpe.typeSymbol == defn.NothingClass =>
            TypeRepr.of[Transmittable.SurrogateNothing]
          case _ =>
            super.transform(tpe)
      end surrogator

      val resolutionType =
        val AppliedType(tycon, List(b, i, r, p, t)) = approximator.transform(TypeRepr.of[Transmittable.Resolution[B, I, R, P, T]]): @unchecked
        surrogator.transform(tycon.appliedTo(List(TypeRepr.of[B], inferredOrWildcard(i), inferredOrWildcard(r), p, t)))

      Implicits.search(resolutionType) match
        case result: ImplicitSearchSuccess =>
          object deskolemizerAndTransmittablesAliaser extends SimpleTypeMap(quotes):
            override def transform(tpe: TypeRepr) = tpe match
              case _ if tpe.typeSymbol == transmittable =>
                val aliased =
                  aux.appliedTo(transmittableParameters map { param =>
                    transform(boundsAsAlias(tpe.resolvedMemberType(param)))
                  })
                if aliased != tpe then aliased else tpe
              case TypeRef(qualifier, _) if qualifier.getClass.getSimpleName contains "Skolem" =>
                val dealiased = tpe.dealias
                if dealiased != tpe then transform(dealiased) else tpe
              case _ if tpe.typeSymbol == surrogateNothing =>
                TypeRepr.of[Nothing]
              case _ =>
                super.transform(tpe)
          end deskolemizerAndTransmittablesAliaser

          object optimizer extends TreeMap:
            override def transformTypeTree(tree: TypeTree)(owner: Symbol) =
              val tpe = deskolemizerAndTransmittablesAliaser.transform(tree.tpe)
              if tpe != tree.tpe then TypeTree.of(using tpe.asType) else tree

            override def transformTerm(tree: Term)(owner: Symbol) = tree match
              case Apply(TypeApply(fun, _), List(_, _, arg))
                  if fun.symbol.owner == resolutionDefault || fun.symbol.owner == resolutionAlternation =>
                val tpe = arg.tpe.widenTermRefByName
                val args = transmittableParameters map { param =>
                  deskolemizerAndTransmittablesAliaser.transform(boundsAsAlias(tpe.resolvedMemberType(param)))
                }
                val Apply(TypeApply(resolution, _), _) = '{ Transmittable.Resolution(???) }.asTerm.underlying: @unchecked

                resolution.appliedToTypes(args).appliedTo(transformTerm(arg)(owner))

              case _ if tree.tpe.derivesFrom(identicallyTransmittable) =>
                val AppliedType(_, List(tpe)) = tree.tpe.widenTermRefByName.dealias: @unchecked
                val arg = deskolemizerAndTransmittablesAliaser.transform(tpe)
                val Apply(TypeApply(transmittable, _), _) = '{ IdenticallyTransmittable[Any]() }.asTerm.underlying: @unchecked

                transmittable.appliedToType(arg).appliedToNone

              case _ =>
                super.transformTerm(tree)(owner)
          end optimizer

          def typeParamInstantiations(expected: TypeRepr, actual: TypeRepr): Map[ParamRef, TypeRepr] =
            def stripRefinements(tpe: TypeRepr): TypeRepr = tpe match
              case Refinement(parent, _ , _) => stripRefinements(parent)
              case _ => tpe

            def stripTypeVar(tpe: TypeRepr): TypeRepr =
              if tpe.getClass.getSimpleName contains "TypeVar" then TypeTree.of(using tpe.asType).tpe else tpe

            def transmittableArgs(tpe: TypeRepr): List[TypeRepr] =
              transmittableParameters.take(3) map { param => stripTypeVar(boundsAsAlias(tpe.resolvedMemberType(param))) }

            def hasParamRef(tpe: TypeRepr): Boolean = stripTypeVar(tpe) match
              case AppliedType(tycon, args) => hasParamRef(tycon) || (args exists hasParamRef)
              case TermRef(qual, _) => hasParamRef(qual)
              case TypeRef(qual, _) => hasParamRef(qual)
              case AnnotatedType(underlying, _) => hasParamRef(underlying)
              case ThisType(ref) => hasParamRef(ref)
              case SuperType(thistpe, supertpe) => hasParamRef(thistpe) || hasParamRef(supertpe)
              case Refinement(parent, _, info) => hasParamRef(parent) || hasParamRef(info)
              case ByNameType(underlying) => hasParamRef(underlying)
              case AndType(lhs, rhs) => hasParamRef(lhs) || hasParamRef(rhs)
              case OrType(lhs, rhs) => hasParamRef(lhs) || hasParamRef(rhs)
              case TypeBounds(low, hi) => hasParamRef(low) || hasParamRef(hi)
              case ConstantType(_) => false
              case _: NoPrefix => false
              case _ => true

            (stripTypeVar(expected), stripTypeVar(actual)) match
              case (expected, actual) if expected.derivesFrom(transmittable) && actual.derivesFrom(transmittable) =>
                val expectedArgs = transmittableArgs(expected)
                val actualArgs =
                  if actual.derivesFrom(identicallyTransmittable) then
                    val List(b, i, r) = expectedArgs
                    if !hasParamRef(b) then List(b, b, b)
                    else if !hasParamRef(r) then List(r, r, r)
                    else if !hasParamRef(i) then List(i, i, i)
                    else transmittableArgs(actual)
                  else
                    transmittableArgs(actual)

                (expectedArgs zip actualArgs flatMap typeParamInstantiations).toMap

              case (expected: AppliedType, actual: AppliedType) =>
                (expected.dealias, actual.dealias) match
                  case (AppliedType(expectedFun, expectedArgs), AppliedType(actualFun, actualArgs))
                      if expectedFun.typeSymbol == actualFun.typeSymbol &&
                         expectedArgs.size == actualArgs.size =>
                    (expectedArgs zip actualArgs flatMap typeParamInstantiations).toMap
                  case _=>
                    Map.empty

              case (expected: ParamRef, actual) if expected != actual && expected =:= actual =>
                Map(expected -> actual)

              case (expected, actual: ParamRef) if expected != actual && expected =:= actual =>
                Map(actual -> expected)

              case (expected: Refinement, actual) =>
                typeParamInstantiations(stripRefinements(expected), stripRefinements(actual))

              case (expected, actual: Refinement) =>
                typeParamInstantiations(stripRefinements(expected), stripRefinements(actual))

              case _ =>
                Map.empty
          end typeParamInstantiations

          object typeParamInstantiator:
            val instatiations = mutable.Map.empty[ParamRef, TypeRepr]

            def updateInstatiations(update: Map[ParamRef, TypeRepr]) =
              val newInstatiations = update -- instatiations.keys
              if newInstatiations.nonEmpty then
                instatiations ++= newInstatiations
                var updated = true
                while updated do
                  updated = false
                  instatiations mapValuesInPlace { (_, instatiation) =>
                    val newInstatiation = substitutor.transform(instatiation)
                    if newInstatiation != instatiation then updated = true
                    newInstatiation
                  }

            object substitutor extends SimpleTypeMap(quotes):
              override def transform(tpe: TypeRepr) = tpe match
                case tpe: ParamRef => instatiations.get(tpe) match
                  case Some(tpe) => transform(tpe)
                  case _ => super.transform(tpe)
                case _ => super.transform(tpe)

            object instatiationsCollector extends TreeMap:
              override def transformTerm(tree: Term)(owner: Symbol) = tree match
                case Apply(fun, args) =>
                  val transformedFun = transformTerm(fun)(owner)

                  transformedFun.tpe.widenTermRefByName match
                    case MethodType(_, params, _) if args.size == params.size =>
                      val transformedArgs = params zip args map { (param, arg) =>
                        updateInstatiations(typeParamInstantiations(param, arg.tpe.widenTermRefByName))
                        transformTerm(arg)(owner)
                      }
                      transformedFun.appliedToArgs(transformedArgs)
                    case _ =>
                      transformedFun.appliedToArgs(transformTerms(args)(owner))

                case TypeApply(fun, args) =>
                  val argsTypes = args map { _.tpe }

                  val argsTypesInstantiated = argsTypes map {
                    case tpe: ParamRef => instatiations.getOrElse(tpe, tpe)
                    case tpe => tpe
                  }

                  if argsTypes != argsTypesInstantiated then
                    super.transformTerm(fun.appliedToTypes(argsTypesInstantiated))(owner)
                  else
                    super.transformTerm(tree)(owner)

                case _ =>
                  super.transformTerm(tree)(owner)
            end instatiationsCollector

            object instatiationsInserter extends TreeMap:
              override def transformTypeTree(tree: TypeTree)(owner: Symbol) =
                val tpe = substitutor.transform(tree.tpe)
                if tpe != tree.tpe then TypeTree.of(using tpe.asType) else tree
            end instatiationsInserter

            def transformTerm(tree: Term)(owner: Symbol) =
              instatiationsInserter.transformTerm(instatiationsCollector.transformTerm(tree)(owner))(owner)
          end typeParamInstantiator

          val optimized = optimizer.transformTerm(
            typeParamInstantiator.transformTerm(
              result.tree)(
              Symbol.spliceOwner))(
            Symbol.spliceOwner)

          optimized.asExpr match
            case result: Expr[Transmittable.Resolution[B, I, R, P, T]] @unchecked => result
    }
  end optimizedTransmittableResolution

  def delegatingResolution[D <: Transmittable.Delegating: Type](using Quotes) =
    import quotes.reflect.*

    def summon[T: Type](using Quotes): Expr[T] =
      val transmittable =
        Type.of[T] match
          case '[ Transmittable.Aux[b, i, r, p, t] ] =>
            Expr.summon[Transmittable.Resolution[b, i, r, p, t]] map { resolution =>
              '{$resolution.transmittable}.asExprOf[T]
            }
          case _ =>
            None

      transmittable getOrElse report.errorAndAbort("Delegation is not transmittable")

    def resolve[D: Type](using Quotes): Expr[D] =
      Type.of[D] match
        case '[ d / t ] => '{ /(${resolve[d]}, ${summon[t]}) }.asExprOf[D]
        case _ => summon[D]

    '{ Transmittable.Delegating.Resolution(${resolve[D]}) }
  end delegatingResolution
end TransmittableResolution
