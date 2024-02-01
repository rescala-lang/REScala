package loci
package transmitter

import scala.annotation.compileTimeOnly
import scala.collection.mutable
import scala.concurrent.Future
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait TransmittableDummy {
  this: Transmittable.Base =>

  @compileTimeOnly("Value is not transmittable")
  final implicit def resolutionFailure[
      B, I, R, P, T <: Transmittables,
      TransmittableFallback[B, I, R, P, T <: Transmittables]]: TransmittableFallback[B, I, R, P, T]
    = macro TransmittableResolutionFailure[B, I, R, P, T]

  @compileTimeOnly("Value is not transmittable")
  final def dummy[B, I, R, P, T <: Transmittables]: Transmittable.Aux[B, I, R, P, T]
    = throw new NotImplementedError
}

object TransmittableResolutionFailure {
  def apply[
      B: c.WeakTypeTag,
      I: c.WeakTypeTag,
      R: c.WeakTypeTag,
      P: c.WeakTypeTag,
      T <: Transmittables: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val B = weakTypeOf[B]
    val I = weakTypeOf[I]
    val R = weakTypeOf[R]
    val P = weakTypeOf[P]
    val T = weakTypeOf[T]

    val none = typeOf[Transmittables.None]
    val transmittableDummy = symbolOf[TransmittableDummy]
    val TypeRef(futurePre, futureSym, _) = typeOf[Future[Any]]: @unchecked
    val ExistentialType(existentialQuantified, TypeRef(pre, sym, existentialArgs)) =
      typeOf[Transmittable.Any[_, _, _]]: @unchecked

    def originalType(tpe: Type) = tpe map {
      case tpe @ TypeRef(_, _, List(_, original, _))
          if tpe <:< typeOf[TransmittableBase.SurrogateType[_, _, _]] =>
        original
      case tpe =>
        tpe
    }

    def originalName(tpe: Type) = {
      val names = mutable.ListBuffer.empty[(String, String)]

      val originalType = tpe map {
        case tpe @ TypeRef(_, _, List(_, original, ConstantType(Constant(name: String))))
            if tpe <:< typeOf[TransmittableBase.SurrogateType[_, _, _]] =>
          if (original.toString != name) {
            val nameType = internal.constantType(Constant(" :: <" + name + "> :: "))
            names += nameType.toString -> name
            nameType
          }
          else
            original
        case tpe =>
          tpe
      }

      names.foldLeft(originalType.toString) {
        case (originalName, (typeName, name)) =>
          val index = originalName.indexOf(typeName)
          if (index != -1)
            originalName.substring(0, index) +
            name +
            originalName.substring(index + typeName.length)
          else
            originalName
      }
    }

    def instantiatedTypeOrElse(tpe: Type, alternative: Type, alternativeSymbol: Symbol) = {
      val symbol = tpe.typeSymbol
      if (symbol.owner.owner == transmittableDummy)
        alternative -> Some(alternativeSymbol)
      else
        tpe -> None
    }

    def instantiatedOriginalTypeOrElse(tpe: Type, alternative: Type, alternativeSymbol: Symbol) =
      instantiatedTypeOrElse(originalType(tpe), alternative, alternativeSymbol)

    val (typeI, _) = instantiatedTypeOrElse(I, B, NoSymbol)
    val (typeR, _) = instantiatedTypeOrElse(R, B, NoSymbol)
    val (typeP, _) = instantiatedTypeOrElse(P, internal.typeRef(futurePre, futureSym, List(B)), NoSymbol)
    val (typeT, _) = instantiatedTypeOrElse(T, none, NoSymbol)

    val originalB = originalType(B)
    val nameB = originalName(B)
    val symbolB = originalB.typeSymbol

    val (args, quantified) =
      (List(B, I, R) zip existentialQuantified zip existentialArgs).foldRight(List.empty[Type] -> List.empty[Symbol]) {
        case (((tpe, symbol), arg), (args, quantified)) =>
          val (instantiatedType, instantiatedSymbol) = instantiatedOriginalTypeOrElse(tpe, arg, symbol)
          (instantiatedSymbol
            map { instantiatedSymbol => (instantiatedType :: args) -> (instantiatedSymbol :: quantified) }
            getOrElse { (instantiatedType :: args) -> quantified })
      }

    val transmittableTypeRef = internal.typeRef(pre, sym, args)

    val transmittableType =
      if (quantified.nonEmpty)
        internal.existentialType(quantified, transmittableTypeRef)
      else
        transmittableTypeRef

    val baseMessage = s"$nameB is not transmittable"

    val hintMessage =
      if (symbolB.isClass && symbolB.asClass.isCaseClass) {
        val impl = if (symbolB.isModuleClass) "case object" else "case class"
        s"$baseMessage; you may consider defining an `IdenticallyTransmittable[$originalB]` instance for $impl ${symbolB.name}"
      }
      else
        baseMessage

    val message =
      if (quantified.size < existentialQuantified.size)
        s"$hintMessage${utility.implicitHints.values(c)(transmittableType)}"
      else
        hintMessage

    q"""{
      @${termNames.ROOTPKG}.scala.annotation.compileTimeOnly($message) def resolutionFailure() = ()
      resolutionFailure()
      ${termNames.ROOTPKG}.loci.transmitter.TransmittableBase.dummy[$B, $typeI, $typeR, $typeP, $typeT]
    }"""
  }
}
