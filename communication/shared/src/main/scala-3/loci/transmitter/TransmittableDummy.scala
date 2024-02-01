package loci
package transmitter

import utility.reflectionExtensions.*

import scala.annotation.compileTimeOnly
import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.quoted.*

trait TransmittableDummy:
  this: Transmittable.Base =>

  @compileTimeOnly("Value is not transmittable")
  transparent inline given resolutionFailure[B, I, R, TransmittableFallback[_, _, _]](using
      inline ev: Transmittable.Any[B, I, R] =:= TransmittableFallback[B, I, R])
    : TransmittableFallback[B, I, R] =
      ${ Transmittable.resolutionFailureImpl[B, I, R, TransmittableFallback[B, I, R]] }

  @compileTimeOnly("Value is not transmittable")
  final def dummy[B, I, R, P, T <: Transmittables]: Transmittable.Aux[B, I, R, P, T] =
    throw new NotImplementedError

  def resolutionFailureImpl[B: Type, I: Type, R: Type, TransmittableFallback: Type](using Quotes) =
    import quotes.reflect.*

    val tpe = TypeRepr.of[B]
    val symbol = tpe.typeSymbol

    val baseMessage = s"${tpe.safeShow("Value")} is not transmittable"

    val hintMessage =
      val (impl, suffix) =
        if symbol.flags is Flags.Case then
          if symbol.flags is Flags.Module then "case object" -> ".type" else "case class" -> ""
        else if symbol.flags is Flags.Enum then
          "enum" -> ""
        else
          "" -> ""

      if impl.nonEmpty then
        val name = if symbol.name endsWith "$" then symbol.name.dropRight(1) else symbol.name
        s"$baseMessage; you may consider defining an `IdenticallyTransmittable[${tpe.show}$suffix]` instance for $impl $name"
      else
        baseMessage

    val message = s"$hintMessage${utility.implicitHints.values(TypeRepr.of[Transmittable.Any[B, ?, ?]])}"

    val transmittableDummy =
      (Seq(
          '{ Transmittable.dummy[B, B, B, Future[B], Transmittables.None] },
          '{ Transmittable.dummy[B, I, B, Future[B], Transmittables.None] },
          '{ Transmittable.dummy[B, B, R, Future[B], Transmittables.None] },
          '{ Transmittable.dummy[B, I, R, Future[B], Transmittables.None] })
        collectFirst Function.unlift { expr =>
          try Some(expr.asExprOf[TransmittableFallback])
          catch { case NonFatal(_) => None }
        })

    '{
      @compileTimeOnly(${Expr(message)}) def resolutionFailure() = ()
      resolutionFailure()
      ${transmittableDummy.get}
    }
  end resolutionFailureImpl
end TransmittableDummy
