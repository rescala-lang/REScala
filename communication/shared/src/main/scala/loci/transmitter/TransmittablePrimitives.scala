package loci
package transmitter

import scala.concurrent.duration.{Duration, FiniteDuration}

trait TransmittablePrimitives extends TransmittableDummy {
  this: Transmittable.Base =>

  final implicit def unit: IdenticallyTransmittable[Unit] =
    IdenticallyTransmittable()
  final implicit def boolean: IdenticallyTransmittable[Boolean] =
    IdenticallyTransmittable()
  final implicit def char: IdenticallyTransmittable[Char] =
    IdenticallyTransmittable()
  final implicit def byte: IdenticallyTransmittable[Byte] =
    IdenticallyTransmittable()
  final implicit def short: IdenticallyTransmittable[Short] =
    IdenticallyTransmittable()
  final implicit def int: IdenticallyTransmittable[Int] =
    IdenticallyTransmittable()
  final implicit def long: IdenticallyTransmittable[Long] =
    IdenticallyTransmittable()
  final implicit def float: IdenticallyTransmittable[Float] =
    IdenticallyTransmittable()
  final implicit def double: IdenticallyTransmittable[Double] =
    IdenticallyTransmittable()
  final implicit def string: IdenticallyTransmittable[String] =
    IdenticallyTransmittable()
  final implicit def symbol: IdenticallyTransmittable[Symbol] =
    IdenticallyTransmittable()
  final implicit def bigInt: IdenticallyTransmittable[BigInt] =
    IdenticallyTransmittable()
  final implicit def bigDecimal: IdenticallyTransmittable[BigDecimal] =
    IdenticallyTransmittable()
  final implicit def duration: IdenticallyTransmittable[Duration] =
    IdenticallyTransmittable()
  final implicit def finiteDuration: IdenticallyTransmittable[FiniteDuration] =
    IdenticallyTransmittable()
  final implicit def infiniteDuration: IdenticallyTransmittable[Duration.Infinite] =
    IdenticallyTransmittable()

  final implicit def javaBoolean: IdenticallyTransmittable[java.lang.Boolean] =
    IdenticallyTransmittable()
  final implicit def javaChar: IdenticallyTransmittable[java.lang.Character] =
    IdenticallyTransmittable()
  final implicit def javaByte: IdenticallyTransmittable[java.lang.Byte] =
    IdenticallyTransmittable()
  final implicit def javaShort: IdenticallyTransmittable[java.lang.Short] =
    IdenticallyTransmittable()
  final implicit def javaInt: IdenticallyTransmittable[java.lang.Integer] =
    IdenticallyTransmittable()
  final implicit def javaLong: IdenticallyTransmittable[java.lang.Long] =
    IdenticallyTransmittable()
  final implicit def javaFloat: IdenticallyTransmittable[java.lang.Float] =
    IdenticallyTransmittable()
  final implicit def javaDouble: IdenticallyTransmittable[java.lang.Double] =
    IdenticallyTransmittable()
  final implicit def javaBigInteger: IdenticallyTransmittable[java.math.BigInteger] =
    IdenticallyTransmittable()
  final implicit def javaBigDecimal: IdenticallyTransmittable[java.math.BigDecimal] =
    IdenticallyTransmittable()
  final implicit def javaUuid: IdenticallyTransmittable[java.util.UUID] =
    IdenticallyTransmittable()
  final implicit def javaDate: IdenticallyTransmittable[java.util.Date] =
    IdenticallyTransmittable()
}
