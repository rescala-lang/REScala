package rescala.core

import io.circe.{Decoder, Encoder}

import scala.annotation.implicitNotFound
import scala.util.{Success, Try}

@implicitNotFound("${T} is not serializable")
trait ReSerializable[T] {
  def serialize(value: T): String
  def deserialize(value: String): Try[T]
}

object ReSerializable {


  implicit def resevent[R <: Reactive[_]]: ReSerializable[R] = doNotSerialize
  implicit def resarray[R <: Reactive[_]]: ReSerializable[Array[R]] = doNotSerialize
  implicit def restrav[T <: Traversable[_ <: Reactive[_]]]: ReSerializable[T] = doNotSerialize
  implicit def resopt[T <: Option[_ <: Reactive[_]]]: ReSerializable[T] = doNotSerialize

  implicit def doNotSerialize[T]: ReSerializable[T] = null

  def pulseEncoder[T: Encoder](): Encoder[Pulse[T]] = io.circe.Encoder.encodeOption[T].contramap(_.toOption)
  def pulseDecoder[T: Decoder](): Decoder[Pulse[T]] = io.circe.Decoder.decodeOption[T].map(Pulse.fromOption)

  def pulseSerializable[T: ReSerializable](): ReSerializable[Pulse[T]] = new ReSerializable[Pulse[T]] {
    override def serialize(value: Pulse[T]): String = value.toOption.fold("")(implicitly[ReSerializable[T]].serialize)
    override def deserialize(value: String): Try[Pulse[T]] = if (value.isEmpty) Success(Pulse.empty) else implicitly[ReSerializable[T]].deserialize(value).map(Pulse.Value(_))
  }
}

object ReCirce {
  implicit def recirce[T: Encoder : Decoder]: ReSerializable[T] = new ReSerializable[T] {
    override def serialize(value: T): String = implicitly[Encoder[T]].apply(value).noSpaces
    override def deserialize(value: String): Try[T] = implicitly[Decoder[T]].decodeJson(io.circe.parser.parse(value).right.get).toTry
  }
}
